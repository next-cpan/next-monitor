package Perl::ModuleStability;

use strict;
use warnings;

use FindBin;
use Moose;

use experimental 'signatures';

use File::Slurper    ();
use Cpanel::JSON::XS ();
use LWP::UserAgent   ();
use Git::Wrapper     ();

use constant ZERO        => '19700101';
use constant THA_FUTURE  => '30001212';
use constant RECENT_PERL => 20;           # 5.20.0

use constant MINIMUM_UNKNOWN_RESULTS => 10;
use constant MINIMUM_RECENT_RESULTS  => 10;
use constant MINIMUM_RESULTS         => 30;

use constant RECENT_REPORT_DATE => '20100101';

use constant MINIMUM_ALL_PASS_THRESHOLD         => '0.5';
use constant MINIMUM_RECENT_PERL_PASS_THRESHOLD => '0.5';

use constant CACHE_AGE_INVALIDATION => 60 * 60 * 24 * 7;    # 7 days.

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

has 'base_dir'   => ( isa => 'Str', is => 'ro', required => 1 );
has 'repos_dir'  => ( isa => 'Str', is => 'ro', required => 1 );
has 'git_binary' => ( isa => 'Str', is => 'ro', required => 1 );

has 'cache_file' => ( isa => 'Str',     is => 'ro', lazy => 1, default => sub { shift->base_dir . "/data/repo_stability.txt" } );
has 'cache'      => ( isa => 'HashRef', is => 'rw', lazy => 1, builder => '_build_cache' );
has 'cache_modified' => ( isa => 'Bool', is => 'rw', default => 0 );

has 'ua' => ( isa => 'Object', lazy => 1, is => 'ro', lazy => 1, builder => '_build_useragent' );

sub guess_version ( $self, $distro ) {

    my $cache = $self->cache;
    if ( $cache->{'distro'} && ref $cache->{'distro'} eq 'HASH' && %{ $cache->{'distro'} } ) {
        my ($version) = sort { $a cmp $b } keys %{ $cache->{'distro'} };
        length $version and return $version;
    }

    my $git = Git::Wrapper->new( { 'dir' => $self->repos_dir . '/' . $distro, 'git_binary' => $self->git_binary } ) || die( 'Failed to create Git::Wrapper for ' . $self->repos_dir );
    foreach my $entry ( $git->log( '-2', 'PAUSE' ) ) {
        $entry && $entry->message =~ m/Import \S+ version (\S+) from PAUSE/ or next;
        return "$1";
    }
    die("Unexpected log entry in PAUSE for $distro");
}

sub is_passing ( $self, $distro, $version = undef ) {

    $version //= $self->guess_version($distro);

    my $stats = $self->get_stats_for_module( $distro, $version );

    defined $stats->{'oldest_report'} or return 0;    # Means there are no results.

    if (0) {
        print "*** $distro $version\n";
        printf( "ALL: %0.3f\n",    $stats->{'all_results'}->{'pass'} / $stats->{'all_results'}->{'total'} );
        printf( "RECENT: %0.3f\n", $stats->{'recent_perl_results'}->{'pass'} / $stats->{'recent_perl_results'}->{'total'} );
        print Dumper($stats);
        exit;
    }

    # We have a lower threshold for where all results are unknown.
    return 0 if $stats->{'all_results'}->{'total'} >= MINIMUM_UNKNOWN_RESULTS && $stats->{'all_results'}->{'total'} == $stats->{'all_results'}->{'unknown'};

    # We can't yet judge this module based on results.
    return 1 if $stats->{'all_results'}->{'total'} < MINIMUM_RESULTS;

    # Just cause it's old doesn't mean it's not valid. We can't skip over this.
    # return 0 if( (RECENT_REPORT_DATE cmp $stats->{'oldest_report'}) > 0);

    # Detemine if the all time pass rate is abyssimal.
    return 0 if ( $stats->{'all_results'}->{'pass'} / $stats->{'all_results'}->{'total'} ) < MINIMUM_ALL_PASS_THRESHOLD;

    # We can't yet judge this module based on recent perl results.
    return 1 if $stats->{'recent_perl_results'}->{'total'} < MINIMUM_RECENT_RESULTS;

    return 0 if ( $stats->{'recent_perl_results'}->{'pass'} / $stats->{'recent_perl_results'}->{'total'} ) < MINIMUM_RECENT_PERL_PASS_THRESHOLD;

    return 1;
}

sub oldest_report ( $self, $distro, $version = undef ) {
    $version //= $self->get_version_from_repo($distro);
    my $stats = $self->get_stats_for_module( $distro, $version );
    return $stats->{'oldest_report'};
}

sub get_stats_for_module ( $self, $distro, $version ) {
    my $mtime = $self->cache->{$distro}->{$version}->{'mtime'} // 0;
    if ( time - $mtime < CACHE_AGE_INVALIDATION ) {
        return $self->cache->{$distro}->{$version};
    }

    # Initialize our data we're going to gather.
    my $oldest_report = THA_FUTURE;
    my $newest_report = ZERO;
    my %all_results;
    my %recent_perl_results;

    $all_results{$_}         //= 0 foreach (qw/pass unknown fail/);
    $recent_perl_results{$_} //= 0 foreach (qw/pass unknown fail/);

    my $stats = {
        all_results         => \%all_results,
        recent_perl_results => \%recent_perl_results,
        oldest_report       => undef,
        newest_report       => undef,
        mtime               => time,
    };

    my $url = "http://api.cpantesters.org/v3/summary/${distro}/${version}?osname=linux&perl_maturity=stable";
    my $r   = $self->ua->get($url);

    if ( !$r->is_success ) {
        die( "Failed to retrieve $url: " . $r->status_line );
    }
    my $json = Cpanel::JSON::XS::decode_json( $r->decoded_content );
    if ( ref $json eq 'HASH' ) {
        if ( $json->{'errors'} && $json->{'errors'}->[0]->{'message'} eq 'No results found' ) {
            return $self->cache->{$distro}->{$version} = $stats;
        }
    }
    ref $json eq 'ARRAY' or die( "Unexpected output from $url:\n" . Dumper($json) );

    foreach my $test_result (@$json) {

        # Find the oldest report.
        $test_result->{'date'} =~ m/^([0-9]{4,4})-([0-9]{2,2})-([0-9]{2,2})/ or die( "Unexpected date in test result:\n" . Dumper($test_result) );
        my $stamp = "$1$2$3";
        $oldest_report = $stamp if ( ( $stamp cmp $oldest_report ) < 0 );
        $newest_report = $stamp if ( ( $stamp cmp $newest_report ) > 0 );

        # Gather all report results.
        $all_results{ $test_result->{'grade'} }++;
        $all_results{'total'}++;

        # Gather recent perl report results.
        if ( is_recent_perl( $test_result->{'perl'} ) ) {
            $recent_perl_results{ $test_result->{'grade'} }++;
            $recent_perl_results{'total'}++;
        }
    }

    # Store our SVs into the hash.
    $stats->{'oldest_report'} = $oldest_report;
    $stats->{'newest_report'} = $newest_report;

    print "Retrieved cpantesters stats for $distro $version\n";

    $self->cache_modified(1);

    # Assure cache is updated and return it.
    return $self->cache->{$distro}->{$version} = $stats;
}

sub is_recent_perl ($version) {
    $version =~ m/^5\.([0-9]+)/ or die("Unexpected perl version string '$version'");
    return $1 >= RECENT_PERL;
}

sub _build_useragent ($self) {
    my $ua = LWP::UserAgent->new( timeout => 10 );

    # curl -X GET --header 'Accept: application/json' 'http://api.cpantesters.org/v3/summary/YAML-Syck/1.32?osname=linux&perl_maturity=stable'
    $ua->default_header( 'Accept' => 'application/json' );

    return $ua;
}

sub _build_cache ($self) {

    my $file = $self->cache_file;
    return {} unless -f $file && !-z _;

    return Cpanel::JSON::XS::decode_json( File::Slurper::read_binary($file) );
}

sub DEMOLISH ( $self, $in_global_destruction ) {
    $in_global_destruction and die( "Unexpectedly in global destruction to clean up " . __PACKAGE__ );

    return unless $self->cache_modified;
    print "Updating repo stability cache\n";
    open( my $fh, '>', $self->cache_file ) or die("Couldn't open cache file for write: $!");

    File::Slurper::write_text( $self->cache_file, Cpanel::JSON::XS->new->pretty->canonical( [1] )->encode( $self->cache ) );

    return;
}

1;

