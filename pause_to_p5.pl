#!/perl/bin/perl
package PauseTop5;

use strict;
use warnings;

use FindBin;

use lib 'lib';
use Perl::Distro    ();
use Perl::RepoCache ();

use Moose;
with 'MooseX::SimpleConfig';
with 'MooseX::Getopt';

use experimental 'signatures';

use CPAN::DistnameInfo  ();
use version             ();
use Config::INI::Reader ();
use File::Slurper       ();

has 'base_dir'   => ( isa => 'Str', is => 'ro', required => 1, documentation => 'REQUIRED - The base directory where our data is stored.' );                                # = /root/projects/pause-monitor
has 'git_binary' => ( isa => 'Str', is => 'ro', lazy     => 1, default       => '/usr/bin/git', documentation => 'The location of the git binary that should be used.' );

has 'repo_user_name' => ( isa => 'Str', is => 'ro', required => 1, documentation => 'The name that will be on commits for this repo.' );
has 'repo_email'     => ( isa => 'Str', is => 'ro', required => 1, documentation => 'The email that will be on commits for this repo.' );

has 'push_to_github' => ( isa => 'Bool', is => 'ro', required => 0, default => sub { 1 }, documentation => 'push changes to github.' );

has 'parsed_pause_archives_file' => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { $_[0]->base_dir . '/data/parsed_pause_archives.txt' } );
has 'repos_dir'                  => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { my $d = $_[0]->base_dir . '/repos'; -d $d or mkdir $d; return $d } );
has 'repo_list' => ( isa => 'ArrayRef', is => 'rw', lazy => 1, builder => '_build_repo_list' );

has 'pause_branch' => ( isa => 'Str',    is   => 'ro', default => 'PAUSE' );
has 'p5_branch'    => ( isa => 'Str',    is   => 'ro', default => 'p5' );
has 'repo_cache'   => ( isa => 'Object', lazy => 1,    is      => 'ro', lazy => 1, builder => '_build_repo_cache' );

sub _build_repo_cache ($self) {
    return Perl::RepoCache->new( { base_dir => $self->base_dir } );
}

sub _build_repo_list ($self) {
    my $repos_dir = $self->repos_dir;

    opendir( my $dh, $repos_dir ) or die("Cannot read $repos_dir: $!");
    my @repos = readdir $dh;
    close $dh;

    # Strip out directories starting with '.'
    @repos = sort { $a cmp $b } grep { $_ !~ m/^\.|^tmp$/ } @repos;

    return \@repos;
}

sub skip_list {
    my @skip_list;

    foreach my $line ( split( "\n", File::Slurper::read_text('data/skip_list.txt') ) ) {
        next unless $line =~ m/\S/;
        $line =~ s/\s.+$//;
        push @skip_list, $line;
    }

    return @skip_list;
}

sub run ( $self, @optional_repos ) {

    my @skip_list = skip_list();

    my $repo_list = $self->repo_list;
    if (@optional_repos) {
        $repo_list = \@optional_repos;
    }

    my $repo_cache = $self->repo_cache;

    foreach my $repo (@$repo_list) {
        if ( grep { $repo eq $_ } @skip_list ) {
            print "Skipping $repo\n";
            next;
        }

        next unless $repo_cache->needs_check($repo);
        print "--- Processing repo   $repo\n";

        my $repo_dir = $self->repos_dir . '/' . $repo;
        my $cd       = Cwd::Guard->new($repo_dir);

        my $distro = Perl::Distro->new( distro => $repo, repo_path => $repo_dir, git_binary => $self->git_binary, push_to_github => $self->push_to_github );

        $distro->do_the_do;

        $repo_cache->update_cache_for_repo($repo);
    }

    return 0;
}

sub parse_files_for_deps ( $self, $files_hash ) {
    my %deps;
    foreach my $file ( sort { $a cmp $b } keys %$files_hash ) {
        my $used = $self->parse_file_for_deps($file);
        next unless %$used;

        foreach my $module ( sort { $a cmp $b } keys %$used ) {
            if ( $file =~ m{^t/} ) {
                $deps{'requires_build'}->{$module} += $used->{$module};
            }
            elsif ( $file =~ m{^lib/} ) {
                $deps{'requires_runtime'}->{$module} += $used->{$module};
            }
            else {
                die("I don't know how to process deps for $file ($module)");
            }
        }

    }

    my %return;
    foreach my $dep_type (qw/requires_runtime requires_build/) {
        next unless $deps{$dep_type} && ref $deps{$dep_type} eq 'HASH';
        $return{$dep_type} = [ sort { $a cmp $b } keys %{ $deps{$dep_type} } ];
    }

    return \%return;
}

#sub DEBUG ($msg) {
#    chomp $msg;
#    print $msg . "\n";
#}

package main;

my $o = PauseTop5->new_with_options( configfile => "${FindBin::Bin}/settings.ini" );

exit( $o->run(@ARGV) );

