#!/perl/bin/perl

package PauseToGithubRepo;

use strict;
use warnings;

use Moose;
with 'MooseX::SimpleConfig';
with 'MooseX::Getopt';

use experimental 'signatures';
use experimental 'state';

use FindBin;

my $processed = 0;

use Time::HiRes qw/usleep/;
use LWP::UserAgent     ();
use File::Basename     ();
use CPAN::Meta::YAML   ();
use CPAN::DistnameInfo ();
use version            ();
use Net::GitHub::V3;
BEGIN { $Net::GitHub::V3::Orgs::VERSION == '2.0' or die("Need custom version of Net::GitHub::V3::Orgs to work!") }
use YAML::Syck   ();
use Git::Wrapper ();

use Parallel::ForkManager  ();
use IO::Uncompress::Gunzip ();
use Data::Dumper;

has 'base_dir'     => ( isa => 'Str', is => 'ro', required => 1, documentation => 'REQUIRED - The base directory where our data is stored.' );                     # = /root/projects/pause-monitor
has 'github_user'  => ( isa => 'Str', is => 'ro', required => 1, documentation => q{REQUIRED - The github username we'll use to create and update repos.} );       # = pause-parser
has 'github_token' => ( isa => 'Str', is => 'ro', required => 1, documentation => q{REQUIRED - The token we'll use to authenticate.} );
has 'github_org'   => ( isa => 'Str', is => 'ro', required => 1, documentation => q{REQUIRED - The github organization we'll be creating/updating repos in.} );    # = pause-play

has 'pause_cache_time'           => ( isa => 'Str',  is => 'ro', default => 3600,                                documentation => 'The amount of time we cache 02packages.details.txt. Defaults to 3600 seconds (1 hour).' );
has 'parallel_downloads'         => ( isa => 'Int',  is => 'ro', default => 40,                                  documentation => 'How many downloads we are going to attempt at one time.' );                                                         # = 40
has 'pause_base_url'             => ( isa => 'Str',  is => 'ro', default => 'http://httpupdate.cpanel.net/CPAN', documentation => 'The base CPAN URL of upstream PAUSE. Defaults to http://httpupdate.cpanel.net/CPAN' );
has 'validate_existing_archives' => ( isa => 'Bool', is => 'rw', default => 0,                                   documentation => 'Do we need to validate the existing archives before we start? This takes a little while so is off by default.' );
has 'git_binary'                 => ( isa => 'Str',  is => 'ro', default => '/usr/bin/git',                      documentation => 'The location of the git binary that should be used.' );
has 'maximum_modules_to_process' => ( isa => 'Str',  is => 'ro', default => 1000,                                documentation => 'The maximum modules to process at a time.' );
has 'reparse'                    => ( isa => 'Str',  is => 'ro', default => '',                                  documentation => 'Reparse a sindle tarball based on package name' );
has 'forceupdate'                => ( isa => 'Bool', is => 'ro', default => 0,                                   documentation => 'Update all the repos matching PAUSE and p5' );
has 'todo_file'                  => ( isa => 'Str',  is => 'ro', default => '',                                  documentation => 'A list of tarballs to create a repo for.' );

has 'repo_user_name' => ( isa => 'Str', is => 'ro', required => 1, documentation => 'The name that will be on commits for this repo.' );
has 'repo_email'     => ( isa => 'Str', is => 'ro', required => 1, documentation => 'The email that will be on commits for this repo.' );

has 'parsed_pause_archives_file' => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { $_[0]->base_dir . '/data/parsed_pause_archives.txt' } );
has 'tar_cache_dir'              => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { my $d = $_[0]->base_dir . '/tar_cache'; -d $d or mkdir $d; return $d } );
has 'repos_dir'                  => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { my $d = $_[0]->base_dir . '/repos'; -d $d or mkdir $d; return $d } );
has 'temp_repo_dir'              => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { my $d = $_[0]->repos_dir . '/tmp'; -d $d or mkdir $d; return $d } );

has 'tarball_parsed_cache' => ( isa => 'HashRef', is => 'rw', lazy => 1, builder => '_build_tarball_parsed_cache' );
has 'distros_requested'    => ( isa => 'HashRef', is => 'rw', lazy => 1, builder => '_build_distros_requested' );

has 'need_to_stop' => ( isa => 'Bool', is => 'rw', default => 0 );

has 'gh'      => ( isa => 'Object', is => 'ro', lazy => 1, default => sub { Net::GitHub::V3->new( version => 3, login => $_[0]->github_user, access_token => $_[0]->github_token ) } );
has 'gh_org'  => ( isa => 'Object', is => 'ro', lazy => 1, default => sub { $_[0]->gh->org } );
has 'gh_repo' => ( isa => 'Object', is => 'ro', lazy => 1, default => sub { $_[0]->gh->repos } );

has 'github_repos' => (
    isa     => 'HashRef',
    is      => 'rw',
    lazy    => 1,
    default => sub {
        my %repos = map { $_->{'name'} => 1 } $_[0]->gh_org->list_repos( $_[0]->github_org );
        return \%repos;
    }
);

sub _build_tarball_parsed_cache ($self) {
    my %cache;

    if ( open( my $fh, '<', $self->parsed_pause_archives_file ) ) {
        while (<$fh>) {
            chomp;
            $cache{$_} = 1;
        }
    }

    return \%cache;
}

sub _build_distros_requested ($self) {
    return {} unless $self->todo_file;

    open( my $fh, '<', $self->todo_file ) or die;
    my %requested;
    while ( my $line = <$fh> ) {
        next if $line =~ m/^\s*#/;
        next unless $line =~ m/\S/;
        chomp $line;
        $requested{$line} = 1;
    }

    return \%requested;
}

sub run ($self) {

    mkdir $self->base_dir;    # Make sure the base data dir is there.

    $self->stage_tarballs();

    if ( $self->forceupdate ) {
        $self->force_update_repos;
    }
    elsif ( $self->reparse ) {
        $self->reparse_module_tarball;
    }
    else {
        $self->process_updates();
    }

    return 0;
}

sub stage_tarballs ($self) {
    local $0 = "tarball_to_repo - Preparing to download archives.";

    my %tarball_requested;

    my $fh = $self->otwo_packages_fh();
    my @to_download;
    while ( my $line = <$fh> ) {
        my ( $module, $module_version, $author_path ) = split( qr/\s+/, $line );
        $author_path or next;
        chomp $author_path;

        # The legacy pm files we're just going to ignore.
        next if $author_path =~ m/\.(?:pm|pm.gz)$/i;

        my $url          = $self->pause_base_url . "/authors/id/$author_path";
        my $tarball_file = $self->path_to_tarball_cache_file($author_path);

        next if -f $tarball_file && $self->archive_was_parsed($author_path);    # Skip download if we have already parsed the tarball.

        # Have we have already determined we need this file?
        next if $tarball_requested{$tarball_file};
        $tarball_requested{$tarball_file} = 1;

        unlink $tarball_file if -z $tarball_file;

        next if ( -f $tarball_file && !$self->validate_existing_archives );     # No need to download it if we don't plan to validate it and it is there.

        push @to_download, [ $url, $tarball_file ];
    }

    $0 = "tarball_to_repo - Downloading and validating archives";
    my $pm = Parallel::ForkManager->new( $self->parallel_downloads );
    $pm->set_waitpid_blocking_sleep(0);

    my $files_validated = 0;
    foreach my $get (@to_download) {
        $files_validated++;

        $pm->start and next;

        my ( $url, $file ) = @$get;
        if ( -f $file ) {    # We can assume that we need to validate this.
            $0 = sprintf( "Validating %s", substr( $file, 6 + length( $self->tar_cache_dir ) ) );

            DEBUG("Validating ($files_validated) $file") if ( $files_validated % 100 == 0 );
            validate_archive($file) and $pm->finish;    # Nothing more to do if it is valid.

            DEBUG("$file was invalid!");
        }

        local $0 = sprintf( "Fetching %s", substr( $file, 6 + length( $self->tar_cache_dir ) ) );
        DEBUG($0);
        if ( !fetch_file( $url, $file ) || !validate_archive($file) ) {
            DEBUG("Re-downloading $url from $file");
            fetch_file( $url, $file ) && validate_archive($file);
        }
        $pm->finish;
    }
    $pm->wait_all_children;
}

sub validate_archive ($archive_file) {

    # Try to parse it.
    local $?;
    my $errors = '';
    if ( $archive_file =~ m/\.(?:tar\.[a-z]+|tar|tgz)$/i ) {    #.tar.gz or .tar.bz2 or .tar
        $errors = `/usr/bin/tar -tzf $archive_file 2>&1`;
    }
    elsif ( $archive_file =~ m/\.bz2$/ ) {
        $errors = `/usr/bin/tar -tjf $archive_file 2>&1`;
    }
    elsif ( $archive_file =~ m/\.zip$/ ) {
        $errors = `/usr/bin/unzip -q -t $archive_file 2>&1`;
    }
    else {
        unlink $archive_file;
        die("I don't know how to parse $archive_file");
    }

    return 1 if ( !$? );    # Nothing to do. The tarball is valid.

    warn "Got errors validating $archive_file with $? and $errors\n";

    # Remove it. It's invaalid.
    unlink $archive_file;

    return 0;
}

sub path_to_tarball_cache_file ( $self, $author_path ) {
    $author_path =~ s{/}{-}g;
    return $self->tar_cache_dir . '/' . $author_path;
}

sub otwo_packages_fh ($self) {
    my $otwofile = '02packages.details.txt';

    my $local_otwo_file = $self->base_dir . "/$otwofile";

    # Delete the cache if its over an hour (by default)
    my $pause_cache = ( $self->pause_cache_time // 0 + 0 ) || 60 * 60;
    my @age         = stat($local_otwo_file);
    if ( @age && $age[9] + $pause_cache < time ) {
        print "Removing cache $otwofile\n";
        unlink $otwofile;
    }

    if ( !-e $local_otwo_file ) {
        my $url = $self->pause_base_url . "/modules/$otwofile.gz";
        fetch_file( $url, "$local_otwo_file.gz" )
          or die "Failed to retrieve $url";
        IO::Uncompress::Gunzip::gunzip( "$local_otwo_file.gz" => $local_otwo_file )
          or die "gunzip failed for $local_otwo_file.gz: ${IO::Uncompress::Gunzip::GunzipError}";
        unlink "$local_otwo_file.gz";
    }

    open( my $fh, '<', $local_otwo_file ) or die;

    # Read in and ignore the header.
    my $line = <$fh>;
    while ( $line =~ m/\S/ ) {
        $line = <$fh>;
    }

    return $fh;
}

sub fetch_file ( $url, $file ) {
    my $_ua = LWP::UserAgent->new( timeout => 10 );

    my $response = $_ua->mirror( $url, $file );

    if ( !$response->is_success ) {
        print "Retrying download of $url\n";
        unlink $file;
        unlink "$file-$$";    # Temp file UA uses?
        $response = $_ua->mirror( $url, $file );
    }

    if ( !$response->is_success ) {
        warn "Failed to download $file: " . $response->status_line;
        unlink "$file-$$";    # Temp file UA uses?
        return 0;
    }

    return 1;
}

sub DEBUG ($msg) {
    chomp $msg;
    print $msg . "\n";
}

sub force_update_repos ($self) {
    my $fh = $self->otwo_packages_fh();

    my $repo_dir = $self->repos_dir;

    my %parsed = ( 'perl' => 1 );

    while ( my $line = <$fh> ) {
        my ( $module, $module_version, $author_path ) = split( qr/\s+/, $line );
        chomp $author_path;
        next if $author_path =~ m{\.pm\.gz$};

        my $d      = CPAN::DistnameInfo->new($author_path);
        my $distro = $d->dist;
        $distro or die("Couldn't parse $author_path");
        next if $parsed{$distro};

        my $repo_path = "$repo_dir/$distro";
        next if !-d $repo_path;
        $parsed{$distro} = 1;

        DEBUG("Processing $author_path for $distro");
        my $tarball_file = $self->path_to_tarball_cache_file($author_path);

        my $git  = Git::Wrapper->new( { dir => $repo_path, git_binary => $self->git_binary } ) or die("Failed to create Git::Wrapper for $repo_path");
        my $diff = $git->diff( '--stat', qw/PAUSE..p5/ );
        next if $diff;    # p5 has been updated. Move to the next.

        my $extracted_distro_name = $self->expand_distro( $tarball_file, $author_path );
    }
}

sub reparse_module_tarball ($self) {
    my $fh = $self->otwo_packages_fh();

    my $module_to_find = $self->reparse;

    while ( my $line = <$fh> ) {
        my ( $module, $module_version, $author_path ) = split( qr/\s+/, $line );
        next unless $module eq $module_to_find;
        chomp $author_path;

        DEBUG("Processing $author_path for $module");
        my $tarball_file = $self->path_to_tarball_cache_file($author_path);

        my $extracted_distro_name = $self->expand_distro( $tarball_file, $author_path );
        return 0;
    }

    print "Could not find $module_to_find\n";
    return 1;    # Failure;
}

sub process_updates ($self) {

    my $tarballs = $self->tarballs_to_process();

    foreach my $author_path (@$tarballs) {
        DEBUG("Processing $author_path");
        my $tarball_file          = $self->path_to_tarball_cache_file($author_path);
        my $extracted_distro_name = $self->expand_distro( $tarball_file, $author_path );

        $self->sleep_until_not_throttled();
        return if $self->need_to_stop;    # ctrl-C;
    }

    return 0;
}

use constant A_REALLY_BIG_VERSION => 99999999;

sub tarballs_to_process ($self) {
    my $a_really_big_version = 999999;
    my %checked_distros      = (
        'Acme-Working-Out-Dependencies-From-META-files-Will-Be-Wrong-At-Some-Point-Like-This-Module-For-Instance' => A_REALLY_BIG_VERSION,    # Longer than max repo length.
        'perl'                                                                                                    => A_REALLY_BIG_VERSION,    # Perl isn't a module/package.
    );

    open( my $uvt_fh, '<', 'data/unversioned_tarballs' ) or die;
    my %checked_tarballs;
    while ( my $tar = <$uvt_fh> ) {
        chomp $tar;
        $checked_tarballs{$tar} = 1;
    }

    my $requested = $self->distros_requested;
    $requested = undef unless %$requested;

    my %todo;

    my $fh = $self->otwo_packages_fh();
    while ( my $line = <$fh> ) {
        my ( $module, $module_version, $author_path ) = split( qr/\s+/, $line );
        chomp $author_path;

        next if $checked_tarballs{$author_path};    # We already looked at this one.
        $checked_tarballs{$author_path} = 1;

        # The legacy pm files we're just going to ignore.
        next if $author_path =~ m/\.(?:pm|pm.gz)$/i;

        my $d              = CPAN::DistnameInfo->new($author_path);
        my $distro         = $d->dist;
        my $distro_version = $d->version;
        next if $distro =~ m/^Bundle-/;
        ( $distro && defined $distro_version ) or die( sprintf( "Couldn't parse $module => $author_path (%s,%s)", $distro // 'undef', $distro_version // 'undef' ) );

        # If a list was provided, skip rows which aren't related to this distro.
        next if ( $requested && !$requested->{$distro} );

        # We've already planned a newer tarball
        next if compare( $checked_distros{$distro} // 0, '>=', $distro_version );
        $checked_distros{$distro} = $distro_version;

        $todo{$distro} = $author_path;

        exit if $self->need_to_stop;
    }

    my @list = grep { !$self->archive_was_parsed($_) } map { defined $todo{$_} ? $todo{$_} : () } sort { $a cmp $b } keys %todo;

    DEBUG( sprintf( "%d tarballs to process\n", scalar @list ) );

    #print "$_\n" foreach @list; exit;

    return \@list;
}

my $loop;

sub sleep_until_not_throttled ($self) {
    my $rate_remaining;

    $loop++;
    my $gh = $self->gh;

    while ( ( $rate_remaining = $gh->rate_limit_remaining() ) < 1000 ) {
        my $time_to_wait = time - $gh->rate_limit_reset() + 1;
        $time_to_wait > 0 or die("time_remaining == $time_to_wait");
        $time_to_wait = int( $time_to_wait / 2 );
        DEBUG("Only $rate_remaining API queries are allowed for the next $time_to_wait seconds.");
        DEBUG("Sleeping until we can send more API queries");
        sleep 10;
        $gh->update_rate_limit();
    }

    DEBUG( "        Rate remaining is $rate_remaining. Resets at " . $gh->rate_limit_reset() ) if $loop % 10 == 0;

    return;
}

sub archive_was_parsed ( $self, $author_path ) {
    return $self->tarball_parsed_cache->{$author_path};
}

sub report_archive_parsed ( $self, $archive ) {
    $self->tarball_parsed_cache->{$archive} = 1;
    open( my $fh, '>>', $self->parsed_pause_archives_file ) or die( "Cannot open " . $self->parsed_pause_archives_file . " for write." );
    print {$fh} "$archive\n";
    close $fh;
    return;
}

sub expand_distro ( $self, $tarball_file, $author_path ) {
    $tarball_file or die;
    $author_path  or die;

    # Do we know how to process this file?
    my $tool;
    $tool = '/usr/bin/tar -xf' if $tarball_file =~ m/\.tar\.(gz|bz2)$|\.tgz$/i;
    $tool = '/usr/bin/unzip'   if $tarball_file =~ m/\.zip$/i;
    $tool or die("Don't know how to handle $tarball_file");

    # Setup the temp repo dir.
    my $temp_dir = $self->temp_repo_dir;
    `/bin/rm -rf "$temp_dir" 2>&1`;    # Just in case.
    mkdir($temp_dir) or die("Couldn't create temp dir $temp_dir");
    chdir $temp_dir  or die;

    my $untar_got   = `$tool "$tarball_file" 2>&1`;
    my $untar_error = $?;

    if ($untar_error) {
        DEBUG("Error extracting $tarball_file ($untar_error)");
        DEBUG($untar_got);
        die;
    }

    state @crazy_files = qw{
      dfjhfjhfjhf
    };

    # Collapse all
    my @files = glob('*');
    if ( scalar @files == 1 && -d $files[0] ) {
        my $dir = $files[0];
        $dir && $dir !~ m/^\./ or die("Unexpected dir $dir");

        `find "$temp_dir" -name .git -exec /bin/rm -rf {} \\; 2>&1`;    # remove extracted .git dirs.
        `mv $temp_dir/"$dir" $temp_dir/"$dir.$$"`;
        `mv $temp_dir/"$dir.$$"/*  $temp_dir 2>&1`;
        `mv $temp_dir/"$dir.$$"/.* $temp_dir 2>&1`;
        `/usr/bin/rm -rf .github`; # Make sure github .worklfow stuff isn't triggered.

        unlink @crazy_files;
        rmdir @crazy_files;

        rmdir "$temp_dir/$dir.$$" or die("Could not remove directory $temp_dir/$dir.$$");
    }
    elsif ( scalar @files ) {
        DEBUG("$tarball_file had no base dir????");
    }
    else {
        DEBUG("XXXX Could not find any extracted files for $tarball_file");
        die;
    }

    # Zero out the big files.
    #my @big_files = `find $temp_dir -type f -size +5M`;
    #foreach my $bigfile (@big_files) {
    #    chomp $bigfile;
    #    next if $bigfile =~ m{/sqlite3.c$}; # We're going to white list sqlite3.c
    #    open(my $fh, '>', $bigfile) or die ("Can't write to $bigfile?? $?");
    #    print {$fh} "The contents of this file exceeded 5MB and were deemed inappropriate for this repository.\n";
    #}

    # Read the meta file we just extracted and try to determine the distro dir.
    my ( $distro, $version ) = $self->determine_distro_and_version( $temp_dir, $author_path );
    $distro or die "$distro -- $version -- $author_path";

    my $repo_dir = $self->repos_dir . '/' . $distro;
    my ( undef, $existing_version ) = get_dist_version_from_meta_yaml("$repo_dir/META.yml");

    # If there's a repo dir there but we can't parse its META.yaml...
    if ( !length $existing_version && -d $repo_dir ) {
        $repo_dir or die("Couldn't parse version for existing $repo_dir");
    }

    if ( $self->forceupdate ) {
        return 0 if ( $distro eq 'XML-Toolkit' );
        $self->update_extracted_tarball_from_tmp_to_repo( $distro, $version, $author_path );
    }
    elsif ( $self->reparse ) {
        $self->add_extracted_tarball_from_tmp_to_repo( $distro, $version, $author_path );
    }
    elsif ( compare( $existing_version // 0, '<', $version ) ) {    # TODO: This is probably fragile. if version schemes change, this could lead to versions being skipped.
        $self->add_extracted_tarball_from_tmp_to_repo( $distro, $version, $author_path );
    }
    else {
        DEBUG("Skipping parse of $distro version $version. We already have version $existing_version");
    }

    $self->report_archive_parsed($author_path);

    if ( $processed >= $self->maximum_modules_to_process ) {
        print "Processed $processed distros. Stopping\n";
        exit;
    }

    return 0;
}

sub create_github_repo ( $self, $distro ) {

    print "Creating repo $distro\n";
    my $rp = $self->gh_repo->create(
        {
            org         => $self->github_org,
            name        => $distro,
            description => "The CPAN Distro $distro",
            homepage    => "https://metacpan.org/release/$distro",
            has_issues  => 0,
        }
    );

    return if ( $rp && $rp->{'name'} eq $distro );
    DEBUG("Unexpected failure creating github repo $distro:");
    print Dumper $rp;
    die;
}

sub delete_all_repo_files ( $self, $git, $distro ) {

    # Delete any old stuff.
    my @files = eval { $git->ls_files };
    if (@files) {
        eval { $git->rm( '-f', @files ) };

        # Just in case any files are remaining.
        eval { @files = $git->ls_files };
        if (@files) {
            foreach my $file (@files) {
                $git->rm( '-f', $file );
            }
            @files = $git->ls_files;
        }
        @files and die( "Unexpected files could not be deleted from $distro repo: " . Dumper \@files );
    }
}

sub update_extracted_tarball_from_tmp_to_repo ( $self, $distro, $version, $author_path ) {
    my $temp_dir  = $self->temp_repo_dir;
    my $repo_path = $self->repos_dir . '/' . $distro;

    -d $repo_path or die("Unexpected missing path on update request - $repo_path");

    my $git = Git::Wrapper->new( { dir => $repo_path, git_binary => $self->git_binary } ) or die("Failed to create Git::Wrapper for $repo_path");

    # Cleanup the repo.
    eval { $git->merge('--abort') };    # In case we were in the middle of a merge.
    $git->reset('.');
    $git->clean('-dxf');
    $git->checkout('.');

    # Switch to PAUSE.
    $git->checkout( '-f', 'PAUSE' );
    $self->delete_all_repo_files( $git, $distro );

    `/usr/bin/mv $temp_dir/* $repo_path/ 2>&1`;
    `/usr/bin/mv $temp_dir/.* $repo_path/ 2>&1`;
    rmdir $temp_dir or die( "Could not move all files from temp dir to $repo_path via $temp_dir: " . `ls -al $temp_dir` );

    eval { $git->add( '-f', '.' ) };    # Ignore .gitignore
    return unless $git->status->is_dirty;

    $git->commit( '-m', "Import $distro version $version from PAUSE\n\n    $author_path" );
    $git->push( 'origin', '+PAUSE' );
    $git->checkout( '-f', 'p5' );
    $git->reset( '--hard', 'PAUSE' );
    $git->push( 'origin', '+p5' );
    print "~~~~~~~~ Updated $author_path via force_update\n";

    return;
}

sub add_extracted_tarball_from_tmp_to_repo ( $self, $distro, $version, $author_path ) {
    my $temp_dir  = $self->temp_repo_dir;
    my $repo_path = $self->repos_dir . '/' . $distro;

    my $git         = eval { Git::Wrapper->new( { dir => $repo_path, git_binary => $self->git_binary } ) } or die("Failed to create Git::Wrapper for $repo_path: $@");
    my $just_cloned = 0;

    if ( !-d $repo_path ) {
        my $url = sprintf( "git\@github.com:%s/%s.git", $self->github_org, $distro );
        if ( !$self->github_repos->{$distro} ) {
            mkdir $repo_path or die("Can't mkdir $repo_path");

            $self->create_github_repo($distro);
            $git->init;
            $git->remote( 'add', 'origin', $url );
            $git->checkout( '-b', 'p5' );

            $just_cloned = 1;
        }
        else {
            $git->clone( $url, $repo_path );
        }

        $just_cloned = 1;
        $git->config( 'user.name',  $self->repo_user_name );
        $git->config( 'user.email', $self->repo_email );
    }
    else {
        $git->checkout( '-f', 'PAUSE' );
    }

    -d $repo_path or die("Can't proceed without a $repo_path dir");

    $self->delete_all_repo_files( $git, $distro );

    `/usr/bin/mv $temp_dir/* $repo_path/ 2>&1`;
    `/usr/bin/mv $temp_dir/.* $repo_path/ 2>&1`;
    rmdir $temp_dir or die( "Could not move all files from temp dir to $repo_path via $temp_dir: " . `ls -al $temp_dir` );

    eval { $git->add( '-f', '.' ) };    # Ignore .gitignore
    eval { $git->commit( '-m', "Import $distro version $version from PAUSE\n\n    $author_path" ) };

    # We only need to do this the first time since we couldn't create a branch and set upstream until we make our first commit.
    if ($just_cloned) {
        eval { $git->push(qw/--set-upstream origin p5/) };

        eval { $git->checkout(qw/-b PAUSE/) };
        eval { $git->push(qw/--set-upstream origin PAUSE/) };
    }
    else {
        eval { $git->push(qw/origin PAUSE/) };
    }

    $processed++;
    return 1;
}

sub determine_distro_and_version ( $self, $extracted_dir, $author_path ) {

    my $d       = CPAN::DistnameInfo->new($author_path);
    my $distro  = $d->dist;
    my $version = $d->version;

    # Is the version parseable?
    if ( $version and eval { version->parse($version); 1 } ) {
        return ( $distro, $version );
    }

    my $new_yaml = "$extracted_dir/META.yml";

    my ( $meta_name, $meta_version ) = get_dist_version_from_meta_yaml($new_yaml);

    # Couldn't parse meta. Just fall back to CPAN::DistnameInfo
    $meta_name or return ( $distro, $version );

    $meta_name =~ s/::/-/g;
    return ( $meta_name, $meta_version );
}

sub get_dist_version_from_meta_yaml ($file) {
    open( my $fh, "<:utf8", $file ) or return ( '', 0 );
    my $yaml_text = do { local $/; <$fh> };
    my $yaml      = eval { CPAN::Meta::YAML->read_string($yaml_text) } or return ( '', 0 );

    return ( $yaml->[0]->{'name'} // '', $yaml->[0]->{'version'} // 0 );
}

sub compare {
    my ( $left, $right ) = align( $_[0], $_[2] );
    my $cmp = $_[1];

    return $left gt $right if ( $cmp eq '>' );
    return $left lt $right if ( $cmp eq '<' );
    return ( $left gt $right or $left eq $right ) if ( $cmp eq '>=' );
    return ( $left lt $right or $left eq $right ) if ( $cmp eq '<=' );
    return $left eq $right if ( $cmp eq '=' );

    die("Unknown comparison: '$cmp' -- $_[0] $_[1] $_[2]");
}

sub align ( $left = '', $right = '' ) {

    # Leading/trailing whitespace.
    $_ =~ s/^\s+// foreach ( $left, $right );
    $_ =~ s/\s+$// foreach ( $left, $right );

    $left  = "0:$left"  if ( $left  !~ m/^\d*:/ && $right =~ /^\d*:/ );    # Insert 0 epoch if not on both.
    $right = "0:$right" if ( $right !~ m/^\d*:/ && $left  =~ /^\d*:/ );    # Insert 0 epoch if not on both.

    for ( $left, $right ) {
        $_ =~ "$_-0" if ( $_ !~ m/-\d+$/ );                                # Force a -0 version on each, similar to forcing an epoch.
    }

    # Split
    my (@left_array)  = split( /[\.\-\:]/, $left );
    my (@right_array) = split( /[\.\-\:]/, $right );

    # Pad each section with zeros or spaces.
    for my $seg ( 0 .. $#left_array ) {
        $right_array[$seg] = 0 if ( !$right_array[$seg] );                 # In case right is not set.

        my ( $left_len, $right_len ) = ( 0, 0 );
        $left_array[$seg] =~ m/^(\d+|^\D+)/
          and $left_len = length($1);
        $right_array[$seg] =~ m/^(\d+|^\D+)/
          and $right_len = length($1);

        if ( $left_len < $right_len ) {
            my $appender = $left_array[$seg] =~ m/^\d/ ? '0' : ' ';
            $left_array[$seg] = $appender x ( $right_len - $left_len ) . $left_array[$seg];
        }
        elsif ( $left_len > $right_len ) {
            my $appender = $right_array[$seg] =~ m/^\d/ ? '0' : ' ';
            $right_array[$seg] = $appender x ( $left_len - $right_len ) . $right_array[$seg];
        }
    }

    # Right segments length is > left segments length?
    for my $seg ( scalar @left_array .. $#right_array ) {
        $left_array[$seg] = "0" x length("$right_array[$seg]");
    }

    return ( join( "~", @left_array ), join( "~", @right_array ) );
}

package main;

my $ptgr = PauseToGithubRepo->new_with_options( configfile => "${FindBin::Bin}/settings.ini" );

$SIG{INT} = sub {
    $ptgr->need_to_stop(1);
    print "Exiting at next convenient point\n";
};

exit( $ptgr->run );
