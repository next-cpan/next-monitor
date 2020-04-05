package Perl::Distro;

use strict;
use warnings;

use FindBin;
use Moose;

use experimental 'signatures';
use experimental 'state';

use version          ();
use Git::Wrapper     ();
use File::Slurper    ();
use Cwd::Guard       ();
use Cpanel::JSON::XS ();
use CPAN::Meta::YAML ();
use File::Path qw/mkpath/;
use Pod::Markdown::Github ();

use PPI::Document ();
use PPI::Dumper   ();

use Carp;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

has 'distro'            => ( isa => 'Str',  is => 'ro', required => 1 );
has 'repo_path'         => ( isa => 'Str',  is => 'ro', required => 1 );
has 'git_binary'        => ( isa => 'Str',  is => 'ro', required => 1 );
has 'push_to_github'    => ( isa => 'Bool', is => 'ro', required => 1 );
has 'clean_dirty_repos' => ( isa => 'Bool', is => 'ro', default  => 1 );

has 'git'       => ( isa => 'Object',  lazy => 1,    is   => 'ro', lazy    => 1, builder => '_build_git' );
has 'dist_meta' => ( isa => 'HashRef', is   => 'rw', lazy => 1,    builder => '_build_meta' );
has 'repo_files' => (
    isa     => 'HashRef',
    is      => 'rw',
    lazy    => 1,
    default => sub {
        return { map { ( $_ => 1 ) } shift->git->ls_files };
    }
);

has 'scripts'            => ( isa => 'ArrayRef', is => 'rw', default => sub { return [] } );
has 'requires_build'     => ( isa => 'HashRef',  is => 'rw', default => sub { return {} } );
has 'requires_runtime'   => ( isa => 'HashRef',  is => 'rw', default => sub { return {} } );
has 'requires_develop'   => ( isa => 'HashRef',  is => 'rw', default => sub { return {} } );
has 'recommends_build'   => ( isa => 'HashRef',  is => 'rw', default => sub { return {} } );
has 'recommends_runtime' => ( isa => 'HashRef',  is => 'rw', default => sub { return {} } );
has 'provides'           => ( isa => 'HashRef',  is => 'rw', default => sub { return {} } );
has 'ppi_cache'          => ( isa => 'HashRef',  is => 'rw', default => sub { return {} } );
has 'BUILD_json'         => ( isa => 'HashRef',  is => 'rw', default => sub { return {} } );
has 'BUILD_file'         => ( isa => 'Str',      is => 'rw', default => 'BUILD.json' );

sub _build_git ($self) {
    return Git::Wrapper->new( { 'dir' => $self->repo_path, 'git_binary' => $self->git_binary } ) || die( 'Failed to create Git::Wrapper for ' . $self->repo_path );
}

sub _build_meta ($self) {

    if ( -f 'META.json' ) {
        return Cpanel::JSON::XS::decode_json( $self->try_to_read_file('META.json') );
    }
    if ( -f 'META.yml' ) {
        my $txt  = $self->try_to_read_file('META.yml');
        my $yaml = CPAN::Meta::YAML->read_string($txt);
        return $yaml->[0] if $yaml && ref $yaml eq 'CPAN::Meta::YAML';
    }
    if ( -f 'Makefile.PL' ) {    # Generate MYMETA.json?
        `$^X Makefile.PL`;
        if ( -e 'MYMETA.json' ) {
            my $json = Cpanel::JSON::XS::decode_json( $self->try_to_read_file('MYMETA.json') );
            $self->git->clean('-dxf');
            return $json;
        }
    }

    die( 'No META data found in ' . $self->distro . "\n" . `ls -l` . "\n\n" . Carp::longmess );
}

sub dump_self ($self) {
    $self->ppi_cache( {} );
    print Dumper $self;
}

sub is_play ($self) {
    return $self->BUILD_json->{'builder'} eq 'play';
}

sub do_the_do ($self) {
    $self->check_if_dirty_and_die;
    $self->checkout_p5_branch;

    my @log = $self->git->log(qw/p5..PAUSE/);

    # Has the PAUSE branch been updated since p5 was last merged from it?
    if (@log) {    # Reset the branch to its state on the current PAUSE branch and re-process it.
        my $git = $self->git;
        eval { $git->merge('PAUSE') };    # This will probably fail but that's ok.
        $self->delete_all_repo_files;
        my $bin = $self->git_binary;
        `$bin archive PAUSE | /usr/bin/tar -x`;
        $self->git->add( '-f', '.' );
    }
    elsif ( -e $self->BUILD_file ) {
        print "Nothing to update\n";

        # There are no new changes and BUILD.yaml has already been created.
        return;
    }

    $self->fix_special_repos;
    $self->determine_installer;

    if ( $self->is_play ) {
        $self->parse_maker;
        $self->cleanup_tree;
        $self->update_p5_branch_from_PAUSE;
    }
    else {
        $self->determine_primary_module;

        # Try to parse the POD
        my $primary = $self->find_non_play_primary;
        $self->gather_non_play_provides_from_meta;
        $self->parse_pod($primary) if $primary;

        $self->generate_build_json;
    }

    $self->git_commit;
}

sub delete_all_repo_files ($self) {
    my $git = $self->git;

    my @files = eval { $git->ls_files };
    return unless @files;

    # remove any files and check if we succeed.
    eval { $git->rm( '-f', @files ) };
    eval { @files = $git->ls_files };
    return unless @files;

    # If we still have files, try to do it one at a time.
    foreach my $file (@files) {
        $git->rm( '-f', $file );
    }

    # fail if there are still files at this point.
    @files = $git->ls_files;
    @files and die( sprintf( "Unexpected files could not be deleted from %s repo: %s", $self->distro, Dumper \@files ) );

    return;
}

sub gather_non_play_provides_from_meta ($self) {
    my $meta = $self->dist_meta;
    return unless $meta->{'provides'};

    my $provides = $self->provides;

    foreach my $module ( sort { $a cmp $b } keys %{ $meta->{'provides'} } ) {
        $provides->{$module}->{'file'}    = $meta->{'provides'}->{$module}->{'file'};
        $provides->{$module}->{'version'} = $meta->{'provides'}->{$module}->{'version'} // 0;
    }
}

# This is only done in the event the module isn't play and we need to look around for it.
sub find_non_play_primary ($self) {
    my @parts    = split( "::", $self->BUILD_json->{'primary'} );
    my $lib_path = join( "/", 'lib', @parts ) . ".pm";
    return $lib_path if -f $lib_path;

    print "no $lib_path\n";

    my $no_lib = $parts[-1] . ".pm";
    return $no_lib if -f $no_lib;

    print "no $no_lib\n";

    return;
}

sub check_if_dirty_and_die ($self) {
    my $git = $self->git;
    my $st  = $git->status;
    return unless $st->is_dirty;

    my $txt = '';
    foreach my $status (qw/indexed changed unknown/) {
        my @status = $st->get($status) or next;
        my $type   = $status eq 'unknown' ? 'Untracked' :    #
          $status eq 'changed' ? 'Changes not staged' :      #
          'Changes to be committed';                         #
        $txt .= "$type: \n";
        foreach my $s (@status) {
            $txt .= sprintf( "  - %s\n", $s->from );
        }
    }

    if ( $self->clean_dirty_repos ) {
        $git->reset('.');
        $git->clean('-dxf');
        $git->checkout('.');
    }
    else {
        my $repo_path = $self->git->dir;
        die("\nThe repo $repo_path is unexpectedly dirty. Please correct this:\n$txt\n");
    }

    return;
}

sub checkout_p5_branch ($self) {
    my $git = $self->git;

    my @branches = eval { $git->branch };

    # the p5 branch is already there. Just switch to it.
    if ( grep { $_ =~ m/^\s*\*?\s+p5\z/ } @branches ) {
        eval { $git->checkout( '-f', 'p5' ) };
        return;
    }

    # All of this only needs to happen if we don't have a local p5 branch already.

    eval { $git->fetch('origin') };
    @branches = eval { $git->branch('-a') };

    # If the remote exists for some reason, let's check out the remote and track it.
    if ( grep { $_ =~ m{^\s*remotes/origin/p5\z} } @branches ) {
        eval { $git->checkout(qw{-t origin/p5}) };
        @branches = eval { $git->branch };

        # the p5 branch is now there.
        return if grep { $_ =~ m/^\s*\*\s+p5\z/ } @branches;
        die( "Could not checkout p5 branch from upstream in repo: " . $git->dir );
    }

    # We haven't generated the p5 branch yet. we need to fork it from PAUSE.

    # Make sure there's a PAUSE branch.
    if ( !grep { $_ =~ m/^\s*\*?\s+PAUSE\z/ } @branches ) {
        die( "Cannot create a p5 branch because PAUSE is missing for repo " . $git->dir );
    }

    $git->checkout(qw{-b p5});
    $git->reset(qw{--hard PAUSE});
    $git->push(qw{--set-upstream origin p5}) if $self->push_to_github;
}

sub fix_special_repos ( $self ) {

    my $distro = $self->distro;

    # Distro doesn't match the file.
    $self->parse_pod('Ace.pm') if $distro eq 'AcePerl';

    $self->BUILD_json->{'license'} = 'unknown' if $distro eq 'Acme-Code-FreedomFighter';

    state $files_to_delete = {
        'Acme-Aheui'                                => [qw{bin/aheui}],
        'Acme-BeCool'                               => [qw{example.pm}],
        'Acme-Beatnik'                              => [qw{example.pl findwords.pl generate.pl}],
        'Acme-Blarghy-McBlarghBlargh'               => [qw{blarghymcblarghblargh.pl}],
        'Acme-Buckaroo'                             => [qw{retest.txt}],
        'Acme-Buffy'                                => [qw{buffy}],
        'Acme-CPAN-Testers-UNKNOWN'                 => [qw{messup.PL}],
        'Acme-CPANAuthors-Acme-CPANAuthors-Authors' => [qw{scripts/author_info.pl scripts/basic_info.pl}],
        'Acme-Cow-Interpreter'                      => ['bin/*'],
        'Test-Unit'                                 => [ 'TestRunner.pl', 'TkTestRunner.pl' ],
        'Acme-CPANAuthors-AnyEvent'                 => [qw{script/AnyEvent.tt script/generate.pl}],
    };

    return unless $files_to_delete->{ $self->distro };
    $self->git->rm( '-f', @{ $files_to_delete->{ $self->distro } } );
}

sub cleanup_tree ($self) {
    my $git   = $self->git;
    my $files = $self->repo_files;

    $self->dist_meta;    # Initialize META data in memory before removing it.

    # Delete explicit files we don't want.
    foreach my $unwanted_file (
        qw{ MANIFEST MANIFEST.SKIP MANIFEST.bak INSTALL SIGNATURE dist.ini README README.md README.pod Makefile.PL Build.PL weaver.ini
        META.yml META.json ignore.txt .gitignore Changes.PL cpanfile cpanfile.snapshot minil.toml .cvsignore .travis.yml travis.yml
        .project t/boilerplate.t MYMETA.json MYMETA.yml Makefile Makefile.old maint/Makefile.PL.include metamerge.json README.bak dist.ini.bak
        CREDITS doap.ttl author_test.sh cpants.pl makeall.sh
        }
    ) {
        next unless $files->{$unwanted_file};
        delete $files->{$unwanted_file};
        $git->rm( '-f', $unwanted_file );
    }

    # Throw out maint/cip- files. Not sure what they are.
    foreach my $file ( keys %$files ) {
        next unless $file =~ m{^maint/(cip-|release)};
        delete $files->{$file};
        $git->rm( '-f', $file );
    }

    # Get rid of directories we don't want.
    foreach my $file ( keys %$files ) {
        next unless $file =~ m{^(?:inc|debian|ubuntu-[^/]+)/};
        delete $files->{$file};
        $git->rm( '-f', $file );
    }

    # *~ or /#file#
    foreach my $file ( keys %$files ) {
        next unless $file =~ m{~$|/#.+#$};
        delete $files->{$file};
        $git->rm( '-f', $file );
    }

    # Normalize all TODO files to 'Todo' and throw out the boilerplate ones.
    my @todo = sort grep { $_ =~ m/^todo$/i } keys %$files;
    if (@todo) {
        scalar @todo == 1 or die( "Too many TODO files.\n" . Dumper($files) );
        my $todo = shift @todo;
        if ( $todo ne 'Todo' ) {
            delete $files->{$todo};
            $files->{'Todo'} = 1;
            $git->mv( $todo, 'Todo' );
        }

        # See if the TODO is worthless.
        my $content = $self->try_to_read_file('Todo');
        if ( $content =~ m/- Nothing yet/ms ) {
            $git->rm( '-f', 'Todo' );
            delete $files->{'Todo'};
        }
    }

    # Alternative license files.
    if ( -f 'COPYRIGHT' ) {
        if ( !-f 'LICENSE' ) {
            $git->mv( 'COPYRIGHT', 'LICENSE' );
            $files->{'LICENSE'} = 1;
        }
        $git->rm( '-f', 'COPYRIGHT' );
        delete $files->{'COPYRIGHT'};
    }

    # Normalize all Changelog files to a common 'Changelog'
    foreach my $changes_variant (qw/CHANGES Changes ChangeLog/) {
        if ( $files->{$changes_variant} ) {
            $files->{'Changelog'} && die("Unexpectedly saw Changelog and $changes_variant in the same repo. I don't know what to do");

            print "Renaming $changes_variant to Changelog\n";
            $git->mv( $changes_variant, 'Changelog' );
            delete $files->{$changes_variant};
            $files->{'Changelog'} = 1;
            last;    # We can't move 2 files to the same destination so this will fail later.
        }
    }

    foreach my $file ( sort { $a cmp $b } keys %$files ) {
        next unless $self->is_xt_test($file);

        print "Moving $file to xt/\n";

        mkdir('xt');
        $git->mv( $file, 'xt/' );

        # update the files hash.
        delete $files->{$file};
        $file =~ s{^t/}{xt/};
        $files->{$file} = 1;
    }

    if ( -f 'test.pl' ) {
        mkdir 't';
        -e 't/test_pl.t' and die("Unexpected test.pl / test_pl.t combo??");
        $git->mv( 'test.pl', 't/test_pl.t' );
        delete $files->{'test.pl'};
        $files->{'t/test_pl.t'} = 1;
    }

    return;
}

sub determine_primary_module ($self) {
    my $git        = $self->git;
    my $distro     = $self->distro;
    my $build_json = $self->BUILD_json;
    my $meta       = $self->dist_meta;

    # Try to determine the primary package of this distro.
    # Let's make sure it matches the 'name' of the module.
    $build_json->{'primary'} = $meta->{'name'};
    $build_json->{'primary'} =~ s/-/::/g;
    $build_json->{'primary'} =~ s/\\?'/::/g;    # Acme::Can't

    return unless $self->is_play;               # We shouldn't alter the location of the module if we're not a play module.

    # Move the module into lib/ if we need to.
    my @module      = split( '::', $build_json->{'primary'} );
    my $module_path = join( '/', ( 'lib', @module ) ) . ".pm";
    if ( !-f $module_path ) {
        print "Primary module $module_path is in the wrong place\n";

        my $filename = $module[-1] . ".pm";
        if ( -f $filename ) {
            mkpath($module_path);
            rmdir $module_path;
            $git->mv( $filename, $module_path );

            delete $self->repo_files->{$filename};
            $self->repo_files->{$module_path} = 1;
        }

        if ( !-f $module_path ) {
            $self->dump_self;
            die("Can't find $module_path");
        }
    }

    return;
}

sub update_p5_branch_to_not_play ($self) {
    my $build_json = $self->BUILD_json;
    my $meta       = $self->dist_meta;
    $build_json->{'version'} = $meta->{'version'};
    $self->determine_primary_module;
}

sub is_extra_files_we_ship ( $self, $file ) {

    # Explicit files we're going to ignore.
    return 1 if ( grep { $file eq $_ } qw/Changelog LICENSE CONTRIBUTING Todo author.yml/ );

    # paths with example files we're going to ignore.
    return 1 if $file =~ m{^(eg|examples|ex)/};

    my $distro = $self->distro;

    # Wierd files for specific distros.
    return 1 if $file =~ m{^proto\b} && $distro =~ m/^AC-/;
    return 1 if $file =~ m{^fortune/}          && $distro eq 'Acme-24';
    return 1 if $file =~ m{^Roms/}             && $distro eq 'Acme-6502';
    return 1 if $file =~ m{^share/}            && $distro eq 'Acme-AllThePerlIsAStage';
    return 1 if $file =~ m{^ascii-art\.pl$}    && $distro eq 'Acme-AsciiArtinator';
    return 1 if $file =~ m{^demo/|unbleach.pl} && $distro eq 'Acme-Bleach';
    return 1 if $file =~ m{^testlib/}          && $distro eq 'abbreviation';

    return 0;
}

# We can assume we are checked out into the p5 branch but it is
# Indeterminate if PAUSE has merged in or if the p5 branch has been
# converted.
sub update_p5_branch_from_PAUSE ($self) {
    my $git        = $self->git;
    my $distro     = $self->distro;
    my $build_json = $self->BUILD_json;
    my $meta       = $self->dist_meta;

    $build_json->{'XS'} and die("play doesn't support XS distros");

    $self->determine_primary_module;

    # If test paths are specified in META, transfer that unless they're just t/* or t/*.t.
    if ( $meta->{'tests'} ) {
        my $tests = $meta->{'tests'};
        delete $meta->{'tests'};
        if ( $tests !~ m{^t/*\S+$} ) {
            $build_json->{'tests'} = [ map { $_ .= ".t" if m/\*$/; $_ } split( " ", $tests ) ];
        }
    }

    my $files = $self->repo_files;
    foreach my $file ( sort { $a cmp $b } keys %$files ) {
        $self->parse_code($file);
        $self->parse_comments($file);
        $self->parse_pod($file);
    }

    {    #  Look for any unexpected files in the file list.
        my %files_copy = %{ $self->repo_files };

        foreach my $script ( @{ $self->scripts } ) {
            $build_json->{'scripts'} ||= [];

            push @{ $build_json->{'scripts'} }, $script;
            delete $files_copy{$script};
        }

        #Anything in lib, t, xt is good. Just pass it through.
        delete $files_copy{$_} foreach grep { m{^lib/|^t/|^xt/} } keys %$files;

        # Files we know are ok, we'll delete from the hash.
        foreach my $file ( sort keys %files_copy ) {
            delete $files_copy{$file} if $self->is_extra_files_we_ship($file);
        }

        # Nothing was found.
        %files_copy and die( "Unexpected files found in $distro:\n" . Dumper( \%files_copy ) );
    }

    $self->generate_build_json;

    {    # Generate README.md from the primary module.
        $self->BUILD_json->{'provides'}->{ $self->BUILD_json->{'primary'} }->{'file'} or die( "Unexpected primary file location not found:\n" . Dumper $self->BUILD_json );
        my $primary_file = $self->BUILD_json->{'provides'}->{ $self->BUILD_json->{'primary'} }->{'file'};

        my $primary_file_stem = $primary_file;
        $primary_file_stem =~ s/\.pm$//;
        $primary_file = -e "$primary_file_stem.pod" ? "$primary_file_stem.pod" : $primary_file;

        my $markdown;
        my $parser = Pod::Markdown::Github->new;
        $parser->unaccept_targets(qw( html ));
        $parser->output_string( \$markdown );
        $parser->parse_string_document( $self->try_to_read_file($primary_file) );
        File::Slurper::write_text( 'README.md', $markdown );
        -f 'README.md' && !-z _ or die("Couldn't generate README.md");

        $git->add('README.md');
    }

    return;
}

sub try_to_read_file ( $self, $filename ) {
    -f $filename or return;
    -z _ and return '';

    local $@;
    my $contents;
    eval { $contents = File::Slurper::read_text($filename);   1 } and return $contents;
    eval { $contents = File::Slurper::read_binary($filename); 1 } and return $contents;
    die("Could not parse $filename: $@");
}

sub git_commit ($self) {
    my $git = $self->git;

    $git->commit( '-m', sprintf( "Update %s version %s to play.", $self->distro, $self->BUILD_json->{'version'} ) );

    $git->push if $self->push_to_github;

    return;
}

sub determine_installer ( $self ) {
    my $build_json = $self->BUILD_json;
    my $files      = $self->repo_files;

    my $builder = 'play';

    # Tag the BUILD file with whether this repo has XS.
    if ( grep { $_ =~ m/\.xs$/ } keys %$files ) {
        $build_json->{'XS'} = 1;
        $builder = 'legacy';
    }
    else {
        $build_json->{'XS'} = 0;
    }

    # We can't support Alien modules yet.
    my $meta = $self->dist_meta;
    if ( $meta->{'prereqs'} && ref $meta->{'prereqs'} eq 'HASH' ) {
        foreach my $prereq ( values %{ $meta->{'prereqs'} } ) {
            next unless defined $prereq->{'requires'}->{'Alien::Base'};
            $builder = 'legacy';
            last;
        }
    }
    $builder = 'legacy' if defined $meta->{'requires'}->{'Alien::Base'};
    $builder = 'legacy' if defined $meta->{'configure_requires'}->{'Alien::Base'};

    # Validate x_static_install matches our own decision.
    if ( defined $meta->{'x_static_install'} ) {
        if ( $meta->{'x_static_install'} ) {
            $builder eq 'play' or die('x_static_install says this distro is static but I detected things that might make it legacy');
        }
        else {
            $builder eq 'legacy' or warn(q{x_static_install says this distro isn't static but I don't know why it is play at this point?});
            $builder = 'legacy';
        }
        delete $meta->{'x_static_install'};
    }

    # If the builder is play, then set it and return.
    if ( $builder eq 'play' ) {
        $build_json->{'builder'} = 'play';
        return;
    }

    # Guess based on what builder to use based on files in the repo.
    $build_json->{'builder'} = $files->{'Build.PL'} ? 'Build.PL' : $files->{'Makefile.PL'} ? 'Makefile.PL' : die( "Can't determine builder for distro " . $self->distro );
    return;
}

sub generate_build_json ($self) {
    my $build_json = $self->BUILD_json;
    my $meta       = $self->dist_meta;

    $build_json->{'version'} = $meta->{'version'};

    # Sometimes the meta license isn't just a string. Let's normalize it.
    if ( ref $meta->{'license'} eq 'ARRAY' ) {
        $meta->{'license'} = join( ", ", @{ $meta->{'license'} } );
    }
    ref $meta->{'license'} and die( "Unexpected meta license data: " . Dumper $meta);

    # unknown isn't a valid license except when it is.
    # We have a few distros which truly don't have a meaningful license.
    if ( $meta->{'license'} && $meta->{'license'} eq 'unknown' ) {
        if ( $build_json->{'primary'} !~ m{^(ACL::Regex)$} ) {
            delete $meta->{'license'};
        }
    }

    # Use the meta license preferentially if it's there.
    if ( $meta->{'license'} ) {
        $build_json->{'license'} = $meta->{'license'};
    }
    if ( !$build_json->{'license'} ) {
        if ( -e 'LICENSE' ) {
            my $lic = $self->try_to_read_file('LICENSE');
            $build_json->{'license'} = 'perl' if $lic =~ m/Terms\s*(of|as)\s*Perl\s*itself/msi;
        }
        $build_json->{'license'} or die("Missing license for distro");
    }

    if ( %{ $self->provides } ) {
        $build_json->{'provides'} = $self->provides;
    }

    # Decode the resource urls. These have been inconsistently stored over the years.
    foreach my $resource (qw/bugtracker repository/) {
        next unless $meta->{'resources'}->{$resource};
        my $value = $meta->{'resources'}->{$resource};

        my $value_ref = ref $value;
        $value = !$value_ref ? $value : $value_ref eq 'HASH' ? $value->{'web'} : die( "Unexpected resources structure for $resource:\n" . Dumper( $meta->{'resources'} ) );

        $build_json->{$resource} = $value;
    }

    # unused vars in meta.
    delete $meta->{$_} foreach (
        qw/dynamic_config generated_by meta-spec x_generated_by_perl x_serialization_backend license resources x_deprecated
        release_status x_Dist_Zilla x_authority distribution_type installdirs version_from no_index x_contributors x_spdx_expression/
    );
    foreach my $prereq_key (qw/configure build runtime test develop/) {
        next unless $meta->{'prereqs'};
        next unless $meta->{'prereqs'}->{$prereq_key};
        next unless $meta->{'prereqs'}->{$prereq_key}->{'requires'};

        if ( $prereq_key eq 'configure' ) {
            $meta->{'configure_requires'} ||= {};
            merge_dep_into_hash( $meta->{'prereqs'}->{$prereq_key}->{'requires'}, $meta->{'configure_requires'} );
        }
        if ( $prereq_key eq 'runtime' ) {
            $meta->{'requires'} ||= {};
            merge_dep_into_hash( $meta->{'prereqs'}->{$prereq_key}->{'requires'}, $meta->{'requires'} );

            if ( $meta->{'prereqs'}->{$prereq_key}->{'recommends'} ) {
                merge_dep_into_hash( $meta->{'prereqs'}->{$prereq_key}->{'recommends'}, $self->recommends_runtime );
                delete $meta->{'prereqs'}->{$prereq_key}->{'recommends'};
            }
        }
        if ( $prereq_key eq 'test' || $prereq_key eq 'build' ) {
            $meta->{'build_requires'} ||= {};
            merge_dep_into_hash( $meta->{'prereqs'}->{$prereq_key}->{'requires'}, $meta->{'build_requires'} );
            if ( $meta->{'prereqs'}->{$prereq_key}->{'recommends'} ) {
                merge_dep_into_hash( $meta->{'prereqs'}->{$prereq_key}->{'recommends'}, $self->recommends_build );
                delete $meta->{'prereqs'}->{$prereq_key}->{'recommends'};
            }
        }
        if ( $prereq_key eq 'develop' ) {
            $meta->{'develop_requires'} ||= {};
            merge_dep_into_hash( $meta->{'prereqs'}->{$prereq_key}->{'requires'}, $meta->{'develop_requires'} );
        }

        delete $meta->{'prereqs'}->{$prereq_key}->{'requires'};
        keys %{ $meta->{'prereqs'}->{$prereq_key} } and die( "Unexpected prereqs found in $prereq_key:\n" . Dumper $meta);
    }
    prune_ref($meta);
    $meta->{'prereqs'} and die( "Unexpected prereqs found:\n" . Dumper $meta);

    my $provides = $self->provides;
    foreach my $req ( $self->requires_build, $self->requires_runtime, $self->requires_develop ) {
        foreach my $module ( keys %$req ) {
            delete $req->{$module} if $provides->{$module};
        }
    }

    # Merge in detected requires_runtime into BUILD.yaml. Validate against META as we go.
    foreach my $req (qw/requires recommends build_requires configure_requires develop_requires /) {
        my $build_req_name =
            $req eq 'requires'           ? 'requires_runtime'
          : $req eq 'recommends'         ? 'recommends_runtime'
          : $req eq 'build_requires'     ? 'requires_build'
          : $req eq 'configure_requires' ? 'requires_build'
          : $req eq 'develop_requires'   ? 'requires_develop'
          :                                die("Unknown req $req");

        my $build_req = $self->$build_req_name;

        foreach my $module ( keys %{ $meta->{$req} } ) {

            # Skip specifying our own modules in require sections.
            if ( $provides->{$module} ) {
                delete $meta->{$req}->{$module};
                next;
            }

            # These requirements never move over to p5.
            if ( $module =~ m/^(?:ExtUtils::MakeMaker|Module::Build|App::ModuleBuildTiny|Module::Build::Tiny|(inc::)?Module::Install|Module::Build::Pluggable(?:::.+)?|ExtUtils::MakeMaker::CPANfile|Module::Install|strict|warnings|version|lib)$/ ) {
                delete $meta->{$req}->{$module};
                next;
            }

            # special handling for minimum perl version.
            if ( $module eq 'perl' ) {
                $build_req->{$module} = $meta->{$req}->{$module};
                delete $meta->{$req}->{$module};
                next;
            }

            # Let's not be so strict on develop requires. Maybe they know what they're talking about.
            # Also who cares? play doesn't use develop_requires right now.
            if ( !exists $build_req->{$module} && $req eq 'develop_requires' ) {
                $build_req->{$module} = $meta->{$req}->{$module};
                delete $meta->{$req}->{$module};
                next;
            }

            # Add a recommends section.
            if ( !exists $build_req->{$module} && $req eq 'requires' ) {
                $build_req->{$module} = $meta->{$req}->{$module};
                delete $meta->{$req}->{$module};
                next;
            }

            if ( !exists $build_req->{$module} ) {
                if ( !$self->is_play or $req eq 'recommends' ) {    # Just pass it through.
                    $build_req->{$module} = $meta->{$req}->{$module};
                    delete $meta->{$req}->{$module};
                    next;
                }
                elsif ( $module !~ m{^(CPAN::Meta|CPAN::Meta::Prereqs|Test::Pod|Test::MinimumVersion|Test::MinimumVersion::Fast|Test::PAUSE::Permissions|Test::Spellunker|Test::CPAN::Meta|Software::License|Catalyst)$} ) {    # Ignore stuff that's probably release testing.
                    die( "META specified requirement '$module' for '$req' was not detected.\n" . Dumper( $self->requires_develop, $self->requires_build, $meta ) );
                }
            }

            # Upgrade the version required if some META specified a version.
            if ( $meta->{$req}->{$module} ) {
                my $meta_version = version->parse( $meta->{$req}->{$module} );
                if ( $meta_version > 0 ) {
                    if ( $build_req->{$module} && version->parse( $build_req->{$module} ) < $meta_version ) {
                        $build_req->{$module} = $meta->{$req}->{$module};
                    }
                }
            }
            delete $meta->{$req}->{$module};
        }

        %{ $meta->{$req} } and die("Modules still listed in req $req");

        $build_json->{$build_req_name} = $build_req;
        delete $meta->{$req};
    }

    # Sometimes we detect a recommends runtime we want to process.
    $build_json->{'recommends_runtime'} = $self->recommends_runtime if %{ $self->recommends_runtime };
    $build_json->{'recommends_build'}   = $self->recommends_build   if %{ $self->recommends_build };

    # Where this branch got its data from.
    $build_json->{'source'} = 'PAUSE';

    $build_json->{'builder_API_version'} = '1';

    $build_json->{'primary'} or die("Never determined primary module name?");
    $build_json->{'name'} = $build_json->{'primary'};
    $build_json->{'name'} =~ s/::/-/g;

    if ( $meta->{'author'} ) {
        my $author = $meta->{'author'};
        if ( ref $author eq 'ARRAY' ) {
            $build_json->{'maintainers'} = $author;
        }
        elsif ( !ref $author ) {
            $build_json->{'maintainers'} = [$author];
        }
        delete $meta->{'author'};
    }

    $build_json->{'maintainers'} or die("Could not determine maintainers for this repo");

    # Verify everything we think we figured out matches META.
    if ( length $meta->{'abstract'} && $meta->{'abstract'} ne 'unknown' ) {

        if ( $build_json->{'abstract'} ) {
            $build_json->{'abstract'} eq $meta->{'abstract'} or warn( sprintf( "META abstract does not match what's in the module. I'm going to trust what is in META. meta=%s lib=%s\n", $meta->{'abstract'}, $build_json->{'abstract'} ) );
        }
        $build_json->{'abstract'} = $meta->{'abstract'};
        delete $meta->{'abstract'};
    }
    elsif ( exists $meta->{'abstract'} ) {
        delete $meta->{'abstract'};
    }

    if ( $meta->{'provides'} ) {
        foreach my $module ( sort { $a cmp $b } keys %{ $meta->{'provides'} } ) {
            $provides->{$module}                                                                  or die( "Meta provides $module but it was not detected: " . Dumper( $meta, $provides ) );
            $meta->{'provides'}->{$module}->{'file'} eq $provides->{$module}->{'file'}            or die( "Meta provides $module file is not the same as was detected: " . Dumper( $meta, $provides ) );
            $meta->{'provides'}->{$module}->{'version'} // 0 eq $provides->{$module}->{'version'} or die( "Meta provides $module version is not the same as was detected: " . Dumper( $meta, $provides ) );
            delete $meta->{'provides'}->{$module};
        }
        prune_ref($meta);
    }

    # Validate name detection worked.
    $meta->{'name'} or die( "No name for distro?\n" . Dumper($meta) );
    $meta->{'name'} =~ s/\\?'/-/g;    # Acme::Can't
    $build_json->{'name'} eq $meta->{'name'} or die( "Bad detection of name?\n" . Dumper( $meta, $build_json ) );
    delete $meta->{'name'};

    # Validate we detected version correctly.
    $meta->{'version'} or die( "No version for distro?\n" . Dumper($meta) );
    $build_json->{'version'} eq $meta->{'version'} or die( "Bad detection of version?\n" . Dumper( $meta, $build_json ) );
    delete $meta->{'version'};

    if ( $build_json->{'builder'} eq 'play' && %$meta ) {
        $self->dump_self;
        die("Unparsed information still exists in dist_meta. Please review.");
    }

    File::Slurper::write_text( $self->BUILD_file, Cpanel::JSON::XS->new->pretty->canonical( [1] )->encode($build_json) );
    $self->git->add( $self->BUILD_file );

    return;
}

sub merge_dep_into_hash ( $from, $to ) {
    ref $from eq 'HASH' or die ref $from;
    ref $to eq 'HASH'   or die ref $to;

    foreach my $key ( keys %$from ) {
        next if $to->{$key} && $to->{$key} + 0 > $from->{$key} + 0;
        $to->{$key} = $from->{$key};
    }
}

sub prune_ref ($var) {
    my $type = ref $var;
    if ( $type eq 'HASH' ) {
        foreach my $key ( keys %$var ) {
            my $value      = $var->{$key};
            my $value_type = ref $value;
            if ($value_type) {
                prune_ref($value);
            }
            delete $var->{$key} if ( $value_type eq 'ARRAY' && !@{$value} );
            delete $var->{$key} if ( $value_type eq 'HASH'  && !%{$value} );
        }
    }
    return;
}

sub get_ppi_doc ( $self, $filename ) {
    return if $filename =~ m{\.(bak|yml|json|yaml|txt|out)$}i;
    return if $self->is_extra_files_we_ship($filename);

    if ( -l $filename || -d _ || -z _ ) {
        warn("$filename isn't a normal file. Skipping PPI parse");
        return;
    }

    return $self->ppi_cache->{$filename} if $self->ppi_cache->{$filename};

    print "PPI $filename\n";
    my $content = $self->try_to_read_file($filename);

    return $self->ppi_cache->{$filename} = PPI::Document->new( \$content );
}

sub _file_to_package ( $self, $filename ) {
    $filename or die;
    $filename =~ s{^lib/}{};
    $filename =~ s{\.(pm|pod)$}{};
    $filename =~ s{/}{::}g;
    return $filename;
}

sub is_xt_test ( $self, $filename ) {
    return 0 unless $filename =~ m{^t/.+\.t$};

    my $doc = $self->get_ppi_doc($filename) or return 0;

    # remove pods
    my $quotes = $doc->find( sub { $_[1]->class =~ m/^PPI::Token::Quote::/ } ) || [];

    # Look for comments about this being an author test.
    foreach my $quote (@$quotes) {
        my $content = $quote->content;
        return 1 if $content =~ m{SKIP these tests are for testing by the author};
    }

    return 0;
}

sub parse_maker ($self) {
    if ( -e 'Makefile.PL' ) {
        my $doc = $self->get_ppi_doc('Makefile.PL');

        my $quotes = $doc->find( sub { $_[1]->class =~ m/^PPI::Token::Quote::|^PPI::Token::Word$/ && $_[1]->content =~ m{^['"]?EXE_FILES['"]?$} } ) || [];

        if (@$quotes) {    # EU::MM Format.
            my $node = $quotes->[0];
            $node = $node->snext_sibling();
            $node->content eq '=>' or die( "Unexpected sibling to EXE_FILES: " . dump_tree( $quotes->[0] ) );

            $node = $node->snext_sibling();
            $node->class eq 'PPI::Structure::Constructor' or die( "Unexpected sibling value EXE_FILES: " . dump_tree($node) );

            my ($script_nodes) = $node->schildren;
            $script_nodes or return;    # No EXE_FILES in the list.
            foreach my $script_node ( $script_nodes->children ) {
                next   if $script_node->class eq 'PPI::Token::Operator';
                next   if $script_node->class eq 'PPI::Token::Whitespace';
                return if $script_node->class eq 'PPI::Token::Word';         # Looks like code not a list. Forget it!
                $script_node->class =~ m{^PPI::Token::Quote::} or die( "Unexpected child node parsing EXE_FILES: " . Dumper($script_node) );

                my ($script) = $script_node->content =~ m/^\s*['"](.+)['"]\s*$/;
                push @{ $self->scripts }, $script || $script_node->content;

            }
        }
        return if @{ $self->scripts };                                       # Found Scripts!

        # Look for Module::Install stuff.
        my $statements = $doc->find( sub { $_[1]->class eq 'PPI::Statement' } ) || [];
        foreach my $statement_node (@$statements) {
            my $node = $statement_node->schild(0);
            $node->class eq 'PPI::Token::Word' && $node->content eq 'install_script' or next;
            $node = $node->snext_sibling;
            $node->class =~ m/^PPI::Token::Quote/ or die( "Unexpected token after install_script: " . Dumper($node) );
            my $script = $node->content;
            $script =~ s/^\s*['"](.+)['"]\s*$/$1/;
            push @{ $self->scripts }, $script;
        }
    }
}

sub parse_comments ( $self, $filename ) {

    my $doc = $self->get_ppi_doc($filename) or return;

    # remove pods
    my $comments = $doc->find('PPI::Token::Comment') || [];

    # Search for abstract if the file is the primary package
    my $primary_package = $self->BUILD_json->{'primary'};
    if ( $primary_package eq $self->_file_to_package($filename) ) {
        foreach my $comment (@$comments) {
            next unless $primary_package eq $self->_file_to_package($filename);
            next unless $comment->content =~ m/^# ABSTRACT:\s+(\S.+$)/;
            $self->BUILD_json->{'abstract'} = "$1";
            chomp $self->BUILD_json->{'abstract'};
            last;
        }
    }
}

sub parse_pod ( $self, $filename ) {
    return unless $filename =~ m/\.(?:pl|pm|pod)$/i;    # only perl files for pod.

    my $doc = $self->get_ppi_doc($filename) or return;

    my $primary_package = $self->BUILD_json->{'primary'};

    # remove pods
    my $pods = $doc->find('PPI::Token::Pod') || [];

    my $abstract;
    my @author;
    my $license_data     = '';
    my $copyright_author = '';

    foreach my $pod (@$pods) {
        my @pod_lines = split( "\n", $pod->content );
        while (@pod_lines) {
            my $line = shift @pod_lines;
            if ( $line =~ m{^=head1 NAME} ) {
                while ( @pod_lines && $pod_lines[0] && $pod_lines[0] !~ m/^=/ ) {
                    my $line = shift @pod_lines;
                    next unless $line =~ m/^\s*\Q$primary_package\E(?:.pm)?\s+-\s+(.+)/;    # Skip empt
                    $abstract = $1;
                    $abstract =~ s/\s+$//;                                                  # Strip off trailing white space.

                    $self->BUILD_json->{'abstract'} = $abstract;
                    last;
                }
            }
            if ( $line =~ m{^=head1 AUTHOR} ) {
                while ( @pod_lines && $pod_lines[0] !~ m/^=/ ) {
                    my $line = shift @pod_lines;
                    next unless $line =~ m/\S/;
                    if ( $line =~ m/copyright|terms|disclaimers of warranty|free software|redistribute|itself/i ) {
                        $license_data .= $line;
                        next;
                    }
                    $line =~ s/^\s+//;
                    $line =~ s/\s+\z//;
                    if ( !$self->BUILD_json->{'maintainers'} ) {
                        $self->BUILD_json->{'maintainers'} = \@author;
                    }
                    push @author, $line;
                }
            }
            if ( $line =~ m{^=head1 (COPYRIGHT|LICENSE|LICENCE)|^=head1 .+ E COPYRIGHT}i ) {
                while ( @pod_lines && $pod_lines[0] !~ m/^=/ ) {
                    my $line = shift @pod_lines;
                    next unless $line =~ m/\S/;
                    $license_data .= " $line";
                }
            }

            if ( $license_data && !$self->BUILD_json->{'license'} ) {
                $license_data =~ s/\s\s+/ /msg;    # Strip double spaces to make parsing the text easier.
                $license_data =~ s/\s/ /msg;       # Convert all white space to a single space.

                if ( $license_data =~ m/or the Artistic License/msi ) {
                    $self->BUILD_json->{'license'} = 'perl';
                }
                elsif ( $license_data =~ m/Terms (of|as) Perl itself/msi ) {
                    $self->BUILD_json->{'license'} = 'perl';
                }
                elsif ( $license_data =~ m/same as Perl itself/msi ) {
                    $self->BUILD_json->{'license'} = 'perl';
                }
                elsif ( $license_data =~ m/L<perlartistic>/msi ) {
                    $self->BUILD_json->{'license'} = 'perl';
                }
                elsif ( $license_data =~ m/under the terms of the Perl Artistic License/msi ) {
                    $self->BUILD_json->{'license'} = 'perl';
                }
                elsif ( $license_data =~ m/under the terms of GNU General Public License \(GPL\)/msi ) {
                    $self->BUILD_json->{'license'} = 'GPL';
                }
                elsif ( $license_data =~ m/under the terms of GNU General Public License 3/msi ) {
                    $self->BUILD_json->{'license'} = 'GPLv3+';
                }
                elsif ( $license_data =~ m/MIT License/msi ) {
                    $self->BUILD_json->{'license'} = 'MIT';
                }
                elsif ( $license_data =~ m/under the terms of the Apache 2.0 license/msi ) {
                    $self->BUILD_json->{'license'} = 'Apache_2_0';
                }
                elsif ( $license_data =~ m/L<Software::License::(\S+)>/msi ) {
                    $self->BUILD_json->{'license'} = "$1";
                }
                elsif ( $license_data =~ m/into the public domain/msi ) {
                    $self->BUILD_json->{'license'} = "PublicDomain";
                }

                else {
                    1    #die "Unknown license: $license_data==\n";
                }
                $license_data = '';    # Clear it for the next check.
            }
        }
    }
}

sub parse_code ( $self, $filename ) {

    my $requires_runtime_hash;
    $requires_runtime_hash = $self->requires_build   if $filename =~ m{^t/.+\.(pl|pm|t)$}i;
    $requires_runtime_hash = $self->requires_runtime if $filename =~ m{\.pm$} && $filename !~ m{^t/};
    $requires_runtime_hash = $self->requires_develop if $filename =~ m{^xt/.+\.t$};
    return unless $requires_runtime_hash;    # Doesn't look like perl code we know about.

    my $doc = $self->get_ppi_doc($filename) or return;

    # find use/require statements and parse them.
    my $use_find = $doc->find('PPI::Statement::Include') || [];
    foreach my $use_node (@$use_find) {
        my $module = get_package_usage($use_node) or next;
        $requires_runtime_hash->{$module} = 0;
    }

    my $primary_module = $self->BUILD_json->{'primary'};

    # Find packages that are provides.
    if ( $filename =~ m{\.pm$} ) {
        my $packages_find = $doc->find('PPI::Statement::Package') || [];
        foreach my $pkg_token (@$packages_find) {
            my $module = get_package_provided($pkg_token) or next;

            $self->provides->{$module}->{'file'}    = $filename;
            $self->provides->{$module}->{'version'} = 0;

            # Try to determine the VERSION value in each package.
            while ( $pkg_token = $pkg_token->snext_sibling ) {
                my $class = $pkg_token->class;
                last if $class eq 'PPI::Statement::Package';
                next unless $class eq 'PPI::Statement::Variable';

                my $node = $pkg_token->find( sub { $_[1]->content eq '$VERSION' && $_[1]->class eq 'PPI::Token::Symbol' } ) or next;
                $node = $node->[0];
                $node = $node->snext_sibling;
                $node->class eq 'PPI::Token::Operator' or die dump_tree( $pkg_token, "Unexpected operator in VERSION statement" );
                $node->content eq '='                  or die dump_tree( $pkg_token, "Unexpected operator in VERSION statement" );
                $node = $node->snext_sibling;

                my $version;

                # Try to handle all the stupid things people have done with version lines :(
                if ( $node->class =~ m/^PPI::Token::(Quote|Number)/ ) {
                    $version = $node->content;
                    $version =~ s/['"]//g;
                }
                elsif ( $node->class eq 'PPI::Token::Word' && $node->content eq 'qv' ) {    # our $version = qv{v0.0.2}
                    $node = $node->snext_sibling;
                    $node->class eq 'PPI::Structure::List' or die dump_tree($node);

                    $node = $node->schild(0);
                    $node->class eq 'PPI::Statement::Expression' or die dump_tree($node);

                    $node = $node->schild(0);
                    $node->class =~ m/^PPI::Token::Quote::/ or die dump_tree($node);

                    $version = $node->content;
                    $version =~ s/^\s*['"](.+)['"]\s*$/v$1/;                                # Make it a v-string since that's what they were going for.
                }
                elsif ( $node->class eq 'PPI::Token::Word' && $node->content eq 'version' ) {    # our $VERSION = version->declare('v0.2.2');
                    $node = $node->snext_sibling;                                                # ->
                    $node->class eq 'PPI::Token::Operator' or die dump_tree($node);

                    $node = $node->snext_sibling;                                                # declare
                    $node->class eq 'PPI::Token::Word' && $node->content eq 'declare' or die dump_tree($node);

                    $node = $node->snext_sibling;                                                # ( ... )
                    $node->class eq 'PPI::Structure::List' or die dump_tree($node);

                    $node = $node->schild(0);
                    $node->class eq 'PPI::Statement::Expression' or die dump_tree($node);

                    $node = $node->schild(0);
                    $node->class =~ m/^PPI::Token::Quote::/ or die dump_tree($node);

                    $version = $node->content;
                    $version =~ s/^\s*['"](.+)['"]\s*$/v$1/;                                     # Make it a v-string since that's what they were going for.
                }
                elsif ( $node->class eq 'PPI::Token::Symbol' && $node->content =~ m/"?\$\Q$primary_module\E::VERSION"?$/ ) {    # our $VERSION = $accessors::fast::VERSION;
                    if ( !$self->provides->{$primary_module} ) {
                        ...;
                    }
                    $version = $self->provides->{$primary_module}->{'version'};
                }
                else {
                    #                    my $str = $pkg_token->content;
                    #                    my ($version) = $str =~ m/sprintf.+Revision: ([0-9]+\.[0-9]+)/;

                    $self->dump_self;
                    $version or die sprintf( "TOKEN (%s=%s): %s--\n", $node->class, $node->content, $pkg_token->content ) . dump_tree( $pkg_token, "Unexpected content in VERSION statement" );
                }

                $self->provides->{$module}->{'version'} = $version;
                last;
            }
        }
    }
}

sub get_package_provided ($element) {
    my $token = $element->first_token;
    $token eq 'package' or die( dump_tree( $element, "$token not a package?" ) );

    # Package statements where the package is on a different line than the package indicates that they are trying to hide it from the PAUSE parser.
    # We're going to do the same.
    $token = $token->next_sibling;
    if ( $token->class eq 'PPI::Token::Whitespace' && $token->content eq "\n" ) {
        print "Skipping detected package '$element' because of new line after package\n";
        return;
    }

    $token = $token->snext_sibling;
    $token and $token->class eq 'PPI::Token::Word' or die( dump_tree($element) );

    my $package = $token->content;
    $package =~ s/\\?'/::/g;         # Acme::Can't
    return if $package eq 'main';    # main is not a supported package on CPAN.

    return $package;
}

sub get_package_usage ($element) {
    my $top = $element;

    my $token       = $element->first_token;
    my $require_str = $token->content;
    return if ( $require_str eq 'no' );    # Not an include usually for 'no warnings...'
    $token =~ /^(use|require)$/ or die( dump_tree( $element, "$token not a sub?" ) );
    my $is_use = $1;

    $token = $token->snext_sibling;

    return if ( $token->content =~ m/^5\.\d+/ );    # skip use 5.xx
    if ( $token->content =~ m/^\s*\$/ ) {           # Dynamic require can't be parsed.
        printf( "Failed to parse require: %s\n", $token->content );
        return;
    }

    my $module = $token->content;
    return if $module eq 'main';                    # Main is not a legal CPAN package.
    $module =~ s/'/::/g;                            # Acme::Can't

    # use base 'accessors';
    if ( $is_use eq 'use' and $module =~ /^(base|parent)$/ ) {
        $token = $token->snext_sibling;
        $token->class =~ m/^PPI::Token::Quote::/ or die dump_tree( $top, "Unexpected sequence parsing use parent/base" );
        $module = strip_quotes( $token->content );
        return $module;
    }

    # no imports for require.
    return $module if ( $is_use ne 'use' );

    # We need to know if imports are being blocked.
    $token = $token->snext_sibling or die( dump_tree( $top, "Should never happen?" ) );
    if ( $token->class eq 'PPI::Structure::List' && !$token->child(0) ) {
        return ($module);
    }

    # A module is loaded here but it allows imports (BOO!)
    return $module;
}

sub strip_quotes ($string) {
    $string =~ s/^(['"])(.+)\1\z/$2/ms;
    return $string;
}

sub dump_tree ( $element, $die_msg = '' ) {
    my $dump = PPI::Dumper->new($element);
    $dump->print;
    return $die_msg ? $die_msg : ();
}

1;
