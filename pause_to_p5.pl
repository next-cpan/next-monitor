#!/perl/bin/perl

package Perl::Distro;

use strict;
use warnings;

use FindBin;
use Moose;

use experimental 'signatures';

use version          ();
use Git::Wrapper     ();
use File::Slurper    ();
use Cwd::Guard       ();
use Cpanel::JSON::XS ();
use CPAN::Meta::YAML ();
use File::Path qw/mkpath/;

use PPI::Document ();
use PPI::Dumper   ();

use Carp;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

has 'distro'         => ( isa => 'Str',  is => 'ro', required => 1 );
has 'repo_path'      => ( isa => 'Str',  is => 'ro', required => 1 );
has 'git_binary'     => ( isa => 'Str',  is => 'ro', required => 1 );
has 'push_to_github' => ( isa => 'Bool', is => 'ro', required => 1 );

has 'git'       => ( isa => 'Object',  lazy => 1,    is   => 'ro', lazy    => 1, builder => '_build_git' );
has 'dist_meta' => ( isa => 'HashRef', is   => 'rw', lazy => 1,    builder => '_build_meta' );

has 'requires_runtime' => ( isa => 'HashRef', is => 'rw', default => sub { return {} } );
has 'requires_develop' => ( isa => 'HashRef', is => 'rw', default => sub { return {} } );
has 'provides'         => ( isa => 'HashRef', is => 'rw', default => sub { return {} } );
has 'requires_build'   => ( isa => 'HashRef', is => 'rw', default => sub { return {} } );
has 'ppi_cache'        => ( isa => 'HashRef', is => 'rw', default => sub { return {} } );
has 'BUILD_json'       => ( isa => 'HashRef', is => 'rw', default => sub { return {} } );
has 'BUILD_file'       => ( isa => 'Str',     is => 'rw', default => 'BUILD.json' );

sub _build_git ($self) {
    return Git::Wrapper->new( { 'dir' => $self->repo_path, 'git_binary' => $self->git_binary } ) || die( 'Failed to create Git::Wrapper for ' . $self->repo_path );
}

sub _build_meta ($self) {
    if ( -f 'META.json' ) {
        return Cpanel::JSON::XS::decode_json( File::Slurper::read_binary('META.json') );
    }
    if ( -f 'META.yml' ) {
        my $txt  = File::Slurper::read_binary('META.yml');
        my $yaml = CPAN::Meta::YAML->read_string($txt);
        return $yaml->[0] if $yaml && ref $yaml eq 'CPAN::Meta::YAML';
    }

    die( 'No META data found in ' . $self->distro . "\n" . `ls -l` . "\n\n" . Carp::longmess );
}

sub check_if_dirty_and_die ($self) {
    my $st = $self->git->status;
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

    my $repo_path = $self->git->dir;
    die("\nThe repo $repo_path is unexpectedly dirty. Please correct this:\n$txt\n");
}

sub checkout_p5_branch ($self) {
    my $git = $self->git;

    my @branches = eval { $git->branch };

    # the p5 branch is already there. Just switch to it.
    if ( grep { $_ =~ m/^\s*\*?\s+p5\z/ } @branches ) {
        eval { $git->checkout( '-f', 'p5' ) };
        return;
    }

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

    # We haven't generated the p5 branch we need to fork it from PAUSE.

    # Make sure there's a PAUSE branch.
    if ( !grep { $_ =~ m/^\s*\*?\s+PAUSE\z/ } @branches ) {
        die( "Cannot create a p5 branch because PAUSE is missing for repo " . $git->dir );
    }

    $git->checkout(qw{-b p5});
    $git->reset(qw{--hard PAUSE});
    $git->push(qw{--set-upstream origin p5}) if $self->push_to_github;
}

sub fix_special_repos ( $self, $distro ) {
    return;    # Nothing is special case yet.

    #    my $meta = $self->dist_meta;
    #    my $name = $meta->{'name'} or return;
    #    return unless grep {$_ eq $name} qw//;
}

# We can assume we are checked out into the p5 branch but it is
# Indeterminate if PAUSE has merged in or if the p5 branch has been
# converted.
sub update_p5_branch_from_PAUSE ( $self, $distro ) {
    my $git = $self->git;

    my @log = $git->log(qw/p5..PAUSE/);

    # Has the PAUSE branch been updated since p5 was last merged from it?
    if (@log) {
        die(q{We need to determine how we're going to merge from PAUSE once we've updated p5 once.});
    }
    elsif ( -e $self->BUILD_file ) {
        print "Nothing to update\n";

        # There are no new changes and BUILD.yaml has already been created.
        return;
    }

    my $build_json = $self->BUILD_json;
    my $meta       = $self->dist_meta;

    $build_json->{'version'} = $meta->{'version'};

    my $builder = $build_json->{'builder'} = $self->determine_installer($distro);

    # Try to determine the primary package of this distro.
    # Let's make sure it matches the 'name' of the module.
    if ( $builder eq 'play' ) {
        $build_json->{'primary'} = $meta->{'name'};
        $build_json->{'primary'} =~ s/-/::/g;
        my @module      = split( '::', $build_json->{'primary'} );
        my $module_path = join( '/', ( 'lib', @module ) ) . ".pm";
        if ( !-f $module_path ) {
            print "Primary module $module_path is in the wrong place\n";

            my $filename = $module[-1] . ".pm";
            if ( -f $filename ) {
                mkpath($module_path);
                rmdir $module_path;
                $git->mv( $filename, $module_path );
            }

            if ( !-f $module_path ) {
                $self->ppi_cache( {} );
                die( "Can't find $module_path\n" . Dumper($self) );
            }
        }
        my $module_txt = File::Slurper::read_binary($module_path);
        my ($package) = $module_txt =~ m{package\s+(\S+?)\s*;};
        $build_json->{'primary'} eq $package or die("Unexpected distro name / primary mismatch in distro $distro");
    }
    else {
        $build_json->{'primary'} = $meta->{'name'};
        $build_json->{'primary'} =~ s/-/::/g;
    }

    # If test paths are specified in META, transfer that unless they're just t/* or t/*.t.
    if ( $meta->{'tests'} ) {
        my $tests = $meta->{'tests'};
        delete $meta->{'tests'};
        if ( $tests !~ m{^t/*\S+$} ) {
            $build_json->{'tests'} = $tests;
        }
    }

    my %files = map { ( $_ => 1 ) } $git->ls_files;
    $build_json->{'XS'} = 1 if grep { $_ =~ m/\.xs$/ } keys %files;

    foreach my $file ( sort { $a cmp $b } keys %files ) {
        $self->parse_code($file);
        $self->parse_comments($file);
        $self->parse_pod($file);
    }

    # Re-work the file tree.
    if ( $builder eq 'play' ) {

        $build_json->{'XS'} and die("play doesn't support XS distros");

        # Do we have local files?
        if ( grep { m{^[^/]+\.pm$} } keys %files ) {
            ...;    # there is a lib file in the base directory and we need to re-locate it.
        }

        foreach my $unwanted_file (
            qw/MANIFEST MANIFEST.SKIP SIGNATURE dist.ini README README.md Makefile.PL Build.PL
            META.yml META.json ignore.txt .gitignore Changes.PL cpanfile cpanfile.snapshot minil.toml
            /
        ) {
            next unless $files{$unwanted_file};
            delete $files{$unwanted_file};
            $git->rm($unwanted_file);
        }

        # Normalize all TODO Todo files to be named Todo
        my @todo = sort grep { $_ =~ m/^todo$/i } keys %files;
        if (@todo) {
            scalar @todo == 1 or die( "Too many TODO files.\n" . Dumper( \%files ) );
            my $todo = shift @todo;
            if ( $todo ne 'Todo' ) {
                $git->mv( $todo, 'Todo' );
            }

            # See if the TODO is worthless.
            my $content = File::Slurper::read_binary('Todo');
            if ( $content =~ m/- Nothing yet/ms ) {
                $git->rm( '-f', 'Todo' );
            }

            delete $files{$todo};
        }

        if ( $files{'Changes'} ) {
            $git->mv( 'Changes', 'Changelog' );
            delete $files{'Changes'};
        }

        # Remove files which don't ship with p5.
        foreach my $file ( keys %files ) {
            next unless $file =~ m{^(?:inc|proto)/};
            delete $files{$file};
            $git->rm($file);
        }

        # Files we know are ok, we'll delete from the hash.
        foreach my $file ( sort keys %files ) {

            # Explicit files we're going to ignore.
            delete $files{$file} if ( grep { $file eq $_ } qw/Changelog LICENSE proto CONTRIBUTING/ );

            # paths with example files we're going to ignore.
            delete $files{$file} if $file =~ m{^(eg|examples)/};

            # Wierd files found in Acme
            delete $files{$file} if $file =~ m{^fortune/};
        }

        delete $files{$_} foreach grep { m{^lib/|^t/} } keys %files;

        # Nothing was found.
        %files and die( "Unexpected files found in repository:\n" . Dumper( \%files ) );

    }

    $self->generate_build_json;

    if ( $builder eq 'play' ) {    # Generate README.md from the primary module.
        $self->BUILD_json->{'provides'}->{ $self->BUILD_json->{'primary'} }->{'file'} or die( "Unexpected primary file location not found:\n" . Dumper $self->BUILD_json );
        my $primary_file = $self->BUILD_json->{'provides'}->{ $self->BUILD_json->{'primary'} }->{'file'};
        my $got          = `pod2markdown '$primary_file' README.md 2>&1`;
        -f 'README.md' && !-z _ or die("Couldn't generate README.md: $got");

        $git->add('README.md');
    }

    $git->add( $self->BUILD_file );

    $git->commit( '-m', sprintf( "Update %s version %s to play.", $distro, $self->BUILD_json->{'version'} ) );
    $self->push_to_github and die("Publishing to github should be off");
    $git->push if $self->push_to_github;
}

sub determine_installer ( $self, $repo ) {
    my $git = $self->git;

    my @files = $git->ls_files;

    return 'play' unless grep { $_ =~ m/\.xs$/i } @files;

    # Guess based on what files are there.
    return -e 'Build.PL' ? 'Build.PL' : -e 'Makefile.PL' ? 'Makefile.PL' : die( "Can't determine builder for distro " . $self->distro );
}

sub generate_build_json ($self) {

    my $build_json = $self->BUILD_json;
    my $meta       = $self->dist_meta;

    # copy the license across.
    #    $build_json->{'license'}     = $meta->{'license'};
    if ( $meta->{'license'} ) {
        $build_json->{'license'} = $meta->{'license'};
    }
    if ( !$build_json->{'license'} ) {
        if ( -e 'LICENSE' ) {
            my $lic = File::Slurper::read_binary('LICENSE');
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
        qw/dynamic_config generated_by meta-spec x_generated_by_perl x_serialization_backend license resources
        release_status x_Dist_Zilla x_authority distribution_type installdirs version_from no_index/
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
        }
        if ( $prereq_key eq 'test' || $prereq_key eq 'build' ) {
            $meta->{'build_requires'} ||= {};
            merge_dep_into_hash( $meta->{'prereqs'}->{$prereq_key}->{'requires'}, $meta->{'build_requires'} );
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
    foreach my $req ( $self->requires_build, $self->requires_runtime ) {
        foreach my $module ( keys %$req ) {
            delete $req->{$module} if $provides->{$module};
        }
    }

    # Merge in detected requires_runtime into BUILD.yaml. Validate against META as we go.
    foreach my $req (qw/requires build_requires configure_requires develop_requires /) {
        my $build_req_name =
            $req eq 'requires'           ? 'requires_runtime'
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
            if ( $module =~ m/^(?:ExtUtils::MakeMaker|Module::Build|Module::Build::Tiny|Module::Install|strict|warnings)$/ ) {
                delete $meta->{$req}->{$module};
                next;
            }

            # Add a recommends section.
            if ( !exists $build_req->{$module} && $req eq 'requires' ) {
                $build_json->{'recommends'}->{$module} = $meta->{$req}->{$module};
                delete $meta->{$req}->{$module};
                next;
            }
            if ( !exists $build_req->{$module} ) {
                if ( $module !~ m{^(Test::Pod|Test::MinimumVersion::Fast|Test::PAUSE::Permissions|Test::Spellunker|Test::CPAN::Meta)$} ) {    # Ignore stuff that's probably release testing.
                    die( "META specified requirement '$module' for '$req' was not detected.\n" . Dumper( $build_req, $meta ) );
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
    if ( $meta->{'abstract'} ) {
        if ( $build_json->{'abstract'} ) {
            $build_json->{'abstract'} eq $meta->{'abstract'} or die( "Bad detection of abstract?\n" . Dumper( $meta, $build_json ) );
        }
        else {
            $build_json->{'abstract'} = $meta->{'abstract'};
        }
        delete $meta->{'abstract'};
    }

    # Validate name detection worked.
    $meta->{'name'} or die( "No name for distro?\n" . Dumper($meta) );
    $build_json->{'name'} eq $meta->{'name'} or die( "Bad detection of name?\n" . Dumper( $meta, $build_json ) );
    delete $meta->{'name'};

    # Validate we detected version correctly.
    $meta->{'version'} or die( "No version for distro?\n" . Dumper($meta) );
    $build_json->{'version'} eq $meta->{'version'} or die( "Bad detection of version?\n" . Dumper( $meta, $build_json ) );
    delete $meta->{'version'};

    if ( $build_json->{'builder'} eq 'play' && %$meta ) {
        $self->ppi_cache( {} );
        die( "Unparsed information still exists in dist_meta. Please review.\n" . Dumper($self) );
    }

    File::Slurper::write_binary( $self->BUILD_file, Cpanel::JSON::XS->new->pretty->canonical( [1] )->encode($build_json) );
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
    return if -l $filename || -d _ || -z _;

    return $self->ppi_cache->{$filename} if $self->ppi_cache->{$filename};

    my $content = File::Slurper::read_binary($filename);
    if ( $content =~ m/use\s+utf8/ && $content !~ m/no utf8/ ) {
        $content = File::Slurper::read_text($filename);
    }
    return $self->ppi_cache->{$filename} = PPI::Document->new( \$content );
}

sub _file_to_package ( $self, $filename ) {
    $filename or die;
    $filename =~ s{^lib/}{};
    $filename =~ s{\.pm$}{};
    $filename =~ s{/}{::}g;
    return $filename;
}

sub parse_comments ( $self, $filename ) {
    return unless $filename =~ m/\.(?:pm)$/i;    # only perl files for pod.
    return if $self->BUILD_json->{'abstract'};   # We don't look for anything but this right now.

    my $primary_package = $self->BUILD_json->{'primary'};
    return unless $primary_package eq $self->_file_to_package($filename);

    my $doc = $self->get_ppi_doc($filename);

    # remove pods
    my $comments = $doc->find('PPI::Token::Comment') || [];
    foreach my $comment (@$comments) {
        next unless $comment->content =~ m/^# ABSTRACT:\s+(\S.+$)/;
        $self->BUILD_json->{'abstract'} = "$1";
        chomp $self->BUILD_json->{'abstract'};
        last;
    }
}

sub parse_pod ( $self, $filename ) {
    return unless $filename =~ m/\.(?:pl|pm|pod)$/i;    # only perl files for pod.

    my $doc = $self->get_ppi_doc($filename);

    my $primary_package = $self->BUILD_json->{'primary'};

    # remove pods
    my $pods = $doc->find('PPI::Token::Pod') || [];

    my $abstract;
    my @author;
    my $license_data = '';

    foreach my $pod (@$pods) {
        my @pod_lines = split( "\n", $pod->content );
        while (@pod_lines) {
            my $line = shift @pod_lines;
            if ( $line =~ m{^=head1 NAME} ) {
                while ( @pod_lines && $pod_lines[0] !~ m/^=/ ) {
                    my $line = shift @pod_lines;
                    next unless $line =~ m/^\s*\Q$primary_package(?:.pm)?\E\s+-\s+(.+)/;    # Skip empt
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
                    if ( $line =~ m/copyright|terms|disclaimers of warranty|free software/i ) {
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
            if ( $line =~ m{^=head1 COPYRIGHT}i ) {
                while ( @pod_lines && $pod_lines[0] !~ m/^=/ ) {
                    my $line = shift @pod_lines;
                    next unless $line =~ m/\S/;
                    $license_data .= $line;
                }
            }

            if ( $license_data && !$self->BUILD_json->{'license'} ) {
                if ( $license_data =~ m/or\s*the\s*Artistic\s*License/msi ) {
                    $self->BUILD_json->{'license'} = 'perl';
                }
                elsif ( $license_data =~ m/Terms\s*(of|as)\s*Perl\s*itself/msi ) {
                    $self->BUILD_json->{'license'} = 'perl';
                }
                elsif ( $license_data =~ m/under\s*the\s*terms\s*of\s*GNU\s*General\s*Public\s*License\s*\(GPL\)/msi ) {
                    $self->BUILD_json->{'license'} = 'GPL';
                }
                elsif ( $license_data =~ m/under\s*the\s*terms\s*of\s*GNU\s*General\s*Public\s*License\s*3/msi ) {
                    $self->BUILD_json->{'license'} = 'GPLv3+';
                }
                $license_data = '';    # Clear it for the next check.
            }
        }
    }
}

sub parse_code ( $self, $filename ) {

    my $requires_runtime_hash;
    $requires_runtime_hash = $self->requires_build   if $filename =~ m{^t/.+\.(pl|pm|t)$}i;
    $requires_runtime_hash = $self->requires_runtime if $filename =~ m{\.pm$} && $filename !~ m/^(t|xt)/;
    return unless $requires_runtime_hash;    # Doesn't look like perl code we know about.

    print "Reading $filename\n";
    my $doc = $self->get_ppi_doc($filename);

    # find use/require statements and parse them.
    my $use_find = $doc->find('PPI::Statement::Include') || [];
    foreach my $use_node (@$use_find) {
        my $module = get_package_usage($use_node) or next;
        $requires_runtime_hash->{$module} = 0;
    }

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
                else {
                    my $str = $pkg_token->content;
                    my ($version) = $str =~ m/sprintf.+Revision: ([0-9]+\.[0-9]+)/;

                    $version or die $pkg_token->content . "\n" . dump_tree( $pkg_token, "Unexpected content in VERSION statement" );
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

    return $token->content;
}

sub get_package_usage ($element) {
    my $top = $element;

    my $token       = $element->first_token;
    my $require_str = $token->content;
    return if ( $require_str eq 'no' );    # Not an include usually for 'no warnings...'
    $token =~ /^(use|require)$/ or die( dump_tree( $element, "$token not a sub?" ) );
    my $is_use = $1;

    $token = $token->snext_sibling;

    return if ( $token->content =~ m/^5\.\d+/ );                     # skip use 5.xx
    die( $element->content ) if ( $token->content =~ m/^\s*\$/ );    # Dynamic require can't be parsed.

    my $module = $token->content;

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

sub dump_tree ( $element, $die_msg = '' ) {
    my $dump = PPI::Dumper->new($element);
    $dump->print;
    return $die_msg ? $die_msg : ();
}

package PauseTop5;

use strict;
use warnings;

use FindBin;

use Moose;
with 'MooseX::SimpleConfig';
with 'MooseX::Getopt';

use experimental 'signatures';

use CPAN::DistnameInfo  ();
use version             ();
use YAML::Syck          ();
use Config::INI::Reader ();
use File::Slurper       ();

has 'base_dir'   => ( isa => 'Str', is => 'ro', required => 1, documentation => 'REQUIRED - The base directory where our data is stored.' );                                # = /root/projects/pause-monitor
has 'git_binary' => ( isa => 'Str', is => 'ro', lazy     => 1, default       => '/usr/bin/git', documentation => 'The location of the git binary that should be used.' );

has 'repo_user_name' => ( isa => 'Str', is => 'ro', required => 1, documentation => 'The name that will be on commits for this repo.' );
has 'repo_email'     => ( isa => 'Str', is => 'ro', required => 1, documentation => 'The email that will be on commits for this repo.' );

has 'push_to_github' => ( isa => 'Bool', is => 'ro', required => 0, default => sub { 1 }, documentation => 'push changes to github.' );

has 'parsed_pause_archives_file' => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { $_[0]->base_dir . '/parsed_pause_archives.txt' } );
has 'repos_dir'                  => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { my $d = $_[0]->base_dir . '/repos'; -d $d or mkdir $d; return $d } );
has 'repo_list' => ( isa => 'ArrayRef', is => 'rw', lazy => 1, builder => '_build_repo_list' );

has 'pause_branch' => ( isa => 'Str', is => 'ro', default => 'PAUSE' );
has 'p5_branch'    => ( isa => 'Str', is => 'ro', default => 'p5' );

sub _build_repo_list ($self) {
    my $repos_dir = $self->repos_dir;

    opendir( my $dh, $repos_dir ) or die("Cannot read $repos_dir: $!");
    my @repos = readdir $dh;
    close $dh;

    # Strip out directories starting with '.'
    @repos = sort { $a cmp $b } grep { $_ !~ m/^\./ } @repos;

    return \@repos;
}

sub run ($self) {

    my @skip_list = qw{
      AAAA-Crypt-DH
      AAAAAAAAA
    };

    my $repo_list = $self->repo_list;
    foreach my $repo (@$repo_list) {
        if ( grep { $repo eq $_ } @skip_list ) {
            print "Skipping $repo\n";
            next;
        }
        print "Processing repos/$repo\n";

        my $repo_dir = $self->repos_dir . '/' . $repo;
        my $cd       = Cwd::Guard->new($repo_dir);

        my $distro = Perl::Distro->new( distro => $repo, repo_path => $repo_dir, git_binary => $self->git_binary, push_to_github => $self->push_to_github );

        $distro->check_if_dirty_and_die;
        $distro->checkout_p5_branch;
        $distro->fix_special_repos($repo);
        $distro->update_p5_branch_from_PAUSE($repo);
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

exit( $o->run );

