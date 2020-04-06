package Perl::RepoCache;

use strict;
use warnings;

use FindBin;
use Moose;

use experimental 'signatures';

use File::Slurper ();

has 'base_dir' => ( isa => 'Str', is => 'ro', required => 1 );

has 'repos_dir'  => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { $_[0]->base_dir . '/repos' } );
has 'cache_file' => ( isa => 'Str', is => 'ro', lazy => 1, default => sub { shift->base_dir . "/data/repo_cache.txt" } );

has 'cache' => ( isa => 'HashRef', is => 'rw', lazy => 1, builder => '_build_cache' );

sub needs_check ( $self, $repo ) {
    return 1 if $ENV{IGNORE_REPO_CACHE};

    my $cached_mtime = $self->cache->{$repo} || return 1;
    my $repo_dir     = $self->repos_dir . '/' . $repo;

    my $current_mtime = ( stat($repo_dir) )[9];
    $current_mtime or die("$repo_dir is missing");

    return $cached_mtime < $current_mtime;
}

sub update_cache_for_repo ( $self, $repo ) {
    my $repo_dir = $self->repos_dir . '/' . $repo;

    my $current_mtime = ( stat($repo_dir) )[9];
    $current_mtime or die("$repo_dir is missing");

    return $self->cache->{$repo} = $current_mtime;
}

sub _build_cache ($self) {
    my @lines = split( "\n", File::Slurper::read_text( $self->cache_file ) );
    my %cache;
    foreach my $line (@lines) {
        next unless $line =~ m/^\S+\s+[0-9]+/;
        my ( $module, $mtime ) = split( " ", $line, 2 );
        $mtime = $mtime + 0;
        $mtime or die( "Unexpected mtime value for $module in " . $self->cache_file );

        $cache{$module} = $mtime;
    }

    return \%cache;
}

sub DEMOLISH ( $self, $in_global_destruction ) {
    $in_global_destruction and die( "Unexpectedly in global destruction to clean up " . __PACKAGE__ );

    print "Updating repo cache\n";
    open( my $fh, '>', $self->cache_file ) or die("Couldn't open cache file for write: $!");

    my $cache = $self->cache;
    foreach my $module ( sort { $a cmp $b } keys %$cache ) {
        print {$fh} sprintf( "%s %s\n", $module, $cache->{$module} );
    }
    close $fh;

    return;
}

1;
