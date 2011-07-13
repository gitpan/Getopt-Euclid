use strict;
use warnings;
use Module::Build;

# If you updated this, don't forget to update the Makefile.PL file as well!

my $builder = Module::Build->new(
    module_name         => 'Getopt::Euclid',
    dist_author         => 'Damian Conway <DCONWAY@cpan.org>',
    license             => 'perl',
    dist_version_from   => 'lib/Getopt/Euclid.pm',

    requires => {
        'Test::More'            => 0,
        'version'               => 0,
        'File::Basename'        => 0,
        'File::Spec::Functions' => 0,
        'List::Util'            => 0,
        'Text::Balanced'        => 0,
        'Perl::Tidy'            => 0,
    },
    add_to_cleanup      => [ 'Getopt-Euclid-*' ],
);

$builder->create_build_script();
