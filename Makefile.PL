use strict;
use warnings;
use ExtUtils::MakeMaker;

# If you updated this, don't forget to update the Build.PL file as well!

WriteMakefile(
    NAME                => 'Getopt::Euclid',
    AUTHOR              => 'Damian Conway <DCONWAY@cpan.org>',
    LICENSE             => 'perl',
    VERSION_FROM        => 'lib/Getopt/Euclid.pm',
    ABSTRACT_FROM       => 'lib/Getopt/Euclid.pm',
    PREREQ_PM => {
        'Test::More'            => 0,
        'version'               => 0,
        'File::Basename'        => 0,
        'File::Spec::Functions' => 0,
        'List::Util'            => 0,
        'Text::Balanced'        => 0,
        'Perl::Tidy'            => 0,
    },
    PL_FILES            => {},
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Getopt-Euclid-*' },
);
