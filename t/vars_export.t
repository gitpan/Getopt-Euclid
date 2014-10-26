our ( $INFILE, $OUTFILE, $LEN, $H, $W, $TIMEOUT );
BEGIN {
    $INFILE  = $0;
    $OUTFILE = $0;
    $LEN     = 42;
    $H       = 2;
    $W       = -10;
    $TIMEOUT = 7;

    @ARGV = (
        # doesn't include --missing in order to test that the corresponding variable
        # is still exported even if not present in @ARGV
        "-i   $INFILE",
        "-out=", $OUTFILE,
        "-lgth $LEN",
        "size ${H}x${W}",
        '-v',
        '--skip-some',
        '--also 42',
        '--also 43',
        "--timeout $TIMEOUT",
        '-w', 's p a c e s',
        7,
    );
}

use Getopt::Euclid qw( :vars<opt_> );
use Test::More 'no_plan';

use strict;

sub got_arg {
    my ($key, $val) = @_;
    my $var_name = "opt_$key";
    no strict 'refs';
    is ${$var_name}, $val, "Got expected value for $var_name";
}

sub not_arg {
    my ($key, $val) = @_;
    my $var_name = "opt_$key";
    no strict 'refs';
    is ${$var_name}, undef, "$var_name should be undefined";
}

not_arg 'i'       => $INFILE;
got_arg 'infile'  => $INFILE;

not_arg 'l'       => $LEN;
not_arg 'len'     => $LEN;
got_arg 'length'  => $LEN;
not_arg 'lgth'    => $LEN;

got_arg 'girth'   => 42;

not_arg 'o'       => $OUTFILE;
not_arg 'ofile'   => $OUTFILE;
not_arg 'out'     => $OUTFILE;
got_arg 'outfile' => $OUTFILE;

not_arg 'v'       => 1,
got_arg 'verbose' => 1,

not_arg 'skip_some'      => 1,
got_arg 'skip_something' => 1,

is $opt_missing, undef, 'Got $opt_missing as undef and use strict was happy';

is_deeply \@opt_also, [ 42, 43 ] => 'Got repeated options as array';

is $opt_timeout{min}, $TIMEOUT  => 'Got expected value for timeout <min>';
is $opt_timeout{max}, -1        => 'Got default value for timeout <max>';

is $opt_size{h}, $H           => 'Got expected value for size <h>';
is $opt_size{w}, $W           => 'Got expected value for size <w>';

is_deeply \@opt_w, ['s p a c e s']      => 'Handled spaces correctly';

is $opt_step, 7      => 'Handled step size correctly';

__END__

=head1 NAME

orchestrate - Convert a file to Melkor's .orc format

=head1 VERSION

This documentation refers to orchestrate version 1.9.4

=head1 USAGE

    orchestrate  -in source.txt  --out dest.orc  -verbose  -len=24

=head1 REQUIRED ARGUMENTS

=over

=item  -i[nfile]  [=]<file>    

Specify input file

=for Euclid:
    file.type:    readable
    file.default: '-'

=item  -o[ut][file]= <file>    

Specify output file

=for Euclid:
    file.type:    writable
    file.default: '-'

=back

=head1 OPTIONS

=over

=item  size <h>x<w>

Specify height and width

=item  -l[[en][gth]] <l>

Display length [default: 24 ]

=for Euclid:
    l.type:    int > 0
    l.default: 24

=item  -girth <g>

Display girth [default: 42 ]

=for Euclid:
    g.default: 42

=item -v[erbose]

Print all warnings

=item --skip-some[thing]

Don't do something that would normally be done.

=item --also <also>

Also do these things

=for Euclid:
    repeatable

=item --timeout [<min>] [<max>]

=for Euclid:
    min.type: int
    max.type: int
    max.default: -1

=item -w <space>

Test something spaced

=for Euclid:
    repeatable

=item <step>

Step size

=item --missing

The missing link

=item --version

=item --usage

=item --help

=item --man

Print the usual program information

=back

=begin remainder of documentation here...

=end

=head1 AUTHOR

Damian Conway (damian@conway.org)

=head1 BUGS

There are undoubtedly serious bugs lurking somewhere in this code.
Bug reports and other feedback are most welcome.

=head1 COPYRIGHT

Copyright (c) 2002, Damian Conway. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the terms of the Perl Artistic License
  (see http://www.perl.com/perl/misc/Artistic.html)

