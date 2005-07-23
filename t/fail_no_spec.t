BEGIN {
    $INFILE  = $0;
    $OUTFILE = 'nexistpas';
    $LEN     = 42;
    $H       = 2;
    $W       = -10;
    $TIMEOUT = 7;

    @ARGV = (
        '-v',
        "-out=", $OUTFILE,
        "size ${H}x${W}",
        "-i   $INFILE",
        "-lgth $LEN",
        "--timeout $TIMEOUT",
    );
}

use Test::More 'no_plan';

if (eval { require Getopt::Euclid and Getopt::Euclid->import(); 1 }) {
    ok 0 => 'Unexpectedly succeeded';
}
else {
    like $@, qr/Unknown argument/   => 'Failed as expected'; 
}
