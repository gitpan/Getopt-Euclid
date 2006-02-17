use Test::More 'no_plan';

if (eval { require Getopt::Euclid and Getopt::Euclid->import(':foo'); 1 }) {
    ok 0 => 'Unexpectedly succeeded';
}
else {
    like $@, qr/Unknown mode \(':foo'\)/ => 'Failed as expected'; 
}

if (eval { require Getopt::Euclid and Getopt::Euclid->import(':minimal_keys'); 1 }) {
    ok 1 => 'Minimal mode accepted';
}
else {
    ok 0 => 'Unexpectedly failed';
}
