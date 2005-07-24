package Getopt::Euclid;

use version; $VERSION = qv('0.0.3');

use warnings;
use strict;
use Carp;
use File::Spec::Functions qw(splitpath);
use List::Util qw( first );

# Utility sub to factor out hash key aliasing...
sub _make_equivalent {
    my ($hash_ref, %pairs) = @_;

    for my $name (keys %pairs) {
        for my $alias ( @{$pairs{$name}} ) {
            $hash_ref->{$alias} = $hash_ref->{$name};
        }
    }

    return;
}

# Report problems in specification...

sub _fail {
    my (@msg) = @_;
    die "Getopt::Euclid: @msg\n";
}

my $has_run;
my @std_POD;

END { $has_run = 1 }

sub Getopt::Euclid::Importer::DESTROY {
    return if $has_run;
    croak '.pm file cannot define an explicit import() when using Getopt::Euclid';
}

sub import {
    if ($has_run) {
        carp "Getopt::Euclid loaded a second time";
        warn "Second attempt to parse command-line was ignored\n";
        return;
    }
    
    # Handle calls from .pm files...
    my @caller = caller;
    if ($caller[1] =~ m/[.]pm \z/xms) {
        # Save module's POD...
        open my $fh, '<', $caller[1]
            or croak "Getopt::Euclid was unable to access POD\n($!)\nProblem was";
        push @std_POD, do{ local $/; <$fh>};

        # Install this import() sub as module's import sub...
        no strict 'refs';
        croak '.pm file cannot define an explicit import() when using Getopt::Euclid'
            if *{"$caller[0]::import"}{CODE};

        my $lambda;  # Needed so the anon sub is generated at run-time
        *{"$caller[0]::import"}
            = bless sub { $lambda = 1; goto &Getopt::Euclid::import },
                    'Getopt::Euclid::Importer';

        return;
    }

    $has_run = 1;

    # Acquire POD source...
    open my $fh, '<', $0
        or croak "Getopt::Euclid was unable to access POD\n($!)\nProblem was";
    my $source = do{ local $/; <$fh>};

    # Set up parsing rules...
    my $HWS      = qr{ [^\S\n]*      }xms;
    my $EOHEAD   = qr{ (?= ^=head1 | \z)  }xms;
    my $POD_CMD  = qr{ \n\n = [^\W\d]\w+ [^\n]* \n\n}xms;
    my $POD_CUT  = qr{ \n\n = cut $HWS \n\n}xms;

    my $NAME     = qr{ $HWS NAME    $HWS \n }xms;
    my $VERS     = qr{ $HWS VERSION $HWS \n }xms;
    my $USAGE    = qr{ $HWS USAGE   $HWS \n }xms;

    my $STD      = qr{ STANDARD | STD  }xms;
    my $ARG      = qr{ $HWS ARG(?:UMENT)?S? }xms;

    my $OPTIONS  = qr{ $HWS $STD? $HWS OPTION(?:AL|S)? $ARG? $HWS \n }xms;
    my $REQUIRED = qr{ $HWS $STD? $HWS REQUIRED        $ARG? $HWS \n }xms;

    my $EUCLID_ARG = qr{ ^=item \s* ([^\n]*?) \s* \n\s*\n
                        (
                        .*?
                        (?:
                            ^=for \s* (?i: Euclid) .*? \n\s*\n
                            | (?= ^=[^\W\d]\w* | \z)
                        )
                        )
                    }xms;


    # Extract POD alone...
    my @chunks = $source =~ m{ $POD_CMD .*? (?: $POD_CUT | \z ) }gxms;
    my $pod = join q{}, @chunks;

    # Extract essential interface components...
    my ($prog_name)     = (splitpath($0))[-1];

    my ($version)
        = $pod =~ m/^=head1 $VERS     .*? (\d+(?:[._]\d+)+) .*? $EOHEAD /xms;

    my ($opt_name, $options)
        = $pod =~ m/^=head1 ($OPTIONS)  (.*?) $EOHEAD /xms;

    my ($req_name, $required)
        = $pod =~ m/^=head1 ($REQUIRED) (.*?) $EOHEAD /xms;

    my ($licence)
        = $pod =~ m/^=head1 [^\n]+ (?i: licen[sc]e | copyright ) .*? \n \s* (.*?) \s* $EOHEAD /xms;

    # Extra info from higher-level pod...
    for my $std_POD (reverse @std_POD) {
        my (undef, $more_options)
            = $std_POD =~ m/^=head1 ($OPTIONS)  (.*?) $EOHEAD /xms;
        $options = ($more_options||q{}) . ($options||q{});

        my (undef, $more_required)
            = $std_POD =~ m/^=head1 ($REQUIRED) (.*?) $EOHEAD /xms;
        $required = ($more_required||q{}) . ($required||q{});

        my ($more_licence)
            = $std_POD =~ m/^=head1 [^\n]+ (?i: licen[sc]e | copyright ) .*? \n \s* (.*?) \s* $EOHEAD /xms;
        $licence = ($more_licence||q{}) . ($licence|q{});
    }

    # Clean up interface titles...
    NAME:
    for my $name ($opt_name, $req_name) {
        next NAME if ! defined $name;
        $name =~ s{\A \s+ | \s+ \z}{}gxms;
    }

    # Extract the actual interface...
    my @requireds = ($required||q{}) =~ m{ $EUCLID_ARG }gxms;
    my @options   = ($options||q{})  =~ m{ $EUCLID_ARG }gxms;

    # Convert each arg entry to a hash...
    my (%requireds, %options);
    my $seq_num = 0;
    while (@requireds) {
        my ($name, $spec) = splice @requireds, 0, 2;
        $requireds{$name} = { seq => $seq_num++, src => $spec, name => $name };
    }
    while (@options) {
        my ($name, $spec) = splice @options, 0, 2;
        $options{$name} = { seq => $seq_num++, src => $spec, name => $name };
    }

    my %STD_CONSTRAINT_FOR;
    BEGIN {
        %STD_CONSTRAINT_FOR = (
            'string'    => sub { 1 }, # Always okay (matcher ensures this)
            'integer'   => sub { 1 }, # Always okay (matcher ensures this)
            '+integer'  => sub { $_[0] > 0 },
            '0+integer' => sub { $_[0] >= 0 },
              'number'  => sub { 1 }, # Always okay (matcher ensures this)
             '+number'  => sub { $_[0] > 0 },
            '0+number'  => sub { $_[0] >= 0 },
              'input'   => sub { -r $_[0] },
              'output'  => sub { my (undef, $dir) = splitpath($_[0]);
                               $dir ||= '.';
                               -e $_[0] ? -w $_[0] : -w $dir
                           },
        );

        _make_equivalent(\%STD_CONSTRAINT_FOR, 
              'integer' => [qw(   int   i )],
             '+integer' => [qw(  +int  +i )],
            '0+integer' => [qw( 0+int 0+i )],
               'number' => [qw(   num   n )],
              '+number' => [qw(  +num  +n )],
             '0+number' => [qw( 0+num 0+n )],
               'string' => [qw( str s )],
                'input' => [qw( in readable )],
               'output' => [qw( out writable writeable )],
        );
    }

    # Extract Euclid information...
    ARG:
    for my $arg (values(%requireds), values(%options) ) {
        $arg->{src} =~ s{^ =for \s+ Euclid\b [^\n]* \s* (.*) \z}{}ixms
            or next ARG;
        my $info = $1;
        while ($info =~ m{\G \s* (([^.]+)\.([^.\s]+) \s*[:=]\s* ([^\n]*)) }gcxms) {
            my ($spec, $var, $field, $val) = ($1, $2, $3, $4);
            if ($field eq 'type') {
                my ($matchtype, $comma, $constraint)
                    = $val =~ m/([^,\s]+)\s*(?:(,))?\s*(.*)/xms;
                $arg->{var}{$var}{type} = $matchtype;

                if ($comma && length $constraint) {
                    ($arg->{var}{$var}{constraint_desc} = $constraint)
                        =~ s/\s*\b\Q$var\E\b\s*//g;
                    $constraint =~ s/\b\Q$var\E\b/\$_[0]/g;
                    $arg->{var}{$var}{constraint} = eval "sub{ $constraint }"
                        or _fail("Invalid .type constraint: $spec\n($@)");
                }
                elsif (length $constraint) {
                    $arg->{var}{$var}{constraint_desc} = $constraint;
                    $arg->{var}{$var}{constraint} = eval "sub{ \$_[0] $constraint }"
                        or _fail("Invalid .type constraint: $spec\n($@)");
                }
                else {
                    $arg->{var}{$var}{constraint_desc} = $matchtype;
                    $arg->{var}{$var}{constraint} = $STD_CONSTRAINT_FOR{$matchtype}
                        or _fail("Unknown .type constraint: $spec");
                }
            }
            elsif ($field eq 'default') {
                eval "\$val = $val; 1"
                    or _fail("Invalid .default value: $spec\n($@)");
                $arg->{var}{$var}{default} = $val;
                $arg->{has_defaults} = 1;
            }
            else {
                _fail("Unknown specification: $spec");
            }
        }
        if ($info =~ m{\G \s* ([^\s\0\1] [^\n]*) }gcxms) {
                _fail("Unknown specification: $1");
        }
    }

    # Build one-line representation of interface...
    my $arg_summary = join ' ',
                        sort { $requireds{$a}{seq} <=> $requireds{$b}{seq} }
                             keys %requireds;
    1 while $arg_summary =~ s/\[ [^]]* \]//gxms;
    $arg_summary .= lc " [$opt_name]" if $opt_name;
    $arg_summary =~ s/\s+/ /gxms;

    $pod =~ s{ ^(=head1 $NAME \s*) .*? (- .*) $EOHEAD }
            {$1 $prog_name $2}xms;

    $pod =~ s{ ^(=head1 $USAGE \s*) .*? (\s*) $EOHEAD }
            {$1 $prog_name $arg_summary $2}xms;

    $pod =~ s{ ^(=head1 $VERS    \s*) .*? (\s*) $EOHEAD }
            {$1 This document refers to $prog_name version $version $2}xms;


    # Handle standard args...
    if (grep { / --man /xms } @ARGV) {
        _print_and_exit($pod, 'paged');
    }
    elsif (first { $_ eq '--usage' } @ARGV) {
        print "Usage: $prog_name $arg_summary\n",
            "       $prog_name --help\n";
        exit;
    }
    elsif (first { $_ eq '--help' } @ARGV) {
        my $pod = "=head1 Usage:\n\n$prog_name $arg_summary\n\n";
        $pod .= "=head1 \L\u$req_name:\E\E\n\n$required\n\n"
            if ($required||q{}) =~ /\S/;
        $pod .= "=head1 \L\u$opt_name:\E\E\n\n$options\n\n"
            if ($options||q{}) =~ /\S/;
        _print_and_exit($pod);
    }
    elsif (first { $_ eq '--version' } @ARGV) {
        print "This is $prog_name version $version\n";
        if ($licence) {
            print "\n$licence\n";
        }
        exit;
    }

    # Convert arg specifications to regexes...

    _convert_to_regex(\%requireds);
    _convert_to_regex(\%options);

    # Build matcher...

    my @arg_list = (values(%requireds), values(%options));
    my $matcher = join '|', map { $_->{matcher} }
                    sort({$b->{name} cmp $a->{name}} grep {$_->{name} =~ /^[^<]/} @arg_list),
                    sort({$a->{seq} <=> $b->{seq}} grep {$_->{name} =~ /^[<]/} @arg_list);

    $matcher .= '|(?> (.+)) (?{ push @errors, $^N }) (?!)';

    $matcher = '(?:'.$matcher.')';

    # Report problems in parsing...

    *_bad_arglist = sub {
        my (@msg) = @_;
        my $msg = join q{}, @msg;
        $msg =~ tr/\0\1/ \t/;
        $msg =~ s/\n?\z/\n/xms;
        die "$msg(Try: $prog_name --help)\n\n";
    };

    # Run matcher...
    my $all_args_ref = { %options, %requireds };

    my $argv = join(q{ }, map {my $arg=$_; $arg=~tr/ \t/\0\2/; $arg} @ARGV);
    if (my $error = _doesnt_match($matcher, $argv, $all_args_ref)) {
        _bad_arglist($error);
    }


    # Check all requireds have been found...

    my @missing;
    for my $req ( keys %requireds ) {
        push @missing, "\t$req\n"
            if ! exists $ARGV{$req};
    }
    _bad_arglist('Missing required argument', (@missing==1?q{}:q{s}),
                 ":\n", @missing)
        if @missing;

    # Back-translate \0-quoted spaces and \1-quoted tabs...

    _rectify_args();

    # Check constraints and fill in defaults...

    _verify_args($all_args_ref);


    # Clean up %ARGV...

    for my $arg_name (keys %ARGV) {
        my $val = delete $ARGV{$arg_name};
        my $var_count = keys %{$val};
        $val = $var_count == 0 ? 1                    # Boolean -> true
             : $var_count == 1 ? (values %{$val})[0]  # Single var -> var's val
             :                   $val                 # Otherwise keep hash 
             ;
        for my $arg_flag ( _get_variants($arg_name) ) {
            $ARGV{$arg_flag} = $val;
        }
    }
}

# ###### Utility subs #############


# Do match, recursively trying to expand cuddles...

sub _doesnt_match {
    use re 'eval';
    my ($matcher, $argv, $arg_specs_ref) = @_;

    our @errors;
    local @errors = ();
    %ARGV = ();

    $argv =~ m{\A (?: \s* $matcher )* \s* \z}xms;

    for my $error (@errors) {
        if ($error =~ m/\A ((\W) (\w) (\w+))/xms) {
            my ($bundle, $marker, $firstchar, $chars) = ($1, $2, $3, $4);
            $argv =~ s{\Q$bundle\E}{$marker$firstchar $marker$chars}xms;
            return if ! _doesnt_match($matcher, $argv, $arg_specs_ref);
        }
        ARG:
        for my $arg_spec_ref (values %{$arg_specs_ref}) {
            our $bad_type;
            local $bad_type;
            next ARG
                if $error !~ m/\A [\s\0\1]* ($arg_spec_ref->{generic_matcher})/xms
                || !$bad_type;
            
            return qq{Invalid "$bad_type->{arg}" argument\n}
                 . qq{$bad_type->{var} must be $bad_type->{type}}
                 . qq{ but the supplied value ("$bad_type->{val}") isn't.}
        }
        return "Unknown argument: $error";
    }

    return;  # No error
}
# Assign default values to missing components in %ARGV...

sub _rectify_args {
    for my $arg (values %ARGV) {
        if (ref $arg eq 'HASH') {
            for my $var (values %{$arg}) {
                if (ref $var eq 'ARRAY') { 
                    tr/\0\1/ \t/ for @{$var};
                }
                else {
                    tr/\0\1/ \t/ for $var;
                }
            }
        }
        else {
            if (ref $arg eq 'ARRAY') { 
                tr/\0\1/ \t/ for @{$arg};
            }
            else {
                tr/\0\1/ \t/ for $arg;
            }
        }
    }
}

sub _verify_args {
    my ($arg_specs_ref) = @_;

    ARG:
    for my $arg_name (keys %{$arg_specs_ref}) {
        # Skip non-existent/non-defaulting arguments
        next ARG if !exists $ARGV{$arg_name}
                 && !$arg_specs_ref->{$arg_name}{has_defaults};

        # Ensure all vars exist within arg...
        my @vars = @{$arg_specs_ref->{$arg_name}{placeholders}||[]};
        @{$ARGV{$arg_name}}{@vars} = @{$ARGV{$arg_name}}{@vars};

        # Get arg specs...
        my $arg_vars = $arg_specs_ref->{$arg_name}{var};

        VAR:
        for my $var (@vars) {

            # Check constraints on vars...
            if (exists $ARGV{$arg_name}) {

                # Named vars...
                if (ref $ARGV{$arg_name} eq 'HASH' && defined $ARGV{$arg_name}{$var}) {
                    for my $val (ref $ARGV{$arg_name}{$var} eq 'ARRAY'
                                    ? @{$ARGV{$arg_name}{$var}}
                                    :   $ARGV{$arg_name}{$var}
                                ) {
                        _bad_arglist( qq{Invalid "$arg_name" argument.\n},
                                    qq{<$var> must be },
                                    $arg_vars->{$var}{constraint_desc},
                                    qq{ but the supplied value ("$val") isn't.}
                                    )
                            if $arg_vars->{$var}{constraint}
                            && ! $arg_vars->{$var}{constraint}->($val);
                    }
                    next VAR;
                }
                # Unnamed vars...
                elsif (ref $ARGV{$arg_name} ne 'HASH' && defined $ARGV{$arg_name}) {
                    for my $val (ref $ARGV{$arg_name} eq 'ARRAY'
                                    ? @{$ARGV{$arg_name}}
                                    :   $ARGV{$arg_name}
                                ) {
                        _bad_arglist( qq{Invalid "$arg_name" argument.\n},
                                    qq{<$var> must be },
                                    $arg_vars->{$var}{constraint_desc},
                                    qq{ but the supplied value ("$val") isn't.}
                                    )
                            if $arg_vars->{$var}{constraint}
                            && ! $arg_vars->{$var}{constraint}->($val);
                    }
                    next VAR;
                }
            }

            # Assign defaults (if necessary)...
            next ARG if !exists $arg_specs_ref->{$arg_name}{var}{$var}{default};

            $ARGV{$arg_name}{$var}
                = $arg_specs_ref->{$arg_name}{var}{$var}{default};
        }
    }
}

# Convert arg specification syntax to Perl regex syntax

my %STD_MATCHER_FOR;
BEGIN {
    %STD_MATCHER_FOR = (
        integer => '[+-]?\\d+',
        number  => '[+-]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][+-]?\d+)?',
        input   => '\S+',
        output  => '\S+',
        file    => '\S+',
        string  => '\S+',
        q{}     => '\S+',
    );

    _make_equivalent(\%STD_MATCHER_FOR,
        integer => [qw( int i +int +i 0+int 0+i )],
        number  => [qw( num n +num +n 0+num 0+n )],
        input   => [qw( readable in )],
        output  => [qw( writable writeable out )],
        string  => [qw( str s )],
    );
}

sub _convert_to_regex {
    my ($args_ref) = @_;

    for my $arg_name ( keys %{$args_ref} ) {
        my $arg = $args_ref->{$arg_name};
        my $regex = $arg_name;
        1 while $regex =~ s/ \[ ([^]]*) \] /(?:$1)?/gxms;
        $regex =~ s/ (\s+) /$1.'[\\s\\0\\1]*'/egxms;
        my $generic = $regex;
        $regex =~ s{ < (.*?) >(\.\.\.|) }
                   { my ($var_name, $var_rep) = ($1, $2);
                     $var_name =~ s/(\s+)\[\\s\\0\\1]\*/$1/gxms;
                     my $type = $arg->{var}{$var_name}{type} || q{};
                     push @{$arg->{placeholders}}, $var_name;
                     my $matcher = $STD_MATCHER_FOR{ $type }
                        or _fail("Unknown type ($type) in specification: $arg_name");
                    $var_rep ? "(?:[\\s\\0\\1]*($matcher)(?{push \@{\$ARGV{q{$arg_name}}{q{$var_name}}}, \$^N}))+"
                             : "(?:($matcher)(?{\$ARGV{q{$arg_name}}{q{$var_name}} = \$^N}))"

                   }gexms;
        $arg->{matcher} = "(??{exists\$ARGV{q{$arg_name}}?'(?!)':''}) $regex (?:(?<!\\w)|(?!\\w)) (?{\$ARGV{q{$arg_name}} ||= {}})";

        $generic =~ s{ < (.*?) > }
                     { my $var_name = $1;
                       $var_name =~ s/(\s+)\[\\s\\0\\1]\*/$1/gxms;
                       my $type = $arg->{var}{$var_name}{type} || q{};
                       my $matcher = $STD_MATCHER_FOR{ $type };
                       "(?:($matcher|([^\\s\\0\\1]+)"
                       . "(?{\$bad_type ||= "
                       .  "{arg=>q{$arg_name},type=>q{$type},var=>q{<$var_name>},val=>\$^N};})))"
                     }gexms;
        $arg->{generic_matcher} = $generic;
    }
    return;
}

sub _print_and_exit {
    my ($pod, $paged) = @_;

    if (-t *STDOUT and eval { require POD::Text }) {
        if ($paged) {
            eval { require IO::Page } or eval { require IO::Pager::Page };
        }
        open my $pod_handle, '<', \$pod;
        my $parser = Pod::Text->new (sentence => 0, width => 78);
        $parser->parse_from_filehandle($pod_handle);
    }
    else {
        print $pod;
    }

    exit;
}

sub _get_variants {
    my @arg_desc = @_;

    # Only consider first "word"...
    return $1 if $arg_desc[0] =~ m/\A (< [^>]+ >)/xms;
   
    $arg_desc[0] =~ s/\A ([^\s<]+) \s* (?: < .*)? \z/$1/xms;

    # Variants are all those with and without each optional component...
    my %variants;
    while (@arg_desc) {
        my $arg_desc_with    = shift @arg_desc;

        my $arg_desc_without = $arg_desc_with;
        if ($arg_desc_without =~ s/ \[ [^][]* \] //xms) {
            push @arg_desc, $arg_desc_without;
        }
        if ($arg_desc_with =~ s/ \[ ([^][]*) \] /$1/xms) {
            push @arg_desc, $arg_desc_with;
        }

        $arg_desc_with =~ s/[][]//gxms;
        $arg_desc_with =~ s/\b\W .* \z//xms;
        $variants{$arg_desc_with} = 1;
    }

    return keys %variants;
}



1; # Magic true value required at end of module
__END__

=head1 NAME

Getopt::Euclid - Executable Uniform Command-Line Interface Descriptions


=head1 VERSION

This document describes Getopt::Euclid version 0.0.3


=head1 SYNOPSIS

    use Getopt::Euclid;

    if ($ARGV{-i}) {
        print "Interactive mode...\n";
    }

    for my $x (0..$ARGV{-size}{h}-1) {
        for my $y (0..$ARGV{-size}{w}-1) {
            do_something_with($x, $y);
        }
    }

    __END__

    =head1 NAME

    yourprog - Your program here

    =head1 VERSION

    This documentation refers to yourprog version 1.9.4

    =head1 USAGE

        yourprog [options]  -s[ize]=<h>x<w>  -o[ut][file] <file>

    =head1 REQUIRED ARGUMENTS

    =over

    =item  -s[ize]=<h>x<w>    

    Specify size of simulation

    =for Euclid:
        h.type:    int > 0
        h.default: 24
        w.type:    int >= 10
        w.default: 80

    =item  -o[ut][file] <file>    

    Specify output file

    =for Euclid:
        file.type:    writable
        file.default: '-'

    =back

    =head1 OPTIONS

    =over

    =item  -i

    Specify interactive simulation

    =item  -l[[en][gth]] <l>

    Length of simulation [default: 99]

    =for Euclid:
        l.type:    int > 0
        l.default: 99

    =item --version

    =item --usage

    =item --help

    =item --man

    Print the usual program information

    =back

    Remainder of documentation starts here...

    =head1 AUTHOR

    Damian Conway (DCONWAY@CPAN.org)

    =head1 BUGS

    There are undoubtedly serious bugs lurking somewhere in this code.
    Bug reports and other feedback are most welcome.

    =head1 COPYRIGHT

    Copyright (c) 2005, Damian Conway. All Rights Reserved.
    This module is free software. It may be used, redistributed
    and/or modified under the terms of the Perl Artistic License
    (see http://www.perl.com/perl/misc/Artistic.html)
  
  
=head1 DESCRIPTION

Getopt::Euclid uses your program's own documentation to create a command-line
argument parser. This ensures that your program's documented interface and
its actual interface always agree.

To use the module, you simply write:

    use Getopt::Euclid;

at the top of your program.

When the module is loaded within a regular Perl program, it will:

=over

=item 1.

locate any POD in the same file,

=item 2.

extract information from that POD, most especially from 
the C<=head1 REQUIRED ARGUMENTS> and C<=head1 OPTIONS> sections,

=item 3.

build a parser that parses the arguments and options the POD specifies,

=item 4.

parse the contents of C<@ARGV> using that parser, and

=item 5.

put the results in the global C<%ARGV> variable.

=back

As a special case, if the module is loaded within some other module
(i.e. from within a C<.pm> file), it still locates and extracts POD
information, but instead of parsing C<@ARGV> immediately, it caches that
information and installs an C<import()> subroutine in the caller module.
That new C<import()> acts just like Getopt::Euclid's own import, except that
it adds the POD from the caller module to the POD of the callee. See
L<Module Interface> for more details.

=head1 INTERFACE 

=head2 Program Interface

You write:

    use Getopt::Euclid;

and your command-line is parsed automagically.

There are no options to pass. Getopt::Euclid doesn't export anything. It just
works.

=head2 Module Interface

You write:

    use Getopt::Euclid;

and your module will then become just like Getopt::Euclid, except that your
module's POD will be prepended to the POD of any module that loads yours.

There are no options to pass.

Getopt::Euclid installs an C<import()> subroutine in your module. If
your module already has an C<import()> subroutine defined, terrible
things happen. So don't do that.

=head2 POD Interface

This is where all the action is.

When Getopt::Euclid is loaded in a non-C<.pm> file, it searches that file for
the following POD documentation:

=over

=item =head1 NAME

Getopt::Euclid ignores the name specified here. In fact, if you use the
standard C<--help>, C<--usage>, C<--man>, or C<--version> arguments (see
L<Standard arguments>), the module replaces the name specified in this
POD section with the actual name by which the program was invoked (i.e.
with C<$0>).

=item =head1 USAGE

Getopt::Euclid ignores the usage line specified here. If you use the
standard C<--help>, C<--usage>, or C<--man> arguments, the module
replaces the usage line specified in this POD section with a usage line
that reflects the actual interface that the module has constructed.

=item =head1 VERSION

Getopt::Euclid extracts the current version number from this POD section.
To do that it simply takes the first substring that matches
I<< <digit> >>.I<< <digit> >> or I<< <digit> >>_I<< <digit> >>. It also
accepts one or more additional trailing .I<< <digit> >> or _I<< <digit> >>,
allowing for multi-level and "alpha" version numbers such as:

    =head1 VERSION
    
    This is version 1.2.3
    
or:

    =head1 VERSION
    
    This is alpha release 1.2_34
    

=item =head1 REQUIRED ARGUMENTS

Getopt::Euclid uses the specifications in this POD section to build a
parser for command-line arguments. That parser requires that every one
of the specified arguments is present in any command-line invocation.
See L<Specifying arguments> for details of the specification syntax.

The actual headings that Getopt::Euclid can recognize here are:

    =head1 [STD|STANDARD] REQUIRED [ARG|ARGUMENT][S]


=item =head1 OPTIONS

Getopt::Euclid uses the specifications in this POD section to build a
parser for command-line arguments. That parser does not require that any
of the specified arguments is actually present in a command-line invocation.
Again, see L<Specifying arguments> for details of the specification syntax.

Typically a program will specify both C<REQUIRED ARGUMENTS> and C<OPTIONS>,
but there is no requirement that it supply both, or either.

The actual headings that Getopt::Euclid recognizes here are:

    =head1 [STD|STANDARD] OPTION[AL|S] [ARG|ARGUMENT][S]

=item =head1 COPYRIGHT

Getopt::Euclid prints this section whenever the standard C<--version> option
is specified on the command-line.

The actual heading that Getopt::Euclid recognizes here is any heading
containing any of the words "COPYRIGHT", "LICENCE", or "LICENSE".

=back

=head2 Specifying arguments

Each required or optional argument is specified in the POD in the following
format:

    =item ARGUMENT_STRUCTURE

    ARGUMENT_DESCRIPTION

    =for Euclid:
        PLACEHOLDER_CONSTRAINTS

=head3 Argument structure

=over 

=item *

Each argument is specified as an C<=item>.

=item *

Any part(s) of the
specification that appear in square brackets are treated as optional.

=item *

Any parts that appear in angle brackets are placeholders for actual
values that must be specified on the command-line.

=item *

Any placeholder that is immediately followed by C<...> may be repeated as many
times as desired.

=item *

Any whitespace in the structure specifies that any amount of whitespace
(including none) is allowed at the same position on the command-line.

=item *

A vertical bar within an optional component indicates an alternative.

=back

For example, the argument specification:

    =item -i[n] [=] <file>

indicates that any of the following may appear on the command-line:

    -idata.txt    -i data.txt    -i=data.txt    -i = data.txt
                                     
    -indata.txt   -in data.txt   -in=data.txt   -in = data.txt

as well as any other combination of whitespacing.

Any of the above variations would cause both C<$ARGV{'-i'}> and C<$ARGV{'-
in'}> to be set to the string C<'data.txt'>.

You could allow the optional C<=> to also be an optional colon by specifying:

    =item -i[n] [=|:] <file>

Optional components may also be nested, so you could write:

    =item -i[n[put]] [=] <file>

which would allow C<-i>, C<-in>, and C<-input> as synonyms for this
argument and would set all three of C<$ARGV{'-i'}>, C<$ARGV{'-in'}>, and
C<$ARGV{'-input'}> to the supplied file name.

The point of setting every possible variant within C<%ARGV> is that this
allows you to use a single key (say C<$ARGV{'-input'}>, regardless of
how the argument is actually specified on the command-line.

=head2 Multiple placeholders

An argument can have two or more placeholders:

    =item -size <h> <w>

The corresponding command line argument would then have to provide two values:

    -size 24 80

Multiple placeholders can optionally be separated by literal characters
(which must then appear on the command-line). For example:

    =item -size <h>x<w>

would then require a command-line of the form:

    -size 24x80

If an argument has two or more placeholders, the corresponding entry in
C<%ARGV> becomes a hash reference, with each of the placeholder names as one
key. That is, the above command-line would set both C<$ARGV{'-size'}{'h'}> and
C<$ARGV{'-size'}{'w'}>.

=head2 Optional placeholders

Placeholders can be specified as optional as well:

    =item -size <h> [<w>]

This specification then allows either:

    -size 24

or:

    -size 24 80

on the command-line. If the second placeholder value is not provided, the
corresponding C<$ARGV{'-size'}{'w'}> entry is set to C<undef>. See also
L<Placeholder defaults>.

=head2 Unflagged placeholders

If an argument consists of a single placeholder with no "flag" marking it:

    =item <filename>

then the corresponding entry in C<%ARG> will have a key the same as the
placeholder (including the surrounding angle brackets):

    if ($ARGV{'<filename>'} eq '-') {
        $fh = \*STDIN;
    }

The same is true for any more-complicated arguments that begin with a
placeholder:

    =item <h> [x <w>]

The only difference in the more-complex cases is that, if the argument
has any additional placeholders, the entire entry in C<%ARGV> becomes a hash:

    my $total_size
        = $ARGV{'<h>'}{'h'} * $ARGV{'<h>'}{'w'}

Note that, as in earlier multi-placeholder examples, the individual second-
level placeholder keys I<don't> retain their angle-brackets.

=head2 Repeated placeholders

Any placeholder that is immediately followed by C<...>, like so:

    =item -lib <files>...

    =item <offsets>...

    =for Euclid:
        offsets.type: integer > 0

will match as many times as possible, but at least once. Note that
this implies that an unconstrained repeated unflagged placeholder
(see L<Placeholder constraints> and L<Unflagged placeholders>) will
consume the rest of the command-line, and so should be specified last
in the POD.

If a placeholder is repeated, the corresponding entry in C<%ARGV>
will then be an array reference, with each individual placeholder match
in a separate element. For example:

    for my $lib (@{ $ARGV{'-lib'} }) {
        add_lib($lib);
    }

    warn "First offset is: $ARGV{'<offsets>'}[0]";
    my $first_offset = shift @{ $ARGV{'<offsets>'} };


=head2 Placeholder constraints

You can specify that the value provided for a particular placeholder
must satisfy a particular set of restrictions by using a C<=for Euclid>
block. For example:

    =item -size <h>x<w>

    =for Euclid:
        h.type: integer
        w.type: integer

specifies that both the C<< <h> >> and C<< <w> >> must be given integers.
You can also specify an operator expression after the type name:

    =for Euclid:
        h.type: integer > 0
        w.type: number <= 100

specifies that C<< <h> >> has to be given an integer that's greater than zero,
and that C<< <w> >> has to be given a number (not necessarily an integer)
that's no more than zero.

These type constraints have two alternative syntaxes:

    PLACEHOLDER.type: TYPE BINARY_OPERATOR EXPRESSION

as shown above, and the more general:

    PLACEHOLDER.type: TYPE [, EXPRESSION_INVOLVING(PLACEHOLDER)]

Using the second syntax, you could write the previous constraints as:

    =for Euclid:
        h.type: integer, h > 0
        w.type: number,  w <= 100

In other words, the first syntax is just sugar for the most common case of the
second syntax. The expression can be as complex as you wish and can refer to
the placeholder as many times as necessary:

    =for Euclid:
        h.type: integer, h > 0 && h < 100
        w.type: number,  Math::is_prime(w) || w % 2 == 0

=head2 Standard placeholder types

Getopt::Euclid recognizes the following standard placeholder types:

    Name            Placeholder value...        Synonyms
    ============    ====================        ================

    integer         ...must be an integer       int    i

    +integer        ...must be a positive       +int   +i
                    integer
                    (same as: integer > 0)

    0+integer       ...must be a positive       0+int  0+i
                    integer or zero
                    (same as: integer >= 0)

    number          ...must be an number        num    n
                    (same as: number > 0)

    +number         ...must be a positive       +num   +n
                    number
                    (same as: number > 0)

    0+number        ...must be a positive       0+num  0+n
                    number or zero
                    (same as: number >= 0)

    string          ...may be any string        str    s
                    (default type)

    readable        ...must be the name         input  in
                    of a readable file

    writeable       ...must be the name         writable output out
                    of a writeable file
                    (or of a non-existent
                    file in a writeable
                    directory)
                    

=head2 Placeholder defaults

You can also specify a default value for any placeholders that aren't
given values on the command-line (either because their argument isn't
provided at all, or because the placeholder is optional within the argument).

For example:

    =item -size <h>[x<w>]

    Set the size of the simulation

    =for Euclid:
        h.default: 24
        w.default: 80

This ensures that if no C<< <w> >> value is supplied:

    -size 20

then C<$ARGV{'-size'}{'w'}> is set to 80.

Likewise, of the C<-size> argument is omitted entirely, both
C<$ARGV{'-size'}{'h'}> and C<$ARGV{'-size'}{'w'}> are set to their
respective default values.

The default value can be any valid Perl compile-time expression:

    =item -pi=<pi value>

    =for Euclid:
        pi value.default: atan2(0,-1)

=head2 Argument cuddling

Getopt::Euclid allows any "flag" argument to be "cuddled". A flag
argument consists of a single non- alphanumeric character, followed by a
single alpha-numeric character:

    =item -v

    =item -x

    =item +1

    =item =z

Cuddling means that two or more such arguments can be concatenated after a
single common non-alphanumeric. For example:

    -vx

Note, however, that only flags with the same leading non-alphanumeric can be
cuddled together. Getopt::Euclid would not allow:

    -vxz

That's because cuddling is recognized by progressively removing the second
character of the cuddle. In other words:

    -vxz

becomes:

    -v -xz

which becomes:

    -v -x z

which will fail, unless a C<z> argument has also been specified.

On the other hand, if the argument:

    =item -e <cmd>

had been specified, the module I<would> accept:

    -vxe'print time'

as a cuddled version of:

    -v -x -e'print time'


=head2 Standard arguments

Getopt::Euclid automatically provides four standard arguments to any
program that uses the module. The behaviours of these arguments are "hard-
wired" and cannot be changed, not even by defining your own arguments of
the same name.

The standard arguments are:

=over

=item --usage

This argument cause the program to print a short usage summary and exit.

=item --help

This argument cause the program to print a longer usage summary (including a
full list of required and optional arguments) and exit.

=item --man

This argument cause the program to print the complete POD documentation
for the program and exit. If the standard output stream is connected to
a terminal and the POD::Text module is available, the POD is formatted
before printing. If the IO::Page or IO::Pager::Page module is available,
the formatted documentation is then paged.

If standard output is not connected to a terminal or POD::Text is not
available, the POD is not formatted.

=item --version

This argument causes the program to print the version number of the
program (as specified in the C<=head1 VERSION> section of the POD) and
any copyright information (as specified in the C<=head1 COPYRIGHT>
POD section) and then exit.

=back


=head1 DIAGNOSTICS

=head2 Compile-time diagnostics

The following diagnostics are mainly caused by problems in the POD
specification of the command-line interface:

=over

=item Getopt::Euclid was unable to access POD

Something is horribly wrong. Getopt::Euclid was unable to read your
program to extract the POD from it. Check your program's permissions,
though it's a mystery how I<perl> was able to run the program in the
first place, if it's not readable.

=item .pm file cannot define an explicit import() when using Getopt::Euclid

You tried to define an C<import()> subroutine in a module that was also
using Getopt::Euclid. Since the whole point of using Getopt::Euclid in a
module is to have it build an C<import()> for you, supplying your own
C<import()> as well defeats the purpose.


=item Unknown specification: %s

You specified something in a C<=for Euclid> section that
Getopt::Euclid didn't understand. This is often caused by typos, or by
reversing a I<placeholder>.I<type> or I<placeholder>.I<default>
specification (that is, writing I<type>.I<placeholder> or
I<default>.I<placeholder> instead).

=item Unknown type (%s) in specification: %s

=item Unknown .type constraint: %s

Both these errors mean that you specified a type constraint that
Getopt::Euclid didn't recognize. This may have been a typo:

    =for Euclid
        count.type: inetger

or else the module simply doesn't know about the type you specified:

    =for Euclid
        count.type: complex

See L<Standard placeholder types> for a list of types that Getopt::Euclid
I<does> recognize.

=item Invalid .type constraint: %s

You specified a type constraint that isn't valid Perl. For example:

    =for Euclid
        max.type: integer not equals 0

instead of:

    =for Euclid
        max.type: integer != 0

=item Invalid .default value: %s

You specified a default value that isn't valid Perl. For example:

    =for Euclid
        curse.default: *$@!&

instead of:

    =for Euclid
        curse.default: '*$@!&'

=item Getopt::Euclid loaded a second time

You tried to load the module twice in the same program.
Getopt::Euclid doesn't work that way. Load it only once.

=back

=head2 Run-time diagnostics

The following diagnostics are caused by problems in parsing the command-line

=over 

=item Missing required argument(s): %s

One or more arguments specified in the C<REQUIRED ARGUMENTS> POD section
wasn't present on the command-line.


=item Invalid %s argument. %s must be %s but the supplied value (%s) isn't.

Getopt::Euclid recognized the argument you were trying to specify on the
command-line, but the value you gave to one of that argument's placeholders
was of the wrong type.


=item Unknown argument: %s

Getopt::Euclid didn't recognize an argument you were trying to specify on the
command-line. This is often caused by command-line typos.

=back


=head1 CONFIGURATION AND ENVIRONMENT

Getopt::Euclid requires no configuration files or environment variables.


=head1 DEPENDENCIES

=over 

=item *

File::Spec::Functions

=item *

List::Util

=back

=head1 INCOMPATIBILITIES

None reported.


=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Please report any bugs or feature requests to
C<bug-getopt-euclid@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 AUTHOR

Damian Conway  C<< <DCONWAY@cpan.org> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2005, Damian Conway C<< <DCONWAY@cpan.org> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.


=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
