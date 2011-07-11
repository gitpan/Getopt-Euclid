package Getopt::Euclid::PodExtract;


=head1 NAME

Getopt::Euclid::PodExtract - Perl::Tidy formatter to extract POD from source code

=head1 SYNOPSIS

    use Perl::Tidy;
    my $source = 'somefile.pl';
    my $pod    = '';
    Perl::Tidy::perltidy(
          argv      => [],
          source    => $source,
          formatter => Getopt::Euclid::PodExtract->new(\$pod),
    );
    print $pod;

=head1 DESCRIPTION

This is a formatter to plug into Perl::Tidy. This formatter simply takes source
code and deletes everything except for POD, which it returns in its raw form in
the specified variable. Do not use the destination option of perltidy as it is
ignored when using a formatter.

Perl::Tidy seems to have a more robust POD parsing mechanisms than Pod::Parser
or Pod::Simple, which makes it useful to correctly parse POD code, even when
rogue POD hide inside Perl variables, as in this example:

  use strict;
  use warnings;

  =head1 NAME
  
  Tricky

  =cut

  print "Starting...\n--------\n";
  my $var =<<EOS;

  =head1 FAKE_POD_ENTRY_HERE

  This should not be extracted as POD since it is the content of a variable

  =cut

  EOS

  print $var;
  print "--------\nDone!\n";
  exit;

  __END__

  =head1 SYNOPSIS

  Tricky file to test proper POD parsing

=head1 AUTHOR

Florent Angly C<< <florent.angly@gmail.com> >>

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2011, Florent Angly C<< <florent.angly@gmail.com> >>

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut



sub new {
    # Initialize formatter
    my ($class, $strref) = @_;
    my $self = {};
    bless $self, ref($class) || $class;
    die "Error: Need to initialize the Getopt::Euclid::PodExtract formatter ".
      "with a string reference to store the results but none was given\n" if not
      defined $strref;
    die "Error: Need to initialize the Getopt::Euclid::PodExtract formatter ".
      "with a string reference to store the results but a ".ref($strref).
      " reference was given\n" if (not ref $strref  eq 'SCALAR');
    $self->{_strref} = $strref;
    return $self;
}


sub write_line {
    my ($self, $tokens) = @_;
    # This is called by perltidy, for each source code line
    # Print POD_START, POD and POD_END tokens only
    ${$self->{_strref}} .= $tokens->{_line_text} if $tokens->{_line_type} =~ m/^POD/;
}


1;
