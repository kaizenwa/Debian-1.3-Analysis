# Mail::Util.pm
#
# Copyright (c) 1995 Graham Barr <bodg@tiuk.ti.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package Mail::Util;

$VERSION = sprintf("%d.%02d", q$Revision: 1.10 $ =~ /(\d+)\.(\d+)/);
sub Version { $VERSION }

=head1 NAME

Mail::Util - mail utility functions

=head1 SYNOPSIS

use Mail::Util qw( ... );

=head1 DESCRIPTION

This package provides several mail related utility functions. Any function
required must by explicitly listed on the use line to be exported into
the calling package.

=head2 read_mbox( $file )

Read C<$file>, a binmail mailbox file, and return a list of  references.
Each reference is a reference to an array containg one message.

=head2 maildomain()

Attempt to determine the current uers mail domain string via the following
methods

 Look for a sendmail.cf file and extract DH parameter
 Look for a smail config file and usr the first host defined in hostname(s)
 Try an SMTP connect (if Net::SMTP exists) first to mailhost then localhost
 Use value from Net::Domain::domainname (if Net::Domain exists)

=head2 mailaddress()

Return a guess at the current users mail address. The user can force
the return value by setting C<$ENV{MAILADDRESS}>

=head1 AUTHOR

Graham Barr <bodg@tiuk.ti.com>

=head1 COPYRIGHT

Copyright (c) 1995 Graham Barr. All rights reserved. This program is free
software; you can redistribute it and/or modify it under the same terms
as Perl itself.

=head1 REVISION

$Revision: 1.10 $

=cut

require 5.000;
use AutoLoader;
require Exporter;
use Carp;

@ISA = qw(Exporter AutoLoader);

@EXPORT_OK = qw(read_mbox maildomain mailaddress smtpsend);

1;

__END__

sub read_mbox;


use FileHandle;
require POSIX;

 sub read_mbox {
 my $file  = shift;
 my $fd    = FileHandle->new($file,"r") || croak "cannot open '$file': $!\n";
 my @mail  = ();
 my $mail  = [];
 my $blank = 1;

 local $_;

 while(<$fd>) {
  if($blank && /\AFrom .*\d{4}/) {
   push(@mail, $mail) if scalar(@{$mail});
   $mail = [ $_ ];
   $blank = 0;
  }
  else {
   $blank = m#\A\Z#o ? 1 : 0;
   push(@{$mail}, $_);
  }
 }

 push(@mail, $mail) if scalar(@{$mail});

 $fd->close;

 return wantarray ? @mail : \@mail;
}


sub maildomain {

 ##
 ## return imediately if already found
 ##

 return $domain if(defined $domain);

 ##
 ## Try sendmail config file if exists
 ##

 local *CF;
 my @sendmailcf = qw(/etc /etc/sendmail /etc/ucblib /etc/mail /usr/lib);

 my $config = (grep(-r, map("$_/sendmail.cf", @sendmailcf)))[0];

 if(defined $config && open(CF,$config)) {
  while(<CF>) {
   if(/\ADH(\S+)/) {
    $domain = $1;
    last;
   }
  }
  close(CF);
  return $domain if(defined $domain);
 }

 ##
 ## Try smail config file if exists
 ##

 if(open(CF,"/usr/lib/smail/config")) {
  while(<CF>) {
   if(/\A\s*hostnames?\s*=\s*(\S+)/) {
    $domain = (split(/:/,$1))[0];
    last;
   }
  }
  close(CF);
  return $domain if(defined $domain);
 }

 ##
 ## Try a SMTP connection to 'mailhost'
 ##

 if(eval "require Net::SMTP") {
  my $host;

  foreach $host (qw(mailhost localhost)) {
   my $smtp = eval { Net::SMTP->new($host) };

   if(defined $smtp) {
    $domain = $smtp->domain;
    $smtp->quit;
    last;
   }
  }
 }

 ##
 ## Use internet(DNS) domain name, if it can be found
 ##

 unless(defined $domain) {
  if(eval "require Net::Domain") {
   $domain = Net::Domain::domainname();
  }
 }

 $domain = "localhost" unless(defined $domain);

 return $domain;
}


sub mailaddress {

 ##
 ## Return imediately if already found
 ##

 return $mailaddress if(defined $mailaddress);

 ##
 ## Get user name from environment
 ##

 $mailaddress = $ENV{MAILADDRESS} ||
                $ENV{USER} ||
                $ENV{LOGNAME} ||
                (getpwuid($>))[6] ||
                "postmaster";

 ##
 ## Add domain if it does not exist
 ##

 $mailaddress .= "@" . maildomain() unless($mailaddress =~ /\@/);

 $mailaddress;
}



