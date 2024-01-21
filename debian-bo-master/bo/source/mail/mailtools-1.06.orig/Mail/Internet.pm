# Mail::Internet.pm
#
# Copyright (c) 1995 Graham Barr <Graham.Barr@tiuk.ti.com>. All rights
# reserved. This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package Mail::Internet;

=head1 NAME

Mail::Internet - manipulate Internet format (RFC 822) mail messages

=head1 SYNOPSIS

use Mail::Internet;

=head1 DESCRIPTION

This package provides a class object which can be used for reading, creating,
manipulating and writing a message with RFC822 compliant headers.

=head2 METHODS

=over 4

=item body ()

Returns the body of the message. This is a reference to an array.
Each entry in the array represents a single line in the message.

=item print_header ( [ FILEHANDLE ] )

=item print_body ( [ FILEHANDLE ] )

=item print ( [ FILEHANDLE ] )

Print the header, body or whole message to file descriptor I<FILEHANDLE>.
I<$fd> should be a reference to a GLOB. If I<FILEHANDLE> is not given the
output will be sent to STDOUT.

    $mail->print( \*STDOUT );  # Print message to STDOUT

=item head ()

Returns the C<Mail::Header> object which holds the headers for the current
message

=back

=head1 UTILITY METHODS

The following methods are more a utility type that a manipulation
type of method.

=over 4

=item remove_sig ( [ NLINES ] )

Attempts to remove a users signature from the body of a message. It does this 
by looking for a line equal to C<'-- '> within the last C<NLINES> of the
message. If found then that line and all lines after it will be removed. If
C<NLINES> is not given a default value of 10 will be used. This would be of
most use in auto-reply scripts.

=item tidy_body ()

Removes all leading and trailing lines from the body that only contain
white spaces.

=item reply ()

Create a new object with header initialised for a reply to the current 
object. And the body will be a copy of the current message indented.

=item add_signature ( [ FILE ] )

Append a signature to the message. C<FILE> is a file which contains
the signature, if not given then the file "$ENV{HOME}/.signature"
will be checked for.

=item smtpsend ()

Send a Mail::Internet message via SMTP

The message will be sent to all addresses on the To, Cc and Bcc
lines. The SMTP host is found by attempting connections first
to hosts specified in C<$ENV{SMTPHOSTS}>, a colon separated list,
then C<mailhost> and C<localhost>.

=item nntppost ()

Post an article via NNTP, require News::NNTPClient.

=back

=head1 SEE ALSO

L<Mail::Header>
L<Mail::Address>

=head1 AUTHOR

Graham Barr <Graham.Barr@tiuk.ti.com>

=head1 REVISION

$Revision: 1.23 $

The VERSION is derived from the revision turning each number after the
first dot into a 2 digit number so

	Revision 1.8   => VERSION 1.08
	Revision 1.2.3 => VERSION 1.0203

=head1 COPYRIGHT

Copyright (c) 1995 Graham Barr. All rights reserved. This program is free
software; you can redistribute it and/or modify it under the same terms
as Perl itself.

=cut

require 5.002;

use Carp;
use AutoLoader;
use Mail::Header;
use vars qw($VERSION @ISA);

# This one will turn 1.2 => 1.02 and 1.2.3 => 1.0203 and so on ...
$VERSION = do{my @r=(q$Revision: 1.23 $=~/(\d+)/g);sprintf "%d."."%02d"x$#r,@r};
@ISA     = qw(AutoLoader);

# stop import being inherited from AutoLoader by use Mail::Internet :-(
sub import {}

sub new
{
 my $self = shift;
 my $type = ref($self) || $self;

 my $me = bless {}, $type;

 my $arr  = [];
 my ($line,$buffer);
 my $fd = undef;

 $me->head->fold_length(79); # Default fold length

 @{$arr} = @_ if(scalar(@_) > 1);

 if(scalar(@_) == 1)
  {
   if(ref($_[0]) eq 'ARRAY')
    {
     @{$arr} = @{$_[0]};
    }
   elsif(defined fileno($fd = $_[0]))
    {
     undef $arr;
    }
  }

 if(defined $arr)
  {
   $me->header($arr);
   $me->body($arr);
  } 
 else
  {
   $me->read_header($fd);
   $me->read_body($fd);
  }

 return $me;
}

sub read
{
 my $me = shift;

 $me->read_header(@_);
 $me->read_body(@_);
}

sub read_body
{
 my($me,$fd) = @_;

 $me->body( [ <$fd> ] );
}


sub extract
{
 my $me = shift;
 my $arg = shift;

 $me->head->extract($arg);
 $me->body($arg);
}


sub body
{
 my $me = shift;
 my $body = $me->{'mail_inet_body'} ||= [];

 if(@_)
  {
   my $new = shift;
   $me->{'mail_inet_body'} = ref($new) eq 'ARRAY' ? $new : [ $new ];
  }

 return $body;
}

sub header { shift->head->header(@_) }
sub fold { shift->head->fold(@_) }
sub fold_length { shift->head->fold_length(@_) }
sub combine { shift->head->combine(@_) }
sub print_header { shift->{'mail_inet_head'}->print(@_) }
sub head { shift->{'mail_inet_head'} ||= new Mail::Header }

sub read_header
{
 my $me = shift;
 my $head = $me->head;

 $head->read(@_);
 $head->header();
}

sub clean_header
{
 carp "clean_header depreciated, use ->header" if $^W;
 shift->header();
}

sub tidy_headers
{
 carp "tidy_headers no longer required" if $^W;
}


sub add
{
 my $me = shift;
 my $head = $me->head;
 my $ret;

 while(@_)
  {
   ($tag,$line) = splice(@_,0,2);

   $ret = $head->add($tag,$line,-1) or
	return undef;
  }

 $ret;
}

sub replace
{
 my $me = shift;
 my $head = $me->head;
 my $ret;

 while(@_)
  {
   ($tag,$line) = splice(@_,0,2);

   $ret = $head->replace($tag,$line,0) or
	return undef;
  }

 $ret;
}

sub get
{
 my $me = shift;
 my $head = $me->head;
 my @ret = ();
 my $tag;

 foreach $tag (@_)
  {
   last
	if push(@val, $head->get($tag)) && !wantarray;
  }

 wantarray ? @val : shift @val;
}

sub delete
{
 my $me = shift;
 my $head = $me->head;
 my @ret = ();
 my $tag;

 foreach $tag (@_)
  {
   push(@ret, $head->delete($tag));
  }

 @ret;
}

sub dup
{
 my $me = shift;
 my $type = ref($me);
 my $dup = $type->new;

 $dup->{'mail_inet_body'} = [@{$me->body}]
	if exists $me->{'mail_inet_body'};

 $dup->{'mail_inet_head'} = $me->{'mail_inet_head'}->dup
	if exists $me->{'mail_inet_head'};

 $dup;
}

sub empty
{
 my $me = shift;

 %{*$me} = ();

 1;
}

sub print_body
{
 my $me = shift;
 my $fd = shift || \*STDOUT;
 my $ln;

 foreach $ln (@{$me->body})
  {
   print $fd $ln or
	return 0;
  }

 1;
}

sub print
{
 my $me = shift;
 my $fd = shift || \*STDOUT;

 $me->print_header($fd)
    and print $fd "\n"
    and $me->print_body($fd);
}

sub remove_sig
{
 my $me = shift;
 my $nlines = shift || 10;

 my $body = $me->body;
 my($line,$i);

 $line = scalar(@{$body});
 return unless($line);

 while($i++ < $nlines && $line--)
  {
   if($body->[$line] =~ /\A--\040?[\r\n]+/)
    {
     splice(@{$body},$line,$i);
     last;
    }
  }
}


sub tidy_body
{
 my $me = shift;

 my $body = $me->body;
 my $line;

 if(scalar(@{$body}))
  {
   shift @$body
        while(scalar(@{$body}) && $body->[0] =~ /\A\s*\Z/);
   pop @$body
        while(scalar(@{$body}) && $body->[-1] =~ /\A\s*\Z/);
  }

 return $body;
}

# Auto loaded methods go after __END__
__END__

sub reply;


use Mail::Address;

 sub reply
{
 my $me = shift;
 my %arg = @_;
 my $pkg = ref $me;
 my @reply = ();

 if(open(MAILHDR,"$ENV{HOME}/.mailhdr")) 
  {
   # User has defined a mail header template
   @reply = <MAILHDR>;
   close(MAILHDR);
  }

 my $reply = $pkg->new(\@reply);

 my($to,$cc,$name,$body,$id);

 # The Subject line

 my $subject = $me->get('Subject') || "";

 $subject = "Re: " . $subject if($subject =~ /\S+/ && $subject !~ /Re:/i);

 $reply->replace('Subject',$subject);

 # Locate who we are sending to
 $to = $me->get('Reply-To')
       || $me->get('Return-Path')
       || $me->get('From')
       || "";

 # Mail::Address->parse returns a list of refs to a 2 element array
 my $sender = (Mail::Address->parse($to))[0];

 $name = $sender->name;
 $id = $sender->address;

 unless(defined $name)
  {
   my $fr = $me->get('From');

   $fr = (Mail::Address->parse($fr))[0] if(defined $fr);
   $name = $fr->name if(defined $fr);
  }

 my $indent = $arg{Indent} || ">";

 if($indent =~ /%/) 
  {
   my %hash = ( '%' => '%');
   my @name = grep(do { length > 0 }, split(/[\n\s]+/,$name || ""));
   my @tmp;

   @name = "" unless(@name);

   $hash{f} = $name[0];
   $hash{F} = $#name ? substr($hash{f},0,1) : $hash{f};

   $hash{l} = $#name ? $name[$#name] : "";
   $hash{L} = substr($hash{l},0,1) || "";

   $hash{n} = $name || "";
   $hash{I} = join("",grep($_ = substr($_,0,1), @tmp = @name));

   $indent =~ s/%(.)/defined $hash{$1} ? $hash{$1} : $1/eg;
  }

 $reply->replace('To', $id);

 # Find addresses not to include
 my %nocc = ();
 my $mailaddresses = $ENV{MAILADDRESSES} || "";
 my $addr;

 $nocc{lc $id} = 1;

 foreach $addr (Mail::Address->parse($reply->get('Bcc'),$mailaddresses)) 
  {
   my $lc = lc $addr->address;
   $nocc{$lc} = 1;
  }

 if($arg{ReplyAll} || 0)
  {
   # Who shall we copy this to
   my %cc = ();

   foreach $addr (Mail::Address->parse($me->get('To'),$me->get('Cc'))) 
    {
     my $lc = lc $addr->address;
     $cc{$lc} = $addr->format unless(defined $nocc{$lc});
    }
   $cc = join(', ',values %cc);

   $reply->replace('Cc', $cc);
  }

 # References
 my $refs = $me->get('References') || "";
 my $mid = $me->get('Message-Id');

 $refs .= " " . $mid if(defined $mid);
 $reply->replace('References',$refs);

 # In-Reply-To
 my $date = $me->get('Date');
 my $inreply = "";

 if(defined $mid)
  {
   $inreply  = $mid;
   $inreply .= " from " . $name if(defined $name);
   $inreply .= " on " . $date if(defined $date);
  }
 elsif(defined $name)
  {
   $inreply = $name . "'s message";
   $inreply .= "of " . $date if(defined $date);
  }

 $reply->replace('In-Reply-To', $inreply);

 # Quote the body
 $body  = $reply->body;

 @$body = @{$me->body};		# copy body
 $reply->remove_sig;		# remove signature, if any
 $reply->tidy_body;		# tidy up
 map { s/\A/$indent/ } @$body;	# indent

 # Add references
 unshift @{$body}, (defined $name ? $name . " " : "") . "<$id> writes:\n";

 if(defined $arg{Keep} && 'ARRAY' eq ref($arg{Keep})) 
  {
   # Copy lines from the original
   my $keep;

   foreach $keep (@{$arg{Keep}}) 
    {
     my $ln = $me->get($keep);
     $reply->replace($keep,$ln) if(defined $ln);
    }
  }

 if(defined $arg{Exclude} && 'ARRAY' eq ref($arg{Exclude}))
  {
   # Exclude lines
   $reply->delete(@{$arg{Exclude}});
  }

 # remove empty header lins
 $reply->head->cleanup;

 $reply;
}

sub add_signature
{
 my $me = shift;
 carp "add_signature depriciated, use ->sign" if $^W;
 $me->sign(File => shift || "$ENV{HOME}/.signature");
}

sub sign
{
 my $me = shift;
 my %arg = @_;
 my $sig;
 my @sig;

 if($sig = delete $arg{File})
  {
   local *SIG;

   if(open(SIG,$sig))
    {
     while(<SIG>) { last unless /\A(--)?\s*\Z/; }

     @sig = ($_,<SIG>,"\n");

     close(SIG);
    }
  }
 elsif($sig = delete $arg{Signature})
  {
   @sig = ref($sig) ? @$sig : split(/\n/, $sig);
  }

 if(@sig)
  {
   $me->remove_sig;
   map(s/\n?\Z/\n/,@sig);
   push(@{$me->body}, "-- \n",@sig);
  }
}

sub smtpsend;

use Carp;
use Mail::Util qw(mailaddress);
use Mail::Address;
use Net::Domain qw(hostname);
use Net::SMTP;


 sub smtpsend 
{
 my $src  = shift;
 my($mail,$smtp,@hosts);

 require Net::SMTP;

 @hosts = qw(mailhost localhost);
 unshift(@hosts, split(/:/, $ENV{SMTPHOSTS})) if(defined $ENV{SMTPHOSTS});

 foreach $host (@hosts) {
  $smtp = eval { Net::SMTP->new($host) };
  last if(defined $smtp);
 }

 croak "Cannot initiate a SMTP connection" unless(defined $smtp);

 $smtp->hello( hostname() );
 $mail = $src->dup;

 $mail->delete('From '); # Just in case :-)

 $mail->replace('X-Mailer', "Perl5 Mail::Internet v" . $Mail::Internet::VERSION);

 # Ensure the mail has the following headers
 # Sender, From, Reply-To

 my($from,$name,$tag);

 $name = (getpwuid($>))[6] || $ENV{NAME} || "";
 while($name =~ s/\([^\(]*\)//) { 1; }

 $from = sprintf "%s <%s>", $name, mailaddress();
 $from =~ s/\s{2,}/ /g;

 foreach $tag (qw(Sender From Reply-To))
  {
   $mail->add($tag,$from) unless($mail->get($tag));
  }

 # An original message should not have any Recieved lines

 $mail->delete('Recieved');

 # Who is it to

 my @rcpt = ($mail->get('To', 'Cc', 'Bcc'));
 my @addr = map($_->address, Mail::Address->parse(@rcpt));

 return () unless(@addr);

 $mail->delete('Bcc'); # Remove blind Cc's
 $mail->clean_header;

 # Send it

 my $ok = $smtp->mail( mailaddress() ) &&
            $smtp->to(@addr) &&
            $smtp->data(join("", @{$mail->header},"\n",@{$mail->body}));

 $smtp->quit;

 $ok ? @addr : ();
}

sub nntppost;

use Mail::Util qw(mailaddress);


require News::NNTPClient;

 sub nntppost
{
 my $mail = shift;

 my $groups = $mail->get('Newsgroups') || "";
 my @groups = split(/[\s,]+/,$groups);

 return () unless @groups;

 my $art = $mail->dup;

 $art->clean_header;
 $art->replace('X-Mailer', "Perl5 Mail::Internet v" . $Mail::Internet::VERSION);

 unless($art->get('From'))
  {
   my $name = $ENV{NAME} || (getpwuid($>))[6];
   while( $name =~ s/\([^\(]*\)// ) {1};
   $art->replace('From',$name . " <" . mailaddress() . ">");
  }

 # Remove these incase the NNTP host decides to mail as well as me
 $art->delete(qw(To Cc Bcc)); 
 $art->clean_header;

 my $news = new News::NNTPClient;
 $news->post(@{$art->header},"\n",@{$art->body});

 my $code = $news->code;
 $news->quit;

 return 240 == $code ? @groups : ();
}


1; # keep require happy



