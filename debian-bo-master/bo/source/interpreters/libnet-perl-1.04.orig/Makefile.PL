# This -*- perl -*- script makes the Makefile
# $Id: Makefile.PL,v 1.8 1996/03/28 08:03:48 gbarr Exp $

#--- Distribution section ---

$NAME     = 'Net';
$DISTNAME = "libnet";
$VERSION  = "1.04";

BEGIN { require 5.002 }

use ExtUtils::MakeMaker;
use ExtUtils::Manifest qw(maniread);

#--- MY package

sub MY::libscan
{
 my($self,$path) = @_;

 return '' 
	if($path =~ m:/(RCS|CVS|SCCS)/: ||
	   $path =~ m:[~%]$: ||
	   $path =~ m:\.(orig|rej|nfs)$:
	  );
 
 $path;
}

#--- Installation check

sub chk_version
{
 my($pkg,$wanted,$msg) = @_;

 $| = 1;
 print "Checking for $pkg...";

 eval { my $p; ($p = $pkg . ".pm") =~ s#::#/#g; require $p; };

 my $vstr = ${"${pkg}::VERSION"} ? "found v" . ${"${pkg}::VERSION"}
				 : "not found";
 my $vnum = ${"${pkg}::VERSION"} || 0;

 print $vnum >= $wanted ? "ok\n" : " " . $vstr . "\n";

 $vnum >= $wanted;
}

#--- Check for Socket

chk_version(Socket => '1.30') or
    warn  "\n"
	. "*** For Net::Cmd to work you require version 1.30, or later, of\n"
	. "    Socket.pm from CPAN/modules/by-module/Socket/Socket-x.x.tar.gz\n\n";

chk_version(IO::Socket => '1.05') or
    warn  "\n"
	. "*** For Net::Cmd to work you require version 1.05, or later, of\n"
	. "    IO/Socket.pm from CPAN/modules/by-module/IO/IO-x.x.tar.gz\n\n";

#--- Create Net::Config

use IO::File;

my $config = "Net/Config.pm";

# Use %INC and IO::File to determine hoe this machine
# maps package names to path names

foreach (keys %INC) {
 last if ($config = $_) =~ s/^IO(.)File/Net$1Config/;
}

system($^X, 'Configure', -f $config ? '-noprompt' : '-prompt');

#--- Write the Makefile

my @clean = qw(*% *.html *.b[ac]k *.old);

push @clean, map { "*/$_" } @clean;

WriteMakefile(
	VERSION   => $VERSION,
	DISTNAME  => $DISTNAME,
	NAME      => $NAME,
 
	'dist'    => {COMPRESS => 'gzip -9f',
		      SUFFIX   => '.gz', 
		      DIST_DEFAULT => 'all tardist',
		     },
 
	'clean'   => {FILES => join(" ", @clean)},
);

