#!/usr/bin/perl
# 
# Author:       James Brister <brister@vix.com> -- berkeley-unix --
# Start Date:   Wed Jan  3 00:09:01 1996
# Project:      INN -- innfeed
# File:         testListener.pl
# RCSId:        $Id: testListener.pl,v 1.9 1996/03/23 10:16:37 brister Exp $
# Description:  Generate news files for testing the innfeed feeder.
#
#               Run like this:
#
#			testListener.pl -t 30 -d tmp | innfeed
#
#		or like this:
#
#			innfeed -s 'perl testListener.pl -t 30 -d tmp'
# 

$0 =~ s!.*/!! ;

require 'getopts.pl' ;

$usage = "$0 [ -a -d directory -c count -t sleep-amt ] peername ...\n" .
" -a is for duplicate article id's periodically and random file deletions\n" ;

&Getopts ("d:a:c:t:rl:") || die $usage ;

$total = $opt_c ;

$sleepAmt = 1 ;
$sleepAmt = $opt_t if ($opt_t =~ /^[\d\.]+/) ;

$lineCount = 50 ;
$lineCount = $opt_l if ($opt_l =~ /^\d+$/) ;

$directory = "." ;
$directory = $opt_d if $opt_d ;

$SIG{'INT'} = 'IGNORE' ;
$SIG{'TERM'} = 'sigHup' ;
$SIG{'QUIT'} = 'sigHup' ;
$SIG{'HUP'} = 'sigHup' ;

sub sigHup {
	exit (1) ;
}


$monstr = "JanFebMarAprMayJunJulAugSepOctNovDec" ;
$letstr = "abcdefghijklmnopqrstuvwxyz" ;

sub createArticle {
	local ($counter) = @_ ;
	local ($filename,$msgid,$i) ;
	local ($time) = $^T ;
	local ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)
                                                  = gmtime($time);
	local ($index) = $counter ;


	if ($opt_a && ((int (rand (4)) % 2) == 0)) {
		$index = int ($index / 2) ;
	}

	$msgid = "<$index.$$.$time\@home.octet.com.au>" ;

	$filename = sprintf ("%s/SampleArticle.%06d",$directory,$index) ;

	open (ARTICLE,">$filename") || 
		die "open ($filename): $!\n" ;
	print ARTICLE  "Path: home.octet.com.au!not-for-mail\n" ;
	print ARTICLE "From: brister\@home.octet.com.au\n" ;
	print ARTICLE "Newsgroups: junk\n" ;
	print ARTICLE "Subject: Test\n" ;
	print ARTICLE "Date: " ;

	printf ARTICLE "%d %s %d %02d:%02d:%02d UTC\n",
		$mday, substr($monstr,$mon * 3, 3), $year + 1900,
		$hour, $min, $sec ;

	print ARTICLE "Organization: None that I can think of\n" ;
	print ARTICLE "Lines: 5\n" ;
	print ARTICLE "Distribution: world\n" ;
	print ARTICLE "Message-ID: $msgid\n" ;
	print ARTICLE "NNTP-Posting-Host: localhost\n" ;
	print ARTICLE "\n" ;
	
	for ($i = 0 ; $i < $lineCount ; $i++) {
		print ARTICLE "x" x (40 + rand(39)), "\n";
	}
	print ARTICLE ".This line has a leading dot.\n" ;
	print ARTICLE "And the next line only has a dot.\n" ;
	print ARTICLE ".\n" ;
	print ARTICLE "And the next line has just two dots...\n" ;
	print ARTICLE "..\n" ;
	print ARTICLE "foo\n" ;
	print ARTICLE "And the next line is the last line of the article\n" ;
	print ARTICLE "and it only has a single dot on it.\n" ;
	print ARTICLE ".\n" ;

	close (ARTICLE) ;

	return ($msgid, $filename) ;
}

srand ;


$| = 1 ;

open (STDERR,">>/tmp/TESTLISTENER.LOG") || die ;

srand ;
$sleepAmt = 1 if ($sleepAmt < 0) ;

die "Must give peernames on command line:\n$usage" if ( ! @ARGV ) ;

for ( $i = 0 ; $total == 0 || $i < $total ; $i++ ) {
	($msgid,$filename) = &createArticle ($i) ;
	if ($opt_a && ((rand (3) % 3) == 0)) { 
		print TTY "Removing file $filename\n" ;
		unlink ($filename) if $opt_r ;
	}
	print "$filename $msgid @ARGV\n" ;
	select (undef,undef,undef,(rand ($sleepAmt-1) + 1)) if $sleepAmt ;
}

sleep 11500 ;
