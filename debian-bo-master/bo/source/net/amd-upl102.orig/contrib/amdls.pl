From jgd@acl.lanl.gov Wed Jun 16 12:37:23 1993
Received: from cs.columbia.edu by shekel.cs.columbia.edu with SMTP id AA12047
  (5.65c/IDA-1.4.4.5/jba/jtt/ad for ezk); Wed, 16 Jun 1993 12:37:22 -0400
Received: from cunixf.cc.columbia.edu by cs.columbia.edu (5.65c/0.6/jba+ad+jtt) with SMTP 
	id AA08967; Wed, 16 Jun 1993 12:37:16 -0400
Received: from acl.lanl.gov by cunixf.cc.columbia.edu (5.59/FCB/jba)
	id AA14613; Wed, 16 Jun 93 12:36:51 EDT
Received: by acl.lanl.gov (5.67/SMI-4.1)
	id AA29589; Wed, 16 Jun 93 10:01:51 -0600
Received: from kodiak.acl.lanl.gov by acl.lanl.gov (5.67/SMI-4.1)
	id AA29582; Wed, 16 Jun 93 10:01:48 -0600
Received: by kodiak.acl.lanl.gov (5.65/SMI-4.1)
	id AA10568; Wed, 16 Jun 93 10:01:46 -0600
Message-Id: <9306161601.AA10568@kodiak.acl.lanl.gov>
Reply-To: Jerry G. DeLapp <jgd@acl.lanl.gov>
Errors-To: owner-amd-workers@acl.lanl.gov
From: jgd@acl.lanl.gov (Jerry G. DeLapp)
Sender: <owner-amd-workers@acl.lanl.gov>
Subject: Perl script to front-end ls for amd
To: amd-workers@acl.lanl.gov
Date: Wed, 16 Jun 93 10:01:46 -0600

Appended below is the perl script I mentioned in a previous posting that shows
the possible contents of an amd mount point without (necessarily) causing
things to mount. Enjoy.

Jerry G. DeLapp -- ACL System Scientist -- (505) 665-4531 <jgd@lanl.gov>
Computing beyond the Bleeding Edge -- The Advanced Computing Laboratory

#!/usr/bin/perl
#$Id: aml,v 2.3 1993/06/16 15:55:23 jgd Exp $
# aml -- a 'front-end' to ls perl script which understands amd mount points.

# First, some administrativa...

$copyright=<<EndofCopyright;

			Los Alamos National Laboratory

Copyright, 1993.  The Regents of the University of California.  This
software was produced under a U.S. Government contract (W-7405-ENG-36)
by Los Alamos National Laboratory, which is operated by the University
of California for the U.S. Department of Energy.  The U.S. Government
is licensed to use, reproduce, and distribute this software.
Permission is granted to the public to copy and use this software
without charge, provided that this Notice and any statement of
authorship are reproduced on all copies.  Neither the Government nor
the University makes any warranty, express or implied, or assumes any
liability or responsibility for the use of this software.

EndofCopyright

# Suggestions for modification/enhancements to this script are welcome at
# jgd@acl.lanl.gov

CONFIG:
{
    $amq='/etc/amq';
    $ypcat='/usr/bin/ypcat';
    $pwd='/usr/bin/pwd';
    $ls='/bin/ls';
    %amd=();
    $literal=0;
    $lsargs='-1' unless (-t STDOUT);
    $screen_width=80;			# Should probably use ioctl here
    chop($here=`$pwd 2> /dev/null`);
}
{
    local(*AMQ);
    open(AMQ,"$amq |") || die "Couldn't instantiate $amq";
    grep(/([^\s]+)\s+toplvl\s+([^\s]+)/ && ($amd{$1}=$2), <AMQ>);
    close AMQ;
}

@argv=@ARGV;
push (@argv,".") unless @argv;	# To handle no arguments

while ($a = shift(@argv)) {

    ((! $literal) && ($a=~/^--$/)) && # double minus to allow -names
	do {
	    $literal == 1;
	    next;
	};
    
    ((! $literal) && ($a=~/^-/)) &&
	do {
	    $lsargs.=" $a";
	    next;
	};

    ($a eq ".") && defined $amd{$here} &&
	do {
	    &amdls($here);
	    next;
	};
    
    defined $amd{$a} &&
	do {
	    &amdls($a);
	    next;
	};
    
    defined $amd{$here.$a} &&
	do {
	    &amdls($here.$a);
            next;
        };

    defined $amd{$here."/".$a} &&
	do {
	    &amdls($here."/".$a);
	    next;
	};

    system("$ls $lsargs $a");

};
exit;
sub amdls {
    local($a)=@_;
    die "No directory specified! This shouldn't happen\n " unless $a;
    local(*MAP);
    local(%map);

    # Gather the content of the pertinent amd map
    if ($amd{$a} =~ /^\//) {
	open(MAP,"< $amd{$a}") || die "Couldn't read map file for $a";
    } else {
	open(MAP,"$ypcat -k $amd{$a} |") || die "Couldn't read NIS map for $a";
    }
    grep(/^([^\s]+)\s+(\S.*)/ && ($map{$1}=$2),<MAP>);
    close MAP;

    # At this point we have a sorted set of all keys and values for the
    # pertinent amd mount point in %map

    local($max);
    foreach $i (sort keys %map) { # What is the longest name in the output?
	$max = (length($i) > $max) ? length($i) : $max;
    }

    # Now actually print stuff

    if ($lsargs =~ /l/) {	# Most complicated
	local($formname)='BAWDY';
	# Some day I need to modify this to print the map such that you could
	# feed it back to amd verbatim and have it work.
	eval join('',
		  "format $formname =\n",
		  '@','<' x ($max-1),
		  ' ^','<' x ($screen_width-$max-3),"\n",
		  '$i,$map{$i}',"\n",
		  '~~',' ' x ($max-2),
		  ' ^','<' x ($screen_width-$max-3),"\n",
		  '$map{$i}',"\n.\n");
	$~=$formname;
	$-=0;
	$:=~s/-//;
	foreach $i (sort keys %map) {
	    write;$^-0;
	}
	$:.='-';
    } elsif ($lsargs=~/1/) {	# One entry per line
	foreach $i (sort keys %map) {
	    printf("%s\n",$i);
	}
    } else {			# Print them across the line
	local($pos)=1;
	foreach $i (sort keys %map) {
	    printf("%-${max}s",$i);
	    $pos += $max;
	    if ( ($pos+$max) > $screen_width ) {
		printf("\n");
		$pos=1;
	    } else {
		printf(" ");
		$pos++;
	    }
	}
	printf("\n") unless $pos == 1;
    }
}

