#!/usr/bin/perl

$state="notstarted";

print "# Remainder generated automatically by mkdescr.pl\n\n";
print "# Remainder derived from:\n\n";
for ($i=0;$i<3;$i++) {
    $_=<>;
    print "# $_";
}

print "\n";

while (<>) {
    $_ =~ s/"/\\\\"/g;
#    print;
    if ($state eq "notstarted") {
	if (m,  3.  General Kernel setup,) {
	    $state="default";
	}
	next;
    }
    elsif (m,^  Load command:,) {
	$state="loadcommand";
    }
    elsif (m,   */sbin/modprobe (.*)\.o( (.*)|)$,) {
	$module=$1;
	print "Module: $1\n";
	if ( $3 eq "") {
	    print "NoParams:\n";
	}
	print " $1 $3\n \n";
	$state="modprobe";
    }
    elsif (m,_________,) {
	if ($state eq "modprobe") {
	    print "\n";
	}
	$state="default";
	$count=0;
    }
    elsif ($state eq "modprobe") {
	if (m,          (.*),){
	    print " $1\n";
	    $count+=1;
	    if ( $count == 13 ) {
		print STDERR "Description for $module is too long !!!\n";
	    }
	}
    }
}
