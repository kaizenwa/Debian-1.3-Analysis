## Generic/Standard routines
## by Ken Hollis <khollis@bitgate.com>
##
## This program is GPLed.  Please read "LICENSE" for more information.
## Copyright (C) 1996, Bitgate Software.

sub get_os {
	print "Operating system ... ";

	local($uname) = `uname`;
	chop($uname);
	$uname =~ tr/a-z/A-Z/;

	print "$uname\n";
	return $uname;
}

sub get_lastlog {
	print "Lastlog ... ";

	if (-e "/var/adm/lastlog") {
		local($ll) = "/var/adm/lastlog";
	} elsif (-e "/var/log/lastlog") {
		local($ll) = "/var/log/lastlog";
	} else {
		local($ll) = "/var/adm/wtmp";
	}

	print "$ll\n";
	return $ll;
}

sub get_nobody {
	print "Nobody UID/GID ... ";

	open(PW, "grep nobody /etc/passwd|");
	while(<PW>) {
		chop;
		($un,$pw,$uid,$gid,$gecos,$dir) = split(/:/);
	}
	close(PW);

	if (($uid eq "") || ($gid eq "")) {
		$uid = 65535;
		$gid = 65535;
	}

	print "$uid, $gid\n";
	return "$uid,$gid";
}

sub has_shadow {
	print "Shadow passwords ... ";

	if (-e "/etc/shadow") {
		local($shad) = "Y";
	} else {
		local($shad) = "N";
	}

	print (($shad eq "Y") ? "/etc/shadow\n" : "No shadow\n");
	return $shad;
}

1;
