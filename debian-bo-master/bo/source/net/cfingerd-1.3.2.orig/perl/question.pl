## Ask question script
## by Ken Hollis <khollis@bitgate.com>
##
## This has been tested in JigModIRC2.8.21, and XIRC.  It also includes
## a routine to display text files.
##
## This program is GPLed.  Please read LICENSE for more information.
## Copyright (C) 1996, Bitgate Software.
##
## Directions for inclusion:
##	Include "require 'question.pl'" in your Perl program, and call
##	ask_question to ask your question.

sub ask_question {
	local($question, $value, $type, $cfile) = @_;

repeat_ask:
	print "$question ";
	if ($type eq "1") {
		$value =~ tr/a-z/A-Z/;
		print (($value eq "Y") ? "[Y/n/?] " : "[y/N/?] ");
	} elsif ($type eq "2") {
		print "[$value] ";
	} elsif ($type eq "3") {
		print "$value ";
	}

	$entry = <stdin>;
	chop($entry);

	if ($type eq "1") {
		$entry =~ tr/a-z/A-Z/;
		if ($entry eq "") {
			$entry = $value;
		} elsif ($entry eq "Y") {
			$entry = "Y";
		} elsif ($entry eq "N") {
			$entry = "N";
		} else {
			&disp_file($cfile);
			goto repeat_ask;
		}
	} elsif (($type eq "2") || ($type eq "3")) {
		if ($entry eq "") {
			$entry = $value;
		}
	}

	return $entry;
}

sub disp_file {
	open(FL, "$_[0]") || die "$_[0]: $!\n";
	print $_ while (<FL>);
}

1;
