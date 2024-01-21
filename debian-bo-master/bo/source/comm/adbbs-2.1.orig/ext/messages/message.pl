#!/usr/local/bin/perl
#
# message.pl (c) 1996 aD! Data Systems
# Chris Church (psylark@aD.org)
# v 1.3
# Message Base System for aD!BBS
# see ext/message/README for details
#----------------------------------------


######################################################
# $MESDIR = the directory in which the base dir's lie
# $basest = the original screen (the following bases..)
#   string.
# $basecst = choose a base string
# $mestop = the following messsages... string
# $whichm = which message to read.. string
# $maction = action after reading message string
#
# $ed = editor to use for editing messages
#
# You should probably list all options on each string..
# As I've shown here..
######################################################

$MESDIR = '/usr/multi/bbs/mess';
$basest = "The following bases are available for reading :\n";
$basecst = "Which base would you like to read? \[Hit \'E\' To Exit\] -> ";
$mestop = "The Following Messages Are Available:\n";
$whichm = "\nWhich message to read? \[\'B\' Return To Bases, \'P\' To Post\]-> "; 
$maction = "\n[R]eply [D]elete [C]ontinue -> ";
$ed = "/usr/local/bin/pico.safe -t";

################################
# Do NOT Edit Past Here!       #
################################

require '/usr/local/etc/adbbs.cf';
require 'bbs-lib.pl';



($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
	localtime(time);


&clear;
&int_envvars;

sub get_bases {
 &clear;
 opendir(MD, "$MESDIR") || warn("Unable to open $MESDIR -> $!\n");
 @mesfs = grep(!/^\./, readdir(MD));
 closedir(MD);
 print("$basest");
 foreach $b (0..$#mesfs) {
	$c = $b; $c++;
	print("\($c\) $mesfs[$b]\n");
	}
 print("$basecst");
 chomp($enval = <>);
 if($enval =~ /e/i) {
	return;
	} elsif($enval =~ /\d+/) {
		$enval--;
		&get_messages($mesfs[$enval]);
		} else {
			&get_bases;
			}
}

sub get_messages {
 &clear;
 $LD2 = shift(@_);
 $LD = $MESDIR . "/" . $LD2;
 undef(@mestf);
 opendir(LD, "$LD") || warn("Unable to open $LD -> $!\n");
 @mess = grep(!/^\./, readdir(LD));
 closedir(LD);
 foreach $b (0..$#mess) {
	$file = $LD . "/". $mess[$b];
	open(file) || warn("Unable to open $file -> $!\n");
	while(<file>) {
		if(/^Subject: (.*)/) {
			$sub = $1;
			}
		if(/^From: (.*)/) {
			$frm = $1;
			}
		if(/^Time: (.*)/) {
			$tme = $1;
			}

		}
	push(@mestf, "$b : $sub : $frm : $tme");
	}
 if($#mestf > 20) {
	$ff = time();
	open(TH, "> /tmp/.$$.$ff") || warn("Unable to create temp file -> $!\n");
	foreach $b (0..$#mestf) {
		print(TH "$mestf[$b]\n");
		}
	close(TH);
	&rsp("/tmp/.$$.$ff");
	unlink("/tmp/.$$.$ff");
	} else {
		 print("$mestop");
		 foreach $b (0..$#mestf) {
			print("$mestf[$b]\n");
			}
		}
 print("$whichm");
 chomp($enval = <>);
 if($enval =~ /b/i) {
	&get_bases;
	} elsif($enval =~ /\d+/) {
		&read_mess($LD, $enval, $LD2);
		&get_messages($LD2);
	} elsif($enval =~ /p/i) {
		&post($LD, $#mestf);
		&get_messages($LD2);
		} elsif($enval !~ /b/i || $enval !~ /\d+/ || $enval !~ /p/i) {
			&get_messages($LD2);
			}
}


sub read_mess {
 &clear;
 $DIR = shift(@_);
 $MID = shift(@_);
 $LD2 = shift(@_);
 $TEMPF = "/tmp/.$MID.$$";
 $file = $DIR . "/" . $MID;
 open(FH, "$file") || warn("unable to open $file -> $!\n");
 open(TH, "> $TEMPF") || warn("unable to open $TEMPF -> $!\n");
 while(<FH>) {
	if(/^Subject: (.*)/) {
		$ks = $1;
		print("Subject : $ks ! \n");
		next;
		}
	if(/^From: (.*)/) {
		print("From : $1\n"); 
		next;
		}
	if(/^Time: (.*)/) {
		print("Time Posted : $1\n");
		next;
		}
	else {
		print(TH "$_");
		}
	}
 &eprom;
 close(FH); close(TH);
 print("\n");
 &rsp($TEMPF);
 unlink($TEMPF);
 print("$maction");
 chomp($enval = <>);
 if($enval =~ /r/i) {
	&reply($DIR, $MID, $ks);
	} elsif($enval =~ /d/i) {
		&delm($DIR, $MID);
	} elsif($enval =~ /c/i) {
		&get_messages($LD2);
		}
}
		

sub reply {
 $DIR = shift(@_);
 $MID = shift(@_);
 $ks = shift(@_);
 $cnt = $#mess;
 $cnt++;
 $MYF = $DIR . "/" . $cnt;
 $REPM = $DIR . "/" . $MID;
 open(TEMPF, "> $TEMPF") || warn("Unable to open $TEMPF -> $!\n");
 open(REPM) || warn("Unable to open $REPM -> $!\n");
 while(<REPM>) {
	if(/^Subject: .*/ || /^From: .*/ || /^Time: .*/) {
		next;
		} else {
			print(TEMPF "\> $_");
			}
	}
 if($ks !~ /^re -.*/) {
	$ks = "re - $ks";
	}
 close(REPM);
 close(TEMPF);
 system("$ed $TEMPF");
 system("/bin/mv $TEMPF \"$MYF\"");
 open(MYF, ">> $MYF");
 $mon++;
 print(MYF "Subject: $ks\n");
 print(MYF "From: $id\n");
 print(MYF "Time: ${mon}/${mday}/${year}  ${hour}:${min}\n");
 close(MYF);
}


sub delm {
 $DIR = shift(@_);
 $MID = shift(@_);
 &clear;
 $file = $DIR . "/" . $MID;
 if(! -w $file) {
	print("Sorry, You Don't Have Access To Delete This Message!\n");
	&eprom;
	} elsif(-w $file) {
		unlink($file);
		print("Message Deleted\n");
		&eprom;
		} 
}

sub post {
 $DIR = shift(@_);
 $cnt = shift(@_);
 $cnt++;
 $MF = $DIR . "/" . $cnt;
 $TF = "/tmp/.$cnt.$$";
 
 print("Subject: ");
 chomp($sub = <>);
 
 system("$ed $TF");
 open(TF, ">> $TF");
 print(TF "Subject: $sub\nFrom: $id\nTime: ${mon}/${mday}/${year} ${hour}:${min}\n");
 close(TF);
 system("/bin/mv $TF \"$MF\"");
}

&get_bases;


