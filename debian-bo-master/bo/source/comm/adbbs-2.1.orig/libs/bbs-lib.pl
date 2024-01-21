;# bbs-lib.pl (0.60)
;# aD! BBS 2.1 (c) Chris Church / aD! Data Systems
;#---------------------------------------------------

require 'pwd.pl';
require 'ansi.ph';
require 'fbase.ph';
require 'fsearch.ph';
&initpwd;
if($u_singk == 1) { use Term::ReadKey; }


$maxflen = 24;
$SIG{'HUP'} = clean;
$SIG{'KILL'} = clean;
$SIG{'STOP'} = 'ignore';
$SIG{'INT'} = clean;
$SIG{'ALRM'} = kbtimeout;

sub clean {
 print("Bye Now!\n");
 exit;
}

sub kbtimeout {

 &clear;
 print("Input Timeout ($kbsec seconds) - aD!BBS Exiting\n");
 ReadMode 0 if($u_singk == 1);
 &clean;
}


sub clear {
 
 system("$tppath", 'clear'); 

}

sub checkcolor {
 $fb = shift(@_);
 $color = shift(@_);
 if($ENV{'TERM'} eq 'vt52' || $ENV{'TERM'} eq 'sun') {
	return; 
 	} else {
		if($fb =~ /fore/i) {
			&setforecolor($color);
				} else {
					&setbackcolor($color);
					}
		}
}
					
sub write_rc {
 $DF = $home . "/" . ".aDrc";
 open(DF, ">> $DF") || warn("Unable to open $DF -> $!\n");
 print DF @_;
 close(DF);
}
 
sub user_defs {
 $DF = $home . "/" . ".aDrc";
 return if(! -r $DF);
 open(DF) || die("Unable to open $DF -> $!\n");
 while(<DF>) {
	if(/^#/) {
		next; 
		}
	elsif(/\s*TERM\s+(.*)/) {
		$tset = 1;
		$term = $1;
		&set_term;
		}
	elsif(/\s*CPROM\s+(.*)/) {
		$cprom = $1;
		next;
		}
	elsif(/\s*EPROM\s+(.*)/) {
		$eprom = $1;
		next;
		}
	}
}

sub get_ttype {
 return if($tset == 1);
 &clear; 
 print("Enter The Number Which Corresponds To Your Terminal Type\n");
 print("1\) vt100 \(ANSI Compliant\)\n");
 print("2\) vt102 \(ANSI Compliant\)\n");
 print("3\) vt52 \(Default NON-ANSI term\)\n");
 print("4\) console \(ANSI Compliant\)\n");
 print("5\) sun\n");
 &ckc;
 if($enval eq '1') {
	 $term = 'vt100'; 
	 } elsif($enval eq '2') { 
 		$term = 'vt102';
 	 } elsif($enval eq '3') {
		 $term = 'vt52';
	 } elsif($enval eq '4') {
		 $term = 'console';
	 } else {
		 $term = 'sun';
		}
 &write_rc("TERM	$term\n");
 &set_term;	
}

sub set_term {
 $ENV{'TERM'} = $term;
}

  	

sub eprom {
 my(@blarr) = split(//, $eprom);
 foreach $p (0 .. $#blarr) {
	if($kun == 1) {
		$kun = 0;
		if($blarr[$p] !~ /\d/) {
			print("\%$blarr[$p]");
			next;
			}
		&checkcolor('fore', $blarr[$p]);
		next;
		}
	if($blarr[$p] eq '%') {
		$kun = 1;
		next;
		} else {
			print($blarr[$p]);
			}			
	}
 &checkcolor('fore', 7);

##############################
# getc stuff suggested / added
# by uncle Bob
##############################

 alarm($kbsec) if($u_ktime == 1);
 chomp($ent = <>) if($u_singk != 1);
 if($u_singk == 1) {
	ReadMode(4);
	$ent = getc(STDIN);
	ReadMode(0);
	}
 alarm(0) if($u_ktime == 1);

}

 
sub ckc {
 $kun = 0 if(!defined($kun));
 my(@blarr) = split(//, $cprom);
 foreach $p (0 .. $#blarr) {
	if($kun == 1) {
		$kun = 0;
		if($blarr[$p] !~ /\d/) {
			print("\%$blarr[$p]");
			next;
			}
		&checkcolor('fore', $blarr[$p]);
		next;
		}
	if($blarr[$p] eq '%') {
		$kun = 1;
		next;
		} else {
			print($blarr[$p]);
			}			
	}
 &checkcolor('fore', 7);
 alarm($kbsec) if($u_ktime == 1);
 chomp($enval = <>) if($u_singk != 1);
 if($u_singk == 1) {
	ReadMode(4);
	$enval = getc(STDIN);
	ReadMode(0);
	}
 alarm(0) if($u_ktime == 1);

}

sub conf_prompt {
 &clear;
 print("[Prompt Configuration]\n");
 print("Colours should be prefixed by a \'\%\' sign.\n");
 print("The Following Colours Are Valid:\n");
 print("-> ");
 foreach $f (0 .. 9) {
	&checkcolor('fore', $f);
	print("$f ");
	}
 &checkcolor('fore', 7);
 print("<-\n");
 print("Which prompt to change ? [E]nter or [C]ommand? -> ");
 chomp($enval = <>);
 if($enval =~ /e/i) {
	print("-> ");
	chomp($eprom = <>);
	&write_rc("EPROM	$eprom\n");
	} else {
		print("-> ");
		chomp($cprom =<>);
		&write_rc("CPROM	$cprom\n");
		}
}
	

sub int_envvars {

 $maxflen = 24;
 $fhost = "$ENV{'HOSTNAME'}"; 
 if ($fhost =~ /^(\w*).(\S*)/) {
	$shost = $1;      ;# make sure the hostname is made of words, 
	$hostend = "$2"; ;# and then chop it up
	 }

 $MAILF = "$ENV{'MAIL'}";
 $home = "$ENV{'HOME'}";
 $id = getlogin || (getpwuid($<))[0] || "NOUNAME"

# note - older version of aD!BBS used the environment variable LOGNAME,
# which could be set by a malicious user via an external prog.
# here, we set it using getlogin, getpwuid, and all else failing,
# a default value.  This currently really has no effect on
# anything, and was merely an annoyance, not a security bug
# but the effects of which will be seen in future versions

}
 
sub int_intcalls {

 
 	if ($progs[$i] =~ /goodbye\b/i) {
	&clear;
	open(GBFILE);
	while (<GBFILE>) {
		print;
		}
	close(GBFILE);
	&rtpi("$MDIR/goodbye.pi") if(-r "$MDIR/goodbye.pi");
	exit;
		} elsif ($progs[$i] =~ /^ed\s*(.*)/) {
			&rsp($1);
		} elsif ($progs[$i] =~ /uinfo\b/i) {
			&clear;
			&uinfo;
		} elsif ($progs[$i] =~ /cprom\b/i) {
			&conf_prompt;
		} elsif ($progs[$i] =~ /ttype\b/i) {
			&get_ttype;
		} elsif ($progs[$i] =~ /fbase\b/i) {
			&fmain;
		} elsif ($progs[$i] =~ /rtpi\s*(.*)/) {
			&rtpi($1);
		} else { 
##########################
# POSIX security code 
# removed : too bulky
##########################
		ReadMode(0) if($u_singk); # in case of something funky
      	 	&clear;                   # we set the terminal clean
		system("$progs[$i]");
		&clear;       # in case the program dies!
		&eprom;
	        }
}	

sub rtpi {
 $RTPF = shift(@_);
 open(RTPF) || warn("rpti loader: Unable to open $RTPF -> $!\n");
 while(<RTPF>) {
	$string = $string . $_;
	}
 eval "$string";
 undef($string);
}
 	

sub check_messages {
 unless(open(MAILF)) { print("No Mail / Mail Folder\n"); return; } 
 while(<MAILF>) {
	$c++ if (/^Return-Path.*/);
	 }
 close(MAILF);
 print("You Have $c Messages In Your Inbox\n");

}
 
sub rsp {

 $EDF = shift(@_);
 open(EDF) || die("Can't Open $EDF -> $!");
 $g = 0;
 while(<EDF>) {
	if ($g <= $maxflen) {
        	print;
		$g++;
		next;
		} else {
			&eprom;
			print("\n");
			print;
 			$g = 1;
		}
	}
 close(EDF);
 &eprom;
}			 

1;