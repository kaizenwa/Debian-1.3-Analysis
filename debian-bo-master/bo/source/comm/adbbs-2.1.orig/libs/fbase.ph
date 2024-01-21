# fbase.ph 0.36 (c) 1995, 1996 aD! Data Systems / Chris Church
# psylark@aD.org
#--------------------------------------------------------------

$TFILE = "/tmp/.file.$$";
$tdinr = $FDIR;

sub fetch_dirs {
  &clear;
  $page = 0;
  opendir(CURDIR, $FDIR) || die("Unable to open dir $MDIR -> $!\n");
  @files = grep(!/^\./, readdir(CURDIR));# || die("readdir\(${CURDIR}\) -> $!\n");
  $page = 1 if($#files >= 24); 
  foreach $file (@files) {
	$CF = $FDIR . "/" . $file;
	($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
		$mtime,$ctime,$blksize,$blocks) 
			= stat($CF);
	if($page == 1) {
		open(TFILE, ">> $TFILE") || die("Unable to open $TFILE -> $!\n");
		print TFILE "${file}/\n" if(-d _);
		print TFILE "${file}\n" if(! -d _);
		} else {
			print("${file}/\n") if(-d _);
			print("${file}\n") if(! -d _);
			}
	}
  if($page == 1) {
	&rsp($TFILE); unlink($TFILE);
	} else { ; }
}

sub fhelp {
 &clear;
 &checkcolor('fore', 5);
 print("aD!BBS FileBaseUtil (v) 0.36 (c) 1995, 1996 aD! Data Systems\n");
 &checkcolor('fore', 9);
 print("Usage:\n");
 print("? / help        : show this screen\n");
 print("s / show        : show info about a file\n");
 print("d / down <file> : download <file>\n");
 print("p / put <file>  : put a file\n");
 print("cd <dir>        : change directories to <dir>\n");
 print("cdup            : go up one dir\n");
 print("view <file>     : view a text file\n");
 print("q / quit        : exit file utility\n");
 print("ls              : show files in current dir\n");
 print("find <string>   : search through readme's and 
		: index's for <string>.  Can be
		: any regexp, except those using
		: ^ <begin> & <end> or / <perl delim>\n\n");
 &checkcolor('fore', 7);
}

sub showf {
 $blah = shift(@_);
 $CF = $FDIR . "/" . $blah;
 ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
	$mtime,$ctime,$blksize,$blocks) 
		= stat($CF);
 ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
		localtime($ctime);
 &clear;
 print("Status of $blah :\n");
 print("Size: $size\n");
 $mon++; 
 print("Created/Uploaded: ${hour}:${min} ${mon}/${mday}/${year}\n");
 print("Text File\n") if(-T _);
 print("Binary file\n") if(! -T _);
}

sub check_enter {
 while(1) {
	 print("File >> ");
	 chomp($fent = <>);
	 if($fent =~ /\?/i || $fent =~ /help/i) {
		&fhelp;
		} elsif($fent =~ /^s\s+(.*)/i || $fent =~ /show\s+(.*)/i) {
			&showf($1);	
		} elsif($fent =~ /^d\s+(.*)/i || $fent =~ /down\s+(.*)/i) {
			&dl($1);
		} elsif($fent =~ /^p\s+(.*)/i || $fent =~ /put\s+(.*)/i) {
			&put($1);
		} elsif($fent =~ /cd\s+(.*)/i && $fent !~ /cd\s+\.\.*/i && $fent !~ /cd\s+\/\..*/i) {
			$FDIR = $FDIR . "/" . $1;
			&fetch_dirs;
		} elsif($fent =~ /cdup/i || $fent =~ /cd\s+\.\..*/i) {
			if($FDIR =~ /.*${tdinr}$/) {
				print("You are at the highest directory\n");
				next;
				}
			@funk = split(/\//, $FDIR);
			undef($FDIR);
			pop(@funk);
			foreach $fur (@funk) {
				$FDIR = $FDIR . "/" . $fur;
				}
			&fetch_dirs; 
			undef(@funk);
		} elsif($fent =~ /view\s+(.*)/i) {
			$plo = $FDIR . "/" . $1;
			if(! -T $plo) {
				print("This file is not a text file\n");
				undef($plo);
				next;
				}				
			&rsp($plo);
			undef($plo);
		} elsif($fent =~ /^q/i) {
			last;
		} elsif($fent =~ /ls/i) {
			&fetch_dirs;
		} elsif($fent =~ /find\s+(.*)/i) {
			my($bnknis) = $1;
			&dbsrch($bnknis);
		}  

	}
}

sub dl {
 $file = shift(@_);
 &clear;
 print("Downloading ${file}:\n");
 print("1) zmodem\n");
 print("2) xmodem\n");
 print("3) abort\n");
 &ckc;
 if($enval =~ /1\b/) {
	print("Sending $file via zmodem\n");
	$file = $FDIR . "/" . $file;
	system("$sz  $file");
	} elsif($enval =~ /2\b/) {
		print("Sending $file via xmodem\n");
		$file = $FDIR . "/" . $file;
		system("$sx $file");
	} else { return ; }
}

sub put {
 $file = shift(@_);
 &clear;
 print("Uploading ${file}:\n");
 print("1) zmodem\n");
 print("2) xmodem\n");
 print("3) abort\n");
 &ckc;
 if($enval =~ /1\b/) {
	print("Recieving $file via zmodem\n");
	$file = $IDIR . "/" . $file;
	system("$rz  $file");
	} elsif($enval =~ /2\b/) {
		print("Recieving $file via xmodem\n");
		$file = $IDIR . "/" . $file;
		system("$rx $file");
	} else { return ; }
}

sub fmain {
 &clear;
 &checkcolor('fore', 5);
 print("aD!BBS FileBaseUtil (v) 0.3 (c) 1995, 1996 aD! Data Systems\n");
 &checkcolor('fore', 7); 
 print("This area is only of use to those on a dial-up line, or telnetting\n");
 print("in with a client that accepts binary transfers\n");
 print("type");
 &checkcolor('fore', 9); 
 print(" 'help' ");
 &checkcolor('fore', 7);
 print("for help\n");
 &check_enter;
}

1;