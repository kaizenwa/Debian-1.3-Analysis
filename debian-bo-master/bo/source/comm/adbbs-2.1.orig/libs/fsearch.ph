#!/usr/local/bin/perl
#
# fsearch.ph (c) 1996 Chris Church / aD! Data Systems
# 		psylark@aD.org
# Version 1.15
#
# -------------------------------------------




$ftp_dir = $FDIR;

sub get_files {
	open(OFILE, ">> $lib_file");
	opendir(CURDIR, $ftp_dir);
	@files = grep(!/^\./, readdir(CURDIR));
        foreach $i (0 .. $#files) {
		$curfile = "$ftp_dir/$files[$i]";
		($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$sine,
			$atime,$mtime,$ctime,$blksize,$blocks)
				= stat($curfile);	
		print OFILE "$files[$i]\n";
		push (@dirs, $files[$i]) if -d _;		
	}
	closedir(CURDIR);
	close(OFILE);
}

sub go_new_dir {
	open(OFILE, ">> $lib_file");
	foreach $i (0 .. $#dirs) {
		$cnf = $dirs[$i];
		opendir(CHDIR, "$ftp_dir/$cnf");
		@openfiles = grep(!/^\./, readdir(CHDIR));
		foreach $f (0 .. $#openfiles) {
		$curfile = "$ftp_dir/$cnf/$openfiles[$f]";
		($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$sine,
		$atime,$mtime,$ctime,$blksize,$blocks)
				= stat($curfile);
		print OFILE "$cnf/$openfiles[$f]\n";
		push (@newdirs, "$cnf/$openfiles[$f]") if -d _;
		}
	}
	closedir(CHDIR);
	close(OFILE);
}
		

sub crunch_readmes {
 open(LF, "$lib_file") || die("Unable to open $lib_file -> $!\n");
 open(CF, "> $CF") || die("Unable to open $cr_file -> $!\n");
 while(<LF>) {
	if(/(.*\.README)/i || /(.*INDEX)/i) {
		$BF = $1;
		$BF = $ftp_dir . "/" . $BF;
		open(BF) || warn("Unable to open $BF -> $!\n");
		my($cnt) = 0;
		while(<BF>) {
			$cnt++;
			$_ =~ s/ //g;
			print(CF "\000$BF:$cnt\000$_");
			}
		
		}
	}
}

# this is the sub-routine needed to update the databases...
# just &update_db...
# =)
sub update_db {
 &get_files;
 while (@dirs) {
 	&go_new_dir;
 	@dirs = @newdirs;
 	undef @newdirs;
	}
}

# this is the sub-routine to do quick searches, any regexp should
# work, except those starting or ending with [/^$] look at the
# sub below to see why..
# call : dbsrch("string")

sub dbsrch {

 my($srchstr) = shift(@_);
 open(CF) || warn("Unable to open $CF -> $!\n");
 while(<CF>) {
	my(@lar) = split(/\000/);
	my($file, $line) = split(/:/, $lar[1]);
	if($lar[2] =~ /.*$srchstr.*/) {
		if($file =~ /$FDIR\/(.*)/) {
			$file = $1;
			}
		print("File: $file, Line: $line\n");
		}
	}
}



1;