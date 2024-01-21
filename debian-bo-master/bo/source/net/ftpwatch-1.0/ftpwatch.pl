#!/usr/bin/perl
# ftpwatch - Simple script to notify you of changes on ftp servers
# Copyright (C) 1997 Hakan Ardo <hakan@debian.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

$Tmp="/tmp/ftpwatch.$$";
$EMail="hakan\@munin.ub2.lu.se";
$ConfDir="/home/hakan/.ftpwatch";

open (PAS, "</etc/passwd");
while (<PAS>) {
    ($EMail, $p, $uid, $gid, $name, $dir, $sh)=split (/:/, $_);
    $ConfDir="$dir/.ftpwatch";

    if ((-f "$dir/.ftpwatchrc") && !(-d $ConfDir)) {system("mkdir $ConfDir");}
    open (C, "<$dir/.ftpwatchrc");    
    while (<C>) {
	if (/^([^:]+):(.+)$/) {
	    $sv=$1; $dir=$2;
	    open (F, "|/usr/bin/ncftp > $Tmp");
	    print F ("open $sv\ndir $dir\nbye\n");
	    close (F);
	    open (F, "<$Tmp"); open (T, ">$Tmp.2");
	    while (<F>) {
		if (/^[\-d]/ && !/\s+\.\.?\s*$/) {
		    s///g; 
		    print T $_;
		}
	    }
	    close (F); close(T);
	    
	    $dir =~ s/\//_/g;
	    system ("diff $ConfDir/$sv$dir $Tmp.2 > $Tmp.3");
	    system ("cp $Tmp.2 $ConfDir/$sv$dir");

	    ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,
	     $mtime,$ctime,$blksize,$blocks) = stat("$Tmp.3");
	    if ($size > 0) {
		system ("mail -s \"Ftp Update: $sv$dir\" $EMail< $Tmp.3");
	    }
	}
	system("rm $Tmp*");
    }
}


