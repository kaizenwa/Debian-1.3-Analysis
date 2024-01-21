#!/usr/local/bin/perl
#
# updatedb.pl (c) 1996 Chris Church / aD! Data Systems
#
#
# usage : updatedb.pl
#-----------------------------------


require '/usr/local/etc/adbbs.cf';

require 'bbs-lib.pl';

if(-w $lib_file) {
	unlink($lib_file);
	}

&update_db;
&crunch_readmes;
$time = `date`;
print("$lib_file deleted and updated on $time\n");
