#!/usr/bin/perl -w
# $Id: debian.extract-changes.pl,v 1.1 1996/06/20 05:39:06 torin Exp $
#
# yank the changes out of the changelog for the current version
# based on a script found in libdb, probably by Ray Dassen 
#   but there were no comments in the file

my($pv, $in);

$pv = join(' ', @ARGV);
$in = 0;

open(CL, 'debian.Changelog') or die("Cannot open debian.Changelog: $!");

while(<CL>)
{
    if ($in && /^\S+/) { $in = 0; } 	# don't print if we're starting a new chunk of comments
    print if $in && !/^\s*$/;
    if (/^$pv/i) { $in = 1; }		# start printing if this is the right version
}

close(CL);

# end of debian.extract-changes.pl
