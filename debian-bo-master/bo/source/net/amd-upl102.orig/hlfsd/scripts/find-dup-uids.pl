#!/usr/local/gnu/bin/perl
# check to see if the password maps contain users with same UID.

$passwdfile = "/yp/passwd";	# change to /etc/passwd or the like

$namebyuid = 0;
$ret = 0;

open(passwd, $passwdfile) || die "cannot open $passwdfile: $!";
while(<passwd>) {
    chop;
    ($name,$passwd,$uid,$gid,$gcos,$dir,$shell) = split(/:/, $_);
    next if ($uid == 0);
    if (!($name2 = $namebyuid{$uid})) {
	$namebyuid{$uid} = $name;
    } elsif ($name2 ne $name) {
	printf(STDOUT "Duplicate uids: users \"%s\" and \"%s\" have uid %d.\n",
	       $name, $name2, $uid);
	$ret++;
    }
}
close(passwd);
exit($ret);
