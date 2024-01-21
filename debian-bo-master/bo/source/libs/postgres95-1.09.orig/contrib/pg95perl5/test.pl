#-------------------------------------------------------
#
# $Id: test.pl,v 1.1.1.1 1996/10/23 07:19:27 scrappy Exp $
#
#-------------------------------------------------------

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..61\n"; }
END {print "not ok 1\n" unless $loaded;}
use Pg;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

$pghost    = 'localhost';
$pgport    = 5432;
$pgoptions = '';
$pgtty     = '';
$dbmain    = 'template1';
$dbname    = 'pgperltest';
$trace     = '/tmp/pgtrace.out';

$DEBUG = 0; # set this to 1 for traces
$cnt   = 2;

######################### the following functions will not be tested here

#	PQreset()
#	PQdb()
#	PQhost()
#	PQoptions()
#	PQport()
#	PQtty()
#	PQfname()
#	PQfnumber()
#	PQftype()
#	PQfsize()
#	PQgetlength()
#	PQgetisnull()
#	PQlo_lseek()
#	PQlo_tell()
#	PQlo_import()
#	PQlo_export()

######################### create and connect to test database

$conn = PQsetdb($pghost, $pgport, $pgoptions, $pgtty, $dbmain);
cmp_eq(PGRES_CONNECTION_OK, PQstatus($conn));

PQexec($conn, "drop database $dbname");
$res = exe(PGRES_COMMAND_OK, "create database $dbname");

PQfinish($conn);

$conn = PQsetdb($pghost, $pgport, $pgoptions, $pgtty, $dbname);
cmp_eq(PGRES_CONNECTION_OK, PQstatus($conn));

######################### debug, PQtrace

if ($DEBUG) {
    open(TRACE, ">$trace") || die "can not open $trace: $!";
    PQtrace($conn, TRACE);
}

######################### create and insert into table

$res = exe(PGRES_COMMAND_OK, "create table person (name char16, age int4, location point)");
cmp_eq("CREATE", PQcmdStatus($res));

for ($i=50; $i <= 90; $i = $i + 10) {
   $res = exe(PGRES_COMMAND_OK, "insert into person values (\'fred\', $i, \'($i,10)\'::point)");
   cmp_ne(0, PQoidStatus($res));
}

######################### copy to stdout, PQgetline

exe(PGRES_COPY_OUT, "copy person to stdout");

$i = 50;
while (-1 != $ret) {
    $ret = PQgetline($conn, $string, 256);
    last if $string eq ".";
    cmp_eq("fred	$i	($i,10)" ,$string);
    $i += 10;
}

cmp_eq(0, PQendcopy($conn));

######################### delete and copy from stdin, PQputline

exe(PGRES_COMMAND_OK, "begin");

$res = exe(PGRES_COMMAND_OK, "delete from person");
cmp_eq("DELETE", PQcmdStatus($res));

exe(PGRES_COPY_IN, "copy person from stdin");

for ($i=50; $i <= 90; $i = $i + 10) {
    # watch the tabs and do not forget the newlines
    PQputline($conn, "fred	$i	($i,10)\n");
}
PQputline($conn, ".\n");

cmp_eq(0, PQendcopy($conn));

exe(PGRES_COMMAND_OK, "end");

######################### select from person, PQgetvalue

exe(PGRES_COMMAND_OK, "begin");

$res = exe(PGRES_COMMAND_OK, "declare eportal cursor for select * from person");
cmp_eq("SELECT", PQcmdStatus($res));

$result = PQexec($conn, "fetch all in eportal");
cmp_eq(PGRES_TUPLES_OK, PQresultStatus($result));

$nfields = PQnfields($result);
$ntuples = PQntuples($result);
$i = 50;
for ($k=0; $k < $ntuples; $k++) {
    $string = "";
    for ($l=0; $l < $nfields; $l++) {
        $string .= PQgetvalue($result, $k, $l) . " ";
    }
    cmp_eq("fred $i ($i,10) ", $string);
    $i += 10;
}
PQclear($result);

$res = exe(PGRES_COMMAND_OK, "close eportal");
cmp_eq("CLOSE", PQcmdStatus($res));

exe(PGRES_COMMAND_OK, "end");

######################### PQnotifies

$res = exe(PGRES_COMMAND_OK, "listen person");
cmp_eq("LISTEN", PQcmdStatus($res));

if (!defined($pid = fork)) {
    die "can not fork: $!";
} elsif (! $pid) {
    # i'm the child
    sleep 2;
    $conn = PQsetdb($pghost, $pgport, $pgoptions, $pgtty, $dbname);
    $result = PQexec($conn, "notify person");
    PQclear($result);
    PQfinish($conn);
    exit;
}

while (1) {
    $result = PQexec($conn, " ");
    ($table, $pid) = PQnotifies($conn);
    PQclear($result);
    last if $pid;
}

cmp_eq("person", $table);

######################### PQprint

$result = PQexec($conn, "select location from person where age = 70");
cmp_eq(PGRES_TUPLES_OK, PQresultStatus($result));

open(PRINT, "| read IN; read IN; if [ \"\$IN\" = \"myLocation (70,10)\" ]; then echo \"ok $cnt\"; else echo \"not ok $cnt: PQprint\"; fi ") || die "can not fork: $|";
$SIG{PIPE} = sub { die "broken pipe" };
$cnt ++;

PQprint(PRINT, $result, 0, 0, 0, 0, 1, 0, " ", "", "", "myLocation");
PQclear($result);
close(PRINT) || die "bad PRINT: $!";

######################### PQlo_creat, PQlo_open, PQlo_write

$cwd = `pwd`;
chop $cwd;
$filename = "$cwd/Changes";
open(LO, $filename) || die "can not open $filename: $!";

exe(PGRES_COMMAND_OK, "begin");

$lobjId = PQlo_creat($conn, PGRES_INV_READ|PGRES_INV_WRITE);
cmp_ne(0, $lobjId);

$lobj_fd = PQlo_open($conn, $lobjId, PGRES_INV_WRITE);
cmp_ne(-1, $lobj_fd);

$i = 0;
while (($nbytes = read(LO, $buf, 1024)) > 0) {
    $cmp_ary[$i] = $buf;
    cmp_eq($nbytes, PQlo_write($conn, $lobj_fd, $buf, $nbytes));
    $i++;
}

close(LO)|| die "bad LO: $!";
cmp_eq(0, PQlo_close($conn, $lobj_fd));

exe(PGRES_COMMAND_OK, "end");

######################### PQlo_read, PQlo_unlink

exe(PGRES_COMMAND_OK, "begin");

$lobj_fd = PQlo_open($conn, $lobjId, PGRES_INV_READ);
cmp_ne(-1, $lobj_fd);

$i = 0;
while (($nbytes = PQlo_read($conn, $lobj_fd, $buf, 1024)) > 0) {
    cmp_eq($cmp_ary[$i], $buf);
    $i++;
}

cmp_eq(0, PQlo_close($conn, $lobj_fd));

cmp_ne(-1, PQlo_unlink($conn, $lobjId));

exe(PGRES_COMMAND_OK, "end");

######################### debug, PQuntrace

if ($DEBUG) {
    close(TRACE) || die "bad TRACE: $!";
    PQuntrace($conn);
}

######################### disconnect and drop test database

PQfinish($conn);

$conn = PQsetdb($pghost, $pgport, $pgoptions, $pgtty, $dbmain);
cmp_eq(PGRES_CONNECTION_OK, PQstatus($conn));

$res = exe(PGRES_COMMAND_OK, "drop database $dbname");
cmp_eq("DELETE", PQcmdStatus($res));

PQfinish($conn);

######################### hopefully

print "all tests passed.\n" if 62 == $cnt;

######################### utility functions

sub cmp_eq {

    my $cmp = shift;
    my $ret = shift;

    if ("$cmp" eq "$ret") {
	print "ok $cnt\n";
    } else {
	die "not ok $cnt: $cmp, $ret\n";
    }
    $cnt++;
}

sub cmp_ne {

    my $cmp = shift;
    my $ret = shift;

    if ("$cmp" ne "$ret") {
	print "ok $cnt\n";
    } else {
	die "not ok $cnt: $cmp, $ret\n";
    }
    $cnt++;
}

sub exe {

    my $cmp = shift;
    my $cmd = shift;

    my $result;
    my $status;

    $result = PQexec($conn, $cmd);
    $status = PQresultStatus($result);

    if ("$cmp" eq "$status") {
	print "ok $cnt\n";
    } else {
	die "not ok $cnt: \n$cmd\n" . PQerrorMessage($conn);
    }
    $cnt++;
    return $result;
}

######################### EOF
