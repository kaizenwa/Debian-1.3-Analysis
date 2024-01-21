#-------------------------------------------------------
#
# $Id: Pg.pm,v 1.1.1.1 1996/10/23 07:19:26 scrappy Exp $
#
#-------------------------------------------------------

package Pg;

use strict;
# no strict "subs";
use Carp;
use vars qw($VERSION @ISA @EXPORT $AUTOLOAD);

require Exporter;
require DynaLoader;
require AutoLoader;

@ISA = qw(Exporter DynaLoader);

# Items to export into callers namespace by default.
@EXPORT = qw(
	PQsetdb
	PQfinish
	PQreset
	PQdb
	PQhost
	PQoptions
	PQport
	PQtty
	PQstatus
	PQerrorMessage
	PQtrace
	PQuntrace
	PQexec
	PQgetline
	PQendcopy
	PQputline
	PQresultStatus
	PQntuples
	PQnfields
	PQfname
	PQfnumber
	PQftype
	PQfsize
	PQcmdStatus
	PQoidStatus
	PQgetvalue
	PQgetlength
	PQgetisnull
	PQclear
	PQprintTuples
	PQprint
	PQnotifies
	PQlo_open
	PQlo_close
	PQlo_read
	PQlo_write
	PQlo_lseek
	PQlo_creat
	PQlo_tell
	PQlo_unlink
	PQlo_import
	PQlo_export
	PGRES_CONNECTION_OK
	PGRES_CONNECTION_BAD
	PGRES_EMPTY_QUERY
	PGRES_COMMAND_OK
	PGRES_TUPLES_OK
	PGRES_COPY_OUT
	PGRES_COPY_IN
	PGRES_BAD_RESPONSE
	PGRES_NONFATAL_ERROR
	PGRES_FATAL_ERROR
	PGRES_INV_SMGRMASK
	PGRES_INV_ARCHIVE
	PGRES_INV_WRITE
	PGRES_INV_READ
	PGRES_InvalidOid
);

$VERSION = '1.3';

sub AUTOLOAD {
    # This AUTOLOAD is used to 'autoload' constants from the constant()
    # XS function.  If a constant is not found then control is passed
    # to the AUTOLOAD in AutoLoader.

    my $constname;
    ($constname = $AUTOLOAD) =~ s/.*:://;
    my $val = constant($constname, @_ ? $_[0] : 0);
    if ($! != 0) {
	if ($! =~ /Invalid/) {
	    $AutoLoader::AUTOLOAD = $AUTOLOAD;
	    goto &AutoLoader::AUTOLOAD;
	}
	else {
		croak "Your vendor has not defined Pg macro $constname";
	}
    }
    eval "sub $AUTOLOAD { $val }";
    goto &$AUTOLOAD;
}

bootstrap Pg $VERSION;

# Preloaded methods go here.

# Autoload methods go after =cut, and are processed by the autosplit program.

sub doQuery {

    my $conn      = shift;
    my $query     = shift;
    my $array_ref = shift;

    my ($cmd, $result, $status, $nfields, $ntuples, $i, $j);

    $result = PQexec($conn, "begin");
    $status = PQresultStatus($result);
    PQclear($result);
    return($status) if (1 != $status);

    $result = PQexec($conn, "declare eportal cursor for $query");
    $status = PQresultStatus($result);
    PQclear($result);
    return($status) if (1 != $status);

    $result = PQexec($conn, "fetch all in eportal");
    $status = PQresultStatus($result);
    return($status) if (2 != $status);
    $nfields = PQnfields($result);
    $ntuples = PQntuples($result);
    for ($i=0; $i < $ntuples; $i++) {
        for ($j=0; $j < $nfields; $j++) {
            $$array_ref[$i][$j] = PQgetvalue($result, $i, $j);
        }
    }
    PQclear($result);

    $result = PQexec($conn, "close eportal");
    $status = PQresultStatus($result);
    PQclear($result);
    return($status) if (1 != $status);

    $result = PQexec($conn, "end");
    $status = PQresultStatus($result);
    PQclear($result);
    return($status) if (1 != $status);

    return 0;
}

1;
__END__


=head1 NAME

Pg - Perl extension for Postgres95


=head1 SYNOPSIS

use Pg;


=head1 DESCRIPTION

The Pg module permits you to access all functions of the 
libpq-interface of Postgres95. It tries to resemble the 
libpq-interface as close as possible. For examples of how 
to use this module, look at the file test.pl. 


=head1 GUIDELINES

There are two exceptions, where the perl-functions differs 
from the C-counterpart: PQprint and PQnotifies. These 
functions deal with structures, which have been implemented 
in perl using lists. 

All functions and constants are imported into the calling 
packages namespace. In order to to get a uniform naming, 
all functions start with 'PQ' (e.g. PQlo_open) and all 
constants start with 'PGRES_' (e.g. PGRES_CONNECTION_OK). 

There are two functions, which allocate memory, that has to be 
freed by the user: 

	PQsetdb, use PQfinish to free memory.
	PQexec,  use PQclear to free memory.

Pg.pm contains one convenience function: doQuery. It fills a
two-dimensional array with the result of your query. Usage:

Pg::doQuery($conn, "select attr1, attr2 from tbl", \@ary);

for $i ( 0 .. $#ary ) {
    for $j ( 0 .. $#{$ary[$i]} ) {
        print "$ary[$i][$j]\t";
    }
    print "\n";
}

Notice the inner loop !


=head1 FUNCTIONS

The functions have been divided into four categories:
Connections, Execution, Miscellaneous, Large Objects.


=head2 1. Connection

With these functions you can establish and close a connection to a 
database. In libpq a connection is represented by a structure called
PGconn. Using the appropriate functions you can access almost all 
fields of this structure.

B<$conn = PQsetdb($pghost, $pgport, $pgoptions, $pgtty, $dbname)>

Opens a new connection to the backend. You may use an empty string for
any argument, in which case first the environment is checked and then 
hardcoded defaults are used. The connection identifier $conn ( a pointer 
to the PGconn structure ) must be used in subsequent commands for unique 
identification. Before using $conn you should call PQstatus to ensure, 
that the connection was properly made. Use the functions below to access 
the contents of the PGconn structure.

B<$pghost = PQhost($conn)>

Returns the host name of the connection.

B<$pgtty = PQtty($conn)>

Returns the tty of the connection.

B<$pgport = PQport($conn)>

Returns the port of the connection.

B<$pgoptions = PQoptions($conn)>

Returns the options used in the connection.

B<$dbName = PQdb($conn)>

Returns the database name of the connection.

B<$status = PQstatus($conn)>

Returns the status of the connection. For comparing the status 
you may use the following constants: 
 - PGRES_CONNECTION_OK
 - PGRES_CONNECTION_BAD

B<$errorMessage = PQerrorMessage($conn)>

Returns the last error message associated with this connection.

B<PQfinish($conn)>

Closes the connection to the backend and frees all memory. 

B<PQreset($conn)>

Resets the communication port with the backend and tries
to establish a new connection.


=head2 2. Execution

With these functions you can send commands to a database and
investigate the results. In libpq the result of a command is 
represented by a structure called PGresult. Using the appropriate 
functions you can access almost all fields of this structure.

B<$result = PQexec($conn, $query)>

Submits a query to the backend. The return value is a pointer to 
the PGresult structure, which contains the complete query-result 
returned by the backend. In case of failure, the pointer points 
to an empty structure. In this, the perl implementation differs 
from the C-implementation. In perl, even the empty structure has 
to be freed using PQfree. Before using $result you should call 
PQresultStatus to ensure, that the query was properly executed. 

Use the functions below to access the contents of the PGresult structure.

B<$ntups = PQntuples($result)>

Returns the number of tuples in the query result.

B<$nfields = PQnfields($result)>

Returns the number of fields in the query result.

B<$fname = PQfname($result, $field_num)>

Returns the field name associated with the given field number. 

B<$fnumber = PQfnumber($result, $field_name)>

Returns the field number associated with the given field name. 

B<$ftype = PQftype($result, $field_num)>

Returns the oid of the type of the given field number. 

B<$fsize = PQfsize($result, $field_num)>

Returns the size in bytes of the type of the given field number. 
It returns -1 if the field has a variable length.

B<$value = PQgetvalue($result, $tup_num, $field_num)>

Returns the value of the given tuple and field. This is 
a null-terminated ASCII string. Binary cursors will not
work. 

B<$length = PQgetlength($result, $tup_num, $field_num)>

Returns the length of the value for a given tuple and field. 

B<$null_status = PQgetisnull($result, $tup_num, $field_num)>

Returns the NULL status for a given tuple and field. 

B<$result_status = PQresultStatus($result)>

Returns the status of the result. For comparing the status you 
may use one of the following constants depending upon the 
command executed:
 - PGRES_EMPTY_QUERY
 - PGRES_COMMAND_OK
 - PGRES_TUPLES_OK
 - PGRES_COPY_OUT
 - PGRES_COPY_IN
 - PGRES_BAD_RESPONSE
 - PGRES_NONFATAL_ERROR
 - PGRES_FATAL_ERROR

B<$cmdStatus = PQcmdStatus($result)>

Returns the command status of the last query command.

B<$oid = PQoidStatus($result)>

In case the last query was an INSERT command it returns the oid of the 
inserted tuple. 

B<PQprintTuples($result, $fout, $printAttName, $terseOutput, $width)>

Kept for backward compatibility. Use PQprint.

B<PQprint($fout, $result, $header, $align, $standard, $html3, $expanded, $pager, $fieldSep, $tableOpt, $caption, ...)>

Prints out all the tuples in an intelligent  manner. This function 
differs from the C-counterpart. The struct PQprintOpt has been 
implemented by a list. This list is of variable length, in order 
to care for the character array fieldName in PQprintOpt. 
The arguments $header, $align, $standard, $html3, $expanded, $pager
are boolean flags. The arguments $fieldSep, $tableOpt, $caption
are strings. You may append additional strings, which will be 
taken as replacement for the field names. 

B<PQclear($result)>

Frees all memory of the given result. 


=head2 3. Miscellaneous

These functions cover the topic of copying data from or into
a table, as well as the debugging using traces. Also there is
a function for asynchronous notification.

B<PQputline($conn, $string)>

Sends a string to the backend. The application must explicitly 
send the single character "." to indicate to the backend that 
it has finished sending its data. 

B<$ret = PQgetline($conn, $string, $length)>

Reads a string up to $length - 1 characters from the backend. 
PQgetline returns EOF at EOF, 0 if the entire line has been read, 
and 1 if the buffer is full. If a line consists of the single 
character "." the backend has finished sending the results of 
the copy command. 


B<$ret = PQendcopy($conn)>

This function waits  until the backend has finished the copy. 
It should either be issued when the last string has been sent 
to  the  backend  using  PQputline or when the last string has 
been received from the backend using PQgetline. PQendcopy returns 
0 on success, nonzero otherwise. 

B<PQtrace($conn, $debug_port)>

Messages passed between frontend and backend are echoed to the 
debug_port file stream. 

B<PQuntrace($conn)>

Disables tracing. 

B<($table, $pid) = PQnotifies($conn)>

Checks for asynchronous notifications. This functions differs from 
the C-counterpart which returns a pointer to a new allocated structure, 
whereas the perl implementation returns a list. $table is the table 
which has been listened to and $pid is the process id of the backend. 


=head2 4. Large Objects

These functions provide file-oriented access to user data. 
The large object interface is modeled after the Unix file 
system interface with analogues of open, close, read, write, 
lseek, tell. In order to get a consistent naming, all function 
names have been prepended with 'PQ'. 

B<$lobjId = PQlo_creat($conn, $mode)>

Creates a new large object. $mode is a bitmask describing 
different attributes of the new object. Use the following constants: 
 - PGRES_INV_SMGRMASK
 - PGRES_INV_ARCHIVE
 - PGRES_INV_WRITE
 - PGRES_INV_READ

Upon failure it returns PGRES_InvalidOid. 

B<$ret = PQlo_unlink($conn, $lobjId)>

Deletes a large object. Returns -1 upon failure. 

B<$lobj_fd = PQlo_open($conn, $lobjId, $mode)>

Opens an existing large object and returns an object id. 
For the mode bits see PQlo_create. Returns -1 upon failure. 

B<$ret = PQlo_close($conn, $lobj_fd)>

Closes an existing large object. Returns 0 upon success 
and -1 upon failure. 

B<$nbytes = PQlo_read($conn, $lobj_fd, $buf, $len)>

Reads $len bytes into $buf from large object $lobj_fd. 
Returns the number of bytes read and -1 upon failure. 

B<$nbytes = PQlo_write($conn, $lobj_fd, $buf, $len)>

Writes $len bytes of $buf into the large object $lobj_fd. 
Returns the number of bytes written and -1 upon failure. 

B<$ret = PQlo_lseek($conn, $lobj_fd, $offset, $whence)>

Change the current read or write location on the large object 
$obj_id. Currently $whence can only be 0 (L_SET). 

B<$location = PQlo_tell($conn, $lobj_fd)>

Returns the current read or write location on the large object 
$lobj_fd. 

B<$lobjId = PQlo_import($conn, $filename)>

Imports a Unix file as large object and returns 
the object id of the new object. 

B<$ret = PQlo_export($conn, $lobjId, $filename)>

Exports a large object into a Unix file. 
Returns -1 upon failure, 1 otherwise. 


=head1 AUTHOR

Edmund Mergl <E.Mergl@bawue.de>

=head1 SEE ALSO

libpq(3), large_objects(3).

=cut
