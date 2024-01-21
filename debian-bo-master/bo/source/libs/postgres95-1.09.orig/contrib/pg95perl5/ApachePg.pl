
# demo script, 
# it has been tested with apache_1.1.1 and mod_perl-0.81

require Apache::CGI;
use Pg;


$query = new Apache::CGI;

$query->print($query->header, 
              $query->start_html(-title=>'A Simple Example')
             );

if (! $query->param) {

    $query->print( 
        $query->startform,
        "<CENTER><H3>Testing Module Pg</H3></CENTER>",
        "Enter the name of the database: ",
            $query->textfield(-name=>'dbname'), "<P>",
        "Enter the command to execute: ",
            $query->textfield(-name=>'cmd', -size=>40),  "<P>",
        $query->submit(-value=>'Submit'),
        $query->endform,
    );
} else {

    $query->print("<CENTER><H3>$cmd</H3></CENTER>");

    $dbname = $query->param('dbname');
    $conn = PQsetdb('localhost', 5432, '', '', $dbname);
    $cmd = $query->param('cmd');
    Pg::doQuery($conn, $cmd, \@ary);
    PQfinish($conn);

    $query->print("<PRE>");
    for $i ( 0 .. $#ary ) {
        for $j ( 0 .. $#{$ary[$i]} ) {
            $query->print("$ary[$i][$j]\t");
        }
        $query->print("\n");
    }
    $query->print("</PRE>");

    $query->delete('dbname');
    $query->delete('cmd');
}

$query->print($query->end_html);
