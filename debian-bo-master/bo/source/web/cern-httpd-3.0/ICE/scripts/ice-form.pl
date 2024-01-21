#!/hgdv/bin/perl
#
# ice-form.pl -- cgi compliant ICE search interface // Jun 21 1994
#
# (C) Christian Neuss, Fraunhofer IGD (neuss@igd.fhg.de) 

#--- start of configuration --- put your changes here ---
# Title or name of your server:
#   Example: local($title)="ICE Indexing Gateway";
local($title)="ICE Indexing Gateway";

# location of "ice.pl"
#   Example: require "/igd/a3/home1/neuss/Perl/ice.pl";
require "/hgdv/www/.index/ice.pl";

# URL path of this script "ice.pl"
#   Example: 
local($scriptname)="/htbin/ice-form.pl";

# search directories to present in the search dialogue
#   Example: 
# local(@directories)=(
#    "Image Communication  Information Board (/icib)",
#    "WISE (/some/where/wise)"
# );

local(@directories)=(
    "Image Communication  Information Board (/icib)",
    "WISE (/wise)",
    "Multimedia Survey (/mms)",
    "Department A2 (/a2-www)",
    "Department A8 (/a8-www)",
    "Department A9 (/a9-www)",
    "DZSIM (/dzsim)",
    "CSCW Laboratory (/cscw-lab)",
    "Software Catalog (/sw-catalog)",
    "DZSIM (/dzsim)",
    "ZGDV User Interface GROUP (/zgdv-uig)"
);
#--- end of configuration --- don't change anything below ---

sub send_index {
    print "Content-type: text/html\n\n";
    print "<HEAD>\n<TITLE>", $title;
    print "</TITLE>\n</HEAD>\n";
    print "<H1>$title</H1>\n";

    print "<FORM ACTION=\"$scriptname\">\n";
    print "<HR>\n";
    print "Type in the keyword, or provide several keywords\n";
    print "connected with \"and\" and \"or\". <BR>\n";
    print "Examples: \"picture and binary\".\n";
    # print "<INPUT NAME=\"KEYWORDS\" VALUE=\"test\" SIZE=60><BR>\n";
    print "<INPUT NAME=\"KEYWORDS\"  SIZE=60><BR>\n";
    print "<HR>\n";
  
    print "You can restrict searches to documents that have changed\n";
    print "during the last <I>n</i> days (leave the other fields\n";
    print "empty to get all documents changed during that time):<BR>\n";
    print "Number of days:<INPUT NAME=\"DAYS\" VALUE=\"\" SIZE=8> <BR>\n";
		print "<HR>\n";

    print "Turn on use of thesaurus to extend a search to\n";
    print "all synonyms of a term, turn on substring matching\n";
    print "to extend searches to words which contain the given\n";
    print "term as a substring:<BR> <inPUT TYPE=\"checkbox\"\n";
    print "NAME=\"THESAURUS\" VALUE=\"thesaurus\">   Use thesaurus\n";
    print "<inPUT TYPE=\"checkbox\" NAME=\"SUBSTRING\"\n";
    print "VALUE=\"substring\"> Substring matching\n";
    print "<HR>\n";

    print "Limits search to a subtree of the document hierarchy:<BR>\n";
    print "<SELECT NAME=\"CONTEXT\">\n";
    print "<OPTION> Search in all documents\n";
    foreach $dir (@directories){
		  print "<OPTION> $dir\n";
		}
		print "</SELECT>\n";
    print "<P>\n";

    print "Start search: <INPUT TYPE=\"submit\" VALUE=\" Ok \">\n";
    print "<HR>\n";

    # print "Reset to default values: <inPUT TYPE=\"reset\" \n";
    # print "VALUE=\"Reset\"> </FORM>\n";
    print "</BODY>\n";
}

sub doit {
    do { &'send_index; return; } unless (length($ENV{"QUERY_STRING"})>0);
    local(@query_strings) = split("&", $ENV{"QUERY_STRING"});
    foreach $q (@query_strings) {
      ($attr, $val) = split("=", $q);
      # this is still just a hack..
      $val =~ s/%20/ /g;
      $val =~ s/%21/!/g;
      $val =~ s/%22/"/g;
      $val =~ s/%23/#/g;
      $val =~ s/%24/$/g;
      $val =~ s/%25/%/g;
      $val =~ s/%26/&/g;
      $val =~ s/%27/"/g;
      $val =~ s/%28/(/g;
      $val =~ s/%29/)/g;
      $val =~ s/%2C/,/g;
      $val =~ s/%2F/\//g;
      $val =~ s/%3B/;/g;
      $val =~ s/%3F/?/g;
      $val =~ s/%3D/=/g;
      $val =~ s/%5B/[/g;
      $val =~ s/%5D/]/g;
      $val =~ s/%5E/^/g;
      $val =~ s/%7B/{/g;
      $val =~ s/%7D/}/g;
      $val =~ s/%5C/\\/g;
      $val =~ s/\+/ /g;
      $val =~ s/^\s//;
      $queryvals{$attr} = $val;
    }

    $pquery = $query = $queryvals{KEYWORDS};
    $context = $queryvals{CONTEXT};
    $context = $queryvals{CONTEXT};
    if($context =~ m:\(([^)]*)\):) {
		  $context=$1;
    }else{
		  $context="";
		}
    $thesaurus = $queryvals{THESAURUS};
    $substring = $queryvals{SUBSTRING};
    $days = $queryvals{DAYS};
    if(length($days)>0){
      $pquery.=" -D $days";
    }
    if(length($thesaurus)>0){
      $pquery.=" -T";
    }
    if(length($substring)>0){
      $pquery.=" -S";
    }
    if(length($context)>0){
      $pquery.=" @ $context";
    }

    print "Content-type: text/html\n\n";

    print "<HEAD>\n<TITLE>Search of $title\n";
    print "</TITLE>\n</HEAD>\n";
    print "<BODY>\n<H1>", $title, "</H1>\n";

 ###print "query $pquery\n";
    ($err,$page) = &getquery($pquery);
    if($err){
      print "Query was: $query<BR>\n";
      print "Problem: $err\n";
      print "</BODY>";
      return undef;
    }
    print "Preferences set for this query:\n";
    print "<UL>\n";
    if ($query) {
		  print "<LI> query was \"$query\"\n";
		}
    if($context){
      print "<LI> context was set to $context.\n";
    }
    if($thesaurus){
      print "<LI> use of thesaurus turned on\n";
    }
    if($substring){
      print "<LI> substring matching turned on\n";
    }
    if($days){
      print "<LI> changed in the last $days days\n";
    }
    print "</UL>\n";
    if($page){
     print "<P>The index contains the following\n";
     print "items relevant to the query\n";
     print "$page\n";
    }else{
     print "<P> Nothing found.\n";
    }      
 
    print "</BODY>\n";
}

&doit();