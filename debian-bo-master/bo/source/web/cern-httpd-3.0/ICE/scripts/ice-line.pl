#!/hgdv/bin/perl
#
# ice-line.pl -- cgi compliant ICE search interface // Jun 21 1994
#
# (C) Christian Neuss, Fraunhofer IGD (neuss@igd.fhg.de) 

#--- start of configuration --- put your changes here ---
# Title or name of your server:
#   Example: local($title)="ICE Indexing Gateway";
local($title)="ICE Indexing Gateway";

# location of "ice.pl"
#   Example: require "/igd/a3/home1/neuss/Perl/ice.pl";
require "/hgdv/www/.index/ice.pl";

#--- end of configuration --- don't change anything below ---

sub send_index {
    print "Content-type: text/html\n\n";
    print "<HEAD>\n<ISINDEX>\n<TITLE>", $title;
    print "</TITLE>\n</HEAD>\n";
    print "<H1>$title</H1>\n";

    print "This is a gateway to the ICE indexing software.\n";
    print "By supplying keywords and setting special options, you can\n";
    print "do a free text search on this World Wide Web archive.<P>\n";

    print "<HR>\n";
    print "Type in a search query in your browsers search dialog.\n";
    print "Here is a short description of the syntax:\n"; 
    
    print "Enter the keyword, or provide several keywords\n";
    print "connected with \"and\" and \"or\".\n";
    print "Example: \"picture and binary\". Ed (1) style pattern\n";
    print "matching is also possible.\n";
    print "<P>\n";
  
    print "Turn on use of thesaurus to extend a search to\n";
    print "all synonyms of a term, turn on fault tolerant searches\n";
    print "to extend searches to words which are \"similar\" to the\n";
    print "given term. The thesaurus can be turned on by adding \"-T\"\n";
    print "after the keyword(s).\n";
		print "By adding -S after the keywords, a search not only matches\n";
		print "whole words but also substrings.\n";
		print "Adding -D followed by a number restricts the search to\n";
		print "those files that where updated in the last <I>n</I> days.\n";
    print "<HR>\n";

    print "You can provide an optional path context which limits \n";
    print "searches to certain subtrees of the document hierarchy.\n";
    print "The context is seperated with a \"@\" and added to the\n";
    print "end of the query. <P>\n";

    print "Here's a fully blown example:\n";
    print "<PRE> .ender.* or animation -T @ /icib/it</PRE>\n";
    print "will retrieve files which <UL>\n";
    print "<LI> have a URL path starting with /icib/it\n";
    print "<LI> contain the word animation or a related term\n";
    print "<LI> contain a word starting with any letter followed\n";
    print "     by the string \"ender\", followed by an arbitrary\n";
    print "     number of letters, or a term related to any\n";
    print "     word in the thesaurus which matches this expression.\n";
    print "</OL>\n";
    print "Ok, normally you won't need all these features at once.\n";
    print "</BODY>\n";
}

sub doit {
    do { &'send_index; return; } unless (length($ENV{"QUERY_STRING"})>0);
    local($val) = $ENV{"QUERY_STRING"};
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

    local($query)=$val;

    print "Content-type: text/html\n\n";

    print "<HEAD>\n<TITLE>Search of $title\n";
    print "</TITLE>\n</HEAD>\n";
    print "<BODY>\n<H1>", $title, "</H1>\n";

    ($err,$page) = &getquery($query);
    print "Query was: $query<BR>\n";
    if($err){
      print "Problem: $err\n";
      print "</BODY>";
      return undef;
    }
    
    if($page){
     print "<P>The index contains the following\n";
     print "items relevant to \`$query\':<P>\n";
     print "$page\n";
    }else{
     print "<P> Nothing found.\n";
    }      
 
    print "</BODY>\n";
}

&doit();