########################################################################
# SGMLSPL script to convert from the DocBook DTD to LaTeX.
#
# by David Megginson <dmeggins@aix1.uottawa.ca>
#
# This is a simple translation sample script.  Note that this
# script is customised for the SGMLS.pm and sgmlspl documentation in
# this directory, and is not meant as a general-purpose DocBook->LaTeX
# translator (though it could form the basis of one).
#
# $Log: tolatex.pl,v $
# Revision 1.4  1995/12/03  22:08:03  david
# Changed to use SGMLS::Output instead of Output, and to take advantage
# of the SGMLS::Refs package for forward references.
#
# Revision 1.3  1995/08/12  16:25:07  david
# Oops! Fixed comment leader in RCS file.
#
# Revision 1.2  1995/08/12  16:22:17  david
# Revised for release 1.01: fixed handling of prefixed sysid's from
# NSGMLS.
#
########################################################################

use SGMLS;			# Use the SGMLS package.
use SGMLS::Output;		# Use stack-based output.
use SGMLS::Refs;		# Use forward-reference manager.

$version = '$Id: tolatex.pl,v 1.4 1995/12/03 22:08:03 david Exp $';

$basename = shift;		# We need an explicit basename to
				# produce different reference
				# files for the two documents.

########################################################################
# Document handlers, called at the beginning and end of the entire parse.
########################################################################

sgml('start', sub {
    system("touch .redo_$basename");
                                # Start up the reference manager.
    $Refs = new SGMLS::Refs("$basename.refs");
});
sgml('end', sub {
    unlink(".redo_$basename") unless $Refs->warn;
});


########################################################################
# Specific element handlers, called at the beginning and end of each
# corresponding element.
########################################################################

#
# The <ARTICLE> is the top-level argument: when it starts, print the
# beginning of the LaTeX preamble; when it ends, print the LaTeX
# close.
#
sgml('<ARTICLE>', "\\documentstyle[11pt]{article}\n\n" .
     "\\setlength{\\parskip}{3ex}\n" .
     "\\raggedright\n\n");
sgml('</ARTICLE>', "\\end{document}\n");

#
# The <ARTHEADER> contains bibliographical information about the
# article.  Push output to nul so that nothing prints by default,
# grab any relevant information into variables using the sub-elements,
# then end the preamble and begin the actual document once the
# header is finished.
#
sgml('<ARTHEADER>', sub { push_output('nul'); });
sgml('</ARTHEADER>', sub {
    pop_output;
    output "\\title{$title}\n";
    output "\\author{$firstname $surname \\\\\n";
    output "  $orgdiv, \\\\\n" if $orgdiv;
    output "  $orgname, \\\\\n" if $orgname;
    output "  Email: {\\tt $email} \\\\\n" if $email;
    output "}\n\n";
    output "\\date{$date}\n" if $date;
    output "\n\\begin{document}\n\\maketitle\n\n";
});

#
# Use push_output and pop_output to save the contents of the <TITLE>
# element.  The element's treatment is context-sensitive: inside
# an <ARTHEADER>, save the title in the $title variable; inside <SECT1>,
# or <IMPORTANT>, begin the new section now; inside a <TABLE>, print 
# the caption.
#
sgml('<TITLE>', sub { push_output 'string'; });
sgml('</TITLE>', sub {
    my $element = shift;
    my $data = pop_output;
    if ($element->in(ARTHEADER)) {
	$title = $data;
    } elsif ($element->in(SECT1) || $element->in(IMPORTANT)) {
	$Refs->put("title:$current_section",$data);
    } elsif ($element->in(TABLE)) {
	$Refs->put("title:$current_table",$data);
    } else {
	die "Do not know what to do with TITLE in "
	    . $element->parent->name . "\n";
    }
});

# Element: AUTHORGROUP
sgml('<AUTHORGROUP>', "");
sgml('</AUTHORGROUP>', "");

# Element: AUTHOR
sgml('<AUTHOR>', "");
sgml('</AUTHOR>', "");

#
# Save the contents of <FIRSTNAME> into the variable $firstname,
# presuming that this appears only within the <ARTHEADER>.
#
sgml('<FIRSTNAME>', sub { push_output('string'); });
sgml('</FIRSTNAME>', sub { $firstname = pop_output; });

#
# Save the contents of <SURNAME> into the variable $surname,
# presuming that this appears only within the <ARTHEADER>.
#
sgml('<SURNAME>', sub { push_output('string'); });
sgml('</SURNAME>', sub{ $surname = pop_output; });

# Element: AFFILIATION
sgml('<AFFILIATION>', "");
sgml('</AFFILIATION>', "");

#
# Save the contents of <ORGNAME> into the variable $orgname,
# presuming that this appears only within the <ARTHEADER>.
#
sgml('<ORGNAME>', sub { push_output('string'); });
sgml('</ORGNAME>', sub { $orgname = pop_output; });

#
# Save the contents of <ORGDIV> into the variable $orgdiv,
# presuming that this appears only within the <ARTHEADER>.
#
sgml('<ORGDIV>', sub { push_output('string'); });
sgml('</ORGDIV>', sub { $orgdiv = pop_output; });

# Element: ADDRESS
sgml('<ADDRESS>', "");
sgml('</ADDRESS>', "");

#
# Save the contents of <EMAIL> into the variable $email,
# presuming that this appears only within the <ARTHEADER>.
#
sgml('<EMAIL>', sub { push_output('string'); });
sgml('</EMAIL>', sub { $email = pop_output; });

# Element: ARTPAGENUMS
sgml('<ARTPAGENUMS>', "");
sgml('</ARTPAGENUMS>', "");

#
# Print an <IMPORTANT> section in italics.
#
sgml('<IMPORTANT>', sub {
    my $element = shift;

    push @current_section_stack,$current_section;
    $current_section = $element->attribute(ID)->value || generate_id();
    my $title = $Refs->get("title:$current_section") || '[Untitled]';
    output "\n\n\{\\em\\section{$title}\n\\label{$current_section}\n\n";
});
sgml('</IMPORTANT>', "\n\n\}\n\n");

#
# Blank lines delimit paragraphs in LaTeX.
#
sgml('<PARA>', "\n");
sgml('</PARA>', "\n");

#
# The actual section will begin with the <TITLE> element, but note
# the section's ID now.
#
sgml('<SECT1>', sub {
    my $element = shift;

    push @current_section_stack,$current_section;
    $current_section = $element->attribute(ID)->value || generate_id();
    my $title = $Refs->get("title:$current_section") || '[Untitled]';
    output "\n\n\\section{$title}\n\\label{$current_section}\n\n";
});
sgml('</SECT1>', sub {
    $current_section = pop @current_section_stack;
    output "\n\n";
});

# Element: LINK (used only for HTML version)
sgml('<LINK>', "");
sgml('</LINK>', "");

# Element: ULINK (used only for HTML version)
sgml('<ULINK>', "");
sgml('</ULINK>', "");

#
# An XREF may print the section number (or table number, etc.) or the actual
# page number, depending upon the value of the ROLE= attribute.
#
sgml('<XREF>', sub {
    my $element = shift;
    my $idref = $element->attribute('LINKEND')->value;
    my $type = $element->attribute('ROLE')->value;
    if ($type eq 'page') {
      output "\\pageref{$idref}";
    } else {
      output "\\ref{$idref}";
    }
});
sgml('</XREF>', "");

#
# The name of an application will appear in small caps.
#
sgml('<APPLICATION>', "{\\sc ");
sgml('</APPLICATION>', "}");

#
# Use LaTeX emphasis for emphasis.
#
sgml('<EMPHASIS>', "{\\em ");
sgml('</EMPHASIS>', "\\/}");

#
# Technical terms are simply emphasised.
#
sgml('<GLOSSTERM>', "{\\em ");
sgml('</GLOSSTERM>', "\\/}");

#
# Use proper quotation marks for quotes, with braces to get the ligaturing
# right.
#
sgml('<QUOTE>', "{``}");
sgml('</QUOTE>', "{''}");

#
# Acronyms appear in small caps.
#
sgml('<ACRONYM>', "{\\sc ");
sgml('</ACRONYM>', "}");

#
# Filenames appear in typewriter.
#
sgml('<FILENAME>', "{\\tt ");
sgml('</FILENAME>', "}");

#
# Symbols appear in typewriter.
#
sgml('<SYMBOL>', "{\\tt ");
sgml('</SYMBOL>', "}");

#
# Program listings appear in the verbatim environment, which
# preserves whitespace but also prints control characters as-is
# (see the CDATA and SDATA handlers below for the special
# treatment required).
#
sgml('<PROGRAMLISTING>', "\n{\\footnotesize\\begin{verbatim}\n");
sgml('</PROGRAMLISTING>', "\n\\end{verbatim}}\n");

#
# Class names appear in typewriter.
#
sgml('<CLASSNAME>', "{\\tt ");
sgml('</CLASSNAME>', "}");

#
# Commands (ie. methods, etc.) appear in typewriter.
#
sgml('<COMMAND>', "{\\tt ");
sgml('</COMMAND>', "}");

#
# Begin a formal table.  The actual tabular environment will come
# later, but for now, note the table's ID (so that it can follow
# the caption -- see <TITLE>, above), and begin a floating environment
# with the following placement preference: here, top, bottom, page.
# Print tables in a small font to save space.
#
sgml('<TABLE>', sub {
  my $element = shift;
  push @current_table_stack,$current_table;
  $current_table = $element->attribute('ID')->value || generate_id();
  my $title = $Refs->get("title:$current_table");
  output "\n\\begin{table}[htbp]\n\\footnotesize\n";
  output "\\caption{$title}\n\\label{$current_table}\n";
});
sgml('</TABLE>', "\\end{table}\n");

#
# Here is where the tables get tricky: the <TGROUP> element specifies
# the table's width in columns: instead of letting LaTeX sort out the
# width of each column, presume that we want the table 4.45 inches wide
# (!?!) and divide that width by the number of columns.  This is
# a cheezy solution, but it allows the use of parboxes in the tables
# for continuous text.
#
sgml('<TGROUP>', sub {
    my $element = shift;
    $table_columns = $element->attribute('COLS')->value;
    $width = 4.45 / $table_columns;
});
sgml('</TGROUP>', "");

#
# Presume only one <THEAD> for each table, at its beginning.  Begin
# the tabular element here, presuming left justification.
#
sgml('<THEAD>', sub {
    output "\\vspace{2ex}\\begin{tabular}{l" .
	"|l" x ($table_columns - 1) . "}\n";
});
sgml('</THEAD>', "");

#
# For end the tabular environment at the end of the table body.
#
sgml('<TBODY>', "");
sgml('</TBODY>', "\\end{tabular}");

#
# Print a single line under each row in <TBODY>, but a double line under
# the row in <THEAD>.  Reset the variable $row to 0 each time we begin
# a new row.
#
sgml('<ROW>', sub { $row = 0; });
sgml('</ROW>', sub {
    my $element = shift;
    if ($element->in('THEAD')) {
	output "\\\\ \\hline\\hline\n";
    } else {
	output "\\\\ \\hline\n";
    }
});

#
# Here is the tricky part: use the $row variable to determine whether this
# is the _first_ entry of the row (and thus, does not require a leading "&"),
# and use parboxes for the actual entries' contents, so that they can
# take up multiple lines.  Also add 4 points to the top and bottom of each
# parbox, just to make it purtier.
#
sgml('<ENTRY>', sub {
    if ($row == 0) {
	$row = 1;
    } else {
	print " & ";
    }
    print "\\parbox[c]{" . $width . "in}{\\raggedright\\vspace{4pt} ";
});
sgml('</ENTRY>', "\\vspace{4pt}}\t");

#
# Parameters appear in slanted typewriter.
#
sgml('<PARAMETER>', "{\\tt\\sl ");
sgml('</PARAMETER>', "\\/}");

#
# Return values appear in typewriter.
#
sgml('<RETURNVALUE>', "{\\tt ");
sgml('</RETURNVALUE>', "}");

#
# Literal elements appear in typewriter.
#
sgml('<LITERAL>', "{\\tt ");
sgml('</LITERAL>', "}");


########################################################################
# Declare handlers for SDATA strings.
########################################################################

#
# These three will appear only in regular body text, so use simple
# replacement strings for their LaTeX equivalents.
#
sgml('|[mdash ]|', "{---}");
sgml('|[hellip]|', "{\\ldots}");
sgml('|[LaTeX]|', "{\\LaTeX}");

#
# &lt;, &gt;, and &amp; could appear in the <PROGRAMLISTING> element,
# where they should appear as literal '<', '>', and '&'; otherwise,
# they need special treatment in LaTeX.  Note how these handlers use
# the second argument to the handler, $event, to get the element
# currently in force.
#
sgml('|[lt    ]|', sub {
    my ($data,$event) = @_;
    if ($event->element->name eq 'PROGRAMLISTING') {
      output "\<";		# simple less-than in verbatim
    } else {
      output "\$\<\$";		# math less-than in regular text
    }
});
sgml('|[gt    ]|', sub {
    my ($data,$event) = @_;
    if ($event->element->name eq 'PROGRAMLISTING') {
      output "\>";		# simple greater-than in verbatim
    } else {
      output "\$\>\$";		# math greater-than in regular text
    }
});
sgml('|[amp   ]|', sub {
    my ($data,$event) = @_;
    if ($event->element->name eq 'PROGRAMLISTING') {
      output "\&";		# simple ampersand in verbatim
    } else {
      output "\\\&";		# escaped ampersand in regular text
    }
});


########################################################################
# CDATA handler: escape characters which can appear as-is in SGML
# data but will cause problems in LaTeX.
########################################################################

sgml('cdata',sub {
    my ($data,$event) = @_;
    unless ($event->element->name eq 'PROGRAMLISTING') {
	$data =~ s/\\/\\verb\|\\\|/g; # backslash
	$data =~ s/\{/\\\{/g;	# opening brace
	$data =~ s/\}/\\\}/g;	# closing brace
	$data =~ s/\#/\\\#/g;	# hash
	$data =~ s/\$/\\\$/g;	# dollar
	$data =~ s/\%/\\\%/g;	# percent
	$data =~ s/\&/\\\&/g;	# ampersand
	$data =~ s/\~/\\\~/g;	# tilde
	$data =~ s/\_/\\\_/g;	# underscore
	$data =~ s/\^/\\\^/g;	# caret
    }
    output $data;
});


########################################################################
# External data entity handler: deal with CDATA entities only,
# including them verbatim.  For now, I use the SYSID for the
# entity's file name, though I should probably
########################################################################



sgml('entity',sub {
    my $entity = shift;
    my $filename = $entity->filenames->[0] || $entity->sysid;
				# A strange NSGMLS-thing.
    if ($filename =~ /^FILE:/ || $filename =~ /^\<FILE[^>]+\>/) {
      $filename = $';
    }
    if ($entity->type eq 'CDATA') {
	if (-r $filename) {
	    unless (open INPUT, "<$filename") {
		die "Cannot open external file $filename\n";
	    }
	    while (<INPUT>) {
		output $_;
	    }
	    close INPUT;
	} else {
	    die "Cannot read file $filename\n";
	}
    } else {
	die "Cannot handle external entity with type " . $entity->type . "\n";
    }
});


########################################################################
# The following default handlers will catch any new elements, SDATA,
# processing instructions, or subdocument entities which I might add
# to the document later, and will report an error if they are not
# handled properly above.
########################################################################

sgml('start_element',sub { die "Unknown element: " . $_[0]->name; });
sgml('sdata',sub { die "Unknown SDATA: " . $_[0]; });
sgml('pi',sub { die "Unknown processing instruction: " . $_[0]->name; });
sgml('start_subdoc',sub { die "Unknown subdoc entity: " . $_[0]->name; });


########################################################################
# Utility functions.
########################################################################

$id_counter = 1;
sub generate_id {
    return "ID" . $id_counter++;
}

1;
