########################################################################
# SGMLSPL script to convert from the DocBook DTD to HTML pages.
#
# by David Megginson <dmeggins@aix1.uottawa.ca>
#
# This is a slightly more complicated script than tolatex.pl, since it
# uses forward references and an external reference file.  Note that
# this script is customised for the SGMLS.pm and sgmlspl documentation
# in this directory, and is not meant as a general-purpose
# DocBook->HTML translator (though it could form the basis of one).
# Because each parse uses information saved from the last parse,
# you might need two passes to make certain that all references are
# up to date.
#
# $Log: tohtml.pl,v $
# Revision 1.4  1995/12/03  22:07:21  david
# Changed to use SGMLS::Output instead of Output, and to take advantage
# of the SGMLS::Refs package for forward references.
#
# Revision 1.3  1995/08/12  16:25:53  david
# Oops! Fixed comment leader in RCS file.
#
# Revision 1.2  1995/08/12  16:21:06  david
# Changes for release 1.01: fixed handling of prefixed sysid's from
# NSGMLS.
#
########################################################################

use SGMLS;			# Use the SGMLS package.
use SGMLS::Output;		# Use stack-based output.
use SGMLS::Refs;

$version = '$Id: tohtml.pl,v 1.4 1995/12/03 22:07:21 david Exp $';

$basename = shift;		# Extra argument to script is basename.

#
# This conversion script keeps the idea of a current ID and a current
# file.  Since the SGML document will be broken up into a series of
# smaller HTML documents, it is necessary to keep track of the current
# file name.  The current ID is the ID (explicit or implied) of the
# most recent element which wants to capture titles, etc.
#
$current_id = '';		# The ID of the current container element.
@current_id_stack = ();		# The IDs of any parent container elements.
$current_file = '';		# The name of the current output file.
@current_file_stack = ();	# The names of the parent output files.

$top_id = '';			# The ID of the top element.
$top_file = '';			# The ID of the top file.

$previous_file = '';		# The previous file on the same level.

$table_counter = 0;		# The number of the current table.




########################################################################
# Handler declarations for sgmlspl.
########################################################################

#
# Use the 'start' and 'end' handlers of the document to begin and
# terminate reference handling.
#
sgml('start', sub { 
    system("touch .redo_$basename");
                                # Start up the reference manager.
    $Refs = new SGMLS::Refs("$basename.refs");
});
sgml('end', sub {
    unlink(".redo_$basename") unless $Refs->warn;
});


# 
# The <ARTICLE> is the top-level element.
#
sgml('<ARTICLE>', sub {
    start_html(shift);
    $top_id = $current_id;
    $top_file = $current_file;
});
sgml('</ARTICLE>', sub { end_html(); });

#
# Ignore all of the header except for the bits which we actually want,
# by pushing output to 'nul'.
#
sgml('<ARTHEADER>', sub { push_output 'nul'; });
sgml('</ARTHEADER>', sub { pop_output(); });

#
# Save the title of something for future reference.
#
sgml('<TITLE>', sub { push_output 'string'; });
sgml('</TITLE>', sub { $Refs->put("title:$current_id",pop_output()); });

#
# These are just containers in the <ARTHEADER>.
#
sgml('<AUTHORGROUP>', "");
sgml('</AUTHORGROUP>', "");
sgml('<AUTHOR>', "");
sgml('</AUTHOR>', "");
sgml('<AFFILIATION>', "");
sgml('</AFFILIATION>', "");
sgml('<ADDRESS>', "");
sgml('</ADDRESS>', "");
sgml('<ARTPAGENUMS>', "");
sgml('</ARTPAGENUMS>', "");

#
# Save the author's first name for future reference.
#
sgml('<FIRSTNAME>', sub { push_output 'string'; });
sgml('</FIRSTNAME>', sub { $Refs->put("firstname:$current_id",pop_output()); });

#
# Save the author's surname for future reference.
#
sgml('<SURNAME>', sub { push_output 'string'; });
sgml('</SURNAME>', sub { $Refs->put("surname:$current_id",pop_output()); });

#
# Save the organisation name for future reference.
#
sgml('<ORGNAME>', sub { push_output 'string'; });
sgml('</ORGNAME>', sub { $Refs->put("orgname:$current_id",pop_output()); });

#
# Save the organisation division for future reference.
#
sgml('<ORGDIV>', sub { push_output 'string'; });
sgml('</ORGDIV>', sub { $Refs->put("orgdiv:$current_id",pop_output()); });

#
# Save the email address for future reference.
#
sgml('<EMAIL>', sub { push_output('string'); });
sgml('</EMAIL>', sub { $Refs->put("email:$current_id",pop_output()); });



#
# Sectioning elements -- all of these simply call the &start_html
# and &end_html subroutines, which do all of the real work.
#
sgml('<IMPORTANT>', sub { start_html(shift); });
sgml('</IMPORTANT>', sub { end_html(); });
sgml('<SECT1>', sub { start_html(shift); });
sgml('</SECT1>', sub { end_html(); });
sgml('<SECT2>', sub { start_html(shift); });
sgml('</SECT2>', sub { end_html(); });
sgml('<SECT3>', sub { start_html(shift); });
sgml('</SECT3>', sub { end_html(); });
sgml('<SECT4>', sub { start_html(shift); });
sgml('</SECT4>', sub { end_html(); });
sgml('<SECT5>', sub { start_html(shift); });
sgml('</SECT5>', sub { end_html(); });


#
# Paragraphs must be marked explicitly in HTML -- use the HTML 3
# practice (actually just _real_ SGML, for a change) of marking both
# the beginning and the end.
#
sgml('<PARA>', "<P>");
sgml('</PARA>', "</P>\n\n");




#
# Cross-references.
#

#
# This is an internal cross reference -- get the URL by
# simply adding ".html" to the IDREF (note that this would not work
# for tables!!!).
#
sgml('<LINK>', sub {
    my $element = shift;
    output "<A HREF=";
    output lc($element->attribute(LINKEND)->value) . ".html";
    output ">";
});
sgml('</LINK>', "</A>");

#
# This is an external cross-reference, with a supplied URL.
#
sgml('<ULINK>', sub {
    my $element = shift;
    output "<A HREF=\"";
    output $element->attribute(URL)->value;
    output "\">";
});
sgml('</ULINK>', "</A>");


#
# This is a pointer to something (in this case, always a table).
#
sgml('<XREF>', sub {
    my $element = shift;
    output $Refs->get('xref:' . lc($element->attribute(LINKEND)->value));
});
sgml('</XREF>', "");



#
# Inline elements.
#

#
# Print application names in typewriter.
#
sgml('<APPLICATION>', "<TT>");
sgml('</APPLICATION>', "</TT>");

#
# Print acronyms in bold.
#
sgml('<ACRONYM>', "<B>");
sgml('</ACRONYM>', "</B>");

#
# Print terms in italics.
#
sgml('<GLOSSTERM>', "<I>");
sgml('</GLOSSTERM>', "</I>");

#
# Print file names in typewriter.
#
sgml('<FILENAME>', "<TT>");
sgml('</FILENAME>', "</TT>");

#
# Print symbols in typewriter.
#
sgml('<SYMBOL>', "<TT>");
sgml('</SYMBOL>', "</TT>");

#
# Print return values in typewriter.
#
sgml('<RETURNVALUE>', "<TT>");
sgml('</RETURNVALUE>', "</TT>");

#
# Print quotations in quotation marks.
#
sgml('<QUOTE>', '"');
sgml('</QUOTE>', '"');

#
# Print commands in typewriter.
#
sgml('<COMMAND>', "<TT>");
sgml('</COMMAND>', "</TT>");

#
# Print parameters in typewriter.
#
sgml('<PARAMETER>', "<IT>");
sgml('</PARAMETER>', "</IT>");

#
# Print literal elements in typewriter.
#
sgml('<LITERAL>', "<TT>");
sgml('</LITERAL>', "</TT>");

#
# Print class names in typewriter.
#
sgml('<CLASSNAME>', "<TT>");
sgml('</CLASSNAME>', "</TT>");

#
# Emphasise emphasis.
#
sgml('<EMPHASIS>', "<EM>");
sgml('</EMPHASIS>', "</EM>");



#
# Block elements.
#

#
# Program listings are preformatted.
#
sgml('<PROGRAMLISTING>', "<P>\n<PRE>");
sgml('</PROGRAMLISTING>', "</PRE>\n</P>\n");

#
# Keep a counter for table numbers, note the ID, and look up the
# title (caption) for the table.
#
sgml('<TABLE>', sub {
    my $element = shift;
    push @current_id_stack, $current_id;
    $current_id = lc($element->attribute(ID)->value || gen_id());
    $table_counter++;
    $Refs->put("xref:$current_id",$table_counter);
    output "\n<H3>Table $table_counter: " 
	. $Refs->get("title:$current_id") . "</H3>\n\n";
});
sgml('</TABLE>', sub {
    output "\n";
    $current_id = pop @current_id_stack;
});

#
# Nothing needs to be done here -- we don't care how many cells there are.
#
sgml('<TGROUP>', "");
sgml('</TGROUP>', "");

#
# We will keep track of all of the entries in the head, for later use.
#
sgml('<THEAD>', sub { @cell_headings = (); push_output('nul'); });
sgml('</THEAD>', sub { pop_output(); });

#
# Print a single horizontal rule before the beginning of the body.
#
sgml('<TBODY>', "<HR>");
sgml('</TBODY>', "");

#
# Make each row into a labelled list (!!) -- HTML 3 does have tables,
# but they might not be able to handle the paragraph-length entries
# which I used in my documentation (these will not print if we are
# in the <THEAD>, since output will be 'nul').
#
sgml('<ROW>', sub { 
    output "\n<DL>\n";
    $cell_counter = 0;
});
sgml('</ROW>', "\n</DL>\n<HR>\n\n");

#
# If an entry is in the <THEAD>, save it for later use; otherwise,
# print the entry as a list item with its corresponding <THEAD> entry
# as a label.
#
sgml('<ENTRY>', sub {
    my $element = shift;
    if ($element->within(THEAD)) {
	push_output 'string';
    } else {
	output "<DT><B>";
	output $cell_headings[$cell_counter];
	output "</B></DT>\n<DD>";
    }
});
sgml('</ENTRY>', sub {
    my $element = shift;
    if ($element->within(THEAD)) {
	$cell_headings[$cell_counter] = pop_output();
    } else {
	output "</DD>\n";
    }
    $cell_counter++;
});



########################################################################
# SDATA Handlers -- use HTML entities wherever possible.
########################################################################

sgml('|[lt    ]|', "&lt;");
sgml('|[gt    ]|', "&gt;");
sgml('|[mdash ]|', "--");
sgml('|[LaTeX]|', "LaTeX");
sgml('|[hellip]|', "...");
sgml('|[amp   ]|', "&amp;");



########################################################################
# The generic external data entity handler.  Handle only entities
# with type CDATA, and simply dump their files into the current
# document with minimal conversion.
########################################################################

sgml('entity',sub {
    my $entity = shift;
				# Use the first generated filename
				# or the system identifier.
    my $filename = $entity->filenames->[0] || $entity->sysid;
				# A strange, NSGMLS-thing.
    if ($filename =~ /^FILE:/ || $filename =~ /^\<FILE[^>]+\>/) {
      $filename = $';
    }

				# Handle only CDATA.
    if ($entity->type eq 'CDATA') {

	if (-r $filename) {
	    unless (open INPUT, "<$filename") {
		die "Cannot open external file $filename\n";
	    }
				# Convert special SGML characters.
	    while (<INPUT>) {
		s/\&/\&amp;/g;
		s/\</\&lt;/g;
		s/\>/\&gt;/g;
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
# Default handlers -- these will pick up any unrecognised elements,
# SDATA strings, processing instructions, or subdocument entities,
# and report an error to the user.
#########################################################################


sgml('start_element',sub { die "Unknown element: " . $_[0]->name; });
sgml('sdata',sub { die "Unknown SDATA: " . $_[0]; });
sgml('pi',sub { die "Unknown processing instruction: " . $_[0]; });
sgml('start_subdoc',sub { die "Unknown subdoc entity: " . $_[0]->name; });


#
# End of sgmlspl handler declarations.
#


########################################################################
# Utility procedures.
########################################################################


#
# Given an element, start a new HTML document for it.
#
sub start_html {
    my $element = shift;
    my $old_file = $current_file;

				# Save the old values on the stack.
    push @current_id_stack, $current_id;
    push @current_file_stack, $current_file;

				# Get the new ID and file.
    $current_id = lc($element->attribute(ID)->value || gen_id());
    $current_file = $current_id . '.html';

				# Note the previous child, if any.
    if ($previous_file) {
	$Refs->put("previous:$current_file",$previous_file);
	$Refs->put("next:$previous_file",$current_file);
    }
    $previous_file = '';

				# Put a reference up to the parent.
    if ($old_file) {
	$Refs->put("up:$current_file",$old_file);
    }

				# Look up the title reference.
    my $plaintitle = my $title = $Refs->get("title:$current_id");

				# Strip the title.
    $plaintitle =~ s/\<[^\>]+\>//g;

				# If this is not the top-level
				# file, send out a link
				# before beginning the new file.
    if ($old_file) {
	output "<LI><A HREF=\"$current_file\">$plaintitle</A></LI>\n";
    }

				# Send output to the new file.
    push_output('file',$current_file);

				# Print the front matter.
    output "<HTML>\n<HEAD>\n<TITLE>$plaintitle</TITLE>\n</HEAD>\n<BODY>\n";

				# Navigational aids, if this is not
				# the top-level file.
    if ($old_file) {
	output "\n<P><B>Links</B>: ";
	my $up = $Refs->get("up:$current_file");
	my $previous = $Refs->get("previous:$current_file");
	my $next = $Refs->get("next:$current_file");
	output "<A HREF=$next>Next</A> " if $next;
	output "<A HREF=$previous>Previous</A> " if $previous;
	output "<A HREF=$up>Up</A> " if $up;
	output "<A HREF=$top_file>Top</A>";
	output "</P>\n\n";
    }

    
    output "<H1>$title</H1>\n\n";
}

#
# End the HTML document.
#
sub end_html {
				# Look up the name and email info.
    my $firstname = $Refs->get("firstname:$current_id") ||
	$Refs->get("firstname:$top_id");
    my $surname = $Refs->get("surname:$current_id") ||
	$Refs->get("surname:$top_id");
    my $email = $Refs->get("email:$current_id") ||
	$Refs->get("email:$top_id");

				# Restore the previous ID and file,
				# and note this as the previous
				# child.
    $previous_file = $current_file;
    $current_id = pop @current_id_stack;
    $current_file = pop @current_file_stack;

				# If this is not the top-level file,
				# add some navigational information.
    if ($current_file) {
	output "\n<P><B>Links</B>: ";
	my $up = $Refs->get("up:$previous_file");
	my $previous = $Refs->get("previous:$previous_file");
	my $next = $Refs->get("next:$previous_file");
	output "<A HREF=$next>Next</A> " if $next;
	output "<A HREF=$previous>Previous</A> " if $previous;
	output "<A HREF=$up>Up</A> " if $up;
	output "<A HREF=$top_file>Top</A>";
	output "</P>\n\n";
    }

				# Add an address, if available,
				# including a MAILTO URL.
    output "\n<ADDRESS>";
    output "$firstname $surname " if $firstname || $surname;
    output "<A HREF=\"mailto:$email\">&lt;$email&gt;</A>" if $email;
    output "</ADDRESS>\n</BODY>\n</HTML>\n";
    pop_output();
}

#
# Generate a new ID for anything which does not already have one.
#
sub gen_id {
    $id_counter++;
    return "node$id_counter";
}

1;
