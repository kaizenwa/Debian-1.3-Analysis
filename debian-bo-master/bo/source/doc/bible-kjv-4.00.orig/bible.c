/* -*-C-*-
*******************************************************************************
*
* File:         bible.c
* RCS:          $Header: bible.c,v 1.19 93/04/26 11:17:27 chip Exp $
* Description:  Write Bible text to stdout.
* Author:       Chip Chapin, Hewlett Packard Company
* Created:      Jan 21 1989
* Modified:     Mon Apr 26 11:11:45 1993 (Chip Chapin) chip@hpclbis
* Language:     C
* Package:      Bible Retrieval System
* Status:       Experimental (Do Not Distribute)
*
*******************************************************************************
*
* Revisions:
*
* Mon Apr 26 11:09:58 1993 (Chip Chapin) chip@hpclbis
*  Print welcome message when invoked interactively.
* Fri Apr 23 12:57:34 1993 (Chip Chapin) chip@hpclbis
*  Changed fflush( NULL ) to fflush( stdout ) to make SunOS happy.
* Thu Apr 22 11:20:56 1993 (Chip Chapin) chip@hpclbis
*  Support non-POSIX sprintf.
* Tue Jan  5 17:10:01 1993 (Chip Chapin) chip@hpclbis
*  Removed printverse(), might as well call brl_printverse().
*  get_verse() is now commented out.  Nobody uses it anymore.
*  Allow leading white space on input lines.
* Tue Jan  5 10:46:17 1993 (Chip Chapin) chip@hpclbis
*  Added line formatting to cmd_list output.
*  Added ?w command.
* Mon Jan  4 15:41:26 1993 (Chip Chapin) chip@hpclbis
*  Added ?or and ?in commands.  Some big changes here.
* Thu Dec 24 11:04:04 1992 (Chip Chapin) chip@hpclbis
*  Added prompter function.
* Wed Dec 23 13:19:22 1992 (Chip Chapin) chip@hpclbis
*  Add release version to help output.
*  Rewrite help text.
*  Fix bug in command processing "?".
*  Minor tweaks to eliminate compile warnings.
* Mon Dec 21 19:12:30 1992 (Chip Chapin) chip@hpclbis
*  Added interactive command processing and concordance.
*******************************************************************************
*
* $Log:	bible.c,v $
 * Revision 1.19  93/04/26  11:17:27  11:17:27  chip (Chip Chapin)
 * Release 4.00
 * Public release of portable datafile version.
 * 
 * Revision 1.18  93/04/23  13:07:58  13:07:58  chip (Chip Chapin)
 * PORTABILITY RELEASE
 * This version supports portable data files, usable on machines with
 * differing native byte-orders.
 * Also, this version compiles and runs on non-HPUX systems.  It has been
 * tested on SunOS 4.? and ULTRIX 4.?, using SPARC and DEC 3100 hardware
 * respectively.  Note that the data file format has rolled again.
 * 
 * Revision 1.17  93/01/05  19:03:13  19:03:13  chip (Chip Chapin)
 * Release 3.00: (not for distribution)
 * Fixed errors (blank lines) in bible.data file.  Data file is not compatible
 * with previous (1.x and 2.x) distributions.  Further changes pending.
 * Rewrote context handling, and added "<" and ">" commands.
 * Tools for building brl-index are now part of release.
 * 
 * Revision 1.16  93/01/05  11:31:34  11:31:34  chip (Chip Chapin)
 * Release 2.2c: Minor tweaks.
 * 
 * Revision 1.15  93/01/05  11:19:02  11:19:02  chip (Chip Chapin)
 * Release 2.2b: Another cmd fix.
 * 
 * Revision 1.14  93/01/05  11:02:01  11:02:01  chip (Chip Chapin)
 * Release 2.2a: Fix command bug.
 * 
 * Revision 1.13  93/01/05  10:49:07  10:49:07  chip (Chip Chapin)
 * Release 2.2, Added ?w command and line formatting to ?l output.
 * 
 * Revision 1.12  93/01/04  16:20:55  16:20:55  chip (Chip Chapin)
 * Release 2.1, implements ?in and ?or commands.
 * 
 * Revision 1.11  92/12/24  11:09:11  11:09:11  chip (Chip Chapin)
 * Release 2.04.  Include verse ref in prompt line.
 * 
 * Revision 1.10  92/12/23  14:10:14  14:10:14  chip (Chip Chapin)
 * Release 2.03: minor tweaks and bug fixes.
 * 
 * Revision 1.9  92/12/22  18:17:04  18:17:04  chip (Chip Chapin)
 * Minor tweaks for release 2.02.
 * 
 * Revision 1.8  92/12/22  11:28:30  11:28:30  chip (Chip Chapin)
 * Minor release 2.01 -- fix a couple of bugs.
 * 
 * Revision 1.7  92/12/21  20:00:45  20:00:45  chip (Chip Chapin)
 * Release 2.0.  This release adds the concordance, and some small fixes.
 * 
 * Revision 1.6  89/10/02  22:19:55  22:19:55  chip (Chip Chapin)
 * Fix bug in processing "-l 2peter" sorts of things.
 * 
 * Revision 1.5  89/10/02  14:24:42  14:24:42  chip (Chip Chapin)
 * Revised usage text.
 * 
 * Revision 1.4  89/09/14  20:33:35  20:33:35  chip (Chip Chapin)
 * Release 1-2.  Supports -f and -l options for formatting the output.
 * Updates primarily brl.c, bible.c, and bible.1.
 * 
 * Revision 1.3  89/09/13  21:48:26  21:48:26  chip (Chip Chapin)
 * Implement -f and -l options for pretty-printing and linewidth limitation.
 * 
 * Revision 1.2  89/09/08  09:00:26  09:00:26  chip (Chip Chapin)
 * Bug fix and simplification: send whole input lines or arguments to BRL,
 * and let BRL worry about multiple references.  We don't care.
 * 
 * Revision 1.1  89/09/05  17:47:27  17:47:27  chip (Chip Chapin)
 * Initial revision
 * 
*/


/*----------------------------------------------------------------------
|   NAME:
|       bible.c
|
|   PURPOSE:
|       Reads verse specs from the command line or from stdin and
|       writes Bible verses to stdout.  Uses the Bible Retrieval
|       Library for all text access functions.
|
|   FUNCTIONS:
|       main
|
|   HISTORY:
|       890121 cc Initial Creation
|       890824 cc Updated to use new brl_verse_text.
|       890829 cc Updated to think about buffer size.
|       921221 cc See Revision list above for further history...
|
\*----------------------------------------------------------------------*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include "tsl.h"
#include "brl.h"
#include "version.h"

#define FALSE	(0)
#define TRUE	(1)

#define REFSZ     15
#define VSPECSZ 132
#define LINESZ 1024
#define VBUFFSZ 32768

static char rcs_ident[]="@(#)$Header: bible.c,v 1.19 93/04/26 11:17:27 chip Exp $";


char *myname;			/* Program Name */
int  mem_limit;			/* Limit on buffer space used by BRL */
int  pretty_printing=FALSE;	/* Format the output */
int  list_always=FALSE;		/* List refs after a search */
int  line_width=0;		/* Line width to use in formatting */
int  skip_inc=1;		/* Scrolling direction through text */
int  select_count=0;		/* Count of verses selected via concordance */
ref_t selectbuf[SELECTSZ];	/* List of selected verses */
ref_t inrange_start=0,
      inrange_end=0;		/* Range limits on concordance selection */
FILE *outf=NULL;		/* Output file */

char help_text[]="\n\
 -Bible verse specifications:\n\
     Verses may be specified using various standard abbreviations and \n\
     notations, including both single verses and verse ranges.\n\
     E.g.  Jn3:16, jn3:16,17 ps1:1-6\n\
     Partial specs are interpreted in the context of the previous verse.\n\
     E.g.  \"Rev3:20\" followed by \"15\" prints Rev3:15.\n\
 -Concordance (word search) commands:\n\
      ??word     Find all verses containing \"word\".\n\
                 Creates a \"ref list\" for subsequent use.\n\
      ?list      List the references in ref list.       (abbrev ?l)\n\
      ?view      View text of verses in ref list.       (abbrev ?v)\n\
      ?and word  Combine ref list w/MATCHING refs for \"word\".    (abbrev ?a)\n\
      ?or word   Combine ref list w/ALL refs for \"word\".    (abbrev ?o)\n\
      ?in <verse range>       Limit ref list to range of verses.\n\
      ?in all    Reset ref list limit.\n\
     To get a list of refs, each containing multiple words, start with:\n\
      ??word     for the first word, followed by\n\
      ?and word  for each following word.The order of the words doesn't matter.\n\
  -A few miscellaneous program control commands.\n\
     ?, ?h, ?help              -- Prints this help text.\n\
     <, >                      -- Change direction through text.\n\
     ?w file                   -- Begin writing (appending) output to \"file\".\n\
     ?w                        -- Stop writing output to a file.\n\
     ?f                        -- Toggles output formatting modes.\n\
     q, ?bye, ?exit, ?quit, ?q -- End this program.\n\
  Note that a blank line will advance one verse in current direction.\n\
";



int do_concordance( word, buf )
/*----------------------------------------------------------------------
|   NAME:
|       do_concordance
|
|   ALGORITHM:
|       Utility/wrapper function for tsl_scan_concordance.
|       Prints results of search.  Called from all concordance
|       command functions.
|
|   HISTORY:
|       930104 cc Initial creation.
|
\*----------------------------------------------------------------------*/

char *word;
ref_t *buf;
{
    int cnt;
    char *ref_word;
    char range_str[VSPECSZ];
    char vref1[REFSZ], vref2[REFSZ];
    
    printf( "  Searching for '%s'...", word );
    if (outf != NULL)
	fprintf( outf, "  Searching for '%s'...", word );
    fflush( stdout );
    
    if (inrange_start) {
	sprintf( range_str, " in %s-%s",
		brl_num_to_ref(vref1, &inrange_start),
		brl_num_to_ref(vref2, &inrange_end) );
    } else {
	range_str[0] = '\0';
    }
    
    if ((cnt=tsl_scan_concordance( word, buf, inrange_start, inrange_end )) == 0) {
	printf( " not found%s.\n", range_str );
	if (outf != NULL)
	    fprintf( outf, " not found%s.\n", range_str );
    } else {
	if (cnt == 1) ref_word = "ref";
	else ref_word = "refs";
	printf( " [%d %s%s]\n", cnt, ref_word, range_str );
	if (outf != NULL)
	    fprintf( outf, " [%d %s%s]\n", cnt, ref_word, range_str );
    }
    return (cnt);
} /* do_concordance */



cmd_ANDconcordance( word )
/*----------------------------------------------------------------------
|   NAME:
|       cmd_ANDconcordance
|
|   ALGORITHM:
|       Look up a word in the concordance, if found, combine ref list
|	with current ref list using logical AND.
|
|   HISTORY:
|       921218 cc Initial creation.
|
\*----------------------------------------------------------------------*/

char *word;
{
    int cnt;
    int i, j, k;
    ref_t sbuf[SELECTSZ];	/* List of selected verses */
    ref_t tbuf[SELECTSZ];	/* temp buff of selected verses */

    if (select_count < 1) {
	fprintf( stderr, "No references.  Use '??word'.\n" );
	return;
    }
    if (*word == '\0') {
	fprintf( stderr, "To AND-search for a word use '?and word'\n" );
	return;
    }

    if ((cnt = do_concordance( word, sbuf )) == 0) return;

    /* AND with existing list */
    i = j = k = 0;
    while ((i < select_count) && (j < cnt)) {
	if (selectbuf[i] == sbuf[j]) {
	    tbuf[k++] = sbuf[j++];
	    i++;
	}
	else if (selectbuf[i] > sbuf[j]) j++;
	else if (selectbuf[i] < sbuf[j]) i++;
    }
    
    /* Update global list */
    select_count = k;
    for (i=0; i<cnt; i++) selectbuf[i] = tbuf[i];
    printf( "  [%d refs in combined list]\n", select_count );
    if (outf != NULL)
	fprintf( outf, "  [%d refs in combined list]\n", select_count );

    if (list_always)
	/* Go ahead and display the refs */
	cmd_list();
} /* cmd_ANDconcordance */



cmd_ORconcordance( word )
/*----------------------------------------------------------------------
|   NAME:
|       cmd_ORconcordance
|
|   ALGORITHM:
|       Look up a word in the concordance, if found, combine ref list
|	with current ref list using logical OR.
|
|   HISTORY:
|       930104 cc Initial creation.
|
\*----------------------------------------------------------------------*/

char *word;
{
    int cnt;
    int i, j, k;
    ref_t sbuf[SELECTSZ];	/* List of selected verses */
    ref_t tbuf[SELECTSZ];	/* temp buff of selected verses */

    if (*word == '\0') {
	fprintf( stderr, "To OR-search for a word use '?OR word'\n" );
	return;
    }

    if ((cnt = do_concordance( word, sbuf )) == 0) return;
    
    /* OR with existing list (i.e. merge the lists) */
    i = j = k = 0;
    while ((i < select_count) && (j < cnt)) {
	if (selectbuf[i] < sbuf[j]) {
	    tbuf[k++] = selectbuf[i++];
	} else if (selectbuf[i] == sbuf[j]) {
	    tbuf[k++] = selectbuf[i++];
	    j++;
	} else if (selectbuf[i] > sbuf[j]) {
	    tbuf[k++] = sbuf[j++];
	}
    }
    /* One of the lists probably has remaining elements.
       Only ONE of the following loops will execute.
     */
    while (i < select_count) {
	tbuf[k++] = selectbuf[i++];
    }
    while (j < cnt) {
	tbuf[k++] = sbuf[j++];
    }
    
    /* Update global list */
    select_count = k;
    for (i=0; i<select_count; i++) selectbuf[i] = tbuf[i];
    printf( "  [%d refs in combined list]\n", select_count );
    if (outf != NULL)
	fprintf( outf, "  [%d refs in combined list]\n", select_count );

    if (list_always)
	/* Go ahead and display the refs */
	cmd_list();
} /* cmd_ORconcordance */



cmd_concordance( word )
/*----------------------------------------------------------------------
|   NAME:
|       cmd_concordance
|
|   ALGORITHM:
|       Look up a word in the concordance.
|
|   HISTORY:
|       921217 cc Initial creation.
|
\*----------------------------------------------------------------------*/

char *word;
{
    if (*word == '\0') {
	fprintf( stderr, "To search for a word type '??word'\n" );
	return;
    }

    select_count = do_concordance( word, selectbuf );
    
    if (list_always)
	/* Go ahead and display the refs */
	cmd_list();
} /* cmd_concordance */



cmd_help()
/*----------------------------------------------------------------------
|   NAME:
|       cmd_help
|
|   ALGORITHM:
|       Print Help message for this program.
|
|   HISTORY:
|       921217 cc Initial creation.
|	921223 cc Added release_version.
|
\*----------------------------------------------------------------------*/

{
    printf( "%s: %s\n", myname, release_version );
    printf( "%s", help_text );
    if (outf != NULL) {
	fprintf( outf, "%s: %s\n", myname, release_version );
	fprintf( outf, "%s", help_text );
    }
    fflush( stdout );
} /* cmd_help */



cmd_inrange( range )
/*----------------------------------------------------------------------
|   NAME:
|       cmd_inrange
|
|   ALGORITHM:
|       Limit concordance ref list to a certain verse range.  The
|       limit range will be applied immediately to modify the
|       current ref list (if any), and will apply to all
|       subsequent searches.
|       
|       If the specified verse range is "all", then the limits are
|       removed.
|
|   HISTORY:
|       930104 cc Initial creation.
|
\*----------------------------------------------------------------------*/

char *range;
{
    ref_t start;		/* starting verse */
    int count;			/* number of verses */
    ref_t tbuf[SELECTSZ];	/* temp buffer */
    int i;
    char vref1[REFSZ], vref2[REFSZ];

    if (strcmp(range, "all") == 0) {
	/* Reset limits */
	inrange_start = inrange_end = 0;
	return;
    }

    if (start = brl_verse_spec( &range, &count )) {
	inrange_start = start;
	inrange_end = start + count -1;

	if (select_count) {
	    /* Edit current ref list */
	    for (count=i=0; i < select_count; i++) {
		if (selectbuf[i] > inrange_end) break;
		if (inrange_start <= selectbuf[i]) tbuf[count++]=selectbuf[i];
	    }
	    /* Update global list */
	    select_count = count;
	    for (i=0; i < count; i++) selectbuf[i] = tbuf[i];
	    printf( "  [%d refs in range %s-%s]\n", select_count,
		   brl_num_to_ref(vref1, &inrange_start),
		   brl_num_to_ref(vref2, &inrange_end) );
	    if (outf != NULL) {
		fprintf( outf, "  [%d refs in range %s-%s]\n", select_count,
			brl_num_to_ref(vref1, &inrange_start),
			brl_num_to_ref(vref2, &inrange_end) );
	    }
	} else {
	    printf( "  [range is %s-%s]\n", 
		   brl_num_to_ref(vref1, &inrange_start),
		   brl_num_to_ref(vref2, &inrange_end) );
	    if (outf != NULL) {
		fprintf( outf, "  [range is %s-%s]\n", 
			brl_num_to_ref(vref1, &inrange_start),
			brl_num_to_ref(vref2, &inrange_end) );
	    }
	}
    }
} /* cmd_inrange */



cmd_list()
/*----------------------------------------------------------------------
|   NAME:
|       cmd_list
|
|   ALGORITHM:
|       List references found in previous search.
|       List is formatted using line_width.
|
|   HISTORY:
|       921217 cc Initial creation.
|       930105 cc Format output into lines.
|
\*----------------------------------------------------------------------*/

{
    char vref[REFSZ];		/* verse ref buffer */
    int  cnt;
    char lbuf[LINESZ];		/* line buffer */
    int  lcnt;			/* count of chars in line */
    int  i;

    if (select_count < 1) {
	printf( "No references.  Use '??word' first.\n" );
	return;
    }
    sprintf( lbuf, "  References [%d]: ", select_count );
    /* Support non-POSIX sprintf */
    lcnt = strlen(lbuf);

    for (cnt=0; cnt < select_count; cnt++) {
	brl_num_to_ref(vref, &selectbuf[cnt]);
	i = strlen(vref);
	if ((lcnt + i +1)> line_width) {
	    puts( lbuf );
	    if (outf != NULL) {
		fputs( lbuf, outf );
		putc( '\n', outf );
	    }
	    lbuf[0] = '\0'; lcnt = 0;
	} else {
	    strcat( lbuf, " " );
	    lcnt++;
	}
	strcat( lbuf, vref );
	lcnt += i;
    }
    puts( lbuf );
    if (outf != NULL) {
	fputs( lbuf, outf );
	putc( '\n', outf );
    }
} /* cmd_list */



cmd_pretty_print(verbose)
/*----------------------------------------------------------------------
|   NAME:
|       cmd_pretty_print
|
|   ALGORITHM:
|       Toggle pretty-printing modes
|
|   HISTORY:
|       921217 cc Initial creation.
|
\*----------------------------------------------------------------------*/

int verbose;
{
    char *s;
    
    if (pretty_printing = pretty_printing ? FALSE : TRUE) {
	if (line_width == 0) {
	    /*
	      This forces pretty_printing to be done with a
	      restricted line width.  If somebody doesn't like that,
	      they can use "-l999" or something.
	      */
	    if ((s=getenv("COLUMNS")) == NULL)
		line_width = 79;	/* Take a guess.  Oh well */
	    else {
		line_width = atoi(s) -1;
		if (line_width > LINESZ) line_width = LINESZ -1;
	    }
	}
    } else {
	/* If pretty_printing is being turned off, they probably don't
	   want line breaks either */
	line_width = 0;
    }
    if (verbose) {
	printf( "Output formatting %s\n", pretty_printing?"ON":"OFF" );
	if (outf != NULL)
	    fprintf( outf, "Output formatting %s\n", pretty_printing?"ON":"OFF" );
    }
} /* cmd_pretty_print */



cmd_view()
/*----------------------------------------------------------------------
|   NAME:
|       cmd_view
|
|   ALGORITHM:
|       View full text of refs found in previous search.
|
|   HISTORY:
|       921217 cc Initial creation.
|
\*----------------------------------------------------------------------*/

{
    char vref[REFSZ];
    int cnt;
    
    if (select_count < 1) {
	fprintf( stderr, "No references.  Use '??word' first.\n" );
	return;
    }
    printf( "Viewing References [%d]: \n", select_count );
    if (outf != NULL)
	fprintf( outf, "Viewing References [%d]: \n", select_count );

    for (cnt=0; cnt < select_count; cnt++) {
	/* Print refs without updating current context */
	brl_num_to_ref( vref, &selectbuf[cnt] );
	brl_printverse( vref, pretty_printing, line_width, outf );
    }
} /* cmd_view */



cmd_write( fname )
/*----------------------------------------------------------------------
|   NAME:
|       cmd_write
|
|   ALGORITHM:
|       Begin writing a copy of program output to a file.  If we
|       are already writing to a file, then close it, and open a
|       new one.  The file is opened in "append" mode.
|       
|         fname -- String with name of file.
|         
|       If fname is null then close current file (if any) and
|       stop writing. 
|
|   HISTORY:
|       930105 cc Initial creation.
|
\*----------------------------------------------------------------------*/

char *fname;
{
    if (outf != NULL) {
	fclose( outf );
	outf = NULL;
	printf( "%s: Output file closed.\n", myname );
    }
    if (*fname) {
	if ((outf = fopen( fname, "a" )) == NULL) {
	    fprintf( stderr, "%s: Cannot append to file '%s'\n",
		    myname, fname );
	}
	printf( "%s: Writing to %s.\n", myname, fname );
    }
} /* cmd_write */



do_command( cmd )
/*----------------------------------------------------------------------
|   NAME:
|       do_command
|
|   ALGORITHM:
|       Handle a control command.
|       For now, all control commands begin with "?".
|
|   HISTORY:
|       921217 cc Initial creation
|
\*----------------------------------------------------------------------*/

char *cmd;
{
    int n;
    char w[VSPECSZ];
    char *p;

    /* Convert to lower case */
    for (p=cmd; *p; p++) {
	if (isupper(*p)) *p = tolower(*p);
    }
    n=sscanf(cmd, "?%s", w);

    if ((n <= 0) ||
	/* HELP */
	(strcmp(w, "") == 0) ||
	(strcmp(w, "h") == 0) ||
	(strcmp(w, "help") == 0))
    {
	cmd_help();
    } else if (*w == '?') {
	/* Concordance (word search) */
	if (w[1] == '\0') {
	    /* Support "?? word" (with space) */
	    w[0] = '\0';	/* ensure correctness for plain "??" */
	    p = cmd + 2;
	    n=sscanf(p, "%s", w);
	    cmd_concordance( w );
	} else {
	    cmd_concordance( w+1 );
	}
    } else if ((strcmp(w, "a") == 0) ||
	       (strcmp(w, "and") == 0)) {
	/* "and" word search */
	p = cmd + strlen(w) +1;
	n=sscanf(p, "%s", w);
	cmd_ANDconcordance( w );
    } else if ((strcmp(w, "f") == 0)) {
	/* Toggle pretty-print modes */
	cmd_pretty_print(TRUE);
    } else if (strcmp(w, "in") == 0) {
	/* IN -- limit ref list to a verse range */
	p = cmd + strlen(w) +1;
	n=sscanf(p, "%s", w);
	cmd_inrange( w );
    } else if ((strcmp(w, "l") == 0) ||
	       (strcmp(w, "list") == 0)) {
	/* LIST refs from previous search */
	cmd_list();
    } else if ((strcmp(w, "o") == 0) ||
	       (strcmp(w, "or") == 0)) {
	/* "or" word search */
	p = cmd + strlen(w) +1;
	n=sscanf(p, "%s", w);
	cmd_ORconcordance( w );
    } else if ((strcmp(w, "v") == 0) ||
	       (strcmp(w, "view") == 0)) {
	/* VIEW text of refs from previous search */
	cmd_view();
    } else if (*w == 'w') {
	/* WRITE to a file */
	if (w[1] == '\0') {
	    /* Support "?w file" (with space) */
	    w[0] = '\0';	/* ensure correctness for plain "?w" */
	    p = cmd + 2;
	    n=sscanf(p, "%s", w);
	    cmd_write( w );
	} else {
	    /* Support "?wfile" (no space) */
	    cmd_write( w+1 );
	}
    } else if ((strcmp(w, "bye") == 0) ||
	       (strcmp(w, "exit") == 0) ||
	       (strcmp(w, "q") == 0) ||
	       (strcmp(w, "quit") == 0)) {
	/* Let's go home */
	brl_close();
	exit( 0 );
    } else {
	/* Hmmm... */
	fprintf( stderr, "%s: Unrecognized command '%s'\n", myname, w);
	cmd_help();
    }
} /* do_command */



user_input( cmd )
/*----------------------------------------------------------------------
|   NAME:
|       user_input
|
|   ALGORITHM:
|       Process a user command.
|       Commands could be:
|          1) A Bible verse spec, e.g. "Jn3:16-18", or even "3"
|             (context sensitive).
|          2) A control command, e.g. "?"
|
|   HISTORY:
|       921217 cc Initial creation.
|
\*----------------------------------------------------------------------*/

char *cmd;
{
    char vs[REFSZ];
    int  b, c, v;	/* Junk variables */
    
    get_nonblank(cmd);
	
    /* Is it a control command?
       For now, all control commands start with "?". */
    if (*cmd == '?') {
	/* Control Command */
	do_command( cmd );
    } else if (*cmd == '>' || *cmd == '<' || *cmd == '\0') {
	if (*cmd == '>') skip_inc = 1;
	if (*cmd == '<') skip_inc = -1;

	/* Print next verse and update context. */
	brl_cur_vnum += skip_inc;
	brl_printverse( brl_num_to_ref(vs, &brl_cur_vnum),
		       pretty_printing, line_width, outf );
    } else {
	/* Verse Spec.
	   Print it and update context.
	 */
	brl_cur_vnum = brl_printverse( cmd, pretty_printing, line_width, outf );
    }
} /* user_input */



prompter()
/*----------------------------------------------------------------------
|   NAME:
|       prompter
|
|   ALGORITHM:
|       Display prompt for interactive user.
|
|   HISTORY:
|       921224 cc Initial creation.
|
\*----------------------------------------------------------------------*/

{
    char vbuf[REFSZ];
    
    printf( "%s(%s) [%s]%s ", myname,
	   brl_textname, brl_num_to_ref(vbuf, &brl_cur_vnum),
	   (skip_inc > 0 ? ">" : "<") );	
    fflush( stdout );
} /* prompter */



usage()
/*----------------------------------------------------------------------
|   NAME:
|       usage
|
|   ALGORITHM:
|       Print usage message to stderr, then exit program.
|
|   HISTORY:
|       890830 cc Created.
|       890912 cc Added -f, -l.
|       891002 cc Tinkered with help text.
|
\*----------------------------------------------------------------------*/

{
    fprintf( stderr, "Usage: %s [-f][-l[cols]][-m mem][-p path][-d file][<verse spec>...]\n",
	    myname );
    fprintf( stderr, "\n\
    -d file  Override default datafile name\n\
    -f       Format the output (implies -l)\n\
    -l[cols] Set line width (default value: COLUMNS env. variable)\n\
    -m mem   Specify maximum buffer memory usage\n\
             in Kbytes.  Defaults to 1024K.\n\
    -p path  Override default datafile search path.\n\
\n" );
    exit(1);
} /* usage */



main(argc, argv)
/*----------------------------------------------------------------------
|   NAME:
|       main
|
|   ALGORITHM:
|       Main Program.
|       
|       Handle command line options.
|       Initialize the Bible Retrieval Library.
|       Read user commands from the command line or from stdin.
|
|   HISTORY:
|       890830 cc Added options processing.
|       890908 cc Send whole lines to printverse instead of
|       	partially parsing them.
|       890912 cc Added -f, -l options.
|
\*----------------------------------------------------------------------*/

int argc;
char **argv;
{
  char line[VSPECSZ];
  char *dfname, *dfpath, *s;
  char ch;

  mem_limit = 1024;		/* Default 1024K of buffer space */
  dfname = dfpath = NULL;	/* Use library's default values */
    
  myname = s = *argv++; argc--;	/* Program name */
  while (*s) {
      if (*s == '/') myname = s+1;
      s++;
  }

  cmd_pretty_print(FALSE);	/* Kind of hokey -- this calls turns it on */
  
#define ARGVAL()  (*++(*argv) || (--argc && *++argv))
  for (;argc && **argv == '-'; argc--, argv++) {
      /* Got an option flag */
      while (*++(*argv)) {
	  /* Process all flags in this argument */
	  switch (**argv) {
	    case 'd':
	      if (!ARGVAL()) {
		  fprintf( stderr, "%s: -d Missing datafile-name\n", myname );
		  usage();
	      }
	      dfname = *argv;
	      goto nextarg;
	    case 'f':
	      cmd_pretty_print(FALSE);
	      break;
	    case 'l':
	      if (isdigit(*(*argv+1))) {
		  line_width = atoi(++(*argv));
		  if (line_width > LINESZ) line_width = LINESZ -1;
		  goto nextarg;
	      } else if (--argc
			 && isdigit(*(argv[1]))
			 && ((ch= *(argv[1]+1)) == '\0' || isdigit(ch))) {
		  line_width = atoi(*++argv);
		  if (line_width > LINESZ) line_width = LINESZ -1;
		  goto nextarg;
	      } else {
		  argc++;	/* hack-alert!  Fix error from above */
		  /* Set line width to COLUMNS-1.  Avoids auto-newline. */
		  if ((s=getenv("COLUMNS")) == NULL)
		      line_width = 79;	/* Take a guess.  Oh well */
		  else {
		      line_width = atoi(s) -1;
		      if (line_width > LINESZ) line_width = LINESZ -1;
		  }
	      }
	      break;
	    case 'm':
	      if (!ARGVAL() || !isdigit(**argv)) {
		  fprintf(stderr, "%s: -m Missing memory-limit\n", myname);
		  usage();
	      }
	      mem_limit = atoi(*argv);
	      goto nextarg;
	    case 'p':
	      if (!ARGVAL()) {
		  fprintf( stderr, "%s: -p Missing path-list\n", myname );
		  usage();
	      }
	      dfpath = *argv;
	      goto nextarg;
	    default:
	      fprintf(stderr, "%s: Unknown flag: '%c'\n", myname, **argv);
	      usage();
	  } /* switch */
      } /* while */
    nextarg: continue;
  } /* for */
  
  brl_init( dfname, dfpath, mem_limit );   /* Initialize Bible Retrieval Lib */

  if (argc) {
      /* read from command line */
      list_always = TRUE;
      while (argc--) {
	  user_input( *argv++ );
      }
  } else {
      /* read from stdin */
      printf( "%s: %s\n", myname, release_version );
      printf( "Hit '?' for help.\n" );
      brl_printverse( "Gen1:1", pretty_printing, line_width, outf );
      prompter();
      while(fgets(line, VSPECSZ, stdin) != NULL) {
	  user_input( line );
	  prompter();
      }
      printf( "\n" );
  }
  
  brl_close();
} /* main */
