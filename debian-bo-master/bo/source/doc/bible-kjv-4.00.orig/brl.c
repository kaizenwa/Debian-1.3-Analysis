/* -*-C-*-
*******************************************************************************
*
* File:         brl.c
* RCS:          $Header: brl.c,v 1.16 93/04/26 11:18:12 chip Exp $
* Description:  Bible Retrieval Library
* Author:       Chip Chapin, Hewlett Packard Company
* Created:      Jan 14 1989, Chip Chapin
* Modified:     Mon Apr 26 11:11:59 1993 (Chip Chapin) chip@hpclbis
* Language:     C
* Package:      Bible Retrieval System
* Status:       Experimental (Do Not Distribute)
*
*******************************************************************************
*
* Revisions:
*
* Thu Apr 22 14:39:43 1993 (Chip Chapin) chip@hpclbis
*  Fix bad return value from brl_printverse when no errors occurred.
*  Support non-POSIX sprintf.
* Tue Jan  5 18:23:41 1993 (Chip Chapin) chip@hpclbis
*  Change how current context is handled.  Now use single var
*  "brl_cur_vnum", and make our caller responsible for updating it.
*  brl_getverse is now commented out.  Nobody uses it anymore.
*  Added file parameter to brl_printverse.
* Mon Jan  4 10:11:19 1993 (Chip Chapin) chip@hpclbis
*  Renamed verse_spec() as brl_verse_spec() and exposed it to callers.
* Thu Dec 24 11:04:42 1992 (Chip Chapin) chip@hpclbis
*  Added brl_cur_ref(), and converted all absolute verse refs to use
*  "ref_t", instead of "int" and "long".
* Tue Dec 22 17:11:41 1992 (Chip Chapin) chip@hpclbis
*  Fix bug parsing "ma.." books.
*  Added tolower fix submitted by Stephen North, AT&T.
* Mon Dec 21 19:13:10 1992 (Chip Chapin) chip@hpclbis
*  Fixed minor verse ref parsing bugs.
*  Added brl_num_to_ref to support concordance.
*******************************************************************************
*
* $Log:	brl.c,v $
 * Revision 1.16  93/04/26  11:18:12  11:18:12  chip (Chip Chapin)
 * Release 4.00
 * Public release of portable datafile version.
 * 
 * Revision 1.15  93/04/23  13:08:01  13:08:01  chip (Chip Chapin)
 * PORTABILITY RELEASE
 * This version supports portable data files, usable on machines with
 * differing native byte-orders.
 * Also, this version compiles and runs on non-HPUX systems.  It has been
 * tested on SunOS 4.? and ULTRIX 4.?, using SPARC and DEC 3100 hardware
 * respectively.  Note that the data file format has rolled again.
 * 
 * Revision 1.14  93/01/05  19:05:30  19:05:30  chip (Chip Chapin)
 * Release 3.00: (not for distribution)
 * Fixed errors (blank lines) in bible.data file.  Data file is not compatible
 * with previous (1.x and 2.x) distributions.  Further changes pending.
 * Rewrote context handling, and added "<" and ">" commands.
 * Tools for building brl-index are now part of release.
 * 
 * Revision 1.13  93/01/05  10:49:09  10:49:09  chip (Chip Chapin)
 * Release 2.2, Added ?w command and line formatting to ?l output.
 * 
 * Revision 1.12  93/01/04  16:20:57  16:20:57  chip (Chip Chapin)
 * Release 2.1, implements ?in and ?or commands.
 * 
 * Revision 1.11  92/12/24  11:09:14  11:09:14  chip (Chip Chapin)
 * Release 2.04.  Include verse ref in prompt line.
 * 
 * Revision 1.10  92/12/23  14:10:43  14:10:43  chip (Chip Chapin)
 * Release 2.03: minor tweaks and bug fixes.
 * 
 * Revision 1.9  92/12/22  18:17:06  18:17:06  chip (Chip Chapin)
 * Minor tweaks for release 2.02.
 * 
 * Revision 1.8  92/12/21  20:00:47  20:00:47  chip (Chip Chapin)
 * Release 2.0.  This release adds the concordance, and some small fixes.
 * 
 * Revision 1.7  89/10/02  22:20:45  22:20:45  chip (Chip Chapin)
 * Fix bugs: looping when lwidth very small,
 * and blank-line after verse range not printing verse AFTER the last
 * verse of the range.
 * 
 * Revision 1.6  89/09/14  20:33:45  20:33:45  chip (Chip Chapin)
 * Release 1-2.  Supports -f and -l options for formatting the output.
 * Updates primarily brl.c, bible.c, and bible.1.
 * 
 * Revision 1.5  89/09/13  21:49:13  21:49:13  chip (Chip Chapin)
 * Implement -f and -l options for pretty-printing and linewidth limitation.
 * 
 * Revision 1.4  89/09/08  13:22:47  13:22:47  chip (Chip Chapin)
 * Better error checking on verse syntax; automatic test suite.
 * 
 * Revision 1.3  89/09/08  09:01:22  09:01:22  chip (Chip Chapin)
 * Bug fix and simplification: send whole input lines or arguments to BRL,
 * and let BRL worry about multiple references.  We don't care.
 * 
 * Revision 1.2  89/09/05  20:22:59  20:22:59  chip (Chip Chapin)
 * Workaround 6.2 C compiler enum gripes.
 * 
 * Revision 1.1  89/09/05  17:49:15  17:49:15  chip (Chip Chapin)
 * Initial revision
 * 
*******************************************************************************
*/

/*----------------------------------------------------------------------
|   NAME:
|       brl.c
|
|   PURPOSE:
|       Provide a set of routines for retrieving Bible text from
|       the Text Storage Library (tsl).  All of these routines
|       should take a high-level view of the text itself; they may
|       know something about the structure of the Bible, but
|       should *not* depend in any way upon the manner in which
|       the text is stored.  Call on the TSL for that.
|
|   FUNCTIONS:
|       Grammar Functions: Used within the BRL to parse verse
|       specifications.
|       	brl_verse_spec (also called from applications)
|       	verse_id
|       	verse_continuation
|       	
|       Access Functions: Called from application programs to
|       retrieve Bible text.
|		brl_getverse
|		brl_printverse
|       
|       Utility Functions: Called from application programs.
|       	brl_init
|       	brl_close
|		brl_num_to_ref
|
|   HISTORY:
|       890114 cc Initial creation.
|	890824 cc Improved partitioning between BRL and TSL.  Created
|		brl_getverse to hide brl_verse_spec and verse numbers from
|		user programs.  Fix bug in verse_id parsing "jn".
|	921217 cc Added brl_num_to_ref.
|
\*----------------------------------------------------------------------*/


#include <stdio.h>
#include <ctype.h>
#include "tsl.h"
#include "brl.h"

#define FALSE	(0)

static char rcs_ident[]="@(#)$Header: brl.c,v 1.16 93/04/26 11:18:12 chip Exp $";

char *booknamestr[] = { "Genesis",
			   "Exodus",
			   "Leviticus",
			   "Numbers",
			   "Deuteronomy",
			   "Joshua",
			   "Judges",
			   "Ruth",
			   "1 Samuel",
			   "2 Samuel",
			   "1 Kings",
			   "2 Kings",
			   "1 Chronicles",
			   "2 Chronicles",
			   "Ezra",
			   "Nehemiah",
			   "Esther",
			   "Job",
			   "Psalms",
			   "Proverbs",
			   "Ecclesiastes",
			   "Song of Solomon",
			   "Isaiah",
			   "Jeremiah",
			   "Lamentations",
			   "Ezekiel",
			   "Daniel",
			   "Hosea",
			   "Joel",
			   "Amos",
			   "Obadiah",
			   "Jonah",
			   "Micah",
			   "Nahum",
			   "Habakkuk",
			   "Zephaniah",
			   "Haggai",
			   "Zechariah",
			   "Malachi",
			   "Matthew",
			   "Mark",
			   "Luke",
			   "John",
			   "Acts",
			   "Romans",
			   "1 Corinthians",
			   "2 Corinthians",
			   "Galatians",
			   "Ephesians",
			   "Philippians",
			   "Colossians",
			   "1 Thessalonians",
			   "2 Thessalonians",
			   "1 Timothy",
			   "2 Timothy",
			   "Titus",
			   "Philemon",
			   "Hebrews",
			   "James",
			   "1 Peter",
			   "2 Peter",
			   "1 John",
			   "2 John",
			   "3 John",
			   "Jude",
			   "Revelation",
			   "BAD"
};

/* Standard abbreviations */
char *bookabbrvstr[] = { "Gen",
			   "Ex",
			   "Lev",
			   "Num",
			   "Dt",
			   "Jsh",
			   "Jdg",
			   "Ru",
			   "1Sa",
			   "2Sa",
			   "1Ki",
			   "2Ki",
			   "1Ch",
			   "2Ch",
			   "Ezr",
			   "Ne",
			   "Es",
			   "Job",
			   "Ps",
			   "Pr",
			   "Ec",
			   "SoS",
			   "Is",
			   "Je",
			   "Lam",
			   "Ezk",
			   "Da",
			   "Ho",
			   "Jl",
			   "Am",
			   "Ob",
			   "Jon",
			   "Mi",
			   "Na",
			   "Hab",
			   "Zep",
			   "Hag",
			   "Zec",
			   "Mal",
			   "Mt",
			   "Mk",
			   "Lu",
			   "Jn",
			   "Ac",
			   "Ro",
			   "1Co",
			   "2Co",
			   "Ga",
			   "Ep",
			   "Php",
			   "Co",
			   "1Th",
			   "2Th",
			   "1Ti",
			   "2Ti",
			   "Ti",
			   "Phm",
			   "He",
			   "Ja",
			   "1Pe",
			   "2Pe",
			   "1Jn",
			   "2Jn",
			   "3Jn",
			   "Jde",
			   "Re",
			   "BAD"
};


ref_t brl_cur_vnum;		/* Current context. */

/* Name of text.  This should really be initialized or provided by tsl */
char brl_textname[]="KJV";


ref_t verse_id( );
ref_t verse_num( );


/*----------------------------------------------------------------------
|
|       Verse Specification Grammar
|       
|       The following routines implement the grammar by which
|       verses may be specified.
|       See each routine, starting with brl_verse_spec, for the grammar.
|
\*----------------------------------------------------------------------*/

#define getnum(s,n)	n=0; get_nonblank(s); \
    while (isdigit(*s)) n = 10 * n + (*s++ - '0');



int get_book( s, book )
/*----------------------------------------------------------------------
|   NAME:
|       get_book
|
|   ALGORITHM:
|       Parse a bible book name or abbreviation.  Defaults to the
|       book number passed in "book" if no book is specified.
|       
|       Returns the number of the book, 0..65, -1 if an
|       unrecognizable book name is given, or -2 if no book name
|       is given (needs a bit of cleanup!).
|
|   HISTORY:
|       890902 cc Extracted from verse_id.
|       890904 cc Revised to allow non-fatal errors.
|
\*----------------------------------------------------------------------*/

char **s;
int book;
{
    int c;
    char *s1;

    book = -2;		/* assume no book is given */
    get_nonblank(*s);
    s1 = *s;
    if (*s1 && (isalpha(*s1) || isalpha(*(s1+1)))) {
	switch (c = *s1++) {
	  case '1':	/* 1Sa, 1Ki, 1Ch, 1Co, 1Th, 1Ti, 1Pe, 1Jn */
	  case '2':	/* 2Sa, 2Ki, 2Ch, 2Co, 2Th, 2Ti, 2Pe, 2Jn */
	  case '3':	/* 3Jn */
	    get_nonblank(s1);
	    switch (*s1++) {
	      case 'c': 
		if (*s1=='h') book= (int)CHRON1;
		else if (*s1=='o') book= (int)COR1;
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
		break;
	      case 'j':
		book= (int)JOHN1;
		break;
	      case 'k':
		book= (int)KINGS1;
		break;
	      case 'p':
		book= (int)PET1;
		break;
	      case 's':
		book= (int)SAM1;
		break;
	      case 't':
		if (*s1=='h') book= (int)THESS1;
		else if (*s1=='i') book= (int)TIM1;
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
		break;
	      default:
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    if (c != '1') book++;
	    if (c == '3') 
		if (book == (int)JOHN2) book++;
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    break;
	  case 'a': /* amos, acts */
	    c = *s1++;
	    if (c == 'm') book = (int)AMOS;
	    else if (c == 'c') book = (int)ACTS;
	    else {
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    break;
	  case 'c': /* col */
	    book = (int)COLOS;
	    break;
	  case 'd': /* deut, dan */
	    c = *s1++;
	    if (c == 'e' || c == 't') book = (int)DEUT; /* de, dt */
	    else if (c == 'a') book = (int)DANIEL;
	    else {
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    break;
	  case 'e': /* exodus, ezra, esther, eccl, ezekiel, eph */
	    c = *s1++;
	    if (c == 'x') book = (int)EXODUS;	/* ex */
	    else if (c == 's') book = (int)ESTHER;	/* es */
	    else if (c == 'c') book = (int)ECCL;	/* ec */
	    else if (c == 'p') book = (int)EPH;	/* ep */
	    else {
		if (c == 'z') c = *s1++;
		if (c == 'r') book = (int)EZRA;	/* ezr, er */
		else if (c == 'e' || c == 'k')
		    book = (int)EZEKIEL; /* eze,ee,ezk,ek */
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    }
	    break;
	  case 'g': /* gen, gal */
	    c = *s1++;
	    if (c == 'e' || c == 'n') book = (int)GENESIS;
	    else if (c == 'a' || c == 'l') book = (int)GAL;
	    else {
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    break;
	  case 'h': /* hebrews, hosea, habakkuk, haggai */
	    c = *s1++;
	    if (c == 'o') book = (int)HOSEA;		/* ho */
	    else if (c == 'e' || c == 'b')
		book = (int)HEBREWS;	/* he, hb (more likely than Hab.) */
	    else {
		if (c == 'a') c = *s1++;
		if (c == 'b') book = (int)HABAK;	/* hab */
		else if (c == 'g') book = (int)HAGGAI; /* hag, hg */
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    }
	    break;
	  case 'i': /* isaiah */
	    book = (int)ISAIAH;
	    break;
	  case 'j': /* josh, judges, job, jer, joel, jonah, john, jam, jude */
	    c = *s1++;
	    if (c == 'a') book = (int)JAMES;		/* ja */
	    else if (c == 'e' || c == 'r') book = (int)JEREM;	/* je, jr */
	    else if (c == 'b') book = (int)JOB;	/* jb */
	    else if (c == 'd') {
		if ((c = *s1++) == 'g') book = (int)JUDGES;	/* jdg */
		else if (c == 'e') book = (int)JUDE;	/* jde */
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    }
	    else if (c == 'g') book = (int)JUDGES;	/* jg */
	    else if (c == 'l') book = (int)JOEL;	/* jl */
	    else if (c == 'n') book = (int)JOHN;	/* jn */
	    else if (c == 's') {
		if ((c = *s1++) == 'h') book = (int)JOSHUA;	/* jsh */
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    }
	    else if (c == 'u') {
		if ((c = *s1++) == 'd') c = *s1++;
		if (c == 'g') book = (int)JUDGES;	/* judg, jug (ha-ha) */
		else if (c == 'e') book = (int)JUDE;	/* jude, jue (ha-ha) */
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    }
	    else if (c == 'o') {
		if ((c = *s1++) == 's') book = (int)JOSHUA;	/* jos */
		else if (c == 'b') book = (int)JOB;		/* job */
		else if (c == 'e') book = (int)JOEL;		/* joe */
		else if (c == 'n') book = (int)JONAH;	        /* jon */
		else if (c == 'h') book = (int)JOHN;		/* joh */
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    }
	    else {
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    break;
	  case 'l': /* lev, lam, luke */
	    c = *s1++;
	    if (c == 'e' || c == 'v') book = (int)LEVIT;
	    else if (c == 'a' || c == 'm') book = (int)LAMENT;
	    else if (c == 'u' || c == 'k') book = (int)LUKE;
	    else {
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    break;
	  case 'm': /* micah, malachi, matt, mark */
	    c = *s1++;
	    if (c == 'i' || c == 'c') book = (int)MICAH;
	    else {
		/* mal, ml, mat, mt, mar, mr, mk (and "mak"!) */
		if (c == 'a') c = *s1++;
		if (c == 'l') book = (int)MALACHI;
		else if (c == 't') book = (int)MATT;
		else if (c == 'r' || c == 'k') book = (int)MARK;
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    }
	    break;
	  case 'n': /* num, neh, nahum */
	    c = *s1++;
	    if (c == 'u' || c == 'm') book = (int)NUM;
	    else if (c == 'e') book = (int)NEHEM;
	    else if (c == 'a') book = (int)NAHUM;
	    else {
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    break;
	  case 'o': /* obadiah */
	    book = (int)OBADIAH;
	    break;
	  case 'p': /* psalms, proverbs, philippians, philemon */
	    c = *s1++;
	    if (c == 's') book = (int)PSALMS;
	    else if (c == 'r') book = (int)PROV;
	    else {
		if (c == 'h') c = *s1++;
		if (c == 'i') c = *s1++;
		if (c == 'l') c = *s1++;
		if (c == 'i' || c == 'p' || !isalpha(c)) {
		    /* phili, philp, phil, phi, ph, php, pp, etc. */
		    /* This gives preference to Philippians over Philemon */
		    book = (int)PHILIP;
		} else if (c == 'e' || c == 'm') book = (int)PHILEM;
		else {
		    tsl_error( FALSE, BADBOOK, *s );
		    return -1;
		}
	    }
	    break;
	  case 'q': /* Not a book, but it makes typing easier */
	    brl_close();
	    exit( 0 );
	    break;
	  case 'r': /* ruth, romans, revelation */
	    c = *s1++;
	    if (c == 'u' || c == 't') book = (int)RUTH;
	    else if (c == 'o' || c == 'm') book = (int)ROMANS;
	    else if (c == 'e' || c == 'v') book = (int)REV;
	    else {
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    break;
	  case 's': /* song */
	    book = (int)SONG;
	    break;
	  case 't': /* titus */
	    book = (int)TITUS;
	    break;
	  case 'z': /* zeph, zech */
	    if ((c = *s1++) == 'e') c = *s1++;
	    if (c == 'p') book = (int)ZEPH;
	    else if (c == 'c') book = (int)ZECH;
	    else {
		tsl_error( FALSE, BADBOOK, *s );
		return -1;
	    }
	    break;
	  default:
	    tsl_error( FALSE, BADBOOK, *s );
	    return -1;
	} /* switch on first letter of book name */
	
	/* Skip any remaining letters in book name */
	while (isalpha(*s1)) s1++;
	get_nonblank(s1);
    }
    *s = s1;
    return book;
} /* get_book */



int verse_continuation( s, book, chapter, absverse )
/*----------------------------------------------------------------------
|   NAME:
|       verse_continuation
|
|   ALGORITHM:
|       <verse continuation> ::= <null>
|                            ::= - <verse id>
|       		     
|       "s" is pointing to the first char of the <verse
|       continuation>.  If it is a null continuation we leave it
|       where it is, otherwise on exit we update it to point to
|       the first non-blank after the continuation.
|       	
|       book     -- Number of the book in which the verse spec began.
|       chapter  -- Number of the chapter ...
|       absverse -- ABSOLUTE VERSE NUMBER where verse spec began
|       	(this is more useful than the relative num).
|       	
|       Returns the total number of verses to be fetched, unless
|       there was a problem.  Returns zero if there was a problem. 
|       	
|
|   HISTORY:
|       890829 cc Initial implementation (formerly always returned 1). 
|       890908 cc Error return.
|
\*----------------------------------------------------------------------*/

char **s;
int book, chapter;
ref_t absverse;
{
    int	n;
    int verse;
    char *s1;

    s1 = *s;	/* For easier handling.  Be sure to update it */
    n = 1;	/* Assume NO following verses */

    if (*s1++ == '-') {
	/* yup, there's a continuation */
	if (verse_id( &s1, &book, &chapter, &verse ) == 0)
	    return 0;
	*s = s1;

	/* Determine absolute verse number of ending verse */
	n = verse_num( book, chapter, verse );

	/* how many verses is that? */
	n = n - absverse +1;
    }
    
    return n;
} /* verse_continuation */



ref_t verse_id( s, bookp, chapterp, versep )
/*----------------------------------------------------------------------
|   NAME:
|       verse_id
|
|   ALGORITHM:
|       <verse id>  ::= <book> <chapter> : <verse>
|       	    ::= <book> <chapter>	# implies vs. 1
|           	    ::= <book> 			# implies 1:1
|       	    ::= <chapter> : <verse>	# implies current book
|       	    ::= <verse>			# implies current book/chapter
|       
|       "s" is pointing to the first character of the <verse id>.
|       On exit, we update it to be pointing to the next non-blank
|       after what we've parsed.
|       
|       The values of *bookp, and *chapterp are used as
|       defaults in case of a partial verse spec.  For example,
|       "15" would mean verse 15 in the default book/chapter.
|           
|       We return an absolute verse number (Gen 1:1 == 1) for the
|       referenced verse.  Also update *bookp, *chapterp*, and
|       *versep with the book, chapter, and verse numbers.
|       
|       If a problem occurs, we return 0.
|
|   HISTORY:
|       890902 cc Revised to handle incomplete verse specs.
|       890908 cc Fix to blank line handling.  Revised error return.
|
\*----------------------------------------------------------------------*/

char **s;
int *bookp, *chapterp, *versep;
{
    short book, chapter, verse;
    int   num;
    ref_t vn;
    char  *s1;

    s1 = *s;		/* For easier handling.  Be sure to update it */
    if ((book = get_book( &s1, *bookp )) == -1) return 0;
    getnum(s1, num);
    if (num < 1) {
	/* <verse spec> ::= <book> */
	if (book < 0) {
	    /* Special case: if this is a blank line, then print
	       the very next verse.  But if there's still junk on
	       the line, then what we have here is an error.
	       */
	    if (*s1) {
		/* Bad news, gang */
		tsl_error( FALSE, "Extra garbage on line: '%s'", s1 );
		return 0;
	    }

	    /* Get the last verse printed, increment it,
	       and translate that back into book/chapter/verse,
	       while making sure it stays in the proper range.
	       */
	    vn = verse_num( *bookp, *chapterp, *versep );
	    vn++;
	    vn = brl_extract_num( vn, bookp, chapterp, versep );
	    *s = s1;
	    return( vn );
	} else {
	    chapter = 1;
	    verse   = 1;
	}
    } else {
	get_nonblank(s1);
	if (*s1 != ':') {
	    if (book < 0) {
		/* <verse spec> ::= <verse> */
		book    = *bookp;
		chapter = *chapterp;
		verse   = num;
	    } else {
		/* <verse spec> ::= <book> <chapter> */
		chapter = num;
		verse   = 1;
	    }
	} else {
	    /* <verse spec> ::= <book> <chapter> : <verse> */
	    /* <verse spec> ::= <chapter> : <verse> */
	    s1++;	/* skip past the ':' */
	    if (book < 0)
		book = *bookp; 		/* default book */
	    chapter = num;
	    getnum(s1, verse);
	    if (verse < 1) {
		tsl_error( FALSE, NO_VERSE, *s );
	    } else {
		get_nonblank(s1);
	    }
	}
    }

    *s = s1;		/* Update the pointer we were passed */
    *bookp    = book;
    *chapterp = chapter;
    *versep   = verse;
    /* return absolute verse number */
    return( verse_num( book, chapter, verse ) );
} /* verse_id */



ref_t brl_extract_num( absverse, bp, cp, vp )
/*----------------------------------------------------------------------
|   NAME:
|       brl_extract_num
|
|   ALGORITHM:
|       Extract the book, chapter, [relative] verse corresponding
|       to the absolute verse number passed in "absverse", if
|       "absverse" is valid.  If it is not valid, then coerce it
|       into something reasonable.  Update book, chapter, and
|       verse through their pointers "bp", "cp", and "vp",
|       respectively.  Return the [possibly changed] value of
|       absverse. 
|
|   HISTORY:
|       890904 cc Created (in a hurry)
|
\*----------------------------------------------------------------------*/

ref_t absverse;
int *bp, *cp, *vp;
{
    int bk, chp;

    if (absverse < 1)
	absverse = 1;	/* cheap insurance */
    
    for (bk= (int)GENESIS; bk <= (int)REV; bk++)
	if (absverse <= start_verse[start_chapter[bk+1]+1])
	    /* we've got the right book */
	    for (chp=start_chapter[bk]+1; chp <= start_chapter[bk+1]; chp++)
		if (absverse <= start_verse[chp+1]) {
		    /* we've got the right chapter */
		    *bp = bk;
		    *cp = chp - start_chapter[bk];
		    *vp = absverse - start_verse[chp];
		    return absverse;
		}
    /* if we got here, then things are messed up.
     Assume that the verse is off the back.
     */
    *bp = (int)REV;
    *cp = start_chapter[ (int)REV+1 ] - start_chapter[(int)REV];
    *vp = start_verse[(*cp)+1] - start_verse[*cp];
    return verse_num( *bp, *cp, *vp );
} /* brl_extract_num */



ref_t verse_num(b, c, v)
/*----------------------------------------------------------------------
|   NAME:
|       verse_num
|
|   ALGORITHM:
|       Return the absolute verse number, given the book, chapter
|       and [relative] verse number.
|       
|       Look up the starting absolute verse number of the
|       specified book/chapter, using the "start_verse" and
|       "start_chapter" tables, then add the [relative] verse
|       number. 
|       
|       Error checking: Ensure chapter is not too big for the
|       book, and that verse is not too big for the chapter.
|
|   HISTORY:
|       890830 cc Created.  The original lookup didn't do any
|       	error checking.
|
\*----------------------------------------------------------------------*/

int b, c, v;
{
    int abschapter;
    ref_t absverse;

    /* force book into proper range */
    if (b < (int)GENESIS) b = (int)GENESIS;
    else if (b > (int)REV) b = (int)REV;
    
    if ((abschapter = start_chapter[b] + c) > start_chapter[b+1])
	abschapter = start_chapter[b+1];
    if ((absverse = start_verse[abschapter] + v) > start_verse[abschapter+1])
	absverse = start_verse[abschapter+1];
    return absverse;
} /* verse_num */



ref_t brl_verse_spec( s, n )
/*----------------------------------------------------------------------
|   NAME:
|       brl_verse_spec
|
|   ALGORITHM:
|          
|           <verse spec> ::= <verse id> <verse continuation>
|       
|       Translate a <verse specifier> string s into a starting
|       absolute verse number, returned as the function result,
|       and a count n of verses to read.  Gen. 1:1 == verse 1.
|       
|       Returns 0 if something went wrong, leaving n undefined.
|
|   HISTORY:
|
\*----------------------------------------------------------------------*/

char	**s;
int	*n;
{
    ref_t av;
    char *s1;
    int book, chapter, verse;

    /* Munge off leading spaces,
       convert string to lower case,
       convert commas into blanks, and
       dispose of possible yucky '\n'.
     */
    get_nonblank(*s);
    s1 = *s;
    while (*s1) {
	if (isupper(*s1)) *s1 = tolower(*s1);
	if (*s1 == ',')
	    *s1 = ' ';
	else if (*s1 == '\n')
	    *s1 = '\0';
	s1++;
    }

    brl_cur_vnum = brl_extract_num( brl_cur_vnum, &book, &chapter, &verse );
    if ((av = verse_id( &(*s), &book, &chapter, &verse )) == 0)
	return 0;
    if (**s) {
	if ((*n = verse_continuation( &(*s), book, chapter, av )) == 0)
	    return 0;
    } else {
	*n = 1;
    }
    return av;
} /* brl_verse_spec */



#if 0 /* COMMENTED OUT -- NOT USED ANYMORE */
int brl_getverse( vs, vb, vbsize, pretty, lwidth )
/*----------------------------------------------------------------------
|   NAME:
|       brl_getverse
|
|   ALGORITHM:
|       Stuff buffer "vb" with text of verses specified by string
|       "vs", a <verse spec>.
|       
|       "pretty" and "lwidth" are formatting options passed to
|       tsl_gettext.  (NOT IMPLEMENTED!)
|
|   HISTORY:
|       890824 cc Created to hide internal functions
|       	tsl_gettext and brl_verse_spec from user programs.
|       890912 cc Added pretty and lwidth.
|
\*----------------------------------------------------------------------*/

char *vs;
char *vb;
int  vbsize;
int  pretty, lwidth;
{
    ref_t vn;
    int vc, bytecount;

    bytecount = 0;
    while (*vs && (vn = brl_verse_spec( &vs, &vc )))
	   bytecount += tsl_gettext( vn, vc, vb, vbsize );

    return bytecount;
} /* brl_getverse */
#endif


ref_t brl_printverse( vs, pretty, lwidth, outf )
/*----------------------------------------------------------------------
|   NAME:
|       brl_printverse
|
|   ALGORITHM:
|       Print text to stdout of verses specified by string "vs".
|       
|       pretty -- If true, then we want special output formatting.
|       lwidth -- If non-zero, then insert new-lines if necessary
|       	between words to prevent lines from being longer
|       	than lwidth.
|       outf   -- If non-NULL, then copy output to this file in
|           	addition to stdout.
|
|       Returns absolute verse number of last verse printed.
|
|   HISTORY:
|       890902 cc Created as alternative to brl_getverse.
|       890912 cc Added pretty and lwidth.
|       921221 cc Print bookname with chapter.
|       921223 cc Print chapter heading anytime v.1 is printed.
|       930105 cc Added outf functionality & return verse number.
|       930422 cc Return correct versenum when errors occur.
|
\*----------------------------------------------------------------------*/

char *vs;
int  pretty;
int  lwidth;
FILE *outf;
{
    ref_t vn, vref;
    int vc, vcount;
    int len, indent, i;
#define VBSIZE 512
    char vb1[VBSIZE], vb2[VBSIZE];
    char *srcp, *dstp, *endp;
    int  curbook, curchapter, curverse;
    int  oldbook, oldchapter, oldverse;

    if (pretty || lwidth) {
	/* Get current context info to help with pretty printing */
	brl_cur_vnum = brl_extract_num( brl_cur_vnum,
				       &oldbook, &oldchapter, &oldverse );

	/* Process all verse specs that we're given */
	while (*vs && (vref = brl_verse_spec( &vs, &vcount ))) {
	    brl_cur_vnum = vref +vcount -1; /* Update context.
					       Won't be used unless next loop
					       gets an error. */
	    for (vc=vcount, vn=vref; vc; vc--) {
		/* get text for a single verse */
		len=tsl_gettext( vn++, 1, vb1, VBSIZE );
		srcp = vb1;
		dstp = vb2;
		indent = 0;
		if (pretty) {
		    brl_extract_num( vn-1, &curbook, &curchapter, &curverse );
		    /* print book/chapter heading?? */
		    if (curbook != oldbook ||
			curchapter != oldchapter ||
			curverse == 1) {
			/* print chapter heading */
			sprintf( dstp, "\n%s %d\n\n",
				 booknamestr[curbook], curchapter );
			/* Advance dstp (supports non-POSIX sprintf) */
			while (*dstp) dstp++;
		    }
		    oldbook = curbook;
		    oldchapter = curchapter;
		    oldverse = curverse;
		    
		    /* advance to the verse number in source */
		    while (*srcp != ':') srcp++;
		    srcp++;

		    /* insert indentation in dest */
		    indent = 2;		/* two for verse */
		    *dstp++ = ' '; *dstp++ = ' ';
		}

		if (lwidth) {
		    /* Line width limitation. */
		    while (strlen(srcp) > (lwidth-indent)) {
			/* split this line.  But where? */
			endp=srcp+lwidth-indent;
			while (*endp!=' ' && endp>srcp) endp--;
			if (endp <= srcp) {
			    /* oops.  Not enough room for even one word.
			     * Print it anyway.
			     */
			    endp=srcp+1;
			    while (*endp!=' ' && *endp!='\n' && *endp) endp++;
			}

			/* style is to indent only the first line of the
			 * verse, before the verse number.  Subsequent lines
			 * are *not* indented.
			 */
			indent=0;
			
			len = endp-srcp;
			strncpy(dstp, srcp, len);
			dstp += len;
			*dstp++ = '\n';
			srcp = ++endp;		/* past the ' ' */
		    }
		    /* last line, get the rest */
		    strcpy(dstp, srcp);
		} else {
		    /* No line length limit.
		       Just copy the rest of the verse line.
		       */
		    strcpy(dstp, srcp);
		}
		/* now print out the verse */
		printf( vb2 );
		if (outf != NULL)
		    fprintf( outf, vb2 );
	    } /* for */
	} /* while */
    } else {
	/* Raw output.  Not pretty or line-wrapped */
	while (*vs && (vref = brl_verse_spec( &vs, &vcount )))
	    tsl_printtext( vref, vcount );
    }
    if (vref) return vref + vcount-1;
    else      return brl_cur_vnum;
} /* brl_printverse */



char* brl_num_to_ref( vbuf, absversep )
/*----------------------------------------------------------------------
|   NAME:
|       brl_num_to_ref
|
|   ALGORITHM:
|       Accepts an absolute verse number and returns
|       a string containing a legal verse reference.
|       
|       This is useful with some concordance functions.
|
|	  vbuf -- Points to buffer to use for string.
|
|       Returns a copy of vbuf pointer.
|
|   HISTORY:
|       921217 cc Initial creation.
|
\*----------------------------------------------------------------------*/

char *vbuf;
ref_t *absversep;
{
    int bk, chp, v;
    
    *absversep = brl_extract_num( *absversep, &bk, &chp, &v);
    sprintf(vbuf, "%s%d:%d", bookabbrvstr[bk], chp, v);
    return( vbuf );
} /* brl_num_to_ref */



brl_init( dfname, dfpath, memlimit )
/*----------------------------------------------------------------------
|   NAME:
|       brl_init
|
|   ALGORITHM:
|       Initialize the library.
|       
|       memlimit	Limit (in Kbytes) on buffer space used by TSL.
|       dfname		Name of the data file.
|       dfpath		List of paths to use in searching for the
|       		data file.
|
|   HISTORY:
|       890830 cc Added memlimit.
|       890905 cc Added filename and paths.
|
\*----------------------------------------------------------------------*/

int memlimit;
char *dfname, *dfpath;
{
    if (dfname == NULL)
	dfname = "bible.data";
    if (dfpath == NULL)
	dfpath = "./ " DESTLIB "/"; /* /usr/local/lib/; */
    tsl_init( dfname, dfpath, memlimit );

    /* Set (low) illegal value for current context.
       Making it low means that it will be coerced to Gen1:1 if
       the user gives a null input.
     */
    brl_cur_vnum = 0;
} /* brl_init */


brl_close()
/*----------------------------------------------------------------------
|   NAME:
|       brl_close
|
|   ALGORITHM:
|       Close the library.
|
|   HISTORY:
|
\*----------------------------------------------------------------------*/
{
    tsl_close();
} /* brl_close */


