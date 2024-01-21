/* -*-C-*-
*******************************************************************************
*
* File:         tsl.c
* RCS:          $Header: tsl.c,v 1.9 93/04/26 11:18:14 chip Exp $
* Description:  Text Storage Library
* Author:       Chip Chapin, Hewlett Packard Company
* Created:      Thu Aug 24 15:37:16 1989
* Modified:     Mon Apr 26 11:16:05 1993 (Chip Chapin) chip@hpclbis
* Language:     C
* Package:      Bible Retrieval System
* Status:       Experimental (Do Not Distribute)
*
*******************************************************************************
*
* Revisions:
*
* Fri Apr 23 10:14:37 1993 (Chip Chapin) chip@hpclbis
*  Revised tsl_init for new portable data file headers.
*  Rewrite shift-and-or expressions to account for Ultrix C oddities.
* Thu Apr 22 11:38:47 1993 (Chip Chapin) chip@hpclbis
*  Fix incorrect cast to cf_index.
* Thu Mar  4 12:11:24 1993 (Chip Chapin) chip@hpclbis
*  Added SEEK_SET definition for non-SysV systems.
* Wed Jan  6 10:05:43 1993 (Chip Chapin) chip@hpclbis
*  Added compression support for concordance lists.
* Mon Jan  4 11:21:50 1993 (Chip Chapin) chip@hpclbis
*  Added range parameters to tsl_scan_concordance.
* Thu Dec 24 11:32:22 1992 (Chip Chapin) chip@hpclbis
*  Tweak a couple of things to eliminate compile warnings.
* Mon Dec 21 13:40:54 1992 (Chip Chapin) chip@hpclbis
*  Made findfile a separate function in util.c
*******************************************************************************
* $Log:	tsl.c,v $
 * Revision 1.9  93/04/26  11:18:14  11:18:14  chip (Chip Chapin)
 * Release 4.00
 * Public release of portable datafile version.
 * 
 * Revision 1.8  93/04/23  13:08:02  13:08:02  chip (Chip Chapin)
 * PORTABILITY RELEASE
 * This version supports portable data files, usable on machines with
 * differing native byte-orders.
 * Also, this version compiles and runs on non-HPUX systems.  It has been
 * tested on SunOS 4.? and ULTRIX 4.?, using SPARC and DEC 3100 hardware
 * respectively.  Note that the data file format has rolled again.
 * 
 * Revision 1.7  93/01/07  12:17:29  12:17:29  chip (Chip Chapin)
 * Release 3.01: Greatly improved compression of concordance data file.
 * 
 * Revision 1.6  93/01/04  16:20:59  16:20:59  chip (Chip Chapin)
 * Release 2.1, implements ?in and ?or commands.
 * 
 * Revision 1.5  92/12/24  11:37:10  11:37:10  chip (Chip Chapin)
 * Minor release 2.04a, fixes certain compile warnings.
 * 
 * Revision 1.4  92/12/22  11:28:57  11:28:57  chip (Chip Chapin)
 * Minor release 2.01 -- fix a couple of bugs.
 * 
 * Revision 1.3  92/12/21  20:00:49  20:00:49  chip (Chip Chapin)
 * Release 2.0.  This release adds the concordance, and some small fixes.
 * 
 * Revision 1.2  89/09/14  20:33:52  20:33:52  chip (Chip Chapin)
 * Release 1-2.  Supports -f and -l options for formatting the output.
 * Updates primarily brl.c, bible.c, and bible.1.
 * 
 * Revision 1.1  89/09/05  17:49:19  17:49:19  chip (Chip Chapin)
 * Initial revision
 * 
*
*/

/*----------------------------------------------------------------------
|   NAME:
|       tsl.c
|       
|   PURPOSE:
|       This file implements the library of routines that are
|       dependent on the storage structure of the text database.
|
|   FUNCTIONS:
|       tsl_gettext
|       	Return text for a particular range of lines.
|	tsl_printtext
|		Write text to stdout instead of returning a buffer.
|       tsl_scan_concordance
|		Return list of references for a particular target.
|       tsl_init
|       	Initialize.
|       tsl_close
|       	Wrap up.
|
|   HISTORY:
|       890824 cc Extracted storage-dependent functions from brl.c
|
\*----------------------------------------------------------------------*/

#include <stdio.h>
#include <varargs.h>
/* #include <search.h> */
#include "tsl.h"


#  ifndef SEEK_SET
#    define SEEK_SET 0
#  endif /* SEEK_SET */

#define FALSE	(0)
#define TRUE	(1)

static char rcs_ident[]="@(#)$Header: tsl.c,v 1.9 93/04/26 11:18:14 chip Exp $";

char *malloc();
char *strtok();

FILE *tfp;			/* Text data file pointer */
FILE *cfp;			/* Concordance data file pointer */
struct tsl_conc_fileheader cfh;	/* Concordance file header */
char *cf_words;			/* Concordance string (word) buffer */
short int *cf_index;		/* Concordance index */
long tsl_wsize;			/* Window size (bytes) */
int tsl_wnum;			/* Number of windows */
file_ptr_t *tsl_wtable=NULL;	/* Table of window offsets in data file */

/* buffer structures.
   We maintain a doubly linked list of uncompressed text buffers, sorted in
   LRU order.
   */
struct buffer_rec {
    struct buffer_rec	*prev, *next;	/* doubly linked list */
    int		win;		/* number of associated window */
    char	*bufferp;	/* the buffer */
};

struct buffer_rec tsl_firstbuffer;	/* ptr to first buffer. */
struct buffer_rec tsl_lastbuffer;	/* take a guess... */
struct buffer_rec **tsl_wbuffers=NULL;	/* table for associating a window with
					   a particular buffer.  Indexed by
					   window number, the table yields a
					   pointer to a buffer_rec. */

char *tsl_cmpbuffer=NULL;	/* Global buffer for compressed text */
int tsl_numbuffs;		/* Count how many buffers active */
int tsl_maxbuffs;		/* Maximum number of buffers we're allowed */
int tsl_maxbuffusage=0x100000;	/* Max buffer mem usage (bytes)	 */



tsl_error( fatal, va_alist )
/*----------------------------------------------------------------------
|   NAME:
|       tsl_error
|
|   ALGORITHM:
|       Report an error specific to the TSL library.
|       
|       fatal	 TRUE if the error should cause an exit.
|       va_alist Variable argument list for printing the error
|       	 report.
|
|   HISTORY:
|       890904 cc Created.
|
\*----------------------------------------------------------------------*/

int fatal;
va_dcl
{
    va_list ap;
    char *format;

    va_start(ap);

    format = va_arg(ap, char *);
    vfprintf(stderr, format, ap);
    putc('\n', stderr);

    va_end(ap);
    if (fatal) exit(-1);
} /* tsl_error */



tsl_scan_concordance( target, sbuf, range_start, range_end )
/*----------------------------------------------------------------------
|   NAME:
|       tsl_scan_concordance
|
|   ALGORITHM:
|       Read concordance data file, searching for a specific
|       target word.   Return list of references, optionally
|       limited to those within a specific range.
|       
|         target    	Target string to search for
|         sbuf	    	Output buffer for references
|         range_start	Starting reference.  If zero, then return
|           	    	all references.
|         range_end 	Ending reference.
|
|   HISTORY:
|       921217 cc Initial creation.
|       921221 cc Revised to use new concordance file format.
|       930104 cc Added range_start, range_end params.
|
\*----------------------------------------------------------------------*/

char *target;
ref_t *sbuf;
ref_t range_start;
ref_t range_end;
{
    char *word;
    int index, n;
    int i, j, rsize;
    ref_t ref, curbase;
    unsigned char mapbyte;
    file_ptr_t inx_start;
    unsigned char tbuf[SELECTSZ*sizeof(ref_t)];

    if (cfp == NULL) {
	tsl_error( FALSE, "(No concordance data file)" );
	return(0);
    }

    /* Search through concordance string buffer */
    /* Note that the last string is guaranteed to be "~" */
    word = cf_words;
    index = 0;
    inx_start = 0;		/* Keep track of index into ref data pool */
                                /* See makeconcfile.c */
    while ((n=strcmp(word, target)) < 0) {
	/* This isn't it, but keep looking... */
	while (*word++) /* advance to next word */
	    ;
	rsize = cf_index[index];
	inx_start += rsize < 0 ? 0 : rsize;
	index++;	/* index is count of words */
    }
    if (n > 0) {
	/* This isn't it, and we've passed where it would be */
	return(0);
    }

    /* Found it! */
    rsize = cf_index[index];
    if (rsize < 0) {
	/* Special case 1: singletons
	   If a word has only a single ref, then the ref is
	   negated and put in the word's index entry.
	 */
	*sbuf = -rsize;
	if (range_start <= *sbuf && *sbuf <= range_end)
	    return( 1 );
	else
	    return( 0 );
    }
    
    /* Read all the refs for this word */
    /* Where do they end? */
    fseek( cfp, (long)(inx_start + univ2int(cfh.data_ptr)), SEEK_SET );
    fread( tbuf, 1, rsize, cfp );

    /* Process the ref list.
       Expand compressed references.
       If a range has been specified, exclude refs that are out of it. 
     */
    for (n=i=0; i < rsize; i++) {
	if (tbuf[i] > 0x7f) {
	    /* This begins a compressed entry */
	    /* First get the base ref, which has been negated */
	    ref = (tbuf[i] << 8); 	/* Do this in two parts. */
	    ref |= tbuf[++i];		/* Ultrix C goofs otherwise. */
	    curbase = ref = -ref;
	    if (range_start) {
		if (ref > range_end) break;
		if (range_start > ref) {
		    ;
		} else {
		    sbuf[n++] = ref;
		}
	    } else {
		sbuf[n++] = ref;
	    }
	    
	    mapbyte = tbuf[++i];
	    /* Two zero bytes means end of map */
	    while (mapbyte | tbuf[i+1]) {
		/* Extract refs from mapbyte */
		for (j=0; j<8; j++) {
		    if (mapbyte & (0x01 <<j)) {
			ref = curbase +j +1;
			if (range_start) {
			    if (ref > range_end) return( n );
			    if (range_start > ref) continue;
			}
			sbuf[n++] = ref;
		    }
		} /* for each bit */
		mapbyte = tbuf[++i];
		curbase += 8;
	    } /* while */
	    i++; /* Advance to second zero byte */

	} else {
	    /* This is a simple reference.
	       References are two-byte integers, in hi,lo byte order.
	     */
	    ref = (tbuf[i] << 8); 	/* Do this in two parts. */
	    ref |= tbuf[++i];		/* Ultrix C goofs otherwise. */
	    if (range_start) {
		if (ref > range_end) break;
		if (range_start > ref) continue;
	    }
	    sbuf[n++] = ref;
	}
    }

    /* Return count of entries */
    return( n );
} /* tsl_scan_concordance */



int tsl_gettext( vn, vc, vb, vbsize )
/*----------------------------------------------------------------------
|   NAME:
|       tsl_gettext
|
|   ALGORITHM:
|       Stuff buffer "vb" with text of line number "vn" and the
|       "vc-1" following lines, but no more than "vbsize" (buffer
|       size) characters.
|       
|       Returns the size (characters) of the text, *not* counting
|       the terminating null.
|
|   HISTORY:
|       890114 cc Initial implementation using simple plain text file.
|       890829 cc Updated to think about buffer size limits.
|
\*----------------------------------------------------------------------*/

int vn, vc;
char *vb;
int vbsize;
{
    long vstart, vsize;
    
    vstart = line_locator[vn];
    vsize = line_locator[ vn+vc ] - vstart;
    if (vsize >= vbsize)
	vsize = vbsize-1;	/* Leave room for trailing null */
    return tsl_textread( vstart, vsize, vb );
} /* tsl_gettext */
	


int tsl_printtext( vn, vc )
/*----------------------------------------------------------------------
|   NAME:
|       tsl_printtext
|
|   ALGORITHM:
|       Write text of line number "vn" and the "vc-1" following
|       lines to stdout.
|       
|       Returns the number of characters written.
|
|   HISTORY:
|       890902 cc Creation from tsl_gettext.
|
\*----------------------------------------------------------------------*/

int vn, vc;
{
    long vstart, vsize;
    
    vstart = line_locator[vn];
    vsize = line_locator[ vn+vc ] - vstart;

    return tsl_textread( vstart, vsize, NULL );
} /* tsl_printtext */
	


tsl_textread( start, vsize, vb )
/*----------------------------------------------------------------------
|   NAME:
|       tsl_textread
|
|   ALGORITHM:
|       Get text starting at absolute byte location "start", and
|       continuing for "vsize" bytes.  If "vb" is NULL, then write
|       the text to stdout, otherwise put it into the buffer
|       pointed to by "vb" and append a null (\0).
|       
|       Returns the size (characters) of the text, *not* counting
|       the terminating null.
|
|   HISTORY:
|       890824 cc Rewritten to handle windowed compressed files.
|       890829 cc Added all the buffer handling -- used to throw
|       	them away each time.
|       890902 cc Added stdout option.
|       890904 cc Iterate on multiple windows, instead of recursing.
|
\*----------------------------------------------------------------------*/

long start, vsize;
char *vb;
{
#ifdef PLAINTEXT
    /* here's the version that works with a plain text file */
    if (fseek( tfp, (long)start, 0 ) == EOF) {
	tsl_error( TRUE, "Cannot seek" );
    } 
    else {
	if (fread( vb, 1, vsize, tfp ) != vsize) {
	    tsl_error( TRUE, "Short read" );
	}
	vb[vsize] = '\0';
    }
#else
    /* here's the version that works with a windowed compressed file */
    /* We use the starting byte in the original file ("start"), and
       the window size ("tsl_wsize") to determine which window we need.
       Using "tsl_wtable", we know where the compressed window starts in the
       file, and how big it is.  Read the compressed window and uncompress it.
       Now we can locate the start of the text and return it.
       */
    long window;			/* current window number */
    long bstart;			/* starting byte relative to beginning
					   of uncompressed window */
    file_ptr_t wstart;			/* starting position of compressed
					   window within the data file */
    long cmpwsize;			/* size of compressed window within the
					   data file */
    long bytes_remaining;		/* Number of bytes yet to be done */
    long size;				/* bytes needed from current window */
    char *uncbuf;			/* buffer for uncompressed data */
    struct buffer_rec *brp;		/* current buffer rec */
    char *cp, *ep;			/* Handy pointers */
    
    bytes_remaining = vsize;
    while (bytes_remaining > 0) {
	window = start / tsl_wsize;	/* which window? */
	if (window >= tsl_wnum)		/* Yikes!  Off the end! */
	    tsl_error( TRUE, "Window %d out of range 0-%d", window, tsl_wnum );
	bstart = start % tsl_wsize;	/* where in [uncompressed] window? */
	if (bstart+bytes_remaining > tsl_wsize)
	    /* Request crosses boundary into next window */
	    size = tsl_wsize-bstart;
	else
	    /* Request can be completed in current window */
	    size = bytes_remaining;
	start += size;
	
	/* Notes on buffer handling ...
	   Three main cases are recognized:
	   1) The buffer for this window is already present.
	   2) It's not present and we can allocate a new buffer.
	   3) It's not present and we reuse an existing buffer.
	   */
	
	if (tsl_wbuffers[window] != NULL) {
	    /* Buffer is already present */
	    brp = tsl_wbuffers[window];
	    uncbuf = brp->bufferp;
	    
	    /* Unlink the buffer from the list.
	       We completely unlink it so the code for putting it back in
	       can be the same regardless of whether or not this is a new buffer.
	       */
	    brp->prev->next = brp->next;
	    brp->next->prev = brp->prev;
	    
	} else {
	    wstart   = tsl_wtable[window];	/* window start in file */
	    cmpwsize = tsl_wtable[window+1]
		- wstart;			/*Size of compressed window*/
	    if (fseek( tfp, (long)wstart, 0 ) == EOF) {
		tsl_error( TRUE, "Bad seek" );
	    } 
	    if (cmpwsize > tsl_wsize) {
		/* This should never happen */
		tsl_error( TRUE, "Compressed window bigger than window size!");
	    }
	    if (fread( tsl_cmpbuffer, 1, cmpwsize, tfp ) != cmpwsize) {
		tsl_error( TRUE, "Short read" );
	    }
	    
	    /* Need a new buffer */
	    if ( tsl_numbuffs >= tsl_maxbuffs ) {
		/* We're at the limit -- need to recycle one
		   Grab the buffer at the end of the LRU list.
		   */
		brp = tsl_lastbuffer.prev;	/* there it is */
		brp->prev->next = brp->next;	/* unlink it */
		brp->next->prev = brp->prev;
		uncbuf = brp->bufferp;
		tsl_wbuffers[brp->win] = NULL;	/* former owner loses */
	    } else {
		/* allocate a new buffer */
		tsl_wbuffers[window] = brp =
		    (struct buffer_rec *)malloc( sizeof(struct buffer_rec) );
		if (brp == NULL)
		    tsl_error( TRUE, "Bad malloc" );
		brp->bufferp = uncbuf = malloc( tsl_wsize );
		if (uncbuf == NULL)
		    tsl_error( TRUE, "Bad malloc" );
		tsl_numbuffs++;
	    } /* new buffer */
	    tsl_wbuffers[window] = brp;
	    brp->win = window;
	    
	    if (cmp_decompress( tsl_cmpbuffer, uncbuf, cmpwsize ) != tsl_wsize) {
		/* Last window is probably small.  Just ignore its size */
		if (window != (tsl_wnum-1)) {
		    free(uncbuf);
		    tsl_error( TRUE, "Bad decompression, result is wrong size" );
		}
	    }
	} /* else we read and decompressed the window */
	
	/* Insert this buffer at head of list */
	brp->next = tsl_firstbuffer.next;
	brp->next->prev = brp;
	tsl_firstbuffer.next = brp;
	brp->prev = &tsl_firstbuffer;
	
	/* If we've gotten this far, we have a nice decompressed buffer to use */
	cp = &uncbuf[bstart];		/* starting address */
	if (vb == NULL) {
	    ep = cp+size;
	    while (cp != ep) putchar( *cp++ );
	} else {
	    memcpy( vb, cp, size );
	    vb += size;
	}
	bytes_remaining -= size;
    } /* while */
    
    if (vb != NULL) *vb = '\0';
#endif
    
    return vsize - bytes_remaining;
} /* tsl_textread */



tsl_init(dfname, path, memlimit)
/*----------------------------------------------------------------------
|   NAME:
|       tsl_init
|
|   ALGORITHM:
|       Initialize the TSL library.
|       
|       dfname		Name of data file.
|       path   		Search path to use for file.
|       memlimit	Limit (in Kbytes) on buffer space to use.
|
|   HISTORY:
|       890825 cc Rewrite for compressed windowed files.
|       890830 cc Added memlimit.
|       890904 cc Implemented search path.
|       921221 cc Moved path search into separate function findfile().
|                 Added initialization for concordance.
|       930423 cc Revised data file headers using Univ_Int.
|
\*----------------------------------------------------------------------*/

char *dfname, *path;
int  memlimit;
{
    struct tsl_fileheader fh;
    int i;
    Short_Univ_Int *sup;
    Univ_Int *up;
    int tablesize;
#define STRSZ 255
    char cfname[STRSZ];

    if (memlimit > 0)
	tsl_maxbuffusage = memlimit<<10;	/* times 1024 */
    
    /* Open main data file */
    if ((tfp = findfile(dfname, path)) == NULL) {
	tsl_error( TRUE, "Cannot open data file %s", dfname );
    }
	
    /* What do we have here?  Let's check out the file header... */
    if (!fread( &fh, sizeof(fh), 1, tfp )) {
	tsl_error( TRUE, "Cannot read data file %s", dfname );
    }
    if ((fh.magic[0] != TSL_MAGIC1) || (fh.magic[1] != TSL_MAGIC2))
	tsl_error( TRUE, "Cannot use data file %s: Bad magic number",
		   dfname );
    if ((fh.version[0] != TSL_FVERSION1) || (fh.version[1] != TSL_FVERSION2))
    	tsl_error( TRUE, "Cannot use data file %s: Wrong version",
		   dfname );

    tsl_wsize = univ2int( fh.wsize );
    tsl_wnum  = univ2int( fh.wnum );

    /* Grab the window table */
    tablesize = sizeof(int)*(tsl_wnum+1);	/* +1 for ending entry */
    if ((tsl_wtable = (file_ptr_t *)malloc( tablesize )) == NULL)
	tsl_error( TRUE, "Bad malloc" );
    if (!fread( tsl_wtable, tablesize, 1, tfp )) {
	tsl_error( TRUE, "Error reading data file %s", dfname );
    }
    /* Convert Univ_Ints in window table to regular longs */
    up = (Univ_Int *) tsl_wtable;
    for (i=0; i<=tsl_wnum; i++) {
	tsl_wtable[i] = (file_ptr_t) univ2int( *up++ );
    }

    /* Create buffer table (parallel array to window table) */
    if ((tsl_wbuffers =
	 (struct buffer_rec **)malloc(sizeof(*tsl_wbuffers)*tsl_wnum )) == NULL)
	tsl_error( TRUE, "Bad malloc" );
    for (i=0; i< tsl_wnum; i++) tsl_wbuffers[i] = NULL;
    
    tsl_numbuffs = 0;		/* active buffers of uncompressed text */
    tsl_maxbuffs = tsl_maxbuffusage / tsl_wsize;
    if (tsl_maxbuffs < 1) tsl_maxbuffs = 1;
    tsl_firstbuffer.next = &tsl_lastbuffer;
    tsl_firstbuffer.prev = NULL;
    tsl_firstbuffer.win  = NULL;
    tsl_firstbuffer.bufferp = NULL;
    tsl_lastbuffer.prev = &tsl_firstbuffer;
    tsl_lastbuffer.next = NULL;
    tsl_lastbuffer.win  = NULL;
    tsl_lastbuffer.bufferp = NULL;

    /* Global buffer for compressed text.  Much bigger than needed. :-) */
    if ((tsl_cmpbuffer = malloc( tsl_wsize )) == NULL)
	tsl_error( TRUE, "Bad malloc" );
    
    cmp_init();		/* Initialize decompression */

    /* OK, now let's see if there's a matching concordance file */
    strncpy( cfname, dfname, STRSZ );
    strncat( cfname, ".conc", STRSZ );
    if ((cfp = findfile(cfname, path)) == NULL) {
	tsl_error( FALSE, "(No concordance file '%s' found)", cfname );
    } else {
	/* Got a file.  Now read the header. */
	if (!fread( &cfh, sizeof(cfh), 1, cfp )) {
	    tsl_error( FALSE,
		      "Warning: Error reading concordance '%s'", cfname );
	    cfp = NULL;
	} else {
	    if ((cfh.magic[0] != TSL_CONCMAGIC1) ||
		(cfh.magic[1] != TSL_CONCMAGIC2))
		tsl_error( TRUE,
			  "Cannot use concordanc file '%s': Bad magic number",
			  cfname );
	    if ((cfh.version[0] != TSL_CONCFVERSION1) ||
		(cfh.version[1] != TSL_CONCFVERSION2))
		tsl_error( TRUE,
			  "Cannot use concordance file %s: Wrong version",
			  cfname );

	    /* Allocate & initialize buffer for strings (all words) */
	    i=univ2int(cfh.index_ptr) - univ2int(cfh.word_ptr);
	    cf_words = malloc( i );
	    fread( cf_words, 1, i, cfp );

	    /* Allocate & initialize buffer for index */
	    i=univ2int(cfh.data_ptr) - univ2int(cfh.index_ptr);
	    cf_index = (short int *) malloc( i );
	    fread( cf_index, 1, i, cfp );
	    /* Convert from Short_Univ_Int to short int */
	    sup = (Short_Univ_Int *) cf_index;
	    for (i=0; i<=univ2int(cfh.word_cnt); i++) {
		cf_index[i] = (short int) shortuniv2int( *sup++ );
	    }
	}
    }
} /* tsl_init */


tsl_close()
/*----------------------------------------------------------------------
|   NAME:
|       tsl_close
|
|   ALGORITHM:
|       Tidy up before leaving the TSL library.
|
|   HISTORY:
|
\*----------------------------------------------------------------------*/

{
    int i;
    struct buffer_rec *bufp, *nbufp;
    
    fclose( tfp);
    fclose( cfp);

    /* Free all kinds of buffers and tables */
    bufp=tsl_firstbuffer.next;
    while (bufp != &tsl_lastbuffer) {
	nbufp = bufp->next;
	if (bufp->bufferp != NULL)
	    free(bufp->bufferp);	       	/* free the buffer */
	free(bufp);				/* free the buffer rec */
	bufp = nbufp;				/* on to next buffer rec */
    }
    if (tsl_wtable != NULL) free(tsl_wtable);
    if (tsl_cmpbuffer != NULL) free(tsl_cmpbuffer);
} /* tsl_close */
