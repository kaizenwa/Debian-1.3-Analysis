/* -*-C-*-
*******************************************************************************
*
* File:         tsl.h
* RCS:          $Header: tsl.h,v 1.8 93/04/26 11:18:26 chip Exp $
* Description:  Text Storage Library header file
* Author:       Chip Chapin, Hewlett Packard Company
* Created:      Jan 14 1989
* Modified:     Mon Apr 26 11:12:13 1993 (Chip Chapin) chip@hpclbis
* Language:     C
* Package:      Bible Retrieval System
* Status:       Experimental (Do Not Distribute)
*
*******************************************************************************
*
* Revisions:
*
* Fri Apr 23 09:38:02 1993 (Chip Chapin) chip@hpclbis
*  Revised headers to use Univ_Int byte-portable integers.
* Thu Jan  7 11:56:21 1993 (Chip Chapin) chip@hpclbis
*  Revised concordance file header to match bible file header.
*******************************************************************************
*
* $Log:	tsl.h,v $
 * Revision 1.8  93/04/26  11:18:26  11:18:26  chip (Chip Chapin)
 * Release 4.00
 * Public release of portable datafile version.
 * 
 * Revision 1.7  93/04/23  13:08:10  13:08:10  chip (Chip Chapin)
 * PORTABILITY RELEASE
 * This version supports portable data files, usable on machines with
 * differing native byte-orders.
 * Also, this version compiles and runs on non-HPUX systems.  It has been
 * tested on SunOS 4.? and ULTRIX 4.?, using SPARC and DEC 3100 hardware
 * respectively.  Note that the data file format has rolled again.
 * 
 * Revision 1.6  93/01/07  12:18:08  12:18:08  chip (Chip Chapin)
 * Release 3.01: Greatly improved compression of concordance data file.
 * 
 * Revision 1.5  93/01/04  16:21:03  16:21:03  chip (Chip Chapin)
 * Release 2.1, implements ?in and ?or commands.
 * 
 * Revision 1.4  92/12/21  20:01:43  20:01:43  chip (Chip Chapin)
 * Release 2.0.  This release adds the concordance, and some small fixes.
 * 
 * Revision 1.3  90/01/02  12:12:12  12:12:12  chip (Chip Chapin)
 * Fix typo in comment regarding "tsl-index.c"
 * 
 * Revision 1.2  89/09/14  20:34:18  20:34:18  chip (Chip Chapin)
 * Release 1-2.  Supports -f and -l options for formatting the output.
 * Updates primarily brl.c, bible.c, and bible.1.
 * 
 * Revision 1.1  89/09/05  17:49:50  17:49:50  chip (Chip Chapin)
 * Initial revision
 * 
*
*/

#include "util.h"

#define	TSL_MAGIC1	'E'	/* Magic number: two ASCII chars EC */
#define	TSL_MAGIC2	'C'
#define	TSL_CONCMAGIC1	'E'	/* Magic number: two ASCII chars EF */
#define	TSL_CONCMAGIC2	'F'
#define TSL_FVERSION1	'0'	/* File structure version: '01' */
#define TSL_FVERSION2	'2'
#define TSL_CONCFVERSION1 '0'	/* File structure version: '01' */
#define TSL_CONCFVERSION2 '2'

#define TSL_DESCRSZ	80	/* Size of name field in Concordance header */


extern int line_locator[];	/* Defined in "xxx-index.c" (xxx == progname) */

/*
  Structure of Text Data File.
  
    File includes a header section that identifies its nature, and an
    index (from the statistics provided by squish) to the starting
    location of every compression window.

  Output File Layout:
     bytes  Contents
     0,1	  magic number
     2,3    file version
     4,83   description: null terminated ASCII string
     --- remaining fields assume the file has been compressed with windows ---
     84-87  window size (bytes)
     88-91  number of windows
     92-95  starting location (byte in file) of window #0
     96-99  starting location (byte in file) of window #1
     ...    etc.
     xxxx   compressed data begins
   
*/
struct tsl_fileheader {
    char	magic[2];
    char	version[2];
    char	description[TSL_DESCRSZ];
    Univ_Int	wsize;
    Univ_Int	wnum;
};


/*
  Structure of Concordance Data File.

    The concordance data file is designed so that retrieval software
    can either search the file directly, or create efficient runtime
    data structures.

    Various compression strategies are used on the reference index and
    reference data pool.  For details, see makeconcfile.c and tsl.c.

    <file header>		See structure definition below.
    <word list>			All the words, in order, separated by null
                                bytes.  The order implies a unique index
				number for each word.
    <reference index>		A table, indexed by word number, of pointers
                                into the reference data.  To save space,
				these are actually cumulative offsets.
    <reference data>		Pool of reference data.
    
 */

typedef long file_ptr_t;	/* File-relative pointers */
typedef unsigned short ref_t;	/* Representation of a <ref>. */
#define SELECTSZ 32000		/* There are 31102 verses in KJV Bible. */

struct tsl_conc_fileheader {
    char	magic[2];	/* Magic number */
    char	version[2];	/* For versioning */
    char  name[TSL_DESCRSZ];	/* String describing file contents */
    Univ_Int	word_cnt;	/* Number of entries in concordance */
    Univ_Int	word_ptr;	/* File-relative pointer to word list */
    Univ_Int	index_ptr;	/* File-relative pointer to reference index */
    Univ_Int	data_ptr;	/* File-relative pointer to reference data */
};

