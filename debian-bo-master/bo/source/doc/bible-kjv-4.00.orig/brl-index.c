/* -*-C-*-
*******************************************************************************
*
* File:         brl-index.c
* RCS:          $Header: brl-index.c,v 1.3 93/01/05 19:05:27 chip Exp $
* Description:  BRL verse index tables
* Author:       Chip Chapin, Hewlett Packard Company
* Created:      Jan 21 1989
* Modified:     Tue Jan  5 15:27:22 1993 (Chip Chapin) chip@hpclbis
* Language:     C
* Package:      Bible Retrieval System
* Status:       Experimental (Do Not Distribute)
*
* $Log:	brl-index.c,v $
 * Revision 1.3  93/01/05  19:05:27  19:05:27  chip (Chip Chapin)
 * Release 3.00: (not for distribution)
 * Fixed errors (blank lines) in bible.data file.  Data file is not compatible
 * with previous (1.x and 2.x) distributions.  Further changes pending.
 * Rewrote context handling, and added "<" and ">" commands.
 * Tools for building brl-index are now part of release.
 * 
 * Revision 1.2  89/09/14  20:33:40  20:33:40  chip (Chip Chapin)
 * Release 1-2.  Supports -f and -l options for formatting the output.
 * Updates primarily brl.c, bible.c, and bible.1.
 * 
 * Revision 1.1  89/09/05  17:49:09  17:49:09  chip (Chip Chapin)
 * Initial revision
 * 
*
*******************************************************************************
*
* Revisions:
*
* Tue Jan  5 14:29:08 1993 (Chip Chapin) chip@hpclbis
*  Revised to INCLUDE the tables, which can thus be more easily regenerated
*  (by the makeindex2 program).  This is the first time I've needed to change
*  the tables since 1989.
*******************************************************************************
*/


/*----------------------------------------------------------------------
|   NAME:
|       brl-index.c
|
|   PURPOSE:
|       This file contains predefined tables that are used by the
|       Bible Retrieval Library to index the Bible text.  The
|       tables allow one to locate the absolute verse number (i.e.
|       counting from the start of the text) of a particular
|       Book/Chapter/Verse.  Another table (currently kept in
|       bsc-index.c) can then be used to determine the starting
|       byte location for that verse in the data file.
|       
|   FUNCTIONS:
|       No functions.
|       
|       Two tables: "start_verse" and "start_chapter", described below.
|
|   HISTORY:
|       890121 cc Initial version.
|       890830 cc Revised so that the tables can be indexed by
|       	"n+1", i.e. start_chapter[Rev+1] and start_verse[Rev23].
|       	This is very useful for forcing chapter and verse
|       	into the correct range.
|
\*----------------------------------------------------------------------*/

static char rcs_ident[]="@(#)$Header: brl-index.c,v 1.3 93/01/05 19:05:27 chip Exp $";


/* 
    This table gives the number of verses occuring in the Bible BEFORE
    any particular chapter.  Gen 1 is "chapter 1", Gen 2 is "chapter 2"
    etc.  Since there are no verses prior to Gen 1, start_verse[1] == 0.
    start_verse[2] == 31 because there are 31 verses prior to Gen 2:1.
*/
#include "brl-startverse.h"

/*
    The start_chapter table is indexed by Bible BOOK (0..65) and
    gives the number of CHAPTERS in the whole Bible that precede that
    particular book.  Thus given the BOOK, CHAPTER, and VERSE for a
    reference, we can easily compute an absolute verse number for that
    reference as follows:

	abs_verse = start_verse[ start_chapter[BOOK] + CHAPTER ] + VERSE
*/
#include "brl-startchapter.h"


/* end brl-index.c */
