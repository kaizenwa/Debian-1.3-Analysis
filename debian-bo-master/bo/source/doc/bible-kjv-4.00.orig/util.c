/* -*-C-*-
*******************************************************************************
*
* File:         util.c
* RCS:          $Header: util.c,v 1.4 93/04/26 11:18:15 chip Exp $
* Description:  Some general-purpose functions.
* Author:       Chip Chapin
* Created:      Mon Dec 21 13:29:17 1992
* Modified:     Mon Apr 26 11:15:51 1993 (Chip Chapin) chip@hpclbis
* Language:     C
* Package:      N/A
* Status:       Experimental (Do Not Distribute)
*
*******************************************************************************
*
* Revisions:
*
* Fri Apr 23 09:49:40 1993 (Chip Chapin) chip@hpclbis
*  Added Univ_Int stuff.
* Tue Dec 22 11:22:41 1992 (Chip Chapin) chip@hpclbis
*  Rewrite without strtok (oops!).
* Mon Dec 21 20:43:01 1992 (Chip Chapin) chip@hpclbis
*  Initial creation and release with Bible Retrieval System 2.0.
*******************************************************************************
* $Log:	util.c,v $
 * Revision 1.4  93/04/26  11:18:15  11:18:15  chip (Chip Chapin)
 * Release 4.00
 * Public release of portable datafile version.
 * 
*
*/


#include <stdio.h>
#include "util.h"

static char rcs_ident[]="@(#)$Header: util.c,v 1.4 93/04/26 11:18:15 chip Exp $";


FILE *findfile(dfname, pathlist)
/*----------------------------------------------------------------------
|   NAME:
|       findfile
|
|   ALGORITHM:
|       Given a file name and a path list, attempt to open the
|       file using each possible path until it's opened
|       successfully or the paths are exhausted.
|
|       pathlist -- list of paths separated by blanks or ":".
|       Returns a stream pointer (FILE*).
|
|   HISTORY:
|       921221 cc Created by extraction from tsl.c
|
\*----------------------------------------------------------------------*/

char *dfname, *pathlist;
{
#define PATHSZ 1024
    char dfpath[PATHSZ];
    char *dp, *pp, *np;
    FILE *fp;

    fp = NULL;
    pp = pathlist;
    /* Keep trying until we succeed or run out of possible paths */
    while ((fp == NULL) && *pp) {
	dp = dfpath;
	
	/* copy next item in pathlist into dfpath and */
	/* advance pathlist pointer past separator    */
	while (*pp && *pp != ' ' && *pp != ':') *dp++ = *pp++;
	while (*pp == ' ' || *pp == ':') pp++;
	
	/* Ensure a '/' separator between path and filename */
	if (*(dp-1) != '/')
	    *dp++ = '/';
	
	/* Cat dfname onto dfpath, including trailing \0 */
	for (np=dfname; *dp++ = *np++; )
	    ;
	
	fp=fopen( dfpath, "r");
    }
    return fp;
} /* findfile */



void univ_assign( dst, src )
/*----------------------------------------------------------------------
|   NAME:
|       univ_assign
|
|   ALGORITHM:
|       Assign value of "src", an int, to "dst", a Univ_Int.
|       The Univ_Int is a 4-byte integer with the bytes in a
|       pre-defined order for portability.  This facilitates the
|       construction of portable data files.
|       
|       The order of the bytes is as follows:
|           
|           dst[0] <- highest-order
|           dst[1] <- next highest
|           dst[2] <- next highest
|           dst[3] <- lowest-order
|
|   HISTORY:
|       930423 cc Initial creation.
|
\*----------------------------------------------------------------------*/

Univ_Int dst;
int	src;
{
    unsigned char b;
    int i;

    for (i=3; i>=0; i--) {
	b = src & 0x000000ff;
	src >>= 8;
	dst[i] = b;
    }
} /* univ_assign */


void shortuniv_assign( dst, src )
/*----------------------------------------------------------------------
|   NAME:
|       shortuniv_assign
|
|   ALGORITHM:
|       Assign value of "src", an int, to "dst", a Short_Univ_Int.
|       The Short_Univ_Int is a 2-byte integer with the bytes in a
|       pre-defined order for portability.  This facilitates the
|       construction of portable data files.
|       IGNORES src out-of-range errors.
|       
|       The order of the bytes is as follows:
|           
|           dst[0] <- highest-order
|           dst[1] <- lowest-order
|
|   HISTORY:
|       930423 cc Initial creation.
|
\*----------------------------------------------------------------------*/

Univ_Int dst;
int	src;
{
    unsigned char b;
    int i;

    dst[0] = (src >> 8) & 0x000000ff;
    dst[1] = src & 0x000000ff;
} /* shortuniv_assign */


int univ2int(src)
/*----------------------------------------------------------------------
|   NAME:
|       univ2int
|
|   ALGORITHM:
|       Return integer value of "src", where "src" is a Univ_Int
|       universal integer.
|
|   HISTORY:
|       930423 cc Initial creation.
|
\*----------------------------------------------------------------------*/

Univ_Int src;
{
    int result, i;

    result = 0;
    for (i=0; i<=3; i++) {
	result <<= 8;
	result |= src[i];
    }
    return result;
} /* univ2int */


int shortuniv2int(src)
/*----------------------------------------------------------------------
|   NAME:
|       shortuniv2int
|
|   ALGORITHM:
|       Return integer value of "src", where "src" is a Short_Univ_Int
|       universal integer.
|
|   HISTORY:
|       930423 cc Initial creation.
|
\*----------------------------------------------------------------------*/

Short_Univ_Int src;
{
    int result, i;

    result = 0;
    result |= src[0];
    result <<= 8;
    result |= src[1];
    return result;
} /* shortuniv2int */


