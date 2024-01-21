/* $Source: /usr/local/src/pax/RCS/regexp.h,v $
 *
 * $Revision: 2.1 $
 *
 * regexp.h - Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 *
 * DESCRIPTION
 *
 *	Underneath the reformatting and comment blocks which were added to
 *	make it consistent with the rest of the code, you will find a
 *	modified version of Henry Specer's regular expression library.
 *	Henry's functions were modified to provide the minimal regular
 *	expression matching, as required by P1003.  Henry's code was
 *	copyrighted, and copy of the copyright message and restrictions
 *	are provided, verbatim, below:
 *
 *	Copyright (c) 1986 by University of Toronto.
 *	Written by Henry Spencer.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. The author is not responsible for the consequences of use of
 *         this software, no matter how awful, even if they arise
 *	   from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *	   by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *	   be misrepresented as being the original software.
 *
 * 	Beware that some of this code is subtly aware of the way operator
 * 	precedence is structured in regular expressions.  Serious changes in
 * 	regular-expression syntax might require a total rethink.
 *
 * AUTHORS
 *
 *     Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
 *     Henry Spencer, University of Torronto (henry@utzoo.edu)
 */

#ifndef _PAX_REGEXP_H
#define _PAX_REGEXP_H

#define NSUBEXP		10

typedef struct regexp {
    char               *startp[NSUBEXP];
    char               *endp[NSUBEXP];
    char                regstart;	/* Internal use only. */
    char                reganch;	/* Internal use only. */
    char               *regmust;	/* Internal use only. */
    int                 regmlen;	/* Internal use only. */
    char                program[1];	/* Unwarranted chumminess with
					 * compiler. */
}                   regexp;

/*
 * The first byte of the regexp internal "program" is actually this magic
 * number; the start node begins in the second byte.
 */

#define	MAGIC	0234

#endif				/* _PAX_REGEXP_H */
