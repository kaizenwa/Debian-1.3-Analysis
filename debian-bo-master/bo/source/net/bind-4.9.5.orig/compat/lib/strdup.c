#include "../../conf/portability.h"

#if !defined(NEED_STRDUP)
int __bindcompat_strdup;
#else
/* Copyright (c) 1996 by Internet Software Consortium.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE CONSORTIUM DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET SOFTWARE
 * CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#if defined(LINT) && !defined(CODECENTER)
static char rcsid[] = "$Id: strdup.c,v 8.2 1996/06/02 08:20:33 vixie Exp $";
#endif

/*
 * Make a copy of a string and return a pointer to it.
 */
char *
strdup(str)
	const char *str;
{
	char *cp = (char *) malloc(strlen(str) + sizeof '\0');

	if (cp)
		strcpy(cp, str);
	return (cp);
}

#endif
