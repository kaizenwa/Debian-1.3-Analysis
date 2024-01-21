/*
 * brace.c: csh brace expansion -- includes pieces of
 * [t]csh's sh.misc.c and sh.glob.c.
 *
 * Modified for use as a standalone brace-globbing subroutine.
 *	- Will Deich, Mar 93.
 * Modified to optionally apply braces to input string, and glob
 * the resulting string.  This lets us take "a,b,c" and act as if
 * it was "{a,b,c}".
 *	- Will Deich, Jan 96.
 */

#include "version.h"

/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#define _POSIX_SOURCE

#include <sys/types.h>
#include <sys/param.h>
#include <string.h>

typedef void * ptr_t;

#ifndef NULL
#define NULL 0
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

extern void *malloc();
extern void *realloc();
extern void free();

#define xmalloc		malloc
#define xrealloc	realloc
#define xfree		free

#define	GLOBSPACE	100	/* Alloc increment */

#define LBRC '{'
#define RBRC '}'
#define LBRK '['
#define RBRK ']'
#define EOS '\0'

#define strsave(s) strsavew((s), 0)

static char *strsavew();
void blkfree();
static int blklen();

int
globbraces(v, wrap, blp)
char *v;
int wrap;
char ***blp;
{
    char   *s;
    char  **nv, **vl, **el;
    int     size = GLOBSPACE;
    int     glob1brace();


    nv = vl = (char **) xmalloc((size_t) sizeof(char *) * size);
    *vl++ = strsavew(v, wrap);
    *vl = NULL;

    el = vl;
    vl = nv;
    for (s = *vl; s; s = *++vl) {
	char   *b;
	char  **vp, **bp;

	if ((b = strchr(s, LBRC)) && b[1] != '\0' && b[1] != RBRC) {
	    char **bl;
	    int len;

	    if ((len = glob1brace(s, &bl)) < 0) {
		blkfree(nv);
		return -len;
	    }
	    xfree((ptr_t) s);
	    if (len == 1) {
		*vl-- = *bl;
		xfree((ptr_t) bl);
		continue;
	    }
	    len = blklen(bl);
	    if (&el[len] >= &nv[size]) {
		int     l, e;

		l = &el[len] - &nv[size];
		size += GLOBSPACE > l ? GLOBSPACE : l;
		l = vl - nv;
		e = el - nv;
		nv = (char **) xrealloc((ptr_t) nv, (size_t)
					size * sizeof(char *));
		vl = nv + l;
		el = nv + e;
	    }
	    vp = vl--;
	    *vp = *bl;
	    len--;
	    for (bp = el; bp != vp; bp--)
		bp[len] = *bp;
	    el += len;
	    vp++;
	    for (bp = bl + 1; *bp; *vp++ = *bp++)
		continue;
	    xfree((ptr_t) bl);
	}
    }
    *blp = nv;
    return 0;
}

int
glob1brace(s, bl)
    char   *s, ***bl;
{
    char *p;
    int     i, len;
    char   *pm, *pe, *lm, *pl;
    char  **nv, **vl;
    char    gbuf[MAXPATHLEN];
    int     size = GLOBSPACE;

    nv = vl = (char **) xmalloc((size_t) sizeof(char *) * size);
    *vl = NULL;

    len = 0;
    /* copy part up to the brace */
    for (lm = gbuf, p = s; *p != LBRC; *lm++ = *p++)
	continue;

    /* check for balanced braces */
    for (i = 0, pe = ++p; *pe; pe++) {
	if (*pe == LBRK) {
	    /* Ignore everything between [] */
	    for (++pe; *pe != RBRK && *pe != EOS; pe++)
		continue;
	    if (*pe == EOS) {
		blkfree(nv);
		return (-RBRK);
	    }
	}
	else if (*pe == LBRC)
	    i++;
	else if (*pe == RBRC) {
	    if (i == 0)
		break;
	    i--;
	}
    }

    if (i != 0 || *pe == '\0') {
	blkfree(nv);
	return (-RBRC);
    }

    for (i = 0, pl = pm = p; pm <= pe; pm++)
	switch (*pm) {
	case LBRK:
	    for (++pm; *pm != RBRK && *pm != EOS; pm++)
		continue;
	    if (*pm == EOS) {
		*vl = NULL;
		blkfree(nv);
		return (-RBRK);
	    }
	    break;
	case LBRC:
	    i++;
	    break;
	case RBRC:
	    if (i) {
		i--;
		break;
	    }
	    /* FALLTHROUGH */
	case ',':
	    if (i && *pm == ',')
		break;
	    else {
		char    savec = *pm;

		*pm = EOS;
		(void) strcpy(lm, pl);
		(void) strcat(gbuf, pe + 1);
		*pm = savec;
		*vl++ = strsave(gbuf);
		len++;
		pl = pm + 1;
		if (vl == &nv[size]) {
		    size += GLOBSPACE;
		    nv = (char **) xrealloc((ptr_t) nv, (size_t)
					    size * sizeof(char *));
		    vl = &nv[size - GLOBSPACE];
		}
	    }
	    break;
	default:
	    break;
	}
    *vl = NULL;
    *bl = nv;
    return (len);
}

static char *
strsavew(s, wrap)
    char *s;
    int wrap;		/* If 0: save s.   If !0: save '{' s '}' */
{
    char   *n;
    register char *p;

    if (s == NULL)
	s = (char *) "";
    for (p = (char *) s; *p++;);
    n = p = (char *) xmalloc((size_t) ((p - s) * sizeof(char)) +
					(wrap ? 2 : 0));
    if (wrap)
	*p++ = '{';
    while ( (*p++ = *s++) ) ;
    if (wrap) {
	*(p-1) = '}';
	*p = '\0';
    }
    return (n);
}

void
blkfree(av0)
    char **av0;
{
    register char **av = av0;

    if (!av0)
	return;
    for (; *av; av++)
	xfree((ptr_t) * av);
    xfree((ptr_t) av0);
}

static int
blklen(av)
    char **av;
{
    register int i = 0;

    while (*av++)
	i++;
    return i;
}

#ifdef TEST
#include <stdio.h>

main(argc, argv)
    int argc;
    char **argv;
{
    int i, status;
    char **p;

    while (*(++argv)) {
	status = globbraces(*argv, 0, &p);
	if (status == 0 - '}')
	    printf("} error\n");
	else if (status == 0 - ']')
	    printf("] error\n");
	else if (status < 0)
	    printf("globbraces returns %d\n", status);
	else {
	    printf("%s expands to:\n", *argv);
	    while (*p)
		printf("\t`%s'\t\n", *p++);
	}
	printf("\nRepeat, with wrap-string-in-braces enabled:\n");
	status = globbraces(*argv, 1, &p);
	if (status == 0 - '}')
	    printf("} error\n");
	else if (status == 0 - ']')
	    printf("] error\n");
	else if (status < 0)
	    printf("globbraces returns %d\n", status);
	else {
	    printf("%s expands to:\n", *argv);
	    while (*p)
		printf("\t`%s'\t\n", *p++);
	}
    }
}
#endif
