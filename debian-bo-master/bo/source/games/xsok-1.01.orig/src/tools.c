/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module tools.c				     */
/*									     */
/*	Miscellaneous utility functions.				     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#include "xsok.h"

void fatal(const char *msg, ...) {
    va_list args;

    va_start(args, msg);
    vfprintf(stderr, msg, args);
    fprintf (stderr, "\n");
    exit (1);
}

void *malloc_(size_t n) {
    void *p;
    if (!n)
	return NULL;	/* since malloc(0) may return NULL */
    p = malloc(n);
    if (!p)
	fatal("out of memory");
    return p;
}

void *calloc_(size_t n, size_t s) {
    void *p;
    if (!n)
	return NULL;	/* WATCOM C says "out of memory" in the case n = 0 */
    if (!(p = calloc(n, s)))
        fatal("out of memory");
    return p;
}

void *realloc_(void *p, size_t n) {
    if (p == NULL)	/* no old block of size > 0 exists */
	return malloc_(n);
    if (!n) {
	free_(p);
	return NULL;
    }
    if (!(p = realloc(p, n)))
        fatal("out of memory\n");
    return p;
}

void free_(void *p) {
    if (p)
	free(p);
}

char *strsav(const char *txt) {
    char *p = malloc_(1 + strlen(txt));
    strcpy(p, txt);
    return p;
}
