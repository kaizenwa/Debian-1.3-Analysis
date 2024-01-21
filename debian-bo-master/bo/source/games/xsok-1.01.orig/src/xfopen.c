/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module xfopen.c				     */
/*									     */
/*	Possible emulation of the popen() / pclose() functions.		     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

#ifdef HAVE_POPEN	/* extra prototype (they are not POSIX.1) */

#include "xsok.h"

FILE *popen(const char *, const char *);
int pclose(FILE *);

FILE *zreadopen(const char *filename) {
    char zcmd[MAXXSOKDIRLEN+20+100];	/* assume strlen(GUNZIP_PATH) <= 100 */
    sprintf(zcmd, "%s < %s.gz", GUNZIP_PATH, filename);
    return popen(zcmd, "r");
}
void zreadclose(FILE *fp) {
    pclose(fp);
}

#else

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "xsok.h"

#ifndef GUNZIP_PATH
#define GUNZIP_PATH "gunzip"
#endif

FILE *zreadopen(const char *filename) {
    int filedes[2];
    char zname[MAXXSOKDIRLEN+17];
    FILE *fp;

    sprintf(zname, "%s.gz", filename);
    if (access(zname, R_OK))
	return NULL;		/* simpler and more correct */
    if (GUNZIP_PATH[0] == '/')
	if (access(GUNZIP_PATH, X_OK))
	    return NULL;		/* simpler and more correct */
    /* following taken from D. Lewine "POSIX programmers guide, page 104 */
    /* try fork and exec. This requires the path of the gzip binary */
    /* but it is much faster */
    if (pipe(filedes))
	return NULL;	/* cannot create pipe */
    switch (fork()) {
    case -1:		/* cannot fork */
	/* close the pipe and return NULL */
	close(filedes[0]);
	close(filedes[1]);
	return NULL;
    case 0:			/* we are the child. exec the gunzip binary */
	close(STDOUT_FILENO);
	dup(filedes[1]);
	close(filedes[0]);
	close(filedes[1]);
	execlp(GUNZIP_PATH, "gunzip", "-c", zname, NULL);
	exit(1);	/* if exec failed! */
    default:		/* we are the main process */
	fp = fdopen(filedes[0], "r");
	close(filedes[1]);
    }
    return fp;
}

void zreadclose(FILE *fp) {
    int status;
    fclose(fp);
    wait(&status);
}

#endif
