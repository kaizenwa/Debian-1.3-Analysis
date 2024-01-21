#if 0
static char rcsid[] = "@(#)$Id: msgcat.c,v 5.2 1993/06/10 03:12:10 syd Exp $";
#endif

/* -*- c++ -*- */

/***********************************************************
Copyright 1990, by Alfalfa Software Incorporated, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that Alfalfa's name not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

ALPHALPHA DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
ALPHALPHA BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

If you make any modifications, bugfixes or other changes to this software
we'd appreciate it if you could send a copy to us so we can keep things
up-to-date.  Many thanks.
				Kee Hinckley
				Alfalfa Software, Inc.
				267 Allston St., #3
				Cambridge, MA 02139  USA
				nazgul@alfalfa.com
    
******************************************************************/

/* Edit History

11/07/93   1 mitch	fast mmap() support. (m.dsouza@mrc-apu.cam.ac.uk)
03/06/91   4 schulert	remove working directory from nlspath
01/18/91   2 hamilton	#if not rescanned
01/12/91   3 schulert	conditionally use prototypes
11/03/90   1 hamilton	Alphalpha->Alfalfa & OmegaMail->Poste
10/15/90   2 schulert	> #include <unistd.h> if MIPS
08/13/90   1 schulert	move from ua to omu
*/

/*
 * We need a better way of handling errors than printing text.  I need
 * to add an error handling routine.
 */

#include "nl_types.h"
#include "msgcat.h"
#include <locale.h>

#ifdef BSD
#include <sys/file.h>
#include <sys/param.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>

#ifdef MIPS
#include <unistd.h>
#endif

#ifndef True
# define True	~0
# define False	0
#endif

/* take care of sysv diffs */
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#ifndef FD_CLOEXEC
#define FD_CLOEXEC 1
#endif

#define	NLERR	((nl_catd) -1)

char	*MCAppPath = NULL;

#ifndef __linux__
static nl_catd loadCat();
static nl_catd loadSet();
extern char *malloc(), *getenv();
#else /* Strict prototypes makes better programmers */
#ifdef HAVE_MMAP
/*
 *  We need to find the size of the file if we are mmap()'ing, but we have
 *  already filled in the stat structure in the catopen() routine, so we
 *  just pass this structure back to loadCat() to save an extra stat() call.
 *
 */
static nl_catd loadCat(char *, int, struct stat *);
#else
static nl_catd loadCat(char *, int);
#endif
static nl_catd loadSet(MCCatT *, MCSetT *);
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <paths.h>
#endif

#ifdef HAVE_MMAP
#include <sys/types.h>
#include <sys/mman.h>
#endif

nl_catd 	catopen( name, type)
const char *name;
int type;
{
    char	path[MAXPATHLEN];
    const char	*catpath = NULL;
    char	*nlspath, *tmppath = NULL;
    char	*lang;
    long	len;
    char	*base, *cptr, *pathP;
    struct stat	sbuf;
    
    if (!name || !*name) return(NLERR);
    if (*name == '/') {
	catpath = name;
	if (stat(catpath, &sbuf)) return(NLERR);
    } else {
#if BROKEN_SETLOCALE
	if ((lang = (char *) getenv("LANG")) == NULL) lang = "C";
#else
	/* Query the locale from the previous setlocale call in msgcat-libc.c*/
	if ((lang = (char *) setlocale(LC_MESSAGES,(char *) NULL)) == NULL)
		lang="C";
#endif
	if ((nlspath = (char *) getenv("NLSPATH")) == NULL) {
#if OLD_NLS_PATHS
	    nlspath = "/nlslib/%L/%N.cat:/nlslib/%N/%L";
#else
	    nlspath = "/etc/locale/%L/%N.cat:"_PATH_LOCALE"/%L/%N.cat:"_PATH_LOCALE"/%N/%L"
		      "/usr/share/locale/%L/%N.cat:/usr/local/share/locale/%L/%N.cat";
#endif
	}
	if (MCAppPath) {
	    tmppath = (char *) malloc(strlen(nlspath) + strlen(MCAppPath) + 3);
	    if (!tmppath) return(NLERR);
	    strcpy(tmppath, nlspath);
	    if (tmppath[strlen(tmppath)-1] != ':' && *MCAppPath != ':')
	      strcat(tmppath, ":");
	    strcat(tmppath, MCAppPath);
	    nlspath = tmppath;
	}
	
	len = strlen(nlspath);
	base = cptr = (char *) malloc(len + 2);
	if (!base) return(NLERR);
	strcpy(cptr, nlspath);
	cptr[len] = ':';
	cptr[len+1] = '\0';
        
	for (nlspath = cptr; *cptr; ++cptr) {
	    if (*cptr == ':') {
		*cptr = '\0';
		for (pathP = path; *nlspath; ++nlspath) {
		    if (*nlspath == '%') {
			if (*(nlspath + 1) == 'L') {
			    ++nlspath;
			    strcpy(pathP, lang);
			    pathP += strlen(lang);
			} else if (*(nlspath + 1) == 'N') {
			    ++nlspath;
			    strcpy(pathP, name);
			    pathP += strlen(name);
			} else *(pathP++) = *nlspath;
		    } else *(pathP++) = *nlspath;
		}
		*pathP = '\0';
		if (stat(path, &sbuf) == 0) {
		    catpath = path;
		    break;
		}
		nlspath = cptr+1;
	    }
	}
	free(base);
	if (tmppath) free(tmppath);

	if (!catpath) return(NLERR);
    }

#ifdef HAVE_MMAP
    return(loadCat(catpath, type, &sbuf));
#else
    return(loadCat(catpath, type));
#endif
}

/*
 * We've got an odd situation here.  The odds are real good that the
 * number we are looking for is almost the same as the index.  We could
 * use the index, check the difference and do something intelligent, but
 * I haven't quite figured out what's intelligent.
 *
 * Here's a start.
 *	Take an id N.  If there are > N items in the list, then N cannot
 *	be more than N items from the start, since otherwise there would
 *	have to be duplicate items.  So we can safely set the top to N+1
 *	(after taking into account that ids start at 1, and arrays at 0)
 *
 *	Let's say we are at position P, and we are looking for N, but have
 *	V.  If N > V, then the furthest away that N could be is
 *	P + (N-V).  So we can safely set hi to P+(N-V)+1.  For example:
 *		We are looking for 10, but have 8
 *		8	?	?	?	?
 *			>=9	>=10	>=11
 *
 */
MCSetT	*MCGetSet( cat, setId)
MCCatT *cat;
int setId;
{
    MCSetT	*set;
    long	lo, hi, cur, dir;

    if (cat==(MCCatT *)NLERR || !cat || setId <= 0) return(NULL);

    lo = 0;
    if (setId - 1 < cat->numSets) {
	cur = setId - 1;
	hi = setId;
    } else {
	hi = cat->numSets;
	cur = (hi - lo) / 2;
    }
    
    while (True) {
	set = cat->sets + cur;
	if (set->setId == setId) break;
	if (set->setId < setId) {
	    lo = cur+1;
	    if (hi > cur + (setId - set->setId) + 1) hi = cur+(setId-set->setId)+1;
	    dir = 1;
	} else {
	    hi = cur;
	    dir = -1;
	}
	if (lo >= hi) return(NULL);
	if (hi - lo == 1) cur += dir;
	else cur += ((hi - lo) / 2) * dir;
    }
    if (set->invalid) loadSet(cat, set);
    return(set);
}

    
MCMsgT	*MCGetMsg( set, msgId)
MCSetT *set;
int msgId;
{
    MCMsgT	*msg;
    long	lo, hi, cur, dir;
    
    if (!set || set->invalid || msgId <= 0) return(NULL);
    
    lo = 0;
    if (msgId - 1 < set->numMsgs) {
	cur = msgId - 1;
	hi = msgId;
    } else {
	hi = set->numMsgs;
	cur = (hi - lo) / 2;
    }
    
    while (True) {
	msg = set->u.msgs + cur;
	if (msg->msgId == msgId) break;
	if (msg->msgId < msgId) {
	    lo = cur+1;
	    if (hi > cur + (msgId - msg->msgId) + 1) hi = cur+(msgId-msg->msgId)+1;
	    dir = 1;
	} else {
	    hi = cur;
	    dir = -1;
	}
	if (lo >= hi) return(NULL);
	if (hi - lo == 1) cur += dir;
	else cur += ((hi - lo) / 2) * dir;
    }
    return(msg);
}

char		*catgets( catd, setId, msgId, dflt)
nl_catd catd;
int setId;
int msgId;
const char *dflt;
{
    MCMsgT	*msg;
    MCCatT	*cat = (MCCatT *) catd;
    char	*cptr;

    msg = MCGetMsg(MCGetSet(cat, setId), msgId);
    if (msg) cptr = msg->msg.str;
    else cptr = dflt;
    return(cptr);
}


int	catclose (catd)
nl_catd catd;
{
    MCCatT	*cat = (MCCatT *) catd;
    MCSetT	*set;
    MCMsgT	*msg;
    int		i, j;

    if (cat==(MCCatT *)NLERR || !cat) return -1;
    
    if (cat->loadType != MCLoadAll)
#ifndef HAVE_MMAP
	close(cat->fd);
#else
	munmap(cat->u.addr,cat->size);
#endif
    for (i = 0; i < cat->numSets; ++i) {
	set = cat->sets + i;
	if (!set->invalid) {
#if 0
	    free(set->data);
#endif
	    free(set->data.str);
	    free(set->u.msgs);
	}
    }
    free(cat->sets);
    free(cat);
    return 0;
}

/*
 * Internal routines
 */

/* Note that only malloc failures are allowed to return an error */
#define ERRNAME	"Message Catalog System"
#define CORRUPT() {fprintf(stderr, "%s: corrupt file.\n", ERRNAME); return(0);}
#define NOSPACE() {fprintf(stderr, "%s: no more memory.\n", ERRNAME); return(NLERR);}

#ifdef HAVE_MMAP
static nl_catd loadCat( catpath, type, st)
#else
static nl_catd loadCat( catpath, type)
#endif
char *catpath;
int type;
#ifdef HAVE_MMAP
struct stat *st;
{
#else
{
    MCHeaderT	header;
#endif
    MCCatT	*cat;
    MCSetT	*set;
    MCMsgT	*msg;
    long	i, j;
    off_t	nextSet;
#ifdef HAVE_MMAP
    caddr_t a;
#endif

    cat = (MCCatT *) malloc(sizeof(MCCatT));
    if (!cat) return(NLERR);
    cat->loadType = type;

#ifdef HAVE_MMAP
    if ((cat->u.fd = open(catpath, O_RDONLY)) < 0) {
#else
    if ((cat->fd = open(catpath, O_RDONLY)) < 0) {
#endif
	return(0);
    }


#ifdef HAVE_MMAP
    if ((a=mmap(0,cat->size=st->st_size,
		PROT_READ,MAP_SHARED,cat->u.fd,0))==(caddr_t)-1)
								return(0);
    close(cat->u.fd);
    if(cat->size < sizeof(MCHeaderT) ||
       strncmp((cat->u.addr=a), MCMagic, MCMagicLen)!=0) CORRUPT();
#else

    fcntl(cat->fd, F_SETFD, FD_CLOEXEC);
    if (read(cat->fd, &header, sizeof(header)) != sizeof(header)) CORRUPT();

    if (strncmp(header.magic, MCMagic, MCMagicLen) != 0) CORRUPT();

#endif

#ifdef HAVE_MMAP
    if (((MCHeaderT *)(cat->u.addr))->majorVer !=MCMajorVer) {
#else
    if (header.majorVer != MCMajorVer) {
#endif
	fprintf(stderr, "%s: %s is version %d, we need %d.\n", ERRNAME, catpath,
#ifdef HAVE_MMAP
		((MCHeaderT *)(cat->u.addr))->majorVer, MCMajorVer);
#else
		header.majorVer, MCMajorVer);
#endif
	return(0);
    }

#ifdef HAVE_MMAP
    if (((MCHeaderT *)(cat->u.addr))->numSets <= 0) {
#else
    if (header.numSets <= 0) {
#endif
	fprintf(stderr, "%s: %s has %d sets!\n", ERRNAME, catpath,
#ifdef HAVE_MMAP
		((MCHeaderT *)(cat->u.addr))->numSets);
#else
		header.numSets);
#endif
	return(0);
    }
#ifdef HAVE_MMAP
    cat->numSets = ((MCHeaderT *)(cat->u.addr))->numSets;
    cat->sets = (MCSetT *) malloc(sizeof(MCSetT) *
				  ((MCHeaderT *)(cat->u.addr))->numSets);
#else
    cat->numSets = header.numSets;
    cat->sets = (MCSetT *) malloc(sizeof(MCSetT) * header.numSets);
#endif
    if (!cat->sets) NOSPACE();

#ifdef HAVE_MMAP
    nextSet = ((MCHeaderT *)(cat->u.addr))->firstSet;
#else
    nextSet = header.firstSet;
#endif
    for (i = 0; i < cat->numSets; ++i) {

#ifndef HAVE_MMAP
	if (lseek(cat->fd, nextSet, 0) == -1) CORRUPT();
#else
	if (nextSet > cat->size) CORRUPT();
#endif
	/* read in the set header */
	set = cat->sets + i;

#ifdef HAVE_MMAP
	if (nextSet+sizeof(*set) > cat->size) CORRUPT();
	bcopy(cat->u.addr+nextSet,set,sizeof(*set));
#else
	if (read(cat->fd, set, sizeof(*set)) != sizeof(*set)) CORRUPT();
#endif

	/* if it's invalid, skip over it (and backup 'i') */
	
	if (set->invalid) {
	    --i;
	    nextSet = set->nextSet;
	    continue;
	}
	if (cat->loadType == MCLoadAll) {
	    nl_catd	res;
	    if ((res = loadSet(cat, set)) <= 0) {
		if (res == -1) NOSPACE();
		CORRUPT();
	    }
	} else set->invalid = True;
	nextSet = set->nextSet;
    }
    if (cat->loadType == MCLoadAll) {
#ifndef HAVE_MMAP
	close(cat->fd);
	cat->fd = -1;
#else
	munmap(cat->u.addr,cat->size);
#endif
    }
    return((nl_catd) cat);
}

static nl_catd loadSet( cat, set)
MCCatT *cat;
MCSetT *set;
{
    MCMsgT	*msg;
    int		i;
#ifdef HAVE_MMAP
    long offset;
#endif

    /* Get the data */
#ifndef HAVE_MMAP
    if (lseek(cat->fd,set->data.off, 0) == -1) return(0);
#else
   if ((offset=set->data.off) > cat->size) return(0);
#endif

    if ((set->data.str = (char *) malloc(set->dataLen)) == NULL) return(-1);

#ifndef HAVE_MMAP
    if (read(cat->fd, set->data.str, set->dataLen) != set->dataLen) return(0);
#else
    if (offset+set->dataLen > cat->size) return(0);
    bcopy(cat->u.addr+offset,
	  set->data.str,set->dataLen);
#endif
    /* Get the messages */

#ifndef HAVE_MMAP
    if (lseek(cat->fd, set->u.firstMsg, 0) == -1) return(0);
#else
    if ((offset=set->u.firstMsg) > cat->size) return(0);
#endif

    if ((set->u.msgs = (MCMsgT *) malloc(sizeof(MCMsgT) * set->numMsgs)) == NULL) return(-1);

    for (i = 0; i < set->numMsgs; ++i) {
	msg = set->u.msgs + i;
#ifndef  HAVE_MMAP
	if (read(cat->fd, msg, sizeof(*msg)) != sizeof(*msg)) return(0);
#else
	if (offset+sizeof(*msg) > cat->size) return(0);
	bcopy(cat->u.addr+offset+i*sizeof(*msg),
	      msg,sizeof(*msg));
#endif
	if (msg->invalid) {
	    --i;
	    continue;
	}
	msg->msg.str = (char *) (set->data.str + msg->msg.off);
    }
    set->invalid = False;
    return(1);
}
