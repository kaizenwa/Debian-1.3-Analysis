/*
 * File:	spool.h
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	 1 Sep 95   Framstag	initial version
 * 		12 Apr 96   Framstag	sign is now in filelist
 * 					sign, comment and fname are now dynamic
 * 		24 Apr 96   Framstag	added outgoing spool support
 *
 * Header-file for functions for operations on files in the sendfile spool
 * directory.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


/* attribute flags */
#define F_SOURCE        1
#define F_TEXT          2
#define F_COMPRESS      4
#define F_CRYPT		8
#define F_TAR           16
#define F_EXE           32


/* list of all files from one sender */
struct filelist
{ int
    id,				/* id number */
    flags;			/* binary, source, text, compress and tar flag */
  unsigned long
    osize,			/* original size */
    csize,			/* compressed size */
    tsize;			/* transfered size */
  char
    rdate[DLEN+1],		/* receiving date */
    date[DLEN+1],		/* file date */
    charset[DLEN+1],		/* character set name */
    *sign,			/* pgp signature */
    *comment,			/* file comment (ISO Latin-1) */
    *fname;			/* UTF-7 file name */
  struct filelist *next;	/* next list element */
};

/* list of all senders */
struct senderlist
{ char from[MAXLEN];		/* sender */
  struct senderlist *next;	/* next list element */
  struct filelist *flist;	/* list of files */
};

/* list of all files from to a single host */
struct outfilelist
{ char
    *to,			/* recipient name */
    *from,			/* sender name */
    *fname,			/* file name (UTF-7) */
    *oshfn;			/* outgoing spool header file name */
  struct outfilelist *next;	/* next list element */
};

/* list of hosts for outgoing spool */
struct hostlist
{ char host[MAXLEN];		/* recipient host */
  struct hostlist *next;	/* next list element */
  struct outfilelist *flist;	/* list of files */
};


/* send string conforming to NVT standard */
void out(const char *);

/* scan the spool directory and create structure lists */
struct senderlist *scanspool(char *);

/* scan the outgoing spool directory and create structure lists */
struct hostlist *scanoutspool(char *);

/* create new sender list element and fill it out */
struct senderlist *newsle(struct filelist *, const char *);

/* create new host list element and fill it out */
struct hostlist *newhle(struct outfilelist *, const char *);

/* delete a spool file */
int delete_sf(struct filelist *, int);
