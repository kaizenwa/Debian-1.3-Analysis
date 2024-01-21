/*
 * Program:	Dummy routines for DOS
 *
 * Author:	Mark Crispin
 *		Networks and Distributed Computing
 *		Computing & Communications
 *		University of Washington
 *		Administration Building, AG-44
 *		Seattle, WA  98195
 *		Internet: MRC@CAC.Washington.EDU
 *
 * Date:	24 May 1993
 * Last Edited:	30 July 1996
 *
 * Copyright 1996 by the University of Washington
 *
 *  Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appears in all copies and that both the
 * above copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the University of Washington not be
 * used in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  This software is made
 * available "as is", and
 * THE UNIVERSITY OF WASHINGTON DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
 * WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT LIMITATION ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND IN
 * NO EVENT SHALL THE UNIVERSITY OF WASHINGTON BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, TORT
 * (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */


#include <ctype.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include "mail.h"
#include "osdep.h"
#include <sys\stat.h>
#include <dos.h>
#include <io.h>
#include "dummy.h"
#include "misc.h"

long dummy_badname (char *tmp,char *s);

/* Dummy routines */


/* Driver dispatch used by MAIL */

DRIVER dummydriver = {
  "dummy",			/* driver name */
  DR_LOCAL|DR_MAIL,		/* driver flags */
  (DRIVER *) NIL,		/* next driver */
  dummy_valid,			/* mailbox is valid for us */
  dummy_parameters,		/* manipulate parameters */
  dummy_scan,			/* scan mailboxes */
  dummy_list,			/* list mailboxes */
  dummy_lsub,			/* list subscribed mailboxes */
  NIL,				/* subscribe to mailbox */
  NIL,				/* unsubscribe from mailbox */
  dummy_create,			/* create mailbox */
  dummy_delete,			/* delete mailbox */
  dummy_rename,			/* rename mailbox */
  NIL,				/* status of mailbox */
  dummy_open,			/* open mailbox */
  dummy_close,			/* close mailbox */
  dummy_fetchfast,		/* fetch message "fast" attributes */
  dummy_fetchflags,		/* fetch message flags */
  dummy_fetchstructure,		/* fetch message structure */
  dummy_fetchheader,		/* fetch message header only */
  dummy_fetchtext,		/* fetch message body only */
  dummy_fetchbody,		/* fetch message body section */
  NIL,				/* unique identifier */
  dummy_setflag,		/* set message flag */
  dummy_clearflag,		/* clear message flag */
  NIL,				/* search for message based on criteria */
  NIL,				/* sort messages */
  NIL,				/* thread messages */
  dummy_ping,			/* ping mailbox to see if still alive */
  dummy_check,			/* check for new messages */
  dummy_expunge,		/* expunge deleted messages */
  dummy_copy,			/* copy messages to another mailbox */
  dummy_append,			/* append string message to mailbox */
  dummy_gc			/* garbage collect stream */
};


				/* prototype stream */
MAILSTREAM dummyproto = {&dummydriver};

				/* driver parameters */
static char *file_extension = NIL;

/* Dummy validate mailbox
 * Accepts: mailbox name
 * Returns: our driver if name is valid, NIL otherwise
 */

DRIVER *dummy_valid (char *name)
{
  char *s,tmp[MAILTMPLEN];
  struct stat sbuf;
				/* must be valid local mailbox */
  return (name && *name && (*name != '{') && (*name != '#') &&
	  (s = mailboxfile (tmp,name)) && (!*s || !stat (s,&sbuf))) ?
	    &dummydriver : NIL;
}


/* Dummy manipulate driver parameters
 * Accepts: function code
 *	    function-dependent value
 * Returns: function-dependent return value
 */

void *dummy_parameters (long function,void *value)
{
  switch ((int) function) {
  case SET_EXTENSION:
    if (file_extension) fs_give ((void **) &file_extension);
    if (*(char *) value) file_extension = cpystr ((char *) value);
    break;
  case GET_EXTENSION:
    value = (void *) file_extension;
    break;
  default:
    value = NIL;		/* error case */
    break;
  }
  return value;
}

/* Dummy list mailboxes
 * Accepts: mail stream
 *	    reference
 *	    pattern to search
 *	    string to scan
 */

#define LISTTMPLEN 128

void dummy_scan (MAILSTREAM *stream,char *ref,char *pat,char *contents)
{
  char *s,test[LISTTMPLEN],file[LISTTMPLEN];
  long i = 0;
				/* get canonical form of name */
  if (dummy_canonicalize (test,ref,pat)) {
				/* found any wildcards? */
    if (s = strpbrk (test,"%*")) {
				/* yes, copy name up to that point */
      strncpy (file,test,(size_t) (i = s - test));
      file[i] = '\0';		/* tie off */
    }
    else strcpy (file,test);	/* use just that name then */
				/* find directory name */
    if (s = strrchr (file,'\\')) {
      *++s = '\0';		/* found, tie off at that point */
      s = file;
    }
				/* silly case */
    else if (file[0] == '#') s = file;
				/* do the work */
    dummy_list_work (stream,s,test,contents,0);
  }
}

/* Dummy list mailboxes
 * Accepts: mail stream
 *	    reference
 *	    pattern to search
 */

void dummy_list (MAILSTREAM *stream,char *ref,char *pat)
{
  dummy_scan (stream,ref,pat,NIL);
}


/* Dummy list subscribed mailboxes
 * Accepts: mail stream
 *	    pattern to search
 */

void dummy_lsub (MAILSTREAM *stream,char *ref,char *pat)
{
  void *sdb = NIL;
  char *s,test[LISTTMPLEN];
				/* get canonical form of name */
  if (dummy_canonicalize (test,ref,pat) && (s = sm_read (&sdb)))
    do if ((*s != '#') && (*s != '{') && pmatch_full (s,test,'\\'))
      mm_lsub (stream,'\\',s,NIL);
  while (s = sm_read (&sdb)); /* until no more subscriptions */
}

/* Dummy list mailboxes worker routine
 * Accepts: mail stream
 *	    directory name to search
 *	    search pattern
 *	    string to scan
 *	    search level
 */

void dummy_list_work (MAILSTREAM *stream,char *dir,char *pat,char *contents,
		      long level)
{
  struct find_t f;
  struct stat sbuf;
  char *s,tmp[LISTTMPLEN],tmpx[LISTTMPLEN];
  char *base = (dir && (dir[0] == '\\')) ? NIL : myhomedir ();
				/* build name */
  if (base) sprintf (tmpx,"%s\\",base);
  else tmpx[0] = '\0';
  if (dir) strcat (tmpx,dir);
				/* punt if bogus name */
  if (!mailboxfile (tmp,tmpx)) return;
				/* make directory wildcard */
  strcat (tmp,(tmp[strlen (tmp) -1] == '\\') ? "*." : "\\*.");
  strcat (tmp,file_extension ? file_extension : "*");
				/* do nothing if can't open directory */
  if (!_dos_findfirst (tmp,_A_NORMAL|_A_SUBDIR,&f)) {
				/* list it if not at top-level */
    if (!level && dir && pmatch_full (dir,pat,'\\'))
      dummy_listed (stream,'\\',dir,LATT_NOSELECT,contents);
				/* scan directory */
    if (tmpx[strlen (tmpx) - 1] == '\\') do if (*f.name != '.') {
      if (base) sprintf (tmpx,"%s\\",base);
      else tmpx[0] = '\0';
      if (dir) sprintf (tmpx + strlen (tmpx),"%s%s",dir,f.name);
      else strcat (tmpx,f.name);
      if (!stat (mailboxfile (tmp,tmpx),&sbuf)) {
				/* suppress extension */
	if (file_extension && (s = strchr (f.name,'.'))) *s = '\0';
				/* now make name we'd return */
	if (dir) sprintf (tmp,"%s%s",dir,f.name);
	else strcpy (tmp,f.name);
				/* only interested in file type */
	switch (sbuf.st_mode &= S_IFMT) {
	case S_IFDIR:		/* directory? */
	  if (pmatch_full (tmp,pat,'\\')) {
	    dummy_listed (stream,'\\',tmp,LATT_NOSELECT,contents);
	    strcat (tmp,"\\");	/* set up for dmatch call */
	  }
				/* try again with trailing / */
	  else if (pmatch_full (strcat (tmp,"\\"),pat,'\\'))
	    dummy_listed (stream,'\\',tmp,LATT_NOSELECT,contents);
	  if (dmatch (tmp,pat,'\\') &&
	      (level < (long) mail_parameters (NIL,GET_LISTMAXLEVEL,NIL)))
	    dummy_list_work (stream,tmp,pat,contents,level+1);
	  break;
	case S_IFREG:		/* ordinary name */
	  if (pmatch_full (tmp,pat,'\\'))
	    dummy_listed (stream,'\\',tmp,LATT_NOINFERIORS,contents);
	  break;
	}
      }
    }
    while (!_dos_findnext (&f));
  }
}

/* Mailbox found
 * Accepts: hierarchy delimiter
 *	    mailbox name
 *	    attributes
 *	    contents to search before calling mm_list()
 * Returns: T, always
 */

#define BUFSIZE MAILTMPLEN

long dummy_listed (MAILSTREAM *stream,char delimiter,char *name,
		   long attributes,char *contents)
{
  struct stat sbuf;
  int fd;
  size_t csiz,ssiz,bsiz;
  char *buf,tmp[MAILTMPLEN];
  if (contents) {		/* want to search contents? */
				/* forget it if can't select or open */
    if ((attributes & LATT_NOSELECT) || !(csiz = strlen (contents)) ||
	stat (mailboxfile (tmp,name),&sbuf) || (csiz > sbuf.st_size) ||
	((fd = open (tmp,O_RDONLY,NIL)) < 0)) return T;
				/* get buffer including slop */    
    buf = (char *) fs_get (BUFSIZE + (ssiz = 4 * ((csiz / 4) + 1)) + 1);
    memset (buf,'\0',ssiz);	/* no slop area the first time */
    while (sbuf.st_size) {	/* until end of file */
      read (fd,buf+ssiz,bsiz = min (sbuf.st_size,BUFSIZE));
      if (search (buf,bsiz+ssiz,contents,csiz)) break;
      memcpy (buf,buf+BUFSIZE,ssiz);
      sbuf.st_size -= bsiz;	/* note that we read that much */
    }
    fs_give ((void **) &buf);	/* flush buffer */
    close (fd);			/* finished with file */
    if (!sbuf.st_size) return T;/* not found */
  }
				/* notify main program */
  mm_list (stream,delimiter,name,attributes);
  return T;
}

/* Dummy create mailbox
 * Accepts: mail stream
 *	    mailbox name to create
 *	    driver type to use
 * Returns: T on success, NIL on failure
 */

long dummy_create (MAILSTREAM *stream,char *mailbox)
{
  struct stat sbuf;
  char c,*s,mbx[MAILTMPLEN];
  long ret = NIL;
  int fd,wantdir;
  if (!mailboxfile (mbx,mailbox)) return dummy_badname (mbx,mailbox);
  wantdir = (s = strrchr (mbx,'\\')) && !s[1];
  if (wantdir) *s = '\0';	/* flush trailing delimiter for directory */
  if (s = strrchr (mbx,'\\')) {	/* found superior to this name? */
    c = *++s;			/* remember first character of inferior */
    *s = '\0';			/* tie off to get just superior */
				/* name doesn't exist, create it */
    if ((stat (mbx,&sbuf) || ((sbuf.st_mode & S_IFMT) != S_IFDIR)) &&
	!dummy_create (stream,mbx)) return NIL;
    *s = c;			/* restore full name */
  }
				/* want to create directory? */
  if (wantdir) ret = !mkdir (mbx);
				/* create file */
  else if ((fd = open (mbx,O_WRONLY|O_CREAT|O_EXCL,S_IREAD|S_IWRITE)) >= 0)
    ret = !close (fd);		/* close file */
  if (!ret) {			/* error? */
    char tmp[MAILTMPLEN];
    sprintf (tmp,"Can't create mailbox node %s: %s",mbx,strerror (errno));
    mm_log (tmp,ERROR);
    return NIL;
  }
  return ret;			/* return status */
}

/* Dummy delete mailbox
 * Accepts: mail stream
 *	    mailbox name to delete
 * Returns: T on success, NIL on failure
 */

long dummy_delete (MAILSTREAM *stream,char *mailbox)
{
  struct stat sbuf;
  char *s,tmp[MAILTMPLEN];
  if (!mailboxfile (tmp,mailbox)) return dummy_badname (tmp,mailbox);
				/* no trailing / (workaround BSD kernel bug) */
  if ((s = strrchr (tmp,'\\')) && !s[1]) *s = '\0';
  if (stat (tmp,&sbuf) || ((sbuf.st_mode & S_IFMT) == S_IFDIR) ?
      rmdir (tmp) : unlink (tmp)) {
    sprintf (tmp,"Can't delete mailbox %s: %s",mailbox,strerror (errno));
    mm_log (tmp,ERROR);
    return NIL;
  }
  return T;			/* return success */
}


/* Mail rename mailbox
 * Accepts: mail stream
 *	    old mailbox name
 *	    new mailbox name
 * Returns: T on success, NIL on failure
 */

long dummy_rename (MAILSTREAM *stream,char *old,char *newname)
{
  struct stat sbuf;
  char c,*s,tmp[MAILTMPLEN],file[MAILTMPLEN];
				/* make file name */
  if (!mailboxfile (file,old)) return dummy_badname (tmp,old);
  if (newname && !mailboxfile (tmp,newname))
    return dummy_badname (tmp,newname);
  if (s = strrchr (tmp,'\\')) {	/* found superior to destination name? */
    c = *++s;			/* remember first character of inferior */
    *s = '\0';			/* tie off to get just superior */
				/* name doesn't exist, create it */
    if ((stat (file,&sbuf) || ((sbuf.st_mode & S_IFMT) != S_IFDIR)) &&
	!dummy_create (stream,file)) return NIL;
    *s = c;			/* restore full name */
  }
  if (rename (file,tmp)) {
    sprintf (tmp,"Can't rename mailbox %s to %s: %s",old,newname,
	     strerror (errno));
    mm_log (tmp,ERROR);
    return NIL;
  }
  return LONGT;			/* return success */
}

/* Dummy open
 * Accepts: stream to open
 * Returns: stream on success, NIL on failure
 */

MAILSTREAM *dummy_open (MAILSTREAM *stream)
{
  char tmp[MAILTMPLEN];
  struct stat sbuf;
  int fd = -1;
				/* OP_PROTOTYPE call or silence */
  if (!stream || stream->silent) return NIL;
  if (strcmp (ucase (strcpy (tmp,stream->mailbox)),"INBOX") &&
      ((fd = open (mailboxfile (tmp,stream->mailbox),O_RDONLY,NIL)) < 0))
    sprintf (tmp,"%s: %s",strerror (errno),stream->mailbox);
  else {
    if (fd >= 0) {		/* if got a file */
      fstat (fd,&sbuf);		/* sniff at its size */
      close (fd);
      if (sbuf.st_size) sprintf (tmp,"Not a mailbox: %s",stream->mailbox);
      else fd = -1;		/* a-OK */
    }
    if (fd < 0) {		/* no file, right? */
      if (!stream->silent) {	/* only if silence not requested */
				/* say there are 0 messages */
	mail_exists (stream,(long) 0);
	mail_recent (stream,(long) 0);
      }
      return stream;		/* return success */
    }
  }
  if (!stream->silent) mm_log (tmp,ERROR);
  return NIL;			/* always fails */
}


/* Dummy close
 * Accepts: MAIL stream
 *	    options
 */

void dummy_close (MAILSTREAM *stream,long options)
{
				/* return silently */
}

/* Dummy fetch fast information
 * Accepts: MAIL stream
 *	    sequence
 *	    option flags
 */

void dummy_fetchfast (MAILSTREAM *stream,char *sequence,long flags)
{
  fatal ("Impossible dummy_fetchfast");
}


/* Dummy fetch flags
 * Accepts: MAIL stream
 *	    sequence
 *	    option flags
 */

void dummy_fetchflags (MAILSTREAM *stream,char *sequence,long flags)
{
  fatal ("Impossible dummy_fetchflags");
}


/* Dummy fetch envelope
 * Accepts: MAIL stream
 *	    message # to fetch
 *	    pointer to return body
 *	    option flags
 * Returns: envelope of this message, body returned in body value
 */

ENVELOPE *dummy_fetchstructure (MAILSTREAM *stream,unsigned long msgno,
				BODY **body,long flags)
{
  fatal ("Impossible dummy_fetchstructure");
  return NIL;
}


/* Dummy fetch message header
 * Accepts: MAIL stream
 *	    message # to fetch
 *	    list of headers
 *	    pointer to returned length
 *	    options
 * Returns: message header in RFC822 format
 */

char *dummy_fetchheader (MAILSTREAM *stream,unsigned long msgno,
			 STRINGLIST *lines,unsigned long *len,long flags)
{
  fatal ("Impossible dummy_fetchheader");
  return NIL;
}

/* Dummy fetch message text (body only)
 * Accepts: MAIL stream
 *	    message # to fetch
 *	    pointer to returned length
 *	    options
 * Returns: message text in RFC822 format
 */

char *dummy_fetchtext (MAILSTREAM *stream,unsigned long msgno,
		       unsigned long *len,long flags)
{
  fatal ("Impossible dummy_fetchtext");
  return NIL;
}


/* Dummy fetch message body as a structure
 * Accepts: Mail stream
 *	    message # to fetch
 *	    section specifier
 *	    pointer to returned length
 *	    options
 * Returns: pointer to section of message body
 */

char *dummy_fetchbody (MAILSTREAM *stream,unsigned long msgno,char *sec,
		       unsigned long *len,long flags)
{
  fatal ("Impossible dummy_fetchbody");
  return NIL;
}

/* Dummy set flag
 * Accepts: MAIL stream
 *	    sequence
 *	    flag(s)
 *	    options
 */

void dummy_setflag (MAILSTREAM *stream,char *sequence,char *flag,long flags)
{
  fatal ("Impossible dummy_setflag");
}


/* Dummy clear flag
 * Accepts: MAIL stream
 *	    sequence
 *	    flag(s)
 *	    options
 */

void dummy_clearflag (MAILSTREAM *stream,char *sequence,char *flag,long flags)
{
  fatal ("Impossible dummy_clearflag");
}

/* Dummy ping mailbox
 * Accepts: MAIL stream
 * Returns: T if stream alive, else NIL
 * No-op for readonly files, since read/writer can expunge it from under us!
 */

long dummy_ping (MAILSTREAM *stream)
{
  char tmp[MAILTMPLEN];
  MAILSTREAM *test = mail_open (NIL,stream->mailbox,OP_PROTOTYPE);
				/* swap streams if looks like a new driver */
  if (test && (test->dtb != stream->dtb))
    test = mail_open (stream,strcpy (tmp,stream->mailbox),NIL);
  return test ? T : NIL;
}


/* Dummy check mailbox
 * Accepts: MAIL stream
 * No-op for readonly files, since read/writer can expunge it from under us!
 */

void dummy_check (MAILSTREAM *stream)
{
  dummy_ping (stream);		/* invoke ping */
}


/* Dummy expunge mailbox
 * Accepts: MAIL stream
 */

void dummy_expunge (MAILSTREAM *stream)
{
				/* return silently */
}

/* Dummy copy message(s)
 * Accepts: MAIL stream
 *	    sequence
 *	    destination mailbox
 *	    options
 * Returns: T if copy successful, else NIL
 */

long dummy_copy (MAILSTREAM *stream,char *sequence,char *mailbox,long options)
{
  fatal ("Impossible dummy_copy");
  return NIL;
}


/* Dummy append message string
 * Accepts: mail stream
 *	    destination mailbox
 *	    stringstruct of message to append
 * Returns: T on success, NIL on failure
 */

long dummy_append (MAILSTREAM *stream,char *mailbox,char *flags,char *date,
		   STRING *message)
{
  struct stat sbuf;
  int fd = -1;
  int e;
  char tmp[MAILTMPLEN];
  if ((strcmp (ucase (strcpy (tmp,mailbox)),"INBOX")) &&
	   ((fd = open (mailboxfile (tmp,mailbox),O_RDONLY,NIL)) < 0)) {
    if ((e = errno) == ENOENT) {/* failed, was it no such file? */
      mm_notify (stream,"[TRYCREATE] Must create mailbox before append",
		 (long) NIL);
      return NIL;
    }
    sprintf (tmp,"%s: %s",strerror (e),mailbox);
    mm_log (tmp,ERROR);		/* pass up error */
    return NIL;			/* always fails */
  }
  else if (fd >= 0) {		/* found file? */
    fstat (fd,&sbuf);		/* get its size */
    close (fd);			/* toss out the fd */
    if (sbuf.st_size) {		/* non-empty file? */
      sprintf (tmp,"Indeterminate mailbox format: %s",mailbox);
      mm_log (tmp,ERROR);
      return NIL;
    }
  }
  return (*default_proto ()->dtb->append) (stream,mailbox,flags,date,message);
}

/* Dummy garbage collect stream
 * Accepts: mail stream
 *	    garbage collection flags
 */

void dummy_gc (MAILSTREAM *stream,long gcflags)
{
				/* return silently */
}


/* Return bad file name error message
 * Accepts: temporary buffer
 *	    file name
 * Returns: long NIL always
 */

long dummy_badname (char *tmp,char *s)
{
  sprintf (tmp,"Invalid mailbox name: %s",s);
  mm_log (tmp,ERROR);
  return (long) NIL;
}

/* Dummy canonicalize name
 * Accepts: buffer to write name
 *	    reference
 *	    pattern
 * Returns: T if success, NIL if failure
 */

long dummy_canonicalize (char *tmp,char *ref,char *pat)
{
  if (ref) {			/* preliminary reference check */
    if ((*ref == '{') || (*ref == '#')) return NIL;
    else if (!*ref) ref = NIL;	/* treat empty reference as no reference */
  }
  switch (*pat) {
  case '#':			/* namespace name */
  case '{':			/* remote names not allowed */
    return NIL;
  case '\\':			/* rooted name */
  case '~':			/* home directory name */
    ref = NIL;			/* nuke the reference */
				/* fall through */
  default:			/* apply reference for all other names */
    if (!ref) strcpy (tmp,pat);	/* just copy if no reference */
				/* wants root? */
    else if (*pat == '\\') strcpy (strchr (strcpy (tmp,ref),'\\'),pat);
				/* otherwise just append */
    else sprintf (tmp,"%s%s",ref,pat);
  }
  return T;
}
