/* $Id: util.c,v 3.0 1992/02/23 21:25:39 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "final.h"
#include "term.h"
#include "nntp.h"
#include "nntpauth.h"
#include "INTERN.h"
#include "util.h"

void
util_init()
{
    ;
}
    
/* fork and exec a shell command */

int
doshell(shl,s)
char *s, *shl;
{
    int status, pid, w;
    char *shell;
    bool mouse_flag;	/* if TRUE, mouse tracking was on */

    mouse_flag = xmouse_status;
    xmouse_off();

#ifdef SIGTSTP
    sigset(SIGTSTP,SIG_DFL);
    sigset(SIGTTOU,SIG_DFL);
    sigset(SIGTTIN,SIG_DFL);
#endif
    if (shl != Nullch)
	shell = shl;
    else if ((shell = getenv("SHELL")) == Nullch || !*shell)
	shell = PREFSHELL;
    if ((pid = vfork()) == 0) {
#ifdef USE_NNTP
        int i;

	/* This is necessary to keep bourne shell from puking */

        for (i = 3; i < 10; ++i) {
#ifdef USE_GENAUTH
	    if (cookiefd != -1) {
		if (i == fileno(ser_rd_fp)
		 || i == fileno(ser_wr_fp)
		 || i == cookiefd)
		    continue;
	    }
#endif
            close(i);
	}
#endif /* USE_NNTP */

	if (*s)
	    execl(shell, shell, "-c", s, Nullch);
	else
	    execl(shell, shell, Nullch, Nullch, Nullch);
	_exit(127);
    }
    signal(SIGINT, SIG_IGN);
#ifdef SIGQUIT
    signal(SIGQUIT, SIG_IGN);
#endif 
    termlib_reset();
    waiting = TRUE;
    while ((w = wait(&status)) != pid)
	if (w == -1 && errno != EINTR)
	    break;
    if (w == -1)
	status = -1;
    termlib_init();
    if (mouse_flag)
	xmouse_on();
    waiting = FALSE;
    sigset(SIGINT, int_catcher);	/* always catch interrupts */
#ifdef SIGQUIT
    signal(SIGQUIT, SIG_DFL);
#endif 
#ifdef SIGTSTP
    sigset(SIGTSTP,stop_catcher);
    sigset(SIGTTOU,stop_catcher);
    sigset(SIGTTIN,stop_catcher);
#endif
    return status;
}

static char nomem[] = "trn: out of memory!\n";

/* paranoid version of malloc */

char *
safemalloc(size)
MEM_SIZE size;
{
    char *ptr;

    ptr = malloc(size ? size : (MEM_SIZE)1);
    if (ptr == Nullch) {
	fputs(nomem,stdout) FLUSH;
	sig_catcher(0);
    }
    return ptr;
}

/* paranoid version of realloc.  If where is NULL, call malloc */

char *
saferealloc(where,size)
char *where;
MEM_SIZE size;
{
    char *ptr;

    if (!where)
	ptr = malloc(size ? size : (MEM_SIZE)1);
    else
	ptr = realloc(where, size ? size : (MEM_SIZE)1);
    if (!ptr) {
	fputs(nomem,stdout) FLUSH;
	sig_catcher(0);
    }
    return ptr;
}

/* safe version of string copy */

char *
safecpy(to,from,len)
char *to;
register char *from;
register int len;
{
    register char *dest = to;

    if (from != Nullch) 
	for (len--; len && (*dest++ = *from++); len--) ;
    *dest = '\0';
    return to;
}

/* safe version of string concatenate, with \n deletion and space padding */

char *
safecat(to,from,len)
char *to;
register char *from;
register int len;
{
    register char *dest = to;

    len--;				/* leave room for null */
    if (*dest) {
	while (len && *dest++) len--;
	if (len) {
	    len--;
	    *(dest-1) = ' ';
	}
    }
    if (from != Nullch)
	while (len && (*dest++ = *from++)) len--;
    if (len)
	dest--;
    if (*(dest-1) == '\n')
	dest--;
    *dest = '\0';
    return to;
}

/* copy a string up to some (non-backslashed) delimiter, if any */

char *
cpytill(to,from,delim)
register char *to, *from;
register int delim;
{
    for (; *from; from++,to++) {
	if (*from == '\\' && from[1] == delim)
	    from++;
	else if (*from == delim)
	    break;
	*to = *from;
    }
    *to = '\0';
    return from;
}

/* effective access */

#ifdef SETUIDGID
int
eaccess(filename, mod)
char *filename;
int mod;
{
    int protection, euid;
    
    mod &= 7;				/* remove extraneous garbage */
    if (stat(filename, &filestat) < 0)
	return -1;
    euid = geteuid();
    if (euid == ROOTID)
	return 0;
    protection = 7 & (filestat.st_mode >>
      (filestat.st_uid == euid ? 6 :
        (filestat.st_gid == getegid() ? 3 : 0)
      ));
    if ((mod & protection) == mod)
	return 0;
    errno = EACCES;
    return -1;
}
#endif

/*
 * Get working directory
 */
#ifndef HAS_GETWD
#ifdef HAS_GETCWD
char *
getwd(np)
char *np;
{
    char *ret;
    extern char *getcwd();

    if ((ret = getcwd(np,512)) == Nullch) {
	printf("Cannot determine current working directory!\n") FLUSH;
	finalize(1);
    }
    return ret;
}
#else
char *
getwd(np)
char *np;
{
    FILE *popen();
    FILE *pipefp;

    if ((pipefp = popen("/bin/pwd","r")) == Nullfp) {
	printf("Can't run /bin/pwd\n") FLUSH;
	finalize(1);
    }
    fgets(np,512,pipefp);
    np[strlen(np)-1] = '\0';	/* wipe out newline */
    if (pclose(pipefp) == EOF) {
	printf("Failed to run /bin/pwd\n") FLUSH;
	finalize(1);
    }
    return np;
}
#endif
#endif

/* just like fgets but will make bigger buffer as necessary */

char *
get_a_line(buffer,buffer_length,realloc_ok,fp)
char *buffer;
register int buffer_length;
bool_int realloc_ok;
FILE *fp;
{
    register int bufix = 0;
    register int nextch;

    do {
	if (bufix >= buffer_length) {
	    buffer_length *= 2;
	    if (realloc_ok) {		/* just grow in place, if possible */
		buffer = saferealloc(buffer,(MEM_SIZE)buffer_length+1);
	    }
	    else {
		char *tmp = safemalloc((MEM_SIZE)buffer_length+1);
		strncpy(tmp,buffer,buffer_length/2);
		buffer = tmp;
		realloc_ok = TRUE;
	    }
	}
	if ((nextch = getc(fp)) == EOF)
	    return Nullch;
	buffer[bufix++] = (char)nextch;
    } while (nextch && nextch != '\n');
    buffer[bufix] = '\0';
    len_last_line_got = bufix;
    buflen_last_line_got = buffer_length;
    return buffer;
}

/* copy a string to a safe spot */

char *
savestr(str)
char *str;
{
    register char *newaddr = safemalloc((MEM_SIZE)(strlen(str)+1));

    strcpy(newaddr,str);
    return newaddr;
}

int
makedir(dirname,nametype)
register char *dirname;
int nametype;
{
#ifdef MAKEDIR
    register char *end;
    register char *s;
    char tmpbuf[1024];
    register char *tbptr = tmpbuf+5;

    for (end = dirname; *end; end++) ;	/* find the end */
    if (nametype == MD_FILE) {		/* not to create last component? */
	for (--end; end != dirname && *end != '/'; --end) ;
	if (*end != '/')
	    return 0;			/* nothing to make */
	*end = '\0';			/* isolate file name */
    }
    strcpy(tmpbuf,"mkdir");

    s = end;
    for (;;) {
	if (stat(dirname,&filestat) >= 0 && S_ISDIR(filestat.st_mode)) {
					/* does this much exist as a dir? */
	    *s = '/';			/* mark this as existing */
	    break;
	}
	s = rindex(dirname,'/');	/* shorten name */
	if (!s)				/* relative path! */
	    break;			/* hope they know what they are doing */
	*s = '\0';			/* mark as not existing */
    }
    
    for (s=dirname; s <= end; s++) {	/* this is grody but efficient */
	if (!*s) {			/* something to make? */
	    sprintf(tbptr," %s",dirname);
	    tbptr += strlen(tbptr);	/* make it, sort of */
	    *s = '/';			/* mark it made */
	}
    }
    if (nametype == MD_DIR)		/* don't need final slash unless */
	*end = '\0';			/*  a filename follows the dir name */

    return (tbptr==tmpbuf+5 ? 0 : doshell(sh,tmpbuf));
					/* exercise our faith */
#else
    sprintf(cmd_buf,"%s %s %d", filexp(DIRMAKER), dirname, nametype);
    return doshell(sh,cmd_buf);
#endif
}

void
notincl(feature)
char *feature;
{
    printf("\nNo room for feature \"%s\" on this machine.\n",feature) FLUSH;
}

char *
getval(nam,def)
char *nam,*def;
{
    char *val;

    if ((val = getenv(nam)) == Nullch || !*val)
	val = def;
    return val;
}

/* grow a static string to at least a certain length */

void
growstr(strptr,curlen,newlen)
char **strptr;
int *curlen;
int newlen;
{
    if (newlen > *curlen) {		/* need more room? */
	if (*curlen)
	    *strptr = saferealloc(*strptr,(MEM_SIZE)newlen);
	else
	    *strptr = safemalloc((MEM_SIZE)newlen);
	*curlen = newlen;
    }
}

void
setdef(buffer,dflt)
char *buffer,*dflt;
{
#ifdef STRICTCR
    if (*buffer == ' ')
#else
    if (*buffer == ' ' || *buffer == '\n')
#endif
    {
	if (*dflt == '^' && isupper(dflt[1]))
	    pushchar(Ctl(dflt[1]));
	else
	    pushchar(*dflt);
	getcmd(buffer);
    }
}

void
safelink(old, new)
char *old, *new;
{
#if 0
    extern int sys_nerr;
    extern char *sys_errlist[];
#endif

    if (link(old,new)) {
	printf("Can't link backup (%s) to .newsrc (%s)\n", old, new) FLUSH;
#if 0
	if (errno>0 && errno<sys_nerr)
	    printf("%s\n", sys_errlist[errno]);
#endif
	finalize(1);
    }
}
