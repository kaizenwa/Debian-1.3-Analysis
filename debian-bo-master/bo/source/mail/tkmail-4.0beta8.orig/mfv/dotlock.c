static char rcsid[] = "$Header$";
/*
 * dotlock - creates/deletes *.lock file for given filename
 *				Paul Raines (raines@slac.stanford.edu)
 *
 * NOTES:
 *    Note that not all system use the *.lock convention for locking
 *    mail files. You should test it before assuming it works.
 *
 *    It is my belief it is safe to set the dotlock binary set setuid
 *    root if it is necessary to create files in the mail spool
 *    directory. The program will reset the uid back to the real user
 *    id unless the --system option is given and this will only allow
 *    creation of the hard coded file <user>.lock in the mail spool
 *    directory as set by the mailsysdir constant below.
 *
 *    If it is possible (as it should be on well designed system), dotlock
 *    should not be setuid, but setgid to the group of the mail spool directory
 *    and the directory should be writable by that group which is designed soley
 *    for that purpose (such as 'mail' but not 'wheel', 'sys', 'bin', ...)
 *
 * COPYRIGHT:
 * 	Copyright 1994 by Paul Raines (raines@slac.stanford.edu)
 *  
 * 	Permission to use, copy, modify, and distribute this software and
 * 	its documentation for any purpose and without fee is hereby
 * 	granted, provided that the above copyright notice appear in all
 * 	copies.  The University of Pennsylvania makes no representations
 * 	about the suitability of this software for any purpose.  It is
 * 	provided "as is" without express or implied warranty.
 *  
 * DISCLAIMER:
 * 	UNDER NO CIRCUMSTANCES WILL THE AUTHOR OF THIS SOFTWARE OR THE
 * 	UNIVERSITY OF PENNSYLVANIA, STANFORD UNIVERSITY, THE STANFORD
 * 	LINEAR ACCELERATOR CENTER, OR THE DEPARTMENT OF ENERGY BE HELD
 * 	RESPONSIBLE FOR ANY DIRECT OR INCIDENTAL DAMAGE ARISING FROM THE
 * 	USE OF THIS SOFTWARE AND ITS DOCUMENTATION. THE SOFTWARE HEREIN IS
 * 	PROVIDED "AS IS" WITH NO IMPLIED OBLIGATION TO PROVIDE SUPPORT,
 * 	UPDATES, OR MODIFICATIONS.
 *  
 *  Please mail any suggestions, bugs, whines to raines@slac.stanford.edu
 *  
 *  HISTORY:
 *   raines - Jan 19, 1996: Created.
 *   See Changelog.
 */

#include <stdio.h>
#include <sys/types.h>
#include <ctype.h>
#ifdef NO_STDLIB_H
# include "compat/stdlib.h"
#else
# include <stdlib.h>
#endif
#ifdef NO_STRING_H
# include "compat/string.h"
#else
# include <string.h>
#endif
#ifdef NO_UNISTD_H
# include "compat/unistd.h"
#else
# include <unistd.h>
#endif

/* of course, if you don't have one of the above, your probably don't have
   at least one of the below, but I am not prepared to handle that yet */
#include <errno.h>
#include <pwd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define ckstrdup(sourceStr) \
  (strcpy (ckmalloc (strlen (sourceStr) + 1), sourceStr))

/* files used */
char *baseprog = NULL;
char *fname = NULL;
char *fdir = NULL;
char *lockfile = NULL;

/* options */
char lockit = TRUE;
char sysmail = FALSE;
int  retries = -1;
int  timeout = 5;
char override = FALSE;
char quiet = FALSE;

/* string globals */
char tmpsfx[] = "/mfvXXXXXX";
#ifdef MAILSPOOLDIR
char sysmaildir[] = MAILSPOOLDIR;
#else
char sysmaildir[] = "/usr/mail";
#endif

#ifndef HAVE_STRERROR
char *strerror(int errnum)
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return (char *) "Unknown error";
}
#endif

static void *ckmalloc (unsigned int size)
{
  void *result = (void *) malloc (size);
  if (!result) {
    fprintf(stderr, "ERROR::%s: virtual memory exhausted\n", baseprog);
    exit(1);
  }
  return result;
}

void usage(char *mesg, char *data, int stat)
{
  FILE *fp = (stat > 0 ? stderr : stdout);
  
  if (mesg) {
    fprintf(fp,	"%s::%s", fp == stderr ? "ERROR" : "INFO", mesg);

    if (data) fprintf(fp, ": \"%s\"", data);
    fprintf(fp, "\n\n");
  }
  
  fprintf(fp,
      "Usage:  %s [<options>] <foldername>\n"
      "        %s [<options>] [-s | --system]\n\n"
      "Options: -l, --lock             Lock folder (default)\n"
      "         -u, --unlock           Unlock folder\n"
      "         -o, --override         Override lock if all tries fail\n"
      "         -q, --quiet            Only report system errors\n"
      "         -r, --retries count    Number times to retry (default -1)\n"
      "         -t, --timeout seconds  Seconds between retries (default 5)\n"
      "         -w, --where            Simply reports where mail spool is\n"
      "         --                     End of options\n\n"
      "Locks/unlocks folder by creating/deleting a <foldername>.lock file.\n"
      "If --system option is given, folder is taken as %s/<username>.\n"
      "If --retries argument is negative, it makes infinitie retries\n"
      "unless --override is given.\n\n", baseprog, baseprog, sysmaildir);

  exit(stat);
}

/* User is responsible for freeing response */
char *ckgetcwd()
{
  size_t len = 0;
  char *cwd = NULL;

  do {
    free(cwd);
    len += 255;
    if ((cwd = ckmalloc(sizeof(char)*len)) == NULL) return NULL;
  } while ( getcwd(cwd,len) == NULL && errno == ERANGE );
  return cwd;
}

main(int argc, char **argv)
{
  char c, *p, *opt, longopt, *tempfile;
  int i, status, fd, tries;
  size_t length;
  struct stat statbuf;
  struct passwd *pw;

  p = strrchr(argv[0], '/');
  baseprog = (p == NULL ? argv[0] : p+1);
  
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    p = argv[i]+1;
    longopt = FALSE;
    
    if (*p == '-') {
      p++;
      if (*p == 0) { i++; break; }
      longopt = TRUE;
    } else {
      longopt = FALSE;
    }
    length = strlen(p);

    while (*p != 0) {
      switch (*p) {
      case 'l':
	if (!longopt || strncmp(p, "lock", length) == 0) {
	  lockit = TRUE;
	} else goto badopt;
	break;
      case 'u':
	if (!longopt || strncmp(p, "unlock", length) == 0) {
	  lockit = FALSE;
	} else goto badopt;
	break;
      case 's':
	if (!longopt || strncmp(p, "system", length) == 0) {
	  sysmail = TRUE;
	} else goto badopt;
	break;
      case 'r':
	if (!longopt || strncmp(p, "retries", length) == 0) {
	  if (i >= argc)
	    usage("missing count argument for retries", NULL, 1);
	  if (sscanf(argv[++i], "%d", &retries) < 1)
	    usage("invalid count argument for retries", argv[i], 1);
	} else goto badopt;
	break;
      case 't':
	if (!longopt || strncmp(p, "timeout", length) == 0) {
	  if (i >= argc)
	    usage("missing seconds argument for timeout", NULL, 1);
	  if (sscanf(argv[++i], "%d", &timeout) < 1 || timeout < 1)
	    usage("invalid seconds argument for timeout", argv[i], 1);
	} else goto badopt;
	break;
      case 'o':
	if (!longopt || strncmp(p, "override", length) == 0) {
	  override = TRUE;
	} else goto badopt;
	break;
      case 'q':
	if (!longopt || strncmp(p, "quiet", length) == 0) {
	  quiet = TRUE;
	} else goto badopt;
	break;
      case 'w':
	printf("%s\n", sysmaildir);
	exit(0);
      case 'h':
	usage(NULL, NULL, 0);
      default:
	badopt:
	if (!longopt) *(p+1) = 0;
	usage("bad option", p, 1);
      }
      
      if (longopt) break;
      else p++;
    }
  }

  if (i >= argc && !sysmail)
    usage("missing <foldername> argument", NULL, 1);
  else if ((sysmail && i < argc) || i < argc-1)
    usage("too many <foldername> arguments", NULL, 1);

  if (sysmail) {
    fdir = sysmaildir;
    pw = getpwuid(getuid());
    fname = (char *) ckmalloc(strlen(fdir) + strlen(pw->pw_name) + 3);
    strcpy(fname, fdir);
    strcat(fname, "/");
    strcat(fname, pw->pw_name);
  } else {
    if (setuid(getuid()) != 0) {
      fprintf(stderr, "ERROR::%s: can't setuid back to user -- %s\n",
	  baseprog, strerror(errno));
      exit(1);
    }
    fname = argv[i];
    if (!strlen(fname)) usage("empty <foldername> argument", NULL, 1);
    if(stat(fname, &statbuf) == -1) {
      if (errno != ENOENT) {
	fprintf(stderr, "ERROR::%s: can't stat %s -- %s.\n",
	    baseprog, fname, strerror(errno));
	exit(1);
      }
    } else {
      if (S_ISDIR(statbuf.st_mode))
	usage("Sorry, can't lock directories yet", NULL, 1);
    }
    fdir = ckstrdup(fname);
    if ((p = strrchr(fdir, '/')) != NULL && p != fdir) *p = 0;
    if (p == NULL) strcpy(fdir, ".");
  }
  
  /* check directory */
  if(access(fdir, F_OK) != 0) {
    fprintf(stderr, "ERROR::%s: Directory %s does not exist.\n",
	baseprog, fdir);
    exit(1);
  }

  lockfile = (char *) ckmalloc(strlen(fname) + strlen(".lock") + 1);
  strcpy(lockfile, fname);
  strcat(lockfile, ".lock");

  /* try to create *.lock file */
  if(lockit) {
    
    tempfile = (char *) ckmalloc(strlen(fdir) + strlen(tmpsfx) + 1);
    strcpy(tempfile, fdir);
    strcat(tempfile, tmpsfx);
    mktemp(tempfile);
    unlink(tempfile);

    tries = 0;
    while(1) {
      fd = open(tempfile, O_WRONLY | O_CREAT | O_EXCL, 0666);
      if (fd < 0) {
	fprintf(stderr, "ERROR::%s: creating %s -- %s\n", baseprog,
	    lockfile, strerror(errno));
	exit(1);
      }
      close(fd);
      
      status = link(tempfile, lockfile);
      unlink(tempfile);
      if (status == 0) {
	break;
      }
      if (errno != EEXIST) {
	fprintf(stderr, "ERROR::%s: %s -- %s\n", baseprog,
	    lockfile, strerror(errno));
	exit(1);
      }

      if (++tries > retries && (retries > -1 || override) ) {
	if (override) {
	  unlink(lockfile);
	  tries = 0;
	  continue;
	}
	if (!quiet) fprintf(stderr, "FAIL::%s: could not lock %s.\n",
	    baseprog, fname);
	exit(2);
      }

      sleep(timeout);
    }
        
  } else {
    if (access(lockfile, F_OK) == 0 && unlink(lockfile) != 0) {
      fprintf(stderr, "ERROR::%s: %s -- %s\n", baseprog,
	  lockfile, strerror(errno));
      exit(1);
    }
  }
  
  exit(0);  
}
