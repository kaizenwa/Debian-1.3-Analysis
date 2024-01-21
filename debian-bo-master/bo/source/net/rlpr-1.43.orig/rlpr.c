/* filename: rlpr.c
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr.c,v 1.24 1997/01/14 05:01:02 meem Exp $
 * contents: functions pertinent to the main flow of the program
 * 
 * Time-stamp: <1997/01/13 23:00 -- meem@sherilyn.wustl.edu>
 */

/* copyright (c) 1996, 1997 meem, meem@gnu.ai.mit.edu
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include "config.h"

#ifndef STDC_HEADERS
#error currently there is no support for compiling on machines without ANSI headers. \
       write meem@gnu.ai.mit.edu if you are interested in adding it.
#endif

#include <assert.h>
#include <stdio.h>
#include <string.h>                   /* for strcpy() strcat(), etc */
#include <stdlib.h>                   /* for getenv(), etc */
#include <unistd.h>                   /* for unlink(), getopt(), etc */
#include <fcntl.h>                    /* for O_RDWR, etc */
#include <pwd.h>                      /* for accessing the password file */
#include <stdarg.h>

#ifdef __GNU_LIBRARY__                    
#undef __GNU_LIBRARY__
#include "getopt.h"
#define __GNU_LIBRARY__
#else  /* not __GNU_LIBRARY__ */
#include "getopt.h"
#endif /* not __GNU_LIBRARY__ */

#include "rlpr.h"
#include "rlpr-net.h"                 /* network rlpr commands are here */
#include "rlpr-dbfile.h"              /* accessing the rlpr database file */
#include "rlpr-common.h"              /* common to all rlpr c source files */
#include "rlpr-client.h"              /* common to parts of the rlpr client */

char *     name;                      /* external program name */
print_attr opts_;                     /* global printing options struct */
net_attr   net_;                      /* global network options struct  */


int main(int argc, char *argv[])
{
  int    cfd;                         /* control file descriptor */
  int    sockfd;                      /* the socket we connect to */
  char * cfname;                      /* control file name */
  char * dfname;                      /* data file name */
  char * cffullname;                  /* full pathname to control file */
  int    nf;                          /* number of files spooled */
  int    i;                           /* scrap variable */
  int    orig_argc = argc;            /* original # args */
  name = *argv;                       /* program name */

  toggle_euid();                      /* lose euid of root */

  /* this sets the opts_, props_ and net_ structures */

  argc -= parse_args(orig_argc, argv);
  argv += orig_argc - argc;

  /* create name templates for temp files */
  
  cfname = rlpr_malloc(strlen(net_.localhost) + strlen("cfAxxx") + 1);
  sprintf(cfname, "cfA%03d%s", (int) (getpid() % 1000), net_.localhost);
  dfname     = rlpr_strdup(cfname);
  dfname[0]  = 'd';
  cffullname = rlpr_malloc(strlen(cfname) + strlen(opts_.tmpdir) + 2);
  sprintf(cffullname, "%s/%s", opts_.tmpdir, cfname);

  if (!opts_.wflag) {                 /* if not connecting to a winNT lpd */
    sockfd = open_connection();       /* server now connected */
    send_recvj_req(sockfd);           /* tell lpd we have a job */
  }

  nf = argc ? argc : 1;               /* find out number of files to print */

  /*
   * open the cf once.  we're going to keep reusing the same one
   * on the client side because we only need one cf at a time
   */
  
  if ((cfd = open(cffullname, O_RDWR|O_TRUNC|O_CREAT, 0600)) < 0)
    rlpr_msg(FATAL, ERRNO, "%s - cannot create", cffullname);
  if (unlink(cffullname) < 0)
    rlpr_msg(FATAL, ERRNO, "%s - cannot unlink", cffullname);
  
  do {
    if (opts_.wflag) {                /* if connecting to a winNT lpd */
      sockfd = open_connection();
      send_recvj_req(sockfd);
    }

    lseek(cfd, 0, SEEK_SET);          /* reposition at start of file */

    cf_header(cfd, argc ? *argv : NULL); 

    for (i = 0; i < opts_.copies; i++)
      cf_add(opts_.filetype, dfname, cfd);
    
    cf_add('U', dfname, cfd);

    /* from how i read the rfc, this next 'N' line should not be necessary.
     * however, berkeley lpd's seem to require it here in order for correct
     * processing of the burst page. berkeley nonstandard? :-) never
     */

    {
      static char safe_filename[MAX_SOURCE_LEN + 1];
      cf_add('N', strncpy(safe_filename, argc ? *argv : "", MAX_SOURCE_LEN), cfd);
    }

    send_cf(sockfd, cfd, cfname);

    send_df(sockfd, argc ? *argv : NULL, dfname);
    if (opts_.rflag && argc)          /* unlink the file if user requested to */
      if (unlink(*argv) < 0)          /* and if it's possible... */
        rlpr_msg(WARNING, ERRNO, "%s - cannot unlink", *argv);
    
    argv++;
    dfname[2]++;                      /* inc. letter (dfA --> dfB), etc */
    cfname[2]++;        

    if (opts_.wflag) close_connection(sockfd); /* close sock under NT */
  } while (--argc > 0);
  
  if (!opts_.wflag) close_connection(sockfd);  /* close sock */
  
  rlpr_msg(INFO, NO_ERRNO, "%i file%s spooled to %s:%s (proxy: %s)",
           nf, (nf > 1) ? "s" : "", net_.printhost, opts_.printer,
           net_.proxyhost ? net_.proxyhost : "none");
  
  /* free() stuff.. totally unnecessary but morally satisfying */

  free(cffullname);
  free(cfname);
  free(dfname);
  
  exit(EXIT_SUCCESS);
}

/* add an entry to the control file */

void cf_add(char op, const char *str, int cfd)
{
  /* we could do an sprintf(), but we'd waste a lot of our time just
     malloc()'ing and free()'ing, so instead just employ brute force */

  safe_writen(cfd, &op, 1);
  safe_writen(cfd, str, strlen(str));
  safe_writen(cfd, "\n", 1);
}

/* uses options struct to put standard header on control file */

void cf_header(int cfd, char *filename)
{
  cf_add('H', net_.localhost, cfd);   /* hostname */
  cf_add('P', opts_.user,     cfd);   /* responsible user */

  if (opts_.indent_cols)              /* indent if appropriate */
    cf_add('I', opts_.indent_cols, cfd);

  if (opts_.mflag)                    /* mail user if appropriate */
    cf_add('M', opts_.user,   cfd);         

  if (opts_.bflag) {                  /* print burst page */

    /* the defaults on the 'J' and 'C' options are meant to appease
     * berkeley lpd, which is apparently more finicky than the RFC
     * calls for.. also the ordering of these commands mirrors
     * berkeley lpr, in case anyone is relying on the behavior
     */
    
    cf_add('J', opts_.job   ? opts_.job   : filename ? filename : "stdin", cfd);
    cf_add('C', opts_.class ? opts_.class : net_.localhost, cfd);
    cf_add('L', opts_.user,   cfd);
  }

  /* another berkeleyism that disagrees with the RFC */
  if (strcmp(opts_.width, DEFAULT_WIDTH) != 0)
    cf_add('W', opts_.width,  cfd);   /* width for f, l and p files */

  if (opts_.title)                    /* title for pr(1) */
    cf_add('T', opts_.title,  cfd);
}

int parse_args(int argc, char *argv[])
{
  int c, num_args = 1;               /* the number of arguments read */

  static struct option long_opts[] = {
    { "copies",    1, NULL, '#' },
    { "debug",     0, NULL, -6  },
    { "help",      0, NULL, -2  },
    { "indent",    1, NULL, 'i' },
    { "job",       1, NULL, 'J' },
    { "mail",      0, NULL, 'm' },
    { "no-burst",  0, NULL, 'h' },
    { "port",      1, NULL, -4  },
    { "printhost", 1, NULL, 'H' },
    { "printer",   1, NULL, 'P' },
    { "proxy",     1, NULL, 'X' },
    { "proxyhost", 1, NULL, 'X' },
    { "queue",     1, NULL, 'P' },
    { "quiet",     0, NULL, 'q' },
    { "remove",    0, NULL, 'r' },
    { "silent",    0, NULL, 'q' },
    { "tmpdir",    1, NULL, -3  },
    { "title",     1, NULL, 'T' },
    { "user",      1, NULL, 'U' },
    { "width",     1, NULL, 'w' },
    { "windows",   1, NULL, 'W' },
    { "version",   0, NULL, 'V' },
    { "verbose",   0, NULL, -5  },
    { 0, 0, 0, 0 }
  };

  init_options();                     /* initialize options struct */
  
  while ((c = getopt_long(argc, 
                          argv, 
                          "1234cdfglnopqtv#:hi::mrsw::C:FH:J:P:Q:T:U:VWX:",
                          long_opts,
                          NULL)) != EOF) {
    num_args++;
    switch(c) {

    case '1':                         /* troff type R */
    case '2':                         /* troff type I */
    case '3':                         /* troff type B */
    case '4':                         /* troff type S */
    case 'c':                         /* cifplot */
    case 'd':                         /* TeX/DVI */
    case 'f':                         /* fortran */
    case 'g':                         /* graph */
    case 'l':                         /* leave control characters */
    case 'n':                         /* ditroff */
    case 'o':                         /* postscript (not in rlpr) */
    case 'p':                         /* pr */
    case 't':                         /* troff */
    case 'v':                         /* raster input */
      opts_.filetype = (c == 'f') ? 'r' : c;
      break;
    case '#':                         /* number of copies to make */
      opts_.copies = atoi(optarg);
      break;
    case 'h':                         /* suppress printing of burst page */
      opts_.bflag  = 0;
      break;
    case 'i':                         /* indentation */
      opts_.indent_cols  = optarg ? optarg : DEFAULT_INDENT_NO_PARAM;
      break;
    case 'm':                         /* send mail after printing */
      opts_.mflag  = 1;
      break;
    case 'q':                         /* be quiet! */
      props_.quiet = 1;
      break;
    case 'r':                         /* remove file after printing */
      opts_.rflag  = 1;
      break;
    case 's':                         /* for compatibility with BSD lpr */
      rlpr_msg(WARNING, NO_ERRNO, "symlink option not applicable (ignored)");
      break;
    case 'w':                         /* width for pr */
      opts_.width  = optarg ? optarg : DEFAULT_WIDTH_NO_PARAM;
      break;
    case 'C':                         /* class */
      opts_.class  = optarg;
      break;
    case 'F':                         /* form feed after printing */
      opts_.fflag  = 1;
      break;
    case 'H':                         /* for overriding PRINTHOST */
      net_.printhost = optarg;
      break;
    case 'J':                         /* job name on burst page */
      opts_.job = optarg;
      break;
    case 'P':                         /* printer queue name (FALLTHRU) */
    case 'Q':                         /* i think this is a better name */
      opts_.printer = optarg;
      break;
    case 'T':                         /* title for pr (def: filename) */
      opts_.title = optarg;
      break;
    case 'U':                         /* username to print on title... */
      opts_.user = optarg;
      break;
    case 'V':                         /* print version info */
      fprintf(stdout, "%s: version "VERSION" from "__DATE__" "__TIME__ 
              " -- meem@gnu.ai.mit.edu\n", name);
      exit(EXIT_SUCCESS);
    case 'W':
      opts_.wflag = 1;
      break;
    case 'X':                         /* for overriding PROXY  */
      net_.proxyhost = optarg;
      break;
    case -2:
      printf("usage: rlpr [-Hprinthost]  [-Pprinter]  [-Xproxyhost] [OPTIONS] "
             "[file ...]\n\nplease see the manpage for detailed help.\n");
      exit(EXIT_SUCCESS);
    case -3:
      opts_.tmpdir = optarg;          /* tmpdir to use */
      break;
    case -4:
      net_.port = atoi(optarg);       /* port number to connect to */
      break;
    case -5:                          /* verbose flag */
      props_.quiet = 0;
      break;
    case -6:
      props_.debug = 1;
      break;
    case '?':
      fprintf(stderr,"please see manpage for help\n");
      break;
    }
  }
  tidy_and_check_options();
  return num_args;
}


void init_options(void)
{
  char *tmp;

  opts_.fflag    = 0;                 /* don't auto-form feed by default */
  opts_.bflag    = 1;                 /* burst page on by default */
  opts_.mflag    = 0;                 /* don't mail after printing */
  opts_.rflag    = 0;                 /* don't remove after printing */
  opts_.wflag    = 0;                 /* windows braindead mode (off) */
  opts_.copies   = 1;                 /* number of copies to make */
  opts_.job      = NULL;
  opts_.class    = NULL;
  opts_.title    = NULL;
  opts_.filetype = 'f';               /* default to a regular file */
  opts_.width    = DEFAULT_WIDTH;
  opts_.indent_cols = NULL;           /* number of columns to indent output */

  { /* find out what user we are -- user can always provide an alternate
     * choice with the -u option
     */

    struct passwd *pw;
    if ((pw = getpwuid(getuid())) != NULL)
      opts_.user = rlpr_strdup(pw->pw_name);
    else rlpr_msg(FATAL, ERRNO, "who are you?");
  }
  
  /* be quiet by default if we were called as "lpr" */
  props_.quiet   = strcmp(name, "lpr") ? 0 : 1;
  props_.debug   = 0;                 /* debugging off by default */
  props_.syslog  = 0;

  tmp = getenv("TMPDIR");
  opts_.tmpdir   = tmp ? tmp : DEFAULT_TMP_DIR;
  
  /* first consult PRINTER, if it's NULL then LPDEST */
  tmp = getenv("PRINTER");
  opts_.printer  = tmp ? tmp : getenv("LPDEST");        

  /* configure net_ stuff */
  net_.port      = DEFAULT_RLPRD_TO_PORT;
  tmp = getenv(PROXYHOST);
  net_.proxyhost = tmp ? tmp : getenv(PROXYHOST2);

  net_.printhost = getenv(PRINTHOST);
  net_.localhost = rlpr_malloc(MAXHOSTNAMELEN);
  if (get_local_hostname(net_.localhost, MAXHOSTNAMELEN) == -1)
    rlpr_msg(FATAL, NO_ERRNO, "unable to resolve your local hostname!");
}

void tidy_and_check_options(void)
{
  /* resolve remainder of printer/printhost information */
  
  if (net_.printhost == NULL && opts_.printer == NULL)
    rlpr_msg(FATAL, NO_ERRNO, "no printhost or printer set - read %s(1)", name);

  if (net_.printhost == NULL)
    net_.printhost = gethostfromq(opts_.printer);
  else if (opts_.printer == NULL)
    opts_.printer  = getqfromhost(net_.printhost);

  /* now we need to go through all the options and make sure they're
   * not too large.. if they are, we may overflow the limits of some
   * poorly-coded lpd's
   */

  rlpr_msg(DEBUG, NO_ERRNO, "checking user-supplied params...");
  if (opts_.printer)  assert(strlen(opts_.printer)  < MAX_QUEUE_LEN);
  if (opts_.title)    assert(strlen(opts_.title)    < MAX_TITLE_LEN);
  if (opts_.job)      assert(strlen(opts_.job)      < MAX_JOB_LEN);
  if (opts_.user)     assert(strlen(opts_.user)     < MAX_USER_LEN);
  if (opts_.class)    assert(strlen(opts_.class)    < MAX_CLASS_LEN);
  if (net_.localhost) assert(strlen(net_.localhost) < MAXHOSTNAMELEN);
  if (net_.printhost) assert(strlen(net_.printhost) < MAX_HOST_LEN);
  if (net_.proxyhost) assert(strlen(net_.proxyhost) < MAXHOSTNAMELEN);
  if (opts_.width)    assert(strlen(opts_.width)    < MAX_STR_LEN);
  if (opts_.indent_cols) assert(strlen(opts_.indent_cols) < MAX_STR_LEN);
  rlpr_msg(DEBUG, NO_ERRNO, "passed");
}

