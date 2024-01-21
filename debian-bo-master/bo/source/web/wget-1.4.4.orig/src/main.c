/* Command line parsing.
   
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* $Id: main.c,v 1.1.1.1.2.3 1997/02/15 19:23:07 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif /* HAVE_STRING_H */
#include <assert.h>
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "wget.h"
#include "options.h"
#include "main.h"
#include "utils.h"
#include "url.h"
#include "init.h"
#include "retr.h"
#include "recur.h"
#include "ftp.h"
#include "getopt.h"

#ifdef WINDOWS
#  include "wsstartup.h"
#endif

extern char *version_string;
extern int errno;

struct options opt;

int
main(int argc, char *const *argv)
{
   char **url, **t, *new;
   char *usage = "Usage: %s [options] [URL-list]\n";
   int i, c, nurl, status, append_to_log, first_time, dt;
   const char *exec_name;
   char *com, *val;

   static struct option long_options[] =
   {
      { "backups", no_argument, NULL, 'b' }, /* undocumented! */
      { "continue", no_argument, NULL, 'c' },
      { "convert-links", no_argument, NULL, 'k' },
      { "debug", no_argument, NULL, 'd' },
      { "email-address", no_argument, NULL, 'E' }, /* undocumented! */
      { "follow-ftp", no_argument, NULL, 14 },
      { "force-hier", no_argument, NULL, 'x' },
      { "force-html", no_argument, NULL, 'F'},
      { "help", no_argument, NULL, 'h' },
      { "ignore-length", no_argument, NULL, 10 },
      { "mirror", no_argument, NULL, 'm' },
      { "no-clobber", no_argument, NULL, 13 },
      { "no-parent", no_argument, NULL, 5 },
      { "passive-ftp", no_argument, NULL, 11 },
      { "quiet", no_argument, NULL, 'q' },
      { "recursive", no_argument, NULL, 'r' },
      { "relative", no_argument, NULL, 'L' },
      { "retr-symlinks", no_argument, NULL, 9 },
      { "save-headers", no_argument, NULL, 's' },
      { "server-response", no_argument, NULL, 'S' },
      { "span-hosts", no_argument, NULL, 'H' },
      { "spider", no_argument, NULL, 4 },
      { "timestamping", no_argument, NULL, 'N' },
      { "verbose", no_argument, NULL, 'v' },
      { "version", no_argument, NULL, 'V' },
      
      { "accept", required_argument, NULL, 'A' },
      { "append-output", required_argument, NULL, 'a' },
      { "base", required_argument, NULL, 'B' },
      { "cache", required_argument, NULL, 'C' },
      { "delete-after", no_argument, NULL, 8 },
      { "directory-prefix", required_argument, NULL, 'P' },
      { "domains", required_argument, NULL, 'D' },
      { "dot-style", required_argument, NULL, 6 },
      { "execute", required_argument, NULL, 'e' },
      { "exclude-directories", required_argument, NULL, 'X' },
      { "exclude-domains", required_argument, NULL, 12 },
      { "glob", required_argument, NULL, 'g' },
      { "header", required_argument, NULL, 3 },
      { "htmlify", required_argument, NULL, 7 },
      { "http-passwd", required_argument, NULL, 2 },
      { "http-user", required_argument, NULL, 1 },
      { "include-directories", required_argument, NULL, 'I' },
      { "input-file", required_argument, NULL, 'i' },
      { "level", required_argument, NULL, 'l' },
      { "no", required_argument, NULL, 'n' },
      { "output-document", required_argument, NULL, 'O' },
      { "output-file", required_argument, NULL, 'o' },
      { "proxy", required_argument, NULL, 'Y' },
      { "proxy-passwd", required_argument, NULL, 16 },
      { "proxy-user", required_argument, NULL, 15 },
      { "quota", required_argument, NULL, 'Q' },
      { "reject", required_argument, NULL, 'R' },
      { "timeout", required_argument, NULL, 'T' },
      { "tries", required_argument, NULL, 't' },
      { "user-agent", required_argument, NULL, 'U' }, /* undocumented! */
      { "use-proxy", required_argument, NULL, 'Y' },
      { "wait", required_argument, NULL, 'w' },
      { 0, 0, 0, 0 }
   };

   append_to_log = 0;
   initialize();

   /* Construct the name of the executable, without the directory
      part. */
   exec_name = strrchr(argv[0], '/');
   if (!exec_name)
      exec_name = argv[0];
   else
      ++exec_name;

   while ((c = getopt_long(argc, argv, "hVqvdksxmNWrHSLcFbEY:g:T:U:O:l:n:i:o:a:t:D:A:R:P:B:e:Q:X:I:w:",
			   long_options, (int *)0)) != EOF)
   {
      switch(c)
      {
	 /* Options without arguments: */
	 case 4:
	    setval("spider", "on");
	    break;
	 case 5:
	    setval("noparent", "on");
	    break;
	 case 8:
	    setval("deleteafter", "on");
	    break;
	 case 9:
	    setval("retrsymlinks", "on");
	    break;
	 case 10:
	    setval("ignorelength", "on");
	    break;
	 case 11:
	    setval("passiveftp", "on");
	    break;
	 case 13:
	    setval("noclobber", "on");
	    break;
	 case 14:
	    setval("followftp", "on");
	    break;
	 case 'b':
	    setval("backups", "on");
	    break;
	 case 'c':
	    setval("alwaysrest", "on");
	    break;
	 case 'd':
	    setval("debug", "on");
	    break;
	 case 'E':
	    printf("%s\n", opt.ftp_pass);
	    exit(0);
	    break;
	 case 'F':
	    setval("forcehtml", "on");
	    break;
	 case 'H':
	    setval("spanhosts", "on");
	    break;
	 case 'h':
	    printhelp();
	    exit(0);
	    break;
	 case 'k':
	    setval("convertlinks", "on");
	    break;
	 case 'L':
	    setval("relativeonly", "on");
	    break;
	 case 'm':
	    setval("mirror", "on");
	    break;
	 case 'N':
	    setval("timestamping", "on");
	    break;
	 case 'S':
	    setval("serverresponse", "on");
	    break;
	 case 's':
	    setval("saveheaders", "on");
	    break;
	 case 'q':
	    setval("quiet", "on");
	    break;
	 case 'r':
	    setval("recursive", "on");
	    break;
	 case 'V':
	    printf("%s\n", version_string);
	    exit(0);
	    break;
	 case 'v':
	    setval("verbose", "on");
	    break;
	 case 'x':
	    setval("dirstruct", "on");
	    break;

	    /* Options accepting an argument: */
	 case 1:
	    setval("httpuser", optarg);
	    break;
	 case 2:
	    setval("httppasswd", optarg);
	    break;
	 case 3:
	    setval("header", optarg);
	    break;
	 case 6:
	    setval("dotstyle", optarg);
	    break;
	 case 7:
	    setval("htmlify", optarg);
	    break;
	 case 12:
	    setval("excludedomains", optarg);
	    break;
	 case 15:
	    setval("proxyuser", optarg);
	    break;
	 case 16:
	    setval("proxypasswd", optarg);
	    break;
	 case 'A':
	    setval("accept", optarg);
	    break;
	 case 'a':
	    setval("logfile", optarg);
	    append_to_log = 1;
	    break;
	 case 'B':
	    setval("basehref", optarg);
	    break;
	 case 'C':
	    setval("cache", optarg);
	    break;
	 case 'D':
	    setval("domains", optarg);
	    break;
	 case 'e':
	    if (parse_line((unsigned char *)optarg,
			   (unsigned char **)&com, (unsigned char **)&val))
	    {
	       if (!setval(com, val))
		  exit(1);
	       free(com);
	       free(val);
	    }
	    else
	    {
	       fprintf(stderr, "%s: invalid command (try --help)\n", optarg);
	       exit(1);
	    }
	    break;
	 case 'g':
	    setval("glob", optarg);
	    break;
	 case 'I':
	    setval("includedirectories", optarg);
	    break;
	 case 'i':
	    setval("input", optarg);
	    break;
	 case 'l':
	    setval("reclevel", optarg);
	    break;
	 case 'n':
	    for (i = 0; i < strlen(optarg); i++)
	       switch(optarg[i])
	       {
		  case 'v':
		     setval("verbose", "off");
		     break;
		  case 'h':
		     setval("simplehostcheck", "on");
		     break;
		  case 'H':
		     setval("addhostdir", "off");
		     break;
		  case 'd':
		     setval("dirstruct", "off");
		     break;
		  case 'c':
		     setval("noclobber", "on");
		     break;
		  case 'r':
		     setval("removelisting", "off");
		     break;
		  case 'p':
		     setval("noparent", "on");
		     break;
		  default:
		     printf("-n%c option is unknown. Try %s -h.\n", optarg[i],
			    exec_name);
		     exit(1);
	       }
	    break;
	 case 'O':
	    setval("outputdocument", optarg);
	    break;
	 case 'o':
	    setval("logfile", optarg);
	    break;
	 case 'P':
	    setval("dirprefix", optarg);
	    break;
	 case 'Q':
	    setval("quota", optarg);
	    break;
	 case 'R':
	    setval("reject", optarg);
	    break;
	 case 'T':
	    setval("timeout", optarg);
	    break;
	 case 't':
	    setval("numtries", optarg);
	    break;
	 case 'U':
	    setval("useragent", optarg);
	    break;
	 case 'w':
	    setval("wait", optarg);
	    break;
	 case 'X':
	    setval("excludedirectories", optarg);
	    break;
	 case 'Y':
	    setval("useproxy", optarg);
	    break;

	 case '?':
	    printf(usage, exec_name);
	    exit(0);
	    break;
      }
   }
   if (opt.verbose == -1)
      opt.verbose = !opt.quiet;

   /* Sanity checks. */
   if (opt.verbose && opt.quiet)
   {
      printf("Can't be verbose and quiet at the same time.\n");
      printf(usage, exec_name);
      exit(1);
   }
   if (opt.timestamping && opt.noclobber)
   {
      printf("Can't timestamp and not clobber old files at the same time.\n");
      printf(usage, exec_name);
      exit(1);
   }
   nurl = argc - optind;
   if (!nurl && !opt.input_filename)
   {
      /* No URL specified. */
      printhelp();
      exit(1);
   }
   /* Allocate basic pointer. */
   url = (char **)nmalloc((nurl + 1) * sizeof(char *));
   /* Fill in the arguments. */
   for (i = 0; i < nurl; i++, optind++)
      url[i] = nstrdup(argv[optind]);
   url[i] = NULL;

/* Change the title of console window on Windows. */
#ifdef WINDOWS
   ws_changetitle(nurl, url);
#endif

   /* Open log filename. */
   if (opt.lfilename)
   {
      opt.lfile = fopen(opt.lfilename, append_to_log ? "a" : "w");
      if (opt.lfile == NULL)
      {
	 perror(opt.lfilename);
	 exit(1);
      }
   }
   else
      opt.lfile = stdout;
   /* Change the buffering. */
   setbuf(opt.lfile, NULL);
#ifdef DEBUG
   if (opt.debug)
      fprintf(opt.lfile, "DEBUG output created by %s on %s.\n\n",
	      version_string, OS_TYPE);
#endif
   /* Open the output filename (if necessary). */
   if (opt.output_document)
   {
      if (ISHYPHEN(opt.output_document))
	 opt.dfp = stdout;
      else                      /* not ISHYPHEN(...) */
      {
	 opt.dfp = fopen(opt.output_document, "wb");
	 if (opt.dfp == NULL)
	 {
	    perror(opt.output_document);
	    exit(1);
	 }
      }
   } /* opt.output_document */

#ifdef WINDOWS
   wsock_startup();
#endif

   /* Setup the signal handler. */
#ifdef HAVE_SIGNAL
   signal(SIGHUP, hangup);
#endif

   status = RETROK;             /* Initialize it, just-in-case. */
   /* Retrieve the URLs from argument list. */
   for (first_time = 1, t = url; *t; t++)
   {
      char *filename;
      status = retrieve_url(*t, &filename, &new, NULL, &dt);
      if (opt.recursive && status == RETROK && (dt & TEXTHTML))
	 status = recursive_retrieve(filename, new ? new : *t,
				     first_time ? RFIRST_TIME : 0);
      if (new)
	 free(new);
      if (filename)
	 free(filename);
      first_time = 0;
   }

   /* And then from the input file (if exists). */
   if (opt.input_filename)
   {
      int count;
      status = retrieve_from_file(opt.input_filename, opt.force_html, &count);
      if (!count && !opt.quiet)
	 fprintf(opt.lfile, "No URLs found in %s.\n", opt.input_filename);
   }
   /* Print the downloaded sum. */
   if (!opt.quiet && (opt.recursive || nurl > 1
		      || (opt.input_filename && opt.downloaded != 0)))
   {
      fprintf(opt.lfile,
	      "\nFINISHED --%s--\nDownloaded: %s bytes in %d files\n",
	      time_str(NULL), legible(opt.downloaded), opt.numurls);
      /* Print quota warning, if exceeded. */
      if (opt.quota && opt.downloaded > opt.quota)
	 fprintf(opt.lfile, "Download quota (%s bytes) EXCEEDED!\n",
		 legible(opt.quota));
   }
   /* Convert all links.  */
   if (opt.convert_links)
   {
      convert_all_links();
   }
   /* Close the log file. */
   fclose(opt.lfile);
   /* Free the local data. */
   free_vec(url);
   /* ...and clean up the global data. */
   cleanup();
   if (status == RETROK)
      return 0;
   else
      return 1;
}


/* Print the help message. */
void
printhelp(void)
{
   printf("      GNU %s by Hrvoje Niksic <hniksic@srce.hr>\n",
	  version_string);
   printf("Options:\n\
      -o --output-file      log messages to logfile\n\
      -i --input-file       read URL-s from file\n\
      -q --quiet            quiet (no output)\n\
      -t --tries            set number of retries per URL (0 to unlimit)\n\
      -r --recursive        recursive web-suck -- use with care!\n\
      -l --level            maximum recursion depth (0 to unlimit)\n\
      -D --domains          comma-separated list of domains to accept\n\
      -A --accept           comma-separated list of extensions to accept\n\
      -R --reject           comma-separated list of extensions to reject\n\
      -c --continue-ftp     restart getting an existing file\n\
Refer to the info documentation for a complete list of options.\n\
Mail bug reports to <bug-wget@prep.ai.mit.edu>.\n\n\
Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.\n\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n");
}

/* Hangup signal handler. When wget receives hangup, it will proceed
   operation as usual, trying to write into a log file. If that is
   impossible, the output will be turned off. */

#ifdef HAVE_SIGNAL
RETSIGTYPE
hangup(int sig)
{
   char *logname;
   int count;
   
   signal(SIGHUP, hangup);
   if (opt.lfile != stdout)
      return;
   for (count = 0; (logname = unique_name(DEFAULT_LOGFILE, count)) == NULL;
	count++)
      ;
   opt.lfile = fopen(logname, "w");
   if (opt.lfile == NULL)
   {
      printf("%s: %s\n", logname, mystrerror(errno));
      opt.lfile = fopen("/dev/null", "w");
      assert(opt.lfile != NULL);
   }
   /* Change the buffering. */
   setbuf(opt.lfile, NULL);
   printf("SIGHUP received, redirecting output to %s.\n", logname);
   free(logname);
}
#endif /* HAVE_SIGNAL */
