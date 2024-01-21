/*
 * This file is part of uudeview, the simple and friendly multi-part multi-
 * file uudecoder  program  (c)  1994 by Frank Pilhofer. The author may be
 * contacted by his email address,          fp@informatik.uni-frankfurt.de
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef SYSTEM_WINDLL
#include <windows.h>
#endif
#ifdef SYSTEM_OS2
#include <os2.h>
#endif

/*
 * Main function for standalone uuenview
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <stdio.h>
#include <time.h>

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif


#include <uudeview.h>
#include <fptools.h>
#include <uufnflt.h>


char * uuenview_id = "$Id: uuenview.c,v 1.14 1996/09/26 17:32:20 fp Exp $";

/*
 * mail and news software
 */

#ifdef PROG_INEWS
char *  uue_inewsprog = PROG_INEWS;
#else
char *  uue_inewsprog = NULL;
#endif
#ifdef PROG_MAILER
char *  uue_mailprog  = PROG_MAILER;
#else
char *  uue_mailprog  = NULL;
#endif
#ifdef MAILER_NEEDS_SUBJECT
int     uue_mpsubject = 1;
#else
int     uue_mpsubject = 0;
#endif

/*
 * defines
 */

#define UUE_TOFILE	1
#define UUE_MAILTO	2
#define UUE_POSTTO	3

/*
 * the progress meter is only displayed if stderr is a terminal.
 * take care of systems where we can't check this condition.
 */

#ifdef HAVE_ISATTY
#define UUISATTY(f)	(isatty(fileno(f)))
#else
#define UUISATTY(f)	(1)
#endif

/*
 * Message Callback
 */

static void
MessageCallback (void *param, char *message, int level)
{
#if 0
  if (UUGetOption (UUOPT_VERBOSE, NULL, NULL, 0) || level >= UUMSG_WARNING) {
    fprintf (stderr, "%70s\r", "");
    fprintf (stderr, "%s\n", message);
  }
#else
  if (level >= UUMSG_WARNING) {
    fprintf (stderr, "%70s\r", "");
    fprintf (stderr, "%s\n", message);
  }
#endif
}

static int
BusyCallback (void *param, uuprogress *progress)
{
  char stuff[26];
  int count, pcts;
  char *ptr;

  /*
   * Display progress meter only if stderr is a terminal
   */

  if (!UUISATTY(stderr))
    return 0;

  if ((ptr = _FP_strrchr (progress->curfile, DIRSEPARATOR[0])) == NULL)
    ptr = progress->curfile;
  else
    ptr++;

  if (progress->action == 4 && UUGetOption (UUOPT_VERBOSE, NULL, NULL, 0)) {
    pcts = (int)((100*progress->partno+progress->percent-100) /
		 progress->numparts);
    for (count=0, stuff[25]='\0'; count<25; count++)
      stuff[count] = (count<pcts/4)?'#':'.';
    fprintf (stderr, "encoding part%3d of%3d %s\r",
	     progress->partno, progress->numparts,
	     stuff);
    fflush  (stderr);
  }
  return 0;
}

/*
 * usage
 */

static void
usage (char *argv0)
{
  if (_FP_stristr (argv0, "uuencode") != NULL) {
    printf ("\n\
  uuencode -- a simple encoder (w) 1995 Frank Pilhofer\n\n\
  usage:\n\
    %s [infile] remotefile\n\n\
",
	    argv0);
    printf ("\
\tinfile      the local file, where to read data from. If no infile\n\
\t            is given, or infile is a single hyphen, the standard\n\
\t            input is used instead.\n\
\tremotefile  the name as which the recepient will receive the file\n\
\n\
  For much more powerful encoding facilities, try calling this program\n\
  as 'uuenview'.\n\
\n\
");
    return;
  }
  printf ("\n\
  UUENVIEW %spl%s -- a simple encoder (w) 1995 Frank Pilhofer\n\n\
  usage:\n\
    %s [-type] [-lines] ", VERSION, PATCH, argv0);
#ifndef SYSTEM_DOS
  printf ("[-m address] ");
  printf ("[-p newsgroup] ");
  printf ("\n\t\t");
  printf ("[-s subject] ");
  printf ("[-o[d path]] ");
#else
  printf ("[-od path] ");
#endif
  printf ("[-a] ");
  printf ("file(s)\n\n");
  printf ("\
\ttype      one of 'u', 'x', 'b' for UUencoding, XXencoding or Base64\n\
\t          encoding, respectively. Defaults to uuencoding.\n\
\tlines     sets the number of encoded lines per file. Values smaller\n\
\t          than 200 are ignored. 1000 lines encode 45kb UU/XX and 57kb\n\
\t          Base64 data. Default is unlimited.\n\
");
#ifndef SYSTEM_DOS
  printf ("\
\taddress   comma-separated list of email recepients.\n\
\tnewsgroup comma-separated list of newsgroups to post the file to.\n\
\tsubject   used on the subject line for emailing and posting.\n\
");
#endif
  printf ("\
\tfile(s)   one or more files to be encoded.\n\
");
#ifdef SYSTEM_DOS
  printf ("\
\t          The encoded files are saved to files with the same base\n\
\t          name as the original but with an extension of .001, .002\n\
\t          etc. If you give a pathname with the -od parameter, the\n\
\t          files are written to that directory, otherwise the're\n\
\t          written to the current working directory.\n\
");
#else
  printf ("\
\t          If -o is used, the encoded files will be saved to files\n\
\t          with the same base name as the original but an extension\n\
\t          of .001, .002 etc. If you use -od and give an existing\n\
\t          pathname, the encoded files are written to that directory\n\
\t          instead of the current working directory.\n\
");
  printf ("\
\t          If neither of -o, -m, -p is specified, output is sent to\n\
\t          standard output, and the maximum line count is ignored.\n\
");
#endif
  printf ("\n\
  example:\n\
");
#ifdef SYSTEM_DOS
  printf ("\
    %s -b -2000 -o uuenview.exe\n\
\tEncodes 'uuenview.exe' as Base64 into 2000-line chunks, and writes\n\
\tthe result to uuenview.001 and uuenview.002.\n\
\n\
",
	  argv0);
#else
  printf ("\
    %s -b -2000 -m root -o uudeview.tar.gz\n\
\tEncodes 'uudeview.tar.gz' as Base64 into 2000-line chunks. It is\n\
\tboth mailed to your system administrator and written to the files\n\
\tuudeview.001 and uudeview.002\n\
\n\
",
	  argv0);
#endif

  return;
}

/*
 * -----------------------------------------------------------------------
 * Stolen from uuscan.c to parse boundary
 * -----------------------------------------------------------------------
 */

/*
 * Extract the value from a MIME attribute=value pair. This function
 * receives a pointer to the attribute.
 */
static char *
ParseValue (char *attribute)
{
  static char uuscan_pvvalue[256];
  char *ptr=uuscan_pvvalue;
  int length=0;

  if (attribute == NULL)
    return NULL;

  while (*attribute && *attribute != '=')
    attribute++;

  if (*attribute == '=') {
    attribute++;
    while (isspace (*attribute))
      attribute++;
  }
  if (!*attribute)
    return NULL;

  if (*attribute == '"') {
    /* quoted-string */
    attribute++;
    while (*attribute && *attribute != '"' && length < 255) {
      if (*attribute == '\\')
	*ptr++ = *++attribute;
      else
	*ptr++ = *attribute;
      attribute++;
      length++;
    }
    *ptr = '\0';
  }
  else {
    /* tspecials from RFC1521 */

    while (*attribute && !isspace (*attribute) &&
	   *attribute != '(' && *attribute != ')' &&
	   *attribute != '<' && *attribute != '>' &&
	   *attribute != '@' && *attribute != ',' &&
	   *attribute != ';' && *attribute != ':' &&
	   *attribute != '\\' &&*attribute != '"' &&
	   *attribute != '/' && *attribute != '[' &&
	   *attribute != ']' && *attribute != '?' &&
	   *attribute != '=' && length < 255)
      *ptr++ = *attribute++;

    *ptr = '\0';
  }
  return uuscan_pvvalue;
}

/*
 * -----------------------------------------------------------------------
 * Stolen from uuscan.c (end)
 * -----------------------------------------------------------------------
 */

/*
 * Create command line to mail or post a file. The result is malloc()ed
 * and must be freed manually
 */

static char *
SendMkCommand (char **rcptlist, char *towhom, char *subject, int isemail)
{
  char *command, *ptr;
  int len, count;

  *rcptlist = NULL;

  if (isemail && (uue_mailprog == NULL || *uue_mailprog == '\0')) {
    fprintf (stderr, "error: Cannot Email file: option not configured\n");
    return NULL;
  }
  else if (!isemail && (uue_inewsprog == NULL || *uue_inewsprog == '\0')) {
    fprintf (stderr, "error: Cannot Post file: option not configured\n");
    return NULL;
  }

  len = strlen ((isemail)?uue_mailprog:uue_inewsprog) + 
    ((uue_mpsubject)?strlen(subject):0) +
    ((isemail)?strlen(towhom):0) + 32;

  if ((command = (char *) malloc (len)) == NULL) {
    fprintf (stderr, "error: Out of memory allocating %d bytes\n", len);
    return NULL;
  }

  if ((*rcptlist = (char *) malloc (strlen (towhom) + 16)) == NULL) {
    fprintf (stderr, "error: Out of memory allocating %d bytes\n",
	     strlen (towhom)+16);
    _FP_free (command);
    return NULL;
  }

  if (isemail) {
    if (uue_mpsubject && subject!=NULL)
      sprintf (command, "%s -s \"%s\"", uue_mailprog, subject);
    else
      sprintf (command, "%s", uue_mailprog);

    /*
     * Attach list of recipients to mailer command and compose another list
     * of recipients
     */

    count = 0;
    (*rcptlist)[0] = '\0';
    ptr = _FP_strtok (towhom, ",; ");

    while (ptr) {
      strcat (command, " ");
      strcat (command, ptr);

      if (count++)
	strcat (*rcptlist, ",");
      strcat (*rcptlist, ptr);

      ptr = _FP_strtok (NULL, ",; ");
    }
  }
  else {
    /* posting */
    sprintf (command, "%s", uue_inewsprog);

    count = 0;
    (*rcptlist)[0] = '\0';
    ptr = _FP_strtok (towhom, ";, ");

    while (ptr) {
      if (count++)
	strcat (*rcptlist, ",");
      strcat (*rcptlist, ptr);
      ptr = _FP_strtok (NULL, ";, ");
    }
  }

  return command;
}

/*
 * Attach something. A text is read from stdin and converted into a proper
 * MIME message. All files from the command line are then attached MIME-
 * style. The result is mailed or posted.
 * If towhom==NULL, the result is sent to stdout.
 */

static int
AttachFiles (char *towhom, char *subject, int isemail, int encoding,
	     int argc, char *argv[])
{
  static char input[1024], *ctype=NULL, *cte=NULL, boundary[64];
  char *command=NULL, *rcptlist=NULL, *ptr;
  FILE *thepipe;
  int index, res;
  /* remember the headers we care about */
  int hadsubject=0, hadgroups=0, hadto=0, hadmime=0, hadmulti=0;

  if (towhom) {
#ifndef HAVE_POPEN
    fprintf (stderr, "error: Your system does not support %s of files\n",
	     (isemail)?"mailing":"posting");
    return UURET_ILLVAL;
#else
    if ((command = SendMkCommand (&rcptlist, towhom,
				  subject, isemail)) == NULL) {
      return UURET_ILLVAL;
    }
    if ((thepipe = popen (command, "w")) == NULL) {
      fprintf  (stderr, "error: could not open pipe %s\n", command);
      _FP_free (rcptlist);
      _FP_free (command);
      return UURET_IOERR;
    }
#endif
  }
  else {
    thepipe = stdout;
  }

  /*
   * okay, copy and scan header
   */

  index=0;
  while (!feof (stdin)) {
    if (_FP_fgets (input, 1024, stdin) == NULL)
      break;
    if (input[0] == '\0' || input[0] == '\012' || input[0] == '\015')
      break;

    /*
     * If the first line does not appear to be a header
     */
    if (index == 0) {
      ptr = input;
      while (*ptr && !isspace(*ptr) && *ptr!=':')
	ptr++;
      if (*ptr != ':' && _FP_strnicmp (input, "From ", 5) != 0) {
	break;
      }
      index++;
    }

    if (_FP_strnicmp (input, "Subject:", 8) == 0) {
      if (subject) {
	fprintf (thepipe, "Subject: %s\n", subject);
      }
      else {
	fprintf (thepipe, "%s", input);
      }
      hadsubject = 1;
    }
    else if (_FP_strnicmp (input, "Newsgroups:", 11) == 0) {
      if (towhom && !isemail && rcptlist) {
	fprintf (thepipe, "Newsgroups: %s\n", rcptlist);
      }
      else {
	fprintf (thepipe, "%s", input);
      }
      hadgroups = 1;
    }
    else if (_FP_strnicmp (input, "To:", 3) == 0) {
      if (towhom && isemail && rcptlist) {
	fprintf (thepipe, "To: %s\n", rcptlist);
      }
      else {
	fprintf (thepipe, "%s", input);
      }
      hadto = 1;
    }
    else if (_FP_strnicmp (input, "Mime-Version:", 13) == 0) {
      fprintf (thepipe, "%s", input);
      hadmime = 1;
    }
    else if (_FP_strnicmp (input, "Content-Type:", 13) == 0) {
      if (_FP_stristr (input, "multipart") != NULL) {
	/* it is already a multipart posting. grab the boundary */
	if ((ptr = _FP_stristr (input, "boundary=")) != NULL) {
	  fprintf(thepipe,  input);
	  strcpy (boundary, ParseValue (ptr));
	  hadmulti = 1;
	}
      }
      else {
	/*
	 * save content-type for later, must be only one line!
	 */
	ctype = _FP_strdup (input);
      }
    }
    else if (_FP_strnicmp (input, "Content-Transfer-Encoding:", 26) == 0) {
      /*
       * save for later, must be only one line
       */
      cte = _FP_strdup (input);
    }
    else {
      /*
       * just copy header line
       */
      fprintf (thepipe, "%s", input);
    }
  }

  /*
   * okay, header is copied. add our own fields if necessary
   */
  if (!hadmime)
    fprintf (thepipe, "Mime-Version: 1.0\n");
  if (!hadmulti) {
    /* must invent a boundary */
    sprintf (boundary, "==UUD_=_%ld", (long) time (NULL));
    fprintf (thepipe, "Content-Type: multipart/mixed; boundary=\"%s\"\n",
	     boundary);
  }

  if (index == 0) {
    /*
     * huh, there have been no headers.
     */
    if (subject) {
      fprintf (thepipe, "Subject: %s\n", subject);
    }
    if (towhom && !isemail && rcptlist) {
      fprintf (thepipe, "Newsgroups: %s\n", rcptlist);
    }
    if (towhom && isemail && rcptlist) {
      fprintf (thepipe, "To: %s\n", rcptlist);
    }
  }

  /*
   * end headers
   */
  fprintf (thepipe, "\n");

  /*
   * okay, copy text if available
   */
  if (!feof (stdin)) {
    if (!hadmulti) {
      /*
       * need our own header
       */
      fprintf (thepipe, "--%s\n", boundary);
      if (ctype) {
	fprintf (thepipe, "%s", ctype);
      }
      else {
	fprintf (thepipe, "Content-Type: text/plain\n");
      }
      if (cte) {
	fprintf (thepipe, "%s", cte);
      }
      else {
	fprintf (thepipe, "Content-Transfer-Encoding: 8bit\n");
      }
      fprintf (thepipe, "\n");

      if (index == 0) {
	fprintf (thepipe, "%s", input);
      }

      /*
       * just copy stdin
       */

      while (!feof (stdin)) {
	if (_FP_fgets (input, 256, stdin) == NULL)
	  break;
	fprintf (thepipe, "%s", input);
      }
      /*
       * the last crlf is to be ignored, so add another one
       */
      fprintf (thepipe, "\n");
    }
    else {
      /*
       * this was already a multipart/mixed posting, copy everything up
       * to the final boundary
       */
      while (!feof (stdin)) {
	if (_FP_fgets (input, 256, stdin) == NULL)
	  break;
	if (input[0] == '-' && input[1] == '-' &&
	    strncmp (input+2, boundary, strlen (boundary)) == 0 &&
	    input[strlen(boundary)+2] == '-' &&
	    input[strlen(boundary)+3] == '-')
	  break;
	fprintf (thepipe, "%s", input);
      }
    }
  }

  /*
   * okay, so let's finally make our attachments
   */

  for (index=1; index<argc; index++) {
    if (*argv[index] == '-' && argv[index][1] != '\0') {
      switch (argv[index][1]) {
      case 'm': /* skip parameters of options */
      case 'p':
      case 's':
      case 'i':
	if (index+1 < argc && argv[index+1][0] != '-')
	  index++;
	break;
      case 'o': /* may or may have not a parameter */
	if (argv[index][2]=='d' && index+1<argc && argv[index+1][0]!='-')
	  index++;
	break;
      default:  /* parameter without option */
	break;
      }
    }
    else /* this is a filename */ {
      /*
       * new boundary
       */
      fprintf (thepipe, "--%s\n", boundary);

      if ((res = UUEncodeMulti (thepipe, NULL, argv[index], encoding,
				NULL, NULL, 0)) != UURET_OK) {
	fprintf (stderr, "error while attaching %s: %s %s\n",
		 argv[index], UUstrerror (res),
		 (res==UURET_IOERR)?
		 strerror (UUGetOption (UUOPT_ERRNO, NULL, NULL, 0)) : "");
      }
      fprintf (thepipe, "\n");
    }
  }
  /*
   * done.
   */
  fprintf (thepipe, "--%s--\n\n", boundary);

  if (towhom) {
#ifdef HAVE_POPEN
    pclose   (thepipe);
    _FP_free (rcptlist);
    _FP_free (command);
#endif
  }
  _FP_free (ctype);
  _FP_free (cte);

  /* phew */
  return UURET_OK;
}

/*
 * Mail or Post a file. Remember to keep in sync with uutcl.c
 */

static int
SendAFile (FILE *infile,   char *infname,
	   int encoding,   int linperfile,
	   char *outfname, char *towhom,
	   char *subject,  int isemail)
{
  char *command, *rcptlist;
  FILE *thepipe, *theifile;
  int res, part;

  if (towhom==NULL ||
      (outfname==NULL&&infname==NULL) || (infile==NULL&&infname==NULL) ||
      (encoding!=UU_ENCODED&&encoding!=XX_ENCODED&&encoding!=B64ENCODED)) {
    fprintf (stderr, "oops: Parameter check failed in SendAFile()\n");
    return UURET_ILLVAL;
  }

#ifndef HAVE_POPEN
  fprintf (stderr, "error: Your system does not support %s of files\n",
	   (isemail)?"mailing":"posting");
  return UURET_ILLVAL;
#else
  if ((command = SendMkCommand (&rcptlist, towhom,
				subject, isemail)) == NULL) {
    return UURET_ILLVAL;
  }

  /*
   * Get going ...
   */

  if (infile == NULL) {
    if ((theifile = fopen (infname, "rb")) == NULL) {
      fprintf (stderr, "error: Could not open input file %s: %s\n",
	       infname, strerror (errno));
      _FP_free (rcptlist);
      _FP_free (command);
      return UURET_IOERR;
    }
  }
  else {
    theifile = infile;
  }

  for (part=1; !feof (theifile); part++) {
    if ((thepipe = popen (command, "w")) == NULL) {
      fprintf (stderr, "error: could not open pipe %s\n", command);
      if (infile==NULL) fclose (theifile);
      _FP_free (rcptlist);
      _FP_free (command);
      return UURET_IOERR;
    }

#if 0
    if (UUGetOption(UUOPT_VERBOSE, NULL, NULL, 0)) {
      fprintf (stderr, "%s part %03d of %s to %s ... ",
	       (isemail)?"mailing":"posting",
	       part, (infname)?infname:outfname,
	       rcptlist);
      fflush  (stderr);
    }
#endif

    res = UUE_PrepPartial (thepipe, theifile, infname, encoding,
			   outfname, 0, part, linperfile, 0,
			   rcptlist, NULL, subject, isemail);

#if 0
    if (UUGetOption (UUOPT_VERBOSE, NULL, NULL, 0)) {
      if (res == UURET_OK)
	fprintf (stderr, "ok.\n");
      else
	fprintf (stderr, "%s\n", UUstrerror (res));
    }
#endif

    pclose (thepipe);

    if (res != UURET_OK) {
      if (infile == NULL) fclose (theifile);
      _FP_free (rcptlist);
      _FP_free (command);
      return res;
    }
  }

  if (infile == NULL) fclose (theifile);
  _FP_free (rcptlist);
  _FP_free (command);
  return UURET_OK;
#endif
}

/*
 * main function from uuenview
 */

int
main (int argc, char *argv[])
{
  int outflags[5], tostdout, linperfile, uuencode, encoding;
  int index, introfile, subject, count, fileflag, stdinused;
  char filename[512], usename[512], outdir[512];
  int iskipflag, attach;
  FILE *testit;

  /*
   * set defaults
   */
  outflags[UUE_TOFILE] = -1;
  outflags[UUE_MAILTO] = -1;
  outflags[UUE_POSTTO] = -1;
  subject              = -1;
  introfile            = -1;
  tostdout             =  1;
  linperfile           =  0;
  uuencode             =  0;
  encoding             =  UU_ENCODED;
  count                =  0;
  stdinused            =  0;
  attach               =  0;

  if (UUInitialize () != UURET_OK) {
    fprintf (stderr, "oops: could not initialize decoding library\n");
    return 2;
  }
  UUSetOption (UUOPT_VERBOSE, 0, NULL);

#if defined(SYSTEM_DOS) || defined(SYSTEM_QUICKWIN)
  UUSetFNameFilter (NULL, UUFNameFilterDOS);
#else
  UUSetFNameFilter (NULL, UUFNameFilterUnix);
#endif
  /*
   * Setup Callback
   */
  UUSetMsgCallback  (NULL, MessageCallback);
  UUSetBusyCallback (NULL, BusyCallback, 100);

  /*
   * OK to overwrite target files
   */

  UUSetOption (UUOPT_OVERWRITE, 1, NULL);

  if (argc < 2) {
    usage (argv[0]);
    return 1;
  }
  if (_FP_stristr (argv[0], "uuencode") != NULL)
    uuencode = 1; /* uuencode compatibility */

  /*
   * In DOS, the default is to create files, not to send it to stdout
   */

#ifdef SYSTEM_DOS
  outflags[UUE_TOFILE] = -42; /* MAGIC */
  tostdout             =  0;
#endif  

  /*
   * Check for environment variable called INEWS and override
   * compile-time option if present
   */
  if (getenv ("INEWS") && *(char *)getenv ("INEWS")) {
    uue_inewsprog = getenv ("INEWS");
  }

  /*
   * browse command line flags
   */
  for (index=1; index<argc; index++) {
    if (*argv[index] == '-') {
      switch (argv[index][1]) {
      case '\0': /* read from stdin */
	if (attach) {
	  fprintf (stderr, "error: stdin already in use for attachment\n");
	  return 1;
	}
	if (stdinused) {
	  fprintf (stderr, "warning: can only use stdin once\n");
	  stdinused--;
	}
	stdinused++;
	break;
      case 'b': encoding = B64ENCODED; break;
      case 'u': encoding = UU_ENCODED; break;
      case 'x': encoding = XX_ENCODED; break;
#ifndef SYSTEM_DOS
      case 'm': 
	if (index+1<argc && argv[index+1][0] != '-') {
	  outflags[UUE_MAILTO] = ++index; 
	  tostdout             = 0;
	}
	else
	  fprintf (stderr, "error: -m requires a parameter\n");
	break;
      case 'p':
	if (index+1<argc && argv[index+1][0] != '-') {
	  outflags[UUE_POSTTO] = ++index;
	  tostdout             = 0;
	}
	else
	  fprintf (stderr, "error: -p requires a parameter\n");
	break;
#endif
      case 'o':
	if (argv[index][2] == 'd') {
	  if (index+1<argc && argv[index+1][0] != '-') {
	    strcpy (outdir, argv[++index]);
	    outflags[UUE_TOFILE] = index;
	    tostdout             = 0;
	    if (strlen(outdir)>0) {
	      if (outdir[strlen(outdir)-1]!=DIRSEPARATOR[0]) {
		strcat (outdir, DIRSEPARATOR);
	      }
	    }
	    UUSetOption (UUOPT_SAVEPATH, 0, outdir);
	  }
	  else {
	    fprintf (stderr, "error: -od requires a parameter\n");
	  }
	}
	else {
	  outflags[UUE_TOFILE] = index;
	  tostdout             = 0;
	}
	break;
      case 'i':
	if (index+1<argc && argv[index+1][0] != '-') {
	  introfile = ++index;
	}
	else
	  fprintf (stderr, "error: -i requires a parameter\n");
	break;
      case 's':
	if (index+1<argc && argv[index+1][0] != '-') {
	  subject   = ++index;
	}
	else
	  fprintf (stderr, "error: -s requires a parameter\n");
	break;
      case 'v':
	UUSetOption (UUOPT_VERBOSE, 1, NULL);
	break;
      case 'a':
	if (linperfile) {
	  fprintf (stderr, "error: cannot attach to splitfile\n");
	  return 1;
	}
	attach = 1;
	break;
      case 'h':
      case '?':
	usage(argv[0]);
	return 0;
      case 'V':
	fprintf (stdout, "uuenview %spl%s compiled on %s\n",
		 VERSION, PATCH, __DATE__);
	return 0;
      default:
	/* should be a line count of the form -1000 or a forced line count
	 * like --100 which disables the sanity check
	 */
	if (argv[index][1]>='0' && argv[index][1]<='9') {
	  if (attach) {
	    fprintf (stderr, "error: cannot attach to splitfile\n");
	    return 1;
	  }
	  linperfile = atoi (argv[index] + 1);
	  if (linperfile != 0 && linperfile < 200) {
	    fprintf (stderr,
		     "warning: lines per file must be >= 200 (ignored).\n");
	    linperfile=0;
	  }
	}
	else if (argv[index][1]=='-' &&
		 argv[index][2]>='0' && argv[index][2]<='9') {
	  if (attach) {
	    fprintf (stderr, "error: cannot attach to splitfile\n");
	    return 1;
	  }
	  linperfile = atoi (argv[index] + 2);
	}
	else if (argv[index][1]=='-') {
	  usage(argv[0]);
	  return 0;
	}
	else {
	  fprintf (stderr, "warning: unknown option '%s' ignored.\n",
		   argv[index]);
	}
      }
    }
  }
  /*
   * check integrity
   */

  if (linperfile != 0 && tostdout) {
    fprintf (stderr, "warning: cannot split file on standard output (use -o)\n");
    linperfile=0;
  }
  if (introfile>0 && tostdout) {
    fprintf (stderr, "warning: -i not possible on standard output (use -m or -p)\n");
    introfile = -1;
  }
  if (introfile>0) {
    if ((testit = fopen (argv[introfile], "r")) == NULL) {
      fprintf (stderr, "warning: introfile %s not readable.\n",
	       argv[introfile]);
      introfile = -1;
    }
    else
      fclose (testit);
  }
  if (subject>0 && tostdout && !attach) {
    fprintf (stderr, "warning: -s not possible on standard output\n");
    subject = -1;
  }
  if (encoding!=UU_ENCODED && encoding!=XX_ENCODED &&
      encoding!=B64ENCODED) {
    fprintf (stderr, "warning: unknown encoding method (%d)?\n",
	     encoding);
    encoding=UU_ENCODED;
  }

#ifdef SYSTEM_DOS
  if (tostdout==0 && outflags[UUE_TOFILE] == -1) {
    fprintf (stderr, "error: no output defined?\n");
    exit    (2);
  }
#endif

  /*
   * In Attach mode, first open the pipe and copy the existing article
   */

  if (attach) {
    if (outflags[UUE_POSTTO]==0 && outflags[UUE_MAILTO]==0 && !tostdout) {
      fprintf (stderr, "error: attach only possible to mail, news or stdout\n");
      exit (2);
    }
    if (outflags[UUE_POSTTO]>0 && outflags[UUE_MAILTO]>0) {
      fprintf (stderr, "warning: can attach only to one destination (sending mail only)\n");
      outflags[UUE_POSTTO] = 0;
    }
    if (outflags[UUE_POSTTO]>0) {
      AttachFiles (argv[outflags[UUE_POSTTO]],
		   (subject>0)?argv[subject]:NULL,
		   0, encoding, argc, argv);
    }
    else if (outflags[UUE_MAILTO]>0) {
      AttachFiles (argv[outflags[UUE_MAILTO]],
		   (subject>0)?argv[subject]:NULL,
		   1, encoding, argc, argv);
    }
    else /* tostdout */ {
      AttachFiles (NULL, (subject>0)?argv[subject]:NULL,
		   0, encoding, argc, argv);
    }
    /* finished here */
    count=1;
    goto uuenview_end;
  }

  /*
   * okay, now process the files
   */

  for (index=1, iskipflag=0; index<argc; index+=iskipflag+1) {
    iskipflag = 0;

    if (*argv[index] == '-' && argv[index][1] != '\0') {
      switch (argv[index][1]) {
      case 'm': /* skip parameters of options */
      case 'p':
      case 's':
      case 'i':
	if (index+1 < argc && argv[index+1][0] != '-')
	  index++;
	break;
      case 'o': /* may or may have not a parameter */
	if (argv[index][2]=='d' && index+1<argc && argv[index+1][0]!='-')
	  index++;
	break;
      default:  /* parameter without option */
	break;
      }
    }
    else /* this is a filename */ {
      if (uuencode) {
	/*
	 * we're in uuencode compatibility mode. This means the actual
	 * parameter is the file name on disk, the next parameter, if
	 * it exists and is not an option, is the name for the encoded
	 * file. If there is no next parameter, we shall read from stdin
	 * and use the given name as output filename
	 */
	if (index+1<argc && argv[index+1][0] != '-') {
	  strcpy (filename, argv[index]);
	  strcpy (usename,  argv[index+1]);
	  iskipflag = 1; /* ignore next argument */
	}
	else {
	  if (stdinused == 0) {
	    /* oops, stdin already used up */
	    continue;
	  }
	  stdinused--;
	  filename[0] = '\0';
	  strcpy (usename, argv[index]);
	}
      }
      else if (argv[index][0]=='-' && argv[index][1]=='\0') {
	/*
	 * supposed to read from stdin. we expect the next parameter to be
	 * the name to be used
	 */
	if (stdinused == 0) {
	  /* oops, stdin already used up */
	  continue;
	}
	stdinused--;

	if (index+1<argc && argv[index+1][0] != '-') {
	  strcpy (usename, argv[index+1]);
	  filename[0] = '\0';
	  iskipflag = 1; /* ignore next argument */
	}
	else {
	  fprintf (stderr, "error: need additional name for file from standard input\n");
	  continue;
	}
      }
      else {
	strcpy (filename, argv[index]);
	strcpy (usename,  filename);
      }

      if (filename[0] == '\0') {
	if (((outflags[UUE_POSTTO] > 0) ? 1 : 0) +
	    ((outflags[UUE_MAILTO] > 0) ? 1 : 0) +
	    ((outflags[UUE_TOFILE] > 0) ? 1 : 0) > 1) {
	  fprintf (stderr, "error: can use standard input only once\n");
	  continue;
	}
      }
      else if ((testit = fopen (argv[index], "r")) == NULL) {
	fprintf (stderr, "error: '%s' unreadable.\n", argv[index]);
	continue;
      }
      else
	fclose (testit);

      fileflag = 0;

#ifndef SYSTEM_DOS
      /*
       * post it
       */

      if (outflags[UUE_POSTTO] > 0) {
	if (SendAFile ((filename[0])?NULL:stdin,
		       (filename[0])?filename:NULL,
		       encoding, linperfile, usename,
		       argv[outflags[UUE_POSTTO]],
		       (subject>0)?argv[subject]:NULL, 0) == UURET_OK)
	  fileflag++;
      }

      /*
       * mail it separately to each recepient
       */

      if (outflags[UUE_MAILTO] > 0) {
	if (SendAFile ((filename[0])?NULL:stdin,
		       (filename[0])?filename:NULL,
		       encoding, linperfile, usename,
		       argv[outflags[UUE_MAILTO]],
		       (subject>0)?argv[subject]:NULL, 1) == UURET_OK)
	  fileflag++;
      }
#endif

      /*
       * store output into a file
       */
      if (outflags[UUE_TOFILE] > 0 || outflags[UUE_TOFILE] == -42) {
	if (UUEncodeToFile ((filename[0])?NULL:stdin,
			    (filename[0])?filename:NULL,
			    encoding, usename,
			    NULL, linperfile) == UURET_OK)
	  fileflag++;
      }

      /*
       * send it to stdout
       */
      if (tostdout) {
	if (UUEncodeToStream (stdout,
			      (filename[0])?NULL:stdin,
			      (filename[0])?filename:NULL,
			      encoding, usename, 0) == UURET_OK)
	  fileflag++;
      }

      if (fileflag > 0)
	count++;

    } /* end file processing */
  } /* end loop */

uuenview_end:
  if (UUISATTY(stderr)) {
    fprintf (stderr, "%70s\r", "");
    fflush  (stderr);
  }

  if (count==0) {
    fprintf (stderr, "error: no files.\n");
  }

  UUCleanUp ();

  return 0;
}


