/* Reading/parsing the initialization file.
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


/* $Id: init.c,v 1.1.1.1.2.5 1997/02/17 20:45:39 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <assert.h>

#include "wget.h"
#include "options.h"
#include "utils.h"
#include "init.h"
#include "url.h"
#include "ftp.h"
#include "host.h"
#include "recur.h"
#include "netrc.h"

extern char *version_string;
extern struct options opt;
extern int errno;

extern acc_t *netrc_list;

/* List of recognized commands, each consisting of a string-name, ID
   and what_to_do.

   When adding a new command, you must append it to the list (it is a
   good idea to keep the list sorted).  Each command has an ID (as
   listed in init.h, a pointer over which it will work, and an
   indication how it will work.

   CONOFF  -> integer toggle
   CNUM    -> integer
   CNUMINF -> integer, with "inf" synomim for 0
   CSTR    -> free old string, if non-NULL, and strdup val
   CVEC    -> vector
   CSPEC   -> special, has to be treated specially in setval */
struct cmd commands[] = {
   { "accept",		ACCEPT,		&opt.accepts,		CVEC },
   { "addhostdir",	ADDHOSTDIR,	&opt.add_hostdir,	CONOFF },
   { "alwaysrest",	ALWAYSREST,	&opt.always_rest,	CONOFF },
   { "backups",		BACKUPS,	&opt.backups,		CONOFF },
   { "basehref",	BASEHREF,	&opt.base_href,		CSTR },
   { "cache",		CACHE,		&opt.proxy_cache,	CONOFF },
   { "convertlinks",	CONVERTLINKS,	&opt.convert_links,	CONOFF },
   { "debug",		DEBUG_,		NULL,			CSPEC },
   { "deleteafter",	DELETEAFTER,	&opt.delete_after,	CONOFF },
   { "dirmode",		DIRMODE,	NULL,			CSPEC },
   { "dirprefix",	DIRPREFIX,	&opt.dir_prefix,	CSTR },
   { "dirstruct",	DIRSTRUCT,	NULL,			CSPEC },
   { "domains",		DOMAINS,	&opt.domains,		CVEC },
   { "dotbytes",	DOTBYTES,	&opt.dot_bytes,		CBYT },
   { "dotsinline",	DOTSINLINE,	&opt.dots_in_line,	CNUM },
   { "dotspacing",	DOTSPACING,	&opt.dot_spacing,	CNUM },
   { "dotstyle",	DOTSTYLE,	NULL,			CSPEC },
   { "excludedirectories", EXCLUDEDIRECTORIES,	&opt.excludes,	CVECDIR },
   { "excludedomains",	EXCLUDEDOMAINS,	&opt.exclude_domains,	CVEC },
   { "followftp",	FOLLOWFTP,	&opt.follow_ftp,	CONOFF },
   { "forcehtml",	FORCEHTML,	&opt.force_html,	CONOFF },
   { "ftpproxy",	FTPPROXY,	&opt.ftp_proxy,		CSTR },
   { "glob",		GLOB,		&opt.ftp_glob,		CONOFF },
   { "header",		HEADER,		NULL,			CSPEC },
   { "htmlify",		HTMLIFY,	NULL,			CSPEC },
   { "httppasswd",	HTTPPASSWD,	&opt.http_passwd,	CSTR },
   { "httpproxy",	HTTPPROXY,	&opt.http_proxy,	CSTR },
   { "httpuser",	HTTPUSER,	&opt.http_user,		CSTR },
   { "ignorelength",	IGNORELENGTH,	&opt.ignore_length,	CONOFF },
   { "includedirectories", INCLUDEDIRECTORIES, &opt.includes,	CVECDIR },
   { "input",		INPUT,		&opt.input_filename,	CSTR },
   { "killlonger",	KILLLONGER,	&opt.kill_longer,	CONOFF },
   { "logfile",		LOGFILE,	&opt.lfilename,		CSTR },
   { "login",		LOGIN,		&opt.ftp_acc,		CSTR },
   { "mirror",		MIRROR,		NULL,			CSPEC },
   { "netrc",		NETRC,		&opt.netrc,		CONOFF },
   { "noclobber",	NOCLOBBER,	&opt.noclobber,		CONOFF },
   { "noparent",	NOPARENT,	&opt.no_parent,		CONOFF },
   { "noproxy",		NOPROXY,	&opt.no_proxy,		CVEC },
   { "numtries",	NUMTRIES,	&opt.ntry,		CNUMINF },
   { "outputdocument",	OUTPUTDOCUMENT, NULL,			CSPEC },
   { "passiveftp",	PASSIVEFTP,	&opt.ftp_pasv,		CONOFF },
   { "passwd",		PASSWD,		&opt.ftp_pass,		CSTR },
   { "quiet",		QUIET,		&opt.quiet,		CONOFF },
   { "quota",		QUOTA_,		&opt.quota,		CBYT },
   { "proxypasswd",	PROXYPASSWD,	&opt.proxy_passwd,	CSTR },
   { "proxyuser",	PROXYUSER,	&opt.proxy_user,	CSTR },
   { "reclevel",	RECLEVEL,	&opt.maxreclevel,	CNUMINF },
   { "recursive",	RECURSIVE,	NULL,			CSPEC },
   { "reject",		REJECT,		&opt.rejects,		CVEC },
   { "relativeonly",	RELATIVEONLY,	&opt.relative_only,	CONOFF },
   { "removelisting",	REMOVELISTING,	&opt.remove_listing,	CONOFF },
   { "retrsymlinks",	RETRSYMLINKS,	&opt.retr_symlinks,	CONOFF },
   { "robots",		ROBOTS,		&opt.use_robots,	CONOFF },
   { "saveheaders",	SAVEHEADERS,	&opt.save_headers,	CONOFF },
   { "serverresponse",	SERVERRESPONSE, &opt.server_response,	CONOFF },
   { "simplehostcheck", SIMPLEHOSTCHECK,&opt.simple_check,	CONOFF },
   { "spanhosts",	SPANHOSTS,	&opt.spanhost,		CONOFF },
   { "spider",		SPIDER,		&opt.spider,		CONOFF },
   { "timeout",		TIMEOUT,	NULL,			CSPEC },
   { "timestamping",	TIMESTAMPING,	&opt.timestamping,	CONOFF },
   { "useproxy",	USEPROXY,	&opt.use_proxy,		CONOFF },
   { "useragent",	USERAGENT,	&opt.useragent,		CSTR },
   { "verbose",		VERBOSE,	&opt.verbose,		CONOFF },
   { "wait",		WAIT,		&opt.wait,		CNUM },
   { NULL, LAST }
};


/* Return index of a valid command, -1 if none. */
int
comind(const struct cmd *commands, const char *com)
{
   int i;

   for (i = 0; commands[i].name; i++)
      if (!strcasecmp(commands[i].name, com))
	 return i;
   return -1;
}

/* Reset the variables to default values. */
void
defaults(void)
{
   char *tmp;
   
#ifdef DEBUG
   opt.debug = 0;
#endif
   opt.verbose = -1;
   opt.quiet = 0;
   
   opt.kill_longer = 0;
   opt.ignore_length = 0;
   opt.lfilename = NULL;
   opt.input_filename = NULL;
   opt.dirstruct = 0;
   opt.no_dirstruct = 0;
   opt.dir_prefix = nstrdup(DEFAULT_DIRPREFIX);
   
   opt.ntry = DEFAULT_NUMBER_OF_TRIES;
   opt.recursive = 0;
   opt.maxreclevel = DEFAULT_MAXRECLEVEL;
   opt.spanhost = 0;
   opt.relative_only = 0;
   opt.no_parent = 0;
   opt.accepts = opt.rejects = NULL;
   opt.includes = opt.excludes = NULL;
   
   opt.simple_check = 0;
   opt.dirmode = DEFAULT_DIRMODE;
   opt.add_hostdir = 1;
   opt.noclobber = 0;

   opt.follow_ftp = 0;
   opt.retr_symlinks = 0;
   opt.always_rest = 0;
   opt.ftp_acc = nstrdup(DEFAULT_FTP_ACCT);
   opt.ftp_pass = nstrdup(ftp_getaddress());
   opt.netrc = 1;
   opt.ftp_glob = 1;
   opt.ftp_pasv = 0;
   opt.htmlify = 1;

   opt.http_user = NULL;
   opt.http_passwd = NULL;
   opt.proxy_user = NULL;
   opt.proxy_passwd = NULL;
   opt.user_header = NULL;

   opt.useragent = NULL;
   opt.output_document = NULL;

   opt.use_proxy = 1;
   opt.http_proxy = opt.ftp_proxy = NULL;
   opt.no_proxy = NULL;
   tmp = getenv("no_proxy");
   if (tmp)
      opt.no_proxy = sepstring(tmp);
   opt.proxy_cache = 1;
   opt.base_href = NULL;

#ifdef HAVE_SELECT
   opt.timeout = DEFAULT_TIMEOUT;
#endif
   opt.wait = 0;

   opt.use_robots = 1;

   opt.quota = 0;
   opt.downloaded = 0;
   opt.numurls = 0;

   opt.server_response = 0;
   opt.save_headers = 0;
   opt.timestamping = 0;
   opt.remove_listing = 1;
   opt.delete_after = 0;

   opt.backups = 0;
   opt.convert_links = 0;

   opt.dot_bytes = DOT_BYTES;
   opt.dot_spacing = DOT_SPACING;
   opt.dots_in_line = DOTS_IN_LINE;
}

/* Return the user's home directory (strdup-ed), or NULL if none found. */
char *
home_dir(void)
{
   char *home;
#ifdef WINDOWS
/* TODO - maybe I should grab home_dir from registry,
   but the best that I could get from there is users
	Start menu. It sucks! */
		home = getenv("HOME");
	return home ? nstrdup(home) : NULL;

#else /* Unix or stuff */
   struct passwd *pwd;

   home = getenv("HOME");
   /* If HOME is not defined, try getting it from the password file. */
   if (!home)
   {
      pwd = getpwuid(getuid());
      if (!pwd || !pwd->pw_dir)
	 return NULL;
      home = pwd->pw_dir;
   }
   if (home)
      return nstrdup(home);
   return NULL;
#endif /* WINDOWS */
}

/* Return the path to the user's .wgetrc. */
char *
init_path(void)
{
   char *env, *home, *path;
   struct stat buf;
   int err;

   /* Try the environment. */
   if ((env = getenv("WGETRC")))
   {
      path = nstrdup(env);
      err = stat(path, &buf);
      if (err == 0)
	 return path;
      free(path);
      fprintf(stderr, "Cannot load .wgetrc.\n");
      exit(1);
   }
   /* If that failed, try $HOME/.wgetrc. */
   home = home_dir();
   if (!home)
      return NULL;
   path = (char *)nmalloc(strlen(home) + 1 + strlen(".wgetrc") + 1);
   sprintf(path, "%s/.wgetrc", home);
   free(home);
   err = stat(path, &buf);
   if (err == -1)
   {
      free(path);
      return NULL;
   }
   return path;
}

/* Initialize variables from a wgetrc file */
void
run_wgetrc(const char *path)
{
   char *com, *val;
   FILE *fp;
   char *line;
   int stat, ln, length;

   fp = fopen(path, "r");
   if (!fp)
   {
      fprintf(stderr, "Cannot read %s (%s).\n", path, mystrerror(errno));
      return;
   }
   /* Reset line number. */
   ln = 1;
   /* While there are lines in the file: */
   while ((line = read_whole_line(fp)))
   {
      length = strlen(line);
      if (length && line[length - 1] == '\r')
	 line[length - 1] = '\0';
      /* Parse the line. */
      stat = parse_line((unsigned char *)line,
			(unsigned char **)&com, (unsigned char **)&val);
      free(line);
      /* If everything is OK, set the value. */
      if (stat == 1)
      {
	 if (!setval(com, val))
	    fprintf(stderr, "Error in %s at line %d.\n", path, ln);
	 free(com);
	 free(val);
      }
      else if (stat == 0)
	 fprintf(stderr, "Error in %s at line %d.\n", path, ln);
      ++ln;
   }
   fclose(fp);
}

/* Initialize the defaults and run the system wgetrc and user's own
   wgetrc.  */
void
initialize(void)
{
   char *path;
   struct stat buf;
   int err;

   /* Load defaults. */
   defaults();

   /* If SYSTEM_WGETRC is defined, use it. */
#ifdef SYSTEM_WGETRC
   err = stat(SYSTEM_WGETRC, &buf);
   if (err != -1) /* File exists */
      run_wgetrc(SYSTEM_WGETRC);
#endif
   /* Override it with your own, if one exists. */
   path = init_path();
   if (path)
   {
      run_wgetrc(path);
      free(path);
   }
}

/* Parse the line pointed by line, with the syntax:
   <sp>* command <sp>* = <sp>* value <newline>
   Uses malloc to allocate space for command and value.
   If the line is invalid, data is freed and 0 is returned.

   Return values:
    1 - success
    0 - failure
   -1 - comment */
int
parse_line(const unsigned char *line, unsigned char **com, unsigned char **val)
{
   int i, j;

   /* Skip spaces. */
   for (i = 0; line[i] == ' ' || line[i] == '\t'; i++);

   /* Skip empty or hashed lines. */
   if (!line[i] || line[i] == '\n' || line[i] == '#')
      return -1;
   
   /* Allocate command. */
   *com = (unsigned char *)nmalloc(1);
   (*com)[0] = '\0';
   for (j = 0; (isalpha(line[i]) || line[i] == '_' || line[i] == '-'); i++)
   {
      if (line[i] == '_' || line[i] == '-')
	 continue;
      *com = (unsigned char *)nrealloc(*com, j + 2);
      (*com)[j] = tolower(line[i]);
      (*com)[j + 1] = '\0';
      ++j;
   }
   /* The next char should be space or '='. */
   if (!isspace(line[i]) && (line[i] != '='))
   {
      free(*com);
      return 0;
   }
   /* If the command is invalid, exit now. */
   if (comind(commands, (char *)*com) == NONE)
   {
      free(*com);
      return 0;
   }
   /* Skip spaces before '='. */
   for (; isspace(line[i]); i++);
   /* If '=' not found, bail out. */
   if (line[i] != '=')
   {
      free(com);
      return 0;
   }
   /* Skip spaces after '='. */
   for (++i; isspace(line[i]); i++);
   /* Get the ending position. */
   for (j = i; line[i] && line[i] != '\n'; i++);
   /* Allocate *val, and copy from line. */
   (*val) = (unsigned char *)nmalloc(i - j + 1);
   strncpy((char *)*val, (char *)line + j, i - j);
   (*val)[i - j] = '\0';
   return 1;
}

/* Do the job for each line.  No fatals -- error signal prints a
   warning and resets to default value.  All error messages are
   printed to stderr, *not* to opt.lfile, since opt.lfile wasn't even
   generated yet.  */
int
setval(const char *com, const char *val)
{
   int flag, mirror;
   int i, ind;                  /* To be used for various loops. */
   const char *p;               /* Likewise. */

   if (!com || !val)
      return 0;
   ind = comind(commands, com);
   if (ind == -1)
   {
#ifdef DEBUG
      fprintf(stderr, "Unknown command `%s', value `%s'.\n", com, val);
#endif
      return 0;
   }
   switch (commands[ind].what_to_do)
   {
      case CONOFF:
	 return setonoff((int *)commands[ind].varp, com, val);
	 break;
      case CNUM:
	 return setnum((int *)commands[ind].varp, com, val, 0);
	 break;
      case CNUMINF:
	 return setnum((int *)commands[ind].varp, com, val, SINF);
	 break;
      case CSTR:
	 if (*(char **)commands[ind].varp)
	    free(*(char **)commands[ind].varp);
	 *(char **)commands[ind].varp = (char *)nstrdup(val);
	 break;
      case CVEC:
	 if (*val)
	    *(char ***)commands[ind].varp = merge_vecs(*(char ***)commands[ind].varp,
						     sepstring(val));
	 else
	 {
	    free_vec(*(char ***)commands[ind].varp);
	    *(char ***)commands[ind].varp = NULL;
	 }
	 break;
      case CVECDIR:
	 if (*val)
	 {
	    /* Strip the trailing slashes from directories. */
	    char **t, **seps;
	    int len;

	    seps = sepstring(val);
	    for (t = seps; t && *t; t++)
	    {
	       len = strlen(*t);
	       /* Skip degenerate case of root directory. */
	       if (len > 1)
	       {
		  if ((*t)[len - 1] == '/')
		     (*t)[len - 1] = '\0';
	       }
	    }
	    *(char ***)commands[ind].varp =
	       merge_vecs(*(char ***)commands[ind].varp, seps);
	 }
	 else
	 {
	    free_vec(*(char ***)commands[ind].varp);
	    *(char ***)commands[ind].varp = NULL;
	 }
	 break;
      case CBYT:
	 return setbytes((long *)commands[ind].varp, com, val);
	 break;
      case CSPEC:
	 switch(commands[ind].id)
	 {
	    case DEBUG_:
#ifdef DEBUG
	       return setonoff(&opt.debug, com, val);
#else
	       fprintf(stderr, "%s: debug support not loaded.\n", com);
	       return 0;
#endif
	       break;
	    case DIRMODE:
	       flag = getperms(val);
	       if (flag == -1)
	       {
		  fprintf(stderr, "%s: Invalid mode specification `%s'.\n",
			  com, val);
		  return 0;
	       }
	       opt.dirmode = DEFAULT_DIRMODE;
	       break;
	    case DIRSTRUCT:
	       if (!setonoff(&opt.dirstruct, com, val))
		  return 0;
	       /* Since dirstruct behaviour is explicitly changed,
		  no_dirstruct must be affected inversely. */
	       if (opt.dirstruct)
		  opt.no_dirstruct = 0;
	       else
		  opt.no_dirstruct = 1;
	       break;
	    case DOTSTYLE:
	       /* Retrieval styles. */
	       if (!strcasecmp(val, "default"))
	       {
		  /* Default style: 1K dots, 10 dots in a cluster, 50
		     dots in a line. */
		  opt.dot_bytes = DOT_BYTES;
		  opt.dot_spacing = DOT_SPACING;
		  opt.dots_in_line = DOTS_IN_LINE;
	       }
	       else if (!strcasecmp(val, "binary"))
	       {
		  /* "Binary" retrieval: 8K dots, 16 dots in a cluster,
		     64 dots (512K) in a line. */
		  opt.dot_bytes = 8192;
		  opt.dot_spacing = 16;
		  opt.dots_in_line = 64;
	       }
	       else if (!strcasecmp(val, "mega"))
	       {
		  /* "Mega" retrieval, for retrieving very long files;
		     each dot is 64K, 8 dots in a cluster, 6 clusters
		     (3M) in a line.  */
		  opt.dot_bytes = 65536L;
		  opt.dot_spacing = 8;
		  opt.dots_in_line = 48;
	       }
	       else if (!strcasecmp(val, "micro"))
	       {
		  /* "Micro" retrieval, for retrieving very small files;
		     each dot is 128 bytes, 8 dots in a cluster, 6
		     clusters (6K) in a line.  */
		  opt.dot_bytes = 128;
		  opt.dot_spacing = 8;
		  opt.dots_in_line = 48;
	       }
	       else
	       {
		  fprintf(stderr, "%s: Invalid specification `%s'.\n", com, val);
		  return 0;
	       }
	       break;
	    case HEADER:
	       if (!*val)
	       {
		  /* Empty header means to reset headers. */
		  if (opt.user_header)
		  {
		     free(opt.user_header);
		     opt.user_header = NULL;
		  }
	       }
	       else
	       {
		  /* Check for sanity of the header value. */
		  flag = 0;
		  for (p = val; *p && *p != ':' && !isspace(*p); p++);
		  /* The header MUST contain ':' preceded by at least one
		     non-space character. */
		  if (*p != ':' || p == val)
		     flag = 1;
		  /* The header MUST NOT contain newlines. */
		  if (!flag)
		     if (strchr(val, '\n'))
			flag = 1;
		  if (flag)
		  {
		     fprintf(stderr, "%s: Invalid specification `%s'.\n", com, val);
		     return 0;
		  }
		  i = opt.user_header ? strlen(opt.user_header) : 0;
		  opt.user_header = (char *)nrealloc(opt.user_header,
						     i + strlen(val) + 2 + 1);
		  strcpy(opt.user_header + i, val);
		  i += strlen(val);
		  opt.user_header[i++] = '\r';
		  opt.user_header[i++] = '\n';
		  opt.user_header[i] = '\0';
	       }
	       break;
	    case HTMLIFY:
	       flag = setonoff(&opt.htmlify, com, val);
	       if (flag && !opt.htmlify)
		  opt.remove_listing = 0;
	       return flag;
	       break;
	    case MIRROR:
	       if (!setonoff(&mirror, com, val))
		  return 0;
	       if (mirror)
	       {
		  opt.recursive = 1;
		  if (!opt.no_dirstruct)
		     opt.dirstruct = 1;
		  opt.timestamping = 1;
		  opt.maxreclevel = 0;
		  opt.remove_listing = 0;
	       }
	       break;
	    case OUTPUTDOCUMENT:
	       if (opt.output_document)
		  free(opt.output_document);
	       opt.output_document = nstrdup(val);
	       opt.ntry = 1;
	       if (ISHYPHEN(opt.output_document))
		  opt.quiet = 1;
	       break;
	    case RECURSIVE:
	       if (!setonoff(&opt.recursive, com, val))
		  return 0;
	       else
	       {
		  if (opt.recursive && !opt.no_dirstruct)
		     opt.dirstruct = 1;
	       }
	       break;
	    case TIMEOUT:
#ifdef HAVE_SELECT
	       return setnum(&opt.timeout, com, val, SINF);
#else
	       fprintf(stderr, "Unable to set timeout without select().\n");
#endif
	       break;
	    default:
	       assert(0);
	 }
	 break;
      default:
	 assert(0);
   }
   return 1;
}

/* Return 0 if off, 1 if on, -1 if error. */
int
onoff(const char *val)
{
   if (strcasecmp(val, "on") == 0 || strcmp(val, "1") == 0)
      return 1;
   else if (strcasecmp(val, "off") == 0 || strcmp(val, "0") == 0)
      return 0;
   else
      return -1;
}

/* Set the integer value pointed to by what on value represented in
   string val. String com is used only for error messages. */
int
setonoff(int *what, const char *com, const char *val)
{
   int flag;

   flag = onoff(val);
   if (flag == -1)
   {
      fprintf(stderr, "%s: Please specify on or off.\n", com);
      return 0;
   }
   *what = flag;
   return 1;
}

/* Similar to setonoff, but converts numbers.  With incorrect
   specification, the number remains unchanged.  With flags set to
   SINF, uses "inf" for meaning zero.  */
int
setnum(int *what, const char *com, const char *val, int flags)
{
   int res;

   if (flags == SINF && !strcasecmp(val, "inf"))
   {
      *what = 0;
      return 1;
   }
   res = myatoi(val);
   if (res == -1)
   {
      fprintf(stderr, "%s: Invalid specification `%s'.\n", com, val);
      return 0;
   }
   *what = res;
   return 1;
}

/* Return the int value of a positive integer written in a string,
   or -1 if an error was encountered */
int
myatoi(const char *s)
{
   int res;
   const char *orig;

   orig = s;
   for (res = 0; *s && isdigit(*s); s++)
   {
      res *= 10;
      res += *s - '0';
   }
   if (*s || orig == s)
      return -1;
   else
      return res;
}

/* Return the decimal representation of octal mode three-digit
   permission string, or -1 if incorrect. */
int
getperms(const char *s)
{
   if (strlen(s) != 3 || !ISODIGIT(s[0]) || !ISODIGIT(s[1]) || !ISODIGIT(s[2]))
      return -1;
   return (s[2] - '0') + 8 * (s[1] - '0') + 64 * (s[0] - '0');
}

/* Free the memory allocated by global variables. */
void
cleanup(void)
{
   recursive_retrieve(NULL, NULL, RCLEANUP);
   clean_hosts();
   free_netrc(netrc_list);
   if (opt.dfp)
      fclose(opt.dfp);
   if (opt.lfilename)
      free(opt.lfilename);
   free(opt.dir_prefix);
   if (opt.input_filename)
      free(opt.input_filename);
   if (opt.output_document)
      free(opt.output_document);
   free_vec(opt.accepts);
   free_vec(opt.rejects);
   free_vec(opt.excludes);
   free_vec(opt.includes);
   free_vec(opt.domains);
   free(opt.ftp_acc);
   free(opt.ftp_pass);
   if (opt.ftp_proxy)
      free(opt.ftp_proxy);
   if (opt.http_proxy)
      free(opt.http_proxy);
   free_vec(opt.no_proxy);
   if (opt.useragent)
      free(opt.useragent);
   if (opt.http_user)
      free(opt.http_user);
   if (opt.http_passwd)
      free(opt.http_passwd);
   if (opt.user_header)
      free(opt.user_header);
}

/* Sets the value stored in string val to a long int out, allowing
   several postfices, with the following syntax (regexp):

   [0-9]+       -> bytes
   [0-9]+[kK]   -> bytes * 1024
   [0-9]+[mM]   -> bytes * 1024 * 1024
   inf          -> 0

   Anything else is flagged as incorrect, and *out is unchanged. */
int
setbytes(long *out, const char *com, const char *val)
{
   long result;
   const char *p;

   result = 0;
   p = val;
   /* Check for "inf". */
   if (p[0] == 'i' && p[1] == 'n' && p[2] == 'f' && p[3] == '\0')
   {
      *out = 0L;
      return 1;
   }
   /* Search for digits and construct result. */
   for (; *p && isdigit(*p); p++)
   {
      result *= 10;
      result += *p - '0';
   }
   /* If no digits were found, or more than one character is following
      them, bail out. */
   if (p == val || (*p != '\0' && *(p + 1) != '\0'))
   {
      printf("%s: Invalid specification `%s'\n", com, val);
      return 0;
   }
   /* Search for a designator. */
   switch(*p)
   {
      case 'K': case 'k':
	 /* Kilobytes */
	 result *= 1024;
	 break;
      case 'M': case 'm':
	 /* Megabytes */
	 result *= (long)1024 * 1024;
	 break;
      case '\0':
	 /* Never mind. */
	 break;
      default:
	 printf("%s: Invalid specification `%s'\n", com, val);
	 return 0;
   }
   *out = result;
   return 1;
}
