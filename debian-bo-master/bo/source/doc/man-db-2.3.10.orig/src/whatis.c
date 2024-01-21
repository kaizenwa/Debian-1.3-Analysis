/*
 * whatis.c: search the index or whatis database(s) for words.
 *  
 * Copyright (C), 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * routines for whatis and apropos programs. Whatis looks up the 
 * keyword for the description, apropos searches the entire database 
 * for word matches.
 *
 * Mon Aug  8 20:35:30 BST 1994  Wilf. (G.Wilford@ee.surrey.ac.uk) 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#ifndef STDC_HEADERS
extern int errno;
#endif

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

#if defined(STDC_HEADERS)
#  include <stdlib.h>
#  include <string.h>
#elif defined(HAVE_STRING_H)
#  include <string.h>
#elif defined(HAVE_STRINGS_H)
#  include <strings.h>
#else
extern char *strrchr(), *strtok();
#endif /* no string(s) header */

#define NLS_SET	whatisSet
#include "nls/nls.h"

#ifdef NLS
nl_catd catfd;
#endif /* NLS */

#ifdef HAVE_REGEX_H
#  include <sys/types.h>
#  include <regex.h>
#endif /* HAVE_REGEX_H */

#ifdef HAVE_FNMATCH_H
#  include <fnmatch.h>
#else /* !HAVE_FNMATCH_H */
#  include "lib/fnmatch.h"
#endif /* HAVE_FNMATCH_H */

#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#else /* !HAVE_GETOPT_H */
#  include "lib/getopt.h"
#endif /* HAVE_GETOPT_H */

#include "manconfig.h"
#include "libdb/mydbm.h"
#include "libdb/db_storage.h"
#include "lib/error.h"
#include "manp.h"

extern char *manpathlist[];
extern char *optarg;
extern int optind, opterr, optopt;

#ifndef debug
int debug;
#endif
char *program_name;
char *database;
MYDBM_FILE dbf;
int quiet = 1;

#if defined(POSIX_REGEX) || defined(BSD_REGEX)
#  define REGEX
#endif

#ifdef REGEX
#  if defined(POSIX_REGEX)
static regex_t preg;  
#  endif
static int regex;
#  ifndef HAVE_REGEX_H
extern char *re_comp();
extern void regfree();
#  endif
#endif

static int wildcard;
static int status = OK;

#if !defined(APROPOS) && !defined(WHATIS)
#  error #define WHATIS or APROPOS, so I know who I am
#endif

static const char args[] = "drwhVm:M:fk";

static const struct option long_options[] =
{
    {"debug",        no_argument,       0, 'd'},
    {"regex",        no_argument,       0, 'r'},
    {"wildcard",     no_argument,       0, 'w'},
    {"help",         no_argument,       0, 'h'},
    {"version",      no_argument,       0, 'V'},
    {"systems",      required_argument, 0, 'm'},
    {"manpath",      required_argument, 0, 'M'},
    {"whatis",	     no_argument,	0, 'f'},
    {"apropos",	     no_argument,	0, 'k'},
    {0, 0, 0, 0}
};

#ifdef APROPOS
static void usage(int status)
{
	printf(CATGETS(whatis_APROPOS_USAGE, 
		       "usage: %s [-d] [-r|-w] [-m systems] [-M manpath] | [-h] | [-V] keyword ...\n"), program_name);
	printf(CATGETS(whatis_APROPOS_OPTIONS, 
	       "-d --debug		produce debugging info.\n"
	       "-r --regex 		interpret each keyword as a regex.\n"
	       "-w --wildcard		the keyword(s) contain wildcards.\n"
	       "-m --systems system	include alternate systems man pages.\n"
	       "-M --manpath path	set search path for manual pages to `path'.\n"
	       "-V --version		show version.\n"
	       "-h --help		show this usage message.\n")
	);

	exit (status);
}
#else	
static void usage(int status)
{
	printf(CATGETS(whatis_WHATIS_USAGE, "usage: %s [-d] [-r|-w] [-m systems] [-M manpath] | [-h] | [-V] keyword ...\n"), program_name);
	printf(CATGETS(whatis_WHATIS_OPTIONS, 
	       "-d --debug		produce debugging info.\n"
	       "-r --regex 		interpret each keyword as a regex.\n"
	       "-w --wildcard		the keyword(s) contain wildcards.\n"
	       "-m --systems system	include alternate systems man pages.\n"
	       "-M --manpath path	set search path for manual pages to `path'.\n"
	       "-V --version		show version.\n"
	       "-h --help		show this usage message.\n")
	);

	exit (status);
}
#endif
	
/* do the old thing, if we cannot find the relevant database */
static __inline__ int use_grep(char *page, char *manpath)
{
	char *whatis_file = strappend(NULL, manpath, "/whatis", NULL);
	int status;

	if (access(whatis_file, R_OK) == 0) {
#if defined(WHATIS)
		char *command = strappend(NULL, "grep -i '^", page, 
				    	  "' ", whatis_file, NULL);
#elif defined(APROPOS)
		char *flags, *command;

#ifdef REGEX
		if (regex)
			flags = "-iE";
		else
#endif
			flags = "-iEw";
			
		command = strappend(NULL, "grep ", flags, " '", page,
				    "' ", whatis_file, NULL);
#endif 	
		status = (system(command) == 0);
		free(command);
	} else {
		error (0, 0, 
		       CATGETS(whatis_WHATIS,
			       "warning: can't read the fallback whatis text database."));
		error (0, errno,
		       "%s/whatis", manpath);
		status = 0;
	}

	free(whatis_file);
	return status;
}

/* fill_in_whatis() is really a ../libdb/db_lookup.c routine but whatis.c
   is the only file that actually requires access to the whatis text... */
   
/* Take mandata struct (earlier returned from a dblookup()) and return 
   the relative whatis */
static char *get_whatis(struct mandata *info)
{
	struct mandata *newinfo;

	/* In the future we may need to gaurd against an infinate 
	   recursive loop here, but as info->pointer only points to
	   whatis refs, we're safe for the time being */

	/* ensure we need to fill in the whatis */
	if (*(info->pointer) == '-') {
		if (*(info->whatis))
			return xstrdup(info->whatis);
		else
			return xstrdup(CATGETS(whatis_UNKNOWN, "(unknown)"));
	}

	newinfo = dblookup_exact(info->pointer, info->ext);

	/* If the pointer lookup fails, do nothing */
	if (newinfo) {
		char *whatis;
		whatis = get_whatis(newinfo);
		free_mandata_struct(newinfo);
		return whatis;
	} else
		return xstrdup(CATGETS(whatis_UNKNOWN, "(unknown)"));
}

/* print out any matches found */
static void display(struct mandata *info, char *page)
{
	char *string, *whatis;
	
	whatis = get_whatis(info);
	
	if (debug)
		dbprintf(info);

	if (*(info->pointer) == '-')
		string = strappend(NULL, page, " (", info->ext, ")", NULL);
	else
		string = strappend(NULL, page, " (", info->ext, ") [",
				   info->pointer, "]", NULL);

	if (strlen(string) < (size_t) 20)
		printf("%-20s - %s\n", string, whatis);
	else
		printf("%s - %s\n", string, whatis);

	free(whatis);
	free(string);
}

/* return lowered version of s */
static char *lower(char *s)
{
	char *low, *p;

	p = low = (char *) xmalloc (strlen(s) +1);

	while (*s) {
		if (isupper(*s))
			*p++ = tolower(*s++);
		else
			*p++ = *s++;
	}

	*p = *s;
	return low;
}

#ifdef WHATIS
/* lookup the page and display the results */
static __inline__ int whatis(char *page)
{
	struct mandata *info;
	int count = 0;

	info = dblookup_all(page, NULL);
	while (info) {
		struct mandata *pinfo;
			
		display(info, page);
		count++;
		pinfo = info->next;	/* go on to next structure */
	 	free(info->addr);	/* free info's `content' */
	 	free(info);		/* free info */
		info = pinfo;
	}
	return count;
}
#endif /* WHATIS */

/* return 1 if page matches name, else 0 */
static int parse_name(char *page, char *dbname)
{ 
#ifdef REGEX
	if (regex)
#  if defined(POSIX_REGEX)
		return (regexec(&preg, dbname, 0, (regmatch_t *) 0, 0) == 0);
#  elif defined(BSD_REGEX)
		return re_exec(dbname);
#  endif
#endif /* REGEX */

#ifdef APROPOS
	if (!wildcard) {
		char *lowdbname = lower(dbname);
		return STREQ(lowdbname, page);
		free(lowdbname);
	}
#endif

	return (fnmatch(page, dbname, 0) == 0);
}

#ifdef APROPOS
/* return 1 on word match */
static int match(char *lowpage, char *whatis)
{
	char *lowwhatis = lower(whatis);
	size_t len = strlen(lowpage);
	char *p, *begin;

	begin = lowwhatis;
	
	/* check for string match, then see if it is a _word_ */
	while (lowwhatis && (p = strstr(lowwhatis, lowpage))) {
		char *left = p - 1; 
		char *right = p + len;
		
		if ((p == begin || (!islower(*left) && *left != '_')) &&
		    (!*right || (!islower(*right) && *right != '_')) ) {
		    	free(begin);
		    	return 1;
		}
		lowwhatis = p + 1;
	}
	
	free(begin);
	return 0;
}

/* return 1 if page matches whatis, else 0 */
static int parse_whatis(char *page, char *lowpage, char *whatis)
{ 
#ifdef REGEX
	if (regex) 
#  if defined(POSIX_REGEX)
		return (regexec(&preg, whatis, 0, (regmatch_t *) 0, 0) == 0);
#  elif defined(BSD_REGEX)
		return re_exec(whatis);
#  endif
#endif /* REGEX */

	if (wildcard)
		return (fnmatch(page, whatis, 0) == 0);

	return match(lowpage, whatis);
}
#endif /* APROPOS */

/* scan for the page, print any matches */
static int apropos(char *page, char *lowpage)
{
	datum key, cont;
	int found = 0;

#ifndef BTREE
	datum nextkey;

	key = MYDBM_FIRSTKEY(dbf);
	while (key.dptr) {
		if (*key.dptr != '$') {
			cont= MYDBM_FETCH(dbf, key);
#else /* BTREE */
	int end;

	end = btree_nextkeydata(dbf, &key, &cont);
	while (!end) {
		if (*key.dptr != '$') {
#endif /* !BTREE */
			if (*cont.dptr != '\t') {	/* a real page */
				char *tab;
				int match;
#ifdef APROPOS
				char *whatis;
#endif

				if ((tab = strrchr(key.dptr, '\t'))) 
					 *tab = '\0';


#ifdef APROPOS
				match = parse_name(lowpage, key.dptr);
				whatis = strrchr(cont.dptr, '\t');
				if (!(whatis && *++whatis))
					whatis = NULL;
					
				if (!match && whatis)
					match = parse_whatis(page, lowpage,
							     whatis);
#else /* WHATIS */
				match = parse_name(page, key.dptr);
#endif /* APROPOS */
				if (match) {
					struct mandata info;
					split_content(cont.dptr, &info);
					display(&info, key.dptr);
					found++;
					cont.dptr = info.addr;
				}
				
				found += match;
				if (tab)
					*tab = '\t';
			}
#ifndef BTREE
			MYDBM_FREE(cont.dptr);
		}
		nextkey = MYDBM_NEXTKEY(dbf, key);
		MYDBM_FREE(key.dptr);
		key = nextkey; 
#else /* BTREE */
		}
		MYDBM_FREE(cont.dptr);
		MYDBM_FREE(key.dptr);
		end = btree_nextkeydata(dbf, &key, &cont);
#endif /* !BTREE */
	}

	return found;
}

/* loop through the man paths, searching for a match */
static void search(char *page)
{
	int found = 0;
	char *lowpage = lower(page);
	char *catpath, **mp;
	
	if (debug)
		fprintf(stderr, "lower(%s) = \"%s\"\n", page, lowpage);
		
	for (mp = manpathlist; *mp; mp++) {
		catpath = global_catpath(*mp);
		
		if (catpath) {
			database = mkdbname(catpath);
			free(catpath);
		} else
			database = mkdbname(*mp);

		if (debug)
			fprintf(stderr, "path=%s\n", *mp);

		if ( !(dbf = MYDBM_RDOPEN(database)) || dbver_rd(dbf)) {
			found += use_grep(page, *mp);			
			continue;
		}
			
#ifdef WHATIS
# ifdef REGEX
		if (regex || wildcard) {
# else /* !REGEX */
		if (wildcard) {
# endif /* REGEX */
			found += apropos(page, lowpage);
		} else
			found += whatis(page);
#else /* APROPOS */
		found += apropos(page, lowpage);
#endif /* WHATIS */
		MYDBM_CLOSE(dbf);
	}
	
	chkr_garbage_detector();

	if (!found) {
		printf(CATGETS(whatis_NOTHING_APPROPRIATE,
			       "%s: nothing appropriate.\n"), page);
		status = NOT_FOUND;
	}
	
	free (lowpage);
}

int main(int argc, char *argv[]) 
{
	int c;
	char *manp = NULL, *alt_systems = NULL;
	char *locale;
	int option_index;

	program_name = xstrdup(basename((argv[0])));

#ifdef HAVE_SETLOCALE
	locale = NLS_INIT;
#endif /* HAVE_SETLOCALE */
	                
	while ((c = getopt_long (argc, argv, args,
				 long_options, &option_index)) != EOF) {
		switch (c) {

			case 'd':
#ifndef debug
				debug = 1;
#endif
				break;
			case 'm':
				alt_systems = optarg;
				break;
			case 'M':
				manp = optarg;
				break;
			case 'r':
#ifdef REGEX
				regex = 1;
#endif
				break;
			case 'w':
				wildcard = 1;
				break;
			case 'f': /* fall through */
			case 'k': /* ignore, may be passed by man */
				break;
			case 'V':
				ver();
				break;
			case 'h':
				usage(OK);
				break;
			default:
				usage(FAIL);
				break;
		}
	}

#if defined(REGEX) && defined(APROPOS)
	/* Become it even if it's null - GNU standards */
	if (getenv("POSIXLY_CORRECT")) 
		regex = 1;
#endif

	/* Make sure that we have a keyword! */
	if (argc == optind) {
		printf (CATGETS(whatis_WHAT_WHAT, "%s what?\n"), program_name);
		return 0;
	}

	/* sort out the internal manpath */
	if (manp == NULL)
		manp = add_nls_manpath(manpath (alt_systems), locale);
	else
		free(manpath(NULL));
                                                                
	create_pathlist(xstrdup(manp), manpathlist);

	while (optind < argc) {
#if defined(POSIX_REGEX)		
		if (regex) {
			int errcode = regcomp(&preg, argv[optind], 
					      REG_EXTENDED|
					      REG_NOSUB|
					      REG_ICASE);
						   
			if (errcode) {
				char error_string[64];
				regerror(errcode, &preg, error_string, 64);
				error (FAIL, 0,
				       CATGETS(whatis_FAILED_REGEX,
					       "fatal: regex `%s': %s"),
				       argv[optind], error_string);
			}
		}
#elif defined(BSD_REGEX)
		if (regex) {

			/* How to set type of regex ...? */

			char *error_string = re_comp(argv[optind]);
			if (error_string)
				error (FAIL, 0,
				       CATGETS(whatis_FAILED_REGEX,
					       "fatal: regex `%s': %s"),
				       argv[optind], error_string);
		
		} 
#endif /* REGEX */
		search(argv[optind++]);
	}
	
	exit (status);
}
