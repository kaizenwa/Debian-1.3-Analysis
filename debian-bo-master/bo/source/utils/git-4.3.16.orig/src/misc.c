/* misc.c -- Miscelaneous functions used in git/gitps/gitview.  */

/* Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Tudor Hulubei and Andrei Pitis.  */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#else /* !HAVE_STDLIB_H */
#include "ansi_stdlib.h"
#endif /* !HAVE_STDLIB_H */

#include <signal.h>

#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#include <pwd.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "xstring.h"
#include "xmalloc.h"
#include "configure.h"
#include "file.h"
#include "tty.h"
#include "misc.h"


static char CONFIGFILE_PREFIX[] = "/.gitrc.";
static char term[] = TERMDIR;

char  *tty_name;
size_t tty_name_len;
char  *login_name;
size_t login_name_len;


char *day_name[] =
{
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};


char *month_name[] =
{
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};


/* The pointer to the head of the file type attributes list.  */
file_type_info_t *fti_head = NULL;

extern void fatal PROTO ((char *));


void
display_exit_message(signame)
    char *signame;
{
    struct tm *time = get_local_time();

    fprintf(stderr, "%s %d %2d:%02d:%02d %s[%d]: exiting on %s signal\n",
	    month_name[time->tm_mon], time->tm_mday, time->tm_hour,
	    time->tm_min, time->tm_sec, program, (int)pid, signame);
}


RETSIGTYPE
fatal_signal(signum)
    int signum;
{
    extern void clean_up PROTO (());

    clean_up();

    switch (signum)
    {
	case SIGTERM:
	case SIGQUIT:

	    display_exit_message((signum == SIGTERM) ? "TERM" : "QUIT");
	    break;

	case SIGHUP:
	case SIGINT:

	    display_exit_message((signum == SIGHUP) ? "HUP" : "INT");
	    break;

	case SIGSEGV:

	    display_exit_message("SEGV");
	    goto ask_report;

	default:

	    fprintf(stderr,
		    "%s: got a stupid signal (%d). Unless it was a joke ...\n",
		    program, signum);
	  ask_report:
	    fprintf(stderr, "%s: please report to tudor@cs.unh.edu\n",
		    program);
	    break;
    }

    exit(signum);
}


void
configuration_fatal_error(configfile)
    char *configfile;
{
    fprintf(stderr, "%s: installation problem: \n", program);
    fprintf(stderr, "%s: cannot find configuration file '%s'.\n\n",
	    program, configfile);
}


void
configuration_warning(configfile)
    char *configfile;
{
    fprintf(stderr,
	    "\n%s: Cannot open configuration file '%s'.\n",
	    program, configfile);
    fprintf(stderr,
	    "%s: See the info documentation for details.\n",
	    program);
    fprintf(stderr,
	   "%s: If the TERM environment variable is, say, vt102, your\n",
	    program);
    fprintf(stderr,
	    "%s: configuration file name is '.gitrc.vt102'.\n",
	    program);
    fprintf(stderr,
	   "%s: You can copy a configuration file in your home directory\n",
	    program);
    fprintf(stderr,
	    "%s: and modify it in order to overwrite the default one.\n",
	    program);
    fprintf(stderr,
	    "%s: Try modifying '.gitrc.generic'...\n\n",
	    program);
}


void
common_configuration_init()
{
    /* Load the global .gitrc.common file.  */
    char *configfile = xmalloc(strlen(term) + 1 + strlen(CONFIGFILE_PREFIX) +
			       sizeof("common") + 1);
    strcpy(configfile, term);
    strcat(configfile, CONFIGFILE_PREFIX);
    strcat(configfile, "common");

    if (configuration_init(configfile) == 0)
    {
	/* Give up if global .gitrc.common is not found.  */
	configuration_fatal_error(configfile);
	exit(1);
    }
}


int
specific_configuration_init()
{
    char *configfile = xmalloc(strlen(home) + 1 + strlen(CONFIGFILE_PREFIX) +
			       strlen(tty_type) + 1);
    strcpy(configfile, home);
    strcat(configfile, CONFIGFILE_PREFIX);
    strcat(configfile, tty_type);

    if (configuration_init(configfile) == 0)
    {
	xfree(configfile);
	configfile = xmalloc(strlen(term) + 1 + strlen(CONFIGFILE_PREFIX) +
			     strlen(tty_type) + 1);
	strcpy(configfile, term);
	strcat(configfile, CONFIGFILE_PREFIX);
	strcat(configfile, tty_type);

	if (configuration_init(configfile) == 0)
	{
	    configuration_warning(configfile);

	    xfree(configfile);
	    configfile = xmalloc(strlen(term) + 1 + strlen(CONFIGFILE_PREFIX) +
				 sizeof("generic") + 1);
	    strcpy(configfile, term);
	    strcat(configfile, CONFIGFILE_PREFIX);
	    strcat(configfile, "generic");

	    if (configuration_init(configfile) == 0)
	    {
		configuration_fatal_error(configfile);
		exit(1);
	    }

	    return 0;
	}
    }

    xfree(configfile);

    return 1;
}


void
use_section(section)
    char *section;
{
    if (configuration_section(section) == -1)
    {
	fprintf(stderr,
		"%s: can't find section %s in the configuration file.\n",
		program, section);
	exit(1);
    }
}


int
get_int_var(var_name, default_value)
    char *var_name;
    int default_value;
{
    char *data;

    configuration_getvarinfo(var_name, &data, 1, DO_SEEK);

    return data ? atoi(data) : default_value;
}


int
get_const_var(var_name, options, options_no, default_value)
    char *var_name, *options[];
    int options_no, default_value;
{
    int i;
    char *data;

    configuration_getvarinfo(var_name, &data, 1, DO_SEEK);

    if (data)
    {
	for (i = 0; i < options_no; i++)
	    if (strcmp(data, options[i]) == 0)
		break;

	if (i == options_no)
	    fprintf(stderr, "%s: invalid %s (%s).\n", program, var_name, data);
	else
	    return i;
    }

    return default_value;
}


int
get_flag_var(var_name, default_value)
    char *var_name;
    int default_value;
{
    char *data;

    configuration_getvarinfo(var_name, &data, 1, DO_SEEK);

    if (data)
    {
	if (strcmp(data, "ON")  == 0)
	    return 1;

	if (strcmp(data, "OFF") == 0)
	    return 0;

	fprintf(stderr, "%s: invalid %s (%s).\n", program, var_name, data);
	return default_value;
    }

    return default_value;
}


char *
get_string_var(var_name, default_value)
    char *var_name, *default_value;
{
    char *data;

    configuration_getvarinfo(var_name, &data, 1, DO_SEEK);

    if (data)
	return xstrdup(data);

    return default_value;
}


void
get_colorset_var(charset, colorset_name, fields_no)
    int *charset;
    char *colorset_name[];
    int fields_no;
{
    int i, index;
    char *data;

    for (i = 0; i < fields_no; i++)
    {
	configuration_getvarinfo(colorset_name[i], &data, 1, DO_SEEK);

	if (data)
	{
	    index = tty_get_color_index(data);
	    if (index == -1)
		fprintf(stderr, "%s: invalid %s (%s).\n",
			program, colorset_name[i], data);
	    else
		charset[i] = index;
	}
    }
}


/*
 * Minimize the path, that is, convert paths like /etc/./././///./ into
 * their cleaner form: /etc.  Stolen from the Thix kernel.  Enhanced to
 * minimize paths like /usr/local/bin/../lib as well.
 */

char *
minimize_path(path)
    char *path;
{
    char *cpath = path;
    char *opath = path;

    if (*opath == '/')
	*cpath++ = *opath++;
    else
	fatal("relative path encounter");

    while (*opath)
    {
	while (*opath == '/' ||
	       (*opath == '.' &&
		(*(opath + 1) == '/' || *(opath + 1) == '\0')))
	    opath++;

	if (*opath == '.' && *(opath + 1) == '.' &&
	    (*(opath + 2) == '/' || *(opath + 2) == '\0'))
	{
	    /* This is something like /usr/local/bin/.. and we are
	       going to remove the trailing .. along with the
	       directory that no longer meakes sense: bin.  */

	    /* Check for /..  and do nothing if this is the case.  */
	    if (cpath - 1 != path)
	    {
		for (cpath -= 2; *cpath != '/'; cpath--);
		cpath++;
	    }

	    opath += 2;
	    continue;
	}

	while (*opath && *opath != '/')
	    *cpath++ = *opath++;

	if (*opath)
	    *cpath++ = '/';
    }

    if (*(cpath - 1) == '/' && cpath - path > 1)
	cpath--;

    *cpath = '\0';
    return path;
}


void
get_tty_name()
{
    if ((tty_name = ttyname(1)) == NULL)
    {
	fprintf(stderr, "%s: can't get terminal name.\n", program);
	exit(1);
    }

    tty_name_len = strlen(tty_name);
}


void
get_login_name()
{
    struct passwd *pwd;
    int euid = geteuid();

    if ((pwd = getpwuid(euid)) == NULL)
    {
	fprintf(stderr,
		"%s: OOOPS, I can't get your user name (euid = %d) !\n",
		program, euid);
	fprintf(stderr,
		"%s: Your account no longer exists or you are a SYSTEM CRACKER !! :-)\n",
		program);
	fprintf(stderr, "%s: Correct the problem and try again.\n", program);
	exit(1);
    }

    login_name     = xstrdup(pwd->pw_name);
    login_name_len = strlen(login_name);
}


void
truncate_long_name(name, dest, len)
    char *name, *dest;
    size_t len;
{
    size_t name_len = strlen(name);

    if (name_len > len)
    {
	dest[0] = dest[1] = dest[2] = '.';
	memcpy(dest + 3, name + name_len - len + 3, len - 3);
    }
    else
	memcpy(dest, name, name_len);
}


char *
truncate_string(path, temppath, len)
    char *path;
    char *temppath;
    size_t len;
{
    truncate_long_name(path, temppath, len - 1);
    temppath[min(len - 1, strlen(path))] = '\0';
    return temppath;
}


int
get_file_length(fd)
    int fd;
{
    int current, length;

    current = lseek(fd, 0, SEEK_CUR);
    length  = lseek(fd, 0, SEEK_END);
    lseek(fd, current, SEEK_SET);
    return length;
}


struct tm *
get_local_time()
{
    time_t __time;

    /* Get the broken-down time representation.  */
    __time = time(NULL);
    return localtime(&__time);
}


#ifndef HAVE_PUTENV
#ifndef HAVE_SETENV
#ifdef NeXT
/*
 * m68k-next-nextstep (NeXT - Next Step 3.2) seems to lack putenv().
 * I have included /NextDeveloper/Source/GNU/debug/gdb/gdb/putenv.c
 * here.  Paul Nevai <nevai@ops.mps.ohio-state.edu> used it to make
 * git compile on the NeXT.
 */

/*
 * Path: hoptoad!pacbell!ames!ll-xn!mit-eddie!uw-beaver!ssc-vax!uvicctr!tholm
 * From: tholm@uvicctr.UUCP (Terrence W. Holm)
 * Newsgroups: comp.os.minix
 * Subject: putenv(3)
 * Message-ID: <395@uvicctr.UUCP>
 * Date: 5 May 88 06:40:52 GMT
 * Organization: University of Victoria, Victoria B.C. Canada
 *
 * EFTH Minix report #2  - May 1988 -  putenv(3)
 *
 * This is an implementation of putenv(3) that we
 * wrote for Minix. Please consider this a public
 * domain program.
 *
 */

extern char **environ;

/****************************************************************/
/*								*/
/*      int							*/
/*	putenv( entry )						*/
/*								*/
/*		The "entry" should follow the form 		*/
/*		"NAME=VALUE". This routine will search the 	*/
/*		user environment for "NAME" and replace its 	*/
/*		value with "VALUE".				*/
/*								*/
/*		Note that "entry" is not copied, it is used 	*/
/*		as the environment entry. This means that it 	*/
/*		must not be unallocated or otherwise modifed 	*/
/*		by the caller, unless it is replaced by a 	*/
/*		subsequent putenv().				*/
/*								*/
/*		If the name is not found in the environment, 	*/
/*		then a new vector of pointers is allocated, 	*/
/*		"entry" is put at the end and the global 	*/
/*		variable "environ" is updated.			*/
/*								*/
/*		This function normally returns NULL, but -1	*/
/*		is returned if it can not allocate enough 	*/
/*		space using malloc(3), or "entry" does not	*/
/*		contain a '='.					*/
/*								*/
/****************************************************************/


int
putenv(entry)
    char *entry;
{
    unsigned length;
    unsigned size;
    char     *temp;
    char     **p;
    char     **new_environ;

    /* Find the length of the "NAME=".  */
    temp = strchr(entry,'=');

    if (temp == 0)
	return -1;

    length = (unsigned) (temp - entry + 1);

    /* Scan through the environment looking for "NAME="  */
    for (p = environ; *p != 0 ; p++)
	if (strncmp(entry, *p, length) == 0)
	{
	    *p = entry;
	    return 0;
	}


    /*  The name was not found, build a bigger environment  */
    size = p - environ;
    new_environ = (char **)malloc((size + 2) * sizeof(char *));

    if (new_environ == (char **)NULL)
	return -1;

    memcpy((char *)new_environ, (char *)environ, size * sizeof(char *));

    new_environ[size]   = entry;
    new_environ[size+1] = NULL;

    environ = new_environ;

    return 0;
}
#else
/* I guess we are simply out of luck.  #error ?  */
#endif /* NeXT */
#endif /* HAVE_SETENV */
#endif /* HAVE_PUTENV */


/*
 * Check if it is a background command.  We are doing this by looking
 * for a '&' at the end of the command string.
 */

int
is_a_bg_command(cmd)
    char *cmd;
{
    int i;

    for (i = strlen(cmd) - 1; i >= 0; i--)
    {
	if (cmd[i] == '&')
	    return 1;

	/* Skip spaces and tabs.  */
	if (cmd[i] != ' ' && cmd[i] != key_TAB)
	    return 0;
    }

    /* No '&' found.  */
    return 0;
}


/*
 * Check if it is a empty command (containing only spaces and ';'s.
 * My guess that almost any command that doesn't contain at least one
 * alphanumeric character should be considered as being empty, but I
 * don't want to take any chances.
 */

int
is_an_empty_command(cmd)
    char *cmd;
{
    for (; *cmd; cmd++)
	if (*cmd != ' ' && *cmd != ';')
	    return 0;

    return 1;
}


void
get_file_type_info()
{
    char *contents[3];
    char pattern[80];
    int brightness, foreground, background;
    file_type_info_t *previous = NULL, *fti, *fti_head1 = NULL;


    for (;;)
    {
	configuration_getvarinfo(pattern, contents, 3, NO_SEEK);

	if (*pattern == '\0')
	    break;

	if (contents[0])
	    foreground = tty_get_color_index(contents[0]);
	else
	    foreground = -1;

	if (contents[1])
	    background = tty_get_color_index(contents[1]);
	else
	    background = -1;

	if (contents[2])
	    brightness = tty_get_color_index(contents[2]);
	else
	    brightness = -1;

	/* Insert the file type entry just obtained into the list.  */
	fti = (file_type_info_t *)xmalloc(sizeof(file_type_info_t));

	if (fti_head1 == NULL)
	    fti_head1 = previous = fti;
	else
	    previous->next = fti;

	fti->pattern    = xstrdup(pattern);
	fti->foreground = foreground;
	fti->background = background;
	fti->brightness = brightness;
	fti->next       = NULL;

	previous = fti;
    }

    /* Fixed by Marian Ciobanu <ciobi@liis.sfos.ro>, October 24, 1995.  */
    if (fti_head1)
	if (fti_head)
	{
	    previous->next = fti_head;
	    fti_head = fti_head1;
	}
	else
	    fti_head = fti_head1;
}
