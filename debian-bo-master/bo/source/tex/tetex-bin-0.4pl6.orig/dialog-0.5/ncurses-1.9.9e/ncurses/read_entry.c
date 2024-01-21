
/***************************************************************************
*                            COPYRIGHT NOTICE                              *
****************************************************************************
*                ncurses is copyright (C) 1992-1995                        *
*                          Zeyd M. Ben-Halim                               *
*                          zmbenhal@netcom.com                             *
*                          Eric S. Raymond                                 *
*                          esr@snark.thyrsus.com                           *
*                                                                          *
*        Permission is hereby granted to reproduce and distribute ncurses  *
*        by any means and for any fee, whether alone or as part of a       *
*        larger distribution, in source or in binary form, PROVIDED        *
*        this notice is included with any such distribution, and is not    *
*        removed from any of its header files. Mention of ncurses in any   *
*        applications linked with it is highly appreciated.                *
*                                                                          *
*        ncurses comes AS IS with no warranty, implied or expressed.       *
*                                                                          *
***************************************************************************/



/*
 *	read_entry.c -- Routine for reading in a compiled terminfo file
 *
 */

#include "curses.priv.h"

#include <stdlib.h>

#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#if !HAVE_EXTERN_ERRNO
extern int errno;
#endif

#include "term.h"
#include "tic.h"

TERMINAL *cur_term;

/*
 *	int
 *	_nc_read_file_entry(filename, ptr)
 *
 *	Read the compiled terminfo entry in the given file into the
 *	structure pointed to by ptr, allocating space for the string
 *	table.
 */

#undef  BYTE
#define BYTE(p,n)	(unsigned char)((p)[n])

#define IS_NEG1(p)	((BYTE(p,0) == 0377) && (BYTE(p,1) == 0377))
#define IS_NEG2(p)	((BYTE(p,0) == 0376) && (BYTE(p,1) == 0377))
#define LOW_MSB(p)	(BYTE(p,0) + 256*BYTE(p,1))

int _nc_read_file_entry(const char *const filename, TERMTYPE *ptr)
/* return 1 if read, 0 if not found or garbled, -1 if database inaccessible */
{
    int		name_size, bool_count, num_count, str_count, str_size;
    int		i, fd, numread;
    char 	buf[MAX_ENTRY_SIZE];

    if ((fd = open(filename, 0)) < 0)
    {
	if (errno == ENOENT)
	{  
	    char	*slash;

	    (void) strcpy(buf, filename);
	    if ((slash = strrchr(buf, '/')) != (char *)NULL)
		*slash = '\0';

	    if (slash && access(buf, R_OK))
		return(-1);
	}

	return(0);
    }

    /* grab the header */
    (void) read(fd, buf, 12);
    if (LOW_MSB(buf) != MAGIC)
    {
	close(fd);
	return(0);
    }
    name_size  = LOW_MSB(buf + 2);
    bool_count = LOW_MSB(buf + 4);
    num_count  = LOW_MSB(buf + 6);
    str_count  = LOW_MSB(buf + 8);
    str_size   = LOW_MSB(buf + 10);

    /* try to allocate space for the string table */
    ptr->str_table = malloc((unsigned)str_size);
    if (ptr->str_table == NULL)
    {
	close(fd);
	return(0);
    }

    /* grab the name */
    read(fd, buf, min(MAX_NAME_SIZE, (unsigned)name_size));
    buf[MAX_NAME_SIZE] = '\0';
    ptr->term_names = calloc(strlen(buf) + 1, sizeof(char));
    (void) strcpy(ptr->term_names, buf);
    if (name_size > MAX_NAME_SIZE)
	lseek(fd, (off_t) (name_size - MAX_NAME_SIZE), 1);

    /* grab the booleans */
    read(fd, ptr->Booleans, min(BOOLCOUNT, (unsigned)bool_count));
    if (bool_count > BOOLCOUNT)
	lseek(fd, (off_t) (bool_count - BOOLCOUNT), 1);
    else
	for (i=bool_count; i < BOOLCOUNT; i++)
	    ptr->Booleans[i] = 0;

    /*
     * If booleans end on an odd byte, skip it.  The machine they 
     * originally wrote terminfo on must have been a 16-bit 
     * word-oriented machine that would trap out if you tried a
     * word access off a 2-byte boundary.
     */
    if ((name_size + bool_count) % 2 != 0)
	read(fd, buf, 1);

    /* grab the numbers */
    (void) read(fd, buf, min(NUMCOUNT*2, (unsigned)num_count*2));
    for (i = 0; i < min(num_count, NUMCOUNT); i++) 
    {
	if (IS_NEG1(buf + 2*i))
	    ptr->Numbers[i] = -1;
	else if (IS_NEG2(buf + 2*i))
	    ptr->Numbers[i] = -2;
	else
	    ptr->Numbers[i] = LOW_MSB(buf + 2*i);
    }
    if (num_count > NUMCOUNT)
	lseek(fd, (off_t) (2 * (num_count - NUMCOUNT)), 1);
    else
	for (i=num_count; i < NUMCOUNT; i++)
	    ptr->Numbers[i] = -1;

    /* grab the string offsets */
    numread = read(fd, buf, (unsigned)(str_count*2));
    if (numread < str_count*2)
    {
	close(fd);
	return(0);
    }
    for (i = 0; i < numread/2; i++)
    {
	if (IS_NEG1(buf + 2*i))
	    ptr->Strings[i] = (char *)0;
	else if (IS_NEG2(buf + 2*i))
	    ptr->Strings[i] = (char *)-1;
	else
	    ptr->Strings[i] = (LOW_MSB(buf+2*i) + ptr->str_table);
    }
    if (str_count > STRCOUNT)
	lseek(fd, (off_t) (2 * (str_count - STRCOUNT)), 1);
    else
	for (i = str_count; i < STRCOUNT; i++)
	    ptr->Strings[i] = 0;

    /* finally, grab the string table itself */
    numread = read(fd, ptr->str_table, (unsigned)str_size);
    close(fd);
    if (numread != str_size)
	return(0);

    return(1);
}

/*
 *	_nc_read_entry(char *tn, char *filename, TERMTYPE *tp)
 *
 *	Find and read the compiled entry for a given terminal type,
 *	if it exists.  We take pains here to make sure no combination
 *	of environment variables and terminal type name can be used to
 *	overrun the file buffer.
 */

int _nc_read_entry(const char *const tn, char *const filename, TERMTYPE *const tp)
{
char		*envp;
char		ttn[MAX_ALIAS + 1];

/* maximum safe length of terminfo root directory name */
#define MAX_TPATH	(PATH_MAX - MAX_ALIAS - 6)

	/* truncate the terminal name to prevent dangerous buffer airline */
	(void) strncpy(ttn, tn, MAX_ALIAS);
	ttn[MAX_ALIAS] = '\0';

	/* this is System V behavior */
	if ((envp = getenv("TERMINFO")) != NULL)
	{
		char	terminfo[PATH_MAX];

		(void) strncpy(terminfo, envp, MAX_TPATH);
		terminfo[MAX_TPATH] = '\0';
		(void) sprintf(filename, "%s/%c/%s", terminfo, ttn[0], ttn);
		if (_nc_read_file_entry(filename, tp) == 1)
			return(1);
	}

	/* this is an ncurses extension */
	else if ((envp = getenv("HOME")) != NULL)
	{
		char	home[MAX_TPATH + 1];

		(void) strncpy(home, envp, MAX_TPATH - strlen(PRIVATE_INFO));
		home[MAX_TPATH - strlen(PRIVATE_INFO)] = '\0';
		(void) sprintf(filename, PRIVATE_INFO, home);
		(void) sprintf(filename + strlen(filename), "/%c/%s",ttn[0],ttn);
		if (_nc_read_file_entry(filename, tp) == 1)
			return(1);
	}

	/* try the system directory */
	(void) sprintf(filename, "%s/%c/%s", TERMINFO, ttn[0], ttn);
	return(_nc_read_file_entry(filename, tp));
}

/*
 *	_nc_first_name(char *names)
 *
 *	Extract the primary name from a compiled entry.
 */

char *_nc_first_name(const char *const sp)
/* get the first name from the given name list */
{
    static char	buf[MAX_NAME_SIZE];
    register char *cp;

    (void) strcpy(buf, sp);

    cp = strchr(buf, '|');
    if (cp)
	*cp = '\0';

    return(buf);
}

/*
 *	bool _nc_name_match(namelist, name, delim)
 *
 *	Is the given name matched in namelist?
 */

int _nc_name_match(char *const namelst, const char *const name, const char *const delim)
/* microtune this, it occurs in several critical loops */
{
char namecopy[MAX_ENTRY_SIZE];	/* this may get called on a TERMCAP value */
register char *cp;

	if (namelst == NULL)
		return(FALSE);
    	(void) strcpy(namecopy, namelst);
    	if ((cp = strtok(namecopy, delim)) != NULL)
    		do {
			/* avoid strcmp() function-call cost if possible */
			if (cp[0] == name[0] && strcmp(cp, name) == 0)
			    return(TRUE);
    		} while
		    ((cp = strtok((char *)NULL, delim)) != NULL);

    	return(FALSE);
}

