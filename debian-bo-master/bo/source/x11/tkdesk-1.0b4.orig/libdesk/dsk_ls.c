/* ============================================================================
 *
 * File:	dsk_ls.c
 * Project:	TkDesk
 * Started:	24.06.94
 * Changed:	26.06.94
 *
 * Description:	ls-replacement for tkdesk. Does sorting, (masking) and all the
 *		other stuff that tkdesk requires. Output format is a simple
 *		Tcl-list or a Tcl-list of Tcl-lists.
 *
 * Copyright (C) 1996  Christian Bolik
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
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * See the file "COPYING" in the base directory of this distribution
 * for more.
 *
 * ----------------------------------------------------------------------------
 *
 * Functions:
 *
 *	linked_file
 *	perm_string
 *	type_char
 *	write_element
 *	write_list
 *	write_file_lists
 *	qsort_by_name
 *	qsort_by_size
 *	qsort_by_date
 *	qsort_by_ext
 *	sort_by_type
 *	process_file_list
 *	process_file_lists
 *	new_file_entry
 *	new_file_list
 *	read_list
 *	read_file_lists
 *	process_cmdline
 *	dsk_ls_init
 *	main
 *
 * ========================================================================== */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <time.h>		/* for ctime */
#include <unistd.h>		/* for readlink */
#include <pwd.h>		/* for getpwuid */
#include <grp.h>		/* for getgrgid */
#include <sys/types.h>
#include <sys/stat.h>
#include "libdesk.h"

/* === Defines ============================================================== */

#if defined(S_IFLNK) && !defined(S_ISLNK)
#define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)
#endif

#define MAXPATHLEN	1024
#define MAXFILELEN	256

#define BY_NAME		0
#define BY_SIZE 	1
#define BY_DATE 	2
#define BY_EXT		3
#define BY_NTH		4

#define TYPE_REG	0
#define TYPE_EXEC	1
#define TYPE_DIR	2
#define TYPE_LINK	3
#define TYPE_SOCK	4
#define TYPE_UNKN	5
#define TYPE_LDIR       6

#define USAGE		"Usage: dsk_ls [options] <path>"


/* === Typedefs ============================================================= */

typedef struct _file_entry {
    struct _file_entry *next;
    char *name;			/* file name without path */
    char *path;			/* file name with path */
    struct stat sb;
    int type;			/* one of TYPE_* */
} FILE_ENTRY;

typedef struct _file_list {
    struct _file_list *next;
    struct _file_entry *first;    
    int num_files;
    struct _file_entry **fe_array;	/* for qsorting */
} FILE_LIST;


/* === Global Variables ===================================================== */

/* these are set throught the cmd-line */

int par_every_dir = 0,		/* read files for every dir in path? */
    par_sort_type = BY_NAME,	/* how to sort the file-lists */
    par_sort_by_type = 0,	/* sort by type? (only distincts dir/other) */
    par_show_all = 0,		/* list all files? (starting with .) */
    par_invert = 0,		/* invert the file-lists? */
    par_add_date = 0,		/* add date to every file? */
    par_add_size = 0,		/* add size to every file? */
    par_long_listing = 0,	/* long listing? (implies date and size) */
    par_no_points = 0,		/* hide . and .. entries? */
    par_append_char = 0,	/* append *,@,/,=," " ? */
    par_print_num = 0,		/* print # files as first entry? */
    par_one_file = 0,		/* treat a directory like other files? */
    par_only_directories = 0;   /* list only directories? */

char par_path[MAXPATHLEN] = "";	/* path for which to list files */

/* others */

FILE_LIST root_of_lists = {NULL, NULL, 0, NULL};

int just_one_file = 0;


/* ========================================================================== */
/*                                 Functions				      */
/* ========================================================================== */


/* ============================================================================
 * Name: linked_file
 * Desc: if fe->name is a symlink, it returns the name of the linked file
 * In  : fe - adress of file entry
 * Out : "" or name of linked file
 * -------------------------------------------------------------------------- */

char *linked_file (fe)
FILE_ENTRY *fe;
{
static char linkname[MAXPATHLEN];
int len;

    if (S_ISLNK(fe->sb.st_mode)) {
	strcpy (linkname, " -> ");
	len = readlink (fe->path, linkname + 4, MAXPATHLEN - 4);
	*(linkname + 4 + len) = 0;
	return (linkname);
    } else {
	return (strcpy (linkname, ""));
    }
} /* linked_file */


/* ============================================================================
 * Name: perm_string
 * Desc: Builds a perm-string from the st_mode-field. Almost completely
 *	 copied from the O'Reilly-Book "Using C ...", Ex. 4.3.
 * In  : fe - adress of file-entry
 * Out : perm-string
 * -------------------------------------------------------------------------- */

char *perm_string (fe)
    FILE_ENTRY *fe;
{
static char permstr[12];
static char *modes[] = {
    	"---", "--x", "-w-", "-wx",
    	"r--", "r-x", "rw-", "rwx"
    };
int i,j;
mode_t mb;

    mb = fe->sb.st_mode;

    /*
     * Get the file type.  For convenience (and to
     * make this example universal), we ignore the
     * other types which are version-dependent.
     */
    switch (mb & S_IFMT) {
    case S_IFREG:    permstr[0] = '-'; break;
    case S_IFDIR:    permstr[0] = 'd'; break;
    case S_IFCHR:    permstr[0] = 'c'; break;
    case S_IFBLK:    permstr[0] = 'b'; break;
    case S_IFLNK:    permstr[0] = 'l'; break;	/* symbolic link */
    case S_IFSOCK:   permstr[0] = 's'; break;
    default:         permstr[0] = '?'; break;
    }

    /*
     * Get each of the three groups of permissions
     * (owner, group, world).  Since they're just
     * bits, we can count in binary and use this
     * as a subscript (see the modes array, above).
     */
    permstr[1] = 0;
    for (i = 2; i >= 0; i--) {
        /*
         * Since we're subscripting, we don't
         * need the constants.  Just get a
         * value between 0 and 7.
         */
        j = (mb >> (i*3)) & 07;

        /*
         * Get the perm bits.
         */
        strcat(permstr, modes[j]);
    }

    /*
     * Handle special bits which replace the 'x'
     * in places.
     */
    if ((mb & S_ISUID) != 0) {
	if (mb & S_IXUSR)
	    permstr[3] = 's';
	else
	    permstr[3] = 'S';
    }
    if ((mb & S_ISGID) != 0) {
	if (mb & S_IXGRP)
	    permstr[6] = 's';
	else
	    permstr[6] = 'S';
    }
    if ((mb & S_ISVTX) != 0)
        permstr[9] = 't';

    return (permstr);
} /* perm_string */


/* ============================================================================
 * Name: type_char
 * Desc: return a character that represents the type of file fe
 * In  : fe - adress of file entry
 * Out : see Desc
 * -------------------------------------------------------------------------- */

char type_char (fe)
    FILE_ENTRY *fe;
{
char c;
struct stat rs;

    if (!par_append_char)
	return (' ');

    switch (fe->type) {
	case TYPE_REG:	c = '_'; break;
	case TYPE_EXEC:	c = '*'; break;
	case TYPE_DIR:	c = '/'; break;
	case TYPE_LDIR:	c = '-'; break;
	case TYPE_LINK:
	  /* determine type of linked-to file */
	  stat(fe->path, &rs);
	  switch (rs.st_mode & S_IFMT) {
	    case S_IFREG:    	
	      if (rs.st_mode & S_IEXEC)
    	    	c = '+';
	      else
    	    	c = '@';
	      break;
	    case S_IFDIR:    	
	      c = '-';
	      break;
	    default: c = '@';
	  }
	  break;
	case TYPE_SOCK:	c = '='; break;
	case TYPE_UNKN:	c = '_'; break;
	default: c = '!';
    }

    return (c);
    
} /* type_char */

/* ============================================================================
 * Name: get_user
 * Desc: Returns the login name for a given uid.
 * In  : uid - user id
 * Out : pointer to static string: login name
 * -------------------------------------------------------------------------- */

char *get_user (uid)
uid_t uid;
{
static char name[32];
struct passwd *pw;

    pw = getpwuid (uid);
    if (pw) {
	strcpy (name, pw->pw_name);
    } else {
	sprintf (name, "%d", (int) uid);
    }

    return (name);
} /* get_user */

/* ============================================================================
 * Name: get_group
 * Desc: Returns the group name for a given gid.
 * In  : gid - group id
 * Out : pointer to static string: group name
 * ------------------------------------------------------------------------- */

char *get_group (gid)
gid_t gid;
{
static char name[32];
struct group *gr;

    gr = getgrgid (gid);
    if (gr) {
	strcpy (name, gr->gr_name);
    } else {
	sprintf (name, "%d", (int) gid);
    }

    return (name);
} /* get_group */

/* ============================================================================
 * Name: file_time
 * Desc: Returns the formatted modification time.
 * In  : mtime
 * Out : pointer to static string: "Jan 31 12:00" or "Jan 31 1990"
 *       (2nd format if file is older than half a year (182 days))
 * ------------------------------------------------------------------------- */

char *file_time (filetime)
time_t filetime;
{
    time_t curtime;
    struct tm *filetmp, *curtmp, filetm, curtm;
    static char timestr[32];

    time (&curtime);
    filetmp = gmtime (&filetime);
    filetm.tm_year = filetmp->tm_year;
    filetm.tm_yday = filetmp->tm_yday;
    curtmp = gmtime (&curtime);
    curtm.tm_year = curtmp->tm_year;
    curtm.tm_yday = curtmp->tm_yday;

    strncpy (timestr, ctime (&filetime) + 4, 12);
    if ((filetm.tm_year == curtm.tm_year &&
	 (curtm.tm_yday - filetm.tm_yday) < 182) ||
	(filetm.tm_year == curtm.tm_year - 1 &&
	 filetm.tm_yday > (182 + curtm.tm_yday)))
	timestr[12] = '\0';
    else {
	timestr[6] = '\0';
	sprintf (timestr, "%s 19%d", timestr, filetm.tm_year);
    }

    return timestr;
} /* file_time */

/* ============================================================================
 * Name: write_element
 * Desc: writes info on fe to stdout as a tcl-list-element
 * In  : fe - address of file entry
 * Out : 
 * ------------------------------------------------------------------------- */

void write_element (fe)
    FILE_ENTRY *fe;
{
    char buf[256], *np;

    np = (par_only_directories) ? fe->path : fe->name;

    /* this may look funny, but I guess that it's faster to have printf
       just called once per call to write_element */

    if (!par_long_listing && !par_add_date && !par_add_size)
    	sprintf (buf, "{%s%c} ", np, type_char (fe));
    else if (par_long_listing)
#ifndef OLDLSFORMAT
	sprintf (buf, "{%s%c\\t%d\\t%s\\t%s\\t%s\\t%s\\t%3d\\t%s} ", 
		 np, type_char (fe),
		 (int) fe->sb.st_size, file_time((fe->sb.st_mtime)), 
		 get_user(fe->sb.st_uid), get_group(fe->sb.st_gid),
		 perm_string (fe), fe->sb.st_nlink, linked_file(fe));
#else
	sprintf (buf, "{%s\\t%3d\\t%s\\t%s\\t%d\\t%.12s\\t%s%c%s} ", 
			perm_string (fe), fe->sb.st_nlink,
			get_user(fe->sb.st_uid), get_group(fe->sb.st_gid), 
			(int) fe->sb.st_size, ctime(&(fe->sb.st_mtime)) + 4, 
			np, type_char (fe), linked_file(fe));
#endif
    else if (par_add_date && !par_add_size)
	sprintf (buf, "{%.12s\\t%s%c} ", ctime(&(fe->sb.st_mtime)) + 4, np,
				type_char (fe));
    else if (!par_add_date && par_add_size)
	sprintf (buf, "{\\t%d\\t%s%c} ", 
		(int) fe->sb.st_size, np, type_char (fe));
    else if (par_add_date && par_add_size)
	sprintf (buf, "{\\t%d\\t%.12s\\t%s%c} ", (int) fe->sb.st_size,
		ctime(&(fe->sb.st_mtime)) + 4, np, type_char (fe));

    Tcl_AppendResult (dsk_interp, buf, NULL);
} /* write_element */


/* ============================================================================
 * Name: write_list
 * Desc: writes a list of fl's files to stdout
 * In  : fl - adress of file list
 * Out : 
 * -------------------------------------------------------------------------- */

void write_list (fl)
    FILE_LIST *fl;
{
FILE_ENTRY *fe;
int i;
char buf[32];
    
    fe = fl->first;

    if (par_print_num) {
	sprintf (buf, "{%d} ", fl->num_files);
	Tcl_AppendResult (dsk_interp, buf, NULL);
    }

/*    while (fe) {
	write_element (fe);
	fe = fe->next;
    }
*/

    if (!par_invert)
        for (i = 0; i < fl->num_files; i++)
	    write_element (fl->fe_array[i]);
    else
        for (i = fl->num_files - 1; i >= 0; i--)
	    write_element (fl->fe_array[i]);

    /* one blank entry for drag'n'drop (to satisfie Tk) ONLY FOR LISTBOXES! */
    /* Tcl_AppendResult (dsk_interp, "{}", NULL); */
} /* write_list */


/* ============================================================================
 * Name: write_file_lists
 * Desc: calls write_list, multiple times if par_every_dir
 * In  : 
 * Out : TCL_OK or TCL_ERROR
 * -------------------------------------------------------------------------- */

int write_file_lists ()
{
FILE_LIST *fl;

    
    fl = &root_of_lists;

    if (!fl->next)
	write_list (fl);
    else {
	while (fl) {
	    Tcl_AppendResult (dsk_interp, " {", NULL);
	    write_list (fl);
	    Tcl_AppendResult (dsk_interp, "} ", NULL);
	    fl = fl->next;
	}
    }

    return (TCL_OK);
} /* write_file_lists */


/* ============================================================================
 * Name: qsort_by_name
 * Desc: compare-function for qsort if par_sort_type == BY_NAME
 * In  : pointers to two file-entries
 * Out : -1, 0, 1 (see strcmp(3) and qsort(3))
 * -------------------------------------------------------------------------- */

int qsort_by_name (fe1, fe2)
    FILE_ENTRY **fe1;
    FILE_ENTRY **fe2;
{
    return (strcmp ((*fe1)->name, (*fe2)->name));

} /* qsort_by_name */


/* ============================================================================
 * Name: qsort_by_size
 * Desc: compare-function for qsort if par_sort_type == BY_SIZE
 * In  : pointers to two file-entries
 * Out : -1, 0, 1 (see strcmp(3) and qsort(3))
 * -------------------------------------------------------------------------- */

int qsort_by_size (fe1, fe2)
    FILE_ENTRY **fe1;
    FILE_ENTRY **fe2;
{
    if ((*fe1)->name[0] == '.') {
	if (strcmp ((*fe1)->name, ".") == 0)
	    return -1;
    
	if (strcmp ((*fe1)->name, "..") == 0) {
	    if (strcmp ((*fe2)->name, ".") == 0)
		return 1;
	    else
		return -1;
	}
    }

    if ((*fe1)->sb.st_size > (*fe2)->sb.st_size)
	return (-1);
    else if ((*fe1)->sb.st_size < (*fe2)->sb.st_size)
	return (1);
    else
    	return (strcmp ((*fe1)->name, (*fe2)->name));

} /* qsort_by_size */


/* ============================================================================
 * Name: qsort_by_date
 * Desc: compare-function for qsort if par_sort_type == BY_DATE
 * In  : pointers to two file-entries
 * Out : -1, 0, 1 (see strcmp(3) and qsort(3))
 * -------------------------------------------------------------------------- */

int qsort_by_date (fe1, fe2)
    FILE_ENTRY **fe1;
    FILE_ENTRY **fe2;
{

    if ((*fe1)->name[0] == '.') {
	if (strcmp ((*fe1)->name, ".") == 0)
	    return -1;
    
	if (strcmp ((*fe1)->name, "..") == 0) {
	    if (strcmp ((*fe2)->name, ".") == 0)
		return 1;
	    else
		return -1;
	}
    }

    if ((*fe1)->sb.st_mtime > (*fe2)->sb.st_mtime)
	return (-1);
    else if ((*fe1)->sb.st_mtime < (*fe2)->sb.st_mtime)
	return (1);
    else
    	return (strcmp ((*fe1)->name, (*fe2)->name));

} /* qsort_by_date */


/* ============================================================================
 * Name: qsort_by_ext
 * Desc: compare-function for qsort if par_sort_type == BY_EXT
 * In  : pointers to two file-entries
 * Out : -1, 0, 1 (see strcmp(3) and qsort(3))
 * -------------------------------------------------------------------------- */

int qsort_by_ext (fe1, fe2)
    FILE_ENTRY **fe1;
    FILE_ENTRY **fe2;
{
    char *lp1, *lp2;
    int r;

    if ((*fe1)->name[0] == '.') {
	if (strcmp ((*fe1)->name, ".") == 0)
	    return -1;
    
	if (strcmp ((*fe1)->name, "..") == 0) {
	    if (strcmp ((*fe2)->name, ".") == 0)
		return 1;
	    else
		return -1;
	}
    }

    lp1 = strrchr ((*fe1)->name, '.');
    lp2 = strrchr ((*fe2)->name, '.');

    if (!lp1 && !lp2)
    	return (strcmp ((*fe1)->name, (*fe2)->name));
    else if (!lp1 && lp2)
	return (-1);
    else if (lp1 && !lp2)
	return (1);
    else {
	r = strcmp (lp1 + 1, lp2 + 1);
	if (r)
	    return (r);
	else
	    return (strcmp ((*fe1)->name, (*fe2)->name));
    }

} /* qsort_by_ext */


/* ============================================================================
 * Name: sort_by_type
 * Desc: sorts the file list by type (currently just dir or not dir)
 * In  : fl - adress of file list
 * Out : 
 * -------------------------------------------------------------------------- */

void sort_by_type (fl)
    FILE_LIST *fl;
{
FILE_ENTRY **fea1, **fea2;
int i, num_dir = 0, num_oth = 0;

    /* fea1 will hold directories */
    fea1 = (FILE_ENTRY **) malloc (
			fl->num_files * (sizeof (FILE_ENTRY *)));
    if (!fea1) {
	perror ("sort_by_type (fea1)");
	exit (1);
    }
    
    /* fea2 will hold everything but directories */
    fea2 = (FILE_ENTRY **) malloc (
			fl->num_files * (sizeof (FILE_ENTRY *)));
    if (!fea2) {
	perror ("sort_by_type (fea2)");
	exit (1);
    }

    for (i = 0; i < fl->num_files; i++) {
	if (fl->fe_array[i]->type == TYPE_DIR
	    || fl->fe_array[i]->type == TYPE_LDIR) {
	    fea1[num_dir] = fl->fe_array[i];
	    num_dir++;
	} else {
	    fea2[num_oth] = fl->fe_array[i];
	    num_oth++;
	}
    }

    /* ensure that directories are always on top */
    if (!par_invert) {    
    	memcpy ((char *)fl->fe_array, fea1, num_dir * sizeof (FILE_ENTRY *));
    	memcpy ((char *)fl->fe_array + num_dir * sizeof (FILE_ENTRY *), 
		fea2, num_oth * sizeof (FILE_ENTRY *));
    } else {
    	memcpy ((char *)fl->fe_array, fea2, num_oth * sizeof (FILE_ENTRY *));
    	memcpy ((char *)fl->fe_array + num_oth * sizeof (FILE_ENTRY *), 
		fea1, num_dir * sizeof (FILE_ENTRY *));
    }

    free (fea1);
    free (fea2);

} /* sort_by_type */


/* ============================================================================
 * Name: process_file_list
 * Desc: sorts the list using qsort on an array of pointers to file entries
 * In  : fl - adress of file list
 * Out : 
 * ------------------------------------------------------------------------- */

void process_file_list (fl)
    FILE_LIST *fl;
{
    FILE_ENTRY *fe;
    int i = 0;

    /* bug-fix by Zsolt Koppany <zkoppany@multix.de> */
    if (fl->num_files == 0)
        return;
    
    fl->fe_array = (FILE_ENTRY **) malloc (
			fl->num_files * (sizeof (FILE_ENTRY *)));

    if (!fl->fe_array) {
	perror ("process_file_list");
	exit (1);
    }

    if (fl->num_files < 2) {
	fl->fe_array[0] = fl->first;
	return;
    }

    /* fill the array of pointers to file entries */
    fe = fl->first;
    while (fe) {
	fl->fe_array[i++] = fe;
	fe = fe->next;
    }

    switch (par_sort_type) {
	case BY_NAME:
	    qsort (fl->fe_array, fl->num_files, sizeof (FILE_ENTRY *),
			qsort_by_name); break;
	case BY_SIZE:
	    qsort (fl->fe_array, fl->num_files, sizeof (FILE_ENTRY *),
			qsort_by_size); break;
	case BY_DATE:
	    qsort (fl->fe_array, fl->num_files, sizeof (FILE_ENTRY *),
			qsort_by_date); break;
	case BY_EXT:
	    qsort (fl->fe_array, fl->num_files, sizeof (FILE_ENTRY *),
			qsort_by_ext); break;
    }

    if (par_sort_by_type)
    	sort_by_type (fl);

} /* process_file_list */


/* ============================================================================
 * Name: process_file_lists
 * Desc: calls write_list, multiple times if par_every_dir
 * In  : 
 * Out : TCL_OK or TCL_ERROR
 * -------------------------------------------------------------------------- */

int process_file_lists ()
{

FILE_LIST *fl;

    fl = &root_of_lists;

    if (!fl->next)
	process_file_list (fl);
    else {
	while (fl) {
	    process_file_list (fl);
	    fl = fl->next;
	}
    }

    return TCL_OK;
} /* process_file_lists */


/* ============================================================================
 * Name: new_file_entry
 * Desc: allocates and fills a new FILE_ENTRY
 * In  : name - complete name of file
 * Out : address of new FILE_ENTRY
 * -------------------------------------------------------------------------- */

FILE_ENTRY *new_file_entry (name)
    char *name;
{
    FILE_ENTRY *nfe, *fe;
    FILE_LIST *fl;
    char *np, *ls, buf[BUFSIZ];
    struct stat sb;

#ifdef LSDEBUG
    fprintf (stderr, "\tEntering new_file_entry ...\n");
#endif
    
    nfe = (FILE_ENTRY *) malloc (sizeof (FILE_ENTRY));
    if (!nfe) {
	perror ("new_file_entry (nfe)");
	exit (1);
    }

    /* alloc length * 2 to have space for escaping special characters */
    np = (char *) malloc (strlen(name) * 2);
    if (!np) {
	perror ("new_file_entry (np)");
	exit (1);
    }

    nfe->next = NULL;
    nfe->path = np;
    escape_chars (name, "\"[]{}$;", buf);
    strcpy (nfe->path, buf);
#ifdef LSDEBUG
    fprintf (stderr, "\tnfe->path: %s\n", nfe->path);
#endif
    ls = strrchr (nfe->path, '/');
    if (!ls)
    	nfe->name = nfe->path;
    else
    	nfe->name = ls+1;
#ifdef LSDEBUG
    fprintf (stderr, "\tnfe->name: %s\n", nfe->name);
#endif

    lstat (name, &(nfe->sb));

    switch (nfe->sb.st_mode & S_IFMT) {
        case S_IFREG:    	
    	    if (nfe->sb.st_mode & S_IEXEC)
    	    	nfe->type = TYPE_EXEC;
    	    else
    	    	nfe->type = TYPE_REG;
    	    break;
        case S_IFDIR:    	
    	    nfe->type = TYPE_DIR;
    	    break;
        case S_IFLNK:    	
    	    stat (name, &sb);
    	    if (S_ISDIR(sb.st_mode))
    	        nfe->type = TYPE_LDIR;
    	    else
    	        nfe->type = TYPE_LINK;
    	    break;
        case S_IFSOCK:   	
    	    nfe->type = TYPE_SOCK;
    	    break;
        default:		
    	    nfe->type = TYPE_UNKN;
    	    break;
    }

    /* append new entry to appropriate list */

    fl = &root_of_lists;
    while (fl->next) {
	fl = fl->next;
    }

    fl->num_files++;

    if (!fl->first) {
	fl->first = nfe;
    } else {
	fe = fl->first;
	while (fe->next) {
	    fe = fe->next;
	}
	fe->next = nfe;
    }

    return (nfe);

} /* new_file_entry */


/* ============================================================================
 * Name: new_file_list
 * Desc: allocates and fills a new FILE_LIST
 * In  : 
 * Out : address of new FILE_LIST
 * -------------------------------------------------------------------------- */

FILE_LIST *new_file_list ()
{
FILE_LIST *nfl, *fl;

    nfl = (FILE_LIST *) malloc (sizeof (FILE_LIST));
    if (!nfl) {
	perror ("new_file_list");
	exit (1);
    }

    nfl->next = NULL;
    nfl->first = NULL;
    nfl->num_files = 0;
    nfl->fe_array = NULL;

    /* append the new file_list */

    fl = &root_of_lists ;
    while (fl->next)
	fl = fl->next;

    fl->next = nfl;

    return (nfl);
    
} /* new_file_list */


/* ============================================================================
 * Name: read_list
 * Desc: reads the files of directory path (last char: /)
 * In  : path
 * Out : TCL_OK or TCL_ERROR
 * -------------------------------------------------------------------------- */

int read_list (path)
    char *path;
{
DIR *dp;
struct dirent *dir;
char buffer[MAXPATHLEN];
struct stat sb;

#ifdef LSDEBUG
    fprintf (stderr, "Opening %s ...\n", path);
#endif
    if ((dp = opendir (path)) == NULL) {
	sprintf (dsk_interp->result, "no such file or dir: %s", path);
	return (TCL_ERROR);
    }

    while ((dir = readdir (dp))) {

	/* skip removed files */
	if (dir->d_ino == 0)
	    continue;

#ifdef LSDEBUG
    	fprintf (stderr, "Found file: %s\n", dir->d_name);
#endif
        if (!strcmp (".", dir->d_name) || !strcmp ("..", dir->d_name)) {
    	    if (!par_no_points) {
       	        strcpy (buffer, path);
       	    	strcat (buffer, dir->d_name);
    	    	new_file_entry (buffer);
    	    }
        } else if (dir->d_name[0] == '.') {
    	    if (par_show_all) {
       	        strcpy (buffer, path);
       	    	strcat (buffer, dir->d_name);
    	    	new_file_entry (buffer);
	    }
    	} else {
	    strcpy (buffer, path);
            strcat (buffer, dir->d_name);
	    if (!par_only_directories) {
		new_file_entry (buffer);
	    } else {
		stat (buffer, &sb);
		if ((sb.st_mode & S_IFMT) == S_IFDIR) {
		    new_file_entry (buffer);
		}
	    }
	}
    }

    closedir (dp);
    return (TCL_OK);       
} /* read_list */


/* ============================================================================
 * Name: read_file_lists
 * Desc: prepares par_path and par_mask, calls read_list
 * In  : 
 * Out : TCL_OK or TCL_ERROR
 * -------------------------------------------------------------------------- */

int read_file_lists ()
{
struct stat sb;
char bc, *slash;

    if (par_path[strlen (par_path) - 1] == '/' && !par_one_file) {
	/* par_path is the name of a directory */
	if (stat (par_path, &sb) < 0) {
	    sprintf (dsk_interp->result, "no such file or dir: %s", par_path);
	    return (TCL_ERROR);
	}
    } else {
	if (stat (par_path, &sb) < 0) {
	    sprintf (dsk_interp->result, "no such file or dir: %s", par_path);
	    return (TCL_ERROR);
	}
	if (!S_ISDIR(sb.st_mode) || par_one_file) {
	    /* par_path is a file */
	    write_element (new_file_entry (par_path));
	    just_one_file = 1;
	    return (TCL_OK);
	} else
	    strcat (par_path, "/");
    }

    if (!par_every_dir) {
	read_list (par_path);
    } else {
	/* read root directory */
	bc = par_path[1];
	par_path[1] = 0;
	read_list (par_path);
	par_path[1] = bc;

	/* and now one by one */
	slash = strchr (par_path + 1, '/');
	while (slash++) {
	    new_file_list ();
	    bc = *slash;
	    *slash = 0;
	    read_list (par_path);
	    *slash = bc;
	    slash = strchr (slash, '/');
	}
    }

    return TCL_OK;
} /* read_file_lists */


/* ============================================================================
 * Name: process_cmdline
 * Desc: sets global var. (par_*)  accordingly to the command line parameters
 * In  : argc, argv as in main
 * Out : TCL_OK or TCL_ERROR
 * -------------------------------------------------------------------------- */

int process_cmdline (argc, argv)
    int argc;
    char *argv[];
{
int i;

    i = 1;
    while (argc > 1) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
		case 'e':
		    par_every_dir = 1;
		    break;
		case 's':
		    if (argc == 2) {
			sprintf (dsk_interp->result, "%s", USAGE);
			return (TCL_ERROR);
		    }
		    i++; argc--;
		    if (!strcmp ("size", argv[i]))
			par_sort_type = BY_SIZE;
		    else if (!strcmp ("name", argv[i]))
			par_sort_type = BY_NAME;
		    else if (!strcmp ("date", argv[i]))
			par_sort_type = BY_DATE;
		    else if (!strcmp ("ext", argv[i]))
			par_sort_type = BY_EXT;
		    else if (!strcmp ("not", argv[i]))
			par_sort_type = BY_NTH;
		    else {
			sprintf (dsk_interp->result,
				"Don't know how to sort by %s.\n%s",
				argv[i], USAGE);
			return (TCL_ERROR);
		    }
		    break;
		case 'f':
		    par_sort_by_type = 1;
		    break;
		case 'a':
		    par_show_all = 1;
		    break;
		case 'i':
		    par_invert = 1;
		    break;
		case 'S':
		    par_add_size = 1;
		    break;
		case 'D':
		    par_add_date = 1;
		    break;
		case 'l':
		    par_long_listing = 1;
		    break;
		case 'p':
		    par_no_points = 1;
		    break;
		case 't':
		    par_append_char = 1;
		    break;
		case 'n':
		    par_print_num = 1;
		    break;
		case 'o':
		    par_one_file = 1;
		    break;
		case 'd':
		    par_only_directories = 1;
		    break;
		default:
		    sprintf (dsk_interp->result, "Dont understand: -%c\n%s",
				argv[i][1], USAGE);
		    return (TCL_ERROR);
	    }
	} else {
	    if (*par_path) {
		sprintf (dsk_interp->result, "%s", USAGE);
		return (TCL_ERROR);
	    }
	    strcpy (par_path, argv[i]);
	}
	i++;
	argc--;
    }

    if (!*par_path) {
	sprintf (dsk_interp->result, "%s", USAGE);
	return (TCL_ERROR);
    }

#ifdef LSDEBUG
    fprintf (stderr, "par_every_dir: %d\npar_sort_type: %d\npar_sort_by_type: %d\npar_show_all: %d\npar_invert: %d\npar_add_date: %d\npar_add_size: %d\npar_long_listing: %d\npar_no_points: %d\npar_append_char: %d\npar_path: %s\n", par_every_dir, par_sort_type, par_sort_by_type, par_show_all, par_invert, par_add_date, par_add_size, par_long_listing, par_no_points, par_append_char, par_path);
#endif

    return (TCL_OK);
} /* process_cmdline */


/* ============================================================================
 * Name: dsk_ls_init
 * Desc: reinitializes global variables
 * In  : 
 * Out : 
 * -------------------------------------------------------------------------- */

void dsk_ls_init ()
{
    par_every_dir = 0;		/* read files for every dir in path? */
    par_sort_type = BY_NAME;	/* how to sort the file-lists */
    par_sort_by_type = 0;	/* sort by type? (only distincts dir/other) */
    par_show_all = 0;		/* list all files? (starting with .) */
    par_invert = 0;		/* invert the file-lists? */
    par_add_date = 0;		/* add date to every file? */
    par_add_size = 0;		/* add size to every file? */
    par_long_listing = 0;	/* long listing? (implies date and size) */
    par_no_points = 0;		/* hide . and .. entries? */
    par_append_char = 0;	/* append *,@,/,=," " ? */
    par_print_num = 0;		/* print # files as first entry? */
    par_one_file = 0;
    par_only_directories = 0;

    par_path[0] = 0;

    root_of_lists.next = NULL;
    root_of_lists.first = NULL;
    root_of_lists.num_files = 0;
    root_of_lists.fe_array = NULL;    

    just_one_file = 0;
} /* dsk_ls_init */


/* ============================================================================
 * Name: dsk_ls_Cmd
 * Desc: entry point, control
 * In  : argc, argv
 * Out : TCL_OK or TCL_ERROR
 * -------------------------------------------------------------------------- */

int dsk_ls_Cmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{

    dsk_ls_init ();

    if (process_cmdline (argc, argv) != TCL_OK)
	return (TCL_ERROR);

#ifdef LSDEBUG
    fprintf( stderr, "\nEntering read_file_lists ...\n");
#endif
    if (read_file_lists () != TCL_OK)
	return (TCL_ERROR);

    if (just_one_file)
	return (TCL_OK);

#ifdef LSDEBUG
    fprintf( stderr, "\nEntering process_file_lists ...\n");
#endif
    if (process_file_lists () != TCL_OK)
	return (TCL_ERROR);

#ifdef LSDEBUG
    fprintf( stderr, "\nEntering write_file_lists ...\n");
#endif
    if (write_file_lists () != TCL_OK)
	return (TCL_ERROR);

    return (TCL_OK);
    
} /* main */
