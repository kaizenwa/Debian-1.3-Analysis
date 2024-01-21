/*
**	$Id: defaults.c,v 2.0 90/09/19 19:42:09 paul Rel $
**
**	Routines to access runtime defaults file.
**	This is to allow program features to be configured
**	without the need to recompile.
**
**	XENIX has defopen(S) and defread(S), but I think this is better,
**	since it reads the file only once, storing the values in core.
**	It is certainly more portable.
*/

/*
**	Copyright 1989,1990 by Paul Sutcliffe Jr.
**
**	Permission is hereby granted to copy, reproduce, redistribute,
**	or otherwise use this software as long as: there is no monetary
**	profit gained specifically from the use or reproduction or this
**	software, it is not sold, rented, traded or otherwise marketed,
**	and this copyright notice is included prominently in any copy
**	made.
**
**	The author make no claims as to the fitness or correctness of
**	this software for any use whatsoever, and it is provided as is. 
**	Any use of this software is at the user's own risk.
*/

/*
**	$Log:	defaults.c,v $
**	Revision 2.0  90/09/19  19:42:09  paul
**	Initial 2.0 release
**	
*/


#include "getty.h"
#include "defaults.h"
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>


/*
**	defbuild() - create in-core list of defaults
**
**	Returns (DEF**)NULL if no defaults file found or an error occurs.
*/

DEF **
defbuild(filename)
char *filename;
{
	register int i;
	register DEF *dp;
	register DEF *next;
	FILE *fp;
	char *fname, defname[MAXLINE+1], buf[MAXLINE+1];
	static DEF *deflist[MAXDEF+1];		/* in-core list */
	struct stat st;
	extern int errno;

	debug(D_DEF, "defbuild(%s) called",
			((filename == (char *) NULL) ? "NULL" : filename));

	/* look to see if there's a DEFAULTS/MyName.Device file
	 */
	(void) sprintf(buf, "%s", DEFAULTS);
	(void) strcat(buf, ".%s");
	(void) sprintf(defname, buf, MyName, Device);
	debug(D_DEF, "looking for %s", defname);
	if ((stat(defname, &st) == FAIL) && errno == ENOENT) {	/* Nope */
		debug(D_DEF, "stat failed, no file");
		(void) sprintf(defname, DEFAULTS, MyName);
	}

	fname = (filename != (char *) NULL) ? filename : defname;

	/* if fname doesn't begin with a '/', assume it's a
	 * filename to be made "DEFAULTS/fname"
	 */
	if (*fname != '/') {
		(void) sprintf(defname, DEFAULTS, fname);
		fname = defname;
	}

	debug(D_DEF, "fname = (%s)", fname);

	if ((fp = defopen(fname)) == (FILE *) NULL) {
		debug(D_DEF, "defopen() failed");
		return((DEF **) NULL);		/* couldn't open file */
	}

	for (i=0; i < MAXDEF; i++) {
		if ((dp = defread(fp)) == (DEF *) NULL)
			break;
		if ((next = (DEF *) malloc((unsigned) sizeof(DEF))) ==
		    (DEF *) NULL) {
			logerr("malloc() failed: defaults list truncated");
			break;
		}
		next->name = dp->name;
		next->value = dp->value;
		deflist[i] = next;
		debug(D_DEF, "deflist[%d]: name=(%s), value=(%s)",
				i, deflist[i]->name, deflist[i]->value);
	}
	deflist[i] = (DEF *) NULL;	/* terminate list */
	(void) defclose(fp);
	debug(D_DEF, "defbuild() successful");
	return(deflist);
}


/*
**	defvalue() - locate the value in "deflist" that matches "name"
**
**	Returns (char*)NULL if no match is made.
*/

char *
defvalue(deflist, name)
register DEF **deflist;
register char *name;
{
	debug(D_DEF, "defvalue(%s) called", name);

	if (deflist != (DEF **) NULL)
		for (; *deflist != (DEF *) NULL; deflist++)
			if (strequal(name, (*deflist)->name)) {
				debug(D_DEF, "defvalue returns (%s)",
						(*deflist)->value);
				return((*deflist)->value);  /* normal exit */
			}

	debug(D_DEF, "defvalue returns NULL");
	return((char *) NULL);
}


/*
**	defopen() - open the defaults file
**
**	Returns (FILE*)NULL if file not found or an error occurs.
*/

FILE *
defopen(filename)
register char *filename;
{
	if (filename != (char *) NULL)
		return(fopen(filename, "r"));

	return((FILE *) NULL);
}


/*
**	defread() - read a line from the defaults file
**
**	Returns (DEF*)NULL if an error occurs.
*/

DEF *
defread(fp)
register FILE *fp;
{
	register char *p, *p2;
	char buf[MAXLINE+1];	/* buffer large enough for 1 line */
	static DEF def;

	do {
		if (fgets(buf, sizeof(buf), fp) == (char *) NULL)
			return((DEF *) NULL);	/* no more lines */

	} while ((buf[0] == '#') || (buf[0] == '\n'));
	  /* SMR - ignore comment lines */

	buf[strlen(buf)-1] = '\0';		/* rm trailing \n */

	/* lines should be in the form "NAME=value"
	 */
	if ((p = index(buf, '=')) == (char *) NULL) {
		logerr("bad defaults line: %s", buf);
		return((DEF *) NULL);
	}
	*p++ = '\0';		/* split into two fields, name and value */
	while (*p && isspace(*p)) {
		p++;		/* Jump past space before value. */
	}
	p2 = p + strlen(p) - 1;
	while (isspace(*p2)) {
		*p2-- = 0;	/* Remove spaces from end of value. */
	}
	def.value = strdup(p);
	p = buf;
	while (*p == ' ') {
		p++;		/* Jump past spaces before variable name. */
	}
	p2 = p + strlen(p) - 1;
	while (isspace(*p2)) {
		*p2-- = 0;	/* Remove spaces from end of variable name. */
	}
	def.name = strdup(p);

	return(&def);
}


/*
**	defclose() - closes the defaults file
**
**	Returns EOF if an error occurs.
*/

int
defclose(fp)
register FILE *fp;
{
	return(fclose(fp));
}


