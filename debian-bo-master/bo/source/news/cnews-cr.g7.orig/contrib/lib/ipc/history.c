#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include "dbz.h"
#include "fgetfln.h"
#include "history.h"
#include "debug.h"
#include "libcnews.h"

#ifndef SEEK_SET
# define SEEK_SET 0
#endif

static FILE *histfp;
static char *histfile;

void
histinit()
{
	if (histfile == NULL)
		histfile = ctlfile("history");
	if (dbminit(histfile) < 0)
		error("dbminit(%s) failed", histfile);
	histfp = efopen(histfile, "r");
}

void
histclose()
{
	(void) dbmclose();
	(void) fclose(histfp);
	histfp = NULL;
}
	
/*
 * Code to do history file lookups.  Some of this code is adapted from C
 * News relay/history.c.  Modified to use fgetline instead of fgetms.
 */
char *
histlook(msgid)
char *msgid;
{
	datum key, result;
	long pos;
	char *histline;
	size_t len;

	if (histfp == NULL)
		histinit();
	key.dptr = msgid;
	key.dsize = strlen(msgid) + 1;
	result = dbzfetch(key);
	if (result.dptr == NULL)
		return NULL;
	(void) memcpy((char *)&pos, result.dptr, sizeof pos); /* align */
	ddprintf(dfp, "Seeking to history position %ld\n", pos);
	if (fseek(histfp, pos, SEEK_SET) != 0)
		error("fseek(%s) in history file", "" /* !! ltos(pos)*/);
	if ((histline = fgetline(histfp, &len)) == NULL)
		return NULL;
	if (histline[--len] == '\n')
		histline[len] = '\0';
	else
		len++;	/* shouldn't happen, just for consistency */
	return histline;
}

/*
 * Converts the files entry from the history file from its
 * newsgroup/file form to a real filename.
 */
char *
histslash(s)
char *s;
{
	register char *p = s;

	if (p == NULL)
		return p;
	while (*p != '\0') {
		if (*p == '.')
			*p = '/';
		p++;
	}
	return s;
}
