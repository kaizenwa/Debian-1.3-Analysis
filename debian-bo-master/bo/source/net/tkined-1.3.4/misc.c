/*
 * misc.c
 *
 * Some utility functions that are used in other places.
 *
 * Copyright (c) 1993, 1994, 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "tkined.h"

/*
 * A general purpose sprintf buffer. See tkined.h for its intended use.
 */

char *buffer;

/*
 * Make sure our general purpose sprintf buffer is large enough
 * to hold at least size bytes. It will never be smaller than
 * 1024 bytes.
 */

void
buffersize (size)
    int size;
{
    static int buffer_size = 0;

    if (size < 1024) size = 1024;

    if (buffer_size == 0) {
	buffer = ckalloc(size);
    } else {
	if (size > buffer_size) {
	    buffer = ckrealloc (buffer, size);
	}
    }

    buffer_size = size;
}

/*
 * Write a buffer to a file descriptor. This wrapper is needed on
 * broken SYS V machines.
 */

int
xwrite (fd, buf, len)
    int fd;
    char *buf;
    int len;
{
    int rc;

    do {
	while ((rc = write (fd, buf, len)) < 0
	       && (errno == EINTR || errno == EAGAIN))
		continue;
	len -= rc;
	buf += rc;
    } while ((len > 0) && (rc > 0));

    return rc;
}

/*
 * Read a buffer from a file descriptor. This wrapper is needed on
 * broken SYS V machines.
 */

int 
xread (fd, buf, len)
    int fd;
    char *buf;
    int len;
{
    int rc;

    while ((rc = read (fd, buf, len)) < 0
	   && (errno == EINTR || errno == EAGAIN))
	    continue;

    return rc;
}

/* 
 * Find a file searching in some likely places. Return the
 * absolute filename or NULL if not found.
 */

char* 
findfile (name)
    char *name;
{
    char *tmp;

    buffersize (strlen(name)+10);

    /*
     * Substitue ~ - we do it by hand to avoid the need for an 
     * interpreter handle.
     */

    if (name[0] == '~') {

	if ((name[1] == '/') || (name[1] == '\0')) {
	    tmp = getenv("HOME");
	    if (tmp == NULL) return (char *) NULL;
	    buffersize (strlen(tmp)+strlen(name)+10);
	    strcpy (buffer, tmp);
	    strcat (buffer, name+1);
	    if (access(buffer, R_OK) == 0) {
		return buffer;
	    } else {
		return (char *) NULL;
	    }
	} else {
	    struct passwd *pwPtr;
	    for (tmp = &name[1]; (*tmp != 0) && (*tmp != '/'); tmp++);
	    strncpy (buffer, name+1, tmp - (name+1));
	    buffer[tmp - (name+1)] = '\0';
	    pwPtr = getpwnam (buffer);
	    if (pwPtr == NULL) {
		endpwent();
		return (char *) NULL;
	    } else {
		buffersize (strlen(pwPtr->pw_dir)+strlen(name)+10);
		strcpy (buffer, pwPtr->pw_dir);
		strcat (buffer, tmp);
		endpwent();
		return buffer;
	    }
	}
    }

    if (access(name, R_OK) == 0) {
	strcpy (buffer, name);
	return buffer;
    }

    strcpy (buffer, "bitmaps/");
    strcat (buffer, name);
    if (access(buffer, R_OK) == 0) return buffer;

    if ((tmp = getenv("TKINED_PATH")) != NULL) {
	char *p, *s;
	tmp = ckstrdup(tmp);
	for (s = p = tmp; *p; p++) {
	    if (*p == ':') {
		*p = '\0';
		strcpy (buffer, s);
		strcat (buffer, "/");
		strcat (buffer, name);
		if (access(buffer, R_OK) == 0) {
		    ckfree (tmp);
		    return buffer;
		}
		s = ++p;
	    }
	}
	if (*s) {
	    strcpy (buffer, s);
	    strcat (buffer, "/");
	    strcat (buffer, name);
	    if (access(buffer, R_OK) == 0) {
		ckfree (tmp);
		return buffer;
	    }
	}
	ckfree (tmp);
    }

    if ((tmp = getenv("HOME")) != NULL) {
	buffersize (strlen(tmp)+strlen(name)+10);
	strcpy (buffer, tmp);
	strcat (buffer, "/.tkined/");
	strcat (buffer, name);
	if (access(buffer, R_OK) == 0) return buffer;
    }

    buffersize (strlen(TKINEDLIB)+strlen(name)+10);

    strcpy (buffer, TKINEDLIB);
    strcat (buffer, "/bitmaps/");
    strcat (buffer, name);
    if (access(buffer, R_OK) == 0) return buffer;

    strcpy (buffer, TKINEDLIB);
    strcat (buffer, "/site/");
    strcat (buffer, name);
    if (access(buffer, R_OK) == 0) return buffer;

    strcpy (buffer, TKINEDLIB);
    strcat (buffer, "/apps/");
    strcat (buffer, name);
    if (access(buffer, R_OK) == 0) return buffer;

    strcpy (buffer, TKINEDLIB);
    strcat (buffer, "/");
    strcat (buffer, name);
    if (access(buffer, R_OK) == 0) return buffer;

    return (char *) NULL;
}

/*
 * Convert a type id to the appropriate string.
 */

char* 
type_to_string (type)
    int type;
{
    char *result;

    switch (type) {
        case TKINED_NODE:        result = "NODE"; break;
	case TKINED_GROUP:       result = "GROUP"; break;
	case TKINED_NETWORK:     result = "NETWORK"; break;
	case TKINED_LINK:        result = "LINK"; break;
	case TKINED_TEXT:        result = "TEXT"; break;
	case TKINED_IMAGE:       result = "IMAGE"; break;
	case TKINED_INTERPRETER: result = "INTERPRETER"; break;
	case TKINED_MENU:        result = "MENU"; break;
	case TKINED_LOG:         result = "LOG"; break;
	case TKINED_REFERENCE:   result = "REFERENCE"; break;
	case TKINED_STRIPCHART:  result = "STRIPCHART"; break;
	case TKINED_BARCHART:    result = "BARCHART"; break;
	case TKINED_GRAPH:	 result = "GRAPH"; break;
	case TKINED_HTML:	 result = "HTML"; break;
	case TKINED_DATA:	 result = "DATA"; break;
	case TKINED_EVENT:	 result = "EVENT"; break;
	default:                 result = "";
    }

    return result;
}

int 
string_to_type (str)
    char *str;
{
    int type = TKINED_NONE;

    if (str != NULL) {
	if      ((*str == 'N') && (strcmp(str, "NODE") == 0))
		type = TKINED_NODE;
	else if ((*str == 'G') && (strcmp(str, "GROUP") == 0))
		type = TKINED_GROUP;
	else if ((*str == 'N') && (strcmp(str, "NETWORK") == 0))
		type = TKINED_NETWORK;
	else if ((*str == 'L') && (strcmp(str, "LINK") == 0))
		type = TKINED_LINK;
	else if ((*str == 'T') && (strcmp(str, "TEXT") == 0))
		type = TKINED_TEXT;
	else if ((*str == 'I') && (strcmp(str, "IMAGE") == 0))
		type = TKINED_IMAGE;
	else if ((*str == 'I') && (strcmp(str, "INTERPRETER") == 0))
		type = TKINED_INTERPRETER;
	else if ((*str == 'M') && (strcmp(str, "MENU") == 0))
		type = TKINED_MENU;
	else if ((*str == 'L') && (strcmp(str, "LOG") == 0))
		type = TKINED_LOG;
	else if ((*str == 'R') && (strcmp(str, "REFERENCE") == 0))
		type = TKINED_REFERENCE;
	else if ((*str == 'S') && (strcmp(str, "STRIPCHART") == 0))
		type = TKINED_STRIPCHART;
	else if ((*str == 'B') && (strcmp(str, "BARCHART") == 0))
		type = TKINED_BARCHART;
#ifdef HAVE_BLT
	else if ((*str == 'G') && (strcmp(str, "GRAPH") == 0))
		type = TKINED_GRAPH;
#else
	else if ((*str == 'G') && (strcmp(str, "GRAPH") == 0))
		type = TKINED_STRIPCHART;
#endif
	else if ((*str == 'H') && (strcmp(str, "HTML") == 0))
		type = TKINED_HTML;
	else if ((*str == 'D') && (strcmp(str, "DATA") == 0))
		type = TKINED_DATA;
	else if ((*str == 'E') && (strcmp(str, "EVENT") == 0))
		type = TKINED_EVENT;
   }

    return type;
}

/*
 * Delete the item from the list stored in slist.
 */

void
ldelete (interp, slist, item)
    Tcl_Interp *interp;
    char *slist;
    char *item;
{
    int largc, i;
    char **largv;

    if (item == NULL) return;

    if (Tcl_SplitList (interp, slist, &largc, &largv) != TCL_OK) {
	Tcl_ResetResult (interp);
	return;
    }

    *slist = 0;
    for (i = 0; i < largc; i++) {
        if ((item[0] != largv[i][0]) || (strcmp (item, largv[i]) != 0)) {
	    strcat (slist, largv[i]);
	    strcat (slist, " ");
        }
    }
    ckfree ((char*) largv);

    i = strlen (slist) - 1;
    if (slist[i] == ' ') slist[i] = '\0';
}

/*
 * Append an item to the list stored in slist.
 */

void
lappend (slist, item)
    char **slist;
    char *item;
{
    *slist = ckrealloc (*slist, strlen (*slist) + strlen(item) + 2);
    if (**slist != '\0') strcat (*slist, " ");
    strcat (*slist, item);
}


/*
 * Return a copy of the input string with all newlines replaced by
 * "\n". The result string is malloced and must be freed by the caller.
 */

char*
ckstrdupnn (s)
    char *s;
{
    char *p, *t, *r;
    int n = 2;
    
    for (p = s; *p != 0; p++) {
	if (*p == '\n') n++;
    }

    n += (p - s);
    r = ckalloc (n);

    for (t = r, p = s; *p != 0; p++, t++) {
	if (*p == '\n') {
	    *t++ = '\\'; 
	    *t = 'n';
	} else {
	    *t = *p;
	}
    }
    *t = 0;
    
    return r;
}
