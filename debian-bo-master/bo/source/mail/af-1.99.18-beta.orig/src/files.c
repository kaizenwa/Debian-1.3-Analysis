/* Files.c - File handling for af.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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


#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "complete.h"
#include "io.h"
#include "misc.h"
#include STRING_HDR
#include DIR_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: files.c,v 1.48 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup(), *vstrcat();
extern char *getenv(), *strerror(), *strudate(), *get_user();
extern char *get_home(), *get_pwd(), *get_mailbox();
extern char *get_vtext(), *get_line(), *utos();
extern int readlink(), strncasecmp(), get_vval(), listed();
extern int is_header(), is_blank(), close_folder();
extern void free(), msgl(), emsgl(), cmsg(), abort_folder();
extern FILE *open_folder();
extern DATEZONE *date_now();
extern CLIST *add_clist();

#ifdef READ_VIA_POP3
extern char *fullfolder();
extern int write_pop3();
extern long size_pop3();
#endif /* READ_VIA_POP3 */

/* Local function declarations */

char *expand();
int mmdf_form(), nonempty();
int write_text();
long filesize();
static char *dir_tail();
static int afstatus(), aftags();
static int at_digest_end();
static MSG_TEXT *next_digest_entry();

#ifdef MTA_CONTENT_LENGTH
static int conlength();
#endif /* MTA_CONTENT_LENGTH */

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* The next line of a mail digest to be parsed */

static MSG_TEXT *digest_text = NULL;

/****************************************************************************/
int write_messages(list, folder, eofpos)
MESSAGE *list;
char *folder;
long eofpos;
{
	/*
	 * Write the messages in the buffer into folder, skipping
	 * any messages marked as deleted.  Returns WM_OK on success,
	 * WM_FAILED on failure, or WM_RESYNC if the file has grown.
	 */

	int mmdf, update, status;
	unsigned msg_no = 0;
	MESSAGE *m;
	FILE *fp;

#ifdef READ_VIA_POP3
	/* If this is a pop folder just call the required routine */

	if (POP3_MBOX(folder)) {
		return(write_pop3(list, folder, eofpos));
	}
#endif /* READ_VIA_POP3 */

	/* Let the user know what we're doing */

	if (list != NULL) {
		msgl("Writing ", folder, "...", NULL);
	}

	/* Check the size of the file */

	if (eofpos >= 0 && filesize(folder) > eofpos) {
		return(WM_RESYNC);
	}

	/* Check if the file is in MMDF format */

	mmdf = mmdf_form(folder);

	/* Open the file for writing */

	if ((fp = open_folder(folder, TRUE, eofpos)) == NULL) {
		return(WM_FAILED);
	}

	/* How often should we update the message count? */

	update = get_vval(V_MSG_UPDATE);

	/* Write the messages in the buffer to the folder */

	for (m = list; m != NULL && m->text != NULL; m = m->next) {
		/* Skip messages marked as deleted */

		if (m->deleted) {
			continue;
		}

		/* Update the message count and any user message */

		msg_no++;
		if (update > 0 && (msg_no == 1 || (msg_no % update) == 0)) {
			msgl("Writing ", folder, "; message ",
			     utos(msg_no), "...", NULL);
		}

		/* Write the message */

		if (status = write_text(fp, m, NULL, NULL, HS_MBOX, mmdf)) {
			emsgl("Error writing ", folder, ": ",
			      strerror(status), NULL);
			abort_folder(folder, fp);
			return(WM_FAILED);
		}
	}

	/* Close the file and check status */

	if (close_folder(folder, fp)) {
		return(WM_FAILED);
	}

	/* Confirm the write and return success */

	if (list != NULL) {
		msgl("(Wrote ", utos(msg_no), " message",
		     (msg_no == 1) ? ")" : "s)", NULL);
	}
	return(WM_OK);
}
/****************************************************************************/
int write_text(fp, message, prefix, hdrlist, skip_hdrs, mmdf)
FILE *fp;
MESSAGE *message;
char *prefix, *hdrlist;
int skip_hdrs, mmdf;
{
	/*
	 * Write the text of the message to the file indicated
	 * by fp with the specified prefix.  Skip header lines
	 * as given by skip_hdrs.
	 */

	int status = 0;
	int found = FALSE;
	MSG_TEXT *t;

	/* Write any header required to the file */

	if (mmdf && skip_hdrs == HS_MBOX && fputs(MMDF_DELIM, fp) == EOF) {
		return(errno);
	}

	/* Write the headers, skipping as required */

	for (t = message->text; t != NULL &&
	     !is_blank(t->line); t = t->next) {
		/* Write this header if we need to */

		if (skip_hdrs != HS_ALL &&
		    (skip_hdrs != HS_FROM ||
		     strncmp(t->line, MFROM, strlen(MFROM))) &&
		    (skip_hdrs != HS_COPY || listed(t->line, hdrlist)) &&
		    (skip_hdrs != HS_SHOW || !listed(t->line, hdrlist)) &&
#ifdef MTA_CONTENT_LENGTH
		    strncasecmp(t->line, CONTENT_LENGTH,
				strlen(CONTENT_LENGTH)) &&
#endif */ MTA_CONTENT_LENGTH */
    		    strncasecmp(t->line, AFSTATUS, strlen(AFSTATUS)) &&
		    strncasecmp(t->line, AFTAGS, strlen(AFTAGS))) {
			/* Write the header and set found */

			if (prefix != NULL && fputs(prefix, fp) == EOF
			    || fputs(t->line, fp) == EOF) {
				/* Error writing the header */

				return(errno);
			}
			found = TRUE;
		}
	}

	/* If we're writing all headers then add status and tags */

	if (skip_hdrs == HS_MBOX || skip_hdrs == HS_NONE) {
#ifdef MTA_CONTENT_LENGTH
		/* We also need to write the Content-Length header */

		if (status = conlength(fp, message->length)) {
			return(status);
		}
#endif /* MTA_CONTENT_LENGTH */

		if (status = afstatus(fp, message)) {
			return(status);
		}
		if (status = aftags(fp, message)) {
			return(status);
		}
	}

	/* Skip the blank line if no headers found */

	t = (!found && t != NULL) ? t->next : t;

	/* Now copy the message body */

	while (t != NULL && t->line != NULL) {
#ifndef MTA_CONTENT_LENGTH
	/* Quote the line if it begins with a message delimiter */

	if (skip_hdrs == HS_MBOX && prefix == NULL &&
	    (mmdf && !strncmp(t->line, MMDF_DELIM, strlen(MMDF_DELIM))
	     || !mmdf && !strncmp(t->line, MFROM, strlen(MFROM)))
	    && fputs(DELIM_PFX, fp) == EOF) {
		/* Error writing the prefix */

		return(errno);
	}
#endif /* ! MTA_CONTENT_LENGTH */

		/* Copy each line with any prefix */

		if (prefix != NULL && fputs(prefix, fp) == EOF
		    || fputs(t->line, fp) == EOF) {
			/* Error writing text */

			return(errno);
		}
		t = t->next;
	}

	/* Append any footer required to the text */

	if (mmdf && skip_hdrs == HS_MBOX) {
		status = (fputs(MMDF_DELIM, fp) == EOF) ? errno : 0;
	} else if (skip_hdrs != HS_COPY) {
		status = (putc('\n', fp) == EOF) ? errno : 0;
	}

	/* And return status */

	return(status);
}
/****************************************************************************/
#ifdef MTA_CONTENT_LENGTH
static int conlength(fp, length)
FILE *fp;
unsigned length;
{
	/* Write a content-length header for message to fp */

	return((fputs(CONTENT_LENGTH, fp) == EOF
		|| putc(' ', fp) == EOF
		|| fputs(utos(length), fp) == EOF
		|| putc('\n', fp) == EOF) ? errno : 0);
}
#endif /* MTA_CONTENT_LENGTH */
/****************************************************************************/
static int afstatus(fp, message)
FILE *fp;
MESSAGE *message;
{
	/* Write a status header for message to fp */

	if (fputs(AFSTATUS, fp) == EOF) {
		return(errno);
	}
	if (putc(' ', fp) == EOF) {
		return(errno);
	}
	if (putc(ST_OLD, fp) == EOF) {
		return(errno);
	}
	if (message->read && (putc(ST_READ, fp) == EOF)) {
		return(errno);
	}
	if (message->saved && (putc(ST_SAVED, fp) == EOF)) {
		return(errno);
	}
	if (message->printed && (putc(ST_PRINTED, fp) == EOF)) {
		return(errno);
	}
	if (message->replied && (putc(ST_REPLIED, fp) == EOF)) {
		return(errno);
	}
	if (message->forwarded && (putc(ST_FORWARDED, fp) == EOF)) {
		return(errno);
	}
	if (putc('\n', fp) == EOF) {
		return(errno);
	}

	return(0);
}
/****************************************************************************/
static int aftags(fp, message)
FILE *fp;
MESSAGE *message;
{
	/* Write a tags header for message to fp */	

	if (message->user_tags != NULL) {
		if (fputs(AFTAGS, fp) == EOF) {
			return(errno);
		}
		if (putc(' ', fp) == EOF) {
			return(errno);
		}
		if (fputs(message->user_tags, fp) == EOF) {
			return(errno);
		}
		if (putc('\n', fp) == EOF) {
			return(errno);
		}
	}

	return(0);
}
/****************************************************************************/
int open_digest(message)
MESSAGE *message;
{
	/* Open message as a mail digest to be parsed for entries */

	/* Initialise the current digest line */

	digest_text = message->text;

	/* Skip the digest's preamble */

	while (digest_text != NULL &&
	       next_digest_entry(digest_text) == NULL) {
		digest_text = digest_text->next;
	}

	/* Return whether this is a valid digest */

	return(digest_text != NULL);
}
/****************************************************************************/
/*ARGSUSED*/
char *get_digest(fp, fold)
FILE *fp;
int fold;
{
	/*
	 * Process the lines of the message pointed to by the
	 * static variable digest_text, expanding digest entries
	 * into individual messages and returning each line of
	 * the message as an allocated string, one line per call.
	 * If fold is true then process folded lines (newlines
	 * followed by white space are not line separators).
	 */

	char *line;
	MSG_TEXT *next_entry;

	/* Have we reached the end of an entry in the digest? */

	if ((next_entry = next_digest_entry(digest_text)) != NULL) {
		/* Move to the start of the next entry */

		digest_text = next_entry;

		/* And return a delimiter */

		return(vstrcat(MFROM, get_user(), "@", get_vtext(V_DOMAIN),
			       " ", strudate(date_now()), "\n", NULL));
	}

	/* Or the end of the digest itself? */

	if (digest_text == NULL || at_digest_end(digest_text)) {
		return(NULL);
	}

	/* Copy the current line of the digest */

	line = xstrdup(digest_text->line);
	digest_text = digest_text->next;

	/* Handle folds in the current line */

	while (fold && is_header(line) && digest_text != NULL
	       && (digest_text->line[0] == ' ' ||
		   digest_text->line[0] == '\t')) {
		/* Add the next line to the current one */

		line = xrealloc(line, strlen(line) +
				strlen(digest_text->line));
		(void) strcat(line, digest_text->line);

		/* And check if further lines are folded */

		digest_text = digest_text->next;
	}
			
	/* Return the expanded line of the digest */

	return(line);
}
/****************************************************************************/
static MSG_TEXT *next_digest_entry(text)
MSG_TEXT *text;
{
	/*
	 * If text points to the end of an entry in a mail
	 * digest then return the first line in the next
	 * entry, otherwise return NULL.
	 */

	MSG_TEXT *t;

	/* The current line must be a digest delimiter */

	if (text == NULL || strncasecmp(text->line, DIGEST_DELIM,
					strlen(DIGEST_DELIM))) {
		return(NULL);
	}
	t = text->next;

	/* Skip any trailing blank lines */

	while (t != NULL && is_blank(t->line)) {
		t = t->next;
	}

	/* If we now have a header, return it */

	return((t != NULL && is_header(t->line)) ? t : NULL);
}
/****************************************************************************/
static int at_digest_end(text)
MSG_TEXT *text;
{
	/* Return TRUE if text points to the end of a digest */

	MSG_TEXT *t;

	/* The current line must be a digest delimiter */

	if (strncasecmp(text->line, DIGEST_DELIM, strlen(DIGEST_DELIM))) {
		return(FALSE);
	}

	/* Check for more digest entries before end-of-message */

	for (t = text->next; t != NULL; t = t->next) {
		if (next_digest_entry(t) != NULL) {
			return(FALSE);
		}
	}

	/* We have reached the end of the digest */

	return(TRUE);
}
/****************************************************************************/
long filesize(folder)
char *folder;
{
	/* Return the last seek position of the file */

	struct stat buf;

#ifdef READ_VIA_POP3
	/* We need a different method for the size of a POP folder */

	if (POP3_MBOX(folder)) {
		return(size_pop3(folder));
	}
#endif /* READ_VIA_POP3 */

	/* Return the size of the file in bytes */

	return((stat(folder, &buf) < 0) ? 0L : (long) buf.st_size);
}
/****************************************************************************/
int nonempty(folder)
char *folder;
{
	/* Report whether the mailfile is empty */

	struct stat sbuf;

#ifdef READ_VIA_POP3
	/* We need a different method for POP folders */

	if (POP3_MBOX(folder)) {
		return(size_pop3(folder) > 0);
	}
#endif /* READ_VIA_POP3 */

	/* There is mail if we can get a nonzero size */

	return(stat(folder, &sbuf) >= 0 && sbuf.st_size > 0);
}
/****************************************************************************/
int mmdf_form(folder)
char *folder;
{
	/* Return TRUE if a folder is in MMDF format */

	char *line;
	int mmdf;
	FILE *fp;

	/* If the file is non-empty then check it */

	if (nonempty(folder) && (fp = fopen(folder, "r")) != NULL) {
		/* Opened the file, try to get a line */

		if ((line = get_line(fp, FALSE)) != NULL) {
			/* Check the file type and return status */
#ifdef MTA_MMDF_FORMAT
			mmdf = (strncasecmp(line, MFROM, strlen(MFROM)));
#else /* ! MTA_MMDF_FORMAT */
			mmdf = (!strncasecmp(line, MMDF_DELIM,
					     strlen(MMDF_DELIM)));
#endif /* ! MTA_MMDF_FORMAT */

			/* Clean up and return status */

			(void) fclose(fp);
			free(line);
			return(mmdf);
		}

		/* Close the file */

		(void) fclose(fp);
	}

	/* The format should be the system default */

#ifdef MTA_MMDF_FORMAT
	return(TRUE);
#else /* ! MTA_MMDF_FORMAT */
	return(FALSE);
#endif /* ! MTA_MMDF_FORMAT */
}
/****************************************************************************/
char *expand(filnam)
char *filnam;
{
	/* Return an allocated string containing filnam in canonical form */

	char *buf, *prefix = NULL, *suffix = NULL, *slash;

	/* Check if the file name starts with an escape */

	switch (*filnam) {
	case '\\':					/* Quoting */
		filnam = filnam + 1;
		break;
	case '+':					/* Folder directory */
		if ((prefix = get_vtext(V_FOLDER)) == NULL) {
			prefix = get_home(NULL);
		}
		suffix = filnam + 1;
		break;
	case '=':					/* News directory */
		if ((prefix = get_vtext(V_NEWSFOLDER)) == NULL) {
			prefix = get_home(NULL);
		}
		suffix = filnam + 1;
		break;
	case'%':					/* Incoming mailbox */
		if (*(filnam + 1) == '\0') {
			filnam = get_mailbox(NULL);
		} else if ((suffix = get_mailbox(filnam + 1)) != NULL) {
			filnam = suffix;
		}
		break;
	case '~':					/* Home directory */
		if (*(filnam + 1) == DIRSEP) {
			prefix = get_home(NULL);
			suffix = filnam + 2;
		} else if (*(filnam + 1) == '\0') {
			prefix = get_home(NULL);
			suffix = filnam + 1;
		} else if ((slash = strchr(filnam, DIRSEP)) != NULL) {
			*slash = '\0';
			if ((prefix = get_home(filnam + 1)) != NULL) {
				suffix = slash + 1;
			}
			*slash = DIRSEP;
		} else if ((prefix = get_home(filnam + 1)) != NULL) {
			suffix = filnam + strlen(filnam);
		}
		break;
	}

	/* Copy the path, with any prefix */

	if (prefix != NULL) {
		buf = vstrcat(prefix, "/", suffix, NULL);
	} else {
		buf = xstrdup(filnam);
	}

	return(buf);
}
/****************************************************************************/
char *fullpath(filnam)
char *filnam;
{
	/*
	 * Return an allocated string containing the absolute
	 * path of filnam.  If filnam can't be resolved, due
	 * to errors in symbolic links, then we return the
	 * relative path for fopen() to fail on.
	 */

	char *path, *slash, *next;
	char *dirs, *dir, *newpath;

#ifdef HAVE_READLINK
	char link[MAXPATHLEN + 1];
	char *newdirs;
	int len, no_links = 0;
#endif /* HAVE_READLINK */

#ifdef READ_VIA_POP3
	/* For POP folders, return the canonical folder name */

	if (POP3_MBOX(filnam)) {
		return(fullfolder(filnam));
	}
#endif /* READ_VIA_POP3 */

	/* First expand the file name */

	filnam = expand(filnam);

	/* Set up the checked and unchecked paths */

	path = xstrdup((*filnam != '/') ? get_pwd(FALSE) : "/");
	dir = dirs = xstrdup((*filnam != '/') ? filnam : filnam + 1);

	/* Now check the unchecked section of the path */

	while (dir != NULL && *dir != '\0') {
		/* Find the next directory in dirs */

		if ((slash = strchr(dir, '/')) != NULL) {
			*slash = '\0';
		}

		/* Find the directory to check next time */

		next = (slash != NULL) ? slash + 1 : NULL;

		/* Now handle the directory name as required */

		if (!strcmp(dir, "..")) {
			/* Is there a directory name in path? */

			if ((slash = strrchr(path, '/')) != NULL) {
				/* Strip the last directory in path */

				slash = (slash == path) ? slash + 1 : slash;
				*slash = '\0';
				path = xrealloc(path, slash - path + 1);
			}
		} else if (*dir != '\0' && strcmp(dir, ".")) {
			/* Add the directory to the path */

			newpath = vstrcat(path, (strlen(path) != 1)
					  ? "/" : "", dir, NULL);
			free(path);
			path = newpath;

#ifdef HAVE_READLINK
			/* Check whether the file is a link */

			if ((len = readlink(path, link, MAXPATHLEN)) > 0) {
				/* Make sure the link is null-terminated */

				link[len] = '\0';

				/* Have we reached the maximum no of links? */

				if (++no_links > MAXSYMLINKS) {
					/* Failed; return the relative path */

					free(path);
					free(dirs);
#ifdef READ_VIA_POP3					
					/* Check for fake POP3 folders */

					if (POP3_MBOX(filnam)) {
						path = vstrcat("./", filnam,
							       NULL);
						free(filnam);
						return(path);
					}
#endif /* READ_VIA_POP3 */
					return(filnam);
				}

				/* Remove the obsoleted section of path */

				slash = (link[0] != '/') ?
					strrchr(path, '/') : path;
				*slash = '\0';
				path = xrealloc(path, slash - path + 1);

				/* And add the link expansion to dirs */

				newdirs = vstrcat(link, "/", next, NULL);
				free(dirs);
				next = dirs = newdirs;
			}
#endif /* HAVE_READLINK */
		}

		/* Now check the next directory */

		dir = next;
	}

	/* Free space and return the path */

	free(dirs);
	free(filnam);
	return(path);
}
/****************************************************************************/
char *pathfind(filnam, path, suffix)
char *filnam, *path, *suffix;
{
	/*
	 * Search the path for the specified file, or the file
	 * with the given suffix appended.  Return the full
	 * pathname in an allocated string if found, or NULL
	 * if not found.  Full pathnames are acceptable, but
	 * relative ones aren't.
	 */

	char *dir, *end, *fullnam;
	int len, error = ENOENT;

	/* Expand prefixes in the file name */

	filnam = expand(filnam);

	/* Do we have an absolute pathname? */

	if (*filnam == '/') {
		/* Return the path */

		return(filnam);
	}

	/* Search the load path for the library */

	dir = path;
	while (dir != NULL && *dir != '\0') {
		/* Find the end of the directory */

		end = strchr(dir + 1, ':');

		/* How long is the current directory? */

		if ((len = (end != NULL) ? end - dir : strlen(dir)) > 0) {
			/* Build the full name for this directory */

			fullnam = xmalloc(len + strlen(filnam) + 2);
			(void) strncpy(fullnam, dir, len);
			(void) strcpy(fullnam + len, "/");
			(void) strcat(fullnam, filnam);

			/* Now check if the file exists */

			if (!access(fullnam, 04)) {
				/* We've found the file */

				free(filnam);
				return(fullnam);
			}

			/* Update the error number */

			error = (errno != ENOENT) ? errno : error;

			/* Try the file with the suffix */

			if (suffix != NULL) {
				/* Add the suffix to the full name */

				fullnam = xrealloc(fullnam, strlen(fullnam)
						   + strlen(suffix) + 1);
				(void) strcat(fullnam, suffix);

				/* Check if this file exists */

				if (!access(fullnam, 04)) {
					/* We've found the file */

					free(filnam);
					return(fullnam);
				}

				/* Update the error number again */

				error = (errno != ENOENT) ? errno : error;
			}

			/* We don't need this full name any more */

			free(fullnam);
		}

		/* Update the loop counter */

		dir = (end != NULL) ? end + 1 : NULL;
	}

	/* We didn't find the file in the path */

	errno = error;
	free(filnam);
	return(NULL);
}
/****************************************************************************/
CLIST *fn_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of files completing base */

	char *buf, *dname, *fname;
	char *slash, *prefix;
	DIR *dirp;
	struct dirent *entry;

	/* Expand prefixes in the file name */

	dname = expand(base);

	/* Get the directory name */

	if ((slash = strrchr(dname, DIRSEP)) != NULL) {
		/* Extract the filename from the path */

		fname = xstrdup(slash + 1);
		slash = (slash == dname) ? slash + 1 : slash;
		*slash = '\0';

		/* Get the prefix for the file names */

		if ((slash = strrchr(base, DIRSEP)) != NULL) {
			slash++;
		} else if (*base == '+' || *base == '=' ||
			   *base == '~' || *base == '%') {
			slash = base + 1;
		} else {
			slash = base + strlen(base);
		}
		prefix = xmalloc(slash - base + 1);
		(void) strncpy(prefix, base, slash - base);
		prefix[slash - base] = '\0';
	} else {
		/* The file's in the current directory */

		fname = dname;
		prefix = xstrdup("");
		dname = xstrdup(".");
	}

	/* Open the directory */

	if ((dirp = opendir(dname)) == NULL) {
		free(prefix);
		free(fname);
		free(dname);
		return(list);
	}

	/* If the file name is null, add the directory */

	if (*fname == '\0' && *prefix != '\0') {
		list = add_clist(list, prefix, TRUE);
	}

	/* Add any other matching entries to the list */

	while ((entry = readdir(dirp)) != NULL) {
		/* Check if this entry matches the prefix */

		if (!strncmp(fname, entry->d_name, strlen(fname)) &&
			(*fname != '\0' || strcmp(entry->d_name, "..")
			 && strcmp(entry->d_name, "."))) {
			/* Build the full path, with slash if directory */

			buf = vstrcat(prefix, entry->d_name,
				      dir_tail(dname, entry->d_name), NULL);

			/* Add the entry to the list and free it */

			list = add_clist(list, buf, TRUE);
			free(buf);
		}
	}

	/* Close the directory and free space */

	(void) closedir(dirp);
	free(prefix);
	free(dname);
	free(fname);

	return(list);
}
/****************************************************************************/
static char *dir_tail(dirnam, filnam)
char *dirnam, *filnam;
{
	/* Return the directory tail for dirnam/filnam */

	static char is_tail[] = "/";
	static char not_tail[] = "";

	char *pathnam;
	struct stat buf;

	/* Form the path and stat it */

	pathnam = vstrcat(dirnam, "/", filnam, NULL);
	if (stat(pathnam, &buf) < 0) {
		free(pathnam);
		return(not_tail);
	}
	free(pathnam);

	/* Return the appropriate tail */

	return(((buf.st_mode & S_IFMT) == S_IFDIR) ? is_tail : not_tail);
}
/****************************************************************************/
