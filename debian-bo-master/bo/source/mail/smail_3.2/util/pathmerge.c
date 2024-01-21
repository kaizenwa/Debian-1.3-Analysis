/* @(#) pathmerge.c,v 1.4 1992/09/06 01:10:35 tron Exp */
/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * pathmerge - PATH-FILE MERGING PROGRAM
 *
 *	This program takes a set of sorted path files, as produced
 *	by pathalias, and generates on the standard output a merge
 *	of the path information, with one path given for each
 *	hostname.  Precedence in preferred paths goes to the files
 *	given earliest in the argument list.  One of the filenames
 *	given in the argument list can be `-' for the standard
 *	input.
 */
#include <stdio.h>
#include <ctype.h>
#include "defs.h"

extern int errno;		/* last system error */
char *malloc();			/* our storage allocator */
char *program;			/* who we are */

#define MAXLINE	(4096+1+1)	/* max line length + newline + NULL */

#define TRUE 1
#define FALSE 0

#define lower(c) ((char)(lowercase(toascii((int)(c)))))	/* safe tolower */

/*
 * file state
 */
enum state {
	eof,			/* the file has reached EOF */
	readme,			/* the file needs to be read */
	ready			/* the file has a line ready */
};

main(argc,argv)
	register int argc;
	char *argv[];
{
	char **line;		/* current input line, per file */
	enum state *status;	/* end, readme or ready state, per-file */
	FILE **fd;		/* per-file file descriptors */
	register int i;		/* index */
	int eofs;		/* number of files at EOF */
	char *previous;		/* the previous line written */
	int next;		/* the next line to be written */

	/*
	 * arg check
	 */
	if (argc <= 1) {
		/* no arguments, do nothing */
		exit(0);
	} else {
		program = argv[0]; /* remember who we are */
		++argv;		/* ignore argv[0] */
		--argc;
	}

	/*
	 * malloc our arrays
	 */
	/* input buffers */
	line  = (char **)malloc((argc+1) * sizeof(char **));
	if (line == NULL) {
		perror(program);
		exit(errno);
	}
	for (i=0; i < argc+1; ++i) { /* line[argc] is also previous */
		line[i] = (char *)malloc((MAXLINE+1) * sizeof(char));
		if (line[i] == NULL) {
			perror(program);
			exit(errno);
		}
		/* note line[i][MAXLINE-1] is initially '\0' */
	}
	previous = line[argc];	/* note previous is an empty string */
	/* file states */
	status  = (enum state *)malloc(argc * sizeof(enum state *));
	if (status == NULL) {
		perror(program);
		exit(errno);
	}
	/* per-file file descriptors */
	fd = (FILE **)malloc(argc * sizeof(FILE **));
	if (fd == NULL) {
		perror(program);
		exit(errno);
	}

	/*
	 * open our files, assign buffers and eof status
	 */
	for (i = 0; i < argc; i++) {
		/* open the file */
		if (strcmp(argv[i], "-") == 0) {
			fd[i] = stdin; /* use standard input */
		} else 	if ((fd[i] = fopen(argv[i], "r")) == NULL) {
			fprintf(stderr, "%s: ", program);
			perror(argv[i]);
			exit(errno);
		}
		/* enable the read on this file */
		status[i] = readme;
	}

	/*
	 * if only one file, pass it thru and exit
	 */
	if (argc == 1) {
		/* only one file to merge */
		while ((i = getc(fd[0])) != EOF) {
			putchar(i);
		}
		exit(0);
	}

	/*
	 * sort the files
	 */
	eofs = 0;		/* no files at EOF */
	for (;;) {
		/*
		 * read lines on files that need reading
		 */
		for (i = 0; i < argc; i++) {
			/* only look at files that need to be read */
			if (status[i] != readme) {
				continue;
			}
			/* read a line from the file */
			eofs = readline(argv, fd, line, status, eofs, i);
		}
		/* if all files then done, stop */
		if (eofs >= argc) {
			break;
		}

		/*
		 * find the alphabetically least line
		 */
		for (next = -1, i = 0; i < argc; i++) {
			/*
			 * only look at lines that are ready
			 * if current line is earler than before, note it
			 */
			if (status[i] == ready &&
			    ((next < 0) || cmp(line[i], line[next]) < 0)) {
				next = i; /* our newest early line */
			}
		}

		/*
		 * if we found a next line, print it
		 */
		if (next >= 0) {
			/* write the line if it is different */
			if (cmp(previous, line[next]) != 0) {
				/* write out that line */
				fputs(line[next], stdout);
				/* save it */
				strncpy(previous, line[next], MAXLINE);
			}
			status[next] = readme; /* ready to read again */
			/* skip lines that are equal to the selected one */
			for (i=next+1; i < argc; ++i) {
				if (status[i] == ready &&
				    cmp(line[i], previous) == 0) {
					status[i] = readme;
				}
			}
		}
	}

	/*
	 * all done
	 */
	exit(0);
}

/*
 * readline - read a line from a file and check for EOFs and long lines
 *
 * returns the new EOFs count
 */
int
readline(name, fd, line, status, eofs, which)
	char *name[];		/* the names of the files */
	FILE **fd;		/* per-file file descriptors */
	char **line;		/* current input line, per file */
	enum state *status;	/* end, readme or ready state, per-file */
	int eofs;		/* number of files on EOF state */
	int which;		/* which file is being read */
{
	int i;			/* char buffer */

	/*
	 * read the line
	 *
	 * If the line is MAXLINE-2 chars + newline, then line[MAXLINE-1]
	 * will be '\0'.  Any line that is longer will alter line[MAXLINE-1].
	 */
	if (fgets(line[which], MAXLINE+1, fd[which]) != NULL) {
		/*
		 * we got a line, is it a good one?
		 */
		if (line[which][MAXLINE-1] != '\0') {
			/* the line was too long, skip it */
			fprintf(stderr, "%s: %s line too long\n",
				program, name[which]);
			while ((i = getc(fd[which])) != '\n') {
				/* watch for EOF */
				if (i == EOF) {
					status[which] = eof;
					return(eofs+1); /* nothing left */
				}
			}
			/* clear for next read */
			line[which][MAXLINE-1] = '\0';
			status[which] = readme; /* try again later */
		} else {
			/* we have a line to use */
			status[which] = ready;
		}
	} else {
		/* we read the end of the file */
		status[which] = eof;
		++eofs;
	}
	return(eofs);		/* return EOF count */
}
	
/*
 * cmp - compare two pathalias lines
 *
 * compare two strings up to the first tab (or space) character
 * to determine if the first is less than, equal to,
 * or greater than the second, returning -1, 0 and 1
 * respectively.
 */
int
cmp(s, t)
	register char *s;	/* first line */
	register char *t;	/* second line */
{
	/*
	 * look for an irregularity
	 */
	while (*s == *t && !isspace(*s) && *s != '\0' && *s != ':') {
		++s;
		++t;
	}
	/* we have stopped at the end of the string, or on the tab */

	/*
	 * determine the relationship between the lines
	 */
	/* if we matched all the way, then note a line match */
	if ((isspace(*s) || *s == ':' || *s == '\0') &&
	    (isspace(*t) || *t == ':' || *t == '\0'))
	{
	    return (0);
	/* if the first line ended early, it is before the second */
	} else if (isspace(*s) || *s == '\0' || *s == ':') {
		return (-1);
	/* if the second line ended early, it is before the first */
	} else if (isspace(*t) || *t == '\0' || *t == ':') {
		return (1);
	/* we hit a difference spot, note which is earler */
	} else {
		return ((lower(*s) < lower(*t)) ? -1 : 1);
	}
}
