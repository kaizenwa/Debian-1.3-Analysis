/* @(#) unsharmap.c,v 1.9 1992/07/11 11:40:32 tron Exp */
/*
 *    Copyright (C) 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992 Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * unsharmap - unshar a USENET comp.news.maps article
 *
 * usage:  unsharmap [-d dir] < filelist > log
 *
 *	   -d dir	- cd to 'dir' before unsharing
 *	   -s spooldir	- news spool directory
 *	   -n newsgroups - colon separated list of allowed newsgroups
 *
 * where "filelist" contains a list of files that are in the format below.
 *
 * The USENET map project distributes files in the following format:
 *
 * The header, which starts at the firts line and continues up until
 * there is a blank line as the follow lins it:
 *
 *    Newsgroups: comp.mail.maps
 *    Subject: UUCP ...
 *    Message-ID: ...
 *    Date: ...
 *    Approved: ...
 *
 * These lines are not the complete list, nor do they show up in that order.
 * After the header is a blank line.  Next comes the shar file preamble
 * which consists of a bunch of lines that begin with a :, followed
 * by the 2 lines:
 *
 *    echo shar: extracting THE-FILE-NAME
 *    cat << 'SHAR_EOF' > THE-FILE-NAME
 *
 * what follows is the map data intended to override THE-FILE-NAME in the
 * UNSHAR_MAP_DIR directory.  The final 3 lines:
 *
 *    SHAR_EOF
 *    :   End of shell archive
 *    exit 0
 *
 * terminate the shar file, and are not considered a part of the map data.
 *
 * For each shar file processed, the following information is written
 * to stdout:
 *
 *	filename read
 *	filename written
 *	subject line
 *	message-ID
 *	date
 *	approved info
 *	any error message
 *
 * All error message lines begin with 'error:'.
 *
 * exits by 0 if there was no errors, non-0 otherwise.
 */
 
#include <stdio.h>

#define HEADER_OK 1			/* the article header is correct */
#define HEADER_ERROR 2			/* bad header, skip and log */
#define HEADER_SKIP 3			/* not a map shar, ignore file */

char *program;				/* our name */
char err[BUFSIZ+1];			/* error and log message */
char buf[BUFSIZ+1];			/* input buffer */
char mapname[BUFSIZ+1];			/* name of the output map file */
int lineno = 0;				/* current line number */
char *newsgroups = "comp.mail.maps";	/* : list of allowed newsgroups */
char *newsspool = NULL;			/* news article spool directory */
char *mapdir = NULL;			/* destination dir for maps */

char *get_date();			/* system dependent date string */
void logit();				/* write to log and error log */
int check_header();			/* check the article header */
void skip_article();			/* skip a bad article */
void ignore_article();			/* ignore the non-map article */
char *check_preamble();			/* check shar preamble, get map name */
int write_map();			/* write a map file */
char *check_newline();			/* check to be sure we read a '\n' */

char *xmalloc();			/* for string.c and strcolon()*/
char *xrealloc();			/* for string.c and strcolon() */

extern char *strcolon();		/* grap strcolon() from src/string.c */

main( argc, argv )
    int argc;				/* arg count */
    char *argv[];			/* args */
{
    char filename[BUFSIZ+1];		/* name of the input article */
    char *pathname;			/* file pathname */
    char *p;				/* pointer */
    int c;				/* the option flag */
    int header;				/* HEADER_OK,ERROR,SKIP */
    int errors=0;			/* number of problems found */
    FILE *article;			/* input article stream */
    extern char *optarg;		/* the option argument */
    extern int optind;			/* operand index */

    /*
     * parse args
     */
    program = argv[0];
    while ((c = getopt(argc, argv, "d:n:s:")) != EOF) {
	switch(c) {
	case 'd':			/* directory to cd to */
	    mapdir = optarg;
	    break;

	case 's':
	    newsspool = optarg;
	    break;

	case 'n':
	    newsgroups = optarg;
	    break;

	case '?':
	    sprintf(err,
		    "error: usage: %s [-d dir] < filelist > log 2> error\n",
		    program);
	    logit(err);
	    sprintf(err, "exiting: status: 1 -  %s", get_date());
	    logit(err);
	    exit(1);
	    break;
	}
    }
    if (optind != argc) {
	sprintf(err, "error: usage: %s [-d dir] < filelist > log 2> error\n",
		program);
	logit(err);
	sprintf(err, "exiting: status: 1 -  %s", get_date());
	logit(err);
	exit(1);
    }

    /*
     * process all file names on input
     */
    sprintf(err, "starting: %s", get_date());
    logit(err);
    while (fgets(filename, BUFSIZ, stdin) != NULL) {

	if ((p = check_newline(filename)) == NULL) {
	    sprintf(err, "error: filename longer than %d chars\n", BUFSIZ-2);
	    logit(err);
	    sprintf(err, "exiting: status: 2 -  %s", get_date());
	    logit(err);
	    exit(2);
	}
	*p = '\0';	/* remove the newline from the filename */

	/*
	 * open the map file
	 *
	 * if the filename does not begin with a /, then prepend the
	 * newsspool directory to it
	 */

	lineno = 0;			/* clear the line count */
	if (filename[0] == '/' || newsspool == NULL) {
	    pathname = xmalloc(strlen(filename) + 1);
	    strcpy(pathname, filename);
	} else {
	    pathname = xmalloc(strlen(filename) + strlen(newsspool) + 2);
	    sprintf(pathname, "%s/%s", newsspool, filename);
	}

	article = fopen(pathname, "r");
	if (article == NULL) {
	    sprintf(err, "error: cannot open article: %s\n", pathname);
	    logit(err);
	    free(pathname);
	    ++errors;
	    continue;
	}
	sprintf(err, "filename: %s\n", pathname);
	logit(err);
	free(pathname);

	/*
	 * verify the article header
	 *
	 * close file if error or skip
	 */
	switch (header = check_header(article)) {
	case HEADER_OK:			/* we have a good header */
	    break;
	case HEADER_ERROR:		/* the article is bad, skip & log */
	    skip_article(article, filename);
	    ++errors;
	    break;
	case HEADER_SKIP:		/* not a map article, ignore it */
	    ignore_article(article, filename);
	    break;
	default:			/* how did we get here? */
	    sprintf(err, "error: check_header returned %d, why?\n", header);
	    logit(err);
	    sprintf(err, "exiting: status: 3 -  %s", get_date());
	    logit(err);
	    exit(3);
	}
	if (header != HEADER_OK) {
	    continue;			/* try another file */
	}

	/*
	 * check the shar preamble, get the shar file name
	 */
	if (check_preamble(article) == NULL) {
	    skip_article(article, filename);
	    ++errors;
	    continue;
	}

	/*
	 * write the map, verify final lines
	 */
	if (write_map(article) != 0) {
	    skip_article(article, filename);
	    ++errors;
	    continue;
	}

	/*
	 * all done with this article
	 */
	fclose(article);
	fflush(stdout);
    }

    /*
     * note if everything went ok
     */
    sprintf(err, "exiting: status: %d -  %s", (errors>0) ? 4 : 0, get_date());
    logit(err);
    exit( (errors>0) ? 4 : 0 );
}


/*
 * get_date - get the date followed by newline in the standard way
 *
 * returns the date in ctime(3) format
 */
char *
get_date()
{
    extern long time();
    long clock = time((long *)0);   /* seconds since 1-jan-1970 0:00:00 GMT */
    char *ctime();		    /* convert clock into string */

    return(ctime(&clock));
}


/*
 * log - write a message to the log
 */
void
logit( msg )
    char *msg;				/* the message to write */
{
    /* write to the log */
    fputs(msg, stdout);
}


/*
 * check_header - check the article header
 *
 * This routine verifies that a article header has the following lines:
 *
 *    Newsgroups: comp.mail.maps
 *    Subject: UUCP ...
 *    Message-ID: ...
 *    Date: ...
 *    Approved: ...
 *
 * The text of all but the Newsgroups lines are logged.  If the Newsgroups
 * line is bad, that too is logged.  The header ends before a blank line.
 * The final blank line is read and discarded.
 *
 * returns HEADER_OK is all 5 header lines were read and were valid,
 *         HEADER_ERROR is the header was bad and needs to be skipped & logged,
 *	   HEADER_SKIP if the file is not a UUCP shar file and should be skipped
 */
int
check_header( article )
    FILE *article;			/* the article stream */
{
    int saw_groups=0;			/* 1 ==> saw Newsgroups: */
    int saw_subject=0;			/* 1 ==> saw Subject: */
    int saw_message=0;			/* 1 ==> saw Message-ID: */
    int saw_date=0;			/* 1 ==> saw Date: */
    int saw_approved=0;			/* 1 ==> saw Approved: */
    char *p;				/* temp */

    /*
     * read the header
     */
    do {
	/*
	 * read the line
	 */
	clearerr(article);
	if (fgets(buf, BUFSIZ, article) == NULL) {
	    if (ferror(article)) {
		sprintf(buf, "error: bad header read after line %d\n", lineno);
		logit(err);
	    } else {
		logit("error: EOF while reading the header\n");
	    }
	    return(HEADER_ERROR);
	}
	++lineno;			/* count this line */
	if (! check_newline(buf)) {
	    sprintf(err, "error: line %d, header line too long\n", lineno);
	    logit(err);
	    return(HEADER_ERROR);
	}

	/*
	 * look for special types
	 */
	switch (buf[0]) {
	case 'n':
	case 'N':			/* could be Newsgroups: */
	    if (strncmpic("Newsgroups: ", buf, 12) == 0) {
		for (p = strcolon(newsgroups); p; p = strcolon((char *)NULL)) {
		    if (strncmpic(buf + 12, p, strlen(p)) == 0 &&
			buf[12 + strlen(p)] == '\n')
		    {
			break;
		    }
		}
		if (p != NULL) {
		    saw_groups = 1;
		} else {
		    sprintf(err, "error: line %d, bad %s", lineno, buf);
		    logit(err);
		    return(HEADER_ERROR);
		}
	    }
	    break;
	case 's':
	case 'S':			/* Subject: buf */
	    if (strncmpic("Subject: ", buf, 9) == 0) {
		if (strncmpic("Subject: UUCP ", buf, 14) == 0) {
		    saw_subject = 1;
		    logit("    ");
		    logit(buf);
		} else {
		    logit("    ");
		    logit(err);
		    return(HEADER_SKIP); /* not a shar map file */
		}
	    }
	    break;
	case 'm':
	case 'M':			/* Message-ID: buf */
	    if (strncmpic("Message-ID: ", buf, 12) == 0) {
		    saw_message = 1;
		    logit("    ");
		    logit(buf);
	    }
	    break;
	case 'd':
	case 'D':			/* Message-ID: buf */
	    if (strncmpic("Date: ", buf, 6) == 0) {
		saw_date = 1;
		logit("    ");
		logit(buf);
	    }
	    break;
	case 'a':
	case 'A':			/* Message-ID: buf */
	    if (strncmpic("Approved: ", buf, 10) == 0) {
		saw_approved = 1;
		logit("    ");
		logit(buf);
	    }
	    break;
	}

    } while(strcmp("\n", buf) != 0);	/* while heading the header */

    /*
     * report if er got everything
     */
    if (saw_groups == 0) {
	logit("error: no Newsgroups: line\n");
	return(HEADER_ERROR);
    }
    if (saw_subject == 0) {
	logit("error: no Subject: line\n");
	return(HEADER_ERROR);
    }
    if (saw_message == 0) {
	logit("error: no Message-ID: line\n");
	return(HEADER_ERROR);
    }
    if (saw_date == 0) {
	logit("error: no Date: line\n");
	return(HEADER_ERROR);
    }
    if (saw_approved == 0) {
	logit("error: no Approved: line\n");
	return(HEADER_ERROR);
    }
    return(HEADER_OK);			/* passed all the tests */
}

/*
 * skip_article - skip article, log and close it
 */
void
skip_article( article, filename )
    FILE *article;			/* the article stream */
    char *filename;			/* the filename we are skipping */
{
    /*
     * log that we are skipping the remainder of the article
     */
    sprintf(err, "    skipping: %s after reading line %d\n", filename, lineno);
    logit(err);

    /*
     * close the stream
     */
    fclose(article);

    /*
     * report the close
     */
    sprintf(err, "finished: %s\n", filename);
    logit(err);
    return;
}


/*
 * ingore_article - ignore an article, close and report it
 */
void
ignore_article( article, filename )
    FILE *article;			/* the article stream */
    char *filename;			/* the filename we are skipping */
{
    /*
     * log that we are ignoring the remainder of the article
     */
    sprintf(err, "    ignore non map file: %s after reading line %d\n",
	    filename, lineno);
    logit(err);

    /*
     * close the stream
     */
    fclose(article);

    /*
     * report the close
     */
    sprintf(err, "finished: %s\n", filename);
    logit(err);
    return;
}


/*
 * check_preamble - check for a valid shar preamble
 *
 * The shar preamble consists of a bunch of lines that begin with :,
 * followed by the 2 lines:
 *
 *    echo shar: extracting THE-FILE-NAME
 *    cat << 'SHAR_EOF' > THE-FILE-NAME
 *
 * Any other line is printed to both logs as an error.  The "THE-FILE-NAME"
 * string is returned if a valid preamble is found, NULL otherwise.
 */
char *
check_preamble( article )
    FILE *article;			/* the article stream */
{
    char *p;				/* pointer */
    char *q;				/* pointer, NULL byte of mapname */
    int bad_echo=0;			/* 1 ==> a bad echo was found */

    /*
     * read the preamble
     */
    mapname[0] = '\0';	/* no mapname yet */
    while (1) {
	/*
	 * read the line
	 */
	clearerr(article);
	if (fgets(buf, BUFSIZ, article) == NULL) {
	    if (ferror(article)) {
		sprintf(err, "error: bad preamble read after line %d\n",
			lineno);
		logit(err);
	    } else {
		logit("error: EOF while reading the preamble\n");
	    }
	    return(NULL);
	}
	++lineno;			/* count line */
	if (! check_newline(buf)) {
	    sprintf(err, "error: line %d, preamble line too long\n", lineno);
	    logit(err);
	    return(NULL);
	}

	/*
	 * skip the : lines and any blank lines
	 */
	if (buf[0] == ':' || buf[0] == '\n') {
	    continue;
	}

	/*
	 * look for the echo line
	 */
	if (strncmp("echo shar: extracting ", buf, 22) == 0) {
	    /* grab the mapname */
	    for (p=buf+22, q=mapname, bad_echo=0; *p; ++p) {
		/* be sure it is well formed */
		switch (*p) {
		case ' ':
		case '\t':
		    bad_echo = 1;	/* unlikely char in a filename */
		    *q++ = *p;
		    break;
		case '\n':		/* should be the end */
		    if (*(p+1) != '\0') {
			bad_echo = 1;	/* why is '\n' not the end */
		    }
		    break;		/* don't copy the newline */
		case '\\':		/* avoid \ and / in filenames */
		case '/':
		    bad_echo = 1;
		    *q++ = *p;
		    break;
		default:
		    *q++ = *p;
		    break;
		}
	    }
	    *q = '\0';			/* NULL terminate filename */

	    /* verify mapname */
	    if (bad_echo == 1) {
		sprintf(err, "error: line %d, bad echo mapname: %s\n",
			lineno, mapname);
		logit(err);
		return(NULL);		/* bad preamble */
	    }

	/*
	 * watch for the cat line
	 */
	} else if (strncmp("cat << 'SHAR_EOF' > ", buf, 20) == 0) {

	    /*
	     * compare cat filename against echo filename
	     */
	    if (mapname[0] == '\0') {
		sprintf(err, "error: line %d, cat with no preceding echo: %s",
			lineno, buf);
		logit(err);
		return(NULL);		/* bad preamble */
	    } else if (strncmp(buf+20, mapname, q-mapname) != 0) {
		sprintf(err,
			"error: line %d, echo mapname: %s != cat mapname: %s",
			lineno, mapname, buf+20);
		logit(err);
		return(NULL);		/* bad preamble */
	    } else {
		return(mapname);	/* found end of preamble */
	    }
	    
	/*
	 * watch for unkown lines
	 */
	} else {
	    sprintf(err, "error: line %d, unknown preamble: %s", lineno, buf);
	    logit(err);
	    return(NULL);
	}
    }
    /* NOT REACHED */
}


/*
 * write_map - write a map file
 *
 * Given an verified article header and shar preamble, copy the contents
 * of the shar HERE_IS document up until the ending shar lines:
 *
 *    SHAR_EOF
 *    :   End of shell archive
 *    exit 0
 *
 * A previous call to check_preamble placed the name of the mapfile in
 * the mapname array.
 *
 * returns 1 if all was ok, 0 otherwise.
 */
int
write_map( article )
    FILE *article;			/* the article stream */
{
    int fd;				/* map file descriptor */
    FILE *map;				/* the map file stream */
    char *mappath;

    /*
     * open and truncate the map file
     */
    if (mapdir) {
	mappath = xmalloc(strlen(mapdir) + strlen(mapname) + 2);
	sprintf(mappath, "%s/%s", mapdir, mapname);
	fd = creat(mappath, 0644);
	free(mappath);
    } else {
	fd = creat(mapname, 0644);
    }
    if (fd < 0) {
	sprintf(err, "error: unable to creat/truncate %s\n", mapname);
	logit(err);
	return(1);
    }
    map = fdopen(fd, "w");
    sprintf(err, "extracting: %s\n", mapname);
    logit(err);

    /*
     * copy article shar data to the map file
     */
    while (1) {
	/*
	 * read the line
	 */
	clearerr(article);
	if (fgets(buf, BUFSIZ, article) == NULL) {
	    if (ferror(article)) {
		sprintf(err, "error: bad map data read after line %d\n",
			lineno);
		logit(err);
	    } else {
		logit("error: EOF while reading the map data\n");
	    }
	    goto fail;
	}
	++lineno;
	if (! check_newline(buf)) {
	    sprintf(err, "error: line %d, map data line too long\n", lineno);
	    logit(err);
	    goto fail;
	}

	/*
	 * watch for the end of the shar
	 */
	if (strcmp("SHAR_EOF\n", buf) == 0) {
	    /* end of the shar file */
	    fclose(map);
	    mapname[0] = '\0';
	    break;			/* look for final two lines */
	}

	/*
	 * write the line
	 */
	if (fputs(buf, map) == EOF) {
	    logit("error: bad write\n");
	    goto fail;
	}
    }

    /*
     * verify the : line after the end of the map
     */
    clearerr(article);
    if (fgets(buf, BUFSIZ, article) == NULL) {
	if (ferror(article)) {
	    sprintf(err, "error: bad ending : read after line %d\n",
		    lineno);
	    logit(err);
	} else {
	    logit("error: EOF while reading the ending : line\n");
	}
	return(1);
    }
    ++lineno;
    if (! check_newline(buf)) {
	sprintf(err, "error: line %d, ending : line too long\n", lineno);
	logit(err);
	return(1);
    }
    if (buf[0] != ':') {
	sprintf(err, "error: line %d, not an ending : line: %s", lineno, buf);
	logit(err);
	return(1);
    }

    /*
     * verify the exit 0
     */
    clearerr(article);
    if (fgets(buf, BUFSIZ, article) == NULL) {
	if (ferror(article)) {
	    sprintf(err, "error: bad ending exit read after line %d\n",
		    lineno);
	    logit(err);
	} else {
	    logit("error: EOF while reading the ending exit line\n");
	}
	return(1);
    }
    ++lineno;
    if (! check_newline(buf)) {
	sprintf(err, "error: line %d, ending exit line too long\n", lineno);
	logit(err);
	return(1);
    }
    if (strcmp("exit 0\n", buf) != 0) {
	sprintf(err, "error: line %d, not an ending exit line: %s",
		lineno, buf);
	logit(err);
	return(1);
    }

    /*
     * if we are here, all must be ok
     */
    return(0);

 fail:

    /*
     * if a failure was encountered, then close and remove the
     * file
     */

    fclose(map);
    (void) unlink(mappath);

    return(1);
}


/*
 * check_newline - check to be sure that the string endds in a newline
 *
 * returns a pointer to the newline, or NULL if none found
 */
char *
check_newline( str )
    char *str;				/* check for newline in str */
{
    char *p;				/* pointer */

    /* guess the newline location */
    p = str + strlen(str);
    p -= ((p > str) ? 1 : 0);

    /* return result of guess */
    return( (*p == '\n') ? p : NULL );
}

char *
xmalloc(size)
    unsigned size;
{
    extern char *malloc();
    char *ret = malloc(size);

    if (ret == NULL) {
	sprintf(err, "error: out of memory in xmalloc\n");
	logit(err);
	sprintf(err, "exiting: status: 1 -  %s", get_date());
	logit(err);
	exit(1);
    }

    return ret;
}

char *
xrealloc(region, size)
    char *region;
    unsigned size;
{
    extern char *realloc();
    register char *ret = realloc(region, size);

    if (ret == NULL) {
	sprintf(err, "error: out of memory in xrealloc\n");
	logit(err);
	sprintf(err, "exiting: status: 1 - %s", get_date());
	logit(err);
	exit(1);
    }

    return ret;
}
