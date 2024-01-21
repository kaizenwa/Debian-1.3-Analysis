/* @(#) uuwho.c,v 1.17 1992/08/02 00:23:20 tron Exp */
/*
 * uuwho:
 *	store/retrieve uucp map info in dbm format
 *
 * uuwho uses a dbm file with pointers into the real map files,
 * hence initialization MUST be done any time a map file has changed.
 *
 * usage:
 *   initialize:  uuwho -i [-d dbmfile] mapfile ...
 *   retrieve  :  uuwho [-d dbmfile] [sitename ...]
 *
 * Based on a program written by someone in the public domain,
 * which was then ported to System V by Gordon Moffett, which 
 * was then improved by Landon Noll, which was then re-written
 * again to fit into the smail system.
 *
 * Modified to use sorted files if DBM is not present by Ronald S. Karr.
 * Also use sorted files if UUWHO_USE_DBM is not defined.
 * Modified to search parent domains by Chip Salzenberg.
 */

#include <stdio.h>
#include <ctype.h>
#include "defs.h"

#ifdef UUWHO_USE_DBM
#include "dbm_compat.h"
#endif

#ifdef HAVE_UNAME
#include <sys/utsname.h>
#endif /* HAVE_UNAME */

#ifndef UUWHO_FILE
#define UUWHO_FILE "uuwho"	/* database basename in SMAIL_LIB_DIR */
#endif

#define NLEN	512

static char *cmdname;		/* same as argv[0] */

/* variable length record stored in who database */
typedef struct {
	long	pos;
	char	*name;
} position;

static int find();		/* find host's map entry */
static int find_exact();	/* find exactly host's map entry */
static void touchdbm();	/* be sure that the .dir and .pag files exist */
static void printmap();	/* print a map form using the location form dbm */
static char *skip_map_dir();	/* skip map directory in pathname */
static char *get_hostname();	/* get our histname */

static char line[BUFSIZ + 1];
static FILE *fp;
#ifndef UUWHO_USE_DBM
static FILE *db;		/* pipe to sort process, or open database */
#endif

static int rawformat = 0;

extern int errno;
extern long atol();
extern char *malloc();
extern char *realloc();

main(argc, argv)
    int argc;
    char *argv[];
{
    char *fbasename=UUWHO_FILE;	/* database basename */
    char *fname;		/* database filename */
    char *hostname=NULL;	/* != NULL ==> only print hostname */
    char *hname;		/* host name to lookup */
    int opt;			/* flag char */
    int namelen;		/* length of sitename */
    int newdbm = 0;		/* 1 ==> new database creation is requested */
    position pos;		/* the fetched record */
    extern char *optarg;	/* the arg of a flag */
    extern int optind;		/* the first non flag */

    /*
     * parse args
     */
    cmdname = argv[0];
    while ((opt = getopt(argc,argv,"ird:")) != EOF) {
	switch (opt) {
	case 'i':	/* new database being formed */
	    newdbm = 1;
	    break;
	case 'r':
	    rawformat = 1;
	    break;
	case 'd':
	    fbasename = optarg;
	    break;
	case '?':
	    usage();
	    break;
	}
    }
    if (newdbm == 1 && optind >= argc) {
	usage();			/* -i flag must have args */
    } else if (newdbm == 0 && optind >= argc) {
	/* looking now for the hostname, get it */
	hostname = get_hostname();
    }

    /*
     * open the database
     */
    if (fbasename[0] == '/' || fbasename[0] == '.') {
	fname = fbasename;
    } else {
	fname = malloc(strlen(SMAIL_LIB_DIR)+1+strlen(fbasename)+1);
	if (fname == NULL) {
	    perror(cmdname);
	    exit(1);
	}
	sprintf(fname, "%s/%s", SMAIL_LIB_DIR, fbasename);
    }

#ifdef UUWHO_USE_DBM
    touchdbm(fname,newdbm);
    if (dbminit(fname) < 0) {
	fprintf(stderr, "%s: dbinit(%s) failed\n", cmdname, fname);
	exit(2);
    }
#else
    if (newdbm) {
	char *sortcmd;
	char *p, *q;

	sortcmd = malloc(strlen(fname) * 2 + sizeof("sort +0 -1 > \"\""));
	if (sortcmd == NULL) {
	    perror(cmdname);
	    exit(1);
	}
	strcpy(sortcmd, "sort +0 -1 > \"");
	p = sortcmd + strlen(sortcmd);
	q = fname;
	while (*q) {
	    switch (*q) {
	    case '\\':
	    case '"':
	    case '$':
	    case '`':
		*p++ = '\\';
		break;
	    }
	    *p++ = *q++;
	}
	*p++ = '"';
	*p = '\0';

	db = popen(sortcmd, "w");
	if (db == NULL) {
	    perror(cmdname);
	    exit(1);
	}
    } else {
	db = fopen(fname, "r");
	if (db == NULL) {
	    perror(fname);
	    exit(1);
	}
    }
#endif
    /*
     * process each map or site
     */
    do {
	
	/*
	 * if we are initializing a database, just load each file
	 */
	if (newdbm) {
	    getfile(argv[optind]); /* add entries to database */
	    
	    
	    /*
	     * we are not loading but rather doing a querry, so look it up
	     */
	} else {
	    /*
	     * try to fetch something from the database
	     */
	    hname = hostname? hostname: argv[optind];
	    if (find(&pos, hname)) {
		printmap(&pos);
	    } else {
		fprintf(stderr, "no site entry for %s\n", hname);
	    }
	}
    } while (++optind < argc);

#ifdef UUWHO_USE_DBM
    dbmclose();
#else
    if (newdbm)
	pclose(db);
#endif

    /* end of the show */
    exit(0);
}

#ifdef UUWHO_USE_DBM

static int
find(posp, hname)
    position *posp;
    char *hname;
{
    char *h, *t;
    int ret = 0;

    if ((t = malloc(strlen(hname) + 2)) == NULL) {
	perror(cmdname);
	exit(1);
    }
    *t = '.';
    strcpy(t + 1, hname);

    h = t + 1;
    while (*h) {
	while (*h == '.') {
	    ++h;
	}
	if ((ret = find_exact(posp, h)) != 0 ||
	    (ret = find_exact(posp, h - 1)) != 0)
	{
	    break;
	}
	while (*h && *h != '.') {
	    ++h;
	}
    }
    free(t);
    return ret;
}

static int
find_exact(posp, hname)
    position *posp;
    char *hname;
{
    datum fileinfo;
    datum sitename;
    char *p;

    sitename.dptr = hname;
    sitename.dsize = strlen(hname);
    fileinfo = fetch(sitename);
    if (fileinfo.dptr == NULL)
	return 0;
    p = fileinfo.dptr;
    while (isspace(*p))
	p++;
    posp->pos = atol(p);
    while (*p && !isspace(*p))
	p++;
    while (isspace(*p))
	p++;
    posp->name = p;
    return 1;
}

#else	/* not UUWHO_USE_DBM */

static int
find(posp, hname)
    position *posp;
    char *hname;
{
    long middle, hi, lo;
    int c;
    int flag;
    int gotnum;
    int len = strlen(hname);		/* length of comparison */
    static char line[512];		/* fetched record */
    char *p;
    int i;

#ifndef SEEK_END
#define SEEK_END 2
#endif
    lo = 0;
    fseek(db, 0L, SEEK_END);
    hi = ftell(db);

    /*
     * "Binary search routines are never written right the first time around."
     * - Robert G. Sheldon.
     * << above comment retained 'cause I thought it was cute -- tron >>
     */
    for( ;; ) {
	middle = (hi + lo + 1)/2;
	(void) fseek(db, middle, 0); /* find midpoint */
	if (middle != 0) {		/* to beginning of next line */
	    while((c = getc(db)) != EOF && c != '\n') ;
	    if (c == EOF && ferror(db)) {
		perror(cmdname);
		exit(1);
	    }
	}
	p = line;
	while ((c = getc(db)) != EOF && !isspace(c) && c != ':') {
	    *p++ = c;
	}
	if (c == EOF && ferror(db)) {
	    perror(cmdname);
	    exit(1);
	}
	*p = '\0';
	flag = strcmp(line, hname);
	if (flag == 0)
	    break;
	if ( lo>=middle ) {		/* failure? */
	    return 0;
	}
	if ( c != EOF && flag < 0 ) {	/* close window */
	    lo = middle;
	} else {
	    hi = middle - 1;
	}
    }

    /*
     * Now just copy the result.
     */
    p = line;
    flag = 0;
    gotnum = 0;
    while ((c = getc(db)) != EOF && c != '\n') {
	if (isspace(c)) {
	    if (! flag)
		continue;
	    if (! gotnum) {
		*p = '\0';
		posp->pos = atol(line);
		p = line;
		gotnum = 1;
		flag = 0;
		continue;
	    }
	}
	flag = 1;
	*p++ = c;
    }
    if (c == EOF && ferror(db)) {
	perror(cmdname);
	exit(1);
    }
    if (! gotnum) {
	fprintf(stderr, "%s: warning: bad format in uuwho database\n", cmdname);
	return 0;
    }
    *p = '\0';

    posp->name = line;
    return 1;
}

#endif	/* UUWHO_USE_DBM */

/*
 * printmap - print a map file
 */
static void
printmap(p)
    position *p;		/* the fetched record */
{
    int comment;		/* 1 ==> within a #comment set */
    int pathdata;		/* 1 ==> have seen path data */
    int ctr = 0;		/* >0 ==> we have a new #N record */
    char *cp;			/* pointer */
    char *tp;			/* pointer */
    char c;			/* the char we are looking at */
    char unknown[2];		/* unknown #x line name */
    int lastblank;		/* last line was blank */

    /*
     * open the file
     */
#ifdef UNSHAR_MAP_DIR
    if (p->name[0] != '/') {
	cp = malloc(sizeof(UNSHAR_MAP_DIR) + strlen(p->name) + 1);
	if (cp == NULL) {
	    perror(cmdname);
	    exit(1);
	}
	sprintf(cp, "%s/%s", UNSHAR_MAP_DIR, p->name);
    }
    else
#endif	/* UNSHAR_MAP_DIR */
    {
	cp = p->name;
    }
    if ((fp = fopen(cp, "r")) == NULL) {
	fprintf(stderr, "%s: Can't open data file %s\n", cmdname, cp);
	return;
    }

    /* seek to the starting location in the file */
    fseek(fp, p->pos, 0);
    
    /*
     * process the entry one line at a time
     */
    pathdata = 0;	/* no path data seen yet */
    comment = 0;	/* not in a #comment set */
    unknown[1] = '\0';	/* terminate string */
    lastblank = 0;
    while (fgets(line, BUFSIZ, fp)) {

	if (rawformat) {
	    if (strncmp("#N", line, 2) == 0 && (line[2] == '\t' ||
		line[2] == ' ') && ctr++) {
		break;
	    }
	    fputs(line, stdout);
	    lastblank = line[0] == '\0';
	    continue;
	}

	/* always print non # (route) lines */
	if (*(cp = line) != '#') {
	    pathdata = 1; /* we have seen path data */
	    /* deal with trailing #comments */
	    if ( comment != 0 ) {
		putchar('\n');
		comment = 0; /* no more comments */
	    }
	    fputs(line, stdout);
	    continue;
	}

	/* skip the '#' */
	++cp;

	/* print out #<space>, #<tab> and #\n lines always */
	if (*cp == ' ' || *cp == '\t' || *cp == '\n') {
	    /* deal with #comment white spacing */
	    if ( pathdata == 0 && comment == 0 ) {
		/* white space before #comment set */
		putchar('\n');
		/* we have seen #comment lines */
		comment = 1;
	    }
	    fputs(line, stdout);
	    continue;
	    /* turn of #comment white spacing if needed */
	} else if ( pathdata == 0 && comment != 0 ) {
	    putchar('\n');
	    comment = 0; /* no more comments */
	}
	
	/* note if it is another #N line */
	if (*cp == 'N' && ctr++) {
	    /* a new entry, so stop reading */
	    break;
	}
	
	/* skip the x in #x, saving x in the process */
	c = *cp;
	if (*cp != '\n' && *cp != '\0') {
	    ++cp;
	}
	
	/* skip white space after #x for later use */
	while (*cp == ' ' || *cp == '\t') {
	    ++cp;
	}
	
	/* determine the name of the #X line */
	switch (c) {
	case 'N':
	    tp = "System name:";
	    break;
	case 'S':
	    tp = "System type:";
	    break;
	case 'F':
	    tp = "Arpa forwarder:";
	    break;
	case 'O':
	    tp = "Organization:";
	    break;
	case 'C':
	    tp = "Contact person:";
	    break;
	case 'E':
	    tp = "Email Address:";
	    break;
	case 'T':
	    tp = "Telephone:";
	    break;
	case 'P':
	    tp = "Postal Address:";
	    break;
	case 'L':
	    tp = "Long/Lat:";
	    break;
	case 'R':
	    tp = "Remarks:";
	    break;
	case 'U':
	    tp = "News links:";
	    break;
	case 'W':
	    tp = "Author & date:";
	    break;
	case '\t': /* line starts at #<tab>, special case */
	case '\n': /* line just as a #, special case */
	    tp = 0;
	    break;
	default: /* unknown #x line */
	    unknown[0] = c;
	    tp = unknown;
	    break;
	}
	
	/* print the #X line name and contents */
	printf("%-16.16s %s", tp, cp);
    }
    if (rawformat && ! lastblank)
	putchar('\n');
    fclose(fp);
}

/*
 * get a file and install it in the who database
 */
getfile(filename)
    char *filename;			/* the file to add */
{
    register char *cp;		/* pointer */
    register int namelen;	/* length of sitename */
    static int input=0;		/* 1 ==> already read input */
    char *malloc();		/* storage allocator */
    int linelen;		/* the length of the current line */
    long pos = 0;		/* current file position */
    position pp;		/* the variable length record to save */
#ifdef UUWHO_USE_DBM
    datum sitename;
    datum fileinfo;
#endif
    char *name;
    char info[512];

    /* try to open the new file */
    if (input == 0 && strcmp(filename, "-") == 0) {
	fp = stdin;
	input = 1;
	filename = "[stdin]";
    } else if (input == 1 && strcmp(filename, "-") == 0) {
	fprintf(stderr,
		"%s: already processed standard input\n", cmdname);
	return;
    } else if ((fp = fopen(filename, "r")) == (FILE *)NULL) {
	fprintf(stderr, "%s: Can't open %s\n", cmdname, filename);
	return;
    }

    /* omit default map directory */
    filename = skip_map_dir(filename, strlen(filename));

    /*
     * for each entry, note the position in the file
     */
    pos = 0; /* we start at the beginning */
    while (fgets(line, BUFSIZ, fp)) { /* one line at a time */
	char *p;		/* start of filename */
	char *q;		/* after filename */
	char *r;		/* pointer */
	
	/* watch for a file entry */
	if (strncmp(line, "file", 4) == 0) {
	    /* skip over whitespace */
	    p = line + 4 + strspn(&line[4], " \t\n");
	    /* skip over the opening { */
	    if (*p != '{') {
		continue;	/* not a useful line */
	    }
	    /* skip whitespace to find the filename */
	    p += strspn(p+1, " \t\n") + 1;
	    if (*p == '\0') {
		continue;	/* not a useful line */
	    }
	    /* find the end of the filename */
	    q = p + strcspn(p, " \t\n}");
	    if (*q == '\0') {
		continue;	/* not a useful line */
	    }
	    /* skip whitespace */
	    r = q + strspn(q, " \t\n");
	    /* skip over the closing } */
	    if (*r != '}') {
		continue;	/* not a useful line */
	    }
	    /* verify end of line */
	    if (*(r+1) != '\n' || *(r+2) != '\0') {
		continue;	/* not a useful line */
	    }
	    /* omit default map directory */
	    p = skip_map_dir(p, q - p);
	    /* form new fileinfo module */
	    filename = malloc(q - p + 1);
	    if (filename == NULL) {
		perror(cmdname);
		exit(1);
	    }
	    strncpy(filename, p, q-p);
	    filename[q-p] = '\0';
	    pos = 0;	/* assume beginning of the filename */
	    continue;
	}
	
	/* watch for a new entry */
	if (strncmp(line, "#N", 2) == 0 &&
		(line[2] == '\t' || line[2] == ' ')) {
	    
	    /*
	     * we have new entry, determine the name
	     *
	     * note that lines are of the form:
	     * #N sitename
	     * #N name1, name2, ...
	     */
	    cp = line + 2; /* skip over #N */
	    while (1) {
		/* skip white space or ',' */
		cp += strspn(cp, " \t,");
		/* if no more #N names stop processing line */
		if (*cp == '\n') {
		    break;
		}
		/* note size of name */
		name = cp; /* name start addr */
		cp += strcspn(cp, "\n, \t");
		namelen = cp - name; /* length */
		/* limit name length for sanity */
		namelen = (namelen > NLEN) ? NLEN : namelen;
		/* form line containing file and offset */
		sprintf(info, "%ld %s", pos, filename);

#ifdef UUWHO_USE_DBM
		/*
		 * store the file/position info with the
		 * sitename as the fetch key
		 */
		sitename.dptr = name;
		sitename.dsize = namelen;
		fileinfo.dptr = info;
		fileinfo.dsize = strlen(info) + 1;
		store(sitename, fileinfo);
#else	/* not UUWHO_USE_DBM */
		fprintf(db, "%.*s\t%s\n", namelen, name, info);
#endif	/* UUWHO_USE_DBM */
		/* process other sitenames on the line */
	    }
	}
	/* note the new position */
	linelen = strlen(line);
	if (linelen >= BUFSIZ-2) {	/* avoid excessively bogus lines */
	    fprintf(stderr,
		    "%s: %s has a line >= %d chars\n",
		    cmdname, filename, BUFSIZ-2);
	    exit(1);
	}
	pos += (long)linelen;
    }
    
    /* cleanup */
    fclose(fp);
}

static char *
skip_map_dir(p, len)
    char *p;
    unsigned len;
{
#ifdef UNSHAR_MAP_DIR
    if (len >= sizeof(UNSHAR_MAP_DIR)
	&& strncmp(p, UNSHAR_MAP_DIR, sizeof(UNSHAR_MAP_DIR) - 1) == 0)
    {
	p += sizeof(UNSHAR_MAP_DIR) - 1;
	len -= sizeof(UNSHAR_MAP_DIR) - 1;
	while (len && *p == '/') {
	    ++p, --len;
	}
    }
#endif	/* UNSHAR_MAP_DIR */
    return p;
}

/*
 * show command usage, and die
 */
usage()
{
    fprintf(stderr, "usage: %s [-d dbmfile] -i mapfile ...\n", cmdname);
    fprintf(stderr, "usage: %s [-d dbmfile] [sitename ...]\n", cmdname);
    exit(-1);
}

#ifdef UUWHO_USE_DBM
/*
 * touchdbm - be sure that the dbm files .pag and .dir exist
 */
static void
touchdbm(name,wr)
    char *name;				/* basename of the dbm file */
    int wr;				/* 1=>open for writing, 0=> reading */
{
    char *filename;			/* the .dir and .pag names*/
    int fd;				/* .dir or .pag file */

    /*
     * form the namespace for filename
     */
    filename = malloc(strlen(name) + 4 + 1);
    if (filename == NULL) {
	perror(cmdname);
	exit(1);
    }
    
    /*
     * be sure that the .pag name exists
     */
    sprintf(filename, "%s.pag", name);
    fd = open(filename, wr);
    if (fd < 0 && ! wr) {
	fprintf(stderr, "%s: can not open %s\n", cmdname, filename);
	exit(1);
    } else if (fd < 0 && (fd = creat(filename, 0666)) < 0) {
	fprintf(stderr, "%s: can not create or write %s\n", cmdname, filename);
	exit(1);
    }
    close(fd);
    
    /*
     * be sure that the .dir name exists
     */
    sprintf(filename, "%s.dir", name);
    fd = open(filename, wr);
    if (fd < 0 && ! wr) {
	fprintf(stderr, "%s: can not open %s\n", cmdname, filename);
	exit(1);
    } else if (fd < 0 && (fd = creat(filename, 0664)) < 0) {
	fprintf(stderr, "%s: can not create or write %s\n", cmdname, filename);
	exit(1);
    }
    close(fd);
}
#endif	/* UUWHO_USE_DBM */

/*
 * get_hostname - get our hostname for a no arg querry
 */
#ifdef HAVE_GETHOSTNAME
static char *
get_hostname()
{
    char *hostname;			/* who we are */
    char *malloc();			/* memory allocator */

    /*
     * My man page says that 255 chars (plus nul byte) is the limit
     * on length of the local host name.  There appears to be no
     * #define for it in 4.2BSD.
     */
    hostname = malloc(NLEN+1+1);
    if (hostname == NULL) {
	fprintf(stderr, "%s: bad mallof of hostname\n", cmdname);
	exit(3);
    }
    if (gethostname(hostname, NLEN+1) < 0) {
	hostname = NULL;		/* unknown hostname */
    }
    return(hostname);
}
#else	/* not HAVE_GETHOSTNAME */
# ifdef HAVE_UNAME
static char *
get_hostname()
{
    static struct utsname utsname;

    (void) uname(&utsname);
    /* is the sysname tag used for something interesting? */
    return(utsname.nodename);
}
#else	/* not HAVE_UNAME */
static char *
get_hostname()
{
    return NULL;
}
# endif	/* HAVE_UNAME */
#endif	/* not HAVE_GETHOSTNAME */
