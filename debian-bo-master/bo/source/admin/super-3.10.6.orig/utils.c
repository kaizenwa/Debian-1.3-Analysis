#include "super.h"

/* Variables table flag -- !0 means has been created */
static int created_variables_table = 0;

char *realloc();
/* an expandable input buffer */
struct Ebuf {
    char *buf;
    int l;
    int nalloc;
};
/* checksize of Ebuf; grow an Ebuf */
static char *checksize __P((struct Ebuf *cb, int N));
static char *grow __P((struct Ebuf *cb, int N));

/* The input, cleaned input, and variable expansion buffers */
static struct Ebuf ebuf = { NULL, 0, 0 };
static struct Ebuf ebuf_clean = { NULL, 0, 0 };
static struct Ebuf variablebuf = { NULL, 0, 0 };


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Get environment variable */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
Getenv(s)
char *s;
{
    /* Like getenv(), but returns ptr to the <name> in "name=xxxx",
     * not just the xxxx.
     */
    char **envp; 
    int l;
    extern char **environ;

    if (!s)
	return (char *) NULL;
    l = strlen(s);
    for (envp=environ; *envp ; envp++)
	if (strncmp(*envp, s, l) == 0  &&  *(*envp+l) == '=')
	    return *envp;
    return (char *) NULL;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Get login directory of a user */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
getlogdir(user, buf)
char *user;
char *buf;
{
    /* Gets the login directory of the named user, and puts it into buf.
     * If user==NULL || *user == '\0', the current user is obtained.
     * Best if buf is MAXPATHLEN long.
     * 0 is returned on success; -1 on error.
     */
 
    struct passwd *pass;
    char *p;
    char *getlogin();
 
    buf[0] = '\0';
    if (user != NULL && *user != '\0') {
	/* Name given; use getpwnam */
	pass = getpwnam(user);
    } else {
	/* No user given; use current uid */
	pass = getpwuid(getuid());
    }
 
    if (pass == (struct passwd *) NULL)
	return -1;
 
    (void) strcpy(buf, pass->pw_dir);
 
    return 0;
}
 
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Copies in to out, prefixing with "^" and suffixing with "$"
 * if these are missing.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
anchor(in, out)
char *in;
char *out;
{
    void re_anchor __P((char *, char *));
    if (need_re_anchor)
	re_anchor(in, out);
    else
	(void) strcpy(out, in);
}

void
re_anchor(in, out)
char *in;
char *out;
{
    int i;
    i = (*in != '^');
    if (i)
	out[0] = '^';
    (void) strcpy(out+i, in);
    i = strlen(out);
    if (out[i-1] != '$')
	out[i++] = '$';
    out[i] = '\0';
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Grow an expandable buffer */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
static char *
grow(cb, nb)
struct Ebuf *cb;
int nb;			/* amount to grow, bytes */
{
    if (cb->buf)
	cb->buf = realloc(cb->buf, cb->nalloc += nb);
    else
	cb->buf = malloc(cb->nalloc += nb);

    return cb->buf;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Grow buffer if less than N bytes free */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
static char *
checksize(cb, N)
struct Ebuf *cb;
int N;
{
    if (cb->nalloc - cb->l  <  N)
	return grow(cb, 2*N);
    else
	return cb->buf;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Check if string s1 ends with string s2 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char
*ends(s1, s2)
char *s1, *s2;
/* If s1 ends with string s2, a pointer to the ending of s1 is returned;
 * else null
 */
{
    int l1, l2;
    l1 = strlen(s1);
    l2 = strlen(s2);
    if (l1 < l2)
	return NULL;
    else if (strcmp(s1+l1-l2, s2) == 0)
	return s1+l1-l2;
    else
	return NULL;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Copy the input buffer to outbuf (default clean_buf),
 * cleaning out comments and the backslash-newline-whitespace parts; the
 * latter become plain whitespace if the backslash is preceded by letter,
 * digit, or underscore; otherwise, they are deleted.
 * Comments are also deleted.
 * Return a ptr to the string in the cleaned buffer.

 * Returns null pointer on malloc error.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
clean_buf(buf, outbuf)
char *buf;		/* input buffer */
char *outbuf;		/* output buffer; NULL means to use the
			 * ebuf_clean buffer.  If non-null, you must make
			 * sure it's >= buf in size.
			 */
{
    register char *s, *t;
    char inquote = '\0';

    if (outbuf) {
	t = outbuf;

    } else {
	if (!(t = outbuf = checksize(&ebuf_clean, ebuf.nalloc))) {
	    Error(1, 0,
		"%t\n\tFailed to malloc space for clean copy of input text\n");
	    return NULL;
	}
    }

    /* Copy s (input buffer) to t (clean input buffer);
     * delete comments, delete backslash-newline-whitespace
     * or replace w/ ' '
     */
    for (s = buf; *s; ) {
	/* Assert not in comment */
	if (*s == '\\' && *(s+1) == '\n') {
	    /* At continued line; skip over backslash-newline-whitespace;
	     * make it a blank if following digit,letter,_  .
	     */
	    if ( (s > buf) && (isalnum(*(s-1)) || *(s-1) == '_') )
		*t++ = ' ';
	    s += 2;
	    while (isspace(*s))
		s++;
	    /* s has been left positioned at next character to process */

	} else if (inquote) {
	    /* In a quote */
	    if (*s == inquote) {
		/* end of quote */
		inquote = '\0';
	    }
	    *t++ = *s++;

	} else if (my_qm[*(unsigned char *)s]) {
	    /* Begin quote */
	    inquote = *t++ = *s++;

	} else if (my_cc[*(unsigned char *)s]) {
	    /* Begin comment; skip over until EOL */
	    char last_noncmt = (s > buf) ? *(s-1) : '\0';

	    while (*s && *s != '\n')
		s++;
	    if (*s && *(s-1) == '\\') {
		/* At continued line.  Ignore comment part, and
		 * look at last non-comment char before
		 * backslash-newline-whitespace; then process as above.
		 */
		if ( (s > buf) && (isalnum(last_noncmt) || last_noncmt == '_') )
		    *t++ = ' ';
		s++;
		while (isspace(*s))
		    s++;
	    }
	    /* s has been left positioned at next character to process */

	} else {
	    /* Ordinary character */
	    *t++ = *s++;
	    /* s has been left positioned at next character to process */
	}
    }
    *t = '\0';

    return outbuf;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Do fgets to get one logical line: join lines that are terminated
 * with backslash-newline.  Don't discard backslash or newline (so that
 * we can print the exact text, if desired).
 * The result is stored in "ebuf" and a pointer to the string
 * is returned.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
fillbuffer(fp, all_indented, nl)
FILE *fp;
int *all_indented;	/* Returned !0 if continued lines are all indented */
int *nl;		/* Returned with number of lines read in */
{
    char c, *s;
    ebuf.l = 0;	/* clear the extensible buffer */

    /* Collect lines until we have a non-zero buffer (which happens with
     * the first line, of course) and it isn't terminated "\\\n".
     */

    if (nl) *nl = 0;
    *all_indented = 1;
    UNTIL(ebuf.l && !(s=ends(ebuf.buf, "\\\n"))) {
	
	if (!checksize(&ebuf, 1024)) {
	    /* Needed to, but couldn't increase the allocated space */
	    return NULL;
	}
	if (!fgets(ebuf.buf+ebuf.l, ebuf.nalloc - ebuf.l, fp))
	    return NULL;
	c = *(ebuf.buf + ebuf.l) ;
	if (nl)
	    (*nl)++;
	if (ebuf.l != 0 && !(isspace(c) || c == '#')) {
	    /* Continued line not indented. */
	    *all_indented = 0;
	}
	ebuf.l += strlen(ebuf.buf+ebuf.l);
    }
    return ebuf.buf;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Open a file; chain it to the previous opened list.
 * Returns NULL pointer on malloc error, stat error, fopen error,
 *	ownership error, etc.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
FileList *
file_open(parent, name)
FileList *parent;
char *name;
{
    char *s;

    FileList *fl;
    struct stat st;
    static int nobody_uid = -999;
    static struct passwd *nobodypw;
    int uid, euid;

    if (nobody_uid == -999) {
	nobodypw = getpwnam("nobody");
	nobody_uid = nobodypw ? nobodypw->pw_uid : 65534;
    }

    fl = (FileList *) malloc(sizeof(FileList));
    if (!fl) {
	Error(0, 0,
	"%tFailed to malloc list member space for include file <%s>\n", name);
	return NULL;
    }

    if (!(fl->givenname = malloc(strlen(name)+1)) ) {
	Error(0, 0, "%tFailed to malloc space for filename <%s>\n", name);
	return NULL;
    }
    strcpy(fl->givenname, name);

    if (*name != '/' && parent != NULL) {
	/* it must be relative to the superfile directory */

	int l1, l2;

	s = strrchr(superfile, '/');
	if (s) {
	    l1 = s + 1 - superfile;
	    l2 = strlen(name);
	    if (!(fl->fullname = malloc(l1+l2+1)) ) {
		Error(0, 0,
		    "%tFailed to malloc space for expanded filename of <%s>\n",
		    name);
		return NULL;
	    }

	    sprintf(fl->fullname, "%.*s%s", l1, superfile,
			(strncmp("./", name, 2) == 0) ? name+2 : name);
	} else {
	    fl->fullname = fl->givenname;
	}
    } else {
	/* accept the name as is */
	fl->fullname = fl->givenname;
    }

    if (debug)
	if (parent == NULL)
	    fprintf(stderr, "\tOpening file %s\n", fl->fullname);
	else
	    fprintf(stderr, "\tLine %d: opening include file %s\n",
		parent->line, fl->fullname);

    /* For any file:
     *	If we are running as root:
     *		make sure that the super.tab file is owned by
     *		root or nobody, and is not writable by group/world.
     *	Otherwise, if our real uid differs from that under which we
     *		started the program, make sure the super file is owned
     *		by ourselves and not group/world writable.
     */
    if (stat(fl->fullname, &st) == -1) {
	Error(1, 0, "Couldn't stat super.tab %sfile `%s': ",
		    parent == NULL ? "" : "include ", fl->fullname);
	return NULL;
    }
    euid = geteuid();
    uid = getuid();
    if (euid == 0 && uid == 0) {
	/* Being run by root */
	if (st.st_uid != 0 && st.st_uid != nobody_uid) {
	    Error(0, 0, "super.tab %sfile `%s' isn't owned by root \
or nobody,\n\tbut we are being run by root.  Bailing out.\n",
		parent == NULL ? "" : "include ", fl->fullname);
	    return NULL;
	}
	if (st.st_mode & (S_IWGRP || S_IWOTH)) {
	    Error(0, 0, "super.tab %sfile `%s' is owned by root \
or nobody,\n\tbut is group- or world-writeable.  Bailing out.\n",
		parent == NULL ? "" : "include ", fl->fullname);
	    return NULL;
	}

    } else if (euid == 0) {
	/* Running as root, original user someone else */
	if (st.st_uid != 0 && st.st_uid != nobody_uid) {
	    Error(0, 0, "super.tab %sfile `%s' isn't owned by root \
or nobody,\n\tbut our euid==0.   Reverting to uid=%d.\n",
		    parent == NULL ? "" : "include ", fl->fullname, getuid());
	    setuid(uid);
	}
	if (st.st_mode & (S_IWGRP || S_IWOTH)) {
	    Error(0, 0, "super.tab %sfile `%s' is owned by root \
or nobody,\n\but is group- or world-writeable.  Reverting to uid=%d.\n",
		parent == NULL ? "" : "include ", fl->fullname, getuid());
	    setuid(uid);
	}

    } else if (uid != userinfo.caller.pw_uid) {
	/* Real uid has changed -- make sure uid is owner of superfile. */
	if (st.st_uid != uid) {
	    Error(0, 0, "super.tab %sfile `%s' isn't owned by uid %d.\n",
		parent == NULL ? "" : "include ", fl->fullname, uid);
	    return NULL;
	}
	if (st.st_mode & (S_IWGRP || S_IWOTH)) {
	    if (getuid() == 0) {
		Error(0, 0,
	    "super.tab %sfile `%s' is group or world-writable.  Bailing out.\n",
		    parent == NULL ? "" : "include ", fl->fullname);
		return NULL;
	    }
	}
    }


    if ((fl->fp = fopen(fl->fullname, "r")) == NULL) {
	Error(1, 0, "%t\n\tCouldn't open super.tab %sfile `%s': ",
		parent == NULL ? "" : "include ", fl->fullname);
	return NULL;
    }

    fl->line = 1;
    fl->nl = 0;
    fl->prev = parent;

    return fl;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Close a file; return previous in list, or NULL when all done */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
FileList *
file_close(curr)
FileList *curr;
{
    FileList *parent;

    if (!curr)
	return (FileList *) NULL;

    parent = curr->prev;

    if (debug) {
	fprintf(stderr, "\tClosing %sfile %s.\n",
		parent == NULL ? "" : "include ", curr->fullname);
	if (parent)
	    fprintf(stderr, "\tReturning to %s file %s, line %d\n",
		parent->prev == NULL ? "top-level" : "include ",
		parent->fullname, parent->line);
    }

    fclose(curr->fp);

    if (curr->fullname != curr->givenname)
	free(curr->fullname);
    free(curr->givenname);
    free(curr);

    return parent;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Initialize an StrArray */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
StrInit(a)
StrArray *a;
{
    a->n = 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Get address of string element of StrArray */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
StrEltGetPtr(a, ielt)
StrArray *a;	/* StrArray with element to fill */
int ielt;	/* Element to return */
    /* Returns NULL if no such element (or if unused) */
{
    if (ielt >= 0 && ielt < a->n && a->str[ielt].used)
	return a->str[ielt].s;
    else
	return NULL;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Put string into element of StrArray.
 * Returns -1 on malloc/realloc failure, 0 otherwise.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
StrEltCpy(a, ielt, str)
StrArray *a;	/* StrArray with element to fill */
int ielt;	/* Element to be filled */
char *str;	/* what to put into ielt'th place */
{
    int l = strlen(str);
    if (StrNalloc(a, ielt) == -1)
	return -1;

    a->str[ielt].used = 1;
    if (a->str[ielt].n == 0) {
	a->str[ielt].s = malloc(l+1);
	a->str[ielt].n = l+1;
    } else if (a->str[ielt].n < l+1) {
	a->str[ielt].s = realloc(a->str[ielt].s, l+1);
	a->str[ielt].n = l+1;
    }
    if (!a->str[ielt].s)
	return -1;
    strcpy(a->str[ielt].s, str);
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Returns number of elements of StrArray */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
StrNElts(a)
StrArray *a;
{
    return a->n;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Return number of in-use elements of StrArray */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
StrLastInUse(a)
StrArray *a;	/* StrArray with element to fill */
{
    int i;
    int last;

    for (i=0, last = -1; i < a->n; i++) {
	if (a->str[i].used)
	    last = i;
    }

    return last;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Marks all elements of StrArray as unused */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
StrEltsUnused(a)
StrArray *a;
{
    int i;
    for (i=0; i < a->n; i++)
	a->str[i].used = 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Makes StrArray large enough to have an ielt'th element.
 * Returns -1 on malloc/realloc failure, 0 otherwise.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
StrNalloc(a, ielt)
StrArray *a;	/* StrArray that must have an ielt'th element */
int ielt;
{
    int i;

    if (ielt >= a->n) {
	/* Have to increase the new space */
	unsigned int new_n;
	if (a->n == 0) {
	    new_n = (ielt < 4) ? 4 : 2*ielt ;
	    a->str = (CountedString *) malloc(sizeof(CountedString) * new_n);
	} else {
	    new_n = 2*ielt;
	    a->str = (CountedString *)
		    realloc((void *) a->str, sizeof(CountedString) * new_n);
	}
	if (!a->str)
	    return -1;
	for (i=a->n; i < new_n; i++)
	    a->str[i].n = 0;
	a->n = new_n;
    }
    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Copy up to n-1 characters into "to"; then null-terminate.
 * Return 1 if all characters fitted into "to", else return 0.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
stringcopy(to, from, n)
char *to;       /* buffer to copy into */
char *from;     /* string to copy */
int n;          /* size of to buffer */
{
    int l = strlen(from);
    if (l >= n) {
	strncpy(to, from, n-1);
	to[n-1] = '\0';
	return 0;
    } else {
	strcpy(to, from);
	return 1;
    }
    /* NOTREACHED */
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Opens the logfile. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
opensuperlog()
{
    extern FILE *error_logfile;	/* to tell Error() where the log is */

    close_writer();		/* in case there is already one running */

    if (*globalinfo.log.filename == '\0') {
	Error(0, 0, "opensuperlog(): logfile name is (nil)\n");
	return;
    }

    globalinfo.log.fp = open_writer(globalinfo.log.user,
				globalinfo.log.filename, &globalinfo.log.pid);

    error_logfile = globalinfo.log.fp;	/* ...so Error() writes here too */
    return;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
    /* In order to implement the loguser=xxx option, we (1) create a pipe,
     * (2) fork, in the child setuid to loguid; (3) child opens logfile;
     * (4) child copies from pipe to logfile.  Meanwhile, we return a pointer
     * to a stream to the pipe as the log stream seen by the parent program.
     * This allows us to implement a special uid for the logfile writer,
     * without needing the operating system to offer saved uid's or
     * interprocess file-descriptor passing, etc.
     */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
FILE *
open_writer(user, filename, pid_p)
char *user;
char *filename;
pid_t *pid_p;	/* filled with pid of created logger */
{
    FILE *fp = NULL;
    int fildes[2];
    pid_t child;

    if (pipe(fildes) == -1) {
	(void) Error(1, 0, "Failed to created pipe for logfile; no logging: ");
	return NULL;
    }
    child = fork();
    if (child == -1) {
	(void) Error(1, 0, "Failed to create child for logfile; no logging: ");
	return NULL;

    } else if (child > 0) {
	/* In parent -- close read side, and aim logstream at write side */
	if (pid_p)
	    *pid_p = child;
	(void) close(fildes[0]);
	if (!(fp = fdopen(fildes[1], "w"))) {
	    (void) Error(1, 0,
		"failed to fdopen logfile pipe writer; no logging: ");
	    (void) close(fildes[1]);
	    return NULL;
	}

    } else if (child == 0) {
	/* In child.  Open log file and copy from pipe to log. */
	FILE *input;
	char text[2000];
	(void) close(fildes[1]);
	if (!(input = fdopen(fildes[0], "r"))) {
	    (void) Error(1, 0,
		"failed to fdopen logfile pipe reader; no logging: ");
	    (void) close(fildes[1]);
	    exit(1);
	}
	if (user && *user != '\0') {
	    stringcopy(localinfo.user, user, sizeof(localinfo.user));
	    *localinfo.group = '\0';
	    *localinfo.u_g = '\0';
	    if (set_u_g() == -1) {
		(void) Error(1, 0,
		"failed to setuid %s before opening logfile; no logging: ",
		globalinfo.log.user);
		exit(1);
	    }
	}
	if (!(fp = fopen(globalinfo.log.filename, "a"))) {
	    if (user && *user != '\0')
		(void) Error(1, 0,
		    "failed to open logfile `%s' using uid `%s': ",
		    globalinfo.log.filename, localinfo.user);
	    else
		(void) Error(1, 0,
		    "failed to open logfile `%s': ", globalinfo.log.filename);
	    exit(1);
	}
	while (fgets(text, sizeof(text), input)) {
	    if (fputs(text, fp) == EOF)
		(void) Error(1, 0, "fputs to logfile failed: ");
	}
	(void) fclose(fp);
	exit(0);
    }
    return fp;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Closes the logfile stream, then calls wait(). */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void
close_writer()
{
    pid_t pid;

    if (!globalinfo.log.fp && globalinfo.log.pid == -1)
	return;

    if (globalinfo.log.fp) {
	if (fclose(globalinfo.log.fp) == EOF)
	    Error(1, 0, "failed to close globalinfo.log.fp: ");
	globalinfo.log.fp = NULL;
    }

    if (globalinfo.log.pid != -1) {
	while ((pid = wait((int *) NULL)) > 0 && pid != globalinfo.log.pid) {
	    Error(0, 0, "wait() surprised! close_writer() received pid %d;\n\t\
expected logger pid = %d; waiting for correct pid...\n",
			pid, globalinfo.log.pid);
	}
	if (pid == -1)
	    Error(1, 0, "while waiting for logger process to exit");
	globalinfo.log.pid = -1;
    }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
    /* Construct a host-unique directory name:
     * xyz.home.caltech.edu -> prefix/xyz.home.caltech.edu/user
     * If hostname is empty string, file is prefix/user.

     * WARNING: the hostname used is that from gethostname().
     * Note that this is not necessarily unique across
     * internet domains, since it is frequently not a
     * fully-qualified domain name.  Therefore you should NOT
     * share the timestamp directory outside the local domain.
     */

    /* returns NULL on error, constructed path otherwise */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
makedirname(prefix, hostname, file)
char *prefix, *hostname, *file;
{
    int l;
    char *s, str[MAXPATHLEN];

    l = strlen(prefix) + 1 + strlen(hostname) + 1;
    if (l >= MAXPATHLEN) {
	Error(1, 0,
	"Can't create timestamp directory: it would exceed MAXPATHLEN = %d\n",
	MAXPATHLEN);
	return NULL;
    }

    strcpy(file, prefix);

    if (!*hostname)
	return file;

#ifndef HAVE_LONG_FILE_NAMES
    strncpy(str, hostname, 14);
    str[14] = '\0';	/* in case exactly 14 chars were copied */
#else
    strcpy(str, hostname);
#endif
    for (s = strrchr(str, '.'); s; *s = '\0', s = strrchr(str, '.')) {
	strcat(file, "/");
	strcat(file, s+1);
    }
    strcat(file, "/");
    strcat(file, str);

    return file;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Creates a directory, including any needed directories leading
 * to it.  Returns -1 on stat/mkdir failure; 0 otherwise.
 * WARNING: doesn't check if final component is a directory.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
makedir(dir)
char *dir;	/* path with directories only. */
{
    static struct stat st;
    char *q;
    char path[MAXPATHLEN];

    /* First create directories along way, if necessary */
    strcpy(path, dir);

    for (q=path; q && *q; ) {

	/* skip leading slashes */
	while (*q == '/')
	    q++;

	/* check directory before next slash */
	q = strchr(q, '/');
	if (q)
	    *q = '\0';

	/* Stat directory; if missing, create it */
	if (stat(path, &st) != 0) {
	    if (errno != ENOENT) {
		return Error(1, 0, "Failed to stat directory `%s'\n", path);
	    } else {
		if (mkdir(path, 0700) != 0)
		    return Error(1, 0,
				"Failed to create directory `%s'\n", path);
	    }
	}

	/* Restore slash */
	if (q)
	    *q = '/';
    }

    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Adds a variable definition.
 * Returns 0 on success, -1 on error.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
add_variable(varname, vardefn)
char *varname;
char *vardefn;
{
    /*
     * Allocate space for varname, vardefn, and insert into
     * vars table.
     */
    ENTRY item;
    char *space;
    int ln = strlen(varname);
    int lb = strlen(vardefn);

    if (!created_variables_table) {
	if (!hcreate(1000))
	    return Error(0, 0,
		"%tCouldn't allocate hash table for variable processing\n");
	created_variables_table = 1;
    }

    if (debug > 1)
	fprintf(stderr, "entering add_variable(\"%s\",\"%s\")\n",
		varname, vardefn);

    space = malloc(ln+1 + lb+1);
    if (!space)
	return Error(0, 0,
	    "%tFailed to allocate space for definition of variable `%s'\n",
	    varname);

    item.key = space;
    item.data = space + ln+1;
    strcpy(item.key, varname);
    strcpy(item.data, vardefn);

    if (debug > 1)
	fprintf(stderr, "Adding variable `%s' = `%s'\n", item.key, item.data);

    if (!hsearch(item, ENTER))
	return Error(0, 0,
	"%tFailed to allocate space for hash-table entry for variable `%s'\n",
		varname);

    return 0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Does variable substitution on a string.  */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
do_variables(string)
char *string;
{
    char c, *dollar, *varname;
    char *tail=NULL;
    char savechar;
    char key[2];
    int l;
    ENTRY wanted_item, *found_item;

    dollar = strchr(string, '$');	/* Special early return for no vars */
    if (!dollar)
	return string;

    if (debug > 1)
	fprintf(stderr, "Applying variable expansion to: `%s'\n", string);

    if (!created_variables_table) {
	Error(0, 0,
"%tNo variables have been defined, but super.tab file is using some anyway!\n");
	return NULL;
    }

    variablebuf.l = 0;		/* Clear the buffer */

    /* Initialize the buffer to be at least the same size as the input buffer */
    if (!checksize(&variablebuf, ebuf.l)) {
	Error(1, 0, "%tCouldn't increase space for variable processing\n");
	return NULL;
    }

    while (string && *string) {
	/* First, copy up to variable character */
	dollar = strchr(string, '$');
	if (!dollar)
	    l = strlen(string);
	else
	    l = dollar - string;
	strncpy(variablebuf.buf + variablebuf.l, string, l);
	variablebuf.l += l;
	variablebuf.buf[variablebuf.l] = '\0';
	string += l;

	/* Reached end of string? */
	if (!*string)
	    break;

	/* Not at string end; expand variable */

	if ( (c = *(dollar+1)) == '$') {
	    /* $$ is a special macro name */
	    key[0] = c;
	    key[1] = '\0';
	    varname = key;
	    string += 2;
	    savechar = '\0';

	} else if (isalnum(c) || c == '_') {
	    /* Replace $name -> variable defn */
	    for (tail=dollar+2; isalnum(*tail) || *tail == '_'; tail++)
		;
	    varname = dollar+1;
	    /* tail points to one past last char in variable name.
	     * Save this character, then overwrite with null char.
	     */
	    savechar = *tail;
	    *tail = '\0';
	    string = tail;

	} else if (c == '(') {
	    /* Replace $(name) -> variable defn */
	    for (tail=dollar+2; isalnum(*tail) || *tail == '_'; tail++)
		;
	    /* must have reached right paren, and must make sure first
	     * character was alphabetic
	     */
	    if (tail == dollar+2) {
		Error(0, 0, "%tEmpty variable name is illegal: `$()'\n");
		return NULL;

	    } else if (*tail != ')' || !isalpha(*(dollar+2))) {
		Error(0, 0, "%tNot a valid variable name: `%.*s'\n",
				tail+1-dollar, dollar);
		return NULL;
	    }
	    savechar = '\0';
	    *tail = '\0';
	    varname = dollar+2;
	    string = tail+1;

	} else {
	    /* All other $X is error */
	    Error(0, 0,
		"%tIllegal variable name `$%c'.  \n\
(Use `$$' to get a plain `$' passed on to the rest of file parsing.)\n", c);
	    return NULL;
	}

	if (debug > 1) {
	    fprintf(stderr, "Variable name is `%s'\n", varname);
	}
	if (varname) {
	    /* Have a variable requiring expansion */
	    wanted_item.key = varname;
	    found_item = hsearch(wanted_item, FIND);
	    if (!found_item) {
		Error(0, 0, "%t\n\tNo such variable as `$%s'\n",
			wanted_item.key);
		return NULL;
	    }
	    l = strlen(found_item->data);
	    if (!checksize(&variablebuf, ROUNDUP(l+1, 1024))) {
	    Error(1, 0, "%tCouldn't increase space for variable processing\n");
		return NULL;
	    }
	    strcpy(variablebuf.buf+variablebuf.l, found_item->data);
	    variablebuf.l += l;
	    variablebuf.buf[variablebuf.l] = '\0';
	    /* Restore the saved character */
	    if (savechar && tail)
		*tail = savechar;
	}
    }

    if (debug > 1)
	fprintf(stderr, "variable-expanded string is `%s'\n", variablebuf.buf);

    return variablebuf.buf;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Input is string like   "lhs<sep>rhs"
 * If there is no <sep>, null ptr is returned.
 * If lhs == left, then ptr to rhs is returned; else null pointer.
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
char *
str_val(left, sep, str)
char *left;
int sep;
char *str;
{
    char *s = strchr(str, sep);

    if (!s					/* equal sign? */
	|| strlen(left) != (s - str)		/* not same size as `left'? */
	|| strncmp(left, str, s-str) != 0)	/* lhs != left */
	return NULL;
    
    return s+1;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* re_comp()-style interface to wildmat. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
static char *shell_pattern;
char *
shell_compile(s)
char *s;
{
    shell_pattern = s;
    return NULL;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* re_exec()-style interface to wildmat. */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
shell_compare(s)
char *s;
{
    extern int wildmat __P((char *str, char *pat));
    return wildmat(s, shell_pattern);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
init_strqtokS()
{
    unsigned char *p;
    memset(my_qm, '\0', sizeof(my_qm));
    for (p=(unsigned char *) QM; *p; )
	my_qm[*p++] = 1;

    memset(my_cc, '\0', sizeof(my_cc));
    for (p=(unsigned char *) CM; *p; )
	my_cc[*p++] = 1;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* Finds the hostname, like gethostname(), but uses gethostbyname, if
 * available to canonicalise the name.
 * Returns 0 on success, and -1 on error, like gethostname().
 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
get_canonical_hostname(buf, len)
char *buf;
int len;
{
#ifdef HAVE_GETHOSTBYNAME
    struct hostent *he, *he_nodot, *he_dot;
#endif

    if (gethostname(buf, len) < 0)
	return Error(0, 0, "gethostname(\"%s\", %d) failed\n", buf, len);

#ifdef HAVE_GETHOSTBYNAME
    if (globalinfo.gethostbyname) {
	if ((l=strlen(buf)) > len - 2)
	    return Error(0, 0,
			"hostname %s is too long for our buffer!\n", buf);
	/* On systems using DNS, appending a dot should force gethostbyname()
	 * to fully expand and return fqdn.  On systems using NIS, appending a
	 * dot will cause match failure.  So we try both forms, and take the
	 * longer match.
	 */
	he_nodot = gethostbyname(buf);
	strcat(buf, ".");
	he_dot = gethostbyname(buf);
	if (he_dot && he_nodot) {
	    he = (strlen(he_dot->h_name) > strlen(he_nodot->h_name)) ?
							he_dot : he_nodot ;
	} else if (he_dot) {
	    he = he_dot;
	} else if (he_nodot) {
	    he = he_nodot;
	} else {
	    return Error(0, 0,
		    "host `%s' not recognized by gethostbyname().\n", buf);
	}
	if (strlen(he->h_name) > len - 1)
	    return Error(0, 0,
	    "gethostbyname() returns `%s' which is too long for our buffer!\n",
		he->h_name);
	strcpy(buf, he->h_name);
	/* Delete trailing dot, if still present */
	if ((l=strlen(buf)) > 0 && buf[l-1] == '.')
	    buf[l-1] = '\0';
    }
#endif
    
    return 0;
}

/*
 * Downcase a null-terminated string.
 */
void
strtolower(s)
char *s;
{
    for (; *s; s++)
	if (isupper(*s))
	    *s = tolower(*s);
}


#ifndef HAVE_STRTOL
static unsigned long digit_a2d[128] = {0};

/* Initialize the character-to-digit mapping. */
static void
init_digit(void)
{
    int c;
    for (c=0; c < NELEM(digit_a2d); c++)
	digit_a2d[c] = -1;

    for (c = '0' ; c <= '9'; c++)
	digit_a2d[c] = c - '0';

    digit_a2d['a']=digit_a2d['A'] = 10; digit_a2d['b']=digit_a2d['B'] = 11;
    digit_a2d['c']=digit_a2d['C'] = 12; digit_a2d['d']=digit_a2d['D'] = 13;
    digit_a2d['e']=digit_a2d['E'] = 14; digit_a2d['f']=digit_a2d['F'] = 15;
    digit_a2d['g']=digit_a2d['G'] = 16; digit_a2d['h']=digit_a2d['H'] = 17;
    digit_a2d['i']=digit_a2d['I'] = 18; digit_a2d['j']=digit_a2d['J'] = 19;
    digit_a2d['k']=digit_a2d['K'] = 20; digit_a2d['l']=digit_a2d['L'] = 21;
    digit_a2d['m']=digit_a2d['M'] = 22; digit_a2d['n']=digit_a2d['N'] = 23;
    digit_a2d['o']=digit_a2d['O'] = 24; digit_a2d['p']=digit_a2d['P'] = 25;
    digit_a2d['q']=digit_a2d['Q'] = 26; digit_a2d['r']=digit_a2d['R'] = 27;
    digit_a2d['s']=digit_a2d['S'] = 28; digit_a2d['t']=digit_a2d['T'] = 29;
    digit_a2d['u']=digit_a2d['U'] = 30; digit_a2d['v']=digit_a2d['V'] = 31;
    digit_a2d['w']=digit_a2d['W'] = 32; digit_a2d['x']=digit_a2d['X'] = 33;
    digit_a2d['y']=digit_a2d['Y'] = 34; digit_a2d['z']=digit_a2d['Z'] = 35;
}

long
strtol(register const char *str, char **ptr, int base)
{
    long sign=1;
    long value=0;
    long digit;

    if (*digit_a2d == 0)
	init_digit();

    /* skip leading whitespace */
    while (isspace(*str))
        ++str;

    /* Check for optional sign */
    switch (*str) {
    case '-':
	sign = -1;
	str++;
	break;
    case '+':
	str++;
	break;
    }

    /* Determine base */
    if (base == 0) {
        if (*str != '0') {
            base = 10;
        } else if (*++str == 'x' || *str == 'X') {
            base = 16;
            ++str;
        } else {
            base = 8;
        }
    }

    /* Skip 0[xX], if present */
    if (base == 16 && *str == '0')
	if (*++str == 'x' || *str == 'X')
	    ++str;

    /* Convert value */
    while (*str < NELEM(digit_a2d) &&
		(digit=digit_a2d[*str]) < base && digit != -1) {
	value = value*base + digit;
	++str;
    }

    if (ptr)
	*ptr = (char *) str;
    
    return sign*value;
}
#endif

#ifndef HAVE_MEMSET
void *
memset(s, c, n)
void *s;
int c;
int n;
{
    register int i;
    register unsigned char *p = (unsigned char *) s;

    for (i=0; i<n; i++)
	*p++ = (unsigned char) c;
}
#endif

#ifdef HAVE_SYS_ERRLIST
    extern char *sys_errlist[];
    extern int sys_nerr;
#endif

/*
 * Safe strerror(): range-checks the argument.
 * (C standard doesn't guarantee any range checks.)
 */
char
*Strerror(errnum)
int errnum;
{
    static char buf[500];
#ifndef HAVE_SYS_ERRLIST
    sprintf(buf, "Error %d", errnum);
    return buf;
#else
    if (errnum < 0 || errnum > sys_nerr) {
	sprintf(buf, "Error %d (!)", errnum);
	return buf;
    } else {
#ifdef HAVE_STRERROR
	return strerror(errnum);
#else
	return sys_errlist[errnum];
#endif
    }
#endif
}

#ifndef HAVE_STRDUP
char *
strdup(s)
char *s;
{
    char *t = (char *) malloc(strlen(s)+1);
    strcpy(t, s);
    return t;
}
#endif

#ifndef HAVE_VPRINTF
/* Very system-specific.  May not work for you...
 */
void
vfprintf(stream, format, ap)
FILE *stream;
char *format;
va_list ap;
{
    _doprnt(format, ap, stream);
}

void
vsprintf(str, format, ap)
char *str;
char *format;
va_list ap;
{
    FILE fp;
    fp._cnt = 077777;
    fp._base = fp._ptr = str;
    fp._bufsiz = 0;
    fp._flag = _IOSTRG | _IOWRT;
    fp._file = 0;

    _doprnt(format, ap, &fp);
}
#ifndef HAVE_DOPRNT
ERROR -- YOU HAVE NEITHER v?printf() NOR _doprnt().
YOU ARE HOSED UNTIL YOU SUPPLY ONE OF THESE.
#endif
#endif
