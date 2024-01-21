/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#undef  _POSIX_SOURCE
#define _POSIX_SOURCE 1
#undef  _POSIX_SOURCE
#define _POSIX_SOURCE 2

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <signal.h>
#include "codes.h"
#include "knewsd.h"

#define IS_SPACE(c) ((c) == ' ' || (c) == '\t')

#undef  True
#define True  1
#undef  False
#define False 0

static int make_over_rec(int, long, FILE*);

static long	*arts = NULL;
static long	n_arts = 0;
static long	curr_art_ind = -1;
static char	*group_path = NULL;
static int	updating_overview = False;

#ifndef NEWSGROUPS_FILE
#  define NEWSGROUPS_FILE 0
#endif
#ifndef OVERVIEW_DIR
#  define OVERVIEW_DIR    0
#endif
#ifndef POSTING_AGENT
#  define POSTING_AGENT   0
#endif

static char	*spool_dir	= SPOOL_DIR;
static char	*active		= ACTIVE_FILE;
static char	*newsgroups	= NEWSGROUPS_FILE;
static char	*overview_dir	= OVERVIEW_DIR;
static char	*posting_agent  = POSTING_AGENT;
static int	update_overview	= False;
static pid_t    inews_pid;

static void sigpipe_handler(int sig)
{
    if (updating_overview &&
	(!overview_dir || chdir(overview_dir) == 0) &&
	chdir(group_path) == 0)
	unlink(".overview");

    _exit(0);
}

static void handle_sigpipe(void)
{
    struct sigaction	sig_act;

    sig_act.sa_handler = sigpipe_handler;
    sigemptyset(&sig_act.sa_mask);
    sig_act.sa_flags = 0;
    if (sigaction(SIGPIPE, &sig_act, NULL) < 0)
	perror("knews: sigaction");
}

static void ignore_sigpipe(void)
{
    struct sigaction	sig_act;

    sig_act.sa_handler = SIG_IGN;
    sigemptyset(&sig_act.sa_mask);
    sig_act.sa_flags = 0;
    if (sigaction(SIGPIPE, &sig_act, NULL) < 0)
	perror("knewsd: sigaction");
}

static void *xrealloc(void *ptr, size_t n)
{
    if (ptr) /* hack for broken C libraries */
	ptr = realloc(ptr, n);
    else
	ptr = malloc(n);

    if (!ptr) {
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " Malloc failed.\r\n");
	exit(0);
    }

    return ptr;
}

static int only_digits(char *c)
{
    while ((unsigned)(*c - '0') < 10)
	c++;

    return *c == '\0';
}

static int long_cmp(const void *c1, const void *c2)
{
    const long	*l1 = c1;
    const long	*l2 = c2;

    if (*l1 < *l2)
	return -1;
    if (*l1 > *l2)
	return 1;
    return 0;
}

static long art_search(long *arts, long n_arts, long no)
{
    long	i;

    for (i = 0 ; i < n_arts ; i++)
	if (arts[i] == no)
	    return i;

    return -1;
}

static void get_article(char *args, int head, int body)
{
    char	buffer[32];
    FILE	*fp;
    long	no;
    int		c, bol, in_head;

    if (!group_path) {
	printf(CODE_TO_STR(NNTP_ERR_NCING) " Not in a newsgroup.\r\n");
	return;
    }

    if (!args) {
	no = curr_art_ind;
	if (no < 0) {
	    printf(CODE_TO_STR(NNTP_ERR_NOCRNT) " No current article.\r\n");
	    return;
	}
    } else if (!only_digits(args)) {
	if (args[0] == '<')
	    printf(CODE_TO_STR(NNTP_ERR_FAULT)
		   " Message-id lookup not implemented.\r\n");
	else
	    printf(CODE_TO_STR(NNTP_ERR_CMDSYN) " Syntax error.\r\n");
	return;
    } else if (sscanf(args, "%ld", &no) != 1) {
	printf(CODE_TO_STR(NNTP_ERR_CMDSYN) " Syntax error.\r\n");
	return;
    } else {
	no = art_search(arts, n_arts, no);
	if (no < 0) {
	    printf(CODE_TO_STR(NNTP_ERR_NOART) " No such article.\r\n");
	    return;
	}
    }

    sprintf(buffer, "%ld", arts[no]);
    fp = fopen(buffer, "r");

    if (!fp) {
	printf(CODE_TO_STR(NNTP_ERR_NOART) " No such article.\r\n");
	return;
    }

    curr_art_ind = no;
    no = arts[no];

    if (head && body)
	printf(CODE_TO_STR(NNTP_OK_ARTICLE) " %ld article.\r\n", no);
    else if (head)
	printf(CODE_TO_STR(NNTP_OK_HEAD) " %ld head.\r\n", no);
    else
	printf(CODE_TO_STR(NNTP_OK_BODY) " %ld body.\r\n", no);

    bol = True;
    in_head = True;

    while ((c = getc(fp)) != EOF) {
	if (in_head && bol && c == '\n') {
	    in_head = False;
	    if (!body)
		break;
	    if (!head)
		continue;
	}

	if (!(in_head ? head : body))
	    bol = (c == '\n');
	else {
	    if (c == '\n') {
		bol = True;
		putchar('\r');
	    } else {
		if (bol && c == '.')
		    putchar('.');
		bol = False;
	    }

	    putchar(c);
	}
    }

    if (bol)
	printf(".\r\n");
    else
	printf("\r\n.\r\n");

    fclose(fp);
}

static void kill_inews(char *msg)
{
    if (inews_pid == 0)
	return;

    fprintf(stderr,
	    "knewsd: %s\n        Killing '%s' just to make sure...   ",
	    msg, posting_agent);

    if (kill(inews_pid, SIGKILL) < 0)  /* kill that sucker */
	perror("kill");
    else
	fprintf(stderr, "Got the sucker!\n");

    inews_pid = 0;
}

static int get_char(void)
{
    int ch = getchar();

    if (ch == '\r')
	ch = getchar();

    if (ch == EOF) {
	kill_inews("Unexpected end-of-file while posting!");
	_exit(1);
    }

    return ch;
}

/************************************************************************/

static void do_article(char *args)
{
    get_article(args, True, True);
}

static void do_body(char *args)
{
    get_article(args, False, True);
}

static void do_group(char *args)
{
    char		path[1024];
    DIR			*dir;
    char		*p;
    long		n_alloc;
    struct dirent	*dp;

    curr_art_ind = -1;

    if (!args) {
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " No newsgroup specified.\r\n");
	return;
    }

    if (strlen(args) > 512) {
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " Group name too long.\r\n");
	return;
    }

    strcpy(path, spool_dir);
    p = path + strlen(path);
    *p++ = '/';
    strcpy(p, args);
    while (*p != '\0')
	if (*p == '.')
	    *p++ = '/';
	else
	    p++;

    if (chdir(path) < 0 || !(dir = opendir("."))) {
	perror(path);
	printf(CODE_TO_STR(NNTP_ERR_NOGROUP) " No such newsgroup.\r\n");
	return;
    }

    n_alloc = n_arts;
    group_path = xrealloc(group_path, strlen(args) + 1);
    strcpy(group_path, args);
    for (p = strchr(group_path, '.') ; p ; p = strchr(p + 1, '.'))
	*p = '/';

    n_arts = 0;
    while ( (dp = readdir(dir)) )
	if (only_digits(dp->d_name)) {
	    if (n_arts > n_alloc - 2) {
		n_alloc = 2 * (n_alloc + 1);
		arts = xrealloc(arts, n_alloc * sizeof(long));
	    }

	    arts[n_arts++] = atol(dp->d_name);
	}

    closedir(dir);

    if (n_arts > 0) {
	qsort(arts, n_arts, sizeof(long), long_cmp);
	curr_art_ind = 0;
    }

    printf(CODE_TO_STR(NNTP_OK_GROUP) " %ld %ld %ld.\r\n",
	   n_arts + 7,
	   n_arts > 0 ? arts[0] : 0l,
	   n_arts > 0 ? arts[n_arts - 1] : 0l);
}

static void do_head(char *args)
{
    get_article(args, True, False);
}

static void do_help(char *args)
{
    printf(CODE_TO_STR(NNTP_INF_HELP) " Legal commands\r\n"
	   "  article [Number]\r\n"
	   "  body [Number]\r\n"
	   "  group newsgroup\r\n"
	   "  head [Number]\r\n"
	   "  help\r\n"
	   "  list [active|newsgroups]\r\n"
	   "  next\r\n"
	   "  post\r\n"
	   "  quit\r\n"
	   "  stat [Number]\r\n"
	   "  xover [range]\r\n"
	   ".\r\n");
}

static void do_list(char *args)
{
    FILE	*fp = NULL;
    int		c, prev;

    if (!args || strcmp(args, "active") == 0) {
	if (!active) {
	    printf(CODE_TO_STR(NNTP_ERR_FAULT)
		   " Active file not available.\r\n");
	    return;
	}

	fp = fopen(active, "r");
	if (fp)
	    printf(CODE_TO_STR(NNTP_OK_GROUPS) " Active file follows.\r\n");
	else
	    perror(active);
    } else if (strcmp(args, "newsgroups") == 0) {
	if (!newsgroups) {
	    printf(CODE_TO_STR(NNTP_ERR_FAULT)
		   " Newsgroups file not available.\r\n");
	    return;
	}

	fp = fopen(newsgroups, "r");
	if (fp)
	    printf(CODE_TO_STR(NNTP_OK_GROUPS) " Descriptions follow.\r\n");
	else
	    perror(newsgroups);
    } else {
	printf(CODE_TO_STR(NNTP_ERR_CMDSYN) " Syntax error.\r\n");
	return;
    }


    if (!fp) {
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " Couldn't open file.\r\n");
	return;
    }

    prev = 0;
    while ((c = getc(fp)) != EOF) {
	prev = c;
	if (c == '\n')
	    putchar('\r');
	putchar(c);
    }

    if (prev == '\n')
	printf(".\r\n");
    else
	printf("\r\n.\r\n");

    fclose(fp);
}

static void do_next(char *args)
{
    if (!group_path) {
	printf(CODE_TO_STR(NNTP_ERR_NCING) " Not in a newsgroup.\r\n");
	return;
    }

    if (curr_art_ind < 0) {
	printf(CODE_TO_STR(NNTP_ERR_NOCRNT) " No current article.\r\n");
	return;
    }

    if (curr_art_ind >= n_arts - 1) {
	printf(CODE_TO_STR(NNTP_ERR_NONEXT) " No next article.\r\n");
	return;
    }

    curr_art_ind++;
    printf(CODE_TO_STR(NNTP_OK_NOTEXT) " %ld\r\n", arts[curr_art_ind]);
}

static void do_not_impl(char *args)
{
    printf(CODE_TO_STR(NNTP_ERR_FAULT) " Command not implemented.\r\n");
}

static void do_post(char *args)
{
    int	fd[2];
    FILE	*fp;
    int		bol;
    int		status, temp;

    if (!posting_agent) {
	printf(CODE_TO_STR(NNTP_ERR_NOPOST)
	       " knewsd not configured for posting.\r\n");
	return;
    }

    if (pipe(fd) < 0) {
	perror("knewsd: pipe");
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " pipe failed.\r\n");
	return;
    }

    inews_pid = fork();
    if (inews_pid < 0) {
	perror("knewsd: fork");
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " fork failed.\r\n");
	close(fd[0]);
	close(fd[1]);
	return;
    }

    if (inews_pid == 0) { /* child */
	close(fd[1]);
	if (fd[0] != STDIN_FILENO) {
	    if (dup2(fd[0], STDIN_FILENO) != STDIN_FILENO) {
		perror("knewsd: dup2");
		_exit(126);
	    }
	    close(fd[0]);
	}
	if (dup2(STDOUT_FILENO, STDERR_FILENO) != STDERR_FILENO) {
	    perror("knews: dup2");
	    _exit(126);
	}

	execl("/bin/sh", "sh", "-c", posting_agent, (char *)0);
	perror("knewsd: execl");
	_exit(127);
    }

    /* parent */
    close(fd[0]);
    fp = fdopen(fd[1], "w");
    if (!fp) {
	perror("knewsd: fdopen");
	close(fd[1]);
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " Internal error.\r\n");
	kill_inews("Internal error while posting!");
	return;
    }

    printf(CODE_TO_STR(NNTP_CONT_POST) " Ok.\r\n");
    fflush(stdout);
    ignore_sigpipe();

    bol = True;
    for (;;) {
	int	ch = get_char();

	if (bol && ch == '.') {
	    ch = get_char();
	    if (ch == '\n')
		break;
	    putc('.', fp);
	}

	putc(ch, fp);
	bol = (ch == '\n');
    }
    fclose(fp);

    handle_sigpipe();

    do {
	temp = waitpid(inews_pid, &status, 0);
    } while (temp < 0 && errno == EINTR);

    if (temp < 0) {
	perror("knewsd: waitpid");
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " Internal error.\r\n");
	kill_inews("Internal error while posting!");
	return;
    }

    if (WIFEXITED(status))
	switch (WEXITSTATUS(status)) {
	case 0:
	    printf(CODE_TO_STR(NNTP_OK_POSTED) " Article posted.\r\n");
	    break;
	case 126:
	    printf(CODE_TO_STR(NNTP_ERR_POSTFAIL) " Internal error.\r\n");
	    break;
	case 127:
	    printf(CODE_TO_STR(NNTP_ERR_POSTFAIL) " Failed to start %s.\r\n",
		   posting_agent);
	    break;
	default:
	    printf(CODE_TO_STR(NNTP_ERR_POSTFAIL)
		   " %s didn't accept your article.\r\n",
		   posting_agent);
	    break;
	}
    else if (WIFSIGNALED(status))
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " %s caught signal %d.\r\n",
	       posting_agent, WTERMSIG(status));
    else
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " %s terminated abnormally: "
	       "beats the hell out of me.\r\n", posting_agent);
}

static void do_stat(char *args)
{
    long	no;

    if (!group_path) {
	printf(CODE_TO_STR(NNTP_ERR_NCING) " Not in a newsgroup.\r\n");
	return;
    }

    if (!args || sscanf(args, "%ld", &no) != 1) {
	printf(CODE_TO_STR(NNTP_ERR_CMDSYN) " Syntax error.\r\n");
	return;
    }

    no = art_search(arts, n_arts, no);
    if (no < 0) {
	printf(CODE_TO_STR(NNTP_ERR_NOART) " No such article.\r\n");
	return;
    }

    curr_art_ind = no;
    printf(CODE_TO_STR(NNTP_OK_NOTEXT) " %ld\r\n", arts[no]);
}

static void do_quit(char *args)
{
    printf(CODE_TO_STR(NNTP_OK_GOODBYE) " Goodbye.\r\n");
    exit(0);
}

static void do_xover(char *args)
{
    char	buffer[1024];
    FILE	*fp = NULL;
    long	first, last, no;
    char	*c;

    if (!group_path) {
	printf(CODE_TO_STR(NNTP_ERR_NCING) " Not in a newsgroup.\r\n");
	return;
    }

    if (!args) {
	printf(CODE_TO_STR(NNTP_ERR_NOCRNT)
	       " XOVER can't handle current article.\r\n");
	return;
    }

    c = strchr(args, '-');
    if (c) {
	*c++ = '\0';
	if (sscanf(args, "%ld", &first) != 1 || sscanf(c, "%ld", &last) != 1) {
	    printf(CODE_TO_STR(NNTP_ERR_CMDSYN) " Syntax error.\r\n");
	    return;
	}
    } else {
	if (sscanf(args, "%ld", &first) != 1) {
	    printf(CODE_TO_STR(NNTP_ERR_CMDSYN) " Syntax error.\r\n");
	    return;
	}
	last = first;
    }

    if (overview_dir)
	sprintf(buffer, "%s/%s/.overview",  overview_dir, group_path);
    else
	sprintf(buffer, ".overview");

    fp = fopen(buffer, "r");
    if (!fp)
	perror(buffer);

    printf(CODE_TO_STR(NNTP_OK_XOVER) " Overview file follows.\r\n");

    no = 0;
    if (fp) {
	while (fgets(buffer, sizeof(buffer), fp)) {
	    if (buffer[0] < '0' || buffer[0] > '9' ||
		(no = atol(buffer)) < first) {
		if (!strchr(buffer, '\n')) {
		    int	ch;

		    do {
			ch = getc(fp);
		    } while (ch != EOF && ch != '\n');

		    if (ch == EOF)
			break;
		}
		continue;
	    }
	    if (no > last)
		break;

	    fputs(buffer, stdout);
	    if (!strchr(buffer, '\n')) {
		int	ch;

		while ((ch = getc(fp)) != EOF && ch != '\n')
		    putchar(ch);

		printf("\r\n");
	    }
	}

	fclose(fp);
	fp = NULL;
    }

    if (no < last) {
	long	n = 0;

	if (no < first)
	    no = first;
	else if (update_overview) {
	    if (overview_dir)
		sprintf(buffer, "%s/%s/.overview", overview_dir, group_path);
	    else
		sprintf(buffer, ".overview");

	    fp = fopen(buffer, "a");
	    if (fp)
		updating_overview = True;
	    else
		perror(".overview");
	}

	while (n < n_arts && arts[n] < no)
	    n++;

	while (n < n_arts && arts[n] <= last) {
	    int	fd;

	    sprintf(buffer, "%ld", arts[n]);
	    fd = open(buffer, O_RDONLY);
	    if (fd < 0)
		perror("open");
	    else {
		make_over_rec(fd, arts[n], fp);
		close(fd);
	    }

	    n++;
	}

	updating_overview = False;
	if (fp)
	    fclose(fp);
    }

    printf(".\r\n");
}

/*************************************************************************/

int main(int argc, char **argv)
{
#define N_COMMANDS	(sizeof commands / sizeof commands[0])
    static struct {
	char	*name;
	void	(*proc)(char*);
    } commands[] = {
	{"article",	do_article},
	{"authinfo",	do_not_impl},
	{"body",	do_body},
	{"date",	do_not_impl},
	{"group",	do_group},
	{"head",	do_head},
	{"help",	do_help},
	{"ihave",	do_not_impl},
	{"last",	do_not_impl},
	{"list",	do_list},
	{"listgroup",	do_not_impl},
	{"next",	do_next},
	{"newgroups",	do_not_impl},
	{"newnews",	do_not_impl},
	{"post",	do_post},
	{"quit",	do_quit},
	{"sendme",	do_not_impl},
	{"slave",	do_not_impl},
	{"stat",	do_stat},
	{"xgtitle",	do_not_impl},
	{"xhdr",	do_not_impl},
	{"xover",	do_xover},
	{"xpat",	do_not_impl},
	{"xpath",	do_not_impl},
    };
    char	command[512];

    handle_sigpipe();

    while (--argc > 0 && **++argv == '-') {
	int	n = strlen(*argv) - 1;

#define IS_OPTION(o, has_arg)             \
	(strncmp(*argv, o, n) == 0 &&     \
	 n < sizeof(o) && (!has_arg || argc > 0))

	switch (argv[0][1]) {
	case 's':
	    if (IS_OPTION("-spool", True)) {
		spool_dir = *++argv;
		argc--;
		continue;
	    }
	    break;
	case 'a':
	    if (IS_OPTION("-active", True)) {
		active = *++argv;
		argc--;
		continue;
	    }
	    break;
	case 'n':
	    if (IS_OPTION("-newsgroups", True)) {
		newsgroups = *++argv;
		argc--;
		continue;
	    }
	    break;
	case 'o':
	    if (IS_OPTION("-overview", True)) {
		overview_dir = *++argv;
		argc--;
		continue;
	    }
	    break;
	case 'p':
	    if (IS_OPTION("-postingagent", True)) {
		posting_agent = *++argv;
		argc--;
		continue;
	    }
	    break;
	case 'u':
	    if (IS_OPTION("-update", False)) {
		update_overview = True;
		continue;
	    }
	    break;
	default:
	    break;
	}
#undef IS_OPTION
	break;
    }

    if (argc != 0) {
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " Bad command line arguments.\r\n");
	exit(1);
    }

    if (strlen(spool_dir) > 480) {
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " Spool dir name to long.\r\n");
	exit(1);
    }

    if (overview_dir && strlen(overview_dir) > 480) {
	printf(CODE_TO_STR(NNTP_ERR_FAULT) " Overview dir name to long.\r\n");
	exit(1);
    }

    if (chdir(spool_dir) < 0) {
	perror(spool_dir);
	printf(CODE_TO_STR(NNTP_ERR_FAULT)
	       " Couldn't chdir to spool dir.\r\n");
	exit(1);
    }

    printf("%d knews, copyright 1995, 1996 Karl-Johan Johnsson, %s.\r\n",
	   posting_agent ? NNTP_OK_CANPOST : NNTP_OK_NOPOST,
	   posting_agent ? "posting ok" : "no posting");
	   
    if (posting_agent && !strstr(posting_agent, "exec "))
	fprintf(stderr, "Warning: posting_agent should be execed!\n");

    for (;;) {
	char	*c, *arg;
	int	i;

	fflush(stdout);
	if (!fgets(command, sizeof command, stdin))
	    break;

	c = strchr(command, '\n');
	if (c) {
	    *c = '\0';
	    if (*--c == '\r')
		*c = '\0';
	} else {
	    int	ch;

	    printf(CODE_TO_STR(NNTP_ERR_FAULT) " Input buffer overflow.\r\n");
	    do {
		ch = getchar();
	    } while (ch != EOF && ch != '\n');
	    if (ch == EOF)
		break;
	    else
		continue;
	}

	for (c = command, arg = NULL ; *c != '\0' ; c++)
	    if (*c >= 'A' && *c <= 'Z')
		*c -= 'A' - 'a';
	    else if (*c == ' ') {
		*c = '\0';
		if (!arg)
		    arg = c + 1;
	    }

	for (i = 0 ; i < N_COMMANDS ; i++)
	    if (command[0] == commands[i].name[0] &&
		strcmp(command, commands[i].name) == 0) {
		commands[i].proc(arg);
		break;
	    }

	if (i == N_COMMANDS)
	    printf(CODE_TO_STR(NNTP_ERR_COMMAND)
		   " Command not recognized.\r\n");
    }

    return 0;
}

/*************************************************************************/

static char *find_lflf(char *c)
{
    for (c = strchr(c, '\n') ; c ; c = strchr(c, '\n'))
	if (*++c == '\n')
	    return c;

    return NULL;
}

static char *get_header(int fd)
{
    static char	buffer[16384 + 3];
    char	*c, *p;
    long	n;

    n = sizeof(buffer);
    c = buffer;

    do {
	long	i = read(fd, c, 1024);

	if (i <= 0) {
	    if (i < 0)
		perror("read");
	    return NULL;
	}

	c[i] = '\0';
	c += i;
	n -= i;

	if ( (p = find_lflf(buffer)) ) {
	    *p = '\0';
	    return buffer;
	}
    } while (n > 0);

    return NULL;
}

static char *hack_inreplyto(char *inreplyto)
{
    char	*c = strchr(inreplyto, '<');
    char	*p = c;

    if (!c)
	return NULL;

    for (;;)
	switch (*p++) {
	case '>':
	    *p = '\0';
	    return c;
	case '\0':
	case '\t':
	case ' ':
	    return NULL;
	}

    /* not reached */
}

static char *stat_lines(int fd)
{
    static char	result[32];
    struct stat	st;

    if (fstat(fd, &st) < 0)
	return "";

    sprintf(result, "%ld", (long)st.st_size);

    return result;
}

#define ISUPPER(c)  \
((unsigned int)((c) - 'A') <= 'Z' - 'A')
#define LOWER(u)    \
((unsigned char)(u) + ('a' - 'A'))
#define TOLOWER(c)  \
(ISUPPER(c) ? LOWER(c) : (unsigned char)(c))

static int case_lstrncmp(const char *c1, const char *c2, long n)
{
    while (n--) {
	int	tmp = TOLOWER(*c1) - (unsigned char)*c2;

	if (tmp != 0)
	    return tmp;

	if (*c1 == '\0')
	    return 0;

	c1++;
	c2++;
    }

    return 0;
}

static int make_over_rec(int fd, long no, FILE *fp)
{
    register char	*header;
    char	*subject = "";
    char	*from = "";
    char	*date = "";
    char	*messageid = NULL;
    char	*refs = NULL;
    char	*bytes = NULL;
    char	*lines = "";
    char	*xref = NULL;
    char	*inreplyto = NULL;

    header = get_header(fd);
    if (!header)
	return -1;

    while (*header != '\0') {
	unsigned char	ch = *header;
	int		skip = True;

	switch (TOLOWER(ch)) {
	case 'b':
	    if (case_lstrncmp(header, "bytes:", 6) == 0) {
		header += 6;
		while (IS_SPACE(*header))
		    header++;
		bytes = header;
		skip = False;
	    }
	    break;
	case 'd':
	    if (case_lstrncmp(header, "date:", 5) == 0) {
		header += 5;
		while (IS_SPACE(*header))
		    header++;
		date = header;
		skip = False;
	    }
	    break;
	case 'f':
	    if (case_lstrncmp(header, "from:", 5) == 0) {
		header += 5;
		while (IS_SPACE(*header))
		    header++;
		from = header;
		skip = False;
	    }
	    break;
	case 'i':
	    if (case_lstrncmp(header, "in-reply-to:", 12) == 0) {
		header += 12;
		while (IS_SPACE(*header))
		    header++;
		inreplyto = header;
		skip = False;
	    }
	    break;
	case 'l':
	    if (case_lstrncmp(header, "lines:", 6) == 0) {
		header += 6;
		while (IS_SPACE(*header))
		    header++;
		lines = header;
		skip = False;
	    }
	    break;
	case 'm':
	    if (case_lstrncmp(header, "message-id:", 11) == 0) {
		header += 11;
		while (IS_SPACE(*header))
		    header++;
		messageid = header;
		skip =  False;
	    }
	    break;
	case 'r':
	    if (case_lstrncmp(header, "references:", 11) == 0) {
		header += 11;
		while (IS_SPACE(*header))
		    header++;
		refs = header;
		skip = False;
	    }
	    break;
	case 's':
	    if (case_lstrncmp(header, "subject:", 8) == 0) {
		header += 8;
		while (IS_SPACE(*header))
		    header++;
		subject = header;
		skip = False;
	    }
	    break;
	case 'x':
	    if (case_lstrncmp(header, "xref:", 5) == 0) {
		xref = header;
		skip = False;
	    }
	    break;
	}

	if (skip) {
	    header = strchr(header, '\n');
	    while (header && IS_SPACE(header[1]))
		header = strchr(header + 1, '\n');
	    if (!header)
		break;
	    header++;
	} else {
	    char	*src, *dest;

	    src = strchr(header, '\n');
	    if (!src)
		break;
	    dest = src;

	    for (;;) {
		if (*src == '\0')
		    break;
		else if (*src == '\n') {
		    src++;
		    if (!IS_SPACE(*src))
			break;
		    *dest++ = ' ';
		} else if (*src == '\t') {
		    src++;
		    *dest++ = ' ';
		} else {
		    *dest++ = *src++;
		}
	    }

	    *dest = '\0';
	    header = src;
	}
    }

    if (!messageid) {
	fprintf(stderr, "%ld: no message-id, skipping.\n", no);
	return -1;
    }
    if (!refs) {
	if (inreplyto)
	    refs = hack_inreplyto(inreplyto);
	else
	    refs = "";
    }
    if (!bytes)
	bytes = stat_lines(fd);

    printf("%ld\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
	   no, subject, from, date, messageid, refs, bytes, lines);
    if (xref)
	printf("\t%s", xref);
    putchar('\n');

    if (fp) {
	fprintf(fp, "%ld\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
		no, subject, from, date, messageid, refs, bytes, lines);
	if (xref)
	    fprintf(fp, "\t%s", xref);
	putc('\n', fp);
    }

    return 0;
}
