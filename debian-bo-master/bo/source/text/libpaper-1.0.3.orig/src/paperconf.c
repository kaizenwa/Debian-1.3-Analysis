
#include <sys/param.h>

#include <ctype.h>
#include <getopt.h>
#include <stdio.h>
#include <string.h>

#include <paper.h>

#if __STDC__ - 0 == 0
#define const
#define void
#endif

#if __STDC__ - 0 == 0
static usage(name)
    char* name;
#else
static void usage(const char* name)
#endif
{
    fprintf(stderr,
	"usage: %s [ [ -p ] papername | -d | -a ] [ -z ] [ -n | N ] [ -s ] [ -w ] [ -h ]\n",
	    name);
    exit(1);
}

#define OPT_NAME	1
#define OPT_UPPERNAME	2
#define OPT_SIZE	4
#define OPT_WIDTH	8
#define OPT_HEIGHT     16

#define OPT_CONTINUE  128

#if __STDC__ - 0 == 0
static printinfo(paper, options)
    struct paper* paper;
    int options
{
#else
static void printinfo(const struct paper* paper, int options)
{
#endif
    int pr = 0;

    if ((options & ~(OPT_CONTINUE)) == 0) {
	options = OPT_NAME;
    }

    if (options & OPT_NAME) {
	printf("%s", papername(paper));
	pr = 1;
    } else if (options & OPT_UPPERNAME) {
	if (islower(*papername(paper))) {
	    printf("%c%s", toupper(*papername(paper)), papername(paper) + 1);
	} else {
	    printf("%s", papername(paper));
	}
	pr = 1;
    }

    if (options & OPT_SIZE) {
	if (pr) putchar(' ');
	printf("%u %u", (unsigned) paperpswidth(paper),
	    (unsigned) paperpsheight(paper));
	pr = 1;
    }
    if (options & OPT_WIDTH) {
	if (pr) putchar(' ');
        printf("%u", (unsigned) paperpswidth(paper));
	pr = 1;
    }
    if (options & OPT_HEIGHT) {
	if (pr) putchar(' ');
        printf("%u", (unsigned) paperpsheight(paper));
	pr = 1;
    }

    putchar('\n');
}

#if __STDC__ - 0 == 0
int main(argc, argv)
    int argc;
    char** argv;
#else
int main(int argc, char** argv)
#endif
{
    int c;

    int all = 0;
    const char* paper = 0;
    unsigned options = 0;

    const char* progname;

#if __STDC__ - 0 == 0
    for (progname = *argv + strlen(*argv) - 1; progname != *argv
	&& *progname != '/'; --progname);
    if (*progname == '/') {
	++progname;
    }
#else
    progname = strrchr(*argv, '/');
    if (progname) {
	++progname;
    } else {
	progname = *argv;
    }
#endif

    while ((c = getopt(argc, argv, "adznNswhp:")) != EOF) {
	switch (c) {
	    case 'a':
		if (paper) {
		    usage(progname);
		}
		all = 1;
		break;

	    case 'd':
		if (paper) {
		    usage(progname);
		}
		paper = defaultpapername();
		break;

	    case 'z':
		options |= OPT_CONTINUE;
		break;

	    case 'n':
		if (options & OPT_UPPERNAME) usage(progname);
		options |= OPT_NAME;
		break;

	    case 'N':
		if (options & OPT_NAME) usage(progname);
		options |= OPT_UPPERNAME;
		break;

	    case 's':
		options |= OPT_SIZE;
		break;

	    case 'w':
		options |= OPT_WIDTH;
		break;

	    case 'h':
		options |= OPT_HEIGHT;
		break;

	    case 'p':
		if (all) {
		    usage(progname);
		}
		paper = optarg;
		break;

	    default:
		usage(progname);
	}
    }

    if (optind < argc - 1 || (paper && optind != argc)) {
	usage(progname);
    } else if (optind != argc) {
	paper = argv[optind];
    }

    paperinit();

    if (all) {
 	const struct paper* papers;

	for (papers = paperfirst(); papers; papers = papernext(papers)) {
	    printinfo(papers, options);
	}
    } else {
        const struct paper* syspaper;

        if (!paper) paper = systempapername();
	if (!paper) {
	    extern int errno;

	    char errmsg[2 * MAXPATHLEN + 64];

	    sprintf(errmsg, "%s: cannot get paper size from %s",
		progname, systempapersizefile());

	    if (errno) {
		perror(errmsg);
	    } else {
	        fputs(errmsg, stderr);
	    }

	    paperdone();

	    exit(3);
	}

        syspaper = paperinfo(paper);

        if (syspaper) {
            printinfo(syspaper, options);
        } else {
	    fprintf(stderr, "%s: unknown paper `%s'\n", progname, paper);
	    if (options & OPT_CONTINUE) {
		puts(paper);
	    }

	    paperdone();

	    exit(2);
        }
    }

    paperdone();

    return 0;
}

