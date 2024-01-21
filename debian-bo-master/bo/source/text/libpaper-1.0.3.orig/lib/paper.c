
/*
 * Copyright (C) Yves Arrouye <Yves.Arrouye@marin.fdn.fr>, 1996.
 *
 * Use under the GPL version 2. You are not allowed to remove this
 * copyright notice.
 *
 */

#include <sys/stat.h>

#include <stdio.h>
#include <malloc.h>

#include <unistd.h>

#ifdef __STDC__
#include <stdlib.h>
#else
extern double atof();
extern char* getenv();
#endif

#include "paper.h"

struct paper {
    const char* name;
    double pswidth, psheight;
};

#if __STDC__ - 0 == 0
#define const
#define void
#endif

#ifndef PAPERSIZEVAR
#define PAPERSIZEVAR	"PAPERSIZE"
#endif

#ifndef PAPERSIZE
#define PAPERSIZE	"/etc/papersize"
#endif

#ifndef PAPERVAR
#define PAPERVAR	"PAPERCONF"
#endif

/* The following choice should be in the papers[] array. */

#ifndef PAPERCONF
#define PAPERCONF	"letter"
#endif

/* This table will be searched in order: please put typical paper sizes
   at the beginning of it. */

/* The paper names have been got from gs 3.68 gs_stadt.ps file, with
   some personal additions. */

static struct paper papers[] = {
#include "paperspecs.h"
    { 0 }
};

int paperinit(void) {
    return 0;
}

int paperdone(void) {
    return 0;
}

#if __STDC__ - 0 == 0
char* papername(spaper)
    struct paper* spaper;
#else
const char* papername(const struct paper* spaper)
#endif
{
    return spaper->name;
}

#if __STDC__ - 0 == 0
double paperpswidth(spaper)
    struct paper* spaper;
#else
double paperpswidth(const struct paper* spaper)
#endif
{
    return spaper->pswidth;
}

#if __STDC__ - 0 == 0
double paperpsheight(spaper)
    struct paper* spaper;
#else
double paperpsheight(const struct paper* spaper)
#endif
{
    return spaper->psheight;
}

const struct paper* paperfirst(void) {
    return papers;
}

const struct paper* paperlast(void) {
    static const struct paper* lastpaper = 0;
   
    const struct paper* next = papers;
    while (next->name) {
	lastpaper = next, ++next;
    }

    return lastpaper;
}

#if __STDC__ - 0 == 0
struct paper* papernext(spaper)
    struct paper* spaper;
#else
const struct paper* papernext(const struct paper* spaper)
#endif
{
    return (++spaper)->name ? spaper : 0;
}

#if __STDC__ - 0 == 0
struct paper* paperprev(spaper)
    struct paper* spaper;
#else
const struct paper* paperprev(const struct paper* spaper)
#endif
{
    return spaper == papers ? 0 : --spaper;
}

const char* defaultpapersizefile(void) {
    return PAPERSIZE;
}

const char* systempapersizefile(void) {
    const char* papersize = getenv(PAPERSIZEVAR);
    return papersize ? papersize : defaultpapersizefile();
}

const char* defaultpapername(void) {
    return PAPERCONF;
}

char* systempapername(void) {
    char* paper = (char*) getenv(PAPERVAR);
   
    if (!paper) {
	extern int errno;

	FILE* ps;
	const char* papersize = getenv(PAPERSIZEVAR);
	struct stat statbuf;

	if (papersize && stat(papersize, &statbuf) == -1) {
	    return 0;
	}

	if (!papersize) papersize = PAPERSIZE;

	errno = 0;

	if (stat(papersize, &statbuf) == -1) {
	    const char* pap = defaultpapername();
	    char* papstr;

	    papstr = malloc(sizeof(pap) + sizeof(char));
	    return papstr ? strcpy(papstr, pap) : 0;
	}

	if ((ps = fopen(papersize, "r")) != 0) {
	    int c;
	    while ((c = getc(ps)) != EOF) {
		if (c == '#') {
		    while ((c = getc(ps)) != EOF && c != '\n');
		    if (c == EOF) {
			break;
		    }
		} else if (!isspace(c)) {
		    unsigned n = 0, m = 64;
		    char* papername = malloc(m * sizeof(char));

		    if (!papername) break;

		    do {
			if (n == m) {
			    char* newpaper = realloc(papername,
				(m *= 2) * sizeof(char));
			    if (!newpaper) {
				free(papername);
				return 0;
			    }
			}
			papername[n++] = c;
		    } while ((c = getc(ps)) != EOF && !isspace(c));

		    papername[n] = 0;

		    if ((paper = malloc((strlen(papername) + 1)
			* sizeof(char))) != 0) {
			strcpy(paper, papername);
		    }

		    free(papername);

		    break;
		}
	    }
	    fclose(ps);
	}
    } else {
	char* papstr = malloc((strlen(paper) + 1) * sizeof(char));
	if (papstr) {
	    return strcpy(papstr, paper);
	}
    }

    return paper;
}

#if __STDC__ - 0 == 0
struct paper* papeinfo(paper)
    char* paper;
#else
const struct paper* paperinfo(const char* paper)
#endif
{
    const struct paper* pp;

    for (pp = paperfirst(); pp; pp = papernext(pp)) {
	if (!strcasecmp(pp->name, paper)) {
	    return pp;
	}
    }

    return 0;
}

#if __STDC__ - 0 == 0
struct paper* paperwithsize(pswidth, psheight)
    double pswidth;
    double psheight;
#else
const struct paper* paperwithsize(double pswidth, double psheight)
#endif
{
    const struct paper* pp;

    for (pp = paperfirst(); pp; pp = papernext(pp)) {
	if (pp->pswidth == pswidth
	    && pp->psheight == psheight) {
	    return pp;
	}
    }

    return 0;
}

