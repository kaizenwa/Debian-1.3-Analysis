
/*
 * gsfrontend.c: a Ghostscript frontend using the libpaper.
 *
 * Copyright (C) 1996, Yves Arrouye <Yves.Arrouye@marin.fdn.fr>
 *
 * This file can be redistributed under the terms of the GNU General
 * Public License (GPL) version 2.
 *
 */

#ifndef GS
#define GS "ghostscript"	/* Or "gs4.01" or whatever... */
#endif

/*
 * To use this frontend, rename your Ghostscript to the value of the GS
 * define above and  compile this file as gs or whatever name you want
 * to use for the Ghostscript interpreter.
 *
 * Note that the GS environment variable will override the compiled
 * default name.
 *
 */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <malloc.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <paper.h>

#ifndef ARGS_GROWTH
#define ARGS_GROWTH 16
#endif

const char* gspapersizearg = "-sPAGESIZE=";

void error(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);

    vfprintf(stderr, fmt, args);
    if (errno > 0 && errno < sys_nerr) {
	fprintf(stderr, ": %s", _sys_errlist[errno]);
    }
    fputc('\n', stderr);

    va_end(args);

    exit(errno ? errno : -1);
}

int runprog(const char* myname, const char* progname, char** progargv) {
    if (execvp(progname, progargv) == -1) {
	error("%s: cannot execute %s", myname, progname);
    }

    return 0;
}

const char* addarg(const char* arg, char*** argv, int* argc, int* margc) {
    if (*argc >= *margc) {
	int growth = *argc - *margc;

	if (growth < ARGS_GROWTH) growth = ARGS_GROWTH;

	*argv = realloc(*argv, (*margc += growth)
	    * sizeof(char*));
    }

    if (*argv) {
	if (arg) {
	    if (((*argv)[*argc] = malloc(strlen(arg) + 1))) {
		strcpy((*argv)[(*argc)++], arg);
	    } else {
		return 0;
	    }
	} else {
	    (*argv)[(*argc)++] = 0;
	}
    }

    return arg;
}

int addpaperarg(const char* paper, char*** argv, int* argc, int* margc) {
    const struct paper* papinfo = paperinfo(paper);

    if (papinfo) {
	char parg[32];

	sprintf(parg, "-dDEVICEWIDTHPOINTS=%d", (int) paperpswidth(papinfo));
	addarg(parg, argv, argc, margc);
	sprintf(parg, "-dDEVICEHEIGHTPOINTS=%d", (int) paperpsheight(papinfo));
	addarg(parg, argv, argc, margc);

	return 2;
    } else if (paper) {
	char* parg = malloc(strlen(gspapersizearg) + strlen(paper) + 1);

	if (parg) {
	    strcat(strcpy(parg, gspapersizearg), paper);
	    addarg(parg, argv, argc, margc);
	    free(parg);

	    return 1;
	}
    }

    return 0;
}

main(int argc, char** argv) {
    char** nargv;
    int nargc = 0, mnargc = argc + 3;

    int help = 0;
    int size = 0;

    int pcount = 0;

    int gspapersizearglen = strlen(gspapersizearg);

    char* gs;
    char* gsname;

    char* progname = strrchr(*argv, '/');

    if (progname) {
	++progname;
    } else {
	progname = *argv;
    }

    if ((nargv = malloc(mnargc * sizeof(char*))) == 0) {
	error("%s: cannot copy arguments", progname);
    }

    paperinit();

    if ((gs = getenv("GS")) == 0) {
	gs = GS;
    }

    if ((gsname = strrchr(gs, '/'))) {
	++gsname;
    } else {
	gsname = gs;
    }
    
    addarg(gsname, &nargv, &nargc, &mnargc);
    pcount = addpaperarg(systempapername(), &nargv, &nargc, &mnargc);

    for (++argv; *argv; ++argv) {
	if (!strcmp(*argv, "-h") || !strcmp(*argv, "--help")) {
	    help = 1;
	} else if (!strcmp(*argv, "--nopapersize")) {
	    size = 1;
	} else if (!strncmp(*argv, gspapersizearg, gspapersizearglen)) {
	    size = 1;
	    addpaperarg(*argv + gspapersizearglen, &nargv, &nargc, &mnargc);
	    continue;
	} else if (!strncmp(*argv, "-g", 2)) {
	    size =1 ;
	}

	addarg(*argv, &nargv, &nargc, &mnargc);
    }

    addarg(0, &nargv, &nargc, &mnargc);

    paperdone();

    if (size && pcount) {
	int i;

	for (i = 0; i < pcount; ++i) {
	    free(nargv[i + 1]);
	}

	nargv[pcount] = *nargv;
	nargv += pcount;
    }

    if (help) {
	switch (fork()) {
	    case 0:
		runprog(progname, gs, nargv);
		break;

	    case -1:
		error("%s: cannot fork", progname);
		break;

	    default: {
		int status;

		wait(&status);
		printf("This is a Ghostscript frontend, --nopapersize means no default paper size.\n");
	    }
	}
    } else {
	runprog(progname, gs, nargv);
    }
}

