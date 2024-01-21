/*
 * args.c
 */

/*
 * mpage:    a program to reduce pages of print so that several pages
 *           of output appear on one printed page.
 *
 * Written by:
 *   ...!uunet!\                       Mark Hahn, Sr Systems Engineer
 *              >pyrdc!mark            Pyramid Technology Corporation
 * ...!pyramid!/                       Vienna, Va    (703)848-2050
 *
 *
 * Copyright (c) 1988 Mark P. Hahn, Herndon, Virginia
 * Copyright (c) 1994-1996 Marcel J.E. Mol, Rijswijk, The Netherlands
 *                    marcel@mesa.nl
 *  
 *     Permission is granted to anyone to make or distribute verbatim
 *     copies of this document as received, in any medium, provided
 *     that this copyright notice is preserved, and that the
 *     distributor grants the recipient permission for further
 *     redistribution as permitted by this notice.
 *
 */

#include "mpage.h"
#include <ctype.h>
#include <string.h>

/*
 * Function declarations
 */
static char **slice();



int
do_args(argc, argv, envflag)
 int argc;
 char **argv;
 int envflag;
{
    char *optstr;
    int consumed;
    int currarg;
    int opterrors;

#define OPTARG()  \
    { consumed = 1; if(*++optstr == '\0') optstr = argv[++currarg]; }

    opterrors = 0;
    for (currarg = 1; currarg < argc; currarg++) {
        if (*argv[currarg] != '-') {
            if (envflag)
                opterrors++;
            break;
        }
        optstr = argv[currarg];
        consumed = 0;
        while (!consumed && *++optstr) {
            switch (*optstr) {
                default:
                        fprintf(stderr, "%s: unknown option -%c\n",
                                        MPAGE, *optstr);
                        opterrors++;
                        break;
                case '1':
                        sheetindex = 0;
                        break;
                case '2':
                        sheetindex = 1;
                        break;
                case '4':
                        sheetindex = 2;
                        break;
                case '8':
                        sheetindex = 3;
                        break;
                case 'a':    /* toggle between accross and updown */
                        sheetorder = sheetorder == LEFTRIGHT ?
                                     UPDOWN : LEFTRIGHT;
                        break;
                case 'A':    /* A4 sized, european paper */
                        opt_page = PAGE_A4;
                        break;
                case 'b':     /* base paper type */
                        OPTARG();
                        if (!strcasecmp(optstr, "A4"))
                            opt_page = PAGE_A4;
                        else if (!strcasecmp(optstr, "Letter"))
                            opt_page = PAGE_LETTER;
                        else if (!strcasecmp(optstr, "Legal"))
                            opt_page = PAGE_LEGAL;
                        else
                            fprintf(stderr, "unkown paper type %c\n", *optstr);
                        break;
                case 'B': {
                        int pb = 0; /* set a default */

                        consumed = 1;
                        opt_textbox = 1 - opt_textbox;
                        if (*++optstr == '\0') {
                            break;
                        }
                                
                        while (*optstr) {
                            int sign;
                            if (*optstr == '-') {
                                sign = -1;
                                optstr++;
                            }
                            else
                                sign = 1;
                            if (isdigit(*optstr)) {
                                pb = *optstr - '0';
                                while (*++optstr && isdigit(*optstr))
                                    pb = pb*10 + *optstr - '0';
                            }
                            pb *= sign;
                            switch (*optstr) { 
                                case 'l': textmargin_left = pb;
                                          break;
                                case 'r': textmargin_right = pb;
                                          break;
                                case 't': textmargin_top = pb;
                                          break;
                                case 'b': textmargin_bottom = pb;
                                          break;
                                case '\0':textbox.thick = pb;
                                          break;
                                default:  fprintf(stderr,
                                           "%s: Unknown -B box specifier: %c\n",
                                           MPAGE, *optstr);
                                          break;
                            }
                            if (*optstr)
                                optstr++;
                        }
                        break;
                    }
                case 'c':    /* concat pages from different files on sheet */
                        opt_file = 1 - opt_file;
                        break;
                case 'C':    /* select character definitions */
                        consumed = 1;
                        if (*++optstr) { /* did we get a encoding name ? */
                            if ((charvec_file = (char *) malloc(strlen(libdir) +
                                                                strlen(optstr) +
                                                                2)) == NULL) {
                                perror(optstr);
                                fprintf(stderr,
                                    "ignoring character encoding definition\n");
                            }
                            else {
                                (void) strcpy(charvec_file, libdir);
                                (void) strcat(charvec_file, "/");
                                (void) strcat(charvec_file, optstr);
                                opt_encoding = 1;
                            }
                        }
                        else /* no encoding name, toggle default one */
                            opt_encoding = 1 - opt_encoding;
                        break;
                case 'd':
                        OPTARG();
                        switch (*optstr) {
			    case 'a': opt_input = IN_ASCII; break;
			    case 'p': opt_input = IN_PS; break;
			    default :
                                fprintf(stderr,
                                        "ignoring input file type -d%c\n",
                                        *optstr);
				break;
			}
                        break;
                case 'D':
                        OPTARG();
                        dateformat = optstr;
                        break;
	        case 'E': /* GPN. for coli, 2,3(inside) pages */
	                Coli = 2;
                        break;
                case 'f':    /* fold long lines */
                        opt_fold = 1 - opt_fold;
                        break;
                case 'F':
                        OPTARG();
                        fontname = optstr;
                        break;
                case 'h':
                        OPTARG();
                        opt_header = optstr;
                        break;
                case 'H':
                        opt_mp_header = 1;
                        break;
                case 'I':
                        OPTARG();
                        opt_indent = atoi(optstr);
                        break;
                case 'j':    /* Just these sheets */
                        OPTARG();
                        opt_first = isdigit(*optstr) ? 
                                    strtol(optstr, &optstr, 10) : 1;
                        if (*optstr == '-') {
                            if (isdigit(*++optstr))
                                opt_last = strtol(optstr, &optstr, 10);
                        }
                        else
                            opt_last = MAXINT;
                        if (*optstr == '/' || *optstr == '%')
                            opt_alt = atoi(++optstr);
                        break;
                case 'l':    /* landscape */
                        sheetaspect = sheetaspect == LANDSCAPE ?
                                      PORTRAIT : LANDSCAPE;
                        break;
                case 'L':
                        OPTARG();
                        opt_lines = atoi(optstr);
                        break;
                case 'm': {
                        int sm = 2*DEFAULTSMARGIN; /* set a default */

                        consumed = 1;
                        if (*++optstr == '\0') {
                            sheetmargin_left = sm;
#if defined(ALL_MARGINS)
                            sheetmargin_right = sm;
                            sheetmargin_top = sm;
                            sheetmargin_bottom = sm;
#endif
                            break;
                        }
                                
                        while (*optstr) {
                            int sign;
                            if (*optstr == '-') {
                                sign = -1;
                                optstr++;
                            }
                            else
                                sign = 1;
                            if (isdigit(*optstr)) {
                                sm = *optstr - '0';
                                while (*++optstr && isdigit(*optstr))
                                    sm = sm*10 + *optstr - '0';
                            }
                            sm *= sign;
                            switch (*optstr) { 
                                case 'l': sheetmargin_left = sm;
                                          break;
                                case 'r': sheetmargin_right = sm;
                                          break;
                                case 't': sheetmargin_top = sm;
                                          break;
                                case 'b': sheetmargin_bottom = sm;
                                          break;
                                case '\0':sheetmargin_left = sm;
                                          sheetmargin_right = sm;
                                          sheetmargin_top = sm;
                                          sheetmargin_bottom = sm;
                                          break;
                                default:  fprintf(stderr,
                                        "%s: Unknown -m margin specifier: %c\n",
                                        MPAGE, *optstr);
                                          break;
                            }
                            if (*optstr)
                                optstr++;
                        }
                        break;
                    }
                case 'M': {
                        int pm = 2*DEFAULTPMARGIN; /* set a default */

                        consumed = 1;
                        if (*++optstr == '\0') {
                            pagemargin_left = pm;
#if defined(ALL_MARGINS)
                            pagemargin_right = pm;
                            pagemargin_top = pm;
                            pagemargin_bottom = pm;
#endif
                            break;
                        }
                                
                        while (*optstr) {
                            int sign;
                            if (*optstr == '-') {
                                sign = -1;
                                optstr++;
                            }
                            else
                                sign = 1;
                            if (isdigit(*optstr)) {
                                pm = *optstr - '0';
                                while (isdigit(*++optstr))
                                    pm = pm*10 + *optstr - '0';
                            }
                            pm *= sign;
                            switch (*optstr) { 
                                case 'l': pagemargin_left = pm;
                                          break;
                                case 'r': pagemargin_right = pm;
                                          break;
                                case 't': pagemargin_top = pm;
                                          break;
                                case 'b': pagemargin_bottom = pm;
                                          break;
                                case '\0':pagemargin_left = pm;
                                          pagemargin_right = pm;
                                          pagemargin_top = pm;
                                          pagemargin_bottom = pm;
                                          break;
                                default:  fprintf(stderr,
                                        "%s: Unknown -M margin specifier: %c\n",
                                           MPAGE, *optstr);
                                          break;
                            }
                            if (*optstr)
                                optstr++;
                        }
                        break;
                    }
                case 'o':    /* toggle print outlines */
                        opt_outline = 1 - opt_outline;
                        break;
	        case 'O': /* GPN. for coli, 4,1(outside) pages */
	                Coli = 1;
                        break;
                case 'p':    /* pr */
                        opt_pr = 1;
                        consumed = 1;
                        if (*++optstr)
                            prprog = optstr;
                        break;
                case 'P':    /* Printer */
                        consumed = 1;
                        doprint = 1;
                        if (*++optstr)
                            if (!strcmp(optstr, "-"))
                                doprint = 0; /* kill MPAGE envvar que setting*/
                            else {
                                printque = optstr;
                            }
                        break;
                case 'r':
                        opt_reverse = 1;
                        break;
                case 'R':    /* reorient  */
                        sheetaspect = LANDSCAPE_PORTRAIT;
                        break;
                case 's': /* tab Stops */
                        OPTARG();
                        if ((opt_tabstop = atoi(optstr)) < 2)
                            opt_tabstop = DEFAULTTABSTOP;
                        break;
                case 'S':
                        opt_square = 0;
                        break;
                case 't':
                        opt_duplex = 1 - opt_duplex;
                        break;
                case 'T':
                        opt_tumble = 1 - opt_tumble;
                        break;
                case 'U':    /* Letter sized, US paper */
                        opt_page = PAGE_LETTER;
                        break;
                case 'v':    /* verbose (print page count) */
                        opt_verbose = 1 - opt_verbose;
                        break;
                case 'W':
                        OPTARG();
                        opt_width = atoi(optstr);
                        break;
                case 'x': /* force usage. Could be extended to usagelevel */
                        opterrors = 1;
                        break;
                case 'X':
                        opt_sheetheader = 1;
                        consumed = 1;
                        if (*++optstr)
                            sheethead = optstr;
                        break;
                case 'z':
                        OPTARG();
                        printprog = optstr;
                        break;
                case 'Z':
                        OPTARG();
                        printarg = optstr;
                        break;

            }
        }
    }
    set_page();

    if (opterrors)
        return -1;

    return currarg;

} /* do_args */



int
do_env()
{
    int argc;
    char **argv;
    char copy[LINESIZE];
    char *env;

#if SPOOLER == ATT_SPOOLER
    if ((env = getenv("LPDEST")) != NULL)
#elif SPOOLER == BSD_SPOOLER
    if ((env = getenv("PRINTER")) != NULL)
#endif
        printque = env;

    if ((env = getenv("MPAGE_LIB")) != NULL)
        libdir = env;

    if ((env = getenv("MPAGE")) != NULL) {
        strcpy(copy, env);
        argv = slice(copy, &argc);
        if (do_args(argc, argv, 1) < 0) {
            fprintf(stderr, "%s: error in environment \"%s\"\n", MPAGE, env);
            return -1;
        }
    }

    return 0;

} /* do_env */

#define ARGCNT	20

char *slc_argv[ARGCNT+1];

static char **
slice(string, cntp)
 char *string;
 int *cntp;
{
    int count;

    /*
     * mimic the shell for conformity
     */
    slc_argv[0] = MPAGE;
    count = 1;
    /*
     * while there are still characters to be processed
     */
    while (*string && count < ARGCNT) {
        /*
         * skip any leading or leftover white space
         */
        while (*string == ' ')
            string++;
        /*
         * make sure we had more than just white space before
         * we believe we actually have an argument
         */
        if (*string) {
            /*
             * point the next slot in argv to this string
             */
            slc_argv[count++] = string;
            /*
             * and go looking for the end of this string
             * which is delienated by a space or NULL
             */
            while (*string && *string != ' ')
                string++;
            /*
             * if this not the end of the string, then convert
             * the space into a NULL and move forward one byte.
             * if this is the end of the string, we already have
             * a suitable NULL byte for the string and it also
             * drops us out of all the loops
             */
            if (*string) {
                *string = 0;
                string++;
            }
        }
    }
    /*
     * return the count via the integer pointer we were given
     * and put a null pointer into the argv array for conformity
     */
    if (*string && count == ARGCNT)
        fprintf(stderr,
           "%s: to many options in MPAGE environment variable, skipping '%s'\n",
               MPAGE, string);
    slc_argv[count] = 0;
    *cntp = count;

    return slc_argv;

} /* slice */
