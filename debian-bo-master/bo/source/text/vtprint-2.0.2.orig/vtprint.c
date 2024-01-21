/**************************************************************************
 **************************************************************************
 **                                                                      **
 ** vtprint.c       vtprint C source file                                **
 ** =========                                                            **
 **                                                                      **
 ** Purpose:        Provides printing services to a terminal printer     **
 **                 using terminal escape sequences.                     **
 **                                                                      **
 ** Author:         Garrett D'Amore <garrett@sciences.sdsu.edu>          **
 **                                                                      **
 ** Copyright:      1994, Garrett E. D'Amore                             **
 **                                                                      **
 ** NO WARRANTY:    This program is provided entirely without warranty.  **
 **                 The user assumes full responsibility for the use of  **
 **                 this program, and agrees to indemnify the author and **
 **                 the copyright holder from any damage or loss that    **
 **                 may result from the use of or inability to use this  **
 **                 program.  In simple language: YOU USE THIS PROGRAM   **
 **                 AT YOUR OWN RISK!                                    **
 **                                                                      **
 ** Warning:        None.                                                **
 **                                                                      **
 ** Usage:          vtprint [options] [files]                            **
 **                                                                      **
 ** Restrictions:   None.                                                **
 **                                                                      **
 ** Algorithm:      None.                                                **
 **                                                                      **
 ** References:     None.                                                **
 **                                                                      **
 ** File formats:   vtprintcap(5)                                        **
 **                                                                      **
 ** Rev. History:   June 4, 1994    Garrett D'Amore                      **
 **                 -- Initial coding.                                   **
 **                                                                      **
 ** Notes:          None.                                                **
 **                                                                      **
 **************************************************************************
 **************************************************************************/

/* >>>>>>>>>> Headers <<<<<<<<<< */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <memory.h>

#include "arglist.h"
#include "termstate.h"

/* >>>>>>>>>> Defines <<<<<<<<<< */

#define CR  '\xD'
#define LF  '\xA'
#define FF  '\xC'

#ifndef MAN_SECT
#define MAN_SECT "1"
#endif

#ifndef VERSION_NO
#define VERSION_NO "2.0dev"
#endif

#ifndef VERSION_DT
#define VERSION_DT "January 1, 1970"
#endif

#ifndef DEVTTY
#define DEVTTY "/dev/tty"
#endif

#ifndef LIBFILE
#define LIBFILE "/etc/vtprintcap"
#endif

/* >>>>>>>>>> Variables <<<<<<<<<< */

/* stuff used by getopt() */
extern char *optarg;
extern int  optind;

/* default printer control codes (ANSI/vt100/vt220) */
char    *ptr_on  = "\033[5i";
char    *ptr_off = "\033[4i";

/* string operating parameters */
char    *termname = "";
char    *libtermname = LIBFILE;
char    *devname = DEVTTY;
char    *vtbasename = "";

/* modes of operation */
int     quiet   = 0;    /* minimal message reporting? */
int     binary  = 0;    /* binary newline translation? */
int     ffeed   = 1;    /* formfeed between files? */
int     device  = 0;    /* use tty device instead of stdout? */
int     stripcr = 0;    /* strip CRs from CR/LFs? */
int     addcr   = 0;    /* add LFs to bare CRs? */
int     builtin = 0;    /* use builtin escape codes by default? */
int     forcenv = 0;    /* force using the getenv(TERM) value */
int     dowork  = 1;    /* whether or not to actually do processing */

/* terminal states */
int         state_saved = 0;
termstate   oldstate;
termstate   newstate;

/* output file descriptor */
FILE    *outfile;

/* >>>>>>>>>> Prototypes <<<<<<<<<< */

void    sighandler (int);
int     process_opts (arglist *);
void    help (void);
void    license (void);
void    warranty (void);
void    intro (void);

/* >>>>>>>>>> Functions <<<<<<<<<< */

/***************************************
 *
 *  sighandler      Signal handler for terminal clean-up.
 *
 *  Purpose         Ensures that the terminal is restored to a "sane"
 *                  state when a signal is received, and then exits
 *                  from program.
 *
 *  Parameters      signum:     signal number that was caught
 *
 *  Returns         None.
 *
 */

void sighandler (int signum)
{
    char *signame;

    fprintf (outfile, "%s", ptr_off);
    fflush (outfile);
    if (state_saved) restore_termstate(fileno(outfile), &oldstate);

    if (!quiet)
    {
        switch (signum)
        {
            case SIGHUP     : { signame = "hangup"; break; }
            case SIGINT     : { signame = "interrupt"; break; }
            case SIGQUIT    : { signame = "quit"; break; }
            case SIGTERM    : { signame = "terminate"; break; }
            case SIGTSTP    : { signame = "keyboard stop"; break; }
            default         : { signame = "unknown"; break; }
        }
        fprintf (outfile,"\nCaught %s signal!  Exiting vtprint.\n", signame);
    }
    exit (1);
}

/***************************************
 *
 *  process_opts    Process command-line and environment options.
 *
 *  Purpose         Process specific program options from an arglist.
 *                  Called once for environment, then again for argv[].
 *
 *  Parameters      arglist:    arglist of parameters to process
 *
 *  Returns         Pointer to "next" index in arglist to process (stuff
 *                  that getopt () can't process itself).
 *
 */

int process_opts (arglist *args)
{
    int c;

    optind = 1; /* reset getopt index */

    args -> argv [0] = vtbasename;

    do 
    {
        c = getopt (args->argc, args->argv, "hwlvdDbBcCeEfFnNqQT:tL:V:");
        switch (c)
        {
            case 'h' : { help (); dowork = 0; break; }
            case 'w' : { warranty (); dowork = 0; break; }
            case 'l' : { license (); dowork = 0; break; }
            case 'v' : { intro(); dowork = 0; break; }

            case 'd' : { device = 1; break; }
            case 'D' : { device = 0; break; }
            case 'b' : { binary = 1; break; }
            case 'B' : { binary = 0; break; }
            case 'c' : { stripcr = 0; addcr = 1; break; }
            case 'C' : { stripcr = 0; addcr = 0; break; }
            case 'e' : { builtin = 1; forcenv = 0; break; }
            case 'E' : { builtin = 0; forcenv = 0; break; }
            case 'f' : { ffeed = 0; break; }
            case 'F' : { ffeed = 1; break; }
            case 'n' : { addcr = 0; stripcr = 1; break; }
            case 'N' : { addcr = 0; stripcr = 0; break; }
            case 'q' : { quiet = 1; break; }
            case 'Q' : { quiet = 0; break; }
            case 't' : {
                           forcenv = 1;
                           termname = getenv ("TERM");
                           break;
                       }
            case 'T' : {
                           termname = optarg;
                           break;
                       }
            case 'L' : {
                           libtermname = optarg;
                           break;
                       }
            case 'V' : {
                           devname = optarg;
                           device = 1;
                           break;
                       }
            case '?' : { help(); exit (1); }
        }
    } while (c != EOF) ;
    return (optind);
}
                       
/***************************************
 *
 *  help            Provide "basic" on-line user help.
 *
 *  Purpose         To assist users who might get lost. (RTFM).
 *
 *  Parameters      None.
 *
 *  Returns         None.
 *
 */

void help (void)
{
    if ((!strcmp (vtbasename, "vtprton")) || (!strcmp (vtbasename, "vtprtoff")))
        fprintf (stderr, "Usage: %s [ -dDeEhlqQNtvw ] \
[ -L vtprintcap ] [ -T termtype ] [ -V device ]\n", vtbasename);

    else fprintf (stderr, "Usage: %s [ -bBcCdDeEfFhlnNqQtvW ] \
[ -L vtprintcap ] [ -T termtype ] [ -V device ] [ filename... ]\n",
    vtbasename);

    fprintf (stderr, "\nPlease see the %s(%s) manual page for more\n",
        vtbasename,MAN_SECT);

    fprintf (stderr, "information about the meaning of these options.\n");
}

/***************************************
 *
 *  intro           Provides a brief intro to vtprint.
 *
 *  Purpose         Displayed whenever user starts vtprint.
 *
 *  Parameters      None.
 *
 *  Returns         None.
 *
 */

void intro (void)
{
    fprintf (stderr, "\n*** %s (v%s) ***\n\n", vtbasename, VERSION_NO);
    fprintf (stderr, "Copyright 1993-1994, Garrett D'Amore\n");
    fprintf (stderr, "Last revised %s.\n\n", VERSION_DT);
    fprintf (stderr, "NO WARRANTY!  Use \"%s -w\" for info.\n",
        vtbasename);
    fprintf (stderr, "Freely redistributable.  Use \"%s -l\" for info.\n\n",
        vtbasename);

    return;
}

void warranty (void)
{
    fprintf (stderr, "%s: This program is provided entirely without\n",
        vtbasename);
    fprintf (stderr, "%s: warranty!  The user agrees to indemnify the\n",
        vtbasename);
    fprintf (stderr, "%s: author from any claim of damage or loss that\n",
        vtbasename);
    fprintf (stderr, "%s: may result from the use of or inability to\n",
        vtbasename);
    fprintf (stderr, "%s: use this program.\n\n", vtbasename);
}

void license (void)
{
    fprintf (stderr, "%s: This program is copyrighted material.  The user\n",
        vtbasename);
    fprintf (stderr, "%s: is given the non-exclusive right to use, modify\n",
        vtbasename);
    fprintf (stderr, "%s: and redistribute this program or any derived\n",
        vtbasename);
    fprintf (stderr, "%s: work, subject to the following limitation:\n\n",
        vtbasename);
    fprintf (stderr, "%s: All copyright notices and warranty disclaimers\n",
        vtbasename);
    fprintf (stderr, "%s: must be left intact!  The user may not claim \n",
        vtbasename);
    fprintf (stderr, "%s: credit for code written by the author.\n\n",
        vtbasename);
}

/***************************************
 *
 *  vtprintcap      Loads a vtprintcap description for use.
 *
 *  Purpose         Allows use of user-defined printer control codes.
 *
 *  Parameters      None.
 *
 *  Returns         zero on successful lookup,
 *                  one on lookup failure,
 *                  negative one on file open failure,
 *                  negative two on file parse error.
 *
 */

int vtprintcap ()
{
    FILE *libfile;
    char buffer [1025];
    static char on [1025];
    static char off [1025];
    char *temp = NULL;

    if ((!termname)||(!*termname)) termname = getenv ("TERM");

    libfile = fopen (libtermname, "r");
    if (!libfile) return (-1);

    while (fgets (buffer, 1024, libfile))
    {
        if (buffer[0] == '#') buffer [0] = '\0';
        temp = strchr (buffer,'\n');
        if (temp)  *temp = '\0';

        temp = strtok (buffer, ", \t");
        while (temp)
        {
            if (!strcmp (temp, termname)) break;
            temp = strtok (NULL, ", \t");
        }
        if (temp) break;
    }
    if (temp) 
    {
        if (!fgets (on, 1025, libfile))
        {
            fclose (libfile);
            return (-2);
        }
        if (!fgets (off, 1025, libfile))
        {
            fclose (libfile);
            return (-2);
        }

        fclose (libfile);

        /* verify no illegal substitution formats are present */

        if (on [strlen (on)-1] == '\n') on [strlen (on) - 1] = '\0';
        if (off [strlen (off)-1] == '\n') off [strlen (off) -1 ] = '\0';

        temp = on;
        while (temp)
        {
            temp = strchr (temp, '%');
            if (temp)
            {
                if (temp[1] != '%') return (-2);
                temp += 2;
            }
        }
        temp = off;
        while (temp)
        {
            temp = strchr (temp, '%');
            if (temp)
            {
                if (temp[1] != '%') return (-2);
                temp += 2;
            }
        }

        if (!escape (on)) return (-2);
        if (!escape (off)) return (-2);

        ptr_on = on;
        ptr_off = off;
        return 0;
    };
    fclose (libfile);
    return 1;
}

/* >>>>>>>>>> Main <<<<<<<<<< */

int main (int argc, char *argv[])
{
    char *tmp;
    char *env;
    int  result;
    int  cnt = 0;
    int  total;

    arglist args;
    args.argv = NULL;

    /* what are we called? */
    vtbasename = strrchr (argv[0],'/');
    vtbasename = (vtbasename ? vtbasename+1 : argv[0]);

    /* set up the environment variable properly */
    tmp = getenv (vtbasename);
    if (!tmp) tmp = getenv ("VTPRINT");
    if (!tmp) tmp = "";

    env = (char *) malloc (strlen (tmp) + 1);
    if (!env)
    {
        fprintf (stderr,"%s: Out of memory!\n",  vtbasename);
        exit (1);
    }

    strcpy (env, tmp);

    /* now we go through getopt (twice actually) */

    /* first for the environment variable (if present) */

    if (env)
    {
        string2arglist (env, &args);
        process_opts (&args);
    }

    /* then command line parameters */
    argv2arglist (argc, argv, &args);
    process_opts (&args);

    if (!dowork) exit(0);

    /* display intro on screen */
    if (!quiet) intro();

    /* get library file settings if needed */
    if (!builtin)
    {
        result = vtprintcap ();
        switch (result)
        {
            case -2 : {
                        if ((!quiet) && (!forcenv))
                        {
                            fprintf (stderr, 
                            "%s: Parse error in %s, using builtin codes.\n",
                            vtbasename, libtermname);
                        }
                        else if (forcenv)
                        {
                            fprintf (stderr, "%s: Parse error in %s!\n", 
                                vtbasename, libtermname);
                            exit(1);
                        }
                        break;
                      }
            case -1 : {
                        if ((!quiet) && (!forcenv))
                        {
                            fprintf (stderr,
                            "%s: Can't open %s, using builtin codes.\n",
                            vtbasename, libtermname);
                        }
                        else if (forcenv)
                        {
                            fprintf (stderr, "%s: Can't open %s!\n",
                                vtbasename, libtermname);
                            exit (1);
                        }
                        break;
                      }
            case 0  : {
                        if (!quiet)
                        {
                            fprintf (stderr, "%s: Using %s control codes.\n",
                                vtbasename, termname);
                        }
                        break;
                      }
            case 1  : {
                        if ((!quiet) && (!forcenv))
                        {
                            fprintf (stderr,
                            "%s: Terminal %s unknown, using builtin codes.\n",
                            vtbasename, termname);
                        }
                        else if (forcenv)
                        {
                            fprintf (stderr,
                            "%s: Terminal %s unknown!\n", vtbasename, termname);
                            exit(1);
                        }
                        break;
                      }
            default : {
                        fprintf (stderr, "%s: INTERNAL ERROR!!\n", vtbasename);
                        abort();
                      }
        }
    }

    /* set up the output file descriptor */
    if (device) 
    {
        outfile = fopen (devname, "w");
        if (!outfile)
        {
            fprintf (stderr, "%s: Failed to open %s for writing!\n",
                vtbasename, devname);
            exit (1);
        }
        if (!quiet)
            fprintf (stderr,"%s: Using %s for output.\n", vtbasename, devname);
    }
    else
    {
        outfile = stdout;
        if (!quiet)
            fprintf (stderr, "%s: Using <stdout> for output.\n", vtbasename);
    }

    /* special handling for vtprton and vtprtoff */
    if (!strcmp (vtbasename, "vtprton"))
    {
        fflush (outfile);
        fprintf (outfile, ptr_on);
        fflush (outfile);
        exit (0);
    }
    if (!strcmp (vtbasename, "vtprtoff"))
    {
        fflush (outfile);
        fprintf (outfile, ptr_off);
        fflush (outfile);
        exit (0);
    }


    /* deal with terminal states */
    if (isatty (fileno(outfile)))
    {
        save_termstate(fileno (outfile), &oldstate);
        state_saved = 1;
        memcpy ((char *) &newstate, (char *) &oldstate, sizeof (termstate));
        if (binary)
        {
            raw_termstate(&newstate);
            restore_termstate (fileno (outfile), &newstate);
        }
    }
    else
    {
        binary = 0; /* force this! */
    }

    if (!quiet)
    {
        fprintf (stderr, "%s: Output flags: ", vtbasename);
        if (builtin) fprintf (stderr, "builtin ");
        if (binary) fprintf (stderr, "binary ");
        if (ffeed)  fprintf (stderr, "formfeed ");
        if (stripcr) fprintf (stderr, "noCR ");
        if (addcr) fprintf (stderr, "addCR ");
        fprintf (stderr, "\n");
        fflush (stderr);
    }

    /* establish signal handlers */
    signal (SIGHUP, sighandler);
    signal (SIGQUIT, sighandler);
    signal (SIGTSTP, sighandler);
    signal (SIGINT, sighandler);
    signal (SIGTERM, sighandler);

    total = argc - optind;

    /* the main processing loop */
    do
    {
        int ch;
        int lastch = 0;
        FILE *infile;

        if (optind == argc)
            infile = stdin;
        else infile = fopen (argv[optind], "r");

        if (!infile)
        {
            fprintf (stderr, "%s: Couldn't open %s for reading.\n",
                vtbasename, argv[optind]);
            optind++;
            continue;
        }

        fflush (outfile);
        fprintf (outfile, ptr_on);
        fflush (outfile);

        while ((ch = fgetc (infile)) != EOF)
        {
            switch (ch)
            {
                case CR :   {
                                if (!stripcr) fputc (ch, outfile);
                                break;
                            }
                case LF :   {
                                if ((lastch != CR) && (addcr))
                                    fputc (CR, outfile);
                                fputc (ch, outfile);
                                break;
                            }
                default :   {
                                if ((lastch == CR) && (stripcr))
                                    fputc (CR, outfile);
                                fputc (ch, outfile);
                                break;
                            }
            }
            lastch = ch;
        }

        fclose (infile);
        if (ffeed) fputc (FF, outfile);
        fprintf (outfile, ptr_off);
        fflush (outfile);

        cnt++;

        if (!quiet)
        {
            if (argc == optind)
                fprintf  (stderr, "%s: Printed <stdin>.\n", vtbasename);
            else
            fprintf (stderr, "%s: Printed %s.\n", vtbasename, argv[optind]);
        }   

        optind++;

    } while (optind < argc);

    if (state_saved) restore_termstate (fileno (outfile), &oldstate);

    if (!quiet) fprintf (stderr,
        "%s: Successfully printed %d file%s (%d specified).\n",
        vtbasename, cnt, cnt == 1 ? "" : "s", total);

    return 0;
}
