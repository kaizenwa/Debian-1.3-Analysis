#include "kiss.h"

int dokiss (int argc, char **argv)
{
    register int
	cflag = 0,
	opt;
    struct passwd
	*pwd;
    char
	buf [LINELEN];

    orgargc = argc;
    orgargv = argv;
    
    while ( (opt = getopt (argc, argv, "cCdeEhkv")) != -1 && ! cflag)
	switch (opt)
	{
	    case 'c':
		cflag = 1;
		break;
	    case 'd':
		flags.debug = 1;
		break;
	    case 'C':
		flags.ctrlc = 1;
		break;
	    case 'v':
		flags.version = 1;
		break;
	    case 'e':
		flags.supressstat = 1;
		break;
	    case 'E':
		flags.noenviron = 1;
		break;
	    case 'k':
		flags.controlkids = 1;
		break;
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s [-cCdeEhkv] [file(s)]\n"
		       "Where:\n"
		       "    -c cmd: run command and terminate, must be "
							    "last argument\n"
		       "    -C: exit on ^C, default is to ignore\n"
		       "    -d: debug run, no program started\n"
		       "    -e: suppress printing exit status of programs\n"
		       "    -E: suppress setting of environment variables\n"
		       "    -h: this message\n"
		       "    -k: keep control of background processes, "
						    "kill when finished\n"
		       "        (default: release background processes)\n"
		       "    -v: print version and stop\n"
		       "     file(s): scripts to interpret, when absent: "
							    "stdin is used\n"
		       , progname);
	    }
    
    if (flags.version)
	banner ();
    
    if (flags.ctrlc)
	signal (SIGINT, sighandler);
    else
	signal (SIGINT, SIG_IGN);

    signal (SIGUSR1, sighandler);
    signal (SIGUSR2, sighandler);
    signal (SIGSEGV, sighandler);

    if (! flags.noenviron)		    /* set environ strings */
    {
	addtoenv ("SHELL", argv [0]);	    /* SHELL */
	setshlvl ();			    /* SHLVL */

	if (! (pwd = getpwuid (getuid ())) )
	    warning ("failure while reading user info");
	else				    /* USER and UID and HOME */
	{
	    addtoenv ("USER", pwd->pw_name);
	    strcpy (username, pwd->pw_name);
	    sprintf (buf, "%d", pwd->pw_uid);
	    addtoenv ("UID", buf);
	    if (pwd->pw_dir)
	    {
		addtoenv ("HOME", pwd->pw_dir);
		strcpy (homedir, pwd->pw_dir);
	    }
	}

	if (ttyname (STDIN_FILENO))	    /* TTY */
	    addtoenv ("TTY", ttyname (STDIN_FILENO));
    }

    /* -c flag given? */
    if (cflag)
    {
	if (optind >= argc)
	    error ("-c flag needs command to run");
	command (optind);
    }
    else
    {
	/* more file args? */
	if (optind < argc)
	{
	    if (! (yyin = fopen (argv [optind], "r")) )
		warning ("cannot open script \"%s\" for reading",
			 argv [optind]);
	    else
	    {
		lastfile = optind;
		yyparse ();
	    }
	}
	else
	{
	    yypushfile (stdin);
	    startupfiles ();
	    yyparse ();
	    if (isatty (fileno (yyin)))
		putchar ('\n');
	}
    }    
    return (laststatus);
}
