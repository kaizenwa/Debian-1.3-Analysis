
static char rcsid[] = "@(#)$Id: args.c,v 5.9 1993/04/12 03:35:08 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.9 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 *****************************************************************************
 *
 *****************************************************************************/

/** starting argument parsing routines for ELM system...

**/

#include "headers.h"
#include "patchlevel.h"
#include "s_elm.h"
#include "me.h"

extern char *optarg;		/* optional argument as we go */
extern int   optind;			/* argnum + 1 when we leave   */

char * parse_arguments(argc, argv, to_whom, size_r)
     int argc;
     char *argv[], *to_whom;
     int *size_r;
{
	/** Set flags according to what was given to program.  If we are 
	    fed a name or series of names, put them into the 'to_whom' buffer
	    and if the check_only flag wasn't presented, set mail_only to ON,
	    and if stdin is not a tty, set batch_only  to ON;
	    Return req_mfile, which points to a named mail file or is empty.
	    **/

	register int c = 0;
	static char req_mfile[SLEN];

	*size_r = sizeof req_mfile;

	to_whom[0] = '\0';
	batch_subject[0] = '\0';
	included_file[0] = '\0';

        while ((c = getopt(argc, argv, "?aA:cd:f:hi:kKms:tVvwz")) != EOF) {
	  switch (c) {
	  case 'a' : arrow_cursor++;		break;
#ifdef MIME
	  case 'A' : 
	    if(!Attach_it(optarg)) exit(1);
	    break;
#endif 
	  case 'c' : check_only++; use_tite = 0;	break;
	  case 'd' : debug = atoi(optarg);	break;
	  case 'f' : strfcpy(req_mfile, optarg,sizeof req_mfile);	
	    break;
	  case '?' :
	  case 'h' : args_help();
	  case 'i' : strfcpy(included_file, optarg, sizeof included_file);  
	    break;
	       /* case 'k' : hp_terminal++;	break;
		  case 'K' : hp_terminal++; hp_softkeys++;	break;
		  */
	  case 'm' : mini_menu = 0;	break;
	  case 's' : strfcpy(batch_subject, optarg, sizeof batch_subject);
	    break;
	  case 't' : use_tite = 0;	break;
	  case 'V' : sendmail_verbose++;     break;
	  case 'v' : args_version();
	  case 'w' : write_elmrc++; break;
	  case 'z' : check_size++; use_tite = 0;   break;
	  }
	}


#ifndef DEBUG
	if (debug)
	  printf(catgets(elm_msg_cat, ElmSet, ElmArgsIngoringDebug,
     "Warning: system created without debugging enabled - request ignored\n"));
	debug = 0;
#endif

	if (optind < argc) {
	  while (optind < argc) {
		if (strlen(to_whom) + strlen(to_whom[0] != '\0'? "," : "") +
			strlen(argv[optind]) > SLEN)
				exit(printf(catgets(elm_msg_cat, ElmSet, ElmArgsTooManyAddresses,
					"\n\rToo many addresses, or addresses too long!\n\r")));

	    sprintf(to_whom, "%s%s%s", to_whom, 
	            to_whom[0] != '\0'? "," : "", argv[optind]);
	    if(!check_only)
	      mail_only++;
	    optind++;
	  }
	  check_size = 0;	/* NEVER do this if we're mailing!! */
	}

	 if (strlen(batch_subject) > 0 && ! mail_only) 
	   exit(printf(catgets(elm_msg_cat, ElmSet, ElmArgsSubjectNotSend,
     "\n\rDon't understand specifying a subject and no-one to send to!\n\r")));

	if (!isatty(fileno(stdin)) && !check_only) {
	  batch_only = ON;
	  if(*batch_subject == '\0')
	    strfcpy(batch_subject, DEFAULT_BATCH_SUBJECT, 
		    sizeof batch_subject);
	}

	if (strlen(included_file) > 0
#ifdef MIME
	    || attach_files
#endif
	    ) {
	  if (! mail_only)
	    exit(printf(catgets(elm_msg_cat, ElmSet, ElmArgsInclFileNotSend,
            "\n\rCan't specify an included file with no-one to send to!\n\r")));
	  if (batch_only)
	    exit(printf(catgets(elm_msg_cat, ElmSet, ElmArgsInclFileBatch,
              "\n\rCan't specify an included file in batch mode!\n\r")));
        }

	return(req_mfile);


}

args_help()
{
	/**  print out possible starting arguments... **/

	printf(catgets(elm_msg_cat, ElmSet, ElmArgsHelp1,
	  "\nPossible Starting Arguments for ELM program:\n\n\r\
\targ\t\t\tMeaning\n\r\
\t -a \t\tArrow - use the arrow pointer regardless\n\r\
\t -A \t\tAttach file (requires .elm/mime.types)\n\r\
\t -c \t\tCheckalias - check the given aliases only\n\r\
\t -dn\t\tDebug - set debug level to 'n'\n\r\
\t -fx\t\tFolder - read folder 'x' rather than incoming mailbox\n\r\
\t -h \t\tHelp - give this list of options\n\r\
\t -ix\t\tInclude prepared file 'x' in edit buffer for send\n\r"));
	printf(catgets(elm_msg_cat, ElmSet, ElmArgsHelp2,
	  "\t -m \t\tMenu - Turn off menu, using more of the screen\n\r\
\t -sx\t\tSubject 'x' - for batch mailing\n\r\
\t -t \t\tTiTe - don't use termcap/terminfo ti/te entries.\n\r\
\t -V \t\tEnable sendmail voyeur mode.\n\r\
\t -v \t\tPrint out ELM version information.\n\r\
\t -w \t\tWrite .elm/elmrc\n\r\
\t -z \t\tZero - don't enter ELM if no mail is pending\n\r\
\n\n"));
	/*
	  \t -k \t\tKeypad - enable HP 2622 terminal keyboard\n\r
	  \t -K \t\tKeypad&softkeys - enable use of softkeys + \"-k\"\n\r\
	  */
	exit(1);
}

args_version()
{
	/** print out version information **/

	printf("\nElm Version and Identification Information:\n\n");
	printf("\tElm %s PL%s, of %s\n",VERSION,PATCHLEVEL,VERS_DATE);
	printf("\t(C) Copyright 1988-1992 USENET Community Trust\n");
	printf("\tBased on Elm 2.0, (C) Copyright 1986,1987 Dave Taylor\n");
	printf("\t----------------------------------\n");
	printf("\tConfigured %s\n", CONFIGURE_DATE);
	printf("\t----------------------------------\n");

#ifdef MMDF
	printf("\tUse MMDF Mail Transport Agent/Mailbox Delimiters: MMDF\n");
#else /* MMDF */
	printf("\tUse UNIX Mailbox Delimiters and %s Mail Transport Agent: not MMDF\n", mailer);
#endif /* MMDF */

#ifdef DONT_ADD_FROM
	printf("\tLet the MTA add the From: header: DONT_ADD_FROM\n");
#else /* DONT_ADD_FROM */
	printf("\tElm will add the From: header: not DONT_ADD_FROM\n");
#endif /* DONT_ADD_FROM */

	printf("\tFollowing mail spool locking protocols will be used:");
#ifdef USE_DOTLOCK_LOCKING
	printf(" USE_DOTLOCK_LOCKING (.lock)");
#endif
#ifdef USE_FLOCK_LOCKING
	printf(" USE_FLOCK_LOCKING");
#endif
#ifdef USE_FCNTL_LOCKING
	printf(" USE_FCNTL_LOCKING");
#endif
	printf("\n");

#ifdef USE_EMBEDDED_ADDRESSES
	printf("\tFrom: and Reply-To: addresses are good: USE_EMBEDDED_ADDRESSES\n");
#else /* USE_EMBEDDED_ADDRESSES */
	printf("\tFrom: and Reply-To: addresses ignored: not USE_EMBEDDED_ADDRESSES\n");
#endif /* USE_EMBEDDED_ADDRESSES */

#ifdef MIME
	printf("\tSupport Multipurpose Internet Mail Extensions: MIME\n");

#ifdef USE_8BITMIME
      printf("\tUse 8BIT content_transfer_encoding and -B8BITMIME -option: USE_8BITMIME\n");
#else /* USE_8BITMIME */
      printf("\tDon't use 8BIT content_transfer_encoding with -B8BITMIME -option: not USE_8BITMIME\n");
#endif /* USE_8BITMIME */

#ifdef USE_BINARYMIME
      printf("\tUse BINARY content_transfer_encoding and -BBINARYMIME -option: USE_BINARYMIME");
#else /* USE_BINARYMIME */
      printf("\tDon't use BINARY content_transfer_encoding with -BBINARYMIME -option: not USE_BINARYMIME");
#endif /* USE_BINARYMIME */
      printf(" (This option isn't fully implemented yet.)\n");

#ifdef USE_DSN
      printf("\tUse DSN and options -N -R -V: USE_DSN\n");
#else /* USE_DSN */
      printf("\tDon't use DSN with -N -R -V -options: not USE_DSN\n");
#endif /* USE_DSN */

#else /* MIME */
	printf("\tIgnore Multipurpose Internet Mail Extensions: not MIME\n");
#endif /* MIME */

#ifdef INTERNET
	printf("\tPrefers Internet address formats: INTERNET\n");
#else /* INTERNET */
	printf("\tInternet address formats not used: not INTERNET\n");
#endif /* INTERNET */

#ifdef DEBUG
	printf("\tDebug options are available: DEBUG\n");
#else /* DEBUG */
	printf("\tNo debug options are available: not DEBUG\n");
#endif /* DEBUG */
		
#ifdef CRYPT
	printf("\tCrypt function enabled: CRYPT\n");
#else /* CRYPT */
	printf("\tCrypt function disabled: not CRYPT\n");
#endif /* CRYPT */

#ifdef ALLOW_MAILBOX_EDITING
	printf("\tMailbox editing included: ALLOW_MAILBOX_EDITING\n");
#else /* ALLOW_MAILBOX_EDITING */
	printf("\tMailbox editing not included: not ALLOW_MAILBOX_EDITING\n");
#endif /* ALLOW_MAILBOX_EDITING */

#ifdef ALLOW_SUBSHELL
	printf("\tSubshell menu items included: ALLOW_SUBSHELL\n");
#else /* ALLOW_SUBSHELL */
	printf("\tSubshell menu items not included: not ALLOW_SUBSHELL\n");
#endif /* ALLOW_SUBSHELL */

#ifdef ISPELL
	printf("\tSpell checking feature enabled: ISPELL\n");
	printf("\t\t(Default spelling checker is %s options '%s')\n", ISPELL_PATH, ISPELL_OPTIONS);
#else /* ISPELL */
	printf("\tSpell checking feature disabled: not ISPELL\n");
#endif /* ISPELL */

#ifdef ENABLE_CALENDAR
	printf("\tCalendar file feature enabled: ENABLE_CALENDAR\n");
	printf("\t\t(Default calendar file is %s)\n",dflt_calendar_file);
#else /* ENABLE_CALENDAR */
	printf("\tCalendar file feature disabled: not ENABLE_CALENDAR\n");
#endif /* ENABLE_CALENDAR */

#ifdef USE_PGP
	printf("\tPGP support enabled: USE_PGP\n");
#else
	printf("\tPGP support disabled: not USE_PGP\n");
#endif
#ifdef USE_REMAILER
	printf("\tRemailer support enabled: USE_REMAILER\n");
#else
	printf("\tRemailer support disabled: not USE_REMAILER\n");
#endif
	printf("\n\n");
	exit(1);

}

