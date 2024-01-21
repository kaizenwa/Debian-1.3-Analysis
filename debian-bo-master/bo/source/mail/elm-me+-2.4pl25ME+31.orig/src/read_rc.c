
static char rcsid[] = "@(#)$Id: read_rc.c,v 5.25 1993/08/23 02:58:04 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.25 $   $State: Exp $
 *
 *			Copyright (c) 1988-1992 USENET Community Trust
 *			Copyright (c) 1986,1987 Dave Taylor
 *****************************************************************************/

/** This file contains programs to allow the user to have a .elm/elmrc file
    in their home directory containing any of the following: 

	fullname= <username string>
	maildir = <directory>
	tmpdir  = <directory>
	sentmail = <file>
	editor  = <editor>
	receviedmail= <file>
	calendar= <calendar file name>
	shell   = <shell>
	print   = <print command>
	weedout = <list of headers to weed out>
	prefix  = <copied message prefix string>
	pager   = <command to use for displaying messages>
	configoptions = <list of letters to use for options menu>
	precedences = <list of delivery precedences>
	
	escape  = <single character escape, default = '~' >

--
	signature = <.signature file for all outbound mail>
OR:
	localsignature = <.signature file for local mail>
	remotesignature = <.signature file for non-local mail>
--

	bounceback= <hop count threshold, or zero to disable>
	readmsginc = <new folder message count display increment>
	sleepmsg = time to 'sleep' while displaying transient messages
	timeout = <seconds for main menu timeout or zero to disable>
	userlevel = <0=amateur, 1=okay, 2 or greater = expert!>

	sortby  = <sent, received, from, size, subject, mailbox, status>
	alias_sortby  = <alias, name>

	alternatives = <list of addresses that forward to us>

	builtinlines = <use builtin pager if message shorter than # lines>

    and/or the logical arguments:
	
	autocopy    [on|off]
	askcc	    [on|off]
	copy        [on|off]	
	resolve     [on|off]
	weed        [on|off]
	noheader    [on|off]
	titles      [on|off]
	savebyname  [on|off]
	forcename   [on|off]
	movepage    [on|off]
	pointnew    [on|off]
	hpkeypad    [on|off]
	hpsoftkeys  [on|off]
	alwayskeep  [on|off]
	alwaysstore [on|off]
	alwaysdel   [on|off]
	arrow	    [on|off]
	menus	    [on|off]
	metoo	    [on|off]
	forms	    [on|off]
	names	    [on|off]
	ask	    [on|off]
	keepempty   [on|off]
	promptafter [on|off]
	sigdashes   [on|off]
	usetite	    [on|off]


    Lines starting with '#' are considered comments and are not checked
    any further!

**/

#define SAVE_OPTS
#include "headers.h"
#include "save_opts.h"
#include "s_elm.h"
#include <errno.h>
#include "me.h"

extern opts_menu *find_cfg_opts();
extern int errno;

char  *error_description(), *shift_lower(), *mode_to_str();

#define  metachar(c)	(c == '+' || c == '%' || c == '=')
#ifndef ok_rc_char
#define ok_rc_char(c)   (isalnum(c) || c == '-' || c == '_')
#endif


#define ASSIGNMENT      0
#define WEEDOUT		1
#define ALTERNATIVES	2

#define SYSTEM_RC	0
#define LOCAL_RC	1

static int lineno = 0;
static int errors = 0;

static void do_expand_env P_((char *, char *, char *, unsigned));
static void do_rc P_((FILE *file,int lcl, char *filaname));
static int do_set P_((FILE *, char *, char *, int, int, char *));

static void do_expand_env(descr, dest, src, destlen)
    char	*descr, *dest, *src;
    unsigned	destlen;
{
    if (expand_env(dest, src, destlen) != 0) {
	printf(catgets(elm_msg_cat, ElmSet, ElmCannotInitErrorExpanding,
	    "\r\nCannot initialize \"%s\" - error expanding \"%s\".\r\n"),
	    descr, src);
	leave(0);
    }
}

int init_defaults() {
  char *cp;
  FILE * file;

  /* Establish some defaults in case elmrc is incomplete or not there.
   * Defaults for other elmrc options were established in their
   * declaration - in elm.h.  And defaults for sent_mail and recvd_mail
   * are established after the elmrc is read in since these default
   * are based on the folders directory name, which may be given
   * in the emrc.
   * Also establish alternative_editor here since it is based on
   * the default editor and not on the one that might be given in the
   * elmrc.
   */
	 
  default_weedlist();
  errors = 0;

  alternative_addresses = NULL; 	/* none yet! */
  
  raw_local_signature[0] = raw_remote_signature[0] =
    local_signature[0] = remote_signature[0] =
    raw_recvdmail[0] = raw_sentmail[0] = 
    allowed_precedences[0] = '\0';
  /* no defaults for those */

  if (NULL != (cp = getenv("SHELL")) && '\0' != cp[0]) {
    strfcpy(raw_shell, "$SHELL", sizeof raw_shell);
    do_expand_env("shell", shell, raw_shell, sizeof(shell));
  } else {
    /* Shell is set from /etc/passwd in initialize () */
    raw_shell[0] = '\0';
  }
  strfcpy(raw_pager, ((cp = getenv("PAGER")) == NULL)? default_pager : cp,
	  sizeof raw_pager);
  do_expand_env("pager", pager, raw_pager, sizeof(pager));
  
  strfcpy(raw_temp_dir, (cp = getenv("TMPDIR")) ? cp : default_temp,
	  sizeof raw_temp_dir);

  do_expand_env("temp_dir", temp_dir, raw_temp_dir, sizeof(temp_dir));
  if (temp_dir[strlen (temp_dir)-1] != '/')
    strfcat(temp_dir, "/", sizeof temp_dir);
  
  if ((cp = getenv("EDITOR")) != NULL && '\0' != cp[0]) {
    strfcpy(raw_editor, "$EDITOR", sizeof raw_editor);
    strfcpy(editor, cp, sizeof editor);
  } else {
    strfcpy(raw_editor, default_editor, sizeof raw_editor);
    do_expand_env("editor", editor, raw_editor, sizeof(editor));
  }

  strfcpy(alternative_editor, editor, sizeof alternative_editor);
  
  strfcpy(raw_printout, default_printout, sizeof raw_printout);
  do_expand_env("printout", printout, raw_printout, sizeof(printout));
  
  sprintf(raw_folders, "~/%s", default_folders);
  do_expand_env("folders", folders, raw_folders, sizeof(folders));
  
  sprintf(raw_calendar_file, "~/%s", dflt_calendar_file);
  do_expand_env("calendar_file", calendar_file, raw_calendar_file,
		sizeof(calendar_file));

  strfcpy(e_editor, emacs_editor, sizeof e_editor);
  strfcpy(v_editor, default_editor, sizeof v_editor);
  
  strfcpy(raw_printout, default_printout, sizeof raw_printout);
  strfcpy(printout, raw_printout, sizeof printout);

  sprintf(raw_folders, "%s/%s", home, default_folders);
  strfcpy(folders, raw_folders, sizeof folders);

  sprintf(raw_calendar_file, "%s/%s", home, dflt_calendar_file);
  strfcpy(calendar_file, raw_calendar_file, sizeof calendar_file);

#ifdef MIME
  strfcpy(charset, default_charset, sizeof charset);
  strfcpy(charset_compatlist, COMPAT_CHARSETS, 
	  sizeof charset_compatlist);
  if (!getenv("NOMETAMAIL") && getenv("MM_CHARSET"))
    strfcpy(raw_display_charset, "$MM_CHARSET",
	    sizeof raw_display_charset);
  else
    strfcpy(raw_display_charset, default_display_charset,
	    sizeof raw_display_charset);
#endif

  /* try system-wide rc file */
  file = fopen(system_rc_file, "r");
  if ( file != NULL ) {
    do_rc(file, SYSTEM_RC,system_rc_file);
    fclose(file);
  }

  return errors;
}

int read_rc_file()
{
	/** this routine does all the actual work of reading in the
	    .rc file... **/

	FILE *file;
	char buffer[SLEN], filename[SLEN], *cp, 
	     	temp[SLEN]; /* for when an option is run through expandenv */
	int  i, ch, len, err;

	errors = 0;
	/* see if the user has a $HOME/.elm directory */
	sprintf(filename, "%s/.elm", home);
	if (access(filename, 00) == -1) {
	  if(batch_only)  {
	    printf(catgets(elm_msg_cat, ElmSet, ElmBatchDirNotice, "\nNotice:\
\nThis version of ELM requires the use of a .elm directory to store your\
\nelmrc and alias files. I'd like to create the directory .elm for you\
\nand set it up, but I can't in \"batch mode\".\
\nPlease run ELM in \"normal mode\" first.\n"));
	    exit(0);
	  }

	  Raw(ON | NO_TITE);
	redraw1:
	  MCprintf(catgets(elm_msg_cat, ElmSet, ElmDirNotice, "\n\rNotice:\
\n\rThis version of ELM requires the use of a .elm directory in your home\
\n\rdirectory to store your elmrc and alias files. Shall I create the\
\n\rdirectory .elm for you and set it up (%c/%c/q)? %c%c"),
		*def_ans_yes, *def_ans_no, *def_ans_no, BACKSPACE);

	  fflush(stdout);
	  ch=ReadCh(REDRAW_MARK);
	  if (REDRAW_MARK == ch)
	    goto redraw1;

	  ch = tolower(ch);
	  if (ch == '\n' || ch == '\r') /* they took the default by pressing cr */
		ch = *def_ans_no;

	  if (ch == *def_ans_no) {
	    printf(catgets(elm_msg_cat, ElmSet, ElmDirNoticeNo,
"No.\n\rVery well. I won't create it.\n\rBut, you may run into difficulties later.\n\r"));
	    if (sleepmsg > 0)
		sleep(sleepmsg * 2);
	  }
	  else if (ch == *def_ans_yes) {
	    printf(catgets(elm_msg_cat, ElmSet, ElmDirNoticeYes,
		"Yes.\n\rGreat! I'll do it now.\n\r"));
	    create_new_elmdir();
	  }
	  else {
	    printf(catgets(elm_msg_cat, ElmSet, ElmDirNoticeQuit,
		    "Quit.\n\rOK.  Bailing out of ELM.\n\r"));
	    Raw(OFF | NO_TITE);
	    exit(0);
	  }
	  Raw(OFF | NO_TITE);
	}

	/* Look for the elmrc file */
	sprintf(filename, "%s/%s", home, elmrcfile);
	if ((file = fopen(filename, "r")) == NULL) {
	  dprint(2, (debugfile, "Warning:User has no \".elm/elmrc\" file\n\n"));

	  /* look for old-style .elmrc file in $HOME */
	  sprintf(filename, "%s/.elmrc", home);
	  if (access(filename, 00) != -1) {
	    move_old_files_to_new();

	    /* try to open elmrc file again */
	    sprintf(filename, "%s/%s", home, elmrcfile);
	    if((file = fopen(filename, "r")) == NULL) {
	      err = errno;
	      dprint(2, (debugfile,
		"Warning: could not open new \".elm/elmrc\" file.\n"));
	      dprint(2, (debugfile, "** %s **\n", error_description(err)));
	      printf(catgets(elm_msg_cat, ElmSet, ElmCouldNotOpenNewElmrc,
		"Warning: could not open new \".elm/elmrc\" file! Using default parameters.\n\r"));
	      if (sleepmsg > 0)
		    sleep(sleepmsg * 2);
	    }
	  }
	}
	if (file != NULL) {
  	  do_rc(file, LOCAL_RC, ".elm/elmrc");
	  fclose(file);
	}

/* validate/correct config_options string */

	if (config_options[0]) {
	    register char *s, *t;
	    register opts_menu *o;
	    s = shift_lower(config_options);
	    for (t = config_options; *s; ++s) {
		if (*s == '_' || *s == '^') {
		    *t++ = *s;
		    continue;
		}
		o = find_cfg_opts(*s);
		if (o != NULL)
		    *t++ = *s; /* silently remove invalid options */
		}
	    *t = '\0';
	}
	do_expand_env("folders", folders, raw_folders, sizeof(folders));

	do_expand_env("temp_dir", temp_dir, raw_temp_dir, sizeof(temp_dir));
  	if ('\0' == temp_dir[0])
	  strfcpy(temp_dir,"/tmp/", sizeof temp_dir);
	else if (temp_dir[strlen (temp_dir)-1] != '/')
	  strfcat(temp_dir, "/", sizeof temp_dir);
  
	if (raw_shell[0] != '\0') {
	  /* shell is taken from /etc/passwd in initialize() */
	  do_expand_env("shell", shell, raw_shell, sizeof(shell));
	}
	do_expand_env("editor", editor, raw_editor, sizeof(editor));

	do_expand_env("calendar_file", calendar_file, raw_calendar_file,
	    sizeof(calendar_file));

	do_expand_env("printout", printout, raw_printout, sizeof(printout));

	do_expand_env("pager", pager, raw_pager, sizeof(pager));
	if (equal(pager, "builtin+") || equal(pager, "internal+"))
		clear_pages++;

	do_expand_env("local_signature", local_signature,
	    raw_local_signature, sizeof(local_signature));
	do_expand_env("remote_signature", remote_signature,
	    raw_remote_signature, sizeof(remote_signature));

	if (equal(local_signature, remote_signature) &&
	    (equal(shift_lower(local_signature), "on") ||
	    equal(shift_lower(local_signature), "off"))) {
	    errors++;

	    printf(catgets(elm_msg_cat, ElmSet, ElmSignatureObsolete,
	"\"signature\" used in obsolete way in .elm/elmrc file. Ignored!\n\r\
\t(Signature should specify the filename to use rather than on/off.)\n\r\n"));

	    raw_local_signature[0] = raw_remote_signature[0] =
		local_signature[0] = remote_signature[0] = '\0';
	}

	/* if (hp_softkeys) hp_terminal=TRUE; */	/* must be set also! */

	allow_forms = (allow_forms?MAYBE:NO);

	if ((elm_timeout != 0) && (elm_timeout < 10)) {
	    errors++;
	    printf(catgets(elm_msg_cat, ElmSet, ElmTimeoutLTTen,
		 "Warning: timeout is set to less than 10 seconds. Ignored.\n\r"));
	    elm_timeout = 0;
	}

	if (readmsginc < 1) {
		errors++;
		printf(catgets(elm_msg_cat, ElmSet, ElmReadMessageIncrement,
		"Warning: readmsginc is set to less than 1.  Ignored.\n\r"));
		readmsginc = 1;
	}

	if (sleepmsg < 0) {
		errors++;
		printf(catgets(elm_msg_cat, ElmSet, ElmSleepMessageInvalid,
		"Warning: sleepmsg is set to less than 0.  Setting to 0.\n\r"));
		sleepmsg = 0;
	}

	/* see if the user has a folders directory */
	if (access(folders, 00) == -1) {
	  if(batch_only)  {
	    printf(catgets(elm_msg_cat, ElmSet, ElmBatchNoticeFoldersDir, "\n\
Notice:\n\
ELM requires the use of a folders directory to store your mail folders in.\n\
I'd like to create the directory %s for you,\n\
but I can't in \"batch mode\". Please run ELM in \"normal mode\" first.\n"),
		folders);
	    exit(0);
	  }

	  Raw( ON | NO_TITE );
	redraw2:
	  MCprintf(catgets(elm_msg_cat, ElmSet, ElmNoticeFoldersDir,"\n\rNotice:\n\r\
ELM requires the use of a folders directory to store your mail folders in.\n\r\
Shall I create the directory %s for you (%c/%c/q)? %c%c"),
		folders, *def_ans_yes, *def_ans_no, *def_ans_yes, BACKSPACE);

	  fflush(stdout);
	  ch=ReadCh(REDRAW_MARK);
	  if (REDRAW_MARK == ch)
	    goto redraw2;

	  ch = tolower(ch);
	  if (ch == '\n' || ch == '\r') /* they took the default by pressing cr */
		ch = *def_ans_yes;

	  if (ch == *def_ans_no) {
	    printf(catgets(elm_msg_cat, ElmSet, ElmDirNoticeNo,
"No.\n\rVery well. I won't create it.\n\rBut, you may run into difficulties later.\n\r"));
	    if (sleepmsg > 0)
		    sleep(sleepmsg * 2);
	  }
	  else if (ch == *def_ans_yes) {
	    printf(catgets(elm_msg_cat, ElmSet, ElmDirNoticeYes,
		"Yes.\n\rGreat! I'll do it now.\n\r"));
	    create_new_folders();
	  }
	  else {
	    printf(catgets(elm_msg_cat, ElmSet, ElmDirNoticeQuit,
		    "Quit.\n\rOK.  Bailing out of ELM.\n\r"));
	    Raw(OFF | NO_TITE);
	    exit(0);
	  }
	  Raw( OFF | NO_TITE );
	}

	/* If recvd_mail or sent_mail havent't yet been established in
	 * the elmrc, establish them from their defaults.
	 * Then if they begin with a metacharacter, replace it with the
	 * folders directory name.
	 */
	if(*raw_recvdmail == '\0') {
	  strfcpy(raw_recvdmail, default_recvdmail,
		  sizeof raw_recvdmail);
	}

	do_expand_env("recvd_mail", recvd_mail, raw_recvdmail,
	    sizeof(recvd_mail));

	if(metachar(recvd_mail[0])) {
	  strfcpy(buffer, &recvd_mail[1], sizeof buffer);
	  sprintf(recvd_mail, "%s/%s", folders, buffer);
	}

	if(*raw_sentmail == '\0') {
	  sprintf(raw_sentmail, default_sentmail);
	  sprintf(sent_mail, default_sentmail);
	}

	do_expand_env("sent_mail", sent_mail, raw_sentmail, sizeof(sent_mail));

	if(metachar(sent_mail[0])) {
	  strfcpy(buffer, &sent_mail[1], sizeof buffer);
	  sprintf(sent_mail, "%s/%s", folders, buffer);
	}

#ifdef MIME
	do_expand_env("display_charset", display_charset, raw_display_charset, 
		      sizeof(display_charset));

	if (strcmp(raw_metamail_path,"none") == 0 || 
	    raw_metamail_path[0] == '\0') {
	  strfcpy(raw_metamail_path,"none", sizeof raw_metamail_path);
	  strfcpy(metamail_path,"none", sizeof metamail_path);
	} else if(strcmp(raw_metamail_path,"metamail") == 0) {
	  if (getenv("NOMETAMAIL"))
	    strfcpy(metamail_path,"none", sizeof metamail_path);
	  else
	    strfcpy(metamail_path,"metamail", sizeof metamail_path);
	} else {
	  do_expand_env("metamail", metamail_path, raw_metamail_path, 
			sizeof(metamail_path));
	}
#endif

	if (debug > 10) 	/** only do this if we REALLY want debug! **/
	  dump_rc_results();

	return errors;
}

static void weedout P_((char *)); /* Prototype */
static void alternatives P_((char *));      /* Prototype */
static int breakup P_((char *, char *, char *, int, int)); /* Prototype */

void do_rc(file, lcl, filename)
     FILE *file;
     int lcl;
     char *filename;
{
	static int prev_type = 0;
	int x;
	char buffer[SLEN], word1[SLEN], word2[SLEN];

	if (!file) return;
	lineno=0;

	while (x = mail_gets(buffer, SLEN, file)) {
	    lineno++;
	    no_ret(buffer);	 	/* remove return */
	    if (buffer[0] == '#'        /* comment       */
	     || x < 2)     /* empty line    */
	      continue;

	    if(breakup(buffer, word1, word2,
		       sizeof word1, sizeof word2) == -1)
	        continue;		/* word2 is null - let default value stand */

	    if(strcmp(word1, "warnings") == 0)
		continue;		/* grandfather old keywords */

	    strfcpy(word1, shift_lower(word1), 
		    sizeof word1);	/* to lower case */
	    x = do_set(file, word1, word2, lcl, 
		       sizeof word2, filename);

	    if (x == 0) {
		if (prev_type == DT_ALT) {
		    alternatives(buffer);
		} else if (prev_type == DT_WEE) {
		    weedout(buffer);
		} else {
		    errors++;
		    printf(catgets(elm_msg_cat, ElmSet, ElmBadKeyInElmrc,
      "I can't understand keyword \"%s\" in line %d in \"%s\" file\n\r"),
			word1, lineno,filename);
		}
	    } else
		prev_type = x;
	}
}

/*
 * set the named parameter according to save_info structure.
 * This routine may call itself (DT_SYN or DT_MLT).
 * Also tags params that were set in "local" (personal) RC file
 * so we know to save them back out in "o)ptions" screen.
 * Uses an internal table to decode sort-by params...should be coupled
 * with sort_name(), etc...but...
 */

int do_set(file, word1, word2, lcl, word2_size, filename)
     FILE *file;
     int lcl;
     char *word1, *word2;
     int word2_size;
     char *filename;
{
	register int x, y;

	for (x=0; x < NUMBER_OF_SAVEABLE_OPTIONS; ++x) {
	    y = strcmp(word1, save_info[x].name);
	    if (y <= 0)
		break;
	}

	if (y != 0)
	    return(0);

	if (save_info[x].flags & FL_SYS && lcl == LOCAL_RC)
	    return(0);

	if (lcl == LOCAL_RC)
	    save_info[x].flags |= FL_LOCAL;

	switch (save_info[x].flags & DT_MASK) {
	    case DT_SYN:
		return(do_set(file, SAVE_INFO_SYN(x), word2, lcl,
			      word2_size, filename));

	    case DT_MLT:
		y=0;
		{ register char **s;
		for (s = SAVE_INFO_MLT(x); *s; ++s)
		    y |= do_set(file, *s, word2, lcl, word2_size,filename);
		}

/* a kludge that should be part of the "machine", but... */
		if (equal(save_info[x].name, "alwaysleave")) {
		    always_store = !always_store;
		}
		return(y); /* we shouldn't "or" the values into "y" */

	    case DT_STR:
	      if (strlen(word2) >= save_info[x].size_val) {
		  printf("Value of \"%s\" in line %d in \"%s\" file is too long\n\r",word1,lineno,filename);
		  errors++;
	      }
	      strfcpy(SAVE_INFO_STR(x), word2,
		      save_info[x].size_val);
	      if (save_info[x].flags & FL_NOSPC) {
		register char *s;
		for (s = SAVE_INFO_STR(x); *s; ++s)
		  if (*s == '_') *s=' ';
	      }
	      break;

	    case DT_CHR:
		*SAVE_INFO_CHR(x) = word2[0];
		break;

	    case DT_NUM:
		*SAVE_INFO_NUM(x) = atoi(word2);
		break;

	    case DT_PRM:
	        {
		    char *s = word2;
		    int m = 0;
		    char *modecharp = "rwxrwxrwx";
		    int modebit = 0400;
		    char c;

		    while ((c = *s++) != '\0') {
		    	if (c == *modecharp)
			    m |= modebit;
			else if (c != '-') {
			    errors++;
			    printf(catgets(elm_msg_cat, ElmSet, ElmBadModeInElmrc,
	  "I can't understand file permissions \"%s\" in line %d in \"%s\" file\n\r"),
			      word2, lineno,filename);
			    goto out;
			}
			modecharp++;
			modebit >>= 1;
		    }
		    if (*modecharp != '\0') {
		      errors++;
		      printf(catgets(elm_msg_cat, ElmSet, ElmBadModeInElmrc,
				     "I can't understand file permissions \"%s\" in line %d in \"%s\" file\n\r"),
			     word2, lineno,filename);
		      break;
		    }
		    *SAVE_INFO_NUM(x) = m;
		}
	out:
	        break;

	    case DT_BOL:
		if (save_info[x].flags & FL_OR)
		    *SAVE_INFO_BOL(x) |= is_it_on(word2);
		else if (save_info[x].flags & FL_AND)
		    *SAVE_INFO_BOL(x) &= is_it_on(word2);
		else
		    *SAVE_INFO_BOL(x) = is_it_on(word2);
		break;

	    case DT_SRT:
		{ static struct { char *kw; int sv; } srtval[]={
		    {"sent", SENT_DATE},
		    {"received", RECEIVED_DATE},
		    {"recieved", RECEIVED_DATE},
		    {"rec", RECEIVED_DATE},
		    {"from", SENDER},
		    {"sender", SENDER},
		    {"size", SIZE},
		    {"lines", SIZE},
		    {"subject", SUBJECT},
		    {"mailbox", MAILBOX_ORDER},
		    {"folder", MAILBOX_ORDER},
		    {"status", STATUS},
		    {NULL, 0} };
		    char *s = word2;
		    int f;

		    f = 1;
		    strfcpy(word2, shift_lower(word2), word2_size);
		    if (strncmp(s, "rev-", 4) == 0 ||
			strncmp(s, "reverse-", 8) == 0) {
			f = -f;
			s = index(s, '-') + 1;
		    }

		    for (y= 0; srtval[y].kw; y++) {
			if (equal(s, srtval[y].kw))
			    break;
		    }
		    if (srtval[y].kw) {
			*SAVE_INFO_SRT(x) = f > 0 ? srtval[y].sv : -srtval[y].sv;
		    } else {
			errors++;
			printf(catgets(elm_msg_cat, ElmSet, ElmBadSortKeyInElmrc,
	  "I can't understand sort key \"%s\" in line %d in \"%s\" file\n\r"),
			    word2, lineno, filename);
		    }
		}
		break;

	    case DT_ASR:
		{ static struct { char *kw; int sv; } srtval[]={
		    {"alias", ALIAS_SORT},
		    {"name", NAME_SORT},
		    {"text", TEXT_SORT},
		    {NULL, 0} };
		    char *s = word2;
		    int f;

		    f = 1;
		    strfcpy(word2, shift_lower(word2), word2_size);
		    if (strncmp(s, "rev-", 4) == 0 ||
			strncmp(s, "reverse-", 8) == 0) {
			f = -f;
			s = index(s, '-') + 1;
		    }

		    for (y= 0; srtval[y].kw; y++) {
			if (equal(s, srtval[y].kw))
			    break;
		    }
		    if (srtval[y].kw) {
			*SAVE_INFO_SRT(x) = f > 0 ? srtval[y].sv : -srtval[y].sv;
		    } else {
			errors++;
			printf(catgets(elm_msg_cat, ElmSet, ElmBadAliasSortInElmrc,
	"I can't understand alias sort key \"%s\" in line %d in \"%s\" file\n\r"),
			    word2, lineno,filename);
		    }
		}
		break;

	    case DT_ALT:
		alternatives(word2);
		break;

	    case DT_WEE:
		weedout(word2);
		break;
	    }

	return(save_info[x].flags & DT_MASK);
}
	
static void weedout(string)
     char *string;
{
	/** This routine is called with a list of headers to weed out.   **/

	char *strptr, *header, *p;
	int Len;
	int finished;

	finished = FALSE;
	strptr = string;
	while (!finished && (header = strtokq(strptr, "\t ,", TRUE)) != NULL) {
	  strptr = NULL;

	  if (!*header)
	    continue;

	  for (p = header; *p; ++p) {
	    if (*p == '_')
	      *p = ' ';
	  }

	  if (! istrcmp(header, "*end-of-user-headers*"))
	    break;

	  if (! istrcmp(header, "*end-of-defaults*"))
	    finished = TRUE;

	  if (! istrcmp(header, "*clear-weed-list*")) {
	    while (weedcount)
	      free(weedlist[--weedcount]);
	    header = "*end-of-defaults*";
	  }

	  if (matches_weedlist(header))
	    continue;

	  if (weedcount > MAX_IN_WEEDLIST) {
	    printf(catgets(elm_msg_cat, ElmSet, ElmTooManyWeedHeaders,
		   "Too many weed headers!  Leaving...\n\r"));
	    exit(1);
	  }
	  Len = strlen(header) + 1;
	  if ((p = malloc(Len)) == NULL) {
	    printf(catgets(elm_msg_cat, ElmSet, ElmTooManyWeedPmalloc,
		   "Too many weed headers!  Out of memory!  Leaving...\n\r"));
	    exit(1);
	  }
	  strfcpy(p, header, Len);
	  weedlist[weedcount++] = p;
	}
}

static void alternatives(string)
     char *string;
{
	/** This routine is called with a list of alternative addresses
	    that you may receive mail from (forwarded) **/

	char *strptr, *address;
	struct addr_rec *current_record, *previous_record;

	previous_record = alternative_addresses;	/* start 'er up! */
	/* move to the END of the alternative addresses list */

	if (previous_record != NULL)
	  while (previous_record->next != NULL)
	    previous_record = previous_record->next;

	strptr = (char *) string;

	while ((address = strtok(strptr, "\t ,\"'")) != NULL) {
	  if (previous_record == NULL) {
	    previous_record = (struct addr_rec *) pmalloc(sizeof 
		*alternative_addresses);

	    strfcpy(previous_record->address, address,
		    sizeof previous_record->address);
	    previous_record->next = NULL;
	    alternative_addresses = previous_record;
	  }
	  else {
	    current_record = (struct addr_rec *) pmalloc(sizeof 
		*alternative_addresses);
	  
	    strfcpy(current_record->address, address,
		    sizeof current_record->address);
	    current_record->next = NULL;
	    previous_record->next = current_record;
	    previous_record = current_record;
	  }
	  strptr = (char *) NULL;
	}
}

default_weedlist()
{
	/** Install the default headers to weed out!  Many gracious 
	    thanks to John Lebovitz for this dynamic method of 
	    allocation!
	**/

	static char *default_list[] = { ">From", "In-Reply-To:",
		       "References:", "Newsgroups:", "Received:",
		       "Apparently-To:", "Message-Id:", "Content-Type:",
		       "Content-Length", "MIME-Version",
		       "Content-Transfer-Encoding",
		       "From", "X-Mailer:", "Status:",
		       "X-ELM-",
		       "*end-of-defaults*", NULL
		     };

	for (weedcount = 0; default_list[weedcount] != (char *) 0;weedcount++){
	  int Len = strlen(default_list[weedcount]) + 1;
	  if ((weedlist[weedcount] = 
	      malloc(Len)) == NULL) {
	    printf(catgets(elm_msg_cat, ElmSet, ElmNoMemDefaultWeed,
		"\nNot enough memory for default weedlist. Leaving.\n"));
	    leave(1);
	  }
	  strfcpy(weedlist[weedcount], default_list[weedcount], Len);
	}
}

int
matches_weedlist(buffer)
char *buffer;
{
	/** returns true iff the first 'n' characters of 'buffer' 
	    match an entry of the weedlist **/
	
	register int i;

	for (i=0;i < weedcount; i++)
	  if (strincmp(buffer, weedlist[i], strlen(weedlist[i])) == 0) 
	    return(1);

	return(0);
}

static int breakup(buffer, word1, word2, size_word1, size_word2)
     char *buffer, *word1, *word2;
     int size_word1, size_word2;
{
	/** This routine breaks buffer down into word1, word2 where 
	    word1 is alpha characters only, and there is an equal
	    sign delimiting the two...
		alpha = beta
	    For lines with more than one 'rhs', word2 is set to the
	    entire string.
	    Return -1 if word 2 is of zero length, else 0.
	**/

	int i;
	
	for (i=0;
	     buffer[i] != '\0' && ok_rc_char(buffer[i]) && i < size_word1 -1; 
	     i++)
	  if (buffer[i] == '_')
	    word1[i] = '-';
	  else
	    word1[i] = tolower(buffer[i]);

	word1[i++] = '\0';	/* that's the first word! */

	/** spaces before equal sign? **/

	while (whitespace(buffer[i])) i++;
	if (buffer[i] == '=') i++;

	/** spaces after equal sign? **/

	while (whitespace(buffer[i])) i++;

	if (buffer[i] != '\0')
	  strfcpy(word2, (char *) (buffer + i), size_word2);
	else
	  word2[0] = '\0';

	/* remove trailing spaces from word2! */
	i = strlen(word2) - 1;
	while(i && (whitespace(word2[i]) || word2[i] == '\n'))
	  word2[i--] = '\0';

	return(*word2 == '\0' ? -1 : 0 );

}


/*
 * expand_env() - Perform environment expansion on a pathname.  Also
 * replaces "~" at the front of the path with the user's home directory.
 * Environment expansion occurs at the path component boundaries, e.g.
 * "/foo/$BAR/baz" is subject to expansion but "/foo/zzz$BAR/baz" is not.
 * Returns 0 if expansion successful, -1 if an error occurs (result too
 * long, cannot get home directory, or environment expansion failed).
 */
expand_env(dest, src, destlen)
char *dest;		/* pointer to space to hold the result	*/
char *src;		/* pointer to string to expand		*/
unsigned destlen;	/* size of the destination buffer	*/
{
    char envname_buf[SLEN], *envname_front, *expval;
    int check_for_env, len, ret;

    --destlen;		/* reserve space for trailing '\0' */
    ret = 0;		/* assume success */

    /*
     * Replace "~" at front with user's home directory.
     */
    if (src[0] == '~' && (src[1] == '\0' || src[1] == '/')) {
	if (home[0] == '\0') {
	    expval = "~";
	    ret = -1;
	} else {
	    expval = home;
	}
	if ((len = strlen(expval)) > destlen)
	    len = destlen;
	strfcpy(dest, expval, len+1);
	dest += len;
	destlen -= len;
	++src;
    }

    /*
     * Copy through the rest, performing $NAME expansion where appropriate.
     */
    check_for_env = TRUE;
    while (destlen > 0 && *src != '\0') {

	/*
	 * Check for "$NAME" at the start of every path component.
	 */
	if (check_for_env && *src == '$') {

	    /*
	     * Get the environment parameter name into "envname_buf"
	     * and advance "src" to the next path component.
	     */
	    envname_front = ++src;
	    if ((len = strcspn(src, "/")) == 0)
		len = strlen(src);
	    src += len;
	    if (len > sizeof(envname_buf)-1)
		len = sizeof(envname_buf)-1;
	    strfcpy(envname_buf, envname_front, len+1);

	    /*
	     * Copy over the environment expansion.  If the environment
	     * parameter is undefined then copy over unchanged and set
	     * a fail return status.
	     */
	    if ((expval = getenv(envname_buf)) == NULL) {
		*dest++ = '$';
		--destlen;
		expval = envname_buf;
		ret = -1;
	    }
	    if ((len = strlen(expval)) > destlen)
		len = destlen;
	    strfcpy(dest, expval, len+1);
	    dest += len;
	    destlen -= len;
	    check_for_env = FALSE;

	} else {

	    check_for_env = (*src == '/');
	    *dest++ = *src++;
	    --destlen;

	}

    }

    *dest = '\0';
    if (destlen <= 0)
	ret = -1;
    return ret;
}

#define on_off(s)	(s == 1? "ON " : "OFF")
dump_rc_results()
	{
	register int i, j, len = 0;
	char buf[SLEN], *s;

	for (i = 0; i < NUMBER_OF_SAVEABLE_OPTIONS; i++) {
	    extern char *sort_name(), *alias_sort_name();

	    switch (save_info[i].flags & DT_MASK) {
		case DT_SYN:
		case DT_MLT:
		    break;
		case DT_ALT:
		    break; /* not dumping addresses to debug file */
		case DT_WEE:
		    fprintf(debugfile, "\nAnd we're skipping the following headers:\n\t");

		    for (len = 8, j = 0; j < weedcount; j++) {
			if (weedlist[j][0] == '*') continue;	/* skip '*end-of-defaults*' */
			if (len + strlen(weedlist[j]) > 80) {
			    fprintf(debugfile, " \n\t");
			    len = 8;
			}
			fprintf(debugfile, "%s  ", weedlist[j]);
			len += strlen(weedlist[j]) + 3;
		    }
		    fprintf(debugfile, "\n\n");
		    break;

		default:
		    switch (save_info[i].flags&DT_MASK) {
	
		    case DT_STR:
			s = SAVE_INFO_STR(i);
			break;

		    case DT_NUM:
			sprintf(buf, "%d", *SAVE_INFO_NUM(i));
			s = buf;
			break;

		    case DT_CHR:
			sprintf(buf, "%c", *SAVE_INFO_CHR(i));
			s = buf;
			break;

		    case DT_PRM:
			s = mode_to_str(*SAVE_INFO_NUM(i));
			break;

		    case DT_BOL:
			s = on_off(*SAVE_INFO_BOL(i));
			break;

		    case DT_SRT:
			s = sort_name(SHORT);
			break;

		    case DT_ASR:
			s = alias_sort_name(SHORT);
			break;
			}

		    fprintf(debugfile, "%s = %s\n", save_info[i].name, s);
		    break;
		}
	    }
	fprintf(debugfile, "\n\n");
}

is_it_on(word)
char *word;
{
	/** Returns TRUE if the specified word is either 'ON', 'YES'
	    or 'TRUE', and FALSE otherwise.   We explicitly translate
	    to lowercase here to ensure that we have the fastest
	    routine possible - we really DON'T want to have this take
	    a long time or our startup will be major pain each time.
	**/

	static char mybuffer[NLEN];
	register int i, j;

	for (i=0, j=0; word[i] != '\0'; i++)
	  mybuffer[j++] = tolower(word[i]);
	mybuffer[j] = '\0';

	return(  (strncmp(mybuffer, "on",   2) == 0) ||
		 (strncmp(mybuffer, "yes",  3) == 0) ||
		 (strncmp(mybuffer, "true", 4) == 0)
	      );
}

char *
mode_to_str(mode)
int mode;
{
	static char modestr[9+1];	/* "rwxrwxrwx\0" */
	char *s = &modestr[0];
	char *modecharp = "rwxrwxrwx";
	int modebit = 0400;

	while (modebit != 0) {
	    if (mode & modebit)
	        *s++ = *modecharp;
	    else
		*s++ = '-';
	    modecharp++;
	    modebit >>= 1;
	}
	*s = '\0';
	return(modestr);
}
