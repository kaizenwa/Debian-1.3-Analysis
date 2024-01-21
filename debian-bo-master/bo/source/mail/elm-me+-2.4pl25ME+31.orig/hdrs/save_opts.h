
/* @(#)$Id: save_opts.h,v 5.10 1993/08/10 18:49:32 syd Exp $ */

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.10 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 *******************************************************************************
 * Bug reports, patches, comments, suggestions should be sent to:
 *
 *	Syd Weinstein, Elm Coordinator
 *	elm@DSI.COM			dsinc!elm
 *
 *******************************************************************************
 * $Log: save_opts.h,v $
 * Revision 5.10  1993/08/10  18:49:32  syd
 * When an environment variable was given as the tmpdir definition the src
 * and dest overlapped in expand_env.  This made elm produce a garbage
 * expansion because expand_env cannot cope with overlapping src and
 * dest.  I added a new variable raw_temp_dir to keep src and dest not to
 * overlap.
 * From: Jukka Ukkonen <ukkonen@csc.fi>
 *
 * Revision 5.9  1993/06/12  05:28:06  syd
 * Missing checkin
 *
 * Revision 5.8  1993/05/08  18:56:16  syd
 * created a new elmrc variable named "readmsginc".  This specifies an
 * increment by which the message count is updated.  If this variable is
 * set to, say, 25, then the message count will only be updated every 25
 * messages, displaying 0, 25, 50, 75, and so forth.  The default value
 * of 1 will cause Elm to behave exactly as it currently does in PL21.
 * From: Eric Peterson <epeterso@encore.com>
 *
 * Revision 5.7  1993/01/20  04:01:07  syd
 * Adds a new integer parameter builtinlines.
 * if (builtinlines < 0) and (the length of the message < LINES on
 *       screen + builtinlines) use internal.
 * if (builtinlines > 0) and (length of message < builtinlines)
 * 	use internal pager.
 * if (builtinlines = 0) or none of the above conditions hold, use the
 * external pager if defined.
 * From: "John P. Rouillard" <rouilj@ra.cs.umb.edu>
 *
 * Revision 5.6  1992/10/25  02:43:50  syd
 * fix typo
 *
 * Revision 5.5  1992/10/25  02:38:27  syd
 * Add missing new flags for new elmrc options for confirm
 * From: Syd
 *
 * Revision 5.4  1992/10/24  13:44:41  syd
 * There is now an additional elmrc option "displaycharset", which
 * sets the charset supported on your terminal. This is to prevent
 * elm from calling out to metamail too often.
 * Plus a slight documentation update for MIME composition (added examples)
 * From: Klaus Steinberger <Klaus.Steinberger@Physik.Uni-Muenchen.DE>
 *
 * Revision 5.3  1992/10/17  22:58:57  syd
 * patch to make elm use (or in my case, not use) termcap/terminfo ti/te.
 * From: Graham Hudspith <gwh@inmos.co.uk>
 *
 * Revision 5.2  1992/10/17  22:42:24  syd
 * Add flags to read_rc to support command line overrides of the option.
 * From: Jan Djarv <Jan.Djarv@sa.erisoft.se>
 *
 * Revision 5.1  1992/10/03  22:34:39  syd
 * Initial checkin as of 2.4 Release at PL0
 *
 *
 ******************************************************************************/

/*
 *	Defines for the storage of options portion of the Elm system.
 */

typedef struct {
    char letter;		/* menu letter on options screen */
    char *menu;			/* menu prompt */
    int  menu_msg;		/* NLS message number of menu prompt */
    char *parm;			/* parameter to modify */
    int (*post)();		/* post processing function */
    char *one_liner;		/* one line help message */
    int  one_liner_msg; 	/* NLS message number of one line help message */
    } opts_menu;

#define DT_SYN 0 /* synonym entry (old name) */
#define DT_STR 1 /* string */
#define DT_NUM 2 /* number */
#define DT_BOL 3 /* ON/OFF (boolean) */
#define DT_CHR 4 /* character */
#define DT_WEE 5 /* weed list */
#define DT_ALT 6 /* alternate addresses list */
#define DT_SRT 7 /* sort-by code */
#define DT_MLT 8 /* multiple destinations for data */
#define DT_ASR 9 /* sort-by code */
#define DT_PRM 10 /* file permissions */
#define DT_MASK 037 /* mask for data type */
#define FL_LOCAL 0040          /* flag if changed */
#define FL_NOSPC 0100          /* flag if preserve blanks as "_" */
#define FL_SYS   0200          /* flag if only valid in system RC */
#define FL_OR    0400          /* flag if boolean value may have been set */
#define FL_AND  01000          /* flag if boolean value may have been unset */

typedef struct { 
	char 	name[NLEN]; 	/* name of instruction */
	long 	offset;		/* offset into elmrc-info file */
        int	flags;	/* DT_STR, DT_NUM, DT_BOL, etc */
        union {
          char 	*str;
          int 	*num;
          int 	*bol;
          char 	*chr;
          char 	**weed;
          struct addr_rec **alts;
          int 	*sort;
          } val;
          int size_val;
	} save_info_recs;

/*
 *	since many C compilers cannot init a union as a static
 *	init, make the same structure with just the char * for
 *	the union pointer.
 */
typedef struct { 
	char 	name[NLEN]; 	/* name of instruction */
	long 	offset;		/* offset into elmrc-info file */
        int	flags;	/* DT_STR, DT_NUM, DT_BOL, etc */
        char 	*str;
        int size_val;
	} save_info_recs_init;

#define SAVE_INFO_STR(x) (save_info[x].val.str)
#define SAVE_INFO_NUM(x) (save_info[x].val.num)
#define SAVE_INFO_BOL(x) (save_info[x].val.bol)
#define SAVE_INFO_CHR(x) (save_info[x].val.chr)
#define SAVE_INFO_WEE(x) (save_info[x].val.weed)
#define SAVE_INFO_ALT(x) (save_info[x].val.alts)
#define SAVE_INFO_SRT(x) (save_info[x].val.sort)
#define SAVE_INFO_ASR(x) (save_info[x].val.sort)
#define SAVE_INFO_SYN(x) (save_info[x].val.str)
#define SAVE_INFO_MLT(x) (save_info[x].val.weed)

#ifdef SAVE_OPTS

/* "lists" for DT_MLT.  These and DT_SYN could be eliminated if support
   of the old parameter names was dropped.
*/
char *SIGS[]={"remotesignature","localsignature",NULL},
	*ALWAYS[]={"alwayskeep","alwaysstore",NULL};

save_info_recs_init save_info_data[] = {
{"aliassortby",		-1L,DT_ASR,(char *)&alias_sortby, 0},
{"alteditor",		-1L,DT_STR,alternative_editor,
   sizeof alternative_editor},
{"alternatives",	-1L,DT_ALT,(char *)&alternative_addresses, 0},
{"alwaysdelete",	-1L,DT_BOL,(char *)&always_del, 0},
{"alwayskeep",		-1L,DT_BOL,(char *)&always_keep, 0},
{"alwaysleave",		-1L,DT_MLT,(char *)ALWAYS, 0},
{"alwaysstore",		-1L,DT_BOL,(char *)&always_store, 0},
{"arrow",		-1L,DT_BOL|FL_OR,(char *)&arrow_cursor, 0},
{"ask",			-1L,DT_BOL,(char *)&question_me, 0},
{"askcc",		-1L,DT_BOL,(char *)&prompt_for_cc, 0},
#ifdef USE_PGP
{"askpgpsig",		-1L,DT_BOL,(char *)&pgp_askpgpsig, 0},
#endif
{"attribution",		-1L,DT_STR,attribution, sizeof attribution},
{"auto_cc",		-1L,DT_SYN,"copy", 0},
{"autocopy",		-1L,DT_BOL,(char *)&auto_copy, 0},
  /* {"bounce",		-1L,DT_SYN,"bounceback", 0},
     {"bounceback",		-1L,DT_NUM,(char *)&bounceback, 0},
     */
{"builtinlines",	-1L,DT_NUM,(char *)&builtin_lines, 0},
{"calendar",		-1L,DT_STR,raw_calendar_file, 
   sizeof raw_calendar_file},
{"cc",			-1L,DT_SYN,"askcc", 0},
#ifdef MIME
{"charset",		-1L,DT_STR,charset, sizeof charset},
{"compatcharsets",		-1L,DT_STR,charset_compatlist,
   sizeof charset_compatlist},
#endif
{"configoptions",	-1L,DT_STR,config_options, sizeof config_options},
{"confirmappend",	-1L,DT_BOL,(char *)&confirm_append, 0},
{"confirmcreate",	-1L,DT_BOL,(char *)&confirm_create, 0},
{"confirmfiles",	-1L,DT_BOL,(char *)&confirm_files, 0},
{"confirmfolders",	-1L,DT_BOL,(char *)&confirm_folders, 0},
{"copy",		-1L,DT_BOL,(char *)&auto_cc, 0},
{"delete",		-1L,DT_SYN,"alwaysdelete", 0},
#ifdef MIME
{"displaycharset",	-1L,DT_STR,raw_display_charset,
   sizeof raw_display_charset},
#endif
{"easyeditor",		-1L,DT_STR,e_editor,   sizeof e_editor},
{"editor",		-1L,DT_STR,raw_editor, sizeof raw_editor},
{"escape",		-1L,DT_CHR,(char *)&escape_char, 0},
{"folders",		-1L,DT_SYN,"maildir", 0},
{"forcename",		-1L,DT_BOL,(char *)&force_name, 0},
{"form",		-1L,DT_SYN,"forms", 0},
{"forms",		-1L,DT_BOL,(char *)&allow_forms, 0},
{"fullname",		-1L,DT_STR,full_username, sizeof full_username},
{"hostdomain",		-1L,DT_STR|FL_SYS,hostdomain,   sizeof hostdomain},
{"hostfullname",	-1L,DT_STR|FL_SYS,hostfullname, sizeof hostfullname},
{"hostname",		-1L,DT_STR|FL_SYS,hostname,     sizeof hostname},
  /* {"hpkeypad",		-1L,DT_SYN,"keypad", 0}, */
  /* {"hpsoftkeys",		-1L,DT_SYN,"softkeys", 0}, */
{"keep",		-1L,DT_SYN,"keepempty", 0},
{"keepempty",		-1L,DT_BOL,(char *)&keep_empty_files, 0},
#ifdef USE_PGP
{"keeppassfor", -1L,DT_NUM,(char *)&pgp_keeppassfor, 0},
#endif
  /* {"keypad",		-1L,DT_BOL|FL_OR,(char *)&hp_terminal, 0}, */
{"localsignature",	-1L,DT_STR,raw_local_signature, 
   sizeof raw_local_signature},
{"mailbox",		-1L,DT_SYN,"receivedmail", 0},
{"maildir",		-1L,DT_STR,raw_folders, sizeof raw_folders},
{"mailedit",		-1L,DT_SYN,"editor", 0},
{"mailpermissions",	-1L,DT_PRM,(char *)&mail_permissions, 0},
{"menu",		-1L,DT_BOL|FL_AND,(char *)&mini_menu, 0},
{"menus",		-1L,DT_SYN,"menu", 0},
#ifdef MIME
{"metamail",		-1L,DT_STR,raw_metamail_path, 
   sizeof raw_metamail_path},
#endif
{"metoo",		-1L,DT_BOL,(char *)&metoo, 0},
#ifdef MIME
{"mimeforward",		-1L,DT_BOL,(char *)&mimeforward, 0},
#endif
{"movepage",		-1L,DT_BOL,(char *)&move_when_paged, 0},
{"movewhenpaged",	-1L,DT_SYN,"movepage", 0},
{"name",		-1L,DT_SYN,"fullname", 0},
{"names",		-1L,DT_BOL,(char *)&names_only, 0},
#ifdef MIME
{"noencoding",-1L,DT_NUM,(char *)&allow_no_encoding, 0},
                                 /* 1: Allow 8bit without -B8BITMIME
                                  * 2: Allow binary without -BBINARYMIME and
                                  *    and 8bit without -B8BITMIME */
{"nohdrencoding",       -1L,DT_BOL,(char *)&allow_no_hdrencoding, 0},
#endif /* MIME **/
{"noheader",		-1L,DT_BOL,(char *)&noheader, 0},
{"noheaderfwd",		-1L,DT_BOL,(char *)&noheaderfwd, 0},
{"page",		-1L,DT_SYN,"pager", 0},
#ifdef MIME
{"pagemultipart",	-1L,DT_BOL,(char *)&pagemultipart, 0},
#endif /* MIME */
{"pager",		-1L,DT_STR,raw_pager, sizeof raw_pager},
{"pointnew",		-1L,DT_BOL,(char *)&point_to_new, 0},
{"pointtonew",		-1L,DT_SYN,"pointnew", 0},
{"precedences",		-1L,DT_STR,allowed_precedences, 
   sizeof allowed_precedences},
{"prefix",		-1L,DT_STR|FL_NOSPC,prefixchars,
   sizeof prefixchars},
{"print",		-1L,DT_STR,raw_printout, sizeof raw_printout},
{"printmail",		-1L,DT_SYN,"print", 0},
{"promptafter",		-1L,DT_BOL,(char *)&prompt_after_pager, 0},
{"question",		-1L,DT_SYN,"ask", 0},
{"quoteforward",	-1L,DT_BOL,(char *)&quote_forward, 0},
{"readmsginc",		-1L,DT_NUM,(char *)&readmsginc, 0},
{"receivedmail",	-1L,DT_STR,raw_recvdmail, sizeof raw_recvdmail},
{"remotesignature",	-1L,DT_STR,raw_remote_signature, 
   sizeof raw_remote_signature},
#ifdef MIME
{"require-mime-version-for-body-encoding",
   -1L,DT_BOL,(char *)&req_mime_bodyencoding, 0},
{"require-mime-version-for-hdr-encoding",
   -1L,DT_BOL,(char *)&req_mime_hdrencoding, 0},
#endif
{"resolve",		-1L,DT_BOL,(char *)&resolve_mode, 0},
{"savebyname",		-1L,DT_SYN,"savename", 0},
{"savemail",		-1L,DT_SYN,"sentmail", 0},
{"savename",		-1L,DT_BOL,(char *)&save_by_name, 0},
{"saveto",		-1L,DT_SYN,"sentmail", 0},
{"sentmail",		-1L,DT_STR,raw_sentmail, sizeof raw_sentmail},
{"shell",		-1L,DT_STR,raw_shell, sizeof raw_shell},
#ifdef USE_PGP
{"showpgppreamble",	-1L,DT_BOL,(char *)&pgp_noarmor, 0},
#endif
{"showto",		-1L,DT_BOL,(char *)&showto, 0},
{"sigdashes",		-1L,DT_BOL,(char *)&sig_dashes, 0},
{"signature",		-1L,DT_MLT,(char *)SIGS, 0},
{"sleepmsg",		-1L,DT_NUM,(char *)&sleepmsg, 0},
  /* {"softkeys",		-1L,DT_BOL|FL_OR,(char *)&hp_softkeys, 0}, */
{"sort",		-1L,DT_SYN,"sortby", 0},
{"sortby",		-1L,DT_SRT,(char *)&sortby, 0},
{"store",		-1L,DT_SYN,"alwaysstore", 0},
#ifdef MIME
  /* {"textencoding", -1L,DT_STR,text_encoding, 0}, */
#endif
{"timeout",		-1L,DT_NUM,(char *)&elm_timeout, 0},
{"titles",		-1L,DT_BOL,(char *)&title_messages, 0},
{"tmpdir",		-1L,DT_STR,raw_temp_dir, sizeof raw_temp_dir},
#ifdef USE_PGP
{"usepgppass",          -1L,DT_BOL,(char *)&pgp_keeppass, 0},
#endif
{"userlevel",		-1L,DT_NUM,(char *)&user_level, 0},
{"username",		-1L,DT_SYN,"fullname", 0},
{"usetite",		-1L,DT_BOL|FL_AND,(char *)&use_tite, 0},
{"visualeditor",	-1L,DT_STR,v_editor, sizeof v_editor},
{"weed",		-1L,DT_BOL,(char *)&elm_filter, 0},
{"weedout",		-1L,DT_WEE,(char *)weedlist, 0},
};
int NUMBER_OF_SAVEABLE_OPTIONS=(sizeof(save_info_data)/sizeof(save_info_recs_init));
save_info_recs *save_info = (save_info_recs *) save_info_data;
#else
extern save_info_recs *save_info;
extern int NUMBER_OF_SAVEABLE_OPTIONS;
#endif

