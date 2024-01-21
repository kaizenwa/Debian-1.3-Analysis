/* 'new' config.h:
 *	A configuration file designed to make best use of the abilities
 *	of ircII, and trying to make things more intuitively understandable.
 *
 * Done by Carl v. Loesch <lynx@dm.unirm1.it>
 * Based on the 'classic' config.h by Michael Sandrof.
 * Copyright(c) 1991 - See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * Warning!  You will most likely have to make changes to your .ircrc file to
 * use this version of IRCII!  Please read the INSTALL and New2.2 files
 * supplied with the distribution for details!
 *
 * @(#)$Id: config.h.dist,v 1.9.2.3 1995/11/01 13:16:23 mrg Exp $
 */

#ifndef __config_h_
#define __config_h_

/*
 * Set your favorite default server list here.  This list should be a
 * whitespace separated hostname:portnum:password list (with portnums and
 * passwords optional).  This IS NOT an optional definition. Please set this
 * to your nearest servers.  However if you use a seperate 'ircII.servers'
 * file and the ircII can find it, this setting is overridden.
 */

#ifndef DEFAULT_SERVER
#define DEFAULT_SERVER	    "irc.limited.net"
#endif

/*
 * Define this if you want the $glob() function to be in your client.
 * There is a case for having this functino and a case against having
 * this function:
 *
 * Pro: makes it easier to write scripts like xdcc, since they can easily
 *      get at the filenames in your xdcc directory
 * Cons: with $unlink(), $rmdir(), etc, it makes it that much easier for
 *      a backdoor to do damage to your account.
 *
 * You will have to weigh the evidence and decide if you want to include it.
 */
#define INCLUDE_GLOB_FUNCTION

/*
 * Youll want to define this if your system is missing the glob()
 * call, or if its broken (solaris).
 *
 * Actually, you should #define this if you can compile the supplied
 * glob.c.  If it works, dont mess with it.
 */
#define NEED_GLOB

/*
 * Define this to be the number of seconds that you want the client
 * to block on a server-connection.  I found it annoying that it was
 * hard coded to 15 seconds, as that was never long enough to make
 * a connection.  Change it back if you want.
 *
 * This is ONLY for /server and 'server-like' connections, not for DCC.
 */
#define CONNECT_TIMEOUT 20

/*
 * If you use menus, and if you think having a seperator between the
 * menu and the rest of the window would be nice, then #define this
 * to a string which you would like to use as a repeating pattern
 * for the seperator.  The string you specify here will be repeated 
 * as many times as neccesary to span the width of the screen.
 * If you #undef this, then no seperator line will be used (this is 
 * the "old" behavior).   If you dont want the seperator, make sure 
 * you #undef this, dont just #define it to NULL! =)
 */
#define MENU_BOTTOM "  Menu"

/*
 * Set the following to 1 if you wish for IRCII not to disturb the tty's flow
 * control characters as the default.  Normally, these are ^Q and ^S.  You
 * may have to rebind them in IRCII.  Set it to 0 for IRCII to take over the
 * tty's flow control.
 */
#define USE_FLOW_CONTROL 1

/*
 * Uncomment the following if the gecos field of your /etc/passwd has other
 * information in it that you don't want as the user name (such as office
 * locations, phone numbers, etc).  The default delimiter is a comma, change
 * it if you need to. If commented out, the entire gecos field is used. 
 */
#define GECOS_DELIMITER ','

/*
 * MAIL_DELIMITER specifies the unique text that separates one mail message
 * from another in the mail spool file when using UNIX_MAIL.
 */
#define MAIL_DELIMITER "From "

/* Thanks to Allanon a very useful feature, when this is defined, ircII will
 * be able to read help files in compressed format (it will recognize the .Z)
 * If you undefine this you will spare some code, though, so better only
 * set if you are sure you are going to keep your help-files compressed.
 */
#define ZCAT "/bin/zcat"

/* Define ZSUFFIX in case we are using ZCAT */
#ifdef ZCAT
# define ZSUFFIX ".gz"
#endif

/* Make ^Z stop the irc process by default,
 * if undefined, ^Z will self-insert by default
 */
#define ALLOW_STOP_IRC /**/

/* And here is the port number for default client connections.  */
#define IRC_PORT 6667

/*
 * Uncomment the following to make ircII read a list of irc servers from
 * the ircII.servers file in the ircII library. This file should be
 * whitespace separated hostname:portnum:password (with the portnum and
 * password being optional). This server list will supercede the
 * DEFAULT_SERVER. 
*/

#define SERVERS_FILE "ircII.servers"

/* Uncomment the following if you want ircII to display the file
 * ircII.motd in the ircII library at startup.
 */
/*#define MOTD_FILE "ircII.motd"*/
/*#define PAUSE_AFTER_MOTD 0*/
/*#define ALWAYS_SHOW_MOTD*/

/* If you define UNAME_HACK, the uname information displayed in the
 * CTCP VERSION info will appear as "*IX" irregardless of any other
 * settings.  Useful for paranoid users who dont want others to know
 * that theyre running a buggy SunOS machine. >;-)
 */
#undef UNAME_HACK


/*
 * Below are the IRCII variable defaults.  For boolean variables, use 1 for
 * ON and 0 for OFF.  You may set string variable to NULL if you wish them to
 * have no value.  None of these are optional.  You may *not* comment out or
 * remove them.  They are default values for variables and are required for
 * proper compilation.
 */
#define DEFAULT_KICK_REASON "Bitch-X BaBy!"
#define DEFAULT_SHOW_CTCP_IDLE 1
#define DEFAULT_PING_TYPE 1
#define DEFAULT_FAKE_HOST NULL
#define DEFAULT_HACKPASS NULL
#define DEFAULT_MSGLOG 1
#define DEFAULT_SHOW_TOOMANY 1
#define DEFAULT_MSGLOGFILE "BitchX.away"
#define DEFAULT_KILLLOGFILE ".killlog"
#define DEFAULT_PUBLOGSTR ""
#define DEFAULT_AUTO_NSLOOKUP 0
#define DEFAULT_REASON_TYPE 1
#define DEFAULT_SHOW_SERVER_KILLS 1
#define DEFAULT_SHOW_UNAUTHS 0
#define DEFAULT_SHOW_FAKES 1
#define DEFAULT_LONG_MSG 1
#define DEFAULT_ANNOY_KICK 0
#define DEFAULT_FLOOD_KICK 1
#define DEFAULT_FLOOD_PROTECTION 1
#define DEFAULT_CTCP_FLOOD_PROTECTION 1
#define DEFAULT_CHECK_BEEP_USERS 1
#define DEFAULT_FAKEIGNORE 1
#define DEFAULT_SHOW_SERVER_CRAP 0
#define DEFAULT_PROTECT_CHANNELS "*"

#define DEFAULT_AOP_VAR 0
#define DEFAULT_KICK_OPS 1

#define DEFAULT_SHITLIST_REASON "Surplus Lamerz must go!!!!"
#define DEFAULT_MAX_AUTOGET_SIZE 2000000
#define DEFAULT_WALLOP_MSG "Message to fascists on %s"
#define DEFAULT_LLOOK_DELAY 120

#define DEFAULT_ALWAYS_SPLIT_BIGGEST 1
#define DEFAULT_AUTO_UNMARK_AWAY 0
#define DEFAULT_AUTO_WHOWAS 0
#define DEFAULT_BEEP 1
#define DEFAULT_BEEP_MAX 3
#define DEFAULT_BEEP_ON_MSG "NONE"
#define DEFAULT_BEEP_WHEN_AWAY 0
#define DEFAULT_BOLD_VIDEO 1
#define DEFAULT_CHANNEL_NAME_WIDTH 10
#define DEFAULT_CLOCK 1
#define DEFAULT_CLOCK_24HOUR 0
#define DEFAULT_CLOCK_ALARM NULL
#define DEFAULT_CMDCHARS "/"
#define DEFAULT_COMMAND_MODE 0
#define DEFAULT_COMMENT_HACK 1
#define DEFAULT_CONTINUED_LINE "          "
#define DEFAULT_DCC_BLOCK_SIZE 2048
#define DEFAULT_DISPLAY 1
#define DEFAULT_EIGHT_BIT_CHARACTERS 1
#define DEFAULT_ENCRYPT_PROGRAM NULL
#define DEFAULT_EXEC_PROTECTION 1
#define DEFAULT_FLOOD_AFTER 4
#define DEFAULT_FLOOD_RATE 5
#define DEFAULT_FLOOD_USERS 10
#define DEFAULT_FLOOD_WARNING 0
#define DEFAULT_FULL_STATUS_LINE 1
#define DEFAULT_HELP_PAGER 1
#define DEFAULT_HELP_PROMPT 1
#define DEFAULT_HELP_WINDOW 0
#define DEFAULT_HIDE_PRIVATE_CHANNELS 0
#define DEFAULT_HIGHLIGHT_CHAR "INVERSE"
#define DEFAULT_HISTORY 100
#define DEFAULT_HOLD_MODE 0
#define DEFAULT_HOLD_MODE_MAX 0
#define DEFAULT_INDENT 0
#define DEFAULT_INPUT_ALIASES 0
#if 0
#define DEFAULT_INPUT_PROMPT "<$N\\($C\\)> "
#else
#define DEFAULT_INPUT_PROMPT "[$C] "
#endif
#define DEFAULT_INPUT_PROTECTION 0
#define DEFAULT_INSERT_MODE 1
#define DEFAULT_INVERSE_VIDEO 1
#define DEFAULT_LASTLOG 1000
#define DEFAULT_LASTLOG_LEVEL "ALL"
#define DEFAULT_MSGLOG_LEVEL "MSGS NOTICES SEND_MSG"

#define DEFAULT_LOG 0
#define DEFAULT_LOGFILE "IrcLog"
#define DEFAULT_MAIL 1
#define DEFAULT_MAX_RECURSIONS 10
#define DEFAULT_NO_CTCP_FLOOD 1
#define DEFAULT_NOTIFY_HANDLER "QUIET"
#define DEFAULT_NOTIFY_LEVEL "ALL DCC"
#define DEFAULT_NOTIFY_ON_TERMINATION 0
#define DEFAULT_SAME_WINDOW_ONLY 0
#define DEFAULT_SCROLL 1
#define DEFAULT_SCROLL_LINES 1
#define DEFAULT_SEND_IGNORE_MSG 0
#define DEFAULT_SHELL "/bin/sh"
#define DEFAULT_SHELL_FLAGS "-c"
#define DEFAULT_SHELL_LIMIT 0
#define DEFAULT_SHOW_AWAY_ONCE 1
#define DEFAULT_SHOW_CHANNEL_NAMES 1
#define DEFAULT_SHOW_END_OF_MSGS 0
#define DEFAULT_SHOW_NUMERICS 0
#define DEFAULT_SHOW_STATUS_ALL 0
#define DEFAULT_SHOW_WHO_HOPCOUNT 0
#define DEFAULT_STATUS_AWAY " (away)"
#define DEFAULT_STATUS_CHANNEL "%C"
#define DEFAULT_STATUS_CHANOP "@"
#define DEFAULT_STATUS_CLOCK "%T"
#if 1
#define DEFAULT_STATUS_FORMAT "[%T][User: %N%#%A]%M [On: %@%=%C%+%W] %Q %H%B "
#define DEFAULT_STATUS_FORMAT1 "[%T][User: %N%#%A]%M [On: %@%=%C%+%W] %Q %H%B "
#define DEFAULT_STATUS_FORMAT2 "%L %! %K Aw%^ %>%D "
#define DEFAULT_STATUS_FORMAT3 "BitchX by panasync "
#else
#define DEFAULT_STATUS_FORMAT "[%R]%T %*%@%N%#%S%H%B%Q%A%C%+%I%O%M%F%U %W"
#define DEFAULT_STATUS_FORMAT1 "[%R]%T %*%@%N%#%S%H%B%Q%A%C%+%I%O%M%F%U %W"
#define DEFAULT_STATUS_FORMAT2 "%W %X %L %Y %Z %D"
#define DEFAULT_STATUS_FORMAT3 "BitchX by panasync "
#endif
#define DEFAULT_STATUS_HOLD " -- more --"
#define DEFAULT_STATUS_HOLD_LINES " (%B)"
#define DEFAULT_STATUS_INSERT ""
#define DEFAULT_STATUS_LAG " [Lag %L]"
#define DEFAULT_STATUS_MODE " (+%+)"
#define DEFAULT_STATUS_MAIL " [Mail: %M]"
#define DEFAULT_STATUS_NOTIFY " [Activity: %F]"
#define DEFAULT_STATUS_OPER "*"
#define DEFAULT_STATUS_VOICE "+"
#define DEFAULT_STATUS_OVERWRITE "(overtype) "
#define DEFAULT_STATUS_QUERY " [Query: %Q]"
#define DEFAULT_STATUS_SERVER " via %S"
#define DEFAULT_STATUS_TOPIC "%-"
#define DEFAULT_STATUS_UMODE " (+%#)"
#define DEFAULT_STATUS_USER " * type /help for help "
#define DEFAULT_STATUS_USER1 ""
#define DEFAULT_STATUS_USER2 ""
#define DEFAULT_STATUS_USER3 ""
#define DEFAULT_STATUS_USER4 ""
#define DEFAULT_STATUS_USER5 ""
#define DEFAULT_STATUS_USER6 ""
#define DEFAULT_STATUS_USER7 ""
#define DEFAULT_STATUS_USER8 ""
#define DEFAULT_STATUS_USER9 ""
#define DEFAULT_STATUS_USER10 ""
#define DEFAULT_STATUS_USER11 ""
#define DEFAULT_STATUS_USER12 ""
#define DEFAULT_STATUS_USER13 ""
#define DEFAULT_STATUS_USER14 ""
#define DEFAULT_STATUS_USER15 ""
#define DEFAULT_STATUS_USER16 ""
#define DEFAULT_STATUS_USER17 ""
#define DEFAULT_STATUS_USER18 ""
#define DEFAULT_STATUS_USER19 ""
#define DEFAULT_STATUS_WINDOW "^^^^^^^^"
#define DEFAULT_SUPPRESS_SERVER_MOTD 1
#define DEFAULT_TAB 1
#define DEFAULT_TAB_MAX 8
#define DEFAULT_UNDERLINE_VIDEO 1
#define DEFAULT_USERINFO ""
#define DEFAULT_USER_WALLOPS 0
#define DEFAULT_VERBOSE_CTCP 1
#define DEFAULT_WARN_OF_IGNORES 0
#define DEFAULT_XTERM_OPTIONS NULL
#define DEFAULT_NOVICE 0

#define DEFAULT_DISPLAY_ANSI 1

#define DEFAULT_DCC_AUTOGET 1
#define DEFAULT_DCC_DLDIR "~"
#define DEFAULT_DCC_GET_LIMIT 0
#define DEFAULT_DCC_SEND_LIMIT 5
#define DEFAULT_DCC_QUEUE_LIMIT 10
#define DEFAULT_DCC_LIMIT 10
#define DEFAULT_FLOATING_POINT_MATH 0
#define DEFAULT_LLOOK 0
#define DEFAULT_CLOAK 0
#define DEFAULT_CTOOLZ_DIR "~/.BitchX"
#define DEFAULT_SCRIPT_HELP_FILE DEFAULT_CTOOLZ_DIR"/BitchX.help"
#define DEFAULT_BITCHX_HELP_FILE DEFAULT_CTOOLZ_DIR"/BitchX.help"
#define DEFAULT_IDENT_HACK ".noident"

#define DEFAULT_AINV 0
#define DEFAULT_AUTO_REJOIN 1
#define DEFAULT_DEOPFLOOD 1
#define DEFAULT_DEOPFLOOD_TIME 30
#define DEFAULT_DEOP_ON_DEOPFLOOD 3
#define DEFAULT_DEOP_ON_KICKFLOOD 3
#define DEFAULT_KICK_IF_BANNED 0
#define DEFAULT_HACKING 0
#define DEFAULT_JOINFLOOD 1
#define DEFAULT_JOINFLOOD_TIME 50
#define DEFAULT_KICKFLOOD 1
#define DEFAULT_KICKFLOOD_TIME 30
#define DEFAULT_KICK_ON_DEOPFLOOD 4
#define DEFAULT_KICK_ON_JOINFLOOD 5
#define DEFAULT_KICK_ON_KICKFLOOD 4
#define DEFAULT_KICK_ON_NICKFLOOD 3
#define DEFAULT_KICK_ON_PUBFLOOD 40
#define DEFAULT_NICKFLOOD 1
#define DEFAULT_NICKFLOOD_TIME 30
#define DEFAULT_LAMELIST 1
#define DEFAULT_SHITLIST 1
#define DEFAULT_USERLIST 1
#define DEFAULT_PUBFLOOD 0
#define DEFAULT_PUBFLOOD_TIME 8
#define DEFAULT_SHIT_VAR 1
#define DEFAULT_EGO 1
#define DEFAULT_PAD_CHAR ' '
#define DEFAULT_STATUS_OPER_KILLS "[nk %K|ok %K]"
#define DEFAULT_STATUS_USERS "[O:%! N:%! I:%! V:%! F:%!]"

#define DEFAULT_USERMODE "+iw"  /* change this to the default usermode */
#define DEFAULT_OPERMODE "swfck"

#undef ISP_VERSION   /* define this if something offensive offends you
		      * like BitchX Roqz yur NutZ an stuff
		      */
#define HACKED_DCC_WARNING     /*
				* gives a better warning for those weird 
				* people who like to give us useful files
				* like /dev/zero
				*/
#define EMACS_KEYBINDS	       /* change this is you have problems with 
				* your keyboard
				*/
#define BITCHX_DEBUG /* define this for coredumps on panic */

#define EXEC_COMMAND
#define IDENT_FAKE              /* define this if your using a hacked
				 * ident and want to fake your username.
				 * maybe we could also use this to specify
				 * what file to write this hack to. Some
				 * examples are ~/.noident and ~/.authlie
				 */

#if 0
#define CLOAKED   "emacs"	/*
				 * define this to the program you want to
				 * show up in "ps" and "top" to hide irc
				 * from evil sys-admins.
				 */
#endif
				 
#define HARD_REFRESH

#undef OLD_RANDOM_BEHAVIOR   /* semi randomness for randm() */
/* 
 * on certain systems we can define NON_BLOCKING to 1 
 * connects are then done alot differantly. We can perform actual work 
 * in the background, while connecting. This also protects us from certain
 * "bombs" that are available.  If you have trouble with this undef 
 * the NON_BLOCKING_CONNECTS
 * non-blocking takes a fair bit of cpu on dcc sends so we undefine it
 * ONLY define FAST_DCC_SENDS if you are on a private system. your ISP will
 * not appreciate you using 30% CPU while transferring files.
 */
 
#if defined(NBLOCK_POSIX) || defined(NBLOCK_BSD) || defined(NBLOCK_SYSV)
#define NON_BLOCKING_CONNECTS 1
#define FAST_DCC_SENDS 1
#define DEFAULT_DCC_FAST 1       /* define this for fast default sends */
#else
#define DEFAULT_DCC_FAST 0
#endif

#if defined(HAVE_DLLIB)
#define WANT_DLL 1		/* define this for DLL and /loaddll commands */
#else
#undef WANT_DLL
#endif

#define MIRC_BROKEN_DCC_RESUME 1


#endif /* __config_h_ */
