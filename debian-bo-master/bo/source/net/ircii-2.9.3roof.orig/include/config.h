/*
 * phone's config.h
 *
 * copyright(c) 1993, 1994, 1995 matthew green.
 *
 * $Id: config.h,v 1.1.2.2 1996/06/26 03:11:45 mrg Exp $
 */

#ifndef __config_h_
#define __config_h_

/*
 * Set your favorite default server list here.  This list should be a
 * whitespace separated hostname:portnum:password list (with portnums and
 * passwords optional).  This IS NOT an optional definition. Please set this
 * to your nearest servers.  However if you use a seperate 'ircII.servers'
 * file and the ircII can find it, this setting is overridden.
 *
 * NOTE:  This can be set in the Makefile using
 *	configure --with-default-server=<server here>
 */
#ifndef DEFAULT_SERVER
# define DEFAULT_SERVER	    "you.need.to.change.this"
#endif

/*
 * Uncomment the following if the gecos field of your /etc/passwd has other
 * information in it that you don't want as the user name (such as office
 * locations, phone numbers, etc).  The default delimiter is a comma, change
 * it if you need to. If commented out, the entire gecos field is used. 
 */
#define GECOS_DELIMITER ','

/*
 * Set the following to 1 if you wish for IRCII not to disturb the tty's flow
 * control characters as the default.  Normally, these are ^Q and ^S.  You
 * may have to rebind them in IRCII.  Set it to 0 for IRCII to take over the
 * tty's flow control.
 */
#define USE_FLOW_CONTROL 1

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
#define ZCAT "/usr/bin/gzcat"

/* Define ZSUFFIX in case we are using ZCAT */
#ifdef ZCAT
# define ZSUFFIX ".gz"
#endif /* ZCAT */

/*
 * Make ^Z stop the irc process by default, * if undefined,
 * ^Z will self-insert by default
 */
#define ALLOW_STOP_IRC

/*
 * And here is the port number for default client connections.
 */
#define IRC_PORT 6667

/*
 * Uncomment the following to make ircII read a list of irc servers from
 * the ircII.servers file in the ircII library. This file should be
 * whitespace separated hostname:portnum:password (with the portnum and
 * password being optional). This server list will supercede the
 * DEFAULT_SERVER. 
*/

#define SERVERS_FILE "ircII.servers"

/*
 * Uncomment the following if you want ircII to display the file
 * ircII.motd in the ircII library at startup.
 */
#define MOTD_FILE "ircII.motd"
#define PAUSE_AFTER_MOTD 1

/*
 * Below are the IRCII variable defaults.  For boolean variables, use 1 for
 * ON and 0 for OFF.  You may set string variable to NULL if you wish them to
 * have no value.  None of these are optional.  You may *not* comment out or
 * remove them.  They are default values for variables and are required for
 * proper compilation.
 */
#define DEFAULT_ALWAYS_SPLIT_BIGGEST 1
#define DEFAULT_AUTO_UNMARK_AWAY 0
#define DEFAULT_AUTO_WHOWAS 0
#define DEFAULT_BEEP 1
#define DEFAULT_BEEP_MAX 3
#define DEFAULT_BEEP_ON_MSG "NONE"
#define DEFAULT_BEEP_WHEN_AWAY 0
#define DEFAULT_BOLD_VIDEO 1
#define DEFAULT_CHANNEL_NAME_WIDTH 10
#define DEFAULT_CLOCK 0
#define DEFAULT_CLOCK_24HOUR 1
#define DEFAULT_CLOCK_ALARM NULL
#define DEFAULT_CMDCHARS "/"
#define DEFAULT_COMMAND_MODE 0
#define DEFAULT_CONTINUED_LINE "  "
#define DEFAULT_DCC_BLOCK_SIZE 2048
#define DEFAULT_DISPLAY 1
#define DEFAULT_EIGHT_BIT_CHARACTERS 1
#define DEFAULT_ENCRYPT_PROGRAM NULL
#define DEFAULT_EXEC_PROTECTION 1
#define DEFAULT_FLOOD_AFTER 3
#define DEFAULT_FLOOD_RATE 3
#define DEFAULT_FLOOD_USERS 3
#define DEFAULT_FLOOD_WARNING 1
#define DEFAULT_FULL_STATUS_LINE 1
#define DEFAULT_HELP_PAGER 1
#define DEFAULT_HELP_PROMPT 1
#define DEFAULT_HELP_WINDOW 0
#define DEFAULT_HIDE_PRIVATE_CHANNELS 1
#define DEFAULT_HIGHLIGHT_CHAR "INVERSE"
#define DEFAULT_HISTORY 100
#define DEFAULT_HISTORY_FILE NULL
#define DEFAULT_HOLD_MODE 0
#define DEFAULT_HOLD_MODE_MAX 0
#define DEFAULT_INDENT 0
#define DEFAULT_INPUT_ALIASES 0
#define DEFAULT_INPUT_PROMPT "$T> "
#define DEFAULT_INPUT_PROTECTION 1
#define DEFAULT_INSERT_MODE 1
#define DEFAULT_INVERSE_VIDEO 1
#define DEFAULT_LASTLOG 512
#define DEFAULT_LASTLOG_LEVEL "ALL DCC -CRAP,WALLOP"
#define DEFAULT_LOG 0
#define DEFAULT_LOGFILE "IrcLog"
#define DEFAULT_MAIL 1
#define DEFAULT_MAX_RECURSIONS 20
#define DEFAULT_MINIMUM_SERVERS 0
#define DEFAULT_MINIMUM_USERS 0
#define DEFAULT_NO_CTCP_FLOOD 1
#define DEFAULT_NOTIFY_HANDLER "QUIET"
#define DEFAULT_NOTIFY_LEVEL "ALL DCC"
#define DEFAULT_NOTIFY_ON_TERMINATION 0
#define DEFAULT_NOVICE 0
#define DEFAULT_SAME_WINDOW_ONLY 0
#define DEFAULT_SCROLL 1
#define DEFAULT_SCROLL_LINES 1
#define DEFAULT_SEND_IGNORE_MSG 0
#define DEFAULT_SHELL "/bin/sh"
/* XXX MUST MAKE THIS PORTABLE! */
#define DEFAULT_SHELL_FLAGS "-cf"
#define DEFAULT_SHELL_LIMIT 0
#define DEFAULT_SHOW_AWAY_ONCE 1
#define DEFAULT_SHOW_CHANNEL_NAMES 1
#define DEFAULT_SHOW_END_OF_MSGS 0
#define DEFAULT_SHOW_NUMERICS 0
#define DEFAULT_SHOW_STATUS_ALL 0
#define DEFAULT_SHOW_WHO_HOPCOUNT 1
#define DEFAULT_STATUS_AWAY " (away)"
#define DEFAULT_STATUS_CHANNEL " on %C"
#define DEFAULT_STATUS_CHANOP "@"
#define DEFAULT_STATUS_CLOCK " %T"
#define DEFAULT_STATUS_FORMAT "[%R]%T %*%@%N%#%S%H%B%Q%A%C%+%I%O%M%F%U %W"
#define DEFAULT_STATUS_FORMAT1 "[%R]%T %*%@%N%#%S%H%B%Q%A%C%+%I%O%M%F%U"
#define DEFAULT_STATUS_FORMAT2 "%W %X %Y %Z"
#define DEFAULT_STATUS_HOLD " --- more ---"
#define DEFAULT_STATUS_HOLD_LINES " (%B)"
#define DEFAULT_STATUS_INSERT ""
#define DEFAULT_STATUS_MODE " (+%+)"
#define DEFAULT_STATUS_MAIL " [Mail: %M]"
#define DEFAULT_STATUS_NOTIFY " [Activity: %F]"
#define DEFAULT_STATUS_OPER "*"
#define DEFAULT_STATUS_OVERWRITE "(overtype) "
#define DEFAULT_STATUS_QUERY " [Query: %Q]"
#define DEFAULT_STATUS_SERVER " via %S"
#define DEFAULT_STATUS_UMODE " (+%#)"
#define DEFAULT_STATUS_USER ""
#define DEFAULT_STATUS_USER1 ""
#define DEFAULT_STATUS_USER2 ""
#define DEFAULT_STATUS_USER3 ""
#define DEFAULT_STATUS_WINDOW "^^^^^^^"
#define DEFAULT_SUPPRESS_SERVER_MOTD 0
#define DEFAULT_TAB 1
#define DEFAULT_TAB_MAX 4
#define DEFAULT_UNDERLINE_VIDEO 1
#define DEFAULT_USE_OLD_MSG 0
#define DEFAULT_USERINFO "disabled"
#define DEFAULT_USER_WALLOPS 1
#define DEFAULT_VERBOSE_CTCP 0
#define DEFAULT_WARN_OF_IGNORES 0
#define DEFAULT_XTERM_OPTIONS NULL

/* define these if you want them included */
#undef COMMAND_LINE_B
#undef COMMAND_LINE_L

/*
 * undefine this if you want an operator kill to quit the client
 * doesn't make much sense in this world
 */
#define NO_QUIT_ON_OPERATOR_KILL

/*
 * define this if you want to have non blocking connects (if your
 * system support them).
 */
#define NON_BLOCKING_CONNECTS

/*
 * These are my defaults.
 */
#ifdef PHONE
#define COMMAND_LINE_L
#define ON_KICK
#define INPUT_BUFFER_SIZE 460
#define DEBUG
#endif /* PHONE */

#endif /* __config_h_ */
