/*
 * vars.c: All the dealing of the irc variables are handled here. 
 *
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"

#include "status.h"
#include "window.h"
#include "lastlog.h"
#include "log.h"
#include "crypt.h"
#include "history.h"
#include "notify.h"
#include "vars.h"
#include "input.h"
#include "ircaux.h"
#include "whois.h"
#include "ircterm.h"
#include "output.h"
#include "screen.h"
#include "list.h"
#include "server.h"
#include "window.h"
#include "misc.h"
#include "stack.h"
#ifdef WANT_CD
#include "cdrom.h"
#endif

char	*var_settings[] =
{
	"OFF", "ON", "TOGGLE"
};


/* For the NOVICE variable. Complain loudlly if turned off manually.  */
extern	int	load_depth;
extern	char	*auto_str;

extern  Screen	*screen_list, *current_screen;

int	loading_global = 0;

extern ChannelList default_statchan;


enum VAR_TYPES	find_variable _((char *, int *));
static	void	exec_warning _((Window *, char *, int));
static	void	input_warning _((Window *, char *, int));
static	void	eight_bit_characters _((Window *, char *, int));
static	void	set_realname _((Window *, char *, int));
static 	void	reinit_chan_int _((Window *, char *, int));
static	void	cdefault_set_int_var _((enum CVAR_TYPES, unsigned int));
static  int *set_channel_variable _((ChannelList *, enum CVAR_TYPES));
static	void	set_numeric_string _((Window *, char *, int));
static	void	set_user_mode _((Window *, char *, int));
static	void	set_ov_mode _((Window *, char *, int));
static	void	set_away_time _((Window *, char *, int));
static	void	reinit_screen _((Window *, char *, int));
static	void	reinit_status _((Window *, char *, int));
	int	save_formats _((FILE *));
/*
 * irc_variable: all the irc variables used.  Note that the integer and
 * boolean defaults are set here, which the string default value are set in
 * the init_variables() procedure 
 */

static	IrcVariable chan_variable[] =
{
	{ "AINV",			INT_TYPE_VAR,	DEFAULT_AINV, NULL, NULL, 0, 0 },
	{ "AOP",			BOOL_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "AUTO_REJOIN",		INT_TYPE_VAR,   DEFAULT_AUTO_REJOIN, NULL, NULL, 0, 0 },
	{ "BITCH",			BOOL_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "DEOPFLOOD",			BOOL_TYPE_VAR,	DEFAULT_DEOPFLOOD, NULL, NULL, 0, 0 },
	{ "DEOPFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_DEOPFLOOD_TIME, NULL, NULL, 0, 0 },
	{ "DEOP_ON_DEOPFLOOD",		INT_TYPE_VAR,	DEFAULT_DEOP_ON_DEOPFLOOD, NULL, NULL, 0, 0 },
	{ "DEOP_ON_KICKFLOOD",		INT_TYPE_VAR,	DEFAULT_DEOP_ON_KICKFLOOD, NULL, NULL, 0, 0 },
	{ "HACKING",			INT_TYPE_VAR,	DEFAULT_HACKING, NULL, NULL, 0, 0 },
	{ "JOINFLOOD",			BOOL_TYPE_VAR,	DEFAULT_JOINFLOOD, NULL, NULL, 0, 0 },
	{ "JOINFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_JOINFLOOD_TIME, NULL, NULL, 0, 0 },
	{ "KICK_IF_BANNED",		BOOL_TYPE_VAR,  DEFAULT_KICK_IF_BANNED, NULL, NULL, 0, 0 },
	{ "KICK_ON_DEOPFLOOD",		INT_TYPE_VAR,   DEFAULT_KICK_ON_DEOPFLOOD, NULL, NULL, 0, 0 },
	{ "KICK_ON_JOINFLOOD",		INT_TYPE_VAR,	DEFAULT_KICK_ON_JOINFLOOD, NULL, NULL, 0, 0 },
	{ "KICK_ON_KICKFLOOD",		INT_TYPE_VAR,   DEFAULT_KICK_ON_KICKFLOOD, NULL, NULL, 0, 0 },
	{ "KICK_ON_NICKFLOOD",		INT_TYPE_VAR,   DEFAULT_KICK_ON_NICKFLOOD, NULL, NULL, 0, 0 },
	{ "KICK_ON_PUBFLOOD",		INT_TYPE_VAR,   DEFAULT_KICK_ON_PUBFLOOD, NULL, NULL, 0, 0 },
	{ "KICKFLOOD",			BOOL_TYPE_VAR,	DEFAULT_KICKFLOOD, NULL, NULL, 0, 0 },
	{ "KICKFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_KICKFLOOD_TIME, NULL, NULL, 0, 0 },
	{ "LAMELIST",			BOOL_TYPE_VAR,  DEFAULT_LAMELIST, NULL, NULL, 0, 0 },
	{ "NICKFLOOD",			BOOL_TYPE_VAR,	DEFAULT_NICKFLOOD, NULL, NULL, 0, 0 },
	{ "NICKFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_NICKFLOOD_TIME, NULL, NULL, 0, 0 },
	{ "PUBFLOOD",			BOOL_TYPE_VAR,	DEFAULT_PUBFLOOD, NULL, NULL, 0, 0 },
	{ "PUBFLOOD_IGNORE_TIME",	INT_TYPE_VAR,	1, NULL, NULL, 0, 0 },
	{ "PUBFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_PUBFLOOD_TIME, NULL, NULL, 0, 0 },
	{ "SHITLIST",			BOOL_TYPE_VAR,  DEFAULT_SHITLIST, NULL, NULL, 0, 0 },
	{ "USERLIST",			BOOL_TYPE_VAR,  DEFAULT_USERLIST, NULL, NULL, 0, 0 },
	{ NULL, 0, 0, NULL, NULL, 0, 0 }
};

static	IrcVariable irc_variable[] =
{
	{ "AINV",			INT_TYPE_VAR,	DEFAULT_AINV, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "ALWAYS_SPLIT_BIGGEST",	BOOL_TYPE_VAR,	DEFAULT_ALWAYS_SPLIT_BIGGEST, NULL, NULL, 0, 0 },
	{ "ANNOY_KICK",			BOOL_TYPE_VAR,	DEFAULT_ANNOY_KICK, NULL, NULL, 0, VIF_BITCHX },
	{ "AOP",			BOOL_TYPE_VAR,	DEFAULT_AOP_VAR, NULL, NULL, 0, VIF_BITCHX  },
	{ "AUTOCK",			BOOL_TYPE_VAR,	0,NULL, NULL, 0, VIF_BITCHX  },
	{ "AUTOKICK_ON_VERSION",	BOOL_TYPE_VAR,  0,NULL, NULL, 0, VIF_BITCHX  },
	{ "AUTO_AWAY",			BOOL_TYPE_VAR,  1, NULL, NULL, 0, VIF_BITCHX  },
	{ "AUTO_AWAY_TIME",		INT_TYPE_VAR,	60*10, NULL, set_away_time, 0, VIF_BITCHX  },
	{ "AUTO_NSLOOKUP",		BOOL_TYPE_VAR,  DEFAULT_AUTO_NSLOOKUP, NULL, NULL, 0, VIF_BITCHX  },
	{ "AUTO_RECONNECT",		BOOL_TYPE_VAR,	1, NULL, NULL, 0, VIF_BITCHX },
	{ "AUTO_REJOIN",		INT_TYPE_VAR,   DEFAULT_AUTO_REJOIN, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "AUTO_RESPONSE",		STR_TYPE_VAR,	0, NULL, reinit_autoresponse, 0, VIF_BITCHX },
	{ "AUTO_UNMARK_AWAY",		BOOL_TYPE_VAR,	DEFAULT_AUTO_UNMARK_AWAY, NULL, NULL, 0, 0 },
	{ "AUTO_WHO_ON_JOIN",		BOOL_TYPE_VAR,	1, NULL, NULL, 0, VIF_BITCHX  },
	{ "AUTO_WHOWAS",		BOOL_TYPE_VAR,	DEFAULT_AUTO_WHOWAS, NULL, NULL, 0, 0 },
	{ "BEEP",			BOOL_TYPE_VAR,	DEFAULT_BEEP, NULL, NULL, 0, VIF_BITCHX },
	{ "BEEP_ALWAYS",		BOOL_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "BEEP_MAX",			INT_TYPE_VAR,	DEFAULT_BEEP_MAX, NULL, NULL, 0, 0 },
	{ "BEEP_ON_MSG",		STR_TYPE_VAR,	0, NULL, set_beep_on_msg, 0, VIF_BITCHX },
	{ "BEEP_WHEN_AWAY",		INT_TYPE_VAR,	DEFAULT_BEEP_WHEN_AWAY, NULL, NULL, 0, VIF_BITCHX },
	{ "BITCH",			BOOL_TYPE_VAR,  0, NULL, NULL, 0, VIF_BITCHX },
	{ "BITCHX_HELP",		STR_TYPE_VAR,   0, NULL, NULL, 0, VIF_BITCHX },
	{ "BOLD_VIDEO",			BOOL_TYPE_VAR,	DEFAULT_BOLD_VIDEO, NULL, NULL, 0, 0 },
	{ "BOT_LOG",			BOOL_TYPE_VAR,	1, NULL, NULL, 0, VIF_BITCHX  },
	{ "BOT_LOGFILE",		STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX  },
	{ "BOT_MODE",			BOOL_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "BOT_PASSWD",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "BOT_RETURN",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "BOT_TCL",			BOOL_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "CDCC_FLOOD_AFTER",		INT_TYPE_VAR,	3, NULL, NULL, 0, VIF_BITCHX  },
	{ "CDCC_FLOOD_RATE",		INT_TYPE_VAR,	4,NULL, NULL, 0, VIF_BITCHX  },
	{ "CDCC_PROMPT",		STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "CDCC_SECURITY",		INT_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX  },
#ifdef WANT_CD
	{ "CD_DEVICE",			STR_TYPE_VAR,	0, NULL, set_cd_device, 0, VIF_BITCHX },
#endif
	{ "CHANNEL_NAME_WIDTH",		INT_TYPE_VAR,	DEFAULT_CHANNEL_NAME_WIDTH, NULL, update_all_status, 0, 0 },
	{ "CHECK_BEEP_USERS",		BOOL_TYPE_VAR,  DEFAULT_CHECK_BEEP_USERS, NULL, NULL, 0, 0 },
	{ "CLIENT_INFORMATION",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "CLOAK",			INT_TYPE_VAR,	DEFAULT_CLOAK, NULL, update_all_status, 0, VIF_BITCHX },
	{ "CLOCK",			BOOL_TYPE_VAR,	DEFAULT_CLOCK, NULL, update_all_status, 0, 0 },
	{ "CLOCK_24HOUR",		BOOL_TYPE_VAR,	DEFAULT_CLOCK_24HOUR, NULL, reset_clock, 0, 0 },
	{ "CMDCHARS",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "COMMAND_MODE",		BOOL_TYPE_VAR,	DEFAULT_COMMAND_MODE, NULL, NULL, 0, 0 },
	{ "COMMENT_BREAKAGE",		BOOL_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "CONTINUED_LINE",		STR_TYPE_VAR,	0, NULL, set_continued_line, 0, 0 },
	{ "CTCP_DELAY",			INT_TYPE_VAR,	3, NULL, NULL, 0, VIF_BITCHX },
	{ "CTCP_FLOOD_PROTECTION",	BOOL_TYPE_VAR,	DEFAULT_CTCP_FLOOD_PROTECTION, NULL, NULL, 0, 0 },
	{ "CTOOLZ_DIR",			STR_TYPE_VAR,   0, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_AUTOGET",		BOOL_TYPE_VAR,	DEFAULT_DCC_AUTOGET, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_AUTORENAME",		BOOL_TYPE_VAR,	1, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_BLOCK_SIZE",		INT_TYPE_VAR,	DEFAULT_DCC_BLOCK_SIZE, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_DLDIR",			STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_FAST",			BOOL_TYPE_VAR,	DEFAULT_DCC_FAST, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_GET_LIMIT",		INT_TYPE_VAR,	DEFAULT_DCC_GET_LIMIT, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_SEND_LIMIT",		INT_TYPE_VAR,	DEFAULT_DCC_SEND_LIMIT, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_QUEUE_LIMIT",		INT_TYPE_VAR,	DEFAULT_DCC_QUEUE_LIMIT, NULL, NULL, 0, VIF_BITCHX },
	{ "DCC_ULDIR",			STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "DEBUG",			INT_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "DEFAULT_REASON",		STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "DEOPFLOOD",			BOOL_TYPE_VAR,	DEFAULT_DEOPFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "DEOPFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_DEOPFLOOD_TIME, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "DEOP_ON_DEOPFLOOD",		INT_TYPE_VAR,	DEFAULT_DEOP_ON_DEOPFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "DEOP_ON_KICKFLOOD",		INT_TYPE_VAR,	DEFAULT_DEOP_ON_KICKFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "DISPLAY",			BOOL_TYPE_VAR,	DEFAULT_DISPLAY, NULL, NULL, 0, 0 },
	{ "DISPLAY_ANSI",		BOOL_TYPE_VAR,	DEFAULT_DISPLAY_ANSI, NULL, reinit_screen, 0, VIF_BITCHX },
	{ "DOUBLE_STATUS_LINE",		INT_TYPE_VAR,	1, NULL, reinit_status, 0, VIF_BITCHX },
	{ "EIGHT_BIT_CHARACTERS",	BOOL_TYPE_VAR,	DEFAULT_EIGHT_BIT_CHARACTERS, NULL, eight_bit_characters, 0, VIF_BITCHX },
	{ "EXEC_PROTECTION",		BOOL_TYPE_VAR,	DEFAULT_EXEC_PROTECTION, NULL, exec_warning, 0, VF_NODAEMON },
	{ "FAKE_SPLIT_PATS",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },

	{ "FAKEIGNORE",			BOOL_TYPE_VAR,  DEFAULT_FAKEIGNORE, NULL, NULL, 0, 0 },
	{ "FLOATING_POINT_MATH_VAR",	BOOL_TYPE_VAR,	DEFAULT_FLOATING_POINT_MATH, NULL, NULL, 0, 0 },
	{ "FLOOD_AFTER",		INT_TYPE_VAR,	DEFAULT_FLOOD_AFTER, NULL, NULL, 0, VIF_BITCHX },
	{ "FLOOD_KICK",			BOOL_TYPE_VAR,	DEFAULT_FLOOD_KICK, NULL, NULL, 0, VIF_BITCHX },
	{ "FLOOD_PROTECTION",		BOOL_TYPE_VAR,	DEFAULT_FLOOD_PROTECTION, NULL, NULL, 0, VIF_BITCHX },
	{ "FLOOD_RATE",			INT_TYPE_VAR,	DEFAULT_FLOOD_RATE, NULL, NULL, 0, VIF_BITCHX },
	{ "FLOOD_USERS",		INT_TYPE_VAR,	DEFAULT_FLOOD_USERS, NULL, NULL, 0, VIF_BITCHX },
	{ "FLOOD_WARNING",		BOOL_TYPE_VAR,	DEFAULT_FLOOD_WARNING, NULL, NULL, 0, VIF_BITCHX },


	{ "FORMAT_381",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_391",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_443",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_471",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_473",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_474",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_475",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_476",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FORMAT_ACTION",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ACTION_AR",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ACTION_OTHER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ACTION_OTHER_AR",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ACTION_USER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ACTION_USER_AR",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ALIAS",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ASSIGN",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_AWAY",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_BACK",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_BANS",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_BANS_HEADER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_BITCH",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_BOT",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_BOT_FOOTER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_BOT_HEADER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_BWALL",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FORMAT_CHANNEL_SIGNOFF",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FORMAT_CONNECT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CSET",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_CLOAK",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_CLOAK_FUNC",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_CLOAK_FUNC_USER",STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_CLOAK_UNKNOWN",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_CLOAK_UNKNOWN_USER", STR_TYPE_VAR,0,NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_CLOAK_USER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_FUNC",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_FUNC_USER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_UNKNOWN",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_UNKNOWN_USER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_USER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_CTCP_REPLY",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_DCC_CHAT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_DCC_CONNECT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_DCC_ERROR",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_DCC_LOST",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_DCC_REQUEST",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_DCCSTATUS",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_DESYNC",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	
	{ "FORMAT_DISCONNECT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ENCRYPTED_NOTICE",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_ENCRYPTED_PRIVMSG",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_FLOOD",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_FRIEND_JOIN",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_HOOK",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FORMAT_IGNORE_INVITE",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_IGNORE_MSG",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_IGNORE_MSG_AWAY",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_IGNORE_NOTICE",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_IGNORE_WALL",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FORMAT_INVITE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_INVITE_USER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_JOIN",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_KICK",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_KICK_USER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_KILL",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_LEAVE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_LINKS",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_LIST",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_MAIL",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_MODE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_MODE_CHANNEL",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_MSG",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_MSG_GROUP",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NAMES",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NAMES_BANNER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FORMAT_NAMES_NICKCOLOR",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NAMES_NONOP",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NAMES_OP",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NAMES_OPCOLOR",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NAMES_VOICE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NAMES_VOICECOLOR",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NETADD",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NETJOIN",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NETSPLIT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NETSPLIT_HEADER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NICK_AUTO",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NICK_COMP",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NICK_MSG",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NICKNAME",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NICKNAME_OTHER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NICKNAME_USER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NONICK",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NOTE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NOTICE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NOTIFY_OFF",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NOTIFY_ON",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NOTIFY_SIGNOFF",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NOTIFY_SIGNOFF_UH",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NOTIFY_SIGNON",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_NOTIFY_SIGNON_UH",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_OPER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_PUBLIC",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_PUBLIC_AR",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_PUBLIC_MSG",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_PUBLIC_MSG_AR",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_PUBLIC_NOTICE",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_PUBLIC_NOTICE_AR",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_PUBLIC_OTHER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_PUBLIC_OTHER_AR",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_REL",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_RELM",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_RELN",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_RELS",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_RELSM",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_RELSN",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_ACTION",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_ACTION_OTHER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_AWAY",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_CTCP",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_DCC_CHAT",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_MSG",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_NOTICE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_PUBLIC",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SEND_PUBLIC_OTHER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_MSG1",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_MSG1_FROM",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_MSG2",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_MSG2_FROM",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FORMAT_SERVER_NOTICE_BOT",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_BOT1",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_BOT_ALARM",STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_CLIENT_CONNECT",STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_CLIENT_EXIT",STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_CLIENT_INVALID",STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_CLIENT_TERM",STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_FAKE",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_KILL",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_KILL_LOCAL",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_KLINE",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_NICKC",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_OPER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_REHASH",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_STATS",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_TRAFFIC_HIGH",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_TRAFFIC_NORM",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SERVER_NOTICE_UNAUTH",STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FORMAT_SET",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SET_NOVALUE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SHITLIST",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SHITLIST_FOOTER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SHITLIST_HEADER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SIGNOFF",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SILENCE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_SMODE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_STATUS",		STR_TYPE_VAR,	0, NULL, reinit_status, 0, 0},
	{ "FORMAT_STATUS1",		STR_TYPE_VAR,	0, NULL, reinit_status, 0, 0},
	{ "FORMAT_STATUS2",		STR_TYPE_VAR,	0, NULL, reinit_status, 0, 0},
	{ "FORMAT_STATUS3",		STR_TYPE_VAR,	0, NULL, reinit_status, 0, 0},
	{ "FORMAT_TIMER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_TOPIC",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_TOPIC_CHANGE",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_TOPIC_CHANGE_HEADER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_TOPIC_SETBY",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_TOPIC_UNSET",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USAGE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERLIST",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERLIST_FOOTER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERLIST_HEADER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERMODE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERS",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERS_BOT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERS_SHIT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERS_USER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_USERS_HEADER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_VERSION",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "FORMAT_WALL",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WALL_AR",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WALLOP",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHO",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_AWAY",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_BOT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_CHANNELS",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_FOOTER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_FRIEND",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_HEADER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_IDLE",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_SHIT",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_SIGNON",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_NAME",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_NICK",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_OPER",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOIS_SERVER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOLEFT_FOOTER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOLEFT_HEADER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOLEFT_USER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOWAS_HEADER",	STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WHOWAS_NICK",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WIDELIST",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},
	{ "FORMAT_WINDOW_SET",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0},

	{ "FTP",			STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "FULL_STATUS_LINE",		BOOL_TYPE_VAR,	DEFAULT_FULL_STATUS_LINE, NULL, update_all_status, 0, 0 },
	{ "HACKING",			INT_TYPE_VAR,	DEFAULT_HACKING, NULL, NULL, 0, VIF_BITCHX },
	{ "HARD_UH_NOTIFY",		BOOL_TYPE_VAR,  0, NULL, NULL, 0, VIF_BITCHX },
	{ "HELP_PAGER",			BOOL_TYPE_VAR,	DEFAULT_HELP_PAGER, NULL, NULL, 0, 0 },
	{ "HELP_PATH",			STR_TYPE_VAR,	0, NULL, NULL, 0, VF_EXPAND_PATH|VF_NODAEMON },
	{ "HELP_PROMPT",		BOOL_TYPE_VAR,	DEFAULT_HELP_PROMPT, NULL, NULL, 0, 0 },
	{ "HELP_WINDOW",		BOOL_TYPE_VAR,	DEFAULT_HELP_WINDOW, NULL, NULL, 0, 0 },
	{ "HIDE_PRIVATE_CHANNELS",	BOOL_TYPE_VAR,	DEFAULT_HIDE_PRIVATE_CHANNELS, NULL, update_all_status, 0, 0 },
	{ "HIGHLIGHT_CHAR",		STR_TYPE_VAR,	0, NULL, set_highlight_char, 0, 0 },
	{ "HISTORY",			INT_TYPE_VAR,	DEFAULT_HISTORY, NULL, set_history_size, 0, VF_NODAEMON },
	{ "HTTP_GRAB",			BOOL_TYPE_VAR,  0, NULL, NULL, 0, VIF_BITCHX }, 
	{ "HOLD_MODE",			BOOL_TYPE_VAR,	DEFAULT_HOLD_MODE, NULL, reset_line_cnt, 0, 0 },
	{ "HOLD_MODE_MAX",		INT_TYPE_VAR,	DEFAULT_HOLD_MODE_MAX, NULL, NULL, 0, 0 },
	{ "IDENT_HACK",			STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "IDLE_CHECK",			INT_TYPE_VAR,	120, NULL, NULL, 0, VIF_BITCHX },
	{ "IGNORE_TIME",		INT_TYPE_VAR,	10, NULL, NULL, 0, VIF_BITCHX },
	{ "INDENT",			BOOL_TYPE_VAR,	DEFAULT_INDENT, NULL, NULL, 0, 0 },
	{ "INPUT_ALIASES",		BOOL_TYPE_VAR,	DEFAULT_INPUT_ALIASES, NULL, NULL, 0, 0 },
	{ "INPUT_PROMPT",		STR_TYPE_VAR,	0, NULL, set_input_prompt, 0, 0 },
	{ "INPUT_PROTECTION",		BOOL_TYPE_VAR,	DEFAULT_INPUT_PROTECTION, NULL, input_warning, 0, 0 },
	{ "INSERT_MODE",		BOOL_TYPE_VAR,	DEFAULT_INSERT_MODE, NULL, update_all_status, 0, 0 },
	{ "INVERSE_VIDEO",		BOOL_TYPE_VAR,	DEFAULT_INVERSE_VIDEO, NULL, NULL, 0, 0 },
	{ "JOINFLOOD",			BOOL_TYPE_VAR,	DEFAULT_JOINFLOOD, NULL, NULL, 0, VIF_BITCHX },
	{ "JOINFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_JOINFLOOD_TIME, NULL, NULL, 0, VIF_BITCHX },
	{ "KICK_IF_BANNED",		BOOL_TYPE_VAR,  DEFAULT_KICK_IF_BANNED, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "KICK_ON_DEOPFLOOD",		INT_TYPE_VAR,   DEFAULT_KICK_ON_DEOPFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "KICK_ON_JOINFLOOD",		INT_TYPE_VAR,	DEFAULT_KICK_ON_JOINFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "KICK_ON_KICKFLOOD",		INT_TYPE_VAR,   DEFAULT_KICK_ON_KICKFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "KICK_ON_NICKFLOOD",		INT_TYPE_VAR,   DEFAULT_KICK_ON_NICKFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "KICK_ON_PUBFLOOD",		INT_TYPE_VAR,   DEFAULT_KICK_ON_PUBFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "KICK_OPS",			BOOL_TYPE_VAR,	DEFAULT_KICK_OPS, NULL, NULL, 0, VIF_BITCHX  },
	{ "KICKFLOOD",			BOOL_TYPE_VAR,	DEFAULT_KICKFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "KICKFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_KICKFLOOD_TIME, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "LAMELIST",			BOOL_TYPE_VAR,	DEFAULT_LAMELIST, NULL, NULL, 0, VIF_BITCHX },
	{ "LASTLOG",			INT_TYPE_VAR,	DEFAULT_LASTLOG, NULL, set_lastlog_size, 0, 0 },
	{ "LASTLOG_ANSI",		BOOL_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "LASTLOG_LEVEL",		STR_TYPE_VAR,	0, NULL, set_lastlog_level, 0, 0 },
	{ "LLOOK",			BOOL_TYPE_VAR,	DEFAULT_LLOOK, NULL, NULL, 0, VIF_BITCHX  },
	{ "LLOOK_DELAY",		INT_TYPE_VAR,	DEFAULT_LLOOK_DELAY, NULL, NULL, 0, VIF_BITCHX  },
	{ "LOAD_PATH",			STR_TYPE_VAR,	0, NULL, NULL, 0, VF_NODAEMON },
	{ "LOG",			BOOL_TYPE_VAR,	DEFAULT_LOG, NULL, logger, 0, 0 },
	{ "LOGFILE",			STR_TYPE_VAR,	0, NULL, set_log_file, 0, VF_NODAEMON },
	{ "MAIL",			INT_TYPE_VAR,	DEFAULT_MAIL, NULL, update_all_status, 0, VF_NODAEMON },
	{ "MAX_AUTOGET_SIZE",		INT_TYPE_VAR,	DEFAULT_MAX_AUTOGET_SIZE, NULL, 0, VIF_BITCHX },
	{ "MAX_DEOPS",			INT_TYPE_VAR,	2, NULL, 0, VIF_BITCHX  },
	{ "MAX_IDLEKICKS",		INT_TYPE_VAR,	2, NULL, 0, VIF_BITCHX  },
	{ "MAX_RECURSIONS",		INT_TYPE_VAR,	DEFAULT_MAX_RECURSIONS, NULL, NULL, 0, 0 },
	{ "MIRCS",			BOOL_TYPE_VAR,  0, NULL, NULL, 0, VIF_BITCHX  },
	{ "MODE_STRIPPER",		BOOL_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "MSGCOUNT",			INT_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "MSGLOG",			BOOL_TYPE_VAR,	DEFAULT_MSGLOG, NULL, NULL, 0, 0 },
	{ "MSGLOG_FILE",		STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX|VF_EXPAND_PATH },
	{ "MSGLOG_LEVEL",		STR_TYPE_VAR,	0, NULL, set_msglog_level, 0, VIF_BITCHX},
	{ "NEXT_SERVER_ON_LOCAL_KILL",  BOOL_TYPE_VAR,  0, NULL, NULL, 0, VIF_BITCHX  },
	{ "NICK_COMPLETION",		BOOL_TYPE_VAR,	1, NULL, NULL, 0, VIF_BITCHX  },
	{ "NICK_COMPLETION_TYPE",	INT_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX  },

	{ "NICKFLOOD",			BOOL_TYPE_VAR,	DEFAULT_NICKFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "NICKFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_NICKFLOOD_TIME, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "NO_CTCP_FLOOD",		BOOL_TYPE_VAR,	DEFAULT_NO_CTCP_FLOOD, NULL, NULL, 0, 0 },
	{ "NOTIFY_HANDLER",		STR_TYPE_VAR, 	0, NULL, set_notify_handler, 0, 0 },
	{ "NOTIFY_LEVEL",		STR_TYPE_VAR,	0, NULL, set_notify_level, 0, 0 },
	{ "NOTIFY_ON_TERMINATION",	BOOL_TYPE_VAR,	DEFAULT_NOTIFY_ON_TERMINATION, NULL, NULL, 0, VF_NODAEMON },
	{ "NOVICE",			BOOL_TYPE_VAR,	DEFAULT_NOVICE, NULL, NULL, 0, 0 },
	{ "NUM_BANMODES",		INT_TYPE_VAR,   4, NULL, NULL, 0, VIF_BITCHX  },
	{ "NUM_OPMODES",		INT_TYPE_VAR,   3, NULL, NULL, 0, VIF_BITCHX  },
	{ "OPER_MODES",			STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "OV",				BOOL_TYPE_VAR,  0, NULL, set_ov_mode, 0, VIF_BITCHX },
	{ "PAD_CHAR",			CHAR_TYPE_VAR,	DEFAULT_PAD_CHAR, NULL, NULL, 0, 0 },
	{ "PING_TYPE",			INT_TYPE_VAR,   DEFAULT_PING_TYPE, NULL, NULL, 0, VIF_BITCHX  },
	{ "PROTECT_CHANNELS",		STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "PUBFLOOD",			BOOL_TYPE_VAR,	DEFAULT_PUBFLOOD, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "PUBFLOOD_TIME",		INT_TYPE_VAR,	DEFAULT_PUBFLOOD_TIME, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "REALNAME",			STR_TYPE_VAR,	0, 0, set_realname, 0, VF_NODAEMON },
	{ "REASONFILE",			STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX  },
	{ "REASON_TYPE",		INT_TYPE_VAR,	DEFAULT_REASON_TYPE, NULL, 0, VIF_BITCHX  },
	{ "REVERSE_STATUS",		BOOL_TYPE_VAR,	0, NULL, reinit_status, 0, 0 },
	{ "SAME_WINDOW_ONLY",		BOOL_TYPE_VAR,	DEFAULT_SAME_WINDOW_ONLY, NULL, NULL, 0, 0 },
	{ "SAVEFILE",			STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX  },
	{ "SCRIPT_HELP",		STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX  },
	{ "SCROLL",			BOOL_TYPE_VAR,	DEFAULT_SCROLL, NULL, set_scroll, 0, 0 },
	{ "SCROLL_LINES",		INT_TYPE_VAR,	DEFAULT_SCROLL_LINES, NULL, set_scroll_lines, 0, 0 },
	{ "SEND_AWAY_MSG",		BOOL_TYPE_VAR,	1, NULL, NULL, 0, 0 },
	{ "SEND_IGNORE_MSG",		BOOL_TYPE_VAR,	DEFAULT_SEND_IGNORE_MSG, NULL, NULL, 0, 0 },
	{ "SERVER_PROMPT",		STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "SHELL",			STR_TYPE_VAR,	0, NULL, NULL, 0, VF_NODAEMON },
	{ "SHELL_FLAGS",		STR_TYPE_VAR,	0, NULL, NULL, 0, VF_NODAEMON },
	{ "SHELL_LIMIT",		INT_TYPE_VAR,	DEFAULT_SHELL_LIMIT, NULL, NULL, 0, VF_NODAEMON },
	{ "SHITLIST",			BOOL_TYPE_VAR,  DEFAULT_SHITLIST, NULL, reinit_chan_int, 0, VIF_BITCHX  },
	{ "SHITLIST_REASON",		STR_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX  },
	{ "SHOW_AWAY_ONCE",		BOOL_TYPE_VAR,	DEFAULT_SHOW_AWAY_ONCE, NULL, NULL, 0, 0 },
	{ "SHOW_BOTS",			BOOL_TYPE_VAR,	1, NULL, NULL, 0, VIF_BITCHX },
	{ "SHOW_CHANNEL_NAMES",		BOOL_TYPE_VAR,	DEFAULT_SHOW_CHANNEL_NAMES, NULL, NULL, 0, 0 },
	{ "SHOW_CLIENT",		BOOL_TYPE_VAR,	1, NULL, NULL, 0, 0 },
	{ "SHOW_CTCP_IDLE",		BOOL_TYPE_VAR,	DEFAULT_SHOW_CTCP_IDLE, NULL, NULL, 0, 0 },
	{ "SHOW_END_OF_MSGS",		BOOL_TYPE_VAR,	DEFAULT_SHOW_END_OF_MSGS, NULL, NULL, 0, 0 },
	{ "SHOW_FAKES",			BOOL_TYPE_VAR,	DEFAULT_SHOW_FAKES, NULL, NULL, 0, VIF_BITCHX  },
	{ "SHOW_NUMERICS",		BOOL_TYPE_VAR,	DEFAULT_SHOW_NUMERICS, NULL, NULL, 0, 0 },
	{ "SHOW_NUMERICS_STR",		STR_TYPE_VAR,	0, NULL, set_numeric_string, 0, 0 },
	{ "SHOW_SERVER_CRAP",		BOOL_TYPE_VAR,	DEFAULT_SHOW_SERVER_CRAP, NULL, NULL, 0, VIF_BITCHX },
	{ "SHOW_SERVER_KILLS",		BOOL_TYPE_VAR,	DEFAULT_SHOW_SERVER_KILLS, NULL, NULL, 0, VIF_BITCHX },
	{ "SHOW_STATUS_ALL",		BOOL_TYPE_VAR,	DEFAULT_SHOW_STATUS_ALL, NULL, update_all_status, 0, 0 },
	{ "SHOW_TOOMANY",		BOOL_TYPE_VAR,	DEFAULT_SHOW_TOOMANY, NULL, NULL, 0, VIF_BITCHX },
	{ "SHOW_TRAFFIC",		BOOL_TYPE_VAR,	1, NULL, NULL, 0, VIF_BITCHX },
	{ "SHOW_UNAUTHS",		BOOL_TYPE_VAR,	DEFAULT_SHOW_UNAUTHS, NULL, NULL, 0, VIF_BITCHX },
	{ "SHOW_WHO_HOPCOUNT", 		BOOL_TYPE_VAR,	DEFAULT_SHOW_WHO_HOPCOUNT, NULL, NULL, 0, 0 },
	{ "STATUS_AWAY",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_CHANNEL",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_CHANOP",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_CLOCK",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_FORMAT",              STR_TYPE_VAR,   0, NULL, build_status, 0, 0 },
	{ "STATUS_FORMAT1",             STR_TYPE_VAR,   0, NULL, build_status, 0, 0 },
	{ "STATUS_FORMAT2",             STR_TYPE_VAR,   0, NULL, build_status, 0, 0 },
	{ "STATUS_FORMAT3",             STR_TYPE_VAR,   0, NULL, build_status, 0, 0 },
	{ "STATUS_HOLD",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_HOLD_LINES",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_INSERT",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_LAG",			STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_MAIL",		STR_TYPE_VAR,	0, NULL, build_status, 0, VF_NODAEMON },
	{ "STATUS_MODE",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_NOTIFY",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_OPER",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_OPER_KILLS",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_OVERWRITE",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_QUERY",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_SERVER",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_TOPIC",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_UMODE",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USERS",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER1",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER2",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER3",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER4",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER5",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER6",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER7",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER8",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER9",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER10",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER11",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER12",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER13",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER14",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER15",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER16",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER17",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER18",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_USER19",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_VOICE",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "STATUS_WINDOW",		STR_TYPE_VAR,	0, NULL, build_status, 0, 0 },
	{ "SUPPRESS_SERVER_MOTD",	BOOL_TYPE_VAR,	DEFAULT_SUPPRESS_SERVER_MOTD, NULL, NULL, 0, VF_NODAEMON },
	{ "TAB",			BOOL_TYPE_VAR,	DEFAULT_TAB, NULL, NULL, 0, 0 },
	{ "TAB_MAX",			INT_TYPE_VAR,	DEFAULT_TAB_MAX, NULL, NULL, 0, 0 },
	{ "UNDERLINE_VIDEO",		BOOL_TYPE_VAR,	DEFAULT_UNDERLINE_VIDEO, NULL, NULL, 0, 0 },
	{ "USER_INFORMATION", 		STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "USERLIST",			BOOL_TYPE_VAR,  DEFAULT_USERLIST, NULL, reinit_chan_int, 0, VIF_BITCHX },
	{ "USERMODE",			STR_TYPE_VAR,	0, NULL, set_user_mode, 0, VIF_BITCHX},
	{ "VERBOSE_CTCP",		BOOL_TYPE_VAR,	DEFAULT_VERBOSE_CTCP, NULL, NULL, 0, 0 },
	{ "WALLOP_MSG",			STR_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ "WARN_OF_IGNORES",		BOOL_TYPE_VAR,	DEFAULT_WARN_OF_IGNORES, NULL, NULL, 0, 0 },
	{ "_CDCC_CLOSE_IDLE_SENDS_TIME",INT_TYPE_VAR,	55,NULL, NULL, 0, VIF_BITCHX },
	{ "_CDCC_MINSPEED_TIME",	INT_TYPE_VAR,	0, NULL, NULL, 0, VIF_BITCHX },
	{ "_CDCC_PACKS_OFFERED",	INT_TYPE_VAR,	0, NULL, NULL, 0, 0 },
	{ NULL, 0, 0, NULL, NULL, 0, 0 }
};

#ifdef WANT_DLL
IrcVariableDll *dll_variable = NULL;
#endif

/*
 * get_string_var: returns the value of the string variable given as an index
 * into the variable table.  Does no checking of variable types, etc 
 */
char	*	get_string_var(enum VAR_TYPES var)
{
	return (irc_variable[var].string);
}

#ifdef WANT_DLL
char *get_dllstring_var(char *typestr)
{
IrcVariableDll *dll = NULL;
	if (typestr)
		dll = (IrcVariableDll *) find_in_list((List **)&dll_variable, typestr, 0);
	return (dll?dll->string:NULL);
}
#endif

char	* cget_string_var(enum CVAR_TYPES var)
{
	return (chan_variable[var].string);
}

/*
 * get_int_var: returns the value of the integer string given as an index
 * into the variable table.  Does no checking of variable types, etc 
 */
int get_int_var(enum VAR_TYPES var)
{
	return (irc_variable[var].integer);
}

#ifdef WANT_DLL
int get_dllint_var(char *typestr)
{
IrcVariableDll *dll = NULL;
	if (typestr)
		dll = (IrcVariableDll *) find_in_list((List **)&dll_variable, typestr, 0);
	return (dll?dll->integer:-1);
}
#endif

int cget_int_var(enum CVAR_TYPES var, char *channel)
{
ChannelList *chan;
int *tmp = NULL;
	if (channel)
	{
		if ((chan = (ChannelList *)find_in_list((List **)&(server_list[from_server].chan_list), channel, 0)))
		{	
			if ((tmp = set_channel_variable(chan, var)))
				return (*tmp);
		}
	}
	return (-1);
}

/*
 * set_string_var: sets the string variable given as an index into the
 * variable table to the given string.  If string is null, the current value
 * of the string variable is freed and set to null 
 */
void set_string_var(enum VAR_TYPES var, char *string)
{
	if (string)
		malloc_strcpy(&(irc_variable[var].string), string);
	else
		new_free(&(irc_variable[var].string));
}

#ifdef WANT_DLL
void set_dllstring_var(char *typestr, char *string)
{
	if (typestr)
	{
		IrcVariableDll *dll = NULL;
		if (typestr)
			dll = (IrcVariableDll *) find_in_list((List **)&dll_variable, typestr, 0);
		if (!dll)
			return;
		if (string)
			malloc_strcpy(&dll->string, string);
		else
			new_free(&dll->string);
	}
}
#endif

void cset_string_var(enum CVAR_TYPES var, char *string)
{
	if (string)
		malloc_strcpy(&(chan_variable[var].string), string);
	else
		new_free(&(chan_variable[var].string));
}


/*
 * set_int_var: sets the integer value of the variable given as an index into
 * the variable table to the given value 
 */
void set_int_var(enum VAR_TYPES var, unsigned int value)
{
	if (var == NOVICE_VAR && !load_depth && !value)
	{
say("WARNING: Setting NOVICE to OFF enables commands in your client which");
say("         could be used by others on IRC to control your IRC session");
say("         or compromise security on your machine. If somebody has");
say("         asked you to do this, and you do not know EXACTLY why, or if");
say("         you are not ABSOLUTELY sure what you are doing, you should");
say("         immediately /SET NOVICE ON and ask the IRC operators about");
say("         the commands you have been asked to enter on channel");
say("         #Twilight_Zone.");
	}
	irc_variable[var].integer = value;
}

#ifdef WANT_DLL
void set_dllint_var(char *typestr, unsigned int value)
{
	if (typestr)
	{
		IrcVariableDll *dll = NULL;
		if (typestr)
			dll = (IrcVariableDll *) find_in_list((List **)&dll_variable, typestr, 0);
		if (!dll)
			return;
		dll->integer = value;
	}
}
#endif

void cset_int_var(enum CVAR_TYPES var, unsigned int value)
{
char *channel;
ChannelList *chan;
int *tmp = NULL;
	channel = get_channel_by_refnum(0);
	if (channel)
	{
		if ((chan = (ChannelList *)find_in_list((List **)&(server_list[from_server].chan_list), channel, 0)))
		{	
			if ((tmp = set_channel_variable(chan, var)))
				*tmp = value;
		}
	}
}

#ifdef __STDC__
void cdefault_set_int_var(enum CVAR_TYPES var, unsigned int value)
#else
void cdefault_set_int_var(var, value)
	enum CVAR_TYPES var;
	unsigned int value;
#endif
{
int *tmp = NULL;
	tmp = set_channel_variable(&default_statchan, var);	
	chan_variable[var].integer = *tmp = value;
}


void reinit_chan_int(Window *win, char *unused, int tmp)
{
	cdefault_set_int_var(AOP_CVAR, get_int_var(AOP_VAR));
	cdefault_set_int_var(AINV_CVAR, get_int_var(AINV_VAR));
	cdefault_set_int_var(AUTO_REJOIN_CVAR, get_int_var(AUTO_REJOIN_VAR));
	cdefault_set_int_var(DEOP_ON_DEOPFLOOD_CVAR, get_int_var(DEOP_ON_DEOPFLOOD_VAR));
	cdefault_set_int_var(DEOP_ON_KICKFLOOD_CVAR, get_int_var(DEOP_ON_KICKFLOOD_VAR));
	cdefault_set_int_var(DEOPFLOOD_CVAR, get_int_var(DEOPFLOOD_VAR));
	cdefault_set_int_var(DEOPFLOOD_TIME_CVAR, get_int_var(DEOPFLOOD_TIME_VAR));
	cdefault_set_int_var(HACKING_CVAR, get_int_var(HACKING_VAR));

	cdefault_set_int_var(KICK_ON_DEOPFLOOD_CVAR, get_int_var(KICK_ON_DEOPFLOOD_VAR));
	cdefault_set_int_var(KICK_ON_JOINFLOOD_CVAR, get_int_var(KICK_ON_JOINFLOOD_VAR));
	cdefault_set_int_var(KICK_ON_KICKFLOOD_CVAR, get_int_var(KICK_ON_KICKFLOOD_VAR));
	cdefault_set_int_var(KICK_ON_NICKFLOOD_CVAR, get_int_var(KICK_ON_NICKFLOOD_VAR));
	cdefault_set_int_var(KICK_ON_PUBFLOOD_CVAR, get_int_var(KICK_ON_PUBFLOOD_VAR));

	cdefault_set_int_var(KICKFLOOD_CVAR, get_int_var(KICKFLOOD_VAR));
	cdefault_set_int_var(KICKFLOOD_TIME_CVAR, get_int_var(KICKFLOOD_TIME_VAR));
	cdefault_set_int_var(NICKFLOOD_CVAR, get_int_var(NICKFLOOD_VAR));
	cdefault_set_int_var(NICKFLOOD_TIME_CVAR, get_int_var(NICKFLOOD_TIME_VAR));
	cdefault_set_int_var(PUBFLOOD_CVAR, get_int_var(PUBFLOOD_VAR));
	cdefault_set_int_var(PUBFLOOD_TIME_CVAR, get_int_var(PUBFLOOD_TIME_VAR));
	cdefault_set_int_var(JOINFLOOD_CVAR, get_int_var(JOINFLOOD_VAR));
	cdefault_set_int_var(JOINFLOOD_TIME_CVAR, get_int_var(JOINFLOOD_TIME_VAR));

	cdefault_set_int_var(USERLIST_CVAR, get_int_var(USERLIST_VAR));
	cdefault_set_int_var(SHITLIST_CVAR, get_int_var(SHITLIST_VAR));
	cdefault_set_int_var(LAMELIST_CVAR, get_int_var(LAMELIST_VAR));
	cdefault_set_int_var(KICK_IF_BANNED_CVAR, get_int_var(KICK_IF_BANNED_VAR));
	cdefault_set_int_var(PUBFLOOD_IGNORE_TIME_CVAR, 1);
	cdefault_set_int_var(BITCH_CVAR, get_int_var(BITCH_VAR));
}

void init_window_variables(Window *win)
{
	if (!win)
		return;
	wset_string_var(win, STATUS_FORMAT_VAR, get_string_var(STATUS_FORMAT_VAR));
	wset_string_var(win, STATUS_FORMAT1_VAR, get_string_var(STATUS_FORMAT1_VAR));
	wset_string_var(win, STATUS_FORMAT2_VAR, get_string_var(STATUS_FORMAT2_VAR));
	wset_string_var(win, STATUS_FORMAT3_VAR, get_string_var(STATUS_FORMAT3_VAR));
	wset_string_var(win, STATUS_AWAY_VAR, get_string_var(STATUS_AWAY_VAR));
	wset_string_var(win, STATUS_CHANNEL_VAR, get_string_var(STATUS_CHANNEL_VAR));
	wset_string_var(win, STATUS_CHANOP_VAR, get_string_var(STATUS_CHANOP_VAR));
	wset_string_var(win, STATUS_CLOCK_VAR, get_string_var(STATUS_CLOCK_VAR));
	wset_string_var(win, STATUS_HOLD_VAR, get_string_var(STATUS_HOLD_VAR));
	wset_string_var(win, STATUS_HOLD_LINES_VAR, get_string_var(STATUS_HOLD_LINES_VAR));
	wset_string_var(win, STATUS_INSERT_VAR, get_string_var(STATUS_INSERT_VAR));
	wset_string_var(win, STATUS_LAG_VAR, get_string_var(STATUS_LAG_VAR));
	wset_string_var(win, STATUS_MAIL_VAR, get_string_var(STATUS_MAIL_VAR));
	wset_string_var(win, STATUS_MODE_VAR, get_string_var(STATUS_MODE_VAR));
	wset_string_var(win, STATUS_OPER_VAR, get_string_var(STATUS_OPER_VAR));
	wset_string_var(win, STATUS_VOICE_VAR, get_string_var(STATUS_VOICE_VAR));
	wset_string_var(win, STATUS_OPER_KILLS_VAR, get_string_var(STATUS_OPER_KILLS_VAR));
	wset_string_var(win, STATUS_OVERWRITE_VAR, get_string_var(STATUS_OVERWRITE_VAR));
	wset_string_var(win, STATUS_QUERY_VAR, get_string_var(STATUS_QUERY_VAR));
	wset_string_var(win, STATUS_SERVER_VAR, get_string_var(STATUS_SERVER_VAR));
	wset_string_var(win, STATUS_TOPIC_VAR, get_string_var(STATUS_TOPIC_VAR));
	wset_string_var(win, STATUS_UMODE_VAR, get_string_var(STATUS_UMODE_VAR));
	wset_string_var(win, STATUS_USER_VAR, get_string_var(STATUS_USER_VAR));
	wset_string_var(win, STATUS_USERS_VAR, get_string_var(STATUS_USERS_VAR));
	wset_string_var(win, STATUS_USER1_VAR, get_string_var(STATUS_USER1_VAR));
	wset_string_var(win, STATUS_USER2_VAR, get_string_var(STATUS_USER2_VAR));
	wset_string_var(win, STATUS_USER3_VAR, get_string_var(STATUS_USER3_VAR));
	wset_string_var(win, STATUS_USER4_VAR, get_string_var(STATUS_USER4_VAR));
	wset_string_var(win, STATUS_USER5_VAR, get_string_var(STATUS_USER5_VAR));
	wset_string_var(win, STATUS_USER6_VAR, get_string_var(STATUS_USER6_VAR));
	wset_string_var(win, STATUS_USER7_VAR, get_string_var(STATUS_USER7_VAR));
	wset_string_var(win, STATUS_USER8_VAR, get_string_var(STATUS_USER8_VAR));
	wset_string_var(win, STATUS_USER9_VAR, get_string_var(STATUS_USER9_VAR));
	wset_string_var(win, STATUS_USER10_VAR, get_string_var(STATUS_USER10_VAR));
	wset_string_var(win, STATUS_USER11_VAR, get_string_var(STATUS_USER11_VAR));
	wset_string_var(win, STATUS_USER12_VAR, get_string_var(STATUS_USER12_VAR));
	wset_string_var(win, STATUS_USER13_VAR, get_string_var(STATUS_USER13_VAR));
	wset_string_var(win, STATUS_USER14_VAR, get_string_var(STATUS_USER14_VAR));
	wset_string_var(win, STATUS_USER15_VAR, get_string_var(STATUS_USER15_VAR));
	wset_string_var(win, STATUS_USER16_VAR, get_string_var(STATUS_USER16_VAR));
	wset_string_var(win, STATUS_USER17_VAR, get_string_var(STATUS_USER17_VAR));
	wset_string_var(win, STATUS_USER18_VAR, get_string_var(STATUS_USER18_VAR));
	wset_string_var(win, STATUS_USER19_VAR, get_string_var(STATUS_USER19_VAR));
	wset_string_var(win, STATUS_WINDOW_VAR, get_string_var(STATUS_WINDOW_VAR));
	wset_string_var(win, STATUS_NOTIFY_VAR, get_string_var(STATUS_NOTIFY_VAR));
}

/*
 * init_variables: initializes the string variables that can't really be
 * initialized properly above 
 */
void init_variables()
{
int old_display = window_display;
	window_display = 0;

	set_string_var(SCRIPT_HELP_VAR, DEFAULT_SCRIPT_HELP_FILE);
	set_string_var(BITCHX_HELP_VAR, DEFAULT_BITCHX_HELP_FILE);
	set_string_var(IDENT_HACK_VAR, DEFAULT_IDENT_HACK);
	set_string_var(AUTO_RESPONSE_VAR, nickname);
#if 1
	set_string_var(SHOW_NUMERICS_STR_VAR, "[1;31mù[0m[1;37mí[1;31mù[0m");
	set_numeric_string(curr_scr_win, "[1;31mù[0m[1;37mí[1;31mù[0m", 0);
#else
	set_string_var(SHOW_NUMERICS_STR_VAR, "***");
	set_numeric_string(curr_scr_win, "***", 0);
#endif						
	reinit_autoresponse(curr_scr_win, nickname, 0);
	set_string_var(MSGLOGFILE_VAR, DEFAULT_MSGLOGFILE);
	set_string_var(MSGLOG_LEVEL_VAR, DEFAULT_MSGLOG_LEVEL);
	set_string_var(CTOOLZ_DIR_VAR, DEFAULT_CTOOLZ_DIR);
	set_int_var(LLOOK_DELAY_VAR, DEFAULT_LLOOK_DELAY);
	set_int_var(MSGCOUNT_VAR, 0);

	set_string_var(SHITLIST_REASON_VAR, DEFAULT_SHITLIST_REASON);
	set_string_var(WALLOP_MSG_VAR, DEFAULT_WALLOP_MSG);
	set_string_var(PROTECT_CHANNELS_VAR, DEFAULT_PROTECT_CHANNELS);
	set_string_var(DEFAULT_REASON_VAR, DEFAULT_KICK_REASON);
	set_string_var(DCC_DLDIR_VAR, DEFAULT_DCC_DLDIR);
	set_string_var(DCC_ULDIR_VAR, DEFAULT_DCC_DLDIR);	

	set_string_var(CMDCHARS_VAR, DEFAULT_CMDCHARS);
	set_string_var(LOGFILE_VAR, DEFAULT_LOGFILE);
	set_string_var(SHELL_VAR, DEFAULT_SHELL);
	set_string_var(SHELL_FLAGS_VAR, DEFAULT_SHELL_FLAGS);
	set_string_var(CONTINUED_LINE_VAR, DEFAULT_CONTINUED_LINE);
	set_string_var(INPUT_PROMPT_VAR, DEFAULT_INPUT_PROMPT);
	set_string_var(HIGHLIGHT_CHAR_VAR, DEFAULT_HIGHLIGHT_CHAR);
	set_string_var(LASTLOG_LEVEL_VAR, DEFAULT_LASTLOG_LEVEL);
	set_string_var(NOTIFY_HANDLER_VAR, DEFAULT_NOTIFY_HANDLER);
	set_string_var(NOTIFY_LEVEL_VAR, DEFAULT_NOTIFY_LEVEL);
	set_string_var(REALNAME_VAR, realname);

	set_string_var(STATUS_FORMAT_VAR, DEFAULT_STATUS_FORMAT);
	set_string_var(STATUS_FORMAT1_VAR, DEFAULT_STATUS_FORMAT1);
	set_string_var(STATUS_FORMAT2_VAR, DEFAULT_STATUS_FORMAT2);
	set_string_var(STATUS_FORMAT3_VAR, DEFAULT_STATUS_FORMAT3);


	set_string_var(STATUS_AWAY_VAR, DEFAULT_STATUS_AWAY);
	set_string_var(STATUS_CHANNEL_VAR, DEFAULT_STATUS_CHANNEL);
	set_string_var(STATUS_CHANOP_VAR, DEFAULT_STATUS_CHANOP);
	set_string_var(STATUS_CLOCK_VAR, DEFAULT_STATUS_CLOCK);
	set_string_var(STATUS_HOLD_VAR, DEFAULT_STATUS_HOLD);
	set_string_var(STATUS_HOLD_LINES_VAR, DEFAULT_STATUS_HOLD_LINES);
	set_string_var(STATUS_INSERT_VAR, DEFAULT_STATUS_INSERT);
	set_string_var(STATUS_LAG_VAR, DEFAULT_STATUS_LAG);
	set_string_var(STATUS_MAIL_VAR, DEFAULT_STATUS_MAIL);
	set_string_var(STATUS_MODE_VAR, DEFAULT_STATUS_MODE);
	set_string_var(STATUS_OPER_VAR, DEFAULT_STATUS_OPER);
	set_string_var(STATUS_VOICE_VAR, DEFAULT_STATUS_VOICE);
	set_string_var(STATUS_NOTIFY_VAR, DEFAULT_STATUS_NOTIFY);
	set_string_var(STATUS_OPER_KILLS_VAR, DEFAULT_STATUS_OPER_KILLS);
	set_string_var(STATUS_OVERWRITE_VAR, DEFAULT_STATUS_OVERWRITE);
	set_string_var(STATUS_QUERY_VAR, DEFAULT_STATUS_QUERY);
	set_string_var(STATUS_SERVER_VAR, DEFAULT_STATUS_SERVER);
	set_string_var(STATUS_TOPIC_VAR, DEFAULT_STATUS_TOPIC);
	set_string_var(STATUS_UMODE_VAR, DEFAULT_STATUS_UMODE);
	set_string_var(STATUS_USER_VAR, DEFAULT_STATUS_USER);
	set_string_var(STATUS_USERS_VAR, DEFAULT_STATUS_USERS);
	set_string_var(STATUS_USER1_VAR, DEFAULT_STATUS_USER1);
	set_string_var(STATUS_USER2_VAR, DEFAULT_STATUS_USER2);
	set_string_var(STATUS_USER3_VAR, DEFAULT_STATUS_USER3);
	set_string_var(STATUS_USER4_VAR, DEFAULT_STATUS_USER4);
	set_string_var(STATUS_USER5_VAR, DEFAULT_STATUS_USER5);
	set_string_var(STATUS_USER6_VAR, DEFAULT_STATUS_USER6);
	set_string_var(STATUS_USER7_VAR, DEFAULT_STATUS_USER7);
	set_string_var(STATUS_USER8_VAR, DEFAULT_STATUS_USER8);
	set_string_var(STATUS_USER9_VAR, DEFAULT_STATUS_USER9);
	set_string_var(STATUS_USER10_VAR, DEFAULT_STATUS_USER10);
	set_string_var(STATUS_USER11_VAR, DEFAULT_STATUS_USER11);
	set_string_var(STATUS_USER12_VAR, DEFAULT_STATUS_USER12);
	set_string_var(STATUS_USER13_VAR, DEFAULT_STATUS_USER13);
	set_string_var(STATUS_USER14_VAR, DEFAULT_STATUS_USER14);
	set_string_var(STATUS_USER15_VAR, DEFAULT_STATUS_USER15);
	set_string_var(STATUS_USER16_VAR, DEFAULT_STATUS_USER16);
	set_string_var(STATUS_USER17_VAR, DEFAULT_STATUS_USER17);
	set_string_var(STATUS_USER18_VAR, DEFAULT_STATUS_USER18);
	set_string_var(STATUS_USER19_VAR, DEFAULT_STATUS_USER19);
	set_string_var(STATUS_WINDOW_VAR, DEFAULT_STATUS_WINDOW);

	set_string_var(USERINFO_VAR, DEFAULT_USERINFO);

	set_string_var(USERMODE_VAR, DEFAULT_USERMODE);
	set_user_mode(curr_scr_win, DEFAULT_USERMODE, 0);

	set_beep_on_msg(curr_scr_win, DEFAULT_BEEP_ON_MSG, 0);
	set_string_var(CLIENTINFO_VAR, IRCII_COMMENT);
	set_string_var(FAKE_SPLIT_PATS_VAR, "*fuck* *shit* *suck* *dick* *penis* *cunt* *haha* *fake* *split* *ass* *hehe* *bogus* *yawn* *leet* *blow* *screw* *dumb* *fbi*");
	
	set_string_var(FORMAT_381_VAR, "%K>%n>%W> You are now a %GIRC%n whore");
	set_string_var(FORMAT_391_VAR, "$G [$1] Channel is full");
	set_string_var(FORMAT_443_VAR, "$G [$1] Channel is full");

	set_string_var(FORMAT_471_VAR, "$G [$1] Channel is full");
	set_string_var(FORMAT_473_VAR, "$G [$1] invite only channel");
	set_string_var(FORMAT_474_VAR, "$G [$1] banned from channel");
	set_string_var(FORMAT_475_VAR, "$G [$1] bad channel key");
	set_string_var(FORMAT_476_VAR, "$G [$1] you are not opped");

	set_string_var(FORMAT_ACTION_VAR, "%Kð %W$1 %n$4-");
	set_string_var(FORMAT_ACTION_AR_VAR, "%Kð %Y$1 %n$4-");
	set_string_var(FORMAT_ACTION_OTHER_VAR, "%Kð %n>%c$1 %n$3-");
	set_string_var(FORMAT_ACTION_OTHER_AR_VAR, "%Kð %n>%c$1 %n$3-");
	set_string_var(FORMAT_ACTION_USER_VAR, "%Kð %y>%c$1 %n$3-");
	set_string_var(FORMAT_ACTION_USER_AR_VAR, "%Kð %n>%c$1 %n$3-");
	set_string_var(FORMAT_ALIAS_VAR, "Alias $[20.]0 $1-");
	set_string_var(FORMAT_ASSIGN_VAR, "Assign $[20.]0 $1-");
	set_string_var(FORMAT_AWAY_VAR, " ($3-) $1 $2");	
	set_string_var(FORMAT_BACK_VAR, "is back from the dead. Gone $1 min $2 secs");
	set_string_var(FORMAT_BANS_HEADER_VAR,"#  Channel    SetBy        Sec  Ban");
	set_string_var(FORMAT_BANS_VAR, "$[2]0 $[10]1 $[10]3 $[-5]numdiff($time() $4)  $2");
	set_string_var(FORMAT_BITCH_VAR, "%RBitch Mode Activated%n $1 $4 $5 on $3");
	set_string_var(FORMAT_BOT_HEADER_VAR, "Aop Prot Bot         Channel    Hostname");
	set_string_var(FORMAT_BOT_FOOTER_VAR, "There are $1 on the BotList");
	set_string_var(FORMAT_BOT_VAR, "$[2]0 $[2]1 $[11]2 $[10]3 $4");

	set_string_var(FORMAT_BWALL_VAR, "[%GBX-Wall%n/%W$1:$2%n] $4-");

	set_string_var(FORMAT_CHANNEL_SIGNOFF_VAR, "$G %nSignOff %W$1%n: $3 %K(%n$4-%K)");
	set_string_var(FORMAT_CONNECT_VAR, "$G Connecting to server $1/%c$2%n");


	set_string_var(FORMAT_CTCP_VAR, "%K>%n>%W> %G$1 %K[%g$2%K]%g requested $4- from $3");
	set_string_var(FORMAT_CTCP_CLOAK_VAR, "%K>%n>%W> %C$1 %K[%c$2%K]%c requested $4- from $3");
	set_string_var(FORMAT_CTCP_CLOAK_FUNC_VAR, "%K>%n>%W> %C$1 %K[%c$2%K]%c requested $4- from $3");
	set_string_var(FORMAT_CTCP_CLOAK_FUNC_USER_VAR, "%K>%n>%W> %C$1 %K[%c$2%K]%c requested $4- from you");
	set_string_var(FORMAT_CTCP_CLOAK_UNKNOWN_VAR, "%K>%n>%W> %C$1 %K[%c$2%K]%c requested unknown ctcp $4- from $3");
	set_string_var(FORMAT_CTCP_CLOAK_UNKNOWN_USER_VAR, "%K>%n>%W> %C$1 %K[%c$2%K]%c requested unknown ctcp $4- from $3");
	set_string_var(FORMAT_CTCP_CLOAK_USER_VAR, "%K>%n>%W> %C$1 %K[%c$2%K]%c requested $4- from you");
	set_string_var(FORMAT_CTCP_FUNC_VAR, "%K>%n>%W> %G$1 %K[%g$2%K]%g requested $4- from $3");
	set_string_var(FORMAT_CTCP_FUNC_USER_VAR, "%K>%n>%W> %G$1 %K[%g$2%K]%g requested $4- from you");
	set_string_var(FORMAT_CTCP_UNKNOWN_VAR, "%K>%n>%W> %G$1 %K[%g$2%K]%g requested unknown ctcp $4- from $3");
	set_string_var(FORMAT_CTCP_UNKNOWN_USER_VAR, "%K>%n>%W> %G$1 %K[%g$2%K]%g requested unknown ctcp $4- from %g$3");
	set_string_var(FORMAT_CTCP_USER_VAR, "%K>%n>%W> %G$1 %K[%g$2%K]%g requested $4- from you");
	set_string_var(FORMAT_CTCP_REPLY_VAR, "$G %nCTCP %W$3 %nreply from %n$1: $4-");


	set_string_var(FORMAT_DCC_CHAT_VAR, "%K[%G$1%K(%gdcc%K)] %n$3-");
	set_string_var(FORMAT_DCC_CONNECT_VAR, "$G DCC $1 %nconnection with %W$2%K[%c$4, port $5%K]%n established");
	set_string_var(FORMAT_DCC_ERROR_VAR, "$G %rDCC lost %w$1%w %rto $2 %K[%w$3-%K]");
	set_string_var(FORMAT_DCC_LOST_VAR, "$G %nDCC %W$1%n:%g$2%n %K[%C$3%K]%n $4 $5 completed in $6 secs %K(%W$7 $8/sec%K)");
	set_string_var(FORMAT_DCC_REQUEST_VAR, "$G DCC $1 %K(%n$2%K)%n request from %W$3%K[%c$4 [$5:$6]%K]%n $8 $7");
	set_string_var(FORMAT_DCCSTATUS_VAR, "[$0-]");
	set_string_var(FORMAT_DESYNC_VAR, "$G $1 is desynced from $2 at $0");
	set_string_var(FORMAT_DISCONNECT_VAR, "$G Use %G/Server%n to connect to a server");
	set_string_var(FORMAT_ENCRYPTED_NOTICE_VAR, "%K-%Y$1%K(%p$2%K)-%n $3-");
	set_string_var(FORMAT_ENCRYPTED_PRIVMSG_VAR, "%K[%Y$1%K(%p$2%K)]%n $3-");
	set_string_var(FORMAT_FLOOD_VAR, "%Y$1%n flood detected from %G$2%K(%g$3%K)%n on %K[%G$4%K]");
	set_string_var(FORMAT_FRIEND_JOIN_VAR, "$G %R$1 %K[%c$2%K]%n has joined $3");
	set_string_var(FORMAT_HOOK_VAR, "$0-");
	set_string_var(FORMAT_INVITE_VAR, "%K>%n>%W> $1 Invites You to $2-");
	set_string_var(FORMAT_INVITE_USER_VAR, "%K>%n>%W> Inviting $1 to $2-");
	set_string_var(FORMAT_JOIN_VAR, "$G %C$1 %K[%c$2%K]%n has joined $3");
	set_string_var(FORMAT_KICK_VAR, "$G %n$3 was kicked off $2 by %c$1 %K(%n$4-%K)");
	set_string_var(FORMAT_KICK_USER_VAR, "%K>%n>%W> %WYou%n have been kicked off %c$2%n by %c$1 %K(%n$4-%K)");
	set_string_var(FORMAT_KILL_VAR, "%K>%n>%W> %RYou have been killed by $1 for $2-");
	set_string_var(FORMAT_LEAVE_VAR, "$G $1 %K[%w$2%K]%n has left $3");
	set_string_var(FORMAT_LINKS_VAR, "%K³%n$[24]0%K³ ³%n$[24]1%K³ ³%n$[3]2%K³ ³%n$[13]3%K³");
	set_string_var(FORMAT_LIST_VAR, "$[12]1 $[-5]2   $[40]3-");
	set_string_var(FORMAT_MAIL_VAR, "%K>%n>%W> You have new Mail");
	set_string_var(FORMAT_MODE_VAR, "$G %nmode%K/%c$3 %K[%W$4-%K]%n by %W$1");
	set_string_var(FORMAT_SMODE_VAR, "$G %RServerMode%K/%c$3 %K[%W$4-%K]%n by %W$1");
	set_string_var(FORMAT_MODE_CHANNEL_VAR, "$G %nmode%K/%c$3 %K[%W$4-%K]%n by %W$1");

	set_string_var(FORMAT_MSG_VAR, "%K[%P$1%K(%p$2%K)]%n $3-");

	set_string_var(FORMAT_OPER_VAR, "%C$1 %K[%c$2%K]%n is now %Wan%w %GIRC%n whore");

	set_string_var(FORMAT_IGNORE_INVITE_VAR, "%K>%n>%W> You have been invited to $1-");
	set_string_var(FORMAT_IGNORE_MSG_VAR, "%K[%P$1%P$2%K(%p$3%K)]%n $4-");
	set_string_var(FORMAT_IGNORE_MSG_AWAY_VAR, "%K[%P$1%P$2%K(%p$3%K)]%n $4-");
	set_string_var(FORMAT_IGNORE_NOTICE_VAR, "%K-%P$2%K(%p$3%K)-%n $4-");
	set_string_var(FORMAT_IGNORE_WALL_VAR, "%K%P$1%n $2-");
	set_string_var(FORMAT_MSG_GROUP_VAR, "%K-%P$1%K:%p$2%K-%n $3-");
	set_string_var(FORMAT_NAMES_VAR, "$G %K[%GUsers%K(%g$1%K:%g$2%K)]%c $3");
	set_string_var(FORMAT_NAMES_NICKCOLOR_VAR, "%K[%B $[10]1%K]");
	set_string_var(FORMAT_NAMES_NONOP_VAR, "$G %K[%GNonChanOps%K(%g$1%K:%g$2%K)]%c $3");
	set_string_var(FORMAT_NAMES_VOICECOLOR_VAR, "%K[%Mv%B$[10]1%K]");
	set_string_var(FORMAT_NAMES_OP_VAR, "$G %K[%GChanOps%K(%g$1%K:%g$2%K)]%c $3");
	set_string_var(FORMAT_NAMES_VOICE_VAR, "$G %K[%MVoiceUsers%K(%m$1%K:%m$2%K)]%c $3");
	set_string_var(FORMAT_NAMES_OPCOLOR_VAR, "%K[%C$0%n%B$[10]1%K]");
	set_string_var(FORMAT_NETADD_VAR, "$G %nAdded: %W$1 $2");
	set_string_var(FORMAT_NETJOIN_VAR, "$G %nNetjoined: %W$1 $2");
	set_string_var(FORMAT_NETSPLIT_VAR, "$G %nNetSplit detected: %W$1%n split from %W$2 %K[%c$0%K]");
	set_string_var(FORMAT_NICKNAME_VAR, "$G %W$1 %nis now known as %c$3");
	set_string_var(FORMAT_NICKNAME_OTHER_VAR, "$G %W$1 %nis now known as %c$4");
	set_string_var(FORMAT_NICKNAME_USER_VAR, "%K>%n>%W> %WYou%K(%n$1%K)%n are now known as %c$3");
	set_string_var(FORMAT_NONICK_VAR, "%W$1%K:%n $3-");


	set_string_var(FORMAT_NOTE_VAR, "($0) ($1) ($2) ($3) ($4) ($5-)");


	set_string_var(FORMAT_NOTICE_VAR, "%K-%P$1%K(%p$2%K)-%n $3-");

	set_string_var(FORMAT_REL_VAR,  "%K[%rmsg->$1%K]%n $4-");
	set_string_var(FORMAT_RELN_VAR, "%K-%P$1%K(%p$2%K)-%n $4-");
	set_string_var(FORMAT_RELM_VAR, "%K[%P%P$1%K(%p$2%K)]%n $4-");

	set_string_var(FORMAT_RELSN_VAR, "%K[%rnotice%K(%R$1%K)] %n$2-");
	set_string_var(FORMAT_RELS_VAR, "$1-");
	set_string_var(FORMAT_RELSM_VAR, "%K[%rmsg%K(%R$1%K)] %n$2-");

	set_string_var(FORMAT_NOTIFY_SIGNOFF_VAR, "$G %GSignoff%n by %r$[10]1%n at $0");
	set_string_var(FORMAT_NOTIFY_SIGNOFF_UH_VAR, "$G %GSignoff%n by %r$1%K!%r$2%K@%r$3%n at $0");
	set_string_var(FORMAT_NOTIFY_SIGNON_UH_VAR, "$G %GSignon%n by %R$1%K!%R$2%K@%R$3%n at $0");
	set_string_var(FORMAT_NOTIFY_SIGNON_VAR, "$G %GSignon%n by %r$[-10]1%n at $0");
	set_string_var(FORMAT_PUBLIC_VAR, "%b<%n$1%b>%n $3-");
	set_string_var(FORMAT_PUBLIC_AR_VAR, "%b<%Y$1%b>%n $3-");
	set_string_var(FORMAT_PUBLIC_MSG_VAR, "%b(%n$1%K/%n$3%b)%n $4-");
	set_string_var(FORMAT_PUBLIC_MSG_AR_VAR, "%b(%Y$1%K/%Y$3%b)%n $4-");
	set_string_var(FORMAT_PUBLIC_NOTICE_VAR, "%K-%P$1%K:%p$3%K-%n $4-");
	set_string_var(FORMAT_PUBLIC_NOTICE_AR_VAR, "%K-%G$1%K:%g$3%K-%n $4-");
	set_string_var(FORMAT_PUBLIC_OTHER_VAR, "%b<%n$1%K:%n$2%b>%n $3-");
	set_string_var(FORMAT_PUBLIC_OTHER_AR_VAR, "%b<%Y$1%K:%n$2%b>%n $3-");
	set_string_var(FORMAT_SEND_ACTION_VAR, "%Kð %W$1 %n$3-");
	set_string_var(FORMAT_SEND_ACTION_OTHER_VAR, "%Kð %n-> %W$1%n/%c$2 %n$3-");
	set_string_var(FORMAT_SEND_AWAY_VAR, "[Away ($strftime($1 %a %b %d %I:%M%p))] [Current ($strftime($0 %a %b %d %I:%M%p))] [BX-MsgLog $2]");
	set_string_var(FORMAT_SEND_CTCP_VAR, "%K[%rctcp%K(%R$1%K)] %n$2");
	set_string_var(FORMAT_SEND_DCC_CHAT_VAR, "%K[%rdcc%K(%R$1%K)] %n$2-");
	set_string_var(FORMAT_SEND_MSG_VAR, "%K[%rmsg%K(%R$1%K)] %n$3-");
	set_string_var(FORMAT_SEND_NOTICE_VAR, "%K[%rnotice%K(%R$1%K)] %n$3-");
	set_string_var(FORMAT_SEND_PUBLIC_VAR, "%p<%n$2%p>%n $3-");
	set_string_var(FORMAT_SEND_PUBLIC_OTHER_VAR, "%p<%n$2%K:%n$1%p>%n $3-");
	set_string_var(FORMAT_SERVER_VAR, "$G%n $1: $2-");
	set_string_var(FORMAT_SERVER_MSG1_VAR, "$G%n $1: $2-");
	set_string_var(FORMAT_SERVER_MSG1_FROM_VAR, "$G%n $1: $2-");
	set_string_var(FORMAT_SERVER_MSG2_VAR, "$G%n $1-");
	set_string_var(FORMAT_SERVER_MSG2_FROM_VAR, "$G%n $1-");

	set_string_var(FORMAT_SERVER_NOTICE_VAR, "%G!%g$1%G%n $2-");
	set_string_var(FORMAT_SERVER_NOTICE_BOT_VAR, "Possible bot: %C$1 %K[%c$2-%K]%n");
	set_string_var(FORMAT_SERVER_NOTICE_BOT1_VAR, "Possible $1 bot: %C$2 %K[%c$3-%K]%n");
	set_string_var(FORMAT_SERVER_NOTICE_BOT_ALARM_VAR, "$1 alarm activated: %C$2 %K[%c$3-%K]%n");
	set_string_var(FORMAT_SERVER_NOTICE_CLIENT_CONNECT_VAR, "Client Connecting: %C$1 %K[%c$2-%K]%n");
	set_string_var(FORMAT_SERVER_NOTICE_CLIENT_EXIT_VAR, "Client Exiting: %C$1 %K[%c$2-%K]%n");
	set_string_var(FORMAT_SERVER_NOTICE_CLIENT_INVALID_VAR, "Invalid username: %C$1 %K[%c$2-%K]%n");
	set_string_var(FORMAT_SERVER_NOTICE_CLIENT_TERM_VAR, "Terminating client for excess flood %C$1%K [%c$2-%K]%n");
	set_string_var(FORMAT_SERVER_NOTICE_FAKE_VAR, "Fake Mode detected on $1 -> $2-");
	set_string_var(FORMAT_SERVER_NOTICE_KILL_VAR, "Foreign OperKill: %W$1%n killed %c$2%n %K(%n$3-%K)%n");
	set_string_var(FORMAT_SERVER_NOTICE_KILL_LOCAL_VAR, "Local OperKill: %W$1%n killed %c$2%n %K(%n$3-%K)%n");
	set_string_var(FORMAT_SERVER_NOTICE_KLINE_VAR, "%W$1%n added a new K-Line %K[%c$2%K]%n for $3-");
	set_string_var(FORMAT_SERVER_NOTICE_NICK_COLLISION_VAR, "Nick collision: %W$1%n killed %c$2%n %K(%n$3-%K)%n");
	set_string_var(FORMAT_SERVER_NOTICE_OPER_VAR, "%C$1 %K[%c$2%K]%n is now %Wa%w %GIRC%n whore");
	set_string_var(FORMAT_SERVER_NOTICE_REHASH_VAR, "%W$1%n is rehashing the Server config file");
	set_string_var(FORMAT_SERVER_NOTICE_STATS_VAR, "Stats $1: %C$2 %K[%c$3-%K]%n");
	set_string_var(FORMAT_SERVER_NOTICE_TRAFFIC_HIGH_VAR, "Entering high-traffic mode %K(%n$1 > $2-%K)%n");
	set_string_var(FORMAT_SERVER_NOTICE_TRAFFIC_NORM_VAR, "Resuming standard operation %K(%n$1 $2 $3-%K)%n");
	set_string_var(FORMAT_SERVER_NOTICE_UNAUTH_VAR, "Unauthorized Connection from $1-");

	set_string_var(FORMAT_SET_VAR, "%g$[-30.]0 %w$1-");
	set_string_var(FORMAT_CSET_VAR, "%r$[-14]1 %R$[-20.]0 %w$[-5]2-");
	set_string_var(FORMAT_SET_NOVALUE_VAR, "%g$[-30.]0 has no value");
	set_string_var(FORMAT_SHITLIST_VAR," $[3]0 $[34]1 $[10]2  $3-");
	set_string_var(FORMAT_SHITLIST_FOOTER_VAR, "There are $1 users on the shitlist");
	set_string_var(FORMAT_SHITLIST_HEADER_VAR, " lvl nick!user@host                     channels   reason");
	set_string_var(FORMAT_SIGNOFF_VAR, "$G %nSignOff: %W$1 %K(%n$3-%K)");


	set_string_var(FORMAT_SILENCE_VAR, "$G %RWe are $1 silencing $2 at $0");


	set_string_var(FORMAT_TIMER_VAR, "$G $[-5]0 $[-10]1 $2-");
	set_string_var(FORMAT_TOPIC_VAR, "$G Topic for %c$1%K:%n $2-");
	set_string_var(FORMAT_TOPIC_CHANGE_VAR, "$G %W$1 %nhas changed the topic on channel $2 to%K:%n $3-");
	set_string_var(FORMAT_TOPIC_SETBY_VAR, "$G %ntopic set by %c$2%K [%c$stime($3)%K]");
	set_string_var(FORMAT_TOPIC_UNSET_VAR, "$G %ntopic unset by $1 on $2");
	
	set_string_var(FORMAT_USAGE_VAR, "$G Usage: /$0  $1-");
	set_string_var(FORMAT_USERMODE_VAR, "$G %nMode change %K[%W$4-%K]%n for user %c$3");
	set_string_var(FORMAT_USERLIST_VAR,"$[-3]0   $[-3]1  $[-3]2  $[-10]3 $[10]4   $[-25]5 $[10]6");

	set_string_var(FORMAT_USERLIST_FOOTER_VAR, "There are $1 users on the userlist");
	set_string_var(FORMAT_USERLIST_HEADER_VAR, "level aop prot  nick       password     host                      channels");
	set_string_var(FORMAT_USERS_VAR, "%K[%n$[10]3%K] %K[%n%C$6%B$[10]4%K] %K[%n$[37]5%K] %K[%n$[-3]0%b:%n$1%b:%n$2%K]");
	set_string_var(FORMAT_USERS_USER_VAR, "%K[%n$[10]3%K] %K[%n%C$6%B$[10]4%K] %K[%n%B$[37]5%K] %K[%n$[-3]0%b:%n$1%b:%n$2%K]");
	set_string_var(FORMAT_USERS_BOT_VAR, "%K[%n$[10]3%K] %K[%n%C$6%B$[10]4%K] %K[%n%g$[37]5%K] %K[%n$[-3]0%b:%n$1%b:%n$2%K]");
	set_string_var(FORMAT_USERS_SHIT_VAR, "%K[%n$[10]3%K] %K[%n%C$6%B$[10]4%K] %K[%n%r$[37]5%K] %K[%n$[-3]0%b:%n$1%b:%n$2%K]");
	set_string_var(FORMAT_USERS_HEADER_VAR, "%K[ %WC%nhannel  %K] [ %WN%wickname  %K] [%n %Wu%wser@host                           %K] [%n %Wl%wevel %K]");
	set_string_var(FORMAT_VERSION_VAR, "\002$0\002 by panasync \002-\002 $2 $3");


	set_string_var(FORMAT_WALL_VAR, "%G!%g$1:$2%G!%n $3-");
	set_string_var(FORMAT_WALL_AR_VAR, "%G!%g$1:$2%G!%n $3-");


	set_string_var(FORMAT_WALLOP_VAR, "%G!%g$1$2%G!%n $3-");
	set_string_var(FORMAT_WHO_VAR, "%Y$[10]0 %W$[10]1%w %c$[3]2 %w$3%R@%w$4 ($6-)");
	set_string_var(FORMAT_WHOIS_AWAY_VAR, "%K| %Wa%nway     : $0 - $1-");
	set_string_var(FORMAT_WHOIS_BOT_VAR, "%g| %Wb%not      : A:$0 P:$1 [$2] $3-");
	set_string_var(FORMAT_WHOIS_CHANNELS_VAR, "%g| %Wc%nhannels : $0-");
	set_string_var(FORMAT_WHOIS_FRIEND_VAR, "%g| %Wf%nriend   : L:$0 A:$1 P:$2 $3-");
	set_string_var(FORMAT_WHOIS_HEADER_VAR, "%GÚÄÄÄÄÄ---%gÄ%G--%gÄÄ%G-%gÄÄÄÄÄÄ---%KÄ%g--%KÄÄ%g-%KÄÄÄÄÄÄÄÄÄ--- --  -");
	set_string_var(FORMAT_WHOIS_IDLE_VAR, "%K: %Wi%ndle     : $0 hours $1 mins $2 secs");
	set_string_var(FORMAT_WHOIS_SHIT_VAR, "%g| %Ws%nhit     : L:$0 [$1] $2 $3-");
	set_string_var(FORMAT_WHOIS_SIGNON_VAR, "%K %Ws%nignon   : $0-");
	set_string_var(FORMAT_WHOIS_NAME_VAR, "%G³ %Wi%nrcname  : $0-");
	set_string_var(FORMAT_WHOIS_NICK_VAR, "%G| %W$0 %K(%n$1@$2%K)");
	set_string_var(FORMAT_WHOIS_OPER_VAR, "%K| %Wo%nperator : $0 $1-");
	set_string_var(FORMAT_WHOIS_SERVER_VAR, "%K³ %Ws%nerver   : $0 ($1-)");
	set_string_var(FORMAT_WHOLEFT_HEADER_VAR, "%GÚÄÄÄÄÄ %WWho %GÄÄÄÄÄ%g---%GÄÄÄ%g--%GÄÄÄÄÄ%gÄ-%GÄÄ %WChannel%gÄÄÄ %wServer %G-%gÄÄ--%GÄÄ%g--%GÄÄÄÄ%g %wSeconds");
	set_string_var(FORMAT_WHOLEFT_USER_VAR, "%G|%n $[-10]0!$[20]1 $[10]2 $[20]4 $3");
	set_string_var(FORMAT_WHOWAS_HEADER_VAR, "%GÚÄÄÄÄÄ---%gÄ%G--%gÄÄ%G-%gÄÄÄÄÄÄ---%KÄ%g--%KÄÄ%g-%KÄÄÄÄÄÄÄÄÄ--- --  -");
	set_string_var(FORMAT_WHOWAS_NICK_VAR, "%G| %W$0%n was %K(%n$1@$2%K)");
	set_string_var(FORMAT_WIDELIST_VAR, "$1-");
	set_string_var(FORMAT_WINDOW_SET_VAR, "$0-");
	
	set_string_var(FORMAT_NICK_MSG_VAR, "$0 $1 $2-");
	set_string_var(FORMAT_NICK_COMP_VAR, "$0\002:\002 $1-");
	set_string_var(FORMAT_NICK_AUTO_VAR, "$0\002:\002 $1-");
	set_string_var(FORMAT_STATUS_VAR, "%4%W$0-");
	set_string_var(FORMAT_STATUS1_VAR, "%4%W$0-");
	set_string_var(FORMAT_STATUS2_VAR, "%4%W$0-");
	set_string_var(FORMAT_STATUS3_VAR, "%4%W$0-");
	set_string_var(FORMAT_NOTIFY_OFF_VAR, "$0-");
	set_string_var(FORMAT_NOTIFY_ON_VAR, "%G$0!$1@$2");
	set_string_var(CDCC_PROMPT_VAR, "%GC%gDCC");
	set_string_var(SERVER_PROMPT_VAR, "[1;32m[Ser[0;32mver][0m");
	set_string_var(FTP_VAR, "/usr/bin/ncftp");
	set_string_var(BOT_LOGFILE_VAR, "tcl.log");

	set_string_var(HELP_PATH_VAR, DEFAULT_HELP_PATH);

#ifdef WANT_CD
	set_cd_device(curr_scr_win, "/dev/cdrom", 0);
#endif
	set_string_var(OPER_MODES_VAR, DEFAULT_OPERMODE);	

	set_lastlog_size(curr_scr_win, NULL, irc_variable[LASTLOG_VAR].integer);
	set_history_size(curr_scr_win, NULL, irc_variable[HISTORY_VAR].integer);

	set_highlight_char(curr_scr_win, irc_variable[HIGHLIGHT_CHAR_VAR].string, 0);
	set_lastlog_level(curr_scr_win, irc_variable[LASTLOG_LEVEL_VAR].string, 0);
	set_notify_level(curr_scr_win, irc_variable[NOTIFY_LEVEL_VAR].string, 0);
	set_msglog_level(curr_scr_win, irc_variable[MSGLOG_LEVEL_VAR].string, 0);

	set_input_prompt(curr_scr_win, DEFAULT_INPUT_PROMPT, 0);

	reinit_chan_int(curr_scr_win, NULL, 1);
	init_window_variables(curr_scr_win);
	window_display = old_display;
}

/*
 * find_variable: looks up variable name in the variable table and returns
 * the index into the variable array of the match.  If there is no match, cnt
 * is set to 0 and -1 is returned.  If more than one match the string, cnt is
 * set to that number, and it returns the first match.  Index will contain
 * the index into the array of the first found entry 
 */
enum VAR_TYPES find_variable(org_name, cnt)
	char	*org_name;
	int	*cnt;
{
	IrcVariable *v,
		    *first;
	int	len;
enum VAR_TYPES		var_index;
	char	*name = NULL;

	malloc_strcpy(&name,org_name);
	upper(name);
	len = strlen(name);
	var_index = 0;
	for (first = irc_variable; first->name; first++, var_index++)
	{
		if (strncmp(name, first->name, len)==0)
		{
			*cnt = 1;
			break;
		}
	}
	if (first->name)
	{
		if (strlen(first->name) != len)
		{
			v = first;
			for (v++; v->name; v++, (*cnt)++)
			{
				if (strncmp(name, v->name, len) != 0)
					break;
			}
		}
		new_free(&name);
		return (var_index);
	}
	else
	{
		*cnt = 0;
		new_free(&name);
		return (-1);
	}
}

enum CVAR_TYPES cfind_variable(char *org_name, int *cnt)
{
	IrcVariable *v,
		    *first;
	int	len;
enum CVAR_TYPES		var_index;
	char	*name = NULL;

	malloc_strcpy(&name,org_name);
	upper(name);
	len = strlen(name);
	var_index = 0;
	for (first = chan_variable; first->name; first++, var_index++)
	{
		if (strncmp(name, first->name, len) == 0)
		{
			*cnt = 1;
			break;
		}
	}
	if (first->name)
	{
		if (strlen(first->name) != len)
		{
			v = first;
			for (v++; v->name; v++, (*cnt)++)
			{
				if (strncmp(name, v->name, len) != 0)
					break;
			}
		}
		new_free(&name);
		return (var_index);
	}
	else
	{
		*cnt = 0;
		new_free(&name);
		return (-1);
	}
}

/*
 * do_boolean: just a handy thing.  Returns 1 if the str is not ON, OFF, or
 * TOGGLE 
 */
int do_boolean(str, value)
	char	*str;
	int	*value;
{
	upper(str);
	if (strcmp(str, var_settings[ON]) == 0)
		*value = 1;
	else if (strcmp(str, var_settings[OFF]) == 0)
		*value = 0;
	else if (strcmp(str, "TOGGLE") == 0)
	{
		if (*value)
			*value = 0;
		else
			*value = 1;
	}
	else
		return (1);
	return (0);
}

/*
 * set_var_value: Given the variable structure and the string representation
 * of the value, this sets the value in the most verbose and error checking
 * of manors.  It displays the results of the set and executes the function
 * defined in the var structure 
 */

#ifdef WANT_DLL
void set_var_value(int var_index, char *value, IrcVariableDll *dll)
#else
void set_var_value(int var_index, char *value)
#endif
{
	char	*rest;
	IrcVariable *var;
	int	old;
#ifdef WANT_DLL
	if (dll)
	{
		var = (IrcVariable *) new_malloc(sizeof(IrcVariable));
		var->type = dll->type;
		var->string = dll->string;
		var->integer = dll->integer;
		var->int_flags = dll->int_flags;
		var->flags = dll->flags;
		var->name = dll->name;
		var->func = dll->func;
	}
	else
#endif
		var = &(irc_variable[var_index]);
	switch (var->type)
	{
	case BOOL_TYPE_VAR:
		if (value && *value && (value = next_arg(value, &rest)))
		{
			old = var->integer;
			if (do_boolean(value, &(var->integer)))
			{
				say("Value must be either ON, OFF, or TOGGLE");
				break;
			}
			if (!(var->int_flags & VIF_CHANGED))
			{
				if (old != var->integer)
					var->int_flags |= VIF_CHANGED;
			}
			if (loading_global)
				var->int_flags |= VIF_GLOBAL;
			if (var->func)
				(var->func) (curr_scr_win, NULL, var->integer);
			say("Value of %s set to %s", var->name,
				var->integer ? var_settings[ON]
					     : var_settings[OFF]);
		}
		else
			put_it("%s", convert_output_format(get_string_var(FORMAT_SET_VAR), "%s %s", var->name, var->integer?var_settings[ON] : var_settings[OFF]));
		break;
	case CHAR_TYPE_VAR:
		if (!value)
		{
			if (!(var->int_flags & VIF_CHANGED))
			{
				if (var->integer)
					var->int_flags |= VIF_CHANGED;
			}
			if (loading_global)
				var->int_flags |= VIF_GLOBAL;
			var->integer = ' ';
			if (var->func)
				(var->func) (curr_scr_win, NULL, var->integer);
			say("Value of %s set to '%c'", var->name, var->integer);
		}

		else if (value && *value && (value = next_arg(value, &rest)))
		{
			if (strlen(value) > 1)
				say("Value of %s must be a single character",
					var->name);
			else
			{
				if (!(var->int_flags & VIF_CHANGED))
				{
					if (var->integer != *value)
						var->int_flags |= VIF_CHANGED;
				}
				if (loading_global)
					var->int_flags |= VIF_GLOBAL;
				var->integer = *value;
				if (var->func)
					(var->func) (curr_scr_win, NULL, var->integer);
				say("Value of %s set to '%c'", var->name,
					var->integer);
			}
		}
		else
			put_it("%s", convert_output_format(get_string_var(FORMAT_SET_VAR), "%s %c", var->name, var->integer));
		break;
	case INT_TYPE_VAR:
		if (value && *value && (value = next_arg(value, &rest)))
		{
			int	val;

			if (!is_number(value))
			{
				say("Value of %s must be numeric!", var->name);
				break;
			}
			if ((val = my_atol(value)) < 0)
			{
				say("Value of %s must be greater than 0", var->name);
				break;
			}
			if (!(var->int_flags & VIF_CHANGED))
			{
				if (var->integer != val)
					var->int_flags |= VIF_CHANGED;
			}
			if (loading_global)
				var->int_flags |= VIF_GLOBAL;
			var->integer = val;
			if (var->func)
				(var->func) (curr_scr_win, NULL, var->integer);
			say("Value of %s set to %d", var->name, var->integer);
		}
		else
			put_it("%s", convert_output_format(get_string_var(FORMAT_SET_VAR), "%s %d", var->name, var->integer));
		break;
	case STR_TYPE_VAR:
		if (value)
		{
			if (*value)
			{
				char	*temp = NULL;

				if (var->flags & VF_EXPAND_PATH)
				{
					temp = expand_twiddle(value);
					if (temp)
						value = temp;
					else
						say("SET: no such user");
				}
				if ((!var->int_flags & VIF_CHANGED))
				{
					if ((var->string && ! value) ||
					    (! var->string && value) ||
					    my_stricmp(var->string, value))
						var->int_flags |= VIF_CHANGED;
				}
				if (loading_global)
					var->int_flags |= VIF_GLOBAL;
				malloc_strcpy(&(var->string), value);
				if (temp)
					new_free(&temp);
			}
			else
			{
				put_it("%s", convert_output_format(get_string_var(var->string?FORMAT_SET_VAR:FORMAT_SET_NOVALUE_VAR), "%s %s", var->name, var->string));
				return;
			}
		}
		else
			new_free(&(var->string));
		if (var->func)
			(var->func) (curr_scr_win, var->string, 0);
		say("Value of %s set to %s", var->name, var->string ?
			var->string : "<EMPTY>");
		break;
	}
#ifdef WANT_DLL
	if (dll)
	{
		dll->integer = var->integer;
		new_free((char **)&var);
	}
#endif

}

static void do_dop_reset(ChannelList *tmp)
{
NickList *nick;
	if (tmp)
	{
		for (nick = tmp->nicks; nick; nick = nick->next)
		{
			nick->dopcount = 0;
			nick->doptime = 0;
		}
	}
}
 
static void do_nick_reset(ChannelList *tmp)
{
NickList *nick;
	if (tmp)
	{
		for (nick = tmp->nicks; nick; nick = nick->next)
		{
			nick->nickcount = 0;
			nick->nicktime = 0;
		}
	}
}
 
static void do_kick_reset(ChannelList *tmp)
{
NickList *nick;
	if (tmp)
	{
		for (nick = tmp->nicks; nick; nick = nick->next)
		{
			nick->kickcount = 0;
			nick->kicktime = 0;
		}
	}
}
 
static void do_pubflood_reset(ChannelList *tmp)
{
NickList *nick;
	if (tmp)
	{
		for (nick = tmp->nicks; nick; nick = nick->next)
		{
			nick->floodcount = 0;
			nick->floodtime = 0;
		}
	}
}
 
static void do_join_reset(ChannelList *tmp)
{
NickList *nick;
	if (tmp)
	{
		for (nick = tmp->nicks; nick; nick = nick->next)
		{
			nick->joincount = 0;
			nick->jointime = 0;
		}
	}
}
 
int *set_channel_variable(ChannelList *channel, enum CVAR_TYPES var_index)
{
int *tmp = NULL;
enum RESET_TYPES {dop_reset = 0, kick_reset, nick_reset, pubflood_reset, join_reset, invalid_reset};
enum RESET_TYPES reset;
	reset = invalid_reset;
	switch(var_index)
	{
		case AOP_CVAR:
			tmp = &channel->set_aop;
			break;
		case AINV_CVAR:
			tmp = &channel->set_ainv;
			break;
		case AUTO_REJOIN_CVAR:
			tmp = &channel->set_auto_rejoin;
			break;
		case BITCH_CVAR:
			tmp = &channel->bitch_mode;
			break;
		case DEOPFLOOD_CVAR:
			tmp = &channel->set_deopflood;
			reset = dop_reset;
			break;
		case DEOPFLOOD_TIME_CVAR:
			tmp = &channel->set_deopflood_time;
			reset = dop_reset;
			break;
		case DEOP_ON_DEOPFLOOD_CVAR:
			tmp = &channel->set_deop_on_deopflood;
			reset = dop_reset;
			break;
		case DEOP_ON_KICKFLOOD_CVAR:
			tmp = &channel->set_deop_on_kickflood;
			reset = kick_reset;
			break;
		case HACKING_CVAR:
			tmp = &channel->set_hacking;
			break;
		case JOINFLOOD_CVAR:
			tmp = &channel->set_joinflood;
			break;
		case JOINFLOOD_TIME_CVAR:
			tmp = &channel->set_joinflood_time;
			break;
		case KICK_IF_BANNED_CVAR:
			tmp = &channel->set_kick_if_banned;
			break;
		case KICK_ON_DEOPFLOOD_CVAR:
			tmp = &channel->set_kick_on_deopflood;
			reset = dop_reset;
			break;
		case KICK_ON_JOINFLOOD_CVAR:
			tmp = &channel->set_kick_on_joinflood;
			reset = join_reset;
			break;
		case KICK_ON_KICKFLOOD_CVAR:
			tmp = &channel->set_kick_on_kickflood;
			reset = kick_reset;
			break;
		case KICK_ON_NICKFLOOD_CVAR:
			tmp = &channel->set_kick_on_nickflood;
			reset = nick_reset;
			break;
		case KICK_ON_PUBFLOOD_CVAR:
			tmp = &channel->set_kick_on_pubflood;
			reset = pubflood_reset;
			break;
		case KICKFLOOD_CVAR:
			tmp = &channel->set_kickflood;
			reset = kick_reset;
			break;
		case KICKFLOOD_TIME_CVAR:
			tmp = &channel->set_kickflood_time;
			reset = kick_reset;
			break;
		case LAMELIST_CVAR:
			tmp = &channel->set_lamelist;
			break;
		case NICKFLOOD_CVAR:
			tmp = &channel->set_nickflood;
			reset = nick_reset;
			break;
		case NICKFLOOD_TIME_CVAR:
			tmp = &channel->set_nickflood_time;
			reset = nick_reset;
			break;
		case PUBFLOOD_CVAR:
			tmp = &channel->set_pubflood;
			reset = pubflood_reset;
			break;
		case PUBFLOOD_IGNORE_TIME_CVAR:
			tmp = &channel->set_pubflood_ignore;
			break;
		case PUBFLOOD_TIME_CVAR:
			tmp = &channel->set_pubflood_time;
			reset = pubflood_reset;
			break;
		case SHITLIST_CVAR:
			tmp = &channel->set_shitlist;
			break;
		case USERLIST_CVAR:
			tmp = &channel->set_userlist;
			break;
		default:
			break;
	}
	switch(reset)
	{
		case dop_reset:
			do_dop_reset(channel);
			break;
		case kick_reset:
			do_kick_reset(channel);
			break;
		case pubflood_reset:
			do_pubflood_reset(channel);
			break;
		case nick_reset:
			do_nick_reset(channel);
			break;
		case join_reset:
			do_join_reset(channel);
			break;
		default:
			break;
	}
	return tmp;
}

static void set_cvar_value(int var_index, char *value, ChannelList *channel)
{
	char	*rest;
	IrcVariable *var;
	int	*chan_set = NULL;

	var = &(chan_variable[var_index]);
	chan_set = set_channel_variable(channel, var_index);
	if (!chan_set)
	{
		bitchsay("Error in /cset value");
		return;
	}
	switch (var->type)
	{
	case BOOL_TYPE_VAR:
		if (value && *value && (value = next_arg(value, &rest)))
		{
			if (do_boolean(value, chan_set))
			{
				say("Value must be either ON, OFF, or TOGGLE");
				break;
			}
			if (var->func)
				(var->func) (curr_scr_win, NULL, *chan_set);
			put_it("%s", convert_output_format(get_string_var(FORMAT_CSET_VAR), "%s %s %s", var->name, channel->channel, (*chan_set)?var_settings[ON] : var_settings[OFF]));
		}
		else
			put_it("%s", convert_output_format(get_string_var(FORMAT_CSET_VAR), "%s %s %s", var->name, channel->channel, (*chan_set)?var_settings[ON] : var_settings[OFF]));
		break;
	case INT_TYPE_VAR:
		if (value && *value && (value = next_arg(value, &rest)))
		{
			int	val;

			if (!is_number(value))
			{
				say("Value of %s must be numeric!", var->name);
				break;
			}
			if ((val = my_atol(value)) < 0)
			{
				say("Value of %s must be greater than 0", var->name);
				break;
			}
			*chan_set = val;
			if (var->func)
				(var->func) (curr_scr_win, NULL, *chan_set);
			put_it("%s", convert_output_format(get_string_var(FORMAT_CSET_VAR), "%s %s %d", var->name, channel->channel, *chan_set));
		}
		else
			put_it("%s", convert_output_format(get_string_var(FORMAT_CSET_VAR), "%s %s %d", var->name, channel->channel, *chan_set));
		break;
	}
}

extern AliasStack *set_stack;
void do_stack_set(int type, char *args)
{
	AliasStack *aptr = set_stack;
	AliasStack **aptrptr = &set_stack;
		
	if (!*aptrptr && (type == STACK_POP || type == STACK_LIST))
	{
		say("Set stack is empty!");
		return;
	}

	if (STACK_PUSH == type)
	{
		enum VAR_TYPES var_index;
		int cnt = 0;
		/* Dont need to unstub it, we're not actually using it. */
		var_index = find_variable(args, &cnt);
		switch (cnt)
		{
			default:
			case 1:
				aptr = (AliasStack *)new_malloc(sizeof(AliasStack));
				aptr->next = aptrptr ? *aptrptr : NULL;
				*aptrptr = aptr;
				aptr->set = (IrcVariable *) new_malloc(sizeof(IrcVariable));
				memcpy(aptr->set, &irc_variable[var_index], sizeof(IrcVariable));
				aptr->name = m_strdup(irc_variable[var_index].name);
				aptr->set->string = m_strdup(irc_variable[var_index].string);
				aptr->var_index = var_index;
				break;
			case 0:
				say("No such Set [%s]", args);
				break;
		}
		return;
	}

	if (STACK_POP == type)
	{
		AliasStack *prev = NULL;
		for (aptr = *aptrptr; aptr; prev = aptr, aptr = aptr->next)
		{
			/* have we found it on the stack? */
			if (!my_stricmp(args, aptr->name))
			{
				/* remove it from the list */
				if (prev == NULL)
					*aptrptr = aptr->next;
				else
					prev->next = aptr->next;

				new_free(&(irc_variable[aptr->var_index].string));
				memcpy(&irc_variable[aptr->var_index], aptr->set, sizeof(IrcVariable));
				/* free it */
				new_free((char **)&aptr->name);
				new_free((char **)&aptr);
				return;
			}
		}
		say("%s is not on the %s stack!", args, "Set");
		return;
	}
	if (STACK_LIST == type)
	{
		AliasStack *prev = NULL;
		for (aptr = *aptrptr; aptr; prev = aptr, aptr = aptr->next)
		{
			switch(aptr->set->type)
			{
				case BOOL_TYPE_VAR:
					say("Variable [%s] = %s", aptr->set->name, on_off(aptr->set->integer));
					break;
				case INT_TYPE_VAR:
					say("Variable [%s] = %d", aptr->set->name, aptr->set->integer);
					break;
				case CHAR_TYPE_VAR:
					say("Variable [%s] = %c", aptr->set->name, aptr->set->integer);
					break;
				case STR_TYPE_VAR:
					say("Variable [%s] = %s", aptr->set->name, aptr->set->string?aptr->set->string:"<Empty String>");
					break;
				default:
					bitchsay("Error in do_stack_set: unknown set type");
			}
		}
		return;
	}
	say("Unknown STACK type ??");
}


char **win_find_variable(Window *win, enum VAR_TYPES win_var)
{
int status_line = 0;
	if (!win)
		return NULL;
	switch (win_var)
	{
		case STATUS_AWAY_VAR:
			return &irc_variable[win_var].string;
			break;
		case STATUS_CHANNEL_VAR:
			return  &win->status_channel;
			break;
		case STATUS_CHANOP_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_CLOCK_VAR:
			return  &win->status_clock;
			break;
		case STATUS_FORMAT3_VAR:
			status_line++;
		case STATUS_FORMAT2_VAR:
			status_line++;
		case STATUS_FORMAT1_VAR:
			status_line++;
		case STATUS_FORMAT_VAR:
			return  &win->format_status[status_line];
			break;
		case STATUS_HOLD_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_HOLD_LINES_VAR:
			return  &win->status_hold_lines;
			break;
		case STATUS_INSERT_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_LAG_VAR:
			return  &win->status_lag;
			break;
		case STATUS_MAIL_VAR:
			return  &win->status_mail;
			break;
		case STATUS_MODE_VAR:
			return  &win->status_mode;
			break;
		case STATUS_NOTIFY_VAR:
			return  &win->status_notify;
			break;
		case STATUS_OPER_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_OPER_KILLS_VAR:
			return  &win->status_oper_kills;
			break;
		case STATUS_OVERWRITE_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_QUERY_VAR:
			return  &win->status_query;
			break;
		case STATUS_SERVER_VAR:
			return  &win->status_server;
			break;
		case STATUS_TOPIC_VAR:
			return  &win->status_topic;
			break;
		case STATUS_UMODE_VAR:
			return  &win->status_umode;
			break;
		case STATUS_USER_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USERS_VAR:
			return  &win->status_users;
			break;
		case STATUS_USER1_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER2_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER3_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER4_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER5_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER6_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER7_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER8_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER9_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER10_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER11_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER12_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER13_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER14_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER15_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER16_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER17_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER18_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_USER19_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_VOICE_VAR:
			return  &irc_variable[win_var].string;
			break;
		case STATUS_WINDOW_VAR:
			return  &irc_variable[win_var].string;
			break;
		default:
			break;
	}
	return NULL;
}


#ifdef __STDC__
char	*	wget_string_var(Window *win, enum VAR_TYPES var)
#else
char *	wget_string_var(win, var)
	Window *win;
	enum VAR_TYPES var;
#endif
{
	return *(win_find_variable(win, var));
}

/*
 * wset_string_var: sets the string variable given as an index into the
 * variable table to the given string.  If string is null, the current value
 * of the string variable is freed and set to null 
 */
#ifdef __STDC__
void wset_string_var(Window *win, enum VAR_TYPES var, char *string)
#else
void wset_string_var(win, var, string)
	Window *win;
	enum VAR_TYPES var;
	char *string;
#endif
{
char **what = NULL;
	if (win)
	{
		what = NULL;
		what = win_find_variable(win, var);
		if (what && string)
			malloc_strcpy(what, string);
		else if (!string && what)
			new_free(what);
	}
}

/*
 * wset_var_value: Given the variable structure and the string representation
 * of the value, this sets the value in the most verbose and error checking
 * of manors.  It displays the results of the set and executes the function
 * defined in the var structure 
 */
void wset_var_value(win, lame, var_index, variable, value)
	Window	*win;
	enum	VAR_TYPES lame;
	char	**var_index;
	char	*variable;
	char	*value;
{
	if (value)
	{
		if (*value)
			malloc_strcpy(var_index, value);
		else
		{
			put_it("%s", convert_output_format(get_string_var(FORMAT_WINDOW_SET_VAR), "Window %s %s", variable, var_index?*var_index:"Not Set"));
			return;
		}
	}
	else
		new_free(var_index);
	if (irc_variable[lame].func)
		(irc_variable[lame].func)(win, *var_index, 0);
	say("Value of Window %s set to %s", variable, var_index ? *var_index : "<EMPTY>");
}


void window_set_variable(Window *win, enum VAR_TYPES win_var, char *var, char *args)

{
char **what = NULL;
	what = win_find_variable(win, win_var);
	if (what)
		wset_var_value(win, win_var, what, var, args);
	else
		say("This is not a window variable %s", var);
}

int window_set_var(Window *win, char *args)
{
enum VAR_TYPES var_index;
char	*variable;
int 	cnt = 0;
	if (args && *args && (variable = next_arg(args, &args)))
	{
		if (*variable == '-')
		{
			variable++;
			args = NULL;
		}
		var_index = find_variable(variable, &cnt);
		if (var_index < STATUS_AWAY_VAR || var_index > STATUS_WINDOW_VAR)
			cnt = 0;
		switch(cnt)
		{
			case 0:
				say("No such window variable %s", variable);
				break;
			case 1:
				window_set_variable(win, var_index, variable, args);
				break;
			default:
				say("window set %s is ambigous", variable);
				for (cnt+=var_index; var_index < cnt; var_index++)
					window_set_variable(win, var_index, variable, empty_string);
				break;
		}
		return 1;
	}	
	else
	{
		for (var_index = STATUS_AWAY_VAR; var_index <= STATUS_WINDOW_VAR; var_index++)
			window_set_variable(win, var_index, irc_variable[var_index].name, empty_string);
	}
	return 0;
}

/*
 * set_variable: The SET command sets one of the irc variables.  The args
 * should consist of "variable-name setting", where variable name can be
 * partial, but non-ambbiguous, and setting depends on the variable being set 
 */
void set_variable(char *command, char *args, char *subargs)
{
	char	*var;
	int	cnt;
enum VAR_TYPES	var_index;
#ifdef WANT_DLL
	IrcVariableDll *dll = NULL;
#endif	
	if ((var = next_arg(args, &args)) != NULL)
	{
		if (*var == '-')
		{
			var++;
			args = NULL;
		}
		var_index = find_variable(var, &cnt);
		switch (cnt)
		{
			case 0:
#ifdef WANT_DLL
				if (!(dll = (IrcVariableDll *)find_in_list((List **)&dll_variable, var, 0)))
					say("No such variable \"%s\"", var);
				else
					set_var_value(-1, args, dll);
#else
				say("No such variable \"%s\"", var);
#endif
				return;
			case 1:
#ifdef WANT_DLL
				set_var_value(var_index, args, NULL);
#else
				set_var_value(var_index, args);
#endif
				return;
			default:
				say("%s is ambiguous", var);
				for (cnt += var_index; var_index < cnt; var_index++)
#ifdef WANT_DLL
					set_var_value(var_index, empty_string, NULL);
#else
					set_var_value(var_index, empty_string);
#endif
				return;
		}
	}
	else
        {
		int var_index;
		for (var_index = 0; var_index < NUMBER_OF_VARIABLES; var_index++)
#ifdef WANT_DLL
			set_var_value(var_index, empty_string, NULL);
		for (dll = dll_variable; dll; dll = dll->next)
			set_var_value(-1, empty_string, dll);
#else
			set_var_value(var_index, empty_string);  
#endif
	}
}

#if 0
/*
 * set_variable: The SET command sets one of the irc variables.  The args
 * should consist of "variable-name setting", where variable name can be
 * partial, but non-ambbiguous, and setting depends on the variable being set 
 */
void window_set_variable(win, args, subargs)
	Window	*win;
	char	*args,
		*subargs;
{
	char	*var;
	int	cnt;
enum WVAR_TYPES	var_index;

	if ((var = next_arg(args, &args)) != NULL)
	{
		if (*var == '-')
		{
			var++;
			args = NULL;
		}
		var_index = win_find_variable(win, var, &cnt);
		switch (cnt)
		{
			case 0:
				say("No such Window variable \"%s\"", var);
				return;
			case 1:
				window_set_var_value(win, var_index, args);
				return;
			default:
				say("%s is ambiguous", var);
				for (cnt += var_index; var_index < cnt; var_index++)
					window_set_var_value(win, var_index, empty_string);
				return;
		}
	}
	else
        {
		int var_index;
		for (var_index = 0; var_index < WIN_NUMBER_OF_VARIABLES; var_index++)
			window_set_var_value(win, var_index, empty_string);
	}
}
#endif

#ifdef _WANT_ANSI
void cset_ansi_display(ChannelList *chan)
{
put_it("%s",convert_output_format("    ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ[ channel sets ]ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸", NULL));
put_it("%s",convert_output_format("    ³  Channel: $[55]0 ³", "%s", chan->channel));
put_it("%s",convert_output_format("    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄ[Toggle]ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄ[Time] [k] [deop]´", NULL));
put_it("%s",convert_output_format("    ³ PubFlood  $[-3]0  KickFlood $[-3]1  LameNick  $[-3]2 ³ Pub   $[-3]3  $[-3]4  $[-3]5 ³",
"%s %s %s %d %d %d", 
on_off(chan->set_pubflood),on_off(chan->set_kickflood),on_off(chan->set_lamelist), chan->set_pubflood_time, chan->set_kick_on_pubflood, chan->set_pubflood_ignore));
put_it("%s",convert_output_format("    ³ NickFlood $[-3]0  JoinFlood $[-3]1  KickOnBan $[-3]2 ³ Nick  $[-3]3  $[-3]4      ³", "%s %s %s %d %d", on_off(chan->set_nickflood), on_off(chan->set_joinflood), on_off(chan->set_kick_if_banned), chan->set_nickflood_time, chan->set_kick_on_nickflood));
put_it("%s",convert_output_format("    ³ DeopFlood $[-3]0  UserList  $[-3]1  ShitList  $[-3]2 ³ Kick  $[-3]3  $[-3]4  $[-3]5 ³", "%s %s %s %d %d %d", on_off(chan->set_deopflood), on_off(chan->set_userlist), on_off(chan->set_shitlist), chan->set_kickflood_time, chan->set_kick_on_kickflood, chan->set_deop_on_deopflood));
put_it("%s",convert_output_format("    ³ Hacking   $[-3]0  AutoRJoin $[-3]1  AInv      $[-3]2 ³ De-Op $[-3]3  $[-3]4  $[-3]5 ³", "%d %d %d %d %d %d ", chan->set_hacking, chan->set_auto_rejoin, chan->set_ainv, chan->set_deopflood_time, chan->set_kick_on_deopflood, chan->set_deop_on_kickflood));
put_it("%s",convert_output_format("    ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾", NULL));

}
#endif

void cset_variable(char *command, char *args, char *subargs)
{
	char	*var = NULL;
	char *channel = NULL, *tmp;
	ChannelList *tmp_chan;
	int	cnt;
enum CVAR_TYPES	var_index;
extern int from_server;
	tmp = next_arg(args, &args);

	if (tmp && *tmp == '#')
	{
		channel = tmp;
		var = next_arg(args, &args);
	}
	else
		var = tmp;
	
	if (channel)
		tmp_chan = (ChannelList *)find_in_list((List **)&(server_list[from_server].chan_list), channel, 0);
	else
	{
		channel = get_channel_by_refnum(0);	
		if (channel)
			tmp_chan = (ChannelList *)find_in_list((List **)&server_list[from_server].chan_list, channel, 0);
		else
		{
			bitchsay("No current channel");
			return;
		}
	}
	if (!tmp_chan)
		return;	
	if (var && !*var)
		var = NULL;

	if (var)
		var_index = cfind_variable(var, &cnt);
	else
	{
		var_index = 0;
		cnt = NUMBER_OF_CVARIABLES;
	}
	switch (cnt)
	{
		case 0:
			say("No such variable \"%s\"", var);
			return;
		case 1:
			set_cvar_value(var_index, args, tmp_chan);
			return;
		default:
#ifdef _WANT_ANSI
			cset_ansi_display(tmp_chan);
#else
			for (cnt += var_index; var_index < cnt; var_index++)
				set_cvar_value(var_index, empty_string, tmp_chan);
#endif
			return;
	}
}

/*
 * save_variables: this writes all of the IRCII variables to the given FILE
 * pointer in such a way that they can be loaded in using LOAD or the -l switch 
 */
void save_variables(FILE *fp, int do_all)
{
	IrcVariable *var;

	for (var = irc_variable; var->name; var++)
	{
		if (!(var->int_flags & VIF_CHANGED))
			continue;
		if ((do_all == 1) || !(var->int_flags & VIF_GLOBAL))
		{
			if (strcmp(var->name, "DISPLAY") == 0 || strcmp(var->name, "CLIENT_INFORMATION") == 0)
				continue;
			fprintf(fp, "SET ");
			switch (var->type)
			{
			case BOOL_TYPE_VAR:
				fprintf(fp, "%s %s\n", var->name, var->integer ?
					var_settings[ON] : var_settings[OFF]);
				break;
			case CHAR_TYPE_VAR:
				fprintf(fp, "%s %c\n", var->name, var->integer);
				break;
			case INT_TYPE_VAR:
				fprintf(fp, "%s %u\n", var->name, var->integer);
				break;
			case STR_TYPE_VAR:
				if (var->string)
					fprintf(fp, "%s %s\n", var->name,
						var->string);
				else
					fprintf(fp, "-%s\n", var->name);
				break;
			}
		}
	}
}

void savebitchx_variables(FILE *fp)
{
	IrcVariable *var;
	int count = 0;
	for (var = irc_variable; var->name; var++)
	{
#if 0
		if (!(var->int_flags & VIF_CHANGED))
			continue;
#endif
		if (!(var->flags & VIF_BITCHX))
			continue;
		count++;
		fprintf(fp, "SET ");
		switch (var->type)
		{
		case BOOL_TYPE_VAR:
			fprintf(fp, "%s %s\n", var->name, var->integer ?
				var_settings[ON] : var_settings[OFF]);
			break;
		case CHAR_TYPE_VAR:
			fprintf(fp, "%s %c\n", var->name, var->integer);
			break;
		case INT_TYPE_VAR:
			fprintf(fp, "%s %u\n", var->name, var->integer);
			break;
		case STR_TYPE_VAR:
			if (var->string)
				fprintf(fp, "%s %s\n", var->name,
					var->string);
			else
				fprintf(fp, "-%s\n", var->name);
			break;
		}
	}
	bitchsay("Saved %d variables", count);
}

char	*make_string_var(char *var_name)
{
	int	cnt,
		var_index;
	char	buffer[BIG_BUFFER_SIZE + 1],
		*ret = NULL;

	if (((var_index = find_variable(var_name, &cnt)) == -1) ||
	    (cnt > 1) || my_stricmp(var_name,irc_variable[var_index].name))
		return NULL;
	switch (irc_variable[var_index].type)
	{
	case STR_TYPE_VAR:
		malloc_strcpy(&ret, irc_variable[var_index].string);
		break;
	case INT_TYPE_VAR:
		sprintf(buffer, "%u", irc_variable[var_index].integer);
		malloc_strcpy(&ret, buffer);
		break;
	case BOOL_TYPE_VAR:
		malloc_strcpy(&ret, var_settings[irc_variable[var_index].integer]);
		break;
	case CHAR_TYPE_VAR:
		sprintf(buffer, "%c", irc_variable[var_index].integer);
		malloc_strcpy(&ret, buffer);
		break;
	}
	return (ret);

}

char	*	cmake_string_var(char *var_name)
{
	int	cnt,
		var_index;
	char	buffer[BIG_BUFFER_SIZE + 1],
		*ret = NULL;

	if (((var_index = cfind_variable(var_name, &cnt)) == -1) ||
	    (cnt > 1) ||
	    my_stricmp(var_name,chan_variable[var_index].name))
		return NULL;
	switch (chan_variable[var_index].type)
	{
	case STR_TYPE_VAR:
		malloc_strcpy(&ret, chan_variable[var_index].string);
		break;
	case INT_TYPE_VAR:
		sprintf(buffer, "%u", chan_variable[var_index].integer);
		malloc_strcpy(&ret, buffer);
		break;
	case BOOL_TYPE_VAR:
		malloc_strcpy(&ret, var_settings[chan_variable[var_index].integer]);
		break;
	case CHAR_TYPE_VAR:
		sprintf(buffer, "%c", chan_variable[var_index].integer);
		malloc_strcpy(&ret, buffer);
		break;
	}
	return (ret);

}

/* exec_warning: a warning message displayed whenever EXEC_PROTECTION is turned off.  */
static	void exec_warning(Window *win, char *unused, int value)
{
	if (value == OFF)
	{
		bitchsay("Warning!  You have turned EXEC_PROTECTION off");
		bitchsay("Please read the /HELP SET EXEC_PROTECTION documentation");
	}
}

static	void input_warning(Window *win, char *unused, int value)
{
	if (value == OFF)
	{
		bitchsay("Warning!  You have turned INPUT_PROTECTION off");
		bitchsay("Please read the /HELP ON INPUT, and /HELP SET INPUT_PROTECTION documentation");
	}
}

/* returns the size of the character set */
int charset_size(void)
{
	return get_int_var(EIGHT_BIT_CHARACTERS_VAR) ? 256 : 128;
}

static	void eight_bit_characters(Window *win, char *unused, int value)
{
	if (value == ON && !term_eight_bit())
		say("Warning!  Your terminal says it does not support eight bit characters");
	set_term_eight_bit(value);
}

static	void set_realname(Window *win, char *value, int unused)
{
	strmcpy(realname, value, REALNAME_LEN);
}

void reinit_autoresponse(Window *win, char *value, int unused)
{
int old_window = window_display;
	window_display = 1;
	if (value)
		malloc_strcpy(&auto_str, value);
	else 
		bitchsay("Auto Response is set to - %s", auto_str);
	window_display = old_window;
}

static void set_numeric_string(Window *win, char *value, int unused)
{
	malloc_strcpy(&thing_ansi, value);
}

static void set_user_mode(Window *win, char *value, int unused)
{
	malloc_strcpy(&send_umode, value);
}

static void set_ov_mode (Window *win, char *value, int unused)
{
extern void toggle_ov _((char, char *));
int old_window_display = window_display;
	if (!curr_scr_win)
		return;
	window_display = 0;
	setup_ov_mode(unused ? 0 : 1);
	window_display = old_window_display;
}

static void set_away_time(Window *win, char *unused, int value)
{
	if (value == 0)
		set_int_var(AUTO_AWAY_TIME_VAR, 0);
	else if ((value / 60) == 0)		
		set_int_var(AUTO_AWAY_TIME_VAR, value * 60);
	else if (value < 60 * 10)
		set_int_var(AUTO_AWAY_TIME_VAR, 60 * 10);
	else
		set_int_var(AUTO_AWAY_TIME_VAR, value);
}

void clear_sets(void)
{
int i = 0;
	for(i = 0; irc_variable[i].name; i++)
	{
		new_free(&irc_variable->string);
	}
}

static void reinit_screen(Window *win, char *unused, int value)
{
	set_input_prompt(curr_scr_win, NULL, 0);
	set_input_prompt(curr_scr_win, get_string_var(INPUT_PROMPT_VAR), 0);
	update_all_windows();
	update_all_status(curr_scr_win, NULL, 0);
	update_input(UPDATE_ALL);
}
static void reinit_status(Window *win, char *unused, int value)
{
	update_all_windows();
	redraw_all_status();
}

int save_formats(FILE *outfile)
{
char thefile[BIG_BUFFER_SIZE+1];
char *p;
int i;
int count = 1;
	sprintf(thefile, "%s/%s.formats", get_string_var(CTOOLZ_DIR_VAR), version);
	p = expand_twiddle(thefile);
	outfile = fopen(p, "w");
	if (!outfile)
	{
		bitchsay("Cannot open file %s for saving!", thefile);
		new_free(&p);
		return 1;
	}
	for (i = FORMAT_381_VAR; i <= FORMAT_WINDOW_SET_VAR; i++)	
	{
		if (irc_variable[i].string)
		{
			fprintf(outfile, "SET %s %s\n", irc_variable[i].name, irc_variable[i].string);
			count++;
		}
	}
	fclose(outfile);
	bitchsay("Saved %d formats to %s", count, thefile);
	new_free(&p);
	return 0;
}
