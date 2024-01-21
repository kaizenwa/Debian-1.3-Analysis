/*
 * functions.c -- Built-in functions for ircII
 *
 * Written by Michael Sandrof
 * Copyright(c) 1990 Michael Sandrof
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 */

#include "irc.h"
#include "alias.h"
#include "array.h"
#include "dcc.h"
#include "edit.h"
#include "files.h"
#include "history.h"
#include "hook.h"
#include "input.h"
#include "ircaux.h"
#include "names.h"
#include "output.h"
#include "list.h"
#include "parse.h"
#include "screen.h"
#include "server.h"
#include "status.h"
#include "vars.h"
#include "window.h"
#include "ircterm.h"
#include "notify.h"
#include "misc.h"
#include "userlist.h"
#include "bot.h"
#include "numbers.h"

#include "bitchx"
#include <sys/stat.h>

static	char	*alias_detected _((void));
static	char	*alias_sent_nick _((void));
static	char	*alias_recv_nick _((void));
static	char	*alias_msg_body _((void));
static	char	*alias_joined_nick _((void));
static	char	*alias_public_nick _((void));
static	char	*alias_dollar _((void));
static	char	*alias_channel _((void));
static	char	*alias_server _((void));
static	char	*alias_query_nick _((void));
static	char	*alias_target _((void));
static	char	*alias_nick _((void));
static	char	*alias_invite _((void));
static	char	*alias_cmdchar _((void));
static	char	*alias_line _((void));
static	char	*alias_away _((void));
static	char	*alias_oper _((void));
static	char	*alias_chanop _((void));
static	char	*alias_modes _((void));
static	char	*alias_buffer _((void));
static	char	*alias_time _((void));
static	char	*alias_version _((void));
static	char	*alias_currdir _((void));
static	char	*alias_current_numeric _((void));
static	char	*alias_server_version _((void));
static  char	*alias_show_userhost _((void));
static	char	*alias_show_realname _((void));
static	char	*alias_online _((void));
static	char 	*alias_idle _((void));
static	char	*alias_version_str _((void));
static	char	*alias_thingansi _((void));
static	char	*alias_uptime _((void));

typedef struct
{
	char	name;
	char	*(*func) _((void));
}	BuiltIns;

static	BuiltIns built_in[] =
{
	{ '.',		alias_sent_nick },
	{ ',',		alias_recv_nick },
	{ ':',		alias_joined_nick },
	{ ';',		alias_public_nick },
	{ '`',		alias_uptime },
	{ '$',		alias_dollar },
	{ 'A',		alias_away },
	{ 'B',		alias_msg_body },
	{ 'C',		alias_channel },
	{ 'D',		alias_detected },
	{ 'E',		alias_idle },
	{ 'F',		alias_online },
	{ 'G',		alias_thingansi },
	{ 'H', 		alias_current_numeric },
	{ 'I',		alias_invite },
	{ 'J',		alias_version_str },
	{ 'K',		alias_cmdchar },
	{ 'L',		alias_line },
	{ 'M',		alias_modes },
	{ 'N',		alias_nick },
	{ 'O',		alias_oper },
	{ 'P',		alias_chanop },
	{ 'Q',		alias_query_nick },
	{ 'R',		alias_server_version },
	{ 'S',		alias_server },
	{ 'T',		alias_target },
	{ 'U',		alias_buffer },
	{ 'V',		alias_version },
	{ 'W',		alias_currdir },
	{ 'X',		alias_show_userhost },
	{ 'Y',		alias_show_realname },
	{ 'Z',		alias_time },
	{ 0,	 	NULL }
};

/* the 30 "standard" functions */
static	char	*function_channels 	_((char *));
static	char	*function_connect 	_((char *));
static	char	*function_curpos 	_((char *));
static	char	*function_decode 	_((unsigned char *));
static	char	*function_encode 	_((unsigned char *));
static	char	*function_index 	_((char *));
static	char	*function_ischannel 	_((char *));
static	char	*function_ischanop 	_((char *));
static	char	*function_left 		_((char *));
static	char	*function_listen 	_((char *));
static	char	*function_match 	_((char *));
static	char	*function_mid 		_((char *));
static	char	*function_notify	_((char *));
static	char	*function_pid 		_((char *));
static	char	*function_ppid 		_((char *));
static	char	*function_rand 		_((char *));
static	char	*function_right 	_((char *));
static	char	*function_rindex 	_((char *));
static	char	*function_rmatch 	_((char *));
static	char	*function_servers 	_((char *));
static	char	*function_srand 	_((char *));
static	char	*function_stime 	_((char *));
static	char	*function_strip 	_((char *));
static	char	*function_tdiff 	_((char *));
static	char	*function_tdiff2 	_((char *));
static	char	*function_time 		_((char *));
static	char	*function_tolower 	_((char *));
static	char	*function_toupper 	_((char *));
static	char	*function_userhost 	_((char *));
static	char	*function_winnum 	_((char *));
static	char	*function_winnam 	_((char *));
static	char	*function_word 		_((char *));
static	char	*function_utime		_((char *));
static	char	*function_umode		_((char *));

/* CDE added functions */
static	char	*function_eleet		_(( char *));
static	char	*function_cool		_(( char *));
static	char 	*function_annoy		_(( char *));
static	char 	*function_uptime	_(( char *));
static  char	*function_cluster	_((char *));
static  char	*function_checkshit	_((char *));
static	char	*function_checkuser	_((char *));
static	char	*function_checkbot	_((char *));
static	char	*function_rot13		_((char *));
	char	*function_addtabkey	_((char *));
	char	*function_gettabkey	_((char *));
static	char	*function_lastnotice	_((char *));
static	char	*function_lastmessage	_((char *));
static	char	*function_userver	_((char *));
static  char	*function_help		_((char *));
static	char	*function_isuser	_((char *));
/* Thanks Jordy */
static 	char	*function_pad		_((char *));
static	char	*function_isban		_((char *));
static	char	*function_isop		_((char *));
static	char	*function_isvoice	_((char *));
static	char	*function_randomnick	_((char *));

static	char	*function_openserver	_((char *));
static	char	*function_readserver	_((char *));
static	char	*function_writeserver	_((char *));
static	char	*function_closeserver	_((char *));
static	char	*function_getreason	_((char *));

/* the 53 "extended" functions */
static	char *	function_after 		_((char *));
static	char *	function_afterw 	_((char *));
static	char *	function_aliasctl	_((char *));
static	char *	function_ascii 		_((char *));
static	char *	function_before 	_((char *));
static	char *	function_beforew 	_((char *));
static	char *	function_center 	_((char *));
static	char *	function_channelmode	_((char *));
static	char *	function_channelnicks	_((char *));
static	char *	function_chngw 		_((char *));
static	char *	function_chops 		_((char *));
static	char *	function_chr 		_((char *));
static	char *	function_close 		_((char *));
static	char *	function_common 	_((char *));
static	char *	function_convert 	_((char *));
static	char *	function_copattern 	_((char *));
static	char *	function_crypt 		_((char *));
static	char *	function_diff 		_((char *));
static	char *	function_epic 		_((char *));
static	char *	function_eof 		_((char *));
static	char *	function_glob	 	_((char *));
static	char *	function_fexist 	_((char *));
static	char *	function_filter 	_((char *));
static	char *	function_fromw 		_((char *));
static	char *	function_fsize	 	_((char *));
static	char *	function_geom		_((char *));
static	char *	function_info		_((char *));
static	char *	function_insertw 	_((char *));
static	char *	function_iptoname 	_((char *));
static	char *	function_isalpha 	_((char *));
static	char *	function_isdigit 	_((char *));
static	char *  function_isnum		_((char *));
static	char *	function_jot 		_((char *));
static	char *	function_key 		_((char *));
static	char *	function_lastserver	_((char *));
static	char *	function_leftw 		_((char *));
static	char *	function_mkdir		_((char *));
static	char *	function_midw 		_((char *));
static	char *	function_nametoip 	_((char *));
static	char *	function_nochops 	_((char *));
static	char *	function_notw 		_((char *));
static	char *	function_numonchannel 	_((char *));
static	char *	function_numwords 	_((char *));
static	char *	function_numsort 	_((char *));
static	char *	function_onchannel 	_((char *));
static	char *	function_open 		_((char *));
static	char *	function_pass		_((char *));
static	char *	function_pattern 	_((char *));
static	char *	function_read 		_((char *));
static	char *	function_remw 		_((char *));
static	char *	function_rename 	_((char *));
static	char *	function_restw 		_((char *));
static	char *	function_reverse 	_((char *));
static	char *	function_revw 		_((char *));
static	char *	function_rfilter 	_((char *));
static	char *	function_rightw 	_((char *));
static	char *	function_rmdir 		_((char *));
static	char *	function_rpattern 	_((char *));
static	char *	function_sar 		_((char *));
static	char *	function_server_version _((char *));
static	char *	function_servername	_((char *));
static	char *	function_sort		_((char *));
static	char *	function_split 		_((char *));
static	char *	function_splice 	_((char *));
static	char *	function_stripansi	_((char *));
static	char *	function_stripansicodes	_((char *));
static	char *	function_stripmirc	_((char *));
static	char *  function_strftime	_((char *));
static	char *	function_strlen		_((char *));
static	char *	function_tow 		_((char *));
static	char *	function_translate 	_((char *));
static	char *	function_truncate 	_((char *));
static	char *	function_unlink 	_((char *));
static	char *	function_umask		_((char *));
static	char *	function_which 		_((char *));
static	char *	function_winserv	_((char *));
static	char *	function_winsize	_((char *));
static	char *	function_write 		_((char *));
static	char *	function_writeb		_((char *));
static	char *  function_idle		_((char *));
static	char *	function_flash		_((char *));
static	char *	function_repeat		_((char *));
static	char *  function_bcopy		_((char *));
static	char *	function_cparse		_((char *));
static	char *	function_chmod		_((char *));
static	char *	function_twiddle	_((char *));
static	char *	function_uniq		_((char *));
static	char *	function_uhost 		_((char *));
static	char *	function_numdiff	_((char *));
	char *	function_getkey		_((char *));
static	char *  function_winvisible	_((char *));
static	char *	function_mircansi	_((char *));
static	char *	function_banonchannel	_((char *));
static	char *	function_winrefs	_((char *));
static	char *	function_gethost	_((char *));
static	char *	function_getenv		_((char *));
static	char *	function_getvar		_((char *));

/* 
 * This is the built-in function list.  This list *must* be sorted because
 * it is binary searched.   See the code for each function to see how it
 * is used.  Or see the help files.  Or see both.  Or look at the code
 * and see how it REALLY works, irregardless of the documentation >;-)
 */
static BuiltInFunctions	built_in_functions[] =
{
	{ "ADDTABKEY",		function_addtabkey	},
	{ "AFTER",              function_after 		},
	{ "AFTERW",             function_afterw 	},
	{ "ALIASCTL",		function_aliasctl	},
	{ "ANNOY",		function_annoy		},
	{ "ASCII",              function_ascii 		},
	{ "BANONCHANNEL",	function_banonchannel	},
	{ "BCOPY",		function_bcopy		},
	{ "BEFORE",             function_before 	},
	{ "BEFOREW",            function_beforew 	},
	{ "BITCHX",		function_epic		},
	{ "CENTER",		function_center 	},
	{ "CHANMODE",		function_channelmode	},
	{ "CHANNICKS",		function_channelnicks	},
	{ "CHANUSERS",		function_onchannel 	},
	{ "CHECKBOT",		function_checkbot	},
	{ "CHECKSHIT",		function_checkshit	},
	{ "CHECKUSER",		function_checkuser	},
	{ "CHMOD",		function_chmod		},
	{ "CHNGW",              function_chngw 		},
	{ "CHOPS",              function_chops 		},
	{ "CHR",                function_chr 		},
	{ "CLOSE",		function_close 		},
	{ "CLOSESOCKET",	function_closeserver	},
	{ "CLUSTER",		function_cluster	},
	{ "COMMON",             function_common 	},
	{ "CONNECT",		function_connect 	},
	{ "CONVERT",		function_convert 	},
	{ "COOL",		function_cool		},
	{ "COPATTERN",          function_copattern 	},
	{ "CPARSE",		function_cparse		},
	{ "CRYPT",		function_crypt		},
	{ "CURPOS",		function_curpos 	},
	{ "DECODE",	  (bf *)function_decode 	},
	{ "DELARRAY",           function_delarray 	},
	{ "DELITEM",            function_delitem 	},
	{ "DIFF",               function_diff 		},
	{ "ELEET",		function_eleet		},
	{ "ENCODE",	  (bf *)function_encode 	},
	{ "EOF",		function_eof 		},
	{ "EPIC",		function_epic		},
	{ "FEXIST",             function_fexist 	},
	{ "FILTER",             function_filter 	},
	{ "FINDITEM",           function_finditem 	},
	{ "FLASH",		function_flash		},
	{ "FROMW",              function_fromw 		},
	{ "FSIZE",		function_fsize		},
	{ "GEOM",		function_geom		},
	{ "GETARRAYS",          function_getarrays 	},
	{ "GETENV",		function_getenv		},
	{ "GETHOST",		function_gethost	},
	{ "GETITEM",            function_getitem 	},
	{ "GETKEY",		function_getkey		},
	{ "GETMATCHES",         function_getmatches 	},
	{ "GETREASON",		function_getreason	}, 
	{ "GETRMATCHES",        function_getrmatches 	},
	{ "GETTABKEY",		function_gettabkey	},
	{ "GETVAR",		function_getvar		},
	{ "GLOB",		function_glob		},
	{ "HELP",		function_help		},
	{ "IDLE",		function_idle		},
	{ "IFINDFIRST",         function_ifindfirst 	},
	{ "IFINDITEM",          function_ifinditem 	},
	{ "IGETITEM",           function_igetitem 	},
	{ "INDEX",		function_index 		},
	{ "INDEXTOITEM",        function_indextoitem 	},
	{ "INFO",		function_info		},
	{ "INSERTW",            function_insertw 	},
	{ "IPTONAME",		function_iptoname 	},
	{ "ISALPHA",		function_isalpha 	},
	{ "ISBAN",		function_isban		},
	{ "ISCHANNEL",		function_ischannel 	},
	{ "ISCHANOP",		function_ischanop 	},
	{ "ISDIGIT",		function_isdigit 	},
	{ "ISNUM",		function_isnum		},
	{ "ISOP",		function_isop		},
	{ "ISUSER",		function_isuser		},
	{ "ISVOICE",		function_isvoice	},
	{ "ITEMTOINDEX",        function_itemtoindex 	},
	{ "JOT",                function_jot 		},
	{ "KEY",                function_key 		},
	{ "LASTMESSAGE",	function_lastmessage	},
	{ "LASTNOTICE",		function_lastnotice	},
	{ "LASTSERVER",		function_lastserver	},
	{ "LEFT",		function_left 		},
	{ "LEFTW",              function_leftw 		},
	{ "LISTEN",		function_listen 	},
	{ "MATCH",		function_match 		},
	{ "MATCHITEM",          function_matchitem 	},
	{ "MID",		function_mid 		},
	{ "MIDW",               function_midw 		},
	{ "MIRCANSI",		function_mircansi	},
	{ "MKDIR",		function_mkdir		},
	{ "MYCHANNELS",		function_channels 	},
	{ "MYSERVERS",		function_servers 	},
	{ "NAMETOIP",		function_nametoip 	},
	{ "NOCHOPS",            function_nochops 	},
	{ "NOTIFY",		function_notify		},
	{ "NOTW",               function_notw 		},
	{ "NUMARRAYS",          function_numarrays 	},
	{ "NUMDIFF",		function_numdiff	},
	{ "NUMITEMS",           function_numitems 	},
	{ "NUMONCHANNEL",	function_numonchannel 	},
	{ "NUMSORT",		function_numsort	},
	{ "NUMWORDS",		function_numwords	},
	{ "ONCHANNEL",          function_onchannel 	},
	{ "OPEN",		function_open 		},
	{ "OPENSOCKET",		function_openserver	},
	{ "PAD",		function_pad		},
	{ "PASS",		function_pass		},
	{ "PATTERN",            function_pattern 	},
	{ "PID",		function_pid 		},
	{ "POP",		function_pop 		},
	{ "PPID",		function_ppid 		},
	{ "PUSH",		function_push 		},
	{ "RAND",		function_rand 		},
	{ "RANDOMNICK",		function_randomnick	},
	{ "READ",		function_read 		},
	{ "READSOCKET",		function_readserver	},
	{ "REMW",               function_remw 		},
	{ "RENAME",		function_rename 	},
	{ "REPEAT",		function_repeat		},
	{ "RESTW",              function_restw 		},
	{ "REVERSE",            function_reverse 	},
	{ "REVW",               function_revw 		},
	{ "RFILTER",            function_rfilter 	},
	{ "RIGHT",		function_right 		},
	{ "RIGHTW",             function_rightw 	},
	{ "RINDEX",		function_rindex 	},
	{ "RMATCH",		function_rmatch 	},
	{ "RMATCHITEM",         function_rmatchitem 	},
	{ "RMDIR",		function_rmdir 		},
	{ "ROT13",		function_rot13		},
	{ "RPATTERN",           function_rpattern 	},
	{ "SAR",		function_sar 		},
	{ "SERVERNAME",		function_servername	},
	{ "SETITEM",            function_setitem 	},
	{ "SHIFT",		function_shift 		},
	{ "SORT",		function_sort		},
	{ "SPLICE",		function_splice 	},
	{ "SPLIT",		function_split 		},
	{ "SRAND",		function_srand 		},
	{ "STIME",		function_stime 		},
	{ "STRFTIME",		function_strftime	},
	{ "STRIP",		function_strip 		},
	{ "STRIPANSI",		function_stripansi	},
	{ "STRIPANSICODES",	function_stripansicodes	},
	{ "STRIPMIRC",		function_stripmirc	},
	{ "STRLEN",		function_strlen		},
	{ "TDIFF",		function_tdiff 		},
	{ "TDIFF2",		function_tdiff2 	},
	{ "TIME",		function_time 		},
	{ "TOLOWER",		function_tolower 	},
	{ "TOUPPER",		function_toupper 	},
	{ "TOW",                function_tow 		},
	{ "TR",			function_translate 	},
	{ "TRUNC",		function_truncate 	},
	{ "TWIDDLE",		function_twiddle	},
	{ "UHOST",		function_uhost		},
	{ "UMASK",		function_umask		},
	{ "UNIQ",		function_uniq		},
	{ "UNLINK",		function_unlink 	},
	{ "UNSHIFT",		function_unshift 	},
	{ "UPTIME",		function_uptime		},
	{ "USERHOST",		function_userhost 	},
	{ "USERMODE",		function_umode		},
	{ "USERVER",		function_userver	},
	{ "UTIME",		function_utime	 	},
	{ "VERSION",		function_server_version },
	{ "WHICH",		function_which 		},
	{ "WINNAM",		function_winnam 	},
	{ "WINNUM",		function_winnum 	},
	{ "WINREFS",		function_winrefs	},
	{ "WINSERV",		function_winserv	},
	{ "WINSIZE",		function_winsize	},
	{ "WINVISIBLE",		function_winvisible	},
	{ "WORD",		function_word 		},
	{ "WRITE",		function_write 		},
	{ "WRITEB",		function_writeb		},
	{ "WRITESOCKET",	function_writeserver	},
	{ NULL,			NULL 			}
};

#define	NUMBER_OF_ALIASES (sizeof(built_in_functions) / sizeof(BuiltInFunctions)) - 2
int in_cparse  = 0;
extern time_t start_time;

/* function_stack and function_stkptr - hold the return values from functions */
#if 0
static	char	*function_stack[128] = { NULL };
static	int	function_stkptr = 0;
#endif

char **get_builtins(char *name, int len, char **matches, int matches_size, int *cnt)
{
char *last_match = NULL;
int i = 0;
	while (built_in_functions[i].func && i <= NUMBER_OF_ALIASES)
	{
		if (strncmp(name, built_in_functions[i].name, len) == 0)
		{
			matches[*cnt] = NULL;
			malloc_strcpy(&(matches[*cnt]), built_in_functions[i].name);
			last_match = matches[*cnt];
			if (++(*cnt) == matches_size)
			{
				matches_size += 5;
				matches = (char	**) new_realloc((char *)matches,
					sizeof(char *) * matches_size);
			}
		}
		else if (*cnt)
			break;
		i++;
	}
	return matches;
}

char	*built_in_alias (char c)
{
	BuiltIns	*tmp;

	for (tmp = built_in;tmp->name;tmp++)
		if (c == tmp->name)
			return tmp->func();

	return m_strdup(empty_string);
}

char	*call_function (char *name, char *f_args, char *args, int *args_flag)
{
	char	*tmp;
	char	*result = NULL;
	char	*debug_copy = NULL;
	int min, max, pos, old_pos = -1, c;
#ifdef WANT_DLL	
	BuiltInDllFunctions *dll = NULL;
#endif	
	tmp = expand_alias(NULL, f_args, args, args_flag, NULL);
	if (get_int_var(DEBUG_VAR) & DEBUG_FUNCTIONS)
		malloc_strcpy(&debug_copy, tmp);

	upper(name);
	min = 0;
	max = NUMBER_OF_ALIASES + 1;
#ifdef WANT_DLL
	for (dll = dll_functions; dll; dll = dll->next)
		if (!strcmp(name, dll->name))
			break;
#endif
	while (1)
	{
		pos = (max + min) / 2;
		if (pos == old_pos)
		{
			pos = -1;
			break;
		}
		old_pos = pos;
		c = strcmp(name, built_in_functions[pos].name);
		if (c == 0)
			break;
		else if (c > 0)
			min = pos;
		else
			max = pos;
	}
#ifdef WANT_DLL
	if (dll)
		result = (dll->func)(tmp);
	else 
#endif
	if (pos != -1)
		result = built_in_functions[pos].func(tmp);
	else
		result = call_user_function(name, tmp);
	if (debug_copy)
	{
		yell("Function %s(%s) returned %s", name, debug_copy, result);
		new_free(&debug_copy);
	}
	new_free(&tmp);
	return result;
}



/* built in expando functions */
static	char	*alias_line 		_((void)) { return m_strdup(get_input()); }
static	char	*alias_buffer 		_((void)) { return m_strdup(cut_buffer); }
static	char	*alias_time 		_((void)) { return m_strdup(update_clock(GET_TIME)); }
static	char	*alias_dollar 		_((void)) { return m_strdup("$"); }
static	char	*alias_detected 	_((void)) { return m_strdup(last_notify_nick); }
static	char	*alias_nick 		_((void)) { return m_strdup(curr_scr_win->server != -1? get_server_nickname(curr_scr_win->server) : empty_string); }
static	char	*alias_away 		_((void)) { return m_strdup(curr_scr_win->server!= -1 ? (server_list[curr_scr_win->server].away ? server_list[curr_scr_win->server].away : empty_string):empty_string); }
static	char	*alias_sent_nick 	_((void)) { return m_strdup((sent_nick) ? sent_nick : empty_string); }
static	char	*alias_recv_nick 	_((void)) { return m_strdup((recv_nick) ? recv_nick : empty_string); }
static	char	*alias_msg_body 	_((void)) { return m_strdup((sent_body) ? sent_body : empty_string); }
static	char	*alias_joined_nick 	_((void)) { return m_strdup((joined_nick) ? joined_nick : empty_string); }
static	char	*alias_public_nick 	_((void)) { return m_strdup((public_nick) ? public_nick : empty_string); }
static  char    *alias_show_realname 	_((void)) { return m_strdup(realname); }
static	char	*alias_version_str 	_((void)) { return m_strdup(irc_version); }
static	char	*alias_invite 		_((void)) { return m_strdup((invite_channel) ? invite_channel : empty_string); }
static	char	*alias_oper 		_((void)) { return m_strdup(get_server_operator(from_server) ?  get_string_var(STATUS_OPER_VAR) : empty_string); }
static	char	*alias_version 		_((void)) { return m_strdup(internal_version); }
static  char    *alias_online 		_((void)) { return m_sprintf("%ld",(long)start_time); }
static  char    *alias_idle 		_((void)) { return m_sprintf("%ld",time(NULL)-idle_time); }
static  char    *alias_show_userhost 	_((void)) { return m_sprintf("%s@%s",username, hostname); }
static	char	*alias_current_numeric	_((void)) { return m_sprintf("%03d", -current_numeric); }
static	char	*alias_thingansi	_((void)) { return m_sprintf("%s", numeric_banner()); }
static	char	*alias_uptime		_((void)) { return m_sprintf("%s", convert_time(time(NULL)-start_time)); }
static	char	*alias_currdir  	_((void))
{
	char 	*tmp = (char *)new_malloc(MAXPATHLEN+1);
	return getcwd(tmp, MAXPATHLEN);
}

static	char	*alias_channel 		_((void)) 
{ 
	char	*tmp; 
#if 1
	return m_strdup((tmp = get_channel_by_refnum(0)) ? tmp : "0");
#else
	return ((tmp = get_channel_by_refnum(0)) ? tmp : "0");
#endif
}

static	char	*alias_server 		_((void)) 
{
	return m_strdup((parsing_server_index == -1) ?
		         get_server_itsname(parsing_server_index) :
		         (get_window_server(0) != -1) ?
			        get_server_itsname(get_window_server(0)) : empty_string);
}

static	char	*alias_query_nick 	_((void))
{
	char	*tmp;
	return m_strdup((tmp = query_nick()) ? tmp : empty_string);
}

static	char	*alias_target 		_((void))
{
	char	*tmp;
	return m_strdup((tmp = get_target_by_refnum(0)) ? tmp : empty_string);
}

static	char	*alias_cmdchar 		_((void))
{
	char	*cmdchars, tmp[2];

	if ((cmdchars = get_string_var(CMDCHARS_VAR)) == NULL)
		cmdchars = DEFAULT_CMDCHARS;
	tmp[0] = cmdchars[0];
	tmp[1] = 0;
	return m_strdup(tmp);
}

static	char	*alias_chanop 		_((void))
{
	char	*tmp;
	return m_strdup(((tmp = get_channel_by_refnum(0)) && get_channel_oper(tmp, from_server)) ?
		"@" : empty_string);
}

static	char	*alias_modes 		_((void))
{
	char	*tmp;
	return m_strdup((tmp = get_channel_by_refnum(0)) ?
		get_channel_mode(tmp, from_server) : empty_string);
}

static	char	*alias_server_version  _((void))
{
	return function_server_version(ltoa(curr_scr_win->server));
}


/*	*	*	*	*	*	*	*	*	*
		These are the built-in functions.

	About 80 of them are here, the rest are in array.c.  All of the
	stock client's functions are supported, as well as about 60 more.
	Most of the 30 stock client's functions have been re-written for
	optimization reasons, and also to further distance ircii's code
	from EPIC.
 *	*	*	*	*	*	*	*	*	*/

/* 
 * These are defined to make the construction of the built-in functions
 * easier and less prone to bugs and unexpected behaviors.  As long as
 * you consistently use these macros to do the dirty work for you, you
 * will never have to do bounds checking as the macros do that for you. >;-) 
 *
 * Yes, i realize it makes the code slightly less efficient, but i feel that 
 * the cost is minimal compared to how much time i have spent over the last 
 * year debugging these functions and the fact i wont have to again. ;-)
 */
#define EMPTY empty_string
#define RETURN_EMPTY return m_strdup(EMPTY)
#define RETURN_IF_EMPTY(x) if (empty( x )) RETURN_EMPTY
#define GET_INT_ARG(x, y) {RETURN_IF_EMPTY(y); x = atol(safe_new_next_arg(y, &y));}
#define GET_FLOAT_ARG(x, y) {RETURN_IF_EMPTY(y); x = atof(safe_new_next_arg(y, &y));}
#define GET_STR_ARG(x, y) {RETURN_IF_EMPTY(y); x = new_next_arg(y, &y);RETURN_IF_EMPTY(x);}
#define RETURN_STR(x) return m_strdup(x ? x : EMPTY)
#define RETURN_INT(x) return m_strdup(ltoa(x));

#ifdef __STDC__
#define BUILT_IN_FUNCTION(x, y) static char * x (char * y)
#else 
#define BUILT_IN_FUNCTION(x, y) static char * x ( y ) char * y; 
#endif

/*
 * Usage: $left(number text)
 * Returns: the <number> leftmost characters in <text>.
 * Example: $left(5 the quick brown frog) returns "the q"
 *
 * Note: the difference between $[10]foo and $left(10 foo) is that the former
 * is padded and the latter is not.
 */
BUILT_IN_FUNCTION(function_left, word)
{
	long	count;

	GET_INT_ARG(count, word);
	RETURN_IF_EMPTY(word);

	if (strlen(word) > count)
		word[count] = 0;

	RETURN_STR(word);
}

extern char *channel_key(char * channel);

char    *function_getkey(input)
char    *input;
{
	char *temp;
	RETURN_IF_EMPTY(input);
	temp = channel_key(input);
	RETURN_STR(temp);
}

/*
 * Usage: $right(number text)
 * Returns: the <number> rightmost characters in <text>.
 * Example: $right(5 the quick brown frog) returns " frog"
 */
BUILT_IN_FUNCTION(function_right, word)
{
	long	count;

	GET_INT_ARG(count, word);
	RETURN_IF_EMPTY(word);

	if (strlen(word) > count)
		word += strlen(word) - count;

	RETURN_STR(word);
}

/*
 * Usage: $mid(start number text)
 * Returns: the <start>th through <start>+<number>th characters in <text>.
 * Example: $mid(3 4 the quick brown frog) returns " qui"
 *
 * Note: the first character is numbered zero.
 */
BUILT_IN_FUNCTION(function_mid, word)
{
	long	start, length;

	GET_INT_ARG(start, word);
	GET_INT_ARG(length, word);
	RETURN_IF_EMPTY(word);

	if (start < strlen(word))
	{
		word += start;
		if (length < strlen(word))
			word[length] = 0;
	}
	else
		word = EMPTY;

	RETURN_STR(word);
}

#if defined(HAVE_GETTIMEOFDAY) && !defined(OLD_RANDOM_BEHAVIOR)
static unsigned long randl _((void))
{
	struct timeval tp1;
	get_time(&tp1);
	return (unsigned long) tp1.tv_usec;
}
#endif

/* patch from Sarayan to make $rand() better */

#define RAND_A 16807L
#define RAND_M 2147483647L
#define RAND_Q 127773L
#define RAND_R 2836L

#ifdef __STDC__
static	unsigned long	randm(unsigned long l)
#else
static	unsigned long	randm(l)
	unsigned long	l;
#endif
{
/* 
     If we have gettimeofday(2), we use it, because its 'leet.
	-- basically how it works is we take two samplings of 
	   gettimeofday().  Since who knows how long apart the samples
	   will be, we can be assured theyre *NOT* psudeo-random.  Then
	   we take the lower 16 bits of each sample, and then pack them
	   into a long, and viola, a random number.
	-- oh.  since we know that the usec field of timeval is microseconds,
	   the lower 16 bits change at a constant rate, which we cant say
	   for the 17th bit. >;-)
 */
#if defined(HAVE_GETTIMEOFDAY) && !defined(OLD_RANDOM_BEHAVIOR)
	unsigned long t1, t2, t;

	t1 = randl();
	t2 = randl();
	t = (t1 & 65535) * 65536 + (t2 & 65535);
	return t;
#else
	static	u_long	z = 0;
	long	t;

	if (!z)
		z = (u_long) getuid();
	if (!l)
	{
		t = RAND_A * (z % RAND_Q) - RAND_R * (z / RAND_Q);
		if (t > 0)
			z = t;
		else
			z = t + RAND_M;
		return (z >> 8) | ((z & 255) << 23);
	}
	else
	{
		if (l < 0)
			z = (u_long) getuid();
		else
			z = l;
		return 0;
	}
#endif
}

/*
 * Usage: $rand(max)
 * Returns: A random number from zero to max-1.
 * Example: $rand(10) might return any number from 0 to 9.
 */
BUILT_IN_FUNCTION(function_rand, word)
{
	long	tempin;

	GET_INT_ARG(tempin, word);
	if (tempin == 0)
		tempin = (unsigned long) -1;	/* This is cheating. :P */
	RETURN_INT(randm(0L) % tempin)
}

/*
 * Usage: $srand(seed)
 * Returns: Nothing.
 * Side effect: seeds the random number generater.
 * Note: the argument is ignored.
 */
BUILT_IN_FUNCTION(function_srand, word)
{
	randm((long) time(NULL));
	RETURN_EMPTY;
}

/*
 * Usage: $time()
 * Returns: The number of seconds that has elapsed since Jan 1, 1970, GMT.
 * Example: $time() returned something around 802835348 at the time I
 * 	    wrote this comment.
 */
BUILT_IN_FUNCTION(function_time, input)
{
	RETURN_INT(time(NULL))
}

/*
 * Usage: $stime(time)
 * Returns: The human-readable form of the date based on the <time> argument.
 * Example: $stime(1000) returns what time it was 1000 seconds from the epoch.
 * 
 * Note: $stime() is really useful when you give it the argument $time(), ala
 *       $stime($time()) is the human readable form for now.
 */
BUILT_IN_FUNCTION(function_stime, input)
{
	time_t	ltime;

	GET_INT_ARG(ltime, input);
	RETURN_STR(my_ctime(ltime));
}

/*
 * Usage: $tdiff(seconds)
 * Returns: The time that has elapsed represented in days/hours/minutes/seconds
 *          corresponding to the number of seconds passed as the argument.
 * Example: $tdiff(3663) returns "1 hour 1 minute 3 seconds"
 */
BUILT_IN_FUNCTION(function_tdiff, input)
{
	time_t	ltime;
	time_t	days,
		hours,
		minutes,
		seconds;
	char	tmp[80];
	char	*tstr;

	GET_INT_ARG(ltime, input);

	seconds = ltime % 60;
	ltime = (ltime - seconds) / 60;
	minutes = ltime % 60;
	ltime = (ltime - minutes) / 60;
	hours = ltime % 24;
	days = (ltime - hours) / 24;
	tstr = tmp;
	if (days)
	{
		sprintf(tstr, "%ld day%s ", days, plural(days));
		tstr += strlen(tstr);
	}
	if (hours)
	{
		sprintf(tstr, "%ld hour%s ", hours, plural(hours));
		tstr += strlen(tstr);
	}
	if (minutes)
	{
		sprintf(tstr, "%ld minute%s ", minutes, plural(minutes));
		tstr += strlen(tstr);
	}
	if (seconds || (!days && !hours && !minutes))
	{
		sprintf(tstr, "%ld second%s", seconds, plural(seconds));
		tstr += strlen(tstr);
	}
	else
		*--tstr = 0;

	RETURN_STR(tmp);
}

/*
 * Usage: $index(characters text)
 * Returns: The number of leading characters in <text> that do not occur 
 *          anywhere in the <characters> argument.
 * Example: $index(f three fine frogs) returns 6 (the 'f' in 'fine')
 *          $index(frg three fine frogs) returns 2 (the 'r' in 'three')
 */
BUILT_IN_FUNCTION(function_index, input)
{
	char	*schars;
	char	*iloc;

	GET_STR_ARG(schars, input);
	iloc = sindex(input, schars);
	RETURN_INT(iloc ? iloc - input : -1)
}

/*
 * Usage: $rindex(characters text)
 * Returns: The number of leading characters in <text> that occur before the
 *          *last* occurance of any of the characters in the <characters> 
 *          argument.
 * Example: $rindex(f three fine frogs) returns 12 (the 'f' in 'frogs')
 *          $rindex(frg three fine frogs) returns 15 (the 'g' in 'frogs')
 */
BUILT_IN_FUNCTION(function_rindex, word)
{
	char	*chars, *last;

	/* need to find out why ^x doesnt work */
	GET_STR_ARG(chars, word);
	last = rsindex(word + strlen(word) - 1, word, chars);
	RETURN_INT(last ? last - word : -1)
}

/*
 * Usage: $match(pattern list of words)
 * Returns: if no words in the list match the pattern, it returns 0.
 *	    Otherwise, it returns the number of the word that most
 *	    exactly matches the pattern (first word is numbered one)
 * Example: $match(f*bar foofum barfoo foobar) returns 3
 *	    $match(g*ant foofum barfoo foobar) returns 0
 *
 * Note: it is possible to embed spaces inside of a word or pattern simply
 *       by including the entire word or pattern in quotation marks. (")
 */
BUILT_IN_FUNCTION(function_match, input)
{
	char	*pattern, 	*word;
	long	current_match,	best_match = 0,	match = 0, match_index = 0;

	GET_STR_ARG(pattern, input);

	while (input && *input)
	{
		while (input && my_isspace(*input))
			input++;
		match_index++;
		GET_STR_ARG(word, input);
		if ((current_match = wild_match(pattern, word)) > best_match)
		{
			match = match_index;
			best_match = current_match;
		}
	}

	RETURN_INT(match)
}

/*
 * Usage: $rmatch(word list of patterns)
 * Returns: if no pattern in the list matches the word, it returns 0.
 *	    Otherwise, it returns the number of the pattern that most
 *	    exactly matches the word (first word is numbered one)
 * Example: $rmatch(foobar f*bar foo*ar g*ant) returns 2 
 *	    $rmatch(booya f*bar foo*ar g*ant) returns 0
 * 
 * Note: It is possible to embed spaces into a word or pattern simply by
 *       including the entire word or pattern within quotation marks (")
 */
BUILT_IN_FUNCTION(function_rmatch, input)
{
	char	*pattern,	*word;
	int	current_match,	best_match = 0,	match = 0, rmatch_index = 0;

	GET_STR_ARG(word, input);

	while (input && *input)
	{
		while (input && my_isspace(*input))
			input++;
		rmatch_index++;
		GET_STR_ARG(pattern, input);
		if ((current_match = wild_match(pattern, word)) > best_match)
		{
			match = rmatch_index;
			best_match = current_match;
		}
		/* WARNING WARNING HACK IN PROGRESS WARNING WARNING */
		while (input && my_isspace(*input))
			input++;
	}

	RETURN_INT(match)
}

/*
 * Usage: $userhost()
 * Returns: the userhost (if any) of the most previously recieved message.
 * Caveat: $userhost() changes with every single line that appears on
 *         your screen, so if you want to save it, you will need to assign
 *         it to a variable.
 */
BUILT_IN_FUNCTION(function_userhost, input)
{
	RETURN_STR(FromUserHost);
}

/* 
 * Usage: $strip(characters text)
 * Returns: <text> with all instances of any characters in the <characters>
 *	    argument removed.
 * Example: $strip(f free fine frogs) returns "ree ine rogs"
 *
 * Note: it can be difficult (actually, not possible) to remove spaces from
 *       a string using this function.  To remove spaces, simply use this:
 *		$tr(/ //$text)
 *
 *	 Actually, i recommend not using $strip() at all and just using
 *		$tr(/characters//$text)
 *	 (but then again, im biased. >;-)
 */
BUILT_IN_FUNCTION(function_strip, input)
{
	char	*result;
	char	*chars;
	char	*cp, *dp;

	GET_STR_ARG(chars, input);
	RETURN_IF_EMPTY(input);

	result = (char *)new_malloc(strlen(input) + 1);
	for (cp = input, dp = result; *cp; cp++)
	{
		/* This is expensive -- gotta be a better way */
		if (!index(chars, *cp))
			*dp++ = *cp;
	}
	*dp = '\0';

	return result;		/* DONT USE RETURN_STR HERE! */
}

/*
 * Usage: $encode(text)
 * Returns: a string, uniquely identified with <text> such that the string
 *          can be used as a variable name.
 * Example: $encode(fe fi fo fum) returns "GGGFCAGGGJCAGGGPCAGGHFGN"
 *
 * Note: $encode($decode(text)) returns text (most of the time)
 *       $decode($encode(text)) also returns text.
 */
static char * function_encode (unsigned char * input)
{
	char	*result;
	int	i = 0;

	result = (char *)new_malloc(strlen((char *)input) * 2 + 1);
	while (*input)
	{
		result[i++] = (*input >> 4) + 0x41;
		result[i++] = (*input & 0x0f) + 0x41;
		input++;
	}
	result[i] = '\0';

	return result;		/* DONT USE RETURN_STR HERE! */
}


/*
 * Usage: $decode(text)
 * Returns: If <text> was generated with $encode(), it returns the string
 *          you originally encoded.  If it wasnt, you will probably get
 *	    nothing useful in particular.
 * Example: $decode(GGGFCAGGGJCAGGGPCAGGHFGN) returns "fe fi fo fum"
 *
 * Note: $encode($decode(text)) returns "text"
 *       $decode($encode(text)) returns "text" too.
 *
 * Note: Yes.  $decode(plain-text) does compress the data by a factor of 2.
 *       But it ignores non-ascii text, so use this as compression at your
 *	 own risk and peril.
 */
static char *function_decode(unsigned char * input)
{
	char	*result;
	int	i = 0;

	result = (char *)new_malloc(strlen((char *)input) / 2 + 1);

	while (input[0] && input[1])
	{
		/* oops, this isnt quite right. */
		result[i] = ((input[0] - 0x41) << 4) | (input[1] - 0x41);
		input += 2;
		i++;
	}
	result[i] = '\0';

	return result;		/* DONT USE RETURN_STR HERE! */
}


/*
 * Usage: $ischannel(text)
 * Returns: If <text> could be a valid channel name, 1 is returned.
 *          If <text> is an illegal channel name, 0 is returned.
 *
 * Note: Contrary to popular belief, this function does NOT determine
 * whether a given channel name is in use!
 */
BUILT_IN_FUNCTION(function_ischannel, input)
{
	RETURN_INT(is_channel(input))
}

/*
 * Usage: $ischanop(nick channel)
 * Returns: 1 if <nick> is a channel operator on <channel>
 *          0 if <nick> is not a channel operator on <channel>
 *			* O R *
 *	      if you are not on <channel>
 *
 * Note: Contrary to popular belief, this function can only tell you
 *       who the channel operators are for channels you are already on!
 *
 * Boo Hiss:  This should be $ischanop(channel nick <nick...nick>)
 *	      and return a list (1 1 ... 0), which would allow us to
 *	      call is_chanop() without ripping off the nick, and allow 
 *	      us to abstract is_chanop() to take a list. oh well... 
 *	      Too late to change it now. :/
 */
BUILT_IN_FUNCTION(function_ischanop, input)
{
	char	*nick;

	GET_STR_ARG(nick, input);
	RETURN_INT(is_chanop(input, nick))
}


/*
 * Usage: $word(number text)
 * Returns: the <number>th word in <text>.  The first word is numbered zero.
 * Example: $word(3 one two three four five) returns "four" (think about it)
 */
BUILT_IN_FUNCTION(function_word, word)
{
	int	cvalue;
	char	*w_word;

	GET_INT_ARG(cvalue, word);
	if (cvalue < 0)
		RETURN_EMPTY;

	while (cvalue-- > 0)
		GET_STR_ARG(w_word, word);

	GET_STR_ARG(w_word, word);
	RETURN_STR(w_word);
}


/*
 * Usage: $winnum()
 * Returns: the index number for the current window
 * 
 * Note: returns -1 if there are no windows open (ie, in dumb mode)
 */
BUILT_IN_FUNCTION(function_winnum, input)
{
	RETURN_INT(curr_scr_win ? curr_scr_win->refnum : -1)
}

BUILT_IN_FUNCTION(function_winnam, input)
{
	if (curr_scr_win)
		RETURN_STR(curr_scr_win->name);
	else
		RETURN_EMPTY;
}

BUILT_IN_FUNCTION(function_connect, input)
{
	char	*host;
	int	port;

	GET_STR_ARG(host, input);
	GET_INT_ARG(port, input);

	return dcc_raw_connect(host, port);	/* DONT USE RETURN_STR HERE! */
}


BUILT_IN_FUNCTION(function_listen, input)
{
	int	port = 0;
	char	*result;

	/* Oops. found by CrowMan, listen() has a default. erf. */
	if (input && *input)
	{
		char *tmp, *ptr;
		if ((tmp = new_next_arg(input, &input)))
		{
			port = strtoul(tmp, &ptr, 10);
			if (ptr == tmp)
				RETURN_EMPTY;	/* error. */
		}
	}

	result = dcc_raw_listen(port);
	RETURN_STR(result);			/* DONT REMOVE RESULT! */
}

BUILT_IN_FUNCTION(function_toupper, input)
{
	return (upper(m_strdup(input)));
}

BUILT_IN_FUNCTION(function_tolower, input)
{
	return (lower(m_strdup(input)));
}

BUILT_IN_FUNCTION(function_curpos, input)
{
	RETURN_INT(current_screen->buffer_pos)
}

BUILT_IN_FUNCTION(function_channels, input)
{
	long	winnum;
	Window  *window = curr_scr_win;

	if (isdigit(*input))
	{
		GET_INT_ARG(winnum, input);
		window = get_window_by_refnum(winnum);
	}
	if (window->server == -1)
		RETURN_EMPTY;
	return create_channel_list(window);	/* DONT USE RETURN_STR HERE! */
}

BUILT_IN_FUNCTION(function_servers, input)
{
	return create_server_list();		/* DONT USE RETURN_STR HERE! */
}

BUILT_IN_FUNCTION(function_pid, input)
{
	RETURN_INT(getpid())
}

BUILT_IN_FUNCTION(function_ppid, input)
{
	RETURN_INT(getppid())
}

/*
 * strftime() patch from hari (markc@arbld.unimelb.edu.au)
 */
BUILT_IN_FUNCTION(function_strftime, input)
{
	char		result[128];
	time_t		ltime;
	struct tm	*tm;

	if (isdigit(*input))
		ltime = strtoul(input, &input, 10);
	else
		ltime = time(NULL);

	while (*input && my_isspace(*input))
		++input; 

	if (!*input)
		return m_strdup(empty_string);


	tm = localtime(&ltime);

	if (!strftime(result, 128, input, tm))
		return m_strdup(empty_string);

	return m_strdup(result);
}

BUILT_IN_FUNCTION(function_idle, input)
{
	return alias_idle();
}



/* The new "added" functions */

/* $before(chars string of text)
 * returns the part of "string of text" that occurs before the
 * first instance of any character in "chars"
 * EX:  $before(! hop!jnelson@iastate.edu) returns "hop"
 */
BUILT_IN_FUNCTION(function_before, word)
{
	char	*pointer = NULL;
	char	*chars;
	char	*tmp;
	long	numint;

	GET_STR_ARG(tmp, word);			/* DONT DELETE TMP! */
	numint = atol(tmp);

	if (numint)
	{
		GET_STR_ARG(chars, word);
	}
	else
	{
		numint = 1;
		chars = tmp;
	}

	if (numint < 0)
		pointer = word + strlen(word) - 1;

	pointer = search(word, &pointer, chars, numint);

	if (!pointer)
		RETURN_EMPTY;

	*pointer = '\0';
	RETURN_STR(word);
}

/* $after(chars string of text)
 * returns the part of "string of text" that occurs after the 
 * first instance of any character in "chars"
 * EX: $after(! hop!jnelson@iastate.edu)  returns "jnelson@iastate.edu"
 */
BUILT_IN_FUNCTION(function_after, word)
{
	char	*chars;
	char	*pointer = NULL;
	char 	*tmp;
	long	numint;

	GET_STR_ARG(tmp, word);
	numint = atol(tmp);

	if (numint)
		chars = new_next_arg(word, &word);
	else
	{
		numint = 1;
		chars = tmp;
	}

	if (numint < 0)
		pointer = word + strlen(word) - 1;

	pointer = search(word, &pointer, chars, numint);

	if (!pointer)
		RETURN_EMPTY;

	RETURN_STR(pointer + 1);
}

/* $leftw(num string of text)
 * returns the left "num" words in "string of text"
 * EX: $leftw(3 now is the time for) returns "now is the"
 */
BUILT_IN_FUNCTION(function_leftw, word)
{
	int value;
 
	GET_INT_ARG(value, word);
	if (value < 1)
		RETURN_EMPTY;

	return (extract(word, 0, value-1));	/* DONT USE RETURN_STR HERE! */
}

/* $rightw(num string of text)
 * returns the right num words in "string of text"
 * EX: $rightw(3 now is the time for) returns "the time for"
 */
BUILT_IN_FUNCTION(function_rightw, word)
{
	int     value;

	GET_INT_ARG(value, word);
	if (value < 1)
		RETURN_EMPTY;
		
	return extract2(word, -value, EOS); 
}


/* $midw(start num string of text)
 * returns "num" words starting at word "start" in the string "string of text"
 * NOTE: The first word is word #0.
 * EX: $midw(2 2 now is the time for) returns "the time"
 */
BUILT_IN_FUNCTION(function_midw, word)
{
	int     start, num;

	GET_INT_ARG(start, word);
	GET_INT_ARG(num, word);

	if (num < 1)
		RETURN_EMPTY;

	return extract(word, start, (start + num - 1));
}

/* $notw(num string of text)
 * returns "string of text" with word number "num" removed.
 * NOTE: The first word is numbered 0.
 * EX: $notw(3 now is the time for) returns "now is the for"
 */
BUILT_IN_FUNCTION(function_notw, word)
{
	char    *booya = NULL;
	int     where;
	
	GET_INT_ARG(where, word);

	/* An illegal word simply returns the string as-is */
	if (where < 0)
		RETURN_STR(word);

	if (where > 0)
	{
		char *part1, *part2;
		part1 = extract(word, 0, (where - 1));
		part2 = extract(word, (where + 1), EOS);

		booya = m_strdup(part1);
		m_s3cat_s(&booya, space, part2);
		new_free(&part1);
		new_free(&part2);
	}
	else /* where == 0 */
		booya = extract(word, 1, EOS);

	return booya;				/* DONT USE RETURN_STR HERE! */
}

/* $restw(num string of text)
 * returns "string of text" that occurs starting with and including
 * word number "num"
 * NOTE: the first word is numbered 0.
 * EX: $restw(3 now is the time for) returns "time for"
 */
static
#ifdef __STDC__
char	*function_restw(char *word)
#else
char    *function_restw(word)
char    *word;
#endif
{
	int     where;
	
	GET_INT_ARG(where, word);
	if (where < 0)
		RETURN_EMPTY;
	return extract(word, where, EOS);
}

/* $remw(word string of text)
 * returns "string of text" with the word "word" removed
 * EX: $remw(the now is the time for) returns "now is time for"
 */
static
#ifdef __STDC__
char	*function_remw (char *word)
#else
char    *function_remw(word)
char    *word;
#endif
{
	char    *booya = NULL;
	char    *blah = NULL;
	char    *lame = NULL, *plame;
	int	where;

	lame = m_strdup(word);
	plame = lame;
	blah = function_rmatch(word);
	where = atoi(blah);

	/* No matches, return the string as-is */
	if (where == 0)
	{
		new_next_arg(lame, &lame);
		booya = m_strdup(lame);
	}
	else
	{
		char *str1, *str2;
		str1 = extract(lame, 1, (where - 1));
		str2 = extract(lame, (where + 1), EOS);
		booya = m_strdup(str1);
		m_s3cat_s(&booya, space, str2);
		new_free(&str1);
		new_free(&str2);
	}
	new_free(&blah);
	new_free(&plame);
	return booya;				/* DONT USE RETURN_STR HERE! */
}

/* $insertw(num word string of text)
 * returns "string of text" such that "word" is the "num"th word
 * in the string.
 * NOTE: the first word is numbered 0.
 * EX: $insertw(3 foo now is the time for) returns "now is the foo time for"
 */
static
#ifdef __STDC__
char	*function_insertw (char *word)
#else
char    *function_insertw(word)
char    *word;
#endif
{
	int     where;
	char    *what;
	char    *booya= NULL;
	char 	*str1, *str2;

	GET_INT_ARG(where, word);
	
	/* If the word goes at the front of the string, then it
	   already is: return it. ;-) */
	if (where < 1)
		booya = m_strdup(word);
	else
	{
		GET_STR_ARG(what, word);
		str1 = extract(word, 0, (where - 1));
		str2 = extract(word, where, EOS);
		booya = m_strdup(str1);
		if (str1 && *str1)
			booya = m_2dup(str1, space);
		malloc_strcat(&booya, what);
		m_s3cat_s(&booya, space, str2);
		new_free(&str1);
		new_free(&str2);
	}

	return booya;				/* DONT USE RETURN_STR HERE! */
}

/* $chngw(num word string of text)
 * returns "string of text" such that the "num"th word is removed 
 * and replaced by "word"
 * NOTE: the first word is numbered 0
 * EX: $chngw(3 foo now is the time for) returns "now is the foo for"
 */
static
#ifdef __STDC__
char	*function_chngw (char *word)
#else
char    *function_chngw(word)
char    *word;
#endif
{
	int     which;
	char    *what;
	char    *booya= NULL;
	char	*str1, *str2;
	
	GET_INT_ARG(which, word);
	GET_STR_ARG(what, word);

	if (which < 0)
		RETURN_STR(word);

	/* hmmm. if which is 0, extract does the wrong thing. */
	str1 = extract(word, 0, which - 1);
	str2 = extract(word, which + 1, EOS);
	booya = m_strdup(str1);
	if (str1 && *str1)
		booya = m_2dup(str1, space);
	malloc_strcat(&booya, what);
	m_s3cat_s(&booya, space, str2);
	new_free(&str1);
	new_free(&str2);

	return (booya);
}


/* $common (string of text / string of text)
 * Given two sets of words seperated by a forward-slash '/', returns
 * all words that are found in both sets.
 * EX: $common(one two three / buckle my two shoe one) returns "one two"
 * NOTE: returned in order found in first string.
 */
static
#ifdef __STDC__
char	*function_common (char *word)
#else
char    *function_common(word)
char    *word;
#endif
{
	char    *left = NULL, **leftw = NULL,
		*right = NULL, **rightw = NULL,	*booya = NULL;
	int	leftc, lefti;
	int	rightc, righti;
	
	left = word;
	if ((right = index(word,'/')) == NULL)
		RETURN_EMPTY;

 	*right++ = 0;
	leftc = splitw(left, &leftw);
	rightc = splitw(right, &rightw);

	for (lefti = 0; lefti < leftc; lefti++)
	{
		for (righti = 0; righti < rightc; righti++)
		{
			if (rightw[righti] && !my_stricmp(leftw[lefti], rightw[righti]))
			{
				m_s3cat(&booya, space, leftw[lefti]);
				rightw[righti] = NULL;
			}
		}
	}
	new_free((char **)&leftw);
	new_free((char **)&rightw);
	if (!booya)
		RETURN_EMPTY;

	return (booya);				/* DONT USE RETURN_STR HERE! */
}

/* $diff(string of text / string of text)
 * given two sets of words, seperated by a forward-slash '/', returns
 * all words that are not found in both sets
 * EX: $diff(one two three / buckle my two shoe)
 * returns "one two three buckle my shoe"
 */
static
#ifdef __STDC__
char	*function_diff (char *word)
#else
char    *function_diff(word)
char    *word;
#endif
{
	char 	*left = NULL, **leftw = NULL,
	     	*right = NULL, **rightw = NULL, *booya = NULL;
	int 	lefti, leftc,
	    	righti, rightc;
	int 	found;

	left = word;
	if ((right = index(word,'/')) == NULL)
		RETURN_EMPTY;

	*right++ = 0;
	leftc = splitw(left, &leftw);
	rightc = splitw(right, &rightw);

	for (lefti = 0; lefti < leftc; lefti++)
	{
		found = 0;
		for (righti = 0; righti < rightc; righti++)
		{
			if (rightw[righti] && !my_stricmp(leftw[lefti], rightw[righti]))
			{
				found = 1;
				rightw[righti] = NULL;
			}
		}
		if (!found)
			m_s3cat(&booya, space, leftw[lefti]);
	}

	for (righti = 0; righti < rightc; righti++)
	{
		if (rightw[righti])
			m_s3cat(&booya, space, rightw[righti]);
	}

	new_free((char **)&leftw);
	new_free((char **)&rightw);

	if (!booya)
		RETURN_EMPTY;

	return (booya);
}

/* $pattern(pattern string of words)
 * given a pattern and a string of words, returns all words that
 * are matched by the pattern
 * EX: $pattern(f* one two three four five) returns "four five"
 */
static
#ifdef __STDC__
char	*function_pattern (char *word)
#else
char    *function_pattern(word)
char    *word;
#endif
{
	char    *blah;
	char    *booya = m_strdup(empty_string);
	char    *pattern;

	if ((pattern = new_next_arg(word, &word)))
	{
		while (((blah = new_next_arg(word, &word)) != NULL))
		{
			if (wild_match(pattern, blah))
				m_s3cat(&booya, space, blah);
		}
		return (booya);
	} 
	else 
		return m_strdup(empty_string);	
}

/* $filter(pattern string of words)
 * given a pattern and a string of words, returns all words that are 
 * NOT matched by the pattern
 * $filter(f* one two three four five) returns "one two three"
 */
static
#ifdef __STDC__
char	*function_filter (char *word)
#else
char    *function_filter(word)
char    *word;
#endif
{
	char    *blah;
	char    *booya = m_strdup(empty_string);
	char    *pattern;

	if ((pattern = new_next_arg(word, &word)))
	{
		while ((blah = new_next_arg(word, &word)) != NULL)
		{
			if (!wild_match(pattern, blah))
				m_s3cat(&booya, space, blah);
		}
		return (booya);
	} 
	else 
		return m_strdup(empty_string);
}

/* $rpattern(word list of patterns)
 * Given a word and a list of patterns, return all patterns that
 * match the word.
 * EX: $rpattern(jnelson@iastate.edu *@* jnelson@* f*@*.edu)
 * returns "*@* jnelson@*"
 */
static
#ifdef __STDC__
char	*function_rpattern (char *word)
#else
char    *function_rpattern(word)
char    *word;
#endif
{
	char    *blah;
	char    *booya = m_strdup(empty_string);
	char    *pattern;

	if ((blah = new_next_arg(word, &word)))
	{
		while ((pattern = new_next_arg(word, &word)) != NULL)
		{
			if (wild_match(pattern, blah))
				m_s3cat(&booya, space, pattern);
		}
		return (booya);
	} 
	else 
		return m_strdup(empty_string);
}

/* $rfilter(word list of patterns)
 * given a word and a list of patterns, return all patterns that
 * do NOT match the word
 * EX: $rfilter(jnelson@iastate.edu *@* jnelson@* f*@*.edu)
 * returns "f*@*.edu"
 */
static
#ifdef __STDC__
char	*function_rfilter (char *word)
#else
char    *function_rfilter(word)
char    *word;
#endif
{
	char    *blah;
	char    *booya = m_strdup(empty_string);
	char    *pattern;

	if ((blah = new_next_arg(word, &word)))
	{
		while ((pattern = new_next_arg(word, &word)) != NULL)
		{
			if (!wild_match(pattern, blah))
				m_s3cat(&booya, space, pattern);
		}
		return (booya);
	} 
	else 
		return m_strdup(empty_string);
}

/* $copattern(pattern var_1 var_2)
 * Given a pattern and two variable names, it returns all words
 * in the variable_2 corresponding to any words in variable_1 that
 * are matched by the pattern
 * EX: @nicks = [hop IRSMan skip]
 *     @userh = [jnelson@iastate.edu irsman@iastate.edu sanders@rush.cc.edu]
 *     $copattern(*@iastate.edu userh nicks) 
 *	returns "hop IRSMan"
 */
static
#ifdef __STDC__
char	*function_copattern (char *word)
#else
char    *function_copattern(word)
char    *word;
#endif
{
	char	*booya = NULL,
		*pattern = NULL,
		*firstl = NULL, *firstlist = NULL, *firstel = NULL,
		*secondl = NULL, *secondlist = NULL, *secondel = NULL;
	char 	*sfirstl, *ssecondl;

	GET_STR_ARG(pattern, word);
	GET_STR_ARG(firstlist, word);
	GET_STR_ARG(secondlist, word);

	firstl = find_inline(firstlist);
	secondl = find_inline(secondlist);
	sfirstl = firstl;
	ssecondl = secondl;

	while ((firstel = new_next_arg(firstl, &firstl)))
	{
		if (!(secondel = new_next_arg(secondl, &secondl)))
		{
			(void) (booya || (booya = m_strdup(empty_string)));
			break;
		}

		if (wild_match(pattern, firstel))
			m_s3cat(&booya, space, secondel);
	}
	new_free(&sfirstl);
	new_free(&ssecondl);

	if (!booya) 
		RETURN_EMPTY;

	return (booya);
}

/* $beforew(pattern string of words)
 * returns the portion of "string of words" that occurs before the 
 * first word that is matched by "pattern"
 * EX: $beforew(three one two three o leary) returns "one two"
 */
static
#ifdef __STDC__
char	*function_beforew (char *word)
#else
char    *function_beforew(word)
char    *word;
#endif
{
	int     where;
	char	*lame = NULL;
	char	*placeholder;

	lame = m_strdup(word);
	placeholder = function_rmatch(word);
	where = atoi(placeholder);

	new_free(&placeholder);

	if (where < 1)
	{
		new_free(&lame);
		RETURN_EMPTY;
	}
	placeholder = extract(lame, 1, where - 1);
	new_free(&lame);
	return placeholder;
}
		
/* Same as above, but includes the word being matched */
static
#ifdef __STDC__
char	*function_tow (char *word)
#else
char    *function_tow(word)
char    *word;
#endif
{
	int     where;
	char	*lame = NULL;
	char	*placeholder;

	lame = m_strdup(word);
	placeholder = function_rmatch(word);
	where = atoi(placeholder);

	new_free(&placeholder);

	if (where < 1)
	{
		new_free(&lame);
		RETURN_EMPTY;
	}
	placeholder = extract(lame, 1, where);
	new_free(&lame);
	return placeholder;
}

/* Returns the string after the word being matched */
static
#ifdef __STDC__
char	*function_afterw (char *word)
#else
char    *function_afterw(word)
char    *word;
#endif
{
	int     where;
	char	*lame = NULL;
	char	*placeholder;

	lame = m_strdup(word);
	placeholder = function_rmatch(word);
	where = atoi(placeholder);

	new_free(&placeholder);

	if (where < 1)
	{
		new_free(&lame);
		RETURN_EMPTY;
	}
	placeholder = extract(lame, where + 1, EOS);
	new_free(&lame);
	return placeholder;
}

/* Returns the string starting with the word being matched */
static
#ifdef __STDC__
char	*function_fromw (char *word)
#else
char    *function_fromw(word)
char    *word;
#endif
{
	int     where;
	char 	*lame = NULL;
	char	*placeholder;

	lame = m_strdup(word);
	placeholder = function_rmatch(word);
	where = atoi(placeholder);

	new_free(&placeholder);

	if (where < 1)
	{
		new_free(&lame);
		RETURN_EMPTY;
	}

	placeholder = extract(lame, where, EOS);
	new_free(&lame);
	return placeholder;
}

/* Cut and paste a string */
static
#ifdef __STDC__
char	*function_splice (char *word)
#else
char    *function_splice(word)
char    *word;
#endif
{
	char    *variable;
	int	start;
	int	length;
	char 	*left_part = NULL;
	char	*middle_part = NULL;
	char	*right_part = NULL;
	char	*old_value = NULL;
	char	*new_value = NULL;
	int 	old_display = window_display;
	int	num_words;

	GET_STR_ARG(variable, word);
	GET_INT_ARG(start, word);
	GET_INT_ARG(length, word);

	old_value = find_inline(variable);
	num_words = word_count(old_value);

	if (start < 0)
	{
		if ((length += start) <= 0)
			RETURN_EMPTY;
		start = 0;
	}

	if (start >= num_words)
	{
		left_part = m_strdup(old_value);
		middle_part = m_strdup(empty_string);
		right_part = m_strdup(empty_string);
	}

	else if (start + length >= num_words)
	{
		left_part = extract(old_value, 0, start - 1);
		middle_part = extract(old_value, start, EOS);
		right_part = m_strdup(empty_string);
	}

	else
	{
		left_part = extract(old_value, 0, start - 1);
		middle_part = extract(old_value, start, start + length - 1);
		right_part = extract(old_value, start + length, EOS);
	}

	new_value = NULL;
	malloc_strcpy(&new_value, left_part);
	if (new_value && *new_value && word && *word)
		malloc_strcat(&new_value, space);
	if (word && *word)
		malloc_strcat(&new_value, word);
	if (new_value && *new_value && *right_part && *right_part)
		malloc_strcat(&new_value, space);
	if (right_part && *right_part)
		malloc_strcat(&new_value, right_part);

	window_display = 0;
	add_alias(VAR_ALIAS, variable, new_value);
	window_display = old_display;

	new_free(&old_value);
	new_free(&new_value);
	new_free(&left_part);
	new_free(&right_part);
	return middle_part;
}
	
static
#ifdef __STDC__
char	*function_numonchannel (char * word)
#else
char    *function_numonchannel(word)
char    *word;
#endif
{
	char            channel[255];
	ChannelList	*chan = NULL;
	NickList	*tmp = NULL;
	int             counter = 0;
	
	if (word && *word)
		strmcpy(channel, new_next_arg(word, &word), 254);
	else
		strmcpy(channel, empty_string, 254);

	if ((channel == NULL) || (!is_channel(channel)) ||
	    (!(chan = lookup_channel(channel, curr_scr_win->server,0))))
		RETURN_INT(0)

	for (tmp = chan->nicks; tmp; tmp = tmp->next)
		counter++;

	RETURN_INT(counter)
}
	
static
#ifdef __STDC__
char	*function_onchannel (char *word)
#else
char    *function_onchannel(word)
char    *word;
#endif
{
	char		channel[255];	/* max channel name size */
	ChannelList     *chan = NULL;
	NickList        *tmp = NULL;
	char            *nicks = NULL;
	char	*curr_chan;

	if (word && *word)
		strmcpy(channel, new_next_arg(word, &word), 254);
	else
		strmcpy(channel, empty_string, 254);

	curr_chan = get_channel_by_refnum(0);
	if (!curr_chan)
		curr_chan = empty_string;

	if ((*channel == 0) || (!strcmp(channel, "*")))
		strmcpy(channel, curr_chan, 254);

	if (*channel == 0 || *channel == '0')
		RETURN_EMPTY;

	if (!is_channel(channel))   /* must be a new onchannel */
	{
		char nick[255];

		strmcpy(nick, channel, 254);
		if (word && *word)
			strmcpy(channel, new_next_arg(word, &word), 254);
		else
			strmcpy(channel, curr_chan, 254);

		if (*channel)
			RETURN_INT(is_on_channel(channel, from_server, nick) ? 1 : 0)
		else
			RETURN_INT(0)
	}

	chan = lookup_channel(channel,curr_scr_win->server,0);
	if (chan == NULL)
		RETURN_EMPTY;

	for (tmp = chan->nicks; tmp; tmp = tmp->next)
	{
		if (nicks)
		    malloc_strcat(&nicks, space);
		malloc_strcat(&nicks, tmp->nick);
	}
	if (!nicks)
		malloc_strcpy(&nicks, empty_string);
	return(nicks);
}

static
#ifdef __STDC__
char	*function_channelnicks (char *word)
#else
char    *function_channelnicks(word)
char    *word;
#endif
{
	char		channel[255];	/* max channel name size */
	ChannelList     *chan = NULL;
	NickList        *tmp = NULL;
	char            *nicks = NULL;
	char	*curr_chan;

	if (word && *word)
		strmcpy(channel, new_next_arg(word, &word), 254);
	else
		strmcpy(channel, empty_string, 254);

	curr_chan = get_channel_by_refnum(0);
	if (!curr_chan)
		curr_chan = empty_string;

	if ((*channel == 0) || (!strcmp(channel, "*")))
		strmcpy(channel, curr_chan, 254);

	if (*channel == 0 || *channel == '0')
		RETURN_EMPTY;

	if (!is_channel(channel))   /* must be a new onchannel */
	{
		char nick[255];

		strmcpy(nick, channel, 254);
		if (word && *word)
			strmcpy(channel, new_next_arg(word, &word), 254);
		else
			strmcpy(channel, curr_chan, 254);

		if (*channel)
			RETURN_INT(is_on_channel(channel, from_server, nick) ? 1 : 0)
		else
			RETURN_INT(0)
	}

	chan = lookup_channel(channel,curr_scr_win->server,0);
	if (chan == NULL)
		RETURN_EMPTY;

	for (tmp = chan->nicks; tmp; tmp = tmp->next)
	{
		if (nicks)
		    malloc_strcat(&nicks, ",");
		malloc_strcat(&nicks, tmp->nick);
	}
	if (!nicks)
		malloc_strcpy(&nicks, empty_string);
	return(nicks);
}

static
#ifdef __STDC__
char    *function_chops(char *word)
#else
char    *function_chops(word)
char    *word;
#endif
{
	char		channel[255];	/* maximum size of channel name */
	ChannelList     *chan;
	NickList        *tmp;
	char            *nicks = NULL;
	char 	*curr_chan;

	if (word && *word)
		strmcpy(channel, new_next_arg(word, &word), 254);
	else
		strmcpy(channel, empty_string, 254);

	curr_chan = get_channel_by_refnum (0);
	if (!curr_chan)
		curr_chan = empty_string;

	if ((*channel == 0) || (!strcmp(channel, "*")))
		strmcpy(channel, curr_chan, 254);

	if (*channel == 0 || *channel == '0')
		RETURN_EMPTY;

	chan = lookup_channel(channel,curr_scr_win->server,0);
	if (!chan)
		RETURN_EMPTY;

	for (tmp = chan->nicks; tmp; tmp = tmp->next)
	{
		if (tmp->chanop == 1) 
			m_s3cat(&nicks, space, tmp->nick);
	}
	if (!nicks)
		RETURN_EMPTY;

	return(nicks);
}        

static
#ifdef __STDC__
char    *function_nochops(char *word)
#else
char    *function_nochops(word)
char    *word;
#endif
{
	ChannelList     *chan;
	NickList        *tmp;
	char            *nicks = NULL;
	char            channel[255];
	char	*curr_chan;

	if (word && *word)
		strmcpy(channel, new_next_arg(word, &word), 254);
	else
		strmcpy(channel, empty_string, 254);

	curr_chan = get_channel_by_refnum(0);
	if (!curr_chan)
		curr_chan = empty_string;

	if ((*channel == 0) || (!strcmp(channel, "*")))
		strmcpy(channel, curr_chan, 254);

	if (*channel == 0 || *channel == '0')
		RETURN_EMPTY;

	chan = lookup_channel(channel,curr_scr_win->server,0);
	if (!chan)
		RETURN_EMPTY;

	for (tmp = chan->nicks; tmp; tmp = tmp->next)
	{
		if (tmp->chanop == 0) 
			m_s3cat(&nicks, space, tmp->nick);
	}
	if (!nicks)
		RETURN_EMPTY;

	return(nicks);
}

static
#ifdef __STDC__
char    *function_key(char *word)
#else
char    *function_key(word)
char    *word;
#endif
{
	char	channel[257];
	char    *booya = NULL;
	int debug = 0;
	char *curr_chan;

	if (word && *word)
		strmcpy(channel, new_next_arg(word, &word), 254);
	else
		strmcpy(channel, empty_string, 254);

	curr_chan = get_channel_by_refnum(0);
	if (!curr_chan)
		curr_chan = empty_string;

	for (;;)
	{
		char    *key;

		if (debug++ > 100)
			ircpanic("function_key: infinite loop");

		/* If we are at the end of the list */
		if (!*channel)
		{
			/* If there were other channels, stop */
			if (booya)
				break;

			/* Empty list -- use current channel */
			else
				strmcpy(channel, curr_chan, 256);
		}

		/* Not at end of list, look to see if channel is "*" */
		else if (!strcmp(channel, "*"))
			strmcpy(channel, curr_chan, 256);

		/* If there is still no channel, stop */
		if (!*channel)
			return (booya ? booya : m_strdup(empty_string));

		if ((key = channel_key(channel)))
			m_s3cat(&booya, space, (key || *key) ? key : "*");

		if (!word || !*word)
			break;

		strmcpy(channel, new_next_arg(word, &word), 256);
	}

	if (!booya)
		RETURN_EMPTY;

	return (booya);
}

static
#ifdef __STDC__
char    *function_revw (char *words)
#else
char    *function_revw (words)
char    *words;
#endif
{
	char	*booya = NULL;

	while (words && *words)
		m_s3cat(&booya, space, last_arg(&words));
	if (!booya)
		RETURN_EMPTY;

	return booya;
}

static
#ifdef __STDC__
char    *function_reverse(char *words)
#else
char    *function_reverse(words)
char    *words;
#endif
{
	int     length = strlen(words);
	char    *booya = NULL;
	int     x = 0;

	booya = (char *)new_malloc(length+1);
	for(length--; length >= 0; length--,x++)
		booya[x] = words[length];
	booya[x]='\0';
	return (booya);
}

static
#ifdef __STDC__
char    *function_jot(char *input)
#else
char    *function_jot(input)
char    *input;
#endif
{
	int     start = 0;
	int     stop = 0;
	int     interval = 1;
	int     counter;
	char    *booya = NULL;

        GET_INT_ARG(start,input)
        GET_INT_ARG(stop, input)
        if (input && *input)
                GET_INT_ARG(interval, input)
        else
                interval = 1;

        if (interval < 0) 
                interval = -interval;

        if (start < stop)
                for (counter = start;counter <= stop;counter+=interval)
			m_s3cat(&booya, space, ltoa(counter));
        else
                for (counter = start;counter >= stop;counter-=interval)
			m_s3cat(&booya, space, ltoa(counter));


        return booya;
}

#ifdef __STDC__
extern char    *function_shift(char *word)
#else
extern char    *function_shift(word)
char	*word;
#endif
{
	char    *value = NULL;
	char    *var    = NULL;
	char	*booya 	= NULL;
	char	*blah;
	int     old_display = window_display;
	char    *placeholder;

	GET_STR_ARG(var, word);

	if (word && *word)
	{
		malloc_strcpy(&value, var);
		malloc_strcat(&value, word);
		var = NULL;
	}
	else
		value = find_inline(var);

	if (!value && !*value)
		RETURN_EMPTY;

	placeholder = value;
	blah = new_next_arg(value, &value);
	malloc_strcpy(&booya, blah);
	if (var)
	{
		window_display = 0;
		add_alias(VAR_ALIAS, var, value);
		window_display = old_display;
	}
	new_free(&placeholder);
	if (!booya)
		RETURN_EMPTY;
	return booya;
}

#ifdef __STDC__
extern char    *function_unshift(char *word)
#else
extern char    *function_unshift(word)
char	*word;
#endif
{
	char    *value = NULL;
	char    *var    = NULL;
	char	*booya  = NULL;
	int     old_display = window_display;

	GET_STR_ARG(var, word);
	value = find_inline(var);
	if (!word || !*word)
		return value;

	booya = m_strdup(word);
	m_s3cat_s(&booya, space, value);

	window_display = 0;
	add_alias(VAR_ALIAS, var, booya);
	window_display = old_display;
	new_free(&value);
	return booya;
}

#ifdef __STDC__
extern char    *function_push(char *word)
#else
extern char    *function_push(word)
char	*word;
#endif
{
	char    *value = NULL;
	char    *var    = NULL;
	int     old_display = window_display;

	GET_STR_ARG(var, word);
	value = find_inline(var);
	m_s3cat(&value, space, word);
	window_display = 0;
	add_alias(VAR_ALIAS, var, value);
	window_display = old_display;
	/* oops. this was a memory leak since value is already malloced */
	return value;
}

#ifdef __STDC__
extern char	*function_pop(char *word)
#else
extern char	*function_pop(word)
char	*word;
#endif
{
	char *value	= NULL;
	char *var	= NULL;
	char *pointer	= NULL;
	int   old_display = window_display;
	char *blech     = NULL;

	GET_STR_ARG(var, word);

	if (word && *word)
	{
		pointer = rindex(word, ' ');
		RETURN_STR(pointer ? pointer : word);
	}

	value = find_inline(var);
	if (!value || !*value)
	{
		new_free(&value);
		RETURN_EMPTY;
	}

	if (!(pointer = rindex(value, ' ')))
	{
		window_display = 0;
		add_alias(VAR_ALIAS, var, empty_string); /* dont forget this! */
		window_display = old_display;
		return value;	/* one word -- return it */
	}

	*pointer++ = '\0';
	window_display = 0;
	add_alias(VAR_ALIAS, var, value);
	window_display = old_display;

	/* because pointer points to value, we *must* make a copy of it
	 * *before* we free value! (And we cant forget to free value, either) */
	blech = m_strdup(pointer);
	new_free(&value);
	return blech;
}


/* Search and replace function --
   Usage:   $sar(c/search/replace/data)
   Commands:
		r - treat data as a variable name and 
		    return the replaced data to the variable
		g - Replace all instances, not just the first one
   The delimiter may be any character that is not a command (typically /)
   The delimiter MUST be the first character after the command
   Returns empty string on error
*/
static
#ifdef __STDC__
char    *function_sar(char *word)
#else
char    *function_sar(word)
char    *word;
#endif
{
	char    delimiter;
	char	*pointer	= NULL;
	char    *search         = NULL;
	char    *replace        = NULL;
	char    *data		= NULL;
	char	*value		= NULL;
	char	*booya		= NULL;
	int	variable = 0,global = 0,searchlen,oldwindow = window_display;
	char 	*svalue;

	while (((*word == 'r') && (variable = 1)) || ((*word == 'g') && (global = 1)))
		word++;

	RETURN_IF_EMPTY(word);

	delimiter = *word;
	search = word + 1;
	if ((replace = index(search, delimiter)) == 0)
		RETURN_EMPTY;

	*replace++ = 0;
	if ((data = index(replace,delimiter)) == 0)
		RETURN_EMPTY;

	*data++ = '\0';

	value = (variable == 1) ? find_inline(data) : m_strdup(data);

	if (!value || !*value)
	{
		if (value)
			new_free(&value);
		RETURN_EMPTY;
	}
	svalue = value;

	pointer = value;
	searchlen = strlen(search) - 1;

	if (global)
	{
		while ((pointer = strstr(pointer,search)) != NULL)
		{
			pointer[0] = pointer[searchlen] = 0;
			pointer += searchlen + 1;
			m_e3cat(&booya, value, replace);
			value = pointer;
		}
	} 
	else
	{
		if ((pointer = strstr(pointer,search)) != NULL)
		{
			pointer[0] = pointer[searchlen] = 0;
			pointer += searchlen + 1;
			m_e3cat(&booya, value, replace);
			value = pointer;
		}
	}

	malloc_strcat(&booya, value);
	if (variable) 
	{
		window_display = 0;
		add_alias(VAR_ALIAS, data, booya);
		window_display = oldwindow;
	}
	new_free(&svalue);
	return (booya);
}

static
#ifdef __STDC__
char    *function_center(char *word)
#else
char    *function_center(word)
char    *word;
#endif
{
	int	length,pad,width;
	char 	*padc;

	if (!word || !*word)
		RETURN_EMPTY;

	width = atoi(new_next_arg(word, &word));
	RETURN_IF_EMPTY(word);
	length = strlen(word);
	
	if ((pad = width - length) < 0)
		RETURN_STR(word);

	pad /= 2;
	padc = (char *)new_malloc(width+1);
	memset(padc, ' ', pad);
	padc[pad] = '\0';

	return strcat(padc, word);
}

static
#ifdef __STDC__
char    *function_split(char *word)
#else
char    *function_split(word)
char    *word;
#endif
{
	char	*chrs;
	char	*pointer;

	chrs = next_arg(word, &word);
	pointer = word;
	while ((pointer = sindex(pointer,chrs)))
		*pointer++ = ' ';

	RETURN_STR(word);
}

static
#ifdef __STDC__
char *function_chr(char *word)
#else
char *function_chr(word)
char *word;
#endif
{
	char aboo[BIG_BUFFER_SIZE];
	char *ack = aboo;
	char *blah;

	while ((blah = next_arg(word, &word)))
		*ack++ = (char)atoi(blah);

	*ack = '\0';
	RETURN_STR(aboo);
}

static
#ifdef __STDC__
char *function_ascii(char *word)
#else
char *function_ascii(word)
char *word;
#endif
{
	char *aboo = NULL;

	if (!word || !*word)
		RETURN_EMPTY;

	aboo = m_strdup(ltoa((long) *word));
	while (*++word)
		m_3cat(&aboo, space, ltoa((long) *word));
		
	return (aboo);
}

static
#ifdef __STDC__
char	*function_which(char *word)
#else
char	*function_which(word)
char	*word;
#endif
{
	char *file1;

	GET_STR_ARG(file1, word);

	if ((file1 = path_search(file1, (word && *word) ? word :
		get_string_var(LOAD_PATH_VAR))))
	{
		RETURN_STR(file1);
	}
	else
	{
		new_free(&file1);
		RETURN_EMPTY;
	}
}


static
#ifdef __STDC__
char *function_isalpha(char *words)
#else
char *function_isalpha(words)
char *words;
#endif
{
	if (((*words >= 'a') && (*words <= 'z')) ||
	    ((*words >= 'A') && (*words <= 'Z')))
		RETURN_INT(1)
	else
		RETURN_INT(0)
}

static
#ifdef __STDC__
char *function_isdigit(char *words)
#else
char *function_isdigit(words)
char *words;
#endif
{
	if (((*words >= '0') && (*words <= '9')) ||
	    ((*words == '-') && ((*(words+1) >= '0') || (*(words+1) <= '9'))))
		RETURN_INT(1)
	else
		RETURN_INT(0)
}

static
#ifdef __STDC__
char *function_isnum(char *words)
#else
char *function_isnum(words)
char *words;
#endif
{
int num = 0;
char *p = words;
	while (*p)
	{
		if (((*p >= '0') && (*p <= '9')) || (*p == '#') ||
		    ((*p == '-') && ((*(p+1) >= '0') || (*(p+1) <= '9'))))
			num = 1;
		else
		{
			num = 0;
			break;
		}
		p++;
	}
	RETURN_INT(num);
}

static
#ifdef __STDC__
char *function_open (char *words)
#else
char *function_open (words)
char *words;
#endif
{
	char *filename;
	char *mode;
	char *bin_mode = NULL;
	
	GET_STR_ARG(filename, words);
	GET_STR_ARG(mode, words);
	if (in_cparse)
		RETURN_EMPTY;
	if (words && *words)
		bin_mode = words;
		
	if (*mode == 'R')
	{
		if (bin_mode && *bin_mode == 'B')
			RETURN_INT(open_file_for_read(filename))
		else
			RETURN_INT(open_file_for_read(filename))
	}
	else if (*mode == 'W')
	{
		if (bin_mode && *bin_mode == 'B')
			RETURN_INT(open_file_for_bwrite(filename))
		else
			RETURN_INT(open_file_for_write(filename))
	}
	RETURN_EMPTY;	
}

static
#ifdef __STDC__
char *function_close (char *words)
#else
char *function_close (words)
char *words;
#endif
{
	RETURN_IF_EMPTY(words);
	if (in_cparse)
		RETURN_INT(0);
	RETURN_INT(file_close(atoi(new_next_arg(words, &words))))
}	

static
#ifdef __STDC__
char *function_write (char *words)
#else
char *function_write (words)
char *words;
#endif
{
	char *fdc;
	if (in_cparse)
		RETURN_INT(0);
	GET_STR_ARG(fdc, words);
	RETURN_INT(file_write(atoi(fdc), words))
}

static
#ifdef __STDC__
char *function_writeb (char *words)
#else
char *function_writeb (words)
char *words;
#endif
{
	char *fdc;
	if (in_cparse)
		RETURN_INT(0);
	GET_STR_ARG(fdc, words);
	RETURN_INT(file_writeb(atoi(fdc), words))
}

static
#ifdef __STDC__
char *function_read (char *words)
#else
char *function_read (words)
char *words;
#endif
{
	char *fdc = NULL, *numb = NULL;
	if (in_cparse)
		RETURN_INT(0);

	GET_STR_ARG(fdc, words);
	if (words && *words)
		GET_STR_ARG(numb, words);

	if (numb)
		return file_readb (atoi(fdc), atoi(numb));
	else
		return file_read (atoi(fdc));
}

static
#ifdef __STDC__
char *function_eof (char *words)
#else
char *function_eof (words)
char *words;
#endif
{
	RETURN_IF_EMPTY(words);
	RETURN_INT(file_eof(atoi(new_next_arg(words, &words))))
}


static
#ifdef __STDC__
char *function_iptoname (char *words)
#else
char *function_iptoname (words)
char *words;
#endif
{
	RETURN_STR(ip_to_host(words));
}

static
#ifdef __STDC__
char *function_nametoip (char *words)
#else
char *function_nametoip (words)
char *words;
#endif
{
	RETURN_STR(host_to_ip(words));
}

static
#ifdef __STDC__
char *function_convert (char *words)
#else
char *function_convert (words)
char *words;
#endif
{
	RETURN_STR(one_to_another(words));
}

static
#ifdef __STDC__
char *function_translate (char *words)
#else
char *function_translate (words)
char *words;
#endif
{
register char *	oldc;
	char *	newc, 
	     *	text,
		delim;
register char *	ptr;
	int 	size_old, 
		size_new,
		x;

	RETURN_IF_EMPTY(words);

	oldc = words;
	/* First character can be a slash.  If it is, we just skip over it */
	delim = *oldc++;
	newc = strchr(oldc, delim);

	if (!newc)
		RETURN_EMPTY;	/* no text in, no text out */

	text = strchr(++newc, delim);

	if (newc == oldc)
		RETURN_EMPTY;

	if (!text)
		RETURN_EMPTY;
	*text++ = '\0';

	if (newc == text)
	{
		*newc = '\0';
		newc = empty_string;
	}
	else
		newc[-1] = 0;

	/* this is cheating, but oh well, >;-) */
	text = m_strdup(text);

	size_new = strlen(newc);
	size_old = strlen(oldc);

	for (ptr = text; ptr && *ptr; ptr++)
	{
		for (x = 0;x < size_old;(void)x++)
		{
			if (*ptr == oldc[x])
			{
				/* Check to make sure we arent
				   just eliminating the character.
				   If we arent, put in the new char,
				   otherwise strcpy it away */
				if (size_new)
					*ptr = newc[(x<size_new)?x:size_new-1];
				else
				{
					strcpy (ptr, ptr+1);
					ptr--;				
				}
				break;
			}
		}
	}
	return text;
}

static
#ifdef __STDC__
char *function_server_version (char *word)
#else
char *function_server_version (word)
char *word;
#endif
{
	int servnum;
	int version;

	servnum = ((word && *word) ? atoi(next_arg(word, &word)) : primary_server);

	if (servnum > number_of_servers)
		RETURN_STR("unknown");

	version = get_server_version(servnum);

	if (version == Server2_8) 		RETURN_STR("2.8");
	else if (version == Server_u2_8) 	RETURN_STR("u2.8");
	else if (version == Server_u2_9) 	RETURN_STR("u2.9");
	else if (version == Server_u2_10)	RETURN_STR("u2.10");
	else if (version == Server2_9) 		RETURN_STR("2.9");
	else if (version == Server_u3_0)	RETURN_STR("u3.0");

	RETURN_STR("Unknown");
}

static
#ifdef __STDC__
char *function_unlink(char *words)
#else
char *function_unlink (words)
char *words;
#endif
{
	char *	expanded;
	int 	failure = 0;
	if (in_cparse)
		RETURN_INT(0);
	while (words && *words)
	{
		expanded = expand_twiddle(new_next_arg(words, &words));
		failure -= unlink(expanded);	
		new_free(&expanded);
	}
	RETURN_INT(failure)
}

static
#ifdef __STDC__
char *function_rename (char *words)
#else
char *function_rename (words)
char *words;
#endif
{
	char *	filename1, 
	     *	filename2;
	char *expanded1, *expanded2;
	int 	failure = 0;
	if (in_cparse)
		RETURN_INT(failure);
	GET_STR_ARG(filename1, words);
	GET_STR_ARG(filename2, words);
	expanded1 = expand_twiddle(filename1);
	expanded2 = expand_twiddle(filename2);
	failure = rename(expanded1, expanded2);
	new_free(&expanded1); new_free(&expanded2);
	RETURN_INT(failure)
}

#ifdef __STDC__
static char *function_rmdir (char *words)
#else
static char *function_rmdir (words)
char *words;
#endif
{
	int 	failure = 0;
	char	*expanded;
	
	if (in_cparse)
		RETURN_INT(failure);
	while (words && *words)
	{
		expanded = expand_twiddle(new_next_arg(words, &words));
		failure -= rmdir(expanded);
		new_free(&expanded);
	}
	
	RETURN_INT(failure)
}

#ifdef __STDC__
static char *function_truncate (char *words)
#else
static char *function_truncate (words)
char *words;
#endif
{
	int		num = 0;
	double		value = 0;
	char		buffer[BIG_BUFFER_SIZE],
			format[1024];

	GET_INT_ARG(num, words);
	GET_FLOAT_ARG(value, words);

	if (num < 0)
	{
		float foo;
		int end;

		sprintf(format, "%%.%de", -num-1);
		sprintf(buffer, format, value);
		foo = atof(buffer);
		sprintf(buffer, "%f", foo);
		end = strlen(buffer) - 1;
		if (end == 0)
			RETURN_EMPTY;
		while (buffer[end] == '0')
			end--;
		if (buffer[end] == '.')
			end--;
		buffer[end+1] = 0;
	}
	else if (num > 0)
	{
		sprintf(format, "%%10.%dlf", num);
		sprintf(buffer, format, value);
	}
	else
		RETURN_EMPTY;

	while (*buffer == ' ')
		strcpy(buffer, buffer+1);

	RETURN_STR(buffer);
}


/*
 * Apprantly, this was lifted from a CS client.  I reserve the right
 * to replace this code in future versions. (hop)
 */
/*
	I added this little function so that I can have stuff formatted
	into days, hours, minutes, seconds; but with d, h, m, s abreviations.
		-Taner
*/

BUILT_IN_FUNCTION(function_tdiff2, input)
{
	time_t	ltime;
	time_t	days,
		hours,
		minutes,
		seconds;
	char	tmp[80];
	char	*tstr;

	GET_INT_ARG(ltime, input);

	seconds = ltime % 60;
	ltime = (ltime - seconds) / 60;
	minutes = ltime%60;
	ltime = (ltime - minutes) / 60;
	hours = ltime % 24;
	days = (ltime - hours) / 24;
	tstr = tmp;

	if (days)
	{
		sprintf(tstr, "%ldd ", days);
		tstr += strlen(tstr);
	}
	if (hours)
	{
		sprintf(tstr, "%ldh ", hours);
		tstr += strlen(tstr);
	}
	if (minutes)
	{
		sprintf(tstr, "%ldm ", minutes);
		tstr += strlen(tstr);
	}
	if (seconds || (!days && !hours && !minutes))
	{
		sprintf(tstr, "%lds", seconds);
		tstr += strlen(tstr);
	}
	else
		*--tstr = 0;	/* chop off that space! */

	RETURN_STR(tmp);
}


/* 
 * Apparantly, this was lifted from a CS client.  I reserve the right 
 * to replace this code in a future release.
 */
BUILT_IN_FUNCTION(function_utime, input)
{
	struct  timeval         tp;
	if (gettimeofday(&tp, NULL))
		RETURN_EMPTY;
	return ( m_sprintf("%ld %ld",tp.tv_sec, tp.tv_usec) );
}


/*
 * This inverts any ansi sequence present in the string
 * from: Scott H Kilau <kilau@prairie.NoDak.edu>
 */
BUILT_IN_FUNCTION(function_stripansi, input)
{
	char	*cp;

	for (cp = input; *cp; cp++)
		if (*cp < 31 && *cp > 13)
			if (*cp != 15 && *cp !=22)
				*cp = (*cp & 127) | 64;

	RETURN_STR(input);
}

BUILT_IN_FUNCTION(function_stripansicodes, input)
{
	RETURN_IF_EMPTY(input);
	RETURN_STR(stripansicodes(input));
}

/*
 * This inverts any ansi sequence present in the string
 * from: Scott H Kilau <kilau@prairie.NoDak.edu>
 */
BUILT_IN_FUNCTION(function_stripmirc, input)
{
	char	*cp;

	for (cp = input; *cp; cp++)
		if (*cp == 3)
			*cp = (*cp & 127) | 64;

	RETURN_STR(input);
}

BUILT_IN_FUNCTION(function_servername, input)
{
	int sval;
	char *which;

	GET_INT_ARG(sval, input);

	/* garbage in, garbage out. */
	if (sval < 0 || sval >= number_of_servers)
		RETURN_EMPTY;

	/* First we try to see what the server thinks it name is */
	which = server_list[sval].itsname;

	/* Next we try what we think its name is */
	if (!which)
		which = server_list[sval].name;

	/* Ok. i give up, return a null. */
	RETURN_STR(which);
}

BUILT_IN_FUNCTION(function_lastserver, input)
{
	RETURN_INT(last_server)
}

BUILT_IN_FUNCTION(function_winserv, input)
{
	int win = 0;
	char *tmp;
	Window *winp;

	if (input && *input)
	{
		if ((tmp = new_next_arg(input, &input)))
			win = atoi(tmp);
	}
	if ((winp = get_window_by_refnum(win)))
		RETURN_INT(winp->server)

	RETURN_INT(-1)
}

BUILT_IN_FUNCTION(function_numwords, input)
{
	RETURN_INT(word_count(input))
}

BUILT_IN_FUNCTION(function_strlen, input)
{
	RETURN_INT(strlen(input))
}

BUILT_IN_FUNCTION(function_aliasctl, input)
{
	int list = -1;
	char *listc;
	enum { GET, SET, MATCH } op;

	GET_STR_ARG(listc, input);
	RETURN_IF_EMPTY(listc);
	if (!my_strnicmp(listc, "AS", 2))
		list = VAR_ALIAS;
	else if (!my_strnicmp(listc, "AL", 2))
		list = COMMAND_ALIAS;
	else if (!my_strnicmp(listc, "LO", 2))
		list = VAR_ALIAS_LOCAL;
	else
		RETURN_EMPTY;

	GET_STR_ARG(listc, input);
	RETURN_IF_EMPTY(listc);
	if (!my_strnicmp(listc, "G", 1))
		op = GET;
	else if (!my_strnicmp(listc, "S", 1))
		op = SET;
	else if (!my_strnicmp(listc, "M", 1))
		op = MATCH;
	else
		RETURN_EMPTY;

	GET_STR_ARG(listc, input);
	RETURN_IF_EMPTY(listc);

	switch (op)
	{
		case (GET) :
		{
			Alias *alias;
			Alias **a_list;

			if (list == VAR_ALIAS_LOCAL)
				a_list = find_local_variable(listc, NULL, 0);
			else
				a_list = &(alias_list[list]);

			if ((alias = find_alias(a_list, listc, 0, NULL, 1)))
				RETURN_STR(alias->stuff);
			else
				RETURN_EMPTY;
		}
		case (SET) :
		{
			add_alias(list, listc, input);
			RETURN_INT(1)
		}
		case (MATCH) :
		{
			char **mlist;
			char *mylist = NULL;
			int num, ctr;

			if (!my_stricmp(listc, "*"))
				listc = empty_string;

			upper(listc);
			mlist = match_alias(listc, &num, list);
			for (ctr = 0; ctr < num; ctr++)
			{
				if (mylist)
					malloc_strcat(&mylist, space);
				malloc_strcat(&mylist, mlist[ctr]);
				new_free((char **)&mlist[ctr]);
			}
			new_free((char **)&mlist);
			if (mylist)
				return mylist;
			RETURN_EMPTY;
		}
		default :
			yell("aliasctl: Error");
			RETURN_EMPTY;
	}
	RETURN_EMPTY;
}


/* 
 * Next two contributed by Scott H Kilau (sheik), who for some reason doesnt 
 * want to take credit for them. *shrug* >;-)
 *
 * Deciding not to be controversial, im keeping the original (contributed)
 * semantics of these two functions, which is to return 1 on success and
 * -1 on error.  If you dont like it, then tough. =)  I didnt write it, and
 * im not going to second guess any useful contributions. >;-)
 */
BUILT_IN_FUNCTION(function_fexist, words)
{
        char	FileBuf[BIG_BUFFER_SIZE+1];
	char	*filename, *fullname;

	if ((filename = new_next_arg(words, &words)))
	{
		if (*filename == '/')
			strcpy(FileBuf, filename);

		else if (*filename == '~') 
		{
			if (!(fullname = expand_twiddle(filename)))
				RETURN_INT(-1)

			strmcpy(FileBuf, fullname, BIG_BUFFER_SIZE);
			new_free(&fullname);
		}
		else 
		{
			getcwd(FileBuf, BIG_BUFFER_SIZE);
			strmcat(FileBuf, "/", BIG_BUFFER_SIZE);
			strmcat(FileBuf, filename, BIG_BUFFER_SIZE);
		}
		if (access(FileBuf, R_OK) == -1)
			RETURN_INT(-1)

		else
			RETURN_INT(1)
	}
	RETURN_INT(-1)
}

/* XXXX - ugh. do we really have to do a access() call first? */
BUILT_IN_FUNCTION(function_fsize, words)
{
        char	FileBuf[BIG_BUFFER_SIZE+1];
	char	*filename, *fullname;
        struct  stat    stat_buf;
	int	filesize = 0;

	filename = next_arg(words, &words);
	if (filename) 
	{
		if (*filename == '/')
			strcpy(FileBuf, filename);

		else if (*filename == '~') 
		{
			if (!(fullname = expand_twiddle(filename)))
				RETURN_INT(-1)

			strmcpy(FileBuf, fullname, BIG_BUFFER_SIZE);
			new_free(&fullname);
		}
		else 
		{
			getcwd(FileBuf, sizeof(FileBuf));
			strmcat(FileBuf, "/", BIG_BUFFER_SIZE);
			strmcat(FileBuf, filename, BIG_BUFFER_SIZE);
		}
		if (access(FileBuf, R_OK) == -1)
			RETURN_INT(-1)
		stat_file(FileBuf, &stat_buf);
		filesize = stat_buf.st_size;
		RETURN_INT(filesize)
	}
	RETURN_INT(-1)
}

/* 
 * Contributed by CrowMan
 * I changed two instances of "RETURN_INT(result)"
 * (where result was a null pointer) to RETURN_STR(empty_string)
 * because i dont think he meant to return a null pointer as an int value.
 */
/*
 * $crypt(password seed)
 * What it does: Returns a 13-char encrypted string when given a seed and
 *    password. Returns zero (0) if one or both args missing. Additional
 *    args ignored.
 * Caveats: Password truncated to 8 chars. Spaces allowed, but password
 *    must be inside "" quotes.
 * Credits: Thanks to Strongbow for showing me how crypt() works.
 * This cheap hack by: CrowMan
 */
BUILT_IN_FUNCTION(function_crypt, words)
{
        char pass[9] = "\0";
        char seed[3] = "\0";
        char *blah, *bleh, *crypt _((const char *, const char *));

	GET_STR_ARG(blah, words)
	GET_STR_ARG(bleh, words)
	strmcpy(pass, blah, 8);
	strmcpy(seed, bleh, 2);
	RETURN_STR(crypt(pass, seed));
}

BUILT_IN_FUNCTION(function_info, words)
{
/*
	char *which;
	extern char *compile_info;
	extern char *info_c_sum;

	GET_STR_ARG(which, words);

	     if (!my_strnicmp(which, "C", 1))
		RETURN_STR(compile_info);
	else if (!my_strnicmp(which, "S", 1))
		RETURN_STR(info_c_sum);

	else
*/
		return m_sprintf("%s+%s", version, compile_time_options);
	/* more to be added as neccesary */
}

/*
 * Based on a contribution made a very long time ago by wintrhawk
 */
BUILT_IN_FUNCTION(function_channelmode, word)
{
	char	channel[257];
	char    *booya = NULL;
	int debug = 0;
	char *curr_chan;

	if (word && *word)
		strmcpy(channel, new_next_arg(word, &word), 254);
	else
		strmcpy(channel, empty_string, 254);

	curr_chan = get_channel_by_refnum(0);
	if (!curr_chan)
		curr_chan = empty_string;

	for (;;)
	{
		char    *mode;

		if (debug++ > 100)
			ircpanic("function_channelmode: infinite loop");

		/* If we are at the end of the list */
		if (!*channel)
		{
			/* If there were other channels, stop */
			if (booya)
				break;

			/* Empty list -- use current channel */
			else
				strmcpy(channel, curr_chan, 256);
		}

		/* Not at end of list, look to see if channel is "*" */
		else if (!strcmp(channel, "*"))
			strmcpy(channel, curr_chan, 256);

		/* If there is still no channel, stop */
		if (!*channel)
			return (booya ? booya : m_strdup(empty_string));

		if ((mode = get_channel_mode(channel, curr_scr_win->server)))
			m_s3cat(&booya, space, (mode || *mode) ? mode : "*");
			
		if (!word || !*word)
			break;

		strmcpy(channel, new_next_arg(word, &word), 256);
	}

	if (!booya)
		RETURN_EMPTY;

	return (booya);
}

BUILT_IN_FUNCTION(function_geom, words)
{
	/* Erf. CO and LI are ints. (crowman) */
	return m_sprintf("%d %d", CO, LI);
}

BUILT_IN_FUNCTION(function_pass, words)
{
	char *lookfor;
	char *final, *ptr;

	GET_STR_ARG(lookfor, words);
	final = (char *)new_malloc(strlen(words) + 1);
	ptr = final;

 	while (*words)
	{
		if (index(lookfor, *words))
			*ptr++ = *words;
		words++;
	}

	*ptr = 0;
	return final;
}

BUILT_IN_FUNCTION(function_uptime, input)
{
	time_t  ltime;
	time_t  days,hours,minutes,seconds;
	struct  timeval         tp;
	static time_t timestart = 0;
	time_t timediff;
	char buffer[BIG_BUFFER_SIZE+1];

	*buffer = '\0';
	get_time(&tp);
	if (timestart == 0)  {
		timestart = tp.tv_sec;
		timediff = 0;
	} else
		timediff = tp.tv_sec - timestart;

	ltime = timediff;
	seconds = ltime % 60;
	ltime = (ltime - seconds) / 60;
	minutes = ltime%60;
	ltime = (ltime - minutes) / 60;
	hours = ltime % 24;
	days = (ltime - hours) / 24;
	sprintf(buffer, "%ldd %ldh %ldm %lds", days, hours, minutes, seconds);
	RETURN_STR(buffer);
}

BUILT_IN_FUNCTION(function_eleet, input)
{
	char temp[BIG_BUFFER_SIZE];
	int  tindex = 0;

	RETURN_IF_EMPTY(input);

	strmcpy(temp,input, sizeof(temp)-1);
	while(temp[tindex])
	{
		switch(temp[tindex])
		{
			case 'e':
			case 'E':
				temp[tindex] = '3';
				break;
			case 'i':
			case 'I':
				temp[tindex] = '1';
				break;
			case 't':
			case 'T':
				temp[tindex] = '+';
				break;
			case 'o':
			case 'O':
				temp[tindex] = '0';
				break;
			case 'c':
			case 'C':
				temp[tindex] = '(';
				break;
			case 's':
			case 'S':
				temp[tindex] = '5';
				break;
			case 'l':
			case 'L':
				temp[tindex] = '|';
				break;
			case 'a':
			case 'A':			
				temp[tindex] = '4';
				break;
			default:
				break;
		}
		tindex++;
	}
	RETURN_STR(temp);
}

BUILT_IN_FUNCTION(function_cool, input)
{
	char temp[BIG_BUFFER_SIZE];
	char this;
	int  cindex = 0;

	RETURN_IF_EMPTY(input);
	strmcpy(temp, input, sizeof(temp)-1);
	while(temp[cindex])
	{
		this = temp[cindex];
		temp[cindex++] = toupper(this);
		this = temp[cindex];
		temp[cindex++] = tolower(this);
	}
	RETURN_STR(temp);
}

BUILT_IN_FUNCTION(function_annoy, input)
{
	char temp[BIG_BUFFER_SIZE];
	int aindex = 0, rnum, r2num;

	RETURN_IF_EMPTY(input);

	strmcpy(temp, input, sizeof(temp)-1);
	while (*input)
	{
		rnum = (rand() % 4);
		r2num = (rand() % 3);
		switch(rnum)
		{
			case 0:
				if (r2num == 0)
					temp[aindex++] = *input;
				else if (r2num == 1)
					temp[aindex++] = tolower(*input);
				else if (r2num == 2)
					temp[aindex++] = toupper(*input);
				input++;
				break;
			case 1:
				temp[aindex++] = '\002';
                                if (r2num == 0)
                                        temp[aindex++] = *input;
                                else if (r2num == 1)
                                        temp[aindex++] = tolower(*input);
                                else if (r2num == 2)
                                        temp[aindex++] = toupper(*input);
				temp[aindex++] = '\002';
				input++;
				break;
			case 2:
                                temp[aindex++] = '\026';
                                if (r2num == 0)
                                        temp[aindex++] = *input;
                                else if (r2num == 1)
                                        temp[aindex++] = tolower(*input);
                                else if (r2num == 2)
                                        temp[aindex++] = toupper(*input);
                                temp[aindex++] = '\026';
                                input++;
                                break;
        		case 3:
                                temp[aindex++] = '\037';
                                if (r2num == 0)
                                        temp[aindex++] = *input;
                                else if (r2num == 1)
                                        temp[aindex++] = tolower(*input);
                                else if (r2num == 2)
                                        temp[aindex++] = toupper(*input);
                                temp[aindex++] = '\037';
                                input++;
                                break;
		}
	}
	RETURN_STR(temp);
}

BUILT_IN_FUNCTION(function_cluster, input)
{
char *q;
	RETURN_IF_EMPTY(input);
	q = cluster(input);
	if (q)
		RETURN_STR(q);
	else
		RETURN_EMPTY;
}

BUILT_IN_FUNCTION(function_checkshit, input)
{
char *uh = NULL;
char *channel = NULL, *thuh = NULL;
ShitList *tmp;
int matched = 0;
	GET_STR_ARG(uh, input);
	GET_STR_ARG(channel, input);	
	RETURN_IF_EMPTY(uh);
	RETURN_IF_EMPTY(channel);

	for (tmp = shitlist_list; tmp; tmp = tmp->next)
	{
		if (wild_match(tmp->filter, uh) && check_channel_match(tmp->channels, channel))
		{
			matched = 1;
			break;
		}
	}

	if (tmp && matched)
	{
		malloc_sprintf(&thuh, "%d %s", tmp->level, tmp->channels);
		return(thuh);
	}
	RETURN_EMPTY;
}

BUILT_IN_FUNCTION(function_checkuser, input)
{
char *uh = NULL;
char *channel = NULL, *thuh = NULL;
UserList *tmp;
int matched = 0;
	GET_STR_ARG(uh, input);
	GET_STR_ARG(channel, input);	
	RETURN_IF_EMPTY(uh);
	RETURN_IF_EMPTY(channel);

	for (tmp = user_list; tmp; tmp = tmp->next)
	{
		malloc_sprintf(&thuh, "*!%s", tmp->host);
		if (wild_match(thuh, uh) && check_channel_match(tmp->channels, channel))
		{
			matched = 1;
			break;
		}
	}
	   
	new_free(&thuh);

	if (tmp && matched)
	{
		malloc_sprintf(&thuh, "%d %d %d %s %s %s", tmp->level, tmp->aop, tmp->prot, tmp->host, tmp->channels, tmp->password?tmp->password:empty_string);
		return(thuh);
	}

	RETURN_EMPTY;
}

BUILT_IN_FUNCTION(function_checkbot, input)
{
char *uh = NULL;
char *channel = NULL, *thuh = NULL;
UserList *tmp;
int matched = 0;
	GET_STR_ARG(uh, input);
	GET_STR_ARG(channel, input);	
	RETURN_IF_EMPTY(uh);
	RETURN_IF_EMPTY(channel);

	for (tmp = Bot_list; tmp; tmp = tmp->next)
	{
		malloc_sprintf(&thuh, "*!%s", tmp->host);
		if (wild_match(thuh, uh) && check_channel_match(tmp->channels, channel))
		{
			matched = 1;
			break;
		}
	}
	   
	new_free(&thuh);

	if (tmp && matched)
	{
		malloc_sprintf(&thuh, "%d %d %s %s", tmp->aop, tmp->prot, tmp->host, tmp->channels);
		return(thuh);
	}

	RETURN_EMPTY;
}

BUILT_IN_FUNCTION(function_rot13, input)
{
	char temp[BIG_BUFFER_SIZE];
	char *p = NULL;
	int rotate = 13;
	strmcpy(temp, input, sizeof(temp)-1);
	for (p = temp; *p; p++) {
		if (*p >= 'A' && *p <='Z')
			*p = (*p - 'A' + rotate) % 26 + 'A';
		else if (*p >= 'a' && *p <= 'z')
			*p = (*p - 'a' + rotate) % 26 + 'a';
	}
	RETURN_STR(temp);
}

BUILT_IN_FUNCTION(function_flash, input )
{
int num;
	GET_INT_ARG(num, input);
	switch (num)
	{
		case 3:
			RETURN_STR("**\030B00");
		case 4:
			RETURN_STR("\033**EMSI_IRQ8E08");
		case 2:
			RETURN_STR("\033[2J\033#8\033[?3;5h\033(0");
		case 1:
		default:
			RETURN_STR("\033#8\033[?3;5h\033(0]");
			break;
	}
	RETURN_EMPTY;
}


BUILT_IN_FUNCTION(function_repeat, words)
{
	int num;
	char *final = NULL;

	GET_INT_ARG(num, words);
	if (num < 1)
		RETURN_EMPTY;

	final = (char *)new_malloc(strlen(words) * num + 1);
	*final = 0;
	for (; num > 0; num--)
		strcat(final, words);
	if (strlen(final) > BIG_BUFFER_SIZE)
		final[BIG_BUFFER_SIZE] = 0;
	return final;
}


BUILT_IN_FUNCTION(function_bcopy, words)
{
	int from, to;
	if (in_cparse)
		RETURN_INT(0);
	GET_INT_ARG(from, words);
	GET_INT_ARG(to, words);	
	RETURN_INT(file_copy(from, to)); 	
}

BUILT_IN_FUNCTION(function_epic, words)
{
	RETURN_INT(1);
}

BUILT_IN_FUNCTION(function_winsize, words)
{
	int refnum;
	Window *win;

	if (words && *words)
	{
		GET_INT_ARG(refnum, words);
		win = get_window_by_refnum(refnum);
	}
	else
		win = curr_scr_win;

	if (!win)
		RETURN_EMPTY;
		
	RETURN_INT(win->display_size);
}

BUILT_IN_FUNCTION(function_umode, words)
{
	int servnum;

	if (words && *words)
	{
		GET_INT_ARG(servnum, words);
	}
	else
		servnum = from_server;

	RETURN_STR(get_umode(servnum));
}


BUILT_IN_FUNCTION(function_lastnotice, words)
{
	int count = 0;
	char *str = NULL;
	GET_INT_ARG(count, words);
	if (count >= MAX_LAST_MSG)
		count = 0;
	RETURN_IF_EMPTY(last_notice[count].last_msg);
	malloc_sprintf(&str, "%s %s %s %s %s", last_notice[count].time, last_notice[count].from, last_notice[count].uh, last_notice[count].to, last_notice[count].last_msg);
	return str;
}

BUILT_IN_FUNCTION(function_lastmessage, words)
{
	int count = 0;
	char *str = NULL;
	GET_INT_ARG(count, words);
	if (count >= MAX_LAST_MSG)
		count = 0;
	RETURN_IF_EMPTY(last_msg[count].last_msg);
	malloc_sprintf(&str, "%s %s %s %s %s", last_msg[count].time, last_msg[count].from, last_msg[count].uh, last_msg[count].to, last_msg[count].last_msg);
	return str;
}

char *function_addtabkey _((char *words))
{
char *arrayname = NULL;
char *nick = NULL;

	GET_STR_ARG(nick, words);
	if (words && *words)
		arrayname = new_next_arg(words, &words);
	addtabkey(nick, arrayname ? 1: 0);
	RETURN_EMPTY;
}

char *function_gettabkey _((char *words))
{
char *arrayname = NULL;
char *nick = NULL;
int direction;

	GET_INT_ARG(direction, words);
	
	if (words && *words)
		arrayname = new_next_arg(words, &words);
	
	if (arrayname && !my_stricmp(arrayname, "AUTOREPLY"))
		nick = gettabkey(direction, 1, NULL);
	else
		nick = gettabkey(direction, 0, NULL);
	
	return nick ? m_strdup(nick) : m_strdup("");
}

#ifdef __STDC__
static int sort_it (const void *one, const void *two)
#else
static int sort_it (one, two)
const void *one, *two;
#endif
{
	return my_stricmp(*(char **)one, *(char **)two);
}

BUILT_IN_FUNCTION(function_sort, words)
{
	int wordc;
	char **wordl;

	wordc = splitw(words, &wordl);
	qsort((void *)wordl, wordc, sizeof(char *), sort_it);

	return unsplitw(wordl, wordc);	/* DONT USE RETURN_STR() HERE */
}

BUILT_IN_FUNCTION(function_notify, words)
{
	int showon = -1, showserver = 0;
	char *firstw;

	while (words && *words)
	{
		firstw = new_next_arg(words, &words);
 		if (!my_strnicmp(firstw, "on", 2))
 		{
 			showon = 1;
 			continue;
		}
		if (!my_strnicmp(firstw, "off", 3))
		{
			showon = 0;
			continue;
		}
		if (!my_strnicmp(firstw, "serv", 4))
		{
			GET_INT_ARG(showserver, words);
 		}
	}
 
	/* dont use RETURN_STR() here. */
	return get_notify_nicks(showserver, showon);
}

#ifdef __STDC__
static int num_sort_it (const void *one, const void *two)
#else
static int num_sort_it (one, two)
const char *one, *two;
#endif
{
	char *oneptr = *(char **)one;
	char *twoptr = *(char **)two;
	long val1, val2;

	while (*oneptr && *twoptr)
	{
		while (*oneptr && *twoptr && !isdigit(*oneptr) && !isdigit(*twoptr))
		{
			if (*oneptr != *twoptr)
				return (*oneptr - *twoptr);
			oneptr++, twoptr++;
		}

		if (!*oneptr || !*twoptr)
			break;

		val1 = strtol(oneptr, (char **)&oneptr, 10);
		val2 = strtol(twoptr, (char **)&twoptr, 10);
		if (val1 != val2)
			return val1 - val2;
	}
	return (*oneptr - *twoptr);
}

BUILT_IN_FUNCTION(function_numsort, words)
{
	int wordc;
	char **wordl;

	wordc = splitw(words, &wordl);
	qsort((void *)wordl, wordc, sizeof(char *), num_sort_it);

	return unsplitw(wordl, wordc);	/* DONT USE RETURN_STR() HERE */
}

#ifdef NEED_GLOB
#define glob bsd_glob
#define globfree bsd_globfree
#endif

BUILT_IN_FUNCTION(function_glob, word)
{
#ifdef INCLUDE_GLOB_FUNCTION
	char *path, *path2, *retval = NULL;
	int numglobs, i;
	glob_t globbers;

	bzero(&globbers, sizeof(glob_t));
	while (word && *word)
	{
		GET_STR_ARG(path, word);
		path2 = expand_twiddle(path);
		if (!path2)
			path2 = path;

		numglobs = glob(path2, GLOB_MARK, NULL, &globbers);
		if (numglobs < 0)
			RETURN_INT(numglobs);
		for (i = 0; i < globbers.gl_pathc; i++)
			m_s3cat(&retval, space, globbers.gl_pathv[i]);

		globfree(&globbers);
	}

	RETURN_IF_EMPTY(retval);
	return retval;
#else
	RETURN_EMPTY;
#endif
}

BUILT_IN_FUNCTION(function_userver, word)
{
ChannelList *chan;
char *nick = NULL;
NickList *Nick;

	GET_STR_ARG(nick, word);
	for (chan = server_list[from_server].chan_list; chan; chan = chan->next)
	{
		if ((Nick = (NickList *)find_in_list((List **)&chan->nicks, nick, 0)))
			RETURN_STR(Nick->server);
	}
	RETURN_EMPTY;
}

BUILT_IN_FUNCTION(function_mkdir, words)
{
	int 	failure = 0;
	char *expanded;

	while (words && *words)
	{
		expanded = expand_twiddle(new_next_arg(words, &words));
		failure -= mkdir(expanded, 0777);
		new_free(&expanded);
	}

	RETURN_INT(failure)
}

BUILT_IN_FUNCTION(function_umask, words)
{
	int new_umask;
	GET_INT_ARG(new_umask, words);
	RETURN_INT(umask(new_umask));
}

extern char *get_help_topic _((char *, int));
extern int  read_file _((FILE *, int));

BUILT_IN_FUNCTION(function_help, words)
{
char *filename = NULL, *subject = NULL;
static int first_time = 1;
FILE *help_file;
	GET_STR_ARG(subject, words);
	if (words && *words)
		words = next_arg(filename, &words);
	else
		malloc_strcpy(&filename, get_string_var(SCRIPT_HELP_VAR));
	if (first_time && filename)
	{
		char *new_file = NULL;
		malloc_strcpy(&new_file, filename);
	        if (!(help_file = uzfopen(&new_file, ".")))
        	{
			new_free(&new_file);
			RETURN_EMPTY;
		}
		first_time = 0;
		new_free(&new_file);
		read_file(help_file, 1);
		fclose(help_file);
	} else if (first_time) RETURN_EMPTY;
	return get_help_topic(subject, (filename) ? 1 : 0);
}

BUILT_IN_FUNCTION(function_isuser, words)
{
char *nick = NULL;
char *uhost = NULL;
char *tmp = NULL;
char *channel = NULL;
UserList *User;
UserList *Bot;
ShitList *Shit;

	GET_STR_ARG(nick, words);
	GET_STR_ARG(uhost, words);
	if (words && *words)
		channel = words;
	else
		channel = "*";
	if ((Bot = lookup_userlevelc(nick, uhost, channel, Bot_list)))
	{
		malloc_sprintf(&tmp, "BOT %s %d %d %s %s", Bot->aop, Bot->prot, Bot->host, Bot->channels);
		return tmp;			
	}
	if ((User = lookup_userlevelc("*", uhost, channel, user_list)))
	{
		malloc_sprintf(&tmp, "USER %d %d %d %s %s", User->level, User->aop, User->prot, User->host, User->channels);
		return tmp;
	}
#if 0
	if ((User = nickinuser(nick, uhost)) && check_channel_match(User->channels, channel))
	{
		malloc_sprintf(&tmp, "USER %d %d %d %s %s", User->level, User->aop, User->prot, User->host, User->channels);
		RETURN_STR(tmp);
	}
	if ((Bot = nickinbot(nick, uhost)) && check_channel_match(Bot->channels, channel))
	{
		malloc_sprintf(&tmp, "BOT %s %d %d %s %s", Bot->aop, Bot->prot, Bot->host, Bot->channels);
		RETURN_STR(tmp);			
	}
#endif
	if ((Shit = nickinshit(nick, uhost)) && check_channel_match(Shit->channels, channel))
	{
		malloc_sprintf(&tmp, "SHIT %s %d %s", Shit->filter, Shit->level, Shit->channels);
		return tmp;
	}
	RETURN_EMPTY;
}

/*
$pad(N string of text goes here)
if N is negative, it'll pad to the right 
if N is positive, it'll pad to the left

so something like: $pad(20 letters) would output:
            letters
and $pad(20 some string) would be:
        some string
GREAT way to allign shit, and if you use $curpos() can can add that and
figure out the indent :p
hohoho, aren't we ingenious, better yet, you can do a strlen() on the
string youw anna output, then - the curpos, and if its over, grab all
words to the right of that position and output them [so its on one line]
then indent, grab the next section of words, and then the next and then
next

   Jordy (jordy@thirdwave.net) 19960622 
*/

BUILT_IN_FUNCTION(function_pad, word)
{
	int	length = 0;
	char	*strbuf = NULL;

	GET_INT_ARG(length, word);
	malloc_sprintf(&strbuf, "%*s", length, word);
	return strbuf;
}

BUILT_IN_FUNCTION(function_isban, word)
{
	char *channel;
	char *ban;
	ShitList *Ban;
	GET_STR_ARG(channel, word);
	GET_STR_ARG(ban, word);
	if ((Ban = (ShitList *)find_in_list((List **)&shitlist_list, ban, 0)) && check_channel_match(channel, Ban->channels))
		RETURN_INT(1);
	RETURN_INT(0);
}

BUILT_IN_FUNCTION(function_banonchannel, word)
{
	ChannelList *chan;
	char *channel;
	char *ban;
	GET_STR_ARG(channel, word);
	GET_STR_ARG(ban, word);
	if ((chan = lookup_channel(channel, from_server, 0)))
	{
		if (ban_is_on_channel(ban, chan))
			RETURN_INT(1);
	}
	RETURN_INT(0);
}

BUILT_IN_FUNCTION(function_isop, word)
{
	char *channel;
	char *nick;
	ChannelList *chan;
	NickList *Nick;
	GET_STR_ARG(channel, word);
	GET_STR_ARG(nick, word);
	if ((chan = lookup_channel(channel, from_server, CHAN_NOUNLINK)))
	{
		if ((Nick = (NickList *)list_lookup((List **)&(chan->nicks), nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)) && 
				((Nick->userlist && Nick->userlist->aop) || 
				(Nick->botlist && Nick->botlist->aop)))
			RETURN_INT(1);
	}
	RETURN_INT(0);
}

BUILT_IN_FUNCTION(function_isvoice, word)
{
	char *channel;
	char *nick;
	ChannelList *chan;
	NickList *Nick;
	GET_STR_ARG(channel, word);
	GET_STR_ARG(nick, word);
	if ((chan = lookup_channel(channel, from_server, CHAN_NOUNLINK)))
	{
		if ((Nick = (NickList *)list_lookup((List **)&(chan->nicks), nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)) && Nick->voice)
			RETURN_INT(1);
	}
	RETURN_INT(0);
}

BUILT_IN_FUNCTION(function_randomnick, word)
{
	RETURN_STR(random_str(3,9));
}



BUILT_IN_FUNCTION(function_openserver, word)
{
int port = -1;
char *servern = NULL;
int socket_num = -1;
unsigned short this_sucks;
CloneList *new;
	GET_STR_ARG(servern, word);
	GET_INT_ARG(port, word);
	this_sucks = (unsigned short)port;
	if ((socket_num = connect_by_number(servern, &this_sucks, SERVICE_CLIENT, PROTOCOL_TCP, 1)) < 0)
		RETURN_INT(-1);
	FD_SET(socket_num, &writables);
	FD_SET(socket_num, &readables);

	new = (CloneList *)new_malloc(sizeof(CloneList));
	new->number = m_sprintf("%d", socket_num);
	new->server = m_strdup(servern);
	new->port = port;
	new->socket_num = socket_num;
	add_to_list((List **)&clones, (List *)new);

	RETURN_INT(socket_num);
}

BUILT_IN_FUNCTION(function_closeserver, word)
{
int socket_num = -1;
CloneList *new;
	GET_INT_ARG(socket_num, word);
	if (socket_num != -1)
	{
		if ((new = (CloneList *)remove_from_list((List **)&clones, ltoa(socket_num))))
		{
			new_free(&new->server);
			new_free(&new->number);
			new_free((char **)&new);
			FD_CLR(socket_num, &writables);
			FD_CLR(socket_num, &readables);
			new_close(socket_num);
		}
	}	
	RETURN_INT(socket_num);
}

BUILT_IN_FUNCTION(function_readserver, word)
{
int socket_num = -1;
CloneList *new;
	GET_INT_ARG(socket_num, word);
	if (socket_num != -1 && (new = (CloneList *)find_in_list((List **)&clones, ltoa(socket_num), 0)) )
	{
		char buffer[IRCD_BUFFER_SIZE+1];
		char *str;
		char *s;
		int old_timeout = dgets_timeout(1);
		*buffer = 0;
		str = buffer;
		switch(dgets(str, IRCD_BUFFER_SIZE-2, socket_num, NULL))
		{
			case -1:
				socket_num = 0;
				break;
			case 0:
				FD_CLR(socket_num, &readables);
				socket_num = -1;
				break;
			default:
				dgets_timeout(old_timeout);
				if ((buffer[strlen(buffer)-1] == '') || (buffer[strlen(buffer)-1] == '\r') || (buffer[strlen(buffer)-1] == '\n'))
					buffer[strlen(buffer)-1] = 0;
				if ((buffer[strlen(buffer)-1] == '') || (buffer[strlen(buffer)-1] == '\r') || (buffer[strlen(buffer)-1] == '\n'))
					buffer[strlen(buffer)-1] = 0;
				s = m_sprintf("%d %s", strlen(buffer), buffer);
				new->warn = 0;
				return s;
		}
		(void) dgets_timeout(old_timeout);
		new->warn = 0;
	}
	RETURN_INT(socket_num);
}

BUILT_IN_FUNCTION(function_writeserver, word)
{
int socket_num = -1;
int len = -1;
int i = 0;
	GET_INT_ARG(socket_num, word);
	if (socket_num != -1)
	{
		char buffer[IRCD_BUFFER_SIZE + 1];
		i = -1;
		strncpy(buffer, word, IRCD_BUFFER_SIZE - 2);
		strmcat(buffer, "\n", IRCD_BUFFER_SIZE - 1);
		len = strlen(buffer);
/*
		FD_SET(socket_num, &writables);
		FD_CLR(socket_num, &writables);
*/
		i = send(socket_num, buffer, len, 0);
	}
	RETURN_INT(len);
}

BUILT_IN_FUNCTION(function_cparse, word)
{
char *format = NULL;
	GET_STR_ARG(format, word);
	in_cparse++;
	RETURN_STR(convert_output_format(format, word));
}

BUILT_IN_FUNCTION(function_getreason, word)
{
char *nick = NULL;
	GET_STR_ARG(nick, word);
	RETURN_STR(get_reason(nick));
}

BUILT_IN_FUNCTION(function_chmod, words)
{
	char *filearg, *after;
	int fd = -1;
	char *perm_s;
	mode_t perm;

	GET_STR_ARG(filearg, words);
	fd = (int) strtoul(filearg, &after, 10);
	GET_STR_ARG(perm_s, words);
	perm = (mode_t) strtol(perm_s, &perm_s, 8);

	if (after != words && *after == 0)
	{
		if (file_valid(fd))
			RETURN_INT(fchmod(fd, perm))
		else
			RETURN_EMPTY;
	}
	else
		RETURN_INT(chmod(filearg, perm))
}

BUILT_IN_FUNCTION(function_twiddle, words)
{
	if (words && *words)
		return expand_twiddle(new_next_arg(words, &words));

	RETURN_EMPTY;
}


/* 
 * Date: Sun, 29 Sep 1996 19:17:25 -0700
 * Author: Thomas Morgan <tmorgan@pobox.com>
 * Submitted-by: Archon <nuance@twri.tamu.edu>
 *
 * $uniq (string of text)
 * Given a set of words, returns a new set of words with duplicates
 * removed.
 * EX: $uniq(one two one two) returns "one two"
 */
BUILT_IN_FUNCTION(function_uniq, word)
{
        char    **list = NULL, *booya = NULL;
        int     listc, listi;

        listc = splitw(word, &list);

        for (listi = 0; listi < listc; listi++)
        {
                if (booya == NULL)
                        m_s3cat(&booya, space, list[listi]);
                else
                {
                        char *rmatch_input =
                                malloc(strlen(list[listi])
                                       + strlen(booya) + 2);

                        strcpy(rmatch_input, list[listi]);
                        strcat(rmatch_input, space);
                        strcat(rmatch_input, booya);

                        if (!function_rmatch(rmatch_input))
                                m_s3cat(&booya, space, list[listi]);

                        free(rmatch_input);
                }
        }

        new_free((char **)&list);

        if (!booya)
                RETURN_EMPTY;

        return (booya);                         /* DONT USE RETURN_STR HERE! */
}

BUILT_IN_FUNCTION(function_uhost, word)
{
char *nick;
char *answer = NULL;
int i;
ChannelList *chan;
NickList *n;
	GET_STR_ARG(nick, word);
	for (i = 0; i < number_of_servers; i++)
	{
		for (chan = server_list[i].chan_list; chan; chan = chan->next)
		{
			if ((n = (NickList *)find_in_list((List **)&chan->nicks, nick, 0)))
			{
				malloc_strcat(&answer, n->host);
				malloc_strcat(&answer, " ");
			}
		}
	}
	if (!answer)
		RETURN_EMPTY;
	return answer;
}

BUILT_IN_FUNCTION(function_numdiff, word)
{
int value1 = 0, value2 = 0;
	GET_INT_ARG(value1, word);
	GET_INT_ARG(value2, word);
	RETURN_INT(value1-value2);
}

BUILT_IN_FUNCTION(function_winvisible, word)
{
	RETURN_INT(get_visible_by_refnum(word));
}

BUILT_IN_FUNCTION(function_mircansi, word)
{
/* mconv v1.00 (c) copyright 1996 Ananda, all rights reserved.	*/
/* -----------------------------------------------------------	*/
/* mIRC->ansi color code convertor:	12.26.96		*/
/* map of mIRC color values to ansi color codes	*/
/* format: ansi fg color	ansi bg color	*/
struct {
	char *fg, *bg;
} codes[16] = {

	{ "[1;37m",   "[47m"        },      /* white                */
	{ "[0m",      "[40m"        },      /* black (grey for us)  */
	{ "[0;34m",   "[44m"        },      /* blue                 */
	{ "[0;32m",   "[42m"        },      /* green                */
	{ "[0;31m",   "[41m"        },      /* red                  */
	{ "[0;33m",   "[43m"        },      /* brown                */
	{ "[0;35m",   "[45m"        },      /* magenta              */

	{ "[1;31m",   "[46m"        },      /* bright red           */
	{ "[1;33m",   "[47m"        },      /* yellow               */

	{ "[1;32m",   "[42m"            },      /* bright green         */
	{ "[0;36m",   "[44m"            },      /* cyan                 */
	{ "[1;36m",   "[44m"            },      /* bright cyan          */
	{ "[1;34m",   "[44m"            },      /* bright blue          */
	{ "[1;35m",   "[45m"            },      /* bright magenta       */
	{ "[1;30m",   "[40m"            },      /* dark grey            */
	{ "[0;37m",   "[47m"            }       /* grey                 */
};
	char new_buffer[2*BIG_BUFFER_SIZE+1];
	register char *sptr = word, *dptr = new_buffer;
	short code;
	char *save;
	
	RETURN_IF_EMPTY(word);
	while (*sptr) {
		if (*sptr == '' && isdigit(sptr[1])) {
			save = sptr;
			sptr++;
			code = atoi(sptr);
			if (code > 15 || code < 0) 
				continue;
			while (isdigit(*sptr)) 
				sptr++;
			sprintf(dptr, "%s", codes[code].fg);
			while (*dptr) dptr++;
			if (*sptr == ',') {
				sptr++;
				code = atoi(sptr);
				if (code > 0 && code < 15)
				{
					sprintf(dptr, "%s", codes[code].bg);
					while (*dptr) dptr++;
				}
				while (isdigit(*sptr)) 
					sptr++;
			}
		} else *dptr++ = *sptr++;
	}
	*dptr = 0;
	RETURN_STR(new_buffer);
}

BUILT_IN_FUNCTION(function_winrefs, word)
{
char buffer[BIG_BUFFER_SIZE];
Screen *screen;
Window *win;
	*buffer = 0;
	for (screen = screen_list; screen; screen = screen->next)
		for(win = screen->window_list; win; win = win->next)
		{
			strcat(buffer, ltoa(win->refnum));
			strcat(buffer, " ");
		}
	for (win = invisible_list; win; win = win->next)
	{
		strcat(buffer, ltoa(win->refnum));
		strcat(buffer, " ");
	}
	RETURN_STR(buffer);
}

BUILT_IN_FUNCTION(function_getenv, word)
{
char *p;
char *q;
	GET_STR_ARG(q, word);
	p = getenv(q);
	if (p && *p)
		RETURN_STR(p);
	RETURN_EMPTY;
}

BUILT_IN_FUNCTION(function_gethost, word)
{
ChannelList *chan;
NickList *nick;
char *arg;
	GET_STR_ARG(arg, word);
	if (from_server != -1)
	{
	for (chan = server_list[from_server].chan_list; chan; chan = chan->next)
		{
			for (nick = chan->nicks; nick; nick = nick->next)
			{
				if (!my_stricmp(arg, nick->nick))
					RETURN_STR(nick->host);
			}
		}
	}
	RETURN_EMPTY;
}

BUILT_IN_FUNCTION(function_getvar, words)
{
	Alias	*alias;
	if ((alias = find_alias(&(alias_list[VAR_ALIAS]), words, 0, NULL, 1)))
	{
		if (alias->stuff)
			RETURN_STR(alias->stuff);
		else
			RETURN_EMPTY;
	}
	RETURN_EMPTY;	
}
