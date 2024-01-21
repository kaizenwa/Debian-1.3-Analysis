/*
 * edit.c: This is really a mishmash of function and such that deal with IRCII
 * commands (both normal and keybinding commands) 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"

#include <sys/stat.h>

#ifdef ESIX
# include <lan/net_types.h>
#endif

#include "parse.h"
#include "ircterm.h"
#include "server.h"
#include "chelp.h"
#include "edit.h"
#include "crypt.h"
#include "vars.h"
#include "ircaux.h"
#include "lastlog.h"
#include "window.h"
#include "screen.h"
#include "whois.h"
#include "hook.h"
#include "input.h"
#include "ignore.h"
#include "keys.h"
#include "names.h"
#include "alias.h"
#include "history.h"
#include "funny.h"
#include "ctcp.h"
#include "dcc.h"
#include "output.h"
#include "exec.h"
#include "notify.h"
#include "numbers.h"
#include "status.h"
#include "if.h"
#include "help.h"
#include "stack.h"
#include "queue.h"
#include "timer.h"
#include "list.h"
#include "userlist.h"
#include "misc.h"
#include "tcl_bx.h"

#ifdef WANT_CD
#include "cdrom.h"
#endif

#ifdef WANT_TCL
Tcl_Interp *interp = NULL;
#endif

#define COMMENT_HACK 

static	int	save_which;
static	int	save_do_all;
	int	in_e_nick = 0;
extern	int	doing_notice;

static	void	oper_password_received _((char *, char *));

int	no_hook_notify = 0;
char	*last_sent_msg_body = NULL;

extern void io _((void));

extern	char	*forwardnick;
extern char	*last_sent_msg;
extern char	*last_sent_notice;
extern char	*last_sent_ctcp;
extern char	*last_sent_topic;
extern char	*last_sent_wall;
extern char	cx_function[];

/* used with /save */
#define	SFLAG_ALIAS	0x0001
#define	SFLAG_BIND	0x0002
#define	SFLAG_ON	0x0004
#define	SFLAG_SET	0x0008
#define	SFLAG_NOTIFY	0x0010
#define	SFLAG_DIGRAPH	0x0020

/* The maximum number of recursive LOAD levels allowed */
#define MAX_LOAD_DEPTH 10

/* recv_nick: the nickname of the last person to send you a privmsg */
	char	*recv_nick = NULL;

/* sent_nick: the nickname of the last person to whom you sent a privmsg */
	char	*sent_nick = NULL;
	char	*sent_body = NULL;

/* Used to keep down the nesting of /LOADs and to determine if we
 * should activate the warning for /ONs if the NOVICE variable is set.
 */
	int	load_depth = 0;


	int	interactive = 0;
	
/* Used to prevent global messaging */
extern	int	who_on_join;
extern  int	doing_who;

typedef	struct	WaitCmdstru
{
	char	*stuff;
	struct	WaitCmdstru	*next;
}	WaitCmd;

static	WaitCmd	*start_wait_list = NULL,
		*end_wait_list = NULL;

	char	lame_wait_nick[] = "1#LAME";

/* a few advance declarations */
extern	void	repeatcmd _((char *, char *, char *));
static	void	do_unkey _((char *, char *, char *));
static	void	do_unscrew _((char *, char *, char *));
static	void	do_getout _((char *, char *, char *));
static	void	do_mynames _((char *, char *, char *));
static	void	my_whois _((char *, char *, char *));
static	void	do_4op _((char *, char *, char *));
static	void	do_umode _((char *, char *, char *));
static	void	do_invite _((char *, char *, char *));
static	void	do_forward _((char *, char *, char *));
static	void	do_oops _((char *, char *, char *));
static	void	sendlinecmd _((char *, char *, char *));
static	void	do_send_text _((char *, char *, char *));
static	void	funny_stuff _((char *, char *, char *));
static	void	cd _((char *, char *, char *));
static	void	e_wall _((char *, char *, char *));
static	void	send_2comm _((char *, char *, char *));
static	void	send_comm _((char *, char *, char *));
static	void	send_topic _((char *, char *, char *));
static	void	send_kick _((char *, char *, char *));
static	void	send_channel_com _((char *, char *, char *));
static	void	quote _((char *, char *, char *));
static	void	e_privmsg _((char *, char *, char *));
static	void	flush _((char *, char *, char *));
static	void	oper _((char *, char *, char *));
static	void	e_channel _((char *, char *, char *));
static	void	who _((char *, char *, char *));
static	void	whois _((char *, char *, char *));
static	void	ison _((char *, char *, char *));
static	void	userhost _((char *, char *, char *));
static	void	info _((char *, char *, char *));
static	void	e_nick _((char *, char *, char *));
static	void	comment _((char *, char *, char *));
static	void	sleepcmd _((char *, char *, char *));
static	void	version1 _((char *, char *, char *));
static	void	ctcp _((char *, char *, char *));
static	void	dcc _((char *, char *, char *));
static	void	deop _((char *, char *, char *));
static	void	my_echo _((char *, char *, char *));
static	void	save_settings _((char *, char *, char *));
static	void	redirect _((char *, char *, char *));
static	void	waitcmd _((char *, char *, char *));
static	void	describe _((char *, char *, char *));
static	void	me _((char *, char *, char *));
static	void	evalcmd _((char *, char *, char *));
static	void	hook _((char *, char *, char *));
static	void	inputcmd _((char *, char *, char *));
static	void	pingcmd _((char *, char *, char *));
static	void	xtypecmd _((char *, char *, char *));
static	void	beepcmd _((char *, char *, char *));
static	void	abortcmd _((char *, char *, char *));
static	void	really_save _((char *, char *));
static	void	e_debug _((char *, char *, char *));
static	void	do_scan _((char *, char *, char *));
static	void	push_cmd _((char *, char *, char *));
static	void	pop_cmd _((char *, char *, char *));
static	void	unshift_cmd _((char *, char *, char *));
static	void	shift_cmd _((char *, char *, char *));
static	void	exec_cmd _((char *, char *, char *));
static	void	auto_join _((char *, char *, char *));
static	void	dcc_crash _((char *, char *, char *));
static	void	do_msay _((char *, char *, char *));
static	void	send_mode _((char *, char *, char *));
static	void	do_offers _((char *, char *, char *));

static	void	ctcp_version _((char *, char *, char *));
static	void	about _((char *, char *, char *));
	void	dcc_stat_comm _((char *, char *, char *));
	void	sping _((char *, char *, char *));
static  void    realname_cmd    _((char *, char *, char *));
static  void    set_username    _((char *, char *, char *));
static  void    e_call		_((char *, char *, char *));


extern	void	do_toggle _((char *, char *, char *));
extern	void	e_quit _((char *, char *, char *));
extern	void	do_ig _((char *, char *, char *));
extern	void	do_listshit _((char *, char *, char *));
extern	void	savelists _((char *, char *, char *));
extern	void	mknu _((char *, char *, char *));
extern	void	reconnect_cmd _((char *, char *, char *));
extern  void    LameKick  _((char *, char *, char *));
extern  void    ChanWallOp _((char *, char *, char *));
extern  void    NewUser _((char *, char *, char *));
extern  void    ReconnectServer _((char *, char *, char *));
extern  void    MegaDeop _((char *, char *, char *));
extern  void    do_flood _((char *, char *, char *));
extern  void    cycle _((char *, char *, char *));
extern  void    bomb _((char *, char *, char *));
extern  void    finger _((char *, char *, char *));
extern  void    multkick _((char *, char *, char *));
extern  void    massdeop _((char *, char *, char *));
extern  void    doop _((char *, char *, char *));
extern  void    dodeop _((char *, char *, char *));
extern  void    massop _((char *, char *, char *));
extern  void    whokill _((char *, char *, char *));
extern  void 	ban _((char *, char *, char *));
extern  void 	kickban _((char *, char *, char *));
extern  void 	massban _((char *, char *, char *));
extern  void 	dokick _((char *, char *, char *));
extern  void 	nslookup _((char *, char *, char *));
extern  void 	masskick _((char *, char *, char *));
extern  void 	do_flood _((char *, char *, char *));
extern  void 	reset _((char *, char *, char *));
extern  void 	users _((char *, char *, char *));
extern	void	my_ignorehost _((char *, char *,char *));
extern	void	my_ignore _((char *, char *,char *));
extern	void	unban _((char *, char *,char *));
extern	void	masskickban _((char *, char *,char *));
extern  void    linklook _((char *, char *, char *));
extern  void    newhost _((char *, char *, char *));
extern  void	do_dump _((char *, char *, char *));
extern  void	do_dirlastnotice _((char *, char *, char *));
extern	void	do_dirlistmsg _((char *, char *, char *));
extern  void	do_dirlastmsg _((char *, char *, char *));
extern  void	do_dirlastctcp _((char *, char *, char *));
extern  void	do_dirlastctcpreply _((char *, char *, char *));
extern  void	do_dirlastinvite _((char *, char *, char *));
extern	void 	readlog _((char *, char *, char *));
extern	void 	remove_log _((char *, char *, char *));
extern	void 	add_user _((char *, char *, char *));
extern	void	bot _((char *, char *, char *));
extern	void	do_uptime _((char *, char *, char *));
extern	void	cdcc _((char *, char *, char *));
extern	void	extern_write _((char *, char *, char *));
extern	void	showuserlist _((char *, char *, char *));
extern	void	init_dcc_chat _((char *, char *, char *));
extern	void	add_shit _((char *, char *, char *));
extern	void	showshitlist _((char *, char *, char *));
extern	void	channel_stats _((char *, char *, char *));
extern	void	my_clear _((char *, char *, char *));
extern	void	stubcmd _((char *, char *, char *));
extern	void	addidle _((char *, char *, char *));
extern	void	showidle _((char *, char *, char *));
extern	void	kickidle _((char *, char *, char *));
extern	void	usage _((char *, char *, char *));
extern	void	add_env _((char *));
extern	void	reload_save _((char *, char *, char *));
extern	void	cset_variable _((char *, char *, char *));
extern	void	banstat _((char *,char *, char *));
extern	void	nwhois _((char *,char *, char *));
extern	void	statkgrep _((char *, char *, char *));
extern	void	tban _((char *, char *, char *));
extern	void	bantype _((char *, char *, char *));
extern	void	whowas _((char *, char *, char *));
extern	void	do_sk _((char *, char *, char *));
extern	void	findports _((char *, char *, char *));
extern	void	add_ban_word _((char *, char *, char *));
extern	void	show_word_kick _((char *, char *, char *));
extern	void	whereis _((char *, char *, char *));
extern	void	clear_tab _((char *, char *, char *));
extern	void	topic_lock _((char *, char *, char *));
extern	void	mode_lock _((char *, char *, char *));
extern	void	randomnick _((char *, char *, char *));
extern	void	topic_lock _((char *, char *, char *));
extern	void	show_version _((char *, char *, char *));
extern	void	chat _((char *, char *, char *));
extern	void	back _((char *, char *, char *));
extern	void	tog_fprot _((char *, char *, char *));
extern	void	ftp _((char *, char *, char *));
extern	void	tcl_command _((char *, char *, char *));
extern	void	do_dirsentlastnotice _((char *, char *, char *));
extern	void	do_dirsentlastmsg _((char *, char *, char *));
extern	void	do_dirlastwall _((char *, char *, char *));
extern	void	do_dirlasttopic _((char *, char *, char *));
extern	void	do_dirsentlastwall _((char *, char *, char *));
extern	void	do_dirsentlasttopic _((char *, char *, char *));
extern	void	do_dirlastserver _((char *, char *, char *));
extern	void	botlink _((char *, char *, char *));
extern	void	jnw _((char *, char *, char *));
extern	void	lkw _((char *, char *, char *));
extern	void	whokill _((char *, char *, char *));
extern	void	csay		_((char *, char *, char *));
extern	void	clink		_((char *, char *, char *));
extern	void	cwho		_((char *, char *, char *));
extern	void	cboot		_((char *, char *, char *));
extern	void	cmsg		_((char *, char *, char *));
extern	void	toggle_xlink	_((char *, char *, char *));
extern	void	dcx		_((char *, char *, char *));
extern	void	orig_nick	_((char *, char *, char *));
extern	void	print_structs	_((char *, char *, char *));
static	void	pretend_cmd	_((char *, char *, char *));
static	void	e_pause		_((char *, char *, char *));
extern	void	add_bad_nick	_((char *, char *, char *));
extern	void	serv_stat	_((char *, char *, char *));
extern	void	fuckem		_((char *, char *, char *));
extern	void	tracekill	_((char *, char *, char *));
extern	void	traceserv	_((char *, char *, char *));
extern	void	dll_load	_((char *, char *, char *));
extern	void	tignore		_((char *, char *, char *));
extern	void	dumpcmd		_((char *, char *, char *));
extern	void	aliascmd	_((char *, char *, char *));
extern	void	set_autoreply	_((char *, char *, char *));

static	IrcCommand *find_command _((char *, int *));
#ifdef WANT_DLL
	IrcCommandDll *find_dll_command _((char *, int *));
#endif

AJoinList *ajoin_list = NULL;

#define NONOVICEABBREV 0x0001
#define	NOINTERACTIVE  0x0002
#define	NOSIMPLESCRIPT 0x0004
#define	NOCOMPLEXSCRIPT 0x0008

/*
 * irc_command: all the availble irc commands:  Note that the first entry has
 * a zero length string name and a null server command... this little trick
 * makes "/ blah blah blah" to always be sent to a channel, bypassing queries,
 * etc.  Neato.  This list MUST be sorted.
 */

#ifdef WANT_DLL
IrcCommandDll *dll_commands = NULL;
#endif

static	IrcCommand irc_command[] =
{
	{ "",		empty_string,	do_send_text,		NOSIMPLESCRIPT| NOCOMPLEXSCRIPT },
	/*
	 * I really want to remove #, but it will break a lot of scripts.  - mycroft
	 *
	 * I agree - it should be converted to a special character in parse_line.
	 *                                                            - Troy
	 */
	{ "#",		NULL,		comment, 		0 },
	{ "4OP",	NULL,		do_4op,			0 },
	{ ":",		NULL,		comment, 		0 },
        { "ABORT",      NULL,           abortcmd,               0 },
	{ "ABOUT",	NULL,		about,			0 },
	{ "ADDBOT",	"AddBot",	bot,			0 },
	{ "ADDFORWARD",	"AddForward", 	do_forward,		0 },
	{ "ADDIDLE",	"AddIdle",	addidle,		0 },
	{ "ADDLAMENICK","AddLameNick",	add_bad_nick,		0 },
	{ "ADDSHIT",	NULL,		add_shit,		0 },
	{ "ADDUSER",	NULL,		add_user,		0 },
	{ "ADDWORD",	NULL,		add_ban_word,		0 },
	{ "ADMIN",	"ADMIN",	send_comm, 		0 },
	{ "AJOIN",	NULL,		auto_join,		0 },
	{ "AJOINLIST",	"AJoinList",	auto_join,		0 },
	{ "ALIAS",	"0",		aliascmd,		0 },
	{ "ASSIGN",	"1",		aliascmd,		0 },
	{ "AWAY",	"Away",		away,			0 },
	{ "B",		NULL,		ban,			0 },
	{ "BACK",	"Back",		back,			0 },
	{ "BAN",	NULL,		ban,			0 },
	{ "BANSTAT",	NULL,		banstat,		0 },
	{ "BANTYPE",	NULL,		bantype,		0 },
	{ "BANWORDS",	NULL,		add_ban_word,		0 },
	{ "BEEP",	NULL,		beepcmd,		0 },
	{ "BHELP",	NULL,		chelp,			0 },
	{ "BIND",	NULL,		bindcmd,		0 },
	{ "BK",		NULL,		kickban,		0 },
	{ "BKI",	"Bki",		kickban,		0 },
	{ "BLINK",	NULL,		botlink,		0 },
	{ "BOMB",	NULL,		bomb,			0 },
	{ "BOOT",	NULL,		multkick,		0 },
	{ "BOT",	NULL,		bot,			0 },
	{ "BOTLIST",	"BotList",	bot,			0 },
	{ "BYE",	"QUIT",		e_quit,			NONOVICEABBREV},
	{ "C",		"MODE",		send_mode,		0 },
	{ "CALL",	NULL,		e_call,			0 },
	{ "CBOOT",	"Cboot",	cboot,			0 },
	{ "CD",		NULL,		cd,			0 },
	{ "CDCC",	NULL,		cdcc,			0 },
#ifdef WANT_CD
	{ "CDEJECT",	NULL,		cd_eject,		0 },
	{ "CDHELP",	NULL,		cd_help,		0 },
	{ "CDLIST",	NULL,		cd_list,		0 },
	{ "CDPAUSE",	NULL,		cd_pause,		0 },
	{ "CDPLAY",	NULL,		cd_play,		0 },
	{ "CDSTOP",	NULL,		cd_stop,		0 },
	{ "CDVOL",	NULL,		cd_volume,		0 },
#endif
	{ "CHANNEL",	"JOIN",		e_channel,		0 },
	{ "CHANST",	NULL,		channel_stats,		0 },
	{ "CHAT",	"Chat",		chat,			0 },
	{ "CHELP",	"Chelp",	chelp,			0 },
	{ "CHGAOP",	"ChgAop",	change_user,		0 },
	{ "CHGCHAN",	"ChgChan",	change_user,		0 },
	{ "CHGLEVEL",	"ChgLevel",	change_user,		0 },
	{ "CHGPROT",	"ChgProt",	change_user,		0 },
	{ "CHOPS",	"Chops",	users,			0 },
	{ "CLEAR",	NULL,		my_clear,		0 },
	{ "CLEARAUTO",	"CLEARAUTO",	clear_tab,		0 },
	{ "CLEARLOCK",	"ClearLock",	mode_lock,		0 },
	{ "CLEARTAB",	NULL,		clear_tab,		0 },
	{ "CLINK",	"Clink",	clink,			0 },
	{ "CMSG",	"Cmsg",		cmsg,			0 },
	{ "COMMENT",	NULL,		comment,		0 },
	{ "CONNECT",	"CONNECT",	send_comm,		0 },
	{ "CSAY",	"Csay",		csay,			0 },
	{ "CSET",	"Cset",		cset_variable,		0 },
	{ "CTCC",	NULL,		dcc,			0 },
	{ "CTCP",	NULL,		ctcp,			0 },
	{ "CTOGGLE",	NULL,		toggle_xlink,		0 },
	{ "CWHO",	"Cwho",		cwho,			0 },
	{ "CWHOM",	"Cwhom",	cwho,			0 },
	{ "CYCLE",	NULL,		cycle,			0 },
	{ "D",		NULL,		describe,		0 },
	{ "DATE",	"TIME",		send_comm,		0 },
	{ "DBAN",	NULL,		unban,			0 },
	{ "DC",		NULL,		init_dcc_chat,		0 },
	{ "DCA",	"Dca",		dcx,			0 },
	{ "DCC",	NULL,		dcc,			0 },
	{ "DCCCRASH",	NULL,		dcc_crash,		0 },
	{ "DCG",	"Dcg",		dcx,			0 },
	{ "DCS",	"Dcs",		dcx,			0 },
	{ "DCX",	"Dcx",		dcx,			0 },
	{ "DEBUG",	NULL,		e_debug,		0 },
	{ "DEOP",	NULL,		dodeop,			0 },
	{ "DEOPER",	NULL,		deop,			0 },
	{ "DESCRIBE",	NULL,		describe,		0 },
	{ "DEVOICE",	"DeVoice",	dodeop,			0 },
	{ "DF",		"df",		exec_cmd,		0 },
	{ "DIE",	"DIE",		send_comm,		0 },
	{ "DISCONNECT",	NULL,		disconnectcmd,		0 },
	{ "DNS",	NULL,		nslookup,		0 },
	{ "DO",		NULL,		docmd,			0 },
	{ "DOP",	NULL,		dodeop,			0 },
	{ "DS",		NULL,		dcc_stat_comm,		0 },
	{ "DUMP",	NULL,		dumpcmd,		0 },
	{ "ECHO",	NULL,		my_echo,		0 },
	{ "ENCRYPT",	NULL,		encrypt_cmd,		0 },
	{ "EVAL",	NULL,		evalcmd,		0 },
	{ "EXEC",	NULL,		execcmd,		0 },
	{ "EXIT",	"QUIT",		e_quit,			NONOVICEABBREV},
	{ "FE",		"Fe",		fe,			0 },
	{ "FEC",	"Fec",		fe,			0 },
	{ "FINGER",	NULL,		finger,			0 },
	{ "FK",		"FK",		masskick,		0 },
	{ "FKLINE",	NULL,		statkgrep,		0 },
	{ "FLOOD",	NULL,		do_flood,		0 },
	{ "FLUSH",	NULL,		flush,			0 },
	{ "FOR",	NULL,		forcmd,			0 },
	{ "FOREACH",	NULL,		foreach,		0 },
	{ "FORWARD",	"Forward", 	do_forward,		0 },
	{ "FPORTS",	NULL,		findports,		0 },
	{ "FPROT",	"FProt",	tog_fprot,		0 },
	{ "FTP",	"Ftp",		ftp,			0 },
	{ "FUCK",	"Fuck",		kickban,		0 },
	{ "FUCKEM",	NULL,		fuckem,			0 },
	{ "HASH",	"HASH",		send_comm,		0 },
	{ "HELP",	NULL,		help,			0 },
	{ "HISTORY",	NULL,		history,		0 },
	{ "HOOK",	NULL,		hook,			0 },
	{ "HOST",	"USERHOST",	userhost,		0 },
	{ "HOSTNAME",	"HOSTNAME",	e_hostname,		0 },
	{ "I",		"INVITE",	do_invite,		0 },
	{ "IF",		"IF",		ifcmd,			0 },
	{ "IG",		"Ig",		do_ig,			0 },
	{ "IGH",	"IgH",		do_ig,			0 },
	{ "IGNORE",	NULL,		ignore,			0 },
	{ "INFO",	"INFO",		info,			0 },
	{ "INPUT",	"Input",	inputcmd,		0 },
	{ "INPUT_CHAR", "Input_Char",	inputcmd,		0 },
	{ "INVITE",	"INVITE",	do_invite,		0 },
	{ "IRCHOST",	"HOSTNAME",	e_hostname,		0 },
	{ "IRCNAME",	NULL,		realname_cmd,		0 },
	{ "IRCUSER",	NULL,		set_username,		0 },
	{ "ISON",	"ISON",		ison,			0 },
	{ "J",		"JOIN",		e_channel,		0 },
	{ "JNW",	"Jnw",		jnw,			0 },
	{ "JOIN",	"JOIN",		e_channel,		0 },
	{ "K",		NULL,		dokick,			0 },
	{ "KB",		NULL,		kickban,		0 },
	{ "KICK",	"KICK",		send_kick,		0 },
	{ "KICKIDLE",	NULL,		kickidle,		0 },
	{ "KILL",	"KILL",		send_2comm,		0 },
	{ "L",		"PART",		do_getout,		0 },
	{ "LAMENICKLIST",NULL,		add_bad_nick,		0 },
	{ "LASTLOG",	NULL,		lastlog,		0 },
	{ "LEAVE",	"PART",		send_2comm,		0 },
	{ "LINKS",	"LINKS",	send_comm,		NONOVICEABBREV},
	{ "LIST",	"LIST",		funny_stuff,		0 },
#ifdef WANT_DLL
	{ "LISTDLL",	"LISTDLL",	dll_load,		0 },
#endif
	{ "LK",		"LameKick",	LameKick,		0 },
	{ "LKW",	"Lkw",		lkw,			0 },
	{ "LLOOK",	NULL,		linklook,		0 },
	{ "LOAD",	"LOAD",		load,			0 },
#ifdef WANT_DLL
	{ "LOADDLL",	NULL,		dll_load,		0 },
#endif
	{ "LOCAL",	"2",		aliascmd,		0 },
	{ "LS",		"ls",		exec_cmd,		0 },
	{ "LUSERS",	"LUSERS",	send_comm,		0 },
	{ "M",		"PRIVMSG",	e_privmsg,		0 },
	{ "MB",		NULL,		massban,		0 },
	{ "MD",		NULL,		massdeop,		0 },
	{ "MDOP",	NULL,		massdeop,		0 },
	{ "MDVOICE",	"MDVoice",	massdeop,		0 },
	{ "ME",		NULL,		me,			0 },
	{ "MESG",	NULL,		extern_write,		0 },
	{ "MK",		NULL,		masskick,		0 },
	{ "MKB",	NULL,		masskickban,		0 },
	{ "MKNU",	NULL,		mknu,			0 },
	{ "MODE",	"MODE",		send_channel_com,	0 },
	{ "MODELOCK",	"ModeLock",	mode_lock,		0 },
	{ "MOP",	NULL,		massop,			0 },
	{ "MORE",	"More",		readlog,		0 },
	{ "MOTD",	"MOTD",		send_comm,		0 },
	{ "MSAY",	"MSay",		do_msay,		0 },
	{ "MSG",	"PRIVMSG",	e_privmsg,		0 },
	{ "MUB",	NULL,		unban,			0 },
	{ "MULT",	NULL,		multkick,		0 },
	{ "MVOICE",	"MVoice",	massop,			0 },
	{ "N",		"NAMES",	do_mynames,		0 },
	{ "NAMES",	"NAMES",	funny_stuff,		0 },
	{ "NEWNICK",	NULL,		newnick,		0 },
	{ "NEWUSER",	NULL,		newuser,		0 },
	{ "NICK",	"NICK",		e_nick,			0 },
	{ "NOCHAT",	"NoChat",	chat,			0 },
	{ "NOFORWARD",  "NoForward",	do_forward,		0 },
	{ "NOPS",	"Nops",		users,			0 },
	{ "NOTE",	"NOTE",		send_comm,		0 },
	{ "NOTICE",	"NOTICE",	e_privmsg,		0 },
	{ "NOTIFY",	NULL,		notify,			0 },
	{ "NSLOOKUP",	NULL,		nslookup,		0 },
	{ "NWHOIS",	NULL,		nwhois,			0 },
	{ "NWHOWAS",	NULL,		whowas,			0 },
	{ "OFFERS",	"Offers",	do_offers,		0 },
	{ "ON",		NULL,		on,			0 },
	{ "OOPS",	NULL,		do_oops,		0 },
	{ "OP",		NULL,		doop,			0 },
	{ "OPER",	"OPER",		oper,			0 },
	{ "ORIGNICK",	"OrigNick",	orig_nick,		0 },
	{ "OSTAT",	NULL, 		serv_stat,		0 },
	{ "P",		"Ping",		pingcmd,		0 },
	{ "PARSEKEY",	NULL,		parsekeycmd,		0 },
	{ "PART",	"PART",		send_2comm,		0 },
	{ "PAUSE",	NULL,		e_pause,		0 },
	{ "PING",	NULL, 		pingcmd,		0 },
	{ "POP",	NULL,		pop_cmd,		0 },
	{ "PRETEND",	NULL,		pretend_cmd,		0 },
	{ "PS",		"ps",		exec_cmd,		0 },
	{ "PUSH",	NULL,		push_cmd,		0 },
	{ "Q",		NULL,		query,			0 },
	{ "QK",		NULL,		dokick,			0 },
	{ "QUERY",	NULL,		query,			0 },
	{ "QUEUE",      NULL,           queuecmd,               0 },
	{ "QUIT",	"QUIT",		e_quit,			NONOVICEABBREV},
	{ "QUOTE",	NULL,		quote,			0 },
	{ "RANDOMNICK",	NULL,		randomnick,		0 },
	{ "RBIND",	NULL,		rbindcmd,		0 },
	{ "READLOG",	"ReadLog",	readlog,		0 },
	{ "RECONNECT",	NULL,		reconnect_cmd,		0 },
	{ "REDIRECT",	NULL,		redirect,		0 },
	{ "REHASH",	"REHASH",	send_comm,		0 },
	{ "REL",	NULL,		do_dirlistmsg,		0 },

	{ "RELC",	NULL,		do_dirlastctcp,		0 },
	{ "RELCR",	NULL,		do_dirlastctcpreply,	0 },
	{ "RELCRT",	"RelCrt",	do_dirlastctcpreply,	0 },
	{ "RELCT",	"RelCt",	do_dirlastctcp,		0 },

	{ "RELI",	NULL,		do_dirlastinvite,	0 },
	{ "RELIT",	"RelIt",	do_dirlastinvite,	0 },
	{ "RELM",	NULL,		do_dirlastmsg,		0 },
	{ "RELMT",	"RelMt",	do_dirlastmsg,		0 },
	{ "RELN",	NULL,		do_dirlastnotice,	0 },
	{ "RELNT",	"RelNt",	do_dirlastnotice,	0 },
	{ "RELOAD",	NULL,		reload_save,		0 },
	{ "RELS",	NULL,		do_dirlastserver,	0 },
	{ "RELSM",	NULL,		do_dirsentlastmsg,	0 },
	{ "RELSMT",	"RelSmt",	do_dirsentlastmsg,	0 },

	{ "RELSN",	NULL,		do_dirsentlastnotice,	0 },
	{ "RELSNT",	"RelSnt",	do_dirsentlastnotice,	0 },
	{ "RELST",	NULL,		do_dirsentlasttopic,	0 },
	{ "RELSTT",	"RelLstt",	do_dirsentlasttopic,	0 },
	{ "RELSW",	NULL,		do_dirsentlastwall,	0 },
	{ "RELSWT",	"RelLstw",	do_dirsentlastwall,	0 },
	{ "RELT",	NULL,		do_dirlasttopic,	0 },
	{ "RELTT",	"RelTt",	do_dirlasttopic,	0 },
	{ "RELW",	NULL,		do_dirlastwall,		0 },
	{ "RELWT",	"RelWt",	do_dirlastwall,		0 },
	{ "REMLOG",	"RemLog",	remove_log,		0 },
	{ "REPEAT", 	NULL, 		repeatcmd,		0 },
	{ "REQUEST",	NULL,		ctcp,			0 },
	{ "RESET",	NULL,		reset,			0 },
	{ "RESTART",	"RESTART",	send_comm,		0 },
	{ "RPING",	"RPING",	send_comm,		0 },
	{ "SAVEA",	NULL,		save_settings,		0 },
	{ "SAVEALL",	"SaveAll",	savelists,		0 },
	{ "SAVELIST",	NULL, 		savelists,		0 },
	{ "SAY",	empty_string,	do_send_text,		0 },
	{ "SC",		"NAMES",	do_mynames,		0 },
	{ "SCAN",	"scan",		do_scan,		0 },
	{ "SCANI",	"ScanI",	do_scan,		0 },
	{ "SCANN",	"ScanN",	do_scan,		0 },
	{ "SCANO",	"ScanO",	do_scan,		0 },
	{ "SCANV",	"ScanV",	do_scan,		0 },
	{ "SEND",	NULL,		do_send_text,		0 },
	{ "SENDLINE",	empty_string,	sendlinecmd,		0 },
	{ "SERVER",	NULL,		server,			0 },
	{ "SET",	NULL,		set_variable,		0 },
	{ "SETAR",	NULL,		set_autoreply,		0 },
	{ "SHELL",	"Shell",	ftp,			0 },
	{ "SHIFT",	NULL,		shift_cmd,		0 },
	{ "SHIT",	NULL,		add_shit,		0 },
	{ "SHITLIST",	NULL,		showshitlist,		0 },
	{ "SHOWIDLE",	NULL,		showidle,		0 },
	{ "SHOWLOCK",	"ShowLock",	mode_lock,		0 },
	{ "SHOWSPLIT",	NULL,		linklook,		0 },
	{ "SHOWWORDKICK",NULL,		show_word_kick,		0 },
	{ "SIGNOFF",	"QUIT",		e_quit,			NONOVICEABBREV},
	{ "SK",		NULL,		do_sk,			0 },
	{ "SLEEP",	NULL,		sleepcmd,		0 },
	{ "SPING",	"Sping",	sping,			0 },
	{ "SQUIT",	"SQUIT",	send_2comm,		0 },
	{ "STACK",	NULL,		stackcmd,		0 },
	{ "STATS",	"STATS",	send_comm,		0 },
#ifdef WANT_STRUCT
	{ "STRUCT",	NULL,		print_structs,		0 },
#endif
	{ "STUB",	"Stub",		aliascmd,		0 },
	{ "SUMMON",	"SUMMON",	send_comm,		0 },
	{ "SV",		"Sv",		show_version,		0 },
	{ "SWITCH",	"SWITCH",	switchcmd,		0 },
	{ "T", 		"TOPIC",	send_topic,		0 },
	{ "TBAN",	NULL,		tban,			0 },
	{ "TCL",	NULL,		tcl_command,		0 },
	{ "TELNET",	"telnet",	ftp,			0 },
	{ "TIGNORE",	NULL,		tignore,		0 },
	{ "TIME",	"TIME",		send_comm,		0 },
	{ "TIMER",	"TIMER",	timercmd,		0 },
	{ "TLOCK",	"TLock",	topic_lock,		0 },
	{ "TOGGLE",	NULL,		do_toggle,		0 },
	{ "TOPIC",	"TOPIC",	send_topic,		0 },
	{ "TRACE",	"TRACE",	send_comm,		0 },
	{ "TRACEKILL",	"TraceKill",	tracekill,		0 },
	{ "TRACESERV",	"TraceServ",	traceserv,		0 },
	{ "TYPE",	NULL,		type,			0 },
	{ "U",		NULL,		users,			0 },
	{ "UB",		NULL,		unban,			0 },
	{ "UMODE",	"MODE",		do_umode,		0 },
	{ "UNAJOIN",	"UnAjoin",	auto_join,		0 },
	{ "UNBAN",	NULL,		unban,			0 },
	{ "UNBANWORD",	"UnWordKick",	add_ban_word,		0 },
	{ "UNBOT",	"UnBot",	bot,			0 },
	{ "UNFORWARD",  "NoForward",	do_forward,		0 },
	{ "UNIDLE",	"UnIdle",	addidle,		0 },
	{ "UNIG",	"UnIg",		do_ig,			0 },
	{ "UNIGH",	"UnIgH",	do_ig,			0 },
	{ "UNKEY",	NULL,		do_unkey,		0 },
	{ "UNLAMENICK","UnLameNick",	add_bad_nick,		0 },
	{ "UNLESS",	"UNLESS",	ifcmd,			0 },
	{ "UNSCREW",	NULL,		do_unscrew,		0 },
	{ "UNSHIFT",	NULL,		unshift_cmd,		0 },
	{ "UNSHIT",	"UnShit",	add_shit,		0 },
	{ "UNTIL",	"UNTIL",	whilecmd,		0 },
	{ "UNTOPIC",	"UNTOPIC",	send_topic,		0 },
	{ "UNUSER",	"UnUser",	add_user,		0 },
	{ "UNVOICE",	"Unvoice",	dodeop,			0 },
	{ "UNWORDKICK",	"UnWordKick",	add_ban_word,		0 },
	{ "UPING",	"uPing",	pingcmd,		0 },
	{ "UPTIME",	NULL,		do_uptime,		0 },
	{ "USAGE",	NULL,		usage,			0 },
	{ "USER",	NULL,		users,			0 },
	{ "USERHOST",	NULL,		userhost,		0 },
	{ "USERLIST",	NULL,		showuserlist,		0 },
	{ "USERS",	"USERS",	send_comm,		0 },
	{ "VER",	"Version",	ctcp_version,		0 },
	{ "VERSION",	"VERSION",	version1,		0 },
	{ "VOICE",	"Voice",	doop,			0 },
	{ "W",		"W",		who,			0 },
	{ "WAIT",	NULL,		waitcmd,		0 },
	{ "WALL",	"WALL",		ChanWallOp,		0 },
	{ "WALLMSG",	NULL,		ChanWallOp,		0 },
	{ "WALLOPS",	"WALLOPS",	e_wall,			0 },
	{ "WHEREIS",	NULL,		whereis,		0 },
	{ "WHICH",	"WHICH",	load,			0 },
	{ "WHILE",	"WHILE",	whilecmd,		0 },
	{ "WHO",	"Who",		who,			0 },
	{ "WHOIS",	"WHOIS",	whois,			0 },
	{ "WHOKILL",	"WhoKill",	whokill,		0 },
	{ "WHOLEFT",	"WhoLeft",	whowas,			0 },
	{ "WHOWAS",	"WhoWas",	whois,			0 },
	{ "WI",		"Whois",	whois,			0 },
	{ "WII",	"Whois",	my_whois,		0 },
	{ "WINDOW",	NULL,		window,			0 },
	{ "WORDLIST",	NULL,		show_word_kick,		0 },
	{ "WW",		"WhoWas",	whois,			0 },
	{ "XECHO",	"XECHO",	my_echo,		0 },
	{ "XTRA",	"XTRA",		e_privmsg,		0 },
	{ "XTYPE",	NULL,		xtypecmd,		0 },
	{ NULL,		NULL,		comment,		0 }
};

/* number of entries in irc_command array */
#define	NUMBER_OF_COMMANDS (sizeof(irc_command) / sizeof(IrcCommand)) - 2

BUILT_IN_COMMAND(about)
{
	int i = strip_ansi_in_echo;
	strip_ansi_in_echo = 0; 

	put_it("(U[0;25;35;40m [0m");
	put_it("[35m    ÜÜÜÜÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛßÛÛ[1;45m°±²[0;35;40m  [1;45m²±[0;35;40mß[1;32mÜ[42m±²[40mÜ[0;35;40m [1;45m²[40mÞ[0m");
	put_it("[35m [1;31mÜß[0m   [1;35;45m²[0;35;40mÛÛÛÛÛÛÛÛÛÛÛÛ[1;31;41m²[40mß[0m   [1;35;45m²[0;35;40mÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ[1;31;41m²[40mß[0m   [1;35;45m°[0;35;40mÛÛÛÛÛÛÛÛßß[1;32mÜÝ[0;35;40mÞÛÛ[1;45m°±[0;35;40m  [1mß[0;32;40mÜ[1;42m°±²Û[40mß[0;35;40m [1;45m±[0;35;40mÞ[0m");
	put_it("[35mÞ[1;31;41m±[0m    [35mÜ[1;31mÜ[0;32;40m   [35mßÛ[1;31;41m²[40mß[0;32;40m  [1;35mÜ[45m²[31;41m±[0m    [35mÜÜÜÜ[1;45mß[0;35;40mÛÛ[1;31;45mÜ[40mß[0;32;40m   [35mÜ[1;31mÜ[0;32;40m  [1;35m ß[45mÜ[31;41m±[0m    [35mÜ[1;31mÜ[0;32;40m   ÜÜÜÜ[1;42m°±²[0m [35mÛÛÛÛ[1;45m°[0;35;40m [32mÜ[1;42m°±[0;32;40mßß[1mß[35mÜ[45m²[0;35;40m [1;45m°[0;35;40mÞ[0m");
	put_it("[35mÛ[1;31;41m°[0m    [1;35;45m°[31;41mÛ[0m    [1;35;45m [31mÜÜ[0;35;40mßßßÛ[1;31;41m°[0m    [1;35;45m°[31;41mÛ[40mß[0;32;40m   [1;35;45m°[31;41mÛ[0m    [1;35;45m°[31;41m²[0m    [1;35;45m²[31;41m°[0m    [1;35;45m°[31;41mÛ[0m    [35mÜ[32mßßßß[1;42m±²[40mÜ[0;35;40mß[1;45m°[0;35;40mß[32mÜßß[1;35mÜÜ[45m²[40mÝ[0;35;40m [1;45m²±[0;35;40m ÛÞ[0m");
	put_it("[35mÛ[1;31;41m [0m    [1;35;45m±[31;41m²[0m    [1;35;45m°[31;41m²[0m    [1;35;45m°[31;41m [0m    [1;35;45m±[31;41m²[0m    [1;35;45m±[31;41m²[0m    [1;35;45m±[31mÜ[0;35;40mßßßß[1;45mÜ[31;41m [0m    [1;35;45m±[31;41m²[0m    [1;35;45m°[0;35;40mÛÛÛÛÜÜÜ[32mß[1mþ[0;32;40mÜ[35mþ  [1;45m²±±[0;35;40mÝ [1;45m±°[0;35;40m Û[1;30mÞ[0m");
	put_it("[35mÛ[31;45m²[0m    [1;35;45m²[31;41m±[0m    [1;35;45m±[31;41m±[0m    [1;35;45m±[0;31;45m²[0m    [1;35;45m²[31;41m±[0m    [1;35;45m²[31;41m±[0m    [1;35;45m²[31;41m±[0m    [1;35;45m±[0;31;45m²[0m    [1;35;45m²[31;41m±[0m    [1;35;45m±[0;35;40mÛÛÛÛÛß[32mÜß[35mÜÜ[1;32mßÜÜ[0;32;40mÜ[35mßßÝ [1;45m°[0;35;40mÛ ÛÞ[0m");
	put_it("[35mÛ[31;45m±[0m    [1;35;45mÛ[31;41m°[0m    [1;35;45m²[31;41m°[0m    [1;35;45m²[0;31;45m±[0m    [1;35;45mÛ[31;41m°[0m    [1;35;45mÛ[31;41m°[0m    [1;35;45mÛ[31;41m°[0m    [1;35;45m²[0;31;45m±[0m    [1;35;45mÛ[31;41m°[0m    [1;35;45m²[0;35;40mÛÛÛß[32mÜ[1;42m°[0;32;40mÝ[35mÞÛ[1;30;45m°[0;35;40mÝ[1;32mÞÛ[42m²±°[0;32;40mÜÜÜ[35mß ÛÞ[0m");
	put_it("[35mÛ[31;45m°[0m    [1;35mß[0;31;40mß[1;34m   [35mÜ[45mÛ[31;41m [0m   [1;35mÜ[45mÛ[0;31;45mß[40mÜ[1;34m   [35mß[0;31;40mß[1;34m   [35mÜ[45mß[0;31;45mß[40mÜ[1;34m   [35mß[0;31;40mß[1;34m   [35mÜ[45mß[0;31;45m°[0m   [1;35mÜ[45mÛ[31;41m [0m   [1;35mÜ[45mÛ[0;35;40mÛß[1;32mÜ[42m±°[0;32;40mÛ[0m [35mÛÛÛÛ[34m [1;32;42m²±±[0;32;40mÛ[1;42m°[0;32;40mÛÛÛÜÜ[1;30mÞ[0m");
	put_it("[35mÞÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛß[1;32mÜ[42m²±°[0;32;40mÛÝ[35mÞÛ[1;30;45m°[0;35;40mÛ[1;30;45m°[0;35;40m [34m [1;32;42m±°°[0;32;40mÛÛÛßß[1;30mÜ[0;35;40mÞ");
	put_it(" ßÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÝ[1;32mÞ[42m²±°[0;32;40mÛÛ[0m [35mÛÛÛ[1;30;45m°±[0;35;40m  [34m [1;32;42m°[0;32;40mßß[35m [1;30mÜ[45m²[0;35;40m [1;30;45m²[40mÞ[0;35;40m");
	put_it("    ßßßßÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÜ[1;32mß[0;32;40mßßß[35mÜ[30;45mrE[35;40mÛ[1;30;45m°±²[0;35;40m  [1;30;45m²[40mÜ[45m²[40mÝ[0;35;40m [1;30;45m²±[0;35;40m [1;30;45m±[40mÞ[0m");
	put_it(empty_string);
	put_it("[1mÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ²[0mÜ");
	put_it("[1;47mÛ[0;30;47m                                                                             [0m");
	put_it("[1;47m²[0;30;47m Grtz To: Trench, HappyCrappy, Yak, Zircon, Otiluke, Masonry, BuddahX, Hob, [1m°[0m");
	put_it("[1;47m±[0;30;47m          Lifendel, JondalaR, JVaughn, Suicide, NovaLogic, Jordy, BigHead,  [1m±[0m");
	put_it("[1;47m°[0;30;47m          Ananda, Hybrid, Reefa, BlackJac, GenX and Bark0de!                [1m²[0m");
	put_it("[1;47m [0;30;47m                                                                            [1mÛ[0m");
	put_it("ß[1;30m²ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß[0m");
	put_it(empty_string);
	strip_ansi_in_echo = i; 
}


BUILT_IN_COMMAND(dcc_stat_comm)
{
	dcc_stats(NULL);
}

void handle_dcc_chat(WhoisStuff *stuff, char *nick, char *args)
{
	if (!stuff || !stuff->nick || !nick || !strcmp(stuff->user, "<UNKNOWN>") || !strcmp(stuff->host, "<UNKNOWN>"))
	{
		bitchsay("No such nick %s", nick);
		return;
	}
	dcc_chat(nick);
}

BUILT_IN_COMMAND(init_dcc_chat)
{
char *nick = next_arg(args, &args);
	if (nick)
	{
		do {
			add_to_userhost_queue(nick, handle_dcc_chat, nick, NULL);
		} while ((nick = next_arg(args, &args)));
	}
	else
		userage("dc", "<nick|nick,nick>");
}

/*
 * find_command: looks for the given name in the command list, returning a
 * pointer to the first match and the number of matches in cnt.  If no
 * matches are found, null is returned (as well as cnt being 0). The command
 * list is sorted, so we do a binary search.  The returned commands always
 * points to the first match in the list.  If the match is exact, it is
 * returned and cnt is set to the number of matches * -1.  Thus is 4 commands
 * matched, but the first was as exact match, cnt is -4.
 */
static	IrcCommand *
find_command(com, cnt)
	char	*com;
	int	*cnt;
{
	int	len;
	context;
	if (com && (len = strlen(com)))
	{
		int	min,
			max,
			pos,
			old_pos = -1,
			c;

		min = 1;
		max = NUMBER_OF_COMMANDS + 1;
		while (1)
		{
			pos = (max + min) / 2;
			if (pos == old_pos)
			{
				*cnt = 0;
				return (NULL);
			}
			old_pos = pos;
			c = strncmp(com, irc_command[pos].name, len);
			if (c == 0)
				break;
			else if (c > 0)
				min = pos;
			else
				max = pos;
		}
		*cnt = 0;
		(*cnt)++;
		min = pos - 1;
		while ((min > 0) && (strncmp(com, irc_command[min].name, len) == 0))
		{
			(*cnt)++;
			min--;
		}
		min++;
		max = pos + 1;
		while ((max < NUMBER_OF_COMMANDS + 1) && (strncmp(com, irc_command[max].name, len) == 0))
		{
			(*cnt)++;
			max++;
		}
		if (*cnt)
		{
			if (strlen(irc_command[min].name) == len)
				*cnt *= -1;
			else if (*cnt == 1 && irc_command[min].flags&NONOVICEABBREV && get_int_var(NOVICE_VAR))
			{
				bitchsay("As a novice you may not abbreviate the %s command", irc_command[min].name);
				*cnt=0;
				return NULL;
			}
			return (&(irc_command[min]));
		}
		else
			return NULL;
	}
	else
	{
		*cnt = -1;
		return (irc_command);
	}
}

#ifdef WANT_DLL
IrcCommandDll * find_dll_command(char *com, int *cnt)
{
	int	len;
	context;
	if (com && (len = strlen(com)))
	{
		int	min,
			max;
		IrcCommandDll *old, *old_next = NULL;
		
		*cnt = 0;
		min = 1;
		max = 0;
		for (old = dll_commands; old; old = old->next)
			max++;
		
		if (!max || !dll_commands)
			return NULL;
		old = dll_commands;
		while (1)
		{
			if (!my_strnicmp(com, old->name, len))
			{
				if (!old_next)
					old_next = old;
				(*cnt)++;
			}
			if (old->next == NULL)
				return (old_next);
			else
				old = old->next;
		}
	}
	else
	{
		*cnt = -1;
		return (NULL);
	}
}
#endif

/* IRCUSER command. Changes your userhost on the fly.  Takes effect
 * the next time you connect to a server 
 */
BUILT_IN_COMMAND(set_username)
{
        char *blah = next_arg(args, &args);
	context;
	if (blah && *blah)
	{
		if (!strcmp(blah, "-"))
			strmcpy(username, empty_string, NAME_LEN);
		else 
			strmcpy(username, blah, NAME_LEN);
		say("Username has been changed to '%s'",username);
	}
	else
		userage("IrcUser","text");
}

/* This code is courtesy of Richie B. (richie@morra.et.tudelft.nl) */
/*
 * REALNAME command. Changes the current realname. This will only be parsed
 * to the server when the client is connected again.
 */
BUILT_IN_COMMAND(realname_cmd)
{
	context;
        if (*args)
	{
                strmcpy(realname, args, REALNAME_LEN);
		say("Realname at next server connnection: %s", realname);
	}
	else
		userage("RealName", "[text of realname]");
}

BUILT_IN_COMMAND(pop_cmd)
{
        char *blah = function_pop(args);
	context;
        new_free(&blah);
}

BUILT_IN_COMMAND(push_cmd)
{
        char *blah = function_push(args);
	context;
        new_free(&blah);
}

BUILT_IN_COMMAND(shift_cmd)
{
        char *blah = function_shift(args);
	context;
        new_free(&blah);
}

BUILT_IN_COMMAND(unshift_cmd)
{
        char *blah = function_unshift(args);
	context;
        new_free(&blah);
}

BUILT_IN_COMMAND(do_forward)
{
	context;
	if (command && (!my_stricmp(command, "NOFORWARD") || !my_stricmp(command, "UNFORWARD")))
	{
		if (forwardnick)
		{
			bitchsay("No longer forwarding messages to %s", forwardnick);
		        send_to_server("NOTICE %s :%s is no longer forwarding to you",
				forwardnick, get_server_nickname(from_server));
		} 
		else
			userage(command? command : "Forward", "<nick\002|\002channel>");
		new_free(&forwardnick);
		return;
	}
	if (args && *args)
	{
		malloc_strcpy(&forwardnick, args);
		if (strchr(forwardnick, ' '))
			*strchr(forwardnick, ' ') = '\0';
		send_to_server("NOTICE %s :%s is now forwarding messages to you",
			forwardnick, get_server_nickname(from_server));
		bitchsay("Now forwarding messages to %s", forwardnick);
	}
	else
		userage(command? command : "Forward", "<nick\002|\002channel>");
	return;
}

BUILT_IN_COMMAND(auto_join)
{
AJoinList *new = NULL;
	context;
	if (command && *command && !my_stricmp(command, "AJoinList"))
	{
		int count = 0;
		for (new = ajoin_list; new; new = new->next)
		{
			if (!count)
				put_it("AJoin List");
			put_it("%10s %s", new->name, new->key?new->key:empty_string);
			count++;
		}
		if (count)
			put_it("End of AJoin List");
		return;
	}
	if (!args || !*args)
	{
		userage(command? command: "AJoin", "<channel>");
		return;
	}
	if (command && *command && !my_stricmp(command, "UNAJOIN"))
	{
		if ((new = (AJoinList *)remove_from_list((List **)&ajoin_list, args)) && new->ajoin_list)
		{
			new_free(&new->name);
			new_free(&new->key);
			new_free((char **)&new);
		}
		return;
	} 
	if ((new = (AJoinList *)find_in_list((List **) &ajoin_list, args, 1)) == NULL)
	{
		char *channel;
		char *key;
		channel = next_arg(args, &args);
		key = next_arg(args, &args);

		new = (AJoinList *) new_malloc(sizeof(AJoinList));
		malloc_sprintf(&new->name, "%s%s", *channel == '#' ? empty_string: "#", channel);
		if (key && *key)
			new->key = m_strdup(key);
		new->ajoin_list = 1;
		new->server = -1;
		add_to_list((List **) &ajoin_list, (List *)new);
	}
	if (from_server > -1 && server_list[from_server].chan_list)
	{
		bitchsay("Auto-Joining %s%s%s", new->name, new->key?space:empty_string, new->key?new->key:empty_string);
		send_to_server("JOIN %s %s", new->name, new->key? new->key:empty_string);
	}
}

BUILT_IN_COMMAND(do_oops)
{
	char 	*newmsg;

	context;
	if (args && *args && last_sent_msg_body)
	{
		newmsg = next_arg(args, &args);
		send_to_server("PRIVMSG %s :Oops, that /msg wasn't for you", sent_nick);
		send_to_server("PRIVMSG %s :%s", newmsg, last_sent_msg_body);
		if (window_display && do_hook(SEND_MSG_LIST, "%s %s", newmsg, last_sent_msg_body))
put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_MSG_VAR),"%s %s %s %s", update_clock(GET_TIME), newmsg, get_server_nickname(from_server), last_sent_msg_body));
	}
	else
		userage("oops", "<nick>");
	return;
}

/*
 * RECONNECT command. Closes the server, and then reconnects again.
 * Works also while connected to multiple servers. It only reconnects to the
 * current server number (which is stored in from_server). 
 * This command puts the REALNAME command in effect.
 */
BUILT_IN_COMMAND(reconnect_cmd)
{
	int server_number=from_server;
	char scommnd[4];
	context;
	if (from_server == -1)
	{
		bitchsay("Try connecting to a server first.");
		return;
	}
	if (do_hook(DISCONNECT_LIST, "Reconnecting to server"))
		say("Reconnecting to server %d", from_server);
	sprintf(scommnd, "+%i", from_server);
	close_server(from_server,(args && *args) ? args : "Reconnecting");
	clean_whois_queue();
	window_check_servers();
	clear_channel_list(server_number);
	server( NULL, scommnd, empty_string );
}

/* End of contributed code */

/* clear: the CLEAR command.  Figure it out */
BUILT_IN_COMMAND(my_clear)
{
	char	*arg;
	int	all = 0,
		unhold = 0;

	context;
	while ((arg = next_arg(args, &args)) != NULL)
	{
	/* -ALL and ALL here becuase the help files used to be wrong */
		if (!my_strnicmp(arg, "A", 1) || !my_strnicmp(arg+1, "A", 1))
			all = 1;
	/* UNHOLD */
		else if (!my_strnicmp(arg+1, "U", 1))
			unhold = 1;
		else
			userage("clear", "[-All\002|\002-UnHold]");
	}
	if (all)
		clear_all_windows(unhold);
	else
	{
		if (unhold)
			hold_mode(NULL, OFF, 1);
		clear_window_by_refnum(0);
	}
	update_input(UPDATE_JUST_CURSOR);
}


BUILT_IN_COMMAND(do_invite)
{
	char *inick;
	ChannelList *chan = NULL;
	int server = from_server;
	
	context;
	if (args && *args)
	{
		while(1)
		{
			inick = next_arg(args, &args);
			if (!inick)
				return;
			if (args && *args)
			{
				if (!is_channel(args) || !(chan = prepare_command(&server, args, NO_OP)))
					return;
			}
			else
				if (!(chan = prepare_command(&server, NULL, NO_OP)))
					return;
			
			if (!chan)
				return;
			my_send_to_server(server, "INVITE %s %s%s%s", inick, chan->channel, chan->key?" ":"", chan->key?chan->key:"");
		}
	}
	else
		userage("invite", "<nick>");
	return;
}

/*
   This isnt a command, its used by the wait command.  Since its extern,
   and it doesnt use anything static in this file, im sure it doesnt
   belong here.
 */
extern void io _((void));

void oh_my_wait _((void))
{
	int w_server = from_server;
	if (w_server == -1)
		w_server = primary_server;
	if (is_server_connected(w_server))
	{
		int local_waiting = ++waiting;
		send_to_server("%s", lame_wait_nick, waiting);
		while (local_waiting <= waiting)
			io();
	}
}

BUILT_IN_COMMAND(do_umode)
{
	context;
	send_to_server("%s %s %s", command, get_server_nickname(from_server), 
		(args && *args) ? args : empty_string);
}

BUILT_IN_COMMAND(do_getout)
{
        char    *channel = NULL;
ChannelList *chan;
int server = from_server;

	context;
        if (args)
                channel = next_arg(args, &args);
	if (!(chan = prepare_command(&server, channel, NO_OP)))	
		return;

	my_send_to_server(server, "PART %s", chan->channel);
}

BUILT_IN_COMMAND(do_unscrew)
{
        char    *channel = NULL;
	int server = from_server;
	ChannelList *chan;

	context;
        if (args && *args)
                channel = next_arg(args, &args);

	if (!(chan = prepare_command(&server, channel, NEED_OP)))
		return;

	my_send_to_server(server, "MODE %s -k %s", chan->channel, chan->key);
	my_send_to_server(server, "MODE %s +k \033(B\033[2J", chan->channel);
	my_send_to_server(server, "MODE %s -k \033(B\033[2J", chan->channel);
}

BUILT_IN_COMMAND(do_4op)
{
        char    *channel = NULL;
	char	*nick = NULL;
ChannelList *chan;
int	server = from_server;


	context;
        if (args && *args)
                channel = next_arg(args, &args);

	if (channel)
	{
		if (is_channel(channel))
			nick = args;
		else
		{
			nick = channel;
			channel = NULL;
		}
	}
	if (!(chan = prepare_command(&server, channel, NEED_OP)))
		return;

	if (!nick)
	{
		userage("4op", "<nick>");
		return;
	}
	my_send_to_server(server, "MODE %s +oooo %s %s %s %s", chan->channel, nick, nick, nick, nick);
}

BUILT_IN_COMMAND(do_scan)
{
int voice = 0, ops = 0, nops = 0, ircops = 0, all = 0;
char *channel = NULL;
ChannelList *chan;
NickList *nick;
char *s;
char *buffer = NULL;
int count = 0;
int server;
	context;
	if (command && !my_stricmp(command, "scanv"))
		voice = 1;
	else if (command && !my_stricmp(command, "scano"))
		ops = 1;
	else if (command && !my_stricmp(command, "scann"))
		nops = 1;
	else if (command && !my_stricmp(command, "scani"))
		ircops = 1;
	else 
		all = 1;

	while (args && *args)
	{
		s = next_arg(args, &args);
		if (is_channel(s))
			channel = s;
		else if (s && all)
		{
			all = 0;
			if (*s == 'v')
				voice = 1;
			else if (*s == 'o')
				ops = 1;
			else if (*s == 'n')
				nops = 1;
			else if (*s == 'i')
				ircops = 1;
		}
	}
	if (!(chan = prepare_command(&server, channel, NO_OP)))
		return;

	for (nick = chan->nicks; nick; nick = nick->next)
	{
		if (voice && nick->voice)
			count++;
		else if (ops && nick->chanop)
			count++;
		else if (nops && !nick->chanop)
			count++;
		else if (ircops && nick->ircop)
			count++;
		else if (all)
			count++;
	}
	
	if (voice) 
		s = get_string_var(FORMAT_NAMES_VOICE_VAR);
	else if (ops)
		s = get_string_var(FORMAT_NAMES_OP_VAR);
	else if (ops)
		s = get_string_var(FORMAT_NAMES_OP_VAR);
	else if (nops)
		s = get_string_var(FORMAT_NAMES_NONOP_VAR);
	else
		s = get_string_var(FORMAT_NAMES_VAR);
put_it("%s", convert_output_format(s, "%s %s %d %s", update_clock(GET_TIME), chan->channel, count, space));
	if (count)
	{
		count = 0;
		for (nick = chan->nicks; nick; nick = nick->next)
		{
			if (all && (nick->chanop || nick->voice))
				malloc_strcat(&buffer, convert_output_format(get_string_var(nick->chanop?FORMAT_NAMES_OPCOLOR_VAR:FORMAT_NAMES_VOICECOLOR_VAR),"%c %s",nick->chanop?'@':'+', nick->nick));
			else if (all)
				malloc_strcat(&buffer, convert_output_format(get_string_var(FORMAT_NAMES_NICKCOLOR_VAR),"%c %s",'$', nick->nick));
			else if (voice && nick->voice)
				malloc_strcat(&buffer, convert_output_format(get_string_var(FORMAT_NAMES_VOICECOLOR_VAR),"%c %s",'+', nick->nick));
			else if (ops && nick->chanop)
				malloc_strcat(&buffer, convert_output_format(get_string_var(FORMAT_NAMES_OPCOLOR_VAR),"%c %s",'@', nick->nick));
			else if (nops && !nick->chanop)
				malloc_strcat(&buffer, convert_output_format(get_string_var(FORMAT_NAMES_NICKCOLOR_VAR),"%c %s",'$', nick->nick));
			else if (ircops && nick->ircop)
				malloc_strcat(&buffer, convert_output_format(get_string_var(FORMAT_NAMES_OPCOLOR_VAR),"%c %s",'*', nick->nick));
			else 
				continue;
			malloc_strcat(&buffer, space);
			if (count++ == 4)
			{
				if (get_string_var(FORMAT_NAMES_BANNER_VAR))
					put_it("%s%s", convert_output_format(get_string_var(FORMAT_NAMES_BANNER_VAR), NULL, NULL), buffer);
				else
					put_it(buffer);
				new_free(&buffer);
				count = 0;
			}
		}
		if (count && buffer)
			if (get_string_var(FORMAT_NAMES_BANNER_VAR))
				put_it("%s%s", convert_output_format(get_string_var(FORMAT_NAMES_BANNER_VAR), NULL, NULL), buffer);
			else
				put_it(buffer);
		new_free(&buffer);
	}
}

BUILT_IN_COMMAND(do_mynames)
{
        char    *channel = NULL;
	int	server = from_server;
	ChannelList *chan;
	
	context;
        if (args)
                channel = next_arg(args, &args);

        if (!(chan = prepare_command(&server, channel, NO_OP)))
        	return;

	my_send_to_server(server, "NAMES %s", chan->channel);
}

BUILT_IN_COMMAND(my_whois)
{
        char    *channel = NULL;

	context;
        if (args && *args)
	{
                channel = next_arg(args, &args);
        	send_to_server("WHOIS %s %s", channel, channel);
	}
	else
		send_to_server("WHOIS %s %s", get_server_nickname(from_server),
			get_server_nickname(from_server));
}

BUILT_IN_COMMAND(do_unkey)
{
	char	*channel = NULL;
	int	server = from_server;
	ChannelList *chan;
	
	context;
	if (args)
		channel = next_arg(args, &args);
	if (!(chan = prepare_command(&server, channel, NEED_OP)))
		return;
	my_send_to_server(server, "MODE %s -k %s", chan->channel, chan->key);
}

#if 0
BUILT_IN_COMMAND(do_repeat)
{
	char *number;
	int num = 0;

	context;
	if ((number = next_arg(args, &args)))
	{
		num = atoi(number);
		if (num > 0 && args)
		{
			while (num--)
				parse_line(empty_string, args, args, 0, 0);
			return;
		}
	}
	userage("repeat", "<number> <command>");
}
#endif

/* pingcmd: ctcp ping, duh - phone, jan 1993. */
BUILT_IN_COMMAND(pingcmd)
{
	struct  timeval         tp;
	char	*to;
	char	buffer[BIG_BUFFER_SIZE + 1];
	int	ping_type = get_int_var(PING_TYPE_VAR);

	context;
	if (command && !my_stricmp(command, "uPING"))
		ping_type = 1;
	get_time(&tp);

	if ((to = next_arg(args, &args)) == NULL || !strcmp(to, "*"))
	{
		if ((to = get_channel_by_refnum(0)) == NULL)
			to = zero;
	}

	switch(ping_type)
	{
		case 1:
			sprintf(buffer, "%s PING %ld %ld", to, (long)tp.tv_sec,(long)tp.tv_usec);
			break;
		case 2:
			sprintf(buffer, "%s ECHO %ld %ld", to, (long)tp.tv_sec,(long)tp.tv_usec);
			break;
		default:
			sprintf(buffer, "%s PING %ld", to, time(NULL));
	}
	ctcp(command, buffer, empty_string);
}

BUILT_IN_COMMAND(ctcp_version)
{
char *person;
int type = 0;

	context;
	if ((person = next_arg(args, &args)) == NULL || !strcmp(person, "*"))
	{
		if ((person = get_channel_by_refnum(0)) == NULL)
			person = zero;
	}		
	if ((type = in_ctcp()) == -1)
		my_echo(NULL, "*** You may not use the CTCP command in an ON CTCP_REPLY!", empty_string);
	else
	{
		send_ctcp(type, person, CTCP_VERSION, NULL);
		put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_CTCP_VAR),
			"%s %s %s %s",update_clock(GET_TIME), person, "VERSION", NULL));
		malloc_strcpy(&last_sent_ctcp, convert_output_format(get_string_var(FORMAT_SEND_CTCP_VAR),
			"%s %s %s %s",update_clock(GET_TIME), person, "VERSION", NULL));
	}
}

BUILT_IN_COMMAND(do_offers)
{
char *person;
int type = 0;

	context;
	if ((person = next_arg(args, &args)) == NULL || !strcmp(person, "*"))
	{
		if ((person = get_channel_by_refnum(0)) == NULL)
			person = zero;
	}		
	if ((type = in_ctcp()) == -1)
		my_echo(NULL, "*** You may not use the CTCP command in an ON CTCP_REPLY!", empty_string);
	else
	{
		send_ctcp(type, person, CTCP_CDCC2, "%s", "LIST");
		put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_CTCP_VAR),
			"%s %s %s %s",update_clock(GET_TIME), person, "XDCC LIST", NULL));
		malloc_strcpy(&last_sent_ctcp, convert_output_format(get_string_var(FORMAT_SEND_CTCP_VAR),
			"%s %s %s %s",update_clock(GET_TIME), person, "XDCC LIST", NULL));
	}
}

/*ARGSUSED*/
BUILT_IN_COMMAND(ctcp)
{
	char	*to;
	char	*stag;
	int	tag;
	int	type;

	context;
	if ((to = next_arg(args, &args)) != NULL)
	{
		if (!strcmp(to, "*"))
			if ((to = get_channel_by_refnum(0)) == NULL)
				to = "0";

		if ((stag = next_arg(args, &args)) != NULL)
			tag = get_ctcp_val(upper(stag));
		else
			tag = CTCP_VERSION;

		if ((type = in_ctcp()) == -1)
			say("You may not use the CTCP command from an ON CTCP_REPLY!");
		else
		{
			if (args && *args)
				send_ctcp(type, to, tag, "%s", args);
			else
				send_ctcp(type, to, tag, NULL);
		}
	}
	else
		userage("Ctcp", "<nick|channel|*> <request>");
}

/*ARGSUSED*/
BUILT_IN_COMMAND(hook)
{
	context;
	if (*args)
		do_hook(HOOK_LIST, "%s", args);
	else
		userage("Hook", "<text>");
}

/*ARGSUSED*/
BUILT_IN_COMMAND(dcc)
{
	context;
	if (*args)
		process_dcc(args);
	else
		dcc_glist((char *) NULL);
}

BUILT_IN_COMMAND(deop)
{
	send_to_server("MODE %s -o", get_server_nickname(from_server));
}

BUILT_IN_COMMAND(funny_stuff)
{
	char	*arg,
		*stuff;
	int	min = 0,
		max = 0,
		flags = 0;
		
	context;
	stuff = empty_string;
	while ((arg = next_arg(args, &args)) != NULL)
	{
		if (*arg == '/' || *arg == '-')
		{
			if (my_strnicmp(arg+1, "MA", 2) == 0)	/* MAX */
			{
				if ((arg = next_arg(args, &args)) != NULL)
					max = atoi(arg);
			}
			else if (my_strnicmp(arg+1, "MI", 2) == 0) /* MIN */
			{
				if ((arg = next_arg(args, &args)) != NULL)
					min = atoi(arg);
			}
			else if (my_strnicmp(arg+1, "A", 1) == 0) /* ALL */
				flags &= ~(FUNNY_PUBLIC | FUNNY_PRIVATE);
			else if (my_strnicmp(arg+1, "PU", 2) == 0) /* PUBLIC */
			{
				flags |= FUNNY_PUBLIC;
				flags &= ~FUNNY_PRIVATE;
			}
			else if (my_strnicmp(arg+1, "PR", 2) == 0) /* PRIVATE */
			{
				flags |= FUNNY_PRIVATE;
				flags &= ~FUNNY_PUBLIC;
			}
			else if (my_strnicmp(arg+1, "T", 1) == 0)	/* TOPIC */
				flags |= FUNNY_TOPIC;
			else if (my_strnicmp(arg+1, "W", 1) == 0)	/* WIDE */
				flags |= FUNNY_WIDE;
			else if (my_strnicmp(arg+1, "U", 1) == 0)	/* USERS */
				flags |= FUNNY_USERS;
			else if (my_strnicmp(arg+1, "N", 1) == 0)	/* NAME */
				flags |= FUNNY_NAME;
			else
				stuff = arg;
		}
		else stuff = arg;
	}
	set_funny_flags(min, max, flags);
	if (strcmp(stuff, "*") == 0)
		if (!(stuff = get_channel_by_refnum(0)))
			stuff = empty_string;
	if (index(stuff, '*'))
	{
		funny_match(stuff);
		send_to_server("%s %s", command, empty_string);
	}
	else
	{
		funny_match(NULL);
		send_to_server("%s %s", command, stuff);
	}
#if 0	
		add_wait_prompt("Are You sure [N/y]", send_funny_stuff, command, WAIT_PROMPT_LINE);
#endif
}

BUILT_IN_COMMAND(waitcmd)
{
	int	w_index;
	char	*flag;
	char	*procindex;
	int	cmd = 0,
		len;
	char	buffer[BIG_BUFFER_SIZE + 1];

	context;
	while (args && (*args == '-' || *args == '/'))
	{
		flag = next_arg(args, &args);
		len = strlen(++flag);
		if (!my_strnicmp("C", flag, 1))
		{
			cmd = 1;
			break;
		}
		else
			yell("Unknown argument to WAIT: %s", flag);
	}
	if ((procindex = next_arg(args, &args)) && *procindex == '%' &&
			(w_index = get_process_index(&procindex)) != -1)
	{
		if (is_process_running(w_index))
		{
			if (cmd)
			{
				add_process_wait(w_index, args?args:empty_string);
				return;
			}
			else
			{
				set_input(empty_string);
				while (is_process_running(w_index))
					io();
			}
		}
		else
		{
			bitchsay("Not a valid process!");
			return;
		}
	}
	else if (cmd)
	{
		WaitCmd	*new;

		sprintf(buffer, "%s %s", procindex, args);
		new = (WaitCmd *) new_malloc(sizeof(WaitCmd));
		memset(new, 0, sizeof(WaitCmd));
		malloc_strcpy(&new->stuff, buffer);
		if (end_wait_list)
			end_wait_list->next = new;
		end_wait_list = new;
		if (!start_wait_list)
			start_wait_list = new;
		send_to_server("%s", wait_nick);
		return;
	}

	oh_my_wait();
}

int
check_wait_command(nick)
	char 	*nick;
{
	if (waiting && !strcmp(nick, lame_wait_nick))
	{
		waiting--;
		return 1;
	}
	if (start_wait_list && !strcmp(nick, wait_nick))
	{
		if (start_wait_list->stuff)
		{
			parse_line(NULL, start_wait_list->stuff, empty_string, 0, 0);
			new_free(&start_wait_list->stuff);
		}
		start_wait_list = start_wait_list->next;
		return 1;
	}
	return 0;
}

BUILT_IN_COMMAND(redirect)
{
	char	*who;

	context;
	if ((who = next_arg(args, &args)) != NULL)
	{
		if (!strcmp(who, "*") && !(who = get_channel_by_refnum(0)))
		{
			bitchsay("Must be on a channel to redirect to '*'");
			return;
		}

		if (!my_stricmp(who, get_server_nickname(from_server)))
		{
			bitchsay("You may not redirect output to yourself");
			return;
		}


 		if ((*who == '=') && !dcc_active(who + 1))
 		{
 			bitchsay("No active DCC CHAT:chat connection for %s", who+1);
			return;
 		} 
		window_redirect(who, from_server);
		server_list[from_server].sent = 0;
		parse_line(NULL, args, NULL, 0, 0);
		if (server_list[from_server].sent)
			send_to_server("%s %d", current_screen->redirect_token, current_screen->screennum);
		else
			window_redirect(NULL, from_server);
	}
	else
		userage("redirect", "<nick\002|\002channel\002|\002%%process> [cmd]");
}

BUILT_IN_COMMAND(sleepcmd)
{
	char	*arg;

	context;
	if ((arg = next_arg(args, &args)) != NULL)
		sleep(atoi(arg));
	else
		userage("sleep", "<time in seconds>");
}

/*
 * my_echo: simply displays the args to the screen, or, if it's XECHO,
 * processes the flags first, then displays the text on
 * the screen
 */
BUILT_IN_COMMAND(my_echo)
{
	unsigned int	display;
	int	lastlog_level = 0;
	int	from_level = 0;
	char	*flag_arg;
	int	temp;
	Window *old_to_window;
	
	old_to_window = to_window;
	context;
	if (command && *command == 'X')
	{
		while (args && (*args == '-' || *args == '/'))
		{
			flag_arg = next_arg(args, &args);
			switch(toupper(flag_arg[1]))
			{
				case 'C':
				{
					to_window = curr_scr_win;
					break;
				}

				case 'L':
				case 'l':
					if (!(flag_arg = next_arg(args, &args)))
						break;
					if ((temp = parse_lastlog_level(flag_arg)) != 0)
					{
						lastlog_level = set_lastlog_msg_level(temp);
						from_level = message_from_level(temp);
					}
					break;
				case 'w':
				case 'W':
					if (!(flag_arg = next_arg(args, &args)))
						break;
					if (isdigit(*flag_arg))
						to_window = get_window_by_refnum(atoi(flag_arg));
					else
						to_window = get_window_by_name(flag_arg);
					break;
			}
				if (!args)
					args = empty_string;
		}
	}
	display = window_display;
	window_display = 1;
	strip_ansi_in_echo = 0;
	put_it("%s%s", args, subargs?" ":"", subargs?subargs:"");
	strip_ansi_in_echo = 1;
	window_display = display;
	if (lastlog_level)
	{
		set_lastlog_msg_level(lastlog_level);
		message_from_level(from_level);
	}
	to_window = old_to_window;
}

/*
 */
static	void
oper_password_received(data, line)
	char	*data;
	char	*line;
{
	send_to_server("OPER %s %s", data, line);
}

/* oper: the OPER command.  */
BUILT_IN_COMMAND(oper)
{
	char	*password;
	char	*nick;

	oper_command = 1;
	if (!(nick = next_arg(args, &args)))
		nick = nickname;
	if (!(password = next_arg(args, &args)))
	{
		add_wait_prompt("Operator Password:",
			oper_password_received, nick, WAIT_PROMPT_LINE);
		return;
	}
	send_to_server("OPER %s %s", nick, password);
}

        
/* This generates a file of your ircII setup */
static	void
really_save(ircrc_file, line)
	char	*ircrc_file;
	char	*line;
{
	FILE	*fp;
	int	save_do_all = 0;

	context;
	if (*line != 'y' && *line != 'Y')
		return;
	if ((fp = fopen(ircrc_file, "w")) != NULL)
	{
		if (save_which & SFLAG_BIND)
			save_bindings(fp, save_do_all);
		if (save_which & SFLAG_ON)
			save_hooks(fp, save_do_all);
		if (save_which & SFLAG_NOTIFY)
			save_notify(fp);
		if (save_which & SFLAG_SET)
			save_variables(fp, save_do_all);
		if (save_which & SFLAG_ALIAS)
			save_aliases(fp, save_do_all);
		fclose(fp);
		bitchsay("IRCII settings saved to %s", ircrc_file);
	}
	else
		bitchsay("Error opening %s: %s", ircrc_file, sys_errlist[errno]);
}

/* Full scale abort.  Does a "save" into the filename in line, and
        then does a coredump */
BUILT_IN_COMMAND(abortcmd)
{
        char    *filename = next_arg(args, &args);

	context;
        filename = filename ? filename : "irc.aborted";
	save_which = SFLAG_ALIAS | SFLAG_BIND | SFLAG_ON | SFLAG_SET |
			     SFLAG_NOTIFY | SFLAG_DIGRAPH;
        really_save(filename, "y");
#ifdef ALLOC_DEBUG
        alloc_cmd("ALLOC", "d", empty_string);
#endif
        abort();
}

/* save_settings: saves the current state of IRCII to a file */
BUILT_IN_COMMAND(save_settings)
{
	char	*arg = NULL;
	int	all = 1;
	char buffer[BIG_BUFFER_SIZE + 1];

	context;
	save_which = save_do_all = 0;
	while ((arg = next_arg(args, &args)) != NULL)
	{
		if ('-' == *arg || '/' == *arg)
		{
			all = 0;
			arg++;
			if (0 == my_strnicmp("ALI", arg, 3))
				save_which |= SFLAG_ALIAS;
			else if (0 == my_strnicmp("AS", arg, 2))
				save_which |= SFLAG_ALIAS;
			else if (0 == my_strnicmp("B", arg, 1))
				save_which |= SFLAG_BIND;
			else if (0 == my_strnicmp("O", arg, 1))
				save_which |= SFLAG_ON;
			else if (0 == my_strnicmp("S", arg, 1))
				save_which |= SFLAG_SET;
			else if (0 == my_strnicmp("N", arg, 1))
				save_which |= SFLAG_NOTIFY;
			else if (0 == my_strnicmp("ALL", arg, 3))
				save_do_all = 1;
			else
			{
				userage("save", "<Alias\002|\002Assign\002|\002On\002|\002Set\002|\002Notify\002|\002All>");
				return;
			}
			continue;
		}
		if (!(arg = expand_twiddle(bircrc_file)))
		{
			bitchsay("Unknown user");
			return;
		}
	}
	if (all)
		save_which = SFLAG_ALIAS | SFLAG_BIND | SFLAG_ON | SFLAG_SET |
			     SFLAG_NOTIFY;
	if (dumb)
		really_save(arg?arg:bircrc_file, "y"); /* REAL dumb!  -lynx */
	else
	{
		sprintf(buffer, "Really write %s? ", arg?arg:bircrc_file);
		add_wait_prompt(buffer, really_save, arg?arg:bircrc_file, WAIT_PROMPT_LINE);
	}
}

/*
 * e_channel: does the channel command.  I just added displaying your current
 * channel if none is given 
 */
BUILT_IN_COMMAND(e_channel)
{
	char	*chan;
	int	len;
	char	*buffer=NULL;
	context;
	message_from(NULL, LOG_CURRENT);

	if ((chan = next_arg(args, &args)) != NULL)
	{
		len = strlen(chan);
		if (my_strnicmp(chan, "-i", 2) == 0)
		{
			if (invite_channel)
				send_to_server("%s %s %s", command, invite_channel, args);
			else
				bitchsay("You have not been invited to a channel!");
		}
		else
		{
			if (*chan == '#' || *chan == '&')
				malloc_strcpy(&buffer, chan);
			else
				buffer = m_sprintf("#%s", chan);
			if (is_on_channel(buffer, from_server, get_server_nickname(from_server)))
			{
				/* XXXX -
				   right here we want to check to see if the
				   channel is bound to this window.  if it is,
				   we set it as the default channel.  If it
				   is not, we warn the user that we cant do it
				 */
				if (is_bound_anywhere(buffer) &&
				    !(is_bound_to_window(curr_scr_win, buffer)))
					bitchsay("Channel %s is bound to another window", buffer);

				else
				{
					is_current_channel(buffer, from_server, 1);
					bitchsay("You are now talking to channel %s", 
						set_channel_by_refnum(0, buffer));
					update_all_windows();
				}
			}
			else
			{
				send_to_server("%s %s%s%s", command, buffer, args?" ":empty_string, args?args:empty_string);
				if (!is_bound(buffer, curr_scr_win->server))
					malloc_strcpy(&curr_scr_win->waiting_channel, buffer);
			}
		}
	}
	else
		list_channels();
	new_free(&buffer);
	message_from(NULL, LOG_CRAP);
}

/* comment: does the /COMMENT command, useful in .ircrc */
/*ARGSUSED*/
BUILT_IN_COMMAND(comment)
{
	/* nothing to do... */
}

/*
 * e_nick: does the /NICK command.  Records the users current nickname and
 * sends the command on to the server 
 */
/*ARGSUSED*/
BUILT_IN_COMMAND(e_nick)
{
	char	*nick;

	context;
	if (!(nick = next_arg(args, &args)))
	{
		bitchsay("Your nickname is %s", get_server_nickname(get_window_server(0)));
		return;
	}
	if ((nick = check_nickname(nick)) != NULL)
	{
		in_e_nick = 1;
		send_to_server("NICK %s", nick);
/*		in_e_nick = 0;*/
		set_string_var(AUTO_RESPONSE_VAR, nick);
		if (attempting_to_connect)
			set_server_nickname(get_window_server(0), nick);
	}
	else
		bitchsay("Bad nickname");
}

/* version: does the /VERSION command with some IRCII version stuff */
BUILT_IN_COMMAND(version1)
{
	char	*host;

	context;
	if ((host = next_arg(args, &args)) != NULL)
		send_to_server("%s %s", command, host);
	else
	{ 
		bitchsay("Client: ircII %s (internal version %s)", irc_version, internal_version);
		send_to_server("%s", command);
	}
}

#ifdef __STDC__
extern void e_hostname (char *command, char *args, char *subargs)
#else
extern void e_hostname (command, args, subargs)
	char	*command,
		*args;
	char	*subargs;
#endif
{
	struct hostent *hp;
	context;

	if (args && *args)
	{
		malloc_strcpy(&LocalHostName, args);
		if ((hp = gethostbyname(LocalHostName)))
			bcopy(hp->h_addr, (void *)&LocalHostAddr, sizeof(LocalHostAddr));

		bitchsay("Local host name is now %s", LocalHostName);
	} else
		bitchsay("Local Host Name is %s", (LocalHostName)? LocalHostName: hostname);
}

/*
 * info: does the /INFO command.  I just added some credits
 * I updated most of the text -phone, feb 1993.
 */
extern void display_bitchx _((void));
BUILT_IN_COMMAND(info)
{
	context;
	if (!args || !*args)
	{
		display_bitchx();
		return;
	}
	send_to_server("%s %s", command, args);
}

void
ison_now(notused, nicklist)
	char		*notused;
	char		*nicklist;
{
	context;
	if (do_hook(current_numeric, "%s", nicklist))
		put_it("%s Currently online: %s", numeric_banner(), nicklist);
}

BUILT_IN_COMMAND(ison)
{
	context;
	if (!args[strspn(args, space)])
		args = get_server_nickname(from_server);
	add_ison_to_whois(args, ison_now);

}

/*
 * userhost: Does the USERHOST command.  Need to split up the queries,
 * since the server will only reply to 5 at a time.
 */
BUILT_IN_COMMAND(userhost)
{
	int	n = 0,
		total = 0,
		userhost_cmd = 0;
	char	*nick;
	char    buffer[BIG_BUFFER_SIZE+1];

	context;
	while ((nick = next_arg(args, &args)) != NULL)
	{
		int	len;
		char	*body;
		
		++total;
		len = strlen(nick);
		if ((*nick == '-' || *nick =='/') && !my_strnicmp(nick+1, "C", 1))
		{
			if (total < 2)
			{
				userage("userhost", "<-cmd> <nick>");
				return;
			}
			userhost_cmd = 1;
			while (my_isspace(*args))
				args++;
 			body = next_expr_failok(&args, '{');
			if (body)
				args = body;
			break;
		}
		else
		{
			if (n++)
				strmcat(buffer, space, BIG_BUFFER_SIZE);
			else
				*buffer = '\0';
			strmcat(buffer, nick, BIG_BUFFER_SIZE);
		}
	}
	if (n)
	{
		char	*the_list = NULL;
		char	*s, *t;
		int	i;

		malloc_strcpy(&the_list, buffer);
		s = t = the_list;
		while (n)
		{
			for (i = 5; i && *s; s++)
				if (isspace(*s))
					i--, n--;
			if (isspace(s[-1]))
				s[-1] = '\0';
			else
				n--;
			strcpy(buffer, t);
			t = s;

			if (userhost_cmd)
				add_to_whois_queue(buffer, userhost_cmd_returned, "%s", args);
			else
				add_to_whois_queue(buffer, USERHOST_USERHOST, "%s", empty_string);
		}
		new_free(&the_list);
	}
	else if (!total)
		/* Default to yourself.  */
		add_to_whois_queue(get_server_nickname(from_server), USERHOST_USERHOST, "%s", get_server_nickname(from_server));
}

/*
 * whois: the WHOIS and WHOWAS commands.  This translates the 
 * to the whois handlers in whois.c 
 */
BUILT_IN_COMMAND(whois)
{
	char *stuff = NULL;

	context;
	if (!my_stricmp(command, "WHOWAS"))
	{
		char *word_one = next_arg (args, &args);
		if (args && *args)
			malloc_sprintf(&stuff, "%s %s", word_one, args);
		else if (word_one && *word_one)
			malloc_sprintf(&stuff, "%s %d", word_one, /*get_int_var(NUM_OF_WHOWAS_VAR)*/ 4);
		else
			malloc_sprintf(&stuff, "%s %d", get_server_nickname(from_server), /*get_int_var(NUM_OF_WHOWAS_VAR)*/ 4);

		send_to_server("WHOWAS %s", stuff);
		new_free(&stuff);
	}
	else /* whois command */
		send_to_server("WHOIS %s", args && *args ? args : get_server_nickname(from_server));
}

int doing_who = 0;
/*
 * who: the /WHO command. Parses the who switches and sets the who_mask and
 * whoo_stuff accordingly.  Who_mask and who_stuff are used in whoreply() in
 * parse.c 
 */
BUILT_IN_COMMAND(who)
{
	char	*arg,
		*channel = NULL;
	int	no_args = 1,
		len;

	if (who_on_join)
		return;
	if (doing_who)
		oh_my_wait();
	context;

	doing_who = 1;

	who_mask = 0;
	new_free(&who_name);
	new_free(&who_host);
	new_free(&who_server);
	new_free(&who_file);
	new_free(&who_nick);
	new_free(&who_real);
	while ((arg = next_arg(args, &args)) != NULL)
	{
		lower(arg);
		no_args = 0;
		if ((*arg == '-' || *arg == '/') && (!isdigit(*(arg + 1))))
		{
			arg++;
			if ((len = strlen(arg)) == 0)
			{
				userage(command, "-<Ops\002|\002Lusers\002|\002Chops\002|\002Hosts\002|\002Here\002|\002Away\002|\002Servers\002|\002Names\002|\002Real\002|\002nick\002|\002file> [requested info]");
				return;
			}
			if (strncmp(arg, "o", 1) == 0) /* OPS */
				who_mask |= WHO_OPS;
			else if (strncmp(arg, "l", 1) == 0) /* LUSERS */
				who_mask |= WHO_LUSERS;
			else if (strncmp(arg, "c", 1) == 0) /* CHOPS */
				who_mask |= WHO_CHOPS;
			else if (strncmp(arg, "ho", 2) == 0) /* HOSTS */
			{
				if ((arg = next_arg(args, &args)) != NULL)
				{
					who_mask |= WHO_HOST;
					malloc_strcpy(&who_host, arg);
					channel = who_host;
				}
				else
				{
					userage(command, "<-Hosts> <requested info>");
					return;
				}
			}
			else if (strncmp(arg, "he", 2) ==0) /* here */
				who_mask |= WHO_HERE;
			else if (strncmp(arg, "a", 1) ==0) /* away */
				who_mask |= WHO_AWAY;
			else if (strncmp(arg, "s", 1) == 0) /* servers */
			{
				if ((arg = next_arg(args, &args)) != NULL)
				{
					who_mask |= WHO_SERVER;
					malloc_strcpy(&who_server, arg);
					channel = who_server;
				}
				else
				{
					userage(command, "<-Servers> <requested info>");
					return;
				}
			}
			else if (strncmp(arg, "na", 2) == 0) /* names */
			{
				if ((arg = next_arg(args, &args)) != NULL)
				{
					who_mask |= WHO_NAME;
					malloc_strcpy(&who_name, arg);
					channel = who_name;
				}
				else
				{
					userage(command, "<-Name> <requested info>");
					return;
				}
			}
			else if (strncmp(arg, "r", 1) == 0) /* real name */
			{
				if ((arg = next_arg(args, &args)) != NULL)
				{
					who_mask |= WHO_REAL;
					malloc_strcpy(&who_real, arg);
					channel = who_real;
				}
				else
				{
					userage(command, "<-RealName> <requested info>");
					return;
				}
			}
			else if (strncmp(arg, "ni", 2) == 0) /* nick */
			{
				if ((arg = next_arg(args, &args)) != NULL)
				{
					who_mask |= WHO_NICK;
					malloc_strcpy(&who_nick, arg);
					channel = who_nick;
				}
				else
				{
					userage(command, "<-Nick> <requested info>");
					return;
				}
				/* WHO -FILE by Martin 'Efchen' Friedrich */
			}
			else if (strncmp(arg, "f", 1) == 0) /* file */
			{
				who_mask |= WHO_FILE;
				if ((arg = next_arg(args, &args)) != NULL)
				{
					malloc_strcpy(&who_file, arg);
				}
				else
				{
					userage(command, "<-File> <requested info>");
					return;
				}
			}
			else
			{
				userage(command, "-<Ops\002|\002Lusers\002|\002Chops\002|\002Hosts\002|\002Here\002|\002Away\002|\002Servers\002|\002Names\002|\002Real\002|\002nick\002|\002file> [requested info]");
				return;
			}
		}
		else if (strcmp(arg, "*") == 0)
		{
			channel = get_channel_by_refnum(0);
			if (!channel || *channel == '0')

			{
				not_on_a_channel(curr_scr_win);
				return;
			}
		}
		else
			channel = arg;
	}
	if (no_args)
	{
		channel = get_channel_by_refnum(0);
		if (!channel || !*channel)
		{
			not_on_a_channel(curr_scr_win);
			return;
		}
		send_to_server("WHO %s", channel);
		return;
	}
	if (no_args)
		bitchsay("No arguements specified");
	else
	{
		if (!channel && who_mask & WHO_OPS)
			channel = "*.*";
		send_to_server("%s %s %c", "WHO", channel ? channel :
				empty_string, (who_mask & WHO_OPS) ?
					'o' : '\0');
	}
	
}

/*
 * query: the /QUERY command.  Works much like the /MSG, I'll let you figure
 * it out.
 */
/*ARGSUSED*/
BUILT_IN_COMMAND(query)
{
	char	*nick,
		*rest;

	context;
	message_from(NULL, LOG_CURRENT);
	if ((nick = next_arg(args, &rest)) != NULL)
	{
		if (strcmp(nick, ".") == 0)
		{
			if (!(nick = sent_nick))
			{
				bitchsay("You have not messaged anyone yet");
				return;
			}
		}
		else if (strcmp(nick, ",") == 0)
		{
			if (!(nick = recv_nick))
			{
				bitchsay("You have not recieved a message from \
						anyone yet");
				return;
			}
		}
		else if ((strcmp(nick, "*") == 0) && !(nick = get_channel_by_refnum(0)))
		{
			bitchsay("You are not on a channel");
			return;
		}

		if (*nick == '%')
		{
#ifndef WINNT
			if (is_process(nick) == 0)
#endif
			{
				bitchsay("Invalid processes specification");
				message_from(NULL, LOG_CRAP);
				return;
			}
		}
		bitchsay("Starting conversation with %s", nick);
		set_query_nick(nick);
	}
	else
	{
		if (query_nick())
		{
			bitchsay("Ending conversation with %s", query_nick());
			set_query_nick(NULL);
		}
		else
			userage("query", "[Nickname\002|\002none to cancel]");
	}
	update_input(UPDATE_ALL);
	message_from(NULL, LOG_CRAP);
}

void read_away_log(char *stuff, char *line)
{
	context;
	if (!line || !*line || (line && toupper(*line) == 'Y'))
		readlog(NULL, NULL, NULL);
	update_input(UPDATE_ALL);
}

BUILT_IN_COMMAND(back)
{
int i = 0;
char *tmp = NULL;
int minutes = 0;
int hours = 0;
int seconds = 0;

	context;
	if (!curr_scr_win || curr_scr_win->server == -1)
		return;

	new_free(&(server_list[curr_scr_win->server].away));
	if (server_list[curr_scr_win->server].awaytime)
	{
		ChannelList *chan;
		time_t current_t = time(NULL) - server_list[curr_scr_win->server].awaytime;
		int old_server = from_server;
		from_server = curr_scr_win->server;
		
		hours = current_t / 3600;
		minutes = (current_t - (hours * 3600)) / 60;
		seconds = current_t % 60;
		

		if (get_string_var(FORMAT_BACK_VAR) && get_int_var(SEND_AWAY_MSG_VAR))
		{
			bitchsay("You were /away for %i hours %i minutes and %i seconds. [\002BX\002-MsgLog %s]",
				hours, minutes, seconds,
				on_off(get_int_var(MSGLOG_VAR)));
			for (chan = server_list[curr_scr_win->server].chan_list; chan; chan = chan->next)
			{
/*
				if (is_current_channel(chan->channel, curr_scr_win->server, 0))
					me(NULL, stripansicodes(convert_output_format(get_string_var(FORMAT_BACK_VAR), "%s %d %d %d %s",
						update_clock(GET_TIME),
						minutes + (hours * 60), seconds, 
					        get_int_var(MSGCOUNT_VAR), args?args: server_list[curr_scr_win->server].away)), NULL);
				else
*/
					send_to_server("PRIVMSG %s :ACTION %s", chan->channel, 
						stripansicodes(convert_output_format(get_string_var(FORMAT_BACK_VAR), "%s %d %d %d %s",
							update_clock(GET_TIME),
							minutes + (hours * 60), seconds, 
						        get_int_var(MSGCOUNT_VAR), args?args:server_list[curr_scr_win->server].away)));
			}
		}
		send_to_server("%s", "AWAY");
		from_server = old_server;
	}
	if (get_int_var(MSGLOG_VAR))
	{
		log_toggle(0);
	}
	server_list[curr_scr_win->server].awaytime = (time_t)0;
	away_set = 0;
	for(i = 0; i < number_of_servers; i++)
	{
		if (server_list[i].read != -1 && server_list[i].away)
			away_set = 1;
	}
	if (get_string_var(FORMAT_BACK_VAR) && get_int_var(MSGLOG_VAR))
	{
		malloc_sprintf(&tmp, " read /away msgs (%d msg%s) log [Y/n]? ", get_int_var(MSGCOUNT_VAR), plural(get_int_var(MSGCOUNT_VAR)));
		add_wait_prompt(tmp, read_away_log, empty_string, WAIT_PROMPT_LINE); 
		new_free(&tmp);
	}
	set_int_var(MSGCOUNT_VAR, 0);
	update_all_status(curr_scr_win, NULL, 0);
}

/*
 * away: the /AWAY command.  Keeps track of the away message locally, and
 * sends the command on to the server.
 */
BUILT_IN_COMMAND(away)
{
	int	len;
	char	*arg = NULL;
	int	flag = AWAY_ONE;
	int	i;

	context;
	if (*args)
	{
		if ((*args == '-') || (*args == '/'))
		{
			arg = index(args, ' ');
			if (arg)
				*arg++ = '\0';
			else
				arg = empty_string;
			len = strlen(args);
			if (0 == my_strnicmp(args+1, "A", 1)) /* all */
			{
				flag = AWAY_ALL;
				args = arg;
			}
			else if (0 == my_strnicmp(args+1, "O", 1))/* one */
			{
				flag = AWAY_ONE;
				args = arg;
			}
			else
			{
				userage(command?command:"Away", "<-All\002|\002One>");
				return;
			}
		}
	}
	if (flag == AWAY_ALL)
	{
		if (*args)
		{
			away_set = 1;
			MarkAllAway(command, args, subargs);
		}
		else
		{
			away_set = 0;
			MarkAllAway(command, NULL, NULL);
			for(i = 0; (i < number_of_servers); i++)
				if (server_list[i].whois_stuff.away)
					new_free(&(server_list[i].away));
		}
	}
	else
	{
		if (*args)
		{
			away_set = 1;
			if (!curr_scr_win || curr_scr_win->server == -1)
				return;
			malloc_strcpy(&(server_list[curr_scr_win->server].away), args);
			if (!server_list[curr_scr_win->server].awaytime)
				server_list[curr_scr_win->server].awaytime = time(NULL);
			if (get_int_var(MSGLOG_VAR))
				log_toggle(1);
			if (get_string_var(FORMAT_AWAY_VAR) && get_int_var(SEND_AWAY_MSG_VAR))
			{
				ChannelList *chan;
				int old_server = from_server;
				from_server = curr_scr_win->server;
				for (chan = server_list[curr_scr_win->server].chan_list; chan; chan = chan->next)
				{
						send_to_server("PRIVMSG %s :ACTION %s", chan->channel, 
						stripansicodes(convert_output_format(get_string_var(FORMAT_AWAY_VAR), "%s [\002BX\002-MsgLog %s] %s",update_clock(GET_TIME), get_int_var(MSGLOG_VAR)?"On":"Off", args)));
				}
				from_server = old_server;
			}
			if (get_string_var(FORMAT_AWAY_VAR))
				send_to_server("%s :%s", "AWAY", stripansicodes(convert_output_format(get_string_var(FORMAT_AWAY_VAR), "%s [\002BX\002-MsgLog %s] %s", update_clock(GET_TIME), get_int_var(MSGLOG_VAR)?"On":"Off",args)));
			else
				send_to_server("%s :%s", "AWAY", stripansicodes(convert_output_format(args, NULL)));
			put_it("%s",convert_output_format(get_string_var(FORMAT_AWAY_VAR), "%s [\002BX\002-MsgLog %s] %s",update_clock(GET_TIME), get_int_var(MSGLOG_VAR)?"On":"Off", args));
		}
		else
		{
			back("BACK", args, NULL);
			set_int_var(MSGCOUNT_VAR, 0);
		}
	}
	update_all_status(curr_scr_win, NULL, 0);
}

#if 0
#ifndef RAND_MAX
#define RAND_MAX 213234444
#endif
#endif

static void real_quit _((char *dummy, char *ptr))
{
	context;
	if (ptr && *ptr)
	{
		if (*ptr == 'Y' || *ptr == 'y')
		{
			irc_exit(1, "%s", dummy);
		}
	}
	bitchsay("Excelllaaant!!");
}

/* e_quit: The /QUIT, /EXIT, etc command */
BUILT_IN_COMMAND(e_quit)
{
	int	old_server = from_server;
	char	*Reason;
	DCC_list *Client;
	int active_dcc = 0;
	
	context;
	if (args && *args)
		Reason = args;
	else
		Reason = get_signoffreason(nickname);
	if (!Reason || !*Reason)
		Reason = (char *)irc_version;
	
	for (Client = ClientList; Client; Client = Client->next)
		if (((Client->flags & DCC_TYPES) ==DCC_REGETFILE) || ((Client->flags & DCC_TYPES) ==DCC_FILEREAD) || ((Client->flags & DCC_TYPES) ==DCC_FILEOFFER) || ((Client->flags & DCC_TYPES) ==DCC_RESENDOFFER))
			active_dcc++;
	if (active_dcc)
		add_wait_prompt("Active DCC's. Really Quit [y/N] ? ", real_quit, Reason, WAIT_PROMPT_LINE);
	else
	{
		from_server = old_server;
		irc_exit(1, "%s", Reason);
	}
}

/* flush: flushes all pending stuff coming from the server */
BUILT_IN_COMMAND(flush)
{
	context;
	if (get_int_var(HOLD_MODE_VAR))
	{
		while (curr_scr_win->held_lines)
			remove_from_hold_list(curr_scr_win);
		hold_mode(NULL, OFF, 1);
	}
	bitchsay("Standby, Flushing server output...");
	flush_server();
	bitchsay("Done");
}

/* e_wall: used for WALL and WALLOPS */
BUILT_IN_COMMAND(e_wall)
{
	context;
	if ((!args || !*args))
	{
		userage(command, "<message>");
		return;
	}
	message_from(NULL, LOG_WALLOP);
	if (!in_on_who)
		send_to_server("%s :%s", command, args);
	if (!get_server_operator(current_screen->current_window->server))
	{
		static int warned = 0;
		if (!warned)
		{
			say("WARNING: Using the /WALLOPS command as a non-oper");
			warned = 1;
		}
	}
	if (get_server_flag(from_server, USER_MODE_W))
		put_it("!! %s", args);
	malloc_sprintf(&last_sent_wall, "!! %s", args);
	message_from(NULL, LOG_CRAP);
}

/*
 * e_privmsg: The MSG command, displaying a message on the screen indicating
 * the message was sent.  Also, this works for the NOTICE command. 
 */
BUILT_IN_COMMAND(e_privmsg)
{
	char	*nick;

	context;
	if ((nick = next_arg(args, &args)) != NULL)
	{
		if (strcmp(nick, ".") == 0)
		{
			if (!(nick = sent_nick))
			{
				bitchsay("You have not sent a message to anyone yet");
				return;
			}
		}
		else if (strcmp(nick, ",") == 0)
		{
			if (!(nick = recv_nick))
			{
				bitchsay("You have not received a message from anyone yet");
				return;
			}
		}
		else if (!strcmp(nick, "*") && (!(nick = get_channel_by_refnum(0))))
				nick = "0";
		send_text(nick, args, command, 1, 1);
	}
	else
		userage(command, "<nickname\002|\002channel\002|\002*\002|\002,\002|\002.> <text>");
}

BUILT_IN_COMMAND(dcc_crash)
{
char *nick;
int old_server;
	context;
	if ((nick = next_arg(args, &args)) != NULL)
	{
		if (*nick == '=') /* DCC chat */
		{
			old_server = from_server;
			from_server = -1;
			dcc_chat_crash_transmit(nick + 1, args);
			from_server = old_server;
		}
	}
}

/*
 * quote: handles the QUOTE command.  args are a direct server command which
 * is simply send directly to the server 
 */
BUILT_IN_COMMAND(quote)
{
	context;
	if (!in_on_who && !doing_privmsg && args && *args)
		send_to_server("%s", args);
}

/*
 * send_comm: the generic command function.  Uses the full command name found
 * in 'command', combines it with the 'args', and sends it to the server 
 */
BUILT_IN_COMMAND(send_comm)
{
	context;
	if (args && *args)
		send_to_server("%s %s", command, args);
	else
		send_to_server("%s", command);
}

BUILT_IN_COMMAND(send_topic)
{
	char	*arg = NULL;
	char	*arg2;
	ChannelList *chan;
	int server;
	int unset = 0;	
	
	context;
	arg = next_arg(args, &args);
#if 0
		if (!strcmp("*", arg))
			arg = get_channel_by_refnum(0);
		if (!arg) 
		{
			bitchsay("You're not on a channel!");
			return;
		}
#endif
	if (!my_stricmp(command, "UNTOPIC"))
		unset = 1;
	if (!(chan = prepare_command(&server, arg?(is_channel(arg)?arg:NULL):NULL, NO_OP)))
		return;
	if (unset)
	{
		my_send_to_server(server, "TOPIC %s :", chan->channel);
		return;
	}
	if (arg && (!(chan->mode&MODE_TOPIC) || chan->chop))
	{
		if (is_channel(arg))
		{
			malloc_strcpy(&last_sent_topic, args);
			if ((arg2 = next_arg(args, &args)))
				my_send_to_server(server, "%s %s :%s %s", command, chan->channel, arg2, args);
			else
				my_send_to_server(server, "%s %s", command, chan->channel);
		} 
		else
		{
			my_send_to_server(server, "%s %s :%s%s%s", command, chan->channel, arg, args?space:empty_string, args?args:empty_string);
			malloc_sprintf(&last_sent_topic, "%s%s%s", arg, args?space:empty_string, args?args:empty_string);
		}
	}
	else
		my_send_to_server(server, "%s %s", command, chan->channel);
}

BUILT_IN_COMMAND(send_2comm)
{
	char	*reason = NULL;

	context;
#ifdef CDE
	args = next_arg(args, &reason);
	if (!args)
		args = empty_string;
#endif

	if ((reason = index(args, ' ')) != NULL)
		*reason++ = '\0';
	else
		reason = empty_string;

	if (!strcmp(args, "*"))
		args = get_channel_by_refnum(0);
	if (!args || !*args)
		args = "*";     /* what-EVER */

	if (reason && *reason)
		send_to_server("%s %s :%s", command, args, reason);
	else
		send_to_server("%s %s", command, args);
}

/*
 * send_kick: sends a kick message to the server.  Works properly with
 * kick comments.
 */

BUILT_IN_COMMAND(send_kick)
{
	char	*kickee,
		*comment,
		*channel = NULL;
ChannelList *chan;
int server = from_server;
 	
	context;
        if (!(channel = next_arg(args, &args)))
        {
		userage("kick", "<channel\002|\002*> <nick> [comment]");
		return;
        }
        
        if (!(kickee = next_arg(args, &args)))
        {
		userage("kick", "<channel\002|\002*> <nick> [comment]");
 		return;
        }

        comment = args?args:get_reason(kickee);

	if (!(chan = prepare_command(&server, (channel&&!strcmp(channel, "*"))?NULL:channel, NEED_OP)))
		return;

	my_send_to_server(server, "%s %s %s :%s", command, channel, kickee, comment);
}

BUILT_IN_COMMAND(send_mode)
{
char *channel;
	context;
	if ((channel = get_channel_by_refnum(0)))
		send_to_server("%s %s %s", command, channel, args? args: empty_string);
}

/*
 * send_channel_com: does the same as send_com for command where the first
 * argument is a channel name.  If the first argument is *, it is expanded
 * to the current channel (a la /WHO).
 */
BUILT_IN_COMMAND(send_channel_com)
{
	char	*ptr,
		*s;
	context;
        ptr = next_arg(args, &args);

	if (ptr && !strcmp(ptr, "*"))
	{
		if ((s = get_channel_by_refnum(0)) != NULL)
			send_to_server("%s %s %s", command, s, args?args:empty_string);
		else
			say("%s * is not valid since you are not on a channel", command);
	}
	else if (ptr)
			send_to_server("%s %s %s", command, ptr, args?args:empty_string);
	else
		userage(command, "<*\002|\002#channel> <arguments>");
}

struct target_type
{
	char *nick_list;
	char *message;
	int  hook_type;
	char *command;
	char *format;
	int  level;
	char *output;
	char *other_output;
};

int current_target = 0;

/*
 * The whole shebang.
 *
 * The message targets are parsed and collected into one of 4 buckets.
 * This is not too dissimilar to what was done before, except now i 
 * feel more comfortable about how the code works.
 *
 * Bucket 0 -- Unencrypted PRIVMSGs to nicknames
 * Bucket 1 -- Unencrypted PRIVMSGs to channels
 * Bucket 2 -- Unencrypted NOTICEs to nicknames
 * Bucket 3 -- Unencrypted NOTICEs to channels
 *
 * All other messages (encrypted, and DCC CHATs) are dispatched 
 * immediately, and seperately from all others.  All messages that
 * end up in one of the above mentioned buckets get sent out all
 * at once.
 */
#ifdef __STDC__
extern	void 	send_text(char *nick_list, char *text, char *command, int hook, int log)
#else
extern	void 	send_text (nick_list, text, command, hook, log)
	char 	*nick_list, 
		*text, 
		*command;
	int 	hook;
	int	log;
#endif
{
	int i, af, old_server;
	int lastlog_level;
	int not_done = 0;
	int is_current = 0;
static	int recursion = 0;	
	char *current_nick, *next_nick, *free_nick;
	char *line, *key, *copy = NULL;
	int old_window_display = window_display;
	        
struct target_type target[4] = 
{	
	{NULL, NULL, SEND_MSG_LIST,     "PRIVMSG", "*%s*> %s" , LOG_MSG, NULL, NULL },
	{NULL, NULL, SEND_PUBLIC_LIST,  "PRIVMSG", "%s> %s"   , LOG_PUBLIC, NULL, NULL },
	{NULL, NULL, SEND_NOTICE_LIST,  "NOTICE",  "-%s-> %s" , LOG_NOTICE, NULL, NULL }, 
	{NULL, NULL, SEND_NOTICE_LIST,  "NOTICE",  "-%s-> %s" , LOG_NOTICE, NULL, NULL }
};

	context;

	target[0].output = get_string_var(FORMAT_SEND_MSG_VAR);
	target[1].output = get_string_var(FORMAT_SEND_PUBLIC_VAR);
	target[1].other_output = get_string_var(FORMAT_SEND_PUBLIC_OTHER_VAR);
	target[2].output = get_string_var(FORMAT_SEND_NOTICE_VAR);
	target[3].output = get_string_var(FORMAT_SEND_NOTICE_VAR);
	target[3].other_output = get_string_var(FORMAT_SEND_NOTICE_VAR);

	if (recursion)
		hook = 0;
	window_display = hook;
	recursion++;
	free_nick = next_nick = m_strdup(nick_list);

	while ((current_nick = next_nick))
	{
		new_free(&copy);
		if ((next_nick = index(current_nick, ',')))
			*next_nick++ = 0;

		if (!*current_nick)
			continue;

		if (*current_nick == '%')
		{
			if ((i = get_process_index(&current_nick)) == -1)
				say("Invalid process specification");
			else
				text_to_process(i, text, 1);
		}
		/*
		 * This test has to be here because it is legal to
		 * send an empty line to a process, but not legal
		 * to send an empty line anywhere else.
		 */
		else if (!text || !*text)
			;
		else if (*current_nick == '@')
			say("DCC TALK not supported.");
		else if (*current_nick == '"')
			send_to_server("%s", text);
		else if (*current_nick == '/')
		{
			line = new_malloc(strlen(current_nick) + strlen(text) + 2);
			strcpy(line, current_nick);
			strcat(line, space);
			strcat(line, text);
			parse_inline(line, NULL, &af);
			new_free(&line);
		}
		else if (*current_nick == '=')
		{

			if (!dcc_active(current_nick + 1) && !dcc_activebot(current_nick+1))
			{
				yell("No DCC CHAT connection open to %s", current_nick + 1);
				continue;
			}

			copy = m_strdup(text);
			if ((key = is_crypted(current_nick)) != 0)
				line = crypt_msg(text, key);
			else
				line = m_strdup(text);

			old_server = from_server;
			from_server = -1;

			if (hook && do_hook(SEND_DCC_CHAT_LIST, "%s %s", current_nick + 1, copy))
put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_DCC_CHAT_VAR), "%c %s %s", '=', current_nick+1, copy));
			if (dcc_active(current_nick+1))
			{
				dcc_chat_transmit(current_nick + 1, line, command);
				addtabkey(current_nick, 0);
			}
			else
				dcc_bot_transmit(current_nick + 1, line, command);
			from_server = old_server;
			new_free(&line);
			new_free(&copy);
		}
		else
		{

			if (doing_notice)
			{
				say("You cannot send a message from within ON NOTICE");
				continue;
			}

			copy = m_strdup(text);

			if (!(i = is_channel(current_nick)))
			{
				addtabkey(current_nick, 0);
			}		
				
			if (doing_notice || (command && !strcmp(command, "NOTICE")))
				i += 2;

			if ((key = is_crypted(current_nick)))
			{
				lastlog_level = set_lastlog_msg_level(target[i].level);
				message_from(current_nick, target[i].level);

				line = crypt_msg(text, key);
				if (hook && do_hook(target[i].hook_type, "%s %s", current_nick, copy))
	put_it("%s", convert_output_format(get_string_var(target[i].hook_type == SEND_MSG_LIST?FORMAT_ENCRYPTED_PRIVMSG_VAR:FORMAT_ENCRYPTED_NOTICE_VAR),
		"%s %s %s %s",update_clock(GET_TIME), get_server_nickname(from_server),current_nick, copy));

				send_to_server("%s %s :%s", target[i].command, current_nick, line);
				new_free(&line);

				set_lastlog_msg_level(lastlog_level);
			}
			else
			{
				if (target[i].nick_list)
					malloc_strcat(&target[i].nick_list, ",");
				malloc_strcat(&target[i].nick_list, current_nick);
				if (!target[i].message)
					target[i].message = text;
			}
		}
		if (get_int_var(WARN_OF_IGNORES_VAR) && (check_ignore(current_nick, NULL, current_nick, IGNORE_MSGS) == IGNORED))
			bitchsay("Warning: You are ignoring private messages from %s", current_nick);
	}
	
	new_free(&copy);
	for (i = 0; i < 4; i++)
	{
		if (!target[i].message)
			continue;

		lastlog_level = set_lastlog_msg_level(target[i].level);
		message_from(target[i].nick_list, target[i].level);

		copy = m_strdup(target[i].message);
		/* do we forward this?*/

		if (forwardnick && not_done)
		{
			send_to_server("NOTICE %s :-> *%s* %s", forwardnick, nick_list, copy);
			not_done = 1;
		}

		/* log it if logging on */
		if (log)
			logmsg(1, LOG_SEND_MSG, target[i].hook_type == SEND_MSG_LIST?"*":"-", target[i].nick_list, target[i].message, "*", 0);
		/* save this for /oops */
	
		malloc_strcpy(&last_sent_msg_body, target[i].message);

		if (i == 0)
			malloc_strcpy(&sent_nick, target[0].nick_list);
		
		if (i == 1 || i == 3)
			is_current = is_current_channel(target[i].nick_list, from_server, 0);
		else
			is_current = 1;
			
		if (hook && do_hook(target[i].hook_type, "%s %s", target[i].nick_list, target[i].message))
			if (is_current)
	put_it("%s", convert_output_format(target[i].output,
		"%s %s %s %s",update_clock(GET_TIME), target[i].nick_list, get_server_nickname(from_server), copy));
			else
	put_it("%s", convert_output_format(target[i].other_output,
		"%s %s %s %s",update_clock(GET_TIME), target[i].nick_list, get_server_nickname(from_server), copy));
if ((i == 0))
	malloc_sprintf(&last_sent_msg, "%s %s %s",update_clock(GET_TIME), target[i].nick_list, copy);
else if ((i == 2) || (i == 3))
	malloc_sprintf(&last_sent_notice, "%s %s %s",update_clock(GET_TIME), target[i].nick_list, copy);
		new_free(&copy);
		send_to_server("%s %s :%s", target[i].command, target[i].nick_list, target[i].message);
		new_free(&target[i].nick_list);
		target[i].message = NULL;
		set_lastlog_msg_level(lastlog_level);
	}
	new_free(&copy);
	
	if (hook && server_list[curr_scr_win->server].away && get_int_var(AUTO_UNMARK_AWAY_VAR))
		parse_line(NULL, "AWAY", empty_string, 0, 0);

	message_from(NULL, LOG_CRAP);
	new_free(&free_nick);
	window_display = old_window_display;
	recursion--;
}

BUILT_IN_COMMAND(do_send_text)
{
	char	*tmp;

	context;
	if (command)
		tmp = get_channel_by_refnum(0);
	else
		tmp = get_target_by_refnum(0);
	send_text(tmp, args, NULL, 1, 1);
}

BUILT_IN_COMMAND(do_msay)
{
	char *channels = NULL;
	int i = get_window_server(0);
	ChannelList *chan;
	context;
	if (i != -1)
	{
		int old_from_server = from_server;
		for (chan  = server_list[i].chan_list; chan; chan = chan->next)
		{
			malloc_strcat(&channels, chan->channel);
			if (chan->next)
				malloc_strcat(&channels, ",");
		}
		from_server = i;
		if (channels)
			send_text(channels, args, NULL, 1, 1);
		new_free(&channels);
		from_server = old_from_server;
	} else
		bitchsay("No server for this window");
}

/*
 * command_completion: builds lists of commands and aliases that match the
 * given command and displays them for the user's lookseeing 
 */
void
command_completion(char unused, char *not_used)
{
	int	do_aliases, do_functions;
	int	cmd_cnt,
		alias_cnt,
		function_cnt,
		i,
		c;
	char	**aliases = NULL;
	char	**functions = NULL;
	
	char	*line = NULL,
		*com,
		*cmdchars,
		*rest,
		firstcmdchar = '/';
	IrcCommand	*command;
	char	buffer[BIG_BUFFER_SIZE + 1];
	
	context;
	malloc_strcpy(&line, get_input());
	if ((com = next_arg(line, &rest)) != NULL)
	{
		if (!(cmdchars = get_string_var(CMDCHARS_VAR)))
			cmdchars = DEFAULT_CMDCHARS;
		if (index(cmdchars, *com))
		{
			firstcmdchar = *cmdchars;
			com++;
			if (*com && index(cmdchars, *com))
			{
				do_aliases = 0;
				do_functions = 0;
				alias_cnt = 0;
				function_cnt = 0;
				com++;
			}
			else if (*com && index("$", *com))
			{
				do_aliases = 0;
				alias_cnt = 0;
				do_functions = 1;
				com++;
			} else
				do_aliases = do_functions = 1;
			upper(com);
			if (do_aliases)
				aliases = match_alias(com, &alias_cnt,
					COMMAND_ALIAS);
			if (do_functions)
				functions = match_alias(com, &function_cnt, -1);
				
			if ((command = find_command(com, &cmd_cnt)) != NULL)
			{
				if (cmd_cnt < 0)
					cmd_cnt *= -1;
				/* special case for the empty string */

				if (*(command[0].name) == (char) 0)
				{
					command++;
					cmd_cnt = NUMBER_OF_COMMANDS;
				}
			}
			if ((alias_cnt == 1) && (cmd_cnt == 0))
			{
				sprintf(buffer, "%c%s %s", firstcmdchar, aliases[0], rest);
				set_input(buffer);
				new_free((char **)&(aliases[0]));
				new_free((char **)&aliases);
				update_input(UPDATE_ALL);
			}
			else if (((cmd_cnt == 1) && (alias_cnt == 0)) ||
			    ((cmd_cnt == 1) && (alias_cnt == 1) &&
			    (strcmp(aliases[0], command[0].name) == 0)))
			{
				sprintf(buffer, "%c%s%s %s", firstcmdchar,
					do_aliases ? empty_string : &firstcmdchar,
					command[0].name, rest);
				set_input(buffer);
				update_input(UPDATE_ALL);
			}
			else
			{
				*buffer = (char) 0;
				if (command)
				{
					bitchsay("Commands:");
					*buffer = 0;
					c = 0;
					for (i = 0; i < cmd_cnt; i++)
					{
						strmcat(buffer, command[i].name,
							BIG_BUFFER_SIZE);
						strmcat(buffer, " ", BIG_BUFFER_SIZE);
						if (++c == 4)
						{
put_it("%s", convert_output_format("$G $[15]0 $[15]1 $[15]2 $[15]3", "%s", buffer));
							*buffer = 0;
							c = 0;
						}
					}
					if (c)
put_it("%s", convert_output_format("$G $[15]0 $[15]1 $[15]2 $[15]3", "%s", buffer));
				}
				if (aliases)
				{
					bitchsay("Aliases:");
					*buffer = 0;
					c = 0;
					for (i = 0; i < alias_cnt; i++)
					{
						strmcat(buffer, aliases[i], BIG_BUFFER_SIZE);
						strmcat(buffer, " ", BIG_BUFFER_SIZE);
						if (++c == 4)
						{
							put_it("%s", convert_output_format("$G $[15]0 $[15]1 $[15]2 $[15]3", "%s", buffer));
							*buffer = 0;
							c = 0;
						}
						new_free(&(aliases[i]));
					}
					if (strlen(buffer) > 1)
put_it("%s", convert_output_format("$G $[15]0 $[15]1 $[15]2 $[15]3", "%s", buffer));
					new_free((char **)&aliases);
				}
				if (functions)
				{
					bitchsay("Functions:");
					*buffer = 0;
					c = 0;
					for (i = 0; i < function_cnt; i++)
					{
						strmcat(buffer, functions[i], BIG_BUFFER_SIZE);
						strmcat(buffer, " ", BIG_BUFFER_SIZE);
						if (++c == 4)
						{
							put_it("%s", convert_output_format("$G $[15]0 $[15]1 $[15]2 $[15]3", "%s", buffer));
							*buffer = 0;
							c = 0;
						}
						new_free(&(functions[i]));
					}
					if (strlen(buffer) > 1)
put_it("%s", convert_output_format("$G $[15]0 $[15]1 $[15]2 $[15]3", "%s", buffer));
					new_free((char **)&functions);
				}
				if (!*buffer)
					term_beep();
			}
		}
		else
			term_beep();
	}
	else
		term_beep();
	new_free(&line);
}


void e_call (char *cmd, char *args, char *subargs)
{
	dump_call_stack();
}


/* parse_line: This is the main parsing routine.  It should be called in
 * almost all circumstances over parse_command().
 *
 * parse_line breaks up the line into commands separated by unescaped
 * semicolons if we are in non interactive mode. Otherwise it tries to leave
 * the line untouched.
 *
 * Currently, a carriage return or newline breaks the line into multiple
 * commands too. This is expected to stop at some point when parse_command
 * will check for such things and escape them using the ^P convention.
 * We'll also try to check before we get to this stage and escape them before
 * they become a problem.
 *
 * Other than these two conventions the line is left basically untouched.
 */
/* Ideas on parsing: Why should the calling function be responsible
 *  for removing {} blocks?  Why cant this parser cope with and {}s
 *  that come up?
 */
extern void parse_line (char *name, char *org_line, char *args, int hist_flag, int append_flag)
{
	char	*line = NULL,
		*free_line,
		*stuff,
		*buffer = NULL,
		*s,
		*t;
	int	args_flag = 0;

	context;

	make_local_stack(name);
	malloc_strcpy(&line, org_line);
	free_line = line;

	if (!*line)
	{
		do_send_text(NULL, empty_string, empty_string);
		set_input(empty_string);
	}
	else
        if (args) do 
        {
                while (*line == '{') 
                {
                        if ((stuff = next_expr(&line, '{')) == NULL) 
			{
                                yell("Unmatched {");
				destroy_local_stack();
                                new_free(&free_line);
                                return;
                        }
                        parse_line(name, stuff, args, hist_flag, append_flag);
			while (line && *line && ((*line == ';') || (my_isspace(*line))))
				*line++ = '\0';
		}

		if (!line || !*line)
		{
			destroy_local_stack();
			return;
		}
		stuff = expand_alias(name, line, args, &args_flag, &line);
		if (!line && append_flag && !args_flag && args && *args)
		{
                        malloc_sprintf(&buffer, "%s %s",stuff, args);
			new_free(&stuff);
			stuff = buffer;
		}
		parse_command(stuff, hist_flag, args);
		if (!line) new_free(&free_line);
                new_free(&stuff);
	} while (line && *line);
        else
	{
		if (load_depth)
			parse_command(line, hist_flag, args);
		else
			for (;(s = line);)
			{
				if ((t = sindex(line, "\r\n")))
				{
					*t++ = '\0';
					line = t;
				}
				else
					line = NULL;
				parse_command(s, hist_flag, args);
			}
	}
	new_free(&free_line);
	destroy_local_stack();
	return;
}

/*
 * parse_command: parses a line of input from the user.  If the first
 * character of the line is equal to irc_variable[CMDCHAR_VAR].value, the
 * line is used as an irc command and parsed appropriately.  If the first
 * character is anything else, the line is sent to the current channel or to
 * the current query user.  If hist_flag is true, commands will be added to
 * the command history as appropriate.  Otherwise, parsed commands will not
 * be added. 
 *
 * Parse_command() only parses a single command.  In general, you will want
 * to use parse_line() to execute things.Parse command recognized no quoted
 * characters or anything (beyond those specific for a given command being
 * executed). 
 */
int
parse_command(line, hist_flag, sub_args)
	char	*line;
	int	hist_flag;
	char	*sub_args;
{
static	unsigned int	 level = 0;
	unsigned int	display,
			old_display_var;
		char	*cmdchars,
			*com,
			*this_cmd = NULL;
		int	args_flag = 0,
			add_to_hist,
			cmdchar_used = 0;
		int	noisy = 1;
		
	context;
	if (!line || !*line)
		return 0;

	if (get_int_var(DEBUG_VAR) & DEBUG_COMMANDS)
		yell("Executing [%d] %s", level, line);
	level++;

	if (!(cmdchars = get_string_var(CMDCHARS_VAR)))
		cmdchars = DEFAULT_CMDCHARS;

	this_cmd = m_strdup(line);

	set_current_command(this_cmd);

	add_to_hist = 1;
	display = window_display;
	old_display_var = (unsigned) get_int_var(DISPLAY_VAR);
	for ( ;*line;line++)
	{
		if (*line == '^' && (!hist_flag || cmdchar_used))
		{
			if (!noisy)
				break;
			noisy = 0;
		}
		else if (index(cmdchars, *line))
		{
			cmdchar_used++;
			if (cmdchar_used > 2)
				break;
		}
		else
			break;
	}
	if (!noisy)
		window_display = 0;
	com = line;
	/*
	 * always consider input a command unless we are in interactive mode
	 * and command_mode is off.   -lynx
	 */
	if (hist_flag && !cmdchar_used && !get_int_var(COMMAND_MODE_VAR))
	{
		context;
		do_send_text(NULL, line, empty_string);
		if (hist_flag && add_to_hist)
		{
			add_to_history(this_cmd);
			set_input(empty_string);
		}
		/* Special handling for ' and : */
	}
	else if (*com == '\'' && get_int_var(COMMAND_MODE_VAR))
	{
		context;
		do_send_text(NULL, line+1, empty_string);
		if (hist_flag && add_to_hist)
		{
			add_to_history(this_cmd);
			set_input(empty_string);
		}
	}
	else if ((*com == '@') || (*com == '('))
	{
		/* This kludge fixes a memory leak */
		char	*tmp, *l_ptr;
		context;
		/*
		 * This new "feature" masks a weakness in the underlying
		 * grammar that allowed variable names to begin with an
		 * lparen, which inhibited the expansion of $s inside its
		 * name, which caused icky messes with array subscripts.
		 *
		 * Anyhow, we now define any commands that start with an
		 * lparen as being "expressions", same as @ lines.
		 */
		if (*com == '(')
		{
			if ((l_ptr = MatchingBracket(line + 1, '(', ')')))
				*l_ptr++ = 0;
		}

		if ((tmp = parse_inline(line + 1, sub_args, &args_flag)))
			new_free(&tmp);

		if (hist_flag && add_to_hist)
		{
			add_to_history(this_cmd);
			set_input(empty_string);
		}
	}
	else
	{
		char	*rest,
			*alias = NULL,
			*alias_name;
		int	cmd_cnt,
#ifdef WANT_DLL
			dll_cnt,
#endif
			alias_cnt = 0;
		IrcCommand	*command = NULL; /* = (IrcCommand *) 0 */
#ifdef WANT_DLL
		IrcCommandDll	*dll = NULL;
#endif		
		context;
		if ((rest = (char *) index(com, ' ')) != NULL)
			*(rest++) = (char) 0;
		else
			rest = empty_string;
		upper(com);

		if (cmdchar_used < 2)
			alias = get_alias(COMMAND_ALIAS, com, &alias_cnt, &alias_name);

		if (alias && (alias_cnt == 0))
		{
			context;
			if (hist_flag && add_to_hist)
			{
				add_to_history(this_cmd);
				set_input(empty_string);
			}
			execute_alias(alias_name, alias, rest);
			new_free(&alias_name);
		}
		else
		{
			/* History */
			context;
			if (*com == '!')
			{
				if ((com = do_history(com + 1, rest)) != NULL)
				{
					if (level == 1)
					{
						set_input(com);
						update_input(UPDATE_ALL);
					}
					else
						parse_command(com, 0, sub_args);
					new_free(&com);
				}
				else
					set_input(empty_string);
			}
			else
			{
				char unknown[] = "Unknown command:";
				context;
				
				if (hist_flag && add_to_hist)
				{
					add_to_history(this_cmd);
					set_input(empty_string);
				}
				command = find_command(com, &cmd_cnt);
#ifdef WANT_DLL
				dll = find_dll_command(com, &dll_cnt);
				if ((dll && (dll_cnt < 0)) || ((alias_cnt == 0) && (dll_cnt == 1)))
				{
					context;
					strcpy(cx_function, dll->name);
					if (dll->func)
					{
						dll->func(dll_commands, dll->server_func, rest, sub_args);
						if (dll->result)
							put_it("%s", dll->result);
					}
					else
						bitchsay("%s: command disabled", dll->name);
					*cx_function = 0;
				}
				else 
#endif
				if ((command && (cmd_cnt < 0)) || ((alias_cnt == 0) && (cmd_cnt == 1)))
				{
					context;
					strcpy(cx_function, command->name);
					if (command->func)
						command->func(command->server_func, rest, sub_args);
					else
						bitchsay("%s: command disabled", command->name);
					*cx_function = 0;
				}
				else if (alias && (alias_cnt == 1) && 
				    (cmd_cnt == 1) && 
				    (strcmp(alias_name, command[0].name) == 0))
					execute_alias(alias_name, alias, rest);
				else if ((alias_cnt + cmd_cnt) > 1)
					bitchsay("Ambiguous command: %s", com);
				else if (alias && (alias_cnt == 1))
					execute_alias(alias_name, alias, rest);
				else if (!my_stricmp(com, nickname))
					me(NULL, rest, empty_string);
				else if (!my_stricmp(com, "ENV"))
					add_env(rest);
#ifdef WANT_TCL
				else
				{
					context;
					if (interp)
					{
						if ((Tcl_Eval(interp, strchr(this_cmd, '/') ? this_cmd+1:this_cmd) != TCL_OK))
							bitchsay("%s %s", interp->result?empty_string:unknown, interp->result?interp->result:empty_string);
						else
							bitchsay("%s %s", unknown, interp->result?interp->result:empty_string);
					} else
							bitchsay("%s %s", unknown, com);
				}
#else
				else
					bitchsay("%s %s", unknown, com);
#endif
			}
			if (alias)
				new_free(&alias_name);
			context;
		}
	}
	if (old_display_var != get_int_var(DISPLAY_VAR))
		window_display = get_int_var(DISPLAY_VAR);
	else
		window_display = display;
	new_free(&this_cmd);
	level--;
	unset_current_command();
	return 0;
}


/*
 * load: the /LOAD command.  Reads the named file, parsing each line as
 * though it were typed in (passes each line to parse_command). 
	Right now, this is broken, as it doesnt handle the passing of
	the '-' flag, which is meant to force expansion of expandos
	with the arguments after the '-' flag.  I think its a lame
	feature, anyhow.  *sigh*.
 */

#ifdef __STDC__
extern void	load (char *command, char *args, char *subargs)
#else
extern void	load(command, args, subargs)
	char	*command,
		*args;
	char	*subargs;
#endif
{
	FILE	*fp;
	char	*filename,
		*expanded = NULL;
	int	flag = 0;
        int     paste_level = 0;
	char	*start,
		*current_row = NULL,
		buffer[BIG_BUFFER_SIZE + 1];
	int	no_semicolon = 1;
	char	*irc_path;
	int	display;
        int     ack = 0;

	context;
	no_hook_notify = 1;
	irc_path = get_string_var(LOAD_PATH_VAR);
	if (!irc_path)
	{
		bitchsay("LOAD_PATH has not been set");
		return;
	}

	if (load_depth == MAX_LOAD_DEPTH)
	{
		bitchsay("No more than %d levels of LOADs allowed", MAX_LOAD_DEPTH);
		return;
	}
	load_depth++;
	status_update(0);

	/* 
	 * We iterate over the whole list -- if we use the -args flag, the
	 * we will make a note to exit the loop at the bottom after we've
	 * gone through it once...
	 */
	while (args && *args && (filename = next_arg(args, &args)))
	{
		/* 
		   If we use the args flag, then we will get the next
		   filename (via continue) but at the bottom of the loop
		   we will exit the loop 
		 */
		if (my_strnicmp(filename, "-args", strlen(filename)) == 0)
		{
			flag = 1;
			continue;
		}
		else if ((expanded = expand_twiddle(filename)))
		{
			if (!(fp = uzfopen(&expanded, irc_path)))
			{
				/* uzfopen emits an error if the file
				 * is not found, so we dont have to. */
				status_update(1);
				load_depth--;
				new_free(&expanded);
				return;
			}
			if (command && *command == 'W')
			{
				bitchsay ("%s", expanded);
				if (fp)
					fclose (fp);
				status_update(1);
				load_depth--;
				new_free(&expanded);
				return;
			}
/* Reformatted by jfn */
/* *_NOT_* attached, so dont "fix" it */
{
	int	in_comment 	= 0;
	int	comment_line 	= -1;
	int 	line 		= 1;
	int 	paste_line	= -1;

	display = window_display;
	window_display = 0;
	current_row = NULL;

	for (;;line++)
	{
	       if (fgets(buffer,BIG_BUFFER_SIZE / 2,fp))
	       {
			int     len;
			char    *ptr;

			for (start = buffer; my_isspace(*start); start++)
				;
			if (!*start || *start == '#')
				continue;

			len = strlen(start);
			/*
			 * this line from stargazer to allow \'s in scripts for continued
			 * lines <spz@specklec.mpifr-bonn.mpg.de>
			 */
			/* 
			   If we have \\ at the end of the line, that
			   should indicate that we DONT want the slash to 
			   escape the newline (hop)

			   We cant just do start[len-2] because we cant say
			   what will happen if len = 1... (a blank line)

			   SO.... 
			   If the line ends in a newline, and
			   If there are at least 2 characters in the line
				and the 2nd to the last one is a \ and,
			   If there are EITHER 2 characters on the line or
				the 3rd to the last character is NOT a \ and,
			   If the line isnt too big yet and,
			   If we can read more from the file,
			   THEN -- adjust the length of the string
			*/
			while ( (start[len-1] == '\n') && 
				(len >= 2 && start[len-2] == '\\') &&
				(len < 3 || start[len-3] != '\\') && 
				(len < BIG_BUFFER_SIZE / 2) && 
				(fgets(&(start[len-2]), BIG_BUFFER_SIZE / 2 - len, fp)))
			{
				len = strlen(start);
				line++;
			}

			if (start[len - 1] == '\n')
				start[--len] = '\0';

			while (start && *start)
			{
				char    *optr = start;

				/* Skip slashed brackets */
				while ((ptr = sindex(optr, "{};/")) && 
				      ptr != optr && ptr[-1] == '\\')
					optr = ptr+1;

				/* if no_semicolon is set, we will not attempt
				 * to parse this line, but will continue
				 * grabbing text
				 */
				if (no_semicolon)
					no_semicolon = 0;
				else if ((!ptr || (ptr != start || *ptr == '/')) && current_row)
				{
					if (!paste_level)
					{
						parse_line(NULL, current_row, flag ? args : NULL, 0, 0);
						new_free(&current_row);
					}
					else if (!in_comment)
						malloc_strcat(&current_row, ";");
				}

				if (ptr)
				{
					char    c = *ptr;

					*ptr = '\0';
					if (!in_comment)
						malloc_strcat(&current_row, start);
					*ptr = c;

					switch (c)
					{
		/* switch statement tabbed back */
case '/' :
{
	/* If we're in a comment, any slashes that arent preceeded by
	   a star is just ignored (cause its in a comment, after all >;) */
	if (in_comment)
	{
		/* ooops! cant do ptr[-1] if ptr == optr... doh! */
		if ((ptr > start) && (ptr[-1] == '*'))
		{
			in_comment = 0;
			comment_line = -1;
		}
		else
			break;
	}
	/* We're not in a comment... should we start one? */
	else
	{
		/* COMMENT_BREAKAGE_VAR */
		if ((ptr[1] == '*') && !get_int_var(COMMENT_BREAKAGE_VAR) && (ptr == start))
			/* This hack (at the request of Kanan) makes it
			   so you can only have comments at the BEGINNING
			   of a line, and any midline comments are ignored.
			   This is required to run lame script packs that
			   do ascii art crap (ie, phoenix, textbox, etc) */
		{
			/* Yep. its the start of a comment. */
			in_comment = 1;
			comment_line = line;
		}
		else
		{
			/* Its NOT a comment. Tack it on the end */
			malloc_strcat(&current_row, "/");

			/* Is the / is at the EOL? */
			if (ptr[1] == '\0')
			{
				/* If we are NOT in a block alias, */
				if (!paste_level)
				{
					/* Send the line off to parse_line */
					parse_line(NULL, current_row, flag ? args : NULL, 0, 0);
					new_free(&current_row);
					ack = 0; /* no semicolon.. */
				}
				else
					/* Just add a semicolon and keep going */
					ack = 1; /* tack on a semicolon */
			}
		}


	}
	no_semicolon = 1 - ack;
	ack = 0;
	break;
}
case '{' :
{
	if (in_comment)
		break;

	/* If we are opening a brand new {} pair, remember the line */
	if (!paste_level)
		paste_line = line;
		
	paste_level++;
	if (ptr == start)
		malloc_strcat(&current_row, " {");
	else
		malloc_strcat(&current_row, "{");
	no_semicolon = 1;
	break;
}
case '}' :
{
	if (in_comment)
		break;
        if (!paste_level)
		yell("Unexpected } in %s, line %d", expanded, line);
	else 
	{
        	--paste_level;

		if (!paste_level)
			paste_line = -1;
		malloc_strcat(&current_row, "}"); /* } */
		no_semicolon = ptr[1]? 1: 0;
	}
	break;
}
case ';' :
{
	if (in_comment)
		break;
	if ((*(ptr+1) == '\0') && (!paste_level))
	{
		parse_line(NULL, current_row, flag ? args : NULL, 0, 0);
		new_free(&current_row);
	}
	else
		malloc_strcat(&current_row, ";");
	no_semicolon = 1;
	break;
}
	/* End of reformatting */
					}
					start = ptr+1;
				}
				else
				{
					if (!in_comment)
						malloc_strcat(&current_row, start);
					start = NULL;
				}
			}
		}
		else
			break;
	}
	if (in_comment)
		yell("File %s ended with an unterminated comment in line %d", expanded, comment_line);
	if (current_row)
	{
		if (paste_level)
			yell("Unexpected EOF in %s trying to match '{' at line %d",
					expanded, paste_line);
		else
			parse_line(NULL,current_row, flag ? args: NULL,0,0);
		new_free(&current_row);
	}
	new_free(&expanded);
	fclose(fp);
	if (get_int_var(DISPLAY_VAR))
	       window_display = display;
}
/* End of reformatting */
		}
		else
			bitchsay("Unknown user");
		/* 
		 * brute force -- if we are using -args, load ONLY one file
		 * then exit the while loop.  We could have done this
		 * by assigning args to NULL, but that would only waste
		 * cpu cycles by relooping...
		 */
		if (flag)
			break;
	}	/* end of the while loop that allows you to load
		   more then one file at a time.. */
	status_update(1);
	load_depth--;
/*	send_to_server("%s", "WAITFORNOTIFY");*/
}



/* The SENDLINE command.. */
BUILT_IN_COMMAND(sendlinecmd)
{
	int	server;
	int	display;

	context;
	server = from_server;
	display = window_display;
	window_display = 1;
	parse_line(NULL, args, get_int_var(INPUT_ALIASES_VAR) ?empty_string : NULL, 1, 0);
	update_input(UPDATE_ALL);
	window_display = display;
	from_server = server;
}

/*
 * irc_clear_screen: the CLEAR_SCREEN function for BIND.  Clears the screen and
 * starts it if it is held 
 */
/*ARGSUSED*/
void
irc_clear_screen(char key, char *ptr)
{
	context;
	hold_mode(NULL, OFF, 1);
	my_clear(NULL, empty_string, empty_string);
}

BUILT_IN_COMMAND(cd)
{
	char	*arg,
		*expand;
	char buffer[BIG_BUFFER_SIZE + 1];

	context;
	if ((arg = next_arg(args, &args)) != NULL)
	{
		if ((expand = expand_twiddle(arg)) != NULL)
		{
			if (chdir(expand))
				bitchsay("CD: %s", sys_errlist[errno]);
			new_free(&expand);
		}
		else
			bitchsay("CD No such dir");
	}
	getcwd(buffer, BIG_BUFFER_SIZE+1);
	bitchsay("Current directory: %s", buffer);
}

BUILT_IN_COMMAND(exec_cmd)
{
	char	buffer[BIG_BUFFER_SIZE + 1];
	context;
	sprintf(buffer, "%s %s", command, args);
	execcmd(NULL, buffer, subargs);
}

BUILT_IN_COMMAND(describe)
{
	char	*target;

	context;
	target = next_arg(args, &args);
	if (target && args && *args)
	{
		int	old;
		int	from_level;
		char	*message;

		message = args;
		send_ctcp(CTCP_PRIVMSG, target, CTCP_ACTION, "%s", message);

		old = set_lastlog_msg_level(LOG_ACTION);
		from_level = message_from_level(LOG_ACTION);
		if (do_hook(SEND_ACTION_LIST, "%s %s", target, message))
	put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_ACTION_OTHER_VAR), 
		"%s %s %s %s", update_clock(GET_TIME), get_server_nickname(from_server), target, message));
		set_lastlog_msg_level(old);
		message_from_level(from_level);

	}
	else if (target && (!args || !*args))
		bitchsay("No Description");
	else
		userage("describe", "<channel\002|\002nick> <action description>");
}

/*
 * New 'me' command - now automatically appends period.
 * Necessary for new 'action' script.   -lynx'92
 * Hardly, removed this as it was generally considered offensive
 */
BUILT_IN_COMMAND(me)
{
	context;
	if (args && *args)
	{
		char	*target;

		if ((target = get_target_by_refnum(0)) != NULL)
		{
			int	old;
			char	*message;

			if (*target == '=' || !strncmp(target, get_string_var(CMDCHARS_VAR), 1))
			{
				if (!(target = get_channel_by_refnum(0)))
				{
					bitchsay("No target, neither channel nor query");
					return;
				}
			}
			message = args;
			send_ctcp(CTCP_PRIVMSG, target, CTCP_ACTION, "%s", message);

			message_from(target, LOG_ACTION);
			old = set_lastlog_msg_level(LOG_ACTION);
			if (do_hook(SEND_ACTION_LIST, "%s %s", target, message))
			{
				if (strchr("&#", *target))
	put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_ACTION_VAR), 
		"%s %s %s %s", update_clock(GET_TIME), get_server_nickname(from_server), target, message));
				else
	put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_ACTION_OTHER_VAR), 
		"%s %s %s %s", update_clock(GET_TIME), get_server_nickname(from_server), target, message));
			}
			set_lastlog_msg_level(old);
			message_from(NULL, LOG_CRAP);
		}
		else
			bitchsay("No target, neither channel nor query");
	}
	else
		userage("me", "<action description>");
}

/*
 * eval_inputlist:  Cute little wrapper that calls parse_line() when we
 * get an input prompt ..
 */

void
eval_inputlist(args, line)
	char	*args,
		*line;
{
	context;
	parse_line(NULL, args, line ? line : empty_string, 0, 0);
}

BUILT_IN_COMMAND(evalcmd)
{
	context;
	parse_line(NULL, args, subargs ? subargs : empty_string, 0, 0);
}

/*
 * inputcmd:  the INPUT command.   Takes a couple of arguements...
 * the first surrounded in double quotes, and the rest makes up
 * a normal ircII command.  The command is evalutated, with $*
 * being the line that you input.  Used add_wait_prompt() to prompt
 * the user...  -phone, jan 1993.
 */

BUILT_IN_COMMAND(inputcmd)
{
	char	*prompt;
	int	wait_type;
	
	context;
	if (!args || !*args)
		return;

	if (*args++ != '\"')
	{
		bitchsay("Need \" to begin prompt for INPUT");
		return;
	}

	prompt = args;
	if ((args = index(prompt, '"')) != NULL)
		*args++ = '\0';
	else
	{
		bitchsay("Missing \" in INPUT");
		return;
	}

	for (; *args == ' '; args++)
		;
	if (!my_stricmp(command, "INPUT"))
		wait_type = WAIT_PROMPT_LINE;
	else
		wait_type = WAIT_PROMPT_KEY;

	add_wait_prompt(prompt, eval_inputlist, args, wait_type);
}

BUILT_IN_COMMAND(xtypecmd)
{
	char	*arg;

	context;
	if (args && (*args == '-' || *args == '/'))
	{
		char saved = *args;
		args++;
		if ((arg = next_arg(args, &args)) != NULL)
		{
			if (!my_strnicmp(arg, "L", 1))
			{
				for (; *args; args++)
					input_add_character(*args, empty_string);
			}
			else
				bitchsay ("Unknown flag -%s to XTYPE", arg);
			return;
		}
		input_add_character(saved, empty_string);
	}
	else
		type(command, args, empty_string);
	return;
}

BUILT_IN_COMMAND(beepcmd)
{
	term_beep();
}

#ifdef __STDC__
extern void e_debug (char *command, char *args, char *subargs)
#else
extern void e_debug(command, args, subargs)
char *command, *args, *subargs;
#endif
{
	int x;
	char buffer[BIG_BUFFER_SIZE + 1];

	*buffer = 0;
	for (x = 0; x < FD_SETSIZE; x++)
	{
		if (FD_ISSET(x, &readables))
		{
			strcat(buffer, space);
			strcat(buffer, ltoa(x));
		}
	}
	yell(buffer);
}
void pretend_cmd (char *command, char *args, char *subargs)
{
	parse_server(args);
}

/*
 * This is a quick and dirty hack (emphasis on dirty) that i whipped up
 * just for the heck of it.  I feel really dirty about using the add_timer
 * call (bletch!) to fake a timeout for io().  The better answer would be
 * for io() to take an argument specifying the maximum threshold for a
 * timeout, but i didnt want to deal with that here.  So i just add a
 * dummy timer event that does nothing (wasting two function calls and
 * about 20 bytes of memory), and call io() until the whole thing blows
 * over.  Nice and painless.  You might want to try this instead of /sleep,
 * since this is (obviously) non-blocking.  This also calls time() for every
 * io event, so that might also start adding up.  Oh well, TIOLI.
 *
 * Without an argument, it waits for the user to press a key.  Any key.
 * and the key is accepted.  Thats probably not right, ill work on that.
 */
BUILT_IN_COMMAND(e_pause)
{
	char *sec;
	int seconds;
	time_t start;

	if (!(sec = next_arg(args, &args)))
	{
		cursor_to_input();
		get_a_char();
		input_backspace(0, NULL);
		return;
	}

	seconds = atoi(sec);
	time(&start);

	/* 
	 * I use comment here simply becuase its not going to mess
	 * with the arguments.
	 */
	add_timer("", seconds, (int (*)(void *))comment, NULL, NULL);
	while (time(NULL) < start + seconds)
		io();
}

#ifndef WANT_TCL
void tcl_command (char *command, char *args, char *subargs )
{
	bitchsay("This Client is not compiled with tcl support.");
}
#endif
