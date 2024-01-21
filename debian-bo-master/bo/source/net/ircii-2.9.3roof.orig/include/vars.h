/*
 * vars.h: header for vars.c
 *
 * Generated from vars.h.proto automatically by the Makefile
 *
 * @(#)$Id: vars.h,v 1.4 1995/12/13 23:53:20 scottr Exp $
 */

#ifndef __vars_h_
#define __vars_h_

	int	do_boolean _((char *, int *));
	void	set_variable _((char *, char *, char *));
	int	get_int_var _((int));
	char	*get_string_var _((int));
	void	set_int_var _((int, unsigned int));
	void	set_string_var _((int, char *));
	void	init_variables _((void));
	char	*make_string_var _((char *));
	void	set_highlight_char _((char *));
	int	charset_size _((void));
	void	save_variables _((FILE *, int));
	void	set_var_value _((int, char *));

extern	char	*var_settings[];
extern	int	loading_global;

/* var_settings indexes ... also used in display.c for highlights */
#define OFF 0
#define ON 1
#define TOGGLE 2

#define	DEBUG_COMMANDS		0x0001
#define	DEBUG_EXPANSIONS	0x0002
#define DEBUG_FUNCTIONS		0x0004

/* indexes for the irc_variable array */
#define ALWAYS_SPLIT_BIGGEST_VAR 0
#define AUTO_UNMARK_AWAY_VAR 1
#define AUTO_WHOWAS_VAR  2
#define BEEP_VAR 3
#define BEEP_MAX_VAR 4
#define BEEP_ON_MSG_VAR 5
#define BEEP_WHEN_AWAY_VAR 6
#define	BOLD_VIDEO_VAR 7
#define CHANNEL_NAME_WIDTH_VAR 8
#define CLIENTINFO_VAR 9
#define CLOCK_VAR 10
#define CLOCK_24HOUR_VAR 11
#define CLOCK_ALARM_VAR 12
#define CMDCHARS_VAR 13
#define COMMAND_MODE_VAR 14
#define CONTINUED_LINE_VAR 15
#define DCC_BLOCK_SIZE_VAR 16
#define	DEBUG_VAR 17
#define DISPLAY_VAR 18
#define EIGHT_BIT_CHARACTERS_VAR 19
#define ENCRYPT_PROGRAM_VAR 20
#define EXEC_PROTECTION_VAR 21
#define FLOOD_AFTER_VAR 22
#define FLOOD_RATE_VAR 23
#define FLOOD_USERS_VAR 24
#define FLOOD_WARNING_VAR 25
#define FULL_STATUS_LINE_VAR 26
#define HELP_PAGER_VAR 27
#define HELP_PATH_VAR 28
#define HELP_PROMPT_VAR 29
#define HELP_SERVICE_VAR 30
#define HELP_WINDOW_VAR 31
#define HIDE_PRIVATE_CHANNELS_VAR 32
#define HIGHLIGHT_CHAR_VAR 33
#define HISTORY_VAR 34
#define HISTORY_FILE_VAR 35
#define HOLD_MODE_VAR 36
#define HOLD_MODE_MAX_VAR 37
#define INDENT_VAR 38
#define INPUT_ALIASES_VAR 39
#define INPUT_PROMPT_VAR 40
#define INPUT_PROTECTION_VAR 41
#define INSERT_MODE_VAR 42
#define INVERSE_VIDEO_VAR 43
#define LASTLOG_VAR 44
#define LASTLOG_LEVEL_VAR 45
#define	LOAD_PATH_VAR 46
#define LOG_VAR 47
#define LOGFILE_VAR 48
#define MAIL_VAR 49
#define MAX_RECURSIONS_VAR 50
#define	MENU_VAR 51
#define MINIMUM_SERVERS_VAR 52
#define MINIMUM_USERS_VAR 53
#define NO_CTCP_FLOOD_VAR 54
#define NOTIFY_HANDLER_VAR 55
#define NOTIFY_LEVEL_VAR 56
#define NOTIFY_ON_TERMINATION_VAR 57
#define NOVICE_VAR 58
#define REALNAME_VAR 59
#define SAME_WINDOW_ONLY_VAR 60
#define SCREEN_OPTIONS_VAR 61
#define SCROLL_VAR 62
#define SCROLL_LINES_VAR 63
#define SEND_IGNORE_MSG_VAR 64
#define SHELL_VAR 65
#define SHELL_FLAGS_VAR 66
#define SHELL_LIMIT_VAR 67
#define SHOW_AWAY_ONCE_VAR 68
#define SHOW_CHANNEL_NAMES_VAR 69
#define SHOW_END_OF_MSGS_VAR 70
#define SHOW_NUMERICS_VAR 71
#define SHOW_STATUS_ALL_VAR 72
#define SHOW_WHO_HOPCOUNT_VAR 73
#define STATUS_AWAY_VAR 74
#define STATUS_CHANNEL_VAR 75
#define	STATUS_CHANOP_VAR 76
#define STATUS_CLOCK_VAR 77
#define STATUS_FORMAT_VAR 78
#define STATUS_FORMAT1_VAR 79
#define STATUS_FORMAT2_VAR 80
#define STATUS_HOLD_VAR 81
#define STATUS_HOLD_LINES_VAR 82
#define STATUS_INSERT_VAR 83
#define STATUS_MAIL_VAR 84
#define STATUS_MODE_VAR 85
#define STATUS_NOTIFY_VAR 86
#define STATUS_OPER_VAR 87
#define STATUS_OVERWRITE_VAR 88
#define STATUS_QUERY_VAR 89
#define STATUS_SERVER_VAR 90
#define	STATUS_UMODE_VAR 91
#define STATUS_USER_VAR 92
#define STATUS_USER1_VAR 93
#define STATUS_USER2_VAR 94
#define STATUS_USER3_VAR 95
#define STATUS_WINDOW_VAR 96
#define SUPPRESS_SERVER_MOTD_VAR 97
#define TAB_VAR 98
#define	TAB_MAX_VAR 99
#define TRANSLATION_VAR 100
#define UNDERLINE_VIDEO_VAR 101
#define USE_OLD_MSG_VAR 102
#define USER_INFO_VAR 103
#define	USERINFO_VAR USER_INFO_VAR
#define	USER_WALLOPS_VAR 104
#define VERBOSE_CTCP_VAR 105
#define WARN_OF_IGNORES_VAR 106
#define XTERM_OPTIONS_VAR 107
#define NUMBER_OF_VARIABLES 108

#endif /* __vars_h_ */
