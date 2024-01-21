/* setterm.c, set terminal attributes.
 *
 * Copyright (C) 1990 Gordon Irlam (gordoni@cs.ua.oz.au).  Conditions of use,
 * modification, and redistribution are contained in the file COPYRIGHT that
 * forms part of this distribution.
 * 
 * Adaption to Linux by Peter MacDonald.
 *
 * Enhancements by Mika Liljeberg (liljeber@cs.Helsinki.FI)
 *
 *
 * Syntax:
 *
 * setterm
 *   [ -term terminal_name ]
 *   [ -reset ]
 *   [ -initialize ]
 *   [ -cursor [on|off] ]
 *   [ -keyboard pc|olivetti|dutch|extended ]
 *   [ -repeat [on|off] ]
 *   [ -appcursorkeys [on|off] ]
 *   [ -linewrap [on|off] ]
 *   [ -snow [on|off] ]
 *   [ -softscroll [on|off] ]
 *   [ -defaults ]
 *   [ -foreground black|red|green|yellow|blue|magenta|cyan|white|default ]
 *   [ -background black|red|green|yellow|blue|magenta|cyan|white|default ]
 *   [ -ulcolor black|grey|red|green|yellow|blue|magenta|cyan|white ]
 *   [ -ulcolor bright red|green|yellow|blue|magenta|cyan|white ]
 *   [ -hbcolor black|grey|red|green|yellow|blue|magenta|cyan|white ]
 *   [ -hbcolor bright red|green|yellow|blue|magenta|cyan|white ]
 *   [ -inversescreen [on|off] ]
 *   [ -bold [on|off] ]
 *   [ -half-bright [on|off] ]
 *   [ -blink [on|off] ]
 *   [ -reverse [on|off] ]
 *   [ -underline [on|off] ]
 *   [ -store ]
 *   [ -clear [ all|rest ] ]
 *   [ -tabs [tab1 tab2 tab3 ... ] ]     (tabn = 1-160)
 *   [ -clrtabs [ tab1 tab2 tab3 ... ]   (tabn = 1-160)
 *   [ -regtabs [1-160] ]
 *   [ -blank [0-60] ]
 *   [ -dump   [1-NR_CONS ] ]
 *   [ -append [1-NR_CONS ] ]
 *   [ -file dumpfilename ]
 *   [ -standout [attr] ]
 *   [ -msg [on|off] ]
 *   [ -msglevel [0-8] ]
 *   [ -powersave [on|off] ]
 *
 *
 * Semantics:
 *
 * Setterm writes to standard output a character string that will invoke the
 * specified terminal capabilities.  Where possibile termcap is consulted to
 * find the string to use.  Some options however do not correspond to a
 * termcap capability.  In this case if the terminal type is "con*", or
 * "linux*" the string that invokes the specified capabilities on the PC
 * Linux virtual console driver is output.  Options that are not implemented
 * by the terminal are ignored.
 *
 * The following options are non-obvious.
 *
 *   -term can be used to override the TERM environment variable.
 *
 *   -reset displays the terminal reset string, which typically resets the
 *      terminal to its power on state.
 *
 *   -initialize displays the terminal initialization string, which typically
 *      sets the terminal's rendering options, and other attributes to the
 *      default values.
 *
 *   -default sets the terminal's rendering options to the default values.
 *
 *   -store stores the terminal's current rendering options as the default
 *      values.
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <termcap.h>
#include <linux/config.h>
#include <sys/time.h>
#include <unistd.h>
#include <termios.h>
#include <string.h>

/* for syslog system call */
#include <linux/unistd.h>
#include <errno.h>
_syscall3(int, syslog, int, type, char*, buf, int, len);

/* Constants. */

/* Termcap constants. */
#define TC_BUF_SIZE 1024	/* Size of termcap(3) buffer. */
#define TC_ENT_SIZE 50		/* Size of termcap(3) entry buffer. */

/* General constants. */
#define TRUE  1
#define FALSE 0

/* Keyboard types. */
#define PC	 0
#define OLIVETTI 1
#define DUTCH    2
#define EXTENDED 3

/* Colors. */
#define BLACK   0
#define RED     1
#define GREEN   2
#define YELLOW  3
#define BLUE    4
#define MAGENTA 5
#define CYAN    6
#define WHITE   7
#define GREY	8
#define DEFAULT 9

/* Control sequences. */
#define ESC "\033"
#define DCS "\033P"
#define ST  "\033\\"

/* Static variables. */

char tc_buf[TC_BUF_SIZE];	/* Termcap buffer. */

/* Option flags.  Set if the option is to be invoked. */
int opt_term, opt_reset, opt_initialize, opt_cursor, opt_keyboard;
int opt_linewrap, opt_snow, opt_softscroll, opt_default, opt_foreground;
int opt_background, opt_bold, opt_blink, opt_reverse, opt_underline;
int opt_store, opt_clear, opt_blank, opt_snap, opt_snapfile, opt_standout;
int opt_append, opt_ulcolor, opt_hbcolor, opt_halfbright, opt_repeat;
int opt_tabs, opt_clrtabs, opt_regtabs, opt_appcursorkeys, opt_inversescreen;
int opt_msg, opt_msglevel, opt_powersave;

/* Option controls.  The variable names have been contracted to ensure
 * uniqueness.
 */
char *opt_te_terminal_name;	/* Terminal name. */
int opt_cu_on, opt_li_on, opt_sn_on, opt_so_on, opt_bo_on, opt_hb_on, opt_bl_on;
int opt_re_on, opt_un_on, opt_rep_on, opt_appck_on, opt_invsc_on;
int opt_msg_on, opt_ps_on;	/* Boolean switches. */
int opt_ke_type;		/* Keyboard type. */
int opt_fo_color, opt_ba_color;	/* Colors. */
int opt_ul_color, opt_hb_color;
int opt_cl_all;			/* Clear all or rest. */
int opt_bl_min;			/* Blank screen. */
int opt_sn_num = 0;		/* Snap screen. */
int opt_st_attr;
int opt_rt_len;			/* regular tab length */
int opt_tb_array[161];		/* Array for tab list */
int opt_msglevel_num;

char opt_sn_name[200] = "screen.dump";

/* Command line parsing routines.
 *
 * Note that it is an error for a given option to be invoked more than once.
 */

void parse_term(argc, argv, option, opt_term, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Term flag to set. */
char **opt_term;		/* Terminal name to set. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -term specification. */

  if (argc != 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	*opt_term = argv[0];
  }
}

void parse_none(argc, argv, option, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Option flag to set. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a parameterless specification. */

  if (argc != 0 || *option) *bad_arg = TRUE;
  *option = TRUE;
}

void parse_switch(argc, argv, option, opt_on, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Option flag to set. */
int *opt_on;			/* Boolean option switch to set or reset. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a boolean (on/off) specification. */

  if (argc > 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	if (strcmp(argv[0], "on") == 0)
		*opt_on = TRUE;
	else if (strcmp(argv[0], "off") == 0)
		*opt_on = FALSE;
	else
		*bad_arg = TRUE;
  } else {
	*opt_on = TRUE;
  }
}

#if 0
void parse_keyboard(argc, argv, option, opt_keyboard, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Keyboard flag to set. */
int *opt_keyboard;		/* Keyboard type to set. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -keyboard specification. */

  if (argc != 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	if (strcmp(argv[0], "pc") == 0)
		*opt_keyboard = PC;
	else if (strcmp(argv[0], "olivetti") == 0)
		*opt_keyboard = OLIVETTI;
	else if (strcmp(argv[0], "dutch") == 0)
		*opt_keyboard = DUTCH;
	else if (strcmp(argv[0], "extended") == 0)
		*opt_keyboard = EXTENDED;
	else
		*bad_arg = TRUE;
  }
}
#endif

void par_color(argc, argv, option, opt_color, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Color flag to set. */
int *opt_color;			/* Color to set. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -foreground or -background specification. */

  if (argc != 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	if (strcmp(argv[0], "black") == 0)
		*opt_color = BLACK;
	else if (strcmp(argv[0], "red") == 0)
		*opt_color = RED;
	else if (strcmp(argv[0], "green") == 0)
		*opt_color = GREEN;
	else if (strcmp(argv[0], "yellow") == 0)
		*opt_color = YELLOW;
	else if (strcmp(argv[0], "blue") == 0)
		*opt_color = BLUE;
	else if (strcmp(argv[0], "magenta") == 0)
		*opt_color = MAGENTA;
	else if (strcmp(argv[0], "cyan") == 0)
		*opt_color = CYAN;
	else if (strcmp(argv[0], "white") == 0)
		*opt_color = WHITE;
	else if (strcmp(argv[0], "default") == 0)
		*opt_color = DEFAULT;
	else if (isdigit(argv[0][0]))
	    *opt_color = atoi(argv[0]);
	else    
		*bad_arg = TRUE;
       if(*opt_color < 0 || *opt_color > 15)
	    *bad_arg = TRUE;
  }
}

void par_color2(argc, argv, option, opt_color, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Color flag to set. */
int *opt_color;			/* Color to set. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -ulcolor or -hbcolor specification. */

  if (!argc || argc > 2 || *option) *bad_arg = TRUE;
  *option = TRUE;
  *opt_color = 0;
  if (argc == 2) {
	if (strcmp(argv[0], "bright") == 0)
		*opt_color = 8;
	else {
		*bad_arg = TRUE;
		return;
	}
  }
  if (argc) {
	if (strcmp(argv[argc-1], "black") == 0) {
		if(*opt_color)
			*bad_arg = TRUE;
		else
			*opt_color = BLACK;
	} else if (strcmp(argv[argc-1], "grey") == 0) {
		if(*opt_color)
			*bad_arg = TRUE;
		else
			*opt_color = GREY;
	} else if (strcmp(argv[argc-1], "red") == 0)
		*opt_color |= RED;
	else if (strcmp(argv[argc-1], "green") == 0)
		*opt_color |= GREEN;
	else if (strcmp(argv[argc-1], "yellow") == 0)
		*opt_color |= YELLOW;
	else if (strcmp(argv[argc-1], "blue") == 0)
		*opt_color |= BLUE;
	else if (strcmp(argv[argc-1], "magenta") == 0)
		*opt_color |= MAGENTA;
	else if (strcmp(argv[argc-1], "cyan") == 0)
		*opt_color |= CYAN;
	else if (strcmp(argv[argc-1], "white") == 0)
		*opt_color |= WHITE;
	else if (isdigit(argv[argc-1][0]))
	    *opt_color = atoi(argv[argc-1]);
	else    
		*bad_arg = TRUE;
        if(*opt_color < 0 || *opt_color > 15)
	    *bad_arg = TRUE;
  }
}

void parse_clear(argc, argv, option, opt_all, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *opt_all;			/* Clear all switch to set or reset. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -clear specification. */

  if (argc > 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	if (strcmp(argv[0], "all") == 0)
		*opt_all = TRUE;
	else if (strcmp(argv[0], "rest") == 0)
		*opt_all = FALSE;
	else
		*bad_arg = TRUE;
  } else {
	*opt_all = TRUE;
  }
}

void parse_blank(argc, argv, option, opt_all, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *opt_all;			/* Clear all switch to set or reset. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -clear specification. */

  if (argc > 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	*opt_all = atoi(argv[0]);
	if ((*opt_all > 60) || (*opt_all < 0))
		*bad_arg = TRUE;
  } else {
	*opt_all = 0;
  }
}

#if 0
void parse_standout(argc, argv, option, opt_all, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *opt_all;			/* Clear all switch to set or reset. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -clear specification. */

  if (argc > 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	*opt_all = atoi(argv[0]);
  } else {
	*opt_all = -1;
  }
}
#endif

void parse_msglevel(argc, argv, option, opt_all, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *opt_all;			/* Clear all switch to set or reset. */
int *bad_arg;			/* Set to true if an error is detected. */
{

  if (argc > 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	*opt_all = atoi(argv[0]);
  	if (*opt_all < 0 || *opt_all > 8)
		*bad_arg = TRUE;
  } else {
	*opt_all = -1;
  }
}

void parse_snap(argc, argv, option, opt_all, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *opt_all;			/* Clear all switch to set or reset. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -clear specification. */

  if (argc > 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	*opt_all = atoi(argv[0]);
	if ((*opt_all <= 0))
		*bad_arg = TRUE;
  } else {
	*opt_all = 0;
  }
}

void parse_snapfile(argc, argv, option, opt_all, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *opt_all;			/* Clear all switch to set or reset. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a -clear specification. */

  if (argc != 1 || *option) *bad_arg = TRUE;
  *option = TRUE;
  if (argc == 1) {
	  strcpy((char *)opt_all, argv[0]);
  }
}

void parse_tabs(argc, argv, option, tab_array, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *tab_array;			/* Array of tabs */
int *bad_arg;			/* Set to true if an error is detected. */
{
  if (*option || argc > 160) *bad_arg = TRUE;
  *option = TRUE;
  tab_array[argc] = -1;
  while(argc--) {
    tab_array[argc] = atoi(argv[argc]);
    if(tab_array[argc] < 1 || tab_array[argc] > 160) {
      *bad_arg = TRUE;
      return;
    }
  }
}

void parse_clrtabs(argc, argv, option, tab_array, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *tab_array;			/* Array of tabs */
int *bad_arg;			/* Set to true if an error is detected. */
{
  if (*option || argc > 160) *bad_arg = TRUE;
  *option = TRUE;
  if(argc == 0) {
    tab_array[0] = -1;
    return;
  }
  tab_array[argc] = -1;
  while(argc--) {
    tab_array[argc] = atoi(argv[argc]);
    if(tab_array[argc] < 1 || tab_array[argc] > 160) {
      *bad_arg = TRUE;
      return;
    }
  }
}

void parse_regtabs(argc, argv, option, opt_len, bad_arg)
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *option;			/* Clear flag to set. */
int *opt_len;			/* Regular tab length. */
int *bad_arg;			/* Set to true if an error is detected. */
{
  if (*option || argc > 1) *bad_arg = TRUE;
  *option = TRUE;
  if(argc == 0) {
    *opt_len = 8;
    return;
  }
  *opt_len = atoi(argv[0]);
  if(*opt_len < 1 || *opt_len > 160) {
    *bad_arg = TRUE;
    return;
  }
}

void show_tabs()
{
  int i, co = tgetnum("co");

  if(co > 0) {
    printf("\r         ");
    for(i = 10; i < co-2; i+=10)
      printf("%-10d", i);
    putchar('\n');
    for(i = 1; i <= co; i++)
      putchar(i%10+'0');
    putchar('\n');
    for(i = 1; i < co; i++)
      printf("\tT\b");
    putchar('\n');
  }
}


#define STRCMP(str1,str2) strncmp(str1,str2,strlen(str1))

void parse_option(option, argc, argv, bad_arg)
char *option;			/* Option with leading '-' removed. */
int argc;			/* Number of arguments for this option. */
char *argv[];			/* Arguments for this option. */
int *bad_arg;			/* Set to true if an error is detected. */
{
/* Parse a single specification. */

  if (STRCMP(option, "term") == 0)
	parse_term(argc, argv, &opt_term, &opt_te_terminal_name, bad_arg);
  else if (STRCMP(option, "reset") == 0)
	parse_none(argc, argv, &opt_reset, bad_arg);
  else if (STRCMP(option, "initialize") == 0)
	parse_none(argc, argv, &opt_initialize, bad_arg);
  else if (STRCMP(option, "cursor") == 0)
	parse_switch(argc, argv, &opt_cursor, &opt_cu_on, bad_arg);
#if 0
  else if (STRCMP(option, "keyboard") == 0)
	parse_keyboard(argc, argv, &opt_keyboard, &opt_ke_type, bad_arg);
#endif
  else if (STRCMP(option, "repeat") == 0)
	parse_switch(argc, argv, &opt_repeat, &opt_rep_on, bad_arg);
  else if (STRCMP(option, "appcursorkeys") == 0)
	parse_switch(argc, argv, &opt_appcursorkeys, &opt_appck_on, bad_arg);
  else if (STRCMP(option, "linewrap") == 0)
	parse_switch(argc, argv, &opt_linewrap, &opt_li_on, bad_arg);
#if 0
  else if (STRCMP(option, "snow") == 0)
	parse_switch(argc, argv, &opt_snow, &opt_sn_on, bad_arg);
  else if (STRCMP(option, "softscroll") == 0)
	parse_switch(argc, argv, &opt_softscroll, &opt_so_on, bad_arg);
#endif
  else if (STRCMP(option, "default") == 0)
	parse_none(argc, argv, &opt_default, bad_arg);
  else if (STRCMP(option, "foreground") == 0)
	par_color(argc, argv, &opt_foreground, &opt_fo_color, bad_arg);
  else if (STRCMP(option, "background") == 0)
	par_color(argc, argv, &opt_background, &opt_ba_color, bad_arg);
  else if (STRCMP(option, "ulcolor") == 0)
	par_color2(argc, argv, &opt_ulcolor, &opt_ul_color, bad_arg);
  else if (STRCMP(option, "hbcolor") == 0)
	par_color2(argc, argv, &opt_hbcolor, &opt_hb_color, bad_arg);
  else if (STRCMP(option, "inversescreen") == 0)
	parse_switch(argc, argv, &opt_inversescreen, &opt_invsc_on, bad_arg);
  else if (STRCMP(option, "bold") == 0)
	parse_switch(argc, argv, &opt_bold, &opt_bo_on, bad_arg);
  else if (STRCMP(option, "half-bright") == 0)
	parse_switch(argc, argv, &opt_halfbright, &opt_hb_on, bad_arg);
  else if (STRCMP(option, "blink") == 0)
	parse_switch(argc, argv, &opt_blink, &opt_bl_on, bad_arg);
  else if (STRCMP(option, "reverse") == 0)
	parse_switch(argc, argv, &opt_reverse, &opt_re_on, bad_arg);
  else if (STRCMP(option, "underline") == 0)
	parse_switch(argc, argv, &opt_underline, &opt_un_on, bad_arg);
  else if (STRCMP(option, "store") == 0)
	parse_none(argc, argv, &opt_store, bad_arg);
  else if (STRCMP(option, "clear") == 0)
	parse_clear(argc, argv, &opt_clear, &opt_cl_all, bad_arg);
  else if (STRCMP(option, "tabs") == 0)
	parse_tabs(argc, argv, &opt_tabs, opt_tb_array, bad_arg);
  else if (STRCMP(option, "clrtabs") == 0)
	parse_clrtabs(argc, argv, &opt_clrtabs, opt_tb_array, bad_arg);
  else if (STRCMP(option, "regtabs") == 0)
	parse_regtabs(argc, argv, &opt_regtabs, &opt_rt_len, bad_arg);
  else if (STRCMP(option, "blank") == 0)
	parse_blank(argc, argv, &opt_blank, &opt_bl_min, bad_arg);
  else if (STRCMP(option, "dump") == 0)
	parse_snap(argc, argv, &opt_snap, &opt_sn_num, bad_arg);
  else if (STRCMP(option, "append") == 0)
	parse_snap(argc, argv, &opt_append, &opt_sn_num, bad_arg);
  else if (STRCMP(option, "file") == 0)
	parse_snapfile(argc, argv, &opt_snapfile, (int *)opt_sn_name, bad_arg);
  else if (STRCMP(option, "msg") == 0)
	parse_switch(argc, argv, &opt_msg, &opt_msg_on, bad_arg);
  else if (STRCMP(option, "msglevel") == 0)
	parse_msglevel(argc, argv, &opt_msglevel, &opt_msglevel_num, bad_arg);
  else if (STRCMP(option, "powersave") == 0)
	parse_switch(argc, argv, &opt_powersave, &opt_ps_on, bad_arg);
#if 0
  else if (STRCMP(option, "standout") == 0)
	parse_standout(argc, argv, &opt_standout, &opt_st_attr, bad_arg);
#endif
  else
	*bad_arg = TRUE;
}

/* End of command line parsing routines. */

void usage(prog_name)
char *prog_name;		/* Name of this program. */
{
/* Print error message about arguments, and the command's syntax. */

  fprintf(stderr, "%s: Argument error, usage\n", prog_name);
  fprintf(stderr, "\n");
  fprintf(stderr, "%s\n", prog_name);
  fprintf(stderr, "  [ -term terminal_name ]\n");
  fprintf(stderr, "  [ -reset ]\n");
  fprintf(stderr, "  [ -initialize ]\n");
  fprintf(stderr, "  [ -cursor [on|off] ]\n");
#if 0
  fprintf(stderr, "  [ -snow [on|off] ]\n");
  fprintf(stderr, "  [ -softscroll [on|off] ]\n");
  fprintf(stderr, "  [ -keyboard pc|olivetti|dutch|extended ]\n");
#endif
  fprintf(stderr, "  [ -repeat [on|off] ]\n");
  fprintf(stderr, "  [ -appcursorkeys [on|off] ]\n");
  fprintf(stderr, "  [ -linewrap [on|off] ]\n");
  fprintf(stderr, "  [ -default ]\n");
  fprintf(stderr, "  [ -foreground black|blue|green|cyan");
  fprintf(stderr, "|red|magenta|yellow|white|default ]\n");
  fprintf(stderr, "  [ -background black|blue|green|cyan");
  fprintf(stderr, "|red|magenta|yellow|white|default ]\n");
  fprintf(stderr, "  [ -ulcolor black|grey|blue|green|cyan");
  fprintf(stderr, "|red|magenta|yellow|white ]\n");
  fprintf(stderr, "  [ -ulcolor bright blue|green|cyan");
  fprintf(stderr, "|red|magenta|yellow|white ]\n");
  fprintf(stderr, "  [ -hbcolor black|grey|blue|green|cyan");
  fprintf(stderr, "|red|magenta|yellow|white ]\n");
  fprintf(stderr, "  [ -hbcolor bright blue|green|cyan");
  fprintf(stderr, "|red|magenta|yellow|white ]\n");
#if 0
  fprintf(stderr, "  [ -standout [ attr ] ]\n");
#endif
  fprintf(stderr, "  [ -inversescreen [on|off] ]\n");
  fprintf(stderr, "  [ -bold [on|off] ]\n");
  fprintf(stderr, "  [ -half-bright [on|off] ]\n");
  fprintf(stderr, "  [ -blink [on|off] ]\n");
  fprintf(stderr, "  [ -reverse [on|off] ]\n");
  fprintf(stderr, "  [ -underline [on|off] ]\n");
  fprintf(stderr, "  [ -store ]\n");
  fprintf(stderr, "  [ -clear [all|rest] ]\n");
  fprintf(stderr, "  [ -tabs [ tab1 tab2 tab3 ... ] ]      (tabn = 1-160)\n");
  fprintf(stderr, "  [ -clrtabs [ tab1 tab2 tab3 ... ] ]   (tabn = 1-160)\n");
  fprintf(stderr, "  [ -regtabs [1-160] ]\n");
  fprintf(stderr, "  [ -blank [0-60] ]\n");
  fprintf(stderr, "  [ -dump   [1-NR_CONSOLES] ]\n");
  fprintf(stderr, "  [ -append [1-NR_CONSOLES] ]\n");
  fprintf(stderr, "  [ -file dumpfilename ]\n");
  fprintf(stderr, "  [ -msg [on|off] ]\n");
  fprintf(stderr, "  [ -msglevel [0-8] ]\n");
  fprintf(stderr, "  [ -powersave [on|off] ]\n");
}

char tc_ent_buf[TC_ENT_SIZE];	/* Buffer for storing a termcap entry. */

char *tc_entry(name)
char *name;			/* Termcap capability string to lookup. */
{
/* Return the specified termcap string, or an empty string if no such termcap
 * capability exists.
 */

  char *buf_ptr;

  buf_ptr = tc_ent_buf;
  if (tgetstr(name, &buf_ptr) == NULL) tc_ent_buf[0] = '\0';
  return tc_ent_buf;
}

void perform_sequence(vcterm)
int vcterm;			/* Set if terminal is a virtual console. */
{
  int result;
/* Perform the selected options. */

  /* -reset. */
  if (opt_reset) {
	printf("%s", tc_entry("rs"));
  }

  /* -initialize. */
  if (opt_initialize) {
	printf("%s", tc_entry("is"));
  }

  /* -cursor [on|off]. */
  if (opt_cursor) {
	if (opt_cu_on)
		printf("%s", tc_entry("ve"));
	else
		printf("%s", tc_entry("vi"));
  }

#if 0
  /* -keyboard pc|olivetti|dutch|extended.  Vc only. */
  if (opt_keyboard && vcterm) {
	switch (opt_ke_type) {
	    case PC:
		printf("%s%s%s", DCS, "keyboard.pc", ST);
		break;
	    case OLIVETTI:
		printf("%s%s%s", DCS, "keyboard.olivetti", ST);
		break;
	    case DUTCH:
		printf("%s%s%s", DCS, "keyboard.dutch", ST);
		break;
	    case EXTENDED:
		printf("%s%s%s", DCS, "keyboard.extended", ST);
		break;
	}
  }
#endif

  /* -linewrap [on|off]. Vc only (vt102) */
  if (opt_linewrap && vcterm) {
	if (opt_li_on)
		printf("\033[?7h");
	else
		printf("\033[?7l");
  }

  /* -repeat [on|off]. Vc only (vt102) */
  if (opt_repeat && vcterm) {
	if (opt_rep_on)
		printf("\033[?8h");
	else
		printf("\033[?8l");
  }

  /* -appcursorkeys [on|off]. Vc only (vt102) */
  if (opt_appcursorkeys && vcterm) {
	if (opt_appck_on)
		printf("\033[?1h");
	else
		printf("\033[?1l");
  }

#if 0
  /* -snow [on|off].  Vc only. */
  if (opt_snow && vcterm) {
	if (opt_sn_on)
		printf("%s%s%s", DCS, "snow.on", ST);
	else
		printf("%s%s%s", DCS, "snow.off", ST);
  }

  /* -softscroll [on|off].  Vc only. */
  if (opt_softscroll && vcterm) {
	if (opt_so_on)
		printf("%s%s%s", DCS, "softscroll.on", ST);
	else
		printf("%s%s%s", DCS, "softscroll.off", ST);
  }
#endif

  /* -default.  Vc sets default rendition, otherwise clears all
   * attributes.
   */
  if (opt_default) {
	if (vcterm)
		printf("\033[0m");
	else
		printf("%s", tc_entry("me"));
  }

  /* -foreground black|red|green|yellow|blue|magenta|cyan|white|default.
   * Vc only (ANSI).
   */
  if (opt_foreground && vcterm) {
	printf("%s%s%c%s", ESC, "[3", '0' + opt_fo_color, "m");
  }

  /* -background black|red|green|yellow|blue|magenta|cyan|white|default.
   * Vc only (ANSI).
   */
  if (opt_background && vcterm) {
	printf("%s%s%c%s", ESC, "[4", '0' + opt_ba_color, "m");
  }

  /* -ulcolor black|red|green|yellow|blue|magenta|cyan|white|default.
   * Vc only.
   */
  if (opt_ulcolor && vcterm) {
	printf("\033[1;%d]", opt_ul_color);
  }

  /* -hbcolor black|red|green|yellow|blue|magenta|cyan|white|default.
   * Vc only.
   */
  if (opt_hbcolor && vcterm) {
	printf("\033[2;%d]", opt_hb_color);
  }

  /* -inversescreen [on|off].  Vc only (vt102).
   */
  if (opt_inversescreen) {
	if (vcterm)
		if (opt_invsc_on)
			printf("\033[?5h");
		else
			printf("\033[?5l");
  }

  /* -bold [on|off].  Vc behaves as expected, otherwise off turns off
   * all attributes.
   */
  if (opt_bold) {
	if (opt_bo_on)
		printf("%s", tc_entry("md"));
	else {
		if (vcterm)
			printf("%s%s", ESC, "[22m");
		else
			printf("%s", tc_entry("me"));
	}
  }

  /* -half-bright [on|off].  Vc behaves as expected, otherwise off turns off
   * all attributes.
   */
  if (opt_halfbright) {
	if (opt_hb_on)
		printf("%s", tc_entry("mh"));
	else {
		if (vcterm)
			printf("%s%s", ESC, "[22m");
		else
			printf("%s", tc_entry("me"));
	}
  }

  /* -blink [on|off].  Vc behaves as expected, otherwise off turns off
   * all attributes.
   */
  if (opt_blink) {
	if (opt_bl_on)
		printf("%s", tc_entry("mb"));
	else {
		if (vcterm)
			printf("%s%s", ESC, "[25m");
		else
			printf("%s", tc_entry("me"));
	}
  }

  /* -reverse [on|off].  Vc behaves as expected, otherwise off turns
   * off all attributes.
   */
  if (opt_reverse) {
	if (opt_re_on)
		printf("%s", tc_entry("mr"));
	else {
		if (vcterm)
			printf("%s%s", ESC, "[27m");
		else
			printf("%s", tc_entry("me"));
	}
  }

  /* -underline [on|off]. */
  if (opt_underline) {
	if (opt_un_on)
		printf("%s", tc_entry("us"));
	else
		printf("%s", tc_entry("ue"));
  }

  /* -store.  Vc only. */
  if (opt_store && vcterm) {
	printf("\033[8]");
  }

  /* -clear [all|rest]. */
  if (opt_clear) {
	if (opt_cl_all)
		printf("%s", tc_entry("cl"));
	else
		printf("%s", tc_entry("cd"));
  }

  /* -tabs Vc only. */
  if (opt_tabs && vcterm) {
    int i;

    if (opt_tb_array[0] == -1)
      show_tabs();
    else {
      for(i=0; opt_tb_array[i] > 0; i++)
        printf("\033[%dG\033H", opt_tb_array[i]);
      putchar('\r');
    }
  }

  /* -clrtabs Vc only. */
  if (opt_clrtabs && vcterm) {
    int i;

    if (opt_tb_array[0] == -1)
      printf("\033[3g");
    else
      for(i=0; opt_tb_array[i]; i++)
        printf("\033[%dG\033[g", opt_tb_array[i]);
    putchar('\r');
  }

  /* -regtabs Vc only. */
  if (opt_regtabs && vcterm) {
    int i;

    printf("\033[3g\r");
    for(i=opt_rt_len+1; i<=160; i+=opt_rt_len)
      printf("\033[%dC\033H",opt_rt_len);
    putchar('\r');
  }

  /* -blank [0-60]. */
  if (opt_blank) 
    printf("\033[9;%d]", opt_bl_min);
    
  /* -powersave [on|off] (console) */
  if (opt_powersave) {
        char ioctlarg[2];
	ioctlarg[0] = 10;	/* powersave */
	ioctlarg[1] = opt_ps_on;
        if (ioctl(0,TIOCLINUX,ioctlarg))
	    fprintf(stderr,"cannot (un)set powersave mode\n");
  }

#if 0
  /* -standout [num]. */
  if (opt_standout)
	/* nothing */;
#endif

  /* -snap [1-NR_CONS]. */
  if (opt_snap || opt_append) {
      FILE *F;

      F = fopen(opt_sn_name, opt_snap ? "w" : "a");
      if (!F) {
	  perror(opt_sn_name);
	  fprintf(stderr,"setterm: can not open dump file %s for output\n",
		  opt_sn_name); 
	  exit(-1);
      }
      screendump(opt_sn_num, F);
      fclose(F);
  }

  /* -msg [on|off]. */
  if (opt_msg && vcterm) {
       if (opt_msg_on)
               /* 7 -- Enable printk's to console */
               result = syslog(7, NULL, 0);
       else
               /*  6 -- Disable printk's to console */
               result = syslog(6, NULL, 0);

       if (result != 0)
               printf("syslog error: %s\n", strerror(result));
  }

  /* -msglevel [0-8] */
  if (opt_msglevel && vcterm) {
       /* 8 -- Set level of messages printed to console */
       result = syslog(8, NULL, opt_msglevel_num);
       if (result != 0)
               printf("syslog error: %s\n", strerror(result));
   }
}

extern char *malloc();

screendump(int vcnum, FILE *F){
#include <sys/param.h>
    char infile[MAXPATHLEN];
    unsigned char header[4];
    unsigned int rows, cols;
    int fd, i, j;
    char *inbuf, *outbuf, *p, *q;

    sprintf(infile, "/dev/vcsa%d", vcnum);
    fd = open(infile, 0);
    if (fd < 0 || read(fd, header, 4) != 4)
      goto try_ioctl;
    rows = header[0];
    cols = header[1];
    if (rows * cols == 0)
      goto try_ioctl;
    inbuf = malloc(rows*cols*2);
    outbuf = malloc(rows*(cols+1));
    if(!inbuf || !outbuf) {
	fprintf(stderr, "Out of memory\n");
	exit(1);
    }
    if (read(fd, inbuf, rows*cols*2) != rows*cols*2) {
	fprintf(stderr, "Error reading %s\n", infile);
	exit(1);
    }
    p = inbuf;
    q = outbuf;
    for(i=0; i<rows; i++) {
	for(j=0; j<cols; j++) {
	    *q++ = *p;
	    p += 2;
	}
	while(j-- > 0 && q[-1] == ' ')
	  q--;
	*q++ = '\n';
    }
    if (fwrite(outbuf, 1, q-outbuf, F) != q-outbuf) {
	fprintf(stderr, "Error writing screendump\n");
	exit(1);
    }
    return;

try_ioctl:
    {
#define NUM_COLS 160
#define NUM_ROWS 75
	char buf[NUM_COLS+1];
	unsigned char screenbuf[NUM_ROWS*NUM_COLS];
	screenbuf[0] = 0;
	screenbuf[1] = (unsigned char) vcnum;
	if (ioctl(0,TIOCLINUX,screenbuf) < 0) {
	    fprintf(stderr,"couldn't read %s, and cannot ioctl dump\n",
		    infile);
	    exit(1);
	}
	rows = screenbuf[0];
	cols = screenbuf[1];
	for (i=0; i<rows; i++) {
	    strncpy(buf, screenbuf+2+(cols*i), cols);
	    buf[cols] = '\0';
	    j = cols;
	    while (--j && (buf[j] == ' '))
	      buf[j] = '\0';
	    fputs(buf,F);
	    fputc('\n',F); 
	}
    }
}

void main(int argc, char **argv)
{
  int bad_arg = FALSE;		/* Set if error in arguments. */
  int arg, modifier;
  char *term;			/* Terminal type. */
  int vcterm;			/* Set if terminal is a virtual console. */

  if (argc < 2) bad_arg = TRUE;

  /* Parse arguments. */

  for (arg = 1; arg < argc;) {
	if (*argv[arg] == '-') {

		/* Parse a single option. */

		for (modifier = arg + 1; modifier < argc; modifier++) {
			if (*argv[modifier] == '-') break;
		}
		parse_option(argv[arg] + 1, modifier - arg - 1,
			     &argv[arg + 1], &bad_arg);
		arg = modifier;
	} else {

		bad_arg = TRUE;
		arg++;
	}
  }

  /* Display syntax message if error in arguments. */

  if (bad_arg) {
	usage(argv[0]);
	exit(1);
  }

  /* Find out terminal name. */

  if (opt_term) {
	term = opt_te_terminal_name;
  } else {
	term = getenv("TERM");
	if (term == NULL) {
		fprintf(stderr, "%s: $TERM is not defined.\n", argv[0]);
		exit(1);
	}
  }

  /* Find termcap entry. */

  if (tgetent(tc_buf, term) != 1) {
	fprintf(stderr, "%s: Could not find termcap entry for %s.\n",
		argv[0], term);
	exit(1);
  }

  /* See if the terminal is a virtual console terminal. */

  vcterm = (!strncmp(term, "con", 3) || !strncmp(term, "linux", 5));

  /* Perform the selected options. */

  perform_sequence(vcterm);

  exit(0);
}
