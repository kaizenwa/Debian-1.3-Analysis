/* gitview.c -- A hex/ascii file viewer.  */

/* Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Tudor Hulubei and Andrei Pitis.  */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#else /* !HAVE_STDLIB_H */
#include "ansi_stdlib.h"
#endif /* !HAVE_STDLIB_H */

#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#include <limits.h>
#include "file.h"
#include <signal.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "stdc.h"
#include "xstring.h"
#include "xmalloc.h"
#include "xio.h"
#include "window.h"
#include "configure.h"
#include "tty.h"
#include "misc.h"
#include "tilde.h"


#define MAX_KEYS                        2048

#define BUILTIN_OPERATIONS              11


#define BUILTIN_previous_line           -1
#define BUILTIN_next_line               -2
#define BUILTIN_scroll_down             -3
#define BUILTIN_scroll_up               -4
#define BUILTIN_beginning_of_file       -5
#define BUILTIN_end_of_file             -6
#define BUILTIN_refresh                 -7
#define BUILTIN_exit                    -8
#define BUILTIN_hard_refresh            -9
#define BUILTIN_backspace               -10
#define BUILTIN_action                  -11


#define MAX_BUILTIN_NAME                20

char built_in[BUILTIN_OPERATIONS][MAX_BUILTIN_NAME] =
{
    "previous-line",
    "next-line",
    "scroll-down",
    "scroll-up",
    "beginning-of-file",
    "end-of-file",
    "refresh",
    "exit",
    "hard-refresh",
    "backspace",
    "action",
};


int SCREEN_X;
int SCREEN_Y;

#define VIEWER_FIELDS 12

static char *ViewerFields[VIEWER_FIELDS] =
{
    "TitleForeground",
    "TitleBackground",
    "TitleBrightness",
    "HeaderForeground",
    "HeaderBackground",
    "HeaderBrightness",
    "ScreenForeground",
    "ScreenBackground",
    "ScreenBrightness",
    "StatusForeground",
    "StatusBackground",
    "StatusBrightness"
};

#ifdef HAVE_LINUX
static int ViewerColors[VIEWER_FIELDS] =
{
    CYAN, BLUE, ON, CYAN, RED, ON, BLACK, CYAN, OFF, CYAN, BLUE, ON
};
#else   /* !HAVE_LINUX */
static int ViewerColors[VIEWER_FIELDS] =
{
    BLACK, WHITE, OFF, WHITE, BLACK, ON, WHITE, BLACK, OFF, BLACK, WHITE, OFF
};
#endif  /* !HAVE_LINUX */

#define TitleForeground                 ViewerColors[0]
#define TitleBackground                 ViewerColors[1]
#define TitleBrightness                 ViewerColors[2]
#define HeaderForeground                ViewerColors[3]
#define HeaderBackground                ViewerColors[4]
#define HeaderBrightness                ViewerColors[5]
#define ScreenForeground                ViewerColors[6]
#define ScreenBackground                ViewerColors[7]
#define ScreenBrightness                ViewerColors[8]
#define StatusForeground                ViewerColors[9]
#define StatusBackground                ViewerColors[10]
#define StatusBrightness                ViewerColors[11]


#ifdef HAVE_LINUX
extern int LinuxConsole;
#endif /* HAVE_LINUX */

#ifdef HAVE_LINUX
int AnsiColors = ON;
#else   /* !HAVE_LINUX */
int AnsiColors = OFF;
#endif  /* !HAVE_LINUX */


#ifdef STAT_MACROS_BROKEN
#ifdef S_IFREG
#undef S_IFREG
#endif
#ifdef S_IFBLK
#undef S_IFBLK
#endif
#endif /* STAT_MACROS_BROKEN */


#ifndef S_IFREG
#define S_IFREG         0100000
#endif

#ifndef S_IFBLK
#define S_IFBLK         0060000
#endif

#ifndef S_ISREG
#define S_ISREG(m)      (((m) & S_IFMT) == S_IFREG)
#endif

#ifndef S_ISBLK
#define S_ISBLK(m)      (((m) & S_IFMT) == S_IFBLK)
#endif

/* Finally ... :-( */


char *home;
pid_t pid;
char *program;
char *screen;
char *header_text;
int  UseLastScreenChar;
char *global_buf = NULL;
char cSection[]  = "[GITVIEW-Color]";
char bwSection[] = "[GITVIEW-Monochrome]";
int fd, regular_file;
int current_line, lines;
window_t *title_win, *header_win, *screen_win, *status_win;
#ifdef HAVE_GCC
static char title_text[] = " "PRODUCT" "VERSION" - Hex/Ascii File Viewer";
#else
static char title_text[] = " GNU Interactive Tools 4.3.16 - Hex/Ascii File Viewer";
#endif /* !HAVE_GCC */
static char *GitViewModeHelp;
static char info_txt[] =
    "   Offset    00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F        \
Ascii       ";
static char line_txt[]    =
    "  ------------------------------------------------------------------\
----------  ";
static char seek_txt[]    = "  Seek at: ";


int
file_length()
{
    int current, length;

    if (!regular_file)
	return 0x7FFFFFFF;

    current = lseek(fd, 0, SEEK_CUR);
    length  = lseek(fd, 0, SEEK_END);
    lseek(fd, current, SEEK_SET);
    return length;
}


void
settitle()
{
    memset(global_buf, ' ', SCREEN_X);
    memcpy(global_buf, title_text, strlen(title_text));

    tty_colors(TitleBrightness, TitleForeground, TitleBackground);

    window_goto(title_win, 0, 0);
    window_puts(global_buf, SCREEN_X);
}


void
setheader()
{
    memset(global_buf, ' ', SCREEN_X);
    memcpy(global_buf, header_text, min(strlen(header_text), SCREEN_X));

    tty_colors(HeaderBrightness, HeaderForeground, HeaderBackground);

    window_goto(header_win, 0, 0);
    window_puts(global_buf, SCREEN_X);
}


void
setstatus()
{
    memset(global_buf, ' ', SCREEN_X);
    memcpy(global_buf, GitViewModeHelp, strlen(GitViewModeHelp));

    tty_colors(StatusBrightness, StatusForeground, StatusBackground);

    window_goto(status_win, 0, 0);

    if (UseLastScreenChar)
	window_puts(global_buf, SCREEN_X);
    else
	window_puts(global_buf, SCREEN_X - 1);
}


char
char_to_print(c)
    char c;
{
    return is_print(c) ? c : '.';
}


void
update_line(line)
    int line;
{
    int r;
    unsigned char ln[16];

    memset(ln, 0, 16);
    memset(global_buf, ' ', SCREEN_X);
    lseek(fd, line * 16, SEEK_SET);

    if ((r = read(fd, ln, 16)))
	sprintf(global_buf, "  %07X0   %02X %02X %02X %02X %02X %02X %02X\
 %02X %02X %02X %02X %02X %02X %02X %02X %02X  %c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c  ",
		line,
		ln[0], ln[1], ln[2],  ln[3],  ln[4],  ln[5],  ln[6],  ln[7],
		ln[8], ln[9], ln[10], ln[11], ln[12], ln[13], ln[14], ln[15],
		char_to_print(ln[0]),  char_to_print(ln[1]),
		char_to_print(ln[2]),  char_to_print(ln[3]),
		char_to_print(ln[4]),  char_to_print(ln[5]),
		char_to_print(ln[6]),  char_to_print(ln[7]),
		char_to_print(ln[8]),  char_to_print(ln[9]),
		char_to_print(ln[10]), char_to_print(ln[11]),
		char_to_print(ln[12]), char_to_print(ln[13]),
		char_to_print(ln[14]), char_to_print(ln[15]));

    if (r < 0)
	r = 0;

    if (r != 16)
    {
	memset(global_buf + 13 + r * 3, ' ', (16 - r) * 3 - 1);
	memset(global_buf + 13 + 16 * 3 - 1 + 2 + r, ' ', 16 - r + 2);
    }

    window_puts((char *)global_buf, SCREEN_X);
}


void
update_all()
{
    int i;

    tty_colors(ScreenBrightness, ScreenForeground, ScreenBackground);

    for (i = current_line; i < current_line + 16; i++)
    {
	window_goto(screen_win, 3 + i - current_line, 0);
	update_line(i);
    }
}


void
clean_up()
{
    tty_exit(NULL);
}


void
fatal(postmsg)
    char *postmsg;
{
    clean_up();
    fprintf(stderr, "%s: fatal error: %s.\n", program, postmsg);
    exit(1);
}


RETSIGTYPE
panic(signum)
    int signum;
{
    fatal_signal(signum);
}


int
read_keys(keys)
    int keys;
{
    char *contents;
    char key_seq[80];
    int i, j, need_conversion;


    for (i = keys; i < MAX_KEYS; i++)
    {
	configuration_getvarinfo(key_seq, &contents, 1, NO_SEEK);

	if (*key_seq == 0)
	    break;

	if (*key_seq != '^')
	{
	    char *key_seq_ptr = tty_get_symbol_key_seq(key_seq);

	    if (key_seq_ptr)
	    {
		/* Ignore empty/invalid key sequences.  */
		if (*key_seq_ptr == '\0')
		    continue;

		/* We got the key sequence in the correct form, as
		   returned by tgetstr, so there is no need for
		   further conversion.  */
		strcpy(key_seq, key_seq_ptr);
		need_conversion = 0;
	    }
	    else
	    {
		/* This is not a TERMCAP symbol, it is a key sequence
		   that we will have to convert it with
		   tty_key_convert() into a machine usable form before
		   using it.  */
		need_conversion = 1;
	    }
	}
	else
	    need_conversion = 1;

	if (contents == NULL)
	    continue;

	for (j = 0; j < BUILTIN_OPERATIONS; j++)
	    if (strcmp(contents, built_in[j]) == 0)
		break;

	if (j < BUILTIN_OPERATIONS)
	{
	    if (!need_conversion || tty_key_convert((unsigned char *)key_seq))
		tty_key_list_insert((unsigned char *)key_seq,  built_in[-j-1]);
	}
	else
	    fprintf(stderr, "%s: invalid built-in operation: %s.\n",
		    program, contents);
    }

    return i;
}


int
main(argc, argv)
    int argc;
    char *argv[];
{
    char c;
    struct stat s;
    tty_key_t *ks;
    char offset[16];
    int cnt = 0, size;
    int key, first_time = 1;
    int keys, repeat_count, need_update;


    program = argv[0];

    home = getenv("HOME");
    if (home == NULL)
	home = ".";

    get_tty_name();
    get_login_name();

    tty_name_len = strlen(tty_name);

    tty_get_capabilities();
    tty_kbdinit(TTY_RESTRICTED_INPUT);

    signal(SIGTERM, panic);
    signal(SIGINT , panic);
    signal(SIGQUIT, panic);
    signal(SIGSEGV, panic);
    signal(SIGHUP,  panic);

    signal(SIGILL,  SIG_IGN);
    signal(SIGTRAP, SIG_IGN);
    signal(SIGABRT, SIG_IGN);
    signal(SIGUSR1, SIG_IGN);
    signal(SIGUSR2, SIG_IGN);

#ifdef SIGTSTP
    signal(SIGTSTP, SIG_IGN);
#endif

#ifdef SIGCONT
    signal(SIGCONT, SIG_IGN);
#endif

    signal(SIGALRM, SIG_IGN);
    signal(SIGPIPE, SIG_IGN);
    signal(SIGFPE,  SIG_IGN);

    if (argc != 2)
    {
	fprintf(stderr, "usage: gitview filename\n");
	exit(1);
    }

    xstat(argv[1], &s);

/*
    if (!(S_ISREG(s.st_mode) || S_ISBLK(s.st_mode)))
    {
	fprintf(stderr, "%s: %s is neither regular file nor block device.\n",
		program, argv[1]);
	exit(1);
    }
*/
    fd = open(argv[1], O_RDONLY);

    if (fd == -1)
    {
	fprintf(stderr, "%s: cannot open file %s.\n", program, argv[1]);
	exit(1);
    }

    regular_file = S_ISREG(s.st_mode);

    common_configuration_init();
    use_section("[GITVIEW-Keys]");
    keys = read_keys(0);
    configuration_end();

    specific_configuration_init();

    use_section("[Setup]");

    AnsiColors         = get_flag_var("AnsiColors", OFF);
    UseLastScreenChar  = get_flag_var("UseLastScreenChar", OFF);


    use_section("[GITVIEW-Setup]");

    GitViewModeHelp = get_string_var("GitViewModeHelp", "");

    use_section(AnsiColors ? cSection : bwSection);

    get_colorset_var(ViewerColors, ViewerFields, VIEWER_FIELDS);


    use_section("[GITVIEW-Keys]");

    keys = read_keys(keys);

    if (keys == MAX_KEYS)
	fprintf(stderr, "%s: too many key sequences; only %d are allowed.\n",
		program, MAX_KEYS);

    configuration_end();

    tty_get_size(&SCREEN_X, &SCREEN_Y);
    tty_startup(UseLastScreenChar);

    if (SCREEN_Y < 24)
    {
	SCREEN_Y = 24;
	fprintf(stderr, "%s: WARNING: can't use less than 24 columns.\n",
		program);
    }

#ifdef HAVE_LINUX
    if (LinuxConsole)
	screen = xmalloc(4 + SCREEN_X * SCREEN_Y * 2);
#endif  /* HAVE_LINUX */

    global_buf  = xmalloc(SCREEN_X + 1);

    header_text = xmalloc(strlen(argv[1]) + 10);
    sprintf(header_text, " File: %s", argv[1]);

    title_win  = window_init(0, 0,            1,            SCREEN_X);
    header_win = window_init(0, 1,            1,            SCREEN_X);
    screen_win = window_init(0, 2,            SCREEN_Y - 2, SCREEN_X);
    status_win = window_init(0, SCREEN_Y - 1, 1,            SCREEN_X);

    tty_get_screen(screen);
    tty_set_mode(TTY_NONCANONIC);
    tty_defaults();

    offset[cnt]  = 0;
    current_line = 0;


  restart:

    tty_colors(ScreenBrightness, ScreenForeground, ScreenBackground);

    if (first_time)
    {
	tty_fill();
	first_time = 0;
    }

    settitle();
    setstatus();
    setheader();

    tty_colors(ScreenBrightness, ScreenForeground, ScreenBackground);

    window_goto(screen_win, 1, 0);
    window_puts(info_txt, sizeof(info_txt) - 1);
    window_goto(screen_win, 2, 0);
    window_puts(line_txt, sizeof(line_txt) - 1);

    size = file_length();
    lines = size / 16 + (size % 16 ? 1 : 0);

    current_line = min(current_line, (lines / 16) * 16);

    update_all();

    window_goto(screen_win, 20, 0);
    window_puts(seek_txt, sizeof(seek_txt) - 1);
    window_goto(screen_win, 20, sizeof(seek_txt) - 1);
    window_puts(offset, cnt);

    window_goto(screen_win, 20, sizeof(seek_txt) - 1 + cnt);

    first_time = 1;

    while (1)
    {
	ks  = tty_get_key(&repeat_count);

	if (ks->aux_data == NULL)
	    key = ks->key_seq[0];
	else
	    key = ((char *)ks->aux_data - (char *)built_in) / MAX_BUILTIN_NAME;

	size = file_length();
	lines = size / 16 + (size % 16 ? 1 : 0);

	switch (key)
	{
	    case key_INTERRUPT:
		goto end;

	    case BUILTIN_previous_line:

		need_update = 0;

		while (repeat_count--)
		{
		    if (current_line == 0)
			break;

		    current_line--, need_update = 1;
		}

		if (need_update)
		    update_all();

		break;

	    case BUILTIN_next_line:

		need_update = 0;

		while (repeat_count--)
		{
		    if (current_line >= lines - 16)
			break;

		    current_line++, need_update = 1;
		}

		if (need_update)
		    update_all();

		break;

	    case BUILTIN_scroll_down:

		if (current_line == 0)
		    break;

		current_line = max(0, current_line - 16);
		update_all();
		break;

	    case BUILTIN_scroll_up:

		if (current_line >= lines - 16)
		    break;

		current_line += 16;
		update_all();
		break;

	    case BUILTIN_beginning_of_file:

		if (current_line)
		{
		    current_line = 0;
		    update_all();
		}

		break;

	    case BUILTIN_end_of_file:

		if (regular_file && current_line < lines - 16)
		{
		    current_line = lines - 16;
		    update_all();
		}

		break;

	    case BUILTIN_hard_refresh:

		first_time = 1;

	    case BUILTIN_refresh:

		goto restart;

	    case '0': case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9': case 'A': case 'B':
	    case 'C': case 'D': case 'E': case 'F': case 'a': case 'b':
	    case 'c': case 'd': case 'e': case 'f':
		if (cnt < 8)
		{
		    window_goto(screen_win, 20, strlen(seek_txt) + cnt);
		    c = (char)key;
		    window_putc(c);
		    offset[cnt++] = c;
		}
		else
		    tty_beep();
		break;

	    case BUILTIN_backspace:

		if (cnt)
		    cnt--;

		window_goto(screen_win, 20, strlen(seek_txt) + cnt);
		window_putc(' ');
		break;

	    case BUILTIN_action:

		if (cnt == 0)
		    tty_beep();
		else
		{
		    offset[cnt] = 0;
		    sscanf(offset, "%x", &cnt);
		    window_goto(screen_win, 20, strlen(seek_txt));
		    window_puts("        ", 8);

		    if (cnt < 0)
			cnt = 0;

		    if (cnt > size)
			cnt = size;

		    current_line = cnt >> 4;
		    update_all();
		    cnt = 0;
		}

		break;

	    case BUILTIN_exit:
		goto end;

	    default:
		break;
	}

	window_goto(screen_win, 20, strlen(seek_txt) + cnt);
    }

  end:

    tty_exit(screen);
    return 0;
}
