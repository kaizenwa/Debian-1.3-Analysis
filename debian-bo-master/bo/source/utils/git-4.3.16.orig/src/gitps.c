/* gitps.c -- A process viewer/killer utility.  */

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

#include <ctype.h>
#include <limits.h>
#include "file.h"
#include <fcntl.h>
#include <signal.h>

/* SVR2/SVR3.  */
#if !(defined(SIGCHLD)) && defined(SIGCLD)
#define SIGCHLD	SIGCLD
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <errno.h>

/* Not all systems declare ERRNO in errno.h... and some systems #define it! */
#if !defined (errno)
extern int errno;
#endif /* !errno */

#include "stdc.h"
#include "xstring.h"
#include "xmalloc.h"
#include "tty.h"
#include "window.h"
#include "configure.h"
#include "tilde.h"
#include "misc.h"


int SCREEN_X;
int SCREEN_Y;

#define MAX_KEYS        2048
#define MAX_PROCESSES   1024
#define PS_FIELDS         12


static char *PSFields[PS_FIELDS] =
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
static int PSColors[PS_FIELDS] =
{
    CYAN, BLUE, ON, CYAN, RED, ON, BLACK, CYAN, OFF, CYAN, BLUE, ON
};
#else   /* !HAVE_LINUX */
static int PSColors[PS_FIELDS] =
{
    BLACK, WHITE, OFF, WHITE, BLACK, ON, WHITE, BLACK, OFF, BLACK, WHITE, OFF
};
#endif  /* !HAVE_LINUX */

#define TitleForeground                 PSColors[0]
#define TitleBackground                 PSColors[1]
#define TitleBrightness                 PSColors[2]
#define HeaderForeground                PSColors[3]
#define HeaderBackground                PSColors[4]
#define HeaderBrightness                PSColors[5]
#define ScreenForeground                PSColors[6]
#define ScreenBackground                PSColors[7]
#define ScreenBrightness                PSColors[8]
#define StatusForeground                PSColors[9]
#define StatusBackground                PSColors[10]
#define StatusBrightness                PSColors[11]


#ifdef HAVE_LINUX
extern int LinuxConsole;
#endif /* HAVE_LINUX */

#ifdef HAVE_LINUX
int AnsiColors = ON;
#else   /* !HAVE_LINUX */
int AnsiColors = OFF;
#endif  /* !HAVE_LINUX */


char cSection[]  = "[GITPS-Color]";
char bwSection[] = "[GITPS-Monochrome]";
int  processes;
int  PID_index;
int  signal_type = 11;  /* index of SIGTERM in signals table */

pid_t pid;
char *home;
char *program;
char *header_text;
char *TempDirectory;
int UseLastScreenChar;
int StartupScrollStep;
char *stdout_log_name;
char *stderr_log_name;
char *ps_vect[MAX_PROCESSES];
char *screen;
char *global_buf;
int first_on_screen, current_process, scroll_step;
window_t *title_win, *header_win, *screen_win, *status_win;
#ifdef HAVE_GCC
static char title_text[] = " "PRODUCT" "VERSION" - Process Viewer/Killer";
#else
static char title_text[] = " GNU Interactive Tools 4.3.16 - Process Viewer/Killer";
#endif /* !HAVE_GCC */
static char *GitPsModeHelp;
static char no_perm[] = "not owner !";
static char no_proc[] = "no such process ! (REFRESH recommended)";


struct SIGNAL
{
    char signame[8];
    int  signal;
};

static struct SIGNAL signals[] =
{
    { "SIGHUP ", SIGHUP  },
    { "SIGINT ", SIGINT  },
    { "SIGQUIT", SIGQUIT },
    { "SIGILL ", SIGILL  },
    { "SIGFPE ", SIGFPE  },
    { "SIGKILL", SIGKILL },
    { "SIGUSR1", SIGUSR1 },
    { "SIGSEGV", SIGSEGV },
    { "SIGUSR2", SIGUSR2 },
    { "SIGPIPE", SIGPIPE },
    { "SIGALRM", SIGALRM },
    { "SIGTERM", SIGTERM },
    { "SIGCHLD", SIGCHLD },

#ifdef SIGSTOP
    { "SIGSTOP", SIGSTOP },
#endif

#ifdef SIGTSTP
    { "SIGTSTP", SIGTSTP },
#endif

#ifdef SIGCONT
    { "SIGCONT", SIGCONT },
#endif

    { "SIGABRT", SIGABRT },
    { "SIGTRAP", SIGTRAP },
};


#define BUILTIN_OPERATIONS              26


#define BUILTIN_previous_line            0
#define BUILTIN_next_line                1
#define BUILTIN_scroll_down              2
#define BUILTIN_scroll_up                3
#define BUILTIN_beginning_of_list        4
#define BUILTIN_end_of_list              5
#define BUILTIN_next_signal              6
#define BUILTIN_SIGHUP                   7
#define BUILTIN_SIGINT                   8
#define BUILTIN_SIGQUIT                  9
#define BUILTIN_SIGILL                  10
#define BUILTIN_SIGFPE                  11
#define BUILTIN_SIGKILL                 12
#define BUILTIN_SIGUSR1                 13
#define BUILTIN_SIGSEGV                 14
#define BUILTIN_SIGUSR2                 15
#define BUILTIN_SIGPIPE                 16
#define BUILTIN_SIGALRM                 17
#define BUILTIN_SIGTERM                 18
#define BUILTIN_SIGCHLD                 19
#define BUILTIN_SIGCONT                 20
#define BUILTIN_kill_process            21
#define BUILTIN_refresh                 22
#define BUILTIN_exit                    23
#define BUILTIN_hard_refresh            24


#define MAX_BUILTIN_NAME                20

char built_in[BUILTIN_OPERATIONS][MAX_BUILTIN_NAME] =
{
    "previous-line",
    "next-line",
    "scroll-down",
    "scroll-up",
    "beginning-of-list",
    "end-of-list",
    "next-signal",
    "SIGHUP",
    "SIGINT",
    "SIGQUIT",
    "SIGILL",
    "SIGFPE",
    "SIGKILL",
    "SIGUSR1",
    "SIGSEGV",
    "SIGUSR2",
    "SIGPIPE",
    "SIGALRM",
    "SIGTERM",
    "SIGCHLD",
    "SIGCONT",
    "kill-process",
    "refresh",
    "exit",
    "hard-refresh",
};


void
remove_log()
{
    if (stdout_log_name) unlink(stdout_log_name);
    if (stderr_log_name) unlink(stderr_log_name);
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
    memcpy(global_buf, header_text, strlen(header_text));

    tty_colors(HeaderBrightness, HeaderForeground, HeaderBackground);

    window_goto(header_win, 0, 0);
    window_puts(global_buf, SCREEN_X);
}


void
setstatus(what)
   char *what;
{
    memset(global_buf, ' ', SCREEN_X);

    if (what)
	memcpy(global_buf, what, strlen(what));
    else
	memcpy(global_buf, GitPsModeHelp, strlen(GitPsModeHelp));

    tty_colors(StatusBrightness, StatusForeground, StatusBackground);

    window_goto(status_win, 0, 0);
    window_puts(global_buf, SCREEN_X - (sizeof(signals[0].signame) - 1) - 1);
}


void
setsignal()
{
    int len = sizeof(signals[0].signame) - 1;

    tty_colors(StatusBrightness, StatusForeground, StatusBackground);

    window_goto(status_win, 0, SCREEN_X - len - 1);
    window_puts(signals[signal_type].signame, len);
    window_putc(' ');
}


void
free_ps_list()
{
    int i;

    for (i = 0; i < MAX_PROCESSES; i++)
	if (ps_vect[i])
	{
	    xfree(ps_vect[i]);
	    ps_vect[i] = NULL;
	}
}


char *
read_ps_line(ps_output, line)
    FILE *ps_output;
    char *line;
{
    int c;
    char *ok;
    size_t lastchar;

    ok = fgets(line, SCREEN_X + 1, ps_output);

    if (line[lastchar = strlen(line) - 1] == '\n')
	line[lastchar] = 0;
    else
	while ((c = fgetc(ps_output)) != '\n' && c != EOF);

    return ok;
}


int
get_PID_index(ps_output)
    FILE *ps_output;
{
    int i;
    char *h = header_text;

    if (read_ps_line(ps_output, header_text) == NULL)
	return -1;

    if (strstr(header_text, "PID") == NULL)
	return -1;

    for (i = 0; ; i++)
    {
	while (isspace(*h))
	    h++;

	if (memcmp(h, "PID", 3) == 0)
	    return i;

	while (!isspace(*h))
	    h++;
    }
}


int
kill_process(process_index)
    int process_index;
{
    int i;
    char pidstr[32];
    char *p = ps_vect[process_index];

    if (p == NULL)
	return 0;

    for (i = 0; i < PID_index; i++)
    {
	while (isspace(*p)) p++;
	if (memcmp(p, "PID", 3) == 0) return i;
	while (!isspace(*p)) p++;
    }

    i = 0;
    while (isspace(*p)) p++;
    while (!isspace(*p)) pidstr[i++] = *p++;
    pidstr[i] = 0;
    return !kill(atoi(pidstr), signals[signal_type].signal);
}


void
build_ps_list(ps_output)
    FILE *ps_output;
{
    int i = 0;

    do
	ps_vect[i] = xmalloc(SCREEN_X + 1);
    while (read_ps_line(ps_output, ps_vect[i++]));

    xfree(ps_vect[--i]);
    ps_vect[i] = NULL;
    processes = i;
}


void
update_process(process, update_color)
    int process, update_color;
{
    memset(global_buf, ' ', SCREEN_X);
    memcpy(global_buf, ps_vect[process], strlen(ps_vect[process]));

    if (update_color)
    {
	tty_brightness(ScreenBrightness);

	if (process == current_process)
	{
	    tty_foreground(ScreenBackground);
	    tty_background(ScreenForeground);
	}
	else
	{
	    tty_foreground(ScreenForeground);
	    tty_background(ScreenBackground);
	}
    }

    window_goto(screen_win, process - first_on_screen, 0);
    window_puts(global_buf, SCREEN_X);
}


void
update_all()
{
    int i;

    tty_colors(ScreenBrightness, ScreenForeground, ScreenBackground);

    window_goto(screen_win, 0, 0);

    for (i = first_on_screen;
	 i < processes && (i - first_on_screen < SCREEN_Y - 3); i++)
	    if (i != current_process)
		update_process(i, OFF);
	    else
		window_goto(screen_win, i - first_on_screen, 0);

    update_process(current_process, ON);

    tty_colors(ScreenBrightness, ScreenForeground, ScreenBackground);

    memset(global_buf, ' ', SCREEN_X);

    for (; i - first_on_screen < SCREEN_Y - 3; i++)
    {
	window_goto(screen_win, i - first_on_screen, 0);
	window_puts(global_buf, SCREEN_X);
    }

    window_goto(screen_win, current_process-first_on_screen, SCREEN_X-1);
}


void
clean_up()
{
    tty_exit(NULL);
    remove_log();
}


void
fatal(postmsg)
    char *postmsg;
{
    clean_up();
    fprintf(stderr, "%s: fatal error: %s.\n", program, postmsg);
    exit(1);
}


int
ps(args)
    char *args;
{
    char *ps_cmd;
    FILE *stdout_log, *stderr_log;

    /* See the comment in system.c on closing tty descriptors.  */

    int old_stdout = dup(1);
    int old_stderr = dup(2);

    close(1);
    close(2);

    stdout_log = fopen(stdout_log_name, "w");
    stderr_log = fopen(stderr_log_name, "w");

    ps_cmd = xmalloc(16 + (args ? strlen(args) : 0) + 1);

    if (args)
	sprintf(ps_cmd, "ps %s", args);
    else
	sprintf(ps_cmd, "ps");

    if (system(ps_cmd) != 0)
    {
	fclose(stdout_log);
	fclose(stderr_log);

	dup(old_stdout);
	dup(old_stderr);

	close(old_stdout);
	close(old_stderr);

	fprintf(stderr, "%s: invalid command line.\n", program);
	return 0;
    }

    xfree(ps_cmd);

    fclose(stdout_log);
    fclose(stderr_log);

    dup(old_stdout);
    dup(old_stderr);

    close(old_stdout);
    close(old_stderr);

    return 1;
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
		tty_key_list_insert((unsigned char *)key_seq, built_in[j]);
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
    int key, keys;
    tty_key_t *ks;
    FILE *stdout_log;
    int repeat_count;
    char *tmp, *data = NULL;
    int need_update, need_update_all, old_current_process;


    program = argv[0];
    pid     = getpid();

    home = getenv("HOME");
    if (home == NULL)
	home = ".";

    get_tty_name();
    get_login_name();

    tty_get_capabilities();
    tty_kbdinit(TTY_FULL_INPUT);

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

    common_configuration_init();
    use_section("[GITPS-Keys]");
    keys = read_keys(0);
    configuration_end();

    specific_configuration_init();

    tty_get_size(&SCREEN_X, &SCREEN_Y);
    tty_startup(UseLastScreenChar);


    use_section("[Setup]");

    configuration_getvarinfo("TempDirectory", &data, 1, DO_SEEK);
    TempDirectory = data ? tilde_expand(data) : "/tmp";

    AnsiColors         = get_flag_var("AnsiColors", OFF);
    UseLastScreenChar  = get_flag_var("UseLastScreenChar", OFF);
    StartupScrollStep  = get_int_var("StartupScrollStep", (SCREEN_Y - 3) / 2);

    if (StartupScrollStep <= 0 || StartupScrollStep >= (SCREEN_Y - 3) - 1)
	StartupScrollStep = (SCREEN_Y - 3) / 2;

    scroll_step = StartupScrollStep;


    use_section("[GITPS-Setup]");

    GitPsModeHelp = get_string_var("GitPsModeHelp", "");


    use_section(AnsiColors ? cSection : bwSection);

    get_colorset_var(PSColors, PSFields, PS_FIELDS);


    use_section("[GITPS-Keys]");

    keys = read_keys(keys);

    if (keys == MAX_KEYS)
	fprintf(stderr, "%s: too many key sequences; only %d are allowed.\n",
		program, MAX_KEYS);

    configuration_end();

#ifndef HAVE_LONG_FILE_NAMES
    fprintf(stderr, "%s: warning: your system doesn't support long file names.",
	    program);
#endif /* !HAVE_LONG_FILE_NAMES */

#ifdef HAVE_LINUX
    if (LinuxConsole)
	screen = xmalloc(4 + SCREEN_X * SCREEN_Y * 2);
#endif  /* HAVE_LINUX */

    stdout_log_name = xmalloc(32 + strlen(TempDirectory) + 1);
    stderr_log_name = xmalloc(32 + strlen(TempDirectory) + 1);
    sprintf(stdout_log_name, "%s/gitps.1.%d", TempDirectory, (int)pid);
    sprintf(stderr_log_name, "%s/gitps.2.%d", TempDirectory, (int)pid);

    global_buf  = xmalloc(SCREEN_X + 1);

    header_text = xmalloc(SCREEN_X + 1);

    title_win  = window_init(0, 0,            1,            SCREEN_X);
    header_win = window_init(0, 1,            1,            SCREEN_X);
    screen_win = window_init(0, 2,            SCREEN_Y - 3, SCREEN_X);
    status_win = window_init(0, SCREEN_Y - 1, 1,            SCREEN_X);

    tty_get_screen(screen);
    tty_set_mode(TTY_NONCANONIC);
    tty_defaults();

    first_on_screen = current_process = 0;

restart:

    if (ps(argc > 1 ? argv[1] : NULL) == 0)
    {
	remove_log();
	goto end;
    }

    stdout_log = fopen(stdout_log_name, "r");
    remove_log();

    if ((PID_index = get_PID_index(stdout_log)) == -1)
	goto end;

    free_ps_list();
    build_ps_list(stdout_log);
    fclose(stdout_log);

    settitle();
    setstatus((char *)NULL);
    setsignal();
    setheader();

    current_process = min(current_process, processes - 1);

    update_all();

    while (1)
    {
	ks  = tty_get_key(&repeat_count);
	key = ((char *)ks->aux_data - (char *)built_in) / MAX_BUILTIN_NAME;

	switch (key)
	{
	    case BUILTIN_previous_line:

		need_update_all = need_update = 0;

		while (repeat_count--)
		{
		    if (current_process != 0)
			current_process--;
		    else
			break;

		    if (current_process + 1 == first_on_screen)
		    {
			first_on_screen = max(0, first_on_screen -
					      scroll_step);
			need_update_all = 1;
		    }
		    else
		    {
			if (!need_update)
			    update_process(current_process + 1, ON);

			need_update = 1;
		    }
		}

		if (need_update_all)
		    update_all();
		else
		    if (need_update)
			update_process(current_process, ON);
		break;

	    case BUILTIN_next_line:

		need_update_all = need_update = 0;

		while (repeat_count--)
		{
		    if (current_process < processes - 1)
			current_process++;
		    else
			break;

		    if (current_process - first_on_screen >= SCREEN_Y - 3)
		    {
			first_on_screen = min(first_on_screen +
					      scroll_step,
					      processes - 1 -
					      (SCREEN_Y - 3) + 1);
			need_update_all = 1;
			continue;
		    }

		    if (!need_update)
			update_process(current_process - 1, ON);

		    need_update = 1;
		}

		if (need_update_all)
		    update_all();
		else
		    if (need_update)
			update_process(current_process, ON);
		break;

	    case BUILTIN_scroll_down:

		if (current_process == 0)
		    break;

		old_current_process = current_process;

		if (current_process < SCREEN_Y - 3)
		    current_process = first_on_screen = 0;
		else
		{
		    current_process -= SCREEN_Y - 3;
		    first_on_screen = max(0, first_on_screen - (SCREEN_Y - 3));
		}

		if (processes > SCREEN_Y - 3)
		    update_all();
		else
		{
		    update_process(old_current_process, ON);
		    update_process(current_process, ON);
		}

		break;

	    case BUILTIN_scroll_up:

		if (current_process == processes - 1)
		    break;

		old_current_process = current_process;

		if (processes - 1 - first_on_screen < SCREEN_Y - 3)
		    current_process = processes - 1;
		else
		    if (processes - 1 - current_process < SCREEN_Y - 3)
		    {
			current_process = processes - 1;
			first_on_screen = processes - 1 - (SCREEN_Y - 3) + 1;
		    }
		    else
		    {
			current_process += SCREEN_Y - 3;
			first_on_screen = min(first_on_screen + SCREEN_Y - 3,
					      (processes - 1) -
					      (SCREEN_Y - 3) + 1);
		    }

		if (processes > SCREEN_Y - 3)
		    update_all();
		else
		{
		    update_process(old_current_process, ON);
		    update_process(current_process, ON);
		}

		break;

	    case BUILTIN_beginning_of_list:

		if (current_process == 0)
		    break;

		current_process = first_on_screen = 0;
		update_all();
		break;

	    case BUILTIN_end_of_list:

		if (current_process == processes - 1)
		    break;

		current_process = processes - 1;
		first_on_screen = max(0, (processes - 1) - (SCREEN_Y - 3) + 1);
		update_all();
		break;

	    case BUILTIN_next_signal:

		signal_type++;
		signal_type %= sizeof(signals) / sizeof(struct SIGNAL);
		setsignal();
		break;

	    case BUILTIN_SIGHUP : signal_type =  0; setsignal(); break;
	    case BUILTIN_SIGINT : signal_type =  1; setsignal(); break;
	    case BUILTIN_SIGQUIT: signal_type =  2; setsignal(); break;
	    case BUILTIN_SIGILL : signal_type =  3; setsignal(); break;
	    case BUILTIN_SIGFPE : signal_type =  4; setsignal(); break;
	    case BUILTIN_SIGKILL: signal_type =  5; setsignal(); break;
	    case BUILTIN_SIGUSR1: signal_type =  6; setsignal(); break;
	    case BUILTIN_SIGSEGV: signal_type =  7; setsignal(); break;
	    case BUILTIN_SIGUSR2: signal_type =  8; setsignal(); break;
	    case BUILTIN_SIGPIPE: signal_type =  9; setsignal(); break;
	    case BUILTIN_SIGALRM: signal_type = 10; setsignal(); break;
	    case BUILTIN_SIGTERM: signal_type = 11; setsignal(); break;
	    case BUILTIN_SIGCHLD: signal_type = 12; setsignal(); break;
	    case BUILTIN_SIGCONT: signal_type = 13; setsignal(); break;

	    case BUILTIN_hard_refresh:

		tty_touch();

	    case BUILTIN_refresh:

		goto restart;

	    case BUILTIN_exit:

		goto end;

	    case BUILTIN_kill_process:

		if (!kill_process(current_process))
		{
		    tty_beep();
		    memset(global_buf, ' ', SCREEN_X);
		    tmp = xmalloc(16 + strlen((errno==EPERM)?no_perm:no_proc));
		    sprintf(tmp, "Error: %s", (errno==EPERM)?no_perm:no_proc);
		    setstatus(tmp);
		    xfree(tmp);
		    errno = 0;
		    tty_get_key(NULL);
		    setstatus((char *)NULL);
		    setsignal();
		}
		break;
	}
    }

  end:

    remove_log();
    tty_exit(screen);
    return 0;
}
