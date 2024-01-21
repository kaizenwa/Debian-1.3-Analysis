/* Slang interface to the Midnight Commander
   This emulates some features of ncurses on top of slang
   
   Copyright (C) 1995, 1996 The Free Software Foundation.

   Author Miguel de Icaza
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */
#include <config.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <termios.h>
#include <signal.h>
#include "tty.h"
#include "mad.h"
#include "color.h"
#include "util.h"
#include "mouse.h"		/* Gpm_Event is required in key.h */
#include "key.h"		/* define_sequence */
#include "main.h"		/* extern: force_colors */
#include "win.h"		/* do_exit_ca_mode */
#include "background.h"		/* we_are_background definition */

#ifdef HAVE_SLANG

/* Taken from S-Lang's sldisply.c file */
#ifndef USE_TERMCAP
#   define tgetstr(a,b) SLtt_tgetstr (a)
#else
    extern char *tgetstr(char *, char **);
#endif

#ifndef SA_RESTART
#    define SA_RESTART 0
#endif

/* Various saved termios settings that we control here */
static struct termios boot_mode;
static struct termios new_mode;

/* The file descriptor used by Slang */
static int Read_FD;

/* Set if we get an interrupt */
static int slinterrupt;

/* Controls whether we should wait for input in getch */
static int no_slang_delay;

static void
slang_intr (int signo)
{
    slinterrupt = 1;
}

int
interrupts_enabled (void)
{
    struct sigaction current_act;

    sigaction (SIGINT, NULL, &current_act);
    return current_act.sa_handler == slang_intr;
}

void
enable_interrupt_key(void)
{
    struct sigaction act;
    
    act.sa_handler = slang_intr;
    sigemptyset (&act.sa_mask);
    act.sa_flags = 0;
    sigaction (SIGINT, &act, NULL);
    slinterrupt = 0;
}

void
disable_interrupt_key(void)
{
    struct sigaction act;
    
    act.sa_handler = SIG_IGN;
    act.sa_flags = 0;
    sigemptyset (&act.sa_mask);
    sigaction (SIGINT, &act, NULL);
}

int
got_interrupt ()
{
    int t;

    t = slinterrupt;
    slinterrupt = 0;
    return t;
}

/* Only done the first time */
void
slang_init (void)
{
    struct sigaction act, oact;
    
    Read_FD = fileno (stdin);
    SLtt_get_terminfo ();
    tcgetattr (Read_FD, &boot_mode);
    /* 255 = ignore abort char; XCTRL('g') for abort char = ^g */
    SLang_init_tty (XCTRL('c'), 1, 0);	
    tcgetattr (Read_FD, &new_mode);
    slang_prog_mode ();
    load_terminfo_keys ();
    act.sa_handler = slang_intr;
    sigemptyset (&act.sa_mask);
    act.sa_flags = SA_RESTART;
    sigaction (SIGINT, &act, &oact);
}

/* Done each time we come back from done mode */
void
slang_prog_mode (void)
{
    tcsetattr (Read_FD, TCSANOW, &new_mode);
    SLsmg_init_smg ();
    SLsmg_touch_lines (0, LINES);
}

/* Called each time we want to shutdown slang screen manager */
void
slang_shell_mode (void)
{
    tcsetattr (Read_FD, TCSANOW, &boot_mode);
}

void
slang_shutdown ()
{
    char *op_cap;
    
    slang_shell_mode ();
    do_exit_ca_mode ();
    SLang_reset_tty ();

    /* Load the op capability to reset the colors to those that were
     * active when the program was started up
     */
    op_cap = SLtt_tgetstr ("op");
    if (op_cap){
	fprintf (stderr, "%s", op_cap);
	fflush (stderr);
    }
}

/* keypad routines */
void
slang_keypad (int set)
{
    char *keypad_string;
    
    keypad_string = (char *) SLtt_tgetstr (set ? "ks" : "ke");
    if (keypad_string)
	SLtt_write_string (keypad_string);
}

void
set_slang_delay (int v)
{
    no_slang_delay = v;
}

void
hline (int ch, int len)
{
    int last_x, last_y;

    last_x = SLsmg_get_column ();
    last_y = SLsmg_get_row ();
    
    if (ch == 0)
	ch = ACS_HLINE;

    if (ch == ACS_HLINE){
	SLsmg_draw_hline (len);
    } else {
	while (len--)
	    addch (ch);
    }
    move (last_y, last_x);
}

void
vline (int character, int len)
{
    if (!slow_terminal){
	SLsmg_draw_vline (len);
    } else {
	int last_x, last_y, pos = 0;

	last_x = SLsmg_get_column ();
	last_y = SLsmg_get_row ();

	while (len--){
	    move (last_y + pos++, last_x);
	    addch (' ');
	}
	move (last_x, last_y);
    }
}

void
init_pair (int index, char *foreground, char *background)
{
    SLtt_set_color (index, "", foreground, background);
}

char *color_terminals [] = {
#ifdef linux
    "console",
#endif
    "linux",
    "xterm-color",
    "color-xterm",
    "xtermc",
    "ansi",
    0
};

int has_colors ()
{
    char *terminal = getenv ("TERM");
    int  i;

    SLtt_Use_Ansi_Colors = force_colors;
    if (NULL != getenv ("COLORTERM"))
	SLtt_Use_Ansi_Colors = 1;

    /* We want to allow overwriding */
    if (!disable_colors){
	for (i = 0; color_terminals [i]; i++)
	    if (strcmp (color_terminals [i], terminal) == 0)
		SLtt_Use_Ansi_Colors = 1;
    }
    
    /* Setup emulated colors */
    if (SLtt_Use_Ansi_Colors){
	init_pair (A_REVERSE, "black", "white");
    } else {
	SLtt_set_mono (A_BOLD,    NULL, SLTT_BOLD_MASK);
	SLtt_set_mono (A_REVERSE, NULL, SLTT_REV_MASK);
	SLtt_set_mono (A_BOLD|A_REVERSE, NULL, SLTT_BOLD_MASK | SLTT_REV_MASK);
    }
    return SLtt_Use_Ansi_Colors;
}

void
attrset (int color)
{
    if (!SLtt_Use_Ansi_Colors){
	SLsmg_set_color (color);
	return;
    }
    
    if (color & A_BOLD){
	if (color == A_BOLD)
	    SLsmg_set_color (A_BOLD);
	else
	    SLsmg_set_color ((color & (~A_BOLD)) + 8);
	return;
    }

    if (color == A_REVERSE)
	SLsmg_set_color (A_REVERSE);
    else
	SLsmg_set_color (color);
}

/* This table describes which capabilities we want and which values we
 * assign to them.
 */
struct {
    int  key_code;
    char *key_name;
} key_table [] = {
    { KEY_F(0),  "k0" },
    { KEY_F(1),  "k1" },
    { KEY_F(2),  "k2" },
    { KEY_F(3),  "k3" },
    { KEY_F(4),  "k4" },
    { KEY_F(5),  "k5" },
    { KEY_F(6),  "k6" },
    { KEY_F(7),  "k7" },
    { KEY_F(8),  "k8" },
    { KEY_F(9),  "k9" },
    { KEY_F(10), "k;" },
    { KEY_F(11), "F1" },
    { KEY_F(12), "F2" },
    { KEY_F(13), "F3" },
    { KEY_F(14), "F4" },
    { KEY_F(15), "F5" },
    { KEY_F(16), "F6" },
    { KEY_F(17), "F7" },
    { KEY_F(18), "F8" },
    { KEY_F(19), "F9" },
    { KEY_F(20), "FA" },	
    { KEY_IC,    "kI" },
    { KEY_NPAGE, "kN" },
    { KEY_PPAGE, "kP" },
    { KEY_LEFT,  "kl" },
    { KEY_RIGHT, "kr" },
    { KEY_UP,    "ku" },
    { KEY_DOWN,  "kd" },
    { KEY_DC,    "kD" },
    { KEY_BACKSPACE, "kb" },
    { KEY_HOME,  "kh" },
    { KEY_END,	 "@7" },
    { 0, 0}
};
	
void
do_define_key (int code, char *strcap)
{
    char    *seq;

    seq = (char *) SLtt_tgetstr (strcap);
    if (seq)
	define_sequence (code, seq, MCKEY_NOACTION);
}

void
load_terminfo_keys ()
{
    int i;

    for (i = 0; key_table [i].key_code; i++)
	do_define_key (key_table [i].key_code, key_table [i].key_name);
}

int getch ()
{
    unsigned int ch;

    if (no_slang_delay)
	if (SLang_input_pending (0) == 0)
	    return -1;

    ch = SLang_getkey ();

#ifndef __GO32__
#define DEC_8BIT_HACK 64

   /* This has the effect of mapping all characters in the range 128-159 to
    * ESC [ something 
    */
   if ( (use_8th_bit_as_meta == 1) && (ch & 0x80) )
     {
	unsigned char i;
	i = (unsigned char) (ch & 0x7F);
	if (i < ' ')
	  {
	     i += DEC_8BIT_HACK;
	     SLang_ungetkey (i);
	     ch = 27;
	  }
     }

#endif    

   return (ch);
}

extern int slow_terminal;

#else

/* Non slang builds do not understand got_interrupt */
int got_interrupt ()
{
    return 0;
}
#endif /* HAVE_SLANG */

#ifdef HAVE_X
mc_refresh ()
{
}
#else
mc_refresh ()
{
    if (!we_are_background)
	refresh ();
}
#endif
