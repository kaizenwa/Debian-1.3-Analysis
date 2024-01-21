/* Keyboard support routines.

   Copyright (C) 1994,1995 the Free Software Foundation.

   Written by: 1994, 1995 Miguel de Icaza.
               1994, 1995 Janne Kukonlehto.
	       1995  Jakub Jelinek.
   
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
#include <sys/types.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif
#include <sys/types.h>		/* FD_ZERO et al */
#include <sys/time.h>		/* struct timeval */
#if HAVE_SYS_SELECT_H
#   include <sys/select.h>
#endif
#include "tty.h"
#include <ctype.h>
#include <errno.h>
#include <malloc.h> 
#include "util.h"		/* For xmalloc prototype */
#include "mad.h"		/* The memory debugger */
#include "global.h"
#include "mouse.h"
#include "key.h"
#include "main.h"
#include "file.h"
#include "../vfs/vfs.h"
#ifdef __linux__
#  include <linux/termios.h> /* This is needed for TIOCLINUX */
#endif
#ifdef HAVE_TK
#   include <tcl.h>
#endif

/* "$Id: key.c,v 1.18 1995/02/21 19:06:13 miguel Exp $" */

/* This macros were stolen from gpm 0.15 */
#define GET_TIME(tv) (gettimeofday(&tv, (struct timezone *)NULL))
#define DIF_TIME(t1,t2) ((t2.tv_sec -t1.tv_sec) *1000+ \
			 (t2.tv_usec-t1.tv_usec)/1000)
			 
/* timeout for old_esc_mode in usec */
#define ESCMODE_TIMEOUT 1000000

int mou_auto_repeat = 100;
int double_click_speed = 250;
int old_esc_mode = 0;

int use_8th_bit_as_meta = 1;

#ifdef HAVE_XVIEW
int xmouse_flag = 0;
#endif

#ifndef HAVE_XVIEW
typedef struct key_def {
    char ch;			/* Holds the matching char code */
    int code;			/* The code returned, valid if child == NULL */
    struct key_def *next;
    struct key_def *child;	/* sequence continuation */
    int action;			/* optional action to be done. Now used only
                                   to mark that we are just after the first
                                   Escape */
} key_def;

/* This holds all the key definitions */
static key_def *keys = 0;
#endif

static int input_fd;
static fd_set select_set;
static int disabled_channels = 0; /* Disable channels checking */

#ifndef HAVE_TK
/* File descriptor monitoring add/remove routines */
typedef struct SelectList {
    int fd;
    select_fn callback;
    void *info;
    struct SelectList *next;
} SelectList;

SelectList *select_list = 0;
#endif

int xgetch_second (void);

#ifdef HAVE_TK
void add_select_channel (int fd, select_fn callback, void *info)
{
    Tcl_File handle;

    handle = Tcl_GetFile ((ClientData) fd, TCL_UNIX_FD);
    Tcl_CreateFileHandler (handle, TCL_READABLE, (Tcl_FileProc *) callback, 0);
}

void delete_select_channel (int fd)
{
    Tcl_File handle;

    handle = Tcl_GetFile ((ClientData) fd, TCL_UNIX_FD);
    Tcl_DeleteFileHandler (handle);
    Tcl_FreeFile (handle);
}
#else
void add_select_channel (int fd, select_fn callback, void *info)
{
    SelectList *new;

    new = xmalloc (sizeof (SelectList), "add_select_channel");
    new->fd = fd;
    new->callback = callback;
    new->info = info;
    new->next = select_list;
    select_list = new;
}

void delete_select_channel (int fd)
{
    SelectList *p = select_list;
    SelectList *prev = 0;
    
    while (p){
	if (p->fd == fd){
	    if (prev)
		prev->next = p->next;
	    else
		select_list = p->next;
	    free (p);
	}
	prev = p;
	p = p->next;
    }
}

inline static int add_selects (fd_set *select_set)
{
    SelectList *p;
    int        top_fd = 0;

    if (disabled_channels)
	return 0;
    
    for (p = select_list; p; p = p->next){
	FD_SET (p->fd, select_set);
	if (p->fd > top_fd)
	    top_fd = p->fd;
    }
    return top_fd;
}

static void check_selects (fd_set *select_set)
{
    SelectList *p;

    if (disabled_channels)
	return;
    
    for (p = select_list; p; p = p->next)
	if (FD_ISSET (p->fd, select_set))
	    (*p->callback)(p->fd, p->info);
}
#endif

void channels_down (void)
{
    disabled_channels ++;
}

void channels_up (void)
{
    if (!disabled_channels)
	fprintf (stderr,
		 "Error: channels_up called with disabled_channels = 0\n");
    disabled_channels--;
}

typedef struct {
    int code;
    char *seq;
    int action;
} key_define_t;

#ifndef HAVE_XVIEW
key_define_t mc_bindings [] = {
    { KEY_END,    ESC_STR ">", MCKEY_NOACTION },
    { KEY_HOME,   ESC_STR "<", MCKEY_NOACTION },

#ifdef linux
    /* Incredible, but many Linuxes still have old databases */
    { KEY_IC,     ESC_STR "[2~", MCKEY_NOACTION },
#endif
    { 0, 0, MCKEY_NOACTION },
};

/* Broken terminfo and termcap databases on xterminals */
key_define_t xterm_key_defines [] = {
    { KEY_F(1),   ESC_STR "[11~", MCKEY_NOACTION },
    { KEY_F(2),   ESC_STR "[12~", MCKEY_NOACTION },
    { KEY_F(3),   ESC_STR "[13~", MCKEY_NOACTION },
    { KEY_F(4),   ESC_STR "[14~", MCKEY_NOACTION },
    { KEY_F(5),   ESC_STR "[15~", MCKEY_NOACTION },
    { KEY_F(6),   ESC_STR "[17~", MCKEY_NOACTION },
    { KEY_F(7),   ESC_STR "[18~", MCKEY_NOACTION },
    { KEY_F(8),   ESC_STR "[19~", MCKEY_NOACTION },
    { KEY_F(9),   ESC_STR "[20~", MCKEY_NOACTION },
    { KEY_F(10),  ESC_STR "[21~", MCKEY_NOACTION },
    { 0, 0, MCKEY_NOACTION },
};

key_define_t mc_default_keys [] = {
    { ESC_CHAR,	ESC_STR, MCKEY_ESCAPE },
    { ESC_CHAR, ESC_STR ESC_STR, MCKEY_NOACTION },
    { 0, 0, MCKEY_NOACTION },
};
#endif

void define_sequences (key_define_t *kd)
{
#ifndef HAVE_XVIEW
    int i;
    
    for (i = 0; kd [i].code; i++)
	define_sequence(kd [i].code, kd [i].seq, kd [i].action);
#endif	
}

void init_key (void)
{
#ifndef HAVE_XVIEW
    char *term = (char *) getenv ("TERM");
    
    /* This has to be the first define_sequence */
    /* So, we can assume that the first keys member has ESC */
    define_sequences (mc_default_keys);
    
    /* Terminfo on irix does not have some keys */
    if ((!strncmp (term, "iris-ansi", 9)) || (!strncmp (term, "xterm", 5)))
	define_sequences (xterm_key_defines);
    
    define_sequences (mc_bindings);
    
#ifdef HAVE_SLANG
    input_fd = fileno (stdin);
#endif
#endif /* !HAVE_XVIEW */
}

#ifndef HAVE_XVIEW
void xmouse_get_event (Gpm_Event *ev)
{
    int btn;
    static struct timeval tv1 = { 0, 0 }; /* Force first click as single */
    static struct timeval tv2;
    static int clicks;

    /* Decode Xterm mouse information to a GPM style event */

    /* Variable btn has following meaning: */
    /* 0 = btn1 dn, 1 = btn2 dn, 2 = btn3 dn, 3 = btn up */
    btn = xgetch () - 32;
    
    /* There seems to be no way of knowing which button was released */
    /* So we assume all the buttons were released */

    if (btn == 3){
        ev->type = GPM_UP | (GPM_SINGLE << clicks);
        ev->buttons = 0;
	GET_TIME (tv1);
	clicks = 0;
    } else {
        ev->type = GPM_DOWN;
	GET_TIME (tv2);
	if (tv1.tv_sec && (DIF_TIME (tv1,tv2) < double_click_speed)){
	    clicks++;
	    clicks %= 3;
	} else
	    clicks = 0;
	
        switch (btn) {
	case 0:
            ev->buttons |= GPM_B_LEFT;
            break;
	case 1:
            ev->buttons |= GPM_B_MIDDLE;
            break;
	case 2:
            ev->buttons |= GPM_B_RIGHT;
            break;
	default:
            /* Nothing */
            break;
        }
    }
    /* Coordinates are 33-based */
    /* Transform them to 1-based */
    ev->x = xgetch () - 32;
    ev->y = xgetch () - 32;
}

static key_def *create_sequence (char *seq, int code, int action)
{
    key_def *base, *p, *attach;

    for (base = attach = NULL; *seq; seq++){
	p = xmalloc (sizeof (key_def), "create_sequence");
	if (!base) base = p;
	if (attach) attach->child = p;
	
	p->ch   = *seq;
	p->code = code;
	p->child = p->next = NULL;
	if (!seq[1])
	    p->action = action;
	else
	    p->action = MCKEY_NOACTION;
	attach = p;
    }
    return base;
}

/* The maximum sequence length (32 + null terminator) */
static int seq_buffer [33];
static int *seq_append = 0;

static int push_char (int c)
{
    if (!seq_append)
	seq_append = seq_buffer;
    
    if (seq_append == &(seq_buffer [sizeof (seq_buffer)-2]))
	return 0;
    *(seq_append++) = c;
    *seq_append = 0;
    return 1;
}
#endif /* !HAVE_XVIEW */

void define_sequence (int code, char *seq, int action)
{
#ifndef HAVE_XVIEW
    key_def *base;

    if (strlen (seq) > sizeof (seq_buffer)-1)
	return;
    
    for (base = keys; (base != 0) && *seq; ){
	if (*seq == base->ch){
	    if (base->child == 0){
		if (*(seq+1)){
		    base->child = create_sequence (seq+1, code, action);
		    return;
		} else {
		    /* The sequence clashes */
		    return;
		}
	    } else {
		base = base->child;
		seq++;
	    }
	} else {
	    if (base->next)
		base = base->next;
	    else {
		base->next = create_sequence (seq, code, action);
		return;
	    }
	}
    }
    keys = create_sequence (seq, code, action);
#endif    
}

#ifndef HAVE_XVIEW
static int *pending_keys;
#endif

int correct_key_code (int c)
{
    /* This is needed on some OS that do not support ncurses and */
    /* do some magic after read()ing the data */
    if (c == '\r')
	return '\n';

#ifdef IS_AIX
    if (c == KEY_SCANCEL)
	return '\t';
#endif

    if (c == KEY_F(0))
	return KEY_F(10);
	
    switch (c) {
        case KEY_KP_ADD: c = alternate_plus_minus ? ALT('+') : '+'; break;
        case KEY_KP_SUBTRACT: c = alternate_plus_minus ? ALT('-') : '-'; break;
        case KEY_KP_MULTIPLY: c = alternate_plus_minus ? ALT('*') : '*'; break;
    }

    return c;
}

int get_key_code (int no_delay)
{
#ifndef HAVE_XVIEW
    int c;
    static key_def *this = NULL, *parent;
    static struct timeval esctime = { -1, -1 };
    static int lastnodelay = -1;
    
    if (no_delay != lastnodelay) {
        this = NULL;
        lastnodelay = no_delay;
    }

 pend_send:
    if (pending_keys){
	int d = *pending_keys++;
 check_pend:
	if (!*pending_keys){
	    pending_keys = 0;
	    seq_append = 0;
	}
	if (d == ESC_CHAR && pending_keys){
	    d = ALT(*pending_keys++);
	    goto check_pend;
	}
	if ((d & 0x80) && use_8th_bit_as_meta)
	    d = ALT(d & 0x7f);
	this = NULL;
	return correct_key_code (d);
    }

 nodelay_try_again:
    if (no_delay) {
#ifdef BUGGY_CURSES
        wtimeout(stdscr, 500);
#else
        nodelay (stdscr, TRUE);
#endif
    }
    c = xgetch ();
    if (no_delay) {
#ifdef BUGGY_CURSES
        notimeout (stdscr, TRUE);
#else
        nodelay (stdscr, FALSE);
#endif
        if (c == ERR) {
            if (this != NULL && parent != NULL && 
                parent->action == MCKEY_ESCAPE && old_esc_mode) {
                struct timeval current, timeout;
                
                if (esctime.tv_sec == -1)
                    return ERR;
                GET_TIME (current);
                timeout.tv_sec = ESCMODE_TIMEOUT / 1000000 + esctime.tv_sec;
                timeout.tv_usec = ESCMODE_TIMEOUT % 1000000 + esctime.tv_usec;
                if (timeout.tv_usec > 1000000) {
                    timeout.tv_usec -= 1000000;
                    timeout.tv_sec++;
                }
                if (current.tv_sec < timeout.tv_sec)
                    return ERR;
                if (current.tv_sec == timeout.tv_sec && 
                    current.tv_usec < timeout.tv_usec)
                    return ERR;
                this = NULL;
		pending_keys = seq_append = NULL;
		return ESC_CHAR;
            }
            return ERR;
        }
    } else if (c == ERR){
	/* Maybe we got an incomplete match.
	   This we do only in delay mode, since otherwise
	   xgetch can return ERR at any time. */
	if (seq_append) {
	    pending_keys = seq_buffer;
	    goto pend_send;
	}
	this = NULL;
	return ERR;
    }
    
    /* Search the key on the root */
    if (!no_delay || this == NULL) {
        this = keys;
        parent = NULL;

        if ((c & 0x80) && use_8th_bit_as_meta) {
            c &= ~0x7f;

	    /* The first sequence defined starts with esc */
	    parent = keys;
	    this = keys->child;
        }
    }
    while (this){
	if (c == this->ch){
	    if (this->child){
		if (!push_char (c)){
		    pending_keys = seq_buffer;
		    goto pend_send;
		}
		parent = this;
		this = this->child;
		if (parent->action == MCKEY_ESCAPE && old_esc_mode) {
		    if (no_delay) {
		        GET_TIME (esctime);
		        if (this == NULL) {
		            /* Shouldn't happen */
		            fprintf (stderr, "Internal error\n");
		            exit (1);
		        }
		        goto nodelay_try_again;
		    }
		    esctime.tv_sec = -1;
		    c = xgetch_second ();
		    if (c == ERR) {
		        pending_keys = seq_append = NULL;
		        this = NULL;
		        return ESC_CHAR;
		    }
		} else {
		    if (no_delay)
		        goto nodelay_try_again;
		    c = xgetch ();
		}
	    } else {
		/* We got a complete match, return and reset search */
		int code;
		
		pending_keys = seq_append = NULL;
		code = this->code;
		this = NULL;
		return correct_key_code (code);
	    }
	} else {
	    if (this->next)
		this = this->next;
	    else {
	        if (parent != NULL && parent->action == MCKEY_ESCAPE) {
	            /* This is just to save a lot of define_sequences */
	            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
			|| (c == '\n') || (c == '\t') || (c == XCTRL('h'))
			|| (c == KEY_BACKSPACE) || (c == '!') || (c == '\r')
			|| c == 127 || c == '+' || c == '-' || c == '\\' 
			|| c == '?')
			c = ALT(c);
		    else if (isdigit(c))
	                c = KEY_F (c-'0');
		    else if (c == ' ')
			c = ESC_CHAR;
		    pending_keys = seq_append = NULL;
		    this = NULL;
		    return correct_key_code (c);
		}
		/* Did not find a match or {c} was changed in the if above,
		   so we have to return everything we had skipped
		 */
		push_char (c);
		pending_keys = seq_buffer;
		goto pend_send;
	    }
	}
    }
    this = NULL;
    return correct_key_code (c);
#else
    return ERR;
#endif /* HAVE_XVIEW */
}

#ifndef HAVE_TK
/* If set timeout is set, then we wait 0.1 seconds, else, we block */
void try_channels (int set_timeout)
{
    struct timeval timeout;
    static fd_set select_set;
    struct timeval *timeptr;
    int v;

    while (1){
	FD_ZERO (&select_set);
	FD_SET  (input_fd, &select_set);	/* Add stdin */
	add_selects (&select_set);
	
	if (set_timeout){
	    timeout.tv_sec = 0;
	    timeout.tv_usec = 100000;
	    timeptr = &timeout;
	} else
	    timeptr = 0;

	v = select (FD_SETSIZE, &select_set, NULL, NULL, timeptr);
	if (v > 0){
	    check_selects (&select_set);
	    if (FD_ISSET (input_fd, &select_set))
		return;
	}
    }
}

#ifndef HAVE_XVIEW
/* Workaround for System V Curses vt100 bug */
static int getch_with_delay (void)
{
    int c;

    /* This routine could be used on systems without mouse support,
       so we need to do the select check :-( */
    while (1){
	if (!pending_keys)
	    try_channels (0);

	/* Try to get a character */
	c = get_key_code (0);
	if (c != ERR)
	    break;
	/* Failed -> wait 0.1 secs and try again */
	try_channels (1);
    }
    /* Success -> return the character */
    return c;
}
#endif

#ifndef HAVE_LIBGPM
#define gpm_flag 0
#endif
#endif /* !HAVE_XVIEW */

extern int max_dirt_limit;

/* Returns a character read from stdin with appropriate interpretation */
/* Also takes care of generated mouse events */
/* Returns 0 if it is a mouse event */
/* The current behavior is to block allways */
int get_event (Gpm_Event *event, int redo_event)
{
#ifndef HAVE_X
#define block 1
    int c;
    static int flag;			/* Return value from select */
#ifdef HAVE_LIBGPM
    static Gpm_Event ev;		/* Mouse event */
#endif
    struct timeval timeout;
    struct timeval *time_addr = NULL;
    static int dirty = 3;

    if ((dirty == 3) || is_idle ()){
	mc_refresh ();
	doupdate ();
	dirty = 1;
    } else
	dirty++;

    vfs_timeout_handler ();
    
    /* Ok, we use (event->x < 0) to signal that the event does not contain
       a suitable position for the mouse, so we can't use show_mouse_pointer
       on it.
    */
    if (event->x > 0){
	show_mouse_pointer (event->x, event->y);
	if (!redo_event)
	    event->x = -1;
    }

    /* Repeat if using mouse */
    while ((xmouse_flag || gpm_flag) && !pending_keys)
    {
	if (xmouse_flag || gpm_flag)
	{
	    FD_ZERO (&select_set);
	    FD_SET  (input_fd, &select_set);
	    add_selects (&select_set);

#ifdef HAVE_LIBGPM
	    if (gpm_flag) {
		FD_SET  (gpm_fd, &select_set);
	    }
#endif

	    if (redo_event){
	        timeout.tv_usec = mou_auto_repeat * 1000;
		timeout.tv_sec = 0;

		time_addr = &timeout;
	    } else {
		int seconds;
		
		if ((seconds = vfs_timeouts ())){
		    /* the timeout could be improved and actually be
		     * the number of seconds until the next vfs entry
		     * timeouts in the stamp list.
		     */

		    timeout.tv_sec = seconds;
		    timeout.tv_sec = 0;
		    time_addr = &timeout;
		} else
		    time_addr = NULL;
	    }

	    if (!block){
		time_addr = &timeout;
		timeout.tv_sec = 0;
		timeout.tv_usec = 0;
	    }
	    enable_interrupt_key ();
	    flag = select (FD_SETSIZE, &select_set, NULL, NULL, time_addr);
	    disable_interrupt_key ();
	    
	    /* select timed out: it could be for any of the following reasons:
	     * redo_event -> it was because of the MOU_REPEAT handler
	     * !block     -> we did not block in the select call
	     *               (this is currently unused).
	     * else       -> 10 second timeout to check the vfs status.
	     */
	    if (flag == 0){
		if (redo_event)
		    return 0;
		if (!block)
		    return -1;
		vfs_timeout_handler ();
	    }
	    if (flag == -1 && errno == EINTR)
		return -1;

	    check_selects (&select_set);
	    
	    if (FD_ISSET (input_fd, &select_set))
	        break;
	}
#ifdef HAVE_LIBGPM
	if (gpm_flag && FD_ISSET (gpm_fd, &select_set)){
	    if (gpm_flag){
		Gpm_GetEvent (&ev);
		Gpm_FitEvent (&ev);
	    }
	    *event = ev;
	    return 0;
	}
#endif
    }
#   ifndef HAVE_SLANG
    flag = is_wintouched(stdscr);
    untouchwin (stdscr);
#   endif

    c = getch_with_delay ();
    
#   ifndef HAVE_SLANG
    if (flag)
        touchwin (stdscr);
#   endif
    
    if (!c) { /* Mouse event */
        xmouse_get_event (event);
        return 0;
    }

    return c;
#else
    return -1;
#endif /* HAVE_XVIEW */
}

/* Returns a key press, mouse events are discarded */
int mi_getch ()
{
#ifdef HAVE_X
#ifdef HAVE_TK
    return tk_getch ();
#else
    /* FIXME: XView requires this routine to work with quote */
#endif /* HAVE_TK */
#else
    Gpm_Event ev;
    int       key;
    
    while ((key = get_event (&ev, 0)) == 0)
	;
    return key;
#endif /* HAVE_X */
}

int xgetch_second (void)
{
    fd_set Read_FD_Set;
    int c;
    struct timeval timeout;

    timeout.tv_sec = ESCMODE_TIMEOUT / 1000000;
    timeout.tv_usec = ESCMODE_TIMEOUT % 1000000;
#ifdef BUGGY_CURSES
    wtimeout(stdscr, 500);
#else
    nodelay (stdscr, TRUE);
#endif
    FD_ZERO (&Read_FD_Set);
    FD_SET (input_fd, &Read_FD_Set);
    select (FD_SETSIZE, &Read_FD_Set, NULL, NULL, &timeout);
    c = xgetch ();
#ifdef BUGGY_CURSES
    notimeout (stdscr, TRUE);
#else
    nodelay (stdscr, FALSE);
#endif
    return c;
}

#ifndef HAVE_X
void learn_store_key (char *buffer, char **p, int c)
{
    if (*p - buffer > 253)
        return;
    if (c == ESC_CHAR) {
        *(*p)++ = '\\';
        *(*p)++ = 'e';
    } else if (c < ' ') {
    	*(*p)++ = '^';
    	*(*p)++ = c + 'a' - 1;
    } else if (c == '^') {
    	*(*p)++ = '^';
    	*(*p)++ = '^';
    } else
    	*(*p)++ = (char) c;
}

char *learn_key (void)
{
/* LEARN_TIMEOUT in usec */
#define LEARN_TIMEOUT 200000

    fd_set Read_FD_Set;
    struct timeval endtime;
    struct timeval timeout;
    int c = xgetch ();
    char buffer [256];
    char *p = buffer;
    
    while (c == ERR)
        c = xgetch (); /* Sanity check, should be unnecessary */
    learn_store_key (buffer, &p, c);
    GET_TIME (endtime);
    endtime.tv_usec += LEARN_TIMEOUT;
    if (endtime.tv_usec > 1000000) {
        endtime.tv_usec -= 1000000;
        endtime.tv_sec++;
    }
#ifdef BUGGY_CURSES
    wtimeout(stdscr, 500);
#else
    nodelay (stdscr, TRUE);
#endif
    for (;;) {
        while ((c = xgetch ()) == ERR) {
            GET_TIME (timeout);
            timeout.tv_usec = endtime.tv_usec - timeout.tv_usec;
            if (timeout.tv_usec < 0)
                timeout.tv_sec++;
            timeout.tv_sec = endtime.tv_sec - timeout.tv_sec;
            if (timeout.tv_sec >= 0 && timeout.tv_usec > 0) {
    		FD_ZERO (&Read_FD_Set);
    		FD_SET (input_fd, &Read_FD_Set);
                select (FD_SETSIZE, &Read_FD_Set, NULL, NULL, &timeout);
            } else
            	break;
        }
        if (c == ERR)
            break;
	learn_store_key (buffer, &p, c);        
    }
#ifdef BUGGY_CURSES
    notimeout (stdscr, TRUE);
#else
    nodelay (stdscr, FALSE);
#endif
    *p = 0;
    return strdup (buffer);
}
#endif /* !HAVE_X */

/* A function to check if we're idle.
   Currently checks only for key presses.
   We could also check the mouse. */
int is_idle (void)
{
    /* Check for incoming key presses     *
     * If there are any we say we're busy */

    fd_set select_set;
    struct timeval timeout;
    FD_ZERO (&select_set);
    FD_SET (0, &select_set);
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    select (FD_SETSIZE, &select_set, 0, 0, &timeout);
    return ! FD_ISSET (0, &select_set);
}


int get_modifier ()
{
#ifdef __linux__
    unsigned char modifiers;

    modifiers = 6;

    if (ioctl (0, TIOCLINUX, &modifiers) < 0)
	return 0;

    return (int) modifiers;
#else
    return 0;
#endif
}

int ctrl_pressed ()
{
#ifdef __linux__
    if (get_modifier () & CONTROL_PRESSED)
	return 1;
#endif
    return 0;
}

#ifdef HAVE_MAD
#ifndef HAVE_XVIEW
void k_dispose (key_def *k)
{
    if (!k)
	return;
    k_dispose (k->child);
    k_dispose (k->next);
    free (k);
}

void s_dispose (SelectList *sel)
{
    if (!sel)
	return;

    s_dispose (sel->next);
    free (sel);
}

void done_key ()
{
    k_dispose (keys);
    s_dispose (select_list);
}

#else

void done_key () 
{
}

#endif /* HAVE_XVIEW */
#endif /* HAVE_MAD */
