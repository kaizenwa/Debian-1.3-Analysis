/* status.c -- A simple status line management file.  */

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


#ifdef HAVE_UTSNAME
#include <sys/utsname.h>
#endif

#include "xtime.h"

#include "xstring.h"
#include "xmalloc.h"
#include "window.h"
#include "status.h"
#include "configure.h"
#include "tty.h"
#include "misc.h"


extern int AnsiColors;


static window_t *status_win = NULL;
static char *stemp;
static int columns;
static int line;
static char *default_msg;

#ifdef HAVE_UTSNAME
static struct utsname u;
#endif


#define STATUSBAR_FIELDS        9

static char *StatusBarFields[STATUSBAR_FIELDS] =
{
    "StatusBarForeground",
    "StatusBarBackground",
    "StatusBarBrightness",
    "StatusBarWarningForeground",
    "StatusBarWarningBackground",
    "StatusBarWarningBrightness",
    "StatusBarErrorForeground",
    "StatusBarErrorBackground",
    "StatusBarErrorBrightness"
};

#ifdef HAVE_LINUX
static int StatusBarColors[STATUSBAR_FIELDS] =
{
    BLACK, CYAN, OFF, BLACK, WHITE, OFF, WHITE, RED, ON
};
#else   /* !HAVE_LINUX */
static int StatusBarColors[STATUSBAR_FIELDS] =
{
    BLACK, WHITE, OFF, BLACK, WHITE, OFF, BLACK, WHITE, ON
};
#endif  /* !HAVE_LINUX */

#define StatusBarForeground             StatusBarColors[0]
#define StatusBarBackground             StatusBarColors[1]
#define StatusBarBrightness             StatusBarColors[2]
#define StatusBarWarningForeground      StatusBarColors[3]
#define StatusBarWarningBackground      StatusBarColors[4]
#define StatusBarWarningBrightness      StatusBarColors[5]
#define StatusBarErrorForeground        StatusBarColors[6]
#define StatusBarErrorBackground        StatusBarColors[7]
#define StatusBarErrorBrightness        StatusBarColors[8]


void
status_init(_columns, _begin_y, def_msg)
    int _columns, _begin_y;
    char *def_msg;
{
    use_section(AnsiColors ? cSection : bwSection);

    get_colorset_var(StatusBarColors, StatusBarFields, STATUSBAR_FIELDS);


    columns     = _columns;
    line        = _begin_y;
    default_msg = def_msg;
    status_win  = window_init(0, _begin_y, 1, _columns);
    stemp       = xmalloc(columns);

#ifdef HAVE_UTSNAME
    uname(&u);
#endif
}


void
status_end()
{
    window_end(status_win);
}


static void
build_msg(msg_name, center)
   char *msg_name;
   int center;
{
    int i, j;
    struct tm *time;
    char date_str[32];
    char *ptr, *temp_msg;
    size_t len, temp_msg_len;


    memset(stemp, ' ', columns);

    if (msg_name == NULL)
	msg_name = default_msg;

    temp_msg = xmalloc(temp_msg_len = (strlen(msg_name) + 1));

    for (i = 0, j = 0; msg_name[i]; i++)
	if (msg_name[i] == '\\')
	    switch (msg_name[i + 1])
	    {
		case 'h' :

#ifdef HAVE_UTSNAME
			ptr = u.nodename;
#else
			ptr = "";
#endif
			goto get_system_info;

		case 's' :

#ifdef HAVE_UTSNAME
			ptr = u.sysname;
#else
			ptr = "";
#endif
			goto get_system_info;

		case 'm' :

#ifdef HAVE_UTSNAME
			ptr = u.machine;
#else
			ptr = "";
#endif

			    get_system_info:

				if (ptr[0])
				{
				    len = strlen(ptr);
				    temp_msg = xrealloc(temp_msg,
							temp_msg_len += len);
				    memcpy(&temp_msg[j], ptr, len);
				}
				else
				{
				    len = 6;
				    temp_msg = xrealloc(temp_msg,
							temp_msg_len += len);
				    memcpy(&temp_msg[j], "(none)", len);
				}

				j += len;
				i++;
				break;

		case 'd' :      time = get_local_time();
				sprintf(date_str, "%s %s %02d %d",
					day_name[time->tm_wday],
					month_name[time->tm_mon],
					time->tm_mday,
					time->tm_year + 1900);
				len = strlen(date_str);
				temp_msg = xrealloc(temp_msg,
						    temp_msg_len += len);
				memcpy(&temp_msg[j], date_str, len);
				j += len;
				i++;
				break;

		case '\\':      temp_msg[j++] = '\\';
				i++;
				break;

		case '\0':      temp_msg[j++] = '\\';
				break;

		default  :      temp_msg[j++] = '\\';
				temp_msg[j++] = msg_name[++i];
				break;
	    }
	else
	{
	    if (msg_name[i] == '\t')
	    {
		temp_msg = xrealloc(temp_msg, temp_msg_len += 8);
		memcpy(&temp_msg[j], "        ", 8);
		j += 8;
	    }
	    else
		temp_msg[j++] = msg_name[i];
	}

    temp_msg[j] = 0;

    len = strlen(msg_name = temp_msg);

    if (center && len < columns)
	memcpy(stemp + ((columns - len) >> 1), msg_name, len);
    else
	memcpy(stemp, msg_name, min(len, columns));

    xfree(temp_msg);

    for (i = 0; i < columns; i++)
	if (stemp[i] == '\r' || stemp[i] == '\n')
	    stemp[i] = ' ';
}


int
status(msg_name, wait, sound, restore, msg_type, center)
    char *msg_name;
    int wait, sound, restore, msg_type, center;
{
    int key = 0;
    tty_key_t *ks;
    tty_status_t status;


    tty_save(&status);

    build_msg(msg_name, center);

    if (sound)
	tty_beep();

    switch (msg_type)
    {
	case MSG_STATUS:

	    tty_colors(StatusBarWarningBrightness,
		       StatusBarWarningForeground,
		       StatusBarWarningBackground);
	    break;

	case MSG_ERROR:

	    tty_colors(StatusBarErrorBrightness,
		       StatusBarErrorForeground,
		       StatusBarErrorBackground);
	    break;

	default:

	    tty_colors(StatusBarBrightness,
		       StatusBarForeground,
		       StatusBarBackground);
	    break;
    }

    window_goto(status_win, 0, 0);
    window_puts(stemp, columns);
    window_update();

    if (wait)
    {
	ks = tty_get_key(NULL);
	window_goto(status_win, line, columns);
	key = ks->key_seq[0];
    }

    if (restore)
    {
	build_msg(default_msg, MSG_CENTERED);

	tty_colors(StatusBarBrightness,
		   StatusBarForeground,
		   StatusBarBackground);

	window_goto(status_win, 0, 0);
	window_puts(stemp, columns);
	window_update();
    }

    tty_restore(&status);
    return key;
}
