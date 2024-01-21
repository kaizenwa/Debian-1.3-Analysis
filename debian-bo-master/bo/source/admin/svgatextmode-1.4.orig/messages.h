/*  SVGATextMode -- An SVGA textmode manipulation/enhancement tool
 *
 *  Copyright (C) 1995,1996  Koen Gadeyne
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/***
 *** message printing tools
 ***/

#ifndef _MESSAGES_H
#define _MESSAGES_H

#include "vga_prg.h"   /* for SCREEN_ON */

#include "misc.h"

void print_msg(char *format,...);

extern char* CommandName;
extern bool debug_messages;

#define MSGTYP_ERR   3
#define MSGTYP_WARN  2
#define MSGTYP_MSG   1
#define MSGTYP_DBG   0

extern int msgtype;

/* the "do { ... } while (0)" construct allows us to use these macro's as if they were functions.
 * Otherwise you cannot ALWAYS place a ';' at the end of a message printing macro.
 */  

#ifndef NO_DEBUG
#  define PDEBUG(arg)  do { msgtype=MSGTYP_DBG ; print_msg arg; } while (0)
#else
#  define PDEBUG(arg)  do {} while (0)       /* avoids lots of warnings about empty statements */
#endif

/* PERROR() must switch screen back on, so you can read the error */
#define PERROR(arg) \
 do \
 { \
   msgtype=MSGTYP_ERR; \
   print_msg arg; \
   if (vga_open) SCREEN_ON; \
   exit (1); \
 } while (0)

#define PWARNING(arg) \
 do \
 { \
   msgtype=MSGTYP_WARN; \
   print_msg arg; \
 } while (0)

#define PMESSAGE(arg) \
 do \
 { \
   msgtype=MSGTYP_MSG; \
   print_msg arg; \
 } while (0)

#define PVERSION \
 do \
 { \
   PDEBUG(("%s version: %s\n", CommandName, VERSION)); \
 } while (0);
    

#endif

