/*
 * gpm-Linux configuration file (server only)
 *
 * Copyright 1994-1996   rubini@ipvvis.unipv.it
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ********/

#define GPM_NAME    "gpm-Linux"
#define GPM_RELEASE "1.10"
#define GPM_DATE    "July 1996"

/* You can define DEBUG, or undefine it. */
/* Well, better putting it in the Makefile... */
/* #undef DEBUG  */

/* Is logging enabled? */
#undef CONFIG_GPM_LOG

/* timeout for the select() syscall */
#define SELECT_TIME 600

/* How many vitual consoles are managed? */
#define MAX_VC    32

/* How many buttons may the mouse have? */
/* #define MAX_BUTTONS 3  ===> not used, it is hardwired :-( */

/* all the default values */
#define DEF_TYPE          "ms"
#define DEF_DEV           NULL     /* use the type-related one */
#define DEF_LUT   "-a-zA-Z0-9_./\300-\326\330-\366\370-\377"
#define DEF_SEQUENCE     "123"     /* how buttons are reordered */
#define DEF_BAUD          1200
#define DEF_SAMPLE         100
#define DEF_DELTA           25
#define DEF_ACCEL            2
#define DEF_SCALE           10
#define DEF_TIME           250    /* time interval (ms) for multiple clicks */
#define DEF_CLUSTER         10    /* maximum number of clustered events */
#define DEF_THREE            0    /* have three buttons? */
#define DEF_KERNEL           0    /* no kernel module, by default */

#define DEF_TEST             0
#define DEF_PTRDRAG          1    /* double or triple click */
#define DEF_TOGGLE           0
#define DEF_GLIDEPOINT_TAP   0    /* tapping emulates no buttons by default */
