/*
 * This file sets Pdmenu's limits.
 */

/* 
 * Copyright (c) 1995, 1996, 1997 Joey Hess (joey@kite.ml.org)
 * All rights reserved. See COPYING for full copyright information (GPL).
 */

/*
 * Misc. limits.
 */

/* Minimum size of the screen */
#define MIN_ROWS 6
#define MIN_COLS 6

/*
 * Menu limits.
 */

/* How many items can be in a menu? 
 * You should be generous here; this is also the max lines of a command 
 * that can be displayed in a menu.
 */
#define MAX_ITEMS_IN_MENU 1000

/* How many menus and other windows can be showing at once? */
#define MAX_WINDOWS 500

/* How long can a item in one of the menus be?
 * When setting this, consider: how wide will your screen ever be? 
 * Then subtract 4 from that. 
 */
#define MENU_ITEM_TEXTSIZE 76

/* What's the longest command that can be bound to a line in a menu? */
#define MENU_ITEM_COMMANDSIZE 255

/* The longest name for a menu can be? */
#define NAMELEN 80

/* 
 * Rc file limits
 */

/* Maximum number of menu definitions that can be loaded at a time */
#define MAX_NUM_MENUS MAX_WINDOWS

/* Largest line length allowed in a rc file */
#define MAX_LINE_LEN 512

/* How many fields can possibly be in a line of a rc file (subtract 1)? */
#define NUM_FIELDS 2

/* The max number of flags that can show up on a single command (and make 
 * any kind of sense).
 */
#define NUMFLAGS 4

/* How long can a title of a window be? */
#define TITLELEN 76

/* The max length for the help text? */
#define HELPTEXTLEN 80
