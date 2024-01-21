/* Keymaps.h - Keymap and binding definitions for af.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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


/****************************************************************************/
/* RCS info */

#define KEYMAPID	"$Id: keymaps.h,v 1.35 1997/03/05 21:23:45 malc Exp $"

/****************************************************************************/
/* These definitions define the names of the keymaps */

#define GLOBAL_MAP		"global"
#define HELP_MAP		"help-command"
#define PREFIX_MAP		"prefix-command"
#define CONTROL_X_MAP		"control-x-prefix"
#define CONTROL_C_MAP		"control-c-prefix"
#define MAIL_MAP		"mail"
#define MAIL_PREFIX_MAP		"mail-prefix-command"
#define MAIL_CONTROL_T_MAP	"mail-control-t-prefix"
#define MAIL_CONTROL_X_MAP	"mail-control-x-prefix"
#define MAIL_CONTROL_X_4_MAP	"mail-control-x-4-prefix"
#define MAIL_CONTROL_C_MAP	"mail-control-c-prefix"
#define POP3_MAP		"pop3"
#define TYPEOUT_MAP		"typeout"
#define TYPEOUT_PREFIX_MAP	"typeout-prefix-command"
#define TYPEOUT_CONTROL_X_MAP	"typeout-control-x-prefix"
#define TYPEOUT_CONTROL_C_MAP	"typeout-control-c-prefix"
#define MBUF_MAP		"minibuffer"
#define MBUF_PREFIX_MAP		"minibuffer-prefix-command"
#define MBUF_CONTROL_X_MAP	"minibuffer-control-x-prefix"
#define MBUF_CONTROL_C_MAP	"minibuffer-control-c-prefix"
#define COMPLETE_MAP		"complete"

/****************************************************************************/
/* The types of object that may be bound within a keymap */

#define B_UNBOUND	0
#define B_COMMAND	1
#define B_KEYMAP	2
#define B_MACRO		3

/****************************************************************************/
/* Macros used to check if a key is bound to a given type */

#define IS_UNBOUND(x)	((x) == NULL || (x)->type == B_UNBOUND)
#define IS_COMMAND(x)	((x) != NULL && (x)->type == B_COMMAND)
#define IS_KEYMAP(x)	((x) != NULL && (x)->type == B_KEYMAP)
#define IS_MACRO(x)	((x) != NULL && (x)->type == B_MACRO)

/* And one to discover if a binding is active in a given mode */

#define IS_ACTIVE(x, m)	(!IS_COMMAND(x) || (x)->object.cmd->modes & (m))

/****************************************************************************/
/* A special key that asks for any existing map in mode_keymap */

#define ANY_KEY		-1

/****************************************************************************/
/* The union used to store a bound object */

typedef union object {
	struct command *cmd;			/* A bound command */
	struct keymap *map;			/* A bound keymap */
	struct macro *macro;			/* A bound macro */
} OBJECT;

/* The internal representation of a key binding */

typedef struct binding {
	char key;				/* The key within the map */
	char type;				/* Type of object bound */
	OBJECT object;				/* The bound object */
	struct binding *next;			/* Next binding in list */
} BINDING;

/****************************************************************************/
/* The structure used to store a keymap */

typedef struct keymap {
	char *name;			/* The name of the keymap */
	BINDING *bindings;		/* Keys bound in the map */
	struct keymap *next;		/* Next keymap in list */
} KEYMAP;

/****************************************************************************/
/* The string output by help-on-help */

#define HELP_STRING	"C-a C-c C-d C-f C-k C-v C-w a b c d f i j k l \
m n v or C-h or h again: "

/****************************************************************************/
