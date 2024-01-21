/* Variable.h - Declarations for variable handling in af.
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

#define VARIABLEID	"$Id: variable.h,v 1.37 1997/03/05 21:23:45 malc Exp $"

/****************************************************************************/
/* The structure to hold the details of a variable */

typedef struct {
	char *name;				/* Name of the variable */
	char *(*getfunc)();			/* Function to get value */
	char *(*formfunc)();			/* Function to handle form */
	int (*setfunc)();			/* Function to set var */
	char *(*printfunc)();			/* Function to print var */
	FORM *(*evalfunc)();			/* Function to eval var */
	char *deflt;				/* Initialisation text */
	char *text;				/* Current text */
	int value;				/* Non-string value */
} VARIABLE;

/****************************************************************************/
/* Af's configuration variables */

#define V_ADDRESSES	"addresses-to-ignore"
#define V_ASK_BCC	"ask-bcc"
#define V_ASK_CC	"ask-cc"
#define V_AUTOFOLD	"auto-fold-headers"
#define V_CASEFOLD	"case-fold-search"
#define V_ASK_PRINT	"confirm-print"
#define V_COPY		"copy-on-reply"
#define V_PREFACE	"copy-preface"
#define V_COPY_PFX	"copy-prefix"
#define V_DOMAIN	"domain"
#define V_ECHO		"echo-keystrokes"
#define V_EDIT_IHDRS	"edit-initial-headers"
#define V_EDIT_ISIG	"edit-initial-signature"
#define V_EDIT_REPLY	"edit-reply-address"
#define	V_EDITOR	"editor"
#define V_FIRSTNEW	"first-unread-message"
#define V_FOLDER	"folder"
#define V_ARROW		"header-line-arrow"
#define V_HDRLINE	"header-line-format"
#define V_NOTDISP	"headers-not-displayed"
#define V_TOCOPY	"headers-to-copy"
#define V_INFO		"info-browser"
#define V_INIT_SORT	"initial-buffer-sort"
#define V_KILL_RING	"kill-ring-max"
#define V_LOADPATH	"load-path"
#define V_MSG_UPDATE	"message-count-update"
#define V_METACHAR	"meta-prefix-char"
#define V_MIMEPAGER	"mime-pager"
#define V_MIMEPRINTER	"mime-printer"
#define V_MIMESAVER	"mime-saver"
#define V_MODELINE	"mode-line-format"
#define V_MREP_WARN	"multiple-reply-warning"
#define V_NET_TIMEOUT	"network-timeout"
#define V_NEWSFOLDER	"news-folder"
#define V_CONTEXT	"next-screen-context-lines"
#define V_ORG		"organization"
#define V_OUTBOUND	"outbound-folder"
#define V_THRESHOLD	"outbound-threshold"
#define V_PAGER		"pager"
#define V_PAUSE		"pause-after-paging-message"
#define V_PENDING	"pending-folder"
#define V_PTAGS		"persistent-tags"
#define V_KEEP_CC	"preserve-cc-in-group-reply"
#define V_PRINT_CMD	"print-command"
#define V_QUITCHAR	"quit-char"
#define V_REALNAME	"real-name"
#define V_REPLY		"reply-address"
#define V_RESYNC	"resync-time"
#define V_SHOW_LTIME	"show-dates-in-local-time"
#define V_SIGFILE	"signature-file"
#define V_SEPARATOR	"signature-separator"
#define V_SMTP_HOST	"smtp-posting-host"
#define V_SPELLCHECK	"spell-checker"
#define V_VIEWABLE	"viewable-charsets"

/****************************************************************************/
/* Special text values for variables */

#define VI_TRUE		"true"
#define VI_FALSE	"false"
#define VI_ASK		"ask"
#define VI_ACCEPT	"accept"

/****************************************************************************/
/* Special values for variables */

#define V_TRUE		TRUE
#define V_FALSE		FALSE
#define V_ASK		2
#define V_ACCEPT	V_ASK
#define V_ASK_SIGFILE	"ask:"
#define V_USE_TYPEOUT	"typeout"

/****************************************************************************/
