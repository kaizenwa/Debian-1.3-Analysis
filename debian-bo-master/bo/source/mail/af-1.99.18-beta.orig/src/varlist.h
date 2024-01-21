/* Varlist.h - List of internal variables for af
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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

#ifndef lint
static char *VarlistId = "$Id: varlist.h,v 1.18 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Functions used to get values for variables */

static char *vg_string(), *vg_file(), *vg_sort(), *vg_char();
static char *vg_boolean(), *vg_tristate(), *vg_edithdrs();
static char *vg_numeric();

/* Functions used to expand form values for variables */

static char *vf_string(), *vf_char(), *vf_tristate();
static char *vf_numeric(), *vf_list();

/* Functions used to set variables */

static int vs_string(), vs_nonnull(), vs_file(), vs_format();
static int vs_address(), vs_name(), vs_domain(), vs_tags();
static int vs_viewable(), vs_char(), vs_numeric(), vs_timer();
static int vs_echo(), vs_boolean(), vs_tristate(), vs_edithdrs();
static int vs_arrow(), vs_hdr_format(), vs_mode_format();
static int vs_localtime();

/* Functions used to print variables */

static char *vp_string(), *vp_char(), *vp_tristate();
static char *vp_numeric();

/* Functions used to evaluate variables */

static FORM *ve_string(), *ve_char(), *ve_tristate();
static FORM *ve_numeric(), *ve_list();

/****************************************************************************/
/* Defaults for the variables that have them */

#define VDEF_PREFIX		"> "
#define VDEF_ECHO		"2"
#define VDEF_NOTDISP		">From :Return-Path:Received:Message-Id:\
References:Apparently-To:Mailer:X-Mailer:\
Status:X-Status:X-Af-Status:X-Af-Tags:"
#define VDEF_ARROW		"=>"
#define VDEF_HDRLINE		"%a %t  %o  %s"
#define VDEF_INFO		DEFINFOBROWSER
#define VDEF_KILL_RING		"8"
#define VDEF_MSG_UPDATE		"5"
#define VDEF_METACHAR		"\033"
#define VDEF_MIMEPAGER		DEFMIMEPAGER
#define VDEF_MIMEPRINTER	DEFMIMEPRINTER
#define VDEF_MIMESAVER		DEFMIMESAVER
#define VDEF_MODELINE		"=%*= Af: %b == %n == (%m) == %p =="
#define VDEF_CONTEXT		"2"
#define VDEF_THRESHOLD		"100"
#define VDEF_PAGER		V_USE_TYPEOUT
#define VDEF_PRINT_CMD		DEFSPOOLER
#define VDEF_QUITCHAR		"\007"
#define VDEF_RESYNC		"600"
#define VDEF_SEPARATOR		"-- "
#define VDEF_SMTP_HOST		DEFAULT_SMTP_HOST
#define VDEF_SPELLCHECK		DEFSPELLCHECKER
#define VDEF_VIEWABLE		"us-ascii"

/****************************************************************************/
/* The list of af variables by name */

static VARIABLE variables[] = {
	{ V_ADDRESSES,	vg_string, vf_string, vs_address,
			vp_string, ve_string, NULL },
	{ V_ASK_BCC,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_FALSE },
	{ V_ASK_CC,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_FALSE },
	{ V_AUTOFOLD,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_TRUE },
	{ V_CASEFOLD,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_TRUE },
	{ V_ASK_PRINT,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_TRUE },
	{ V_COPY,	vg_tristate, vf_tristate, vs_tristate,
			vp_tristate, ve_tristate, VI_ASK },
	{ V_PREFACE,	vg_string, vf_string, vs_format,
			vp_string, ve_string, NULL },
	{ V_COPY_PFX,	vg_string, vf_string, vs_nonnull,
			vp_string, ve_string, VDEF_PREFIX },
	{ V_DOMAIN,	vg_string, vf_string, vs_domain,
			vp_string, ve_string, NULL },
	{ V_ECHO,	vg_numeric, vf_numeric, vs_echo,
			vp_numeric, ve_numeric, VDEF_ECHO },
	{ V_EDIT_IHDRS,	vg_edithdrs, vf_tristate, vs_edithdrs,
			vp_tristate, ve_tristate, VI_FALSE },
	{ V_EDIT_ISIG,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_FALSE },
	{ V_EDIT_REPLY,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_FALSE },
	{ V_EDITOR,	vg_string, vf_string, vs_string,
			vp_string, ve_string, NULL },
	{ V_FIRSTNEW,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_TRUE },
	{ V_FOLDER,	vg_file, vf_string, vs_file,
			vp_string, ve_string, NULL },
	{ V_ARROW,	vg_string, vf_string, vs_arrow,
			vp_string, ve_string, VDEF_ARROW },
	{ V_HDRLINE,	vg_string, vf_string, vs_hdr_format,
			vp_string, ve_string, VDEF_HDRLINE },
	{ V_NOTDISP,	vg_string, vf_list, vs_string,
			vp_string, ve_list, VDEF_NOTDISP },
	{ V_TOCOPY,	vg_string, vf_list, vs_string,
			vp_string, ve_list, NULL },
	{ V_INFO,	vg_string, vf_string, vs_string,
			vp_string, ve_string, VDEF_INFO },
	{ V_INIT_SORT,	vg_sort, vf_string, vs_string,
			vp_string, ve_string, NULL },
	{ V_KILL_RING,	vg_numeric, vf_numeric, vs_numeric,
			vp_numeric, ve_numeric, VDEF_KILL_RING },
	{ V_LOADPATH,	vg_string, vf_list, vs_string,
			vp_string, ve_list, NULL },
	{ V_MSG_UPDATE,	vg_numeric, vf_numeric, vs_numeric,
			vp_numeric, ve_numeric, VDEF_MSG_UPDATE },
	{ V_METACHAR,	vg_char, vf_char, vs_char,
			vp_char, ve_char, VDEF_METACHAR },
	{ V_MIMEPAGER,	vg_string, vf_string, vs_string,
			vp_string, ve_string, VDEF_MIMEPAGER },
	{ V_MIMEPRINTER,vg_string, vf_string, vs_string,
			vp_string, ve_string, VDEF_MIMEPRINTER },
	{ V_MIMESAVER,	vg_string, vf_string, vs_string,
			vp_string, ve_string, VDEF_MIMESAVER },
	{ V_MODELINE,	vg_string, vf_string, vs_mode_format,
			vp_string, ve_string, VDEF_MODELINE },
	{ V_MREP_WARN,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_FALSE },
	{ V_NEWSFOLDER,	vg_file, vf_string, vs_file,
			vp_string, ve_string, NULL },
	{ V_CONTEXT,	vg_numeric, vf_numeric, vs_numeric,
			vp_numeric, ve_numeric, VDEF_CONTEXT },
	{ V_ORG,	vg_string, vf_string, vs_string,
			vp_string, ve_string, NULL },
	{ V_OUTBOUND,	vg_file, vf_string, vs_file,
			vp_string, ve_string, NULL },
	{ V_THRESHOLD,	vg_numeric, vf_numeric, vs_numeric,
			vp_numeric, ve_numeric, VDEF_THRESHOLD },
	{ V_PAGER,	vg_string, vf_string, vs_string,
			vp_string, ve_string, VDEF_PAGER },
	{ V_PAUSE,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_TRUE },
	{ V_PENDING,	vg_string, vf_string, vs_file,
			vp_string, ve_string, NULL },
	{ V_PTAGS,	vg_string, vf_string, vs_tags,
			vp_string, ve_string, NULL },
	{ V_KEEP_CC,	vg_boolean, vf_tristate, vs_boolean,
			vp_tristate, ve_tristate, VI_TRUE },
	{ V_PRINT_CMD,	vg_string, vf_string, vs_string,
			vp_string, ve_string, VDEF_PRINT_CMD },
	{ V_QUITCHAR,	vg_char, vf_char, vs_char,
			vp_char, ve_char, VDEF_QUITCHAR },
	{ V_REALNAME,	vg_string, vf_string, vs_name,
			vp_string, ve_string, NULL },
	{ V_REPLY,	vg_string, vf_string, vs_address,
			vp_string, ve_string, NULL },
	{ V_RESYNC,	vg_numeric, vf_numeric, vs_timer,
			vp_numeric, ve_numeric, VDEF_RESYNC },
	{ V_SHOW_LTIME,	vg_boolean, vf_tristate, vs_localtime,
			vp_tristate, ve_tristate, VI_TRUE },
	{ V_SIGFILE,	vg_file, vf_string, vs_file,
			vp_string, ve_string, NULL },
	{ V_SEPARATOR,	vg_string, vf_string, vs_string,
			vp_string, ve_string, VDEF_SEPARATOR },
#ifdef MTA_IS_SMTP
	{ V_SMTP_HOST,	vg_string, vf_string, vs_nonnull,
			vp_string, ve_string, VDEF_SMTP_HOST },
#endif /* MTA_IS_SMTP */
	{ V_SPELLCHECK,	vg_string, vf_string, vs_string,
			vp_string, ve_string, VDEF_SPELLCHECK },
	{ V_VIEWABLE,	vg_string, vf_list, vs_viewable,
			vp_string, ve_list, VDEF_VIEWABLE },
	{ NULL,		NULL, NULL, NULL, NULL, NULL, NULL }
};

/****************************************************************************/
