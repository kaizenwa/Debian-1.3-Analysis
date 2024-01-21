/*********************************************************************************/
/* This file is  part of Xcolorsel, a multipurpose viewer for X11 rgb.txt files. */
/*                     Copyright (C) 1993, 1994 Michael Weller                   */
/*                                                                               */
/* This program is free software; you can redistribute it and/or modify it under */
/* the terms of the GNU General Public License as published be the Free Software */
/* Foundation;  either version 2 of the License,  or (at your opinion) any later */
/* version.                                                                      */
/*                                                                               */
/* This program is  distributed in the hope  that it will be useful, but WITHOUT */
/* ANY WARRANTY; without even the  implied warranty of  MERCHANBILITY or FITNESS */
/* FOR  A  PARTICULAR  PURPOSE.   See the  GNU General  Public License  for more */
/* details.                                                                      */
/*                                                                               */
/* You should have received a copy  of the GNU General Public License along with */
/* this program; if not,  write to the  Free Software Foundation, Inc., 675 Mass */
/* Ave, Cambridge, MA 02139, USA.                                                */
/*                                                                               */
/* The author can be  reached by means of email  as eowmob@exp-math.uni-essen.de */
/* or  eowmob@pollux.exp-math.uni-essen.de   or   mat42b@vm.hrz.uni-essen.de  or */
/* mat42b@de0hrz1a.bitnet.  Or as:  Michael Weller,  Heiderhoefen 116b,  D 46049 */
/* Oberhausen, Germany.                                                          */
/*                                                                               */
/* Part of the files are derived  from the Template Widget of the Athena Widgets */
/* as published  by the  Massachusetts Institute of  Technology.  Actually these */
/* files do not contain any code  by the M.I.T  but only  variable declarations. */
/* Nevertheless these parts of these files are still distributed under the terms */
/* of the M.I.T copyright which are here repeated:                               */
/*                                                                               */
/* Copyright    Massachusetts Institute of Technology   1987, 1988               */
/*                                                                               */
/* Permission to use, copy,  modify, distribute,  and sell this software and its */
/* documentation for  any purpose is hereby  granted without fee,  provided that */
/* the  above  copyright  notice  appear  in  all  copies  and  that  both  that */
/* copyright   notice   and   this  permission   notice  appear  in   supporting */
/* documentation,  and that the  name of M.I.T.  not  be used  in advertising or */
/* publicity  pertaining  to distribution  of  the  software  without  specific, */
/* written  prior  permission.    M.I.T.  makes  no  representations  about  the */
/* suitability  of  this  software  for  any  purpose.   It is provided  "as is" */
/* without express or implied warranty.                                          */
/*                                                                               */
/* M.I.T. DISCLAIMS ALL  WARRANTIES WITH REGARD  TO THIS SOFTWARE, INCLUDING ALL */
/* IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS,  IN NO EVENT SHALL M.I.T. */
/* BE LIABLE FOR ANY SPECIAL,  INDIRECT OR CONSEQUENTIAL  DAMAGES OR ANY DAMAGES */
/* WHATSOEVER RESULTING FROM LOSS OF USE,  DATA OR PROFITS, WHETHER IN AN ACTION */
/* OF CONTRACT,  NEGLIGENCE  OR  OTHER  TORTIOUS  ACTION,  ARISING OUT OF  OR IN */
/* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                      */
/*********************************************************************************/

/* Please insert C-Options here instead of specifying them in the Makefiles */

/* To activate the options shown here, remove the surrounding / * * / around the */
/* #define lines. To disable the options surround them with / *   * /  (But omit */
/* the  spaces  between * and / that  I inserted  here  to not  to confuse  some */
/* compilers with nested comments. */

#ifndef MYCONFIG_H
#define MYCONFIG_H

/* Define BIGLENS  if you want to have a bigger magnifying glass cursor compiled */
/* in as default. You may need  to undefine  this if your server doesn't support */
/* such large cursors (or my drawing makes you puke on the server ;-) ) */

/* You may define an absolute Helpfile name here that will be searched first. */

/* #define HELPFILE "/usr/lib/X11/Xcolorsel.help" */

/* When grabbing the Cursor, RgbText itself ensures that the colors of windows with
   own colormaps are displayed when the cursor goes in. If you want to have it to
   reinstall the colormaps when the grab started at the end of the grab define
   REINSTALLCOLORMAPS. However any normal window manager will set the colormap to
   the one that RgbText/Xcolorsel has choosen immediately when it gets control of
   the mouse back. So this will lead to some flicker. So define REINSTALLCOLORMAPS
   only as a try when your colormaps get confused otherwise (But probably definition
   of this flag will even raise the confusion.) */

/* #define REINSTALLCOLORMAPS */

/* Some Windowmanagers (read fvwm) intercept ColormapInstall events and decide to
   reinstall what they think. This gives annoying flicker when Xcolorsel installs
   a colormap during color grabbing and then fvwm decides to reinstall the colormap
   of Xcolorsel's window. #defining GRABSERVER grabs the server during colorgrabbing
   and gives fvwm no chance to do dirty things. However it blocks all windows as
   well and is very risky. Thus it is also only done for Xservers that need
   colormaps, that is no direct color servers. Still when the grab finishes fvwm
   switches colors around and gives a short flicker. Since fvwm has the same
   problems with xmag, someone should just fix fvwm. Use this flag only as a last
   resort. */

#define GRABSERVER

/* Some systems (AIX) seem to have problems with Xpalette installing colormaps during
   color grabbing (For details see the BUGS section in the man page or Xcolorsel.help).
   So here is way for you do disable this: */

/*#define NOCOLORMAPINSTALL*/

#endif /*MYCONFIG_H*/
