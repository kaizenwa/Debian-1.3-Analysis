/* version.c - Tracking releases
 *
 * Copyright (C) 1994 Valeriy E. Ushakov
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you can either send email to this
 * program's author (see below) or write to:
 * 
 *              The Free Software Foundation, Inc.
 *              675 Mass Ave.
 *              Cambridge, MA 02139, USA. 
 * 
 * Please send bug reports, etc. to uwe@niif.spb.su
 * 
 * $Log: gwm.shar,v $
 * Revision 1.115  1995/12/08 07:51:55  colas
 * ********************
 * *** Version 1.8c ***
 * ********************
 *
 * Revision 1.100  1995/05/29  15:56:57  colas
 * simple-win.gwm: new parameters:
 *     label like simple-icon
 *     legend to place the label on sides of window
 *     lpad and rpad: number of () to pad the label with stretchable space
 * bar-max-wdths set by default to 1000
 *
 * John Carr <jfc@MIT.EDU>: patches to supress warnings on AIX/RS_6000/xlc
 * rxterm install fixed once more
 *
 * Revision 1.97  1995/05/16  16:16:36  colas
 * contrib/scripts/find-bar-nils
 *
# Revision 1.5  1995/05/15  22:29:34  colas
# bar can have abitrary shaped backgrounds (shaped tiles)
#
 * Revision 1.95  1995/05/11  17:06:56  colas
 * better spy
 *
 * Revision 1.93  1995/04/26  16:34:51  colas
 * Makefile added in distrib
 *
 * simple-icon.gwm:
 *
 *     - customize item "legend" can now be instead of () or t the strings:
 *       "top" "base" "right" "left" for the positions where you want the string
 *       to appear
 *       e.g: (customize simple-icon any XTerm "left")
 *
 *     - new customization item "label" to provide either a fixed string or a
 *       lambda which will be used to filter the label
 *       must return a non-empty string otherwise the unfiltered label is used
 *       e.g: to supress the Netscape: in netscape icon titles
 *       (customize simple-icon any Netscape
 *           label (lambdaq (s) (match "Netscape: \\(.*\\)$" s 1))
 *       )
 *
 * iconify a window doesnt not loose the window anymore in case of error in wool
 * code
 *
 * Revision 1.92  1995/04/25  14:31:09  colas
 * *** Version 1.7p_beta_2 ***
 *
 * Revision 1.0  1994/09/29  18:29:40  uwe
 * Initial revision
 *
 */

static char *Version = "$Id: gwm.shar,v 1.115 1995/12/08 07:51:55 colas Exp $";
