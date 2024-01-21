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

String default_help=
"This is Xcolorsel, a multipurpose viewer for X11 rgb.txt files.\n"
"Copyright (C) 1993, 1994 Michael Weller\n"
"\n"
"This program is free software; you can redistribute it and/or modify it under\n"
"the terms of the GNU General Public License as published be the Free Software\n"
"Foundation; either version 2 of the License, or (at your opinion) any later\n"
"version.\n"
"\n"
"This program is distributed in the hope that it will be useful, but WITHOUT\n"
"ANY WARRANTY; without even the implied warranty of MERCHANBILITY or FITNESS\n"
"FOR A PARTICULAR PURPOSE. See the GNU General Public License for more\n"
"details.\n"
"\n"
"You should have received a copy of the GNU General Public License along with\n"
"this program; if not, write to the Free Software Foundation, Inc., 675 Mass\n"
"Ave, Cambridge, MA 02139, USA.\n"
"\n"
"The author can be reached by means of email as eowmob@exp-math.uni-essen.de\n"
"or eowmob@pollux.exp-math.uni-essen.de or mat42b@aixrs1.hrz.uni-essen.de.\n"
"Or as: Michael Weller, Heiderhoefen 116b, D 46049 Oberhausen, Germany.\n"
"\n"
"Part of the files are derived from the Template Widget of the Athena Widgets\n"
"as published by the Massachusetts Institute of Technology. Actually these\n"
"files do not contain any code by the M.I.T but only variable declarations.\n"
"Nevertheless these parts of these files are still distributed under the terms\n"
"of the M.I.T copyright which are here repeated:\n"
"\n"
"Copyright Massachusetts Institute of Technology 1987, 1988\n"
"\n"
"Permission to use, copy, modify, distribute, and sell this software and its\n"
"documentation for any purpose is hereby granted without fee, provided that\n"
"the above copyright notice appear in all copies and that both that\n"
"copyright notice and this permission notice appear in supporting\n"
"documentation, and that the name of M.I.T. not be used in advertising or\n"
"publicity pertaining to distribution of the software without specific,\n"
"written prior permission. M.I.T. makes no representations about the\n"
"suitability of this software for any purpose. It is provided \"as is\"\n"
"without express or implied warranty.\n"
"\n"
"M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL\n"
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.\n"
"BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES\n"
"WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION\n"
"OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN\n"
"CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.\n"
"\n"
"When you see this default message, Xcolorsel could not load its helpfile.\n"
"Set the helpfile Resource of Xcolorsel to the absolut filename of the\n"
"helpfile.\n"
"You can also use the -helpfile >name< command line parameter to do this or\n"
"put Xcolorsel.help in the current directory.\n"
"\n"
"To skip searching for an explicit file you may set the absolut filename to\n"
"the empty string.\n"
"\n"
"But before looking in the current directory, Xcolorsel searches its helpfile\n"
"with XtResolvePathname and a file type of \"help\" or a suffix \".help\".\n"
"Also --- between these two possibilities --- it looks for type \"help\" AND\n"
"suffix \".help\".\n"
"\n"
"Although this is implementation specific it will probably result in a search\n"
"path like:\n"
"    /usr/lib/X11/help/Xcolorsel:/usr/lib/X11/help/Xcolorsel.help:\\\n"
"    /usr/lib/X11/Xcolorsel.help\n"
"\n"
"The actual absolut setting where to find the help file is:\n"
"    %s\n";
