/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef lint
static char RCSid[] =
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/help.c,v 3.0 1996/05/06 16:05:28 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"
#include "patchlvl.h"

#include "color.e"
#include "dialog.e"
#ifndef _NO_EXTERN
#include "help.e"
#endif
#include "menu.e"
#include "msg.e"
#include "setup.e"
#include "util.e"
#include "version.e"

#define HELP_ABOUT 0
#define HELP_COPYRIGHT 1

#define MAXHELPS 2

int numHelp=MAXHELPS;

char *helpMenuStr[] = {
      "About     ",
      "Copyright ",
      NULL
};
static char *helpMenuDesc[] = {
      "Version information, where to send bug reports, etc.",
      "Copyright information",
      NULL
};

/* ----------------------- About ----------------------- */

void About()
{
   char *c_ptr=gszMsgBox;

   if (TGIF_PATCHLEVEL == 0) {
      sprintf(c_ptr, "%s Version %s", TOOL_NAME, versionString);
   } else {
      sprintf(c_ptr, "%s Version %s (patchlevel %1d)", TOOL_NAME,
            versionString, TGIF_PATCHLEVEL);
   }
   c_ptr = (&c_ptr[strlen(c_ptr)]);
   sprintf(c_ptr, "\n\n%s", copyrightString);
   c_ptr = (&c_ptr[strlen(c_ptr)]);
   sprintf(c_ptr, "\n\nWWW Hypertext Home Page: %s", homePageURL);
   c_ptr = (&c_ptr[strlen(c_ptr)]);
   sprintf(c_ptr, "\n\nCurrent Release Information: %s", currentReleaseURL);
   c_ptr = (&c_ptr[strlen(c_ptr)]);
   sprintf(c_ptr, "\n\nWWW Hyper-Graphics Home Page: %s", hyperGraphicsURL);
   c_ptr = (&c_ptr[strlen(c_ptr)]);
   sprintf(c_ptr, "\n\nPlease send bug reports to %s or %s.",
         "william@cs.ucla.edu", "william@cs.columbia.edu");
   c_ptr = (&c_ptr[strlen(c_ptr)]);
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);

}

/* ----------------------- Copyright ----------------------- */

static char *gszCopyright[] = {
   "Copyright (C) 1990-1996, William Chia-Wei Cheng",
   "",
   "William Cheng (\"Author\") grants to the party hereby receiving \"tgif\"",
   "(\"Recipient\") a non-exclusive, royalty-free license limited to copy,",
   "display, and distribute without charging for a fee, and produce derivative",
   "works of \"tgif\", provided that the above copyright notice appears in",
   "all copies made of \"tgif\" and both the copyright notice and this license",
   "appear in supporting documentation, and that the name of Author not be",
   "used in advertising or publicity pertaining to \"tgif\".  All other rights",
   "(including, but not limited to, the right to sell \"tgif\", the right to",
   "sell derivative works of \"tgif\", and the right to distribute \"tgif\"",
   "for a fee) are reserved by the Author.",
   "",
   "\"Tgif\" is provided \"as is\" without express or implied warranty. Author",
   "does not and cannot warrant the performance of \"tgif\" or the results",
   "that may be obtained by its use or its fitness for any specific use by",
   "Recipient or any third party.  In no event shall Author become liable",
   "to Recipient or any other party, for any loss or damages,",
   "consequential or otherwise, including but not limited to time, money,",
   "or goodwill, arising from use or possession, authorized or not, of",
   "\"tgif\" by Recipient or any other party.",
   NULL
};

void Copyright()
{
   char **s_ptr=gszCopyright, *c_ptr=gszMsgBox;

   for ( ; *s_ptr != NULL; s_ptr++) {
      if (**s_ptr == '\0') {
         strcpy(c_ptr, "\n\n");
      } else {
         sprintf(c_ptr, "%s ", *s_ptr);
      }
      c_ptr = (&c_ptr[strlen(c_ptr)]);
   }
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
}

/* ----------------------- Init and Clean Up ----------------------- */

void CleanUpHelp()
{
}

void InitHelp()
{
}

/* ----------------------- Menu Functions ----------------------- */

void HelpSubMenu(nIndex)
   int nIndex;
{
   switch (nIndex) {
   case HELP_ABOUT: About(); break;
   case HELP_COPYRIGHT: Copyright(); break;
   }
}

int HelpMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXHELPS, &fore_colors, &valid, &init_rv, NULL);

   activeMenu = MENU_HELP;
   index = TextMenuLoop(X, Y, helpMenuStr, MAXHELPS, fore_colors,
         valid, init_rv, helpMenuDesc, SINGLECOLOR, TrackMenubar);

   if (index >= 0) HelpSubMenu(index);
   return index;
}
