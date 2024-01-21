/* ########################################################################

			       cb_names.c

   File: cb_names.c
   Path: /home/fournigault/c/X11/xcoral-2.31/cb_names.c
   Description: 
   Created: Fri Jan 27 10:51:26 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:51:27 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "options.h"
#include "xcoral.h"
#include "browser_init.h"
#include "cb_names.h"
#include "chars_cmds.h"
#include "new_window.h"
#include "get_file.h"
#include "man_box.h"
#include "process.h"
#include "bm_search.h"
#include "mark_reg.h"
#include "page.h"
#include "ctr_version.h"

char *menu_names[] = { 
	"File", "Window", "Modes", "Search", "Region", "Version", "Font", "Misc", 0
};

static char *file_menu [] = {
	"New file        ^x k",
	"Read file       ^x^f",
	"Insert file     ^x i",
	"Save file       ^x^s",
	"Write file as   ^x^w",
	"Quit            ^x^c",
	0
};
			
static char *window_menu [] = {
	"New text window     ^x n",
	"Delete text window  ^x^c",
	"Display kill buffer",
	"Display open files  ^x b",
	"Display browser",
	"Display manual",
	0
};

static char *mode_menu [] = {
	"Select default mode",
	"Select C mode",
	"Select C++ mode",
	"Select Latex mode",
	"Select Html mode",
	"Select Perl mode",
	"Select Ada mode",
	"Select Fortran mode",
	"Select shell-script mode",
	"Execute a sub Shell",
	"Select others ...",
	"Create new mode",
	0 
};
			
static char *search_menu [] = { 
	"Forward search        ^s",
	"Backward search       ^r",
	"Query replace      Esc q",
	"Global replace     Esc r",
	"Goto line number    ^x l",
	"Regexp forward search",
	"Regexp backward search",
	"Regexp replace",
	0
};

static char *mark_menu [] = {
	"Set mark              ^space",
	"Goto the mark           ^x m",
	"Exchange point mark     ^x^x",
	"Kill region               ^w",
	"Copy region            Esc w",
	"Paste region              ^y",
	"Indent region          Esc i",
	"Eval region            Esc e",
	"Color buffer                ",
	"Color region                ",
	"Current line to top    Esc t",
	0
};

static char *version_menu [] = {
        "Initialize version",
        "Check in",
	"Check out locked",
	"Check out unlocked",
	"Check in and out locked",
	"Check in and out unlocked",
	"Lock revision",
	"Unlock revision",
	"Display diff",
	"Display log",
	"Display repository",
	0
};

static char *font_menu [] = { 
	"Courier bold 10",
	"Courier bold 12",
	"Courier bold 14",
	"Courier medium 10",
	"Courier medium 12",
	"Courier medium 14",
	"Courier medium 18",
	"Helvetica 10",
	"Helvetica 14",
	"Times bold 14",
	"Times 18",
	"Times 20",
	"Times 24",
	"Schoolbook 18",
	0 
};

static char *font_names [] = {
	"-adobe-courier-bold-r-normal--*-100-*-*-m-*-iso8859-1",  
	"-adobe-courier-bold-r-normal--*-120-*-*-m-*-iso8859-1",
	"-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1",
	"-adobe-courier-medium-r-normal--10-100-75-75-m-60-iso8859-1",
	"-adobe-courier-medium-r-normal--*-120-*-*-m-*-iso8859-1",
	"-adobe-courier-medium-r-normal--*-140-*-*-m-*-iso8859-1",
	"-adobe-courier-medium-r-normal--*-180-*-*-m-*-iso8859-1",
	"-adobe-helvetica-medium-r-normal--*-100-*-*-p-*-iso8859-1",
	"-adobe-helvetica-medium-r-normal--*-140-*-*-p-*-iso8859-1",
	"-adobe-times-bold-r-normal--*-140-*-*-p-*-iso8859-1",
	"-adobe-times-medium-r-normal--*-180-*-*-p-*-iso8859-1",
	"-adobe-times-medium-r-normal--*-200-*-*-p-*-iso8859-1",
	"-adobe-times-medium-r-normal--*-240-*-*-p-*-iso8859-1",
	"-adobe-new century schoolbook-medium-r-normal--*-180-*-*-p-*-iso8859-1",
	0
};

static char *misc_menu [] = {
	"New C++ class",
	"New C++ method",
	"New C function",
	"New include header",
	"Latex style and macros",
	"Html macros",
	"Misc commands",
	"User commands",
	"Load and eval file",
	"Eval expression ^x^e",
	"Xcoral release",
	"Help",
	0
};

char **item_names [] = {
	file_menu,
	window_menu,
	mode_menu,
	search_menu,
	mark_menu,
	version_menu,
	font_menu,
	misc_menu,
	0
};

static void (*f_file []) () = {
	MenuNewFile,
	MenuReadFile,
	MenuInsertFile,
	MenuSaveFile,
	MenuWriteFile,
	(void (*)()) DeleteWindow, 
	0
};

static void (*f_window []) () = { 
	NewWindow,
	(void (*)()) DeleteWindow,
	DisplayKillBuffer,
	DisplayOpenFiles,
	DisplayBrowser,
	DisplayManBox,
	0
};

static void (*f_mode []) () = {
	SetDefaultMode,
	SetCMode,
	SetCCMode,
	SetLatexMode,
	SetHtmlMode,
	SetPerlMode,
	SetAdaMode,
	SetFortranMode,
	SetshellMode,
	SetShellMode,
	LoadMode,
	NewMode,
	0
};

static  void (*f_search []) () = { 
	MenuForwardSearch,
	MenuBackwardSearch,
	MenuQueryReplace,
	MenuGlobalReplace,
	MenuGotoLine,
	RE_MenuForwardSearch,
	RE_MenuBackwardSearch,
	RE_MenuReplace,
	0
};

static void (*f_mark []) () = {
	SetMark,
	GotoTheMark,
	ExchangePointMark,
	KillRegion,
	CopyRegion,
	PasteRegion,
	IndentRegion,
	EvalRegion,
	ColorBuffer,
	ColorRegion,
	CurrentLineToTopFromMenu,
	0
};

static void (*f_version []) () = {
        CV_Initialize,
	CV_CheckIn,
	CV_CheckOutLocked,
	CV_CheckOutUnlocked,
	CV_CheckInAndOutLocked,
	CV_CheckInAndOutUnlocked,
	CV_LockRevision,
	CV_UnlockRevision,
	CV_ListDiff,
	CV_ListLog,
	CV_ListRepository,
	0
};

static void (*f_font []) () = { 
	0
};

static void (*f_misc []) () = {
	ClassHeader,
	MethodHeader,
	FunctionHeader,
	IncludeHeader,
	LatexMacros,
	HtmlMacros,
	MiscCommands,
	UserCommands,
	LoadAndEvalFile,
	EvalExpressionFromMenu,
	Version,
	DisplayManBox,
	0
};

void (**func_names []) () = {
	f_file, f_window, f_mode, f_search, f_mark, f_version, f_font, f_misc, 0
};

#define M_FONTS 6

/*
**	Function name : ExecMenuFunc
**
**	Description : Les fonctions utilisables a partir dee menus.
**	Input : 
**	Ouput :
*/
void ExecMenuFunc ( vm, item )
    int vm, item;
{
  extern void exit ();
  
  if ( vm == -1 ) return;
  
  if ( vm ==  M_FONTS ) {
    ChangeFont ( dpy, edwin -> text, font_names [item] );
    return;
  }
  ((func_names [vm]) [item]) ( edwin -> text );
  if ( IsLastWindow ( 0 ) == True ) {
    XCloseDisplay ( dpy );
    (void) exit (0);
  }
}

