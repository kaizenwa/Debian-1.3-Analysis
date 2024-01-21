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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/mainloop.c,v 3.0 1996/05/06 16:05:49 william Exp $";
#endif

#include <stdio.h>
#include <signal.h>
#include <sys/wait.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#ifdef USE_XT_INITIALIZE
#include <X11/Intrinsic.h>
#endif
#include "const.h"
#include "patchlvl.h"
#include "types.h"

#include "animate.e"
#include "auxtext.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cutpaste.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "exec.e"
#include "expr.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "help.e"
#include "imgproc.e"
#include "import.e"
#ifndef _NO_EXTERN
#include "mainloop.e"
#endif
#include "mainmenu.e"
#include "menu.e"
#include "msg.e"
#include "names.e"
#include "navigate.e"
#include "obj.e"
#include "page.e"
#include "ps.e"
#include "raster.e"
#include "remote.e"
#include "ruler.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#include "shape.e"
#include "shortcut.e"
#include "stk.e"
#include "text.e"
#include "version.e"
#ifdef _TGIF_WB
#include "wb1.e"
#endif /* _TGIF_WB */
#include "xbitmap.e"
#include "xpixmap.e"

extern int	atoi ARGS_DECL((char *));
#ifndef _NO_EXTERN
extern int	fork ARGS_DECL((void));
#if defined(IRIX) || defined(linux)
extern int	system ARGS_DECL((const char *));
#else /* ~(defined(IRIX) || defined(linux)) */
extern int	system ARGS_DECL((char *));
#endif /* defined(IRIX) || defined(linux) */
#endif

#ifdef USE_XAPPLRESDIR
extern char	* getenv ARGS_DECL((char *));
extern int	putenv ARGS_DECL((char *));
#else
#ifdef USE_XT_INITIALIZE
Widget		toplevel;
#endif
#endif

int	origArgC = 0;
char	* * origArgV = NULL;

int	cmdLineBW = FALSE;
int	cmdLineRV = INVALID;
int	cmdLineCWO = FALSE;
char	* cmdLineForeground = NULL;
char	* cmdLineBackground = NULL;
char	* cmdLineBorder = NULL;
int	geometrySpecified = FALSE;
int	exitNormally = FALSE;
char	geometrySpec[80];
char	* displayName = NULL;
char	initMsg1[80], initMsg2[80];

int			numExtraWins = 0;
struct WinInfoRec	* extraWinInfo = NULL;

static int	maxExtraWins = 0;
static int	quitDraw = TRUE;
#ifdef USE_XAPPLRESDIR
static char	* xEnvStr = NULL;
#endif

int AddExtraWinInfo (win, mapped, raise, expose_handler, ev_handler,
      cleanup_routine)
   Window	win;
   int		mapped, raise;
   void		(* expose_handler)();
   int		(* ev_handler)();
   void		(* cleanup_routine)();
{
   register int	i;

   for (i = 0; i < numExtraWins; i++)
      if (extraWinInfo[i].window == None)
         break;

   if (i == numExtraWins && numExtraWins == maxExtraWins)
   {
      maxExtraWins += 10;
      extraWinInfo = (struct WinInfoRec *) realloc (extraWinInfo,
            maxExtraWins*sizeof(struct WinInfoRec));
   }
   extraWinInfo[i].window = win;
   extraWinInfo[i].mapped = mapped;
   extraWinInfo[i].raise = raise;
   extraWinInfo[i].expose_handler = expose_handler;
   extraWinInfo[i].ev_handler = ev_handler;
   extraWinInfo[i].cleanup = cleanup_routine;
   return ((i==numExtraWins) ? numExtraWins++ : i);
}

static
void InitExtraWinInfo ()
{
   extraWinInfo = (struct WinInfoRec *)malloc(10*sizeof(struct WinInfoRec));
   if (extraWinInfo == NULL) FailAllocMessage();
   memset(extraWinInfo, 0, 10*sizeof(struct WinInfoRec));
   maxExtraWins += 10;
}

static
void CleanUpExtraWinInfo ()
{
   register int	i;

   if (extraWinInfo != NULL) {
      for (i = 0; i < numExtraWins; i++) {
         if (extraWinInfo[i].window != None) {
            (*(extraWinInfo[i].cleanup))();
         }
      }
      free(extraWinInfo);
   }
   numExtraWins = 0;
   maxExtraWins = 0;
   extraWinInfo = NULL;
}

static
void ExecWithFile (CmdName, FileName)
   char	* CmdName, * FileName;
{
   char	s[255];

#ifdef _BACKGROUND_DONT_FORK
   sprintf (s, "xterm -bd red -e %s %s", CmdName, FileName);
   strcat(s, " &");
   system(s);
#else /* ~_BACKGROUND_DONT_FORK */
   int	pid;

   sprintf (s, "xterm -bd red -e %s %s", CmdName, FileName);
   pid = fork ();
   if (pid == 0)
   {
      system(s);
      exit (0);
   }
#endif /* _BACKGROUND_DONT_FORK */
}

void DeallocStrings(FStr, Str1, Menu1, Str2, Menu2, Str3, Menu3)
   char **FStr, **Str1, **Menu1, **Str2, **Menu2, **Str3, **Menu3;
{
   free(*FStr);
   free(*Str1);
   free(*Menu1);
   free(*Str2);
   free(*Menu2);
   free(*Str3);
   free(*Menu3);
}

static
void AllocStrings(FStr, Str1, Menu1, Str2, Menu2, Str3, Menu3)
   char **FStr, **Str1, **Menu1, **Str2, **Menu2, **Str3, **Menu3;
{
   char *s;

   if((s=(char*)malloc(80*sizeof(char))) == NULL) FailAllocMessage();
   *FStr = s; **FStr = '\0';
   if((s=(char*)malloc(80*sizeof(char))) == NULL) FailAllocMessage();
   *Str1 = s; **Str1 = '\0';
   if((s=(char*)malloc(80*sizeof(char))) == NULL) FailAllocMessage();
   *Menu1 = s; **Menu1 = '\0';
   if((s=(char*)malloc(80*sizeof(char))) == NULL) FailAllocMessage();
   *Str2 = s; **Str2 = '\0';
   if((s=(char*)malloc(80*sizeof(char))) == NULL) FailAllocMessage();
   *Menu2 = s; **Menu2 = '\0';
   if((s=(char*)malloc(80*sizeof(char))) == NULL) FailAllocMessage();
   *Str3 = s; **Str3 = '\0';
   if((s=(char*)malloc(80*sizeof(char))) == NULL) FailAllocMessage();
   *Menu3 = s; **Menu3 = '\0';
}

/* 
 * static
 * int MyErrorHandler (display, event)
 *    Display	* display;
 *    XErrorEvent	* event;
 * {
 *    if (event->type == 0) return (TRUE);
 *    printf ("\tError: type -- %1d\n", event->type);
 *    exit (-1);
 * }
 */

void CleanUp ()
{
   TieLooseEnds ();

   ResetPSInfo ();
   CleanUpShape ();
   CleanUpHelp ();
   CleanUpImport ();
   CleanUpImageProc ();
   CleanUpExec ();
   CleanUpCmds ();
   CleanUpExpr ();
   CleanUpNavigate ();
   CleanUpRemote ();

   CleanUpExtraWinInfo ();
   CleanUpDrawingWindow ();
   CleanUpPage ();
   CleanUpStk ();
   CleanUpStatus ();
   CleanUpChoices ();

   CleanUpScrolls ();
   CleanUpCursors ();

   CleanUpNames ();
   CleanUpRuler ();
   CleanUpRasters ();
   CleanUpFonts ();
   CleanUpMenu ();
   CleanUpText ();
   CleanUpColors ();
   CleanUpFiles ();
   CleanUpGrids ();
   CleanUpCutBuffer ();
   CleanUpXBm ();
   CleanUpXPm ();
   CleanUpMsg ();
   CleanUpShortCut ();

   if (iconWindowCreated)
   {
      XDestroyWindow (mainDisplay, iconBaseWindow);
      iconWindowCreated = FALSE;
   }
   CleanUpResiduals ();
   XDestroyWindow (mainDisplay, mainWindow);
#ifndef DONT_FREE_COLORMAP
   if (newColormapUsed) XFreeColormap (mainDisplay, mainColormap);
#endif
}

void ExposeEventHandler(input, recursive)
   XEvent *input;
   int recursive;
{
   register int i;
   XEvent ev;

   if (input->xany.window == choiceWindow) {
      while (XCheckWindowEvent(mainDisplay, choiceWindow, ExposureMask, &ev)) ;
      RedrawChoiceWindow();
   } else if (input->xany.window == drawWindow) {
      while (XCheckWindowEvent(mainDisplay, drawWindow, ExposureMask, &ev)) ;
      if (topSel != NULL || curChoice==VERTEXMODE || SomethingDirty() ||
            showCrossHair) {
         ClearAndRedrawDrawWindow();
      } else {
         RedrawDrawWindow(botObj);
         RedrawCurText();
      }
   } else if (input->xany.window == vRuleWindow) {
      while (XCheckWindowEvent(mainDisplay, vRuleWindow, ExposureMask, &ev)) ;
      RedrawVRuler();
   } else if (input->xany.window == hRuleWindow) {
      while (XCheckWindowEvent(mainDisplay, hRuleWindow, ExposureMask, &ev)) ;
      RedrawHRuler();
   } else if (input->xany.window == iconWindow && input->type == Expose) {
      while (XCheckWindowEvent(mainDisplay, iconWindow, ExposureMask, &ev)) ;
      RedrawIconWindow();
   } else if (input->xany.window == titleWindow) {
      while (XCheckWindowEvent(mainDisplay, titleWindow, ExposureMask, &ev)) ;
      RedrawTitleWindow();
   } else if (input->xany.window == menubarWindow) {
      while (XCheckWindowEvent(mainDisplay,menubarWindow,ExposureMask,&ev)) ;
      RedrawMenubarWindow();
   } else if (input->xany.window == msgWindow) {
      while (XCheckWindowEvent(mainDisplay, msgWindow, ExposureMask, &ev)) ;
      RedrawMsg();
   } else if (input->xany.window == vSBarWindow) {
      while (XCheckWindowEvent(mainDisplay, vSBarWindow, ExposureMask, &ev)) ;
      RedrawVScrollWindow();
   } else if (input->xany.window == hSBarWindow) {
      while (XCheckWindowEvent(mainDisplay, hSBarWindow, ExposureMask, &ev)) ;
      RedrawHScrollWindow();
   } else if (input->xany.window == statusWindow) {
      while (XCheckWindowEvent(mainDisplay, statusWindow, ExposureMask, &ev)) ;
      RedrawStatusWindow();
   } else if (input->xany.window == dummyWindow1) {
      while (XCheckWindowEvent(mainDisplay, dummyWindow1, ExposureMask, &ev)) ;
      RedrawDummyWindow1();
   } else if (pinnedMainMenu && input->xany.window == mainMenuWindow) {
      while (XCheckWindowEvent(mainDisplay,mainMenuWindow,ExposureMask,&ev)) ;
      RedrawMainMenuWindow();
   } else if (input->xany.window == pageWindow) {
      RedrawPageWindow();
   } else if (input->xany.window == pageDummyWindow) {
      RedrawPageDummyWindow();
   } else if (input->xany.window == colorWindow) {
      RedrawColorWindow();
   } else if (input->xany.window == colorDummyWindow) {
      RedrawColorDummyWindow();
   } else if (input->xany.window==mainWindow && input->type==VisibilityNotify &&
         input->xvisibility.state==VisibilityUnobscured) {
      XEvent	tmp_ev;

      while (XCheckWindowEvent(mainDisplay, mainWindow, VisibilityChangeMask,
            &tmp_ev)) ;
      if (iconWindowShown) {
         UnIconify();
      } else {
         if (pinnedMainMenu) XMapRaised(mainDisplay, mainMenuWindow);
         for (i = 0; i < numExtraWins; i++) {
            if (extraWinInfo[i].window != None && extraWinInfo[i].mapped &&
                  extraWinInfo[i].raise) {
               XMapRaised(mainDisplay, extraWinInfo[i].window);
            }
         }
      }
   } else {
      for (i = 0; i < numExtraWins; i++) {
         if (input->xany.window == extraWinInfo[i].window &&
               extraWinInfo[i].window != None) {
            while (XCheckWindowEvent(mainDisplay, extraWinInfo[i].window,
                  ExposureMask, &ev)) ;
            (*(extraWinInfo[i].expose_handler))(input);
            break;
         }
      }
   }

   if (recursive && (XCheckMaskEvent(mainDisplay, ExposureMask, &ev) ||
         XCheckMaskEvent(mainDisplay, VisibilityChangeMask, &ev))) {
      ExposeEventHandler(&ev, recursive);
   }
}

void MainLoop (Op, FileName, FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3)
   char	* Op, * FileName, * * FuncStr;
   char	* * Str1, * * Menu1, * * Str2, * * Menu2, * * Str3, * * Menu3;
{
   XEvent		input;
   char			* c_ptr, file_name[MAXPATHLENGTH+1], s[MAXPATHLENGTH+1];
   char			full_name[MAXPATHLENGTH+1], * rest;
   int			rc, len, i, read_status, short_name;
   struct ObjRec	* obj_ptr;
   FILE			* fp;
   int			tmp_linenum, obj_ext_len, need_to_check_auto_exec=FALSE;
   char			tmp_filename[MAXPATHLENGTH+1], obj_ext_str[MAXSTRING+1];

/* printf ("--> MainLoop (%s, %s, ...)\n", Op, FileName); */

   if (strcmp (Op, "init") == 0 || strcmp (Op, "justinit") == 0)
   {
      int	file_is_remote=FALSE, remote_buf_sz=0, is_html=FALSE;
      char	remote_fname[MAXPATHLENGTH+1], *tmp_remote_fname=NULL;
      char	* remote_buf=NULL, *page_spec=NULL;

      if (TGIF_PATCHLEVEL == 0)
         sprintf (initMsg1, "%s Version %s", TOOL_NAME, versionString);
      else
         sprintf (initMsg1, "%s Version %s (patchlevel %1d)", TOOL_NAME,
               versionString, TGIF_PATCHLEVEL);
      sprintf (initMsg2, "%s %s", copyrightString, "(william@cs.UCLA.edu)");
      TwoLineMsg (initMsg1, initMsg2);
      fprintf (stderr, "%s\n", initMsg1);
      fprintf (stderr, "%s\n", initMsg2);
      fflush(stderr);

      exitNormally = FALSE;

#ifdef USE_XAPPLRESDIR
#ifdef XAPPLOADDIR_DEF
      if (getenv ("XENVIRONMENT") == NULL)
      {
         FILE	* xenv_fp;
         char	tmp_buf[MAXPATHLENGTH+1];

         if (((c_ptr = getenv ("XAPPLRESDIR")) == NULL) ||
               ((len = strlen (c_ptr)) == 0))
         {
            strcpy (tmp_buf, XAPPLOADDIR_DEF);
            c_ptr = tmp_buf;;
         }
         len = strlen (c_ptr);
#else
      if ((getenv ("XENVIRONMENT") == NULL) &&
            ((c_ptr = getenv ("XAPPLRESDIR")) != NULL) &&
            ((len = strlen (c_ptr)) > 0))
      {
         FILE	* xenv_fp;
#endif
         xEnvStr = (char*)malloc(
               (strlen("XENVIRONMENT=")+strlen(TOOL_NAME)+len+3)*sizeof(char));
         if (xEnvStr == NULL) FailAllocMessage();
         sprintf (xEnvStr, "%s%s%s", c_ptr,
               ((c_ptr[len-1] == '/') ? "" : "/"), TOOL_NAME);
         if ((xenv_fp = fopen (xEnvStr, "r")) != NULL)
         {
            fclose (xenv_fp);
            sprintf (xEnvStr, "XENVIRONMENT=%s%s%s", c_ptr,
                  ((c_ptr[len-1] == '/') ? "" : "/"), TOOL_NAME);
            if (putenv (xEnvStr))
            {
               sprintf (s, "Warning:  Can not putenv(%s).", xEnvStr);
               Msg (s);
               *xEnvStr = '\0';
               free(xEnvStr);
               xEnvStr = NULL;
            }
         }
      }
      if ((mainDisplay = XOpenDisplay (displayName)) == 0)
#else
#ifdef USE_XT_INITIALIZE
      toplevel = XtInitialize (TOOL_NAME, TOOL_NAME, NULL, 0, &origArgC,
            origArgV);
      if (toplevel == NULL || (mainDisplay = XtDisplay (toplevel)) == 0)
#else
      if ((mainDisplay = XOpenDisplay (displayName)) == 0)
#endif
#endif
      {
         if (displayName == NULL)
         {
            fprintf (stderr, "Could not open the default display!  Abort!\n");
            exit (-1);
         }
         else
         {
            fprintf (stderr, "Could not open display '%s'!  Abort!\n",
                  displayName);
            exit (-1);
         }
      }
      mainScreen = DefaultScreen (mainDisplay);
      mainColormap = DefaultColormap (mainDisplay, mainScreen);
      mainDepth = DefaultDepth (mainDisplay, mainScreen);
      mainVisual = DefaultVisual (mainDisplay, mainScreen);
      rootWindow = RootWindow (mainDisplay, mainScreen);

      XSetErrorHandler (EmergencySaveForX);
      XSetIOErrorHandler (IOEmergencySaveForX);
      signal (SIGHUP, EmergencySave);
      signal (SIGFPE, EmergencySave);
#ifndef linux
      signal (SIGBUS, EmergencySave);
#endif
      signal (SIGSEGV, EmergencySave);

      Setup ();
      InitExtraWinInfo ();
      quitDraw = FALSE;

      *remote_fname = '\0';
      if (*FileName != '\0' && FileIsRemote (FileName))
      {
         if (!FormNewFileName (curDir, FileName, NULL, remote_fname,
               &page_spec))
         {
            sprintf(s, "Invalid remote file name '%s'.", FileName);
            MsgBox (s, TOOL_NAME, INFO_MB);
            *FileName = '\0';
         }
         else
         {
            char *content_type=NULL;

            SaveStatusStrings ();
            rc = LoadRemoteFileInMem(remote_fname, &remote_buf,
                  &content_type, &remote_buf_sz, &is_html, TRUE);
            RestoreStatusStrings ();
            if (rc && remote_buf != NULL &&
                  (tmp_remote_fname=WriteRemoteFileIntoTemp(remote_buf,
                  remote_buf_sz, NULL)) != NULL)
            {
               if (UseExternalViewer(is_html, remote_fname, content_type,
                     tmp_remote_fname) != FALSE) {
                  unlink(tmp_remote_fname);
                  if (remote_buf != NULL) FreeRemoteBuf(remote_buf);
                  if (tmp_remote_fname != NULL) {
                     FreeRemoteBuf(tmp_remote_fname);
                  }
                  remote_buf = tmp_remote_fname = NULL;
                  file_is_remote = FALSE;
                  *FileName = '\0';
               } else {
                  file_is_remote = TRUE;
               }
            }
            else
            {
               if (remote_buf != NULL) FreeRemoteBuf (remote_buf);
               remote_buf = NULL;
               *FileName = '\0';
            }
            if (content_type != NULL) FreeRemoteBuf(content_type);
         }
      }
      if (*FileName != '\0')
      {
         int obj_file=TRUE;

         sprintf (obj_ext_str, ".%s", OBJ_FILE_EXT);
         obj_ext_len = strlen (obj_ext_str);

         *file_name = '\0';
         if (file_is_remote)
         {
            if ((fp=fopen(tmp_remote_fname, "r")) == NULL)
            {
               sprintf (s, "Can not read tmp file '%s'.", tmp_remote_fname);
               MsgBox (s, TOOL_NAME, INFO_MB);

               unlink (tmp_remote_fname);
               FreeRemoteBuf (remote_buf);
               FreeRemoteBuf (tmp_remote_fname);
               remote_buf = tmp_remote_fname = NULL;
               file_is_remote = FALSE;
            }
            else
            {
               len = strlen (remote_fname);
               sprintf (obj_ext_str, ".%s", SYM_FILE_EXT);
               obj_ext_len = strlen (obj_ext_str);
               if (len >= obj_ext_len &&
                     strcmp (&FileName[len-obj_ext_len], obj_ext_str) == 0)
                  obj_file = FALSE;
               else
                  obj_file = TRUE;
            }
         }
         else
         {
            len = strlen (FileName);
            if (len >= obj_ext_len &&
                  strcmp (&FileName[len-obj_ext_len], obj_ext_str) == 0)
               strcpy (file_name, FileName);
            else
            {
               sprintf (obj_ext_str, ".%s", SYM_FILE_EXT);
               obj_ext_len = strlen (obj_ext_str);

               if (len >= obj_ext_len &&
                     strcmp (&FileName[len-obj_ext_len], obj_ext_str) == 0)
               {
                  strcpy (file_name, FileName);
                  obj_file = FALSE;
               }
               else
                  sprintf (file_name, "%s.%s", FileName, OBJ_FILE_EXT);
            }
            if ((short_name = IsPrefix (bootDir, file_name, &rest))) ++rest;
         }

         if (*file_name != '\0' && (fp = fopen (file_name, "r")) == NULL)
         {
            DelAllPages ();
            lastPageNum = 1;
            InitPage ();

            if (short_name)
               sprintf (s, "Can not open '%s'.", rest);
            else
               sprintf (s, "Can not open '%s'.", file_name);
            Msg (s);

            if (*file_name == '/')
               strcpy (full_name, file_name);
            else
               sprintf (full_name, "%s/%s", curDir, file_name);

            if (obj_file)
            {
               SetCurDir (full_name);
               *curSymDir = '\0';
            }
            else
               SetCurSymDir (full_name);
            curFileDefined = TRUE;

            SetFileModified (FALSE);
            if (short_name)
               sprintf (s, "Current (empty) file is '%s'.", rest);
            else
               sprintf (s, "Current (empty) file is '%s'.", file_name);
            Msg (s);
         }
         else if (file_is_remote || *file_name != '\0')
         {
            SetFileModified (FALSE);
            if (file_is_remote)
               sprintf (s, "Loading '%s' ...", remote_fname);
            else
            {
               if (short_name)
                  sprintf (s, "Loading '%s' ...", rest);
               else
                  sprintf (s, "Loading '%s' ...", file_name);
            }
            Msg (s);

            strcpy (tmp_filename, scanFileName);
            tmp_linenum = scanLineNum;
            if (file_is_remote)
               strcpy (scanFileName, tmp_remote_fname);
            else
               strcpy (scanFileName, (short_name ? rest : file_name));
            scanLineNum = 0;

            SetWatchCursor (drawWindow);
            SetWatchCursor (mainWindow);

            readingPageNum = 0;
            loadedCurPageNum = 0;
            foundGoodStateObject = FALSE;
            while ((read_status = ReadObj (fp, &obj_ptr)) == TRUE)
               if (obj_ptr != NULL)
               {
                  AdjForOldVersion (obj_ptr);
                  AddObj (NULL, topObj, obj_ptr);
               }

            strcpy (scanFileName, tmp_filename);
            scanLineNum = tmp_linenum;

            fclose (fp);

            if (loadedCurPageNum <= 0)
            {
               DelAllPages ();
               loadedCurPageNum = curPageNum = lastPageNum = 1;
               InitPage ();
            }
            if (read_status == INVALID)
            {
               sprintf (s, "File version too large (=%1d).  Load aborted!",
                     fileVersion);
               Msg (s);
               sprintf (s, "    You may need a more recent version of %s",
                     TOOL_NAME);
               Msg (s);
            }
            else
            {
               curFileDefined = TRUE;
               if (file_is_remote)
               {
                  SetCurDir (remote_fname);
                  if (!foundGoodStateObject) PasteString(remote_buf);
               }
               else
               {
                  if (*file_name == '/')
                     strcpy (full_name, file_name);
                  else
                     sprintf (full_name, "%s/%s", curDir, file_name);
                  if (obj_file)
                  {
                     SetCurDir (full_name);
                     *curSymDir = '\0';
                  }
                  else
                     SetCurSymDir (full_name);
               }
               if (file_is_remote)
                  sprintf (s, "Current file is '%s'.", remote_fname);
               else
               {
               if (short_name)
                  sprintf (s, "Current file is '%s'.", rest);
               else
                  sprintf (s, "Current file is '%s'.", file_name);
               }
               Msg (s);
            }
            GotoPageNum (loadedCurPageNum);
            SetDefaultCursor (mainWindow);
            SetDefaultCursor (drawWindow);

            if (file_is_remote)
            {
               unlink (tmp_remote_fname);
               if (remote_buf != NULL) FreeRemoteBuf (remote_buf);
               if (tmp_remote_fname != NULL) FreeRemoteBuf (tmp_remote_fname);
               remote_buf = tmp_remote_fname = NULL;
               CommitNavigate();
            }
            if (foundGoodStateObject) {
               struct AttrRec *exec_attr=FindFileAttrWithName("auto_exec=");

               CommitNavigate ();
               if (exec_attr != NULL) {
                  XSync(mainDisplay, False);
                  need_to_check_auto_exec = TRUE;
               }
            }
         }
         if (page_spec != NULL) {
            int new_page_num=(-1);

            if (!GetPageNumFromPageSpec(page_spec, &new_page_num)) {
               sprintf(s, "Invalid page specified for '%s'.", FileName);
               Msg(s);
            } else if (new_page_num != curPageNum) {
               GotoPageNum(new_page_num);
            }
         }
      }
      else
      {
         DelAllPages ();
         lastPageNum = 1;
         InitPage ();
      }
      SetDefaultDrawWinClipRecs ();
      UpdateDirInfo ();
      if (page_spec != NULL) free(page_spec);
   }
   SaveDrawWinInfo();

   if (strcmp (Op, "justinit") == 0)
   {
      XSync (mainDisplay, False);
      AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
      strcpy (*FuncStr, "");
      return;
   }

   if (strcmp (Op, "save") == 0)
   {
      SaveFile ();
      AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
      strcpy (*FuncStr, "");
      return;
   }

   if (strcmp (Op, "vi") == 0)
      ExecWithFile ("vi", FileName);

   if (strcmp (Op, "less") == 0)
      ExecWithFile ("less", FileName);

   if (strcmp (Op, "quit") == 0)
   {
      CleanUp ();
      quitDraw = TRUE;
      XFlush (mainDisplay);
      XSync (mainDisplay, True);
#ifdef USE_XT_INITIALIZE
      XtDestroyWidget (toplevel);
#else
      XCloseDisplay (mainDisplay);
#endif
      exitNormally = TRUE;
      AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
      strcpy (*FuncStr, "Quit");
      strcpy (*Str1, "");
      return;
   }

   if (strcmp (Op, "msg") == 0)
   {
      Msg (FileName);
      AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
      strcpy (*FuncStr, "");
      return;
   }

   if (strcmp (Op, "dialog") == 0)
   {
      Dialog (FileName, "", file_name);
      AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
      strcpy (*FuncStr, file_name);
      strcpy (*Str1, "");
      return;
   }

   if (strcmp (Op, "mainmenu") == 0 && quitDraw)
   {
      AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
      strcpy (*FuncStr, "Fail");
      strcpy (*Str1, "");
      return;
   }

   while (TRUE)
   {
      exitNormally = FALSE;
      if (need_to_check_auto_exec && XPending(mainDisplay) <= 0) {
         struct AttrRec *exec_attr=FindFileAttrWithName("auto_exec=");

         need_to_check_auto_exec = FALSE;
         if (exec_attr != NULL) {
            DoExecLoop(NULL, exec_attr);
         }
      }
#ifdef _TGIF_WB
      if (XPending(mainDisplay))
      {
         XNextEvent (mainDisplay, &input);
         CheckClientServer();
      }
      else
      {
         CheckClientServer();
         continue;
      }
#else /* ~_TGIF_WB */
      XNextEvent (mainDisplay, &input);
#endif /* _TGIF_WB */

      if (input.type == KeyRelease)
         continue;
      else if (input.type == KeyPress)
      {
         rc = ShortHand (&input);
         switch (rc)
         {
            case BAD: /* <CONTROL> or <META> */ continue;
            case INVALID: break;
            default:
               AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
               strcpy (*FuncStr, fileMenuStr[rc]);
               strcpy (*Str1, curDomainName);
               sprintf (*Menu1, "tmpmodel.%s", OBJ_FILE_EXT);
               strcpy (*Str2, "");
               for (c_ptr = *FuncStr; *c_ptr != '\0'; c_ptr++)
                  if (*c_ptr == ' ')
                  {
                     *c_ptr = '\0';
                     break;
                  }
               return;
         }
      }

      if (input.xany.window == choiceWindow)
      {
         if ((rc = ChoiceEventHandler (&input)) != INVALID)
         {
            AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
            strcpy (*FuncStr, fileMenuStr[rc]);
            strcpy (*Str1, curDomainName);
            sprintf (*Menu1, "tmpmodel.%s", OBJ_FILE_EXT);
            strcpy (*Str2, "");
            for (c_ptr = *FuncStr; *c_ptr != '\0'; c_ptr++)
               if (*c_ptr == ' ')
               {
                  *c_ptr = '\0';
                  break;
               }
            return;
         }
      }
      else if (input.xany.window == drawWindow)
      {
         if ((rc = DrawingEventHandler (&input)) != INVALID)
         {
            AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
            strcpy (*FuncStr, fileMenuStr[rc]);
            strcpy (*Str1, curDomainName);
            sprintf (*Menu1, "tmpmodel.%s", OBJ_FILE_EXT);
            strcpy (*Str2, "");
            for (c_ptr = *FuncStr; *c_ptr != '\0'; c_ptr++)
               if (*c_ptr == ' ')
               {
                  *c_ptr = '\0';
                  break;
               }
            return;
         }
      }
      else if (input.xany.window == mainWindow)
      {
         if ((rc = mainWinEventHandler (&input)) != INVALID)
         {
            AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
            strcpy (*FuncStr, fileMenuStr[rc]);
            strcpy (*Str1, curDomainName);
            sprintf (*Menu1, "tmpmodel.%s", OBJ_FILE_EXT);
            strcpy (*Str2, "");
            for (c_ptr = *FuncStr; *c_ptr != '\0'; c_ptr++)
               if (*c_ptr == ' ')
               {
                  *c_ptr = '\0';
                  break;
               }
            return;
         }
      }
      else if (input.xany.window == vRuleWindow ||
            input.xany.window == hRuleWindow)
         RulersEventHandler (&input);
      else if (input.xany.window == iconWindow ||
            input.xany.window == iconBaseWindow)
         IconEventHandler (&input);
      else if (input.xany.window == titleWindow)
         TitleEventHandler (&input);
      else if (menubarWindow != None && input.xany.window == menubarWindow)
      {
         if ((rc = MenubarEventHandler (&input)) != INVALID)
         {
            AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
            strcpy (*FuncStr, fileMenuStr[rc]);
            strcpy (*Str1, curDomainName);
            sprintf (*Menu1, "tmpmodel.%s", OBJ_FILE_EXT);
            strcpy (*Str2, "");
            for (c_ptr = *FuncStr; *c_ptr != '\0'; c_ptr++)
               if (*c_ptr == ' ')
               {
                  *c_ptr = '\0';
                  break;
               }
            return;
         }
      }
      else if (input.xany.window == msgWindow)
         MsgEventHandler (&input);
      else if (input.xany.window == vSBarWindow ||
            input.xany.window == hSBarWindow)
         ScrollEventHandler (&input);
      else if (statusWindow != None && input.xany.window == statusWindow)
         StatusEventHandler (&input);
      else if (input.xany.window == dummyWindow1 ||
            input.xany.window == dummyWindow2)
         DummiesEventHandler (&input);
      else if (input.xany.type == MappingNotify)
         XRefreshKeyboardMapping (&(input.xmapping));
      else if (pinnedMainMenu && input.xany.window == mainMenuWindow)
      {
         if ((rc = MainMenuEventHandler (&input)) != INVALID)
         {
            AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
            strcpy (*FuncStr, fileMenuStr[rc]);
            strcpy (*Str1, curDomainName);
            sprintf (*Menu1, "tmpmodel.%s", OBJ_FILE_EXT);
            strcpy (*Str2, "");
            for (c_ptr = *FuncStr; *c_ptr != '\0'; c_ptr++)
               if (*c_ptr == ' ')
               {
                  *c_ptr = '\0';
                  break;
               }
            return;
         }
      }
      else if (input.xany.window == pageWindow)
         PageEventHandler (&input);
      else if (input.xany.window == pageDummyWindow)
         PageDummyEventHandler (&input);
      else if (input.xany.window == colorWindow)
         ColorEventHandler (&input);
      else if (input.xany.window == colorDummyWindow)
         ColorDummyEventHandler (&input);
      else
      {
         for (i = 0; i < numExtraWins; i++)
            if (input.xany.window == extraWinInfo[i].window &&
                  extraWinInfo[i].window != None)
            {
               if ((rc = (*(extraWinInfo[i].ev_handler))(&input)) != INVALID)
               {
                  AllocStrings (FuncStr, Str1, Menu1, Str2, Menu2, Str3, Menu3);
                  strcpy (*FuncStr, fileMenuStr[rc]);
                  strcpy (*Str1, curDomainName);
                  sprintf (*Menu1, "tmpmodel.%s", OBJ_FILE_EXT);
                  strcpy (*Str2, "");
                  for (c_ptr = *FuncStr; *c_ptr != '\0'; c_ptr++)
                     if (*c_ptr == ' ')
                     {
                        *c_ptr = '\0';
                        break;
                     }
                  return;
               }
               break;
            }
      }
   }
}

static
void HandleSimpleEvent (input)
   XEvent	input;
{
   if ((input.type & (PointerMotionMask | EnterWindowMask | LeaveWindowMask))
         != 0) return;

   if (input.xany.window == drawWindow)
      DrawingEventHandler (&input);
   else if (input.xany.window == choiceWindow)
      ChoiceEventHandler (&input);
   else if (input.xany.window==iconWindow || input.xany.window==iconBaseWindow)
      IconEventHandler (&input);
   else if (input.xany.window == titleWindow)
      TitleEventHandler (&input);
   else if (input.xany.window == msgWindow)
      MsgEventHandler (&input);
   else if (input.xany.window==vSBarWindow || input.xany.window==hSBarWindow)
      ScrollEventHandler (&input);
   else if (input.xany.window == hRuleWindow)
      RedrawHRuler ();
   else if (input.xany.window == vRuleWindow)
      RedrawVRuler ();
   else if (pinnedMainMenu && input.xany.window == mainMenuWindow)
      MainMenuEventHandler (&input);
}

static
void AllocReturnStr (ReturnStr)
   char	* * ReturnStr;
{
   if((*ReturnStr=(char*)malloc(80*sizeof(char))) == NULL) FailAllocMessage();
}

void Animate (TypeStr, PolyId, SpeedStr, ColorStr, ReturnStr)
   char		* TypeStr, * PolyId, * SpeedStr, * ColorStr;
   char		* * ReturnStr;
{
   struct ObjRec	* obj_ptr;
   char			s[80];
   int			i, poly_id, speed, pixel, clicked = FALSE;
   XEvent		input;
   XButtonEvent		* button_event;

   AllocReturnStr (ReturnStr);
   strcpy (*ReturnStr, "");

   while (XPending (mainDisplay) != 0)
   {
      XPeekEvent (mainDisplay, &input);
      if ((input.type & (PointerMotionMask | ExposureMask | EnterWindowMask |
            LeaveWindowMask)) != 0)
      {
         XNextEvent (mainDisplay, &input);
         HandleSimpleEvent (input);
      }
      else
      {
         if (input.type == ButtonPress) clicked = TRUE;
         strcpy (*ReturnStr, "Interrupt");
         break;
      }
   }

   printf ("--> Animate (%s, %s, %s, %s)\n",TypeStr,PolyId,SpeedStr,ColorStr);
   if (strcmp (TypeStr, "waitclick") == 0)
   {
      Msg ("Left:step.  Middle:run.  Right:stop.");
      if (!clicked)
      {
         while(TRUE)
            if (XPending (mainDisplay) != 0)
            {
               XNextEvent (mainDisplay, &input);
               if (input.type == ButtonPress)
                  break;
            }
      }
      else
         XNextEvent (mainDisplay, &input);

      button_event = &(input.xbutton);
      switch (button_event->button)
      {
         case Button1 : strcpy (*ReturnStr, "Left"); break;
         case Button2 : strcpy (*ReturnStr, "Middle"); break;
         case Button3 : strcpy (*ReturnStr, "Right"); break;
      }
      Msg ("");
   }
   else
   {
      poly_id = atoi (PolyId);
      for (obj_ptr = topObj; obj_ptr != NULL; obj_ptr = obj_ptr->next)
         if (obj_ptr->type == OBJ_POLY && obj_ptr->id == poly_id)
            break;

      if (obj_ptr == NULL)
      {
         sprintf (s, "Can not find poly id = %1d.", poly_id);
         Msg (s);
      }
      else if (strcmp (TypeStr, "send") == 0)
      {
         speed = atoi (SpeedStr);
         pixel = (colorDisplay) ? xorColorPixels[obj_ptr->color] : 1;
         AnimateSend (obj_ptr->detail.p, speed, pixel);
      }
      else if (strcmp (TypeStr, "flash") == 0)
      {
         if (colorDisplay)
            strcpy (s, ColorStr);
         else
            strcpy (s, "white");
         for (i = 0; i < maxColors; i++)
            if (strcmp (colorMenuItems[i], s) == 0)
            {
               AnimateFlashColor (obj_ptr, i);
               break;
            }
      }
   }
}

static
void MyFormat (Str)
   char	* Str;
{
   register char	* c_ptr = Str, * period_ptr = NULL;
   register int		i;

   for ( ; *c_ptr != '\0'; c_ptr++)
      if (*c_ptr >= '0' && *c_ptr <= '9')
         continue;
      else if (*c_ptr == '.')
      {
         if (period_ptr != NULL) return;
         period_ptr = c_ptr;
         continue;
      }
      else
         return;

   if (period_ptr == NULL) return;
   for (c_ptr = period_ptr, i = 0; *c_ptr != '\0' && i < 7; c_ptr++, i++) ;
   if (*c_ptr != '\0') *c_ptr = '\0';
}

void UpdAttrVal (ObjId, AttrName, AttrColor, AttrVal, ReturnStr)
   char		* ObjId, * AttrName, * AttrColor, * AttrVal;
   char		* * ReturnStr;
{
   struct ObjRec	* obj_ptr;
   struct AttrRec	* attr_ptr;
   char			s[MAXSTRING+1];
   int			obj_id;
   XEvent		input;
   XButtonEvent		* button_event;

   AllocReturnStr (ReturnStr);
   strcpy (*ReturnStr, "");

   while (XPending (mainDisplay) != 0)
   {
      XPeekEvent (mainDisplay, &input);
      if ((input.type & (PointerMotionMask | ExposureMask | EnterWindowMask |
            LeaveWindowMask)) != 0)
      {
         XNextEvent (mainDisplay, &input);
         HandleSimpleEvent (input);
      }
      else
      {
         strcpy (*ReturnStr, "Interrupt");
         button_event = &(input.xbutton);
         switch (button_event->button)
         {
            case Button1 : strcpy (*ReturnStr, "Left"); break;
            case Button2 : strcpy (*ReturnStr, "Middle"); break;
            case Button3 : strcpy (*ReturnStr, "Right"); break;
         }
         break;
      }
   }

   printf ("--> UpdAttrVal (%s, %s, %s, %s)\n", ObjId, AttrName,
         AttrColor, AttrVal);

   obj_id = atoi (ObjId);

   for (obj_ptr = topObj; obj_ptr != NULL; obj_ptr = obj_ptr->next)
      if (obj_ptr->id == obj_id)
         break;

   if (obj_ptr == NULL)
   {
      sprintf (s, "Can not find obj id = %1d.", obj_id);
      Msg (s);
   }
   else
   {
      attr_ptr = obj_ptr->fattr;
      for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
      {
         if (strcmp (AttrName, attr_ptr->attr_name.s) == 0 &&
               strcmp (AttrColor, colorMenuItems[attr_ptr->obj->color]) == 0)
            break;
      }
      if (attr_ptr == NULL)
      {
         sprintf (s, "Can not find attr name '%s' and color '%s'.",
               AttrName, AttrColor);
         Msg (s);
      }
      else
      {
         DynStrSet (&attr_ptr->attr_value, AttrVal);
         MyFormat(attr_ptr->attr_value.s);
         if (attr_ptr->nameshown)
            sprintf (s, "%s%s", attr_ptr->attr_name.s, attr_ptr->attr_value.s);
         else
            strcpy (s, attr_ptr->attr_value.s);

         if (attr_ptr->shown) RepaintFirstStr (attr_ptr->obj, s);

         DynStrSet (&attr_ptr->obj->detail.t->first->dyn_str, s);
      }
   }
}
