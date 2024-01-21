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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/msg.c,v 3.0 1996/05/06 16:06:08 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"
#include "patchlvl.h"

#include "button.e"
#include "cutpaste.e"
#include "cursor.e"
#include "dialog.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "mainloop.e"
#include "menu.e"
#ifndef _NO_EXTERN
#include "msg.e"
#endif
#include "navigate.e"
#include "pattern.e"
#include "ps.e"
#include "raster.e"
#include "setup.e"
#include "util.e"
#include "version.e"
#include "xbitmap.e"

extern char *getenv ARGS_DECL((char *));

#define MSG_ROWS 2

char	scanFileName[MAXPATHLENGTH+1];
int	scanLineNum = 0;

char	progName[MAXPATHLENGTH+1];

struct MsgRec {
   char			* s;
   struct MsgRec	* next, * prev;
};

static struct MsgRec	* topMsg = NULL, * botMsg = NULL;
static struct MsgRec	* mostRecentTopMsgPtr = NULL;
static int		msgCount = 0;
static int		topMsgNumber = 0, mostRecentTopMsgNumber = INVALID;
static int		firstCharIndex = 0;

static int		reverseMouseStatusButtons=FALSE;

int FailAllocMessage()
{
   fprintf(stderr, "Out of virtual memory and can not malloc().\n");
   fflush(stderr);
   return FALSE;
}

static
void AddMsg (Msg)
   char	* Msg;
{
   char			* s;
   struct MsgRec	* msg_ptr;

   firstCharIndex = 0;
   if (*Msg == '\0') { topMsgNumber = msgCount; return; }

   s = (char*)malloc((strlen(Msg)+1)*sizeof(char));
   if (s == NULL) FailAllocMessage();
   msg_ptr = (struct MsgRec *)malloc(sizeof(struct MsgRec));
   if (msg_ptr == NULL) FailAllocMessage();

   strcpy (s, Msg);
   msg_ptr->s = s;

   ++msgCount;
   if (msgCount > topMsgNumber+MSG_ROWS) topMsgNumber = msgCount-MSG_ROWS;

   msg_ptr->prev = botMsg;
   msg_ptr->next = NULL;

   if (botMsg == NULL)
      topMsg = msg_ptr;
   else
      botMsg->next = msg_ptr;

   botMsg = msg_ptr;
}

void CleanUpMsg()
{
   register struct MsgRec *msg_ptr, *prev_msg;

   for (msg_ptr=botMsg; msg_ptr != NULL; msg_ptr=prev_msg) {
      prev_msg = msg_ptr->prev;
      free(msg_ptr->s);
      free(msg_ptr);
   }
   topMsg = botMsg = mostRecentTopMsgPtr = NULL;
   msgCount = topMsgNumber = firstCharIndex = 0;
   mostRecentTopMsgNumber = INVALID;
}

static
struct MsgRec * FindMsg (Number)
   int	Number;
{
   register int			i;
   register struct MsgRec	* ptr;

   if (Number >= msgCount)
      return (botMsg);
   else if (Number < 0)
      return (topMsg);
   else if (Number > (int)(msgCount/2))
      for (i = msgCount-1, ptr = botMsg; i != Number; i--, ptr = ptr->prev) ;
   else
      for (i = 0, ptr = topMsg; i != Number; i++, ptr = ptr->next) ;

   return (ptr);
}

void RedrawMsg ()
{
   int			i, x, y, len;
   XEvent       	ev;
   struct MsgRec	* msg_ptr;

   if (msgWindow == None) return;

   XClearWindow (mainDisplay, msgWindow);
   XSync (mainDisplay, False);
   while (XCheckWindowEvent (mainDisplay, msgWindow, ExposureMask, &ev)) ;

   if (topMsgNumber == msgCount) return;

   x = 2;
   y = 2 + defaultFontAsc;

   mostRecentTopMsgPtr = msg_ptr = (topMsgNumber == mostRecentTopMsgNumber) ?
         mostRecentTopMsgPtr : FindMsg (topMsgNumber);
   mostRecentTopMsgNumber = topMsgNumber;

   for (i = topMsgNumber; i < min(msgCount,topMsgNumber+MSG_ROWS); i++)
   {
      len = strlen (msg_ptr->s);
      if (len > firstCharIndex)
         XDrawString (mainDisplay, msgWindow, defaultGC, x, y,
               &msg_ptr->s[firstCharIndex], len-firstCharIndex);
      msg_ptr = msg_ptr->next;
      y += defaultFontHeight;
   }
   XSync (mainDisplay, False);
}

void Msg (Message)
   char	* Message;
{
   AddMsg (Message);
   RedrawMsg ();
}

void Warning (Where, Message)
   char	* Where, * Message;
{
   char	buf[MAXSTRING];

   sprintf (buf, "Warning in %s.\n\n%s.", Where, Message);
   MsgBox (buf, TOOL_NAME, INFO_MB);
}

void TwoLineMsg (Msg1, Msg2)
   char	* Msg1, * Msg2;
{
   AddMsg (Msg1);
   AddMsg (Msg2);
   RedrawMsg ();
}

void PrintMsgBuffer ()
{
   char			file_name[MAXPATHLENGTH], * rest, msg[MAXSTRING];
   FILE			*fp;
   struct MsgRec	* msg_ptr;

   Dialog ("Please enter a file name to write the message buffer content:",
         "( \"stdout\", \"stderr\", <ESC>: cancel )", file_name);
   if (*file_name == '\0') return;

   if (strcmp (file_name, "stdout") == 0)
   {
      for (msg_ptr = topMsg; msg_ptr != NULL; msg_ptr = msg_ptr->next)
         printf ("%s\n", msg_ptr->s);
   }
   else if (strcmp (file_name, "stderr") == 0)
   {
      for (msg_ptr = topMsg; msg_ptr != NULL; msg_ptr = msg_ptr->next)
         fprintf (stderr, "%s\n", msg_ptr->s);
   }
   else
   {
      int	short_name;

      if (!OkayToCreateFile (file_name)) return;

      if ((short_name = IsPrefix (bootDir, file_name, &rest))) ++rest;
      if ((fp = fopen (file_name, "w")) == NULL)
      {
         if (short_name)
            sprintf (msg, "Can not open '%s', message buffer not written out.",
                  rest);
         else
            sprintf (msg, "Can not open '%s', message buffer not written out.",
                  file_name);
         MsgBox (msg, TOOL_NAME, INFO_MB);
         return;
      }
      writeFileFailed = FALSE;
      for (msg_ptr = topMsg; msg_ptr != NULL; msg_ptr = msg_ptr->next)
         if (fprintf (fp, "%s\n", msg_ptr->s) == EOF)
            writeFileFailed = TRUE;
      fclose (fp);

      if (writeFileFailed)
      {
         writeFileFailed = FALSE;
         sprintf (msg, "Fail to write to '%s'.\n\nFile system may be full.",
               file_name);
         MsgBox (msg, TOOL_NAME, INFO_MB);
      }
      else
      {
         if (short_name)
            sprintf (msg, "Message buffer saved into '%s'.", rest);
         else
            sprintf (msg, "Message buffer saved into '%s'.", file_name);
         MsgBox (msg, TOOL_NAME, INFO_MB);
      }
   }
}

#include "xbm/btn1.xbm"

static int	oneLineStatus=FALSE;
static char	btnStatusStr[MAX_STATUS_BTNS][MAXSTRING+1];
static char	oneLineStatusStr[MAXSTRING+1];

static
void RedrawStatusStrings ()
{
   register int	i;

   if (PRTGIF || noStatusWindow) return;

   if (oneLineStatus) {
      int y;

      XClearWindow (mainDisplay, statusWindow);
      if (msgFontPtr == NULL) {
         if (defaultFontHeight+(brdrW<<1)+2 > btn1_height) {
            y = 2 + brdrW + defaultFontAsc;
         } else {
            y = ((statusWindowH-defaultFontHeight)>>1)+defaultFontAsc;
         }
         XDrawString(mainDisplay, statusWindow, defaultGC, 2, y,
               oneLineStatusStr, strlen (oneLineStatusStr));
      } else {
         if (msgFontHeight+(brdrW<<1)+2 > btn1_height) {
            y = 2 + brdrW + msgFontAsc;
         } else {
            y = ((statusWindowH-msgFontHeight)>>1)+msgFontAsc;
         }
         XSetFont(mainDisplay, defaultGC, msgFontPtr->fid);
         XDrawString(mainDisplay, statusWindow, defaultGC, 2, y,
               oneLineStatusStr, strlen(oneLineStatusStr));
         XSetFont(mainDisplay, defaultGC, defaultFontPtr->fid);
      }
   } else {
      int left=0, w=(int)(statusWindowW/3), right=0;

      if (msgFontPtr != NULL) {
         XSetFont(mainDisplay, defaultGC, msgFontPtr->fid);
      }
      for (i=0; i < MAX_STATUS_BTNS; i++) {
         right += w;
         if (right >= statusWindowW) right = statusWindowW-1;
         XClearWindow (mainDisplay, statusSubWindow[i]);
         if (msgFontPtr == NULL) {
            XDrawString (mainDisplay, statusSubWindow[i], defaultGC, 2,
                  1+defaultFontAsc, btnStatusStr[i], strlen(btnStatusStr[i]));
         } else {
            XDrawString (mainDisplay, statusSubWindow[i], defaultGC, 2,
                  1+msgFontAsc, btnStatusStr[i], strlen(btnStatusStr[i]));
         }
         left += w;
      }
      if (msgFontPtr != NULL) {
         XSetFont(mainDisplay, defaultGC, defaultFontPtr->fid);
      }
   }
}

void RedrawStatusWindow ()
{
   register int	i;
   XEvent	ev;

   if (PRTGIF || noStatusWindow) return;

   if (!oneLineStatus) {
      int left=0, w=(int)(statusWindowW/3), y, right=0;

      if (msgFontPtr == NULL) {
         if (defaultFontHeight+(brdrW<<1)+2 > btn1_height) {
            y = (statusWindowH-btn1_height)>>1;
         } else {
            y = 1;
         }
      } else {
         if (msgFontHeight+(brdrW<<1)+2 > btn1_height) {
            y = (statusWindowH-btn1_height)>>1;
         } else {
            y = 1;
         }
      }
      for (i=0; i < MAX_STATUS_BTNS; i++)
      {
         int	x=left+(brdrW<<2);

         right += w;
         if (right >= statusWindowW) right = statusWindowW-1;
         XSetTSOrigin (mainDisplay, rasterGC, x, y);
         XSetStipple (mainDisplay, rasterGC, statusBtnPixmap[i]);
         XFillRectangle (mainDisplay, statusWindow, rasterGC,
               x, y, btn1_width, btn1_height);
         XSetTSOrigin (mainDisplay, rasterGC, 0, 0);
         left += w;
      }
   }
   RedrawStatusStrings ();
   XSync (mainDisplay, False);
   while (XCheckWindowEvent (mainDisplay, statusWindow, ExposureMask, &ev)) ;
}

void SetMouseStatus (Left, Middle, Right)
   char	* Left, * Middle, * Right;
   /* If Left, Middle, and Right are all NULL, just refresh the status */
{
   register int	i;
   int		force_redraw=FALSE, left_index, right_index;

   if (reverseMouseStatusButtons)
   {
      left_index = 2;
      right_index = 0;
   }
   else
   {
      left_index = 0;
      right_index = 2;
   }

   if (PRTGIF || noStatusWindow) return;

   if (oneLineStatus)
   {
      oneLineStatus = FALSE;
      XClearWindow (mainDisplay, statusWindow);
      for (i=0; i < MAX_STATUS_BTNS; i++)
         XMapWindow (mainDisplay, statusSubWindow[i]);
      XSync (mainDisplay, False);
      force_redraw = TRUE;
   }
   else if (Left != NULL || Middle != NULL || Right != NULL)
   {
      if (((Left == NULL && *btnStatusStr[left_index] == '\0') ||
            (Left != NULL && strcmp (Left, btnStatusStr[left_index]) == 0)) &&
            ((Middle == NULL && *btnStatusStr[1] == '\0') ||
            (Middle != NULL && strcmp (Middle, btnStatusStr[1]) == 0)) &&
            ((Right == NULL && *btnStatusStr[right_index] == '\0') ||
            (Right != NULL && strcmp (Right, btnStatusStr[right_index]) == 0)))
         return;
   }
   if (Left != NULL || Middle != NULL || Right != NULL)
   {
      if (Left != NULL) {
         if (Left != btnStatusStr[left_index] &&
               strcmp(Left, btnStatusStr[left_index]) != 0) {
            UtilStrCpy (btnStatusStr[left_index], MAXSTRING+1, Left);
         }
      } else {
         *btnStatusStr[left_index] = '\0';
      }
      if (Middle != NULL) {
         if (Middle != btnStatusStr[1] &&
               strcmp(Middle, btnStatusStr[1]) != 0) {
            UtilStrCpy (btnStatusStr[1], MAXSTRING+1, Middle);
         }
      } else {
         *btnStatusStr[1] = '\0';
      }
      if (Right != NULL) {
         if (Right != btnStatusStr[right_index] &&
               strcmp(Right, btnStatusStr[right_index]) != 0) {
            UtilStrCpy (btnStatusStr[right_index], MAXSTRING+1, Right);
         }
      } else {
         *btnStatusStr[right_index] = '\0';
      }
   }
   if (force_redraw) {
      RedrawStatusWindow ();
   } else {
      RedrawStatusStrings ();
   }
}

void SetStringStatus (StatusStr)
   char	* StatusStr;
   /* If StatusStr is NULL, just refresh the status */
{
   register int	i;
   int		force_redraw=FALSE;

   if (PRTGIF || noStatusWindow) return;

   if (!oneLineStatus)
   {
      oneLineStatus = TRUE;
      for (i=0; i < MAX_STATUS_BTNS; i++)
         XUnmapWindow (mainDisplay, statusSubWindow[i]);
      XSync (mainDisplay, False);
      force_redraw = TRUE;
   }
   else if (StatusStr != NULL && strcmp (StatusStr, oneLineStatusStr) == 0)
      return;

   if (StatusStr != NULL) UtilStrCpy (oneLineStatusStr, MAXSTRING+1, StatusStr);
   if (force_redraw)
      RedrawStatusWindow ();
   else
      RedrawStatusStrings ();
}

typedef struct StatusInfoRec {
   char btn_str[MAX_STATUS_BTNS][MAXSTRING+1];
   char one_line_str[MAXSTRING+1];
   int one_line_status;
   struct StatusInfoRec *next;
} *StatusInfoPtr;

static struct StatusInfoRec *topStatusInfo=NULL;

void SaveStatusStrings ()
{
   struct StatusInfoRec *psi;
   int i;

   if (PRTGIF || noStatusWindow) return;

   psi = (struct StatusInfoRec *)malloc(sizeof(struct StatusInfoRec));
   if (psi == NULL) FailAllocMessage();
   psi->next = topStatusInfo;
   for (i=0; i < MAX_STATUS_BTNS; i++) {
      UtilStrCpy(psi->btn_str[i], MAXSTRING+1, btnStatusStr[i]);
   }
   UtilStrCpy(psi->one_line_str, MAXSTRING+1, oneLineStatusStr);
   psi->one_line_status = oneLineStatus;
   topStatusInfo = psi;
}

void RestoreStatusStrings ()
{
   struct StatusInfoRec *psi;
   int i;

   if (PRTGIF || noStatusWindow || topStatusInfo == NULL) return;

   for (i=0; i < MAX_STATUS_BTNS; i++) {
      UtilStrCpy(btnStatusStr[i], MAXSTRING+1, topStatusInfo->btn_str[i]);
   }
   UtilStrCpy(oneLineStatusStr, MAXSTRING+1, topStatusInfo->one_line_str);
   if (topStatusInfo->one_line_status != oneLineStatus) {
      if (topStatusInfo->one_line_status) {
         SetStringStatus(oneLineStatusStr);
      } else {
         SetMouseStatus(btnStatusStr[0], btnStatusStr[1], btnStatusStr[2]);
      }
   } else {
      RedrawStatusWindow();
   }
   psi = topStatusInfo->next;
   free(topStatusInfo);
   topStatusInfo = psi;
}

void SaveStatusStringsIntoBuf(ppsz_buf, pn_one_line)
   char ppsz_buf[MAX_STATUS_BTNS+1][MAXSTRING+1];
   int *pn_one_line;
   /* dimension of ppsz_buf must be [MAX_STATUS_BTNS+1][MAXSTRING+1] */
{
   register int i;

   if (PRTGIF || noStatusWindow) return;
   for (i=0; i < MAX_STATUS_BTNS; i++) {
      UtilStrCpy(ppsz_buf[i], MAXSTRING+1, btnStatusStr[i]);
   }
   UtilStrCpy(ppsz_buf[i], MAXSTRING+1, oneLineStatusStr);
   *pn_one_line = oneLineStatus;
}

void RestoreStatusStringsFromBuf(ppsz_buf, one_line)
   char ppsz_buf[MAX_STATUS_BTNS+1][MAXSTRING+1];
   int one_line;
   /* dimension of ppsz_buf must be [MAX_STATUS_BTNS+1][MAXSTRING+1] */
{
   register int i;

   if (PRTGIF || noStatusWindow) return;
   for (i=0; i < MAX_STATUS_BTNS; i++) {
      UtilStrCpy(btnStatusStr[i], MAXSTRING+1, ppsz_buf[i]);
   }
   UtilStrCpy(oneLineStatusStr, MAXSTRING+1, ppsz_buf[i]);
   if (one_line != oneLineStatus) {
      if (one_line) {
         SetStringStatus(oneLineStatusStr);
      } else {
         SetMouseStatus(btnStatusStr[0], btnStatusStr[1], btnStatusStr[2]);
      }
   } else {
      RedrawStatusWindow();
   }
}

void MsgEventHandler (input)
   XEvent	* input;
{
   XButtonEvent	* button_ev;
   double	frac;

   if (input->type == Expose)
      RedrawMsg ();
   else if (input->type == EnterNotify)
      SetMouseStatus ("scroll down 1 line", "scroll around",
            "scroll up 1 line");
   else if (input->type == ButtonPress)
   {
      button_ev = &(input->xbutton);
      if (button_ev->button == Button1)
      {
         if (button_ev->state & (ShiftMask | ControlMask))
         {
            firstCharIndex += 4;
            RedrawMsg ();
         }
         else
         {
            if (topMsgNumber+1 >= msgCount) return;

            topMsgNumber++;
            RedrawMsg ();
         }
      }
      else if (button_ev->button == Button2)
      {
         int	done=FALSE, saved_x=button_ev->x, saved_y=button_ev->y, x, y;
         int	saved_index=firstCharIndex;
         XEvent	ev;

         frac = ((double)saved_y) / ((double)msgWindowH);
         topMsgNumber = max(0,round(msgCount * frac));
         RedrawMsg ();

         XGrabPointer (mainDisplay, msgWindow, False,
            PointerMotionMask | ButtonReleaseMask, GrabModeAsync,
            GrabModeAsync, None, handCursor, CurrentTime);

         while (!done)
         {
            XNextEvent (mainDisplay, &ev);

            if (ev.type == Expose || ev.type == VisibilityNotify)
               ExposeEventHandler (&ev, TRUE);
            else if (ev.type == ButtonRelease)
            {
               XUngrabPointer (mainDisplay, CurrentTime);
               done = TRUE;
            }
            else if (ev.type == MotionNotify)
            {
               x = ev.xmotion.x;
               y = ev.xmotion.y;
               if (!((y<0 && topMsgNumber<=0) || (y>msgWindowH &&
                     topMsgNumber>=msgCount)) && y != saved_y)
               {
                  saved_y = y;
                  frac = ((double)saved_y) / ((double)msgWindowH);
                  topMsgNumber = max(0,round(msgCount * frac));
                  firstCharIndex = ((x-saved_x)>>3) + saved_index;
                  if (firstCharIndex < 0) firstCharIndex = 0;
                  RedrawMsg ();
               }
               while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
            }
         }
      }
      else if (button_ev->button == Button3)
      {
         if (button_ev->state & (ShiftMask | ControlMask))
         {
            if (firstCharIndex <= 0) return;

            firstCharIndex -= 4;
            if (firstCharIndex < 0) firstCharIndex = 0;
            RedrawMsg ();
         }
         else
         {
            if (topMsgNumber == 0) return;

            topMsgNumber--;
            RedrawMsg ();
         }
      }
   }
}

void StatusEventHandler (input)
   XEvent	* input;
{
   if (PRTGIF || noStatusWindow) return;

   if (input->type == Expose)
      RedrawStatusWindow ();
}

void InitStatus ()
{
   register int	i;
   char		* c_ptr;

   oneLineStatus = FALSE;
   *oneLineStatusStr = '\0';
   for (i=0; i < MAX_STATUS_BTNS; i++) *btnStatusStr[MAX_STATUS_BTNS] = '\0';

   reverseMouseStatusButtons = FALSE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "ReverseMouseStatusButtons")) != NULL &&
         (strcmp (c_ptr, "true") == 0 || strcmp (c_ptr, "True") == 0))
      reverseMouseStatusButtons = TRUE;
}

void CleanUpStatus()
{
   while (topStatusInfo != NULL) {
      struct StatusInfoRec *next_si=topStatusInfo->next;

      free(topStatusInfo);
      topStatusInfo = next_si;
   }
}

static char	* scanVal = NULL;
static char	* scanSep = NULL;

void InitScan (s, pat)
   char	* s, * pat;
{
   scanVal = s;
   scanSep = pat;
}

static
char * GetString ()
{
   char	* c_ptr;

   if (scanVal == NULL || scanSep == NULL) return (NULL);
   while (*scanVal!='\0' && strchr(scanSep,*scanVal)!=NULL) *scanVal++ = '\0';
   if (*scanVal == '\0') return (NULL);

   c_ptr=scanVal;
   for ( ; *scanVal!='\0' && strchr(scanSep,*scanVal)==NULL; scanVal++) ;
   if (*scanVal != '\0') *scanVal++ = '\0';

   return (c_ptr);
}

int ScanValue (fmt, v, item, stype)
   char	* fmt, * item, * stype;
   void	* v;
{
   char	* c_ptr, msg[MAXPATHLENGTH];

   if (scanVal == NULL) return (INVALID);
#ifdef DEBUG
   printf ("get %s for %s from %s:  ", item, stype, scanVal);
#endif

   if ((c_ptr = GetString()) == NULL)
   {

#ifdef DEBUG
      printf ("nothing\n");
#endif

      (void) sprintf (msg, "%s, %d:  Missing %s in %s",
            scanFileName, scanLineNum, item, stype);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      return (INVALID);
   }

   if (sscanf (c_ptr, fmt, v) != 1)
   {

#ifdef DEBUG
      printf ("error in %s\n", c_ptr);
#endif

      (void) sprintf (msg, "%s, %d:  Bad %s in %s [%s]",
            scanFileName, scanLineNum, item, stype, c_ptr);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      return (INVALID);
   }

#ifdef DEBUG
      printf ("got %d\n", (*(int *)v));
#endif

   return (0);
}

#define MAXEMERGENCYCOUNT 5

static int	emergencyCount = 0;

void EmergencySave (sig)
   int	sig;
{
   switch (sig)
   {
      case SIGHUP:
         fprintf(stderr, "SIGHUP signal received!  %s aborted!\n", TOOL_NAME);
         break;
      case SIGFPE:
         fprintf(stderr, "SIGFPE signal received!  %s aborted!\n", TOOL_NAME);
         break;
#ifndef linux
      case SIGBUS:
         fprintf(stderr, "SIGBUS signal received!  %s aborted!\n", TOOL_NAME);
         break;
#endif
      case SIGSEGV:
         fprintf(stderr, "SIGSEGV signal received!  %s aborted!\n", TOOL_NAME);
         break;
   }

   if (++emergencyCount > MAXEMERGENCYCOUNT)
   {
      fprintf (stderr, "Error count exceeds %1d in %s!  %s aborted!\n",
            MAXEMERGENCYCOUNT, "EmergencySave()", TOOL_NAME);
      exit (-1);
   }

   if (exitNormally) return;

   signal (SIGHUP, SIG_DFL);
   signal (SIGFPE, SIG_DFL);
#ifndef linux
   signal (SIGBUS, SIG_DFL);
#endif
   signal (SIGSEGV, SIG_DFL);

   if (fileModified)
   {
      switch (SaveTmpFile ("EmergencySave"))
      {
         case OBJ_FILE_SAVED:
            fprintf (stderr, "Saved to EmergencySave.%s.\n", OBJ_FILE_EXT);
            break;
         case SYM_FILE_SAVED:
            fprintf (stderr, "Saved to EmergencySave.%s.\n", SYM_FILE_EXT);
            break;
         case INVALID:
            fprintf (stderr, "Unable to save working file.\n"); break;
      }
   }
   exitNormally = TRUE;
   exit (0);
}

static
int EmergencySaveForXCont (s)
   char	* s;
{
   if (++emergencyCount > MAXEMERGENCYCOUNT)
   {
      fprintf (stderr, "Error count exceeds %1d in %s!  %s aborted!\n",
            MAXEMERGENCYCOUNT, s, TOOL_NAME);
      exit (-1);
   }

   if (exitNormally) return (0);

   if (copyingToCutBuffer)
   {
      copyingToCutBuffer = INVALID;
      return (0);
   }

   signal (SIGHUP, SIG_DFL);
   signal (SIGFPE, SIG_DFL);
#ifndef linux
   signal (SIGBUS, SIG_DFL);
#endif
   signal (SIGSEGV, SIG_DFL);

   if (fileModified)
   {
      switch (SaveTmpFile ("EmergencySave"))
      {
         case OBJ_FILE_SAVED:
            fprintf (stderr, "Saved to EmergencySave.%s.\n", OBJ_FILE_EXT);
            break;
         case SYM_FILE_SAVED:
            fprintf (stderr, "Saved to EmergencySave.%s.\n", SYM_FILE_EXT);
            break;
         case INVALID:
            fprintf (stderr, "Unable to save working file.\n");
            break;
      }
   }
   exitNormally = TRUE;
   return (-1);
}

int EmergencySaveForX (dsp, ev)
   Display	* dsp;
   XErrorEvent	* ev;
{
   char	msg[MAXSTRING+1];

   XGetErrorText (mainDisplay, (int)(ev->error_code), msg, MAXSTRING);
   fprintf (stderr, "X ERROR:  %s.\n", msg);

   return (EmergencySaveForXCont ("EmergencySaveForX()"));
}

int IOEmergencySaveForX (dsp)
   Display	* dsp;
{
   return (EmergencySaveForXCont ("IOEmergencySaveForX()"));
}

void Error (Where, Message)
   char	* Where, * Message;
{
   fprintf (stderr, "Error in %s.  %s.\n", Where, Message);
   fprintf (stderr, "%s aborted!\n", TOOL_NAME);
   if (fileModified) EmergencySave (0);
   exit (-1);
}

void Usage (tool_name)
   char	* tool_name;
{
   fprintf (stderr, "Usage:\t%s \\\n", tool_name);
   fprintf (stderr, "\t[-display displayname] \\\n");
   fprintf (stderr, "\t[-fg <color>] \\\n");
   fprintf (stderr, "\t[-bg <color>] \\\n");
   fprintf (stderr, "\t[-bd <color>] \\\n");
   fprintf (stderr, "\t[-rv] [-nv] \\\n");
   fprintf (stderr, "\t[-bw] \\\n");
   fprintf (stderr, "\t[-cwo] \\\n");
   fprintf (stderr, "\t[-hyper] \\\n");
   fprintf (stderr, "\t[-a4] \\\n");
   fprintf (stderr, "\t[-geometry <geom>] [=<geom>] \\\n");
   fprintf (stderr, "\t[<filename>[.obj]]\n");
   fprintf (stderr, "or:\n");
   fprintf (stderr, "\t%s -print \\\n", tool_name);
   fprintf (stderr, "\t[-eps] [-p] \\\n");
   fprintf (stderr, "\t[-ps] [-f] \\\n");
   fprintf (stderr, "\t[-text] \\\n");
   fprintf (stderr, "\t[-gray] \\\n");
   fprintf (stderr, "\t[-adobe | -adobe=<number>/<number] \\\n");
   fprintf (stderr, "\t[-page <number>] \\\n");
   fprintf (stderr, "\t[-print_cmd \"<command>\"] \\\n");
   fprintf (stderr, "\t[-one_file_per_page] \\\n");
   fprintf (stderr, "\t[-pepsc] \\\n");
   fprintf (stderr, "\t[-a4] \\\n");
   fprintf (stderr, "\t[-bop_hook \"<string>\"] \\\n");
   fprintf (stderr, "\t[-eop_hook \"<string>\"] \\\n");
   fprintf (stderr, "\t[-odir] \\\n");
   fprintf (stderr, "\t[<file1>[.obj] <file2>[.obj] ...]\n");
   exit (-1);
}

int ProcessPrTgifOptions(argc, argv, from_prtgif)
   int argc, from_prtgif;
   char *argv[];
{
   char	*c_ptr;

   cmdLineOneFilePerPage = FALSE;
   if (TGIF_PATCHLEVEL == 0) {
      fprintf(stderr, "%s Version %s\n", TOOL_NAME, versionString);
   } else {
      fprintf(stderr, "%s Version %s (patchlevel %1d)\n", TOOL_NAME,
            versionString, TGIF_PATCHLEVEL);
   }
   fprintf(stderr, "Copyright (C) 1990-1996, %s\n\n", "William Chia-Wei Cheng");
   fflush(stderr);

   if (!from_prtgif) {
      if ((c_ptr=getenv("TGIFPATH")) == NULL)
         strcpy(drawPath, TGIF_PATH);
      else
         if (((int)strlen(c_ptr)) >= MAXSTRING) {
            /* must be an error */
            strcpy(drawPath, TGIF_PATH);
         } else {
            strcpy(drawPath, c_ptr);
         }
#ifdef PRINT_CMD
      strcpy(printCommand, PRINT_CMD);
#else
#ifdef VMS
      strcpy(printCommand, "print");
#else
#ifdef SYSV
      strcpy(printCommand, "lp -dpostscript");
#else
      strcpy(printCommand, "lpr");
#endif /* SYSV */
#endif /* VMS */
#endif /* PRINT_CMD */
   }

   whereToPrint = PRINTER;
   usePsAdobeString = FALSE;
   *adobeString = '\0';
   *epsfString = '\0';

   c_ptr = printCommand;
   c_ptr += strlen(printCommand);
   for (argc--, argv++; argc > 0; argc--, argv++) {
      if (**argv == '-') {
         int len;

         if (strcmp(*argv, "-p") == 0 || strcmp(*argv, "-eps") == 0) {
            whereToPrint = LATEX_FIG;
         } else if (strcmp(*argv,"-f") == 0 || strcmp(*argv,"-ps") == 0) {
            whereToPrint = PS_FILE;
         } else if (strcmp(*argv, "-text") == 0) {
            whereToPrint = TEXT_FILE;
         } else if (strcmp(*argv, "-page") == 0) {
            argc--; argv++;
            if (argc <= 0) {
               fprintf(stderr, "Missing page number.\n");
               Usage(progName);
            }
            cmdLineHasPageNum = TRUE;
            strcpy(cmdLinePageNumStr, *argv);
            if (sscanf(cmdLinePageNumStr, "%d", &cmdLinePageNum) != 1 ||
                  cmdLinePageNum <= 0) {
               cmdLineHasPageNum = FALSE;
               fprintf(stderr, "Invalid page number.\n");
               Usage(progName);
            }
         } else if (strcmp(*argv, "-one_file_per_page") == 0) {
            cmdLineOneFilePerPage = TRUE;
         } else if (strcmp(*argv, "-pepsc") == 0) {
            stripEPSComments = FALSE;
         } else if (strcmp(*argv, "-a4") == 0) {
            cmdLineA4 = TRUE;
         } else if (strcmp(*argv, "-bop_hook") == 0) {
            argc--; argv++;
            if (argc <= 0) {
               fprintf(stderr, "Missing argument for -bop_hook.\n");
               Usage(progName);
            }
            SetBopHook(*argv);
         } else if (strcmp(*argv, "-eop_hook") == 0) {
            argc--; argv++;
            if (argc <= 0) {
               fprintf(stderr, "Missing argument for -bop_hooe.\n");
               Usage(progName);
            }
            SetEopHook(*argv);
         } else if (strcmp(*argv, "-gray") == 0) {
            useGray = TRUE;
         } else if (strcmp(*argv, "-bw") == 0) {
         } else if (strcmp(*argv, "-color") == 0) {
            fprintf(stderr, "Sorry, -color is not supported.\n");
            fflush(stderr);
            return (-1);
         } else if (strcmp(*argv, "-adobe") == 0) {
            usePsAdobeString = TRUE;
         } else if (strncmp(*argv, "-adobe=", 7) == 0) {
            char tmp_str[80];

            strcpy(tmp_str, &(*argv)[7]);
            if (!ParsePsAdobeString(tmp_str, &usePsAdobeString, adobeString,
                  epsfString))
            {
               fprintf(stderr, "%s %s*UsePsAdobeString:  '%s', %s.\n",
                     "Invalid", TOOL_NAME, tmp_str, "'false' is used");
               usePsAdobeString = FALSE;
            }
         } else if (strcmp(*argv, "-tgif_path") == 0) {
            argc--; argv++;
            if (argc <= 0) {
               fprintf(stderr, "Missing tgif_path.\n");
               Usage(progName);
            }
            len = strlen(*argv);
            if (**argv == '"' && (*argv)[len-1] == '"') {
               strcpy(drawPath, &(*argv)[1]);
               drawPath[strlen(drawPath)-1] = '\0';
            } else {
               strcpy(drawPath, *argv);
            }
            if (!from_prtgif) {
               fprintf(stderr, "Warning:  -tgif_path should not be used!\n");
            }
         } else if (strcmp(*argv, "-print_cmd") == 0) {
            argc--; argv++;
            if (argc <= 0) {
               fprintf(stderr, "Missing print_cmd.\n");
               Usage(progName);
            }
            len = strlen(*argv);
            if (**argv == '"' && (*argv)[len-1] == '"') {
               strcpy(printCommand, &(*argv)[1]);
               printCommand[strlen(printCommand)-1] = '\0';
            } else {
               strcpy(printCommand, *argv);
            }
         } else if (strncmp(*argv, "-o", 2) == 0) {
            strcpy (outputDir, &(*argv)[2]);
         } else if (strcmp(*argv, "-prtgif") != 0 &&
               strcmp(*argv, "-print") != 0 &&
               strcmp(*argv, "-PRINT") != 0) {
            strcat(c_ptr++, " ");
            strcat(c_ptr, *argv);
            c_ptr += strlen(*argv);
         }
      } else {
         break;
      }
   }
   if (cmdLineOneFilePerPage) {
      if (whereToPrint == PRINTER) {
         fprintf(stderr, "%s %s.\n",
               "Printing to printer is incompatible with",
               "-one_file_page_page");
         Usage(progName);
      }
   }
   return argc;
}

int ProcessTgifOptions(argc, argv, file_name)
   int argc;
   char *argv[], *file_name;
   /* All these strangeness with strings are related to */
   /*	Prolog's foreign function interface. */
{
   register int i;

   *file_name = '\0';
   for (i=1; i < argc; i++) {
      if (strcmp(argv[i], "-display") == 0) {
         if (++i == argc) {
            fprintf(stderr, "Missing display name.\n");
            Usage(progName);
         }
         displayName = argv[i];
      } else if (strcmp(argv[i], "-bw") == 0) {
         cmdLineBW = TRUE;
      } else if (strcmp(argv[i], "-rv") == 0) {
         cmdLineRV = TRUE;
      } else if (strcmp(argv[i], "-nv") == 0) {
         cmdLineRV = FALSE;
      } else if (strcmp(argv[i], "-cwo") == 0) {
         cmdLineCWO = TRUE;
      } else if (strcmp(argv[i], "-hyper") == 0) {
         inHyperSpace = TRUE;
      } else if (strcmp(argv[i], "-a4") == 0) {
         cmdLineA4 = TRUE;
      } else if (strcmp(argv[i], "-fg") == 0) {
         if (++i == argc) {
            fprintf(stderr, "Missing foreground color.\n");
            Usage(progName);
         }
         cmdLineForeground = (char*)malloc(80*sizeof(char));
         if (cmdLineForeground == NULL) FailAllocMessage();
         strcpy(cmdLineForeground, argv[i]);
      } else if (strcmp(argv[i], "-bg") == 0) {
         if (++i == argc)
         { fprintf(stderr, "Missing background color.\n"); Usage(progName); }
         cmdLineBackground = (char*)malloc(80*sizeof(char));
         if (cmdLineBackground == NULL) FailAllocMessage();
         strcpy(cmdLineBackground, argv[i]);
      } else if (strcmp(argv[i], "-bd") == 0) {
         if (++i == argc)
         { fprintf(stderr, "Missing border color.\n"); Usage(progName); }
         cmdLineBorder = (char*)malloc(80*sizeof(char));
         if (cmdLineBorder == NULL) FailAllocMessage();
         strcpy(cmdLineBorder, argv[i]);
      } else if (strcmp(argv[i], "-geometry") == 0) {
         if (++i == argc)
         { fprintf(stderr, "Missing geometry spec.\n"); Usage(progName); }
         strcat(geometrySpec, argv[i]);
         geometrySpecified = TRUE;
      } else if (*argv[i] == '=') {
         strcpy(geometrySpec, argv[i]);
         geometrySpecified = TRUE;
      } else if (*argv[i] != '-') {
         strcpy(file_name, argv[i]);
      } else {
         Usage(progName);
      }
   }
   return TRUE;
}
