/*
 * xboard.c -- X front end for XBoard
 * $Id: xboard.c,v 1.66 1997/01/02 20:50:46 mann Exp $
 *
 * Copyright 1991 by Digital Equipment Corporation, Maynard, Massachusetts.
 * Enhancements Copyright 1992-96 Free Software Foundation, Inc.
 *
 * The following terms apply to Digital Equipment Corporation's copyright
 * interest in XBoard:
 * ------------------------------------------------------------------------
 * All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * ------------------------------------------------------------------------
 *
 * The following terms apply to the enhanced version of XBoard distributed
 * by the Free Software Foundation:
 * ------------------------------------------------------------------------
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 * ------------------------------------------------------------------------
 *
 * See the file ChangeLog for a revision history.
 */

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

#if !OMIT_SOCKETS
# if HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <netdb.h>
# else /* not HAVE_SYS_SOCKET_H */
#  if HAVE_LAN_SOCKET_H
#   include <lan/socket.h>
#   include <lan/in.h>
#   include <lan/netdb.h>
#  else /* not HAVE_LAN_SOCKET_H */
#   define OMIT_SOCKETS 1
#  endif /* not HAVE_LAN_SOCKET_H */
# endif /* not HAVE_SYS_SOCKET_H */
#endif /* !OMIT_SOCKETS */

#if STDC_HEADERS
# include <stdlib.h>
# include <string.h>
#else /* not STDC_HEADERS */
extern char *getenv();
# if HAVE_STRING_H
#  include <string.h>
# else /* not HAVE_STRING_H */
#  include <strings.h>
# endif /* not HAVE_STRING_H */
#endif /* not STDC_HEADERS */

#if HAVE_SYS_FCNTL_H
# include <sys/fcntl.h>
#else /* not HAVE_SYS_FCNTL_H */
# if HAVE_FCNTL_H
#  include <fcntl.h>
# endif /* HAVE_FCNTL_H */
#endif /* not HAVE_SYS_FCNTL_H */

#if HAVE_SYS_SYSTEMINFO_H
# include <sys/systeminfo.h>
#endif /* HAVE_SYS_SYSTEMINFO_H */

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
# define HAVE_DIR_STRUCT
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
#  define HAVE_DIR_STRUCT
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
#  define HAVE_DIR_STRUCT
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
#  define HAVE_DIR_STRUCT
# endif
#endif

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiText.h>

#if HAVE_LIBXPM
#include <X11/xpm.h>
#endif

#include "common.h"
#include "frontend.h"
#include "backend.h"
#include "moves.h"
#include "xboard.h"
#include "childio.h"
#include "bitmaps/bitmaps.h"
#include "xgamelist.h"
#include "xedittags.h"

typedef struct {
    String string;
    XtActionProc proc;
} MenuItem;

typedef struct {
    String name;
    MenuItem *mi;
} Menu;

void main P((int argc, char **argv));
RETSIGTYPE CmailSigHandler P((int sig));
RETSIGTYPE IntSigHandler P((int sig));
void CreateGCs P((void));
void CreateXIMPieces P((void));
void CreateXPMPieces P((void));
void CreatePieces P((void));
void CreatePieceMenus P((void));
Widget CreateMenuBar P((Menu *mb));
Widget CreateButtonBar P ((MenuItem *mi));
char *FindFont P((char *pattern, int targetPxlSize));
void PieceMenuPopup P((Widget w, XEvent *event,
		       String *params, Cardinal *num_params));
static void PieceMenuSelect P((Widget w, ChessSquare piece, caddr_t junk));
static void DropMenuSelect P((Widget w, ChessSquare piece, caddr_t junk));
void ReadBitmap P((Pixmap *pm, String name, unsigned char bits[],
		   u_int wreq, u_int hreq));
void CreateGrid P((void));
int EventToSquare P((int x));
void DrawSquare P((int row, int column, ChessSquare piece, int do_flash));
void EventProc P((Widget widget, caddr_t unused, XEvent *event));
void HandleUserMove P((Widget w, XEvent *event,
		     String *prms, Cardinal *nprms));
void WhiteClock P((Widget w, XEvent *event,
		   String *prms, Cardinal *nprms));
void BlackClock P((Widget w, XEvent *event,
		   String *prms, Cardinal *nprms));
void DrawPositionProc P((Widget w, XEvent *event,
		     String *prms, Cardinal *nprms));
void XDrawPosition P((Widget w, /*Boolean*/int repaint, 
		     Board board));
void CommentPopUp P((char *title, char *label));
void CommentPopDown P((void));
void CommentCallback P((Widget w, XtPointer client_data,
			XtPointer call_data));
void ICSInputBoxPopUp P(( void ));
void ICSInputBoxPopDown P((void));
void FileNamePopUp P((char *label, char *def,
		      FileProc proc, char *openMode));
void FileNameCallback P((Widget w, XtPointer client_data,
			 XtPointer call_data));
void FileNameAction P((Widget w, XEvent *event,
		       String *prms, Cardinal *nprms));
void PromotionPopUp P((void));
void PromotionCallback P((Widget w, XtPointer client_data,
			  XtPointer call_data));
void EditCommentPopDown P((void));
void EditCommentCallback P((Widget w, XtPointer client_data,
			    XtPointer call_data));
void SelectCommand P((Widget w, XtPointer client_data, XtPointer call_data));
void ResetProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void LoadGameProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void LoadNextGameProc P((Widget w, XEvent *event, String *prms,
			 Cardinal *nprms));
void LoadPrevGameProc P((Widget w, XEvent *event, String *prms,
			 Cardinal *nprms));
void ReloadGameProc P((Widget w, XEvent *event, String *prms,
		       Cardinal *nprms));
void LoadPositionProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void SaveGameProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void SavePositionProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void MailMoveProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void ReloadCmailMsgProc P((Widget w, XEvent *event, String *prms,
			    Cardinal *nprms));
void QuitProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void PauseProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void MachineBlackProc P((Widget w, XEvent *event, String *prms,
			 Cardinal *nprms));
void MachineWhiteProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void AnalyzeModeProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void AnalyzeFileProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void TwoMachinesProc P((Widget w, XEvent *event, String *prms,
			Cardinal *nprms));
void IcsClientProc P((Widget w, XEvent *event, String *prms,
		      Cardinal *nprms));
void EditGameProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void EditPositionProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void EditCommentProc P((Widget w, XEvent *event,
			String *prms, Cardinal *nprms));
void IcsInputBoxProc P((Widget w, XEvent *event,
			String *prms, Cardinal *nprms));
void AcceptProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void DeclineProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void CallFlagProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void DrawProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void AbortProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void AdjournProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void ResignProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void EnterKeyProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void StopObservingProc P((Widget w, XEvent *event, String *prms,
			  Cardinal *nprms));
void StopExaminingProc P((Widget w, XEvent *event, String *prms,
			  Cardinal *nprms));
void BackwardProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void ForwardProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void ToStartProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void ToEndProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void RevertProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void TruncateGameProc P((Widget w, XEvent *event, String *prms,
			 Cardinal *nprms));
void RetractMoveProc P((Widget w, XEvent *event, String *prms,
			Cardinal *nprms));
void MoveNowProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void AlwaysQueenProc P((Widget w, XEvent *event, String *prms,
			Cardinal *nprms));
void AutocommProc P((Widget w, XEvent *event, String *prms,
		     Cardinal *nprms));
void AutoflagProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void AutobsProc P((Widget w, XEvent *event, String *prms,
			Cardinal *nprms));
void AutosaveProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void BellProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void FlipViewProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void CheckLegalityProc P((Widget w, XEvent *event, String *prms,
			  Cardinal *nprms));
void GetMoveListProc P((Widget w, XEvent *event, String *prms,
			Cardinal *nprms));
void OldSaveStyleProc P((Widget w, XEvent *event, String *prms,
			 Cardinal *nprms));
void QuietPlayProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void ShowCoordsProc P((Widget w, XEvent *event, String *prms,
		       Cardinal *nprms));
void ShowThinkingProc P((Widget w, XEvent *event, String *prms,
			 Cardinal *nprms));
void PeriodicUpdatesProc P((Widget w, XEvent *event, String *prms,
			 Cardinal *nprms));
void InfoProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void ManProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void HintProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void BookProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void AboutGameProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void AboutProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void DebugProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void NothingProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void Iconify P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void DisplayMove P((int moveNumber));
void DisplayTitle P((char *title));
void Usage P((void));
void ICSInitScript P((void));
int LoadGamePopUp P((FILE *f, int gameNumber, char *title));
void ErrorPopUp P((char *title, char *text));
void ErrorPopDown P((void));
static char *ExpandPathName P((char *path));

/*
* XBoard depends on Xt R4 or higher
*/
int xtVersion = XtSpecificationRelease;

int xScreen;
Display *xDisplay;
Window xBoardWindow;
Pixel lightSquareColor, darkSquareColor, whitePieceColor, blackPieceColor;
GC lightSquareGC, darkSquareGC, lineGC, wdPieceGC, wlPieceGC,
  bdPieceGC, blPieceGC, wbPieceGC, bwPieceGC, coordGC, highlineGC;
Pixmap iconPixmap, wIconPixmap, bIconPixmap, xMarkPixmap;
Widget shellWidget, layoutWidget, formWidget, boardWidget, messageWidget, 
  whiteTimerWidget, blackTimerWidget, titleWidget, widgetList[16], 
  commentShell, promotionShell, whitePieceMenu, blackPieceMenu, dropMenu,
  menuBarWidget, buttonBarWidget, editShell, errorShell, analysisShell,
  ICSInputShell;
XSegment gridSegments[(BOARD_SIZE + 1) * 2];
Font clockFontID, coordFontID;
XFontStruct *clockFontStruct, *coordFontStruct;
XtAppContext appContext;
char *layoutName;

FileProc fileProc;
char *fileOpenMode;

Position commentX = -1, commentY = -1;
Dimension commentW, commentH;

int squareSize, smallLayout = 0, tinyLayout = 0,
  fromX = -1, fromY = -1, toX, toY, commentUp = False, analysisUp = False,
  ICSInputBoxUp = False,
  filenameUp = False, promotionUp = False, pmFromX = -1, pmFromY = -1,
  editUp = False, errorUp = False, errorExitStatus = -1, lineGap;
Pixel timerForegroundPixel, timerBackgroundPixel;
Pixel buttonForegroundPixel, buttonBackgroundPixel;
char *chessDir, *programName, *programVersion;

#define SOLID 0
#define OUTLINE 1
typedef struct {
    int squareSize;
    unsigned char *bits[2][6];
} BuiltInBits;
BuiltInBits builtInBits[] = BUILT_IN_BITS;
Pixmap pieceBitmap[2][6];
Pixmap xpmPieceBitmap[4][6];	/* LL, LD, DL, DD */
Pixmap xpmLightSquare, xpmDarkSquare;
int useXPMs, useXPMsqs;
XImage *ximPieceBitmap[4][6];	/* LL, LD, DL, DD */
XImage *ximLightSquare, *ximDarkSquare;
int useXIMs, useXIMsqs;
XImage *xim_Cross;

#define pieceToSolid(piece) &pieceBitmap[SOLID][((int)(piece)) % 6]
#define pieceToOutline(piece) &pieceBitmap[OUTLINE][((int)(piece)) % 6]

SizeDefaults sizeDefaults[] = SIZE_DEFAULTS;

MenuItem fileMenu[] = {
    {"Reset Game", ResetProc},
    {"----", NothingProc},
    {"Load Game", LoadGameProc},
    {"Load Next Game", LoadNextGameProc},
    {"Load Previous Game", LoadPrevGameProc},
    {"Reload Same Game", ReloadGameProc},
    {"Load Position", LoadPositionProc},
    {"----", NothingProc},
    {"Save Game", SaveGameProc},
    {"Save Position", SavePositionProc},
    {"----", NothingProc},
    {"Mail Move", MailMoveProc},
    {"Reload CMail Message", ReloadCmailMsgProc},
    {"----", NothingProc},
    {"Exit", QuitProc},
    {NULL, NULL}
};

MenuItem modeMenu[] = {
    {"Machine White", MachineWhiteProc},
    {"Machine Black", MachineBlackProc},
    {"Analysis Mode", AnalyzeModeProc},
    {"Analyze File", AnalyzeFileProc },
    {"Two Machines", TwoMachinesProc},
    {"ICS Client", IcsClientProc},
    {"Edit Game", EditGameProc},
    {"Edit Position", EditPositionProc},
    {"----", NothingProc},
    {"Show Game List", ShowGameListProc},
    {"Edit Tags", EditTagsProc},
    {"Edit Comment", EditCommentProc},
    {"ICS Input Box", IcsInputBoxProc},
    {"Pause", PauseProc},
    {NULL, NULL}
};

MenuItem actionMenu[] = {
    {"Accept", AcceptProc},
    {"Decline", DeclineProc},
    {"----", NothingProc},    
    {"Call Flag", CallFlagProc},
    {"Draw", DrawProc},
    {"Adjourn", AdjournProc},
    {"Abort", AbortProc},
    {"Resign", ResignProc},
    {"----", NothingProc},    
    {"Stop Observing", StopObservingProc},
    {"Stop Examining", StopExaminingProc},
    {NULL, NULL}
};

MenuItem stepMenu[] = {
    {"Backward", BackwardProc},
    {"Forward", ForwardProc},
    {"Back to Start", ToStartProc},
    {"Forward to End", ToEndProc},
    {"Revert", RevertProc},
    {"Truncate Game", TruncateGameProc},
    {"----", NothingProc},    
    {"Move Now", MoveNowProc},
    {"Retract Move", RetractMoveProc},
    {NULL, NULL}
};    

MenuItem optionsMenu[] = {
    {"Always Queen", AlwaysQueenProc},
    {"Auto Comment", AutocommProc},
    {"Auto Flag", AutoflagProc},
    {"Auto Observe", AutobsProc},
    {"Auto Save", AutosaveProc},
    {"Bell", BellProc},
    {"Check Legality", CheckLegalityProc},
    {"Flip View", FlipViewProc},
    {"Get Move List", GetMoveListProc},
    {"Old Save Style", OldSaveStyleProc},
    {"Periodic Updates", PeriodicUpdatesProc},	
    {"Quiet Play", QuietPlayProc},
    {"Show Coords", ShowCoordsProc},
    {"Show Thinking", ShowThinkingProc},
    {NULL, NULL}
};

MenuItem helpMenu[] = {
    {"Info XBoard", InfoProc},
    {"Man XBoard", ManProc},
    {"----", NothingProc},
    {"Hint", HintProc},
    {"Book", BookProc},
    {"----", NothingProc},
    {"About XBoard", AboutProc},
    {NULL, NULL}
};

Menu menuBar[] = {
    {"File", fileMenu},
    {"Mode", modeMenu},
    {"Action", actionMenu},
    {"Step", stepMenu},
    {"Options", optionsMenu},
    {"Help", helpMenu},
    {NULL, NULL}
};

#define PAUSE_BUTTON "P"
MenuItem buttonBar[] = {
    {"<<", ToStartProc},
    {"<", BackwardProc},
    {PAUSE_BUTTON, PauseProc},
    {">", ForwardProc},
    {">>", ToEndProc},
    {NULL, NULL}
};

#define PIECE_MENU_SIZE 10
String pieceMenuStrings[PIECE_MENU_SIZE] = {
    "----", "Pawn", "Knight", "Bishop", "Rook", "Queen", "King",
    "----", "Empty square", "Clear board"
  };
/* must be in same order as PieceMenuStrings! */
ChessSquare pieceMenuTranslation[2][PIECE_MENU_SIZE] = {
    { (ChessSquare) 0, WhitePawn, WhiteKnight, WhiteBishop,
	WhiteRook, WhiteQueen, WhiteKing,
	(ChessSquare) 0, EmptySquare, ClearBoard },
    { (ChessSquare) 0, BlackPawn, BlackKnight, BlackBishop,
	BlackRook, BlackQueen, BlackKing,
	(ChessSquare) 0, EmptySquare, ClearBoard },
};

#define DROP_MENU_SIZE 6
String dropMenuStrings[DROP_MENU_SIZE] = {
    "----", "Pawn", "Knight", "Bishop", "Rook", "Queen"
  };
/* must be in same order as PieceMenuStrings! */
ChessSquare dropMenuTranslation[DROP_MENU_SIZE] = {
    (ChessSquare) 0, WhitePawn, WhiteKnight, WhiteBishop,
    WhiteRook, WhiteQueen
};

typedef struct {
    char piece;
    char* widget;
} DropMenuSensitivity;

DropMenuSensitivity dmSensitivity[] = {
    { 'P', "Pawn" },
    { 'N', "Knight" },
    { 'B', "Bishop" },
    { 'R', "Rook" },
    { 'Q', "Queen" }
};

Arg shellArgs[] = {
    { XtNwidth, 0 },
    { XtNheight, 0 },
    { XtNminWidth, 0 },
    { XtNminHeight, 0 },
    { XtNmaxWidth, 0 },
    { XtNmaxHeight, 0 }
};

Arg layoutArgs[] = {
    { XtNborderWidth, 0 },
    { XtNdefaultDistance, 0 },
};

Arg formArgs[] = {
    { XtNborderWidth, 0 },
};

Arg boardArgs[] = {
    { XtNborderWidth, 0 },
    { XtNwidth, 0 },
    { XtNheight, 0 }
};

Arg titleArgs[] = {
    { XtNjustify, (XtArgVal) XtJustifyRight },
    { XtNlabel, (XtArgVal) "..." },
    { XtNresizable, (XtArgVal) True },
    { XtNresize, (XtArgVal) False }
};

Arg messageArgs[] = {
    { XtNjustify, (XtArgVal) XtJustifyLeft },
    { XtNlabel, (XtArgVal) "..." },
    { XtNresizable, (XtArgVal) True },
    { XtNresize, (XtArgVal) False }
};

Arg timerArgs[] = {
    { XtNborderWidth, 0 },
    { XtNjustify, (XtArgVal) XtJustifyLeft }
};

XtResource clientResources[] = {
    { "whitePieceColor", "whitePieceColor", XtRString, sizeof(String),
	XtOffset(AppDataPtr, whitePieceColor), XtRString,
	WHITE_PIECE_COLOR },
    { "blackPieceColor", "blackPieceColor", XtRString, sizeof(String),
	XtOffset(AppDataPtr, blackPieceColor), XtRString,
	BLACK_PIECE_COLOR },
    { "lightSquareColor", "lightSquareColor", XtRString,
	sizeof(String), XtOffset(AppDataPtr, lightSquareColor),
	XtRString, LIGHT_SQUARE_COLOR }, 
    { "darkSquareColor", "darkSquareColor", XtRString, sizeof(String),
	XtOffset(AppDataPtr, darkSquareColor), XtRString,
	DARK_SQUARE_COLOR },
    { "movesPerSession", "movesPerSession", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, movesPerSession), XtRImmediate,
	(XtPointer) MOVES_PER_SESSION },
    { "timeIncrement", "timeIncrement", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, timeIncrement), XtRImmediate,
	(XtPointer) TIME_INCREMENT },
    { "initString", "initString", XtRString, sizeof(String),
	XtOffset(AppDataPtr, initString), XtRString, INIT_STRING },
    { "whiteString", "whiteString", XtRString, sizeof(String),
	XtOffset(AppDataPtr, whiteString), XtRString, WHITE_STRING },
    { "blackString", "blackString", XtRString, sizeof(String),
	XtOffset(AppDataPtr, blackString), XtRString, BLACK_STRING },
    { "firstChessProgram", "firstChessProgram", XtRString,
	sizeof(String), XtOffset(AppDataPtr, firstChessProgram),
	XtRString, FIRST_CHESS_PROGRAM },
    { "secondChessProgram", "secondChessProgram", XtRString,
	sizeof(String), XtOffset(AppDataPtr, secondChessProgram),
	XtRString, SECOND_CHESS_PROGRAM },
    { "noChessProgram", "noChessProgram", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, noChessProgram),
	XtRImmediate, (XtPointer) False },
    { "firstHost", "firstHost", XtRString, sizeof(String),
	XtOffset(AppDataPtr, firstHost), XtRString, FIRST_HOST },
    { "secondHost", "secondHost", XtRString, sizeof(String),
	XtOffset(AppDataPtr, secondHost), XtRString, SECOND_HOST },
    { "bitmapDirectory", "bitmapDirectory", XtRString,
	sizeof(String), XtOffset(AppDataPtr, bitmapDirectory),
	XtRString, "" },
    { "remoteShell", "remoteShell", XtRString, sizeof(String),
	XtOffset(AppDataPtr, remoteShell), XtRString, REMOTE_SHELL },
    { "remoteUser", "remoteUser", XtRString, sizeof(String),
	XtOffset(AppDataPtr, remoteUser), XtRString, "" },
    { "timeDelay", "timeDelay", XtRFloat, sizeof(float),
	XtOffset(AppDataPtr, timeDelay), XtRString,
	(XtPointer) TIME_DELAY_QUOTE },
    { "timeControl", "timeControl", XtRString, sizeof(String),
	XtOffset(AppDataPtr, timeControl), XtRString,
	(XtPointer) TIME_CONTROL },
    { "internetChessServerMode", "internetChessServerMode",
	XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, icsActive), XtRImmediate,
	(XtPointer) False },
    { "internetChessServerHost", "internetChessServerHost",
	XtRString, sizeof(String),
	XtOffset(AppDataPtr, icsHost),
	XtRString, (XtPointer) ICS_HOST },
    { "internetChessServerPort", "internetChessServerPort",
	XtRString, sizeof(String),
	XtOffset(AppDataPtr, icsPort), XtRString,
	(XtPointer) ICS_PORT },
    { "internetChessServerCommPort", "internetChessServerCommPort",
	XtRString, sizeof(String),
	XtOffset(AppDataPtr, icsCommPort), XtRString,
	ICS_COMM_PORT },
    { "internetChessServerLogonScript", "internetChessServerLogonScript",
	XtRString, sizeof(String),
	XtOffset(AppDataPtr, icsLogon), XtRString,
	ICS_LOGON },
    { "internetChessServerInputBox", "internetChessServerInputBox",
	XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, icsInputBox), XtRImmediate,
	(XtPointer) False },
    { "useTelnet", "useTelnet", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, useTelnet), XtRImmediate,
	(XtPointer) False },
    { "telnetProgram", "telnetProgram", XtRString, sizeof(String),
	XtOffset(AppDataPtr, telnetProgram), XtRString, TELNET_PROGRAM },
    { "gateway", "gateway", XtRString, sizeof(String),
	XtOffset(AppDataPtr, gateway), XtRString, "" },
    { "loadGameFile", "loadGameFile", XtRString, sizeof(String),
	XtOffset(AppDataPtr, loadGameFile), XtRString, "" },
    { "loadGameIndex", "loadGameIndex",
	XtRInt, sizeof(int),
	XtOffset(AppDataPtr, loadGameIndex), XtRImmediate,
	(XtPointer) 0 },
    { "saveGameFile", "saveGameFile", XtRString, sizeof(String),
	XtOffset(AppDataPtr, saveGameFile), XtRString, "" },
    { "autoSaveGames", "autoSaveGames", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, autoSaveGames),
	XtRImmediate, (XtPointer) False },
    { "loadPositionFile", "loadPositionFile", XtRString,
	sizeof(String), XtOffset(AppDataPtr, loadPositionFile),
	XtRString, "" },
    { "loadPositionIndex", "loadPositionIndex",
	XtRInt, sizeof(int),
	XtOffset(AppDataPtr, loadPositionIndex), XtRImmediate,
	(XtPointer) 1 },
    { "savePositionFile", "savePositionFile", XtRString,
	sizeof(String), XtOffset(AppDataPtr, savePositionFile),
	XtRString, "" },
    { "matchMode", "matchMode", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, matchMode), XtRImmediate, (XtPointer) False },
    { "monoMode", "monoMode", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, monoMode), XtRImmediate,
	(XtPointer) False },
    { "debugMode", "debugMode", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, debugMode), XtRImmediate,
	(XtPointer) False },
    { "clockMode", "clockMode", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, clockMode), XtRImmediate,
	(XtPointer) True },
    { "boardSize", "boardSize", XtRString, sizeof(String),
	XtOffset(AppDataPtr, boardSize), XtRString, "" },
    { "searchTime", "searchTime", XtRString, sizeof(String),
	XtOffset(AppDataPtr, searchTime), XtRString,
	(XtPointer) "" },
    { "searchDepth", "searchDepth", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, searchDepth), XtRImmediate, 
	(XtPointer) 0 },
    { "showCoords", "showCoords", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, showCoords), XtRImmediate,
	(XtPointer) False },
    { "showThinking", "showThinking", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, showThinking), XtRImmediate,
	(XtPointer) False },
    { "periodicUpdate", "periodicUpdate", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, periodicUpdates), XtRImmediate,
	(XtPointer) True },
    { "clockFont", "clockFont", XtRString, sizeof(String),
	XtOffset(AppDataPtr, clockFont), XtRString, CLOCK_FONT },
    { "coordFont", "coordFont", XtRString, sizeof(String),
	XtOffset(AppDataPtr, coordFont), XtRString, COORD_FONT },
    { "ringBellAfterMoves", "ringBellAfterMoves",
	XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, ringBellAfterMoves),
	XtRImmediate, (XtPointer) False	},
    { "autoCallFlag", "autoCallFlag", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, autoCallFlag),
	XtRImmediate, (XtPointer) False },
    { "autoObserve", "autoObserve", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, autoObserve),
	XtRImmediate, (XtPointer) False },
    { "autoComment", "autoComment", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, autoComment),
	XtRImmediate, (XtPointer) False },
    { "getMoveList", "getMoveList", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, getMoveList),
	XtRImmediate, (XtPointer) True },
    { "checkLegality", "checkLegality", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, checkLegality),
	XtRImmediate, (XtPointer) True },
    { "flipView", "flipView", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, flipView),
	XtRImmediate, (XtPointer) False },
    { "cmail", "cmailGameName", XtRString, sizeof(String),
	XtOffset(AppDataPtr, cmailGameName), XtRString, "" },
    { "alwaysPromoteToQueen", "alwaysPromoteToQueen", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, alwaysPromoteToQueen),
	XtRImmediate, (XtPointer) False },
    { "oldSaveStyle", "oldSaveStyle", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, oldSaveStyle),
	XtRImmediate, (XtPointer) False },
    { "quietPlay", "quietPlay", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, quietPlay),
	XtRImmediate, (XtPointer) False },
    { "borderXoffset", "borderXoffset", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, borderXoffset), XtRImmediate,
	(XtPointer) BORDER_X_OFFSET },
    { "borderYoffset", "borderYOffset", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, borderYoffset), XtRImmediate,
	(XtPointer) BORDER_Y_OFFSET },
    { "titleInWindow", "titleInWindow", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, titleInWindow),
	XtRImmediate, (XtPointer) False },
#ifdef ZIPPY
    { "zippyTalk", "zippyTalk", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, zippyTalk),
	XtRImmediate, (XtPointer) False },
    { "zippyPlay", "zippyPlay", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, zippyPlay),
	XtRImmediate, (XtPointer) False },
#endif
    { "flashCount", "flashCount", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, flashCount), XtRImmediate,
	(XtPointer) FLASH_COUNT  },
    { "flashRate", "flashRate", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, flashRate), XtRImmediate,
	(XtPointer) FLASH_RATE },
    { "clickClick", "clickClick", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, clickClick),
	XtRImmediate, (XtPointer) False },
    { "xpmDirectory", "xpmDirectory", XtRString,
	sizeof(String), XtOffset(AppDataPtr, xpmDirectory),
	XtRString, "" },
    { "msLoginDelay", "msLoginDelay", XtRInt, sizeof(int),
      XtOffset(AppDataPtr, msLoginDelay), XtRImmediate,
      (XtPointer) MS_LOGIN_DELAY },
    { "colorizeMessages", "colorizeMessages", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, colorize),
	XtRImmediate, (XtPointer) False },	
    { "colorShout", "colorShout", XtRString,
	sizeof(String), XtOffset(AppDataPtr, colorShout),
	XtRString, COLOR_SHOUT },
    { "colorSShout", "colorSShout", XtRString,
	sizeof(String), XtOffset(AppDataPtr, colorSShout),
	XtRString, COLOR_SSHOUT },
    { "colorChannel1", "colorChannel1", XtRString,
	sizeof(String), XtOffset(AppDataPtr, colorChannel1),
	XtRString, COLOR_CHANNEL1 },
    { "colorChannel", "colorChannel", XtRString,
	sizeof(String), XtOffset(AppDataPtr, colorChannel),
	XtRString, COLOR_CHANNEL },
    { "colorKibitz", "colorKibitz", XtRString,
	sizeof(String), XtOffset(AppDataPtr, colorKibitz),
	XtRString, COLOR_KIBITZ },
    { "colorTell", "colorTell", XtRString,
	sizeof(String), XtOffset(AppDataPtr, colorTell),
	XtRString, COLOR_TELL },
    { "colorChallenge", "colorChallenge", XtRString,
	sizeof(String), XtOffset(AppDataPtr, colorChallenge),
	XtRString, COLOR_CHALLENGE },
    { "colorNormal", "colorNormal", XtRString,
	sizeof(String), XtOffset(AppDataPtr, colorNormal),
	XtRString, COLOR_NORMAL },	
};

XrmOptionDescRec shellOptions[] = {
    { "-whitePieceColor", "whitePieceColor", XrmoptionSepArg, NULL },
    { "-blackPieceColor", "blackPieceColor", XrmoptionSepArg, NULL },
    { "-lightSquareColor", "lightSquareColor", XrmoptionSepArg, NULL },
    { "-darkSquareColor", "darkSquareColor", XrmoptionSepArg, NULL },
    { "-movesPerSession", "movesPerSession", XrmoptionSepArg, NULL },
    { "-mps", "movesPerSession", XrmoptionSepArg, NULL },
    { "-timeIncrement", "timeIncrement", XrmoptionSepArg, NULL },
    { "-inc", "timeIncrement", XrmoptionSepArg, NULL },
    { "-initString", "initString", XrmoptionSepArg, NULL },
    { "-whiteString", "whiteString", XrmoptionSepArg, NULL },
    { "-blackString", "blackString", XrmoptionSepArg, NULL },
    { "-firstChessProgram", "firstChessProgram", XrmoptionSepArg, NULL },
    { "-fcp", "firstChessProgram", XrmoptionSepArg, NULL },
    { "-secondChessProgram", "secondChessProgram", XrmoptionSepArg, NULL },
    { "-scp", "secondChessProgram", XrmoptionSepArg, NULL },
    { "-noChessProgram", "noChessProgram", XrmoptionSepArg, NULL },
    { "-ncp", "noChessProgram", XrmoptionNoArg, "True" },
    { "-xncp", "noChessProgram", XrmoptionNoArg, "False" },
    { "-firstHost", "firstHost", XrmoptionSepArg, NULL },
    { "-fh", "firstHost", XrmoptionSepArg, NULL },
    { "-secondHost", "secondHost", XrmoptionSepArg, NULL },
    { "-sh", "secondHost", XrmoptionSepArg, NULL },
    { "-bitmapDirectory", "bitmapDirectory", XrmoptionSepArg, NULL },
    { "-bm", "bitmapDirectory", XrmoptionSepArg, NULL },
    { "-remoteShell", "remoteShell", XrmoptionSepArg, NULL },
    { "-rsh", "remoteShell", XrmoptionSepArg, NULL },
    { "-remoteUser", "remoteUser", XrmoptionSepArg, NULL },
    { "-ruser", "remoteUser", XrmoptionSepArg, NULL },
    { "-timeDelay", "timeDelay", XrmoptionSepArg, NULL },
    { "-td", "timeDelay", XrmoptionSepArg, NULL },
    { "-timeControl", "timeControl", XrmoptionSepArg, NULL },
    { "-tc", "timeControl", XrmoptionSepArg, NULL },
    { "-internetChessServerMode", "internetChessServerMode",
	XrmoptionSepArg, NULL },
    { "-ics", "internetChessServerMode", XrmoptionNoArg, "True" },
    { "-xics", "internetChessServerMode", XrmoptionNoArg, "False" },
    { "-internetChessServerHost", "internetChessServerHost",
	XrmoptionSepArg, NULL },
    { "-icshost", "internetChessServerHost", XrmoptionSepArg, NULL },
    { "-internetChessServerPort", "internetChessServerPort",
	XrmoptionSepArg, NULL },
    { "-icsport", "internetChessServerPort", XrmoptionSepArg, NULL },
    { "-internetChessServerCommPort", "internetChessServerCommPort",
	XrmoptionSepArg, NULL },
    { "-icscomm", "internetChessServerCommPort", XrmoptionSepArg, NULL },
    { "-internetChessServerLogonScript", "internetChessServerLogonScript",
	XrmoptionSepArg, NULL },
    { "-icslogon", "internetChessServerLogonScript", XrmoptionSepArg, NULL },
    { "-internetChessServerInputBox", "internetChessServerInputBox",
	XrmoptionSepArg, NULL },
    { "-icsinput", "internetChessServerInputBox", XrmoptionNoArg, "True" },
    { "-xicsinput", "internetChessServerInputBox", XrmoptionNoArg, "False" },
    { "-useTelnet", "useTelnet", XrmoptionSepArg, NULL },
    { "-telnet", "useTelnet", XrmoptionNoArg, "True" },
    { "-xtelnet", "useTelnet", XrmoptionNoArg, "False" },
    { "-telnetProgram", "telnetProgram", XrmoptionSepArg, NULL },
    { "-gateway", "gateway", XrmoptionSepArg, NULL },
    { "-loadGameFile", "loadGameFile", XrmoptionSepArg, NULL },
    { "-lgf", "loadGameFile", XrmoptionSepArg, NULL },
    { "-loadGameIndex", "loadGameIndex", XrmoptionSepArg, NULL },
    { "-lgi", "loadGameIndex", XrmoptionSepArg, NULL },
    { "-saveGameFile", "saveGameFile", XrmoptionSepArg, NULL },
    { "-sgf", "saveGameFile", XrmoptionSepArg, NULL },
    { "-autoSaveGames", "autoSaveGames", XrmoptionSepArg, NULL },
    { "-autosave", "autoSaveGames", XrmoptionNoArg, "True" },
    { "-xautosave", "autoSaveGames", XrmoptionNoArg, "False" },
    { "-loadPositionFile", "loadPositionFile", XrmoptionSepArg, NULL },
    { "-lpf", "loadPositionFile", XrmoptionSepArg, NULL },
    { "-loadPositionIndex", "loadPositionIndex", XrmoptionSepArg, NULL },
    { "-lpi", "loadPositionIndex", XrmoptionSepArg, NULL },
    { "-savePositionFile", "savePositionFile", XrmoptionSepArg, NULL },
    { "-spf", "savePositionFile", XrmoptionSepArg, NULL },
    { "-matchMode", "matchMode", XrmoptionSepArg, NULL },
    { "-mm", "matchMode", XrmoptionNoArg, "True" },
    { "-xmm", "matchMode", XrmoptionNoArg, "False" },
    { "-monoMode", "monoMode", XrmoptionSepArg, NULL },
    { "-mono", "monoMode", XrmoptionNoArg, "True" },
    { "-xmono", "monoMode", XrmoptionNoArg, "False" },
    { "-debugMode", "debugMode", XrmoptionSepArg, NULL },
    { "-debug", "debugMode", XrmoptionNoArg, "True" },
    { "-xdebug", "debugMode", XrmoptionNoArg, "False" },
    { "-clockMode", "clockMode", XrmoptionSepArg, NULL },
    { "-clock", "clockMode", XrmoptionNoArg, "True" },
    { "-xclock", "clockMode", XrmoptionNoArg, "False" },
    { "-boardSize", "boardSize", XrmoptionSepArg, NULL },
    { "-size", "boardSize", XrmoptionSepArg, NULL },
    { "-searchTime", "searchTime", XrmoptionSepArg, NULL },
    { "-st", "searchTime", XrmoptionSepArg, NULL },
    { "-searchDepth", "searchDepth", XrmoptionSepArg, NULL },
    { "-sd", "searchDepth", XrmoptionSepArg, NULL },
    { "-showCoords", "showCoords", XrmoptionSepArg, NULL },
    { "-coords", "showCoords", XrmoptionNoArg, "True" },
    { "-xcoords", "showCoords", XrmoptionNoArg, "False" },
    { "-showThinking", "showThinking", XrmoptionSepArg, NULL },
    { "-thinking", "showThinking", XrmoptionNoArg, "True" },
    { "-xthinking", "showThinking", XrmoptionNoArg, "False" },
    { "-clockFont", "clockFont", XrmoptionSepArg, NULL },
    { "-coordFont", "coordFont", XrmoptionSepArg, NULL },
    { "-ringBellAfterMoves", "ringBellAfterMoves", XrmoptionSepArg, NULL },
    { "-bell", "ringBellAfterMoves", XrmoptionNoArg, "True" },
    { "-xbell", "ringBellAfterMoves", XrmoptionNoArg, "False" },
    { "-autoCallFlag", "autoCallFlag", XrmoptionSepArg, NULL },
    { "-autoflag", "autoCallFlag", XrmoptionNoArg, "True" },
    { "-xautoflag", "autoCallFlag", XrmoptionNoArg, "False" },
    { "-autoObserve", "autoObserve", XrmoptionSepArg, NULL },
    { "-autobs", "autoObserve", XrmoptionNoArg, "True" },
    { "-xautobs", "autoObserve", XrmoptionNoArg, "False" },
    { "-autoComment", "autoComment", XrmoptionSepArg, NULL },
    { "-autocomm", "autoComment", XrmoptionNoArg, "True" },
    { "-xautocomm", "autoComment", XrmoptionNoArg, "False" },
    { "-getMoveList", "getMoveList", XrmoptionSepArg, NULL },
    { "-moves", "getMoveList", XrmoptionNoArg, "True" },
    { "-xmoves", "getMoveList", XrmoptionNoArg, "False" },
    { "-checkLegality", "checkLegality", XrmoptionSepArg, NULL },
    { "-legal", "checkLegality", XrmoptionNoArg, "True" },
    { "-xlegal", "checkLegality", XrmoptionNoArg, "False" },
    { "-flipView", "flipView", XrmoptionSepArg, NULL },
    { "-flip", "flipView", XrmoptionNoArg, "True" },
    { "-xflip", "flipView", XrmoptionNoArg, "False" },
    { "-cmail", "cmailGameName", XrmoptionSepArg, NULL },
    { "-alwaysPromoteToQueen", "alwaysPromoteToQueen",
	XrmoptionSepArg, NULL },
    { "-queen", "alwaysPromoteToQueen", XrmoptionNoArg, "True" },
    { "-xqueen", "alwaysPromoteToQueen", XrmoptionNoArg, "False" },
    { "-oldSaveStyle", "oldSaveStyle", XrmoptionSepArg, NULL },
    { "-oldsave", "oldSaveStyle", XrmoptionNoArg, "True" },
    { "-xoldsave", "oldSaveStyle", XrmoptionNoArg, "False" },
    { "-quietPlay", "quietPlay", XrmoptionSepArg, NULL },
    { "-quiet", "quietPlay", XrmoptionNoArg, "True" },
    { "-xquiet", "quietPlay", XrmoptionNoArg, "False" },
    { "-borderXoffset", "borderXoffset", XrmoptionSepArg, NULL },
    { "-borderYoffset", "borderYoffset", XrmoptionSepArg, NULL },
    { "-titleInWindow", "titleInWindow", XrmoptionSepArg, NULL },
    { "-title", "titleInWindow", XrmoptionNoArg, "True" },
    { "-xtitle", "titleInWindow", XrmoptionNoArg, "False" },
#ifdef ZIPPY
    { "-zippyTalk", "zippyTalk", XrmoptionSepArg, NULL },
    { "-zt", "zippyTalk", XrmoptionNoArg, "True" },
    { "-xzt", "zippyTalk", XrmoptionNoArg, "False" },
    { "-zippyPlay", "zippyPlay", XrmoptionSepArg, NULL },
    { "-zp", "zippyPlay", XrmoptionNoArg, "True" },
    { "-xzp", "zippyPlay", XrmoptionNoArg, "False" },
#endif
    { "-flashCount", "flashCount", XrmoptionSepArg, NULL },
    { "-flashRate", "flashRate", XrmoptionSepArg, NULL },
    { "-clickClick", "clickClick", XrmoptionSepArg, NULL },	
    { "-click", "clickClick", XrmoptionNoArg, "True" },	
    { "-xclick", "clickClick", XrmoptionNoArg, "False" },	
    { "-xpmDirectory", "xpmDirectory", XrmoptionSepArg, NULL },
    { "-msLoginDelay", "msLoginDelay", XrmoptionSepArg, NULL },
    { "-xpm", "xpmDirectory", XrmoptionSepArg, NULL },
    { "-nxpm", "xpmDirectory", XrmoptionNoArg, "" },
    { "-colorizeMessages", "colorizeMessages", XrmoptionSepArg, NULL },
    { "-colorize", "colorizeMessages", XrmoptionNoArg, "True" },
    { "-xcolorize", "colorizeMessages", XrmoptionNoArg, "False" },
    { "-colorShout", "colorShout", XrmoptionSepArg, NULL },
    { "-colorSShout", "colorSShout", XrmoptionSepArg, NULL },
    { "-colorChannel1", "colorChannel", XrmoptionSepArg, NULL },
    { "-colorChannel", "colorChannel", XrmoptionSepArg, NULL },
    { "-colorKibitz", "colorKibitz", XrmoptionSepArg, NULL },
    { "-colorTell", "colorTell", XrmoptionSepArg, NULL },
    { "-colorChallenge", "colorChallenge", XrmoptionSepArg, NULL },
    { "-colorNormal", "colorNormal", XrmoptionSepArg, NULL },
};


XtActionsRec boardActions[] = {
    { "DrawPosition", DrawPositionProc },
    { "HandleUserMove", HandleUserMove },
    { "FileNameAction", FileNameAction },
    { "PieceMenuPopup", PieceMenuPopup },
    { "WhiteClock", WhiteClock },
    { "BlackClock", BlackClock },
    { "Iconify", Iconify },
    { "ResetProc", ResetProc },
    { "LoadGameProc", LoadGameProc },
    { "LoadNextGameProc", LoadNextGameProc },
    { "LoadPrevGameProc", LoadPrevGameProc },
    { "LoadSelectedProc", LoadSelectedProc },
    { "ReloadGameProc", ReloadGameProc },
    { "LoadPositionProc", LoadPositionProc },
    { "SaveGameProc", SaveGameProc },
    { "SavePositionProc", SavePositionProc },
    { "MailMoveProc", MailMoveProc },
    { "ReloadCmailMsgProc", ReloadCmailMsgProc },
    { "QuitProc", QuitProc },
    { "MachineWhiteProc", MachineWhiteProc },
    { "MachineBlackProc", MachineBlackProc },
    { "AnalyzeModeProc", AnalyzeModeProc },
    { "AnalyzeFileProc", AnalyzeFileProc },
    { "TwoMachinesProc", TwoMachinesProc },
    { "IcsClientProc", IcsClientProc },
    { "EditGameProc", EditGameProc },
    { "EditPositionProc", EditPositionProc },
    { "ShowGameListProc", ShowGameListProc },
    { "EditTagsProc", EditCommentProc },
    { "EditCommentProc", EditCommentProc },
    { "IcsInputBoxProc", IcsInputBoxProc },
    { "PauseProc", PauseProc },
    { "AcceptProc", AcceptProc },
    { "DeclineProc", DeclineProc },
    { "CallFlagProc", CallFlagProc },
    { "DrawProc", DrawProc },
    { "AdjournProc", AdjournProc },
    { "AbortProc", AbortProc },
    { "ResignProc", ResignProc },
    { "EnterKeyProc", EnterKeyProc },
    { "StopObservingProc", StopObservingProc },
    { "StopExaminingProc", StopExaminingProc },
    { "BackwardProc", BackwardProc },
    { "ForwardProc", ForwardProc },
    { "ToStartProc", ToStartProc },
    { "ToEndProc", ToEndProc },
    { "RevertProc", RevertProc },
    { "TruncateGameProc", TruncateGameProc },
    { "MoveNowProc", MoveNowProc },
    { "RetractMoveProc", RetractMoveProc },
    { "AlwaysQueenProc", AlwaysQueenProc },
    { "AutoflagProc", AutoflagProc },
    { "AutobsProc", AutobsProc },
    { "AutosaveProc", AutosaveProc },
    { "BellProc", BellProc },
    { "CheckLegalityProc", CheckLegalityProc },
    { "FlipViewProc", FlipViewProc },
    { "GetMoveListProc", GetMoveListProc },
    { "OldSaveStyleProc", OldSaveStyleProc },
    { "PeriodicUpdatesProc", PeriodicUpdatesProc },	
    { "QuietPlayProc", QuietPlayProc },
    { "ShowCoordsProc", ShowCoordsProc },
    { "ShowThinkingProc", ShowThinkingProc },
    { "InfoProc", InfoProc },
    { "ManProc", ManProc },
    { "HintProc", HintProc },
    { "BookProc", BookProc },
    { "AboutGameProc", AboutGameProc },
    { "AboutProc", AboutProc },
    { "DebugProc", DebugProc },
    { "NothingProc", NothingProc },
};
     
char globalTranslations[] =
  ":<Key>R: ResignProc() \n \
   :<Key>r: ResetProc() \n \
   :<Key>g: LoadGameProc() \n \
   :<Key>N: LoadNextGameProc() \n \
   :<Key>P: LoadPrevGameProc() \n \
   :<Key>Q: QuitProc() \n \
   :<Key>F: ToEndProc() \n \
   :<Key>f: ForwardProc() \n \
   :<Key>B: ToStartProc() \n \
   :<Key>b: BackwardProc() \n \
   :<Key>p: PauseProc() \n \
   :<Key>d: DrawProc() \n \
   :<Key>t: CallFlagProc() \n \
   :<Key>i: Iconify() \n \
   :<Key>c: Iconify() \n \
   <KeyDown>Control_L: BackwardProc() \n \
   <KeyUp>Control_L: ForwardProc() \n \
   <KeyDown>Control_R: BackwardProc() \n \
   <KeyUp>Control_R: ForwardProc() \n";


char boardTranslations[] =
  "<Expose>: DrawPosition() \n \
   <Btn1Down>: HandleUserMove() \n \
   <Btn1Up>: HandleUserMove() \n \
   Shift<Btn2Down>: XawPositionSimpleMenu(menuB) XawPositionSimpleMenu(menuD) \
                 PieceMenuPopup(menuB) \n \
   None<Btn2Down>: XawPositionSimpleMenu(menuW) XawPositionSimpleMenu(menuD) \
                 PieceMenuPopup(menuW) \n \
   Shift<Btn3Down>: XawPositionSimpleMenu(menuW) XawPositionSimpleMenu(menuD) \
                 PieceMenuPopup(menuW) \n \
   None<Btn3Down>: XawPositionSimpleMenu(menuB) XawPositionSimpleMenu(menuD) \
                 PieceMenuPopup(menuB) \n \
   <Message>WM_PROTOCOLS: QuitProc() \n";
     
char whiteTranslations[] = "<BtnDown>: WhiteClock()\n";
char blackTranslations[] = "<BtnDown>: BlackClock()\n";
     
char ICSInputTranslations[] =
    "<Key>Return: EnterKeyProc() \n";

String xboardResources[] = {
    TINY_FONT,
    DEFAULT_FONT,
    "*Dialog*value.translations: #override \\n <Key>Return: FileNameAction()",
    NULL
  };
     

/* Max possible square size */
#define MAXSQSIZE 256

static int xpm_avail[MAXSQSIZE];

#ifdef HAVE_DIR_STRUCT

/* Extract piece size from filename */
static int xpm_getsize( name, len, ext )
     char *name;
     int len;
     char *ext;
{
    char *p, *d;
    char buf[10];
  
    if ( len < 4 )
      return 0;

    if ( (p=strchr( name, '.' )) == NULL ||
	StrCaseCmp( p+1, ext ) != 0 )
      return 0;
  
    p = name + 3;
    d = buf;

    while( *p && isdigit( *p ) )
      *(d++) = *(p++);

    *d = 0;
    return atoi( buf );
}

/* Setup xpm_avail */
static int xpm_getavail( dirname, ext )
     char *dirname;
     char *ext;
{
    DIR *dir;
    struct dirent *ent;
    int  i;

    for( i=0; i<MAXSQSIZE; ++i )
      xpm_avail[i] = 0;

    if ( appData.debugMode )
      printf("XPM dir:%s:ext:%s:\n", dirname, ext );
  
    dir = opendir( dirname );
    if ( !dir )
      {
	  printf("Error: Can't access XPM directory %s\n", dirname );
	  exit(1);
      }
  
    while( (ent=readdir(dir)) != NULL ) {
	i = xpm_getsize( ent->d_name, NAMLEN(ent), ext );
	if ( i > 0 && i < MAXSQSIZE )
	  xpm_avail[i] = 1;
    }

    closedir( dir );

    return 0;
}

void xpm_print_avail( ext )
     char *ext;
{
    int i;

    printf("Available `%s' sizes:\n", ext);
    for( i=1; i<MAXSQSIZE; ++i ) {
	if ( xpm_avail[i] )
	  printf("%d\n", i );
    }
}
	
/* Return XPM piecesize closest to size */
int xpm_closest_to( dirname, size, ext )
     char *dirname;
     int size;
     char *ext;
{
    int i;
    int sm_diff = MAXSQSIZE;
    int sm_index = 0;
    int diff;
  
    xpm_getavail( dirname, ext );

    if ( appData.debugMode )
      xpm_print_avail( ext );
  
    for( i=1; i<MAXSQSIZE; ++i ) {
	if ( xpm_avail[i] ) {
	    diff = size - i;
	    diff = (diff<0) ? -diff : diff;
	    if ( diff < sm_diff )	{
		sm_diff = diff;
		sm_index = i;
	    }
	}
    }

    if ( !sm_index )
      {
	  printf("Error: No `%s' files!\n", ext);
	  exit(1);
      }

    return sm_index;
}
#else	/* !HAVE_DIR_STRUCT */
/* If we are on a system without a DIR struct, we can't
   read the directory, so we can't collect a list of
   filenames, etc., so we can't do any size-fitting. */
int xpm_closest_to( dirname, size, ext )
     char *dirname;
     int size;
     char *ext;
{
    fprintf( stderr, "Warning: No DIR structure found on this system --\n");
    fprintf( stderr, "         Unable to autosize for XPM/XIM pieces.\n");
    fprintf( stderr, "         You will only be able to use sizes 21, 40, 64, and 80\n");
    fprintf( stderr, "   Please report this error to frankm@hiwaay.net.\n");
    fprintf( stderr, "   Include system type & operating system in message.\n");
    return size;
}
#endif /* HAVE_DIR_STRUCT */

void main(argc, argv)
     int argc;
     char **argv;
{
    int i, j, clockFontPxlSize, coordFontPxlSize;
    XSetWindowAttributes window_attributes;
    Arg args[16];
    Dimension timerWidth, boardWidth, w, h, sep, bor, wr, hr;
    XrmValue vFrom, vTo;
    XtGeometryResult gres;
    char *p;

    setbuf(stdout, NULL);
    setbuf(stderr, NULL);
    fromUserFP = stdin;
    toUserFP = stdout;
    debugFP = stderr;
    
    programName = strrchr(argv[0], '/');
    if (programName == NULL)
      programName = argv[0];
    else
      programName++;

    shellWidget =
      XtAppInitialize(&appContext, "XBoard", shellOptions,
		      XtNumber(shellOptions),
		      &argc, argv, xboardResources, NULL, 0);
    if (argc > 1)
      Usage();
    
    if ((chessDir = (char *) getenv("CHESSDIR")) == NULL) {
	chessDir = ".";
    } else {
	if (chdir(chessDir) != 0) {
	    fprintf(stderr, "%s: can't cd to CHESSDIR: ", programName);
	    perror(chessDir);
	    exit(1);
	}
    }
    
    XtGetApplicationResources(shellWidget, (XtPointer) &appData,
			      clientResources, XtNumber(clientResources),
			      NULL, 0);
    InitBackEnd1();


    if (appData.noChessProgram) {
	programVersion = (char*) malloc(5 + strlen(PRODUCT) + strlen(VERSION)
					+ strlen(PATCHLEVEL));
	sprintf(programVersion, "%s %s.%s", PRODUCT, VERSION, PATCHLEVEL);
    } else {
	char *p, *q;
	q = appData.firstChessProgram;
	while (*q != ' ' && *q != NULLCHAR) q++;
	p = q;
	while (p > appData.firstChessProgram && *(p-1) != '/') p--;
	programVersion = (char*) malloc(8 + strlen(PRODUCT) + strlen(VERSION)
					+ strlen(PATCHLEVEL) + (q - p));
	sprintf(programVersion, "%s %s.%s + ", PRODUCT, VERSION, PATCHLEVEL);
	strncat(programVersion, p, q - p);
    }
    xDisplay = XtDisplay(shellWidget);
    xScreen = DefaultScreen(xDisplay);

    /*
     * Determine boardSize
     */
    if (*appData.boardSize == NULLCHAR) {
	if (((DisplayWidth(xDisplay, xScreen) < 640) ||
	     (DisplayHeight(xDisplay, xScreen) < 640)))
	  appData.boardSize = "Small";
	else if (((DisplayWidth(xDisplay, xScreen) < 800) ||
		  (DisplayHeight(xDisplay, xScreen) < 800)))
	  appData.boardSize = "Medium";
	else
	  appData.boardSize = "Large";
    }
    
    if (isdigit(appData.boardSize[0])) {
        i = sscanf(appData.boardSize, "%d,%d,%d,%d,%d,%d", &squareSize,
		   &lineGap, &clockFontPxlSize, &coordFontPxlSize,
		   &smallLayout, &tinyLayout);
        if (i == 0) {
	    fprintf(stderr, "%s: bad boardSize option %s\n",
		    programName, appData.boardSize);
	    Usage();
	}
	if (i < 6) {
	    /* Find some defaults; use the nearest known size */
	    SizeDefaults *szd, *nearest;
	    int distance = 99999;
	    nearest = szd = sizeDefaults;
	    while (szd->name != NULL) {
		if (abs(szd->squareSize - squareSize) < distance) {
		    nearest = szd;
		    distance = abs(szd->squareSize - squareSize);
		    if (distance == 0) break;
		}
		szd++;
	    }
	    if (i < 2) lineGap = nearest->lineGap;
	    if (i < 3) clockFontPxlSize = nearest->clockFontPxlSize;
	    if (i < 4) coordFontPxlSize = nearest->coordFontPxlSize;
	    if (i < 5) smallLayout = nearest->smallLayout;
	    if (i < 6) tinyLayout = nearest->tinyLayout;
	}
    } else {
	SizeDefaults *szd = sizeDefaults;
	while (szd->name != NULL &&
	       StrCaseCmp(szd->name, appData.boardSize) != 0) szd++;
	if (szd->name == NULL) {
	    fprintf(stderr, "%s: bad boardSize option %s\n",
		    programName, appData.boardSize);
	    Usage();
	}
	squareSize = szd->squareSize;
	lineGap = szd->lineGap;
	clockFontPxlSize = szd->clockFontPxlSize;
	coordFontPxlSize = szd->coordFontPxlSize;
	smallLayout = szd->smallLayout;
	tinyLayout = szd->tinyLayout;
    }

#if HAVE_LIBXPM
	/* Now, using squareSize as a hint, find a good XPM set size */
	if ( strlen( appData.xpmDirectory ) > 0 )
	  {
	    p = ExpandPathName(appData.xpmDirectory);
	    if ( !p ) {
	      printf("Error expanding path name \"%s\"\n",
		     appData.xpmDirectory );
	      exit(1);
	    }

	    if( appData.debugMode ) {
		printf("XBoard square size (hint): %d\n", squareSize );
	      printf("XPM fulldir:%s:\n", p );
	    }
	    
	    squareSize = xpm_closest_to( p, squareSize, "xpm" );
	    if ( appData.debugMode )
	      printf("Closest XPM size: %d\n", squareSize );
	  }
#else /* !HAVE_LIBXPM */
	/* Now, using squareSize as a hint, find a good XIM set size */
	if ( strlen( appData.xpmDirectory ) > 0 )
	  {
	    p = ExpandPathName(appData.xpmDirectory);
	    if ( !p ) {
	      printf("Error expanding path name \"%s\"\n",
		     appData.xpmDirectory );
	      exit(1);
	    }

	    if( appData.debugMode ) {
	      printf("XBoard square size (hint): %d\n", squareSize );
	      printf("XIM fulldir:%s:\n", p );
	    }

	    squareSize = xpm_closest_to( p, squareSize, "xim" );

	    if ( appData.debugMode )
	      printf("Closest XIM size: %d\n", squareSize );
	  }
#endif /* HAVE_LIBXPM */		
		
    boardWidth = lineGap + BOARD_SIZE * (squareSize + lineGap);
    XtSetArg(boardArgs[1], XtNwidth, boardWidth);
    XtSetArg(boardArgs[2], XtNheight,
	     lineGap + BOARD_SIZE * (squareSize + lineGap));

    /*
     * Determine what fonts to use.
     */
    appData.clockFont = FindFont(appData.clockFont, clockFontPxlSize);
    clockFontID = XLoadFont(xDisplay, appData.clockFont);
    clockFontStruct = XQueryFont(xDisplay, clockFontID);
    appData.coordFont = FindFont(appData.coordFont, coordFontPxlSize);
    coordFontID = XLoadFont(xDisplay, appData.coordFont);
    coordFontStruct = XQueryFont(xDisplay, coordFontID);

    /*
     * Detect if there are not enough colors are available and adapt.
     */
    if (DefaultDepth(xDisplay, xScreen) <= 2)
      appData.monoMode = True;

    if (!appData.monoMode) {
	vFrom.addr = (caddr_t) appData.lightSquareColor;
	vFrom.size = strlen(appData.lightSquareColor);
	XtConvert(shellWidget, XtRString, &vFrom, XtRPixel, &vTo);
	if (vTo.addr == NULL)
	  appData.monoMode = True;
	else
	  lightSquareColor = *(Pixel *) vTo.addr;
    }
    if (!appData.monoMode) {
	vFrom.addr = (caddr_t) appData.darkSquareColor;
	vFrom.size = strlen(appData.darkSquareColor);
	XtConvert(shellWidget, XtRString, &vFrom, XtRPixel, &vTo);
	if (vTo.addr == NULL)
	  appData.monoMode = True;
	else
	  darkSquareColor = *(Pixel *) vTo.addr;
    }
    if (!appData.monoMode) {
	vFrom.addr = (caddr_t) appData.whitePieceColor;
	vFrom.size = strlen(appData.whitePieceColor);
	XtConvert(shellWidget, XtRString, &vFrom, XtRPixel, &vTo);
	if (vTo.addr == NULL)
	  appData.monoMode = True;
	else
	  whitePieceColor = *(Pixel *) vTo.addr;
    }
    if (!appData.monoMode) {
	vFrom.addr = (caddr_t) appData.blackPieceColor;
	vFrom.size = strlen(appData.blackPieceColor);
	XtConvert(shellWidget, XtRString, &vFrom, XtRPixel, &vTo);
	if (vTo.addr == NULL)
	  appData.monoMode = True;
	else
	  blackPieceColor = *(Pixel *) vTo.addr;
    }

    if (appData.monoMode && appData.debugMode) {
	fprintf(stderr, "white pixel = 0x%lx, black pixel = 0x%lx\n",
		(unsigned long) XWhitePixel(xDisplay, xScreen),
		(unsigned long) XBlackPixel(xDisplay, xScreen));
    }
    
    XtAppAddActions(appContext, boardActions, XtNumber(boardActions));
    
    /*
     * widget hierarchy
     */
    if (tinyLayout) {
	layoutName = "tinyLayout";
    } else if (smallLayout) {
	layoutName = "smallLayout";
    } else {
	layoutName = "normalLayout";
    }
    /* Outer layoutWidget is there only to provide a name for use in
       resources that depend on the layout style */
    layoutWidget =
      XtCreateManagedWidget(layoutName, formWidgetClass, shellWidget,
			    layoutArgs, XtNumber(layoutArgs));
    formWidget =
      XtCreateManagedWidget("form", formWidgetClass, layoutWidget,
			    formArgs, XtNumber(formArgs));
    XtSetArg(args[0], XtNdefaultDistance, &sep);
    XtGetValues(formWidget, args, 1);
    
    j = 0;
    widgetList[j++] = menuBarWidget = CreateMenuBar(menuBar);

    widgetList[j++] = whiteTimerWidget =
      XtCreateWidget("whiteTime", labelWidgetClass,
		     formWidget, timerArgs, XtNumber(timerArgs));
    XtSetArg(args[0], XtNfont, clockFontStruct);
    XtSetValues(whiteTimerWidget, args, 1);
    
    widgetList[j++] = blackTimerWidget =
      XtCreateWidget("blackTime", labelWidgetClass,
		     formWidget, timerArgs, XtNumber(timerArgs));
    XtSetArg(args[0], XtNfont, clockFontStruct);
    XtSetValues(blackTimerWidget, args, 1);
    
    if (appData.titleInWindow) {
	widgetList[j++] = titleWidget = 
	  XtCreateWidget("title", labelWidgetClass, formWidget,
			 titleArgs, XtNumber(titleArgs));
    }

    widgetList[j++] = buttonBarWidget = CreateButtonBar(buttonBar);

    widgetList[j++] = messageWidget =
      XtCreateWidget("message", labelWidgetClass, formWidget,
		     messageArgs, XtNumber(messageArgs));
    
    widgetList[j++] = boardWidget =
      XtCreateWidget("board", widgetClass, formWidget, boardArgs,
		     XtNumber(boardArgs));
    
    XtManageChildren(widgetList, j);
    
    timerWidth = (boardWidth - sep) / 2;
    XtSetArg(args[0], XtNwidth, timerWidth);
    XtSetValues(whiteTimerWidget, args, 1);
    XtSetValues(blackTimerWidget, args, 1);
    
    XtSetArg(args[0], XtNbackground, &timerBackgroundPixel);
    XtSetArg(args[1], XtNforeground, &timerForegroundPixel);
    XtGetValues(whiteTimerWidget, args, 2);
    
    XtSetArg(args[0], XtNbackground, &buttonBackgroundPixel);
    XtSetArg(args[1], XtNforeground, &buttonForegroundPixel);
    XtGetValues(XtNameToWidget(buttonBarWidget, PAUSE_BUTTON), args, 2);

    /*
     * formWidget uses these constraints but they are stored
     * in the children.
     */
    i = 0;
    XtSetArg(args[i], XtNfromHoriz, 0); i++;
    XtSetValues(menuBarWidget, args, i);
    if (appData.titleInWindow) {
	if (smallLayout) {
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, menuBarWidget); i++;
	    XtSetValues(whiteTimerWidget, args, i);
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, menuBarWidget); i++;
	    XtSetArg(args[i], XtNfromHoriz, whiteTimerWidget); i++;
	    XtSetValues(blackTimerWidget, args, i);
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, whiteTimerWidget); i++;
            XtSetArg(args[i], XtNjustify, XtJustifyLeft); i++;
	    XtSetValues(titleWidget, args, i);
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, titleWidget); i++;
	    XtSetValues(messageWidget, args, i);
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, titleWidget); i++;
	    XtSetArg(args[i], XtNfromHoriz, messageWidget); i++;
	    XtSetValues(buttonBarWidget, args, i);
	} else {
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, titleWidget); i++;
	    XtSetValues(whiteTimerWidget, args, i);
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, titleWidget); i++;
	    XtSetArg(args[i], XtNfromHoriz, whiteTimerWidget); i++;
	    XtSetValues(blackTimerWidget, args, i);
	    i = 0;
	    XtSetArg(args[i], XtNfromHoriz, menuBarWidget); i++;
	    XtSetValues(titleWidget, args, i);
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, whiteTimerWidget); i++;
	    XtSetValues(messageWidget, args, i);
	    i = 0;
	    XtSetArg(args[i], XtNfromVert, whiteTimerWidget); i++;
	    XtSetArg(args[i], XtNfromHoriz, messageWidget); i++;
	    XtSetValues(buttonBarWidget, args, i);
	}
    } else {
	i = 0;
	XtSetArg(args[i], XtNfromVert, menuBarWidget); i++;
	XtSetValues(whiteTimerWidget, args, i);
	i = 0;
	XtSetArg(args[i], XtNfromVert, menuBarWidget); i++;
	XtSetArg(args[i], XtNfromHoriz, whiteTimerWidget); i++;
	XtSetValues(blackTimerWidget, args, i);
	i = 0;
	XtSetArg(args[i], XtNfromVert, whiteTimerWidget); i++;
	XtSetValues(messageWidget, args, i);
	i = 0;
	XtSetArg(args[i], XtNfromVert, whiteTimerWidget); i++;
	XtSetArg(args[i], XtNfromHoriz, messageWidget); i++;
	XtSetValues(buttonBarWidget, args, i);
    }
    i = 0;
    XtSetArg(args[0], XtNfromVert, messageWidget);
    XtSetValues(boardWidget, args, 1);
    
    XtRealizeWidget(shellWidget);

    /*
     * Correct the width of the message and title widgets.
     * It is not known why some systems need the extra fudge term.
     * The value "2" is probably larger than needed.
     */
#define WIDTH_FUDGE 2
    i = 0;
    XtSetArg(args[i], XtNwidth, &w);  i++;
    XtGetValues(buttonBarWidget, args, i);
    i = 0;
    XtSetArg(args[i], XtNborderWidth, &bor);  i++;
    XtSetArg(args[i], XtNheight, &h);  i++;
    XtGetValues(messageWidget, args, i);

    w = boardWidth - w - sep - 2*bor - WIDTH_FUDGE;
    gres = XtMakeResizeRequest(messageWidget, w, h, &wr, &hr);
    if (gres != XtGeometryYes) {
	fprintf(stderr, "%s: messageWidget geometry error %d %d %d %d %d\n",
		programName, gres, w, h, wr, hr);
    }

    if (appData.titleInWindow) {
	i = 0;
	XtSetArg(args[i], XtNborderWidth, &bor); i++;
	XtSetArg(args[i], XtNheight, &h);  i++;
	XtGetValues(titleWidget, args, i);
	if (smallLayout) {
	    w = boardWidth - 2*bor;
	} else {
	    XtSetArg(args[0], XtNwidth, &w);
	    XtGetValues(menuBarWidget, args, 1);
	    w = boardWidth - w - sep - 2*bor - WIDTH_FUDGE;
	}

	gres = XtMakeResizeRequest(titleWidget, w, h, &wr, &hr);
	if (gres != XtGeometryYes) {
	    fprintf(stderr,
		    "%s: titleWidget geometry error %d %d %d %d %d\n",
		    programName, gres, w, h, wr, hr);
	}
    }
    xBoardWindow = XtWindow(boardWidget);
    
    /* 
     * Create X checkmark bitmap and initialize option menu checks.
     */
    ReadBitmap(&xMarkPixmap, "checkmark.bm",
	       checkmark_bits, checkmark_width, checkmark_height);
    XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    if (appData.alwaysPromoteToQueen) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Always Queen"),
		    args, 1);
    }
    if (appData.autoComment) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Comment"),
		    args, 1);
    }
    if (appData.autoCallFlag) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Flag"),
		    args, 1);
    }
    if (appData.autoObserve) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Observe"),
		    args, 1);
    }
    if (appData.autoSaveGames) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Save"),
		    args, 1);
    }
    if (appData.saveGameFile[0] != NULLCHAR) {
	/* Can't turn this off from menu */
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Save"),
		    args, 1);
	XtSetSensitive(XtNameToWidget(menuBarWidget, "menuOptions.Auto Save"),
		       False);

    }
    if (appData.ringBellAfterMoves) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Bell"),
		    args, 1);
    }
    if (appData.checkLegality) {
	XtSetValues(XtNameToWidget(menuBarWidget,"menuOptions.Check Legality"),
		    args, 1);
    }
    if (appData.getMoveList) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Get Move List"),
		    args, 1);
    }
    if (appData.oldSaveStyle) {
	XtSetValues(XtNameToWidget(menuBarWidget,
				   "menuOptions.Old Save Style"), args, 1);
    }
    if (appData.quietPlay) {
	XtSetValues(XtNameToWidget(menuBarWidget,
				   "menuOptions.Quiet Play"), args, 1);
    }
    if (appData.showCoords) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Show Coords"),
		    args, 1);
    }
    if (appData.showThinking) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Show Thinking"),
		    args, 1);
    }
    if (appData.periodicUpdates) {
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Periodic Updates"),
		    args, 1);
    }	

    /*
     * Create an icon.
     */
    ReadBitmap(&wIconPixmap, "icon_white.bm",
	       icon_white_bits, icon_white_width, icon_white_height);
    ReadBitmap(&bIconPixmap, "icon_black.bm",
	       icon_black_bits, icon_black_width, icon_black_height);
    iconPixmap = wIconPixmap;
    i = 0;
    XtSetArg(args[i], XtNiconPixmap, iconPixmap);  i++;
    XtSetValues(shellWidget, args, i);
    
    /*
     * Create a cursor for the board widget.
     */
    window_attributes.cursor = XCreateFontCursor(xDisplay, XC_hand2);
    XChangeWindowAttributes(xDisplay, xBoardWindow,
			    CWCursor, &window_attributes);
    
    /*
     * Inhibit shell resizing.
     */
    shellArgs[0].value = (XtArgVal) &w;
    shellArgs[1].value = (XtArgVal) &h;
    XtGetValues(shellWidget, shellArgs, 2);
    shellArgs[4].value = shellArgs[2].value = w;
    shellArgs[5].value = shellArgs[3].value = h;
    XtSetValues(shellWidget, &shellArgs[2], 4);
    
    CreateGCs();
    CreateGrid();
#if HAVE_LIBXPM
	CreateXPMPieces();
	/* Create regular pieces if -xpm not specified */
	if ( useXPMs == 0 )
    CreatePieces();
#else
	CreateXIMPieces();
	/* Create regular pieces if -xpm not specified */
	if ( useXIMs == 0 )
    CreatePieces();
#endif  

    CreatePieceMenus();
    
    XtAugmentTranslations(formWidget,
			  XtParseTranslationTable(globalTranslations));
    XtAugmentTranslations(boardWidget,
			  XtParseTranslationTable(boardTranslations));
    XtAugmentTranslations(whiteTimerWidget,
			  XtParseTranslationTable(whiteTranslations));
    XtAugmentTranslations(blackTimerWidget,
			  XtParseTranslationTable(blackTranslations));

    /* Why is the following needed on some versions of X? */
    XtAddEventHandler(boardWidget, ExposureMask, False,
		      (XtEventHandler) EventProc, NULL);
    /* end why */

    InitBackEnd2();
    
    if (errorExitStatus == -1) {
	if (appData.icsActive) {
	    /* We now wait until we see "login:" from the ICS before
	       sending the logon script (problems with timestamp otherwise) */
	    /*ICSInitScript();*/
	    if (appData.icsInputBox) ICSInputBoxPopUp();
	}

	signal(SIGINT, IntSigHandler);
	signal(SIGTERM, IntSigHandler);
	if (*appData.cmailGameName != NULLCHAR) {
	    signal(SIGUSR1, CmailSigHandler);
	}
    }

    XtAppMainLoop(appContext);
}

RETSIGTYPE
IntSigHandler(sig)
     int sig;
{
    ExitEvent(sig);
}

RETSIGTYPE
CmailSigHandler(sig)
     int sig;
{
    int dummy = 0;
    int error;

    signal(SIGUSR1, SIG_IGN);			  /* suspend handler     */

    /* Activate call-back function CmailSigHandlerCallBack()             */
    OutputToProcess(cmailPR, (char *)(&dummy), sizeof(int), &error);

    signal(SIGUSR1, CmailSigHandler);		  /* re-activate handler */
}

void CmailSigHandlerCallBack(isr, message, count, error)
     InputSourceRef isr;
     char *message;
     int count;
     int error;
{
    XtMapWidget(shellWidget);                       /* Open if iconified */
    XtPopup(shellWidget, XtGrabNone);               /* Raise if lowered  */

    ReloadCmailMsgEvent(TRUE);			    /* Reload cmail msg  */

}
/**** end signal code ****/

void Usage()
{
    fprintf(stderr, "Usage: %s\n", programName);
    fprintf(stderr, "\t-timeControl (or -tc) minutes[:seconds]\n");
    fprintf(stderr, "\t-movesPerSession (or -mps) number\n");
    fprintf(stderr, "\t-timeIncrement (or -inc) seconds\n");
    fprintf(stderr, "\t-clockMode (True | False), or -[x]clock\n");
    fprintf(stderr, "\t-searchTime (or -st) minutes[:seconds]\n");
    fprintf(stderr, "\t-searchDepth (or -sd) number\n");
    fprintf(stderr, "\t-matchMode (True | False), or -[x]mm\n");
    fprintf(stderr, "\t-internetChessServerMode (True | False), or -[x]ics\n");
    fprintf(stderr, "\t-internetChessServerHost (or -icshost) host_name\n");
    fprintf(stderr, "\t-loadGameFile (or -lgf) file_name\n");
    fprintf(stderr, "\t-loadGameIndex (or -lgi) number\n");
    fprintf(stderr,
	    "\t-boardSize (or -size) (Large | Medium | Small | Tiny)\n");
    fprintf(stderr, "\t-noChessProgram (True | False), or -[x]ncp\n");
    fprintf(stderr, "\t-debugMode (True | False), or -[x]debug\n");
    fprintf(stderr, "See the man page for more options and information\n");
    exit(2);
}

void ICSInitScript()
{
    FILE *f;
    char buf[MSG_SIZ];
    char *p;

    f = fopen(appData.icsLogon, "r");
    if (f == NULL) {
	p = getenv("HOME");
	if (p != NULL) {
	    strcpy(buf, p);
	    strcat(buf, "/");
	    strcat(buf, appData.icsLogon);
	    f = fopen(buf, "r");
	}
    }
    if (f != NULL)
      ProcessICSInitScript(f);
}

void ResetFrontEnd()
{
    CommentPopDown();
    EditCommentPopDown();
    TagsPopDown();
    return;
}

typedef struct {
    char *name;
    Boolean value;
} Sensitivity;

void SetMenuSensitivity(sens)
     Sensitivity *sens;
{
    while (sens->name != NULL) {
	XtSetSensitive(XtNameToWidget(menuBarWidget, sens->name), sens->value);
	sens++;
    }
}

Sensitivity icsSensitivity[] = {
    { "menuFile.Mail Move", False },
    { "menuFile.Reload CMail Message", False },
    { "menuMode.Machine Black", False },
    { "menuMode.Machine White", False },
    { "menuMode.Analysis Mode", False },
    { "menuMode.Analyze File", False },
    { "menuMode.Two Machines", False },
#ifndef ZIPPY
    { "menuHelp.Hint", False },
    { "menuHelp.Book", False },
    { "menuStep.Move Now", False },
    { "menuOptions.Periodic Updates", False },	
    { "menuOptions.Show Thinking", False },
#endif
    { NULL, False }
};

void SetICSMode()
{
    SetMenuSensitivity(icsSensitivity);
}

Sensitivity ncpSensitivity[] = {    
    { "menuFile.Mail Move", False },
    { "menuFile.Reload CMail Message", False },
    { "menuMode.Machine White", False },
    { "menuMode.Machine Black", False },
    { "menuMode.Analysis Mode", False },
    { "menuMode.Analyze File", False },
    { "menuMode.Two Machines", False },
    { "menuMode.ICS Client", False },
    { "menuMode.ICS Input Box", False },
    { "Action", False },
    { "menuStep.Revert", False },
    { "menuStep.Move Now", False },
    { "menuStep.Retract Move", False },
    { "menuOptions.Auto Comment", False },
    { "menuOptions.Auto Flag", False },
    { "menuOptions.Auto Observe", False },
    { "menuOptions.Bell", False },
    { "menuOptions.Get Move List", False },
    { "menuOptions.Quiet Play", False },
    { "menuOptions.Show Thinking", False },
    { "menuOptions.Periodic Updates", False },	
    { "menuHelp.Hint", False },
    { "menuHelp.Book", False },
    { NULL, False }
};

void SetNCPMode()
{
    SetMenuSensitivity(ncpSensitivity);
}

Sensitivity gnuSensitivity[] = {    
    { "menuMode.ICS Client", False },
    { "menuMode.ICS Input Box", False },
    { "menuAction.Accept", False },
    { "menuAction.Decline", False },
    { "menuAction.Draw", False },
    { "menuAction.Adjourn", False },
    { "menuAction.Stop Examining", False },
    { "menuAction.Stop Observing", False },
    { "menuStep.Revert", False },
    { "menuOptions.Auto Comment", False },
    { "menuOptions.Auto Flag", False },
    { "menuOptions.Auto Observe", False },
    { "menuOptions.Get Move List", False },
    { "menuOptions.Quiet Play", False },

    /* The next two options rely on SetCmailMode being called *after*    */
    /* SetGNUMode so that when GNU is being used to give hints these     */
    /* menu options are still available                                  */

    { "menuFile.Mail Move", False },
    { "menuFile.Reload CMail Message", False },
    { NULL, False }
};

void SetGNUMode()
{
    SetMenuSensitivity(gnuSensitivity);
}

Sensitivity cmailSensitivity[] = {    
    { "Action", True },
    { "menuAction.Call Flag", False },
    { "menuAction.Draw", True },
    { "menuAction.Adjourn", False },
    { "menuAction.Abort", False },
    { "menuAction.Stop Observing", False },
    { "menuAction.Stop Examining", False },
    { "menuFile.Mail Move", True },
    { "menuFile.Reload CMail Message", True },
    { NULL, False }
};

void SetCmailMode()
{
    SetMenuSensitivity(cmailSensitivity);
}

#define Abs(n) ((n)<0 ? -(n) : (n))

/*
 * Find a font that matches "pattern" that is as close as
 * possible to the targetPxlSize.  Prefer fonts that are k
 * pixels smaller to fonts that are k pixels larger.  The
 * pattern must be in the X Consortium standard format, 
 * e.g. "-*-helvetica-bold-r-normal--*-*-*-*-*-*-*-*".
 * The return value should be freed with XtFree when no
 * longer needed.
 */
char *FindFont(pattern, targetPxlSize)
     char *pattern;
     int targetPxlSize;
{
    char **fonts, *p, *best;
    int i, j, nfonts, minerr, err, pxlSize;

    fonts = XListFonts(xDisplay, pattern, 999999, &nfonts);
    if (nfonts < 1) {
	fprintf(stderr, "%s: No fonts match pattern %s\n",
		programName, pattern);
	exit(2);
    }
    best = "";
    minerr = 999999;
    for (i=0; i<nfonts; i++) {
	j = 0;
	p = fonts[i];
	if (*p != '-') continue;
	while (j < 7) {
	    if (*p == NULLCHAR) break;
	    if (*p++ == '-') j++;
	}
	if (j < 7) continue;
	pxlSize = atoi(p);
	if (pxlSize == targetPxlSize) {
	    best = fonts[i];
	    break;
	}
	err = pxlSize - targetPxlSize;
	if (Abs(err) < Abs(minerr) ||
	    (minerr > 0 && err < 0 && -err == minerr)) {
	    best = fonts[i];
	    minerr = err;
	}
    }
    p = (char *) XtMalloc(strlen(best) + 1);
    strcpy(p, best);
    XFreeFontNames(fonts);
    return p;
}

void CreateGCs()
{
    XtGCMask value_mask = GCLineWidth | GCLineStyle | GCForeground
      | GCBackground | GCFunction | GCPlaneMask;
    XGCValues gc_values;
    GC copyInvertedGC;
    
    gc_values.plane_mask = AllPlanes;
    gc_values.line_width = lineGap;
    gc_values.line_style = LineSolid;
    gc_values.function = GXcopy;
    
    gc_values.foreground = XBlackPixel(xDisplay, xScreen);
    gc_values.background = XBlackPixel(xDisplay, xScreen);
    lineGC = XtGetGC(shellWidget, value_mask, &gc_values);
    
    gc_values.foreground = XWhitePixel(xDisplay, xScreen);
    gc_values.background = XWhitePixel(xDisplay, xScreen);
    highlineGC = XtGetGC(shellWidget, value_mask, &gc_values);	

    gc_values.background = XWhitePixel(xDisplay, xScreen);
    coordGC = XtGetGC(shellWidget, value_mask, &gc_values);
    XSetFont(xDisplay, coordGC, coordFontID);
    
    if (appData.monoMode) {
	gc_values.foreground = XWhitePixel(xDisplay, xScreen);
	gc_values.background = XBlackPixel(xDisplay, xScreen);
	lightSquareGC = wbPieceGC 
	  = XtGetGC(shellWidget, value_mask, &gc_values);

	gc_values.foreground = XBlackPixel(xDisplay, xScreen);
	gc_values.background = XWhitePixel(xDisplay, xScreen);
	darkSquareGC = bwPieceGC
	  = XtGetGC(shellWidget, value_mask, &gc_values);

	if (DefaultDepth(xDisplay, xScreen) == 1) {
	    /* Avoid XCopyPlane on 1-bit screens to work around Sun bug */
	    gc_values.function = GXcopyInverted;
	    copyInvertedGC = XtGetGC(shellWidget, value_mask, &gc_values);
	    gc_values.function = GXcopy;
	    if (XBlackPixel(xDisplay, xScreen) == 1) {
		bwPieceGC = darkSquareGC;
		wbPieceGC = copyInvertedGC;
	    } else {
		bwPieceGC = copyInvertedGC;
		wbPieceGC = lightSquareGC;
	    }
	}
    } else {
	gc_values.foreground = lightSquareColor;
	gc_values.background = darkSquareColor;
	lightSquareGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = darkSquareColor;
	gc_values.background = lightSquareColor;
	darkSquareGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = whitePieceColor;
	gc_values.background = darkSquareColor;
	wdPieceGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = whitePieceColor;
	gc_values.background = lightSquareColor;
	wlPieceGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = blackPieceColor;
	gc_values.background = darkSquareColor;
	bdPieceGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = blackPieceColor;
	gc_values.background = lightSquareColor;
	blPieceGC = XtGetGC(shellWidget, value_mask, &gc_values);
    }
}

void loadXIM( xim, filename, dest )
	 XImage *xim;
	 char *filename;
	 Pixmap *dest;
{
  int x, y, w, h, p;
  FILE *fp;

  fp = fopen( filename, "rb" );
  if ( !fp ) {
	  fprintf( stderr, "Error loading XIM!\n");
	  exit(1);
	}
	  
  w = fgetc( fp );
  h = fgetc( fp );
  
  for( y=0; y<h; ++y ) {
	for( x=0; x<h; ++x ) {
	  p = fgetc( fp );

	  switch (p) {
		case 0:	XPutPixel( xim, x, y, blackPieceColor ); break;
		case 1:	XPutPixel( xim, x, y, darkSquareColor ); break;
		case 2:	XPutPixel( xim, x, y, whitePieceColor ); break;
		case 3:	XPutPixel( xim, x, y, lightSquareColor ); break;
		}
	}
  }

  *dest = XCreatePixmap( xDisplay, DefaultRootWindow( xDisplay ),
						w, h, xim->depth );
  XPutImage( xDisplay, *dest, lightSquareGC, xim,
			0, 0, 0, 0, w, h );  
}

void CreateXIMPieces()
{
  int piece, kind;
  char buf[MSG_SIZ];
  u_int ss = squareSize;
  char *ximkind[] = { "ll", "ld", "dl", "dd" };

  /* The XSynchronize calls were copied from CreatePieces.
	 Not sure if needed, but can't hurt */
  XSynchronize(xDisplay, True); /* Work-around for xlib/xt
								   buffering bug */
  
  if ( strlen( appData.xpmDirectory ) == 0 )
	useXIMs = 0;
  else {
	  
	useXIMs = 1;

	fprintf( stderr, "\nLoading XIMs...\n");

	/* Load pieces */
	for( piece = (int) WhitePawn; piece <= (int) WhiteKing; piece++ )
	  {
	    fprintf( stderr, "%d", piece+1 );
	    for( kind=0; kind<4; kind++ )
	      {
		fprintf(stderr,"." );
		sprintf( buf, "%s/%c%s%u.xim",
					ExpandPathName(appData.xpmDirectory),
					ToLower(PieceToChar((ChessSquare)piece)),
					ximkind[kind], ss );
			
			ximPieceBitmap[kind][piece] =
			  XGetImage( xDisplay, DefaultRootWindow(xDisplay),
						0, 0, ss, ss, AllPlanes, XYPixmap );
		if ( appData.debugMode )
		  fprintf(stderr, "(File:%s:) ", buf );
			loadXIM( ximPieceBitmap[kind][piece], buf,
					&(xpmPieceBitmap[kind][piece]) );
		  }
		fprintf(stderr," " );
	  }

	/* Load light and dark squares */
	  
	/* If the LSQ and DSQ pieces don't exist, we will 
	   draw them with solid squares. */
	sprintf( buf, "%s/lsq%u.xim", ExpandPathName(appData.xpmDirectory), ss );
	if ( access( buf, 0 ) != 0 )
	  useXIMsqs = 0;
	else {
	  useXIMsqs = 1;
	  fprintf( stderr, "light square " );
	  ximLightSquare= 
		XGetImage( xDisplay, DefaultRootWindow(xDisplay),
				  0, 0, ss, ss, AllPlanes, XYPixmap );
		if ( appData.debugMode )
		  fprintf(stderr, "(File:%s:) ", buf );

	  loadXIM( ximLightSquare, buf, &xpmLightSquare );
	  
	  fprintf( stderr, "dark square " );
	  sprintf( buf, "%s/dsq%u.xim", ExpandPathName(appData.xpmDirectory), ss );
		if ( appData.debugMode )
		  fprintf(stderr, "(File:%s:) ", buf );

	  ximDarkSquare= 
		XGetImage( xDisplay, DefaultRootWindow(xDisplay),
				  0, 0, ss, ss, AllPlanes, XYPixmap );
	  loadXIM( ximDarkSquare, buf, &xpmDarkSquare );
	}
	  
	fprintf( stderr, "Done.\n");
  }

  XSynchronize(xDisplay, False); /* Work-around for xlib/xt buffering bug */  
}

#if HAVE_LIBXPM
void CreateXPMPieces()
{
  int piece, kind, r;
  char buf[MSG_SIZ];
  u_int ss = squareSize;
  XpmAttributes attr;
  char *xpmkind[] = { "ll", "ld", "dl", "dd" };
  XpmColorSymbol symbols[4];

  if ( appData.debugMode ) {
    printf("XPM Library Version: %d.%d%c\n", XpmFormat, XpmVersion,
	   (char)('a' + XpmRevision - 1) );
  }
  
  /* The XSynchronize calls were copied from CreatePieces.
	 Not sure if needed, but can't hurt */
  XSynchronize(xDisplay, True); /* Work-around for xlib/xt buffering bug */
  
  if ( strlen( appData.xpmDirectory ) == 0 )
	useXPMs = 0;
  else {
	  
	useXPMs = 1;
	/* Setup translations so piece colors match square colors */
	symbols[0].name = "light_piece";
	symbols[0].value = appData.whitePieceColor;
	symbols[1].name = "dark_piece";
	symbols[1].value = appData.blackPieceColor;
	symbols[2].name = "light_square";
	symbols[2].value = appData.lightSquareColor;
	symbols[3].name = "dark_square";
	symbols[3].value = appData.darkSquareColor;

	attr.valuemask = XpmColorSymbols;
	attr.colorsymbols = symbols;
	attr.numsymbols = 4;
	
	fprintf( stderr, "\nLoading XPMs...\n");

	/* Load pieces */
	for( piece = (int) WhitePawn; piece <= (int) WhiteKing; piece++ )
	  {
	    fprintf( stderr, "%d ", piece+1 );
	    for( kind=0; kind<4; kind++ )
	      {
		sprintf( buf, "%s/%c%s%u.xpm",
			 ExpandPathName(appData.xpmDirectory),
			 ToLower(PieceToChar((ChessSquare)piece)),
			 xpmkind[kind], ss );
		if ( appData.debugMode )
		  fprintf(stderr, "(File:%s:) ", buf );
		
		if ( (r=XpmReadFileToPixmap( xDisplay, xBoardWindow, buf,
					  &(xpmPieceBitmap[kind][piece]),
					  NULL, &attr )) != 0 ) {
		  fprintf( stderr, "Error %d loading XPM file \"%s\"\n",
			   r, buf );
		  exit(1); 
		}	
	      }	
	  }

	/* Load light and dark squares */
	  
	/* If the LSQ and DSQ pieces don't exist, we will 
	   draw them with solid squares. */
	fprintf( stderr, "light square " );
	sprintf( buf, "%s/lsq%u.xpm", ExpandPathName(appData.xpmDirectory), ss );
	if ( access( buf, 0 ) != 0 )
	  useXPMsqs = 0;
	else {
	  useXPMsqs = 1;
		if ( appData.debugMode )
		  fprintf(stderr, "(File:%s:) ", buf );

		if ( (r=XpmReadFileToPixmap( xDisplay, xBoardWindow, buf,
				    &xpmLightSquare,
				    NULL, &attr )) != 0 ) {
	    fprintf( stderr, "Error %d loading XPM file \"%s\"\n", r, buf);
	    exit(1);
	  }
	  	  
	  fprintf( stderr, "dark square " );
	  sprintf( buf, "%s/dsq%u.xpm", ExpandPathName(appData.xpmDirectory), ss );
		if ( appData.debugMode )
		  fprintf(stderr, "(File:%s:) ", buf );

		if ( (r=XpmReadFileToPixmap( xDisplay, xBoardWindow, buf,
				    &xpmDarkSquare,
				    NULL, &attr )) != 0 ) {
	    fprintf( stderr, "Error %d loading XPM file \"%s\"\n", r, buf);
	    exit(1);
	  }
	}
	  
	fprintf( stderr, "Done.\n");
  }

  XSynchronize(xDisplay, False); /* Work-around for xlib/xt
									buffering bug */  
}
#endif /* HAVE_LIBXPM */

void CreatePieces()
{
    BuiltInBits* bib = builtInBits;
    int piece, kind;
    char buf[MSG_SIZ];
    u_int ss = squareSize;
	
    XSynchronize(xDisplay, True); /* Work-around for xlib/xt
				     buffering bug */

    while (bib->squareSize != ss && bib->squareSize != 0) bib++;

    for (kind = SOLID; kind <= (appData.monoMode ? OUTLINE : SOLID); kind++) {
	for (piece = (int) WhitePawn; piece <= (int) WhiteKing; piece++) {
	    sprintf(buf, "%c%u%c.bm", ToLower(PieceToChar((ChessSquare)piece)),
		    ss, kind == SOLID ? 's' : 'o');
	    ReadBitmap(&pieceBitmap[kind][piece], buf,
		       bib->bits[kind][piece], ss, ss);
	}
    }
    
    XSynchronize(xDisplay, False); /* Work-around for xlib/xt
				      buffering bug */
}

void ReadBitmap(pm, name, bits, wreq, hreq)
     Pixmap *pm;
     String name;
     unsigned char bits[];
     u_int wreq, hreq;
{
    int x_hot, y_hot;
    u_int w, h;
    int errcode;
    char msg[MSG_SIZ], fullname[MSG_SIZ];
    
    if (*appData.bitmapDirectory != NULLCHAR) {
        strcpy(fullname, appData.bitmapDirectory);
	strcat(fullname, "/");
	strcat(fullname, name);
	errcode = XReadBitmapFile(xDisplay, xBoardWindow, fullname,
				  &w, &h, pm, &x_hot, &y_hot);
	if (errcode != BitmapSuccess) {
	    switch (errcode) {
	      case BitmapOpenFailed:
		sprintf(msg, "Can't open bitmap file %s", fullname);
		break;
	      case BitmapFileInvalid:
		sprintf(msg, "Invalid bitmap in file %s", fullname);
		break;
	      case BitmapNoMemory:
		sprintf(msg, "Ran out of memory reading bitmap file %s",
			fullname);
		break;
	      default:
		sprintf(msg, "Unknown XReadBitmapFile error %d on file %s",
			errcode, fullname);
		break;
	    }
	    fprintf(stderr, "%s: %s...using built-in\n",
		    programName, msg);
	} else if (w != wreq || h != hreq) {
	    fprintf(stderr,
		    "%s: Bitmap %s is %dx%d, not %dx%d...using built-in\n",
		    programName, fullname, w, h, wreq, hreq);
	} else {
	    return;
	}
    }
    if (bits == NULL) {
	fprintf(stderr, "%s: No built-in bitmap for %s; giving up\n",
		programName, name);
	    exit(1);
    } else {
	*pm = XCreateBitmapFromData(xDisplay, xBoardWindow, (char *) bits,
				    wreq, hreq);
    }
}

void CreateGrid()
{
    int i;
    
    if (lineGap == 0) return;
    for (i = 0; i < BOARD_SIZE + 1; i++) {
	gridSegments[i].x1 = gridSegments[i + BOARD_SIZE + 1].y1 = 0;
	gridSegments[i].y1 = gridSegments[i].y2
	  = lineGap / 2 + (i * (squareSize + lineGap));
	gridSegments[i].x2 = lineGap + BOARD_SIZE *
	  (squareSize + lineGap);
	gridSegments[i + BOARD_SIZE + 1].x1 =
	  gridSegments[i + BOARD_SIZE + 1].x2 = lineGap / 2
	    + (i * (squareSize + lineGap));
	gridSegments[i + BOARD_SIZE + 1].y2 =
	  BOARD_SIZE * (squareSize + lineGap);
    }
}

static void MenuBarSelect(w, addr, index)
     Widget w;
     caddr_t addr;
     caddr_t index;
{
    XtActionProc proc = (XtActionProc) addr;

    (proc)(NULL, NULL, NULL, NULL);
}

void CreateMenuBarPopup(parent, name, mb)
     Widget parent;
     String name;
     Menu *mb;
{
    int j;
    Widget menu, entry;
    MenuItem *mi;
    Arg args[16];

    menu = XtCreatePopupShell(name, simpleMenuWidgetClass,
			      parent, NULL, 0);
    j = 0;
    XtSetArg(args[j], XtNleftMargin, 20);   j++;
    XtSetArg(args[j], XtNrightMargin, 20);  j++;
    mi = mb->mi;
    while (mi->string != NULL) {
	if (strcmp(mi->string, "----") == 0) {
	    entry = XtCreateManagedWidget(mi->string, smeLineObjectClass,
					  menu, args, j);
	} else {
	    entry = XtCreateManagedWidget(mi->string, smeBSBObjectClass,
					  menu, args, j);
	    XtAddCallback(entry, XtNcallback,
			  (XtCallbackProc) MenuBarSelect,
			  (caddr_t) mi->proc);
	}
	mi++;
    }
}	

Widget CreateMenuBar(mb)
     Menu *mb;
{
    int j;
    Widget anchor, menuBar;
    Arg args[16];
    char menuName[MSG_SIZ];

    j = 0;
    XtSetArg(args[j], XtNorientation, XtorientHorizontal);  j++;
    XtSetArg(args[j], XtNvSpace, 0);                        j++;
    XtSetArg(args[j], XtNborderWidth, 0);                   j++;
    menuBar = XtCreateWidget("menuBar", boxWidgetClass,
			     formWidget, args, j);

    while (mb->name != NULL) {
	strcpy(menuName, "menu");
	strcat(menuName, mb->name);
	j = 0;
	XtSetArg(args[j], XtNmenuName, XtNewString(menuName));  j++;
	if (tinyLayout) {
	    char shortName[2];
	    shortName[0] = mb->name[0];
	    shortName[1] = NULLCHAR;
	    XtSetArg(args[j], XtNlabel, XtNewString(shortName)); j++;
	}
	XtSetArg(args[j], XtNborderWidth, 0);                   j++;
	anchor = XtCreateManagedWidget(mb->name, menuButtonWidgetClass,
				       menuBar, args, j);
	CreateMenuBarPopup(menuBar, menuName, mb);
	mb++;
    }
    return menuBar;
}

Widget CreateButtonBar(mi)
     MenuItem *mi;
{
    int j;
    Widget button, buttonBar;
    Arg args[16];

    j = 0;
    XtSetArg(args[j], XtNorientation, XtorientHorizontal); j++;
    if (tinyLayout) {
	XtSetArg(args[j], XtNhSpace, 0); j++;
    }
    XtSetArg(args[j], XtNborderWidth, 0); j++;
    XtSetArg(args[j], XtNvSpace, 0);                        j++;
    buttonBar = XtCreateWidget("buttonBar", boxWidgetClass,
			       formWidget, args, j);

    while (mi->string != NULL) {
	j = 0;
	if (tinyLayout) {
	    XtSetArg(args[j], XtNinternalWidth, 2); j++;
	    XtSetArg(args[j], XtNborderWidth, 0); j++;
	}
	button = XtCreateManagedWidget(mi->string, commandWidgetClass,
				       buttonBar, args, j);
	XtAddCallback(button, XtNcallback,
		      (XtCallbackProc) MenuBarSelect,
		      (caddr_t) mi->proc);
	mi++;
    }
    return buttonBar;
}     

void CreatePieceMenus()
{
    int i;
    Widget entry;
    Arg args[16];
    ChessSquare selection;
    
    XtSetArg(args[0], XtNlabel, "White");
    whitePieceMenu = XtCreatePopupShell("menuW", simpleMenuWidgetClass,
					boardWidget, args, 1);
    for (i = 0; i < PIECE_MENU_SIZE; i++) {
	String item = pieceMenuStrings[i];
	
	if (strcmp(item, "----") == 0) {
	    entry = XtCreateManagedWidget(item, smeLineObjectClass,
					  whitePieceMenu, NULL, 0);
	} else {
	    entry = XtCreateManagedWidget(item, smeBSBObjectClass,
					  whitePieceMenu, NULL, 0);
	    selection = pieceMenuTranslation[0][i];
	    XtAddCallback(entry, XtNcallback,
			  (XtCallbackProc) PieceMenuSelect,
			  (caddr_t) selection);
	    if (selection == WhitePawn) {
		XtSetArg(args[0], XtNpopupOnEntry, entry);
		XtSetValues(whitePieceMenu, args, 1);
	    }
	}
    }
    
    XtSetArg(args[0], XtNlabel, "Black");
    blackPieceMenu = XtCreatePopupShell("menuB", simpleMenuWidgetClass,
					boardWidget, args, 1);
    for (i = 0; i < PIECE_MENU_SIZE; i++) {
	String item = pieceMenuStrings[i];
	
	if (strcmp(item, "----") == 0) {
	    entry = XtCreateManagedWidget(item, smeLineObjectClass,
					  blackPieceMenu, NULL, 0);
	} else {
	    entry = XtCreateManagedWidget(item, smeBSBObjectClass,
					  blackPieceMenu, NULL, 0);
	    selection = pieceMenuTranslation[1][i];
	    XtAddCallback(entry, XtNcallback,
			  (XtCallbackProc) PieceMenuSelect,
			  (caddr_t) selection);
	    if (selection == BlackPawn) {
		XtSetArg(args[0], XtNpopupOnEntry, entry);
		XtSetValues(blackPieceMenu, args, 1);
	    }
	}
    }
    
    XtRegisterGrabAction(PieceMenuPopup, True,
			 (unsigned)(ButtonPressMask|ButtonReleaseMask),
			 GrabModeAsync, GrabModeAsync);

    XtSetArg(args[0], XtNlabel, "Drop");
    dropMenu = XtCreatePopupShell("menuD", simpleMenuWidgetClass,
					boardWidget, args, 1);
    for (i = 0; i < DROP_MENU_SIZE; i++) {
	String item = dropMenuStrings[i];
	
	if (strcmp(item, "----") == 0) {
	    entry = XtCreateManagedWidget(item, smeLineObjectClass,
					  dropMenu, NULL, 0);
	} else {
	    entry = XtCreateManagedWidget(item, smeBSBObjectClass,
					  dropMenu, NULL, 0);
	    selection = dropMenuTranslation[i];
	    XtAddCallback(entry, XtNcallback,
			  (XtCallbackProc) DropMenuSelect,
			  (caddr_t) selection);
	}
    }
}	

void SetupDropMenu()
{
    int i, j, count;
    extern char white_holding[], black_holding[];
    char label[32];
    Arg args[16];
    Widget entry;
    char* p;

    for (i=0; i<sizeof(dmSensitivity)/sizeof(DropMenuSensitivity); i++) {
	entry = XtNameToWidget(dropMenu, dmSensitivity[i].widget);
	p = strchr(gameMode == IcsPlayingWhite ? white_holding : black_holding,
		   dmSensitivity[i].piece);
	XtSetSensitive(entry, p != NULL);
	count = 0;
	while (p && *p++ == dmSensitivity[i].piece) count++;
	sprintf(label, "%s  %d", dmSensitivity[i].widget, count);
	j = 0;
	XtSetArg(args[j], XtNlabel, label); j++;
	XtSetValues(entry, args, j);
    }
}

void PieceMenuPopup(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    String whichMenu;
    if (event->type != ButtonPress) return;
    if (errorUp) ErrorPopDown();
    switch (gameMode) {
      case EditPosition:
      case IcsExamining:
	whichMenu = params[0];
	break;
      case IcsPlayingWhite:
      case IcsPlayingBlack:
	if (!ics_bughouse) return;
	SetupDropMenu();
	whichMenu = "menuD";
	break;
      default:
	return;
    }
    
    if (((pmFromX = EventToSquare(event->xbutton.x)) < 0) ||
	((pmFromY = EventToSquare(event->xbutton.y)) < 0)) {
	pmFromX = pmFromY = -1;
	return;
    }
    if (flipView)
      pmFromX = BOARD_SIZE - 1 - pmFromX;
    else
      pmFromY = BOARD_SIZE - 1 - pmFromY;
    
    XtPopupSpringLoaded(XtNameToWidget(boardWidget, whichMenu));
}

static void PieceMenuSelect(w, piece, junk)
     Widget w;
     ChessSquare piece;
     caddr_t junk;
{
    if (pmFromX < 0 || pmFromY < 0) return;
    EditPositionMenuEvent(piece, pmFromX, pmFromY);
}

static void DropMenuSelect(w, piece, junk)
     Widget w;
     ChessSquare piece;
     caddr_t junk;
{
    if (pmFromX < 0 || pmFromY < 0) return;
    DropMenuEvent(piece, pmFromX, pmFromY);
}

void WhiteClock(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode == EditPosition) {
	SetWhiteToPlayEvent();
    } else if (gameMode == IcsPlayingBlack || gameMode == MachinePlaysWhite) {
	CallFlagEvent();
    }
}

void BlackClock(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode == EditPosition) {
	SetBlackToPlayEvent();
    } else if (gameMode == IcsPlayingWhite || gameMode == MachinePlaysBlack) {
	CallFlagEvent();
    }
}


/*
 * If the user selects on a border boundary, return -1; if off the board,
 *   return -2.  Otherwise map the event coordinate to the square.
 */
int EventToSquare(x)
     int x;
{
    if (x <= 0) 
      return -2;
    if (x < lineGap)
      return -1;
    x -= lineGap;
    if ((x % (squareSize + lineGap)) >= squareSize)
      return -1;
    x /= (squareSize + lineGap);
    if (x >= BOARD_SIZE)
      return -2;
    return x;
}

static void do_flash_delay( msec )
	 unsigned long msec;
{
  TimeDelay( msec );
}

static void drawHighlight( row, col, gc )
	 int row, col;
	 GC gc;
{
  int x, y;

  if ( lineGap == 0 )
	return;
    
    if (flipView) {
	x = lineGap/2 + ((BOARD_SIZE-1)-col) * 
	  (squareSize + lineGap);
	y = lineGap/2 + row * (squareSize + lineGap);
    } else {
	x = lineGap/2 + col * (squareSize + lineGap);
	y = lineGap/2 + ((BOARD_SIZE-1)-row) * 
	  (squareSize + lineGap);
    }
    
  XDrawRectangle( xDisplay, xBoardWindow, gc, x, y,
				 squareSize+lineGap, squareSize+lineGap );
}
    
static void BlankSquare( x, y, color, piece )
	 int x, y, color;
	 ChessSquare piece;
{
#if HAVE_LIBXPM
  if ( useXPMs && useXPMsqs )
	XCopyArea(xDisplay, color ? xpmLightSquare : xpmDarkSquare,
			  xBoardWindow, (int) piece < (int) BlackPawn
			  ? wlPieceGC : blPieceGC, 0, 0,
			  squareSize, squareSize, x, y);
  else
	XFillRectangle(xDisplay, xBoardWindow,
				   color ? lightSquareGC : darkSquareGC,
		       x, y, squareSize, squareSize);
#else
  if ( useXIMs && useXIMsqs )
#ifdef SLOW_IMAGE	
	XPutImage( xDisplay, xBoardWindow, color ? lightSquareGC : darkSquareGC,
			  color ? ximLightSquare : ximDarkSquare, 0, 0, x, y,
			  squareSize, squareSize );
#else
	XCopyArea(xDisplay, color ? xpmLightSquare : xpmDarkSquare,
			  xBoardWindow, (int) piece < (int) BlackPawn
			  ? wlPieceGC : blPieceGC, 0, 0,
			  squareSize, squareSize, x, y);
#endif  
  else
	XFillRectangle(xDisplay, xBoardWindow,
				   color ? lightSquareGC : darkSquareGC,
				   x, y, squareSize, squareSize);
#endif  
}

/*
   I split out the routines to draw a piece so that I could
   make a generic flash routine.
*/
static void monoDrawPiece_1bit( piece, square_color, x, y )
	 ChessSquare piece;
	 int square_color, x, y;
{
	    /* Avoid XCopyPlane on 1-bit screens to work around Sun bug */
	    if (square_color)
	      XCopyArea(xDisplay, (int) piece < (int) BlackPawn
			 ? *pieceToOutline(piece)
			 : *pieceToSolid(piece),
			 xBoardWindow, bwPieceGC, 0, 0,
			 squareSize, squareSize, x, y);
	    else
	      XCopyArea(xDisplay, (int) piece < (int) BlackPawn
			 ? *pieceToSolid(piece)
			 : *pieceToOutline(piece),
			 xBoardWindow, wbPieceGC, 0, 0,
			 squareSize, squareSize, x, y);
}

static void monoDrawPiece( piece, square_color, x, y )
	 ChessSquare piece;
	 int square_color, x, y;
{
	    if (square_color)
	      XCopyPlane(xDisplay, (int) piece < (int) BlackPawn
			 ? *pieceToOutline(piece)
			 : *pieceToSolid(piece),
			 xBoardWindow, bwPieceGC, 0, 0,
			 squareSize, squareSize, x, y, 1);
	    else
	      XCopyPlane(xDisplay, (int) piece < (int) BlackPawn
			 ? *pieceToSolid(piece)
			 : *pieceToOutline(piece),
			 xBoardWindow, wbPieceGC, 0, 0,
			 squareSize, squareSize, x, y, 1);
}

static void colorDrawPiece( piece, square_color, x, y )
	 ChessSquare piece;
	 int square_color, x, y;
{
  if (square_color) {
	  XCopyPlane(xDisplay, *pieceToSolid(piece),
		     xBoardWindow, (int) piece < (int) BlackPawn
		     ? wlPieceGC : blPieceGC, 0, 0,
		     squareSize, squareSize, x, y, 1);
  } else {
	  XCopyPlane(xDisplay, *pieceToSolid(piece),
		     xBoardWindow, (int) piece < (int) BlackPawn
		     ? wdPieceGC : bdPieceGC, 0, 0,
		     squareSize, squareSize, x, y, 1);
    }
}

#if HAVE_LIBXPM
static void colorDrawPiece_xpm( piece, square_color, x, y )
	 ChessSquare piece;
	 int square_color, x, y;
{
  int kind;

  if ( square_color ) {
	if ( (int)piece < (int) BlackPawn )
	  kind = 0;
	else {
	  kind = 2;
	  piece -= BlackPawn;
	}
  }
  else {
	if ( (int)piece < (int) BlackPawn )
	  kind = 1;
	else {
	  kind = 3;
	  piece -= BlackPawn;
	}
  }

  XCopyArea(xDisplay, xpmPieceBitmap[kind][piece],
			xBoardWindow, (int) piece < (int) BlackPawn
			? wlPieceGC : blPieceGC, 0, 0,
			squareSize, squareSize, x, y);		
}
#else /* !HAVE_LIBXPM */
static void colorDrawPiece_xim( piece, square_color, x, y )
	 ChessSquare piece;
	 int square_color, x, y;
{
  int kind;

  if ( square_color ) {
	if ( (int)piece < (int) BlackPawn )
	  kind = 0;
	else {
	  kind = 2;
	  piece -= BlackPawn;
	}
  }
  else {
	if ( (int)piece < (int) BlackPawn )
	  kind = 1;
	else {
	  kind = 3;
	  piece -= BlackPawn;
	}
  }

#ifdef SLOW_IMAGE  
  XPutImage(xDisplay, xBoardWindow, (int) piece < (int) BlackPawn
			? wlPieceGC : blPieceGC, ximPieceBitmap[kind][piece],
			0, 0, x, y, squareSize, squareSize );
#else
  XCopyArea(xDisplay, xpmPieceBitmap[kind][piece],
			xBoardWindow, (int) piece < (int) BlackPawn
			? wlPieceGC : blPieceGC, 0, 0,
			squareSize, squareSize, x, y);
#endif  
}
#endif /* HAVE_LIBXPM */

void DrawSquare(row, column, piece, do_flash )
     int row, column, do_flash;
     ChessSquare piece;
{
    int square_color, x, y, direction, font_ascent, font_descent;
	int i;
    char string[2];
    XCharStruct overall;
    void (*drawfunc)();
	int flash_delay;

	/* Calculate delay in milliseconds (2-delays per complete flash) */
	flash_delay = 500 / appData.flashRate;
	
    if (flipView) {
	x = lineGap + ((BOARD_SIZE-1)-column) * 
	  (squareSize + lineGap);
	y = lineGap + row * (squareSize + lineGap);
    } else {
	x = lineGap + column * (squareSize + lineGap);
	y = lineGap + ((BOARD_SIZE-1)-row) * 
	  (squareSize + lineGap);
  }
    
    square_color = ((column + row) % 2) == 1;
    
    if (piece == EmptySquare) {
	  BlankSquare( x, y, square_color, piece );
    } else if (appData.monoMode) {
	if (DefaultDepth(xDisplay, xScreen) == 1) {
	  drawfunc = monoDrawPiece_1bit;
	} else {
	  drawfunc = monoDrawPiece;
	  }
    } else {
#if HAVE_LIBXPM
	  if ( useXPMs == 1 )
		drawfunc = colorDrawPiece_xpm;
	  else
		drawfunc = colorDrawPiece;
#else /* !HAVE_LIBXPM */
	  if ( useXIMs == 1 )
		drawfunc = colorDrawPiece_xim;
	  else
		drawfunc = colorDrawPiece;
#endif	/* HAVE_LIBXPM */	  
	}

	if ( piece != EmptySquare )
	  {
		if ( do_flash ) {
		  for( i=0; i<appData.flashCount; ++i ) {

			drawfunc( piece, square_color, x, y );
			XSync( xDisplay, False );
			do_flash_delay( flash_delay );

			BlankSquare( x, y, square_color, piece );
			XSync( xDisplay, False );
			do_flash_delay( flash_delay );
		  }
		}

		drawfunc( piece, square_color, x, y );
	  }
	
    string[1] = NULLCHAR;
    if (appData.showCoords && row == (flipView ? 7 : 0)) {
	string[0] = 'a' + column;
	XTextExtents(coordFontStruct, string, 1, &direction, 
		     &font_ascent, &font_descent, &overall);
	if (appData.monoMode) {
	    XDrawImageString(xDisplay, xBoardWindow, coordGC,
			     x + squareSize - overall.width - 2, 
			     y + squareSize - font_descent - 1, string, 1);
	} else {
	    XDrawString(xDisplay, xBoardWindow, coordGC,
			x + squareSize - overall.width - 2, 
			y + squareSize - font_descent - 1, string, 1);
	}
    }
    if (appData.showCoords && column == (flipView ? 7 : 0)) {
	string[0] = '1' + row;
	XTextExtents(coordFontStruct, string, 1, &direction, 
		     &font_ascent, &font_descent, &overall);
	if (appData.monoMode) {
	    XDrawImageString(xDisplay, xBoardWindow, coordGC,
			     x + 2, y + font_ascent + 1, string, 1);
	} else {
	    XDrawString(xDisplay, xBoardWindow, coordGC,
			x + 2, y + font_ascent + 1, string, 1);
	}	    
    }   
  }

/* Why is this needed on some versions of X? */
void EventProc(widget, unused, event)
     Widget widget;
     caddr_t unused;
     XEvent *event;
{
    if (!XtIsRealized(widget))
      return;

    switch (event->type) {
      case Expose:
	DrawPositionProc(widget, event, NULL, NULL);
	break;
      default:
	return;
    }
}
/* end why */

void DrawPosition(fullRedraw, board)
     /*Boolean*/int fullRedraw;
     Board board;
{
    XDrawPosition(boardWidget, fullRedraw, board);
}

/* Returns 1 if there are "too many" differences between b1 and b2
   (i.e. more than 1 move was made) */
static int too_many_diffs( b1, b2 )
	 Board b1, b2;
{
  int i, j;
  int c = 0;
  
  for( i=0; i<BOARD_SIZE; ++i ) {
	for( j=0; j<BOARD_SIZE; ++j ) {
	  if ( b1[i][j] != b2[i][j] ) {
		if ( ++c > 4 ) /* Castling causes 4 diffs */
		  return 1;
	  }
	}
  }

  return 0;
}

/* Matrix describing castling maneuvers */
/* Row, ColRookFrom, ColKingFrom, ColRookTo, ColKingTo */
static int castling_matrix[4][5] = {
  { 0, 0, 4, 3, 2 },	/* 0-0-0, white */
  { 0, 7, 4, 5, 6 },	/* 0-0,   white */
  { 7, 0, 4, 3, 2 },	/* 0-0-0, black */
  { 7, 7, 4, 5, 6 }		/* 0-0,   black */
};

/* Checks whether castling occurred. If it did, *rrow and *rcol
   are set to the destination (row,col) of the rook that moved.

   Returns 1 if castling occurred, 0 if not.

   Note: Only handles a max of 1 castling move, so be sure
   to call too_many_diffs() first.
*/
static int check_castle_draw( newb, oldb, rrow, rcol )
	 Board newb, oldb;
	 int *rrow, *rcol;
{
  int i, *r, j;
  int match;

  /* For each type of castling... */
  for( i=0; i<4; ++i ) {
	r = castling_matrix[i];

	match = 0;

	/* Check the 4 squares involved in the castling move */
	for( j=1; j<=4; ++j ) {
	  if ( newb[r[0]][r[j]] == oldb[r[0]][r[j]] ) {
		match = 1;
		break;
	  }
	}

	if ( !match ) {	/* All 4 changed, so it must be a castling move */
	  *rrow = r[0];
	  *rcol = r[3];
	  return 1;
	}
  }

  return 0;
}

/*
 * event handler for redrawing the board
 */
void XDrawPosition(w, repaint, board)
     Widget w;
     /*Boolean*/int repaint;
     Board board;
{
    int i, j, do_flash;
    static int lastFlipView = 0;
    static int lastBoardValid = 0;
    static Board lastBoard;
    Arg args[16];
    int rrow, rcol;
    
    if (board == NULL) {
	if (!lastBoardValid) return;
	board = lastBoard;
    }

    /*
     * It would be simpler to clear the window with XClearWindow()
     * but this causes a very distracting flicker.
     */
    
    if (!repaint && lastBoardValid && lastFlipView == flipView) {

	/* If too much changes (begin observing new game, etc.), don't
	   do flashing */
	do_flash = too_many_diffs( board, lastBoard ) ? 0 : 1;

	/* Special check for castling so we don't flash both the king
	   and the rook (just flash the king). */
	if ( do_flash ) {
		if( check_castle_draw( board, lastBoard, &rrow, &rcol ) ) {
		  /* Draw rook with NO flashing. King will be drawn flashing later */
		  DrawSquare( rrow, rcol, board[rrow][rcol], 0 );
		  lastBoard[rrow][rcol] = board[rrow][rcol];
		}
	  }

	/* First pass -- Draw (newly) empty squares. This prevents
	   you from having a piece show up twice while it is flashing
	   on its new square */
	for (i = 0; i < BOARD_SIZE; i++)
	  for (j = 0; j < BOARD_SIZE; j++)
	    if (board[i][j] != lastBoard[i][j] && board[i][j] == EmptySquare )
	      DrawSquare(i, j, board[i][j], 0);

	/* Second pass -- Draw piece(s) in new position and flash them */
	for (i = 0; i < BOARD_SIZE; i++)
	  for (j = 0; j < BOARD_SIZE; j++)
	    if (board[i][j] != lastBoard[i][j])
	      DrawSquare(i, j, board[i][j], do_flash);	  
    } else {
	if (lineGap > 0)
	  XDrawSegments(xDisplay, xBoardWindow, lineGC,
			gridSegments, (BOARD_SIZE + 1) * 2);
	
	for (i = 0; i < BOARD_SIZE; i++)
	  for (j = 0; j < BOARD_SIZE; j++)
	    DrawSquare(i, j, board[i][j], 0);
    }
    
    if (!lastBoardValid || lastFlipView != flipView) {
	XtSetArg(args[0], XtNleftBitmap, (flipView ? xMarkPixmap : None));
	XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Flip View"),
		    args, 1);
    }

    CopyBoard(lastBoard, board);
    lastBoardValid = 1;
    lastFlipView = flipView;
    
    XSync(xDisplay, False);
}

/*
 * event handler for redrawing the board
 */
void DrawPositionProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    XDrawPosition(w, True, NULL);
}


/*
 * event handler for parsing user moves
 */

/* This version handles the Click-Click logic */
static void HandleUserMove_clickClick(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    int x, y;
    static int waiting_promote = 0;
	
    if (w != boardWidget)
      return;

    if (promotionUp) {
	XtPopdown(promotionShell);
	XtDestroyWidget(promotionShell);
	promotionUp = False;
	drawHighlight( fromY, fromX, lineGC );
	drawHighlight( toY, toX, lineGC );
	fromX = fromY = -1;
    }
    
    x = EventToSquare(event->xbutton.x);
    y = EventToSquare(event->xbutton.y);
    if (!flipView && y >= 0) {
	y = BOARD_SIZE - 1 - y;
    }
    if (flipView && x >= 0) {
	x = BOARD_SIZE - 1 - x;
    }

    switch (event->type) {
      case ButtonPress:
	if (errorExitStatus != -1) return;
	if (errorUp) ErrorPopDown();

	if ( fromX == -1 )	/* First square? */
	  {
	      if (OKToStartUserMove(x, y)) {
		  fromX = x;
		  fromY = y;
		  drawHighlight( y, x, highlineGC );
	      } 
	  }
	else if ( x == fromX && y == fromY )
	  {
	      /* Clicked again on first square -- reset */
	      drawHighlight( fromY, fromX, lineGC );
	      fromX = fromY = -1;
	  }
	else
	  {
	      toX = x;
	      toY = y;
	      drawHighlight( toY, toX, highlineGC );

	      if (IsPromotion(fromX, fromY, toX, toY)) {
		  if (appData.alwaysPromoteToQueen) {
		      UserMoveEvent(fromX, fromY, toX, toY, 'q');
		      drawHighlight( fromY, fromX, lineGC );
		      drawHighlight( toY, toX, lineGC );
		      fromX = fromY = -1;
		  } else {
		      waiting_promote = 1;
		  }
	      } else {
		  UserMoveEvent(fromX, fromY, toX, toY, NULLCHAR);
		  drawHighlight( fromY, fromX, lineGC );
		  drawHighlight( toY, toX, lineGC );
		  fromX = fromY = -1;
	      }
	  }
	break;

      case ButtonRelease:
	/* The promotion popup is unhappy about popping up when
	   the button is still down. So, we wait. */
	if (waiting_promote) {
	    waiting_promote = 0;
	    PromotionPopUp();
	}
	break;
    }
}

void HandleUserMove(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    int x, y;
    
    if (w != boardWidget)
      return;
    
    if ( appData.clickClick == True ) {
	HandleUserMove_clickClick(w, event, prms, nprms);
	return;
    }
	
    if (promotionUp) {
	XtPopdown(promotionShell);
	XtDestroyWidget(promotionShell);
	promotionUp = False;
	fromX = fromY = -1;
    }
    
    x = EventToSquare(event->xbutton.x);
    y = EventToSquare(event->xbutton.y);
    if (!flipView && y >= 0) {
	y = BOARD_SIZE - 1 - y;
    }
    if (flipView && x >= 0) {
	x = BOARD_SIZE - 1 - x;
    }

    switch (event->type) {
      case ButtonPress:
	if (errorExitStatus != -1) return;
	if (errorUp) ErrorPopDown();
	if (OKToStartUserMove(x, y)) {
	    fromX = x;
	    fromY = y;
	} else {
	    fromX = fromY = -1;
	}
	break;

      case ButtonRelease:
	toX = x;
	toY = y;
	if (IsPromotion(fromX, fromY, toX, toY)) {
	    if (appData.alwaysPromoteToQueen) {
		UserMoveEvent(fromX, fromY, toX, toY, 'q');
		fromX = fromY = -1;
	    } else {
		PromotionPopUp();
	    }
	} else {
	    UserMoveEvent(fromX, fromY, toX, toY, NULLCHAR);
	    fromX = fromY = -1;
	}
	break;
    }

}

Widget CommentCreate(name, text, mutable, callback, nobuttons)
     char *name, *text;
     int /*Boolean*/ mutable;
     XtCallbackProc callback;
     int /*Boolean*/ nobuttons;
{
    Arg args[16];
    Widget shell, layout, form, edit, b_ok, b_cancel, b_clear, b_close, b_edit;
    Dimension fw_width;
    int j;

    j = 0;
    XtSetArg(args[j], XtNwidth, &fw_width);  j++;
    XtGetValues(formWidget, args, j);

    j = 0;
    XtSetArg(args[j], XtNresizable, True);  j++;
#if TOPLEVEL
    shell =
      XtCreatePopupShell(name, topLevelShellWidgetClass,
			 shellWidget, args, j);
#else
    shell =
      XtCreatePopupShell(name, transientShellWidgetClass,
			 shellWidget, args, j);
#endif
    layout =
      XtCreateManagedWidget(layoutName, formWidgetClass, shell,
			    layoutArgs, XtNumber(layoutArgs));
    form =
      XtCreateManagedWidget("form", formWidgetClass, layout,
			    formArgs, XtNumber(formArgs));

    j = 0;
    if (mutable) {
	XtSetArg(args[j], XtNeditType, XawtextEdit);  j++;
	XtSetArg(args[j], XtNuseStringInPlace, False);  j++;
    }
    XtSetArg(args[j], XtNstring, text);  j++;
    XtSetArg(args[j], XtNtop, XtChainTop);  j++;
    XtSetArg(args[j], XtNbottom, XtChainBottom);  j++;
    XtSetArg(args[j], XtNleft, XtChainLeft);  j++;
    XtSetArg(args[j], XtNright, XtChainRight);  j++;
    XtSetArg(args[j], XtNresizable, True);  j++;
    XtSetArg(args[j], XtNwidth, fw_width);  j++; /*force wider than buttons*/
    XtSetArg(args[j], XtNscrollVertical, XawtextScrollWhenNeeded);  j++;
    XtSetArg(args[j], XtNautoFill, True);  j++;
    edit =
      XtCreateManagedWidget("text", asciiTextWidgetClass, form, args, j);

    if (mutable && nobuttons == False ) {
	j = 0;
	XtSetArg(args[j], XtNfromVert, edit);  j++;
	XtSetArg(args[j], XtNtop, XtChainBottom); j++;
	XtSetArg(args[j], XtNbottom, XtChainBottom); j++;
	XtSetArg(args[j], XtNleft, XtChainLeft); j++;
	XtSetArg(args[j], XtNright, XtChainLeft); j++;
	b_ok =
	  XtCreateManagedWidget("ok", commandWidgetClass, form, args, j);
	XtAddCallback(b_ok, XtNcallback, callback, (XtPointer) 0);

	j = 0;
	XtSetArg(args[j], XtNfromVert, edit);  j++;
	XtSetArg(args[j], XtNfromHoriz, b_ok);  j++;
	XtSetArg(args[j], XtNtop, XtChainBottom); j++;
	XtSetArg(args[j], XtNbottom, XtChainBottom); j++;
	XtSetArg(args[j], XtNleft, XtChainLeft); j++;
	XtSetArg(args[j], XtNright, XtChainLeft); j++;
	b_cancel =
	  XtCreateManagedWidget("cancel", commandWidgetClass, form, args, j);
	XtAddCallback(b_cancel, XtNcallback, callback, (XtPointer) 0);

	j = 0;
	XtSetArg(args[j], XtNfromVert, edit);  j++;
	XtSetArg(args[j], XtNfromHoriz, b_cancel);  j++;
	XtSetArg(args[j], XtNtop, XtChainBottom); j++;
	XtSetArg(args[j], XtNbottom, XtChainBottom); j++;
	XtSetArg(args[j], XtNleft, XtChainLeft); j++;
	XtSetArg(args[j], XtNright, XtChainLeft); j++;
	b_clear =
	  XtCreateManagedWidget("clear", commandWidgetClass, form, args, j);
	XtAddCallback(b_clear, XtNcallback, callback, (XtPointer) 0);
    } else if ( nobuttons == False ) {
	j = 0;
	XtSetArg(args[j], XtNfromVert, edit);  j++;
	XtSetArg(args[j], XtNtop, XtChainBottom); j++;
	XtSetArg(args[j], XtNbottom, XtChainBottom); j++;
	XtSetArg(args[j], XtNleft, XtChainLeft); j++;
	XtSetArg(args[j], XtNright, XtChainLeft); j++;
	b_close =
	  XtCreateManagedWidget("close", commandWidgetClass, form, args, j);
	XtAddCallback(b_close, XtNcallback, callback, (XtPointer) 0);

	j = 0;
	XtSetArg(args[j], XtNfromVert, edit);  j++;
	XtSetArg(args[j], XtNfromHoriz, b_close);  j++;
	XtSetArg(args[j], XtNtop, XtChainBottom); j++;
	XtSetArg(args[j], XtNbottom, XtChainBottom); j++;
	XtSetArg(args[j], XtNleft, XtChainLeft); j++;
	XtSetArg(args[j], XtNright, XtChainLeft); j++;
	b_edit =
	  XtCreateManagedWidget("edit", commandWidgetClass, form, args, j);
	XtAddCallback(b_edit, XtNcallback, callback, (XtPointer) 0);
    }

    if (commentX == -1) {
	Position y1, y2;
	int xx, yy;
	Window junk;

	j = 0;
	XtSetArg(args[j], XtNy, &y1); j++;
	XtGetValues(menuBarWidget, args, j);
	y1 -= appData.borderYoffset; /* offset by banner height */
	j = 0;
	XtSetArg(args[j], XtNy, &y2); j++;
	XtGetValues(messageWidget, args, j);
	commentW = fw_width - 16;
	commentH = y2 - y1;

	XSync(xDisplay, False);
#ifdef NOTDEF
	/* This code seems to tickle an X bug if it is executed too soon
	   after xboard starts up.  The coordinates get transformed as if
	   the main window was positioned at (0, 0).
	   */
	XtTranslateCoords(shellWidget, (fw_width - commentW) / 2, y1,
			  &commentX, &commentY);
#else  /*!NOTDEF*/
        XTranslateCoordinates(xDisplay, XtWindow(shellWidget),
			      RootWindowOfScreen(XtScreen(shellWidget)),
			      (fw_width - commentW) / 2, y1,
			      &xx, &yy, &junk);
	commentX = xx;
	commentY = yy;
#endif /*!NOTDEF*/
    }
    j = 0;
    XtSetArg(args[j], XtNheight, commentH);  j++;
    XtSetArg(args[j], XtNwidth, commentW);  j++;
    XtSetArg(args[j], XtNx, commentX - appData.borderXoffset);  j++;
    XtSetArg(args[j], XtNy, commentY - appData.borderYoffset);  j++;
    XtSetValues(shell, args, j);

    XtRealizeWidget(shell);

    return shell;
}

static int savedIndex;  /* gross that this is global */

void EditCommentPopUp(index, title, text)
     int index;
     char *title, *text;
{
    Widget edit;
    Arg args[16];
    int j;

    savedIndex = index;
    if (text == NULL) text = "";

    if (editShell == NULL) {
	editShell =
	  CommentCreate(title, text, True, EditCommentCallback, False); 
    } else {
	edit = XtNameToWidget(editShell, "*form.text");
	j = 0;
	XtSetArg(args[j], XtNstring, text); j++;
	XtSetValues(edit, args, j);
	j = 0;
	XtSetArg(args[j], XtNiconName, (XtArgVal) title);   j++;
	XtSetArg(args[j], XtNtitle, (XtArgVal) title);      j++;
	XtSetValues(editShell, args, j);
    }

    XtPopup(editShell, XtGrabNone);
    XtSetKeyboardFocus(shellWidget, editShell);

    editUp = True;
    j = 0;
    XtSetArg(args[j], XtNleftBitmap, xMarkPixmap); j++;
    XtSetValues(XtNameToWidget(menuBarWidget, "menuMode.Edit Comment"),
		args, j);
}

void EditCommentCallback(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    String name, val;
    Arg args[16];
    int j;
    Widget edit;

    j = 0;
    XtSetArg(args[j], XtNlabel, &name);  j++;
    XtGetValues(w, args, j);

    if (strcmp(name, "ok") == 0) {
	edit = XtNameToWidget(editShell, "*form.text");
	j = 0;
	XtSetArg(args[j], XtNstring, &val); j++;
	XtGetValues(edit, args, j);
	ReplaceComment(savedIndex, val);
	EditCommentPopDown();
    } else if (strcmp(name, "cancel") == 0) {
	EditCommentPopDown();
    } else if (strcmp(name, "clear") == 0) {
	edit = XtNameToWidget(editShell, "*form.text");
	XtCallActionProc(edit, "select-all", NULL, NULL, 0);
	XtCallActionProc(edit, "kill-selection", NULL, NULL, 0);
    }
}

void EditCommentPopDown()
{
    Arg args[16];
    int j;

    if (!editUp) return;
    j = 0;
    XtSetArg(args[j], XtNx, &commentX); j++;
    XtSetArg(args[j], XtNy, &commentY); j++;
    XtSetArg(args[j], XtNheight, &commentH); j++;
    XtSetArg(args[j], XtNwidth, &commentW); j++;
    XtGetValues(editShell, args, j);
    XtPopdown(editShell);
    XtSetKeyboardFocus(shellWidget, formWidget);
    editUp = False;
    j = 0;
    XtSetArg(args[j], XtNleftBitmap, None); j++;
    XtSetValues(XtNameToWidget(menuBarWidget, "menuMode.Edit Comment"),
		args, j);
}

void ICSInputBoxPopUp()
{
    Widget edit;
    Arg args[16];
    int j;
    char *title = "ICS Input";
    XtTranslations tr;
	
    if (ICSInputShell == NULL) {
	ICSInputShell =
	  CommentCreate(title, "", True, NULL, True);
	tr = XtParseTranslationTable(ICSInputTranslations);
	edit = XtNameToWidget(ICSInputShell, "*form.text");
	XtOverrideTranslations(edit, tr);
	
    } else {
	edit = XtNameToWidget(ICSInputShell, "*form.text");
	j = 0;
	XtSetArg(args[j], XtNstring, ""); j++;
	XtSetValues(edit, args, j);
	j = 0;
	XtSetArg(args[j], XtNiconName, (XtArgVal) title);   j++;
	XtSetArg(args[j], XtNtitle, (XtArgVal) title);      j++;
	XtSetValues(ICSInputShell, args, j);
    }

    XtPopup(ICSInputShell, XtGrabNone);
    XtSetKeyboardFocus(shellWidget, ICSInputShell);

    ICSInputBoxUp = True;
    j = 0;
    XtSetArg(args[j], XtNleftBitmap, xMarkPixmap); j++;
    XtSetValues(XtNameToWidget(menuBarWidget, "menuMode.ICS Input Box"),
		args, j);
}

void ICSInputSendText()
{
    Widget edit;
    int j;
    Arg args[16];
    String val;
  
    edit = XtNameToWidget(ICSInputShell, "*form.text");
    j = 0;
    XtSetArg(args[j], XtNstring, &val); j++;
    XtGetValues(edit, args, j);
    SendMultiLineToICS( val );
    XtCallActionProc(edit, "select-all", NULL, NULL, 0);
    XtCallActionProc(edit, "kill-selection", NULL, NULL, 0);
}

void ICSInputBoxPopDown()
{
    Arg args[16];
    int j;

    if (!ICSInputBoxUp) return;
    j = 0;
    XtSetArg(args[j], XtNx, &commentX); j++;
    XtSetArg(args[j], XtNy, &commentY); j++;
    XtSetArg(args[j], XtNheight, &commentH); j++;
    XtSetArg(args[j], XtNwidth, &commentW); j++;
    XtGetValues(ICSInputShell, args, j);
    XtPopdown(ICSInputShell);
    XtSetKeyboardFocus(shellWidget, formWidget);
    ICSInputBoxUp = False;
    j = 0;
    XtSetArg(args[j], XtNleftBitmap, None); j++;
    XtSetValues(XtNameToWidget(menuBarWidget, "menuMode.ICS Input Box"),
		args, j);
}

void CommentPopUp(title, text)
     char *title, *text;
{
    Arg args[16];
    int j;
    Widget edit;

    if (commentShell == NULL) {
	commentShell =
	  CommentCreate(title, text, False, CommentCallback, False);
    } else {
	edit = XtNameToWidget(commentShell, "*form.text");
	j = 0;
	XtSetArg(args[j], XtNstring, text); j++;
	XtSetValues(edit, args, j);
	j = 0;
	XtSetArg(args[j], XtNiconName, (XtArgVal) title);   j++;
	XtSetArg(args[j], XtNtitle, (XtArgVal) title);      j++;
	XtSetValues(commentShell, args, j);
    }

    XtPopup(commentShell, XtGrabNone);
    XSync(xDisplay, False);

    commentUp = True;
}

/* Copy next "line" of text from src to dest and return a pointer
   to next char of src. A "line" ends when: (a) The text is "maxwidth"
   wide, (b) end-of-string, or (c) '\n' found. Routine will only break
   lines at whitespace. If no break point found, routine simply
   copies src to dest. */
static char *nextStringPart( dest, src, font, maxwidth )
  char *dest, *src;
  XFontStruct *font;
  int maxwidth;
{
  int match_len = 0, srci = 0, desti = 0, skipped = 0;

  /* Skip leading whitespace */
  while( isspace(src[srci]) ) {
    ++srci;
    ++skipped;
  }
  
  do {
    /* Copy in next piece until whitespace, end-of-line
     or end-of-string */
    while( src[srci] && src[srci] != '\n' && !isspace(src[srci]) )
      dest[desti++] = src[srci++];

    dest[desti] = '\n';
    dest[desti+1] = 0;

    /* Does this make the string too wide? */
    if ( XTextWidth( font, dest, desti ) > maxwidth )
      {
	/* Yes, discard last segment and return */
	dest[match_len] = '\n';
	dest[match_len+1] = 0;

	/* If user has made window so small that no text can
	   fit in it, don't do any line breaking (caller would
	   loop endlessly). Copy src to dest and return */
	if ( match_len == 0 ) {
	  strcpy( dest, src );
	  return (src+strlen(src));
	}
	
	return (src + match_len + skipped);
      }

    /* Not too wide - keep segment */
    match_len = desti;

    /* End of string or end of line? */
    if ( !src[srci] )
      return (src + srci);
    if ( src[srci] == '\n' )
      return (src + srci + 1);
    
    /* Copy extra whitespace in (we don't care if this gets clipped off
       by the window) */
    while( src[srci] && isspace( src[srci] ) ) {
      dest[desti++] = src[srci++];
      ++match_len;
    }
  } while( 1 );
}

/* Reformat (word wrap) text from src to dest using the given
   font and max line width (in pixels). See above routine
   for rules on line wrapping. */
static void ReformatAnalysisText( dest, src, font, width )
  char *dest, *src;
  XFontStruct *font;
  int width;
{
  char buf[MSG_SIZ];
  
  dest[0] = 0;
  
  while( *src ) {
    src = nextStringPart( buf, src, font, width );
    strcat( dest, buf );
  }
}

void AnalysisPopUp(title, text)
     char *title, *text;
{
    Arg args[16];
    int j;
    Widget edit;
    int w;
    XFontStruct *font;
    char rtext[MSG_SIZ];

    if (analysisShell == NULL) {
	analysisShell =
	  CommentCreate(title, text, False, NULL, True);
    } else {
	edit = XtNameToWidget(analysisShell, "*form.text");

	/* Get width of edit window and font to use */
	j = 0;
	XtSetArg(args[j], XtNwidth, &w); j++;
	XtSetArg(args[j], XtNfont, &font); j++;
	XtGetValues(edit, args, j);

	if ( appData.debugMode ) {
	  printf("WIDTH=%d, TEXTWIDTH=%d\n", w,
		 XTextWidth(font, text, strlen(text)) );
	}
	
	/* Reformat text (word wrap) to fit correctly in window */
	ReformatAnalysisText( rtext, text, font, w );
	
	j = 0;
	XtSetArg(args[j], XtNstring, rtext); j++;
	XtSetValues(edit, args, j);
	j = 0;
	XtSetArg(args[j], XtNiconName, (XtArgVal) title);   j++;
	XtSetArg(args[j], XtNtitle, (XtArgVal) title);      j++;
	XtSetValues(analysisShell, args, j);
    }

    XtPopup(analysisShell, XtGrabNone);
    XSync(xDisplay, False);

    analysisUp = True;
}

void CommentCallback(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    String name;
    Arg args[16];
    int j;

    j = 0;
    XtSetArg(args[j], XtNlabel, &name);  j++;
    XtGetValues(w, args, j);

    if (strcmp(name, "close") == 0) {
	CommentPopDown();
    } else if (strcmp(name, "edit") == 0) {
	CommentPopDown();
	EditCommentEvent();
    }
}


void CommentPopDown()
{
    Arg args[16];
    int j;

    if (!commentUp) return;
    j = 0;
    XtSetArg(args[j], XtNx, &commentX); j++;
    XtSetArg(args[j], XtNy, &commentY); j++;
    XtSetArg(args[j], XtNwidth, &commentW); j++;
    XtSetArg(args[j], XtNheight, &commentH); j++;
    XtGetValues(commentShell, args, j);
    XtPopdown(commentShell);
    XSync(xDisplay, False);
    commentUp = False;
}

void AnalysisPopDown()
{
    if (!analysisUp) return;
    XtPopdown(analysisShell);
    XSync(xDisplay, False);
    analysisUp = False;
}


void FileNamePopUp(label, def, proc, openMode)
     char *label;
     char *def;
     FileProc proc;
     char *openMode;
{
    Arg args[16];
    Widget popup, layout, dialog;
    Window root, child;
    int x, y, i;
    int win_x, win_y;
    unsigned int mask;
    
    fileProc = proc;          /* I can't see a way not */
    fileOpenMode = openMode;  /*   to use globals here */
    
    i = 0;
    XtSetArg(args[i], XtNresizable, True); i++;
    XtSetArg(args[i], XtNwidth, DIALOG_SIZE); i++;
    popup =
      XtCreatePopupShell("File Name Prompt", transientShellWidgetClass,
			 shellWidget, args, i);
    
    layout =
      XtCreateManagedWidget(layoutName, formWidgetClass, popup,
			    layoutArgs, XtNumber(layoutArgs));
  
    i = 0;
    XtSetArg(args[i], XtNlabel, label); i++;
    XtSetArg(args[i], XtNvalue, def); i++;
    XtSetArg(args[i], XtNborderWidth, 0); i++;
    dialog = XtCreateManagedWidget("dialog", dialogWidgetClass,
				   layout, args, i);
    
    XawDialogAddButton(dialog, "ok", FileNameCallback, (XtPointer) dialog);
    XawDialogAddButton(dialog, "cancel", FileNameCallback,
		       (XtPointer) dialog);
    
    XtRealizeWidget(popup);
    
    XQueryPointer(xDisplay, xBoardWindow, &root, &child,
		  &x, &y, &win_x, &win_y, &mask);
    
    XtSetArg(args[0], XtNx, x - 10);
    XtSetArg(args[1], XtNy, y - 10);
    XtSetValues(popup, args, 2);
    
    XtPopup(popup, XtGrabExclusive);
    filenameUp = True;
    
    XtSetKeyboardFocus(shellWidget, popup);
}

void FileNameCallback(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    String name;
    Arg args[16];
    
    XtSetArg(args[0], XtNlabel, &name);
    XtGetValues(w, args, 1);
    
    if (strcmp(name, "cancel") == 0) {
	XtPopdown(w = XtParent(XtParent(XtParent(w))));
	XtDestroyWidget(w);
	filenameUp = False;
	ModeHighlight();
	return;
    }
    
    FileNameAction(w, NULL, NULL, NULL);
}

void FileNameAction(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    char buf[MSG_SIZ];
    String name;
    FILE *f;
    char *p, *fullname;
    int index;

    name = XawDialogGetValueString(w = XtParent(w));
    
    if ((name != NULL) && (*name != NULLCHAR)) {
	strcpy(buf, name);
	XtPopdown(w = XtParent(XtParent(w)));
	XtDestroyWidget(w);
	filenameUp = False;

	p = strrchr(buf, ' ');
	if (p == NULL) {
	    index = 0;
	} else {
	    *p++ = NULLCHAR;
	    index = atoi(p);
	}
	fullname = ExpandPathName(buf);
	if ( !fullname ) {
	  ErrorPopUp("Error", "Can't open file" );
	}
	else {
	  f = fopen( fullname, fileOpenMode );
	if (f == NULL) {
	    DisplayError("Failed to open file", errno);
	} else {
	    (void) (*fileProc)(f, index, buf);
	}
	}
	ModeHighlight();
	XtSetKeyboardFocus(shellWidget, formWidget);
	return;
    }
    
    XtPopdown(w = XtParent(XtParent(w)));
    XtDestroyWidget(w);
    filenameUp = False;
    ModeHighlight();
    XtSetKeyboardFocus(shellWidget, formWidget);
}

void PromotionPopUp()
{
    Arg args[16];
    Widget dialog, layout;
    Position x, y;
    Dimension bw_width, pw_width;
    int j;

    j = 0;
    XtSetArg(args[j], XtNwidth, &bw_width); j++;
    XtGetValues(boardWidget, args, j);
    
    j = 0;
    XtSetArg(args[j], XtNresizable, True); j++;
    promotionShell =
      XtCreatePopupShell("Promotion", transientShellWidgetClass,
			 shellWidget, args, j);
    layout =
      XtCreateManagedWidget(layoutName, formWidgetClass, promotionShell,
			    layoutArgs, XtNumber(layoutArgs));
    
    j = 0;
    XtSetArg(args[j], XtNlabel, "Promote pawn to what?"); j++;
    XtSetArg(args[j], XtNborderWidth, 0); j++;
    dialog = XtCreateManagedWidget("promotion", dialogWidgetClass,
				   layout, args, j);
    
    XawDialogAddButton(dialog, "Queen", PromotionCallback, 
		       (XtPointer) dialog);
    XawDialogAddButton(dialog, "Rook", PromotionCallback, 
		       (XtPointer) dialog);
    XawDialogAddButton(dialog, "Bishop", PromotionCallback, 
		       (XtPointer) dialog);
    XawDialogAddButton(dialog, "Knight", PromotionCallback, 
		       (XtPointer) dialog);
    XawDialogAddButton(dialog, "cancel", PromotionCallback, 
		       (XtPointer) dialog);
    
    XtRealizeWidget(promotionShell);
    
    j = 0;
    XtSetArg(args[j], XtNwidth, &pw_width); j++;
    XtGetValues(promotionShell, args, j);
    
    XtTranslateCoords(boardWidget, (bw_width - pw_width) / 2,
		      lineGap + squareSize/3 +
		      ((toY == 7) ^ (flipView) ?
		       0 : 6*(squareSize + lineGap)), &x, &y);
    
    j = 0;
    XtSetArg(args[j], XtNx, x); j++;
    XtSetArg(args[j], XtNy, y); j++;
    XtSetValues(promotionShell, args, j);
    
    XtPopup(promotionShell, XtGrabNone);
    
    promotionUp = True;
}

void PromotionCallback(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    String name;
    Arg args[16];
    int promoChar;
    
    XtSetArg(args[0], XtNlabel, &name);
    XtGetValues(w, args, 1);
    
    XtPopdown(w = XtParent(XtParent(XtParent(w))));
    XtDestroyWidget(w);
    promotionUp = False;
    
    if (fromX == -1) return;
    
    if (strcmp(name, "cancel") == 0) {
	fromX = fromY = -1;
	return;
    } else if (strcmp(name, "Knight") == 0) {
	promoChar = 'n';
    } else {
	promoChar = ToLower(name[0]);
    }

    UserMoveEvent(fromX, fromY, toX, toY, promoChar);

    /* Need this in click-click mode so that the next click
       is recognized correctly */
    if ( appData.clickClick == True )
      fromX = fromY = -1;
}


void ErrorCallback(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    errorUp = False;
    XtPopdown(w = XtParent(XtParent(XtParent(w))));
    XtDestroyWidget(w);
    if (errorExitStatus != -1) ExitEvent(errorExitStatus);
}


void ErrorPopDown()
{
    if (!errorUp) return;
    errorUp = False;
    XtPopdown(errorShell);
    XtDestroyWidget(errorShell);
}

void ErrorPopUp(title, label)
     char *title, *label;
{
    Arg args[16];
    Widget dialog, layout;
    Position x, y;
    int xx, yy;
    Window junk;
    Dimension bw_width, pw_width;
    Dimension pw_height;
    int i;
    
    i = 0;
    XtSetArg(args[i], XtNresizable, True);  i++;
    errorShell = 
      XtCreatePopupShell(title, transientShellWidgetClass,
			 shellWidget, args, i);
    layout =
      XtCreateManagedWidget(layoutName, formWidgetClass, errorShell,
			    layoutArgs, XtNumber(layoutArgs));
    
    i = 0;
    XtSetArg(args[i], XtNlabel, label); i++;
    XtSetArg(args[i], XtNborderWidth, 0); i++;
    dialog = XtCreateManagedWidget("dialog", dialogWidgetClass,
				   layout, args, i);
    
    XawDialogAddButton(dialog, "ok", ErrorCallback, (XtPointer) dialog);
    
    XtRealizeWidget(errorShell);
    
    i = 0;
    XtSetArg(args[i], XtNwidth, &bw_width);  i++;
    XtGetValues(boardWidget, args, i);
    i = 0;
    XtSetArg(args[i], XtNwidth, &pw_width);  i++;
    XtSetArg(args[i], XtNheight, &pw_height);  i++;
    XtGetValues(errorShell, args, i);

#ifdef NOTDEF
    /* This code seems to tickle an X bug if it is executed too soon
       after xboard starts up.  The coordinates get transformed as if
       the main window was positioned at (0, 0).
    */
    XtTranslateCoords(boardWidget, (bw_width - pw_width) / 2,
		      0 - pw_height - appData.borderYoffset
		      + squareSize / 3, &x, &y);
#else
    XTranslateCoordinates(xDisplay, XtWindow(boardWidget),
			  RootWindowOfScreen(XtScreen(boardWidget)),
			  (bw_width - pw_width) / 2,
			  0 - pw_height - appData.borderYoffset
			  + squareSize / 3, &xx, &yy, &junk);
    x = xx;
    y = yy;
#endif

    i = 0;
    XtSetArg(args[i], XtNx, x);  i++;
    XtSetArg(args[i], XtNy, y);  i++;
    XtSetValues(errorShell, args, i);

    errorUp = True;
    XtPopup(errorShell, XtGrabNone);
}


char *ModeToWidgetName(mode)
     GameMode mode;
{
    switch (mode) {
      case BeginningOfGame:
	if (appData.icsActive)
	  return "menuMode.ICS Client";
	else if (appData.noChessProgram ||
		 *appData.cmailGameName != NULLCHAR)
	  return "menuMode.Edit Game";
	else
	  return "menuMode.Machine Black";
      case MachinePlaysBlack:
	return "menuMode.Machine Black";
      case MachinePlaysWhite:
	return "menuMode.Machine White";
      case AnalyzeMode:
	return "menuMode.Analysis Mode";
      case AnalyzeFile:
	return "menuMode.Analyze File";
      case TwoMachinesPlay:
	return "menuMode.Two Machines";
      case EditGame:
	return "menuMode.Edit Game";
      case PlayFromGameFile:
	return "menuFile.Load Game";
      case EditPosition:
	return "menuMode.Edit Position";
      case IcsPlayingWhite:
      case IcsPlayingBlack:
      case IcsObserving:
      case IcsIdle:
      case IcsExamining:
	return "menuMode.ICS Client";
      default:
      case EndOfGame:
	return NULL;
    }
}     

void ModeHighlight()
{
    Arg args[16];
    static int oldPausing = FALSE;
    static GameMode oldmode = (GameMode) -1;
    char *wname;
    
    if (pausing != oldPausing) {
	oldPausing = pausing;
	if (pausing) {
	    XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
	} else {
	    XtSetArg(args[0], XtNleftBitmap, None);
	}
	XtSetValues(XtNameToWidget(menuBarWidget, "menuMode.Pause"),
		    args, 1);

	if (pausing) {
	    XtSetArg(args[0], XtNbackground, buttonForegroundPixel);
	    XtSetArg(args[1], XtNforeground, buttonBackgroundPixel);
	} else {
	    XtSetArg(args[0], XtNbackground, buttonBackgroundPixel);
	    XtSetArg(args[1], XtNforeground, buttonForegroundPixel);
	}
	XtSetValues(XtNameToWidget(buttonBarWidget, PAUSE_BUTTON), args, 2);
    }

    wname = ModeToWidgetName(oldmode);
    if (wname != NULL) {
	XtSetArg(args[0], XtNleftBitmap, None);
	XtSetValues(XtNameToWidget(menuBarWidget, wname), args, 1);
    }
    wname = ModeToWidgetName(gameMode);
    if (wname != NULL) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
	XtSetValues(XtNameToWidget(menuBarWidget, wname), args, 1);
    }
    oldmode = gameMode;
}


/*
 * Button/menu procedures
 */
void ResetProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ResetGameEvent();
	AnalysisPopDown();
}

int LoadGamePopUp(f, gameNumber, title)
     FILE *f;
     int gameNumber;
     char *title;
{
    cmailMsgLoaded = FALSE;
    if (gameNumber == 0) {
	int error = GameListBuild(f);
	if (error) {
	    DisplayError("Cannot build game list", error);
	} else if (!ListEmpty(&gameList) &&
		   ((ListGame *) gameList.tailPred)->number > 1) {
	    GameListPopUp(f, title);
	    return TRUE;
	}
	GameListDestroy();
	gameNumber = 1;
    }
    return LoadGame(f, gameNumber, title, FALSE);
}

void LoadGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
  if ( gameMode == AnalyzeMode || gameMode == AnalyzeFile ) {
	ExitAnalyzeMode();
	Reset(FALSE);
  }

    FileNamePopUp("Load game file name?", "", LoadGamePopUp, "r");
}

void LoadNextGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ReloadGame(1);
}

void LoadPrevGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ReloadGame(-1);
}

void ReloadGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ReloadGame(0);
}

void LoadPositionProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
  if ( gameMode == AnalyzeMode || gameMode == AnalyzeFile ) {
	ExitAnalyzeMode();
	Reset(FALSE);
  }
  
    FileNamePopUp("Load position file name?", "", LoadPosition, "r");
}

void SaveGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    FileNamePopUp("Save game file name?",
		  DefaultFileName(appData.oldSaveStyle ? "game" : "pgn"),
		  SaveGame, "a");
}

void SavePositionProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    FileNamePopUp("Save position file name?",
		  DefaultFileName(appData.oldSaveStyle ? "pos" : "fen"),
		  SavePosition, "a");
}

void ReloadCmailMsgProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ReloadCmailMsgEvent(FALSE);
}

void MailMoveProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    MailMoveEvent();
}

void AutoSaveGame()
{
    SaveGameProc(NULL, NULL, NULL, NULL);
}


void QuitProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ExitEvent(0);
}

void PauseProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    PauseEvent();
}


void MachineBlackProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    MachineBlackEvent();
}

void MachineWhiteProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    MachineWhiteEvent();
}

void AnalyzeModeProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if ( !appData.showThinking )
      ShowThinkingProc( w,event,prms,nprms );

    AnalyzeModeEvent();
    AnalysisPopUp( "Analysis",
  "Analysis mode requires Crafty 9.25 or higher.\nStarting chess program..." );
}

void AnalyzeFileProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Reset(FALSE);

    if ( !appData.showThinking )
      ShowThinkingProc( w,event,prms,nprms );

    AnalyzeFileEvent();
    FileNamePopUp("File to analyze", "", LoadGamePopUp, "r");
    AnalysisPopUp( "Analysis",
  "Analysis mode requires Crafty 9.25 or higher.\nStarting chess program..." );
    AnalysisPeriodicEvent(1);
}

void TwoMachinesProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    TwoMachinesEvent();
}

void IcsClientProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    IcsClientEvent();
}

void EditGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    EditGameEvent();
}

void EditPositionProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    EditPositionEvent();
}

void EditCommentProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (editUp) {
	EditCommentPopDown();
    } else {
	EditCommentEvent();
    }
}

void IcsInputBoxProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (ICSInputBoxUp) {
	ICSInputBoxPopDown();
    } else {
	ICSInputBoxPopUp();
    }
}

void AcceptProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    AcceptEvent();
}

void DeclineProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    DeclineEvent();
}

void CallFlagProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    CallFlagEvent();
}

void DrawProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    DrawEvent();
}

void AbortProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    AbortEvent();
}

void AdjournProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    AdjournEvent();
}

void ResignProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ResignEvent();
}

void EnterKeyProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
  if ( ICSInputBoxUp == True )
	ICSInputSendText();
}

void StopObservingProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    StopObservingEvent();
}

void StopExaminingProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    StopExaminingEvent();
}


void ForwardProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ForwardEvent();
}


void BackwardProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    BackwardEvent();
}

void ToStartProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ToStartEvent();
}

void ToEndProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ToEndEvent();
}

void RevertProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    RevertEvent();
}

void TruncateGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    TruncateGameEvent();
}
void RetractMoveProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    RetractMoveEvent();
}

void MoveNowProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    MoveNowEvent();
}


void AlwaysQueenProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.alwaysPromoteToQueen = !appData.alwaysPromoteToQueen;

    if (appData.alwaysPromoteToQueen) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Always Queen"),
		args, 1);
}

void AutocommProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.autoComment = !appData.autoComment;

    if (appData.autoComment) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Comment"),
		args, 1);
}


void AutoflagProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.autoCallFlag = !appData.autoCallFlag;

    if (appData.autoCallFlag) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Flag"),
		args, 1);
}

void AutobsProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.autoObserve = !appData.autoObserve;

    if (appData.autoObserve) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Observe"),
		args, 1);
}

void AutosaveProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.autoSaveGames = !appData.autoSaveGames;

    if (appData.autoSaveGames) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Auto Save"),
		args, 1);
}

void BellProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.ringBellAfterMoves = !appData.ringBellAfterMoves;

    if (appData.ringBellAfterMoves) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Bell"),
		args, 1);
}


void CheckLegalityProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.checkLegality = !appData.checkLegality;

    if (appData.checkLegality) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Check Legality"),
		args, 1);
}


void FlipViewProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    flipView = !flipView;
    DrawPosition(True, NULL);
}

void GetMoveListProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.getMoveList = !appData.getMoveList;

    if (appData.getMoveList) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
	GetMoveListEvent();
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Get Move List"),
		args, 1);
}

void OldSaveStyleProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.oldSaveStyle = !appData.oldSaveStyle;

    if (appData.oldSaveStyle) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Old Save Style"),
		args, 1);
}

void PeriodicUpdatesProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

	PeriodicUpdatesEvent( !appData.periodicUpdates );
	
    if (appData.periodicUpdates) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Periodic Updates"),
		args, 1);
}

void QuietPlayProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.quietPlay = !appData.quietPlay;

    if (appData.quietPlay) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Quiet Play"),
		args, 1);
}

void ShowCoordsProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    appData.showCoords = !appData.showCoords;

    if (appData.showCoords) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Show Coords"),
		args, 1);

    DrawPosition(True, NULL);
}

void ShowThinkingProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];

    ShowThinkingEvent(!appData.showThinking);

    if (appData.showThinking) {
	XtSetArg(args[0], XtNleftBitmap, xMarkPixmap);
    } else {
	XtSetArg(args[0], XtNleftBitmap, None);
    }
    XtSetValues(XtNameToWidget(menuBarWidget, "menuOptions.Show Thinking"),
		args, 1);
}

void InfoProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    char buf[MSG_SIZ];
    sprintf(buf, "xterm -e info --directory . --directory %s -f %s &",
	    INFODIR, INFOFILE);
    system(buf);
}

void ManProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    char buf[MSG_SIZ];
    String name;
    if (nprms && *nprms > 0)
      name = prms[0];
    else
      name = "xboard";
    sprintf(buf, "xterm -e man %s &", name);
    system(buf);
}

void HintProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    HintEvent();
}

void BookProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    BookEvent();
}

void AboutProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    char buf[MSG_SIZ];
#if ZIPPY
    char *zippy = " (with Zippy code)";
#else
    char *zippy = "";
#endif
    sprintf(buf, "%s%s\n\n%s\n%s\n\n%s%s\n%s",
	    programVersion, zippy,
	    "Copyright 1991 Digital Equipment Corporation",
	    "Enhancements Copyright 1992-96 Free Software Foundation",
	    PRODUCT, " is free software and carries NO WARRANTY;",
	    "see the file COPYING for more information.");
    ErrorPopUp("About XBoard", buf);
}

void DebugProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    appData.debugMode = !appData.debugMode;
}

void AboutGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    AboutGameEvent();
}

void NothingProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    return;
}

void Iconify(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[16];
    
    fromX = fromY = -1;
    
    XtSetArg(args[0], XtNiconic, False);
    XtSetValues(shellWidget, args, 1);
    XtSetArg(args[0], XtNiconic, True);
    XtSetValues(shellWidget, args, 1);
}

void DisplayMessage(message, extMessage)
     char *message, *extMessage;
{
    char buf[MSG_SIZ];
    Arg arg;
    
    if (extMessage) {
	if (*message) {
	    sprintf(buf, "%s  %s", message, extMessage);
	    message = buf;
	} else {
	    message = extMessage;
	}
    }
    XtSetArg(arg, XtNlabel, message);
    XtSetValues(messageWidget, &arg, 1);
}

void DisplayTitle(text)
     char *text;
{
    Arg args[16];
    int i;
    char title[MSG_SIZ];
    char icon[MSG_SIZ];

    if (text == NULL) text = "";

    if (appData.titleInWindow) {
	i = 0;
	XtSetArg(args[i], XtNlabel, text);   i++;
	XtSetValues(titleWidget, args, i);
    }

    if (*text != NULLCHAR) {
	strcpy(icon, text);
	sprintf(title, "%s", icon);
    } else if (appData.icsActive) {
	strcpy(icon, appData.icsHost);
	sprintf(title, "%s: %s", programName, icon);
    } else if (appData.cmailGameName[0] != NULLCHAR) {
	strcpy(icon, "CMail");
	sprintf(title, "%s: %s", programName, icon);
    } else if (appData.noChessProgram) {
	strcpy(icon, programName);
	sprintf(title, "%s", programName);
    } else {
	if (StrStr(appData.firstChessProgram, "gnuchess")) {
	    strcpy(icon, "GNU Chess");
	} else {
	    char *p, *q;
	    p = strrchr(appData.firstChessProgram, '/');
	    if (p == NULL)
	      p = appData.firstChessProgram;
	    else
	      p++;
	    strcpy(icon, p);
	    q = strchr(icon, ' ');
	    if (q != NULL) *q = NULLCHAR;
	}
	sprintf(title, "%s: %s", programName, icon);
    }
    i = 0;
    XtSetArg(args[i], XtNiconName, (XtArgVal) icon);    i++;
    XtSetArg(args[i], XtNtitle, (XtArgVal) title);      i++;
    XtSetValues(shellWidget, args, i);
}


void DisplayError(message, error)
     String message;
     int error;
{
    extern char *sys_errlist[];
    char buf[MSG_SIZ];

    if (error == 0) {
	if (appData.debugMode || appData.matchMode) {
	    fprintf(stderr, "%s: %s\n", programName, message);
	}
	ErrorPopUp("Error", message);
    } else {
	if (appData.debugMode || appData.matchMode) {
	    fprintf(stderr, "%s: %s: %s\n",
		    programName, message, sys_errlist[error]);
	}
	sprintf(buf, "%s: %s", message, sys_errlist[error]);
	ErrorPopUp("Error", buf);
    }	
}


void DisplayFatalError(message, error, status)
     String message;
     int error, status;
{
    extern char *sys_errlist[];
    char buf[MSG_SIZ];

    errorExitStatus = status;
    if (errorExitStatus != -1) {
	XtSetSensitive(menuBarWidget, False);
	XtSetSensitive(buttonBarWidget, False);
	XtUninstallTranslations(formWidget);
	XtUninstallTranslations(boardWidget);
	XtUninstallTranslations(whiteTimerWidget);
	XtUninstallTranslations(blackTimerWidget);
    }
    if (error == 0) {
	fprintf(stderr, "%s: %s\n", programName, message);
	ErrorPopUp("Fatal Error", message);
    } else {
	fprintf(stderr, "%s: %s: %s\n",
		programName, message, sys_errlist[error]);
	sprintf(buf, "%s: %s", message, sys_errlist[error]);
	ErrorPopUp("Fatal Error", buf);
    }
}


void DisplayInformation(message)
     String message;
{
    ErrorPopDown();
    ErrorPopUp("Information", message);
}

void RingBell()
{
    putc(BELLCHAR, stderr);
}

void EchoOn()
{
    system("stty echo\n");
}

void EchoOff()
{
    system("stty -echo\n");
}

char *UserName()
{
    return getpwuid(getuid())->pw_name;
}

static char *ExpandPathName( path )
	 char *path;
{
  static char static_buf[2000];
  char *d, *s, buf[2000];
  struct passwd *pwd;
  
  s = path;
  d = static_buf;

  while( *s && isspace( *s ) )
	++s;

  if ( !*s ) {
	*d = 0;
	return static_buf;
  }

  if ( *s == '~' ) {
    if ( *(s+1) == '/' ) {
	  strcpy( d, getpwuid(getuid())->pw_dir );
      strcat( d, s+1 );
    }
	else {
	  strcpy( buf, s+1 );
	  *strchr( buf, '/' ) = 0;
	  pwd = getpwnam( buf );
	  if ( !pwd )
		{
		  fprintf( stderr, "ERROR: Unknown user %s (in path %s)\n",
				  buf, path );
	  return NULL;
		}
	  strcpy( d, pwd->pw_dir );
      strcat( d, strchr( s+1, '/' ) );
	}
  }
  else
	strcpy( d, s );

  return static_buf;
}  
	
char *HostName()
{
    static char host_name[MSG_SIZ];
    
#if HAVE_GETHOSTNAME
    gethostname(host_name, MSG_SIZ);
    return host_name;
#else /* not HAVE_GETHOSTNAME */
# if HAVE_SYSINFO && HAVE_SYS_SYSTEMINFO_H
    sysinfo(SI_HOSTNAME, host_name, MSG_SIZ);
    return host_name;
# else /* not (HAVE_SYSINFO && HAVE_SYS_SYSTEMINFO_H) */
    return "localhost";
# endif /* not (HAVE_SYSINFO && HAVE_SYS_SYSTEMINFO_H) */
#endif /* not HAVE_GETHOSTNAME */
}

XtIntervalId loadGameTimerXID = 0;

int LoadGameTimerRunning()
{
    return loadGameTimerXID != 0;
}

int StopLoadGameTimer()
{
    if (loadGameTimerXID != 0) {
	XtRemoveTimeOut(loadGameTimerXID);
	loadGameTimerXID = 0;
	return TRUE;
    } else {
	return FALSE;
    }
}

void LoadGameTimerCallback(arg, id)
     XtPointer arg;
     XtIntervalId *id;
{
    loadGameTimerXID = 0;
    LoadGameLoop();
}

void StartLoadGameTimer(millisec)
     long millisec;
{
    loadGameTimerXID =
      XtAppAddTimeOut(appContext, millisec,
		      (XtTimerCallbackProc) LoadGameTimerCallback,
		      (XtPointer) 0);
}

XtIntervalId analysisClockXID = 0;

void AnalysisClockCallback(arg, id)
     XtPointer arg;
     XtIntervalId *id;
{
  if ( gameMode == AnalyzeMode || gameMode == AnalyzeFile ) {
	AnalysisPeriodicEvent(0);
	StartAnalysisClock();
  }
}

void StartAnalysisClock()
{
    analysisClockXID =
      XtAppAddTimeOut(appContext, 2000,
		      (XtTimerCallbackProc) AnalysisClockCallback,
		      (XtPointer) 0);
}

XtIntervalId clockTimerXID = 0;

int ClockTimerRunning()
{
    return clockTimerXID != 0;
}

int StopClockTimer()
{
    if (clockTimerXID != 0) {
	XtRemoveTimeOut(clockTimerXID);
	clockTimerXID = 0;
	return TRUE;
    } else {
	return FALSE;
    }
}

void ClockTimerCallback(arg, id)
     XtPointer arg;
     XtIntervalId *id;
{
    clockTimerXID = 0;
    DecrementClocks();
}

void StartClockTimer(millisec)
     long millisec;
{
    clockTimerXID =
      XtAppAddTimeOut(appContext, millisec,
		      (XtTimerCallbackProc) ClockTimerCallback,
		      (XtPointer) 0);
}

void DisplayTimerLabel(w, color, timer, highlight)
     Widget w;
     char *color;
     long timer;
     int highlight;
{
    char buf[MSG_SIZ];
    Arg args[16];
    
    if (appData.clockMode) {
	sprintf(buf, "%s: %s", color, TimeString(timer));
	XtSetArg(args[0], XtNlabel, buf);
    } else {
	sprintf(buf, "%s  ", color);
	XtSetArg(args[0], XtNlabel, buf);
    }
    
    if (highlight) {
	XtSetArg(args[1], XtNbackground, timerForegroundPixel);
	XtSetArg(args[2], XtNforeground, timerBackgroundPixel);
    } else {
	XtSetArg(args[1], XtNbackground, timerBackgroundPixel);
	XtSetArg(args[2], XtNforeground, timerForegroundPixel);
    }
    
    XtSetValues(w, args, 3);
}

void DisplayWhiteClock(timeRemaining, highlight)
     long timeRemaining;
     int highlight;
{
    Arg args[16];
    DisplayTimerLabel(whiteTimerWidget, "White", timeRemaining, highlight);
    if (highlight && iconPixmap == bIconPixmap) {
	iconPixmap = wIconPixmap;
	XtSetArg(args[0], XtNiconPixmap, iconPixmap);
	XtSetValues(shellWidget, args, 1);
    }
}

void DisplayBlackClock(timeRemaining, highlight)
     long timeRemaining;
     int highlight;
{
    Arg args[16];
    DisplayTimerLabel(blackTimerWidget, "Black", timeRemaining, highlight);
    if (highlight && iconPixmap == wIconPixmap) {
	iconPixmap = bIconPixmap;
	XtSetArg(args[0], XtNiconPixmap, iconPixmap);
	XtSetValues(shellWidget, args, 1);
    }
}

#define CPReal 1
#define CPComm 2
#define CPSock 3
#define CPLoop 4
typedef int CPKind;

typedef struct {
    CPKind kind;
    int pid;
    int fdTo, fdFrom;  
} ChildProc;


int StartChildProcess(cmdLine, pr)
     char *cmdLine; 
     ProcRef *pr;
{
    char *argv[64], *p;
    int i, pid;
    int to_prog[2], from_prog[2];
    ChildProc *cp;
    
    if (appData.debugMode)
      fprintf(stderr, "%s: StartChildProcess %s\n",
	      programName, cmdLine);

    /* We do NOT feed the cmdLine to the shell; we just
       parse it into blank-separated arguments in the
       most simple-minded way possible.
    */
    i = 0;
    p = cmdLine;
    for (;;) {
	argv[i++] = p;
	p = strchr(p, ' ');
	if (p == NULL) break;
	*p++ = NULLCHAR;
    }
    argv[i] = NULL;

    SetUpChildIO(to_prog, from_prog);

    if ((pid = fork()) == 0) {
	/* Child process */
	dup2(to_prog[0], 0);
	dup2(from_prog[1], 1);
	close(to_prog[0]);
	close(to_prog[1]);
	close(from_prog[0]);
	close(from_prog[1]);
	dup2(1, fileno(stderr)); /* force stderr to the pipe */

        execvp(argv[0], argv);
	
	perror(argv[0]);
	exit(1);
    }
    
    /* Parent process */
    close(to_prog[0]);
    close(from_prog[1]);
    
    cp = (ChildProc *) calloc(1, sizeof(ChildProc));
    cp->kind = CPReal;
    cp->pid = pid;
    cp->fdFrom = from_prog[0];
    cp->fdTo = to_prog[1];
    *pr = (ProcRef) cp;
    return 0;
}

void DestroyChildProcess(pr)
     ProcRef pr;
{
    ChildProc *cp = (ChildProc *) pr;

    if (cp->kind != CPReal) return;
    if (kill(cp->pid, SIGTERM) == 0)
      wait((int *) 0);
    close(cp->fdFrom);
    close(cp->fdTo);
}

void InterruptChildProcess(pr)
     ProcRef pr;
{
    ChildProc *cp = (ChildProc *) pr;

    if (cp->kind != CPReal) return;
    (void) kill(cp->pid, SIGINT); /* stop it thinking */
}

int OpenTelnet(host, port, pr)
     char *host;
     char *port;
     ProcRef *pr;
{
    char cmdLine[MSG_SIZ];

    if (port[0] == NULLCHAR) {
	sprintf(cmdLine, "%s %s", appData.telnetProgram, host);
    } else {
	sprintf(cmdLine, "%s %s %s", appData.telnetProgram, host, port);
    }
    return StartChildProcess(cmdLine, pr);
}

int OpenTCP(host, port, pr)
     char *host;
     char *port;
     ProcRef *pr;
{
#if OMIT_SOCKETS
    DisplayFatalError("Socket support is not configured in", 0, 2);
#else /* !OMIT_SOCKETS */
    int s;
    struct sockaddr_in sa;
    struct hostent     *hp;
    unsigned short uport;
    ChildProc *cp;

    if ((s = socket(AF_INET, SOCK_STREAM, 6)) < 0) {
	return errno;
    }

    memset((char *) &sa, (int)0, sizeof(struct sockaddr_in));
    sa.sin_family = AF_INET;
    sa.sin_addr.s_addr = INADDR_ANY;
    uport = (unsigned short) 0;
    sa.sin_port = htons(uport);
    if (bind(s, (struct sockaddr *) &sa, sizeof(struct sockaddr_in)) < 0) {
	return errno;
    }

    memset((char *) &sa, (int)0, sizeof(struct sockaddr_in));
    if (!(hp = gethostbyname(host))) {
	int b0, b1, b2, b3;
	if (sscanf(host, "%d.%d.%d.%d", &b0, &b1, &b2, &b3) == 4) {
	    hp = (struct hostent *) calloc(1, sizeof(struct hostent));
	    hp->h_addrtype = AF_INET;
	    hp->h_length = 4;
	    hp->h_addr_list = (char **) calloc(2, sizeof(char *));
	    hp->h_addr_list[0] = (char *) malloc(4);
	    hp->h_addr_list[0][0] = b0;
	    hp->h_addr_list[0][1] = b1;
	    hp->h_addr_list[0][2] = b2;
	    hp->h_addr_list[0][3] = b3;
	} else {
	    return ENOENT;
	}
    }
    sa.sin_family = hp->h_addrtype;
    uport = (unsigned short) atoi(port);
    sa.sin_port = htons(uport);
    memcpy((char *) &sa.sin_addr, hp->h_addr, hp->h_length);

    if (connect(s, (struct sockaddr *) &sa, 
		sizeof(struct sockaddr_in)) < 0) {
	return errno;
    }

    cp = (ChildProc *) calloc(1, sizeof(ChildProc));
    cp->kind = CPSock;
    cp->pid = 0;
    cp->fdFrom = s;
    cp->fdTo = s;
    *pr = (ProcRef) cp;

#endif /* !OMIT_SOCKETS */

    return 0;
}

int OpenCommPort(name, pr)
     char *name;
     ProcRef *pr;
{
    int fd;
    ChildProc *cp;

    fd = open(name, 2, 0);
    if (fd < 0) return errno;

    cp = (ChildProc *) calloc(1, sizeof(ChildProc));
    cp->kind = CPComm;
    cp->pid = 0;
    cp->fdFrom = fd;
    cp->fdTo = fd;
    *pr = (ProcRef) cp;

    return 0;
}

int OpenLoopback(pr)
     ProcRef *pr;
{
    ChildProc *cp;
    int to[2], from[2];

    SetUpChildIO(to, from);

    cp = (ChildProc *) calloc(1, sizeof(ChildProc));
    cp->kind = CPLoop;
    cp->pid = 0;
    cp->fdFrom = to[0];  /* note not from[0]; we are doing a loopback */
    cp->fdTo = to[1];
    *pr = (ProcRef) cp;

    return 0;
}

int OpenRcmd(host, user, cmd, pr)
     char *host, *user, *cmd;
     ProcRef *pr;
{
    DisplayFatalError("internal rcmd not implemented for Unix", 0, 1);
    return -1;
}    

#define INPUT_SOURCE_BUF_SIZE 4096

typedef struct {
    CPKind kind;
    int fd;
    FILE *f;
    int lineByLine;
    InputCallback func;
    XtInputId xid;
    char buf[INPUT_SOURCE_BUF_SIZE];
} InputSource;

void DoInputCallback(closure, source, xid) 
     caddr_t closure;
     int *source;
     XtInputId *xid;
{
    InputSource *is = (InputSource *) closure;
    int count;
    int error;

    if (is->lineByLine) {
	if (fgets(is->buf, INPUT_SOURCE_BUF_SIZE, is->f) == NULL) {
	    error = ferror(is->f);
	    if (error == 0)
	      count = 0;
	    else
	      count = -1;
	    (is->func)((InputSourceRef) is, is->buf, count, error);
	} else {
	    (is->func)((InputSourceRef) is, is->buf, strlen(is->buf), 0);
	}
    } else {
	count = read(is->fd, is->buf, INPUT_SOURCE_BUF_SIZE);
	if (count == -1)
	  error = errno;
	else
	  error = 0;
	(is->func)((InputSourceRef) is, is->buf, count, error);
    }	
}

InputSourceRef AddInputSource(pr, lineByLine, func)
     ProcRef pr;
     int lineByLine;
     InputCallback func;
{
    InputSource *is;
    ChildProc *cp = (ChildProc *) pr;

    is = (InputSource *) calloc(1, sizeof(InputSource));
    is->lineByLine = lineByLine;
    is->func = func;
    if (pr == NoProc) {
	is->kind = CPReal;
	is->fd = fileno(stdin);
    } else {
	is->kind = cp->kind;
	is->fd = cp->fdFrom;
    }
    if (lineByLine) {
	is->f = fdopen(is->fd, "r");
	setbuf(is->f, NULL);
    }
    
    is->xid = XtAppAddInput(appContext, is->fd,
			    (XtPointer) (XtInputReadMask),
			    (XtInputCallbackProc) DoInputCallback,
			    (XtPointer) is);
    return (InputSourceRef) is;
}

void RemoveInputSource(isr)
     InputSourceRef isr;
{
    InputSource *is = (InputSource *) isr;

    if (is->xid == 0) return;
    XtRemoveInput(is->xid);
    if (is->lineByLine) {
	fclose(is->f);
    }
    is->xid = 0;
}

int OutputToProcess(pr, message, count, outError)
     ProcRef pr;
     char *message;
     int count;
     int *outError;
{
    ChildProc *cp = (ChildProc *) pr;
    int outCount;

    outCount = write(cp->fdTo, message, count);
    if (outCount == -1)
      *outError = errno;
    else
      *outError = 0;
    return outCount;
}

/* Output message to process, with "ms" milliseconds of delay
   between each character. This is needed when sending the logon
   script to ICC, which for some reason doesn't like the
   instantaneous send. */
int OutputToProcessDelayed(pr, message, count, outError, msdelay )
     ProcRef pr;
     char *message;
     int count;
     int *outError;
     long msdelay;
{
    ChildProc *cp = (ChildProc *) pr;
    int outCount = 0;
    int r;
    
    while( count-- ) {
      r = write(cp->fdTo, message++, 1 );
      if ( r == -1 ) {
	*outError = errno;
	return outCount;
      }
      ++outCount;
      if ( msdelay >= 0 )
      TimeDelay( msdelay );
    }

    return outCount;
}


