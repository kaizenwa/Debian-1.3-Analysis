/* modified slightly for BowMan
 * by Bo yang
 *
 * modified even more slightly by Frank Fejes
 */
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


#ifndef _MISC_
#define _MISC_

#include <ctype.h>
#include "menus.h"
#include "../lib/aftersteplib.h"

/************************************************************************
 * ReapChildren - wait() for all dead child processes
 ************************************************************************/
#include <sys/wait.h>
#ifdef HAVE_WAITPID
#define ReapChildren()  while ((waitpid(-1, NULL, WNOHANG)) > 0);
#else
#define ReapChildren()  while ((wait3(NULL, WNOHANG, NULL)) > 0);
#endif

typedef struct name_list_struct
{
  struct name_list_struct *next;   /* pointer to the next name */
  struct name_list_struct *prev; /* fsf add */
  char *name;		  	   /* the name of the window */
  char *value;                     /* icon name */
  int Desk;                        /* Desktop number */
  unsigned long on_flags; 
  unsigned long off_flags; 
  int border_width;
  int resize_width;
  char *ForeColor;
  char *BackColor;

  unsigned long on_buttons;
  unsigned long off_buttons;

} name_list;

/* used for parsing configuration */
struct config
{
  char *keyword;
#ifdef __STDC__
  void (*action)(char *, FILE *, char **, int *);
#else
  void (*action)();
#endif
  char **arg;
  int *arg2;
};

/* values for name_list flags */
#define STICKY_FLAG          1
#define STAYSONTOP_FLAG      2
#define NOBORDER_FLAG        4
#define NOTITLE_FLAG         8
#define ICON_FLAG           32
#define CIRCULATESKIP_FLAG  64
#define LISTSKIP_FLAG      128
#define STAYSONDESK_FLAG   256
#define SUPPRESSICON_FLAG  512
#define BW_FLAG           1024
#define NOBW_FLAG         2048
#define START_ICONIC_FLAG 4096
#define FORE_COLOR_FLAG   8192
#define BACK_COLOR_FLAG  16384
/* #define NOICON_TITLE_FLAG 32768 */
#define NOFOCUS_FLAG     32768 

/* some fancy font handling stuff */
#define NewFontAndColor(newfont,color,backcolor) {\
   Globalgcv.font = newfont;\
   Globalgcv.foreground = color;\
   Globalgcv.background = backcolor;\
   Globalgcm = GCFont | GCForeground | GCBackground; \
   XChangeGC(dpy,Scr.FontGC,Globalgcm,&Globalgcv); \
}

#ifdef NO_ICONS
#define ICON_HEIGHT 1
#else
/*
#define ICON_HEIGHT (IconFont->height+6)
*/
#define ICON_HEIGHT 64
#endif

extern XGCValues Globalgcv;
extern unsigned long Globalgcm;
extern MyFont *IconFont;
extern Time lastTimestamp;
extern XEvent Event;
extern char NoName[];

extern unsigned long LookInList(name_list *, char *, XClassHint *, 
				char **value, int *Desk, int *bw, int *nobw,
				char **forecolor, char **backcolor, 
                                unsigned long * buttons);
extern void       MoveOutline(Window, int,int,int,int);
extern void       DoResize(int, int, ASWindow *);
extern void       DisplaySize(ASWindow *, int, int, Bool);
extern void       DisplayPosition(ASWindow *, int, int,Bool);
extern void       SetupFrame(ASWindow *,int,int,int,int,Bool);
extern void       CreateGCs(void);
extern void       InstallWindowColormaps(ASWindow *);
extern void       InstallRootColormap(void);
extern void       UninstallRootColormap(void);
extern void       FetchWmProtocols(ASWindow *);
extern void       FetchWmColormapWindows (ASWindow *tmp);
extern void       PaintEntry(MenuRoot *, MenuItem *);
extern void       PaintMenu(MenuRoot *, XEvent *);
extern void       MakeMenus(const char*, char*);
extern void       InitEvents(void);
extern void       DispatchEvent(void);
extern void       HandleEvents(void);
extern void       HandleExpose(void);
extern void       HandleFocusIn(void);
extern void       HandleFocusOut(void);
extern void       HandleDestroyNotify(void);
extern void       HandleMapRequest(void);
extern void       HandleMapNotify(void);
extern void       HandleUnmapNotify(void);
extern void       HandleMotionNotify(void);
extern void       HandleButtonRelease(void);
extern void       HandleButtonPress(void);
extern void       HandleEnterNotify(void);
extern void       HandleLeaveNotify(void);
extern void       HandleConfigureRequest(void);
extern void       HandleClientMessage(void);
extern void       HandlePropertyNotify(void);
extern void       HandleKeyPress(void);
extern void       HandleVisibilityNotify(void);
extern void       HandleColormapNotify(void);
extern void       SetTitleBar(ASWindow *, Bool,Bool);
extern void       RestoreWithdrawnLocation(ASWindow *, Bool);
extern void       Destroy(ASWindow *);
extern void       GetGravityOffsets (ASWindow *, int *, int *);
extern void       MoveViewport(int newx, int newy,Bool);
extern ASWindow *AddWindow(Window w);
extern int        MappedNotOverride(Window w);
extern void       GrabButtons(ASWindow *);
extern void       GrabKeys(ASWindow *);
extern void       GetWindowSizeHints(ASWindow *);
extern void       RedrawPager(void);
extern void       ReallyRedrawPager(void);
extern void       SwitchPages(Bool,Bool);
extern void       NextPage(void);
extern void       PrevPage(void);
extern void       moveLoop(ASWindow *, int, int, int,int, int *, int *,Bool,Bool);

extern void       Keyboard_shortcuts(XEvent *, int);
extern void       RedoIconName(ASWindow *);
extern void       DrawIconWindow(ASWindow *);
extern void       CreateIconWindow(ASWindow *tmp_win, int def_x, int def_y);
extern void       GetIcon(ASWindow *tmp_win);
extern void       SearchIcon(ASWindow *tmp_win, char **value);


extern AFTER_INLINE void RelieveWindow(ASWindow *, Window, 
				      int, int, int, int, GC, GC, int);
void RelieveParts(ASWindow *t,int i,GC hor, GC vert);
#define NO_HILITE     0x0000
#define TOP_HILITE    0x0001
#define RIGHT_HILITE  0x0002
#define BOTTOM_HILITE 0x0004
#define LEFT_HILITE   0x0008
#define FULL_HILITE   0x000F

extern void       sleep_a_little(int);
extern void       PagerMoveWindow(void);
extern void       Maximize(ASWindow *,int,int,int, int);
extern void	  Shade(ASWindow *);
extern void	  ResetShade(ASWindow *);
extern void       RaiseWindow(ASWindow *t);
extern void       LowerWindow(ASWindow *t);
extern Bool       GrabEm(int);
extern void       UngrabEm(void);
extern MenuRoot   *NewMenuRoot(char *name);
extern void       AddToMenu(MenuRoot *, char *, char *, char *,int, 
			    long,long, char, char);
extern void       MakeMenu(MenuRoot *);
extern void       CaptureAllWindows(void);
extern void       SetTimer(int);
extern int        flush_expose(Window w);
extern void       ExecuteFunction(int, char *,Window, ASWindow *, XEvent *, 
				  unsigned long, long, long,int,int,
				  MenuRoot *,  int module);
extern void       do_windowList(int, int);
extern void       RaiseThisWindow(int);
extern int        GetContext(ASWindow *, XEvent *, Window *dummy);
extern void       ConstrainSize (ASWindow *, int *, int *);
extern void       HandlePaging(int, int, int *, int *, int *, int *,Bool);
extern void       SetShape(ASWindow *, int);
extern void       AutoPlace(ASWindow *);
extern void       afterstep_err(char *, char *, char *, char *);
extern void       MoveResizePagerView(ASWindow *t);
extern void       MoveResizeViewPortIndicator(void);
extern void       executeModule(char *action,FILE *fd, char **arg, int *junk);
extern Bool       SetFocus(Window,ASWindow*,Bool);
extern void       CheckAndSetFocus(void);
extern void       initModules(void);
extern int        HandleModuleInput(Window w, int channel);
extern void       nofont(char *name);
extern char       *stripcpy(char *);
extern char       *stripcpy2(char *,int, Bool);
extern char       *stripcpy3(char *, Bool);
extern void       match_string(struct config *, char *, char *, FILE *);
extern void       no_popup(char *ptr);
extern void       KillModule(int channel, int place);
extern void       ClosePipes(void);
extern char       *findIconFile(char *icon, char *pathlist, int mode);
extern void       GetBitmapFile(ASWindow *tmp_win);
extern void       GetXPMFile(ASWindow *tmp_win);
extern void       GetIconWindow(ASWindow *tmp_win);
extern void       GetIconBitmap(ASWindow *tmp_win);
extern void SmartPlacement(ASWindow *t, int width, int height,int *x,int *y);
extern void usage(void);
void Broadcast(unsigned long event_type, unsigned long num_datum,
	       unsigned long data1, unsigned long data2, 
	       unsigned long data3, unsigned long data4,
	       unsigned long data5, unsigned long data6, 
	       unsigned long data7);
void BroadcastConfig(unsigned long event_type, ASWindow *t);
void SendPacket(int channel, unsigned long event_type, unsigned long num_datum,
		unsigned long data1, unsigned long data2, 
		unsigned long data3, unsigned long data4,
		unsigned long data5, unsigned long data6, 
		unsigned long data7);
void SendConfig(int module, unsigned long event_type, ASWindow *t);
void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *name);
void SendName(int channel, unsigned long event_type,unsigned long data1,
	      unsigned long data2, unsigned long data3, char *name);
void DeadPipe(int nonsense);
void GetMwmHints(ASWindow *t);
void SelectDecor(ASWindow *, unsigned long, int,int);
extern Bool PopUpMenu(MenuRoot *, int, int);
void ComplexFunction(Window, ASWindow *, XEvent *,unsigned long, MenuRoot *);
extern int DeferExecution(XEvent *, Window *,ASWindow **, unsigned long *, int, int);
void send_clientmessage (Window, Atom, Time);
void SetBorder (ASWindow *, Bool,Bool,Bool, Window);
void move_window(XEvent *,Window,ASWindow *,int,int, int,int,int);
void resize_window(Window,ASWindow *, int,int,int,int);
void CreateIconWindow(ASWindow *, int, int);
void send_clientmessage (Window, Atom, Time);
void SetMapStateProp(ASWindow *, int);
void SetStickyProp(ASWindow *, int, int, int);
void SetClientProp(ASWindow *);
void Iconify(ASWindow *, int, int);
void DeIconify(ASWindow *);
void PopDownMenu(void);
void KeepOnTop(void);
void show_panner(void);
void WaitForButtonsUp(void);
void FocusOn(ASWindow *t,int DeIconifyOnly, Bool circulating);
Bool PlaceWindow(ASWindow *tmp_win, unsigned long flags,int Desk);
void free_window_names (ASWindow *tmp, Bool nukename, Bool nukeicon);

int do_menu (MenuRoot *menu);
int check_allowed_function(MenuItem *mi);
int check_allowed_function2(int function, ASWindow *t);
void ReInstallActiveColormap(void);
void ParsePopupEntry(char *,FILE *, char **, int *);
void ParseMouseEntry(char *,FILE *, char **,int *);
void ParseKeyEntry(char *, FILE *, char **,int *);
void SetOneStyle(char *text,FILE *,char **,int *);
void AddToList(char *name, char *icon_name, unsigned long off_flags, 
	       unsigned long on_flags, int desk,int bw, int nobw, 
	       char *forecolor, char *backcolor,
               unsigned long off_buttons, unsigned long on_buttons);

void ParseStyle(char *text,FILE *,char **,int *);
void assign_string(char *text, FILE *fd, char **arg,int *);
void ButtonStyle();
void IconStyle();
void SetTitleButton(char *text,FILE *,char **,int *);
void SetFlag(char *text, FILE *fd, char **arg,int *);
void SetTextureFlag(char *text, FILE *fd, char **arg,int *);
void SetIconFlag(char *text, FILE *fd, char **arg,int *);
void SetCursor(char *text, FILE *fd, char  **arg,int *);
void SetInts(char *text, FILE *fd, char **arg,int *);
void SetButtonList(char *text, FILE *fd, char **arg,int *);
void SetBox(char *text, FILE *fd, char **arg,int *);
void set_func(char *, FILE *, char **,int *);
void copy_config(FILE **config_fd);
Pixel    GetShadow(Pixel);
Pixel    GetHilite(Pixel);


#define UP 1
#define DOWN 0
extern ASWindow *Circulate(ASWindow *tmp_win, char *action,Bool Direction);
void changeDesks(int val1,int val2);
void changeWindowsDesk(ASWindow *t,int val1);
void MapIt(ASWindow *t);
void UnmapIt(ASWindow *t);
void do_save(void);
void checkPanFrames(void);
void raisePanFrames(void);
void initPanFrames(void);
Bool StashEventTime (XEvent *ev);
int matchWildcards(char *pattern, char *string);
int My_XNextEvent(Display *dpy, XEvent *event);
void SetCirculateSequence();
void MyXGrabButton(Display*, unsigned, unsigned, Window, Bool, unsigned,
		   int, int, Window, Cursor);
void MyXUngrabButton(Display*, unsigned, unsigned, Window);
void MyXGrabKey(Display*, int, unsigned, Window, Bool, int, int);
void GrabRaiseClick(ASWindow*);
void UngrabRaiseClick(ASWindow*);
void UpdateVisibility(void);
void CorrectStackOrder(void);
void FlushQueue(int module);
void QuickRestart(void);
void     AddFuncKey (char *, int, int, int, char *, int, int, MenuRoot *,
		     char , char);

void InteractiveMove(Window *w, ASWindow *tmp_win, int *FinalX, int *FinalY,
		     XEvent *eventp);
#ifdef BROKEN_SUN_HEADERS
#include "sun_headers.h"
#endif
#ifdef NEEDS_ALPHA_HEADER
#include "alpha_header.h"
#endif /* NEEDS_ALPHA_HEADER */
#endif /* _MISC_ */
