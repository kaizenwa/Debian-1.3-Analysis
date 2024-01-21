#include <stdio.h>
#include <stdarg.h>
#include <signal.h>
#include <string.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <unistd.h>
#include <ctype.h>
#include <stdlib.h>
#include <limits.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>

#ifdef MALLOC_H
#include <malloc.h>
#endif

#ifndef FVWM_VERSION
#define FVWM_VERSION 2
#endif

#ifdef COMPILE_STANDALONE
#include "fvwmlib.h"
#else
#include "../../libs/fvwmlib.h"
#endif

#if defined ___AIX || defined _AIX || defined __QNX__ || defined ___AIXV3 || defined AIXV3 || defined _SEQUENT_
#include <sys/select.h>
#endif

#ifndef DEFAULT_ACTION
#define DEFAULT_ACTION "Iconify"
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) > (b) ? (b) : (a))
#endif



#if 1
#define OUTPUT_FILE "/dev/console"
#else
#define OUTPUT_FILE "/usr/bradym/log"
#endif

#if 0
#define PRINT_DEBUG      
#endif

#define MAX_ARGS 3

#if !defined (PRINT_DEBUG) && defined (__GNUC__)
#define ConsoleDebug(fmt, args...)
#else
extern void ConsoleDebug(char *fmt, ...);
#endif

#ifdef TRACE_MEMUSE

#define MALLOC_MAGIC 0xdeadbeaf

extern long MemUsed;

struct malloc_header {
  unsigned long magic, len;
};

#endif

extern void PrintMemuse (void);

typedef unsigned long Ulong;
typedef unsigned char Uchar;

typedef signed char Schar;

typedef enum {
  SHOW_GLOBAL = 0,
  SHOW_DESKTOP = 1,
  SHOW_PAGE = 2
} Resolution;

typedef enum { 
  BUTTON_FLAT,
  BUTTON_UP,
  BUTTON_DOWN
} ButtonState;

/* The clicks must be the first three elements in this type, X callbacks
	depend on it! */
typedef enum { 
  SELECT,
  MOUSE,
  KEYPRESS,
  NUM_ACTIONS
} Action;

typedef enum {
  PLAIN_CONTEXT,
  FOCUS_CONTEXT,
  SELECT_CONTEXT,
  FOCUS_SELECT_CONTEXT,
  NUM_CONTEXTS
} Contexts;

typedef enum {
  NO_NAME       = 0,
  TITLE_NAME    = 1,
  ICON_NAME     = 2,
  RESOURCE_NAME = 4,
  CLASS_NAME    = 8,
  ALL_NAME      = 15
} NameType;

typedef struct win_list {
  int n;
  struct win_data *head, *tail;
} WinList;

typedef struct string_list {
  NameType type;
  char *string;
  struct string_list *next;
} StringEl;

typedef struct {
  Uchar mask;
  StringEl *list;
} StringList;

typedef enum { 
  NoArg, 
  StringArg,
  ButtonArg, 
  WindowArg,
  ManagerArg
} BuiltinArgType;

typedef enum { 
  NoButton, 
  SelectButton, 
  FocusButton, 
  AbsoluteButton 
} ButtonType; /* doubles for manager too */

typedef struct {
  int offset;
  ButtonType base;
} ButtonValue;

typedef struct builtin_arg {
  BuiltinArgType type;
  union {
    char *string_value;
    ButtonValue button_value;
  } value;
} BuiltinArg;

typedef struct Function {
  int (*func)(int numargs, BuiltinArg *args);
  int numargs;
  BuiltinArg args[MAX_ARGS];
  struct Function *next;
} Function;

typedef struct Binding
{
  char IsMouse;           /* Is it a mouse or key binding 1= mouse; */
  int Button_Key;         /* Mouse Button number of Keycode */
  char *key_name;         /* In case of keycode, give the key_name too */
  int Modifier;           /* Modifiers for keyboard state */   
  char *Action;           /* What to do? */
  Function *Function;
  struct Binding *NextBinding, *LastBinding; 
} Binding;

typedef struct {
  int index;
  Resolution res;
  Window theWindow;
  Pixel backcolor[NUM_CONTEXTS], forecolor[NUM_CONTEXTS];
  Pixel hicolor[NUM_CONTEXTS], shadowcolor[NUM_CONTEXTS];
  GC hiContext[NUM_CONTEXTS], backContext[NUM_CONTEXTS], 
    reliefContext[NUM_CONTEXTS];
  GC shadowContext[NUM_CONTEXTS], flatContext[NUM_CONTEXTS];
  XFontStruct *ButtonFont;
#ifdef MINI_ICONS
  int draw_icons;
#endif
  int fontheight, boxheight, fontwidth;
  int win_width, win_height;
  int win_x, win_y, win_title, win_border;
  WinList icon_list;
  StringList show;
  StringList dontshow;
  Binding *bindings[NUM_ACTIONS];
  char *fontname;
  char *backColorName[NUM_CONTEXTS];
  char *foreColorName[NUM_CONTEXTS];
  char *geometry;
  char *titlename, *iconname;
  char *formatstring;
  int we_are_drawing, draw_events_pending, configures_expected;
  NameType format_depend;
  Schar select_box, focus_box;
  Uchar cursor_in_window;
  Uchar window_up, window_mapped;
  Schar grow_direction;
  ButtonState buttonState[NUM_CONTEXTS];
  Uchar followFocus;
  Uchar usewinlist;
  Uchar sort;
} WinManager;

typedef struct win_data {
  Ulong desknum;
  long x, y, width, height;
  Ulong app_id;
  char *resname;
  char *classname;
  char *titlename;
  char *iconname;
#ifdef MINI_ICONS
  Picture pic;
#endif
  char *display_string; /* what gets shown in the manager window */
  struct win_data *win_prev, *win_next, *icon_prev, *icon_next;
  WinManager *manager;
  Uchar iconified;
  Uchar in_iconlist;
  Uchar complete;
  Uchar focus;
  Uchar sticky;
  Uchar winlistskip;
  int app_id_set : 1;
  int geometry_set : 1;
} WinData;

typedef struct {
  Ulong desknum;
  Ulong x, y;             /* of the view window */
  Ulong screenx, screeny; /* screen dimensions */
  WinManager *managers;
  int num_managers;
  int transient;
  WinData *focus_win;
  WinData *select_win;
} GlobalData;

typedef struct {
  char *name;
  ButtonState state;
  char *forecolor[2]; /* 0 is mono, 1 is color */
  char *backcolor[2]; /* 0 is mono, 1 is color */
} ContextDefaults;

typedef WinList HashTab[256];

extern char *contextNames[NUM_CONTEXTS];

extern GlobalData globals;
extern int Fvwm_fd[2];
extern int x_fd;
extern Display *theDisplay;
extern char *Module;
extern int ModuleLen;
extern ContextDefaults contextDefaults[];

extern void ReadFvwmPipe();
extern void *Malloc (size_t size);
extern void Free (void *p);
extern void ConsoleMessage(char *fmt, ...);
extern void ShutMeDown (int flag);
extern void DeadPipe (int nothing);
extern void SendFvwmPipe(char *message, unsigned long window);
extern char *copy_string (char **target, char *src);

extern void init_globals (void);
extern int allocate_managers (int num);

extern WinData *new_windata (void);
extern void free_windata (WinData *p);
extern int check_win_complete (WinData *p);
extern void set_displaystring (WinData *win);
extern int set_win_manager (WinData *win, Uchar mask);
extern void init_winlists (void);
extern void insert_win_iconlist (WinData *win);
extern void delete_win_iconlist (WinData *win, WinManager *man);
extern void delete_win_hashtab (WinData *win);
extern void insert_win_hashtab (WinData *win);
extern WinData *find_win_hashtab (Ulong id);
extern void print_iconlist (void);
extern void walk_hashtab (void (*func)(void *));
extern int accumulate_walk_hashtab (int (*func)(void *));
extern void print_stringlist (StringList *list);
extern void add_to_stringlist (StringList *list, char *s);
extern void update_window_stuff (WinManager *man);
extern void print_managers (void);

extern void init_display (void);
extern void xevent_loop (void);
extern void init_window (int man_id);
extern void init_boxes (void);
extern void draw_button (WinManager *man, WinData *win, int button );
extern void draw_added_icon (WinManager *man);
extern void draw_deleted_icon (WinManager *man);
extern int win_to_box (WinManager *man, WinData *win);
extern void draw_window (WinManager *man);
extern WinManager *find_windows_manager (Window win);
extern void map_new_manger (WinManager *man);
extern void move_highlight (WinManager *man, int box);
extern int move_win_iconlist (WinData *win);

