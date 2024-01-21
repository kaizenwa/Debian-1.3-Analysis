#define HAS_PALETTE_CODE 0

#include "config.h"
#include "jed-feat.h"

#include <windows.h>
#include <stdio.h>
#include <slang.h>

#include "jdmacros.h"

#include <process.h>
#include <stdlib.h>
#include <string.h>

#ifdef VC32
# include <sys\stat.h>
#else
# include <dir.h>
#endif

#include <time.h>
#include <dos.h>
#include <assert.h>
#include <io.h>
#include <errno.h>

#include "display.h"
#include "sysdep.h"
#include "screen.h"
#include "keymap.h"
#include "hooks.h"
#include "ins.h"
#include "ledit.h"
#include "misc.h"
#include "cmds.h"
#include "sig.h"

#if JED_HAS_SUBPROCESSES
# include "jprocess.h"
#endif

#ifndef _USERENTRY
# define _USERENTRY
#endif


#define MSW_STRING_TYPE   1
#define MSW_INT_TYPE      2
#define MSW_COLOR_TYPE    3

#define MAX_KEYS          128

#define KEY_SHIFT         1
#define KEY_CONTROL       2
#define KEY_ALT	 	  4

#define SPACE_CHAR        (32 | (JNORMAL_COLOR << 8))

#define MAX_MENU_ID       256

/* externs */
#ifndef VC32
extern HINSTANCE _hInstance;
#else
HINSTANCE _hInstance;
#endif
extern HINSTANCE _hPrev;

#ifdef VC32
typedef HANDLE HTASK, HCURSOR;
#endif

typedef struct
{
   char  *name;
   int    type;
   char  *dflt;
   void  *buf;
} Msw_Ini_Type;

typedef struct
{
   COLORREF bg;
   COLORREF fg;
   HBRUSH   hbrBG;
} Color_Type;

typedef struct
{
   char *name;
   int r, g, b;
} Color_Value_Type;

typedef struct
{
   char        title[30];
   HWND        w;
   HDC         hdc;
   int         ndc;
   
   char        font_name[50];
   int         font_height;
   int         font_bold;
   int         font_width;
   HFONT       font;
   
   int         x;
   int         y;
   int         height;
   int         width;
   
   int         scroll_r1, scroll_r2;        /* scrolling region */
   int         cursor_row, cursor_col;      /* row column of cursor (1,1) origin */
   int         vis_curs_row, vis_curs_col;  /* position of VISIBLE cursor */
   int         cursor_showing;
   
   Color_Type *current_color;
   
   int         focus;
} MSWindow_Type;

LRESULT CALLBACK JEDWndProc(HWND, UINT, WPARAM, LPARAM);

#ifndef __WIN32__
BOOL CALLBACK EnumWndProc(HWND, LPARAM);
#endif

void _USERENTRY msw_cleanup(void);
static void msw_get_defaults(void);
static void msw_get_default(Msw_Ini_Type *t);
static void get_dc(void);
static void release_dc(void);
static void init_application(void);
static void init_instance(void);
static void process_message(void);
static void show_cursor(void);
static void hide_cursor(void);
static void msw_normal_video(void);
static void set_window_name(char *s);
static void x_warp_pointer(void);
static int x_insert_cutbuffer(void);
static void x_region_2_cutbuffer(void);
static int msw_system(char *, int *, int *);
static int dos_system(char *);
static void msw_define_color(char *, int *, int *, int *);

static void get_menubar(void);
static void destroy_menubar(void);
static void create_popup_menu(void);
static void destroy_menu(void);
static void append_menu_item(void);
static void append_popup_menu(void);
static void append_separator(void);
static void insert_menu_item(void);
static void insert_menu_item_pos(void);
static void insert_popup_menu(void);
static void insert_popup_menu_pos(void);
static void insert_separator(void);
static void insert_separator_pos(void);
static void delete_menu_item(void);
static void delete_menu_item_pos(void);
static void get_menu_state(void);
static void get_menu_state_pos(void);
static void get_popup_menu(void);
static void check_menu_item(void);
static void check_menu_item_pos(void);
static void enable_menu_item(void);
static void enable_menu_item_pos(void);
static void redraw_menubar(void);

static void set_init_popup_callback(void);
static void msw_help(void);


#ifdef __WIN32__
    HINSTANCE _hPrev;
# if JED_HAS_SUBPROCESSES
    HANDLE Input_Events[MAX_PROCESSES + 1];
# endif
#endif


int Abort_Char = 7;
int NumLock_Is_Gold = 0;
int PC_Alt_Char = 27;

static HINSTANCE hPrevInst;
static HINSTANCE hInstance;
static char *szJedSection = "WJED";

static int JX_Screen_Cols;
static int JX_Screen_Rows;
static int JX_Term_Cannot_Scroll = 0;
static int JX_Term_Cannot_Insert = 0;
static int JX_Baud_Rate = 0;
static int JX_Use_Ansi_Colors = 1;
static int JX_Ignore_Beep = 3;
static int Performing_Update;

static MSWindow_Type This_Window;
static Color_Type colors[JMAX_COLORS];

static char *InitPopup_Callback;
static char *Menu_Callbacks[MAX_MENU_ID];

#if HAS_PALETTE_CODE
static LPLOGPALETTE The_Lplgpl = NULL;
static HPALETTE The_Hpalette = NULL;
#endif

Msw_Ini_Type Msw_Ini_List[] =
{
     { "Font",         MSW_STRING_TYPE,   "fixed",         This_Window.font_name },
     { "FontHeight",   MSW_INT_TYPE,      "12",            &This_Window.font_height },
     { "FontBold",     MSW_INT_TYPE,      "-1",             &This_Window.font_bold },
     { "Background",   MSW_COLOR_TYPE,    "0,0,0",         &colors[JNORMAL_COLOR].bg },
     { "Foreground",   MSW_COLOR_TYPE,    "192,192,192",   &colors[JNORMAL_COLOR].fg },
     { "StatusBG",     MSW_COLOR_TYPE,    "0,0,128",       &colors[JSTATUS_COLOR].bg },
     { "StatusFG",     MSW_COLOR_TYPE,    "255,255,0",     &colors[JSTATUS_COLOR].fg },
     { "RegionBG",     MSW_COLOR_TYPE,    "255,0,255",     &colors[JREGION_COLOR].bg },
     { "RegionFG",     MSW_COLOR_TYPE,    "255,255,255",   &colors[JREGION_COLOR].fg },
     { "OperatorBG",   MSW_COLOR_TYPE,    "0,0,0",         &colors[JOP_COLOR].bg },
     { "OperatorFG",   MSW_COLOR_TYPE,    "255,255,255",   &colors[JOP_COLOR].fg },
     { "NumberBG",     MSW_COLOR_TYPE,    "0,0,0",         &colors[JNUM_COLOR].bg },
     { "NumberFG",     MSW_COLOR_TYPE,    "0,0,192",       &colors[JNUM_COLOR].fg },
     { "StringBG",     MSW_COLOR_TYPE,    "0,0,0",         &colors[JSTR_COLOR].bg },
     { "StringFG",     MSW_COLOR_TYPE,    "0,192,255",     &colors[JSTR_COLOR].fg },
     { "CommentBG",    MSW_COLOR_TYPE,    "0,0,0",         &colors[JCOM_COLOR].bg },
     { "CommentFG",    MSW_COLOR_TYPE,    "0,128,0",       &colors[JCOM_COLOR].fg },
     { "KeywordBG",    MSW_COLOR_TYPE,    "0,0,0",         &colors[JKEY_COLOR].bg },
     { "KeywordFG",    MSW_COLOR_TYPE,    "255,255,255",   &colors[JKEY_COLOR].fg },
     { "Keyword1BG",   MSW_COLOR_TYPE,    "0,0,0",         &colors[JKEY_COLOR + 1].bg },
     { "Keyword1FG",   MSW_COLOR_TYPE,    "255,255,255",   &colors[JKEY_COLOR + 1].fg },
     { "DelimiterBG",  MSW_COLOR_TYPE,    "0,0,0",         &colors[JDELIM_COLOR].bg },
     { "DelimiterFG",  MSW_COLOR_TYPE,    "255,255,255",   &colors[JDELIM_COLOR].fg },
     { "PreprocessBG", MSW_COLOR_TYPE,    "0,0,0",         &colors[JPREPROC_COLOR].bg },
     { "PreprocessFG", MSW_COLOR_TYPE,    "0,255,0",       &colors[JPREPROC_COLOR].fg },
     { "MessageBG",    MSW_COLOR_TYPE,	  "0,0,0",         &colors[JMESSAGE_COLOR].bg},
     { "MessageFG",    MSW_COLOR_TYPE,	  "255,255,0",     &colors[JMESSAGE_COLOR].fg},
     { "ErrorFG",      MSW_COLOR_TYPE,	  "255,0,0",       &colors[JERROR_COLOR].fg},
     { "ErrorBG",      MSW_COLOR_TYPE,	  "0,0,0",         &colors[JERROR_COLOR].bg},
     { "MenuFG",       MSW_COLOR_TYPE,	  "0,0,0",         &colors[JMENU_COLOR].fg},
     { "MenuBG",       MSW_COLOR_TYPE,	  "0,255,255",     &colors[JMENU_COLOR].bg},
     {"CursorFG",      MSW_COLOR_TYPE,	  "0,255,0",       &colors[JCURSOR_COLOR].fg},
     {"CursorBG",      MSW_COLOR_TYPE,	  "255,0,0",       &colors[JCURSOR_COLOR].bg},
     {"DollarFG",      MSW_COLOR_TYPE,	  "0,0,255",       &colors[JDOLLAR_COLOR].fg},
     {"DollarBG",      MSW_COLOR_TYPE,	  "0,0,0",         &colors[JDOLLAR_COLOR].bg},
     { "Title",        MSW_STRING_TYPE,   "WJED",          &This_Window.title },
     { "X",            MSW_INT_TYPE,      "0",             &This_Window.x },
     { "Y",            MSW_INT_TYPE,      "0",             &This_Window.y },
     { "Width",        MSW_INT_TYPE,      "700",           &This_Window.width },
     { "Height",       MSW_INT_TYPE,      "500",           &This_Window.height },
     { NULL,           0,                 NULL,            NULL }
};

static Color_Value_Type Msw_Std_Color[] =
{
     {"black", 0, 0, 0},
     {"blue", 0, 0, 192},
     {"green", 0, 128, 0},
     {"cyan", 0, 192, 192},
     {"red", 192, 0, 0},
     {"magenta", 192, 0, 192},
     {"lightgray", 192, 192, 192},
     {"gray", 128, 128, 128},
     {"brightblue", 0, 0, 255},
     {"brightred", 255, 0, 0},
     {"brightgreen", 0, 255, 0},
     {"brightcyan", 0, 255, 255},
     {"brightmagenta", 255, 0, 255},
     {"yellow", 255, 255, 0},
     {"white", 255, 255, 255},
     {"brown", 110, 74, 32},
     {NULL, 0, 0, 0}
};



static SLang_Name_Type Sl_Msw_Table[] =
{
   MAKE_INTRINSIC(".x_set_window_name", set_window_name, VOID_TYPE, 1),
     MAKE_INTRINSIC(".x_warp_pointer", x_warp_pointer, VOID_TYPE, 0),
     MAKE_INTRINSIC(".x_insert_cutbuffer", x_insert_cutbuffer, INT_TYPE, 0),
     /* Prototype: Integer x_insert_cut_buffer ();
      * Inserts cutbuffer into the current buffer and returns the number
      * of characters inserted.
      */
     MAKE_INTRINSIC(".x_copy_region_to_cutbuffer", x_region_2_cutbuffer, VOID_TYPE, 0),
     /*Prototype: Void x_copy_region_to_cutbuffer();*/
     MAKE_INTRINSIC(".define_color", msw_define_color, VOID_TYPE, 4),
     /*Prototype: Void msw_define_color(char *, int, int, int);*/
     MAKE_INTRINSIC(".get_menubar", get_menubar, VOID_TYPE, 0),
     /* Prototype: Integer get_menubar(Void) 
      * Returns integer which is handle of menubar. If there is no
      * menubar, it creates it.
      * To show menubar, call `redraw_menu'
      */
     MAKE_INTRINSIC(".destroy_menubar", destroy_menubar, VOID_TYPE, 0),
     /* Prototype: Void destroy_menubar(Void) 
      * Destroys menubar
      */ 
     MAKE_INTRINSIC(".create_popup_menu", create_popup_menu, VOID_TYPE, 0),
     /* Prototype: Integer create_popup_menu(Void) 
      * Creates empty popup menu and returns integer value which is 
      * it's handle. If popup is not appended to another menu, it must 
      * destroyed after use.
      */
     MAKE_INTRINSIC(".destroy_menu", destroy_menu, VOID_TYPE, 0),
     /* Prototype: Void destroy_menu(Integer hmenu)
      * Destroys menu and all it's popup menus.
      * Note: Do not destroy menubar with this function 
      *       (use `destroy_menubar')
      */
     MAKE_INTRINSIC(".append_menu_item", append_menu_item, VOID_TYPE, 0),
     /* Prototype: Void append_menu_item(Integer hmenu, String name, Integer id, String callback) 
      * Appends menu item with name 'name' and identifier 'id' at the end 
      * of 'hmenu'. When item is selected, the 'callback' will be executed.
      * Callback can be intrinsic or internal function.
      */
     MAKE_INTRINSIC(".append_popup_menu", append_popup_menu, VOID_TYPE, 0),
     /* Prototype: Void append_popop_menu(Integer hmenu, String name, Integer popup) 
      * Appends popup menu with name 'name' and handle 'popup' at the end 
      * of 'hmenu'
      */ 
     MAKE_INTRINSIC(".append_separator", append_separator, VOID_TYPE, 0),
     /* Prototype: Void append_separator(Integer hmenu) 
      * Appends menu separator at the end of 'hmenu' 
      */
     MAKE_INTRINSIC(".insert_menu_item", insert_menu_item, VOID_TYPE, 0),
     /* Prototype: Void insert_menu_item(Integer hmenu, Integer id, String name, Integer idNew, String callback)
      * Inserts menu item with name 'name' and identifier 'idNew' before
      * menu item with identifier 'id'.
      * When item is selected, the 'callback' will be executed.
      * Callback can be intrinsic or internal function.
      */ 
     MAKE_INTRINSIC(".insert_menu_item_pos", insert_menu_item_pos, VOID_TYPE, 0),
     /* Prototype: Void insert_menu_item_pos(Integer hmenu, Integer pos, String name, Integer idNew, String callback)
      * Inserts menu item with name 'name' and identifier 'idNew' before
      * menu item with zero-based position 'pos' in 'hmenu'.
      * When item is selected, the 'callback' will be executed.
      * Callback can be intrinsic or internal function.
      */ 
     MAKE_INTRINSIC(".insert_popup_menu", insert_popup_menu, VOID_TYPE, 0),
     /* Prototype: Void insert_popup_menu(Integer hmenu, Integer id, String name, Integer popup) 
      * Inserts popup menu with name 'name' and handle 'popup' before
      * menu item with identifier 'id'
      */ 
     MAKE_INTRINSIC(".insert_popup_menu_pos", insert_popup_menu_pos, VOID_TYPE, 0),
     /* Prototype: Void insert_popup_menu_pos(Integer hmenu, Integer pos, String name, Integer popup) 
      * Inserts popup menu with name 'name' and handle 'popup' before
      * menu item with zero-based position 'pos' in 'hmenu'
      */ 
     MAKE_INTRINSIC(".insert_separator", insert_separator, VOID_TYPE, 0),
     /* Prototype: Void insert_separator(Integer hmenu, Integer id)
      * Inserts menu separator before menu item with identifier 'id'
      */ 
     MAKE_INTRINSIC(".insert_separator_pos", insert_separator_pos, VOID_TYPE, 0),
     /* Prototype: Void insert_separator_pos(Integer hmenu, Integer pos) 
      * Inserts menu separator before menu item with zero-based position 'pos'
      */ 
     MAKE_INTRINSIC(".delete_menu_item", delete_menu_item, VOID_TYPE, 0),
     /* Prototype: Void delete_menu_item(Integer hmenu, Integer id) 
      * Deletes menu item with identifier id from menu with handle 'hmenu'
      */ 
     MAKE_INTRINSIC(".delete_menu_item_pos", delete_menu_item_pos, VOID_TYPE, 0),
     /* Prototype: Void delete_menu_item_pos(Integer hmenu, Integer pos) 
      * Deletes menu item at zero-based position 'pos' from menu 'hmenu'
      */ 
     MAKE_INTRINSIC(".get_menu_state", get_menu_state, VOID_TYPE, 0),
     /* Prototype: Integer get_menu_state(Integer hmenu, Integer id) 
      * Gets state of menu item with identifier 'id'
      * <return value> & 1 == 1 if menu item is enabled
      * <return value> & 2 == 1 if menu item is checked
      */ 
     MAKE_INTRINSIC(".get_menu_state_pos", get_menu_state_pos, VOID_TYPE, 0),
     /* Prototype: Integer get_menu_state(Integer hmenu, Integer pos) 
      * Gets state of menu item at zero-based position 'pos' 
      * <return value> & 1 == 1 if menu item is enabled
      * <return value> & 2 == 1 if menu item is checked
      */ 
     MAKE_INTRINSIC(".get_popup_menu", get_popup_menu, VOID_TYPE, 0),
     /* Prototype: Void get_popup_menu(Integer hmenu, Integer pos) 
      * Returns handle of popup menu at zero-based position 'pos'
      * If return value is 0, there is no popup at the position.
      */ 
     MAKE_INTRINSIC(".check_menu_item", check_menu_item, VOID_TYPE, 0),
     /* Prototype: Void check_menu_item(Integer hmenu, Integer id, Integer flag) 
      * This functions changes check state of menu item. If flag is nonzero, 
      * it checks menu item, otherwise it unchecks it
      */ 
     MAKE_INTRINSIC(".check_menu_item_pos", check_menu_item_pos, VOID_TYPE, 0),
     /* Prototype: Void check_menu_item(Integer hmenu, Integer pos, Integer flag) 
      * This functions changes check state of menu item. If flag is nonzero, 
      * it checks menu item, otherwise it unchecks it
      */ 
     MAKE_INTRINSIC(".enable_menu_item", enable_menu_item, VOID_TYPE, 0),
     /* Prototype: Void check_menu_item(Integer hmenu, Integer id, Integer flag) 
      * This functions enable or disable menu item. If flag is nonzero, the
      * menu item will be enabled, otherwise it'll be disabled.
      */ 
     MAKE_INTRINSIC(".enable_menu_item_pos", enable_menu_item_pos, VOID_TYPE, 0),
     /* Prototype: Void check_menu_item(Integer hmenu, Integer pos, Integer flag) 
      * This functions enable or disable menu item. If flag is nonzero, the
      * menu item will be enabled, otherwise it'll be disabled.
      */ 
     MAKE_INTRINSIC(".redraw_menubar", redraw_menubar, VOID_TYPE, 0),
     /* Prototype: Void redraw_menubar(Void) 
      * Redraws menubar. This functions should be called if menubar is changed
      */
     
     MAKE_INTRINSIC(".set_init_popup_callback", set_init_popup_callback, VOID_TYPE, 0),
     /* Prototype: Void set_init_popup_callback(String callback)
      * Executes callback before menu poppup was popped up.
      */
     
     MAKE_INTRINSIC(".msw_help", msw_help, VOID_TYPE, 0),
     /* Prototype: Void msw_help(String filename, String keword, Integer Partial_Keys) 
      * Starts Windows Help with 'filename' help file. If 'keyword' is not null
      * string shows topic with specified keyword. If 'Partial_Keys' != 0
      * shows Search dialog if there is more than one help topic beginnig with
      * 'keyword'
      */
     SLANG_END_TABLE
};

/* Key storage functions */
static void _putkey(char c)
{
   int ch = c;
   ungetkey(&ch);
}


/* Getting defaults from INI file */
static COLORREF msw_get_color(char *s, char *dflt)
{
   
   if (s[0] >= '0' && s[0] <= '9')
     {
	long r, g, b;
	char *sptr, *endptr;
	
	sptr = s;
	r = strtol(sptr, &endptr, 0);
	while ((*endptr == ' ') || (*endptr == '\t')) endptr++; /* skipping whitespace */
	if ((sptr == endptr) || (*endptr++ != ',')) return msw_get_color(dflt, dflt);
	sptr = endptr;
	
	g = strtol(sptr, &endptr, 0);
	while ((*endptr == ' ') || (*endptr == '\t')) endptr++; /* skipping whitespace */
	
	if ((sptr == endptr) || (*endptr++ != ',')) return msw_get_color(dflt, dflt);
	sptr = endptr;
	
	b = strtol(sptr, &endptr, 0);
	while ((*endptr == ' ') || (*endptr == '\t')) endptr++; /* skipping whitespace */
	if ((sptr == endptr) || (*endptr++ != 0)) return msw_get_color(dflt, dflt);
	
#if HAS_PALETTE_CODE
	return PALETTERGB((BYTE)r, (BYTE)g, (BYTE)b);
#else
	return RGB((BYTE)r, (BYTE)g, (BYTE)b);
#endif
     }
   else
     {
	char buf[50];
	GetProfileString(szJedSection, s, dflt, buf, sizeof(buf));
	if (buf[0] >='0' && buf[0] <= '9')
	  return msw_get_color(buf, dflt);
	else
	  return msw_get_color(dflt, dflt);
     }
}

static void msw_get_default(Msw_Ini_Type *t)
{
   char s[50];
   
   GetProfileString(szJedSection, t->name, t->dflt, s, sizeof(s));
   
   switch (t->type)
     {
      case MSW_STRING_TYPE:
	strcpy((char *)t->buf, s);
	break;
	
      case MSW_INT_TYPE:
	*(int *)t->buf = atoi(s);
	break;
	
      case MSW_COLOR_TYPE:
	*(COLORREF *)t->buf = msw_get_color(s, t->dflt);
	break;
     }
   
}


static void msw_get_defaults(void)
{
   int i = 0;
   char buf[20];
   
   /* Check for standard color names in INI file.
    * Add they if cannot find. */
   while (Msw_Std_Color[i].name != NULL)
     {
	if (!GetProfileString(szJedSection, Msw_Std_Color[i].name, "", buf, sizeof(buf)))
	  msw_define_color(Msw_Std_Color[i].name, &Msw_Std_Color[i].r, &Msw_Std_Color[i].g, &Msw_Std_Color[i].b);
	
	i++;
     }
   
   i = 0;
   while (Msw_Ini_List[i].name != NULL)
     msw_get_default(&Msw_Ini_List[i++]);
}


static void get_dc(void)
{
   
   if (!This_Window.ndc)
     {
	This_Window.hdc = GetDC(This_Window.w);
#if HAS_PALETTE_CODE
	SelectPalette (This_Window.hdc, The_Hpalette, FALSE);
#endif
     }
   This_Window.ndc++;
}


static void release_dc(void)
{
   assert(This_Window.ndc);
   
   if (This_Window.ndc == 1) ReleaseDC(This_Window.w, This_Window.hdc);
   This_Window.ndc--;
}

static void process_message(void)
{
   MSG msg;
   
   if (
#ifdef VC32
       GetMessage(&msg, NULL, 0, 0)
#else
       PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)
#endif
       )
     {
	TranslateMessage(&msg);
	DispatchMessage(&msg);
     }
}

static void init_application(void)
{
   WNDCLASS wc;
   
   wc.style = 0;
   wc.lpfnWndProc = JEDWndProc;
   wc.cbClsExtra = 0;
   wc.cbWndExtra = 0;
   wc.hInstance = hInstance;
   wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
   wc.hCursor = LoadCursor(NULL, IDC_IBEAM);
   wc.hbrBackground = NULL;
   wc.lpszMenuName = NULL;
   wc.lpszClassName = "wjed";
   
   RegisterClass(&wc);
}


static void init_instance(void)
{
   int i;
   TEXTMETRIC tm;
   int font_weight;
   
   This_Window.hdc = NULL;
   This_Window.ndc = 0;
   
   for (i = 0; i < MAX_MENU_ID; i++) Menu_Callbacks[i] = NULL;
   InitPopup_Callback = NULL;
   
   msw_get_defaults();
   
   /* creating fonts, brushes etc */
   if (This_Window.font_bold < 0)
     font_weight = FW_DONTCARE;
   else
     font_weight = (This_Window.font_bold > 0) ? FW_BOLD : FW_NORMAL;
   
   This_Window.font = CreateFont(-This_Window.font_height, 0, 0, 0,
				 font_weight,
				 0, 0, 0, DEFAULT_CHARSET, 0, 0, 0, FIXED_PITCH,
				 This_Window.font_name);
#if !HAS_PALETTE_CODE
   for (i = 0; i < JMAX_COLORS; i++)
     colors[i].hbrBG = CreateSolidBrush(colors[i].bg);
#endif

   msw_normal_video();
   
   /* creating window */
   This_Window.w = CreateWindow("wjed", This_Window.title, WS_OVERLAPPEDWINDOW, This_Window.x, 
				This_Window.y, This_Window.width,This_Window.height, NULL, NULL, 
				hInstance, NULL);
#if HAS_PALETTE_CODE
   get_dc();
   if ((RC_PALETTE & GetDeviceCaps (This_Window.hdc, RASTERCAPS)) &&
       (NULL != (The_Lplgpl = malloc (sizeof(*The_Lplgpl) - sizeof(The_Lplgpl->palPalEntry)
				      + 2*JMAX_COLORS * sizeof(PALETTEENTRY)))))
     {
	The_Lplgpl->palVersion = 0x300;
	The_Lplgpl->palNumEntries = JMAX_COLORS*2;
	for (i = 0; i < JMAX_COLORS; i++)
	  {
	     The_Lplgpl->palPalEntry[i*2].peRed = GetRValue (colors[i].fg);
	     The_Lplgpl->palPalEntry[i*2].peGreen = GetGValue (colors[i].fg);
	     The_Lplgpl->palPalEntry[i*2].peBlue = GetBValue (colors[i].fg);
	     The_Lplgpl->palPalEntry[i*2].peFlags = PC_NOCOLLAPSE;
	     
	     The_Lplgpl->palPalEntry[i*2+1].peRed = GetRValue (colors[i].bg);
	     The_Lplgpl->palPalEntry[i*2+1].peGreen = GetGValue (colors[i].bg);
	     The_Lplgpl->palPalEntry[i*2+1].peBlue = GetBValue (colors[i].bg);
	     The_Lplgpl->palPalEntry[i*2+1].peFlags = PC_NOCOLLAPSE;
	  }
	The_Hpalette = CreatePalette (The_Lplgpl);
	if (The_Hpalette)
	  {
	     SelectPalette (This_Window.hdc, The_Hpalette, FALSE);
	     RealizePalette (This_Window.hdc);
	  }
     }
   ReleaseDC (This_Window.w, This_Window.hdc);
   
   for (i = 0; i < JMAX_COLORS; i++)
     colors[i].hbrBG = CreateSolidBrush(colors[i].bg);
#endif				       /* HAS_PALETTE_CODE */
   
#ifdef VC32
   SetTimer (This_Window.w, 43, 100, NULL);
#endif
   
   /* function that will be called when jed exits (deletes fonts, brushes etc.) */
   atexit(msw_cleanup);
   
   /* retrieve font metrics (width and base) */
   get_dc();
   
   SelectObject(This_Window.hdc, This_Window.font);
   GetTextMetrics(This_Window.hdc, &tm);
   This_Window.font_width = tm.tmAveCharWidth;
   This_Window.font_height = tm.tmHeight;
   release_dc();
   
   ShowWindow(This_Window.w, SW_SHOW);
   UpdateWindow(This_Window.w);
   
#if !defined(__WIN32__) || !defined(HAS_SUBPROCESS)
   SetTimer(This_Window.w, 42, 30000, NULL);     /* used for updating display time */
#endif
}

int init_tty (void)
{
   if (Batch) return 0;
   
   hInstance = _hInstance;
   hPrevInst = _hPrev;
   
   if (!hPrevInst) init_application();
   
   init_instance();
   return 0;
}

void reset_tty(void)
{
}

static void copy_rect(int x1, int y1, int x2, int y2, int x3, int y3)
{
   int dx, dy;
   RECT rcSrc;
   
   dx = (x3 - x1) * This_Window.font_width;
   dy = (y3 - y1) * This_Window.font_height;
   
   SetRect(&rcSrc, x1 * This_Window.font_width, y1 * This_Window.font_height, x2 * This_Window.font_width, y2 * This_Window.font_height);
   
   ScrollWindow(This_Window.w, dx, dy, &rcSrc, NULL);

#ifdef __WIN32__
   UpdateWindow(This_Window.w);
#endif
}



static void blank_rect(int x1, int y1, int x2, int y2)
{
   RECT rc;
   
   SetRect(&rc, x1 * This_Window.font_width, y1 * This_Window.font_height, x2 * This_Window.font_width, y2 * This_Window.font_height);
   get_dc();
   FillRect(This_Window.hdc, &rc, colors[JNORMAL_COLOR].hbrBG);
   release_dc();
}


/* This routine assumes that cursor is in the correct location.  The
 cursor is placed at the end of the string. */
static void tt_write(char *s, int n)
{
   get_dc();
   
   if (This_Window.cursor_showing) hide_cursor();
   SelectObject(This_Window.hdc, This_Window.font);
   SetTextColor(This_Window.hdc, This_Window.current_color->fg);
   SetBkColor(This_Window.hdc, This_Window.current_color->bg);
   TextOut(This_Window.hdc, This_Window.cursor_col * This_Window.font_width, This_Window.cursor_row * This_Window.font_height, s, n);
   
   This_Window.cursor_col += n;
   
   if (This_Window.cursor_col >= JX_Screen_Cols) This_Window.cursor_col = JX_Screen_Cols - 1;
   
   release_dc();
}

static void hide_cursor(void)
{
   unsigned short *s;
   char ch;
   int col = This_Window.vis_curs_col, row = This_Window.vis_curs_row;
   Color_Type *color;
   
   if (This_Window.cursor_showing == 0) return;
   
   This_Window.cursor_showing = 0;
   
   s = JScreen[row].old;
   if (s == NULL) return;
   s += col;
   ch = (char) (*s & 0xFF);
   
   get_dc();
   color = &colors[*s >> 8];
   SelectObject(This_Window.hdc, This_Window.font);
   SetTextColor(This_Window.hdc, color->fg);
   SetBkColor(This_Window.hdc, color->bg);
   TextOut(This_Window.hdc, This_Window.cursor_col * This_Window.font_width, This_Window.cursor_row * This_Window.font_height, &ch, 1);
   release_dc();
}


static void show_cursor(void)
{
   unsigned short *s;
   char ch;
   int c, r;
   Color_Type *curs_color;
   RECT rc;
   
   if (This_Window.cursor_showing) hide_cursor();
   
   This_Window.cursor_showing = 1;
   
   r = This_Window.vis_curs_row = This_Window.cursor_row;
   c = This_Window.vis_curs_col = This_Window.cursor_col;
   
   s = JScreen[r].old;
   if (s == NULL) return;
   
   s += c;
   ch = (char) (*s & 0xFF);
   curs_color = &colors[JCURSOR_COLOR];
   
   get_dc();
   if (This_Window.focus)
     {
	SelectObject(This_Window.hdc, This_Window.font);
	SetTextColor(This_Window.hdc, curs_color->fg);
	SetBkColor(This_Window.hdc, curs_color->bg);
	TextOut(This_Window.hdc, This_Window.cursor_col * This_Window.font_width, This_Window.cursor_row * This_Window.font_height, &ch, 1);
     }
   else
     {
	rc.left = This_Window.cursor_col * This_Window.font_width;
	rc.top = This_Window.cursor_row * This_Window.font_height;
	rc.right = rc.left + This_Window.font_width;
	rc.bottom = rc.top + This_Window.font_height;
	FrameRect(This_Window.hdc, &rc, curs_color->hbrBG);
     }
   
   release_dc();
}

#if !defined(__WIN32__) || !JED_HAS_SUBPROCESSES
unsigned char sys_getkey(void)
{
   int n;
   
   while (!SLKeyBoard_Quit && !Input_Buffer_Len) process_message ();
   
   if (SLKeyBoard_Quit) return Abort_Char;
   
   n = my_getkey();
   
   SLKeyBoard_Quit = 0;
   
   return n;
}

int sys_input_pending(int *tsecs, int unused)
{
   DWORD t = GetTickCount() + *tsecs * 100L;
   
   (void) unused;
   
   while ((!Input_Buffer_Len) && (GetTickCount() < t)) process_message();
   
   return Input_Buffer_Len != 0;
}

#else
unsigned char sys_getkey(void)
{
   int n = 450;
   
   if (SLKeyBoard_Quit) return((unsigned char)Abort_Char);
   /* sleep for 45 second and try again */
   while (!SLKeyBoard_Quit && !sys_input_pending(&n, Num_Subprocesses))
      {
	 /* update status line incase user is displaying time */
	 if (SLKeyBoard_Quit) break;
	 JWindow->trashed = 1;
	 update((Line *) NULL, 0, 1);
      }
   
   if (SLKeyBoard_Quit) return (unsigned char)Abort_Char;
   
   n = my_getkey();
   
   SLKeyBoard_Quit = 0;
   
   return (unsigned char) n;
}

int sys_input_pending(int *tsecs, int all)
{
   DWORD ret;
   int i, n;
   
   if ((all >= 0) && (Input_Buffer_Len || Batch)) return (Input_Buffer_Len);
   
   if (all < 0)
      {
	 ret = WaitForMultipleObjects(Num_Subprocesses, Input_Events, FALSE, *tsecs * 100);
      }
   else
      {
	 DWORD t;
	 long rtime = *tsecs * 100L;
	 
	 do
	    {
	       t = GetTickCount();
	       ret = MsgWaitForMultipleObjects(Num_Subprocesses, Input_Events, FALSE, rtime, QS_ALLINPUT);
	       
	       if ((WAIT_OBJECT_0 + Num_Subprocesses) != ret) break;
	       rtime -= GetTickCount() - t;
	       
	       process_message();
	    }
	 while ((rtime > 0) && !Input_Buffer_Len);
	 
	 if ((rtime < 0) || Input_Buffer_Len)
	   return Input_Buffer_Len;
      }
   
   if (WAIT_TIMEOUT == ret) return 0;
   
   i = n = 0;
   while (i < Num_Subprocesses)
      {
	 /* Check if current subprocess has input */
	 if (WAIT_TIMEOUT != WaitForSingleObject(Input_Events[i], 0))
	    {
	       read_process_input (i);
	       n++;
	    }
	 i++;
      }
   if (all < 0) return n;
   else return 0;
}
#endif 

void sys_pause (int ms)
{
   DWORD t = GetTickCount() + ms;
   
   while (GetTickCount() < t)
     process_message();
}


void sys_suspend(void)
{
   ShowWindow(This_Window.w, SW_MINIMIZE);
}


int get_term_dimensions(int *cols, int *rows)
{
   *cols = This_Window.width / This_Window.font_width;
   *rows = This_Window.height / This_Window.font_height;
   
   return 0;
}

/* Hooks */
static void msw_update_open (void)
{
   hide_cursor ();
   Performing_Update = 1;
}

static void msw_update_close (void)
{
   Performing_Update = 0;
   if (JWindow->trashed) return;
   show_cursor ();
}

static void msw_suspend (void)
{
   WINDOWPLACEMENT wndpl;
   
   GetWindowPlacement(This_Window.w, &wndpl);
   
   if (wndpl.showCmd == SW_MINIMIZE)
     ShowWindow(This_Window.w, SW_NORMAL);
   else
     ShowWindow(This_Window.w, SW_MINIMIZE);
}

static int msw_init_slang (void)
{
   SLadd_name ("msw_system", (long) msw_system, SLANG_INTRINSIC, SLANG_MAKE_ARGS(INT_TYPE, 3));
   return SLang_add_table(Sl_Msw_Table, "MSWJed")
     && SLdefine_for_ifdef("MSWINDOWS")
     && SLdefine_for_ifdef("MOUSE");
}


static void msw_define_xkeys (SLKeyMap_List_Type *map)
{
   SLkm_define_key ("^[Ow", (FVOID_STAR) bob, map);
   SLkm_define_key ("^[Oq", (FVOID_STAR) eob, map);
   SLkm_define_key ("\xE0\xE0", (FVOID_STAR) ins_char_cmd, map);
}

/* This routine is called from S-Lang inner interpreter.  It serves
 as a poor mans version of an interrupt 9 handler */
static void msw_check_kbd(void)
{
   MSG msg;
   
   if (Batch) return;
   
   while (PeekMessage(&msg, 0, 0, 0, PM_NOREMOVE))
     process_message();
}

/* Terminal functions */
static void msw_goto_rc(int r, int c)
{
   get_dc();
   if (This_Window.cursor_showing) hide_cursor();
   
   if (r > JX_Screen_Rows) r = JX_Screen_Rows;
   if (c > JX_Screen_Cols) c = JX_Screen_Cols;
   
   This_Window.cursor_row = r + This_Window.scroll_r1;
   This_Window.cursor_col = c;
   
   if (!Performing_Update) show_cursor();
   release_dc();
}

static void msw_begin_insert(void)
{
   hide_cursor();
   copy_rect(This_Window.cursor_col, This_Window.cursor_row, JX_Screen_Cols - 1, This_Window.cursor_row + 1,
	     This_Window.cursor_col + 1, This_Window.cursor_row);
}

static void msw_end_insert(void)
{
}

static void msw_del_eol(void)
{
   blank_rect(This_Window.cursor_col, This_Window.cursor_row, JX_Screen_Cols, This_Window.cursor_row + 1);
}

static void msw_delete_nlines(int n)
{
   int r1, r2;
   r1 = This_Window.cursor_row;
   r2 = This_Window.scroll_r2;
   
   if (r1 <= r2 - n) copy_rect(0, r1 + n, JX_Screen_Cols, r2 + 1, 0, r1);
   if (Scroll_By_Copying == 0) blank_rect(0, r2 - n, JX_Screen_Cols, r2);
}

static void msw_delete_char(void)
{
   copy_rect(This_Window.cursor_col + 1, This_Window.cursor_row, JX_Screen_Cols, This_Window.cursor_row + 1,
	     This_Window.cursor_col, This_Window.cursor_row);
}

static void msw_erase_line(void)
{
   blank_rect(0, This_Window.cursor_row, JX_Screen_Cols, This_Window.cursor_row + 1);
}

static int Rev_Vid_Flag;
static void msw_reverse_video(int color)
{
   Rev_Vid_Flag = color;
   This_Window.current_color = &colors[color];
}

static void msw_normal_video(void)
{
   Rev_Vid_Flag = JNORMAL_COLOR;
   This_Window.current_color = &colors[JNORMAL_COLOR];
}


static void msw_cls(void)
{
   RECT rc;
   
   get_dc();
   GetClientRect(This_Window.w, &rc);
   FillRect(This_Window.hdc, &rc, colors[JNORMAL_COLOR].hbrBG);
   release_dc();
}

static void msw_beep(void)
{
   if (JX_Ignore_Beep & 0x1) MessageBeep(0);
   
   if (JX_Ignore_Beep & 0x2) 
      {
	 RECT rc;
	 
	 get_dc();
	 GetClientRect(This_Window.w, &rc);
	 InvertRect(This_Window.hdc, &rc);
	 release_dc();
	 
	 InvalidateRect(This_Window.w, NULL, TRUE);
      }
   MessageBeep(0);
}

static void msw_reverse_index(int n)
{
   int r1, r2;
   
   r1 = This_Window.scroll_r1;
   r2 = This_Window.scroll_r2;
   
   if (r2 >= r1 + n) copy_rect(0, r1, JX_Screen_Cols, r2 - n + 1, 0, r1 + n);
   
   if (Scroll_By_Copying == 0) blank_rect(0, r1, JX_Screen_Cols, r1 + n);
}


static void msw_write_string (char *s)
{
   get_dc();
   tt_write(s, strlen(s));
   if (!Performing_Update) show_cursor ();
   release_dc();
}



static void send_attr_str(unsigned short *s, unsigned short *smax)
{
   unsigned char out[250], ch, attr, *p;
   register unsigned short sh;
   
   p = out;
   get_dc();
   while (s < smax)
     {
	sh = (unsigned short) *s++;
	ch = sh & 0xFF;
	attr = sh >> 8;
	if ((attr == 0) && (Rev_Vid_Flag != 0))
	  {
	     if (p != out)
	       {
		  *p = 0;
		  msw_write_string ((char *) out);
		  p = out;
	       }
	     tt_normal_video();
	     /* Rev_Vid_Flag = 0; */
	  }
	else if ((attr != 0) && (Rev_Vid_Flag != attr))
	  {
	     if (p != out)
	       {
		  *p = 0;
		  msw_write_string ((char *) out);
		  p = out;
	       }
	     msw_reverse_video(attr);
	     /* Rev_Vid_Flag = 1; */
	  }
	*p++ = ch;
     }
   *p = 0;
   if (p != out) msw_write_string ((char *) out);
   /* if (Rev_Vid_Flag) tt_normal_video(); */
   release_dc();
}

static void msw_smart_puts(unsigned short *neww, unsigned short *oldd, int len, int row)
{
   if (0 == memcmp ((char *) neww, (char *) oldd, len * sizeof (short)))
     return;
   
   msw_goto_rc (row, 0);
   send_attr_str (neww, neww + len);
}

/* This function is called assuming that cursor is in correct position */
static void msw_putchar(char ch)
{
   if (ch == '\b')
     {
	ch = ' ';
	if (This_Window.cursor_col == 0) return;
	This_Window.cursor_col--;
     }
   
   get_dc();
   if (Rev_Vid_Flag != JNORMAL_COLOR) tt_normal_video();
   tt_write(&ch, 1);
   show_cursor ();
   release_dc();
}

static void msw_init_video (void)
{
}

static void msw_reset_video (void)
{
   tt_normal_video ();
}

static void msw_set_scroll_region(int r1, int r2)
{
   This_Window.scroll_r1 = r1;
   This_Window.scroll_r2 = r2;
}


static void msw_reset_scroll_region()
{
   This_Window.scroll_r1 = 0;
   This_Window.scroll_r2 = JX_Screen_Cols - 1;
}

static void msw_get_terminfo()
{
   JX_Screen_Cols = 80;
   JX_Screen_Rows = 24;
   Scroll_By_Copying = 1;
   
   /* init hooks */
   X_Update_Open_Hook = msw_update_open;
   X_Update_Close_Hook = msw_update_close;
   X_Suspend_Hook = msw_suspend;
   /*   X_Argc_Argv_Hook = X_eval_command_line; */
   X_Init_SLang_Hook = msw_init_slang;
   X_Define_Keys_Hook = msw_define_xkeys;
   SLang_Interrupt = msw_check_kbd;
   
   /* Set this so that main will not try to read from stdin.  It is quite
    * likely that this is started from a menu or something.
    */
   Stdin_Is_TTY = -1;
}

static void msw_set_color (int i, char *what, char *fg, char *bg)
{
   int r, g, b;
   char buf[30];

   if ((i < 0) || (i > JMAX_COLORS))
     return;

   (void) what;
   r = GetRValue(colors[i].fg);
   g = GetGValue(colors[i].fg);
   b = GetBValue(colors[i].fg);
   sprintf(buf, "%d,%d,%d", r, g, b);
   colors[i].fg = msw_get_color(fg, buf);
   
   r = GetRValue(colors[i].bg);
   g = GetGValue(colors[i].bg);
   b = GetBValue(colors[i].bg);
   sprintf(buf, "%d,%d,%d", r, g, b);
   colors[i].bg = msw_get_color(bg, buf);
   
#if HAS_PALETTE_CODE
   if (The_Hpalette)
     {
	The_Lplgpl->palPalEntry[i*2].peRed = GetRValue (colors[i].fg);
	The_Lplgpl->palPalEntry[i*2].peGreen = GetGValue (colors[i].fg);
	The_Lplgpl->palPalEntry[i*2].peBlue = GetBValue (colors[i].fg);
	The_Lplgpl->palPalEntry[i*2].peFlags = PC_NOCOLLAPSE;
	The_Lplgpl->palPalEntry[i*2+1].peRed = GetRValue (colors[i].bg);
	The_Lplgpl->palPalEntry[i*2+1].peGreen = GetGValue (colors[i].bg);
	The_Lplgpl->palPalEntry[i*2+1].peBlue = GetBValue (colors[i].bg);
	The_Lplgpl->palPalEntry[i*2+1].peFlags = PC_NOCOLLAPSE;
	SetPaletteEntries (The_Hpalette, i*2, 2, The_Lplgpl->palPalEntry+i*2);
	get_dc();
	SelectPalette (This_Window.hdc, The_Hpalette, FALSE);
	RealizePalette (This_Window.hdc);
	release_dc();
     }
#endif
   DeleteObject(colors[i].hbrBG);
   colors[i].hbrBG = CreateSolidBrush(colors[i].bg);
   
   /* InvalidateRect(This_Window.w, NULL, FALSE); */
}

static void msw_narrow_width (void)
{
}
static void  msw_wide_width (void)
{
}

static void msw_enable_cursor_keys(void)
{
}

static void msw_set_term_vtxxx (int *n)
{
   (void) n;
}

typedef struct _Process_Info
{
   HINSTANCE hInstance;
   HWND hWnd;
   HTASK hTask;
} Process_Info;

int msw_system(char *command_line, int *nCmdShow, int *wait)
{
   UINT retcode;
   HCURSOR hcur, hcur_old;
   MSG msg;
#ifndef __WIN32__
   WNDENUMPROC enumproc;
   Process_Info pi;
   
   retcode = WinExec(command_line, *nCmdShow);
   
   if (retcode < 32)
     {
	switch (retcode)
	  {
	   case 0:
	   case 11:
	   case 12:
	   case 14:
	   case 15:
	     msg_error("Invalid EXE file");
	     break;
	     
	   case 2:
	     msg_error("File not found");
	     break;
	     
	   case 3:
	     msg_error("Path not found");
	     break;
	     
	   case 8:
	     msg_error("Out of memory");
	     break;
	     
	   case 10:
	     msg_error("Incorrect MS Windows version");
	     break;
	     
	   case 16:
	     msg_error("Cannot run more that one instance of application");
	     break;
	     
	   case 20:
	     msg_error("Cannot find one of required DLL's");
	     break;
	     
	   case 21:
	     msg_error("Application requires MS Windows 32-bit extension");
	     break;
	     
	   default:
	     msg_error("Unknown error");
	  }
	return retcode;
     }   
#else
   STARTUPINFO si;
   PROCESS_INFORMATION pi;
   /* DWORD dwExitCode; */

   memset ((char *) &si, 0, sizeof (STARTUPINFO));
   /* si.lpReserved = si.lpDesktop = si.lpTitle = NULL;
    * si.lpReserved2 = NULL;
    * si.cbReserved2 = NULL;
    */
   si.wShowWindow = *nCmdShow;
   si.dwFlags = STARTF_USESHOWWINDOW;
   si.cb = sizeof(si);
   retcode = CreateProcess (NULL, command_line, NULL, NULL,
			    0, NORMAL_PRIORITY_CLASS, NULL,
			    NULL, &si, &pi);
   if (!retcode)
     {
	msg_error ("Unable to create process");
	return 1;
     }
#endif				       /* NOT __WIN32__ */
      
   if (!*wait) return 0;

#ifndef __WIN32__
   pi.hInstance = (HINSTANCE) retcode;
   pi.hWnd = NULL;
   pi.hTask = NULL;
   
   enumproc = (WNDENUMPROC) MakeProcInstance((FARPROC)EnumWndProc,hInstance);
   EnumWindows(enumproc, (LPARAM) &pi);
   FreeProcInstance((FARPROC)enumproc);
#endif
   
   hcur = LoadCursor(NULL, IDC_WAIT);
   hcur_old = SetCursor(hcur);
#ifdef __WIN32__
# if 0
   while (GetExitCodeProcess(pi.hProcess, &dwExitCode) &&
	  dwExitCode == STILL_ACTIVE)
     {
	if (PeekMessage(&msg, 0, 0, 0, PM_NOREMOVE))
	  {
	      process_message();
	      SetCursor(hcur);
	  }
	Sleep(50);
     }
# else
   WaitForSingleObject(pi.hProcess, INFINITE);
# endif
#else

   while (1)
      {
	 if (!IsWindow(pi.hWnd) || !IsTask(pi.hTask)) break;
	 
	 if ((HINSTANCE) GetWindowWord(pi.hWnd, GWW_HINSTANCE) != pi.hInstance) break;
	 if (GetWindowTask(pi.hWnd) != pi.hTask) break;
	 
	 if (PeekMessage(&msg, 0, 0, 0, PM_NOREMOVE))
	    {
	       process_message();
	       SetCursor(hcur);
	    }
      }
#endif   
   SetCursor(hcur_old);
   return 0;
}

#ifndef __WIN32__
BOOL CALLBACK EnumWndProc(HWND hWnd, LPARAM lParam)
{
   Process_Info *pi = (Process_Info *) lParam;
   
   if ((HINSTANCE) GetWindowWord(hWnd, GWW_HINSTANCE) == pi->hInstance)
      {
	 pi->hWnd = hWnd;
	 pi->hTask = GetWindowTask(hWnd);
	 return FALSE;
      }
   
   return TRUE;
}
#endif 				       /* NOT __WIN32__ */

int sys_System(char *command_line)
{
   return dos_system(command_line);
}

static int dos_system(char *command_line)
{
   char *tmp_file = "jedshell.tmp";
   char buf[JED_MAX_PATH_LEN];
   FILE *f;
   int ret;
   int nCmdShow = SW_SHOWMINIMIZED;
   int wait = TRUE;
   
   sprintf(buf, "%s\\bin\\mswshell.pif %s", Jed_Root_Dir, command_line);
   
   sys_delete_file(tmp_file);
   
   if (msw_system(buf, &nCmdShow, &wait) != 0) return 1;
   
   if (NULL == (f = fopen(tmp_file, "r"))) return 1;
   
   fgets(buf, sizeof(buf), f);
   ret = (int) atol(buf);
   *buf = 0;
   fgets(buf, sizeof(buf), f);
   fclose(f);
   sys_delete_file(tmp_file);
   
   if (*buf != 0)
     msg_error(buf);
   
   return ret;
}

/* to make slang.lib happy
 * There is no definition of function system for MS Windows
 */

int system(const char *command)
{
   return sys_System((char *) command);
}

static void cover_exposed_area (int x, int y, int width, int height)
{
   unsigned short *s, *smax;
   int row, save_row, save_col, max_col, max_row, col;
   
   Performing_Update++;
   hide_cursor ();
   save_row = This_Window.cursor_row;
   save_col = This_Window.cursor_col;
   col = x / This_Window.font_width;
   row = y / This_Window.font_height;
   
   max_col = 2 + col + width / This_Window.font_width;
   max_row = 2 + row + height / This_Window.font_height;
   if (max_col > JX_Screen_Cols) max_col = JX_Screen_Cols;
   if (max_row > JX_Screen_Rows) max_row = JX_Screen_Rows;
   
   for (This_Window.cursor_row = row; This_Window.cursor_row < max_row; This_Window.cursor_row++)
     {
	This_Window.cursor_col = col;
	s = JScreen[This_Window.cursor_row].old + This_Window.cursor_col;
	smax = JScreen[This_Window.cursor_row].old + max_col;
	if (s) send_attr_str(s, smax);
     }
   This_Window.cursor_row = save_row;
   This_Window.cursor_col = save_col;
   Performing_Update--;
   
   show_cursor ();
}


static void push_mouse_event(int button, int x, int y, int state, int type)
{
   unsigned int s = 0;
   int col, row;
   static int last_button;
   static JMouse_Type jm;
#if JED_HAS_MULTICLICK
   static DWORD last_press_time;
   static unsigned int clicks = 0;
   static unsigned long MultiClick_Time = -1;
#endif
   int id;
   
   col = 1 + x / This_Window.font_width;
   row = 1 + y / This_Window.font_height;
   
   if ((type == JMOUSE_DRAG)
       && (col == jm.x) && (row == jm.y))
     return;
   
   if (button == 0) 
     button = last_button;
   else
     last_button = button;

#if JED_HAS_MULTICLICK
   if (type == JMOUSE_DOWN)
     {
	DWORD the_time = GetTickCount();
	if (MultiClick_Time == -1)
	   MultiClick_Time = GetDoubleClickTime();
	if ((last_button == button)
	    && (the_time - last_press_time < MultiClick_Time))
	  {
	     clicks++;
	     if (clicks == 2) 
	       type = JMOUSE_DOUBLE_CLICK;
	     else
	       type = JMOUSE_TRIPLE_CLICK;
	  }
	else
	  {
	     clicks = 1;
	     last_button = button;
	  }
	last_press_time = the_time;
     }
   else if ((clicks > 1) && (last_button == button))
     {
	/* Last was a multi-click.  Ignore this event. */
	type = JMOUSE_IGNORE_EVENT;
     }
#endif

   
   switch (button)
     {
      case WM_LBUTTONUP:
      case WM_LBUTTONDOWN:
	if (!(GetKeyState(VK_MENU) & 0x8000))
	  {
	     jm.button = JMOUSE_BUTTON_1;
	     break;
	  }
	/* drop */
      case WM_MBUTTONUP:
      case WM_MBUTTONDOWN:
	jm.button = JMOUSE_BUTTON_2;
	break;
	
      case WM_RBUTTONUP:
      case WM_RBUTTONDOWN:
	jm.button = JMOUSE_BUTTON_3;
	break;
	
      default:
	return;
     }
   
   if ((type == JMOUSE_UP)
       && (0 == (jm.state & jm.button)))
     {
	/* Make sure this button was down. */
	return;
     }
   
   if (state & MK_LBUTTON) s |= JMOUSE_BUTTON_1;
   if (state & MK_MBUTTON) s |= JMOUSE_BUTTON_2;
   if (state & MK_RBUTTON) s |= JMOUSE_BUTTON_3;
   if (state & MK_CONTROL) s |= JMOUSE_CTRL;
   if (state & MK_SHIFT) s |= JMOUSE_SHIFT;
   
   jm.state = s;
   jm.x = col;
   jm.y = row;
   jm.type = type;
   
   if (-1 == (id = jed_mouse_add_event (&jm)))
     {
	msg_error ("Failed!");
	return;
     }
   

   _putkey(id);
   _putkey(0);
   _putkey('\033');
}

static void x_warp_pointer (void)
{
}

static void x_region_2_cutbuffer (void)
{
   int nbytes;
   char *dat;
   int i, x;
   HGLOBAL hBuf;
   char *buf;
   
   
   
   dat = make_buffer_substring(&nbytes);
   if (dat == NULL) return;
   
   OpenClipboard(This_Window.w);
   EmptyClipboard();
   
   for(i = x = 0; i < nbytes; i++)
     if (dat[i] == '\n') x++;
   
   hBuf = GlobalAlloc(GHND, x + nbytes);
   buf = (char *) GlobalLock(hBuf);
   
   for(i = x = 0; i < nbytes; i++)
     {
	if (dat[i] == '\n') buf[x++] = '\r';
	buf[x++] = dat[i];
     }
   
   GlobalUnlock(hBuf);
   
   SetClipboardData(CF_TEXT, hBuf);
   CloseClipboard();
}

static int x_insert_cutbuffer (void)
{
   int nbytes = 0;
   char *dat;
   int i, x;
   HGLOBAL hBuf;
   char *buf;
   
   CHECK_READ_ONLY
     
     OpenClipboard(This_Window.w);
   hBuf = GetClipboardData(CF_TEXT);
   CloseClipboard();
   
   if (hBuf)
     {
	buf = (char *)GlobalLock(hBuf);
	for(i = x = 0; buf[i] != 0; i++)
	  if (buf[i] != '\r') x++;
	
	nbytes = x;
	dat = SLMALLOC(x + 1);
	for(i = x = 0; buf[i] != 0; i++)
	  if (buf[i] != '\r')
	  dat[x++] = buf[i];
	
	dat[x] = 0;
	
	ins_chars((unsigned char *) dat, nbytes);
	GlobalUnlock(hBuf);
     }
   
   return nbytes;
}

static void msw_define_color(char *color_name, int *pr, int *pg, int *pb)
{
   char buf[30];
   
   sprintf(buf, "%d,%d,%d", *pr, *pg, *pb);
   WriteProfileString(szJedSection, color_name, buf);
}

static void set_window_name (char *s)
{
   if (Batch) return;
   strcpy(This_Window.title, s);
   SetWindowText(This_Window.w, s);
}

static void _USERENTRY msw_cleanup(void)
{
   int i;
   char buf[10];
   RECT rc;
   
   GetWindowRect(This_Window.w, &rc);
   itoa(rc.left, buf, 10);
   WriteProfileString(szJedSection, "X", buf);
   itoa(rc.top, buf, 10);
   WriteProfileString(szJedSection, "Y", buf);
   itoa(rc.right - rc.left, buf, 10);
   WriteProfileString(szJedSection, "Width", buf);
   itoa(rc.bottom - rc.top, buf, 10);
   WriteProfileString(szJedSection, "Height", buf);
   KillTimer(This_Window.w, 42);
#ifdef VC32
   KillTimer (This_Window.w, 43);
#endif

   if (This_Window.w) DestroyWindow(This_Window.w);
   DeleteObject(This_Window.font);
   for(i = 0; i < JMAX_COLORS; i++)
     if (colors[i].hbrBG) DeleteObject(colors[i].hbrBG);   
}


static int Ignore_Wchar_Message;

static char f_keys[4][12] = 
{
     { 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 133, 134 },
     { 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 135, 136 },
     { 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 137, 138 },
     { 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 139, 140 }
};

static char small_keypad_keys[4][13] = 
{
     { 'G', 'H', 'I', 0, 'K', 0, 'M', 0, 'O', 'P', 'Q', 'R', 'S' },   /* normal */
     { '0', '1', '2', 0, '3', 0, '4', 0, '5', '6', '7', '8', '9' },   /* shift */
     { 'w', 141, 132, 0, 's', 0, 't', 0, 'u', 145, 'v', 146, 147 },   /* ctrl */
     { 151, 152, 153, 0, 155, 0, 157, 0, 159, 160, 161, 162, 163 }   /* alt */
};

static char num_keypad_keys[4][13] =
{
     { 'w', 'x', 'y', 0, 't', 'u', 'v', 0, 'q', 'r', 's', 'p', 'n' },
     { '0', '1', '2', 0, '3',  0 , '4', 0, '5', '6', '7', '8', '9' },
     { 'w', 141, 132, 0, 's', 143, 't', 0, 'u', 145, 'v', 146, 147 },
     { 'w', 'x', 'y', 0, 't', 'u', 'v', 0, 'q', 'r', 's', 'p', 'n' }
};

LRESULT CALLBACK process_key_down (HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
   unsigned int key_state = 0;
   unsigned int scan;
   char prefix, c1;
   int i, state;

   if (GetKeyState(VK_CONTROL) & 0x8000) key_state |= KEY_CONTROL;
   if (GetKeyState(VK_SHIFT) & 0x8000) key_state |= KEY_SHIFT;
   if (GetKeyState(VK_MENU) & 0x8000) key_state |= KEY_ALT;
   
   Ignore_Wchar_Message = 0;
   
   scan = (unsigned int) ((lParam >> 16) & 0x1FF);

   switch (scan)
     {
      default: return DefWindowProc(hWnd, msg, wParam, lParam);

      case 0x00E:		       /* backspace */
	_putkey (127);
	Ignore_Wchar_Message = 1;
	return 0;
	
      case 0x039: 		       /* space */
	if (key_state & KEY_CONTROL)
	  {
	     Ignore_Wchar_Message = 1;
	     _putkey (3); _putkey (0);
	  }
	return 0;
	
      case 0x003:		       /* 2 key */
	if (((key_state & KEY_ALT) == 0)
	    && (key_state & KEY_CONTROL))
	  {
	     Ignore_Wchar_Message = 1;
	     _putkey (0);
	     return 0;
	  }
	return DefWindowProc(hWnd, msg, wParam, lParam);
	
      case 0x00C:		       /* -/_ key */
	if (((key_state & KEY_ALT) == 0)
	    && (key_state & KEY_CONTROL))
	  {
	     Ignore_Wchar_Message = 1;
	     _putkey (31);	       /* ^_ */
	     return 0;
	  }
	return DefWindowProc(hWnd, msg, wParam, lParam);
	
      case 0xE02F: 
	c1 = 'Q'; break;	       /* KEYPAD SLASH */

      case 0x037:		       /* KEYPAD STAR */
	c1 = 'R';
	break;

      case 0x04A:		       /* KEYPAD MINUS */
	c1 = 'S';
	break;

      case 0x04E:		       /* KEYPAD PLUS */
	c1 = 'm';
	break;

      case 0x047:            /* KEYPAD HOME */
      case 0x048:            /* KEYPAD UP */
      case 0x049:            /* KEYPAD PGUP */
      case 0x04B:            /* KEYPAD LEFT */
      case 0x04C:            /* KEYPAD 5 */
      case 0x04D:            /* KEYPAD RIGHT */
      case 0x04F:            /* KEYPAD END */
      case 0x050:            /* KEYPAD DOWN */
      case 0x051:            /* KEYPAD PGDN */
      case 0x053:            /* KEYPAD DEL */
      case 0x052:            /* KEYPAD INSERT */
	if (GetKeyState(VK_NUMLOCK) & 0x0001)
	  return DefWindowProc(hWnd, msg, wParam, lParam);
 	 
 	state = 0;
 	if (key_state & KEY_SHIFT) state = 1;
 	if (key_state & KEY_CONTROL) state = 2;
 	if (key_state & KEY_ALT) state = 3;
 	
 	if (key_state & (KEY_CONTROL | KEY_ALT))
 	  {
	     Ignore_Wchar_Message = 1;
 	     _putkey (num_keypad_keys[state][scan - 0x47]);
 	     _putkey (0);
 	     return 0;
 	  }
 	else
 	  c1 = num_keypad_keys[state][scan - 0x47];
 	break;

	
      case 0x11C:            /* KEYPAD ENTER */
	if (key_state & KEY_ALT)
	  {
 	     _putkey(166);
 	     _putkey(0);
	     Ignore_Wchar_Message = 1;
	     return 0;
 	  }
 	else
 	  {
 	     c1 = 'M';
 	     break;
 	  }

      case 0x147: 		       /* home */
      case 0x148:		       /* UP */
      case 0x149:		       /* PGUP */
      case 0x14B:		       /* LEFT */
      case 0x14D:		       /* RIGHT */
      case 0x14F:		       /* END */
      case 0x150:		       /* DOWN */
      case 0x151:		       /* PGDN */
      case 0x153:		       /* DEL */
      case 0x152:		       /* INSERT */
	prefix = 0xE0;
	state = 0;
	if (key_state & KEY_SHIFT) state = 1;
	if (key_state & KEY_CONTROL) state = 2;
	if (key_state & KEY_ALT)
	  {
	     prefix = 0;
	     state = 3;
	  }
	
	_putkey (small_keypad_keys[state][scan - 0x147]);
	_putkey (prefix);
	Ignore_Wchar_Message = 1;
	return 0;
	
      case 0x3b:		       /* F1 */
      case 0x3c:
      case 0x3d:
      case 0x3e:
      case 0x3f:
      case 0x40:
      case 0x41:
      case 0x42:
      case 0x43:
      case 0x44:
      case 0x57:
      case 0x58:		       /* F12 */
	i = scan - 0x3b;
	if (i > 9) i -= 0x12;
	
	state = 0;
	if (key_state & KEY_SHIFT) state = 1;
	if (key_state & KEY_CONTROL) state = 2;
	if (key_state & KEY_ALT) 
	  if (i == 3)		       /* Alt-F4 */
	  return DefWindowProc(hWnd, msg, wParam, lParam);   
	else
	  state = 3;
	
	_putkey(f_keys[state][i]);
	_putkey(0);
	Ignore_Wchar_Message = 1;
	return 0;
     }
   
   _putkey (c1);
   _putkey ('O');
   _putkey (27);
   Ignore_Wchar_Message = 1;
   return 0;
}



LRESULT CALLBACK JEDWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
   static int mouse_button_down;
   PAINTSTRUCT ps;
   RECT rc; 
   
   
   switch (msg)
     {
      case WM_CREATE:
	This_Window.w = hWnd;
	mouse_button_down = 0;
	return 0;
	
      case WM_SIZE:
	if (wParam != SIZE_MINIMIZED)
	  {
	     This_Window.width = LOWORD(lParam);
	     This_Window.height = HIWORD(lParam);
	     reset_display();
	     init_display(1);
	  }
	break;
	
      case WM_PAINT:
	BeginPaint(This_Window.w, &ps);
#if HAS_PALETTE_CODE
	if (NULL != The_Hpalette)
	  SelectPalette (ps.hdc, The_Hpalette, FALSE);
#endif
	This_Window.hdc = ps.hdc;
	This_Window.ndc = 1;
	cover_exposed_area(ps.rcPaint.left, ps.rcPaint.top, 
			   ps.rcPaint.right - ps.rcPaint.left, 
			   ps.rcPaint.bottom - ps.rcPaint.top);
	EndPaint(This_Window.w, &ps);
	This_Window.hdc = NULL;
	This_Window.ndc = 0;
	break;
	
      case WM_ERASEBKGND:
	GetClientRect(This_Window.w, &rc);
	FillRect((HDC)wParam, &rc, colors[JNORMAL_COLOR].hbrBG);
	break;
	
       case WM_MENUCHAR:
	_putkey(wParam);
	_putkey('\033');
	return MAKELONG(0, 1);
	
      case WM_CHAR:
	if (Ignore_Wchar_Message == 0)
	  {
	     if (wParam == 0xE0) _putkey(wParam);
	     _putkey(wParam);
	     break;
	  }
	return DefWindowProc(hWnd, msg, wParam, lParam);

	
      case WM_SYSKEYDOWN:
      case WM_KEYDOWN:
	return process_key_down (hWnd, msg, wParam, lParam);
	
      case WM_KEYUP:
	Ignore_Wchar_Message = 0;
	break;
	
      case WM_SYSKEYUP:
	Ignore_Wchar_Message = 0;
	return DefWindowProc(hWnd, msg, wParam, lParam);
	
      case WM_LBUTTONUP:
      case WM_MBUTTONUP:
      case WM_RBUTTONUP:
	     
	if (!mouse_button_down) break;
	     
	push_mouse_event(msg, LOWORD(lParam), HIWORD(lParam), wParam, JMOUSE_UP);
	mouse_button_down--;
	break;
     
	
      case WM_LBUTTONDOWN:
      case WM_MBUTTONDOWN:
      case WM_RBUTTONDOWN:
	mouse_button_down++;
	push_mouse_event(msg, LOWORD(lParam), HIWORD(lParam), wParam, JMOUSE_DOWN);
	break;
	
      case WM_MOUSEMOVE:
	if (wParam & (MK_LBUTTON | MK_MBUTTON | MK_RBUTTON))
	  push_mouse_event(0, LOWORD(lParam), HIWORD(lParam), wParam, JMOUSE_DRAG);
	break;
	
      case WM_SETFOCUS:
      case WM_KILLFOCUS:
	hide_cursor();
	This_Window.focus = (msg == WM_SETFOCUS);
	show_cursor();
	break;

#if 0
	/* This does not quite work when coming back from a shell */
      case WM_ACTIVATE:
	if ((wParam != WA_INACTIVE) && (CBuf != NULL)) check_buffers();
	break;
#endif
      case WM_CLOSE:
	exit_jed();
	break;
	
#if !defined(__WIN32__) || !JED_HAS_SUBPROCESSES
      case WM_TIMER:
	if ((wParam == 42) && Display_Time)
	  {
	     JWindow->trashed = 1;
	     update((Line *) NULL, 0, 1);
	  }
	return 0;
#endif
	
#if defined(VC32) && defined(HAS_SUBPROCESSES)
      case WM_TIMER:
	/*
	 * just stop the GetMessage call...
	 */
        return 0;
#endif

      case WM_COMMAND:
	if (wParam < MAX_MENU_ID)
	  {
	     int force = 1;
	     if (Menu_Callbacks[wParam])
	       {
		  if (is_internal(Menu_Callbacks[wParam]))
		    {
		       char buf[50];
		       sprintf(buf, ". \"%s\" call", Menu_Callbacks[wParam]);
		       SLang_load_string (buf);
		    }
		  else
		    if (SLang_execute_function(Menu_Callbacks[wParam]) == 0) msg_error(Menu_Callbacks[wParam]);
	       }
	     
	     update_cmd(&force);
	  }
	return 0;
	
      case WM_INITMENUPOPUP:
	SLang_push_integer(wParam);
	if (InitPopup_Callback != NULL)
	  if (SLang_execute_function(InitPopup_Callback) == 0) 
	    msg_error(InitPopup_Callback);
	return 0;

      default:
	return DefWindowProc(hWnd, msg, wParam, lParam);
     }
   
   return 0;
}


/* FUNCTIONS STOLEN FROM DOS_OS2.C */
void sys_flush (int dummy)
{
   (void) dummy;
}

/* Delete the file NAME.  returns 0 on failure, 1 on sucess
 * Under OS/2 and DOS, unlink()[UNIX] and remove()[ANSI] are equivalent.
 */

int sys_delete_file(char *name)
{
   return(1 + remove(name));
}

/* Rename the file or directory OLDNAME to NEWNAME.  Moving a file
 * to a different directory on the same drive is possible.
 * returns 0 on success and -1 on error
 */

int sys_rename(char *oldname, char *newname)
{
   return(rename(oldname,newname));
}

#ifdef VC32
static WIN32_FIND_DATA findata;
static HANDLE hsearch;

#else
static struct ffblk The_ffblk;
static int File_Attr;
  
# define HIDDEN FA_HIDEN
# define SYSTEM FA_SYSTEM
# define SUBDIR FA_DIREC
# define READON FA_RDONLY

#endif

static char Found_Dir[JED_MAX_PATH_LEN];
  
#define lcase(x) if (((x) >= 'A') && ((x) <= 'Z')) (x) |= 0x20
  
static void fixup_name(char *file)
{
   int dir;
   char *p, name[JED_MAX_PATH_LEN];
     
   strcpy (file, Found_Dir);
   
#ifdef VC32
   strcpy (name, findata.cFileName);
   dir = (findata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
#else
   strcpy (name, The_ffblk.ff_name);
   dir = (The_ffblk.ff_attrib & SUBDIR);
#endif
   
   p = name;
   while (*p)
     {
	lcase(*p);
	p++;
     }
   strcat(file, name);
   if (dir) strcat(file, "\\");
}

int sys_findfirst(char *thefile)
{
   char *f, the_path[JED_MAX_PATH_LEN], *file, *f1;
   char *pat;
   
#ifndef VC32
   File_Attr = READON | SUBDIR;
#endif
   
   file = expand_filename(thefile);
   f1 = f = extract_file(file);
   strcpy (Found_Dir, file);
   
   
   Found_Dir[(int) (f - file)] = 0;
   
   strcpy(the_path, file);
   
   while (*f1 && (*f1 != '*')) f1++;
   if (! *f1)
     {
	while (*f && (*f != '.')) f++;
 	if (*f) strcat(the_path, "*"); 
	else
#ifdef __WIN32__
 	  strcat(the_path, "*");
#else
 	strcat(the_path, "*.*");
#endif
     }
   pat = the_path;
   
   if (
#ifdef VC32
       (INVALID_HANDLE_VALUE != (hsearch = FindFirstFile (pat, &findata)))
#else
       !findfirst (pat, &The_ffblk, File_Attr)
#endif
       )
     {
 	fixup_name(file);
 	strcpy(thefile, file);
 	return(1);
     }
   else
     return 0;
}


int sys_findnext(char *file)
{
   if (
#ifdef VC32
       FindNextFile (hsearch, &findata)
#else
       !findnext (&The_ffblk)
#endif 
       )
     {
 	fixup_name(file);
 	return 1;
     }
   
#ifdef VC32
   FindClose (hsearch);
#endif

   return 0;
}

/* Here we do a find first followed by calling routine to conver time.
 * 
 * WARNING: Does this routine work??
 */
unsigned long sys_file_mod_time(char *file)
{
   /* struct tm *local; */
   time_t secs;
#ifdef VC32
   HANDLE hsearch;
   WIN32_FIND_DATA findata;
#else
   unsigned int dat, tim;
   File_Attr = READON | SUBDIR;
#endif
   

#ifdef VC32
   if ((hsearch = FindFirstFile(file, &findata)) != INVALID_HANDLE_VALUE)
     {
	struct tm t;
	SYSTEMTIME systime;
	FindClose(hsearch);
	if (!FileTimeToSystemTime(&findata.ftLastWriteTime, &systime))
	  return 0;
	t.tm_min = systime.wMinute;
	t.tm_hour = systime.wHour;
	t.tm_sec = systime.wSecond;
	t.tm_mday = systime.wDay;
	t.tm_mon = systime.wMonth - 1;
	t.tm_year = systime.wYear - 1900;
 	secs = mktime(&t);
 	return((unsigned long) secs);
     }
#else
   if (!findfirst (file, &The_ffblk, File_Attr))
     {
# if 1
	struct time t;
	struct date d;

 	tim = The_ffblk.ff_ftime;
 	dat = The_ffblk.ff_fdate;

	t.ti_min = (tim >> 5) & 63;
	t.ti_hour = (tim >> 11) & 31;
	t.ti_hund = 0;
	t.ti_sec = 2 * (tim & 31);
	d.da_day = dat & 31;
	d.da_mon = (dat >> 5) & 15;
	d.da_year = 1980 + ((dat >> 9) & 0x7F);
	secs = dostounix(&d, &t);
	return((unsigned long) secs);
# else
	struct tm t;

 	tim = The_ffblk.ff_ftime;
 	dat = The_ffblk.ff_fdate;
	
 	t.tm_min = (tim >> 5) & 63;
 	t.tm_hour = (tim >> 11) & 31;
 	t.tm_sec = 2 * (tim & 31);
 	t.tm_mday = dat & 31;
 	t.tm_mon = (dat >> 5) & 15 - 1;
 	t.tm_year = 80 + ((dat >> 9) & 0x7F);
 	secs = mktime(&t);
 	return((unsigned long) secs);
# endif
     }
#endif				       /* VC32 */
   
   return 0;
}

#ifdef VC32
/* returns 0 if file does not exist, 1 if it is not a dir, 2 if it is */
int sys_chmod(char *file, int what, int *mode, short *uid, short *gid)
{
   struct _stat buf;
   char ourname[256];
   int len;

   if (what)
     {
	_chmod(file, *mode);
	return(0);
     }

   /* strip the trailing backslash if necessary */
   strcpy(ourname, file);
   len = strlen(ourname);
   if (len>1 && ourname[len-1] == SLASH_CHAR && ourname[len-2] != ':')
     ourname[strlen(ourname)-1] = '\0';

   if (_stat(ourname, &buf) < 0)
     return 0;
   
   *mode = buf.st_mode & 0777;
   
   if (buf.st_mode & S_IFDIR) return (2);
   return(1);
}
#else
int sys_chmod(char *file, int what, int *mode, short *dum1, short *dum2)
{
   int flag, m = *mode;
   (void) dum1; (void) dum2;
   
   file = msdos_pinhead_fix_dir (file);
   if ((m = _chmod(file, what, m)) == -1)
     {
 	flag = errno;
 	/* Here if carry flag is set */
 	if (flag == ENOENT) return(0);/* file not found */
 	return -1;
     }
   if (what == 0)
     {
	*mode = m;
     }
   
   if (m & 0x10)
     {
	/* msg_error("File is a directory."); */
	return(2);
     }
   
   return(1);
}
#endif				       /* VC32 */

void flush_output (void)
{
   fflush (stdout);
}

void get_menubar()
{
   HMENU hmenu = GetMenu(This_Window.w);
   
   if (!hmenu) 
     {
	hmenu = CreateMenu();
	SetMenu(This_Window.w, hmenu);
	DrawMenuBar(This_Window.w);
     }
   
   SLang_push_integer((int) hmenu);
}

void destroy_menubar()
{
   int i;
   HMENU hmenu = GetMenu(This_Window.w);
   
   if (hmenu) 
     {
	SetMenu(This_Window.w, NULL);
	DestroyMenu(hmenu);
	
     }
   
   for(i = 0; i < MAX_MENU_ID; i++) Menu_Callbacks[i] = NULL;
}


void create_popup_menu()
{
   HMENU hmenu = CreatePopupMenu();
   
   SLang_push_integer((int) hmenu);
}

void destroy_menu()
{
   HMENU hmenu;
   int count, i;
   int id;
   
   if (!SLang_pop_integer((int *)&hmenu))
     {
	count = GetMenuItemCount(hmenu);
	for(i = 0; i < count; i++)
	  {
	     id = GetMenuItemID(hmenu, i);
	     if (id > 0 && Menu_Callbacks[id] != NULL)
	       {
		  SLFREE(Menu_Callbacks[id]);
		  Menu_Callbacks[id] = NULL;
	       }
	  }
	
	if (!DestroyMenu(hmenu)) msg_error("Cannot destroy menu");
     }
}

void append_menu_item()
{
   HMENU hmenu;
   char *name = NULL;
   char *callback = NULL;
   int   id;
   
   if (!SLpop_string(&callback) &&
       !SLang_pop_integer(&id) &&
       !SLpop_string(&name) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if ((id < 0) || (id >= MAX_MENU_ID))
	  msg_error("Id is out of range.");
	else
	  {
	     if (!AppendMenu(hmenu, MF_STRING, id, name))
	       msg_error("Cannot append menu item");
	     else
	       {
		  if (Menu_Callbacks[id] != NULL)
		    {
		       char buf[50];
		       sprintf(buf, "Id %d is already used.", id);
		       msg_error(buf);
		    }
		  else
		    {
		       Menu_Callbacks[id] = callback;
		       callback = NULL;
		    }
	       }
	  }
     }
	
   if (name != NULL) SLFREE(name);
   if (callback != NULL) SLFREE(callback);
}

void append_popup_menu()
{
   HMENU hmenu;
   char *name = NULL;
   HMENU popup;
   
   if (!SLang_pop_integer((int *)&popup) &&
       !SLpop_string (&name) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (!AppendMenu(hmenu, MF_STRING | MF_POPUP, (UINT)popup, name))
	  msg_error("Cannot append popup menu");
	
     }
   if (name != NULL) SLFREE(name);
}

void append_separator()
{
   HMENU hmenu;
   
   if (!SLang_pop_integer((int *)&hmenu))
     {
	if (!AppendMenu(hmenu, MF_STRING | MF_SEPARATOR, 0, 0))
	  msg_error("Cannot append separator");
     }
}

void insert_menu_item()
{
   HMENU hmenu;
   int   id;
   char *name = NULL;
   int   idNew;
   char *callback = NULL;
   
   if (!SLpop_string(&callback) &&
       !SLang_pop_integer(&idNew) &&
       !SLpop_string(&name) &&
       !SLang_pop_integer(&id) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if ((idNew < 0) || (idNew >= MAX_MENU_ID))
	  msg_error("Id is out of range");
	else
	  {
	     if (!InsertMenu(hmenu, id, MF_STRING | MF_BYCOMMAND, idNew, name))
	       msg_error("Cannot insert menu item");
	     else
	       {
		  if (Menu_Callbacks[idNew] != NULL)
		    {
		       char buf[50];
		       sprintf(buf, "Id %d is already used.", idNew);
		       msg_error(buf);
		    }
		  else
		    {
		       Menu_Callbacks[idNew] = callback;
		       callback = NULL;
		    }
	       }
	  }
     }
   if (name != NULL) SLFREE(name);
   if (callback != NULL) SLFREE(callback);
}

void insert_popup_menu()
{
   HMENU hmenu;
   int   id;
   char *name = NULL;
   HMENU popup;
   
   if (!SLang_pop_integer((int *)&popup) &&
       !SLpop_string (&name) &&
       !SLang_pop_integer(&id) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (!InsertMenu(hmenu, id, MF_STRING | MF_POPUP | MF_BYCOMMAND, (UINT)popup, name))
	  msg_error("Cannot insert popup menu");
	
     }
   if (name != NULL) SLFREE (name);
}

void insert_separator()
{
   HMENU hmenu;
   int   id;
   
   if (!SLang_pop_integer(&id) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (!InsertMenu(hmenu, id, MF_STRING | MF_SEPARATOR | MF_BYCOMMAND, 0, 0))
	  msg_error("Cannot insert separator");
     }
}

void insert_menu_item_pos()
{
   HMENU hmenu;
   int   pos;
   char *name = NULL;
   int   idNew;
   char *callback = NULL;
   
   if (!SLpop_string(&callback) &&
       !SLang_pop_integer(&idNew) &&
       !SLpop_string(&name) &&
       !SLang_pop_integer(&pos) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if ((idNew < 0) || (idNew >= MAX_MENU_ID))
	  msg_error("Id is out of range.");
	else
	  {
	     if (!InsertMenu(hmenu, pos, MF_STRING | MF_BYPOSITION, idNew, name))
	       msg_error("Cannot insert menu item");
	     else
	       {
		  if (Menu_Callbacks[idNew] != NULL)
		    {
		       char buf[50];
		       sprintf(buf, "Id %d is already used.", idNew);
		       msg_error(buf);
		    }
		  else
		    {
		       Menu_Callbacks[idNew] = callback;
		       callback = NULL;
		    }
	       }
	  }
     }
   if (name != NULL) SLFREE(name);
   if (callback != NULL) SLFREE(callback);
}

void insert_popup_menu_pos()
{
   HMENU hmenu;
   int   pos;
   char *name = NULL;
   HMENU popup;
   
   if (!SLang_pop_integer((int *)&popup) &&
       !SLpop_string(&name) &&
       !SLang_pop_integer(&pos) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (!InsertMenu(hmenu, pos, MF_STRING | MF_POPUP | MF_BYPOSITION, (UINT)popup, name))
	  msg_error("Cannot insert popup menu");
	
     }
   if (name != NULL) SLFREE(name);
}

void insert_separator_pos()
{
   HMENU hmenu;
   int   pos;
   
   if (!SLang_pop_integer(&pos) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (!InsertMenu(hmenu, pos, MF_STRING | MF_SEPARATOR | MF_BYPOSITION, 0, 0))
	  msg_error("Cannot insert separator");
     }
}

void delete_menu_item()
{
   HMENU hmenu;
   int   id;
   
   if (!SLang_pop_integer(&id) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if ((id < 0) || (id >= MAX_MENU_ID))
	  {
	     msg_error("Id is out of range.");
	     return;
	  }
	
	
	if (!DeleteMenu(hmenu, id, MF_BYCOMMAND))
	  msg_error("Cannot delete menu");
	else
	  {
	     if (Menu_Callbacks[id]) 
	       {
		  SLFREE(Menu_Callbacks[id]);
		  Menu_Callbacks[id] = NULL;
	       }
	  }
     }
}

void delete_menu_item_pos()
{
   HMENU hmenu, popup;
   int   pos;
   int   id;
   
   if (!SLang_pop_integer(&pos) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	id = GetMenuItemID(hmenu, pos);
	if ((id > 0) && (Menu_Callbacks[id] != NULL))
	  {
	     SLFREE(Menu_Callbacks[id]);
	     Menu_Callbacks[id] = NULL;
	  }
	else
	  if (id == -1)
	  {
	     popup = GetSubMenu(hmenu, pos);
	     SLang_push_integer((int)popup);
	     destroy_menu();
	  }
	
	if (!DeleteMenu(hmenu, pos, MF_BYPOSITION))
	  msg_error("Cannot delete menu");
     }
}

void get_menu_state()
{
   HMENU hmenu;
   int   id;
   UINT  mstate;
   int   state;
   
   if (!SLang_pop_integer(&id) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (-1 == (int)(mstate = GetMenuState(hmenu, id, MF_BYCOMMAND)))
	  msg_error("Cannot get menu state");
	else
	  {
	     state = 0;
	     if (mstate & MF_ENABLED) state = 1;
	     if (mstate & MF_CHECKED) state |= 2;
	     
	     SLang_push_integer(state);
	  }
     }
}

void get_menu_state_pos()
{
   HMENU hmenu;
   int   pos;
   UINT  mstate;
   int   state;
   
   if (!SLang_pop_integer(&pos) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (-1 == (int)(mstate = GetMenuState(hmenu, pos, MF_BYPOSITION)))
	  msg_error("Cannot get menu state");
	else
	  {
	     state = 0;
	     if (mstate & MF_GRAYED) state = 1;
	     if (mstate & MF_CHECKED) state |= 2;
	     
	     SLang_push_integer(state);
	  }
     }
}

void get_popup_menu()
{
   HMENU hmenu;
   int   pos;
   HMENU popup;
   
   if (!SLang_pop_integer(&pos) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	popup = GetSubMenu(hmenu, pos);
	SLang_push_integer((int) popup);
     }
}

void check_menu_item()
{
   HMENU hmenu;
   int   id;
   int   flag;
   
   if (!SLang_pop_integer(&flag) &&
       !SLang_pop_integer(&id) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (-1 == (int) CheckMenuItem(hmenu, id, MF_BYCOMMAND | (flag)?MF_CHECKED:MF_UNCHECKED))
	  msg_error("Menu item does not exist");
     }
}

void check_menu_item_pos()
{
   HMENU hmenu;
   int   pos;
   int   flag;
   
   if (!SLang_pop_integer(&flag) &&
       !SLang_pop_integer(&pos) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (!CheckMenuItem(hmenu, pos, MF_BYPOSITION | (flag)?MF_CHECKED:MF_UNCHECKED))
	  msg_error("Menu item does not exist");
     }
}

void enable_menu_item()
{
   HMENU hmenu;
   int   id;
   int   flag;
   
   if (!SLang_pop_integer(&flag) &&
       !SLang_pop_integer(&id) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (-1 == EnableMenuItem(hmenu, id, MF_BYCOMMAND | (flag)?MF_ENABLED:MF_GRAYED))
	  msg_error("Menu item does not exist");
     }
}

void enable_menu_item_pos()
{
   HMENU hmenu;
   int   pos;
   int   flag;
   
   if (!SLang_pop_integer(&flag) &&
       !SLang_pop_integer(&pos) &&
       !SLang_pop_integer((int *)&hmenu))
     {
	if (-1 == EnableMenuItem(hmenu, pos, MF_BYPOSITION | (flag)?MF_ENABLED:MF_DISABLED))
	  msg_error("Menu item does not exist");
     }
}

void redraw_menubar()
{
   DrawMenuBar(This_Window.w);
}

void set_init_popup_callback()
{
   char *callback;
   
   if (SLpop_string(&callback))
     return;
   
   if (InitPopup_Callback != NULL) 
     SLFREE(InitPopup_Callback);
   
   InitPopup_Callback = callback;
}
   
void msw_help (void)
{
   char *file = NULL;
   char *keyword = NULL;
   int   partial_keyword;
   UINT  help_type;
   DWORD dwData;
   
   if (!SLang_pop_integer(&partial_keyword) &&
       !SLpop_string (&keyword) &&
       !SLpop_string(&file))
      {
	 help_type = (partial_keyword) ? HELP_PARTIALKEY : HELP_KEY;
	 dwData = (DWORD)(LPSTR) keyword;
	 
	 if (*keyword == '\0')
	    {
	       help_type = HELP_CONTENTS;
	       dwData = 0;
	    }
	 
	 if (!WinHelp(This_Window.w, file, help_type, dwData))
	    {
	       msg_error("Help file not found.");
	    }
	 
      }
   if (keyword != NULL) SLFREE(keyword);
   if (file != NULL) SLFREE(file);
}


int (*X_Argc_Argv_Hook)(int, char **);

/* the links to functions and variables here */
void (*tt_goto_rc)(int, int)  		= msw_goto_rc;
void (*tt_begin_insert)(void)  		= msw_begin_insert;
void (*tt_end_insert)(void) 	 	= msw_end_insert;
void (*tt_del_eol)(void)  		= msw_del_eol;
void (*tt_delete_nlines)(int)  		= msw_delete_nlines;
void (*tt_delete_char)(void)  		= msw_delete_char;
void (*tt_erase_line)(void)  		= msw_erase_line;
void (*tt_tt_normal_video)(void)  	= msw_normal_video;
void (*tt_cls)(void)  			= msw_cls;
void (*tt_beep)(void)  			= msw_beep;
void (*tt_reverse_index)(int) 		= msw_reverse_index;
void (*tt_smart_puts)(unsigned short *, unsigned short *, int, int) = msw_smart_puts;
void (*tt_write_string)(char *)  	= msw_write_string;
void (*tt_putchar)(char)  		= msw_putchar;
void (*tt_init_video)(void)  		= msw_init_video;
void (*tt_reset_video)(void)  		= msw_reset_video;
void (*tt_normal_video)(void)  		= msw_normal_video;
void (*tt_set_scroll_region)(int, int)  = msw_set_scroll_region;
void (*tt_reset_scroll_region)(void)  	= msw_reset_scroll_region;
void (*tt_get_terminfo)(void)  		= msw_get_terminfo;
void (*tt_set_color)(int, char *, char *, char *) = msw_set_color;
void (*tt_set_color_esc)(int, char *);

void (*tt_wide_width)(void)  		= msw_wide_width;
void (*tt_narrow_width)(void)  		= msw_narrow_width;
void (*tt_enable_cursor_keys)(void)  	= msw_enable_cursor_keys;
void (*tt_set_term_vtxxx)(int *)  	= msw_set_term_vtxxx;

int *tt_Ignore_Beep  		= &JX_Ignore_Beep;
int *tt_Use_Ansi_Colors  	= &JX_Use_Ansi_Colors;
int *tt_Term_Cannot_Scroll  	= &JX_Term_Cannot_Scroll;
int *tt_Term_Cannot_Insert  	= &JX_Term_Cannot_Insert;
int *tt_Screen_Rows  		= &JX_Screen_Rows;
int *tt_Screen_Cols  		= &JX_Screen_Cols;

int *tt_Baud_Rate               = &JX_Baud_Rate;

/* Unused but required. */
int (*X_Open_Mouse_Hook)(void);
void (*X_Close_Mouse_Hook)(void);


#ifdef __WIN32__

extern int main(int, char **);

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInst, LPSTR lpszCmdLine, int nCmdShow)
{
   char **argv;
   int  argc;
   int count;
   char *pt;
   int ret;
   char *command_line;
   
   /* add 8 for "wjed32", the separating space, and a terminating '\0' */   
   if (NULL == (command_line = SLMALLOC (strlen(lpszCmdLine) + 8)))
     return 0;
   
#ifdef VC32
   _hInstance = hInstance;
#else
   (void) hInstance;
#endif
   (void) nCmdShow;
   
#if JED_HAS_SUBPROCESSES
   InitializeCriticalSection(&Critical_Section);
#endif
   

   strcpy(command_line, "wjed32 ");
   strcat(command_line, lpszCmdLine);
   _hPrev = hPrevInst;
   
   while ( (*command_line != '\0') && (*command_line == ' '))
     command_line++;		       /* start on 1st non-space */
   
   pt = command_line;
   count = 0;
   while ( *pt != '\0' ) {
      count++;			       /* this is an argument */
      while ((*pt != '\0') && (*pt != ' '))
	pt++;			       /* advance until a space */
      while ( *pt == ' '  )
	pt++;			       /* advance until a non-space */
   }
   
   argv = (char **) SLMALLOC( (count+3) * sizeof(char *) );
   if (argv == NULL )
     return 0;			       /* malloc error */
   
   argc = 0;
   pt = command_line;
   while ((argc < count) && (*pt != '\0')) {
      argv[ argc ] = pt;
      argc++;
      while ( *pt != '\0' && *pt != ' ' )
	pt++;			       /* advance until a space */
      if ( *pt != '\0' )
	*(pt++) = '\0';		       /* parse argument here */
      while ( *pt == ' ')
	pt++;			       /* advance until a non-space */
   }
   argv [ argc ] = (char *) NULL;      /* NULL terminated list */
   
   ret = main(argc, argv);
   
   SLFREE(command_line);
   return ret;
}
#endif
