/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:	27.xii.1995
 *
 *	main header file
 */

#ifndef _xtrojka_h_
#define _xtrojka_h_

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#ifdef XPM
#include <X11/xpm.h>
#endif

#ifndef True
#define True	1
#endif

#ifndef False
#define False	0
#endif

/*
 *	version definition
 */
#define VERSION "Version 1.2.3"

#define kFORCED		1
#define kUNFORCED	0

#define kDEFAULTSPEED	-1

#define COLORS	9

#define NO_COLOR -1

#define BLACK	0
#define MAGENTA	1
#define BLUE	2
#define CYAN	3
#define YELLOW	4
#define GREEN	5
#define RED	6
#define PURPLE	7
#define WHITE	8

/*
 *	types
 */
#ifdef SUNOS4
typedef int flag;
#else
typedef char flag;
#endif


typedef enum _game_state {
	st_playing = 0,
	st_idle
} GAME_STATE;

typedef struct _AppData {
	Pixel color[COLORS];
	String slist_font;
	String game_font;
	String str_best_players;
	String wstr_stat;
	String wstr_open_prefs;
	String wstr_create_prefs;
	String wstr_write_prefs;
	String wstr_read_prefs;
} AppData;

#ifdef XPM
typedef struct _Pic {
	XpmAttributes	attr;
	Pixmap		pic;
	Pixmap		picMask;
} Pic;
#endif

extern AppData app_data;

/*
 *	function prototypes
 */

void quit_appl_action(Widget,XtPointer,XEvent *,Boolean *);
void quit_appl(void);
void init_xtrojka(void);
void init_uif(void);
void init_map_catcher(void);
void i_just_got_mapped_hlr(Widget, XtPointer, XMapEvent*, Boolean*);
void show_no_debug(void);
void show_help(void);
void show_startup_string(void);


#endif /* _xtrojka_h_ */
