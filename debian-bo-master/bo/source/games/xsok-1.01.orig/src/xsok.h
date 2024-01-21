/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module xsok.h				     */
/*									     */
/*	This file is included by all sources of xsok.			     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

/* defaults for user configurable directories */
#ifndef XSOKDIR		/* directory where to get the game data files */
#define XSOKDIR		"/usr/games/lib/xsok"
#endif
#ifndef XSOKSAVE	/* directory where to save moves of solved levels */
#define XSOKSAVE	"/var/games/xsok"
#endif

#define MAXSAVEFILELEN	200	/* pathname length including trailing zero */
#define MAXXSOKDIRLEN	 99	/* pathname length of the xsok directory */
#define MAXFILENAMELEN	 30	/* filename length of keyboard and message file */

/* maximum board dimensions, inclusive outer borders (tunable) */
#define MAXCOL          32	/* max x dim. */
#define MAXROW          22	/* max y dim. */
/* other maximum settings (tunable) */
#define MAXWALLS	32	/* max. different floor types */
#define MAXOBJECTS	32	/* max. different object types */
#define MAXINSTANCES	256	/* max. number of objects */

/* global string variables */
extern char username[256];   /* which user to blame for success					*/
extern const char *savedir;  /* in which directory should saved games be stored (XSOKSAVE)	*/
extern const char *xsokdir;  /* from which directory to read the level files (default: XSOKDIR) */
extern const char *langdir;		/* a string (empty?) defining the language */
extern const char *rulepool[16];
extern int highscore[300];
extern int pushcost;
extern int movecost;
extern void (*lastcmd)(void);
extern int numeric_arg;

#ifndef EXIT_FAILURE	/* poor old SUN's */
#define EXIT_FAILURE (-1)
#endif

#ifndef max
#define max(a, b)     ((a) > (b) ? (a) : (b))
#define min(a, b)     ((a) < (b) ? (a) : (b))
#endif

typedef int boolean;            /* just one bit of information           */
struct key_action {
    char *string;
    void (*action)(void);
    struct key_action *next;
};

#define True		1
#define False		0

#if 0
/* obsolete */
#define XSOK1MAGIC1	0xb5
#define XSOK1MAGIC2	0xa0
#define ARR_UP		0x100
#define ARR_LEFT	0x101
#define ARR_DOWN	0x102
#define ARR_RIGHT	0x103

#endif

#define Disable 0
#define Enable	1
#define EnableAndRedraw 2


struct walls {
    int chr;	/* character for representation		*/
    int pic;	/* picture number			*/
    int enter;
    int leave;
    int mask;
    int effect;
};

/* definitions for the effect field */
/* all effects plus 100: square will turn to normal floor if touched. */
#define E_ONCE		100	/* add this to make one-time effects */
#define E_NOTHING	0
#define E_TURN_CCW	1
#define E_TURN_180	2
#define E_TURN_CW	3
#define E_DEST		4	/* no effect, but required by finished() */
#define E_EXIT		5	/* dito */
#define E_ADDPOWER	6
#define E_SUBPOWER	7
#define E_TELEPORT	8

/* addstrength, teleporters */

struct objects {
    int chr;	/* character for representation		*/
    int pic;	/* picture number			*/
    int movedir;
    int pushdir;
    int weight;
    int power;
    int mask;
    int score;
};

struct game {
    int numrows;	/* game size */
    int numcols;
    int x;		/* player pos */
    int y;
    int n_pushes;	/* counter */
    int n_moves;
    int stored_moves;
    int bookmark;
    int finished;
    int level;
    int score;
    const char *type;
    int macroStart, macroEnd;
    int macro_x, macro_y;
};


extern char levelcomment[100];
extern char levelauthor[100];
extern int gamegraphic;
extern int maxlevel;		/* maximum level number for this type of game */
extern int nwalls, nobjects, ninstances;
extern struct objects objects[MAXOBJECTS];
extern struct walls walls[MAXWALLS];	/* wall types */
extern struct game game;
extern struct walls *map[MAXROW][MAXCOL];
extern struct objects *obj[MAXROW][MAXCOL];
extern struct objects instance[MAXINSTANCES];
extern char *movetab;
extern int numalloc;


/* function prototypes */
/* parse.c */
void ParseDefinitionFile(void);	/* read definitions */
void ParseMapFile(void);	/* read a level */
void OrgLevel(void);		/* restart game */

/* tools.c */
void fatal(const char *, ...);
void *malloc_(size_t);
void *calloc_(size_t, size_t);
void *realloc_(void *, size_t);
void free_(void *);
char *strsav(const char *);

/* main.c */
void change_rules(const char *);
int compute_score(void);
int finished(void);

/* move.c */
#if 0
void savegame(const char *);
int loadgame(const char *);
#endif
void graphics_control(int);
void playermove(int);
#if 0
void restart(void);
int redo_move(void);
int undo_move(void);
#endif

/* username.c */
void buildusername(const char *);

/* messages.c */
void read_message_file(const char *);
void add_keybinding(struct key_action **, const char *, const char *);
void read_keyboard_file(const char *);
void key_pressed(char *);

/* loadsave.c */
void cmd_ReadHighscores(void);
void WriteHighscores(void);
void switch_uid(int);
void setlangdir(void);
void load_game(const char *);
void save_game(const char *);
void link_game(const char *, const char *);

/* commands.c */
void cmd_Up(void);
void cmd_Left(void);
void cmd_Down(void);
void cmd_Right(void);
void cmd_Repeat(void);
void cmd_LevelInfo(void);
void cmd_NextUnsolved(void);
void cmd_NextLevel(void);
void cmd_PrevLevel(void);
void rq_LeaveSok(void);
void rq_RestartGame(void);
void rq_PrevLevel(void);
void rq_NextLevel(void);
void rq_NextUnsolved(void);
void cmd_DropBookmark(void);
void jumpto_movenr(int);
void cmd_RestartGame(void);
void cmd_GotoBookmark(void);
void cmd_SaveGame(void);
void cmd_ShowVersion(void);
void cmd_ShowScore(void);
void cmd_ShowBestScore(void);
void cmd_ShowAuthor(void);
void cmd_ReplayGame(void);
void cmd_LoadGame(void);
void cmd_UndoMove(void);
void cmd_RedoMove(void);

extern const char *xsok_messages[];
#define TXT_QUIT_CONFIRM	(xsok_messages[0])
#define TXT_NEW_CONFIRM		(xsok_messages[1])
#define TXT_RESTART_CONFIRM	(xsok_messages[2])
#define TXT_NEXT_CONFIRM	(xsok_messages[3])
#define TXT_PREV_CONFIRM	(xsok_messages[4])
#define TXT_MOVENOTPOSSIBLE	(xsok_messages[5])
#define TXT_BOOKMARK_SET	(xsok_messages[6])
#define TXT_YOU_WIN		(xsok_messages[7])
#define TXT_OK			(xsok_messages[8])
#define TXT_VERSION		(xsok_messages[9])
#define TXT_SCORE		(xsok_messages[10])
#define TXT_NOUNDO		(xsok_messages[11])
#define TXT_UNDO		(xsok_messages[12])
#define TXT_NOREDO		(xsok_messages[13])
#define TXT_REDO		(xsok_messages[14])
#define TXT_WELCOME		(xsok_messages[15])

#define TXT_SAVE_ERR_BASIC	(xsok_messages[16])
#define TXT_LOAD_ERR_BASIC	(xsok_messages[17])
#define TXT_SAVE_ERR_OPEN	(xsok_messages[18])
#define TXT_LOAD_ERR_OPEN	(xsok_messages[19])
#define TXT_SAVE_ERR_HEADER	(xsok_messages[20])
#define TXT_LOAD_ERR_HEADER	(xsok_messages[21])
#define TXT_SAVE_ERR_MOVES	(xsok_messages[22])
#define TXT_LOAD_ERR_MOVES	(xsok_messages[23])
#define TXT_SAVE_OK		(xsok_messages[24])
#define TXT_LOAD_OK		(xsok_messages[25])
#define TXT_LOAD_ERR_BADMAGIC	(xsok_messages[26])

#define TXT_NOAUTHOR		(xsok_messages[27])
#define TXT_NEWHIGH		(xsok_messages[28])
#define TXT_NOLOAD		(xsok_messages[29])
#define TXT_HELP_KEYS		(xsok_messages[30])
#define TXT_HELP_RULES		(xsok_messages[31])

#define TXT_STARTMACRO		(xsok_messages[32])
#define TXT_ENDMACRO		(xsok_messages[33])
#define TXT_MACRO_BADPOS	(xsok_messages[34])
#define TXT_UNSOLVED_CONFIRM	(xsok_messages[35])
#define TXT_BEST		(xsok_messages[36])
#define TXT_UNSOLVED		(xsok_messages[37])
#define TXT_SAVE_ERR_LINK	(xsok_messages[38])


/* Xaw-help.c */
/*
void create_help(Widget);
void popup_help(void);
void popdown_help(Widget, XtPointer, XtPointer);
*/

/* Xaw-main.c */
void show_message(const char *str, ...);
void SetTitle(void);
void cmd_LeaveSok(void);
void cmd_Confirm(void);
void cmd_Cancel(void);
void request_confirm(void (*)(void), const char *);
#ifdef SOUND
int checksound(void);
#endif
int main(int argc, char *argv[]);
/* void Force_Resize(XSize_t, XSize_t); */

/* X-widget.c */
/* void AskWidgetForResize(XSize_t, XSize_t); */

/* X-events.c */
void refresh_screen(void);
/* void button_press(XButtonPressedEvent *);
void key_press(XKeyPressedEvent *); */
void cmd_Resize(void);
/* void resize_event(XSize_t, XSize_t); */

/* X-gfx.c */
void sync_and_wait(void);
void NewLevel(int);
void init_layout(void);
void init_gfx(const char *);
/* void dotPaint(int, int, int, int); */
void doPaint(int, int, int, int);
/* void redraw_table(XExposeEvent *); */


#if SOUND
/* X-sound_SUN.c */
int checksound(void);
void play_sound(const char *);
#else
#define play_sound(x)
#endif

/* dummy.c */
void cmd_debug(void);

/* xfopen.c */
FILE *zreadopen(const char *filename);
void zreadclose(FILE *fp);

/* mousemove.c */
void cmd_MouseUndo(void);
void cmd_MouseMove(void);
void cmd_MousePush(void);

extern int mouse_x, mouse_y;

void cmd_StartMacro(void);
void cmd_EndMacro(void);
void cmd_PlayMacro(void);
