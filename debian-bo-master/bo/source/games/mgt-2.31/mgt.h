/* "mgt" Copyright (c) 1991 Shodan  */

#define VERSION "2.31"

#ifdef DEBUG
#define BUG(s) fprintf(debug,s)
#else
#define BUG(s)
#endif

#define FUNCTION
#define false 0
#define true 1

#define MAX(a,b) ((a)>(b)?a:b)
#define MIN(a,b) ((a)<(b)?a:b)

#define MAX_FILES 200
#define MAXCOMMENTWIDTH 50
#define COMMENTALLOC 4096

typedef int boolean;

typedef enum {
   C_QUIT = 0,
   C_DOWN, C_UP,
   C_WALKDOWN, C_WALKUP,
   C_END, C_BEGINNING,
   C_SEARCHCOMMENT, C_SEARCHBACKCOMMENT,
   C_DOWNFORK, C_UPFORK,
   C_GOTO,
   C_WRITE,
   C_ADDBLACK, C_ADDWHITE,
   C_ADDVAR,
   C_TREECUT,
   C_ADDLETTER, C_ADDMARK,
   C_LOAD, C_PASTE,
   C_EDCOMMENT,
   C_DELNODE, C_ADDNAME, C_SCORE, C_PASSMOVE, C_TOPLAY, C_TOGGLESTONE, C_BACKFILE, C_NEXTFILE,
   C_REDRAW, C_SAVESHORT, C_TUTORSWAP, C_SAVESCREEN, C_INFO, C_MOVE,
   C_DOWNLEFT, C_CURDOWN, C_DOWNRIGHT, C_CURLEFT,
   C_CURRIGHT, C_UPLEFT, C_CURUP, C_UPRIGHT,
   C_LASTCOMMAND, C_UNDO
}  command;


/* C_COMMENTSCROLLDOWN, C_COMMENTSCROLLUP, C_TREESCROLLDOWN, C_TREESCROLLUP, */

#define C_NOTHING ((command) 75)
#define C_CHOSECHILD ((command) 76)
#define C_NEXTCMD ((command) 127)

typedef enum {
   t_White,
   t_Black,
   t_Open,
   t_Close,
   t_NewNode,
   t_Comment,
   t_AddWhite,
   t_AddBlack,
   t_Letter,
   t_Mark,
   t_AddEmpty,
   t_Name,
   t_Pass,
   t_Player,
   t_Size,
   t_Handicap,
   t_PlayerBlack,
   t_BlackRank,
   t_PlayerWhite,
   t_WhiteRank,
   t_GameName,
   t_Event,
   t_Round,
   t_Date,
   t_Place,
   t_TimeLimit,
   t_Result,
   t_GameComment,
   t_Source,
   t_User,
   t_Komi,

   t_WS,
   t_EOF
}
   Token;


#define FIRSTINFO ((int)t_PlayerBlack)
#define LASTINFO  ((int)t_User)


typedef enum {
   P_NOTHING = 0, P_BLACK, P_WHITE, P_DAME, P_BLACKTERR, P_WHITETERR, P_CHECKED
}
   piece;

typedef piece boardType[19][19];

typedef struct {
   boardType b;
}
   board, *pBoard;


typedef int (*pfi) ();

typedef struct {
   char *name;
   char *option;
   char *storage;
   pfi open, close, refreshIO, plotPiece, displayComment;
   pfi clearComment, initializeBoard, clearScreen, idle;
   pfi drawTree, highlightLast, readEnv, notifyMessage;
   pfi notifyClear, queryStr, setCursor, plotMark, plotLetter;
   pfi getPoint, editComment, askYN, notifyError, displayInfo;
   pfi getInfoToChange, editInfo;
}
   interface, *interfaceP;


typedef struct {
   char x, y;
}
   coord;


typedef struct coordstruct {
   char x, y;
   struct coordstruct *next;
}  coordList;


typedef struct propertyRec {
   struct propertyRec *next;
   Token t;
   union {
      coordList *stones;
      char *comment;
      Token player;
   }  data;
}
   property;


typedef struct noderec {
   property *p;
   int nodeNum;
   struct noderec *parent, *child, *nextSibling, *lastSibling;
}
   node, *nodep;


/* board coordinates used to indicate a pass */

#define PASSVAL 25



#include "proto.h"
#include <stdio.h>

#ifdef MGT_IBM
#include <alloc.h>
#endif


extern char *info[LASTINFO - FIRSTINFO + 2];
extern int handicap;

extern int prisoners[];
extern interface *io;
extern int boardsize;
extern FILE *input;
extern int xcur, ycur;
extern Token curPlayer;
extern int mailFlag;
extern int saveShort;
extern int tutor;
extern char *saveName;
extern int retVal;
extern char name_buf[512];
extern filecount, currentfile;
extern char *files[MAX_FILES];

#ifdef DEBUG
extern FILE *debug;
extern unsigned long totalmemory;
#endif
