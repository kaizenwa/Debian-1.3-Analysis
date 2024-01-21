/* ascii.c test version 9/2/92 wclrtoeol */
/* "mgt" Copyright (c) 1991 Shodan  */

#include <ctype.h>
#include "mgt.h"

#define TOP 2
#define LEFT 4
#define RIGHT 79
#define STATUS_X 45
#define STATUS_Y 0
#define STATUS_WIDE (RIGHT-STATUS_X)
#define COMMENT_X 46
#define COMMENT_Y 2
#define COMMENT_WIDE (RIGHT-COMMENT_X)
#define COMMENT_HIGH (14-COMMENT_Y)
#define TREE_X COMMENT_X
#define TREE_Y 15
#define TREEWIDE (RIGHT-TREE_X)
#define TREEHIGH 6
#define COMMAND_X TREE_X
#define COMMAND_Y (TREE_Y+TREEHIGH)
#define WINLEFT   COMMENT_X
#define WINRIGHT  (RIGHT-1)
#define WINTOP    2
#define WINBOT    13
#define WINHIGH   (WINBOT-WINTOP)
#define WINWIDE   (WINRIGHT-WINLEFT)
#define SCRHIGH   120
#define PGOFFSET  3
#define CTRLC ('C'-64)
#define CTRLL ('L'-64)
#define ESC   27
#define EDHELP "%s - write %s   %s - keep old %s"

#define EDUP 	0
#define EDDOWN 	1
#define EDPUP 	2
#define EDPDOWN	3
#define EDLEFT	4
#define EDRIGHT	5
#define EDBOL	6
#define EDEOL	7
#define EDBOC	8
#define EDEOC	9
#define EDDEL	10
#define EDDEOL	11
#define EDTINS	12
#define EDSAVE	13
#define EDABORT	14
#define MAXEDCMDS 14

#define C_COMMENTSCROLLDOWN C_LASTCOMMAND
#define C_COMMENTSCROLLUP   ((command) ((int)C_LASTCOMMAND+1))
#define C_TREESCROLLDOWN    ((command) ((int)C_LASTCOMMAND+2))
#define C_TREESCROLLUP      ((command) ((int)C_LASTCOMMAND+3))
#define C_VIDEO             ((command) ((int)C_LASTCOMMAND+4))
#define C_LASTOFS 5

enum {
   CHAR_BLACK, CHAR_WHITE,
   CHAR_DAME,
   CHAR_BLACKTERR, CHAR_WHITETERR,
   CHAR_NOTHING,
   CHAR_HANDICAP,
   CHAR_VERT, CHAR_HORIZ,
   CHAR_UPLEFT, CHAR_UPRIGHT,
   CHAR_DOWNLEFT, CHAR_DOWNRIGHT,
   ASC_NUMCHARS
};


static boolean commentExists;
static int commentLine;
static int treeLine;
static int inverseFlag = 0;
static char boardPiece();
static int getLine();
static void drawPiece();
static void printstatus();
static void setCursor();

char keys[] = "q><.,eb}{][gwzxv!lm#^cdnspotrfLWTFI012346789kiju&";
char edcmds[] = "PNpnBFAE<>DKIzW";
char emacscmds[] = "PNpnBFAE<>DKIzW";
char vicmds[] = "kj\002\006hl0$HLxDRwq";

int edvi = 0;

char xAxisChars[] = "ABCDEFGHJKLMNOPQRSTUVWXYZ";
char helpMsg[] = "? for help";

static void fixedkeys()
{
   char *ptr;
   if (!edvi)
      for (ptr = edcmds; *ptr; ptr++)
	 if (*ptr >= 'A' && *ptr <= 'Z')
	    *ptr -= 64;
	 else
	    *ptr |= 128;
}


char *dispedkey(key, keystr)
char key;
char *keystr;
{
   if (key & 128) {
      keystr[0] = ']';
      keystr[1] = key & 127;
      keystr[2] = 0;
   } else if (key < ' ') {
      keystr[0] = '^';
      keystr[1] = key + 64;
      keystr[2] = 0;
   } else {
      keystr[0] = key;
      keystr[1] = 0;
   }
   return keystr;
}


static void fixkeys()
{
   char *keyptr;
   for (keyptr = keys; *keyptr; keyptr++)
      if (*keyptr >= 'A' && *keyptr <= 'Z')
	 *keyptr -= 64;
}


#ifdef MGT_IBM
#include "asc_ibm.inc"
#endif

#if (MGT_UNIX || vms)
#include "asc_unix.inc"
#endif


static void notifyClearAscii()
{
   move(23, 1);
   clrtoeol();
}

static void notifyMessageAscii(s)
char *s;
{
   mvaddstr(23, 1, s);
   refresh();
}

static void notifyErrorAscii(s)
char *s;
{
   mvaddstr(23, 1, s);
   printw("  Hit a key.");
   refresh();
   getKey();
   notifyClearAscii();
}

static void clearScreenAscii()
{
   clear();
}


static void printstatus(mesg)
char *mesg;
{
   int i;
   move(STATUS_Y, STATUS_X);
   for (i = 0; i < STATUS_WIDE && mesg[i]; i++)
      addch(mesg[i]);
}


static int getLine(query, dst, maxLen)	/* char *, char *, int */
char *query, *dst;
int maxLen;
{
   int qLen, dLen;
   char c;
   mvaddstr(23, 1, query);
   clrtoeol();
   dLen = 0;
   qLen = strlen(query);
   do {
      move(23, qLen + 1 + dLen);
      refresh();
      c = getKeyLine();
      if (isprint(c) && dLen < maxLen) {
	 move(23, qLen + dLen + 1);
	 refresh();
	 addch(c);
	 dst[dLen++] = c;
      } else if (dLen && (c == '\b' || c == '\177')) {
	 dLen--;
	 move(23, qLen + dLen + 1);
	 addch(' ');
      }
   }
   while (c != '\n' && c != '\r');
   dst[dLen] = 0;
   move(23, 1);
   clrtoeol();
   return dLen;
}


static int askYNAscii(query, def)
char *query;
int def;
{
   static char *yn[] =
   {" (y/N)? ", " (Y/n)? "};
   char line[80];
   char buf[3];

   strcpy(line, query);
   strcat(line, yn[def]);
   if (getLine(line, buf, 2)) {
      if (*buf == 'Y' || *buf == 'y')
	 return 1;
      if (*buf == 'N' || *buf == 'n')
	 return 0;
   }
   return def;;
}


static void setCursor(x, y)
int x, y;
{
   move(TOP + y, LEFT + 2 * x);
}


static void printNStr(s, n)
char *s;
int n;
{
   int i;
   for (i = 0; i < n && s[i]; i++)
      addch(s[i]);
}


static void drawTreeAscii0(n)
nodep n;
{
   property *p;

   move(TREE_Y, TREE_X + 1);
   printw(" Node #%d: ", n->nodeNum);
   if (p = getprop(n, t_Name)) {
      int num, width;

      width = TREEWIDE - 10;
      num = n->nodeNum;
      do {
	 num /= 10;
	 width--;
      } while (num);
      printNStr(p->data.comment, width);
   }
   clrtoeol();
   {
      int temp;
      if (treeLine < 0)
	 treeLine = 0;
      else {
	 temp = treeCountSiblings(n) - TREEHIGH + 1;
	 temp = MAX(temp, 0);
	 treeLine = treeLine > temp ? temp : treeLine;
      }
   }
   {
      nodep ch;
      int index;
      index = 0;
      ch = nthChild(n, treeLine);
      while (ch && index < TREEHIGH - 1) {
	 move(index + TREE_Y + 1, TREE_X);
	 printw("%c:", 'A' + index + treeLine);
	 clrtoeol();
	 if (p = getprop(ch, t_Name))
	    printNStr(p->data.comment, 80 - (TREE_X + 3));
	 ch = ch->nextSibling;
	 index++;
      }
      while (index < TREEHIGH - 1) {
	 move(index++ + TREE_Y + 1, TREE_X);
	 clrtoeol();
      }
      move(TREE_Y + TREEHIGH - 1, TREE_X - 1);
      addch(ch ? '+' : ' ');
      move(TREE_Y + 1, TREE_X - 1);
      addch(treeLine ? '-' : ' ');
   }
   refresh();
}


/* draw the tree children when node currently is at n */
static void drawTreeAscii(n)
nodep n;
{
   treeLine = 0;
   drawTreeAscii0(n);
}


static void closeAscii()
{
   endwin();
}


static void refreshAscii()
{
   refresh();
}

static char boardPiece(x, y)
int x, y;
{
   return

      (((boardsize == 19 && x == 9) || (boardsize >= 10 && (x == 3 || x == boardsize - 4))
	|| (boardsize > 6 && boardsize < 10 && (x == 2 || x == boardsize - 3)))
       &&
       ((boardsize == 19 && y == 9) || (boardsize >= 10 && (y == 3 || y == boardsize - 4))
	|| (boardsize > 6 && boardsize < 10 && (y == 2 || y == boardsize - 3)))
      )
      ? chars[(int) CHAR_HANDICAP] : chars[(int) CHAR_NOTHING];
}




static void plotPieceAscii(b, i, j)
pBoard b;
int i, j;
{
   piece p;
   char c;
   p = boardGet(b, i, j);
   switch (p) {
      case P_NOTHING:
	 c = boardPiece(i, j);
	 break;
      case P_BLACK:
	 c = chars[(int) CHAR_BLACK];
	 break;
      case P_WHITE:
	 c = chars[(int) CHAR_WHITE];
	 break;
      case P_DAME:
	 c = chars[(int) CHAR_DAME];
	 break;
      case P_BLACKTERR:
	 c = chars[(int) CHAR_BLACKTERR];
	 break;
      case P_WHITETERR:
	 c = chars[(int) CHAR_WHITETERR];
	 break;
   }
   drawPiece(i, j, c);
}


static char *savetype[] =
{"long ", "short"};

static void highlightAscii(x, y, movenum, turn)
int x, y, movenum, turn;
{
   extern int madechanges;
   if (turn != -1){
      move(23, 1);
      printw("%s #%d ", turn ? "White" : "Black", movenum);
      if (x == PASSVAL && y == PASSVAL)
	 printw("Pass      ");
      else
	 printw("at %c%d  ", xAxisChars[x], boardsize - y);
   } else {
      move(23, 1);
      clrtoeol();
   }
   drawPrisoners();
   move(COMMAND_Y, COMMAND_X + 15);
   if (tutor)
      printw("tutor");
   else if (madechanges)
      printw("edit ");
   else
      printw("read ");
   printw("    %s", savetype[saveShort ? 1 : 0]);
   refresh();
}

static void showCommentAscii(i)	/* display from line i onward */
int i;
{
   short line;

   /* line = MIN(commentLines() - i, COMMENT_HIGH); / */
   line = COMMENT_HIGH;

   move(COMMENT_Y, COMMENT_X - 1);
   addch(i ? '-' : ' ');
   move(COMMENT_Y + COMMENT_HIGH - 1, COMMENT_X - 1);
   addch(commentLines() - i > COMMENT_HIGH ? '+' : ' ');

   while (line--) {
      mvaddstr(line + COMMENT_Y, COMMENT_X, commentGet(line + i));
      clrtoeol();
   }
   refresh();
}

static void clearCommentAscii()
{
   int line;

   commentExists = false;
   for (line = COMMENT_Y; line < COMMENT_Y + COMMENT_HIGH; line++)
      move(line, COMMENT_X), clrtoeol();
   move(COMMENT_Y, COMMENT_X - 1);
   addch(' ');
   move(COMMENT_Y + COMMENT_HIGH - 1, COMMENT_X - 1);
   addch(' ');
   refresh();
}

static void displayCommentAscii(s)
char *s;
{
   commentExists = true;
   commentLine = 0;
   formatComment(s, COMMENT_WIDE);
   showCommentAscii(0);
   refresh();
}


static command charToCommand(c)
char c;
{
   int i;
   if ((c == ESC) || (c == CTRLC))
      return C_QUIT;
   for (i = (int) C_LASTCOMMAND + C_LASTOFS; i--;) {
      if (c == keys[i])
	 break;
   }
   return (command) i;		/* -1 = not found */
}


static int getPointAscii()
{
   char c;

   setCursor(xcur, ycur);
   refresh();
   while (1) {
      c = getKeyScore();
      if (c == 'u')
	 return (int) C_UNDO;
      if ((c == ESC) || (c == keys[(int) C_QUIT]))
	 return (int) C_QUIT;
      if (c == 014)
	 return (int) C_REDRAW;
      if ((c == '\r') || (c == '\n'))
	 return (int) C_SCORE;
      if ((c == ' ') || (c == keys[(int) C_MOVE]))
	 return (int) C_MOVE;
      switch (charToCommand(c)) {
	 case C_CURLEFT:
	    xcur = (xcur - 1 + boardsize) % boardsize;
	    setCursor(xcur, ycur);
	    refresh();
	    break;
	 case C_CURRIGHT:
	    xcur = (xcur + 1) % boardsize;
	    setCursor(xcur, ycur);
	    refresh();
	    break;
	 case C_CURUP:
	    ycur = (ycur - 1 + boardsize) % boardsize;
	    setCursor(xcur, ycur);
	    refresh();
	    break;
	 case C_CURDOWN:
	    ycur = (ycur + 1) % boardsize;
	    setCursor(xcur, ycur);
	    refresh();
	    break;
	 case C_UPLEFT:
	    ycur = (ycur - 1 + boardsize) % boardsize;
	    xcur = (xcur - 1 + boardsize) % boardsize;
	    setCursor(xcur, ycur);
	    refresh();
	    break;
	 case C_UPRIGHT:
	    xcur = (xcur + 1) % boardsize;
	    ycur = (ycur - 1 + boardsize) % boardsize;
	    setCursor(xcur, ycur);
	    refresh();
	    break;
	 case C_DOWNLEFT:
	    xcur = (xcur - 1 + boardsize) % boardsize;
	    ycur = (ycur + 1) % boardsize;
	    setCursor(xcur, ycur);
	    refresh();
	    break;
	 case C_DOWNRIGHT:
	    xcur = (xcur + 1) % boardsize;
	    ycur = (ycur + 1) % boardsize;
	    setCursor(xcur, ycur);
	    refresh();
	    break;
      }
   }
}




static char *edhelp[] =
{
   "                Comment Editor Help",
   "",
   " Cursor control ('^' control, ']' <esc>/<meta>):",
   "    %s: Previous line         %s: Next line",
   "    %s: Previous page         %s: Next page",
   "    %s: Back one character    %s: Forward one character",
   "    %s: Beginning of line     %s: End of line",
   "    %s: Beginning of comment  %s: End of comment",
   "    %s: Delete one character  %s: Delete to end of line",
   " Press %s to toggle insert mode",
   " Press %s to exit and write your comment",
   " Press %s to exit and keep the old comment",
   "",
   "  (Warning:  comments longer than 120 lines",
   "             will be truncated if edited)",
   "",
   "                    Other Notes",
   "",
   "  When saving a file, * stands for the current file",
   "       (printed in the upper right corner)",
   "",
   "               Hit any key to return"
};




#define HALFWAY (((int)C_DOWNLEFT)/2)


char *fixkey(key, keystr)
char key;
char *keystr;
{
   if (key == keys[(int) C_QUIT])
      printw("ESC or ");
   if (key == keys[(int) C_MOVE])
      printw("Space or ");
   if (key < ' ') {
      keystr[0] = '^';
      keystr[1] = key + 64;
      keystr[2] = 0;
   } else {
      keystr[0] = key;
      keystr[1] = 0;
   }
   return keystr;
}


static void helpAscii()
{
   int i;
   extern char *helpStr[];
   char keystr[3];
   char kstr[3];
   char ch;

   /* don't put anything here */
   preserveScreen();
   clear();
   move(0, 33);
   printw("MGT Version %s", VERSION);
   move(1, 12);
   printw("Written by Greg Hale        Enhancements by Adrian Mariano");
   move(2, 11);
   printw("(hale@scam.Berkely.edu)      (adrian@bsdserver.ucsf.edu)");
   for (i = 0; i < HALFWAY; i++) {
      move(i + 3, 8);
      printw("%s: %s", fixkey(keys[i], keystr), helpStr[i]);
      move(i + 3, 46);
      printw("%s: %s", fixkey(keys[i + HALFWAY], keystr), helpStr[i + HALFWAY]);
   }
   move(21, 8);
   printw("%s", fixkey(keys[C_TREESCROLLUP], keystr));
   printw(", %s: Scroll tree window", fixkey(keys[C_TREESCROLLDOWN], keystr));
   move(21, 46);
   printw("%s", fixkey(keys[C_COMMENTSCROLLUP], keystr));
   printw(", %s: Scroll comment window", fixkey(keys[C_COMMENTSCROLLDOWN], keystr));
   move(22, 8);
   printw("Uppercase letters visit variations");
   move(22, 46);
   for (i = (int) C_DOWNLEFT; i <= (int) C_UPRIGHT; i++)
      addch(keys[i]);
   printw(": Cursor movement");
   move(23, 21);
   printw("Hit return to return, any key to continue");
   refresh();
   ch = getKey();
   if (ch != '\r' && ch != '\n') {
      clear();
      for (i = 0; i < 3; i++) {
	 move(i, 10);
	 printw(edhelp[i]);
      }
      for (i = 0; i < 12; i += 2) {
	 move((i / 2 + 3), 10);
	 printw(edhelp[(i / 2 + 3)], dispedkey(edcmds[i], kstr), dispedkey(edcmds[i + 1], keystr));
      }
      move(9, 10);
      printw(edhelp[9], dispedkey(edcmds[EDTINS], kstr));
      move(10, 10);
      printw(edhelp[10], dispedkey(edcmds[EDSAVE], kstr));
      move(11, 10);
      printw(edhelp[11], dispedkey(edcmds[EDABORT], kstr));
      for (i = 12; i < sizeof(edhelp) / sizeof(char *); i++)
	 mvaddstr(i, 10, edhelp[i]);
      refresh();
      ch = getKey();
      if (ch != '\r' && ch != '\n')
	 specialHelp();
   }
   restoreScreen();
}



static int idleAscii(curnode)
nodep curnode;
{
   char c;
   command r;
   command com;

   highlightLast();
   setCursor(xcur, ycur);
   refresh();

   c = getKeyIdle();
   if (r = specialKeysIdle(c))
      return (int) r;
   r = C_NOTHING;
   if (c >= 'A' && c <= 'Z') {
      r = (command) ((char) C_CHOSECHILD + (c - 'A'));
   } else
      switch (c) {
	 case ' ':
	    return (int) C_MOVE;
	 case '?':
	    helpAscii();
	    return (int) C_NOTHING;
	 default:
	    com = charToCommand(c);
	    switch (com) {
	       case C_QUIT:
		  {
		     char buf[5];
		     if (c != ESC) {
			getLine("Quit (y/N)?", buf, 1);
			if (buf[0] == 'y')
			   r = C_QUIT;
		     } else
			r = C_QUIT;
		  }
		  break;
	       case C_VIDEO:
		  inverseFlag = !inverseFlag;
		  return (int) C_REDRAW;
	       case C_COMMENTSCROLLDOWN:
		  if (commentExists) {
		     if (commentLine < commentLines() - COMMENT_HIGH)
			commentLine += COMMENT_HIGH - PGOFFSET;
		     showCommentAscii(commentLine);
		  }
		  break;
	       case C_COMMENTSCROLLUP:
		  if (commentExists && commentLine) {
		     commentLine -= COMMENT_HIGH - PGOFFSET;
		     showCommentAscii(commentLine);
		  }
		  break;
	       case C_TREESCROLLDOWN:
		  treeLine++;
		  drawTreeAscii0(curnode);
		  break;
	       case C_TREESCROLLUP:
		  treeLine--;
		  drawTreeAscii0(curnode);
		  break;
	       default:
		  r = (command) com;
	    }
      }
   return (int) r;
}


#define MATCH(string,action) if (!strncmp((*env), (string), strlen(string))){ \
                               (*env) += strlen(string);\
                               action;\
                             } else

static void readEnvAscii(env)
char **env;
{
   MATCH("ASCCOM:", strncpy(keys, *env, (int) C_LASTCOMMAND + C_LASTOFS))
      MATCH("ASCCHAR:", strncpy(chars, *env, (int) ASC_NUMCHARS))
      MATCH("ASCED:", strncpy(edcmds, *env, MAXEDCMDS + 1))
      MATCH("ASCINV", inverseFlag = 1)
      MATCH("ASCEDVI", (edvi = 1, strncpy(edcmds, vicmds, MAXEDCMDS + 1)))
      MATCH("ASCEDEMACS", (edvi = 0, strncpy(edcmds, emacscmds, MAXEDCMDS + 1)))
#ifdef MGT_IBM
      MATCH("ASCARROW:", cursorkey =
	    (0 == strnicmp(*env, "cursor", 6)) +
	    2 * (0 == strnicmp(*env, "mode", 4)))
      MATCH("ASCNOGRAPH", graphics = 0)
      MATCH("ASCMOUSENORM", graphicalCursor = 0)
      MATCH("ASCNOCOLOR", (usecolor = 0, graphics = 0))
      MATCH("ASCBCOL:", blackstone = atoi(*env))
      MATCH("ASCWCOL:", whitestone = atoi(*env))
      MATCH("ASCDAME:", damecolor = atoi(*env))
      MATCH("ASCBOARD:", boardcolor = atoi(*env))
      MATCH("ASCBG:", background = atoi(*env))
      MATCH("ASCFG:", foreground = atoi(*env))
      MATCH("ASCMARK:", markcolor = atoi(*env))
      MATCH("ASCLET:", lettercolor = atoi(*env))
      MATCH("ASCMENUBG:", menubg = atoi(*env))
      MATCH("ASCMENUFG:", menufg = atoi(*env))
      MATCH("RED:", bred = atoi(*env))
      MATCH("GREEN:", bgreen = atoi(*env))
      MATCH("BLUE:", bblue = atoi(*env))
#endif
      ;
}



char screen[SCRHIGH + 1][WINWIDE + 1];
int yoffset, cmplus, cmminus;

#define inwin(Y) ((Y <= (WINHIGH + yoffset)) && (Y >= yoffset))

static void showcommentindicator()
{
   int rfr;
   rfr = 0;
   mainwin();
   if (!((yoffset > 0) == cmminus)) {
      cmminus = !cmminus;
      move(COMMENT_Y, COMMENT_X - 1);
      addch(cmminus ? '-' : ' ');
      rfr = 1;
   }
   if (!((screen[yoffset + WINHIGH + 1][0] > 0) == cmplus)) {
      cmplus = !cmplus;
      move(COMMENT_Y + COMMENT_HIGH - 1, COMMENT_X - 1);
      addch(cmplus ? '+' : ' ');
      rfr = 1;
   }
   if (rfr)
      refresh();
   unmainwin();
}


static void adjustwin(win, yloc)
WINDOW *win;
int yloc;
{
   int x, y;
   if ((yloc < yoffset) || (yloc > (yoffset + WINHIGH))) {
      if (yloc < yoffset)
	 yoffset = yloc - WINHIGH + PGOFFSET;
      if (yloc > (yoffset + WINHIGH))
	 yoffset = yloc - PGOFFSET;
      if (yoffset < 0)
	 yoffset = 0;
      if (yoffset > (SCRHIGH - WINHIGH))
	 yoffset = (SCRHIGH - WINHIGH);
      for (; yoffset > 0 && !screen[yoffset + WINHIGH][0]; yoffset--);
      for (y = yoffset; y <= (yoffset + WINHIGH) && screen[y][0]; y++) {
	 for (x = 0; x <= WINWIDE && screen[y][x]; x++) {
	    wmove(win, y - yoffset, x);
	    waddch(win, screen[y][x] < ' ' ? ' ' : screen[y][x]);
	 }
	 if (x <= WINWIDE) {
	    wmove(win, y - yoffset, x);
	    wclrtoeol(win);
	 }
      }
      if (y <= (yoffset + WINHIGH)) {
	 wmove(win, y - yoffset, 0);
	 wclrtobot(win);
      }
   }
   showcommentindicator();
}


static void fixnewline(win, xloc, yloc)
WINDOW *win;
int xloc, yloc;
{
   if (xloc < WINWIDE && screen[yloc][xloc] == '\n' &&
       screen[yloc][xloc + 1]) {
      int myx, myy;
      for (myy = SCRHIGH - 1; myy > yloc && !screen[myy][0]; myy--);
      for (; myy > yloc; myy--)
	 for (myx = 0; myx <= WINWIDE && (screen[myy + 1][myx] || screen[myy][myx]); myx++) {
	    screen[myy + 1][myx] = screen[myy][myx];
	    if (inwin((myy + 1))) {
	       wmove(win, myy - yoffset + 1, myx);
	       waddch(win, screen[myy][myx] >= ' ' ?
		      screen[myy][myx] : ' ');
	    }
	 }
      for (myx = xloc + 1; myx <= WINWIDE && screen[myy][myx]; myx++) {
	 screen[myy + 1][myx - xloc - 1] = screen[myy][myx];
	 if (inwin(myy)) {
	    wmove(win, myy - yoffset, myx);
	    waddch(win, ' ');
	 }
	 if (inwin((myy + 1))) {
	    wmove(win, myy - yoffset + 1, myx - xloc - 1);
	    waddch(win, screen[myy][myx] >= ' ' ?
		   screen[myy][myx] : ' ');
	 }
	 screen[myy][myx] = 0;
      }
      for (myx = myx - xloc - 1; myx <= WINWIDE && screen[myy + 1][myx]; myx++) {
	 screen[myy + 1][myx] = 0;
	 if (inwin((myy + 1))) {
	    wmove(win, myy - yoffset + 1, myx);
	    waddch(win, ' ');
	 }
      }
      wrefresh(win);
   }
}


static void deletechar(win, x, y, x1, y1)
WINDOW *win;
int x, y, x1, y1;
{
   char c;
   c = ' ';
   while (c && !(c == '\n') && x1 <= WINWIDE && y1 <= SCRHIGH) {
      c = screen[y1][x1];
      screen[y][x] = c;
      if (inwin(y)) {
	 wmove(win, y - yoffset, x);
	 waddch(win, c < ' ' ? ' ' : c);
      }
      screen[y1][x1] = 0;
      if (x < WINWIDE)
	 x++;
      else {
	 x = 0;
	 y++;
      }
      if (x1 < WINWIDE)
	 x1++;
      else {
	 x1 = 0;
	 y1++;
      }
   }
   for (; x <= WINWIDE; x++) {
      screen[y][x] = 0;
      if (inwin(y)) {
	 wmove(win, y - yoffset, x);
	 waddch(win, ' ');
      }
   }
   if (!(y == y1)) {
      y++;
      y1++;
      for (; y1 <= SCRHIGH && screen[y1][0]; y++, y1++)
	 for (x = 0; x <= WINWIDE; x++) {
	    screen[y][x] = screen[y1][x];
	    if (inwin(y)) {
	       wmove(win, y - yoffset, x);
	       waddch(win, screen[y][x] < ' ' ? ' ' : screen[y][x]);
	    }
	 }
      for (x = 0; x <= WINWIDE; x++) {
	 screen[y1 - 1][x] = 0;
	 if (inwin(y1 - 1)) {
	    wmove(win, y1 - 1 - yoffset, x);
	    waddch(win, ' ');
	 }
      }
   }
}


static void insertchar(win, c, x, y, mflag)
WINDOW *win;
char c;
int x, y;
char mflag;
{

   int xend = -1, yend;
   char k, nlflag;
   nlflag = 0;
   while (c && x <= WINWIDE && y <= SCRHIGH) {
      k = screen[y][x];
      screen[y][x] = c;
      if (inwin(y) && !nlflag) {
	 wmove(win, y - yoffset, x);
	 waddch(win, c >= ' ' ? c : ' ');
      }
      if (c == '\n')
	 nlflag = 1;
      if (x < WINWIDE)
	 x++;
      else {
	 x = 0;
	 y++;
      }
      c = k;
      if (xend == -1) {
	 xend = x;
	 yend = y;
      }
   }
   for (y = 0; y <= SCRHIGH; y++)
      for (x = 0; x <= WINWIDE; x++)
	 if (screen[y][x] == '\n')
	    fixnewline(win, x, y);
   if (mflag) {
      adjustwin(win, (yend + 1));
      wmove(win, yend - yoffset + 1, 0);
   } else {
      adjustwin(win, yend);
      wmove(win, yend - yoffset, xend);
   }
}



static void edit(inp, out, mesg)
char *inp, **out, *mesg;
{
   int x, y, x1, y1, insert, cmdmode;
   char c;
   char str[(SCRHIGH + 1) * (WINWIDE + 1) + 1];
   char *s;
   WINDOW *win;
   char edithelp[80];
   char kstr[10];
   char keystr[10];
   dispedkey(edcmds[EDSAVE], keystr);
   if (keystr[0] == ']')
      sprintf(keystr, "ESC-%c", edcmds[EDSAVE] & 127);
   dispedkey(edcmds[EDABORT], kstr);
   if (keystr[0] == ']')
      sprintf(kstr, "ESC-%c", edcmds[EDABORT] & 127);
   sprintf(edithelp, EDHELP, keystr, mesg, kstr, mesg);
   notifyMessageAscii(edithelp);
   openwin();
   cmplus = 0;
   cmminus = 0;
   insert = 1;
   cmdmode = 0;
   yoffset = 1;
   for (y = 0; y < SCRHIGH + 1; y++)
      for (x = 0; x < WINWIDE + 1; x++) {
	 screen[y][x] = 0;
      }
   if (inp) {
      for (s = inp, x = y = 0; *s && (x <= WINWIDE) && (y <= SCRHIGH); s++) {
	 c = screen[y][x] = *s;
	 if (*s == '\n') {
	    x = 0;
	    y++;
	 } else if (x < WINWIDE)
	    x++;
	 else {
	    x = 0;
	    y++;
	 }
      }
      if (!(c == '\n') && !(x == WINWIDE && y == SCRHIGH))
	 screen[y][x] = '\n';	/* removed when comment is saved */
   }
   adjustwin(win, 0);
   wmove(win, 0, 0);
   wrefresh(win);
   if (!screen[0][0])
      screen[0][0] = '\n';
   do {
      fixcursor();
      c = getKeyEdit();
      x = xpos(win);
      y = ypos(win) + yoffset;
      if (((edvi && !cmdmode) || !edvi) && (c >= ' ') && (c < 127)) {
	 if (insert)
	    insertchar(win, c, x, y, 0);
	 else {
	    if (screen[y][x] == '\n')
	       insertchar(win, c, x, y, 0);
	    else {
	       screen[y][x] = c;
	       waddch(win, c);
	    }
	 }
	 wrefresh(win);
      }
      if (c == CTRLL) {
	 doredraw();
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (!edvi && (c == edcmds[EDTINS]))	/* toggle insert mode */
	 insert = !insert;
      if (edvi && cmdmode && (c == edcmds[EDTINS]))	/* toggle cmdmode */
	 cmdmode = !cmdmode;
      if (edvi && !cmdmode && (c == ESC)) {	/* toggle cmdmode */
	 cmdmode = !cmdmode;
	 c = ' ';		/* get rid of the ESC */
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDDEOL])) {	/* delete to end of line */
	 x1 = x;
	 y1 = y;
	 wmove(win, y - yoffset, x);
	 wclrtoeol(win);
	 for (; x <= WINWIDE; x++)
	    screen[y][x] = 0;
	 screen[y1][x1] = '\n';
	 adjustwin(win, y1);
	 wmove(win, y1 - yoffset, x1);
	 wrefresh(win);
      }
      if (((edvi && !cmdmode) || !edvi) && ((c == '\n') || (c == '\r'))) {	/* newline, return */
	 if (y < SCRHIGH)
	    insertchar(win, '\n', x, y, 1);
	 else {
	    int xsave;
	    xsave = x;
	    screen[y][x] = '\n';
	    waddch(win, ' ');
	    for (x++; x <= WINWIDE; x++) {
	       screen[y][x] = 0;
	       waddch(win, ' ');
	    }
	    adjustwin(win, y);
	    wmove(win, y - yoffset, xsave);
	 }
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDDEL])) {	/* Delete a character */
	 y1 = y;
	 x1 = x;
	 if (x1 < WINWIDE) {
	    x1++;
	    if (!screen[y1][x1]) {
	       x1 = 0;
	       y1++;
	    }
	 } else {
	    x1 = 0;
	    y1++;
	 }
	 deletechar(win, x, y, x1, y1);
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == '\b' || c == '\177') && (y || x)) {	/* backspace */
	 y1 = y;
	 x1 = x;
	 if (x)
	    x--;
	 else {
	    y--;
	    for (x = WINWIDE; !screen[y][x]; x--);
	 }
	 deletechar(win, x, y, x1, y1);
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDUP]) && (y)) {	/* cursor up */
	 y--;
	 for (; x > 0 && !screen[y][x]; x--);
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDPUP])) {	/* page up */
	 y = yoffset - 1;
	 if (y < 0)
	    y = 0;
	 for (; x > 0 && !screen[y][x]; x--);
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDBOC])) {	/* beginning of comment */
	 y = x = 0;
	 adjustwin(win, y);
	 wmove(win, 0, 0);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDDOWN]) && (y < SCRHIGH)) {	/* cursor down */
	 y++;
	 for (; x > 0 && !screen[y][x]; x--);
	 if (!screen[y][x])
	    screen[y][x] = '\n';
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDPDOWN])) {	/* page down */
	 y = yoffset + WINHIGH + 1;
	 if (y > SCRHIGH)
	    y = SCRHIGH;
	 for (; y > 0 && !screen[y][0]; y--);
	 for (; x > 0 && !screen[y][x]; x--);
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDEOC])) {	/* end of comment */
	 for (y = SCRHIGH; y > 0 && !screen[y][0]; y--);
	 for (x = WINWIDE; x > 0 && !screen[y][x]; x--);
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDLEFT]) && (y || x)) {	/* cursor left */
	 if (x)
	    x--;
	 else {
	    y--;
	    x = WINWIDE;
	    for (; x > 0 && !screen[y][x]; x--);
	 }
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDRIGHT])
	  && !((x == WINWIDE) && (y == SCRHIGH))) {	/* cursor right */
	 if (x < WINWIDE) {
	    x++;
	    if (!screen[y][x]) {
	       y++;
	       x = 0;
	    }
	 } else {
	    y++;
	    x = 0;
	 }
	 for (; x > 0 && !screen[y][x]; x--);
	 if (!screen[y][x])
	    screen[y][x] = '\n';
	 adjustwin(win, y);
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDEOL])) {	/* cursor to end of line */
	 for (x = WINWIDE; x > 0 && !screen[y][x]; x--);
	 if (x < WINWIDE && screen[y][x] && screen[y][x] != '\n')
	    x++;
	 wmove(win, y - yoffset, x);
	 wrefresh(win);
      }
      if (((edvi && cmdmode) || !edvi) && (c == edcmds[EDBOL])) {	/* cursor to beginning
									 * of line */
	 wmove(win, y - yoffset, 0);
	 wrefresh(win);
      }
   }
   while ((((edvi && cmdmode) || !edvi) &&
	   (c != edcmds[EDSAVE]) && (c != edcmds[EDABORT]) && (c != ESC)) ||
	  (edvi && !cmdmode));
   closewin();
   notifyClearAscii();
   if (c == edcmds[EDABORT])
      *out = inp;
   else {
      for (y = SCRHIGH; y >= 0; y--)
	 for (x = WINWIDE; x >= 0; x--)
	    if ((screen[y][x] == 0) || (screen[y][x] == '\n'))
	       screen[y][x] = 0;
	    else
	       y = x = 0;
      strcpy(str, "");
      s = str;
      for (y = 0; y <= SCRHIGH; y++)
	 for (x = 0; x <= WINWIDE && screen[y][x]; x++, s++)
	    *s = screen[y][x];
      *s = 0;
      if (inp)
	 free(inp);
      *out = dupStr(str);
   }

}



static char *names[] =
{
   " Size        ",
   " Handicap    ",
   " Komi        ",
   " playerBlack ",
   " bLackrank   ",
   " playerWhite ",
   " whIterank   ",
   " Gamename    ",
   " Event       ",
   " rouNd       ",
   " Date        ",
   " Place       ",
   " Time        ",
   " Result      ",
   " gameComment ",
   " sOurce      ",
   " User        "
};




getInfoToChange()
{
   static char convert[] = "SHBLWIGENDPTRCOUK";
   static flag = 0;
   static char savearea[28][17];
   static WINDOW *wi;
   static char didhelp = 0;

   char ch;
   int con;
   if (flag == 1) {
      flag = 0;
      return t_BlackRank;
   }
   if (flag == 2) {
      flag = 0;
      return t_WhiteRank;
   }
   notifyClearAscii();
   notifyMessageAscii("Press capital letter to edit info field (? for list) or return to exit");
   while (1) {
      ch = getKey();
      if (ch == 'W')
	 flag = 2;
      if (ch == 'B')
	 flag = 1;
      if (ch == '?') {
	 saveRegion(16, 3, 13, 17);
	 for (ch = 0; ch < 17; ch++)
	    mvaddstr(ch + 3, 16, names[ch]);
	 refresh();
	 didhelp = 1;
      }
      if (ch == keys[C_COMMENTSCROLLDOWN]) {
	 if (commentLine < commentLines() - COMMENT_HIGH)
	    commentLine += COMMENT_HIGH - 1;
	 showCommentAscii(commentLine);
      } else if (ch == keys[C_COMMENTSCROLLUP])
	 if (commentLine) {
	    commentLine -= COMMENT_HIGH - 1;
	    showCommentAscii(commentLine);
	 }
      for (con = strlen(convert); con--;)
	 if (ch == convert[con]) {
	    notifyClearAscii();
	    return (int) t_Size + con;
	 }
      if (ch == '\n' || ch == '\r' || ch == ESC) {
	 notifyClearAscii();
	 if (didhelp) {
	    restoreRegion(16, 3, 13, 17);
	    didhelp = 0;
	 }
	 return (int) t_EOF;
      }
   }
}


static char *editName[] =
{"Size: ", "Handicap: ", "black's name", "Black's rank: ", "white's name", "White's rank: ", "game name", "event", "round", "date", "place", "time", "result", "game comment", "source", "user", "Komi: "};


static void editInfo(in, out, prop)
char *in, **out;
Token prop;
{
   if (prop == t_Size || prop == t_Handicap || prop == t_BlackRank
       || prop == t_WhiteRank || prop == t_Komi) {
      if (in)
	 free(in);
      *out = (char *) malloc(21);
      getLine(editName[prop - FIRSTINFO + 2], *out, 20);
   } else
      edit(in, out, editName[prop - FIRSTINFO + 2]);
}


static void commentEdit(in, out)
char *in, **out;
{
   edit(in, out, "comment");
}



interface asciiInterface =
{
   (char *) 0,
   (char *) 0,
   (char *) 0,
   (pfi) initAscii,
   (pfi) closeAscii,
   (pfi) refreshAscii,
   (pfi) plotPieceAscii,
   (pfi) displayCommentAscii,
   (pfi) clearCommentAscii,
   (pfi) initBoardAscii,
   (pfi) clearScreenAscii,
   idleAscii,
   (pfi) drawTreeAscii,
   (pfi) highlightAscii,
   (pfi) readEnvAscii,
   (pfi) notifyMessageAscii,
   (pfi) notifyClearAscii,
   getLine,
   (pfi) setCursor,
   (pfi) plotMarkAscii,
   (pfi) drawPiece,
   getPointAscii,
   (pfi) commentEdit,
   askYNAscii,
   (pfi) notifyErrorAscii,
   (pfi) displayCommentAscii,
   getInfoToChange,
   (pfi) editInfo
};
