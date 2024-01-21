/* "mgt" Copyright (c) 1991 Shodan  */


#include <curses.h>

char chars[] = "#O=+-.+|-++++";
#define doredraw() clearok(stdscr, TRUE), refresh(),clearok(stdscr, FALSE)

#define openwin() win=subwin(stdscr,WINHIGH+1,WINWIDE+1,WINTOP,WINLEFT)
/* ,wclear(win) */
#define fixcursor()
#define specialKeysEdit(c) (c)
#define closewin() delwin(win)
#define getKey getch
#define getKeyIdle getch
#define getKeyLine getch
#define getKeyScore getch
#define specialHelp()
#define preserveScreen() WINDOW *wi;wi=newwin(0,0,0,0);overwrite(stdscr,wi);touchwin(stdscr)
#define restoreScreen() overwrite(wi,stdscr);refresh();delwin(wi)

/* #define saveRegion(x1,y1,dx,dy) { WINDOW *ww;
 * ww=subwin(stdscr,dy,dx,y1,x1);wi=newwin(dy,dx,y1,x1);overwrite(ww,wi);delwin(
 * ww);} #define restoreRegion(x1,y1,dx,dy)
 * overwrite(wi,stdscr);refresh();delwin(wi) */

#define saveRegion(a,b,c,d) wi=newwin(0,0,0,0);overwrite(stdscr,wi);touchwin(stdscr)
#define restoreRegion(a,b,c,d) restoreScreen()
#define dopris() printw(" %c %c: %d %c    %c %c: %d %c ", a, chars[(int)\
          CHAR_BLACK], prisoners[0], b, c, \
	  chars[(int) CHAR_WHITE], prisoners[1], d)
#define mainwin()
#define unmainwin()

static char getKeyEdit()
{
   char c;
   c = getch();
   if (!edvi && (c == ESC)) {
      c = 128 | getch();
   }
   if (c == '\333') {		/* ('[' | 128) */
      c = getch();
      if (c == 'A')
	 c = edcmds[EDUP];
      if (c == 'B')
	 c = edcmds[EDDOWN];
      if (c == 'C')
	 c = edcmds[EDRIGHT];
      if (c == 'D')
	 c = edcmds[EDLEFT];
   }
   return c;
}

static void drawPrisoners()
{
   extern int ispl;
   char a, b, c, d;
   if (curPlayer == t_Black) {
      a = ispl ? '}' : '>';
      b = ispl ? '{' : '<';
      c = d = ' ';
   } else {
      a = b = ' ';
      c = ispl ? '}' : '>';
      d = ispl ? '{' : '<';
   }
   move(23, 50);
   printw("%c %c: %d %c    %c %c: %d %c ", a, chars[(int)
					      CHAR_BLACK], prisoners[0], b, c,
	  chars[(int) CHAR_WHITE], prisoners[1], d);
   clrtoeol();
}



static void initAscii()
{
   fixkeys();
   fixedkeys();
   initscr();
   noecho();
   crmode();
}



static void plotMarkAscii(b, i, j)
pBoard b;
int i, j;
{
   void drawPiece();
   piece p;
   p = boardGet(b, i, j);
   inverseFlag = !inverseFlag;

   drawPiece(i, j, (p == P_NOTHING ? boardPiece(i, j) :
		    (p == P_BLACK ? chars[(int) CHAR_BLACK] :
		     chars[(int) CHAR_WHITE])));
   inverseFlag = !inverseFlag;
}

static int xpos(win)
WINDOW *win;
{
   int x, y;
   getyx(win, y, x);
   return x;
}


static int ypos(win)
WINDOW *win;
{
   int y, x;
   getyx(win, y, x);
   return y;
}


static command specialKeysIdle(c)
char c;
{
   return (command) 0;
}

static void set_inverse()
{

   if (inverseFlag)
      standout();
}

static void unset_inverse()
{
   standend();
}



static void drawPiece(i, j, c)
int i, j;
char c;
{
   move(TOP + j, LEFT + 2 * i);
   set_inverse();
   addch(c);
   unset_inverse();
}


static void initBoardAscii()
{
   int i, j;

   clear();
   if (inverseFlag) {
      set_inverse();
      for (i = LEFT + 1; i < LEFT + boardsize * 2 - 2; i += 2)
	 for (j = TOP; j < TOP + boardsize; j++)
	    mvaddch(j, i, ' ');
      unset_inverse();
   }
   for (i = boardsize; i--;) {
      /* left */
      move(TOP + i, LEFT - 3);
      printw("%2d", boardsize - i);
      set_inverse();
      addch(chars[(int) CHAR_VERT]);

      /* right */
      move(TOP + i, LEFT + boardsize * 2 - 1);
      addch(chars[(int) CHAR_VERT]);
      unset_inverse();
      printw("%2d", boardsize - i);
      /* printw("%2d%c", boardsize - i, chars[(int) CHAR_VERT]); */

      /* top */
      move(TOP - 2, LEFT + i * 2);
      addch(xAxisChars[i]);
      move(TOP - 1, LEFT + i * 2 - 1);
      set_inverse();
      printw("%c%c", chars[(int) CHAR_HORIZ], chars[(int) CHAR_HORIZ]);

      /* bottom */
      move(TOP + boardsize, LEFT + i * 2 - 1);
      printw("%c%c", chars[(int) CHAR_HORIZ], chars[(int) CHAR_HORIZ]);
      unset_inverse();
      move(TOP + boardsize + 1, LEFT + i * 2);
      addch(xAxisChars[i]);
      for (j = boardsize; j--;)
	 drawPiece(i, j, boardPiece(i, j));
   }
   set_inverse();
   move(TOP - 1, LEFT - 1);
   addch(chars[(int) CHAR_UPLEFT]);
   move(TOP - 1, LEFT + boardsize * 2 - 1);
   addch(chars[(int) CHAR_UPRIGHT]);
   move(TOP + boardsize, LEFT - 1);
   addch(chars[(int) CHAR_DOWNLEFT]);
   move(TOP + boardsize, LEFT + boardsize * 2 - 1);
   addch(chars[(int) CHAR_DOWNRIGHT]);
   unset_inverse();
   mvaddstr(COMMAND_Y, COMMAND_X, helpMsg);
   printstatus(name_buf);
}
