/* "mgt" Copyright (c) 1991 Shodan  */

char chars[] = "\3O?+-\372.\263\304\332\277\300\331";
char graphchar[] = "\2O?\17.\305\307\335     ";

int graphicalCursor = 1;
int graphics = 1;
int usecolor = 1;

int boardcolor = 6;
int blackstone = 0;
int damecolor = 1;
int whitestone = 7;
int background = 0;
int foreground = 7;
int markcolor = 9;
int lettercolor = 1;
int menufg = 2;
int menubg = 0;

int branchmode = 0;
int stonemode = 1;

int bred = /* 0x3f; 63 44  40; */ 39;
int bblue = /* 0x16; 22 15  14; */ 5;
int bgreen = /* 0x2e; 46 32  30;  */ 33;

int fixfont = 0;

int menu = 0;

int cursorkey = 0;		/* 0 tree mode 1 movement 2 variable */

char *stoneModeName[] =
{"mark", "stone"};
char *branchModeName[] =
{"walk", "branch"};
char *menumode[] =
{"move", "play", "edit", "files"};

#define MENUITEMS 4


/* 6789012345678901234567890123456789012345 */
char *menulines[] =
{"    start  end  var  comment    ",
 "       score  pass  player       ",
 "info  cut  paste  delnode  addvar",
 "     save  load  next  prev      "};


struct {
   char bottom, top;
}  menubounds[4][5] = {
   {{
	 -1, -1
   }, {
      10, 14
   }, {
      17, 19
   }, {
      22, 24
   }, {
      27, 33
   }
   },
   {{
	 -1, -1
   }, {
      13, 17
   }, {
      20, 23
   }, {
      26, 31
   }, {
      -1, -1
   }
   },
   {{
	 6, 9
   }, {
      12, 14
   }, {
      17, 21
   }, {
      24, 30
   }, {
      33, 38
   }
   },
   {{
	 -1, -1
   }, {
      11, 14
   }, {
      17, 20
   }, {
      23, 26
   }, {
      29, 32
   }
   }
};


#include <conio.h>
#include <dos.h>
#include "mou.h"

#define doredraw()
#define move(Y, X) gotoxy((X)+1, (Y)+1)
#define mvaddstr(Y, X, STRING) gotoxy((X)+1, (Y)+1), cputs(STRING)
#define mvprintw(Y, X, STRING) gotoxy((X)+1, (Y)+1), cputs(STRING)
#define mvaddch(Y, X, CHAR) gotoxy((X)+1, (Y)+1), putch(CHAR)
#define addch(CHAR) putch(CHAR)
#define printw cprintf
#define clrtoeol() clreol()
#define clear() clrscr()
#define refresh()
#define wrefresh(win)
#define wmove(win,y,x) move(y,x)
#define waddch(win,c) addch(c)
#define mvwaddch(win,y,x,c) mvaddch(y,x,c)
#define waddstr(win,c) cputs(c)
#define wclrtoeol(win) clreol()
#define closewin() window(1,1,80,25)
#define xpos(w) (wherex()-1)
#define ypos(w) (wherey()-1)
#define WINDOW char
#define openwin() win=0,window(WINLEFT+1,WINTOP+1,WINRIGHT+1,WINBOT+2),clear()
#define fixcursor() if (ypos(win)>WINHIGH) wmove(win,WINHIGH,WINWIDE);
#define preserveScreen() char screen[160][25];gettext(1,1,80,25,&screen)
#define restoreScreen() puttext(1,1,80,25,&screen)
#define restoreRegion(x1,y1,dx,dy) puttext(x1+1,y1+1,x1+1+dx,y1+dy,savearea)
#define saveRegion(x1,y1,dx,dy)    gettext(x1+1,y1+1,x1+1+dx,y1+dy,savearea)
#define normalize(c) (c)
#define set_inverse()
#define unset_inverse()
#define mainwin() closewin()
#define unmainwin() window(WINLEFT+1,WINTOP+1,WINRIGHT+1,WINBOT+2)

void wclrtobot(win)
int *win;
{
   int savex, savey, y;
   savex = wherex();
   savey = wherey();
   clreol();
   for (y = savey + 1; y <= WINBOT + 2; y++) {
      gotoxy(1, y);
      clreol();
   }
   gotoxy(savex, savey);
}


static unsigned char far *getfont()
{
   struct REGPACK regs;
   regs.r_ax = 0x1130;
   regs.r_bx = 0x0600;
   intr(0x10, &regs);
   return (unsigned char far *) MK_FP(regs.r_es, regs.r_bp);
}


static void setfont(unsigned char far * newfont)
{
   struct REGPACK regs;
   regs.r_cx = 256;
   regs.r_bx = 0x1000;
   regs.r_dx = 0;
   regs.r_ax = 0x1100;
   regs.r_es = FP_SEG(newfont);
   regs.r_bp = FP_OFF(newfont);
   intr(0x10, &regs);
}

static void endwin()
{
   closewin();
   if (graphics && isegavga())
      setfont(getfont());
   normvideo();
   MOUdeinit();
   clrscr();
   if (fixfont) {
      _AX = 0x1112;
      _BL = 0;
      geninterrupt(0x10);
   }
}


static void setcolor(bg, fg)
int bg, fg;

{
   if (usecolor)
      textattr((bg << 4) + fg);
}


static void drawPrisoners()
{
   extern int ispl;
   char a, b, c, d;
   if (curPlayer == t_Black) {
      a = ispl ? '\20' : '>';
      b = ispl ? '\21' : '<';
      c = d = ' ';
   } else {
      a = b = ' ';
      c = ispl ? '\20' : '>';
      d = ispl ? '\21' : '<';
   }
   move(23, 49);
   if (usecolor) {
      setcolor(boardcolor, blackstone);
      printw(" %c %c: %d %c    ", a, chars[CHAR_BLACK], prisoners[0], b);
      setcolor(boardcolor, whitestone);
      printw("%c %c: %d %c ", c, chars[CHAR_BLACK], prisoners[1], d);
      setcolor(background, foreground);
   } else
      printw(" %c %c: %d %c    %c %c: %d %c ", a, chars[(int)
					      CHAR_BLACK], prisoners[0], b, c,
	     chars[(int) CHAR_WHITE], prisoners[1], d);
   clrtoeol();
}



static char getKey()
{
   MOUINFOREC m;
   for (;
	;
      ) {
      if (MOUcheck()) {
	 MOUget(&m);
	 if (m.buttonstat & (LEFTBPRESS | RIGHTBPRESS))
	    return ' ';
      } if (kbhit())
	 return getch();
   }
}


int iskey()
{
   _AH = 1;
   geninterrupt(0x16);
   return (!(_FLAGS & 64));
}



char altkey[] = "qwertyuiop????asdfghjkl?????zxcvbnm";


char getKeyEdit()
{
   MOUINFOREC m;
   int key;

   for (;
	;
      ) {
      if (MOUcheck())
	 MOUget(&m);
      if (iskey()) {
	 _AX = 0;
	 geninterrupt(0x16);
	 if (_AL == ESC) {
	    _AX = 0;
	    geninterrupt(0x16);
	    key = _AL;
	    return 128 | key;
	 }
	 if (_AL)
	    return _AL;
	 key = _AH;
	 switch (key) {
	    case 77:
	       return edcmds[EDRIGHT];
	    case 75:
	       return edcmds[EDLEFT];
	    case 80:
	       return edcmds[EDDOWN];
	    case 72:
	       return edcmds[EDUP];
	    case 79:
	       return edcmds[EDEOL];
	    case 71:
	       return edcmds[EDBOL];
	    case 82:
	       return edcmds[EDTINS];
	    case 83:
	       return edcmds[EDDEL];
	    case 73:
	       return edcmds[EDPUP];
	    case 81:
	       return edcmds[EDPDOWN];
	    case 119:		/* ctrl home */
	       return 0;
	    case 117:		/* ctrl end */
	       return edcmds[EDDEOL];
	    case 132:		/* Ctrl Pg Up */
	       return edcmds[EDBOC];
	    case 118:		/* Ctrl Pg Dn */
	       return edcmds[EDEOC];
	    default:
	       if (key >= 16 && key <= 50) {
		  return altkey[key - 16] | 128;
	       }
	       return 0;
	 }
      }
   }
}



char getKeyLine()
{
   static lastmouse = 0;
   MOUINFOREC m;
   for (;
	;
      ) {
      if (MOUcheck()) {
	 MOUget(&m);
	 if (m.buttonstat & LEFTBPRESS) {
	    if (lastmouse == 1) {
	       lastmouse = 0;
	       return '\r';
	    }
	    if (!lastmouse) {
	       lastmouse = 1;
	       return 'y';
	    }
	    lastmouse = 0;
	    return '\b';
	 }
	 if (m.buttonstat & RIGHTBPRESS) {
	    if (lastmouse == 2) {
	       lastmouse = 0;
	       return '\r';
	    }
	    if (!lastmouse) {
	       lastmouse = 2;
	       return 'n';
	    }
	    lastmouse = 0;
	    return '\b';
	 }
      }
      if (kbhit()) {
	 lastmouse = 0;
	 return getch();
      }
   }
}


static char scoring = 0;


void printmodes()
{
   setcolor(menubg, menufg);
   move(0, 4);
   printw("  help   quit %7s%7s%7s", stoneModeName[stonemode], branchModeName[branchmode], menumode[menu]);
   setcolor(background, foreground);
   setCursor(xcur, ycur);
}


int getmenu(cx)
int cx;
{
   int i;
   for (i = 0;
	i < 5;
	i++)
      if (cx <= menubounds[menu][i].top && cx >= menubounds[menu][i].bottom)
	 return (menu << 4) + i;
   return -1;
}


int checkLeftButton(cx, cy)
int cx, cy;
{
   if (cy == COMMAND_Y && cx >= COMMAND_X) {
      cx -= COMMAND_X;
      if (cx < 10)
	 return '?';
      if (cx >= 15 && cx < 20)
	 return keys[C_TUTORSWAP];
      if (cx >= 24 && cx < 29)
	 return keys[C_SAVESHORT];
      cx += COMMAND_X;
   }
   if (cy >= COMMENT_Y && cy < COMMENT_Y + COMMENT_HIGH && cx >= COMMENT_X)
      return keys[C_EDCOMMENT];
   if ((cy == 22) && usecolor) {
      switch (getmenu(cx)) {
	 case 0x01:
	    return keys[C_BEGINNING];
	 case 0x02:
	    return keys[C_END];
	 case 0x03:
	    return keys[C_DOWNFORK];
	 case 0x04:
	    return keys[C_SEARCHCOMMENT];

	 case 0x11:
	    return keys[C_SCORE];

	 case 0x12:
	    return keys[C_PASSMOVE];

	 case 0x13:
	    return keys[C_TOPLAY];



	 case 0x20:
	    return keys[C_INFO];
	 case 0x21:
	    return keys[C_TREECUT];
	 case 0x22:
	    return keys[C_PASTE];
	 case 0x23:
	    return keys[C_DELNODE];
	 case 0x24:
	    return keys[C_ADDVAR];

	 case 0x31:
	    return keys[C_WRITE];
	 case 0x32:
	    return keys[C_LOAD];
	 case 0x33:
	    return keys[C_NEXTFILE];
	 case 0x34:
	    return keys[C_BACKFILE];
      }
   }
   if (cy == STATUS_Y && cx >= STATUS_X) {
      return keys[filecount ? C_NEXTFILE : C_LOAD];
   }
   if ((cy == 0) && usecolor) {
      MOUhide();
      switch ((cx - 4) / 7) {
	 case 0:
	    MOUshow();
	    return '?';
	 case 1:
	    MOUshow();
	    return keys[C_QUIT];
	 case 2:
	    stonemode = !stonemode;
	    printmodes();
	    break;
	 case 3:
	    branchmode = !branchmode;
	    printmodes();
	    break;
	 case 4:
	    menu++;
	    menu %= MENUITEMS;
	    printmodes();
	    setcolor(menubg, menufg);
	    mvaddstr(22, 6, menulines[menu]);
	    setcolor(background, foreground);
	    setCursor(xcur, ycur);
	    break;
      }
      MOUshow();
   }
   if (scoring && cy == 23 && cx <= 39) {
      if (cx <= 12)
	 return '\r';
      if (cx <= 24)
	 return 0;
      else if (cx <= 32)
	 return 'u';
      else if (cx <= 39)
	 return 'q';
   }
   if (!((cx - LEFT) & 1) && inRange(cy - TOP, (cx - LEFT) / 2)) {
      xcur = (cx - LEFT) / 2;
      ycur = (cy - TOP);
      return stonemode || scoring ? 32 : keys[C_ADDLETTER];
   }
   if ((cy == 23) && ((cx >= 48 && cx <= 54 && curPlayer != t_Black) ||
		      (cx >= 59 && cx <= 67 && curPlayer == t_Black)))
      return keys[C_TOGGLESTONE];
   if (cx == COMMENT_X - 1 && cy == COMMENT_Y)
      return keys[C_COMMENTSCROLLUP];
   if (cx == TREE_X - 1 && cy == TREE_Y + 1)
      return keys[C_TREESCROLLUP];
   if (cx == TREE_X - 1 && cy == TREE_Y + TREEHIGH - 1)
      return keys[C_TREESCROLLDOWN];
   if (cx == COMMENT_X - 1 && cy == COMMENT_Y + COMMENT_HIGH - 1)
      return keys[C_COMMENTSCROLLDOWN];
   if (cx >= TREE_X && cy > TREE_Y && cy < TREE_Y + TREEHIGH)
      return branchmode ? 'A' + treeLine + cy - TREE_Y - 1 : keys[C_WALKDOWN];
   if (cy == TREE_Y) {
      if (cx > TREE_X + 10)
	 return keys[C_ADDNAME];
      if (cx > TREE_X + 1)
	 return keys[C_GOTO];
   }
   return 0;
}




int checkRightButton(cx, cy)
int cx, cy;
{

   if (scoring)
      return '\r';
   if ((cy == 22) && usecolor) {
      if (getmenu(cx) == 3)
	 return keys[C_UPFORK];
      if (getmenu(cx) == 4)
	 return keys[C_SEARCHBACKCOMMENT];
   }
   if ((!((cx - LEFT) & 1))
       && inRange(cy - TOP, (cx - LEFT) / 2)) {
      xcur = (cx - LEFT) / 2;
      ycur = (cy - TOP);
      return keys[stonemode ?
		  (curPlayer == t_Black ? C_ADDBLACK : C_ADDWHITE)
		  :
		  C_ADDMARK];
   }
   if (cy == STATUS_Y && cx >= STATUS_X && filecount) {
      return keys[C_BACKFILE];
   }
   if (cx >= TREE_X && cy > TREE_Y && cy < TREE_Y + TREEHIGH)
      return keys[branchmode ? C_UP : C_WALKUP];
   return 0;
}




char getKeyIdle()
{
   int key;
   MOUINFOREC m;

   MOUshow();
   for (;;) {
      if (MOUcheck()) {
	 MOUget(&m);
	 if ((m.buttonstat & LEFTBPRESS) && (key = checkLeftButton(m.cx, m.cy))) {
	    MOUhide();
	    return key;
	 }
	 if ((m.buttonstat & RIGHTBPRESS) && (key = checkRightButton(m.cx, m.cy))) {
	    MOUhide();
	    return key;
	 }
      }
      if (kbhit()) {
	 MOUhide();
	 return getch();
      }
   }
}



char getKeyScore()
{
   char temp;
   scoring = 1;
   temp = getKeyIdle();
   scoring = 0;
   return temp;
}




static void initAscii()
{
   char stone[] =
   {0x00, 0x00, 0x00, 0x7E, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0x7E, 0x00, 0x00, 0x00};
   char terr[] =
   {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3c, 0x7e,
    0x7e, 0x3c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
   char hoshi[] =
   {0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
    0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18};

   /* note 0x3c and 0xff for dot in hoshi are set below. */

   unsigned char *dest, far * font;
   int i, j, k;
   unsigned char myfont[256][16];
   extern vgapresent;
   struct text_info info;

   gettextinfo(&info);
   fixfont = info.screenheight == 50 || info.screenheight == 43;

   textmode(C80);

   if (graphics && isegavga()) {
      _AX = 0x1003;
      _BX = 0;
      geninterrupt(0x10);	/* enable high intensity background */

      if (vgapresent) {
	 _AX = 0x1010;
	 _BX = 0x0014;
	 _CH = bgreen;
	 _CL = bblue;
	 _DH = bred;

	 geninterrupt(0x10);	/* change color 6 */
      }
      font = getfont();

      /* adaptively set hoshi point character */
      for (k = 0; k < 16; k++)
	 if (*(font + 197 * 16 + k) == 255)
	    break;
      if (k == 16)
	 k = 9;
      for (i = k - 2; i <= k + 2; i++)
	 hoshi[i] = 0x3c;
      hoshi[k] = 0xff;

      for (dest = (char *) myfont, k = 0; k < 16 * 256; k++, dest++, font++)
	 *dest = *font;
      for (k = 0; k < 16; k++) {
	 myfont[2][k] = stone[k];
	 myfont[15][k] = terr[k];
	 myfont[199][k] = hoshi[k];
      }
      setfont((unsigned char far *) myfont);
      strcpy(chars, graphchar);
      usecolor = 1;
   } else
      graphics = 0;
   if ((!graphics) && usecolor)
      strcpy(chars + CHAR_VERT, graphchar + CHAR_VERT);

   MOUinit();
   MOUhide();
   fixkeys();
   fixedkeys();
}



static void plotMarkAscii(b, i, j)
pBoard b;
int i, j;
{
   void plotPieceAscii();
   int sc;

   if (usecolor) {
      sc = boardcolor;
      boardcolor = markcolor;
   } else
      textattr(inverseFlag ? 0x07 : 0x70);
   plotPieceAscii(b, i, j);
   if (usecolor) {
      boardcolor = sc;
   } else
      textattr(inverseFlag ? 0x70 : 0x07);
}



static command specialKeysIdle(c)
char c;
{
   if (!c) {
      c = getKey();
      switch (c) {
	 case 77:
	    return (cursorkey == 0 || (cursorkey == 2 && tutor)) ? C_DOWN : C_CURRIGHT;
	 case 75:
	    return (cursorkey == 0 || (cursorkey == 2 && tutor)) ? C_UP : C_CURLEFT;
	 case 79:
	    return (cursorkey == 0 || (cursorkey == 2 && tutor)) ? 0 : C_DOWNLEFT;
	 case 80:
	    return (cursorkey == 0 || (cursorkey == 2 && tutor)) ? C_DOWNFORK : C_CURDOWN;
	 case 71:
	    return (cursorkey == 0 || (cursorkey == 2 && tutor)) ? 0 : C_UPLEFT;
	 case 72:
	    return (cursorkey == 0 || (cursorkey == 2 && tutor)) ? C_UPFORK : C_CURUP;
	 case 81:
	    return (cursorkey == 0 || (cursorkey == 2 && tutor)) ? C_SEARCHCOMMENT : C_DOWNRIGHT;
	 case 73:
	    return (cursorkey == 0 || (cursorkey == 2 && tutor)) ? C_SEARCHBACKCOMMENT : C_UPRIGHT;
	 default:
	    return (command) 0;
      }
   } else
      return (command) 0;
}




static void drawPiece(i, j, c)
int i, j;
char c;
{
   move(TOP + j, LEFT + 2 * i);
   setcolor(boardcolor, blackstone);

   if (c == chars[(int) CHAR_NOTHING]) {
      if (graphics) {
	 if (i == 0) {
	    if (j == boardsize - 1)
	       c = '\300';
	    else if (j == 0)
	       c = '\332';
	    else
	       c = '\303';
	 } else if (i == boardsize - 1) {
	    if (j == boardsize - 1)
	       c = '\331';
	    else if (j == 0)
	       c = '\277';
	    else
	       c = '\264';
	 } else if (j == 0)
	    c = '\302';
	 else if (j == boardsize - 1)
	    c = '\301';
      }
   }
   if (usecolor) {
      if (c == chars[(int) CHAR_WHITE]) {
	 c = chars[(int) CHAR_BLACK];
	 setcolor(boardcolor, whitestone);
      }
      if (c == chars[(int) CHAR_WHITETERR]) {
	 c = chars[(int) CHAR_BLACKTERR];
	 setcolor(boardcolor, whitestone);
      }
      if (c == chars[(int) CHAR_DAME]) {
	 setcolor(boardcolor, damecolor);
      }
      if (c >= 'a' && c <= 'z')
	 setcolor(boardcolor, lettercolor);
   }
   addch(c);
   setcolor(background, foreground);
}


static void initBoardAscii()
{
   int i, j;

   if (!usecolor) {
      if (inverseFlag)
	 textattr(0x70);
      else
	 textattr(0x07);
   }
   if (inverseFlag && usecolor) {
      inverseFlag = foreground;
      foreground = background;
      background = inverseFlag;
      menubg = background;
      inverseFlag = 0;
   }
   setcolor(background, foreground);
   clrscr();
   if (mouseinstalled & usecolor) {
      printmodes();
      setcolor(menubg, menufg);
      mvaddstr(22, 6, menulines[menu]);
      setcolor(background, foreground);
   }
   if (usecolor) {
      setcolor(boardcolor, blackstone);
      for (i = LEFT + 1; i < LEFT + boardsize * 2 - 2; i += 2)
	 for (j = TOP; j < TOP + boardsize; j++)
	    mvaddch(j, i, graphics ? '\304' : ' ');
   }
   for (i = boardsize; i--;) {

      /* left */
      set_inverse();
      move(TOP + i, LEFT - 1);
      setcolor(boardcolor, background);
      addch(chars[(int) CHAR_VERT]);

      setcolor(background, boardcolor);
      /* top */
      move(TOP - 1, LEFT + i * 2 - 1);
      printw("%c%c", chars[(int) CHAR_HORIZ], chars[(int) CHAR_HORIZ]);

      /* bottom */
      move(TOP + boardsize, LEFT + i * 2 - 1);
      printw("%c%c", chars[(int) CHAR_HORIZ], chars[(int) CHAR_HORIZ]);

      /* right */
      move(TOP + i, LEFT + boardsize * 2 - 1);
      setcolor(background, boardcolor);
      addch(chars[(int) CHAR_VERT]);
      setcolor(background, foreground);

      unset_inverse();

      /* right */
      printw("%-2d%c", boardsize - i, '\263');

      /* left */
      move(TOP + i, LEFT - 3);
      printw("%2d", boardsize - i);


      /* top */
      move(TOP - 2 + usecolor, LEFT + i * 2);
      addch(xAxisChars[i]);

      /* bottom */
      move(TOP + boardsize + 1 - usecolor, LEFT + i * 2);
      addch(xAxisChars[i]);

      for (j = boardsize; j--;)
	 drawPiece(i, j, boardPiece(i, j));
   }
   set_inverse();
   move(TOP - 1, LEFT - 1);
   addch(chars[(int) CHAR_UPLEFT]);
   move(TOP + boardsize, LEFT - 1);
   addch(chars[(int) CHAR_DOWNLEFT]);
   move(TOP - 1, LEFT + boardsize * 2 - 1);
   addch(chars[(int) CHAR_UPRIGHT]);
   move(TOP + boardsize, LEFT + boardsize * 2 - 1);
   addch(chars[(int) CHAR_DOWNRIGHT]);
   unset_inverse();
   mvaddstr(COMMAND_Y, COMMAND_X, helpMsg);
   printstatus(name_buf);
}

static char *mouseHelp[] =
{
   "                                 Mouse Commands",
   "Click on           (left)=left click         (right)=right click",
   " board        \"stone\" mode: makes a move (left)  sets stone (right)",
   "              \"mark\" mode: places letter (left)  places mark (right)",
   "",
   " \"Node #\"     Goes to node by number",
   " node name    Sets node name",
   " comment      Edits the comment",
   " \"+\" or \"-\"   (next to comment or variations) scroll the comment/variations",
   " filename     Next (left) or previous (right) file, or load file.",
   " variation    Go to that variation (left) back up (right)",
   " long/short   Toggles save file format",
   " read/edit/tutor Toggles tutor mode",
   "",
"When scoring a game, right button scores, left buttons kills, and clicking ",
   "on words at the bottom of the screen executes whatever function.",
   "",
   "Mouse clicks will remove error messages.",
   "At a (y/n) prompt, double click left to respond yes, double click right for no",
   "",
   "Menus above and below the board provide other functions",
   "",
   "                         Hit a key (or click) to continue"};

static char *ibmHelp[] =
{"               Cursor Keys",
 "",
 "To move the cursor on the board",
 "you must use the number keys",
 "(have num lock on)",
 "",
 "The cursor keys have the following functions:",
 "",
 "right  down tree like ",
 "left   up tree like ",
 "up     previous variation branch",
 "down   next variation branch",
 "pgup   last comment",
 "pgdn   next comment",
 "",
 "         press a key"
};



static void specialHelp()
{
   int i;
   char ch;
   clrscr();
   for (i = 0; i < sizeof(ibmHelp) / sizeof(char *); i++) {
      move(i + 5, 14);
      cputs(ibmHelp[i]);
      if (i == 8)
	 addch(keys[C_DOWN]);
      if (i == 9)
	 addch(keys[C_UP]);
   }
   ch = getKey();
   if (ch != '\n' && ch != '\r' && mouseinstalled) {
      clrscr();
      for (i = 0; i < sizeof(mouseHelp) / sizeof(char *); i++) {
	 move(i, 0);
	 if (i == 4) {
	    printw(" ");
	    if (usecolor)
	       setcolor(boardcolor, blackstone);
	    printw(" %c: ?? ", chars[CHAR_BLACK]);
	    if (usecolor)
	       setcolor(background, foreground);
	    printw("      selects black stones, ");
	    if (usecolor) {
	       setcolor(boardcolor, whitestone);
	       printw(" %c: ?? ", chars[CHAR_BLACK]);
	       setcolor(background, foreground);
	    } else
	       printw(" %c: ?? ", chars[CHAR_WHITE]);
	    printw(" selects white stones.");


	 }
	 cputs(mouseHelp[i]);
      }
      getKey();
      clrscr();
      printmodes();


      mvaddstr(1, 20, " \263      \263      \300\304 indicates menu chosen.  Click here to");
      mvaddstr(2, 20, " \263      \263         switch to a different menu.");
      mvaddstr(3, 20, " \263      \300\304 type of movement through game tree (see previous");
      move(4, 20);
      printw(" \263         screen).  Branch for movement like %c and %c, ", keys[C_DOWN], keys[C_UP]);
      move(5, 20);
      printw(" \263         walk for movement like %c and %c. ", keys[C_WALKDOWN], keys[C_WALKUP]);
      mvaddstr(6, 20, " \300\304 Mouse button on the board sets stones or sets marks.");
      mvaddstr(7, 20, "    Click here to toggle.");

      setcolor(menubg, menufg);
      mvaddstr(9, 0, menulines[0]);
      setcolor(background, foreground);
      mvaddstr(10, 2, "Click on start or end to go to start of file or end of current variation.");
      mvaddstr(11, 2, "Left click on var or comment to go to next variation branch or comment.");
      mvaddstr(12, 2, "Right click to go to last variation branch or comment.");
      setcolor(menubg, menufg);
      mvaddstr(13, 0, menulines[1]);
      setcolor(background, foreground);

      mvaddstr(14, 2, "Click on score to score, pass to play a pass move, and player to toggle the");
      mvaddstr(15, 2, "current player with an item in the game tree.");

      setcolor(menubg, menufg);
      mvaddstr(16, 0, menulines[2]);
      setcolor(background, foreground);

      mvaddstr(17, 2, "Click on info to get info, cut or paste to cut and paste, delnode to delete a");
      mvaddstr(18, 2, "node or addvar to add a variation.");

      setcolor(menubg, menufg);
      mvaddstr(19, 0, menulines[3]);
      setcolor(background, foreground);

      mvaddstr(20, 2, "Click on save to save the game record, load to load a new file, next to go to");
      mvaddstr(21, 2, "the next file or prev to go to the previous file.");

      mvaddstr(23, 24, "Hit a key or click to continue.");
      getKey();

   }
}
