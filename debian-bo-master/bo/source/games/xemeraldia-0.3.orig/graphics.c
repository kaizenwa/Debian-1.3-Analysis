/*                             */
/* xemeraldia   --- graphics.c */
/*                             */

#include "graphics.h"
#include "games.h"

static void   printStar ();
static void  animateTmpScore ();

XtIntervalId  timer, tmp_sc_timer;

struct TmpSc
{
  long sc;
  int  x, y;
  char cnt;
};
static struct TmpSc  tmpSc[50];


void  RedrawBoard (w, event, parms, num_parms)
     Widget        w;
     XExposeEvent *event;
     String       *parms;
     Cardinal     *num_parms;
{
  if (! paused)
    {
      XCopyArea (XtDisplay (w), board_pix, XtWindow (w), draw_gc,
		 0, 0, WIN_WIDTH, WIN_HEIGHT, 0, 0);
    }
  else
    {
      XFillRectangle (XtDisplay (w), XtWindow (w), delete_gc, 0, 0,
		      WIN_WIDTH, WIN_HEIGHT);

      XDrawRectangle (XtDisplay (w), XtWindow (w), draw_gc,
		      DIFF_X / 2, DIFF_Y / 2,
		      BLOCK_WIDTH * BOARD_WIDTH + DIFF_X,
		      BLOCK_HEIGHT * BOARD_HEIGHT + DIFF_Y);
      XDrawImageString (XtDisplay (board_w), XtWindow (board_w), draw_gc,
			90, 220, "PAUSE!", 6);
    }
}


void  RedrawNextItem (w, event, parms, num_parms)
      Widget        w;
      XExposeEvent *event;
      String       *parms;
      Cardinal     *num_parms;
{
  clearNextItem ();
  if ((! gameover_flag) && (! paused))
    printNextItem ();
}


void  showTmpScore (tmp_sc, sc_x, sc_y, ch_s)
     long  tmp_sc;
     int   sc_x, sc_y;
     long  ch_s;
{
  char  tmp_sc_str[8];

  tmpSc[ch_s].x = sc_x;  tmpSc[ch_s].y = sc_y;
  tmpSc[ch_s].sc = tmp_sc;
  tmpSc[ch_s].cnt = 8;
  sprintf (tmp_sc_str, "%7ld", tmpSc[ch_s].sc);
  XDrawString (XtDisplay (board_w), XtWindow (board_w), draw_gc,
	       (tmpSc[ch_s].x - 1) * BLOCK_WIDTH ,
	       (tmpSc[ch_s].y + 1) * BLOCK_HEIGHT - 24 + tmpSc[ch_s].cnt * 3,
	       tmp_sc_str, 8);
  XSync (XtDisplay (board_w), FALSE);
  --tmpSc[ch_s].cnt;
  tmp_sc_timer = XtAppAddTimeOut (XtWidgetToApplicationContext (board_w),
				  20, animateTmpScore, (XtPointer) ch_s);

}


static void  animateTmpScore (ch_s)
     long ch_s;
{
  char  tmp_sc_str[8];

  XCopyArea (XtDisplay (board_w), board_pix, XtWindow (board_w), draw_gc,
	     (tmpSc[ch_s].x - 1) * BLOCK_WIDTH,
	     (tmpSc[ch_s].y + 1) * BLOCK_HEIGHT - 43 + tmpSc[ch_s].cnt * 3,
	     80, 25,
	     (tmpSc[ch_s].x - 1) * BLOCK_WIDTH,
	     (tmpSc[ch_s].y + 1) * BLOCK_HEIGHT - 43 + tmpSc[ch_s].cnt * 3);
  sprintf (tmp_sc_str, "%7ld", tmpSc[ch_s].sc);
  XDrawString (XtDisplay (board_w), XtWindow (board_w), draw_gc,
	       (tmpSc[ch_s].x - 1) * BLOCK_WIDTH ,
	       (tmpSc[ch_s].y + 1) * BLOCK_HEIGHT - 24 + tmpSc[ch_s].cnt * 3,
	       tmp_sc_str, 8);
  XSync (XtDisplay (board_w), FALSE);
  if (--tmpSc[ch_s].cnt > 0)
    tmp_sc_timer = XtAppAddTimeOut (XtWidgetToApplicationContext (board_w),
				    45, animateTmpScore, (XtPointer) ch_s);
  else
    XCopyArea (XtDisplay (board_w), board_pix, XtWindow (board_w), draw_gc,
	       (tmpSc[ch_s].x - 1) * BLOCK_WIDTH,
	       (tmpSc[ch_s].y + 1) * BLOCK_HEIGHT - 43 + tmpSc[ch_s].cnt * 3,
	       80, 25,
	       (tmpSc[ch_s].x - 1) * BLOCK_WIDTH,
	       (tmpSc[ch_s].y + 1) * BLOCK_HEIGHT - 43 + tmpSc[ch_s].cnt * 3);
}


void  clearNextItem ()
{
  XFillRectangle (XtDisplay (nextItem_w), XtWindow (nextItem_w),
		  delete_gc, 0, 0,
		  BLOCK_WIDTH * 3, BLOCK_HEIGHT * 3);
}


void  printNextItem ()
{
  int  i;

  clearNextItem ();
  if (next_i.col[0] == STAR)
    XCopyArea (XtDisplay (nextItem_w), star, XtWindow (nextItem_w), draw_gc,
	       0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
	       BLOCK_WIDTH / 2 + DIFF_X, BLOCK_HEIGHT /2 + DIFF_Y);
  else
    {
      for (i = 0; i < 3; i++)
	XCopyArea (XtDisplay (nextItem_w), block[next_i.col[i]],
		   XtWindow (nextItem_w), draw_gc,
		   0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
		   BLOCK_WIDTH * (iRot_vx[0][i]) + DIFF_X,
		   BLOCK_HEIGHT * (1 + iRot_vy[0][i]) + DIFF_Y);
    }
}


void  clearScreen (w)
      Widget  w;
{
  XFillRectangle (XtDisplay (w), board_pix, delete_gc, 0, 0,
		  WIN_WIDTH, WIN_HEIGHT);
  XDrawRectangle (XtDisplay (w), board_pix, draw_gc, DIFF_X / 2, DIFF_Y / 2,
		  BLOCK_WIDTH * BOARD_WIDTH + DIFF_X,
		  BLOCK_HEIGHT * BOARD_HEIGHT + DIFF_Y);
}


void  printItem ()
{
  int  i;

  if (star_comes)
    printStar (drop_i.x, drop_i.y);
  else
    for (i = 0; i < 3; i++)
      {
	printBlock (drop_i.x + iRot_vx[drop_i.rot][i],
		    drop_i.y + iRot_vy[drop_i.rot][i],
		    drop_i.col[i]);
	board[drop_i.x + iRot_vx[drop_i.rot][i]]
          [drop_i.y + iRot_vy[drop_i.rot][i]].blk = drop_i.col[i];
      }
}


void  printBlock (x, y, color)
      int  x, y, color;
{
  int  pixmap_number;

  if ((board[x][y].sub == CRACKED) || (board[x][y].sub == DELETE))
    pixmap_number = color + BLOCK_VARIETY;
  else  pixmap_number = color;
  if ((color > 0) && (color <= BLOCK_VARIETY * 2))
    {
      XCopyArea (XtDisplay (board_w), block[pixmap_number],
		 board_pix, draw_gc,
		 0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
		 BLOCK_WIDTH * x - DIFF_X, BLOCK_HEIGHT * y + DIFF_Y);
      XCopyArea (XtDisplay (board_w), block[pixmap_number],
		 XtWindow (board_w), draw_gc,
		 0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
		 BLOCK_WIDTH * x - DIFF_X, BLOCK_HEIGHT * y + DIFF_Y);
    }
}


void  deleteBlocks ()
{
  int  x, y;

  for (x = 1; x <= BOARD_WIDTH; x++)
    for (y = 0; y <= BOARD_HEIGHT; y++)
      if (board[x][y].sub == DELETE)
	{
	  delete_1_block (x, y);
	  board[x][y].blk = EMPTY;
	  board[x][y].sub = EMPTY;
	}
}


void  delete_1_block (x, y)
      int  x, y;
{
  XFillRectangle (XtDisplay (board_w), board_pix, delete_gc,
		  BLOCK_WIDTH * x - DIFF_X, BLOCK_WIDTH * y + DIFF_Y,
		  BLOCK_WIDTH, BLOCK_WIDTH);
  XFillRectangle (XtDisplay (board_w), XtWindow (board_w), delete_gc,
		  BLOCK_WIDTH * x - DIFF_X, BLOCK_WIDTH * y + DIFF_Y,
		  BLOCK_WIDTH, BLOCK_WIDTH);
}


void  startTimer ()
{
  int  reduce;

  if (blocks < 200)  reduce = (blocks / 20) * 70;
  else  if (blocks < 400) reduce = ((blocks - 200) / 20) * 50;
  else  if (blocks < 600) reduce = 500 + ((blocks - 400) / 20) * 25;
  else  if (blocks < 800) reduce = 200 + ((blocks - 600) / 20) * 50;
  else  if (blocks < 1000) reduce = 700 + ((blocks - 800) / 20) * 10;
  else  reduce = 860;

  if (star_comes)  reduce = 0;

  timer = XtAppAddTimeOut (XtWidgetToApplicationContext (board_w),
			   MAX_DELAY - reduce, DropItem, NULL);
}


void  stopTimer ()
{
  XtRemoveTimeOut (timer);
  timer = (XtIntervalId) NULL;
}


void  printScore ()
{
  char  buf[30];

  sprintf (buf, "%7ld", sc);
  XtVaSetValues (score_disp, XtNlabel, buf, NULL);
}


void  printLevel ()
{
  char  buf[30];

  sprintf (buf, "%7ld", blocks / 20 + 1);
  XtVaSetValues (level_disp, XtNlabel, buf, NULL);
}


void  Done (w, event, pars, npars)
      Widget  w;
      XEvent *event;
      String *pars;
      Cardinal *npars;
{
  XtPopdown (score_frame);
}


void  crack_1_block (x, y)
      int  x, y;
{
  if ((board[x][y].blk > 0) && (board[x][y].blk <= BLOCK_VARIETY))
    {
      XCopyArea (XtDisplay (board_w), block[board[x][y].blk + BLOCK_VARIETY],
		 board_pix, draw_gc, 0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
		 BLOCK_WIDTH * x - DIFF_X, BLOCK_HEIGHT * y + DIFF_Y);
      XCopyArea (XtDisplay (board_w), block[board[x][y].blk + BLOCK_VARIETY],
		 XtWindow (board_w), draw_gc, 0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
		 BLOCK_WIDTH * x - DIFF_X, BLOCK_HEIGHT * y + DIFF_Y);
    }
}


void  crushAnimate (x, y, num)
      int  x, y, num;
{
  XCopyArea (XtDisplay (board_w), crush[num], board_pix, draw_gc,
	     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
	     BLOCK_WIDTH * x - DIFF_X, BLOCK_HEIGHT * y + DIFF_Y);
  XCopyArea (XtDisplay (board_w), crush[num], XtWindow (board_w), draw_gc,
	     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
	     BLOCK_WIDTH * x - DIFF_X, BLOCK_HEIGHT * y + DIFF_Y);
}


static void  printStar (x, y)
      int  x, y;
{
  XCopyArea (XtDisplay (board_w), star, board_pix, draw_gc,
	     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
	     BLOCK_WIDTH * x - DIFF_X, BLOCK_HEIGHT * y + DIFF_Y);
  XCopyArea (XtDisplay (board_w), star, XtWindow (board_w), draw_gc,
	     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT, 
	     BLOCK_WIDTH * x - DIFF_X, BLOCK_HEIGHT * y + DIFF_Y);
}
