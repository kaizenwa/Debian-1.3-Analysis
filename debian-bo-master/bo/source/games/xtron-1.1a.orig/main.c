/* main.c - xtron v1.1  main source
 *
 *  Copyright (C) 1995 Rhett D. Jacobs <rhett@hotel.canberra.edu.au>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Last Modified: 16/4/95
 */

#include "main.h"

Display	*display;
int screen;
Window main_window;
GC gc;
unsigned long foreground;
unsigned long background;
int p_state = 1;

int LookAHEAD;

char KEYS_PLR1_UP, KEYS_PLR1_DOWN, KEYS_PLR1_LEFT, KEYS_PLR1_RIGHT;
char KEYS_PLR2_UP, KEYS_PLR2_DOWN, KEYS_PLR2_LEFT, KEYS_PLR2_RIGHT;

/* game windows */
Window game_port, plr1_win, plr2_win, pause_win, again_win, quit_win;
Window plr1_human, plr1_computer, plr2_human, plr2_computer;

/* set pixmap for interface */
Pixmap human_pic, computer_pic, again_pic, againr_pic; 
Pixmap pause_pic, pauser_pic, quit_pic, quitr_pic;
Pixmap human_rev, computer_rev, score_pic, scorer_pic;

/* player pieces bitmaps */
Pixmap p_im[3], board_im;


void ButtonEvent(XButtonEvent *pEvent)
{  
  char score1[30];

  if (pEvent->subwindow == plr1_win) {
    XCopyArea(display, scorer_pic, plr1_win, gc, 0, 0,
	      WBUTTON2, HBUTTON2, 0, 0);
    XCopyArea(display, score_pic, plr1_win, gc, 0, 0,
	      WBUTTON2, HBUTTON2, 0, 0);
    p[0].score = 0;
    sprintf(score1,"Player 1 - Score: %d",p[0].score);
    XDrawImageString(display, plr1_win, gc, 10, 20, score1, strlen(score1));
  }
  if (pEvent->subwindow == plr2_win) {
    XCopyArea(display, scorer_pic, plr2_win, gc, 0, 0,
	      WBUTTON2, HBUTTON2, 0, 0);
    XCopyArea(display, score_pic, plr2_win, gc, 0, 0,
	      WBUTTON2, HBUTTON2, 0, 0);
    p[1].score = 0;
    sprintf(score1,"Player 2 - Score: %d",p[1].score);
    XDrawImageString(display, plr2_win, gc, 10, 20, score1, strlen(score1));
  }
    
  /* player 1 options check */
  if (pEvent->subwindow == plr1_human)

    /* if in computer mode, switch to human */ 
    if (p[0].plr_type == computer) {
      p[0].plr_type = human;
      XCopyArea(display, human_rev, plr1_human, gc, 0, 0,
		 WBUTTON1, HBUTTON1, 0, 0);
      XCopyArea(display, computer_pic, plr1_computer, gc, 0, 0,
		 WBUTTON1, HBUTTON1, 0, 0);
    }

  if (pEvent->subwindow == plr1_computer)

    /* if in human mode, switch to computer */ 
    if (p[0].plr_type == human) {
      p[0].plr_type = computer;
      XCopyArea(display, human_pic, plr1_human, gc, 0, 0,
		 WBUTTON1, HBUTTON1, 0, 0);
      XCopyArea(display, computer_rev, plr1_computer, gc, 0, 0,
		 WBUTTON1, HBUTTON1, 0, 0);
    }

  /* player 2 options check */
  if (pEvent->subwindow == plr2_human)

    /* if in computer mode, switch to human */ 
    if (p[1].plr_type == computer) {
      p[1].plr_type = human;
      XCopyArea(display, human_rev, plr2_human, gc, 0, 0,
		 WBUTTON1, HBUTTON1, 0, 0);
      XCopyArea(display, computer_pic, plr2_computer, gc, 0, 0,
		 WBUTTON1, HBUTTON1, 0, 0);
    }

  if (pEvent->subwindow == plr2_computer)

    /* if in human mode, switch to computer */ 
    if (p[1].plr_type == human) {
      p[1].plr_type = computer;
      XCopyArea(display, human_pic, plr2_human, gc, 0, 0,
		 WBUTTON1, HBUTTON1, 0, 0);
      XCopyArea(display, computer_rev, plr2_computer, gc, 0, 0,
		 WBUTTON1, HBUTTON1, 0, 0);
    }

  /* play again */
  if(pEvent->subwindow == again_win) {
    XCopyArea(display, againr_pic, again_win, gc, 0, 0,
	      WBUTTON3, HBUTTON3, 0, 0);    
    restart_game();
    XCopyArea(display, again_pic, again_win, gc, 0, 0,
	      WBUTTON3, HBUTTON3, 0, 0);    

  }

  /* pause button */
  if(pEvent->subwindow == pause_win)
    if (p_state) {
      p_state = 0;
      XCopyArea(display, pauser_pic, pause_win, gc, 0, 0,
		  WBUTTON2, HBUTTON2, 0, 0);    
    } else {
      p_state = 1;
      XCopyArea(display, pause_pic, pause_win, gc, 0, 0,
		  WBUTTON2, HBUTTON2, 0, 0);    
    }

  /* quit */
  if (pEvent->subwindow == quit_win) {
    XCopyArea(display, quitr_pic, quit_win, gc, 0, 0,
	      WBUTTON2, HBUTTON2, 0, 0);    
    win_shutdown();
  }
}


void KeyEvent(XKeyEvent *pEvent)
{
  int key_buffer_size = 64;
  char key_buffer[65];
  XComposeStatus compose_status;
  KeySym key_sym;

  XLookupString(pEvent, key_buffer, key_buffer_size,
		&key_sym, &compose_status);
  
  if (p[0].plr_type == human) {
    if (key_sym == KEYS_PLR1_UP)
      plr_turn(0,up);
    if (key_sym == KEYS_PLR1_DOWN)
      plr_turn(0,down);
    if (key_sym == KEYS_PLR1_LEFT)
      plr_turn(0,left);
    if (key_sym == KEYS_PLR1_RIGHT)
      plr_turn(0,right);
  }
  if (p[1].plr_type == human) {
    if (key_sym == KEYS_PLR2_UP || key_sym == XK_Up)
      plr_turn(1,up);
    if (key_sym == KEYS_PLR2_DOWN || key_sym == XK_Down)
      plr_turn(1,down);
    if (key_sym == KEYS_PLR2_LEFT || key_sym == XK_Left)
      plr_turn(1,left);
    if (key_sym == KEYS_PLR2_RIGHT || key_sym == XK_Right)
      plr_turn(1,right);
  }
}


void ExposeEvent(XExposeEvent *pEvent)
{
  int i, ic, j, jc;
  char score1[28], score2[28];

  /* remove unused events etc... */
  XSync(display,1);

  /* background */
  for(i = 0; i < TOTAL_WIDTH; i+=WBOARD)
    for(j = 0; j < TOTAL_HEIGHT; j+=HBOARD)
      XCopyArea(display, board_im, game_port, gc, 0, 0,
		 WBOARD, HBOARD, i, j);
		
  /* refresh board */
  for (i = 0, ic = 0; i < TOTAL_WIDTH; i+=WPIECE, ic++)
    for(j = 0, jc = 0; j < TOTAL_WIDTH; j+=HPIECE, jc++)
      switch (b.contents[ic][jc]) {
      case 1:

	/* player 1 */
	XCopyArea(display, p_im[0], game_port, gc, 0, 0,
		   WPIECE, HPIECE, i, j);
	break;
      case 2:

 	/* player 2 */
	XCopyArea(display, p_im[1], game_port, gc, 0, 0,
		   WPIECE, HPIECE, i, j);
	break;
      }
  
  /* human and computer icons */
  /* check player setting to highlight appropriate window */
  if(p[0].plr_type == human) {
    XCopyArea(display, human_rev, plr1_human, gc, 0, 0,
	       WBUTTON1, HBUTTON1, 0, 0);
    XCopyArea(display, computer_pic, plr1_computer, gc, 0, 0,
	       WBUTTON1, HBUTTON1, 0, 0);
  } else {
    XCopyArea(display, human_pic, plr1_human, gc, 0, 0,
	       WBUTTON1, HBUTTON1, 0, 0);
    XCopyArea(display, computer_rev, plr1_computer, gc, 0, 0,
	       WBUTTON1, HBUTTON1, 0, 0);
  }
  
  /* check player setting to highlight appropriate window */
  if(p[1].plr_type == human) {
    XCopyArea(display, human_rev, plr2_human, gc, 0, 0,
	       WBUTTON1, HBUTTON1, 0, 0);
    XCopyArea(display, computer_pic, plr2_computer, gc, 0, 0,
	       WBUTTON1, HBUTTON1, 0, 0);
  } else {
    XCopyArea(display, human_pic, plr2_human, gc, 0, 0,
	       WBUTTON1, HBUTTON1, 0, 0);
    XCopyArea(display, computer_rev, plr2_computer, gc, 0, 0,
	       WBUTTON1, HBUTTON1, 0, 0);
  }
  
  /* again icon */
  XCopyArea(display, again_pic, again_win, gc, 0, 0,
	     WBUTTON3, HBUTTON3, 0, 0);
  
  /* pause icon */
  if (p_state)
    XCopyArea(display, pause_pic, pause_win, gc, 0, 0,
	      WBUTTON2, HBUTTON2, 0, 0);    
  else
    XCopyArea(display, pauser_pic, pause_win, gc, 0, 0,
	      WBUTTON2, HBUTTON2, 0, 0);    
  
  /* quit icon */
  XCopyArea(display, quit_pic, quit_win, gc, 0, 0,
	     WBUTTON2, HBUTTON2, 0, 0);  

  /* output current scores */
  XCopyArea(display, score_pic, plr1_win, gc, 0, 0,
	    WBUTTON2, HBUTTON2, 0, 0);
  XCopyArea(display, score_pic, plr2_win, gc, 0, 0,
	    WBUTTON2, HBUTTON2, 0, 0);

  sprintf(score1,"Player 1 - Score: %d",p[0].score);
  sprintf(score2,"Player 2 - Score: %d",p[1].score);

  XDrawImageString(display, plr1_win, gc, 10, 20, score1, strlen(score1));
  XDrawImageString(display, plr2_win, gc, 10, 20, score2, strlen(score2));
}


Window set_window(int x, int y, int width, int height)
{
  return(win_open(x, y, width, height, 1, main_window, 0, 0, 
		  NULL, NULL));
}


void mapwindows(void)
{
  XMapWindow(display,main_window);
  XMapWindow(display,game_port);
  XMapWindow(display,plr1_win);
  XMapWindow(display,plr2_win);
  XMapWindow(display,again_win);
  XMapWindow(display,pause_win);
  XMapWindow(display,quit_win);
  XMapWindow(display,plr1_human);
  XMapWindow(display,plr2_human);
  XMapWindow(display,plr1_computer);
  XMapWindow(display,plr2_computer);
}


Pixmap set_icon(char *filen)
{
  return(ReadXPM(filen));
}


int check_valid(int p_num, int x_inc, int y_inc)
{
  if (b.contents[p[p_num].co_ords[0]+x_inc][p[p_num].co_ords[1]+y_inc] != 0)
    return (0);
  
  if (y_inc != 0) {
    if ((p[p_num].co_ords[1]+y_inc) < MAXVERT &&
	(p[p_num].co_ords[1]+y_inc)  >= MINVERT)  
      return (1);
  } else {
    if (x_inc != 0)
      if ((p[p_num].co_ords[0]+x_inc) < MAXHORZ &&
	  (p[p_num].co_ords[0]+x_inc) >= MINHORZ)
	return (1);
  }
  return(0);
}


/* artificial intelligence routines for computer player */
void think(int p_num)
{
  enum directions sides[2];
  int flags[6] = {0,0,0,0,0,0};
  int index[2];
  int dis_forward,  dis_left, dis_right;
 
  dis_forward = dis_left = dis_right = 1;

  switch (p[p_num].plr_dir) {
  case left:

    /* forward flags */
    flags[0] = -1;
    flags[1] = 0;

    /* left flags */
    flags[2] = 0;
    flags[3] = 1;

    /* right flags */
    flags[4] = 0;
    flags[5] = -1;

    /* turns to either side */
    sides[0] = down;
    sides[1] = up;
    break;
  case right:
    flags[0] = 1;
    flags[1] = 0;
    flags[2] = 0;
    flags[3] = -1;
    flags[4] = 0;
    flags[5] = 1;
    sides[0] = up;
    sides[1] = down;
    break;
  case up:
    flags[0] = 0;
    flags[1] = -1;
    flags[2] = -1;
    flags[3] = 0;
    flags[4] = 1;
    flags[5] = 0;
    sides[0] = left;
    sides[1] = right;
    break;
  case down:
    flags[0] = 0;
    flags[1] = 1;
    flags[2] = 1;
    flags[3] = 0;
    flags[4] = -1;
    flags[5] = 0;
    sides[0] = right;
    sides[1] = left;
    break;
  }

  /* check forward */
  index[0] = p[p_num].co_ords[0]+flags[0];
  index[1] = p[p_num].co_ords[1]+flags[1];
  while (index[0] < MAXHORZ && index[0] >= MINHORZ &&
	 index[1] < MAXVERT && index[1] >= MINVERT &&
	 b.contents[index[0]][index[1]] == 0) {
    dis_forward++;
    index[0] += flags[0];
    index[1] += flags[1];
  }
 
  if (dis_forward < LookAHEAD) {
    dis_forward = 100 - 100/dis_forward;

    /* check left */
    index[0] = p[p_num].co_ords[0]+flags[2];
    index[1] = p[p_num].co_ords[1]+flags[3];
    while (index[0] < MAXHORZ && index[0] >= MINHORZ &&
	   index[1] < MAXVERT && index[1] >= MINVERT &&
	   b.contents[index[0]][index[1]] == 0) {
      dis_left++;
      index[0] += flags[2];
      index[1] += flags[3];
    } 
    
    /* check right */
    index[0] = p[p_num].co_ords[0]+flags[4];
    index[1] = p[p_num].co_ords[1]+flags[5];
    while (index[0] < MAXHORZ && index[0] >= MINHORZ &&
	   index[1] < MAXVERT && index[1] >= MINVERT &&
	   b.contents[index[0]][index[1]] == 0) {
      dis_right++;
      index[0] += flags[4];
      index[1] += flags[5];
    } 
    if(!(dis_left == 1 && dis_right == 1))
      if ((int)rand()%100 >= dis_forward || dis_forward == 0) {
       
	/* change direction */
	if ((int)rand()%100 <= (100*dis_left)/(dis_left+dis_right))
	  if (dis_left != 1)

	    /* turn to the left */
	    p[p_num].plr_dir = sides[0];
	  else

	    /* turn to the right */
	    p[p_num].plr_dir = sides[1];
	else
	  if (dis_right != 1)

	    /*  turn to the right */
	    p[p_num].plr_dir = sides[1];
	  else

	    /* turn to the left */
	    p[p_num].plr_dir = sides[0];
      }
  }
}


int game_update(void)
{
  int i, ic, j , jc;
  int x_inc = 0, y_inc = 0;

  for (i = 0; i< 2; i++) {
    if (p[i].plr_type == computer)
      think(i);
    switch (p[i].plr_dir) {
    case left:
      if (check_valid(i,-1,0))
	p[i].co_ords[0]--;
      else
	p[i].alive = 0;
      break;
    case right:
      if (check_valid(i,1,0))
	p[i].co_ords[0]++;
      else
	p[i].alive = 0;
      break;
    case up:
      if (check_valid(i,0,-1))
	p[i].co_ords[1]--;
      else
	p[i].alive = 0;
      break;
    case down:
      if (check_valid(i,0,1))
	p[i].co_ords[1]++;
      else
	p[i].alive = 0;
      break;
    }
    b.contents[p[i].co_ords[0]][p[i].co_ords[1]] = i+1;
  }
  XCopyArea(display, p_im[0], game_port, gc, 0, 0, WPIECE, HPIECE, 
	     p[0].co_ords[0]*WPIECE, p[0].co_ords[1]*HPIECE);  
  XCopyArea(display, p_im[1], game_port, gc, 0, 0, WPIECE, HPIECE, 
	     p[1].co_ords[0]*WPIECE, p[1].co_ords[1]*HPIECE);

  /* player collision check */
  if(!p[1].alive) { 
    switch(p[1].plr_dir) {
    case left:
      x_inc = -1; break;
    case right:
      x_inc = 1; break;
    case up:
      y_inc = -1; break;
    case down:
      y_inc = 1; break;
    }
    if ((p[1].co_ords[0]+x_inc) == p[0].co_ords[0])   
      if ((p[1].co_ords[1]+y_inc) == p[0].co_ords[1]) {
	for(i = 0, ic = 0; i < TOTAL_WIDTH; i+=WPIECE, ic++)
	  for(j = 0, jc = 0; j < TOTAL_WIDTH; j+=HPIECE, jc++)
	    if (b.contents[ic][jc])
	      XCopyArea(display, p_im[2], game_port, gc, 0, 0,
			 WPIECE, HPIECE, i, j);
	return(0);
      }
  }

  if(!p[0].alive && !p[1].alive) {
    for(i = 0, ic = 0; i < TOTAL_WIDTH; i+=WPIECE, ic++)
      for(j = 0, jc = 0; j < TOTAL_WIDTH; j+=HPIECE, jc++)
	if (b.contents[ic][jc])
	  XCopyArea(display, p_im[2], game_port, gc, 0, 0,
		     WPIECE, HPIECE, i, j);
    return(0);
  }
  
  if (!p[0].alive) {

    /* player 1 dead */
    p[1].score++;
    for(i = 0, ic = 0; i < TOTAL_WIDTH; i+=WPIECE, ic++)
      for(j = 0, jc = 0; j < TOTAL_WIDTH; j+=HPIECE, jc++)
	if (b.contents[ic][jc] == 1)
	  XCopyArea(display, p_im[1], game_port, gc, 0, 0,
		     WPIECE, HPIECE, i, j);
    return(0);
  }
  if (!p[1].alive) {

    /* player 2 dead */
    p[0].score++;
    for(i = 0, ic = 0; i < TOTAL_WIDTH; i+=WPIECE, ic++)
      for(j = 0, jc = 0; j < TOTAL_WIDTH; j+=HPIECE, jc++)
	if (b.contents[ic][jc] == 2)
	  XCopyArea(display, p_im[0], game_port, gc, 0, 0,
		     WPIECE, HPIECE, i, j);
    return(0);
  }		       
  return(1);
}


void restart_game(void)
{
  int i;
  XEvent event;

  p[0].plr_dir = left;
  p[1].plr_dir = right;
  for (i=0; i<2; i++) {
    p[i].alive = 1;
    p[i].co_ords[1] = MAXVERT/2;
  }
  p[0].co_ords[0] = (MAXHORZ/2)-3;
  p[1].co_ords[0] = (MAXHORZ/2)+3;
  brd_setup();
  ExposeEvent(&event.xexpose);
}


void open_windows(int argc, char **argv)
{
  main_window = win_open(0, 0, TOTAL_WIDTH, TOTAL_HEIGHT, 5,
 			 DefaultRootWindow(display), 1, argc, argv,
			 "xtron v1.1a - <rhett@hotel.canberra.edu.au>");
  game_port = set_window(0, 1, TOTAL_WIDTH, TOTAL_WIDTH);
  plr1_win = set_window(0, TOTAL_WIDTH, (TOTAL_WIDTH/2),
			((TOTAL_HEIGHT-TOTAL_WIDTH)/2));
  plr2_win = set_window((TOTAL_WIDTH/2), TOTAL_WIDTH,
			(TOTAL_WIDTH/2), ((TOTAL_HEIGHT-TOTAL_WIDTH)/2));
  again_win = set_window(0, (TOTAL_WIDTH+((TOTAL_HEIGHT-TOTAL_WIDTH)/2)),
			 TOTAL_WIDTH, (TOTAL_HEIGHT/16));
  pause_win = set_window(0, (TOTAL_HEIGHT-(TOTAL_HEIGHT/16)),
			 (TOTAL_WIDTH/2), (TOTAL_HEIGHT/16));
  quit_win = set_window((TOTAL_WIDTH/2),	
			(TOTAL_HEIGHT-(TOTAL_HEIGHT/16)),
			(TOTAL_WIDTH/2), (TOTAL_HEIGHT/16));
  plr1_human = set_window(0, (TOTAL_WIDTH+TOTAL_HEIGHT/16),
			  (TOTAL_WIDTH/4), (TOTAL_HEIGHT/16));
  plr1_computer = set_window((TOTAL_WIDTH/4), (TOTAL_WIDTH+TOTAL_HEIGHT/16),
			     (TOTAL_WIDTH/4), (TOTAL_HEIGHT/16));
  plr2_human = set_window((TOTAL_WIDTH/2), (TOTAL_WIDTH+TOTAL_HEIGHT/16),
			  (TOTAL_WIDTH/4), (TOTAL_HEIGHT/16));
  plr2_computer = set_window((TOTAL_WIDTH-TOTAL_WIDTH/4),
			     (TOTAL_WIDTH+TOTAL_HEIGHT/16),
			     (TOTAL_WIDTH/4),(TOTAL_HEIGHT/16));
}


void assign_bitmaps(void) 
{
  /* assign bitmaps used for user interface */
  human_pic = set_icon("human.xpm");
  computer_pic = set_icon("computer.xpm");
  human_rev = set_icon("humanr.xpm");
  computer_rev = set_icon("comprr.xpm");
  again_pic = set_icon("again.xpm");
  againr_pic = set_icon("againr.xpm");
  pause_pic = set_icon("pause.xpm");
  pauser_pic = set_icon("pauser.xpm");
  quit_pic = set_icon("quit.xpm");
  quitr_pic = set_icon("quitr.xpm");
  score_pic = set_icon("score.xpm");
  scorer_pic = set_icon("scorer.xpm");
  
  /* assign bitmaps used for player pieces */
  p_im[0] = set_icon("p1.xpm");
  p_im[1] = set_icon("p2.xpm");
  p_im[2] = set_icon("p3.xpm");
  board_im = set_icon("back.xpm");
}


void delay(int len)
{
  struct timeval tm;
  
  tm.tv_sec  = len/1000000;
  tm.tv_usec = len%1000000;
  select(0,0,0,0,&tm);
}


void assign_keys(void)
{
  KEYS_PLR1_UP = KeyRESOURCE(1,0);
  KEYS_PLR2_UP = KeyRESOURCE(2,0);
  KEYS_PLR1_DOWN = KeyRESOURCE(1,1);
  KEYS_PLR2_DOWN = KeyRESOURCE(2,1);
  KEYS_PLR1_LEFT = KeyRESOURCE(1,2);
  KEYS_PLR2_LEFT = KeyRESOURCE(2,2);
  KEYS_PLR1_RIGHT = KeyRESOURCE(1,3);
  KEYS_PLR2_RIGHT = KeyRESOURCE(2,3);

  LookAHEAD = LookAhead();
}


int main (int argc, char *argv[])    
{    
  int i = 0;
  XEvent event; 
  
  InitialiseResource();
  assign_keys();

  win_setup();
  plr_setup();
  brd_setup();
  srand(time(0));
 
   open_windows(argc, argv);
   gc = win_getGC();
   mapwindows();
   assign_bitmaps();
   
  XNextEvent(display, &event);
   if (event.type == Expose)
     ExposeEvent(&event.xexpose);

  /* game loop */
  for(;;) {
    delay(5000);
    if (p_state)
      if (i++ == 4) {
	if(!(game_update())) {
	  XFlush(display);
	  delay(1500000);
	  restart_game();
	}
	i = 0;
      }
    
    if (XCheckWindowEvent(display,main_window,
			  ExposureMask | KeyPressMask | ButtonPressMask |
			  VisibilityChangeMask,
			  &event))
      switch (event.type) {
      case Expose:
	ExposeEvent(&event.xexpose); break;
      case VisibilityNotify:
        ExposeEvent(&event.xexpose); break;
      case KeyPress:
	KeyEvent(&event.xkey); break;
      case ButtonPress:
	ButtonEvent(&event.xbutton); break;
      case MappingNotify:
	XRefreshKeyboardMapping(&event.xmapping); break;
      }
  }
}
