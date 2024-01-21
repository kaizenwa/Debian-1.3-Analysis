/***********************************************************
*  Mirror Magic II -- McDuffins Revenge                    *
*----------------------------------------------------------*
*  ©1994 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.ms.sub.org                  *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  events.c                                                *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#include "events.h"
#include "screens.h"
#include "tools.h"
#include "game.h"
#include "editor.h"

void EventLoop(void)
{
  while(1)
  {
    if (XPending(display))	/* got an event */
    {
      XEvent event;

      XNextEvent(display, &event);

      switch(event.type)
      {
	case Expose:
	  HandleExposeEvent((XExposeEvent *) &event);
	  break;
	case UnmapNotify:
	  SleepWhileUnmapped();
	  break;
	case ButtonPress:
	  button_status = ((XButtonEvent *) &event)->button;
	  HandleButtonEvent((XButtonEvent *) &event);
	  break;
	case ButtonRelease:
	  button_status = MB_RELEASED;
	  HandleButtonEvent((XButtonEvent *) &event);
	  break;
	case MotionNotify:
	  HandleMotionEvent((XMotionEvent *) &event);
	  break;
	case KeyPress:
	case KeyRelease:
	  HandleKeyEvent((XKeyEvent *) &event);
	  break;
	default:
	  break;
      }
    }
    else			/* got no event, but don't be lazy... */
    {
      HandleNoXEvent();

      if (game_status!=PLAYING)
	Delay(10000);		/* don't use all CPU time when idle */
    }

    if (game_status==EXITGAME)
      return;
  }
}

void ClearEventQueue()
{
  while(XPending(display))
  {
    XEvent event;

    XNextEvent(display, &event);

    switch(event.type)
    {
      case Expose:
        HandleExposeEvent((XExposeEvent *) &event);
	break;
      case UnmapNotify:
	SleepWhileUnmapped();
	break;
      case ButtonRelease:
	button_status = MB_RELEASED;
	break;
      default:
	break;
    }
  }
}

void SleepWhileUnmapped()
{
  BOOL window_unmapped = TRUE;

  while(window_unmapped)
  {
    XEvent event;

    XNextEvent(display, &event);

    switch(event.type)
    {
      case Expose:
        HandleExposeEvent((XExposeEvent *) &event);
	break;
      case ButtonRelease:
	button_status = MB_RELEASED;
	break;
      case MapNotify:
	window_unmapped = FALSE;
	break;
      default:
	break;
    }
  }
}

void HandleExposeEvent(XExposeEvent *event)
{
  int x=event->x, y=event->y;
  int w=event->width, h=event->height;

  XCopyArea(display,pix[PIX_DB_BACK],window,gc,x,y,w,h,x,y);
}

void HandleButtonEvent(XButtonEvent *event)
{
  HandleButton(event->x, event->y, button_status);
}

void HandleMotionEvent(XMotionEvent *event)
{
  if (game_status!=PLAYING)
    HandleButton(event->x, event->y, button_status);
}

void HandleButton(int mx, int my, int button)
{
  static int old_mx=0, old_my=0;
  int delay;

  if (mx<0 || my<0)
  {
    mx=old_mx;
    my=old_my;
  }
  else
  {
    old_mx=mx;
    old_my=my;
  }

  ColorCycling();

  switch(game_status)
  {
    case MAINMENU:
      HandleMainMenu(mx,my,0,0,button);
      break;
    case TYPENAME:
      HandleTypeName(0,XK_Return);
      break;
    case HALLOFFAME:
      HandleHallOfFame(button);
      break;
    case LEVELED:
      LevelEd(mx,my,button);
      break;
    case HELPSCREEN:
      HandleHelpScreen(button);
      break;
    case SETUP:
      HandleSetupScreen(mx,my,0,0,button);
      break;
    case PLAYING:
      delay=Counter();
      ClickElement(mx,my,button);
      if (WN)
	GameWon();
      else
      {
	switch(GameActions(mx,my,button))
	{
	  case ACT_GAME_OVER:
	    game_status=MAINMENU;
	    DrawMainMenu();
	    BackToFront();
	    break;
	  case ACT_NEW_GAME:
	    game_status=PLAYING;
	    InitGame();
	    break;
	  case ACT_GO_ON:
	    break;
	  default:
	    break;
	}
      }
      if (!button && !OL && delay<=Counter())
	WaitCounter(delay+5);
      break;
    default:
      break;
  }
}

void HandleKeyEvent(XKeyEvent *event)
{
  KeySym key = XLookupKeysym(event,event->state);

  if (key==XK_Escape && game_status!=MAINMENU)	/* quick quit to MAINMENU */
  {
    game_status=MAINMENU;
    XCopyArea(display,pix[PIX_BACK],drawto,gc,
	      DX,DY, 100,280, DX,DY);
    redraw_mask|=REDRAW_DOOR;
    DR=0;
    DrawMainMenu();
    BackToFront();
    return;
  }

  switch(game_status)
  {
    case TYPENAME:
      HandleTypeName(0,key);
      break;
    case MAINMENU:
    case SETUP:
    {
      int dx=0, dy=0;

      switch(key)
      {
	case XK_Return:
	  if (game_status==MAINMENU)
	    HandleMainMenu(0,0,0,0,MB_MENU_CHOICE);
	  else if (game_status==SETUP)
	    HandleSetupScreen(0,0,0,0,MB_MENU_CHOICE);
	  break;
	case XK_Left:
#ifdef XK_KP_Left
	case XK_KP_Left:
#endif
	case XK_KP_4:
	case XK_J:
	case XK_j:
	  dx=-1;
	  break;
	case XK_Right:
#ifdef XK_KP_Right
	case XK_KP_Right:
#endif
	case XK_KP_6:
	case XK_K:
	case XK_k:
	  dx=1;
	  break;
	case XK_Up:
#ifdef XK_KP_Up
	case XK_KP_Up:
#endif
	case XK_KP_8:
	case XK_I:
	case XK_i:
	  dy=-1;
	  break;
	case XK_Down:
#ifdef XK_KP_Down
	case XK_KP_Down:
#endif
	case XK_KP_2:
	case XK_M:
	case XK_m:
	  dy=1;
	  break;
	default:
	  break;
      }

      if (dx || dy)
      {
	if (game_status==MAINMENU)
	  HandleMainMenu(0,0,dx,dy,MB_MENU_MARK);
	else if (game_status==SETUP)
	  HandleSetupScreen(0,0,dx,dy,MB_MENU_MARK);
      }
      break;
    }
    case HALLOFFAME:
    case HELPSCREEN:
      switch(key)
      {
	case XK_Return:
	case XK_Escape:
	  game_status=MAINMENU;
	  DrawMainMenu();
	  BackToFront();
	  break;
	default:
	  break;
      }
      break;
    case PLAYING:
    default:
      break;
  }
}

void HandleNoXEvent()
{
  int delay;

  if (button_status)
  {
    HandleButton(-1,-1,button_status);
    return;
  }

  ColorCycling();

  switch(game_status)
  {
    case MAINMENU:
      HandleMainMenu(0,0,0,0,TRUE);
      break;
    case HALLOFFAME:
      HandleHallOfFame(TRUE);
      break;
    case HELPSCREEN:
      HandleHelpScreen(TRUE);
      break;
    case SETUP:
      HandleSetupScreen(0,0,0,0,TRUE);
      break;
    case PLAYING:
      delay=Counter();
      ClickElement(0,0,MB_NOT_PRESSED);
      if (WN)
	GameWon();
      else
      {
	switch(GameActions(0,0,MB_NOT_PRESSED))
	{
	  case ACT_GAME_OVER:
	    game_status=MAINMENU;
	    DrawMainMenu();
	    BackToFront();
	    break;
	  case ACT_NEW_GAME:
	    game_status=PLAYING;
	    InitGame();
	    break;
	  case ACT_GO_ON:
	    break;
	  default:
	    break;
	}
      }
      if (!OL && delay<=Counter())
	WaitCounter(delay+5);
      break;
    default:
      break;
  }
}
