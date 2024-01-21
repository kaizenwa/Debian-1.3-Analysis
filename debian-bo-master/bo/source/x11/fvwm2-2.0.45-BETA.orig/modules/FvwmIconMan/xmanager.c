#include "FvwmIconMan.h"
#include "readconfig.h"

#define GRAB_EVENTS (ButtonPressMask|ButtonReleaseMask|ButtonMotionMask|EnterWindowMask|LeaveWindowMask)

#define FONT_STRING "8x13"
#define DEFAULT_WIN_WIDTH  100
#define DEFAULT_WIN_HEIGHT 100

Display *theDisplay;
static Window theRoot;
static int theDepth, theScreen;

static enum {
  NOT_GRABBED = 0,
  NEED_TO_GRAB = 1,
  HAVE_GRABBED = 2
} grab_state = NOT_GRABBED;

typedef struct {
  int button_x, button_y, button_h, button_w; /* dim's of the whole button */
  int icon_x, icon_y, icon_h, icon_w;         /* what denotes icon state */
  int text_x, text_y, text_h, text_w;         /* text field */
  int text_base;                              /* text baseline */
} ButtonGeometry;

WinData *find_win (WinManager *man, int box);

static void ClipRectangle (WinManager *man, int focus,
			   int x, int y, int w, int h)
{
  XRectangle r;

  r.x = x;
  r.y = y;
  r.width = w;
  r.height = h;
  XSetClipRectangles(theDisplay, man->hiContext[focus], 0, 0, &r, 1, 
		     YXBanded);
}

void init_boxes (void)
{
}

static void grab_pointer (WinManager *man)
{
  /* This should only be called after we get our EXPOSE event */
  if (grab_state == NEED_TO_GRAB) {
    if (XGrabPointer (theDisplay, man->theWindow, True, GRAB_EVENTS,
		      GrabModeAsync, GrabModeAsync, None, 
		      None, CurrentTime) != GrabSuccess) {
      ConsoleMessage ("Couldn't grab pointer\n");
      ShutMeDown (0);
    }
    grab_state = HAVE_GRABBED;
  }
}

static void map_manager (WinManager *man)
{
  if (man->window_mapped == 0 && man->win_height > 0) {
    XMapWindow (theDisplay, man->theWindow);
    man->window_mapped = 1;
    XFlush (theDisplay);
    if (globals.transient) {
      /* wait for an expose event to actually do the grab */
      grab_state = NEED_TO_GRAB;
    }
  }
}

static void unmap_manager (WinManager *man)
{
  if (man->window_mapped == 1) {
    XUnmapWindow (theDisplay, man->theWindow);
    man->window_mapped = 0;
    XFlush (theDisplay);
  }
}

static void resize_manager (WinManager *man, int w, int h)
{
  XSizeHints size;
  long mask, oldwidth, oldheight;

  if (man->win_height == h)
    return;

  XGetWMNormalHints (theDisplay, man->theWindow, &size, &mask);
  
  oldwidth = man->win_width;
  oldheight = man->win_height;

  man->win_width = w;
  man->win_height = h;

  size.min_width = 0;
  size.max_width = 1000;
  size.min_height = h;
  size.max_height = h;
  XSetWMNormalHints (theDisplay, man->theWindow, &size);

  if (man->grow_direction == SouthGravity) {
    ConsoleDebug (">>>>>>>>>>>>>>><<<<<<<<<<<<<<<<\n");
    ConsoleDebug ("moving from: %d %d %d %d (%d). delta y: %d\n", 
		  man->win_x, man->win_y, oldwidth,
		  oldheight, man->win_y + oldheight, oldheight - h);
    man->win_y -= h - oldheight;

    ConsoleDebug ("moving to: %d %d %d %d (%d)\n", man->win_x, man->win_y,
		  man->win_width, man->win_height, 
		  man->win_y + man->win_height);
    XMoveResizeWindow (theDisplay, man->theWindow, 
		       man->win_x + man->win_border, 
		       man->win_y + man->win_border + man->win_title, 
		       man->win_width, man->win_height);
  }  
  else {
    XResizeWindow (theDisplay, man->theWindow, 
		   man->win_width, man->win_height);
  }
}

static int lookup_color (char *name, Pixel *ans)
{
  XColor color;
  XWindowAttributes attributes;

  XGetWindowAttributes(theDisplay, theRoot, &attributes);
  color.pixel = 0;
  if (!XParseColor (theDisplay, attributes.colormap, name, &color)) {
    ConsoleDebug("Could not parse color '%s'\n", name);
    return 0;
  }
  else if(!XAllocColor (theDisplay, attributes.colormap, &color)) {
    ConsoleDebug("Could not allocate color '%s'\n", name);
    return 0;
  }
  *ans = color.pixel;
  return 1;
}

static int lookup_hilite_color (Pixel background, Pixel *ans)
{
  XColor bg_color, white_p;
  XWindowAttributes attributes;

  XGetWindowAttributes(theDisplay, theRoot, &attributes);
  
  bg_color.pixel = background;
  XQueryColor(theDisplay, attributes.colormap, &bg_color);

  if (lookup_color ("white", &white_p.pixel) == 0)
    return 0;
  XQueryColor(theDisplay, attributes.colormap, &white_p);
  
  bg_color.red = MAX((white_p.red/5), bg_color.red);
  bg_color.green = MAX((white_p.green/5), bg_color.green);
  bg_color.blue = MAX((white_p.blue/5), bg_color.blue);
  
  bg_color.red = MIN(white_p.red, (bg_color.red*140)/100);
  bg_color.green = MIN(white_p.green, (bg_color.green*140)/100);
  bg_color.blue = MIN(white_p.blue, (bg_color.blue*140)/100);
  
  if(!XAllocColor(theDisplay, attributes.colormap, &bg_color))
    return 0;

  *ans = bg_color.pixel;
  return 1;
}

static int lookup_shadow_color (Pixel background, Pixel *ans)
{
  XColor bg_color;
  XWindowAttributes attributes;
  
  XGetWindowAttributes(theDisplay, theRoot, &attributes);
  
  bg_color.pixel = background;
  XQueryColor(theDisplay, attributes.colormap, &bg_color);
  
  bg_color.red = (unsigned short)((bg_color.red*50)/100);
  bg_color.green = (unsigned short)((bg_color.green*50)/100);
  bg_color.blue = (unsigned short)((bg_color.blue*50)/100);
  
  if(!XAllocColor(theDisplay, attributes.colormap, &bg_color))
    return 0;
  
  *ans = bg_color.pixel;
  return 1;
}

static void get_button_geometry (WinManager *man, WinData *win, int box,
				 ButtonGeometry *g)
{
  assert (man);

  g->button_x = 0;
  g->button_y = box * man->boxheight;
  g->button_w = man->win_width;
  g->button_h = man->boxheight;

  if (theDepth > 2) {
#ifdef MINI_ICONS
    if (man->draw_icons && win && win->pic.picture) {
      /* If no window, then icon_* aren't used, so doesn't matter what 
	 they are */
      g->icon_x = 4;
      g->icon_y = box * man->boxheight + 2;
      g->icon_w = MIN (win->pic.width, man->boxheight);
      g->icon_h = MIN (man->boxheight - 4, win->pic.height);
    }
    else {
#else
    if (1) {
#endif
      g->icon_x = 4;
      g->icon_y = box * man->boxheight + 4;
      g->icon_h = man->boxheight - 8;
      g->icon_w = g->icon_h;
    }
  }
  else {
    g->icon_x = 4;
    g->icon_y = box * man->boxheight + 4;
    g->icon_h = man->boxheight - 8;
    g->icon_w = g->icon_h;
  }
  
  g->text_x = g->icon_x + g->icon_w + 2;
  g->text_y = box * man->boxheight + 2;
  g->text_base = (box + 1) * man->boxheight - 4;
  g->text_w = man->win_width - 4 - g->text_x;
  g->text_h = man->fontheight;
}

static void draw_3d_square (WinManager *man, int x, int y, int w, int h,
			    GC rgc, GC sgc)
{
  int i;
  XSegment seg[4];

  i=0;
  seg[i].x1 = x;        seg[i].y1   = y;
  seg[i].x2 = w+x-1;    seg[i++].y2 = y;

  seg[i].x1 = x;        seg[i].y1   = y;
  seg[i].x2 = x;        seg[i++].y2 = h+y-1;

  seg[i].x1 = x+1;      seg[i].y1   = y+1;
  seg[i].x2 = x+w-2;    seg[i++].y2 = y+1;

  seg[i].x1 = x+1;      seg[i].y1   = y+1;
  seg[i].x2 = x+1;      seg[i++].y2 = y+h-2;
  XDrawSegments(theDisplay, man->theWindow, rgc, seg, i);

  i=0;
  seg[i].x1 = x;        seg[i].y1   = y+h-1;
  seg[i].x2 = w+x-1;    seg[i++].y2 = y+h-1;

  seg[i].x1 = x+w-1;    seg[i].y1   = y;
  seg[i].x2 = x+w-1;    seg[i++].y2 = y+h-1;
  XDrawSegments(theDisplay, man->theWindow, sgc, seg, i);

  i=0;
  seg[i].x1 = x+1;      seg[i].y1   = y+h-2;
  seg[i].x2 = x+w-2;    seg[i++].y2 = y+h-2;

  seg[i].x1 = x+w-2;    seg[i].y1   = y+1;
  seg[i].x2 = x+w-2;    seg[i++].y2 = y+h-2;

  XDrawSegments(theDisplay, man->theWindow, sgc, seg, i);
  XFlush (theDisplay);
}  

static void draw_3d_icon (WinManager *man, int box, ButtonGeometry *g,
			  int iconified, int dir, Contexts contextId)
{
  if (iconified == 0) {
    draw_3d_square (man, g->icon_x, g->icon_y, g->icon_w, g->icon_h, 
		    man->flatContext[contextId], 
		    man->flatContext[contextId]);
  }
  else {
    if (dir == 1) {
      draw_3d_square (man, g->icon_x, g->icon_y, g->icon_w, g->icon_h, 
		      man->reliefContext[contextId], 
		      man->shadowContext[contextId]);
    }
    else {
      draw_3d_square (man, g->icon_x, g->icon_y, g->icon_w, g->icon_h, 
		      man->shadowContext[contextId],
		      man->reliefContext[contextId]);
    }
  }
}
  

  /* this routine should only be called from draw_button() */
static void iconify_box (WinManager *man, WinData *win, int box, 
			 ButtonGeometry *g, int iconified,
                         Contexts contextId)
{
  int focus;
#ifdef MINI_ICONS
  XGCValues gcv;
  unsigned long gcm;
#endif

  focus = (box == man->focus_box);
  
  if (!man->window_up)
    return;
  
  if (theDepth > 2) {
#ifdef MINI_ICONS
    if (man->draw_icons && win->pic.picture) {
      if (iconified == 1) {
	gcm = GCClipMask|GCClipXOrigin|GCClipYOrigin;
	gcv.clip_mask = win->pic.mask;
	gcv.clip_x_origin = g->icon_x;
	gcv.clip_y_origin = g->icon_y;
	XChangeGC (theDisplay, man->hiContext[contextId], gcm, &gcv);
      
	XCopyArea(theDisplay, win->pic.picture, man->theWindow, 
		  man->hiContext[contextId], 0, 0, g->icon_w, g->icon_h, 
		  g->icon_x, g->icon_y);
	gcm = GCClipMask;
	gcv.clip_mask = None;
	XChangeGC(theDisplay, man->hiContext[contextId], gcm, &gcv);
      }
    } 
    else {
#endif
      draw_3d_icon (man, box, g, iconified, 1, contextId);
#ifdef MINI_ICONS
    }
#endif
  }
  else {
    if (iconified == 0) {
      XFillArc (theDisplay, man->theWindow, man->backContext[contextId], 
		g->icon_x, g->icon_y, g->icon_w, g->icon_h, 0, 360 * 64);
    }
    else {
      XFillArc (theDisplay, man->theWindow, man->hiContext[contextId], 
		g->icon_x, g->icon_y, g->icon_w, g->icon_h, 0, 360 * 64);
    }
    XFlush (theDisplay);
  }
}

void draw_button( WinManager *man, WinData *win, int button )
{
  GC context1, context2;
  ButtonState state;
  int focus = (button == man->focus_box && man->followFocus);
  int selected = (button == man->select_box);
  int draw_name;
  Contexts contextId;
  int len;
  char *name;
  ButtonGeometry g;

  assert (man);
  ConsoleDebug ("DrawButton: %d\n", button);

  if ( !man->window_up )
     return;

  if ( ! win )
     win = find_win( man, button );

  /* Important: we can be in here without a win. */
     
  draw_name = (win) ? 1 : 0;

  if (selected && focus)
    contextId = FOCUS_SELECT_CONTEXT;
  else if (selected)
    contextId = SELECT_CONTEXT;
  else if (focus)
    contextId = FOCUS_CONTEXT;
  else
    contextId = PLAIN_CONTEXT;

  state = man->buttonState[contextId];

  get_button_geometry (man, win, button, &g);

     /* draw the background */
  XFillRectangle (theDisplay, man->theWindow, man->backContext[contextId], 
		  g.button_x, g.button_y, g.button_w, g.button_h);
  
  if ( theDepth > 2 ) { 
    switch ( state )
    { 
    case BUTTON_FLAT:
      context1 = man->flatContext[contextId];
      context2 = man->flatContext[contextId];
      break;
	
    case BUTTON_UP:
      context1 = man->reliefContext[contextId];
      context2 = man->shadowContext[contextId];
      break;
	
    case BUTTON_DOWN:
      context1 = man->shadowContext[contextId];
      context2 = man->reliefContext[contextId];
      break;

    default:
      ConsoleMessage ("Internal error in draw_button\n");
      break;
    }
    draw_3d_square (man, g.button_x, g.button_y, g.button_w, g.button_h,
		    context1, context2);
  }
  else {
    if (selected)
      XDrawRectangle (theDisplay, man->theWindow, man->hiContext[contextId], 
		      g.button_x + 2, g.button_y + 1, 
		      g.button_w - 4, g.button_h - 2);
  }

  if (draw_name) {
    name = win->display_string;
    assert (name);
    len = strlen (name);
    
    ClipRectangle (man, focus, g.text_x, g.text_y, g.text_w, g.text_h);
    
    XDrawString (theDisplay, man->theWindow, man->hiContext[contextId], 
		 g.text_x, g.text_base, name, len);
    
    XSetClipMask (theDisplay, man->hiContext[contextId], None);
  }

  if (win) 
    iconify_box (man, win, button, &g, win->iconified, contextId);
}


/* If man == NULL || box < 0, then there will be no selected button */

void move_highlight (WinManager *man, int box)
{
  WinData *new_win;
  WinManager *select_man;
  int select_box;

  ConsoleDebug ("move_highlight: 0x%x, %d\n", man, box);

  if (man && box >= 0) {
    new_win = find_win (man, box);
    if (new_win == NULL) {
      ConsoleMessage ("Internal error in move_highlight\n");
      return;
    }
    if (new_win == globals.select_win) {
      return;
    }
    if (globals.select_win) {
      select_man = globals.select_win->manager;
      select_box = select_man->select_box;
      select_man->select_box = -1;
      draw_button (select_man, NULL, select_box);
    }
    globals.select_win = new_win;
    globals.select_win->manager->select_box = box;
    draw_button (globals.select_win->manager, NULL, box);
  }
  else {
    if (globals.select_win) {
      select_man = globals.select_win->manager;
      select_box = select_man->select_box;
      select_man->select_box = -1;
      globals.select_win = NULL;
      draw_button (select_man, NULL, select_box);
    }
  }
}
  

void draw_window (WinManager *man)
{
  WinData *p;
  int i, focus;

  if (!man || !man->window_up) {
    return;
  }
  ConsoleDebug ("draw_window\n");

  XFillRectangle (theDisplay, man->theWindow, man->backContext[PLAIN_CONTEXT], 
		  0, 0, man->win_width, man->win_height);

  for (i = 0, p = man->icon_list.head; 
       i < man->icon_list.n; 
       i++, p = p->icon_next) {
    focus = (i == man->focus_box);
    draw_button (man, p, i );
  }

  XFlush (theDisplay);
}

void draw_added_icon (WinManager *man)
{
  if (!man || !man->window_up)
    return;

  ConsoleDebug ("Draw added icon: %d\n", man->icon_list.n);
  resize_manager (man, man->win_width, man->boxheight * man->icon_list.n);
  if (man->icon_list.n == 1)
    map_manager (man);
  draw_window (man);
  XFlush (theDisplay);
}

void draw_deleted_icon (WinManager *man)
{
  if (!man || !man->window_up)
    return;

  ConsoleDebug ("In draw_deleted_icon\n");

  ConsoleDebug ("Draw deleted icon: %d\n", man->icon_list.n);
  if (man->icon_list.n == 0) {
    unmap_manager (man); 
  }
  else {
    resize_manager (man, man->win_width, man->boxheight * man->icon_list.n);
  }
  draw_window (man);
  XFlush (theDisplay);
  ConsoleDebug ("Exiting draw_deleted_icon\n");
}

void update_window_stuff (WinManager *man)
{
  int height;

  if (!man)
    return;
  
  height = man->boxheight * man->icon_list.n;
  ConsoleDebug ("update_window_stuff: %d\n", man->icon_list.n);

  if (height != man->win_height || (height && man->window_mapped == 0)) {
    ConsoleDebug ("update: have to do some work\n");
    if (height == 0) {
      unmap_manager (man); 
    }
    else {
      map_manager (man);
      resize_manager (man, man->win_width, height);
    }
  }
  draw_window (man);
}

static int which_box (WinManager *man, int x, int y)
{
  int temp = y / man->boxheight;

  if (x >= 0 && x <= man->win_width && temp >= 0 && temp < man->icon_list.n)
    return temp;
  else
    return -1;
}

WinData *find_win (WinManager *man, int box)
{
  WinData *p;
  int i;
  for (i = 0, p = man->icon_list.head; i < box && p; 
       i++, p = p->icon_next);
  if (i != box)
    return NULL;
  else
    return p;
}

int win_to_box (WinManager *man, WinData *win) 
{
  WinData *p;
  int i;

  for (i = 0, p = man->icon_list.head; p && p != win; 
       i++, p = p->icon_next);
  if (p)
    return i;
  else
    return -1;
}

WinManager *find_windows_manager (Window win)
{
  int i;

  for (i = 0; i < globals.num_managers; i++) {
    if (globals.managers[i].theWindow == win)
      return &globals.managers[i];
  }
  
/*  ConsoleMessage ("error in find_windows_manager:\n");
  ConsoleMessage ("Window: %x\n", win);
  for (i = 0; i < globals.num_managers; i++) {
    ConsoleMessage ("manager: %d %x\n", i, globals.managers[i].theWindow);
  }
*/
  return NULL;
}

static void handle_buttonevent (XEvent *theEvent, WinManager *man)
{
  int k;
  WinData *win;
  unsigned int modifier;
  Binding *MouseEntry;

  k = which_box (man, theEvent->xbutton.x, theEvent->xbutton.y);
  ConsoleDebug ("Got a Button event %d in box: %d\n", 
		theEvent->xbutton.button, k);
  if (k >= 0 && theEvent->xbutton.button >= 1 && 
      theEvent->xbutton.button <= 3) {
    win = find_win (man, k);
    if (win != NULL) {
      ConsoleDebug ("Found the window:\n");
      ConsoleDebug ("\tid:        %d\n", win->app_id);
      ConsoleDebug ("\tdesknum:   %d\n", win->desknum);
      ConsoleDebug ("\tx, y:      %d %d\n", win->x, win->y);
      ConsoleDebug ("\ticon:      %d\n", win->iconname);
      ConsoleDebug ("\ticonified: %d\n", win->iconified);
      ConsoleDebug ("\tcomplete:  %d\n", win->complete);

      modifier = (theEvent->xbutton.state & MODS_USED);
      /* need to search for an appropriate mouse binding */
      for (MouseEntry = man->bindings[MOUSE]; MouseEntry != NULL;
	   MouseEntry= MouseEntry->NextBinding) {
	if(((MouseEntry->Button_Key == theEvent->xbutton.button)||
	    (MouseEntry->Button_Key == 0))&&
	   ((MouseEntry->Modifier == AnyModifier)||
	    (MouseEntry->Modifier == (modifier& (~LockMask)))))
	{
	  Function *ftype = MouseEntry->Function;
	  if (ftype && ftype->func) 
	    run_function_list (ftype);
	  break;
	}
      }
    }
  }
}

void xevent_loop (void)
{
  XEvent theEvent;
  int k, glob_x, glob_y, x, y, mask;
  unsigned int modifier;
  Binding *key;
  static int flag = 0;
  WinManager *man;
  Window root, child;

  if (flag == 0) {
    flag = 1;
    ConsoleDebug ("A virgin event loop\n");
  }
  while (XPending (theDisplay)) {
    XNextEvent (theDisplay, &theEvent);
    ConsoleDebug ("theEvent.type: %d\n", theEvent.type);
    if (theEvent.type == MappingNotify) {
      ConsoleDebug ("XEVENT: MappingNotify\n");
      continue;
    }

    man = find_windows_manager (theEvent.xany.window);
    if (!man) {
      ConsoleDebug ("Event doesn't belong to a manager\n");
      continue;
    }

    switch (theEvent.type) {
#if 0
    case ReparentNotify:
      ConsoleDebug ("XEVENT: ReparentNotify\n");
      man->theParent = theEvent.xreparent.parent;
      XQueryTree (theDisplay, man->theParent, &junkroot, &man->theFrame, 
		  &junkwinlist, &junknumchildren);
      if (junkwinlist)
	XFree (junkwinlist);
      XGetWindowAttributes (theDisplay, man->theWindow, &winattr);
      XGetWindowAttributes (theDisplay, man->theParent, &parentattr);
      man->off_x = winattr.x + winattr.border_width + parentattr.x +
	parentattr.border_width + 1;
      man->off_y = winattr.y + winattr.border_width + parentattr.y +
	parentattr.border_width + 1;
      ConsoleDebug ("ReparentNotify: %d %d\n", man->off_x, man->off_y);
      break;
#endif

    case KeyPress:
      ConsoleDebug ("XEVENT: KeyPress\n");
      /* Here's a real hack - some systems have two keys with the
       * same keysym and different keycodes. This converts all
       * the cases to one keycode. */
      theEvent.xkey.keycode = 
	XKeysymToKeycode (theDisplay,
			  XKeycodeToKeysym(theDisplay,
					   theEvent.xkey.keycode,0));
      modifier = (theEvent.xkey.state & MODS_USED);
      ConsoleDebug ("KeyPress: %d\n", theEvent.xkey.keycode);
      
      for (key = man->bindings[KEYPRESS]; key != NULL; key = key->NextBinding)
      {
	if ((key->Button_Key == theEvent.xkey.keycode) &&
	    ((key->Modifier == (modifier&(~LockMask)))||
	     (key->Modifier == AnyModifier)))
	{
	  Function *ftype = key->Function;
	  if (ftype && ftype->func)
	    run_function_list (ftype);
	  break;
	}
      }
    break;

    case Expose:
      ConsoleDebug ("XEVENT: Expose\n");
      if (theEvent.xexpose.count == 0) {
	draw_window (man);
	if (globals.transient) {
	  grab_pointer (man);
	}
      }
      break;

    case ButtonPress:
      ConsoleDebug ("XEVENT: ButtonPress\n");
      if (!globals.transient)
	handle_buttonevent (&theEvent, man);
      break;

    case ButtonRelease:
      ConsoleDebug ("XEVENT: ButtonRelease\n");
      if (globals.transient) {
	handle_buttonevent (&theEvent, man);
	ShutMeDown (0);
      }
      break;

    case EnterNotify:
      ConsoleDebug ("XEVENT: EnterNotify\n");
      man->cursor_in_window = 1;
      k = which_box (man, theEvent.xcrossing.x, theEvent.xcrossing.y);
      move_highlight (man, k);
      run_binding (man, SELECT);
      break;

    case LeaveNotify:
      ConsoleDebug ("XEVENT: LeaveNotify\n");
      move_highlight (NULL, -1);
      break;

    case ConfigureNotify:
      ConsoleDebug ("XEVENT: Configure Notify: %d %d %d %d\n",
		    theEvent.xconfigure.x, theEvent.xconfigure.y,
		    theEvent.xconfigure.width, theEvent.xconfigure.height);
      if (theEvent.xconfigure.send_event) {
	if (man->win_width != theEvent.xconfigure.width && 
	    man->win_height == theEvent.xconfigure.height) {
	  /* If the height is different, ignore. It's just junk */
	  man->win_width = theEvent.xconfigure.width;
	  man->win_height = theEvent.xconfigure.height;
	  update_window_stuff (man);
	}
	/*
	  man->win_x = theEvent.xconfigure.x;
	  man->win_y = theEvent.xconfigure.y;
	  */
      }
      /* pointer may not be in the same box as before */
      if (XQueryPointer (theDisplay, man->theWindow, &root, &child, &glob_x, 
			 &glob_y,
			 &x, &y, &mask)) {
	k = which_box (man, x, y);
	ConsoleDebug (">>>>> Query: %d %d = %d\n", x, y, k);
	if (k != man->select_box) {
	  move_highlight (man, k);
	  run_binding (man, SELECT);
	}
      }
      else {
	if (man->select_box != -1)
	  move_highlight (NULL, -1);
      }
      break;

    case MotionNotify:
   /* ConsoleDebug ("XEVENT: MotionNotify\n"); */
      k = which_box (man, theEvent.xmotion.x, theEvent.xmotion.y);
      if (k != man->select_box) {
	move_highlight (man, k);
	run_binding (man, SELECT);
      }
      break;

    default:
      ConsoleDebug ("XEVENT: unknown\n");
    }
  }
  XFlush (theDisplay);
}

static void set_window_properties (Window win, char *name, char *icon, 
				   XSizeHints *sizehints)
{
  XTextProperty win_name;
  XTextProperty win_icon;
  XClassHint class;
  XWMHints wmhints;

  wmhints.initial_state = NormalState;
  wmhints.flags = StateHint;

  if (XStringListToTextProperty (&name, 1, &win_name) == 0) {
    ConsoleMessage ("%s: cannot allocate window name.\n",Module);
    return;
  }
  if (XStringListToTextProperty (&icon, 1, &win_icon) == 0) {
    ConsoleMessage ("%s: cannot allocate window icon.\n",Module);
    return;
  }

  class.res_name = Module + 1;
  class.res_class = "FvwmModule";
  

  XSetWMProperties (theDisplay, win, &win_name, &win_icon, NULL, 0,
		    sizehints, &wmhints, &class);

  XFree (win_name.value);
  XFree (win_icon.value);
}

static int load_default_context_fore (WinManager *man, int i)
{
  int j = 0;

  if (theDepth > 2)
    j = 1;

  ConsoleDebug ("Loading: %s\n", contextDefaults[i].backcolor[j]);

  return lookup_color (contextDefaults[i].forecolor[j], &man->forecolor[i]);
}

static int load_default_context_back (WinManager *man, int i) 
{
  int j = 0;

  if (theDepth > 2)
    j = 1;

  ConsoleDebug ("Loading: %s\n", contextDefaults[i].backcolor[j]);

  return lookup_color (contextDefaults[i].backcolor[j], &man->backcolor[i]);
}

void init_window (int man_id)
{
  XSizeHints sizehints;
  XGCValues gcval;
  unsigned long gcmask = 0;
  unsigned long winattrmask = CWBackPixel| CWBorderPixel | CWEventMask |
    CWBackingStore;
  XSetWindowAttributes winattr;
  unsigned int line_width = 1;
  int line_style = LineSolid;
  int cap_style = CapRound;
  int join_style = JoinRound;
  int i, val, x, y;
  unsigned int width, height;
  WinManager *man;

  ConsoleDebug ("In init_window\n");

  man = &globals.managers[man_id];
  
  if (man->window_up)
    return;

  man->win_height = DEFAULT_WIN_HEIGHT;
  man->win_width = DEFAULT_WIN_WIDTH;
  man->select_box = -1;
  man->cursor_in_window = 0;

  if (man->fontname) {
    man->ButtonFont = XLoadQueryFont (theDisplay, man->fontname);
    if (!man->ButtonFont) {
      if (!(man->ButtonFont = XLoadQueryFont (theDisplay, FONT_STRING))) {
	ConsoleMessage ("Can't get font\n");
	ShutMeDown (1);
      }
    }
  }
  else {
    if (!(man->ButtonFont = XLoadQueryFont (theDisplay, FONT_STRING))) {
      ConsoleMessage ("Can't get font\n");
      ShutMeDown (1);
    }
  }

  for ( i = 0; i < NUM_CONTEXTS; i++ ) {
    if (man->backColorName[i]) {
      if (!lookup_color (man->backColorName[i], &man->backcolor[i])) {
        if (!load_default_context_back (man, i)) {
	  ConsoleMessage ("Can't load %s background color\n", 
			  contextDefaults[i].name);
        }
      }
    }
    else if (!load_default_context_back (man, i)) {
      ConsoleMessage ("Can't load %s background color\n", 
		      contextDefaults[i].name);
    }
  
    if (man->foreColorName[i]) {
      if (!lookup_color (man->foreColorName[i], &man->forecolor[i])) {
        if (!load_default_context_fore (man, i)) {
    	ConsoleMessage ("Can't load %s foreground color\n", 
			contextDefaults[i].name);
        }
      }
    }
    else if (!load_default_context_fore (man, i)) {
      ConsoleMessage ("Can't load %s foreground color\n", 
		      contextDefaults[i].name);
    }
  
    if (theDepth > 2) {
      if (!lookup_shadow_color (man->backcolor[i], &man->shadowcolor[i])) {
	ConsoleMessage ("Can't load %s shadow color\n", 
			contextDefaults[i].name);
      }
      if (!lookup_hilite_color (man->backcolor[i], &man->hicolor[i])) {
	ConsoleMessage ("Can't load %s hilite color\n", 
			contextDefaults[i].name);
      }
    }
  }

  man->fontheight = man->ButtonFont->ascent + 
    man->ButtonFont->descent;
  man->boxheight = man->fontheight + 4;

  /* silly hack to guess the minimum char width of the font 
     doesn't have to be perfect. */

  man->fontwidth = XTextWidth (man->ButtonFont, ".", 1);

  man->win_height = man->boxheight * man->icon_list.n;
  if (man->win_height == 0) {
    man->win_height = 1;
  }

  sizehints.width = man->win_width;
  sizehints.height = man->win_height;
  sizehints.min_width = 0;
  sizehints.max_width = 1000;
  sizehints.min_height = man->win_height;
  sizehints.max_height = man->win_height;
  sizehints.win_gravity = NorthWestGravity;
  sizehints.flags = PSize | PMinSize | PMaxSize | PWinGravity;
  sizehints.x = 0;
  sizehints.y = 0;

  if (man->geometry) {
    int gravity;
    val = XWMGeometry (theDisplay, theScreen, man->geometry, "+0+0", 1,
		      &sizehints, &x, &y, &width, &height, &gravity);
    ConsoleDebug ("x, y, w, h = %d %d %d %d\n", x, y, width, height);
    sizehints.x = x;
    sizehints.y = y;
    sizehints.width = width;
    sizehints.height = height;
    sizehints.win_gravity = gravity;
    man->win_width = width;
    man->win_height = height;
    
    if (gravity == SouthGravity || gravity == SouthWestGravity ||
	gravity == SouthEastGravity)
      man->grow_direction = SouthGravity;

    
    ConsoleDebug ("hints: x, y, w, h = %d %d %d %d)\n",
		  sizehints.x, sizehints.y, sizehints.width, sizehints.height);
    ConsoleDebug ("gravity: %d %d\n", sizehints.win_gravity, gravity);
    sizehints.flags |= USPosition;
  }
  if (globals.transient) {
    Window dummyroot, dummychild;
    int x, y, dummymask;

    XQueryPointer(theDisplay, theRoot, &dummyroot, &dummychild, &sizehints.x,
		  &sizehints.y, &x, &y, &dummymask);
    sizehints.win_gravity = NorthWestGravity;
    sizehints.flags |= USPosition;
  }

  man->win_x = sizehints.x;
  man->win_y = sizehints.y;

  winattr.background_pixel = man->backcolor[PLAIN_CONTEXT];
  winattr.border_pixel = man->forecolor[PLAIN_CONTEXT];
  winattr.backing_store = WhenMapped;
  winattr.event_mask = ExposureMask | PointerMotionMask | EnterWindowMask | 
    LeaveWindowMask | KeyPressMask | StructureNotifyMask;

  if (globals.transient)
    winattr.event_mask |= ButtonReleaseMask;
  else
    winattr.event_mask |= ButtonPressMask;

  man->theWindow = XCreateWindow (theDisplay, theRoot, sizehints.x,
				  sizehints.y, man->win_width, man->win_height,
				  1, CopyFromParent, InputOutput, 
				  (Visual *)CopyFromParent, winattrmask,
				  &winattr);

  for (i = 0; i < NUM_CONTEXTS; i++) {
    man->backContext[i] =
      XCreateGC (theDisplay, man->theWindow, gcmask, &gcval);
    XSetForeground (theDisplay, man->backContext[i], man->backcolor[i]);
    XSetLineAttributes (theDisplay, man->backContext[i], line_width, 
			line_style, cap_style,
			join_style);
    
    man->hiContext[i] =
      XCreateGC (theDisplay, man->theWindow, gcmask, &gcval);
    XSetFont (theDisplay, man->hiContext[i], man->ButtonFont->fid);
    XSetForeground (theDisplay, man->hiContext[i], man->forecolor[i]);
    
    gcmask = GCForeground | GCBackground;
    gcval.foreground = man->backcolor[i];
    gcval.background = man->forecolor[i];
    man->flatContext[i] = XCreateGC (theDisplay, man->theWindow, 
						 gcmask, &gcval);
    if (theDepth > 2) {
      gcmask = GCForeground | GCBackground;
      gcval.foreground = man->hicolor[i];
      gcval.background = man->backcolor[i];
      man->reliefContext[i] = XCreateGC (theDisplay, man->theWindow, 
						     gcmask, &gcval);
      
      gcmask = GCForeground | GCBackground;
      gcval.foreground = man->shadowcolor[i];
      gcval.background = man->backcolor[i];
      man->shadowContext[i] = XCreateGC (theDisplay, man->theWindow, 
						     gcmask, &gcval);
    }
  }
    
  set_window_properties (man->theWindow, man->titlename, 
			 man->iconname, &sizehints);
  man->window_up = 1;
  update_window_stuff (man);

  ConsoleDebug ("Leaving init_window\n");
}

void map_new_manager (WinManager *man)
{
  ConsoleDebug ("map_new_manager: %d %d\n", man->win_x, man->win_y);

  update_window_stuff (man);
}

void init_display (void)
{
  theDisplay = XOpenDisplay ("");
  if (theDisplay == NULL) {
    ConsoleMessage ("Can't open display: %s\n", XDisplayName (""));
    ShutMeDown (1);
  }
  x_fd = XConnectionNumber (theDisplay);
  theScreen = DefaultScreen (theDisplay);
  theRoot = RootWindow (theDisplay, theScreen);
  theDepth = DefaultDepth (theDisplay, theScreen);
#ifdef TEST_MONO
  theDepth = 2;
#endif
  globals.screenx = DisplayWidth (theDisplay, theScreen);
  globals.screeny = DisplayHeight (theDisplay, theScreen);
  	
  InitPictureCMap (theDisplay, theRoot);

  ConsoleDebug ("screen width: %d\n", globals.screenx);
  ConsoleDebug ("screen height: %d\n", globals.screeny);
}
