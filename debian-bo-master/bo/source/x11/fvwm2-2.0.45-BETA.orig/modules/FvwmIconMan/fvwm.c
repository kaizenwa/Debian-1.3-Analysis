#include "FvwmIconMan.h"

#ifdef COMPILE_STANDALONE
#include "fvwm.h"
#include "module.h"
#else
#include "../../fvwm/fvwm.h"
#include "../../fvwm/module.h"
#endif

typedef struct {
  Ulong paging_enabled;
} m_toggle_paging_data;

typedef struct {
  Ulong desknum;
} m_new_desk_data;

typedef struct {
  Ulong app_id;
  Ulong frame_id;
  Ulong dbase_entry;
  Ulong xpos;
  Ulong ypos;
  Ulong width;
  Ulong height;
  Ulong desknum;
  Ulong windows_flags;
  Ulong window_title_height;
  Ulong window_border_width;
  Ulong window_base_width;
  Ulong window_base_height;
  Ulong window_resize_width_inc;
  Ulong window_resize_height_inc;
  Ulong window_min_width;
  Ulong window_min_height;
  Ulong window_max_width_inc;
  Ulong window_max_height_inc;
  Ulong icon_label_id;
  Ulong icon_pixmap_id;
  Ulong window_gravity;
} m_add_config_data;
  
typedef struct {
  Ulong x, y, desknum;
} m_new_page_data;

typedef struct {
  Ulong app_id, frame_id, dbase_entry;
} m_minimal_data;

typedef struct {
  Ulong app_id, frame_id, dbase_entry;
  Ulong xpos, ypos, icon_width, icon_height;
} m_icon_data;

typedef struct {
  Ulong app_id, frame_id, dbase_entry;
  union {
    Ulong name_long[1];
    Uchar name[4];
  } name;
} m_name_data;

#ifdef MINI_ICONS
typedef struct {
  Ulong app_id, picture, mask, width, height, depth;
} m_mini_icon_data;
#endif

typedef struct {
  Ulong start, type, len, time /* in fvwm 2 only */;
} FvwmPacketHeader;

typedef union {
  m_toggle_paging_data toggle_paging_data;
  m_new_desk_data      new_desk_data;
  m_add_config_data    add_config_data;
  m_new_page_data      new_page_data;
  m_minimal_data       minimal_data;
  m_icon_data          icon_data;
  m_name_data          name_data;
#ifdef MINI_ICONS
  m_mini_icon_data     mini_icon_data;
#endif
} FvwmPacketBody;

/* only used by count_nonsticky_in_hashtab */
static WinManager *the_manager;

static int count_nonsticky_in_hashtab (void *arg)
{
  WinData *win = (WinData *)arg;
  WinManager *man = the_manager;

  if (!win->sticky && win->complete && win->manager == man)
    return 1;
  return 0;
}

static void set_draw_mode (WinManager *man, int flag)
{
  int num;

  if (!man)
    return;

  if (man->we_are_drawing == 0 && flag && man->draw_events_pending) {
    update_window_stuff (man);
  }
  else if (man->we_are_drawing && !flag) {
    the_manager = man;
    num = accumulate_walk_hashtab (count_nonsticky_in_hashtab);
    ConsoleDebug ("SetDrawMode on 0x%x, num = %d\n", man, num);

    if (num == 0)
      return;
    man->configures_expected = num;
  }
  man->we_are_drawing = flag;
}

static int drawing (WinManager *man)
{
  if (!man)
    return 1;

  if (man->we_are_drawing == 0)
    man->draw_events_pending = 1;

  return man->we_are_drawing;
}

static void got_configure (WinManager *man)
{
  if (man && !man->we_are_drawing) {
    man->configures_expected--;
    ConsoleDebug ("got_configure on 0x%x, num_expected now = %d\n",
		  man, man->configures_expected);
    if (man->configures_expected <= 0) 
      set_draw_mode (man, 1);
  }
}

static int win_in_viewport (WinData *win)
{
  WinManager *manager = win->manager;
  long xmin, xmax, ymin, ymax;
  int flag = 0;
  
  assert (manager);

  switch (manager->res) {
  case SHOW_GLOBAL:
    flag = 1;
    break;

  case SHOW_DESKTOP:
    if (win->sticky || win->desknum == globals.desknum)
      flag = 1;
    break;

  case SHOW_PAGE:
#if 0
    if (win->sticky || (win->desknum == globals.desknum && 
			win->x >= 0 && win->x < globals.screenx &&
			win->y >= 0 && win->y < globals.screeny))
    {
    }
#endif
    xmin = win->x;
    xmax = win->x + win->width;
    ymin = win->y;
    ymax = win->y + win->height;

    /* cases: (1) one of the corners is inside screen - handled here
              (2) one of the edges intersects an edge of the screen
	      (3) (1) with window and screen reversed
     */

/*  case 1:

    xmin >= 0 && xmin < screex && ymin >= 0 && ymin <= screeny
      ||
    xmin >= 0 && xmin < screex && ymax >= 0 && ymax <= screeny
      ||
    xmax >= 0 && xmax < screex && ymin >= 0 && ymin <= screeny
      ||
    xmax >= 0 && xmax < screex && ymax >= 0 && ymax <= screeny

goes to:

    xmin && (ymin || ymax)
      ||
    xmax && (ymin || ymax)

goes to:

    (xmin || xmax) && (ymin || max)

*/

/*  Case 2:
    
    xmin <= 0 && xmax >= 0 && ymin >= 0 && ymin <= screeny ||
    xmin <= screenx && xman >= screenx && ymin >= 0 && ymin <= screeny ||
    ymin <= 0 && ymax >= 0 && xmin >= 0 && xmax <= screenx ||
    ymin <= screeny && ymax >= screeny && xmin >= 0 && xmax <= screenx

goes to:

    (ymin >= 0 && ymin <= screeny) && 
          (xmin <= 0 && xmax >= 0 || xmin <= screenx && xmax >= screenx) ||
    (xmin >= 0 && xmax <= screenx) &&
          (ymin <= 0 && ymax >= 0 || ymin <= screeny && ymax >= screeny)
       
*/

    ConsoleDebug ("Screenx = %d, Screeny = %d\n", globals.screenx,
		  globals.screeny);
    ConsoleDebug ("Window (%s) coords: (%d, %d), (%d, %d)\n", win->iconname, xmin, ymin, xmax, ymax);
    if (win->sticky) {
      ConsoleDebug ("Sticky\n");
      flag = 1;
    }
    else if (win->desknum == globals.desknum) {
      if (((xmin >= 0 && xmin < globals.screenx) ||
	   (xmax >= 0 && xmax < globals.screenx)) &&
	  ((ymin >= 0 && ymin < globals.screeny) ||
	   (ymax >= 0 && ymax < globals.screeny))) {
	ConsoleDebug ("Window in screen\n");
	flag = 1;
      }
      else if (((ymin >= 0 && ymin < globals.screeny) && 
	       ((xmin <= 0 && xmax >= 0) || 
		(xmin < globals.screenx && xmax >= globals.screenx))) ||
	       ((xmin >= 0 && xmax <= globals.screenx) &&
	       ((ymin <= 0 && ymax >= 0) || 
		(ymin < globals.screeny && ymax >= globals.screeny)))) {
	ConsoleDebug ("Screen - window cross\n");
	flag = 1;
      }
      else if (((0 > xmin && 0 < xmax) ||
		(globals.screenx > xmin && globals.screenx < xmax)) &&
	       ((0 > ymin && 0 < ymax) ||
		(globals.screeny > 0 && globals.screeny < ymax))) {
	ConsoleDebug ("Screen in window\n");
	flag = 1;
      }
      else {
	ConsoleDebug ("Not in view\n");
	ConsoleDebug ("xmin = %d\txmax = %d\n", xmin, xmax);
	ConsoleDebug ("ymin = %d\tymax = %d\n", ymin, ymax);
	ConsoleDebug ("screenx = %d\tscreeny = %d\n", globals.screenx, 
		      globals.screeny);
	ConsoleDebug ("Expr: %d\n", 
		      ((0 >= xmin && 0) < xmax ||
		       (globals.screenx >= xmin && globals.screenx < xmax)) &&
		      ((0 >= ymin && 0 < ymax) ||
		       (globals.screeny >= 0 && globals.screeny < ymax)));
	  
      }
    }
    else {
      ConsoleDebug ("Not on desk\n");
    }
  }
  return flag;
}

static void reordered_iconlist (WinManager *man)
{
  WinData *p;
  int i;

  assert (man);

  ConsoleDebug ("Possibly reordered list, moving focus\n");

  for (p = man->icon_list.head, i = 0; p && !p->focus; 
       p = p->icon_next, i++) ;
    
  ConsoleDebug ("Focus was: %d, is %d\n", man->focus_box, i);
    
  man->focus_box = i;
}

static void check_in_iconlist (WinData *win, int draw)
{
  int in_viewport;

  if (win->manager && win->complete &&
      !(win->manager->usewinlist && win->winlistskip)) {
    in_viewport = win_in_viewport (win);
    if (win->in_iconlist == 0 && in_viewport) {
      insert_win_iconlist (win);
      reordered_iconlist (win->manager);
      if (draw)
	draw_added_icon (win->manager);
    }
    else if (win->in_iconlist && !in_viewport) {
      delete_win_iconlist (win, win->manager);
      reordered_iconlist (win->manager);
      if (draw)
	draw_deleted_icon (win->manager);
    }
  }
}

WinData *id_to_win (Ulong id)
{
  WinData *win;
  win = find_win_hashtab (id);
  if (win == NULL) {
    win = new_windata ();
    win->app_id = id;
    win->app_id_set = 1;
    insert_win_hashtab (win);
  }
  return win;
}

static void set_win_configuration (WinData *win, FvwmPacketBody *body)
{
  win->desknum = body->add_config_data.desknum;
  win->x = body->add_config_data.xpos;
  win->y = body->add_config_data.ypos;
  win->width = body->add_config_data.width;
  win->height = body->add_config_data.height;
  win->geometry_set = 1;


#if 0
  if (body->add_config_data.windows_flags & ICONIFIED) {
    win->iconified = 1;
  }
  else {
    win->iconified = 0;
    ConsoleDebug ("set_win_configuration: win(%d)->iconified = 0\n", 
		  win->app_id);
  }
#endif
  if (body->add_config_data.windows_flags & STICKY)
    win->sticky = 1;
  else
    win->sticky = 0;

  if (body->add_config_data.windows_flags & WINDOWLISTSKIP)
    win->winlistskip = 1;
  else
    win->winlistskip = 0;
}  

static void configure_window (FvwmPacketBody *body)
{
  Ulong app_id = body->add_config_data.app_id;
  WinData *win;
  WinManager *man;
  ConsoleDebug ("configure_window: %d\n", app_id);

  man = find_windows_manager (body->add_config_data.app_id);
  if (man) {
    man->win_x = body->add_config_data.xpos;
    man->win_y = body->add_config_data.ypos;
    man->win_title = body->add_config_data.window_title_height;
    man->win_border = body->add_config_data.window_border_width;
    if (man->win_border)
      man->win_border++;
    ConsoleDebug ("New Window: x, y: %d %d. title, border: %d %d\n",
                  man->win_x, man->win_y, man->win_title, man->win_border);
  }

  win = id_to_win (app_id);

  set_win_configuration (win, body);

  check_win_complete (win);
  check_in_iconlist (win, drawing (win->manager));
  got_configure (win->manager);
}

static void focus_change (FvwmPacketBody *body)
{ 
  Ulong app_id = body->minimal_data.app_id;
  WinData *win = id_to_win (app_id);
  int box, oldbox;

  ConsoleDebug ("Focus Change\n");
  ConsoleDebug ("\tID: %d\n", app_id);

  if (globals.focus_win) {
    globals.focus_win->focus = 0;
  }
  win->focus = 1;

  if ( globals.focus_win &&
       globals.focus_win->complete &&
       globals.focus_win->manager ) {
    oldbox = globals.focus_win->manager->focus_box;
    globals.focus_win->manager->focus_box = -1;
    if (drawing (win->manager))
      draw_button (globals.focus_win->manager, globals.focus_win, oldbox);
    globals.focus_win = NULL;
  }

  if ( win->complete  &&
       win->in_iconlist  &&
       win->manager->window_up  &&
       win->manager->followFocus ) {
    box = win_to_box(win->manager, win);
    win->manager->focus_box = box;
    if (drawing (win->manager))
      draw_button (win->manager, win, box);

    /* I don't understand why this is here:
    if (globals.focus_win && globals.focus_win->manager != win->manager)
      draw_window (globals.focus_win->manager); */
  }

  globals.focus_win = win;
  ConsoleDebug ("leaving focus_change\n");
}

static void res_name (FvwmPacketBody *body)
{
  Ulong app_id = body->name_data.app_id;
  Uchar *name = body->name_data.name.name;
  WinData *win;
  WinManager *oldman;
  int new;

  ConsoleDebug ("In res_name\n");

  win = id_to_win (app_id);

  copy_string (&win->resname, (char *)name);
  oldman = win->manager;
  new = set_win_manager (win, ALL_NAME);
  set_displaystring (win);
  if (new) {
    if (oldman && win->in_iconlist) {
      delete_win_iconlist  (win, oldman);
      if (win == globals.focus_win)
	oldman->focus_box = -1;
      if (drawing (oldman))
	draw_deleted_icon (oldman);
    }
    assert (!win->in_iconlist);
  }
  
  check_win_complete (win);
  check_in_iconlist (win, drawing (win->manager));
  ConsoleDebug ("Exiting res_name\n");
}

static void class_name (FvwmPacketBody *body)
{
  Ulong app_id = body->name_data.app_id;
  Uchar *name = body->name_data.name.name;
  WinData *win;
  WinManager *oldman;
  int new;

  ConsoleDebug ("In class_name\n");

  win = id_to_win (app_id);

  copy_string (&win->classname, (char *)name);
  oldman = win->manager;
  new = set_win_manager (win, ALL_NAME);
  set_displaystring (win);
  if (new) {
    if (oldman && win->in_iconlist) {
      delete_win_iconlist  (win, oldman);
      if (win == globals.focus_win)
	oldman->focus_box = -1;
      if (drawing (oldman))
	draw_deleted_icon (oldman);
    }
    assert (!win->in_iconlist);
  }
  
  check_win_complete (win);
  check_in_iconlist (win, drawing (win->manager));
  ConsoleDebug ("Exiting class_name\n");
}

static void icon_name (FvwmPacketBody *body)
{
  WinData *win;
  WinManager *oldman;
  Ulong app_id;
  Uchar *name = body->name_data.name.name;
  int moved = 0, new;

  ConsoleDebug ("In icon_name\n");

  app_id = body->name_data.app_id;

  win = id_to_win (app_id);

  if (win->iconname && !strcmp (win->iconname, name)) {
    ConsoleDebug ("No icon change: %s %s\n", win->iconname, name);
    return;
  }

  copy_string (&win->iconname, (char *)name);
  ConsoleDebug ("new icon name: %s\n", win->iconname);
  oldman = win->manager;
  new = set_win_manager (win, ALL_NAME);
  set_displaystring (win);
  check_win_complete (win);
  if (new) {
    if (oldman && win->in_iconlist) {
      delete_win_iconlist  (win, oldman);
      if (win == globals.focus_win)
	oldman->focus_box = -1;
      if (drawing (oldman))
	draw_deleted_icon (oldman);
    }
    assert (!win->in_iconlist);
    check_in_iconlist (win, drawing (win->manager));
  }
  else {
    if (win->in_iconlist && 
	(win->manager->format_depend & ICON_NAME) && win->manager->sort) {
      moved = move_win_iconlist (win);
    }
    if (moved) 
      reordered_iconlist (win->manager);
    if ((moved || win->in_iconlist) && win->complete && drawing (win->manager))
      draw_window (win->manager);
  }

  ConsoleDebug ("Exiting icon_name\n");
}

static void window_name (FvwmPacketBody *body)
{
  WinData *win;
  WinManager *oldman;
  Ulong app_id;
  Uchar *name = body->name_data.name.name;
  int moved = 0, new;

  ConsoleDebug ("In window_name\n");

  app_id = body->name_data.app_id;

  win = id_to_win (app_id);

  /* This is necessary because bash seems to update the window title on
     every keystroke regardless of whether anything changes */
  if (win->titlename && !strcmp (win->titlename, name)) {
    ConsoleDebug ("No name change: %s %s\n", win->titlename, name);
    return;
  }

  copy_string (&win->titlename, (char *)name);

  oldman = win->manager;
  new = set_win_manager (win, ALL_NAME);
  set_displaystring (win);
  check_win_complete (win);
  if (new) {
    if (oldman && win->in_iconlist) {
      delete_win_iconlist  (win, oldman);
      if (win == globals.focus_win)
	oldman->focus_box = -1;
      if (drawing (oldman))
	draw_deleted_icon (oldman);
    }
    assert (!win->in_iconlist);
    check_in_iconlist (win, drawing (win->manager));
  }
  else {
    if (win->in_iconlist && 
	(win->manager->format_depend & TITLE_NAME) && win->manager->sort) {
      moved = move_win_iconlist (win);
    }
    if (moved) 
      reordered_iconlist (win->manager);
    if ((moved || win->in_iconlist) && win->complete && drawing (win->manager))
      draw_window (win->manager);
  }

  ConsoleDebug ("Exiting window_name\n");
}

static void new_window (FvwmPacketBody *body)
{
  WinData *win;
  WinManager *man;

  man = find_windows_manager (body->add_config_data.app_id);
  if (man) {
    man->win_x = body->add_config_data.xpos;
    man->win_y = body->add_config_data.ypos;
    man->win_title = body->add_config_data.window_title_height;
    man->win_border = body->add_config_data.window_border_width;
    if (man->win_border)
      man->win_border++;
    ConsoleDebug ("New Window: x, y: %d %d. title, border: %d %d\n",
                  man->win_x, man->win_y, man->win_title, man->win_border);
  }


  win = new_windata();
  if (!(body->add_config_data.windows_flags & TRANSIENT)) {
    win->app_id = body->add_config_data.app_id;
    win->app_id_set = 1;
    set_win_configuration (win, body);

    insert_win_hashtab (win);
    check_win_complete (win);
    check_in_iconlist (win, drawing (win->manager));
  }
}

static void destroy_window (FvwmPacketBody *body)
{
  WinData *win;
  Ulong app_id;

  app_id = body->minimal_data.app_id;
  win = id_to_win (app_id);
  delete_win_hashtab (win);
  if (globals.focus_win == win)
    globals.focus_win = NULL;
  if (globals.select_win == win)
    globals.select_win = NULL;
  if (win->in_iconlist) {
    delete_win_iconlist (win, win->manager);
    reordered_iconlist (win->manager);
    update_window_stuff (win->manager);
  }
  free_windata (win);
}

#ifdef MINI_ICONS
static void mini_icon (FvwmPacketBody *body)
{
  Ulong app_id = body->mini_icon_data.app_id;
  WinData *win;
  int box;
  
  win = id_to_win (app_id);
  win->pic.picture = body->mini_icon_data.picture;
  win->pic.mask = body->mini_icon_data.mask;
  win->pic.width = body->mini_icon_data.width;
  win->pic.height = body->mini_icon_data.height;
  win->pic.depth = body->mini_icon_data.depth;

  ConsoleDebug ("mini_icon: 0x%x 0x%x %dx%dx%d\n", win->pic.picture,
		win->pic.mask, win->pic.width, win->pic.height,
		win->pic.depth);

  if (win->complete && win->in_iconlist && win->manager->draw_icons) {
    if (drawing (win->manager)) {
      box = win_to_box (win->manager, win);
      if (box >= 0)
	draw_button (win->manager, win, box);
      else
	ConsoleMessage ("Internal error in mini_icon\n"); 
    }
  }

}
#endif

static void iconify (FvwmPacketBody *body, int dir)
{
  Ulong app_id = body->minimal_data.app_id;
  WinData *win;
  int box;
  
  win = id_to_win (app_id);
  
  if (dir == 0) {
    if (win->iconified == 0) {
      ConsoleDebug ("Already deiconified\n");
      return;
    }
    else {
      ConsoleDebug ("iconify: win(%d)->iconified = 0\n", win->app_id);
      win->iconified = 0;
    }
  }
  else {
    if (win->iconified == 1) {
      ConsoleDebug ("Already iconified\n");
      return;
    }
    else {
      win->iconified = 1;
    }
  }
  
  check_win_complete (win);
  check_in_iconlist (win, drawing (win->manager));
  if (win->complete && win->in_iconlist) {
    if (drawing (win->manager)) {
      box = win_to_box (win->manager, win);
      if (box >= 0)
	draw_button (win->manager, win, box );
      else
	ConsoleMessage ("Internal error in iconify\n");
    }
  }
}

void update_win_in_hashtab (void *arg)
{
  WinData *p = (WinData *)arg;
  check_in_iconlist (p, 0);
}

static void new_desk (FvwmPacketBody *body)
{
  int i;

  globals.desknum = body->new_desk_data.desknum;
  walk_hashtab (update_win_in_hashtab);

  for (i = 0; i < globals.num_managers; i++)
    update_window_stuff (&globals.managers[i]);
}

static void sendtomodule (FvwmPacketBody *body)
{
  extern void execute_function (char *);
  Uchar *string = body->name_data.name.name;

  ConsoleDebug ("Got string: %s\n", string);

  execute_function (string);
}

static void ProcessMessage (Ulong type, FvwmPacketBody *body)
{
  int i;

  ConsoleDebug ("FVWM Message type: %d\n", type); 

  switch(type) {
  case M_CONFIGURE_WINDOW:
    ConsoleDebug ("DEBUG::M_CONFIGURE_WINDOW\n");
    configure_window (body);
    break;

  case M_FOCUS_CHANGE:
    ConsoleDebug ("DEBUG::M_FOCUS_CHANGE\n");
    focus_change (body);
    break;

  case M_RES_NAME:
    ConsoleDebug ("DEBUG::M_RES_NAME\n");
    res_name (body);
    break;

  case M_RES_CLASS:
    ConsoleDebug ("DEBUG::M_RES_CLASS\n");
    class_name (body);
    break;

  case M_MAP:
    ConsoleDebug ("DEBUG::M_MAP\n");
    break;

  case M_ADD_WINDOW:
    ConsoleDebug ("DEBUG::M_ADD_WINDOW\n");
    new_window (body);
    break;

  case M_DESTROY_WINDOW:
    ConsoleDebug ("DEBUG::M_DESTROY_WINDOW\n");
    destroy_window (body);
    break;

#ifdef MINI_ICONS
  case M_MINI_ICON:
    ConsoleDebug ("DEBUG::M_MINI_ICON\n");
    mini_icon (body);
    break;
#endif

  case M_WINDOW_NAME:
    ConsoleDebug ("DEBUG::M_WINDOW_NAME\n");
    window_name (body);
    break;

  case M_ICON_NAME:
    ConsoleDebug ("DEBUG::M_ICON_NAME\n");
    icon_name (body);
    break;

  case M_DEICONIFY:
    ConsoleDebug ("DEBUG::M_DEICONIFY\n");
    iconify (body, 0);
    break;

  case M_ICONIFY:
    ConsoleDebug ("DEBUG::M_ICONIFY\n");
    iconify (body, 1);
    break;

  case M_END_WINDOWLIST:
    ConsoleDebug ("DEBUG::M_END_WINDOWLIST\n");
    ConsoleDebug (">>>>>>>>>>>>>>>>>>>>>>>End window list<<<<<<<<<<<<<<<\n");
    if (globals.focus_win && globals.focus_win->in_iconlist) {
	globals.focus_win->manager->focus_box = 
	  win_to_box (globals.focus_win->manager, globals.focus_win);
    }
    for (i = 0; i < globals.num_managers; i++)
      init_window (i);
    print_iconlist();
    break;

  case M_NEW_DESK:
    ConsoleDebug ("DEBUG::M_NEW_DESK\n");
    new_desk (body);
    break;

  case M_NEW_PAGE:
    ConsoleDebug ("DEBUG::M_NEW_PAGE\n");
    if (globals.x == body->new_page_data.x &&
	globals.y == body->new_page_data.y &&
	globals.desknum == body->new_page_data.desknum) {
      ConsoleDebug ("Useless NEW_PAGE received\n");
      break;
    }
    globals.x = body->new_page_data.x;
    globals.y = body->new_page_data.y;
    globals.desknum = body->new_page_data.desknum;
    for (i = 0; i < globals.num_managers; i++) {
      set_draw_mode (&globals.managers[i], 0);
    }
    break;

  case M_STRING:
    ConsoleDebug ("DEBUG::M_STRING\n");
    sendtomodule (body);
    break;

  default:
    break;
  }
}

void ReadFvwmPipe()
{
  int body_length;
  FvwmPacketHeader header;
  FvwmPacketBody *body;

  PrintMemuse();

  ConsoleDebug("DEBUG: entering ReadFvwmPipe\n");
  body_length = ReadFvwmPacket(Fvwm_fd[1], (unsigned long *) &header,
                 (unsigned long **)&body);
  body_length -= HEADER_SIZE;
  if (header.start == START_FLAG) {
    ProcessMessage (header.type, body);
    if (body_length) {
      Free (body);
    }
  }
  else {
    DeadPipe (1);
  }
  ConsoleDebug("DEBUG: leaving ReadFvwmPipe\n");
}

