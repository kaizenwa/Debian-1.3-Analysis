#include "FvwmIconMan.h"
#include "readconfig.h"

extern WinData *find_win (WinManager *man, int box);

static int find_selected_manager (void)
{
  WinManager *man;
  
  if (globals.select_win) {
    man = globals.select_win->manager;
    return man->index;
  }
  
  ConsoleDebug ("No selected manager\n");
  return -1;
}

static int find_focus_manager (void)
{
  WinManager *man;
  
  if (globals.focus_win) {
    man = globals.focus_win->manager;
    return man->index;
  }
  
  ConsoleDebug ("No focus manager\n");
  return -1;
}

/* Returns NULL if none found */
static WinManager *get_manager (BuiltinArg *arg)
{
  ButtonType base;
  int offset, index, i;
  
  if (arg->type != ManagerArg) {
    ConsoleMessage ("Internal error in get_manager: 1\n");
    return NULL;
  }
  
  base = arg->value.button_value.base;
  offset = arg->value.button_value.offset;

  switch (base) {
  case NoButton:
    ConsoleMessage ("Internal error in get_manager: 2\n");
    return NULL;

  case SelectButton:
    index = find_selected_manager();
    break;

  case FocusButton:
    index = find_focus_manager();;
    break;

  case AbsoluteButton:
    index = 0;
    break;

  default:
    ConsoleMessage ("Internal error in get_manager\n");
    break;
  }

  if (index == -1 || globals.managers[index].icon_list.n == 0) {
    ConsoleDebug ("get_manager: manager not found\n");
    return NULL;
  }

  /* Now we find the manager modulo the VISIBLE managers */
  /* If someone tries this with an obscenely huge offset, tough luck */
  /* We know that if we've made it this far, there must be a visible
     manager */

  i = 0;
  if (offset > 0) {
    while (i != offset) {
      index++;
      if (index == globals.num_managers)
	index = 0;
      if (globals.managers[index].icon_list.n > 0)
	i++;
    }
  }
  else if (offset < 0) {
    while (i != offset) {
      index--;
      if (index == -1) 
	index = globals.num_managers - 1;
      if (globals.managers[index].icon_list.n > 0) 
	i--;
    }
  }

  return &globals.managers[index];
}

/* Returns -1 if none found */
static int get_button (WinManager *man, BuiltinArg *arg)
{
  ButtonType base;
  int offset, index;
  
  if (arg->type != ButtonArg && arg->type != WindowArg) {
    ConsoleMessage ("Internal error in get_button: 1\n");
    return -1;
  }
  
  base = arg->value.button_value.base;
  offset = arg->value.button_value.offset;

  switch (base) {
  case NoButton:
    ConsoleMessage ("Internal error in get_manager: 2\n");
    return -1;

  case SelectButton:
    if (man->select_box >= 0) {
      index = man->select_box;
    }
    else {
      ConsoleDebug ("No selected window\n");
      return -1;
    }
    break;

  case FocusButton:
    if (man->focus_box >= 0) {
      index = man->focus_box;
    }
    else {
      ConsoleDebug ("No focused window\n");
      return -1;
    }
    break;

  case AbsoluteButton:
    index = 0;
    break;
  }

  if (man->icon_list.n == 0) {
    ConsoleMessage ("Internal error in get_button\n");
    index = 0;
  }
  else {
    index = (index + offset) % man->icon_list.n;
  }
  if (index < 0)
    index += man->icon_list.n;

  return index;
}

/* Returns NULL if none */
static WinData *get_window (WinManager *man, BuiltinArg *arg)
{
  int index = get_button (man, arg);
  
  if (index == -1)
    return NULL;

  return find_win (man, index);
}

static WinData *get_window_from_args (BuiltinArg *args)
{
  WinManager *man;
  man = get_manager (args);
  if (man == NULL)
    return NULL;
  return get_window (man, args + 1);
} 

static int get_button_from_args (BuiltinArg *args)
{
  WinManager *man;
  man = get_manager (args);
  if (man == NULL)
    return -1;
  return get_button (man, args + 1);
} 

int builtin_selectbutton (int numargs, BuiltinArg *args)
{
  int button;
  WinManager *man;

  ConsoleDebug ("selectbutton: ");
  print_args (numargs, args);

  man = get_manager (args);
  if (!man)
    return 0;

  button = get_button (man, args + 1);
  if (button == -1)
    return 0;

  move_highlight (man, button);

  return 1;
}

int builtin_sendcommand (int numargs, BuiltinArg *args)
{
  WinData *win;

  ConsoleDebug ("sendcommand: ");
  print_args (numargs, args);

  win = get_window_from_args (args);
  if (win == NULL)
    return 0;
  
  SendFvwmPipe (args[2].value.string_value, win->app_id);

  return 1;
}

int builtin_quit (int numargs, BuiltinArg *args)
{
  ConsoleDebug ("quit: ");
  print_args (numargs, args);
  ShutMeDown (0);
  return 1;
}

int builtin_warp (int numargs, BuiltinArg *args)
{
  WinManager *man;
  int button, x, y;

  man = get_manager (args);
  if (!man)
    return 0;

  button = get_button (man, args + 1);
  if (button == -1)
    return 0;

  x = man->win_width / 2;
  y = button * man->boxheight + man->boxheight / 2;

  XWarpPointer (theDisplay, None, man->theWindow, 0, 0, 0, 0, x, y);
  return 1;
}

int builtin_printdebug (int numargs, BuiltinArg *args)
{
  int i;

  for (i = 0; i < globals.num_managers; i++) {
    ConsoleDebug ("Manager %d\n---------\n");
    ConsoleDebug ("Keys:\n");
    print_bindings (globals.managers[i].bindings[KEYPRESS]);
    ConsoleDebug ("Mice:\n");
    print_bindings (globals.managers[i].bindings[MOUSE]);
    ConsoleDebug ("Select:\n");
    print_bindings (globals.managers[i].bindings[SELECT]);
    ConsoleDebug ("\n");
  }

  return 1;
}

int builtin_raisemanager (int numargs, BuiltinArg *args)
{
  int i;
  
  ConsoleDebug ("raising\n");

  for (i = 0; i < globals.num_managers; i++) {
    if (globals.managers[i].window_mapped)
      XRaiseWindow (theDisplay, globals.managers[i].theWindow);
  }

  XFlush (theDisplay);
  return 1;
}

int builtin_lowermanager (int numargs, BuiltinArg *args)
{
  int i;

  ConsoleDebug ("lowering\n");

  for (i = 0; i < globals.num_managers; i++) {
    if (globals.managers[i].window_mapped)
      XLowerWindow (theDisplay, globals.managers[i].theWindow);
  }

  XFlush (theDisplay);

  return 1;
}
