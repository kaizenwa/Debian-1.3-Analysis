#include "FvwmIconMan.h"
#include "readconfig.h"

#define DEFAULT_MOUSE "0 N sendcommand select select Iconify"

GlobalData globals;
ContextDefaults contextDefaults[] = {
  { "plain", BUTTON_UP, { "black", "white" }, { "white", "gray"} },
  { "focus", BUTTON_UP, { "white", "gray" }, { "black", "white" } },
  { "select", BUTTON_FLAT, { "black", "white" }, { "white", "gray" } },
  { "focusandselect", BUTTON_FLAT, { "white", "gray" }, { "black", "white" } }
};

int Fvwm_fd[2];
int x_fd;
char *Module = "*FvwmIconMan";
int ModuleLen = 12;

/* This is solely so that we can turn a string constant into something
   which can be freed */

static char *alloc_string (char *string)
{
  int len = strlen (string);
  char *ret = (char *)safemalloc ((len + 1) * sizeof (char));
  strcpy (ret, string);
  return ret;
}

static void init_win_manager (int id)
{
  int i;

  globals.managers[id].index = id;
#ifdef MINI_ICONS
  globals.managers[id].draw_icons = 0;
#endif
  globals.managers[id].res = SHOW_PAGE;
  globals.managers[id].icon_list.n = 0;
  globals.managers[id].icon_list.head = NULL;  
  globals.managers[id].icon_list.tail = NULL;
  globals.managers[id].window_up = 0;
  globals.managers[id].window_mapped = 0;
  globals.managers[id].fontname = NULL;
  globals.managers[id].titlename = alloc_string ("FvwmIconMan");
  globals.managers[id].iconname = alloc_string ("FvwmIconMan");
  globals.managers[id].formatstring = alloc_string ("%c: %i");
  globals.managers[id].format_depend = CLASS_NAME | ICON_NAME;

  for ( i = 0; i < NUM_CONTEXTS; i++ ) {
    globals.managers[id].backColorName[i] = NULL;
    globals.managers[id].foreColorName[i] = NULL;
    globals.managers[id].buttonState[i] = contextDefaults[i].state;
  }
  globals.managers[id].geometry = NULL;
  globals.managers[id].show.list = NULL;
  globals.managers[id].show.mask = ALL_NAME;
  globals.managers[id].dontshow.list = NULL;
  globals.managers[id].dontshow.mask = ALL_NAME;
  globals.managers[id].grow_direction = ForgetGravity;
  globals.managers[id].followFocus = 0;
  globals.managers[id].usewinlist = 1;
  globals.managers[id].sort = 1;
  globals.managers[id].focus_box = -1;
  globals.managers[id].bindings[MOUSE]    = ParseMouseEntry (DEFAULT_MOUSE);
  globals.managers[id].bindings[KEYPRESS] = NULL;
  globals.managers[id].bindings[SELECT]   = NULL;
  globals.managers[id].we_are_drawing = 1;
  globals.managers[id].draw_events_pending = 0;
  globals.managers[id].configures_expected = 0;
}  

void print_managers (void)
{
#ifdef PRINT_DEBUG
  int i;

  for (i = 0; i < globals.num_managers; i++) {
    ConsoleDebug ("Manager %d:\n", i + 1);
    if (globals.managers[i].res == SHOW_GLOBAL)
      ConsoleDebug ("ShowGlobal\n");
    else if (globals.managers[i].res == SHOW_DESKTOP)
      ConsoleDebug ("ShowDesktop\n");
    else if (globals.managers[i].res == SHOW_PAGE)
      ConsoleDebug ("ShowPage\n");
    
    ConsoleDebug ("DontShow:\n");
    print_stringlist (&globals.managers[i].dontshow);
    ConsoleDebug ("Show:\n");
    print_stringlist (&globals.managers[i].show);
    
    ConsoleDebug ("Font: %s\n", globals.managers[i].fontname);
    ConsoleDebug ("Geometry: %s\n", globals.managers[i].geometry);
    ConsoleDebug ("\n");
  }

#endif

}

int allocate_managers (int num)
{
  int i;

  if (globals.managers) {
    ConsoleMessage ("Already have set the number of managers\n");
    return 0;
  }
  
  if (num < 1) {
    ConsoleMessage ("Can't have %d managers\n", num);
    return 0;
  }

  globals.num_managers = num;
  globals.managers = (WinManager *)safemalloc (num * sizeof (WinManager));
  
  for (i = 0; i < num; i++) {
    init_win_manager (i);
  }
  
  return 1;
}

void init_globals (void)
{
  globals.desknum = ULONG_MAX;
  globals.x = ULONG_MAX;
  globals.y = ULONG_MAX;
  globals.screenx = 0;
  globals.screeny = 0;
  globals.num_managers = 1;
  globals.managers = NULL;
  globals.focus_win = NULL;
  globals.select_win = NULL;
  globals.transient = 0;
}
