/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.  If
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
*/

#include <stdio.h>

#include "McApp.h"
#include "McResource.h"
#include <strings.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <locale.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>


static XrmOptionDescRec opTable[] = {
  { "-geometry",     "*geometry",     XrmoptionSepArg, (caddr_t)NULL },
  { "-iconGeometry", "*iconGeometry", XrmoptionSepArg, (caddr_t)NULL },
  { "-iconic",       "*iconStartup",  XrmoptionNoArg,  (caddr_t)"on" },
  { "-ic",           "*iconStartup",  XrmoptionNoArg,  (caddr_t)"on" },
  { "-background",   "*background",   XrmoptionSepArg, (caddr_t)NULL },
  { "-bg",           "*background",   XrmoptionSepArg, (caddr_t)NULL },
  { "-foreground",   "*foreground",   XrmoptionSepArg, (caddr_t)NULL },
  { "-fg",           "*foreground",   XrmoptionSepArg, (caddr_t)NULL },
  { "-darkborder",   "*darkBorder",   XrmoptionSepArg, (caddr_t)NULL },
  { "-db",           "*darkBorder",   XrmoptionSepArg, (caddr_t)NULL },
  { "-brightborder", "*brightBorder", XrmoptionSepArg, (caddr_t)NULL },
  { "-bb",           "*brightBorder", XrmoptionSepArg, (caddr_t)NULL },
  { "-hilightcolor", "*hilightColor", XrmoptionSepArg, (caddr_t)NULL },
  { "-hc",           "*hilightColor", XrmoptionSepArg, (caddr_t)NULL },
  { "-menucolor",    "*menuColor",    XrmoptionSepArg, (caddr_t)NULL },
  { "-mc",           "*menuColor",    XrmoptionSepArg, (caddr_t)NULL },
  { "-focuscolor",   "*focusColor",   XrmoptionSepArg, (caddr_t)NULL },
  { "-fc",           "*focusColor",   XrmoptionSepArg, (caddr_t)NULL },
  { "-tipcolor",     "*tipColor",     XrmoptionSepArg, (caddr_t)NULL },
  { "-tc",           "*tipColor",     XrmoptionSepArg, (caddr_t)NULL },
  { "-style",        "*style",	      XrmoptionSepArg, (caddr_t)NULL },
  { "-xrm",           NULL,           XrmoptionResArg, (caddr_t)NULL },
  { "-display",      ".display",      XrmoptionSepArg, (caddr_t)NULL },
  { "-font",         "McTools.font",  XrmoptionSepArg, (caddr_t)NULL },
  { "-fn",           "McTools.font",  XrmoptionSepArg, (caddr_t)NULL },
  { "-ffn",          "*fixedFont",    XrmoptionSepArg, (caddr_t)NULL },
  { "-name",         "*name",         XrmoptionSepArg, (caddr_t)NULL },
  { "-title",        "*title",        XrmoptionSepArg, (caddr_t)NULL },
  { "-resdebug",     "*resdebug",     XrmoptionNoArg,  (caddr_t)"on" },
  { "-clip",         "*clipGadgets",  XrmoptionNoArg,  (caddr_t)"on" },
  { "-rv",           "*reverseVideo", XrmoptionNoArg,  (caddr_t)"on" },
  { "+rv",           "*reverseVideo", XrmoptionNoArg,  (caddr_t)"off" },
  { "-sync",         "*synchronize",  XrmoptionNoArg,  (caddr_t)"on" },
  { "-synchronize",  "*synchronize",  XrmoptionNoArg,  (caddr_t)"on" },
  { "-gb",           "*gadgetBorder", XrmoptionNoArg,  (caddr_t)"on" },
};

static string_t Name2Class(string_t str, string_t buf);
static void MergeDatabases(McApp *app, XrmDatabase cmdlineDB);

/****************************************************************************
 * Parse the command line and open the display connection
 */
Display *McParseOpenDisp(McApp *app, int *argc, char *argv[], string_t class,
			 XrmOptionDescRec *DescTable, int DescSize) {
  char dsn[256], dsc[256], *str_type[20], *displayname=NULL;
  XrmValue value;
  XrmDatabase appDB, cmdlineDB;
  Display *display = NULL;

  XrmInitialize();

  if ((myname=strrchr(argv[0], '/'))) myname++; else myname=argv[0];
  strcpy(dsn,myname);
  strcat(dsn,".display");
  if (class) {
    app->class=class;
    strcpy(dsc,class);
  } else {
    app->class=strdup(myname);
    Name2Class(myname,app->class);
  }
  strcat(dsc,".Display");

  if (!app->argv_save) {
    int len=sizeof(char *) * ((*argc)+2);

    app->argc_save=*argc;
    app->argv_save = (char **)calloc(len, 1);
    memcpy(app->argv_save, argv, len);
  }

  XrmParseCommand(&cmdlineDB, opTable, sizeof(opTable)/sizeof(opTable[0]),
		  myname , argc, argv);

  if (DescTable && DescSize) {
    XrmParseCommand(&appDB, DescTable, DescSize, myname , argc, argv);
    XrmMergeDatabases(appDB, &cmdlineDB);
  }

  if (XrmGetResource(cmdlineDB, dsn, dsc, str_type, &value)==True) {
    displayname=value.addr;
  }

  if (!(display=XOpenDisplay(displayname))) {
    fprintf(stderr,"%s: Can't open display '%s'.\n", myname,
	    XDisplayName(displayname));
    cleanup(1);
  }

  app->display=display;

  MergeDatabases(app, cmdlineDB);

  return display;
}

/****************************************************************************
 * Read the defaults from the resource database
 */
void McReadStandards(McApp *app) {
  string_t str;

  if ((str=McGetResource(app, "McTools.name"))) {
    myname=str;
  }

  if (McGetSwitch(app, "McTools.resdebug")) app->flags|=MCAPP_RESDEBUG;

  if ((str=McGetResource(app, "McTools.title")))      app->window_title=str;
  if ((str=McGetResource(app, "McTools.foreground"))) 
    app->color_names[COL_FOREGROUND]=str;

  if ((str=McGetResource(app, "McTools.background")))
    app->color_names[COL_BACKGROUND]=str;

  if ((str=McGetResource(app, "McTools.brightBorder")))
    app->color_names[COL_BRIGHT]=str;

  if ((str=McGetResource(app, "McTools.darkBorder")))
    app->color_names[COL_DARK]=str;

  if ((str=McGetResource(app, "McTools.hilightColor")))
    app->color_names[COL_SELECTED]=str;

  if ((str=McGetResource(app, "McTools.menuColor"))) 
    app->color_names[COL_MENU]=str;

  if ((str=McGetResource(app, "McTools.focusColor"))) 
    app->color_names[COL_FOCUS]=str;

  if ((str=McGetResource(app, "McTools.tipColor"))) 
    app->color_names[COL_TIP]=str;

  if ((str=McGetResource(app, "McTools.style")))
    app->style=atoi(str);

  if ((str=McGetResource(app, "McTools.font")))     app->default_font_name=str;
  if ((str=McGetResource(app, "McTools.fixedFont")))  app->fixed_font_name=str;

  if (McGetSwitch(app,"McTools.iconStartup"))  app->flags|=MCAPP_ICONIC;
  if (McGetSwitch(app,"McTools.iconic"))       app->flags|=MCAPP_ICONIC;
  if (McGetSwitch(app,"McTools.reverseVideo")) app->flags|=MCAPP_REVERSE;
  if (McGetSwitch(app,"McTools.synchronize"))  app->flags|=MCAPP_SYNCED;
  if (McGetSwitch(app,"McTools.backingstore")) app->flags|=MCAPP_BSTORE;
  if (McGetSwitch(app,"McTools.gadgetBorder")) app->flags|=MCAPP_GADGET_BORDER;

  if ((str=McGetResource(app, "McTools.geometry")))
    app->geometry_flags=XParseGeometry(str, &app->x, &app->y, &app->w,&app->h);

  if ((str=McGetResource(app, "McTools.iconGeometry")))
    app->icon_geometry_flags=
      XParseGeometry(str, &app->ico_x, &app->ico_y, &app->ico_w, &app->ico_h);
}

/****************************************************************************
 * Read one entry from the resource database
 * This resource management is a pain!!
 */

/*
 * where=0 -> return always NULL
 * where=1 -> check resDB
 * where=2 -> check stateDB
 * where=3 -> check stateDB, then resDB
 */
string_t McFindResource(McApp *app, const string_t name, int where) {
  char bun[256], buc[256], *res;
  int l;
  char *str_type[20];
  XrmValue value;

  strcpy(bun,myname);
  l=strlen(bun);
  bun[l++]='.';
  strcpy(bun+l,name);
  bun[l]=tolower(bun[l]);

  strcpy(buc, app->class);
  l=strlen(buc);
  buc[l++]='.';
  strcpy(buc+l,name);
  buc[l]=toupper(buc[l]);

  if ((where & 2) && (app->stateDB) &&
      (XrmGetResource(app->stateDB, bun, buc, str_type, &value)==True)) {
    res=value.addr;
  } else if ((where & 1) && (app->resDB) &&
      (XrmGetResource(app->resDB, bun, buc, str_type, &value)==True)) {
    res=value.addr;
  } else {
    res=NULL;
  }

  if (app->flags&MCAPP_RESDEBUG) {
    if (res)
      printf("%s='%s'\n", name, res);
    else
      printf("%s undefined.\n", name);
  }

  return res;
}

string_t McGetResource(McApp *app, string_t name) {
  return McFindResource(app, name, 3);
}

int McTestSwitch(string_t str) {
  if ((!strcasecmp(str,"on")) || (!strcasecmp(str,"true")) ||
      (!strcasecmp(str,"yes")) || (!strcmp(str,"1"))) return 1;
  return 0;
}

int McGetSwitch(McApp *app, string_t name) {
  string_t str;
  if (!(str=McGetResource(app,name))) return 0;
  return McTestSwitch(str);
}

/****************************************************************************
 *
 * Convenience functions for those W. people...
 */
int McGetProfileInt(struct McApp *app, const string_t section,
		    const string_t name, int def) {
  unsigned char buf[1024], *str;
  sprintf(buf, "%s.%s", section, name);
  str=McGetResource(app, buf);
  if (str) return atoi(str); else return def;
}

const string_t McGetProfileString(struct McApp *app,
					const string_t section,
					const string_t name,
					const string_t def) {
  unsigned char buf[1024], *str;
  sprintf(buf, "%s.%s", section, name);
  str=McGetResource(app, buf);
  if (str) return str; else return def;
}

void McWriteProfileInt(struct McApp *app,
		       const string_t section,
		       const string_t name,
		       int value) {
  unsigned char buf[1024], val[32];
  sprintf(buf, "%s.%s", section, name);
  sprintf(val, "%d", value);
  McPutResource(app, buf, val);
}

void McWriteProfileString(struct McApp *app,
			  const string_t section,
			  const string_t name,
			  const string_t str) {
  unsigned char buf[1024];
  sprintf(buf, "%s.%s", section, name);
  McPutResource(app, buf, str);
}

/***************************************************************************/

void McPutResource(McApp *app, const string_t name, const string_t value) {
  unsigned char buf[1024];

  strcpy(buf, app->class);
  strcat(buf, ".");
  strcat(buf, name);

  XrmPutStringResource(&app->stateDB, buf, value);
}

void McPutSwitch(McApp *app, const string_t name, int bool) {
  McPutResource(app, name, bool?"true":"false");
}

void McWriteResources(McApp *app) {
  char buf[1024], *errstr;
  struct stat st;

  /*
   * Make the profile directory and check if it exists and is really a dir
   */
  strcpy(buf, getenv("HOME"));
  strcat(buf, "/" PROFILE_DIR);
  mkdir(buf, 0777);

  errstr=NULL;
  if (stat(buf, &st)<0) {
    errstr=strerror(errno);
  } else {
    if (!S_ISDIR(st.st_mode)) {
      errstr=_M("not a directory");
    }
  }

  if (errstr) {
    fprintf(stderr, _M("%s: Can't create state directory ~/%s: %s\n"),
	    myname, PROFILE_DIR, strerror(errno));
    return;
  }

  /*
   * XrmPutFileDatabase() won't even touch the file if the database
   * is empty. So make sure a previous file is deleted.
   */
  sprintf(buf, "%s/" PROFILE_DIR "/%s.stateDB", getenv("HOME"), app->class);
  unlink(buf);
  XrmPutFileDatabase(app->stateDB, buf);
}

/****************************************************************************
 * Read lotsa databases scattered across the system (Yuck!)
 * (Have I already mentioned that this resource management is a pain?)
 *
 * Xlib Programming Manual says:
 * - Classname file in the app-defaults directory
 * - Classname file in the directory specified by the XUSERFILESEARCHPATH
 *   or XAPPLRESDIR environment variables.
 * - Property set using xrdb, accessible through the XResourceManagerString()
 *   macro or, if that is empty, the .Xdefaults file in the home directory.
 * - File specified by the XENVIRONMENT environment variable or, if not set,
 *   the .Xdefaults-<hostname> file in the user's home directory.
 * - Command line arguments.
 */


/*
 * Treat <path> as a list of directories separated by colons and
 * try to find <file>. Read in first <file> found.
 *
 */
static int MergeDB(string_t path, string_t file, XrmDatabase *db) {
  string_t add, *start, *p=path, *home=getenv("HOME");
  char buf[1024];
  XrmDatabase daba;

  if (!path || !path[0]) return 0;

  /*
   * This is a simple approach, but I think its enuff,
   * I even think noone will ever use all this... :)
   */
  if (!path || !strlen(path)) return 0;
  do {
    start=p;
    while((*p) && (*p!=':')) p++;
    if ((*start=='~') && ((start[1]=='/')||(start[1]==0)||(start[1]==':'))) {
      strcpy(buf,home);
      add=buf+strlen(home);
      *add++='/';			/* You could do fun things in C */
      start++;
      if (*start=='/') start++;
    } else {
      add=buf;
    }
    strncpy(add, start, p-start);
    if (add[p-start-1]!='/') {
      add[p - start]='/';
      strcpy(add+(p-start)+1,file);
    } else
      strcpy(add+(p-start),file);
    daba=XrmGetFileDatabase(buf);
    if (daba) {
      XrmMergeDatabases(daba, db);
/*      XrmDestroyDatabase(daba); */
      return 1; /* Should I continue searching now? */
    }
  } while(*p++);
  return 0;
}

static void MergeDatabases(McApp *app, XrmDatabase cmdlineDB) {
  char buf[1024], *env;
  XrmDatabase daba;

  /* Get standards from /usr/lib/X11/app-defaults/<class> */
  MergeDB(RESOURCE_PATH, app->class, &app->resDB);

  /* Try $XUSERFILESEARCHPATH */
  MergeDB(getenv("XUSERFILESEARCHPATH"), app->class, &app->resDB);

  /* Try $XAPPLRESDIR */
  MergeDB(getenv("XAPPLRESDIR"), app->class, &app->resDB);

  /* Try $HOME */
  *buf='.';
  strcpy(buf+1, app->class);
  strcat(buf,"-defaults");
  MergeDB(getenv("HOME"), buf, &app->resDB);

  /* Get standards from the server, or, if not available, read ~/.Xdefaults */
  if (XResourceManagerString(app->display)) {
    daba=XrmGetStringDatabase(XResourceManagerString(app->display));
    if (daba) XrmMergeDatabases(daba, &app->resDB);
  } else {
    strcpy(buf, getenv("HOME"));
    strcat(buf,"/.Xdefaults");
    daba=XrmGetFileDatabase(buf);
    if (daba) {
      XrmMergeDatabases(daba=XrmGetFileDatabase(buf), &app->resDB);
/*      XrmDestroyDatabase(daba);*/
    }
  }

  /* Now get server specific data */
  if ((env=getenv("XENVIRONMENT"))==NULL) {
    int len;
    strcpy(buf, getenv("HOME"));
    strcat(buf,"/.Xdefaults-");
    len=strlen(buf);
    gethostname(buf+len, 1024-len);
    env=buf;
  }
  daba=XrmGetFileDatabase(env);
  if (daba) {
    XrmMergeDatabases(daba=XrmGetFileDatabase(env), &app->resDB);
/*    XrmDestroyDatabase(daba);*/
  }

  /* Commandline has highest priority */
  XrmMergeDatabases(cmdlineDB, &app->resDB);

  /* Maybe the program saved its state on the last invocation...
   * Try to read ~/.McTools/<class> into a second database,
   * so that it can be written to a file without all the junk in resDB.
   */
  sprintf(buf, "%s/" PROFILE_DIR "/%s.stateDB", getenv("HOME"), app->class);
  app->stateDB=XrmGetFileDatabase(buf);
}

/****************************************************************************
 * Tell the windowmanager what it has to expect from us.
 */

void McSetHints(McWindow *mcw, string_t title, int argc, char *argv[],
		XSizeHints *sizehints, XWMHints *wm_hints) {
  McApp *app=mcw->app;
  XClassHint	class_hints;
  XTextProperty windowName;
  XTextProperty iconName;
  char *list[2];
  XTextProperty tprop;

  if (title) {
    list[0]=title;
  } else {
    list[0]=app->window_title;
  }
  list[1]=NULL;

  XStringListToTextProperty(list, 1, &windowName);
  XStringListToTextProperty(list, 1, &iconName);

  wm_hints->input = True;
  wm_hints->initial_state = NormalState;
  wm_hints->flags |= StateHint | InputHint;
  class_hints.res_name = myname;
  class_hints.res_class = app->class;

  if (argv) {
    if (mcw->app->flags & MCAPP_ICONIC) {
      wm_hints->initial_state = IconicState;
    }
    if (app->geometry_flags & (WidthValue|HeightValue)) {
      sizehints->flags |= USSize;
    } else {
      sizehints->flags |= PSize;
    }

    if (app->geometry_flags & (XValue|YValue)) {
      sizehints->flags |= USPosition|PWinGravity;
      sizehints->x=app->x;
      sizehints->y=app->y;

      if ((app->geometry_flags&(XNegative|YNegative))==(XNegative|YNegative)) {
	sizehints->win_gravity=SouthEastGravity;
      } else if (app->geometry_flags&XNegative) {
	sizehints->win_gravity=NorthEastGravity;
      } else if (app->geometry_flags&YNegative) {
	sizehints->win_gravity=SouthWestGravity;
      } else {
	sizehints->win_gravity=NorthWestGravity;
      }
    }
  }

  sizehints->width = mcw->w;
  sizehints->height= mcw->h;

  if (!(sizehints->flags & PBaseSize)) {
    sizehints->base_height = sizehints->base_width = 0;
    sizehints->flags |= PBaseSize;
  }
  if (!(sizehints->flags & PResizeInc)) {
    sizehints->width_inc   = sizehints->height_inc = 1;
    sizehints->flags |= PResizeInc;
  }

  if (!app->mainWindow) {
    app->mainWindow = mcw->framewin;
  }
  wm_hints->window_group=app->mainWindow;
  wm_hints->flags|=WindowGroupHint;

  if (app->icon_geometry_flags & (XValue|YValue)) {
    wm_hints->icon_x=app->ico_x;
    wm_hints->icon_y=app->ico_y;
    wm_hints->flags|=IconPositionHint;
  }

  XSetWMProperties(app->display, mcw->framewin, &windowName, &iconName,
		   app->argv_save, app->argc_save,
		   sizehints, wm_hints, &class_hints);

  XFree(windowName.value);
  XFree(iconName.value);

  if (mcw->eventCallback) {
    if (!app->wmDelWin)
      app->wmDelWin = XInternAtom(app->display,"WM_DELETE_WINDOW",False);
    XSetWMProtocols(app->display, mcw->framewin, &app->wmDelWin, True);
  }

  if (!app->McToolName) { /* This is the first window */
    app->McToolName = XInternAtom(app->display, "MCTOOL_NAME", 0);
    *list=app->class;
    XStringListToTextProperty(list, 1, &tprop);
    XSetTextProperty(app->display, mcw->framewin, &tprop, app->McToolName);
    XFree(tprop.value);
  }

  if (app->argv_save) {
    free(app->argv_save);
    app->argv_save=NULL;
    app->argc_save=0;
  }
}

/****************************************************************************
 * Titles...
 */

void McSetTitle(McWindow *mcw, string_t title) {
  XTextProperty tprop;
  char *list[2];

  list[0]=title; list[1]=NULL;
  XStringListToTextProperty(list, 1, &tprop);
  XSetWMName(mcw->app->display, mcw->framewin, &tprop);
  XFree(tprop.value);
}

/****************************************************************************
 * This and that...
 */

static string_t Name2Class(string_t str, string_t buf) {
  strcpy(buf, str);
  *buf=toupper(*buf);
  if ((*buf=='X') && (strlen(buf)>1))
    buf[1]=toupper(buf[1]);
  return buf;
}
