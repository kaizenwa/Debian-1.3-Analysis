char copyright[]=
"xbmbrowser Version 5.0 (c) Copyright Ashley Roll and Anthony Thyssen.";
/*
*****************************************************************************
** FILE: xbmbrowser.c
**
** xbmbrowser is Public Domain. However it, and all the code still belong to me.
** I do, however grant permission for you to freely copy and distribute it on 
** the condition that this and all other copyright notices remain unchanged in 
** all distributions.
**
** This software comes with NO warranty whatsoever. I therefore take no
** responsibility for any damages, losses or problems that the program may 
** cause.
**                                     Anthony Thyssen and Ashley Roll
*****************************************************************************
*/

#define MAIN
#include "xbmbrowser.h"
#include "patchlevel.h"

/* application icon */
#include "icon.xbm"

/* Tick Box Symbols */
#include "tickbox_off.xbm"
#include "tickbox_on.xbm"

/* File Type Symbols */
#include "filesyms/unknown.xbm"
#include "filesyms/dir.xbm"
#include "filesyms/dirup.xbm"
#include "filesyms/dirlink.xbm"
#include "filesyms/dirbad.xbm"
#include "filesyms/file.xbm"
#include "filesyms/text.xbm"
#include "filesyms/binary.xbm"
#include "filesyms/xbm.xbm"  /* Xbm and Xpm do not really need these symbols */
#include "filesyms/xpm.xbm"  /* but are included for completeness, who knows!*/
#include "filesyms/xpmbad.xbm"

/* mask bitmaps for file symbols */
#include "filesyms/dirmask.xbm"
#include "filesyms/filemask.xbm"

/* Define the width to give the main buttons.
** This width is reduced by the form widget to fit the
** applications real width, but sound be the same for all
** the buttons defined in the button box. */
#define  BUTTON_WIDTH 100

/* -------------------------- */
static Atom  wm_delete_window;      /* insure that delete window works */

/* define command line options to change resource settings */
static XrmOptionDescRec cmdline_options[] = {
  /* option      resource      option_type      value */
  /* command line menu configuration file */
  {"-config",      "*cmd_rc",       XrmoptionSepArg,  NULL },
  {"-cf",          "*cmd_rc",       XrmoptionSepArg,  NULL },
  /* symbol resources */
  {"-iconsonly",   "*icons_only",   XrmoptionNoArg,  "True"},
  {"-dir",         "*show_dir",     XrmoptionNoArg,  "True"},
  {"-xpmbad",      "*show_xpmbad",  XrmoptionNoArg,  "True"},
  {"-other",       "*show_other",   XrmoptionNoArg,  "True"},
  /* symbol resources (inverted) */
  {"-noiconsonly", "*icons_only",   XrmoptionNoArg,  "False"},
  {"-nodir",       "*show_dir",     XrmoptionNoArg,  "False"},
  {"-noxpmbad",    "*show_xpmbad",  XrmoptionNoArg,  "False"},
  {"-noother",     "*show_other",   XrmoptionNoArg,  "False"},
  /* special resources toggles */
  {"-solid",       "*solid_bgnd",   XrmoptionNoArg,  "True"},
  {"-stipple",     "*solid_bgnd",   XrmoptionNoArg,  "False"},
  {"-label",       "*label_all",    XrmoptionNoArg,  "True"},
  {"-nolabel",     "*label_all",    XrmoptionNoArg,  "False"},
  /* recursive directory scan */
  {"-R",           "*recursive",    XrmoptionNoArg,  "True"},
  {"-recursive",   "*recursive",    XrmoptionNoArg,  "True"},
};

/* Unknown is a boolean! value which means it was never set
** by any user resource. Used to calculate a correct default
** later in the main initialization section. */
#define Bool_Unset (Boolean)2

/* define the applications own resources */
#define offset(field)  XtOffsetOf(AppData, field)
static XtResource resources[] = {
  /* res_name  res_class  res_type  var_size 
   *   var_ptr   var_type   default_value   */
/* --- configuration resources --- */
  { "cmd_rc",       "Config",   XtRString, sizeof(char *),
       offset(cmd_rc), XtRString, (XtPointer) NULL },
  { "user_rc",      "Config",   XtRString, sizeof(char *),
       offset(user_rc), XtRString, (XtPointer) USERS_RC },
  { "library_rc",   "Config",   XtRString, sizeof(char *),
       offset(library_rc), XtRString, (XtPointer) LIBRARY_RC },
/* --- option menu resources --- */
  /* display style resources */
  { "solid_bgnd",   XtCBoolean,  XtRBoolean, sizeof(Boolean),
       offset(solid_bgnd), XtRImmediate,  (XtPointer) Bool_Unset },
  { "shape_syms",   XtCBoolean,  XtRBoolean, sizeof(Boolean),
       offset(shape_syms), XtRImmediate,  (XtPointer) Bool_Unset },
  { "label_all",   XtCBoolean,  XtRBoolean, sizeof(Boolean),
       offset(label_all), XtRImmediate,   (XtPointer) False },
  { "label_icons",   XtCBoolean,  XtRBoolean, sizeof(Boolean),
       offset(label_icons), XtRImmediate, (XtPointer) False },
  { "label_syms",   XtCBoolean,  XtRBoolean, sizeof(Boolean),
       offset(label_syms), XtRImmediate,  (XtPointer) False },
  { "label_dirs",   XtCBoolean,  XtRBoolean, sizeof(Boolean),
       offset(label_dirs), XtRImmediate,  (XtPointer) False },
  /* visibility resources */
  { "icons_only",   "Show",     XtRBoolean, sizeof(Boolean),
       offset(icons_only), XtRImmediate,  (XtPointer) False },
  { "show_dir",     "Show",     XtRBoolean, sizeof(Boolean),
       offset(show_dir), XtRImmediate,    (XtPointer) True },
  { "show_xpmbad",  "Show",     XtRBoolean, sizeof(Boolean),
       offset(show_xpmbad), XtRImmediate, (XtPointer) True },
  { "show_other",   "Show",     XtRBoolean, sizeof(Boolean),
       offset(show_other), XtRImmediate,  (XtPointer) False },
  { "show_hidden",  "Show",     XtRBoolean, sizeof(Boolean),
       offset(show_hidden), XtRImmediate, (XtPointer) False },
  /* recursive scan resource */
  { "recursive",   XtCBoolean,  XtRBoolean, sizeof(Boolean),
       offset(recursive), XtRImmediate,   (XtPointer) False },
/* --- Colors --- */
  /* symbols and bitmap icon colors */
  { "sym_foreground",  XtCForeground,  XtRPixel, sizeof(Pixel),
       offset(sym_fg), XtRString, (XtPointer) XtDefaultForeground },
  { "sym_background",  XtCBackground,  XtRPixel, sizeof(Pixel),
       offset(sym_bg), XtRString, (XtPointer) XtDefaultBackground },
  { "icon_foreground",  XtCForeground,  XtRPixel, sizeof(Pixel),
       offset(icon_fg), XtRString, (XtPointer) XtDefaultForeground },
  { "icon_background",  XtCBackground,  XtRPixel, sizeof(Pixel),
       offset(icon_bg), XtRString, (XtPointer) XtDefaultBackground },
  /* background colors */
  { "icon_transparent",  XtCBackground,  XtRPixel, sizeof(Pixel),
       offset(icon_tr), XtRString, (XtPointer) XtDefaultBackground },
  { "solid_background",  XtCBackground,  XtRPixel, sizeof(Pixel),
       offset(solid_bg), XtRString, (XtPointer) XtDefaultBackground },
  { "stipple_background",  XtCBackground,  XtRPixel, sizeof(Pixel),
       offset(stipple_bg), XtRString, (XtPointer) XtDefaultBackground },
};
#undef offset

/* declare action routines for translations */
static XtActionsRec  actions[] = {
/* action_name    routine */
  { "Dir_Return", dir_return },      /* directory dialog return */
  { "Set_Name",   set_name },        /* set info line for this file */
  { "Set_Label",  set_label },       /* set general default information */
  { "Pos_Dir",    pos_dir },         /* Set dir menu's location */
  { "Pop_Menu",   popup_user_menu }, /* popup user menu (or cd to subdir) */
  { "Quit",       quit_browser },    /* quit application */
};

/* translation table (override) for menu (command) widget */
static char menu_trans[] =
  "<EnterWindow>:  highlight() \n\
   <LeaveWindow>:  reset() \n\
   <BtnDown>:      Pop_Menu() \n\
   <BtnDown>(2+): ";  /* this stops double clicks crashing the program */

/* translation table for widget displaying the icon images */
char icon_trans[] = 
  "<EnterWindow>:  Set_Name() \n\
   <LeaveWindow>:  Set_Label() \n\
   <BtnDown>:      Pop_Menu() \n\
   <BtnDown>(2+): ";  /* this stops double clicks crashing the program */

/* translation table (override) for "global" popup anywhere in the iconbox */
static char box_trans[] =
  "<BtnDown>:      Pop_Menu() \n\
   <BtnDown>(2+): ";  /* this stops double clicks crashing the program */

/* translation table (override) for the directory text widget */
static char text_trans[] = 
  "<Key>Return:  Dir_Return() \n\
   Ctrl<Key>M:   Dir_Return() \n\
   <Btn3Down>:   Pos_Dir() MenuPopup(dirmenu) ";

/* translation table for the directory list popup menu */
static char list_trans[] =
   "<Enter>:      Set() \n\
    <Leave>:      Unset() \n\
    <BtnMotion>:  Set() \n\
    <BtnUp>:      MenuPopdown(dirmenu) Notify() Unset()"; 

/* translation table for the transient shell containing the list
** This translation makes the list popdown if the button is released 
** outside the window    -- Ashley Roll */
static char tshell_trans[] = 
       "<BtnUp>: MenuPopdown(dirmenu)";


/* fallback resources -- things required to function at all */
static char *fall_back[] = {
   "XbmBrowser.width:    500",
   "XbmBrowser.height:   600",
   "*optmenu*leftMargin: 24", 
   NULL
}; 

/* -------------------------- */

static void usage()
{
  fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s",
    "Usage: xbmbrowser [options...] [directory]\n",
    "    Where ``directory'' is an directory of icons to display.\n",
    "    Options can be any standard X toolkit option or...\n",
    "       -config file     Use the given config file instead\n",
    "       -solid           Use a solid background and shaped windows\n",
    "       -stipple         Stripple background and unshaped icons\n",
    "       -(no)iconsonly   (Don't) Display only the icon images\n",
    "       -(no)dir         (Don't) Display directories found\n",
    "       -(no)xpmbad      (Don't) Display Bad X Pixmap Loads\n",
    "       -(no)other       (Don't) Display other files\n",
    "       -(no)label       (Don't) Label the displayed icon images\n",
    "       -R | -recursive  Recursive Scan of all sub-directories\n"
  );
  exit(1);
}


main(argc, argv)
  int argc;
  char **argv;
{
  XtAppContext appcon;    /* these item only really needed in here */
  Widget       toplevel;  /* the application widget -- only needed in here */

  /* Setup the toplevel window */
  { char title[80];

    (void) sprintf(title,"XbmBrowser Version %s",PATCHLEVEL);
    toplevel = XtVaAppInitialize(
              &appcon, "XbmBrowser",       /* app context, ClassName */
              cmdline_options,             /* app command line options */
              XtNumber(cmdline_options),
              &argc, argv,                 /* command line */
              fall_back,                   /* Fall back resources */
              XtNtitle, (XtArgVal)title,
              NULL);                       /* End Va resource list */

    XtVaGetApplicationResources(
              toplevel, &app_data, resources, XtNumber(resources),
              /* varargs of non-user-defined resources here */
              NULL );

    XtAppAddActions( appcon, actions, XtNumber(actions) );
  }

  /* collect some extra information */
  display  = XtDisplay(toplevel);
  screen   = XtScreen(toplevel);
  colormap = DefaultColormapOfScreen(screen);
  depth    = DefaultDepthOfScreen(screen);

#if 0
  /* Color Tests of the display -- Pixmap Debugging */
  printf("icon: fore=%d, back=%d, trans=%d\n",
     app_data.icon_fore, app_data.icon_back, app_data.icon_trans );
#endif

  /* If no icon is set by the user -- set a default one */
  { Pixmap icon;

    XtVaGetValues(toplevel, XtNiconPixmap, &icon, NULL);
    if ( icon == None ) {
      icon = LOAD_BMAP(icon);
      XtVaSetValues(toplevel, XtNiconPixmap, icon, NULL);
    }
  }

  /* locate the users home directory, and save this for future use */
  /* required to find rc files where to find user's rc file */
  { extern char *getenv();
    (void) strcpy(home_dir, getenv("HOME"));
    if( strcmp(home_dir,"/") != 0 )
      (void) strncat(home_dir, "/", MAXNAMLEN);
  }

  /* Initialize other substitution strings */
  dir_name[0]  = '\0';
  file_name[0] = '\0';
  base_name[0] = '\0';
  suffix[0]    = '\0';
  input[0]     = '\0';

  /* process any other options leftover -- presumably a directory to cd to */
  { Boolean opts_done = FALSE, errored = FALSE;

    while( argc >= 2 && argv[1][0] == '-' && !opts_done ) {
      if( strcmp(argv[1], "--") == 0 )
	opts_done = TRUE;         /* "--" end of options */
      else {
	fprintf(stderr, "Unknown Option: %s\n", argv[1]);
        
      }
      argv++; argc--;
    }
    if( errored ) usage();
    if( argc > 2 ) {
      fprintf(stderr, "Too many arguments found.\n");
      usage();
    }
    if(argc == 2) {      /* any other argument is a directory name */
      strcpy(dir_name, argv[1]);  /* don't chdir() yet! */
    }
  }

  /* Initialize user menus from the appropiate RC file */
  /* Do this before we chdir() into some other directory */
  { FILE *rc;
    char rcfile[MAXNAMLEN];

     menu_global = menu_bitmap = menu_pixmap =
        menu_directory = menu_other = NULL;

    /* if a config file is on command line use it */
    if( app_data.cmd_rc != NULL
        && (rc = fopen(app_data.cmd_rc, "r")) != (FILE *)NULL  ) {
      read_user_menus(toplevel, rc, app_data.cmd_rc);
      fclose(rc);
    }
    else if( app_data.cmd_rc != NULL ) {
      /* We failed to open a config file given on the command line! */
      fprintf(stderr, "xbmbrowser: Cound not open -config \"%s\"\n",
                    app_data.cmd_rc );
      exit(-1);
    }
    /* try to find the config file in users home */
    else if( app_data.user_rc != NULL &&
             ( sprintf(rcfile, "%s%s", home_dir, app_data.user_rc),
               (rc = fopen(rcfile, "r")) != (FILE *)NULL ) ) {
      read_user_menus(toplevel, rc, rcfile);
      fclose(rc);
    }
    /* try the library default config file */
    else if( app_data.library_rc != NULL
             && (rc = fopen(app_data.library_rc, "r")) != (FILE *)NULL  ) {
      read_user_menus(toplevel, rc, app_data.library_rc);
      fclose(rc);
    }
    else {  /* unable to find and load any config file! */
      fprintf(stderr,"xbmbrowser: Can not find a menu configuration file\n"); 
      exit(-1);
    }
  }

  /* CD and Initialize dir_name correctly
  ** DO NOT call change_dir() now as this will reset the current
  ** resursive resource setting given by the user on the command line.
  */
  if( dir_name[0] != '\0' && chdir(dir_name) != 0 ) {
    fprintf(stderr,"xbmbrowser: couldn't chdir to '%s'\n",argv[1]);
    exit(-1);
  }
  (void) getcwd(dir_name, MAXNAMLEN);
  if( strcmp(dir_name,"/") != 0 )
    (void) strncat(dir_name, "/", MAXNAMLEN);

  /* copy this as the initial startup directory */
  strcpy(init_dir, dir_name);

  /* create the cursors */
  normalCursor = XCreateFontCursor(display, XC_left_ptr);
  waitCursor   = XCreateFontCursor(display, XC_watch);

  /* NOTE: LOAD_BMAP(file) is a marco to creat a X bitmap from bitmap data */

  /* create tickbox bitmaps for options menu */
  tickoff = LOAD_BMAP(tickbox_off);
  tickon  = LOAD_BMAP(tickbox_on);

  /* load masks first and save them */
  sym_masks[Dir] = sym_masks[DirUp] = sym_masks[DirLink] =
     sym_masks[DirBad] = LOAD_BMAP(dirmask);
  sym_masks[File] = sym_masks[Text] = sym_masks[Binary] =
     sym_masks[Xbm] = sym_masks[Xpm] = sym_masks[XpmBad] =
     sym_masks[Unknown] = LOAD_BMAP(filemask);

  /* Load and save the rest of the file symbols */
  sym_bmaps[Unknown] = LOAD_BMAP(unknown);
  sym_bmaps[Dir]     = LOAD_BMAP(dir);
  sym_bmaps[DirUp]   = LOAD_BMAP(dirup);
  sym_bmaps[DirLink] = LOAD_BMAP(dirlink);
  sym_bmaps[DirBad]  = LOAD_BMAP(dirbad);
  sym_bmaps[File]    = LOAD_BMAP(file);
  sym_bmaps[Text]    = LOAD_BMAP(text);
  sym_bmaps[Binary]  = LOAD_BMAP(binary);
  sym_bmaps[Xbm]     = LOAD_BMAP(xbm);
  sym_bmaps[Xpm]     = LOAD_BMAP(xpm);
  sym_bmaps[XpmBad]  = LOAD_BMAP(xpmbad);


  /* create a paned widget to put everything into */
  mainpw  = XtVaCreateManagedWidget(
              "main", panedWidgetClass, toplevel,
              XtNcursor,(XtArgVal)normalCursor,  /* the default cursour */
              NULL);

  /* create buttons and options menu */
  { Widget form, button, options, optmenu, optitem;

    /* create form to hold the buttons */
    form = XtVaCreateManagedWidget(
              "buttons", formWidgetClass, mainpw,
              XtNshowGrip,   (XtArgVal)False,
              XtNskipAdjust, (XtArgVal)True,NULL);

    /* create the Buttons */
    mainmenu = XtVaCreateManagedWidget(
              "mainmenu", commandWidgetClass, form,
              XtNwidth,      (XtArgVal)BUTTON_WIDTH,
              NULL);
    XtOverrideTranslations(mainmenu, XtParseTranslationTable(menu_trans));
    options = XtVaCreateManagedWidget(
              "options", menuButtonWidgetClass, form,
              XtNwidth,      (XtArgVal)BUTTON_WIDTH,
              XtNfromHoriz,  (XtArgVal)mainmenu,
              XtNmenuName,   (XtArgVal)"optmenu",
              NULL);
    button = XtVaCreateManagedWidget(
              "rescan", commandWidgetClass, form,
              XtNwidth,      (XtArgVal)BUTTON_WIDTH,
              XtNfromHoriz,  (XtArgVal)options,
              NULL);
    XtAddCallback(button, XtNcallback, rescan, NULL);

    button = XtVaCreateManagedWidget(
              "scan", commandWidgetClass, form,
              XtNwidth,      (XtArgVal)BUTTON_WIDTH,
              XtNfromHoriz,  (XtArgVal)button,
              NULL);
    XtAddCallback(button, XtNcallback, scan, NULL);

    /* initialize the solid_bgnd resource is still unset */
    if ( app_data.solid_bgnd == Bool_Unset )
      app_data.solid_bgnd = (depth > 1);   /* true if on color display */

    if ( app_data.shape_syms == Bool_Unset )
      app_data.shape_syms = app_data.solid_bgnd;  /* true if on color display */

    /* create the options menu */
    optmenu = XtVaCreatePopupShell(
              "optmenu", simpleMenuWidgetClass, options,
              NULL);
    /* ----- display style options ----- */
    optitem = XtVaCreateManagedWidget(
              "solid_bgnd", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.solid_bgnd ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.solid_bgnd);
    optitem = XtVaCreateManagedWidget(
              "shape_syms", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.shape_syms ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.shape_syms);
    optitem = XtVaCreateManagedWidget(
              "label_all", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.label_all ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.label_all);
    optitem = XtVaCreateManagedWidget(
              "label_icons", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.label_icons ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.label_icons);
    optitem = XtVaCreateManagedWidget(
              "label_syms", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.label_syms ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.label_syms);
    optitem = XtVaCreateManagedWidget(
              "label_dirs", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.label_dirs ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.label_dirs);

    /* ----- what is visible options ----- */
    XtVaCreateManagedWidget("line", smeLineObjectClass, optmenu, NULL);
    optitem = XtVaCreateManagedWidget(
              "icons_only", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.icons_only ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.icons_only);
    optitem = XtVaCreateManagedWidget(
              "show_dir", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.show_dir ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.show_dir);
    optitem = XtVaCreateManagedWidget(
              "show_xpmbad", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.show_xpmbad ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.show_xpmbad);
    optitem = XtVaCreateManagedWidget(
              "show_other", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.show_other ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.show_other);
    optitem = XtVaCreateManagedWidget(
              "show_hidden", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.show_hidden ? tickon : tickoff, NULL);
    XtAddCallback(optitem, XtNcallback, toggle_option, &app_data.show_hidden);

    /* ----- recursive scaning of directories ----- */
    /* this is globally known so change_dir() can reset it */
    XtVaCreateManagedWidget("line", smeLineObjectClass, optmenu, NULL);
    recur_opt = XtVaCreateManagedWidget(
              "recursive", smeBSBObjectClass, optmenu,
              XtNleftBitmap, app_data.recursive ? tickon : tickoff, NULL);
    XtAddCallback(recur_opt, XtNcallback, toggle_option, &app_data.recursive);
  }


  /* dialogWidget -- show and change the current directory
  ** NOTE: the actual value for this widget set in change_dir() 
  */
  dirwidget = XtVaCreateManagedWidget(
              "directory", dialogWidgetClass, mainpw,
              XtNlabel,      (XtArgVal)"Current Directory",
              XtNvalue,      (XtArgVal)dir_name, /* current directory */
              XtNshowGrip,   (XtArgVal)False,
              XtNskipAdjust, (XtArgVal)True,
              NULL);
  XtOverrideTranslations(XtNameToWidget(dirwidget,"value"),
              XtParseTranslationTable(text_trans));

  /* create the dirMenu list widget in a popup shell. */
  dirmenu = XtVaCreatePopupShell(
              "dirmenu", transientShellWidgetClass, dirwidget,
              XtNoverrideRedirect, (XtArgVal)True,
              XtNallowShellResize, (XtArgVal)True,
              NULL);
  XtOverrideTranslations(dirmenu, XtParseTranslationTable(tshell_trans));

  dirlist = XtVaCreateManagedWidget(
              "dirlist",listWidgetClass,dirmenu,
              XtNforceColumns,    True,   /* set the defaults */
              XtNdefaultColumns,  1,      /* changed later */
              NULL);
  XtAddCallback(dirlist, XtNcallback, dir_menu, NULL);
  XtOverrideTranslations(dirlist, XtParseTranslationTable(list_trans));

  /* label widget to hold information about the current bitmap */
  strcpy( label_info, "Loading Bitmaps" );   /* initialize label_info */
  label = XtVaCreateManagedWidget("label",labelWidgetClass,mainpw,
              XtNlabel,      (XtArgVal)label_info,
              XtNshowGrip,   (XtArgVal)False,
              XtNskipAdjust, (XtArgVal)True,NULL);


  /* setup the box to hold all the images */
  { Widget  viewport;
    int     width;

    XtVaGetValues(toplevel,XtNwidth,&width,NULL);
 
    /* create a viewport widget to stick the images in */
    viewport = XtVaCreateManagedWidget(
              "viewport",viewportWidgetClass,mainpw,
              XtNwidth,(XtArgVal)width,
              XtNallowHoriz,(XtArgVal)False,
              XtNallowVert,(XtArgVal)True,
              XtNshowGrip,(XtArgVal)False,
              XtNskipAdjust,(XtArgVal)True,NULL);

    /* create the box widget to put all the icon images in */
    iconbox = XtVaCreateManagedWidget(
              "iconbox",      boxWidgetClass,  viewport,
              XtNwidth,       width,
              XtNorientation, (XtArgVal)XtorientVertical,
              XtNfromVert,    (XtArgVal)label,
              NULL);
    /* This is currently NOT working -- It stops the file popups!
    ** All I want is for the global menu to popup on mouse press anywhere
    ** in the iconbox, but for the normal actions to continue to work
    ** when mouse button is used on a displays icon or symbol.
    ** This overrides everything inside the iconbox. Arrgghhh. */
    XtOverrideTranslations(iconbox, XtParseTranslationTable(box_trans));
  }

  /* initialize and set the stipple for the icon box */
  init_stipple(); 

  /* WARNING :- Heavy Magic in progress, Xperts please note....
  **   We need to Register the appropiate Grab Information for the
  ** popup_user_menu action routine. This is only needed as we are
  ** NOT using a MenuButton widget, or the builtin MenuPopup() action
  ** routine to do the actual popup. It is done this way to allow the
  ** application to pick the appropriate menu to popup or do a chdir(),
  ** depending on the type of object the widget is currently
  ** representing.
  **                                 --- Anthony Thyssen   2 May 1994
  */
  XtRegisterGrabAction(popup_user_menu, True,
              ButtonPressMask | ButtonReleaseMask,
              GrabModeAsync, GrabModeAsync);

  /* scan for the images -- DON'T call change_dir() */
  scan_images();

  XtRealizeWidget(toplevel);

  /* Set the window to call quit() action if `deleted' */
  XtOverrideTranslations(toplevel,     
              XtParseTranslationTable("<Message>WM_PROTOCOLS:Quit()") );
  wm_delete_window = XInternAtom(display, "WM_DELETE_WINDOW", False);
  (void) XSetWMProtocols(display, XtWindow(toplevel), &wm_delete_window, 1);

  /* start the main event loop */
  XtAppMainLoop(appcon);
}
