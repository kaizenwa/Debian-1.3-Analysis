/*
*****************************************************************************
** xbmbrowser version 5.0  (c) Copyright Ashley Roll, all rights reserved.
** FILE: xbmbrowser.h 
**
** Copyright transfered to  Anthony Thyssen
**
** xbmbrowser is Public Domain. However it, and all the code still belong to me.
** I do, however grant permission for you to freely copy and distribute it on 
** the condition that this and all other copyright notices remain unchanged in 
** all distributions.
**
** This software comes with NO warranty whatsoever. I therefore take no
** responsibility for any damages, losses or problems that the program may 
** cause.
*****************************************************************************
*/

#include <X11/Xos.h>           /* X operating system includes */
#include <X11/Xlib.h>          /* X programming includes */
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>      
#include <X11/StringDefs.h>

#include <stdio.h>             /* C library includes */
#include <dirent.h>            /* Filename length (and dir functions) */
#include <assert.h>            /* debugging assertions */

#include <X11/Xaw/Form.h>      /* widget includes */
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Dialog.h> 
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Viewport.h>

#include "IconLabel.h"         /* Special Widget -- IconLabel */

#ifdef DO_XPMS
# include <xpm.h>
#endif

/***************************************************************************
    SITE SPECIFIC STUFF - EDIT TO MATCH YOUR SETUP
 ***************************************************************************/
/*
** The full path to the default menu configuration file
*/
#ifndef LIBRARY_RC
#define LIBRARY_RC "/usr/lib/X11/xbmbrowser/xbmbrowser.menu"
#endif

/*
** The name of the users menu (rc) file to find in users home
*/
#ifndef USERS_RC
#define USERS_RC ".xbmbrowserrc"
#endif

/***************************************************************************
    PROGRAM STUFF - CHANGE IT AT YOUR OWN RISK
 ***************************************************************************/

/***** Type Definitions *****/

/* the various types of file we deal with */
enum FileType {
  Unknown,                       /* Unknown, Special Files */
  Dir, DirUp, DirLink, DirBad,   /* Directory types */
  /* NOTE: Non-File == (type < File) */
  File, Text, Binary,    /* File types  ( File = undetermined file ) */
  Xbm, Xpm, XpmBad,      /* Image files ( XpmBad is a filed load ) */
  NumFileTypes           /* Count of file types */
};

/* item structure */
typedef struct _item {
  struct _item     *next;               /* next item */
  char              fname[MAXNAMLEN];   /* This files name in current dir */
  char              info[MAXNAMLEN+40]; /* information to display about icon */
  time_t            mtime;              /* last modification time */
  enum FileType     type;               /* file type */
  Boolean           visible;            /* visible to user? */
  int               index;              /* widget the pixmap is currently in */
  Pixmap            pixmap;             /* pixmap from this file - to widget */
#ifdef DO_XPMS
  Pixmap            mask;               /* the xpm mask bitmap */
  XpmAttributes     attr;               /* xpm attributes */
#endif
} Item;

/* a couple of macros to make life easier */
#define IsDirItem(item)  ( (item)->type != Unknown && (item)->type < File )
#define IsFileItem(item) ( (item)->type >= File)

/* resource data structure */
typedef struct {
  char    *cmd_rc;         /* the config file given on command line */
  char    *user_rc;        /* the user's rc file in his/her home dir */
  char    *library_rc;     /* the library menu configuration file */
  /* --- option menu --- */
  /* Display Style */
  Boolean  solid_bgnd;     /* solid color and shaped windows on/off */
  Boolean  shape_syms;     /* shape the file symbols */
  Boolean  label_all;      /* add labels to all displayed widgets */
  Boolean  label_icons;    /* add labels to icons only */
  Boolean  label_syms;     /* add labels to symbols only */
  Boolean  label_dirs;     /* add labels to direcories only */
  /* Show What */
  Boolean  icons_only;     /* display icons only  */
  Boolean  show_dir;       /* show these file symbols ( unless icons_only ) */
  Boolean  show_xpmbad;
  Boolean  show_other;
  Boolean  show_hidden;
  /* Scan Style */
  Boolean  recursive;      /* recursive directory scan -- watch it */
  /* --- colors --- */
  Pixel    sym_fg;         /* foreground for file symbols */
  Pixel    sym_bg;         /* background for file symbols */
  Pixel    icon_fg;        /* foreground for bitmap icons */
  Pixel    icon_bg;        /* background for bitmap icons */
  Pixel    icon_tr;        /* transparent color for pixmaps (unshaped) */
  Pixel    solid_bg;       /* solid background color (shaped bgnd) */
  Pixel    stipple_bg;     /* stipple background color */
} AppData;


/***** Variable Definitions *****/

#ifdef MAIN
#define ext
#else
#define ext extern
#endif

/* application resource structure */
ext AppData    app_data;

/* Display information */
ext Display   *display;                  /* Xlib display pointer */
ext Screen    *screen;                   /* The Screen we are on */
ext Colormap   colormap;                 /* the default colormap of screen */
ext int        depth;                    /* the depth of the display */

/* Extra Stuff we use -- shouldn't these be resources? or in app_data? */
ext Cursor     normalCursor, waitCursor; /* Cursours */
ext Pixmap     tickoff, tickon;          /* tickbox bitmaps for option menu */
ext Pixmap sym_bmaps[(int)NumFileTypes]; /* default file type icon images */
ext Pixmap sym_masks[(int)NumFileTypes]; /* To be initialized later */

/* Globally Known Widgets */
ext Widget     mainpw;                   /* main pane widget -- for cursors */
ext Widget     mainmenu, dirwidget;      /* menu button and dir_dialog */
ext Widget     dirmenu, dirlist;         /* directory list popup */
ext Widget     label, iconbox;           /* information and iconbox */
ext Widget     recur_opt;                /* recursive toggle menu option */

/* user menu widgets which may or may-not be defined by the user */
ext Widget     menu_main, menu_global;
ext Widget     menu_bitmap, menu_pixmap;
ext Widget     menu_directory, menu_other;

/* substitution strings for function argument macro substitions */
ext char     dir_name[MAXNAMLEN];     /* %d the current directory */
ext char     file_name[MAXNAMLEN];    /* %f current filename */
ext char     base_name[MAXNAMLEN];    /* %b basename for current file */
ext char     suffix[MAXNAMLEN];       /* %s suffix of current file */
ext char     input[MAXNAMLEN];        /* %i input string from user */
ext char     home_dir[MAXNAMLEN];     /* %h the users home directory */
ext char     init_dir[MAXNAMLEN];     /* %D initial startup directory */

/* Information strings for display in application label */
ext char     label_info[MAXNAMLEN];   /* label to show when outside bitmap */
ext Item    *current_item;            /* which item the user is pointing to */

/* some external variables */
extern char icon_trans[];     /* Translations to use for MenuPopup */


/***** Proceedure Definitions *****/

/* callback procedures   "callbacks.c" */
extern void    set_name();           /* set the info line with file name */
extern void    set_label();          /* reset the info line to file counts */
extern void    dir_return();         /* return key in directory string */
extern void    pos_dir();            /* position directory menu */
extern void    dir_menu();           /* menu button in directory string */
extern void    rescan();             /* rescan button callback */
extern void    scan();               /* scan button callback */
extern void    toggle_option();      /* user option toggle callback */
extern void    popup_user_menu();    /* menu popup over icon or symbol */

/* user defined menu module  "user-menu.c" */
extern void    menu_item_abort();    /* abort -- menu function sequence */
extern void    menu_item_continue(); /* continue sequence after popup end */
extern void    read_user_menu();     /* read menu configuration file */

/* user functions       "user-functs.c" */
extern void    quit_browser();       /* quit the browser */
extern void    expand_tilde();       /* expand any ~ to the users home dir */
extern void    change_dir();         /* change the directory to that given */
extern void    exec_string();        /* execute the string given */
extern void    user_confirm();       /* ask the user for confirmation */
extern void    input_string();       /* input a string from the user */
extern void    file_selected();      /* check that user has selected a file */

/* icon display routines   "images.c" */
extern Item   *alloc_item();         /* Item allocation (from misc.c) */
extern Item   *free_item();          /* free file/image item */
extern void    free_list();          /* free a list of items */
extern void    rescan_item();        /* just re-load this one item */
extern void    redisplay_images();   /* just redisplay all images - fast */
extern void    reassign_images();    /* reassign all items to widgets */
extern void    rescan_images();      /* scan for changes and reload them */
extern void    scan_images();        /* full scan of the current directory */

/* miscelanous functions  "misc.c" */
extern void    set_busywait();       /* Set the watch cursor */
extern void    clear_busywait();     /* Remove the watch cursor */
extern void    set_stipple();        /* Set/Reset the stipple background */
extern void    init_stipple();       /* Initialize the stipple color & pixmap */
extern Item   *get_files();          /* Scan a directory and initialise items */
extern time_t  check_file_time();    /* check if a file was modified */


/*********** Macros ************/

/* CAT:  Macro to concatenate two arguments into one compiler symbol
** example:   CAT(symbol,_height)  becomes  symbol_height
** Note: some sites may need to set this explicitely. If this doesn't
** work mail me which element does, and the CPP sysmbols defined for
** machine.
*/
#ifndef CAT
#  ifdef __STDC__
#    define CAT(a,b)  a##b   /* ANSI concatenate */
#  else
#    ifndef sun
#      define CAT(a,b)  a/**/b  /* Old K&R concatenate */
#    else
#      undef  IDENT
#      define IDENT(x)  x
#      define CAT(a,b)  IDENT(a)b  /* Sun concatenate */
#    endif
#  endif
#endif


/* LOAD_BMAP: Macro to create a Bitmap from the Bitmap data given.
** Assumes that the filename given is also the symbol names used within
** the included bitmap file. */
#define LOAD_BMAP(file) \
      XCreateBitmapFromData( display, RootWindowOfScreen(screen),\
              (char *)CAT(file,_bits), CAT(file,_width), CAT(file,_height) )

/* The macros to insure that symbolic links work on all systems
** have been moved into misc.c
*/

/*********************************/

