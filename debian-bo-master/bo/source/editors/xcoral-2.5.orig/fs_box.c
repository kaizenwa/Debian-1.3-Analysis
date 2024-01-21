/* ########################################################################

				fs_box.c

   File: fs_box.c
   Path: /home/fournigault/c/X11/xcoral-2.31/fs_box.c
   Description: 
   Created: Fri Jan 27 11:02:17 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:02:18 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <string.h>
#ifdef apollo
#include <sys/dir.h>
#define dirent direct
#else
#include <dirent.h>
#endif
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef apollo
#define S_ISDIR
#else
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif /* __FreeBSD__ */
#endif

#include "xcoral.h"
#include "main_text.h"
#include "options.h"
#include "fs_box.h"
#include "parse.h"
#include "main_events.h"
#include "input_str.h"
#include "shadow.h"
#include "page.h"
#include "list_box.h"

extern Display *dpy;
FBox fs_box;
static  Atom fs_del;
extern char  *getcwd();

FCT (static int, LoadFilesAndDirs, (char * dir) );
FCT (static void, MapFsBox, () );
FCT (static int, NewCompare, (char ** s1, char ** s2) );
FCT (static void, RefreshDirTitle, (char *dirname) );
FCT (static void, RefreshDirsAndFiles, () );
FCT (static void, RefreshFSTitleBox, () );
FCT (static void, UnmapFsBox, () );

static char *internal_dirname = 0;

/*
**	Function name : InitFsBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InitFsBox ()
{
    XWindowAttributes att;
    Window root;
    int screen;
    unsigned long black, white;
    XGCValues	 gcv;
    XSizeHints sizehints;
    
    black = BlackPixel ( dpy, DefaultScreen ( dpy ));
    white = WhitePixel ( dpy, DefaultScreen ( dpy ));

    fs_box.mwin = 0;
    fs_box.fg = ((DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ? 
		 black : GetOpColor ( OP_MENU_FG ));
    fs_box.bg = ((DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1) ?
		 white : GetOpColor ( OP_MENU_BG ));
    fs_box.ts = GetOpColor ( OP_MENU_TS );
    fs_box.bs = GetOpColor ( OP_MENU_BS );
    fs_box.font = LoadFont ( dpy, FSBOX_FONT );
    fs_box.gc = XCreateGC ( dpy, DefaultRootWindow ( dpy ), 0,  &gcv );
    /*XCopyGC ( dpy, DefaultGC (dpy, DefaultScreen ( dpy )), (~0), fs_box.gc );*/
    
    XSetFont ( dpy, fs_box.gc, fs_box.font -> fid );
    XSetForeground ( dpy, fs_box.gc, fs_box.fg );
    XSetBackground ( dpy, fs_box.gc, fs_box.bg );

    screen = DefaultScreen ( dpy );
    root = RootWindow ( dpy, screen );
    XGetWindowAttributes ( dpy, root, &att );
    fs_box.width = ( 10 * att.width ) / 20;
    fs_box.height = ( 10 * att.height ) / 25 ;
    fs_box.t_height = fs_box.font -> ascent + fs_box.font -> descent + FB_SPACE;
    fs_box.b_width = XTextWidth ( fs_box.font, FB_CANCEL, strlen (FB_CANCEL) ) + (2 * FB_SPACE);
    fs_box.b_height = fs_box.t_height;

    fs_box.str_x = (fs_box.b_width - (fs_box.b_width - (2 * FB_SPACE))) /2 ;
    fs_box.str_y = fs_box.font -> ascent + (FB_SPACE / 2);
    
    fs_box.frame = XCreateSimpleWindow ( dpy, root, ((att.width/2) - (fs_box.width/2)),
					  ((att.height/2) - (fs_box.height/2)),  
				       fs_box.width, fs_box.height, 0, black, black );
    fs_box.title = XCreateSimpleWindow ( dpy, fs_box.frame, 0, 0,
					  10, 10,0, fs_box.fg, fs_box.bg);
    fs_box.dir = XCreateSimpleWindow ( dpy, fs_box.frame, 0, fs_box.t_height,
					  10, 10,0, fs_box.fg, fs_box.bg);
    fs_box.dirframe = XCreateSimpleWindow ( dpy, fs_box.frame, 0, (2 *fs_box.t_height),
					 10, 10 ,0, fs_box.fg, fs_box.bg);
    fs_box.dirtitle = XCreateSimpleWindow ( dpy, fs_box.dirframe, FB_SPACE, FB_SPACE,
					 10, 10 ,0, fs_box.fg, fs_box.bg);
    fs_box.dirtext = XCreateSimpleWindow ( dpy, fs_box.dirframe, 
	FB_SPACE, ( 2* FB_SPACE ) + fs_box.t_height, 10, 10 ,0, fs_box.fg, fs_box.bg);
    
    fs_box.fileframe = XCreateSimpleWindow ( dpy, fs_box.frame, 0, (2 *fs_box.t_height),
					 10, 10 ,0, fs_box.fg, fs_box.bg);
    fs_box.filetitle = XCreateSimpleWindow ( dpy, fs_box.fileframe, FB_SPACE, FB_SPACE,
					 10, 10 ,0, fs_box.fg, fs_box.bg);
    fs_box.filetext = XCreateSimpleWindow ( dpy, fs_box.fileframe, 
	FB_SPACE, ( 2* FB_SPACE ) + fs_box.t_height, 10, 10 ,0, fs_box.fg, fs_box.bg);
    
    fs_box.ctr = XCreateSimpleWindow ( dpy, fs_box.frame, 0, 0,
	10,(2 * fs_box.t_height) + (3 * FB_SPACE),0, fs_box.fg, fs_box.bg);
    fs_box.mb_frame = XCreateSimpleWindow ( dpy, fs_box.ctr, FB_SPACE, FB_SPACE, 10,
					 10,0, fs_box.fg, fs_box.bg);
    fs_box.ok = XCreateSimpleWindow ( dpy, fs_box.ctr, 0, 0,
				       fs_box.b_width, fs_box.b_height,0, fs_box.fg, fs_box.bg);
    fs_box.cancel = XCreateSimpleWindow ( dpy, fs_box.ctr, 0, 0,
					   fs_box.b_width, fs_box.b_height,0, fs_box.fg, fs_box.bg);
    
    sizehints.flags = USPosition | PPosition | PSize | PMinSize | PMaxSize;

    sizehints.width = fs_box.width;
    sizehints.height = fs_box.height;
    sizehints.min_width = fs_box.width / 2;
    sizehints.min_height = fs_box.height / 2;
    sizehints.max_width = ( 3 * att.width ) / 4;
    sizehints.max_height = ( 3 * att.height ) / 4;
    sizehints.x = ((att.width/2) - (fs_box.width/2));
    sizehints.y = ((att.height/2) - (fs_box.height/2));
   
    XSetWMProperties ( dpy, fs_box.frame, 0, 0, 0, 0, &sizehints, 0, 0 );
    fs_del = XInternAtom( dpy, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols ( dpy, fs_box.frame, &fs_del, 1);
    
    XSelectInput ( dpy, fs_box.frame, StructureNotifyMask | KeyPressMask );
    XSelectInput ( dpy, fs_box.title, ExposureMask );
    XSelectInput ( dpy, fs_box.dir, ExposureMask );
    XSelectInput ( dpy, fs_box.dirframe, ExposureMask );
    XSelectInput ( dpy, fs_box.dirtitle, ExposureMask );
    XSelectInput ( dpy, fs_box.dirtext, ExposureMask );
    XSelectInput ( dpy, fs_box.fileframe, ExposureMask );
    XSelectInput ( dpy, fs_box.filetitle, ExposureMask );
    XSelectInput ( dpy, fs_box.filetext, ExposureMask );
    XSelectInput ( dpy, fs_box.mb_frame, ExposureMask );
    XSelectInput ( dpy, fs_box.ctr, ExposureMask );
    XSelectInput ( dpy, fs_box.ok, ExposureMask | ButtonPressMask );
    XSelectInput ( dpy, fs_box.cancel, ExposureMask | ButtonPressMask );
    
    fs_box.text_dir = ( Text * ) MakeTextWindow ( dpy, fs_box.dirtext, FB_SPACE, FB_SPACE );
    fs_box.scroll_dir = ( SWin  * ) MakeScroll ( dpy, fs_box.dirtext, FB_SPACE, FB_SPACE );
    fs_box.text_dir -> swin = fs_box.scroll_dir;
    fs_box.scroll_dir -> text = (char *) fs_box.text_dir;
    fs_box.text_dir -> mwin = 0;
    fs_box.buf_dir = (Buf *) GetBuffer ( (unsigned) SIZEOF_BUFFER );
    fs_box.text_dir -> buf = fs_box.buf_dir;

    fs_box.text_file = ( Text * ) MakeTextWindow ( dpy, fs_box.filetext, FB_SPACE, FB_SPACE );
    fs_box.scroll_file = ( SWin  * ) MakeScroll ( dpy, fs_box.filetext,  FB_SPACE, FB_SPACE );
    fs_box.text_file -> swin = fs_box.scroll_file;
    fs_box.scroll_file -> text = (char *) fs_box.text_file;
    fs_box.text_file -> mwin = 0;
    fs_box.buf_file = (Buf *) GetBuffer ( (unsigned) SIZEOF_BUFFER );
    fs_box.text_file -> buf = fs_box.buf_file;
    
    fs_box.stat = FB_UNMAP;
    fs_box.select_dir = 0;
    fs_box.select_file = 0;
    fs_box.dirname = 0;
    fs_box.click_time = 0;
    fs_box.old_click = 0;
    
    XStoreName ( dpy, fs_box.frame, "Xcoral File Selector" );
}


/*
**	Function name : MapFsBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void MapFsBox ()
{

  XMapSubwindows ( dpy, fs_box.ctr );
  XMapWindow ( dpy, fs_box.ctr );
    
  XMapSubwindows ( dpy, fs_box.dirframe );
  XMapWindow ( dpy, fs_box.dirframe );

  XMapSubwindows ( dpy, fs_box.fileframe );
  XMapWindow ( dpy, fs_box.fileframe );

  XMapSubwindows ( dpy, fs_box.frame );
  XMapRaised ( dpy, fs_box.frame );
}

/*
**	Function name : UnmapFsBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void UnmapFsBox ()
{
  
    KillText ( dpy, fs_box.text_dir );
    KillText ( dpy, fs_box.text_file );
    fs_box.select_dir = 0;
    fs_box.select_file = 0;
    if (internal_dirname) {
	free(internal_dirname);
	internal_dirname = 0;
    }

    XUnmapWindow ( dpy, fs_box.frame );
}

/*
**	Function name : SelectFileFromBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
char *SelectFileFromBox ( msg )
    char *msg;
{
    char *s = 0;
    static char full_path_select [256];
 
    FreezeMenus ();
    if ( msg != 0 ) {
	if ( strlen ( msg ) > (TITLE_SIZE - 2) ) {
	    (void) strncpy ( fs_box.title_text, msg, TITLE_SIZE -1 );
	    fs_box.title_text [TITLE_SIZE] = '\0';
	}
	else
	  (void) strcpy ( fs_box.title_text, msg );
    }
    
    (void) LoadFilesAndDirs ( "." );
    
    if ( fs_box.stat != FB_MAP ) {
	(void) MapFsBox ();
	WaitForMapped ( fs_box.frame, False );
	fs_box.stat = FB_MAP;	
    }
    else
      XMapRaised ( dpy, fs_box.frame );
    
    s = (char *) InputString ( fs_box.mb_frame,
			      fs_box.gc, fs_box.font, "File name : " , 0 );
    if ( (s != 0) && (*s == '~') )
      s = ExpandTildeName ( s );
    (void) UnmapFsBox ();

    if ( (s != 0) && (*s != '\007') && (*s != '/') ) {
	if ( (*s == '.') && (strlen(s) == 1) )
	  (void) sprintf ( full_path_select, "%s", fs_box.dirname );
	else
	  (void) sprintf ( full_path_select, "%s/%s", fs_box.dirname, s );
	UnFreezeMenus ();
	return ( full_path_select );
    }
    else {
	/* Cas ou s est nul, ou commence par slash, ou Ctrl g */
	UnFreezeMenus ();      
	return (s);
    }
}
	

/*
**	Function name : RefreshTitleBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void RefreshFSTitleBox ()
{
    int width;
    
    XClearWindow ( dpy, fs_box.title );
    width = XTextWidth ( fs_box.font, fs_box.title_text, strlen( fs_box.title_text));
    if ( width > ( fs_box.width - ( 2 * FB_SPACE )))
	(void) strcpy ( fs_box.title_text, "<< >>" );

    XDrawString ( dpy, fs_box.title, fs_box.gc,
		 fs_box.text_dir -> x_or,
		 fs_box.str_y,
		 fs_box.title_text, strlen( fs_box.title_text));
    
    Display3D ( dpy, fs_box.title, fs_box.ts, fs_box.bs, 1, 0 );
}

/*
**	Function name : NewCompare
**
**	Description : Compare  2 chaines
**	Input : 2 tableaux de chaines
**	Ouput : Vrai si s1 = s2 Faux sinon
*/
static int NewCompare ( s1, s2 )
	char **s1, **s2;
{
	return ( strcmp ( *s1, *s2 ));
}

/*
**	Function name : LoadFilesAndDirs
**
**	Description : 
**	Input : 
**	Ouput :
*/
static int LoadFilesAndDirs ( dir )
    char *dir;
{
  DIR *dirp;
  struct dirent *dp;
  int n;
  char **tmp_dir, **tmp_file, *pathname, *tmp_stat;
  int ndir, nfile;
  int dir_len = 0;
  int tmp_len;
  int dir_str_len = strlen ( dir );
  struct stat st;
  
  ndir = 0;
  nfile = 0;
  dirp = opendir( dir );
  if ( dirp == 0 ) {
      char *failed = (char *)malloc (strlen(dir)+20);
      sprintf(failed,"%s : opendir failed\n", dir);
      DisplayWMessage (failed, "Open directory", 0);
      free(failed);
      return -1;
  }
 
  ClearBuffer ( fs_box.buf_dir );
  ClearBuffer ( fs_box.buf_file );

  WatchOn ( fs_box.frame );
  for (dp = readdir(dirp); dp != 0; dp = readdir(dirp)) 
    dir_len ++;
  (void) closedir ( dirp );
  dirp = opendir( dir );
  tmp_dir = (char **) malloc ( (unsigned) ( sizeof (char *) * (dir_len + 2)));
  tmp_file = (char **) malloc ( (unsigned) ( sizeof (char *) * (dir_len + 2)));  
  
  for (dp = readdir(dirp); dp != 0; dp = readdir(dirp)) {
    if ( (dp->d_name != 0) && ((tmp_len = strlen(dp->d_name)) != 0) ) {
      tmp_stat = (char *) malloc ( (unsigned) ( tmp_len + dir_str_len + 2 ));
      (void) sprintf ( tmp_stat, "%s/%s", dir, dp->d_name );
      if ( stat ( tmp_stat, &st ) != 0 ) {
/*	perror ("Error stat" ); */
	continue;
      }
      (void) free ( tmp_stat );
      if ( S_ISDIR ( st.st_mode )) {
	tmp_dir [ndir] = (char *) malloc ( (unsigned) strlen (dp->d_name) + 2);
	(void) strcpy ( (char *) tmp_dir[ndir], dp->d_name );
	ndir ++;
      }
      else {
	tmp_file [nfile] = (char *) malloc ( (unsigned) strlen (dp->d_name) + 2);
	(void) strcpy ( (char *) tmp_file [nfile], dp->d_name );
	nfile ++;
      }
    }
  }
  qsort ( (char *) tmp_dir, ndir, sizeof ( char *), NewCompare ); 
  qsort ( (char *) tmp_file, nfile, sizeof ( char *), NewCompare ); 
  
  for ( n = 0; n < ndir; n++ ) {
    InsertNchar ( fs_box.buf_dir, (char *) tmp_dir [n], 
		 strlen ((char *) tmp_dir [n] ));
    if ( tmp_dir[n] != 0 )
      (void) free ( (char *) tmp_dir[n] );
    InsertNchar ( fs_box.buf_dir, "\n", 1 );
  }
  for ( n = 0; n < nfile; n++ ) {
    InsertNchar ( fs_box.buf_file, (char *) tmp_file [n], 
		 strlen ((char *) tmp_file [n] ));
    if ( tmp_file[n] != 0 )
      (void) free ( (char *) tmp_file[n] );
    InsertNchar ( fs_box.buf_file, "\n", 1 );
  }
  
  if ( tmp_dir != 0 )
    (void) free ( (char *) tmp_dir );

  if ( tmp_file != 0 )
    (void) free ( (char *) tmp_file );
  
  fs_box.text_dir -> modif = True;
  (void) strcpy ( (char *) fs_box.text_dir -> filename, "NoName" );
  n = GetNumberOfLineInBuf ( fs_box.text_dir -> buf );
  fs_box.text_dir -> lines_in_buf = n;
  SetScrollLine ( fs_box.text_dir -> swin, n );
  HoleToLeft ( fs_box.buf_dir );

  fs_box.text_file -> modif = True;
  (void) strcpy ( (char *) fs_box.text_file -> filename, "NoName" );
  n = GetNumberOfLineInBuf ( fs_box.text_file -> buf );
  fs_box.text_file -> lines_in_buf = n;
  SetScrollLine ( fs_box.text_file -> swin, n );
  HoleToLeft ( fs_box.buf_file );

  if ( fs_box.dirname != 0 )
    (void) free ( fs_box.dirname );

  pathname = (char *) malloc ( (unsigned) MAXPATHLEN + 2 );
  (void) getcwd ( (char*) pathname, MAXPATHLEN );
  if ( strcmp ( dir, "." ) == 0 ) {
    fs_box.dirname = ( char *) malloc ( (unsigned) strlen (pathname) +2 );
    (void) strcpy ( fs_box.dirname, (char *) pathname );
  }
  else {
    fs_box.dirname = ( char *) malloc ( (unsigned) strlen (dir) +2 );
    (void) strcpy ( fs_box.dirname, dir );
  }
  if ( pathname != 0 )
    (void)  free ( pathname );
  
  (void) closedir ( dirp );
  WatchOff ( fs_box.frame );
  return 0;
  
}

/*
**	Function name : ConfigFsBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ConfigFsBox ( width, height )
    int width, height;
{
    static int fs_first_conf = True;
    int df_width, df_height, bx, by, m_height;

    if ( (width == fs_box.width) 
	&& (height == fs_box.height) 
	&& (fs_first_conf == False )) {
	return;
    }
    else
	fs_first_conf = False;
    
    fs_box.width = width;
    fs_box.height = height;
    df_width = fs_box.width / 2;
    df_height = fs_box.height - ( 4 * fs_box.t_height ) - ( 3 * FB_SPACE);
    by = ( 2 * FB_SPACE ) + fs_box.t_height;
    bx = (( fs_box.width / 2 ) - fs_box.b_width ) / 2;

    XResizeWindow ( dpy, fs_box.frame, fs_box.width, fs_box.height );
    XResizeWindow ( dpy, fs_box.title, fs_box.width, fs_box.t_height );
    XResizeWindow ( dpy, fs_box.dir, fs_box.width, fs_box.t_height );
    XResizeWindow ( dpy, fs_box.dirframe, df_width, df_height );
    XResizeWindow ( dpy, fs_box.fileframe, fs_box.width - df_width, df_height );
    XResizeWindow ( dpy, fs_box.dirtitle, df_width - ( 2 * FB_SPACE ), fs_box.t_height );
    XResizeWindow ( dpy, fs_box.filetitle, df_width - ( 2 * FB_SPACE ), fs_box.t_height );
    XResizeWindow ( dpy, fs_box.dirtext, df_width - ( 2 * FB_SPACE ), 
		   df_height - ( 3 * FB_SPACE ) - fs_box.t_height );	   
    XResizeWindow ( dpy, fs_box.filetext, df_width - ( 2 * FB_SPACE ), 
		   df_height - ( 3 * FB_SPACE ) - fs_box.t_height );	   

    XMoveWindow ( dpy, fs_box.fileframe, fs_box.width - df_width, ( 2 * fs_box.t_height ));

    XResizeWindow ( dpy, fs_box.ctr, fs_box.width, ( 2 * fs_box.t_height ) + (3 * FB_SPACE));
    XMoveWindow ( dpy, fs_box.ctr, 0, ( 2 * fs_box.t_height ) + df_height );
    
    XResizeWindow ( dpy, fs_box.mb_frame, fs_box.width - ( 2 * FB_SPACE), fs_box.t_height );
    XMoveWindow ( dpy, fs_box.ok, bx, by );
    XMoveWindow ( dpy, fs_box.cancel, ( 3 * bx ) + fs_box.b_width, by );

    m_height = df_height - ( 3 * FB_SPACE ) - fs_box.t_height;
    ConfigTextAndScroll ( fs_box.text_dir, 
	 df_width - (2 *FB_SPACE), m_height, FB_SPACE );
    ConfigTextAndScroll ( fs_box.text_file,
	 df_width - (2 *FB_SPACE), m_height, FB_SPACE );
}

/*
**	Function name : ButtonFsBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ButtonFsBox ( ev )
    XButtonEvent *ev;
{
  int result;
  char *text_item;
  int new_select;
  
  if ( ev -> window == fs_box.cancel) {
    Display3D ( dpy, fs_box.cancel, fs_box.ts, fs_box.bs, 1, 1 );
    fs_box.select_dir = 0;
    fs_box.select_file = 0;
    SetCancelButton ();
    return True;
  }
  if ( ev -> window == fs_box.ok) {
    Display3D ( dpy, fs_box.ok, fs_box.ts, fs_box.bs, 1, 1 );
    SetOkButton ();
    fs_box.select_dir = 0;
    fs_box.select_file = 0;
    return True;
  }
  if ( ButtonPressInScroll ( fs_box.scroll_dir, 
	    ev -> window, ev -> y, &result )) {
    RunScrollAndUpdateItem ( fs_box.text_dir, fs_box.select_dir, result );
    return True;
  }
  if ( ButtonPressInScroll ( fs_box.scroll_file, 
	    ev -> window, ev -> y, &result )) {
    RunScrollAndUpdateItem ( fs_box.text_file, fs_box.select_file, result );
    return True;
  }
  if ( ev -> window == fs_box.text_dir -> window ) {
    fs_box.click_time = ev -> time;
    if ( DoubleClick (fs_box.click_time, &fs_box.old_click) == True ) {
      if ( fs_box.select_dir != 0 ) {
	(void) RefreshDirsAndFiles ();
	(void) RefreshDirTitle ( fs_box.dirname );
	(void) EmptyMiniBuffer ();
      }
    }
    else {
      new_select = SelectTextItem ( fs_box.text_dir,
		  ev -> x, ev -> y, fs_box.select_dir );
      if ( new_select ) {
	fs_box.select_dir = new_select;
	text_item = (char *) CurrentTextItem ( fs_box.text_dir );
	if ( text_item ) {
	  FillMiniBuffer ( text_item );
	  (void) free ( text_item );
	}
	fs_box.old_click = fs_box.click_time;
      }
      else {
	fs_box.old_click = 0;
      }
    }
    return True;
  }
  if ( ev -> window == fs_box.text_file -> window ) {
    fs_box.click_time = ev -> time;
    if ( DoubleClick (fs_box.click_time, &fs_box.old_click) == True ) {
      if ( fs_box.select_file != 0 ) {
	fs_box.select_dir = 0;
	fs_box.select_file = 0;
	SetOkButton ();
      }
    }
    else {
      new_select = SelectTextItem ( fs_box.text_file,
		      ev -> x, ev -> y, fs_box.select_file );
      if ( new_select ) {
	fs_box.select_file = new_select;
	text_item = (char *) CurrentTextItem ( fs_box.text_file );
	if ( text_item ) {
	  FillMiniBuffer ( text_item );
	  (void) free ( text_item );
	}
	fs_box.old_click = fs_box.click_time;
      }
      else {
	fs_box.old_click = 0;
      }
    }
    return True;
  }
  return False;
}

/*
**	Function name : GetFsBoxInternalDir
**
**	Description :
**	Input :
**	Output :
*/
char *GetFsBoxInternalDir()
{
    return (internal_dirname);
}


/*
**	Function name : RefreshDirTitle
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void RefreshDirTitle ( dirname )
    char *dirname;
{
  int start, width;
  char *old = dirname;
  
  if(dirname) {
      if (internal_dirname)
	free(internal_dirname);
      internal_dirname = (char *) malloc(strlen(dirname) + 3);
      strcpy(internal_dirname, dirname);
	if ( *internal_dirname == '/'  && strlen(internal_dirname) != 1) { /*pas la racine */
		internal_dirname[strlen(dirname)] = '/';	
		internal_dirname[strlen(dirname)+1] = '\0';	
	}
  }
  
  XClearWindow ( dpy, fs_box.dir );
  start = fs_box.text_dir -> x_or + XTextWidth ( fs_box.font, "Dir : ", 6 ) + 3;

  width = fs_box.width - ( 2 * FB_SPACE ) - start 
    - fs_box.text_dir -> x_or - XTextWidth ( fs_box.font, "...", 3 );
  while ( XTextWidth ( fs_box.font, dirname, strlen(dirname)) > width ) 
    dirname ++;

  XDrawString ( dpy, fs_box.dir, fs_box.gc,
	       fs_box.text_dir -> x_or, fs_box.font -> ascent + 5, "Dir : ", 6 );
  if ( old != dirname ) {
    XDrawString ( dpy, fs_box.dir,
		 fs_box.gc, start, fs_box.font -> ascent + 5, "...", 3 );
    XDrawString ( dpy, fs_box.dir, fs_box.gc,
		 start + XTextWidth ( fs_box.font, "...", 3 ), 
		 fs_box.font -> ascent + 5, dirname, strlen(dirname) );		
  }
  else
    XDrawString ( dpy, fs_box.dir, fs_box.gc,
		 start, fs_box.font -> ascent + 5, dirname, strlen(dirname) );
  Display3D ( dpy, fs_box.dir, fs_box.ts, fs_box.bs, 1, 0 );
  
}

/*
**	Function name : RefreshDirsAndFiles
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void RefreshDirsAndFiles ()
{
  char *s, *name_select, *tmp, *end_name;
  int len;
  struct stat st;

  s = (char * ) GetCurrentLine ( fs_box.buf_dir, &len );
  name_select = (char *) malloc ( (unsigned) len + 2 );
  (void) strncpy ( name_select, s, len);
  name_select [len] = '\0';

  len = strlen(fs_box.dirname) + strlen(name_select);
  tmp = (char *) malloc ( (unsigned) len + 2 ); 
  (void) sprintf ( tmp, "%s/%s", fs_box.dirname, name_select );

  if ( (strcmp ( tmp, "/.." ) == 0) || (strcmp ( name_select, ".") == 0) ) {
    if ( tmp != 0 )
      (void) free (tmp);
    if ( name_select != 0 )
      (void) free (name_select);
    return;
  }
 
  if ( stat ( tmp, &st ) != 0 ) 
    return;
  if ( S_ISDIR(st.st_mode) ) {
    if ( strcmp (name_select, "..") == 0 ) {
      end_name = (char * ) strrchr ( (char *) fs_box.dirname, '/' );
      len = strlen(fs_box.dirname) - strlen(end_name);
      tmp [len] = 0;
      if ( len == 0 ) {	/* Root */
	(void) strncpy ( tmp, "/", 1 );
	tmp [1] = 0;
      }
    }
    else {
      if ( strcmp (fs_box.dirname, "/") == 0 )
	(void) sprintf ( tmp, "%s%s", fs_box.dirname, name_select );
      else 
	(void) sprintf (tmp, "%s/%s", fs_box.dirname, name_select ); 
    }
    if ( LoadFilesAndDirs ( tmp ) == 0 ) {
      UpdateTextItem ( fs_box.text_dir, fs_box.select_dir ); 
      UpdateTextItem ( fs_box.text_file, fs_box.select_file );       
      fs_box.select_dir = 0;
      fs_box.select_file = 0;
      FirstPage ( fs_box.text_dir );
      FirstPage ( fs_box.text_file );
      RefreshPageAndUpdateScroll ( fs_box.text_dir );
      RefreshPageAndUpdateScroll ( fs_box.text_file );
    }
  }
  if ( name_select != 0 )
    (void) free (name_select); 
  if ( tmp != 0 )
    (void) free (tmp);
}

/*
**	Function name : ExposeFsBox
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ExposeFsBox ( ev )
    XEvent *ev;
{
  int x;
  
  if ( ev -> xexpose.window == fs_box.title ) {
    (void) RefreshFSTitleBox ();
    return True;
  }
  
  if ( ev -> xexpose.window == fs_box.dir ) {
    (void) RefreshDirTitle ( fs_box.dirname );
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.dirframe ) {
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.dirtitle ) {
    x = (((fs_box.width) / 2) - ( 2 * FB_SPACE) - 
	 XTextWidth ( fs_box.font, FB_DIR, strlen(FB_DIR))) / 2;
    XDrawString ( dpy, fs_box.dirtitle, fs_box.gc, x, fs_box.str_y, FB_DIR, strlen(FB_DIR));
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }
  
  if ( ev -> xexpose.window == fs_box.dirtext ) {
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.scroll_dir -> frame ) {
    RefreshScrollFrame ( dpy, fs_box.scroll_dir );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.scroll_dir -> scroll ) {
    RefreshScrollBar ( dpy, fs_box.scroll_dir );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.fileframe ) {
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }
	
  if ( ev -> xexpose.window == fs_box.filetitle ) {
    x = (((fs_box.width) / 2) - ( 2 * FB_SPACE) - 
	 XTextWidth ( fs_box.font, FB_FILES, strlen(FB_FILES))) / 2;
    XDrawString ( dpy, fs_box.filetitle, fs_box.gc, x, fs_box.str_y, FB_FILES, strlen(FB_FILES));
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.filetext ) {
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.scroll_file -> frame ) {
    RefreshScrollFrame ( dpy, fs_box.scroll_file );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.scroll_file -> scroll ) {
    RefreshScrollBar ( dpy, fs_box.scroll_file );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.ctr ) {
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.ok ) {
    x = (fs_box.b_width - XTextWidth ( fs_box.font, FB_OK, strlen(FB_OK))) / 2;
    XDrawString ( dpy, fs_box.ok, fs_box.gc, x, fs_box.str_y, FB_OK, strlen(FB_OK));
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.cancel ) {
    x = (fs_box.b_width - XTextWidth ( fs_box.font, FB_CANCEL, strlen(FB_CANCEL))) / 2;
    XDrawString ( dpy, fs_box.cancel, fs_box.gc, x, fs_box.str_y, FB_CANCEL, strlen(FB_CANCEL));
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }
  
  if ( ev -> xexpose.window == fs_box.mb_frame ) {
    Display3D ( dpy, ev -> xexpose.window, fs_box.ts, fs_box.bs, 1, 0 );
    return True;
  }

  if ( ev -> xexpose.window == fs_box.text_dir -> window ) {
    RefreshPageAndUpdateScroll ( fs_box.text_dir );
    UpdateTextItem ( fs_box.text_dir, fs_box.select_dir );	
    XFlush ( dpy );
    return True;
  }
  if ( ev -> xexpose.window == fs_box.text_file -> window ) {
    RefreshPageAndUpdateScroll ( fs_box.text_file );
    UpdateTextItem ( fs_box.text_file, fs_box.select_file );
    XFlush ( dpy );
    return True;
  }
  
  return False;
}

