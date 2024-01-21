/****************************************************************************
** FILE: callbacks.c
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

#include "xbmbrowser.h"

void 
set_name(widget, event)
/* This function is added to the notify callback on all the 
** menuButtons so that the global 'bitmap_info' contains the most 
** reciently selected bitmap information line. This is then used
** in  popup_user_menu() when a menu is requested to set the other
** information about the file the pointed to widget represents.
*/
  Widget   widget;
  XEvent  *event;
{
  current_item = (Item *)GetInfoPtr( widget );
  XtVaSetValues( label, XtNlabel, (XtPointer)current_item->info, NULL );
}


void 
set_label(widget, event)
/* As the pointer leaves a window -- set the label widget to point to
** file counts and/or some error string stored in the label_info string.
*/
  Widget    widget;
  XEvent   *event;
{
  XtVaSetValues(label, XtNlabel, (XtArgVal)label_info, NULL);
  /* current_item = NULL; */
}


void 
rescan(widget, client_data, call_data )
/* User pressed the rescan button -- so do it
** NOTE: no arguments used
*/
  Widget    widget;
  XtPointer client_data, call_data;
{
  set_busywait();
  rescan_images();
  clear_busywait();
}


void 
scan(widget, client_data, call_data )
/* User pressed the scan button -- so do it
** NOTE: no arguments used
*/
  Widget    widget;
  XtPointer client_data, call_data;
{
  set_busywait();
  scan_images();
  clear_busywait();
}


void 
dir_return(widget, event)
/* Event:- User pressed return in directory widget.
** Try to change directory to the dirwidget contents
** NOTE: Arguments not used.
*/
  Widget     widget;
  XEvent    *event;
{
  set_busywait();
  change_dir( XawDialogGetValueString(dirwidget) );
  clear_busywait();
}


void 
dir_menu(widget, client_data, call_data )
/* User selected a directory from the directory menu.
** Try to change dir to item selected by the user.
** NOTE: List widget call back data required.
*/
  Widget    widget;
  XtPointer client_data, call_data;
{
  XawListReturnStruct *dir = (XawListReturnStruct *)call_data;

  set_busywait();
  change_dir( dir->string );
  clear_busywait();
}


void
pos_dir(widget, event)
/* Event:- User is tring to popup directory menu.
** This procedure positions the menu under the cursour
*/
  Widget        widget;
  XButtonEvent *event;
{
  Position w;

  XtVaGetValues(dirmenu, XtNwidth, &w, NULL);
  XtVaSetValues(dirmenu, XtNx, event->x_root - w/2,
                         XtNy, event->y_root, NULL);
}


void
toggle_option(widget, client_data, call_data )
/* toggle the options in the options menu.
** NOTE: the client data is a pointer to the Boolean value which
** is to be toggled.
*/
  Widget     widget;
  XtPointer  client_data, call_data;
{
  Boolean *value = client_data;

  set_busywait();

  *value = !*value;  /* toggle the user option */
  XtVaSetValues(widget, XtNleftBitmap, *value ? tickon : tickoff, NULL);

  if ( value == &app_data.solid_bgnd )
    set_stipple();             /* Add or remove the stipple pattern */

  if (   value == &app_data.shape_syms
      || value == &app_data.solid_bgnd ) 
    redisplay_images(False);   /* Display style change -- FAST (dont unmap) */
  else
  if (   value == &app_data.label_all
      || value == &app_data.label_icons
      || value == &app_data.label_syms )
    redisplay_images(True);    /* Display style change -- FAST (with unmap) */
  else
  if ( value == &app_data.recursive )
    rescan_images();           /* Rescan directory and merge in changes */
  else
  /* if ( vaule == &app_data.show_{...} ) */
    reassign_images();         /* just reassign -- dont re-read directory */

  clear_busywait();
}


void
popup_user_menu(widget, event)
/* Initialize the substitution variables, figure out which menu is
** to be popped up, and if required popup the user defined menu.
** or other action required by the situation.
**
** NOTE: This routine must be registered with XtRegisterGrabAction()
** for it to work properly. See final note on the MenuButton widget
** in the Xaw Interface Guide.     --- Anthony Thyssen   2 May 1994
*/
  Widget        widget;
  XButtonEvent  *event;  /* this may client data */
{
  Widget         popup_menu = NULL;   /* menu to position and popup */

  /* Assume that no item has been selected */ 
  file_name[0] = '\0';
  base_name[0] = '\0';
  suffix[0]    = '\0';

  if ( widget == mainmenu ) {
    /* User pushed the main menu button */
    popup_menu = menu_main;
  }
  else if ( widget == iconbox && (event->subwindow == None) ) {
    /* Button press inside the icon box itself (NOT a icon)
    ** NOTE: the test for subwindow insurse that we really are in the
    ** iconbox window and not a sub-window of iconbox EG: a dispayed icon
    ** or symbol. This is needed because XtRegisterGrabAction() is searched
    ** from the top down and not the bottom up.
    */
    popup_menu = menu_global;
  }
  else if ( current_item != NULL ) {
    /* OK button press was within a displayed icon or symbol
    ** So grab the file name being represented by that widget
    */
    strcpy(file_name, current_item->fname ); 
    
    switch( event->button ) {
    case Button1:
      if ( IsDirItem(current_item) ) {
        /* CD into the directory selected */
	set_busywait();
	change_dir(file_name);   /* WARNING: change_dir() modifies string */
	clear_busywait();        /*          do not pass item->fname direct */
	return;          /* done -- don't continue */
      } /* fall through if NOT a directory */
    case Button2:
      /* popup global menu */
      popup_menu = menu_global;
      break;
    case Button3:
    case Button4:
    case Button5:
      /* popup appropiate menu for the file type */
      switch( current_item->type ) {
      case Xbm:     popup_menu = menu_bitmap;    break;
      case Xpm:
      case XpmBad:  popup_menu = menu_pixmap;    break;
      case Dir:
      case DirUp:
      case DirLink:
      case DirBad:  popup_menu = menu_directory; break;
      default:      popup_menu = menu_other;     break;
      }
    }
  }

  /* Menu Determined -- Do the popup */
  if( popup_menu != NULL ) {
    char      *s;    /* where the suffix starts in file_name */

    /* now complete the substitution list if file_name set */
    if ( file_name[0] != '\0' ) {
      /* extract the basename and suffix of the filename */
      s = rindex(file_name, '.');   /* find split point */
      if ( s == file_name || s == NULL ) {  
	/* The suffix is the whole filename OR non-existant */
	strcpy(base_name, file_name);  /* base_name = file_name */
	suffix[0] = '\0';              /* suffix = "" */
      } else {
	/* A valid split point was found -- split file_name */
	int len = s - file_name;
	strncpy(base_name, file_name,  len );  /* base_name is upto suffix */
	base_name[len] = '\0';
	strcpy(suffix, s);                   /* suffix (includes `.') */
      }
    }

    /* width is invalid unless realised -- so do so */
    XtRealizeWidget(popup_menu);

    /* position the menu as appropriate */
    if ( XtIsSubclass( widget, commandWidgetClass ) ) {
      /* Position under the button pushed -- no event given */
      Position    x, y, h;
      XtVaGetValues(widget, XtNheight, &h, NULL);
      XtTranslateCoords(widget, 0, h+2, &x, &y );
      XtVaSetValues(popup_menu, XtNx, x, XtNy, y, NULL);
    }
    else {
      /* Position relative to the users pointer */
      Dimension   w;    /* width of this menu */
      XtVaGetValues(popup_menu, XtNwidth, &w, NULL); /* get width */
      XtVaSetValues(popup_menu, XtNx, event->x_root - w/2,
				XtNy, event->y_root, NULL);
    } 
    /* OK now popup the menu -- and let user-menu take it from here */
    XtPopupSpringLoaded(popup_menu);

  } else {
    /* no menu found -- beep user */
    XBell( event->display, 0 );
  }
}

