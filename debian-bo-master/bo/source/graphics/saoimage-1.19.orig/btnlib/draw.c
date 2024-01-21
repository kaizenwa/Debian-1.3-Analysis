#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	draw.c (Draw Button)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Draw buttons to server under various circumstances
 * Subroutine:	btn_DrawButton()		returns: void
 * Subroutine:	btn_PutImage()			returns: void
 * Subroutine:	btn_DelightButtons()		returns: void
 * Subroutine:	static btn_DimButtons()		returns: void
 * Subroutine:	btn_RelightButtons()		returns: void
 * Subroutine:	static btn_LightButtons()	returns: void
 * Xlib calls:	XPutImage()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		31 March 1989
 *		{1} Rick Schumeyer (Naval Research Lab)            8 May 1991
 * Fixed button problem on Alliant.  The buttons do not display correctly on
 * the Alliant in the XYBitmap format.  If the -button option is given, I copy
 * each button image from XYBitmap to ZPixmap, and display the ZPixmap.  The
 * variable bad_buttons is set in ../cmdparse.c.
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include <X11/Xlib.h>	/* needed for Buttons.h */
#include "buttons.h"

#ifdef ALLIANT
/* If bad_buttons=0, then draw XYBitmap (normal).  If bad_buttons=1, then draw
 * ZPixmap.  bad_buttons will be set to 1 if -button is given on command line.
 */
int bad_buttons=0;
XImage *zimage[250];
int nbutton=0;
int init_button=1;   /* =1 if buttons need to be initialized */
int button_first=1;
void put_z_image();
#endif

/*
 * Subroutine:	btn_DrawButton
 * Purpose:	Draw button according to its status
 * Returns:	void
 * Called by:	
 * Uses:	btn_PutImage() below
 * Xlib calls:	none
 */
void btn_DrawButton ( button )
     ButtonRecord *button;
{
  void btn_PutImage();

  if( button->highlight ) {
    if( button->occupied )
      btn_PutImage(button, ON_IN);
    else
      btn_PutImage(button, ON_OUT);
  } else {
    if( button->occupied )
      btn_PutImage(button, OFF_IN);
    else
      btn_PutImage(button, OFF_OUT);
  }
  return;
}

/*
 * Subroutine:	btn_PutImage
 * Purpose:	Draw button in given phase
 * Returns:	void
 * Called by:	btn_DrawButton() above
 * Xlib calls:	XPutImage()
 * Pre-state:	XImages for button and a GC must have been prepared before
 */
void btn_PutImage ( button, phase )
     ButtonRecord *button;
     int phase;
{
#ifdef ALLIANT
    if (bad_buttons)
      put_z_image(button, phase);
    else
#endif
  XPutImage(button->display, button->wndwID, button->gc, button->image[phase],
	    0, 0, 0, 0, button->width, button->height);
}

/*
 * Subroutine:	btn_DelightButtons
 * Purpose:	Make all highlighted buttons at this level of submenu
 *		unhighlighted (used with While and Co-mode buttons to supress
 *		some buttons while highlighting a specific one)
 * Returns:	void
 * Uses:	btn_DimButtons() below
 * Called by:
 * Xlib calls:	none
 * Method:	Dim this box and all co-menu boxes
 */
void btn_DelightButtons ( buttonbox )
     ButtonBox buttonbox;
{
  int i;
  static void btn_DimButtons();

  btn_DimButtons(buttonbox);
  for( i=0; i < buttonbox->co_menu_count; i++ ) {
    btn_DimButtons(buttonbox->co_menu[i]);
  }
}


/*
 * Subroutine:	btn_DimButtons
 * Purpose:	Make all highlighted buttons in this box unhighlighted except
 *		the one presently being selected (used with While and Co-mode
 *		buttons to supress all but selected button)
 * Returns:	void
 * Called by:	btn_DelightButtons() above
 * Uses:	btn_PutImage() above
 * Xlib calls:	none
 * Method:	Unhighlight any buttons currently highlighted, except the one
 *		presently being pressed
 */
static void btn_DimButtons ( buttonbox )
     ButtonBox buttonbox;
{
  int i;
  void btn_PutImage();

  for( i = 0; i < buttonbox->btn_cnt; i++ ) {
    if( (buttonbox->buttons[i].highlight) &&
	(buttonbox->down_btn != i) ) {
      buttonbox->buttons[i].highlight = 0;
      btn_PutImage(&buttonbox->buttons[i], OFF_OUT);
    }
  }
}


/*
 * Subroutine:	btn_RelightButtons
 * Purpose:	Make all selected buttons at this level of submenu highlighted
 *		(used to restore default condition after btn_DelightButtons())
 * Returns:	void
 * Called by:	
 * Uses:	btn_LightButtons() below
 * Xlib calls:	none
 */
void btn_RelightButtons ( buttonbox )
     ButtonBox buttonbox;
{
  int i;
  static void btn_LightButtons();

  btn_LightButtons(buttonbox);
  for( i=0; i < buttonbox->co_menu_count; i++ ) {
    btn_LightButtons(buttonbox->co_menu[i] );
  }
}

/*
 * Subroutine:	btn_LightButtons
 * Purpose:	Make all select buttons in this box highlighted (used to
 *		restore default condition after btn_DimButtons())
 * Returns:	void
 * Called by:	btn_RelightButtons() above
 * Uses:	btn_PutImage() above
 * Xlib calls:	none
 */
static void btn_LightButtons ( buttonbox )
     ButtonBox buttonbox;
{
  int i;
  void btn_PutImage();

  for( i = 0; i < buttonbox->btn_cnt; i++ ) {
    if( buttonbox->buttons[i].selected ) {
      buttonbox->buttons[i].highlight = 1;
      if( buttonbox->buttons[i].occupied )
	btn_PutImage(&buttonbox->buttons[i], ON_IN);
      else
	btn_PutImage(&buttonbox->buttons[i], ON_OUT);
    }
  }
}


#ifdef ALLIANT
/*
 * Subroutine: put_z_image
 */
void put_z_image(button, phase)
     ButtonRecord *button;
     int phase;
{
    char *d;
    int i,j,l;
    void init_z_image();

    if (init_button)
      init_z_image();

    if (zimage[button->id[phase]]==NULL) {
        d = (char *)malloc(button->width*button->height);
        zimage[button->id[phase]] = XCreateImage
          (button->display, DefaultVisual(button->display,0),
           8, ZPixmap, 0, d, button->width, button->height, 8, 0);

        for (i=0;i<button->height;i++)
          for (j=0;j<button->width;j++){
              l = XGetPixel(button->image[phase],j,i);
              XPutPixel(zimage[button->id[phase]],j,i,
                        (l?BlackPixel(button->display,0):
                         WhitePixel(button->display,0)));
          }
    }
    XPutImage(button->display, button->wndwID, button->gc, 
              zimage[button->id[phase]], 0, 0, 0, 0, button->width,
              button->height);
}
/*
 * Subroutine: init_z_image
 */
void init_z_image()
{
    int i;

    for (i=0; i<250; i++) {
        if ((zimage[i] != NULL)&&(!button_first))
          XDestroyImage(zimage[i]);
        zimage[i]=NULL;
    }

    nbutton=0;
    init_button=0;
    button_first=0;
}
#endif
