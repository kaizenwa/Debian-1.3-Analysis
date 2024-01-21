#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	editemcs.c (Editor Emacs)
 * Purpose:	Provide EMACS like response for popup line editor
 * Subroutine:	emacs_response()		returns: int
 * Xlib calls:	XLookupString()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 July 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include "hfiles/edit.h"

#define MAX_MAPPED_STRING_LENGTH 16
#define MetaMask Mod1Mask

/*
 * Subroutine:	emacs_response
 * Purpose:	Respond to keys in emacs fashion
 * Returns:	1 on event indicating entry, -1 on cancel, else 0
 * Xlib calls:	XLookupString()
 */
int emacs_response ( xkey, edit )
     XKeyEvent *xkey;
     EditStruct *edit;
{
  KeySym keysym;
  XComposeStatus compose;
  int char_cnt;
  static int escape = 0;
  static int bufsize = MAX_MAPPED_STRING_LENGTH;
  char buffer[MAX_MAPPED_STRING_LENGTH];
  void place_edit_cursor(), move_edit_cursor(), clear_edit_buf();
  void delete_chars_forward(), delete_chars_backward(), insert_chars();
  void store_edit_struct(), recall_edit_struct(), draw_new_string();
  int next_word_end(), last_word_start();

  /* decode the key event to ascii */
  char_cnt = XLookupString(xkey, buffer, bufsize, &keysym, &compose);
  /* ignore modifier key pressing */
  if( (keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R) )
    return( 0 );
  /* escape char ignores modifiers */
  if( (xkey->state & MetaMask) || escape ) {
    escape = 0;
    switch( keysym ) {
    case XK_d:
    case XK_D:
      delete_chars_forward(edit, next_word_end(edit));
      break;
    case XK_f:
    case XK_F:
      move_edit_cursor(edit, next_word_end(edit));
      break;
    case XK_b:
    case XK_B:
      move_edit_cursor(edit, last_word_start(edit));
    default:
      return( 0 );
    }
  } else if( (xkey->state & ControlMask) || (keysym >= 0xF000) ) {
    /* check for control function */
    if( (xkey->state & ControlMask) && ((keysym < XK_A) || (keysym > XK_z)) )
      return( 0 );
    if( (keysym >= 0xF000) && (xkey->state & (ControlMask | MetaMask)) )
      return( 0 );
    switch( keysym ) {
    case XK_A:
    case XK_a:
    case XK_Begin:
      place_edit_cursor(edit, 0);
      break;
    case XK_B:
    case XK_b:
    case XK_BackSpace:
    case XK_Left:
      move_edit_cursor(edit, -1);
      break;
    case XK_C:
    case XK_c:
      /* restore the string to how we started, then exit */
      recall_edit_struct(edit, 0, 1);
      return( -1 );
    case XK_D:
    case XK_d:
      delete_chars_forward(edit, 1);
      break;
    case XK_E:
    case XK_e:
    case XK_End:
      place_edit_cursor(edit, edit->char_cnt);
      break;
    case XK_F:
    case XK_f:
    case XK_Right:
      move_edit_cursor(edit, 1);
      break;
    case XK_G:
    case XK_g:
      escape = 0;
      break;
    case XK_K:
    case XK_k:
      delete_chars_forward(edit, edit->char_cnt - edit->active_position);
      break;
    case XK_P:
    case XK_p:
    case XK_Up:
    case XK_Prior:
      recall_edit_struct(edit, edit->stack_index + 1, 0);
      draw_new_string(edit, 1);
      break;
    case XK_N:
    case XK_n:
    case XK_Next:
      recall_edit_struct(edit, edit->stack_index - 1, 1);
      draw_new_string(edit, 1);
      break;
    case XK_Down:
      recall_edit_struct(edit, edit->stack_index - 1, 0);
      draw_new_string(edit, 1);
      break;
    case XK_T:
    case XK_t:
      break;
    case XK_Tab:
      break;
    case XK_Clear:
      clear_edit_buf(edit);
      draw_new_string(edit, 1);
      break;
    case XK_Linefeed:
    case XK_Return:
      if( edit->char_cnt > 0 )
	store_edit_struct(edit);
      return( 1 );
    case XK_Escape:
      escape = 1;
      break;
    case XK_Delete:
      delete_chars_backward(edit, 1);
      break;
    default:
      break;
    }
  } else if( (char_cnt > 0) && (buffer[0] >= ' ') ) {
    insert_chars(edit, buffer, char_cnt);
  }
  return( 0 );
}
