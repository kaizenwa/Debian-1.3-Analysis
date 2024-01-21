#ifndef _JED_SCREEN_H_
#define _JED_SCREEN_H_
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "window.h"

/* JED screen management routines */

/* Description: JED maintains a list of Lines which contain a pointer
 * to the line in the buffer as well as a flag which describes the line
 * as being changed, etc.. since the last update.
 * 
 * The buffer is changed only through the insertion or deletion of a
 * character.  This can have two effects:  1) It changes a single line leaving
 * other lines on the display untouched, or 2) It deletes/adds a newline
 * character which changes the current line and acuses the other lines to
 * scroll.  Hence, the insert/delete routines will have to inform the display
 * code of these changes.
 * 
 * Finally there are situations when the buffer has not changed but the screen
 * has.  This occurs when the cursor is in motion such as page up and down. For
 * this situation, the screen routines will check to see if it can scroll one
 * or two lines to bring the line back into view and if not it will scroll
 * enough to recenter.
 * 
 * Once the display code takes over it does the following:
 * 
 * 1)  Scrolls to try to bring the point into view.
 * 2)  Then it does a final touch up of the screen.
 * 
 * Note that if one fails, we just simply redraw the window.  This is more
 * efficient then looking at the lines and doing a true screen update like
 * emacs does.  Hopefully this will not be a complete loss.  I suspect it
 * will win most of the time.
 * 
 */

typedef struct Screen_Type
  {
      Line *line;               /* buffer line structure */
      int n;                    /* number of chars written last time */
      int flags;                /* line untouched, etc... */
     unsigned short *old, *neew;
     unsigned char *hi0, *hi1;	       /* beg end of hilights */
  }
Screen_Type;

#define JNORMAL_COLOR 0
#define JCURSOR_COLOR 1
#define JSTATUS_COLOR 2
#define JREGION_COLOR 3
#define JMENU_COLOR 4
#define JOP_COLOR 5
#define JNUM_COLOR 6
#define JSTR_COLOR 7
#define JCOM_COLOR 8
#define JDELIM_COLOR 9
#define JPREPROC_COLOR 10
#define JMESSAGE_COLOR 11
#define JERROR_COLOR 12
#define JDOLLAR_COLOR 13
#define JDOTS_COLOR 14
#define JKEY_COLOR 15		       /* JKEY_COLOR must be last */
#define JMAX_COLORS 128


#ifndef pc_system
#define MAX_SCREEN_SIZE 120
#else
#define MAX_SCREEN_SIZE 75
#endif

extern Screen_Type JScreen[MAX_SCREEN_SIZE];

extern int Screen_Row;		       /* cursor row */
extern int Screen_Col;		       /* cursor col */
extern int Cursor_Motion;	       /* cursor movement only */
extern int Display_Eight_Bit;	       /* if non zero, pass to terminal as is */

extern int redraw(void);
extern void recenter(int *);
extern int window_line(void);
extern void scroll_down(int, int, int);
extern int scroll_up(int, int, int);

extern void update(Line *, int, int);
extern void init_display(int);
extern void point_cursor(int);
extern void point_column(int);
extern int calculate_column(void);

extern void register_change(int);
extern void touch_window(void);
extern void blank_line(int);
extern void reset_display(void);
extern int cursor_visible(void);
extern Line *find_top(void);
extern void do_dialog(char *);
extern void jed_fillmem(char *, char, int);
extern int Goal_Column;
extern int User_Prefers_Line_Numbers;
extern int Wants_Attributes;
extern int Term_Supports_Color;
extern int Wants_Syntax_Highlight;
extern int Display_Time;
extern int Want_Eob;
extern void set_status_format(char *, int *);

extern void init_syntax_highlight (void);
extern void syntax_highlight(unsigned short *, unsigned short *);
extern int Mode_Has_Syntax_Highlight;
extern int JED_CSI;
extern int Wants_HScroll;
extern int Mini_Ghost;
extern void redraw_screen(int);
extern void define_top_screen_line(char *);

#if JED_HAS_DISPLAY_TABLE
extern unsigned char Output_Display_Table[256];
#endif

#if JED_HAS_LINE_ATTRIBUTES
extern Line *jed_find_non_hidden_line (Line *);
#endif

extern int Jed_Version_Number;
extern int Jed_Dollar;

#endif
/* #ifdef _JED_SCREEN_H_ */
