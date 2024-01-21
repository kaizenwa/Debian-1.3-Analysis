/*
 * File:	editwin.h
 * Purpose:	Declarations and definitions for the editing window.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: editwin.h,v 1.15 1996/12/06 13:15:30 liw Exp $"
 */
 
#ifndef editwin_h
#define editwin_h

#include <publib.h>
#include <X11/Intrinsic.h>

/*
 * Structure:	buf_info
 * Purpose:	Describe a buffer from a window's point of view.
 * Description:	This structure is used to store information about a buffer
 *		that is specific for a given editing window.
 * Fields:	buf		the buffer
 *		top_mark	mark at beginning of first visible line
 *		first_col	number of first visible column
 *		selection	current selection for this buffer in this win
 *		marks		list of user settable marks
 *		next		link to next structure in linked list
 */
struct buf_info {
	Sbuf *buf;
	Sbufmark *top_mark;
	long first_col;
	Sbufmark *selection;
	struct buf_info *next;
};



/*
 * Purpose:	Describe an editing buffer.
 * Fields:	window		the X widget identifier for the window in
 *				which the text is displayed
 *		rows		number of text rows in window
 *		cols		number of text columns in window
 *		font		the font to draw characters in
 *		text_gc		X graphics context for normal text
 *		invert_gc	X graphics context for inverted text
 *		rowmarks	marks for rows: if mark is dirty, row is 
 *				repainted
 *		num_marks	number of valid entries in rowmarks
 *		old_display	the currently drawn text in the window
 *			x1,x2	the x coordinates for selection (-1==none)
 *			text	the text on the line
 *		next		next element in global list of editwin's
 * Note:	rows and cols are initialized to -1, so that 
 *		editwin_get_dimensions will automatically set them.
 */
#define MAX_ROWS 128
#define MAX_COLS 256
struct editwin {
        Widget window;
	int rows, cols;
	struct font *font;
	GC text_gc;
	GC invert_gc;
	struct buf_info *buf_list, *buf_current;
	Sbufmark *rowmarks[MAX_ROWS];
	struct row_info {
		int x1, x2;
		char text[MAX_COLS + 1];
	} old_display[MAX_ROWS];
	int num_marks;
	struct editwin *next;
};



/*
 * Variable:	ew_list
 * Purpose:	Point to head of global list of editing windows
 */
extern struct editwin *ew_list;



/*
 * Structure:	draw_line
 * Purpose:	Provide unchanging arguments for function draw_line
 * Fields:	ew	the editing window that is being updated
 *		dpy	the display being drawn into
 *		win	the X window widget being drawn into
 *		cols	text columsn in the window
 *		fw	width of characters in the font being used
 *		bx,by	beginning of selection (in char cells)
 *		ex,ey	end of selection (in char cells)
 *		columnar  is the selection columnar?
 */
struct draw_line {
	struct editwin *ew;
	Display *dpy;
	Window win;
	int cols;
	int fw;
	int bx, by, ex, ey;
	int columnar;
};


int editwin_create(struct editwin **, Widget, GC, GC, Sbuf *);
void editwin_destroy(struct editwin *);

void editwin_force_update(struct editwin *, int, int);
void editwin_update(struct editwin *, int, int);

int editwin_set_buf(struct editwin *, Sbuf *);
int editwin_all_forget_buf(Sbuf *);

void editwin_dirtify(struct editwin *);

#endif
