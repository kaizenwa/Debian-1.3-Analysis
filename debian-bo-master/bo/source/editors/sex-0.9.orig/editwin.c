/*
 * File:	editwin.c
 * Purpose:	Implement  the editing window.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: editwin.c,v 1.48 1996/12/14 12:17:03 liw Exp $"
 * Description:	The editing window consists of the area, where the text
 *		in the editing buffer is displayed, and where the user
 *		may edit it.
 */
 
#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <publib.h>

#include "font.h"
#include "editwin.h"
#include "x.h"
#include "error.h"
#include "tab.h"


struct editwin *ew_list = NULL;


/*
 * Macro:	CTRL_CHAR
 * Purpose:	The character that is substituted for certain control chars.
 */
#define CTRL_CHAR	'.'



/*
 * Macros:	min, max
 * Purpose:	Return the smaller or larger of two values.
 * Note:	As usual with macros, beware of side effects.
 */
#define min(a,b)	((a) < (b) ? (a) : (b))
#define max(a,b)	((a) < (b) ? (b) : (a))
 


/*
 * Prototypes for local functions.
 */
static int create_buf_info(struct buf_info **, Sbuf *);
static void destroy_buf_info(struct buf_info *);
static struct buf_info *find_buf_info(struct buf_info *, Sbuf *);
static int make_new_rowmarks(struct editwin *, int, int);
static void init_draw_line(struct editwin *, struct draw_line *, int);
static void draw_line(struct draw_line *, int);
static long make_line(char *, size_t, struct draw_line *, int);
static void text_to_window(struct editwin *, long, int *, int *);
static size_t same_ends(const char *, const char *, size_t);



/*
 * Purpose:	Create a new editing window descriptor.
 * Arguments:	ew	(out) pointer to pointer to window descriptor
 *		window	the X widget identifier for the window to display the
 *			text in
 *		buf	pointer to descriptor of buffer to be displayed in
 *			window
 * Return:	-1 for error, 0 for success.  If successful, also return
 *		a pointer to the new editing window descriptor via `ew'.
 *		If not successful, set `*ew' to NULL.
 */
int editwin_create(struct editwin **ew, Widget window, GC bw, GC wb, 
Sbuf *buf) {
	struct editwin *p;
	int i;

	p = malloc(sizeof(*p));
	if (p == NULL) {
		*ew = NULL;
		error(NULL, "malloc failed, can't create new editing window");
		return -1;
	}
	
	if (create_buf_info(&p->buf_list, buf) == -1) {
		free(p);
		*ew = NULL;
		return -1;
	}
	p->buf_current = p->buf_list;

	p->window = window;
	p->rows = -1;
	p->cols = -1;
	p->font = font_get_default();
	p->text_gc = bw;
	p->invert_gc = wb;
	p->num_marks = -1;
	for (i = 0; i < MAX_ROWS; ++i) {
		p->rowmarks[i] = NULL;
		p->old_display[i].text[0] = '\0';
	}

	p->next = ew_list;
	ew_list = p;

	*ew = p;

	return 0;
}



/*
 * Function:	editwin_set_buf
 * Purpose:	Set the buffer shown in an editing window.
 * Arguments:	ew	the editing window
 *		buf	the new buffer
 * Return:	-1 for failure, 0 for success.
 */
int editwin_set_buf(struct editwin *ew, Sbuf *buf) {
	struct buf_info *bi;
	int i;

	bi = find_buf_info(ew->buf_list, buf);
	if (bi == NULL) {
		if (create_buf_info(&bi, buf) == -1)
			return -1;
		bi->next = ew->buf_list;
		ew->buf_list = bi;
	}
	ew->buf_current = bi;

	for (i = 0; i < ew->num_marks; ++i) {
		sbuf_unmark(ew->rowmarks[i]);
		ew->rowmarks[i] = NULL;
	}
	editwin_dirtify(ew);
	ew->num_marks = 0;

	return 0;
}



/*
 * Function:	editwin_all_forget_buf
 * Purpose:	Remove a buffer from the buffer list of all buffers.
 * Arguments:	buf	the buffer to forget.
 * Return:	-1 for failure, 0 for OK.  The function fails if the
 *		buffer is currently displayed by any window.
 */
int editwin_all_forget_buf(Sbuf *buf) {
	struct editwin *ew;
	struct buf_info *p, *q;

	for (ew = ew_list; ew != NULL; ew = ew->next) {
		if (ew->buf_current->buf == buf)
			return -1;
			/* xxx should this be checked for all ew's first?) */
		if (ew->buf_list->buf == buf) {
			p = ew->buf_list;
			ew->buf_list = p->next;
			destroy_buf_info(p);
		} else {
			p = ew->buf_list; 
			while (p->next != NULL && p->next->buf != buf)
				p = p->next;
			if (p->next != NULL && p->next->buf == buf) {
				q = p;
				p = p->next;
				q->next = p->next;
				destroy_buf_info(p);
			}
		}
	}
	return 0;
}



/*
 * Purpose:	Destroy an editing window.
 * Arguments:	ew	pointer to the editing window descriptor
 * Return:	nothing
 * Note:	This operation cannot fail.
 */
void editwin_destroy(struct editwin *ew) {
	assert(ew != NULL);
	while (ew->buf_list != NULL) {
		struct buf_info *p = ew->buf_list;
		ew->buf_list = p->next;
		sbuf_unmark(p->top_mark);
		sbuf_unmark(p->selection);
		free(p);
	}

	if (ew_list == ew)
		ew_list = ew->next;
	else {
		struct editwin *p;
		for (p = ew_list; p->next != ew; p = p->next)
			assert(p->next != NULL);
		p->next = p->next->next;
	}
	free(ew);
}



/*
 * Purpose:	Redraw all parts of an editing window (modified or not).
 * Arguments:	ew	pointer to the editing window descriptor
 * Return:	Nothing.
 * Note:	This operation cannot fail.  But it doesn't work very well
 *		if the window is wider than 1024 characters.
 */
void editwin_force_update(struct editwin *ew, int rows, int cols) {
	editwin_dirtify(ew);
	editwin_update(ew, rows, cols);
}



/*
 * Function:	editwin_dirtify
 * Purpose:	Make sure the next update will redraw everything.
 * Argumetns:	ew	the editing window to touch
 * Return:	Nothing.
 */
void editwin_dirtify(struct editwin *ew) {
	int i;
	
	for (i = 0; i < MAX_ROWS; ++i) {
		struct row_info *p = ew->old_display + i;
		p->x1 = p->x2 = -1;
		p->text[0] = '\0';
	}
}

	

/*
 * Purpose:	Redraw all modified parts of an editing window.
 * Arguments:	ew	pointer to the editing window descriptor
 * Return:	Nothing.
 * Note:	This operation cannot fail.  But it doesn't work very well
 *		if the window is wider than 1024 characters.
 */
void editwin_update(struct editwin *ew, int rows, int cols) {
	int i;
	struct draw_line dr;

	if (make_new_rowmarks(ew, rows, cols) == -1)
			return;

	init_draw_line(ew, &dr, cols);
	for (i = 0; i < ew->num_marks && !x_pending(); ++i)
		if (sbuf_mark_is_dirty(ew->rowmarks[i]))
			draw_line(&dr, i);
}

	

/**********************************************************************
 * Local functions                                                    *
 **********************************************************************/



/*
 * Function:	create_buf_info
 * Purpose:	Create and initialize a new struct buf_info.
 * Arguments:	ptr	pointer to pointer that is pointed at new struct
 *		buf	the buffer that is described by the struct
 * Return:	-1 for failure, 0 for success.
 */
static int create_buf_info(struct buf_info **ptr, Sbuf *buf) {
	struct buf_info *p;

	p = malloc(sizeof(*p));
	if (p == NULL) {
		error(NULL, "Couldn't create info for new buffer");
		return -1;
	}

	p->top_mark = sbuf_mark(buf, 0, 0);
	p->selection = sbuf_mark(buf, 0, 0);
	if (p->top_mark == NULL || p->selection == NULL) {
		if (p->top_mark != NULL)
			sbuf_unmark(p->top_mark);
		if (p->selection != NULL)
			sbuf_unmark(p->selection);
		free(p);
		error(NULL, "Couldn't create info for new buffer");
		return -1;
	}

	p->buf = buf;
	p->first_col = 0;
	p->next = NULL;

	*ptr = p;
	return 0;
}



/*
 * Function:	destroy_buf_info
 * Purpose:	Destroy a struct buf_info.
 * Arguments:	p	pointer to the buf_info to be destroyed
 * Return:	Nothing.
 */
static void destroy_buf_info(struct buf_info *p) {
	assert(p != NULL);
	assert(p->top_mark != NULL);
	assert(p->selection != NULL);
	
	sbuf_unmark(p->top_mark);
	sbuf_unmark(p->selection);
	free(p);
}



/*
 * Function:	find_buf_info
 * Purpose:	Find a struct buf_info based on an Sbuf.
 * Arguments:	list	pointer to first element in list
 *		buf	the buf that is searched
 * Return:	Pointer to element in list, or NULL if not found.
 */
static struct buf_info *find_buf_info(struct buf_info *list, Sbuf *buf) {
	while (list != NULL && list->buf != buf)
		list = list->next;
	return list;
}



/*
 * Function:	make_new_rowmarks
 * Purpose:	Initialize the row marks for an editing window.
 * Arguments:	ew	the editing window
 * Return:	-1 for failure, 0 for success.
 */
static int make_new_rowmarks(struct editwin *ew, int rows, int cols) {
	int c, i;
	long pos, boln;
	Sbuf *buf;

	++rows;
	buf = ew->buf_current->buf;
	pos = sbuf_mark_begin(ew->buf_current->top_mark);
	for (i = 0; i < rows; ++i) {
		boln = pos;
		while ((c = sbuf_charat(buf, pos)) != '\n' && c != EOF)
			++pos;
		if (c == '\n')
			++pos;
		if (ew->rowmarks[i] == NULL) {
			ew->rowmarks[i] = sbuf_mark(buf, 0, 0);
			if (ew->rowmarks[i] == NULL) {
				error(0,"couldn't create new mark, no update");
				return -1;
			}
		}
		sbuf_remark(ew->rowmarks[i], boln, pos - boln);
		sbuf_mark_set_dirty(ew->rowmarks[i], 1);
	}
	ew->num_marks = rows;
	return 0;
}



/*
 * Function:	init_draw_line
 * Purpose:	Initialize a struct draw_line for one window update
 * Arguments:	ew	the editing window to be updated
 *		p	the struct draw_line to be updated
 * Return:	Nothing.
 */
static void init_draw_line(struct editwin *ew, struct draw_line *p, int cols) {
	long begin, len;
	
	p->ew = ew;
	p->dpy = XtDisplay(ew->window);
	p->win = XtWindow(ew->window);
	p->cols = cols+1;
	p->fw = font_width(ew->font);

	p->columnar = sbuf_mark_is_columnar(ew->buf_current->selection);	
	begin = sbuf_mark_begin(ew->buf_current->selection);
	text_to_window(ew, begin, &p->by, &p->bx);
	len = sbuf_mark_length(ew->buf_current->selection);
	if (len > 0) {
		text_to_window(ew, sbuf_mark_end(ew->buf_current->selection)-1,
			&p->ey, &p->ex);
		if (!sbuf_mark_is_columnar(ew->buf_current->selection) &&
		    sbuf_charat(ew->buf_current->buf, begin+len-1)=='\n')
			p->ex = p->cols - 1;
	} else {
		p->ey = p->by;
		p->ex = p->bx;
	}
}
 


/*
 * Function:	draw_line
 * Purpose:	Draw a text line to a window at an arbitrary location.
 * Arguments:	p	the unchanging arguments (see struct draw_line)
 *		row	which row to draw
 * Return:	nothing.
 */
static void draw_line(struct draw_line *p, int row) {
	int i, n, y, first, last;
	char str[MAX_COLS * 8 + 1];
	struct row_info ri;
	unsigned long len, len2;
	int x1tab[3], x2tab[3];
	GC gctab[3];
	
	y = font_row_position(p->ew->font, row);

	len = make_line(str, sizeof(str), p, row);
	len2 = strlen(p->ew->old_display[row].text);

#define ENCODE(aa,bb) \
	do { \
		struct row_info *q = p->ew->old_display + row; \
		ri.x1 = aa; ri.x2 = bb; \
		first = strdiff(q->text, str); \
		if (len2 < p->cols) \
			last = len; \
		else \
			last = same_ends(q->text, str, len); \
		if (aa >= 0 && q->x1 >= 0) { \
			if (aa != q->x1) { \
				first = min(first, min(aa, q->x1)); \
				last = max(last, max(aa, q->x1)); \
			} \
			if (bb != q->x2) { \
				first = min(first, min(bb, q->x2)); \
				last = max(last, max(bb, q->x2)); \
			} \
		} else if (aa >= 0) { \
			first = min(first, aa); \
			last = max(last, bb); \
		} else if (q->x1 >= 0) { \
			first = min(first, q->x1); \
			last = max(last, q->x2); \
		} \
		if (last < first) last = first; \
	} while (0)
#define ADD(x1,x2,gc) (void) ((x1) <= (x2) && (x1) <= last && (x2) >= first && \
	(x1tab[n] = max(first, (x1)), x2tab[n] = min(last, (x2)), \
	 gctab[n] = gc, ++n))

	n = 0;
	if (row < p->by || row > p->ey) {
		ENCODE(-1, -1);
		ADD(0, p->cols - 1, p->ew->text_gc);
	} else if ((row == p->by && row == p->ey) || p->columnar) {
		ENCODE(p->bx, p->ex);
		ADD(0, p->bx-1, p->ew->text_gc);
		ADD(p->bx, p->ex, p->ew->invert_gc);
		ADD(p->ex+1, p->cols-1, p->ew->text_gc);
	} else if (row == p->by) {
		ENCODE(p->bx, p->cols-1);
		ADD(0, p->bx-1, p->ew->text_gc);
		ADD(p->bx, p->cols-1, p->ew->invert_gc);
	} else if (row > p->by && row < p->ey) {
		ENCODE(0, p->cols-1);
		ADD(0, p->cols-1, p->ew->invert_gc);
	} else { /* row == p->ey */
		ENCODE(0, p->ex);
		ADD(0, p->ex, p->ew->invert_gc);
		ADD(p->ex+1, p->cols-1, p->ew->text_gc);
	}
	
	for (i = 0; i < n; ++i) {
		XDrawImageString(p->dpy, p->win, gctab[i], x1tab[i] * p->fw, 
			y, str + x1tab[i], x2tab[i] - x1tab[i] + 1);
	}
	
	p->ew->old_display[row].x1 = ri.x1;
	p->ew->old_display[row].x2 = ri.x2;
	strcpy(p->ew->old_display[row].text, str);
}



/*
 * Function:	make_line
 * Purpose:	Fill line image buffer from text editing buffer
 * Return:	Length of line image (possibly shorter than actual line)
 */
static long make_line(char *str, size_t max, struct draw_line *p, int row) {
	Sbuf *buf;
	Sbufmark *mark;
	long i, j, n, len, pos, tabwidth, first_col, col, maxcol;
	int c;

	mark = p->ew->rowmarks[row];
	len = sbuf_mark_length(mark);
	tabwidth = tab_width();
	first_col = p->ew->buf_current->first_col;
	
	if (0 && len < max / tabwidth) {
		sbuf_strat(str, mark);
		if (len > 0 && str[len-1] == '\n') {
			--len;
			str[len] = '\0';
		}
		struntabify(str, tabwidth);
		if (first_col > 0)
			strdel(str, first_col);
		len = strlen(str);
	} else {
		j = 0;
		col = 0;
		maxcol = first_col + p->cols;
		buf = p->ew->buf_current->buf;
		pos = sbuf_mark_begin(mark);

		for (i = 0; i < len && j < max-1 && col < maxcol; ++i) {
			c = sbuf_charat(buf, pos + i);
			if (c == '\n')
				break;
			if (c != '\t') {
				n = 1;
				/*
				 * FIXME: This make bad assumptions about
				 * character sets.  But isprint doesn't
				 * work very well for Latin1, at the
				 * moment.
				 */
#if 0
				if (c < 32)
					c = CTRL_CHAR;
#endif
			} else {
				c = ' ';
				n = tabwidth - (col % tabwidth);
			}
			while (n-- > 0 && j < max-1) {
				if (col >= first_col)
					str[j++] = c;
				++col;
			}
		}
		len = j;
		str[j] = '\0';
	}

	if (len < p->cols)
		memset(str + len, ' ', p->cols - len);
	str[p->cols] = '\0';
	
	return p->cols;
}



/*
 * Function:	text_to_window
 * Purpose:	Convert a text position to window coordinates (in rows/cols)
 * Arguments:	ew	the editing window
 *		pos	the text position
 *		row	pointer to text row
 *		col	pointer to text column
 * Note:	The text position gives the position _before_ a character.
 */
static void text_to_window(struct editwin *ew, long pos, int *row, int *col) {
	Sbuf *buf = ew->buf_current->buf;
	long p;

	*row = (int) (sbuf_lineno(buf, pos) - 
		sbuf_lineno(buf, sbuf_mark_begin(ew->buf_current->top_mark)));
	*col = 0;
	for (p = sbuf_boln(buf, pos); p < pos; ++p) {
		if (sbuf_charat(buf, p) == '\t')
			*col = tab_next(*col);
		else
			++(*col);
	}
	*col -= ew->buf_current->first_col;
	if (*col < 0)
		*col = 0;
}


/*
 * Function:	same_ends
 * Purpose:	Return start of identical ends of two equal length strings.
 */
static size_t same_ends(const char *s, const char *t, size_t len) {
	++len;
	while (len > 0 && s[len-1] == t[len-1])
		--len;
	return len;
}
