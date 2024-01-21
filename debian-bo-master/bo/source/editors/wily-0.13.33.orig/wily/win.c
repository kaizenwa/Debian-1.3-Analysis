#include "tile.h"

static void
win_place(Tile *col, Text *tag, Text *body) {
	Tile	*win;
	int	max, min;

	findplace(col, &min, &max);
	win = tile_new(V,  min, max, tagheight, col, tag, body);
	list_add(col, win);
	assert(ISWIN(win));
}

/* Free the resources tied up by 'win'.  Return 0 for success. */
int
win_del(Tile *w)
{
	if(!w)
		return 0;

	assert(ISWIN(w));

	if ( view_delete(w->body) || view_delete(w->tag) )
		return -1;
	else {
		free(w->body);
		free(w->tag);
		tile_del(w);
		free(w);
		return 0;
	}
}

/* Return the window associated with 'tile', or 0. */
Tile*
tile_win(Tile*tile) {
	return (tile && tile->body) ? tile : 0;
}

/* clone 'win' */
void
win_clone(Tile *win) {
	win_place(win->up, view_text(win->tag), view_text(win->body));
}

/* Create a window to represent 'path' */
void
win_new(char*path, Text*tag, Text*body) {
	win_place(findcol(path), tag, body);
}

/* Add some text to w's tag representing the current selection */
void
win_anchor(Tile*w, char *arg)
{
	char	buf[80];
	ulong	p;
	Text	*t;

	assert(ISWIN(w));

	if(arg) {
		/* line address */
		Range sel;
		int	line;
		View *v;
		
		v = w->body;
		sel = view_getsel(v);
		t = view_text(v);
		line = text_linenumber(t, sel.p0);
		sprintf(buf, " :%d,.", line);
	} else {
		/* character address */
		sprintf(buf, " :#%lu,.", view_getsel(w->body).p0);
	}
	/* append 'buf' to 't' */
	t = view_text(w->tag);
	p = text_length(t);
	text_replaceutf(t, range(p,p), buf);
}
