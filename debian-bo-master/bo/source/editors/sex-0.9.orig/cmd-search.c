/*
 * File:	cmd-search.c
 * Purpose:	Functions that implement search and replace.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: cmd-search.c,v 1.19 1996/12/06 20:11:54 liw Exp $"
 */

#include <publib.h>

#include "anchor.h"
#include "cmd.h"
#include "searchwin.h"
#include "selections.h"
#include "win.h"
#include "error.h"



/*
 * Prototypes for local functions.
 */

static int selection_matches_pattern(struct win *);
static int search_next(struct win *);



/*
 * Function:	cmd_search_popup
 * Purpose:	Show the dialog box.
 */
int cmd_search_popup(struct win *win) {
	cmd_prev_was_cut = 0;
	searchwin_popup(win_searchwin(win));
	return 0;
}



/*
 * Function:	cmd_search_next
 * Purpose:	Find the next occurence of the search string after selection.
 */
int cmd_search_next(struct win *win) {
	anchor_up(win);
	cmd_prev_was_cut = 0;
	
	switch (search_next(win)) {
	case -1:
		return -1;
		
	case 0:
		error(win, "Pattern not found.");
		return -1;
		
	default:
		win_show(win, sbuf_mark_begin(win_selection(win)));
		return 0;
	}
	/*NOTREACHED*/
}



/*
 * Function:	cmd_search_selection
 * Purpose:	Set search string to selection, then search after selection.
 */

static void do_search_selection(char *str, long len, void *win) {
	searchwin_set_search(win_searchwin(win), str);
	(void) cmd_search_next(win);
}

int cmd_search_selection(struct win *win) {
	anchor_up(win);
	cmd_prev_was_cut = 0;
	if (sel_string(win, do_search_selection, win) == -1)
		return -1;
	return 0;
}



/*
 * Function:	cmd_replace_once
 * Purpose:	Replace current selection.
 * Note:	Replace only if current selection matches pattern.
 */
int cmd_replace_once(struct win *win) {
	Sbufmark *sel;
	char *p;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	
	if (!selection_matches_pattern(win)) {
		error(win, "Can't replace (selection does not match pattern)");
		return -1;
	}
	
	sel = win_selection(win);
	p = searchwin_replace_text(win_searchwin(win));

	if (sbuf_strchange(sel, p, strlen(p)) == -1) {
		error(win, "Can't replace (out of memory?)");
		return -1;
	}
	
	return 0;
}



/*
 * Function:	cmd_replace_and_search
 * Purpose:	Replace current selection and search next match.
 * Note:	Replace only if current selection matches pattern.
 */
int cmd_replace_and_search(struct win *win) {
	anchor_up(win);
	cmd_prev_was_cut = 0;
	
	if (cmd_replace_once(win) == -1)
		return -1;
	return cmd_search_next(win);
}



/*
 * Function:	cmd_search_and_replace_all
 * Purpose:	Search and replace all matches.
 */
int cmd_search_and_replace_all(struct win *win) {
	int ok;
	unsigned long count;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	
	if (selection_matches_pattern(win))
		ok = 1;
	else 
		ok = search_next(win);

	count = 0;
	while (ok > 0) {
		if (cmd_replace_once(win) == -1)
			return -1;
		++count;
		ok = search_next(win);
	}

	if (ok != -1)
		error(win, "%lu replacements", count);
	win_show(win, sbuf_mark_begin(win_selection(win)));
	return ok;
}



/*
 * Function:	cmd_cancel_search
 * Purpose:	Pop down search dialog box.
 */
int cmd_cancel_search(struct win *win) {
	cmd_prev_was_cut = 0;
	searchwin_popdown(win_searchwin(win));
	return 0;
}



/**********************************************************************
 * Local functions follow.
 */
 

/*
 * Function:	selection_matches_pattern
 * Purpose:	Does the selection of a window match the search pattern?
 * Return:	True or false.
 */
static int selection_matches_pattern(struct win *win) {
	Sbuf *buf;
	Sbufmark *match, *sel;
	char *pat;
	unsigned long opts;
	int ret;

	buf = win_buf(win);
	sel = win_selection(win);
	pat = searchwin_search_text(win_searchwin(win));
	opts = searchwin_options(win_searchwin(win));
	
	match = sbuf_mark(buf, 0, 0);
	if (match == NULL) {
		error(win, "Can't search (out of memory?)");
		return 0;
	}

	if (sbuf_search(match, sel, pat, strlen(pat), opts) == -1)
		ret = 0;
	else {
		ret = sbuf_mark_begin(match) == sbuf_mark_begin(sel) &&
			sbuf_mark_end(match) == sbuf_mark_end(sel);
		sbuf_unmark(match);
	}

	return ret;
}



/*
 * Function:	search_next
 * Purpose:	Move selection to next match of pattern.
 * Return:	1 (match was found) or 0 (not found) or -1 (error)
 */
static int search_next(struct win *win) {
	Sbuf *buf;
	Sbufmark *area, *match, *sel;
	char *pat;
	long begin, end, len;
	unsigned long opts;
	int ret;

	buf = win_buf(win);
	sel = win_selection(win);
	len = sbuf_length(buf);
	begin = sbuf_mark_begin(sel);
	end = sbuf_mark_end(sel);
	pat = searchwin_search_text(win_searchwin(win));
	opts = searchwin_options(win_searchwin(win));
	
	if (strlen(pat) == 0)	/* No error message! It's too irritating. */
		return -1;

	area = sbuf_mark(buf, 0, 0);
	match = sbuf_mark(buf, 0, 0);
	if (area == NULL || match == NULL) {
		error(win, "Can't search (out of memory?)");
		if (area != NULL)
			sbuf_unmark(area);
		return -1;
	}

	if ((opts & SBUF_BACKWARD) == 0)
		sbuf_remark(area, end, len - end);
	else
		sbuf_remark(area, 0, begin);

	ret = sbuf_search(match, area, pat, strlen(pat), opts);
	if (ret == -1)
		ret = 0;
	else {
		sbuf_remark(sel, sbuf_mark_begin(match), 
			sbuf_mark_length(match));
		sbuf_mark_set_columnar(sel, 0);
		ret = 1;
	}
		
	sbuf_unmark(match);
	sbuf_unmark(area);
	return ret;
}
