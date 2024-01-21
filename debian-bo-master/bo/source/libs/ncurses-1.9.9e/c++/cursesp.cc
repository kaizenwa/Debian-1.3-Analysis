// * this is for making emacs happy: -*-Mode: C++;-*-

/*

  written and
  Copyright (C) 1993 by Anatoly Ivasyuk (anatoly@nick.csh.rit.edu)

*/

#if !defined(lint)
static char vcid[] = "$Id: cursesp.cc,v 1.1.1.1 1996/04/30 00:29:13 mdorman Exp $";
#endif // !lint

#pragma implementation

#include "cursesp.h"

void NCursesPanel::redraw()
{
PANEL *pan;

	pan = panel_above(NULL);
	while (pan) {
		::touchwin(panel_window(pan));
		pan = panel_above(pan);
	}
	update_panels();
	::doupdate();
}

void NCursesPanel::refresh()
{
	update_panels();
	::doupdate();
}

int NCursesPanel::boldframe(char *title)
{
	standout();
	box();
	if (title)
		mvwaddstr(w, 0,
		((p->wendx - p->wstartx + 1) - strlen(title)) / 2,
		title);
	return standend();
}

int NCursesPanel::frame(char *title)
{
	if (!title) {
		return box();
	} else {
		box();
		return mvwaddstr(w, 0,
			((p->wendx - p->wstartx + 1) - strlen(title)) / 2,
			title);
	}
}

