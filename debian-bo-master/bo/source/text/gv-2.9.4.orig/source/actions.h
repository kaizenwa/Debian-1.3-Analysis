/*
**
** actions.h
**
** Copyright (C) 1995-1997 Johannes Plass
** 
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
** 
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
**           Department of Physic
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

#ifndef	_GV_ACTIONS_H_
#define	_GV_ACTIONS_H_

extern void 			action_miscMenu (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_otherPage (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_movePage (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_panner (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_antialias (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_quit (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_handleDSC (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_open (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_redisplay (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_reopen (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_save (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_print (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_prev (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_showThisPage (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_next (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_center (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_setPageMark (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_autoResize (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_set_magstep (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_set_orientation (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_set_pagemedia (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_dismissPopup (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_deleteWindow (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_delete_zoom (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_scroll (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_erase_locator (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_checkFile (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

extern void 			action_popup_menu (
#if NeedFunctionPrototypes
    Widget,
    XEvent *,
    String *,
    Cardinal *
#endif
);

#endif	/* _GV_ACTIONS_H_ */
