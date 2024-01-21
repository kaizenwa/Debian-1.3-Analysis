/* Help for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
#include "macconq.h"

static pascal void draw_instructions_text(WindowPtr win, short ditem);
static pascal void help_vscroll_fn(ControlHandle control, short code);

/* Globals for the instructions window. */

WindowPtr instructionswin = nil;

TEHandle instructions_text = nil;

UserItemUPP draw_instructions_text_proc;

/* Globals for the help window. */

DialogPtr helpwin = nil;

TEHandle helptopic = nil;

TEHandle helptext = nil;

ControlHandle helpvscrollbar;

ControlActionUPP help_vscroll_proc;

ControlHandle topicsbutton;
ControlHandle helpbutton;
ControlHandle prevbutton;
ControlHandle nextbutton;
ControlHandle backbutton;

static HelpNode *cur_help_node = NULL;

char *helpstring = NULL;

/* The help node that is the list of topics. */

static HelpNode *topics_help_node = NULL;

/* The help node that tells about how to use the help system. */

static HelpNode *help_help_node = NULL;

#define NODESTACKSIZE 50

HelpNode **nodestack;

int nodestackpos;

/* The instructions window. */

static pascal void
draw_instructions_text(WindowPtr win, short ditem)
{
	GrafPtr oldport;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(instructionswin, diInstructionsText, &itemtype, &itemhandle, &itemrect);
	GetPort(&oldport);
	SetPort(instructionswin);
	TextSize(14);
	TEUpdate(&itemrect, instructions_text);
	/* This makes the title item draw big. */
	TextSize(18);
	SetPort(oldport);
}

/* Bring up the dialog with instructions on how to play. */

void
instructions_dialog()
{
	int mainwidth, mainheight;
	Rect tmprect;

	if (instructionswin == nil) {
		create_instructions_dialog();
	}
	if (first_windows) {
		get_main_screen_size(&mainwidth, &mainheight);
		tmprect = instructionswin->portRect;
		MoveWindow(instructionswin,
				   mainwidth - (tmprect.right - tmprect.left) - 3,
				   mainheight - (tmprect.bottom - tmprect.top) - 3,
				   FALSE);
	}
	ShowWindow(instructionswin);
	SelectWindow(instructionswin);
}

void
create_instructions_dialog()
{
	Obj *instructions, *rest;
	char *str;
	Str255 tmpstr;
	Rect destrect, viewrect;
	short itemtype;  Handle itemhandle;  Rect itemrect;
	Scorekeeper *sk;

	if (draw_instructions_text_proc == NULL)
	  draw_instructions_text_proc = NewUserItemProc(draw_instructions_text);

	instructionswin = GetNewDialog(dInstructions, nil, (DialogPtr) -1L);
	GetDItem(instructionswin, diInstructionsTitle, &itemtype, &itemhandle, &itemrect);
	c2p(((mainmodule && mainmodule->title) ? mainmodule->title : ""), tmpstr);
	SetIText(itemhandle, tmpstr);
	GetDItem(instructionswin, diInstructionsText, &itemtype, &itemhandle, &itemrect);
	SetDItem(instructionswin, diInstructionsText, itemtype, (Handle) draw_instructions_text_proc, &itemrect);
	destrect = itemrect;
	viewrect = itemrect;
	SetPort(instructionswin);
	/* All text will be in Times. */
	/* (should put sizes in a resource so can be edited) */
	TextFont(times);
	TextSize(14);
	instructions_text = TENew(&destrect, &viewrect);
	TESetSelect(0, 32767, instructions_text);
	TEDelete(instructions_text);
	if (mainmodule) {
		instructions = mainmodule->instructions;
		if (instructions != lispnil) {
			if (stringp(instructions)) {
				str = c_string(instructions);
				TEInsert(str, strlen(str), instructions_text);
			} else if (consp(instructions)) {
			    for (rest = instructions; rest != lispnil; rest = cdr(rest)) {
			    	if (stringp(car(rest))) {
			    		str = c_string(car(rest));
			    		/* An empty line is a line break. */
			    		if (strlen(str) == 0) {
#ifdef THINK_C
			    			str = "\r";
#else
			    			str = "\n";
#endif
			    		}
			    		TEInsert(str, strlen(str), instructions_text);
			    		TESetSelect(32767, 32767, instructions_text);
			    		/* Insert a blank between strings if none present, since they are
			    		   usually word breaks. */
			    		if (str[strlen(str)-1] != ' ')
			    		  TEInsert(" ", 1, instructions_text);
			    		TESetSelect(32767, 32767, instructions_text);
			    	}
		    	}
		    } else {
		    	/* error? */
		    }
		} else {
			if (mainmodule->blurb) {
				str = mainmodule->blurb;
				TEInsert(str, strlen(str), instructions_text);
#ifdef THINK_C
				str = "\r\r";
#else
				str = "\n\n";
#endif
				TEInsert(str, strlen(str), instructions_text);
			}
			str = "(no instructions supplied)";
			TEInsert(str, strlen(str), instructions_text);
			for_all_scorekeepers(sk) {
				/* (should add better scorekeeper descriptions) */
#ifdef THINK_C
			    str = "\r";
#else
			    str = "\n";
#endif
				TEInsert(str, strlen(str), instructions_text);
				str = "a scorekeeper";
				TEInsert(str, strlen(str), instructions_text);
			}
		}
	} else {
		/* this should be impossible? */
	}
	/* This makes the title item draw big. */
	TextSize(18);
	add_window_menu_item("Instructions", instructionswin);
}

int
hit_instructions_dialog(DialogPtr dialog, int itemhit, EventRecord *evt)
{
	switch (itemhit) {
		case diInstructionsHelp:
			/* Just jump to the help dialog. */
			help_dialog(NULL);
			break;
	}
	return TRUE;
}

/* Help window. */

/* This is the top-level access to bring up the help window, can be called
   anywhere, anytime. */

void
help_dialog(HelpNode *helpnode)
{
	if (helpwin == nil) {
		create_help_window();
	}
	if (helpnode != NULL) {
		cur_help_node = helpnode;
		set_help_content(helpnode);
	}
	ShowWindow(helpwin);
	SelectWindow(helpwin);
}

void
describe_menus(int arg, char *key, char *buf)
{
	strcat(buf, "File\n");
	strcat(buf, "  The usual file operations.\n");
	strcat(buf, "\n");
	strcat(buf, "Edit\n");
	strcat(buf, "  Select All selects all of your units at once.\n");
	strcat(buf, "\n");
	strcat(buf, "Find\n");
	strcat(buf, "  Find Selected goes to the next map back and looks at the unit selected or viewed in the front window.\n");
	strcat(buf, "\n");
	strcat(buf, "Play\n");
	strcat(buf, "  This is the main menu for unit actions.");
	strcat(buf, "  Each item represents something that you can do to the currently selected units.\n");
	strcat(buf, "  If an action is grayed out, then none of the selected units can do it.\n");
	strcat(buf, "  Closeup shows details of the selected units.\n");
	strcat(buf, "  Build brings up the construction dialog.\n");
	strcat(buf, "Side\n");
	strcat(buf, "  This menu is for things that affect your whole side.\n");
	strcat(buf, "Windows\n");
	strcat(buf, "  This menu does window control.\n");
	strcat(buf, "View (when a map is front window)\n");
	strcat(buf, "  Items toggle various display elements in the front map window.\n");
	strcat(buf, "\n");
	strcat(buf, "View (when a list is front window)\n");
	strcat(buf, "  Items toggle various display elements in the front list window.\n");
	strcat(buf, "\n");
}

void
describe_mouse(int arg, char *key, char *buf)
{
	strcat(buf, "In move-on-click mode:\n");
	strcat(buf, "  The next unit that can do anything will be selected automatically.\n");
	strcat(buf, "  Click once on a destination to move the selected unit there.\n");
	strcat(buf, "\n");
	strcat(buf, "In normal mode:\n");
	strcat(buf, "  Click once on a unit to select it.\n");
	strcat(buf, "  Drag to destination to move it.\n");
	strcat(buf, "  Shift-click to select additional units.\n");
	strcat(buf, "  Drag out rectangle to select all units inside.\n");
	strcat(buf, "\n");
	strcat(buf, "Command-click to moves all selected units to the clicked-on location.");
	strcat(buf, "\n");
	strcat(buf, "Combat is automatic if another side's unit is adjacent and clicked on.");
}

void
describe_keyboard(int arg, char *key, char *buf)
{
	describe_commands(arg, key, buf);
}

void
describe_help(int arg, char *key, char *buf)
{
	strcat(buf, "Xconq Help consists of a number of ``topics''.  ");
	strcat(buf, "Topics include generic information about Xconq as ");
	strcat(buf, "well as specific details of the game in progress.  ");
	strcat(buf, "To navigate, click the buttons above.");
	strcat(buf, "\n");
	strcat(buf, "Topics: ");
	strcat(buf, "Clicking this button shows you the list of topics.  ");
	strcat(buf, "Clicking on a topic in the list takes you to it directly.");
	strcat(buf, "\n");
	strcat(buf, "Help:  ");
	strcat(buf, "Clicking this button shows you the topic you're looking at right now.");
	strcat(buf, "\n");
	strcat(buf, "Next:  ");
	strcat(buf, "Clicking this button goes to the next topic in order.");
	strcat(buf, "\n");
	strcat(buf, "Previous:  ");
	strcat(buf, "Clicking this button goes to the previous topic in order.");
	strcat(buf, "\n");
	strcat(buf, "Back:  ");
	strcat(buf, "Clicking this button goes to the last topic you were looking at.  ");
	strcat(buf, "Multiple clicks take you farther and farther back.");
	strcat(buf, "\n");
}

void
create_help_window()
{
	int h, v;
	Rect helptopicrect, destrect, viewrect, vscrollrect;

	/* Create the window, color if possible, since images may be in color. */
	if (hasColorQD) {	
		helpwin = GetNewCWindow(wHelp, NULL, (WindowPtr) -1L);
	} else {
		helpwin = GetNewWindow(wHelp, NULL, (WindowPtr) -1L);
	}
	topicsbutton = GetNewControl(cTopicsButton, helpwin);
	helpbutton = GetNewControl(cHelpButton, helpwin);
	prevbutton = GetNewControl(cPrevButton, helpwin);
	nextbutton = GetNewControl(cNextButton, helpwin);
	backbutton = GetNewControl(cBackButton, helpwin);
	SetPort(helpwin);
	/* All text will be in Times. */
	/* (should these be choosable?) */
	TextFont(times);
	/* Set up the topic title. */
	TextSize(18);
	SetRect(&helptopicrect, 45, 45, 305, 75); 
	helptopic = TENew(&helptopicrect, &helptopicrect);
	/* Set up the help text proper. */
	TextSize(14);
	h = window_width(helpwin);  v = window_height(helpwin);
	SetRect(&viewrect, 5, 75, h - sbarwid, v - sbarwid); 
	destrect = viewrect;
	helptext = TENew(&destrect, &destrect);
	/* Set up a vertical scrollbar. */
	vscrollrect = helpwin->portRect;
	vscrollrect.top = 75;
	vscrollrect.bottom -= sbarwid - 1;
	vscrollrect.left = vscrollrect.right - sbarwid;
	vscrollrect.right += 1;
	helpvscrollbar =
		NewControl(helpwin, &vscrollrect, "\p", TRUE, 0, 0, 0, scrollBarProc, 0L);
	HiliteControl(helpvscrollbar, 0);
	/* Add the Mac-specific help nodes. */
	/* (Note that these will appear in *reverse* order from here, because each
	   is being glued right after the first help node. */
	add_help_node("menus", describe_menus, 0, first_help_node);
	add_help_node("mouse", describe_mouse, 0, first_help_node);
	add_help_node("keyboard", describe_keyboard, 0, first_help_node);
	help_help_node = add_help_node("help", describe_help, 0, first_help_node);
	topics_help_node = add_help_node("topics", describe_topics, 0, first_help_node);
	cur_help_node = topics_help_node;
	set_help_content(cur_help_node);
	if (nodestack == NULL)
	  nodestack = (HelpNode **) xmalloc(NODESTACKSIZE * sizeof(HelpNode *));
	nodestackpos = 0;
	add_window_menu_item("Help", helpwin);
}

void
set_help_content(HelpNode *curnode)
{
	char *str;

	get_help_text(curnode);
	/* Set the displayed topic title. */
	TESetSelect(0, 32767, helptopic);
	TECut(helptopic);
	/* Copy in the new help topic text. */
	TESetText(curnode->key, strlen(curnode->key), helptopic);
	/* Set the displayed text. */
	str = curnode->text;
#ifdef THINK_C
	/* Hack up newlines so that TextEdit recognizes them. */
	{
		int i, len = strlen(str);
	
		for (i = 0; i < len; ++i) {
			if (str[i] == '\n')
			  str[i] = '\r';
		}
	}
#endif
	helpstring = str;
	/* Remove all the existing text. */
	TESetSelect(0, 32767, helptext);
	TECut(helptext);
	/* Copy in the new help text. */
	TESetText(helpstring, strlen(helpstring), helptext);
	(*helptext)->destRect = (*helptext)->viewRect;
	/* Update on the screen. */
	draw_help();
	adjust_help_scrollbar();
}

void
draw_help()
{
	Rect tmprect;
	GrafPtr oldport;

	GetPort(&oldport);
	SetPort(helpwin);
	SetRect(&tmprect, 5, 40, 5 + 32, 40 + 32);
	EraseRect(&tmprect);
	if (cur_help_node->nclass == utypenode && is_unit_type(cur_help_node->arg)) {
		draw_unit_image(helpwin, tmprect.left, tmprect.top, 32, 32,
						cur_help_node->arg, -1, 0);
	} else if (cur_help_node->nclass == ttypenode && is_terrain_type(cur_help_node->arg)) {
		draw_terrain_sample(helpwin, tmprect, cur_help_node->arg); 
	}
	TextSize(18);
	TEUpdate(&(helpwin->portRect), helptopic);
	TextSize(14);
	TEUpdate(&(helpwin->portRect), helptext);
	SetPort(oldport);
	adjust_help_scrollbar();
}

void
adjust_help_scrollbar()
{
	int lines, newmax, value;

	lines = (*helptext)->nLines;
	newmax = lines - (((*helptext)->viewRect.bottom - (*helptext)->viewRect.top)
					 / (*helptext)->lineHeight);
	if (newmax < 0) newmax = 0;
	SetCtlMax(helpvscrollbar, newmax);
	value = ((*helptext)->viewRect.top - (*helptext)->destRect.top)
			/ (*helptext)->lineHeight;
	SetCtlValue(helpvscrollbar, value);
}

void
activate_help(int activate)
{
	HiliteControl(helpvscrollbar, (activate ? 0 : 255));
}

static pascal void
help_vscroll_fn(ControlHandle control, short code)
{
	int oldvalue, curvalue, minvalue, maxvalue, pagesize, jump;

	curvalue = GetCtlValue(control);
	minvalue = GetCtlMin(control);
	maxvalue = GetCtlMax(control);
	pagesize = ((*helptext)->viewRect.bottom - (*helptext)->viewRect.top) /
				(*helptext)->lineHeight;
	if (pagesize > 1) pagesize -= 1;
	switch (code) {
		case inPageDown:
			jump = pagesize;
			break;
		case inDownButton:
			jump = 1;
			break;
		case inPageUp:
			jump = - pagesize;
			break;
		case inUpButton:
			jump = -1;
			break;
		default:
			jump = 0;
			break;
	}
	oldvalue = curvalue;
	curvalue = max(min(curvalue + jump, maxvalue), minvalue);
	SetCtlValue(control, curvalue);
	/* Calculate the actual jump and use it to adjust the text. */
	jump = curvalue - oldvalue;
	if (jump != 0)
	  TEScroll(0, - jump * (*helptext)->lineHeight, helptext);
}

/* Respond to an event occurring in the help window. */

void
do_mouse_down_help(Point mouse, int mods)
{
	ControlHandle control;
	short part, value;
	int i;
	HelpNode *prevhelpnode = cur_help_node, *helpnode;

	if (help_vscroll_proc == NULL)
	  help_vscroll_proc = NewControlActionProc(help_vscroll_fn);

	part = FindControl(mouse, helpwin, &control);
	if (control == topicsbutton) {
		cur_help_node = topics_help_node;
	} else if (control == helpbutton) {
		cur_help_node = help_help_node;
	} else if (control == prevbutton) {
		cur_help_node = cur_help_node->prev;
	} else if (control == nextbutton) {
		cur_help_node = cur_help_node->next;
	} else if (control == backbutton) {
		if (nodestackpos > 0) {
			cur_help_node = nodestack[--nodestackpos];
		}
	} else if (control == helpvscrollbar) {
		if (part != 0) {
			switch (part) {
				case inPageDown:
				case inDownButton:
				case inPageUp:
				case inUpButton:
					value = TrackControl(control, mouse, (ControlActionUPP) help_vscroll_proc);
					break;
				case inThumb:
					value = GetCtlValue(control);
					if ((part = TrackControl(control, mouse, nil)) != 0) {
						value -= GetCtlValue(control);
						if (value != 0) {
							TEScroll(0, value * (*helptext)->lineHeight, helptext);
						}
					}
					break;
			}
		}
	} else if (PtInRect(mouse, &((*helptext)->viewRect))) {
		TEClick(mouse, 0, helptext);
		if (cur_help_node == topics_help_node) {
			char strbuf[100], *cr1, *cr2;
			int selstart = (*helptext)->selStart, selend = (*helptext)->selEnd;
			CharsHandle chars = TEGetText(helptext);

			if (selstart == selend) {
				if (strchr("\r\n", *((*chars)+selstart)))
				  --selstart;
				/* Manufacture a "selection" of the line clicked in. */
				for (cr1 = (*chars)+selstart; cr1 != (*chars); --cr1)
				  if (strchr("\r\n", *cr1))
				    break;
				cr2 = strchr((*chars)+selstart, '\r');
				if (cr2 == NULL)
				  cr2 = strchr((*chars)+selstart, '\n');
				selstart = (cr1 != NULL ? cr1 - (*chars) + 1 : 0);
				selend = (cr2 != NULL ? cr2 - (*chars) : 0);
				if (selstart > selend)
				  selstart = selend;
			}
			if (selstart != selend) {
				strncpy(strbuf, (*chars)+selstart, selend - selstart);
				strbuf[selend - selstart] = '\0';
				helpnode = find_help_node(cur_help_node, strbuf);
				if (helpnode != NULL) {
					cur_help_node = helpnode;
				} else {
					beep();
				}
			} else {
				beep();
			}
		}
	}
	/* If we changed help nodes, get its contents and record on the node stack. */
	if (prevhelpnode != cur_help_node) {
		set_help_content(cur_help_node);
		if (control != backbutton) {
			if (nodestackpos >= NODESTACKSIZE) {
				for (i = 1; i < NODESTACKSIZE; ++i) nodestack[i - 1] = nodestack[i];
				nodestackpos = NODESTACKSIZE - 1;
			}
			nodestack[nodestackpos++] = prevhelpnode;
		}
	}
}

void
grow_help(int h, int v)
{
	EraseRect(&helpwin->portRect);
	SizeWindow(helpwin, h, v, 1);
	MoveControl(helpvscrollbar, h - sbarwid, 75);
	SizeControl(helpvscrollbar, sbarwid + 1, v - 75 - sbarwid + 1);
	(*helptext)->viewRect.right = h - sbarwid;
	(*helptext)->viewRect.bottom = v - sbarwid;
	(*helptext)->destRect.right = h - sbarwid;
	TECalText(helptext);
	InvalRect(&helpwin->portRect);
}					

void
zoom_help(int part)
{
	int titleh, h, v;
	Rect zoomrect;
	GDHandle gd, zoomgd;

	EraseRect(&helpwin->portRect);
	if (part == inZoomOut) {
		if (hasColorQD) {
			zoomgd = best_zoom_screen(&helpwin->portRect);
			zoomrect = (*zoomgd)->gdRect;
			if (zoomgd == GetMainDevice()) {
				zoomrect.top += GetMBarHeight();
			}
			titleh = 20; /* (should calc) */
			zoomrect.top += titleh;
			InsetRect(&zoomrect, 3, 3);
		} else {
			/* If no Color QD, then there is only the one screen. */
			zoomrect = QD(screenBits).bounds;
			zoomrect.top += GetMBarHeight();
			titleh = 20; /* (should calc) */
			zoomrect.top += titleh;
			InsetRect(&zoomrect, 4, 4);
		}
		(*((WStateDataHandle) ((WindowPeek) helpwin)->dataHandle))->stdState = zoomrect;
	}
	ZoomWindow(helpwin, part, (helpwin == FrontWindow()));
	h = window_width(helpwin);  v = window_height(helpwin);
	MoveControl(helpvscrollbar, h - sbarwid, 0);
	SizeControl(helpvscrollbar, sbarwid + 1, v - sbarwid + 1);
	adjust_help_scrollbar();
	(*helptext)->viewRect.right = h - sbarwid;
	(*helptext)->viewRect.bottom = v - sbarwid;
	(*helptext)->destRect.right = h - sbarwid;
	TECalText(helptext);
	/* This will force a full redraw at the next update. */
	InvalRect(&helpwin->portRect);
}
