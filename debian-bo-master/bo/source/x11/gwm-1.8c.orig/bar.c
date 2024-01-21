/* Copyright 1989 GROUPE BULL -- See license conditions in file COPYRIGHT
 * Copyright 1989 Massachusetts Institute of Technology
 */
/*********************************************\
* 					      *
* 	BULL WINDOW MANAGER for X11 .	      *
* 					      *
* 	MODULE defining the Bar Wob class.    *
* 					      *
\*********************************************/

 /* include */
#include	"EXTERN.h"
#include	"wool.h"
#include	"wl_atom.h"
#include	"gwm.h"
#include	"wl_fsm.h"
#include	"wl_pixmap.h"
#include	"wl_cursor.h"
#include	"wl_bar.h"

 /* local constants */

 /* external */
extern Wob      NewWob();
extern Plug     NewPlug();
extern WOOL_METHOD WLMenu[];
extern Bar BarOpen();
extern BarEventHandler(), BarClose(), UpdateBarGeometry(), ReconfigureBar();

#ifdef SHAPE			/* compile with -I/usr/include/X11 AND
				   -I/usr/include/X11/extensions to work on
				   machines having shapes.h in either place */
#include	<shape.h>
extern BarIsShaped(), UpdateBarShape();
#define TileIsShaped(tile) \
    (tile && (tile)->type == WLPixmap) && ((WOOL_Pixmap) (tile)) -> mask

#endif /* SHAPE */

WOB_METHOD       BarClass[] = {
			      0,	/* METHODS_ARRAY */
			      WobEval,
			      WobPrint,
			      WobRelease,
			      WobExecute,
			      WobSet,
			      WobGetCValue,
			      (WOB_METHOD) BarOpen,
			      BarClose,
			      BarEventHandler,
                              (WOB_METHOD) wool_undefined_method_1,
			      WobGetDimensions,
                              (WOB_METHOD) wool_undefined_method_2,
                              (WOB_METHOD) wool_undefined_method_2,
                              ReconfigureBar,
			(WOB_METHOD) wool_undefined_method_2,
			(WOB_METHOD) wool_undefined_method_1,
			(WOB_METHOD) wool_undefined_method_1,
			(WOB_METHOD) wool_undefined_method_1,
			(WOB_METHOD) wool_undefined_method_1,
			(WOB_METHOD) wool_undefined_method_1
};

 /* routines */

/*
 * NewBar
 * Creates a new bar object from a WOOL_Bar description
 * Warning: a plug may be NULL to indicate an extensible space
 */

Bar 
NewBar(parent, wl_bar, dir)
Wob                     parent;
WOOL_Bar		wl_bar;
short                   dir;
{
    Bar             bar = (Bar) NewWob(sizeof(struct _Bar)
		       + sizeof(Plug) * Max(0, (wl_bar -> plugs_size - 1)));
    int             i;
    WOOL_OBJECT     object;

    wl_bar = (WOOL_Bar) wool_type_or_evaluate(wl_bar, WLBar);
    bar -> type = BarClass;
    bar -> parent = parent;
    bar -> direction = dir;
    bar -> elength = 0;
    bar -> ewidth = (wl_bar -> plugs_size ? 1 : 0);

    /* set up the box info */
    bar -> box.width = bar -> box.height = wl_bar -> min_width;
    bar -> min_width = wl_bar -> min_width;
    bar -> max_width = wl_bar -> max_width;
    bar -> box.borderwidth = wl_bar -> borderwidth;
    bar -> box.borderpixel = wl_bar -> borderpixel;
    bar -> box.background = wl_bar -> background;
    bar -> plug_separator = wl_bar -> plug_separator;
    increase_reference(bar -> menu = 
                       wool_type_or_evaluate(wl_bar -> menu, WLMenu));
    increase_reference(bar -> property = (WOOL_OBJECT) wl_bar -> property);
    increase_reference(bar -> bordertile =
		     wool_type_or_evaluate(wl_bar -> bordertile, WLPixmap));
    increase_reference(bar -> fsm =
		       wool_type_or_evaluate(wl_bar -> fsm, WLFsm));
    increase_reference(bar -> cursor =
		       wool_type_or_evaluate(wl_bar -> cursor, WLCursor));
    increase_reference(bar -> tile = (wl_bar -> tile == TRU ? wl_bar -> tile :
		       wool_type_or_evaluate(wl_bar -> tile, WLPixmap)));

    /* then recursively sets plug infos */
    bar -> nplugs = wl_bar -> plugs_size;
    for (i = 0; i < wl_bar -> plugs_size; i++) {
      object = (WOOL_OBJECT) wl_bar -> plugs[i];
      if ((object != NIL) && 
          (object -> type != WLBar) && 
          (object -> type != WLPlug)) {
	object = WOOL_send(WOOL_eval, object, (object));
        if (object == UNDEFINED_WOOL_VALUE)
          wool_error(UNDEFINED_VARIABLE, "");
        else if ((object != NIL) && 
                 (object -> type != WLBar) && 
                 (object -> type != WLPlug))
          bad_argument(object, 0, "PLUG or BAR");
      }
      if (object -> type == WLPlug) {
        bar -> plugs[i] = (Wob) NewPlug(bar, object);
        bar -> ewidth = 0;
      }
      else if (object -> type == WLBar) {
        bar -> plugs[i] = (Wob) NewBar(bar, object, !dir);
        if (((Bar) bar -> plugs[i]) -> ewidth) bar -> elength = 1;
        if (!(((Bar) bar -> plugs[i]) -> elength)) 
          bar -> ewidth = 0;
      }
      else {
        bar -> plugs[i] = (Wob) NULL;
        bar -> elength = 1;
      }
    }
    if (bar -> ewidth == -1) 
      bar -> ewidth = 0;
    return bar;
}

BarClose(bar)
Bar             bar;
{
    int             i;

    if(!bar) return;
    for (i = 0; i < bar -> nplugs; i++) {
	if (bar -> plugs[i])
          if (bar -> plugs[i] -> type == PlugClass)
	    PlugClose(bar -> plugs[i]);
          else if (bar -> plugs[i] -> type == BarClass)
	    BarClose(bar -> plugs[i]);
    }
    WobRelease(bar);
}

/*
 * Open a Bar
 */

Bar
BarOpen(bar)
Bar             bar;
{
    int             i;

    if(!bar)
    	return bar;
    check_window_size(bar);
    bar -> hook = XCreateSimpleWindow(dpy, bar -> parent -> hook,
				      bar -> box.x, bar -> box.y,
				      bar -> box.width, bar -> box.height,
				      bar -> box.borderwidth,
				      bar -> box.borderpixel,
				      bar -> box.background);
    if (bar -> parent -> type == ScreenClass)
	bar -> status |= TopLevelXWindowStatus;
    WobRecordHook(bar);
    if (bar -> cursor != NIL)
	XDefineCursor(dpy, bar -> hook,
		      ((WOOL_Cursor) bar -> cursor) -> cursor);
    if (bar -> tile != NIL && bar -> tile != TRU)
	XSetWindowBackgroundPixmap(dpy, bar -> hook,
				   ((WOOL_Pixmap) bar -> tile) -> pixmap);
    if (bar -> bordertile != NIL)
	XSetWindowBorderPixmap(dpy, bar -> hook,
		      ((WOOL_Pixmap) bar -> bordertile) -> pixmap);

    bar -> curstate = (int) WOOL_send(WOOL_open, bar -> fsm, (bar -> fsm));
    for (i = 0; i < bar -> nplugs; i++) {
	if (bar -> plugs[i])
	    WOOL_send(WOOL_open, bar -> plugs[i], (bar -> plugs[i]));
    }
    bar -> input_mask = WobMask | ((WOOL_Fsm) bar -> fsm) -> mask;
    XSelectInput(dpy, bar -> hook, bar -> input_mask);
#ifdef SHAPE
    if (BarIsShaped(bar)) {
      bar -> shaped = 1;
      UpdateBarShape(bar);
    }
#endif /* SHAPE */
    XMapWindow(dpy, bar -> hook);
    XMapSubwindows(dpy, bar -> hook);
    return bar;
}

int 
CalcNaturalBarWidth(bar)
Bar             bar;
{
    Plug  *plugs;
    int             i, curr_width = 1;

    plugs = (Plug *) & ((bar -> plugs)[0]);

    if (bar -> direction == HORIZONTAL) {
      if (!bar -> nplugs && bar -> tile != NIL && bar -> tile != TRU)	 
	curr_width = ((WOOL_Pixmap) bar -> tile) -> height; 
      for (i = 0; i < bar -> nplugs; i++) {
	if (plugs[i]) 
          if (plugs[i] -> type == PlugClass) {
            UpdatePlugGeometry(plugs[i]);
            curr_width = Max(curr_width,
               plugs[i] -> box.height + 2 * plugs[i] -> box.borderwidth);
          } else {
            curr_width = Max(curr_width,
               CalcNaturalBarLength(plugs[i]) + 2 * plugs[i] -> box.borderwidth);
          }
      }
      curr_width = Min(bar -> max_width, Max(bar -> min_width, curr_width));
      return curr_width;
    } else {
      if (!bar -> nplugs && bar -> tile != NIL && bar -> tile != TRU)
	curr_width = ((WOOL_Pixmap) bar -> tile) -> width; 
      for (i = 0; i < bar -> nplugs; i++) {
	if (plugs[i])
          if (plugs[i] -> type == PlugClass) {
            UpdatePlugGeometry(plugs[i]);
            curr_width = Max(curr_width,
               plugs[i] -> box.width + 2 * plugs[i] -> box.borderwidth);
          } else {
            curr_width = Max(curr_width,
               CalcNaturalBarLength(plugs[i]) + 2 * plugs[i] -> box.borderwidth);
          }
      }
      curr_width = Min(bar -> max_width, Max(bar -> min_width, curr_width));
      return curr_width;
    }
}

int
CalcNaturalBarLength(bar)
Bar		bar;
{
    Plug  *plugs;
    int   i, tmp;
    int   current_pos = 0;
    int   space_len = 0, n_spaces = 0;

    plugs = (Plug *) & ((bar -> plugs)[0]);
    if (bar -> direction == HORIZONTAL) {
      for (i = 0; i < bar -> nplugs; i++)
	if (!plugs[i]) {
          n_spaces++;
          current_pos += space_len;
        } else if (plugs[i] -> type == PlugClass) {
          UpdatePlugGeometry(plugs[i]);
          current_pos += plugs[i] -> box.width
            + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
        } else {
          tmp = CalcNaturalBarWidth(plugs[i]);
          if (((Bar) plugs[i]) -> ewidth)
            if (tmp > space_len) {
              current_pos += n_spaces * (tmp - space_len);
              n_spaces++;
              space_len = tmp;
            } else {
              n_spaces++;
              tmp = space_len;
            }
          current_pos += tmp
            + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
        }
      if (current_pos)
	current_pos -= bar -> plug_separator;
      else if (!bar -> nplugs && bar -> tile != NIL && bar -> tile != TRU)
	current_pos = ((WOOL_Pixmap) bar -> tile) -> width; 
      else
        current_pos = 1;
    } else {
      for (i = 0; i < bar -> nplugs; i++)
	if (!plugs[i]) {
          n_spaces++;
          current_pos += space_len;
        } else if (plugs[i] -> type == PlugClass) {
          UpdatePlugGeometry(plugs[i]);
          current_pos += plugs[i] -> box.height
            + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
        } else {
          tmp = CalcNaturalBarWidth(plugs[i]);
          if (((Bar) plugs[i]) -> ewidth)
            if (tmp > space_len) {
              current_pos += n_spaces * (tmp - space_len);
              n_spaces++;
              space_len = tmp;
            } else {
              n_spaces++;
              tmp = space_len;
            }
          current_pos += tmp
            + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
        }
      if (current_pos)
	current_pos -= bar -> plug_separator;
      else if (!bar -> nplugs && bar -> tile != NIL && bar -> tile != TRU)
	current_pos = ((WOOL_Pixmap) bar -> tile) -> height; 
      else
        current_pos = 1;
    }
    return current_pos;
}

int
CalcMinBarLength(bar)
Bar		bar;
{
    Plug  *plugs;
    int             i, current_pos = 0;

    plugs = (Plug *) & ((bar -> plugs)[0]);
    if (bar -> direction == HORIZONTAL) {
      for (i = 0; i < bar -> nplugs; i++)
	if (plugs[i])
          if (plugs[i] -> type == PlugClass) {
            UpdatePlugGeometry(plugs[i]);
            current_pos += plugs[i] -> box.width
              + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
          } else {
            current_pos += bar -> plug_separator + 2 * plugs[i] -> box.borderwidth
              + (((Bar) plugs[i]) -> ewidth ? 1 : CalcNaturalBarWidth(plugs[i]));
          }
      if (current_pos) {
	current_pos -= bar -> plug_separator;
      }
    } else {
      for (i = 0; i < bar -> nplugs; i++)
	if (plugs[i])
          if (plugs[i] -> type == PlugClass) {
            UpdatePlugGeometry(plugs[i]);
            current_pos += plugs[i] -> box.height
              + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
          } else {
            current_pos += bar -> plug_separator + 2 * plugs[i] -> box.borderwidth
              + (((Bar) plugs[i]) -> ewidth ? 1 : CalcNaturalBarWidth(plugs[i]));
          }
      if (current_pos) {
	current_pos -= bar -> plug_separator;
      }
    }
    return current_pos;
}


/*
 * Called BEFORE adjusting the client
 * Here we take a bar setup, and suppose that its box data is updated.
 * Then we ask for the dimension of plugs and proceed to position them.
 * Adjust the width of the bars
 */
 
int
UpdateBarWidth(bar)
Bar		bar;
{
    Plug  *plugs;
    int             i;

    if (!bar)
	return 0;

    if (bar -> direction == HORIZONTAL) {
	bar -> box.height = CalcNaturalBarWidth(bar);
    } else {
	bar -> box.width = CalcNaturalBarWidth(bar);
    }
   
    return 2 * bar -> box.borderwidth + (bar -> direction == HORIZONTAL ?
                                         bar -> box.height : bar -> box.width);
}


/*
 * Called AFTER adjusting the client
 * Adjust space in the length of the bar.
 * If we encounter a NULL plug, treat it as an extensible space
 */

UpdateBarLength(bar)
Bar		bar;
{
    Plug  *plugs;
    int             i, n = 0, current_pos = 0, n_spaces = 0;
    int             total_space, delta = 0, shift = 0;

    if (!bar)
	return;
    plugs = (Plug *) & ((bar -> plugs)[0]);
    for (i = 0; i < bar -> nplugs; i++)
	if (!plugs[i])
          n_spaces++;
        else if ((plugs[i] -> type == BarClass) && ((Bar) plugs[i]) -> ewidth)
          n_spaces++;
    total_space = (bar -> direction == HORIZONTAL ? bar -> box.width
		 : bar -> box.height) - CalcMinBarLength(bar);
    if (n_spaces && (total_space > 0)) {
	shift = total_space / n_spaces;
        delta = total_space % n_spaces;
    }

    if (bar -> direction == HORIZONTAL) {
      for (i = 0; i < bar -> nplugs; i++)
	if (plugs[i])
          if (plugs[i] -> type == PlugClass) {
            plugs[i] -> box.x = current_pos;
            plugs[i] -> box.y = (bar -> box.height
                                 - plugs[i]->box.height - 2 * plugs[i] -> box.borderwidth) / 2;
            current_pos += plugs[i] -> box.width
              + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
          } else {
            plugs[i] -> box.x = current_pos;
            plugs[i] -> box.y = 0;
            if (((Bar) plugs[i]) -> ewidth)
              plugs[i] -> box.width = 1 + (++n == n_spaces ? shift+delta : shift);
            else if (bar -> nplugs == 1)
              plugs[i] -> box.width = bar -> box.width - 2 * plugs[i] -> box.borderwidth;
            else
              plugs[i] -> box.width = CalcNaturalBarWidth(plugs[i]);
            plugs[i] -> box.height = bar -> box.height - 2 * plugs[i] -> box.borderwidth;
            UpdateBarLength(plugs[i]);
            current_pos += plugs[i] -> box.width
              + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
          }
        else {
          current_pos += (++n == n_spaces ? shift+delta : shift);
        }
    } else {
      for (i = 0; i < bar -> nplugs; i++)
	if (plugs[i])
          if (plugs[i] -> type == PlugClass) {
            plugs[i] -> box.y = current_pos;
            plugs[i] -> box.x = (bar -> box.width
                                 - plugs[i]->box.width - 2 * plugs[i] -> box.borderwidth) / 2;
            current_pos += plugs[i] -> box.height
              + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
          } else {
            plugs[i] -> box.y = current_pos;
            plugs[i] -> box.x = 0;
            if (((Bar) plugs[i]) -> ewidth)
              plugs[i] -> box.height = 1 + (++n == n_spaces ? shift+delta : shift);
            else if (bar -> nplugs == 1)
              plugs[i] -> box.height = bar -> box.height - 2 * plugs[i] -> box.borderwidth;
            else
              plugs[i] -> box.height = CalcNaturalBarWidth(plugs[i]);
            plugs[i] -> box.width = bar -> box.width - 2 * plugs[i] -> box.borderwidth;
            UpdateBarLength(plugs[i]);
            current_pos += plugs[i] -> box.height
              + bar -> plug_separator + 2 * plugs[i] -> box.borderwidth;
          }
        else {
          current_pos += (++n == n_spaces ? shift+delta : shift);
        }
    }
}

int
NaturalBarLength(bar)
Bar		bar;
{
    Plug  *plugs;
    int             i;

    if (!bar)
	return 0;
    if (bar -> direction == HORIZONTAL) {
      bar -> box.width = CalcNaturalBarLength(bar);
    } else {
      bar -> box.height = CalcNaturalBarLength(bar);
    }
    return 2 * bar -> box.borderwidth + (bar -> direction == HORIZONTAL ?
                                         bar -> box.width : bar -> box.height);
}

BarEventHandler(bar, evt)
Bar	bar;
XEvent	*evt;
{
    int             i;

    switch (evt -> type) {
    case Expose:
	XClearWindow(dpy, bar -> hook);
	break;
    case GWMUserEvent:			/* TODO: no more test on plugs masks*/
	WLFsm_action(bar -> fsm, bar, evt);
	if (GWM_Propagate_user_events)
	    for (i = 0; i < bar -> nplugs; i++)
		if ((bar -> plugs[i])) {
		    WOOL_send(WOOL_process_event, bar -> plugs[i],
			      (bar -> plugs[i], evt));
		}
	break;
    default:
	WLFsm_action(bar -> fsm, bar, evt);
    }
}

ReconfigureBar(bar, culprit)
Bar	bar;
Wob	culprit;		/* parent or plug */
{
    int             i, width, height, must_resize = 1;

    if (!bar)
	return;
    width = bar -> box.width;
    height = bar -> box.height;
    if (culprit != (Wob) bar -> parent) {	/* from plug or bar below */
	UpdateBarWidth(bar);
        WOOL_send(WOOL_reconfigure, bar -> parent, (bar -> parent, bar));
    }
    else {                                      /* from above */
      if (culprit && culprit -> type != BarClass)
        UpdateBarLength(bar); 
      XMoveResizeWindow(dpy, bar -> hook, bar -> box.x, bar -> box.y,
                        bar -> box.width, bar -> box.height);
      for (i = 0; i < bar -> nplugs; i++)
	if (bar -> plugs[i])
          if (bar -> plugs[i] -> type == PlugClass)
	    ReconfigurePlug(bar -> plugs[i], bar);
          else
            ReconfigureBar(bar -> plugs[i], bar);
#ifdef SHAPE
      if (bar -> shaped || (bar -> shaped = BarIsShaped(bar)))
        UpdateBarShape(bar);
#endif /* SHAPE */
      XClearWindow(dpy, bar -> hook);
    }
}

set_bar_bitmap(bar, wl_pixmap)
Bar     bar;
WOOL_OBJECT     wl_pixmap;
{
    int shaped_tile = 0;
    if (((wl_pixmap -> type == WLAtom) && (wl_pixmap != TRU) && (wl_pixmap != NIL))
         || wl_pixmap -> type == WLList)
	return;
    if (bar -> tile == TRU)
      shaped_tile = -1;
    else if (TileIsShaped(bar -> tile))
      shaped_tile = 1;
    decrease_reference(bar -> tile);
    increase_reference(bar -> tile = wl_pixmap);
    if ((shaped_tile == -1) && (bar -> tile == TRU))
      shaped_tile = 0;        /* No reconfigure necessary if it remains TRU */
    else if ((bar -> tile == TRU) || TileIsShaped(bar -> tile))
      shaped_tile = 1;
    if (wl_pixmap == NIL) 
      XSetWindowBackground(dpy, bar->hook, bar -> box.background);
    else if (wl_pixmap -> type == WLPixmap)
      XSetWindowBackgroundPixmap(dpy, bar->hook,
                                 ((WOOL_Pixmap) wl_pixmap) -> pixmap);
    /* Reconfigure only if anything will change */
    if (shaped_tile || 
        ((!bar -> nplugs) &&
         ((bar -> direction == HORIZONTAL ? bar -> box.height : bar -> box.width) != CalcNaturalBarWidth(bar)))) {
        ReconfigureBar(bar, 0);
    }
    XClearWindow(dpy, bar -> hook);
}

#ifdef SHAPE
/* non-rectangular extension */

int
BarIsShaped(bar)
Bar     bar;
{
  int i;
  if ((bar -> tile == TRU) || TileIsShaped(bar -> tile))
    return 1;
  for (i = 0; i < bar -> nplugs; i++)
    if (bar -> plugs[i])
      if (bar -> plugs[i] -> type == PlugClass) {
        if (((Plug) bar -> plugs[i]) -> graphic -> type == WLPixmap
            && (((WOOL_Pixmap) (((Plug) bar -> plugs[i]) -> graphic))->mask))
          return 1;
      } else {
        if (((Bar) bar -> plugs[i]) -> shaped)
          return 1;
      }
  return 0;
}

UpdateBarShape(bar)
Bar     bar;
{
  XRectangle rect, rect2;
  Plug *plugs;
  int i;
  int shaped_tile = TileIsShaped(bar -> tile);

  rect.x = - bar -> box.borderwidth;
  rect.y = - bar -> box.borderwidth;
  rect.width = bar -> box.width + 2 * bar -> box.borderwidth;
  rect.height = bar -> box.height + 2 * bar -> box.borderwidth;
  XShapeCombineRectangles(dpy, bar -> hook, ShapeBounding, 
                          0, 0,
                          &rect, 1, ShapeSet, 0); 
					/* transparent tile */
  if (bar -> tile == TRU || shaped_tile) {
    rect2.x = 0;
    rect2.y = 0;
    rect2.width = bar -> box.width;
    rect2.height = bar -> box.height;
    XShapeCombineRectangles(dpy, bar -> hook, ShapeBounding,
                            0, 0,
                            &rect2, 1, ShapeSubtract, 0);
  }
					/* shaped tile */
  if (shaped_tile) {
      int x_offset, y_offset;
      /* we do the tiling ourselves by hand */
      for (x_offset = 0; x_offset < bar -> box.width; 
			 x_offset += ((WOOL_Pixmap) (bar -> tile)) -> width) {
	  for (y_offset = 0; y_offset < bar -> box.height; 
			     y_offset += ((WOOL_Pixmap) (bar -> tile))->height) {
	      XShapeCombineMask(dpy, bar -> hook, ShapeBounding,
				x_offset, y_offset,
				((WOOL_Pixmap) (bar -> tile)) -> mask,
				ShapeUnion);
	  }
      }
  }

  plugs = (Plug *) & ((bar -> plugs)[0]);
  for (i = 0; i < bar -> nplugs; i++)
    if (plugs[i]) {
        rect2.x = plugs[i] -> box.x + plugs[i] -> box.borderwidth;
        rect2.y = plugs[i] -> box.y + plugs[i] -> box.borderwidth;
        rect2.width = plugs[i] -> box.width;
        rect2.height = plugs[i] -> box.height;
        XShapeCombineRectangles(dpy, bar -> hook, ShapeBounding,
                                0, 0,
                                &rect2, 1, ShapeSubtract, 0); 
        XShapeCombineShape(dpy, bar -> hook, ShapeBounding,
                           rect2.x, 
                           rect2.y, 
                           plugs[i] -> hook, ShapeBounding,
                           ShapeUnion);
    }
  XShapeCombineRectangles(dpy, bar -> hook, ShapeBounding, 
                          0, 0,
                          &rect, 1, ShapeIntersect, 0); 
}

#endif /* SHAPE */

