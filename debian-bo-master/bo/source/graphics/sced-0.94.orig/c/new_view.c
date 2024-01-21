#define PATCHLEVEL 0
/*
**    ScEd: A Constraint Based Scene Editor.
**    Copyright (C) 1994-1995  Stephen Chenney (stephen@cs.su.oz.au)
**
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
**    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
**	sced: A Constraint Based Object Scene Editor
**
**	new_view.c: Code for changing viewports.
**
**
**	External Functions:
**
**	void
**	Start_Newview_Rotation(Widget, XEvent*, String*, Cardinal*)
**	Begins a new viewpoint rotation.  It is an action procedure which should
**	only be called when a button goes down, with a parameter specifying
**	the type of rotation.
**
**	void
**	Newview_Rotation(Widget, XEvent*, String*, Cardinal*)
**	An action procedure called to update the screen during a new viewpoint
**	rotation.  Should be called for PtrMotion events.
**
**	void
**	Stop_Newview_Rotation(Widget, XEvent*, String*, Cardinal*)
**	Complete a new view rotation.  Should be called for a button up event.
**
**	void
**	Apply_Viewfrom_Text(Widget w)
**	Called to set the viewpoint to the text entered in the text box.
*/

#include <math.h>
#include <sced.h>
#include <instance_list.h>
#include <select_point.h>
#include <View.h>
#include <X11/Xaw/Text.h>

#if HAVE_STRING_H
#include <string.h>
#elif HAVE_STRINGS_H
#include <strings.h>
#endif

enum RotType { both, horiz, vert, rotundef };
enum DistSpeed { fast, med, slow, distundef };



static WindowInfoPtr	window;
static enum RotType	rotation_type = rotundef;
static Boolean		panning;
static enum DistSpeed move_speed = distundef;
static double		*distance;
static Matrix		up_coord_system;
static Matrix		inverse_coords;
static double		scale_to_top;
static double		scale_to_bottom;
static double		horiz_scale;
static double		init_theta, init_phi;
static Vector		start_eye;
static XPoint		start_pt;
static double		start_dist;

Widget	change_viewfrom_shell;
Widget	change_viewfrom_dialog;


/*	void
**	Start_Newview_Rotation(Widget, XEvent*, String*, Cardinal*)
**	Begins a new viewpoint rotation.  It is an action procedure which should
**	only be called when a button goes down, with a parameter specifying
**	the type of rotation.
*/
void
Start_Newview_Rotation(Widget w, XEvent *event, String *params, Cardinal *num_p)
{
	double	cosx, sinx;
	Vector	start_view;
	double	temp_d;
	int		i;

	panning = FALSE;
	for ( i = 0 ; i < *num_p ; i++ )
	{
		/* Get what type of rotation this is. */
		if (!(strcmp(params[i], "both")))
			rotation_type = both;
		else if (!(strcmp(params[i], "horiz")))
			rotation_type = horiz;
		else if (!(strcmp(params[i], "vert")))
			rotation_type = vert;
		else if (!(strcmp(params[i], "pan")))
			panning = TRUE;
	}

	if ( rotation_type == rotundef )
		return;

	/* Work out which window it's in. */
	if ( w == main_window.view_widget )
		window = &main_window;
	else
		window = &csg_window;

	/*
	**	Get various initial parameters.
	*/
	start_pt.x = event->xbutton.x;
	start_pt.y = event->xbutton.y;
	VScalarMul(window->viewport.view_from,
			   window->viewport.view_distance + window->viewport.eye_distance,
			   start_eye);
	VAdd(start_eye, window->viewport.view_at, start_eye);

	/*	Need to build a new coord system with up as the "z" axis.	*/
	up_coord_system.z = window->viewport.view_up;
	if ( IsZero(window->viewport.view_up.z) &&
		 IsZero(window->viewport.view_up.x) )
		VNew(1.0, 0, 0, up_coord_system.x);
	else
		VNew(window->viewport.view_up.z, 0.0, -window->viewport.view_up.x,
			up_coord_system.x);
	VUnit(up_coord_system.x, temp_d, up_coord_system.x);
	VCross(window->viewport.view_up, up_coord_system.x, up_coord_system.y);
	inverse_coords = MInvert(&up_coord_system);

	/* The start_view is a vector in the up_coord space out to the viewpoint.*/
	MVMul(up_coord_system, window->viewport.view_from, start_view);

	/* We want its spherical co-ordinates.	*/
	if ( start_view.z > 1.0 )  start_view.z = 1.0;
	if ( start_view.z < -1.0 )  start_view.z = -1.0;
	init_phi = acos(start_view.z);
	sinx = sin(init_phi);
	if ( IsZero(sinx) )
	{
		cosx = 1.0;
		sinx = 0.0;
	}
	else
	{
		cosx = start_view.x / sinx;
		sinx = start_view.y / sinx;
	}
	if ( cosx > 1.0 ) cosx = 1.0;
	if ( cosx < -1.0 ) cosx = -1.0;
	init_theta = (sinx > 0) ? acos(cosx) : -acos(cosx);

	/* We also want scaling factors in each direction. */
	/* Rig it so that the top of the screen corresponds to the top pole
	** (as defined by view_up) and the bottom of the screen corresponds
	** to the bottom pole (-view_up).  Rotation from one side to the other
	** will produce one complete circuit of the world.
	*/
	if ( start_pt.y == 0 ) start_pt.y++;
	if ( start_pt.y == (int)window->height ) start_pt.y--;
	scale_to_top = (M_PI - init_phi) / start_pt.y;
	scale_to_bottom = init_phi / ((double)window->height - start_pt.y);
	horiz_scale = 2 * M_PI / (double)window->width;

	/* Call View_Update to set up the new window. */
	View_Update(window, window->all_instances, RemoveHidden);
}


/*	void
**	Newview_Rotation(Widget, XEvent*, String*, Cardinal*)
**	An action procedure called to update the screen during a new viewpoint
**	rotation.  Should be called for PtrMotion events.
*/
void
Newview_Rotation(Widget w, XEvent *event, String *params, Cardinal *num_p)
{
	Vector	new_view;
	double	delta_theta, delta_phi;
	double	sinx;
	char	prompt_string[ENTRY_STRING_LENGTH];

	if ( rotation_type == rotundef ||
		 ! ( event->xmotion.state & ( Button1Mask|Button2Mask|Button3Mask )))
		return;

	/* Cut it to the top and bottom of the screen. */
	if (event->xmotion.y >= (short)window->height)
		event->xmotion.y = (short)window->height - 1;
	if (event->xmotion.y <= 0) event->xmotion.y = 1;

	/* Work out the change in the azimuthal (?) angle. */
	/* It's related to the x displacement of the mouse. */
	if (rotation_type != vert)
	{
		if ( panning )
			delta_theta = (event->xmotion.x - start_pt.x) * horiz_scale * 0.1;
		else
			delta_theta = (start_pt.x - event->xmotion.x) * horiz_scale;
	}
	else
		delta_theta = 0;

	/* Work out the polar angle.  It comes from the y displacement. */
	if (rotation_type != horiz)
	{
		if ( panning )
			delta_phi = ( event->xmotion.y - start_pt.y ) * 0.1 *
			((event->xmotion.y < start_pt.y) ? scale_to_top : scale_to_bottom);
		else
			delta_phi = (start_pt.y - event->xmotion.y ) *
			((event->xmotion.y < start_pt.y) ? scale_to_top : scale_to_bottom);
	}
	else
		delta_phi = 0;

	/* Calculate the new view_from vector. */
	sinx = sin(init_phi + delta_phi);
	VNew(sinx * cos(init_theta + delta_theta),
		 sinx * sin(init_theta + delta_theta),
		 cos(init_phi + delta_phi), new_view);
	MVMul(inverse_coords, new_view, window->viewport.view_from);

	if ( panning )
	{
		VScalarMul(window->viewport.view_from,
			-(window->viewport.view_distance + window->viewport.eye_distance),
			new_view);
		VAdd(new_view, start_eye, window->viewport.view_at);
	}

 	Build_Viewport_Transformation(&(window->viewport));

 	View_Update(window, window->all_instances, CalcView | RemoveHidden);

	sprintf(prompt_string, "%1.3g %1.3g %1.3g", window->viewport.view_from.x,
			window->viewport.view_from.y, window->viewport.view_from.z);
	Set_Prompt(window, prompt_string);
}


/*	void
**	Stop_Newview_Rotation(Widget, XEvent*, String*, Cardinal*)
**	Complete a new view rotation.  Should be called for a button up event.
*/
void
Stop_Newview_Rotation(Widget w, XEvent *event, String *params, Cardinal *num_p)
{
	if ( rotation_type == rotundef )
		return;

	View_Update(window, window->all_instances, ViewNone );
	Update_Projection_Extents(window->all_instances);

	rotation_type = rotundef;
}



void
Apply_Viewfrom_Text(WindowInfoPtr w)
{
	Vector	original;

	original = w->viewport.view_from;
	sscanf(w->text_string, "%lg %lg %lg", &(w->viewport.view_from.x),
			&(w->viewport.view_from.y), &(w->viewport.view_from.z));

 	if ( ! Build_Viewport_Transformation(&(w->viewport)))
 	{
 		/* The user entered an invalid vector of some sort. */
 		/* Just replace the old one. */
 		w->viewport.view_from = original;

 		Build_Viewport_Transformation(&(w->viewport));
 	}

	Cancel_Viewport_Change(w);

	View_Update(w, w->all_instances, CalcView);
	Update_Projection_Extents(w->all_instances);
}


/*	void
**	Start_Distance_Change(Widget, XEvent*, String*, Cardinal*)
**	Begins a new view distance change operation.  It is an action procedure
**	which should only be called when a button goes down, with a parameter
**	specifying the speed of motion.
*/
void
Start_Distance_Change(Widget w, XEvent *event, String *params, Cardinal *num_p)
{
	int	i;

	if ( w == main_window.view_widget )
		window = &main_window;
	else
		window = &csg_window;

	/* Get what type of rotation this is. */
	for ( i = 0 ; i < *num_p ; i++ )
	{
		if (!(strcmp(params[i], "fast")))
			move_speed = fast;
		else if (!(strcmp(params[i], "med")))
			move_speed = med;
		else if (!(strcmp(params[i], "slow")))
			move_speed = slow;
		else if ( ! ( strcmp(params[i], "view") ) )
			distance = &(window->viewport.view_distance);
		else if ( ! ( strcmp(params[i], "eye") ) )
			distance = &(window->viewport.eye_distance);
	}

	if ( move_speed == distundef )
		return;

	/*
	**	Get various initial parameters.
	*/
	start_pt.y = event->xbutton.y;
	start_dist = *distance;


	/* Scale it depending on the motion speed. */
	/* Fast motion takes the distance to 0 by the top of the screen. */
	/* Med motion takes the distance to 50% by the top of the screen. */
	/* Slow motion takes the distance to 90% by the top of the screen. */
	switch ( move_speed )
	{
		case fast:
			scale_to_top = (1 + start_dist) / (double)start_pt.y;
			break;
		case med:
			scale_to_top = (1 + start_dist) / 2.0 / (double)start_pt.y;
			break;
		case slow:
			scale_to_top = (1 + start_dist) / 10.0 / (double)start_pt.y;
			break;
		case distundef:;
	}


	/* Call View_Update to set up the new window. */
	View_Update(window, window->all_instances, RemoveHidden);

}


/*	void
**	Distance_Change(Widget, XEvent*, String*, Cardinal*)
**	An action procedure called to update the screen during a view distance
**	change.  Should be called for PtrMotion events.
*/
void
Distance_Change(Widget w, XEvent *event, String *params, Cardinal *num_p)
{
	char	prompt_string[ENTRY_STRING_LENGTH];

	if ( move_speed == distundef )
		return;

	*distance = scale_to_top * (event->xmotion.y - start_pt.y)
						+ start_dist;
	if ( *distance < EPSILON ) *distance = EPSILON;

 	Build_Viewport_Transformation(&(window->viewport));

 	View_Update(window, window->all_instances, CalcView | RemoveHidden );

	sprintf(prompt_string, "%1.3g"HPFIX, *distance);
	Set_Prompt(window, prompt_string);
}


/*	void
**	Stop_Distance_Change(Widget, XEvent*, String*, Cardinal*)
**	Complete a view distance change operation.  Just means ungrabbing the
**	pointer.
*/
void
Stop_Distance_Change(Widget w, XEvent *event, String *params, Cardinal *num_p)
{
	if ( move_speed == distundef )
		return;

 	View_Update(window, window->all_instances, ViewNone);

	move_speed = distundef;
}



void
Apply_Distance_Text(WindowInfoPtr w, Boolean do_view)
{
	double	original;
	char	prompt_string[ENTRY_STRING_LENGTH];

	if ( do_view )
		distance = &(w->viewport.view_distance);
	else
		distance = &(w->viewport.eye_distance);

	original = *distance;
	sscanf(w->text_string, "%lf", distance);

	Cancel_Viewport_Change(w);

	if (*distance <= EPSILON)
 	{
 		/* The user entered an invalid value of some sort. */
 		/* Just replace the old one. */
 		*distance = original;

		sprintf(prompt_string, "%1.3g"HPFIX, *distance);
		Set_Prompt(w, prompt_string);
 	}
 	else
 	{
 		Build_Viewport_Transformation(&(w->viewport));
		View_Update(w, w->all_instances, CalcView);
		Update_Projection_Extents(w->all_instances);
	}
}


void
Prepare_Change_Look(WindowInfoPtr new_window)
{
	InstanceList	axis_elmt = NULL;

	window = new_window;

	Insert_Element(&axis_elmt, &(window->axes));
	if ( window == &main_window && ( camera_object.o_flags & ObjVisible ) )
		Insert_Element(&axis_elmt, &camera_object);
	select_instances = Merge_Selection_Lists(window->all_instances,
											 axis_elmt);
	allow_text_entry = TRUE;

	Prepare_Selection_Drawing();
	Select_Highlight_Closest(XtWindow(window->view_widget));
	Draw_Selection_Points(XtWindow(window->view_widget));
}


int
Change_Lookat_Point_Callback(ConstraintSpecPtr spec)
{
	Vector	original;

	original = window->viewport.view_at;

	window->viewport.view_at = Spec_Absolute(spec, window->viewport.view_at);

	if ( ! Build_Viewport_Transformation(&(window->viewport)) )
	{
		window->viewport.view_at = original;
		Build_Viewport_Transformation(&(window->viewport));
	}

	Free_Selection_List(select_instances);
	Cancel_Change_Look_Event(window);

	View_Update(window, window->all_instances, CalcView);
	Update_Projection_Extents(window->all_instances);

	return IGNORE;
}


static void
Change_Lookup_Vector(Vector start, Vector end)
{
	Vector	original;
	Vector	new;
	double	temp_d;

	original = window->viewport.view_up;

	VSub(end, start, new);
	if ( ! VZero(new) )
		VUnit(new, temp_d, window->viewport.view_up);
	else
		XBell(XtDisplay(main_window.shell), 0);

	if ( ! Build_Viewport_Transformation(&(window->viewport)) )
	{
		window->viewport.view_up = original;
		Build_Viewport_Transformation(&(window->viewport));
	}

	Free_Selection_List(select_instances);
	Cancel_Change_Look_Event(window);

	View_Update(window, window->all_instances, CalcView);
}


int
Change_Lookup_Point_Callback(ConstraintSpecPtr spec)
{
	static ConstraintSpec	start_spec;
	static Vector	start;
	static int		num_so_far = 0;
	Vector	new, diff;

	if ( num_so_far )
	{
		new = Spec_Absolute(spec, select_center);
		if ( VEqual(new, start, diff) )
		{
			Select_Deselect_Point(&start_spec);
			num_so_far = 0;
			return IGNORE;
		}

		Change_Lookup_Vector(start, new);

		num_so_far = 0;
	}
	else
	{
		/* This is the first. */
		start = Spec_Absolute(spec, select_center);
		start_spec = *spec;
		num_so_far = 1;
		return SELECT;
	}

	return IGNORE;
}

