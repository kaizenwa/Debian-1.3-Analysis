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
**	base_objects.c : Functions dealing with base objects.
**
**	Created: 13/03/94
**
**	External functions (base_objects.h):
**
**	Boolean
**	Initialize_Base_Objects()
**	Creates base objects for each of the generic shapes and initializes them
**	appropriately.
**
**	BaseObjectPtr
**	Get_Base_Object_From_Label(String label);
**	Returns the base object with label label.  NULL if there is none.
**
**	Boolean
**	Add_Instance_To_Base(ObjectInstancePtr new, BaseObjectPtr base_obj)
**	Adds a new instance to the base base_obj.
**	Returns True on success, False on failure.
**
**	void
**	Remove_Instance_From_Base(ObjectInstancePtr victim)
**	Removes victim from its parents instance list.
**
*/

#include <sced.h>
#include <base_objects.h>
#include <bezier.h>
#include <csg.h>
#include <csg_wire.h>
#include <edge_table.h>
#include <gen_wireframe.h>
#include <instance_list.h>
#include <torus.h>
#include <triangle.h>
#include <SimpleWire.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Tree.h>

#if HAVE_STRING_H
#include <string.h>
#elif HAVE_STRINGS_H
#include <strings.h>
#endif

/* Internal functions. */
static BaseObjectPtr Create_Generic_Base(GenericObject);

static void	Create_CSG_Instance_Shell();
static Boolean	Base_Determine_Modifiable_Instances(BaseObjectPtr);


/* Internal variables. */
BaseObjectList	base_objects;
int				num_base_objects;
int				num_csg_base_objects = 0;
int				num_wire_base_objects = 0;
static int		base_object_slots;


static WindowInfoPtr	current_window;
static Widget			csg_instance_shell;
static BaseObjectPtr	modify_base;
static Widget			modify_instances_button;



/*	Boolean
**	Initialize_Base_Objects()
**	Creates base objects for each of the generic shapes and initializes them
**	appropriately.
**	Returns True on success, False on failure for whatever reason (memory).
*/
Boolean
Initialize_Base_Objects()
{
	int				i;

	num_base_objects = NUM_GENERIC_OBJS;
	base_object_slots = NUM_GENERIC_OBJS;
	base_objects = New(BaseObjectPtr, NUM_GENERIC_OBJS);

	for ( i = 0 ; i < NUM_GENERIC_OBJS ; i++ )
		base_objects[i] = Create_Generic_Base(i);

	return TRUE;

}


/*	static BaseObjectPtr
**	Create_Generic_Base(GenericObject class)
**	Creates and initializes a new base object of class i.
**	Returns the new object, or NULL on failure.
*/
static BaseObjectPtr
Create_Generic_Base(GenericObject class)
{
	BaseObjectPtr	new_obj = New(BaseObject, 1);

	new_obj->b_class = class;

	new_obj->b_max_density = 0;
	new_obj->b_wireframes = New(WireframePtr, 1);

	switch (class)
	{
		case sphere_obj:
			new_obj->b_label = "sphere";
			new_obj->b_wireframes[0] = Generic_Sphere_Wireframe();
			new_obj->b_ref_num = 3;
			new_obj->b_create_func = Create_Generic_Object;
			new_obj->b_destroy_func = Destroy_Generic_Object;
			break;

		case cylinder_obj:
			new_obj->b_label = "cylinder";
			new_obj->b_wireframes[0] = Generic_Cylinder_Wireframe();
			new_obj->b_ref_num = 1;
			new_obj->b_create_func = Create_Generic_Object;
			new_obj->b_destroy_func = Destroy_Generic_Object;
			break;

		case cone_obj:
			new_obj->b_label = "cone";
			new_obj->b_wireframes[0] = Generic_Cone_Wireframe();
			new_obj->b_ref_num = 2;
			new_obj->b_create_func = Create_Generic_Object;
			new_obj->b_destroy_func = Destroy_Generic_Object;
			break;

		case cube_obj:
			new_obj->b_label = "cube";
			new_obj->b_wireframes[0] = Generic_Cube_Wireframe();
			new_obj->b_ref_num = 0;
			new_obj->b_create_func = Create_Generic_Object;
			new_obj->b_destroy_func = Destroy_Generic_Object;
			break;

		case torus_obj:
			new_obj->b_label = "torus";
			new_obj->b_wireframes[0] = Generic_Torus_Wireframe();
			new_obj->b_ref_num = 9;
			new_obj->b_create_func = Torus_Create_Function;
			new_obj->b_destroy_func = Torus_Destroy_Function;
			break;

		case plane_obj:
			new_obj->b_label = "plane";
			new_obj->b_wireframes[0] = Generic_Plane_Wireframe();
			new_obj->b_ref_num = 2;
			new_obj->b_create_func = Create_Generic_Object;
			new_obj->b_destroy_func = Destroy_Generic_Object;
			break;

		case triangle_obj:
			new_obj->b_label = "triangle";
			new_obj->b_wireframes[0] = Generic_Triangle_Wireframe();
			new_obj->b_ref_num = 0;
			new_obj->b_create_func = Triangle_Create_Function;
			new_obj->b_destroy_func = Triangle_Destroy_Function;
			break;

		case light_obj:
			new_obj->b_label = "light";
			new_obj->b_wireframes[0] = Generic_Light_Wireframe();
			new_obj->b_ref_num = 0;
			new_obj->b_create_func = Light_Create_Point_Light;
			new_obj->b_destroy_func = Light_Destroy_Object;
			break;

		case spotlight_obj:
			new_obj->b_label = "spotlight";
			new_obj->b_wireframes[0] = Generic_Spot_Light_Wireframe();
			new_obj->b_ref_num = 1;
			new_obj->b_create_func = Light_Create_Spot_Light;
			new_obj->b_destroy_func = Light_Destroy_Object;
			break;

		case arealight_obj:
			new_obj->b_label = "arealight";
			new_obj->b_wireframes[0] = Generic_Square_Wireframe();
			new_obj->b_ref_num = 0;
			new_obj->b_create_func = Light_Create_Area_Light;
			new_obj->b_destroy_func = Light_Destroy_Object;
			break;

		case dirlight_obj:
			new_obj->b_label = "dirlight";
			new_obj->b_wireframes[0] = Generic_Direction_Wireframe();
			new_obj->b_ref_num = 1;
			new_obj->b_create_func = Light_Create_Dir_Light;
			new_obj->b_destroy_func = Light_Destroy_Object;
			break;

		case bezier_obj:
			new_obj->b_label = "bezier";
			new_obj->b_wireframes[0] = Generic_Bezier_Wireframe();
			new_obj->b_ref_num = 0;
			new_obj->b_create_func = Bezier_Create_Function;
			new_obj->b_destroy_func = Bezier_Destroy_Function;
			break;

		case csg_obj:
		case wireframe_obj:
			break;
	}

	/* Build each one's edge table. */
	Edge_Table_Build(new_obj->b_wireframes[0]);

	new_obj->b_major_wires = NULL;

	/* None of the generic bases are CSG objects. */
	new_obj->b_csgptr = NULL;

	/* Allocate space for a single instance.  This keeps realloc happy. */
	new_obj->b_instances = New(ObjectInstancePtr, 1);
	new_obj->b_num_instances = 0;
	new_obj->b_num_slots = 1;

	return new_obj;
}



/*	BaseObjectPtr
**	Get_Base_Object_From_Label(String label);
**	Returns the base object with label label.  NULL if there is none.
*/
BaseObjectPtr
Get_Base_Object_From_Label(String label)
{
	int	i;

	for ( i = 0 ; i < num_base_objects ; i++ )
		if (!(strcmp(label, base_objects[i]->b_label)))
			return base_objects[i];

	return NULL;
}



/*	Boolean
**	Add_Instance_To_Base(ObjectInstancePtr new, BaseObjectPtr base_obj)
**	Adds a new instance to the base base_obj.
**	Returns True on success, False on failure.
**
**	Failure would be due to the inability to allocate space for whatever reason.
*/
Boolean
Add_Instance_To_Base(ObjectInstancePtr new, BaseObjectPtr base_obj)
{
	/* Check for space. */
	if (base_obj->b_num_slots <= base_obj->b_num_instances)
	{
		base_obj->b_num_slots += 5;
		if ((base_obj->b_instances = More(base_obj->b_instances,
			ObjectInstancePtr, base_obj->b_num_slots)) == NULL)
		{
			base_obj->b_num_slots -= 5;
			return FALSE;
		}
	}

	base_obj->b_instances[base_obj->b_num_instances++] = (void*)new;
	new->o_parent = base_obj;

	return TRUE;

}


/*	void
**	Remove_Instance_From_Base(ObjectInstancePtr victim)
**	Removes victim from its parents instance list.
*/
void
Remove_Instance_From_Base(ObjectInstancePtr victim)
{
	BaseObjectPtr	base;
	int				i, j;

	base = victim->o_parent;

	for ( i = 0 ; i < base->b_num_instances ; i++ )
	{
		if (base->b_instances[i] == victim)
			break;
	}

	if ( i == base->b_num_instances ) /* Just ignore it.  It wasn't there. */
		return;

	for ( j = i ; j < base->b_num_instances-1 ; j++ )
		base->b_instances[j] = base->b_instances[j+1];

	base->b_num_instances--;

	return;
}


/*	Deletes all the defined base objects.
*/
void
Destroy_All_Base_Objects()
{
	int	i;

	for ( i = num_base_objects - 1 ; i >= NUM_GENERIC_OBJS ; i-- )
	{
		if ( base_objects[i]->b_class == csg_obj )
			CSG_Destroy_Base_Object(NULL, base_objects[i], TRUE);
		else
			Wireframe_Destroy_Base_Object(NULL, base_objects[i]);
	}
}


/*	Highlights all the instances of base. */
static void
Base_Highlight_Instances(BaseObjectPtr base, Boolean state)
{
	int	i;

	for ( i = 0 ; i < base->b_num_instances ; i++ )
		if ( state )
			base->b_instances[i]->o_flags |= ObjSelected;
		else
			base->b_instances[i]->o_flags &= ( ObjAll ^ ObjSelected );

	View_Update(&main_window, NULL, ViewNone);
	if ( csg_window.view_widget )
		View_Update(&csg_window, NULL, ViewNone);
}


/*	void
**	CSG_Destroy_Base_Object(Widget base_widget, BaseObjectPtr base,
**							Boolean destroy)
**	Destroys the base object represented by base_widget, and all it contains.
**	Except if destroy is false, in which case it destroys the base object
**	but sends it's tree off to be edited.
**	If base_widget is NULL, assumes value already in base.
*/
void
CSG_Destroy_Base_Object(Widget base_widget, BaseObjectPtr base, Boolean destroy)
{
	int		victim_index, widget_index;
	int		i;

	if ( base_widget )
		XtVaGetValues(base_widget, XtNbasePtr, &base, NULL);

	widget_index = 0;
	for ( victim_index = 0 ;
		  base != base_objects[victim_index] ;
		  victim_index++ )
		if ( base_objects[victim_index]->b_class == csg_obj )
			widget_index++;

	if ( base->b_num_instances )
	{
		/* Highlight all the instances. */
		Base_Highlight_Instances(base, TRUE);

		if ( destroy )
		{
			Popup_Error("Object has instances!", main_window.shell, "Error");
			Base_Highlight_Instances(base, FALSE);
		}
		else
		{
			if ( ! csg_instance_shell )
				Create_CSG_Instance_Shell();

			/* Determine if any instances may be modified. */
			XtSetSensitive(modify_instances_button,
						   Base_Determine_Modifiable_Instances(base));

			SFpositionWidget(csg_instance_shell);
			XtPopup(csg_instance_shell, XtGrabExclusive);

			modify_base = base;
		}

		return;
	}

	/* See if any of the csg trees originated from this one. */
	CSG_Delete_Original_Base(base);

	/* Destroy it's widget, and rearrange all the others. */
	CSG_Select_Destroy_Widget(widget_index);

	/* Destroy it's wireframes. */
	/* Why wasn't I destroying the b_wireframe? */
	for ( i = 0 ; i < base->b_max_density + 1 ; i++ )
	{
		Wireframe_Destroy(base->b_major_wires[i]);
		Wireframe_Destroy(base->b_wireframes[i]);
	}
	free(base->b_major_wires);
	free(base->b_wireframes);

	if ( destroy )
		CSG_Delete_Tree(base->b_csgptr, TRUE);
	else
		CSG_Insert_Existing_Tree(base->b_csgptr, FALSE, NULL);
	free(base->b_label);
	free(base);

	for ( i = victim_index + 1 ; i < num_base_objects ; i++ )
		base_objects[i-1] = base_objects[i];

	num_base_objects--;
	num_csg_base_objects--;

	if ( ! num_csg_base_objects )
		Set_CSG_Related_Sensitivity(FALSE);

	changed_scene = TRUE;
}




void
CSG_Copy_Base_Object(Widget base_widget, BaseObjectPtr base)
{
	CSGNodePtr	new_tree;

	if ( base_widget )
		XtVaGetValues(base_widget, XtNbasePtr, &base, NULL);

	new_tree = CSG_Copy_Tree(base->b_csgptr, NULL);

	Add_Displayed_Tree(new_tree, FALSE, NULL);

	changed_scene = TRUE;

	XawTreeForceLayout(csg_tree_widget);
}


/*	BaseObjectPtr
**	Add_CSG_Base_Object(CSGNodePtr tree, char *label, CSGWireframe *wireframe)
**	Adds a CSG object to the base objects available.
**	In doing so it:
**	- generates a new wireframe.
**	- adds it to the base array.
**	- creates a new picture in the new_csg_object shell.
*/
BaseObjectPtr
Add_CSG_Base_Object(CSGNodePtr tree, char *label, int density,
					WireframePtr *wireframe, WireframePtr *simple_wireframe,
					int ref_num, Boolean full)
{
	BaseObjectPtr	new_obj = New(BaseObject, 1);
	int	i;

	new_obj->b_label = Strdup(label);
	new_obj->b_class = csg_obj;

	new_obj->b_csgptr = tree;

	new_obj->b_max_density = density;

	new_obj->b_major_wires = wireframe;
	if ( ! simple_wireframe )
	{
		new_obj->b_wireframes = New(WireframePtr, density + 1);
		for ( i = 0 ; i < density + 1 ; i++ )
			new_obj->b_wireframes[i] =
				Wireframe_Simplify(new_obj->b_major_wires[i], FALSE);
	}
	else
		new_obj->b_wireframes = simple_wireframe;

	for ( i = 0 ; i < density + 1 ; i++ )
		if ( ! new_obj->b_wireframes[i]->edges )
			Edge_Table_Build(new_obj->b_wireframes[i]);
	new_obj->b_ref_num = ref_num;

	Calculate_CSG_Bounds(new_obj->b_csgptr);

	new_obj->b_instances = New(ObjectInstancePtr, 1);
	new_obj->b_num_instances = 0;
	new_obj->b_num_slots = 1;
	new_obj->b_use_full = full;

	new_obj->b_create_func = Create_Generic_Object;
	new_obj->b_destroy_func = Destroy_Generic_Object;

	if ( num_base_objects == base_object_slots )
	{
		base_object_slots += 5;
		base_objects = More(base_objects, BaseObjectPtr, base_object_slots);
	}
	
	base_objects[num_base_objects++] = new_obj;
	num_csg_base_objects++;

	if ( num_csg_base_objects == 1 )
		Set_CSG_Related_Sensitivity(TRUE);

	changed_scene = TRUE;

	return new_obj;
}


static void
CSG_Copy_Then_Modify_Callback(Widget w, XtPointer cl, XtPointer ca)
{
	CSGNodePtr		new_tree = CSG_Copy_Tree(modify_base->b_csgptr, NULL);

	XtPopdown(csg_instance_shell);

	Base_Highlight_Instances(modify_base, FALSE);

	Add_Displayed_Tree(new_tree, FALSE, NULL);

	changed_scene = TRUE;

	XawTreeForceLayout(csg_tree_widget);
}


static void
CSG_Modify_Instances_Callback(Widget w, XtPointer cl, XtPointer ca)
{
	CSGNodePtr		new_tree = CSG_Copy_Tree(modify_base->b_csgptr, NULL);

	XtPopdown(csg_instance_shell);

	Base_Highlight_Instances(modify_base, FALSE);

	Add_Displayed_Tree(new_tree, FALSE, modify_base);

	changed_scene = TRUE;

	XawTreeForceLayout(csg_tree_widget);
}

static void
CSG_Modify_Cancel(Widget w, XtPointer cl, XtPointer ca)
{
	Base_Highlight_Instances(modify_base, FALSE);
	XtPopdown(csg_instance_shell);
}


static void
Create_CSG_Instance_Shell()
{
	Widget	dialog;
	Arg		args[5];
	int		n;

	n = 0;
	XtSetArg(args[n], XtNallowShellResize, TRUE);	n++;
	XtSetArg(args[n], XtNtitle, "Modify");			n++;
	csg_instance_shell = XtCreatePopupShell("modifyShell",
						transientShellWidgetClass, csg_window.shell, args, n);

	n = 0;
	XtSetArg(args[n], XtNlabel, "Instances exist.\nCopy then modify?");	n++;
	dialog = XtCreateManagedWidget("modifyDialog",
						dialogWidgetClass, csg_instance_shell, args, n);

	XawDialogAddButton(dialog, "Copy", CSG_Copy_Then_Modify_Callback, NULL);
	XawDialogAddButton(dialog, "Modify Instances",
					   CSG_Modify_Instances_Callback, NULL);
	XawDialogAddButton(dialog, "Cancel", CSG_Modify_Cancel, NULL);

	modify_instances_button = XtNameToWidget(dialog, "Modify Instances");

	XtVaSetValues(XtNameToWidget(dialog, "label"), XtNborderWidth, 0, NULL);

	XtRealizeWidget(csg_instance_shell);
}


BaseObjectPtr
Add_Wireframe_Base_Object(char *label, WireframePtr wireframe,
						  WireframePtr triangle_wire, int ref_num)
{
	BaseObjectPtr	new_obj = New(BaseObject, 1);

	new_obj->b_label = Strdup(label);
	new_obj->b_class = wireframe_obj;

	new_obj->b_csgptr = NULL;
	new_obj->b_max_density = 0;
	new_obj->b_major_wires = New(WireframePtr, 1);
	new_obj->b_wireframes = New(WireframePtr, 1);

	new_obj->b_major_wires[0] = wireframe;
	if ( triangle_wire )
		new_obj->b_wireframes[0] = triangle_wire;
	else
		new_obj->b_wireframes[0] = Wireframe_Triangulate(wireframe);
	if ( ! new_obj->b_wireframes[0]->edges )
		Edge_Table_Build(new_obj->b_wireframes[0]);
	new_obj->b_max_density = 0;

	new_obj->b_ref_num = ref_num;

	new_obj->b_instances = New(ObjectInstancePtr, 1);
	new_obj->b_num_instances = 0;
	new_obj->b_num_slots = 1;
	new_obj->b_use_full = FALSE;

	new_obj->b_create_func = Create_Generic_Object;
	new_obj->b_destroy_func = Destroy_Generic_Object;

	if ( num_base_objects == base_object_slots )
	{
		base_object_slots += 5;
		base_objects = More(base_objects, BaseObjectPtr, base_object_slots);
	}
	
	base_objects[num_base_objects++] = new_obj;
	num_wire_base_objects++;

	if ( num_wire_base_objects == 1 )
		Set_Wireframe_Related_Sensitivity(TRUE);

	changed_scene = TRUE;

	return new_obj;
}


void
Wireframe_Destroy_Base_Object(Widget base_widget, BaseObjectPtr base)
{
	int		victim_index, widget_index;
	int		i;

	if ( base_widget )
		XtVaGetValues(base_widget, XtNbasePtr, &base, NULL);

	widget_index = 0;
	for ( victim_index = 0 ;
		  base != base_objects[victim_index] ;
		  victim_index++ )
		if ( base_objects[victim_index]->b_class == wireframe_obj )
			widget_index++;

	if ( base->b_num_instances )
	{
		Popup_Error("Object has instances!", main_window.shell, "Error");
		return;
	}

	/* Destroy it's widget, and rearrange all the others. */
	Wireframe_Select_Destroy_Widget(widget_index);

	/* Destroy it's wireframe. */
	Wireframe_Destroy(base->b_major_wires[0]);
	Wireframe_Destroy(base->b_wireframes[0]);
	free(base->b_major_wires);
	free(base->b_wireframes);
	free(base->b_label);
	free(base);

	for ( i = victim_index + 1 ; i < num_base_objects ; i++ )
		base_objects[i-1] = base_objects[i];

	num_base_objects--;
	num_wire_base_objects--;

	if ( ! num_wire_base_objects )
		Set_Wireframe_Related_Sensitivity(FALSE);

	changed_scene = TRUE;
}


void
Base_Change(ObjectInstancePtr obj, BaseObjectPtr new_base, Boolean redraw,
			WindowInfoPtr window)
{
	if ( ! obj->o_parent )
		return;

	if ( Obj_Is_Torus(obj) )
		free(obj->o_hook);

	if ( Obj_Is_Control(obj) )
	{
		free(control_part(obj)->control_verts);
		free(obj->o_hook);
	}

	Remove_Instance_From_Base(obj);
	Add_Instance_To_Base(obj, new_base);

	if ( Obj_Is_Torus(obj) )
	{
		obj->o_hook = (void*)New(TorusHook, 1);
		((TorusPtr)obj->o_hook)->major_radius = 2.0;
	}

	if ( Obj_Is_Control(obj) )
	{
		obj->o_hook = (void*)New(ControlHook, 1);
		if ( Obj_Is_Triangle(obj) )
			control_part(obj)->num_control_verts = 3;
		else if ( Obj_Is_Bezier(obj) )
			control_part(obj)->num_control_verts = 16;
		control_part(obj)->control_verts =
			New(Vector, control_part(obj)->num_control_verts);
	}

	obj->o_wireframe = new_base->b_wireframes[0];
	Object_Change_Wire(obj);

	/* Remove any pending flag. */
	/* This prevents another base change from happening without the
	** user's consent. This comes about if an object's base is currently
	** being edited. If the user changes this object's base explicitly,
	** then we don't want the later implicit change base happening when
	** the edited object is completed.
	** If the base is being changed because of completion of an edited object,
	** then there is no problem.
	*/
	obj->o_flags &= ( ObjPending ^ ObjAll );

	if ( redraw )
	{
		InstanceList	temp = NULL;
		Insert_Element(&temp, obj);
		View_Update(window, temp, CalcView);
		Update_Projection_Extents(temp);
		Free_Selection_List(temp);
	}
}

void
Base_Change_List(WindowInfoPtr window, InstanceList insts,
				 BaseObjectPtr new_base)
{
	InstanceList	temp;

	for ( temp = insts ; temp ; temp = temp->next )
		Base_Change(temp->the_instance, new_base, FALSE, NULL);
	View_Update(window, insts, CalcView);
	Update_Projection_Extents(insts);
}


void
Base_Change_Callback(Widget w, XtPointer cl_data, XtPointer ca_data)
{
	current_window = (WindowInfoPtr)cl_data;
	Select_Object_Popup((WindowInfoPtr)cl_data, select_change);
}

void
Base_Change_Select_Callback(Widget w, BaseObjectPtr base)
{
	if ( w )
		XtVaGetValues(w, XtNbasePtr, &base, NULL);

	Base_Change_List(current_window, current_window->selected_instances,
					 base);
}

static Boolean
Base_Determine_Modifiable_Instances(BaseObjectPtr base)
{
	int		i;
	Boolean	result = FALSE;

	for ( i = 0 ; i < base->b_num_instances ; i++ )
	{
		if ( Find_Object_In_Instances(base->b_instances[i],
									  main_window.all_instances) )
		{
			base->b_instances[i]->o_flags |= ObjPending;
			result = TRUE;
		}
		else if ( Find_Object_In_Instances(base->b_instances[i],
										   csg_window.all_instances) )
		{
			base->b_instances[i]->o_flags |= ObjPending;
			result = TRUE;
		}
	}

	return result;
}


