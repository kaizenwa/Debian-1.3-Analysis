#include "elk_private.h"
#include <View.h>

int	T_Object3d;
int	T_Viewport;
int	T_CSGNode;

Object	Sym_Rayshade;
Object	Sym_Radiance;
Object	Sym_POVray;
Object	Sym_Renderman;
Object	Sym_VRML;
Object	Sym_Genray;
Object	Sym_Genscan;

extern void	Remove_Temporaries();

static InstanceList
elk_list_to_instances(Object listobj)
{
	Object			list_elmt_obj;
	InstanceList	result = NULL;

	Check_List(listobj);

	if ( Nullp(listobj) )
		return NULL;

	for ( list_elmt_obj = listobj ;
		  ! Nullp(list_elmt_obj) ;
		  list_elmt_obj = Cdr(list_elmt_obj) )
	{
		Check_Type(Car(list_elmt_obj), T_Object3d);
		Insert_Element(&result, ELKOBJECT3(Car(list_elmt_obj))->object3);
	}

	return result;
}

/***********************************************************************
 *
 * Description:  elk_set_window_csg() is the callback for the scheme
 *               function "set-window-csg". This function causes
 *               subsequent actions to happen in the csg window,
 *               regardless of the window in which the event happened.
 *               Behaviour is cancelled when a call to set-window-none
 *               or set-window-scene happens.
 *
 * Scheme example: (set-window-csg)
 *
 * Return value: Void
 *
 **********************************************************************/
static Object
elk_set_window_csg()
{
	if ( ! csg_window.shell )
		Create_CSG_Display();

	elk_use_csg = 1;
	elk_use_main = 0;
	elk_is_csg_window = TRUE;
	elk_window = &csg_window;

	return Void;
}

/***********************************************************************
 *
 * Description:  elk_set_window_scene() is the callback for the scheme
 *               function "set-window-scene". This function causes
 *               subsequent actions to happen in the scene window,
 *               regardless of the window in which the event happened.
 *               Behaviour is cancelled when a call to set-window-none
 *               or set-window-csg happens.
 *
 * Scheme example: (set-window-scene)
 *
 * Return value: Void
 *
 **********************************************************************/
static Object
elk_set_window_scene()
{
	elk_use_csg = 0;
	elk_use_main = 1;
	elk_is_csg_window = FALSE;
	elk_window = &main_window;

	return Void;
}

/***********************************************************************
 *
 * Description:  elk_set_window_none() is the callback for the scheme
 *               function "set-window-none". This function cancels any
 *               "set-window-csg" or "set-window-scene" calls.
 *
 * Scheme example: (set-window-none)
 *
 * Return value: Void
 *
 **********************************************************************/
static Object
elk_set_window_none()
{
	elk_use_csg = 0;
	elk_use_main = 0;

	return Void;
}


/***********************************************************************
 *
 * Description: elk_csg_open() is the C callback for the scheme function
 *              "csg-open", which opens the CSG window with a list of
 *				instances.
 *
 * Scheme Example: (csg-open '())
 *
 **********************************************************************/
static Object
elk_csg_open(Object listobj)
{
	InstanceList	instances = elk_list_to_instances(listobj);

	CSG_Window_Popup(NULL, &instances, NULL);

	return Void;
}


/***********************************************************************
 *
 * Description: elk_get_selectlist() is the C callback for the scheme
 * 		function "get-selectlist". This function is used to
 *		get a list of objects currently selected.
 *
 * Scheme example: (get-selectlist)
 *
 * Return value: Returns the a scheme list object containing all
 *		 the objects currently selected.
 *
 ***********************************************************************/
static Object
elk_get_selectlist()
{
	Object obj;
	WindowInfo *window;
	InstanceList elmt;
	Object list;

	window = (elk_is_csg_window) ? &csg_window : &main_window;
	list = Null;
	for (elmt = window->selected_instances ; elmt; elmt = elmt->next)
	{
		obj = Alloc_Object(sizeof(Elkobject3), T_Object3d, 0);
		ELKOBJECT3(obj)->object3 = elmt->the_instance;
		list = Cons(obj, list);
	}
	return list;
}


/***********************************************************************
 *
 * Description: elk_clear() is the c callback for the scheme function
 *              "clear", which clears the screens.
 *
 **********************************************************************/
static Object
elk_clear()
{
	Destroy_World(FALSE);

	return Void;
}

/***********************************************************************
 *
 * Description: elk_reset() is the C callback for the scheme function
 *              "reset", which clears the screen and any base objects.
 *
 **********************************************************************/
static Object
elk_reset()
{
	Destroy_World(TRUE);

	return Void;
}


/**********************************************************************
 *
 * Description: elk_preview is the C callback for the scheme function
 *              preview, which causes a scene to be previewed.
 *              rayobj must be one of 'rayshade, 'genray, 'povray,
 *              'genscan or 'radiance.
 *              If listobj is empty, all instances are previewed.
 *              If width or height is 0, the current screen size is used.
 *
 * Parameters: rayobj: A symbol for the target renderer.
 *             listobj: A list of object to render, or the empty list.
 *             widthobj, heightobj: The width and height of the output.
 *
 *********************************************************************/
static Object
elk_preview(Object rayobj, Object instobj, Object widthobj, Object heightobj,
			Object cobj)
{
	Renderer		target;
	InstanceList	instances;
	int				width, height;
	Dimension		temp_dim;

	/* Check types. */
	Check_Type(rayobj, T_Symbol);
	Check_List(instobj);
	Check_Integer(widthobj);
	Check_Integer(heightobj);
	Check_Type(cobj, T_Symbol);

	/* Determine target. */
	if ( EQ(rayobj, Sym_Rayshade) )
		target = Rayshade;
	else if ( EQ(rayobj, Sym_Radiance) )
		target = Radiance;
	else if ( EQ(rayobj, Sym_Renderman) )
		target = Renderman;
	else if ( EQ(rayobj, Sym_VRML) )
		target = VRML;
	else if ( EQ(rayobj, Sym_Genray) )
		target = Genray;
	else if ( EQ(rayobj, Sym_POVray) )
		target = POVray;
	else if ( EQ(rayobj, Sym_Genscan) )
		target = Genray;
	else
		Primitive_Error("Invalid Renderer: ~s", rayobj);

	/* Get instances. */
	if ( Nullp(instobj) )
		instances = Merge_Selection_Lists(elk_window->all_instances, NULL);
	else
		instances = elk_list_to_instances(instobj);

	/* Get sizes. */
	width = Get_Integer(widthobj);
	if ( ! width )
	{
		XtVaGetValues(elk_window->view_widget, XtNwidth, &temp_dim, NULL);
		width = (int)temp_dim;
	}
	height = Get_Integer(heightobj);
	if ( ! height )
	{
		XtVaGetValues(elk_window->view_widget, XtNheight, &temp_dim, NULL);
		height = (int)temp_dim;
	}

	Perform_Preview(elk_window, target, instances, width, height, Truep(cobj));

	return Void;
}


static Object
elk_target(Object rayobj)
{
	/* Check types. */
	Check_Type(rayobj, T_Symbol);

	/* Determine target. */
	if ( EQ(rayobj, Sym_Rayshade) )
		target_renderer = Rayshade;
	else if ( EQ(rayobj, Sym_Radiance) )
		target_renderer = Radiance;
	else if ( EQ(rayobj, Sym_Renderman) )
		target_renderer = Renderman;
	else if ( EQ(rayobj, Sym_VRML) )
		target_renderer = VRML;
	else if ( EQ(rayobj, Sym_Genray) )
		target_renderer = Genray;
	else if ( EQ(rayobj, Sym_POVray) )
		target_renderer = POVray;
	else if ( EQ(rayobj, Sym_Genscan) )
		target_renderer = Genray;
	else
		Primitive_Error("Invalid Renderer: ~s", rayobj);

	return Void;
}

static Object
elk_export(Object strobj)
{
	SceneStruct	export_scene;
	FILE		*outfile;

	if ( target_renderer == NoTarget )
		return Void;

	Check_Type(strobj, T_String);

	outfile = fopen(STRING(strobj)->data, "w");

	export_scene.camera = camera;
	export_scene.light = NULL;
	export_scene.ambient = ambient_light;
	export_scene.instances = main_window.all_instances;
	if ( outfile )
		Export_File(outfile, STRING(strobj)->data, &export_scene, FALSE);

	return Void;
}

static Object
elk_quit()
{
	Sced_Exit(0);

	return Void;
}


/***********************************************************************
 *
 * Description: init_callbacks() is the main function for initializing
 *		the extended types in the scheme interpreter.
 *
 ***********************************************************************/
int
init_callbacks()
{
	T_Object3d = Define_Type(0, "object3d", NOFUNC,
			   sizeof(Elkobject3),
			   elk_object3d_equiv, elk_object3d_equal,
			   elk_object3d_print, NOFUNC);
	T_Viewport = Define_Type(0, "viewport", NOFUNC,
			   sizeof(Elkviewport),
			   elk_viewport_equiv, elk_viewport_equal,
			   elk_viewport_print, NOFUNC);
	T_CSGNode = Define_Type(0, "csgnode", NOFUNC,
			   sizeof(Elkcsgnode),
			   elk_csg_equiv, elk_csg_equal,
			   elk_csg_print, NOFUNC);

	/*
	 * Symbols for csg ops.
	 */
	Define_Symbol(&Sym_Union, "union");
	Define_Symbol(&Sym_Intersection, "intersection");
	Define_Symbol(&Sym_Difference, "difference");

	/*
	 * Renderer symbols
	 */
	Define_Symbol(&Sym_POVray, "povray");
	Define_Symbol(&Sym_Rayshade, "rayshade");
	Define_Symbol(&Sym_Radiance, "radiance");
	Define_Symbol(&Sym_Renderman, "renderman");
	Define_Symbol(&Sym_VRML, "vrml");
	Define_Symbol(&Sym_Genray, "genray");
	Define_Symbol(&Sym_Genscan, "genscan");

	/*
	 * Callbacks for setting the current window.
	 */
	Define_Primitive((Object(*)(ELLIPSIS))elk_set_window_csg,
			 "set-window-csg", 0, 0, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_set_window_scene,
			 "set-window-scene", 0, 0, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_set_window_none,
			 "set-window-none", 0, 0, EVAL);

	/*
	 * define callbacks for manipulating objects
	 */
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_create,
			 "object3d-create", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_destroy,
			 "object3d-destroy", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_position,
			 "object3d-position", 4, 4, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_displace,
			 "object3d-displace", 4, 4, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_scale,
			 "object3d-scale", 4, 4, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_rotate,
			 "object3d-rotate", 4, 4, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_wireframe_query,
			 "object3d-wireframe-query", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_wireframe_level,
			 "object3d-wireframe-level", 2, 2, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_attribs_define,
			 "object3d-attribs-define", 2, 2, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_color,
			 "object3d-color", 4, 4, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_diffuse,
			 "object3d-diffuse", 2, 2, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_specular,
			 "object3d-specular", 3, 3, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_reflect,
			 "object3d-reflect", 2, 2, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_object3d_transparency,
			 "object3d-transparency", 3, 3, EVAL);

	/*
	 * Selection list interface.
	 */
	Define_Primitive((Object(*)(ELLIPSIS))elk_get_selectlist,
			 "get-selectlist", 0, 0, EVAL);

	/*
	 * defined callbacks for manipulating the viewport
	 */
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_create,
			 "viewport-create", 0, 0, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_destroy,
			 "viewport-destroy", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_lookat,
			 "viewport-lookat", 4, 4, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_position,
			 "viewport-position", 4, 4, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_upvector,
			 "viewport-upvector", 4, 4, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_distance,
			 "viewport-distance", 2, 2, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_eye,
			 "viewport-eye", 2, 2, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_setup,
			 "viewport-setup", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_zoom,
			 "viewport-zoom", 2, 2, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_viewport_to_camera,
			 "viewport-to-camera", 1, 1, EVAL);

	/* CSG callbacks. */
	Define_Primitive((Object(*)(ELLIPSIS))elk_csg_open,
			 "csg-open", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_csg_node,
			 "csg-node", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_csg_hide,
			 "csg-hide", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_csg_display,
			 "csg-display", 1, 1, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_csg_attach,
			 "csg-attach", 3, 3, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_csg_complete,
			 "csg-complete", 2, 2, EVAL);

	/* Destroying the world. */
	Define_Primitive((Object(*)(ELLIPSIS))elk_clear, "clear", 0, 0, EVAL);
	Define_Primitive((Object(*)(ELLIPSIS))elk_reset, "reset", 0, 0, EVAL);

	/* Preview. */
	Define_Primitive((Object(*)(ELLIPSIS))elk_preview, "preview", 4, 4, EVAL);

	/* Target. */
	Define_Primitive((Object(*)(ELLIPSIS))elk_target, "target", 1, 1, EVAL);

	/* Export. */
	Define_Primitive((Object(*)(ELLIPSIS))elk_export, "export", 1, 1, EVAL);

	/* Quit. */
	Define_Primitive((Object(*)(ELLIPSIS))elk_quit, "quit-sced", 0, 0, EVAL);

	return 0;
}
