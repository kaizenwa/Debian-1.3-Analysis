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
**	load_simple.c: Functions for loading simple format files. These files
**					are designed to be written by humans or other programs.
*/

#include <math.h>
#include <sced.h>
#include <attributes.h>
#include <base_objects.h>
#include <csg.h>
#include <instance_list.h>
#include <layers.h>
#include <load.h>
#include <View.h>
#include <Vector4.h>

#define Load_Float(f) \
	{ \
		if ((token = yylex()) == INT_TOKEN) \
			f = (double)lex_int; \
		else if (token == FLOAT_TOKEN) \
			f = lex_float; \
		else \
		{ \
			fprintf(stderr, "Malformed input file line %d: Float expected\n", \
					line_num); \
			return EOF_TOKEN; \
		} \
	}

#define Load_Vector(v) \
	{ Load_Float((v).x); Load_Float((v).y); Load_Float((v).z); }

#define Load_Vector4(v) \
	{ Load_Float((v).x); Load_Float((v).y); Load_Float((v).z); \
	  Load_Float((v).w) }
#define Load_Matrix4(m) \
	{ Load_Vector4((m).x); Load_Vector4((m).y); Load_Vector4((m).z); \
	  Load_Vector4((m).w) }

static int	Load_Ambient(Boolean);
static int	Load_Simple_Instance(ObjectInstancePtr*, Boolean);
static int	Load_Simple_Object(ObjectInstancePtr);
static int	Load_Simple_CSG();
static int	Load_Simple_CSG_Tree(int, CSGNodePtr, CSGNodePtr*);
static int	Load_Simple_Wireframe();


void
Load_Simple_File(FILE *file, int merge, int token)
{
	Viewport	camera_viewport;
	ObjectInstancePtr	dummy;

	if ( token == VERS_TOKEN )
	{
		if ((token = yylex()) == INT_TOKEN)
			version = (double)lex_int;
		else if (token == FLOAT_TOKEN)
			version = lex_float;
		else
		{
			fprintf(stderr, "Malformed input file line %d\n", line_num);
			return;
		}
		token = yylex();
	}

	while (token != EOF_TOKEN)
	{
		switch (token)
		{
			case VIEWPORT_TOKEN:
				if ( merge )
					token = Load_View(NULL, NULL);
				else
				{
					token = Load_View(&(main_window.viewport), NULL);
					if ( main_window.view_widget &&
						 main_window.viewport.scr_width )
						XtVaSetValues(main_window.view_widget,
							XtNwidth, main_window.viewport.scr_width,
							XtNdesiredWidth, main_window.viewport.scr_width,
							XtNheight, main_window.viewport.scr_height,
							XtNdesiredHeight, main_window.viewport.scr_height,
							NULL);
					main_window.magnify = main_window.viewport.magnify;
				}
				break;

			case CAMERA_TOKEN:
				token = Load_View(&camera_viewport, NULL);
				if ( ! merge )
					Viewport_To_Camera(&camera_viewport,
									   main_window.view_widget,
									   &camera, FALSE);
				break;

			case TARGET_TOKEN:
				if ( ( token = yylex() ) == POVRAY_TOKEN )
					target_renderer = POVray;
				else if ( token == RAYSHADE_TOKEN )
					target_renderer = Rayshade;
				else if ( token == RADIANCE_TOKEN )
					target_renderer = Radiance;
				else if ( token == RENDERMAN_TOKEN )
					target_renderer = Renderman;
				else if ( token == LOAD_VRML_TOKEN )
					target_renderer = VRML;
				else if ( token == GENRAY_TOKEN )
					target_renderer = Genray;
				else if ( token == GENSCAN_TOKEN )
					target_renderer = Genscan;
				else
					fprintf(stderr, "Invalid target line %d\n", line_num);
				token = yylex();
				break;

			case DECLARE_TOKEN:
				if ( target_renderer == NoTarget )
					fprintf(stderr, "Input file error: A Target must be active"
							" before a Declare statement\n");
				Load_Declaration(target_renderer, merge);
				token = yylex();
				break;

			case OBJECT_TOKEN:
				token = Load_Simple_Instance(&dummy, FALSE);
				break;

			case CSG_TOKEN:
				token = Load_Simple_CSG();
				break;

			case WIREFRAME_TOKEN:
				token = Load_Simple_Wireframe();
				break;

			case AMBIENT_TOKEN:
				token = Load_Ambient(!merge);
				break;

			default:
				if ( merge )
					fprintf(stderr, "Error token %d in file %s line %d\n",
							token, merge_filename, line_num);
				else
					fprintf(stderr, "Error token %d in file %s line %d\n",
							token, io_file_name, line_num);
				token = yylex();
		}
	}

	View_Update(&main_window, main_window.all_instances, CalcView);
	Update_Projection_Extents(main_window.all_instances);
}


static int
Load_Colour(ColorVectorPtr col)
{
	int		token;
	double	r, g, b;

	Load_Float(r);
	Load_Float(g);
	Load_Float(b);

	col->red = r;
	col->green = g;
	col->blue = b;

	return COLOUR_TOKEN;
}

static int
Load_Ambient(Boolean set)
{
	ColorVector	colour;

	if ( Load_Colour(&colour) == EOF_TOKEN )
		return EOF_TOKEN;

	if ( set )
		ambient_light = colour;

	return yylex();
}

static int
Load_Simple_Instance(ObjectInstancePtr *obj, Boolean csg)
{
	char			*obj_name;
	BaseObjectPtr	base;
	int				token;
	int				i;

	/* Load the name. */
	if ( ( token = yylex() ) != STRING_TOKEN )
	{
		fprintf(stderr, "Malformed input file line %d: Expected object name\n",
				line_num);
		return EOF_TOKEN;
	}
	obj_name = lex_string;

	/* Load the base type. */
	if ( ( token = yylex() ) != STRING_TOKEN )
	{
		fprintf(stderr, "Malformed input file line %d: Expected object type\n",
				line_num);
		return EOF_TOKEN;
	}
	base = Get_Base_Object_From_Label(lex_string);
	free(lex_string);
	if ( ! base )
	{
		fprintf(stderr, "Unable to find base \"%s\", line %d\n",
				lex_string, line_num);
		return EOF_TOKEN;
	}

	/* Create the object. */
	(*obj) = Create_Instance(base, obj_name);
	free((*obj)->o_label);
	(*obj)->o_label = obj_name;
	if ( ! csg )
	{
		InstanceList	new_elmt;

		new_elmt = Append_Element(&(main_window.all_instances), *obj);
		if ( Layer_Is_Visible((*obj)->o_layer) )
			(*obj)->o_flags |= ObjVisible;
	}

	if ( Obj_Is_Torus((*obj)) )
	{
		Load_Float(((TorusPtr)(*obj)->o_hook)->major_radius);
		VNew(((TorusPtr)(*obj)->o_hook)->major_radius, 0, 0,
			 (*obj)->o_features[radius_feature].location);
	}

	if ( Obj_Is_Control((*obj)) )
		for ( i = 0 ; i < control_part(*obj)->num_control_verts ; i++ )
			Load_Vector(control_part(*obj)->control_verts[i]);

	return Load_Simple_Object(*obj);
}


static int
Load_Simple_Object(ObjectInstancePtr obj)
{
	Transformation	obj_trans;
	Matrix4		transform;
	Matrix4		new_matrix;
	Matrix		temp_m;
	Vector		new_vector;
	Attributes	attributes = sced_preferences.default_attributes;
	Boolean		done = FALSE;
	Renderer	rend;
	int			token;

	NewIdentityMatrix4(transform);
	attributes.defined = FALSE;

	while ( ! done )
	{
		switch ( token = yylex() )
		{
			case MATRIX_TOKEN:
				Load_Matrix4(new_matrix);
				transform = M4M4Mul(&new_matrix, &transform);
				break;

			case SCALE_TOKEN:
				Load_Vector(new_vector);
				NewIdentityMatrix4(new_matrix);
				new_matrix.x.x = new_vector.x;
				new_matrix.y.y = new_vector.y;
				new_matrix.z.z = new_vector.z;
				transform = M4M4Mul(&new_matrix, &transform);
				break;

			case ROTATE_TOKEN:
				Load_Vector(new_vector);
				Vector_To_Rotation_Matrix(&new_vector, &temp_m);
				NewIdentityMatrix4(new_matrix);
				MToM4(temp_m, new_matrix);
				transform = M4M4Mul(&new_matrix, &transform);
				break;

			case POSITION_TOKEN:
				Load_Vector(new_vector);
				VAdd(new_vector, transform.w, transform.w);
				break;

			case DENSE_TOKEN:
				if ( ( token = yylex() ) != INT_TOKEN )
				{
					fprintf(stderr,
							"Malformed input file line %d: Expected integer\n",
							line_num);
					return EOF_TOKEN;
				}
				Object_Change_Wire_Level(obj, (int)lex_int);
				break;

			case SPECULAR_TOKEN:
				if ( Obj_Is_Light(obj) )
					break;
				attributes.defined = TRUE;
				Load_Float(attributes.spec_coef);
				Load_Float(attributes.spec_power);
				break;

			case DIFFUSE_TOKEN:
				if ( Obj_Is_Light(obj) )
					break;
				attributes.defined = TRUE;
				Load_Float(attributes.diff_coef);
				break;

			case REFLECT_TOKEN:
				if ( Obj_Is_Light(obj) )
					break;
				attributes.defined = TRUE;
				Load_Float(attributes.reflect_coef);
				break;

			case REFRACT_TOKEN:
				if ( Obj_Is_Light(obj) )
					break;
				attributes.defined = TRUE;
				Load_Float(attributes.refract_index);
				break;

			case TRANSPARENCY_TOKEN:
				if ( Obj_Is_Light(obj) )
					break;
				attributes.defined = TRUE;
				Load_Float(attributes.transparency);
				break;

			case COLOUR_TOKEN:
				if ( Obj_Is_Light(obj) )
				{
					Load_Float(((LightInfoPtr)obj->o_attribs)->red);
					Load_Float(((LightInfoPtr)obj->o_attribs)->green);
					Load_Float(((LightInfoPtr)obj->o_attribs)->blue);
				}
				else
				{
					attributes.defined = TRUE;
					if ( Load_Colour(&(attributes.colour)) == EOF_TOKEN )
						return EOF_TOKEN;
				}
				break;

			case EXTEND_TOKEN:
				if ( Obj_Is_Light(obj) )
					break;
				if ( ( token = yylex() ) != STRING_TOKEN )
				{
					fprintf(stderr,
							"Malformed input file line %d: Expected string\n",
							line_num);
					return EOF_TOKEN;
				}
				attributes.defined = TRUE;
				attributes.use_extension = TRUE;
				attributes.extension[target_renderer] = lex_string;
				break;

			default: done = TRUE;
		}
	}

	M4ToM(transform, obj_trans.matrix);
	VNew(transform.w.x, transform.w.y, transform.w.z, obj_trans.displacement);
	Transform_Instance(obj, &obj_trans, TRUE);
	if ( attributes.defined )
		*(AttributePtr)(obj->o_attribs) = attributes;
	for ( rend = NoTarget ; rend < LastTarget ; rend++ )
		((AttributePtr)obj->o_attribs)->extension[rend] = NULL;
	((AttributePtr)obj->o_attribs)->extension[target_renderer] =
		attributes.extension[target_renderer];

	return token;
}


static int
Load_Simple_CSG_Tree(int token, CSGNodePtr parent, CSGNodePtr *ret_tree)
{
	*ret_tree = New(CSGNode, 1);
	(*ret_tree)->csg_parent = parent;
	(*ret_tree)->csg_widget = NULL;

	switch ( token )
	{
		case UNION_TOKEN:
			(*ret_tree)->csg_op = csg_union_op;
			return Load_Simple_CSG_Tree(
						Load_Simple_CSG_Tree(yylex(), *ret_tree,
											 &((*ret_tree)->csg_left_child)),
											 *ret_tree,
											 &((*ret_tree)->csg_right_child));

		case INTERSECTION_TOKEN:
			(*ret_tree)->csg_op = csg_intersection_op;
			return Load_Simple_CSG_Tree(
						Load_Simple_CSG_Tree(yylex(), *ret_tree,
											 &((*ret_tree)->csg_left_child)),
											 *ret_tree,
											 &((*ret_tree)->csg_right_child));

		case DIFFERENCE_TOKEN:
			(*ret_tree)->csg_op = csg_difference_op;
			return Load_Simple_CSG_Tree(
						Load_Simple_CSG_Tree(yylex(), *ret_tree,
											 &((*ret_tree)->csg_left_child)),
											 *ret_tree,
											 &((*ret_tree)->csg_right_child));

		case OBJECT_TOKEN:
			(*ret_tree)->csg_op = csg_leaf_op;
			return Load_Simple_Instance(&((*ret_tree)->csg_instance), TRUE);

		default:
			fprintf(stderr,
					"Malformed input file line %d: Expected CSG tree element\n",
					line_num);
			return EOF_TOKEN;
	}
}

static int
Load_Simple_CSG()
{
	char		*csg_name;
	CSGNodePtr	tree;
	int			token;
	WireframePtr	*wire = New(WireframePtr, 1);

	if ( ( token = yylex() ) != STRING_TOKEN )
	{
		fprintf(stderr, "Malformed input file line %d: Expected string\n",
				line_num);
		return EOF_TOKEN;
	}
	csg_name = lex_string;

	if ( ( token = Load_Simple_CSG_Tree(yylex(), NULL, &tree) ) == EOF_TOKEN )
		return token;

	wire[0] = CSG_Generate_Wireframe(tree, 0, FALSE);
	Add_CSG_Base_Object(tree, csg_name, 0, wire, NULL, 0, FALSE);

	return token;
}

static int
Load_Simple_Wireframe()
{
	char			*wire_name;
	WireframePtr	wireframe = New(Wireframe, 1);
	int				token;
	int				i, j;
	Vector			v1, v2;
	double			temp_d;

	if ( ( token = yylex() ) != STRING_TOKEN )
	{
		fprintf(stderr, "Malformed input file line %d: Expected string\n",
				line_num);
		return EOF_TOKEN;
	}
	wire_name = lex_string;

	/* Get number of vertices. */
	if ( ( token = yylex() ) != INT_TOKEN )
	{
		fprintf(stderr, "Malformed input file line %d: Expected integer\n",
				line_num);
		return EOF_TOKEN;
	}
	wireframe->num_real_verts = (int)lex_int;
	wireframe->num_vertices = (int)lex_int + 1;

	/* Get number of faces. */
	if ( ( token = yylex() ) != INT_TOKEN )
	{
		fprintf(stderr, "Malformed input file line %d: Expected integer\n",
				line_num);
		return EOF_TOKEN;
	}
	wireframe->num_faces = (int)lex_int;

	wireframe->vertices = New(Vector, wireframe->num_vertices);
	for ( i = 0 ; i < wireframe->num_real_verts ; i++ )
		Load_Vector(wireframe->vertices[i])
	VNew(0, 0, 0, wireframe->vertices[i]);

	wireframe->faces = New(Face, wireframe->num_faces);
	for ( i = 0 ; i < wireframe->num_faces ; i++ )
	{
		if ( ( token = yylex() ) != INT_TOKEN )
		{
			fprintf(stderr, "Malformed input file line %d: Expected integer\n",
					line_num);
			return EOF_TOKEN;
		}
		wireframe->faces[i].num_vertices = (int)lex_int;
		for ( j = 0 ; j < wireframe->faces[i].num_vertices ; j++ )
		{
			if ( ( token = yylex() ) != INT_TOKEN )
			{
				fprintf(stderr,
						"Malformed input file line %d: Expected integer\n",
						line_num);
				return EOF_TOKEN;
			}
			wireframe->faces[i].vertices[j] = (int)lex_int;
		}

		if ( wireframe->faces[i].num_vertices > 2 )
		{
			VSub(wireframe->vertices[wireframe->faces[i].vertices[2]],
				 wireframe->vertices[wireframe->faces[i].vertices[0]], v1);
			VSub(wireframe->vertices[wireframe->faces[i].vertices[1]],
				 wireframe->vertices[wireframe->faces[i].vertices[0]], v2);
			VCross(v1, v2, wireframe->faces[i].normal);
			VUnit(wireframe->faces[i].normal, temp_d,
				  wireframe->faces[i].normal);
		}

		wireframe->faces[i].face_attribs = NULL;
	}

	wireframe->vertex_normals = NULL;
	wireframe->attribs = NULL;
	wireframe->num_attribs = 0;
	wireframe->edges = NULL;

	Add_Wireframe_Base_Object(wire_name, wireframe, NULL, 0);

	return yylex();
}

