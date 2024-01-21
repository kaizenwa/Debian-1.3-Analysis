/*
**    ScEd: A Constraint Based Scene Editor.
**    Copyright (C) 1994  Stephen Chenney (stephen@cs.su.oz.au)
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
**	berkeley.c: Export and preview functions for our berkeley format,
**				whatever that's defined to be at the time.
*/

#include <math.h>
#include <sced.h>
#include <attributes.h>
#include <base_objects.h>
#include <csg.h>
#include <time.h> 

#if HAVE_STRING_H
#include <string.h>
#elif HAVE_STRINGS_H
#include <strings.h>
#endif

static int	Berkeley_Export_Camera(FILE*, Camera);
static int	Berkeley_Export_Init_Lights(FILE*, ScenePtr);
static int	Berkeley_Export_Light(FILE*, ObjectInstancePtr);
static int	Berkeley_Export_Instances(FILE*, InstanceList);

/*	int
**	Export_Berkeley(FILE *outfile, ScenePtr scene)
**	Exports all the relevant info into outfile.
*/

int
Export_Berkeley(FILE *outfile, ScenePtr scene)
{
	time_t	current_time;

	time(&current_time);

	if ( Berkeley_Export_Camera(outfile, scene->camera) < 0 ||
		 Berkeley_Export_Init_Lights(outfile, scene) < 0 ||
		 Berkeley_Export_Instances(outfile, scene->instances) < 0 )
	{
		Popup_Error("Write failed!", main_window.shell, "Error");
		return 0;
	}

	return 1;
}

static int
Berkeley_Export_Camera(FILE *outfile, Camera cam)
{
	return 1;
}

static int
Berkeley_Export_Init_Lights(FILE *outfile, ScenePtr scene)
{
	return 1;
}


static int
Berkeley_Export_Light(FILE *outfile, ObjectInstancePtr light)
{
	if ( ! light ) return 1;

	return 1;
}



static int
Export_Polyhedral_Object(FILE *outfile, WireframePtr wire, char *name,
						 AttributePtr def_attribs)
{
	int		i, j;

	fprintf(outfile, "obj %d %d %d\n",
			wire->num_real_verts,
			wire->vertex_normals ? wire->num_real_verts : 0,
			wire->num_faces);

	/* Vertices */
	for(i=0; i<wire->num_real_verts; i++)
		VPrint(outfile,wire->vertices[i]);

	if ( wire->vertex_normals )
		for ( i = 0 ; i < wire->num_real_verts ; i++ )
			VPrint(outfile, wire->vertex_normals[i]);

	/* Faces */
	for ( i=0 ; i<wire->num_faces ; i++ )
	{
		if ( wire->faces[i].num_vertices < 3 )
			continue;

		fprintf(outfile, "%d ", wire->faces[i].num_vertices);

		for( j = 0 ; j < wire->faces[i].num_vertices ; j++ )
			fprintf(outfile, "%d ", wire->faces[i].vertices[j]);

		fprintf(outfile, "%g %g %g ", wire->faces[i].normal.x,
				wire->faces[i].normal.y, wire->faces[i].normal.z);

		if(wire->faces[i].face_attribs && wire->faces[i].face_attribs->defined)
		{
			fprintf(outfile, "%1.5g %1.5g %1.5g %1.5g %1.5g %1.5g\n",
					wire->faces[i].face_attribs->colour.red,
					wire->faces[i].face_attribs->colour.green,
					wire->faces[i].face_attribs->colour.blue,
					wire->faces[i].face_attribs->diff_coef,
					wire->faces[i].face_attribs->spec_coef,
					wire->faces[i].face_attribs->spec_power);
		}
		else if ( def_attribs && def_attribs->defined )
		{
			fprintf(outfile, "%1.5g %1.5g %1.5g %1.5g %1.5g %1.5g\n",
					def_attribs->colour.red,
					def_attribs->colour.green,
					def_attribs->colour.blue,
					def_attribs->diff_coef,
					def_attribs->spec_coef,
					def_attribs->spec_power);
		}
		else
		{
			fprintf(outfile, "%1.5g %1.5g %1.5g %1.5g %1.5g %1.5g\n",
					sced_preferences.default_attributes.colour.red,
					sced_preferences.default_attributes.colour.green,
					sced_preferences.default_attributes.colour.blue,
					sced_preferences.default_attributes.diff_coef,
					sced_preferences.default_attributes.spec_coef,
					sced_preferences.default_attributes.spec_power);
		}
	}

	return 1;
}

static int
Export_Object(FILE *outfile, ObjectInstancePtr obj)
{
	WireframePtr	wireframe = Object_To_Wireframe(obj, TRUE, FALSE);

	Export_Polyhedral_Object(outfile, wireframe, obj->o_label, obj->o_attribs);

	Wireframe_Destroy(wireframe);

	return 1;
}


static int
Export_Plane(FILE *outfile, ObjectInstancePtr plane)
{
	WireframePtr	wireframe = New(Wireframe, 1);
	int				i;
	Vector			temp_v;

	/* Build a dummy wireframe. */
	wireframe->num_vertices = 4;
	wireframe->num_real_verts = 4;
	wireframe->vertices = New(Vector, 4);
	VNew(10, 10, 0, wireframe->vertices[0]);
	VNew(-10, 10, 0, wireframe->vertices[1]);
	VNew(-10, -10, 0, wireframe->vertices[2]);
	VNew(10, -10, 0, wireframe->vertices[3]);
	wireframe->num_faces = 1;
	wireframe->faces = New(Face, 1);
	wireframe->faces[0].num_vertices = 4;
	wireframe->faces[0].vertices = New(int, 4);
	for ( i = 0 ; i < 4 ; i++ )
		wireframe->faces[0].vertices[i] = i;
	wireframe->faces[0].face_attribs = NULL;
	wireframe->vertex_normals = NULL;
	wireframe->num_attribs = 0;
	wireframe->attribs = NULL;

	for ( i = 0 ; i < 4 ; i++ )
	{
		MVMul(plane->o_transform.matrix, wireframe->vertices[i], temp_v);
		VAdd(temp_v, plane->o_transform.displacement, wireframe->vertices[i]);
	}

	Export_Polyhedral_Object(outfile, wireframe, plane->o_label,
							 plane->o_attribs);

	Wireframe_Destroy(wireframe);

	return 1;
}




static int
Berkeley_Export_Instances(FILE *outfile, InstanceList insts)
{
	InstanceList		inst_elmt;
	ObjectInstancePtr	inst;

	for ( inst_elmt = insts ; inst_elmt != NULL ; inst_elmt = inst_elmt->next )
	{
		inst = inst_elmt->the_instance;

		if ( Obj_Is_Light(inst) && ! Obj_Is_Construction(inst) )
			Berkeley_Export_Light(outfile, inst);
	}

	/* actually output the objects */
	for ( inst_elmt = insts ; inst_elmt != NULL ; inst_elmt = inst_elmt->next )
	{
		inst = inst_elmt->the_instance;

		if ( Obj_Is_Construction(inst) )
			continue;

		switch ( inst->o_parent->b_class )
		{
			case cube_obj:
			case triangle_obj:
			case sphere_obj:
			case cylinder_obj:
			case cone_obj:
			case torus_obj:
			case bezier_obj:
			case wireframe_obj:
				Export_Object(outfile, inst);
				break;

			case csg_obj:  
 				if ( ! inst->o_wireframe )
 				{
 					/* Must be a CSG preview. */
 					WireframePtr	main_wireframe;
 
 					main_wireframe =
					 CSG_Generate_Wireframe(inst->o_parent->b_csgptr, 0, FALSE);
 					inst->o_wireframe =
						Wireframe_Simplify(main_wireframe, FALSE);
					Wireframe_Destroy(main_wireframe);
 					Export_Object(outfile, inst);
 					Wireframe_Destroy(inst->o_wireframe);
 					inst->o_wireframe = NULL;
				}
				else
					Export_Object(outfile, inst);
				break;

			case plane_obj:
				Export_Plane(outfile, inst);
				break;

			case light_obj:
			case spotlight_obj:
			case arealight_obj:
			case dirlight_obj:
				break;
		}
	}

	return 1;
}
