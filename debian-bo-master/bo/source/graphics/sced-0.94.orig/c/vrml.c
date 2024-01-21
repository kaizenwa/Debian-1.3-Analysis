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
**	vrml.c : Functions for exporting VRML files.
*/

#include <math.h>
#include <sced.h>
#include <attributes.h>
#include <base_objects.h>
#include <csg.h>
#include <hash.h>
#include <quaternion.h>
#include <time.h> 
#include <torus.h>

static void	VRML_Export_Camera(FILE*, Camera*);
static void	VRML_Export_Lights(FILE*, InstanceList);
static void	VRML_Export_Light(FILE*, ObjectInstancePtr);
static void	VRML_Export_Instances(FILE*, InstanceList, ColorVectorPtr);

static HashTable	wire_hash;

#define Attribs_Defined(a)	( (a) && ( (a)->defined ) )

int
Export_VRML(FILE *outfile, ScenePtr scene)
{
	time_t	current_time;

	time(&current_time);

	wire_hash = Hash_New_Table();

	/* Required VRML header. */
	fprintf(outfile, "#VRML V1.0 ascii\n");

	/* Comments. */
	fprintf(outfile, "# File produced by Sced version "VERSION"\n");
	if ( io_file_name )
		fprintf(outfile, "# Source file: %s\n", io_file_name);
	fprintf(outfile, "# %s\n", ctime(&current_time));

	/* The scene. */
	fprintf(outfile, "Separator {\n");

	/* Rendering hints. */
	fprintf(outfile,
		"ShapeHints {\n\tvertexOrdering CLOCKWISE\n}\n\n");

	/* Camera. */
	VRML_Export_Camera(outfile, &(scene->camera));

	/* Lights. */
	if ( scene->light )
		VRML_Export_Light(outfile, scene->light);
	VRML_Export_Lights(outfile, scene->instances);

	/* Instances. */
	VRML_Export_Instances(outfile, scene->instances, &(scene->ambient));

	Hash_Free(wire_hash);

	return fprintf(outfile, "}\n");
}


static void
VRML_Export_Camera(FILE *outfile, Camera *cam)
{
	Viewport	cam_view;
	Quaternion	rot_quat;
	Vector		rot_axis;
	double		rot_angle;
	Vector		norm_axis;
	Quaternion	norm_quat;
	Quaternion	inv_norm_quat;
	Quaternion	y_quat, new_y_quat, rot_y_quat;
	Vector		new_y;
	double		temp_d;

	Vector		temp_v;

	Camera_To_Viewport(cam, &cam_view);

	VNew(cam_view.world_to_view.matrix.z.y, -cam_view.world_to_view.matrix.z.x,
		 0, norm_axis);
	if ( VZero(norm_axis) )
	{
		if ( cam_view.world_to_view.matrix.z.z > 0.0 )
		{
			norm_quat.real_part = 0.0;
			VNew(0, 1, 0, norm_quat.vect_part);
		}
		else
		{
			norm_quat.real_part = 1;
			VNew(0, 0, 0, norm_quat.vect_part);
		}
	}
	else
	{
		VUnit(norm_axis, temp_d, norm_axis);
		norm_quat = Build_Rotate_Quaternion(norm_axis,
											-cam_view.world_to_view.matrix.z.z);
	}

	/* Need to rotate the world y vector to see where it ends up. */
	inv_norm_quat.real_part = norm_quat.real_part;
	VScalarMul(norm_quat.vect_part, -1, inv_norm_quat.vect_part);

	y_quat.real_part = 0.0;
	VNew(0, 1, 0, y_quat.vect_part);
	new_y_quat = QQMul(&norm_quat, &y_quat);
	new_y_quat = QQMul(&new_y_quat, &inv_norm_quat);
	new_y = new_y_quat.vect_part;

	/* Now need to find out how much to rotate about n to line up y. */
	VCross(new_y, cam_view.world_to_view.matrix.y, temp_v);
	if ( ! VZero(temp_v) )
		VUnit(temp_v, temp_d, temp_v);
	rot_y_quat = Build_Rotate_Quaternion(temp_v,
								VDot(new_y, cam_view.world_to_view.matrix.y));

	rot_quat = QQMul(&rot_y_quat, &norm_quat);
	Quaternion_To_Axis_Angle(&rot_quat, &rot_axis, &rot_angle);

	fprintf(outfile, "PerspectiveCamera {\n");
	fprintf(outfile, "\tposition %1.5g %1.5g %1.5g\n",
			cam->location.x, cam->location.y, cam->location.z);
	fprintf(outfile, "\torientation %1.5g %1.5g %1.5g %1.5g\n",
			rot_axis.x, rot_axis.y, rot_axis.z, rot_angle);
	fprintf(outfile, "\tfocalDistance %1.5g\n", cam->eye_dist);
	fprintf(outfile, "\theightAngle %1.5g\n", cam->vert_fov * M_PI / 180.0);
	fprintf(outfile, "}\n");
}


static void
VRML_Export_Light(FILE *outfile, ObjectInstancePtr obj)
{
	Vector	color;
	double	intensity, temp_d;
	Vector	vect1, vect2;

	intensity = max(((LightInfoPtr)obj->o_attribs)->red,
					((LightInfoPtr)obj->o_attribs)->green);
	intensity = max(intensity, ((LightInfoPtr)obj->o_attribs)->blue);
	color.x = ((LightInfoPtr)obj->o_attribs)->red;
	color.y = ((LightInfoPtr)obj->o_attribs)->green;
	color.z = ((LightInfoPtr)obj->o_attribs)->blue;
	if ( ! IsZero(intensity) )
	{
		temp_d = 1 / intensity;
		VScalarMul(color, temp_d, color);
	}

	switch ( obj->o_parent->b_class )
	{
		case arealight_obj:
			/* These aren't supported, so just export as a point. */
		case light_obj:
			fprintf(outfile, "PointLight {\n");
			fprintf(outfile, "\ton TRUE\n");
			fprintf(outfile, "\tintensity %f\n", intensity);
			fprintf(outfile, "\tcolor %g %g %g\n", color.x, color.y, color.z);
			fprintf(outfile, "\tlocation %1.5g %1.5g %1.5g\n",
					obj->o_transform.displacement.x,
					obj->o_transform.displacement.y,
					obj->o_transform.displacement.z);
			fprintf(outfile, "}\n");
			break;

		case spotlight_obj:
			{
				double	radius;
				double	cos_rad;
				Vector	direction;

				/* Calculate the radius. */
				VSub(obj->o_world_verts[0], obj->o_world_verts[9], vect1);
				VSub(obj->o_world_verts[8], obj->o_world_verts[9], vect2);
				VUnit(vect1, radius, vect1);
				VUnit(vect2, radius, vect2);
				cos_rad = VDot(vect1, vect2);
				radius = acos(cos_rad);

				if ( ((LightInfoPtr)obj->o_attribs)->flag )
				{
					/* Invert it. */
					/* vect2 still points toward direction. */
					VScalarMul(vect2, -1.0, vect2);
					VAdd(vect2, obj->o_world_verts[9], direction);

					radius += M_PI_2;
				}
				else
					direction = obj->o_world_verts[8];

				fprintf(outfile, "SpotLight {\n");
				fprintf(outfile, "\ton TRUE\n");
				fprintf(outfile, "\tintensity %g\n", intensity);
				fprintf(outfile, "\tcolor %g %g %g\n",color.x,color.y,color.z);
				fprintf(outfile, "\tlocation %1.5g %1.5g %1.5g\n",
						obj->o_transform.displacement.x,
						obj->o_transform.displacement.y,
						obj->o_transform.displacement.z);
				fprintf(outfile, "\tdirection %1.15g %1.15g %1.15g\n",
						direction.x, direction.y, direction.z);
				fprintf(outfile, "\tdropOffRate 0\n");
				fprintf(outfile, "\tcutOffAngle %f\n", radius);
				fprintf(outfile, "}\n");
			}
			break;

		case dirlight_obj:
			VSub(obj->o_world_verts[5], obj->o_world_verts[0], vect1);
			fprintf(outfile, "DirectionalLight {\n");
			fprintf(outfile, "\ton TRUE\n");
			fprintf(outfile, "\tintensity %g\n", intensity);
			fprintf(outfile, "\tcolor %g %g %g\n", color.x, color.y, color.z);
			fprintf(outfile, "\tdirection ");	VPrint(outfile, vect1);
			fprintf(outfile, "}\n");
			break;

		default:;
	}
	fprintf(outfile, "\n");
}


static void
VRML_Export_Lights(FILE *outfile, InstanceList insts)
{
	InstanceList	elmt;

	/* Go through all the instances looking for lights. */
	for ( elmt = insts ; elmt ; elmt = elmt->next )
		if ( Obj_Is_Light(elmt->the_instance) &&
			 ! Obj_Is_Construction(elmt->the_instance) )
			VRML_Export_Light(outfile, elmt->the_instance);
}


static void
VRML_Export_Attributes(FILE *outfile, AttributePtr attribs, ColorVectorPtr amb)
{
	if ( ! Attribs_Defined(attribs) )
		return;

	if ( attribs->use_extension && attribs->extension[VRML] )
		fprintf(outfile, "%s\n", attribs->extension[VRML]);
	else
	{
		fprintf(outfile, "Material {\n");
		fprintf(outfile, "\tambientColor %g %g %g\n",
				amb->red * attribs->colour.red,
				amb->green * attribs->colour.green,
				amb->blue * attribs->colour.blue);
		fprintf(outfile, "\tdiffuseColor %g %g %g\n",
				attribs->diff_coef * attribs->colour.red,
				attribs->diff_coef * attribs->colour.green, 
				attribs->diff_coef * attribs->colour.blue);
		fprintf(outfile, "\tspecularColor %g %g %g\n",
				attribs->spec_coef * attribs->colour.red,
				attribs->spec_coef * attribs->colour.green,
				attribs->spec_coef * attribs->colour.blue);
		fprintf(outfile, "\tshininess %g\n", attribs->reflect_coef);
		fprintf(outfile, "\ttransparency %g\n", attribs->transparency);
		fprintf(outfile, "}\n");
	}
}

static void
VRML_Export_Transform(FILE *outfile, Transformation *transform)
{
	fprintf(outfile, "MatrixTransform {\n");
	fprintf(outfile,
			"\tmatrix %1.5g %1.5g %1.5g 0\n"
			"\t       %1.5g %1.5g %1.5g 0\n"
			"\t       %1.5g %1.5g %1.5g 0\n"
			"\t       %1.5g %1.5g %1.5g 1\n"
			"}\n",
			transform->matrix.x.x, transform->matrix.y.x, transform->matrix.z.x,
			transform->matrix.x.y, transform->matrix.y.y, transform->matrix.z.y,
			transform->matrix.x.z, transform->matrix.y.z, transform->matrix.z.z,
			transform->displacement.x, transform->displacement.y,
			transform->displacement.z);

}


static int
VRML_Compare_Func(const void *a, const void *b)
{
	AttributePtr	a_val, b_val;

	if ( ((FacePtr)a)->face_attribs && ((AttributePtr)((FacePtr)a)->face_attribs)->defined )
		a_val = (AttributePtr)((FacePtr)a)->face_attribs;
	else
		a_val = NULL;

	if ( ((FacePtr)b)->face_attribs && ((AttributePtr)((FacePtr)b)->face_attribs)->defined )
		b_val = (AttributePtr)((FacePtr)b)->face_attribs;
	else
		b_val = NULL;

	if ( a_val == b_val )
		return 0;
	else if ( a_val < b_val )
		return -1;
	else
		return 1;

	return 1; /* To keep compilers happy. */
}


static Boolean
VRML_Check_Size(FacePtr face, WireframePtr wire)
{
	Vector	temp_v;
	int		i;

#define VECT_OLD_EPSILON VECT_EPSILON
#undef VECT_EPSILON
#define VECT_EPSILON 1e-5

	for ( i = 1 ; i < face->num_vertices ; i++ )
	{
		VSub(wire->vertices[face->vertices[i]],
			 wire->vertices[face->vertices[i - 1]], temp_v);
		if ( VNearZero(temp_v) )
			return FALSE;
	}

	return TRUE;

#undef VECT_EPSILON
#define VECT_EPSILON VECT_OLD_EPSILON
}


static void
VRML_Export_Wireframe_Obj(FILE *outfile, WireframePtr wire, Boolean sort,
						  ColorVectorPtr amb, Boolean reuse)
{
	int	i, j, k;
	int	start_i;
	AttributePtr	last_attribs, new_attribs;

	if ( ! wire->num_faces )
		return;

	if ( reuse && Hash_Get_Value(wire_hash, (unsigned long)wire) != (void*)-1 )
	{
		fprintf(outfile, "USE Wire_%ld\n", (unsigned long)wire);
		return;
	}
	else
	{
		if ( reuse )
			Hash_Insert(wire_hash, (unsigned long)wire, (void*)1);
		fprintf(outfile, "DEF Wire_%ld Separator {\n", (unsigned long)wire);

		if ( sort )
			/* We wish to sort faces based on attributes. This is so we
			** can easily export faces with identical attributes in one block.
			*/
			qsort((void*)wire->faces, wire->num_faces, sizeof(Face),
				  VRML_Compare_Func);

		/* Export all the vertices. */
		fprintf(outfile, "Coordinate3 { point [\n");
		for ( i = 0 ; i < wire->num_real_verts ; i++ )
		{
			fprintf(outfile, "\t%1.5g %1.5g %1.5g",
				wire->vertices[i].x, wire->vertices[i].y, wire->vertices[i].z);
			if ( i < wire->num_real_verts - 1 )
				fprintf(outfile, ",\n");
		}
		fprintf(outfile, "\t]\n}\n");

		/* All the normals, if present. */
		if ( wire->vertex_normals )
		{
			fprintf(outfile, "Normal { vector [\n");
			for ( i = 0 ; i < wire->num_real_verts ; i++ )
			{
				fprintf(outfile, "\t%1.5g %1.5g %1.5g",
						wire->vertex_normals[i].x, wire->vertex_normals[i].y,
						wire->vertex_normals[i].z);
				if ( i < wire->num_real_verts - 1 )
					fprintf(outfile, ",\n");
			}
			fprintf(outfile, "\t]\n}\n");
		}

		last_attribs = wire->faces[0].face_attribs &&
					   ((AttributePtr)wire->faces[0].face_attribs)->defined ?
					   (AttributePtr)wire->faces[0].face_attribs : NULL;
		for ( i = 0, start_i = 0 ; i < wire->num_faces ; start_i = i )
		{
			if ( last_attribs )
				VRML_Export_Attributes(outfile, last_attribs, amb);
			fprintf(outfile, "IndexedFaceSet { coordIndex [\n");
			for ( ; i < wire->num_faces ; i++ )
			{
				new_attribs = ( wire->faces[i].face_attribs &&
						((AttributePtr)wire->faces[i].face_attribs)->defined ) ?
						(AttributePtr)wire->faces[i].face_attribs : NULL;
				if ( new_attribs != last_attribs ) break;
				if ( ! VRML_Check_Size(wire->faces + i, wire) )
					continue;
				if ( i != start_i )
					fprintf(outfile, "-1,\n");
				fprintf(outfile, "\t");
				for ( j = 0 ; j < wire->faces[i].num_vertices ; j++ )
					fprintf(outfile, "%d, ", wire->faces[i].vertices[j]);
			}
			fprintf(outfile, "-1\n\t]\n");

			if ( wire->vertex_normals )
			{
				fprintf(outfile, "normalIndex [\n");
				for ( k = start_i ; k < i ; k++ )
				{
					fprintf(outfile, "\t");
					for ( j = 0 ; j < wire->faces[k].num_vertices ; j++ )
						fprintf(outfile, "%d, ", wire->faces[k].vertices[j]);
					if ( k < i - 1 )
						fprintf(outfile, "-1,\n");
					else
						fprintf(outfile, "-1\n");
				}
				fprintf(outfile, "\t]\n");
			}
			fprintf(outfile, "}\n");

			last_attribs = new_attribs;
		}
		fprintf(outfile, "}\n");
	}

}


static void
VRML_Export_LOD_Wireframes(FILE *outfile, WireframePtr *wireframes,
						   LODInfoPtr lods, int num_wires,
						   Boolean sort, ColorVectorPtr amb, Boolean reuse)
{
	int	i, j;

	if ( lods->num_lods >= num_wires )
	{
		if ( num_wires == 1 )
		{
			VRML_Export_Wireframe_Obj(outfile, wireframes[0], sort, amb,
									  reuse);
			return;
		}
		else
			i = lods->num_lods - num_wires + 1;
	}
	else
		i = 0;

	fprintf(outfile, "LOD {\n\trange [ ");
	fprintf(outfile, "%g ", lods->lods[i++]);
	for ( ; i < lods->num_lods ; i++ )
		fprintf(outfile, ", %g ", lods->lods[i]);
	fprintf(outfile, "]\n");

	for ( j = num_wires - 1, i = 0 ; i <= lods->num_lods && j >= 0 ; j--, i++ )
		VRML_Export_Wireframe_Obj(outfile, wireframes[j], sort, amb, reuse);

	fprintf(outfile, "}\n");
}


static void
VRML_Export_LOD_Object(FILE *outfile, ObjectInstancePtr obj, ColorVectorPtr amb)
{
	WireframePtr	*wires;
	WireframePtr	orig_wire = obj->o_wireframe;
	int				num_wires;
	int				i;

	num_wires = Wireframe_Density_Level(obj) + 1;

	wires = New(WireframePtr, num_wires);
	for ( i = 0 ; i < num_wires ; i++ )
	{
		obj->o_wireframe = obj->o_parent->b_wireframes[i];
		wires[i] = Object_To_Wireframe(obj, FALSE, FALSE);
	}

	VRML_Export_LOD_Wireframes(outfile, wires, obj->o_lods, num_wires, FALSE,
							   amb, FALSE);

	for ( i = 0 ; i < num_wires ; i++ )
		Wireframe_Destroy(wires[i]);
	free(wires);

	obj->o_wireframe = orig_wire;
}


static void
VRML_Export_Geometry(FILE *outfile, ObjectInstancePtr obj, ColorVectorPtr amb)
{
	switch ( obj->o_parent->b_class )
	{
		case cube_obj:
			fprintf(outfile, "Cube {}\n");
			break;

		case cylinder_obj:
			/* Need to rotate it. */
			fprintf(outfile, "Rotation { rotation 1 0 0 %g }\n", M_PI_2);
			if ( Attribs_Defined((AttributePtr)obj->o_attribs) &&
				 ((AttributePtr)obj->o_attribs)->use_extension &&
				 ((AttributePtr)obj->o_attribs)->open )
				fprintf(outfile, "Cylinder { parts SIDES }\n");
			else
				fprintf(outfile, "Cylinder {}\n");
			break;

		case cone_obj:
			/* Need to rotate it. */
			fprintf(outfile, "Rotation { rotation 1 0 0 %g }\n", M_PI_2);
			if ( Attribs_Defined((AttributePtr)obj->o_attribs) &&
				 ((AttributePtr)obj->o_attribs)->use_extension &&
				 ((AttributePtr)obj->o_attribs)->open )
				fprintf(outfile, "Cone { parts SIDES }\n");
			else
				fprintf(outfile, "Cone {}\n");
			break;

		case sphere_obj:
			fprintf(outfile, "Sphere {}\n");
			break;

		case plane_obj:
			fprintf(outfile, "Coordinate3 {\n");
			fprintf(outfile,
					"\tpoint [ 10 10 0, -10 10 0, -10 -10 0, 10 -10 0 ]\n");
			fprintf(outfile, "}\n");
			fprintf(outfile,
					"IndexedFaceSet {coordIndex [ 0, 3, 2, 1, -1, ]}\n");
			break;

		case triangle_obj:
			fprintf(outfile, "Coordinate3 {\n");
			fprintf(outfile, "\tpoint [ ");
			fprintf(outfile, "%1.5g %1.5g %1.5g, ",
					control_part(obj)->control_verts[0].x,
					control_part(obj)->control_verts[0].y,
					control_part(obj)->control_verts[0].z);
			fprintf(outfile, "%1.5g %1.5g %1.5g, ",
					control_part(obj)->control_verts[1].x,
					control_part(obj)->control_verts[1].y,
					control_part(obj)->control_verts[1].z);
			fprintf(outfile, "%1.5g %1.5g %1.5g ",
					control_part(obj)->control_verts[2].x,
					control_part(obj)->control_verts[2].y,
					control_part(obj)->control_verts[2].z);
			fprintf(outfile, "]\n");
			fprintf(outfile, "}\n");
			fprintf(outfile,
					"IndexedFaceSet {coordIndex [ 0, 1, 2, -1 ]}\n");
			break;

		case torus_obj:
		case bezier_obj:
			if ( obj->o_lods )
				VRML_Export_LOD_Object(outfile, obj, amb);
			else
			{
				WireframePtr	wire = Object_To_Wireframe(obj, FALSE, FALSE);
				VRML_Export_Wireframe_Obj(outfile, wire, FALSE, amb, FALSE);
				Wireframe_Destroy(wire);
			}
			break;

		case wireframe_obj:
		case csg_obj:
 			if ( ! obj->o_wireframe )
 			{
 				/* Must be a CSG preview. */
 				WireframePtr	main_wireframe;
 
 				main_wireframe =
					CSG_Generate_Wireframe(obj->o_parent->b_csgptr, 0, FALSE);
 				obj->o_wireframe = Wireframe_Simplify(main_wireframe, FALSE);
				Wireframe_Destroy(main_wireframe);
 				VRML_Export_Wireframe_Obj(outfile, obj->o_wireframe, TRUE, amb,
										  TRUE);
 				Wireframe_Destroy(obj->o_wireframe);
 				obj->o_wireframe = NULL;
			}
			else if ( obj->o_lods )
				VRML_Export_LOD_Wireframes(outfile,
										   obj->o_parent->b_major_wires,
										   obj->o_lods,
										   obj->o_parent->b_max_density + 1,
										   TRUE, amb, TRUE);
			else
				VRML_Export_Wireframe_Obj(outfile,
					obj->o_parent->b_major_wires[Wireframe_Density_Level(obj)],
					TRUE, amb, TRUE);
			break;

		default:;
	}
}

static void
VRML_Export_Alias(FILE *outfile, ObjectInstancePtr obj, ColorVectorPtr amb)
{
	char	*alias = (char*)obj->o_aliases[VRML];
	int		index;

	fprintf(outfile, "Separator {\n");

	index = 0;
	while ( alias[index] != '\0' )
	{
		if ( alias[index] == '(' && alias[index + 1] == '*' )
		{
			if ( ! strncmp(alias + index, "(*transform*)", 13) )
			{
				VRML_Export_Transform(outfile, &(obj->o_transform));
				index += 13;
			}
			else if ( ! strncmp(alias + index, "(*attributes*)", 14) )
			{
				VRML_Export_Attributes(outfile, (AttributePtr)obj->o_attribs,
									   amb);
				index += 14;
			}
			else if ( ! strncmp(alias + index, "(*geometry*)", 12) )
			{
				VRML_Export_Geometry(outfile, obj, amb);
				index += 12;
			}
			else
				fputc((int)alias[index++], outfile);
		}
		else
			fputc((int)alias[index++], outfile);
	}

	fprintf(outfile, "}\n\n");
}

static void
VRML_Export_Object(FILE *outfile, ObjectInstancePtr obj, ColorVectorPtr amb)
{
	fprintf(outfile, "# %s\n", obj->o_label);

	if ( Obj_Is_Aliased(obj, VRML) )
	{
		VRML_Export_Alias(outfile, obj, amb);
		return;
	}

	fprintf(outfile, "Separator {\n");

	VRML_Export_Transform(outfile, &(obj->o_transform));
	VRML_Export_Attributes(outfile, (AttributePtr)obj->o_attribs, amb);

	VRML_Export_Geometry(outfile, obj, amb);

	fprintf(outfile, "}\n\n");
}


static void
VRML_Export_Instances(FILE *outfile, InstanceList insts, ColorVectorPtr amb)
{
	for ( ; insts ; insts = insts->next )
		if ( ! Obj_Is_Light(insts->the_instance) &&
			 ! Obj_Is_Construction(insts->the_instance) )
			VRML_Export_Object(outfile, insts->the_instance, amb);
}

