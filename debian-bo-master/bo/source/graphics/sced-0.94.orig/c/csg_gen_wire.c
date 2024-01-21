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
**	csg_gen_wire.c: Generating functions for generic CSG wireframes.
*/

#include <math.h>
#include <config.h>
#include <sced.h>
#include <csg.h>
#include <csg_wire.h>
#include <torus.h>
#include <stdlib.h>

typedef struct _SortVertex {
	Vector	location;
	int		posn;
	} SortVertex, *SortVertexPtr;

static int
QSort_Compare_Func(const void *a, const void *b)
{
	CSGVertexPtr	v1 = (CSGVertexPtr)a;
	CSGVertexPtr	v2 = (CSGVertexPtr)b;

	if ( v1->location.x == v2->location.x )
	{
		if ( v1->location.y == v2->location.y )
		{
			if ( v1->location.z < v2->location.z )
				return -1;
			else
				return 1;
		}
		else if ( v1->location.y < v2->location.y )
			return -1;
		else
			return 1;
	}
	else if ( v1->location.x < v2->location.x )
		return -1;
	else
		return 1;
}


static int
KD_Compare_Func(void *e, void *val, void *data, int depth)
{
	int				val_ind = (int)e;
	int				test_ind = (int)val;
	CSGVertexPtr	verts = (CSGVertexPtr)data;
	int				comp_field = depth % 3;

	switch ( comp_field )
	{
		case 0:
			if ( verts[test_ind].location.x < verts[val_ind].location.x )
				return -1;
			else
				return 1;
		case 1:
			if ( verts[test_ind].location.y < verts[val_ind].location.y )
				return -1;
			else
				return 1;
		case 2:
			if ( verts[test_ind].location.z < verts[val_ind].location.z )
				return -1;
			else
				return 1;
	}

	return 1;	/* To keep the compiler happy. */
}

static void
CSG_Build_Add_Vertex(KDTree *tree, int start, int end, CSGVertexPtr verts,
					 SortVertexPtr sort_verts)
{
	int	length = end - start;
	int	median;

	if ( length < 0 )
		return;

	if ( length == 0 )
	{
		KD_Add_Elmt(tree, (void*)sort_verts[start].posn, (void*)verts,
					KD_Compare_Func);
		return;
	}

	median = start + ( length / 2 );
	KD_Add_Elmt(tree, (void*)sort_verts[median].posn, (void*)verts,
				KD_Compare_Func);
	CSG_Build_Add_Vertex(tree, start, median - 1, verts, sort_verts);
	CSG_Build_Add_Vertex(tree, median + 1, end, verts, sort_verts);
}


void
CSG_Build_KD_Tree(CSGWireframePtr wire)
{
	SortVertexPtr	sort_verts = New(SortVertex, wire->num_vertices);
	int	i;

	for ( i = 0 ; i < wire->num_vertices ; i++ )
	{
		sort_verts[i].location = wire->vertices[i].location;
		sort_verts[i].posn = i;
	}

	/* Sort the vertices. */
	qsort((void*)sort_verts, wire->num_vertices, sizeof(SortVertex),
		  QSort_Compare_Func);

	CSG_Build_Add_Vertex(&(wire->vertex_tree), 0, wire->num_vertices - 1,
						 wire->vertices, sort_verts);

	free(sort_verts);
}


CSGWireframePtr
CSG_Generic_Wireframe(ObjectInstancePtr src, int add_level)
{
	WireframePtr	src_wire;
	CSGWireframePtr	res;
	Vector			v1, v2;
	double			temp_d;
	int				i, j, k, l;
	Boolean			invert;

	src_wire = Dense_Wireframe(src->o_parent,
							   Wireframe_Density_Level(src) + add_level);

	switch ( src->o_parent->b_class )
	{
		case cube_obj:
		case sphere_obj:
		case cylinder_obj:
		case cone_obj:
		case torus_obj:
		case plane_obj:
		case triangle_obj:
		case bezier_obj:
		case light_obj:
		case spotlight_obj:
		case dirlight_obj:
		case arealight_obj:
			res = Wireframe_To_CSG(src_wire, FALSE);
			break;

		case csg_obj:
			res = Wireframe_To_CSG(src->o_parent->b_major_wires[
									Wireframe_Density_Level(src) + add_level],
								   FALSE);
			break;

		case wireframe_obj:
			res = Wireframe_To_CSG(src->o_parent->b_major_wires[0], FALSE);
			break;
	}

	/* Transform all the points. */
	for ( i = 0 ; i < res->num_vertices ; i++ )
		Ref_Transform_Vector(src, res->vertices[i].location,
							 res->vertices[i].location);

#define res_vert(i, j)	res->vertices[res->faces[i].face_vertices[j]].location

	VCross(src->o_transform.matrix.y, src->o_transform.matrix.z, v1);
	invert = ( VDot(src->o_transform.matrix.x, v1) < 0 );

	/* Perform operations on faces. */
	for ( i = 0 ; i < res->num_faces ; i++ )
	{
		if ( invert )
		{
			for ( j = 0, k = res->faces[i].face_num_vertices - 1 ;
				  j < k ;
				  j++, k-- )
			{
				l = res->faces[i].face_vertices[j];
				res->faces[i].face_vertices[j] = res->faces[i].face_vertices[k];
				res->faces[i].face_vertices[k] = l;
			}
		}

		/* Recalculate all the planes.  */
		/* It's as fast as transforming, and should be more accurate. */
		VSub(res_vert(i, 0), res_vert(i, 1), v1);
		VSub(res_vert(i, 2), res_vert(i, 1), v2);
		VCross(v1, v2, res->faces[i].face_plane.p_vector);
		VUnit(res->faces[i].face_plane.p_vector, temp_d,
			  res->faces[i].face_plane.p_vector);
		res->faces[i].face_plane.p_point = res_vert(i, 0);

		/* Generate the bounding boxes. */
		CSG_Face_Bounding_Box(res->vertices, res->faces[i].face_vertices,
							  res->faces[i].face_num_vertices,
							  &(res->faces[i].face_extent));

		/* Set face pointers. These are used in exporting attributes to
		** polygon based renderers such as Radiance. Each face must have
		** attributes attached. If the object currently under consideration
		** is not a csg object, then its attributes derive from the object
		** itself. If however it is a csg object, then the attributes derive
		** from the original face if that face had any, or the current object
		** if it didn't.
		*/
		if ( ( src->o_parent->b_class != wireframe_obj &&
			   src->o_parent->b_class != csg_obj ) ||
			 ( res->faces[i].face_attribs &&
			   ! ( res->faces[i].face_attribs->defined ) ) )
			res->faces[i].face_attribs = src->o_attribs;
	}

	/* Build the kd_tree for the vertices. */
	CSG_Build_KD_Tree(res);

	/* Generate the overall bounding box. */
	CSG_Bounding_Box(res->vertices, res->num_vertices, &(res->obj_extent));

	return res;
}

#if I_EVER_WANT_THIS

/* Not needed at the moment, maybe never again. */

CSGWireframePtr
CSG_Copy_Wireframe(CSGWireframePtr src)
{
	CSGWireframePtr	ret = New(CSGWireframe, 1);
	int				i, j;

	ret->num_vertices = src->num_vertices;
	ret->max_vertices = src->num_vertices;
	ret->vertices = New(CSGVertex, ret->num_vertices);
	for ( i = 0 ; i < ret->num_vertices ; i++ )
	{
		ret->vertices[i].location = src->vertices[i].location;
		ret->vertices[i].num_adjacent = ret->vertices[i].max_num_adjacent = 0;
		ret->vertices[i].adjacent = NULL;
		ret->vertices[i].status = vertex_unknown;
	}

	ret->num_faces = src->num_faces;
	ret->faces = New(CSGFace, ret->num_faces);
	for ( i = 0 ; i < ret->num_faces ; i++ )
	{
		ret->faces[i].face_num_vertices = src->faces[i].face_num_vertices;
		ret->faces[i].face_vertices =
			New(int, ret->faces[i].face_num_vertices);
		for ( j = 0 ; j < ret->faces[i].face_num_vertices ; j++ )
			ret->faces[i].face_vertices[j] = src->faces[i].face_vertices[j];
		ret->faces[i].face_plane = src->faces[i].face_plane;
		ret->faces[i].face_attribs = src->faces[i].face_attribs;
	}

	return ret;
}

#endif
