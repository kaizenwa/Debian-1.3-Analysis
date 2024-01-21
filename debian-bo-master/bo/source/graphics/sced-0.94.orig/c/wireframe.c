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
**	wire_select_box.c : Functions for displaying, modifying and using the
**						select wire object dialog.
*/

/*
**	Imported wireframe files are expected to be in the OFF format.
**	The user selects the header file as the file to load. From this, a
**	geometry file and optional normal and color files are determined.
**	Only ASCII type files are loaded. Binary files can be converted to
**	ASCII format if desired, using the conversion program provided with
**	the off distribution.
*/

#include <math.h>
#include <sced.h>
#include <attributes.h>
#include <bezier.h>
#include <csg.h>
#include <csg_wire.h>
#include <edge_table.h>
#include <hash.h>
#include <torus.h>

#define X_COORD	0
#define Y_COORD 1
#define Z_COORD 2
#define NEG_X_COORD	3
#define NEG_Y_COORD 4
#define NEG_Z_COORD 5

typedef struct _EdgeValue {
	int	num_faces;
	int	max_faces;
	int	*faces;
	} EdgeValue, *EdgeValuePtr;


static void			Wireframe_Merge_Faces(WireframePtr, EdgeTable);
static void			Wireframe_Remove_Colinear_Edges(WireframePtr);
static void			Wireframe_Remove_Vertices(WireframePtr);


WireframePtr
Wireframe_Copy(WireframePtr src)
{
	WireframePtr	result = New(Wireframe, 1);
	int	i, j;

	result->num_vertices = src->num_vertices;
	result->num_real_verts = src->num_real_verts;
	result->vertices = New(Vector, result->num_vertices);
	for ( i = 0 ; i < result->num_vertices ; i++ )
		result->vertices[i] = src->vertices[i];

	result->num_faces = src->num_faces;
	result->faces = New(Face, src->num_faces);
	for ( i = 0 ; i < result->num_faces ; i++ )
	{
		result->faces[i].num_vertices = src->faces[i].num_vertices;
		result->faces[i].vertices = New(int, result->faces[i].num_vertices);
		for ( j = 0 ; j < result->faces[i].num_vertices ; j++ )
			result->faces[i].vertices[j] = src->faces[i].vertices[j];
		result->faces[i].normal = src->faces[i].normal;
		result->faces[i].face_attribs = src->faces[i].face_attribs;
	}

	if ( src->edges )
		result->edges = Edge_Table_Copy(src->edges, src->num_real_verts);
	else
		result->edges = NULL;

	if ( src->vertex_normals )
	{
		result->vertex_normals = New(Vector, result->num_real_verts);
		for ( i = 0 ; i < result->num_real_verts ; i++ )
			result->vertex_normals[i] = src->vertex_normals[i];
	}
	else
		result->vertex_normals = NULL;

	if ( src->num_attribs )
	{
		result->num_attribs = src->num_attribs;
		result->attribs = New(AttributePtr, src->num_attribs);
		for ( i = 0 ; i < result->num_attribs ; i++ )
		{
			result->attribs[i] = New(Attributes, 1);
			Attribute_Copy(result->attribs[i], src->attribs[i]);
		}
		/* Reset all the face ptrs for the new attributes list. */
		for ( i = 0 ; i < result->num_faces ; i++ )
		{
			for ( j = 0 ;
				  result->faces[i].face_attribs != src->attribs[j] ;
				  j++ );
			result->faces[i].face_attribs = result->attribs[j];
		}
	}
	else
	{
		result->attribs = NULL;
		result->num_attribs = 0;
	}

	return result;
}


void
Wireframe_Destroy(WireframePtr victim)
{
	int	i;

	free(victim->vertices);
	if ( victim->attribs )
	{
		for ( i = 0 ; i < victim->num_attribs ; i++ )
			free(victim->attribs[i]);
		free(victim->attribs);
	}
	for ( i = 0 ; i < victim->num_faces ; i++ )
		free(victim->faces[i].vertices);
	free(victim->faces);
	if ( victim->edges )
		Edge_Table_Free(victim->edges, victim->num_real_verts);
	if ( victim->vertex_normals )
		free(victim->vertex_normals);
	free(victim);
}


CSGWireframePtr
Wireframe_To_CSG(WireframePtr src, Boolean do_planes)
{
	CSGWireframePtr	result = New(CSGWireframe, 1);
	int	i, j;

	result->num_vertices = result->max_vertices = src->num_real_verts;
	result->vertices = New(CSGVertex, result->num_vertices);
	for ( i = 0 ; i < result->num_vertices ; i++ )
	{
		result->vertices[i].location = src->vertices[i];
		result->vertices[i].num_adjacent =
		result->vertices[i].max_num_adjacent = 0;
		result->vertices[i].adjacent = NULL;
		result->vertices[i].status = vertex_unknown;
	}

	result->num_faces = src->num_faces;
	result->faces = New(CSGFace, src->num_faces);
	for ( i = 0 ; i < result->num_faces ; i++ )
	{
		result->faces[i].face_num_vertices = src->faces[i].num_vertices;
		result->faces[i].face_vertices = New(int,
										result->faces[i].face_num_vertices);
		for ( j = 0 ; j < result->faces[i].face_num_vertices ; j++ )
			result->faces[i].face_vertices[j] = src->faces[i].vertices[j];
		result->faces[i].face_plane.p_vector = src->faces[i].normal;
		if ( do_planes )
			result->faces[i].face_plane.p_point =
				result->vertices[result->faces[i].face_vertices[0]].location;
		result->faces[i].face_attribs = src->faces[i].face_attribs;
		result->faces[i].ignore = FALSE;
		result->faces[i].num_intersected =
		result->faces[i].max_num_intersected = 0;
		result->faces[i].intersected = NULL;
	}

	result->vertex_tree = NULL;

	return result;
}


WireframePtr
CSG_To_Wireframe(CSGWireframePtr src)
{
	WireframePtr	result = New(Wireframe, 1);
	int	i, j;

	result->num_vertices = src->num_vertices + 1;
	result->num_real_verts = src->num_vertices;
	result->vertices = New(Vector, result->num_vertices);
	for ( i = 0 ; i < result->num_real_verts ; i++ )
		result->vertices[i] = src->vertices[i].location;
	VNew(0, 0, 0, result->vertices[i]);

	result->num_faces = src->num_faces;
	result->faces = New(Face, src->num_faces);
	for ( i = 0 ; i < result->num_faces ; i++ )
	{
		result->faces[i].num_vertices = src->faces[i].face_num_vertices;
		result->faces[i].vertices = New(int, result->faces[i].num_vertices);
		for ( j = 0 ; j < result->faces[i].num_vertices ; j++ )
			result->faces[i].vertices[j] = src->faces[i].face_vertices[j];
		result->faces[i].normal = src->faces[i].face_plane.p_vector;
		result->faces[i].face_attribs = src->faces[i].face_attribs;
	}

	result->edges = NULL;
	result->vertex_normals = NULL;
	result->attribs = NULL;
	result->num_attribs = 0;

	return result;
}


/*	Construct a wireframe for a given object. The wireframe is
**	transformed appropriately for the object.
**	If copy_attribs is TRUE, new attribute structures are allocated as
**	required, otherwise the pointers only are copied.
*/
WireframePtr
Object_To_Wireframe(ObjectInstancePtr obj, Boolean transform,
					Boolean copy_attribs)
{
	HashTable		attrib_hash;
	WireframePtr	res;
	double			temp_d;
	Vector			temp_v;
	Vector			temp_v1, temp_v2;
	Matrix			transp;
	int				i;
	int				max_attribs;

	res = Wireframe_Copy(obj->o_wireframe);

	if ( Obj_Is_Bezier(obj) )
		Bezier_Calculate_Vertices(obj, res->vertices, res->num_real_verts,
								  res->num_real_verts);

	if ( transform )
	{
		/* Transform vertices. */
		for ( i = 0 ; i < res->num_vertices ; i++ )
			Ref_Transform_Vector(obj, res->vertices[i], res->vertices[i])

		/* Transform normals. */
		if ( Obj_Is_Bezier(obj) )
		{
			for ( i = 0 ; i < res->num_faces ; i++ )
			{
				VSub(res->vertices[res->faces[i].vertices[2]],
					 res->vertices[res->faces[i].vertices[0]], temp_v1);
				VSub(res->vertices[res->faces[i].vertices[1]],
					 res->vertices[res->faces[i].vertices[0]], temp_v2);
				VCross(temp_v1, temp_v2, res->faces[i].normal);
				if ( VZero(res->faces[i].normal) )
					VNew(1, 0, 0, res->faces[i].normal);
				else
					VUnit(res->faces[i].normal, temp_d, res->faces[i].normal);
			}
		}
		else
		{
			MTrans(obj->o_inverse, transp);
			for ( i = 0 ; i < res->num_faces ; i++ )
			{
				if ( VZero(res->faces[i].normal) )
					continue;
				MVMul(transp, res->faces[i].normal, temp_v);
				VUnit(temp_v, temp_d, res->faces[i].normal);
			}
		}


		if ( res->vertex_normals )
		{
			for ( i = 0 ; i < res->num_real_verts ; i++ )
			{
				MVMul(transp, res->vertex_normals[i], temp_v);
				VUnit(temp_v, temp_d, res->vertex_normals[i]);
			}
		}
	}

	res->edges = NULL;

	if ( Obj_Is_Bezier(obj) )
	{
		res->vertex_normals = New(Vector, res->num_real_verts);
		Bezier_Calculate_Vertex_Normals(obj, res->vertex_normals,
										res->num_real_verts, transform);
	}

	if ( Obj_Is_Torus(obj) )
	{
		res->vertex_normals = New(Vector, res->num_real_verts);
		Torus_Calculate_Vertex_Normals(obj, res->vertex_normals,
									   res->num_real_verts, transform);
	}

	if ( copy_attribs )
	{
		attrib_hash = Hash_New_Table();
		res->num_attribs = max_attribs = 0;
		for ( i = 0 ; i < res->num_faces ; i++ )
			if ( Hash_Get_Value(attrib_hash,
				 (unsigned long)(res->faces[i].face_attribs)) == (void*)-1 )
			{
				Hash_Insert(attrib_hash,
							(unsigned long)(res->faces[i].face_attribs),
							(void*)(res->faces[i].face_attribs));
				if ( res->num_attribs == max_attribs )
				{
					if ( max_attribs )
						res->attribs = More(res->attribs, AttributePtr,
											max_attribs + 3);
					else
						res->attribs = New(AttributePtr, 3);
					max_attribs += 3;
				}
				res->attribs[res->num_attribs] = New(Attributes, 1);
				Attribute_Copy(res->attribs[res->num_attribs],
							   res->faces[i].face_attribs);
				res->num_attribs++;
			}
	}

	return res;
}


static void
Wireframe_Edge_Add_Edge(EdgeTable table, int start, int end, int face_num)
{
	EdgePtr			edge;
	EdgeValuePtr	val;

	if ( ! ( edge = Edge_Table_Get(table, start, end) ) )
	{
		edge = Edge_Table_Add(table, start, end);
		val = New(EdgeValue, 1);
		edge->val = (void*)val;
		val->num_faces = val->max_faces = 0;
	}
	else
		val = (EdgeValuePtr)edge->val;

	if ( face_num == -1 )
		return;

	/* Add the face. */
	if ( val->num_faces == val->max_faces )
	{
		if ( val->max_faces )
			val->faces = More(val->faces, int, val->max_faces + 2);
		else
			val->faces = New(int, 2);
		val->max_faces += 2;
	}
	val->faces[val->num_faces++] = face_num;
}


static void
Wireframe_Edge_Free_Table(EdgeTable table, int size)
{
	int	i, j;

	for ( i = 0 ; i < size ; i++ )
		for ( j = 0 ; j < table[i].num_edges ; j++ )
		{
			if ( ((EdgeValuePtr)table[i].edges[j].val)->num_faces )
				free(((EdgeValuePtr)table[i].edges[j].val)->faces);
			free(table[i].edges[j].val);
		}
	Edge_Table_Free(table, size);
}



WireframePtr
Wireframe_Simplify(WireframePtr src, Boolean stagger)
{
	WireframePtr	res;
	EdgeTable		edge_table;
	int				i, j;

	/* Take a copy of src. */
	res = Wireframe_Copy(src);

	/* Build an edge table. */
	edge_table = Edge_Table_Create(res->num_real_verts);
	for ( i = 0 ; i < res->num_faces ; i++ )
	{
		Wireframe_Edge_Add_Edge(edge_table, res->faces[i].vertices[0],
					  res->faces[i].vertices[res->faces[i].num_vertices-1],
					  i);
		for ( j = 1 ; j < res->faces[i].num_vertices ; j++ )
			Wireframe_Edge_Add_Edge(edge_table, res->faces[i].vertices[j-1],
						  			res->faces[i].vertices[j], i);
	}

	/* Merge faces. */
	Wireframe_Merge_Faces(res, edge_table);

	/* Get rid of excess edges.*/
	Wireframe_Remove_Colinear_Edges(res);

	/* Remove surplus vertices. */
	Wireframe_Remove_Vertices(res);

	/* Free the edge table. */
	Wireframe_Edge_Free_Table(edge_table, src->num_real_verts);

	return res;
}


static void
Wireframe_Merge_Face(WireframePtr src, FacePtr f1, FacePtr f2, int edge_start,
					 int edge_end)
{
	int	index;
	int	old_num;
	int	i, j;
	int	f1_start, f1_end, f2_start, f2_end;

	f1_start = f1_end = f2_start = f2_end = -1;

	/* Find the indices for the vertices involved. */
	for ( i = 0 ; i < f1->num_vertices ; i++ )
	{
		index = f1->vertices[i];
		if ( index == edge_start || index == edge_end )
		{
			f1_start = i;
			if ( i )
				f1_end = i + 1;
			else
			{
				index = f1->vertices[i+1];
				if ( index == edge_start || index == edge_end )
					f1_end = i + 1;
				else
				{
					f1_start = f1->num_vertices - 1;
					f1_end = 0;
				}
			}
			if ( ( edge_start == f1->vertices[f1_start] &&
				   edge_end == f1->vertices[f1_end] ) ||
				 ( edge_start == f1->vertices[f1_end] &&
				   edge_end == f1->vertices[f1_start] ) )
				break;
		}
	}
	for ( i = 0 ; i < f2->num_vertices ; i++ )
	{
		index = f2->vertices[i];
		if ( index == edge_start || index == edge_end )
		{
			f2_start = i;
			if ( i )
				f2_end = i + 1;
			else
			{
				index = f2->vertices[i+1];
				if ( index == edge_start || index == edge_end )
					f2_end = i + 1;
				else
				{
					f2_start = f2->num_vertices - 1;
					f2_end = 0;
				}
			}
			if ( ( edge_start == f2->vertices[f2_start] &&
				   edge_end == f2->vertices[f2_end] ) ||
				 ( edge_start == f2->vertices[f2_end] &&
				   edge_end == f2->vertices[f2_start] ) )
				break;
		}
	}

	if ( f1_start == -1 || f1_end == -1 || f2_start == -1 || f2_end == -1 )
		Sced_Abort(FALSE);

	/* f1 now inherits f2's vertices. */
	old_num = f1->num_vertices;
	f1->num_vertices += f2->num_vertices - 2;
	f1->vertices = More(f1->vertices, int, f1->num_vertices);

	/* Move up f1's vertices that occur after and including f1_end. */
	for ( i = old_num + f2->num_vertices - 3, j = old_num - 1 ;
		  f1_end != 0 && j >= f1_end ;
		  j--, i-- )
		f1->vertices[i] = f1->vertices[j];

	/* Copy f2's vertices over. */
	i = f1_start + 1;
	j = ( f2_end == f2->num_vertices - 1 ? 0 : f2_end + 1 );
	while ( j != f2_start )
	{
		f1->vertices[i] = f2->vertices[j];
		i++;
		j = ( j == f2->num_vertices - 1 ? 0 : j + 1 );
	}

	/* free any f2 memory. */
	free(f2->vertices);
	f2->num_vertices = 0;
}


static Boolean
Wireframe_Remove_Internal_Edge(FacePtr face)
{
	/* We know this face intersects itself along an edge.
	** This edge is either on the path into a hole in the face,
	** or a removable internal line. We seek to detect the later case,
	** and remove the internal line.
	** There is no guarantee that an internal line exists, so be
	** prepared to fail and return having done nothing.
	*/

	int	start, end;
	int	nexts, nexte;
	int	i, j;

	/* Look for a triple of the form a-b-a */
	if ( face->vertices[face->num_vertices - 2] == face->vertices[0] )
	{
		start = face->num_vertices - 2;
		end = 0;
	}
	else if ( face->vertices[face->num_vertices - 1] == face->vertices[1] )
	{
		start = face->num_vertices - 1;
		end = 1;
	}
	else
	{
		for ( start = 0, end = 2 ;
			  end < face->num_vertices &&
			  face->vertices[start] != face->vertices[end] ;
			  start++, end++ );
		if ( end >= face->num_vertices )
			return FALSE; /* No internal line. */
	}

	/* Have a triple.
	** The idea now is to find its maximum extent. That is, the maximum
	** length palindrome.
	*/
	nexts = start == 0 ? face->num_vertices - 1 : start - 1;
	nexte = end == face->num_vertices - 1 ? 0 : end + 1;
	while ( face->vertices[nexts] == face->vertices[nexte] )
	{
		start = nexts;
		end = nexte;
		nexts = start == 0 ? face->num_vertices - 1 : start - 1;
		nexte = end == face->num_vertices - 1 ? 0 : end + 1;
	}

	/* DEBUG */
	/*
	printf("%d:", face->num_vertices);
	for ( nexts = 0 ; nexts < face->num_vertices ; nexts++ )
		printf(" %d", face->vertices[nexts]);
	printf(" : %d %d\n", start, end);
	*/

	/* Remove the palindrome. */
	if ( start > end )
	{
		/* The palindrome wraps around the end of the array. */
		/* Work in 2 stages. */
		/* Chop off the end of the array and hack start and end so the
		** next bit works. */
		face->num_vertices = start + 1;
		start = -1;
	}
	for ( i = start + 1, j = end + 1 ;
		  j < face->num_vertices ;
		  i++, j++ )
		face->vertices[i] = face->vertices[j];
	face->num_vertices -= ( end - start );

	/*
	printf("%d:", face->num_vertices);
	for ( nexts = 0 ; nexts < face->num_vertices ; nexts++ )
		printf(" %d", face->vertices[nexts]);
	printf("\n\n");
	*/

	return TRUE;
}



static void
Wireframe_Merge_Faces(WireframePtr src, EdgeTable edge_table)
{
	int		*face_map;
	int		i, j;
	int		end;
	int		num_faces;
	int		f1_index, f2_index;
	FacePtr	f1, f2;
	Vector	diff;

	/* Need a face table, mapping faces to new faces, so to speak. */
	/* Each face starts as itself. */
	face_map = New(int, src->num_faces);
	for ( i = 0 ; i < src->num_faces ; i++ )
		face_map[i] = i;

	/* Work through all the edges. */
	for ( i = 0 ; i < src->num_real_verts ; i++ )
		for ( j = 0 ; j < edge_table[i].num_edges ; j++ )
		{
			/* Only interested in edges with 2 adjacent faces.
			** Other edges must mark angles. */
			if ( ((EdgeValuePtr)edge_table[i].edges[j].val)->num_faces != 2 )
				continue;

			end = edge_table[i].edges[j].endpt;
			for ( f1_index =
					((EdgeValuePtr)edge_table[i].edges[j].val)->faces[0] ;
				  face_map[f1_index] != f1_index ;
				  f1_index = face_map[f1_index] );
			for ( f2_index =
					((EdgeValuePtr)edge_table[i].edges[j].val)->faces[1] ;
				  face_map[f2_index] != f2_index ;
				  f2_index = face_map[f2_index] );
			f1 = src->faces + f1_index;
			f2 = src->faces + f2_index;

			if ( f1 == f2 )
				/* Previously found to be coplanar and merged.	*/
				Wireframe_Remove_Internal_Edge(f1);
			else if ( VEqual(f1->normal, f2->normal, diff) )
			{
				/* Must be co-planar - common edge and equal normals.
				** Merge the faces.
				*/
				/* Check for compatable attributes. */
				if ( f1->face_attribs != f2->face_attribs &&
					 ( ( f1->face_attribs && f1->face_attribs->defined ) ||
					   ( f2->face_attribs && f2->face_attribs->defined ) ) )
					continue;

				Wireframe_Merge_Face(src, f1, f2, i, end);
				face_map[f2_index] = f1_index;
			}
		}

	for ( num_faces = 0, i = 0 ; i < src->num_faces ; i++ )
	{
		if ( face_map[i] == i )
		{
			if ( num_faces == i )
				num_faces++;
			else
				src->faces[num_faces++] = src->faces[i];
		}
	}
	src->num_faces = num_faces;
	/* reallocate to free any extra memory used by deleted faces. */
	src->faces = More(src->faces, Face, src->num_faces);

	free(face_map);
}


static void
Wireframe_Remove_Colinear_Edges(WireframePtr src)
{
	FacePtr	f;
	Boolean	changed;
	int		i, j, k;

	for ( i = 0 ; i < src->num_faces ; i++ )
	{
		changed = FALSE;
		f = src->faces + i;
		for ( j = 0 ; f->num_vertices > 3 && j < f->num_vertices - 2 ; )
		{
			if (  Points_Colinear(src->vertices[f->vertices[j]],
								  src->vertices[f->vertices[j+1]],
								  src->vertices[f->vertices[j+2]]) )
			{
				for ( k = j + 2 ; k < f->num_vertices ; k++ )
					f->vertices[k-1] = f->vertices[k];
				f->num_vertices--;
				changed = TRUE;
			}
			else
				j++;
		}
		while ( f->num_vertices > 3 &&
				Points_Colinear(src->vertices[f->vertices[0]],
							src->vertices[f->vertices[f->num_vertices - 2]],
							src->vertices[f->vertices[f->num_vertices - 1]]))
		{
			f->num_vertices--;
			changed = TRUE;
		}
		if ( f->num_vertices > 3 &&
			 Points_Colinear(src->vertices[f->vertices[0]],
							 src->vertices[f->vertices[1]],
							 src->vertices[f->vertices[f->num_vertices - 1]]))
		{
			for ( k = 1 ; k < f->num_vertices ; k++ )
				f->vertices[k-1] = f->vertices[k];
			f->num_vertices--;
			changed = TRUE;
		}

		if ( changed )
			f->vertices = More(f->vertices, int, f->num_vertices);
	}
}


static void
Wireframe_Remove_Vertices(WireframePtr src)
{
	Boolean	*used = New(Boolean, src->num_real_verts);
	int		*map = New(int, src->num_real_verts);
	int		num_verts;
	int		i, j;

	for ( i = 0 ; i < src->num_real_verts ; i++ )
		used[i] = FALSE;

	for ( i = 0 ; i < src->num_faces ; i++ )
		for ( j = 0 ; j < src->faces[i].num_vertices ; j++ )
			used[src->faces[i].vertices[j]] = TRUE;

	for ( num_verts = 0, i = 0 ; i < src->num_real_verts ; i++ )
	{
		if ( used[i] )
		{
			if ( num_verts == i )
			{
				num_verts++;
				map[i] = i;
			}
			else
			{
				map[i] = num_verts;
				src->vertices[num_verts++] = src->vertices[i];
			}
		}
	}
	src->num_vertices = src->num_real_verts + 1;
	/* reallocate vertices to free any excess. */
	src->vertices = More(src->vertices, Vector, src->num_vertices);
	VNew(0, 0, 0, src->vertices[src->num_vertices - 1]);

	/* Redo vertex ptr in faces. */
	for ( i = 0 ; i < src->num_faces ; i++ )
		for ( j = 0 ; j < src->faces[i].num_vertices ; j++ )
			src->faces[i].vertices[j] = map[src->faces[i].vertices[j]];

	free(used);
	free(map);
}


static double
Triangulate_Area_2(Vector *v1, Vector *v2, Vector *v3, int max_c)
{
	switch ( max_c )
	{
	  case X_COORD:
	  case NEG_X_COORD:
		return v1->y * v2->z - v1->z * v2->y +
			   v1->z * v3->y - v1->y * v3->z +
			   v2->y * v3->z - v3->y * v2->z;
	  case Y_COORD:
	  case NEG_Y_COORD:
		return v1->z * v2->x - v1->x * v2->z +
			   v1->x * v3->z - v1->z * v3->x +
			   v2->z * v3->x - v3->z * v2->x;
	  case Z_COORD:
	  case NEG_Z_COORD:
		return v1->x * v2->y - v1->y * v2->x +
			   v1->y * v3->x - v1->x * v3->y +
			   v2->x * v3->y - v3->x * v2->y;
	}

	return FALSE;
}


static Boolean
Triangulate_Collinear(Vector *v1, Vector *v2, Vector *v3, int max_c)
{
	double	area = Triangulate_Area_2(v1, v2, v3, max_c);
	return IsZero(area);
}


static Boolean
Triangulate_Right(Vector *v1, Vector *v2, Vector *v3, int max_c)
{
	if ( max_c > Z_COORD )
		return Triangulate_Area_2(v1, v2, v3, max_c) > 0.0;
	else
		return Triangulate_Area_2(v1, v2, v3, max_c) < 0.0;
}


static Boolean
Triangulate_Right_On(Vector *v1, Vector *v2, Vector *v3, int max_c)
{
	if (  max_c > Z_COORD )
		return Triangulate_Area_2(v1, v2, v3, max_c) > -EPSILON;
	else
		return Triangulate_Area_2(v1, v2, v3, max_c) < EPSILON;
}


static Boolean
Triangulate_Intersect_Prop(Vector *v1, Vector *v2, Vector *v3, Vector *v4,
						   int max_c)
{
	if ( Triangulate_Collinear(v1, v2, v3, max_c) ||
		 Triangulate_Collinear(v1, v2, v4, max_c) ||
		 Triangulate_Collinear(v3, v4, v1, max_c) ||
		 Triangulate_Collinear(v3, v4, v2, max_c) )
		return FALSE;

	return ( ( Triangulate_Right(v1, v2, v3, max_c) ^
			   Triangulate_Right(v1, v2, v4, max_c) ) &&
			 ( Triangulate_Right(v3, v4, v1, max_c) ^
			   Triangulate_Right(v3, v4, v2, max_c) ) );
}


static Boolean
Triangulate_Between(Vector *v1, Vector *v2, Vector *v3, int max_c)
{
	if ( ! Triangulate_Collinear(v1, v2, v3, max_c) )
		return FALSE;

	switch ( max_c )
	{
	  case X_COORD:
	  case NEG_X_COORD:
		if ( ! DEqual(v1->y, v2->y) )
			return ( ( v1->y <= v3->y && v3->y <= v2->y ) ||
					 ( v1->y >= v3->y && v3->y >= v2->y ) );
		else
			return ( ( v1->z <= v3->z && v3->z <= v2->z ) ||
					 ( v1->z >= v3->z && v3->z >= v2->z ) );
		break;

	  case Y_COORD:
	  case NEG_Y_COORD:
		if ( ! DEqual(v1->x, v2->x) )
			return ( ( v1->x <= v3->x && v3->x <= v2->x ) ||
					 ( v1->x >= v3->x && v3->x >= v2->x ) );
		else
			return ( ( v1->z <= v3->z && v3->z <= v2->z ) ||
					 ( v1->z >= v3->z && v3->z >= v2->z ) );
		break;

	  case Z_COORD:
	  case NEG_Z_COORD:
		if ( ! DEqual(v1->y, v2->y) )
			return ( ( v1->y <= v3->y && v3->y <= v2->y ) ||
					 ( v1->y >= v3->y && v3->y >= v2->y ) );
		else
			return ( ( v1->x <= v3->x && v3->x <= v2->x ) ||
					 ( v1->x >= v3->x && v3->x >= v2->x ) );
		break;
	}

	return FALSE;
}

static Boolean
Triangulate_Intersect(Vector *v1, Vector *v2, Vector *v3, Vector *v4, int max_c)
{
	if ( Triangulate_Intersect_Prop(v1, v2, v3, v4, max_c) )
		return TRUE;
	else if ( Triangulate_Between(v1, v2, v3, max_c) ||
			  Triangulate_Between(v1, v2, v4, max_c) ||
			  Triangulate_Between(v3, v4, v1, max_c) ||
			  Triangulate_Between(v3, v4, v2, max_c) )
		return TRUE;

	return FALSE;
}


static Boolean
Triangulate_Diagonalie(int i1, int i2, WireframePtr src, int *verts,
					   int num_verts, int max_coord)
{
	int	k;

	if ( ! ( num_verts - 1 == i1 || i1 == 0 ||
			 num_verts - 1 == i2 || i2 == 0 ) &&
		 Triangulate_Intersect(src->vertices + verts[i1],
							   src->vertices + verts[i2],
							   src->vertices + verts[num_verts - 1],
							   src->vertices + verts[0], max_coord) )
			return FALSE;
	for ( k = 1 ; k < num_verts ; k++ )
		if ( ! ( k - 1 == i1 || k == i1 || k - 1 == i2 || k == i2 ) &&
			 Triangulate_Intersect(src->vertices + verts[i1],
								   src->vertices + verts[i2],
								   src->vertices + verts[k - 1],
								   src->vertices + verts[k], max_coord) )
			return FALSE;

	return TRUE;
}


static Boolean
Triangulate_In_Cone(int i1, int i2, WireframePtr src, int *verts, int num_verts,
					int max_coord)
{
	int	i_plus, i_minus;

	i_plus = ( i1 + 1 ) % num_verts;
	i_minus = ( i1 + num_verts - 1 ) % num_verts;

	if ( Triangulate_Right_On(src->vertices + verts[i_minus],
							 src->vertices + verts[i1],
							 src->vertices + verts[i_plus], max_coord) )
		return Triangulate_Right(src->vertices + verts[i1],
								 src->vertices + verts[i2],
								 src->vertices + verts[i_minus], max_coord) &&
			   Triangulate_Right(src->vertices + verts[i2],
								 src->vertices + verts[i1],
								 src->vertices + verts[i_plus], max_coord);
	else
		return ! ( Triangulate_Right_On(src->vertices + verts[i1],
									src->vertices + verts[i2],
									src->vertices + verts[i_plus], max_coord) &&
				   Triangulate_Right_On(src->vertices + verts[i2],
									src->vertices + verts[i1],
									src->vertices + verts[i_minus], max_coord));
}


static Boolean
Triangulate_Diagonal(int i1, int i2, WireframePtr src, int *verts,
					 int num_verts, int max_coord)
{
	return Triangulate_In_Cone(i1, i2, src, verts, num_verts, max_coord) &&
		   Triangulate_Diagonalie(i1, i2, src, verts, num_verts, max_coord);
}

static void
Triangulate_Add_Face(WireframePtr src, int v1, int v2, int v3, WireframePtr res)
{
	Vector	vect_1, vect_2;
	double	temp_d;

	/* Check for 0 size. */
	VSub(src->vertices[v3], src->vertices[v1], vect_1);
	VSub(src->vertices[v2], src->vertices[v1], vect_2);
	VCross(vect_1, vect_2, res->faces[res->num_faces].normal);
	if ( VZero(res->faces[res->num_faces].normal) )
		return;
	VUnit(res->faces[res->num_faces].normal, temp_d,
		  res->faces[res->num_faces].normal);

	res->faces[res->num_faces].num_vertices = 3;
	res->faces[res->num_faces].vertices = New(int, 3);
	res->faces[res->num_faces].vertices[0] = v1;
	res->faces[res->num_faces].vertices[1] = v2;
	res->faces[res->num_faces].vertices[2] = v3;

	res->num_faces++;
	return;
}


/*	Code to triangulate a polygon.
**	Derived from O'Rourke - "Computational Geometry in C"
*/
static void
Triangulate(WireframePtr src, int *verts, int num_verts, WireframePtr res,
			int max_coord)
{
	int		i, j, index, index_2;

	if ( num_verts < 3 )
		return;

	if ( num_verts == 3 )
	{
		Triangulate_Add_Face(src, verts[0], verts[1], verts[2], res);
		return;
	}

	for ( i = 0 ; i < num_verts ; i++ )
	{
		index = ( i + 2 ) % num_verts;
		if ( Triangulate_Diagonal(i, index, src, verts, num_verts, max_coord) )
		{
			index_2 = ( i + 1 ) % num_verts;
			Triangulate_Add_Face(src, verts[i], verts[index_2], verts[index],
								 res);
			for ( j = index_2 + 1 ; j < num_verts ; j++ )
				verts[j - 1] = verts[j];
			Triangulate(src, verts, num_verts - 1, res, max_coord);
			break;
		}
	}
}


/*	Triangulate a polygon face.
*/
WireframePtr
Face_Triangulate(WireframePtr src, FacePtr face)
{
	WireframePtr	res = New(Wireframe, 1);
	int				*orig_face = New(int, face->num_vertices);
	int				max_coord;
	int				i;

	if ( face->num_vertices <= 3 )
		return NULL;

	/* The wireframe returned will have n - 2 faces, with n vertices. */
	res->num_vertices = face->num_vertices;
	res->num_real_verts = face->num_vertices;
	res->vertices = New(Vector, res->num_vertices);
	for ( i = 0 ; i < res->num_vertices ; i++ )
		res->vertices[i] = src->vertices[face->vertices[i]];

	res->num_faces = 0;
	res->faces = New(Face, res->num_vertices - 2);

	if ( fabs(face->normal.x) > fabs(face->normal.y) )
		if ( fabs(face->normal.x) > fabs(face->normal.z) )
			max_coord = face->normal.x > 0.0 ? X_COORD : NEG_X_COORD;
		else
			max_coord = face->normal.z > 0.0 ? Z_COORD : NEG_Z_COORD;
	else
		if ( fabs(face->normal.y) > fabs(face->normal.z) )
			max_coord = face->normal.y > 0.0 ? Y_COORD : NEG_Y_COORD;
		else
			max_coord = face->normal.z > 0.0 ? Z_COORD : NEG_Z_COORD;
	for ( i = 0 ; i < face->num_vertices ; i++ )
		orig_face[i] = face->vertices[i];
	Triangulate(src, orig_face, face->num_vertices, res, max_coord);
	free(orig_face);

	res->edges = NULL;

	if ( face->face_attribs )
	{
		res->attribs = New(AttributePtr, 1);
		res->attribs[0] = New(Attributes, 1);
		Attribute_Copy(res->attribs[0], face->face_attribs);
		res->num_attribs = 1;
		for ( i = 0 ; i < res->num_faces ; i++ )
			res->faces[i].face_attribs = res->attribs[0];
	}
	else
	{
		for ( i = 0 ; i < res->num_faces ; i++ )
			res->faces[i].face_attribs = NULL;
		res->num_attribs = 0;
		res->attribs = NULL;
	}

	if ( src->vertex_normals )
	{
		res->vertex_normals = New(Vector, res->num_real_verts);
		for ( i = 0 ; i < res->num_real_verts ; i++ )
			res->vertex_normals[i] = src->vertex_normals[face->vertices[i]];
	}
	else
		res->vertex_normals = NULL;

	return res;
}


WireframePtr
Wireframe_Triangulate(WireframePtr wire)
{
	WireframePtr	res = New(Wireframe, 1);
	int				*orig_face = New(int, wire->num_vertices);	/* Too big. */
	int				max_faces = 10;
	int				max_c;
	int				old_num;
	int				i, j, index;

	res->num_vertices = wire->num_vertices;
	res->num_real_verts = wire->num_real_verts;
	res->vertices = New(Vector, wire->num_vertices);
	for ( i = 0 ; i < res->num_vertices ; i++ )
		res->vertices[i] = wire->vertices[i];

	res->edges = NULL;

	if ( wire->num_attribs )
	{
		res->num_attribs = wire->num_attribs;
		res->attribs = New(AttributePtr, res->num_attribs);
		for ( i = 0 ; i < res->num_attribs ; i++ )
		{
			res->attribs[i] = New(Attributes, 1);
			Attribute_Copy(res->attribs[i], wire->attribs[i]);
		}
	}
	else
	{
		res->num_attribs = 0;
		res->attribs = NULL;
	}

	/* Triangulate each face. */
	res->num_faces = 0;
	res->faces = New(Face, max_faces);
	for ( i = 0 ; i < wire->num_faces ; i++ )
	{
		if ( res->num_faces + wire->faces[i].num_vertices - 2 > max_faces )
		{
			max_faces += 10;
			res->faces = More(res->faces, Face, max_faces);
		}

		if ( fabs(wire->faces[i].normal.x) > fabs(wire->faces[i].normal.y) )
			if ( fabs(wire->faces[i].normal.x) > fabs(wire->faces[i].normal.z) )
				max_c = wire->faces[i].normal.x > 0.0 ? X_COORD : NEG_X_COORD;
			else
				max_c = wire->faces[i].normal.z > 0.0 ? Z_COORD : NEG_Z_COORD;
		else
			if ( fabs(wire->faces[i].normal.y) > fabs(wire->faces[i].normal.z) )
				max_c = wire->faces[i].normal.y > 0.0 ? Y_COORD : NEG_Y_COORD;
			else
				max_c = wire->faces[i].normal.z > 0.0 ? Z_COORD : NEG_Z_COORD;

		for ( j = 0 ; j < wire->faces[i].num_vertices ; j++ )
			orig_face[j] = wire->faces[i].vertices[j];

		old_num = res->num_faces;

		Triangulate(wire, orig_face, wire->faces[i].num_vertices, res, max_c);

		if ( wire->faces[i].face_attribs )
		{
			for ( index = 0 ;
				  index < wire->num_attribs &&
					wire->attribs[index] != wire->faces[i].face_attribs ;
				  index++ );
			for ( j = old_num ; j < res->num_faces ; j++ )
				res->faces[j].face_attribs = res->attribs[index];
		}
		else
			for ( j = old_num ; j < res->num_faces ; j++ )
				res->faces[j].face_attribs = NULL;
	}
	free(orig_face);

	if ( wire->vertex_normals )
	{
		res->vertex_normals = New(Vector, res->num_real_verts);
		for ( i = 0 ; i < res->num_real_verts ; i++ )
			res->vertex_normals[i] = wire->vertex_normals[i];
	}
	else
		res->vertex_normals = NULL;

	return res;
}


Boolean
Wireframe_Has_Attributes(WireframePtr wire)
{
	int	i;

	for ( i = 0 ; i < wire->num_faces ; i++ )
		if ( wire->faces[i].face_attribs )
			return TRUE;

	return FALSE;
}


int
Wireframe_Count_Edges(WireframePtr wire)
{
	int count;

	if ( wire->edges )
		count = Edge_Table_Count(wire->edges, wire->num_real_verts);
	else
	{
		Edge_Table_Build(wire);
		count = Edge_Table_Count(wire->edges, wire->num_real_verts);
		Edge_Table_Free(wire->edges, wire->num_real_verts);
		wire->edges = NULL;
	}

	return count;
}

