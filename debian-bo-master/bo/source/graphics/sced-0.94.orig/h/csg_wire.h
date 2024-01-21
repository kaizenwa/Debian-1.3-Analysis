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

#define PATCHLEVEL 0
/*
**	sced: A Constraint Based Object Scene Editor
**
**	csg_wireframe.h: private header for csg_wireframe.c and associated files.
**
*/

#ifndef _CSG_WIRE_
#define _CSG_WIRE_

#include <kd_tree.h>

typedef enum _VertexStatus {
	vertex_unknown,
	vertex_inside,
	vertex_outside,
	vertex_boundary
	} VertexStatus;

typedef enum _SegmentStatus {
	segment_vertex,
	segment_edge,
	segment_face
	} SegmentStatus;

typedef struct _CSGVertex {
	Vector			location;
	short			num_adjacent;
	short			max_num_adjacent;
	int				*adjacent;
	VertexStatus	status;
	} CSGVertex, *CSGVertexPtr;

typedef struct _CSGPlane {
	Vector	p_vector;
	Vector	p_point;
	} CSGPlane, *CSGPlanePtr;

typedef struct _CSGFace {
	short			face_num_vertices;
	int				*face_vertices;
	Cuboid			face_extent;
	CSGPlane		face_plane;
	AttributePtr	face_attribs;	/* A pointer back to the attributes
									** for the face.
									** Required for radiance export and
									** scanline previewing
									*/
	Boolean			ignore;			/* TRUE if the face is to be removed.	*/
	short			num_intersected;		/* Count of face intersections. */
	short			max_num_intersected;	/* Max number.					*/
	int				*intersected;			/* The intersected faces.		*/
	} CSGFace, *CSGFacePtr;

typedef struct _CSGWireframe {
	int				num_vertices;
	int				max_vertices;
	CSGVertexPtr	vertices;
	KDTree			vertex_tree;
	int				num_faces;
	CSGFacePtr		faces;
	Cuboid			obj_extent;
	} CSGWireframe, *CSGWireframePtr;

typedef struct _CSGSegment {
	double			start_distance;
	int				start_vertex;
	Vector			start_point;
	SegmentStatus	start_status;
	Boolean			start_was_vert;
	SegmentStatus	middle_status;
	double			end_distance;
	int				end_vertex;
	Vector			end_point;
	SegmentStatus	end_status;
	Boolean			end_was_vert;
	} CSGSegment, *CSGSegmentPtr;


/* Flags for split routines and other reasons. */
#define CSG_OK			0
#define CSG_REMOVED		1
#define CSG_OVERFLOW	2
#define CSG_ERROR		4

typedef int (*PolygonSplitFunction)(CSGWireframePtr, short, CSGSegment*);

/* Redefine a looser EPSILION */
#define CSG_EPSILON 1.e-8
#define CSG_EPSILON_P1 1.00000001
#define CSG_EPSILON_M1 0.99999999
#define CSG_EPSILON_SQ 1.e-16

#define CSGIsZero(a)	( (a) < CSG_EPSILON && (a) > -CSG_EPSILON )
#define CSGDEqual(a, b)	( (a) < (b) + CSG_EPSILON && (a) > (b) - CSG_EPSILON )

#define CSGVNearZero(a)		( CSGIsZero((a).x) && \
							  CSGIsZero((a).y) && \
							  CSGIsZero((a).z) )

#define CSGVZero(a)		( CSGVNearZero(a) && VDot((a), (a)) < CSG_EPSILON_SQ )

#define CSGVEqual(a, b, d)	( VSub((a), (b), (d)), \
							  CSGVNearZero(d) && CSGVZero(d) )


#define CSG_WIRE_DEBUG 0
#define CSG_TIME 0

extern void	CSG_Popup_Progress(char*);
extern void	CSG_Popdown_Progress();
extern void	CSG_Set_Progress(char*);

extern Boolean	CoPlanar(CSGPlanePtr, CSGPlanePtr);
extern void	CSG_Bounding_Box(CSGVertexPtr, short, Cuboid*);
extern void	CSG_Face_Bounding_Box(CSGVertexPtr, int*, short, Cuboid*);
extern void	CSG_Destroy_Wireframe(CSGWireframePtr);

extern CSGWireframePtr	CSG_Generic_Wireframe(ObjectInstancePtr, int);
extern void				CSG_Build_KD_Tree(CSGWireframePtr);
extern CSGWireframePtr	CSG_Copy_Wireframe(CSGWireframePtr);

extern CSGWireframe*	CSG_Combine_Wireframes(CSGWireframePtr,
							CSGWireframePtr, CSGOperation);
extern int				CSG_Add_KD_Compare_Func(void*, void*, void*, int);

extern WireframePtr	CSG_Generate_Full_Wireframe(CSGNodePtr, int);

extern Boolean	CSG_Intersect_Polygons(CSGFacePtr, CSGVertexPtr, CSGFacePtr,
									   CSGVertexPtr, CSGSegmentPtr);
extern Boolean	Extents_Intersect(Cuboid*, Cuboid*);

extern int	CSG_Vert_Vert_Vert_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Vert_Edge_Vert_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Vert_Edge_Edge_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Vert_Face_Vert_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Vert_Face_Edge_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Vert_Face_Face_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Edge_Edge_Edge_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Edge_Face_Edge_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Edge_Face_Face_Split(CSGWireframePtr, short, CSGSegmentPtr);
extern int	CSG_Face_Face_Face_Split(CSGWireframePtr, short, CSGSegmentPtr);

#endif
