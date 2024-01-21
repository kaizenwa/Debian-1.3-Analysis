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
**	bezier.h: header for bezier patch functions.
*/

extern void	Bezier_Create_Function(ObjectInstancePtr);
extern void	Bezier_Destroy_Function(ObjectInstancePtr);
extern void	Bezier_Copy_Hook(ObjectInstancePtr, ObjectInstancePtr);
extern void	Bezier_Calculate_Vertices(ObjectInstancePtr, VectorPtr, int, int);
extern void	Bezier_Calculate_Normals(ObjectInstancePtr, VectorPtr);
extern Vector	Bezier_Calculate_Point(ObjectInstancePtr, double, double);
extern void	Bezier_Calculate_Vertex_Normals(ObjectInstancePtr, Vector*, int,
											Boolean);

extern int	bezier_map[];
