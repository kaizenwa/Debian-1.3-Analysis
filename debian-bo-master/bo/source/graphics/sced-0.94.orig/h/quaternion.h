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
**	quaternion.h: Small header file for quaternion related stuff.
*/

typedef struct _Quaternion {
	Vector	vect_part;
	double	real_part;
	} Quaternion;

extern Matrix		Quaternion_To_Matrix(Quaternion);
extern Quaternion	Build_Rotate_Quaternion(Vector, double);
extern Quaternion	QQMul(Quaternion*, Quaternion*);
extern void			Quaternion_To_Axis_Angle(Quaternion*, Vector*, double*);
