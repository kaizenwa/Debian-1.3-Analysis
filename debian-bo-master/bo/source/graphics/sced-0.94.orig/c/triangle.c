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

#include <math.h>
#include <sced.h>
#include <add_constraint.h>
#include <triangle.h>

void
Triangle_Create_Function(ObjectInstancePtr obj)
{
	Create_Generic_Object(obj);

	obj->o_hook = (void*)New(ControlHook, 1);
	control_part(obj)->num_control_verts = 3;
	control_part(obj)->control_verts = New(Vector, 3);
	VNew(0, 0, 0, control_part(obj)->control_verts[0]);
	VNew(0.5, M_SQRT1_2, 0, control_part(obj)->control_verts[1]);
	VNew(1, 0, 0, control_part(obj)->control_verts[2]);

	/* Create the 3 extra features. */
	obj->o_num_features += 4;
	obj->o_features = More(obj->o_features, Feature, obj->o_num_features);

	Feature_Create_Dummy_Constraints(obj->o_features + tri_dummy_feature);
	Feature_Create_Control_Constraints(obj->o_features + tri0_feature, obj, 0);
	Feature_Create_Control_Constraints(obj->o_features + tri1_feature, obj, 1);
	Feature_Create_Control_Constraints(obj->o_features + tri2_feature, obj, 2);

	obj->o_static_func = Maintain_Control_Obj_Static;
	obj->o_dynamic_func = Maintain_Control_Obj_Dynamic;
}

void
Triangle_Destroy_Function(ObjectInstancePtr victim)
{
	free(control_part(victim)->control_verts);
	free(victim->o_hook);
	Destroy_Generic_Object(victim);
}

void
Triangle_Copy_Hook(ObjectInstancePtr src, ObjectInstancePtr dest)
{
	int	i;

	ControlHookPtr	src_hook = (ControlHookPtr)src->o_hook;
	ControlHookPtr	dest_hook = (ControlHookPtr)dest->o_hook;

	for ( i = 0 ; i < 3 ; i++ )
		dest_hook->control_verts[i] = src_hook->control_verts[i];
}

void
Triangle_Calculate_Vertices(ObjectInstancePtr obj, VectorPtr verts)
{
	verts[0] = control_part(obj)->control_verts[0];
	verts[1] = control_part(obj)->control_verts[1];
	verts[2] = control_part(obj)->control_verts[2];
	VNew(0, 0, 0, verts[3]);
}


void
Triangle_Calculate_Normal(ObjectInstancePtr obj, VectorPtr normal)
{
	Vector	temp_v1, temp_v2;
	double	temp_d;

	VSub(control_part(obj)->control_verts[2],
		 control_part(obj)->control_verts[0], temp_v1);
	VSub(control_part(obj)->control_verts[1], 
		 control_part(obj)->control_verts[0], temp_v2);
	VCross(temp_v1, temp_v2, *normal);
	if ( VZero(*normal) )
		VNew(1, 0, 0, *normal);
	else
		VUnit(*normal, temp_d, *normal);
}

