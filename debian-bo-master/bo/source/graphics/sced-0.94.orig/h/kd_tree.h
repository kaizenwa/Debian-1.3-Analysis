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
**	kd_tree.h: header for kd-tree types and manipulation functions.
*/

#ifndef __KD_TREE__
#define __KD_TREE__

/* The tree elements themselves.  */
typedef struct _KDTreeElmt {
	 void				*value;
	 struct _KDTreeElmt	*left;
	 struct _KDTreeElmt	*right;
	 } KDTreeElmt, *KDTree;

/* The comparison function type. */
typedef int	(*KDCompareFunc)(void*, void*, void*, int);

/* Functions. */
extern void	*KD_Add_Elmt(KDTree*, void*, void*, KDCompareFunc);
extern void	KD_Free(KDTree);
extern void	KD_Print_Tree(KDTree, int);

#endif
