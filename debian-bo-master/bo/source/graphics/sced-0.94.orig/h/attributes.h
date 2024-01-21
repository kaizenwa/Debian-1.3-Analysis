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
**	attributes.h: Header for attribute management stuff. Just includes and
**					declaration headers, and some functions.
*/

#ifndef __SCED_ATTRIBS__
#define __SCED_ATTRIBS__

/* The declarations. */
/* One for each renderer for both before and after the includes. */
extern char	*declarations[];

extern void	Attributes_Change_String(InstanceList, char*, Boolean, Boolean);
extern void	Change_Declarations(Renderer, char*, Boolean);
extern void	Clear_Declarations(Renderer);
extern int	Save_Declarations(FILE *outfile);
extern void	Load_Declaration(Renderer, Boolean);
extern AttributePtr	Attribute_New(AttributePtr, Boolean);
extern void	Attribute_Copy(AttributePtr, AttributePtr);
extern void	Attribute_Destroy(AttributePtr);

#endif /* __SCED_ATTRIBS__ */
