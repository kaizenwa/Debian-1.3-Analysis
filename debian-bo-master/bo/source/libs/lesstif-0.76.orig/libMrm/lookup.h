/**
 *
 * $Id: lookup.h,v 1.4 1996/11/07 23:02:47 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/ 

#ifndef _LOOKUP_H
#define _LOOKUP_H

#ifdef __cplusplus
extern "C" {
#endif

/* Define the type 'CreateFunction' to be a pointer to a function
// that returns a 'Widget'
*/


void GetArgString(int index, char **theString, char *OnCreate);
char *LookUpFunction(char *FunctionName);
int LookUpPredefines(char *, long *);
int LookUpArgIndex(char *ArgName);
int LookUpClassIndex(char *ClassName);
char *ArgNamesString(char *ArgName);
int ReadBitmapFileData(char *Name, unsigned int *width, unsigned int *height, 
		   char **data, int *x_hot, int *y_hot);

enum { C = 1, G, CG, S, CS, SG, CSG };

enum {LIST_INDEX, XM_LIST_INDEX};

#ifdef __cplusplus
}
#endif

#endif



