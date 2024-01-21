/**
 *
 * $Id: glue.h,v 1.2 1996/11/12 03:53:57 miers Exp $
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

#include "misc.h"

extern void SaveTop(char *);
extern char *body_OBJECT_object(char *,char *);
extern void features_arguments(char *,char *);
extern char *Features_NULL();
extern char *controllist_controllist_ID_ID(char *, char *, char *, char);
extern char *control_list_ID_features(char *, char *, char *, char );
extern void ID_ID_features(char *, char *, char *);
extern void features_controls(char * , char * );
extern void features_callbacks(char * , char * );
extern char *Store(char *);
extern char *arglist_arglist_ID_addexpr(char *, char *, char *);
extern char *expr_STRING(char *, int );
extern char *prim_exp(char *);
extern char *expr_BOOL(char *);
extern char *keysym(char *s);
extern char *font(char *s);
extern char *bitmap(char *s);
extern char *pixmap(char *n,char *s);
extern char *expr_ID(char *);
extern char *compoundstring_csSTRING(char *,char *,char *);
extern char *compoundstring_csCS(char *,char *,char *);
extern char *callbacklist_callbacklist_PROCID_arglist(char *, char *, char *, char *);
char *InheritArgument(char *, char *);
char *InheritControls(char *, char *);
char *InheritCallback(char *);
void AddControlList(char *name, char *c);
void AddCallbackList(char *name, char *c);
void AddAttributeList(char *, char *);
char *InsertString(char *, char *);
void MakeTable(char *, char *, int );
char *AppendStrings(char *, char *);
char *Add(char *, char *);
char *Subtract(char *, char *);
char *Multiply(char *, char *);
char *Divide(char *, char *);
char *WidgetArgument(char *, char *, char *);
char *Parameter(char *, char *);
char *color(char *);
char *AddColor(char *, char *, char *, int);
void MakeColorTable(char *);

extern int yyparse(void);

char *string_push(char *);
void string_clear();




