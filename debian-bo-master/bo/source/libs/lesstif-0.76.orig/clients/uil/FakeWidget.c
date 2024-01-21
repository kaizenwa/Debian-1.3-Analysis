/**
 *
 * $Id: FakeWidget.c,v 1.2 1996/11/07 23:02:35 miers Exp $
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
#include "lookup.h"
#include "FakeWidget.h"

static int UniqueID;

FakeWidgetList *FakeWidgetListNew(FakeWidgetList *this) 
{
  if (NULL == this)
    this = (FakeWidgetList *) malloc(sizeof(FakeWidgetList));
  this->theList = NULL;
  this->size = 0;
  return this;
}

  
int FakeWidgetListMaxAttributes(FakeWidgetList *wl) 
{
  int MaxCount = 0;
  int size;
  FakeWidgetListElement *i;
  
  for (i = wl->theList; i != NULL; i = i->Next) {
    if (i->theWidget->Attributes) {
      size = i->theWidget->Attributes->size;
      MaxCount = (size > MaxCount)?size:MaxCount; 
    }
  }
  return MaxCount;
}

void FakeWidgetListEmitList(FakeWidgetList *wl)
{
  FakeWidgetListElement *i;
  int AttributeCount = FakeWidgetListMaxAttributes(wl);

  fwrite(&wl->size, sizeof(int), 1, stdout);
  fwrite(&AttributeCount, sizeof(AttributeCount), 1, stdout);
  for (i = wl->theList; i != NULL; i = i->Next) {
    FakeWidgetTypeEmitName(i->theWidget);
    putchar('"');
  }
  putchar(0);
}

FakeWidgetList *FakeWidgetTypeInheritance(FakeWidgetType *wid) 
{
  if (NULL == wid->Inherits)
    wid->Inherits = FakeWidgetListNew(NULL);
  return wid->Inherits;
}

FakeWidgetList *FakeWidgetTypeKids(FakeWidgetType *this) {
  if (NULL == this->children)
    this->children = FakeWidgetListNew(NULL);
  return this->children;
}

CallbackList *FakeWidgetTypeCalls(FakeWidgetType *this) {
  if (NULL == this->Callbacks)
    this->Callbacks = CallbackListNew();
  return this->Callbacks;
}

AttributeList *FakeWidgetTypeAtts(FakeWidgetType *this) {
  if (NULL == this->Attributes)
    this->Attributes =  AttributeListNew();
  return this->Attributes;
}


FakeWidgetType *FakeWidgetTypeNew() 
{
  FakeWidgetType *this = (FakeWidgetType *) malloc(sizeof(FakeWidgetType));
  this->theName = NULL;
  this->theClass = NULL;
  this->managed = 1;
  this->children = NULL;
  this->parent = NULL;
  this->Attributes = NULL;
  this->Callbacks = NULL;
  this->Inherits = NULL;
  this->IDNumber = 0;
  return this;
}

FakeWidgetType *FakeWidgetTypeNew1(char *newName, char *newClass) 
{
  FakeWidgetType *this = (FakeWidgetType *) malloc(sizeof(FakeWidgetType));
  this->theName = Store(newName);
  this->theClass = Store(newClass);
  this->managed = 1;
  this->children = NULL;
  this->parent = NULL;
  this->Attributes = NULL;
  this->Callbacks = NULL;
  this->Inherits = NULL;
  this->IDNumber = 0;
  return this;
}

FakeWidgetType *FakeWidgetTypeNew2(char *newName, char *newClass, Bool newManaged) 
{
  FakeWidgetType *this = (FakeWidgetType *) malloc(sizeof(FakeWidgetType));
  this->theName = Store(newName);
  this->theClass = Store(newClass);
  this->managed = newManaged;
  this->children = NULL;
  this->parent = NULL;
  this->Attributes = NULL;
  this->Callbacks = NULL;
  this->Inherits = NULL;
  this->IDNumber = 0;
  return this;
}

void FakeWidgetTypeUnmanage(FakeWidgetType *this) {
  this->managed = 0;
}

void FakeWidgetTypeManage(FakeWidgetType *this) {
  this->managed = 1;
}
  
void FakeWidgetTypeSetParent(FakeWidgetType *this,FakeWidgetType *Parent) {
  this->parent = Parent;
}

Bool Equal_FWT_FWT( FakeWidgetType *a,  FakeWidgetType *b)
{
  return (strcmp(a->theName,b->theName) == 0) && 
    (strcmp(a->theClass, b->theClass) == 0);
}

void FakeWidgetTypeEmitName (FakeWidgetType *this)
{
  fwrite(this->theName, 1, strlen(this->theName), stdout);
}

void FakeWidgetListEmit(FakeWidgetList *this)
{
  FakeWidgetListElement *i;

  for (i = this->theList; i != NULL; i = i->Next)
      FakeWidgetTypeEmit(i->theWidget);
}

void FakeWidgetListEmitID(FakeWidgetList *this)
{
  FakeWidgetListElement *i;

  for ( i = this->theList; i != NULL; i = i->Next)
    FakeWidgetTypeEmitID(i->theWidget);
}

FakeWidgetListElement *FakeWidgetListElementNew(FakeWidgetType *wid)
{
  FakeWidgetListElement *this = (FakeWidgetListElement *)
    malloc(sizeof(FakeWidgetListElement));
  
  this->Next = NULL;
  this->theWidget = wid;
  return this;
}

void FakeWidgetListAppend(FakeWidgetList *this, FakeWidgetType *that)
{
  FakeWidgetListElement **i;
  for ( i = &(this->theList); *i != NULL; i = &((*i)->Next));
  *i = FakeWidgetListElementNew(that);
  this->size++;
}
    
void FakeWidgetListAppendLists(FakeWidgetList *this, FakeWidgetList *that)
{
  FakeWidgetListElement **i;
  for ( i = &(this->theList); *i != NULL; i = &((*i)->Next));
  *i = that->theList;
  this->size += that->size;
}

void FakeWidgetListElementDistroy(FakeWidgetListElement *this)
{
  free(this);
}

Bool Equal_FLE_FLE(FakeWidgetListElement *a, FakeWidgetListElement *b)
{
  return (strcmp(a->theWidget->theName, b->theWidget->theName) == 0);
}

FakeWidgetListElement *FakeWidgetListErase(FakeWidgetList *this, 
					   FakeWidgetListElement *del)
{
  FakeWidgetListElement **j, **prev, *Return, *Destroy;
  Bool equal = False;

  for (prev = j = &(this->theList); 
       (*j != NULL) && !(equal = Equal_FLE_FLE(*j,del));
       j = &((*j)->Next))
    prev = j;
  if (equal) {
    Destroy = *j;
    *prev = (*j)->Next;
    Return = *prev;
    FakeWidgetListElementDistroy(*j);
    this->size--;
    return Return;
  }
  Exit(LOC, "Couldn't find widget to delete\n");
  return NULL;
}
    
void FakeWidgetListIndex(FakeWidgetList *this)
{
  int index = 1;
  FakeWidgetListElement *i;

  for (i = this->theList; i != NULL; i = i->Next) {
    i->theWidget->IDNumber = index++;
  }
  this->WidgetCount = index - 1;
}

void FakeWidgetListUpdate(FakeWidgetList *this,FakeWidgetType **FakeWidget)
{
  FakeWidgetListElement *i, *i1;
  Bool Found;
  char *InheritedsName;
  FakeWidgetList *Inheritance = FakeWidgetTypeInheritance(*FakeWidget);

  AttributeList *Attributes = FakeWidgetTypeAtts(*FakeWidget);
  ExpressionElement *j;
  
  CallbackList *CallBacks = FakeWidgetTypeCalls(*FakeWidget);
  CallbackListElement *k;

  if (strcmp((*FakeWidget)->theClass, "XmPulldownMenu") == 0)
    (*FakeWidget)->managed = False;

  for (j = Attributes->theExpressionList->theList; j != NULL; )
    if (ExpressionElementIsType(j, MrmRtypeCountedVector)) {
      Found = False;
      InheritedsName = ExpressionElementGetName(j);
      for (i = this->theList; i != NULL; i = i->Next) {
	if (strcmp(i->theWidget->theName,InheritedsName) == 0) {
	  Found = True;
	  FakeWidgetListAppend(Inheritance, i->theWidget);
	  break;
	}
      }
      if (!Found)
	Exit(LOC, "Can't find %s in order to inherit it\n",
	     ExpressionElementGetName(j));
      j = AttributeListErase(Attributes, j);
    }
    else {
      j = j->Next;
    }
  for (k = CallBacks->theList;k != NULL;)
    if (CallbackListElementIsInherited(k)) {
      Found = False;
      InheritedsName = CallbackListElementGetFunctionName(k);
      for (i = this->theList; i != NULL; i = i->Next) {
	if (strcmp(i->theWidget->theName,InheritedsName) == 0) {
	  Found = True;
	  FakeWidgetListAppend(Inheritance, i->theWidget);
	  break;
	}
      }
      if (!Found)
	Exit(LOC, "Can't find %s in order to inherit it\n",InheritedsName);
      k = CallbackListErase(CallBacks,k);
    }
    else
      k = k->Next;

  if ((*FakeWidget)->children)
    for (i1 =(*FakeWidget)->children->theList; i1 != NULL;)
      if (strcmp(i1->theWidget->theClass, "Inherited") == 0) {
	Found = False;
	InheritedsName = i1->theWidget->theName;
	for (i = this->theList; i != NULL; i = i->Next) {
if (NULL == i->theWidget->theName)
fprintf(stderr,"NULL\n");
	  if (strcmp(i->theWidget->theName, InheritedsName) == 0) {
	    FakeWidgetList *Inheritance=FakeWidgetTypeInheritance(*FakeWidget);
	    
	    Found = True;
	    FakeWidgetListAppend(Inheritance, i->theWidget);
	    break;
	  }
	}
	if (!Found)
	  Exit(LOC, "Can't find %s in order to inherit it\n",
	       i->theWidget->theName);
	i1 = FakeWidgetListErase((*FakeWidget)->children, i1);
      }
      else {
	i1 = i1->Next;
      }
  
  
  for (i = this->theList; i != NULL; i = i->Next) {
    if (Equal_FWT_FWT(i->theWidget,*FakeWidget)){
      FakeWidgetTypeUpdate(i->theWidget, *FakeWidget);
      *FakeWidget = i->theWidget;
      return;
    }
  }
  FakeWidgetListAppend(this, *FakeWidget);
}

void FakeWidgetListSetParents(FakeWidgetList *this,FakeWidgetType *Parent)
{
  FakeWidgetListElement *i;

  for ( i = this->theList;i != NULL; i = i->Next) 
    FakeWidgetTypeSetParent(i->theWidget, Parent);
  
}

void FakeWidgetTypeSetName(FakeWidgetType *this, char *name, char *newType)
{
  if ((NULL == name) && (NULL == this->theName)) {
    char tempName[10];
    
    sprintf(tempName,"temp%d", UniqueID++);
    this->theName = Store(tempName);
    this->theClass = newType;
  }
  else if (name){
    this->theName = name;
  }
  if (newType)
    this->theClass = Store(newType);
}

void FakeWidgetTypeUpdate(FakeWidgetType *this, FakeWidgetType *a)
{
  /* Inheritance */
  if (NULL != a->Inherits) {
    if (NULL == this->Inherits)
      this->Inherits =  FakeWidgetListNew(NULL);
    FakeWidgetListAppendLists(this->Inherits, a->Inherits);
  }
  /* Concat children */
  if (NULL != a->children) {
    if (NULL == this->children)
      this->children =  FakeWidgetListNew(NULL);
    FakeWidgetListAppendLists(this->children, a->children);
  }
  /* Concat Attributes */
  if (NULL != a->Attributes){
    if (NULL == this->Attributes)
      this->Attributes =  AttributeListNew();
    AttributeListAppendLists(this->Attributes, a->Attributes);
  }
  /* Concat Callbacks */
  if (NULL != a->Callbacks) {
    if (NULL == this->Callbacks)
      this->Callbacks =  CallbackListNew();
    CallbackListAppendLists(this->Callbacks, a->Callbacks);
  }
}

void FakeWidgetTypeEmitID(FakeWidgetType *this )
{
  if (this)
    fwrite(&this->IDNumber,sizeof(this->IDNumber),1,stdout);
}

void FakeWidgetTypeEmit(FakeWidgetType *this)
{
  int zero = 0;
  int Class;

  if (this) {
    putchar((unsigned char )MrmRtypeClassRecName);
    fwrite(this->theName, strlen(this->theName), 1, stdout);
    putchar('"');
    fwrite(&this->IDNumber, sizeof(this->IDNumber), 1, stdout);
    Class = LookUpClassIndex(this->theClass);
    fwrite(&Class, sizeof(Class), 1, stdout);
    putchar(this->managed);
    if (this->parent)
      fwrite(&this->parent->IDNumber,sizeof(this->IDNumber), 1, stdout);
    else
      fwrite(&zero, sizeof(zero), 1, stdout);

    if (this->Inherits)
      FakeWidgetListEmitID(this->Inherits);
    fwrite(&zero, sizeof(zero), 1, stdout);
    if (this->children)
      FakeWidgetListEmitID(this->children);
    fwrite(&zero, sizeof(zero), 1, stdout);
    if (this->Callbacks)
      CallbackListEmit(this->Callbacks, this->IDNumber);
    if (this->Attributes)
      AttributeListEmit(this->Attributes);
  }
}

void FakeWidgetListPrint(FakeWidgetList *this)
{
  FakeWidgetListElement *i;

  fprintf(stderr,"THE LIST:  ");
  for (i = this->theList; i != NULL; i = i->Next) {
    if (NULL == i->theWidget->theName) {
      fprintf(stderr,"NOT OK\n");
      return;
    }
  }
  fprintf(stderr,"OK\n");
}
