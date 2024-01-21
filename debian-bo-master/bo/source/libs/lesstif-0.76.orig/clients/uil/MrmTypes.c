/**
 *
 * $Id: MrmTypes.c,v 1.4 1996/11/12 03:53:53 miers Exp $
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
#include "FakeWidget.h"
#include "glue.h"
#include "uil.h"
#include <stdlib.h>
#include <ctype.h>
#include <X11/Xlib.h>
#include "MrmTypes.h"

extern ExpressionList GlobalSymbolTable;
extern ExpressionList LocalSymbolTable;

Char8 *Char8Add( Char8 *s1,  Char8 *s2)
{
  Char8 *Return = s1;
  strcat(Return->lvalue,s2->lvalue);
  Return->theExpression.value = (long) Return->lvalue;
  return Return;
}

int AddrNameGetEvalValue(AddrName *this) {
  ExpressionElement *t = ExpressionListFind(&LocalSymbolTable,
					 (char *)(this->theExpression.value));
  if (NULL == t)
    t = ExpressionListFind(&GlobalSymbolTable,(char *)this->theExpression.value);
  if (NULL == t)
    Exit(LOC, "Can't find %s in Symbol Tables\n",this->theExpression.value);
  if (!ExpressionElementIsType(t,MrmRtypeInteger))
    yyerror("Illegal type in expression");
  return (int)ExpressionElementGetValue(t);
}

void InheritItemEmit(InheritItem *this) {
  putchar(this->theExpression.type);
  fwrite((char *) this->theExpression.value, 1, 
	 strlen((char *) this->theExpression.value), stdout);
  putchar(0);
}

InheritItem *InheritItemNew(char *s)
{
  InheritItem *this = (InheritItem *)malloc(sizeof(InheritItem));

  strcpy(this->lvalue,s);
  this->theExpression.value = (long)this->lvalue;
  this->theExpression.type = MrmRtypeCountedVector;
  this->theExpression.Emit = (PFI ) InheritItemEmit;
  return this;
}

FontM *FontMNew(char *font)
{
  FontM *this = (FontM *)malloc(sizeof(FontM));
  this->theExpression.value = (long)this->lvalue;
  strcpy(this->lvalue, font);
  this->theExpression.type = MrmRtypeFont;
  this->theExpression.Emit = (PFI ) FontMEmit;
  return this;
}

void FontMEmit(FontM *this)
{
  putchar(this->theExpression.type);
  fwrite(this->lvalue, 1, strlen(this->lvalue), stdout);
  putchar(0);
}

Color *ColorNew(char *color)
{
  Color *this = (Color *)malloc(sizeof(Color));
  strcpy(this->lvalue,color);
  this->theExpression.value = (long)this->lvalue;
  this->theExpression.type = MrmRtypeColor;
  this->theExpression.Emit = (PFI ) ColorEmit;
  return this;
}

void ColorSetAddress(Color *this) 
{
  this->theExpression.type = MrmRtypeAddrName;
}

void ColorEmit(Color *this)
{
  putchar(this->theExpression.type);
  fwrite(this->lvalue, 1, strlen(this->lvalue), stdout);
  putchar(0);
}

XBitmapFile *XBitmapFileNew(char *FileName) 
{
  XBitmapFile *this = (XBitmapFile *)malloc(sizeof(XBitmapFile ));
  this->theExpression.type = MrmRtypeXBitmapFile;
  this->theExpression.value = (long) &this->bitmap;
  ReadBitmapFileData(FileName, &(this->bitmap.width), &(this->bitmap.height), 
		     &(this->bitmap.data), &(this->bitmap.x_hot), 
		     &(this->bitmap.y_hot));
  this->theExpression.Emit = (PFI )XBitmapFileEmit;
  return this;
}
  
void XBitmapFileEmit(XBitmapFile *this) {
  putchar(this->theExpression.type);
  fwrite((char *)this->theExpression.value, 
	 sizeof(BitMapType) - sizeof(long), 1, stdout);
  fwrite(this->bitmap.data, 
	 (this->bitmap.width * this->bitmap.height) >> 3, 1, stdout);
}

Keysym *KeysymNew(char *s)
{
  Keysym *this = (Keysym *)malloc(sizeof(Keysym ));
  strcpy(this->lvalue,s);
  this->theExpression.value = (long)this->lvalue;
  this->theExpression.type = MrmRtypeKeysym;
  this->theExpression.Emit = (PFI ) KeysymEmit;
  return this;
}
  
void KeysymEmit(Keysym *this) 
{
  putchar(this->theExpression.type);
  fwrite(this->lvalue, 1, strlen(this->lvalue), stdout);
  putchar(0);
}

ColorElement *ColorElementNew(char *rep, Color *color)
{
  ColorElement *this = (ColorElement *) malloc(sizeof(ColorElement));
  this->Next = NULL;
  this->name = Store(rep);
  this->theColor = color;
  return this;
}

void ColorElementEmit(ColorElement *this) 
{
  fputs(this->name, stdout);
  putchar(0);
  ColorEmit(this->theColor);
}

ColorTable *ColorTableNew()
{
  ColorTable *this = (ColorTable *)malloc(sizeof(ColorTable));
  this->theExpression.type = MrmRtypeColorTable;
  this->theExpression.value = (long) this->ColorVector;
  this->theExpression.Emit = (PFI ) ColorTableEmit;
  return this;
}
  
void ColorTableAppend(ColorTable *this, char *Representation, Color *color) 
{
  ColorElement **i;

  for ( i = &this->ColorVector; *i != NULL; i = &((*i)->Next));
  *i = ColorElementNew(Representation,color);
}

void ColorTableEmit(ColorTable *this) 
{
  ColorElement **j;
  int size = 0;
  
  putchar(this->theExpression.type);
  for (j = &this->ColorVector; *j != NULL; j = &((*j)->Next)) 
    size++;
  fwrite(&size, sizeof(size), 1, stdout);
  for (j = &this->ColorVector; *j != NULL; j = &((*j)->Next)) {
    ColorElementEmit(*j);
  };
}

Char8Vector *Char8VectorNew()
{
  Char8Vector *this = (Char8Vector *)malloc(sizeof(Char8Vector));
  this->theExpression.type = MrmRtypeChar8Vector;
  this->theExpression.value = (long) &this->CharVector;
  this->theExpression.Emit = (PFI ) Char8VectorEmit;
  return this;
}

Char8Element *Char8ElementNew(char *s)
{
  Char8Element *this = (Char8Element *)malloc(sizeof(Char8Element));
  this->Next = NULL;
  strcpy(this->lvalue, s);
  return this;
}

void Char8VectorAppend(Char8Vector *this, char *s) 
{
  Char8Element **j;
  
  for (j = &this->CharVector; *j != NULL; j = &((*j)->Next));
  *j = Char8ElementNew(s);
}

void Char8VectorEmit(Char8Vector *this) 
{
  Char8Element **j;
  
  putchar(this->theExpression.type);
  for (j = &this->CharVector; *j != NULL; j = &((*j)->Next)) {
    fputs((*j)->lvalue, stdout) ;
    putchar('"');
  };
  putchar(0);
}

BooleanM *BooleanMNew(Bool i)
{
  BooleanM *this = (BooleanM *)malloc(sizeof(BooleanM));
  this->theExpression.value = i;
  this->theExpression.type = MrmRtypeBoolean;
  this->theExpression.Emit = (PFI ) BooleanMEmit;
  return this;
}
  
void BooleanMEmit(BooleanM *this) 
{
  putchar(this->theExpression.type);
  fwrite(&this->theExpression.value,sizeof(long), 1, stdout);
}

Integer *IntegerNew(int i)
{
  Integer *this = (Integer *)malloc(sizeof(Integer));
  
  this->theExpression.value = (long)i;
  this->theExpression.type = MrmRtypeInteger;
  this->theExpression.Emit = (PFI ) IntegerEmit;
  return this;
}

void IntegerEmit(Integer *this) 
{
  putchar(this->theExpression.type);
  fwrite(&this->theExpression.value,sizeof(long), 1, stdout);
}

int IntegerGetEvalValue(Integer *this) 
{
  return ((int)this->theExpression.value);
}

AddrName *AddrNameNew(char *s)
{
  AddrName *this = (AddrName *)malloc(sizeof(AddrName));
  strcpy(this->lvalue,s);
  this->theExpression.value = (long)this->lvalue;
  this->theExpression.type = MrmRtypeAddrName;
  this->theExpression.Emit = (PFI ) AddrNameEmit;
  return this;
}
  
void AddrNameEmit(AddrName *this) 
{
  putchar(this->theExpression.type);
  fputs((char *) this->theExpression.value,stdout);
  putchar(0);
}

CString *CStringNew(char *s) 
{
  CString *this = (CString *)malloc(sizeof(CString));
  this->theExpression.type = MrmRtypeCString;
  strcpy(this->lvalue,s);
  this->theExpression.value = (long) this->lvalue;
  this->theExpression.Emit = (PFI ) CStringEmit;
  return this;
}

char *CStringGetStringValue(CString *this) {
  return this->lvalue;
}
      
CString *CStringAdd(CString *this, CString *s) 
{
  strcat(this->lvalue,CStringGetStringValue(s));
  strcat(this->lvalue,"C");
  this->theExpression.value = (long) this->lvalue;
  return this;
}

void CStringEmit(CString *this) 
{
  putchar(this->theExpression.type);
  fputs((char *) this->theExpression.value,stdout);
  putchar(0);
}

Char8 *Char8New(char *s) 
{
  Char8 *this = (Char8 *) malloc(sizeof(Char8));
  char *q = s;
  char *q1 = s;
  
  while (*q)
    if ('\\' == *q) { 
      q++;
      switch (*q) {
      case 'a':
	*q1++ = '\a';
	break;
      case 'n':
	*q1++ = '\n';
	break;
      case 'r':
	*q1++ = '\r';
	break;
      case 't':
	*q1++ = '\t';
	break;
      case '\\':
	*q1++ = '\\';
	break;
      default:
	*q1++ = *q;
	break;
      }
      q++;
    }
    else
      *q1++ = *q++;
  *q1 = '\0';
  this->theExpression.type = MrmRtypeChar8;
  strcpy(this->lvalue,s);
  this->theExpression.value = (long) this->lvalue;
  this->theExpression.Emit = (PFI ) Char8Emit;
  return this;
}

void Char8Emit(Char8 *this) 
{
  putchar(this->theExpression.type);
  fputs((char *)this->theExpression.value,stdout);
  putchar(0);
}

PixmapImage *PixmapImageNew(char *ColorMap, Char8Vector *pixmap) 
{
  Char8Element **i;
  PixmapImage *this = (PixmapImage *)malloc(sizeof(PixmapImage));
  this->theExpression.type = MrmRtypePixmapImage;
  this->thePixmap.height = 0;
  this->theData = NULL;
  if (ColorMap)
    this->theColorMap = Store(ColorMap);
  else
    this->theColorMap = NULL;
  this->thePixmap.width = strlen(pixmap->CharVector->lvalue);
  for (i = &pixmap->CharVector; *i != NULL; i = &((*i)->Next)) {
    this->thePixmap.height += 1;
  }
  this->theData =(char *)malloc(this->thePixmap.height*this->thePixmap.width);
  for (i = &pixmap->CharVector; *i != NULL; i = &((*i)->Next)) {
    strcat(this->theData,(*i)->lvalue);
  }
  this->thePixmap.data = this->theData;
  this->theExpression.value = (long) &this->thePixmap;
  this->theExpression.Emit = (PFI ) PixmapImageEmit;
  return this;
}

void PixmapImageEmit(PixmapImage *this) 
{
  putchar(this->theExpression.type);
  fwrite((char *)this->theExpression.value, 1, sizeof(int)*2, stdout);
  if (this->theColorMap)
    fputs(this->theColorMap, stdout);
  putchar(0);
  fwrite(this->thePixmap.data, 1, 
	 this->thePixmap.width * this->thePixmap.height, stdout);
}






