/* ########################################################################

				 Type.c

   File: Type.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Type.c
   Description: 
   Created: Tue Feb 21 12:46:10 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:46:10 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */



/******************************** Les types **********************************/

#include <string.h>
#include <stdio.h>

#include "Type.h"
#include "type.h"
#include "hash.h"
#include "Instruction.h"
#include "error.h"

Type * Type__Type(subinfo, tot, size_of)
     void * subinfo;
     type_of_type tot;
     int size_of;
{
  Type * this = (Type *) Malloc(sizeof(Type));

  this->_name_or_pointed = subinfo;
  this->_type = tot;
  this->_sizeof = size_of;

  return this;
}


int Type__Equal(t1, t2)
     Type * t1;
     Type * t2;
{
  for (;;) {
    if (t1 == t2)
      return 1;

    if ((t1->_type == T_Builtin) || (t2->_type == T_Builtin))
      return 0;

    if (t1->_type == T_Function) {
      if (t2->_type == T_Function) {
	TypeFunction * tf1 = Type__Function(t1);
	TypeFunction * tf2 = Type__Function(t2);

	if ((tf1->_nbre_arg == tf2->_nbre_arg) &&
	    Type__Equal(tf1->_valtype, tf2->_valtype)) {
	  Type ** at1 = tf1->_argtypes;
	  Type ** at2 = tf2->_argtypes;
	  int index = tf1->_nbre_arg;
	
	  while (index--)
	    if (! Type__Equal(at1[index], at2[index]))
	      return 0;
	  return 1;
	}
      }
      return 0;
    }
    else if (t2->_type == T_Function)
      return 0;
    
    t1 = (Type *) t1->_name_or_pointed;
    t2 = (Type *) t2->_name_or_pointed;
  }
}


/* Ajoute le nom du type a la fin de str */

void Type__Print(t, str)
    Type * t;
    char * str;
{
  if (Type__IsBuiltin(t))
    strcat(str, Type__Name(t));
  else if (Type__IsaFunction(t)) {
    TypeFunction * tf = Type__Function(t);
    
    Type__Print(TypeFunction__ValType(tf), str);
    strcat(str, "(*)(");
    if (TypeFunction__NbreArg(tf)) {
      int i;
      
      Type__Print(TypeFunction__ArgType(tf, 0), str);
      for (i = 1; i != TypeFunction__NbreArg(tf); i += 1) {
	strcat(str, ", ");
	Type__Print(TypeFunction__ArgType(tf, i), str);
      }
    }
    strcat(str, ")");
  }
  else {
    Type__Print(Type__Pointed_Type(t), str);
    strcat(str, "*");
  }
}


/* Compatibilite de deux types

   retourne 0 si t1 ne peut etre transcrit en t2
   	    Type__Convertible si les types sont interchangeables
	    une fonction de conversion sinon */

HashTable * convert_hash_table;

ConvFct Type__Convertible(t1, t2)
     Type * t1;
     Type * t2;
{
  if (Type__Equal(t1, t2))
    return (ConvFct) Type__Convertible;

  {
    /* type * convertible en void * */
    
    Type * st1 = t1;
    Type * st2 = t2;

    while (((st1->_type ==  T_Array) || (st1->_type == T_Pointer)) &&
	   (st2->_type == T_Pointer)) {
      st1 = Type__Pointed_Type(st1);
      st2 = Type__Pointed_Type(st2);
      if (st2 == Type_Void)
	return (ConvFct) Type__Convertible;
    }
    
    if ((st1->_type == T_Function) &&
	(st2->_type == T_Pointer) &&
	(Type__Pointed_Type(st2) == Type_Void))
      return (ConvFct) Type__Convertible;
  }
  
  {
    /* Regarde s'il y a une conversion possible */
    
    char encoded[24];
    
    sprintf(encoded, "%s2", encode_type(t1));
    strcat(encoded, encode_type(t2));

    return (ConvFct) HashTable__Search(convert_hash_table, (Object) encoded);
  }
}


/* La meme chose mais avec la forme ce qui permet de tester le cas
   0, valeur pouvant etre affectee a n'importe quel type d'objet */

ConvFct ExprType__Convertible(i, t)
     Instruction * i;
     Type * t;
{
  Type * ti = GetExprType(i);
  
  if ((ti == Type_Int) && IsaConst(i) && (! Eval(i)))
    return (ConvFct) Type__Convertible;
  
  return Type__Convertible(ti, t);
}


/* Les fonctions de conversion */

char int2char(i)
     int i;
{
  return (char) i;
}

int char2int(c)
     int c;
{
  /* Cette fonction est la parce que l'on ne
     sait pas si un char est ou non signe */
  return (int) ((char) c);
}


/* Retourne un masque pour ne conserver que les bits associes au type */

Object Type__Mask(t)
     Type * t;
{
  if (Type__IsBuiltin(t))
    return   (t == Type_Int)  ? (unsigned int) ~((Object) 0)
  	   : (t == Type_Char) ? (unsigned char) ~((Object) 0)
  			      : 0;
  else
    return ~((Object) 0);
}
		    

Type * find_type(name)
     char * name;
{
  /* On ne vas tout de meme pas utiliser une hash table pour 3 types ! */

  switch (*name) {
  case 'c' : return Type_Char;
  case 'i' : return Type_Int;
  case 'v' : return Type_Void;
  default  :
    sprintf(err_msg, "%s unknown type", name);
    Internal_Error(err_msg);

    return 0;				/* pour le compilo */
  }
}


/* Sort en erreur pour incompatibilite de deux types */

void error_incompatible_types(t1, t2, msg)
    Type * t1;
    Type * t2;
    char * msg;
{
  strcpy(err_msg, msg);
  strcat(err_msg, ", ");
  Type__Print(t1, err_msg);
  strcat(err_msg, " <> ");
  Type__Print(t2, err_msg);
  
  Error(err_msg);
}


/* Les types builtins */

Type * Type_Int;
Type * Type_Char;
Type * Type_Void;

Type * Type_String;
Type * Type_PInt;


/* A appeler dans main */

void Init_type()
{
  Type_Int =  Type__Type("int", T_Builtin, sizeof(int));
  Type_Char = Type__Type("char", T_Builtin, sizeof(char));
  Type_Void = Type__Type("void", T_Builtin, sizeof(void *));

  Type_String = Type__Type(Type_Char, T_Pointer, sizeof(char *));
  Type_PInt = Type__Type(Type_Int, T_Pointer, sizeof(int *));

  /**/
  
  convert_hash_table = HashTable__HashTable(24, 1);
  
  if (sizeof(char) != sizeof(int)) {
    HashTable__Add(convert_hash_table, (Object) "int2char", (Object) int2char);
    HashTable__Add(convert_hash_table, (Object) "char2int", (Object) char2int);
  }
}
