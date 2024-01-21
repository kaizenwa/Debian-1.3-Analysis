/* cfengine for GNU
 
        Copyright (C) 1995
        Free Software Foundation, Inc.
 
   This file is part of GNU cfengine - written and maintained 
   by Mark Burgess, Dept of Computing and Engineering, Oslo College,
   Dept. of Theoretical physics, University of Oslo
 
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

*/
 


/*******************************************************************/
/*                                                                 */
/*  Class string evaluation toolkit for cfengine                   */
/*                                                                 */
/*  Dependency: item.c toolkit                                     */
/*                                                                 */
/*******************************************************************/

#include "cf.defs.h"
#include "cf.extern.h"

/*********************************************************************/
/* Level 1                                                           */
/*********************************************************************/

enum classes ClassStringToCode (str)

char *str;

{ char *sp;
  int i;

Debug2(">> ClassStringToCode(%s)\n",str);

if (strcmp("any",str) == 0)
   {
   return(VSYSTEMHARDCLASS); /* always true */
   }

CLASS = empty;

/*
for (sp = str; *sp != '\0'; sp++)
   {
   *sp = ToLower(*sp);
   }
*/

for (i = 1; CLASSTEXT[i] != '\0'; i++)
   {
   if (strcmp(CLASSTEXT[i],str) == 0)
      {
      CLASS = (enum classes) i;
      break;
      }
   }

return (enum classes) i;
}


/*********************************************************************/

AddDayClass(str)

char *str;

{ int i;

for (i = 0; i < 7; i++)
   {
   if (strncmp(DAYTEXT[i],str,3)==0)
      {
      AddClassToHeap(DAYTEXT[i]);
      }
   }
}

/*******************************************************************/

AddClassToHeap(class)

char *class;

{
if (IsItemIn(VHEAP,class))
   {
   return;
   }

Debug(">>Pushing class %s on heap\n",class);

AppendItem(&VHEAP,class,NULL);
}

/*********************************************************************/

DeleteClassFromHeap(class)

char *class;

{
Debug(">>Deleting class %s from heap\n",class);

DeleteItemMatching(&VHEAP,class);
}

/*********************************************************************/

IsHardClass(sp)  /* true if string matches a hardwired class e.g. hpux */

char *sp;

{ int i;

Debug("IsHardClass(%s)",sp);

for (i = 2; CLASSTEXT[i] != '\0'; i++)
   {
   if (strcmp(CLASSTEXT[i],sp) == 0)
      {
      CLASS = (enum classes) i;
      Debug("CLASS = %s true\n",CLASSTEXT[i]);
      return(true);
      }
   }

for (i = 0; i < 7; i++)
   {
   if (strcmp(DAYTEXT[i],sp)==0)
      {
      Debug("-false\n");
      return(false);
      }
   }

Debug("-false\n");
return(false);
}

/*********************************************************************/

IsExcluded(exception)

char *exception;

{
Debug2("IsExcluded(%s)\n",exception);
 
if (! IsDefinedClass(exception))
   {
   Debug2("%s is excluded!\n",exception);
   return true;
   }  

return false;
}

/*********************************************************************/

IsDefinedClass(class) 

  /* Evaluates a.b.c|d.e.f etc and returns true if the class */
  /* is currently true, given the defined heap and negations */

char *class;

{ char *sp,vbuff[bufsize];
  int result = false;

Debug1("IsDefinedClass(%s)\n",class);

bzero(vbuff,bufsize);

for (sp = class; *sp != '\0'; sp += strlen(vbuff))
   {
   while (*sp == '|')
      {
      sp++;
      }

   sscanf(sp,"%[^|]",vbuff);

   if (strlen(vbuff) == 0)
      {
      break;
      }

   result |= EvaluateANDString(vbuff,VADDCLASSES);
   }

return result;
}


/*********************************************************************/

IsInstallable(class)

char *class;

  /* Evaluates to true if the class string COULD become true in */
  /* the course of the execution - but might not be true now    */

{ char *sp;
  int result = false;

if (DEBUG || D1)
   {
   printf("IsInstallable(%s)\n",class);
   }

VBUFF[0] = '\0';

for (sp = class; *sp != '\0'; sp+=strlen(VBUFF))
   {
   while (*sp == '|')
      {
      sp++;
      }

   sscanf(sp,"%[^|]",VBUFF);

   if (strlen(VBUFF) == 0)
      {
      break;
      }

   result |= EvaluateANDString(VBUFF,VALLADDCLASSES);
   }

return result;
}

/*********************************************************************/

AddCompoundClass(class)

char *class;

{ char *sp = class;
  char cbuff[maxvarsize];

Debug1("AddCompoundClass(%s)",class);

while(*sp != '\0')
   {
   sscanf(sp,"%[^.]",cbuff);

   while ((*sp != '\0') && (*sp !='.'))
      {
      sp++;
      }

   if (*sp == '.')
      {
      sp++;
      }

   if (IsHardClass(cbuff))
      {
      FatalError("cfengine: You cannot use -D to define a reserved class!");
      }

   AddClassToHeap(cbuff);
   }
}

/*********************************************************************/

NegateCompoundClass(class,heap)

char *class;
struct Item **heap;

{ char *sp = class;
  char cbuff[maxvarsize];

Debug1("AddCompoundClass(%s)",class);

while(*sp != '\0')
   {
   sscanf(sp,"%[^.]",cbuff);

   while ((*sp != '\0') && (*sp !='.'))
      {
      sp++;
      }

   if (*sp == '.')
      {
      sp++;
      }

   if (IsHardClass(cbuff))
      { char err[bufsize];
      yyerror("Illegal exception");
      sprintf (err,"Cannot negate the reserved class [%s]\n",cbuff);
      FatalError(err);
      }

   AppendItem(heap,cbuff,NULL);
   }
}

/*********************************************************************/
/* Level 2                                                           */
/*********************************************************************/

EvaluateANDString(class,addclasses)

char *class;
struct Item *addclasses;

{ char *sp, *simpleid;
  char cbuff[maxvarsize];
  int count = 1;          /* no. of members in class */
  int negation = false;

Debug1("EvaluateANDString(%s)={",class);

for (sp = class; *sp != '\0'; sp++)
   {
   if (*sp == '.')
      {
      count++;
      }
   }

sp = class;

while(*sp != '\0')
   {
   negation = false;

   sscanf(sp,"%[^.]",cbuff);

   while ((*sp != '\0') && (*sp !='.'))
      {
      sp++;
      }

   if (*sp == '.')
      {
      sp++;
      }

   simpleid = cbuff;

   if (cbuff[0] == '!')
      {
      negation = true;
      *simpleid++;
      }

   if (IsItemIn(VNEGHEAP,simpleid))
      {
      Debug1("negheap [%s]",simpleid);
      return false;
      } 

   if (IsItemIn(VHEAP,simpleid))
      {
      if (negation)
         {
         return false;
         }
      else
         {
         count--;
         }
      } 
 
   else if (IsItemIn(addclasses,simpleid))
      {
      Debug("addclasses (%s)",simpleid);

      if (negation)
         {
         return false;
         }
      else
         {
         count--;
         }
      } 

   else if (negation)    /* ! (an undefined class) == true */
      {
      Debug("!(undefined class)=true [%s]",simpleid);
      count--;
      }
   }

Debug1("}\n");

if (count == 0)
   {
   return(true);
   }
else
   {
   return(false);
   }
}

/*********************************************************************/
/* Floating                                                          */
/*********************************************************************/

