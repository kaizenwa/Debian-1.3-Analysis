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
 

#include "cf.defs.h"
#include "cf.extern.h"

/*******************************************************************/
/* Macro substitution based on HASH-table                          */
/*******************************************************************/

Hash(name)

char *name;

{ int i, slot = 0;

 for (i = 0; name[i] != '\0'; i++)
   {
   slot = (macroalphabet * slot + name[i]) % hashtablesize;
   }

return slot;
}

/*******************************************************************/

AddMacroValue(name,value)                           /* Like putenv */

char *name,*value;

{ char *sp, buffer[bufsize];
  int slot;

Debug1("AddMacroValue(%s=%s)\n",name,value);

if (name == NULL || value == NULL)
   {
   yyerror("Bad macro");
   }

if (strlen(name) > maxvarsize)
   {
   yyerror("macro name too long");
   return;
   }

buffer[0] = '\0';

sprintf(buffer,"%s=%s",name,value);

if ((sp = malloc(strlen(buffer)+1)) == NULL)
   {
   perror("malloc");
   FatalError("aborting");
   }

strcpy(sp,buffer);

slot = Hash(name);

if (HASH[slot] != 0)
   {
   if (CompareMacro(name,HASH[slot]) == 0)
      {
      sprintf(VBUFF,"Redefinition of macro %s",name);
      Warning(VBUFF);
      free(HASH[slot]);
      HASH[slot] = sp;
      return;
      }

   while ((HASH[++slot] != 0))
      {
      if (slot == hashtablesize-1)
         {
         slot = 0;
         }
      if (slot == Hash(name))
         {
         FatalError("AddMacroValue - internal error #1");
         }
      }
   }

HASH[slot] = sp;

Debug1("Added Macro at hash address %d: %s\n",slot,sp);
}

/*******************************************************************/

char *GetMacroValue(name)

char *name;

{ char *sp;
  int slot;

slot = Hash(name);

if (CompareMacro(name,HASH[slot]) != 0)
   {
   while (true)
      {
      slot++;

      if (CompareMacro(name,HASH[slot]) == 0)
         {
         for(sp = HASH[slot]; *sp != '='; sp++)
            {
            }

         return(sp+1);
         }

      if (slot == hashtablesize-1)
         {
         slot = 0;
         }

      if (slot == Hash(name))
         {
         return(getenv(name));  /* Look in environment if not found */
         }
      }
   }
else
   {
   for(sp = HASH[slot]; *sp != '='; sp++)
      {
      }

   return(sp+1);
   }   
}

/*******************************************************************/

RecordMacroId(name)

char *name;

{
Debug1("RecordMacroId(%s)\n",name);
strcpy(CURRENTITEM,name);
}

/*******************************************************************/

CompareMacro(name,macro)

char *name,*macro;

{ char buffer[bufsize];

if (macro == NULL || name == NULL)
   {
   return 1;
   }

sscanf(macro,"%[^=]",buffer);

Debug1("CompareMacro(%s,%s)=%s\n",name,macro,buffer);
return(strcmp(buffer,name));
}
