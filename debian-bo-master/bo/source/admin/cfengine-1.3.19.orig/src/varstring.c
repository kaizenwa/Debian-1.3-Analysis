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

/*********************************************************************/
/* TOOLKIT : Varstring expansion                                     */
/*********************************************************************/

IsVarString(str)

char *str;

{ char *sp;
  char left = 'x', right = 'x';
  int dollar = false;
  int bracks = 0;

for (sp = str; *sp != '\0' ; sp++)       /* check for varitems */
   {
   switch (*sp)
      {
      case '$': dollar = true;
                break;
      case '(':
      case '{': left = *sp;
                bracks++;
                break;
      case ')':
      case '}': right = *sp;
                bracks--;
                break;
      }
   }

if (left == 'x' && right == 'x')
   {
   return false;
   }

if (bracks != 0 && dollar)
   {
   yyerror("Incomplete variable syntax");
   return false;
   }

if (bracks != 0)
   {
   return false;
   }

if (left == '(' && right == ')' && dollar)
   {
   return true;
   }

if (left == '{' && right == '}' && dollar)
   {
   return true;
   }

yyerror("Bracket mismatch in variable substitution");
return false;
}

/*********************************************************************/

ExpandVarstring(string,buffer,bserver) 

char *string, *buffer, *bserver;

{ char *sp,*env;
  char varstring = false;
  char currentitem[bufsize], scanstr[6];
  int len;

if (bserver != NULL)
   {
   Debug1("(Binserver is %s)\n",bserver);
   }

buffer[0] = '\0';

for (sp = string; /* No exit */ ; sp++)       /* check for varitems */
   {
   currentitem[0] = '\0';

   sscanf(sp,"%[^$]",currentitem);

   strcat(buffer,currentitem);
   sp += strlen(currentitem);

   if (*sp == '$')
      {
      switch (*(sp+1))
         {
         case '(': 
                   varstring = ')';
                   break;
         case '{': 
                   varstring = '}';
                   break;
         default: 
                   strcat(buffer,"$");
                   continue;
         }
      sp++;
      }

   currentitem[0] = '\0';

   if (*sp == '\0')
      {
      break;
      }
   else
      {
      sprintf(scanstr,"%%[^%c]",varstring);   /* select the correct terminator */
      sscanf(++sp,scanstr,currentitem);               /* reduce item */

      switch (ScanVariable(currentitem))
         {
         case fac:
         case sit:
                   if (VFACULTY[0] == '\0')
                      {
                      yyerror("faculty/site undefined variable");
                      }

		   if (BufferOverflow(buffer,VFACULTY))
		      {
		      FatalError("Can't expand varstring");
		      }
                   strcat(buffer,VFACULTY);
                   break;


         case host:
	     	    if (BufferOverflow(buffer,VDEFAULTBINSERVER.name))
		      {
		      FatalError("Can't expand varstring");
		      }
                    strcat(buffer,VDEFAULTBINSERVER.name);
                    break;

	 case fqhost:
	     	    if (BufferOverflow(buffer,VFQNAME))
		      {
		      FatalError("Can't expand varstring");
		      }
                    strcat(buffer,VFQNAME);
                    break;
	     

         case binserv:
                    if (ACTION != links && ACTION != required)
                       {
                       yyerror("Inappropriate use of variable binserver");
                       FatalError("Bad variable");
                       }

		    if (BufferOverflow(buffer,"$(binserver)"))
		      {
		      FatalError("Can't expand varstring");
		      }
                    strcat(buffer,"$(binserver)");
                    break;

         case sysad:
                   if (VSYSADM[0] == '\0')
                      {
                      yyerror("sysadm undefined variable");
                      }

		   if (BufferOverflow(buffer,VSYSADM))
		      {
		      FatalError("Can't expand varstring");
		      }
                   strcat(buffer,VSYSADM);
                   break;

         case dom:
                   if (VDOMAIN[0] == '\0')
                      {
                      yyerror("domain undefined variable");
                      }

		   if (BufferOverflow(buffer,VDOMAIN))
		      {
		      FatalError("Can't expandvarstring");
		      }
                   strcat(buffer,VDOMAIN);
                   break;

         case nfstp:
	           if (BufferOverflow(buffer,VNFSTYPE))
		      {
		      FatalError("Can't expandvarstring");
		      }
                   strcat(buffer,VNFSTYPE);
                   break;

         case timez:
                   if (VTIMEZONE[0] == '\0')
                      {
                      yyerror("timezone undefined variable");
                      }

		   if (BufferOverflow(buffer,VTIMEZONE))
		      {
		      FatalError("Can't expandvarstring");
		      }
                   strcat(buffer,VTIMEZONE);
                   break;

         case clss:
	           if (BufferOverflow(buffer,CLASSTEXT[VSYSTEMHARDCLASS]))
		      {
		      FatalError("Can't expandvarstring");
		      }
                   strcat(buffer,CLASSTEXT[VSYSTEMHARDCLASS]);
                   break;

         case archt:
	           if (BufferOverflow(buffer,VARCH))
		      {
		      FatalError("Can't expandvarstring");
		      }
                   strcat(buffer,VARCH);
                   break;

	 case allclass:
	           if (BufferOverflow(buffer,ALLCLASSBUFFER))
		      {
		      FatalError("Can't expandvarstring");
		      }
                   strcat(buffer,ALLCLASSBUFFER);
                   break;

	 case cfspc:
	     	   if (BufferOverflow(buffer,""))
		      {
		      FatalError("Can't expandvarstring");
		      }
	           strcat(buffer," ");
		   break;

	 case cftab:
	     	   if (BufferOverflow(buffer,""))
		      {
		      FatalError("Can't expandvarstring");
		      }
	           strcat(buffer,"\t");
		   break;

	 case cflf:
	     	   if (BufferOverflow(buffer,""))
		      {
		      FatalError("Can't expandvarstring");
		      }
	           strcat(buffer,"\012");
		   break;
		   
         case cfcr:
	     	   if (BufferOverflow(buffer,""))
		      {
		      FatalError("Can't expandvarstring");
		      }
	           strcat(buffer,"\015");
		   break;

	 case repchar:
	     	   if (BufferOverflow(buffer,""))
		      {
		      FatalError("Can't expandvarstring");
		      }
		   len = strlen(buffer);
	           buffer[len] = REPOSCHAR;
		   buffer[len+1] = '\0';
	           break;

	 case listsep:
	     	   if (BufferOverflow(buffer,""))
		      {
		      FatalError("Can't expandvarstring");
		      }
		   len = strlen(buffer);
	           buffer[len] = LISTSEPARATOR;
		   buffer[len+1] = '\0';
                   break;

         default:  
                   if ((env = GetMacroValue(currentitem)) != NULL)
                      {
 		      if (BufferOverflow(buffer,env))
	   	         {
		         FatalError("Can't expandvarstring");
		         }
                      strcat(buffer,env);
                      break;
                      }

                   printf("Bad variable $(%s)\n",currentitem);
                   FatalError("No such variable or out of context");
         }

      sp += strlen(currentitem);
      currentitem[0] = '\0';
      }
   }

Debug1("-- Varstring expanded to %s\n",buffer);

return varstring;
}

/*********************************************************************/

ExpandVarbinserv(string,buffer,bserver) 

char *string, *buffer, *bserver;

{ char *sp,*env;
  char varstring = false;
  char currentitem[bufsize], scanstr[6];
  int len;

Debug("ExpandVarbinserv %s, ",string);

if (bserver != NULL)
   {
   Debug("(Binserver is %s)\n",bserver);
   }

buffer[0] = '\0';

for (sp = string; /* No exit */ ; sp++)       /* check for varitems */
   {
   currentitem[0] = '\0';

   sscanf(sp,"%[^$]",currentitem);

   strcat(buffer,currentitem);
   sp += strlen(currentitem);

   if (*sp == '$')
      {
      switch (*(sp+1))
         {
         case '(': 
                   varstring = ')';
                   break;
         case '{': 
                   varstring = '}';
                   break;
         default: 
                   strcat(buffer,"$");
                   continue;
         }
      sp++;
      }

   currentitem[0] = '\0';

   if (*sp == '\0')
      {
      break;
      }
   else
      {
      sprintf(scanstr,"%%[^%c]",varstring);   /* select the correct terminator */
      sscanf(++sp,scanstr,currentitem);               /* reduce item */

      switch (ScanVariable(currentitem))
         {
         case binserv:
		    if (BufferOverflow(buffer,bserver))
		      {
		      FatalError("Can't expand varstring");
		      }
                    strcat(buffer,bserver);
                    break;
         }

      sp += strlen(currentitem);
      currentitem[0] = '\0';
      }
   }

return varstring;
}

/*********************************************************************/

enum vnames ScanVariable(name)

char *name;

{ int i;

for (i = 0; VVNAMES[i] != '\0'; i++)
   {
   if (strcmp(VVNAMES[i],name) == 0)
      {
      return (enum vnames) i;
      }
   }
return (enum vnames) i;
}


/*********************************************************************/

struct Item *SplitVarstring(varstring,sep)

char *varstring;
char sep;

{ struct Item *liststart = NULL;
  char format[6], *sp;
  char node[maxlinksize];
  char buffer[bufsize];

Debug1("SplitVarstring(%s)\n",varstring);

if (strcmp(varstring,"") == 0)   /* Handle path = / as special case */
   {
   AppendItem(&liststart,"",NULL);
   return liststart;
   }

sprintf(format,"%%[^%c]",sep);   /* set format string to search */

ExpandVarstring(varstring,buffer,""); 

for (sp = buffer; *sp != '\0'; sp++)
   {
   bzero(node,maxlinksize);
   sscanf(sp,format,node);

   if (strlen(node) == 0)
      {
      continue;
      }
   
   sp += strlen(node)-1;

   AppendItem(&liststart,node,NULL);

   if (*sp == '\0')
      {
      break;
      }
   }

return liststart;
}

