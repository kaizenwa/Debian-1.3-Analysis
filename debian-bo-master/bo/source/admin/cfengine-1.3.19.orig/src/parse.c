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
/*  Parse Zone for cfengine                                        */
/*                                                                 */
/*  This is wide screen entertainment. Resize                      */
/*  your window before viewing!                                    */
/*                                                                 */
/*  The routines here are called by the lexer and the yacc parser  */
/*                                                                 */
/*******************************************************************/

 /*
    The parser uses the classification of strings into id's items
    paths, varpaths etc. Each complete action is gradually assembled
    by setting flags based on what action, class etc is being
    parsed. e.g. HAVEPATH, FROM_LINK etc.. these are typed as
    "flag" in the variables file. When all the necessary critera
    are met, or the beginning of a new definition is found
    the action gets completed and installed in the actions lists.

  */

#define INET

#include <stdio.h>
#include "cf.defs.h"
#include "cf.extern.h"


/*******************************************************************/

SetAction (action)

enum actions action;

{
InstallPending(ACTION);   /* Flush any existing actions */

Debug1("\nBEGIN NEW ACTION %s\n",ACTIONTEXT[action]);

ACTION = action;
strcpy(ACTIONBUFF,ACTIONTEXT[action]);

switch (ACTION)
   {
   case files:
   case makepath:
   case tidy:
   case disable:
   case image:
   case links:
   case processes:  InitializeAction();
   }

strcpy(CLASSBUFF,"any");    /* default class */
CLASS=soft;
}

/*******************************************************************/

HandleId(id)

char *id;

{
Debug1("HandleId(%s)\n",id);

if (ACTION == control)
   {
   switch(ScanVariable(id))
      {
      case fac:    
      case sit:       CLASS = faculty;
                      return;

      case sysad:     CLASS = sysadm;
                      return;

      case dom:       CLASS = domain;
                      return;

      case timez:     CLASS = timezon;
                      return;

      case netmsk:    CLASS = netmask;
                      return;

      case nfstp:     CLASS = nfstypec;
                      return;

      case ssize:     CLASS = sensiblesize;
                      return;

      case scount:    CLASS = sensiblecount; 
                      return;

      case esize:     CLASS = editfilesize;
                      return;

      case actseq:    CLASS = actionsequence;
                      return;

      case acess:     CLASS = accesss;
                      return;

      case mpat:      CLASS = mountpath;
                      return;

      case hpat:      CLASS = homepat;
                      return;

      case adclass:   CLASS = addclasses;
                      return;

      case excludecp: CLASS = excludecopy;
		      return;

      case excludeln: CLASS = excludelink;
	              return;

      case cplinks:   CLASS = copylinks;
	              return;
		      
      case lncopies:  CLASS = linkcopies;
	              return;

      case repos:     CLASS = repository;
	              return;

      case repchar:   CLASS = reposchar;
		      return;

      case listsep:   CLASS = listseparator;
	              return;

      case underscore:
	              CLASS = underscoreclasses;
		      return;

      default:        RecordMacroId(id);
                      CLASS = soft;
                      return;
      }
   }


if (ACTION == groups)
   { int count = 1;
     char *cid = id;

while (*cid != '\0')
   {
   if (*cid++ == '.')
      {
      count++;
      }
   }

   if (strcmp(id,"any") == 0)
      {
      yyerror("Reserved class <any>");
      }

   if (count > 1)                              /* compound class */
      {
      yyerror("Group with compound identifier");
      FatalError("Dots [.] not allowed in group identifiers");
      }

   if (IsHardClass(id))
      {
      yyerror("Reserved class name (choose a different name)");
      }

   strcpy(GROUPBUFF,id);
   }

strcpy(CLASSBUFF,"any");    /* default class */
CLASS=soft;
}

/*******************************************************************/

HandleClass (id)

char *id;

{ int members;

InstallPending(ACTION);

Debug1("HandleClass(%s)\n",id);

if ((members = CompoundId(id)) > 1)             /* Parse compound id */
   {
   Debug1("Compound identifier-class = (%s) with %d members\n",id,members);
   }
else
   {
   Debug1("simple identifier-class = (%s)\n",CLASSTEXT[CLASS]);
   }

Debug1("HandleClass(end)\n");
}

/*******************************************************************/

HandleItem (item)

char *item;

{
Debug1("HandleItem(%s)\n",item);

if (strcmp(item,"+") == 0 || strcmp(item,"-") == 0)
   {
   yyerror("+/- not bound to identifier");
   }

if (item[0] == '+')                               /* Lookup in NIS */
   {
   item++;

   if (item[0] == '@')                               /* Irrelevant */
      {
      item++;
      }

   Debug1("Netgroup item, lookup NIS group (%s)\n",item);

   strcpy(CURRENTITEM,item);

   if (ACTION == groups)
      {
      HandleGroupItem(item,netgroup);
      }
   else
      {
      yyerror("Netgroup reference outside group: action or illegal octal number");
      FatalError("Netgroups may only define internal groups or illegal octal file action.");
      }
   }
else if (item[0] == '-') 
   {
   item++;

   if (item[0] == '@')                               /* Irrelevant */
      {
      item++;

      if (ACTION == groups)
         {
         HandleGroupItem(item,groupdeletion);
         }
      else
         {
         yyerror("Netgroup reference outside group: action or illegal octal number");
         FatalError("Netgroups may only define internal groups or illegal octal file action.");
         }
      }
   else
      {
      if (ACTION == groups)
         {
         HandleGroupItem(item,deletion);
         }
      else
         {
         yyerror("Illegal deletion sign or octal number");
         FatalError("The deletion operator may only be in the groups: action");
         }
      }


   }
else if (item[0] == '\"' || item[0] == '\'' || item[0] == '`') 
   {
   *(item+strlen(item)-1) = '\0';

   if (ACTION == groups)                     /* This test should be redundant */
      {
      HandleGroupItem(item,classscript);
      }
   }
else
   {
   Debug1("simple item = (%s)\n",item);
  
   /* CLASS set by CompoundId via HandleId */

   switch(ACTION)
      {
      case control:     InstallLocalInfo(CLASS,item);
                        break;
      case groups:      HandleGroupItem(item,simple);
                        break;
      case resolve:     AppendNameServer(item);
                        break;
      case image:
      case files:       HandleFileItem(item);
                        break;
      case tidy:        strcpy(CURRENTITEM,item);
                        break;
      case homeservers: InstallHomeserverItem(item);
                        break;
      case binservers:  InstallBinserverItem(item);
                        break;
      case mailserver:  yyerror("Give whole rpc path to mailserver");
                        break;
      case required:    yyerror("Required filesystem must be an absolute path");
                        FatalError("Fatal error");
                        break;
      case mountables:  yyerror("Mountables should be specified by an absolute pathname");
                        break;
      case links:       if (ACTION_IS_LINKCHILDREN && strcmp (item,"linkchildren") == 0)
                           {
                           strcpy(LINKTO,item);
                           }
                        else
                           {
                           yyerror("Links requires path or varitem");
                           }
                        break;
      case import:      AppendImport(item);
                        break;
      case shellcommands:
                        AppendScript(item);
                        break;
      case makepath:    yyerror("makepath needs an abolute pathname");
                        FatalError("Fatal Error");
      case disable:     yyerror("disable needs an absolute path name");
                        FatalError("Fatal Error");
      case broadcast:   InstallBroadcastItem(item);
                        break;
      case defaultroute:InstallDefaultRouteItem(item);
                        break;
      case misc_mounts: if (MOUNT_FROM && MOUNT_ONTO && (strcmp(item,"rw") == 0 || strcmp(item,"ro") == 0))
                           {
                           AppendMiscMount(MOUNTFROM,MOUNTONTO,item);
                           CURRENTITEM[0] = '\0';
                           MOUNT_FROM = false;
                           MOUNT_ONTO = false;
                           }
                        else
                           {
                           yyerror("misc_mounts: host:/frompath /mounton_path [ro|rw]\n");
                           }
                        break;

      case unmounta:    yyerror("Umount must be in the format machine:directory");
                        break;

      case editfiles:   /* action recorded in CURRENTITEM, installed from qstring in lexer */
                        strcpy(CURRENTITEM,item);
                        break;

      case ignore:      AppendIgnore(item);
                        break;

      case processes:   if (strcmp(item,"restart") == 0)
	                   {
			   HAVE_RESTART = true;
			   ACTIONPENDING = false;
			   return;
			   }

                        if (ACTIONPENDING)
			   {
			   InstallPending(ACTION);          /* Flush any existing actions */
                           InitializeAction();
			   }
      
                        if (strcmp(item,"SetOptionString") == 0)
			   {
                           if (EXPR[0] == '\0')
			      {
   			      strcpy(EXPR,item);
			      ACTIONPENDING = false;
			      HAVE_RESTART = true;    /* save option string in restart */
			      return;
			      }
			   else
			      {
			      yyerror("Inappropriate placement of SetOptionString");
			      return;
			      }
			   }

	                if (EXPR[0] == '\0')
	                   {
			   if (HAVE_RESTART)
			      {
			      yyerror("Missing search expression");
			      }
			   strcpy(EXPR,item);
			   ACTIONPENDING = true;
			   }
                        else
			   {
			   if (HAVE_RESTART)
			      {
			      strcpy(RESTART,item);
			      HAVE_RESTART= false;
			      ACTIONPENDING = true;
			      }
			   }
                        break;

      default:          FatalError("Internal software failure: Unknown action (%s)\n",item);
      }
   }
}

/***************************************************************************/

HandlePath (path)

char *path;

{
if (ACTION == processes && ! ACTIONPENDING)
   {
   }
else
   {
   InstallPending(ACTION);           /* Flush any existing actions */
   InitializeAction();                 /* Clear data for files/dirs  */
   }

Debug1("path = (%s)\n",path);

strcpy(CURRENTPATH,path);                   /* Yes this must be here */

ACTIONPENDING = true;                     /* we're parsing an action */

if (ACTION_IS_LINK || ACTION_IS_LINKCHILDREN)  /* to-link (after ->) */
   {
   strcpy(LINKTO,CURRENTPATH);
   }
else
   {
   switch (ACTION)
      {
      case control:  if (CLASS == mountpath)
                        {
                        SetMountPath(path);
			break;
                        }

                     if (CLASS == repository)
			{
			SetRepository(path);
			break;
			}

		     if (CLASS == reposchar)
			{
			if (strlen(path) > 1)
			   {
			   yyerror("reposchar can only be a single letter");
			   break;
			   }
			if (path[0] == '/')
			   {
			   yyerror("illegal value for reposchar");
			   break;
			   }
			REPOSCHAR = path[0];
			}

		     if (CLASS == listseparator)
			{
			if (strlen(path) > 1)
			   {
			   yyerror("listseparator can only be a single letter");
			   break;
			   }
			if (path[0] == '/')
			   {
			   yyerror("illegal value for listseparator");
			   break;
			   }
			LISTSEPARATOR = path[0];
			}

                     if (CLASS == homepat)
                        {
                        yyerror("Path relative to mountpath required");
                        FatalError("Absolute path was specified\n");
                        }

                    if (CLASS == soft)
                       {
                       AddMacroValue(CURRENTITEM,path);
                       }

                       break;
      case import:     AppendImport(path);
                       break;
      case links:      /* from link (see cf.l) */
                       break;
      case required:   InstallRequiredPath(path);
                       break;
      case shellcommands:
                       AppendScript(path);
                       break;
      case mountables: AppendMountable(path);
                       break;
      case mailserver: InstallMailserverPath(path);
                       break;
      case tidy:       strcpy(CURRENTITEM,path);
                       break;
      case disable:    strcpy(CURRENTPATH,path);
                       ACTIONPENDING = true;
                       break;
      case makepath:   strcpy(CURRENTPATH,path);
                       break;
      case ignore:     AppendIgnore(path);
                       break;

      case misc_mounts:if (! MOUNT_FROM)
                          {
                          MOUNT_FROM = true;
                          strcpy(MOUNTFROM,CURRENTPATH);
                          }
                       else
                          {
                          if (MOUNT_ONTO)
			    {
                            yyerror ("Path not expected");
                            FatalError("misc_mount: syntax error");
                            }
                          MOUNT_ONTO = true;
                          strcpy(MOUNTONTO,CURRENTPATH);
                          }
                       break;

      case unmounta:   AppendUmount(path);
                       break;
      case image:
      case files:      
                       break;

      case editfiles:  /* file recorded in CURRENTPATH */
                       break;

      case processes:   if (EXPR[0] == '\0')
	                   {
			   if (HAVE_RESTART)
			      {
			      yyerror("Missing search expression");
			      }
			   strcpy(EXPR,path);
			   }
                        else
			   {
			   if (HAVE_RESTART)
			      {
			      strcpy(RESTART,path);
			      HAVE_RESTART = false;
			      ACTIONPENDING = true;
			      }
			   }
		       break;

      default:         printf ("HandlePath() DEFAULT\n");
                       printf ("Fatal action is %s\n",ACTIONTEXT[ACTION]);
                       FatalError("Internal software error in HandlePath()");
      }
   }
}

/*******************************************************************/

HandleVarpath(varpath)         

  /* Expand <binserver> and <fac> etc. Note that the right hand  */
  /* side of links etc. gets expanded at runtime. <binserver> is */
  /* only legal on the right hand side.                          */

char *varpath;

{
InstallPending(ACTION);   /* Flush any existing actions */
InitializeAction();

Debug1("HandleVarpath(%s)\n",varpath);

if (IsWildCard(varpath) && ! (ACTION == files || ACTION == tidy))
   {
   yyerror("Wildcards can only be used in pathnames in tidy: and files:");
   }

strcpy(CURRENTPATH,varpath);

ACTIONPENDING = true;
 
if (ACTION_IS_LINK || ACTION_IS_LINKCHILDREN)
   {
   strcpy(LINKTO,varpath);
   }
else 
   {
   switch (ACTION)
      {
      case tidy:       strcpy(CURRENTITEM,varpath);
                       break;
		       
      case required:   InstallRequiredPath(varpath);
                       break;
		       
      case makepath:   strcpy(CURRENTPATH,varpath);
                       break;
		       
      case control:    if (CLASS == mountpath)
                          {
                          SetMountPath(varpath);
			  break;
                          }
      
                        if (CLASS == repository)
			   {
			   SetRepository(varpath);
			   break;
			   }

		     if (CLASS == reposchar)
			{
			if (strlen(varpath) > 1)
			   {
			   yyerror("reposchar can only be a single letter");
			   break;
			   }
			if (varpath[0] == '/')
			   {
			   yyerror("illegal value for reposchar");
			   break;
			   }
			REPOSCHAR = varpath[0];
			}

		     if (CLASS == listseparator)
			{
			if (strlen(varpath) > 1)
			   {
			   yyerror("listseparator can only be a single letter");
			   break;
			   }
			if (varpath[0] == '/')
			   {
			   yyerror("illegal value for listseparator");
			   break;
			   }
			LISTSEPARATOR = varpath[0];
			}			

                        if (CLASS == homepat)
                           {
                           yyerror("Home-pattern should be relative to mount-path, not absolute");
                           }

                        if (CLASS == soft)
                           {
                           yyerror("Nested macros not permitted");
                           }
                        break;

      case ignore:      AppendIgnore(varpath);
                        break;

      case links:       /* FROM LINK */
	                break;
      case image:
      case files:
      case editfiles:
                        break;

      case disable:    strcpy(CURRENTPATH,varpath);
                       ACTIONPENDING = true;
                       break;
		       
      case processes:   if (EXPR[0] == '\0')
	                   {
			   if (HAVE_RESTART)
			      {
			      yyerror("Missing search expression");
			      }
			   strcpy(EXPR,varpath);
			   }
                        else
			   {
			   if (HAVE_RESTART)
			      {
			      strcpy(RESTART,varpath);
			      HAVE_RESTART = false;
			      ACTIONPENDING = true;
			      }
			   }
		        break;
			
      default:          FatalError("software error in HandleVarpath()");
      }
   }
}

/*******************************************************************/

HandleWildcard(wildcard)

char *wildcard;

{
Debug1("wildcard = (%s)\n",wildcard);

ACTIONPENDING = true;

switch (ACTION)

   {
   case ignore: AppendIgnore(wildcard);
                break;

   case control:  if (CLASS == homepat)
                     {
                     if (*wildcard == '/')
                        {
                        yyerror("Home pattern was specified as absolute path (should be relative to mountpath)");
                        }


                     Debug1(">>Installing wildcard %s as a home pattern\n",wildcard);
                     HandleHomePattern(wildcard);
		     }
                 else if (CLASS == soft)
                     {
                     AddMacroValue(CURRENTITEM,wildcard);
                     }
                 else if ( CLASS == excludecopy || CLASS == excludelink
 		       || CLASS == copylinks || CLASS == linkcopies
		       || CLASS == reposchar || CLASS == listseparator)
		    {
                    if (*wildcard == '/')
                       {
                       yyerror("Pattern should be a relative name, not an absolute path");
                       }
                    InstallLocalInfo(CLASS,wildcard);
                    }
                 else
                    {
                    RecordMacroId(wildcard);
                    }
                 break;
		 
   case files:
                 HandleOptionalFileAttribute(wildcard);
		 break;
   case image:
                 HandleOptionalImageAttribute(wildcard);
		 break;
   case tidy:
                 HandleOptionalTidyAttribute(wildcard);
                 break;
		 
   case makepath:
                 HandleOptionalDirAttribute(wildcard);
		 break;

   case disable:
                 HandleOptionalDisableAttribute(wildcard);
                 break;
   case links:
                 HandleOptionalLinkAttribute(wildcard);
                 break;
   case processes:
                 HandleOptionalProcessAttribute(wildcard);
                 break;
   default:
                 yyerror("Wildcards can only be used in pathnames in tidy: and files:");
   }
}

/*******************************************************************/

HandleEdit(file,edit,string)      /* child routines in edittools.c */

char *file, *edit, *string;

{
if (string == NULL)
   {
   Debug1("Handling Edit of %s, action [%s] with no data\n",file,edit);
   }
else
   {
   Debug1("Handling Edit of %s, action [%s] with data <%s>\n",file,edit,string);
   }

if (EditFileExists(file))
   {
   AddEditAction(file,edit,string);
   }
else
   {
   InstallEditFile(file,edit,string);
   }
}

/*******************************************************************/
/* Level 2                                                         */
/*******************************************************************/

CompoundId(id)                       /* check for dots in the name */

char *id;

{ int count = 1;
  char *cid = id;

for (cid = id; *cid != '\0'; cid++)
   {
   if (*cid == '.' || *cid == '|')
      {
      count++;
      }
   }

strcpy(CLASSBUFF,id);

switch (count)
   {
   case 1: 
           if (IsHardClass(id))
              {
              CLASS = ClassStringToCode(id);
              }
           else
              {                              /* <soft> class is a group */
              CLASS = soft;
              }

           break;

   default: CLASS = soft;
            break;
   }

return(count);
}

/*******************************************************************/

InstallLocalInfo (class,value)

enum classes class;
char *value;

{ int number = -1;
  char buffer[maxvarsize], *sp;

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",value);
   return;
   }

Debug1("(Action is control, storing class [%s=%s])\n",CLASSTEXT[class],value);

switch (class)
   {
   case site:
   case faculty:  if (VFACULTY[0] != '\0')
                     {
                     yyerror("Multiple declaration of variable faculty / site");
                     FatalError("Redefinition of basic system variable");
                     }

                  strcpy(VFACULTY,value);
                  break;

   case domain:  if (strcmp(VDOMAIN,"undefined.domain") != 0)
                     {
                     yyerror("Multiple declaration of variable domain");
                     FatalError("Redefinition of basic system variable");
                     }

                  strcpy(VDOMAIN,value);
		  
		  if (!strstr(VSYSNAME.nodename,VDOMAIN))
		     {
		     sprintf(VFQNAME,"%s.%s",VSYSNAME.nodename,VDOMAIN);
		     }
		  else
		     {
		     strcpy(VFQNAME,VSYSNAME.nodename);
		     }
		  
		  if (! NOHARDCLASSES)
		     {
		     strcpy(buffer,VFQNAME);
		     for (sp=buffer; *sp != '\0'; sp++)
			{
			if (*sp == '.')
			   {
			   *sp = '_';
			   }
			}
		     AddClassToHeap(buffer);
		     }
                  break;

   case sysadm:  /* Can be redefined */

                  strcpy(VSYSADM,value);
                  break;

   case netmask:  if (VNETMASK[0] != '\0')
                     {
                     yyerror("Multiple declaration of variable netmask");
                     FatalError("Redefinition of basic system variable");
                     }
                  strcpy(VNETMASK,value);
                  break;


   case mountpath: SetMountPath(value);
                   break;

   case repository:
                   SetRepository(value);
		   break;

   case homepat:  
                  Debug1("Installing %s as home pattern\n",value);
                  AppendItem(&VHOMEPATLIST,value,NULL);
                  break;


   case timezon:  if (VTIMEZONE[0] != '\0')
                     {
                     yyerror ("Multiple declaration of variable timezone");
                     FatalError("Redefinition of basic system variable");
                     }

                  strcpy(VTIMEZONE,value);
                  break;

   case sensiblesize: 
                  sscanf(value,"%d",&number);
                  if (number > 0)
                     {
                     SENSIBLEFSSIZE = number;
                     }
                  else
                     {
                     yyerror("Silly value for sensiblesize");
                     }
                  break;

   case sensiblecount:
                  sscanf(value,"%d",&number);
                  if (number > 0)
                     {
                     SENSIBLEFILECOUNT = number;
                     }
                  else
                     {
                     yyerror("Silly value for sensiblecount");
                     }

                  break;

   case editfilesize:
                  sscanf(value,"%d",&number);

                  if (number > 10)
                     {
                     EDITFILESIZE = number;
                     }
                  else
                     {
                     yyerror("Silly value for editfilesize");
                     }

                  break;

   case actionsequence:
                  AppendToActionSequence(value);
                  break;

   case accesss:
                  AppendToAccessList(value);
                  break;

   case nfstypec:
                  strcpy(VNFSTYPE,value); 
                  break;

   case addclasses:
                  AddCompoundClass(value);
                  break;

   case excludecopy:
                  PrependItem(&VEXCLUDECOPY,value,CLASSBUFF);
                  break;
		  
   case excludelink:
                  PrependItem(&VEXCLUDELINK,value,CLASSBUFF);
                  break;
		  
   case copylinks:
                  PrependItem(&VCOPYLINKS,value,CLASSBUFF);
                  break;
   case linkcopies:
                  PrependItem(&VLINKCOPIES,value,CLASSBUFF);
                  break;

   case reposchar:
		    if (strlen(value) > 1)
			{
			yyerror("reposchar can only be a single letter");
			break;
			}
		     if (value[0] == '/')
			{
			yyerror("illegal value for reposchar");
			break;
			}
		     REPOSCHAR = value[0];
		     break;

   case listseparator:
			if (strlen(value) > 1)
			   {
			   yyerror("listseparator can only be a single letter");
			   break;
			   }
			if (value[0] == '/')
			   {
			   yyerror("illegal value for listseparator");
			   break;
			   }
			LISTSEPARATOR = value[0];
			break;
			
   case underscoreclasses:
                        if (strcmp(value,"on") == 0)
			   { char rename[maxvarsize];
			   UNDERSCORE_CLASSES=true;
			   Verbose("Resetting classes using underscores...\n");
			   while(DeleteItemContaining(&VHEAP,CLASSTEXT[VSYSTEMHARDCLASS]))
			      {
			      }

			   sprintf(rename,"_%s",CLASSTEXT[VSYSTEMHARDCLASS]);
			   
                           AddClassToHeap(rename);
			   break;
			   }

			if (strcmp(value,"off") == 0)
			   {
			   UNDERSCORE_CLASSES=false;
			   break;
			   }
			
                        yyerror("illegal value for underscoreclasses");
			break;


   default:       AddMacroValue(CURRENTITEM,value);
                  break;
                  
   }


}

/*******************************************************************/

HandleOptionalFileAttribute(item)

char *item;

{ char value[maxvarsize];

VBUFF[0] = value[0] = '\0';

ExpandVarstring(item,VBUFF,NULL);

sscanf(VBUFF,"%*[^=]=%s",value);

if (value[0] == '\0')
   {
   yyerror("File attribute with no value");
   }

Debug1("HandleOptionalFileAttribute(%s)\n",value);

switch(GetCommAttribute(item))
   {
   case cfrecurse: HandleRecurse(value);
                   break;
   case cfmode:    ParseModeString(value,&PLUSMASK,&MINUSMASK);
                   break;
   case cfowner:   strcpy(VUIDNAME,value);
                   break;
   case cfgroup:   strcpy(VGIDNAME,value);
                   break;
   case cfaction:  FILEACTION = GetFileAction(value);
                   break;
   case cflinks:   HandleTravLinks(value);
                   break;

   default:        yyerror("Illegal file attribute");
   }
}


/*******************************************************************/

HandleOptionalImageAttribute(item)

char *item;

{ char value[maxvarsize];

VBUFF[0] = value[0] = '\0';

ExpandVarstring(item,VBUFF,NULL);

sscanf(VBUFF,"%*[^=]=%s",value);

if (value[0] == '\0')
   {
   yyerror("Image attribute with no value");
   }

Debug1("HandleOptionalImageAttribute(%s)\n",value);

switch(GetCommAttribute(item))
   {
   case cfmode:    ParseModeString(value,&PLUSMASK,&MINUSMASK);
                   break;
   case cfowner:   strcpy(VUIDNAME,value);
                   break;
   case cfgroup:   strcpy(VGIDNAME,value);
                   break;
   case cfdest:    strcpy(DESTINATION,value);
                   break;
   case cfaction:  strcpy(IMAGEACTION,value);
                   break;
   case cfforce:   HandleForceCopy(value);
                   break;
   case cfbackup:  HandleCopyBackup(value);
                   break;
   case cfrecurse: HandleRecurse(value);
                   break;
   case cftype:    HandleCopyType(value);
                   break;
   case cfexclude: PrependItem(&VEXCLUDEPARSE,value,"any");
                   break;
   case cfsymlink: PrependItem(&VCPLNPARSE,value,"any");
                   break;
   case cfinclude: PrependItem(&VINCLUDEPARSE,value,"any");
                   break; 
   case cflntype:  HandleLinkType(value);
                   break;
   case cfserver:  HandleServer(value);
                   break;
   default:        yyerror("Illegal image attribute");
   }
}

/******************************************************************/

HandleOptionalTidyAttribute(item)

char *item;

{ char value[maxvarsize];

VBUFF[0] = value[0] = '\0';

ExpandVarstring(item,VBUFF,NULL);

sscanf(VBUFF,"%*[^=]=%s",value);

if (value[0] == '\0')
   {
   yyerror("Tidy attribute with no value");
   }

Debug1("HandleOptionalTidyAttribute(%s)\n",value);

switch(GetCommAttribute(item))
   {
   case cfrecurse: HandleRecurse(value);
                   break;

   case cfpattern: strcpy(CURRENTITEM,value);
                   if (*value == '/')
		      {
		      yyerror("search pattern begins with / must be a relative name");
		      }
                   break;

   case cfage:     HandleAge(value);
                   break;

   case cflinks:   HandleTravLinks(value);
                   break;

   case cfsize:    HandleTidySize(value);
                   break;

   case cftype:    HandleTidyType(value);
                   break;

   case cfdirlinks:
                   HandleTidyLinkDirs(value);
		   break;

   case cfrmdirs:  HandleTidyRmdirs(value);
                   break;

   default:        yyerror("Illegal tidy attribute");
   }
}

/******************************************************************/

HandleOptionalDirAttribute(item)

char *item;

{ char value[maxvarsize];

VBUFF[0] = value[0] = '\0';

ExpandVarstring(item,VBUFF,NULL);

sscanf(item,"%*[^=]=%s",value);

if (value[0] == '\0')
   {
   yyerror("Directory attribute with no value");
   }

Debug1("HandleOptionalDirAttribute(%s)\n",value);

switch(GetCommAttribute(item))
   {
   case cfmode:    ParseModeString(value,&PLUSMASK,&MINUSMASK);
                   break;
   case cfowner:   strcpy(VUIDNAME,value);
                   break;
   case cfgroup:   strcpy(VGIDNAME,value);
                   break;
   default:        yyerror("Illegal directory attribute");
   }
}


/*******************************************************************/

HandleOptionalDisableAttribute(item)

char *item;

{ char value[maxvarsize];

VBUFF[0] = value[0] = '\0';

ExpandVarstring(item,VBUFF,NULL);

sscanf(VBUFF,"%*[^=]=%s",value);

if (value[0] == '\0')
   {
   yyerror("Disable attribute with no value");
   }

Debug1("HandleOptionalDisableAttribute(%s)\n",value);

switch(GetCommAttribute(item))
   {
   case cftype:    HandleDisableFileType(value);
                   break;

   case cfrotate:  HandleDisableRotate(value);
                   break;

   default:        yyerror("Illegal disable attribute");
   }
}


/*******************************************************************/

HandleOptionalLinkAttribute(item)

char *item;

{ char value[maxvarsize];

VBUFF[0] = value[0] = '\0';

ExpandVarstring(item,VBUFF,NULL);

sscanf(VBUFF,"%*[^=]=%s",value);

if (value[0] == '\0')
   {
   yyerror("Link attribute with no value");
   }

Debug1("HandleOptionalLinkAttribute(%s)\n",value);

switch(GetCommAttribute(item))
   {
   case cfaction:  HandleLinkAction(value);
                   break;
   case cftype:    HandleLinkType(value);
                   break;
   case cfexclude: PrependItem(&VEXCLUDEPARSE,value,"any");
                   break;
   case cfinclude: PrependItem(&VINCLUDEPARSE,value,"any");
                   break;
   case cfcopy:    PrependItem(&VCPLNPARSE,value,"any");
                   break;
   case cfrecurse: HandleRecurse(value);
                   break;
   case cfcptype:  HandleCopyType(value);
                   break;
   default:        yyerror("Illegal link attribute");
   }
}

/*******************************************************************/

HandleOptionalProcessAttribute(item)

char *item;

{ char value[maxvarsize];

if (HAVE_RESTART)     /* wrote `restart' without followingby ".." */
   {
   yyerror("Missing restart command (quoted string expected) or bad SetOptionString placement");
   }

VBUFF[0] = value[0] = '\0';

ExpandVarstring(item,VBUFF,NULL);

sscanf(VBUFF,"%*[^=]=%s",value);

if (value[0] == '\0')
   {
   yyerror("Process attribute with no value");
   }

Debug1("HandleOptionalProcessAttribute(%s)\n",value);

switch(GetCommAttribute(item))
   {
   case cfaction:  if (strcmp(value,"signal") == 0 || strcmp(value,"do") == 0)
                      {
		      PROACTION = 's';
                      }
                   else if (strcmp(value,"warn") == 0)
		      {
		      PROACTION = 'w';
		      }
                   else
		      {
		      yyerror("Unknown action for processes");
		      }
                   break;
   case cfmatches: HandleProcessMatches(value);
                   break;
   case cfsignal:  HandleProcessSignal(value);
                   break;
   default:        yyerror("Illegal process attribute");
   }
}

/*******************************************************************/

HandleFileItem(item)

char *item;

{ int i;
  char err[100];

if (strcmp(item,"home") == 0)
   {
   ACTIONPENDING=true;
   strcpy(CURRENTPATH,"home");
   return;
   }

sprintf(err,"Unknown attribute %s",item);
yyerror(err);
}


/*******************************************************************/

InstallBroadcastItem(item)

char *item;

{
Debug1("Install broadcast mode (%s)\n",item);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",item);
   return;
   }

if (VBROADCAST[0] != '\0')
   {
   yyerror("Multiple declaration of variable broadcast");
   FatalError("Redefinition of basic system variable");
   }

if (strcmp("ones",item) == 0)
   {
   strcpy(VBROADCAST,"one");
   return;
   }

if (strcmp("zeroes",item) == 0)
   {
   strcpy(VBROADCAST,"zero");
   return;
   }

if (strcmp("zeros",item) == 0)
   {
   strcpy(VBROADCAST,"zero");
   return;
   }

yyerror ("Unknown broadcast mode (should be ones, zeros or zeroes)");
FatalError("Unknown broadcast mode");
}

/*******************************************************************/

InstallDefaultRouteItem(item)

char *item;

{ struct hostent *hp;
  struct in_addr inaddr;

Debug1("Install defaultroute mode (%s)\n",item);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",item);
   return;
   }

if (VDEFAULTROUTE[0] != '\0')
   {
   yyerror("Multiple declaration of variable defaultroute");
   FatalError("Redefinition of basic system variable");
   }

if (inet_addr(item) == -1)
   {
   if ((hp = gethostbyname(item)) == NULL)
      {
      perror("InstallDefaultRouteItem: gethostbyname: ");
      yyerror ("Bad specification of default packet route: hostname or decimal IP address");
      FatalError("Stopped.");
      }
   else
      {
      bcopy(hp->h_addr,&inaddr, hp->h_length);
      strcpy(VDEFAULTROUTE,inet_ntoa(inaddr));
      }
   }
else
   {
   strcpy(VDEFAULTROUTE,item);
   }
}

/*******************************************************************/

HandleGroupItem(item,type)

char *item;
enum itemtypes type;

{ char *machine, *user, *domain;

Debug1("Handling item (%s) in group (%s), type=%d\n",item,GROUPBUFF,type);

switch (type)
   {
   case simple:    if (strcmp(item,VDEFAULTBINSERVER.name) == 0)
                      {
                      AddClassToHeap(GROUPBUFF);
                      break;
                      }

                   if (IsItemIn(VHEAP,item))  /* group reference */
                      {
                      AddClassToHeap(GROUPBUFF);
                      break;
                      }

                   break;

   case netgroup:  setnetgrent(item);

                   while (getnetgrent(&machine,&user,&domain))
                      {
                      if (strcmp(machine,VDEFAULTBINSERVER.name) == 0)
                         {
                         Debug1("Matched %s in netgroup %s\n",machine,item);
                         AddClassToHeap(GROUPBUFF);
                         break;
                         }
                      }
                   
                   endnetgrent();
                   break;


   case groupdeletion: 

                   setnetgrent(item);

                   while (getnetgrent(&machine,&user,&domain))
                      {
                      if (strcmp(machine,VDEFAULTBINSERVER.name) == 0)
                         {
                         Debug1("Matched delete item %s in netgroup %s\n",machine,item);
                         DeleteItemStarting(&VHEAP,GROUPBUFF);
                         break;
                         }
                      }
                   
                   endnetgrent();
                   break;

   case classscript:

                   VBUFF[0] = '\0';
                   ExpandVarstring (item+1,VBUFF,NULL);
                   if (VBUFF[0] != '/')
                      {
                      yyerror("Quoted scripts must begin with / for absolute path");
                      break;
                      }

                   if (ShellCommandReturnsZero(VBUFF))
                      {
                      AddClassToHeap(GROUPBUFF);
                      }

                   break;

   case deletion:  if (strcmp(item,VDEFAULTBINSERVER.name) == 0)
                      {
                      DeleteItemStarting(&VHEAP,GROUPBUFF);
                      }
                   break;

   default:        yyerror("Software error");
                   FatalError("Unknown item type");
   }
}

/*******************************************************************/

HandleHomePattern(pattern)

char *pattern;

{
VBUFF[0]='\0';
ExpandVarstring(pattern,VBUFF,"");
AppendItem(&VHOMEPATLIST,VBUFF,NULL);
}

/*******************************************************************/

AppendNameServer(item)

char *item;

{ 
Debug1("Installing item (%s) in the nameserver list\n",item);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",item);
   return;
   }

AppendItem(&VRESOLVE,item,NULL);
}

/*******************************************************************/

AppendImport(item)

char *item;

{ 
if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",item);
   return;
   }

if (strcmp(item,VCURRENTFILE) == 0)
   {
   yyerror("A file cannot import itself");
   FatalError("Infinite self-reference in class inheritance");
   }

Debug1(">>Installing item (%s) in the import list\n",item);

AppendItem(&VIMPORT,item,NULL);
}


/*******************************************************************/

InstallHomeserverItem(item)

char *item;

{
if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",item);
   return;
   }

Debug1("Installing item (%s) in  list for homeservers (%s) in group (%s)\n",item,CLASSBUFF,GROUPBUFF);

AppendItem(&VHOMESERVERS,item,NULL);
}

/*******************************************************************/

InstallBinserverItem(item)           /* Install if matches classes */

char *item;

{
if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",item);
   return;
   }

AppendItem(&VBINSERVERS,item,NULL);
}

/*******************************************************************/

InstallMailserverPath(path)

char *path;

{
if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",path);
   return;
   }

if (VMAILSERVER[0] != '\0')
   {
   FatalError("Redefinition of mailserver");
   }

strcpy(VMAILSERVER,path);

Debug1("Installing mailserver (%s) for group (%s)",path,GROUPBUFF);
}

/*******************************************************************/

AppendScript(item)

char *item;

{
Debug1("Installing item (%s) in the script list\n",item);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",item);
   return;
   }

VBUFF[0]='\0';
ExpandVarstring(item,VBUFF,"");

if (VBUFF[0] != '/')
   {
   yyerror("scripts or commands must have absolute path names");
   return;
   }

AppendItem(&VSCRIPT,VBUFF,CLASSBUFF);
}

/*******************************************************************/

InstallLinkItem (from,to)

char *from, *to;

{ struct Link *ptr;
  char *spfrom,*spto,*spe,*sp;
  char buffer[bufsize];
  
Debug1("Storing Link: %s -> %s\n",from,to);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing link no match\n");
   return;
   }

bzero(VBUFF,bufsize);
ExpandVarstring(from,VBUFF,"");

bzero(buffer,bufsize);
ExpandVarstring(to,buffer,"");

if ((ptr = (struct Link *)malloc(sizeof(struct Link))) == NULL)
   {
   FatalError("Memory Allocation failed for InstallListItem() #1");
   }
if ((spfrom = malloc(strlen(VBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallListItem() #2");
   }
if ((spto = malloc(strlen(buffer)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallListItem() #3");
   }
if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallListItem() #4");
   }

if (VLINKTOP == NULL)                 /* First element in the list */
   {
   VLINK = ptr;
   }
else
   {
   VLINKTOP->next = ptr;
   }

strcpy (spfrom,VBUFF);
strcpy (spto,buffer);
strcpy (spe,CLASSBUFF);

if (strlen(spfrom) > 1)
   {
   DeleteSlash(spfrom);
   }

if (strlen(spto) > 1)
   {
   DeleteSlash(spto);
   }

ptr->classes = spe;
ptr->from = spfrom;
ptr->to = spto;
ptr->force = FORCELINK;
ptr->silent = LINKSILENT;
ptr->type = LINKTYPE;
ptr->copytype = COPYTYPE;
ptr->next = NULL;
ptr->copy = VCPLNPARSE;
ptr->exclusions = VEXCLUDEPARSE;
ptr->inclusions = VINCLUDEPARSE;
ptr->recurse = VRECURSE;
VLINKTOP = ptr;

if (ptr->recurse != 0)
   {
   yyerror("Recursion can only be used with +> multiple links");
   }

InitializeAction();
}

/*******************************************************************/

InstallLinkChildrenItem (from,to)

char *from, *to;

{ struct Link *ptr;
  char *spfrom,*spto, *spe, *sp;
  struct TwoDimList *tp = NULL;

Debug1("Storing Linkchildren item: %s -> %s\n",from,to);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing linkchildren no match\n");
   return;
   }

VBUFF[0]='\0';                                /* Expand any variables */
ExpandVarstring(from,VBUFF,"");

Build2DListFromVarstring(&tp,to,'/');
    
Set2DList(tp);

for (sp = Get2DListEnt(tp); sp != NULL; sp = Get2DListEnt(tp))
   {
   if ((ptr = (struct Link *)malloc(sizeof(struct Link))) == NULL)
      {
      FatalError("Memory Allocation failed for InstallListChildrenItem() #1");
      }

   if ((spfrom = malloc(strlen(VBUFF)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallLinkchildrenItem() #2");
      }

   if ((spto = malloc(strlen(sp)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallLinkChildrenItem() #3");
      }

   if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallLinkChildrenItem() #3");
      }

   if (VCHLINKTOP == NULL)                 /* First element in the list */
      {
      VCHLINK = ptr;
      }
   else
      {
      VCHLINKTOP->next = ptr;
      }

   strcpy (spfrom,VBUFF);
   strcpy (spto,sp);
   strcpy (spe,CLASSBUFF);

   ptr->classes = spe;
   ptr->from = spfrom;
   ptr->to = spto;
   ptr->force = FORCELINK;
   ptr->silent = LINKSILENT;
   ptr->type = LINKTYPE;
   ptr->next = NULL;
   ptr->copy = VCPLNPARSE;
   ptr->exclusions = VEXCLUDEPARSE;
   ptr->inclusions = VINCLUDEPARSE;
   ptr->recurse = VRECURSE;
   VCHLINKTOP = ptr;

   if (ptr->recurse != 0 && strcmp(spto,"linkchildren") == 0)
      {
      yyerror("Sorry don't know how to recurse with linkchildren keyword");
      }
   }

Delete2DList(tp);

InitializeAction();
}


/*******************************************************************/

InstallRequiredPath(path)

char *path;

{ 
Debug1("Installing item (%s) in the required list\n",path);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",path);
   return;
   }

VBUFF[0] = '\0';
ExpandVarstring(path,VBUFF,"");

AppendItem(&VREQUIRED,VBUFF,CLASSBUFF);
}

/*******************************************************************/

AppendMountable(path)

char *path;

{
Debug1("Adding mountable %s to list\n",path);

AppendItem(&VMOUNTABLES,path,CLASSBUFF);
}

/*******************************************************************/

AppendUmount(path)

char *path;

{ 
if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",path);
   return;
   }

Debug1("Adding unmount %s to list\n",path);
AppendItem(&VUNMOUNT,path,CLASSBUFF);
}

/*******************************************************************/

AppendMiscMount(from,onto,perm)

char *from, *onto, *perm;

{ struct MiscMount *ptr;
  char *sp1,*sp2,*sp3, *spe;

Debug1("Adding misc mountable %s %s (%s) to list\n",from,onto,perm);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",from);
   return;
   }

if ((ptr = (struct MiscMount *)malloc(sizeof(struct MiscMount))) == NULL)
   {
   FatalError("Memory Allocation failed for AppendMiscMount #1");
   }

if ((sp1 = malloc(strlen(from)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for AppendMiscMount() #2");
   }

if ((sp2 = malloc(strlen(onto)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for AppendMiscMount() #3");
   }

if ((sp3 = malloc(strlen(perm)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for AppendMiscMount() #4");
   }

if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for AppendMiscMount() #5");
   }

strcpy(sp1,from);
strcpy(sp2,onto);
strcpy(sp3,perm);
strcpy(spe,CLASSBUFF);

if (VMISCMOUNTTOP == NULL)                 /* First element in the list */
   {
   VMISCMOUNT = ptr;
   }
else
   {
   VMISCMOUNTTOP->next = ptr;
   }

ptr->classes = CLASSBUFF;
ptr->from = sp1;
ptr->onto = sp2;
ptr->options  = sp3;
ptr->next = NULL;
VMISCMOUNTTOP = ptr;
}


/*******************************************************************/

AppendIgnore(path)

char *path;

{ struct TwoDimList *tp = NULL;
  char *sp;

Debug1("Installing item (%s) in the ignore list\n",path);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",path);
   return;
   }

Build2DListFromVarstring(&tp,path,'/');
    
Set2DList(tp);

for (sp = Get2DListEnt(tp); sp != NULL; sp = Get2DListEnt(tp))
   {
   AppendItem(&VIGNORE,sp,CLASSBUFF);
   }

Delete2DList(tp);
}

/*******************************************************************/

InstallPending(action)

enum actions action;

{
if (ACTIONPENDING)
   {
   Debug1("\n   [BEGIN InstallPending %s\n",ACTIONTEXT[action]);
   }
else
   {
   Debug1("   (No actions pending in %s)\n",ACTIONTEXT[action]);
   return;
   }

switch (action)
   {
   case files:
                  InstallFileListItem(CURRENTPATH,PLUSMASK,MINUSMASK,FILEACTION,
				      VUIDNAME,VGIDNAME,VRECURSE,(char)PTRAVLINKS);
                  break;

   case processes: InstallProcessItem(EXPR,RESTART,PROMATCHES,PROCOMP,
				      PROSIGNAL,PROACTION,CLASSBUFF);
                   break;
   case image:
                  InstallImageItem(CURRENTPATH,PLUSMASK,MINUSMASK,DESTINATION,
				   IMAGEACTION,VUIDNAME,VGIDNAME,FORCECOPY,
				   IMAGEBACKUP,VRECURSE,COPYTYPE,LINKTYPE,SERVER);
                  break;

   case tidy:     if (VAGE >= 99999)
                     {
                     yyerror("Must specify an age for tidy actions");
                     return;
                     }
                  InstallTidyItem(CURRENTPATH,CURRENTITEM,VRECURSE,VAGE,(char)PTRAVLINKS,
				  TIDYSIZE,AGETYPE,LINKDIRS,TIDYDIRS,CLASSBUFF);
                  break;

   case makepath: InstallMakePath(CURRENTPATH,PLUSMASK,MINUSMASK,VUIDNAME,VGIDNAME);
                  break;

   case disable:  AppendDisable(CURRENTPATH,CURRENTITEM,ROTATE);
                  break;

   case links:

                  if (LINKTO[0] == '\0')
                     {
                     return;
                     }

                  if (ACTION_IS_LINKCHILDREN)
                     {
                     InstallLinkChildrenItem(LINKFROM,LINKTO);
                     ACTION_IS_LINKCHILDREN = false;
                     }
                  else if (ACTION_IS_LINK)
                     {
                     InstallLinkItem(LINKFROM,LINKTO);
                     ACTION_IS_LINK = false;
                     }
                  else
                     {
                     return;                                   /* Don't have whole command */
                     }

                  break;
   }

LINKFROM[0] = '\0';
LINKTO[0] = '\0';
ACTIONPENDING = false;
CURRENTITEM[0] = '\0';
CURRENTITEM[0] = '\0';
VRECURSE = 0;
VAGE=99999;
Debug1("   END InstallPending]\n\n");
}

/*******************************************************************/
/* Level 3                                                         */
/*******************************************************************/

InstallTidyItem (path,wild,rec,age,travlinks,tidysize,type,ldirs,tidydirs,classes)

char *wild, *path;
short age,tidysize,tidydirs;
int rec, travlinks;
char type, ldirs, *classes;

{ struct TwoDimList *tp = NULL;
  char *sp;

Build2DListFromVarstring(&tp,path,'/');
   
Set2DList(tp);

for (sp = Get2DListEnt(tp); sp != NULL; sp = Get2DListEnt(tp))
   {
   if (TidyPathExists(path))
      {
      AddTidyItem(sp,wild,rec,age,travlinks,tidysize,type,ldirs,tidydirs,classes);
      }
   else
      {
      InstallTidyPath(sp,wild,rec,age,travlinks,tidysize,type,ldirs,tidydirs,classes);
      }
   }

Delete2DList(tp);
}

/*******************************************************************/

AppendDisable(path,type,rotate)

char *path, *type;
short rotate;

{ char *sp, *spe, *sp2;
  char buf[bufsize];
  struct Disable *ptr;
  int i;

Debug1("Installing item (%s) in the disable list\n",path);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing %s, no match\n",path);
   return;
   }

if (strlen(type) > 0 && strcmp(type,"plain") != 0 && strcmp(type,"file") !=0 && strcmp(type,"link") !=0
    && strcmp(type,"links") !=0 )
   {
   yyerror("Invalid file type in Disable");
   }

for (i=0; i<bufsize; i++)
   {
   buf[i] = VBUFF[i] = '\0';
   }

ExpandVarstring(path,buf,"");

if ((ptr = (struct Disable *)malloc(sizeof(struct Disable))) == NULL)
   {
   FatalError("Memory Allocation failed for AppendDisable() #1");
   }

if ((sp = malloc(strlen(buf)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for AppendDisable() #2");
   }

if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for AppendDisable() #3");
   }

if (strlen(type) == 0)
   {
   sprintf(VBUFF,"all");
   }
else
   {
   sprintf(VBUFF,"%s",type);
   }

if ((sp2 = malloc(strlen(VBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for AppendDisable() #4");
   }

if (VDISABLETOP == NULL)                 /* First element in the list */
   {
   VDISABLELIST = ptr;
   }
else
   {
   VDISABLETOP->next = ptr;
   }

strcpy (sp,buf);
strcpy (spe,CLASSBUFF);
strcpy (sp2,VBUFF);

ptr->name = sp;
ptr->type = sp2;
ptr->classes = spe;
ptr->rotate = rotate;
ptr->next = NULL;
VDISABLETOP = ptr;
InitializeAction();
}

/*******************************************************************/

InstallMakePath(path,plus,minus,uidnames,gidnames)

char *path;
mode_t plus,minus;
char *uidnames;
char *gidnames;

{ struct File *ptr;
  char *sp, *spe;

Debug1("InstallMakePath (%s) (+%o)(-%o)(%s)(%s)\n",path,plus,minus,uidnames,gidnames);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing directory item, no match\n");
   return;
   }

VBUFF[0]='\0';                                /* Expand any variables */
ExpandVarstring(path,VBUFF,"");

if ((ptr = (struct File *)malloc(sizeof(struct File))) == NULL)
   {
   FatalError("Memory Allocation failed for InstallMakepath() #1");
   }

if ((sp = malloc(strlen(VBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallMakepath() #2");
   }

if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallMakepath() #3");
   }

if (VMAKEPATHTOP == NULL)                 /* First element in the list */
   {
   VMAKEPATH = ptr;
   }
else
   {
   VMAKEPATHTOP->next = ptr;
   }

strcpy (sp,VBUFF);
strcpy (spe,CLASSBUFF);

ptr->classes = spe;
ptr->path = sp;
ptr->plus = plus;
ptr->minus = minus;
ptr->recurse = 0;
ptr->action = fixdirs;
ptr->uid = MakeUidList(uidnames);
ptr->gid = MakeGidList(gidnames);
ptr->next = NULL;
VMAKEPATHTOP = ptr;
InitializeAction();
}

/*******************************************************************/

HandleTravLinks(value)

char *value;

{
if (ACTION == tidy && strncmp(CURRENTPATH,"home",4) == 0)
   {
   yyerror("Can't use links= option with special variable home in tidy");
   yyerror("Use command line options instead.\n");
   }

if (PTRAVLINKS != '?')
   {
   Warning("redefinition of links= option");
   }

if ((strcmp(value,"stop") == 0) || (strcmp(value,"false") == 0))
   {
   PTRAVLINKS = (short) 'F';
   return;
   }

if ((strcmp(value,"traverse") == 0) || (strcmp(value,"follow") == 0) || (strcmp(value,"true") == 0))
   {
   PTRAVLINKS = (short) 'T';
   return;
   }

if ((strcmp(value,"tidy"))==0)
   {
   PTRAVLINKS = (short) 'K';
   return;
   }

yyerror("Illegal links= specifier");
}

/*******************************************************************/

HandleTidySize(value)

char *value;

{int num = -1;

if (strcmp(value,"empty") == 0)
   {
   TIDYSIZE = CF_EMPTYFILE;
   }
else
   {
   sscanf(value,"%d",&num);

   if (num <= 0)
      {
      yyerror("disable/rotate value must be a decimal number >= zero or keyword empty");
      }

   TIDYSIZE = (short) num;
   }
}

/*******************************************************************/

HandleTidyType(value)

char *value;

{
if (strcmp(value,"a")== 0 || strcmp(value,"atime") == 0)
   {
   AGETYPE = 'a';
   return;
   }

if (strcmp(value,"m")== 0 || strcmp(value,"mtime") == 0)
   {
   AGETYPE = 'm';
   return;
   }

if (strcmp(value,"c")== 0 || strcmp(value,"ctime") == 0)
   {
   AGETYPE = 'c';
   return;
   }

yyerror("Illegal age search type, must be atime/ctime/mtime");
}

/*******************************************************************/

HandleTidyLinkDirs(value)

char *value;

{
if (strcmp(value,"keep")== 0)
   {
   LINKDIRS = 'k';
   return;
   }

if (strcmp(value,"tidy")== 0 || strcmp(value,"delete") == 0)
   {
   LINKDIRS = 't';
   return;
   }

yyerror("Illegal linkdirs value, must be keep/delete/tidy");
}

/*******************************************************************/

HandleTidyRmdirs(value)

char *value;

{
if (strcmp(value,"true") == 0)
   {
   TIDYDIRS = true;
   return;
   }

if (strcmp(value,"false") == 0)
   {
   TIDYDIRS = false;
   return;
   }

yyerror("Illegal rmdirs value, must be true/false");
}

/*******************************************************************/

HandleForceCopy(value)

char *value;

{
if (strcmp(value,"true") == 0 || strcmp(value,"on") == 0)
   {
   FORCECOPY = true;
   return;
   }

if (strcmp(value,"false") == 0 || strcmp(value,"off") == 0)
   {
   FORCECOPY = false;
   return;
   }

yyerror("Illegal copy attribute for force= ");
}

/*******************************************************************/

HandleCopyBackup(value)

char *value;

{
if (strcmp(value,"true") == 0 || strcmp(value,"on") == 0)
   {
   IMAGEBACKUP = true;
   return;
   }

if (strcmp(value,"false") == 0 || strcmp(value,"off") == 0)
   {
   IMAGEBACKUP = false;
   return;
   }

yyerror("Illegal copy attribute for force= ");
}

/*******************************************************************/

GetFileAction(action)

char *action;

{ int i;

for (i = 0; FILEACTIONTEXT[i] != '\0'; i++)
   {
   if (strcmp(action,FILEACTIONTEXT[i]) == 0)
      {
      return i;
      }
   }

yyerror("Unknown action type");
return (int) warnall;
}


/*******************************************************************/

InstallFileListItem(path,plus,minus,action,uidnames,gidnames,recurse,travlinks)

char *path;
mode_t plus,minus;
enum fileactions action;
char *uidnames;
char *gidnames;
int recurse;
char travlinks;

{ struct File *ptr;
  char *sp, *spe, *spl;
  struct TwoDimList *tp = NULL;

Debug1("InstallFileaction (%s) (+%o)(-%o) (%s) (%d) (%c)\n",path,plus,minus,FILEACTIONTEXT[action],action,travlinks);

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing file item, no match\n");
   return;
   }

Build2DListFromVarstring(&tp,path,'/');
    
Set2DList(tp);

for (spl = Get2DListEnt(tp); spl != NULL; spl = Get2DListEnt(tp))
   {
   if ((ptr = (struct File *)malloc(sizeof(struct File))) == NULL)
      {
      FatalError("Memory Allocation failed for InstallFileListItem() #1");
      }

   if ((sp = malloc(strlen(spl)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallFileListItem() #2");
      }

   if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallFileListItem() #3");
     }

   if (VFILETOP == NULL)                 /* First element in the list */
      {
      VFILE = ptr;
      }
   else
      {
      VFILETOP->next = ptr;
      }

   strcpy (sp,spl);
   strcpy (spe,CLASSBUFF);

   ptr->classes = spe;
   ptr->path = sp;
   ptr->action = action;
   ptr->plus = plus;
   ptr->minus = minus;
   ptr->recurse = recurse;
   ptr->uid = MakeUidList(uidnames);
   ptr->gid = MakeGidList(gidnames);
   ptr->travlinks = travlinks;
   ptr->next = NULL;
   VFILETOP = ptr;
   }

Delete2DList(tp);

InitializeAction();
}


/*******************************************************************/

InstallProcessItem(expr,restart,matches,comp,signal,action,classes)

char *expr, *restart, *classes;
short matches, signal;
char action, comp;

{ struct Process *ptr;
  char *sp1, *sp2, *spe;
  char buf1[bufsize], buf2[bufsize];

if (comp == 't')
   {
   comp = '=';
   }

Debug1("InstallProcessItem(%s,%s,%d,%d,%c)\n",expr,restart,matches,signal,action);

bzero(buf1,bufsize);
bzero(buf2,bufsize);

ExpandVarstring(expr,buf1,"");
ExpandVarstring(restart,buf2,"");

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing process item, no match\n");
   return;
   }

if ((ptr = (struct Process *)malloc(sizeof(struct Process))) == NULL)
   {
   FatalError("Memory Allocation failed for InstallProcItem() #1");
   }

if ((sp1 = malloc(strlen(buf1)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallProcItem() #2");
   }

if ((sp2 = malloc(strlen(buf2)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallProcItem() #3");
   }

if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallProcItem() #4");
   }


if (VPROCTOP == NULL)                 /* First element in the list */
   {
   VPROCLIST = ptr;
   }
else
   {
   VPROCTOP->next = ptr;
   }

strcpy (sp1,buf1);
strcpy (sp2,buf2);
strcpy (spe,CLASSBUFF);

ptr->classes = spe;
ptr->expr = sp1;
ptr->restart = sp2;
ptr->matches = matches;
ptr->comp = comp;
ptr->signal = signal;
ptr->action = action;
ptr->next = NULL;
VPROCTOP = ptr;
InitializeAction();
}

/*******************************************************************/

InstallImageItem(path,plus,minus,destination,action,uidnames,gidnames,
		 force,backup,rec,type,lntype,server)

char *path, *destination, *action, *server;
mode_t plus,minus;
char *uidnames;
char *gidnames;
short force;
short backup;
char type, lntype;
int rec;

{ struct Image *ptr;
  char *sp, *spe, *spd, *spa, *spl, *sps;
  char buf1[bufsize], buf2[bufsize], buf3[bufsize];
  struct TwoDimList *tp = NULL;
  int i;

Debug1("InstallImageItem (%s) (+%o)(-%o) (%s)\n",path,plus,minus,destination);

if (strlen(destination) == 0)
   {
   yyerror("No destination specified in file image declaration");
   return;
   }

if (strlen(action) == 0)   /* default action */
   {
   strcat(action,"fix");
   }

if (!(strcmp(action,"silent") == 0 || strcmp(action,"warn") == 0 || strcmp(action,"fix") == 0))
   {
   sprintf(VBUFF,"Illegal action in image/copy item: %s",action);
   yyerror(VBUFF);
   return;
   }

bzero(buf1,bufsize);
bzero(buf2,bufsize);
bzero(buf3,bufsize);

ExpandVarstring(path,buf1,"");
ExpandVarstring(destination,buf2,"");
ExpandVarstring(server,buf3,"");

if (strlen(buf1) > 1)
   {
   DeleteSlash(buf1);
   }

if (strlen(buf2) > 1)
   {
   DeleteSlash(buf2);
   }

if (buf2[0] != '/')
   {
   if (strncmp(buf2,"home",4) == 0)
      {
      if (strlen(buf2) > 4 && buf2[4] != '/')
         {
         yyerror("illegal use of home or not absolute pathname");
         return;
         }
      }
   else
      {
      yyerror("Image needs an absolute pathname");
      return;
      }
   }

Build2DListFromVarstring(&tp,path,'/');
    
Set2DList(tp);

for (spl = Get2DListEnt(tp); spl != NULL; spl = Get2DListEnt(tp))
   {
   if (strcmp(spl,buf2) == 0)
      {
      yyerror("image loop: file/dir copies to itself");
      return;
      }

   if ( ! IsInstallable(CLASSBUFF))
      {
      Debug1("Not installing file item, no match\n");
      return;
      }

   if ((ptr = (struct Image *)malloc(sizeof(struct Image))) == NULL)
      {
      FatalError("Memory Allocation failed for InstallImageItem() #1");
      }

   if ((sp = malloc(strlen(spl)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallImageItem() #2");
      }

   if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallImageItem() #3");
      }

   if ((spd = malloc(strlen(buf2)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallImageItem() #4");
      }

   if ((sps = malloc(strlen(buf3)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallImageItem() #5");
      }

   if ((spa = malloc(strlen(action)+1)) == NULL)
      {
      FatalError("Memory Allocation failed for InstallImageItem() #6");
      }

   if (VIMAGETOP == NULL)
      {
      VIMAGE = ptr;
      }
   else
      {
      VIMAGETOP->next = ptr;
      }

   strcpy (sp,spl);
   strcpy (spe,CLASSBUFF);
   strcpy (spd,buf2);
   strcpy (spa,action);
   strcpy (sps,server);

   ptr->classes = spe;
   ptr->server = sps;
   ptr->path = sp;
   ptr->destination = spd;
   ptr->action=spa;
   ptr->plus = plus;
   ptr->minus = minus;
   ptr->uid = MakeUidList(uidnames);
   ptr->gid = MakeGidList(gidnames);
   ptr->force = force;
   ptr->next = NULL;
   ptr->backup = backup;
   ptr->recurse = rec;
   ptr->type = type;
   ptr->linktype = lntype;
   ptr->symlink = VCPLNPARSE;
   ptr->exclusions = VEXCLUDEPARSE;
   ptr->inclusions = VINCLUDEPARSE;
   VIMAGETOP = ptr;
   }


Delete2DList(tp);

InitializeAction();
}

/*******************************************************************/

GetCommAttribute(s)

char *s;

{ int i;
  char comm[maxvarsize];

for (i = 0; s[i] != '\0'; i++)
   {
   s[i] = ToLower(s[i]);
   }

comm[0]='\0';

sscanf(s,"%[^=]",comm);

Debug1("GetCommAttribute(%s)\n",comm);

for (i = 0; COMMATTRIBUTES[i] != '\0'; i++)
   {
   if (strncmp(COMMATTRIBUTES[i],comm,strlen(comm)) == 0)
      {
      Debug1("GetCommAttribute - got: %s\n",COMMATTRIBUTES[i]);
      return i;
      }
   }

return cfbad;
}

/*******************************************************************/

HandleRecurse(value)

char *value;

{ int n = -1;

if (strcmp(value,"inf") == 0)
   {
   VRECURSE = INFINITERECURSE;
   }
else
   {
   if (strncmp(CURRENTPATH,"home",4) == 0)
      {
      yyerror("Recursion is always infinite for home");
      return;
      }

   sscanf(value,"%d",&n);

   if (n == -1)
      {
      yyerror("Illegal recursion specifier");
      }
   else
      {
      VRECURSE = n;
      }
   }
}

/*******************************************************************/

HandleCopyType(value)

char *value;

{
if (strcmp(value,"ctime") == 0)
   {
   Debug1("Set copy by ctime\n");
   COPYTYPE = 't';
   }
else if (strcmp(value,"checksum")==0 || strcmp(value,"sum") == 0)
   {
   Debug1("Set copy by md5 checksum\n");
   COPYTYPE = 'c';
   }
}

/*******************************************************************/

HandleDisableFileType(value)

char *value;

{
if (strlen(CURRENTITEM) != 0)
   {
   Warning("Redefinition of filetype in disable");
   }

if (strcmp(value,"link") == 0 || strcmp(value,"links") == 0)
   {
   strcpy(CURRENTITEM,"link");
   }
else if (strcmp(value,"plain") == 0 || strcmp(value,"file") == 0)
   {
   strcpy(CURRENTITEM,"file");
   }
else
   {
   yyerror("Disable filetype unknown");
   }
}

/*******************************************************************/

HandleDisableRotate(value)

char *value;

{ int num = 0;

if (strcmp(value,"empty") == 0 || strcmp(value,"truncate") == 0)
   {
   ROTATE = CF_TRUNCATE;
   }
else
   {
   sscanf(value,"%d",&num);

   if (num == 0)
      {
      yyerror("disable/rotate value must be a decimal number greater than zero or keyword empty");
      }

   if (! SILENT && num > 99)
      {
      Warning("rotate value looks silly");
      }

   ROTATE = (short) num;
   }
}

/*******************************************************************/

HandleAge(days)

char *days;

{ 
sscanf(days,"%d",&VAGE);
Debug1("HandleAge(%d)\n",VAGE);
}

/*******************************************************************/

HandleProcessMatches(value)

char *value;

{ int i = 0;

switch (*value)
   {
   case '>': PROCOMP = '>';
             value++;
             break;
   case '<': PROCOMP = '<';
             value++;
             break;
   default : PROCOMP = '=';
   }

sscanf(value,"%d",&i);

if (i < 1)
   {
   yyerror("matches attribute with silly value (must be > 0)");
   }

PROMATCHES = (short) i;
}

/*******************************************************************/

HandleProcessSignal(value)

char *value;

{ int i;
  char *sp;

for (i = 1; SIGNALS[i] != 0; i++)
   {
   for (sp = value; *sp != '\0'; sp++)
      {
      *sp = ToUpper(*sp);
      }
   
   if (strcmp(SIGNALS[i]+3,value) == 0)  /* 3 to cut off "sig" */
      {
      PROSIGNAL = (short) i;
      return;
      }
   }

i = 0;

sscanf(value,"%d",&i);

if (i < 1 && i > highest_signal)
   {
   yyerror("Unknown signal in attribute");
   }

PROSIGNAL = (short) i;
}

/*******************************************************************/

AppendToActionSequence (action)

char *action;

{ int j = 0;
  char *sp,cbuff[bufsize],actiontxt[bufsize];

Debug1("Installing item (%s) in the action sequence list\n",action);

AppendItem(&VACTIONSEQ,action,NULL);

cbuff[0]='\0';
actiontxt[0]='\0';
sp = action;

while (*sp != '\0')
   {
   ++j;
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
      yyerror("Error in action sequence: %s\n",action);
      yyerror("You cannot add a reserved class!");
      return;
      }
 
   if (j == 1)
      {
      strcpy(actiontxt,cbuff);
      continue;
      }
   else if (! IsItemIn(VALLADDCLASSES,cbuff))
      {
      AppendItem(&VALLADDCLASSES,cbuff,NULL);
      }
   }
}

/*******************************************************************/

AppendToAccessList (user)

char *user;

{ char id[maxvarsize];
  struct passwd *pw;

Debug1("Adding to access list for %s\n",user);

if (isalpha(user[0]))
   {
   if ((pw = getpwnam(user)) == NULL)
      {
      yyerror("No such user in password database");
      return;
      }

   sprintf(id,"%d",pw->pw_uid);
   AppendItem(&VACCESSLIST,id,NULL);
   }
else
   {
   AppendItem(&VACCESSLIST,user,NULL);
   }
}

/*******************************************************************/

HandleLinkAction(value)

char *value;

{
if (strcmp(value,"silent") == 0)
   {
   LINKSILENT = true;
   return;
   }

yyerror("Invalid link action");
}

/*******************************************************************/

HandleLinkType(value)

char *value;

{
if (strcmp(value,"hard") == 0)
   {
   if (ACTION_IS_LINKCHILDREN)
      {
      yyerror("hard links inappropriate for multiple linkage");
      }

   if (ACTION == image)
      {
      yyerror("hard links inappropriate for copy operation");
      }

   LINKTYPE = 'h';
   return;
   }

if (strcmp(value,"symbolic") == 0 || strcmp(value,"sym") == 0)
   {
   LINKTYPE = 's';
   return;
   }

if (strcmp(value,"abs") == 0 || strcmp(value,"absolute") == 0)
   {
   LINKTYPE = 'a';
   return;
   }

if (strcmp(value,"rel") == 0 || strcmp(value,"relative") == 0)
   {
   LINKTYPE = 'r';
   return;
   }
yyerror("Invalid link type");
}

/*******************************************************************/

HandleServer(value)

char *value;

{ struct hostent *hp;

Debug("Server in copy set to : %s\n",value);
strcpy(SERVER,value);
}

/*******************************************************************/

ShellCommandReturnsZero(comm)

char *comm;

{ int status, i, argc;
  pid_t pid;
  char arg[maxshellargs][bufsize];
  char **argv;

/* Build argument array */

for (i = 0; i < maxshellargs; i++)
   {
   bzero (arg[i],bufsize);
   }

argc = SplitCommand(comm,arg);

if ((pid = fork()) < 0)
   {
   FatalError("Failed to fork new process");
   }
else if (pid == 0)                     /* child */
   {
   argv = (char **) malloc((argc+1)*sizeof(char *));

   for (i = 0; i < argc; i++)
      {
      argv[i] = arg[i];
      }

   argv[i] = (char *) NULL;

   if (execv(arg[0],argv) == -1)
      {
      yyerror("script failed");
      perror("execvp");
      exit(1);
      }
   }
else                                    /* parent */
   {
   if (wait(&status) != pid)
      {
      printf("cfengine: Wait for child failed\n");
      perror("wait");
      return false;
      }
   else
      {
      if (WIFSIGNALED(status))
         {
         printf("cfengine: script %s returned: %s\n",comm,WTERMSIG(status));
         return false;
         }

      if (! WIFEXITED(status))
         {
         return false;
         }

      if (WEXITSTATUS(status) == 0)
         {
         return true;
         }
      else
         {
         return false;
         }
      }
   }

}

/*******************************************************************/
/* Level 4                                                         */
/*******************************************************************/

struct UidList *MakeUidList(uidnames)

char *uidnames;

{ struct UidList *uidlist;
  struct Item *ip, *tmplist;
  char uidbuff[bufsize];
  char *sp;
  int offset;
  struct passwd *pw;
  char *machine, *user, *domain;
  int uid;
  int tmp;

uidlist = NULL;

for (sp = uidnames; *sp != '\0'; sp+=strlen(uidbuff))
   {
   if (*sp == ',')
      {
      sp++;
      }

   if (sscanf(sp,"%[^,]",uidbuff))
      {
      if (uidbuff[0] == '+')        /* NIS group - have to do this in a roundabout     */
         {                          /* way because calling getpwnam spoils getnetgrent */
         offset = 1;
         if (uidbuff[1] == '@')
            {
            offset++;
            }

         setnetgrent(uidbuff+offset);
         tmplist = NULL;

         while (getnetgrent(&machine,&user,&domain))
            {
            if (user != NULL)
               {
               AppendItem(&tmplist,user,NULL);
               }
            }
                   
         endnetgrent();

         for (ip = tmplist; ip != NULL; ip=ip->next)
            {
            if ((pw = getpwnam(ip->name)) == NULL)
               {
               if (VERBOSE || DEBUG || D1)
                  {
                  sprintf(CURRENTITEM,"Unknown user [%s]\n",ip->name);
                  Warning(CURRENTITEM);
                  sprintf(CURRENTITEM,"User in netgroup [%s] not in passwd file\n",uidbuff+offset);
                  Warning(CURRENTITEM);
                  }
               continue;
               }
            else
               {
               uid = pw->pw_uid;
               }
            AddSimpleUidItem(&uidlist,uid); 
            }

         DeleteItemList(tmplist);
         continue;
         }

      if (isdigit(uidbuff[0]))
         {
         sscanf(uidbuff,"%d",&tmp);
         uid = (uid_t)tmp;
         }
      else
         {
         if (strcmp(uidbuff,"*") == 0)
            {
            uid = -1;                     /* signals wildcard */
            }
         else if ((pw = getpwnam(uidbuff)) == NULL)
            {
            printf("Unknown user %s\n",uidbuff);
            Warning("User is not known in this passwd domain");
            continue;
            }
         else
            {
            uid = pw->pw_uid;
            }
         }
      AddSimpleUidItem(&uidlist,uid);
      }
   }

return (uidlist);
}

/*********************************************************************/

struct GidList *MakeGidList(gidnames)

char *gidnames;

{ struct GidList *gidlist;
  char gidbuff[bufsize];
  char err[bufsize];
  char *sp;
  struct group *gr;
  int gid;
  int tmp;

gidlist = NULL;

for (sp = gidnames; *sp != '\0'; sp+=strlen(gidbuff))
   {
   if (*sp == ',')
      {
      sp++;
      }

   if (sscanf(sp,"%[^,]",gidbuff))
      {
      if (isdigit(gidbuff[0]))
         {
         sscanf(gidbuff,"%d",&tmp);
         gid = (gid_t)tmp;
         }
      else
         {
         if (strcmp(gidbuff,"*") == 0)
            {
            gid = -1;                     /* signals wildcard */
            }
         else if ((gr = getgrnam(gidbuff)) == NULL)
            {
            sprintf(err,"Unknown group %s\n",gidbuff);
            Warning(err);
            continue;
            }
         else
            {
            gid = gr->gr_gid;
            }
         }
      AddSimpleGidItem(&gidlist,gid);
      }
   }


return(gidlist);
}

/*******************************************************************/

SplitCommand(comm,arg)

char *comm, arg[maxshellargs][bufsize];

{ char *sp;
  int i = 0, j;
  char buff[bufsize];

for (sp = comm; *sp != '\0'; sp++)
   {
   bzero(buff,bufsize);

   if (i >= maxshellargs-1)
      {
      yyerror("Too many arguments in embedded script");
      FatalError("Use a wrapper");
      }

   while (*sp == ' ' || *sp == '\t')
      {
      sp++;
      }

   switch (*sp)
      {
      case '\"': sscanf (++sp,"%[^\"]",buff);
                 break;
      case '\'': sscanf (++sp,"%[^\']",buff);
                 break;
      case '`':  sscanf (++sp,"%[^`]",buff);
	         break;
      default:   sscanf (sp,"%s",buff);
                 break;
      }

   for (j = 0; j < bufsize; j++)
      {
      arg[i][j] = buff[j];
      }

   sp += strlen(arg[i]);
   i++;
   }
return (i);
}

/*******************************************************************/
/* Level 5                                                         */
/*******************************************************************/

AddSimpleUidItem(uidlist,uid)

struct UidList **uidlist;
int uid;

{ struct UidList *ulp, *u;

if ((ulp = (struct UidList *)malloc(sizeof(struct UidList))) == NULL)
   {
   FatalError("cfengine: malloc() failed #1 in AddSimpleUidItem()");
   }

ulp->uid = uid;
ulp->next = NULL;

if (*uidlist == NULL)
   {
   *uidlist = ulp;
   }
else
   {
   for (u = *uidlist; u->next != NULL; u = u->next)
      {
      }
   u->next = ulp;
   }
}

/*******************************************************************/

AddSimpleGidItem(gidlist,gid)

struct GidList **gidlist;
int gid;

{ struct GidList *glp,*g;

if ((glp = (struct GidList *)malloc(sizeof(struct GidList))) == NULL)
   {
   FatalError("cfengine: malloc() failed #1 in AddSimpleGidItem()");
   }
 
glp->gid = gid;
glp->next = NULL;

if (*gidlist == NULL)
   {
   *gidlist = glp;
   }
else
   {
   for (g = *gidlist; g->next != NULL; g = g->next)
      {
      }
   g->next = glp;
   }

}


/*******************************************************************/
/* Toolkits Misc                                                   */
/*******************************************************************/

InitializeAction()                                   /* Set defaults */

{
Debug1("InitializeAction()\n");

PLUSMASK = 0;
MINUSMASK = 0;
VRECURSE = 0;
VAGE = 99999;
VUIDNAME[0] = '*';
VUIDNAME[1] = '\0';
VGIDNAME[0] = '*';
VGIDNAME[1] = '\0';
HAVE_RESTART = 0;
FILEACTION=warnall;
CURRENTPATH[0] = '\0';
CURRENTITEM[0] = '\0';
ACTIONPENDING = false;
DESTINATION[0] = '\0';
IMAGEACTION[0] = '\0';
PTRAVLINKS = (short) '?';
FORCECOPY = false;
IMAGEBACKUP = true;
ROTATE=0;
TIDYSIZE=0;
PROMATCHES=-1;
AGETYPE='a';
COPYTYPE = 't';
LINKDIRS = 'k';
TIDYDIRS = false;
VEXCLUDEPARSE = '\0';
VINCLUDEPARSE = '\0';
VCPLNPARSE = '\0';
strcpy(SERVER,"localhost");

 /* Make sure we don't clean the buffer in the middle of a link! */

if ( ! ACTION_IS_LINK && ! ACTION_IS_LINKCHILDREN)
   {
   LINKFROM[0] = '\0';
   LINKTO[0] = '\0';
   LINKSILENT = false;
   LINKTYPE = 's';
   }
}

/*********************************************************************/

SetMountPath (value)

char *value;

{ char buff[bufsize];

bzero(buff,bufsize);

ExpandVarstring(value,buff,"");

Debug("Appending [%s] to mountlist\n",value);

AppendItem(&VMOUNTLIST,buff,CLASSBUFF);
}

/*********************************************************************/

SetRepository (value)

char *value;

{
if (*value != '/')
   {
   yyerror("File repository must be an absolute directory name");
   }

if (VREPOSITORY[0] != '\0')
   {
   yyerror("Redefinition of system variable repository");
   }

ExpandVarstring(value,VREPOSITORY,"");
}

/* EOF */
