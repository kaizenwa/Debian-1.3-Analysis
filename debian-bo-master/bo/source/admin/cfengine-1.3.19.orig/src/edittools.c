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
 

/********************************************************************/
/*                                                                  */
/* EDITING of simple textfiles (Toolkit)                            */
/*                                                                  */
/********************************************************************/

#include "cf.defs.h"
#include "cf.extern.h"

/********************************************************************/
/* EDIT Data structure routines                                     */
/********************************************************************/

EditFileExists(file)

char *file;

{ struct Edit *ptr;

VBUFF[0]='\0';                         /* Expand any variables */
ExpandVarstring(file,VBUFF,"");

for (ptr=VEDITLIST; ptr != NULL; ptr=ptr->next)
   {
   if (strcmp(ptr->fname,VBUFF) == 0)
      {
      return true;
      }
   }
return false;
}

/********************************************************************/

InstallEditFile(file,edit,data)

char *file,*edit,*data;

{ char *filename,*datstring;
  struct Edit *ptr;

if (data == NULL)
   {
   Debug1("InstallEditFile(%s,%s,-)\n",file,edit);
   }
else
   {
   Debug1("InstallEditFile(%s,%s,%s)\n",file,edit,data);
   }

if ( ! IsInstallable(CLASSBUFF))
   {
   if (DEBUG || D1) printf("Not installing Edit no match\n");
   return;
   }

VBUFF[0]='\0';                         /* Expand any variables */
ExpandVarstring(file,VBUFF,"");

if ((ptr = (struct Edit *)malloc(sizeof(struct Edit))) == NULL)
   {
   FatalError("Memory Allocation failed for InstallEditFile() #1");
   }

if ((filename = malloc(strlen(VBUFF)+1)) == NULL)
   {
   FatalError("Memory Allocation failed for InstallEditFile() #2");
   }

if (VEDITLISTTOP == NULL)                 /* First element in the list */
   {
   VEDITLIST = ptr;
   }
else
   {
   VEDITLISTTOP->next = ptr;
   }

if (strncmp(VBUFF,"home",4) && strlen(VBUFF) < 6)
   {
   yyerror("Can't edit home directories: missing a filename after home");
   }

strcpy (filename,VBUFF);

ptr->fname=filename;
ptr->next = NULL;
ptr->actions = NULL;
VEDITLISTTOP = ptr;
AddEditAction(file,edit,data);
}

/********************************************************************/

AddEditAction(file,edit,data)

char *file,*edit,*data;

{ struct Edit *ptr;
  struct Edlist *top,*new;
  char varbuff[bufsize];
  char *datstring, *spe;

if (data == NULL)
   {
   Debug2("AddEditAction(%s,%s,-)\n",file,edit);
   }
else
   {
   Debug2("AddEditAction(%s,%s,%s)\n",file,edit,data);
   }

if ( ! IsInstallable(CLASSBUFF))
   {
   Debug1("Not installing Edit no match\n");
   return;
   }

for (ptr = VEDITLIST; ptr != NULL; ptr=ptr->next)
   {
   varbuff[0] = '\0';
   ExpandVarstring(file,varbuff,"");

   if (strcmp(ptr->fname,varbuff) == 0)
      {
      if ((new = (struct Edlist *)malloc(sizeof(struct Edlist))) == NULL)
         {
         FatalError("Memory Allocation failed for AddEditAction() #1");
         }

      if (ptr->actions == NULL)
         {
         ptr->actions = new;
         }
      else
         {
         for (top = ptr->actions; top->next != NULL; top=top->next)
            {
            }
         top->next = new;
         }

      if (data == NULL)
         {
         new->data = NULL;
         }
      else
         {
         VBUFF[0]='\0';                         /* Expand any variables */
         ExpandVarstring(data,VBUFF,"");

         if ((datstring = malloc(strlen(VBUFF)+1)) == NULL)
            {
            FatalError("Memory Allocation failed for AddEditAction() #1");
            }

         strcpy(datstring,VBUFF);
         new->data = datstring;
         }

      new->next = NULL;

      if ((spe = malloc(strlen(CLASSBUFF)+1)) == NULL)
         {
         FatalError("Memory Allocation failed for InstallEditFile() #3");
         }

      strcpy (spe,CLASSBUFF);
      new->classes=spe;

      if ((new->code = EditActionsToCode(edit)) == NoEdit)
         {
         yyerror("Unknown edit action");
         }

      switch(new->code)
         {
         case BeginGroupIfNoMatch:
         case BeginGroupIfNoLineMatching:
         case BeginGroupIfNoSuchLine:
                EDITGROUPLEVEL++;
                break;
         case EndGroup:
                EDITGROUPLEVEL--;
                break;
         case ReplaceAll:
                SEARCHREPLACELEVEL++;
                break;
         case With:
                SEARCHREPLACELEVEL--;
                break;
	 case FixEndOfLine:
	        if (strlen(data) > extra_space - 1)
		   {
		   yyerror("End of line type is too long!");
		   printf("          (max %d characters allowed)\n",extra_space);
		   }
         }

      return;
      }
   }

printf("cfengine: software error - no file matched installing %s edit\n",file);
}

/********************************************************************/

enum editnames EditActionsToCode(edit)

char *edit;

{ int i;

Debug2("EditActionsToCode(%s)\n",edit);

for (i = 0; VEDITNAMES[i] != '\0'; i++)
   {
   if (strcmp(VEDITNAMES[i],edit) == 0)
      {
      return (enum editnames) i;
      }
   }

return (NoEdit);
}

/********************************************************************/

DoEditHomeFiles(ptr)

struct Edit *ptr;

{ DIR *dirh, *dirh2;
  struct dirent *dirp, *dirp2;
  char username[maxvarsize], *sp;
  char homedir[bufsize],dest[bufsize];
  struct passwd *pw;
  struct stat statbuf;
  struct Item *ip;
  uid_t uid;
  
if (!MountPathDefined())
   {
   printf("cfengine: mountpattern is undefined\n");
   return;
   }

for (ip = VMOUNTLIST; ip != NULL; ip=ip->next)
   {
   if (IsExcluded(ip->classes))
      {
      continue;
      }
   
   if ((dirh = opendir(ip->name)) == NULL)
      {
      printf("cfengine: Can't open %s\n",ip->name);
      perror("opendir");
      return;
      }

   for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
      {
      if (strcmp(".",dirp->d_name) == 0 || strcmp("..",dirp->d_name) == 0)
         {
         continue;
         }

      if (strcmp("lost+found",dirp->d_name) == 0)
         {
         continue;
         }

      strcpy(homedir,ip->name);
      AddSlash(homedir);
      strcat(homedir,dirp->d_name);

      if (! IsHomeDir(homedir))
         {
         continue;
         }

      if ((dirh2 = opendir(homedir)) == NULL)
         {
         printf("cfengine: Can't open %s\n",homedir);
         perror("opendir");
         return;
         }

      for (dirp2 = readdir(dirh2); dirp2 != NULL; dirp2 = readdir(dirh2))
         {
         if (strcmp(".",dirp2->d_name) == 0 || strcmp("..",dirp2->d_name) == 0)
            {
            continue;
            }

         if (strcmp("lost+found",dirp2->d_name) == 0 || strcmp(".cfengine.rm",dirp2->d_name) == 0)
            {
            continue;
            }

         strcpy(username,dirp2->d_name);
         strcpy(dest,homedir);
         AddSlash(dest);
         strcat(dest,dirp2->d_name);
         AddSlash(dest);
         sp = ptr->fname + strlen("home/");
         strcat(dest,sp);

         if (stat(dest,&statbuf))
            {
            Verbose("cfengine: file %s doesn't exist for editing, skipping\n",dest);
            continue;
            }
      
         if ((pw = getpwnam(username)) == NULL)
            {
            Debug2("cfengine: directory corresponds to no user %s - ignoring\n",username);
            continue;
            }
         else
            {
            Debug2("(Setting user id to %s)\n",username);
            }

         uid = statbuf.st_uid;

         DoEditFile(ptr,dest);
      
         chown(dest,uid,sameowner);
         }
      closedir(dirh2);
      }
   closedir(dirh);
   }
}

/********************************************************************/

DoEditFile(ptr,filename)

struct Edit *ptr;
char *filename;


   /* Many of the functions called here are defined in the */
   /* item.c toolkit since they operate on linked lists    */

{ struct Edlist *ep;
  struct Item *filestart = NULL, *newlineptr;
  char *currenteditscript, *searchstr;

filestart = NULL;
currenteditscript = NULL;
searchstr = NULL;
strcpy(ACTIONBUFF,"");

if (! LoadItemList(&filestart,filename))
   {
   printf("          File was marked for editing\n");
   return;
   }

NUMBEROFEDITS = 0;
EDITVERBOSE = VERBOSE;
CURRENTLINENUMBER = 1;
CURRENTLINEPTR = filestart;
COMMENTSTART = "# ";
COMMENTEND = "";
EDITGROUPLEVEL = 0;
SEARCHREPLACELEVEL = 0;

Verbose("Edit: BEGIN %s\n",filename);

for (ep = ptr->actions; ep != NULL; ep=ep->next)
   {
   if (IsExcluded(ep->classes))
      {
      continue;
      }

   Debug2("Edit action: %s\n",VEDITNAMES[ep->code]);

   switch(ep->code)
      {
      case NoEdit:
               break;

      case DeleteLinesStarting:
               while (DeleteItemStarting(&filestart,ep->data))
                  {
                  }
               break;

      case DeleteLinesContaining:
               while (DeleteItemContaining(&filestart,ep->data))
                  {
                  }
               break;

      case DeleteLinesMatching:
               while (DeleteItemMatching(&filestart,ep->data))
                 {
		 }
               break;

      case Append:
              AppendItem(&filestart,ep->data,NULL);
              break;

      case AppendIfNoSuchLine:
               if (! IsItemIn(filestart,ep->data))
                  {
                  AppendItem(&filestart,ep->data,NULL);
                  }
               break;

      case SetLine:
               strcpy(ACTIONBUFF,ep->data);
               Verbose("Set current line to %s\n",ACTIONBUFF);
               break;

      case AppendIfNoLineMatching:

               if (strcmp(ACTIONBUFF,"") == 0)
                  {
                  printf("cfengine: SetLine not set when calling AppendIfNoLineMatching %s\n",ep->data);
                  continue;
                  }
	       
               if (LocateNextItemMatching(filestart,ep->data) == NULL)
	          {
                  AppendItem(&filestart,ACTIONBUFF,NULL);
                  }
               break;

      case Prepend:
               PrependItem(&filestart,ep->data,NULL);
               break;

      case PrependIfNoSuchLine:
               if (! IsItemIn(filestart,ep->data))
                  {
                  PrependItem(&filestart,ep->data,NULL);
                  }
               break;

      case PrependIfNoLineMatching:

               if (strcmp(ACTIONBUFF,"") == 0)
                  {
                  printf("cfengine: SetLine not set when calling PrependIfNoLineMatching %s\n",ep->data);
                  continue;
                  }

               if (LocateNextItemMatching(filestart,ep->data) == NULL)
                  {
                  PrependItem(&filestart,ACTIONBUFF,NULL);
                  }
               break;

      case WarnIfNoSuchLine:
               if (LocateNextItemMatching(filestart,ep->data) == NULL)
                  {
                  printf("cfengine: Warning, file %s has no line matching %s\n",filename,ep->data);
                  }
               break;

      case WarnIfLineMatching:
               if (LocateNextItemMatching(filestart,ep->data) != NULL)
                  {
                  printf("cfengine: Warning, file %s has a line matching %s\n",filename,ep->data);
                  }
               break;

      case WarnIfNoLineMatching:
               if (LocateNextItemMatching(filestart,ep->data) == NULL)
                  {
                  printf("cfengine: Warning, file %s has a no line matching %s\n",filename,ep->data);
                  }
               break;

      case WarnIfLineStarting:
               if (LocateNextItemStarting(filestart,ep->data) != NULL)
                  {
                  printf("cfengine: Warning, file %s has a line starting %s\n",filename,ep->data);
                  }
               break;

      case WarnIfNoLineStarting:
               if (LocateNextItemStarting(filestart,ep->data) == NULL)
                  {
                  printf("cfengine: Warning, file %s has no line starting %s\n",filename,ep->data);
                  }
               break;

      case WarnIfLineContaining:
               if (LocateNextItemContaining(filestart,ep->data) != NULL)
                  {
                  printf("cfengine: Warning, file %s has a line containing %s\n",filename,ep->data);
                  }
               break;

      case WarnIfNoLineContaining:
               if (LocateNextItemContaining(filestart,ep->data) == NULL)
                  {
                  printf("cfengine: Warning, file %s has no line containing %s\n",filename,ep->data);
                  }
               break;

      case SetCommentStart:
               COMMENTSTART = ep->data;
               break;

      case SetCommentEnd:
               COMMENTEND = ep->data;
               break;

      case CommentLinesMatching:
               while (CommentItemMatching(&filestart,ep->data,COMMENTSTART,COMMENTEND))
                  {
                  }
               break;

      case CommentLinesStarting:
               while (CommentItemStarting(&filestart,ep->data,COMMENTSTART,COMMENTEND))
                  {
                  }
               break;

      case HashCommentLinesContaining:
               while (CommentItemContaining(&filestart,ep->data,"#",""))
                  {
                  }
               break;

      case HashCommentLinesStarting:
               while (CommentItemStarting(&filestart,ep->data,"#",""))
                  {
                  }
               break;

      case HashCommentLinesMatching:
               while (CommentItemMatching(&filestart,ep->data,"#",""))
                  {
                  }
               break;

      case SlashCommentLinesContaining:
               while (CommentItemContaining(&filestart,ep->data,"//",""))
                  {
                  }
               break;

      case SlashCommentLinesStarting:
               while (CommentItemStarting(&filestart,ep->data,"//",""))
                  {
                  }
               break;

      case SlashCommentLinesMatching:
               while (CommentItemMatching(&filestart,ep->data,"//",""))
                  {
                  }
               break;

      case PercentCommentLinesContaining:
               while (CommentItemContaining(&filestart,ep->data,"%",""))
                  {
                  }
               break;

      case PercentCommentLinesStarting:
               while (CommentItemStarting(&filestart,ep->data,"%",""))
                  {
                  }
               break;

      case PercentCommentLinesMatching:
               while (CommentItemMatching(&filestart,ep->data,"%",""))
                  {
                  }
               break;

      case ResetSearch:
               if (!ResetEditSearch(ep->data,filestart))
                  {
                  printf("cfengine: ResetSearch Failed in %s, aborting editing\n",filename);
                  goto abort;
                  }
               break;

      case LocateLineMatching:
               newlineptr = LocateItemMatchingRegExp(CURRENTLINEPTR,ep->data);

               if (newlineptr == NULL)
                  {
                  EditVerbose("cfengine: LocateLineMatchingRegexp failed in %s, aborting editing\n",filename);
                  goto abort;
                  }
               CURRENTLINEPTR = newlineptr;
               break;

      case InsertLine:
                  InsertItemAfter (&filestart,CURRENTLINEPTR,ep->data);
                  break;

      case InsertFile:
	          InsertFileAfter(&filestart,CURRENTLINEPTR,ep->data);
		  break;

      case IncrementPointer:
               if (! IncrementEditPointer(ep->data,filestart))     /* edittools */
                  {
                  printf ("cfengine: IncrementPointer failed in %s, aborting editing\n",filename);
                  goto abort;
                  }
               break;

     case ReplaceLineWith:
               if (!ReplaceEditLineWith(ep))
                  {
                  printf("cfengine: aborting edit of file %s\n",filename);
                  continue;
                  }
               break;

      case DeleteToLineMatching:
               if (! DeleteToRegExp(&filestart,ep->data))
                  {
                  EditVerbose("cfengine: Nothing matched DeleteToLineMatching regular expression\n");
                  EditVerbose("          Aborting file editing of %s.\n",filename);
                  goto abort;
                  }
               break;

      case DeleteNLines:
               if (! DeleteSeveralLines(&filestart,ep->data))
                  {
                  EditVerbose("cfengine: Could not delete %s lines from file\n",ep->data);
                  EditVerbose("          Aborting file editing of %s.\n",filename);
                  goto abort;
                  }
               break;

      case HashCommentToLineMatching:
               if (! CommentToRegExp(&filestart,ep->data,"#",""))
                  {
                  EditVerbose("cfengine: Nothing matched HashCommentToLineMatching regular expression\n");
                  EditVerbose("          Aborting file editing of %s.\n",filename);
                  goto abort;
                  }
               break;

      case PercentCommentToLineMatching:
               if (! CommentToRegExp(&filestart,ep->data,"%",""))
                  {
                  EditVerbose("cfengine: Nothing matched PercentCommentToLineMatching regular expression\n");
                  EditVerbose("          Aborting file editing of %s.\n",filename);
                  goto abort;
                  }
               break;

      case CommentToLineMatching:
               if (! CommentToRegExp(&filestart,ep->data,COMMENTSTART,COMMENTEND))
                  {
                  EditVerbose("cfengine: Nothing matched CommentToLineMatching regular expression\n");
                  EditVerbose("          Aborting file editing of %s.\n",filename);
                  goto abort;
                  }
               break;

      case CommentNLines:
               if (! CommentSeveralLines(&filestart,ep->data,COMMENTSTART,COMMENTEND))
                  {
                  EditVerbose("cfengine: Could not comment %s lines from file\n",ep->data);
                  EditVerbose("          Aborting file editing of %s.\n",filename);
                  goto abort;
                  }
               break;
	       
      case UnCommentNLines:
               if (! UnCommentSeveralLines(&filestart,ep->data,COMMENTSTART,COMMENTEND))
                  {
                  EditVerbose("cfengine: Could not comment %s lines from file\n",ep->data);
                  EditVerbose("          Aborting file editing of %s.\n",filename);
                  goto abort;
                  }
               break;

      case UnCommentLinesContaining:
               while (UnCommentItemContaining(&filestart,ep->data,COMMENTSTART,COMMENTEND))
                  {
                  }
               break;

      case UnCommentLinesMatching:
               while (UnCommentItemMatching(&filestart,ep->data,COMMENTSTART,COMMENTEND))
                  {
                  }
               break;
	       
      case SetScript:
               currenteditscript = ep->data;
               break;

      case RunScript:
               if (! RunEditScript(ep->data,filename,&filestart))
                  {
                  printf("cfengine: Aborting further edits to %s\n",filename);
                  goto abort;
                  }
               break;

      case RunScriptIfNoLineMatching:
               if (! IsItemIn(filestart,ep->data))
                  {
                  if (! RunEditScript(currenteditscript,filename,&filestart))
                     {
                     printf("cfengine: Aborting further edits to %s\n",filename);
                     goto abort;
                     }
                  }
               break;

      case RunScriptIfLineMatching:
               if (IsItemIn(filestart,ep->data))
                  {
                  if (! RunEditScript(currenteditscript,filename,&filestart))
                     {
                     printf("cfengine: Aborting further edits to %s\n",filename);
                     goto abort;
                     }
                  }
               break;

      case EmptyEntireFilePlease:
               EditVerbose("Edit: Emptying entire file\n");
               DeleteItemList(filestart);
	       filestart = NULL;
	       CURRENTLINEPTR = NULL;
	       CURRENTLINENUMBER=0;
               NUMBEROFEDITS++;
               break;

      case GotoLastLine:
               GotoLastItem(filestart);
               break;

      case BreakIfLineMatches:
               if (LineMatches(CURRENTLINEPTR->name,ep->data))
                  {
                  EditVerbose("Edit: Break!\n",ep->data);
                  goto abort;
                  }
               break;

      case BeginGroupIfNoMatch:
	       if (CURRENTLINEPTR == NULL || CURRENTLINEPTR->name == NULL )
		  {
		  EditVerbose("Edit: (Begin Group - no match for %s - file empty)\n",ep->data);
		  break;
		  }
	       
               if (LineMatches(CURRENTLINEPTR->name,ep->data))
                  {
                  EditVerbose("Edit: (Begin Group - skipping)\n",ep->data);
                  while(ep->code != EndGroup)
                     {
                     ep=ep->next;
                     }
                  }
               else
                  {
                  EditVerbose("Edit: (Begin Group - no match for %s)\n",ep->data);
                  }
               break;

     case BeginGroupIfNoLineMatching:
               if (LocateItemMatchingRegExp(filestart,ep->data) != 0)
                  {
                  EditVerbose("Edit: (Begin Group - skipping)\n");
                  while(ep->code != EndGroup)
                     {
                     ep=ep->next;
                     }
                  }
               else
                  {
                  EditVerbose("Edit: (Begin Group - no line matching %s)\n",ep->data);
                  }
               break;

     case BeginGroupIfNoSuchLine:
               if (IsItemIn(filestart,ep->data))
                  {
                  EditVerbose("Edit: (Begin Group - skipping)\n");
                  while(ep->code != EndGroup)
                     {
                     ep=ep->next;
                     }
                  }
               else
                  {
                  EditVerbose("Edit: (Begin Group - no line %s)\n",ep->data);
                  }
               break;

      case EndGroup:
               EditVerbose("Edit: (End Group)\n");
               break;

      case ReplaceAll:
               searchstr = ep->data;
               break;

      case With:
               GlobalReplace(&filestart,searchstr,ep->data);
               break;

      case FixEndOfLine:
               DoFixEndOfLine(filestart,ep->data);
	       break;

      case AbortAtLineMatching:
               EDABORTMODE = true;
	       strcpy(VEDITABORT,ep->data);
	       break;

      case UnsetAbort:
               EDABORTMODE = false;
	       break;

      case AutoMountDirectResources:
	       HandleAutomountRescources(&filestart,ep->data);
	       break;

      default: printf("cfengine: Unknown action in editing of file %s\n",filename);
               break;
      }
   }

abort :  

EditVerbose("Edit: END %s\n",filename);

EDITVERBOSE = false;

if ((! DONTDO) && (NUMBEROFEDITS > 0))
   {
   SaveItemList(filestart,filename);
   }

DeleteItemList(filestart);
}

/********************************************************************/

IncrementEditPointer(str,liststart)

char *str;
struct Item *liststart;

{ int i,n = 0;
  struct Item *ip;

sscanf(str,"%d", &n);

if (n == 0)
   {
   printf("cfengine: Illegal increment value: %s\n",str);
   return false;
   }

if (CURRENTLINEPTR == NULL)  /* is prev undefined, set to line 1 */
   {
   if (liststart == NULL)
      {
      EditVerbose("Edit: cannot increment line pointer in empty file\n");
      return true;
      }
   else
      {
      CURRENTLINEPTR=liststart;
      CURRENTLINENUMBER=1;
      }
   }


if (n < 0)
   {
   if (CURRENTLINENUMBER + n < 1)
      {
      printf("cfengine: pointer decrements to before start of file!\n");
      return false;
      }

   i = 1;

   for (ip = liststart; ip != CURRENTLINEPTR; ip=ip->next)
      {
      if (++i == CURRENTLINENUMBER + n)
         {
         CURRENTLINENUMBER -= n;
         CURRENTLINEPTR = ip;
         return true;
         }
      }
   }

for (i = 0; i < n; i++)
   {
   if (CURRENTLINEPTR->next != NULL)
      {
      CURRENTLINEPTR = CURRENTLINEPTR->next;
      CURRENTLINENUMBER++;

      if (EDITVERBOSE || DEBUG || D2)
         {
         printf("Edit: incrementing line pointer to line %d\n",CURRENTLINENUMBER);
         }
      }
   else
      {
      EditVerbose("Edit: inc pointer failed, still at %d\n",CURRENTLINENUMBER);
      }
   }

return true;
}

/********************************************************************/

ResetEditSearch (str,list)

char *str;
struct Item *list;

{ int i = 1 ,n = -1;
  struct Item *ip;

sscanf(str,"%d", &n);

if (n < 1)
   {
   printf("cfengine: Illegal reset value: %s\n",str);
   return false;
   }

for (ip = list; (i < n) && (ip != NULL); ip=ip->next, i++)
   {
   }

if (i < n || ip == NULL)
   {
   printf("cfengine: Search for (%s) begins after end of file!!\n",str);
   return false;
   }

EditVerbose("Edit: resetting pointers to line %d\n",n);

CURRENTLINENUMBER = n;
CURRENTLINEPTR = ip;

return true;
}

/********************************************************************/

ReplaceEditLineWith (editptr)

struct Edlist *editptr;

{ char *sp;

if (strcmp(editptr->data,CURRENTLINEPTR->name) == 0)
   {
   EditVerbose("Edit: ReplaceLineWith - line does not need correction.\n");
   return true;
   }

if ((sp = malloc(strlen(editptr->data)+1)) == NULL)
   {
   printf("cfengine: Memory allocation failed in ReplaceEditLineWith, aborting edit.\n");
   return false;
   }

EditVerbose("Edit: Replacing line %d with %10s...\n",CURRENTLINENUMBER,editptr->data);
strcpy(sp,editptr->data);
free (CURRENTLINEPTR->name);
CURRENTLINEPTR->name = sp;
NUMBEROFEDITS++;
return true;
}

/********************************************************************/

RunEditScript (script,fname,filestart)

char *script, *fname;
struct Item **filestart;

{ FILE *pp;
  char buffer[bufsize];

if (script == NULL)
   {
   printf("cfengine: No script defined for with SetScript\n");
   return false;
   }

if (DONTDO)
   {
   return;
   }

if (NUMBEROFEDITS > 0)
   {
   SaveItemList(*filestart,fname);
   }

DeleteItemList(*filestart);

sprintf (buffer,"%s %s %s  2>&1",script,fname,CLASSTEXT[VSYSTEMHARDCLASS]);

EditVerbose("Edit: Running command: %s\n",buffer);

if ((pp = popen(buffer,"r")) == NULL)
   {
   printf("cfengine: edit script %s failed to open.\n",fname);
   return false;
   }

while (!feof(pp))   
   {
   fgets(CURRENTITEM,bufsize,pp);

   if (!feof(pp))
      {
      EditVerbose("%s\n",CURRENTITEM);
      }
   }

pclose(pp);

*filestart = 0;

if (! LoadItemList(filestart,fname))
   {
   printf("          File was marked for editing\n");
   return false;
   }

NUMBEROFEDITS = 0;
CURRENTLINENUMBER = 1;
CURRENTLINEPTR = *filestart;
return true;
}

/************************************************************/

DoFixEndOfLine(list,type)  /* fix end of line char format */

  /* Assumes that extra_space macro allows enough space */
  /* in the allocated strings to add a few characters */

struct Item *list;
char *type;

{ struct Item *ip;
  char *sp;
  int gotCR;

EditVerbose("Edit: Checking end of line conventions: type = %s\n",type);

if (strcmp("unix",type) == 0 || strcmp("UNIX",type) == 0)
   {
   for (ip = list; ip != NULL; ip=ip->next)
      {
      for (sp = ip->name; *sp != '\0'; sp++)
	 {
	 if (*sp == (char)13)
	    {
	    *sp = '\0';
	    NUMBEROFEDITS++;
	    }
	 }
      }
   return;
   }

if (strcmp("dos",type) == 0 || strcmp("DOS",type) == 0)
   {
   for (ip = list; ip != NULL; ip = ip->next)
      {
      gotCR = false;
      
      for (sp = ip->name; *sp !='\0'; sp++)
	 {
	 if (*sp == (char)13)
	    {
	    gotCR = true;
	    }
	 }

      if (!gotCR)
	 {
	 *sp = (char)13;
	 *(sp+1) = '\0';
	 NUMBEROFEDITS++;
	 }
      }
   return;
   }

printf("cfengine: Unknown file format: %s\n",type);
}

/**************************************************************/

HandleAutomountRescources(filestart,opts)

struct Item **filestart;
char *opts;

{ struct Item *ip;
  char buffer[bufsize];
  char *sp;

for (ip = VMOUNTABLES; ip != NULL; ip=ip->next)
   {
   for (sp = ip->name; *sp != ':'; sp++)
      {
      }

   sp++;
   sprintf(buffer,"%s\t%s\t%s",sp,opts,ip->name);

   if (LocateNextItemContaining(*filestart,sp) == NULL)
      {
      AppendItem(filestart,buffer,"");
      NUMBEROFEDITS++;
      }
   else
      {
      EditVerbose("Edit: have a server for %s\n",sp);
      }
   }
}
