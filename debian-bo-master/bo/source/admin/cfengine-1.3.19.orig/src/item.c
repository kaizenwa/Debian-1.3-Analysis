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


/*********************************************************************/
/*                                                                   */
/*  TOOLKIT: the "item" object library for cfengine                  */
/*                                                                   */
/*********************************************************************/

#include "cf.defs.h"
#include "cf.extern.h"


/*********************************************************************/
/* TOOLKIT : Item list                                               */
/*********************************************************************/

IsItemIn(list,item)

struct Item *list;
char *item;

{ struct Item *ptr; 

for (ptr = list; ptr != NULL; ptr=ptr->next)
   {
   if (strcmp(ptr->name,item) == 0)
      {
      return(true);
      }
   }
return(false);
}

/*********************************************************************/

IsWildItemIn(list,item)

struct Item *list;
char *item;

{ struct Item *ptr;

for (ptr = list; ptr != NULL; ptr=ptr->next)
   {
   if (IsExcluded(ptr->classes))
      {
      continue;
      }
   
   if (WildMatch(ptr->name,item))
      {
      return(true);
      }
   }
return(false);
}

/*********************************************************************/

InsertItemAfter (filestart,ptr,string)

struct Item *ptr, **filestart;
char *string;

{ struct Item *ip, *old;
  char *sp;

EditVerbose("Edit: Inserting %s \n",string);

if ((ip = (struct Item *)malloc(sizeof(struct Item))) == NULL)
   {
   perror("Can't allocate memory in InsertItemAfter()");
   FatalError("");
   }

if ((sp = malloc(strlen(string)+1)) == NULL)
   {
   perror("Can't allocate memory in InsertItemAfter()");
   FatalError("");
   }

if (CURRENTLINEPTR == NULL)   /* File is empty */
   {
   if (filestart == NULL)
      {
      old = NULL;
      }
   else
      {
      old = *filestart;
      }
   
   *filestart = ip;
   strcpy(sp,string);
   ip->name = sp;
   ip->next = old ;
   ip->classes = NULL;
   CURRENTLINEPTR = ip;
   CURRENTLINENUMBER = 1;
   }
else
   {
   ip->next = CURRENTLINEPTR->next;
   CURRENTLINEPTR->next = ip;
   CURRENTLINEPTR=ip;
   CURRENTLINENUMBER++;
   strcpy(sp,string);
   ip->name = sp;
   ip->classes = NULL;
   }

NUMBEROFEDITS++;

return;
}


/*********************************************************************/

InsertFileAfter (filestart,ptr,string)

struct Item *ptr, **filestart;
char *string;

{ struct Item *ip, *old;
  char *sp;
  FILE *fp;
  char linebuf[bufsize];

EditVerbose("Edit: Inserting file %s \n",string);

if ((fp=fopen(string,"r")) == NULL)
   {
   Verbose("Edit: could not open file %s\n",string);
   return;
   }

while(!feof(fp))
   {
   bzero(linebuf,bufsize);   
   fgets(linebuf,bufsize,fp);
   
   if (strlen(linebuf) == 0)
      {
      break;
      }
   
   Chop(linebuf);
   
   if ((ip = (struct Item *)malloc(sizeof(struct Item))) == NULL)
      {
      perror("Can't allocate memory in InsertItemAfter()");
      FatalError("");
      }
   
   if ((sp = malloc(strlen(linebuf)+1)) == NULL)
      {
      perror("Can't allocate memory in InsertItemAfter()");
      FatalError("");
      }

   if (CURRENTLINEPTR == NULL)
      {
      if (filestart == NULL)
	 {
	 old = NULL;
	 }
      else
	 {
         old = *filestart;
	 }
      
      *filestart = ip;
      strcpy(sp,linebuf);
      ip->name = sp;
      ip->next = old;
      ip->classes = NULL;
      CURRENTLINEPTR = ip;
      CURRENTLINENUMBER = 1;
      } 
   else
      {
      ip->next = CURRENTLINEPTR->next;
      CURRENTLINEPTR->next = ip;
      CURRENTLINEPTR=ip;
      CURRENTLINENUMBER++;
      strcpy(sp,linebuf);
      ip->name = sp;
      ip->classes = NULL;
      }
   }

NUMBEROFEDITS++;

fclose(fp);
return;
}


/*********************************************************************/

struct Item *LocateNextItemContaining(list,string) 

struct Item *list;
char *string;

{ struct Item *ip;

for (ip = list; ip != NULL; ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return NULL;
      }
   
   if (strstr(ip->name,string))
      {
      return ip;
      }
   }

return NULL;
}

/*********************************************************************/

struct Item *LocateNextItemMatching(list,string) 

struct Item *list;
char *string;

{ struct Item *ip;
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  char *err;
  int match;

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(string,strlen(string),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",string);
   printf("          %s\n",err);
   return NULL;
   }

re_compile_fastmap (&buf);

for (ip = list; (ip != NULL); ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return NULL;
      }
      
   if ((match = re_match(&buf,ip->name,strlen(ip->name),0,&regs)) == strlen(ip->name))
      {
      return ip;
      }
   else if (match < -1)
      {
      printf("cfengine: Regular expression internal error [%s]\n",string); 
      }
   }

return NULL;
}

/*********************************************************************/

struct Item *LocateNextItemStarting(list,string) 

struct Item *list;
char *string;

{ struct Item *ip;

for (ip = list; (ip != NULL); ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return NULL;
      }
      
   if (strncmp(ip->name,string,strlen(string)) == 0)
      {
      return ip;
      }
   }

return NULL;
}

/*********************************************************************/

struct Item *LocateItemMatchingRegExp(list,string) 

struct Item *list;
char *string;

{ struct Item *ip;
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match, line = 1;
  char *err;

if (DEBUG || D2)
   {
   printf("LocateItemMatchingRegExp(list,%s)\n",string);
   }

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(string,strlen(string),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",string);
   printf("          %s\n",err);
   return NULL;
   }

re_compile_fastmap (&buf);

for (ip = list; (ip != NULL); ip=ip->next, line++)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return NULL;
      }
      
   if ((match = re_match(&buf,ip->name,strlen(ip->name),0,&regs)) == strlen(ip->name))
      {
      Debug2("REGEXP matched [%s] to [%s]\n",ip->name,string);
      EditVerbose("Edit: Search ended at line %d\n",line);
      CURRENTLINENUMBER = line;
      return ip;
      }
   else if (match < -1)
      {
      printf("cfengine: Regular expression internal error [%s]\n",string); 
      }
   }

EditVerbose("Edit: Search for %s failed. Current line still %d\n",string,CURRENTLINENUMBER);
return NULL;
}

/*********************************************************************/

struct Item *LocateItemContainingRegExp(list,string) 

struct Item *list;
char *string;

{ struct Item *ip;
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match, line = 1;
  char *err;

Debug2("LocateItemMatchingRegExp(list,%s)\n",string);

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(string,strlen(string),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",string);
   printf("          %s\n",err);
   return NULL;
   }

re_compile_fastmap (&buf);

for (ip = list; (ip != NULL); ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return NULL;
      }
      
   if ((match = re_search(&buf,ip->name,strlen(ip->name),0,strlen(ip->name),&regs)) >= 0)
      {
      Debug2("REGEXP found [%s] in [%s]\n",string,ip->name);
      EditVerbose("Edit: Search ended at line %d\n",line);
      CURRENTLINENUMBER = line;
      return ip;
      }
   else if (match < 0)
      {
      printf ("cfengine: ERROR in regular expression [%s]!\n",string);
      }
   }

EditVerbose("Edit: Search for %s failed. Current line still %d\n",string,CURRENTLINENUMBER);

return NULL;
}

/********************************************************************/

DeleteToRegExp(filestart,string)

  /* Delete up to and including a line matching the regex */

struct Item **filestart;
char *string;

{ struct Item *ip, *ip_prev, *ip_end = NULL;
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match, linefound = false, done;
  char *err;

Debug2("DeleteToRegExp(list,%s)\n",string);

if (CURRENTLINEPTR == NULL)  /* Shouldn't happen */
   {
   printf("cfengine: File line-pointer undefined during editfile action\n");
   return true;
   }

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(string,strlen(string),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",string);
   printf("          %s\n",err);
   return false;
   }

for (ip = CURRENTLINEPTR; (ip != NULL); ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if ((match = re_match(&buf,ip->name,strlen(ip->name),0,&regs)) == strlen(ip->name))
      {
      Debug2("REGEXP matched [%s] to [%s]\n",ip->name,string);
      Debug2("cfengine: (Found a match in file)\n");

      linefound = true;
      ip_end = ip;
      break;
      }
   else if (match < -1)
      {
      printf("cfengine: Regular expression internal error [%s]\n",string); 
      }
   }

if (! linefound)
   {
   return false;
   }

done = false;

for (ip_prev = *filestart; ip_prev != CURRENTLINEPTR && ip_prev->next != CURRENTLINEPTR; ip_prev=ip_prev->next)
   {
   }

for (ip = CURRENTLINEPTR; ip != NULL; ip = CURRENTLINEPTR)
   {
   if (ip == ip_end)
      {
      EditVerbose("Edit: terminating line: %s (Done)\n",ip->name);
      done = true;
      }

   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }

   EditVerbose("Edit: delete line %s\n",ip->name);
   NUMBEROFEDITS++;
   CURRENTLINEPTR = ip->next;

   if (ip == *filestart)
      {
      *filestart = ip->next;
      }
   else
      {
      ip_prev->next = ip->next;
      }

   free (ip->name);
   free ((char *)ip);

   if (done)
      {
      break;
      }
   }

return true;
}

/*********************************************************************/

DeleteItemStarting(list,string) /* deletes first item to match the string */

struct Item **list;
char *string;

{ struct Item *ip, *next, *last = NULL;

for (ip = *list; ip != NULL; ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if (strncmp(ip->name,string,strlen(string)) == 0)
      {
      if (ip == *list)
         {
         free((*list)->name);
         if (ip->classes != NULL) 
	    {
            free(ip->classes);
	    }
         *list = ip->next;
         free((char *)ip);

         EditVerbose("Edit: Deleted item %s\n",ip->name);
         NUMBEROFEDITS++;
         return true;
         }
      else
         {
         last->next = ip->next; 
         free(ip->name);
         if (ip->classes != NULL) 
	    {
            free(ip->classes);
	    }
         free((char *)ip);

         EditVerbose("Edit: Deleted item %s\n",string);
         NUMBEROFEDITS++;
         return true;
         }

      }
   last = ip;
   }

return false;
}

/*********************************************************************/

DeleteItemMatching(list,string) /* deletes first item to match the regex */

struct Item **list;
char *string;

{ struct Item *ip, *next, *last;
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match;
  char *err;

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(string,strlen(string),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",string);
   printf("          %s\n",err);
   return false;
   }

for (ip = *list; ip != NULL; ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if ((match = re_match(&buf,ip->name,strlen(ip->name),0,&regs)) == strlen(ip->name))
      {
      if (ip == *list)
         {
         free((*list)->name);
         if (ip->classes != NULL) 
	    {
            free(ip->classes);
	    }
         *list = ip->next;
         free((char *)ip);

         EditVerbose("Edit: Deleted item %s\n",string);
         NUMBEROFEDITS++;
         return true;
         }
      else
         {
         last->next = ip->next; 
         free(ip->name);
         if (ip->classes != NULL) 
	    {
            free(ip->classes);
	    }
         free((char *)ip);

         EditVerbose("Edit: Deleted item %s\n",string);
         NUMBEROFEDITS++;
         return true;
         }

      }
   last = ip;
   }

return false;
}

/*********************************************************************/

DeleteItemContaining(list,string) /* deletes first item to match the string */

struct Item **list;
char *string;

{ struct Item *ip, *next, *last;

for (ip = *list; ip != NULL; ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if (strstr(ip->name,string))
      {
      if (ip == *list)
         {
         free((*list)->name);
         *list = ip->next;
         if (ip->classes != NULL) 
	    {
            free(ip->classes);
	    }
         free((char *)ip);

         EditVerbose("Edit: Deleted item %s\n",ip->name);
         NUMBEROFEDITS++;
         return true;
         }
      else
         {
         last->next = ip->next; 
         free(ip->name);
         if (ip->classes != NULL) 
	    {
            free(ip->classes);
	    }
         free((char *)ip);

         EditVerbose("Edit: Deleted item %s\n",string);
         NUMBEROFEDITS++;
         return true;
         }

      }
   last = ip;
   }

return false;
}

/*********************************************************************/

PrependItem (liststart,itemstring,classes)

struct Item **liststart;
char *itemstring,*classes;

{ struct Item *ip;
  char *sp,*spe;

EditVerbose("Edit: Prepending %s\n",itemstring);

if ((ip = (struct Item *)malloc(sizeof(struct Item))) == NULL)
   {
   perror("Can't allocate memory in PrependItem()");
   FatalError("");
   }

if ((sp = malloc(strlen(itemstring)+2)) == NULL)
   {
   perror("Can't allocate memory in PrependItem()");
   FatalError("");
   }

if ((classes != NULL) && (spe = malloc(strlen(classes)+2)) == NULL)
   {
   perror("Can't allocate memory in PrependItem()");
   FatalError("");
   }

strcpy(sp,itemstring);
ip->name = sp;
ip->next = *liststart;
*liststart = ip;

if (classes != NULL)
   {
   strcpy(spe,classes);
   ip->classes = spe;
   }
else
   {
   ip->classes = NULL;
   }

NUMBEROFEDITS++;
}

/*********************************************************************/

AppendItem (liststart,itemstring,classes)

struct Item **liststart;
char *itemstring,*classes;

{ struct Item *ip, *lp;
  char *sp,*spe;

EditVerbose("Edit: Appending %s\n",itemstring);

if ((ip = (struct Item *)malloc(sizeof(struct Item))) == NULL)
   {
   perror("Can't allocate memory in AppendItem()");
   FatalError("");
   }

if ((sp = malloc(strlen(itemstring)+extra_space)) == NULL)
   {
   perror("Can't allocate memory in AppendItem()");
   FatalError("");
   }

if (*liststart == NULL)
   {
   *liststart = ip;
   }
else
   {
   for (lp = *liststart; lp->next != NULL; lp=lp->next)
      {
      }

   lp->next = ip;
   }


if ((classes!= NULL) && (spe = malloc(strlen(classes)+2)) == NULL)
   {
   perror("Can't allocate memory in PrependItem()");
   FatalError("");
   }


strcpy(sp,itemstring);
ip->name = sp;
ip->next = NULL;

if (classes != NULL)
   {
   strcpy(spe,classes);
   ip->classes = spe;
   }
else
   {
   ip->classes = NULL;
   }

NUMBEROFEDITS++;
}

/*********************************************************************/

DeleteItemList(item)                    /* delete starting from item */
 
struct Item *item;

{
if (item != NULL)
   {
   DeleteItemList(item->next);
   item->next = NULL;

   if (item->name != NULL)
      {
      free (item->name);
      }

   if (item->classes != NULL)
      {
      free (item->classes);
      }

   free((char *)item);
   }
}

/*********************************************************************/

DeleteItem(liststart,item)
 
struct Item **liststart,*item;

{ struct Item *ip, *sp;

EditVerbose("Delete Item: %s\n",item->name);

if (item != NULL)
   {
   if (item->name != NULL)
      {
      free (item->name);
      }

   if (item->classes != NULL)
      {
      free (item->classes);
      }

   sp = item->next;

   if (item == *liststart)
      {
      *liststart = sp;
      }
   else
      {
      for (ip = *liststart; ip->next != item; ip=ip->next)
         {
         }

      ip->next = sp;
      }

   free((char *)item);

   NUMBEROFEDITS++;
   }
}

/*********************************************************************/

LoadItemList(liststart,file)

struct Item **liststart;
char *file;

{ FILE *fp;
  struct stat statbuf;
  int i;

if (stat(file,&statbuf) == -1)
   {
   printf("cfengine: Couldn't stat %s\n",file);
   return false;
   }

if (statbuf.st_size > EDITFILESIZE)
   {
   printf("cfengine: File %s is bigger than the limit <editfilesize>\n",file);
   return(false);
   }

if (! S_ISREG(statbuf.st_mode))
   {
   printf("cfengine: %s is not a plain file\n",file);
   return false;
   }

if ((fp = fopen(file,"r")) == NULL)
   {
   printf("cfengine: Couldn't read file %s for editing\n",file);
   return false;
   }


for (i = 0; i < bufsize; i++)
   {
   VBUFF[i] = 0;
   }

while(!feof(fp))
   {
   fgets (VBUFF,bufsize,fp);

   if(VBUFF[strlen(VBUFF)-1] == '\n')
      {
      VBUFF[strlen(VBUFF)-1] = '\0';        /* chop */
      }

   if (!feof(fp) || strlen(VBUFF) != 0) 
      {
      AppendItem(liststart,VBUFF,NULL);
      }

   VBUFF[0] = '\0';
   }

fclose(fp);
return (true);
}

/*********************************************************************/

SaveItemList(liststart,file)

struct Item *liststart;
char *file;

{ struct Item *ip;
  struct stat statbuf;
  FILE *fp;

if (stat(file,&statbuf) == -1)
   {
   printf("cfengine: Couldn't stat %s, which needed editing!\n",file);
   printf("          Check definition in program - is file NFS mounted?\n\n");
   return;
   }

strcpy(VBUFF,file);
strcat(VBUFF,CF_SAVED);

if (rename(file,VBUFF) == -1)
   {
   printf("cfengine: Error occurred while renaming %s\n",file);
   perror("rename ");
   return;
   }
else if (Repository(VBUFF))
   {
   unlink(VBUFF);
   }

if ((fp = fopen(file,"w")) == NULL)
   {
   printf("cfengine: Couldn't write file %s after editing\n",file);
   rename(VBUFF,file);
   return false;
   }

for (ip = liststart; ip != NULL; ip=ip->next)
   {
   fprintf(fp,"%s\n",ip->name);
   }

printf("cfengine: Edited file %s \n",file);

fclose(fp);
chmod(file,statbuf.st_mode);                    /* Restore file permissions etc */
chown(file,statbuf.st_uid,statbuf.st_gid);
}

/*********************************************************************/

DebugListItemList(liststart)

struct Item *liststart;

{ struct Item *ptr;

for (ptr = liststart; ptr != NULL; ptr=ptr->next)
   {
   printf("CFDEBUG: [%s]\n",ptr->name);
   }
}

/*********************************************************************/

CommentItemStarting(list,string,comm,end)

struct Item **list;
char *string,*comm,*end;

{ struct Item *ip;
  char buff[bufsize];

for (ip = *list; ip != NULL; ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if (strncmp(ip->name,string,strlen(string)) == 0)
      {
      if (strlen(ip->name)+strlen(comm)+strlen(end)+2 > bufsize)
         {
         printf("cfengine: bufsize overflow while commenting line - abort\n");
         return false;
         }

      if (strncmp(ip->name,comm,strlen(comm))== 0)
         {
         continue;
         }

      EditVerbose("Edit: Commenting %s%s%s\n",comm,ip->name,end);

      sprintf(buff,"%s%s%s",comm,ip->name,end);
      free(ip->name);

      if ((ip->name = malloc(strlen(buff)+1)) == NULL)
         {
         printf("cfengine: malloc in CommentItemStarting\n ");
         exit(1);
         }

      strcpy(ip->name,buff);
      NUMBEROFEDITS++;

      return true;
      }
   }

return false;
}

/*********************************************************************/

CommentItemContaining(list,string,comm,end)

struct Item **list;
char *string,*comm,*end;

{ struct Item *ip;
  char buff[bufsize];

for (ip = *list; ip != NULL; ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if (strstr(ip->name,string))
      {
      if (strlen(ip->name)+strlen(comm)+strlen(end)+2 > bufsize)
         {
         printf("cfengine: bufsize overflow while commenting line - abort\n");
         return false;
         }

      if (strncmp(ip->name,comm,strlen(comm))== 0)
         {
         continue;
         }

      EditVerbose("Edit: Commenting %s%s%s\n",comm,ip->name,end);

      sprintf(buff,"%s%s%s",comm,ip->name,end);
      free(ip->name);

      if ((ip->name = malloc(strlen(buff)+1)) == NULL)
         {
         printf("cfengine: malloc in CommentItemContaining\n ");
         exit(1);
         }

      strcpy(ip->name,buff);
      NUMBEROFEDITS++;

      return true;
      }
   }

return false;
}

/*********************************************************************/

CommentItemMatching(list,string,comm,end)

struct Item **list;
char *string,*comm,*end;

{ struct Item *ip;
  char buff[bufsize];
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match;
  char *err;

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(string,strlen(string),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",string);
   printf("          %s\n",err);
   return false;
   }

for (ip = *list; ip != NULL; ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if ((match = re_match(&buf,ip->name,strlen(ip->name),0,&regs)) == strlen(ip->name))
      {
      if (strlen(ip->name)+strlen(comm)+strlen(end)+2 > bufsize)
         {
         printf("cfengine: bufsize overflow while commenting line - abort\n");
         return false;
         }

      if (strncmp(ip->name,comm,strlen(comm)) == 0) /* Already commented */
         {
         continue;
         }

      EditVerbose("Edit: Commenting %s%s%s\n",comm,ip->name,end);

      sprintf(buff,"%s%s%s",comm,ip->name,end);
      free(ip->name);

      if ((ip->name = malloc(strlen(buff)+1)) == NULL)
         {
         printf("cfengine: malloc in CommentItemContaining\n ");
         exit(1);
         }

      strcpy(ip->name,buff);
      NUMBEROFEDITS++;

      return true;
      }
   }

return false;
}

/********************************************************************/

UnCommentItemMatching(list,string,comm,end)

struct Item **list;
char *string,*comm,*end;

{ struct Item *ip;
  char buff[bufsize];
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match;
  char *err, *sp, *sp1, *sp2, *spc;

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(string,strlen(string),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",string);
   printf("          %s\n",err);
   return false;
   }

for (ip = *list; ip != NULL; ip=ip->next)
   {
   if ((match = re_match(&buf,ip->name,strlen(ip->name),0,&regs)) == strlen(ip->name))
      {
      if (strlen(ip->name)+strlen(comm)+strlen(end)+2 > bufsize)
         {
         printf("cfengine: bufsize overflow while commenting line - abort\n");
         return false;
         }

      if (strstr(ip->name,comm) == NULL)
         {
         CURRENTLINEPTR = ip->next;
         continue;
         }

      EditVerbose("Edit: uncomment line %s\n",ip->name);
      CURRENTLINEPTR = ip->next;

      if ((sp = malloc(strlen(ip->name)+2)) == NULL)
         {
         printf("cfengine: couldn't allocate memory in UnCommentNLines\n");
         perror("malloc");
         return false;
         }

      spc = sp;
   
      for (sp1 = ip->name; isspace(*sp1); sp1++)
         {
         *spc++ = *sp1;
         }

      *spc = '\0';

      sp2 = ip->name+strlen(ip->name);

      if ((strlen(end) != 0) && (strstr(ip->name,end) != NULL))
         {
         for (sp2 = ip->name+strlen(ip->name); strncmp(sp2,end,strlen(end)) != 0; sp2--)
            {
            }

         *sp2 = '\0';
         }

      strcat(sp,sp1+strlen(comm));

      if (sp2 != ip->name+strlen(ip->name))
         {
         strcat(sp,sp2+strlen(end));
         }

      if (strcmp(sp,ip->name) != 0)
         {
         NUMBEROFEDITS++;
         }
   
      free(ip->name);
      ip->name = sp;
      return true;
      }
   }

return false;
}

/********************************************************************/

UnCommentItemContaining(list,string,comm,end)

struct Item **list;
char *string,*comm,*end;

{ struct Item *ip;
  char buff[bufsize];
  char *sp, *sp1, *sp2, *spc;

for (ip = *list; ip != NULL; ip=ip->next)
   {
   if (strstr(ip->name,string))
      {
      if (strstr(ip->name,comm) == NULL)
         {
         CURRENTLINEPTR = ip->next;
         continue;
         }

      EditVerbose("Edit: uncomment line %s\n",ip->name);
      CURRENTLINEPTR = ip->next;

      if ((sp = malloc(strlen(ip->name)+2)) == NULL)
         {
         printf("cfengine: couldn't allocate memory in UnCommentNLines\n");
         perror("malloc");
         return false;
         }

      spc = sp;
   
      for (sp1 = ip->name; isspace(*sp1); sp1++)
         {
         *spc++ = *sp1;
         }

      *spc = '\0';

      sp2 = ip->name+strlen(ip->name);

      if ((strlen(end) != 0) && (strstr(ip->name,end) != NULL))
         {
         for (sp2 = ip->name+strlen(ip->name); strncmp(sp2,end,strlen(end)) != 0; sp2--)
            {
            }

         *sp2 = '\0';
         }

      strcat(sp,sp1+strlen(comm));

      if (sp2 != ip->name+strlen(ip->name))
         {
         strcat(sp,sp2+strlen(end));
         }

      if (strcmp(sp,ip->name) != 0)
         {
         NUMBEROFEDITS++;
         }
   
      free(ip->name);
      ip->name = sp;
      return true;
      }
   }

return false;
}


/********************************************************************/

CommentToRegExp(filestart,string,comm,end)

  /* Delete up to and including a line matching the regex */

struct Item **filestart;
char *string, *comm, *end;

{ struct Item *ip, *ip_end = NULL;
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match, linefound = false, done;
  char *sp, *err;

Debug2("CommentToRegExp(list,%s %s)\n",comm,string);

if (CURRENTLINEPTR == NULL)  /* Shouldn't happen */
   {
   printf("cfengine: File line-pointer undefined during editfile action\n");
   return true;
   }

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(string,strlen(string),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",string);
   printf("          %s\n",err);
   return false;
   }

for (ip = CURRENTLINEPTR; (ip != NULL); ip=ip->next)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if ((match = re_match(&buf,ip->name,strlen(ip->name),0,&regs)) == strlen(ip->name))
      {
      Debug2("REGEXP matched [%s] to [%s]\n",ip->name,string);
      Debug2("cfengine: (Found a match in file)\n");
      linefound = true;
      ip_end = ip;
      break;
      }
   else if (match < -1)
      {
      printf("cfengine: Regular expression internal error [%s]\n",string); 
      }

   }

if (! linefound)
   {
   return false;
   }

done = false;

for (ip = CURRENTLINEPTR; ip != NULL; ip = CURRENTLINEPTR)
   {
   if (ip == ip_end)
      {
      EditVerbose("Edit: terminating line: %s (Done)\n",ip->name);
      done = true;
      }

   EditVerbose("Edit: comment line %s%s%s\n",comm,ip->name, end);
   NUMBEROFEDITS++;
   CURRENTLINEPTR = ip->next;

   if ((sp = malloc(strlen(ip->name)+strlen(comm)+strlen(end)+2)) == NULL)
      {
      printf("cfengine: couldn't allocate memory in CommentToRegExp\n");
      perror("malloc");
      return false;
      }

   strcpy (sp,comm);
   strcat (sp,ip->name);
   strcat (sp,end);

   free (ip->name);
   ip->name = sp;

   if (done)
      {
      break;
      }
   }

return true;
}

/********************************************************************/

DeleteSeveralLines (filestart,string)

  /* Deletes up to N lines from current position */

struct Item **filestart;
char *string;

{ struct Item *ip, *ip_prev;
  int ctr, N = -99, done = false;

Debug2("DeleteNLines(list,%s)\n",string);

sscanf(string,"%d", &N);

if (N < 1)
   {
   printf("cfengine: Illegal number value in DeleteNLines: %s\n",string);
   return false;
   }


if (CURRENTLINEPTR == NULL)  /* Shouldn't happen */
   {
   printf("cfengine: File line-pointer undefined during editfile action\n");
   return true;
   }

for (ip_prev = *filestart; ip_prev->next != CURRENTLINEPTR; ip_prev=ip_prev->next)
   {
   }

ctr = 1;

for (ip = CURRENTLINEPTR; ip != NULL; ip = CURRENTLINEPTR)
   {
   if (EDABORTMODE && ItemMatchesRegEx(ip->name,VEDITABORT))
      {
      Verbose("Aborting search, regex %s matches line\n",VEDITABORT);
      return false;
      }
      
   if (ctr == N)
      {
      EditVerbose("Edit: terminating line: %s (Done)\n",ip->name);
      done = true;
      }

   EditVerbose("Edit: delete line %s\n",ip->name);
   NUMBEROFEDITS++;
   CURRENTLINEPTR = ip->next;

   if (ip == *filestart)
      {
      *filestart = ip->next;
      }
   else
      {
      ip_prev->next = ip->next;
      }

   free (ip->name);
   free ((char *)ip);
   ctr++;

   if (done)
      {
      break;
      }
   }

if (ctr-1 < N)
   {
   if (!SILENT)
      {
      printf("cfengine: DeleteNLines deleted only %d lines (not %d)\n",ctr-1,N);
      }
   }

return true;
}

/********************************************************************/

struct Item *GotoLastItem (list)

struct Item *list;

{ struct Item *ip;

CURRENTLINENUMBER=1;
CURRENTLINEPTR=list;

for (ip = list; ip != NULL && ip->next != NULL; ip=ip->next)
   {
   CURRENTLINENUMBER++;
   }

CURRENTLINEPTR = ip;

Debug2("Last line (%d): [%s]\n",CURRENTLINENUMBER,ip->name);
return ip;
}

/********************************************************************/

LineMatches (line,regexp)

char *line, *regexp;

{ char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match;
  char *err;

Debug2("LineMatches(%s,%s)\n",line,regexp);
buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(regexp,strlen(regexp),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",regexp);
   printf("          %s\n",err);
   return false;
   }

re_compile_fastmap (&buf);

if ((match = re_match(&buf,line,strlen(line),0,&regs)) == strlen(line))
   {
   return true;
   }
else if (match < -1)
   {
   printf("cfengine: Regular expression internal error [%s]\n",regexp); 
   }

return false;
}

/********************************************************************/

GlobalReplace(liststart,search,replace)

struct Item **liststart;
char *search, *replace;

{ int i;
  char *sp, *start = NULL;
  struct Item *ip;
  struct Item *oldCurrentLinePtr = CURRENTLINEPTR;
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match;
  char *err;
  
EditVerbose("Edit: checking for replace/%s/%s\n",search,replace);
buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(search,strlen(search),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for %s\n",search);
   printf("          %s\n",err);
   return false;
   }

re_compile_fastmap (&buf);

for (ip = *liststart; ip != NULL; ip=ip->next)
   {
   if ((match = re_search(&buf,ip->name,strlen(ip->name),0,strlen(ip->name),&regs)) >= 0)
      {
      start = ip->name + match;
      }
   else if (match == -1)
      {
      continue;
      }
   else if (match < -1)
      {
      printf ("cfengine: ERROR in regular expression [%s]!\n",search);
      }

   bzero(VBUFF,bufsize);
   
   i = 0;

   for (sp = ip->name; *sp != '\0'; sp++)
      {
      if (sp != start)
         {
         VBUFF[i] = *sp;
         }
      else
         {
         sp += regs.end[0] - regs.start[0] - 1;
         VBUFF[i] = '\0';
         strcat(VBUFF,replace);
         i += strlen(replace)-1;
	 
         if ((match = re_search(&buf,sp,strlen(sp),0,strlen(sp),&regs)) >= 0)
            {
            start = sp + match;
            }
         else if (match == -1)
            {
            start = 0;
            }
          else if (match < -1)
            {
            printf ("cfengine: ERROR in regular expression [%s]!\n",search);
            }
         }

      i++;
      }

   CURRENTLINEPTR = ip;                  /* patch */
   InsertItemAfter(liststart,ip,VBUFF);
   DeleteItem(liststart,ip);
   /* ip = ip->next; */                  /* patch */
   }

CURRENTLINEPTR = oldCurrentLinePtr;      /* patch */
}


/********************************************************************/

CommentSeveralLines (filestart,string,comm,end)

  /* Comments up to N lines from current position */

struct Item **filestart;
char *string, *comm, *end;

{ struct Item *ip;
  int ctr, N = -99, done = false;
  char *sp;

Debug2("CommentNLines(list,%s)\n",string);

sscanf(string,"%d", &N);

if (N < 1)
   {
   printf("cfengine: Illegal number value in CommentNLines: %s\n",string);
   return false;
   }


if (CURRENTLINEPTR == NULL)  /* Shouldn't happen */
   {
   printf("cfengine: File line-pointer undefined during editfile action\n");
   return true;
   }

ctr = 1;

for (ip = CURRENTLINEPTR; ip != NULL; ip = CURRENTLINEPTR)
   {
   if (ctr > N)
      {
      break;
      }
   
   if (ctr == N)
      {
      EditVerbose("Edit: terminating line: %s (Done)\n",ip->name);
      done = true;
      }

   for (sp = ip->name; isspace(*sp); sp++)
      {
      }
   
   if (strncmp(sp,comm,strlen(comm)) == 0)
      {
      CURRENTLINEPTR = ip->next;
      ctr++;
      continue;
      }

   EditVerbose("Edit: comment line %s\n",ip->name);
   NUMBEROFEDITS++;
   CURRENTLINEPTR = ip->next;

   if ((sp = malloc(strlen(ip->name)+strlen(comm)+strlen(end)+2)) == NULL)
      {
      printf("cfengine: couldn't allocate memory in CommentNLines\n");
      perror("malloc");
      return false;
      }

   strcpy (sp,comm);
   strcat (sp,ip->name);
   strcat (sp,end);

   free (ip->name);
   ip->name = sp;
   ctr++;

   if (done)
      {
      break;
      }
   }

if (ctr-1 < N)
   {
   if (!SILENT)
      {
      printf("cfengine: CommentNLines commented only %d lines (not %d)\n",ctr-1,N);
      }
   }

return true;
}

/********************************************************************/

UnCommentSeveralLines (filestart,string,comm,end)

  /* Comments up to N lines from current position */

struct Item **filestart;
char *string, *comm, *end;

{ struct Item *ip;
  int ctr, N = -99, done = false;
  char *sp, *sp1, *sp2, *spc;

Debug2("UnCommentNLines(list,%s)\n",string);

sscanf(string,"%d", &N);

if (N < 1)
   {
   printf("cfengine: Illegal number value in CommentNLines: %s\n",string);
   return false;
   }


if (CURRENTLINEPTR == NULL)  /* Shouldn't happen */
   {
   printf("cfengine: File line-pointer undefined during editfile action\n");
   return true;
   }

ctr = 1;

for (ip = CURRENTLINEPTR; ip != NULL; ip = CURRENTLINEPTR)
   {
   if (ctr > N)
      {
      break;
      }
   
   if (ctr == N)
      {
      EditVerbose("Edit: terminating line: %s (Done)\n",ip->name);
      done = true;
      }

   if (strstr(ip->name,comm) == NULL)
      {
      CURRENTLINEPTR = ip->next;
      ctr++;
      continue;
      }

   EditVerbose("Edit: uncomment line %s\n",ip->name);
   CURRENTLINEPTR = ip->next;

   if ((sp = malloc(strlen(ip->name)+2)) == NULL)
      {
      printf("cfengine: couldn't allocate memory in UnCommentNLines\n");
      perror("malloc");
      return false;
      }

   spc = sp;
   
   for (sp1 = ip->name; isspace(*sp1); sp1++)
      {
      *spc++ = *sp1;
      }

   *spc = '\0';

   sp2 = ip->name+strlen(ip->name);

   if ((strlen(end) != 0) && (strstr(ip->name,end) != NULL))
      {
      for (sp2 = ip->name+strlen(ip->name); strncmp(sp2,end,strlen(end)) != 0; sp2--)
	 {
	 }

      *sp2 = '\0';
      }

   strcat(sp,sp1+strlen(comm));

   if (sp2 != ip->name+strlen(ip->name))
      {
      strcat(sp,sp2+strlen(end));
      }

   ctr++;

   if (strcmp(sp,ip->name) != 0)
      {
      NUMBEROFEDITS++;
      }
   
   free(ip->name);
   ip->name = sp;

   if (done)
      {
      break;
      }
   }

if (ctr-1 < N)
   {
   if (!SILENT)
      {
      printf("cfengine: CommentNLines commented only %d lines (not %d)\n",ctr-1,N);
      }
   }

return true;
}

/********************************************************************/

ItemMatchesRegEx(item,regex)

char *item, *regex;

{ char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  int match;
  char *err;

Debug2("ItemMatchesRegEx(%s %s)\n",item,regex);

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(regex,strlen(regex),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for AbortAtLineMatching %s\n",regex);
   printf("          %s\n",err);
   return true;
   }

if ((match = re_match(&buf,item,strlen(item),0,&regs)) == strlen(item))
   {
   Debug2("REGEXP matched [%s] to [%s]\n",item,regex);
   Debug2("cfengine: (Found a match in file)\n");
   return true;
   }
else if (match < -1)
   {
   printf("cfengine: Regular expression internal error for AbortAtLineMatchings [%s]\n",regex);
   return true;  /* for safety */
   }

return false;
}
