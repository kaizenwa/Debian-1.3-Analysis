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
/* TOOLKIT : links                                                   */
/*********************************************************************/

LinkChildFiles(from,to,type,inclusions,exclusions,copy)

char *from, *to;
char type;
struct Item *inclusions, *exclusions, *copy;

{ DIR *dirh;
  struct dirent *dirp;
  char pcwdto[bufsize],pcwdfrom[bufsize];
  struct stat statbuf;
  int LinkFiles(), HardLinkFiles(), AbsoluteLink(), RelativeLink();
  int (*linkfiles)();
  
if (stat(to,&statbuf) == -1)
   {
   return(false);  /* no error warning, since the higher level routine uses this */
   }

if ((dirh = opendir(to)) == NULL)
   {
   printf("cfengine: LinkChildFiles(): Can't open directory %s\n",to);
   return false;
   }

for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
   {
   if (strcmp("lost+found",dirp->d_name) == 0 || strcmp("..",dirp->d_name) == 0
       || strcmp(".",dirp->d_name) == 0 )
      {
      continue;
      }

   strcpy(pcwdto,to);                               /* Assemble pathnames */
   AddSlash(pcwdto);

   if (BufferOverflow(pcwdto,dirp->d_name))
      {
      FatalError("Can't build filename in LinkChildFiles");
      }
   strcat(pcwdto,dirp->d_name);

   strcpy(pcwdfrom,from);
   AddSlash(pcwdfrom);

  if (BufferOverflow(pcwdfrom,dirp->d_name))
      {
      FatalError("Can't build filename in LinkChildFiles");
      }
   strcat(pcwdfrom,dirp->d_name);
   
   switch (type)
      {
      case 's':
                linkfiles = LinkFiles;
                break;
      case 'r':
	        linkfiles = RelativeLink;
		break;
      case 'a':
	        linkfiles = AbsoluteLink;
                break;
      case 'h':
                linkfiles = HardLinkFiles;
                break;
      default:
                printf("cfengine: internal error, link type was [%c]\n",type);
                continue;
      }
   
   (*linkfiles)(pcwdfrom,pcwdto,inclusions,exclusions,copy);
   }

closedir(dirh);
return true;
}

/*********************************************************************/

LinkChildren(path,type,rootstat,uid,gid,inclusions,exclusions,copy)


/* --------------------------------------------------------------------
   Here we try to break up the path into a part which will match the
   last element of a mounted filesytem mountpoint and the remainder
   after that. We parse the path backwards to get a math e.g.

   /fys/lib/emacs ->  lib /emacs
                      fys /lib/emacs
                          /fys/lib/emacs

   we try to match lib and fys to the binserver list e.g. /mn/anyon/fys
   and hope for the best. If it doesn't match, tough! 
   --------------------------------------------------------------------- */

char *path, type;
struct stat *rootstat;
uid_t uid;
gid_t gid;
struct Item *inclusions, *exclusions, *copy;

{ char *sp;
  char lastlink[bufsize],server[bufsize],from[bufsize],to[bufsize],relpath[bufsize];
  char odir[bufsize];
  DIR *dirh;
  struct dirent *dirp;
  struct stat statbuf;
  int matched = false;
  int LinkFiles(), HardLinkFiles(), AbsoluteLink(), RelativeLink();
  int (*linkfiles)();


if (! S_ISDIR(rootstat->st_mode))
   {
   printf("cfengine: File %s is not a directory: it has no children to link!\n",path);
   return;
   }

Verbose("Linking the children of %s\n",path);
 
for (sp = path+strlen(path); sp != path-1; sp--)
   {
   if (*(sp-1) == '/')
      {
      relpath[0] = '\0';
      sscanf(sp,"%[^/]%s", lastlink,relpath);

      if (MatchAFileSystem(server,lastlink))
         {
         strcpy(odir,server);

	 if (BufferOverflow(odir,relpath))
	    {
	    FatalError("culprit: LinkChildren()");
	    }
         strcat(odir,relpath);

         if ((dirh = opendir(odir)) == NULL)
            {
            printf("cfengine: LinkChildren: Can't open directory %s\n",path);
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

            strcpy(from,path);
	    AddSlash(from);
	    
	    if (BufferOverflow(from,dirp->d_name))
	       {
	       FatalError("culprit: LinkChildren()");
	       }
	    
            strcat(from,dirp->d_name);
	    
            strcpy(to,odir);
            AddSlash(to);
	    
	    if (BufferOverflow(to,dirp->d_name))
	       {
	       FatalError("culprit: LinkChildren()");
	       }
	    
            strcat(to,dirp->d_name);

            Debug2("LinkChild from = %s to = %s\n",from,to);

            if (stat(to,&statbuf) == -1)
               {
               continue;
               }
            else
               {
	       switch (type)
                  {
                  case 's':
                            linkfiles = LinkFiles;
                            break;
                  case 'r':
	                    linkfiles = RelativeLink;
		            break;
                  case 'a':
	                    linkfiles = AbsoluteLink;
                            break;
                  case 'h':
                            linkfiles = HardLinkFiles;
                            break;
                  default:
                            printf("cfengine: internal error, link type was [%c]\n",type);
                            continue;
                  }

	       matched = (*linkfiles)(from,to,inclusions,exclusions,copy);

               if (matched && !DONTDO)
		 {
                 chown(from,uid,gid);
                 }
               }
            }

         if (matched) return;
         }
      }
   }

printf("cfengine: Couldn't link the children of %s to anything because no\n",path);
printf("          file system was found to mirror it in the defined binservers list.\n");
}

/*********************************************************************/

RecursiveLink(lp,from,to,maxrecurse)

struct Link *lp;
char *from, *to;
int maxrecurse;

{ struct stat statbuf;
  DIR *dirh;
  struct dirent *dirp;
  char newfrom[bufsize];
  char newto[bufsize];
  void *bug_check;
  int LinkFiles(), HardLinkFiles(), AbsoluteLink(), RelativeLink();
  int (*linkfiles)();
  
if (maxrecurse == 0)  /* reached depth limit */
   {
   Debug2("MAXRECURSE ran out, quitting at level %s with endlist = %d\n",to,lp->next);
   return false;
   }

Debug2("RecursiveLink(%s->%s,lev=%d,next=%d)\n",from,to,maxrecurse,lp->next);

if (IgnoreFile(to,""))
   {
   Verbose("cfengine: Ignoring directory %s\n",from);
   return false;
   }

if (strlen(to) == 0)     /* Check for root dir */
   {
   to = "/";
   }

bug_check = lp->next;

if ((dirh = opendir(to)) == NULL)
   {
   printf("cfengine: RecursiveLink(): Can't open directory [%s]\n",from);
   return false;
   }

if (lp->next != bug_check)
   {
   printf("cfengine: SVR4 opendir() bug: opendir wrecked the heap memory!!");
   printf("          in copy to %s, using workaround...\n",from);
   lp->next = bug_check;
   }

for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
   {
   if (strcmp(".",dirp->d_name) == 0 || strcmp("..",dirp->d_name) == 0)
      {
      continue;
      }

   if (IgnoreFile(to,dirp->d_name))
      {
      continue;
      }

   strcpy(newfrom,from);                                   /* Assemble pathname */
   AddSlash(newfrom);
   strcpy(newto,to);
   AddSlash(newto);

   if (BufferOverflow(newfrom,dirp->d_name))
      {
      printf(" culprit: RecursiveLink\n");
      closedir(dirh);
      return true;
      }

   strcat(newfrom,dirp->d_name);

   if (BufferOverflow(newto,dirp->d_name))
      {
      printf(" culprit: RecursiveLink\n");
      closedir(dirh);
      return true;
      }

   strcat(newto,dirp->d_name);

   if (TRAVLINKS)
      {
      if (stat(newto,&statbuf) == -1)
         {
         Verbose("cfengine: RecursiveLink(): Can't stat %s\n",newto);
         continue;
         }
      }
   else
      {
      if (lstat(newto,&statbuf) == -1)
         {
         Verbose("cfengine: RecursiveLink(): Can't stat %s\n",newto);
	 bzero(VBUFF,bufsize);
         if (readlink(newto,VBUFF,bufsize) != -1)
            {
            Verbose("          File is link to -> %s\n",VBUFF);
            }
         continue;
         }
      }

   if (S_ISDIR(statbuf.st_mode))
      {
      RecursiveLink(lp,newfrom,newto,maxrecurse-1);
      }
   else
      {
      switch (lp->type)
	 {
	 case 's':
                   linkfiles = LinkFiles;
                   break;
         case 'r':
	           linkfiles = RelativeLink;
		   break;
         case 'a':
	           linkfiles = AbsoluteLink;
                   break;
         case 'h':
                   linkfiles = HardLinkFiles;
                   break;
         default:
                   printf("cfengine: internal error, link type was [%c]\n",lp->type);
                   continue;
	 }

      (*linkfiles)(newfrom,newto,lp->inclusions,lp->exclusions,lp->copy);
      }
   }

closedir(dirh);
return true;
}

/*********************************************************************/

LinkFiles(from,to,inclusions,exclusions,copy)  /* should return true if 'to' found */

char *from, *to;
struct Item *inclusions, *exclusions, *copy;

{ struct stat buf;
  char linkbuf[bufsize], saved[bufsize], absto[bufsize], *sp, *lastnode;
  struct UidList fakeuid;
  struct Image ip;

Debug2("Linkfiles(%s,%s)\n",from,to);
  
for (lastnode = from+strlen(from); *lastnode != '/'; lastnode--)
   {
   }

lastnode++;

if (inclusions != NULL && !IsWildItemIn(inclusions,lastnode))
   {
   Verbose("cfengine: skipping non-included pattern %s\n",from);
   return true;
   }

if (IsWildItemIn(VEXCLUDELINK,lastnode) || IsWildItemIn(exclusions,lastnode))
   {
   Verbose("cfengine: skipping excluded pattern %s\n",from);
   return true;
   }

if (IsWildItemIn(VCOPYLINKS,lastnode) || IsWildItemIn(copy,lastnode))
   {
   fakeuid.uid = sameowner;
   fakeuid.next = NULL;
   ip.plus = samemode;
   ip.minus = samemode;
   ip.uid = &fakeuid;
   ip.gid = (struct GidList *) &fakeuid;
   ip.action = "do";
   ip.recurse = 0;
   ip.type = 't';
   ip.backup = true;
   ip.exclusions = NULL;
   ip.inclusions = NULL;
   ip.symlink = NULL;
   ip.classes = NULL;
   Verbose("cfengine: link item %s marked for copying instead\n",from);
   MakeDirectoriesFor(to);
   CheckImage(to,from,&ip);
   return;
   }

if (*to == '.')         /* relative path, must still check if exists */
   {
   strcpy(absto,from);
   ChopLastNode(absto);   /* Get the path where WE want to make link */
   AddSlash(absto);

   if (BufferOverflow(absto,to))
      {
      FatalError("culprit: LinkFiles()");
      }
   strcat(absto,to);
   }
else
   {
   strcpy(absto,to);
   }

if (stat(absto,&buf) == -1)
   {
   return(false);  /* no error warning, since the higher level routine uses this */
   }

Debug2("Trying to link %s -> %s (%s)\n",from,to,absto);

if (lstat(from,&buf) == 0)
   {
   if (! S_ISLNK(buf.st_mode) && ! ENFORCELINKS)
      {
      Silent("cfengine: Error linking %s -> %s\n",from,to);
      Silent("          Cannot make link: %s exists and is not a link! (uid %d)\n",from,buf.st_uid);
      return(true);
      }

   if (S_ISREG(buf.st_mode) && ENFORCELINKS)
      {
      Silent("cfengine: moving %s to %s.%s\n",from,from,CF_SAVED);

      if (DONTDO)
         {
         return true;
         }

      saved[0] = '\0';
      strcpy(saved,from);
      strcat(saved,CF_SAVED);

      if (rename(from,saved) == -1)
         {
         perror("rename");
         return(true);
         }

      if (Repository(saved))
	 {
	 unlink(saved);
	 }
      }
   }

for (sp = linkbuf; sp < linkbuf+bufsize; sp++)      /* clear buff, readlink(2) */
   {
   *sp = '\0';
   }

if (readlink(from,linkbuf,bufsize) == -1)
   {
   if (! MakeDirectoriesFor(from))                  /* link doesn't exist */
      {
      Silent("cfengine: Couldn't build directory tree up to %s!\n",from);
      Silent("          One element was a plain file, not a directory!\n");
      return(true);
      }
   }
else
   {
   Debug2("Comparing old links %s to %s\n",linkbuf,to);

   if (strcmp(linkbuf,to) != 0)
      {
      if (ENFORCELINKS)
         {
         Silent("cfengine: remove link %s\n",from);

         if (!DONTDO)
            {
            if (unlink(from) == -1)
               {
               perror("unlink");
               return true;
               }

            DoLink(from,to);
            return true;
            }
         }
      else
         {
         Silent("cfengine: Warning old link %s points somewhere else. Doing nothing!\n",from);
         Silent("          (Link points to %s not %s)\n\n",linkbuf,to);
         return(true);
         }
      }
   else
      {
      Verbose("cfengine: Link (%s->%s) exists and is okay.\n",from,to);
      KillOldLink(from);             /* Check whether link points somewhere */
      return(true);
      }
   }

DoLink(from,to);
return(true);
}

/*********************************************************************/

RelativeLink(from,to,inclusions,exclusions,copy)

char *from, *to;
struct Item *inclusions, *exclusions,*copy;
/* global char LINKTO[] */

{ char *sp, *commonto, *commonfrom, *lastnode;
  char buff[bufsize];
  int levels=0;
  
Debug2("RelativeLink(%s,%s)\n",from,to);

if (*to == '.')
   {
   return LinkFiles(from,to,inclusions,exclusions,copy);
   }

if (!CompressPath(LINKTO,to))
   {
   printf("cfengine: failed to link %s to %s\n",from,to);
   return false;
   }

commonto = LINKTO;
commonfrom = from;

if (strcmp(commonto,commonfrom) == 0)
   {
   printf("cfengine: can't link file to itself!\n");
   printf("          (%s -> %s)\n",from,to);
   return false;
   }

while (*commonto == *commonfrom)
   {
   commonto++;
   commonfrom++;
   }

for (sp = commonfrom; *sp != '\0'; sp++)
   {
   if (*sp == '/')
       {
       levels++;
       }
   }

bzero(buff,bufsize);

strcat(buff,"./");

while(levels-- > 0)
   {
   if (BufferOverflow(buff,"../"))
      {
      printf("cfengine: culprit RelativeLink()\n");
      return false;
      }
   strcat(buff,"../");
   }

if (BufferOverflow(buff,commonto))
   {
   printf("cfengine: culprit RelativeLink()\n");
   return false;
   }

strcat(buff,commonto);

return LinkFiles(from,buff,inclusions,exclusions,copy);
}

/*********************************************************************/

AbsoluteLink(from,to,inclusions,exclusions,copy)

char *from, *to;
struct Item *inclusions,*exclusions,*copy;
/* global LINKTO */

{ char absto[bufsize];
  char expand[bufsize];
  
Debug2("AbsoluteLink(%s,%s)\n",from,to);

if (*to == '.')
   {
   strcpy(LINKTO,from);
   ChopLastNode(LINKTO);
   AddSlash(LINKTO);
   strcat(LINKTO,to);
   }
else
   {
   strcpy(LINKTO,to);
   }

CompressPath(absto,LINKTO);

expand[0] = '\0';

if (!ExpandLinks(expand,absto,0))  /* begin at level 1 and beam out at 15 */
   {
   printf("cfengine: failed to make absolute link in\n");
   printf("          %s -> %s\n",from,to);
   return false;
   }
else
   {
   Debug2("ExpandLinks returned %s\n",expand);
   }

CompressPath(LINKTO,expand);

return LinkFiles(from,LINKTO,inclusions,exclusions,copy);
}

/*********************************************************************/

DoLink (from,to)

char *from, *to;

{
if (DONTDO)
   {
   Verbose("cfengine: Link files %s -> %s\n\n",from,to);
   }
else
   {
   printf("cfengine: Linking files %s -> %s\n\n",from,to);

   if (symlink(to,from) == -1)
      {
      perror("symlink");
      }
   }
}

/*********************************************************************/

KillOldLink (name)

char *name;

{ char linkbuf[bufsize];
  char linkpath[bufsize],*sp;
  struct stat statbuf;
  short i;

for (i = 0; i < bufsize; i++)
   {
   linkbuf[i] = linkpath[i] = '\0';
   }

if (readlink(name,linkbuf,bufsize) == -1)
   {
   printf("cfengine: KillOldLink() contradiction! Can't read existing link!\n");
   return;
   }

if (linkbuf[0] != '/')
   {
   strcpy(linkpath,name);    /* Get path to link */

   for (sp = linkpath+strlen(linkpath); (*sp != '/') && (sp >= linkpath); sp-- )
     {
     *sp = '\0';
     }
   }

strcat(linkpath,linkbuf);

if (stat(linkpath,&statbuf) == -1)               /* link points nowhere */
   {
   if (VERBOSE || KILLOLDLINKS || DEBUG || D2)
      {
      printf("cfengine: %s is a link which points to %s\n",name,linkpath);
      printf("          but that file doesn't exist!\n");
      }

   if (KILLOLDLINKS)
      {
      printf("cfengine: removing dead link %s\n",name);

      if (! DONTDO)
         {
         unlink(name);  /* May not work on a client-mounted system ! */
         }
      }
   }
}

/*********************************************************************/

HardLinkFiles(from,to,inclusions,exclusions,copy)  /* should return true if 'to' found */

char *from, *to;
struct Item *inclusions,*exclusions,*copy;

{ struct stat frombuf,tobuf;
  char saved[bufsize], *lastnode;
  struct UidList fakeuid;
  struct Image ip;
  
for (lastnode = from+strlen(from); *lastnode != '/'; lastnode--)
   {
   }

lastnode++;

if (inclusions != NULL && !IsWildItemIn(inclusions,lastnode))
   {
   Verbose("cfengine: skipping non-included pattern %s\n",from);
   return true;
   }

if (IsWildItemIn(VEXCLUDELINK,lastnode) || IsWildItemIn(exclusions,lastnode))
   {
   Verbose("cfengine: skipping excluded pattern %s\n",from);
   return;
   }

if (IsWildItemIn(VCOPYLINKS,lastnode) || IsWildItemIn(copy,lastnode))
   {
   fakeuid.uid = sameowner;
   fakeuid.next = NULL;
   ip.plus = samemode;
   ip.minus = samemode;
   ip.uid = &fakeuid;
   ip.gid = (struct GidList *) &fakeuid;
   ip.action = "do";
   ip.recurse = 0;
   ip.type = 't';
   ip.backup = true;
   ip.exclusions = NULL;
   ip.symlink = NULL;
   Verbose("cfengine: link item %s marked for copying instead\n",from);
   CheckImage(to,from,&ip);
   return;
   }

if (stat(to,&tobuf) == -1)
   {
   return(false);  /* no error warning, since the higher level routine uses this */
   }

if (! S_ISREG(tobuf.st_mode))
   {
   if (! SILENT)
      {
      printf("cfengine: will only hard link regular files and %s is not regular\n",to);
      }
   return true;
   }

Debug2("Trying to (hard) link %s -> %s\n",from,to);

if (stat(from,&frombuf) == -1)
   {
   DoHardLink(from,to);
   return true;
   }

    /* both files exist, but are they the same file? POSIX says  */
    /* the files could be on different devices, but unix doesn't */
    /* allow this behaviour so the tests below are theoretical...*/

if (frombuf.st_ino != tobuf.st_ino && frombuf.st_dev != frombuf.st_dev)
   {
   Verbose("cfengine: If this is POSIX, unable to determine if %s is hard link is correct\n",from);
   Verbose("          since it points to a different filesystem!\n");

   if (frombuf.st_mode == tobuf.st_mode && frombuf.st_size == tobuf.st_size)
      {
      Verbose("cfengine: Hard link (%s->%s) on different device APPEARS okay\n",from,to);
      return true;
      }
   }

if (frombuf.st_ino == tobuf.st_ino && frombuf.st_dev == frombuf.st_dev)
   {
   Verbose("HARD link (%s=>%s) exists and is okay.\n",from,to);
   return true;
   }

printf("cfengine: %s does not appear to be a hard link to %s\n",from,to);

if (ENFORCELINKS)
   {
   printf("cfengine: moving %s to %s.%s\n",from,from,CF_SAVED);

   if (DONTDO)
      {
      return true;
      }

   saved[0] = '\0';
   strcpy(saved,from);
   strcat(saved,CF_SAVED);

   if (rename(from,saved) == -1)
      {
      perror("rename");
      return(true);
      }

   DoHardLink(from,to);
   }

return(true);
}

/*********************************************************************/

DoHardLink (from,to)

char *from, *to;

{
if (DONTDO)
   {
   Verbose("cfengine: Hardlink files %s -> %s\n\n",from,to);
   }
else
   {
   printf("cfengine: Hardlinking files %s -> %s\n\n",from,to);

   if (link(to,from) == -1)
      {
      perror("link");
      }
   }
}

/*********************************************************************/

CompressPath(dest,src)

char *dest, *src;

{ char *sp;
  char node[maxlinksize];

Debug2("CompressPath(%s,%s)\n",dest,src);

bzero(dest,bufsize);

for (sp = src; *sp != '\0'; sp++)
   {
   if (*sp == '/')
      {
      continue;
      }

   bzero(node,maxlinksize);

   sscanf(sp,"%[^/]",node);
   sp += strlen(node) - 1;

   if (strcmp(node,".") == 0)
      {
      continue;
      }

   if (strcmp(node,"..") == 0)
      {
      if (! ChopLastNode(dest))
	 {
	 printf("cfengine: used .. beyond top of filesystem!\n");
	 return false;
	 }
      continue;
      }
   else
      {
      strcat(dest,"/");
      }

   strcat(dest,node);
   }
return true;
}

/*********************************************************************/

ExpandLinks(dest,from,level)                            /* recursive */

  /* Expand a path contaning symbolic links, up to 4 levels  */
  /* of symbolic links and then beam out in a hurry !        */

char *dest, *from;
int level;

{ char *sp, buff[bufsize];
  char node[maxlinksize];
  struct stat statbuf;
  int lastnode = false;

bzero(dest,bufsize);

Debug2("ExpandLinks(%s,%d)\n",from,level);

if (level >= maxlinklevel)
   {
   printf("cfengine: Too many levels of symbolic links to evaluate absolute path\n");
   return false;
   }

for (sp = from; *sp != '\0'; sp++)
   {
   if (*sp == '/')
      {
      continue;
      }
   
   sscanf(sp,"%[^/]",node);
   sp += strlen(node);

   if (*sp == '\0')
      {
      lastnode = true;
      }
   
   if (strcmp(node,".") == 0)
      {
      continue;
      }

   if (strcmp(node,"..") == 0)
      {
      if (! ChopLastNode(LINKTO))
	 {
	 printf("cfengine: used .. beyond top of filesystem!\n");
	 return false;
	 }
      continue;
      }
   else
      {
      strcat(dest,"/");
      }
   
   strcat(dest,node);

   if (lstat(dest,&statbuf) == -1)  /* File doesn't exist so we can stop here */
      {
      printf("cfengine: can't stat %s in ExpandLinks\n",dest);
      perror("stat");
      return false;
      }

   if (S_ISLNK(statbuf.st_mode))
      {
      bzero(buff,bufsize);
      
      if (readlink(dest,buff,bufsize) == -1)
	 {
	 printf("cfengine: Expand links can't stat %s\n",dest);
	 perror("readlink");
	 return false;
	 }
      else
         {
         if (buff[0] == '.')
	    {
            ChopLastNode(dest);
	    AddSlash(dest);
	    if (BufferOverflow(dest,buff))
	       {
	       printf("cfengine: culprit ExpandLinks()\n");
	       return false;
	       }
	    strcat(dest,buff);
	    }
         else if (buff[0] == '/')
	    {
  	    strcpy(dest,buff);
	    DeleteSlash(dest);

	    if (strcmp(dest,from) == 0)
	       {
	       Debug2("No links to be expanded\n");
	       return true;
	       }
	    
	    if (!lastnode && !ExpandLinks(buff,dest,level+1))
	       {
	       return false;
	       }
	    }
	 else
	    {
	    ChopLastNode(dest);
	    AddSlash(dest);
	    strcat(dest,buff);
	    DeleteSlash(dest);

	    if (strcmp(dest,from) == 0)
	       {
	       Debug2("No links to be expanded\n");
	       return true;
	       }
	    
	    bzero(buff,bufsize);

	    if (!lastnode && !ExpandLinks(buff,dest,level+1))
	       {
	       return false;
	       }	    
	    }
         }
      }
   }
return true;
}
