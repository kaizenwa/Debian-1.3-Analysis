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
/* TOOLKIT : files/directories                                       */
/*********************************************************************/

AddSlash(str)

char *str;

{
if (str[strlen(str)-1] != '/')
   {
   strcat(str,"/");
   }
}

/*********************************************************************/

DeleteSlash(str)

char *str;

{
if (str[strlen(str)-1] == '/')
   {
   str[strlen(str)-1] = '\0';
   }
}

/*********************************************************************/

ChopLastNode(str)

  /* Chop off trailing node name (possible blank) starting from
     last character and removing up to the first / encountered 
     e.g. /a/b/c -> /a/b
          /a/b/ -> /a/b                                        */

char *str;

{ char *sp;
  int hasslash = false;

for (sp = str; *sp != '\0'; sp++)
   {
   if (*sp == '/')
      {
      hasslash = true;
      break;
      }
   }

if ( ! hasslash)
   {
   return false;
   }

for (sp = str+strlen(str); *sp != '/'; sp--)        /* skip link name */
   {
   *sp = '\0';
   }

*sp = '\0';
return true;
}

/*********************************************************************/

char *ReadLastNode(str)

  /* Chop off trailing node name (possible blank) starting from
     last character and removing up to the first / encountered 
     e.g. /a/b/c -> /a/b
          /a/b/ -> /a/b                                        */

char *str;

{ char *sp;

for (sp = str+strlen(str); *sp != '/'; sp--)        /* skip link name */
   {
   }

return sp+1;
}

/*********************************************************************/

IsHomeDir(name)

      /* This assumes that the dir lies under mountpattern */

char *name;

{ char *sp;
  struct Item *ip;

if (VMOUNTLIST == NULL)
   {
   return (false);
   }

for (sp = name+strlen(name); *(sp-1) != '/' && sp >= name; sp--)
   {
   }

for (ip = VHOMEPATLIST; ip != NULL; ip=ip->next)
   {
   if (WildMatch(ip->name,sp))
      {
      return(true);
      }
   }

return(false);
}


/*********************************************************************/

MakeDirectoriesFor(file)  /* Make all directories which underpin file */

char *file;

{ char *sp,*spc;
  char currentpath[bufsize];
  char pathbuf[bufsize];
  struct stat statbuf;

strcpy(pathbuf,file);                                        /* local copy */

for (sp = pathbuf+strlen(file); *sp != '/'; sp--)        /* skip link name */
   {
   *sp = '\0';
   }

if (lstat(pathbuf,&statbuf) != -1)
   {
   if (VERBOSE || DEBUG || D2)
      {
      if (S_ISLNK(statbuf.st_mode))
         {
         printf("cfengine: NOTE: %s is a symbolic link, not a true directory!\n",pathbuf);
         }
      }

   if (! S_ISLNK(statbuf.st_mode) && ! S_ISDIR(statbuf.st_mode))
      {
      printf("cfengine: Warning. The object %s is not a directory.\n",pathbuf);
      printf("          Cannot make a new directory without deleting it!\n\n");
      return(false);
      }
   }

for (sp = file, spc = currentpath; *sp != '\0'; sp++)
   {
   if (*sp != '/' && *sp != '\0')
      {
      *spc = *sp;
      spc++;
      }
   else
      {
      *spc = '\0';

      if (strlen(currentpath) == 0)
         {
         }
      else if (stat(currentpath,&statbuf) == -1)
         {
         Debug2("cfengine: Making directory %s, mode %o\n",currentpath,DEFAULTMODE);

         if (! DONTDO)
            {
            if (mkdir(currentpath,DEFAULTMODE) == -1)
               {
               printf("cfengine: Unable to make path %s\n",file);
               perror("          ");
               return(false);
               }
            }
         }
      else
         {
         if (! S_ISDIR(statbuf.st_mode))
            {
            printf("MakeDirectoriesFor() failed. %s is not a directory!\n",currentpath);
            return(false);
            }
         }

      *spc = '/';
      spc++;
      }
   }

Debug("Directory for %s exists. Okay\n",file);
return(true);
}

/*********************************************************************/

EmptyDir(path)

char *path;

{ DIR *dirh;
  struct dirent *dirp;
  int count = 0;
  
Debug2("cfengine: EmptyDir(%s)\n",path);

if ((dirh = opendir(path)) == NULL)
   {
   printf("cfengine: EmptyDir(): Can't open directory %s\n",path);
   perror("opendir");
   return;
   }

for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
   {
   if (strcmp("..",dirp->d_name) == 0 || strcmp(".",dirp->d_name) == 0 )
      {
      continue;
      }

   count++;
   }

closedir(dirh);

return (!count);
}

/*********************************************************************/

RecursiveCheck(name,plus,minus,action,uidlist,gidlist,recurse,rlevel)

char *name;
mode_t plus,minus;
struct UidList *uidlist;
struct GidList *gidlist;
enum fileactions action;
int recurse;
int rlevel;

{ DIR *dirh;
  struct dirent *dirp;
  char pcwd[bufsize];
  struct stat statbuf;

if (recurse == -1)
   {
   return;
   }

Debug2("cfengine: entering %s\n",name);

if ((dirh = opendir(name)) == NULL)
   {
   printf("cfengine: RecursiveCheck(): Can't open directory %s\n",name);
   perror("opendir");
   return;
   }

for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
   {
   if (strcmp("lost+found",dirp->d_name) == 0 || strcmp("..",dirp->d_name) == 0 || strcmp(".",dirp->d_name) == 0 )
      {
      continue;
      }

   if (strcmp(".cfengine.rm",dirp->d_name) == 0)
      {
      continue;
      }

   if (IgnoreFile(name,dirp->d_name))
      {
      continue;
      }

   strcpy(pcwd,name);                                   /* Assemble pathname */
   AddSlash(pcwd);
   strcat(pcwd,dirp->d_name);

   if (strlen(pcwd) > bufsize - buffer_margin)
      {
      printf("cfengine: buffer overflow constructing string in RecursiveCheck\n");
      printf(" culprit: %s\n",pcwd);
      return;
      }


   if (TRAVLINKS)
      {
      if (stat(pcwd,&statbuf) == -1)
         {
         printf("cfengine: was working in %s when this happened:\n",name);
         printf("          RecursiveCheck(): Can't stat %s\n",dirp->d_name);
         continue;
         }
      }
   else
      {
      if (lstat(pcwd,&statbuf) == -1)
         {
         printf("cfengine: was working in %s when this happened:\n",name);
         printf("          RecursiveCheck(): Can't stat %s\n",dirp->d_name);
         continue;
         }
      }

   if (S_ISLNK(statbuf.st_mode))            /* should we ignore links? */
      {
      CheckExistingFile(pcwd,plus,minus,action,uidlist,gidlist,&statbuf);
      continue;
      }

   if (S_ISDIR(statbuf.st_mode))
      {
      if (IsMountedFileSystem(&statbuf,pcwd,rlevel) || (recurse-1 == 0))
         {
         continue;
         }
      else
         {
         if (strcmp(dirp->d_name,".") != 0)  /* avoid infinite regression */
            {
            CheckExistingFile(pcwd,plus,minus,action,uidlist,gidlist,&statbuf);
            RecursiveCheck(pcwd,plus,minus,action,uidlist,gidlist,recurse-1,rlevel+1);
            }
         else
            {
            CheckExistingFile(pcwd,plus,minus,action,uidlist,gidlist,&statbuf);
            }
         }
      }
   else
      {
      CheckExistingFile(pcwd,plus,minus,action,uidlist,gidlist,&statbuf);
      }
   }

closedir(dirh);
}


/*********************************************************************/

CheckExistingFile(file,plus,minus,action,uidlist,gidlist,stat)

char *file;
mode_t plus,minus;
struct UidList *uidlist;
struct GidList *gidlist;
enum fileactions action;
struct stat *stat;

{ mode_t newperm = stat->st_mode;
  int amroot = true;

Debug2("Checking file/dir %s\n",file);

if (getuid() != 0)                            
   {
   amroot = false;
   }

 /* directories must have x set if r set, regardless  */


newperm = (stat->st_mode & 07777) ;
newperm |= plus;
newperm &= ~minus;


if (S_ISREG(stat->st_mode) && (action == fixdirs || action == warndirs)) 
   {
   return;
   }

if (S_ISDIR(stat->st_mode))  
   {
   if (action == fixplain || action == warnplain)
      {
      return;
      }

   if (stat->st_mode & S_IRUSR)
      {
      newperm  |= S_IXUSR;
      }

   if (stat->st_mode & S_IRGRP)
      {
      newperm |= S_IXGRP;
      }

   if (stat->st_mode & S_IROTH)
      {
      newperm |= S_IXOTH;
      }
   }

if (stat->st_uid == 0 && (stat->st_mode & S_ISUID))
   {
   if (newperm & S_ISUID)
      {
      if (! IsItemIn(VSETUIDLIST,file))
         {
         if (amroot) printf("cfengine: NEW SETUID root PROGRAM %s\n",file);
         PrependItem(&VSETUIDLIST,file,NULL);
         }
      }
   else
      {
      switch (action)
         {
         case fixall:
         case fixdirs:
         case fixplain: printf("cfengine: removing setuid (root) flag from %s...\n\n",file);
                        break;
         case warnall:
         case warndirs:
         case warnplain: printf("cfengine: WARNING setuid (root) flag on %s...\n\n",file);
                         break;

         default:       break;
         }
      }
   }

if (stat->st_uid == 0 && (stat->st_mode & S_ISGID))
   {
   if (newperm & S_ISGID)
      {
      if (! IsItemIn(VSETUIDLIST,file))
         {
         if (S_ISDIR(stat->st_mode))
            {
            /* setgid directory */
            }
         else
            {
            if (amroot) printf("cfengine: NEW SETGID root PROGRAM %s\n",file);
            PrependItem(&VSETUIDLIST,file,NULL);
            }
         }
      }
   else
      {
      switch (action)
         {
         case fixall:
         case fixdirs:
         case fixplain: printf("cfengine: removing setgid (root) flag from %s...\n\n",file);
                        break;
         case warnall:
         case warndirs:
         case warnplain: printf("cfengine: WARNING setgid (root) flag on %s...\n\n",file);
                         break;

         default:        break;
         }
      }
   }

CheckOwner(file,action,uidlist,gidlist,stat);

if (S_ISLNK(stat->st_mode))             /* No point in checking permission on a link */
   {
   KillOldLink(file);
   return;
   }

if ((newperm & 07777) == (stat->st_mode & 07777) && (action != touch))    /* file okay */
   {
   return;
   }

switch (action)
   {
   case linkchildren:

   case warnplain:
                if (S_ISREG(stat->st_mode))
                   {
                   printf("cfengine: %s has permission %o\n",file,stat->st_mode & 07777);
                   printf("          [should be %o]\n",newperm & 07777);
                   }
                break;
   case warndirs:
                if (S_ISDIR(stat->st_mode))
                   {
                   printf("cfengine: %s has permission %o\n",file,stat->st_mode & 07777);
                   printf("          [should be %o]\n",newperm & 07777);
                   }
                break;
   case warnall:   
                printf("cfengine: %s has permission %o\n",file,stat->st_mode & 07777);
                printf("          [should be %o]\n",newperm & 07777);
                break;

   case fixplain:

                if (S_ISREG(stat->st_mode))
                   {
                   if (! DONTDO)
                      {
                      if (chmod (file,newperm & 07777) == -1)
			 {
                         printf("cfengine: chmod failed on %s\n",file);
                         perror("chmod");
                         break;
                         }
                      }
                   if (VERBOSE || DEBUG || D2)
                      {
                      printf("cfengine: %s had permission %o\n",file,stat->st_mode & 07777);
                      printf("          [changed to %o]\n",newperm & 07777);
                      }
                   }
                break;

   case fixdirs:
                if (S_ISDIR(stat->st_mode))
                   {
                   if (! DONTDO)
                      {
                      if (chmod (file,newperm & 07777) == -1)
                         {
                         printf("cfengine: chmod failed on %s\n",file);
                         perror("chmod");
                         break;
                         }
                      }
                   if (VERBOSE || DEBUG || D2)
                      {
                      printf("cfengine: %s had permission %o\n",file,stat->st_mode & 07777);
                      printf("          [changed to %o]\n",newperm & 07777);
                      }
                   }
                break;

   case fixall: if (! DONTDO)
                   {
                   if (chmod (file,newperm & 07777) == -1)
		      {
                      printf("cfengine: chmod failed on %s\n",file);
                      perror("chmod");
                      break;
                      }
                   }

                if (VERBOSE || DEBUG || D2)
                   {
                   printf("cfengine: %s had permission %o\n",file,stat->st_mode & 07777);
                   printf("          [changed to %o]\n",newperm & 07777);
                   }
                break;

   case touch:  if (! DONTDO)
                   {
                   if (chmod (file,newperm & 07777) == -1)
		      {
                      printf("cfengine: chmod failed on %s\n",file);
                      perror("chmod");
                      break;
		      }
                   utime (file,NULL);
                   }
                break;

   default:     FatalError("cfengine: internal error CheckExistingFile(): illegal file action\n");
   }
}

/*********************************************************************/

CheckOwner(file,action,uidlist,gidlist,statbuf)

char *file;
enum fileactions action;
struct UidList *uidlist;
struct GidList *gidlist;
struct stat *statbuf;

{ struct passwd *pw;
  struct group *gp;
  struct UidList *ulp;
  struct GidList *glp;
  short uidmatch = false, gidmatch = false;
  int uid; 
  int gid;

for (ulp = uidlist; ulp != NULL; ulp=ulp->next)
   {
   if (ulp->uid == -1 || statbuf->st_uid == ulp->uid)   /* -1 matches anything */
      {
      uid = ulp->uid;
      uidmatch = true;
      break;
      }
   }

for (glp = gidlist; glp != NULL; glp=glp->next)
   {
   if (glp->gid == -1 || statbuf->st_gid == glp->gid)  /* -1 matches anything */
      {
      gid = glp->gid;
      gidmatch = true;
      break;
      }
   }


if (uidmatch && gidmatch)
   {
   return;
   }
else
   {
   if (! uidmatch)
      {
      uid = uidlist->uid;    /* default is first item in list */
      }

   if (! gidmatch)
      {
      gid = gidlist->gid;
      }

   if (S_ISLNK(statbuf->st_mode) && (action == fixdirs || action == fixplain))
      {
      Debug2("File %s incorrect type (link), skipping...\n",file);
      return;
      }

   if ((S_ISREG(statbuf->st_mode) && action == fixdirs) || (S_ISDIR(statbuf->st_mode) && action == fixplain))
      {
      Debug2("File %s incorrect type, skipping...\n",file);
      return;
      }

   switch (action)
      {
      case fixplain:
      case fixdirs:
      case fixall: 
      case touch:
                  if (VERBOSE || DEBUG || D2)
                     {
                     if (uid == -1 && gid == -1)
                        {
                        printf("cfengine: touching %s\n",file);
                        }
                     else
                        {
                        if (uid != -1)
                           {
                           printf("cfengine: changing owner of %s to uid %d\n",file,uid);
                           }

                        if (gid != -1)
                           {
                           printf("cfengine: changing owner of %s to gid %d\n",file,gid);
                           }
                        }
                     }

                  if (! DONTDO && S_ISLNK(statbuf->st_mode))
                     {
#ifdef HAVE_LCHOWN
                     if (lchown(file,uid,gid) == -1)
                        {
                        printf("cfengine: Cannot set ownership on link %s!\n",file);
                        perror("lchown");
                        }
#endif
                     }
                  else if (! DONTDO)
                     {
                     if (chown(file,uid,gid) == -1)
                        {
                        printf("cfengine: Cannot set ownership on file %s!\n",file);
                        perror("chown");
                        }
                     }
                  break;

      case linkchildren:
      case warnall: 
      case warndirs:
      case warnplain:
                  if ((pw = getpwuid(statbuf->st_uid)) == NULL)
                     {
                     printf ("cfengine: File %s is not owned by anybody in the passwd database\n",file);
                     printf ("          (uid = %d,gid = %d)\n",statbuf->st_uid,statbuf->st_gid);
                     break;
                     }

                  if ((gp = getgrgid(statbuf->st_gid)) == NULL)
                     {
                     printf ("cfengine: File %s is not owned by any group in group database\n",file);
                     break;
                     }

                  printf ("cfengine: File %s is owned by [%s], group [%s]\n",file,pw->pw_name,gp->gr_name);
                  break;
      }
   }
}


/*********************************************************************/

CheckHomeSubDir(testpath,tidypath)

char *testpath, *tidypath;

{ char *sp, *subdirstart, *sp1, *sp2;
  char buffer[bufsize];
  int homelen;

if (strncmp(tidypath,"home/",5) == 0)
   {
   strcpy(buffer,testpath);

   for (ChopLastNode(buffer); strlen(buffer) != 0; ChopLastNode(buffer))
     {
     if (IsHomeDir(buffer))
        {
        break;
        }
     }

   homelen = strlen(buffer);

   if (homelen == 0)   /* No homedir */
      {
      return false;
      }

   Debug2("CheckHomeSubDir(%s,%s)\n",testpath,tidypath);

   subdirstart = tidypath + 4;                                   /* Ptr to start of subdir */

   strcpy(buffer,testpath);

   ChopLastNode(buffer);                                         /* Filename only */

   for (sp1 = buffer + homelen +1; *sp1 != '/' && *sp1 != '\0'; sp1++) /* skip user name dir */
      {
      }

   sp2 = subdirstart;

   if (strncmp(sp1,sp2,strlen(sp2)) != 0)
      {
      return false;
      } 

   Debug2("CheckHomeSubDir(true)\n");
   return(true);
   }

return true;
}
