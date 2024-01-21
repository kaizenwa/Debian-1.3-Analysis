/* 

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
/* File Image copying                                              */
/*                                                                 */
/* last updated: 04/08 1995 Audun                                  */
/*               08/03 1996 Mark                                   */
/*                                                                 */
/*******************************************************************/

#include "cf.defs.h"
#include "cf.extern.h"

/*********************************************************************/
/* Level 1                                                           */
/*********************************************************************/

RecursiveImage(ip,from,to,maxrecurse)

struct Image *ip;
char *from, *to;
int maxrecurse;

{ struct stat statbuf;
  DIR *dirh;
  struct dirent *dirp;
  char newfrom[bufsize];
  char newto[bufsize];
  void *bug_check;

if (maxrecurse == -1)  /* reached depth limit */
   {
   Debug2("MAXRECURSE ran out, quitting at level %s with endlist = %d\n",from,ip->next);
   return;
   }

Debug2("RecursiveImage(%s,lev=%d,next=%d)\n",from,maxrecurse,ip->next);

if (IgnoreFile(from,""))
   {
   Verbose("cfengine: Ignoring directory %s\n",from);
   return;
   }

if (strlen(from) == 0)     /* Check for root dir */
   {
   from = "/";
   }

bug_check = ip->next;

  /* Check that dest dir exists before starting */

strcpy(newto,to);
AddSlash(newto);

if (! MakeDirectoriesFor(newto))
   {
   printf("cfengine: Unable to make directory %s\n",from);
   return;
   }

if ((dirh = opendir(from)) == NULL)
   {
   printf("cfengine: RecursiveImage(): Can't open directory [%s]\n",from);
   return;
   }

if (ip->next != bug_check)
   {
   printf("cfengine: solaris opendir() bug: opendir wrecked the heap memory!!");
   printf("          in copy to %s, using workaround...\n",from);
   ip->next = bug_check;
   }

for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
   {
   if (strcmp(".",dirp->d_name) == 0 || strcmp("..",dirp->d_name) == 0)
      {
      continue;
      }

   if (IgnoreFile(from,dirp->d_name))
      {
      continue;
      }

   strcpy(newfrom,from);                                   /* Assemble pathname */
   AddSlash(newfrom);
   strcpy(newto,to);
   AddSlash(newto);

   if (BufferOverflow(newfrom,dirp->d_name))
      {
      printf(" culprit: RecursiveImage\n");
      closedir(dirh);
      return;
      }

   strcat(newfrom,dirp->d_name);

   if (BufferOverflow(newto,dirp->d_name))
      {
      printf(" culprit: RecursiveImage\n");
      closedir(dirh);
      return;
      }

   strcat(newto,dirp->d_name);

   if (TRAVLINKS)
      {
      if (stat(newfrom,&statbuf) == -1)
         {
         Verbose("cfengine: RecursiveImage(): Can't stat %s\n",newfrom);
         continue;
         }
      }
   else
      {
      if (lstat(newfrom,&statbuf) == -1)
         {
         Verbose("cfengine: RecursiveImage(): Can't stat %s\n",newfrom);
         if (readlink(newfrom,VBUFF,bufsize) != -1)
            {
            Verbose("          File is link to -> %s\n",VBUFF);
            }
         continue;
         }
      }

   if (S_ISDIR(statbuf.st_mode))
      {
      RecursiveImage(ip,newfrom,newto,maxrecurse-1);
      }
   else
      {
      CheckImage(newfrom,newto,ip);
      }
   }

closedir(dirh);
}

/*********************************************************************/

CheckHomeImages(ip)

struct Image *ip;

{ DIR *dirh, *dirh2;
  struct dirent *dirp, *dirp2;
  char *ReadLastNode(), username[maxvarsize];
  char homedir[bufsize],dest[bufsize];
  struct passwd *pw;
  struct stat statbuf;
  struct Item *itp;
  int request_uid = ip->uid->uid;  /* save if -1 */

if (!MountPathDefined())
   {
   printf("cfengine: mountpattern is undefined\n");
   return;
   }

if (stat(ip->path,&statbuf))
   {
   printf("cfengine: Master file %s doesn't exist for copying\n",ip->path);
   return;
   }

for (itp = VMOUNTLIST; itp != NULL; itp=itp->next)
   {
   if (IsExcluded(itp->classes))
      {
      continue;
      }
   
   if ((dirh = opendir(itp->name)) == NULL)
      {
      printf("cfengine: Can't open %s\n",itp->name);
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

      strcpy(homedir,itp->name);
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

         if (strlen(ip->destination) > 4)
            {
            AddSlash(dest);
            strcat(dest,ReadLastNode(ip->destination));
            }

         if (request_uid == -1)
            {
            if ((pw = getpwnam(username)) == NULL)
               {
               Debug2("cfengine: directory corresponds to no user %s - ignoring\n",username);
               continue;
               }
            else
               {
               Debug2("(Setting user id to %s)\n",username);
               }

            ip->uid->uid = pw->pw_uid;
            }

         CheckImage(ip->path,dest,ip);
         }
      closedir(dirh2);
      }
   closedir(dirh);
   }
}

/*********************************************************************/
/* Level 2                                                           */
/*********************************************************************/

CheckImage(source,destination,ip)

char *source;
char *destination;
struct Image *ip;

{ DIR *dirh;
  char sourcefile[bufsize];
  char sourcedir[bufsize];
  char destdir[bufsize];
  char destfile[bufsize];
  struct stat sourcestatbuf, deststatbuf;
  struct dirent *dirp;

Debug2("CheckImage (source=%s destination=%s)\n",source,destination);

if (lstat(source,&sourcestatbuf) == -1)
   {
   printf("cfengine: CheckImage(): Can't stat %s\n",source);
   return;
   }

if (S_ISDIR(sourcestatbuf.st_mode))
   {
   strcpy(sourcedir,source);
   AddSlash(sourcedir);
   strcpy(destdir,destination);
   AddSlash(destdir);

   if ((dirh = opendir(sourcedir)) == NULL)
      {
      printf("cfengine: CheckImage(): Can't open directory %s\n",sourcedir);
      perror("opendir");
      return;
      }

   if (chmod (destdir,sourcestatbuf.st_mode  & 07777) == -1)
      {
      perror("chmod");
      }

   stat(destdir,&deststatbuf);
   CheckExistingFile(destdir,ip->plus,ip->minus,fixall,ip->uid,ip->gid,&deststatbuf);

   for (dirp = readdir(dirh); dirp != NULL; dirp = readdir(dirh))
      {
      if (strcmp("lost+found",dirp->d_name) == 0 || strcmp("..",dirp->d_name) == 0
	  || strcmp(".",dirp->d_name) == 0 )
         {
         continue;
         }

      if (strcmp(".cfengine.rm",dirp->d_name) == 0)
         {
         continue;
         }

      strcpy(sourcefile, sourcedir);
      
      if (BufferOverflow(sourcefile,dirp->d_name))
	 {
	 FatalError("Culprit: CheckImage");
	 }
  
      strcat(sourcefile, dirp->d_name);
      strcpy(destfile, destdir);
      
      if (BufferOverflow(destfile,dirp->d_name))
	 {
	 FatalError("Culprit: CheckImage");
	 }
      strcat(destfile, dirp->d_name);
 
      if (lstat(sourcefile,&sourcestatbuf) == -1)
         {
         printf("cfengine: CheckImage(): Can't stat %s\n",sourcefile);
         return;
	 }

      ImageCopy(sourcefile,destfile,sourcestatbuf,ip);
      }

   closedir(dirh);
   return;
   }

strcpy(sourcefile,source);
strcpy(destfile,destination);


if (S_ISLNK(sourcestatbuf.st_mode) || S_ISREG(sourcestatbuf.st_mode))
   {
   ImageCopy(sourcefile,destfile,sourcestatbuf,ip);
   }
else if (S_ISFIFO (sourcestatbuf.st_mode) || S_ISBLK (sourcestatbuf.st_mode)
	 || S_ISCHR (sourcestatbuf.st_mode)  || S_ISSOCK (sourcestatbuf.st_mode))
   {
   ImageCopy(sourcefile,destfile,sourcestatbuf,ip);
   }
}


/*********************************************************************/
/* Level 3                                                           */
/*********************************************************************/

ImageCopy(sourcefile,destfile,sourcestatbuf,ip)

char *sourcefile;
char *destfile;
struct stat sourcestatbuf;
struct Image *ip;

{ char linkbuf[bufsize], *lastnode;
  struct stat deststatbuf;
  int i, silent = false;
  mode_t srcmode = sourcestatbuf.st_mode;
  int ok_to_copy;
  
Debug2("ImageCopy(%s,%s,+%o,-%o)\n",sourcefile,destfile,ip->plus,ip->minus);

for (lastnode = sourcefile+strlen(sourcefile); *lastnode != '/'; lastnode--)
   {
   }

lastnode++;

if (ip->inclusions != NULL && !IsWildItemIn(ip->inclusions,lastnode))
   {
   Verbose("cfengine: skipping non-included pattern %s\n",sourcefile);
   return;
   }

if (IsWildItemIn(VEXCLUDECOPY,lastnode) || IsWildItemIn(ip->exclusions,lastnode))
   {
   Verbose("cfengine: skipping excluded pattern %s\n",sourcefile);
   return;
   }

if (IsWildItemIn(VLINKCOPIES,lastnode) || IsWildItemIn(ip->symlink,lastnode))
   {
   Verbose("cfengine: copy item %s marked for linking instead\n",sourcefile);
   switch (ip->linktype)
      {
      case 's':
                LinkFiles(destfile,sourcefile,NULL,NULL,NULL);
                break;
      case 'r':
                RelativeLink(destfile,sourcefile,NULL,NULL,NULL);
	        break;
      case 'a':
                AbsoluteLink(destfile,sourcefile,NULL,NULL,NULL);
                break;
      default:
                printf("cfengine: internal error, link type was [%c] in ImageCopy\n",ip->linktype);
                return;
      }
   return;
   }

if (strcmp(ip->action,"warn") == 0)
   {
   printf("cfengine: image file %s is out of date or non-existent\n",destfile);
   printf("          (should be copy of %s)\n", sourcefile);
   Debug2("Leaving ImageCopy\n");
   return;
   }

if (strcmp(ip->action,"silent") == 0)
   {
   silent = true;
   }

bzero(linkbuf,bufsize);

if (lstat(destfile,&deststatbuf) == -1)
   { 
   if (S_ISREG(srcmode))
      {
      Verbose("Image doesn't exist: copying %s to %s\n",sourcefile,destfile);      

      if (CopyReg(sourcefile,destfile,sourcestatbuf,deststatbuf))
         {
         chmod(destfile,sourcestatbuf.st_mode & 07777);
         stat(destfile,&deststatbuf);
         CheckExistingFile(destfile,ip->plus,ip->minus,fixall,ip->uid,ip->gid,&deststatbuf);
         }

      Debug2("Leaving ImageCopy\n");
      return;
      }

   if (S_ISFIFO (srcmode))
      {
#ifdef HAVE_MKFIFO
      if (DONTDO)
         {
         printf("cfengine: Make FIFO %s\n",destfile);
         }
      else if (mkfifo (destfile,srcmode))
         {
         printf ("cfengine: cannot create fifo `%s'", destfile);
	 Debug2("Leaving ImageCopy\n");
         return;
         }
#endif
      }
   else
      {
      if (S_ISBLK (srcmode) || S_ISCHR (srcmode) || S_ISSOCK (srcmode))
         {
         if (DONTDO)
            {
            printf("cfengine: Make BLK/CHR/SOCK %s\n",destfile);
            }
         else if (mknod (destfile, srcmode, sourcestatbuf.st_rdev))
            {
            printf ("cfengine: cannot create special file `%s'", destfile);
	    Debug2("Leaving ImageCopy\n");
            return;
            }
         }
      }

   if (S_ISLNK(srcmode))
      {
      if (readlink(sourcefile,linkbuf,bufsize) == -1)
         {
         printf("cfengine: CheckImage(): Can't readlink %s\n",sourcefile);
	 Debug2("Leaving ImageCopy\n");
         return;
         }

      Verbose("cfengine: imaging link from %s to %s\n",destfile,linkbuf);

      if (ip->linktype == 'a' && linkbuf[0] != '/')      /* Not absolute path - must fix */
         {
         strcpy(VBUFF,sourcefile);
         ChopLastNode(VBUFF);
         AddSlash(VBUFF);
         strcat(VBUFF,linkbuf);
         strcpy(linkbuf,VBUFF);
         }
      
      switch (ip->linktype)
         {
         case 's':
                   LinkFiles(destfile,linkbuf,NULL,NULL,NULL);
                   break;
         case 'r':
                   RelativeLink(destfile,linkbuf,NULL,NULL,NULL);
	           break;
         case 'a':
                   AbsoluteLink(destfile,linkbuf,NULL,NULL,NULL);
                   break;
         default:
                   printf("cfengine: internal error, link type was [%c] in ImageCopy\n",ip->linktype);
                   return;
	 }
      }
   }
else
   {
   if (! ip->force)
      {
      switch (ip->type)
         {
         case 'c': if (S_ISREG(deststatbuf.st_mode) && S_ISREG(srcmode))
		      {
	              ok_to_copy = CompareCheckSums(sourcefile,destfile);
		      }	     
	           else
	              {
		      Verbose("cfengine: checksum comparison replaced by ctime: files not regular\n");
		      Verbose("          %s -> %s\n",sourcefile,destfile);
		      ok_to_copy = (deststatbuf.st_ctime < sourcestatbuf.st_ctime);
	              }
	           break;
		   
         default:  ok_to_copy = (deststatbuf.st_ctime < sourcestatbuf.st_ctime);
	           break;
         }
      }
   
   if ((ip->force) || ok_to_copy)
      {
      if (S_ISREG(srcmode))
         {
         if (! silent && (VERBOSE || DEBUG))
            {
            printf ("cfengine: updating image %s from master %s\n",destfile,sourcefile);
            }

         if (CopyReg(sourcefile,destfile,sourcestatbuf,deststatbuf))
            {
            chmod(destfile,srcmode & 07777);
            stat(destfile,&deststatbuf);
            CheckExistingFile(destfile,ip->plus,ip->minus,fixall,ip->uid,ip->gid,&deststatbuf);
            }

	 Debug2("Leaving ImageCopy\n");
         return;
         }

      if (S_ISLNK(sourcestatbuf.st_mode))
         {
         if (readlink(sourcefile,linkbuf,bufsize) == -1)
            {
            printf("cfengine: CheckImage(): Can't readlink %s\n",sourcefile);
	    Debug2("Leaving ImageCopy\n");
            return;
            }

         Verbose("cfengine: imaging link from %s to %s\n",destfile,linkbuf);

         LinkFiles(destfile,linkbuf,NULL,NULL,NULL);
         }
      }
   else
      {
      if ((S_ISDIR(deststatbuf.st_mode)  && ! S_ISDIR(sourcestatbuf.st_mode))  ||
          (S_ISREG(deststatbuf.st_mode)  && ! S_ISREG(sourcestatbuf.st_mode))  ||
          (S_ISBLK(deststatbuf.st_mode)  && ! S_ISBLK(sourcestatbuf.st_mode))  ||
          (S_ISCHR(deststatbuf.st_mode)  && ! S_ISCHR(sourcestatbuf.st_mode))  ||
          (S_ISSOCK(deststatbuf.st_mode) && ! S_ISSOCK(sourcestatbuf.st_mode)) ||
          (S_ISFIFO(deststatbuf.st_mode) && ! S_ISFIFO(sourcestatbuf.st_mode)) ||
          (S_ISLNK(deststatbuf.st_mode)  && ! S_ISLNK(sourcestatbuf.st_mode)))

         {
         printf("cfengine: image exists but destination type is silly (file/dir/link doesn't match)\n");
         printf("          source=%s, dest=%s\n",sourcefile,destfile);
	 Debug2("Leaving ImageCopy\n");
         return;
         }

      Debug2("cfengine: image file is up to date: %s\n",destfile);
      }
   }
}


/*********************************************************************/
/* Level 2                                                           */
/*********************************************************************/

            /* Based heavily on cp.c in GNU-fileutils */

#ifndef DEV_BSIZE
#ifdef BSIZE
#define DEV_BSIZE BSIZE
#else /* !BSIZE */
#define DEV_BSIZE 4096
#endif /* !BSIZE */
#endif /* !DEV_BSIZE */
 
/* Extract or fake data from a `struct stat'.
   ST_BLKSIZE: Optimal I/O blocksize for the file, in bytes.
   ST_NBLOCKS: Number of 512-byte blocks in the file
   (including indirect blocks). */

#ifndef HAVE_ST_BLOCKS
# define ST_BLKSIZE(statbuf) DEV_BSIZE
# if defined(_POSIX_SOURCE) || !defined(BSIZE) /* fileblocks.c uses BSIZE.  */
#  define ST_NBLOCKS(statbuf) (((statbuf).st_size + 512 - 1) / 512)
# else /* !_POSIX_SOURCE && BSIZE */
#  define ST_NBLOCKS(statbuf) (st_blocks ((statbuf).st_size))
# endif /* !_POSIX_SOURCE && BSIZE */
#else /* HAVE_ST_BLOCKS */
/* Some systems, like Sequents, return st_blksize of 0 on pipes. */
# define ST_BLKSIZE(statbuf) ((statbuf).st_blksize > 0 \
                               ? (statbuf).st_blksize : DEV_BSIZE)
# if defined(hpux) || defined(__hpux__) || defined(__hpux)
/* HP-UX counts st_blocks in 1024-byte units.
   This loses when mixing HP-UX and BSD filesystems with NFS.  */
#  define ST_NBLOCKS(statbuf) ((statbuf).st_blocks * 2)
# else /* !hpux */
#  if defined(_AIX) && defined(_I386)
/* AIX PS/2 counts st_blocks in 4K units.  */
#    define ST_NBLOCKS(statbuf) ((statbuf).st_blocks * 8)
#  else /* not AIX PS/2 */
#    define ST_NBLOCKS(statbuf) ((statbuf).st_blocks)
#  endif /* not AIX PS/2 */
# endif /* !hpux */
#endif /* HAVE_ST_BLOCKS */

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

CopyReg (source,dest,sstat,dstat)

char *source, *dest;
struct stat sstat, dstat;

{ char backup[bufsize];
  int sd, dd, buf_size;
  char *buf, *cp;
  int n_read, *ip;
  long n_read_total = 0;
  int last_write_made_hole = 0;
  int make_holes = 0;

Debug2("CopyFile(%s,%s)\n",source,dest);

if (DONTDO)
   {
   printf("cfengine: copy from %s to %s\n",source,dest);
   return false;
   }

if (IMAGEBACKUP)
  {
   strcpy(backup,dest);
   strcat(backup,CF_SAVED);

   if (rename(dest,backup) == -1)
      {
      /* ignore */
      }
   }

if ((sd = open(source,O_RDONLY)) == -1)
   {
   printf("cfengine: can't copy %s!\n",source);
   perror("open");
   rename(backup,dest);
   return false;
   }


#ifndef IRIX
if (sstat.st_size > sstat.st_blocks * DEV_BSIZE)
#else
# ifdef HAVE_ST_BLOCKS
if (sstat.st_size > sstat.st_blocks * DEV_BSIZE)
# else
if (sstat.st_size > ST_NBLOCKS(sstat) * DEV_BSIZE)
# endif
#endif
   {
   make_holes = 1;   /* must have a hole to get checksum right */
   }

if ((dd = open(dest,O_WRONLY | O_CREAT | O_TRUNC, 0600)) == -1)
   {
   printf("cfengine: can't copy to destination %s!\n",dest);
   perror("open");
   rename(backup,dest);
   return false;
   }

buf_size = ST_BLKSIZE(dstat);
buf = (char *) malloc(buf_size + sizeof(int));

while (true)
   {
   if ((n_read = read (sd, buf, buf_size)) == -1)
      {
      if (errno == EINTR) 
         {
         continue;
         }

      close(sd);
      close(dd);
      free(buf);
      return false;
      }

   if (n_read == 0)
      {
      break;
      }

   n_read_total += n_read;

   ip = 0;

   if (make_holes)
      {
      buf[n_read] = 1;	                   /* Sentinel to stop loop.  */

      /* Find first non-zero *word*, or the word with the sentinel.  */

      ip = (int *) buf;

      while (*ip++ == 0)
         {
         }

      /* Find the first non-zero *byte*, or the sentinel.  */

      cp = (char *) (ip - 1);

      while (*cp++ == 0)
         {
         }

      /* If we found the sentinel, the whole input block was zero,
         and we can make a hole.  */

      if (cp > buf + n_read)
         {
         /* Make a hole.  */
         if (lseek (dd, (off_t) n_read, SEEK_CUR) < 0L)
            {
            printf ("cfengine: lseek in CopyReg, dest=%s\n", dest);
            perror("lseek");
            free(buf);
            return false;
            }
         last_write_made_hole = 1;
         }
      else
         {
         /* Clear to indicate that a normal write is needed. */
         ip = 0;
         }
      }

   if (ip == 0)
      {
      if (full_write (dd, buf, n_read) < 0)
         {
         printf ("cfengine: full_write failed in CopyReg()\n");
         close(sd);
         close(dd);
         free(buf);
	 rename(backup,dest);
         return false;
         }
      last_write_made_hole = 0;
      }
   }

  /* If the file ends with a `hole', something needs to be written at
     the end.  Otherwise the kernel would truncate the file at the end
     of the last write operation.  */

  if (last_write_made_hole)
    {
    /* Write a null character and truncate it again.  */

    if (full_write (dd, "", 1) < 0 || ftruncate (dd, n_read_total) < 0)
       {
       printf("cfengine: full_write or ftruncate error in CopyReg\n");
       free(buf);
       rename(backup,dest);
       return false;
       }
    }

close(sd);
close(dd);

free(buf);

if (Repository(backup))
   {
   unlink(backup);
   }

return true;
}


