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
/*  TOOLKITS: "object" library                                       */
/*                                                                   */
/*********************************************************************/

#include "cf.defs.h"
#include "cf.extern.h"
#include "../pub/global.h"
#include "../pub/md5.h"

/*********************************************************************/
/* Ignore is used by files and tidy modules                          */
/*********************************************************************/

IgnoreFile (pathto,name)

char *pathto, *name;

{ struct Item *ip;

strcpy(VBUFF,pathto);
AddSlash(VBUFF);
strcat(VBUFF,name);

for (ip = VIGNORE; ip != NULL; ip=ip->next)
   {
   if (IsExcluded(ip->classes))
      {
      continue;
      }

   if (*(ip->name) == '/')
      {
      if (strcmp(VBUFF,ip->name) == 0)
         {
         Debug("cfengine: Ignoring abs path [%s][%s]\n",pathto,name);
         return true;
         }
      }
   else
      {
      if (WildMatch(ip->name,name))
         {
         Debug("cfengine: Ignoring pattern [%s][%s]\n",pathto,name);
         return true;
         }
      }
   }

return false;
}

/*********************************************************************/
/* TOOLKIT : Various                                                 */
/*********************************************************************/

BufferOverflow(str1,str2)                   /* Should be an inline ! */

char *str1, *str2;

{ int len = strlen(str2);

if ((strlen(str1)+len) > (bufsize - buffer_margin))
   {
   printf("cfengine: Buffer overflow constructing string. Increase bufsize macro.\n");
   printf("tried to add %s to %s\n",str2,str1);
   return true;
   }

return false;
}

/*********************************************************************/

Repository(file)

char *file;

{ char buffer[bufsize];
  char node[maxlinksize];
  struct stat sstat, dstat;
  char *sp;
  short imagecopy;

if (!IMAGEBACKUP)
   {
   return true;
   }

if (VREPOSITORY[0] == '\0' || HOMECOPY)
   {
   return false;
   }

Debug2("Repository(%s)\n",file);

strcpy (node,file);

buffer[0] = '\0';

for (sp = node; *sp != '\0'; sp++)
   {
   if (*sp == '/')
      {
      *sp = REPOSCHAR;
      }
   }

strcpy(buffer,VREPOSITORY);
AddSlash(buffer);

if (BufferOverflow(buffer,node))
   {
   printf("culprit: Repository()\n");
   return false;
   }

strcat(buffer,node);

MakeDirectoriesFor(buffer);

if (stat(file,&sstat) == -1)
   {
   Debug2("Repository file %s not there\n",file);
   return true;
   }

stat(buffer,&dstat);

imagecopy = IMAGEBACKUP;
IMAGEBACKUP = false;

if (CopyReg(file,buffer,sstat,dstat))
   {
   IMAGEBACKUP = imagecopy;
   Verbose("cfengine: backed up to repository %s\n",buffer);
   return true;
   }
else
   {
   IMAGEBACKUP = imagecopy;
   return false;
   }
}

/*********************************************************************/

Chop(str)

char *str;

{
str[strlen(str)-1] = '\0';
}


/*********************************************************************/

Banner(string)

char *string;

{
Verbose("---------------------------------------------------------------------\n");
Verbose("%s\n",string);
Verbose("---------------------------------------------------------------------\n\n");
}

/*********************************************************************/
/* TOOLIT : Locks and Signals                                        */
/*********************************************************************/

Locked()
 
{ FILE *lock;
  time_t tloc;

if (IGNORELOCK)
   {
   return false;
   }

if (getuid() != 0)    /* Only establish a lock if we are root      */
   {                  /* Other users can only check parsing anyway */
   return(false);
   }
 
if ((lock = fopen(LOCKFILE,"r")) != NULL)
   {
   printf("cfengine: already running? Lock %s already exists!\n",LOCKFILE);
   printf("          If not, remove lock and try again.\n\n");
   fclose(lock);
   return(true);
   }
 
if ((lock = fopen(LOCKFILE,"w")) != NULL)
   {
   fprintf(lock,"%d\n",getpid());
   fclose(lock);
   }
else
   {
   printf("cfengine: Warning: unable to obtain lock %s.",LOCKFILE);
   printf("          Running in no contention mode: disabling scripts, links,\n");
   printf("          mount checking, file checking, copying and editing.\n");
   NOLINKS = true;
   NOMOUNTS = true;
   NOFILECHECK = true;
   NOCOPY = true;
   NOEDITS = true;
   IGNORELOCK = true;
   NOSCRIPTS = true;
   return(false);
   }

if ((tloc = time((time_t *)NULL)) == -1)
   {
   printf("Couldn't read system clock\n");
   }
else if ((lock = fopen(LASTFILE,"w")) != NULL)
   {
   fprintf(lock,"%s\n",ctime(&tloc));
   fclose(lock);
   return(false);
   }
}

/********************************************************************/
 
void HandleSignal(signum)
 
int signum;
 
{
printf("cfengine: Received signal %s\n",SIGNALS[signum]);

if (signum == 15 || signum == 9)
   {
   Unlock();
   exit(0);
   }
}
 
/************************************************************************/
 
Unlock()
 
{
if (IGNORELOCK)
   {
   return;
   }

if (getuid() != 0)
   {
   Debug2("(Not root, no unlock needed)\n");
   return;
   }

if (unlink(LOCKFILE) == -1)
   {
   printf("cfengine: Failed to remove %s\n",LOCKFILE);
   }
}
 


/*********************************************************************/
/* TOOLKIT : actions                                                 */
/*********************************************************************/

enum actions ActionStringToCode (str)

char *str;

{ char *sp;
  int i;

ACTION = none;

for (sp = str; *sp != '\0'; sp++)
   {
   *sp = ToLower(*sp);
   }

for (i = 1; ACTIONID[i] != '\0'; i++)
   {
   if (strcmp(ACTIONID[i],str) == 0)
      {
      ACTION = (enum actions) i;
      break;
      }
   }

if (ACTION == none)
  {
  yyerror("Indexed macro specified no action");
  FatalError("Could not compile action");
  }

return (enum actions) i;
}


/*********************************************************************/
/* TOOLKIT : Error                                                   */
/*********************************************************************/

FatalError(s)

char *s;

{
fprintf (stderr,"cfengine:%s:%s\n",VCURRENTFILE,s);
Unlock();
exit (1);
}

/*********************************************************************/

Warning(s)

char *s;

{
if (WARNINGS)
   { 
   fprintf (stderr,"cfengine:%s:%d: Warning: %s\n",VCURRENTFILE,LINENUMBER,s);
   }
}

/*********************************************************************/
/* TOOLKIT : String                                                  */
/*********************************************************************/

char ToLower (ch)

char ch;

{
if (isdigit(ch) || ispunct(ch))
   {
   return(ch);
   }

if (islower(ch))
   {
   return(ch);
   }
else
   {
   return(ch - 'A' + 'a');
   }
}


/*********************************************************************/

char ToUpper (ch)

char ch;

{
if (isdigit(ch) || ispunct(ch))
   {
   return(ch);
   }

if (isupper(ch))
   {
   return(ch);
   }
else
   {
   return(ch - 'a' + 'A');
   }
}

/*********************************************************************/
/* TOOLKIT : Checksums                                               */
/*********************************************************************/

CompareCheckSums(file1,file2)

        /* See the md5 algorithms in pub-lib/md5.c */

char *file1, *file2;

{ static unsigned char digest1[16], digest2[16];
  int i;

Debug2("Compare checksums on %s & %s\n", file1,file2);

MDFile(file1,digest1);
MDFile(file2,digest2);

for (i = 0; i < 16; i++)
   {
   if (digest1[i] != digest2[i])
      {
      Verbose("Checksum (MD5) mismatch...\n");
      return true;
      }
   }

return false;  /* only if files are identical */
}
