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
/* Mount support toolkit                                             */
/*********************************************************************/

MountPathDefined()

{
if (VMOUNTLIST == NULL)
   {
   printf("cfengine: Program does not define where the host's file systems will be mounted\n");
   printf("          mountpattern undefined\n");
   return false;
   }

return true;
}

/*********************************************************************/


MatchAFileSystem(server, lastlink)

char *server, *lastlink;

{ struct Item *mp;
  char *sp;
  char host[maxvarsize];

for (mp = VMOUNTED; mp != NULL; mp=mp->next)
   {
   sscanf (mp->name,"%[^:]",host);

   if (! IsItemIn(VBINSERVERS,host))
      {
      continue;
      }

   if (strcmp(host,VDEFAULTBINSERVER.name) == 0)
      {
      continue;                      /* Can't link machine to itself! */
      }

   for (sp = mp->name+strlen(mp->name); *(sp-1) != '/'; sp--)
      {
      }

   if (IsHomeDir(sp))
      {
      continue;
      }

   if (strcmp(sp,lastlink) == 0)
      {
      strcpy(server,mp->name+strlen(host)+1);
      return(true);
      }
   }

return(false);
}

/*********************************************************************/

IsMountedFileSystem (childstat,dir,rlevel)

char *dir;
struct stat *childstat;
int rlevel;

{ struct stat parentstat;

strcpy(VBUFF,dir);

if (VBUFF[strlen(VBUFF)-1] == '/')
   {
   strcat(VBUFF,"..");
   }
else
   {
   strcat(VBUFF,"/..");
   }

if (stat(VBUFF,&parentstat) == -1)
   {
   Debug2("File %s couldn't stat its parent directory! Assuming permission\n",dir);
   Debug2("is denied because the file system is mounted from another host.\n");
   return(true);
   }

if (rlevel == 0)  /* If this is the root of a search, don't stop before we started ! */
   {
   return false;
   }

if (childstat->st_dev != parentstat.st_dev)
   {
   Debug2("[%s is on a different file system, not descending]\n",dir);
   return (true);
   }


return(false);
}
