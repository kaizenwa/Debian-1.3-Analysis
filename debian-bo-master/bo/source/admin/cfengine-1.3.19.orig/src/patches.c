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
 

/*********************************************************/
/* patches.c                                             */
/*                                                       */
/* Contains any fixes which need to be made because of   */
/* lack of OS support on a given platform                */
/* These are conditionally compiled, pending extensions  */
/* or developments in the OS concerned.                  */
/*********************************************************/

#include "cf.defs.h"
#include "cf.extern.h"

#ifndef HAVE_GETNETGRENT

/*********************************************************/

setnetgrent()

{
}

/**********************************************************/

getnetgrent(a,b,c)

char *a, *b, *c;

{
*a=NULL;
*b=NULL;
*c=NULL;
return 0;
}

/***********************************************************/

endnetgrent()

{
}

#endif

#ifndef HAVE_UNAME

/***********************************************************/
/* UNAME is missing on some weird OSes                     */
/***********************************************************/

uname (sys)

struct utsname *sys;

{ char buffer[bufsize], *sp;

if (gethostname(buffer,bufsize) == -1)
   {
   perror("gethostname");
   exit(1);
   }

strcpy(sys->nodename,buffer);

if (strcmp(buffer,AUTOCONF_HOSTNAME) != 0)
   {
   Verbose("This binary was complied on a different host (%s).\n",AUTOCONF_HOSTNAME);
   Verbose("This host does not have uname, so I can't tell if it is the exact same OS\n");
   }

strcpy(sys->sysname,AUTOCONF_SYSNAME);
strcpy(sys->release,"cfengine-had-to-guess");
strcpy(sys->machine,"missing-uname(2)");
strcpy(sys->version,"unknown");


  /* Extract a version number if possible */

for (sp = sys->sysname; *sp != '\0'; sp++)
   {
   if (isdigit(*sp))
      {
      strcpy(sys->release,sp);
      strcpy(sys->version,sp);
      *sp = '\0';
      break;
      }
   }

return (0);
}

#endif

/***********************************************************/
/* strstr() missing on old BSD systems                     */
/***********************************************************/

#ifndef HAVE_STRSTR

char *strstr(s1,s2)

char *s1, *s2;

{ char *sp;

for (sp = s1; *sp != '\0'; sp++)
   {
   if (*sp != *s2)
      {
      continue;
      }

   if (strncmp(sp,s2,strlen(s2))== 0)
      {
      return sp;
      }
   }

return NULL;
}

#endif

/***********************************************************/
/* putenv() missing on old BSD systems                     */
/***********************************************************/

#ifndef HAVE_PUTENV

putenv (s)

char *s;

{
Verbose("cfengine: This system does not have putenv()\n");
Verbose("          Cannot update CFALLCLASSES\n");
}


#endif
