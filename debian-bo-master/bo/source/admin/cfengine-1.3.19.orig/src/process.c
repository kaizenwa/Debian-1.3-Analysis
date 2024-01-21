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
/* Process handling                                                */
/*                                                                 */
/*******************************************************************/


#include "cf.defs.h"
#include "cf.extern.h"


/*******************************************************************/

LoadProcessTable(procdata,psopts)

struct Item **procdata;
char *psopts;

{ FILE *pp;
  char pscomm[maxlinksize];

sprintf(pscomm,"%s %s",VPSCOMM[VSYSTEMHARDCLASS],psopts);
  
if ((pp = popen(pscomm,"r")) == NULL)
   {
   printf("cfengine: couldn't open the process list with command\n");
   printf("          %s\n",pscomm);
   return false;
   }

while (!feof(pp))
   {
   fgets(VBUFF,bufsize,pp);
   AppendItem(procdata,VBUFF,"");
   }

pclose(pp);
return true;
}

/*******************************************************************/

DoProcessCheck(pp,procdata)

struct Process *pp;
struct Item *procdata;

{ struct Item *ip;
  char fastmap[(1 << BYTEWIDTH)];
  struct re_pattern_buffer buf;
  struct re_registers regs;
  char *err, line[bufsize];
  int match, pid=-1, ret, matches=0;
  FILE *pipe;

Debug2("Looking for process %s\n",pp->expr);

buf.allocated = 0;
buf.buffer = NULL;
buf.fastmap = fastmap;
buf.translate = 0;

if ((err = re_compile_pattern(pp->expr,strlen(pp->expr),&buf)) != 0)
   {
   printf("cfengine: Regular expression error for process pattern %s\n",pp->expr);
   printf("          %s\n",err);
   return (int) NULL;
   }

re_compile_fastmap (&buf);

for (ip = procdata; ip != NULL; ip=ip->next)
   {
   if ((match = re_search(&buf,ip->name,strlen(ip->name),0,strlen(ip->name),&regs)) >= 0)
      {
      sscanf(ip->name,"%*[^0-9] %d",&pid);
      
      Debug2("Found matching pid %d\n",pid);

      matches++;
      
      if (pid == -1)
	 {
	 printf("cfengine: Unable to extract pid while looking for %s\n",pp->expr);
	 continue;
	 }

      if (pid == 1 && pp->signal == cfhup)
	 {
	 Verbose("(Okay to send HUP to init)\n");
	 }
      else if (pid < 4)
	 {
	 printf("cfengine: will not signal or restart processes 0,1,2,3\n");
	 printf("          occurred while looking for %s\n",pp->expr);
	 }

      if (pp->action == 'w')
	 {
	 printf("cfengine: Process warning, found: %s\n",ip->name);
	 continue;
	 }

      if (pp->signal != cfnosignal)
	 {
	 Verbose("cfengine: signalling process %d with %s\n",pid,SIGNALS[pp->signal]);

	 if (!DONTDO)
	    {
	    if ((ret = kill((pid_t)pid,pp->signal)) < 0)
	       {
	       printf("cfengine: couldn't send signal to pid %d\n",pid);
	       perror("kill");
	       continue;
	       }

	    if ((pp->signal == cfkill || pp->signal == cfterm) && ret >= 0)
	       {
	       printf("cfengine: killed: %s\n",ip->name);
	       }
	    }
	 }

      }
   else if (match < -1)
      {
      printf("cfengine: Regular expression internal error [%s]\n",pp->expr); 
      }
   }

if (pp->matches >= 0)
   {
   switch (pp->comp)
      {
      case '=': if (matches != (int)pp->matches)
                   {
	    	   printf("cfengine: %d processes matched %s (should be %d)\n",
			  matches,pp->expr,pp->matches);
		   }
                break;
      case '>': if (matches <= (int)pp->matches)
	           {
	    	   printf("cfengine: %d processes matched %s (should be >%d)\n",
			  matches,pp->expr,pp->matches);
	           } 
                break;
      case '<': if (matches >= (int)pp->matches)
	           {
	    	   printf("cfengine: %d processes matched %s (should be <%d)\n",
			  matches,pp->expr,pp->matches);
	           } 
      }
   }

if (matches != 0)
   {
   Verbose("Matches found - no restart\n");
   return;
   }

if (strlen(pp->restart) != 0)
   {
   Verbose("cfengine: Executing shell command: %s\n",pp->restart);
	 
   if ((pipe = popen(pp->restart,"r")) == NULL)
      {
      printf ("cfengine: Shell execution failed on %s\n",pp->restart);
      perror("popen");
      return;
      }

   while (!feof(pipe))
      {
      fgets(line,bufsize,pipe);
      Verbose("restart: %s",line);
      }

   pclose(pipe);
   Verbose("(Done with %s)\n",pp->restart);
   }
}





