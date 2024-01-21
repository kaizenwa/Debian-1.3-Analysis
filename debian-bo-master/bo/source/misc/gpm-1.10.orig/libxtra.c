/*
 * libxtra.c - client library - extra functions (gpm-Linux)
 *
 * Copyright 1994,1995   rubini@ipvvis.unipv.it (Alessandro Rubini)
 * 
 * xterm management is mostly by Miguel de Icaza
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ********/

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "gpmInt.h"


/*-------------------------------------------------------------------*/

/*
 * these two functions return version information 
 */

static char *gpml_ver_s=GPM_RELEASE;
static int  gpml_ver_i;

char *Gpm_GetLibVersion(int *where)
{
int i,j,k=0;

if (!gpml_ver_i)
  {
  sscanf(gpml_ver_s,"%d.%d.%d",&i,&j,&k);
  gpml_ver_i=i*10000+j*100+k;
  }
if (where) *where=gpml_ver_i;
return gpml_ver_s;
}

static char gpm_ver_s[16];
static int  gpm_ver_i;

char *Gpm_GetServerVersion(int *where)
{
char line[128];
FILE *f;
int i,j,k=0;

if (!gpm_ver_s[0])
  {
  f=popen("gpm -v","r");
  if (!f) return NULL;
  fgets(line,128,f);
  pclose(f);
  sscanf(line,"%*s %s",gpm_ver_s);  /* "gpm-Linux 0.98, March 1995" */
  gpm_ver_s[strlen(gpm_ver_s)-1]='\0'; /* cut the ',' */

  sscanf(gpm_ver_s,"%d.%d.%d",&i,&j,&k);
  gpm_ver_i=i*10000+j*100+k;
  }
  
if (where) *where=gpm_ver_i;
  return gpm_ver_s;
}


/*-------------------------------------------------------------------*/
/*
 * This returns all the available information about the current situation:
 * The return value is the number of buttons, as known to the server,
 * the ePtr, if any, is filled with information on the current state.
 */
int Gpm_GetSnapshot(Gpm_Event *ePtr)
{
Gpm_Connect conn;
Gpm_Event event;
fd_set sillySet;
struct timeval to={0,0};
int i;

  if (!gpm_ver_i)
	{
	Gpm_GetServerVersion(NULL);
	PDEBUG((stderr,"libgpm: got server version as %i\n",gpm_ver_i));
	}
  if (gpm_ver_i<9802 || gpm_fd <=0)
	{
	PDEBUG((stderr,"libgpm: can't get status info\n"));
	return -1; /* error */
	}

  conn.pid=0; /* this signals a request */
  if (ePtr)
	conn.vc=GPM_REQ_SNAPSHOT;
  else
	{
	conn.vc=GPM_REQ_BUTTONS;
	ePtr=&event;
	}

  if (gpm_fd==-1) return -1;
  FD_ZERO(&sillySet);
  FD_SET(gpm_fd,&sillySet);
  if (select(gpm_fd+1,&sillySet,NULL,NULL,&to)==1)
	return 0;
  write(gpm_fd,&conn,sizeof(Gpm_Connect));

  if ((i=Gpm_GetEvent(ePtr))!=1)
	return -1;

  i=ePtr->type; ePtr->type=0;
  return i; /* number of buttons */
}
  
