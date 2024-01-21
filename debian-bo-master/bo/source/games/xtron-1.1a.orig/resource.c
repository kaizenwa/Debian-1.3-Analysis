/* resource.c - xtron v1.1 resource handler
 *
 *   Copyright (C) 1995 Rhett D. Jacobs <rhett@hotel.canberra.edu.au>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Last Modified: 16/4/95
 */

#include "resource.h"

XrmDatabase XTRONresources;

char *PixmapsDIR(char *name1)
{
  char *string_type[20];
  XrmValue DBvalue;

  XrmGetResource(XTRONresources,"xtron.PixmapsDIR", "Xtron.PixmapsDIR", 
		 string_type, &DBvalue);
  strncpy(name1, DBvalue.addr, (int) DBvalue.size);
  return(name1);
}


char StdGet(char *name1, char *name2)
{
  char *string_type[20];
  XrmValue DBvalue;
  char resource[255];
  
  XrmGetResource(XTRONresources, name1, name2, 
		 string_type, &DBvalue);
  strncpy(resource, DBvalue.addr, (int) DBvalue.size);
  return(resource[0]);
}


char KeyRESOURCE(int plrnum, int keynum)
{
  switch (plrnum) {
  case 1:
    switch (keynum) {
    case 0:
      return(StdGet("xtron.player1UP", "Xtron.player1UP")); break;
    case 1:
      return(StdGet("xtron.player1DOWN", "Xtron.player1DOWN")); break;
    case 2:
      return(StdGet("xtron.player1LEFT", "Xtron.player1LEFT")); break;
    case 3:
      return(StdGet("xtron.player1RIGHT", "Xtron.player1RIGHT")); break;
    }
      break;
  case 2:
    switch (keynum) {
    case 0:
      return(StdGet("xtron.player2UP", "Xtron.player2UP")); break;
    case 1:
      return(StdGet("xtron.player2DOWN", "Xtron.player2DOWN")); break;
    case 2:
      return(StdGet("xtron.player2LEFT", "Xtron.player2LEFT")); break;
    case 3:
      return(StdGet("xtron.player2RIGHT", "Xtron.player2RIGHT")); break;
    }
      break;
  }
  return(' ');
}


int LookAhead(void)
{
  char *string_type[20];
  XrmValue DBvalue;
  char resource[255];
  
  XrmGetResource(XTRONresources, "xtron.lookAHEAD", "Xtron.lookAHEAD", 
		 string_type, &DBvalue);
  strncpy(resource, DBvalue.addr, (int) DBvalue.size);
  return(atoi(resource));
}


int InitialiseResource(void)
{
  XrmInitialize();
  
   if ((XTRONresources = XrmGetFileDatabase("./xtronrc")) == NULL)  {
      if ((XTRONresources = XrmGetFileDatabase("./.xtronrc")) == NULL)  {
	 if ((XTRONresources = XrmGetFileDatabase("$HOME/xtronrc")) == NULL)  {
	    if ((XTRONresources = XrmGetFileDatabase("$HOME/.xtronrc")) == NULL)  {
	       if ((XTRONresources = XrmGetFileDatabase("/usr/X11/lib/X11/app-defaults/xtronrc")) == NULL)  {
		  if ((XTRONresources = XrmGetFileDatabase("/usr/X11/lib/X11/app-defaults/.xtronrc")) == NULL)  {
		     fprintf(stderr,"error: unable to open/read resource file xtronrc\n");
		     return(0);
		  }
	       }
	    }
	 }
      }
   }
   return(1);
}
