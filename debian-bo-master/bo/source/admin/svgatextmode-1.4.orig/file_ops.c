/*  SVGATextMode -- An SVGA textmode manipulation/enhancement tool
 *
 *  Copyright (C) 1995,1996  Koen Gadeyne
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/***
 *** file_ops.c: file operation functions
 ***/
 
#include <stdio.h>
#include <fcntl.h>   /* for open() */
#include "messages.h"


FILE* open_param_file(char* conf_file)
{
  FILE* param_file;
  PDEBUG(("Opening config file '%s'\n",conf_file));
  if ((param_file = fopen(conf_file,"r")) == NULL)
  {
      perror("fopen");
      PERROR(("Could not open Text mode config file '%s'\n",conf_file));
  }
  return(param_file);
}

int opentty(char *devname)
{
  int fd;

  fd = open(devname, O_WRONLY | O_NOCTTY);
  if (fd < 0)
  {
     perror("open");
     PERROR(("Could not open %s for writing\n", devname));
     return -1;
  }
  return(fd);
}

