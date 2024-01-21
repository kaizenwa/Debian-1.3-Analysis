/*
 * lftp and utils
 *
 * Copyright (c) 1996-1997 by Alexander V. Lukyanov (lav@yars.free.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>
#include <sys/types.h>
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <stdarg.h>

#include "StatusLine.h"

int  StatusLine::GetWidth()
{
#ifdef TIOCGWINSZ
   struct winsize sz;
   sz.ws_col=sz.ws_row=0;
   ioctl(fd,TIOCGWINSZ,&sz);
   if(sz.ws_col==0)
      sz.ws_col=80;
   if(sz.ws_row==0)
      sz.ws_row=24;
   return(sz.ws_col);
#else /* !TIOCGWINSZ */
   return 80;
#endif
}

StatusLine::StatusLine(int new_fd)
{
   fd=new_fd;
   strcpy(shown,"");
   if(!isatty(fd))
      fd=-1;
   else
      write(fd,"\r",1);
}

void StatusLine::Show(char *f,...)
{
   if(fd==-1)
      return;

   char newstr[sizeof(shown)];

   va_list v;
   va_start(v,f);
   vsprintf(newstr,f,v);
   va_end(v);

   char *end=newstr+strlen(newstr);
   while(end>newstr && end[-1]==' ')
      end--;
   *end=0;
   if(!strcmp(shown,newstr))
      return;

   int dif=strlen(shown)-strlen(newstr)+2;

   strcpy(shown,newstr);

   while(dif-->0)
      *end++=' ';

   *end=0;

   int w=GetWidth();
   if(end-newstr>=w)
      end=newstr+w-1;

   if(end==newstr)
      return;

   *end++='\r';
   *end=0;

   write(fd,"\r",1);
   write(fd,newstr,strlen(newstr));
}
