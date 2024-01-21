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

#include <fcntl.h>
#include <unistd.h>
#include <termios.h>

#include "xmalloc.h"
#include "GetPass.h"
#include "CharReader.h"

char *GetPass(const char *prompt)
{
   static char *oldpass=0;
   static int tty_fd=-2;
   static FILE *f=0;

   if(oldpass)
   {
      free(oldpass);
      oldpass=0;
   }

   if(tty_fd==-2)
   {
      if(isatty(0))
	 tty_fd=0;
      else
      {
	 tty_fd=open("/dev/tty",O_RDONLY);
   	 if(tty_fd!=-1)
	    fcntl(tty_fd,F_SETFD,FD_CLOEXEC);
      }
   }
   if(tty_fd==-1)
      return 0;

   if(f==0)
      f=fdopen(tty_fd,"r");
   if(f==0)
      return 0;

   write(tty_fd,prompt,strlen(prompt));


   struct termios tc;
   tcgetattr(tty_fd,&tc);
   tcflag_t old_lflag=tc.c_lflag;
   tc.c_lflag&=~ECHO;
   tcsetattr(tty_fd,TCSANOW,&tc);

   oldpass=readline_from_file(f);

   tc.c_lflag=old_lflag;
   tcsetattr(tty_fd,TCSANOW,&tc);

   write(tty_fd,"\r\n",2);

   return oldpass;
}

char *readline_from_file(FILE *f)
{
   int	 size=0x800;
   char	 *line=(char*)xmalloc(size);
   int	 len=0;
   char  *ptr=line;

   for(;;)
   {
      int c=lftp_rl_getc(f);
      if(c==EOF && ptr==line)
      {
	 free(line);
	 return(NULL);
      }
      if(c==EOF || c=='\n')
      {
	 *ptr=0;
	 return(line);
      }
      if(len+2>=size)
      {
	 size*=2;
	 line=(char*)xrealloc(line,size);
	 ptr=line+len;
      }
      *ptr++=c;
      len++;
   }
}

int   lftp_rl_getc(FILE *file)
{
   int res;
   CharReader r(fileno(file));

   for(;;)
   {
      SMTask::Schedule();
      res=r.GetChar();
      if(res==r.EOFCHAR)
	 return EOF;
      if(res!=r.NOCHAR)
	 return res;
      SMTask::Block();
   }
}
