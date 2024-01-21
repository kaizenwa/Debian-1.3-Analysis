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
/*
   This small utility `parallelftp' can be useful when
   it is needed to download a file ASAP via a busy link
   (*busy*, not a slow). It also demonstrates capabilities
   of ftpclass
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include "ftpclass.h"
#include "parallelftp.h"

char  *program_name;

ParallelFtp::ParallelFtp(const char *host, const char *rfile, int ofd, int ns, int port)
{
   if(ns<1)
      ns=1;
   nsess=ns;
   sessions=new session[nsess];
   for(int i=0; i<nsess; i++)
      (sessions[i].ftpconn=new Ftp)->Connect(host,port);
   fd=ofd;
   file=strdup(rfile);
}

int	 ParallelFtp::Download()
{
   Ftp	 *sz=sessions[0].ftpconn;
   char	 sz_str[256];
   int   res;

   sz->Open(file,Ftp::SIZE);
   res=sz->Block();
   if(res<0)
      return(res);
   res=sz->Read(sz_str,sizeof(sz_str));
   if(res<0)
      return(res);
   sz->Close();

   file_size=atol(sz_str);

   long	 chunk=file_size/nsess;
   if(chunk<0x1000)
      chunk=0x1000;

   long	 pos;
   int	 i;
   for(pos=0,i=0; pos<=file_size-2*chunk; pos+=chunk,i++)
   {
      sessions[i].ftpconn->Open(file,Ftp::RETRIEVE,pos);
      sessions[i].rest=chunk;
      sessions[i].pos=pos;
   }
   // Arrange to dload the last part
   sessions[i].ftpconn->Open(file,Ftp::RETRIEVE,pos);
   sessions[i].rest=file_size-pos;
   sessions[i].pos=pos;

   while(++i<nsess)
   {
      sessions[i].ftpconn=NULL;
      sessions[i].rest=0;
   }

   long  tot_rest=file_size;

   for(;;)
   {
      char  buf[0x1000];

      SMTask::Schedule();

      for(i=0; i<nsess; i++)
      {
	 if(!sessions[i].ftpconn || sessions[i].rest<=0)
	    continue;
	 res=sessions[i].ftpconn->Read(buf,sizeof(buf));
	 if(res<0 && !(res==Ftp::SEE_ERRNO && errno==EAGAIN))
	    return(res);
	 if(res>0)
	 {
	    lseek(fd,sessions[i].pos,SEEK_SET);
	    int res1=write(fd,buf,res);
	    if(res1==-1)
	    {
	       perror("write() to local file");
	       return -1;
	    }
	    if(res1<res)
	    {
	       fprintf(stderr,"Cannot write to local file - disk full ?\n");
	       return -1;
	    }
	    sessions[i].rest-=res;
	    tot_rest-=res;
	    if(sessions[i].rest<=0)
	    {
	       delete sessions[i].ftpconn;
	       sessions[i].ftpconn=NULL;
	       tot_rest-=sessions[i].rest;
	       sessions[i].rest=0;
	    }
	    sessions[i].pos+=res;
	 }
      }
      if(isatty(fileno(stdout)))
      {
	 printf("%ld\r",file_size-tot_rest);
	 fflush(stdout);
      }
      if(tot_rest<=0)
	 break;
      SMTask::Block();
   }

   return(0);
}

ParallelFtp::~ParallelFtp()
{
   for(int i=0; i<nsess; i++)
      if(sessions[i].ftpconn)
	 delete sessions[i].ftpconn;
   delete sessions;
   free(file);
}

int   main(int argc,char **argv)
{
   program_name=argv[0];

   if(argc!=4)
   {
      fprintf(stderr,"ParallelFtp | Ftpclass " VERSION " | Copyright (c) 1996 by Alexander V. Lukyanov\n");
      fprintf(stderr,"Usage: %s host file number_of_ftp_sessions\n",argv[0]);
      return 1;
   }
   char *output=strrchr(argv[2],'/');
   if(output)
      output++;
   else
      output=argv[2];

   int fd=open(output,O_TRUNC|O_CREAT|O_WRONLY,0644);
   if(fd==-1)
   {
      perror(output);
      return 1;
   }

   int res=ParallelFtp((const char*)argv[1],argv[2],fd,atoi(argv[3])).Download();
   if(res<0)
   {
      fprintf(stderr,"%s: an error occured during downloading\n",program_name);
      if(res==-1)
	 perror(program_name);
      return 1;
   }

   close(fd);
}
