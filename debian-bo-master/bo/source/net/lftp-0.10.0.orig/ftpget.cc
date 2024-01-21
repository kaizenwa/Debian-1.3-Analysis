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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include "getopt.h"
#include "XferJob.h"
#include "GetPass.h"

extern "C" {
#include "long-options.h"
}

char  *program;

const char version_string[]="FtpGet version " VERSION;

void  PrintUsage(int p)
{
   if(p)
   {
      printf("FtpGet | Version " VERSION " | Copyright (C) 1995-97 Alexander V. Lukyanov\n");
      printf("This is free software with ABSOLUTELY NO WARRANTY. See COPYING for details.\n");
   }
   printf("Usage: ftpget [OPTIONS] host filename [-o local] [filename...]\n"
	  "\n"
	  "-p  --port         set port number\n"
	  "-u  --user         login as user using pass as password\n"
	  "-l  --list         get listing of specified directory(ies)\n"
	  "-c  --continue     reget specified file(s)\n"
	  "    --sync-mode    use synchronous mode (compatible with bugs)\n"
	  "    --norest-mode  don't use REST command\n"
	  "\n"
	  "-o  output to local file `local' (default - base name of filename)\n"
      );
   exit(1);
}

class ftpgetJob : public GetJob
{
public:
   ftpgetJob(Ftp *f,ArgV *args) : GetJob(f,0,args) {}

   void ShowRunStatus(StatusLine *s)
   {
      s->Show("%d",offset);
   }
};

int   main(int argc,char **argv)
{
   int   port=21;
   int   c;
   char  *user=NULL;
   char  *pass=NULL;
   extern char *optarg;
   extern int  optind;
   int   list=0;
   int   cont=0;
   char  *host_name;
   int	 res;
   int	 flags=0;

   enum
   {
      NOREST_MODE_OPT=256,
      SYNC_MODE_OPT
   };

   static struct option ftpget_options[]=
   {
      {"norest-mode",no_argument,0,NOREST_MODE_OPT},
      {"sync-mode",no_argument,0,SYNC_MODE_OPT},
      {"continue",no_argument,0,'c'},
      {"user",required_argument,0,'u'},
      {"post",required_argument,0,'p'},
      {"list",no_argument,0,'l'},
      {0,0,0,0}
   };

   program=argv[0];

   parse_long_options(argc,argv,"ftpget",version_string,PrintUsage);

   while((c=getopt_long(argc,argv,"+p:u:lc",ftpget_options,0))!=-1)
   {
      switch(c)
      {
      case('p'):
         if(sscanf(optarg,"%d",&port)!=1)
         {
            fprintf(stderr,"%s: invalid port number\n",program);
	    exit(1);
         }
         break;
      case('u'):
         user=optarg;
         pass=strchr(optarg,',');
	 if(pass==NULL)
	    pass=strchr(optarg,' ');
	 if(pass==NULL)
	 {
	    pass=GetPass("Password: ");
	 }
	 else
	 {
	    *pass=0;
	    pass++;
         }
	 break;
      case('l'):
         list=1;
         break;
      case('c'):
         cont=1;
         break;
      case(SYNC_MODE_OPT):
	 flags|=Ftp::SYNC_MODE;
	 break;
      case(NOREST_MODE_OPT):
	 flags|=Ftp::NOREST_MODE;
	 break;
      case('?'):
         fprintf(stderr,"Try `%s --help' for more information\n",program);
	 exit(1);
      }
   }

   if(optind+2>argc)
      PrintUsage(1);

   Ftp *f=new Ftp;

   res=f->Connect(host_name=argv[optind++],port);
   if(res<0)
   {
      fprintf(stderr,"%s: %s - %s\n",
		      program,
			  host_name,
			       f->StrError(res));
      exit(1);
   }
   f->SetFlag(flags,1);
   if(user)
      f->Login(user,pass);

   ArgV *args=new ArgV;
   args->Append(list?"ls":(cont?"reget":"get"));
   args->Append("--");

   for( ; optind<argc; optind++)
      args->Append(argv[optind]);

   ftpgetJob *j=new ftpgetJob(f,args);
   StatusLine *s=new StatusLine(1);
   while(!j->Done())
   {
      j->ShowRunStatus(s);
      SMTask::Schedule();
      SMTask::Block();
   }
   j->SayFinal();
   return(j->ExitCode());
}
