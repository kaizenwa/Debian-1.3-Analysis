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
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <ctype.h>

#include "xalloca.h"
#include "ftpclass.h"
#include "lftp.h"
#include "xmalloc.h"
#include "alias.h"
#include "CmdExec.h"
#include "SignalHook.h"
#include "GetPass.h"

extern "C" {
#include "readline/readline.h"
#include "readline/history.h"
}

char  *program="lftp";

int   remote_completion=0;

char  *home=getenv("HOME")?:".";

int   create_directories(char *path)
{
   char  *sl=path;
   int	 res;

   if(access(path,0)==0)
      return 0;

   for(;;)
   {
      sl=strchr(sl,'/');
      if(sl==path)
      {
	 sl++;
	 continue;
      }
      if(sl)
	 *sl=0;
      res=mkdir(path,0755);
      if(res==-1)
      {
	 if(errno!=EEXIST)
	 {
	    fprintf(stderr,"mkdir(%s): %s\n",path,strerror(errno));
      	    if(sl)
	       *sl='/';
	    return(-1);
	 }
      }
      if(sl)
	 *sl++='/';
      else
	 break;
   }
   return 0;
}

void  print_product_title(FILE *file)
{
   fprintf(file,
      "Lftp | Version " VERSION " | Copyright (c) 1996-97 Alexander V. Lukyanov\n"
      "This is free software with ABSOLUTELY NO WARRANTY. See COPYING for details.\n");
}

void CmdExec::print_cmd_help(const char *cmd)
{
   for(int i=0; cmd_table[i].name; i++)
   {
      if(!strcmp(cmd,cmd_table[i].name))
      {
	 if(cmd_table[i].long_desc==0)
	 {
	    if(cmd_table[i].short_desc)
	       puts(cmd_table[i].short_desc);
	    else
	       printf("Sorry, no help for %s\n",cmd);
	    return;
	 }
	 if(!strchr(cmd_table[i].long_desc,' '))
	 {
	    printf("%s is a built-in alias for %s\n",cmd,cmd_table[i].long_desc);
	    print_cmd_help(cmd_table[i].long_desc);
	    return;
	 }
	 puts(cmd_table[i].short_desc);
	 printf("%s",cmd_table[i].long_desc);
	 return;
      }
   }
   const char *a=Alias::Find(cmd);
   if(a)
   {
      printf("%s is an alias to `%s'\n",a,cmd);
      return;
   }
   printf("No such command `%s'. Use `help' to see available commands.\n",cmd);
}

CMD(help)
{
   if(args->count()>1)
   {
      args->rewind();
      for(;;)
      {
	 char *cmd=args->getnext();
	 if(cmd==0)
	    break;
	 print_cmd_help(cmd);
      }
      return 0;
   }
   print_product_title(stdout);
   printf("lftp usage: %s [-d] [-e <cmd>] [-p port] [-u user,pass] [host]\n","lftp");
   printf("\nAvailable commands:\n\n");

   int i=0;
   const char *c1;
   while(cmd_table[i].name)
   {
      while(cmd_table[i].name && !cmd_table[i].short_desc)
	 i++;
      if(cmd_table[i].name)
      {
	 c1=cmd_table[i].short_desc;
	 i++;
	 while(cmd_table[i].name && !cmd_table[i].short_desc)
	    i++;
	 if(cmd_table[i].name)
	 {
	    printf("\t%-35s %s\n",c1,cmd_table[i].short_desc);
	    i++;
	 }
	 else
	    printf("\t%s\n",c1);
      }
   }
   return 0;
}

void  hook_signals()
{
   SignalHook::DoCount(SIGINT);
   SignalHook::DoCount(SIGHUP);
   SignalHook::DoCount(SIGTSTP);
   SignalHook::DoCount(SIGPIPE);
}

void  exec_until_eof(CmdExec *exec,FILE *in_file)
{
   for(;;)
   {
      for(;;)
      {
	 SMTask::Schedule();
	 if(exec->Done())
	    break;
	 SMTask::Block();
      }
      if(exec->interactive)
      {
	 exec->ListDoneJobs();
	 exec->BuryDoneJobs();
      }
      int user_input=(in_file==stdin && isatty(fileno(in_file)));

      ::completion_session=exec->session;
      ::remote_completion=exec->remote_completion;

      char *cmd_buf=user_input
		     ?readline(exec->MakePrompt())
		     :readline_from_file(in_file);
      ::completion_session=0;

      if(cmd_buf==NULL)
	 return;

      if(user_input && *cmd_buf)
	 add_history(cmd_buf);

      exec->FeedCmd(cmd_buf);
   }
}

int   main(int argc,char **argv)
{
   CmdExec *top_exec=new CmdExec(new Ftp());
   top_exec->session->SetDebug(NULL,0);
   top_exec->jobno=-1;
   top_exec->interactive=true;
   top_exec->status_line=new StatusLine(1);

   initialize_readline();

   hook_signals();

   char	 *rc=(char*)alloca(strlen(home)+8+1);
   sprintf(rc,"%s/.lftprc",home);
   FILE  *in_file=fopen(rc,"r");
   if(in_file!=NULL)
   {
      exec_until_eof(top_exec,in_file);
      fclose(in_file);
   }

   char *slash=strrchr(argv[0],'/');
   if(slash)
      memmove(argv[0],slash+1,strlen(slash));

   top_exec->ExecParsed(new ArgV(argc,argv));

   for(;;)
   {
      exec_until_eof(top_exec,stdin);
      puts("exit");
      top_exec->FeedCmd("exit");
   }
}
