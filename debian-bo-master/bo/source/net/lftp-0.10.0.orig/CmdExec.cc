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
#include <errno.h>
#include <unistd.h>
#include "CmdExec.h"
#include "xmalloc.h"
#include "SignalHook.h"
#include "alias.h"

CmdExec	 *CmdExec::cwd_owner=0;

void  CmdExec::SaveCWD()
{
   if(cwd==0)
      cwd=(char*)xmalloc(1024);
   if(getcwd(cwd,1024)==0)
   {
      // A bad case, but we can do nothing
      fprintf(stderr,"Warning: getcwd() failed\n");
      free(cwd);
      cwd=0;
   }
   else
   {
      if(cwd_owner==0)
	 cwd_owner=this;
   }
}
void  CmdExec::RestoreCWD()
{
   if(cwd==0 || cwd_owner==this)
      return;
   if(chdir(cwd)==0)
      cwd_owner=this;
}

void CmdExec::FeedCmd(const char *c)
{
   time(&start_time);
   if(cmd_buf==0)
   {
      cmd_buf=next_cmd=xstrdup(c);
      return;
   }
   int len=strlen(next_cmd);
   if(len>0 && next_cmd[len-1]!='\n')
      next_cmd[len++]='\n';
   memmove(cmd_buf,next_cmd,len);
   cmd_buf=next_cmd=(char*)xrealloc(cmd_buf,len+strlen(c)+1);
   strcpy(next_cmd+len,c);
   partial_cmd=false;
};

void CmdExec::PrependCmd(const char *c)
{
   time(&start_time);
   if(cmd_buf==0)
   {
      cmd_buf=next_cmd=xstrdup(c);
      return;
   }
   int len=strlen(c);
   int nl=(len>0 && c[len-1]!='\n');
   if(next_cmd-cmd_buf<len+1)
      cmd_buf=(char*)xrealloc(cmd_buf,len+nl+strlen(next_cmd));
   memmove(cmd_buf+len+nl,next_cmd,strlen(next_cmd)+1);
   memcpy(cmd_buf,c,len);
   if(nl)
      cmd_buf[len]='\n';
   next_cmd=cmd_buf;
}

void  CmdExec::exec_parsed_command()
{
   switch(condition)
   {
   case(COND_ANY):
      break;
   case(COND_AND):
      if(exit_code!=0)
	 return;
      break;
   case(COND_OR):
      if(exit_code==0)
	 return;
      break;
   }

   exit_code=1;

   SignalHook::ResetCount(SIGINT);
   SignalHook::ResetCount(SIGHUP);
   SignalHook::ResetCount(SIGTSTP);

   const struct cmd_rec *c,*part_cmd=0;
   int part=0;
   char *cmd_name=args->getarg(0);
   for(c=cmd_table; c->name; c++)
   {
      if(!strcmp(c->name,cmd_name))
      {
	 break;
      }
      if(!strncmp(c->name,cmd_name,strlen(cmd_name)))
      {
	 part++;
	 part_cmd=c;
      }
   }
   if(!c->name && part<=0)
      fprintf(stderr,"Unknown command `%s'.\n",cmd_name);
   else if(part>1)
      fprintf(stderr,"Ambiguous command `%s'.\n",cmd_name);
   else
   {
      RestoreCWD();

      if(cmd==0)
	 cmd=args->Combine();

      Job *(CmdExec::*func)()=(part==1?part_cmd->func:c->func);
      waiting=(this->*func)();
      if(waiting==this) // builtin
	 return;
      if(waiting)
      {
	 waiting->parent=this;
	 if(waiting->jobno<0)
	    waiting->AllocJobno();
	 if(cmd && waiting->cmdline==0)
	 {
	    waiting->cmdline=cmd;
	    cmd=0;
      	 }
      }
      if(background)
      {
	 exit_code=0;
	 if(waiting)
	 {
	    waiting->Do();
	    if(!waiting->Done())
	    {
	       if(interactive)
	       {
		  printf("[%d] %s &\n",waiting->jobno,waiting->cmdline?:"?");
		  waiting->PrintStatus(1);
	       }
	       waiting=0;
	    }
	 }
      }
   }
}

void CmdExec::ExecParsed(ArgV *a,FDStream *o,bool b)
{
   if(args)
      delete args;
   args=a;
   if(cmd)
      free(cmd);
   cmd=args->Combine();
   if(output)
      delete output;
   output=o;
   background=b;
   condition=COND_ANY;
   exec_parsed_command();
}

int CmdExec::Done()
{
   return(waiting==0 && source==0
      && (next_cmd==0 || *next_cmd==0 || partial_cmd));
}

int CmdExec::Do()
{
   if(!interactive)
      BuryDoneJobs();	// if it is interactive, someone other will list done
			// jobs, otherwise we just bury them

   if(source)
   {
      // sourcing a file
      const inc=0x1000;
      int off=source_buf_ptr-source_buf;
      if(source_buf_size-off<inc)
      {
	 source_buf=(char*)xrealloc(source_buf,source_buf_size*=2);
	 source_buf_ptr=source_buf+off;
      }
      int fd=source->getfd();
      if(fd==-1)
      {
	 if(source->error())
	 {
	    fprintf(stderr,"source: %s\n",source->error_text);
	 source_err:
	    delete source;
	    source=0;
	    xfree(source_buf);
	    exit_code=1;
	    return MOVED;
	 }
	 block+=TimeOut(1000);
      	 return STALL;
      }
      int res=read(fd,source_buf_ptr,source_buf_size-off);
      if(res==-1)
      {
	 // error
	 if(errno==EAGAIN)
	    goto source_block;
	 perror(source->name);
	 goto source_err;
      }
      if(res==0)
      {
	 // eof
	 delete source;
	 source=0;
	 source_buf_ptr[0]=0;
	 PrependCmd(source_buf);
	 free(source_buf);
	 exit_code=0;
	 return MOVED;
      }
      source_buf_ptr+=res;
   source_block:
      block+=PollVec(fd,POLLIN);
      return MOVED;
   }

   if(waiting==this)
   {
      // cd builtin
      int res=session->Read(0,0);
      if(res==Ftp::SEE_ERRNO && (errno==EAGAIN || errno==EMFILE || errno==ENFILE))
      {
	 if(interactive)
	 {
	    if(SignalHook::GetCount(SIGINT))
	    {
	       puts("Interrupt");
	       session->Close();
	       waiting=0;
	       exit_code=1;
	       return MOVED;
	    }
	    if(SignalHook::GetCount(SIGHUP))
	       interactive=0;
	 }
	 return STALL;
      }
      if(res==0)
      {
	 // done
	 if(interactive)
	 {
	    printf("cd ok, cwd=%s\n",session->GetCwd());
	 }
	 session->Close();
	 exit_code=0;
	 waiting=0;
	 beep_if_long();
	 return MOVED;
      }
      // error
      fprintf(stderr,"cd: %s\n",session->StrError(res));
      session->Close();
      waiting=0;
      beep_if_long();
      exit_code=1;
      return MOVED;
   }

   if(waiting)
   {
      if(waiting->Done())
      {
 	 if(interactive)
	 {
 	    status_line->Show("");
	    waiting->SayFinal(); // final phrase like 'rm succeed'
	 }
	 exit_code=waiting->ExitCode();
	 delete waiting;
	 waiting=0;
	 beep_if_long();
      	 return MOVED;
      }
      if(interactive)
      {
	 if(SignalHook::GetCount(SIGINT))
	 {
	    status_line->Show("Interrupt");
	    putchar('\n');
	    exit_code=1;
	    delete waiting;
	    waiting=0;
	    return MOVED;
	 }
	 if(SignalHook::GetCount(SIGTSTP))
	 {
	    status_line->Show("[%d] %s &",waiting->jobno,waiting->cmdline);
	    putchar('\n');
	    waiting->PrintStatus(1);
	    exit_code=0;
	    waiting=0;
	    return MOVED;
	 }
	 if(SignalHook::GetCount(SIGHUP))
	 {
	    interactive=0;
	    return MOVED;
	 }
	 waiting->ShowRunStatus(status_line);
      }
      return STALL;
   }

   if(next_cmd==0 || *next_cmd==0)
      return STALL;

   switch(parse_one_cmd())
   {
   case(PARSE_ERR):
      return MOVED;
   case(PARSE_AGAIN):
      partial_cmd=true;
      return STALL;
   case(PARSE_OK):
      break;
   }
   if(args->count()==0)
      return MOVED;  // empty command

   if(interactive)
      session->DontSleep();
		     // We don't want to get a delay just after user
		     // entered a command.

   exec_parsed_command();
   return MOVED;
}

void CmdExec::ShowRunStatus(StatusLine *s)
{
   if(waiting && waiting!=this)
      waiting->ShowRunStatus(s);
   else if(Done())
      s->Show("");
}

void CmdExec::PrintStatus(int v)
{
   SessionJob::PrintStatus(v);
   if(v<1)
      return;
   if(source)
   {
      printf("\tSourcing `%s'\n",source->name);
      return;
   }
   if(waiting==this)
   {
      char *s=args->Combine();
      printf("\tExecuting builtin `%s' [%s]\n",s,session->CurrentStatus());
      free(s);
      return;
   }
   if(waiting)
   {
      printf("\tWaiting for job [%d] to terminate\n",waiting->jobno);
      return;
   }
   printf("\tRunning\n");
}

CmdExec::CmdExec(Ftp *f) : SessionJob(f)
{
   cmd=0;
   args=0;
   waiting=0;
   output=0;
   background=false;
   interactive=false;
   status_line=0;
   next_cmd=cmd_buf=0;
   partial_cmd=false;
   default_output=0;
   source=0;
   condition=COND_ANY;
   exit_code=0;

   cwd=0;
   SaveCWD();

   var_ls=xstrdup("");
   var_prompt=xstrdup("lftp> ");
   sync_mode=session->GetFlag(Ftp::SYNC_MODE);
   norest_mode=session->GetFlag(Ftp::NOREST_MODE);
   remote_completion=false;
   long_running=0;
   var_timeout=session->GetTimeout();
   var_redial_interval=session->GetSleep();

   start_time=0;
}

CmdExec::~CmdExec()
{
   free(cmd);
   if(args)
      delete args;
   if(output)
      delete output;
   free(cmd_buf);
   free(cwd);
   if(cwd_owner==this)
      cwd_owner=0;
}

char *CmdExec::MakePrompt()
{
   static char *prompt=0;
   static int prompt_size=256;

   if(prompt==0)
      prompt=(char*)xmalloc(prompt_size);

   if(partial_cmd)
   {
      return strcpy(prompt,"> ");
   }

   char *store=prompt;

   char *scan=var_prompt;
   char ch;
   char str[2]=" ";
   const char *to_add;

   *store=0;
   for(;;)
   {
      ch=*scan++;
      if(ch==0)
	 break;

      if(ch=='\\' && *scan && *scan!='\\')
      {
	 ch=*scan++;
	 switch(ch)
	 {
	 case 'w': // working directory
	    to_add=session->GetCwd();
	    if(to_add[0]==0)
	       to_add="~";
	    break;
	 case 'h':
	    to_add=session->GetHostName();
	    break;
	 case 'u':
	    to_add=session->GetUser();
	    break;
      	 default:
	    to_add="\\?";
	 }
      }
      else
      {
	 if(ch=='\\' && *scan=='\\')
	    scan++;
	 str[0]=ch;
	 to_add=str;
      }

      while(prompt_size<=store-prompt+(int)strlen(to_add))
	 prompt=(char*)xrealloc(prompt,prompt_size*=2);

      strcpy(store,to_add);
      store+=strlen(to_add);
   }
   return(prompt);
}

void CmdExec::beep_if_long()
{
   if(start_time!=0 && long_running!=0
   && time(0)-start_time>long_running
   && interactive && Done() && isatty(1))
      write(1,"\007",1);
}
