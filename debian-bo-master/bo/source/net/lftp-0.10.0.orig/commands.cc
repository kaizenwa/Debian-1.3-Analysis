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
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <ctype.h>

#include "CmdExec.h"
#include "XferJob.h"
#include "mgetJob.h"
#include "mputJob.h"
#include "mkrmJob.h"
#include "SysCmdJob.h"
#include "SiteJob.h"
#include "MirrorJob.h"
#include "mvJob.h"

#include "alias.h"
#include "netrc.h"
#include "url.h"
#include "lftp.h"
#include "vars.h"
#include "GetPass.h"

const struct CmdExec::cmd_rec CmdExec::cmd_table[]=
{
   {"!",       do_shell,   "!<shell_command>",
	 "Launch shell or shell command\n"},
   {"(",       do_subsh,   "(commands)",
	 "Group commands together to be executed as one command\n"
	 "You can launch such a group in background\n"},
   {"?",       do_help,	   0,"help"},
   {"alias",   do_alias,   "alias [<name> [<value>]]",
	 "Define or undefine alias <name>. If <value> omitted,\n"
	 "the alias is undefined, else is takes the value <value>\n"},
   {"anon",    do_anon,    "anon",
	 "anon - login anonymously (by default)\n"},
   {"bye",     do_exit,	   0,"exit"},
   {"cat",     do_get,     "cat <files>",
	 "cat <files> - output remote files to stdin\n"},
   {"cd",      do_cd,      "cd <rdir>",
	 "Change current remote directory\n"},
   {"connect", do_open,	   0,"open"},
   {"debug",   do_debug,   "debug <level>|off"},
   {"exit",    do_exit,    "exit [bg|kill]",
	 "exit - exit from lftp\n"
	 "exit kill - kill all transfers and exit\n"},
   {"get",     do_get,     "get [-c] <rfile> [-o <lfile>]",
	 "Retrieve remote file <rfile> and store it to local file <lfile>.\n"
	 "If -o omitted, the file is stored to local file named as basename\n"
	 "of <rfile>. If option [-c] is specified, try to reget specified files.\n"},
   {"help",    do_help,    "help [<cmd>]",
	 "Print help for command <cmd>, or list of available commands\n"},
   {"jobs",    do_jobs,    "jobs [-v]",
	 "List running jobs. -v means verbose, several -v can be specified.\n"},
   {"kill",    do_kill,    "kill all|<job_no>",
	 "Delete specified job with <job_no> or all jobs\n"},
   {"lcd",     do_lcd,     "lcd <ldir>",
	 "Change current local directory <ldir>\n"},
   {"lftp",    do_open,	   0,"open"},
   {"login",   do_user,	   0,"user"},
   {"ls",      do_get,     "ls <params>",
	 "List remote files. You can redirect output of this command to file\n"
	 "to file or via pipe to external command.\n"},
   {"mget",    do_mget,    "mget [-c] [-d] <files>",
	 "Gets selected files with expanded wildcards\n"
	 " -c  continue, reget\n"
	 " -d  create directories the same as in file names and get the\n"
	 "     files into them instead of current directory\n"},
   {"mirror",  do_mirror,  "mirror [remote_dir [local_dir]]",
	 "Mirror specified remote directory to local directory\n"},
   {"mkdir",   do_mkrm,    "mkdir <dirs>",
	 "Make remote directories\n"},
   {"more",    do_get,     "more <files>",
	 "Same as `cat <files> | more'. if PAGER is set, it is used as filter\n"},
   {"mput",    do_mput,	   "mput [-c] [-d] <files>",
	 "Upload files matching patterns. -c means reput, -d means to create\n"
	 "remote directories and use the same remote name as local one. By default\n"
	 "It uses basename of local name as remote one.\n"},
   {"mv",      do_mv,	   "mv <file1> <file2>",
	 "Rename <file1> to <file2>\n"},
   {"open",    do_open,    "open <host>",
	 "open - select an ftp server\n"
	 "Usage: open [-e cmd] [-u user[,pass]] [-p port] <host|url>\n"},
   {"put",     do_put,     "put [-c] <lfile> [-o <rfile>]",
	 "Upload <lfile> with remote name <rfile>. If -o omitted, basename\n"
	 "of <lfile> is used as remote name. -c means reput, which requires\n"
	 "you to have permission to overwrite remote files in current directory.\n"
	 "Some of ftp servers (wu-ftpd2.4-beta11, ncftpd-?.?) can't be used\n"
	 "to restart upload because of bugs. In that case use `set norest y'.\n"},
   {"pwd",     do_pwd,     "pwd",
	 "Print current remote directory\n"},
   {"quit",    do_exit,	   0,"exit"},
   {"reget",   do_get,     "reget <rfile> [-o <lfile>]",
	 "Same as `get -c'\n"},
   {"reput",   do_put,     "reput <lfile> [-o <rfile>]",
	 "Same as `put -c'\n"},
   {"rm",      do_mkrm,	   "rm <files>",
	 "Remove remote files\n"},
   {"rmdir",   do_mkrm,	   "rmdir <dirs>",
	 "Remove remote directories\n"},
   {"set",     do_set,     "set [<var> <val>]"},
   {"site",    do_site,	   "site <site_cmd>",
	 "Execute site command <site_cmd> and output the result\n"
	 "You can redirect its output\n"},
   {"source",  do_source,  "source <file>",
	 "Execute commands recorded in file <file>\n"},
   {"user",    do_user,    "user <user> [<pass>]",
	 "Use specified info for remote login\n"},
   {"wait",    do_wait,	   "wait <jobno>",
	 "Wait for spacified job to terminate.\n"},
   {"zcat",    do_get,     "zcat <files>",
	 "Same as cat <files>, but filter each file trough zcat\n"},
   {"zmore",   do_get,     "zmore <files>",
	 "Same as more, but filter each file trough zcat\n"},

   {NULL,NULL}
};

CMD(lcd)
{
   if(args->count()<2)
   {
      fprintf(stderr,"Usage: %s local-dir\n",args->getarg(0));
      return 0;
   }
   char *cd_to=args->getarg(1);
   if(cd_to[0]=='~' && (cd_to[1]=='/' || cd_to[1]==0))
   {
      char *home=getenv("HOME")?:"/";
      cd_to=(char*)alloca(strlen(cd_to)+strlen(home));
      sprintf(cd_to,"%s%s",home,args->getarg(1)+1);
   }
   RestoreCWD();
   int res=chdir(cd_to);
   if(res==-1)
   {
      perror(cd_to);
      exit_code=1;
      return 0;
   }
   SaveCWD();
   if(interactive)
      printf("lcd ok, local cwd=%s\n",cwd);
   exit_code=0;
   return 0;
}

CMD(get)
{
   if(!strcmp(args->getarg(0),"ls") && args->count()==1)
      args->Append(var_ls);
   Job *j=new GetJob(Clone(),output,args);
   output=0;
   return j;
}

CMD(mget)
{
   Job *j=new mgetJob(Clone(),args);
   args=0;
   return j;
}

CMD(put)
{
   Job *j=new PutJob(Clone(),args);
   return j;
}

CMD(mput)
{
   Job *j=new mputJob(Clone(),args);
   return j;
}

CMD(shell)
{
   Job *j;
   if(args->count()==1)
      j=new SysCmdJob("exec $SHELL");
   else
      j=new SysCmdJob(args->getarg(1));
   return j;
}

CMD(mkrm)
{
   Job *j=new mkrmJob(Clone(),args);
   args=0;
   return j;
}

CMD(source)
{
   if(args->count()<2)
   {
      printf("Usage: source <file>\n");
      return 0;
   }
   source=new FileStream(args->getarg(1),O_RDONLY);
   source_buf=source_buf_ptr=0;
   source_buf_size=0x1000/2;
   return 0;
}

CMD(jobs)
{
   int opt;
   args->rewind();
   int v=1;
   while((opt=args->getopt("+v"))!=EOF)
   {
      switch(opt)
      {
      case('v'):
	 v++;
	 break;
      case('?'):
	 printf("Usage: jobs [-v] [-v] ...\n");
	 return 0;
      }
   }
   Job::ListJobs(v);
   exit_code=0;
   return 0;
}

CMD(cd)
{
   if(args->count()!=2)
   {
      fprintf(stderr,"Usage: cd remote-dir\n");
      return 0;
   }

   char c;
   if(sscanf(args->getarg(1),"%*[a-z]://%*[^/]%c",&c)==1)
      return do_open();

   GetBetter();
   session->Chdir(args->getarg(1));
   return this;
}

CMD(pwd)
{
   const char *cwd=session->GetCwd();
   printf("%s\n",cwd[0]?cwd:"~");
   exit_code=0;
   return 0;
}

static void  move_to_background()
{
   fflush(stdout);
   switch(fork())
   {
   case(0): // child
      signal(SIGINT,SIG_IGN);
      signal(SIGQUIT,SIG_IGN);
      signal(SIGHUP,SIG_IGN);
      signal(SIGTSTP,SIG_IGN);

      for(;;)
      {
	 SMTask::Schedule();
	 if(Job::NumberOfJobs()==0)
	    break;
	 SMTask::Block();
      }
      exit(0);

   default: // parent
      printf("Moving into background to complete transfers...\n");
      fflush(stdout);
      _exit(0);
   case(-1):
      perror("fork()");
   }
}

CMD(exit)
{
   int bg=0,kill=0;
   if(args->count()>=2)
   {
      if(!strcmp(args->getarg(1),"kill"))
	 kill=1;
      else if(!strcmp(args->getarg(1),"bg"))
	 bg=1;
      else
      {
	 printf("Usage: %s [bg|kill]\n",args->getarg(0));
	 return 0;
      }
   }
   if(Job::NumberOfJobs()>0 && !kill)
   {
      move_to_background();
/*
      printf("There are running jobs\n\007");
      printf("Use 'exit bg' or 'exit kill' to exit\n");
      return 0;
*/
   }
   exit(0);
}

CMD(debug)
{
   FILE	 *new_dfile=stderr;
   int	 new_dlevel=9;

   if(args->count()>1)
   {
      if(!strcmp(args->getarg(1),"off"))
      {
	 new_dfile=NULL;
	 new_dlevel=0;
      }
      else
      {
	 new_dlevel=atoi(args->getarg(1));
      }
   }
   if(new_dfile)
   {
      printf("debug level %d\n",new_dlevel);
   }
   else
   {
      printf("debug off\n");
   }
   GetBetter();
   session->SetDebug(new_dfile,new_dlevel);
   exit_code=0;
   return 0;
}

CMD(user)
{
   char	 *pass;
   if(args->count()<2 || args->count()>3)
   {
      fprintf(stderr,"Usage: %s userid [pass]\n",args->getarg(0));
      return 0;
   }
   if(args->count()==2)
      pass=GetPass("Password: ");
   else
      pass=args->getarg(2);
   if(pass)
      session->Login(args->getarg(1),pass);
   exit_code=0;
   return 0;
}
CMD(anon)
{
   session->AnonymousLogin();
   exit_code=0;
   return 0;
}

CMD(open)
{
   int	 debug=-1;
   int	 port=-1;
   char	 *host=NULL;
   char  *path=NULL;
   char	 *user=NULL;
   char	 *pass=NULL;
   url_components uc;
   int	 c;
   NetRC::Entry *nrc=0;
   char	 *cmd_to_exec=0;

   args->rewind();
   while((c=args->getopt("u:p:e:d"))!=EOF)
   {
      switch(c)
      {
      case('p'):
	 if(!isdigit(optarg[0]))
	 {
	    struct servent *serv=getservbyname(optarg,"tcp");
	    if(serv==NULL)
	    {
	       fprintf(stderr,"%s: %s - no such tcp service\n",args->getarg(0),optarg);
	       return 0;
	    }
	 }
	 else
	    port=atoi(optarg);
	 break;
      case('u'):
         user=optarg;
         pass=strchr(optarg,',');
	 if(pass==NULL)
	    pass=strchr(optarg,' ');
	 if(pass==NULL)
   	    break;
	 *pass=0;
	 pass++;
         break;
      case('d'):
	 debug=9;
	 break;
      case('e'):
	 cmd_to_exec=optarg;
	 break;
      case('?'):
	 fprintf(stderr,"Usage: %s [-p port] [-u user[,pass]] host\n",
	    args->getarg(0));
      }
   }

   if(debug!=-1)
      session->SetDebug(stderr,debug);

   if(optind<args->count())
      host=args->getarg(optind++);

   if(host)
   {
      if(parse_url(host,&uc))
      {
	 if(strcmp(uc.proto,"ftp"))
	 {
	    fprintf(stderr,"%s: %s - not supported protocol\n",args->getarg(0),uc.proto);
	    return 0;
	 }
	 if(uc.user[0] && !user)
	    user=uc.user;
	 host=uc.host;
	 if(uc.port!=-1 && port==-1)
	    port=uc.port;
	 if(uc.path[0] && !path)
	    path=uc.path;
      }
      nrc=NetRC::LookupHost(host);
      if(nrc)
      {
	 if(nrc->user && !user)
	    user=nrc->user;
	 if(nrc->pass && !pass)
	    pass=nrc->pass;
      }

      if(port==-1)
	 port=21;
      int res=session->Connect(host,port);
      if(res<0)
      {
	 fprintf(stderr,"%s: %s - %s\n",args->getarg(0),host,session->StrError(res));
	 if(nrc) delete nrc;
	 return 0;
      }
      session->SetFlag(Ftp::SYNC_MODE,sync_mode);
      session->SetFlag(Ftp::NOREST_MODE,norest_mode);
   }
   if(user)
   {
      if(!pass)
	 pass=GetPass("Password: ");
      if(!pass)
	 fprintf(stderr,"%s: GetPass() failed: %s -- assume anonymous login\n",
	    args->getarg(0),strerror(errno));
      else
	 session->Login(user,pass);
   }

   if(nrc)
      delete nrc;

   if(cmd_to_exec)
      PrependCmd(cmd_to_exec);

   if(path)
   {
      GetBetter();
      session->Chdir(path);
      return this;
   }
   exit_code=0;
   return 0;
}

CMD(kill)
{
   if(args->count()<2)
   {
      printf("Usage: %s <jobno> ... | all\n",args->getarg(0));
      return 0;
   }
   if(!strcmp(args->getarg(1),"all"))
   {
      Job::KillAll();
      exit_code=0;
      return 0;
   }
   args->rewind();
   for(;;)
   {
      char *arg=args->getnext();
      if(arg==0)
	 break;
      if(!isdigit(arg[0]))
      {
	 fprintf(stderr,"%s: `%s' - not a number\n",args->getarg(0),arg);
      	 continue;
      }
      int n=atoi(arg);
      if(Job::Running(n))
	 Job::Kill(n);
      else
	 fprintf(stderr,"%s: %d: No such job\n",args->getarg(0),n);
   }
   exit_code=0;
   return 0;
}

const char * const CmdExec::var_list[]=
{
   "ls-default","prompt","remote-completion","norest-mode","sync-mode",
   "long-running","timeout","redial-interval",
   NULL
};

CMD(set)
{
   if(args->count()<3)
   {
      if(args->count()!=1)
      {
	 printf("Usage: %s <variable> <value>\n",args->getarg(0));
	 return 0;
      }
      printf("set long-running %d\n",long_running);
      printf("set ls-default \"%s\"\n",var_ls);
      printf("set norest-mode %s\n",var_bool2str(norest_mode));
      printf("set prompt \"%s\"\n",var_prompt);
      printf("set redial-interval %d\n",var_redial_interval);
      printf("set remote-completion %s\n",var_bool2str(remote_completion));
      printf("set sync-mode %s\n",var_bool2str(sync_mode));
      printf("set timeout %d\n",var_timeout);
      exit_code=0;
      return 0;
   }
   exit_code=0;
   const char *v;
   if(!find_var_name(args->getarg(1),var_list,&v))
   {
      printf("Unknown variable: `%s'. Use `set' to look at all variables.\n",
	 args->getarg(1));
      exit_code=1;
      return 0;
   }
   if(!v)
   {
      printf("Ambiguous variable name: `%s'. Use `set' to look at all variables.\n",
	 args->getarg(1));
      exit_code=1;
      return 0;
   }

   char *c=args->Combine(2);

   if(!strcmp(v,"ls-default"))
   {
      free(var_ls);
      var_ls=c;
      return 0;
   }
   if(!strcmp(v,"prompt"))
   {
      free(var_prompt);
      var_prompt=c;
      return 0;
   }
   if(!strcmp(v,"remote-completion"))
   {
      remote_completion=var_str2bool(c);
      free(c);
      return 0;
   }
   if(!strcmp(v,"sync-mode"))
   {
      sync_mode=var_str2bool(c);
      session->SetFlag(Ftp::SYNC_MODE,sync_mode);
      free(c);
      return 0;
   }
   if(!strcmp(v,"norest-mode"))
   {
      norest_mode=var_str2bool(c);
      session->SetFlag(Ftp::NOREST_MODE,norest_mode);
      free(c);
      return 0;
   }
   if(!strcmp(v,"long-running"))
   {
      long_running=atoi(c);
      if(long_running<0)
	 long_running=0;
      free(c);
      return 0;
   }
   if(!strcmp(v,"timeout"))
   {
      var_timeout=atoi(c);
      if(var_timeout<30)
	 var_timeout=30;
      session->SetTimeout(var_timeout);
      free(c);
      return 0;
   }
   if(!strcmp(v,"redial-interval"))
   {
      var_redial_interval=atoi(c);
      if(var_redial_interval<0)
	 var_redial_interval=0;
      session->SetSleep(var_redial_interval);
      free(c);
      return 0;
   }
   abort(); // we should have handled all cases
}

CMD(alias)
{
   if(args->count()<2)
   {
      Alias::List();
   }
   else if(args->count()==2)
   {
      Alias::Del(args->getarg(1));
   }
   else
   {
      char *val=args->Combine(2);
      Alias::Add(args->getarg(1),val);
      free(val);
   }
   exit_code=0;
   return 0;
}

CMD(wait)
{
   if(args->count()!=2)
   {
      printf("Usage: wait <jobno>\n");
      return 0;
   }
   args->rewind();
   char *jn=args->getnext();
   if(!isdigit(jn[0]))
   {
      fprintf(stderr,"wait: <jobno> must be a number\n");
      return 0;
   }
   Job *j=FindJob(atoi(jn));
   if(j==0)
   {
      fprintf(stderr,"wait: no such job %s\n",jn);
      return 0;
   }
   if(j->parent && j->parent->waiting==j)
   {
      fprintf(stderr,"wait: some other job waits for job %s\n",jn);
      return 0;
   }
   j->parent=0;
   return j;
}

CMD(site)
{
   if(args->count()<=1)
   {
      printf("Usage: site <site_cmd>\n");
      return 0;
   }
   Job *j=new SiteJob(Clone(),args->Combine(1),
      output?:new FDStream(1,"<stdout>"));
   output=0;
   return j;
}

CMD(subsh)
{
   CmdExec *e=new CmdExec(Clone());
   char *c=args->getarg(1);
   e->FeedCmd(c);
   e->cmdline=(char*)xmalloc(strlen(c)+3);
   sprintf(e->cmdline,"(%s)",c);
   return e;
}

CMD(mirror)
{
   // mirror remote local
   char cwd[1024];
   if(args->count()>=3)
   {
      strcpy(cwd,args->getarg(2));
      create_directories(cwd);
   }
   else
   {
      if(getcwd(cwd,sizeof(cwd))==0)
      {
	 perror("getcwd()");
	 return 0;
      }
   }
   const char *rcwd=session->GetCwd();
   if(args->count()>=2)
      rcwd=args->getarg(1);
   Job *j=new MirrorJob(Clone(),cwd,rcwd);
   return j;
}

CMD(mv)
{
   if(args->count()!=3)
   {
      printf("Usage: mv <file1> <file2>\n");
      return 0;
   }
   Job *j=new mvJob(Clone(),args->getarg(1),args->getarg(2));
   return j;
}
