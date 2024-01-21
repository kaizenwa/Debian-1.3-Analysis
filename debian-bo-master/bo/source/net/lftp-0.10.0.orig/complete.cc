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
#include <sys/types.h>
#include <time.h>
#include "xalloca.h"
#include "xmalloc.h"
#include "ftpclass.h"
#include "lftp.h"
#include "rglob.h"
#include "CmdExec.h"
#include "GetPass.h"

extern "C" {
#include "readline/readline.h"
}

static len;    // lenght of the word to complete
static cindex; // index in completion array
static char **glob_res=NULL;

char *CmdExec::command_generator(char *text,int state)
{
   const char *name;

   /* If this is a new word to complete, initialize now.  This includes
      saving the length of TEXT for efficiency, and initializing the cindex
      variable to 0. */
   if(!state)
      cindex=0;

   /* Return the next name which partially matches from the command list. */
   while ((name=cmd_table[cindex++].name))
      if(strncmp(name,text,len)==0)
	 return(xstrdup(name));

   /* If no names matched, then return NULL. */
   return(NULL);
}

char *remote_generator(char *text,int state)
{
   char *name;

   /* If this is a new word to complete, initialize now.  This includes
      saving the length of TEXT for efficiency, and initializing the cindex
      variable to 0. */
   if(!state)
      cindex=0;

   if(glob_res==NULL)
      return NULL;

   while((name=glob_res[cindex++])!=NULL)
      if(strncmp(name,text,len)==0)
	 return(xstrdup(name));

   glob_res=NULL;
   return NULL;
}

// returns:
//    2 - remote dir
//    1 - remote file
//    0 - local
int   remote_cmd(int start)
{
   // try to guess whether the completion word is remote

   char *scan=rl_line_buffer;
   while(*scan==' ' || *scan=='\t') scan++;
   if(!strncmp(scan,"cd ",3))
      return 2;

   if(!strncmp(scan,"ls ",3))
      return 1;

   int was_o=(start-4>scan-rl_line_buffer
	      && !strncmp(rl_line_buffer+start-4," -o ",4));
   if((!strncmp(scan,"get ",4)
    || !strncmp(scan,"more ",5)
    || !strncmp(scan,"cat ",4)
    || !strncmp(scan,"zcat ",5)
    || !strncmp(scan,"zmore ",6)
   ) && !was_o)
      return 1;
   if(!strncmp(scan,"put ",4) && was_o)
      return 1;

   return 0;
}

Ftp *completion_session;

/* Attempt to complete on the contents of TEXT.  START and END show the
   region of TEXT that contains the word to complete.  We can use the
   entire line in case we want to do some simple parsing.  Return the
   array of matches, or NULL if there aren't any. */
char **lftp_completion (char *text,int start,int end)
{
   if(start==0)
   {
      len=end;
      return completion_matches(text,CmdExec::command_generator);
   }
   int remote_type=0;
   if(remote_completion && (remote_type=remote_cmd(start)))
   {
      len=end-start;
      char *pat=(char*)alloca(len+2);
      strncpy(pat,text,len);
      pat[len]=0;
      if(strchr(pat,'*') || strchr(pat,'?'))
	 return(NULL);
      char *sl=strrchr(pat,'/');
      if(sl)
	 *sl=0;
      else
	 *pat=0;

      completion_session->DontSleep();

      RemoteGlob g(completion_session,pat,Ftp::LIST);
      g.SetSlashFilter(1);
      for(;;)
      {
	 SMTask::Schedule();
	 if(g.Done())
	    break;
	 SMTask::Block();
      }
      glob_res=g.GetResult();

      rl_filename_completion_desired=1;
      char **matches=completion_matches(text,remote_generator);
      if(matches && remote_type==2 /* cd */)
      {
	 if((*matches)[strlen(*matches)-1]==' ')
	    (*matches)[strlen(*matches)-1]='/';
      }
      return matches;
   }
   return(NULL);
}

/* Tell the GNU Readline library how to complete.  We want to try to complete
   on command names if this is the first word in the line, or on filenames
   if not. */
void initialize_readline ()
{
   /* Allow conditional parsing of the ~/.inputrc file. */
   rl_readline_name = "lftp";

   /* Tell the completer that we want a crack first. */
   rl_attempted_completion_function = (CPPFunction *)lftp_completion;

   rl_getc_function = (int (*)(...))lftp_rl_getc;
}
