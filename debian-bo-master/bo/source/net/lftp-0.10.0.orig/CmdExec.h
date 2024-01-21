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

#ifndef CMDEXEC_H
#define CMDEXEC_H

#include "Job.h"
#include "ArgV.h"
#include "Filter.h"

#define	CMD(name) Job *CmdExec::do_##name()

class CmdExec : public SessionJob
{
// current command data
   char *cmd;
   ArgV *args;
   FDStream *output;
   bool background;

   char *next_cmd;
   char *cmd_buf;
   bool partial_cmd;

   enum
   {
      COND_ANY,
      COND_AND,
      COND_OR
   }
      condition;

   int	 exit_code;

   FDStream *source;
   char	 *source_buf;
   int	 source_buf_size;
   char	 *source_buf_ptr;

   void print_cmd_help(const char *cmd);
   void exec_parsed_command();

   enum parse_result
   {
      PARSE_OK,
      PARSE_ERR,
      PARSE_AGAIN
   };
   parse_result parse_one_cmd();

public:
   void FeedCmd(const char *c);
   void PrependCmd(const char *c);
   void ExecParsed(ArgV *a,FDStream *o=0,bool b=false);

   CMD(alias); CMD(anon);  CMD(cd);    CMD(debug);
   CMD(exit);  CMD(get);   CMD(help);  CMD(jobs);
   CMD(kill);  CMD(lcd);   CMD(ls);    CMD(mget);
   CMD(open);  CMD(put);   CMD(pwd);   CMD(set);
   CMD(shell); CMD(source);CMD(user);  CMD(mkrm);
   CMD(wait);  CMD(site);  CMD(subsh); CMD(mirror);
   CMD(mput);  CMD(mv);

   struct cmd_rec
   {
      const char  *name;
      Job   *(CmdExec::*func)();
      const char  *short_desc;
      const char  *long_desc;
   };
   static const cmd_rec cmd_table[];

   static const char * const var_list[];

   CmdExec(Ftp *s);
   ~CmdExec();

   int Done();
   int ExitCode() { return exit_code; }
   int Do();
   void PrintStatus(int);
   void ShowRunStatus(StatusLine *s);

   char *MakePrompt();

   bool interactive;
   StatusLine *status_line;

   static char *command_generator(char *,int);	  // readline completor

   char *var_ls;
   char *var_prompt;
   bool sync_mode;
   bool norest_mode;
   bool remote_completion;
   int	long_running;
   int	 var_timeout;
   int	 var_redial_interval;

   void	 beep_if_long();
   time_t start_time;

   static CmdExec *cwd_owner;
   char	 *cwd;
   void	 SaveCWD();
   void	 RestoreCWD();

   FDStream *default_output;
};

#endif CMDEXEC_H
