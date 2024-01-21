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
#include "CmdExec.h"
#include "alias.h"
#include "xmalloc.h"
#include "xalloca.h"
#include <string.h>

CmdExec::parse_result CmdExec::parse_one_cmd()
{
   int	 in_quotes;

#define quotable(ch) (strchr("\"\\",ch) \
		      || (!in_quotes && strchr(" \t>|;&",ch)))

   char *line=next_cmd;
   char *nextarg=(char*)alloca(strlen(line)+1);
   char *endarg;
   const char *alias=0;

   if(args)
      args->Empty();
   else
      args=new ArgV;
   if(cmd)
   {
      free(cmd);
      cmd=0;
   }
   if(output)
   {
      delete output;
      output=0;
   }

   char redir_type=0;
   char *redir_file=0;
   background=0;

   if(line==0)
   {
      // empty command
      return PARSE_OK;
   }

   if(line[0]=='&' && line[1]=='&')
   {
      condition=COND_AND;
      line+=2;
   }
   else if(line[0]=='|' && line[1]=='|')
   {
      condition=COND_OR;
      line+=2;
   }
   else
   {
      condition=COND_ANY;
   }

   // loop for all arguments
   for(;;)
   {
      // skip leading whitespace
      while(*line==' ' || *line=='\t' || (args->count()==0 && *line=='\n'))
	 line++;

      if(*line==0 || *line=='\n'
      || *line=='|' || *line=='>' || *line==';' || *line=='&')
	 break;

      // endarg points just beyond the last char of arg
      endarg=nextarg;

      if(args->count()==0 && *line=='!')
      {
	 // shell command -- it ends only with '\n'
	 args->Append("!");
	 line++;
	 while(*line==' ' || *line=='\t')
	    line++;
	 while(*line!='\n' && *line)
	    *endarg++=*line++;
	 next_cmd=line;
	 if(*next_cmd=='\n')
	    next_cmd++;
	 *endarg=0;
	 if(*nextarg)
	    args->Append(nextarg);
	 return PARSE_OK;
      }

      if(args->count()==0 && *line=='(')
      {
	 line++;
	 args->Append("(");

	 int level=1;
	 in_quotes=0;
	 for(;;)
	 {
	    if(*line==0)
	       return PARSE_AGAIN;
	    if(*line=='\\' && line[1] && (strchr("\"\\",line[1])
			         || (level==1 && line[1]==')')))
	    {
	       *endarg++=*line++;
	    }
	    else
	    {
	       if(!in_quotes)
	       {
		  if(*line==')')
		  {
		     if(--level==0)
			break;
		  }
		  else if(*line=='(')
		     level++;
	       }
	       if(*line=='"')
		  in_quotes=!in_quotes;
	    }
	    *endarg++=*line++;
	 }
	 *endarg=0;
	 args->Append(nextarg);
	 line++;  // skip )
	 while(*line==' ' || *line=='\t')
	    line++;
	 goto cmd_end;
      }

      // loop for one argument
      in_quotes=0;
      for(;;)
      {
	 if(*line=='\\' && quotable(line[1]))
	 {
	    line++;
	 }
	 else
	 {
	    if(*line==0 || *line=='\n'
	    || (!in_quotes && (*line==' ' || *line=='\t'
		     || *line=='>' || *line=='|' || *line==';' || *line=='&')))
	       break;
	    if(*line=='"')
	    {
	       in_quotes=!in_quotes;
	       line++;
	       continue;
	    }
	 }
	 *endarg++=*line++;
      }
      *endarg=0;
      if(args->count()==0 && !alias)
      {
	 alias=Alias::Find(nextarg);
      	 if(alias)
	 {
	    if((unsigned)(line-cmd_buf) < strlen(alias))
	    {
	       int offs=line-cmd_buf;
	       cmd_buf=(char*)xrealloc(cmd_buf,strlen(line)+1+strlen(alias));
	       memmove(cmd_buf+strlen(alias),cmd_buf+offs,strlen(cmd_buf+offs)+1);
	       line=cmd_buf;
	    }
	    else
	    {
	       line-=strlen(alias);
	    }
	    memcpy(line,alias,strlen(alias));
	    continue;
	 }
      }
      args->Append(nextarg);
   }

   if((line[0]=='&' && line[1]=='&')
   || (line[0]=='|' && line[1]=='|'))
   {
      next_cmd=line;
      return PARSE_OK;
   }

   if(*line=='>' || *line=='|')
   {
      redir_type=*line;
      line++;
      if(*line=='>')
      {
	 // '>>' means append
	 redir_type='+';
	 line++;
      }

      // skip leading whitespace
      while(*line==' ' || *line=='\t')
	 line++;

      if(*line==0 || *line=='\n' || *line==';' || *line=='&')
      {
	 fprintf(stderr,"Missing %s\n",redir_type=='|'?"filter command":"filename");
	 if(*line==';' || *line=='&' || *line=='\n')
	    next_cmd=line+1;
	 else
	    next_cmd=line;
	 return PARSE_ERR;
      }

      redir_file=endarg=nextarg;

      in_quotes=0;
      for(;;)
      {
	 if(*line=='\\' && quotable(line[1]))
	    line++;
	 else
	 {
	    // filename can end with a space, filter command can't.
	    if(*line==0 || *line=='\n' || (!in_quotes
		  && ((redir_type!='|' && (*line==' '||*line=='\t'))
		      || *line==';' || *line=='&')))
	       break;
	    if(*line=='"')
	    {
	       in_quotes=!in_quotes;
	       line++;
	       continue;
	    }
	 }
	 *endarg++=*line++;
      }
      *endarg=0;
   }

cmd_end:
   if((line[0]=='&' && line[1]=='&')
   || (line[0]=='|' && line[1]=='|'))
   {
      next_cmd=line;
   }
   else if(*line==';' || *line=='&' || *line=='\n')
   {
      next_cmd=line+1;
      if(*line=='&')
	 background=1;
   }
   else
   {
      next_cmd=line;
   }

   switch(redir_type)
   {
   case('|'):
      output=new OutputFilter(redir_file);
      break;
   case('>'):
      output=new FileStream(redir_file,O_WRONLY|O_TRUNC|O_CREAT);
      break;
   case('+'):
      output=new FileStream(redir_file,O_WRONLY|O_APPEND|O_CREAT);
      break;
   }

   return PARSE_OK;

#undef quotable
}
