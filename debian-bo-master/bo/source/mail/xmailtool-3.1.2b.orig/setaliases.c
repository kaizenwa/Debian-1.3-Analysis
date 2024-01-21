/*

Copyright 1990 by Cray Research, Inc.

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of Cray Research, Inc. not be used in
advertising or publicity pertaining to distribution of the software without
specific, written prior permission.  Cray Research, Inc. makes no
representations about the suitability of this software for any purpose.  It
is provided "as is" without express or implied warranty.

*/

static char aliases_rcsid[]="$Id: setaliases.c,v 1.5 1995/03/15 23:29:33 bobo Exp $";

#include <stdio.h>
#include <unistd.h>
#include <X11/Xos.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include "defs.h"
#include "patchlevel.h"

extern int debug;
extern Display *dpy;

extern char *StepOverWhiteSpace(),*StepUnderWhiteSpace();

struct alias_ent {
	char *alias;
	char *exp;
	struct alias_ent *next;
} *alias_tab=(struct alias_ent *)0;


int remote_address;


/*
 * This is implemented as a stack.
 * each layer will grab a token, call expand_alias
 * The caller is responsible for freeing the pointer passed
 * to it.  The loop string is passed to indicate what the original
 * string is therefore preventing a possible alias loop.
 * The format of my addresses are such that a comma must separate
 * addresses.  Any token with a bracket or parenthesis will be
 * unexpanded
 */
char *
expand_alias(st,lev,loop)
char *st,*loop;
int lev;
{
struct alias_ent *alias_ptr;
char *addr;
char *exp,*tmp1_exp,*tmp2_exp,*tmp_exp,*tmp_addr;
int expand_flag=1,comma_flag=0;


	exp=(char *)malloc(5);
	strcpy(exp,"");


	if(debug)
	{
		if(st!=NULL)
			fprintf(stderr,"st= [%s]\n",st);
		else
			fprintf(stderr,"st=NULL\n");
	}

	if(lev>10)
	{
		warn("alias level > 10.\nThis is a possible alias loop.");
		return(exp);
	}

	if(st!=NULL)
	{
		if(strcspn(st,"{[<(") == strlen(st))
		{
		int x;

			for(x=1;x<strlen(st);x++)
			{
				if(st[x]==' ')
					st[x]=',';
			}
		}
	}

	/*
	 * grab a token
	 */
	addr=strtok(st,",\n\t");

	if((addr==NULL) && (st==NULL || strlen(StepOverWhiteSpace(st))==0))
		return(exp);

	if(addr==NULL)
	{
		addr=st;
	}

	addr=StepUnderWhiteSpace(StepOverWhiteSpace(addr));

	/*
	 * addresses that are enclosed in brackets, braces, and parens,
	 * are to be left alone.
	 */
	if(strcspn(addr,"{[<(") != strlen(addr))
	{

		expand_flag=0;
		tmp_addr=addr;

	}


	if(debug)
		fprintf(stderr,"addr = [%s]\n",addr);

	/*
	 * got one token.  Expand the rest of the line
	 */
	tmp1_exp=expand_alias((char *)NULL,lev,loop);

	alias_ptr=(struct alias_ent *)0;
	if(expand_flag && (strcmp(loop,addr)!=0))
	{
		/*
		 * find this addr in the alias table.
		 */
		for(alias_ptr=alias_tab;
				alias_ptr!=(struct alias_ent *)0;
				alias_ptr=alias_ptr->next)
		{
			if(strcmp(alias_ptr->alias,addr)==0)
				break;
		}
	}
	/*
	 * this alias is in the alias table.
	 * Expand it... if necessary.
	 */
	if(alias_ptr!=(struct alias_ent *)0)
	{
		tmp_exp=(char *)malloc(strlen(alias_ptr->exp)+1);
		strcpy(tmp_exp,alias_ptr->exp);
		tmp2_exp=expand_alias(tmp_exp,lev+1,addr);
		free(tmp_exp);
	}
	else
	{
		/*
		 * not in the alias table.  This is a pure
		 * address.  use it as is.
		 */
		tmp2_exp=(char *)malloc(strlen(addr)+1);
		strcpy(tmp2_exp,addr);
	}

	if(debug)
		fprintf(stderr,"tmp2_exp= [%s]\n",tmp2_exp);

	exp=(char *)realloc(exp,strlen(tmp1_exp)+strlen(tmp2_exp)+4);

	strcpy(exp," ");
	strcat(exp,tmp2_exp);

	if(strlen(tmp1_exp)!=0)
	{
		strcat(exp,", ");
		strcat(exp,tmp1_exp);
	}
	free(tmp1_exp);	
	free(tmp2_exp);	

	return(exp);
}

void
free_aliases()
{
struct alias_ent *alias_ptr;
struct alias_ent *alias_tmp;

	for(alias_ptr=alias_tab;
		alias_ptr != (struct alias_ent *)0;
		alias_ptr = alias_tmp)
	{
		free(alias_ptr->alias);
		free(alias_ptr->exp);
		alias_tmp=alias_ptr->next;
		free(alias_ptr);
	}

	alias_tab=(struct alias_ent *)0;
}



/*
 * Aliases as I'm defining them must be of the form
 *
 * alias name(space)["|']value["|']
 *
 * at this point however, this function is expecting the
 * alias and trailing space to be stripped.
 */
void
setaliases(ln)
char *ln;
{
struct alias_ent *alias_tmp,*alias_ptr;
static char *alias_line_buff=(char *)0;
char *exp;
char tmp_buff[2048];
int exp_len,exp_index;
static alias_line_len=2024;


	exp=strpbrk(ln," \t");
	exp[0]=(char)0;
	exp++;

	/*
	 * check for quotation marks.
	 *
	 */
	if((exp[0]=='\'') || (exp[0]=='\"'))
	{
		if( exp[0] != exp[strlen(exp)-1])
		{
			fprintf(stderr,"mismatched quote [%c]\n",
					exp[strlen(exp)-1]);
			return;
		}
		exp++;
		exp[strlen(exp)-1]=(char)0;
	}

	alias_tmp=(struct alias_ent *)malloc(sizeof(struct alias_ent));

	/*
	 * pretend that we are starting with a new alias.
	 */
	alias_tmp->exp=(char *)malloc(strlen(exp)+1);
	strcpy(alias_tmp->exp,exp);
	alias_tmp->alias=(char *)malloc(strlen(ln)+1);
	strcpy(alias_tmp->alias,ln);


	/*
	 * if the alias already exists we need to just copy
	 * the new version of the alias.
	 */
	for(alias_ptr=alias_tab;
		alias_ptr != (struct alias_ent *)0;
		alias_ptr = alias_ptr->next)
	{
		if(strcmp(alias_ptr->alias,alias_tmp->alias)==0)
		{
			if(debug)
				fprintf(stderr,
				"Duplicate aliase for [%s] old=[%s] new=[%s]\n",
					alias_ptr->alias,alias_ptr->exp,
					alias_tmp->exp);
			free(alias_ptr->exp);
			exp_len=strlen(exp);
			alias_ptr->exp=(char *)malloc(exp_len +1);
			strcpy(alias_ptr->exp,exp);
			free(alias_tmp->exp);
			free(alias_tmp->alias);
			free(alias_tmp);
			alias_tmp=(struct alias_ent *)0;
			break;
		}
	}

	if(debug)
		fprintf(stderr,"alias [%s] = [%s]\n",ln,exp);

	if(alias_tmp!=(struct alias_ent *)0)
	{
		alias_tmp->next=alias_tab;
		alias_tab=alias_tmp;
	}
}

exp_addrs(f)
char *f;
{
	char *new_file;
	int exp_fd,oldPos,lastPos;
	char line_buff[1024];
	char *tmp_ptr, *tmp_addr;
	int doing_header=0;
	FILE *exp_fp, *old_fp;


	if((old_fp=fopen(f,"r"))==NULL)
	{
		perror(f);
		warn("Couldn't open Out Bond Message file\n");
		XBell (dpy, 10);
		XFlush(dpy);
		return;
	}

	new_file=(char *)malloc(100);
	if((exp_fd=my_mkstemp(new_file,""))==-1)
	{
		warn("Couldn't create expanded Out Bound message file\n");
		XBell (dpy, 10);
		XFlush(dpy);
		free(new_file);
		fclose(old_fp);
		return;
	}
	
	if((exp_fp=fdopen(exp_fd,"w+"))==NULL)
	{
		perror("fdopen");
	}

	/*
	 * we start with the assumtion of a local address.
	 */
	remote_address=0;

	/*
	 * read header stuff.
	 */
	for(;;)
	{
		/*
		 * if this line is empty we will rewind and over-write it.
		 */
		lastPos=ftell(exp_fp);

		if(fgets(line_buff,1024,old_fp)==NULL)
		{
			warn("Couldn't read Out Bound Message file\n");
			XBell (dpy, 10);
			XFlush(dpy);
			free(new_file);
			fclose(exp_fp);
			fclose(old_fp);
			return;
		}


		if(strcmp(line_buff,"\n")==0)
		{
			/*
			 * this is the first empty line.  That means there's
			 * no more header stuff.
			 */
			break;
		}
	
		if(line_buff[0]==' ')
		{
			fseek(exp_fp,-1,SEEK_CUR);
			fputs(line_buff,exp_fp);
			continue;
		}

		/*
		 * if the line contains a colon, then it is a header line
		 */
		if((tmp_ptr=strchr(line_buff,':'))!=(char *)0)
		{
			if(strncasecmp(line_buff,"to:",3) == 0 ||
				strncasecmp(line_buff,"cc:",3) == 0 ||
				strncasecmp(line_buff,"bcc:",4) == 0)
			{
				*tmp_ptr=(char)0;
				fputs(line_buff, exp_fp);
				fputs(":",exp_fp);
				tmp_addr=(char *)malloc(strlen(&tmp_ptr[1])+2);
				strcpy(tmp_addr,&tmp_ptr[1]);
				for(;;)
				{
					oldPos=ftell(old_fp);
					if(fgets(line_buff,1024,old_fp)==NULL)
					{
					}
					if((strcmp(line_buff,"\n")==0) ||
					(strchr(line_buff,':')!=(char *)0))
					{
						fseek(old_fp,oldPos,SEEK_SET);
						break;
					}

					tmp_addr=(char *)realloc(tmp_addr,
						strlen(tmp_addr)+
						strlen(line_buff)+6);
					strcat(tmp_addr,",");
					strcat(tmp_addr,line_buff);
				}

				tmp_ptr=expand_alias(tmp_addr,0,"");
				if(strlen(StepOverWhiteSpace(tmp_ptr))==0)
				{
					fseek(exp_fp,lastPos,SEEK_SET);
				}
				else
				{
				int line_len=0;
				char *tmp_tok;

					for(tmp_tok=strtok(tmp_ptr,",");
						tmp_tok!=(char *)0;
						tmp_tok=strtok(NULL,","))
					{
						tmp_tok=StepOverWhiteSpace(tmp_tok);
						if(strlen(tmp_tok)!=0)
						{
							if((line_len!=0)&&
								((line_len + strlen(tmp_tok))>80))
							{
								fputs("\n",exp_fp);
								line_len=0;
							}

							fputs(" ",exp_fp);
							fputs(tmp_tok,exp_fp);
							fputs(",",exp_fp);
							line_len+=strlen(tmp_tok)+2;
						}
					}
					fseek(exp_fp,-1,SEEK_CUR);
					fputs("\n",exp_fp); 
					free(tmp_addr);
					free(tmp_ptr);
				}
			}
			else
			{
				if((tmp_ptr==(char *)0) || strlen(StepOverWhiteSpace(tmp_ptr+1))>1)
				{
					fputs(StepOverWhiteSpace(line_buff),exp_fp);
				}
			}
			
		}
		else
			fputs(line_buff,exp_fp);

	}

	/*
	 * we are done expanding the header data.  The only thing to do
	 * now is to copy the rest of the message.
	 */
	fputs("\n",exp_fp);

	for(;;)
	{
		if(fgets(line_buff,1024,old_fp)==NULL)
			break;

		if(fputs(line_buff,exp_fp)==EOF)
		{
			perror(f);
			warn("Couldn't write expansion file\n");
			XBell (dpy, 10);
			XFlush(dpy);
			free(new_file);
			fclose(old_fp);
			fclose(exp_fp);
			return;
		}

	}


	/*
	 * close every thing.  Move the new file in to the
	 * old file, and free any data.
	 */

	fclose(old_fp);
	fclose(exp_fp);
	unlink(f);
	link(new_file,f);
	unlink(new_file);
	free(new_file);
}







