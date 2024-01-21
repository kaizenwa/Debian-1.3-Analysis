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

static char vars_rcsid[]="$Id: setvars.c,v 1.11 1995/05/12 19:16:46 bobo Exp $";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include "defs.h"
#include "vardefs.h"
#include "mailhandler.h"

extern Display *dpy;
extern char *username,*system_mbox_fname;

char *allnet=(char *)0;
int alwaysignore;
int append;
int askcc;
int askbcc;
int asksub;
int autoprint;
int bang;
int bell;
int bodyOnly;
char *cmd=(char *)0;
int cmd_box_box;
int confirm_EDITOR;
char *conversion=(char *)0;
int crt;
char *DEAD=(char *)0;
char *DISPLAY=(char *)0;
int debug;
int detectiv;
int do_signature;
int do_xmtaliases;
int do_xmtaccelerators;
int do_xmtformat;
char *xmtformat=(char *)0;
int do_xmtrecord;
int dot;
char *EDITOR=(char *)0;
int edit_needs_xterm;
int editable_msg;
char *escape=(char *)0;
int flash;
char *folder=(char *)0;
int fromAddress;
int header;
int hold;
char *HOME=(char *)0;
#ifdef HYPERTEXT
char *hypertext=(char *)0;
#endif
int ignore;
int ignoreeof;
int include_msg;
int interval;
int keep;
int keepsave;
char *LISTER=(char *)0;
int MacX;
char *MAIL=(char *)0;
char *MAILRC=(char *)0;
char *MBOX=(char *)0;
int metoo;
char *mprefix=(char *)0;
int num_saved_fnames;
int onehop;
char *outbound_suffix=(char *)0;
int outfolder;
int page;
char *PAGER=(char *)0;
char *PRINTER=(char *)0;
char *print_cmd=(char *)0;
char *prompt=(char *)0;
int quiet;
char *record=(char *)0;
int replyall;
char *replyto=(char *)0;
char *realname=(char *)0;
int save;
int saveCurrent;
char *savedFnames=(char *)0;
int screen;
char *sendmail=(char *)0;
int sendwait;
char *SHELL=(char *)0;
int showto;
int showlast;
int showold;
int shownew;
int show_deleted;
int show_header;
char *sign=(char *)0;
char *Sign=(char *)0;
int skipDeleted;
char *sortFields;
int state_box;
int toplines;
char *TMPDIR=(char *)0;
int use_folders_cmd;
int verbose;
char *VISUAL=(char *)0;


int xmtf_from,xmtf_day,xmtf_month,xmtf_date,xmtf_time,xmtf_size,xmtf_subject;

struct var_ent var_table[] = {
	{"allnet",VARCSTAR,(long *)&allnet},
	{"alwaysignore",VARINT,(long *)&alwaysignore},
	{"append",VARINT,(long *)&append},
	{"askcc",VARINT,(long *)&askcc},
	{"askbcc",VARINT,(long *)&askbcc},
	{"asksub",VARINT,(long *)&asksub},
	{"autoprint",VARINT,(long *)&autoprint},
	{"bang",VARINT,(long *)&bang},
	{"bell",VARINT,(long *)&bell},
	{"bodyOnly",VARINT,(long *)&bodyOnly},
	{"cmd",VARCSTAR,(long *)&cmd},
	{"cmd_box",VARINT,(long *)&cmd_box_box},
	{"confirm_EDITOR",VARINT,(long *)&confirm_EDITOR},
	{"conversion",VARCSTAR,(long *)&conversion},
	{"crt",VARINT,(long *)&crt},
	{"DEAD",VARCSTAR,(long *)&DEAD},
	{"DISPLAY",VARCSTAR,(long *)&DISPLAY},
	{"detectiv",VARINT,(long *)&detectiv},
	{"debug",VARINT,(long *)&debug},
	{"do_signature",VARINT,(long *)&do_signature},
	{"do_xmtaliases",VARINT,(long *)&do_xmtaliases},
	{"do_xmtaccelerators",VARINT,(long *)&do_xmtaccelerators},
	{"do_xmtformat",VARINT,(long *)&do_xmtformat},
	{"xmtformat",VARCSTAR,(long *)&xmtformat},
	{"do_xmtrecord",VARINT,(long *)&do_xmtrecord},
	{"dot",VARINT,(long *)&dot},
	{"editable_msg",VARINT,(long *)&editable_msg},
	{"EDITOR",VARCSTAR,(long *)&EDITOR},
	{"edit_needs_xterm",VARINT,(long *)&edit_needs_xterm},
	{"escape",VARCSTAR,(long *)&escape},
	{"flash",VARINT,(long *)&flash},
	{"folder",VARCSTAR,(long *)&folder},
	{"fromAddress",VARINT,(long *)&fromAddress},
	{"header",VARINT,(long *)&header},
	{"hold",VARINT,(long *)&hold},
	{"HOME",VARCSTAR,(long *)&HOME},
#ifdef HYPERTEXT
	{"hypertext",VARCSTAR,(long *)&hypertext},
#endif
	{"ignore",VARINT,(long *)&ignore},
	{"ignoreeof",VARINT,(long *)&ignoreeof},
	{"include_msg",VARINT,(long *)&include_msg},
	{"interval",VARINT,(long *)&interval},
	{"indentprefix",VARCSTAR,(long *)&mprefix},
	{"keep",VARINT,(long *)&keep},
	{"keepsave",VARINT,(long *)&keepsave},
	{"LISTER",VARCSTAR,(long *)&LISTER},
	{"MacX",VARINT,(long *)&MacX},
	{"MAIL",VARCSTAR,(long *)&MAIL},
	{"MAILRC",VARCSTAR,(long *)&MAILRC},
	{"MBOX",VARCSTAR,(long *)&MBOX},
	{"metoo",VARINT,(long *)&metoo},
	{"mprefix",VARCSTAR,(long *)&mprefix},
	{"num_saved_fnames",VARINT,(long *)&num_saved_fnames},
	{"onehop",VARINT,(long *)&onehop},
	{"outbound_suffix",VARCSTAR,(long *)&outbound_suffix},
	{"outfolder",VARINT,(long *)&outfolder},
	{"page",VARINT,(long *)&page},
	{"PAGER",VARCSTAR,(long *)&PAGER},
	{"PRINTER",VARCSTAR,(long *)&PRINTER},
	{"print_cmd",VARCSTAR,(long *)&print_cmd},
	{"prompt",VARCSTAR,(long *)&prompt},
	{"quiet",VARINT,(long *)&quiet},
	{"record",VARCSTAR,(long *)&record},
	{"replyall",VARINT,(long *)&replyall},
	{"replyto",VARCSTAR,(long *)&replyto},
	{"realname",VARCSTAR,(long *)&realname},
	{"save",VARINT,(long *)&save},
	{"saveCurrent",VARINT,(long *)&saveCurrent},
	{"saved_fnames",VARCSTAR,(long *)&savedFnames},
	{"screen",VARINT,(long *)&screen},
	{"sendmail",VARCSTAR,(long *)&sendmail},
	{"sendwait",VARINT,(long *)&sendwait},
	{"SHELL",VARCSTAR,(long *)&SHELL},
	{"showto",VARINT,(long *)&showto},
	{"showlast",VARINT,(long *)&showlast},
	{"showold",VARINT,(long *)&showold},
	{"shownew",VARINT,(long *)&shownew},
	{"showdeleted",VARINT,(long *)&show_deleted},
	{"showheader",VARINT,(long *)&show_header},
	{"sign",VARCSTAR,(long *)&sign},
	{"Sign",VARCSTAR,(long *)&Sign},
	{"skipDeleted",VARINT,(long *)&skipDeleted},
	{"sortFields",VARCSTAR,(long *)&sortFields},
	{"state_box",VARINT,(long *)&state_box},
	{"toplines",VARINT,(long *)&toplines},
	{"TMPDIR",VARCSTAR,(long *)&TMPDIR},
	{"use_folders_cmd",VARINT,(long *)&use_folders_cmd},
	{"verbose",VARINT,(long *)&verbose},
	{"VISUAL",VARCSTAR,(long *)&VISUAL}};

int VART_LEN=(sizeof(var_table)/sizeof(*var_table));
int *sortTypes=(int *)0;

void
set_def_vars()
{
char *tmp_env_val;

if(HOME!=0)
{
	free(HOME);
}
	if((tmp_env_val=getenv("HOME"))!=(char *)0)
	{
		HOME=malloc(strlen(tmp_env_val)+1);
		strcpy(HOME,tmp_env_val);
	}
	else
	{
		fprintf(stderr,"Can't find HOME variable\n");
		exit(1);
	}

if(allnet !=0)
{
	free(allnet);
	allnet =(char *)0;
}
alwaysignore = 1;
append =0;
askcc =0;
askbcc=0;
asksub =1;
autoprint=0;
bang=0;
bell=1;
bodyOnly=0;

if(cmd!=0)
{
	free(cmd);
	cmd=(char *)0;
}

cmd_box_box=0;
confirm_EDITOR=1;

if(conversion!=0)
{
	free(conversion);
	conversion=(char *)0;
}

crt=0;

if(DEAD!=0)
{
	free(DEAD);
}

	if((tmp_env_val=getenv("DEAD"))!=(char *)0)
	{
		DEAD=malloc(strlen(tmp_env_val)+1);
		strcpy(DEAD,tmp_env_val);
	}
	else
	{
		if((DEAD=(char *)malloc(strlen(HOME)+15))==0)
		{
			fprintf(stderr,"failed to do malloc\n");
			say_good_bye();
		}
		sprintf(DEAD,"%s/dead.letter",HOME);
		setenv("DEAD",DEAD,1);
	}


/*
 * the display value is a little strange in that we can get it
 * from three places: the command line, the environment, or a
 * variable in the mailrc file.  For this reason, we don't want
 * to assign a value to DISPLAY if there isn't one already assigned
 * and then we either the command line or environment values in
 * that order. 
 */
if(DISPLAY!=0)
{
	if((DISPLAY=(char *)malloc(strlen(DisplayString(dpy))+1))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	strcpy(DISPLAY,DisplayString(dpy));
	setenv("DISPLAY",DISPLAY,1);
}

detectiv=0;
debug=0;
do_signature=1;
do_xmtaliases=1;
do_xmtaccelerators=1;
do_xmtformat=0;

if(xmtformat!=0)
{
	free(xmtformat);
	xmtformat=(char *)0;
}

#ifdef USE_SENDMAIL
do_xmtrecord=1;
#else
do_xmtrecord=0;
#endif

editable_msg=0;
dot=0;

if(EDITOR!=0)
{
	free(EDITOR);
}
	if((tmp_env_val=getenv("EDITOR"))!=(char *)0)
	{
		EDITOR=malloc(strlen(tmp_env_val)+1);
		strcpy(EDITOR,tmp_env_val);
	}
	else
	{
		if((EDITOR=(char *)malloc(100))==0)
		{
			fprintf(stderr,"failed to do malloc\n");
			say_good_bye();
		}
		sprintf(EDITOR,"none");
		setenv("EDITOR",EDITOR,1);
	}

edit_needs_xterm=1;


if(escape!=0)
{
	free(escape);
}
	if((escape=(char *)malloc(100))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	sprintf(escape,"~");


if(folder!=0)
{
	free(folder);
}
	if((tmp_env_val=getenv("folder"))!=(char *)0)
	{
		folder=malloc(strlen(tmp_env_val)+1);
		strcpy(folder,tmp_env_val);
	}
	else
	{
		folder=(char *)0;
	}

#ifdef HYPERTEXT

if(hypertext!=0)
{
	free(hypertext);
}
	if((tmp_env_val=getenv("hypertext"))!=(char *)0)
	{
		hypertext=malloc(strlen(tmp_env_val)+1);
		strcpy(hypertext,tmp_env_val);
	}
	else
	{
		hypertext=(char *)malloc(strlen(HYPERTEXT)+2);
		strcpy(hypertext,HYPERTEXT);
		setenv("hypertext",HYPERTEXT,1);
	}
#endif

fromAddress=0;
header=1;
hold=0;
ignore=0;
ignoreeof=0;
include_msg=0;
interval=15;
keep=0;
keepsave=0;

if(LISTER!=0)
{
	free(LISTER);
}
	if((LISTER=(char *)malloc(100))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	sprintf(LISTER,"ls");

if((tmp_env_val=getenv("MacX"))!=(char *)0)
{
	MacX=atol(tmp_env_val);
}
else
	MacX=0;

/*
 * the MAIL variable's default setting should only be assigned
 * if there isn't already one.  That is to say that we should
 * only call this part of the code when XMailTool starts up for
 * the first time.  After that, we should leave MAIL alone.
 */
if(MAIL!=0)
{
	free(MAIL);
	MAIL=malloc(strlen(system_mbox_fname)+1);
	strcpy(MAIL,system_mbox_fname);
	setenv("MAIL",MAIL,1);
}
else
{
	if((tmp_env_val=getenv("MAIL"))!=(char *)0)
	{
		MAIL=malloc(strlen(tmp_env_val)+1);
		strcpy(MAIL,tmp_env_val);
	}
	else
	{
		if((MAIL=(char *)malloc(strlen(MAILBOX_DIRECTORY)+strlen(username)+5))==0)
		{
			fprintf(stderr,"failed to do malloc\n");
			say_good_bye();
		}
		sprintf(MAIL,"%s/%s",MAILBOX_DIRECTORY,username);
		setenv("MAIL",MAIL,1);
	}
}

if(MAILRC!=0)
{
	free(MAILRC);
}
	if((tmp_env_val=getenv("MAILRC"))!=(char *)0)
	{
		MAILRC=malloc(strlen(tmp_env_val)+1);
		strcpy(MAILRC,tmp_env_val);
	}
	else
	{
		if((MAILRC=(char *)malloc(strlen(HOME)+10))==0)
		{
			fprintf(stderr,"failed to do malloc\n");
			say_good_bye();
		}
		sprintf(MAILRC,"%s/.mailrc",HOME);
		setenv("MAILRC",MAILRC,1);
	}

if(MBOX!=0)
{
	free(MBOX);
}

	if((tmp_env_val=getenv("MBOX"))!=(char *)0)
	{
		MBOX=malloc(strlen(tmp_env_val)+1);
		strcpy(MBOX,tmp_env_val);
	}
	else
	{
		if((MBOX=(char *)malloc(100))==0)
		{
			fprintf(stderr,"failed to do malloc\n");
			say_good_bye();
		}
		sprintf(MBOX,"%s/mbox",HOME);
		setenv("MBOX",MBOX,1);
	}

if(mprefix!=0)
{
	free(mprefix);
}
	mprefix=(char *)malloc(3);
	sprintf(mprefix,"> ");

metoo=0;
num_saved_fnames=5;
onehop=0;
outfolder=0;
page=0;

if(outbound_suffix!=0)
{
	free(outbound_suffix);
}

if((outbound_suffix=(char *)malloc(100))==0)
{
	fprintf(stderr,"failed to do malloc\n");
	say_good_bye();
}
sprintf(outbound_suffix,".mai");


if(PAGER!=0)
{
	free(PAGER);
}
	if((PAGER=(char *)malloc(100))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	sprintf(PAGER,"more");

if(PRINTER!=0)
{
	free(PRINTER);
}

	if((tmp_env_val=getenv("PRINTER"))!=(char *)0)
	{
		PRINTER=malloc(strlen(tmp_env_val)+1);
		strcpy(PRINTER,tmp_env_val);
	}
	else
	{
		if((PRINTER=(char *)malloc(100))==0)
		{
			fprintf(stderr,"failed to do malloc\n");
			say_good_bye();
		}
		strcpy(PRINTER,"");
	}


if(print_cmd!=0)
{
	free(print_cmd);
}
	if((print_cmd=(char *)malloc(100))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
#ifdef PRINTER_CMD
	strcpy(print_cmd,PRINTER_CMD);
#else	
	strcpy(print_cmd,"lpr -p -JMail");
#endif


if(prompt!=0)
{
	free(prompt);
}
	if((prompt=(char *)malloc(100))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	sprintf(prompt,"&");


quiet=0;

if(record!=0)
{
	free(record);
	record=(char *)0;
}

if(replyto!=0)
{
	free(replyto);
	replyto=(char *)0;
}

if(savedFnames!=0)
{
	free(savedFnames);
	savedFnames=(char *)0;
}

replyall=0;
save=1;
saveCurrent=0;
screen=0;

if(sortTypes!=(int *)0)
{
	free(sortTypes);
}
	
sortTypes=(int *)malloc(sizeof(int)*2);
sortTypes[0]=-SortPosition;
sortTypes[1]=0;


if(sendmail!=0)
{
	free(sendmail);
}

	if((sendmail=(char *)malloc(100))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	sprintf(sendmail,MSENDER);



sendwait=0;

if(SHELL!=0)
{
	free(SHELL);
}
	if((SHELL=(char *)malloc(100))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	if(getenv("SHELL")!=NULL)
		strcpy(SHELL,getenv("SHELL"));
	else
	{
		sprintf(SHELL,"/bin/sh");
		setenv("SHELL",SHELL,1);
	}



showto=0;
skipDeleted=1;
showlast=0;
showold=1;
shownew=0;
show_deleted=1;
show_header=0;

if(sign!=0)
{
	free(sign);
}

	if((sign=(char *)malloc(2))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	strcpy(sign,"");

xmtf_from=0;
xmtf_day=0;
xmtf_month=0;
xmtf_date=0;
xmtf_time=0;
xmtf_size=0;
xmtf_subject=0;

	

if(Sign!=0)
{
	free(Sign);
}
	if((Sign=(char *)malloc(2))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	strcpy(Sign,"");

	if(sortFields!=0)
		free(sortFields);

	if((sortFields=(char *)malloc(100))==0)
	{
		fprintf(stderr,"failed to do malloc\n");
		say_good_bye();
	}
	sprintf(sortFields,"Position");

state_box=1;
toplines=5;
use_folders_cmd=0;
verbose=0;

if(TMPDIR!=0)
{
	free(TMPDIR);
}

	if((tmp_env_val=getenv("TMPDIR"))!=(char *)0)
	{
		TMPDIR=malloc(strlen(tmp_env_val)+1);
		strcpy(TMPDIR,tmp_env_val);
	}
	else
	{
		if((TMPDIR=(char *)malloc(100))==0)
		{
			fprintf(stderr,"failed to do malloc\n");
			say_good_bye();
		}
		sprintf(TMPDIR,"/tmp");
		setenv("TMPDIR",TMPDIR,1);
	}

if(VISUAL!=0)
{
	free(VISUAL);
}

	if((tmp_env_val=getenv("VISUAL"))!=(char *)0)
	{
		VISUAL=malloc(strlen(tmp_env_val)+1);
		strcpy(VISUAL,tmp_env_val);
	}
	else
	{
		if((VISUAL=(char *)malloc(100))==0)
		{
			fprintf(stderr,"failed to do malloc\n");
			say_good_bye();
		}
		sprintf(VISUAL,"vi");
		setenv("VISUAL",VISUAL,1);
	}

}

void
setvars(ln)
char *ln;
{
char buff[2024],tmp_buff[2024];
char *ptr;
int var_index,x;


/*
 * Variables as I'm defining them must be of the form
 *
 * set var[=["|']value["|']]
 * 
 * at this point however, this function is expecting the
 * set and trailing space to be stripped.
 */

	if((ptr=strchr(ln,'='))==(char *)0)
	{

		ptr=strtok(ln," \t");
		/*
		 * this variable is a toggle
		 */
		for(var_index=0;var_index<VART_LEN;var_index++)
		{
			if(strcmp(ptr,var_table[var_index].name)==0)
			{
				if(var_table[var_index].type == VARINT)
				{
					*(var_table[var_index].place)=1;
					setenv(ptr,"1",1);
				} else if(var_table[var_index].type == VARCSTAR)
				{
					*(var_table[var_index].place)=(long)malloc(2);
					strcpy((char *)*(var_table[var_index].place),"");
					setenv(ptr,"",1);
				}

				break;
			}
				
		}

		if(var_index == VART_LEN)
		{
			if(debug)
				fprintf(stderr,"unused variable \"%s\"\n",ptr);
			return;
		}
	}
	else
	{

		/*
		 * check for quotation marks.
		 *
		 */
		if((ptr[1]=='\'') || (ptr[1]=='\"'))
		{
			if( ptr[1] != ptr[strlen(ptr)-1]   )
			{
				fprintf(stderr,"mismatched quote [%c]\n",ptr[strlen(ptr)-1]);
				return;
			}

#ifdef SYSV
			memcpy(tmp_buff, &ptr[2], strlen(ptr) );
#else
			bcopy(&ptr[2],tmp_buff,strlen(ptr) );
#endif
			tmp_buff[strlen(ptr)-3]=(char)0;
		}
		else
		{
#ifdef SYSV
			memcpy(tmp_buff, &ptr[1], strlen(ptr) );
#else
			bcopy(&ptr[1],tmp_buff,strlen(ptr) );
#endif
		}

		*ptr=(char)0;

		for(var_index=0;var_index<VART_LEN;var_index++)
		{
			if(strcmp(ln,var_table[var_index].name)==0)
			{
				setenv(ln,tmp_buff,1);
				if(var_table[var_index].type == VARINT)
				{
					if(strspn(tmp_buff,"-0123456789")==0)
					{
						/* wasn't numeric */
						if((strcasecmp(tmp_buff,"yes")==0)||
								(strcasecmp(tmp_buff,"on")==0) ||
								(strcasecmp(tmp_buff,"true")==0))
							*(var_table[var_index].place) = 1;
						else
							*(var_table[var_index].place) = 0;
					}
					else
						*(var_table[var_index].place) =
							atol(tmp_buff);
				}	
				else
				{
					if(*(var_table[var_index].place) != 0)
						free((void *)*(var_table[var_index].place));


					*(var_table[var_index].place) =
						 (long)malloc(strlen(tmp_buff)+2);

					strcpy((char *)*(var_table[var_index].place),
						tmp_buff);
if(debug)
	fprintf(stderr,"%s = [%s]\n",var_table[var_index].name,*(var_table[var_index].place));
				}

				break;
			}
		}
		if(var_index == VART_LEN)
		{
			if(debug)
				fprintf(stderr,"unused variable \"%s\"\n",ln);
			return;
		}

	}

	if(strcmp(var_table[var_index].name,"MAIL")==0)
	{
		if(MAIL!=(char *)0 && (MAIL[0]=='+'))
		{
			if(folder!=NULL)
			{
				ptr=MAIL;
				MAIL=(char *)malloc(strlen(folder)+strlen(MAIL) +1);
				sprintf(MAIL,"%s/%s",folder,&ptr[1]);
				free(ptr);
			}
		}
	}

	if(strcmp(var_table[var_index].name,"folder")==0)
	{
		if((folder!=NULL) && (folder[0] != '/'))
		{
			ptr=folder;
			folder=(char *)malloc(strlen(ptr) + strlen(HOME)+2);
			sprintf(folder,"%s/%s",HOME,ptr);
			free(ptr);
		}
	}
	if(strcmp(var_table[var_index].name,"record")==0)
	{
		if((record!=NULL) && (record[0] == '+'))
		{
			if(folder!=NULL)
			{
				ptr = record;
				record=(char *)malloc(strlen(folder) + strlen(ptr) +2);
				sprintf(record,"%s/%s",folder,&ptr[1]);
				free(ptr);
			}
		}

	}

	if(strcmp(var_table[var_index].name,"sortFields")==0)
	{
		char *my_tok;
		int sortType_index=0;

		if(sortTypes != (int *)0)
			free(sortTypes);

		sortTypes=(int *)malloc(sizeof(int)*2);

			for(my_tok=strtok(sortFields," \t,");my_tok!=NULL;
						my_tok=strtok(NULL," \t,"))
			{
				sortTypes=(int *)realloc(sortTypes,
					sizeof(int)*(sortType_index+2));
				sortTypes[sortType_index+1]=0;
				if(strcmp(my_tok,"From")==0)
					sortTypes[sortType_index]=SortFrom;
				else if(strcmp(my_tok,"Cc")==0)
					sortTypes[sortType_index]=SortCc;
				else if(strcmp(my_tok,"Subject")==0)
					sortTypes[sortType_index]=SortSubject;
				else if(strcmp(my_tok,"ReturnPath")==0)
					sortTypes[sortType_index]=SortReturnPath;
				else if(strcmp(my_tok,"ReplyTo")==0)
					sortTypes[sortType_index]=SortReplyTo;
				else if(strcmp(my_tok,"References")==0)
					sortTypes[sortType_index]=SortReferences;
				else if(strcmp(my_tok,"Keywords")==0)
					sortTypes[sortType_index]=SortKeywords;
				else if(strcmp(my_tok,"Comments")==0)
					sortTypes[sortType_index]=SortComments;
				else if(strcmp(my_tok,"Date")==0)
					sortTypes[sortType_index]=SortDate;
				else if(strcmp(my_tok,"MessageId")==0)
					sortTypes[sortType_index]=SortMessageId;
				else if(strcmp(my_tok,"Sender")==0)
					sortTypes[sortType_index]=SortSender;
				else if(strcmp(my_tok,"Position")==0)
					sortTypes[sortType_index]=SortPosition;
				else if(strcmp(my_tok,"Size")==0)
					sortTypes[sortType_index]=SortSize;
				else if(strcmp(my_tok,"Status")==0)
					sortTypes[sortType_index]=SortStatus;
				else if(strcmp(my_tok,"Face")==0)
					sortTypes[sortType_index]=SortFace;
				else if(strcmp(my_tok,"-From")==0)
					sortTypes[sortType_index]=-SortFrom;
				else if(strcmp(my_tok,"-Cc")==0)
					sortTypes[sortType_index]=-SortCc;
				else if(strcmp(my_tok,"-Subject")==0)
					sortTypes[sortType_index]=-SortSubject;
				else if(strcmp(my_tok,"-ReturnPath")==0)
					sortTypes[sortType_index]=-SortReturnPath;
				else if(strcmp(my_tok,"-ReplyTo")==0)
					sortTypes[sortType_index]=-SortReplyTo;
				else if(strcmp(my_tok,"-References")==0)
					sortTypes[sortType_index]=-SortReferences;
				else if(strcmp(my_tok,"-Keywords")==0)
					sortTypes[sortType_index]=-SortKeywords;
				else if(strcmp(my_tok,"-Comments")==0)
					sortTypes[sortType_index]=-SortComments;
				else if(strcmp(my_tok,"-Date")==0)
					sortTypes[sortType_index]=-SortDate;
				else if(strcmp(my_tok,"-MessageId")==0)
					sortTypes[sortType_index]=-SortMessageId;
				else if(strcmp(my_tok,"-Sender")==0)
					sortTypes[sortType_index]=-SortSender;
				else if(strcmp(my_tok,"-Position")==0)
					sortTypes[sortType_index]=-SortPosition;
				else if(strcmp(my_tok,"-Size")==0)
					sortTypes[sortType_index]=-SortSize;
				else if(strcmp(my_tok,"-Status")==0)
					sortTypes[sortType_index]=-SortStatus;
				else if(strcmp(my_tok,"-Face")==0)
					sortTypes[sortType_index]=-SortFace;
				else
					sortType_index--;

				sortType_index++;
			}
	}
}

	

