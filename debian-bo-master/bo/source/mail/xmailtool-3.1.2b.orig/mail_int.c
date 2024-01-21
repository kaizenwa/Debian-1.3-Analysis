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

static char mail_int_rcsid[]="$Id: mail_int.c,v 1.52 1995/05/12 19:18:42 bobo Exp $";

/*
 * mail interface routines.
 */
#include <stdio.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/time.h>
#include <fcntl.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Text.h>

#include <utime.h>

#include "defs.h"
#include "patchlevel.h"
#include "mailhandler.h"

extern Widget toplevel,lst_list,toggle;
extern Widget replywidg;

extern Widget deliver_item,cancel_item,cd_item,reply_item,done_item,command_button();
extern Widget next_item,del_item,undel_item,print_item,text_label(),icon_bitmap;
extern Widget mailrc_item,about_box,aboutsw,helpsw,about_item,help_item,quit_item;
extern Widget save_item,folder_item,nmail_item,comp_item,mail_msg,file_item;
extern Widget gripe_item,preserve_item,folder_shell,icon_shell,icon_box,icon_label;
extern Widget header_box,cmd_box,rtsf,msg_box,box,replysw, file_cmd_box,dir_cmd_box,
		list_folder,folder_name_item,file_name_item,help_msg_item,
		dir_name_item,load_item;

extern Widget  state_cmd_box,file_name_label;

extern Cursor watch_cur,norm_cur,del_cur,save_cur,pres_cur,show_cur;

extern struct HEADLIST *Letters_Top;

extern int showto,bodyOnly,skipDeleted;
extern int debug,do_xmtformat,showlast,caught_fatal_signal,safe_to_exit;
extern Display *dpy;
extern struct HEADLIST *Letters_Top,*current_msg;
extern struct HEADLIST **sortedList;
extern struct stat saved_stat;
extern int LetterCount,show_deleted;

extern int header_index,num_headers,header_right,
		header_left,keepsave,alwaysignore;
extern int xmtf_from,xmtf_day,xmtf_month,xmtf_date,xmtf_time, 				xmtf_size,xmtf_subject,cur_msg_num,OldMessageCount,
		editable_msg;
extern char *xmtformat,*MBOX,*cur_mbox_fname,*system_mbox_fname;


int MAIL_pid;

int in_fd[2];
int out_fd[2];
int mail_in,mail_out;

char dbox_fname[50];
char editor_fname[50];
char msg_fname[50];
char reply_fname[50];
char *previous_file_name=(char *)0;
char *expand_fname();
char tmp_buff[1024];
char cmd_buff[1024];

char EOT_FLAG[150];


int
start_mail()
{

int fd;
	
	/*
	 * Make the names for the files used
	 *  --- In previous edditions, string Widgets
	 *  were used so much of this wasn't necesary.
	 */
	strcpy(dbox_fname,"/tmp/XMTdXXXXXX");
	mktemp(dbox_fname);
	strcpy(msg_fname,"/tmp/XMTmXXXXXX");
	mktemp(msg_fname);

	strcpy(reply_fname,"/tmp/XMTrXXXXXX");
	mktemp(reply_fname);

/*	strcpy(editor_fname,"/tmp/XMTeXXXXXX");
	mktemp(editor_fname);
*/

	/*
	 * Fill in the files that need information.
	 */
	if((fd=open(dbox_fname,O_CREAT|O_WRONLY,0600))==-1)
	{
		perror("open(dumby mbox)");
		exit(1);
	}

	if(write(fd,"\n",1)==-1)
	{
		perror("write(dumby mbox)");
		clean_up();
		exit(1);
	}

	close(fd);

	if((fd=open(msg_fname,O_CREAT|O_WRONLY,0600))==-1)
	{
		perror("open(message file)");
		clean_up();
		exit(1);
	}

	if(write(fd,"You Should never see this message.\n",35)==-1)
	{
		perror("write(message file)");
		clean_up();
		exit(1);
	}

	close(fd);

	return(0);
}



static char *header_data=(char *)0;

/*
 * this function creates a buffer containing the pertanent text
 * out the headers.  It also calls load__headers which requires num_headers
 * to be set to the number of headers in mailbox.
 */
int
get_headers(load,ptr)
int load;
char **ptr;
{
register int header_data_index;
int num_char,res,header_index,headers_len;
register struct HEADLIST *m;
struct HEADLIST **header_base;

	if(LetterCount==0)
	{
		/*
		 * there are no headers to be gotten.
		 */
		Letters_Top=0;
		*ptr="";
		return(0);
	}
		
	if(header_data==(char *)0)
		header_data=(char *)malloc(450);

	header_data[0]=(char)0;


	header_data_index=0;

	for(header_index=0;header_index<LetterCount;header_index++)
	{

		if(sortedList[header_index]==(struct HEADLIST *)0)
		{
			fprintf(stderr,"null sorted %d 0x%0x\n",header_index,&sortedList[header_index]);
			continue;
		}

		if(show_deleted || (!ISMMDEL(sortedList[header_index])))
		{
			header_data=(char *)realloc(header_data,
						header_data_index+
				strlen(sortedList[header_index]->Subject) + 
									450);
			if(sortedList[header_index]==current_msg)
			{
				cur_msg_num=header_index;
				current_msg=(struct HEADLIST *)0;
			}

			if(showto && sortedList[header_index]->From_me)
			{
				sprintf(&header_data[header_data_index],
			"%c%c%3d To %-20.20s %3.3s %3.3s %2.2s %5.5s %3d/%dK %s\n",
			((current_msg==(struct HEADLIST *)0) &&
				(header_index==cur_msg_num)?'>':' '),
			(ISMMSAVED(sortedList[header_index]) && 
				(!ISMMDEL(sortedList[header_index]))?'S':
			(ISMMSAVED(sortedList[header_index]) && 
				(ISMMDEL(sortedList[header_index]))?'*':
			(ISMMDEL(sortedList[header_index])?'D':
			(ISMMREAD(sortedList[header_index])?' ':
			(ISMMOLD(sortedList[header_index])?'U':'N'))))),
					header_index+1,
				(sortedList[header_index]->To?
						sortedList[header_index]->To:" "),
				(sortedList[header_index]->From_DAY?
						sortedList[header_index]->From_DAY:" "),
				(sortedList[header_index]->From_MONTH?
						sortedList[header_index]->From_MONTH:" "),
				(sortedList[header_index]->From_DATE?
						sortedList[header_index]->From_DATE:" "),
				(sortedList[header_index]->From_TIME?
						sortedList[header_index]->From_TIME:" "),
					sortedList[header_index]->bodyCount,
					sortedList[header_index]->byteSize/1000,
					sortedList[header_index]->Subject);
			}
			else
			{
				sprintf(&header_data[header_data_index],
				"%c%c%3d %-23.23s %3.3s %3.3s %2.2s %5.5s %3d/%dK %s\n",
			((current_msg==(struct HEADLIST *)0) &&
				(header_index==cur_msg_num)?'>':' '),
			(ISMMSAVED(sortedList[header_index]) && 
				(!ISMMDEL(sortedList[header_index]))?'S':
			(ISMMSAVED(sortedList[header_index]) && 
				(ISMMDEL(sortedList[header_index]))?'*':
			(ISMMDEL(sortedList[header_index])?'D':
			(ISMMREAD(sortedList[header_index])?' ':
			(ISMMOLD(sortedList[header_index])?'U':'N'))))),
					header_index+1,
				(sortedList[header_index]->From_UID?
						sortedList[header_index]->From_UID:" "),
				(sortedList[header_index]->From_DAY?
						sortedList[header_index]->From_DAY:" "),
				(sortedList[header_index]->From_MONTH?
						sortedList[header_index]->From_MONTH:" "),
				(sortedList[header_index]->From_DATE?
						sortedList[header_index]->From_DATE:" "),
				(sortedList[header_index]->From_TIME?
						sortedList[header_index]->From_TIME:" "),
					sortedList[header_index]->bodyCount,
					sortedList[header_index]->byteSize/1000,
					sortedList[header_index]->Subject);
			}

			num_char=strlen(&header_data[header_data_index]);
			sortedList[header_index]->hdr_left=header_data_index;
			sortedList[header_index]->hdr_len=num_char-1;
			header_data_index += num_char;	
		}
		else
		{
			sortedList[header_index]->hdr_left=0;
			sortedList[header_index]->hdr_len=0;
		}
	}

	header_data[header_data_index-1]=(char)0;
	*ptr=header_data;
	return(LetterCount);
}

char *
format_header(s)
char *s;
{
	char from[200],*day="",*month="",
		*date="",*time="",*size="",
		*subject="";
	char *tmp=(char *)0;
	char tmp2[100];

exit(10);

	/*
	 * Look for the user string.  This may also be the To:
	 * string for showto.
	 */
	tmp=strtok(s," \t\r\n");
	strcpy(from,"");

	for(;tmp!=NULL;)
	{

		/*
		 * check to see if we've gotten
		 * part of the date string.
		 */
		if(strstr("SunMonTueWedThuFriSat",tmp)!=NULL)
			break;
		strcat(from,tmp);
		strcat(from," ");
		tmp=strtok((char *)0," \t\r\n");
	}

	/*
	 * At this point we either have the day, or a null pointer.
	 */
	if(tmp!=NULL)
	{
		day=tmp;
		tmp=strtok((char *)0," \t\r\n");
	}

	
	/*
	 * At this point we either have the month, or a null pointer.
	 */
	if(tmp!=NULL)
	{
		month=tmp;
		tmp=strtok((char *)0," \t\r\n");
	}
	/*
	 * At this point we either have the date, or a null pointer.
	 */

	if(tmp!=NULL)
	{
		date=tmp;
		tmp=strtok((char *)0," \t\r\n");
	}
	/*
	 * At this point we either have the time, or a null pointer.
	 */

	if(tmp!=NULL)
	{
		time=tmp;
		tmp=strtok((char *)0," \t\r\n");
	}

	/*
	 * At this point we either have the size, or a null pointer.
	 */
	if(tmp!=NULL)
	{
		size=tmp;
		tmp=strtok((char *)0,"");
	}

	/*
	 * At this point we either have the subject, or a null pointer.
	 */
	if(tmp!=NULL)
	{
		subject=(&tmp[strspn(tmp," \t")]);
	}

	tmp=(char *)malloc(200);

	/*
	 * it would be nice to be able to allow the user
	 * to specify the fields printed... However, I suspect
	 * that not every machine will have all these fields and, I
	 * suspect that some may allow you to ignore them in the header
	 * lines... So... for now I will just do a sprintf and wait for
	 * the complaints about things I don't currently know about.
	 */
	if(xmtformat!=NULL)
	{
		strcpy(tmp,"");
		if(xmtf_from)
		{

			sprintf(tmp2,"%*s",xmtf_from,from);
			if(strlen(tmp2)>abs(xmtf_from))
				tmp2[xmtf_from]=(char)0;


			strcat(tmp,tmp2);
		}
		if(xmtf_day)
		{
			sprintf(tmp2," %*s",xmtf_day,day);

			if(strlen(tmp2)>abs(xmtf_day)+1)
				tmp2[xmtf_day+1]=(char)0;

			strcat(tmp,tmp2);
		}
		if(xmtf_month)
		{
			sprintf(tmp2," %*s",xmtf_month,month);

			if(strlen(tmp2)>abs(xmtf_month)+1)
				tmp2[xmtf_month+1]=(char)0;

			strcat(tmp,tmp2);
		}
		if(xmtf_date)
		{
			sprintf(tmp2," %2s",date);
			strcat(tmp,tmp2);
		}
		if(xmtf_time)
		{
			sprintf(tmp2," %5s",time);
			strcat(tmp,tmp2);
		}
		if(xmtf_size)
		{
			sprintf(tmp2," %*s",xmtf_size,size);

			if(strlen(tmp2)>abs(xmtf_size)+1)
				tmp2[xmtf_size+1]=(char)0;

			strcat(tmp,tmp2);
		}
		if(xmtf_subject)
		{
			sprintf(tmp2," %s",subject);
			strcat(tmp,tmp2);
		}

	}
	else
		sprintf(tmp,"%-18s %3s %3s %2s %5s %9s %s",from,day,month,
			date,time,size,subject);

	return(tmp);
}




show_msg(msg)
int msg;
{
	if(Letters_Top==0)
	{
		display_msg((struct HEADLIST *)NULL,False);
		return;
	}

	display_msg(sortedList[msg],True);

	
}

/*
 * This function should be called if there isn't
 * a valid list of current messages from a read-write
 * mailbox file.
 */
void
load_mail_file(fn)
char *fn; 		/* actual file name to change to */
{
extern Widget folder_name_item;
char message[1024];
static Arg arglist[]={{XtNlabel,(XtArgVal) 0}};

	warn("Loading messages.  -- Please Wait --\n");

	if(strcmp(fn,system_mbox_fname)==0)
	{
		XtSetSensitive(rtsf,False);
		arglist[0].value=(XtArgVal) "System Mail Box";
	}
	else
	{
		XtSetSensitive(rtsf,True);
		arglist[0].value=(XtArgVal) fn;
	}

	XtSetValues(folder_name_item,arglist,1);

	if(LoadMB(fn)==-1)
	{
		refresh_headers(0);
		chang_cursor(toplevel,norm_cur);
		display_msg(NULL,False);
		XFlush(dpy);
		return;
	}

	if(OldMessageCount == LetterCount)
		sprintf(message,"%s: %d messages\n",fn,LetterCount);
	else
		sprintf(message,"%s: %d messages, %d unread\n",fn,
				LetterCount,LetterCount-OldMessageCount);

	warn(message);


	if(Letters_Top==(struct HEADLIST *)0)
	{
			XtSetSensitive(next_item,False);
			XtSetSensitive(load_item,False);
			XtSetSensitive(del_item,False);
			XtSetSensitive(reply_item,False);
			XtSetSensitive(save_item,False);
			XtSetSensitive(print_item,False);
			XtSetSensitive(undel_item,False);
		/*	display_msg(NULL,False); */
	}
	else
	{
			XtSetSensitive(next_item,True);
			if(editable_msg)
				XtSetSensitive(load_item,True);
			XtSetSensitive(del_item,True);
			XtSetSensitive(undel_item,True);
			XtSetSensitive(reply_item,True);
			XtSetSensitive(save_item,True);
			XtSetSensitive(print_item,True);
			if(showlast)
				cur_msg_num=LetterCount-1;
			else
				cur_msg_num=0;
		/*	display_msg(sortedList[0],False);*/
		/*
		 * need to set selection here.
		 */
	}

	refresh_headers(0);
	chang_cursor(toplevel,norm_cur);
	XFlush(dpy);
}

int
current_position()
{
Widget oldsrc;
XawTextBlock text;
int dposition_tmp,return_value;
static Arg arglist[]={{XtNdisplayPosition,(XtArgVal) 0}};

	arglist[0].value=(XtArgVal)&dposition_tmp;
	XtGetValues(msg_box,arglist,1);
	if(bodyOnly)
	{
		return_value = dposition_tmp+2;
	}
	else
	{
		oldsrc = XawTextGetSource(msg_box);
		text.firstPos=0;
		text.length=2;
		text.ptr="\n\n";
		return_value = dposition_tmp -
			XawTextSourceSearch(oldsrc,1,XawsdRight,&text) +
			1;
		if(return_value)
			return_value=0;
	}

	return(return_value);
}

/*
 * This function should be called only if there is
 * a valid list of current messages from a read-write
 * mailbox file.
 */
void
change_mail_file(fn)
char *fn; 		/* actual file name to change to */
{
extern Widget folder_name_item;

	chang_cursor(toplevel,watch_cur);

	if(Letters_Top!=(struct HEADLIST *)0)
	{
		if(VALIDMSG(cur_msg_num))
		{
			sortedList[cur_msg_num]->display_position =
				current_position();
		}

		warn("Saving current messages.  -- Please Wait --\n");
		if(SaveMB(cur_mbox_fname)==-1)
			sleep(10);
	}

	load_mail_file(fn);
}

void
park_mail()
{
	change_mail_file(system_mbox_fname);
}

record_outbound(f)
char *f;
{
int fd,line_len,do_chmod;
FILE *rec_file,*msg_file;
char *tmp_fname,line_buff[1024];
time_t my_time;
extern char *record,*username,*expand_value();

	if(record==(char *)0)
		return;

	tmp_fname=expand_value(record);

	if((fd=open(tmp_fname,O_WRONLY|O_APPEND|O_CREAT,0600))==-1)
	{
		perror(record);
		free(tmp_fname);
		return;
	}

	free(tmp_fname);

	if((rec_file=fdopen(fd,"a"))==NULL)
	{
		perror(record);
		return;
	}

	if((msg_file=fopen(f,"r"))==NULL)
	{
		perror(f);
		fclose(rec_file);
		return;
	}

	my_time=time((time_t *)0);
	fprintf(rec_file,"From %s %s",username,ctime(&my_time));
	fflush(rec_file);

	line_buff[0]='>';
	for(;;)
	{
		if(fgets(&line_buff[1],1023,msg_file)==NULL)
			break;


		if(strncmp(&line_buff[1],"From ",5)==0)
		{
			fputs(line_buff,rec_file);
		}
		else
			fputs(&line_buff[1],rec_file);
	}

	fputs("\n\n",rec_file);
	fclose(msg_file);
	fclose(rec_file);
	return;
}

fix_msg_file(f)
char *f;
{
	char *new_file,*tmp_line_ptr;
	FILE *nfp,*ofp;
	int nfd;
	char line_buff[1024];

	if((ofp=fopen(f,"r"))==NULL)
	{
		perror(f);
		warn("Couldn't open outbound message file\n");
		return;
	}

	new_file=(char *)malloc(100);
	if((nfd=my_mkstemp(new_file,""))==-1)
	{
		warn("Couldn't create temporary outbound message file.\n");
		fclose(ofp);
		free(new_file);
		return;
	}

	if((nfp=fdopen(nfd,"w+"))==NULL)
	{
		perror("fdopen");
	}

	/*
	 * start by copying everything from the header
	 * exactly as it is.  Looking for the HEADER_END_MARK string.
	 */
	for(;;)
	{
		if(fgets(line_buff,1024,ofp)==NULL)
		{
			warn("You removed the HEADER END MARK!!!\n");
			sleep(10);
			break;
		}

		/*
		 * take out any extra empty lines.
		 */
		if(strcmp("\n",line_buff)==0)
			continue;

		if(strcmp(HEADER_END_MARK,line_buff)==0)
		{
			sprintf(line_buff,"X-Mailer: [%s%s]\n\n",
					PATCHLEVEL,BUGFIXLEVEL);
			fputs(line_buff,nfp);
			break;
		}

		fputs(line_buff,nfp);
	}

	line_buff[0]='>';
	tmp_line_ptr=(&line_buff[1]);
	
	for(;;)
	{

		if(fgets(tmp_line_ptr,1023,ofp)==NULL)
			break;

		if(strncmp(tmp_line_ptr,"From ",5)==0)
		{
			if(fputs(line_buff,nfp)==EOF)
			{
				perror(f);
				warn("Couldn't write message file\n");
				free(new_file);
				fclose(ofp);
				fclose(nfp);
				return;
			}
		}
		else
		{
			if(fputs(tmp_line_ptr,nfp)==EOF)
			{
				perror(f);
				warn("Couldn't write message file\n");
				free(new_file);
				fclose(ofp);
				fclose(nfp);
				return;
			}
		}
	}

	fclose(ofp);
	fclose(nfp);
	unlink(f);
	link(new_file,f);
	unlink(new_file);
	free(new_file);
	return;
}

int
send_reply(f)
char *f;
{
int fd,reply_pid;
extern char *sendmail,*SHELL;
extern int do_xmtaliases,do_xmtrecord;

	safe_to_exit=0;
	/*
	 * this is used to prepend From lines
	 * with > so that when they are read, they
	 * don't appear as individual messages.
	 * It also puts in the X-Mailer: line.
	 * and removes the "-----End Header----" line.
	 */
	
	fix_msg_file(f);

	if(do_xmtaliases)
	{
		exp_addrs(f);
	}

	if(do_xmtrecord)
	{
		record_outbound(f);
	}

	if((fd=open(f,O_RDONLY))==-1)
	{
		warn("couldn't open message file");
			XBell (dpy, 10);
			XFlush(dpy);
		safe_to_exit=1;
		return(-1);
	}

	if((reply_pid=fork())==0)
	{
		close(0);
		dup(fd);
		if(strcmp(sendmail,MSENDER)==0)
		{
#ifdef USE_SENDMAIL
			execlp(MSENDER,"sendmail","-t",0);
#else
			execlp(MSENDER,MSENDER,"-t",0);
#endif

		}
		else
		{
			execlp(SHELL,SHELL,"-c",sendmail,0);
		}
		XBell (dpy, 10);
		XFlush(dpy);
	}

	if(reply_pid==-1)
	{
		warn("Couldn't fork() process for sendmail().\n");
			XBell (dpy, 10);
			XFlush(dpy);
		safe_to_exit=1;
		return(-1);
	}

	close(fd);
	safe_to_exit=1;
	if(caught_fatal_signal)
	{
		fprintf(stderr,"send_reply():  Caught_fatal_signal.  Exiting...\n");
		clean_up();
		exit(0);
	}
	return(0);

}

int
delete_msg(msg_num)
{
MMSETDEL(sortedList[msg_num]);
sortedList[msg_num]->modified += MMDELETE;
	if(!show_deleted || skipDeleted)
	{
		for(;msg_num<LetterCount &&
			ISMMDEL(sortedList[msg_num]);
			msg_num++);
	
		if(msg_num >= LetterCount)
		{
			msg_num--;
			for(;msg_num>=0 &&
				ISMMDEL(sortedList[msg_num]);
				msg_num--);
		}
	
		if(msg_num < 0)
		{
	
			if(!show_deleted)
			{
				warn("No messages left.");
				display_msg((struct HEADLIST *)0,False);
			}
		}
		else
		{
			display_msg(sortedList[msg_num],True);

		}
	}
	cur_msg_num=msg_num;
	refresh_headers(0);
}


