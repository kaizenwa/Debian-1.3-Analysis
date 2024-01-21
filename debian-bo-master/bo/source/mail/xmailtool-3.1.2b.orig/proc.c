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

static char proc_rcsid[]="$Id: proc.c,v 1.77 1995/05/12 18:50:19 bobo Exp $";

#include <stdio.h>
#include <unistd.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/List.h>
#include <X11/Shell.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <sys/errno.h>
#include <fcntl.h>
#include "defs.h"

#include "mailhandler.h"

#include <sys/stat.h>


#define min(a,b) ((a)<(b) ? (a) : (b))

extern struct HEADLIST **sortedList;

extern Cursor watch_cur,norm_cur;
extern int errno,cur_msg_num,alwaysignore,keepsave,showlast,state_box,hold;
extern int debug,editable_msg,LetterCount;
extern find_line();
extern Widget command_button();
extern Widget text_label();
extern look_at_line();
extern struct HEADLIST *Letters_Top,*del_list, *cur_msg,*current_msg;
extern int mail_out,mail_in;
extern char *get_data(),file_fname[],pwd[],*EDITOR,*UNAME_STRING,editor_fname[];
extern do_signature;
extern char msg_fname[],*outbound_suffix;
extern char *EDITOR, *MAIL, *DEAD,*cur_mbox_fname,*hypertext,*PRINTER,
	*named_mailrc;
extern int edit_needs_xterm,do_xmtaccelerators,show_deleted,skipDeleted,bodyOnly,saveCurrent;
extern struct proc_msg *pptr_last,*pptr_first;
extern char *SHELL,*MBOX,*system_mbox_fname,*expand_value();
extern Atom wm_delete_window;


extern int num_headers;	/* the number of the header with the highest number */
extern int header_index;
extern int header_right,header_left;	

extern Display *dpy;
extern Window win;
extern Arg place_arg[];

extern Widget toplevel;
extern Widget replywidg,helpsw;

extern Widget deliver_item,cancel_item,cd_item,reply_item,done_item,command_button();
extern Widget next_item,show_item,del_item,undel_item,print_item,text_label();
extern Widget quit_item,mailrc_item,pre_item,copy_item,aboutsw;
extern Widget save_item,folder_item,nmail_item,comp_item,mail_msg,
		preserve_item,saved_menu,saved_list,SaveCurrent_item,show_deleted_item,bodyOnly_item;

extern Widget header_box,cmd_box,msg_box,box,replysw,rtsf;

extern Widget  file_item,file_cmd_box,dir_cmd_box,file_name_item,load_item;
extern Cursor del_cur,save_cur,pres_cur,show_cur;
extern Atom      wm_delete_window;
extern char *ABOUT_STRING;
extern char *print_cmd;
extern char dbox_fname[];
extern void park_mail();
extern Boolean is_iconic;
extern Pixmap envelope;
extern int askcc,askbcc,replyall;
extern int asksub,include_msg;
extern char *mprefix,*replyto, *realname;
extern int do_signature;
extern char *MAILRC,*folder;
extern Widget lst_list,list_folder;
extern char fdirname[],*fnames[];
extern int use_folders_cmd;
extern int current_sf_index,num_saved_fnames;
extern char **saved_fnames;

Window root,child;
int win_x,win_y,root_x,root_y;
unsigned int keys;

static char tmp_buff[MAXMESLEN];
extern int in_fd[],out_fd[];
extern char cmd_buff[];


extern char	*NoToLine,
		*NoHFromLine,
		*NoCcLine,
		*NoSubjectLine,
		*NoReturnPathLine,
		*NoReplyToLine,
		*NoInReplyToLine,
		*NoReferencesLine,
		*NoKeywordsLine,
		*NoCommentsLine,
		*NoEncryptedLine,
		*NoDateLine,
		*NoMessageIdLine,
		*NoSenderLine,
		*NoContentTransferEncodingLin,
		*NoXMailerLine,
		*NoXFaceLine,
		*NoAttachment;

void
center_wig_on_pointer(w)
Widget w;
{
XtWidgetGeometry my_geom;


	XQueryPointer(dpy,win,&root,&child,(int *)&root_x,
			(int *)&root_y,&win_x,&win_y,&keys);

	XtQueryGeometry(w,NULL,&my_geom);

	place_arg[0].value=(XtArgVal) root_x - 
				(my_geom.width/2 + my_geom.border_width);
	place_arg[1].value=(XtArgVal) root_y - 
				(my_geom.height/2 + my_geom.border_width);
	XtSetValues(w,place_arg,2);
}

int pointer_state;

/* ARGSUSED */
void
sd_state_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
int msg_num;
	
	show_deleted=(int)callData;

	if(LetterCount <=0)
		return;

	if(!show_deleted)
	{
		for(msg_num=(cur_msg_num>=0?cur_msg_num:0)
				;msg_num<LetterCount &&
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
	
				warn("No messages left.");
				display_msg((struct HEADLIST *)0,False);
		}
		else
		{
			display_msg(sortedList[msg_num],True);

		}
		cur_msg_num=msg_num;

	}
	else
	{

		if(!VALIDMSG(cur_msg_num) && (LetterCount>0))
		{
			cur_msg_num=(showlast?(LetterCount-1):0);
			display_msg(sortedList[cur_msg_num],True);

		}
	}
	
	refresh_headers(0);

}

/* ARGSUSED */
void
sc_state_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
	
	saveCurrent=(int)callData;
}

/* ARGSUSED */
void
bo_state_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
	
	bodyOnly=(int)callData;
	if(VALIDMSG(cur_msg_num))
		display_msg(sortedList[cur_msg_num],True);
}

/* ARGSUSED */
void
state_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{

	if((int)callData == 1)
	{
		pointer_state=(int)closure;
		switch(pointer_state) {
		case 0:
			chang_cursor(header_box,show_cur);
			break;
		case 1:
			chang_cursor(header_box,del_cur);
			break;
		case 2:
			chang_cursor(header_box,save_cur);
			break;
		case 3:
			chang_cursor(header_box,pres_cur);
			break;
		}
	}
	

	return;

}

/* ARGSUSED */
void
load_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{

	return;
}


/* ARGSUSED */
void
next_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
int tmp_msg_num;

	if(!VALIDMSG(cur_msg_num))
	{
		warn("no messages in buffer\n");
		return;
	}

	if(callData==(char *)0 || strcmp(callData,"next")==0)
	{
	 	tmp_msg_num=cur_msg_num+1;
		if(!show_deleted)
			for(;tmp_msg_num<LetterCount && 
				ISMMDEL(sortedList[tmp_msg_num]);
						tmp_msg_num++);

		if(tmp_msg_num>=LetterCount)
		{
			tmp_msg_num=cur_msg_num-1;
			if(!show_deleted)
				for(;tmp_msg_num>=0 &&
			 		ISMMDEL(sortedList[tmp_msg_num]);
					tmp_msg_num--);
		}

		if(VALIDMSG(tmp_msg_num))
			cur_msg_num=tmp_msg_num;

	}
	else if(strcmp(callData,"prev")==0)
	{
		tmp_msg_num=cur_msg_num-1;
		if(!show_deleted)
			for(;tmp_msg_num>=0 &&
			 	ISMMDEL(sortedList[tmp_msg_num]);
				tmp_msg_num--);

		if(tmp_msg_num<0)
		{
			tmp_msg_num=cur_msg_num+1;
			if(!show_deleted)
				for(;tmp_msg_num<LetterCount && 
					ISMMDEL(sortedList[tmp_msg_num]);
						tmp_msg_num++);
		}

		if(VALIDMSG(tmp_msg_num))
			cur_msg_num=tmp_msg_num;
	
	}
	else
	{
		warn("not a valid command\n");
		return;
	}

	display_msg(sortedList[cur_msg_num],True);
	XawTextSetInsertionPoint(header_box,sortedList[cur_msg_num]->hdr_left);
}

/* ARGSUSED */
void
help_proc(widget,closure,callData)
	Widget widget;
	caddr_t closure;
	caddr_t callData;
{
#ifdef HYPERTEXT
char hypertext_cmd[1024];

	sprintf(hypertext_cmd,"Executing %s %s... Please stand by.\n",hypertext,DEFURL);
	warn(hypertext_cmd);

	sprintf(hypertext_cmd,"%s %s",
		hypertext,DEFURL);

	if(fork()==0)	
		execlp(SHELL,SHELL,"-c",hypertext_cmd,0);

#else
	XtPopup(helpsw,XtGrabNone);
	(void) XSetWMProtocols(dpy, XtWindow(helpsw),
			&wm_delete_window, 1);
	XSetTransientForHint(dpy,XtWindow(helpsw),XtWindow(toplevel));
#endif

}

/* ARGSUSED */
void
about_proc(widget,closure,callData)
	Widget widget;
	caddr_t closure;
	caddr_t callData;
{

	warn(ABOUT_STRING);
	/*center_wig_on_pointer(aboutsw);*/
	XtPopup(aboutsw,XtGrabNone);
	(void) XSetWMProtocols(dpy, XtWindow(aboutsw),
			&wm_delete_window, 1);
	XSetTransientForHint(dpy,XtWindow(aboutsw),XtWindow(toplevel));

}

/* ARGSUSED */
void
del_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{

	if(!VALIDMSG(cur_msg_num))
	{
		warn("no messages in buffer\n");
		return;
	}

	if(callData==(char *)0  ||
			strcmp(callData,"delete")==0)
	{
		if(ISMMDEL(sortedList[cur_msg_num]))
		{
			warn("This message is already marked for deletion.\n");
			return;
		}

		delete_msg(cur_msg_num);
	}
	else if(strcmp(callData,"undelete")==0)
	{
		if(!ISMMDEL(sortedList[cur_msg_num]))
		{
			warn("This message isn't marked for deletion.\n");
			return;
		}
		MMUNDEL(sortedList[cur_msg_num]);
		sortedList[cur_msg_num]->modified=-MMDELETE;
		refresh_headers(0);
	}

	else
	{
		warn("not a valid command\n");
		return;
	}



}


/* ARGSUSED */
void
undel_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{

	if(!VALIDMSG(cur_msg_num))
	{
		warn("no messages in buffer\n");
		return;
	}

	if(ISMMDEL(sortedList[cur_msg_num]))
	{
		MMUNDEL(sortedList[cur_msg_num]);
		sortedList[cur_msg_num]->modified=-MMDELETE;
		refresh_headers(0);
	}
	else
	{
		warn("That message hasn't been marked for deletion.");
	}
	
}

/* ARGSUSED */
void
cancel_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
	XtPopdown((Widget)closure);
	XtSetSensitive(cmd_box,True);

}



/* ARGSUSED */
void
save_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
char *tmp_fname;
int return_value;

	if(!VALIDMSG(cur_msg_num))
	{
		warn("no messages in buffer\n");
		return;
	}

	if(callData==(char *)0  ||
			strcmp(callData,"save")==0)
	{
		if(strlen(file_fname)==0)
		{
			tmp_fname=expand_value(MBOX,1);
			sprintf(tmp_buff,"Copying %d lines of #%d to %s ...\n",
					sortedList[cur_msg_num]->bodyCount, cur_msg_num+1,
					tmp_fname);
			warn(tmp_buff);
			return_value=CopyMessage(sortedList[cur_msg_num],tmp_fname,
							1,0,alwaysignore);
		}
		else
		{
			tmp_fname=expand_value(file_fname,1);
			sprintf(tmp_buff,"Copying %d lines of #%d to %s ...\n",
					sortedList[cur_msg_num]->bodyCount, cur_msg_num,tmp_fname);
			warn(tmp_buff);
			return_value=CopyMessage(sortedList[cur_msg_num],tmp_fname,
							1,0,alwaysignore);
			if(return_value!=-1)
				add_saved_file(file_fname);
		}
	}
	else if(strcmp(callData,"Save")==0)
	{
		if(sortedList[cur_msg_num]->From_username==(char *)0)
		{
			warn("username not valid");
			return;
		}
		sprintf(tmp_buff,"+%s",sortedList[cur_msg_num]->From_username);
		tmp_fname=expand_value(tmp_buff,1);
		sprintf(tmp_buff,"Copying %d lines of #%d to %s ...\n",
			sortedList[cur_msg_num]->bodyCount,cur_msg_num+1,
			tmp_fname);
		warn(tmp_buff);
		return_value=CopyMessage(sortedList[cur_msg_num],tmp_fname,
							1,0,alwaysignore);
	}
	else if(strcmp(callData,"copy")==0)
	{
		if(strlen(file_fname)==0)
		{
			tmp_fname=expand_value(MBOX,1);
			sprintf(tmp_buff,"Copying %d lines of #%d to %s ...\n",
					sortedList[cur_msg_num]->bodyCount, cur_msg_num+1,
					tmp_fname);
			warn(tmp_buff);
			return_value=CopyMessage(sortedList[cur_msg_num],tmp_fname,
							1,0,alwaysignore);
		}
		else
		{
			tmp_fname=expand_value(file_fname,1);
			sprintf(tmp_buff,"Copying %d lines of #%d to %s ...\n",
					sortedList[cur_msg_num]->bodyCount, cur_msg_num,tmp_fname);
			warn(tmp_buff);
			return_value=CopyMessage(sortedList[cur_msg_num],tmp_fname,
							1,0,alwaysignore);
			if(return_value!=-1)
				add_saved_file(file_fname);
		}
		if(return_value!=-1)
		{
			sprintf(tmp_buff,"Copied %d lines of #%d to %s.\n",
				sortedList[cur_msg_num]->bodyCount,
				cur_msg_num+1,tmp_fname);

			warn(tmp_buff);
		}
		free(tmp_fname);
		return;
	}
	else if(strcmp(callData,"Copy")==0)
	{
		sprintf(tmp_buff,"+%s",sortedList[cur_msg_num]->From_username);
		tmp_fname=expand_value(tmp_buff,1);
		sprintf(tmp_buff,"Copying %d lines of #%d to %s ...\n",
			sortedList[cur_msg_num]->bodyCount,cur_msg_num+1,
			tmp_fname);
		warn(tmp_buff);
		if((return_value=CopyMessage(sortedList[cur_msg_num],tmp_fname,
							1,0,alwaysignore))!=-1)
		{
			sprintf(tmp_buff,"Copied %d lines of #%d to %s.\n",
				sortedList[cur_msg_num]->bodyCount,
				cur_msg_num+1,tmp_fname);

			warn(tmp_buff);
		}
		free(tmp_fname);
		return;
	}
	else
	{
		sprintf(tmp_buff,"%s isn't a recognized command\n",callData);
		warn(tmp_buff);
		return;
	}




	if(return_value!=-1)
	{
		MMSETSAVED(sortedList[cur_msg_num]);

		sprintf(tmp_buff,"Saved %d lines of #%d to %s.\n",
			sortedList[cur_msg_num]->bodyCount,
			cur_msg_num+1,tmp_fname);

		warn(tmp_buff);

		if(!keepsave)
		{
			delete_msg(cur_msg_num);
		}
		else
		{
			refresh_headers(0);
		}
	}

	free(tmp_fname);
}


/* ARGSUSED */
void
print_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
int fd,pr_pid,print_fd[2],nread;
char *message, *getenv();
char line_buff[1024];

	if(!VALIDMSG(cur_msg_num))
	{
		warn("no messages in buffer\n");
		return;
	}

	if((fd=open(msg_fname,O_RDONLY))==-1)
	{
		perror("open(message file)");
		warn("couldn't open message file");
			XBell (dpy, 10);
			XFlush(dpy);
		return;
	}


	if(pipe(print_fd)==-1)
	{
		perror("Can't create printer pipe");
		warn("Can't create printer pipe\n");
		return;
	}

	if((pr_pid=fork())==0)
	{
		close(0);
		close(1); 
		close(2);
		close(print_fd[0]);
		dup(fd);
		dup(print_fd[1]);
		dup(print_fd[1]);
		execlp(SHELL,SHELL,"-c",(callData==0?print_cmd:callData),
					(char *)0);

	}
	if(pr_pid==-1)
	{
		warn("Couldn't start printing proccess.  Sorry.");
			XBell (dpy, 10);
			XFlush(dpy);
		return;
	}
	close(fd);
	close(print_fd[1]);
	message=(char *)malloc(2048);
	strcpy(message, "Sent message to printer ");
 	strcat(message, PRINTER);
	strcat(message," using \"");
	strcat(message,(callData==0?print_cmd:callData));
	strcat(message,"\"");
 	strcat(message, ".\nPrinter Output:\n");
	
	for(;;)
	{
		nread=read(print_fd[0],line_buff,1023);
		if(nread<1)
			break;
		line_buff[nread]=(char)0;
		message=(char *)realloc(message,strlen(message)+nread+1);
		strcat(message,line_buff);
	}
	close(print_fd[0]);
		
	warn(message);
	free(message);

}

/* ARGSUSED */
void
nmail_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
char message[1024];
static Arg arglist[]={{XtNdisplayPosition,(XtArgVal)0}};
struct HEADLIST *tmp_msg;
int previous_letter_count;


	if(callData==(char *)0  ||
			strcmp(callData,"nmail")==0)
	{
		change_mail_file(system_mbox_fname);
		if(LetterCount == 0)
			display_msg((struct HEADLIST *)NULL,False);
		else
			display_msg(sortedList[cur_msg_num],True);
		return;
	}
	else if(strcmp(callData,"Update")==0)
	{
		if(Letters_Top!=(struct HEADLIST *)0)
		{
			tmp_msg=sortedList[cur_msg_num];
			current_msg=sortedList[cur_msg_num];
			current_msg->display_position = current_position();
			warn("Updating message list... Please wait.");
			previous_letter_count=LetterCount;
			UpdateSysMB(cur_mbox_fname);
			if(previous_letter_count==LetterCount)
				sprintf(message,"%s: %d messages.",cur_mbox_fname,LetterCount);
			else
				sprintf(message,"%s: %d messages, %d added during update.\n",
						cur_mbox_fname,
						LetterCount,LetterCount-previous_letter_count);
			warn(message);
			current_msg=tmp_msg;
			refresh_headers(0);
/*			display_msg(tmp_msg,True); */
			return;
		}
		else
		{
			warn("Mailbox not open yet.\n");
			return;
		}
	}
	
}

/* ARGSUSED */
void
done_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
Status make_iconic;

	chang_cursor(toplevel,watch_cur);
	
	make_iconic = XIconifyWindow(
		XtDisplay(toplevel),
		XtWindow(toplevel),
		DefaultScreen(XtDisplay(toplevel)));

	XFlush(XtDisplay(toplevel));

	if(make_iconic == 0)
	{
		warn("Unable to iconify");
			XBell (dpy, 10);
			XFlush(dpy);
		/* No Vallet parking.  We'll have to park it our selves. */
		park_mail();
		is_iconic=True;
	}


	chang_cursor(toplevel,norm_cur);
	

}


/* ARGSUSED */
void
outbound_cancel(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
struct proc_msg *parm;
char message[1024];

	parm=(struct proc_msg *)closure;
	XawAsciiSave(XawTextGetSource(parm->text));

	if(fork()==0)
	{
		execlp("cp","cp",parm->file,DEAD,0);
		perror("execlp");
	}

	sleep(1);
	sprintf(message,"Outbound message has been copied to %s\n",DEAD);
	warn(message);
	XtPopdown(parm->box);
	XtDestroyWidget(parm->box);
	unlink(parm->file);
	free(parm->file);

	if(parm->prev!=(struct proc_msg *)0)
	{
		parm->prev->next=parm->next;
	}
	else
	{
		pptr_first=parm->next;
	}

	if(parm->next!=(struct proc_msg *)0)
	{
		parm->next->prev=parm->prev;
	}
	else
	{
		pptr_last=parm->prev;
	}

	free(parm);
}

int
creat_outbound(reply_file,ins,t)
char *reply_file,*t;
int ins;
{
struct proc_msg *parm;
Widget reply_vpane,reply_cmd_box;
void deliver_proc();
int edit_pid;
struct proc_msg *pptr_tmp;
struct stat buff;
char *EDIT_STRING;
static Arg shell_arg[2];
static Arg file_arg[] = {{XtNstring,(XtArgVal)0},
		{XtNtype,(XtArgVal)XawAsciiFile},
		{XtNeditType,(XtArgVal)XawtextEdit},
		{XtNinsertPosition,(XtArgVal)0},
		{XtNdisplayPosition,(XtArgVal)0},
};

	if((EDITOR!=(char *)0) && strcmp(EDITOR,"none")!=0)
	{
		if((edit_pid=fork())==-1)
		{
			warn("Couldn't Fork editor Process\n");
			XBell (dpy, 10);
			XFlush(dpy);
			return(-1);
		}

		if(edit_pid==0)
		{

			if(edit_needs_xterm)
			{
				EDIT_STRING=(char *)malloc(strlen(EDITOR)+
						strlen(reply_file)+
						strlen(t) + 55);
				sprintf(EDIT_STRING,
					"xterm -name outbound_msg -T %s -e %s %s",
					t,EDITOR,reply_file);
				execlp(SHELL,SHELL,"-c",EDIT_STRING,0L);
			}
			else
			{
				EDIT_STRING=(char *)malloc(strlen(EDITOR)+
						strlen(reply_file)+2);
				sprintf(EDIT_STRING,"%s %s",EDITOR,reply_file);
				execlp(SHELL,SHELL,"-c",EDIT_STRING,0L);
			}

			perror("Couldn't exec xterm EDITOR");
			XBell (dpy, 10);
			XFlush(dpy);
			exit(1);
		}

		if(edit_pid!=0)
		{
			pptr_tmp=(struct proc_msg *)malloc(
					sizeof(struct proc_msg));
			stat(reply_file,&buff);
			pptr_tmp->mtime=buff.st_mtime;
			pptr_tmp->file=reply_file;
			pptr_tmp->pid=edit_pid;
			pptr_tmp->next=NULL;
			pptr_tmp->box=(Widget)0;
			pptr_tmp->text=(Widget)0;
			if(pptr_first==NULL)
			{
				pptr_tmp->prev=NULL;
				pptr_first=pptr_tmp;
			}
			else
			{
				pptr_tmp->prev=pptr_last;
				pptr_last->next=pptr_tmp;
			}
			pptr_last=pptr_tmp;
			
		}
		return(0);
	}	

	XtSetArg(shell_arg[0],XtNiconPixmap,envelope);
	XtSetArg(shell_arg[1],XtNtitle,t);
	XtSetArg(file_arg[0],XtNstring,reply_file);
	XtSetArg(file_arg[3],XtNinsertPosition,ins);

	/*
	 * Reply/Compose pop up window.
	 */
	replysw=XtCreatePopupShell("outbound_msg",applicationShellWidgetClass,
		toplevel,shell_arg,2);

	reply_vpane=XtCreateManagedWidget("reply_vpane",panedWidgetClass,
					replysw, NULL, 0);

	reply_cmd_box=XtCreateManagedWidget("reply_cmd_box",boxWidgetClass,
                       	 reply_vpane,NULL,0);

	replywidg=XtCreateManagedWidget("reply",asciiTextWidgetClass,
                        reply_vpane,file_arg,XtNumber(file_arg));

	set_text_columns(replywidg,80);

	pptr_tmp=(struct proc_msg *)malloc(sizeof(struct proc_msg));
	stat(reply_file,&buff);
	pptr_tmp->mtime=buff.st_mtime;
	pptr_tmp->file=reply_file;
	pptr_tmp->pid=0;
	pptr_tmp->next=NULL;
	pptr_tmp->box=replysw;
	pptr_tmp->text=replywidg;
	if(pptr_first==NULL)
	{
		pptr_tmp->prev=NULL;
		pptr_first=pptr_tmp;
	}
	else
	{
		pptr_tmp->prev=pptr_last;
		pptr_last->next=pptr_tmp;
	}
	pptr_last=pptr_tmp;

	deliver_item=command_button("send",reply_cmd_box,
			deliver_proc,pptr_tmp);
	if(do_xmtaccelerators)
		XtInstallAccelerators(replywidg,deliver_item);

	cancel_item=command_button("cancel", reply_cmd_box,
			outbound_cancel,pptr_tmp);

	if(do_xmtaccelerators)
		XtInstallAccelerators(replywidg,cancel_item);

	XtPopup(replysw,XtGrabNone);

	(void) XSetWMProtocols(dpy, XtWindow(replysw),
			&wm_delete_window, 1);
	return(0);
}

void
confirm_send(reply_file)
char *reply_file;
{
struct proc_msg *pptr_tmp;
struct stat buff;
Widget reply_vpane,reply_cmd_box;
void deliver_proc();
static Arg shell_arg[2];
static Arg file_arg[] = {{XtNstring,(XtArgVal)0},
		{XtNtype,(XtArgVal)XawAsciiFile},
		{XtNeditType,(XtArgVal)XawtextEdit},
		{XtNinsertPosition,(XtArgVal)0},
		{XtNdisplayPosition,(XtArgVal)0},
};

	XtSetArg(shell_arg[0],XtNiconPixmap,envelope);
	XtSetArg(shell_arg[1],XtNtitle," Confirm ?? ");
	XtSetArg(file_arg[0],XtNstring,reply_file);

	/*
	 * Reply/Compose pop up window.
	 */
	replysw=XtCreatePopupShell("outbound_msg",applicationShellWidgetClass,
		toplevel,shell_arg,2);

	reply_vpane=XtCreateManagedWidget("reply_vpane",panedWidgetClass,
					replysw, NULL, 0);

	reply_cmd_box=XtCreateManagedWidget("reply_cmd_box",boxWidgetClass,
                       	 reply_vpane,NULL,0);

	replywidg=XtCreateManagedWidget("reply",asciiTextWidgetClass,
                        reply_vpane,file_arg,XtNumber(file_arg));

	set_text_columns(replywidg,80);

	pptr_tmp=(struct proc_msg *)malloc(sizeof(struct proc_msg));
	stat(reply_file,&buff);
	pptr_tmp->mtime=buff.st_mtime;
	pptr_tmp->file=reply_file;
	pptr_tmp->pid=0;
	pptr_tmp->next=NULL;
	pptr_tmp->box=replysw;
	pptr_tmp->text=replywidg;
	if(pptr_first==NULL)
	{
		pptr_tmp->prev=NULL;
		pptr_first=pptr_tmp;
	}
	else
	{
		pptr_tmp->prev=pptr_last;
		pptr_last->next=pptr_tmp;
	}
	pptr_last=pptr_tmp;

	deliver_item=command_button("send",reply_cmd_box,
			deliver_proc,pptr_tmp);
	if(do_xmtaccelerators)
		XtInstallAccelerators(replywidg,deliver_item);

	cancel_item=command_button("cancel", reply_cmd_box,
			outbound_cancel,pptr_tmp);

	if(do_xmtaccelerators)
		XtInstallAccelerators(replywidg,cancel_item);

	XtPopup(replysw,XtGrabNone);

	(void) XSetWMProtocols(dpy, XtWindow(replysw),
			&wm_delete_window, 1);
}


int
put_stuff_in(reply_fd,include_flag,sign_flag)
int include_flag,sign_flag;
{
int msg_fd,count=0;

	count=include_envelope(reply_fd);

	write(reply_fd,HEADER_END_MARK,strlen(HEADER_END_MARK));

	count+=strlen(HEADER_END_MARK);

	tmp_buff[0]=(char)0;

	if(include_flag)
	{	
		strcat(tmp_buff,"---------  Received message begins Here  ---------\n");

		if((msg_fd=open(msg_fname,O_RDONLY))==-1)
		{
			perror("open(message file)");
			warn("Couldn't open message file");
			XBell (dpy, 10);
			XFlush(dpy);
			return(-1);
		}

		if(write(reply_fd,tmp_buff,strlen(tmp_buff))==-1)
		{
			perror("write(message file)");
			warn("Couldn't write to reply file");
			XBell (dpy, 10);
			XFlush(dpy);
			return(-1);
		}

		strcpy(tmp_buff,"\n");
		strcat(tmp_buff,mprefix);
		for(;;)
		{
			if(readln(msg_fd,&tmp_buff[strlen(mprefix)+1],
						MAXMESLEN)==0)
				break;

			if(write(reply_fd,tmp_buff,strlen(tmp_buff))==-1)
			{
				perror("write(reply file)");
				warn("Couldn't write to reply file");
				XBell (dpy, 10);
				XFlush(dpy);
				return(-1);
			}
		}
		close(msg_fd);
	}

	if(write(reply_fd,"\n\n",2)==-1)
	{
		perror("write(reply file)");
		warn("Couldn't write reply file");
		XBell (dpy, 10);
		XFlush(dpy);
		return(-1);
	}

	if(sign_flag)
		if(append_signature(reply_fd)==-1)
		{
			perror("write(reply file)");
			warn("Couldn't write reply file");
			XBell (dpy, 10);
			XFlush(dpy);
			return(-1);
		}
	return(count);
}


/* ARGSUSED */
void
reply_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
int all_flag=0,sign_flag=0,include_flag=0;
int reply_fd,insert_position,count;
char *reply_file,*reply_arg,*reply_string,*tmp_ptr1,*tmp_ptr2;

	if(!VALIDMSG(cur_msg_num))
	{
		warn("no messages in buffer\n");
		return;
	}

	sign_flag=do_signature;
	include_flag=include_msg;

	if(callData!=(caddr_t)0)
	{
		reply_arg=strtok((char *)callData," ,-");
		for(;;)
		{
			if(reply_arg==(char *)0)
				break;

			if(strcmp(reply_arg,"all")==0)
				all_flag=1;
			else if(strcmp(reply_arg,"sign")==0)
				sign_flag=1;
			else if(strcmp(reply_arg,"nosign")==0)
				sign_flag=0;
			else if(strcmp(reply_arg,"notsign")==0)
				sign_flag=(!sign_flag);
			else if(strcmp(reply_arg,"include")==0)
				include_flag=1;
			else if(strcmp(reply_arg,"noinclude")==0)
				include_flag=0;
			else if(strcmp(reply_arg,"notinclude")==0)
				include_flag=(!include_flag);
			else
				warn("invalid argument to reply.");

			reply_arg=strtok((char *)0," ,-");
		}
	}

	reply_file=(char *)malloc(100);

	if((reply_fd=my_mkstemp(reply_file,outbound_suffix))==-1)
	{
		warn("Couldn't create Out Bound message file\n");
		XBell (dpy, 10);
		XFlush(dpy);
		free(reply_file);
		return;
	}

	write(reply_fd,"To: ",4);

	if(sortedList[cur_msg_num]->ReplyTo!=NoReplyToLine)
		reply_string=sortedList[cur_msg_num]->ReplyTo;
	else if(sortedList[cur_msg_num]->HFrom!=NoHFromLine)
		reply_string=sortedList[cur_msg_num]->HFrom;
	else if(sortedList[cur_msg_num]->Sender!=NoSenderLine)
		reply_string=sortedList[cur_msg_num]->Sender;
	else
		reply_string="";

	/*
	 * if reply_string is of the form "Something <address>"
	 * we need to extract the "address" portion of this
	 * value.
	 */
	if((tmp_ptr1=strchr(reply_string,'<'))!=(char *)0)
	{
		strcpy(tmp_buff,tmp_ptr1+1);
		if((tmp_ptr2=strchr(tmp_buff,'>'))!=(char *)0)
		{
			*tmp_ptr2=(char)0;
			reply_string=tmp_buff;
		}
	}

	write(reply_fd,reply_string,strlen(reply_string));
	if(all_flag && sortedList[cur_msg_num]->To != NoToLine)
	{
		write(reply_fd,", ",2);
		write(reply_fd,sortedList[cur_msg_num]->To,
				strlen(sortedList[cur_msg_num]->To));
	}


	if(sortedList[cur_msg_num]->Subject!=NoSubjectLine)
	{
		write(reply_fd,"\nSubject: ",10);
		if(strncasecmp(sortedList[cur_msg_num]->Subject,"re:",3)!=0)
		{
			write(reply_fd,"Re: ",4);
		}
		write(reply_fd,sortedList[cur_msg_num]->Subject,
				strlen(sortedList[cur_msg_num]->Subject));
	}

	if(sortedList[cur_msg_num]->MessageId!=NoMessageIdLine)
	{
		write(reply_fd,"\nIn-Reply-To: ",14);
		write(reply_fd,sortedList[cur_msg_num]->MessageId,
				strlen(sortedList[cur_msg_num]->MessageId));
	}

	if(asksub && sortedList[cur_msg_num]->Subject==NoSubjectLine)
	{
		write(reply_fd,"\nSubject:  Reply to your message.",33);
	}

	if(all_flag && sortedList[cur_msg_num]->Cc!=NoCcLine)
	{
		write(reply_fd,"\nCc: ",5);
		write(reply_fd,sortedList[cur_msg_num]->Cc,
				strlen(sortedList[cur_msg_num]->Cc));
	}

	if(askcc && ( sortedList[cur_msg_num]->Cc==NoCcLine || !all_flag) )
		write(reply_fd,"\nCc: ",5);


	if(askbcc)
		write(reply_fd,"\nBcc: ",6);

	if((replyto!= (char *)0))
	{
		write(reply_fd,"\nReply-To: ",11);
		write(reply_fd,replyto,strlen(replyto));
	}

	write(reply_fd,"\n",1);

	insert_position = lseek(reply_fd,0, SEEK_CUR); 

	if((count=put_stuff_in(reply_fd,include_flag,sign_flag)) == -1)
	{
		free(reply_file);
		close(reply_fd);
		return;
	}


	insert_position+=count;

	close(reply_fd);

	creat_outbound(reply_file,insert_position,"REPLY");
}

/* ARGSUSED */
void
gripe_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
int reply_fd;
char *reply_file;
char gripe_string[1024];

	reply_file=(char *)malloc(100);

	if((reply_fd=my_mkstemp(reply_file,outbound_suffix))==-1)
	{
		warn("Couldn't create Out Bound message file\n");
		XBell (dpy, 10);
		XFlush(dpy);
		free(reply_file);
		return;
	}


	sprintf(gripe_string,GRIPE_MSG,UNAME_STRING,EDITOR,hold);
	if(write(reply_fd,gripe_string,strlen(gripe_string))==-1)
	{
		perror("write(reply file)");
		warn("Couldn'nt write reply file");
		XBell (dpy, 10);
		XFlush(dpy);
		free(reply_file);
		return;
	}
	if(write(reply_fd,ABOUT_STRING,strlen(ABOUT_STRING))==-1)
	{
		perror("write(reply file)");
		warn("Couldn'nt write reply file");
		XBell (dpy, 10);
		XFlush(dpy);
		free(reply_file);
		return;
	}
	if(do_signature)
		if(append_signature(reply_fd)==-1)
		{
			perror("write(reply file)");
			warn("Couldn't write reply file");
			XBell (dpy, 10);
			XFlush(dpy);
			free(reply_file);
			return;
		}

	close(reply_fd);

	creat_outbound(reply_file,212,"Gripe");
 
}


/* ARGSUSED */
void
compose_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
int reply_fd,sign_flag,include_flag;
char *reply_file,*reply_arg;

	sign_flag=do_signature;
	include_flag=0;

	if(callData!=(caddr_t)0)
	{
		reply_arg=strtok((char *)callData," ,-");
		for(;;)
		{
			if(reply_arg==(char *)0)
				break;

			if(strcmp(reply_arg,"sign")==0)
				sign_flag=1;
			else if(strcmp(reply_arg,"nosign")==0)
				sign_flag=0;
			else if(strcmp(reply_arg,"notsign")==0)
				sign_flag=(!sign_flag);
			else if(strcmp(reply_arg,"include")==0)
				include_flag=1;
			else if(strcmp(reply_arg,"noinclude")==0)
				include_flag=0;
			else if(strcmp(reply_arg,"notinclude")==0)
				include_flag=(!include_flag);
			else
				warn("invalid argument to reply.");

			reply_arg=strtok((char *)0," ,-");
		}
	}
	reply_file=(char *)malloc(100);
	if((reply_fd=my_mkstemp(reply_file,outbound_suffix))==-1)
	{
		warn("Couldn't create Out Bound message file\n");
		XBell (dpy, 10);
		XFlush(dpy);
		free(reply_file);
		return;
	}


	strcpy(tmp_buff,"To: \n");

	if(asksub)
		strcat(tmp_buff,"Subject: \n");
	if(askcc)
		strcat(tmp_buff,"Cc: \n");
	if(askbcc)
		strcat(tmp_buff,"Bcc: \n");

	if(replyto!= (char *)0)
	{
		strcat(tmp_buff,"Reply-To: ");
		strcat(tmp_buff,replyto);
		strcat(tmp_buff,"\n");
	}

	if(realname!= (char *)0) {
	  strcat(tmp_buff,"From: ");
	  strcat(tmp_buff,realname);
	  strcat(tmp_buff,"\n");
	}

	write(reply_fd,tmp_buff,strlen(tmp_buff));

	put_stuff_in(reply_fd,(include_flag &&
				 (Letters_Top != (struct HEADLIST *)0)),
						sign_flag);
	close(reply_fd);

	if(include_flag && 
			((cur_msg_num > 0) && (cur_msg_num < LetterCount)))
		creat_outbound(reply_file,4,"Forward");
	else
		creat_outbound(reply_file,4,"New_Message");

}

/* ARGSUSED */
void
deliver_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
struct proc_msg *parm;

	parm=(struct proc_msg *)closure;
	XawAsciiSave(XawTextGetSource(parm->text));
	if(send_reply(parm->file)==0)
	{
		XtPopdown(parm->box);
		XtDestroyWidget(parm->box);
		unlink(parm->file);
		free(parm->file);

		if(parm->prev!=(struct proc_msg *)0)
		{
			parm->prev->next=parm->next;
		}
		else
		{
			pptr_first=parm->next;
		}

		if(parm->next!=(struct proc_msg *)0)
		{
			parm->next->prev=parm->prev;
		}
		else
		{
			pptr_last=parm->prev;
		}


		free(parm);
		warn("Message has been sent.");
	}
}



/* ARGSUSED */
void
mailrc_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
Arg args[1];
	readMailrcFile((char *)NULL);
	readMailrcFile("~/.xmailtoolrc");
	readMailrcFile(named_mailrc);

#if defined(SYSV) || defined(__alpha)
	getcwd(pwd,MAXPATHLEN);
#else
	getwd(pwd);
#endif
	init_saved_fnames();

	XtSetSensitive(folder_item,(folder==(char *)0?False:True));

	if(state_box)
	{
		XtSetArg(args[0],XtNstate,bodyOnly);
		XtSetValues(bodyOnly_item,args,1);
	
		XtSetArg(args[0],XtNstate,saveCurrent);
		XtSetValues(SaveCurrent_item,args,1);

		XtSetArg(args[0],XtNstate,show_deleted);
		XtSetValues(show_deleted_item,args,1);
	}

	if(strcmp(MAIL,system_mbox_fname)!=0)
	{
		free(system_mbox_fname);
		system_mbox_fname=(char *)malloc(strlen(MAIL)+1);
		strcpy(system_mbox_fname,MAIL);
	}
	
}

/* ARGSUSED */
void
rtsf_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
/*	XtPopdown((Widget)closure);
	XFlush(dpy);
	XtSetSensitive(cmd_box,True); */
	XtSetSensitive(rtsf,False);

	nmail_proc(widget,closure,callData);
}

/* ARGSUSED */
void
folder_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
int change_flag=0,save_flag=0,load_flag;
char *tok_ptr,*ptr;
char buff[200];
struct stat st;
static Arg arglist[]={{XtNlabel,(XtArgVal) 0}};
static Arg farglist[]={{XtNstring,(XtArgVal) file_fname}};
myListReturnStruct *item = (myListReturnStruct *)callData;
char *tmp_fname;
int return_value;
	

	tmp_fname=expand_value(folder,0);

	if(strlen(fdirname)==0)
		sprintf(buff,"%s/%s",tmp_fname,&item->string[1]);
	else
		sprintf(buff,"%s/%s/%s",tmp_fname,fdirname,
				&item->string[1]);

	free(tmp_fname);

	if(debug)
		fprintf(stderr,"buff = %s %d\n",buff,strlen(fdirname));

	if(stat(buff,&st)==-1)
	{
		perror(buff);
	}

	if((st.st_mode&S_IFMT)!=S_IFDIR )
	{

		if(item->data==(caddr_t)0)
		/*
		 * by default, the selection of a file will cause
		 * that file to be loaded.
		 */
			load_flag=1;
		else
		{
			change_flag=0;
			save_flag=0;
			load_flag=0;
			for(tok_ptr=strtok((char *)item->data," ,;.")
				;tok_ptr!=(char *)0;
				tok_ptr=strtok((char *)0," ,;."))
			{
				if(strcasecmp("change",tok_ptr)==0)
				{
					change_flag=1;
				}
				else if(strcasecmp("save",tok_ptr)==0)
				{
					save_flag=1;
				}
				else if(strcasecmp("load",tok_ptr)==0)
				{
					load_flag=1;
				}
				else
				{
					sprintf(buff,"%s is an invalid command",
						tok_ptr);
					warn(buff);
				}
			}

			if(save_flag)
			{
				sprintf(tmp_buff,"Copying %d lines of #%d to %s ...\n",
					sortedList[cur_msg_num]->bodyCount, cur_msg_num+1,
					buff);
				warn(tmp_buff);
				return_value=CopyMessage(sortedList[cur_msg_num],buff,
						1,0,alwaysignore);
				if(return_value!=-1)
				{
					MMSETSAVED(sortedList[cur_msg_num]);
					sprintf(tmp_buff,"Saved %d lines of #%d to %s.\n",
						sortedList[cur_msg_num]->bodyCount,
						cur_msg_num+1,buff);
					warn(tmp_buff);

					if(!keepsave)
						if(load_flag)
						{
							sortedList[cur_msg_num]->modified +=
										 MMDELETE;
							MMSETDEL(sortedList[cur_msg_num]);
						}
						else
						{
							delete_msg(cur_msg_num);
						}
					
				}
			}

			if(load_flag)
			{
				change_mail_file(buff);
				if(LetterCount == 0)
					display_msg((struct HEADLIST *)NULL,False);
				else
				{
					display_msg(sortedList[cur_msg_num],True);
				}
			}

			if(change_flag)
			{
				strcpy(file_fname,buff);
				XtSetValues(file_name_item,farglist,1);
				XFlush(dpy);
			}
		}
	}
	else
	{
		if((strcmp("../",&item->string[1])==0) &&
					 (strlen(fdirname)!=0))
		{
			ptr=strrchr(fdirname,'/');
			if(ptr==(char*)0)
				fdirname[0]=(char)0;
			else
				*ptr=(char)0;
		}
		else
		{
			item->string[strlen(item->string)-1]=(char)0;
			if(strlen(fdirname)!=0)
				strcat(fdirname,"/");
			strcat(fdirname,&item->string[1]);
		}

		tmp_fname=expand_value(folder,0);

		if(strlen(fdirname)==0)
			strcpy(buff,tmp_fname);
		else
			sprintf(buff,"%s/%s",tmp_fname,fdirname);

		free(tmp_fname);

		warn("Building Folder list");
		get_dir(buff,(strlen(fdirname)!=0?1:0));
		warn("Done Building Folder list");
		arglist[0].value=(XtArgVal) buff;
		XtSetValues(list_folder,arglist,1);
		XawListChange(lst_list,fnames,0,0,True);
		XFlush(dpy);
	}
}

/* ARGSUSED */
void
saved_fn_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
static Arg arglist[]={{XtNstring,(XtArgVal) file_fname}};
static Arg farglist[]={{XtNstring,(XtArgVal) file_fname}};
myListReturnStruct *item = (myListReturnStruct *)callData;
int change_flag=0,save_flag=0,load_flag;
char *tok_ptr,*ptr;
char buff[200];
char *tmp_fname;
int return_value;

	if(num_saved_fnames==0)
		return;

	if(widget!=saved_list)
	{
		if((callData==(caddr_t)0)  || (strcmp(callData,"circulate")==0))
		{
			current_sf_index=(current_sf_index+1) % num_saved_fnames;
			if(saved_fnames[current_sf_index]==(char *)0)
				current_sf_index=0;
			strcpy(file_fname,saved_fnames[current_sf_index]);
			XtSetValues(file_name_item,arglist,1);
			XFlush(dpy);
		}
		else if (strcmp(callData, "menu")==0)
		{
			XawListChange(saved_list,saved_fnames,0,0,True);
			XtPopup(saved_menu,XtGrabNone);
			(void) XSetWMProtocols(dpy,XtWindow(saved_menu),
					&wm_delete_window,1);
			XSetTransientForHint(dpy,
				XtWindow(saved_menu),XtWindow(toplevel));

		}
		else
		{
			warn("Unknown command\n");
		}
	}
	else
	{
		change_flag=0;
		save_flag=0;
		load_flag=0;
		for(tok_ptr=strtok((char *)item->data," ,;.")
			;tok_ptr!=(char *)0;
			tok_ptr=strtok((char *)0," ,;."))
		{
			if(strcasecmp("change",tok_ptr)==0)
			{
				change_flag=1;
			}
			else if(strcasecmp("save",tok_ptr)==0)
			{
				save_flag=1;
			}
			else if(strcasecmp("load",tok_ptr)==0)
			{
				load_flag=1;
			}
			else
			{
				sprintf(buff,"%s is an invalid command",
					tok_ptr);
				warn(buff);
			}
		}

		tmp_fname=expand_value(item->string,1);

		if(save_flag)
		{
			sprintf(tmp_buff,"Copying %d lines of #%d to %s ...\n",
					sortedList[cur_msg_num]->bodyCount,
					cur_msg_num+1, item->string);
			warn(tmp_buff);
			return_value=CopyMessage(sortedList[cur_msg_num],
						tmp_fname,1,0,alwaysignore);
 			if(return_value!=-1) 
			{
				MMSETSAVED(sortedList[cur_msg_num]);
				sprintf(tmp_buff,"Saved %d lines of #%d to %s.\n",
					sortedList[cur_msg_num]->bodyCount,
					cur_msg_num+1,item->string);
				warn(tmp_buff);

				if(!keepsave)
					if(load_flag)
					{
						sortedList[cur_msg_num]->modified +=
										 MMDELETE;
						MMSETDEL(sortedList[cur_msg_num]);
					}
					else
					{
						delete_msg(cur_msg_num);
					}
			}
		}

		if(load_flag)
		{
			change_mail_file(tmp_fname);
			if(LetterCount == 0)
				display_msg((struct HEADLIST *)NULL,False);
			else
			{
				display_msg(sortedList[cur_msg_num],True);
			}
		}

		if(change_flag)
		{
			strcpy(file_fname,item->string);
			XtSetValues(file_name_item,farglist,1);
			XFlush(dpy);
		}
		free(tmp_fname);

	}
}


/* ARGSUSED */
void
cdfile_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{
char *tmp_fname,message[1024];

	if((callData==(caddr_t)0) || (strcmp(callData,"file")==0))
	{
		tmp_fname=expand_value(file_fname,1);
		change_mail_file(tmp_fname);
		if(LetterCount == 0)
		{
			display_msg((struct HEADLIST *)NULL,FALSE);
		}
		else
		{
			display_msg(sortedList[0],True);
			add_saved_file(file_fname);
		}
	}
	else  if(strcmp(callData,"cd")==0)
	{
		tmp_fname=expand_value(pwd,1);
			if(chdir(tmp_fname)==0)
			{
#if defined(SYSV) || defined(__alpha)
				getcwd(pwd,MAXPATHLEN);
#else
				getwd(pwd);
#endif
				refresh_dir(pwd);
				sprintf(message,"Current directory is now %s\n",tmp_fname);
			}
			else
			{
				sprintf(message,"Couldn't chdir() to %s: %s",
						tmp_fname,strerror(errno));
			}
			warn(message);
	}
	else
	{
		warn("Unknown Command");
		return;
	}
	free(tmp_fname);
	

}

/* ARGSUSED */
void
mime_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{

	if(!VALIDMSG(cur_msg_num))
	{
		warn("no messages in buffer\n");
		return;
	}

	warn("Sending message to external MIME processor (metamail).");

	if(fork()==0)
	{
		close(0);
		close(1);
		close(2);
		open("/dev/null",O_RDWR);
		dup(0);
		dup(0);
		execlp("xterm","xterm", "-e","metamail","-q",
				msg_fname,0);
		perror("metamail");
	}


}


/* ARGSUSED */
void
quit_proc(widget,closure,callData)
    Widget widget;
    caddr_t closure;
    caddr_t callData;
{	
Widget oldsrc;
XawTextBlock text;
int dposition_tmp;
static Arg arglist[]={{XtNdisplayPosition,(XtArgVal) 0}};

	if((callData==(caddr_t)0) || (strcmp(callData,"quit")==0))
	{

	chang_cursor(toplevel,watch_cur);

	if(Letters_Top!=(struct HEADLIST *)0)
	{
		if(VALIDMSG(cur_msg_num))
		{
			arglist[0].value=(XtArgVal)&dposition_tmp;
			XtGetValues(msg_box,arglist,1);
			if(bodyOnly)
			{
				sortedList[cur_msg_num]->display_position =
					dposition_tmp+2;
			}
			else
			{
				oldsrc = XawTextGetSource(msg_box);
				text.firstPos=0;
				text.length=2;
				text.ptr="\n\n";
				sortedList[cur_msg_num]->display_position =
					dposition_tmp -
					XawTextSourceSearch(oldsrc,1,XawsdRight,&text) +
					1;
				if(sortedList[cur_msg_num]->display_position < 0)
					sortedList[cur_msg_num]->display_position=0;
			}
		}

		warn("Quitting... Saving current messages.  -- Please Wait --\n");
		if(SaveMB(cur_mbox_fname)==-1)
			sleep(10);
	}
	}
	else  if(strcmp(callData,"abort")==0)
	{
		warn("Aborting...");
	}
	else
	{
		warn("Unknown Command");
		return;
	}
	clean_up();
	exit(0);
}


