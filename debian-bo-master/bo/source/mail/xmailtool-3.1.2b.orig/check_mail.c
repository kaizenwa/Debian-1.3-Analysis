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

static char check_mail_rcsid[]="$Id: check_mail.c,v 1.19 1995/05/12 19:20:48 bobo Exp $";

#include <X11/Xlib.h>			/* for Xlib definitions */
#include <stdio.h>
#include <X11/cursorfont.h>		/* for cursor constants */
#include <X11/StringDefs.h>		/* for useful atom names */
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/Shell.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#include <sys/stat.h>
#include <errno.h>

#include <pwd.h>			/* for getting username */

#include "check_mail.h"
#include "defs.h"
#include "mailhandler.h"
#include "newmail.icon"
#include "oldmail.icon"
#include "nomail.icon"

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))


extern FILE *MBFP;
extern struct _app_resources app_resources;
extern Widget mail_msg,icon_bitmap,icon_label,icon_shell,icon_box;
extern int interval,MacX,debug,cur_msg_num,LetterCount,OldMessageCount,showlast,
	showold,shownew,UnreadMessageCount,editable_msg;

extern Widget deliver_item,cancel_item,cd_item,reply_item,done_item,command_button();
extern Widget next_item,del_item,undel_item,print_item;
extern Widget quit_item,abort_item,mailrc_item,aboutsw;
extern Widget save_item,folder_item,nmail_item,comp_item,mail_msg,
		preserve_item,load_item;

static void GetMailFile();
static void check_mailbox();
static void beep();
Boolean is_iconic=True;

char *system_mbox_fname;
static struct stat stb;
struct stat saved_stat; /* global values */
extern struct stat OriginalSysMBStat, CurrentSysMBStat;
int mail_state;
Pixmap mbox_pix[3];
extern Widget toplevel;
extern XtAppContext my_app_con;
extern char *expand_fname(),*SHELL, *MAIL;
extern Display *dpy;
extern struct HEADLIST **sortedList,*Letters_Top,*current_msg;


/* ARGSUSED */
static XtTimerCallbackProc
#if NeedFunctionPrototypes
mail_timeout(caddr_t c, XtIntervalId *i)
#else
mail_timeout(c,i)
caddr_t c;
XtIntervalId *i;
#endif
{

	XtAppAddTimeOut(my_app_con,(long)1000 * interval,
		(XtTimerCallbackProc)mail_timeout,
		(XtPointer)0);
	check_mailbox();

/*	check_folder(); */

	return(NULL);
	
}

static void GetMailFile();

void
update_mbox_icon()
{
	Arg arg[1];
	if(toplevel==0)
		return;

	if(debug)
		fprintf(stderr,"xx mail_state = %d\n",mail_state);

	if(MacX)
	{
		XtSetArg(arg[0],XtNiconPixmap,mbox_pix[mail_state]);
		XtSetValues(toplevel,arg,1); 

	}
	XtSetArg(arg[0],XtNbitmap,mbox_pix[mail_state]);

	if(icon_bitmap!=0)
		XtSetValues(icon_bitmap,arg,1);
	XFlush(XtDisplay(toplevel));
}

void
init_mbox(dpy)
Display *dpy;
{
	int ret;
	/*
	 * find mailbox
	 */
	GetMailFile();
	ret=stat(system_mbox_fname, &saved_stat);
	if(ret==-1 & debug )
		perror(system_mbox_fname);
#ifdef SYSV
	memcpy(&OriginalSysMBStat,&saved_stat,
#else
	bcopy((char *)&saved_stat,(char *)&OriginalSysMBStat,
#endif
		sizeof(struct stat));

#ifdef SYSV
	memcpy(&CurrentSysMBStat,&saved_stat,
#else
	bcopy((char *)&saved_stat,(char *)&CurrentSysMBStat,
#endif
		sizeof(struct stat));


	if( ret == -1 )
	{
		mail_state = NO_MAIL;
	}
	else if(saved_stat.st_size == 0)
		mail_state = NO_MAIL;
	else if(saved_stat.st_mtime >= saved_stat.st_atime)
		mail_state = NEW_MAIL;
	else
		mail_state = OLD_MAIL;


	/*
	 * Create pixmaps for Icon
	 */
	mbox_pix[NEW_MAIL] = XCreateBitmapFromData(dpy,DefaultRootWindow(dpy),(char *)newmail_bits,
				newmail_width,newmail_height);
	mbox_pix[OLD_MAIL] = XCreateBitmapFromData(dpy,DefaultRootWindow(dpy),(char *)oldmail_bits,
				oldmail_width,oldmail_height);
	mbox_pix[NO_MAIL] = XCreateBitmapFromData(dpy,DefaultRootWindow(dpy),(char *)nomail_bits,
				nomail_width,nomail_height);
	XFlush(dpy);

	update_mbox_icon();

	/*
	 * setup timer routine
	 */
	XtAppAddTimeOut(my_app_con,(long)1000 * interval,
		(XtTimerCallbackProc)mail_timeout,
		(XtPointer)0); 
}



static void 
check_mailbox ()
{
	extern warn();
	int	CurrentState;
	int ret;
	char tmp_file_name[250];
	struct stat tmp_stat;
	char message[1024];

	if((mail_msg!=0) && debug)
		warn("Stating system mailbox file.\n");

	ret=stat(system_mbox_fname, &stb);
	if((ret == -1 ) && debug)
	{
		sprintf(message,"%s:  %s\n",system_mbox_fname,strerror(errno));	
		warn(message);
	}

	if((mail_msg!=0) && debug)
		warn("Stated system mailbox file.\n");

	if( ret == -1 )
	{
		CurrentState = NO_MAIL;
	}
	else if(stb.st_mtime != CurrentSysMBStat.st_mtime)
	{
		if(debug)
			fprintf(stderr,"mtime has changed is_iconic = %d\n",
				is_iconic);
		/*
		 * if the tool is in the iconic state, we want
		 * to update the message list.  Otherwise,
		 * just tell the user.
		 */
		if(is_iconic)
		{
			/*
			 * there are two possibilities:
			 *	1) new mail arived
			 *	2) another session has modified the mail file.
			 */
			sprintf(tmp_file_name,"%s.lock",system_mbox_fname);

			if(stat(tmp_file_name,&tmp_stat)!=-1)
			{
				/*
				 * try again latter.
				 */
				return;
			}

			if(stb.st_size > CurrentSysMBStat.st_size)
			{
				/*
				 * there are probably new messages.
				 */
				if(MBFP!=NULL)
					UpdateSysMB(system_mbox_fname);
				else
					load_mail_file(system_mbox_fname);
			}
			else
			{
				/*
				 * another session has probably modified
				 * the mailbox.
				 */
				(void)load_mail_file(system_mbox_fname);
			}


			if(Letters_Top==(struct HEADLIST *)0)
			{
				XtSetSensitive(next_item,False);
				XtSetSensitive(load_item,False);
				XtSetSensitive(del_item,False);
				XtSetSensitive(reply_item,False);
				XtSetSensitive(save_item,False);
				XtSetSensitive(print_item,False);
				XtSetSensitive(undel_item,False);
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

				current_msg=(struct HEADLIST *)0;
				if(showold)
				{
					for(cur_msg_num=0;
					(cur_msg_num<LetterCount) &&
					(ISMMREAD(sortedList[cur_msg_num]));
					cur_msg_num++);
					if((cur_msg_num < LetterCount) &&
						(!ISMMREAD(sortedList[cur_msg_num])))
						current_msg=sortedList[cur_msg_num];
				}
				else if(shownew)
				{
					for(cur_msg_num=LetterCount-1;
					(cur_msg_num>=0) &&
					(ISMMREAD(sortedList[cur_msg_num]));
					cur_msg_num--);
					if((cur_msg_num>=0) &&
						(!ISMMREAD(sortedList[cur_msg_num])))
						current_msg=sortedList[cur_msg_num];
				}

				if((current_msg==(struct HEADLIST *)0) && showlast)
				{
					cur_msg_num=LetterCount-1;
					current_msg=sortedList[cur_msg_num];
				}

				if(current_msg==(struct HEADLIST *)0)
				{
					cur_msg_num=0;
					current_msg=sortedList[0];
				}
			}

			refresh_headers(0);

			sprintf(message,"%s: %d messages, %d new, %d unread\n",
					system_mbox_fname, LetterCount,
					LetterCount-OldMessageCount,
					UnreadMessageCount);
			warn(message);

			if(LetterCount==0)
				CurrentState=NO_MAIL;
			else if(LetterCount>OldMessageCount)
				CurrentState=NEW_MAIL;
			else
				CurrentState=OLD_MAIL;
		}
		else
		{
			CurrentState = NEW_MAIL;
		}
	}
	else
	{
		/*
		 * if the modification time hasn't changed it doesn't appear
		 * to me that we need to do anything.
		 */
		if(debug)
			fprintf(stderr,"no change in stat\n");
		return;
	}
		
#ifdef SYSV
	memcpy(&CurrentSysMBStat,&stb,
#else
	bcopy((char *)&stb,(char *)&CurrentSysMBStat,
#endif
		sizeof(struct stat));

	if(debug)
		fprintf(stderr,"mail_stat = %d, CurrentState = %d\n",mail_state,CurrentState);

	if(CurrentState == NEW_MAIL)
	{
		warn("You have 'New Mail' in your system mail box.");
		if(app_resources.newMailCmd != NULL)
		{
			if(fork()==0)
			{
				execlp(SHELL,SHELL,"-c",
					app_resources.newMailCmd,0L);
			}	
		}
		else
		{
			beep(toplevel);
		}
      	}

	mail_state = CurrentState;
	update_mbox_icon();

    return;
} /* end CheckMail() */


/*
 * get user name for building mailbox
 */

static void GetMailFile ()
{
	extern char *resource_mbox_fname;

	system_mbox_fname = (String)malloc(strlen(MAIL) + 1);
	strcpy (system_mbox_fname, MAIL);
	setenv("MAIL",system_mbox_fname,1);
    return;
}



/* ARGSUSED */
static void beep (w)
    Widget w;
{
extern Display *dpy;
extern int bell;
int bell_count;

	for(bell_count=0; bell_count < bell ; bell_count ++)
	{
  		XBell (dpy, 10);
		XFlush(dpy);
	}

    return;
}



/* ARGSUSED */
XtEventHandler
#if NeedFunctionPrototypes
map_handler(Widget w,caddr_t c,XEvent *e)
#else
map_handler(w,c,e)
Widget w;
caddr_t c;
XEvent *e;
#endif
{
extern show_tool();
extern park_mail();
int x,y;
unsigned int width,height,bw,d;
Window mr;
Arg arg[3];

if(debug)
{
fprintf(stderr,"map_handler() ");
if(w==icon_shell)
	fprintf(stderr,"icon ");
else
	fprintf(stderr,"shel ");

switch(e->type) {
	case  MapNotify:
		fprintf(stderr,"MapNotify\n");
		break;
	case UnmapNotify:
		fprintf(stderr,"UnmapNotif\n");
		break;
	case ReparentNotify:
		fprintf(stderr,"ReparentNotify\n");
		break;
	default:
		fprintf(stderr,"unknown type %d\n",e->type);
}
}
	switch(e->type) {
	case MapNotify:
		if(w==icon_shell)
		{
			if(icon_label==(Widget)0)
			{
				XtSetArg(arg[0],XtNlabel,toplevel->core.name);
				icon_label = XtCreateManagedWidget("iconLabel",
					labelWidgetClass, icon_box, arg, 1);
			}
/*			check_mailbox();
			update_mbox_icon();
*/
			break;
		}

		/*
		 * if it's already a tool, we ignore
		 * multiple MapNotifies.
		 */
		if(!is_iconic)
			break;
		if(MacX)
		{
			XtPopdown(icon_shell);
		}

		/*
		 * going from icon to tool.
		 */
		show_tool();
		is_iconic=False;
		break;
	case UnmapNotify:
		if(w==icon_shell)
		{
			if(MacX)
			{
				XMapWindow(dpy,XtWindow(toplevel));
				XtPopdown(icon_shell);
			}
			break;
		}

		/*
		 * if it's already an Icon, we ignore
		 * multiple UnmapNotifies.
		 */
		if(is_iconic)
			break;
		/*
		 * going from tool to icon.
		 */
		if(MacX)
		{
			XtPopup(icon_shell,XtGrabNone);
			XWithdrawWindow(dpy,XtWindow(toplevel),0);
		}

		/*
		 * These next two lines are here because park_mail()
		 * can take a long time.  If the state is really
		 * NEW_MAIL before parking, but should be OLD_MAIL
		 * after parking, the user may see the new mail
		 * flag.  It is better to pretend that all the
		 * mail is OLD for now and determine if it's new
		 * latter.
		 */
		mail_state=OLD_MAIL;
		update_mbox_icon();

		park_mail();
		if(LetterCount==0)
			mail_state=NO_MAIL;
		else if(LetterCount>OldMessageCount)
			mail_state=NEW_MAIL;
		else
			mail_state=OLD_MAIL;
		if(debug)
			fprintf(stderr,"parked mail  doing update\n");
		update_mbox_icon();
		is_iconic=True;
		break;
	case ReparentNotify:
		if(w!=icon_shell)
			break;

		icon_label=(Widget)-1;

		if(e->xreparent.parent==DefaultRootWindow(dpy))
			break;

		if(e->xreparent.x>e->xreparent.y)
		{
			XGetGeometry(dpy,e->xreparent.window,&mr,&x,&y,&width,
						&height,&bw,&d);
/* fprintf(stderr,"x,y = %d,%d  w,h = %d,%d */
			XtSetArg(arg[0],XtNwidth,
			width+2*(e->xreparent.x-e->xreparent.y)+1);
			XtSetArg(arg[1],XtNx,e->xreparent.y);
			XtSetArg(arg[2],XtNy,e->xreparent.y);
			XtSetValues(icon_shell,arg,3);
			XFlush(XtDisplay(toplevel));
		}
		break;
	default:
		/*
		 * must be some other event...
		 * we don't care about them.
		 */
		break;
	}

	return(0);
}
