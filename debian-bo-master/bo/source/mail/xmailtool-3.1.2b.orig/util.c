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

static char util_rcsid[]="$Id: util.c,v 1.47 1994/11/03 22:45:35 bobo Exp $";

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Shell.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <fcntl.h>
#include <string.h>
#include <pwd.h>

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#endif


#ifdef DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

#include "defs.h"

extern Widget header_box,folder_name_item,cmd_box,msg_box,saved_list,
		mail_msg,state_cmd_box,help_msg_item,dir_name_item;

extern char *savedFnames, *HOME, *TMPDIR;

/* extern void (*dump_core)(); */

dump_core()
{
	kill(getpid(),11);
}
extern int errno,debug;
extern char EOT_FLAG[];
extern char **saved_fnames,*folder;
extern int num_saved_fnames;
static int saved_num_saved_fnames=0;

char *fnames[MAXFILES];
int nfnames=0;
char fdirname[200]="";

void my_free();
char *my_malloc(), *my_realloc();

void
init_saved_fnames()
{
int sf_index;
extern char *MBOX;
char *thisFname;
	
	if((saved_num_saved_fnames==0) && (num_saved_fnames ==0))
		return;

	for(sf_index=1;sf_index < saved_num_saved_fnames;sf_index++)
	{
		if(saved_fnames[sf_index]==(char *)0)
			break;

		free(saved_fnames[sf_index]);
	}

	if(saved_fnames != (char **)0)
		free((char *)saved_fnames);

	saved_fnames=(char **)malloc((num_saved_fnames +1) * sizeof(char *));
	
	if(savedFnames!=(char *)0)
		thisFname=strtok(savedFnames," ,");

	for(sf_index=1;sf_index < num_saved_fnames +1;sf_index++)
	{
		if((savedFnames==(char *)0) || (thisFname==(char *)0))
			saved_fnames[sf_index]=(char *)0;
		else
		{
			saved_fnames[sf_index]=(char *)malloc(
							strlen(thisFname)+1);
			strcpy(saved_fnames[sf_index],thisFname);
			thisFname=strtok(NULL," ,");
		}
	}

	saved_fnames[0]=MBOX;
	saved_num_saved_fnames=num_saved_fnames;
}


void
shift_saved_fnames()
{
int sf_index;

	if(saved_fnames[num_saved_fnames-1]!=(char *)0)
		free(saved_fnames[num_saved_fnames-1]);

	for(sf_index=0;sf_index<num_saved_fnames - 2;sf_index++)
	{
		saved_fnames[num_saved_fnames - 1 - sf_index] =
			saved_fnames[num_saved_fnames - 2 - sf_index];
	}
	saved_fnames[1] = (char *)0;
}

void
add_saved_file(n)
char *n;
{
extern char dbox_fname[];
int sf_index;

	if(num_saved_fnames==0)
		return;

	if((strcmp(n,"%")==0) || (strcmp(n,dbox_fname)==0))
		return;

	for(sf_index=0;sf_index<num_saved_fnames;sf_index++)
	{
		if((saved_fnames[sf_index]==(char *)0) ||
			(strcmp(n,saved_fnames[sf_index])==0))
			break;
	}

	/*
	 * at this point we have one of three conditions.
	 * 1)  There is a match in file names
	 * 2)  There is an empty space
	 * 3)  No match and no empty space
	 */
	if((sf_index>=num_saved_fnames) ||
		(saved_fnames[sf_index]==(char *)0))
	{
		shift_saved_fnames();
		saved_fnames[1]=(char *)malloc(strlen(n)+1);
		strcpy(saved_fnames[1],n);
		XawListChange(saved_list,saved_fnames,0,0,True);
	}

}

/*
 * This function will expand file names containing
 * references to environment variables ($VARIABLE)
 * tilde (~[username]) and the plus sign (+) for folders.
 * The value returned is malloced it is up to the caller
 * to free unneeded space.
 */
char *
expand_fname(fname)
char *fname;
{
char *exp_fname; 
char *variable_name,*env_value;
int variable_len;
struct passwd *my_pw_ent;

if((fname[0]=='+') && (folder != (char *)0))
{
	exp_fname=(char *)malloc(strlen(folder) + strlen(fname) +2);	
	sprintf(exp_fname,"%s/%s",folder,&fname[1]);
	return(exp_fname);
}

if(fname[0]=='$')
{
	variable_len=strcspn(fname,"/");
	variable_name=(char *)malloc(variable_len+1);
#ifdef SYSV
	memset(variable_name,0,variable_len+1);
	memcpy(variable_name,fname,variable_len);
#else
	bzero(variable_name,variable_len+1);
	bcopy(fname,variable_name,variable_len);
#endif
	if((env_value=getenv(&variable_name[1]))==(char *)0)
	{
		env_value=variable_name;
	}
	else
		free(variable_name);
	exp_fname=(char *)malloc(strlen(env_value) + strlen(fname) +2);
	sprintf(exp_fname,"%s/%s",env_value,&fname[variable_len]);
	return(exp_fname);
}

if(fname[0]=='~')
{
	variable_len=strcspn(fname,"/");
	if(variable_len>1)
	{
		variable_name=(char *)malloc(variable_len+1);
#ifdef SYSV
		memset(variable_name,0,variable_len+1);
	        memcpy(variable_name,fname,variable_len);
#else
		bzero(variable_name,variable_len+1);
		bcopy(fname,variable_name,variable_len);
#endif
		if((my_pw_ent=getpwnam(&variable_name[1]))==(struct passwd *)0)
		{
			env_value=variable_name;
		}
		else
		{
			env_value=my_pw_ent->pw_dir;
		}
		free(variable_name);
	}
	else
	{
		env_value=HOME;
	}
	exp_fname=(char *)malloc(strlen(env_value) + strlen(fname) +2);
	sprintf(exp_fname,"%s/%s",env_value,&fname[variable_len]);
	return(exp_fname);
}

exp_fname=(char *)malloc(strlen(fname)+1);
strcpy(exp_fname,fname);
return(exp_fname);
}
	
	


int
my_cmp(s1,s2)
char **s1,**s2;
{


	return(strcmp(&(*s1)[1],&(*s2)[1]));
}

int
get_dir(name,dodots)
char name[];		/* specifies the full path of the directory	*/
int dodots;
{
struct stat st;
int dir_index;
char full_name[200];

DIR *dp;
#ifdef DIRENT
struct dirent *mydir;
#else
struct direct *mydir;
#endif

if(debug)
	fprintf(stderr,"get_dir(name=%s,dodots=%d)\n",name,dodots);

	for(dir_index=0;dir_index<nfnames;dir_index++)
	{
		if(fnames[dir_index] != (char *)0)
		{
			free(fnames[dir_index]);
			fnames[dir_index]=(char *)0;
		}
	}

	if((dp=opendir(name))==(DIR *)0)
	{
		perror(name);
		return(-1);
	}

	for(mydir = readdir(dp),dir_index=0;
#ifdef DIRENT
		(mydir != (struct dirent *)0)
#else
		(mydir != (struct direct *)0)
#endif
			&& (dir_index<MAXFILES) ;
			mydir = readdir(dp))
	{
		if((strcmp(mydir->d_name,".")!=0) &&
			((strcmp(mydir->d_name,"..")!=0) || dodots))
		{

			fnames[dir_index] =
				(char *)malloc(strlen(mydir->d_name) + 4);
			fnames[dir_index+1]=(char *)0;
			strcpy(full_name,name);
			strcat(full_name,"/");
			strcat(full_name,mydir->d_name);
			if(stat(full_name,&st)==-1)
			{
				perror(fnames[dir_index]);
			}
			else
			{
				sprintf(fnames[dir_index],"%c%s%s",
					(st.st_mtime>st.st_atime?'*':' '),
					mydir->d_name,
					((st.st_mode&S_IFMT)==S_IFDIR?"/":""));
			if(debug)
				fprintf(stderr,"dir%d = %s\n",dir_index,
						fnames[dir_index]);
			}
			dir_index++;
		}
	}
	nfnames=dir_index;

	qsort(fnames,dir_index,sizeof(char *),
#ifdef __STDC__
		(int(*)(const void*, const void*))my_cmp);
#else
		my_cmp);
#endif

	closedir(dp);

	return(0);
}

Boolean
InParams(str, p, n)
String str;
String *p;
Cardinal n;
{
int i;
	for (i=0; i < n; p++, i++)
		if (! XmuCompareISOLatin1(*p, str))
			return True;
	return False;
}

Widget
make_cancel(parent,p)
Widget parent,p;
{
Widget cancel_item;
extern cancel_proc();

cancel_item=XtCreateManagedWidget("cancel",commandWidgetClass,
		parent,NULL,0);
	XtAddCallback(cancel_item,XtNcallback,(XtCallbackProc)cancel_proc,
			(XtPointer)p);
	return(cancel_item);
}

set_text_columns(w,n)
Widget w;
int n;
{
static XFontStruct *f;

	static Arg arglist[]={{XtNfont,(XtArgVal) &f},
			{XtNwidth,(XtArgVal) 0}};

	XtGetValues(w,arglist,1);

	arglist[1].value =(XtArgVal)( n * f->max_bounds.width) + 20;

	XtSetValues(w,&arglist[1],1);
}

XMT_InstallAccelerators(w)
Widget w;
{
extern int state_box,editable_msg;
extern int auto_help,do_xmtaccelerators;

	if(!do_xmtaccelerators)
		return;

	XtInstallAllAccelerators(header_box,w);
	XtInstallAllAccelerators(mail_msg,w);
	if(!editable_msg)
		XtInstallAllAccelerators(msg_box,w);
	XtInstallAllAccelerators(cmd_box,w);
	XtInstallAllAccelerators(folder_name_item,w);
	if(state_box)
		XtInstallAllAccelerators(state_cmd_box,w);
	if(auto_help)
		XtInstallAllAccelerators(help_msg_item,w);
}

Widget
command_button(name,parent,callback,clientData)
char *name;
Widget parent;
XtCallbackProc callback;
caddr_t clientData;
{
Widget tmp_widget;

	tmp_widget=XtCreateManagedWidget(name,commandWidgetClass,
				parent,NULL,0);
	XtAddCallback(tmp_widget,XtNcallback,callback,clientData);
	return(tmp_widget);
}


void
picture_button(w,p)
Widget w;
Pixmap p;
{
static Arg arglist[]={
        {XtNbitmap,(XtArgVal)0}};

        arglist[0].value = (XtArgVal)p;

        XtSetValues(w,arglist,1);
}

char *tmp_expand_buff=(char *)0;

char *
expand_string(str)
char *str;
{
int str_index,new_index;

	if(tmp_expand_buff!=(char *)0)
	{
		free(tmp_expand_buff);
	}

	tmp_expand_buff=(char *)malloc(strlen(str));

	for(str_index=0,new_index=0;str_index<strlen(str);
				str_index++,new_index++)
	{
		if(str[str_index]=='\\')
		{
			str_index++;
			switch(str[str_index]) {
				case 'n':
					tmp_expand_buff[new_index]='\n';
					break;
				case 'r':
					tmp_expand_buff[new_index]='\r';
					break;
				case 't':
					tmp_expand_buff[new_index]='\t';
					break;
				default:
					tmp_expand_buff[new_index]=
							str[str_index];
			}
		}
		else
		{
			tmp_expand_buff[new_index]=
						str[str_index];
		}
	}
	tmp_expand_buff[new_index]=(char)0;
	return(tmp_expand_buff);
}

int
read_header_text(source,start,buff,len)
Widget source;
int start,len;
char *buff;
{
XawTextBlock text;
int buff_index;

for(buff_index=0;buff_index < len ; buff_index+=text.length)
{
	XawTextSourceRead(source,start+buff_index,
			&text,len-buff_index);
	if(text.length == 0)
		return(0);

#ifdef SYSV
	memcpy(&buff[buff_index],text.ptr,text.length);
#else
	bcopy(text.ptr,&buff[buff_index],text.length);
#endif

}
return(buff_index);
}	

int
readln(fd,buff,nbytes)
int fd,nbytes;
char buff[];
{
int nread=0,indx;
char c;

	for(indx=0;indx<(nbytes-1);indx++)
	{
		if((nread=read(fd,&buff[indx],1))<0)
		{
			if(errno == EINTR)
			{
				indx--;
				continue;
			}
			perror("readln");
			return(-1);
		}

		if(nread==0)
			break;

		if(buff[indx]=='\n')
			break;
	}

	if(nread==0)
	{
		buff[indx]=0;
		return(indx);
	}
	else if((indx>=(nbytes-1))&&(buff[indx]!='\n'))
	{
		/* read until we see end of line */
		for(;;)
		{
			if((nread=read(fd,&c,1))<1)
			{
				if(errno == EINTR)
				{
					continue;
				}
				perror("readln");
				buff[indx]=0;
				return(-1);
			}

			if(nread==0)
				break;

			if(c=='\n')
				break;
		}
	}

	buff[indx]=0;
	return(indx+1);
}


int
get_line(fd,buff,nbytes)
int fd,nbytes;
char *buff;
{
char tmp[200];
int nread;

	if((nread=readln(fd,tmp,200))==-1)
	{
		perror("Can't Get line");
		return(-1);
	}

	if(nread==0)
	{
		warn("End of file on Mail Pipe");
		return(-1);
	}

	if(strcmp(tmp,EOT_FLAG)==0)
	{
		buff[0]='\00';
		return(0);
	}
	else
	{
		strncpy(buff,tmp,nbytes);
		return(strlen(tmp));
	}
}

int
read_rest(fd)
{
char tmp[200];
int nread;

	for(;;)
	{
		if((nread=readln(fd,tmp,200))==-1)
		{
			perror("Can't Read rest");
			return(-1);
		}

		if(nread==0)
		{
			warn("End of file on Mail Pipe");
			return(-1);
		}


		if(strcmp(tmp,EOT_FLAG)==0)
			break;
	}
	return(0);
}

int
include_envelope(fd)
int fd;
{
char env_fname[50];
char tmp_buff[1024];
int env_fd,nread,count=0;

	sprintf(env_fname,"%s/.envelope",HOME);
	if(access(env_fname,4) != -1)
	{
		if((env_fd=open(env_fname,O_RDONLY))==-1)
		{
			perror("open(envelope file)");
			return(-1);
		}

		for(;;)
		{
			if((nread=read(env_fd,tmp_buff,1024))==-1)
			{
				perror("read(envelope file)");
				close(env_fd);
				return(-1);
			}

			if(nread==0)
			{
				close(env_fd);
				return(count);
			}

			write(fd,tmp_buff,nread);
			count+=nread;
		}
	}
	return(0);
}

int
append_signature(fd)
int fd;
{
char sig_fname[50];
char signature[MAXMESLEN];
int sig_fd,nread=0;

	sprintf(sig_fname,"%s/.signature",HOME);
	if(access(sig_fname,4) != -1)
	{
		if((sig_fd=open(sig_fname,O_RDONLY))==-1)
		{
			perror("read(signature file)");
		}
		else
		{
			nread=read(sig_fd,signature,MAXMESLEN);
			close(sig_fd);
		}
	}
	else
		return(0);
	
	if(write(fd,signature,nread)==-1)
	{
		return(-1);
	}

	return(0);
}

int
get_info(fd,buff,nbytes)
int fd,nbytes;
char *buff;
{
char tmp[200];
int tmp_index;
int tmp_eot,nread;

strcpy(buff,"");
tmp_eot=0;
tmp_index=0;

	for(;;)
	{
		if((nread=readln(fd,tmp,200))==-1)
		{
			perror("Can't Get Info");
			return(-1);
		}

		if(nread==0)
		{
			warn("End of file on Mail Pipe");
			return(-1);
		}

		if(strcmp(tmp,EOT_FLAG)==0)
		{
			tmp_eot=1;
			break;
		}

		if(tmp_index + strlen(tmp) > nbytes)
			break;

		strcat(buff,tmp);
		tmp_index=strlen(buff);
	}

	if(tmp_eot==0)
		read_rest(fd);

	return(strlen(buff));
}

/*
 * this function changes the text in the Directory name
 * window to reflect that actual directory Mail is in.
 * If the name starts with the value of $HOME, we simply
 * insert "~" instead.
 */
void
refresh_dir(p)
char *p;
{
Arg tmp_arg[1];
extern Display *dpy;

	XtSetArg(tmp_arg[0],XtNstring,"");
	XtSetValues(dir_name_item,tmp_arg,1);
	if((strncmp(p,HOME,strlen(HOME))==0))
		sprintf(p,"~%s",&p[strlen(HOME)]);
	XtSetArg(tmp_arg[0],XtNstring,p);
	XtSetValues(dir_name_item,tmp_arg,1);
	XawTextSetInsertionPoint(dir_name_item,strlen(p));

	XFlush(dpy);
}	



#ifdef NEED_TRUNCATE
/*
 * In our case we don't care about the size parameter as we will
 * always truncate the file to zero (0).
 */
/* ARGSUSED */
int
truncate(path,size)
char *path;
int size; /* ignored  and complained about */
{
int tmp_fd;

	if((tmp_fd=open(path,O_CREAT|O_TRUNC|O_WRONLY,0600))==-1)
	{
		perror(path);
		return(-1);
	}

	close(tmp_fd);
	return(0);
}
#endif


int
my_mkstemp(fname,suffix)
char *fname,*suffix;
{
int fd;
char *tmp_fname;
int mypid,fnid;

	tmp_fname=(char*)malloc(strlen(TMPDIR)+15+strlen(suffix));
	mypid=(int)getpid();

	for(fnid=0;fnid<0x100;fnid++)
	{
		sprintf(tmp_fname,"%s/XMT%05d%02x%s",TMPDIR,mypid,fnid,
					suffix);
		fd=open(tmp_fname,O_CREAT|O_RDWR|O_EXCL,0600);
		if(fd!=-1)
			break;
	}

	if(fnid<0x100)
	{
		strcpy(fname,tmp_fname);
	}

	free(tmp_fname);
	return(fd);
}

#ifdef NEED_STRSTR
char *strstr(s1,s2)
char *s1,*s2;
{

int slen;

	slen=strlen(s2);

	if(slen==0)
		return(s1);

	while(strlen(s1)>=slen)
	{
		if(strncmp(s1,s2,slen)==0)
			return(s1);
		s1++;
	}

	return((char *)0);
}
#endif


#ifdef NEED_CASECMP
int
strcasecmp(s1,s2)
register char *s1,*s2;
{
	for(;(toupper(*s1) == toupper(*s2)) &&
		(*s1 != '\0') && (*s2 != '\0');*s1++,*s2++)

	if(toupper(*s1) == toupper(*s2))
			return(0);

	return(*s1 - *s2);
}

#endif


#ifdef NEED_SETENV
int
setenv(s1,s2,o)
char *s1,*s2;
int o;
{
char *tmp_val;

	tmp_val=(char *)malloc(strlen(s1)+strlen(s2)+3);
	sprintf(tmp_val,"%s=%s",s1,s2);

	if(o)
	{
		putenv(tmp_val);
	}
	else
	{
		char *tmp_env=getenv(s1);
		if(tmp_env==(char *)0)
			putenv(tmp_val);
		else
			return(-1);
	}
	return(0);
}
#endif
	

