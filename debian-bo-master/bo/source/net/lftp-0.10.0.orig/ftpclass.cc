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

#include "ftpclass.h"
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>

#include "xmalloc.h"

#ifdef HAVE_ARPA_FTP_H
#include <arpa/ftp.h>
#else
enum {TYPE_A,TYPE_I};
#endif
#include <errno.h>
#include <ctype.h>
#include <time.h>
#ifdef TM_IN_SYS_TIME
#include <sys/time.h>
#endif
#include <assert.h>
#include <pwd.h>

#include "xalloca.h"

static int  one=1;

static int NotSerious(int e)
{
   switch(e)
   {
#ifdef ECONNRESET
   case(ECONNRESET):
#endif
   case(ECONNREFUSED):
#ifdef EHOSTUNREACH
   case(EHOSTUNREACH):
#endif
#ifdef EHOSTDOWN
   case(EHOSTDOWN):
#endif
#ifdef ENETRESET
   case(ENETRESET):
#endif
#ifdef ENETUNREACH
   case(ENETUNREACH):
#endif
#ifdef ENETDOWN
   case(ENETDOWN):
#endif
#ifdef ECONNABORTED
   case(ECONNABORTED):
#endif
      return 1;
   }
   return 0;
}

/* Procedures for checking for a special answers */
int   Ftp::NotImplementedCheck(int act,int exp)
{
   if(act==RESP_NOT_IMPLEMENTED || act==RESP_NOT_UNDERSTOOD || act==504)
   {
      if(exp==RESP_REST_OK && !(flags&NOREST_MODE))
      {
	 DebugPrint("---- ","Switching to NOREST mode");
	 flags|=NOREST_MODE;
	 if(!(flags&IO_FLAG) && mode!=STORE)
	 {
	    real_pos=0;
	    return(state);
	 }
      }
      return(FATAL_STATE);
   }
   return(-1);
}

int   Ftp::NoFileCheck(int act,int exp)
{
   (void)exp;
   if(act==RESP_NOT_IMPLEMENTED || act==RESP_NOT_UNDERSTOOD)
      return(FATAL_STATE);
   if(act/100==5)
   {
      if(strstr(line,"Broken pipe"))
	 return INITIAL_STATE;
      return(NO_FILE_STATE);
   }
   return(-1);
}

int   Ftp::TransferCheck(int act,int exp)
{
   (void)exp;
   if(act==RESP_NO_FILE && mode==LIST)
      return(NO_FILE_STATE);
   if(act==RESP_BROKEN_PIPE)
   {
      if(data_sock==-1 && strstr(line,"Broken pipe"))
   	 return(state);
   }
   return(NoFileCheck(act,exp));
}

int   Ftp::LoginCheck(int act,int exp)
{
   (void)exp;
   if(act==RESP_LOGIN_FAILED)
   {
      if(strstr(line,"Login incorrect"))
	 return(LOGIN_FAILED_STATE);
   }
   return(-1);
}

int   Ftp::NoPassReqCheck(int act,int exp)
{
   (void)exp;
   if(act==RESP_LOGGED_IN) // in some cases, ftpd does not ask for pass.
      return(state);
   return(-1);
}

int   Ftp::IgnoreCheck(int,int)
{
   return(state);
}

int   Ftp::RNFR_Check(int act,int exp)
{
   if(act==exp)
   {
      char *str=(char*)alloca(10+strlen(file1));
      sprintf(str,"RNTO %s\n",file1);
      SendCmd(str);
      AddResp(250,INITIAL_STATE,NoFileCheck);
      return state;
   }
   return NoFileCheck(act,exp);
}

char *Ftp::ExtractPWD()
{
   static char pwd[1024];

   if(sscanf(line,"%*d \"%[^\"]\"",pwd)!=1)
      return "";

   if(isalpha(pwd[0]) && pwd[1]==':')
      flags|=DOSISH_PATH;
   int slash=0;
   for(char *s=pwd; *s; s++)
   {
      if(*s=='/')
      {
	 slash=1;
	 break;
      }
   }
   if(slash==0 || (flags&DOSISH_PATH))
   {
      // for safety -- against dosish ftpd
      for(char *s=pwd; *s; s++)
	 if(*s=='\\')
	    *s='/';
   }
   return pwd;
}


int   Ftp::CatchHomePWD(int act,int exp)
{
   if(act==exp && !home[0])
      strcpy(home,ExtractPWD());
   return state;
}

int   Ftp::CatchPWDResponce(int act,int exp)
{
   if(act==exp && !AbsolutePath(cwd))
   {
      char *pwd=ExtractPWD();
      if(pwd[0])
      {
	 char *s=(char*)alloca(strlen(pwd)+20);
	 sprintf(s,"Changing cwd to `%s'",pwd);
	 DebugPrint("---- ",s,9);

	 strcpy(cwd,pwd);
	 if(mode==CHANGE_DIR && !AbsolutePath(file))
	 {
	    strcpy(file,cwd);
	 }
      }
   }
   return(state);
}

int   Ftp::CatchDATE(int act,int exp)
{
   if(act==exp)
      array_for_info[array_ptr].time=ConvertFtpDate(result);
   else if(act!=550 && act!=553 && act!=500 && act!=502)
      return -1;
   else
      array_for_info[array_ptr].time=(time_t)-1;

   array_for_info[array_ptr].get_time=false;
   if(!array_for_info[array_ptr].get_size)
      array_ptr++;

   return state;
}

int   Ftp::CatchSIZE(int act,int exp)
{
   if(act==exp)
      array_for_info[array_ptr].size=atol(result);
   else if(act!=550 && act!=553 && act!=500 && act!=502)
      return -1;
   else
      array_for_info[array_ptr].size=-1;

   array_for_info[array_ptr].get_size=false;
   if(!array_for_info[array_ptr].get_time)
      array_ptr++;

   return state;
}

int   Ftp::CheckHangup(struct pollfd *pfd,int num)
{
#ifdef SO_ERROR
   for(int i=0; i<num; i++)
   {
      char  str[256];
      int   len,s_errno;

      errno=0;

/* When the poll() is emulated by select(), there is no error condition
   flag, so we can only check for socket errors */

// Where does the error number go - to errno or to the pointer?
// It seems that to errno, but if the pointer is NULL it dumps core.

#if !defined(HAVE_POLL)
      len=sizeof(s_errno);
      getsockopt(pfd[i].fd,SOL_SOCKET,SO_ERROR,(char*)&s_errno,&len);
      if(errno!=0)
      {
	 sprintf(str,"Socket error (%s) - reconnecting",strerror(errno));
	 DebugPrint("**** ",str);
	 Disconnect();
	 return 1;
      }
#else
      if(pfd[i].revents&POLLERR)
      {
   	 len=sizeof(s_errno);
	 getsockopt(pfd[i].fd,SOL_SOCKET,SO_ERROR,(char*)&s_errno,&len);
	 sprintf(str,"Socket error (%s) - reconnecting",strerror(errno));
	 DebugPrint("**** ",str);
	 Disconnect();
	 return 1;
      }
#endif /* HAVE_POLL */
   } /* end for */
#endif /* SO_ERROR */
   return 0;
}

const char *Ftp::StrError(int err)
{
   static char str[256];

   switch(err)
   {
   case(IN_PROGRESS):
      return("Operation is in progress");
   case(OK):
      return("Error 0");
   case(SEE_ERRNO):
      return(strerror(errno));
   case(SEE_H_ERRNO):
#ifdef HAVE_HSTRERROR
      return(hstrerror(h_errno));
#else
      if(h_errno==0)
	 return("Error 0");
      if(h_errno==HOST_NOT_FOUND)
	 return("Unknown host");
      if(h_errno==TRY_AGAIN)
	 return("Host name lookup failure");
      if(h_errno==NO_RECOVERY)
	 return("Unknown server error");
      if(h_errno==NO_DATA)
	 return("No address associated with name");
      return("Unknown error");
#endif
   case(NOT_OPEN):
      return("Ftp class is not Open()ed");
   case(NO_FILE):
      if(last_error_resp)
      {
   	 sprintf(str,"Access failed: %s",last_error_resp);
	 return(str);
      }
      return("File cannot be accessed");
   case(NO_HOST):
      return("Not connected");
   case(FATAL):
      if(last_error_resp)
      {
   	 sprintf(str,"Fatal error: %s",last_error_resp);
	 return(str);
      }
      return("Fatal error");
   case(STORE_FAILED):
      return("Store failed - you have to reput");
   case(LOGIN_FAILED):
      return("Login failed");
   }
   return("");
}

void  Ftp::Sleep()
{
   char	 str[256];
   sprintf(str,"Sleeping %d second%s",sleep_time,sleep_time!=1?"s":"");
   DebugPrint("---- ",str);
   sleep(sleep_time);
}

int   Ftp::Connect(const char *hostname,int port,const struct sockaddr *addr)
{
   if(addr==NULL)
   {
      static struct sockaddr_in sa;
      struct hostent *ha;
      time_t   t;

      sa.sin_family=AF_INET;
      sa.sin_port=htons(port);
      try_time=0;
   try_again:
      time(&t);
      if(t-try_time<sleep_time)
	 Sleep();
      time(&try_time);
      DebugPrint("---- ","Resolving host name",2);
      ha=gethostbyname(hostname);
      if(ha==NULL)
      {
	 char  s[512];
	 sprintf(s,"%s: gethostbyname() failed, h_errno=%d",hostname,h_errno);
	 DebugPrint("**** ",s);
	 if(h_errno==TRY_AGAIN)
	    goto try_again;
	 try_time=0;
	 return(SEE_H_ERRNO);
      }
      try_time=0;
      sa.sin_family=ha->h_addrtype;
      memcpy(&sa.sin_addr,ha->h_addr_list[0],ha->h_length);
      addr=(struct sockaddr*)&sa;
   }
   Close();
   Disconnect();

   strcpy(this->hostname,hostname);
   this->port=port;
   strcpy(cwd,"~");

   memcpy(&this->peer_sa,addr,sizeof(struct sockaddr));
   state=INITIAL_STATE;

   flags=0;

   return(OK);
}

void Ftp::InitFtp()
{
   control_sock=-1;
   data_sock=-1;

   file[0]=0;
   file1=0;
   strcpy(cwd,"~");
   strcpy(home,"");
   pos=0;
   mode=CLOSED;
   resp_size=0;
   type=TYPE_A;
   try_time=0;
   time(&event_time);
   timeout=600; // give it 10 minutes before reconnect
   send_cmd_buffer=0;
   send_cmd_alloc=0;
   send_cmd_count=0;
   send_cmd_ptr=send_cmd_buffer;
   result=NULL;
   result_size=0;
   debug_file=stderr;
   debug_level=9;
   state=NO_HOST_STATE;
   flags=0/*SYNC_MODE*/;
   saved_errno=0;

   RespQueue=0;
   RQ_alloc=0;
   EmptyRespQueue();
   last_error_resp=NULL;

   sleep_time=30;   // By default - 30 sec. Change with SetSleep(s)

   port=0;
   hostname[0]=0;

   struct passwd *pw=getpwuid(getuid());
   sprintf(anon_pass,"-%s@",pw?pw->pw_name:"unknown");

   AnonymousLogin();
}
Ftp::Ftp()
{
   InitFtp();
}
Ftp::Ftp(const Ftp *f)
{
   InitFtp();
   *this=*f;
}
const Ftp& Ftp::operator=(const Ftp& f)
{
   Close();
   Disconnect();

   InitFtp();

   strcpy(cwd,f.cwd);
   strcpy(home,f.home);
   strcpy(user,f.user);
   strcpy(pass,f.pass);
   strcpy(group,f.group);
   strcpy(gpass,f.gpass);
   strcpy(anon_pass,f.anon_pass);
   debug_file=f.debug_file;
   debug_level=f.debug_level;
   if(f.state!=NO_HOST_STATE)
      state=INITIAL_STATE;
   sleep_time=f.sleep_time;
   port=f.port;
   strcpy(hostname,f.hostname);
   memcpy(&peer_sa,&f.peer_sa,sizeof(peer_sa));
   flags=f.flags&MODES_MASK;

   return *this;
}

Ftp::~Ftp()
{
   Close();
   Disconnect();
   if(RespQueue)
      free(RespQueue);
   if(send_cmd_buffer)
      free(send_cmd_buffer);
}

void  Ftp::DebugPrint(const char *prefix,const char *str,int level)
{
   if(level>debug_level)
      return;
   if(debug_file==NULL)
   {
      // ?FIXME - store the message in memory ?
      return;
   }
   for(;;)
   {
      fputs(prefix,debug_file);
      if(!strncmp(str,"PASS ",5))
      {
	 // don't log PASS message
	 fputs("PASS XXXX\n",debug_file);
	 while(*str && *str++!='\n');
	 if(*str==0)
	    break;
	 continue;
      }
      while(*str && *str!='\n')
      {
         if(!(*str=='\r' && (str[1]=='\n' || str[1]=='\0')))
            fputc(*str,debug_file);
         str++;
      }
      fputc('\n',debug_file);
      if(!*str++ || !*str)
         break;
   }
   fflush(debug_file);
}

int   Ftp::Poll(int fd,int ev)
{
   struct pollfd pfd;
   pfd.fd=fd;
   pfd.events=ev;
   pfd.revents=0;
   int res=poll(&pfd,1,0);
   if((res==-1 && res==EINTR) || res==0)
      return 0;
   if(res==-1)
   {
      SwitchToState(SYSTEM_ERROR_STATE);
      return 0;
   }
   if(CheckHangup(&pfd,1))
      return 0;
   return pfd.revents;
}

int   Ftp::AbsolutePath(const char *s)
{
   return(s[0]=='/'
      || ((flags&DOSISH_PATH) && isalpha(s[0]) && s[1]==':' && s[2]=='/'));
}

int   Ftp::Do()
{
   char	 str[256],str1[1050];
   int   old_type,addr_len,res;
   unsigned char *a;
   unsigned char *p;
   time_t   t;
   automate_state oldstate;
   int	 m=STALL;

   goto sw;
again:
   m=MOVED;
sw:
   switch(state)
   {
   case(INITIAL_STATE):
      if(mode==CLOSED)
	 return m;

      if(hostname[0]==0)
	 return m;

      time(&t);
      if(t-try_time<sleep_time)
      {
	 block+=TimeOut(1000*(sleep_time-(t-try_time)));
	 return m;
      }
      time(&try_time);

      control_sock=socket(PF_INET,SOCK_STREAM,IPPROTO_TCP);
      if(control_sock==-1)
	 goto system_error;
      setsockopt(control_sock,SOL_SOCKET,SO_KEEPALIVE,(char *)&one,sizeof(one));
      fcntl(control_sock,F_SETFL,O_NONBLOCK);
      fcntl(control_sock,F_SETFD,FD_CLOEXEC);

      a=(unsigned char*)&peer_sa.sin_addr;
      sprintf(str,"Connecting to %s (%d.%d.%d.%d) port %u",hostname,a[0],a[1],a[2],a[3],port);
      DebugPrint("---- ",str,0);
      if(connect(control_sock,(struct sockaddr*)&peer_sa,sizeof(peer_sa))==-1
#ifdef EINPROGRESS
      && errno!=EINPROGRESS
#endif
      )
      {
	 sprintf(str,"connect: %s",strerror(errno));
         DebugPrint("**** ",str,0);
         Disconnect();
	 if(NotSerious(errno))
	    goto again;
	 goto system_error;
      }
      state=CONNECTING_STATE;
      m=MOVED;

   case(CONNECTING_STATE):
      res=Poll(control_sock,POLLOUT);
      if(state!=CONNECTING_STATE)
	 goto again;
      if(!(res&POLLOUT))
      {
	 block.Merge(PollVec(control_sock,POLLOUT));
	 return m;
      }

      if(flags&SYNC_MODE)
	 flags|=SYNC_WAIT; // we need to wait for RESP_READY

      state=EOF_STATE;
      AddResp(RESP_READY,INITIAL_STATE);
      sprintf(str,"USER %s\nPASS %s\n",user,pass);
      AddResp(RESP_PASS_REQ,INITIAL_STATE,NoPassReqCheck);
      AddResp(RESP_LOGGED_IN,INITIAL_STATE,LoginCheck);
      SendCmd(str);

      // FIXME: site group/site gpass

      if(!home[0])
      {
	 SendCmd("PWD");
	 AddResp(RESP_PWD_MKD_OK,INITIAL_STATE,CatchHomePWD);
      }

      if(cwd[0] && strcmp(cwd,"~"))
      {
	 if(cwd[0]=='~' && cwd[1]=='/')
	    sprintf(str,"CWD %s\n",cwd+2);
	 else
	    sprintf(str,"CWD %s\n",cwd);
	 AddResp(RESP_CWD_RMD_DELE_OK,INITIAL_STATE);
	 SendCmd(str);
      }

      type=TYPE_A;   // just after login we are in TEXT mode
      m=MOVED;

   case(EOF_STATE):
      ReceiveResp();
      if(state!=EOF_STATE)
	 goto again;

      if(mode==CLOSED)
      {
	 block.Merge(PollVec(control_sock,POLLIN));
	 return m;
      }

      if(mode==STORE && (flags&NOREST_MODE) && pos>0)
      {
	 DebugPrint("**** ","Cannot put with pos>0 and NOREST mode");
	 xfree(last_error_resp);
	 last_error_resp=strdup("Store failed due to lack of REST command");
	 SwitchToState(FATAL_STATE);
	 return MOVED;
      }

      ReceiveResp();

      if(state!=EOF_STATE)
         goto again;

      if(mode==RETRIEVE || mode==STORE || mode==LIST || mode==LONG_LIST)
      {
	 data_sock=socket(PF_INET,SOCK_STREAM,IPPROTO_TCP);
	 if(data_sock==-1)
	    goto system_error;
	 fcntl(data_sock,F_SETFL,O_NONBLOCK);
	 fcntl(data_sock,F_SETFD,FD_CLOEXEC);
	 setsockopt(data_sock,SOL_SOCKET,SO_KEEPALIVE,(char *)&one,sizeof(one));
      }

      old_type=type;
      if(flags&NOREST_MODE)
	 real_pos=0;
      else
	 real_pos=pos;

      flags&=~IO_FLAG;

      switch(mode)
      {
      case(RETRIEVE):
         type=TYPE_I;
         sprintf(str1,"RETR %s\n",file);
         break;
      case(STORE):
         type=TYPE_I;
         sprintf(str1,"STOR %s\n",file);
         break;
      case(LONG_LIST):
         type=TYPE_A;
         if(file[0])
            sprintf(str1,"LIST %s\n",file);
         else
            sprintf(str1,"LIST\n");
         break;
      case(LIST):
         type=TYPE_A;
         real_pos=0; // REST don't work for NLST
	 if(file[0])
            sprintf(str1,"NLST %s\n",file);
         else
            sprintf(str1,"NLST\n");
         break;
      case(SIZE):
         type=TYPE_I;
         sprintf(str1,"SIZE %s\n",file);
         break;
      case(DATE):
         sprintf(str1,"MDTM %s\n",file);
         break;
      case(CHANGE_DIR):
	 if(!AbsolutePath(file) && !strncmp(file,cwd,strlen(cwd)))
	 {
	    int len=strlen(cwd);
	    switch(file[len])
	    {
	    case(0):
	       str1[0]=0;
	       break;
	    case('/'):
	       sprintf(str1,"CWD .%s\n",file+len);
	       break;
	    default:
	       sprintf(str1,"CWD %s\n",file);
	       break;
	    }
	 }
	 else
	    sprintf(str1,"CWD %s\n",file);
	 break;
      case(MAKE_DIR):
	 sprintf(str1,"MKD %s\n",file);
	 break;
      case(REMOVE_DIR):
	 sprintf(str1,"RMD %s\n",file);
	 break;
      case(REMOVE):
	 sprintf(str1,"DELE %s\n",file);
	 break;
      case(SITE_CMD):
	 sprintf(str1,"SITE %s\n",file);
	 break;
      case(RENAME):
	 sprintf(str1,"RNFR %s\n",file);
	 break;
      case(ARRAY_INFO):
	 type=TYPE_I;
	 break;
      case(CLOSED):
	 abort(); // can't happen
      }
      if(old_type!=type)
      {
         strcpy(str,type==TYPE_I?"TYPE I\n":"TYPE A\n");
	 SendCmd(str);
	 AddResp(RESP_TYPE_OK,INITIAL_STATE);
      }

      if(mode==ARRAY_INFO)
      {
	 for(int i=array_ptr; i<array_cnt; i++)
	 {
	    if(array_for_info[i].get_time)
	    {
	       sprintf(str,"MDTM %s\n",array_for_info[i].file);
	       SendCmd(str);
	       AddResp(RESP_RESULT_HERE,INITIAL_STATE,CatchDATE);
	    }
	    if(array_for_info[i].get_size)
	    {
	       sprintf(str,"SIZE %s\n",array_for_info[i].file);
	       SendCmd(str);
	       AddResp(RESP_RESULT_HERE,INITIAL_STATE,CatchSIZE);
	    }
	 }
      	 state=WAITING_STATE;
	 goto waiting_state_label;
      }

      if(mode==SIZE || mode==DATE || mode==CHANGE_DIR || mode==SITE_CMD
      || mode==REMOVE || mode==REMOVE_DIR || mode==MAKE_DIR || mode==RENAME)
      {
	 /* If the command is CHANGE_DIR, do it if the target
	    dir is different from current and target dir is not the home.
	    The case of home target dir is handled already */
	 if(mode!=CHANGE_DIR || (strcmp(cwd,file) && file[0]))
	 {
	    SendCmd(str1);
	    if(mode==CHANGE_DIR || mode==REMOVE_DIR || mode==REMOVE)
	       AddResp(RESP_CWD_RMD_DELE_OK,INITIAL_STATE,NoFileCheck);
	    else if(mode==SIZE || mode==DATE)
	       AddResp(RESP_RESULT_HERE,INITIAL_STATE,NoFileCheck);
	    else if(mode==MAKE_DIR)
	       AddResp(RESP_PWD_MKD_OK,INITIAL_STATE,NoFileCheck);
	    else if(mode==SITE_CMD)
	       AddResp(0,INITIAL_STATE,IgnoreCheck,true);
	    else if(mode==RENAME)
	       AddResp(350,INITIAL_STATE,RNFR_Check);
	 }
	 if(mode==CHANGE_DIR && !AbsolutePath(file))
	 {
	    AddResp(RESP_PWD_MKD_OK,INITIAL_STATE,CatchPWDResponce);
	    SendCmd("PWD\n");
	 }
	 state=WAITING_STATE;
	 if(result)
	 {
	    free(result);
	    result=0;
	    result_size=0;
	 }
	 goto waiting_state_label;
      }

      addr_len=sizeof(struct sockaddr);
      getsockname(control_sock,(struct sockaddr*)&data_sa,&addr_len);
      data_sa.sin_port=0;
      bind(data_sock,(struct sockaddr*)&data_sa,addr_len);
      listen(data_sock,1);
      getsockname(data_sock,(struct sockaddr*)&data_sa,&addr_len);
      a=(unsigned char*)&data_sa.sin_addr;
      p=(unsigned char*)&data_sa.sin_port;

      sprintf(str,"PORT %d,%d,%d,%d,%d,%d\n",a[0],a[1],a[2],a[3],p[0],p[1]);
      SendCmd(str);
      AddResp(RESP_PORT_OK,INITIAL_STATE);
      if(real_pos!=0)
      {
         sprintf(str,"REST %ld\n",real_pos);
	 SendCmd(str);
	 AddResp(RESP_REST_OK,INITIAL_STATE,NotImplementedCheck);
      }
      SendCmd(str1);
      //AddResp(RESP_DATA_OPEN,INITIAL_STATE,NoFileCheck);
      AddResp(RESP_TRANSFER_OK,mode==STORE?STORE_FAILED_STATE:INITIAL_STATE,TransferCheck);
      state=ACCEPTING_STATE;

   case(ACCEPTING_STATE):
      FlushSendQueue();
      ReceiveResp();

      if(state!=ACCEPTING_STATE)
         goto again;

      res=Poll(data_sock,POLLIN);
      if(!(res&POLLIN))
      {
      accepting_return:
	 block.Merge(PollVec(control_sock,POLLIN));
	 if(send_cmd_count>0 && !(flags&SYNC_WAIT))
	    block.Merge(PollVec(control_sock,POLLOUT));
	 block.Merge(PollVec(data_sock,POLLIN));
	 return m;
      }

      addr_len=sizeof(struct sockaddr);
      res=accept(data_sock,(struct sockaddr *)&data_sa,&addr_len);
      if(res==-1)
      {
	 if(errno==EWOULDBLOCK)
	    goto accepting_return;
	 if(NotSerious(errno))
	 {
	    Disconnect();
	    goto again;
	 }
	 goto system_error;
      }

      close(data_sock);
      data_sock=res;
      state=(mode==STORE?WRITING_DATA_STATE:READING_DATA_STATE);

   case(READING_DATA_STATE):
   case(WRITING_DATA_STATE):
   {
      int ev=(state==READING_DATA_STATE?POLLIN:POLLOUT);
      oldstate=state;

      FlushSendQueue();
      ReceiveResp();

      if(state!=oldstate)
         goto again;

      if(data_sock==-1 && RespQueueIsEmpty())
      {
	 block+=NoWait();
	 return m;
      }
      if(data_sock!=-1)
      {
	 res=Poll(data_sock,ev);
	 if(state!=oldstate)
	    goto again;
	 block.Merge(PollVec(data_sock,ev));
      }

      block.Merge(PollVec(control_sock,POLLIN));
      if(send_cmd_count>0 && !(flags&SYNC_WAIT))
	 block.Merge(PollVec(control_sock,POLLOUT));

      return m;
   }
   case(WAITING_STATE):
   waiting_state_label:
      oldstate=state;

      FlushSendQueue();
      ReceiveResp();

      if(state!=oldstate)
         goto again;

      block.Merge(PollVec(control_sock,POLLIN));
      if(send_cmd_count>0 && !(flags&SYNC_WAIT))
	 block.Merge(PollVec(control_sock,POLLOUT));
      if(RespQueueSize()==0)
	 block+=NoWait();

      return m;

   default:
      // error state - need intervension of other task
      if(mode!=CLOSED)
	 block+=NoWait();
      return m;
   }
   return m;

system_error:
   if(errno==ENFILE || errno==EMFILE)
   {
      // file table overflow
      return m;
   }
   SwitchToState(SYSTEM_ERROR_STATE);
   return MOVED;
}

void  Ftp::LogResp(char *l)
{
   if(result==0)
   {
      result=xstrdup(l);
      result_size=strlen(result);
      return;
   }
   result=(char*)xrealloc(result,result_size+strlen(l)+1);
   strcpy(result+result_size,l);
   result_size+=strlen(l);
}

int   Ftp::ReceiveResp()
{
   char  *store;
   char  *nl;
   int   code;

   if(control_sock==-1)
      return(OK);

   for(;;)
   {
      for(;;)
      {
	 store=resp+resp_size;
	 *store=0;
	 nl=strchr(resp,'\n');
	 if(nl!=NULL)
	 {
	    flags&=~SYNC_WAIT;
	    *nl=0;
	    strcpy(line,resp);
	    memmove(resp,nl+1,strlen(nl+1)+1);
	    resp_size-=nl-resp+1;
	    if(nl-resp>0 && line[nl-resp-1]=='\r')
	       line[nl-resp-1]=0;

	    DebugPrint("<--- ",line,1);
	    if(!RespQueueIsEmpty() && RespQueue[RQ_head].log_resp)
	    {
	       LogResp(line);
	       LogResp("\n");
	    }

	    if(strlen(line)<3 || !isdigit(line[0]))
	       continue;
	    if(sscanf(line,"%3d",&code)!=1)
	       continue;
	    if(line[3]=='-')
	       continue;
	    break;
	 }
	 else
	 {
	    if(sizeof(resp)-resp_size<1)
	    {
	       SwitchToState(FATAL_STATE);
	       return(FATAL);
	    }

	    struct pollfd pfd;
	    pfd.fd=control_sock;
	    pfd.events=POLLIN;
	    int res=poll(&pfd,1,0);
	    if(res==-1)
	       return(SEE_ERRNO);
	    if(res==0)
	       return(OK);
	    if(CheckHangup(&pfd,1))
	       return(OK);

	    res=read(control_sock,resp+resp_size,sizeof(resp)-resp_size-1);
	    if(res==-1)
	    {
	       if(NotSerious(errno))
	       {
		  Disconnect();
		  return(OK);
	       }
	       return(SEE_ERRNO);
	    }
	    if(res==0)
	    {
	       DebugPrint("**** ","Peer closed connection");
	       Disconnect();
	       return(OK);
	    }

	    resp_size+=res;
	 }
      }
      if(code==RESP_RESULT_HERE)
      {
	 if(result)
	    free(result);
	 result=(char*)xmalloc(result_size=strlen(line+4)+1);
	 strcpy(result,line+4);	 // store the responce for reference
      }

      if(code>=100 && code<200)	// intermediate responces are ignored
	 continue;

      int newstate=CheckResp(code);
      if(newstate!=-1 && newstate!=state)
      {
	 if(newstate==FATAL_STATE || newstate==NO_FILE_STATE)
	 {
	    xfree(last_error_resp);
	    last_error_resp=strdup(line);
	 }
	 SwitchToState((automate_state)newstate);
	 if(resp_size==0)
	    return(OK);
      }
   }
   return(OK);
}

void  Ftp::Disconnect()
{
   DataClose();
   if(control_sock>=0)
   {
      DebugPrint("---- ","Closing control socket",2);
//       struct linger l={0,0};
//       setsockopt(control_sock,SOL_SOCKET,SO_LINGER,(char*)&l,sizeof(l));
      shutdown(control_sock,2);
      close(control_sock);
      control_sock=-1;
      DebugPrint("---- ","close() done",9);
   }
   resp_size=0;
   state=(mode==STORE?STORE_FAILED_STATE:INITIAL_STATE);
   send_cmd_count=0;
   flags&=~SYNC_WAIT;
   EmptyRespQueue();
}

void  Ftp::DataClose()
{
   if(data_sock>=0)
   {
      DebugPrint("---- ","Closing data socket",2);
//      struct linger l={0,0};
//      setsockopt(data_sock,SOL_SOCKET,SO_LINGER,(char*)&l,sizeof(l));
      shutdown(data_sock,2);
      close(data_sock);
      data_sock=-1;
      DebugPrint("---- ","close() done",9);
   }
   xfree(result);
   result=NULL;
}

int   Ftp::FlushSendQueue()
{
   if(send_cmd_count==0 || (flags&SYNC_WAIT))
      return(OK);

   int res;
   struct pollfd pfd;

   pfd.events=POLLOUT;
   pfd.fd=control_sock;
   res=poll(&pfd,1,0);
   if(res==-1)
      return(SEE_ERRNO);
   if(res==0)
      return(OK);
   if(CheckHangup(&pfd,1))
      return(OK);
   if(!(pfd.revents&POLLOUT))
      return(OK);

   while(send_cmd_count>0)
   {
      char *line_end=(char*)memchr(send_cmd_ptr,'\n',send_cmd_count);
      if(line_end==NULL)
	 return(OK);
      res=write(control_sock,send_cmd_ptr,line_end-send_cmd_ptr+1);
      if(res==0)
      {
	 errno=EAGAIN;
	 return(SEE_ERRNO);
      }
      if(res==-1)
      {
	 if(NotSerious(errno))
	 {
	    errno=EAGAIN;
	    Disconnect();
	 }
	 return(SEE_ERRNO);
      }
      send_cmd_count-=res;
      send_cmd_ptr+=res;

      if(flags&SYNC_MODE)
      {
	 flags|=SYNC_WAIT;
   	 break;
      }
   }
   return(OK);
}

int   Ftp::SendCmd(const char *cmd)
{
   char ch,prev_ch;
   DebugPrint("---> ",cmd,1);
   if(send_cmd_count==0)
      send_cmd_ptr=send_cmd_buffer;
   prev_ch=0;
   while((ch=*cmd++)!=0)
   {
      if(send_cmd_ptr-send_cmd_buffer+send_cmd_count>=send_cmd_alloc-1)
      {
	 int res=FlushSendQueue();
	 if(res!=OK)
	    return(res);
	 if(send_cmd_ptr-send_cmd_buffer<2)
	 {
	    int shift=send_cmd_ptr-send_cmd_buffer;
	    send_cmd_buffer=(char*)xrealloc(send_cmd_buffer,send_cmd_alloc+=0x1000);
	    send_cmd_ptr=send_cmd_buffer+shift;
	 }
	 memmove(send_cmd_buffer,send_cmd_ptr,send_cmd_count);
	 send_cmd_ptr=send_cmd_buffer;
      }
      if(ch=='\n' && prev_ch!='\r')
      {
	 ch='\r';
	 cmd--;
      }
      else if(ch=='\377')
      	 send_cmd_ptr[send_cmd_count++]=255; // double chr(255) as in telnet protocol
      send_cmd_ptr[send_cmd_count++]=prev_ch=ch;
      if(*cmd==0 && ch!='\n')
	 cmd="\n";
   }
   return(OK);
}

int   Ftp::Login(const char *user,const char *pass)
{
   Disconnect();
   strcpy(this->user,user);
   strcpy(this->pass,pass);
   strcpy(cwd,"~");
   return(OK);
}

int   Ftp::AnonymousLogin()
{
   Login("ftp",anon_pass);
   GroupLogin("","");
   return(OK);
}

int   Ftp::GroupLogin(const char *group,const char *pass)
{
   Disconnect();  // FIXME: Handle this without disconnect ?
   strcpy(this->group,group);
   strcpy(this->gpass,pass);
   return(OK);
}

int   Ftp::Open(const char *fn,open_mode mode,long offs)
{
   Close();
   strcpy(file,fn);
   pos=offs;
   this->mode=mode;
   return(OK);
}

int   Ftp::SendEOT()
{
   if(mode==STORE)
   {
      if(state==WRITING_DATA_STATE)
      {
	 DataClose();
	 state=WAITING_STATE;
      	 return(OK);
      }
      errno=EAGAIN;
      return(SEE_ERRNO);
   }
   return(OK);
}

int   Ftp::Close()
{
   Resume();
   HomifyCWD();
   DataClose();
   if(control_sock!=-1)
   {
      switch(state)
      {
      case(ACCEPTING_STATE):
	 Disconnect();
	 break;
      case(WAITING_STATE):
	 if((mode==CHANGE_DIR || mode==SITE_CMD) && !RespQueueIsEmpty())
	 {
      	    Disconnect();
	    break;
	 }
      case(READING_DATA_STATE):
      case(WRITING_DATA_STATE):
      case(NO_FILE_STATE):
      case(STORE_FAILED_STATE):
      case(FATAL_STATE):
      case(SYSTEM_ERROR_STATE):
	 state=EOF_STATE;
	 break;
      case(NO_HOST_STATE):
      case(INITIAL_STATE):
      case(EOF_STATE):
      case(LOGIN_FAILED_STATE):
	 break;
      case(CONNECTING_STATE):
	 Disconnect();
	 break;
      }
   }
   else
   {
      if(hostname[0])
	 state=INITIAL_STATE;
      else
	 state=NO_HOST_STATE;
   }
   file[0]=0;
   if(file1)
   {
      free(file1);
      file1=0;
   }
   pos=0;
   mode=CLOSED;
   return(OK);
}

int   Ftp::Read(void *buf,int size)
{
   int res,shift;

   Resume();
   Do();
   res=StateToError();
   if(res!=OK)
      return(res);

   if(mode==CLOSED)
      return(0);

   if(mode==CHANGE_DIR || mode==MAKE_DIR || mode==REMOVE_DIR || mode==REMOVE)
   {
      if(state==WAITING_STATE)
      {
	 if(RespQueueIsEmpty())
	 {
	    if(mode==CHANGE_DIR)
	    {
	       strcpy(cwd,file);
	    }
	    mode=CLOSED;
	    return(0);
	 }
      }
      errno=EAGAIN;
      return(SEE_ERRNO);
   }
   if(state==WAITING_STATE && RespQueueIsEmpty())
   {
      if(result_size==0)
      {
	 SwitchToState(EOF_STATE);
	 return(0);
      }
      if(result_size<size)
	 size=result_size;
      memcpy(buf,result,size);
      memmove(result,result+size,result_size-=size);
      return(size);
   }

read_again:
   if(state==READING_DATA_STATE)
   {
      if(data_sock==-1)
	 goto we_have_eof;

      if(RespQueueSize()>1 && real_pos>0)
      {
	 errno=EAGAIN;
	 return(SEE_ERRNO);
      }

      struct pollfd pfd;
      pfd.fd=data_sock;
      pfd.events=POLLIN;
      res=poll(&pfd,1,0);
      if(res==-1)
      {
	 return(SEE_ERRNO);
      }
      if(res==0)
      {
	 errno=EAGAIN;
	 return(SEE_ERRNO);
      }
      if(CheckHangup(&pfd,1))
      {
	 errno=EAGAIN;
	 return(SEE_ERRNO);
      }
      res=read(data_sock,buf,size);
      if(res==-1)
      {
	 if(NotSerious(errno))
	 {
	    Disconnect();
	    errno=EAGAIN;
	 }
	 return(SEE_ERRNO);
      }
      if(res==0)
      {
      we_have_eof:
	 if(RespQueueIsEmpty())
	 {
	    SwitchToState(EOF_STATE);
	    return(0);
	 }
	 else
	 {
	    DataClose();
	    errno=EAGAIN;
	    return(SEE_ERRNO);
	 }
      }
      real_pos+=res;
      if(real_pos<=pos)
	 goto read_again;
      flags|=IO_FLAG;
      if((shift=pos+res-real_pos)>0)
      {
	 memmove(buf,(char*)buf+shift,size-shift);
	 res-=shift;
      }
      pos+=res;
      return(res);
   }
   errno=EAGAIN;
   return(SEE_ERRNO);
}

/*
   Write - send data to ftp server

   * Uploading is not reliable in this realization *
   Well, not less reliable than in any usual ftp client.

   The reason for this is uncheckable receiving of data on the remote end.
   Since that, we have to leave re-putting up to user program
*/
int   Ftp::Write(const void *buf,int size)
{
   if(mode!=STORE)
      return(0);

   Resume();
   Do();
   int res=StateToError();
   if(res!=OK)
      return(res);

   if(state!=WRITING_DATA_STATE || (RespQueueSize()>1 && real_pos>0))
   {
      errno=EAGAIN;
      return(SEE_ERRNO);
   }

   struct pollfd pfd;
   pfd.fd=data_sock;
   pfd.events=POLLOUT;
   res=poll(&pfd,1,0);
   if(res==-1)
      return(SEE_ERRNO);
   if(res==0)
   {
      errno=EAGAIN;
      return(SEE_ERRNO);
   }
   if(CheckHangup(&pfd,1))
   {
      errno=EAGAIN;
      return(SEE_ERRNO);
   }
   res=write(data_sock,buf,size);
   if(res==-1)
   {
      if(NotSerious(errno))
      {
	 Disconnect();
	 errno=EAGAIN;
      }
      return(SEE_ERRNO);
   }
   pos+=res;
   return(res);
}

int   Ftp::StoreStatus()
{
   int res=StateToError();
   if(res!=OK)
      return(res);

   if(mode!=STORE)
      return(OK);

   if(state==WAITING_STATE && RespQueueIsEmpty())
   {
      SwitchToState(EOF_STATE);
      return(OK);
   }
   if(state==WRITING_DATA_STATE)
   {
      // did not send EOT by SendEOT, do it now
      SendEOT();
   }
   return(IN_PROGRESS);
}

void  Ftp::SwitchToState(automate_state ns)
{
   if(ns==state)
      return;
   switch(ns)
   {
   case(INITIAL_STATE):
      Disconnect();
      break;
   case(FATAL_STATE):
      if(mode==DATE)
	 break;
   case(STORE_FAILED_STATE):
   case(LOGIN_FAILED_STATE):
      Disconnect();
      break;
   case(EOF_STATE):
      DataClose();
      file[0]=0;
      break;
   case(NO_FILE_STATE):
      break;
   default:
      abort();
   }
   if(ns==STORE_FAILED_STATE && mode!=STORE)
      state=INITIAL_STATE;
   else
      state=ns;
}

int   Ftp::DataReady()
{
   int	 res;

   res=StateToError();
   if(res!=OK)
      return(1);  // say data ready and let'em get an error

   if(mode==CLOSED)
      return(1);

   if(state==WAITING_STATE)
      return(RespQueueIsEmpty());

   if(state==READING_DATA_STATE
   || state==WRITING_DATA_STATE)
   {
      if(data_sock==-1)
	 return(1);  // eof
      if(real_pos!=0 && RespQueueSize()>1)
	 return(0);  // disallow reading/writing
      int ev=(state==READING_DATA_STATE?POLLIN:POLLOUT);
      if(Poll(data_sock,ev)&ev)
	 return(1);
   }
   return(0);
}

// Wait until requested data is available or operation is completed
int   Ftp::Block()
{
   int res;

   for(;;)
   {
      SMTask::Schedule();

      res=StateToError();
      if(res!=OK)
	 return res;

      if(DataReady())
	 return(OK);

      SMTask::Block();
   }
}

static inline
int last_element_is_doubledot(char *path,char *end)
{
   return((end==path+2 && !strncmp(path,"..",2))
        || (end>path+2 && !strncmp(end-3,"/..",3)));
}

void  Ftp::Chdir(const char *path)
{
   char	 *newcwd=(char*)alloca(strlen(cwd)+strlen(path)+2);
   int	 prefix_size=0;

   if(cwd[0]=='/')
      prefix_size=1;
   else if(isalpha(cwd[0]) && cwd[1]==':')
      prefix_size=2+(cwd[2]=='/');
   else if(cwd[0]=='~' && (cwd[1]=='/' || cwd[1]=='\0'))
      prefix_size=1+(cwd[1]=='/');

   if(path[0]=='/')
   {
      strcpy(newcwd,path);
      prefix_size=1;
   }
   else if(isalpha(path[0]) && path[1]==':')
   {
      strcpy(newcwd,path);
      prefix_size=2+(path[2]=='/');
   }
   else if(path[0]=='~' && (path[1]=='/' || path[1]=='\0'))
   {
      strcpy(newcwd,path);
      prefix_size=1+(path[1]=='/');
   }
   else
   {
      if(cwd[0])
	 sprintf(newcwd,"%s/%s",cwd,path);
      else
	 strcpy(newcwd,path);
   }

   char	 *in;
   char	 *out;

   in=out=newcwd+prefix_size;

   while((in[0]=='.' && (in[1]=='/' || in[1]==0))
   || (in>newcwd && in[-1]=='/' && (in[0]=='/'
	 || (in[0]=='.' && in[1]=='.' && (in[2]=='/' || in[2]==0)))))
   {
      if(in[0]=='.' && in[1]=='.')
	 in++;
      in++;
      if(*in=='/')
	 in++;
   }

   for(;;)
   {
      if(in[0]=='/')
      {
	 // double slash or a slash on the end
	 if(in[1]=='/' || in[1]=='\0')
	 {
	    in++;
	    continue;
	 }
	 if(in[1]=='.')
	 {
	    // . - cur dir
	    if(in[2]=='/' || in[2]=='\0')
	    {
	       in+=2;
	       continue;
	    }
	    // .. - prev dir
	    if(in[2]=='.' && (in[3]=='/' || in[3]=='\0'))
	    {
	       if(last_element_is_doubledot(newcwd+prefix_size,out)
	       || out==newcwd
	       || (out==newcwd+prefix_size && out[-1]!='/'))
	       {
		  if(out>newcwd+prefix_size)
		     *out++='/';
		  *out++='.';
		  *out++='.';
	       }
	       else
		  while(out>newcwd+prefix_size && *--out!='/');
	       in+=3;
	       continue;
	    }
	 }
      }
      if((*out++=*in++)=='\0')
	 break;
   }
   Open(newcwd,CHANGE_DIR);
}

int   Ftp::ChdirStatus()
{
   int res=StateToError();
   if(res!=OK)
      return(res);

   if(mode!=CHANGE_DIR)
      return(OK);

   res=Read(NULL,0);
   if(res==SEE_ERRNO && errno==EAGAIN)
      return(IN_PROGRESS);

   if(res==0)
      res=OK;

   Close();
   return(res);
}

void  Ftp::AddResp(int exp,int fail, int (Ftp::*ck)(int,int),bool log)
{
   int newtail=RQ_tail+1;
   if(newtail>RQ_alloc)
   {
      if(RQ_head-0<newtail-RQ_alloc)
	 RespQueue=(struct expected_responce*)
	    xrealloc(RespQueue,(RQ_alloc=newtail+16)*sizeof(*RespQueue));
      memmove(RespQueue,RespQueue+RQ_head,(RQ_tail-RQ_head)*sizeof(*RespQueue));
      RQ_tail=0+(RQ_tail-RQ_head);
      RQ_head=0;
      newtail=RQ_tail+1;
   }
   RespQueue[RQ_tail].expect=exp;
   RespQueue[RQ_tail].fail_state=fail;
   RespQueue[RQ_tail].check_resp=ck;
   RespQueue[RQ_tail].log_resp=log;
   RQ_tail=newtail;
}

int   Ftp::CheckResp(int act)
{
   int ns=-1;

   if(RespQueueIsEmpty())
      return(-1);

   if(RespQueue[RQ_head].check_resp)
      ns=(this->*RespQueue[RQ_head].check_resp)(act,RespQueue[RQ_head].expect);
   if(ns==-1 && act!=RespQueue[RQ_head].expect)
      ns=RespQueue[RQ_head].fail_state;
   PopResp();
   return(ns);
}
void  Ftp::PopResp()
{
   if(RQ_head!=RQ_tail)
      RQ_head=RQ_head+1;
}

const char *Ftp::CurrentStatus()
{
   switch(state)
   {
   case(EOF_STATE):
   case(NO_FILE_STATE):
      if(control_sock!=-1)
      {
	 if(send_cmd_count>0)
	    return("Sending commands...");
	 if(!RespQueueIsEmpty())
	    return("Waiting for responce...");
	 return("Connection idle");
      }
      return("Not connected");
   case(INITIAL_STATE):
      if(hostname[0])
      {
	 time_t	t=time(NULL);
	 if(t-try_time<sleep_time)
	    return("Delaying before reconnect");
      }
   case(NO_HOST_STATE):
      return("Not connected");
   case(CONNECTING_STATE):
      return("Connecting...");
   case(WAITING_STATE):
      if(mode==STORE)
	 return("Waiting for transfer to complete");
      return("Waiting for responce...");
   case(ACCEPTING_STATE):
      return("Waiting for data connection...");
   case(READING_DATA_STATE):
   case(WRITING_DATA_STATE):
      if(data_sock!=-1)
         return("Data connection open");
      return("Waiting for transfer to complete");
   case(FATAL_STATE):
      return("Fatal protocol error occured");
   case(STORE_FAILED_STATE):
      return("Store failed - reput is needed");
   case(LOGIN_FAILED_STATE):
      return("Login failed");
   case(SYSTEM_ERROR_STATE):
      return(strerror(saved_errno));
   }
   return("");
}

int   Ftp::StateToError()
{
   switch(state)
   {
   case(NO_FILE_STATE):
      return(NO_FILE);
   case(NO_HOST_STATE):
      return(NO_HOST);
   case(FATAL_STATE):
      return(FATAL);
   case(STORE_FAILED_STATE):
      return(STORE_FAILED);
   case(LOGIN_FAILED_STATE):
      return(LOGIN_FAILED);
   case(SYSTEM_ERROR_STATE):
      return(SEE_ERRNO);
   default:
      return(OK);
   }
}

time_t	 Ftp::ConvertFtpDate(const char *s)
{
   struct tm tm;
   memset(&tm,0,sizeof(tm));

   int n=sscanf(s,"%4d%2d%2d%2d%2d%2d",&tm.tm_year,&tm.tm_mon,&tm.tm_mday,
				       &tm.tm_hour,&tm.tm_min,&tm.tm_sec);

   if(n!=6)
      return((time_t)-1);

   tm.tm_year-=1900;
   tm.tm_mon--;

   time_t t;

#ifdef HAVE_TM_ZONE
   t=mktime(&tm);
   tm.tm_sec+=tm.tm_gmtoff;
#elif defined HAVE_TZNAME
   tm.tm_sec-=timezone;
#endif

   t=mktime(&tm);

   return t;
}

void  Ftp::SetFlag(int flag,int val)
{
   flag&=MODES_MASK;  // only certain flags can be changed
   if(val)
      flags|=flag;
   else
      flags&=~flag;

   if(!(flags&SYNC_MODE))
      flags&=~SYNC_WAIT;   // if SYNC_MODE is off, we don't need to wait
}

void  Ftp::HomifyCWD()
{
   if(home[0] && cwd[0]=='~' && (cwd[1]==0 || cwd[1]=='/'))
   {
      memmove(cwd+strlen(home),cwd+1,strlen(cwd+1)+1);
      memmove(cwd,home,strlen(home));
   }
}

bool  Ftp::SameLocationAs(Ftp *o,int force)
{
   if(!strcmp(hostname,o->hostname) && port==o->port
   && !strcmp(user,o->user) && !strcmp(pass,o->pass)
   && !strcmp(group,o->group) && !strcmp(gpass,o->gpass)
   ){
      if(home[0] && !o->home[0])
	 strcpy(o->home,home);
      else if(!home[0] && o->home[0])
	 strcpy(home,o->home);

      if(strcmp(home,o->home))
	 return false;

      HomifyCWD();
      o->HomifyCWD();
      if(force==0 || state!=EOF_STATE)
	 return !strcmp(cwd,o->cwd);

      strcpy(cwd,o->cwd);
      char s[1050];
      sprintf(s,"CWD %s",cwd);
      SendCmd(s);
      AddResp(RESP_CWD_RMD_DELE_OK,INITIAL_STATE);
      FlushSendQueue();
      return true;
   }
   return false;
}

void  Ftp::Rename(const char *f,const char *f1)
{
   Close();
   strcpy(file,f);
   file1=xstrdup(f1);
   mode=RENAME;
}

int   Ftp::Done()
{
   int res=StateToError();
   if(res!=OK)
      return(res);

   if(mode==CLOSED)
      return OK;

   if(mode==RENAME || mode==ARRAY_INFO)
   {
      if(state==WAITING_STATE && RespQueueIsEmpty())
	 return(OK);
      return(IN_PROGRESS);
   }
   abort();
}

void  Ftp::GetInfoArray(struct fileinfo *info,int count)
{
   Close();
   array_for_info=info;
   array_ptr=0;
   array_cnt=count;
   mode=ARRAY_INFO;
}
