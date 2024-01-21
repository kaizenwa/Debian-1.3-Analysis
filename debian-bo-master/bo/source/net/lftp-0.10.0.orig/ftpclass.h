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

#ifndef FTPCLASS_H
#define FTPCLASS_H

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <time.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <poll.h>

#include "SMTask.h"

extern	 h_errno;

class Ftp : public SMTask
{
public:
   enum open_mode
   {
      CLOSED,
      RETRIEVE,
      STORE,
      LONG_LIST,
      LIST,
      SIZE,
      DATE,
      CHANGE_DIR,
      MAKE_DIR,
      REMOVE_DIR,
      REMOVE,
      SITE_CMD,
      RENAME,
      ARRAY_INFO
   };

   struct fileinfo
   {
      char *file;
      long size;
      time_t time;
      bool get_size;
      bool get_time;
   };

private:
   enum automate_state
   {
      EOF_STATE,	   // control connection is open, idle state
      INITIAL_STATE,	   // all connections are closed
      CONNECTING_STATE,	   // we are connecting the control socket
      WAITING_STATE,	   // we're waiting for a responce with data
      ACCEPTING_STATE,	   // we're waiting for an incoming data connection
      READING_DATA_STATE,  // data connection is open, we're waiting for data
      WRITING_DATA_STATE,  // ditto for writing
      FATAL_STATE,	   // fatal error occured (e.g. REST not implemented)
      NO_FILE_STATE,	   // file access is impossible - no such file
      NO_HOST_STATE,	   // host not found or not specified
      STORE_FAILED_STATE,  // STOR failed - you have to reput
      LOGIN_FAILED_STATE,  // login failed due to invalid password
      SYSTEM_ERROR_STATE   // system error occured, errno saved to saved_errno
   };

   enum responce
   {
      RESP_READY=220,
      RESP_PASS_REQ=331,
      RESP_LOGGED_IN=230,
      RESP_CWD_RMD_DELE_OK=250,
      RESP_TYPE_OK=200,
      RESP_PORT_OK=200,
      RESP_REST_OK=350,
      //RESP_DATA_OPEN=150,
      RESP_TRANSFER_OK=226,
      RESP_RESULT_HERE=213,
      RESP_PWD_MKD_OK=257,
      RESP_NOT_IMPLEMENTED=502,
      RESP_NOT_UNDERSTOOD=500,
      RESP_NO_FILE=550,
      RESP_PERM_DENIED=553,
      RESP_BROKEN_PIPE=426,
      RESP_LOGIN_FAILED=530
   };

   struct expected_responce
   {
      int   expect;
      int   fail_state;
      int   (Ftp::*check_resp)(int actual,int expect);
      bool  log_resp;
   }
      *RespQueue;
   int	 RQ_alloc;   // memory allocated

   int	 RQ_head;
   int	 RQ_tail;

   void	 LogResp(char *line);

   void  AddResp(int exp,int fail,int (Ftp::*ck)(int,int)=0,bool log=false);
   int   CheckResp(int resp);
   void  PopResp();
   void	 EmptyRespQueue() { RQ_head=RQ_tail=0; }
   int   RespQueueIsEmpty() { return RQ_head==RQ_tail; }
   int	 RespQueueSize() { return RQ_tail-RQ_head; }

   int	 IgnoreCheck(int act,int exp);
   int	 NotImplementedCheck(int act,int exp);
   int   NoFileCheck(int act,int exp);
   int	 TransferCheck(int,int);
   int	 LoginCheck(int,int);
   int	 NoPassReqCheck(int,int);
   int	 CatchPWDResponce(int,int);
   int	 CatchHomePWD(int,int);
   char  *ExtractPWD();
   int	 RNFR_Check(int,int);
   int	 CatchDATE(int,int);
   int	 CatchSIZE(int,int);

   void	 InitFtp();

   int   control_sock;
   int   data_sock;
   char  hostname[256];
   int   port;
   char  user[32];
   char  pass[256];
   char	 group[32];
   char	 gpass[9];

   open_mode   mode;

   /* type of transfer: TYPE_A or TYPE_I */
   int   type;

   /* address to connect */
   struct sockaddr_in   peer_sa;
   /* address for data accepting */
   struct sockaddr_in   data_sa;

   char  resp[1024];
   int   resp_size;
   char  line[1024];

   char  cwd[1024];
   char  home[1024];
   void	 HomifyCWD();
   char  file[1024];
   char	 *file1;
   long  pos;
   long	 real_pos;

   /* this is intended to prevent too often trys */
   time_t   try_time;

   time_t   event_time; // time of last (poll) event seen

   char	 *result;
   int	 result_size;

   void  DataClose();

   void	 SwitchToState(automate_state);

   int   SendCmd(const char *cmd);
   int	 FlushSendQueue();

   int	 ReceiveResp();
	 // If a responce is received, it checks it for accordance with
	 // responce_queue and switch to a state if necessary

   void  DebugPrint(const char *prefix,const char *text,int level=0);
   int   CheckHangup(struct pollfd *pfd,int num);
	 // Checks for error conditions on given descriptors
	 // and disconnects if found

   int	 Poll();
   int	 Poll(int fd,int ev);

   int	 AbsolutePath(const char *p);

   char	 *send_cmd_buffer;
   int   send_cmd_count;   // in buffer
   int	 send_cmd_alloc;   // total allocated
   char  *send_cmd_ptr;	   // start

   automate_state state;

   FILE	 *debug_file;
   int	 debug_level;

   char	 *last_error_resp;

   int	 sleep_time;
   int	 timeout;

   char	 anon_pass[256];

   int	 flags;

   int	 saved_errno;

   int	 StateToError();

   fileinfo *array_for_info;
   int	 array_ptr;
   int	 array_cnt;

public:
   Ftp();
   Ftp(const Ftp *);
   ~Ftp();

   Ftp *Clone() { return new Ftp(this); }

   bool	 SameLocationAs(Ftp *,int force_level=0);

   int	 GetState() { return state; }

   const Ftp&  operator=(const Ftp&);

   int	 IsConnected() // returns 1 when the connection is established
   {
      if(control_sock==-1)
	 return 0;
      Do();
      return(control_sock!=-1 && state!=CONNECTING_STATE);
   }

   int	 IsClosed() { return mode==CLOSED; }
   int	 IsOpen() { return !IsClosed(); }

   int	 Connect(const char *host,int port=21,const struct sockaddr *sa=0);
	 // when sa is specified, port and host are used for printing only
   int   Login(const char *user,const char *pass);
   int	 AnonymousLogin();
   int	 GroupLogin(const char *group,const char *pass);

   int   Open(const char *file,open_mode mode=RETRIEVE,long offs=0L);
   int   Read(void *buf,int size);
   int   Write(const void *buf,int size);
   int   Close();

	 // When you are putting a file, call SendEOT to terminate
	 // transfer and then call StoreStatus until OK or error.
   int	 SendEOT();
   int	 StoreStatus();

   int	 Block();
   int	 DataReady();
   int	 Do();
   int	 GetPollVector(struct pollfd *,int *);
   void  Disconnect();

   void	 Sleep();
   void	 DontSleep() { try_time=0; }
   void	 SetSleep(int sec) { sleep_time=sec; }
   int	 GetSleep() { return sleep_time; }

   void	 SetTimeout(int sec) { timeout=sec; }
   int	 GetTimeout() { return timeout; }

   void	 SetFlag(int flag,int val);
   int	 GetFlag(int flag) { return flags&flag; }

   static time_t ConvertFtpDate(const char *);

   const char *StrError(int res);
   const char *CurrentStatus();

   void	 Chdir(const char *path);
   int	 ChdirStatus();

   void	 Rename(const char *file,const char *to);
   void	 GetInfoArray(struct fileinfo *info,int count);
   int	 Done();

   const char  *GetCwd() { return cwd; }
   const char  *GetHostName() { return hostname; }
   const char  *GetUser() { return user; }
   int	 GetPort() { return port; }

   void	 SetDebug(FILE *new_debug_file,int new_debug_level /*[0-9]*/)
   {
      debug_file=new_debug_file;
      debug_level=new_debug_level;
   }

   enum status
   {
      IN_PROGRESS=1,	// is returned only by ChdirStatus() or Done()
      OK=0,
      SEE_ERRNO=-1,
      SEE_H_ERRNO=-2,
      NOT_OPEN=-3,
      NO_FILE=-4,
      NO_HOST=-5,
      FATAL=-7,
      STORE_FAILED=-9,
      LOGIN_FAILED=-10
   };

   enum flag_mask
   {
      SYNC_MODE=1,
      SYNC_WAIT=2,
      NOREST_MODE=4,
      IO_FLAG=8,     // this flag is set in Read/Write when data is actually
		     // transferred
      DOSISH_PATH=16,

      MODES_MASK=SYNC_MODE|NOREST_MODE|DOSISH_PATH
   };

   void CopyOptions(Ftp *f)
   {
      flags=(flags&~MODES_MASK)|(f->flags&MODES_MASK);
   }
};

#endif /* FTPCLASS_H */
