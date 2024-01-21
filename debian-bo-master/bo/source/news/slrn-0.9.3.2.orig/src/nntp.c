/*  Copyright (c) 1995 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <string.h>
#include <errno.h>
#include <slang.h>

#include "jdmacros.h"
#include "server.h"

#ifdef VMS
# include "vms.h"
#endif

int Slrn_Query_Reconnect = 1;

static Slrn_Server_Obj_Type NNTP_Server_Obj;
static Slrn_Post_Obj_Type NNTP_Post_Obj;

static NNTP_Type *NNTP_Server;
static int NNTP_Port = -1;
static char *NNTP_Server_Name;

/* If 1, abort.  If -1, abort unless keyboard quit. */
static int _NNTP_Abort_On_Disconnection;

static void _nntp_connection_lost_hook (NNTP_Type *s)
{
   if (_NNTP_Abort_On_Disconnection)
     {
	if ((_NNTP_Abort_On_Disconnection == -1) && SLKeyBoard_Quit)
	  return;

	slrn_exit_error ("Server connection to %s lost.  Cannot recover.", 
			 s->host);
     }
}


static int _nntp_interrupt_handler (void)
{
   (void) slrn_handle_interrupts ();

   if ((SLang_Error == USER_BREAK) || SLKeyBoard_Quit)
     return -1;

   return 0;
}

static void _nntp_close_server (void)
{
   NNTP_Type *s = NNTP_Server;
   
   NNTP_Server = NULL;
   
   if (s != NULL) nntp_close_server (s);
}

static int _nntp_initialize_server (void)
{
   NNTP_Authorization_Hook = slrn_get_authorization;
   SLTCP_Interrupt_Hook = _nntp_interrupt_handler;
   
   if (NNTP_Server != NULL)
     nntp_close_server (NNTP_Server);
   
   if (NULL == (NNTP_Server = nntp_open_server (NNTP_Server_Name, NNTP_Port)))
     return -1;
   
   if (-1 == nntp_has_cmd (NNTP_Server, "XOVER"))
     {
	_nntp_close_server ();
	return -1;
     }
   
   if (NNTP_Server->can_xover == 0)
     {
	slrn_message_now ("Server %s does not implement the XOVER command.", 
			  NNTP_Server->host);
	NNTP_Server_Obj.sv_has_xover = 0;
     }
   else NNTP_Server_Obj.sv_has_xover = 1;
   
   NNTP_Server->flags |= NNTP_RECONNECT_OK;

   NNTP_Connection_Lost_Hook = _nntp_connection_lost_hook;
   return 0;
}


static char *_nntp_read_line (char *buf, unsigned int len)
{
   int status;
   
   status = nntp_read_line (NNTP_Server, buf, len);
   
   if (status == 1)
     return buf;
   
   if (status == 0)
     {
	_NNTP_Abort_On_Disconnection = 0;
	return NULL;
     }

   /* Read fail or user break.  Either way, we have to shut it down. */
   _nntp_connection_lost_hook (NNTP_Server);
   return NULL;
}

static int _nntp_head_cmd (int id, char *msgid, int *real_idp)
{
   return nntp_head_cmd (NNTP_Server, id, msgid, real_idp);
}

static int _nntp_select_article (int n, char *msgid)
{
   return nntp_article_cmd (NNTP_Server, n, msgid);
}

static int _nntp_select_group (char *grp, int *min, int *max)
{
   return nntp_select_group (NNTP_Server, grp, min, max);
}

static int _nntp_put_server_cmd (char *cmd, char *buf, unsigned int len)
{
   int code;
   
   if (-1 != (code = nntp_server_cmd (NNTP_Server, cmd)))
     {
	strncpy (buf, NNTP_Server->rspbuf, len);
	if (len) buf[len - 1] = 0;
     }
   return code;
}

static int _nntp_xpat_cmd (char *hdr, int rmin, int rmax, char *pat)
{
   return nntp_xpat_cmd (NNTP_Server, hdr, rmin, rmax, pat);
}

static int _nntp_xhdr_cmd (char *hdr, int num, 
			   char *buf, unsigned int buflen)
{
   return nntp_xhdr_cmd (NNTP_Server, hdr, num, buf, buflen);
}

static int _nntp_xgtitle_cmd (char *pat)
{
   return nntp_xgtitle_cmd (NNTP_Server, pat);
}

static int _nntp_has_cmd (char *cmd)
{
   return nntp_has_cmd (NNTP_Server, cmd);
}

static int _nntp_list_newsgroups (void)
{
   return nntp_list_newsgroups (NNTP_Server);
}

static int _nntp_list_active (void)
{
   int code;
   
   code = nntp_list_active_cmd (NNTP_Server);
   
   if (OK_GROUPS == code)
     _NNTP_Abort_On_Disconnection = 1;

   return code;
}

static int _nntp_start_post (void)
{
   /* Make sure we're connected to a server -- won't be the case the first
    * time we post if we're reading news from a local spool.  In this case
    * a late connect is better as we don't bother the server until we need
    * to.
    */
   if (NNTP_Server == NULL)
     {
	if (-1 == _nntp_initialize_server ())
	  return -1;
     }

   return nntp_post_cmd (NNTP_Server);
}

static int _nntp_end_post (void)
{
   int status;
   
   status = nntp_end_post (NNTP_Server);
   
   if (status == -1)
     {
	slrn_error ("Error writing to server.");
	return -1;
     }
   
   if (NNTP_Server->code != OK_POSTED)
     {
	slrn_error ("Article rejected: %s", NNTP_Server->rspbuf);
	return -1;
     }

   return 0;
}


static int _nntp_po_puts (char *buf)
{
   char *b;
   
   /* make sure \n --> \r\n */
   b = buf;
   while (NULL != (b = slrn_strchr (buf, '\n')))
     {
	unsigned int len;
	
	len = (unsigned int) (b - buf);
	if ((-1 == nntp_write_server (NNTP_Server, buf, len))
	    || (-1 == nntp_write_server (NNTP_Server, "\r\n", 2)))
	  return -1;
	
	buf = b + 1;
     }
   
   return nntp_fputs_server (NNTP_Server, buf);
}

static int _nntp_po_printf (char *fmt, ...)
{
   va_list ap;
   char buf[NNTP_BUFFER_SIZE];
   
   va_start (ap, fmt);
   vsprintf (buf, fmt, ap);
   va_end (ap);
   
   return _nntp_po_puts (buf);
}

static int _nntp_xover_cmd (int min, int max)
{   
   int status;
   
   if (OK_XOVER == (status = nntp_xover_cmd (NNTP_Server, min, max)))
     {
	_NNTP_Abort_On_Disconnection = -1;
     }
   
   return status;
}


static int _nntp_next_cmd (int *id)
{
   return nntp_next_cmd (NNTP_Server, id);
}

static int nntp_init_objects (void)
{
   NNTP_Post_Obj.po_start = _nntp_start_post;
   NNTP_Post_Obj.po_end = _nntp_end_post;
   NNTP_Post_Obj.po_printf = _nntp_po_printf;
   NNTP_Post_Obj.po_puts = _nntp_po_puts;
   NNTP_Post_Obj.po_can_post = 1;
   
   NNTP_Server_Obj.sv_select_group = _nntp_select_group;
   NNTP_Server_Obj.sv_read_line = _nntp_read_line;
   NNTP_Server_Obj.sv_close = _nntp_close_server;
   NNTP_Server_Obj.sv_initialize = _nntp_initialize_server;
   NNTP_Server_Obj.sv_select_article = _nntp_select_article;
   NNTP_Server_Obj.sv_put_server_cmd = _nntp_put_server_cmd;
   NNTP_Server_Obj.sv_xpat_cmd = _nntp_xpat_cmd;
   NNTP_Server_Obj.sv_xhdr_command = _nntp_xhdr_cmd;
   NNTP_Server_Obj.sv_xgtitle_cmd = _nntp_xgtitle_cmd;
   NNTP_Server_Obj.sv_has_cmd = _nntp_has_cmd;
   NNTP_Server_Obj.sv_list_newsgroups = _nntp_list_newsgroups;
   NNTP_Server_Obj.sv_list_active = _nntp_list_active;
   
   NNTP_Server_Obj.sv_has_xover = 0;
   NNTP_Server_Obj.sv_nntp_xover = _nntp_xover_cmd;
   NNTP_Server_Obj.sv_nntp_head = _nntp_head_cmd;
   NNTP_Server_Obj.sv_nntp_next = _nntp_next_cmd;
   return 0;
}

static int _nntp_get_server_name (void)
{
   char *host;
   
   if (NULL == (host = NNTP_Server_Name))
     {
	host = nntp_get_server_name ();
	if (host == NULL)
	  return -1;
     }
   
   NNTP_Server_Name = NNTP_Server_Obj.sv_name = slrn_safe_strmalloc (host);
   return 0;
}


static int nntp_select_server_object (void)
{
   if (NNTP_Server_Obj.sv_select_group == NULL)
     nntp_init_objects ();
   
   Slrn_Server_Obj = &NNTP_Server_Obj;
   
   if (NNTP_Server_Obj.sv_name == NULL)
     return _nntp_get_server_name ();

   return 0;
}

static int nntp_select_post_object (void)
{
   if (NNTP_Post_Obj.po_start == NULL)
     nntp_init_objects ();
   
   Slrn_Post_Obj = &NNTP_Post_Obj;
   
   if (NNTP_Server_Obj.sv_name == NULL)
     return _nntp_get_server_name ();
   
   return 0;
}

static void nntp_usage (void)
{
   fputs ("--nntp options:\n\
-h nntp-host    Host name to connect to.  This overrides NNTPSERVER variable.\n\
-p NNTP-PORT    Set the NNTP port to NNTP-PORT. The default value is 119.\n\
                 Note: This option has no effect on some systems.\n\
--debug FILE    Write out dialog with server to FILE.\n\
",
	  stdout);
   exit (0);
}

/* returns number parsed */
static int nntp_parse_args (char **argv, int argc)
{
   int i;
   
   for (i = 0; i < argc; i++)
     {
	if (!strcmp (argv[i], "--help"))
	  nntp_usage ();
	else if (i + 1 < argc)
	  {
	     char *arg, *arg1;
	     
	     arg = argv[i];
	     arg1 = argv [i + 1];
	     if (!strcmp ("-p", arg))
	       {
		  NNTP_Port = atoi (arg1);
	       }
	     else if (!strcmp ("-h", arg))
	       {
		  NNTP_Server_Name = arg1;
	       }
	     else if (!strcmp ("--debug", arg))
	       {
		  if (NNTP_Debug_Fp != NULL)
		    fclose (NNTP_Debug_Fp);
		  NNTP_Debug_Fp = fopen (arg1, "w");
		  if (NNTP_Debug_Fp == NULL)
		    slrn_exit_error ("Unable to open %s for debugging.", arg1);
	       }
	     else break;

	     i++;
	  }
	else break;
     }
   
   return i;
}


