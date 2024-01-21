#include "config.h"

#include <stdio.h>
#include <string.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <stdarg.h>
#include <slang.h>
#include "jdmacros.h"

#include "sltcp.h"

#include "nntpcodes.h"
#include "nntplib.h"
#include "util.h"
#include "ttymsg.h"

void (*NNTP_Connection_Lost_Hook) (NNTP_Type *);
int (*NNTP_Authorization_Hook) (char *, char **, char **);

FILE *NNTP_Debug_Fp;

static int _nntp_connect_server (NNTP_Type *);

static NNTP_Type *nntp_allocate_nntp (void)
{
   static NNTP_Type nn;
   NNTP_Type *s;
   
   s = &nn;
   
   memset ((char *) s, 0, sizeof (NNTP_Type));
   s->can_xover = -1;
   s->can_xhdr = -1;
   s->can_xpat = -1;
   s->can_xgtitle = -1;
   
   return s;
}

static void _nntp_deallocate_nntp (NNTP_Type *s)
{
   if (s == NULL) return;
   
   memset ((char *) s, 0, sizeof (NNTP_Type));
}

int nntp_fgets_server (NNTP_Type *s, char *buf, unsigned int len)
{
   if ((s == NULL) || (s->init_state <= 0))
     return -1;

   *buf = 0;
   
   if (-1 == sltcp_fgets (s->tcp, buf, len))
     {
	(void) nntp_disconnect_server (s);
	return -1;
     }

   if (NNTP_Debug_Fp != NULL)
     fprintf (NNTP_Debug_Fp, "<%s", buf);

   return 0;
}

int nntp_fputs_server (NNTP_Type *s, char *buf)
{
   if ((s == NULL) || (s->init_state == 0))
     return -1;
   
   if (NNTP_Debug_Fp != NULL)
     fprintf (NNTP_Debug_Fp, ">%s\n", buf);

   if (-1 == sltcp_fputs (s->tcp, buf))
     {
	(void) nntp_disconnect_server (s);
	return -1;
     }
   
   return 0;
}

int nntp_write_server (NNTP_Type *s, char *buf, unsigned int n)
{
   if ((s == NULL) || (s->init_state == 0))
     return -1;
   
   if (NNTP_Debug_Fp != NULL)
     {
	unsigned int i;
	fputc ('>', NNTP_Debug_Fp);
	for (i = 0; i < n; i++)
	  fputc (buf[i], NNTP_Debug_Fp);
	fputc ('\n', NNTP_Debug_Fp);
     }
   
   if (n != sltcp_write (s->tcp, buf, n))
     {
	(void) nntp_disconnect_server (s);
	return -1;
     }
   
   return 0;
}


int nntp_gets_server (NNTP_Type *s, char *buf, unsigned int len)
{
   if (-1 == nntp_fgets_server (s, buf, len))
     return -1;
   
   len = strlen (buf);
   if (len && (buf[len - 1] == '\n'))
     {
	len--;
	buf[len] = 0;
	if (len && (buf[len - 1] == '\r'))
	  buf[len - 1] = 0;
     }
   
   return 0;
}

int nntp_puts_server (NNTP_Type *s, char *buf)
{
   if ((-1 == nntp_fputs_server (s, buf))
       || (-1 == nntp_fputs_server (s, "\r\n"))
       || (-1 == sltcp_flush_output (s->tcp)))
     {
	nntp_disconnect_server (s);
	return -1;
     }

   return 0;
}


static void _nntp_error_response (NNTP_Type *s, char *fmt)
{
   slrn_error ("%s", fmt);
   slrn_error ("Reason: %s", s->rspbuf);
}

static int _nntp_try_parse_timeout (char *str)
{
   /* I know of only two timeout responses:
    * 503 Timeout
    * 503 connection timed out
    * 
    * Here the idea is to look for 'time' and then 'out'.
    */
   static SLRegexp_Type re;
   unsigned char compiled_pattern_buf[256];
   
   re.pat = (unsigned char *) "time.*out";
   re.buf = compiled_pattern_buf;
   re.case_sensitive = 0;
   re.buf_len = sizeof (compiled_pattern_buf);
   
   (void) SLang_regexp_compile (&re);
   if (NULL == SLang_regexp_match ((unsigned char *) str, strlen (str), &re))
     return -1;
   
   return 0;
}

   
int nntp_get_server_response (NNTP_Type *s)
{
   int status;
   
   if ((s == NULL) || (s->init_state <= 0))
     return -1;
   
   if (-1 == nntp_gets_server (s, s->rspbuf, NNTP_RSPBUF_SIZE))
     return -1;
	
   status = atoi (s->rspbuf);

   if (((status == ERR_FAULT) && (-1 != _nntp_try_parse_timeout (s->rspbuf)))
       || (status == ERR_GOODBYE))
     {
	nntp_disconnect_server (s);
	status = -1;
     }
   
   s->code = status;
   return status;
}

   
static int _nntp_reconnect (NNTP_Type *s)
{
   nntp_disconnect_server (s);
   if (0 == (s->flags & NNTP_RECONNECT_OK))
     {
	if (NNTP_Connection_Lost_Hook != NULL)
	  (*NNTP_Connection_Lost_Hook) (s);
	return -1;
     }
   
   s->flags &= ~NNTP_RECONNECT_OK;
   
   slrn_message_now ("Server %s dropped connection.  Reconnecting...", s->host);
   
   if (-1 == _nntp_connect_server (s))
     {
	if (NNTP_Connection_Lost_Hook != NULL)
	  (*NNTP_Connection_Lost_Hook) (s);
	
	s->flags |= NNTP_RECONNECT_OK;
	return -1;
     }

   if ((s->group_name[0] != 0)
       && (-1 == nntp_server_vcmd (s, "GROUP %s", s->group_name)))
     {
	if (NNTP_Connection_Lost_Hook != NULL)
	  (*NNTP_Connection_Lost_Hook) (s);
	
	s->flags |= NNTP_RECONNECT_OK;
	return -1;
     }
   
   s->flags |= NNTP_RECONNECT_OK;
   return 0;
}

int nntp_start_server_cmd (NNTP_Type *s, char *cmd)
{
   int max_tries = 3;
   
   if (s == NULL)
     return -1;
   
   while ((s->init_state != 0) && max_tries 
	  && (SLKeyBoard_Quit == 0))
     {
	if (-1 != nntp_puts_server (s, cmd))
	  return 0;
	
	if (-1 == _nntp_reconnect (s))
	  return -1;
	
	max_tries--;
     }
   
   return -1;
}

int nntp_server_cmd (NNTP_Type *s, char *cmd)
{
   int max_tries = 3;
   
   do
     {
	int code; 
	
	if (-1 == nntp_start_server_cmd (s, cmd))
	  return -1;
   
	if (-1 != (code = nntp_get_server_response (s)))
	  return code;

	max_tries--;
     }
   while (max_tries && (s->flags & NNTP_RECONNECT_OK));
   
   return -1;
}

int nntp_server_vcmd (NNTP_Type *s, char *fmt, ...)
{
   char buf [NNTP_MAX_CMD_LEN];
   va_list ap;
   
   va_start (ap, fmt);
   vsprintf (buf, fmt, ap);
   va_end (ap);
   
   return nntp_server_cmd (s, buf);
}

int nntp_start_server_vcmd (NNTP_Type *s, char *fmt, ...)
{
   char buf [NNTP_MAX_CMD_LEN];
   va_list ap;
   
   va_start (ap, fmt);
   vsprintf (buf, fmt, ap);
   va_end (ap);
   
   return nntp_start_server_cmd (s, buf);
}

   

int nntp_close_server (NNTP_Type *s)
{
   if (s == NULL)
     return -1;
   
   if (s->init_state <= 0)
     return 0;
   
   s->init_state = -1;		       /* closing */
   
   (void) nntp_puts_server (s, "QUIT");
   (void) sltcp_close (s->tcp);
   
   s->tcp = NULL;
   s->init_state = 0;
   
   _nntp_deallocate_nntp (s);
   
   return 0;
}


static int _nntp_authorization (NNTP_Type *s)
{
   char *name, *pass;
   
   if (NNTP_Authorization_Hook == NULL)
     return 0;
   
   if (-1 == (*NNTP_Authorization_Hook) (s->host, &name, &pass))
     return -1;
   
   if ((name == NULL) || (pass == NULL))
     return 0;
   
   slrn_message_now ("Authenticating %s ...", name);
   
   if (-1 == nntp_server_vcmd (s, "AUTHINFO USER %s", name))
     return -1;
   
   if (s->code != NEED_AUTHDATA)
     return 0;
   
   if (-1 == nntp_server_vcmd (s, "AUTHINFO PASS %s", pass))
     return -1;
   
   switch (s->code)
     {
      case ERR_ACCESS:
	_nntp_error_response (s, "Authorization failed.");
	return -1;
      case OK_AUTH:
	s->can_post = 1;
	break;
     }
   return 0;
}

static int _nntp_connect_server (NNTP_Type *s)
{
   if (s->tcp != NULL)
     sltcp_close (s->tcp);
   
   slrn_message_now ("Connecting to host %s ...", s->host);
   
   if (NULL == (s->tcp = sltcp_open_connection (s->host, s->port)))
     return -1;

   s->init_state = 1;
   
   /* Read logon message. */
   switch (nntp_get_server_response (s))
     {
      case OK_CANPOST:
	s->can_post = 1;
	break;
	
      case OK_NOPOST:
	s->can_post = 0;
	break;
	
      default:
	goto failed;
     }
   
   
   if ((-1 == nntp_server_cmd (s, "MODE READER"))
       || (ERR_ACCESS == s->code)
       || (-1 == _nntp_authorization (s)))
     goto failed;
   
   
   slrn_message_now ("Connected to host.  Posting %sOk.",
		     (s->can_post ? "" : "NOT "));
   return 0;
   
   failed:
   
   _nntp_error_response (s, "Failed to initialize server");
   (void) sltcp_close (s->tcp);
   s->tcp = NULL;
   return -1;
}




/* This mucks with the hostname buffer.  */
static int _nntp_port_change (char *hostname, int *port)
{
   char *colon;
   
   colon = slrn_strchr (hostname, ':');
   if (colon == NULL) return -1;
   
   *colon++ = 0;
   
   *port = atoi (colon);
   return 0;
}

#ifndef NNTPSERVER_FILE
# define NNTPSERVER_FILE NULL
#endif

static char *_nntp_getserverbyfile (char *file)
{
   FILE *fp;
   char *host;
   static char buf[256];
   
   host = getenv("NNTPSERVER");
   
   if (host != NULL)
     {
	strcpy (buf, host);
	return buf;
     }
   
   if (file == NULL)
     {
#ifdef NNTPSERVER_NAME
	strcpy (buf, NNTPSERVER_NAME);
	return buf;
#else
	return NULL;
#endif
     }
   
   if (NULL == (fp = fopen(file, "r")))
     return NULL;
   
   while (NULL != fgets(buf, sizeof (buf), fp))
     {
	char *b;
	
	b = slrn_skip_whitespace (buf);
	if ((*b == 0) || (*b == '#'))
	  continue;
	  
	slrn_trim_string (b);
	(void) fclose(fp);
	return b;
     }
   
   (void) fclose(fp);
   return NULL;    /* No entry */
}


char *nntp_get_server_name (void)
{
   char *host;
   
   if (NULL != (host = _nntp_getserverbyfile(NNTPSERVER_FILE)))
     return host;
	
   fprintf (stderr, "You need to set the NNTPSERVER environment variable to your server name.\r\n");
#ifdef VMS
   fprintf (stderr, "Example: $ define/job NNTPSERVER my.news.server\r\n");
#else
# ifdef __os2__
   fprintf (stderr, "Example: set NNTPSERVER=my.news.server\r\n");
# else
   fprintf (stderr, "Example (csh): setenv NNTPSERVER my.news.server\r\n");
# endif
#endif
   return NULL;
}


NNTP_Type *nntp_open_server (char *host, int port)
{
   NNTP_Type *s;

#if 0
   if (NNTP_Debug_Fp == NULL)
     NNTP_Debug_Fp = fopen ("nntp.log", "w");
#endif
   
   if (host == NULL)
     {
	host = nntp_get_server_name ();
	if (host == NULL)
	  return NULL;
     }
   
   (void) _nntp_port_change (host, &port);
   
   if (port < 0)
     {
	port = sltcp_map_service_to_port ("nntp");
	if (port == -1)
	  port = 119;
     }
   
   if (NULL == (s = nntp_allocate_nntp ()))
     return NULL;   
   
   strncpy (s->host, host, NNTP_MAX_HOST_LEN);
   s->host [NNTP_MAX_HOST_LEN] = 0;
   s->port = port;
   
   if (-1 == _nntp_connect_server (s))
     {
	_nntp_deallocate_nntp (s);
	return NULL;
     }

   return s;
}

int nntp_read_line (NNTP_Type *s, char *buf, unsigned int len)
{
   if (-1 == nntp_gets_server (s, buf, len))
     return -1;
   
   if ((buf[0] == '.') && (buf[1] == 0))
     return 0;
   
   return 1;
}

   
int nntp_discard_output (NNTP_Type *s)
{
   char buf [NNTP_BUFFER_SIZE];
   int status;
   
   while (1 == (status = nntp_read_line (s, buf, sizeof (buf))))
     continue;
   
   return status;
}


int nntp_reconnect_server (NNTP_Type *s)
{
   unsigned int flag;
   int status;
   
   if (s == NULL)
     return -1;
   
   nntp_disconnect_server (s);
   
   flag = s->flags & NNTP_RECONNECT_OK;
   s->flags |= NNTP_RECONNECT_OK;
   
   status = _nntp_reconnect (s);

   if (flag == 0) s->flags &= ~NNTP_RECONNECT_OK;
   
   return status;
}

int nntp_check_connection (NNTP_Type *s)
{
   if ((s == NULL) || (s->tcp == NULL) || (s->tcp->tcp_fd == -1))
     return -1;
   
   return 0;
}

void nntp_disconnect_server (NNTP_Type *s)
{
   if (s == NULL) return;
   
   sltcp_close_socket (s->tcp);
}

static int _nntp_probe_server (NNTP_Type *s, char *cmd)
{
   if (-1 == nntp_server_cmd (s, cmd))
     return -1;
   
   if (ERR_COMMAND == s->code)
     return 0;
   
   return 1;
}

#define PROBE_XCMD(s, var,cmd) (((var) != -1) ? (var) : ((var) = _nntp_probe_server ((s),(cmd))))

int nntp_has_cmd (NNTP_Type *s, char *cmd)
{
   /* DNEWS, in my opinion does not handle XHDR probing properly. 
    * Sigh.
    */
   if (!strcmp (cmd, "XHDR"))
     {
	int ret;
	
	if (s->can_xhdr != -1)
	  return s->can_xhdr;
	
	if (-1 == (ret = nntp_server_cmd (s, cmd)))
	  return -1;

	if (ret == ERR_COMMAND)
	  {
	     s->can_xhdr = 0;
	     return 0;
	  }
	
	s->can_xhdr = 1;
	
	if (ret == OK_HEAD)  /* DNEWS bug?? */
	  (void) nntp_discard_output (s);
	
	return 1;
     }
   
	     
   if (!strcmp (cmd, "XPAT"))
     return PROBE_XCMD(s, s->can_xpat, cmd);

   if (!strcmp (cmd, "XGTITLE"))
     return PROBE_XCMD(s, s->can_xgtitle, cmd);

   if (!strcmp (cmd, "XOVER"))
     return PROBE_XCMD(s, s->can_xover, cmd);

   return _nntp_probe_server (s, cmd);
}

static int _nntp_num_or_msgid_cmd (NNTP_Type *s, char *cmd, int n, char *msgid)
{
   if (n != -1)
     return nntp_server_vcmd (s, "%s %d", cmd, n);
   else
     {
	if (msgid == NULL) msgid = "";
	return nntp_server_vcmd (s, "%s %s", cmd, msgid);
     }
}


int nntp_head_cmd (NNTP_Type *s, int n, char *msgid, int *real_id)
{
   int status;
   
   status = _nntp_num_or_msgid_cmd (s, "HEAD", n, msgid);
   if ((status == OK_HEAD) && (real_id != NULL))
     *real_id = atoi(s->rspbuf + 4);
   return status;
}

int nntp_xover_cmd (NNTP_Type *s, int min, int max)
{
   return nntp_server_vcmd (s, "XOVER %d-%d", min, max);
}

int nntp_article_cmd (NNTP_Type *s, int n, char *msgid)
{
   return _nntp_num_or_msgid_cmd (s, "ARTICLE", n, msgid);
}

int nntp_next_cmd (NNTP_Type *s, int *n)
{
   int status;
   
   if ((OK_NEXT == (status = nntp_server_cmd (s, "NEXT")))
       && (n != NULL))
     *n = atoi (s->rspbuf + 4);

   return status;
}

int nntp_select_group (NNTP_Type *s, char *name, int *minp, int *maxp)
{
   int estim;
   int min, max;
   
   if (-1 == nntp_server_vcmd (s, "GROUP %s", name))
     return -1;
   
   switch (s->code)
     {
      case OK_GROUP:
	
	if (3 != sscanf(s->rspbuf + 4, "%d %d %d", &estim, &min, &max))
	  return -1;
	
	if (minp != NULL) *minp = min;
	if (maxp != NULL) *maxp = max;

	strcpy (s->group_name, name);
	break;

      case ERR_ACCESS:
      default:
	break;
     }
   
   return s->code;
}


int nntp_post_cmd (NNTP_Type *s)
{   
   return _nntp_num_or_msgid_cmd (s, "POST", -1, NULL);
}

int nntp_end_post (NNTP_Type *s)
{
   if (-1 == nntp_puts_server (s, "."))
     return -1;
   
   return nntp_get_server_response (s);
}

int nntp_xpat_cmd (NNTP_Type *s, char *hdr, int rmin, int rmax, char *pat)
{
   if (0 == PROBE_XCMD(s, s->can_xpat, "XPAT"))
     return ERR_COMMAND;
   
   return nntp_server_vcmd (s, "XPAT %s %d-%d *%s*", hdr, rmin, rmax, pat);
}

int nntp_xgtitle_cmd (NNTP_Type *s, char *pattern)
{
   if (s->can_xgtitle == 0) 
     return -1;
   
   /* XGTITLE appears broken on some servers.  So, do not probe for it. */
   slrn_message_now ("Sending Query to Server ...");

   if (-1 == nntp_server_vcmd (s, "XGTITLE %s", pattern))
     return -1;
   
   if (s->code != OK_XGTITLE)
     {
	slrn_message ("Server does not support XGTITLE command.");
	s->can_xgtitle = 0;
	return ERR_COMMAND;
     }
   
   s->can_xgtitle = 1;
   return OK_XGTITLE;
}


/* hdr should not include ':' */
int nntp_xhdr_cmd (NNTP_Type *s, char *hdr, int num, char *buf, unsigned int buflen)
{
   char tmpbuf[1024];
   int found;
   unsigned int colon;
   int status;
   
   status = PROBE_XCMD(s, s->can_xhdr, "XHDR");
   if (status == -1)
     return -1;
   
   if (status == 1)
     {
	char *b, ch;
	
	if (-1 == nntp_server_vcmd (s, "XHDR %s %d", hdr, num))
	  return -1;
	
	if (OK_HEAD != s->code)
	  {
	     /* It comes as no surprise that Micro$oft apparantly makes 
	      * buggy servers too.  Sigh.
	      */
	     if (s->code != 224) return -1;
	     s->code = OK_HEAD;
	  }
	
	status = nntp_read_line (s, tmpbuf, sizeof(tmpbuf));
	if (status != 1)
	  return -1;
	
	/* skip past article number */
	b = tmpbuf;
	while (((ch = *b++) >= '0') && (ch <= '9'))
	  ;
	strncpy (buf, b, buflen - 1);
	buf[buflen - 1] = 0;
	
	/* I should handle multi-line returns but I doubt that there will be
	 * any for our use of xhdr
	 */
	(void) nntp_discard_output (s);
	return 0;
     }
   
   if (-1 == nntp_head_cmd (s, num, NULL, NULL))
     return -1;
   
   if (s->code != OK_HEAD)
     return -1;
   
   found = 0;
   colon = strlen (hdr);

   while (1 == (status = nntp_read_line (s, tmpbuf, sizeof (tmpbuf))))
     {
	char *b;
	
	if (found
	    || slrn_case_strncmp ((unsigned char *) tmpbuf, (unsigned char *) hdr, colon)
	    || (tmpbuf[colon] != ':'))
	  continue;
	
	found = 1;
	
	b = tmpbuf + (colon + 1);      /* skip past colon */
	if (*b == ' ') b++;
	strncpy (buf, b, buflen - 1);
	buf[buflen - 1] = 0;
     }
   
   return status;
}


int nntp_list_newsgroups (NNTP_Type *s)
{
   return _nntp_num_or_msgid_cmd (s, "LIST", -1, "NEWSGROUPS");
}

int nntp_list_active_cmd (NNTP_Type *s)
{
   return _nntp_num_or_msgid_cmd (s, "LIST", -1, NULL);
}


int nntp_listgroup (NNTP_Type *s, char *group)
{
   return nntp_server_vcmd (s, "LISTGROUP %s", group);
   /*  OK_GROUP desired */
}

int nntp_body_cmd (NNTP_Type *s, int n, char *msgid)
{
   return _nntp_num_or_msgid_cmd (s, "BODY", n, msgid);
}


char *nntp_read_and_malloc (NNTP_Type *s)
{
   char line [NNTP_BUFFER_SIZE];
   char *mbuf;
   unsigned int buffer_len, buffer_len_max;
   int status;
   
   mbuf = NULL;
   buffer_len_max = buffer_len = 0;
   
   while (1 == (status = nntp_read_line (s, line, sizeof(line))))
     {
	unsigned int len;
	
	len = strlen (line);
	
	if (len + buffer_len + 4 > buffer_len_max)
	  {
	     char *new_mbuf;
	     
	     buffer_len_max += 4096 + len;
	     new_mbuf = slrn_realloc (mbuf, buffer_len_max, 0);
	     
	     if (new_mbuf == NULL)
	       {
		  slrn_free (mbuf);
		  nntp_discard_output (s);
		  return NULL;
	       }
	     mbuf = new_mbuf;
	  }
   
	strcpy (mbuf + buffer_len, line);
	buffer_len += len;
	mbuf [buffer_len++] = '\n';
	mbuf [buffer_len] = 0;
     }

   if (status == 0)
     return mbuf;

   slrn_free (mbuf);
   return NULL;
}
