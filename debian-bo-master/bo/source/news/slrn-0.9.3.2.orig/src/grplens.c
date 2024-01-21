/* -*- mode: C; mode: fold -*- */
/*  Copyright (c) 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */

#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>
#include <string.h>

#if SLRN_HAS_GROUPLENS
/* Rest of file in this #if statement */

#include <slang.h>
#include <stdarg.h>

#include <ctype.h>

#include "jdmacros.h"

#include "slrn.h"
#include "group.h"
#include "art.h"
#include "misc.h"
#include "uudecode.h"
#include "grplens.h"
#include "sltcp.h"
#include "util.h"
#include "server.h"

/*{{{ Low level grouplens tcp stuff */



#define GL_MAX_RESPONSE_LINE_LEN	1024

/*{{{ GL_Type structure */

typedef struct
{
#define GL_MAX_HOSTNAME_LEN		255
   char hostname[GL_MAX_HOSTNAME_LEN + 1];
   int port;
   
   char *token;
   char *pseudoname;
   int logged_in;
   SLTCP_Type *tcp;
}
GL_Type;

static GL_Type GL_Server;

/*}}}*/

typedef struct
{
   char *msgid;
   float pred;
   float confhigh;
   float conflow;
}
GL_Prediction_Type;

typedef struct
{
   char *msgid;
   int rating;
   int saw_article;
}
GL_Rating_Type;


/*{{{ Error codes and Error functions */

static int GL_Error;
#define GL_ERROR_UNKNOWN	       -1
#define GL_ELOGIN_UNREGISTERED		1
#define GL_ELOGIN_BUSY			2
#define GL_ELOGIN_UNAVAILABLE		3
#define GL_ELOGIN_ALREADY_LOGGEDIN	4
#define GL_ERROR_PROPER_RESPONSE	6
#define GL_ERROR_MALLOC			7
#define GL_ESERVER_WRITE		8
#define GL_ESERVER_READ			9
#define GL_ERROR_TOKEN		       10
#define GL_ERROR_NEWSGROUP	       11

typedef struct
{
   char *err;
   unsigned int len;
   int errcode;
}
GL_Error_Type;



/*{{{ Utility functions */

static char *make_nstring (char *s, unsigned int len)
{
   char *s1;
   
   if (s == NULL) return s;
   
   if (NULL == (s1 = (char *) malloc (len + 1)))
     {
	GL_Error = GL_ERROR_MALLOC;
	return NULL;
     }
   strncpy (s1, s, len);
   s1[len] = 0;
   return s1;
}

static char *make_string (char *s)
{
   if (s == NULL) return s;
   return make_nstring (s, strlen (s));
}

static char *skip_whitespace (char *s)
{
   char ch;
   
   while (((ch = *s) != 0) && isspace(ch)) s++;
   return s;
}

static char *skip_nonwhitespace (char *s)
{
   char ch;
   
   while (((ch = *s) != 0) && (0 == isspace(ch))) s++;
   return s;
}

/*}}}*/

static char *gl_get_error (void)
{
   switch (GL_Error)
     {
      case GL_ELOGIN_UNREGISTERED: return "User is Unregistered";
      case GL_ELOGIN_BUSY: return "Service is Busy";
      case GL_ELOGIN_UNAVAILABLE: return "Service Unavailable";
      case GL_ELOGIN_ALREADY_LOGGEDIN: return "Already logged in";
      case GL_ERROR_PROPER_RESPONSE: return "Server failed to return proper response";
      case GL_ERROR_MALLOC: return "Not enough memory";
      case GL_ESERVER_WRITE: return "Error writing to server";
      case GL_ESERVER_READ: return "Error reading from server";
      case GL_ERROR_TOKEN: return "Token is invalid";
      case GL_ERROR_NEWSGROUP: return "Newsgroup not supported";
     }
   
   return "Unknown Error";
}


static void close_gl (GL_Type *gl)
{
   if (gl != NULL) sltcp_close (gl->tcp);
}

static int get_gl_response (GL_Type *gl, char *buf, unsigned int len)
{
   char *b;
   
   (void) len;
   
   if (-1 == sltcp_fgets (gl->tcp, buf, GL_MAX_RESPONSE_LINE_LEN))
     {
	GL_Error = GL_ESERVER_READ;
	close_gl (gl);
	return -1;
     }
   
   b = buf + strlen (buf);
   if (b != buf)
     {
	b--;
	if (*b == '\n') *b = 0;
	if (b != buf)
	  {
	     b--;
	     if (*b == '\r') *b = 0;
	  }
     }
   return 0;
}

static void handle_error_response (GL_Type *gl, GL_Error_Type *tbl)
{
   char *s, *s1;
   char buf [GL_MAX_RESPONSE_LINE_LEN];
   
   if (-1 == get_gl_response (gl, buf, sizeof (buf)))
     return;
	
   s = skip_whitespace (buf);
   
   GL_Error = GL_ERROR_UNKNOWN;
   
   while (*s != 0)
     {
	unsigned int len;
	GL_Error_Type *t;
	
	s1 = skip_nonwhitespace (s);
	
	len = s1 - s;
	
	t = tbl;
	while (t->err != NULL)
	  {
	     if ((t->len == len) 
		 && (0 == slrn_case_strncmp ((unsigned char *)s, (unsigned char *) t->err, len)))
	       {
		  GL_Error = t->errcode;
		  if (GL_Error == GL_ERROR_TOKEN)
		    {
		       if (gl->token != NULL)
			 free (gl->token);
		       gl->token = NULL;
		       gl->logged_in = 0;
		    }
		  break;
	       }
	     t++;
	  }
	s = skip_whitespace (s1);
     }
   
   close_gl (gl);
}
   
   
static int is_error (char *buf)
{
   if (!slrn_case_strncmp ((unsigned char *) buf, (unsigned char *) "ERROR", 5))
     return 1;
   
   return 0;
}

/*}}}*/


/*{{{ parse_keyword_eqs_value */

/* Scan buf looking for keyword=VALUE */
static int parse_keyword_eqs_value (char *buf, char *kw, char **value, unsigned int *len)
{
   char *b, *b1, *v, *v1;
   char ch;
   unsigned int kwlen;

   *value = NULL;

   kwlen = strlen (kw);
   
   b = buf;
   while (1)
     {
	b = skip_whitespace (b);
	if (*b == 0) return -1;
	
	b1 = b;
	while (((ch = *b1) != 0) && (0 == isspace (ch)) && (ch != '='))
	  b1++;
	
	if (ch == 0) return -1;
	if (ch != '=')
	  {
	     b = b1;
	     continue;
	  }
	
	v = b1 + 1;
	if (*v == '"')
	  {
	     v++;
	     v1 = v;
	     while (((ch = *v1) != 0) && (ch != '"')) v1++;
	  }
	else v1 = skip_nonwhitespace (v);
	
	if ((b + kwlen == b1) 
	    && (0 == slrn_case_strncmp ((unsigned char *) kw, (unsigned char *) b, kwlen)))
	  {
	     *value = v;
	     *len = v1 - v;
	     return 0;
	  }
	
	if (*v1 == '"') v1++;
	b = v1;
     }
}

/*}}}*/
/*{{{ low level GL server functions */



   
static int send_gl_line (GL_Type *gl, char *msg, int flush)
{
   if (msg == NULL) return 0;
   
   if ((-1 == sltcp_fputs (gl->tcp, msg))
       || (-1 == sltcp_fputs (gl->tcp, "\r\n"))
       || (flush && (-1 == sltcp_flush_output (gl->tcp))))
     {
	GL_Error = GL_ESERVER_WRITE;
	close_gl (gl);
	return -1;
     }
   
   return 0;
}

static int connect_to_gl_host (GL_Type *gl, char *cmd)
{
   char buf [GL_MAX_RESPONSE_LINE_LEN];
   
   GL_Error = 0;
   
   if (gl->hostname == NULL)
     {
	GL_Error = GL_ERROR_UNKNOWN;
	return -1;
     }
   
   if (NULL == (gl->tcp = sltcp_open_connection (gl->hostname, gl->port)))
     return -1;
   
   /* We should be able to read OK ...  If not, we failed to make proper 
    * connection.
    */

   if (-1 == get_gl_response (gl, buf, sizeof (buf)))
     return -1;
   
   if (0 != slrn_case_strncmp ((unsigned char *) buf, (unsigned char *) "OK", 2))
     {
	GL_Error = GL_ERROR_PROPER_RESPONSE;
	close_gl (gl);
	return -1;
     }
   
   return send_gl_line (gl, cmd, 1);
}


static int start_command (GL_Type *gl, char *fmt, ...)
{
   va_list ap;
   int ret;
   
   if (-1 == connect_to_gl_host (gl, NULL))
     return -1;
   
   va_start(ap, fmt);
   ret = sltcp_vfprintf (gl->tcp, fmt, ap);
   va_end (ap);
   
   if ((ret == -1)
       || (-1 == sltcp_fputs (gl->tcp, "\r\n")))
     {
	close_gl (gl);
	return -1;
     }
   
   return 0;
}



static int check_for_error_response (GL_Type *gl, GL_Error_Type *tbl)
{
   char response [GL_MAX_RESPONSE_LINE_LEN];
   
   if (-1 == get_gl_response (gl, response, sizeof (response)))
     return -1;
   
   if (is_error (response))
     {
	handle_error_response (gl, tbl);
	return -1;
     }
   return 0;
}

/* Open connection, send command check for error and handle it via table
 * if error occurs.
 */
static int gl_send_command (GL_Type *gl, GL_Error_Type *tbl, char *fmt, ...)
{
   va_list ap;
   int ret;
   
   if (-1 == connect_to_gl_host (gl, NULL))
     return -1;
   
   va_start(ap, fmt);
   ret = sltcp_vfprintf (gl->tcp, fmt, ap);
   va_end (ap);
   
   if ((ret == -1)
       || (-1 == sltcp_flush_output (gl->tcp)))
     {
	GL_Error = GL_ESERVER_WRITE;
	close_gl (gl);
	return -1;
     }
   
   if (tbl == NULL) return 0;

   return check_for_error_response (gl, tbl);
}
   


/*}}}*/

/*{{{ login functions */

static GL_Error_Type Login_Error_Table [] = 
{
   {":busy",		5,	GL_ELOGIN_BUSY},
   {":unavailable",	12,	GL_ELOGIN_UNAVAILABLE},
   {":unregistered",	13,	GL_ELOGIN_UNREGISTERED},
   {":alreadyLogin",	13,	GL_ELOGIN_ALREADY_LOGGEDIN},
   {NULL, 0, 0}
};



static int login (GL_Type *gl)
{
   char response [GL_MAX_RESPONSE_LINE_LEN];
   char *value; 
   unsigned int value_len;

   gl->logged_in = 0;
   
   if (-1 == gl_send_command (gl, Login_Error_Table, "LOGIN %s\r\n", gl->pseudoname))
     return -1;
   
   /* parse response to get token, etc... */
   if (-1 == get_gl_response (gl, response, sizeof (response)))
     return -1;
   
   /* CLose server then parse response. */
   close_gl (gl);
   
   /* expecting 1 or more fields of: 
    * :token=SOMETHING
    * :version=SOMETHING
    * :rkeys="BLA BLA BLA"
    * :pkeys="BLA BLA"
    */

   /* We only require token. */
   if (-1 == parse_keyword_eqs_value (response, ":token", &value, &value_len))
     {
	GL_Error = GL_ERROR_PROPER_RESPONSE;
	return -1;
     }
   
   if (NULL == (gl->token = make_nstring (value, value_len)))
     return -1;
   
   gl->logged_in = 1;

   return 0;
}

static int validate_token (GL_Type *gl)
{   
   if ((gl->token == NULL) || (gl->logged_in == 0))
     return login (gl);
   
   return 0;
}

/*}}}*/
/*{{{ logout functions */

static void logout (GL_Type *gl)
{
   if (gl->logged_in == 0) return;
   
   (void) gl_send_command (gl, NULL, "LOGOUT %s\r\n", gl->token);
   close_gl (gl);
}

/*}}}*/
/*{{{ gl_open_server */

static int gl_open_server (char *host, int port, char *pname)
{
   GL_Type *gl;
   
   if (host == NULL) return -1;
   if (port < 0) return -1;
   if (pname == NULL) return -1;
   
   gl = &GL_Server;
   memset ((char *) gl, 0, sizeof (GL_Type));
   
   if (NULL == (gl->pseudoname = make_string (pname)))
     return -1;
   
   strncpy (gl->hostname, host, GL_MAX_HOSTNAME_LEN);
   gl->hostname[GL_MAX_HOSTNAME_LEN] = 0;
   
   gl->port = port;
   
   if (-1 == login (gl))
     {
	free (gl->pseudoname);
	gl->pseudoname = NULL;
	
	return -1;
     }
   return 0;
}

/*}}}*/


static int gl_open_predictions (char *group)
{
   GL_Type *gl = &GL_Server;
   
   if (-1 == validate_token (gl))
     return -1;
       
   if (-1 == start_command (gl, "GetPredictions %s %s", gl->token, group))
     return -1;
   return 0;
}

static int terminate_command (GL_Type *gl, GL_Error_Type *tbl)
{
   if (-1 == send_gl_line (gl, ".", 1))
     return -1;
   
   return check_for_error_response (gl, tbl);
}

static int gl_want_prediction (char *msgid)
{
   GL_Type *gl;
   
   gl = &GL_Server;
   return send_gl_line (gl, msgid, 0);
}

static GL_Error_Type Predictions_Error_Table [] = 
{
   {":invalidToken",	13,	GL_ERROR_TOKEN},
   {":invalidGroup",	13,	GL_ERROR_NEWSGROUP},
   {NULL, 0, 0}
};

static int gl_close_predictions (void (*f) (GL_Prediction_Type *))
{  
   char buf [GL_MAX_RESPONSE_LINE_LEN];
   GL_Prediction_Type st;
   GL_Type *gl;

   gl = &GL_Server;
   
   if (-1 == terminate_command (gl, Predictions_Error_Table))
     {
	return -1;
     }
   
   
   while (-1 != get_gl_response (gl, buf, sizeof (buf)))
     {
	char *value;
	unsigned int value_len;
	int ok;
	
	ok = 0;
	if ((*buf == '.') && (buf[1] == 0))
	  {
	     close_gl (gl);
	     return 0;
	  }
	
	st.pred = -1.0;
	st.conflow = -1.0;
	st.confhigh = -1.0;

	if (0 == parse_keyword_eqs_value (buf, ":nopred", &value, &value_len))
	  continue;
	
	if ((0 == parse_keyword_eqs_value (buf, ":pred", &value, &value_len))
	    && (1 == sscanf (value, "%f", &st.pred)))
	  ok++;
	
	if ((0 == parse_keyword_eqs_value (buf, ":conflow", &value, &value_len))
	    && (1 == sscanf (value, "%f", &st.conflow)))
	  ok++;
	
	if ((0 == parse_keyword_eqs_value (buf, ":confhigh", &value, &value_len))
	    && (1 == sscanf (value, "%f", &st.confhigh)))
	  ok++;
	
	if (ok && (st.pred > 0.0)) 
	  {
	     char *b;
	     /* Now get message id.  It is first thing on line. */
	     st.msgid = skip_whitespace (buf);
	     b = skip_nonwhitespace (st.msgid);
	     *b = 0;
	     
	     if (*st.msgid == '<') (*f) (&st);
	  }
     }
   return -1;
}

static int gl_open_ratings (char *group)
{
   GL_Type *gl = &GL_Server;
   
   if (-1 == validate_token (gl))
     return -1;
   
   if (-1 == start_command (gl, "PutRatings %s %s", gl->token, group))
     return -1;
   return 0;
}

static int gl_put_rating (GL_Rating_Type *rt)
{
   char buf [GL_MAX_RESPONSE_LINE_LEN];
   GL_Type *gl;
   
   gl = &GL_Server;
   
   if (rt->rating <= 0) return 0;
   sprintf (buf, "%s :rating=%4.2f :sawHeader=1", rt->msgid, (float) rt->rating);
   if (rt->saw_article)
     strcat (buf + strlen (buf), " :sawArticle=1");
   
   return send_gl_line (gl, buf, 0);
}

static int gl_close_ratings (void)
{
   GL_Type *gl;
   
   gl = &GL_Server;
   
   if (-1 == terminate_command (gl, Predictions_Error_Table))
     return -1;
   
   close_gl (gl);
   return 0;
}

static void gl_close_server (void)
{
   GL_Type *gl;
   
   gl = &GL_Server;

   if (gl->logged_in == 0) 
     return;
   
   (void) logout (gl);
   
   if (gl->pseudoname != NULL)
     free (gl->pseudoname);
   
   if (gl->token != NULL)
     free (gl->token);
   
   memset ((char *)gl, 0, sizeof (GL_Type));
}

/*}}}*/

int Slrn_Use_Group_Lens = 0;

static void do_error (void)
{
   slrn_error ("Failed: %s", gl_get_error ());
}

char *Slrn_GroupLens_Host;
int Slrn_GroupLens_Port;
char *Slrn_GroupLens_Pseudoname;

typedef struct GL_Newsgroup_List_Type
{
   char *group;
   struct GL_Newsgroup_List_Type *next;
}
GL_Newsgroup_List_Type;

static GL_Newsgroup_List_Type *Newsgroup_List = NULL;

int slrn_grouplens_add_group (char *group)
{
   GL_Newsgroup_List_Type *g;
   
   if (group == NULL) return -1;
   g = (GL_Newsgroup_List_Type *) malloc (sizeof (GL_Newsgroup_List_Type));
   if (g == NULL)
     return -1;
   
   g->group = slrn_safe_strmalloc (group);
   g->next = Newsgroup_List;
   Newsgroup_List = g;
   
   return 0;
}

static int is_group_valid (char *name)
{
   GL_Newsgroup_List_Type *g;
   
   g = Newsgroup_List;
   while (g != NULL)
     {
	if (0 == slrn_case_strcmp ((unsigned char *)name, (unsigned char *)g->group))
	  return 1;
	
	g = g->next;
     }
   return 0;
}

static int read_grplens_file (void)
{
   char file [256];
   char *name = ".grplens";
   char line [1024];
   FILE *fp;
   
   fp = slrn_open_home_file (name, "r", file, 0);
   if (fp == NULL) return -1;

   slrn_message ("Reading %s", file);
   
   while (NULL != fgets (line, sizeof (line), fp))
     {
	char *b;
	
	b = slrn_skip_whitespace (line);
	if ((*b == 0) || (*b == '#') || (*b == '%'))
	  continue;
	
	(void) slrn_trim_string (b);
	
	if (!slrn_case_strncmp ((unsigned char *)b, (unsigned char *) "BBBHOST", 7))
	  {
	     b = slrn_skip_whitespace (b + 7);
	     if (*b == 0) continue;
	     slrn_free (Slrn_GroupLens_Host);
	     Slrn_GroupLens_Host = slrn_safe_strmalloc (b);
	     continue;
	  }
	
	if (!slrn_case_strncmp ((unsigned char *)b, (unsigned char *)"PSEUDONYM", 9))
	  {
	     b = slrn_skip_whitespace (b + 9);
	     if (*b == 0) continue;
	     slrn_free (Slrn_GroupLens_Pseudoname);
	     Slrn_GroupLens_Pseudoname = slrn_safe_strmalloc (b);
	     continue;
	  }
	
	if (!slrn_case_strncmp ((unsigned char *) b, (unsigned char *)"BBBPORT", 7))
	  {
	     b = slrn_skip_whitespace (b + 7);
	     if (*b == 0) continue;
	     if (1 != sscanf (b, "%d", &Slrn_GroupLens_Port))
	       Slrn_GroupLens_Port = -1;
	     
	     continue;
	  }
	
	if (!slrn_case_strncmp ((unsigned char *)b, (unsigned char *)"DISPLAYTYPE", 11))
	  continue;
	
	/* Anything else is a newsgroup */
	
	(void) slrn_grouplens_add_group (b);
     }
   
   fclose (fp);
   return 0;
}

int slrn_init_grouplens (void)
{
   Slrn_Use_Group_Lens = 0;

   (void) read_grplens_file ();
   
   if ((Slrn_GroupLens_Host == NULL)
       || (Slrn_GroupLens_Port <= 0)
       || (Slrn_GroupLens_Pseudoname == NULL))
     {
	return -1;
     }
   
   slrn_message ("Connecting to GroupLens host %s, port %d ...", Slrn_GroupLens_Host, Slrn_GroupLens_Port);
   
   if (-1 == gl_open_server (Slrn_GroupLens_Host, Slrn_GroupLens_Port, Slrn_GroupLens_Pseudoname))
     {
	slrn_error ("GroupLens login failure: %s", gl_get_error ());
	return -1;
     }
   
   Slrn_Use_Group_Lens = 1;
   slrn_message ("GroupLens support initialized.");
   return 0;
}

void slrn_close_grouplens (void)
{
   if (Slrn_Use_Group_Lens == 0) return;
   slrn_message ("Logging out of GroupLens server ...");
   Slrn_Message_Present = 0;
   Slrn_Use_Group_Lens = 0;
   gl_close_server ();
}

static int Did_Rating = 0;
static int Prediction_Count;

static void prediction_callback (GL_Prediction_Type *s)
{
   Slrn_Header_Type *h;
   int i;
   
   h = slrn_find_header_with_msgid (s->msgid);

   if (h == NULL)
     return;			       /* not supposed to happen */
   
   i = (int) (s->pred + 0.5);

   if (i < 0) i = 0;
   
   h->gl_pred = i;
   Prediction_Count++;
}

int slrn_get_grouplens_scores (void)
{
   Slrn_Header_Type *h;
   int ret;
   
   if ((Slrn_Use_Group_Lens == 0) 
       || (Slrn_First_Header == NULL))
     return -1;

   if (0 == is_group_valid (Slrn_Current_Group_Name))
     return -1;
   
   Prediction_Count = 0;
   
   slrn_message_now ("Getting GroupLens predictions ...");
 
   if (-1 == gl_open_predictions (Slrn_Current_Group_Name))
     {
	do_error ();
	return -1;
     }
   
   h = Slrn_First_Header;
   
   ret = 0;
   while (h != NULL)
     {
	if (ret == 0) ret = gl_want_prediction (h->msgid);
	h = h->real_next;
     }
   
   if (ret == -1) 
     {
	do_error ();
	return -1;
     }
   
   if (-1 == gl_close_predictions (prediction_callback))
     {
	do_error ();
	return -1;
     }
   
   return Prediction_Count;
}

     
int slrn_put_grouplens_scores (void)
{  
   Slrn_Header_Type *h;
   
   if (Slrn_Use_Group_Lens == 0) return 0;
   if (Did_Rating == 0) return 0;
   Did_Rating = 0;
   
   h = Slrn_First_Header;
   if (h == NULL) return 0;
   
   slrn_message_now ("Sending GroupLens ratings ...");
   
   if (-1 == gl_open_ratings (Slrn_Current_Group_Name))
     {
	do_error ();
	return -1;
     }
   
   while (h != NULL)
     {
	GL_Rating_Type r;
	
	if (h->gl_rating > 0)
	  {
	     r.msgid = h->msgid;
	     if (h->gl_rating >= 10)
	       {
		  r.saw_article = 1;
		  r.rating = h->gl_rating / 10;
	       }
	     else
	       {
		  r.saw_article = 0;
		  r.rating = h->gl_rating;
	       }
	     
	     if (-1 == gl_put_rating (&r))
	       {
		  do_error ();
		  return -1;
	       }
	  }
	h = h->real_next;
     }
   
   if (-1 == gl_close_ratings ())
     {
	do_error ();
	return -1;
     }
   
   return 0;
}

void slrn_group_lens_rate_article (Slrn_Header_Type *h, int score, int saw_article)
{
   if (saw_article) score = score * 10;
   h->gl_rating = score;
   Did_Rating++;
}


		
#endif				       /* SLRN_HAS_GROUPLENS */
