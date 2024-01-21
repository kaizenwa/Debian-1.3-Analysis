/* -*- mode: C; mode: fold; -*- */
/*  Copyright (c) 1995, 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */

#include "config.h"

#ifndef SLRNPULL_CODE
# include "slrnfeat.h"
#endif

#include <stdio.h>
#include <string.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#include "util.h"
#include "ttymsg.h"
#include "hash.h"

#ifndef SLRNPULL_CODE
# include "group.h"
# include "art.h"
#endif

#include "xover.h"

#ifndef SLRNPULL_CODE
static int extract_id_from_xref (char *);
#endif

typedef struct /*{{{*/
{
   char *name;
   unsigned int name_len;
   char *value;
}

/*}}}*/
Header_Type;

#define MAX_PARSED_HEADERS 36
static Header_Type Parsed_Headers [MAX_PARSED_HEADERS] = /*{{{*/
{
#define SUBJECT_OFFSET	0
     {"Subject", 7, NULL},
#define FROM_OFFSET	1
     {"From", 4, NULL},
#define DATE_OFFSET	2
     {"Date", 4, NULL},
#define MSGID_OFFSET	3
     {"Message-ID", 10, NULL},
#define REFS_OFFSET	4
     {"References", 10, NULL},
#define LINES_OFFSET	5
     {"Lines", 5, NULL},
#define XREF_OFFSET	6
     {"Xref", 4, NULL},
   
   /* Technically, bytes is in the overview file.  However, this value
    * is not stored in slrn's article structure.
    */
#define BYTES_OFFSET	7
     {"Bytes", 5, NULL}
#define NON_XOVER_HEADER_START 7
};

/*}}}*/

/* The pointers in the above structure will point to the following buffer. */
static char *Malloced_Headers;

static void parse_headers (void) /*{{{*/
{
   unsigned int i;
   unsigned int next_slot;
   char *h, ch;
   
   for (i = 0; i < NON_XOVER_HEADER_START; i++)
     Parsed_Headers[i].value = NULL;
   
   for (i = NON_XOVER_HEADER_START; i < MAX_PARSED_HEADERS; i++)
     {
	Parsed_Headers[i].name = NULL;
	Parsed_Headers[i].value = NULL;
     }
   
   h = Malloced_Headers;
   next_slot = NON_XOVER_HEADER_START;
   
   while (*h != 0)
     {
	char *colon;
	unsigned int len;
	
	colon = h;
	while (*colon && (*colon != ':')) colon++;
	
	if (*colon != ':')
	  break;
	
	*colon = 0;
	len = (unsigned int) (colon - h);
	
	colon++;
	if (*colon == ' ') colon++;   /* space is required to be there */
	
	for (i = 0; i < NON_XOVER_HEADER_START; i++)
	  {
	     if ((Parsed_Headers[i].value != NULL)
		 || (len != Parsed_Headers[i].name_len)
		 || (slrn_case_strcmp ((unsigned char *)h, 
				       (unsigned char *) Parsed_Headers[i].name)))
	       continue;
	     
	     Parsed_Headers[i].value = colon;
	     break;
	  }
	
	if ((i == NON_XOVER_HEADER_START) && (next_slot < MAX_PARSED_HEADERS))
	  {
	     /* Must not be an XOVER header.  Add it to list. */
	     Parsed_Headers[next_slot].name = h;
	     Parsed_Headers[next_slot].name_len = len;
	     Parsed_Headers[next_slot].value = colon;
	     next_slot++;
	  }
	
	/* Now skip to next header line and take care of continuation if
	 * present.
	 */
	
	h = colon;
	while (0 != (ch = *h))
	  {
	     if (ch == '\n')
	       {
		  ch = *(h + 1);
		  if ((ch == ' ') || (ch == '\t'))
		    {
		       *h++ = ' ';
		    }
		  else 
		    {
		       *h++ = 0;
		       break;
		    }
	       }
	     
	     if (ch == '\t') *h = ' ';

	     h++;
	  }
     }
}

/*}}}*/

static int parsed_headers_to_xover (int id, Slrn_XOver_Type *xov) /*{{{*/
{
   unsigned int len;
   char *subj, *from, *date, *msgid, *refs, *bytes, *lines, *xref;
   char *buf;

   if (NULL == (subj = Parsed_Headers[SUBJECT_OFFSET].value))
     subj = "";
   if (NULL == (from = Parsed_Headers[FROM_OFFSET].value))
     from = "";
   if (NULL == (date = Parsed_Headers[DATE_OFFSET].value))
     date = "";
   if (NULL == (msgid = Parsed_Headers[MSGID_OFFSET].value))
     msgid = "";
   if (NULL == (refs = Parsed_Headers[REFS_OFFSET].value))
     refs = "";
   if (NULL == (xref = Parsed_Headers[XREF_OFFSET].value))
     xref = "";
   if (NULL == (lines = Parsed_Headers[LINES_OFFSET].value))
     lines = "";
   if (NULL == (bytes = Parsed_Headers[BYTES_OFFSET].value))
     bytes = "";
   
   len = strlen (subj) + strlen (from) + strlen (date) + strlen (msgid)
     + strlen (refs) + strlen (xref) + 6;

   buf = slrn_malloc (len, 0, 1);
   if (buf == NULL)
     return -1;
   
   xov->subject_malloced = buf;
   strcpy (buf, subj);
   buf += strlen (subj) + 1;
   
   xov->from = buf;
   strcpy (buf, from);
   buf += strlen (from) + 1;

   xov->date = buf;
   strcpy (buf, date);
   buf += strlen (date) + 1;
   
   xov->message_id = buf;
   strcpy (buf, msgid);
   buf += strlen (msgid) + 1;

   xov->references = buf;
   strcpy (buf, refs);
   buf += strlen (refs) + 1;
   
   xov->xref = buf;
   strcpy (buf, xref);
   buf += strlen (xref) + 1;

   xov->bytes = atoi (bytes);
   xov->lines = atoi (lines);

#ifndef SLRNPULL_CODE
   if (id == -1)
     id = extract_id_from_xref (xov->xref);
#endif

   xov->id = id;
   return 0;
}

/*}}}*/

char *slrn_get_extra_xover_header (char *hdr) /*{{{*/
{
   Header_Type *h, *hmax;
   unsigned int len;
   
   h = Parsed_Headers + NON_XOVER_HEADER_START;
   hmax = Parsed_Headers + MAX_PARSED_HEADERS;
   len = strlen (hdr);
   
   while ((h < hmax) && (h->value != NULL))
     {
	if ((len == h->name_len)
	    && (0 == slrn_case_strcmp ((unsigned char *)h->name,
				       (unsigned char *)hdr)))
	  return h->value;
	
	h++;
     }
   return NULL;
}

/*}}}*/

#ifdef SLRNPULL_CODE
static int xover_parse_head (int id, char *headers, Slrn_XOver_Type *xov) /*{{{*/
{
   slrn_free (Malloced_Headers);
   
   if (NULL == (Malloced_Headers = slrn_strmalloc (headers, 1)))
     return -1;
   
   parse_headers ();
   
   return parsed_headers_to_xover (id, xov);
}
/*}}}*/
#endif

void slrn_map_xover_to_header (Slrn_XOver_Type *xov, Slrn_Header_Type *h)
{   
   char *m;
   
   h->subject = xov->subject_malloced;
   h->number = xov->id;
   h->from = xov->from;
   h->date = xov->date;
   h->refs = xov->references;
   h->lines = xov->lines;
   h->xref = xov->xref;
   
   /* Since the subject has been malloced, the message_id pointer can be changed.
    */
   m = xov->message_id;
   while ((*m != '<') && (*m != 0))
     m++;
   
   if (*m != 0)
     {
	h->msgid = m;
	m = slrn_strchr (m, '>');
	if (m != NULL) *(m + 1) = 0;
     }
   else h->msgid = xov->message_id;
   
   h->hash = slrn_compute_hash ((unsigned char *) h->msgid,
				(unsigned char *) h->msgid + strlen (h->msgid));
}

#ifndef SLRNPULL_CODE

# include "server.h"

static int XOver_Done;
static int XOver_Min;
static int XOver_Max;
static int XOver_Next;
static int Suspend_XOver_For_Kill = 0;

static int extract_id_from_xref (char *xref) /*{{{*/
{
   unsigned int group_len;
   char ch;
   
   if (*xref == 0)
     return -1;
   
   group_len = strlen (Slrn_Current_Group_Name);
   
   while ((ch = *xref) != 0)
     {
	if (ch == ' ')
	  {
	     xref++;
	     continue;
	  }

	if (0 == strncmp (xref, Slrn_Current_Group_Name, group_len))
	  {
	     xref += group_len;
	     if (*xref == ':')
	       return atoi (xref + 1);
	  }

	/* skip to next space */
	while (((ch = *xref) != 0) && (ch != ' ')) xref++;
     }
   return -1;
}

/*}}}*/

static char *server_read_and_malloc (void) /*{{{*/
{
   char line [NNTP_BUFFER_SIZE];
   char *mbuf;
   unsigned int buffer_len, buffer_len_max;
   int failed;
   
   mbuf = NULL;
   buffer_len_max = buffer_len = 0;
   failed = 0;
   
   while (NULL != (Slrn_Server_Obj->sv_read_line (line, sizeof(line))))
     {
	unsigned int len;
	
	if (failed) continue;
	
	len = strlen (line);
	
	if (len + buffer_len + 4 > buffer_len_max)
	  {
	     char *new_mbuf;
	     
	     buffer_len_max += 4096 + len;
	     new_mbuf = slrn_realloc (mbuf, buffer_len_max, 0);
	     
	     if (new_mbuf == NULL)
	       {
		  slrn_free (mbuf);
		  failed = 1;
		  continue;
	       }
	     mbuf = new_mbuf;
	  }
   
	strcpy (mbuf + buffer_len, line);
	buffer_len += len;
	mbuf [buffer_len++] = '\n';
	mbuf [buffer_len] = 0;
     }

   if (failed) return NULL;
   
   return mbuf;
}

/*}}}*/

static int read_head_into_xover (int id, Slrn_XOver_Type *xov) /*{{{*/
{
   slrn_free (Malloced_Headers);
   
   if (NULL == (Malloced_Headers = server_read_and_malloc ()))
     return -1;
   
   parse_headers ();
   
   return parsed_headers_to_xover (id, xov);
}
/*}}}*/

int slrn_open_xover (int min, int max) /*{{{*/
{
   int id, status;
   XOver_Done = 1;

   if (Slrn_Server_Obj->sv_has_xover && !Suspend_XOver_For_Kill)
     {
	status = Slrn_Server_Obj->sv_nntp_xover (min, max);
	if (status != OK_XOVER)
	  return status;

	XOver_Next = XOver_Min = min;
	XOver_Max = max;
	XOver_Done = 0;
	return OK_XOVER;
     }

   /* The rest of this function applies when not using XOVER.  
    * It is complicated by the fact that the server may contain huge ranges
    * of missing articles.  In particular, the first article in the desired
    * xover range may be missing.  The following code determines the first
    * article in this range.  If the range is large an no articles are present
    * in the range, it may be rather slow.
    */
   for (id = min; id <= max; id++)
     {
	status = (*Slrn_Server_Obj->sv_nntp_head)(id, NULL, NULL);
	
	if (status == -1)
	  return -1;
	
	if (status == OK_HEAD)
	  break;
     }
   
   XOver_Next = XOver_Min = id;
   XOver_Max = max;
   XOver_Done = 0;
   
   return OK_XOVER;
}

/*}}}*/

/* The line consists of:
 * id|subj|from|date|msgid|refs|bytes|line|misc stuff
 * Here '|' is a TAB.  The following code parses this.
 */
static int parse_xover_line (char *buf, Slrn_XOver_Type *xov) /*{{{*/
{
   char *b;
   int id;
   
   b = buf;
   
   while (*b && (*b != '\t')) b++;
   if (*b) *b++ = 0;
   id = atoi (buf);
   
   Parsed_Headers [SUBJECT_OFFSET].value = b;
   while (*b && (*b != '\t')) b++;
   if (*b) *b++ = 0;

   Parsed_Headers [FROM_OFFSET].value = b;
   while (*b && (*b != '\t')) b++;
   if (*b) *b++ = 0;
   
   Parsed_Headers [DATE_OFFSET].value = b;
   while (*b && (*b != '\t')) b++;
   if (*b) *b++ = 0;
   
   Parsed_Headers [MSGID_OFFSET].value = b;
   while (*b && (*b != '\t')) b++;
   if (*b) *b++ = 0;
   
   Parsed_Headers [REFS_OFFSET].value = b;
   while (*b && (*b != '\t')) b++;
   if (*b) *b++ = 0;

   Parsed_Headers [BYTES_OFFSET].value = b;
   while (*b && (*b != '\t')) b++;
   if (*b) *b++ = 0;

   Parsed_Headers [LINES_OFFSET].value = b;
   while (*b && (*b != '\t')) b++;
   if (*b) *b++ = 0;

   Parsed_Headers [XREF_OFFSET].value = NULL;

   /* Optional fields consist of Header: stuff.  Look for Xref: ... */
   while (*b != 0)
     {
	char *xb = b;
	     
	/* skip to next field. */
	while (*b && (*b != '\t')) b++;
	if (*b) *b++ = 0;
	     
	if (0 == slrn_case_strncmp ((unsigned char *) xb, 
				    (unsigned char *) "Xref: ", 6))
	  {
	     Parsed_Headers[XREF_OFFSET].value = xb + 6;
	     break;
	  }
     }
   
   return parsed_headers_to_xover (id, xov);
}

/*}}}*/

/* Returns -1 upon error, 0, if done, and 1 upon success */
int slrn_read_xover (Slrn_XOver_Type *xov) /*{{{*/
{
   char buf [NNTP_BUFFER_SIZE];
   int id;
   char *overview;
   int status;
   
   if (XOver_Done)
     return 0;

   if (Slrn_Server_Obj->sv_has_xover && !Suspend_XOver_For_Kill)
     {
	while (NULL != (overview = Slrn_Server_Obj->sv_read_line (buf, sizeof (buf))))
	  {
	     id = atoi (overview);
	     if ((id >= XOver_Min) && (id <= XOver_Max))
	       break;
	     
	     /* else server screwed up and gave bad response.  Ignore it. */
	  }

	if (overview == NULL)
	  {
	     XOver_Done = 1;
	     return 0;
	  }

	if (-1 == parse_xover_line (overview, xov))
	  return -1;
	
	return 1;
     }

   /* For non-XOVER, the HEAD command has already been sent. */
   if (-1 == read_head_into_xover (XOver_Next, xov))
     {
	XOver_Done = 1;
	return -1;
     }
   
   if (XOver_Next == XOver_Max)
     {
	XOver_Done = 1;
	return 1;
     }

   XOver_Next++;

   while (1)
     {
	/* Get head of next article ready for next call of this function. */
	status = Slrn_Server_Obj->sv_nntp_head (XOver_Next, NULL, NULL);
	if (status == OK_HEAD)
	  break;
   
	/* Looks like the head is not available for that article.  Do NEXT to 
	 * next available article in the range.
	 */
	if ((status == -1)
	    || (-1 == (status = Slrn_Server_Obj->sv_nntp_next (&id))))
	  {
	     if (SLKeyBoard_Quit)
	       {
		  XOver_Done = 1;
		  break;
	       }
	     
	     slrn_exit_error ("Server closed connection.  Cannot recover.");
	  }
	
	if ((status != OK_NEXT) || (id > XOver_Max))
	  {
	     XOver_Done = 1;
	     break;
	  }

	XOver_Next = id;
     }
   
   return 1;
}

/*}}}*/
void slrn_close_xover (void) /*{{{*/
{
   XOver_Done = 1;
}

/*}}}*/
void slrn_open_suspend_xover (void) /*{{{*/
{
   Suspend_XOver_For_Kill = 1;
}

/*}}}*/
void slrn_close_suspend_xover (void) /*{{{*/
{
   Suspend_XOver_For_Kill = 0;
}

/*}}}*/
int slrn_xover_for_msgid (char *msgid, Slrn_XOver_Type *xov) /*{{{*/
{
   int id, status;
   
   if ((msgid == NULL) || (*msgid == 0)) return -1;

   status = Slrn_Server_Obj->sv_nntp_head (-1, msgid, &id);

   if (OK_HEAD != status)
     {
	if (ERR_FAULT == status)
	  slrn_error ("Server does not provide this capability.");
	return -1;
     }
      
   if (id == 0) id = -1;
   
   return read_head_into_xover (id, xov);
}

/*}}}*/
#endif				       /* NOT SLRNPULL_CODE */
