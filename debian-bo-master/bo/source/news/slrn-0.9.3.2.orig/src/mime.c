/* MIME handling routines.
 *
 * Author: Michael Elkins <elkins@aero.org>
 * Modified by John E. Davis <davis@space.mit.edu>
 */

#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>
#include <string.h>


#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <ctype.h>

#if defined (__os2__)
# include <process.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#include "server.h"
#include "slrn.h"
#include "misc.h"
#include "slrn.h"
#include "group.h"
#include "art.h"
#include "util.h"

#if SLRN_HAS_MIME
/* rest of file in this ifdef */

#include "mime.h"

int Slrn_Use_Mime = 1;
int Slrn_Use_Meta_Mail = 1;
char *Slrn_MetaMail_Cmd;

int Slrn_Mime_Was_Parsed;
int Slrn_Mime_Was_Modified;
int Slrn_Mime_Needs_Metamail;

char *Slrn_Mime_Display_Charset;

/* These are all supersets of US-ASCII */
static char *Compatable_Charsets[] =
{
   "US-ASCII",			       /* This MUST be zeroth element */
     "ISO-8859-1",
     "ISO-8859-2",
     "ISO-8859-3",
     "ISO-8859-4",
     "ISO-8859-5",
     "ISO-8859-6",
     "ISO-8859-7",
     "ISO-8859-8",
     "ISO-8859-9",
     "KOI8-R",
     NULL
};

static char *Char_Set;
static int Content_Type;
#define CONTENT_TYPE_TEXT		0x01
#define CONTENT_TYPE_MESSAGE		0x02
#define CONTENT_TYPE_MULTIPART		0x03
#define CONTENT_TYPE_UNSUPPORTED	0x10

static int Content_Subtype;
#define CONTENT_SUBTYPE_PLAIN		0x01
#define CONTENT_SUBTYPE_UNKNOWN		0x02
#define CONTENT_SUBTYPE_UNSUPPORTED	0x10

static int Encoding_Method;
#define ENCODED_7BIT			1
#define ENCODED_8BIT			2
#define ENCODED_QUOTED			3
#define ENCODED_BASE64			4
#define ENCODED_BINARY			5
#define ENCODED_UNSUPPORTED		6

#ifndef isalnum
#define isalnum(x) \
  ((((x) <= 'Z') && ((x) >= 'A')) \
   || (((x) <= 'z') && ((x) >= 'a')) \
   || (((x) <= '9') && ((x) >= '0')))
#endif

static Slrn_Article_Line_Type *find_header_line (char *header)
{
   Slrn_Article_Line_Type *line = Slrn_Article_Lines;
   unsigned char ch = (unsigned char) UPPER_CASE(*header);
   unsigned int len = strlen (header);
   
   while ((line != NULL) && (line->flags & HEADER_LINE))
     {
	unsigned char ch1 = (unsigned char) *line->buf;
	if ((ch == UPPER_CASE(ch1))
	    && (0 == slrn_case_strncmp ((unsigned char *)header,
					(unsigned char *)line->buf,
					len)))
	  return line;
	line = line->next;
     }
   return NULL;
}


static char *find_compatable_charset (char *cs, unsigned int len)
{
   char **compat_charset;
   
   compat_charset = Compatable_Charsets;
   while (*compat_charset != NULL)
     {
	if ((0 == slrn_case_strncmp ((unsigned char *) cs,
				     (unsigned char *) *compat_charset,
				     len))
	    && (len == strlen(*compat_charset)))
	  return *compat_charset;
	
	compat_charset++;
     }
   return NULL;
}

static int parse_content_type_line (void)
{
   Slrn_Article_Line_Type *line;
   char *b;
   
   /* Use default: text/plain; charset=us-ascii */
   Content_Type = CONTENT_TYPE_TEXT;
   Content_Subtype = CONTENT_SUBTYPE_PLAIN;
   Char_Set = Compatable_Charsets[0];
   
   if (NULL == (line = find_header_line ("Content-Type:")))
     return 0;
   
   b = slrn_skip_whitespace (line->buf + 13);
   
   if (0 == slrn_case_strncmp ((unsigned char *)b,
			       (unsigned char *) "text/",
			       5))
     {
	b += 5;
	if (0 != slrn_case_strncmp ((unsigned char *)b,
				    (unsigned char *) "plain",
				    5))
	  {
	     Content_Subtype = CONTENT_SUBTYPE_UNSUPPORTED;
	     return -1;
	  }
	b += 5;
     }
   else if (0 == slrn_case_strncmp ((unsigned char *)b,
				    (unsigned char *) "message/",
				    5))
     {
	Content_Type = CONTENT_TYPE_MESSAGE;
	Content_Subtype = CONTENT_SUBTYPE_UNKNOWN;
	b += 8;
     }
   else if (0 == slrn_case_strncmp ((unsigned char *)b,
				    (unsigned char *) "multipart/",
				    5))
     {
	Content_Type = CONTENT_TYPE_MULTIPART;
	Content_Subtype = CONTENT_SUBTYPE_UNKNOWN;
	b += 10;
     }
   else
     {
	Content_Type = CONTENT_TYPE_UNSUPPORTED;
	return -1;
     }
   
   do
     {
	while (NULL != (b = slrn_strchr (b, ';')))
	  {
	     char *charset;
	     unsigned int len;
	     
	     b = slrn_skip_whitespace (b + 1);
	     
	     if (0 != slrn_case_strncmp ((unsigned char *)b,
					 (unsigned char *)"charset",
					 7))
	       continue;
	     
	     b = slrn_skip_whitespace (b + 7);
	     while (*b == 0)
	       {
		  line = line->next;
		  if ((line == NULL)
		      || ((line->flags & HEADER_LINE) == 0)
		      || ((*(b = line->buf) != ' ') && (*b == '\t')))
		    return -1;
		  b = slrn_skip_whitespace (b);
	       }
	     
	     if (*b != '=') continue;
	     b++;
	     if (*b == '"') b++;
	     charset = b;
	     while (*b && (*b != ';')
		    && (*b != ' ') && (*b != '\t') && (*b != '\n')
		    && (*b != '"'))
	       b++;
	     len = b - charset;
	     
	     Char_Set = find_compatable_charset (charset, len);
	     if (Char_Set == NULL) return -1;
	     return 0;
	  }
	line = line->next;
     }
   while ((line != NULL)
	  && (line->flags & HEADER_LINE)
	  && ((*(b = line->buf) == ' ') || (*b == '\t')));
   
   return 0;
}

static int parse_content_transfer_encoding_line (void)
{
   Slrn_Article_Line_Type *line;
   unsigned char *buf;
   
   Encoding_Method = ENCODED_7BIT;
   line = find_header_line ("Content-Transfer-Encoding:");
   if (line == NULL) return 0;
   
   buf = (unsigned char *) slrn_skip_whitespace (line->buf + 26);
   if (*buf == '"') buf++;
   
   if (0 == slrn_case_strncmp (buf, (unsigned char *) "7bit", 4))
     Encoding_Method = ENCODED_7BIT;
   else if (0 == slrn_case_strncmp (buf, (unsigned char *) "8bit", 4))
     Encoding_Method = ENCODED_8BIT;
   else if (0 == slrn_case_strncmp (buf, (unsigned char *) "base64", 6))
     Encoding_Method = ENCODED_BASE64;
   else if (0 == slrn_case_strncmp (buf, (unsigned char *) "quoted-printable", 16))
     Encoding_Method = ENCODED_QUOTED;
   else if (0 == slrn_case_strncmp (buf, (unsigned char *) "binary", 6))
     Encoding_Method = ENCODED_BINARY;
   else
     {
	Encoding_Method = ENCODED_UNSUPPORTED;
	return -1;
     }
   return 0;
}

static int Index_Hex[128] =
{
   -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     0, 1, 2, 3,  4, 5, 6, 7,  8, 9,-1,-1, -1,-1,-1,-1,
     -1,10,11,12, 13,14,15,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     -1,10,11,12, 13,14,15,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1
};
#define HEX(c) (Index_Hex[(unsigned char)(c) & 0x7F])

static char *decode_quoted_printable (char *dest,
				      char *src, char *srcmax,
				      int treat_underscore_as_space)
{
   char *allowed_in_qp = "0123456789ABCDEFabcdef";
   char ch;
   while (src < srcmax)
     {
	ch = *src++;
	if ((ch == '=') && (src + 1 < srcmax)
	    && (NULL != slrn_strchr (allowed_in_qp, src[0]))
	    && (NULL != slrn_strchr (allowed_in_qp, src[1])))
	  {
	     *dest++ = (16 * HEX(src[0])) + HEX(src[1]);
	     src += 2;
	  }
	else if ((ch == '_') && treat_underscore_as_space)
	  {
	     *dest++ = ' ';
	  }
	else *dest++ = ch;
     }
   return dest;
}

static int Index_64[128] =
{
   -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,62, -1,-1,-1,63,
     52,53,54,55, 56,57,58,59, 60,61,-1,-1, -1,-1,-1,-1,
     -1, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
     15,16,17,18, 19,20,21,22, 23,24,25,-1, -1,-1,-1,-1,
     -1,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
     41,42,43,44, 45,46,47,48, 49,50,51,-1, -1,-1,-1,-1
};

#define BASE64(c) (Index_64[(unsigned char)(c) & 0x7F])

static char *decode_base64 (char *dest, char *src, char *srcmax)
{
   while (src + 3 < srcmax)
     {
	*dest++ = (BASE64(src[0]) << 2) | (BASE64(src[1]) >> 4);
	*dest++ = ((BASE64(src[1]) & 0xf) << 4) | (BASE64(src[2]) >> 2);
	*dest++ = ((BASE64(src[2]) & 0x3) << 6) | BASE64(src[3]);
	src += 4;
     }
   return dest;
}

int slrn_rfc1522_decode_string (char *s)
{
   char *s1, *s2, ch;
   char *charset, method, *txt;
   unsigned int count = 0;
   unsigned int len;
   
   while (1)
     {
	while ((NULL != (s = slrn_strchr (s, '=')))
	       && (s[1] != '?')) s++;
	if (s == NULL) break;
	
	s1 = s;
	charset = s = s1 + 2;
	while (((ch = *s) != 0)
	       && (ch != '?') && (ch != ' ') && (ch != '\t') && (ch != '\n'))
	  s++;
	
	if (ch != '?')
	  {
	     s = s1 + 2;
	     continue;
	  }
	
	charset = s1 + 2;
	len = s - charset;
	
	charset = find_compatable_charset (charset, len);
	s++;			       /* skip ? */
	method = *s++;		       /* skip B,Q */
	method = UPPER_CASE(method);
	
	if ((charset == NULL) || ((method != 'B') && (method != 'Q'))
	    || (*s != '?'))
	  {
	     s = s1 + 2;
	     continue;
	  }
	
	/* Now look for the final ?= after encoded test */
	s++;			       /* skip ? */
	txt = s;
       
	while ((ch = *s) != 0)
	  {
	     if ((ch == ' ') || (ch == '\t') || (ch == '\n'))
	       break;
	     if ((ch == '?') && (s[1] == '='))
	       break;
	     
	     s++;
	  }
	
	if ((ch != '?') || (s[1] != '='))
	  {
	     s = s1 + 2;
	     continue;
	  }
	
	/* Note: these functions return a pointer to the END of the decoded
	 * text.
	 */
	if (method == 'B')
	  s1 = decode_base64 (s1, txt, s);
	else s1 = decode_quoted_printable (s1, txt, s, 1);
	
	/* Now move everything over */
	s2 = s + 2;		       /* skip final ?= */
	s = s1;			       /* start from here next loop */
	while ((ch = *s2++) != 0) *s1++ = ch;
	*s1 = 0;
	
	count++;
     }
   return count;
}





static void rfc1522_decode_headers (void)
{
   Slrn_Article_Line_Type *line = Slrn_Article_Lines;
   
   while ((line != NULL) && (line->flags & HEADER_LINE))
     {
	if (slrn_rfc1522_decode_string (line->buf))
	  {
	     Slrn_Mime_Was_Modified = 1;
	  }
	line = line->next;
     }
}


static void decode_mime_base64 (void)
{
   Slrn_Mime_Needs_Metamail = 1;
}

/* This function checks if the last character on curr_line is an = and 
 * if it is, then it merges curr_line and curr_line->next. See RFC1341,
 * section 5.1 (Quoted-Printable Content-Transfer-Encoding) rule #5.
 * [csp@ohm.york.ac.uk]
 */
static int merge_if_soft_linebreak (Slrn_Article_Line_Type *curr_line)
{
   Slrn_Article_Line_Type *next_line;

   while ((next_line = curr_line->next) != NULL)
     {
	char *b = curr_line->buf;
	unsigned int len;

	len = strlen (b);
	if (len == 0) return 0;
   
	len--;
	if (b[len] != '=') return 0;
	
	/* Remove the excess = character... */
	b[len] = '\0';
	
	if (NULL == (b = (char *) SLREALLOC (b, 1 + len + strlen (next_line->buf))))
	  return -1;

	curr_line->buf = b;
	
	strcpy (b + len, next_line->buf);
	
	/* Unlink next_line from the linked list of lines in the article... */
	curr_line->next = next_line->next;
	if (next_line->next != NULL)
	  next_line->next->prev = curr_line;
	
	SLFREE (next_line->buf);
	SLFREE (next_line);
     }
   return 0;
}

static void decode_mime_quoted_printable (void)
{
   Slrn_Article_Line_Type *line = Slrn_Article_Lines;
   
   /* skip to body */
   while ((line != NULL) && (line->flags & HEADER_LINE))
     line = line->next;
   if (line == NULL) return;
   
   while (line != NULL)
     {
	char *b;
	unsigned int len;
	
	b = line->buf;
	len = strlen (b);
	if (len && (b[len - 1] == '=') 
	    && (line->next != NULL))
	  {
	     (void) merge_if_soft_linebreak (line);
	     b = line->buf;
	     len = strlen (b);
	  }

	b = decode_quoted_printable (b, b, b + len, 0);
	if (b < line->buf + len)
	  {
	     *b = 0;
	     Slrn_Mime_Was_Modified = 1;
	  }
	
	line = line->next;
     }
}



void slrn_mime_article_init (void)
{
   Slrn_Mime_Was_Modified = 0;
   Slrn_Mime_Was_Parsed = 0;
   Slrn_Mime_Needs_Metamail = 0;
}

void slrn_mime_process_article (void)
{
   if ((Slrn_Use_Mime == 0)
       || Slrn_Mime_Was_Parsed
       || (Slrn_Article_Lines == NULL))
     return;
   
   Slrn_Mime_Was_Parsed = 1;	       /* or will be */
   
   rfc1522_decode_headers ();

   if (NULL == find_header_line ("Mime-Version:")) return;
   if ((-1 == parse_content_type_line ())
       || (-1 == parse_content_transfer_encoding_line ()))
     {
	Slrn_Mime_Needs_Metamail = 1;
	return;
     }
   
   switch (Encoding_Method)
     {
      case ENCODED_7BIT:
      case ENCODED_8BIT:
      case ENCODED_BINARY:
	/* Already done. */
	return;
	
      case ENCODED_BASE64:
	decode_mime_base64 ();
	break;
	
      case ENCODED_QUOTED:
	decode_mime_quoted_printable ();
	break;
	
      default:
	Slrn_Mime_Needs_Metamail = 1;
	return;
     }
}

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

int slrn_mime_call_metamail (void)
{
#ifdef VMS
   return 0;
#else
   int init = Slrn_TT_Initialized;
   char cmd [2 * SLRN_MAX_PATH_LEN];
   char tempfile [SLRN_MAX_PATH_LEN];
   Slrn_Article_Line_Type *ptr = Slrn_Article_Lines;
   FILE *fp;
   char *tmp, *mm;
   
   if ((Slrn_Use_Meta_Mail == 0)
       || Slrn_Batch
       || (slrn_get_yesno (1, "Process this MIME article with metamail") <= 0))
     return 0;

# ifdef __os2__
   if (NULL == (tmp = getenv ("TMP")))
     tmp = ".";
#else
   tmp = "/tmp";
# endif
   
   fp = slrn_open_tmpfile_in_dir (tmp, tempfile, "w");

   if (fp == NULL)
     {
	slrn_error ("Unable to open tmp file for metamail.");
	return 0;
     }
   
   while (ptr) 
     {
	fputs(ptr->buf, fp);
	putc('\n', fp);
	ptr = ptr->next;
     }
   slrn_fclose(fp);

   mm = Slrn_MetaMail_Cmd;
   
   if ((mm == NULL)
       || (*mm == 0)
       || (strlen (mm) > SLRN_MAX_PATH_LEN))
     mm = "metamail";

   sprintf (cmd, "%s %s", mm, tempfile);
   
   /* Make sure that metamail has a normal environment */
   slrn_set_display_state (0);
   
   slrn_posix_system(cmd, 0);
   slrn_delete_file (tempfile);
   
   printf("Press return to continue ...");
   getchar();
   fflush(stdin); /* get rid of any pending input! */
   
   slrn_set_display_state (init);
   return 1;
#endif  /* NOT VMS */
}


/* -------------------------------------------------------------------------
 * MIME encoding routines.
 * -------------------------------------------------------------------------*/

static char *Mime_Posting_Charset;
static int Mime_Posting_Encoding;

int slrn_mime_scan_file (FILE *fp)
{
   /* This routine scans the article to determine what CTE should be used */
   unsigned int linelen = 0;
   unsigned int maxlinelen = 0;
   int ch;
   int cr = 0;
   unsigned int hibin = 0;
   
   
   /* Skip the header.  8-bit characters in the header are taken care of
    * elsewhere since they ALWAYS need to be encoded.
    */
   while ((ch = getc(fp)) != EOF)
     {
	if (ch == '\n')
	  {
	     ch = getc(fp);
	     if (ch == '\n')
	       break;
	  }
     }
   
   if (ch == EOF)
     {
	rewind (fp);
	return -1;
     }
   
   while ((ch = getc(fp)) != EOF)
     {
	linelen++;
	if (ch & 0x80) hibin = 1; /* 8-bit character */
	
	if (ch == '\n')
	  {
	     if (linelen > maxlinelen)	maxlinelen = linelen;
	     linelen = 0;
	  }
	else if (((unsigned char)ch < 32) && (ch != '\t') && (ch != 0xC))
	  cr = 1;		       /* not tab or formfeed */
     }
   if (linelen > maxlinelen) maxlinelen = linelen;
   
   if (hibin > 0)
     {
	/* 8-bit data.  US-ASCII is NOT a valid charset, so use ISO-8859-1 */
	if (slrn_case_strcmp((unsigned char *)"us-ascii",
			     (unsigned char *)Slrn_Mime_Display_Charset) == 0)
	  Mime_Posting_Charset = "iso-8859-1";
	else
	  Mime_Posting_Charset = Slrn_Mime_Display_Charset;
     }
   else if (NULL != find_compatable_charset (Slrn_Mime_Display_Charset,
					     strlen (Slrn_Mime_Display_Charset)))
     /* 7-bit data.  Check to make sure that this display supports US-ASCII */
     Mime_Posting_Charset = "us-ascii";
   else
     Mime_Posting_Charset = Slrn_Mime_Display_Charset;

#if 0
   if ((maxlinelen > 990) || (cr > 0))
     {
	Mime_Posting_Encoding = ENCODED_QUOTED;
     }
   else
#endif
     if (hibin > 0)
     Mime_Posting_Encoding = ENCODED_8BIT;
   else
     Mime_Posting_Encoding = ENCODED_7BIT;
   
   rewind(fp);
   return 0;
}

#define IS_RFC850_SPECIAL(c) \
 (((c) == '(') || ((c) == ')') || ((c) == '<') || ((c) == '>') || ((c) == '"'))

/* This routine returns -1 if d is not big enough to encode s.
 * Technically, this routine is incorrect.  It should encode whitespace
 * separated words and not arbitrary sequences of text.
 */
static int rfc1522_encode (unsigned char *d, unsigned int len,
			   unsigned char *s, int ignore_specials)
{
   /* This routine encodes the a string containing 8-bit characters according
    * to the conventions of RFC1522.  The strategy here is to print up to
    * the first word which contains 8-bit chars before starting the "Q"
    * encoding.  When it has been determined that all of the 8-bit
    * characters have been encoded, switch back to normal mode.  This
    * approach should generate relatively short encoded words and should be
    * more generally readable.
    */
   
   unsigned int hibit;		   /* How many 8-bit characters are left? */
   unsigned char *p, ch, *dmax;
   char charset[256];
   unsigned int charset_len;

   /* First scan the string to see if there are 8-bit characters
    * Count them for later use.
    */
   p = s;
   hibit = 0;
   while ((ch = *p) != 0)
     {
	if (ch & 0x80) hibit++;
	p++;
     }
   
   if (hibit == 0)
     {
	if (s + len < p) return -1;
	
	strcpy((char *)d, (char *)s);
	return 0;
     }
   
   if (0 == slrn_case_strcmp ((unsigned char *)Slrn_Mime_Display_Charset,
			      (unsigned char *)"us-ascii"))
     {
	strcpy (charset, "=?iso-8859-1?Q?");
     }
   else sprintf (charset, "=?%s?Q?", Slrn_Mime_Display_Charset);
   charset_len = strlen (charset);
   
   p = s;
   dmax = d + len;
   /* leave room for the 0 character */
   dmax--;
   
   /* algorithm:
    *  1. copy up to first word that contains one or more eight bit chars.
    *  2. encode to last char of last word that contains eight bit chars,
    *     or until a RFC822 special is encountered: <>()"
    *  3. Repeat
    * The rationale behind this is that 7 bit characters are allowed only
    * in certain portions of the header where an 8 bit character would never
    * appear.  Thus by encoding only in regions of 8 bit charcters delimited
    * by the RFC822 specials, we are sure not to decode into forbidden regions.
    *
    * RFC1522 forbids strarting or ending a =?..?..?..?= sequence in mid-word.
    */
   while (1)
     {
	/* 1. Copy to first 8 bit character */
	while (((ch = *p) != 0) && (d < dmax) && (0 == (ch & 0x80)))
	  {
	     *d++ = ch;
	     p++;
	  }
	
	if (ch == 0)
	  break;
	
	if (d == dmax) return -1;
	
	/* walk back until we find a word boundary or begin-of-line */
	do
	  {
	     p--;
	     ch = *p;
	     if ((ch == ' ') || (ch == '\t')
		 || ((ignore_specials == 0) && IS_RFC850_SPECIAL(ch)))
	       {
		  p++;
		  break;
	       }
 	     d--;
	  }
	while (p != s);

 	
	/* 2. Now start encoding.  Work up to the first occurance of an RFC822
	 *    special character or until there are no more eight bit characters
	 *    left.  RFC1522 
	 */
	if (d + charset_len >= dmax)
	  return -1;
	
	strcpy ((char *)d, charset);
	d += charset_len;

	/* Note: rfc1522 places a limit on the length of the encoded word.  
	 * That limit is ignored here.  Perhaps someday I will recode this to 
	 * take the limit into account.
	 */
	while ((d < dmax) && ((ch = *p) != 0)
	       && ((ignore_specials != 0) || (0 == IS_RFC850_SPECIAL(ch)))
	       && ((hibit != 0) 
		   || ((ch != ' ') && (ch != '\t'))))
	  {
	     if (ch == ' ') *d++ = '_';
	     else if ((ch & 0x80) 
		      || ((ch < 32) && (ch != '\n'))
		      || IS_RFC850_SPECIAL(ch))
	       {
		  /* We need 3 characters to encode this. */
		  if (d + 3 >= dmax)
		    return -1;
		  sprintf ((char *)d, "=%2X", (int) ch);
		  d += 3;
		  hibit--;
	       }
	     else *d++ = ch;
	     p++;
	  }
	
	/* Now turn off encoding.  We need two characters */
	if (d + 1 >= dmax) return -1;
	
	d[0] = '?';
	d[1] = '=';
	d += 2;
     }
   *d = 0;
   return 0;
}

void slrn_mime_header_encode (char *s, unsigned int bytes)
{
   char buf[1024];
   unsigned int len = strlen (s);
   
   if (len < sizeof (buf))
     {
	int ignore_specials;
	
	/* Perhaps this test should be extended to include other headers. */
	ignore_specials = !slrn_case_strncmp ((unsigned char *)s, 
					      (unsigned char *)"Subject: ", 9);
	
	strcpy (buf, s);
	if (0 == rfc1522_encode ((unsigned char *)s, bytes, 
				 (unsigned char *)buf,
				 ignore_specials))
	    return;
     }
   
   /* Cannot do it so strip it to 8 bits. */
   while (*s)
     {
	*s = *s & 0x7F;
	s++;
     }
}

void slrn_mime_add_headers (FILE *fp)
{
   char *encoding;
   
   if (Mime_Posting_Charset == NULL)
     Mime_Posting_Charset = "us-ascii";

   switch (Mime_Posting_Encoding)
     {
      default:
      case ENCODED_8BIT:
	encoding = "8bit";
	break;
	
      case ENCODED_7BIT:
	if (!strcmp ("us-ascii", Mime_Posting_Charset))
	  return;
	encoding = "7bit";
	break;
	
      case ENCODED_QUOTED:
	encoding = "quoted-printable";
     }
   
   if (fp != NULL)
     {
	fprintf (fp, "\
Mime-Version: 1.0\n\
Content-Type: text/plain; charset=%s\n\
Content-Transfer-Encoding: %s\n",
		 Mime_Posting_Charset,
		 encoding);
     }
   else
     {
	Slrn_Post_Obj->po_printf ("\
Mime-Version: 1.0\n\
Content-Type: text/plain; charset=%s\n\
Content-Transfer-Encoding: %s\n",
		 Mime_Posting_Charset,
		 encoding);
     }
}

FILE *slrn_mime_encode (FILE *fp)
{
   if ((Mime_Posting_Encoding == ENCODED_7BIT)
       || (Mime_Posting_Encoding == ENCODED_8BIT))
     return fp;
   
   /* Add encoding later. */
   return fp;
}




#endif  /* SLRN_HAS_MIME */


