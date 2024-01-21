#include "headers.h"
#include "melib.h"

static void header_panic P_((char *,int,char *, char *)); /* Prototype */
static void header_panic(f,ln,pr,ms) 
     char * f;
     int ln;
     char *pr;
     char *ms;
{
  int do_cursor = RawState();

  dprint(1,(debugfile,"\nHEADER PANIC in %s:%d:%s\n",f,ln,pr));
  dprint(1,(debugfile,">>%s\n",ms));

  /* softkeys_off(); */

  if (do_cursor) {
    error1("HEADER PANIC: %s",ms);
    sleep(1);
  }

  if (do_cursor) {
    MoveCursor(elm_LINES, 0);
    Raw(OFF);
  }

  fprintf(stderr,"\nHEADER PANIC in %s:%d:%s\n",f,ln,pr);
  fprintf(stderr,">>%s\n",ms);

#if DEBUG
  if (debug > 10) {
    fprintf(stderr,"ABORTING...\n");
    fflush(stderr);
    abort();
  }
#endif
  emergency_exit();
}

/* Removes comments from string */
void rfc822_reap_comments (ptr, comments, size) 
     char *ptr, *comments;
     int size;
{
  char *w_ptr = ptr, *c_ptr = comments;
  int comment_level = 0, saved_level = 0;
  int in_quote = 0;
  
  while (*ptr) {
    if (*ptr == '\\' && (in_quote || comment_level > 0)) {		  
      /* \ escapes next character  
       * (not allowed outside of quotes or comments) */
      ptr++;
      if (*ptr == '\0')
	break;
      if (comment_level > 0 && comments) {
	if (c_ptr < comments + size - saved_level -3) {
	  *c_ptr++ = '\\';
	  *c_ptr++ = *ptr;
	}
      }
      if (comment_level == 0) {
	*w_ptr++ = '\\';
	*w_ptr++ = *ptr;
      }
      ptr++;
      continue;
    } else if (comment_level > 0) {
      if (*ptr == ')')
	comment_level --;
      if (*ptr == '(')
	comment_level ++;
      if (comments && c_ptr < comments + size - saved_level -3) {
	*c_ptr++ = *ptr;
	saved_level = comment_level;
      }
    } else if (*ptr == '\"') {
      if (in_quote)
	in_quote = 0;
      else
	in_quote = 1;
    } else if (!in_quote && *ptr == '(') {
      comment_level ++;
      if (comments && c_ptr < comments + size - saved_level -4) {
	if (c_ptr != comments)
	  *c_ptr++ = ' ';
	*c_ptr++ = *ptr;
	saved_level = comment_level;
      }
      *w_ptr++ = ' ';  /* RFC 822 (STD 11) says that 
			  comments represents one space */
    }
    if (comment_level == 0 && (in_quote || *ptr != ')'))
      *w_ptr++ = *ptr;
    ptr++;
  }
  while (comments && saved_level > 0) {
    *c_ptr++ = ')';
    saved_level--;
  }

  if (comments)
    *c_ptr = '\0';
  *w_ptr = '\0';
}

int read_header_line (fp, buf, size,flag) 
     FILE *fp;
     char *buf;
     int size;
     int flag;
{
  in_state_t state;
  int result;

  in_state_clear(&state,STATE_in_file);

  set_in_state_file(fp,&state);

  result = state_read_hdr_line(&state,buf,size,flag);

  in_state_destroy(&state);

  return result;
}

int state_read_hdr_line (s, buf, size,flag) 
     in_state_t *s;
     char *buf;
     int size;
     int flag;
{
  /* Read and unfold header line -- stores maximum size-1 bytes to buffer
   * (plus \0). Also handle case when headers are eneded either CR LF or LF.
   * Returns number of bytes stored. Always _read_ end of header
   * (even when buffer fills). Returns 0 when reads empty line (only CR LF).
   * That indicates end of headers.
   *
   * If flag & 1 (RHL_MARK_FOLDING) then folding is marked with '\n' 
   * (instead of ' ' and buffer ended with '\n' before '\0'
   * If flag & 2 (RHL_CHECK_HEADER) then check that this was header line...
   */

  int len = 0,c;
  int col_seen = 0;
  long pos;

  dprint(12,(debugfile,"state_read_hdr_line: size=%d, flag=%d\n",size,flag));
  size--; /* Place for \0 */

  if ( (!in_state_seekable(s) ||
	(pos = in_state_ftell(s)) < 0) && 
       (flag & RHL_CHECK_HEADER)) {
    dprint(12,(debugfile,"state_read_hdr_line=0; not seekable or ftell failed!"));
    buf[0] = '\0';
    return 0;
  }

#define PUTC(c) { if (len < size) buf[len++] = (c); }
  
  while (EOF != (c = state_getc(s))) {
    if ('\r' == c) {                /* Is this CR LF sequence ? */
      if (EOF == (c = state_getc(s)))
	break;
      if (c != '\n') 
	PUTC('\r');
    }
    if (c == '\n') {                /* Readed CR LF or LF, check folding */
      if (!col_seen && len > 0 && (flag & RHL_CHECK_HEADER)) {
      bad_header_line:
	dprint(12,(debugfile,
		   "state_read_hdr_line: Not ':' seen. Not a header line!\n"));
	if (0 != in_state_fseek(s,pos)) {
	  dprint(5,(debugfile,
		    "read_header_line: seek failed!\n"));
	}
	len = 0;
      }
      if (len == 0)
	break;                      /* End of headers ! */
      if (EOF == (c = state_getc(s)))
	break;
      if (c != ' ' && c != '\t') {   /* Not a continuation line */
	state_ungetc(c,s);
	break;
      }
      /* CRLF LWSP sequence should be replaced with ' ' */
      c = ' ';
      if (flag & RHL_MARK_FOLDING)
	c = '\n';
    }
    /* Space before ':' ? */
    if ((' ' == c || '\t' == c) && !col_seen && (flag & RHL_CHECK_HEADER)) {
      /* Skip to next ':' */
      while (' ' == c || '\t' == c)
	c = state_getc(s);
      if (':' != c) 
	goto bad_header_line;
    }
    if (':' == c)
      col_seen = 1;
    PUTC(c);
  }

  if (flag & RHL_MARK_FOLDING) {
    PUTC('\n');
  }

#undef PUTC
  buf[len] = 0;

  dprint(12,(debugfile,"state_read_hdr_line: len=%d, buf=%s\n",len,buf));

  return len;
}

static struct header_info header_types[] = {
  /* From STD 11 (RFC 822): */
  { "Subject",    HDR_TEXT },
  { "Comments",   HDR_TEXT },
  { "Return-path",HDR_STRUCTURED },
  { "Received",   HDR_STRUCTURED },
  { "Reply-To",   HDR_STRUCTURED|HDR_PHRASE },    /* Not exactly correct .. */
  { "From",       HDR_STRUCTURED|HDR_PHRASE },    /* Not exactly correct .. */
  { "Sender",     HDR_STRUCTURED|HDR_PHRASE },    /* Not exactly correct .. */
  { "Resent-Reply-To", HDR_STRUCTURED|HDR_PHRASE },/* Not exactly correct .. */
  { "Resent-From",     HDR_STRUCTURED|HDR_PHRASE },/* Not exactly correct .. */
  { "Resent-Sender",   HDR_STRUCTURED|HDR_PHRASE },/* Not exactly correct .. */
  { "Date",        HDR_STRUCTURED },
  { "Resent-Date", HDR_STRUCTURED },
  { "To",          HDR_STRUCTURED|HDR_PHRASE }, /* Not exactly correct .. */
  { "Resent-To",   HDR_STRUCTURED|HDR_PHRASE }, /* Not exactly correct .. */
  { "cc",          HDR_STRUCTURED|HDR_PHRASE }, /* Not exactly correct .. */
  { "Resent-cc",   HDR_STRUCTURED|HDR_PHRASE }, /* Not exactly correct .. */
  { "bcc",         HDR_STRUCTURED|HDR_PHRASE }, /* Not exactly correct .. */
  { "Resent-bcc",  HDR_STRUCTURED|HDR_PHRASE }, /* Not exactly correct .. */
  { "Message-ID",  HDR_STRUCTURED },
  { "Resent-Message-ID",  HDR_STRUCTURED },
  { "In-Reply-To", HDR_STRUCTURED|HDR_PHRASE }, /* Is correct .. */
  { "References",  HDR_STRUCTURED|HDR_PHRASE }, /* Is correct .. */
  { "Keywords",  HDR_STRUCTURED|HDR_PHRASE },   /* Is correct :-) .. */
  { "Encrypted", HDR_STRUCTURED },              /* Well ... */
  /* From MIME (RFC 1521) */
  { "MIME-Version",              HDR_STRUCTURED },     
  { "Content-Type",              HDR_STRUCTURED },     
  { "Content-Transfer-Encoding", HDR_STRUCTURED },
  { "Content-ID",                HDR_STRUCTURED },     
  { "Content-Description",       HDR_TEXT },
  /* From RFC 1806 */
  { "Content-Disposition",       HDR_STRUCTURED },
  /* From RFC 1864 */
  { "Content-MD5",               0 },

  /* mailbox format */
  { "Content-Length",      0 },
  { "Status",              0 },

  /* Sendmail */
  { "Full-Name",           HDR_TEXT },
  { "Return-Receipt-To",   HDR_STRUCTURED|HDR_PHRASE }, /* Not exactly correct .. */
  { "Auto-Submitted",      HDR_STRUCTURED },
  { "Precedence",          0 },


  /* IDA Sendmail */
  { "X-Charset",           0 },
  { "X-Char-Esc",          0 },

  /* Unknown source */
  { "Action",              0 },
  { "Priority",            HDR_STRUCTURED },
  { "Expires",             HDR_STRUCTURED },
  { "Importance",          HDR_STRUCTURED },
  { "Sensitivity",         HDR_STRUCTURED },

  /* Our non-standard headers */
  { "X-ELM-OSV",                 HDR_STRUCTURED },
  { "X-Mailer",                  HDR_TEXT },
  { "Content-Name",              HDR_TEXT },
  

  /* Tailer */
  { NULL, 0 }
};

/* Grows forever */
static struct hdr_list {
  struct hdr_list * next;
  struct header_info hdr;
} * extra_headers = NULL;
static int extra_header_count = 0;

header_ptr find_header(name, create_flag) 
     char *name;
     int create_flag;
{
  int i;
  struct hdr_list * walk, *last = NULL;

  for (i = 0; header_types[i].header; i++) 
    if (strincmp(name,header_types[i].header,STRING) == 0)
      return &(header_types[i]);

  for (walk = extra_headers; 
       walk != NULL; 
       last = walk, walk = walk -> next)
    if (strincmp(name,walk->hdr.header,STRING) == 0)
      return &(walk->hdr);
  
  if (create_flag) {
    walk = (struct hdr_list * ) safe_malloc (sizeof(struct hdr_list));

    if (last) last->next = walk;
    else extra_headers   = walk;
    extra_header_count++;

    walk->next = NULL;
    walk->hdr.header = strmcpy(NULL,name);
    if (strincmp(name,"X-",2) == 0)
      walk->hdr.flag = HDR_TEXT;     /* Default for user defined headers */
    else
      walk->hdr.flag = 0;            /* We don't know it, right? */

    return &(walk->hdr);
  }
  
  return NULL;
}

int classify_header(name)
     char *name;
{
  header_ptr P = find_header(name,0);

  if (P)
    return P->flag;

  if (strincmp(name,"X-",2) == 0)
    return HDR_TEXT;              /* Default for user defined headers */

  return 0;
}

long skip_envelope(hdr, fp)
     struct header_rec *hdr;
     FILE *fp;
{
  char buf[STRING];
  int tmp;

  long result = hdr->offset;

  if (0 !=  fseek(fp,hdr->offset,SEEK_SET)) {
    error1("Failed to seek beginning of mail envelope (%ld)",hdr->offset);
    sleep_message();
    dprint(9,(debugfile,"skip_envelope=-1 (fseek error)\n"));

    return -1;
  }

  dprint(9,(debugfile,"skip_envelope: scanning offset: %ld\n",
	    result));;

  while (0 < (tmp = mail_gets(buf,sizeof(buf),fp))) {
    dprint(9,(debugfile,"skip_envelope: len=%d, got: %s\n",tmp,buf));
#ifdef MMDF
    if (0 == strcmp(buf,MSG_SEPARATOR))
      continue;
#endif
    if (0 == strncmp(buf,"From ",5))
      continue;
    break;
  }
  result = ftell(fp) - tmp;

  dprint(9,(debugfile,"skip_envelope: beginning of headers=%ld\n",result));
  if (0 !=  fseek(fp,result,SEEK_SET)) {
    error1("Failed to seek beginning of mail headers (%ld)",result);
    sleep_message();
    dprint(9,(debugfile,"skip_envelope=-1 (fseek error)\n"));
    return -1;
  }

  dprint(9,(debugfile,"skip_envelope=%ld\n",result));
  return result;
}

header_list_ptr locate_header(h,n)
     header_list_ptr h;
     header_ptr n;
{
  header_list_ptr walk;

  for (walk = h; 
       walk != NULL; 
       walk = walk -> next_other_header) {
    if (walk -> magic != HEADER_magic)
      header_panic(__FILE__,__LINE__,"locate_header",
		   "Bad magic number");
    if (n == walk -> header_name)
      return walk;
  }
  return NULL;
}

header_list_ptr file_read_headers(fp, flag) 
     FILE * fp;
     int flag;
{
  in_state_t state;
  header_list_ptr result;

  in_state_clear(&state,STATE_in_file);

  set_in_state_file(fp,&state);

  result = state_read_headers(&state, flag);

  in_state_destroy(&state);

  return result;
}

header_list_ptr state_read_headers(s, flag) 
     in_state_t * s;
     int flag;
{
  char buffer[32*1024+1];
  int size;

  header_list_ptr result = NULL, last = NULL;

  dprint(12,(debugfile,"state_read_headers() --> START\n"));

  while (size = state_read_hdr_line(s,buffer,sizeof buffer,
				    RHL_CHECK_HEADER|flag) > 0) {
    header_list_ptr item, last_this = NULL, walk;
    char * k;
    if (0 == strcmp(buffer,"\n"))
      break;
    k = strchr(buffer,':');
    if (!k)
      break;
    *k = '\0';
    k++;
    item = (struct header_list *) safe_malloc(sizeof (struct header_list));
    item -> header_name = find_header(buffer,1);
    
    item -> next_header       = NULL;
    item -> next_this_header  = NULL;
    item -> next_other_header = NULL;
    item -> body              = strmcpy(NULL,k);
    item -> magic             = HEADER_magic;
    
    for (walk = result; 
	 walk != NULL; 
	 last_this = walk, walk = walk -> next_other_header) {
      if (walk -> magic != HEADER_magic)
	header_panic(__FILE__,__LINE__,"state_read_headers",
		     "Bad magic number");
      if (item -> header_name == walk -> header_name)
	break;
    }

    if (walk) {  
      while (walk->next_this_header != NULL) {
	if (walk -> magic != HEADER_magic)
	  header_panic(__FILE__,__LINE__,"state_read_headers",
		       "Bad magic number");
	walk = walk->next_this_header;
      }

      walk->next_this_header = item;

      dprint(12,
	     (debugfile,
	      "state_read_headers- header='%s' -- append next_this_header (%s)\n",
	      item->header_name->header,
	      walk->header_name->header));
		 
    } else if (last_this) {
      last_this -> next_other_header = item;
      dprint(12,
	     (debugfile,
	      "state_read_headers- header='%s' -- append next_other_header (%s)\n",
	      item->header_name->header,
	      last_this->header_name->header));
    }

    if (last) {
      last -> next_header  = item;

      dprint(12,
	     (debugfile,
	      "state_read_headers: header='%s' -- append next_header (%s)\n",
	      item->header_name->header,
	      last->header_name->header));

    }
    else      {
      result               = item;
      dprint(12,
	     (debugfile,
	      "state_read_headers: header='%s' -- head of next_header\n",
	      item->header_name->header));
    }
    last = item;

  }

  dprint(12,(debugfile,"state_read_headers()=%p <-- END\n",result));
  return result;
}

void delete_headers(hdr)
     header_list_ptr hdr;
{
  header_list_ptr next = hdr;
  
  while(next) {
    if (next -> magic != HEADER_magic)
      header_panic(__FILE__,__LINE__,"delete_headers","Bad magic number");

    hdr  = next;
    next = next -> next_header;

    if(hdr -> body) {
      free(hdr -> body);
      hdr -> body = NULL;
    }
    hdr -> next_header       = NULL;
    hdr -> next_this_header  = NULL;
    hdr -> next_other_header = NULL;
    hdr -> magic             = 0;
    free((void *)hdr);
  }
}

int NULL_header_filter(hdr,flag)
     header_list_ptr hdr;
     int flag;
{
  if (hdr -> magic != HEADER_magic)
    header_panic(__FILE__,__LINE__,"NULL_header_filter","Bad magic number");

  flag++;      /* So that flag is used */
  return 1;
}

void NULL_header_converter (hdr,flag,buffer,size)
     header_list_ptr hdr;
     int flag;
     char *buffer;
     int size;
{
  if (hdr -> magic != HEADER_magic)
    header_panic(__FILE__,__LINE__,"NULL_header_converter","Bad magic number");

  flag++;      /* So that flag is used */
  
  if (hdr->body)
    strfcpy(buffer,hdr->body,size);
  else
    buffer[0] = '\0';
}

void state_write_headers(s,hdr,filter,convert,flag) 
     out_state_t * s;
     header_list_ptr hdr;
     header_filter    * filter;
     header_converter * convert;
     int                flag;
{
  header_list_ptr next = hdr;
  int ret;
  
  for (next = hdr; next; next = next -> next_header) {
    char buffer [ 32 * 1024 + 1], *ptr;
    if (next -> magic != HEADER_magic)
      header_panic(__FILE__,__LINE__,"state_write_headers",
		   "Bad magic number");

    if (! (ret = filter(next,flag))) {
      dprint(12,(debugfile,
		 "state_write_headers: header='%s', {filter}=%d FILTERED\n",
		 next->header_name->header, ret));
      continue;
    } else {
      dprint(12,(debugfile,
		 "state_write_headers: header='%s', {filter}=%d PASSED\n",
		 next->header_name->header, ret));
    }

    buffer[0] = '\0';

    convert(next,flag,buffer,sizeof buffer);
    
    buffer[sizeof buffer -1] = '\0';

    state_add_prefix(s);
    state_puts(next->header_name->header,s);
    state_puts(": ",s);

    for (ptr = strtok(buffer,"\n"); ptr; ptr = strtok(NULL,"\n")) { 
      if (ptr > buffer) { /* Do folding */
	--ptr;
	if (*(ptr+1) == ' ')
	  *ptr = ' ';
	else
	  *ptr = '\t';
	state_putc('\n',s);
	state_add_prefix(s);
      }
      state_puts(ptr,s);
    }
    state_putc('\n',s);
  }
}

