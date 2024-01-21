#include "headers.h"
#include "melib.h"

#ifdef MIME

static int index_hex[128] = {
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
     0, 1, 2, 3,  4, 5, 6, 7,  8, 9,-1,-1, -1,-1,-1,-1,
    -1,10,11,12, 13,14,15,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,10,11,12, 13,14,15,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1
};

static int index_64[128] = {
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,62, -1,-1,-1,63,
    52,53,54,55, 56,57,58,59, 60,61,-1,-1, -1,-1,-1,-1,
    -1, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
    15,16,17,18, 19,20,21,22, 23,24,25,-1, -1,-1,-1,-1,
    -1,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
    41,42,43,44, 45,46,47,48, 49,50,51,-1, -1,-1,-1,-1
};

#define base64(c) ((((c) > 0) && ((c) < 127)) ? index_64[ (c) ] : -1)
#define hex(c) ((((c) > 0) && ((c) < 127)) ? index_hex[ (c) ] : -1)

/* Prototype */
static void Xbit_decode	P_((in_state_t *, out_state_t *, int, int)); 

static void Xbit_decode (s_in, s_out, len, is_text)
     in_state_t *s_in;
     out_state_t *s_out;
     int len;
     int is_text;
{
  char buf[VERY_LONG_STRING];
  int offset = 0, length;

  if (s_out->prefix) {
    strfcpy (buf, s_out->prefix, sizeof buf);
    offset = strlen (buf);
  }
  while (len > 0) {
    if (0 ==
	(length = state_getl (buf + offset, sizeof buf - offset, s_in)))
      break;
    len -= length;

    /* Take care of CRLF => LF conversion */
    if (is_text > 0 &&
	length > 1 &&
	buf[offset + length - 1] == '\n' &&
        buf[offset + length - 2] == '\r') {
      buf[offset + length - 2] = '\n';
      buf[offset + length - 1] = '\0';
      length--;
    }

    state_put(buf,offset+length,s_out);
  }
}

void base64_decode (s_in, s_out, length, astext)
     in_state_t  *s_in;
     out_state_t *s_out;
     int length;
     int astext;
{
  /* Notes:  this routine assumes that strlen(line length) % 4 == 0.
     Most intelligent software will do this, so I haven't worried
     about being *completely* compliant with the standard.  I've
     not run into any encoded data where this was not true.  */
  
  char buf[STRING], *p;
  int
    bytes = 0, /* the total number of bytes read so far */
    terminate = FALSE,
    c1, c2, c3, c4;
  unsigned char
    ch,
    store_ch = 0; /* for astext */
  int corrupted = FALSE;

  dprint(10,(debugfile,
           "base64_decode: length=%d, prefix=%s, astext=%d\n",
           length,
           s_out->prefix ? s_out->prefix : "<NULL>",
           astext));

  /* Make sure to quote the first line! */
  state_add_prefix(s_out);

  while (! terminate) {
    int len;

    if (bytes >= length)
      break;

    if ((len = state_getl (buf, VERY_LONG_STRING, s_in)) <= 0)
	break;

    bytes += len;

    p = buf;
    while (*p) {
      c1 = *p++;

      if (base64(c1) == -1) {
        p--;
        /* partial fix -- K E H   <hurtta@dionysos.FMi.FI>  */

        while (*p == ' ' || *p == '\t' || *p == '\r')
          p++;

        if (*p != '\n')
          corrupted = TRUE;  /* Perhaps corrupted */

        if (*p == 0)
          break;

        c1 = *p++;
      }

      /* End of the line. */
      if (c1 == '\n')
	continue;

      c2 = *p++;
      
      c1 = base64(c1);
      if (c1 == -1)
        corrupted = TRUE;
      c2 = base64(c2);
      if (c2 == -1)
        corrupted = TRUE;
      ch = (c1 << 2) | (c2 >> 4);
      
      if (store_ch && ch != '\n') /* for astext */
	state_putc(store_ch,s_out);

      store_ch = 0;
      if (astext && ch == '\r')
        store_ch = ch;
      else {
	state_putc(ch,s_out);
	if (ch == '\n')
	  state_add_prefix(s_out);
      }      
    
      c3 = *p++;
      if (c3 == '=') {
	terminate = TRUE;
	break;
      }
      c3 = base64(c3);
      if (c3 == -1)
	corrupted = TRUE;
      ch = ((c2 & 0xf) << 4) | (c3 >> 2);

      if (store_ch && ch != '\n')
	state_putc(store_ch,s_out);

      store_ch = 0;

      if (astext && ch == '\r')
	store_ch = ch;
      else {
	state_putc(ch,s_out);
	if (ch == '\n')
	  state_add_prefix(s_out);
      }
  
      c4 = *p++;
      if (c4 == '=') {
	terminate = TRUE;
	break;
      }
      
      c4 = base64(c4);
      if (c4 == -1)
	corrupted = TRUE;
      ch = ((c3 & 0x3) << 6) | c4;
      
      if (store_ch && ch != '\n')
	state_putc(store_ch,s_out);

      store_ch = 0;

      if (astext && ch == '\r') {
	store_ch = ch;
      }
      else {
	state_putc(ch,s_out);
	if (ch=='\n')
	  state_add_prefix(s_out);
      }
    }
  }

  /* Make sure to flush anything left in the internal buffer. */
  if (store_ch)  /* for astext */
    state_putc(store_ch,s_out);

  if (corrupted) {
    if (s_out->displaying)
      state_puts("\n[BASE64 data was corrupt!]\n",s_out);
    error ("BASE64 data was corrupt!");
    sleep_message();
  }

  dprint(10,(debugfile,"base64_decode: readed=%d bytes, corrupted=%d.\n",
	     bytes,corrupted));   

  return;
}
  
void quoted_printable_decode (s_in, s_out, length, astext)
     in_state_t  *s_in;
     out_state_t *s_out;
     int length;
     int astext;
{
  int bytes = 0; /* number of bytes read */
  int nl = TRUE; /* flag to indicate hitting a new line */
  char *p;
  int c1, c2;
  unsigned char ch, store_ch = 0;
  char buf[VERY_LONG_STRING];
  int corrupted = 0;

  dprint(10,(debugfile,
	     "quoted_printable_decode: length=%d, prefix=%s, astext=%d\n",
	     length,
	     s_out->prefix ? s_out->prefix : "<NULL>",
	     astext));

  for (;;) {
    int len;

    if (bytes >= length)
      break;

    if ((len=state_getl (buf, VERY_LONG_STRING, s_in)) <= 0)
      break;
    bytes += len;

    p = buf;
    while (*p) {

      /* If there is a prefix and this is the beginning of a new line... */
      if (nl) {
	state_add_prefix(s_out);
	nl = FALSE;
      }

      if (store_ch)
	state_putc(store_ch,s_out);
      
      if (*p == '=') {
	p++;
	/* Ignore spaces in end of line   -- see MIME */
	if (*p == '\r' || *p == ' ' || *p == '\t') {
	  char *t = p;
	  while (*t && (*t == '\r' || *t == ' ' || *t == '\t'))
	    t++;
	  if (*t && *t == '\n')
	    p = t;
	}

	if (*p == '\n') { /* soft linebreak */
	  if (length <= 0)
	    break;
	  p++;
	}
	else {
	  c1 = hex(*p);
	  if (c1 == -1)
	    corrupted = TRUE;
	  p++;
	  c2 = hex(*p);
	  if (c2 == -1)
	    corrupted = TRUE;
	  p++;
	  ch = (c1 << 4) | c2;

	  /* We not need here CR LF -> LF removing, because
	   * CRLF's which presents end of line should NOT be encoded.
	   *                             - K E H <hurtta@dionysos.FMI.FI> */

	  state_putc(ch,s_out);
	}
      }
      else {
       if (astext && *p == '\r')
         store_ch = *p;
       else
	 state_putc(*p,s_out);
	 
       if (*p == '\n') {
	 nl = TRUE;
	 if (length <= 0)
	   break;
       }

       p++;
      }  
    }
  }

  /* Flush anything left in the buffer */
  if (store_ch) /* for astext */
    state_putc(store_ch,s_out);

  if (corrupted) {
    if (s_out -> displaying)
      state_puts("\n[Seems that QUOTED-PRINTABLE data was corrupted.]\n",
		 s_out);
    error ("Seems that QUOTED-PRINTABLE data was corrupted.");
    sleep_message();
  }
  
  dprint(10,(debugfile,
	     "quoted_printable_decode: readed=%d bytes, corrupted=%d.\n",
	     bytes,corrupted));

  return;
}

/* Prototype */
static void multipart_decode  P_((mime_t *, 
				  in_state_t *, out_state_t *)); 

static void multipart_decode (att, state_in, state_out)
     mime_t *att; /* The list of attachments for this MULTIPART data */
     in_state_t   *state_in;
     out_state_t  *state_out;
{
  int nattach = 0;
  char buf[VERY_LONG_STRING];

  dprint(12,(debugfile,
	     "multipart_decode -> START\n"));

  if (att->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"multipart_decode",
	       "Bad magic number");

  if (!in_state_seekable(state_in)) {
    state_puts("[multipart_decode: unsupported input state]\n",state_out);
    error("multipart_decode: unsupported input state");
    sleep_message();
    return;
  }

  while (att) {

    if (att->magic != MIME_magic)
      mime_panic(__FILE__,__LINE__,"multipart_decode",
		 "Bad magic number (next -chain)");

    nattach++;

    dprint(12,(debugfile,
	       "multipart_decode: [%d]: Type: %.15s/%.30s, Encoding: %d, Size: %d\n",
	       nattach, 
	       mime_types[att->type], att->subtype,
	       att->encoding, att->length));

    if (in_state_fseek(state_in,att->offset) != 0) {
      state_puts("[multipart_decode: seek failed]\n",state_out);
      error("multipart_decode: seek failed");
      sleep_message();
      return;
    }

    if (state_out->displaying) {
      char Buffer[70], tmp[40];
      char *Part = "Part";
      char *Encoding = "???";
       
      Buffer[0] = '\0';
      tmp[0] = '\0';
      if (att->description) {
	sprintf(Buffer,"\"%.60s\"]\n[",att->description);
      } else if (att->disposition_opts &&
		 mime_get_param ("filename", tmp, att->disposition_opts, sizeof(tmp))) {
	sprintf(Buffer,"Filename: %.60s]\n[",tmp);
      }

      if (att->disposition != DISP_INLINE)
	Part="Attach";

      Encoding = ENCODING(att->encoding);
      
      /* First print out a nice display line about this attachment */
      sprintf (buf,
	       "%s[%s #%d: %sType: %.15s/%.30s, Encoding: %s, Size: %d]\n",
               nattach > 1 ? "\n" : "",
	       Part,
	       nattach, Buffer,
	       mime_types[att->type], att->subtype,
	       Encoding, att->length);
    }
    else {
      buf[0] = '\0';
      if (nattach > 1)
        strfcpy (buf, "\n", sizeof buf);
      if (att->description && (strlen(att->description) < sizeof(buf) -30)) {
	strfcat (buf, "Content-Description: ", sizeof buf);
	strfcat (buf, att->description, sizeof buf);
	strfcat (buf, "\n\n", sizeof buf);
      }
    }
      
    state_puts(buf,state_out);
    if (state_out->displaying)
      state_putc('\n', state_out);
    mime_decode (att, state_in, state_out);
    att = att->next;
  }
  dprint(12,(debugfile,
	     "multipart_decode <- END\n"));
}

/* Prototype */
static void multipart_0_decode P_((mime_t *, 
				   in_state_t *, out_state_t *)); 

static void multipart_0_decode (ptr, state_in, state_out)
     mime_t *ptr;
     in_state_t *state_in;
     out_state_t *state_out;
{
  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"multipart_0_decode",
	       "Bad magic number");

  if (ptr->parts)
    multipart_decode (ptr->parts, state_in, state_out);
  else {
    state_puts ("[Content-Type: multipart/*, no subtypes (parts = NULL)]\n", 
		state_out);
    error("Content-Type: multipart/*, no subtypes (parts = NULL)");
    sleep_message();
  }
}

/* Prototype */
static void rfc822_decode P_((mime_t *, 
			      in_state_t *, out_state_t *)); 

static int rfc822_header_filter     P_((header_list_ptr, int));
static void rfc822_header_converter P_((header_list_ptr, int, char *, int));

static int rfc822_header_filter(hdr,flag)
     header_list_ptr hdr;
     int flag;                  /* elm_filter */
{
  char buf[80];

  strfcpy(buf,hdr->header_name->header,78);
  strfcat(buf,": ", sizeof buf);
  
  if (flag) {
    if (matches_weedlist (buf)) 
      return 0;
    else
      return 1;
  } else
    return 1;
}

static void rfc822_header_converter (hdr,flag,buffer,size)
     header_list_ptr hdr;
     int flag;
     char *buffer;
     int size;
{
  int class = hdr->header_name->flag;

  if (!hdr->body) {
    buffer[0] = '\0';
    return;
  }

  dprint(12,(debugfile,"rfc822_header_converter: header=%s,class=%d,rest=%s",
	     hdr->header_name->header,class,hdr->body));

  flag++;      /* So that flag is used */

  strfcpy(buffer,hdr->body,size);

  if (class & HDR_TEXT) {
    if (is_rfc1522 (buffer))
      rfc1522_decode (buffer, size);
  } else if (class & HDR_STRUCTURED) {
    rfc1522_decode_structured(class,buffer,size);
  }

  dprint(12,(debugfile,"rfc822_header_converter: decoded rest=%s",buffer));

}

static void rfc822_decode (mt, state_in, state_out)
     mime_t *mt;
     in_state_t *state_in;
     out_state_t *state_out;
{
  /* This looks a lot like multipart_decode() except that we want to print
   * the headers for the message.  NOTE: "mt" should be a pointer to the
   * RFC822 attachment, NOT its subpart, since we need the offset in order
   * to print out the headers.
   */

  header_list_ptr headers = NULL;
  header_list_ptr From_header, mime_version, osv;
  int decode_headers = 1;

  if (mt->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"rfc822_decode",
	       "Bad magic number");
  
  if (!in_state_seekable(state_in)) {
    state_puts("[rfc822_decode: unsupported input state]\n",state_out);
    error("rfc822_decode: unsupported input state");
    sleep_message();
    return;
  }

  if (in_state_fseek(state_in,mt->offset) != 0) {
    state_puts("[rfc822_decode: seek failed]\n",state_out);
    error("rfc822_decode: seek failed");
    sleep_message();
    return;
  }

  headers = state_read_headers(state_in, RHL_MARK_FOLDING);

  mime_version = locate_header_by_name(headers,"MIME-Version");
  osv          = locate_header_by_name(headers,"X-ELM-OSV");

  if (!mime_version && !req_mime_hdrencoding && !req_mime_hdrencoding)
    decode_headers = 0;
  if (osv && osv->body) {
    char value[20];
    if (mime_get_param("no-hdr-encoding",value,osv->body,
		       sizeof value) && atoi(value) > 0)
      decode_headers = 0;
  }

  From_header  = locate_header_by_name(headers,"From");

  if (From_header) {
    char buffer[1024+1],buffer2[1024+50];
    strfcpy(buffer,From_header->body,sizeof buffer);
    if (decode_headers && is_rfc1522 (buffer))
      rfc1522_decode_structured (From_header->header_name->flag,
				 buffer, sizeof buffer);
    sprintf(buffer2,"-- Start of included mail From: %s\n",buffer);
    state_puts(buffer2,state_out);
  }
  else
    state_puts("-- Start of included mail.\n",state_out);

  state_write_headers(state_out,headers,
		      rfc822_header_filter,
		      decode_headers ? rfc822_header_converter :
		      NULL_header_converter,
		      elm_filter);

  delete_headers(headers); headers = NULL;

  state_putc('\n',state_out);

  if (mt->parts == NULL) {
    state_puts("[rfc822_decode: no body of RFC 822 mail (parts = NULL)]\n",
	       state_out);
    error("rfc822_decode: no body of RFC 822 mail (parts = NULL)");
    sleep_message();
    return;
  }

  if (mt->parts->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"rfc822_decode",
	       "Bad magic number (parts)");

  mime_decode (mt->parts, state_in, state_out);
  state_puts("-- End of included mail.\n",state_out);
  return;
}

static int ASCII_filter(c,st)
     char c;
     struct out_state * st;
{
  int res = (unsigned char) c;
  st++;

  if (res & 128)
    res = '_';
  return res;
}

/* Prototype */
static int run_decoder P_((mime_t *, in_state_t *, out_state_t *)); 

static int run_decoder (ptr, state_in, state_out) 
     mime_t *ptr;
     in_state_t *state_in;
     out_state_t *state_out;
{
  int is_text;
  dprint(12,(debugfile,
	     "run_decoder -> state: offset=%d, length=%d\n",
	     ptr -> offset, ptr -> length));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"run_decoder",
	       "Bad magic number");


  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "              (file): ftell=%ld\n",in_state_ftell(state_in))); 
  } else {
    dprint(12,(debugfile,
	       "              (\?\?\?\?): magic=%d\n",state_in->magic));
  }

  is_text = is_text_type (mime_types[ptr->type], ptr->subtype, 
			  ptr->encoding);

  dprint(12,(debugfile,
	     "run_decoder: is_text=%d; type=%s (%d)/%s; encoding =%s (%d)\n",
	     is_text, mime_types[ptr->type], ptr->type, ptr->subtype,
	     ENCODING(ptr->encoding),
	     ptr->encoding));


  if (ptr->encoding == ENCODING_BASE64)
    base64_decode (state_in, state_out, ptr->length, is_text);
  else if (ptr->encoding == ENCODING_QUOTED)
    quoted_printable_decode (state_in, state_out, ptr->length, is_text);
  else if (ptr->encoding != ENCODING_NONE && 
	   ptr->encoding != ENCODING_7BIT &&
	   ptr->encoding != ENCODING_8BIT &&
	   ptr->encoding != ENCODING_BINARY) {

    dprint(12,(debugfile,
	       "run_decoder=0 <- END; \n"));
    if (in_state_seekable(state_in)) {
      dprint(12,(debugfile,
		 "              (file); ftell=%ld\n",
		 in_state_ftell(state_in))); 
    }
    return 0;
  }
  else
    Xbit_decode (state_in, state_out, ptr->length, is_text);

  dprint(12,(debugfile,
	     "run_decoder=1 <- END; \n"));
  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "              (file); ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }
  
  return 1;
}

int set_filter (ptr, state) 
     mime_t *ptr;
     out_state_t *state;
{
  char tmp[SLEN];
  char buf[STRING];
  int code;

  dprint(12,(debugfile,
	     "set_filter -> state: offset=%d, length=%d\n",
	     ptr -> offset, ptr -> length));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"set_filter",
	       "Bad magic number");

  state -> filter = NULL_filter;

  dprint(12,(debugfile,
	     "set_filter: filter=%p (NULL_filter)\n",
	     (void *)(state -> filter)));

  if (0 == (code = mime_get_charset (tmp, ptr->type_opts, sizeof(tmp)))) {
    
    /* Don't show this part (or copy to reply buffer) ... */
	  
    sprintf (buf, "[Charset %.15s unsupported, skipping...]\n", tmp);
    state_puts(buf,state);
    if (state->displaying)
      state_puts(   "[Use 'v' to view or save this part.]\n",state);

    dprint(12,(debugfile,
	       "set_filter=0 <- END\n"));
    return 0;
  } else {
    if (code < 0) {
      sprintf (buf, "[Charset %.15s unsupported, filtering to ASCII...]\n",
	       tmp);
      state_puts(buf,state);
      if (state->displaying)	
	state_puts(   "[You can also use 'v' to view or save this part.]\n\n",
		   state);
      
      state -> filter = ASCII_filter;

      dprint(12,(debugfile,
		 "set_filter: filter=%p (ASCII_filter)\n",
		 (void *)(state -> filter)));
    }
    dprint(12,(debugfile,
	       "set_filter=1 <- END\n"));
    return 1;
  }
}

/* Prototype */
static void text_decode	P_((mime_t *, in_state_t *, out_state_t *)); 

static void text_decode (ptr, state_in, state_out) 
     mime_t *ptr;
     in_state_t *state_in;
     out_state_t *state_out;
{
  dprint(12,(debugfile,
	     "text_decode -> state: offset=%d, length=%d \n",
	     ptr -> offset, ptr -> length));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"text_decode",
	       "Bad magic number");

  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "              (file): ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }

  if (set_filter(ptr,state_out)) {
    if (!run_decoder(ptr,state_in, state_out)) {
      state_puts("[Unsupported encoding, skipping...]\n",state_out);
      if (state_out->displaying)	
	state_puts("[Use 'v' to save this part in encoded form.]\n",state_out);
    }
  }

  state_out -> filter = NULL_filter;

  dprint(12,(debugfile,
	     "text_decode <- END; \n"));
  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "          (file); ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }
}

FILE * arrange_decoded(ptr,state_in,state_out,newstate2)
     mime_t *ptr;
     in_state_t  *state_in;
     out_state_t *state_out;
     in_state_t *newstate2;
{
  FILE * tmpfp = NULL;
  int result = 1,len;
  char fname[STRING];
  out_state_t newstate;

  out_state_clear(&newstate,STATE_out_file);
  
  dprint(12,(debugfile,
	     "arrange_decoded -> state: offset=%d, length=%d \n",
	     ptr -> offset, ptr -> length));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"arrange_decoded",
	       "Bad magic number");

  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "                 (file): ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }
  
  sprintf (fname, "%selmdecode.%d", temp_dir, getpid ());
  if (NULL == (tmpfp = safeopen_rdwr(fname))) {
    error("Failed to create file for decoding.");
    state_puts("[Failed to create file for decoding.]\n",state_out);
    sleep_message();
    result = 0;
  } else { /* Tempfile opened */
    int crypted = OFF;
    int bytes = 0;
    unlink(fname);  /* We can unlink it now ... */
    
    newstate.displaying = state_out->displaying;
    
    set_out_state_file(tmpfp, &newstate);
    newstate.prefix     = NULL;             /* No prefix in that pass */
    newstate.filter     = state_out->filter;
    
    if (run_decoder(ptr,state_in,&newstate)) { 
      int len;
      
      if (EOF == fflush(tmpfp)) {
	error("Error when flushing temporary file.");
	state_puts("[Error when flushing temporary file.]\n",state_out);
	sleep_message();
	result = 0;
      }
      rewind(tmpfp); /* Rewind it for reading */
      
      set_in_state_file(tmpfp,newstate2);
    } else { /* Run decoder failed */
      state_puts("[Unsupported content-transfer-encoding, skipping...]\n",
		 state_out);
      if (state_out->displaying)	
	state_puts("[Use 'v' to save this part in encoded form.]\n",
		   state_out);
      result = 0;
    }
  }
  out_state_destroy(&newstate);

  dprint(12,(debugfile,"arrange_decoded; result=%d\n",result));
  
  if (!result && tmpfp) {
    fclose(tmpfp);
    tmpfp = NULL;
  }
  return tmpfp;
}

/* Prototype */
static void elm_decode	P_((mime_t *, in_state_t *, out_state_t *)); 

static void elm_decode (ptr, state_in, state_out) 
     mime_t *ptr;
     in_state_t  *state_in;
     out_state_t *state_out;
{  
  in_state_t newstate2;

  FILE *tmpfp;
  char buffer[LONG_STRING];
  
  in_state_clear(&newstate2,STATE_in_file);
  
  dprint(12,(debugfile,
	     "elm_decode -> state: offset=%d, length=%d; \n",
	     ptr -> offset, ptr -> length));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"elm_decode",
	       "Bad magic number");

  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "             (file): ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }

  if (tmpfp = arrange_decoded(ptr,state_in,state_out,&newstate2)) {
    if (set_filter(ptr,state_out)) {
      int len, crypted = 0;
      int count = 0;
      getkey(OFF);
      
      while((len = state_getl(buffer,sizeof(buffer),&newstate2)) > 0) {
	count += len;
	  
	if (!strncmp(buffer, START_ENCODE, strlen(START_ENCODE))) {
	  state_puts("-- Start of (Elm) encoded section.\n",state_out);
	  crypted = ON;
	  continue;
	} else if (!strncmp(buffer, END_ENCODE, strlen(END_ENCODE))) {
	  crypted = OFF;
	  state_puts("-- End of (Elm) encoded section.\n",state_out);
	  continue;
	} else if (crypted) {
	  no_ret(buffer);
	  encode(buffer);      
	  strfcat(buffer, "\n", sizeof buffer);
	}
	state_add_prefix(state_out);
	state_puts(buffer,state_out);
      }

      dprint(12,(debugfile,
		 "elm_decode: Readed %d bytes from temp file\n",count));
    }
    fclose(tmpfp);
  }

  dprint(12,(debugfile,
	     "elm_decode <- END; \n"));
  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "           (file); ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }

  in_state_destroy(&newstate2);
}
 /* Prototype */
static void text_unsupported_decode P_((mime_t *, 
					in_state_t *, out_state_t *));

static void text_unsupported_decode (ptr, state_in, state_out) 
     mime_t *ptr;
     in_state_t *state_in;
     out_state_t *state_out;
{
  char buf[STRING];

  dprint(12,(debugfile,
	     "text_unsupported_decode -> state: offset=%d, length=%d; \n",
	     ptr -> offset, ptr -> length));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"text_unsupported_decode",
	       "Bad magic number");

  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "                          (file): ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }


  sprintf (buf, "[%s/%.30s is unsupported, treating like TEXT/PLAIN]\n\n", 
	   TYPE(ptr->type), ptr->subtype);
  state_puts (buf,state_out);
  text_decode (ptr, state_in, state_out);

  dprint(12,(debugfile,
	     "text_unsupported_decode <- END; \n"));
  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "                        (file); ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }
}

void null_decode (ptr, state_in, state_out) 
     mime_t *ptr;
     in_state_t *state_in;
     out_state_t *state_out;
{
  char buf[STRING];

  dprint(12,(debugfile,
	     "null_decode <-> state: offset=%d, length=%d; \n",
	     ptr -> offset, ptr -> length));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"null_decode",
	       "Bad magic number");

  if (in_state_seekable(state_in)) {
    dprint(12,(debugfile,
	       "               (file): ftell=%ld\n",
	       in_state_ftell(state_in))); 
  }

  sprintf (buf, "[%.15s/%.30s is not supported, skipping...]\n",
	   TYPE(ptr->type),ptr->subtype);
  state_puts (buf,state_out);
  if (state_out->displaying)
    state_puts(   "[Use 'v' to view or save this part.]\n",state_out);
}

CT_decoder_t select_CT_decoder (ptr) 
     mime_t *ptr;
{
  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"select_CT_decoder",
	       "Bad magic number");

  if (ptr->type == MIME_TYPE_MULTIPART) {
    dprint(12,(debugfile,
	       "select_CT_decoder = multipart_0_decode\n"));
    return multipart_0_decode;
  }
  else if (ptr->flags & MIME_RFC822) {
    dprint(12,(debugfile,
	       "select_CT_decoder = rfc822_decode\n"));
    return rfc822_decode;
  }
  else if (ptr->type == MIME_TYPE_MESSAGE &&
	   (0 == istrcmp(ptr->subtype, "delivery-status") ||
	    0 == strincmp(ptr->subtype,"X-delivery-status-",18))) {
    dprint(12,(debugfile,
	       "select_CT_decoder = text_decode\n"));
    return text_decode;
  }
  else if (ptr->type == MIME_TYPE_TEXT) {
    if (0 == istrcmp(ptr->subtype, "rfc822-headers") ||
	0 == istrcmp(ptr->subtype, "plain")) {
      dprint(12,(debugfile,
		 "select_CT_decoder = text_decode\n"));
      return text_decode;
    }
#ifdef USE_PGP
    if (0 == istrcmp(ptr->subtype, "x-pgp")) {
      dprint(12,(debugfile,
		 "select_CT_decoder = pgp_decode\n"));
      return pgp_decode;
    }
#endif
    dprint(12,(debugfile,
	       "select_CT_decoder = text_unsupported_decode\n"));
    return text_unsupported_decode; /* For that type we try use
				       metamail if available! */
  }
#ifdef USE_PGP
  else if (ptr->type == MIME_TYPE_APPLICATION &&
	   (istrcmp(ptr->subtype, "pgp") == 0)) {
    dprint(12,(debugfile,
	       "select_CT_decoder = pgp_decode\n"));
    return pgp_decode;
  }
  else if (ptr->type == MIME_TYPE_APPLICATION &&
	   (strincmp(ptr->subtype,"x-pgp",5) == 0)) {
    dprint(12,(debugfile,
	       "select_CT_decoder = pgp_decode\n"));
    return pgp_decode;
  }
#endif
  else if (ptr->type == MIME_TYPE_APPLICATION &&
	   (istrcmp(ptr->subtype, "X-ELM-encode") == 0)) {
    dprint(12,(debugfile,
	       "select_CT_decoder = elm_decode\n"));
    return elm_decode;
  }
  
  dprint(12,(debugfile,
	     "select_CT_decoder = null_decode\n"));
  return null_decode;
}


void mime_decode (ptr, state_in, state_out)
     mime_t *ptr;
     in_state_t *state_in;
     out_state_t *state_out;
{
  /* This routine calls the appropriate routine to decode the data
   * described in "ptr".
   */

  dprint(12,(debugfile,"mime_decode -> state: offset=%d, length=%d\n",
	     ptr -> offset, ptr -> length));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_decode",
	       "Bad magic number");

  if (!in_state_seekable(state_in)) {
    error("mime_decode: unsupported state");
    state_puts("\n[mime_decode: unsupported state]\n", state_out);
    sleep_message();
    return;
  }

  if (ptr->disposition == DISP_INLINE) {
    CT_decoder_t decoder = select_CT_decoder(ptr);

    dprint(12,(debugfile,"mime_decode: decoder=%p\n",decoder));

    if (in_state_fseek(state_in,ptr->offset) != 0) {
      state_puts("[mime_decode: seek failed]\n",state_out);
      error("mime_decode: seek failed");
      sleep_message();
      return;
    }

    decoder (ptr, state_in, state_out);    
  }
  else { 
    if (state_out->displaying)      
      state_puts("[Attachment, skipping...  Use 'v' to view this part.]\n",
		 state_out);
    else
      state_puts("[Attachment, skipping...]\n",state_out);
  }

  dprint(12,(debugfile,"mime_decode <- END\n"));
}

static int rfc1522_decode_word (ptr, state)
     char *ptr;
     out_state_t *state;
{
  char *charset = NULL;
  char *encoded = NULL;
  char *buffer = NULL;
  char *c;
  int encoding = ENCODING_ILLEGAL;
  int count;
  int result = -1; /* Default to ERROR */
 
  count = 0;

  /* Make a copy so the original is not destroyed */
  c = buffer = strmcpy (buffer, ptr+2);

  while ((c = strtok (c, "?")) != NULL) {
    if (count == 0)
      /* get the charset */
      charset = c;
    else if (count == 1) {
      /* Check the encoding */
      if (toupper (*c) == 'Q')
	encoding = ENCODING_QUOTED;
      else if (toupper (*c) == 'B')
	encoding = ENCODING_BASE64;
    }
    else if (count == 2) 
      encoded = c;
    count++;
    c = NULL;
  }

  if (charset && encoding != ENCODING_ILLEGAL && encoded) {
    int code = class_charset(charset);
    result = 0; /* OK */

    dprint (14, (debugfile, 
		"rfc1522_decode_word: OK: ptr=%s, code=%d\n",ptr,code));

    if (code != 0) {
      in_state_t state_in;

      in_state_clear (&state_in, STATE_in_string);

      state->filter = NULL_filter;

      if (code < 0) {
	dprint (14,(debugfile, 
		   "rfc1522_decode_word: unsupported charset: %s -- filtering\n",
		   charset));
	state->filter = ASCII_filter;
	state_puts("[",state);
      }


      if (encoding == ENCODING_QUOTED) { /* Hack */
	char * ptr;
	for (ptr = encoded; *ptr != '\0'; ptr++)
	  if (*ptr == '_')
	    *ptr = ' ';
      }

      set_in_state_buffer (encoded, &state_in);
      
      if (encoding == ENCODING_QUOTED)
	quoted_printable_decode (&state_in, state, strlen (encoded), TRUE);
      else if (encoding == ENCODING_BASE64)
	base64_decode (&state_in,state, strlen (encoded), TRUE);
      if (code < 0) 
	state_puts("]",state);

      state->filter = NULL_filter;

      in_state_destroy(&state_in);
    } else {
      dprint (14, (debugfile, 
		  "rfc1522_decode_word: unsupported charset: %s -- skipping\n",
		  charset));
      state_puts("[?",state);
      state_puts(charset,state);
      state_puts("?]",state); 
    }
  }

  if (result == -1) {
    
    dprint (14, (debugfile, 
		"rfc1522_decode_word: FAILED: ptr=%s\n",ptr));
    dprint (14, (debugfile, 
		"rfc1522_decode_word: -> charset=%s, encoding=%d, encoded=%s\n",
		NONULL(charset),encoding,NONULL(encoded)));
    
  }
  
  free (buffer);
  buffer = NULL;

  return result;
}

static int rfc1522_decode_real (p, state)
     char *p;
     out_state_t *state;
{
  /* This routine decodes RFC1522 compliant headers */

  char *buffer = strmcpy(NULL,p);
  char *ptr = buffer;
  unsigned char last_char = 0;
  int result = 0;

  while (ptr && *ptr) {
    char *ptr2 = strpbrk(ptr," \t\r\n");
    unsigned char last_char2 = 0;
    int len;
    int is_encoded = 0;

    if (ptr2) {
      last_char2 = *ptr2;
      len = ptr2 - ptr;
      *ptr2 = '\0';
    } else {
      len = strlen(ptr);
    }

    /* Check if current word is valid encoded word */
    if (len > 9 && ptr[0] == '=' && ptr[1] == '?' &&
	ptr[len-1] == '=' && ptr[len-2] == '?') {
      char *ptr4;
      int count = 0;
      for (ptr4 = ptr; ptr4 - ptr < len; ptr4++)
	if (*ptr4 == '?')
	  count++;
      if (4 == count)
	is_encoded = 1;
    }

    dprint (14, (debugfile, 
		"rfc1522_decode_real: word = %s, len=%d, is_encoded=%d\n",
		ptr,len,is_encoded));

    /* Space between encoded chars must delete! --
     * This allows splitting more than 75 character long
     * encoded words to two encoded word and using of
     * continuation lines!
     */
    if (!is_encoded && last_char) {
      state_putc (last_char, state);
    }

    if (is_encoded) {
      if (-1 == rfc1522_decode_word(ptr,state)) {
	dprint (14, (debugfile, 
		    "rfc1522_decode_real: Error in rfc1522_decode_word ...\n"));
	result = -1;
	break;
      }
	
    } else {
      state_put(ptr,len,state);
    }

    if (!is_encoded && last_char2) {
      state_putc (last_char2, state);
      last_char2 = 0;
    }

    ptr = ptr2; /* Next word if any */
    if (ptr)
      ptr++;
    last_char = last_char2;
  }

  free(buffer);
  return 0;
}

int is_rfc1522 (s)
     char *s;
{
  /* Returns TRUE if the string contains RFC 1522 format encoded data */

  while ((s = strchr (s, '=')) != NULL) {
    s++;
    if (*s == '?')
      return TRUE;
  }
  return FALSE;
}

void rfc1522_decode (ptr, len)
     char *ptr;
     int len; /* size of "ptr" */
{
  /* Support for RFC 1522, which allows for encoding of non-ASCII characters
   * in RFC 822 headers.
   */
  char *tmp = safe_malloc (len);
  out_state_t state;

  /* Make a working copy of the header */
  strfcpy (tmp, ptr, len);

  /* The output from the decoding should be put back into the memory area
   * which was passed into this routine.
   */
  out_state_clear (&state, STATE_out_string);
  set_out_state_buffer(ptr,len,&state);
  
  if (rfc1522_decode_real (tmp, &state) == -1) {
    /* An error occured.  Restore the original value. */    
    
    strfcpy (ptr, tmp, len);
  }

  out_state_destroy (&state);  

  free (tmp);
}

void rfc1522_decode_structured (class, ptr, size)
     int class;
     char *ptr;
     int size; /* size of "ptr" */
{
  /* Decodes comments from structured fields 
   * if class & HDR_PHRASE decodes also prases (or all words outside of < >)
   *   -- not exactly correct for address lines, but otherwise we need full
   *      STD 11 (RFC 822) parser ....
   */
  char *walk, *walk2;
  char *result = safe_malloc(size);
  int tlen;
  int space = 0; /* Seen space or character after what there can begin
		    phrase (in address line) -- this alcorithm can result
		    incorrect output if addresses are not canonical for
		    -- for example 'some @ some . domain' */

  dprint (14, (debugfile, 
	      "rfc1522_decode_structured -> class=%d,size=%d,ptr=%s\n",
	      class,size,ptr));

  for (walk = ptr, walk2 = result; walk - ptr < size && *walk; walk += tlen) {
    int decoded = 0;
    int rlen;
    int c = *walk;

    tlen = rfc822_toklen(walk);
    strfcpy(walk2,walk,tlen+1);  /* Make initial working copy */

    /* Decode possible phrases */
    if ((class & HDR_PHRASE) && space && 
	NULL == strchr("()[]<>.:@,;\\\" \t\r\n",c) &&
	NULL == strchr("[].@,;",walk[tlen])) {
      char * walk3 = walk + tlen;

       while (walk3 < ptr + size -1 && *walk3 && 
	     NULL != strchr(" \t\r\n",*walk3)) {
	char *tmp = walk3;
	int nlen;

	walk3 += rfc822_toklen(walk3);

	nlen = rfc822_toklen(walk3);

 	if (!*walk3 || NULL != strchr("()[]<>.:@,;\\\" \t\r\n",*walk3)
	    || NULL != strchr("[].@,;",walk3[nlen])) {
	  walk3 = tmp; /* back before space */
	  break;
	}

	walk3 += nlen;
      }

      tlen = walk3 - walk;
      strfcpy(walk2,walk,tlen+1);  /* Make initial working copy */

      dprint (14, (debugfile, 
		  "rfc1522_decode_structured: Possible phrase '%s'\n",walk2));

      if (is_rfc1522(walk2)) {	
	rfc1522_decode(walk2,tlen+1);
	decoded = 1;
      } else {
	dprint (14, (debugfile, "rfc1522_decode_structured: '%s' NOT RFC1522\n",
		    walk2));
      }
    }

    space = 0;
    /* Decode comments */
    if (c == '(' && tlen > 9) {
      char *tmp = walk2 + 1;

      if (walk2[tlen-1] == ')')
	walk2[tlen-1] = 0;

      if (is_rfc1522(tmp)) {
	rfc1522_decode(tmp,tlen-1);
	decoded = 1;
      } else {
	dprint (14, (debugfile, "rfc1522_decode_structured: '%s' NOT RFC1522\n",
		    tmp));
      }

      strfcat(tmp,")", tlen);

      space = 1;
    }

    /* Skip address */
    if (c == '<' && tlen == 1) {
      char *tmp = walk;
      int t1len = tlen;
      tlen = 0;

      while (tmp < ptr + size && *tmp) {
	t1len = rfc822_toklen(tmp);
	tlen += t1len;
	if (*tmp == '>' && t1len == 1)
	  break;
	tmp += t1len;
      }
      dprint (14, (debugfile, "rfc1522_decode_structured: Address '%.*s'\n",
		  tlen,walk));
    }
    if (c == ',' && tlen == 1)
	 space = 1;
	 
    if (strchr(" \t\n\r",c) != NULL)
	 space = 1;

    if (decoded) {
      rlen = strlen(walk2);
      dprint (14, (debugfile, "rfc1522_decode_structured: '%.*s' -> '%s'\n",
		  tlen,walk,walk2));
      walk2 += rlen;
    } else {
      strfcpy(walk2,walk,tlen+1);  /* Make working copy */
      dprint (14, (debugfile, "rfc1522_decode_structured: Skip '%s'\n",walk2));
      walk2 += tlen;
    }
  }

  strfcpy(ptr,result, size);
  free(result);

  dprint (14, (debugfile, "rfc1522_decode_structured <- ptr=%s\n",ptr));
}

static int can_handle(att)     
     mime_t *att;
{
  if (att->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"can_handle",
	       "Bad magic number");

  while (att) {
    if (att->magic != MIME_magic)
      mime_panic(__FILE__,__LINE__,"can_handle",
		 "Bad magic number (next -chain)");

    if (att->disposition == DISP_INLINE) {      
      if (mime_notplain(att)) {
	dprint(9,(debugfile,"can_handle=NO -- Failed: %s/%s\n",
		  mime_types[att->type], att->subtype));
	return FALSE;
      }
      dprint(9,(debugfile,  "can_handle:     Succeed: %s/%s\n",
		mime_types[att->type], att->subtype));
    } else {
      dprint(9,(debugfile,  "can_handle:  Attachment: %s/%s\n",
		mime_types[att->type], att->subtype));
    }
    att = att->next;
  }
  dprint(9,(debugfile,"can_handle=TRUE\n"));
  return TRUE;
}

int mime_notplain (p)
     mime_t *p;
{
  CT_decoder_t decoder = select_CT_decoder(p);
  char buf[STRING];
 
  if (p->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_not_plain",
	       "Bad magic number");

  if (p->encoding < ENCODING_NONE ||
      p->encoding >= ENCODING_EXPERIMENTAL) {
    dprint(9,(debugfile,
	      "mime_notplain=TRUE -- type: %s/%s; encoding=%s (%d)\n",
	      mime_types[p->type], p->subtype,
	      ENCODING(p->encoding),p->encoding));
    return TRUE;      
  }

  if (decoder == rfc822_decode && p->parts && can_handle(p->parts)) {
    dprint(9,(debugfile,"mime_notplain=FALSE -- type: %s/%s\n",
	      mime_types[p->type], p->subtype));
    return FALSE;      
  }
  if (decoder == rfc822_decode) {
    dprint(9,(debugfile,"mime_notplain=TRUE -- type: %s/%s\n",
	      mime_types[p->type], p->subtype));
    return TRUE;
  }
 
  /* Notice: if (decoder == text_unsupported_decode)
   *         we return TRUE, because we want call metamail
   *         if it is available
   */
 
  if (decoder == text_decode || decoder == elm_decode) {
    /* If mime_get_charsets < 0, we want call metamail
     * (instead of filtering to US-ASCII)
     */
    if (mime_get_charset(buf,p->type_opts,sizeof(buf)) > 0) {
      dprint(9,(debugfile,"mime_notplain=FALSE -- type: %s/%s; charset=%s\n",
		mime_types[p->type], p->subtype,buf));
      return FALSE;
    }
  }
  else if (p->type == MIME_TYPE_MULTIPART && 
	   (0 == strincmp(p->subtype,"mixed",STRING) || 
	    0 == strincmp(p->subtype,"digest",STRING) || 
	    0 == strincmp(p->subtype,"report",STRING)) &&
	    p->parts && can_handle(p->parts)) {
    dprint(9,(debugfile,"mime_notplain=FALSE -- type: %s/%s\n",
	      mime_types[p->type], p->subtype));
    return FALSE;	   
  }
  else if (p->type == MIME_TYPE_MULTIPART && pagemultipart) {
    dprint(9,(debugfile,"mime_notplain=TRUE -- type: %s/%s\n",
	      mime_types[p->type], p->subtype));
    return FALSE;
  }
#ifdef USE_PGP
  else if (decoder == pgp_decode) {
    dprint(9,(debugfile,"mime_notplain=FALSE -- type: %s/%s\n",
	      mime_types[p->type], p->subtype));
    return FALSE;
  }
#endif
  dprint(9,(debugfile,"mime_notplain=TRUE -- type: %s/%s\n",
	    mime_types[p->type], p->subtype));
  return TRUE;
}

#endif /* MIME */
