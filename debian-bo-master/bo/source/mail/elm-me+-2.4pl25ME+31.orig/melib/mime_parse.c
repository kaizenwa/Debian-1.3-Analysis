#include "headers.h"
#include "melib.h"

#ifdef MIME
/* As ordered in mime.h */
char *mime_types[] = {
	"*unknown*",
	"application",
	"audio",
	"image",
	"message",
	"multipart",
	"text",
	"video",
	/* Non standard main types */
	"x-world",        /* x-word/x-vmrl */
	"model",          /* draft-nelson-model-mail-ext-02.txt */
        NULL
};

int
mime_needs_processing (p)
     mime_t *p;
{
  /* A routing which metapager() calls to determine if the message specified
   * by "p" to determine whether or not any special MIME processing needs
   * to be performed.  This information is used to decide if a temp file
   * needs to be used in order to effect backwards paging, or if we can page
   * directly out of the mailbox file.
   */

  if (p->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_needs_processing",
	       "Bad magic number");

  if (p->disposition != DISP_INLINE)
    return TRUE;

  if (p->encoding != ENCODING_7BIT && p->encoding != ENCODING_8BIT)
    return TRUE;

  if (p->type != MIME_TYPE_TEXT)
    return TRUE;

  /* This is to support those poor unfortunate folk who don't have true
   * MIME support.  Often, you see "Content-Type: text".  We'll just
   * assume that it's normal text.
   */
  if (p->subtype[0] == '\0')
    return FALSE;

  if (istrcmp (p->subtype, "plain") != 0)
    return TRUE;

  return FALSE;
}

char *
mime_parse_content_opts (str)
     char *str;
{
  /* A routine for parsing the options in a Content-Type: field.  The
   * important point here is to skip the semi-colon if it appears
   * inside of quotes.  This works sort of like strtok, except that
   * the token is already known.
   */
  static char *ptr;
  char *ret;
  int in_quote = 0;

  /* This is the initialization call */
  if (str)
    ptr = str;
  
  if (*ptr == '\0')
    return NULL;

  ret = ptr;
  while (*ptr) {
    if (*ptr == '\\' && in_quote) {		  
      /* \ escapes next character  
       * (not allowed outside of quotes) */
      ptr++;
      if (*ptr == '\0')
	break;
    } else if (*ptr == '\"') {
      if (in_quote)
	in_quote = 0;
      else
	in_quote = 1;
    }
    else if (! in_quote) {
      if (*ptr == ';') {
	*ptr++ = '\0';
	/* skip leading spaces */
	while (*ptr && isspace ((unsigned char)*ptr))
	  ptr++;
	return (ret);
      }
    } 
    ptr++;
  }
  return (ret);
}

void
mime_destroy (ptr)
     mime_t *ptr;
{
  mime_t *tmp;

  dprint(20,(debugfile,"mime_destroy(%p) --> BEGIN\n",ptr));

  if (ptr && ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_destroy",
	       "Bad magic number");

  while (ptr) {
    tmp = ptr;
    ptr = ptr->next;

    if (tmp->magic != MIME_magic)
      mime_panic(__FILE__,__LINE__,"mime_destroy",
		 "Bad magic number (next -chain)");

    if (tmp->description)
      free (tmp->description);
    if (tmp->type_opts)
      free (tmp->type_opts);
    if (tmp->disposition_opts)
      free (tmp->disposition_opts);

    if (tmp->parts)
      mime_destroy (tmp->parts);

    if (tmp->unlink)
      unlink (tmp->pathname);

    tmp -> magic = 0;

    free (tmp);
  }
  dprint(20,(debugfile,"mime_destroy(...) <-- END\n"));
  return;
}

void
mime_t_clear (mt)
     mime_t *mt;
{
  dprint(20,(debugfile,"mime_t_clear(%p) --> BEGIN\n",mt));

  if (mt->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_t_clear",
	       "Bad magic number");

  mt->flags = mt->offset = mt->begin_offset = 0;
  mt->length = -1;
  mt->encoding = ENCODING_7BIT;
  mt->unlink = 0;
  mt->type = MIME_TYPE_TEXT;
  mt->disposition = DISP_INLINE;
  mt->notplain = 0;

  if (mt->parts)
    mime_destroy (mt->parts);
  if (mt->next)
    mime_destroy (mt->next);

  mt->next = mt->prev = mt->parts = NULL;

  if (mt->description)
    free (mt->description);
  mt->description = NULL;
  if (mt->type_opts)
    free (mt->type_opts);
  mt->type_opts = NULL;
  if (mt->disposition_opts)
    free (mt->disposition_opts);
  mt->disposition_opts = NULL;
  if (mt->pathname)
    free (mt->pathname);
  mt->pathname = NULL;

  strfcpy (mt->subtype, "plain", sizeof mt->subtype);

  dprint(20,(debugfile,"mime_t_clear(%p) <-- END\n",mt));
  return;
}

void
mime_get_disposition (str, mt)
     char *str;
     mime_t *mt;
{
  char *c, tmp[VERY_LONG_STRING];

  dprint (9, (debugfile, "mime_get_disposition(): str=\"%s\"\n", str));
  
  if (mt->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_get_disposition",
	       "Bad magic number");

  /* Don't harm "str" */
  strfcpy (tmp, str, sizeof(tmp));
  
  rfc822_reap_comments (tmp, NULL, 0);
  
  /* Look for the options field */
  if ((c = strchr (tmp, ';')) != NULL) {
    char *d = c;
    while (d > tmp && whitespace(*(d-1)))
      d--;
    *d = '\0';
    c++;
    while (*c && whitespace(*c))
      c++;
    mt->disposition_opts = strmcpy (mt->disposition_opts, c);
  }
  else {
    char *d = tmp + strlen(tmp);
    while (d > tmp && whitespace(*(d-1)))
      d--;
    *d = '\0';
    if (mt->disposition_opts) {
      free (mt->disposition_opts);
      mt->disposition_opts = NULL;
    }
  }
  
  /* All that's left now is the main disposition */
  c = tmp;
  while (*c && whitespace(*c))
    c++;
  /* No Content-Disposition -header     -> DISP_INLINE
   *    Content-Disposition: inline     -> DISP_INLINE
   *    Content-Disposition: attachment -> DISP_ATTACH
   *    Content-Disposition: {unknown}  -> DISP_ATTACH
   * See RFC 1806 (Experimental protocol) for details.
   */   
  if (istrcmp (c, "inline") != 0)
    mt->disposition = DISP_ATTACH;
  else
    mt->disposition = DISP_INLINE;
     
  dprint (9, (debugfile,
	     "mime_get_disposition(): disposition=\"%s\", disposition_opts=\"%s\"\n",
	     DISPOSITION(mt->disposition), NONULL(mt->disposition_opts)));
}

int
mime_check_type (str)
	char *str;
{
  int i;
  for (i = 0; mime_types[i] != NULL; i++) {
    if (istrcmp(str,mime_types[i]) == 0)
      return i;
  }
  return MIME_TYPE_UNKNOWN;
}

void
mime_get_content (str, mt)
     char *str;
     mime_t *mt;
{
  char *c, tmp[VERY_LONG_STRING];

  dprint (9, (debugfile, "mime_get_content(): str=\"%s\"\n", str));

  if (mt->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_get_content",
	       "Bad magic number");

  /* Don't harm "str" */
  strfcpy (tmp, str, sizeof(tmp));

  rfc822_reap_comments (tmp, NULL, 0);

  /* Look for the options field */
  if ((c = strchr (tmp, ';')) != NULL) {
    char *d = c;
    while (d > tmp && whitespace(*(d-1)))
      d--;
    *d = '\0';
    c++;
    while (*c && whitespace(*c))
      c++;
    mt->type_opts = strmcpy (mt->type_opts, c);
  }
  else {
    char *d = tmp + strlen(tmp);
    while (d > tmp && whitespace(*(d-1)))
      d--;
    *d = '\0';
    if (mt->type_opts) {
      free (mt->type_opts);
      mt->type_opts = NULL;
    }
  }
  
  mt->subtype[0] = '\0';
  /* Get the subtype */
  if ((c = strchr (tmp, '/')) != NULL) {
    char *d = c;
    while (d > tmp && whitespace(*(d-1)))
      d--;
    *d = '\0';

    c++;
    while (*c && whitespace(*c))
      c++;
    strfcpy (mt->subtype, c, sizeof(mt->subtype));
  }
  
  /* All that's left now is the main type */
  c = tmp;
  while (*c && whitespace(*c))
    c++;
  mt->type = mime_check_type (c);

  /* Mark MESSAGE/RFC822 so we can do special handling later */
  if (mt->type == MIME_TYPE_MESSAGE && istrcmp (mt->subtype, "rfc822") == 0)
    mt->flags |= MIME_RFC822;
  else if (mt->type == MIME_TYPE_MULTIPART) {
    if (istrcmp (mt->subtype, "mixed") == 0)
      mt->flags |= MIME_MIXED;
    else if (istrcmp (mt->subtype, "digest") == 0)
      mt->flags |= MIME_DIGEST;
  }

  dprint (9,(debugfile,
	     "mime_get_content(): type=\"%s\", subtype=\"%s\", opts=\"%s\"\n",
	     mime_types[mt->type], mt->subtype, NONULL(mt->type_opts)));

  return;
}

int
mime_get_param (name, value, opts, size)
     char *name, *opts, *value;
     int size;
{
  char *c, tmp[VERY_LONG_STRING];
  int i = 0, quoted = FALSE, found = FALSE;

  value[0] = '\0';

  if (!opts) {
    dprint(11, (debugfile,"mime_get_param: name=\"%s\", opts=NULL\n",name));
    return 0;
  }

  dprint(11, (debugfile,"mime_get_param: name=\"%s\", opts=\"%s\"\n",
	      name,opts));

  /* Make sure not to harm opts */
  strfcpy (tmp, opts, sizeof (tmp));
  rfc822_reap_comments (tmp, NULL, 0);

  c = tmp;
  while ((c = mime_parse_content_opts (c)) != NULL && !found) {
    char * d = strchr(c,'=');
    if (!d) {
      c = NULL;
      continue;    /* bad paramater */
    }
    while (d > c && (whitespace (*(d-1))))
      d--;
    *d = '\0';
    
    while (*c && whitespace(*c))
      c++;
    
    if (istrcmp (c, name) == 0) {
      found = TRUE;
      
      c = d+1;
      while (*c && whitespace(*c))
	c++;
      if (*c == '"') {
	c++;
        quoted = TRUE;
      }
      /* Either look for a trailing quoted (if quoted==TRUE) or a SPACE */
      while (*c && ((quoted && *c != '"') || (!quoted && *c != ' '))) {
	if (*c == '\\' && quoted) {
	  /* \ escapes next character */
	  c++;
	  if (!*c)
	    break;
	}
	if (i >= size-1)
	  break;    /* Avoid buffer overflow */
	value[i++] = *c++;
      }
      value[i] = '\0';
      break;
    }
    c = NULL;
  }

  dprint(11,(debugfile,"mime_get_param: found=%d, value=%s\n",found,value));

  return found;
}

void mime_get_boundary (boundary, opts, size)
     char *opts, *boundary;
     int size;
{
  if (!mime_get_param ("boundary", boundary, opts, size)) {
    error ("'boundary' paramater is missing from Multipart -type!");
    sleep_message();
  }

  return;
}

int class_charset(charset_value) 
  char * charset_value;
{
 /* Returns 1 if charset is displayable with charset of text
  *     (suitable for replying)
  * Return 2 if charset is displayable with display_charset
  *           (when replying charset should be changed to display_charset)
  * Return -1 if need to filter to US-ASCII
  */
  int ret = 0;

  /* Check agaist charset of text */
  if (0 == istrcmp(charset_value,"US-ASCII") && charset_ok(charset))
    ret = 1; /* If charset of text can show us-ascii? */
  else if (0 == istrcmp(charset_value,charset))
    ret = 1;
  /* Check agaist display charset */
  else if (0 == istrcmp(charset_value,"US-ASCII") && 
	   charset_ok(display_charset))
    ret = 2; /* If display_charset can show us-ascii? */
  else if (0 == istrcmp(charset_value,display_charset))
    ret = 2;
  else if (0 == istrcmp(display_charset,"US-ASCII") && 
	   charset_ok(charset_value))
    ret = -1;
  else if (charset_ok(display_charset) && 
	   charset_ok(charset_value))
    ret = -1;

  dprint(9,(debugfile,"class_charset: charset_value=\"%s\"\n",
	    charset_value));
  dprint(9,(debugfile,
	    "             : (text)charset=\"%s\",display_charset=\"%s\"\n",
	    charset,display_charset));
  dprint(9,(debugfile,"class_charset=%d\n",ret));

  return ret;
}

/* There should better handling in metapager (I have one idea)
 * Perhaps in next version ...    -K E H    <hurtta@dionysos.fmi.fi>
 */
int 
mime_get_charset (charset_value, opts, size)
     char *opts, *charset_value;
     int size;
{  /* Returns 1 if charset is displayable with charset of text
    *     (suitable for replying)
    * Return 2 if cahrset is displayable with display_charset
    *           (when replying charset should be changed to display_charset)
    * Return -1 if need to filter to US-ASCII
    */
  int ret = 0;

  if (!mime_get_param("charset",charset_value,opts,size)) 
    strfcpy(charset_value,"US-ASCII", size); 
    /* Default charset if nothing specified */

  if (!opts) {
    dprint(9,(debugfile,"mime_get_charset: opts=NULL\n",opts));
  } else {
    dprint(9,(debugfile,"mime_get_charset: opts=\"%s\"\n",opts));
  }
  dprint(9,(debugfile,"                : charset_value=\"%s\"\n",
	    charset_value));

  ret = class_charset(charset_value);

  dprint(9,(debugfile,"mime_get_charset=%d\n",ret));

  return ret;
}

void
mime_t_zero (ptr)
     mime_t *ptr;
{
  /* This routine should be called whenever a new "mime_t" is created.  It
   * makes sure that the pointers inside are correctly initialized to NULL
   * so that we don't end up calling free() on an uninitialized pointer.
   */
  dprint(20,(debugfile,"mime_t_zero(%p)\n",ptr));

  ptr->next = ptr->parts = NULL;
  ptr->description = ptr->type_opts = ptr->disposition_opts = 
    ptr->pathname = NULL;
  ptr->magic = MIME_magic;
}

void mime_t_copy(trg, src)
     mime_t *trg, *src;
{
  /* This routines make copy of mime_t structure ... */

  dprint(20,(debugfile,"mime_t_copy(%p,%p) --> BEGIN\n",trg,src));
  
  if (trg->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_t_copy",
	       "Bad magic number (trg)");

  if (src->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_t_copy",
	       "Bad magic number (src)");

  mime_t_clear(trg);

  trg->flags  =            src->flags;
  trg->offset =            src->offset; 
  trg->begin_offset  =     src->begin_offset; 
  trg->length =            src->length;
  trg->encoding =          src->encoding;
  trg->unlink = 0; /* Don't unlink ! */
  trg->type =              src->type;
  trg->disposition =       src->disposition;
  trg->notplain =          src->notplain;

  if (src->parts) {
    trg->parts = mime_t_alloc();
    mime_t_copy(trg->parts,src->parts);
  }

  if (src->next) {
    trg->next = mime_t_alloc();
    mime_t_copy(trg->next,src->next);
  }

  if (src->description) {
    trg->description = strmcpy(trg->description, src->description);
  }

  if (src->type_opts) {
    trg->type_opts = strmcpy(trg->type_opts, src->type_opts);
  }

  if (src->disposition_opts) {
    trg->disposition_opts = strmcpy(trg->disposition_opts, 
				    src->disposition_opts);
  }

  strfcpy(trg->subtype,src->subtype, sizeof trg->subtype);

  dprint(20,(debugfile,"mime_t_copy(%p,%p) <-- END\n",trg,src));
}

mime_t *
mime_t_alloc ()
{
  mime_t *ptr;

  dprint(20,(debugfile,"mime_t_alloc()     --> BEGIN\n"));

  ptr = (mime_t *) safe_malloc (sizeof (mime_t));
  /* Make sure to clear the pointers initially so that later we know when
   * to reclaim memory in mime_t_clear().
   */
  mime_t_zero (ptr);
  mime_t_clear (ptr);

  dprint(20,(debugfile,"mime_t_alloc() = %p <-- END\n",ptr));
  return ptr;
}

mime_t *
parse_mime_headers(header_list_ptr headers,
		   long part_offset,
		   long body_offset,
		   int opts)
{
  mime_t *ptr;
  header_list_ptr this_header;

  dprint(9,(debugfile, 
	    "parse_mime_headers(): part_offset=%ld, body_offset=%ld, opts=%d\n",
	    part_offset,body_offset,opts));

  ptr = mime_t_alloc ();

  /* set some defaults */
  ptr->encoding = ENCODING_7BIT;
  if (opts & MIME_DIGEST) {
    ptr->type = MIME_TYPE_MESSAGE;
    strfcpy (ptr->subtype, "rfc822", sizeof ptr->subtype);
  }
  else {
    ptr->type = MIME_TYPE_TEXT;
    strfcpy (ptr->subtype, "plain", sizeof ptr->subtype);
  }
  ptr->disposition = DISP_INLINE;
  ptr->description = NULL;

  ptr->begin_offset = part_offset;
  ptr->offset       = body_offset;
  ptr->length = -1;

  if (NULL != (this_header = 
	       locate_header_by_name(headers,"Content-Type")) &&
      NULL != this_header->body) {
    mime_get_content (this_header->body, ptr);
    if (this_header->next_this_header) {
      error("PARSE ERROR: Several Content-Type headers!");
      sleep_message();
    }
  } else {
    dprint (9,(debugfile,"parse_mime_headers: No Content-Type -header\n"));
  }

  if (NULL != (this_header = 
	       locate_header_by_name(headers,"Content-Disposition")) &&
      NULL != this_header->body) {
    mime_get_disposition (this_header->body, ptr);
    if (this_header->next_this_header) {
      error("PARSE ERROR: Several Content-Disposition headers!");
      sleep_message();
    }
  } else {
    dprint (9,(debugfile,
	       "parse_mime_headers: No Content-Disposition -header\n"));
  }

  if (NULL != (this_header = 
	       locate_header_by_name(headers,"Content-Transfer-Encoding")) &&
      NULL != this_header->body) {
    char  * value = this_header->body, *c;

    /* This removes comments from buffer this_header->body */

    rfc822_reap_comments (value, NULL, 0);
    c = value;
    while (*c && isspace((unsigned char) *c))
	c++;
    ptr->encoding = check_encoding (c);

    if (this_header->next_this_header) {
      error("PARSE ERROR: Several Content-Transfer-Encoding headers!");
      sleep_message();
    }
  } else {
    dprint (9,(debugfile,
	       "parse_mime_headers: No Content-Transfer-Encoding -header\n"));
  }

  if (NULL != (this_header = 
	       locate_header_by_name(headers,"Content-Description"))  &&
      NULL != this_header->body) {
    char value [32 * 1024 +1], *c;
    strfcpy(value,this_header->body, sizeof value);
      
    /* Content-description is unstructured text header */
    if (is_rfc1522(value))
      rfc1522_decode(value,sizeof(value));

    c = value;
    while (*c && whitespace(*c))
      c++;
    ptr->description = NULL;
    ptr->description = strmcpy (ptr->description, c);

    if (this_header->next_this_header) {
      error("PARSE ERROR: Several Content-Description headers!");
      sleep_message();
    }
  } else {
    dprint (9,(debugfile,
	       "parse_mime_headers: No Content-Description -header\n"));
  }

  /* mime_get_content don't set this if this is set as defualt value
   * because of MIME_DIGEST -flag (in opt)
   */
  if (ptr->type == MIME_TYPE_MESSAGE && 
      istrcmp (ptr->subtype, "rfc822") == 0)
    ptr->flags |= MIME_RFC822;

  dprint(12,(debugfile, 
	     "parse_mime_headers- type=%s/%s; flags=%d;\n",
	     mime_types[ptr->type], ptr->subtype, ptr->flags));
  dprint(12,(debugfile, 
	     "parse_mime_headers- begin=%ld, offset=%ld, length=%ld\n",
	     ptr->begin_offset,ptr->offset,ptr->length));
  dprint(9,(debugfile, "parse_mime_headers=%p <-- END\n",(void *)ptr));

  return ptr;
}

mime_t *
mime_read_header (fp, opts)
     FILE *fp;
     int opts;
{
  mime_t *ptr;
  header_list_ptr headers = NULL;
  long part_offset;
  long body_offset;


  dprint(9,(debugfile, "mime_read_header: opts=%d --> START\n",opts));

  part_offset = ftell (fp);
  headers = file_read_headers(fp,0);
  body_offset = ftell(fp);

  ptr = parse_mime_headers(headers,part_offset,body_offset,opts);

  delete_headers(headers);

  return ptr;
}

mime_t * multipart_parse (fp, length, boundary, opts)
     FILE *fp;
     int length, opts;
     char *boundary;
{
  int blen, len,last_pos;
  long end_offset;
  char buf[VERY_LONG_STRING], subbound[STRING];
  mime_t *ptr = NULL, *tmp, *ret = NULL;

  dprint(9,(debugfile,"multipart_parse --> length=%d, boundary=%s\n",
	    length, boundary));

  blen = strlen (boundary);
  end_offset = ftell (fp) + length;
  last_pos = ftell(fp);

  while (ftell (fp) < end_offset) {
    if ((len = mail_gets (buf, VERY_LONG_STRING, fp)) == 0)
      break;

    if (buf[0] == '-' && buf[1] == '-' &&
        strncmp (buf + 2, boundary, blen) == 0) {

      /* Save the length of the previous part */
      if (ptr) {
        ptr->length = last_pos - ptr->offset;
	dprint(9,(debugfile,"multipart_parse: fixing length=%d\n",
		  ptr->length));
      }
      /* Check for the end boundary. */
      if (buf[blen+2] == '-' && buf[blen+3] == '-')
        break;
      
      tmp = mime_read_header (fp, opts);

      dprint(9,(debugfile,"multipart_parse: (reading) content-type=%s/%s; flags=%d\n",
		mime_types[tmp->type], tmp->subtype, tmp->flags));

      
      if (ret == NULL)
        ptr = ret = tmp;
      else {
        ptr->next = tmp;
        ptr = ptr->next;
      }
#if 0
      if (ptr->length >= 0) {
	/* If the length of this part is known, skip ahead to the next
	 * part.  If the length is not known, we don't have to worry
	 * about it because the algorithm will search for the next
	 * boundary...
	 */
        fseek (fp, (long) (ptr->length), SEEK_CUR);
	continue;
      }
#endif
    }
    if (ptr && ptr->length < 0) { /* mark position before CR LF */
      int pos = ftell(fp);
      if (len > 1 && buf[len-2] == '\r' && buf[len-1] == '\n')
	last_pos = pos -2;
      else if (len > 0 && buf[len-1] == '\n')
	last_pos = pos -1;
    }
  }

  if (ptr && ptr->length != last_pos - ptr->offset) {
    ptr->length = last_pos - ptr->offset;
    dprint(9,(debugfile,"multipart_parse: fixing length=%d (corrupted?)\n",
	      ptr->length));

    error ("Seems that multipart structure was corrupted.");
    sleep_message();
  }

  /* Now that we know what this message consists of, see if any of the
   * parts contain data that needs to be parsed. */

  for (tmp = ret; tmp != NULL; tmp = tmp->next) {

    if (tmp->magic != MIME_magic)
      mime_panic(__FILE__,__LINE__,"multipart_parse",
		 "Bad magic number (next -chain)");

    dprint(9,(debugfile,"multipart_parse: (parsing) content-type=%s/%s; flags=%d\n",
	      mime_types[tmp->type],tmp->subtype,tmp->flags));

    if (tmp->flags & MIME_RFC822) {
      dprint(9,(debugfile,"multipart_parse- (parsing) RFC822\n"));
      fseek (fp, tmp->offset, SEEK_SET);
      tmp->parts = rfc822_parse (fp, tmp->length);
    }
    else if (tmp->type == MIME_TYPE_MULTIPART) {
      fseek (fp, tmp->offset, SEEK_SET);
      mime_get_boundary (subbound, tmp->type_opts, sizeof (subbound));
      dprint(9,(debugfile,
		"multipart_parse- (parsing) MULTIPART; boundary=%s\n",
		subbound));
      tmp->parts = multipart_parse (fp, tmp->length, subbound, tmp->flags);
    }
  }

  /* Make sure to leave the stream at the end of the data since the
   * calling function might be assuming this.  */
  fseek (fp, end_offset, SEEK_SET);

  dprint(9,(debugfile,"multipart_parse <-- DONE\n"));

  return ret;
}

mime_t *
rfc822_parse (fp, len)
     FILE *fp;
     int len;
{
  /* Called to read MESSAGE/RFC822 data.  First reads the header of the
   * message for MIME information, then (when necessary) calls other
   * functions to determine the content of MULTIPART or MESSAGE/RFC822
   * data contained.
   */
  mime_t *ret = NULL;
  long part_offset = ftell (fp);
  long body_offset;
  long end_offset  =  part_offset + len;
  header_list_ptr headers = NULL, mime_version, content_type;
  int pre_mime_content_type = 0;

  dprint(9,(debugfile,"rfc822_parse --> len=%d\n",len));

  headers     = file_read_headers(fp,0);
  body_offset = ftell(fp);

  dprint(9,(debugfile,
	    "rfc822_parse- part_offset=%ld, body_offset=%ld, end_offset=%ld\n",
	    part_offset, body_offset, end_offset));

  if (!locate_header_by_name(headers,"From") &&
      !locate_header_by_name(headers,"Subject") &&
      !locate_header_by_name(headers,"To") &&
      !locate_header_by_name(headers,"CC")) {
    error ("Seems that message/rfc822 data was corrupted.");
    sleep_message();
  }

  mime_version = locate_header_by_name(headers,"MIME-Version");
  content_type = locate_header_by_name(headers,"Content-Type");

  if (content_type && content_type ->body) {
    ret = mime_t_alloc();
    ret->begin_offset = part_offset;
    ret->offset       = body_offset;
    ret->length = -1;

    pre_mime_content_type = is_pre_mime_content_type(ret,content_type->body);
  }

  if (mime_version && pre_mime_content_type) {
    error ("Warning: message/rfc822 data with MIME-Version and pre-mime Content-type");
    sleep_message();
  } 

  if (mime_version || !pre_mime_content_type && req_mime_bodyencoding) {
    if (ret) mime_destroy(ret);
    ret = parse_mime_headers(headers,part_offset,body_offset,MIME_MIXED);  
  } else if (!ret) {
    ret = mime_t_alloc();
    ret->begin_offset = part_offset;
    ret->offset       = body_offset;
    ret->length = -1;

  }

  dprint(9,(debugfile,"rfc822_parse: content-type=%s/%s; flags=%d\n",
	    mime_types[ret->type], ret->subtype,ret->flags));

  if (ret->length < 0) { 
    ret->length = end_offset - body_offset;
    dprint(9,(debugfile,"rfc822_parse: fixing length=%d\n",ret->length));
  }

  if (ret->type == MIME_TYPE_MULTIPART) {
    char boundary[STRING];

    mime_get_boundary (boundary, ret->type_opts, STRING);
    dprint(9,(debugfile,"rfc822_parse- (parsing) MULTIPART; boundary=%s\n",
	      boundary));

    ret->parts = multipart_parse (fp, ret->length, boundary, ret->flags);
  }
  else if (ret->flags & MIME_RFC822) {
    dprint(9,(debugfile,"rfc822_parse- (parsing) RFC822\n"));
    ret->parts = rfc822_parse (fp, ret->length);
  }
  
  delete_headers(headers);

  /* Make sure the leave the stream at the end of the data! */
  fseek (fp, end_offset, SEEK_SET);

  dprint(9,(debugfile,"rfc822_parse <-- DONE\n"));

  return ret;
}

void
mime_warnings(hdr) 
     struct header_rec *hdr;
{

  if (hdr->status & PRE_MIME_CONTENT) {
    error("Error: MIME-message has pre-MIME content-type!");
    sleep_message();
  }
  if (hdr->status & MIME_UNSUPPORTED) {
    error("Warning: Unsupported MIME-Version!");
    sleep_message();
  }
}

void
attach_parse (hdr, fp)
     struct header_rec *hdr;
     FILE *fp;
{
  int parsing = 0;
  /* This routine checks to see if the multipart messages specified by
   * "hdr" has been parsed for its subparts, and if not, calls the routine
   * to do so.
   */

  char boundary[STRING];

  if (hdr->mime_rec.magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_parse",
	       "Bad magic number (mime_rec)");

  /* Copy value */
  if (hdr -> content_length >= 0)
    hdr->mime_rec.length = hdr -> content_length;

  if (hdr->mime_rec.begin_offset <= 0) {

    fseek(fp,hdr->offset,SEEK_SET);
    hdr->mime_rec.begin_offset = hdr->offset;
    /* Skip mailbox's separator lines ... */

    dprint(9,(debugfile,"attach_parse: scanning begin_offset: %d\n",
	      hdr->mime_rec.begin_offset));

    hdr->mime_rec.begin_offset = skip_envelope(hdr,fp);

    dprint(9,(debugfile,"attach_parse: begin_offset=%d\n",
	      hdr->mime_rec.begin_offset));

    if (hdr->mime_rec.begin_offset < 0) {
      error("Can't parse mail...");
      sleep_message();
      return;
    }
  }

  if (hdr->mime_rec.type == MIME_TYPE_MULTIPART) {
    if (hdr->mime_rec.parts == NULL) {
      mime_get_boundary (boundary, hdr->mime_rec.type_opts, STRING);
      if (0 != fseek (fp, hdr->mime_rec.offset, SEEK_SET)) {
	error("Failed to seek beginning body...");
	sleep_message();
      } else {
	int tmp;
	parsing = 1;
	lower_prompt("Parsing MIME structure...");
	hdr->mime_rec.parts = 
	  multipart_parse (fp, hdr->content_length, boundary, 
			 hdr->mime_rec.flags);
	/* Reconsider it */
	tmp = mime_notplain(&(hdr->mime_rec));
	if (tmp != hdr->mime_rec.notplain) {
	  hdr->mime_rec.notplain = tmp;
	  if (!tmp) {
	    lower_prompt("Parsing MIME structure... metamail not needed");
	    parsing = 2;
	  }
	}
      }
    }
  } else if (hdr->mime_rec.flags & MIME_RFC822) {
    if (hdr->mime_rec.parts == NULL) {
      if (0 != fseek (fp, hdr->mime_rec.offset, SEEK_SET)) {
	error("Failed to seek beginning body...");
	sleep_message();
      } else {
	int tmp;
	parsing = 1;
	lower_prompt("Parsing MIME structure...");
	hdr->mime_rec.parts = rfc822_parse (fp, hdr->content_length);
	/* Reconsider it */
	tmp = mime_notplain(&(hdr->mime_rec));
	if (tmp != hdr->mime_rec.notplain) {
	  hdr->mime_rec.notplain = tmp;
	  if (!tmp) {
	    lower_prompt("Parsing MIME structure... metamail not needed");
	    parsing = 2;
	  }
	}	
      }
    }
  }

  if (parsing) {
    if (2 == parsing) {
      if (sleepmsg > 0)
	sleep ((sleepmsg+2)/3);
    }
    lower_prompt("");
  }
  return;
}
#endif /* MIME */



