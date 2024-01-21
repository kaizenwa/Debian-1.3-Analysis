
#include "headers.h"
#include "me.h"

#ifdef MIME
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/wait.h>
/* unistd.h is included in headers.h */

extern int errno;
extern int elm_COLUMNS;

static int can_save(filename) 
     char *filename;
{
  if (access(filename,ACCESS_EXISTS) < 0) {
    if (errno == ENOENT)
      return 1;    /* File not exists -- can save ! */
    return 0;     /* Other error -- don't save with that name ! */
  } else
    return 0;    /* File exits -- don't save over ! */
}

static FILE * 
get_attachment(mail_fd,att)
     FILE * mail_fd;
     mime_t *att;
{
  FILE * result = NULL;

  if (att->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"get_attachment",
	       "Bad magic number");

  if (att->pathname) {
    if (can_open(att->pathname,"r") != 0) {
      error1 ("%.50s isn't readable by user!", att->pathname);
      sleep_message();
      return NULL;
    }

    result = fopen(att->pathname,"r");
    if (!result) {
      error1("Can't open attachment: %.50s",att->pathname);
      sleep_message();
    }
  } else if (!mail_fd) {
      error("Internal error: no parent fd!");
      sleep_message();
  } else {
    result = mail_fd;
    if (fseek(mail_fd,att->offset,SEEK_SET) != 0) {
      error("Failed to seek beginning of attachment!");
      sleep_message();
      result = NULL;
    }
  }
  return result;
}

void close_attachment(mail_fd, tmpfd)
     FILE * mail_fd, *tmpfd;
{
  if (!tmpfd)
    return;
  if (mail_fd != tmpfd)
    fclose(tmpfd);
}

void attachment_copy(att, tmpfd, outfd)
     FILE *tmpfd, *outfd;
     mime_t *att;
{
  if (att->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attachment_copy",
	       "Bad magic number");

  if (att->pathname) {  /* Already decoded */
    char buf[VERY_LONG_STRING];
    int len;
    while (0 < (len = fread(buf,1,sizeof(buf),tmpfd))) {
      if (fwrite(buf,1,len,outfd) != len) {
	error("Write error when copying attachment!");
	sleep_message();
	break;
      }
    }
    if (ferror(tmpfd)) {
      error("Error reading from %.50s",att->pathname);
      sleep_message();
    }
  } else { /* Needs decode */
    in_state_t state_in;
    out_state_t state_out;

    in_state_clear(&state_in, STATE_in_file);
    out_state_clear(&state_out, STATE_out_file);

    set_out_state_file(outfd,&state_out);
    state_out.displaying = FALSE;
    set_in_state_file(tmpfd,&state_in);
    mime_decode(att,&state_in, &state_out);

    in_state_destroy(&state_in);
    out_state_destroy(&state_out);
  }
}

static void
attach_print (att)
     mime_t *att;
{
  char tempfile[STRING];
  char buf[VERY_LONG_STRING];
  
  if (att->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_print",
	       "Bad magic number");

  sprintf(tempfile,"%selm.%d", temp_dir, getpid());

  if (att->type == MIME_TYPE_TEXT && istrcmp(att->subtype,"plain") == 0) {
    FILE *f_out, *f_in;
    int ret;

    dprint(3,(debugfile,"attach_print: printing directly\n"));

    
    if (!(f_out = safeopen(tempfile))) {
      error1("Error creating tempfile %s",tempfile);
      sleep_message();
      return;
    }

    if (!(f_in = get_attachment(mailfile,att))) {
      fclose(f_out);
      unlink(tempfile);
      return;
    }
    attachment_copy(att,f_in,f_out);

    (void) elm_chown (tempfile, userid, groupid);
      
    fclose(f_out);
    sprintf(buf, printout, tempfile);
    ret = system_call(buf,0);
    if (ret == 0)
      error("Print job spooled.");
    else
      error("Error while printing!");
    unlink(tempfile);
    close_attachment(mailfile,f_in);
  }
  else if (have_metamail()) {
    dprint(3,(debugfile,"attach_print: printing via metamail: %s\n",
	      metamail_path));
    if (att->pathname)
      sprintf(buf,"%s -m Elm -h -b -c %s/%s %s",
	      metamail_path,TYPE(att->type), att->subtype, att->pathname);
    else {
      FILE *fpout;

      fpout = safeopen(tempfile);

      if (!fpout) {
	error1("Error creating tempfile %s",tempfile);
	sleep_message();
	return;
      }

      (void) elm_chown (tempfile, userid, groupid);
      fseek(mailfile,att->begin_offset,0);
      while (ftell(mailfile) < att->offset + att->length) {
	int len = mail_gets(buf,VERY_LONG_STRING,mailfile);
	if (len <= 0) 
	  break; /* Error ? */
	fwrite(buf,1,len,fpout);
      }
      /* Option -z cuses that metamail unlinks tempfile */
      fclose(fpout);
      sprintf(buf,"%s -m Elm -h -z %s", metamail_path, tempfile);
    }
    Raw(OFF);
    system_call(buf,SY_ENV_METAMAIL);
    PressAnyKeyToContinue();
    Raw(ON);
  }
  else
    error("Don't know how to print this type of data!");

}

static void attach_save (a)
     mime_t *a;
{
  char savefile[STRING], buf[VERY_LONG_STRING], tmp[STRING];
  int bytes=0, err, is_text;
  in_state_t state_in;
  out_state_t state_out;
  FILE *f_in, *f_out;
  int code;

  in_state_clear(&state_in, STATE_in_file);
  out_state_clear(&state_out, STATE_out_file);

  if (a->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_save",
	       "Bad magic number");

  savefile[0] = '\0';
  if (a->type == MIME_TYPE_APPLICATION &&
      istrcmp (a->subtype, "octet-stream") == 0) {

    /* See if there is a name=<...> field for the default filename */
    tmp[0] = '\0';
    if (a->type_opts &&
	mime_get_param ("name", tmp, a->type_opts, sizeof(tmp))) {
      char *p = strrchr (tmp, '/');
      if (p)
	p++;
      else
	p = tmp;
      if (can_save(p))
	strfcpy (savefile, p, sizeof (savefile));
    }
  }

   /* See if there is a filename=<...> field for the default filename */
  tmp[0] = '\0';
  if (a->disposition_opts &&
      mime_get_param ("filename", tmp, a->disposition_opts, sizeof(tmp))) {
    char *p = strrchr (tmp, '/');
    if (p)
      p++;
    else
      p = tmp;
    if (can_save(p))
      strfcpy (savefile, p, sizeof (savefile));
  }

  ClearLine (elm_LINES-2);

redraw:
  PutLine0 (elm_LINES-2, 0, "To file: ");
  code = optionally_enter(savefile, elm_LINES-2, 9, OE_APPEND_CURRENT|
			  OE_REDRAW_MARK, sizeof savefile);
  if (REDRAW_MARK == code)
    goto redraw;

  if (code < 0 || 
      savefile[0]=='\0'){
    ClearLine(elm_LINES-2);
    error("Mail not saved.");
    
    in_state_destroy(&state_in);
    out_state_destroy(&state_out);
    sleep_message();
    return;
  }

  ClearLine(elm_LINES-2);

  
  if (!(f_in = get_attachment(mailfile,a)))  {

    in_state_destroy(&state_in);
    out_state_destroy(&state_out);
    return;
  }

  if (can_open (savefile,"w") != 0) {
    error ("You do not have permission to write that file!");
    sleep_message();

    in_state_destroy(&state_in);
    out_state_destroy(&state_out);

    close_attachment(mailfile,f_in);

    return;
  }

  
  if (! (f_out = fopen(savefile, "w"))) {
    error ("Error opening file!");
    sleep_message();

    in_state_destroy(&state_in);
    out_state_destroy(&state_out);

    close_attachment(mailfile,f_in);
    return;
  }

  is_text = is_text_type (TYPE(a->type), a->subtype, a->encoding);

  set_in_state_file(f_in,&state_in);
  set_out_state_file(f_out,&state_out);

  state_out.prefix = NULL;
  state_out.displaying = 0;

  if (a->encoding == ENCODING_BASE64 && !a->pathname)
     base64_decode (&state_in, &state_out, a->length, is_text);
  else if (a->encoding == ENCODING_QUOTED && !a->pathname)
     quoted_printable_decode (&state_in, &state_out, a->length, is_text);
  else {
    if (a->encoding != ENCODING_NONE && a->encoding != ENCODING_7BIT &&
	a->encoding != ENCODING_8BIT && a->encoding != ENCODING_BINARY
	&& !a->pathname) {
      error("Unsupported encoding! Decode manually!");
      sleep_message();

      /* save without decoding ! */
      is_text = 0;  /* We can't suppose it is text before decoding... */
    }
    while (bytes < a->length) {
      int chunk = VERY_LONG_STRING;
      int len;

      if (chunk > a->length - bytes)
	chunk = a->length - bytes;
      if ((len = state_getl (buf, chunk, &state_in)) <= 0)
	break;
      bytes += len;
      if (is_text > 0) { /* replace CRLF with LF */
	if (len >= 2 && buf[len-2] == '\r' && buf[len-1] == '\n') {
	  buf[len-2] = '\n';
	  buf[len-1] = '\0';
	  len--;
	}
      }
      state_put(buf,len,&state_out);
    }
  }

  err = ferror (f_out); 
  err = err || fclose (f_out) != 0;
  elm_chown (savefile, userid, groupid);

  close_attachment(mailfile,f_in);

  if (err)
    error ("Error saving file!");
  else
    error ("Mail saved.");

  in_state_destroy(&state_in);
  out_state_destroy(&state_out);
  return;
}

static int attach_info P_((mime_t *ptr));

static void attach_edit (ptr)
     mime_t *ptr;
{
  int savetime;
  struct stat sb;
  char buf[STRING];

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_edit",
	       "Bad magic number");

  if (!ptr->pathname)
    return;

  if (-1 == stat (ptr->pathname, &sb)) {
    error("Can't stat file!");
    sleep_message();
    return;
  }
  if (strlen(ptr->pathname) + strlen(editor) > STRING-5)
    return;
  savetime = sb.st_mtime;
  Raw(OFF);
  sprintf (buf, "%s %s", editor, ptr->pathname);
  system_call (buf, 0);
  Raw(ON);
  if (stat (ptr->pathname, &sb) == -1 || sb.st_mtime != savetime)
    (void) attach_info (ptr);  /* update the information on this attachment */
  return;
}

static void attach_viewer (a)
     mime_t *a;
{
  char buf[LONG_STRING];

  struct header_rec tmp;
  FILE *tmpfp = NULL;
  struct stat sb;

  /* The next line is required so that mime_t_clear() doesn't call
   * mime_destroy() or free() on bad pointers
   */
  header_zero(&tmp);
  mime_t_clear (&tmp.mime_rec);

  if (a->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_viewer",
	       "Bad magic number");
    
  /* So that metapager() doesn't try to do special handling */
  tmp.status = MIME_MESSAGE;  /* Now I test again MIME_MESSAGE
			       * - K E H   <hurtta@dionysos.FMI.FI>
			       */
#ifdef USE_PGP
    tmp.pgp = 0;
#endif

  if (a->pathname) {
    ClearScreen();     /* Extra clear for attach_parse ... */

    if (can_open(a->pathname,"r") != 0) {
      error1 ("%.50s isn't readable by user!", a->pathname);
      sleep_message();
      return;
    }

    tmpfp = fopen (a->pathname, "r");
    if (! tmpfp) {
      error ("Could not open file for reading!");
      sleep_message();
      return;
    }

    tmp.mime_rec.flags = a->flags;
    tmp.mime_rec.type = a->type;
    strfcpy (tmp.mime_rec.subtype,a->subtype,
	     sizeof tmp.mime_rec.subtype);
    if (a->type_opts) {
      tmp.mime_rec.type_opts = strmcpy(tmp.mime_rec.type_opts,a->type_opts);
    }
    tmp.mime_rec.notplain = a->notplain;  

    tmp.mime_rec.disposition = DISP_INLINE;  /* Show it ! */
    tmp.offset = tmp.mime_rec.offset = 0;
    tmp.mime_rec.encoding = ENCODING_7BIT;
    stat(a->pathname, &sb);
    tmp.content_length = tmp.mime_rec.length = sb.st_size;
    attach_parse(&tmp, tmpfp);

  }
  else {

    /* Make copy of mime structure: */
    mime_t_copy(&(tmp.mime_rec),a);
    tmp.mime_rec.disposition = DISP_INLINE;  /* Show it ! */

    tmp.offset = tmp.mime_rec.offset = a -> offset;
    tmp.content_length = tmp.mime_rec.length = a->length;
#ifdef USE_PGP
    if (headers)
      tmp.pgp = headers[current-1]->pgp;
#endif
  }

  if (tmp.lines < 1)
    tmp.lines = tmp.content_length / 60; /* This value is used for selecting
					  * external pager versus internal
					  * pager.
					  */
  /* there is nothing to display! */
  if (tmp.mime_rec.length <= 0)
    goto fail;

  if (!mime_notplain(&(tmp.mime_rec)) || !have_metamail()) {

    metapager (tmpfp == NULL ? mailfile : tmpfp, &tmp, FALSE);
  } else {
    /* otherwise call metamail */

    if (!a->pathname) {
      char tmpfile[STRING], c;
      FILE *out;
      int bytes;

      sprintf (tmpfile, "%selm.%d", temp_dir, getpid());

      out = safeopen(tmpfile);

      if (out == NULL) {
	error1("Error creating tempfile %s",tmpfile);
	sleep_message();
	goto fail;
      }
      /* copy the headers plus the message to a temp file */
      fseek (mailfile, a->begin_offset, 0);
      bytes = a->begin_offset - a->offset;
      while (bytes < a->length) {
	c = fgetc (mailfile);
	fputc (c, out);
	bytes++;
      }
      fclose (out);
      sprintf (buf, "%s -m Elm -p -z %s", metamail_path,tmpfile);
    }
    else
      /* This is the case when we are viewing attachments for an outgoing
       * message.
       */
      sprintf(buf,"%s -m Elm -p -b -c %s/%s %s", 
	      metamail_path,TYPE(a->type), a->subtype, a->pathname);
    

    (void) elm_chown (tmpfile, userid, groupid);

    /* Option -z causes that metamail deletes input file */

    Raw (OFF);
    ClearScreen();
    printf ("Executing: %s\n", buf);
    system_call (buf, SY_ENAB_SIGINT|SY_ENV_METAMAIL);
    PressAnyKeyToContinue();
    Raw (ON);
  }

 fail:
  mime_t_clear (&(tmp.mime_rec));

  if (tmpfp) {
    fclose (tmpfp);
  }

  return;
}

/* Make initial attachment */
int Attach_it(pathname) 
     char *pathname;
{
  int need_enc;
  int is_text;
  mime_t * tmp;
  char buf[40];

  if (access(pathname,READ_ACCESS) < 0) {
    int err = errno;
    error2("%.45s: %.33s",pathname,error_description(err));
    return 0;
  }

  tmp = (mime_t *) mime_t_alloc ();
  tmp->pathname = strmcpy (tmp->pathname, pathname);

  /* Default disposition for attachments: attachment */
  tmp->disposition = DISP_ATTACH;

  need_enc = attach_info (tmp);

  if (need_enc < 0) {
    mime_destroy(tmp);
    return 0;
  }

  is_text = is_text_type (mime_types[tmp->type], 
			  tmp->subtype, tmp->encoding);


  if (tmp->type == MIME_TYPE_TEXT && (need_enc & HAVE_8BIT) &&
      !mime_get_param("charset",buf,tmp->type_opts,sizeof (buf)) &&
      charset[0] != '\0')
    add_parameter_t(tmp, "charset", charset,0);

  is_text = is_text_type (mime_types[tmp->type], 
			  tmp->subtype, tmp->encoding);
  
  if (is_text < 0 && (tmp->encoding == ENCODING_QUOTED || 
		      tmp->encoding == ENCODING_BASE64)) {
    error1 ("%.30s: Structured types don't allow encoding of data.",
	    pathname);
    sleep_message();

    mime_destroy(tmp);
    return 0;
  }

  tmp->next = attach_files;
  attach_files = tmp;
  return 1;
}

static int attach_modify (att, new)
     mime_t *att;
     int new;
{
  char buf[STRING];
  int update = TRUE, prompt = TRUE, need_enc = 0;
  int is_text = -1, ch;

  if (att->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_modify",
	       "Bad magic number");

  buf[0] = '\0';

  if (new) {
    /* set the default charset */
    add_parameter_t (att, "charset", charset, 0);

    /* Default disposition for attachments: attachment */
    att->disposition = DISP_ATTACH;

    /* force a prompt for a filename */
    prompt = FALSE;
    ch = 'f';
    att->unlink = 0;
  }
  
  /* 1 if is text type (true)
   * 0 if not text type
   * -1 if can't be encoded (ie structured) Message/ or Multpart/
   */
  is_text = is_text_type (TYPE(att->type), att->subtype, att->encoding);

  for (;;) {
    if (update) {
      int add = 0;
      ClearScreen ();
      
      Centerline (1, "Attachment Configuration");
      
      if (att->unlink) 
	sprintf (buf, "%-26.26s: %.*s", "Filename", 
		 sizeof(buf)-30, NONULL(att->pathname));
      else
	sprintf (buf, "%-26.26s: %.*s", "F)ilename", 
		 sizeof(buf)-30, NONULL(att->pathname));
      PutLine0 (3, 0, buf);
      sprintf (buf, "%-26.26s: %.*s", "D)escription", 
	       sizeof(buf)-30, NONULL(att->description));
      PutLine0 (4, 0, buf);

      sprintf (buf, "%-26.26s: %.15s/%.30s", "Content-T)ype",
               TYPE(att->type), att->subtype);
      if (att->type_opts) strfcat(buf,";", sizeof buf);
      PutLine0 (5, 0, buf);
      if (att->type_opts) {
	PutLine0 (6+add, 28, att->type_opts);
	add++;
      }
      sprintf (buf, "%-26.26s: %s", "content-transfer-E)ncoding",
	       ENCODING(att->encoding));
      PutLine0 (6+add, 0, buf);
      

      sprintf (buf, "%-26.26s: %.15s", "content-disP)osition", 
	       DISPOSITION(att->disposition));
      if (att->disposition_opts)
        strfcat(buf,";", sizeof buf);
      PutLine0 (7+add, 0, buf);
      if (att->disposition_opts) {
	PutLine0 (8+add, 28, att->disposition_opts);
	add++;
      }

      if (is_text < 0)
	sprintf (buf, "%-26.26s: %s", "CRLF-conversions",
		 "structured (direct content-encoding not allowed)");
      else if (is_text)
	sprintf (buf, "%-26.26s: %s", "CRLF-conversions",
		 "Text (line orienteed, convert LF <-> CRLF)");
      else 
	sprintf (buf, "%-26.26s: %s", "CRLF-conversions",
		 "Binary (no conversions)");
      PutLine0 (9+add, 0, buf);

      update = FALSE;
      show_last_error();
    }
    
    if (prompt) {
      PutLine0 (elm_LINES-2, 0, "Enter letter or RETURN to quit: ");
      ch = ReadCh(REDRAW_MARK);
      clear_error();
    }
    
    if (ch == '\n' || ch == 'q' || ch == 'Q' || ch == 'x' || ch == 'X')
      return (TRUE);
    else if (ch == ctrl('L') || ch == REDRAW_MARK)
      update = TRUE;
    else if (ch == 'f' || ch == 'F') {
      int code;

      if (att->unlink)  {
	error("You can't change filename!");
	continue;
      }
      if (att->pathname)
	strfcpy(buf, att->pathname, sizeof(buf));
      else
	buf[0] = '\0';

      prompt = TRUE;
      PutLine0 (elm_LINES-2, 0, "Filename: ");
      code = optionally_enter (buf, elm_LINES-2, 10, OE_APPEND_CURRENT|
			       OE_REDRAW_MARK, sizeof buf);
      if (REDRAW_MARK == code) {
	update = TRUE;
	prompt = FALSE;
	continue;
      }

      if (buf[0] != '\0') {
	if (access(buf,READ_ACCESS) < 0) {
	  int err = errno;
	  error2("%.45s: %.33s",buf,error_description(err));
	  continue;
	}
        att->pathname = strmcpy (att->pathname, buf);
	update = TRUE;

      } else {
	update = TRUE;

        if (att->pathname) {
          free (att->pathname);
          att->pathname = NULL;
        }
        if (new)
          return FALSE;
        else
          continue;
      }

      /* Set some information about this attachment */
      need_enc = attach_info (att);
      if (need_enc < 0)
	continue;

      /* 1 if is text type (true)
       * 0 if not text type
       * -1 if can't be encoded (ie structured) Message/ or Multpart/
       */
      is_text = is_text_type (TYPE(att->type), att->subtype, att->encoding);
      if (is_text > 0 && (need_enc & HAVE_BINARY)) {
	error ("Warning: BINARY data? Check Content-Type!");
      }

      if (is_text < 0 && (att->encoding == ENCODING_QUOTED || 
			  att->encoding == ENCODING_BASE64)) {
	/* Reconsider encoding ... */
	ch = 'e';
	prompt = FALSE;
	update = TRUE;
	error ("Structured types don't allow encoding of data.");
	sleep_message();

	break;
      } 

      /* now let the user do what they want */
      if (new)
	prompt = TRUE;
      
    }
    else if (ch == 'd' || ch == 'D') {
      int code;

      PutLine0 (elm_LINES-2, 0, "Description: ");
      buf[0] = '\0';
      if (att->description)
	strfcpy(buf,att->description,sizeof(buf));

      prompt = TRUE;
      code =
	optionally_enter (buf, elm_LINES-2, 13, OE_APPEND_CURRENT|
			  OE_REDRAW_MARK, sizeof buf);
      if (REDRAW_MARK == code) {
	update = TRUE;
	prompt = FALSE;
	continue;
      }
      if (code != 0)
	continue;

      if (buf[0])
        att->description = strmcpy (att->description, buf);
      else if (att->description) {
        free (att->description);
	att->description = NULL;
      }
      update = TRUE;
    }
    else if (ch == 't' || ch == 'T') {
      int code;

      prompt = TRUE;
      PutLine0 (elm_LINES-2, 0, "Content-Type: ");
      att -> flags &= ~(MIME_RFC822);
      att -> flags &= ~(MIME_MIXED);
      att -> flags &= ~(MIME_DIGEST);
      sprintf (buf, "%.15s/%.30s", TYPE(att->type), att->subtype);
      if (att->type_opts) {
	int l;

	strfcat (buf, "; ", sizeof buf);
        l = strlen (buf);
	strfcpy (buf + l, att->type_opts, sizeof (buf) - l);
      }
      
      code = optionally_enter (buf, elm_LINES-2, 14, OE_APPEND_CURRENT|
			       OE_REDRAW_MARK, sizeof buf);
      if (REDRAW_MARK == code) {
	update = TRUE;
	prompt = FALSE;
	continue;
      }
      if (0 != code)
	continue;

      mime_get_content (buf, att);
      if (att->type == MIME_TYPE_TEXT && (need_enc & HAVE_8BIT) &&
	  !mime_get_param("charset",buf,att->type_opts,sizeof (buf)))
	add_parameter_t(att, "charset", charset,0);
      
      /* 1 if is text type (true)
       * 0 if not text type
       * -1 if can't be encoded (ie structured) Message/ or Multpart/
       */
      is_text = is_text_type (mime_types[att->type], 
			      att->subtype, att->encoding);
      update = TRUE;
      if (is_text < 0 && (att->encoding == ENCODING_QUOTED || 
			  att->encoding == ENCODING_BASE64)) {
	/* Reconsider encoding ... */
	ch = 'e';
	prompt = FALSE;
	error ("Structured types don't allow encoding of data.");
	sleep_message();
      } 
    }
    else if (ch == 'p' || ch == 'P') {
      int code;

      prompt = TRUE;
      PutLine0 (elm_LINES-2, 0, "Content-Disposition: ");
      strfcpy (buf, DISPOSITION(att->disposition), sizeof buf);
      if (att->disposition_opts) {
	int l;

	strfcat (buf,"; ", sizeof buf);
        l = strlen (buf);
	strfcpy (buf + l, att->disposition_opts, sizeof (buf) - l);
      }
      
      code = optionally_enter (buf, elm_LINES-2, 21, 
			       OE_REDRAW_MARK|OE_APPEND_CURRENT,
			       sizeof buf);
      if (REDRAW_MARK == code) {
	update = TRUE;
	prompt = FALSE;
	continue;
      }
      if (0 != code)
	continue;
      mime_get_disposition (buf, att);
      update = TRUE;
    }
    else if (ch == 'e' || ch == 'E') {
      prompt = TRUE;
      PutLine0 (elm_LINES-2, 0, "Content-Transfer-Encoding: ");
      Centerline (elm_LINES-1, "<SPACE> for next value, <RETURN> to accept.");
      for (;;) {
	MoveCursor (elm_LINES-2, 27);
	CleartoEOLN ();

#define NEXT_ENCODING  { \
			   att->encoding++; \
			   if (att->encoding > 5) att->encoding = 1; \
			   continue; \
		       }

#ifndef USE_8BITMIME
	if (allow_no_encoding < 1) {
	  if (att->encoding == ENCODING_8BIT) {  /* Mailer won't support ! */
	    /* TRY next encosing instead */
	    NEXT_ENCODING;
	  }
	}
#endif
	
#ifndef USE_BINARYMIME
	if (allow_no_encoding < 2) {
	  if (att->encoding == ENCODING_BINARY) {  /* Mailer won't support ! */
	    /* TRY next encoding instead */
	    NEXT_ENCODING;
	  }
	}
#endif

	/* Don't allow 7bit if the file contains 8bit chars... 
	 * 7bit encoding is allowed if file includes control characters 
	 */
	if (att->encoding == ENCODING_7BIT && (need_enc & HAVE_8BIT)) {
	  NEXT_ENCODING;
	}
	/* Don't allow 7bit or 8bit if the file required binary
	 * encoding according of Mime Draft Standard.
	 */
	if ((att->encoding == ENCODING_7BIT || att->encoding == ENCODING_8BIT)
	    && (need_enc & HAVE_BINARY)) {
	  NEXT_ENCODING;
	}

	/* Don't allow encoding for Multipart/ and Message/ 
	 * See mime Draft Standard. Be carefull that don't create
	 * infinitive loop! */

	if (is_text < 0) {
	  static int again = 0;    /* Prevent looping */
	  if (att->encoding == ENCODING_QUOTED || 
	      att->encoding == ENCODING_BASE64) {
	    if (again == att->encoding) {
	      error("Structured types must be encoded in leaf type!");
	      sleep_message();

	      /* prompt for new content-type */
	      prompt = FALSE;
	      ch = 't';
	      break;
	    } else {
	      if (!again)
		again = att->encoding;
	      NEXT_ENCODING;
	    }
	  } else
	    again = 0;
	}

	Write_to_screen (ENCODING(att->encoding), 0);
	ch = ReadCh(REDRAW_MARK);
	if (ch == '\n')
	  break;
	else if (ch == ' ') {
	  NEXT_ENCODING;
	}
	else if (ch == REDRAW_MARK) {
	  update = TRUE;
	  prompt = FALSE;
	  ch =  'e';
	  
	  break;
	}
      }

#undef NEXT_ENCODING

      ClearLine (elm_LINES-1);
      /* 1 if is text type (true)
       * 0 if not text type
       * -1 if can't be encoded (ie structured) Message/ or Multpart/
       */
      is_text = is_text_type (mime_types[att->type], att->subtype, att->encoding);
      update = TRUE;
    }
    else
      error ("Unknown command.");
  }
  /* Not reached. */
}

static void
mime_guess_content_type (ptr)
     mime_t *ptr;
{
  /* This routine tries to guess the content-type of an attachment by looking
   * at the suffix of ptr->pathname.  It first looks to see if it can find
   * an entry in ~/.elm/mime.types, then in "system_mime_types", and failing
   * that, to a small list of builtin definitions.
   */
  int i, found = FALSE;
  char *p, *c, buf[LONG_STRING];
  FILE *fp;

  dprint (3, (debugfile, "mime_guess_content_type: pathname=%s\n",
	      ptr->pathname));

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"mime_guess_content_type",
	       "Bad magic number");

  /* Set the "filename" field for content-disposition */
  p = strrchr (ptr->pathname, '/');
  if (p)
    p++;
  else
    p = ptr->pathname;
  buf[0] = '\0';
  add_parameter (buf, "filename", p, sizeof (buf), 0);
  ptr->disposition_opts = strmcpy (ptr->disposition_opts, buf);

  /* Try to guess the content type of the data by the filename extension */
  p = strrchr (ptr->pathname, '.');
  if (! p)
    return;
  p++;
  for (i = 0; i < 3; i++) {
  dprint (3, (debugfile, 
	      "mime_guess_content_type: searching \"%s\", i=%d\n",
	      p,i));
    if (i == 0) {
      
      /* First try the user's mime.types file */
      
      sprintf (buf, "%s/.elm/mime.types", home);
      fp = fopen (buf, "r");
      if (!fp)
	continue;
    }
    else if (i == 1) {
      
      /* If it wasn't there, try the system file... */
      
      fp = fopen (system_mime_types, "r");
      if (! fp)
	continue;
    }
    else {
      /* Couldn't find user or system mime.types file,
       * use these defaults...
       */
      if (istrcmp (p, "ps") == 0) {
	ptr->type = MIME_TYPE_APPLICATION;
	strfcpy (ptr->subtype, "postscript", sizeof ptr->subtype);
        if (ptr->type_opts) {
          free (ptr->type_opts);
          ptr->type_opts = NULL;
        }
      }
      else if (istrcmp (p, "gif") == 0 || istrcmp (p, "jpeg") == 0 ||
	       istrcmp (p, "tiff") == 0) {
	ptr->type = MIME_TYPE_IMAGE;
	strfcpy (ptr->subtype, p, sizeof ptr->subtype);
        if (ptr->type_opts) {
          free (ptr->type_opts);
          ptr->type_opts = NULL;
        }
      }
      dprint (3, (debugfile, 
		  "mime_guess_content_type: built-in default \"%s\" as \"%s/%s\"\n", 
		  p, TYPE(ptr->type), ptr->subtype));
    }
    
    if (i < 2) {
      while (fgets (buf, LONG_STRING, fp) != NULL) {
	if (buf[0] == '#') /* Skip comments */
	  continue;
	c = buf;
	while (*c && isspace ((unsigned char) *c)) /* skip leading whitespace */
	  c++;
	if (! *c)
	  continue;
	if (strincmp (c, p, strlen (p)) == 0) {
	  buf[strlen (buf) - 1] = '\0';
	  mime_get_content (c + strlen (p), ptr);
	  dprint (3, (debugfile, "mime_guess_content_type: user defined \"%s\" as \"%s/%s\"\n", p, TYPE(ptr->type), ptr->subtype));
	  found = TRUE;
	  break;
	}
      }
      fclose (fp);
      if (found)
	break;
    }
  }
  return;
}

static int attach_info (ptr)
     mime_t *ptr;
{
  struct stat sb;
  FILE *fp;
  int need_enc;

  if (ptr->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_info",
	       "Bad magic number");

  if (stat (ptr->pathname, &sb) == -1) {
    if (errno == ENOENT)
      error ("That file does not exist!");
    else
      error ("Could not stat file!");
    sleep_message();
    return (-1);
  }

  ptr->length = sb.st_size;

  if (can_open(ptr->pathname,"r") != 0) {
    error1 ("%.50s isn't readable by user!", ptr->pathname);
    ptr->pathname[0] = '\0';
    sleep_message();
    return (-1);
  }

  mime_guess_content_type (ptr);

  /* Figure out what the default encoding is... */

  error1 ("Checking %s...", ptr->pathname);
  fp = fopen (ptr->pathname, "r");
  if (!fp) {
    error1 ("Can't open %s!", ptr->pathname);
    sleep_message();
    return -1;
  }

  need_enc = needs_encoding (fp);
  
  if (need_enc & HAVE_CTRL)
    ptr->encoding = (need_enc & HAVE_BINARY) 
      ? ENCODING_BASE64 : ENCODING_QUOTED;
  else if (need_enc & HAVE_BINARY) { 
    /* HAVE_BINARY, but not HAVE_CTRL so that have long lines! */
#ifdef USE_BINARYMIME
    ptr->encoding = ENCODING_BINARY;
#else
    ptr->encoding = ENCODING_QUOTED;
#endif
  }
  else if (need_enc & HAVE_8BIT) {
#ifdef USE_8BITMIME
    ptr->encoding = ENCODING_8BIT;
#else
    ptr->encoding = ENCODING_QUOTED;
#endif
  }
  fclose (fp);
  clear_error();  /* Remove reading ... -message */

  dprint(3,(debugfile,"attach_info: need_enc=%d, encoding=%d, pathname=%s\n",
	    need_enc,ptr->encoding,ptr->pathname));
  return (need_enc);
}

static void
attach_header (mt, num, is_cur, use_desc, offset)
     mime_t *mt;
     int num, is_cur, offset, use_desc;
{
  /* Displays a header for a menu for the specified attachment. */
  char *Encoding = "???";
 
  char buf[LONG_STRING], buf2[LONG_STRING];
  int len, len2;
  int Width = elm_COLUMNS;     /* Protect arbitary big values of COLUMNS */
  if (Width > sizeof (buf)-1)
    Width = sizeof (buf)-1;

  if (mt->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_header",
	       "Bad magic number");

  Encoding = ENCODING(mt->encoding);

  sprintf (buf,
	   "%4d %-30.30s (%d) ",
	   num,
	   use_desc ? NONULL(mt->description) : NONULL(mt->pathname),
	   mt->length);
  if (mt->length < 10)
    strfcat (buf, "   ", sizeof buf);
  else if (mt->length < 100)
    strfcat (buf, "  ", sizeof buf);
  else if (mt->length < 1000)
    strfcat (buf, " ", sizeof buf);
  sprintf (buf2, "%.15s/%.30s", TYPE(mt->type), mt->subtype);
  strfcat (buf, buf2, sizeof buf);
  len = Width - strlen (buf);
  len2 = strlen (Encoding) + 3;
  if (len2 > len) {
    buf[Width-len2] = '\0';
    len = len2 = 0;
  }
  sprintf (buf2, "%s %*.*s[%s]", buf, len-len2, len-len2, "", Encoding);
  
  if (is_cur)
    StartBold();
  PutLine0 (offset, 0, buf2);
  if (is_cur)
    EndBold();
}

mime_t * attach_menu (mt, rdonly)
     mime_t *mt;
     int rdonly;
{
  /* A generic attachment menu.  "rdonly" controls whether or not the list
   * of attachments "mt" may be edited.
   */

  mime_t **ptrs = NULL, *tmp, *ret = NULL;
  int ptr_len = 0, ptr_max = 0, i, cur = 0, offset = 3, key_offset, ch;
  int update = TRUE;
  char buf[STRING];
  int top = 0;

  if (mt && mt->magic != MIME_magic)
    mime_panic(__FILE__,__LINE__,"attach_menu",
	       "Bad magic number");

  /* Generate an array of pointers so it is easier to work with. */
  while (mt) {
    if (mt->magic != MIME_magic)
      mime_panic(__FILE__,__LINE__,"attach_menu",
		 "Bad magic number (next chain)");

    if (ptr_len == ptr_max)
      ptrs = (mime_t **) DynamicArray (ptrs, sizeof (mime_t *), &ptr_len, 5);
    ptrs[ptr_max++] = mt;
    mt = mt->next;
  }

  for (;;) {
    if (cur < top || cur >= top + elm_LINES-3-offset) {
      if (cur < top) 
	top -= elm_LINES-3-offset;
      if (cur >= top + elm_LINES-3-offset) 
	top += elm_LINES-3-offset;
      if (top >= ptr_max)
	top = ptr_max - elm_LINES +3 +offset;
      if (top < 0)
	top = 0;
      update = TRUE;
    }
    if (update) {
      ClearScreen ();
      sprintf (buf, "Attachment Menu (%d attachments)", ptr_max);
      Centerline (1, buf);
      if (! rdonly)
	strfcpy (buf, "a)dd, e)dit, d)elete, m)odify, ", sizeof buf);
      else
	buf[0] = '\0';
      strfcat (buf, "p)rint, s)ave, v)iew subparts, q)uit", sizeof buf);
      Centerline (elm_LINES-1, buf);
      for (i = top; i < ptr_max && i < top + elm_LINES -3 - offset; i++)
	attach_header (ptrs[i], i + 1, i == cur, rdonly, offset + i - top);
      update = FALSE;
      show_last_error(); 
    }
    ClearLine (elm_LINES-2);
    PutLine0 (elm_LINES-2, 0, "Attachments: ");
    ch = ReadCh (REDRAW_MARK|READCH_CURSOR);
    clear_error(); /* Clear the error message (from buffer also) */
    switch (ch) {
    case '-':
    case LEFT_MARK:
    case PAGEUP_MARK:
      attach_header (ptrs[cur], cur+1, FALSE, rdonly, offset + cur - top);
      cur -= elm_LINES -3;
      if (cur < 0)
	cur = 0;
      break;
    case '+':
    case RIGHT_MARK:
    case PAGEDOWN_MARK:
      attach_header (ptrs[cur], cur+1, FALSE, rdonly, offset + cur - top);
      attach_header (ptrs[cur], cur+1, FALSE, rdonly, offset + cur - top);
      cur += elm_LINES -3;
      if (cur > ptr_max - 1) 
	cur = ptr_max - 1;
      break;
    case 's':
      if (ptr_max > 0)
        attach_save (ptrs[cur]);
      else
        error("There are no attachments!");
      break;
    case ' ':
    case '\n':
      if (ptr_max > 0) {
        attach_viewer (ptrs[cur]);
        update = TRUE;
      }
      else
        error("There are no attachments!");
      break;
    case 'p':
      if (ptr_max > 0)
        attach_print(ptrs[cur]);
      else
        error("There are no attachments!");
      break;
    case 'v': /* Perhaps it is better that attachment meny shows whole
	       * structure -- but this is temporary hack.... */
      if (ptr_max == 0) {
        error("There are no attachments!");
        break;
      }
      if (ptrs[cur]->parts) {
	attach_menu(ptrs[cur]->parts,TRUE);
	update = TRUE;
      }
      break;
    case 'e':
      if (! rdonly) {
        if (ptr_max == 0) {
          error("There are no attachments!");
          break;
        }
	attach_edit (ptrs[cur]);
	update = TRUE;
      }
      break;
    case 'd':
      if (! rdonly) {
        if (ptr_max == 0) {
          error("There are no attachments!");
          break;
        }
	if (question_me) {
	  for(;;) {
	    PutLine0(elm_LINES-2, 0, "Are you sure? (y/n): y");
	    MoveCursor(elm_LINES-2, 21);
	    ch = ReadCh(0);
	    if (ch == 'y' || ch == '\n' || ch == 'n')
	      break;
	  }
	  ClearLine(elm_LINES-2);
	  if (ch == 'n')
	    break;
	}
      delete_it:
	/* List will be rebuild in exit */
	ptrs[cur]->next = NULL;
  	mime_destroy (ptrs[cur]);
	ptrs[cur] = NULL;
	/* Shift the rest of the pointers down by one. */
	for (i = cur + 1; i < ptr_max; i++)
	  ptrs[i-1] = ptrs[i];
	ptrs[ptr_max-1] = NULL;
        ptr_max--;
	update = TRUE;
      }
      break;
    case 'a':
      if (! rdonly) {
	tmp = (mime_t *) mime_t_alloc ();
	if (attach_modify (tmp, TRUE)) {
	  if (ptr_len == ptr_max)
	    ptrs = (mime_t **) DynamicArray (ptrs, sizeof (mime_t *), &ptr_len, 5);
	  ptrs[ptr_max++] = tmp;
	}
	else {
	  /* List will be rebuild in exit */
	  tmp->next = NULL;
	  mime_destroy(tmp);
	}
        update = TRUE;
      }
      break;
    case 'm':
      if (! rdonly) {
        if (ptr_max == 0) {
          error("There are no attachments!");
          break;
        }
	attach_modify (ptrs[cur], FALSE);
	/* If there is not pathname it is otherwise assumed to be 
	 * part from mailfile...!
	 */
	if (ptrs[cur]->pathname == NULL)
	  goto delete_it;
        update = TRUE;
      }
      break;
    case 'j':
    case 'J':
    case 'n':
    case DOWN_MARK:
    case ctrl('N'):
    NEXT_ATTACH:
      if (cur >= ptr_max - 1) {
	error ("You are on the last attachment!");
	break;
      }
      attach_header (ptrs[cur], cur+1, FALSE, rdonly, offset + cur - top);
      cur++;
      if (cur < top + elm_LINES -3 - offset)
	attach_header (ptrs[cur], cur+1, TRUE, rdonly, offset + cur - top);
      break;
    case 'k':
    case 'K':
    case ctrl('K'):
    case UP_MARK:
    PREV_ATTACH:
      if (cur == 0) {
	error ("You are on the first attachment!");
	break;
      }
      attach_header (ptrs[cur], cur+1, FALSE, rdonly, offset + cur - top);
      cur--;
      if (cur >= top)
	attach_header (ptrs[cur], cur+1, TRUE, rdonly, offset + cur - top);
      break;
    case 'i':
      if (! rdonly)
        break;
      /* else fall through to next statement! */
    case 'q':
    case 'x':
      if (ptrs == NULL)
        return NULL;
      if (! rdonly) {
	/* The attachments might have been edited, so rebuild the list */
	if (ptr_max > 0) {
	  ret = tmp = ptrs[0];

	  if (ret->magic != MIME_magic)
	    mime_panic(__FILE__,__LINE__,"attach_menu",
		       "Bad magic number (ptrs[0])");

	  for (i = 1; i < ptr_max; i++) {
	    
	    if (ptrs[i]->magic != MIME_magic)
	      mime_panic(__FILE__,__LINE__,"attach_menu",
			 "Bad magic number (ptrs[..])");

	    tmp->next = ptrs[i];
	    tmp = tmp->next;
	  }
	  tmp->next = NULL;
	}
	else
	  ret = NULL;
      }
      else
	ret = mt;
      free (ptrs);
      return ret;
#ifdef USE_PGP
    case ctrl('F'):
      pgp_void_passphrase();
      error("Passphrase forgotten!");
      break;
#endif
    case ctrl('L'):
    case REDRAW_MARK:
      update = TRUE;
      break;
    default:
      error1 ("Unknown command: %c", ch);
    }
  }
}
#endif /* MIME */
