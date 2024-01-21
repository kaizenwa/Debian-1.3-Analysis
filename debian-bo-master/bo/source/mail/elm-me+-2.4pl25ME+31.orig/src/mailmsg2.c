static char rcsid[] = "@(#)$Id: mailmsg2.c,v 5.39 1994/05/30 17:24:38 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.39 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 *****************************************************************************/

/** Interface to allow mail to be sent to users.  Part of ELM  **/

#include "headers.h"
#include "s_elm.h"
#include <errno.h>
#include "me.h"

#ifdef USE_DSN
#include "menu2.h"
#endif

extern int errno;
extern char version_buff[];

char *error_description(), *strip_parens();
char *format_long(), *strip_commas(), *tail_of_string(); 
long ftell();

#ifdef SITE_HIDING 
 char *get_ctime_date();
#endif
FILE *write_header_info();

/* these are all defined in the mailmsg1.c file! */

extern char subject[SLEN], in_reply_to[SLEN], expires[SLEN],
            action[SLEN], priority[SLEN], reply_to[SLEN], to[VERY_LONG_STRING], 
	    cc[VERY_LONG_STRING], expanded_to[VERY_LONG_STRING], 
	    expanded_reply_to[LONG_STRING],
	    expanded_cc[VERY_LONG_STRING], user_defined_header[SLEN],
	    bcc[VERY_LONG_STRING], expanded_bcc[VERY_LONG_STRING],
	    precedence[SLEN], expires_days[SLEN];

long cl_offset;
long cl_start;
long cl_end;

#ifdef	MIME
extern mime_t *attachments;
#endif /* MIME */
#ifdef USE_REMAILER
extern int remailing;
#endif

int  gotten_key;
char *bounce_off_remote();

/*
 * remove_hostbang - Given an expanded list of addresses, remove all
 * occurrences of "thishost!" at the beginning of addresses.
 * This hack is useful in itself, but it is required now because of the
 * kludge disallowing alias expansion on return addresses.
 */

void
remove_hostbang(addrs)
char *addrs;
{
	int i, j, hlen, flen;

	if ((hlen = strlen(hostname)) < 1)
	  return;

	flen = strlen(hostfullname);
	i = j = 0;

	while (addrs[i]) {
	  if (j == 0 || isspace(addrs[j - 1])) {
	    if (strncmp(&addrs[i], hostname, hlen) == 0 &&
	      addrs[i + hlen] == '!') {
	        i += hlen + 1;
	        continue;
	    }
	    if (strncmp(&addrs[i], hostfullname, flen) == 0 &&
	      addrs[i + flen] == '!') {
	        i += flen + 1;
	        continue;
	    }
	  }
	  addrs[j++] = addrs[i++];
	}
	addrs[j] = 0;
}

#ifdef MIME
static int check_8bit_str P_((char *str));    /* Prototype */
#endif

static int verify_transmission P_((char *,int  *, int *, int, char *,
				   int, int, int *, int));  /* Prototype */


int mail(current_header,options,form)
     struct header_rec * current_header;
     int options,form;
{
  int copy_msg       = 0 != current_header && 0 != (options & MAIL_COPY_MSG);
  int edit_message   =                        0 != (options & MAIL_EDIT_MSG);
  int replying       = 0 != current_header && 0 != (options & MAIL_REPLYING);
  int forwarding     = 0 != current_header && 0 != (options & MAIL_FORWARDING);

	/** Given the addresses and various other miscellany (specifically, 
	    'copy-msg' indicates whether a copy of the current message should 
	    be included, 'edit_message' indicates whether the message should 
	    be edited) this routine will invoke an editor for the user and 
	    then actually mail off the message. 'form' can be YES, NO, or
	    MAYBE.  YES=add "Content-Type: mailform" header, MAYBE=add the
	    M)ake form option to last question, and NO=don't worry about it!
	    Also, if 'copy_msg' = FORM, then grab the form temp file and use
	    that...
	    Return TRUE if the main part of the screen has been changed
	    (useful for knowing whether a redraw is needed.
	**/

	FILE *reply, *real_reply; /* second is post-input buffer */
	char *whole_msg_file, *tempnam();
	char filename[SLEN], fname[SLEN], copy_file[SLEN],
             very_long_buffer[VERY_LONG_STRING];
	  
	char *mailerflags[20], **argv;
	char t[80];

	int ch, sys_status, line_len, mf_idx=0;
	register int retransmit = FALSE; 
	int      already_has_text = FALSE;		/* we need an ADDRESS */
	int	 signature_done = FALSE;
	int	 need_redraw = 0;
	int	 err;
	int reask_verify = FALSE;
	int dsn = 0;
#ifdef MIME
	mime_send_t MIME_info;
#endif

	static int cancelled_msg = 0;

        if (current_header && (options & MAIL_ISFORM))
	  copy_msg = FORM;

#ifdef MIME
	/* Initialize structure */
	MIME_info.encoding_top = ENCODING_7BIT;
        MIME_info.type_opts_top[0] = '\0';
	MIME_info.encoding_text = ENCODING_7BIT;
	MIME_info.Charset = charset;
	MIME_info.need_enc = 0;
	MIME_info.type_text = MIME_TYPE_TEXT;
	strfcpy (MIME_info.subtype_text, "plain", 
		 sizeof MIME_info.subtype_text);
        MIME_info.type_opts_text[0] = '\0';
	MIME_info.encoded_subject[0] = '\0';
	strfcpy (MIME_info.encoded_fullname, full_username,
		 sizeof(MIME_info.encoded_fullname));
#endif /* MIME */
#ifdef USE_REMAILER
        remailing = FALSE;
#endif
	cl_offset = cl_start = cl_end = 0;

	dprint(4, (debugfile, "\nMailing to \"%s\" (with%s editing)\n",
		  expanded_to, edit_message? "" : "out"));
	/* this will get set to 1 on a successful reply */ 
	me_retcode = 0;

	gotten_key = 0;		/* ignore previously gotten encryption key */

	/** first generate the temporary filename **/

	sprintf(filename,"%s%s%d", temp_dir, temp_file, getpid());

	/** if possible, let's try to recall the last message? **/

	if (! batch_only && copy_msg != FORM && user_level != 0)
	  retransmit = recall_last_msg(filename, copy_msg, &cancelled_msg, 
		       &already_has_text);

	/** if we're not retransmitting, create the file.. **/

	if (! retransmit)
        {
	  if ((reply = safeopen_rdwr(filename)) == NULL) {
	    err = errno;
	    dprint(1, (debugfile, 
               "Attempt to write to temp file %s failed with error %s (mail)\n",
		 filename, error_description(err)));
	    if(batch_only) {
	      MCprintf(catgets(elm_msg_cat, ElmSet, ElmCouldNotCreateFile,
		"Could not create file %s (%s)."),
		filename, error_description(err));
	      printf("\n");
	    } else
	      error2(catgets(elm_msg_cat, ElmSet, ElmCouldNotCreateFile,
		"Could not create file %s (%s)."),
		filename, error_description(errno));
	    return(need_redraw);
	  }
	}
   

	(void) elm_chown(filename, userid, groupid);

	/* copy the message from standard input */
	if (batch_only) {
	  while (line_len = fread(very_long_buffer, 1, sizeof(very_long_buffer), stdin))
	    fwrite(very_long_buffer, 1, line_len, reply);
	}

	/** if there is an included file, copy it into the temp file **/
	if (*included_file) {
	  FILE *input;
	  if ((input = fopen(included_file,"r")) == NULL) {
	    dprint(1, (debugfile, 
              "Can't open included file %s.  Failed with error %s (mail)\n",
	      included_file, error_description(errno)));
	    error1(catgets(elm_msg_cat, ElmSet, ElmCouldNotOpenFile,
	      "Could not open file %s."), included_file);
	    return(need_redraw);
	  }

	  while (fgets(very_long_buffer, VERY_LONG_STRING, input) != NULL) 
	    fputs(very_long_buffer, reply);

	  fclose(input);
	  already_has_text = TRUE;
	}

	if (copy_msg == FORM) {
	  sprintf(fname, "%s%s%d", temp_dir, temp_form_file, getpid());
	  fclose(reply);	/* we can't retransmit a form! */
	  if (access(fname,ACCESS_EXISTS) != 0) {
	    if(batch_only) {
	      printf(catgets(elm_msg_cat, ElmSet, ElmCouldNotFindForm,
		"Couldn't find forms file!"));
	      printf("\n");
	    } else
	      error(catgets(elm_msg_cat, ElmSet, ElmCouldNotFindForm,
		"Couldn't find forms file!"));
	    return(need_redraw);
	  }
	  dprint(4, (debugfile, "-- renaming existing file %s to file %s --\n",
		  fname, filename));
	  rename(fname, filename);

	  /* kill leftover headers since forms reply skips regular init */
	  expires[0] = '\0';
	  expires_days[0] = '\0';
	  action[0] = '\0';
	  priority[0] = '\0';
	  reply_to[0] = '\0';
	  expanded_reply_to[0] = '\0';
	  cc[0] = '\0';
	  expanded_cc[0] = '\0';
	  user_defined_header[0] = '\0';
	  bcc[0] = '\0';
	  expanded_bcc[0] = '\0';
	  precedence[0] = '\0';
	}
	else if (copy_msg && ! retransmit) {  /* if retransmit we have it! */
#ifdef MIME
	  if (!forwarding || (forwarding && !mimeforward)) {
#endif
	    if (forwarding && !quote_forward) {
#ifdef MIME
	      strfcpy (very_long_buffer, current_header->from,
		       sizeof (very_long_buffer));
              if ((current_header->status & MIME_MESSAGE) && 
		  is_rfc1522 (very_long_buffer))
	        rfc1522_decode (very_long_buffer, sizeof (very_long_buffer));
	      fprintf (reply, "----- Forwarded message from %s -----\n\n", 
		       very_long_buffer);
#else
	      fprintf (reply, "----- Forwarded message from %s -----\n\n", 
		       current_header->from);
#endif
	    }
	    else if (attribution[0] && current_header) {
#ifdef MIME
	      strfcpy (very_long_buffer, current_header->from,
		       sizeof (very_long_buffer));
              if ((current_header->status & MIME_MESSAGE) && 
		  is_rfc1522 (very_long_buffer))
	        rfc1522_decode (very_long_buffer, sizeof (very_long_buffer));
	      fprintf(reply, attribution, very_long_buffer);
#else
	      fprintf(reply, attribution, current_header->from);
#endif
	      fputc('\n', reply);
	    }
	    if (edit_message) {
	      int NOHDR = forwarding ? noheaderfwd : noheader;
	      int NOQUOTE = forwarding && !quote_forward; 
	      copy_message(mailfile,current_header,
			   NOQUOTE ? "" : prefixchars, reply,
			   ( NOHDR ? CM_REMOVE_HEADER : 0 ) 
			   | CM_MMDF_HEAD | CM_DECODE 
			   /* I think it is good idea to use CM_FILT_HDR
			    * even when we don't have forwarding ... -KEH
			    */
			   | CM_FILT_HDR );
	      already_has_text = TRUE;	/* we just added it, right? */
	    }
	    else {
	      int NOHDR = forwarding ? noheaderfwd : noheader;
	      copy_message(mailfile,current_header,
			   "", reply,
			   ( NOHDR ? CM_REMOVE_HEADER : 0 ) 
			   | CM_MMDF_HEAD
			   /* I added CM_DECODE to here -KEH */
			   | CM_DECODE);
	    }
	    if (forwarding && !quote_forward) {
#ifdef MIME
	      strfcpy (very_long_buffer, current_header->from,
		       sizeof (very_long_buffer));
              if ((current_header->status & MIME_MESSAGE) && 
		  is_rfc1522 (very_long_buffer))
	        rfc1522_decode (very_long_buffer, sizeof (very_long_buffer));
	      fprintf (reply, "----- End of forwarded message from %s -----\n",
		       very_long_buffer);
#else
	      fprintf (reply, "----- End of forwarded message from %s -----\n",
		       current_header->from);
#endif
	    }
#ifdef MIME
	  }
	  else {
	    FILE *tmpfp;
	    int len,first_line = 1;
	    int in_headers = TRUE;

	    /* Use MESSAGE/RFC822 to forward messages. */

	    sprintf (very_long_buffer, "%selmfwd.%d", temp_dir, getpid ());
	    if (! (tmpfp = safeopen_rdwr (very_long_buffer))) {
	      error("Failed to create file for forwarding");
	    } else {
	      attachments = (mime_t *) mime_t_alloc ();
	      attachments->flags = MIME_RFC822;
	      attachments->pathname = (char *) strmcpy(attachments->pathname, 
						       very_long_buffer);


	      copy_message(mailfile,current_header,"",tmpfp,
			   CM_REMOVE_ENVELOPE);
	   
	      attachments->length = fsize(tmpfp);
	      fclose (tmpfp);
	      attachments->unlink = 1; /* mark for later deletion */
	      attachments->type = MIME_TYPE_MESSAGE;
	      strfcpy (attachments->subtype, "rfc822", 
		       sizeof attachments->subtype);
	      sprintf (very_long_buffer, "Forwarded message from %s", 
		       current_header->from);
	      attachments->description = (char *)strmcpy (attachments->description,
							  very_long_buffer);
	      
	      /* Pick up the encoding from the message. */
	      attachments->encoding = ENCODING_7BIT;
	      (void) update_encoding (&(attachments->encoding),
				      current_header->mime_rec.encoding);
	      
	    }
	  }
#endif /* MIME */
	}

#ifdef MIME
	/* Initial attachment */
	if(attachments) 
	  attachments->next = attach_files;
	else
	  attachments = attach_files;
	attach_files = NULL;
#endif

        /* append signature now if we are going to use an external editor */
	/* Don't worry about the remote/local determination too much */

        if (already_has_text || 
           (strcmp(editor,"builtin") != 0 && strcmp(editor,"none") != 0)) {
	     signature_done = TRUE;
             if (!retransmit && copy_msg != FORM) 
	       already_has_text |= append_sig(reply);
	}

	if (! retransmit && copy_msg != FORM)
	  if (reply != NULL)
	    (void) fclose(reply);	/* on replies, it won't be open! */

	/** Edit the message **/

	/* calculate default save_file name */
	if(auto_cc) {
	  if(save_by_name) {
	    if(force_name) {
	      strfcpy(copy_file, "=",
		      sizeof copy_file);    /* signals save by 'to' logname */
	    } else {
	      strfcpy(copy_file, "=?",
		      sizeof copy_file); /* conditional save by 'to' logname */
	    }
	  } else {
	    strfcpy(copy_file, "<", sizeof copy_file);	/* signals save to sentmail */
	  }
	} else *copy_file = '\0';	/* signals to not save a copy */

#ifdef MIME
	/* tell to verify_transmission() */
	if (attachments) 
	  options |= MAIL_HAVE_ATTACHMENTS;
#endif
#ifdef USE_PGP
	if (current_header &&
	    current_header->pgp & PGP_MESSAGE)
	  options |= MAIL_HAVE_PGP_ENCODED;
#endif

	do { /* So we can return here if check_for_multipart() fails
	      * - K E H <hurtta@dionysos.FMI.FI>    */

	  reask_verify = 0;

	  /* ask the user to confirm transmission of the message */
	  if (!batch_only) {
	  fatal_label:
	    ch = (edit_message? 'e' : '\0');
	    if (verify_transmission(filename, &form, &need_redraw,
				    already_has_text, copy_file, ch,
				    options, &dsn, sizeof copy_file) != 0) {
	      cancelled_msg = (bytes(filename) > 0);
# ifdef MIME
	      if (attachments) {
		mime_destroy (attachments);
		attachments = 0;
	      }
# endif
	      return need_redraw;
	    }
	    if (form == YES && format_form(filename) < 1) {
	      cancelled_msg = (bytes(filename) > 0);
	      return need_redraw;
	    }
            /* so we can mark the reply flag */
            me_retcode = 1;
	  }

	  if ((reply = fopen(filename,"r")) == NULL) {
	    err = errno;
	    dprint(1, (debugfile,
		       "Attempt to open file %s for reading failed with error %s (mail)\n",
		       filename, error_description(err)));
	    if (!batch_only) {
	      error1(catgets(elm_msg_cat, ElmSet, ElmCouldNotOpenReply,
			     "Could not open reply file (%s)."), error_description(err));
	    } else {
	      printf(catgets(elm_msg_cat, ElmSet, ElmCouldNotOpenReply,
			     "Could not open reply file (%s)."), error_description(err));
	      putchar('\n');
	    }
	    return need_redraw;
	  }
#ifdef MIME
	  MIME_info.encoding_top  = ENCODING_7BIT;/* Encoding for Multipart/ */
	  MIME_info.encoding_text = ENCODING_7BIT;/* Encoding for Text/plain */
	  {
	    /* Determine how this message should be MIME encoded:
	     * 7BIT, 8BIT, BINARY or QUOTED-PRINTABLE.
	     */
	    MIME_info.need_enc = needs_encoding (reply);
	    if (allow_no_hdrencoding)
		MIME_info.need_enc |= check_8bit_str (subject);
	    
	    if (MIME_info.need_enc & HAVE_BINARY) {
#ifdef USE_BINARYMIME
	      MIME_info.encoding_text =  ENCODING_BINARY;
#else
	      if (allow_no_encoding >= 2)
		/* Just send BINARY anyway */
		MIME_info.encoding_text = ENCODING_BINARY;
	      else
		/* no -BBINARYMIME option */
		MIME_info.encoding_text = ENCODING_QUOTED;
#endif
	    }
	    else if (MIME_info.need_enc & HAVE_8BIT) {
#ifdef USE_8BITMIME
	      MIME_info.encoding_text =  ENCODING_8BIT;
#else
	      if (allow_no_encoding >= 1)
		/* Just send 8BIT anyway */
		MIME_info.encoding_text = ENCODING_8BIT;
	      else
		/* no -B8BITMIME option */
		MIME_info.encoding_text = ENCODING_QUOTED;
#endif
	    }
	    /* Control characters can send with encoding 7BIT
	     * HAVE_BINARY takes care really severe stuff */
	    if ((MIME_info.need_enc & HAVE_CTRL) &&
		MIME_info.encoding_text != ENCODING_QUOTED) {
	      if (batch_only) 		
		MIME_info.encoding_text =  ENCODING_QUOTED;
	      else { 
		int ch;
		ch = want_to("Text have control characters. \
Encode text with quoted-printable? ",*def_ans_yes,elm_LINES-1,0);
		if (ch == *def_ans_yes) 
		  MIME_info.encoding_text =  ENCODING_QUOTED;
		else if (ch != *def_ans_no) {
		  edit_message = FALSE;
		  reask_verify = TRUE;
		  continue;    /* Go again to verify_transmission loop */
		}
	      }
	    }
	  }

	  (void) update_encoding(&MIME_info.encoding_top,
				 MIME_info.encoding_text);

	  /* Update Charset */
	  MIME_info.Charset = charset;
	  if (!(MIME_info.need_enc & HAVE_8BIT) &&
	      istrcmp(charset,"US-ASCII") != 0 &&
	      charset_ok(charset))
	    MIME_info.Charset="US-ASCII";
	  else if ((MIME_info.need_enc & HAVE_8BIT) &&
		   istrcmp(charset,"US-ASCII") == 0 &&
		   istrcmp(display_charset,"US-ASCII") != 0 &&
		   charset_ok(display_charset))
	    MIME_info.Charset=display_charset;
	  else if ((MIME_info.need_enc & HAVE_8BIT) &&
		   istrcmp(charset,"US-ASCII") == 0) {
	    error("Text has 8BIT data and charset=US-ASCII, using charset=UNKNOWN-8BIT instead.");
	    
	    if (sleepmsg > 0)
	      sleep(sleepmsg);
	    
	    MIME_info.Charset="UNKNOWN-8BIT";           
         }

#endif /* MIME */

	  cancelled_msg = FALSE;	/* it ain't cancelled, is it? */

#ifdef	MIME
	  MIME_info.msg_is_multipart = check_for_multipart(reply, &MIME_info);
	  if (MIME_info.msg_is_multipart < 0) { /* Error in [include ...] */
	    if (!batch_only) {
	      error ("Please fix [include ...] lines!");
	      if (sleepmsg > 0)
		sleep (sleepmsg);

	      edit_message = FALSE;
	      reask_verify = TRUE; /* Go to verify_transmission again. */
	    }
	  }

	  if (attachments != 0) {
	    /* Determine what the top level Content-Transfer-Encoding: field
	     * should be.  BINARY has the highest weight, followed by
	     * 8BIT.  NOTE: no checking to see if the mailer supports those
	     * encodings since that will already have been done in the
	     * attach_menu() routine.
	     */
	    mime_t *tmp;
	    
	    MIME_info.msg_is_multipart = TRUE;
	    
	    for (tmp = attachments; tmp != 0; tmp = tmp->next) {
	      (void) update_encoding(&MIME_info.encoding_top,tmp->encoding); 
	      if (tmp->pathname) {
		if (access(tmp->pathname,READ_ACCESS) < 0) {
		  error1("Can't access attachment: %.50s",tmp->pathname);
		  reask_verify = TRUE; /* Go to verify_transmission again. */
		  if (sleepmsg > 0)
		    sleep (sleepmsg);
		  edit_message = FALSE;
		}
	      } else {
		error("Bad attachment -- no filename!");
	      }
	    }
	  }

#endif /* MIME */
	  /* End of check_for_multipart failure loop */
	} while (!batch_only && reask_verify);
#ifdef MIME
	if (MIME_info.msg_is_multipart) {
	  (void) mime_generate_boundary (MIME_info.mime_boundary);
          add_parameter(MIME_info.type_opts_top, "boundary",
                        MIME_info.mime_boundary, 
			sizeof(MIME_info.type_opts_top),
                        FALSE);
	}
#ifdef USE_PGP
        if (pgp_status & (PGP_MESSAGE | PGP_SIGNED_MESSAGE | PGP_PUBLIC_KEY)) {
          MIME_info.type_text = MIME_TYPE_APPLICATION;
          strfcpy(MIME_info.subtype_text, "pgp",
		  sizeof MIME_info.subtype_text);
	  if (pgp_status & PGP_PUBLIC_KEY)
            add_parameter(MIME_info.type_opts_text,"format","keys-only",
                          sizeof(MIME_info.type_opts_text), FALSE);
          else {
            add_parameter(MIME_info.type_opts_text, "format", "text",
                          sizeof(MIME_info.type_opts_text), FALSE);
            /* This next bit is a non-non-standard, but exmh does this and it
             * can be very useful when parsing the message.
             */
            if (pgp_status & PGP_MESSAGE) {
              add_parameter(MIME_info.type_opts_text, "x-action",
                            (pgp_status & PGP_SIGNED_MESSAGE) ? 
			    "encryptsign" : "encrypt",
                            sizeof(MIME_info.type_opts_text),FALSE);
            }
            else
              add_parameter(MIME_info.type_opts_text, "x-action", "sign",
                            sizeof(MIME_info.type_opts_text), FALSE);
          }
        }
#endif
	/* charset parameter is valid for all subtypes of Text/ */
        if (MIME_info.type_text == MIME_TYPE_TEXT)
          add_parameter(MIME_info.type_opts_text, "charset", MIME_info.Charset,
                        sizeof(MIME_info.type_opts_text), FALSE);
#endif
         
	/** grab a copy if the user so desires... **/

	remove_hostbang(expanded_to);
	remove_hostbang(expanded_cc);
	remove_hostbang(expanded_bcc);

#ifdef USE_REMAILER
	if (remailing)
	  /* Retrieves the database of remailers and sets the To: address
	   * to the appropriate remailer.  This is done here so that a
	   * saved copy of this message will look the same as the outgoing
	   * message.
	   */
	  remailer_proc();
#endif

#ifdef MIME
	if (allow_no_hdrencoding) {
	  strfcpy(MIME_info.encoded_subject,subject,
		  sizeof(MIME_info.encoded_subject));

	  MIME_info.encoded_fullname[0] = '"';
	  strfcpy(MIME_info.encoded_fullname+1,full_username,
		  sizeof(MIME_info.encoded_fullname)-2);
	  strfcat(MIME_info.encoded_fullname,"\"",
		  sizeof MIME_info.encoded_fullname);

	  strfcpy(MIME_info.encoded_in_reply_to,in_reply_to,
		  sizeof(MIME_info.encoded_in_reply_to));

	  dprint(2,(debugfile,"Subject not encoded: %s\n",
		    MIME_info.encoded_subject));
	  dprint(2,(debugfile,"Fullname not encoded: %s\n",
		    MIME_info.encoded_fullname));
	  dprint(2,(debugfile,"In-Reply-To not encoded: %s\n",
		    MIME_info.encoded_in_reply_to));

	} else {
	  char *ptr,*next_ptr,*rptr;
	  int qcount = 0;

	  rfc1522_encode_text(MIME_info.encoded_subject,
			      sizeof(MIME_info.encoded_subject),
			      subject,0);
	  rfc1522_encode_text(MIME_info.encoded_fullname,
			      sizeof(MIME_info.encoded_fullname),
			      full_username,HDR_PHRASE);

	  for (ptr = in_reply_to, rptr = MIME_info.encoded_in_reply_to; 
	       ptr && *ptr; ptr = next_ptr) {
	    int left = ((MIME_info.encoded_in_reply_to + 
			 sizeof(MIME_info.encoded_in_reply_to)) - rptr)-1;
	    char c = 0;
	    if (left < 2)
	      break;
	    next_ptr = qstrpbrk(ptr,"<>");
	    if (next_ptr) {
	      c = *next_ptr;
	      *next_ptr = '\0';
	      if ('<' == c) 
		qcount ++;
	    }
	    dprint(12,(debugfile,
		       "Encoding in-reply-to - elem='%s', qcount=%d, c='%c'\n",
		       ptr,qcount,c));
	    if (!next_ptr || next_ptr > ptr) {
	      int len;
	      if (qcount > 0) {
		strfcpy(rptr,ptr,left);
	      } else if (0 == qcount) {
		rfc1522_encode_text(rptr,left,ptr,HDR_PHRASE);
	      } else {
		error("Error in In-Reply-To");
		*rptr = '\0';		
	      }
	      dprint(12,(debugfile,
			 "Encoding in-reply-to - encoded='%s'\n",
			 rptr));

	      len = strlen(rptr);
	      rptr += len;
	    }
	    if (next_ptr) {
	      if ('>' == c) 
		qcount --;
	      *next_ptr = c;
	      next_ptr++;
	      if (rptr < (MIME_info.encoded_in_reply_to + 
			  sizeof(MIME_info.encoded_in_reply_to)) -1) 
		*rptr++=c;
	      *rptr='\0';
	    }
	  }
	  *rptr = '\0';
	  if (qcount > 0) {
	    error("Error in In-Reply-To");
	  }

	  dprint(2,(debugfile,"Encoded subject: %s\n",
		    MIME_info.encoded_subject));
	  dprint(2,(debugfile,"Encoded fullname: %s\n",
		    MIME_info.encoded_fullname));
	  dprint(2,(debugfile,"Encoded In-Reply-To: %s\n",
		    MIME_info.encoded_in_reply_to));
	}
#endif

	if (*copy_file) /* i.e. if copy_file contains a name */
#ifdef MIME
	  save_copy(expanded_to, expanded_cc, expanded_bcc,
	       filename, copy_file, form, &MIME_info);
#else
	  save_copy(expanded_to, expanded_cc, expanded_bcc,
	       filename, copy_file, form);
#endif

	/** write all header information into whole_msg_file **/

	if((whole_msg_file=tempnam(temp_dir, "snd.")) == NULL) {
	  dprint(1, (debugfile, "couldn't make temp file nam! (mail)\n"));
	  if(batch_only) {
	    printf(catgets(elm_msg_cat, ElmSet, ElmCouldNotMakeTemp,
		"Sorry - couldn't make temp file name!"));
	    printf("\n");
	  } else if(mail_only)
	    error(catgets(elm_msg_cat, ElmSet, ElmCouldNotMakeTemp,
		"Sorry - couldn't make temp file name."));
	  else
	    set_error(catgets(elm_msg_cat, ElmSet, ElmCouldNotMakeTemp,
		"Sorry - couldn't make temp file name."));
	  return(need_redraw);
	}

	/** try to write headers to new temp file **/

	dprint(6, (debugfile, "Composition file='%s' and mail buffer='%s'\n", 
		    filename, whole_msg_file));

	dprint(2,(debugfile,"--\nTo: %s\nCc: %s\nBcc: %s\nSubject: %s\n---\n", 
		  expanded_to, expanded_cc, expanded_bcc, subject));

	if ((real_reply = 
#ifdef MIME
	   write_header_info(whole_msg_file, expanded_to,
	     expanded_cc, expanded_bcc, form == YES, FALSE, &MIME_info)
#else
	   write_header_info(whole_msg_file, expanded_to,
	     expanded_cc, expanded_bcc, form == YES, FALSE)
#endif
	     ) == NULL) {
	  error("Failed to create temp file for mailing!");
	  sleep_message();
	  edit_message = FALSE;
	  fclose(reply);
	  free(whole_msg_file);

	  if (!batch_only)
	    goto fatal_label;

	  if (!batch_only)
	    goto fatal_label;
	  return TRUE; /* need redraw */
	}

	cl_start = ftell(real_reply);
#ifdef MIME
#ifdef USE_REMAILER
	if (remailing) {
	  int ret;
	  
	  ret = remailer_copy_message_across(reply, real_reply, FALSE,
					     &MIME_info);
	  remailer_destroy_db();
	  /* Check for error to ensure that the message is not sent if
	   * there was a problem, or else the sender's anonymity could
	   * be compromised!
	   */
	  if (ret == -1) {
	    edit_message = FALSE;
	    fclose(reply);
	    fclose(real_reply);
	    free(whole_msg_file);
	    if (!batch_only)
	      goto fatal_label;
	    
	    unlink(filename);
	    return TRUE;
	  }
	}
	else
#endif
	  copy_message_across(reply, real_reply, FALSE, &MIME_info);
	
	/* clean up the allocated space */
	if (attachments) {
	  mime_destroy (attachments);
	  attachments = 0;
	}
#else
#ifdef USE_REMAILER
	if (remailing) {
	  int ret;
	  
	  ret = remailer_copy_message_across(reply, real_reply, FALSE);
	  remailer_destroy_db();
	  /* Check for error to ensure that the message is not sent if
	   * there was a problem, or else the sender's anonymity could
	   * be compromised!
	   */
	  if (ret == -1) {
	    edit_message = FALSE;
	    fclose(reply);
	    fclose(real_reply);
	    free(whole_msg_file);
	    if (!batch_only)
	      goto fatal_label;
	    
	    unlink(filename);
	    return TRUE;
	  }
	}
	else
#endif
	  copy_message_across(reply, real_reply, FALSE);
#endif
	cl_end = ftell(real_reply);	    
	fseek(real_reply, cl_offset, 0);
	fprintf(real_reply, "%d", cl_end - cl_start);
	
	/* Return to the end of the file! */
	fseek (real_reply, 0, 2);
	
	/* Append signature if not done earlier */
	
	if (!signature_done && !retransmit && copy_msg != FORM)
	  append_sig(real_reply);
		
	if (cc[0] != '\0')  				         /* copies! */
	  sprintf(expanded_to,"%s,%s", expanded_to, expanded_cc);
	
	if (bcc[0] != '\0') {
	  strfcat(expanded_to, ",", sizeof expanded_to);
	  strfcat(expanded_to, expanded_bcc, sizeof expanded_to);
	}

	mailerflags[mf_idx++] = mailer;;
	
	if (strcmp(sendmail, mailer) == 0
#ifdef SITE_HIDING
	    && ! is_a_hidden_user(username)
#endif
	    ) {
	  mailerflags[mf_idx++] = "-oi";
	  mailerflags[mf_idx++] = "-oem";
	  if (sendmail_verbose)
	    mailerflags[mf_idx++] = "-v";
	  if (metoo) 
	    mailerflags[mf_idx++] = "-om";
	} else if (strcmp(submitmail, mailer) == 0)
	  mailerflags[mf_idx++] = "-mlrnv";
	else if (strcmp(execmail, mailer) == 0) {
	  if (sendmail_verbose)
	    mailerflags[mf_idx++] = "-d";
	  if (metoo)
	    mailerflags[mf_idx++] = "-m";
	} 
	
#ifdef MIME
#ifdef USE_8BITMIME
	if (MIME_info.encoding_top == ENCODING_8BIT)
	  mailerflags[mf_idx++] = "-B8BITMIME";
#endif
#ifdef USE_BINARYMIME
	if (MIME_info.encoding_top == ENCODING_BINARY)
	  /* With -BBINARYMIME lines must terminate with \r\n
	   * Unix's \n is _NOT_ sufficient - K E H              */
	  mailerflags[mf_idx++] = "-BBINARYMIME";
#endif
#endif

#ifdef USE_DSN
	if (dsn & DSN_FULL) {
	  mailerflags[mf_idx++] = "-R";
	  mailerflags[mf_idx++] = "full";
	} else if (dsn & DSN_HDRS) {
	  mailerflags[mf_idx++] = "-R";
	  mailerflags[mf_idx++] = "hdrs";
	}
	
	if (dsn & DSN_NEVER) {
	  mailerflags[mf_idx++] = "-N";
	  mailerflags[mf_idx++] = "never";
	} else if (dsn & (DSN_SUCCESS|DSN_FAILURE|DSN_DELAY)) {	    
	  t[0] = '\0';
	  if (dsn & DSN_SUCCESS)
	    strfcat(t,"success", sizeof t);
	  if (dsn & DSN_FAILURE) {
	    if (t[0]) strfcat(t,",", sizeof t);
	    strfcat(t,"failure", sizeof t);
	  }
	  if (dsn & DSN_DELAY) {
	    if (t[0]) strfcat(t,",", sizeof t);
	    strfcat(t,"delay", sizeof t);
	  }
	  mailerflags[mf_idx++] = "-N";
	  mailerflags[mf_idx++] = t;
	}
#endif

	mailerflags[mf_idx] = NULL;
	
	if (strcmp(submitmail, mailer) == 0)
	  argv = mailerflags;
	else {
	  char **argvt = argv_from_to(expanded_to);
	  argv = join_argv(mailerflags,argvt);
	  free(argvt);
	}
		
	unlink(whole_msg_file);
	free(whole_msg_file);

	fclose(reply);

	{
	  struct run_state RS;
	  int options = SY_ENV_SHELL;
	  int ret;

	  if (!sendmail_verbose)
	    options |= SY_NOTTY;

	  fseek (real_reply, 0, 0);
#ifdef _POSIX_VERSION
	  fflush(real_reply);  /* Synzronize underlying file descriptor */
#else
	  seek(fileno(real_reply),0,0);
#endif

	  ret=start_run(&RS, options, argv, fileno(real_reply),-1);
	  if (ret) {
	    int exit_code;

	    ret = run_already_done(&RS,&exit_code);
	    if (0 == ret) {
	      error(catgets(elm_msg_cat, ElmSet, ElmSendingMail,
			    "Sending mail..."));
	      fflush(stdout);
	      ret = wait_end(&RS,&exit_code);
	    }
	    if (ret < 0) 
	      error1("%.30s fail: Signal?",argv[0]);
	    else if (ret > 0) {
	      if (RS.errno)
		error2("Failed: %.30s: %.40s",
		       argv[0],error_description(RS.errno));
	      else if (exit_code) {
		sprintf(very_long_buffer, catgets(elm_msg_cat, ElmSet, 
						  ElmMailerReturnedError,
						  "mailer returned error status %d"), 
			exit_code);
		error(very_long_buffer);
	      } else
		error(catgets(elm_msg_cat, ElmSet, ElmMailSent, "Mail sent!"));
	    } else {
	      error2("%.30s lost: %.40s",
		     argv[0],error_description(RS.errno));
	    }
	  } else {
	    if (RS.errno)
	      error2("Failed: %.30s: %.40s",
		     argv[0],error_description(RS.errno));
	    else
	      error1("Can't start %.30s",argv[0]);
	  }
	}
	if (argv != mailerflags)
	  free(argv);

	fclose(real_reply);

	/* Unlink temp file now.
	 * This is a precaution in case the message was encrypted.
	 * I.e. even though this file is readable by the owner only,
	 * encryption is supposed to hide things even from someone
	 * with root privelges. The best we can do is not let this
	 * file just hang after we're finished with it.
	 */
	(void)unlink(filename);
#ifdef USE_PGP
        pgp_status=0;
#endif
	return(need_redraw);
	
}

int mail_form(current_header, address, subj)
     struct header_rec *current_header;
     char *address, *subj;
{
	/** copy the appropriate variables to the shared space... */

	strfcpy(subject, subj, sizeof subject);
	strfcpy(to, address, sizeof to);
	strfcpy(expanded_to, address, sizeof expanded_to);
	return(mail(current_header,MAIL_ISFORM,NO));
}

int
recall_last_msg(filename, copy_msg, cancelled_msg, already_has_text)
char *filename;
int  copy_msg, *cancelled_msg, *already_has_text;
{
	char ch;
	char msg[SLEN];

	/** If filename exists and we've recently cancelled a message,
	    the ask if the user wants to use that message instead!  This
	    routine returns TRUE if the user wants to retransmit the last
	    message, FALSE otherwise...
	**/

	register int retransmit = FALSE;

	if (access(filename, EDIT_ACCESS) == 0 && *cancelled_msg) {
	  Raw(ON);
	  CleartoEOLN();
	  if (copy_msg)
	    MCsprintf(msg, catgets(elm_msg_cat, ElmSet, ElmRecallLastInstead,
		     "Recall last kept message instead? (%c/%c) "),
		     *def_ans_yes, *def_ans_no);
	  else
	    MCsprintf(msg, catgets(elm_msg_cat, ElmSet, ElmRecallLastKept,
		     "Recall last kept message? (%c/%c) "),
		     *def_ans_yes, *def_ans_no);
	  do {
	    ch = want_to(msg, '\0', elm_LINES-1, 0);
	    if (ch == *def_ans_yes) {
              retransmit++;
	      *already_has_text = TRUE;
	    } else if (ch != *def_ans_no) {
	      Write_to_screen("%c??", 1, 07);	/* BEEP */
	      if (sleepmsg > 0)
		    sleep((sleepmsg + 1) / 2);
	      ch = 0;
	    }
	  } while (ch == 0);

	  fflush(stdout);

	  *cancelled_msg = 0;
	}

	return(retransmit);
}

#ifdef USE_DSN

void dsn_menu P_((int *));
void dsn_menu(dsn)
     int * dsn;
{
  static int hdr_only = 0;
  static int failure  = 1;
  static int delay    = 1;
  static int success  = 0;
  static int DSN_off  = 0;

#define BOL 1
  static struct menu_item dsn_items[] = {
    { "Return H)eaders only    :", 'h', 3,  BOL, (char *) &hdr_only,
      sizeof hdr_only },
    { "Return DSN on F)AILURE  :", 'f', 5,  BOL, (char *) &failure,
      sizeof failure },
    { "              D)ELAY    :", 'd', 6,  BOL, (char *) &delay,
      sizeof delay },
    { "              S)UCCESS  :", 's', 7,  BOL, (char *) &success,
      sizeof success },
    { "U)se defaults (DSN off):", 'u', 12, BOL, (char *) &DSN_off,
      sizeof DSN_off }
  };

  if (0 == *dsn)
    DSN_off = 1;
  else {
    if (DSN_FULL    & *dsn) hdr_only = 0;
    if (DSN_HDRS    & *dsn) hdr_only = 1;
    if (DSN_SUCCESS & *dsn) success  = 1;
    if (DSN_FAILURE & *dsn) failure  = 1;
    if (DSN_DELAY   & *dsn) delay    = 1;
    if (DSN_NEVER   & *dsn) { success = 0; failure = 0; delay = 0; }
  }
  
  generic_menu(dsn_items,sizeof dsn_items / sizeof (struct menu_item),
	       "DSN (Delivery Status Notification) Configuration",
	       "DSN: ");

  if (DSN_off) *dsn = 0;
  else if (!success && !failure && !delay) *dsn = DSN_NEVER;
  else {
    int val = 0;
    if (hdr_only) val |= DSN_HDRS;
    else          val |= DSN_FULL;
    if (success)  val |= DSN_SUCCESS;
    if (failure)  val |= DSN_FAILURE;
    if (delay)    val |= DSN_DELAY;

    *dsn = val;
  }
  
}
#endif


/*
 * verify_transmission() - Ask the user to confirm transmission of the
 * message.  Returns 0 to send it, -1 to forget it.
 */
static int
verify_transmission(filename, form_p, need_redraw_p,
		    already_has_text, copy_file, force_cmd, options,
		    dsn, copy_file_size)
     char *filename;	    /* pathname to mail mssg composition file        */
     int  *form_p;	    /* pointer to form message state	             */
     int *need_redraw_p;    /* pointer to flag indicating screen stepped on  */
     int already_has_text;  /* indicates there is already text in the mssg   */
     char *copy_file;	    /* pointer to buffer holding copy file name	     */
     int force_cmd;	    /* command to do, '\0' to prompt user for cmd    */
     int options;
     int *dsn;
     int copy_file_size;
{
    char *prompt_mssg;		/* message to display prompting for cmd	*/
    char prompt_menu[SLEN],	/* menu of available commands		*/
         prompt_menu2[SLEN];
    int bad_cmd;		/* set TRUE to bitch about user's entry	*/
    int did_prompt;		/* TRUE if cmd prompted for and entered	*/
    int prev_form;		/* "*form_p" value last time thru loop	*/
    int cmd;			/* command to perform			*/
    char lbuf[VERY_LONG_STRING];
    int x_coord, y_coord;
    int replying          = 0 != (options & MAIL_REPLYING);
#ifdef MIME
    int have_attachments  = 0 != (options & MAIL_HAVE_ATTACHMENTS);
#endif
#ifdef USE_PGP
    int was_pgp_encoded   = 0 != (options & MAIL_HAVE_PGP_ENCODED);
#endif



    prev_form = *form_p + 1;	/* force build of prompt strings	*/
    bad_cmd = FALSE;		/* nothing to complain about yet	*/

    for (;;) {

	/* build up prompt and menu strings */
	if (prev_form == *form_p) {
	    ; /* not changed - no need to rebuild the strings */
	} else if (user_level == 0) {
	    prompt_mssg = catgets(elm_msg_cat, ElmSet, ElmVfyPromptPleaseChoose,
		"Please choose one of the following options by parenthesized letter: s");
	    strfcpy(prompt_menu, 
		    catgets(elm_msg_cat, ElmSet, ElmVfyMenuUser0,
			    "e)dit message, edit h)eaders, s)end it, or f)orget it."),
		    sizeof prompt_menu);
            prompt_menu2[0] = '\0';

	    /* In some conditions add these also to menu in user_level == 0 */
#ifdef MIME
	    if (have_attachments)
	      strfcat(prompt_menu2, "a)ttachments", sizeof prompt_menu2);
#ifdef USE_DSN
	    if (dsn && *dsn != 0) {
	      if (prompt_menu2[0]) strfcat(prompt_menu2,", ", 
					   sizeof prompt_menu2);
	      strfcat(prompt_menu2, "D)SN", sizeof prompt_menu2);
	    }
#endif
#endif
#ifdef USE_PGP
	    if (replying && was_pgp_encoded) {
	      if (prompt_menu2[0]) strfcat(prompt_menu2,", ",
					   sizeof prompt_menu2);
	      strfcat(prompt_menu2, "p)gp", sizeof prompt_menu2);
	    }
#endif
	} else {
	    prompt_mssg = catgets(elm_msg_cat, ElmSet, ElmVfyPromptAndNow,
		"And now: s");
	    switch (*form_p) {
	    case PREFORMATTED:
		prompt_menu[0] = '\0';
		break;
	    case YES:
		strfcpy(prompt_menu, catgets(elm_msg_cat, ElmSet,
					     ElmVfyMenuEditForm, 
					     "e)dit form, "),
			sizeof prompt_menu);
		break;
	    case MAYBE:
		strfcpy(prompt_menu, catgets(elm_msg_cat, ElmSet,
					     ElmVfyMenuEditMake, 
					     "e)dit msg, m)ake form, "),
			sizeof prompt_menu);
		break;
	    default:
		strfcpy(prompt_menu, catgets(elm_msg_cat, ElmSet,
					     ElmVfyMenuEditMsg, 
					     "e)dit message, "),
			sizeof prompt_menu);
		break;
	    }
	    strfcat(prompt_menu, catgets(elm_msg_cat, ElmSet, ElmVfyMenuVfyCpy,
					"h)eaders, c)opy, "),
		    sizeof prompt_menu);
#ifdef ISPELL
	    strfcat(prompt_menu, catgets(elm_msg_cat, ElmSet, ElmVfyMenuIspell,
					 "i)spell, "),
		    sizeof prompt_menu);
#endif
#ifdef ALLOW_SUBSHELL
	    strfcat(prompt_menu, catgets(elm_msg_cat, ElmSet, ElmVfyMenuShell,
					 "!)shell, "),
		    sizeof prompt_menu);
#endif
            /* The previous line was getting too full... */
            prompt_menu2[0] = '\0';
#ifdef MIME
	    strfcat(prompt_menu2, "a)ttachments", sizeof prompt_menu2);
#ifdef USE_DSN
	    if (dsn) strfcat(prompt_menu2, ", D)SN", sizeof prompt_menu2);
#endif
#endif
#ifdef USE_PGP
	    if (prompt_menu2[0]) strfcat(prompt_menu2,", ", 
					 sizeof prompt_menu2);
            strfcat(prompt_menu2, "p)gp", sizeof prompt_menu2);
#endif
#ifdef USE_REMAILER
	    if (prompt_menu2[0]) strfcat(prompt_menu2,", ", 
					 sizeof prompt_menu2);
	    strfcat(prompt_menu2, "r)emailer", sizeof prompt_menu2);
#endif
	    strfcat(prompt_menu, catgets(elm_msg_cat, ElmSet, ElmVfyMenuSndFgt,
					 "s)end, or f)orget"),
		    sizeof prompt_menu);
	}



	prev_form = *form_p;

	/* complain if last entry was bad */
	if (bad_cmd) {
	    Write_to_screen("%c??", 1, 07);
	    if (sleepmsg > 0)
		sleep((sleepmsg + 1) / 2);
	    bad_cmd = FALSE;
	}

	/* if we don't have a cmd, display prompt and get response from user */
	if (force_cmd != '\0') {
	    cmd = tolower(force_cmd);
	    force_cmd = '\0';
	    did_prompt = FALSE;
	} else {
	redraw:
	    ClearLine(elm_LINES-3);
	    PutLine0(elm_LINES-3, 0, prompt_mssg);
	    GetXYLocation(&x_coord, &y_coord);
	    y_coord--; /* backspace over default answer */
#ifdef USE_PGP
	    if (replying && was_pgp_encoded &&
		! (pgp_status & PGP_MESSAGE)) 
	      PutLine0(elm_LINES-3, y_coord, "p");
#endif
	    ClearLine(elm_LINES-2);
	    Centerline(elm_LINES-2, prompt_menu);
            ClearLine(elm_LINES-1);
            Centerline(elm_LINES-1, prompt_menu2);
	    fflush(stdout);
	    MoveCursor(x_coord, y_coord);
	    cmd = ReadCh(REDRAW_MARK);
	    if (cmd == REDRAW_MARK)
	      goto redraw;
	    if (cmd == EOF)
	      leave(0);
	    cmd = tolower(cmd);
	    did_prompt = TRUE;
	}

	/* handle command */
	switch (cmd) {

#ifdef  MIME
	case 'a':
	    if (did_prompt)
	        Write_to_screen("Attachments", 0);
	  attachments = (mime_t *) attach_menu (attachments, FALSE);
	  break;
#ifdef USE_DSN
        case 'd':
	    if (did_prompt)
	        Write_to_screen("Dsn", 0);
	  if (dsn) dsn_menu(dsn);
	  break;
#endif
#endif
	case '\n':
	case '\r':
#ifdef USE_PGP
	      if (replying && was_pgp_encoded &&
		  ! (pgp_status & PGP_MESSAGE)) 
		goto select_pgp;
#endif
		/* fall thru */
	case 's':
	    if (did_prompt)
	        Write_to_screen("Send", 0);
#ifdef USE_PGP
	      if (replying && was_pgp_encoded &&
		  ! (pgp_status & PGP_MESSAGE)) {
              ClearLine(elm_LINES-2);
	      PutLine0(elm_LINES-2, 0, "The recv'd message was PGP encoded, are you sure? ");
	      for (;;) {
		cmd = ReadCh(0);
		if (cmd == 'y' || cmd == 'Y') {
		  Write_to_screen("Yes", 0);
		  return(0);
		}
		if (cmd == 'n' || cmd == 'N')
		  break;
	      }
	      break;
	    }
#endif /* USE_PGP */
	    return 0;
	    /*NOTREACHED*/
#ifdef USE_PGP
	case ctrl('F'):
	  pgp_void_passphrase();
	  error("Passphrase forgotten!");
	  break;
#endif
       case 'f': 
	    if (did_prompt)
		Write_to_screen("Forget", 0);
	    if (bytes(filename) <= 0) {
		; /* forget about empty files */
	    } else if (mail_only) {
#ifdef MIME
	      FILE * fd;
	      mime_send_t MIME_info;
	      
	      MIME_info.encoding_top  = ENCODING_7BIT;
	      if ((fd = fopen(filename,"r")) != NULL) {
		MIME_info.msg_is_multipart = 
		  check_for_multipart(fd,&MIME_info);
		MIME_info.need_enc = needs_encoding (fd);
		if (allow_no_hdrencoding)
			MIME_info.need_enc |= check_8bit_str (subject);
		fclose(fd);
	      } else {
		MIME_info.msg_is_multipart = 0;
		MIME_info.need_enc = 0;
	      }
	      MIME_info.encoding_text =  ENCODING_7BIT;
	      MIME_info.Charset       =  charset;
	      
	      if (MIME_info.need_enc & HAVE_BINARY) 
		MIME_info.encoding_text =  ENCODING_BINARY;
	      else  if (MIME_info.need_enc & HAVE_8BIT) 
		MIME_info.encoding_text =  ENCODING_8BIT;
	      
	      update_encoding(&MIME_info.encoding_top,MIME_info.encoding_text);
	      
	      if (MIME_info.msg_is_multipart)
		(void) mime_generate_boundary (MIME_info.mime_boundary);
	      
	      sprintf(lbuf, "%s/%s", home, dead_letter);
	      (void) append_copy_to_file(to, cc, bcc, lbuf, filename,
					 *form_p, &MIME_info);
#else
	      sprintf(lbuf, "%s/%s", home, dead_letter);
	      (void) append_copy_to_file(to, cc, bcc, lbuf, filename,
					 *form_p);
#endif
	    } else if (user_level > 0) {
		set_error(catgets(elm_msg_cat, ElmSet, ElmVfyMessageKept,
		    "Message kept.  Can be restored at next f)orward, m)ail or r)eply."));
	    }
#ifdef USE_PGP
            pgp_status = 0; /* make sure to reset! */
#endif
	    return -1;
	    /*NOTREACHED*/

	case 'c':
	    if (did_prompt)
		Write_to_screen("Copy file", 0);
	    if (name_copy_file(copy_file, copy_file_size) != 0)
		*need_redraw_p = TRUE;
	    break;

	case 'e':
	    if (did_prompt)
		Write_to_screen("Edit", 0);
	    if (*form_p == PREFORMATTED) {
		bad_cmd = TRUE;
	    } else {
		if (*form_p == YES)
		    *form_p = MAYBE;
		*need_redraw_p = TRUE;
		if (edit_the_message(filename, already_has_text) != 0)
		    return -1;
	    }
	    break;

	case 'h':
	    if (did_prompt)
		Write_to_screen("Headers", 0);
	    (void) edit_headers();
	    *need_redraw_p = TRUE;
	    break;

	case 'm':
	    if (*form_p != MAYBE) {
		bad_cmd = TRUE;
	    } else {
		switch (check_form_file(filename)) {
		case -1:
		    /* couldn't open file??? */
		    return -1;
		case 0:
		    Write_to_screen(catgets(elm_msg_cat, ElmSet,
			ElmVfyNoFieldsInForm, "No fields in form!\007"), 0);
		    if (sleepmsg > 0)
			sleep(sleepmsg);
		    break;
		default:
		    /* looks like a good form */
		    *form_p = YES;
		    break;
		}
	    }
	    break;

#ifdef ISPELL
	case 'i':
	    if (did_prompt)
		Write_to_screen("Ispell", 0);
	    if (*form_p == PREFORMATTED) {
		bad_cmd = TRUE;
	    } else {
		if (*form_p == YES)
		    *form_p = MAYBE;
		sprintf(lbuf, "%s %s %s",
		    ISPELL_PATH, ISPELL_OPTIONS, filename);
		system_call(lbuf, SY_ENAB_SIGHUP);
		*need_redraw_p = TRUE;
#ifdef ultrix
		/* I'm told this is required to work around some sort of bug */
		force_raw();
#endif
	    }
	    break;
#endif

#ifdef ALLOW_SUBSHELL
	case '!':
	    if (subshell() != 0) {
		ClearScreen();
		*need_redraw_p = TRUE;
	    }
	    break;
#endif
#ifdef USE_PGP
        case 'p':
	select_pgp:
	    if (did_prompt)
	        Write_to_screen("Pgp", 0);
	    if (!pgp_status) {
	      pgp_status = pgp_menu (filename);
	      *need_redraw_p = TRUE;
	    }
            else
	      error ("This message is already encrypted and/or signed!");
            break;
#endif
#ifdef USE_REMAILER
	case 'r':
		remailer_menu ();
		break;
#endif
	default:
	    bad_cmd = TRUE;
	    break;

	}

    }

}

FILE *
#ifdef MIME
write_header_info(filename, long_to, long_cc, long_bcc, form, copy, mime_info)
     mime_send_t *mime_info;
#else
write_header_info(filename, long_to, long_cc, long_bcc, form, copy)
#endif
     char *filename, *long_to, *long_cc, *long_bcc;
     int   form, copy;
{

	/** Try to open filedesc as the specified filename.  If we can,
	    then write all the headers into the file.  The routine returns
	    'filedesc' if it succeeded, NULL otherwise.  Added the ability
	    to have backquoted stuff in the users .elmheaders file!
	    If copy is TRUE, then treat this as the saved copy of outbound
	    mail.
	**/

	char opentype[3];
	time_t time(), thetime;
	char *ctime();
	FILE *filedesc = NULL;		/* our friendly file descriptor  */
        char to_buf[VERY_LONG_STRING];
	int err;
	char fullname_buf[STRING], *qfullname = fullname_buf; 
#ifdef SITE_HIDING
	char  buffer[SLEN];
	int   is_hidden_user;		/* someone we should know about?  */
#endif
#ifdef MMDF
	int   is_submit_mailer;		/* using submit means change From: */
#endif /* MMDF */
#ifndef DONT_ADD_FROM
	char from_buf[3*40+10];
#endif
      	char  *get_arpa_date();

	sprintf(fullname_buf,"\"%.*s\"",sizeof(fullname_buf)-4,full_username);
	save_file_stats(filename);
	
	if (copy) /* Go end of (perhaps existing) file */
	  filedesc = open_end_update(filename);
	else /* Create _new_ temporary file */
	  filedesc = safeopen_rdwr(filename);

	if (filedesc == NULL) {
	  err = errno;
	  dprint(1, (debugfile,
	    "Attempt to open file %s for writing failed! (write_header_info)\n",
	     filename));
	  dprint(1, (debugfile, "** %s **\n\n", error_description(err)));
	  error2(catgets(elm_msg_cat, ElmSet, ElmErrorTryingToWrite,
		"Error %s encountered trying to write to %s."), 
		error_description(err), filename);
	  if (sleepmsg > 0)
		sleep(sleepmsg);
	  return(NULL);		/* couldn't open it!! */
	}

	restore_file_stats(filename);

	if(copy) {	/* Add top line that mailer would add */
#ifdef MMDF
	  fprintf(filedesc, MSG_SEPARATOR);
#endif /* MMDF */
	  thetime = time((long *) 0);
	  fprintf(filedesc,"From %s %s", username, ctime(&thetime));
#ifdef MMDF
	} else if (strcmp(submitmail,mailer) == 0) {
	  sprintf(to_buf, "%s %s %s", long_to, long_cc, long_bcc);
	  do_mmdf_addresses(filedesc, strip_parens(strip_commas(to_buf)));
#endif /* MMDF */
	}

#ifdef SITE_HIDING
	if ( !copy && (is_hidden_user = is_a_hidden_user(username))) {
	  /** this is the interesting part of this trick... **/
	  sprintf(buffer, "From %s!%s %s\n",  HIDDEN_SITE_NAME,
		  username, get_ctime_date());
	  fprintf(filedesc, "%s", buffer);
	  dprint(1,(debugfile, "\nadded: %s", buffer));
	  /** so is this perverted or what? **/
	}
#endif


	/** Subject moved to top of headers for mail because the
	    pure System V.3 mailer, in its infinite wisdom, now
	    assumes that anything the user sends is part of the 
	    message body unless either:
		1. the "-s" flag is used (although it doesn't seem
		   to be supported on all implementations?? )
		2. the first line is "Subject:".  If so, then it'll
		   read until a blank line and assume all are meant
		   to be headers.
	    So the gory solution here is to move the Subject: line
	    up to the top.  I assume it won't break anyone elses program
	    or anything anyway (besides, RFC-822 specifies that the *order*
	    of headers is irrelevant).  Gahhhhh....
	**/

#ifdef MIME
	if (!form) {
	  /* Our standard violations */
	  char OSV[100];
	  OSV[0] = '\0';

	  fprintf(filedesc, "Subject: %s",mime_info ->encoded_subject);
	  print_EOLN(filedesc,mime_info->encoding_top);
	  qfullname = mime_info->encoded_fullname;

#ifndef DONT_ADD_FROM
	  /* Else From can still be Mime Par 2 encoded */
	  if (allow_no_hdrencoding && 
	      (check_8bit_str (mime_info ->encoded_subject) ||
	       is_rfc1522     (mime_info ->encoded_subject) ||
	       check_8bit_str (mime_info ->encoded_in_reply_to) ||
	       is_rfc1522     (mime_info ->encoded_in_reply_to) ||
	       check_8bit_str (qfullname) ||
	       is_rfc1522     (qfullname)))
	    add_parameter(OSV,"no-hdr-encoding","1",sizeof(OSV),TRUE);
#endif

	  if (check_8bit_str (mime_info ->encoded_subject) ||
	      check_8bit_str (qfullname) ||
	      check_8bit_str (mime_info->encoded_in_reply_to))
	    add_parameter(OSV,"hdr-charset",
			  mime_info ->Charset,sizeof(OSV),FALSE);

	  if (strlen(mime_info->encoded_in_reply_to)) {
	    fprintf(filedesc, "In-Reply-To: %s", 
		    mime_info->encoded_in_reply_to);
	    print_EOLN(filedesc,mime_info->encoding_top);
	  }

	  if (OSV[0]) {
	    fprintf(filedesc, "X-ELM-OSV: (Our standard violations) %s",OSV);
	    print_EOLN(filedesc,mime_info->encoding_top);
	  }

	} else 
#endif
	  {
	    fprintf(filedesc, "Subject: %s\n", subject);
	    qfullname = fullname_buf;	    
	    
	    if (strlen(in_reply_to) > 0)
	      fprintf(filedesc, "In-Reply-To: %s\n", in_reply_to);	
	    fprintf(filedesc, 
		    "X-ELM-OSV: (Our standard violations) no-mime=1; no-hdr-encoding=1\n");
	  }

	fprintf(filedesc, "To: %s\n", format_long(long_to, strlen("To:")));

	fprintf(filedesc,"Date: %s\n", get_arpa_date());

#ifndef DONT_ADD_FROM
#ifdef MMDF
	is_submit_mailer = (strcmp(submitmail,mailer) == 0);
#endif /* MMDF */
/*
 *	quote full user name in case it contains specials
 */
# ifdef SITE_HIDING
#    ifdef MMDF
	if (is_submit_mailer)
	  sprintf(from_buf,"%.40s",username);
	else
#    endif /* MMDF */
	if (is_hidden_user)
	  sprintf(from_buf,"%.40s!%.40s!%.40s",
		  hostname, HIDDEN_SITE_NAME, username);
	else
	  sprintf(from_buf,"%.40s!%.40s",
		  hostname, username);
# else
#  ifdef  INTERNET
#   ifdef  USE_DOMAIN
#    ifdef MMDF
	if (is_submit_mailer)
	  sprintf(from_buf,"%.40s", username);
	else
#    endif /* MMDF */
	  sprintf(from_buf,"%.40s@%.40s",  
		username, hostfullname);
#   else
#    ifdef MMDF
	if (is_submit_mailer)
	  sprintf(from_buf,"%.40s", username);
	else
#    endif /* MMDF */
	sprintf(from_buf,"%.40s@%.40s",
		username, hostname);
#   endif
#  else
#    ifdef MMDF
	if (is_submit_mailer)
	  sprintf(from_buf,"%.40s", username);
	else
#    endif /* MMDF */
	  sprintf(from_buf,"%.40s!%.40s", 
		  hostname, username);
#  endif
# endif
	fprintf(filedesc,"From: %s <%s>",qfullname,from_buf);
#ifdef MIME
	print_EOLN(filedesc,mime_info->encoding_top);
#else
	fputc('\n',filedesc);
#endif
#endif

	if (cc[0] != '\0')
	    fprintf(filedesc, "Cc: %s\n", format_long(long_cc, strlen("Cc: ")));

	if (copy && (bcc[0] != '\0'))
	    fprintf(filedesc, "Bcc: %s\n", format_long(long_bcc, strlen("Bcc: ")));

	if (strlen(action) > 0)
	    fprintf(filedesc, "Action: %s\n", action);
	
	if (strlen(priority) > 0)
	    fprintf(filedesc, "Priority: %s\n", priority);

	if (strlen(precedence) > 0)
	    fprintf(filedesc, "Precedence: %s\n", precedence);
	
	if (strlen(expires) > 0)
	    fprintf(filedesc, "Expires: %s\n", expires);
	
	if (strlen(expanded_reply_to) > 0)
	    fprintf(filedesc, "Reply-To: %s\n", expanded_reply_to);

	if (strlen(user_defined_header) > 0)
	    fprintf(filedesc, "%s\n", user_defined_header);

	add_mailheaders(filedesc);

#ifndef NO_XHEADER
	fprintf(filedesc, "X-Mailer: ELM [version %s]\n", version_buff);
#endif /* !NO_XHEADER */

	if (form) 
	  fprintf(filedesc, "Content-Type: mailform\n");
	else {
#ifdef MIME
#ifdef USE_REMAILER
	  if (!remailing)
	    /* Don't write the MIME header if we are remailing.  It will be
	     * taken care of by the remailing code.
	     */
#endif
	    mime_write_header (filedesc, mime_info, 1);
#else
	  fprintf(filedesc, "Content-Type: text\n");
#endif /* MIME */
	}
	fprintf(filedesc, "Content-Length: ");
	cl_offset = ftell(filedesc);
	fprintf(filedesc, "          "); /* Print Blanks as Placeholders */
#ifdef MIME
	print_EOLN(filedesc,mime_info->encoding_top);
 	if (copy) {
	  fprintf(filedesc, "Status: RO");
	  print_EOLN(filedesc,mime_info->encoding_top);
	}
	print_EOLN(filedesc,mime_info->encoding_top);
#else
	fputc('\n',filedesc);
	if (copy) 
	  fprintf(filedesc, "Status: RO\n");
	fputc('\n',filedesc);
#endif
	return(filedesc);
}

#ifdef MIME
copy_message_across(source, dest, copy, mime_info)
mime_send_t *mime_info;
#else
copy_message_across(source, dest, copy)
#endif
FILE *source, *dest;
int copy;
{
	/** Copy the message in the file pointed to by source to the
	    file pointed to by dest.
	    If copy is TRUE, treat as a saved copy of outbound mail. **/

	int  crypted = FALSE;			/* are we encrypting?  */
	int  encoded_lines = 0;			/* # lines encoded     */
	int  line_len;
	char buffer[SLEN];			/* file reading buffer */
#ifdef MIME
	int	text_lines = 0;
	int	at_boundary = FALSE;
#endif /* MIME */

	/* Reserve one byte for conversion LF -> CR LF in
	 * case of binary mime transmission:
	 */
	while (line_len = mail_gets(buffer, SLEN-1, source)) {
	  if (buffer[0] == '[') {
	    if (strncmp(buffer, START_ENCODE, strlen(START_ENCODE))==0) {
#ifdef MIME
	      if (mime_info->msg_is_multipart) {
		if (!at_boundary) {
		  print_EOLN(dest,mime_info->encoding_top);
		  fprintf(dest,"--%s",mime_info->mime_boundary);
		  print_EOLN(dest,mime_info->encoding_top);
		}
		mime_info->type_text    =  MIME_TYPE_APPLICATION;
		strfcpy(mime_info->subtype_text,"X-ELM-encode",
			sizeof mime_info->subtype_text);
		
		mime_write_header (dest, mime_info, 0);
		print_EOLN(dest,mime_info->encoding_top);
		at_boundary = FALSE;
	      }
#endif
	      crypted = TRUE;
	    }
	    else if (strncmp(buffer, END_ENCODE, strlen(END_ENCODE))==0)
	      crypted = FALSE;
	    else if ((strncmp(buffer, DONT_SAVE, strlen(DONT_SAVE)) == 0)
	          || (strncmp(buffer, DONT_SAVE2, strlen(DONT_SAVE2)) == 0)) {
	      if(copy) break;  /* saved copy doesn't want anything after this */
	      else continue;   /* next line? */
#ifdef MIME
	    } else if (strncmp(buffer, MIME_INCLUDE, strlen(MIME_INCLUDE))==0) {
	      text_lines = 0;
	      if (!at_boundary) {
		print_EOLN(dest,mime_info->encoding_top);
		fprintf(dest, "--%s", mime_info->mime_boundary);
		print_EOLN(dest,mime_info->encoding_top);
	      }
	      Include_Part(dest, buffer, FALSE, mime_info, copy);
	      print_EOLN(dest,mime_info->encoding_top);
	      fprintf(dest, "--%s", mime_info->mime_boundary);
	      print_EOLN(dest,mime_info->encoding_top);
	      at_boundary = TRUE;
	      continue;
#endif /* MIME */
	    }

	    if (crypted) {
	      if (! gotten_key++)
	        getkey(ON);
	      else if (! encoded_lines)
	        get_key_no_prompt();		/* reinitialize.. */
	      if (0 != strncmp(buffer, START_ENCODE, strlen(START_ENCODE))) {
	        encode(buffer);
	        encoded_lines++;
	      }
	    }
	  }
	  else if (crypted) {
	    if (batch_only) {
	      printf(catgets(elm_msg_cat, ElmSet, ElmNoEncryptInBatch,
		"Sorry. Cannot send encrypted mail in \"batch mode\".\n"));
	      leave(0);
	    } else if (! gotten_key++)
	      getkey(ON);
	    else if (! encoded_lines)
	      get_key_no_prompt();		/* reinitialize.. */
	    if (0 != strncmp(buffer, START_ENCODE, strlen(START_ENCODE))) {
	      encode(buffer);
	      encoded_lines++;
	    }
#ifdef MIME
	  } else {
	    if (text_lines == 0) {
	      char tmp[STRING];
	      if (mime_info->msg_is_multipart) {
		if (!at_boundary) {
		  print_EOLN(dest,mime_info->encoding_top);
	 	  fprintf(dest,"--%s",mime_info->mime_boundary);
		  print_EOLN(dest,mime_info->encoding_top);
		}

		mime_write_header (dest, mime_info, 0);
		print_EOLN(dest,mime_info->encoding_top);
		at_boundary = FALSE;
	      }
	    }	
	    text_lines++;
#endif /* MIME */
          }

#ifdef MIME
	  if (mime_info->encoding_top == ENCODING_BINARY) {
	    /* It is better perhaps use canonical eol (CRLF) when mail have
	     * content transfer encoding BINARY somewhere (see notes about 
	     * BINARYMIME)
	     */

	    if (buffer[line_len-1] == '\n') {
	      int add = 1;
	      if (line_len >1 && buffer[line_len-2] == '\r')
		add = 0;
	      if (add) {
		buffer[line_len-1] = '\r';
		buffer[line_len] = '\n';
		line_len++;
	      }	      
	    }
	  }

	  /* Do QUOTED-PRINTABLE conversion if necessary... */
	  if (mime_info->encoding_text == ENCODING_QUOTED)
	    line_quoted_printable_encode(buffer,dest,copy,line_len,TRUE,
					 mime_info);
	  else
#endif
#ifndef DONT_ESCAPE_MESSAGES
	  if (copy && (strncmp(buffer, "From ", 5) == 0)) {
	    /* Add in the > to a From on our copy */
	    fprintf(dest, ">");
	    if (fwrite(buffer, 1, line_len, dest) != line_len) {
		MoveCursor(elm_LINES, 0);
		Raw(OFF);
		Write_to_screen(catgets(elm_msg_cat, ElmSet, ElmWriteFailedCopyAcross,
			"\nWrite failed in copy_message_across\n"), 0);
		emergency_exit();
	    }
	  }
#ifdef NEED_LONE_PERIOD_ESCAPE
	  else if (!copy && strcmp(buffer, ".\n") == 0)
	    /* Because some mail transport agents take a lone period to
	     * mean EOF, we add a blank space on outbound message.
	     */
	    fputs(". \n", dest);
#endif /* NEED_LONE_PERIOD_ESCAPE */
  	  else
#endif /* DONT_ESCAPE_MESSAGES */
  	    if (fwrite(buffer, 1, line_len, dest) != line_len) {
		MoveCursor(elm_LINES, 0);
		Raw(OFF);
		Write_to_screen(catgets(elm_msg_cat, ElmSet, ElmWriteFailedCopyAcross,
			"\nWrite failed in copy_message_across\n"), 0);
		emergency_exit();
	    }
	} 
#ifdef MIME
	if (mime_info->msg_is_multipart) {
	  /* now add any attachments (NEW STYLE) */
	  if (attachments)
	    attach_generate_message (attachments, dest, copy, mime_info);

	  print_EOLN(dest,mime_info->encoding_top);
	  fprintf(dest,"--%s--", mime_info->mime_boundary);
	  print_EOLN(dest,mime_info->encoding_top);
	}
#endif /* MIME */
#ifdef MMDF
	if (copy) fputs(MSG_SEPARATOR, dest);
#else
	if (copy) fputs("\n", dest);	/* ensure a blank line at the end */
#endif /* MMDF */
}

int
append_sig(file)
FILE *file;
{
	/* Append the correct signature file to file.  Return TRUE if
           we append anything.  */

        /* Look at the to and cc list to determine which one to use */

	/* We could check the bcc list too, but we don't want people to
           know about bcc, even indirectly */

	/* Some people claim that  user@anything.same_domain should be 
	   considered local.  Since it's not the same machine, better be 
           safe and use the remote sig (presumably it has more complete
           information).  You can't necessarily finger someone in the
           same domain. */

	  if (!batch_only && (local_signature[0] || remote_signature[0])) {

            char filename2[SLEN];
	    char *sig;

  	    if (index(expanded_to, '!') || index(expanded_cc,'!'))
              sig = remote_signature;		/* ! always means remote */
            else {
	      /* check each @ for @thissite.domain */
	      /* if any one is different than this, then use remote sig */
	      int len;
	      char *ptr;
	      char sitename[SLEN];
	      sprintf(sitename,"@%s",hostfullname);
	      len = strlen(sitename);
              sig = local_signature;
              for (ptr = index(expanded_to,'@'); ptr;  /* check To: list */
	          ptr = index(ptr+1,'@')) {
		if (strincmp(ptr,sitename,len) != 0
		    || (*(ptr+len) != ',' && *(ptr+len) != 0
		    && *(ptr+len) != ' ')) {
	          sig = remote_signature;
                  break;
                }
              }
              if (sig == local_signature)		   /* still local? */ 
                for (ptr = index(expanded_cc,'@'); ptr;   /* check Cc: */
		    ptr = index(ptr+1,'@')) {
		  if (strincmp(ptr,sitename,len) != 0
		      || (*(ptr+len) != ',' && *(ptr+len) != 0 
		      && *(ptr+len) != ' ')) {
	            sig = remote_signature;
                    break;
                  }
                }
            }

            if (sig[0]) {  /* if there is a signature file */
	      if (sig[0] != '/')
	        sprintf(filename2, "%s/%s", home, sig);
	      else
	        strfcpy(filename2, sig, sizeof filename2);
	      /* append the file - with a news 2.11 compatible */
	      /* seperator if "sig_dashes" is enabled */
	      (void) append(file, filename2, (sig_dashes ? "\n-- \n" : NULL));

              return TRUE;
            }
          }

return FALSE;

}

#ifdef MIME
static int check_8bit_str (str)
     char *str;
{
  char *s;

  for (s = str;	*s; s++)
    if (*s & 0x80)
      return HAVE_8BIT;
  return 0;
}
#endif
