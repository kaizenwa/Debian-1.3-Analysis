#include "headers.h"
#include "melib.h"

#ifdef USE_PGP
#include <sys/time.h>
#include <errno.h>
#ifdef BSD
#  include <sys/wait.h>
#endif

extern int errno;

extern int pgp_keeppassfor; /* 5 minutes */
extern char pgp_passphrase[STRING];
extern int pgp_expires;

void pgp_void_passphrase ()
{
  int i;

  for (i = 0 ; i < STRING ; i++)
    pgp_passphrase[i] = '\0';
  pgp_expires = 0;
  return;
}

static int QueryExpirePassphrase P_((void));

static int QueryExpirePassphrase()
{
  struct timeval now;

  /* negative implies never expire */
  if (pgp_keeppassfor < 0)
    return(0);
  gettimeofday(&now, 0);
  if (now.tv_sec < pgp_expires) {
    pgp_expires = now.tv_sec + pgp_keeppassfor;
    return(0);
  }
  pgp_void_passphrase ();
  return(1);
}

static int GetPassphrase P_((void));

static int GetPassphrase ()
{
  struct timeval now;
  int status;

redraw:
  PutLine0(elm_LINES-2, 0, "Please enter your PGP passphrase: ");
  CleartoEOS();
  
  status = optionally_enter(pgp_passphrase, elm_LINES-2, 34, 
			    OE_PASSWD|OE_REDRAW_MARK, 
			    sizeof pgp_passphrase);
  if (REDRAW_MARK == status)
    goto redraw;

  if (status != 0)
    return(0);
  gettimeofday(&now, 0);
  if (pgp_keeppassfor > 0)
    pgp_expires = now.tv_sec + pgp_keeppassfor;
  return(pgp_passphrase[0] != '\0');
}

int pgp_goodPassphrase()
{
  if (pgp_passphrase[0] == '\0' || QueryExpirePassphrase())
    return(GetPassphrase());
  else
    return(1);
}

/* opens up a PGP process as a child and returns its stdin and stdout */
int pgp_decrypt_init (fpin, fpout, opts)
     FILE **fpin, **fpout;
     int opts; /* PGP_MESSAGE or PGP_SIGNED_MESSAGE */
{
  int pgp_child_in[2];
  int pgp_child_out[2];
  int passpipe[2];
  int usepass=FALSE;
  int fork_ret;
  char cmd[STRING];

  dprint (2, (debugfile, "pgp_descrypt_init() called with opts=%d\n", opts));

  if (pipe(pgp_child_in) == -1)
    return(-1);
  if (pipe(pgp_child_out) == -1)
    return(-1);
  
  if ((opts & PGP_MESSAGE) && pgp_keeppass) {
    if (pipe(passpipe) == -1) {
      close(pgp_child_in[0]);
      close(pgp_child_out[0]);
      close(pgp_child_in[1]);
      close(pgp_child_out[1]);
      return(-1);
    }
    usepass = TRUE;
  }
  dprint (3, (debugfile, "usepass = %d.\n", usepass));

  Raw(OFF);
  ClearScreen();

  /* Tell the user why they are waiting */
  if (opts & PGP_MESSAGE) {
      Write_to_screen("Running pgp: Decrypting message...\n", 0);
  } else if (opts & PGP_SIGNED_MESSAGE) {
    Write_to_screen("Running pgp: Checking signature...\n", 0);
  } else {
    Write_to_screen("Running pgp ...\n", 0);
  }

  if ((fork_ret = fork()) == 0) {
    close (pgp_child_in[1]);
    close (0);
    dup (pgp_child_in[0]);
    close (pgp_child_in[0]);
    
    close (pgp_child_out[0]);
    close (1);
    dup (pgp_child_out[1]);
    close (pgp_child_out[1]);
    
    /* build up the command */
    if (usepass) {
      close (passpipe[1]);
      sprintf (cmd, "PGPPASSFD=%d; export PGPPASSFD; ",
	       passpipe[0]);
    }
    else
      cmd[0] = '\0';
    strfcat(cmd,PGP_PATH, sizeof cmd);
    strfcat(cmd," -f +verbose=0", sizeof cmd);
    strfcat(cmd, " +KEEPBINARY=OFF", sizeof cmd);
    if (usepass || opts == PGP_SIGNED_MESSAGE)
      strfcat(cmd, " +batchmode", sizeof cmd);
    _exit (system_call(cmd,0));	
  }
  
  close (pgp_child_in[0]);
  close (pgp_child_out[1]);
  
  /* now send the passphrase if needed */
  if (usepass) {
    dprint(3,(debugfile,"pgp_decrypt_init: sending pgp passphrase.\n"));
    close (passpipe[0]);
    write (passpipe[1], pgp_passphrase, strlen(pgp_passphrase));
    write (passpipe[1], "\n", 1);
    close (passpipe[1]);
  }
  
  if ((*fpin = fdopen(pgp_child_out[0], "r")) == 0) {
    Raw(ON);
    return(-1);
  }
  if ((*fpout = fdopen(pgp_child_in[1], "w")) == 0) {
    Raw(ON);
    return(-1);
  }
  
  return(fork_ret);
}

#ifdef MIME
static int pgp_mime_opts P_((char *));

static int pgp_mime_opts (s)
     char *s;
{
  char value[STRING];

  if (s) {
    if (mime_get_param ("format", value, s, sizeof(value))) {
      if (istrcmp (value, "keys-only") ==  0)
	return PGP_PUBLIC_KEY;
    }
    if (mime_get_param ("x-action", value, s, sizeof (value))) {
      if (istrcmp (value, "encryptsign") == 0)
	return (PGP_MESSAGE | PGP_SIGNED_MESSAGE);
      else if (istrcmp (value, "encrypt") == 0)
	return PGP_MESSAGE;
      else if (istrcmp (value, "sign") == 0)
	return PGP_SIGNED_MESSAGE;
      else if (istrcmp (value, "signclear") == 0)
	return PGP_SIGNED_MESSAGE;
    }
  }
  return PGP_MESSAGE;
}

static int wait_pgp P_((int,int)); /* Prototype */  

static int wait_pgp(raw,child)
     int raw;
     int child;
{
#if defined(BSD) && !defined(WEXITSTATUS)
  union wait status;
#else
  int status;
#endif

  int w,stat=-1;
  while ((w = wait(&status)) != child)
    if (w == -1 && errno != EINTR)
      break;
  if (w == child) {
#ifdef	WEXITSTATUS
    stat = WEXITSTATUS(status);
#else
# ifdef	BSD
    stat = status.w_retcode;
# else
    stat = status;
# endif
#endif
  }
  PressAnyKeyToContinue();

  if (raw)
    Raw (ON);

  return stat;
}

void pgp_decode (m, s_in, s_out)
     mime_t *m;
     in_state_t  *s_in;
     out_state_t *s_out;
{
  /* This procedure implements the de-facto standard for using PGP with MIME.
   * Content-Type: application/pgp
   * Required-Parameters: none
   * Optional parameters: format, x-action
   *     format = mime | text | keys-only
   *         mime : indicates that the signed/encrypted body contains a MIME
   *                compliant body and should be parsed recursively.
   *         text : [DEFAULT if there is no format option].  This option
   *                means that the encrypted/signed data should be presented
   *                to the user after processing, no additional processing
   *                needed.
   *         keys-only:
   *                The data in the body represents public key data only
   *     x-action = encryptsign | encrypt | sign
   *         This keyword is meant to be helpful to the application, but is
   *         not required, and may not even be necessary to look at.
   *
   *         encryptsign : the application/pgp data is both signed and
   *                       encrypted.
   *         encrypt     : the data is encrypted only
   *         sign        : the data is signed only
   */

  char tempfile[STRING], buffer[STRING];
  FILE *pgpout, *pgpin, *tmpfp=NULL, *decode_fp = NULL;
  int
    child,
    inbody = FALSE,
    opts,
    len,
    raw,
    stat = -1,
    bytes = 0,
    nested = FALSE; /* PGP output should be parsed as a MIME body */
  mime_t *tmpmt;
  
  in_state_t newstate2;
  in_state_clear(&newstate2,STATE_in_file);

  raw = RawState ();

  if (mime_get_param ("format", buffer, m->type_opts, sizeof (buffer)) &&
      (istrcmp (buffer, "mime") == 0)) {
    nested = TRUE;
    dprint (3, (debugfile, "pgp_decode: format=mime\n"));
    
    sprintf (tempfile, "%selmPT%d", temp_dir, getpid ());
    
    if (NULL == (tmpfp = safeopen_rdwr(tempfile))) {
      error("Failed to create file for decoding.");
      state_puts("[Failed to create file for decoding.]\n",s_out);      
      return;
    }

    unlink (tempfile); /* Filename is no longer needed ... */
  }
  else
    tmpfp = NULL;

  opts = pgp_mime_opts (m->type_opts);

  /* Now metapager does not ask passphrase -- this is called indirectly
   * via copy_message    -KEH
   */
  if ((opts & PGP_MESSAGE) && pgp_keeppass) {
    if (!pgp_goodPassphrase()) {
      error("Decrypting message... Bad PGP passphrase.");
      state_puts("[Decrypting message... Bad PGP passphrase]\n",s_out);
      return;
    }
  }

  buffer[0] = '\0';
  if (opts & PGP_PUBLIC_KEY) {
    strfcpy (buffer, "(** This message contains PGP public key(s)",
	     sizeof buffer);
    strfcat (buffer, " **)\n\n", sizeof buffer);
    state_puts (buffer, s_out);
  }

  if ((child = pgp_decrypt_init (&pgpout, &pgpin, opts)) == -1) {
    state_puts ("[internal error while calling pgp, skipping...]\n", s_out);
    if (raw)
      Raw (ON);
    return;
  }

  /* Decode Content-transfer-encoding */
  
  if (decode_fp = arrange_decoded(m,s_in,s_out,&newstate2)) {  
    /* If arrange_decoded was treated this as text, don't consider
     * about binary PGP files -- that happens when type is text/x-pgp
     */
    int was_text = is_text_type (mime_types[m->type], m->subtype, 
				 m->encoding);

    dprint (4, (debugfile, "pgp_decode: was_text = %d\n", was_text));

    /* Print text before PGP armor 
     *
     */

    len = state_getl (buffer, sizeof (buffer), &newstate2);    
    if ( len < 1) {
      state_puts("[No text in PGP section.]\n",s_out);
    } else {
      int c = (unsigned char) buffer[0],i;

      if (!pgp_noarmor)
	goto pgp_found;

      if (!was_text) {
	/* Check if that is binary PGP file */
	if (c & 0x80) {
	  /* In PGP block have type byte which have higgest bit set always. 
	   * so if in first byte have higgest bit set assume PGP binary
	   * file.
	   */
	  dprint (4, (debugfile, 
		      "pgp_decode: first byte = %d -- assume binary PGP file\n",
		    c));
	  goto pgp_found;
	}
	/* Another check */
	for (i = 0; i < len; i++) {
	  if (buffer[i] == 0) {
	    dprint (4, (debugfile, 
			"pgp_decode: byte idx=%d is zero -- binary file?\n",
			i));
	    state_puts("[Binary file, but does not look like PGP]\n",s_out);
	    goto pgp_found;
	  }
	}
      }
      if (strncmp(buffer, "-----BEGIN PGP", 14) == 0) 
	goto pgp_found;

      state_puts("[There is text before PGP section.]\n",s_out);

      if (set_filter(m,s_out)) { /* Character set filtering */
	do {
	  state_add_prefix(s_out);

	  /* Take care of CRLF => LF conversion in here because
	   * arrange_decoded() is treating application/pgp as
	   * binary type (it need to treate it as binary because
	   * it may be binary)
	   */
	  if (!was_text &&
	      len > 1 &&
	      buffer[len - 1] == '\n' &&
	      buffer[len - 2] == '\r') {
	    buffer[len - 2] = '\n';
	    buffer[len - 1] = '\0';
	    len--;
	  }

	  state_put(buffer,len,s_out);

	} while ((len = state_getl (buffer, sizeof (buffer), &newstate2)) > 0
		 && strncmp(buffer, "-----BEGIN PGP", 14) != 0);

      } else {
	  dprint (4, (debugfile, 
		      "pgp_decode: Unsupported character set?\n"));
      }

    pgp_found:
      s_out -> filter = NULL_filter;

      if ( len < 1) {
	state_puts("[No text in PGP section.]\n",s_out);
      } else {
	do {
	  fwrite(buffer,1,len,pgpin);
	  bytes += len;
	}  while ((len = state_getl (buffer, sizeof (buffer), 
				     &newstate2)) > 0);
      }
    }
    fclose (pgpin);

    sprintf(buffer,"-- Start of PGP%s%s section.\n",
	    opts & PGP_SIGNED_MESSAGE ? " signed" : "",
	    opts & PGP_MESSAGE ? " encoded": "");
    state_puts(buffer,s_out);

    bytes = 0;
    while ((len = mail_gets (buffer, sizeof (buffer), pgpout)) > 0) {
      if (nested) {
	if (buffer[0] == '\n' || (buffer[0] == '\r' && buffer[1] == '\n'))
	  inbody = TRUE;
	fputs (buffer, tmpfp);
	if (inbody)
	  bytes += len;
      } else {
	state_add_prefix(s_out);
	state_puts(buffer,s_out);
      }
    }
    fclose (pgpout);

    if (nested) {
      struct in_state s2_in;
      in_state_clear(&s2_in,STATE_in_file);
      
      dprint (3, (debugfile, "pgp_decode: parsing decrypted data as MIME\n"));

      if (EOF == fflush(tmpfp)) {
	error("Error when flushing temporary file.");
	state_puts("[Error when flushing temporary file.]\n",s_out);
      }
      rewind(tmpfp); /* Rewind it for reading */

      tmpmt = mime_read_header (tmpfp, 0);
      tmpmt->length = bytes;

      if (tmpmt->flags & MIME_RFC822) {
	dprint(9,(debugfile,"pgp_decode- (parsing) RFC822\n"));
	fseek (tmpfp, tmpmt->offset, SEEK_SET);
	tmpmt->parts = rfc822_parse (tmpfp, tmpmt->length);
      }
      else if (tmpmt->type == MIME_TYPE_MULTIPART) {
	char subbound[80];
	fseek (tmpfp, tmpmt->offset, SEEK_SET);
	mime_get_boundary (subbound, tmpmt->type_opts, sizeof (subbound));
	dprint(9,(debugfile,
		  "pgp_decode- (parsing) MULTIPART; boundary=%s\n",
		  subbound));
	tmpmt->parts = multipart_parse (tmpfp, tmpmt->length, subbound, 
					tmpmt->flags);
      }

      set_in_state_file(tmpfp,&s2_in);

      mime_decode (tmpmt, &s2_in, s_out);
      mime_destroy (tmpmt);

      in_state_destroy(&s2_in);

      fclose (tmpfp);
    }
  } else { 
    sprintf(buffer,"-- Start of PGP%s%s section -- can't decode content-transfer-encoding\n",
	    opts & PGP_SIGNED_MESSAGE ? " signed" : "",
	    opts & PGP_MESSAGE ? " encoded": "");
    state_puts(buffer,s_out);    
  }
  
  stat = wait_pgp(raw,child);

  sprintf(buffer,"-- End of PGP%s%s section%s\n",
	  opts & PGP_SIGNED_MESSAGE ? " signed" : "",
	  opts & PGP_MESSAGE ? " encoded" : "",
	  stat ? ", PGP failed!" : ".");
  state_puts(buffer,s_out);

  in_state_destroy(&newstate2);
}
#endif /* MIME */
#endif /* USE_PGP */
