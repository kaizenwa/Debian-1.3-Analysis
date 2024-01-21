#include "headers.h"
#include "me.h"

#ifdef USE_PGP
#include <sys/time.h>
#include <errno.h>
#ifdef BSD
#  include <sys/wait.h>
#endif
#include "menu.h"

/* these are defined in mailmsg1.c */
extern char expanded_to[VERY_LONG_STRING];
extern char expanded_cc[VERY_LONG_STRING];
extern char expanded_bcc[VERY_LONG_STRING];
extern char subject[VERY_LONG_STRING];

extern int errno;

int pgp_keeppassfor = 300; /* 5 minutes */
char pgp_passphrase[STRING];
int pgp_expires;

/* the column in which the userid begins in the 'pgp -kv' command output */
#define PGP_USERID_OFFSET 30


/* 'n' is the key we are looking for.  'k' is the list of possible matches
   which contains 'len' entries.  prompt the user for which key s/he really
   wants to use. */
static char *
PGPSelectKey (n, k, len)
	char *n;
	char **k;
	int len;
{
  int i;
  char buf[STRING];
  menu_t menu;

  sprintf(buf, "Multiple keys match '%s':", n);
  MenuInit (&menu, buf, "Select key or 'q' to quit: ", 0);

  for (i = 0; i < len; i++)
    MenuAdd(&menu, k[i]);

  for (;;) {
    switch (MenuLoop(&menu)) {
    case 'q':
      MenuDestroy(&menu);
      return(0);
    case '\n':
    case '\r':
      MenuDestroy(&menu);
      return(k[MenuCurrent(menu)]);
    }
  }
  /* not reached */
}

static int
GetPGPKey (name, target)
	char *name;
	char *target;
{
  /* given "name", return the string to use during the pgp call to specify
     the key which the user means.  return -1 on error. */
  char buf[STRING], address[STRING], *c, **keys=0, *pc, userpart[STRING];
  int i=0, keys_len=0, keys_max=0, return_val=0, start=0;
  FILE *p;

  if (!name || !target)
    return(-1);

  if (index(name, '@') == NULL && index(name, '(') == NULL && index(name, '<') == NULL) {
    /* this is either just a username, or someone's real name.  in either
       case it only needs to be checked once. */

    strfcpy(address, name, sizeof address);
    i = 2;
  }
  else {
    get_address_from (name, address);
  
    i=0;
    while (address[i] && address[i] != '@') {
      userpart[i] = address[i];
      i++;
    }
    userpart[i] = '\0';

    i = 0;
  }
  c = address;

  /* the following loop first checks to see if any keys with the full
     address, or real name, or finally the username exist */

  for (;;) {
    int fd[2];

    if (pipe (fd) == -1) {
      dprint (1, (debugfile, "GetPGPKey(): ERROR: pipe (errno %d)\n", errno));
      return -1;
    }

    while (*c && isspace (*c)) c++; /* move past any leading space! */
    
    if (fork () == 0) {
      int tmp;
      
      setgid(groupid);
      setuid(userid);

      close (fd[0]);
      close (1);
      dup (fd[1]);
      close (fd[1]);

      execl(PGP_PATH, 
	    PGP_PATH,"+verbose=0", "+language=en", "-kv", c, (char *)0);

      tmp = errno;
      perror(PGP_PATH);
      _exit(tmp);
    }

    close (fd[1]);
    p = fdopen (fd[0], "r");
    if (p == NULL) {
      dprint (1, (debugfile, 
		  "GetPGPKey(): ERROR: fdopen (errno %d)\n", errno));
      return -1;
    }

    while (fgets(buf, STRING, p) != NULL) {
      /* see if we've reached the beginning of the key listings... */
      if (!start && strncmp(buf, "pub", 3)==0)
	  start=1;

      if (start) {
	int Len;

	/* if we've read all the keys, stop here */
	if (buf[0] != 'p' && buf[0] != ' ')
	  break;

	if (keys_len == keys_max)
	  keys = (char**)DynamicArray(keys, sizeof(char*), &keys_max, 5);
	
	pc = rindex(buf, '\n');
	if (!pc) /* this shouldn't happen! */
	  continue;
	*pc = '\0';
	
	pc = buf + PGP_USERID_OFFSET;
	Len = strlen(pc)+1;
	keys[keys_len] = (char*)safe_malloc(Len);
	strfcpy(keys[keys_len], pc, Len);
	++keys_len;
      }
    }
    pclose(p);
    if (keys_len > 0 || i > 1)
      break;
    else {
      if (i == 0) {
	get_real_name(name, address, sizeof (address));

	/* if there was no real name, go on to the userpart check */
	if (strcmp(name, address) == 0) {
	  c = userpart;
	  i++;
	}
      } else if (i == 1)
	c = userpart;
      i++;
    }
  }

  if (keys_len == 1) /* perfect match! */
    get_address_from(keys[0], target);
  else if (keys_len > 1) { /* ask the user which, if any, s/he meant */
    c = PGPSelectKey(name, keys, keys_len);
    if (c)
      get_address_from(c, target);
    else {
      target[0] = '\0';
      return_val = -1;
    }
  } else
    return_val = -1;

  DestroyDynamicArray(keys);

  return(return_val);
}

static int
pgp_call (filename, opts)
	char *filename;
	int opts;
{
  char tobuf[VERY_LONG_STRING] = {0};
  char frombuf[VERY_LONG_STRING] = {0};
  int status;

  if (opts & PGP_MESSAGE) {
    /* build up the list of recipients */
    strfcpy(tobuf, expanded_to, sizeof tobuf);
    if (expanded_cc[0] != '\0') {
      strfcat(tobuf, ", ", sizeof tobuf);
      strfcat(tobuf, expanded_cc, sizeof tobuf);
    }
    if (expanded_bcc[0] != '\0') {
      strfcat(tobuf, ", ", sizeof tobuf);
      strfcat(tobuf, expanded_bcc, sizeof tobuf);
    }
    /* If the message is to be encrypted, give the user a chance to edit
     * the list of ids to encrypt to since the given address may not always
     * be correct.
     */
  redraw:
    PutLine0 (elm_LINES-2, 0, "To: ");
    status = optionally_enter (tobuf, elm_LINES-2, 4, 
			       OE_APPEND_CURRENT|OE_REDRAW_MARK,
			       sizeof tobuf);
    if (REDRAW_MARK == status)
      goto redraw;
    if (status < 0 || tobuf[0] == '\0')
      return FALSE;
  }

  if (pgp_askpgpsig && (opts & PGP_SIGNED_MESSAGE)) {
    /* If the message is to be signed, give the user a chance to specify
     * with which signature.
     */
    strfcat(frombuf, username, sizeof frombuf);
  redraw2:
    PutLine0 (elm_LINES-2, 0, "From: ");
    status = optionally_enter (frombuf, elm_LINES-2, 6,
			       OE_APPEND_CURRENT|OE_REDRAW_MARK,
			       sizeof frombuf);
    if (REDRAW_MARK == status)
      goto redraw2;
    if (status < 0 || frombuf[0] == '\0')
      return FALSE;
  }
  return (pgp_encrypt (filename, tobuf, frombuf, opts, auto_cc));
}

int
pgp_encrypt (filename, ids, sig, opts, metoo)
	char *filename, *ids, *sig;
	int opts, metoo;
{
  int i, id_len=0, id_max=0, st, usepgppass=FALSE, fd[2];
  char
    keyid[STRING], buf[VERY_LONG_STRING], buf2[STRING], **id_array=0, *c,
    *p;
  
  dprint (2, (debugfile, "pgp_encrypt(): ids=\"%s\", signwith=\"%s\", encrypt=%s, sign=%s\n", ids, sig, opts & PGP_MESSAGE ? "TRUE" : "FALSE", opts & PGP_SIGNED_MESSAGE ? "TRUE" : "FALSE"));

  p = ids;
  /* If this message is to be encrypted, look up the keys of all the
     recipients */
  if (opts & PGP_MESSAGE) {
    i=0;
    while ((c = strtok(p, ",")) != NULL) {
      int Len;
      if (GetPGPKey(c, keyid) == -1) {
	error1("Couldn't find key matching '%s'!",c);
	return FALSE;
      }
      if (id_len == id_max)
	id_array = (char**)DynamicArray(id_array, sizeof(char*), &id_max, 25);
      Len = strlen(keyid) + 1;
      id_array[id_len] = (char*)safe_malloc(Len);
      strfcpy(id_array[id_len], keyid, Len);
      id_len++;
      p=NULL;
    }
  }

  if ((opts & PGP_SIGNED_MESSAGE) && pgp_keeppass && pgp_goodPassphrase()) {
    usepgppass = TRUE;
    pipe(fd);
  }

  /* build up the command */
  buf[0] = '\0';
  if (usepgppass) {
    sprintf(buf2, "PGPPASSFD=%d; export PGPPASSFD; ", fd[0]);
    strfcat(buf, buf2, sizeof buf);
  }
  strfcat(buf, PGP_PATH, sizeof buf);
  if (metoo)
    strfcat(buf, " +encrypttoself=on", sizeof buf);
  if (usepgppass)
    strfcat(buf, " +batchmode", sizeof buf);
  if (opts & PGP_SIGNED_MESSAGE)
    strfcat(buf," +clearsig=on", sizeof buf);

  strfcat(buf, " +verbose=0 -atw", sizeof buf);

  if (opts & PGP_SIGNED_MESSAGE)
    strfcat(buf, "s", sizeof buf);
  if (opts & PGP_MESSAGE)
    strfcat(buf, "e", sizeof buf);

  if (pgp_askpgpsig && (opts & PGP_SIGNED_MESSAGE)) {
    strfcat(buf, "u ", sizeof buf);
    strfcat(buf, sig, sizeof buf);
  }

  strfcat(buf, " ", sizeof buf);
  strfcat(buf, filename, sizeof buf);

  /* add all of the userid's protected inside quotes to prevent shell
     no-no's */
  if (opts & PGP_MESSAGE)
    for (i=0; i<id_len; i++) {
      strfcat(buf, " '", sizeof buf);
      strfcat(buf, id_array[i], sizeof buf);
      strfcat(buf, "'", sizeof buf);
    }
  DestroyDynamicArray(id_array); /* don't need this any more... */

  if (usepgppass) {
    Raw(OFF);
    ClearScreen();/**/

    write(fd[1], pgp_passphrase, strlen(pgp_passphrase));
    write(fd[1], "\n", 1); /* pgp expects this as a line terminator! */
    close(fd[1]);
  }
  else {
    Raw(OFF);
    ClearScreen();
    printf("Executing: %s\n\n", buf);
  }
  st = system_call(buf, 0);
/*  if (!usepgppass)/**/
    Raw(ON);
  
  if (st == 0) { /* pgp returns zero upon success */
    /* copy the file into it's final destination */
    sprintf(buf, "%s.asc", filename);
#ifdef RENAME
    if (rename(buf, filename) < 0) {
      error ("Could not rename temporary file!");
      return FALSE;
    }
#else
    if (link(buf, filename) < 0) {
      Centerline(elm_LINES, "Could not create link to temporary file!");
      return FALSE;
    }
    /* this is not fatal, but a warning should be given */
    if (unlink(buf) < 0)
      error ("Could not unlink temporary file!");
#endif
  }
  else {
    error ("pgp returned a non-zero value!");
    if (pgp_keeppass)
      pgp_void_passphrase ();
    return FALSE;
  }
  return opts;
}

int
pgp_menu (filename)
	char *filename;
{
  int update = TRUE;

  for (;;) {
    if (update) {
      MoveCursor (elm_LINES-3, 0);
      CleartoEOS ();
      Centerline (elm_LINES-2, "e)ncrypt, s)ign, b)oth encrypt and sign");
      PutLine0 (elm_LINES-3, 0, "pgp: ");
      update = FALSE;
    }
    switch(ReadCh(REDRAW_MARK)) {
    case 'e':
      Write_to_screen("Encrypt", 0);
      return (pgp_call (filename, PGP_MESSAGE));
    case 's':
      Write_to_screen("Sign", 0);
      return (pgp_call (filename, PGP_SIGNED_MESSAGE));
    case 'b':
      Write_to_screen("Sign and Encrypt", 0);
      return (pgp_call (filename, PGP_MESSAGE | PGP_SIGNED_MESSAGE));
    case '\n':
    case '\r':
      return FALSE;
    case REDRAW_MARK:
    case ctrl('L'):
      update = 1;
    }
  }
  /* not reached */
}

int pgp_mail_public_key ()
{
  int ret;
  struct run_state RS;
  char * argv[20];
  int argc = 0;
  char userid[SLEN], pgpkey[SLEN], tmpfil[STRING], cmd[STRING], subj[STRING];
  int status;
  int need_redraw = FALSE;

  userid[0] = '\0';
  pgpkey[0] = '\0';

  PutLine0(elm_LINES-2, 0, "Enter userid of public key: ");
  CleartoEOS ();
  status = optionally_enter(userid, elm_LINES-2, 28, OE_REDRAW_MARK,
			    sizeof userid);
  while(REDRAW_MARK == status) {
    PutLine0(elm_LINES-2, 0, "Enter userid of public key: ");
    status = optionally_enter(userid, elm_LINES-2, 28, 
			      OE_REDRAW_MARK|OE_APPEND_CURRENT,
			      sizeof userid);
    need_redraw = TRUE;
  }
  if ( status != 0)
    return(need_redraw);

  if (GetPGPKey(userid, pgpkey) < 0) {
    Centerline(elm_LINES, "Sorry, couldn't find that userid.");
    ClearLine(elm_LINES-2);
    return(TRUE);
  }
  sprintf(tmpfil, "%selm.%d", temp_dir, getpid());
  
  argv[argc++] = PGP_PATH;
  argv[argc++] = "+verbose=0";
  argv[argc++] = "-kxa";
  argv[argc++] = pgpkey;
  argv[argc++] = tmpfil;
  argv[argc++] = NULL;

  ret = start_run(&RS,SY_NOTTY,argv,-1,-1);

  if (ret) {
    int exit_code;
    ret = run_already_done(&RS,&exit_code);
    if (0 == ret) {
      error("Running PGP...");
      fflush(stdout);
      ret = wait_end(&RS,&exit_code);
    }
    if (ret < 0) {
      error1("%.30s fail: Signal?",argv[0]);
      return TRUE;
    } else if (ret > 0) {
      if (RS.errno)
	error2("Failed: %.30s: %.40s",
	       argv[0],error_description(RS.errno));
      else if (exit_code) {
	char buffer[90];
	sprintf(buffer,"pgp returned error status %d",exit_code);
	error(buffer);
	return TRUE;
      } else {
	error("Running PGP... Done.");
	sprintf(included_file, "%s.asc", tmpfil);
      }
    } else {
      error2("%.30s lost: %.40s",
		     argv[0],error_description(RS.errno));
      return TRUE;
    }
  
  } else {
    if (RS.errno)
      error2("Failed: %.30s: %.40s",
	     argv[0],error_description(RS.errno));
    else
      error1("Can't start %.30s",argv[0]);
    return TRUE;
  }

  /* set the default subject for this message */
  sprintf(subj, "PGP public key for %s", pgpkey);

  pgp_status = PGP_PUBLIC_KEY;

  /* Now send the message off! */
  send_msg ("", "", subj, 0, 0);

  unlink (included_file); /* make sure to clean up. */
  included_file[0] = '\0';
  pgp_status = 0; /* reset */
  return(TRUE);
}

int pgp_extract_public_key ()
{
  char tempfile[STRING], buf[STRING];
  struct run_state RS;
  char *argv[20];
  int argc = 0;
  FILE *fpout;
  int err,res;

  sprintf(tempfile,"%selm.%d",temp_dir,getpid());
  fpout=safeopen_rdwr(tempfile);
  if (!fpout) {
    error1("Could not open temp file %s for writing!", tempfile);
    return FALSE;
  }
  copy_message(mailfile,headers[current-1],
	       "",fpout,0);
  fclose(fpout);

  argv[argc++] = PGP_PATH;
  argv[argc++] = "+verbose=0";
  argv[argc++] = tempfile;
  argv[argc++] = NULL;
  
  res=start_run(&RS,SY_CLRWAIT,argv,-1,-1);
  if (res) {
    int exit_code;
    wait_end(&RS,&exit_code);
  }
  unlink(tempfile);
  return TRUE;
}

#endif /* USE_PGP */
