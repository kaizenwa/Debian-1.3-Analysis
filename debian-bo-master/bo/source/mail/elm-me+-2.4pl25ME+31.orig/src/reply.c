
static char rcsid[] = "@(#)$Id: reply.c,v 5.15 1993/08/03 19:28:39 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.15 $   $State: Exp $
 *
 *			Copyright (c) 1988-1992 USENET Community Trust
 *			Copyright (c) 1986,1987 Dave Taylor
 ******************************************************************************
 *
 *****************************************************************************/

/*** routine allows replying to the sender of the current message 

***/

#include "headers.h"
#include "me.h"
#include "s_elm.h"
#include <errno.h>

/** Note that this routine generates automatic header information
    for the subject and (obviously) to lines, but that these can
    be altered while in the editor composing the reply message! 
**/

char *strip_parens(), *get_token();

extern int errno;

char *error_description();

/* Prototype */
static void get_reply_subj P_((char *, char *, char *, int));

static int break_down_tolist P_((char *, int *, char *, char *, int, int));

/* Determine the subject to use for a reply.  */
static void get_reply_subj(out_subj,in_subj,dflt_subj, size)
     char *out_subj;	 /* store the resulting subject here		*/
     char *in_subj;	/* subject of the original message		*/
     char *dflt_subj;	/* default to use if "in_subj" is empty		*/
     int size;
{
	if ( *in_subj == '\0' ) {
	  strfcpy(out_subj,dflt_subj, size);
	  return;
	}
	if (
	  ( in_subj[0] == 'r' || in_subj[0] == 'R' ) &&
	  ( in_subj[1] == 'e' || in_subj[1] == 'E' ) &&
	  ( in_subj[2] == ':' )
	) {
	  for ( in_subj += 3 ; whitespace(*in_subj) ; ++in_subj ) ;
	}
	strfcat( strfcpy( out_subj, "Re: ", size ), in_subj, size);
}

int
optimize_and_add(new_address, full_address, size)
     char *new_address, *full_address;
     int size;
{
	/** This routine will add the new address to the list of addresses
	    in the full address buffer IFF it doesn't already occur.  It
	    will also try to fix dumb hops if possible, specifically hops
	    of the form ...a!b...!a... and hops of the form a@b@b etc 
	**/

	register int len, host_count = 0, i;
	char     hosts[MAX_HOPS][SLEN];	/* array of machine names */
	char     *host, *addrptr;

	if (in_list(full_address, new_address))
	  return(1);	/* duplicate address */

	/** optimize **/
	/*  break down into a list of machine names, checking as we go along */
	
	addrptr = (char *) new_address;

	while ((host = get_token(addrptr, "!", 1)) != NULL) {
	  for (i = 0; i < host_count && ! equal(hosts[i], host); i++)
	      ;

	  if (i == host_count) {
	    strfcpy(hosts[host_count++], host, SLEN);
	    if (host_count == MAX_HOPS) {
	       dprint(2, (debugfile,
              "Error: hit max_hops limit trying to build return address (%s)\n",
		      "optimize_and_add"));
	       error(catgets(elm_msg_cat, ElmSet, ElmBuildRAHitMaxHops,
		"Can't build return address. Hit MAX_HOPS limit!"));
	       return(1);
	    }
	  }
	  else 
	    host_count = i + 1;
	  addrptr = NULL;
	}

	/** fix the ARPA addresses, if needed **/
	
	if (qchloc(hosts[host_count-1], '@') > -1)
	  fix_arpa_address(hosts[host_count-1]);
	  
	/** rebuild the address.. **/

	new_address[0] = '\0';

	for (i = 0; i < host_count; i++)  
	  sprintf(new_address, "%s%s%s", new_address, 
	          new_address[0] == '\0'? "" : "!",
	          hosts[i]);

	if (full_address[0] == '\0')
	  strfcpy(full_address, new_address, size);
	else {
	  len = strlen(full_address);
	  if (len + strlen(new_address) > size-5)
	    return 1;   /* Buffer overflow */
	  full_address[len  ] = ',';
	  full_address[len+1] = ' ';
	  full_address[len+2] = '\0';
	  strfcat(full_address, new_address, size);
	}

	return(0);
}

/* Kari E. Hurtta <Kari.Hurtta@Fmi.FI> */
void handle_reply_to(return_address, to_address, size)
     char *return_address, *to_address;
     int size;
{
    char buf[VERY_LONG_STRING], 
         address[SLEN], comment[SLEN];
    int  iindex, line_len, err;
    int count=0;

    /** First off, get to the first line of the message desired **/

    if (skip_envelope(headers[current-1], mailfile) == -1) {
        err = errno;
        dprint(1,(debugfile,"Error: seek %ld resulted in errno %s (%s)\n",
                 headers[current-1]->offset, error_description(err),
                 "handle_reply_to"));
        error2(catgets(elm_msg_cat, ElmSet, ElmSeekFailedFile,
                "ELM [seek] couldn't read %d bytes into file (%s)."),
                headers[current-1]->offset, error_description(err));
        return;
    }


    /** now let's parse the actual message! **/

    /* read_header_line reads also continuation lines ... 
     * 2 == check that this is header line...
     */
    while (0 < (line_len = read_header_line(mailfile,buf,sizeof(buf),
					    RHL_CHECK_HEADER))) {
      /* we only want lines with reply-to addresses */
      if (!header_cmp(buf, "Reply-To", NULL)) 
        continue;

      /* extract the addresses from header */

      iindex = chloc(buf, ':')+1;               /* point beyond header name */
      dprint(2,(debugfile,"> %s\n",buf));

      /* go through all addresses in this line */
      while (break_down_tolist(buf, &iindex, address, comment,
			       sizeof address, sizeof comment)) {
	/* can't use okay_address here ! */

	/* do NOT add sendder's domain in here ! It was MTA's task
           and if it is not added then there is good reason.
	   (adding of senders domain to unqualified addresses
	    BREAKS my local mailing lists!!!)
	   That is: It is possible that Reply-to is written by some
		    other site than From !!!
	   - Kari E. Hurtta <Kari.Hurtta@Fmi.Fi>
         */
          if (!optimize_and_add(address, to_address, size))
		count++;
      }
    }

    /* if there wasn't reply-to header */
    if (!count)
	optimize_and_add(return_address, to_address, size);
}
 

void
get_and_expand_everyone(return_address, full_address, size)
char *return_address, *full_address;
int size;
{
	/** Read the current message, extracting addresses from the 'To:'
	    and 'Cc:' lines.   As each address is taken, ensure that it
	    isn't to the author of the message NOR to us.  If neither,
	    prepend with current return address and append to the 
	    'full_address' string.
	**/

    char ret_address[SLEN], buf[VERY_LONG_STRING], new_address[SLEN],
	 address[SLEN], comment[SLEN];
    int  iindex, line_len, err;

    /** First off, get to the first line of the message desired **/

    if (skip_envelope(headers[current-1], mailfile) == -1) {
	err = errno;
	dprint(1,(debugfile,"Error: seek %ld resulted in errno %s (%s)\n", 
		 headers[current-1]->offset, error_description(err), 
		 "get_and_expand_everyone"));
	error2(catgets(elm_msg_cat, ElmSet, ElmSeekFailedFile,
		"ELM [seek] couldn't read %d bytes into file (%s)."),
		headers[current-1]->offset, error_description(err));
	return;
    }
 
    /** okay!  Now we're there!  **/

    /** let's fix the ret_address to reflect the return address of this
	message with '%s' instead of the persons login name... **/

    translate_return(return_address, ret_address);

    /** now let's parse the actual message! **/

    /* read_header_line reads also continuation lines ... 
     * 2 == check that this is header line
     */
    while (0 < (line_len = read_header_line(mailfile,buf,sizeof(buf),
					    RHL_CHECK_HEADER))) {

      /* we only want lines with addresses */
      if (!header_cmp(buf, "To", NULL) && !header_cmp(buf, "cc", NULL))
	continue;

      iindex = chloc(buf, ':')+1;		/* point beyond header name */
      dprint(2,(debugfile,"> %s\n",buf));

      /* go through all addresses in this line */
      while (break_down_tolist(buf, &iindex, address, comment,
			       sizeof address, sizeof comment)) {
	if (okay_address(address, return_address)) {

	  /**
	      Some mailers can emit unqualified addresses in the
	      headers, e.g. a Cc to a local user might appear as
	      just "user" and not "user@dom.ain".  We do a real
	      low-rent check here.  If it looks like a domain
	      address then we will pass it through.  Otherwise we
	      send it back through the originating host for routing.
	  **/
	  if (qchloc(address, '@') >= 0)
	    strfcpy(new_address, address, sizeof new_address);
	  else
	    sprintf(new_address, ret_address, address);
	  optimize_and_add(new_address, full_address, size);

	}
      }
    }
}

int
reply()
{
	/** Reply to the current message.  Returns non-zero iff
	    the screen has to be rewritten. **/

	char return_address[SLEN], subject[SLEN];
	char to_line[VERY_LONG_STRING];
	int  return_value, form_letter;

	form_letter = (headers[current-1]->status & FORM_LETTER);

	if (get_return(return_address, current-1, sizeof return_address)) {
	  strfcpy(subject, headers[current-1]->subject, sizeof subject);
	} else {
	  get_reply_subj( subject, headers[current-1]->subject,
			  ( form_letter ?
			    catgets(elm_msg_cat, ElmSet, ElmFilledInForm, 
				    "Filled in form") :
			    catgets(elm_msg_cat, ElmSet, ElmReYourMail, 
				    "Re: your mail") ),
			  sizeof subject);
	}
#ifdef USE_EMBEDDED_ADDRESSES
	to_line[0] = '\0';
	handle_reply_to(return_address,to_line, sizeof (to_line));
#else
	strfcpy(to_line,return_address, sizeof toline);
#endif
	if (form_letter)
	  return_value = mail_filled_in_form(to_line, subject);
	else {
	  return_value = send_msg(to_line, "", subject,
				  MAIL_EDIT_MSG | MAIL_REPLYING,
				  NO);
          if (me_retcode) {
            headers[current-1]->status |= REPLIED;
            headers[current-1]->status_chgd = TRUE;
          }
        }
	return(return_value);
}

int
reply_to_everyone()
{
	/** Reply to everyone who received the current message.  
	    This includes other people in the 'To:' line and people
	    in the 'Cc:' line too.  Returns non-zero iff the screen 
            has to be rewritten. **/

	char return_address[SLEN], subject[SLEN];
	char full_address[VERY_LONG_STRING];
	char to_line[VERY_LONG_STRING];
	int  return_value;

	get_return(return_address, current-1, sizeof return_address);
#ifdef USE_EMBEDDED_ADDRESSES
        to_line[0] = '\0';
        handle_reply_to(return_address,to_line, sizeof(to_line));
#else
        strfcpy(to_line,return_address, sizeof to_line);
#endif

	full_address[0] = '\0';			/* no copies yet    */
	get_and_expand_everyone(return_address, full_address, 
				sizeof (full_address));
	dprint(2,(debugfile,
		"reply_to_everyone() - return_addr=\"%s\" full_addr=\"%s\"\n",
		return_address,full_address));

	get_reply_subj( subject, headers[current-1]->subject,
			catgets(elm_msg_cat, ElmSet, ElmReYourMail, 
				"Re: your mail"),
			sizeof subject);

        return_value = send_msg(to_line, full_address, subject,
				MAIL_EDIT_MSG | MAIL_REPLYING,
				NO);
        if (me_retcode) {
          headers[current-1]->status |= REPLIED;
          headers[current-1]->status_chgd = TRUE;
        }
	return(return_value);

}

int
forward()
{
	/** Forward the current message.  What this actually does is
	    to temporarily set forwarding to true, then call 'send' to
	    get the address and route the mail.   Modified to also set
	    'noheader' to FALSE also, so that the original headers
	    of the message sent are included in the message body also.
	    Return TRUE if the main part of the screen has been changed
	    (useful for knowing whether a redraw is needed.
	**/

	char subject[SLEN], address[VERY_LONG_STRING];
	int  results, edit_msg = FALSE;

	address[0] = '\0';

	if (headers[current-1]->status & FORM_LETTER)
	  PutLine0(elm_LINES-3,elm_COLUMNS-40, 
		   catgets(elm_msg_cat, ElmSet, 
			   ElmNoEditingAllowed,
			   "<No editing allowed.>"));
	else {
	  MCsprintf(subject, catgets(elm_msg_cat, ElmSet, ElmEditOutgoingMessage,
		  "Edit outgoing message? (%c/%c) "), *def_ans_yes, *def_ans_no);
	  edit_msg = (want_to(subject,
			      *def_ans_yes, elm_LINES-3, 0) != *def_ans_no);
	}

	if (strlen(headers[current-1]->subject) > 0) {

	  strfcpy(subject, headers[current-1]->subject, sizeof subject); 

	  /* this next strange compare is to see if the last few chars are
	     already '(fwd)' before we tack another on */

	  if (strlen(subject) < 6 || (strcmp(subject+strlen(subject)-5,
					     "(fwd)") != 0))
	    strfcat(subject, " (fwd)", sizeof subject);

	  results = send_msg(address, "", subject, 
			     (edit_msg ? MAIL_EDIT_MSG : 0 ) | MAIL_FORWARDING,
			     headers[current-1]->status & FORM_LETTER ? 
			     PREFORMATTED : allow_forms);
	}
	else
	  results = send_msg(address, "",
		catgets(elm_msg_cat, ElmSet, ElmForwardedMail, 
			"Forwarded mail..."), 
			     (edit_msg ? MAIL_EDIT_MSG : 0 ) | MAIL_FORWARDING,
			     headers[current-1]->status & FORM_LETTER ? 
			     PREFORMATTED : allow_forms);	
	return(results);
}

int
get_return_name(address, name, trans_to_lowercase)
char *address, *name;
int   trans_to_lowercase;
{
	/** Given the address (either a single address or a combined list 
	    of addresses) extract the login name of the first person on
	    the list and return it as 'name'.  Modified to stop at
	    any non-alphanumeric character. **/

	/** An important note to remember is that it isn't vital that this
	    always returns just the login name, but rather that it always
	    returns the SAME name.  If the persons' login happens to be,
	    for example, joe.richards, then it's arguable if the name 
	    should be joe, or the full login.  It's really immaterial, as
	    indicated before, so long as we ALWAYS return the same name! **/

	/** Another note: modified to return the argument as all lowercase
	    always, unless trans_to_lowercase is FALSE... **/

	/**
	 *  Yet another note: Modified to return a reasonable name
	 *  even when double quoted addresses and DecNet addresses
	 *  are embedded in a domain style address.
	 **/

	char single_address[SLEN], *sa;
	register int	i, loc, iindex = 0,
			end, first = 0;
	register char	*c;

	dprint(6, (debugfile,"get_return_name called with (%s, <>, shift=%s)\n",
		   address, onoff(trans_to_lowercase)));

	/* First step - copy address up to a comma, space, or EOLN */

	for (sa = single_address; *address; ) {
	    i = len_next_part(address);
	    if (i > 1) {
		while (--i >= 0)
		    *sa++ = *address++;
	    } else if (*address == ',' || whitespace(*address))
		break;
	    else
		*sa++ = *address++;
	}
	*sa = '\0';

	/* Now is it an Internet address?? */

	if ((loc = qchloc(single_address, '@')) != -1) {	  /* Yes */

	    /*
	     *	Is it a double quoted address?
	     */

	    if (single_address[0] == '"') {
		first = 1;
		/*
		 *  Notice `end' will really be the index of
		 *  the last char in a double quoted address.
		 */
		loc = ((end = chloc (&single_address[1], '"')) == -1)
		    ? loc
		    : end;
	    }
	    else {
		first = 0;
	    }

	    /*
	     *	Hope it is not one of those weird X.400
	     *	addresses formatted like
	     *	/G=Jukka/S=Ukkonen/O=CSC/@fumail.fi
	     */

	    if (single_address[first] == '/') {
		/* OK then, let's assume it is one of them. */

		iindex = 0;
		
		if ((c = strstr (&single_address[first], "/G"))
		    || (c = strstr (&single_address[first], "/g"))) {

		    for (c += 2; *c && (*c++ != '='); );
		    for ( ;*c && (*c != '/'); c++) {
			name[iindex++] = trans_to_lowercase
					? tolower (*c) : *c;
		    }
		    if (iindex > 0) {
			name[iindex++] = '.';
		    }
		}
		if ((c = strstr (&single_address[first], "/S"))
		    || (c = strstr (&single_address[first], "/s"))) {

		    for (c += 2; *c && (*c++ != '='); );
		    for ( ;*c && (*c != '/'); c++) {
			name[iindex++] = trans_to_lowercase
					? tolower (*c) : *c;
		    }
		}
		name[iindex] = '\0';

		for (c = name; *c; c++) {
		    *c = ((*c == '.') || (*c == '-') || isalnum (*c))
			? *c : '_';
		}

		if (iindex == 0) {
		    strfcpy (name, "X.400.John.Doe", sizeof name);
		}
		return 0;
	    }

	    /*
	     *	Is it an embedded DecNet address?
	     */

	    while (c = strstr (&single_address[first], "::")) {
		first = c - single_address + 2;
	    }
		    

	    /*
	     *	At this point the algorithm is to keep shifting our
	     *	copy window left until we hit a '!'.  The login name
	     *	is then located between the '!' and the first meta-
	     *	character to it's right (ie '%', ':', '/' or '@').
	     */

	    for (i=loc; single_address[i] != '!' && i > first-1; i--)
		if (single_address[i] == '%' || 
		    single_address[i] == ':' ||
		    single_address[i] == '/' ||
		    single_address[i] == '@') loc = i-1;
	
	    if (i < first || single_address[i] == '!') i++;

	    for (iindex = 0; iindex < loc - i + 1; iindex++)
		if (trans_to_lowercase)
		    name[iindex] = tolower(single_address[iindex+i]);
		else
		    name[iindex] = single_address[iindex+i];
	    name[iindex] = '\0';

	}
	else {	/* easier - standard USENET address */

	    /*
	     *	This really is easier - we just cruise left from
	     *	the end of the string until we hit either a '!'
	     *	or the beginning of the line.  No sweat.
	     */

	    loc = strlen(single_address)-1; 	/* last char */

	    for (i = loc; i > -1 && single_address[i] != '!'
		 && single_address[i] != '.'; i--) {
		if (trans_to_lowercase)
		    name[iindex++] = tolower(single_address[i]);
		else
		    name[iindex++] = single_address[i];
	    }
	    name[iindex] = '\0';
	    reverse(name);
	}
	return 0;
}

static int break_down_tolist(buf, iindex, address, comment, addr_size, 
			     comm_size)
     char *buf, *address, *comment;
     int  *iindex;
     int addr_size, comm_size;
{
	/** This routine steps through "buf" and extracts a single address
	    entry.  This entry can be of any of the following forms;

		address (name)
		name <address>
		address
	
	    Once it's extracted a single entry, it will then return it as
	    two tokens, with 'name' (e.g. comment) surrounded by parens.
	    Returns ZERO if done with the string...
	**/

	char buffer[LONG_STRING];
	register int i, loc = 0, hold_index, len;

	if (*iindex > strlen(buf)) return(FALSE);

	while (whitespace(buf[*iindex])) (*iindex)++;

	if (*iindex > strlen(buf)) return(FALSE);

	/** Now we're pointing at the first character of the token! **/

	hold_index = *iindex;

	if (buf[*iindex] == '"') {	/* A quoted string */
	  buffer[loc++] = buf[(*iindex)++];
	  while (buf[*iindex] != '"' && buf[*iindex] != '\0') {
	    if (buf[*iindex] == '\\' && buf[(*iindex)+1] != '\0')
	      buffer[loc++] = buf[(*iindex)++];	/* Copy backslash */
	    buffer[loc++] = buf[(*iindex)++];
	  }
	
	  if (buf[*iindex] == '"')
	    buffer[loc++] = buf[(*iindex)++]; /* Copy final " */
	}

	/*
	 * Previously, we just went looking for a "," to seperate the
	 * addresses.  This meant that addresses like:
	 *
	 *	joe@acme.com (LastName, Firstname)
	 *
	 * got split right down the middle.  The following was changed
	 * to step through the address in quanta of RFC-822 tokens.
	 * That fixes the bug, but this routine is still incurably ugly.
	 */
	i = *iindex;
	while (buf[i] != ',' && buf[i] != '\0') {
		len = rfc822_toklen(buf+i);
		strncpy(buffer+loc, buf+i, len);
		loc += len;
		i += len;
	}
	*iindex = i + (buf[i] != '\0' ? 1 : 0);
	buffer[loc] = '\0';

	while (whitespace(buffer[loc])) 	/* remove trailing whitespace */
	  buffer[--loc] = '\0';

	if (strlen(buffer) == 0) return(FALSE);

	dprint(5, (debugfile, "\n* got \"%s\"\n", buffer));

	if (buffer[loc-1] == ')') {	/*   address (name)  format */
	  for (loc = 0, len = strlen(buffer);buffer[loc] != '(' && loc < len; loc++)
		/* get to the opening comment character... */ ;

	  loc--;	/* back up to just before the paren */
	  while (whitespace(buffer[loc])) loc--;	/* back up */

	  /** get the address field... **/

	  for (i=0; 
	       i <= loc && i < addr_size-1; 
	       i++)
	    address[i] = buffer[i];
	  address[i] = '\0';

	  /** now get the comment field, en toto! **/

	  loc = 0;

	  for (i = chloc(buffer, '('), len = strlen(buffer); 
	       i < len && loc < comm_size-1; 
	       i++)
	    comment[loc++] = buffer[i];
	  comment[loc] = '\0';
	}
	else if (buffer[loc-1] == '>') {	/*   name <address>  format */
	  dprint(7, (debugfile, "\tcomment <address>\n"));
	  for (loc = 0, len = strlen(buffer);buffer[loc] != '<' && loc < len; loc++)
		/* get to the opening comment character... */ ;
	  while (whitespace(buffer[loc])) loc--;	/* back up */
	  if (loc >= 0 && !whitespace(buffer[loc])) loc++; /* And fwd again! */

	  /** get the comment field... **/

	  comment[0] = '(';
	  for (i=1; 
	       i <= loc && i < comm_size-2; 
	       i++)
	    comment[i] = buffer[i-1];
	  comment[i++] = ')';
	  comment[i] = '\0';

	  /** now get the address field, en toto! **/

	  loc = 0;

	  for (i = chloc(buffer,'<') + 1, len = strlen(buffer); 
	       i < len - 1 && loc < addr_size-1; 
	       i++)
	    address[loc++] = buffer[i];
	
	  address[loc] = '\0';
	}
	else {
	  /** the next section is added so that all To: lines have commas
	      in them accordingly **/

	  for (i=0; buffer[i] != '\0'; i++)
	    if (whitespace(buffer[i])) break;

	  if (i < strlen(buffer)) {	/* shouldn't be whitespace */
	    buffer[i] = '\0';
	    *iindex = hold_index + strlen(buffer) + 1;
	  }
	  strfcpy(address, buffer, addr_size);
	  comment[0] = '\0';
	}

	dprint(5, (debugfile, "-- returning '%s' '%s'\n", address, comment));

	return(TRUE);
}
