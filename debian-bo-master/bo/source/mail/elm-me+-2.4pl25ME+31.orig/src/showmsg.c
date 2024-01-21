
static char rcsid[] = "@(#)$Id: showmsg.c,v 5.16 1994/08/30 15:09:43 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.16 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 *****************************************************************************/

/** This file contains all the routines needed to display the specified
    message.
**/

#include "headers.h"
#include <errno.h>
#include "s_elm.h"
#include "me.h"

extern int errno;

int pipe_abort = FALSE; /* not used anymore, but won't compile without it */

#ifdef MIME
static int
need_meta (hdr)
struct header_rec *hdr;
{
  /* Determine whether or not we need to call metamail to display the
   * message contents.
   */
  int result = 0;

  if ((hdr->status & (MIME_MESSAGE)) && (hdr->status & MIME_UNSUPPORTED))
      result = TRUE;
  else if (hdr->status & (MIME_MESSAGE | PRE_MIME_CONTENT))
    result = hdr->mime_rec.notplain;

  dprint(9,(debugfile,"need_meta=%d\n",result));
  return result;
}
#endif

int
show_msg(number)
int number;
{
	/*** Display number'th message.  Get starting and ending lines
	     of message from headers data structure, then fly through
	     the file, displaying only those lines that are between the
	     two!

	     Return 0 to return to the index screen or a character entered
	     by the user to initiate a command without returning to
	     the index screen (to be processed via process_showmsg_cmd()).
	***/

	struct header_rec *current_header = headers[number-1];

	dprint(4, (debugfile,"displaying %d lines from message %d using %s\n", 
		current_header->lines, number, pager));

	if (number > message_count || number < 1)
	  return(0);

	if(ison(current_header->status, NEW)) {
	  clearit(current_header->status, NEW);   /* it's been read now! */
	  current_header->status_chgd = TRUE;
	}
	if(ison(current_header->status, UNREAD)) {
	  clearit(current_header->status, UNREAD);   /* it's been read now! */
	  current_header->status_chgd = TRUE;
	}

#ifdef MIME
	/* May need recheck: (Because now defination is recursive) */
	if ((current_header->status & MIME_MESSAGE) && 
	    current_header->mime_rec.notplain)
	  attach_parse(current_header,mailfile);

        if (need_meta(current_header) && have_metamail()) {
		char fname[STRING], Cmd[SLEN], line[VERY_LONG_STRING];
		int code, err;
		long lines = current_header->lines;
		FILE *fpout;

		if (fseek(mailfile, current_header->offset, 0) == -1) {
		    err = errno;
		    dprint(1, (debugfile,
		      "Error: seek %d bytes into file, errno %s (show_message)\n",
		      current_header->offset, error_description(err)));
		    error2(catgets(elm_msg_cat, ElmSet, ElmSeekFailedFile,
		      "ELM [seek] couldn't read %d bytes into file (%s)."),
		      current_header->offset, error_description(err));	
		    return(0);
		}
		sprintf(fname, "%semm.%d.%d", temp_dir, getpid(), getuid());
		if ((fpout = safeopen_rdwr(fname)) == NULL) {
		  err = errno;
		  dprint(1, (debugfile,
			     "Error: open of temporary file %s, errno %s (show_message)\n",
			     fname, error_description(err)));
		  error2(catgets(elm_msg_cat, ElmSet, ElmCantOpenAsOutputFile,
				 "Can't open \"%s\" as output file! (%s)."),
			 fname, error_description(err));
		  return(0);
		}
		/* Let metamail decode it! 
		 * (Now CM_DECODE also decodes MIME and PGP) -KEH
		 */
		copy_message(mailfile,current_header,
			     "", fpout, 0);
		(void) fclose (fpout);
		sprintf(Cmd, "%s -p -z -m Elm %s", metamail_path, fname);
		Raw(OFF);
		ClearScreen();
		printf ("Executing metamail...\n");
		code = system_call(Cmd, SY_ENAB_SIGINT|SY_ENV_METAMAIL);
		Raw(ON | NO_TITE);	/* Raw on but don't switch screen */
		(void) unlink (fname);
		PutLine0(elm_LINES,0, catgets(elm_msg_cat, ElmSet, 
					      ElmPressAnyKeyIndex,
			     "Press any key to return to index."));
		(void) ReadCh(0);
		printf("\r\n");
		Raw(OFF | NO_TITE); /* Raw off so raw on takes effect */
		Raw(ON); /* Finally raw on and switch screen */
		return(0);
	}
#endif
        return (metapager (mailfile, current_header, TRUE));
}
