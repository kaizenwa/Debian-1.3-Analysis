/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 * 
 *****************************************************************************
 * 
 *  xxgdb - X Window System interface to the gdb debugger
 *  
 * 	Copyright 1990,1993 Thomson Consumer Electronics, Inc.
 *  
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of Thomson Consumer
 *  Electronics (TCE) not be used in advertising or publicity pertaining
 *  to distribution of the software without specific, written prior
 *  permission.  TCE makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  TCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 *  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 *  SHALL TCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 *  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 *  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 *  SOFTWARE.
 *
 *  Adaptation to GDB:  Pierre Willard
 *  XXGDB Created:   	December, 1990
 *
 *****************************************************************************/

/*  gdb_parser.c:
 *
 *	WARNING : gdb_parser.c is included by parser.c for GDB.
 *
 *    Parse output messages from dbx using regular expression pattern matching,
 *    and take appropriate action.
 *
 *    parse():		Parse the dbx output and invoke the appropriate action
 *			handler.
 *    filter():		Modify the dbx output before it gets displayed on the
 *			dialog window.
 *    gdb_source_command():	Test for source command.
 */

/*
 * iand	94/02/10 cope better with non-blocking I/O.  Exit when pty is closed rather
 *		 than spinning in a loop.
 *
 */

#include 	<string.h>
#include 	<stdio.h>
#include	<errno.h>

extern Boolean	Prompt;			/* True when gdb prompt arrives */

/*--------------------------------------------------------------------------+
|																			|
|	Function to output a message and ring bell when a answer				|
|	from gdb has not been recognized (possibly because of bug				|
|	in xxgdb which does not anticipate this particular answer).				|
|																			|
|	ECHO_ON means that all the output from gdb was displayed				|
|	on the dialog window (so no need to display any error message).			|
|	In particular we can arrive here because the user typed a				|
|	wrong command.															|
|																			|
|	ECHO_OFF means that the gdb command was issued internally by			|
|	xxgdb, and that the user has no knowledge of it.						|
|																			|
|	ECHO_ON and FILTER_ON means that only part of the						|
|	gdb answer to the command is displayed in the dialog window.			|
|	What is displayed or not is chosen by the filter() function.			|
|	In fact, filter() in this case will display gdb error messages			|
|	from the command sent internally by xxgdb.								|
|																			|
|	This function will only display error messages when echo and			|
|	filter are off. This is more for xxgdb debug than for the user.			|
|																			|
+--------------------------------------------------------------------------*/	
void unknown_output (outputstr, command, flags)
char *outputstr;
char *command;
int flags;
{
	if (command)
		{
		if (debug)
			fprintf(stderr, "\noutput from \"%s\" is not recognized\n", command);
	
		/* if the command was completely silent, we output this
		error message */
		
		if ((flags & (ECHO_ON | FILTER_ON)) == 0)
			{
			AppendDialogText("xxgdb error: output from \"");
			AppendDialogText(command);
			AppendDialogText("\" command is not recognized.\n");
			}
		}
		
	if (outputstr)
		if ((flags & (ECHO_ON | FILTER_ON)) == 0)
			AppendDialogText(outputstr);

	bell(0);	/* ring the bell in ALL cases */
}

/*--------------------------------------------------------------------------+
|																			|
|	Function to remove all 'Reading in symbols' message						|
|	from a string.															|
|																			|
|	This function is used in parser() before matching the output			|
|	because this message can happen any time.								|
|																			|
+--------------------------------------------------------------------------*/
void filter_reading_symbols(output)
char *output;
{
struct re_registers regs;
int	r;
char *p1;
char *p2;

		/* test for reading symbols message */
	
	while (re_match(output_pattern[O_READING_SYMBOLS].buf,output,strlen(output),0,&regs) > 0)
		{
		/* we found a "Reading in symbols for 	...done." pattern */
		
		r = output_pattern[O_READING_SYMBOLS].reg_token[TK_MESG];
		p1=  output+regs.start[r];
		p2 = output+regs.end[r];
		
		/* remove "Reading in symbols for 	...done." */
		
		while((*(p1++) = *(p2++)));
		}
}

/*--------------------------------------------------------------------------+
|																			|
| *	 This routine first parses the command string.							|
| *	 If the command is one of run, cont, next, step, stop at, stop in,		|
| *	 where, up, or down, it parses the dbx output to decide what action		|
| *	 to take and dispatch it to one of the handlers.						|
| *	 For other commands, the appropriate handler is called.					|
| *																			|
| *	 !!! This routine has to be re-entrant.									|
| *																			|
+--------------------------------------------------------------------------*/
void parse(output, command, flags)
char *output;
char *command;
int flags;
{
    int  command_type;
    char *output_string;

    if (debug) {
    char *temp;
    if(!command)temp="";else temp=command;
	fprintf(stderr, "parse(output = %s, command = %s, flags = %d)\n", output, temp, flags);
    }

    /* Make a local copy of `output' and use that instead */
    output_string = XtNewString(output);
    if (output) strcpy(output, "");

	/* test for GDB start-up */

    if (!command) {
/* (PW)28AUG91 : do no test for O_DEBUG pattern because of gdb 4.0
who most of the times displays nothing before the promt.
if (match(output_pattern, output_string, O_DEBUG) != -1)
*/
		{
   		query_gdb_directories();	/* will tell if running gdb 4.0 */
   		
	    debug_handler();	/* some init to gdb, and try to display main() */
	    
	    /* test if a core file was used in input arguments,
	    an display bomb if necessary. */
	    
	    if (match(output_pattern, output_string, O_CORE_FILE) != -1)
			core_file_handler();
	    }
	debug_init();			/* read .gdbinit file (if any) */
	XtFree(output_string);
	return;
    }
    
    /* if not GDB start-up */
    
	if (match(output_pattern, output_string, O_BELL) != -1)
		{
		/* test if this is 'show undefined'. If yes then we are
		not executing the new gdb (4.0).
			(see show_is_undefined in gdb_handler.c)
		*/
		if (match(output_pattern, output_string, O_UNDEF_SHOW) != -1)
			show_is_undefined();
		else
			unknown_output (output_string,command, flags);
    	XtFree(output_string);
		return;
		}
	
    command_type = match(command_pattern, command, C_ANY);
    
    /* remove all "Reading in symbols for pw.c...done."  */

	filter_reading_symbols(output_string);
        	
    switch (command_type) {
      	case C_EXEC: 
		case C_FINISH:
		{
		char * message;
		int signal = 0;
		if (debug) {
			fprintf(stderr, "C_EXEC or C_FINISH\n");
		}
		message = 0;
		if (match(output_pattern, output_string, O_RECEIVED_SIGNAL) != -1)
			{
			message = XtNewString(Token.mesg);
			signal = Token.stop;	/* signal number received */
			}
			
		/* warning : the order of the matching tests is important */
		
	    if ((match(output_pattern, output_string, O_EXEC_MESS_AFTER) != -1)
	    	|| (match(output_pattern, output_string, O_EXEC_MESS_BEFORE) != -1)
	    	|| (match(output_pattern, output_string, O_EXEC_GDB) != -1))
			{
			exec_handler(message,signal);
			}
	    else 
	    	{
			if (match(output_pattern, output_string, O_DONE) != -1)
				done_handler(message,signal);
			else
				unknown_output(output_string, command, flags);
			}
			
		if (message)
			{
			bell(0);
			XtFree(message);
			}
		}
	    break;
	    
	case C_UPDOWN:
		if (debug) {
			fprintf(stderr, "C_UPDOWN\n");
		}
	    if (match(output_pattern, output_string, O_UPDOWN) != -1)
			updown_handler();
	    else if (match(output_pattern, output_string, O_UPDOWN_NOSOURCE) != -1)
			/* here Token.msg and Token.func are updated. Do nothing with
			them. We are in a function with no source information. */
			bell(0);
		else
			unknown_output (output_string, command, flags);
	    break;
	case C_SEARCH:
		if (debug) {
			fprintf(stderr, "C_SEARCH\n");
		}
	    if (match(output_pattern, output_string, O_SEARCH) != -1)
		search_handler();
	    else
			unknown_output(output_string, command, flags);
	    break;
	case C_DELETE:
		if (debug) {
			fprintf(stderr, "C_DELETE\n");
		}
	    delete_handler(); 
	    break;
	case C_LIST:
		if (debug) {
			fprintf(stderr, "C_LIST\n");
		}
	    if (match(output_pattern, output_string, O_LIST) != -1)
	        list_handler();
	    else
			unknown_output(output_string, command, flags);
	    break;
	    
	case C_BREAK: 
		if (debug) {
			fprintf(stderr, "C_BREAK\n");
		}
	    if (match(output_pattern, output_string, O_BREAK) != -1)
			break_handler();
	    else
			unknown_output(output_string, command, flags);
	    break;
	    
	case C_INFO_DIR:
		if (debug) {
			fprintf(stderr, "C_INFO_DIR\n");
		}
	    if (match(output_pattern, output_string, O_INFO_DIR) != -1)
	        info_dir_handler();
	    else
			unknown_output(output_string, command, flags);
		break;
		
	case C_DIRECTORY:
		if (debug) {
			fprintf(stderr, "C_DIRECTORY\n");
		}
	    directory_handler(); 
	    break;
	    
	case C_INFO_LINE:
		if (debug) {
			fprintf(stderr, "C_INFO_LINE\n");
		}
	    if (match(output_pattern, output_string, O_INFO_LINE) != -1)
	    	info_line_handler();		/* command was 'info line' */
	    else
			unknown_output(output_string, command, flags);
	    break;
	    
	case C_INFO_BREAK:
		if (debug) {
			fprintf(stderr, "C_INFO_BREAK\n");
		}
	    info_break_handler(output_string);
	    break;

	case C_DISPLAY:	/* means "display foo\n" command */
		if (debug) {
			fprintf(stderr, "C_DISPLAY\n");
		}
		{
	    if ((strcmp(output_string, "") == 0) ||
	    	(match(output_pattern, output_string, O_DISPLAY) != -1))
	    	display_handler();
	    else
			unknown_output(output_string, command, flags);
		}
	    break;
	    
	case C_UNDISPLAY:
		if (debug) {
			fprintf(stderr, "C_UNDISPLAY\n");
		}
	    if (strcmp(output_string, "") == 0)
			display_handler();
	    else
			unknown_output(output_string, command, flags);
	    break;

	case C_DISPLAY_INFO: /* means "display\n" command */
		if (debug) {
			fprintf(stderr, "C_DISPLAY_INFO\n");
		}
		{
	    if ((strcmp(output_string, "") == 0) ||
	    	(match(output_pattern, output_string, O_DISPLAY_INFO) != -1))
	    	display_info_handler();
	    else
			unknown_output(output_string, command, flags);
		}
	    break;
	    
	case C_CD:
		if (debug) {
			fprintf(stderr, "C_CD\n");
		}
	   if (new_gdb4()) /* this fixes where cd and pwd dont work right for 4.x
						(cd doesn't do anything but beep, pwd doesn't work
						when cwd is not cannonical) */
	        query_gdb ("pwd\n", PARSE_ON);
	    else if (match(output_pattern, output_string, O_CD) != -1)
	    	cd_handler(Token.mesg); 
	    else
			unknown_output(output_string, command, flags);
	    break;
	    
	case C_PWD:
		if (debug) {
			fprintf(stderr, "C_PWD\n");
		}
	    if (match(output_pattern, output_string, O_PWD) != -1)
			pwd_handler(Token.display? Token.display : Token.mesg);
	    else
			unknown_output(output_string, command, flags);
	    break;

	case C_FRAME_CURR:
		if (debug) {
			fprintf(stderr, "C_FRAME_CURR\n");
		}
	    if (match(output_pattern, output_string, O_FRAME_CURR) != -1)
			frame_curr_handler();
	    else
			unknown_output(output_string, command, flags);
	    break;

	case C_PRINT:
		if (debug) {
			fprintf(stderr, "C_PRINT\n");
		}
		{
		/* for GDB, the label of the popup display is the expression
		string which is printed instead of $n */
		char * prtname;
		prtname = 0;
		if ((Token.mesg) && (PopupMode))
			prtname = XtNewString(Token.mesg);
			
	    if (match(output_pattern, output_string, O_PRINT) != -1)
	    	{
	    	if (prtname)
	    		{
				XtFree(Token.mesg);
				Token.mesg = prtname;
				prtname = 0;			/* not to XtFree twice this string */
				}
	    	print_handler(output_string);
	    	}
	    else
			unknown_output(output_string, command, flags);
		
		/* if PopupMode was true but GDB found an error in print
		statement (ie match() returned -1), PopupMode was never reset */
		PopupMode = FALSE;	 
		
		XtFree(prtname);
		}
	    break;
	    
	case C_SYMBOL_FILE:
	case C_FILE:
		if (debug) {
			fprintf(stderr, "C_FILE or C_SYMBOL_FILE\n");
		}
		debug_handler(); 
	    break;

	case C_SOURCE:	/* WE SHOULD NEVER ARRIVE HERE */
		if (debug) {
			fprintf(stderr, "C_SOURCE - WE SHOULD NEVER ARRIVE HERE\n");
		}
		break;

	case C_EXEC_FILE:
		if (debug) {
			fprintf(stderr, "C_EXEC_FILE\n");
		}
		break;
		
	case C_CORE_FILE:
		if (debug) {
			fprintf(stderr, "C_CORE_FILE\n");
		}
	    if (match(output_pattern, output_string, O_CORE_FILE) != -1)
			core_file_handler();
	    else
			unknown_output(output_string, command, flags);
	    break;
	    
	/* in case of 'info source', we do not call unknown_output()
	if the pattern is not matched, because we only test for the
	'compilation directory' line. When there is no compilation
	directory, there is no match and there is no error...

	(PW)8JUN93 : we also test for current source and current
	source full pathname.
	*/
	
	case C_INFO_SOURCE:
		if (debug) {
			fprintf(stderr, "C_INFO_SOURCE\n");
		}
		cdir[0] = 0;
		source_fullpath[0] = 0;
		source_path[0] = 0;
	    if (match(output_pattern, output_string, O_INFO_SOURCE) != -1)
	    	info_source_handler(Token.mesg, Token.file, Token.func);
	    break;

	default:
		if (debug) {
			fprintf(stderr, "C UNKNOWN\n");
		}
	    break;
    }
    XtFree(output_string);
}

#ifdef NEED_STRSTR
/*--------------------------------------------------------------------------+
|																			|
|	Some systems DO NOT have the ANSI strstr function						|
|																			|
+--------------------------------------------------------------------------*/	
char *
strstr (source, substr)
	char *source;
	char *substr;
{
char *src;
char *sub;

	if (!source || !substr)
		return NULL;
		
	while (*source)
		{
		for (src = source, sub = substr;
				(*src) && (*sub) && (*src == *sub);
					src++,sub++);
		if (!*sub)
			return source;
			
		source++;
		}
		
	return NULL;
}
#endif

/*--------------------------------------------------------------------------+
|																			|
|	Function to filter all the display information in a string				|
|																			|
|	input : string pointer,													|
|			already_taken_care is number of char in string already 			|
|			   processed (eg, displayed).									|
|																			|
|	output : none.															|
|																			|
|	See O_EXEC_DISPLAY in gdb_regex.h for the display pattern.				|
|																			|
|	Take care when GDB send some message after '\032\032...\n'				|
|	which is not a display line.											|
|																			|
| (gdb) finish																|
| Run till exit from #0	 foo (n=1) (pw.c line 9)							|
| main () (pw.c line 41)													|
| /usr1/gnu_sun4/xxgdb/pw.c:41:590:beg:0x232c								|
| 1: i = 1																	|
| Value returned is $1 = 1													|
| (gdb)																		|
|																			|
+--------------------------------------------------------------------------*/
void filter_display_info(output, already_taken_care)
char *output;
unsigned int already_taken_care;
{
	struct re_registers regs;
	int	r;
	char *p;
	char *p1;
	char *p2;
	char *cp_output;
	int begin_struct;

	p = cp_output = XtNewString(output);

	p1 = strstr(p,"\032\032");		/* find beginning of special gdb line */
	
	if ((p1 == 0) || ((p2 = strchr(p1+1,'\n')) == 0))
		{
		AppendDialogText(p + already_taken_care);		/* something wrong here */
		XtFree(cp_output);
		return;
		}
		
	*p1 = 0;
	
	if (p1 > (p + already_taken_care)) {
		AppendDialogText(p + already_taken_care);		/* print everything before that line */
	}
	p = p2 + 1;											/* end of that line + skip \n */

	/* test for beginning of a display */
	
	while (re_match(output_pattern[O_EXEC_DISPLAY].buf,p,strlen(p),0,&regs) > 0)
		{
		/* we found a "X:....\n" pattern */
		
		r = output_pattern[O_EXEC_DISPLAY].reg_token[TK_DISP];
		p1=  p+regs.start[r];
		p2 = p+regs.end[r];
					
		/* count number of { and } : if not equal, the next lines are part of this display */
		begin_struct = 0;
		while (p1 < p2)
			{
			switch(*(p1++))
				{
				case '{':
					begin_struct++;
					break;
				case '}':
					begin_struct--;
					break;
				}
			}
			
		p1=p+regs.start[r];
		*p1 = 0;
		if (p != p1) {
			/* do not print anything already printed */
			if (p < (cp_output + already_taken_care)) {
				p = cp_output + already_taken_care;
			}
			if (p < p1) {
				AppendDialogText(p);	/* print what is before display */
			}
		}
		p = p2;						/* skip display text */
					
		if (begin_struct)	/* skip the whole data displayed */
			{
			do				/* find the last '}' */
				{
				switch(*(p++))
					{
					case '{':
						begin_struct++;
						break;
					case '}':
						begin_struct--;
						break;
					}
				}
			while (begin_struct);
			
			/* now skip until end of line */
			while (*(p++) != '\n');
			}
		}
	
	if (p < (cp_output + already_taken_care)) {
		p = cp_output + already_taken_care;	/* do not print anything already printed */
	}
	AppendDialogText(p);		/* print what is after display */
	XtFree(cp_output);
}

/*--------------------------------------------------------------------------+
|																			|
| *	 This function edits the dbx output so that unnecessary information is	|
| *	 not displayed on the dialog window.									|
| *	 It filters away the some output returned by the execution commands;	|
| *	 output from the search commands, and the display command.				|
| *	 On Sun dbx, it also filters away part of the output returned by the	|
| *	 up and down commands.													|
| *																			|
+--------------------------------------------------------------------------*/
void
filter (string, output, command)
char *string, *output, *command;
{
    struct re_registers regs;
    char 		*p;
    char 		*p2;
    static 		Boolean	deleteRest = False;
	static 		Boolean for_gdb_only = False;
    int			command_type = -1;
	static 		unsigned int already_taken_care = 0;	
	Boolean		previous_for_gdb_only;

    if (output == NULL || strcmp(output, "") == 0) 
		return;

/* for GDB, the only things we want to filter are:

	- the line displayed because of the -fullname  option :
	"\032\032/usr1/gnu_sun4/xdbx/pw.c:6:40:beg:0x22b0\n",
	
	- the displayed info which goes into the display window,
	
	- list and search outputs

*/
	if (!string)
		string = "";
		
    if (command)
    	command_type = match(command_pattern, command, C_ANY);
    	
    if ((command_type == C_EXEC)||(command_type == C_FINISH))
    	{
		if ((re_match(output_pattern[O_EXEC_MESS_AFTER].buf,output,strlen(output),0,&regs) > 0)
				|| (re_match(output_pattern[O_EXEC_MESS_BEFORE].buf,output,strlen(output),0,&regs) > 0)
				|| (re_match(output_pattern[O_EXEC_GDB].buf,output,strlen(output),0,&regs) > 0))
			{
			/* Remove display messages from output and print what is not a display */
			
			if (Prompt) {
				filter_display_info (output, already_taken_care);	
				for_gdb_only = False;
				already_taken_care = 0;
				deleteRest = False;
		    } else {
				deleteRest = True;
			}
			return;
			}
		else  /* does not match exec pattern yet */
			{
			if (deleteRest)	 /* if already matched before */
				{
				/* Remove display messages from output and print what is not a display */
				if (Prompt)
					{
					filter_display_info (output, already_taken_care);	
					for_gdb_only = False;
					already_taken_care = 0;
					deleteRest = False;
					}
				return;
				}
			}
		}
		

	/* remember what was the output length was output was matched */
	already_taken_care = strlen(output);

	/* filter any line starting with \032\032 */

	/* (PW)18NOV94: we have a problem with some system where the \032\032 comes in
	   several times. In that case we might end-up showing ^Z^Z in dialog window.
	   SO now do no test for end of line, and note that all following outputs are 
	   for gdb, not for the user.
	   Here we assume that at least the double ^Z^Z is read in one piece.
	   */
	
	previous_for_gdb_only = for_gdb_only;

	p = strchr(string,'\032');
	if (p && (*(p+1) == '\032') && (p == string || *(p-1) == '\n') /* && (p2 = strchr(p,'\n'))*/) {
		if ((p2 = strchr(p,'\n'))) {
			while ((*(p++) = *(++p2)));
		} else {
			*p = 0;
		}
		for_gdb_only = True;
	}

	if ((command_type == C_EXEC)||(command_type == C_FINISH)) {
		if (!previous_for_gdb_only) {
			AppendDialogText(string);
		}
		if (Prompt) {
			for_gdb_only = False;
			already_taken_care = 0;
			deleteRest = False;
		}
		return;
	}
		
    if (Prompt)
    	{
		char *s;

		for_gdb_only = False;
		already_taken_care = 0;
		deleteRest = False;

		s = XtNewString(output);
		switch (command_type)
			{
			case C_DISPLAY:
			    if (match(output_pattern, s, O_DISPLAY) != -1)
				strcpy(s, "");
			    break;

			case C_DISPLAY_INFO:
   				 /* (PW)7MAY91 : display error messages */
			    if (match(output_pattern, s, O_DISPLAY_INFO) != -1)
					{    
    				if (Token.mesg && strcmp(Token.mesg, ""))	
   				    	{
						AppendDialogText(Token.mesg);
						bell(0);
    					}		
					strcpy(s, "");
    				}
			    break;
			    
			case C_SEARCH:
			    if (match(output_pattern, s, O_SEARCH) != -1)
				strcpy(s, "");
			    break;
			case C_LIST:
   				 /* (PW)22MAY91 : display messages ") */
			    if (match(output_pattern, s, O_LIST) != -1)
					{    
    				if (Token.mesg && strcmp(Token.mesg, ""))	
   				    	{
						AppendDialogText(Token.mesg);
						if (strstr(Token.mesg,"Source file is more recent than executable."))
							bell(0); /* Warn user WYSIWYG not true */
    					}		
					strcpy(s, "");
    				}
			    break;
			case C_PRINT:
				if	(PopupMode &&	/* if print goes in a window, don't display here */
			    	(match(output_pattern, s, O_PRINT) != -1))
				strcpy(s, "");
				break;
				
			default:
				XtFree(s);
			    s = XtNewString(string);		/* append 'string' only */
			    break;
			}
		AppendDialogText(s);
		XtFree(s);
	    }
	else	/* no prompt yet */
		{
		switch (command_type)
			{
			case C_DISPLAY:
			case C_DISPLAY_INFO:
			case C_SEARCH:
			case C_LIST:
			case C_PRINT:
				break;
			default:
				if (!previous_for_gdb_only) {
					AppendDialogText(string);
				}
				break;
			}
		}

	return;
}


/*--------------------------------------------------------------------------+
|																			|
|	Function to filter 'source' command										|
|																			|
|	input : command (from .gdbinit or source command or keyboard),			|
|			echo is TRUE if source command must be echoed.					|
|																			|
|	output : TRUE if source command was recognized							|
|																			|
|	In case source command is recognized, it is executed here.				|
|																			|
+--------------------------------------------------------------------------*/
int gdb_source_command(command,echo)
char *command;
int echo;
{
	if (command && (match(command_pattern, command, C_SOURCE) != -1))
		{
		if (echo)
			AppendDialogText(command);
		source_handler();	
		return TRUE;
		}
		
	return FALSE;
}

/*--------------------------------------------------------------------------+
|																			|
|	Function to filter 'define' & 'document' commands						|
|																			|
|	input : command (from .gdbinit or source command),						|
|			fp = file pointer.												|
|																			|
|	output : TRUE if define or document command was recognized				|
|																			|
|	In case the command is recognized, it is executed here.					|
|																			|
+--------------------------------------------------------------------------*/
static int command_sent = 0;	/* flag gdb is busy : send no more command to gdb. */

int gdb_define_command(command,fp)
char *command;
FILE *fp;
{
char s[LINESIZ];
int error_cmd;
int end_found;

	if ((command && (match(command_pattern, command, C_DEFINE) != -1))
		|| (command && (match(command_pattern, command, C_DOCUMENT) != -1)))
		{
		AppendDialogText(command);
		
		/* because silly gdb 4.0 displays nothing with def/doc command when
		confirm is on (possibly a gdb bug) , I just reset confirm to on
		just for this command !. */
		
		if (new_gdb4()) 
			query_gdb("set confirm on\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);

		write_dbx (command);
		
/*
				gdb could ask :
			
					"Redefine command \"%s\"? "
					"Really redefine built-in command \"%s\"? "
					
				gdb could display 
				
					"Type commands for definition of \"%s\".\n"
					"End with a line saying just \"end\"
				
				or
					"Type documentation for \"%s\".\n"
					"End with a line saying just \"end\".\n"
					
				or
				 	"Command \"%s\" is built-in."
*/
	
		error_cmd = FALSE;
				
		/* read message from gdb */
		
		while (1)
			if (fgets(s, LINESIZ, dbxfp))
				{
				if (debug)
					fprintf(stderr, "=>%s", s);
				
				if (strstr(s," is built-in."))	/* error for document of built-in command */
					{
					AppendDialogText(s);
					error_cmd = TRUE;
					bell(0);
					break;
					}
			
				if (strstr(s,"Redefine command ")
				 || strstr(s,"Really redefine built-in command "))
					write_dbx ("y\n"); 	/* answer to question if any */
				else
					{
					if (strcmp(s,"End with a line saying just \"end\".\n") == 0)
						break;
					}
				}
		
		/* write command definition */
		
		end_found = FALSE;
		
		while (fgets (s, LINESIZ, fp))
			{
			if (!error_cmd)
				{
				AppendDialogText(s);
				write_dbx (s);
				}
			
			if (match(command_pattern, s, C_END) != -1)
				{
				end_found = TRUE;
				break;
				}
			}
			
		if ((!error_cmd) && (!end_found))
			{
			AppendDialogText("Error : missing \"end\" in file.\n");
			bell(0);
			write_dbx ("end\n");
			}
		
		command_sent++;				/* flag gdb is busy (because of read_gdb) */
		insert_command("end\n");	/* insert 'end' at head of queue */
		read_until_prompt (PARSE_OFF | ECHO_ON | FILTER_OFF);	/* return when prompt */

		if (new_gdb4()) /* reset confirm to off */
			query_gdb("set confirm off\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);

		return TRUE;
		}
		
	return FALSE;
}


#ifndef NO_MY_FGETS
/*
 * cope with non-blocking I/O correctly
 * ie: exit if child closes pty, but return if would block.
 *
 *	This function seems to fail on some systems (??)
 *
 */
static char *
#ifdef __STDC__
my_fgets(char *buf, int size, FILE *f)
#else
my_fgets(buf, size, f)
char *buf;
int size;
FILE *f;
#endif
{
	char *orig_buf = buf;

	while(size > 1) {
		int cc;

		cc = read(fileno(f), buf, 1);
		if(cc == -1) {
			if(errno == EAGAIN || errno == EWOULDBLOCK) {
				break;
			}
			perror("read from gdb");
			exit(1);
			/*NOTREACHED*/
		}
		if(cc == 0) {
#ifdef READ_ZERO_NOT_EOF   /* for RS6000 */
			break;
#else
			(void) fprintf(stderr, "EOF from gdb\n");
			exit(1);
#endif
			/*NOTREACHED*/
		}
		buf++;
		size--;
		if(*(buf-1) == '\n')
			break;
	}

	*buf = '\0';
	if (buf == orig_buf) {
		return NULL;
	} else {
		return(orig_buf);
	}
}
#endif /* NO_MY_FGETS */

/*--------------------------------------------------------------------------+
|																			|
|	Read gdb output until fgets returns no string.							|
|																			|
|	The ouptut is process according to flags.								|
|																			|
|	In case the gdb prompt is found, the command on top of the				|
|	command queue (ie, the one which is assumed to correspond				|
|	to this output) is remove from the queue, and 'command_sent'			|
|	is decremented to say that gdb is no longer busy.						|
|																			|
+--------------------------------------------------------------------------*/
static int parse_flags = PARSE_ON | ECHO_ON | FILTER_ON; /* last flags used in read_gdb */

void read_gdb (flags)
int flags;
{
    static char *output = NULL; 	/* buffer for gdb output */
    static char *next_string = NULL;
    static char *command;
    char 	*string = NULL;
	char 	*prompt_pointer;
    char 	s[LINESIZ];
	int before_prompt;
    Boolean 	more;
	Boolean		local_prompt;
	parse_flags = flags; 	/* just remember flags for read_dbx() */
	
    more = True;
	Prompt = False;
	local_prompt = False;

    while (more) {

		/* keep reading until no more or until prompt arrives */
#ifdef NO_MY_FGETS
		while ((more = (fgets(s, LINESIZ, dbxfp) != NULL)))
#else
		while ((more = (my_fgets(s, LINESIZ, dbxfp) != NULL)))
#endif
		{
			if (debug)
				fprintf(stderr, "=>%s", s);

			/* new gdb might add '>' characters in front of input for define commands,
			   we have to test for them here ! 
			   */
			
			/* receive prompt? */

#if 1		/* (PW)16NOV94 : new gdb might add '>' characters in front of input 
			   when defining commands, we have to test for them here ! 
			   so use strstr() to catch all prompts...
			   */
			if ((prompt_pointer = strstr(s, dbxprompt)) != NULL)
#else
			if (strncmp(s, dbxprompt, strlen(dbxprompt)) == 0)
#endif
			{
				Prompt = True;
				local_prompt = True;
				before_prompt = prompt_pointer - s;
			
				/* more stuff behind prompt? */
				if (s[before_prompt+ strlen(dbxprompt)]) {
					/* remember it */
					next_string = XtNewString(s+before_prompt+strlen(dbxprompt));
				}
				/* destroy contents */
				strcpy(s, "");
				if (debug)
					fprintf(stderr, " Prompt detected\n");
			}
		    
			string = concat(string, s);
			strcpy(s, "");

			if (local_prompt) {  /* do not read next line if prompt is found */
				break;
			}
		}

		output = concat(output, string);
	
		command = get_command();			/* read queue's top command  */
		
	if (flags & FILTER_ON) {
	    filter (string, output, command);
	}
	    
	if ((flags & ECHO_ON) && local_prompt) {
	    AppendDialogText(xdbxprompt);
	}
	    
	if (string) {
	    XtFree(string);
	    string = NULL;
	}
	
	if (next_string) {
	    string = concat(string, next_string);
	    XtFree(next_string);
	    next_string = NULL;
	}
	
	if ((flags & PARSE_ON) && local_prompt) {
	    parse (output, command, flags);
	}
	
	/* note : it is important to decrement 'command_sent' AFTER
	parse() is executed, so that no other commands will mix-up
	the commands sent by the function handlers executed by parse().
	*/
	
	if (local_prompt) {
	    delete_command();		/* remove top command from queue */
	    if (command && command_sent)
			command_sent--;
	    XtFree(output);
	    output = NULL;
		local_prompt = False;
	}
	}
}

/*--------------------------------------------------------------------------+
| *																			|
| *	 This is a callback procedure invoked everytime when input is pending	|
| *	 on the file descriptor to dbx.											|
| *	 o reads all the data available on the file descriptor line by line		|
| *	   into local variable 'string' and global variable 'output'.			|
| *	   'output' records the entire dbx output whereas 'string' records		|
| *	   only the data read in this invocation of read_dbx().					|
| *	 o in Echo mode, the contents in 'string' is edited by filter()			|
| *	   before it gets displayed on the dialog window.						|
| *	 o once the dbx prompt is read, calls parse() to analyse the dbx output |
| *	   and take appropriate action.											|
| *																			|
+--------------------------------------------------------------------------*/
static volatile int in_read_dbx;

/* ARGSUSED */
void read_dbx(master, source, id)
XtPointer master;
int 	  *source;
XtInputId *id;
{
	if (in_read_dbx++)	/* already running */
		return;

	do {
		read_gdb (parse_flags);

		/* do not write any more command, if prompt was not seen yet */

		if (Prompt) {
			write_dbx_available ();
		}

	} while (--in_read_dbx);
}

/*--------------------------------------------------------------------------+
|																			|
|	Read gdb output until prompt.											|
|	The ouptut is process according to flags.								|
|																			|
+--------------------------------------------------------------------------*/
void read_until_prompt (flags)
int flags;
{
    Prompt = False;
    while (!Prompt)
        read_gdb (flags);
}

/*--------------------------------------------------------------------------+
|																			|
| *	 Sends a command to gdb and read the corresponding output, directly		|
| *	 invoking the Xt input procedure, read_gdb().							|
| *																			|
| * Same as query_dbx() in dbx.c except that echo, filter and parse.		|
| *	 are passed in argument flags:											|
| * PARSE_ON | PARSE_OFF | ECHO_ON | ECHO_OFF | FILTER_ON | FILTER_OFF		|
| *																			|
+--------------------------------------------------------------------------*/
void query_gdb(command, flags)
char *command;
int flags;
{
	command_sent++;				/* flag gdb is busy */
	
    write_dbx(command);			/* send command to gdb */
    
    insert_command(command);	/* insert at head of queue */

	read_until_prompt (flags);	/* return when prompt */
	
	/* test if we are at the top command, and if some commands are waiting */
	
	write_dbx_available ();	/* send next waiting command */
}

/*--------------------------------------------------------------------------+
|																			|
|	Function to send the first command of the command queue to gdb only in	|
|	case gdb is waiting for a command.										|
|																			|
|	WARNING : we must test for 'source' command.							|
|																			|
|	The commands in the command queue are from the keyboard and from		|
|	the buttons.															|
|																			|
+--------------------------------------------------------------------------*/
void write_dbx_available()
{
char *command;

	if ((command = get_command ()))
		{
		if (command_sent++ == 0)	
			{
			/* here, the source command is already displayed, say echo command = FALSE */
			
			if (gdb_source_command (command, FALSE))
				{
				delete_command();		/* remove source command from top of queue */
				command_sent--;
				/* tell gdb to send its prompt */
				query_gdb(" \n", PARSE_OFF | ECHO_ON | FILTER_OFF);
				return;
				}

			if (xxgdb_command (command, FALSE))
				{
				delete_command();		/* remove xxgdb command from top of queue */
				command_sent--;
				/* tell gdb to send its prompt */
				query_gdb(" \n", PARSE_OFF | ECHO_ON | FILTER_OFF);
				return;
				}

			parse_flags = PARSE_ON | ECHO_ON | FILTER_ON; /* for read_gdb */
			write_dbx (command);
		    Prompt = False;
			return;					/* return with command_sent incremented */
			}
		else
			{
			if (command_sent)	/* should never be zero ! (just to be safe) */
				command_sent--;	/* command is not sent, restore previous value */
			return;
			}
		}
}


/*--------------------------------------------------------------------------+
|																			|
|	Function to filter xxgdb only commands									|
|																			|
|	input : command (from .gdbinit or source command or keyboard),			|
|			echo is TRUE if source command must be echoed.					|
|																			|
|	output : TRUE if source command was recognized							|
|																			|
|	In case source command is recognized, it is executed here.				|
|																			|
+--------------------------------------------------------------------------*/
int xxgdb_command (command, echo)
char *command;
int echo;
{
#ifdef CREATE_IO_WINDOW
	if (command && (!strcmp(command,"iowin\n")))	/* test for 'iowin' command */
		{
		if (echo)
			AppendDialogText(command);
		if (!iowinpid)
			{
			char ttycommand[50];
			create_io_window ();
			sprintf (ttycommand, "tty %s\n", iowintty);
			query_gdb (ttycommand, PARSE_OFF | ECHO_OFF | FILTER_OFF);
			}
		return TRUE;
		}
#endif /* CREATE_IO_WINDOW */
		
	return FALSE;
}

