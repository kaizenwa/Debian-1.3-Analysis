% sendmail.sl	-*- mode: slang; mode: fold -*-
%
% (Thanks to olesen@weber.me.queensu.ca (mj olesen) for this)
%
% Sendmail interface for Unix.
%
%  Functions: 
%   mail_send             : send message
%   mail                  : initiate mail mode
%   mail_insert_signature : append contents of Mail_Signature_File
%   mail_kill_buffer      : Delete mail buffer
%
%  Variables:
%   Mail_Reply_To         : Set this to appropriate Reply-To value
%   Mail_Signature_File   : Filename of signature (~/.signature is default)
%   SendMail_Cmd          : Name of sendmail program including switches
%   
% You might want something like the following in mail_hook
%
% define mail_hook ()
% {
%    local_setkey ("mail_send", "^C^C");
%    local_setkey ("mailalias_expand", "^C^E");
%    local_setkey ("mail_kill_buffer", "^Xk");
% }

autoload ("mailalias_expand", "mailalias"); % the mail-alias package

% Create a dummy function if set_line_readonly is not defined.
!if (is_defined ("set_line_readonly")) eval (".(pop) set_line_readonly");

%{{{ Public Variables 

$1 = "~/.signature";
!if (is_defined ("Mail_Signature_File"))
{
   variable Mail_Signature_File = expand_filename ($1);
}


% The sendmail program

$1 = _stkdepth ();
"";
"/usr/bin/sendmail";		% places to look
"/usr/lib/sendmail";
"/usr/sbin/sendmail";

!if (is_defined ("SendMail_Cmd"))
{
   variable SendMail_Cmd;

   while (SendMail_Cmd = (), strlen (SendMail_Cmd))
     {
	if (1 == file_status (SendMail_Cmd))
	  {
	     SendMail_Cmd = strcat (SendMail_Cmd, " -t -oem -oi -odb");
	     break;
	  }
     }
   
}

pop_n (_stkdepth () - $1);

!if (strlen (SendMail_Cmd)) error ("`sendmail' program not found!");

%!% always Reply-To: here instead.
!if (is_defined ("Mail_Reply_To"))
{
   variable Mail_Reply_To = Null_String;
}

%}}}

%{{{ Private Variables

variable Mail_Previous_Buffer = Null_String;
variable Mail_Previous_Windows = 1;
variable Mail_This_Buffer = Null_String;
variable Mail_Filename = dircat (getenv ("HOME"), ".__jed_mail__");

%}}}

%{{{ Private Functions

% format _jed_version xyyzz	into x.yy-zz
define string_jed_version ()
{
   Sprintf ("%d.%d-%d",
	    (_jed_version/10000),
	    ((_jed_version mod 10000)/100),
	    (_jed_version mod 100),
	    3);
}

define mail_sw2_prev_buf ()
{
   if (bufferp (Mail_Previous_Buffer)) sw2buf (Mail_Previous_Buffer);
   if (1 == Mail_Previous_Windows) onewindow ();
   if (bufferp (Mail_This_Buffer) and strcmp (Mail_This_Buffer, Mail_Previous_Buffer))
     pop2buf (Mail_This_Buffer);
}

%}}}

define mail_send () %{{{
{
   variable dir, file, sent, buf = "*mail*";

   flush ("Sending...");
   if (strcmp (buf, whatbuf ())) error ("not *mail* buffer");

   push_spot (); 
   mark_buffer ();
   sent = not (pipe_region (SendMail_Cmd));
   pop_spot ();
   
   if (sent)
     {
	set_buffer_modified_flag (0);
	flush ("Sending...done");
	(file, dir,,) = getbuf_info ();
	() = delete_file (make_autosave_filename (dir, file));
	file = dircat (dir, file);
	!if (strcmp (Mail_Filename, file))
	  () = delete_file (Mail_Filename);
	mail_sw2_prev_buf ();
	bury_buffer (buf);
	return;
     }

   flush ("Error sending message");
   beep ();
}
add_completion ("mail_send");

%}}}

define mail_format_buffer (erase) %{{{
{
   variable km = "mail_map";

   text_mode ();
   
   if (erase)
     {
	erase_buffer ();
	% better to use strncat ?
	insert ("To: \nCc: \nBcc: \nSubject: \n");
	if (strlen (Mail_Reply_To))
	  vinsert ("Reply-To: %s\n", Mail_Reply_To, 1);
	
	vinsert ("X-Mailer: Jed [%s]\n\n", string_jed_version (), 1);
	go_up (2);
	set_line_readonly (1);
	
	bob (); eol ();
	set_buffer_modified_flag(0);
     }

   set_buffer_undo(1);
   setbuf_info (getbuf_info () & ~(0x40)); %  turn off buried buffer flag
   
   !if (keymap_p(km)) make_keymap(km);
   use_keymap(km);
}

%}}}

define mail_kill_buffer () %{{{
{
   call ("kill_buffer");
   mail_sw2_prev_buf ();
}

%}}}

define mail () %{{{
{
   variable status, dir, file, buf = "*mail*";
   variable do_format = 1;
   
   status = bufferp (buf);
   Mail_Previous_Windows = nwindows ();
   Mail_This_Buffer = whatbuf ();
   
   if (BATCH)
     {
	% Mail_Previous_Buffer = whatbuf ();
	sw2buf (buf);
     }
   else
     {
	Mail_Previous_Buffer = pop2buf_whatbuf (buf);
     }

   % if buffer is not old, turn autosave on
   if (status)
     {
	(,,,status) = getbuf_info ();
	if (status & 1) return;
     }
   else
     {
	(dir, file) = parse_filename (Mail_Filename);
	setbuf_info (file, dir, buf, 2);	% autosave
	
	file = make_autosave_filename(dir, file);
	if (1 == file_status (file))
	  {
	     if (get_yes_no ("An autosave file exists.  Use it"))
	       {
		  erase_buffer ();
		  () = insert_file (file);
		  do_format = 0;
	       }
	  }
     }

   mail_format_buffer (do_format);
   push_spot ();
   runhooks("mail_hook");
   pop_spot ();
}

%}}}

define mail_insert_signature () %{{{
{
   !if (strlen (Mail_Signature_File))
     return;
   
   push_spot ();
   eob ();
   () = insert ("\n-- \n");
   () = insert_file (Mail_Signature_File);
   pop_spot ();
}

%}}}

