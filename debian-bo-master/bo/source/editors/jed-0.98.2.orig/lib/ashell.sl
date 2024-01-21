% async shell for jed
!if (is_defined ("Shell_Default_Shell"))
{
   variable Shell_Default_Shell = "sh";
}

variable AShell_Id = -1; 

define ashell_send_input ()
{
   variable buf;
   variable this_line, mark_line;
   variable m, ch, prompt;
   
   m = process_mark (AShell_Id);
   
   this_line = what_line ();
   push_mark ();
   goto_user_mark (m);
   mark_line = what_line ();
   
   if (this_line >= mark_line)
     {
	pop_mark_0 ();
	push_mark_eob ();
	buf = bufsubstr ();
     }
   else
     {
	bskip_chars ("\t ");
	push_mark ();
	!if (bolp ())
	  {
	     go_left_1 ();
	     ch = what_char ();
	  }
	bol ();
	prompt = bufsubstr ();
	pop_mark_1 ();
	bol ();
	if (looking_at (prompt))
	  {
	     go_right (strlen (prompt));
	  }
	else if (ffind (char (ch)))
	  {
	     go_right_1 ();
	  }
	push_mark_eol ();
	buf = bufsubstr ();
	eob ();
	insert (buf);
     }
   newline ();
   move_user_mark (m);
   send_process (AShell_Id, strcat (buf, "\n"));
}

$1 = "AShellMap";
!if (keymap_p ($1))
{
   make_keymap ($1);
   definekey ("ashell_send_input", "^M", $1);
}

define ashell_signal_handler (pid, flags)
{
   eob ();
   if (flags == 2)
     {
	insert ("\n\n----- Process STOPPED ----------\n\n");
     }
   else if (flags == 4)
     {
     	insert ("\n\n----- Process EXITED ----------\n\n");
     }
   else if (flags == 8)
     {
	insert ("\n\n----- Process SIGNALLED ----------\n\n");
     }
   AShell_Id = -1;
}


 
define ashell ()
{
   variable buf = "*ashell*";
   
   if ((AShell_Id != -1) and bufferp (buf))
     {	
	error ("Currently, only one shell process is supported.");
     }
   
   pop2buf (buf);
   use_keymap ("AShellMap");
   erase_buffer ();
   AShell_Id = open_process (Shell_Default_Shell, "-i", 1);
   set_process (AShell_Id, "signal", "ashell_signal_handler");
}

   
