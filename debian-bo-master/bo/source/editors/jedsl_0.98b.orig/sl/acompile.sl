% Asynchronous compilation

variable Compile_Last_Compile_Cmd = Null_String;
variable Compile_Process_Id = -1;

define compile_set_status_line (state)
{
   set_mode (strcat ("compile", state), 0);
}

define compile_signal_handler (pid, flags)
{
   switch (flags)
     { case 2: " stopped"; }
     { case 4: " exited"; }
     { case 8: " signalled"; }
     {
	pop(); Null_String;
     }
   
   dup ();
   
   push_spot ();
   eob ();
   insert ("\n\nProcess nolonger running: ");
   insert (());
   newline ();
   pop_spot ();
   
   compile_set_status_line (());
   
   if (flags != 2) Compile_Process_Id = -1;
}

	
define compile_start_process ()
{
   variable cmd;
   variable dir, name, file, flags;
   
   cmd = read_mini ("Compile command:", Null_String, Compile_Last_Compile_Cmd);
   !if (strlen (cmd))
     return;

   (,dir,,) = getbuf_info ();
   if (change_default_dir (dir)) 
     error ("Unable to chdir.");
   
   pop2buf (Compile_Output_Buffer);

   set_readonly (0);
   erase_buffer ();
   (file,,name,flags) = getbuf_info ();
   setbuf_info (file, dir, name, flags);
   Compile_Line_Mark = 0;
   
   compile_set_status_line (Null_String);
   insert (cmd); newline ();
   
   Compile_Process_Id = open_process ("sh",
				      "-c", cmd,
				      2);
   if (Compile_Process_Id == -1)
     error ("Unable to start subprocess.");
   
   compile_set_status_line (" run");
   Compile_Last_Compile_Cmd = cmd;
   
   set_process (Compile_Process_Id, "signal", "compile_signal_handler");
   set_process (Compile_Process_Id, "output", "@");
}



define compile ()
{
   variable b, n;
   
   Compile_Output_Buffer = "*compile*";
   
   if (Compile_Process_Id != -1)
     {
	if (bufferp (Compile_Output_Buffer))
	  error ("A compile process is already running.");
	ERROR_BLOCK
	  {
	     _clear_error ();
	  }
	kill_process (Compile_Process_Id);
	Compile_Process_Id = -1;
     }
	
   
   b = whatbuf();
   call ("save_buffers");
   
   compile_start_process ();
   
   pop2buf(b);
   
   %compile_parse_errors ();
}
