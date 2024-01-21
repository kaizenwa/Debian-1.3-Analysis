% More emacs functions.  This file is autoloaded upon demand.

define find_buffer_other_window ()
{  
   variable n, buf, trybuf = Null_String;
   variable ch;
   
   n = buffer_list();
   loop (n)
     { 
	buf = ();
	n--;
	ch = buf[0];
	if ((ch == ' ') or (ch == '*') or 
	    not(strcmp (whatbuf (), buf)))
	  continue;
	trybuf = buf;
	break;
     }
   loop (n) pop ();
   
   trybuf = read_with_completion ("Switch to buffer:",
				  trybuf, Null_String, 'b');

   if (strlen (trybuf)) pop2buf (trybuf);
}

define find_file_other_window ()
{
   variable file;

   file = read_file_from_mini ("Find file:");
   !if (strlen(extract_filename(file))) return;
   
   !if (read_file(file)) message ("New file.");
   pop2buf (whatbuf());
}

define find_alternate_file ()
{
   variable file;

   file = read_file_from_mini ("Find alternate file:");
   !if (strlen(extract_filename(file))) return;
   
   delbuf (whatbuf());
   !if (find_file (file)) message ("New file.");
}

define delete_blank_lines () 
{
   variable white = " \t\r\n";
   bskip_chars(white);
   eol_trim(); go_down_1(); eol_trim(); bol();
   if (eolp()) 
     {
	go_down_1();
	push_mark ();
	skip_chars (white);
	bol ();
	del_region ();
     }
}


define forward_sexp () 
{
   skip_chars(" \t\n");
   if (looking_at_char ('(')
       or looking_at_char ('{')
       or looking_at_char ('['))
     {
	if (find_matching_delimiter(what_char ()) == 1) 
	  go_right_1 ();
	return;
     }
   skip_chars("^ \t\n()[]{}");
}

define backward_sexp () 
{
   bskip_chars(" \t\n");
   go_left_1 ();
   if (looking_at_char (')')
       or looking_at_char ('}')
       or looking_at_char (']'))
     {
	() = find_matching_delimiter(0);
	return;
     }
   
   bskip_chars("^ \t\n()[]{}");
}

define kill_sexp () 
{
   variable kr = "yp_kill_region";
   push_mark();
   forward_sexp();
   go_right (eolp ());
   if (is_defined (kr)) eval (kr);
   else call ("kill_region");
}

define scroll_up_in_place ()
{
   variable m;
   m = window_line ();
   if (down_1 ()) recenter (m);
   bol ();
}

define scroll_down_in_place ()
{
   variable m;
   m = window_line ();
   if (up_1 ()) recenter (m);
   bol ();
}

