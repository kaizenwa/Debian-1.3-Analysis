%
%  Miscellaneous functions for C mode:
%
%     c_make_comment ()
%     c_format_paragraph ()
%     c_comment_region ()
%     c_top_of_function ()
%     c_end_of_function ()
%     c_mark_function ()



% produce a comment for C---
define c_make_comment ()
{
   variable d, cbeg, cend, file, mode;
   cbeg = "/*";
   cend = "*/";
   
   (, mode) = what_mode ();
   !if (mode & 2) return;
   
   (file,,,) = getbuf_info();
   
   if (mode & 8) 
     {
	cbeg = "%";
	cend = Null_String;
     }
   

   
   %% search for a comment on the line
   eol();
   if (bfind(cbeg))
     {
	!if (bolp())
	  {
	     go_left_1 ();
	     trim(); 
	     () = ffind(cbeg); 
	  }
	d = C_Comment_Column - what_column;
	if (d > 0) whitespace(d);
	!if (ffind(cend))
	  {
	     eol();
	     insert_spaces (2);
	     insert (cend);
	  }
	
	bfind(cbeg); pop();
	go_right(strlen(cbeg) + 1);
     }
   else				       %/* not found */
     {
	if (what_column() <= C_Comment_Column)
	  {
	     goto_column(C_Comment_Column);
	  }
	else insert("   ");
	insert(cbeg); insert("  "); 
	if (strlen(cend))
	  {
	     insert (cend);
	     go_left(3);
	  }
     } 
}


% This routine formats a comment in C mode.  It is assumed that the standard
% Jed comment format is used.
variable Cmode_Fill_Chars = Null_String;
define c_paragraph_sep ()
{
   if (strlen (Cmode_Fill_Chars)) return 0;
   push_spot ();
   bol_skip_white ();
   if (looking_at ("* "))
     {
	go_right (2);
	skip_white ();
	if (looking_at ("@ ")) eol ();
	eolp ();
     }
   else 1;
   pop_spot ();
}

define c_format_paragraph ()
{
   variable n, dwrap;
   % !if (is_c_mode ()) return;
   Cmode_Fill_Chars = Null_String;
   if (c_paragraph_sep ()) return;
   push_spot (); push_spot (); push_spot ();
   while (not(c_paragraph_sep ()))
     {
	!if (up_1 ()) break;
     }
   if (c_paragraph_sep ()) go_down_1 ();
   push_mark ();
   pop_spot ();
   
   while (not(c_paragraph_sep ()))
     {
	!if (down_1 ()) break;
     }
   if (c_paragraph_sep ()) go_up_1 ();
   narrow ();
   pop_spot ();
   bol ();
   push_mark ();
   skip_white ();
   if (looking_at ("* ")) go_right (2);
   Cmode_Fill_Chars = bufsubstr ();
   dwrap = what_column ();
   bob ();
   do 
     {
	bol_trim ();
	if (looking_at ("* ")) deln (2);
     }
   while (down_1 ());
   WRAP -= dwrap;
   call ("format_paragraph");
   WRAP += dwrap;
   bob ();
   do 
     {
	insert (Cmode_Fill_Chars);
     }
   while (down_1 ());
   Cmode_Fill_Chars = Null_String;
   widen ();
   pop_spot ();
}
 
define c_comment_region ()
{
   variable cbeg = "/* ", cmid = " * ", cend = " */";
   variable c, c1, celm, extra;
   
   check_region (1);
   exchange_point_and_mark ();
   c = what_column ();
   
   narrow ();
   bob (); 

   USER_BLOCK0 
     {
	=extra; =celm;
	bol_skip_white ();
	c1 = what_column ();
	if (c1 > c)
	  {
	     goto_column (c);
	     insert (celm);
	     trim ();
	     whitespace (c1 - what_column () + extra);
	  }
	else 
	  {
	     if (eolp ()) goto_column (c);
	     insert (celm);
	  }
     }
   
   X_USER_BLOCK0 (cbeg, 0);
   
   while (down_1 ())
     {
	X_USER_BLOCK0 (cmid, 1);
     }
   widen ();
   bol_skip_white ();
   if (looking_at(cmid))
     {
	deln (3);
     }
   else 
     {
	eol ();
	!if (right(1)) newline ();
     }
   X_USER_BLOCK0 (cend, 0);
   pop_spot ();
}

define c_top_of_function ()
{
   !if (bol_bsearch_char ('{'))
     {
	error ("Top of function not found.");
     }
}

   
define c_end_of_function ()
{
   !if (bolp () and looking_at_char ('{'))
     c_top_of_function ();
   call ("goto_match");
}

define c_mark_function ()
{
   c_end_of_function ();
   push_visible_mark ();
   eol ();
   go_down_1 ();
   exchange_point_and_mark ();
   c_top_of_function ();
   go_up(2);
   bol ();
}

   
     
	
	
