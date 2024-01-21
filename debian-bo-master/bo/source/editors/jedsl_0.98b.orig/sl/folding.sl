% -*- mode: slang; mode: fold -*-


!if (is_defined ("Fold_Bob_Eob_Error_Action"))
{
%!% Prototype: Integer Fold_Bob_Eob_Error_Action = 1;
%!% This value of this variable determines the what will happen upon
%!% reaching the boundary of the current fold via the up/down keys.
%!% If the value is 0, an error will be generated; if the value is 1, 
%!% the fold will be exited; otherwise, the next/previous fold will be
%!% entered. 
variable Fold_Bob_Eob_Error_Action = 1;
}


%{{{ Associating fold marks with modes
% This variable consists of MODE\rSTART\rEND\rBEG1\r\END1\n objects
% I should make these regular expression objects but I am not going to
% bother with that kind of complexity at the moment.
variable Fold_Mode_Mark_List = "\n";

define fold_add_mode_marks (mode, beg, end, beg1, end1)
{
   Fold_Mode_Mark_List = Sprintf ("\n%s\r%s\r%s\r%s\r%s%s", 
				  mode, beg, end, beg1, end1,
				  Fold_Mode_Mark_List, 6);
}

%{{{ Fold marks for some modes
fold_add_mode_marks ("SLang", "%{{{", "%}}}", Null_String, Null_String);
fold_add_mode_marks ("C", "/*{{{", "/*}}}", "*/", "*/");
fold_add_mode_marks ("Fortran", "C{{{", "C}}}", Null_String, Null_String);
fold_add_mode_marks ("TeX", "%{{{", "%}}}", Null_String, Null_String);
fold_add_mode_marks ("score", "%{{{", "%}}}", Null_String, Null_String);
fold_add_mode_marks ("SH", "#{{{", "#}}}", Null_String, Null_String);
%}}}

define fold_get_marks_for_mode () %{{{
{
   variable m, pos, list;
   
   (m, ) = what_mode ();
   m = Sprintf ("\n%s\r", m, 1);
   
   pos = is_substr (Fold_Mode_Mark_List, m);
   !if (pos)   
     return ("{{{ ", "}}}", Null_String, Null_String);
   
   pos += strlen (m);
   
   () = strchop (extract_element (substr (Fold_Mode_Mark_List, pos, strlen (Fold_Mode_Mark_List)), 
				  0, '\n'),
		 '\r', 0);
}

%}}}

define fold_get_marks () %{{{
{
#ifdef HAS_BLOCAL_VAR
   get_blocal_var ("fold_start");
   get_blocal_var ("fold_end");
   get_blocal_var ("fold_end_of_start");
   get_blocal_var ("fold_end_of_end");
#else
   fold_get_marks_for_mode ();
#endif
}

%}}}

%}}}

%{{{ Basic functions:

define fold_is_marker_line (start, end_of_start) %{{{
{
   bol_skip_white ();
   if (looking_at (start))
     {
	return 1;
     }
   eol ();
   !if (bfind (start))
     {
	return 0;
     }

   go_right (strlen (start));
   skip_white ();
   end_of_start = strtrim (end_of_start);
   !if (looking_at (end_of_start))
     return 0;
   go_right (strlen (end_of_start));
   skip_white ();
   eolp ();
}

%}}}
define fold_find_marker_line (start, end_of_start) %{{{
{
   bol ();
   while (fsearch (start))
     {
	if (fold_is_marker_line (start, end_of_start))
	  return 1;
	eol ();
     }
   return 0;
}

%}}}
define fold_find_marker_line_reverse (start, end_of_start, hidden_check) %{{{
{
   eol ();
   while (bsearch (start))
     {
	if (fold_is_marker_line (start, end_of_start))
	  {
	     !if (hidden_check and is_line_hidden ())
	       return 1;
	  }
	bol ();
     }
   return 0;
}
%}}}
define fold_this_fold (start, end, end_of_start, end_of_end, start_level) %{{{
{
   variable level = start_level;
   
   while (down_1 ())
     {
   	set_line_hidden (level);
	
	if (fold_is_marker_line (start, end_of_start)) level++;
	else if (fold_is_marker_line (end, end_of_end)) 
	  {
	     if (level == start_level) break;
	     level--;
	  }
     }
}

%}}}
define fold_open_buffer () %{{{
{
   push_spot ();
   widen_buffer ();
   mark_buffer ();
   set_region_hidden (0);
   pop_spot ();
}

%}}}
define fold_whole_buffer () %{{{
{
   variable start, end, end_of_start, end_of_end;
   variable level;
   
   flush ("folding buffer...");
   push_spot ();
   
   fold_open_buffer ();
   
   bob ();
   (start, end, end_of_start, end_of_end) = fold_get_marks ();

   while (fold_find_marker_line (start, end_of_start))
     {
	fold_this_fold (start, end, end_of_start, end_of_end, 1);
     }
   pop_spot ();
   skip_hidden_lines_backward (1);
}

%}}}
define fold_is_fold (start, end_of_start) %{{{
{
   push_spot ();
   EXIT_BLOCK
     {
	pop_spot ();
     }
   
   !if (fold_is_marker_line (start, end_of_start))
     return 0;
   
   % Check to make sure this is not the top of the current fold by making
   % sure that the next line is hidden.
   !if (down_1 ()) return 0;
   return is_line_hidden ();
}

%}}}
define fold_open_fold () %{{{
{
   variable start, end, end_of_start, end_of_end;
   
   (start, end, end_of_start, end_of_end) = fold_get_marks ();
   push_spot ();
   if (is_line_hidden ())
     skip_hidden_lines_backward (1);
   if (fold_is_fold (start, end_of_start))
     {
	fold_this_fold (start, end, end_of_start, end_of_end, 0);
     }
   pop_spot ();
}

%}}}
define fold_enter_fold () %{{{
{
   variable start, end, end_of_start, end_of_end;
   variable h;
   
   (start, end, end_of_start, end_of_end) = fold_get_marks ();
   
   push_spot ();
   
   while (fold_find_marker_line_reverse (start, end_of_start, 1))
     {
	push_mark ();
	if (fold_is_fold (start, end_of_start))
	  {
	     fold_this_fold (start, end, end_of_start, end_of_end, 0);
	     narrow ();
	     bob ();
	  }
	else 
	  {
	     pop_mark_1 ();
	     break;
	  }
	goto_spot ();
	!if (is_line_hidden ())
	  break;
     }
   
   pop_spot ();
}

%}}}

define fold_close_this_fold () %{{{
{
   variable start, end, end_of_start, end_of_end;
   
   (start, end, end_of_start, end_of_end) = fold_get_marks ();
   
   !if (fold_find_marker_line (start, end_of_start))
     error ("Unable to find fold-start");
   
   fold_this_fold (start, end, end_of_start, end_of_end, 1);
   skip_hidden_lines_backward (1);
}
%}}}
define fold_close_fold () %{{{
{
   variable start, end, end_of_start, end_of_end;
   variable beg_mark, end_mark, orig_mark;
   variable not_in_a_fold = "Not in a fold.";
   variable end_line;
   
   (start, end, end_of_start, end_of_end) = fold_get_marks ();
   
   orig_mark = create_user_mark ();
   
   ERROR_BLOCK
     {
	goto_user_mark (orig_mark);
     }
   
   EXIT_BLOCK
     {
	fold_this_fold (start, end, end_of_start, end_of_end, 1);
	skip_hidden_lines_backward (1);
     }
   
   if (fold_is_marker_line (start, end_of_start))
     {
	!if (down_1())
	  return;
	is_line_hidden();
	goto_user_mark (orig_mark);
	!if (())
	  return;
     }
 
   beg_mark = create_user_mark ();
   
   if (fold_is_marker_line (end, end_of_end))
     go_up_1 ();
   
   end_mark = create_user_mark ();

   forever
     {
	goto_user_mark (end_mark);
	
	end_line = 0;
	
	if (fold_find_marker_line_reverse (end, end_of_end, 0))
	  {
	     if (up_1 ())
	       end_line = what_line ();
	     
	     move_user_mark (end_mark);
	  }
	
	goto_user_mark (beg_mark);
	
	!if (up_1 ()) break;
	if (fold_find_marker_line_reverse (start, end_of_start, 0))
	  {
	     if (not(end_line)
		 or (what_line () > end_line))
	       break;
	     move_user_mark (beg_mark);
	  }
	else error (not_in_a_fold);
     }
}

%}}}

define fold_exit_fold () %{{{
{
   !if (count_narrows ())
     {
	error ("You are not in a fold.");
	return;
     }
   
   bob ();
   widen ();
   
   fold_close_this_fold ();
   recenter (window_info ('r') / 2);
}

%}}}

define fold_fold_region () %{{{
{
   variable start, end, end_of_start, end_of_end;

   check_region (0);
   (start, end, end_of_start, end_of_end) = fold_get_marks ();
   
   % We have a canonical region with point at end.  See if this line
   % is the start of a fold.  If so, extend it to cover all of fold.
   
   if (fold_is_fold (start, end_of_start))
     {
	skip_hidden_lines_forward (1);
	!if (is_line_hidden ())
	  go_up_1 ();
     }
   
   narrow ();
   eob ();
   newline ();
   insert (end);
   insert (end_of_end);
   newline ();

   bob ();
   % Now look at position of beginning of region.  If it does not occur on
   % a blank line, put fold marks at end of line.
   skip_white ();
   if (eolp ())
     {
	bol ();
	insert (start);
	push_spot ();
	insert (end_of_start);
	newline ();
     }
   else
     {
	eol ();
	trim ();
	insert_single_space ();
	insert (start);
	insert (end_of_start);
	bol ();
	push_spot ();
     }

   fold_exit_fold ();
   pop_spot ();
}

%}}}

%}}}

define fold_parse_errors () %{{{
{
   variable folded;
   
   % compile_parse_errors will widen buffer the buffer.  As a result, when it
   % returns, the buffer will be unfolded but some lines may be hidden.  Simply
   % unhide all lines.  Also take care to reenter a fold only if buffer is not
   % folded.
   compile_parse_errors ();
   
   push_spot ();
   eob ();
   skip_hidden_lines_backward (0);
   folded = is_line_hidden ();
   pop_spot ();
   
   
   if (folded)
     {
	push_spot ();
	fold_open_buffer ();
	fold_whole_buffer ();
	pop_spot ();
	if (is_line_hidden ()) fold_enter_fold ();
     }
}

%}}}

define fold_bob_eob_error_hook (f) %{{{
{
   variable str = "Top Of Buffer.";
   variable start, end, end_of_start, end_of_end;
   
   if (f > 0) str = "End Of Buffer.";

   !if (Fold_Bob_Eob_Error_Action)
     error (str);
   
   !if (count_narrows () and (abs(f) == 1)) error (str);
   
   fold_exit_fold ();
   
   % The rest of this function is should be made optional, e.g.,
   % if (Optional_Flag) return;

   if (Fold_Bob_Eob_Error_Action == 1)
     return;

   bol ();
   if (f > 0)
     {
	skip_hidden_lines_forward (1);
	skip_chars (" \t\n");
     }
   else 
     {
	bskip_chars (" \t\n");
	skip_hidden_lines_backward (1);
     }
   
   (start, end, end_of_start, end_of_end) = fold_get_marks ();
   
   if (fold_is_fold (start, end_of_start))
     fold_enter_fold ();
}

%}}}

%{{{ mouse interface 
$1 = "mouse_goto_position";
$2 = "mouse";
!if (is_defined ($1)) autoload ($1, $2);

define fold_mouse_2click (line, col, but, shift)
{
   variable start, end, end_of_start, end_of_end;
   if (but == 1)
     {
	(start, end, end_of_start, end_of_end) = fold_get_marks ();
	mouse_goto_position (col, line);
	
	ERROR_BLOCK
	  {
	     _clear_error ();
	  }
	if (fold_is_fold (start, end_of_start))
	  fold_enter_fold ();
	else
	  fold_exit_fold ();
	
	return 1;
     }
   
   return -1;
}

%}}}

%{{{ Interactive searching functions 

define fold_search_line_ok ()
{
   not (is_line_hidden ());
}

$1 = "search_generic_search";
$2 = "search";
!if (is_defined ($1)) autoload ($1, $2);

define fold_search_backward ()
{
   search_generic_search ("Fold search backward:", -1, &fold_search_line_ok);
}

define fold_search_forward ()
{
   search_generic_search ("Fold search forward:", 1, &fold_search_line_ok);
}

%}}}

define folding_mode () %{{{
{
   variable s, s1, e, e1;
   
#ifdef HAS_BLOCAL_VAR
   (s, e, s1, e1) = fold_get_marks_for_mode ();
   
   define_blocal_var ("fold_start", 's', s);
   define_blocal_var ("fold_end_of_start", 's', s1);
   define_blocal_var ("fold_end", 's', e);
   define_blocal_var ("fold_end_of_end", 's', e1);
#endif
   
   local_setkey ("fold_whole_buffer",		"^C^W");
   local_setkey ("fold_enter_fold",		"^C>");
   local_setkey ("fold_exit_fold",		"^C<");
   local_setkey ("fold_open_buffer",		"^C^O");
   local_setkey ("fold_open_fold",		"^C^S");
   local_setkey ("fold_close_fold",		"^C^X");
   local_setkey ("fold_fold_region",		"^C^F");
   local_setkey ("fold_search_forward",		"^Cf");
   local_setkey ("fold_search_backward",	"^Cb");
   
   set_buffer_hook ("bob_eob_error_hook", "fold_bob_eob_error_hook");
   set_buffer_hook ("mouse_2click", "fold_mouse_2click");
   
   loop (which_key ("compile_parse_errors"))
     {
	local_setkey ("fold_parse_errors", exch ());
     }
   
   fold_whole_buffer ();

   runhooks ("fold_mode_hook");
}
%}}}
