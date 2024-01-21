% This is a text-macro mode designed to edit text using user defined macros.

%!% Prototype: (file, ext) = filename_extension (filename);
%!% This function breaks @filename@ into two components @file@ and @ext@.
%!% The latter component, @ext@ is the extension of @filename@.  The other
%!% piece, @file@ represents everything else.  The original filename may
%!% be reconstructed by simply concatenating @file@ and @ext@.
%!% Related functions: @parse_filename@
define filename_extension (file_with_ext)
{
   variable ext, last_ext, file;
   variable n, dir;
   
   
   (dir, file_with_ext) = parse_filename (file_with_ext);
   
   file = strncat (dir, extract_element (file_with_ext, 0, '.'), ".", 3);
   
   n = 1;
   last_ext = Null_String;
   while (ext = extract_element (file_with_ext, n, '.'), strlen (ext))
     {
	if (strlen (last_ext)) file = strncat (file, last_ext, ".", 3);
	last_ext = ext;
	n++;
     }
   
   return (file, last_ext);
}


define textmac_is_defined (fname)
{
   variable name, def, num_args;
   
   push_spot_bob ();
   EXIT_BLOCK
     {
	pop_spot ();
     }
   
   name = Sprintf ("#d %s ", fname, 1);
   if (bol_fsearch (name))
     {
	go_right (strlen (name));
	num_args = 0;
     }
   else if (name = Sprintf ("#d %s#", fname, 1), bol_fsearch (name))
     {
	go_right (strlen (name));
	num_args = what_char () - '0';
	go_right_1 ();
     }
   else 
     {
	% Check for internal
	switch (fname)
	  {
	   case "__today__":
	     return (time (), 0, 1);
	  }
	  {
	   case "__btrim__":
	     goto_spot ();
	     push_mark ();
	     bskip_chars ("\n\t ");
	     del_region ();
	     return (Null_String, 0, 1);
	  }
	  {
	   case "__newline__":
	     return ("\n", 0, 1);
	  }
	
	if ((strlen (fname) != 7)
	    or strncmp (fname, "__eval", 6))
	  return 0;
	
	 if (not (isdigit (char (fname[6]))))
	  return 0;
	
	Null_String;
	-(fname[6] - '0');		       %  neg value indicates user defined.
	1;
	return;
     }
   
   skip_white ();
   push_mark_eol ();
   def = bufsubstr ();
   
%   if (def[0] != '{')
%     def = Sprintf ("{%s}", def, 1);

   def;
   num_args;
   1;
}

define textmac_do_user_fun (argv, argc)
{
   variable fname = argv[1];
   variable i;
   
   if (is_defined (fname) <= 0)
     {
	vmessage ("Undefined function: %s.", fname, 1);
	return;
     }
   _for (2, argc - 1, 1)
     {
	i = ();
	argv[i];
     }
   eval (fname);
}


define textmac_do_function (argv, argc)
{
   variable i;
   bob ();
   while (fsearch_char ('$'))
     {
	del ();
	i = what_char () - '0';
	if ((i > 0) and  (i < argc))
	  {
	     del ();
	     insert (argv[i]);
	  }
     }
}



define textmac_parse_buffer ()
{
   variable fname, argc, argv, num_args;
   
   argv = create_array ('s', 10, 1);
   
   bob ();
   
   while (fsearch ("\\"))
     {
	push_spot_bol ();

	if (looking_at_char ('#'))
	  {
	     pop_spot ();
	     eol ();
	     continue;
	  }
	
	pop_spot ();
	
	% Now look for special forms
	if (looking_at ("\\\\"))
	  {
	     go_right (2);
	     continue;
	  }
	if (looking_at ("\\{"))
	  {
	     go_right (2);
	     continue;
	  }
	if (looking_at ("\\}"))
	  {
	     go_right (2);
	     continue;
	  }
	
	
	del ();			       %  nuke \\
	
	push_mark ();
	POINT;
	skip_chars ("-a-zA-Z_0-9");
	if (() == POINT)
	  {
	     go_right (1);
	  }
	fname = bufsubstr_delete ();
	
	push_mark ();		       %  we will return here for expansion
	
	if (textmac_is_defined (fname))
	  {
	     num_args = ();
	     insert (());
	  }
	else
	  {
	     vmessage ("Reference to %s undefined.", fname, 1);
	       % undefined--- put it back
	     pop_mark_1 ();
	     insert ("\\"); insert (fname);
	     continue;
	  }
	
	
	% Now get arguments
	push_mark ();		       %  mar for del_region below
	!if (num_args)
	  {
	     if (looking_at ("{}")) del(2);
	  }
	
	argc = 1;
	loop (abs(num_args))
	  {	     
	     skip_chars ("\n\t ");
	     !if (looking_at_char ('{'))
	       {
		  verror ("%d arguments required for %s.", num_args, fname, 2);
	       }
	     
	     push_mark ();
	     if (1 != find_matching_delimiter (0))
	       {
		  pop_mark_1 ();
		  verror ("Unable to find closing '}' for %s", fname, 1);
	       }
	     del ();		       %  del }
	     exchange_point_and_mark ();
	     del ();		       %  del {
	     exchange_point_and_mark ();
	     
	     argv[argc] = bufsubstr ();
	     argc++;
	  }
	
	del_region ();
	
	% Ok, we now have argc functions on the stack.
	if (num_args)
	  {
	     narrow_to_region ();
	     if (num_args < 0)
	       textmac_do_user_fun (argv, argc);
	     else
	       textmac_do_function (argv, argc);
	     bob ();
	     widen_region ();
	  }
	else pop_mark_1 ();
     }
}

!if (is_defined ("TextMac_Verbatum_Begin"))
{
   variable TextMac_Verbatum_Begin = "<PRE>";
}

!if (is_defined ("TextMac_Verbatum_End"))
{
   variable TextMac_Verbatum_End = "</PRE>";
}

!if (is_defined ("TextMac_Include_Dirs"))
{
   variable TextMac_Include_Dirs = ".";
}

$1 = "HOME";
$2 = "tm";
$3 = ",";
loop (strchopr (JED_LIBRARY, ',', 0))
{
   $4 = ();
   TextMac_Include_Dirs = strncat (TextMac_Include_Dirs, $3, dircat ($4, $2), 3);
}

	

define textmac_include_file (file)
{
   variable dir, dirfile;
   variable n;
   
   push_spot ();
   ERROR_BLOCK
     {
	pop_spot ();
     }
   
   
   if (strncmp (file, "/", 1)
	and strncmp (file, "./", 2)
	and strncmp (file, "../", 3))
     {
	n = 0;
	dirfile = Null_String;
	while (dir = extract_element (TextMac_Include_Dirs, n, ','),
	       strlen (dir))
	  {
	     n++;
	     dirfile = dircat (dir, file);
	     if (1 == file_status (dirfile))
	       break;
	  }
	file = dirfile;
     }
   
   
   if (-1 == insert_file (file))
     {
	verror ("Unable to insert %s", file, 1);
     }
   
   EXECUTE_ERROR_BLOCK;
}

define textmac_preprocess_buffer ()
{
   variable token;
   variable file, cs;
   
   bob ();
   while (bol_fsearch ("#i "))
     {
	go_right (2);
	skip_white ();
	push_mark_eol ();
	file = strtrim (bufsubstr ());
	
	!if (strlen (file)) continue;

	delete_line ();
	textmac_include_file (file);
     }
   
   % Now join continuation lines
   bob ();
   while (bol_fsearch_char ('#'))
     {
	while (not (eobp) and ffind ("\\\n"))
	  {
	     del(); del ();
	  }
	eol ();
     }
   
   cs = CASE_SEARCH;
   CASE_SEARCH = 0;
   
   % delete commented sections
   bob ();
   while (bol_fsearch ("#%+"))
     {
	push_mark ();
	if (bol_fsearch ("#%-"))
	  {
	     eol ();
	     del_region ();
	  }
	else 
	  {
	     pop_mark (1);
	     error ("Unterminated #%+");
	  }
     }

   % Mark verbatim environments
   bob ();
   while (bol_fsearch ("#v+"))
     {
	while (down (1) and not (looking_at ("#v-")))
	  {
	     insert ("#v ");
	  }
     }

   CASE_SEARCH = cs;
}


define textmac_trim_excess ()
{
   bob ();
   forever
     {
	trim ();
	if (eolp ()) 
	  {
	     delete_line ();
	     if (eobp ()) break;
	  }
	else 
	  {
	     eol_trim ();
	     !if (down_1 ()) break;
	  }
     }
}

define textmac_clean_buffer (do_trim)
{
   variable v, cs;
   
   bob ();
   while (bol_fsearch_char ('#'))
     {
	del ();
	switch (what_char ())
	  {
	   case 'd':
	     delete_line ();
	  }
	  {
	   case '%':
	       delete_line ();
	  }
	  {
	   case 'c':
	     del ();
	     insert ("<!-- ");
	     eol ();
	     insert (" -->");
	  }
	  {
	   case '-':
	     insert ("<HR>\n");
	     delete_line ();
	  }
	  {
	     pop ();
	     insert_char ('#');
	  }
     }
   
   % Now expand the escaped characters
   bob ();
   while (fsearch_char ('\\'))
     {
	push_spot_bol ();
	if (looking_at_char ('#'), pop_spot ())
	  {
	     eol ();
	     continue;
	  }
	if (orelse
	    {looking_at ("\\}")}
	    {looking_at ("\\\\")}
	    {looking_at ("\\{")}
	    )
	  del ();
	go_right_1 ();
     }
   
   if (do_trim) textmac_trim_excess ();
   
   % Now remove verbatim marks
   bob ();
   cs = CASE_SEARCH;
   CASE_SEARCH = 0;
   
   while (bol_fsearch ("#v"))
     {
	del (); 
	v = what_char () - 'V';
	del ();
	if (what_char == '+')
	  {
	     delete_line ();
	     if (v) insert (TextMac_Verbatum_Begin);
	     newline ();
	  }
	else if (what_char == '-')
	  {
	     delete_line ();
	     if (v) insert (TextMac_Verbatum_End);
	     newline ();
	  }
	else if (not (eolp))
	  del ();		       %  The trim may have removed space
     }
   CASE_SEARCH = cs;
   bob ();
}



% Create a syntax table.  Basically \ is a quote and {} are matching delimiters.
$1 = "textmac-mode";
create_syntax_table ($1);

%define_syntax ("#%", "", '%', $1);       % Comment Syntax
define_syntax ('\\', '\\', $1);         % Quote character
define_syntax ("{", "}", '(', $1);    % nothing else matches
%define_syntax ('$', '"', $1);           %  string
%define_syntax ("~^_&#", '+', $1);      %  operators
%define_syntax ("|&{}[]", ',', $1);      %  delimiters
define_syntax ("-a-zA-Z_0-9", 'w', $1);
define_syntax ('#', '#', $1);
set_syntax_flags ($1, 8);
   

define textmac_paragraph_separator ()
{
   bol (); 
   if (looking_at_char ('#')) return 1;
   skip_white ();
   eolp () or looking_at ("\\");
}

define textmac_wrap_hook ()
{
   push_spot ();
   go_up_1 ();			       %  at eol
   trim ();
   bol ();
   
   if (looking_at ("#%"))
     {
	go_down_1 ();
	insert ("#% ");
	pop_spot ();
	return;
     }
   
   if (looking_at_char ('#'))
     {
	eol ();
	!if (blooking_at ("\\"), insert_single_space ())
	  insert_char ('\\');
	go_down_1 ();
	bol_trim ();
     }
   else go_down (1);
   
   bol_skip_white ();
   if (bolp ()) insert_single_space ();
   pop_spot ();
}

   
define textmac_mode ()
{
   variable mode = "text-macro";
   variable texmode = "textmac-mode";
   set_mode (mode, 0x1 | 0x20);
   set_buffer_hook ("par_sep", "textmac_paragraph_separator");
   set_buffer_hook ("wrap_hook", "textmac_wrap_hook");
   use_syntax_table (texmode);
   runhooks ("textmac_mode_hook");
   % This is called after the hook to give the hook a chance to load the
   % abbrev table.
   if (abbrev_table_p (mode)) use_abbrev_table (mode);
}


define textmac_process_buffer (remove_excess)
{
   variable cs = CASE_SEARCH;
   variable cbuf, html_buf;
   
   cbuf = whatbuf ();

   CASE_SEARCH = 1;
   ERROR_BLOCK
     {
	CASE_SEARCH = cs;
     }
   
   html_buf = strcat ("*textmac*", cbuf);
   
   if (BATCH) sw2buf (html_buf);
   else pop2buf (html_buf);
   
   erase_buffer ();
   textmac_mode ();
   insbuf (cbuf);
   bob ();
   
   textmac_preprocess_buffer ();
   textmac_parse_buffer ();
   textmac_clean_buffer (remove_excess);
   bob ();
   EXECUTE_ERROR_BLOCK;
}

define textmac_to_linuxdoc ()
{
   TextMac_Verbatum_Begin = "<tscreen><verb>";
   TextMac_Verbatum_End = "</verb></tscreen>";
   textmac_process_buffer (0);
}

define textmac_to_html ()
{
   TextMac_Verbatum_Begin = "<pre>";
   TextMac_Verbatum_End = "</pre>";
   textmac_process_buffer (1);
}
