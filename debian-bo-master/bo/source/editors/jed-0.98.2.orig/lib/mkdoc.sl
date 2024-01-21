_traceback = 1;
% mkdoc.sl ---- load this file from JED_ROOT/lib!!!!
%
%  Extract documentation from .sl and .c files
%  For this to work, a strict format must be followed.
%  In .c files, add_intrinsic and add_variable statements are checked. The
%  syntax prototype is:
%
%        MAKE_INTRINSIC("name", function, TYPE, n)%
%        /* This is a function called name which returns TYPE
%           and takes n arguments. */
%
%  Note that it is assummed that comments cannot be nested.
%  For .sl files, the syntax follows:
%
%     %!% This function pops an integer n off the stack and an object s.
%     %!% It returns n copies of s on the stack.
%     define dupn (n, s)
%     {
%        loop (n) { s; } 
%     }
%
%  Here I search for '%!%' in column 1 which make indicate a doc string and 
%  then search for 'define' preceed by only whitespace.  The '%!%' must start in 
%  column 1.
%
%  There is no ambiguity as long as the doc strings are placed BEFORE the 
%  object they describe.


%!% Extracts doc string from FILE and appends it to BUFFER.
%!% Returns number of items processed
define mkdoc_process_sl_file (file, doc_buf)
{
   variable cbuf, doc_ind = "%!%", chars = "a-zA-Z0-9_$!";
   variable c, ctrla = 1, name;
   variable has_proto;
   variable num;
   
   !if (read_file(file)) return -1;
   cbuf = whatbuf ();

   bob ();

   setbuf (doc_buf);
   eob ();
   !if (bolp()) newline ();
   insert_char (ctrla); insert (file); newline ();
   push_spot ();
   setbuf (cbuf);
   
   num = 0;
   while (bol_fsearch (doc_ind))
     {
	setbuf (doc_buf);
	eob ();
	!if (bolp()) newline ();
	push_spot ();
	setbuf (cbuf);

	go_right (3); push_mark ();
	skip_white (); 
	has_proto = looking_at ("Prototype: ");
	while (down_1 () and looking_at (doc_ind));
	
	copy_region (doc_buf);
	
	% now look for what this documents
	forever 
	  {
	     bol (); skip_chars (" \t\n"); c = what_char ();
	     if (looking_at ("variable"))
	       {
		  go_right (strlen("variable"));
		  skip_white (); push_mark ();
		  skip_chars (chars);
		  name = Sprintf ("V%s ", bufsubstr(), 1);
		  break;
	       }
	     
	     if (looking_at ("define"))
	       {
		  go_right (strlen ("define"));
		  skip_white ();
		  push_mark ();
		  skip_chars (chars);
		  !if (has_proto) if (ffind_char (')')) go_right_1 ();
		  name = strcat ("F", bufsubstr ());
		  break;
	       }
	     

	     !if (down_1 ()) error (strcat ("Error parsing ", file));
	  }
	
	%% now we have the name with the appropriate prefix.  Insert it and
	%% replace newlines with ctrla
	setbuf (doc_buf);
	pop_spot (); insert (name);
	insert_char (ctrla);
	while (eol, not(eobp()))
	  {
	     del (); insert_char (ctrla);
	     if (looking_at (doc_ind)) deln (3);
	  }
	
	newline ();
	setbuf (cbuf);
	num++;
     }
   
   return num;
}

%% same routine for C files.  Looks for MAKE_INTRINSIC and MAKE_VARIABLE
%% macros.
define mkdoc_process_c_file (file, doc_buf)
{
   variable cbuf, name, m;
   variable f, v, ctrla, c, num;
   ctrla = char (1);
   
   !if (read_file (file)) return -1;
   cbuf = whatbuf ();

   setbuf (doc_buf);
   eob ();
   !if (bolp ()) newline ();
   insert (ctrla);
   insert (file);
   newline ();
   
   setbuf (cbuf);

   bob ();
   num = 0;
   
   forever 
     { 
	f = 0; v = 0;
	!if (fsearch ("MAKE_")) break;
	
	f = looking_at ("MAKE_INTRINSIC");
	v = looking_at ("MAKE_VARIABLE");
	
	POINT;
	bol_skip_white ();
	if (POINT - ())
	  {
	     eol ();
	     continue; %%continue if in middle of line
	  } 
	
	%% found
     
	setbuf (doc_buf);
	eob ();
	!if (bolp ()) newline ();
	setbuf (cbuf);
 
	%% formats are MAKE_INTRINSIC(".fun", cfun, TYPE, n)
	%% and         MAKE_VARIABLE(".var", &c_var, TYPE, flag)
	%%   where the & may not be present for string variables
	
	!if (ffind ("\"."))
	  error ("Corrupt file.");
	
	go_right (2); push_mark ();
	!if (ffind ("\""))
	  error ("Corrupt file.");
	
	name = strcat (bufsubstr (),  " ");
	go_right_1 ();
     
	%% if it is a function, get the c function info as well
	if (f)
	  { 
	     name = strncat ("F", name, ":", 3);  %%Ffun:
	     skip_chars (",\t ");
	     push_mark ();
	     skip_chars ("$_A-Za-z0-9");
	     
	     strncat (name, bufsubstr (), ":", 3);      %%Ffun:cfun:
	     skip_chars (",\t ");
	     strncat ((), char (what_char ()), ":", 3);    %%Ffun:cfun:T:
	     
	     () = ffind (",");  %% assume ok
	     skip_chars (", \t");
	     push_mark ();
	     skip_chars ("0-9");
	     name = strcat ((), bufsubstr ()); %%Whew!!      %%Ffun:cfun:T:0
	  }
	else name = strcat ("V", name);
     
	setbuf (doc_buf);
	insert (name);
	
	m = create_user_mark ();
	
	setbuf (cbuf);
     
	%% look for documentation
	while (down_1 ())
	  {
	     bol_skip_white ();
	     if (eolp ()) continue;
	     
	     !if (looking_at ("/*"))
	       break;
	     
	     go_right (2); skip_white ();
	     c = what_column ();
	     push_mark ();
	     () = fsearch ("*/");  %% I assume it compiled 
 	
	     copy_region (doc_buf);
	     setbuf (doc_buf);
	     goto_user_mark (m);
	     
	     %push_spot ();

	     while (down_1 ())
	       { 
		  bol_skip_white ();
		  if (looking_at ("*"))
		    {
		       del ();
		       skip_white ();
		    }
		  
		  what_column ();
		  bol_trim ();
		  insert_spaces (());
	       }
	     
	     goto_user_mark (m); % pop_spot ();
	     insert (ctrla);
	
	     while (eol (), not (eobp ()))
	       {
		  del ();
		  insert (ctrla);
	       }
	     
	     newline ();
	     setbuf (cbuf);
	     break;
	  }
	num++;
     }
   
   num;
}
 



define mkdoc_sort (docbuf)
{
   variable cbuf = whatbuf ();
   setbuf (docbuf);
   goto_column (32);
   push_mark ();
   push_mark ();
   !if (bol_bsearch_char (1))
     {
	pop_mark (0);
	pop_mark (0);
	error ("Error finding sort boundary.");
	return;
     }
   go_down_1 ();
   narrow ();
   sort ();
   trim_buffer ();
   eob ();
   widen ();
   setbuf (cbuf);
}

%%% Make documentation for set of files.  Function takes as top argument
%%% the name of docfile to be produced followed by number n of files
%%% then followed by the n filenames.
define mkdoc (argc, docfile)
{
   variable argv, cbuf = whatbuf ();
   variable type, docbuf, num;

   () = find_file (docfile); %setbuf (docfile); 
   erase_buffer ();
   
   docbuf = whatbuf ();
   setbuf (cbuf);
   
   loop (argc)
     { 
	argv = ();
	type = strlow (file_type (argv));
	flush (strcat ("processing ", argv));
	
	switch (type)
	  {
	   case "c":
	     num = mkdoc_process_c_file (argv, docbuf);
	  }
	  {
	   case "sl":
	     num = mkdoc_process_sl_file (argv, docbuf);
	  }
	  {
	     pop ();
	     verror ("File type %s not supported!", type, 1);
	  }
	
	if (num == -1)
	  error (strcat (argv, " could not be processed!"));

	if (strcmp (whatbuf(), docbuf)) delbuf (whatbuf ());
	if (num > 1)
	  mkdoc_sort (docbuf);
     }
   
   setbuf (docbuf);
   
   !if (write_buffer (docfile))
     error ("Unable to write doc file!");

   set_buffer_modified_flag (0);
   % delbuf (docbuf);
   setbuf (cbuf);
}

!if (is_defined ("__load__mkdoc__only__"))
{
   variable __load__mkdoc__only__ = 0;
}

!if (__load__mkdoc__only__)
{

. _stkdepth =$1
. "site.sl" "buf.sl" "help.sl" "util.sl" "dired.sl" "most.sl" "fortran.sl" 
. "misc.sl" "tex.sl" "cmode.sl" "slmode.sl" "cmisc.sl" "untab.sl" "html.sl"
. "syntax.sl" "latex.sl" "buf.sl" "pymode.sl"
. "../src/intrin.c" "../src/xterm.c" "../src/replace.c"
. "../src/lineattr.c"
% . "../src/mswin.c"
. "../../slang/src/slstd.c"  
. "../../slang/src/slmath.c"
. "../../slang/src/slunix.c"
. _stkdepth $1 -
.  % number of files listed above   

. "jed_funs.hlp" mkdoc
quit_jed ();
}


