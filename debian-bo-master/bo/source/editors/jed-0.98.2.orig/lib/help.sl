%% help.sl

!if (is_defined ("Jed_Doc_File"))
{
   variable Jed_Doc_File = expand_jedlib_file ("jed_funs.hlp");
}


%% apropos function
define apropos()
{
   variable n, cbuf, s;
   if (MINIBUFFER_ACTIVE) return;

   s = read_mini("apropos:", Null_String, Null_String);
   n = slapropos(s);
   cbuf = whatbuf();
   pop2buf("*apropos*");
   erase_buffer();
   loop (n) 
     {
	insert(());
	newline();
     }
   buffer_format_in_columns();   
   bob();
   set_buffer_modified_flag(0);
   pop2buf(cbuf);
}

%!% Prototype: String expand_keystring (String key)
%!% This function takes a key string that is suitable for use in a 'setkey'
%!% definition and expands it to a human readable form.  
%!% For example, it expands ^X to the form "Ctrl-X", ^[ to "ESC", 
%!% ^[[A to "UP", etc...
%!% See also: setkey
define expand_keystring (key)
{
   variable n = strlen (key);
   variable str, i = 1, ch, key_string = Null_String, the_key;
   
   while (i <= n)
     {
	ch = key[i - 1];
	++i;
	switch (ch)
	  {
	     (case '^' and (i <= n)) or (ch < 32) :   
	     % control char
	     %% common cases are '^[, ^[[?, ^[O?'
	  
	     if (ch < 32)
	       {
		  str = strcat (char (ch + '@'), substr (key, i, 2));
		  i--;
	       }
	     else str = substr (key, i, 3);
	     
	     if (int(str) == '[')
	       {
		  i += 3;
		  switch (str)
		    { case "[[A" : "UP"; }
		    { case "[[B" : "DOWN"; }
		    { case "[[C" : "RIGHT"; }
		    { case "[[D" : "LEFT"; }
		    { case "[OP" : "GOLD"; }
		    { case "[OA" : "UP"; }
		    { case "[OB" : "DOWN"; }
		    { case "[OC" : "RIGHT"; }
		    { case "[OD" : "LEFT"; }
		    { pop(); "ESC"; i -= 2;
		    }
	       }
	     else
	       {
		  i++;
		  strcat ("Ctrl-", char(int(str)));
	       }
	  }
	  {
	     char (());
	  }
	the_key = ();
	
	if (strlen(key_string)) key_string = strcat(key_string, " ");
	key_string = strcat (key_string, the_key);
     }
   
   key_string;
}


%% show key
define showkey()
{
   variable f, type, the_type = "internal";
   flush("Show Key: ");
   f = get_key_function();        %% also, type is on stack (if defined)
   !if (strlen(f)) 
     {
	vmessage ("Key \"%s\" is undefined.",
		  expand_keystring (LASTKEY), 1);
	return;
     }
   type = ();
   
   !if (type) the_type = "S-Lang";
   
   vmessage ("Key \"%s\" runs %s function %s.", expand_keystring(LASTKEY),
	     the_type, f, 3);
}

define where_is ()
{
   variable n, cmd;
   if (MINIBUFFER_ACTIVE) return;
   
   cmd = read_with_completion ("Where is command:", Null_String, Null_String, 'F');
   !if (strlen (cmd)) return;
   n = which_key (cmd);
   !if (n) return message ("Not on any keys.");
   
   message (expand_keystring ());
   --n; loop (n) pop ();
}

%%
%%  This next functions requires a doc file to be produced.  This is made
%%  by running 'jed -n -l mkdoc' FROM JED_ROOT/lib
%%

define help_cleanup ()
{
   variable pnt, at = "@";
   bob ();
   while (down_1 ())
     {
	skip_white ();
	if (looking_at ("%!%")) deln (3);
     }
   bob ();
   while (fsearch (at))
     {
	del ();
	if (looking_at_char (' '))
	  {
	     insert_spaces (5);
	     eol ();
	     continue;
	  }
	
	if (looking_at (at))
	  {
	     go_right_1 ();
	     continue;
	  }
	
	pnt = POINT;
	skip_white ();
	if (pnt != POINT) continue;
	insert_char ('`');
	while (ffind (at))
	  {
	     del ();
	     if (looking_at (at))
	       {
		  go_right_1 ();
		  continue;
	       }
	     insert_char ('\'');
	     break;
	  }
     }
   bob ();
   set_buffer_modified_flag (0);
   set_readonly (1);
}

%% gets help for a function
define help_for_function (f)
{
   variable cbuf = whatbuf ();
   variable tmp = " *jed-help*";
   variable help = "*function-help*";
   variable flen, full_f;
   variable p, n;
   
   flen = strlen (f);
  
   !if (strlen (Jed_Doc_File)) error("The variable Jed_Doc_File is invalid.");

   if (is_defined (f)) 
     {
	f; 1;			       %  one f on the stack
     }
   else	slapropos (f);		       %  many fs on the stack 
   n = ();
   
   if (n > 1) flush ("Building list...");
   
   pop2buf (help); set_readonly (0); erase_buffer ();
  
   % Now loop n times popping the functions off the stack
   loop (n)
     {
	full_f = ();
	if (strncmp (f, full_f, flen)
	    or (is_defined (full_f) < 1)) continue;
	
	if (get_doc_string (strcat ("F", full_f), Jed_Doc_File))
	  {  
	     % document string on the stack
	     push_spot ();
	     insert ( () );
	     pop_spot ();
	     if (ffind_char (':'))  
	       {
		  go_right_1 ();
		  %% function:cfun:T:n  where T is type and n is number of args
		  insert ("\tC function: ");
		  p = POINT;
		  !if (ffind_char (':')) error ("Corrupt doc file?");
		  push_spot ();
		  go_right_1 ();
		  switch (what_char ())
		    { case 'V': "void "}
		    { case 'S': "string "}
		    { case 'F': "float "}
		    { pop (); "int "}  % 'I' default  
		  
		  POINT = p;
		  insert (());
		  pop_spot ();
       
		  deln (3);	       % :T: deleted
		  
		  insert (" ("); eol_trim ();
		  insert (" args)");
	       }
	  } else insert (strcat (full_f, " : Undocumented."));
	
	eob ();
	insert ("\n-----------------------------------\n");
     }
      
   if (n > 1) message ("Building list...done.");
   
   help_cleanup ();
   pop2buf (cbuf);
}


. (  
.   MINIBUFFER_ACTIVE {return} if
.   "Describe Function:" Null_String Null_String read_mini help_for_function
. ) describe_function


. (
.   [tmp help f full_f cbuf]  " *jed-help*" =tmp 
.   "*function-help*" =help  
.   whatbuf =cbuf
.   [p flen n]

.   MINIBUFFER_ACTIVE {return} if
.   "Describe Variable:" Null_String Null_String read_mini =f
.   f strlen =flen %%flen {return} !if
  
.   Jed_Doc_File strlen  { "Unable to read Jed_Doc_File!" error} !if
  
.   f slapropos =n
.   n 1 > {"Building list..." flush} if
.   help pop2buf 0 set_readonly erase_buffer
  
.   n {
.       =full_f
.       f full_f flen strncmp %%flen and 
.       {continue} if
  
.       full_f is_defined -1 > {continue} if
.       "V" full_f strcat " " strcat Jed_Doc_File get_doc_string
.         {  
. 	   push_spot
. 	   insert  %% help string
. 	   pop_spot
. 	} { full_f insert} 
.       else
       
.       eol_trim "\tvalue: " insert full_f eval string insert
      
.       eob newline 50 {'-' insert_char} loop newline
.     } loop
    
    
.   newline
.   help_cleanup
.   cbuf pop2buf
. ) describe_variable


define describe_mode ()
{
   variable flags, modstr;
   (modstr, flags) = what_mode ();
   
   modstr = extract_element (modstr, 0, ' ');
   !if (strlen (modstr)) modstr = "no";
   !if (is_defined (modstr))
     {
	modstr = strlow (modstr);
	!if (is_defined (modstr))
	  {
	     modstr = strcat (modstr, "_mode");
	     !if (is_defined (modstr))
	       error (strcat ("Mode is not defined: ", modstr));
	  }
     }
   help_for_function (modstr);
}


define describe_bindings ()
{
   flush("Building bindings..");
   variable map = what_keymap ();
   variable buf = whatbuf ();
   variable cse = CASE_SEARCH;  CASE_SEARCH = 1;
   pop2buf("*KeyBindings*");
   erase_buffer ();
   dump_bindings (map);
   bob(); replace ("ESC [ A", "UP");
   bob(); replace ("ESC [ B", "DOWN");
   bob(); replace ("ESC [ C", "RIGHT");
   bob(); replace ("ESC [ D", "LEFT");
   bob(); replace ("ESC O P", "GOLD");
   bob();
   CASE_SEARCH = cse;
   set_buffer_modified_flag(0);
   pop2buf (buf);
   message ("done");
}

	    
   
