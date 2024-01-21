%% occur function
%%


$1 = "Occur";
!if (keymap_p ($1))
{
   make_keymap ($1);
}

definekey ("occur_goto_line", "g", $1);
variable Occur_Buffer = Null_String;

define occur_goto_line ()
{
   variable line;
   
   !if (bufferp (Occur_Buffer))
     return;
   
   bol ();
   push_mark ();
   !if (ffind (":"))
     {
	pop_mark_0 ();
	return;
     }
   
   line = integer (bufsubstr ());
   
   
   pop2buf (Occur_Buffer);
   goto_line (line);
}


%!% Prototype: Void occur ();
%!% This function may be used to search for all occurances of a string in the
%!% current buffer.  It creates a separate buffer called @*occur*@ and 
%!% associates a keymap called @Occur@ with the new buffer.  In this
%!% buffer, the @g@ key may be used to go to the line described by the
%!% match.
define occur()
{
   variable str, tmp, n;
   
   str = read_mini("Find All (Regexp):", LAST_SEARCH, Null_String);
   tmp = "*occur*";
   Occur_Buffer = whatbuf();
   pop2buf(tmp);
   erase_buffer();
   pop2buf(Occur_Buffer);
   
   push_spot();
   while (re_fsearch(str))
     {
	line_as_string ();  % stack-- at eol too
	n = what_line ();
	
	setbuf(tmp);
	vinsert ("%4d:", n, 1);
	insert(()); 
	newline();
	setbuf(Occur_Buffer);
	eol();             %% so we do not find another occurance on same line
     }
   pop_spot();
   setbuf(tmp);
   bob(); set_buffer_modified_flag(0);
   
   use_keymap ("Occur");
   runhooks ("occur_mode_hook");
}

