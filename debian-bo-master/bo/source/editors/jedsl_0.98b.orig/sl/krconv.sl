define c_indent_buffer ()
{
   variable col, line = -50, max_line;
   push_spot ();
   eob ();
   max_line = what_line ();
   bob ();
   do
     {
	bol_skip_white ();
	if (looking_at ("/*"))
	  {
	     % skip the comment 
	     () = fsearch ("*/");
	     continue;
	  }
	
	eol ();
	if (blooking_at ("\\"))
	  {
	     indent_line ();
	     % skip all continuation lines.
	     while (down (1))
	       {
		  eol ();
		  !if (blooking_at ("\\")) break;
	       }
	     continue;
	  }
	trim ();
	
	bol ();
	USER_BLOCK0 
	  {
	     !if (parse_to_point ())
	       {
		  col = what_column ();
		  bol_skip_white ();
		  if (col != what_column ())
		    {
		       goto_column (col);
		       indent_line ();
		       newline ();
		    }
		  go_right_1 ();
		  trim ();
		  !if (eolp () or looking_at_char (',') or looking_at_char (';'))
		    {
		       indent_line ();
		       newline ();
		    }
		  continue;
	       }
	     go_right_1 ();
	  }
   
	bol_skip_white ();
	!if (looking_at_char ('{'))
	  {
	     while (ffind_char ('}'))
	       {
		  X_USER_BLOCK0 ();
	       }
	     bol ();
	     while (ffind_char ('{'))
	       {
		  X_USER_BLOCK0 ();
	       }
	  }
	indent_line ();
	if (line + 50 < what_line ())
	  {
	     line = what_line ();
	     flush (Sprintf ("processed %d/%d lines.", line, max_line, 2));
	  }
	
	%update (1);
     }
   while (down (1));
   flush (Sprintf ("processed %d/%d lines.", what_line (), max_line, 2));
   pop_spot ();
}

	
	     
