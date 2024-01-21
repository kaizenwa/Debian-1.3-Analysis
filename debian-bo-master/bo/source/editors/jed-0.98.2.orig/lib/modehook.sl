% modehook.sl	-*- SLang -*-
%!% check for the following mode statements:
%!% -*- mode: NAME -*-		set mode to NAME
%!% -*- evalfile: FILE -*-	evaluate file FILE
%!% -*- eval: expression -*-    evaluate expression
%!% -*- VAR: VALUE -*-		set VAR = VALUE
%!%
%!% these statements may be combined
%!% -*- mode: NAME; evalfile: FILE; VAR: VALUE -*-
define modeline_hook2 ()
{
   variable keyword, value, mode = 0, tag = "-*-", modestr;

   bob ();
   !if (fsearch (tag)) return 0;

   while (ffind (tag))
     {
	go_right (3);
#iffalse
	if (looking_at ("END -*-")) break;
#endif

	push_spot ();
	skip_white (); 
	!if (ffind (tag), pop_spot ()) break;	% closing tag exists?

	forever
	  {
	     skip_chars (" \t;");
	     push_mark ();
	     !if (ffind_char (':'))
	       {
		  pop_mark_0 ();
		  break;
	       }
	     keyword = bufsubstr ();	     
	     go_right_1 ();

	     push_mark ();
	     do
	       {
		  skip_chars ("^-;");
		  if (looking_at_char (';') or looking_at (tag))
		    break;
	       }
	     while (right (1));
	     value = strtrim (bufsubstr ());
	     
	     push_spot ();
	     
	     ERROR_BLOCK
	       {
		  pop_spot ();
	       }
	     % error (Sprintf ("keyword <%s> value <%s>", keyword, value, 2));
	     switch (keyword)
	       { case "mode":
		  modestr = "_mode";
		  value = strlow (str_replace_all (value, "-", "_"));
		  !if (is_substr (value, modestr)) 
		    value = strcat (value, modestr);
		  if (is_defined(value))
		    {
		       eval (value);
		       mode++;
		    }
	       }
	       { case "evalfile":
		  () = evalfile (value);
	       }
	       { case "eval" :
		  eval (value);
	       }
	       { is_defined (()) < 0 :		% set a value
		  eval (strncat (keyword, " = ", value, 3));
	       }
#iffalse
	       { is_defined (()) > 0 :
		  if (value [0] == '.')		% RPN interpret
		    eval (strncat (value, " ", keyword, 3));
	       }
#endif
	       { pop (); }
	     
	     pop_spot ();
	  }
	go_down_1 ();
     }
   mode;
}
