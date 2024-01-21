define mktex_fixup_underscores ()
{
   push_spot_bol ();
   while (ffind_char ('_'))
     {
	del ();
	insert ("\\_");
     }
   pop_spot ();
}



define mktex_format_rest ()
{
   variable pnt, at = "@";
   bob ();
   while (down (1))
     {
	skip_white ();
	if (looking_at ("%!%")) deln (3);
	if (looking_at ("Related Functions:")
	    or looking_at ("Related Variables:"))
	  {
	     insert ("\n{\\bf ");
	     go_right (18);
	     insert ("}");
	     trim ();
	     insert_single_space ();
	  }
     }
   bob ();
   
   while (fsearch (at))
     {
	if (looking_at ("@ "))
	  {
	     bskip_chars (" ");
	     !if (bolp ()) 
	       {
		  error ("Found @SPACE not at beginning of line!");
	       }
	     
	     % verbatim 
	     bol ();
	     insert ("\\begin{verbatim}\n");
	     forever 
	       {
		  skip_white ();
		  if (looking_at ("@ ")) 
		    {
		       del ();
		       insert ("    ");
		    }
		  else 
		    {
		       % at the end
		       bol ();
		       insert ("\\end{verbatim}\n");
		       break;
		    }
		  
		  while (ffind ("@")) 
		    {
		       del ();
		       !if (eolp ()) go_right_1 ();
		    }
		  !if (down (1))
		    {
		       eol ();
		       insert ("\n\\end{verbatim}\n");
		       break;
		    }
	       }
	     continue;
	  }
	else
	  {
	     del ();
	     if (looking_at (at)) 
	       {
		  go_right_1 ();
		  continue;
	       }
	  }
	

	insert ("\\verb|");
	forever 
	  {
	     !if (ffind (at)) error ("Unbalanced '@'");
	     del ();
	     if (looking_at (at))
	       {
		  go_right_1 ();
		  continue;
	       }
	     insert_char ('|');
	     break;
	  }
     }
}

define mktex_format_function ()
{
   variable pnt;

   del ();			       %  remove the F at bol
   insert ("\\parbox{\\textwidth}{\\MYBOX{");
   mktex_fixup_underscores ();
   
   if (ffind (":"))
     {
	%% function:cfun:T:n  where T is type and n is number of args
	del (); 
	insert ("}\n");
	
	delete_line ();		       %  c fun information-- unused
	
	if (looking_at ("Prototype:"))
	  {
	     mktex_fixup_underscores ();
	     insert ("{\\bf ");
	     ffind (":"); pop (); go_right_1 ();
	     insert ("}");
	     trim (); insert (" ");
	     push_spot ();
	     % look for return type information 
	     skip_chars ("a-zA-Z_0-9");
	     trim ();
	     if (looking_at ("(")) pop_spot ();
	     else
	       {
		  push_mark ();
		  pop_spot ();
		  insert ("{\\em ");
		  pop_mark_1 ();
		  insert ("\\/}");
	       }
	     
	     insert (" {\\tt ");
	     
	     () = ffind ("(");
	     go_right_1 ();
	     !if (looking_at (")"))
	       {
		  % insert ("}");
		  
		  trim ();
		  forever
		    {
		       push_mark ();
		       skip_chars ("a-zA-Z0-9_");
		       trim ();
		       
		       if (looking_at_char (',')
			   or looking_at_char ('.'))
			 {
			    pop_mark (0);
			    go_right_1 ();
			    trim ();
			    if (what_char () >= 'A')
			      insert_single_space ();
			    continue;
			 }
		       
		       if (looking_at_char (')'))
			 {
			    pop_mark (0);
			    break;
			 }
		       
		       push_spot ();
		       pop_mark (1);
		       
		       insert ("{\\em ");
		       pop_spot ();
		       insert ("\\/}");
		       trim ();
		       insert_single_space ();
		    }
	       }
	     eol ();
	     insert ("}");	       %  close tt
	  }
	insert ("}\n\n");
	mktex_format_rest ();
     }
   else 
     {
	insert ("}}\n\n");
     }
}


	



%% gets help for a function
define mktex_process_buffer (do_header)
{
   variable p, n;
   bob ();
   
   if (do_header)
     {
	insert ("\\documentstyle{article}\n");
	insert ("\\parindent=0mm\n");
	insert ("\\parskip=3mm\n");
	insert ("\\setlength{\\oddsidemargin}{0mm}\n");
	insert ("\\setlength{\\evensidemargin}{0mm}\n");
	insert ("\\setlength{\\textwidth}{160mm}\n");
	insert ("\\setlength{\\topmargin}{-10mm}\n");
	insert ("\\setlength{\\textheight}{220mm}\n");
     }
   
	insert ("\n\\newcommand{\\MYBOX}[1]{{\\rule[2mm]{\\textwidth}{0.25mm}\\par\n");
	insert ("{\\huge\\bf #1}\\hfill\\par\\rule{\\textwidth}{0.25mm}}\\par}\n");

   if (do_header)
     {
	insert ("\\begin{document}\n");
     }
   
   do 
     {
	push_mark ();
	narrow ();
	if (looking_at ("F")) 
	  {
	     replace (char (1), "\n");
	     mktex_format_function ();
	  }
	% else if (looking_at ("V")) mktex_format_variable ();
	else insert ("%");
	eob ();
	newline ();
	widen ();
     }
   while (down (1));
   eob ();
   newline ();
   if (do_header) insert ("\n\\end{document}\n");
}

define mktex_sort_fun (a, b)
{
   strcmp (strlow (str_replace_all (a, "_", Null_String)),
	   strlow (str_replace_all (b, "_", Null_String)))
     < 0;
}

define mktex_sort ()
{
   sort_using_function ("mktex_sort_fun");
}

define mktex_doit ()
{
   () = read_file ("jed_funs.tex");
   erase_buffer ();
   () = insert_file ("jed_funs.hlp");
   bob ();
   push_mark ();
   push_mark ();
   () = bol_fsearch ("\001../src/");
   go_up_1 ();
   goto_column (32);
   narrow ();
   mktex_sort ();
   eob ();
   widen ();
   go_down_1 ();
   push_mark ();
   push_mark ();
   if (re_fsearch ("^\001[^.]"))
     go_up_1 ();
   else eob ();
   goto_column (32);
   narrow ();
   mktex_sort ();
   eob ();
   widen ();
   
   % Now delete unsupported stuff until ready
   push_mark_eob ();
   del_region ();
   newline ();
   
   mktex_process_buffer (1);
   save_buffer ();
}

   
   
   
   
