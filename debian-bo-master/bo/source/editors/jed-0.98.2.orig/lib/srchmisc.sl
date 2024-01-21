%  These routines are common to both regular expression searches and ordinary
%  searches.

define mark_next_nchars (n, dir)
{
   ERROR_BLOCK {pop_mark_0 ()}
   push_visible_mark ();
   go_right (n);
   if (dir < 0) exchange_point_and_mark ();
   update(1);
   ungetkey(getkey());
   EXECUTE_ERROR_BLOCK;
}


% The search function is to return: 0 if non-match found or the length of the 
% item matched.
% search_fun takes the pattern to search for and returns the length of the 
% pattern matched.  If no match occurs, return -1.
% rep_fun returns the length of characters replaced.

define replace_with_query (search_fun, pat, rep, query, rep_fun)
{
   variable n, prompt, doit, err, ch, pat_len;
   variable last;
   variable rep_len = -1;

   ERROR_BLOCK 
     {
	if (rep_len != -1) pop_mark_0 ();
     }
   
   prompt =  Sprintf ("Replace '%s' with '%s'? (y/n/!/+/q/h)", pat, rep, 2);
   
   while (pat_len = search_fun (pat), pat_len >= 0)
     {
	!if (query)
	  {
	     () = rep_fun (rep, pat_len); 
	     continue;
	  }

	do 
	  {
	     message(prompt);
	     mark_next_nchars (pat_len, -1);
	     
	     ch = getkey ();
	     if (ch == 'r')
	       {
		  recenter (window_info('r') / 2);
	       }
	     
	  } while (ch == 'r');
	
	switch(ch)
	  { case 'u' and (rep_len >= 0):
	     pop_mark_1 (); push_spot ();
	     () = rep_fun (last, rep_len);
	     pop_spot ();
	     rep_len = -1;
	  }   
	  { case 'y' :
	     if (rep_len != -1) pop_mark_0 ();
	     push_spot(); push_mark (); rep_len = 0;
	     go_right (pat_len); last = bufsubstr ();
	     pop_spot (); push_mark ();
	     rep_len = rep_fun (rep, pat_len);
	  }
	  { case 'n' : go_right_1 ();}
	  { case '+' : () = rep_fun (rep, pat_len); 
	               break;
	  }
	  { case '!' :
	     do 
	       {
		  () = rep_fun (rep, pat_len);
	       }
	     while (search_fun (pat) >= 0);
	  }
          { case 'q' : break; }
          {  pop();
	     flush ("y:replace, n:skip, !:replace all, u: undo last, +:replace then quit, q:quit");
	     () = input_pending (30); 
	  }
     }
   EXECUTE_ERROR_BLOCK;
}


define search_maybe_again (fun, str, dir, match_ok_fun)
{
   variable ch, len;
   
   while (len = fun (str, dir), len >= 0)
     {	
	if (match_ok_fun ())
	  {
	     if (EXECUTING_MACRO or DEFINING_MACRO) return 1;
	     message ("Press RET to continue searching.");
	     mark_next_nchars (len, -1);
	     ch = getkey ();
	     if (ch != '\r')
	       {
		  ungetkey (ch);
		  return 1;
	       }
	  }
	if (dir > 0) go_right_1 ();
     }
   return 0;
}
