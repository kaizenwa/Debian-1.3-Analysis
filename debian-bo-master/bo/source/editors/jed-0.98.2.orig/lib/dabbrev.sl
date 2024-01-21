% dabbrev.sl - Author: Adrian Savage (afs@jumper.mcc.ac.uk)
%              Date:   May 1994
%
% This file may be freely distributed and modified
%
% find the previous match for the current word and add it to the current word

#ifdef MSDOS
  $1 = 10;
#else
  $1 = 32;
#endif

!if (is_defined("Dabbrev_Num_Matches"))
{
   variable Dabbrev_Num_Matches = 0;
   variable Dabbrev_Last_Matches = create_array ('s', $1, 1);
}


define dabbrev_is_already_matched (str, old)
{
   variable i;
   !if (strcmp(str, old)) return 1;

   _for (0, Dabbrev_Num_Matches - 1, 1)
     {
	i = ();
	!if (strcmp (str, Dabbrev_Last_Matches[i])) return 1;
     }

#ifdef MSDOS
   if (Dabbrev_Num_Matches == 10) return 0;
#else
   if (Dabbrev_Num_Matches == 32) return 0;
#endif
   
   Dabbrev_Last_Matches [Dabbrev_Num_Matches] = str;
   Dabbrev_Num_Matches++;
   return 0;
}


define dabbrev_skip_word_chars (dir)
{
   if (dir > 0)
     {
	do 
	  {
	     skip_chars ("_");
	     skip_word_chars ();
	  }
	while (looking_at_char ('_'));
	return;
     }
  
   forever
     {
	bskip_word_chars ();
	if (bolp()) break;
	go_left_1 ();
	!if (looking_at_char ('_'))
	  {
	     go_right_1 ();
	     break;
	  }
     }
}

define dabbrev_widen ()
{
   variable n = count_narrows ();
   if (n)
     {
	push_narrow ();
	loop (n) widen ();
     }
   n;
}

define dabbrev_expand_dir (dir)
{
   variable hilite_wrd, num_spots = 0, num_marks = 0, ndel, 
     complete, fun, use_call, success = 0, ch, search_fun,
     is_narrow = 0;
   
   ERROR_BLOCK 
     {
	loop (num_spots) pop_spot ();
	loop (num_marks) pop_mark_1 ();
	!if (success)
	  {
	     push_mark ();
	     dabbrev_skip_word_chars(-1);
	     del_region ();
	     insert (hilite_wrd);
	  }
	if (is_narrow) 
	  {
	     pop_narrow ();
	  }
     }
   
   
   push_mark ();  num_marks++;	       % remember initial position
   
   push_mark();			       % mark end of this word  
   dabbrev_skip_word_chars(-1);	       % jump to beginning of word    
   hilite_wrd = bufsubstr();	       % get the word  
   push_mark ();  num_marks++;	       % remember beginning position

   ndel = strlen (hilite_wrd);
   
   search_fun = &bsearch;
   if (dir) 
     {
	search_fun = &fsearch;
	go_right (ndel);
     }
   
   is_narrow = dabbrev_widen ();
   
   forever 
     {
	!if (search_fun (hilite_wrd))
	  {
	     error (strcat ("No more completions for ", hilite_wrd));
	  }
	
	
	dabbrev_skip_word_chars (-1);
	!if (looking_at(hilite_wrd)) 
	  {
	     if (dir) dabbrev_skip_word_chars (1);
	     continue;
	  }
	
	push_mark ();		       % set a mark there  
	dabbrev_skip_word_chars(1);    % skip to end of	word      
	complete = bufsubstr();	       % get the completed word
	
	!if (dir) dabbrev_skip_word_chars (-1);
	if (dabbrev_is_already_matched(complete, hilite_wrd)) continue;
	
	push_spot (); num_spots++;     %  remember how far we got
	
	pop_mark_1 ();		       %  back to beginning
	push_mark;
	
	insert (complete);
	deln (ndel);		       %  delete word
	ndel = strlen (complete);

	if (is_narrow)
	  {
	     pop_narrow ();
	     is_narrow = 0;
	  }
	
	update (1);		       %  force update
	
	ch = getkey (); ungetkey (ch);
	
	fun = get_key_function ();
	if (strlen (fun))
	  {
	     use_call = ();
	  }

	if (strcmp (fun, "dabbrev")) break;   %  user hit another key, done
	variable zzzz = create_user_mark ();
	pop_spot (); num_spots--;      %  returns and continue looking
	
	is_narrow = dabbrev_widen ();
     }
   
   % we get here only when we are finished
   success = 1;
   
   EXECUTE_ERROR_BLOCK;		       %  pop spots/marks
   
   update (0);
   !if (strlen (fun)) return;
   if (use_call) call(fun); else eval(fun);
}

define dabbrev_case_search (cse)
{
   CASE_SEARCH = cse;

   ERROR_BLOCK
     {
	_clear_error ();
	dabbrev_expand_dir (1);
     }
   dabbrev_expand_dir (0);
}

define dabbrev ()
{
   variable cse = CASE_SEARCH;
   
   Dabbrev_Num_Matches = 0;

   ERROR_BLOCK
     {
	_clear_error ();
	ERROR_BLOCK
	  {
	     CASE_SEARCH = cse;		% restore case search
	  }
	dabbrev_case_search (0);
     }
   dabbrev_case_search (1);
   CASE_SEARCH = cse;		% restore case search
}
