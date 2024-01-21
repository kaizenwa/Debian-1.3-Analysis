% These routines user 'user_marks' to implement book marks.  A book mark 
% may be placed in any buffer and returning to the mark may cause a change
% of buffer.

% The only functions that are considered external are 'bkmrk_set_mark'
% and 'bkmrk_goto_mark'.  These functions prompt for a a key '0' - '9'
% or a SPACE.


% user marks are of type 128, last mark for previous position
variable Book_Marks = create_array (128, 10, 1);
variable Bkmrk_Last_Position = create_user_mark ();

define bkmrk_get_or_set_mark (get)
{
   variable n;
   variable prompt;
   
   prompt = "Bookmark number:";
   if (get) prompt = "Bookmark number or SPACE for last position:";
   
   flush (prompt);
   n = getkey ();
   
   if (get and (n == ' '))
     return Bkmrk_Last_Position;
   
   n -= '0';
   
   if ((n < 0) or (n > 9)) error ("Number must be less than 10");
   
   if (get)        
     return Book_Marks[n];
   
   Book_Marks[n] = create_user_mark ();
   vmessage ("Bookmark %d set.", n, 1);
}

define bkmrk_set_mark ()
{
   bkmrk_get_or_set_mark (0);
}

define bkmrk_goto_mark ()
{
   variable mrk = bkmrk_get_or_set_mark (1);
    
   Bkmrk_Last_Position = create_user_mark ();

   sw2buf (user_mark_buffer (mrk));
   !if (is_user_mark_in_narrow (mrk))
     {
#ifdef HAS_BLOCAL_VAR
	variable fun;
	ERROR_BLOCK
	  {
	     _clear_error ();
	     error ("Mark lies outside visible part of buffer.");
	  }
	fun = get_blocal_var ("bookmark_narrow_hook");
	mrk; eval (fun);
#else
	error ("Mark lies outside visible part of buffer.");
#endif
     }
   

   goto_user_mark (mrk);
   message ("done.");
}

   
   
