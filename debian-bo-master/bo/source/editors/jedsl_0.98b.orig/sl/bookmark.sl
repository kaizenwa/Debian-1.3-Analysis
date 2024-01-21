% These routines user 'user_marks' to implement book marks.  A book mark 
% may be placed in any buffer and returning to the mark may cause a change
% of buffer.

% The only functions that are considered external are 'bkmrk_set_mark'
% and 'bkmrk_goto_mark'.  These functions prompt for a a key '0' to '9'.

% It might be better to reserve one of the marks so that when 
% 'bkmrk_goto_mark' is called, the reserved mark is set to the previous
% position.


!if (is_defined("Book_Marks"))
{
   % user marks are of type 128
   variable Book_Marks = create_array (128, 10, 1);
}


define bkmrk_get_number ()
{
   variable n;
   flush ("Bookmark number:");
   n = getkey () - '0';
   if ((n < 0) or (n > 9)) error ("Number must be less than 10.");
   n;
}

define bkmrk_set_mark ()
{
   variable n = bkmrk_get_number ();   
   Book_Marks[n] = create_user_mark ();
   vmessage ("Bookmark %d set.", n, 1);
}

define bkmrk_goto_mark ()
{
   variable n = bkmrk_get_number ();
   variable mrk = Book_Marks[n];
   
   sw2buf (user_mark_buffer (mrk));
   goto_user_mark (mrk);
   message ("done.");
}

   
   
