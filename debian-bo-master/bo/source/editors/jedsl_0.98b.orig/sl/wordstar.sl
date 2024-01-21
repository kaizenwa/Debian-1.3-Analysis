%
%  Wordstar Mode for JED
%
%   Put the line: evalfile ("wordstar.sl"); pop ();
%   in your jed.rc startup file.
%

Help_File = Null_String;

set_status_line (" Jed %v : %b   (%m%n)   (%p %c)   %t", 1);

Help_File = Null_String;

set_abort_char (30);    % ^^ (Control 6 on most keyboards)
% Note, the above command BREAKS ^G emacs abort.
unsetkey("^K");
unsetkey("^X");
unsetkey("^W");
unsetkey("^F");

setkey ("ws_insert_last_block", "^P");
setkey ("next_char_cmd","^D");
setkey ("previous_char_cmd","^S");
setkey ("previous_line_cmd","^E");
setkey ("next_line_cmd","^X");
setkey ("page_up","^R");
setkey ("page_down","^C");
setkey ("skip_word","^F");
setkey ("bskip_word","^A");
setkey ("delete_char_cmd","^G");
setkey ("ws_delete_word","^T");
setkey ("delete_line","^Y");
setkey ("ws_repeat_search","^L");
setkey ("ws_window_up","^W");
setkey ("ws_window_down","^Z");
%
% Control-Q keys  --- hope you figure out how to pass ^Q/^S through system
%
setkey ("eol","^Q^D");
setkey ("eol","^QD");
setkey ("bol","^Q^S");
setkey ("bol","^QS");
setkey ("bob","^Q^R");
setkey ("bob","^QR");
setkey ("eob","^Q^C");
setkey ("eob","^QC");
setkey ("search_forward","^Q^F");
setkey ("search_forward","^QF");
setkey ("replace_cmd","^Q^A");
setkey ("replace_cmd","^QA");
setkey ("kill_line","^Q^Y");
setkey ("kill_line","^QY");
setkey ("goto_top_of_window","^QE");
setkey ("goto_top_of_window","^Q^E");
setkey ("goto_bottom_of_window","^Q^X");
setkey ("goto_bottom_of_window","^QX");
%
% Control-K map
%
setkey ("ws_begin_block","^K^B");
setkey ("ws_begin_block","^KB");
setkey ("ws_end_block","^K^K");
setkey ("ws_end_block","^KK");
setkey ("ws_delete_block","^K^Y");
setkey ("ws_delete_block","^KY");
setkey ("ws_copy_block","^K^C");
setkey ("ws_copy_block","^KC");
setkey ("insert_file","^K^R");
setkey ("insert_file","^KR");
setkey ("save_buffer","^K^S");
setkey ("save_buffer","^KS");
setkey ("exit_jed","^K^Q");
setkey ("exit_jed","^KQ");
setkey ("ws_exit_jed","^K^D");
setkey ("ws_exit_jed","^KD");

define ws_window_up () 
{
   recenter (window_line() + 1);
}

define ws_window_down ()
{
   recenter (window_line() + 1);
}

% Not implemented:
%    ^K^H  - Hide or unhide the currently selected block

define ws_repeat_search ()
{
   go_right_1 ();
   !if (fsearch(LAST_SEARCH)) error ("Not found.");
}



% the blocks are very different.  Here we cheat.  Two marks are pushed--
% One at beginning of block and one at end.  Assumption is that the spots
% we see are the ones we put.

variable WS_Mark_Pushed = 0;

define ws_begin_block ()
{
   loop (WS_Mark_Pushed) pop_mark_0 ();
   WS_Mark_Pushed = 1;
   push_mark(); 
   push_visible_mark ();
   message ("Begin Block.");
}


% copies block to internal buffer-- preserves block
variable WS_Block_Buffer = " *ws-internal*";

define ws_copy_block_to_buffer ()
{
   if (WS_Mark_Pushed < 2) error ("Block Not defined.");
   push_spot ();
   pop_mark_1 (); % end of block
   dupmark(); pop();  % dup beginning because we want to keep it
   whatbuf ();
   setbuf (WS_Block_Buffer); erase_buffer ();
   setbuf(());
   copy_region (WS_Block_Buffer);
   push_mark ();
   pop_spot ();
}

define ws_end_block ()
{
   if (WS_Mark_Pushed != 1)
     {
	loop (WS_Mark_Pushed) pop_mark_0 (); 
	WS_Mark_Pushed = 0;
	error ("Begin Block First!");
     }
   
   !if (markp()) 
     {
	WS_Mark_Pushed = 0;
	error ("Wordstar Error.");
     }
   
   WS_Mark_Pushed = 2;
   pop_mark_0 ();           % pops visible mark from begin block
   push_mark ();
   ws_copy_block_to_buffer ();
   message ("Block Defined.");
}


define ws_delete_block ()
{
   ws_copy_block_to_buffer ();
   pop_mark_1 (); del_region ();
}
       

define ws_copy_block ()
{
   ws_copy_block_to_buffer ();
   insbuf (WS_Block_Buffer);
   message ("Region Copied.");
}


define ws_exit_jed ()
{
   save_buffer ();
   exit_jed ();
}

define ws_delete_word ()
{
   variable p = POINT;
   push_mark ();
   skip_white ();
   if (POINT == p) 
     {
	skip_word ();
	skip_white ();
     }
   del_region ();
}

define ws_insert_last_block ()
{
   if (bufferp(WS_Block_Buffer)) insbuf (WS_Block_Buffer);
}
