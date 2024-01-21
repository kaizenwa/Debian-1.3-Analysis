% wmark.sl
% Implements Windows style of marking - for Windows users
% Author: Luchesar Ionkov
%
% Modified By JED
%
% Holding down Shift key and using arrow keys selects text
% Delete Key cuts the block of text.
% Inserting a character will replace the block with the character.
% Yanking will replace the block with the text in the yank buffer

!if (is_defined ("Wmark_Del_Region_Exec_Funs"))
{
   variable Wmark_Del_Region_Exec_Funs = ",self_insert_cmd,yank,yp_yank";
}

!if (is_defined ("Wmark_Del_Region_Funs"))
{
   variable 
     Wmark_Del_Region_Funs = strncat (",backward_delete_char_untabify",
				      ",delete_char_cmd",
				      ",backward_delete_char",
				      3);
}


variable Wmark_Movement_Flag = 0;

define wmark_copy ()
{
   call ("copy_region");
}

define wmark_cut()
{
   call ("kill_region");
}


define wmark_prefix ()
{
   !if (is_visible_mark ()) push_visible_mark ();
   Wmark_Movement_Flag = 1;
}

variable Wmark_Selection_Mode = 0;
define wmark_suffix ()
{
   variable f, fstr;
   variable type;
   
   if (Wmark_Selection_Mode)
     return;   
   
   Wmark_Selection_Mode = 1;
   
   do
     {
	update (1);

	while (f = get_key_function (), not (strlen (f)))
	  beep ();
	
	type = ();
	
	if (strncmp (f, "wmark_", 6))
	  {
	     fstr = Sprintf (",%s,", f, 1);
	     
	     if (is_substr (Wmark_Del_Region_Funs, fstr))
	       {
		  wmark_cut ();
		  break;
	       }
	     
	     if (is_substr (Wmark_Del_Region_Exec_Funs, fstr))
	       del_region ();
	     else Wmark_Selection_Mode = 0; %  allow function to act on region
	  }
		
	Wmark_Movement_Flag = 0;

	if (type)
	  call (f);
	else
	  eval (f);
	
	% Test to see whether or not function acted on region.
	if (not (Wmark_Selection_Mode) and is_visible_mark ())
	  pop_mark_0 ();
     }
   while (Wmark_Movement_Flag);
   
   Wmark_Selection_Mode = 0;
}

define wmark_up()
{
   wmark_prefix();
   call("previous_line_cmd");
   wmark_suffix();
}

define wmark_down()
{
   wmark_prefix();
   call("next_line_cmd");
   wmark_suffix();
}


define wmark_left()
{
   wmark_prefix();
   call("previous_char_cmd");
   wmark_suffix();
}

define wmark_right()
{
   wmark_prefix();
   call("next_char_cmd");
   wmark_suffix();
}

define wmark_page_up()
{
   wmark_prefix();
   call("page_up");
   wmark_suffix();
}

define wmark_page_down()
{
   wmark_prefix();
   call("page_down");
   wmark_suffix();
}

define wmark_bol()
{
   wmark_prefix();
   bol();
   wmark_suffix();
}

define wmark_eol()
{
   wmark_prefix();
   eol();
   wmark_suffix();
}

define wmark_bob()
{
   wmark_prefix();
   bob();
   wmark_suffix();
}

define wmark_eob()
{
   wmark_prefix();
   eob();
   wmark_suffix();
}

define wmark_next_word()
{
   wmark_prefix();
   skip_word();			       %  was next_word
   wmark_suffix();
}


define wmark_bskip_word()
{
   wmark_prefix();
   bskip_word();
   wmark_suffix();
}

#ifdef UNIX VMS XWINDOWS
% These keybindings will work in rxvt as well as Xjed

local_setkey("wmark_up","\e[a");	       %  Shift-Up
local_setkey("wmark_down","\e[b");	       %  Shift-Down
local_setkey("wmark_left","\e[d");	       %  Shift-Left
local_setkey("wmark_right","\e[c");	       %  Shift-Right
local_setkey("wmark_page_up","\e[5$");	       %  Shift-PageUp
local_setkey("wmark_page_down","\e[6$");	       %  Shift-PageDown
local_setkey("wmark_bol","\e[1$");	       %  Shift-Home
local_setkey("wmark_eol","\e[4$");	       %  Shift-End
local_setkey("yank","\e[2$");		       %  Shift-Ins
local_setkey("wmark_copy", "\e[2^");	       %  Ctrl-Ins
%local_setkey("wmark_cut","");	       %  Shift-Del

#else
local_setkey("wmark_up","\xE01");	       %  Shift-Up
local_setkey("wmark_down","\xE06");	       %  Shift-Down
local_setkey("wmark_left","\xE03");	       %  Shift-Left
local_setkey("wmark_right","\xE04");	       %  Shift-Right
local_setkey("wmark_page_up","\xE02");	       %  Shift-PageUp
local_setkey("wmark_page_down","\xE07");	       %  Shift-PageDown
local_setkey("wmark_bol","\xE00");	       %  Shift-Home
local_setkey("wmark_eol","\xE05");	       %  Shift-End
local_setkey("yank","\xE08");		       %  Shift-Ins
local_setkey("wmark_cut","\xE09");	       %  Shift-Del
local_setkey("wmark_copy","\xE0\x92");	       %  Ctrl-Ins
#endif

%local_setkey("wmark_eob","[34~");	        %  Shift-Ctrl-End
%local_setkey("wmark_next_word","[31~");    %  Shift-Ctrl-Right
%local_setkey("wmark_bskip_word","[32~");   %  Shift-Ctrl-Left
