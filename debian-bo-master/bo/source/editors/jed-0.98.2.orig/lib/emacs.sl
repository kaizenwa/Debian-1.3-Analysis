%
%   Emacs like bindings for Jed.
%    
%      A subset of the emacs global map is implemented here.  If you
%      are lacking a useful keybinding, contact davis@space.mit.edu

  set_status_line("(Jed %v) Emacs: %b    (%m%a%n%o)  %p,%c   %t", 1);

  Help_File = "emacs.hlp";
  KILL_LINE_FEATURE = 0;
#ifdef UNIX
 enable_flow_control(0);  %turns off ^S/^Q processing (Unix only)
#endif
%
%  The default keybindings for Jed use ^W, ^F, and ^K keymaps.  Emacs
%  does not bind these so turn them off

    unsetkey("^K");
    unsetkey("^W");
    unsetkey("^F");

%  Jed default binding of the tab char ^I is to indent line.  Here just insert
%  the tab

%  setkey("self_insert_cmd",	"^I");	

%% The default binding for the quote keys (", ') is 'text_smart_quote'.
%% Most users do not seem to like this so it is unset here.

  setkey("self_insert_cmd",     "\"");
  setkey("self_insert_cmd",     "'");

%  setkey("backward_delete_char",	"^?");	
  setkey("backward_delete_char_untabify",	"^?");	
  setkey("beg_of_line",		"^A");	
  setkey("previous_char_cmd",	"^B");	
  setkey("delete_char_cmd",	"^D");	
  setkey("eol_cmd",		"^E");	
  setkey("next_char_cmd",	"^F");	
  setkey("newline",		"^J");	
  setkey("kill_line",		"^K");	
  setkey("emacs_recenter",	"^L");	
  setkey("newline_and_indent",	"^M");	
  setkey("next_line_cmd",		"^N");	
  setkey("previous_line_cmd",	"^P");	
  setkey("quoted_insert",	"^Q");	
%%
%%  Try these if you prefer
  setkey("isearch_backward",	"^R");	
  setkey("isearch_forward",	"^S");	
%%
%  setkey("search_backward",	"^R");	
%  setkey("search_forward",	"^S");	
  setkey("page_down",		"^V");	
  setkey("kill_region",		"^W");	
  setkey("yank",		"^Y");	
  setkey("sys_spawn_cmd",	"^Z");	

%
%                   The escape map
%
  setkey("forward_sexp",	"\e^F");
  setkey("backward_sexp",	"\e^B");
  setkey("kill_sexp",		"\e^K");
  setkey(".bskip_white trim insert_single_space", "\e ");
  setkey("bskip_word",		"\eB");
  setkey("delete_word",		"\eD");
  setkey("bdelete_word",	"\e^?");
  setkey("skip_word",		"\ef");	
  setkey("replace_cmd",		"\e%");	
  setkey("beg_of_buffer",	"\e<");	
  setkey("end_of_buffer",	"\e>");	
  setkey("narrow_paragraph",	"\eN");	
  setkey("scroll_down_in_place", "\ep");
  setkey("scroll_up_in_place",  "\en");
  setkey("format_paragraph",	"\eQ");	
  setkey("page_up",		"\eV");	   %  see emacs_pageup below
  setkey("copy_region",		"\eW");	
  setkey("trim_whitespace",	"\e\\");	
%
%    ^X map
%
  setkey("delete_blank_lines",  "^X^O");
  setkey("whatpos",             "^X=");	
  setkey("begin_macro",		"^X(");	
  setkey("end_macro",		"^X)");	
  setkey("mark_spot",		"^X/");	
  setkey("delete_window",	"^X0");	
  setkey("one_window",		"^X1");	
  setkey("split_window",	"^X2");	
  setkey("scroll_left",		"^X<");	
  setkey("scroll_right",	"^X>");	
  setkey("switch_to_buffer",	"^XB");
  setkey("dired",		"^XD");	
  setkey("insert_file",		"^XI");	
  setkey("execute_macro",	"^XE");	
  setkey("pop_spot",		"^XJ");	
  setkey("kill_buffer",		"^XK");	
  setkey("other_window",	"^XO");	
  setkey("exit_jed",		"^X^C");	
  setkey("find_file",		"^X^F");	
  setkey(".'d'xform_region",	"^X^L");
  setkey("toggle_readonly",	"^X^Q");	
  setkey("save_buffer",		"^X^S");	
  setkey("transpose_lines",	"^X^T");
  setkey(".'u'xform_region",	"^X^U");
  setkey("write_buffer",	"^X^W");

% rectangle stuff

  setkey("kill_rect", 		"^XRk");
  setkey("open_rect", 		"^XRo");
  setkey("insert_rect", 	"^XRy");
  setkey("copy_rect",	 	"^XRr");

%  On the IBM PC, the ^@ is an extended key like all the arrow keys.  The
%  default Jed bindings enable all these keys including the ^@.  See source
%  for details.

#ifdef MSDOS OS2
  setkey("smart_set_mark_cmd",		"^@^C");	
#else
  setkey("smart_set_mark_cmd",		"^@");	
#endif

setkey ("find_buffer_other_window",	"^X4b");
setkey ("find_alternate_file",		"^X^V");
setkey ("find_file_other_window",	"^X4f");

autoload ("find_file_other_window",	"emacsmsc");
autoload ("find_buffer_other_window",	"emacsmsc");
autoload ("find_alternate_file",	"emacsmsc");
autoload ("delete_blank_lines",		"emacsmsc");
autoload ("forward_sexp",		"emacsmsc");
autoload ("backward_sexp",		"emacsmsc");
autoload ("kill_sexp",			"emacsmsc");
autoload ("scroll_up_in_place",		"emacsmsc");
autoload ("scroll_down_in_place",	"emacsmsc");

%% misc functions

#iffalse
% Is this more emacs-like???  Anyone?
define emacs_pageup ()
{
   variable n = prefix_argument (-1);
   variable m;
   if (n == -1)
     {
	call ("page_up");
	return;
     }
   m = window_line ();
   go_up (n);
   recenter (m);
}
#endif

define emacs_mark_buffer()
{
   mark_buffer ();
   exchange_point_and_mark ();
}
setkey("emacs_mark_buffer", "^XH");
     
define emacs_open_line()
{
   newline();  go_left_1 ();
}
 setkey("emacs_open_line", "^O");
    
define emacs_recenter() { recenter(0); }

define transpose_chars ()
{
   variable c, err;
   err = "Top of Buffer";

   if (eolp()) go_left_1 ();
   !if (left(1)) error(err);
   c = what_char();
   del();
   go_right_1 ();
   insert_char(c);
}
  setkey("transpose_chars",  "^T");	


%%0 9 1 { "^U" exch string strcat "digit_arg" exch setkey } _for
%
%  Emacs Universal argument--- bound to ^U
%
define universal_argument ()
{
   variable n, key, count, msg, cu, force;
   n = 4; count = 0; cu = "C-u"; msg = cu; force = 0;
   
   forever
     {
	!if (force) !if(input_pending(10)) force = 1;
	
	if (force) 
	  {
	     message(strcat (msg, "-"));
	     update(0);
	  }
	
      
	msg = strcat(msg, " ");
	key = getkey();
	
	switch(key)
	  {
	   isdigit(char(())) :
	     key = key - '0';
	     count = 10 * count + key;
	     msg = strcat(msg, string(key));
	  }
	  {
	     case 21 :		       %  ^U
	     !if (count) n = 4 * n;
	     count = 0;
	     msg = strcat (msg, cu);
	  }
	  {
	    ungetkey(());
	     !if (count) count = n;
	     count = string(count);
	     n = strlen(count);
	     _for (n, 1, -1)
	       {
		  count; exch(); 
		  ungetkey(int (substr((), (), 1)));
	       }
	     ungetkey(27);
	     return;
	  }
     }
}
  setkey("universal_argument",  "^U");	

% The 8086 version of jed does not suppot yank-pop whereas jed386 does.  So,
% I do it this way so both executables can execute this file.
if (is_defined ("KILL_ARRAY_SIZE")) 
  {
     () = evalfile ("yankpop");
     setkey ("yp_kill_line", "^K");
     setkey ("yp_yank", "^Y");
     setkey ("yp_yank_pop", "\eY");
     setkey ("yp_kill_word", "\eD");
     setkey ("yp_bkill_word", "\e^?");
     setkey ("yp_copy_region_as_kill", "\eW");
     setkey ("yp_kill_region", "^W");
  }

   
