% This file should not be byte-compiled.
% It is loaded from site.sl and permits various flavors of jed to share the
% same set of S-Lang files. It is written in RPN for efficiency.
#ifdef MSWINDOWS XWINDOWS MOUSE
. "mouse" evalfile pop
# ifdef MSWINDOWS
   mouse_map_buttons (4, 2);	       %  map Right to Middle
   mouse_map_buttons (2, 4);	       %  map Middle to Right
# endif
#endif

#ifdef XWINDOWS
. "HOST" getenv =$1
. $1 strlen { "XJed@" $1 strcat x_set_window_name } if
. "skip_word" 		"^[[c" setkey      %/* shift-right */
. "bskip_word"		"^[[d" setkey      %/* shift-left */
. "goto_top_of_window"	"^[[a" setkey      %/* shift-up */
. "goto_bottom_of_window"	"^[[b" setkey      %/* shift-down */
. "beg_of_line"		"\e[H" setkey		% Home
. "eol_cmd"		"\e[[" setkey		% End
#endif


define goto_visible_eol ()
{
#ifdef HAS_LINE_ATTR
   if (down_1 ())
     {
	if (is_line_hidden ())
	  skip_hidden_lines_forward (1);
	go_left_1 ();
     }
#endif
   eol ();
}

define mark_to_visible_eol ()
{
   push_mark ();
   goto_visible_eol ();
}

define transpose_lines ()
{
   bol (); push_mark ();
#ifdef HAS_LINE_ATTR
   mark_to_visible_eol ();
   bufsubstr ();		       %  on stack
   go_right_1 ();
   del_region();
   skip_hidden_lines_backward (1);
   bol();
   insert(());
   newline();
   skip_hidden_lines_forward (1);      %  goes to bol
#else
   line_as_string ();                  %  on stack
   go_right_1 ();
   del_region();
   go_up_1 (); bol();
   insert(());
   newline();
   go_down_1 ();                               %  goes to bol
#endif
}

#ifdef HAS_LINE_ATTR
define set_selective_display ()
{
   variable c, arg = 1;
   c = prefix_argument (-1);
   
   if (c <= 1) arg = 0;
	
   push_spot ();
   bob ();
   do
     {
	bol_skip_white ();
	set_line_hidden (arg * (what_column () > c));
     }
   while (down_1 ());

   pop_spot ();
}
setkey ("set_selective_display", "^X$");

autoload ("folding_mode", "folding");
add_completion ("folding_mode");

variable Fold_Mode_Ok = 0;
define fold_mode ()
{
   if (Fold_Mode_Ok) folding_mode ();
}

#endif

#ifdef HAS_BLOCAL_VAR
define define_blocal_var (name, type, value)
{
   create_blocal_var (name, type);
   set_blocal_var (value, name);
}
#endif

#ifdef HAS_DFA_SYNTAX
set_highlight_cache_dir (extract_element(JED_LIBRARY, 0, ','));
#else
% dummy functions that enable jed to work in mixed environments
define enable_highlight_cache (x, y);
define define_highlight_rule (x,y,z);
define build_highlight_table (x);
#endif
