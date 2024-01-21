% Python mode 
% File: pymode.sl v1.0
%
% For editing source code written in the Python programming language.
% Provides basic compatibility with Python mode under real Emacs
%
% Authors: Harri Pasanen <hpa@iki.fi>
%          Brien Barton <barton@simsg1.mdc.com>

% See python_mode function for usage.

$1 = "python";

!if (keymap_p ($1)) make_keymap ($1);
definekey ("py_backspace_key", "^?", $1);
definekey ("newline", "^M", $1);
definekey ("py_newline_and_indent", "^J", $1);
definekey ("py_comment", "^C#", $1);
definekey ("py_uncomment", "^C3", $1);
definekey ("py_shift_region_right", "^C>", $1);
definekey ("py_shift_region_left", "^C<", $1);
definekey ("py_exec_region", "^C^C", $1);


% Set the following to your favourite indentation level
!if (is_defined ("Py_Indent_Level")) { % users can set this in .jedrc
   variable Py_Indent_Level = 4;
}

define py_line_ends_with_colon()
{
   eol();
   if (bfind_char(':')) {
      go_right(1);
      skip_white();
      if (eolp() or looking_at_char('#'))
	 return 1;
   }
   return 0;
}

define py_indent_calculate()
{  % return the indentation of the previous python line
   variable col = 0;
   
   EXIT_BLOCK
     {
	pop_spot ();
	return col;
     }
   
   % go to previous non blank line
   push_spot_bol ();
   !if (re_bsearch ("[^ \t\n]"))
     return;
   bol_skip_white();
   
   col = what_column() - 1;

   if (py_line_ends_with_colon())
     col += Py_Indent_Level;   
}

define py_indent_line()
{
   variable col;

   col = py_indent_calculate();
   bol_trim ();
   whitespace( col );
}

define py_comment_line() {
    bol();
    insert("##");
}

define py_comment_region()
{
    narrow();
    bob();
    do {
	py_comment_line();
    } while (down_1 ());
    widen();
}


define py_comment() {
    push_spot();
    if (markp()) {
	py_comment_region();
    } else {
	py_comment_line();
    }
    pop_spot();
}

define py_uncomment_line() {
    bol_skip_white();
    while (looking_at("#")) del();
}


define py_uncomment_region()
{
    narrow();
    bob();
    do {
	py_uncomment_line();
    } while (down_1 ());
    widen();
}

define py_uncomment() {
    push_spot();
    if (markp()) {
	py_uncomment_region();
    } else {
	py_uncomment_line();
    }
    pop_spot();
}

define py_backspace_key()
{
   variable col;
   
   col = what_column();
   push_spot();
   bol_skip_white();
   if ((eolp() or (col == what_column()))
       and col > Py_Indent_Level) {
      pop_spot();
      bol_trim ();
      whitespace ( col - Py_Indent_Level - 1 );
   }
   else {
      pop_spot();
      call("backward_delete_char_untabify");
   }
}


define py_shift_region_right()
{
   variable n;
   check_region (1);		       %  spot_pushed, now at end of region
   n = what_line ();
   pop_mark_1 ();
   loop (n - what_line ())
     {
	bol_skip_white();
	whitespace(Py_Indent_Level);
	go_down_1 ();
     }
   pop_spot();
}

define py_shift_region_left()
{
   variable n;
   
   check_region (1);
   n = what_line ();
   pop_mark_1 ();
   loop (n - what_line ())
     {
	bol_skip_white();
	if (what_column() > Py_Indent_Level) {
	   push_mark();
	   goto_column(what_column() - Py_Indent_Level);
	   del_region();
	}
	go_down_1 ();
     }
   pop_spot();
}

define py_newline()
{
   newline();
}

define py_newline_and_indent()
{
   newline();
   py_indent_line();
}

define py_exec_region() {
    % Run python interpreter on current region if one is defined, otherwise
    % on entire buffer.  Display output in *shell-output* buffer window.
    variable oldbuf, thisbuf;
    variable tmpfile = "_python.tmp";
#ifdef UNIX
    tmpfile = Sprintf("%s/%s", getenv("HOME"), tmpfile, 2);
#endif
#ifdef VMS
    tmpfile = Sprintf("%s%s", getenv("HOME"), tmpfile, 2);
#endif
    thisbuf = whatbuf();
    if (markp()) {	% region is defined
	% Check if 1st line starts in column 1
	exchange_point_and_mark();
	bol_skip_white();
	if (what_column() > 1) {
	    % Workaround in case block is indented
	    write_string_to_file("if 1:\n", tmpfile); bol();
	}
	exchange_point_and_mark();
	append_region_to_file(tmpfile);
    } else {		% create region containing entire buffer
	push_spot_bob ();
	push_mark_eob ();
	write_region_to_file(tmpfile);
	pop_spot();
    }
    oldbuf = pop2buf_whatbuf("*shell-output*"); erase_buffer ();
#ifdef UNIX
    run_shell_cmd(Sprintf("python %s 2>&1", tmpfile, 1));
#else
    run_shell_cmd(Sprintf("python %s", tmpfile, 1));
#endif
    () = delete_file(tmpfile);

    % try to restore any window that got replaced by the shell-output
    if (strlen(oldbuf) and (strcmp(oldbuf, "*shell-output*") != 0)
	and (strcmp(thisbuf, oldbuf) != 0)) {
	splitwindow(); sw2buf(oldbuf); pop2buf("*shell-output*");
    }
}


create_syntax_table ($1);
define_syntax ("#", "", '%', $1);		% comments
define_syntax ("([{", ")]}", '(', $1);		% delimiters
define_syntax ('"', '"', $1);			% quoted strings
define_syntax ('\'', '\'', $1);			% quoted strings
define_syntax ('\\', '\\', $1);			% continuations
define_syntax ("-0-9a-zA-Z_", 'w', $1);		% words
define_syntax ("-+0-9a-fA-FlLxX.", '0', $1);	% Numbers
define_syntax (",;:", ',', $1);			% punctuation
define_syntax ("%-+/&*=<>|!~^", '+', $1);	% operators
set_syntax_flags ($1, 1);			% keywords ARE case-sensitive

() = define_keywords ($1, "ifinisor", 2); % all keywords of length 2
() = define_keywords ($1, "anddefdelfornottry", 3); % of length 3 ....
() = define_keywords ($1, "elifelseexecfrompass", 4);
() = define_keywords ($1, "breakclassprintraisewhile", 5);
() = define_keywords ($1, "exceptglobalimportlambdareturn", 6);
() = define_keywords ($1, "finally", 7);
() = define_keywords ($1, "continue", 8);

%!% Prototype: Void python_mode ();
%!% A major mode for editing python files.
%!% 
%!% The following keys have python specific bindings:
%!%
%!% Backspace deletes line
%!% TAB indents line
%!% ^J  does newline and indent
%!% ^C# comments region or current line
%!% ^C> shifts region right
%!% ^C< shifts region left
%!% ^C^C executes the region, or the buffer if region not marked.
%!% 
%!% Hooks: @python_mode_hook@
%!% 
%!% Related Variables: @Py_Indent_Level@
%!% Related Functions: @set_mode@, @c_mode@
define python_mode ()
{
   variable python = "python";

   TAB = 8;
   set_mode (python, 0x4); % flag value of 4 is generic language mode
   use_keymap(python);
   set_buffer_hook ("indent_hook", "py_indent_line");
   use_syntax_table (python);
   runhooks("python_mode_hook");
}
