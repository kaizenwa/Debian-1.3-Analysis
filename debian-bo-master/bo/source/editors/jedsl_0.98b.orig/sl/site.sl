% -*- mode: slang; mode: fold; -*-
% Note: This file has been folded.

%{{{ Description of site.sl file
% 
% 		      Site specific initialiation file.
% 
% This file must be present in the JED_LIBRARY.  JED loads it first--- even
% before reading command line arguments.  The command line arguments are then
% passed to a hook declared in this file for further processing.
% 
% In addition to some hooks, this file declares some autoloads for various
% functions and defines utility functions.  Any user specific stuff should be
% placed in the jed.rc (.jedrc) user startup file.  Only put here what you
% believe EVERY user on your system should get!
% 
% The best way to make changes in this file is to put all your changes in a
% separate file, defaults.sl.  defaults.sl is NOT distributed with JED.  Code
% at the edn of this file checks for the existence of `defaults.sl' and loads
% it if found. Functions occuring in this file (site.sl) may be overloaded in
% defaults.sl. Making changes this way also makes it easier to upgrade to
% future JED versions.
% 

%}}}
%{{{ Special note on syntax of some functions
% --------------------------------------------------------------------------
% Note: Some of the small routines here have been written in such a way that
% the stack based nature of the language is exploited.  That is, instead of
% writing: 
%        define sum (a, b) { return a + b; }
% I use:
%        define sum () { () + (); }
% The former parses to the bytecode:  =b =a a b + return
% where as the latter parses to:      +
% which is 6 times faster and 6 times more memory efficient!
% --------------------------------------------------------------------------

%}}}
%{{{ Global Variables
variable Null_String = "";

%!% A Comma separated list of info directories to search.
variable Info_Directory;
variable Jed_Bin_Dir;

%!% Prototype: Integer C_CONTINUED_OFFSET = 2;
%!% This variable controls the indentation of statements that are continued
%!% onto the next line as in the following example:
%!% @ if (something)
%!% @   continued_statement ();
%!% @ else
%!% @   another_continued_statement ();
%!% Related Variables: @C_BRA_NEWLINE@, @C_BRACE@, @C_INDENT@, @C_Colon_Offset@
variable C_CONTINUED_OFFSET = 2;

%!% Integer C_Colon_Offset = 1;
%!% This variable may be changed to adjust the indentation of @case@ statements
%!% in C-Mode.
%!% See also: @c_mode@
%!% Related Variables: @C_BRA_NEWLINE@, @C_BRACE@, @C_INDENT@, @C_Colon_Offset@
variable C_Colon_Offset = 1;

%!% Prototype: Integer C_Preprocess_Indent = 1;
%!% This variable controls the indentation of preprocessor directives in
%!% C-mode.  
%!% See also: @c_mode@
%!% Related Variables: @C_BRA_NEWLINE@, @C_BRACE@, @C_INDENT@, @C_Colon_Offset@
variable C_Preprocess_Indent = 1;

%!% Column to begin a C comment--- used by c_make_comment
variable C_Comment_Column = 40;

%!% Prototype: Integer C_INDENT = 3;
%!% This value determines the number of columns the current line is indented
%!% past the previous line containing an opening @'{'@ character.
%!% Related Variables: @C_BRACE@, @C_BRA_NEWLINE@.
variable C_INDENT = 3;

%!% Prototype: Integer C_BRACE = 2;
%!% This is a C-mode variable that specifies how much an opening brace
%!% should be indented compared its surrounding block. 
%!% Related Variables: @C_INDENT@, @C_BRA_NEWLINE@
variable C_BRACE = 2;

%!% Prototype: Integer C_BRA_NEWLINE = 1;
%!% This variable is used by the indentation routines for the C langauge.
%!% If it is non-zero, the @'{'@ character will be placed on a line by
%!% itself when one presses the @'{'@ character.  For K&R indentation style, 
%!% set this variable to zero.
%!% Related Variables: @C_INDENT@, @C_BRACE@
variable C_BRA_NEWLINE = 1;

#ifdef UNIX
if (OUTPUT_RATE > 9600) OUTPUT_RATE = 0;   %% coming through a network?
#endif

% These are for compatibility

variable REPLACE_PRESERVE_CASE = 0;
variable LAST_SEARCH = Null_String;

%}}}
%{{{ Compatibility functions

define define_keywords ()
{
   define_keywords_n (0);
}

define save_search_string ()
{
   LAST_SEARCH = ();
}

% define this now so lib files can refer to it.
define compile_parse_errors ();

%}}}
%{{{ Utility functions required below (dircat, etc)
%{{{ verror, vinsert, vmessage

%!% Prototype: Void verror (String fmt, ..., Integer n)
%!% This function is like @error@ except that it takes a variable number of
%!% arguments.  This function is equivalent to @error (Sprintf (fmt,...,n))@
%!% See also: @vmessage@, @error@, @message@, @Sprintf@
define verror ()
{
   % On stack: fmt ... n
   Sprintf; error;
}

%!% Prototype: Void vmessage (String fmt, ..., Integer n)
%!% This function is like @message@ except that it takes a variable number of
%!% arguments.  This function is equivalent to @error (Sprintf (fmt,...,n))@
%!% See also: @message@, @error@, @message@, @Sprintf@, @flush@, @verror@
define vmessage ()
{
   % On stack: fmt ... n
   Sprintf; message;
}

%!% Prototype: Void vinsert (String, fmt,... Integer n);
%!% This function is like @insert@ except that it takes a variable number
%!% of arguments and a format string.  Formally, it is equivalent to
%!% @insert (Sprintf (fmt,...n))@.
%!% See also: @insert@, @Sprintf@, @insert_char@
define vinsert ()
{
   Sprintf; insert;
}

%}}}

%{{{ dircat

%!% A function to contat a directory with a filename.  Basically checks
%!% for the final slash on the dir and adds on if necessary
define dircat(dir, file)
{
   variable n = strlen(dir);

   if (n)
     {
#ifdef MSDOS OS2
	variable slash = "\\";
	if (strcmp(substr(dir, n, 1), slash)) dir = strcat(dir, slash);
#endif
#ifdef UNIX
	variable slash = "/";
	if (strcmp(substr(dir, n, 1), slash)) dir = strcat(dir, slash);
#endif
#ifdef VMS
	% convert a.dir;1 to [.a] first
	variable f1, d1;
	dir = extract_element(dir, 0, ';');
	f1 = extract_element(dir, 1, ']');
	if (strlen(f1)) f1 = strcat(".", extract_element(f1, 0, '.'));
	d1 = extract_element(dir, 0, ']');
	strcat(d1, f1);
	if (':' != int(substr(dir, strlen(dir), 1))) strcat((), "]");
	dir = ();
#endif
     }
   expand_filename (strcat (dir, file));
}

%}}}

%{{{ str_replace_all (str, old, new)
%!% Prototype: String str_replace_all (str, old, new);
%!% Replace all occurances of @old@ in @str@ with @new@ and return the
%!% result.
%!% Related Functions: @str_replace@, @replace_cmd@
define str_replace_all (str, old, new)
{
   while (str_replace (str, old, new)) 
     str = ();
   
   str;
}

%}}}

%{{{ strncat (n)
%!% Prototype: Void strncat (String a, String b, ..., Integer n);
%!% Returns concatenated string "abc..."
define strncat (n)
{
   n--;
   loop (n) strcat ();
}

%}}}

%{{{ bol_skip_white ()
%!% Prototype: Void bol_skip_white ();
%!% This function combines the two functions @bol@ and @skip_white@ into a
%!% single operation.  That is, it moves the point to the beginning of the
%!% line and then skips over whitespace to the first non-whitespace character.
%!% See also: @bol@, @skip_white@, @skip_chars@
define bol_skip_white ()
{
   bol (); skip_white ();
}

%}}}

%{{{ bskip_white ()
%!% Prototype: Void bskip_white ();
%!% This function skips backward over whitespace.
%!% Note: it does not cross lines.
%!% See also: @skip_white@, @bskip_chars@
define bskip_white ()
{
   bskip_chars ("\t ");
}

%}}}

%{{{ path2list(path)
%% Convert Unix- or OS/2- style path to comma-delimited list
define path2list(path)
{
#ifdef VMS
    path;
#else
    variable n, pathsep, a_path;
 
#ifdef UNIX
    pathsep = ':';
#else
    pathsep = ';';
#endif
 
    n = 0; Null_String; Null_String;
    while (a_path = extract_element(path, n, pathsep), strlen(a_path))
      {
	 strncat ((), (), a_path, 3);
	 % strcat((), strcat((), a_path));
 	","; n++;
      }
    pop();
#endif
}

%}}}

%{{{ file_type(file)
%!% returns type of file.  e.g., /usr/a.b/file.c --> c
define file_type(file)
{
   variable n;
   file = extract_filename(file);
   
   n = is_substr(file, ".");
   !if (n) return (Null_String);
   
   substr(file, n + 1, strlen(file));
}

%}}}

%{{{ expand_jedlib_file(f)
%!% Search for FILE in directories specified by JED_LIBRARY returning
%!% expanded pathname if found or the Null string otherwise.
define expand_jedlib_file(f)
{
   variable n = 0, dir, file;
   forever
     {
	dir = extract_element(JED_LIBRARY, n, ',');
	!if (strlen(dir)) return (Null_String);
	file = dircat(dir, f);
	if (file_status(file) == 1) break;
	++n;
     } 
   file;
}

%}}}

%{{{ find_jedlib_file(file)
%!% find a file from JED_LIBRARY, returns number of lines read or 0 if not 
%!% found.
define find_jedlib_file(file)
{
   file = expand_jedlib_file(file);
   !if (strlen(file)) return(0);
   find_file(file);
}

%}}}

%{{{ parse_filename(fn)
%!% Prototype: (dir, file) = define parse_filename(fn)
%!% breaks a filespec into dir filename--- 
%!% this routine returns dir and filename such that a simple strcat will
%!% suffice to put them together again.  For example, on unix, /a/b/c
%!% returns /a/b/ and c
define parse_filename(fn)
{
   variable f, dir, n;
      
   f = extract_filename(fn);
   n = strlen(fn) - strlen(f);
   dir = substr(fn, 1, n);
   dir; f;
}

%}}}

%}}}
%{{{ Jed info and bin directories

#ifdef VMS
   Info_Directory = strcat(JED_ROOT, "[info]");
   Jed_Bin_Dir = strcat(JED_ROOT, "[bin]");
#else
   Info_Directory = dircat(JED_ROOT, "info");
   Jed_Bin_Dir = dircat(JED_ROOT, "bin");
#endif

#ifdef UNIX
Info_Directory = strcat (Info_Directory, ",/usr/info,/usr/local/info");
#endif

$1 = getenv("INFOPATH");
if (strlen($1)) Info_Directory = path2list($1);
  
%}}}
%{{{ Some key definitions

% These two are for compatability:
  setkey("search_forward", "^Ff");
  setkey("search_backward", "^Fb");
  setkey("skip_word", "^[^[[C");  %escape right arrow.
  setkey("bskip_word", "^[^[[D");  %escape left arrow
  setkey("upcase_word", "^[U");
  setkey("downcase_word", "^[L");
  setkey("capitalize_word", "^[C");
  setkey("emacs_escape_x", "^[X");
  setkey("undo", "^Xu");  %% Also ^_ but vtxxx have problems with it
  setkey("transpose_lines", "^X^T");
  setkey("help_prefix", "^H");
  %setkey("indent_line_cmd", "^I");
  %setkey("insert_colon_cmd", ":");
  % setkey("newline_and_indent_cmd", "^M");
  setkey("do_shell_cmd", "^[!");
  setkey("find_tag", "^[.");
  setkey("dabbrev", "\e/");
  setkey("save_buffers", "^Xs");
  setkey("whatpos", "^X?");
  setkey("list_buffers", "^X^B");
  setkey ("set_fill_column", "^Xf");
  setkey ("compile_parse_errors",    "^X'");

% 16 bit systems do not get these features
if (is_defined ("KILL_ARRAY_SIZE"))
{
   setkey ("reg_insert_register", "^XG");
   setkey ("reg_copy_to_register", "^XX");
}

#ifdef UNIX OS2
  setkey("ispell", "^[$");
#endif
#ifndef MSDOS OS2
  setkey("mail", "^Xm");
#endif

#ifdef MSDOS OS2
 setkey(" /", "\eOQ");
 setkey(" *", "\eOR");
 setkey(" +", "\eOm");
 setkey(" -", "\eOS");
 setkey("toggle_overwrite", "\xE0R");     %/* insert key */
 setkey("toggle_overwrite", "\eOp");     %/* insert key */
#endif

%}}}
%{{{ Autoloads
$0 = _stkdepth ();
  _autoload("c_mode",			"cmode",
	    "slang_mode",		"slmode",
	    "java_mode",		"javamode",
	    "find_binary_file",		"binary",
	    "jed_easy_help",		"jedhelp",
	    "query_replace_match",	"regexp",
	    "re_search_forward",	"regexp",
	    "re_search_backward",	"regexp",
	    "dired",			"dired",
	    "calendar",			"cal",
	    "menu_main_cmds",		"menu",
	    "trim_buffer",		"util",
	    "occur",			"occur",
	    "info_reader",		"info",
	    "info_mode",			"info",
	    "info_find_node",		"info",
	    "list_buffers",		"buf",
	    "append_region",		"buf",
	    "write_region",		"buf",
	    "recover_file",		"buf",
	    "most_mode",			"most",
	    "run_most",			"most",
	    "compile",			"compile",
	    "sort",			"sort",
	    "sort_using_function",	"sort",
	    "untab",			"untab",
	    "fortran_mode",		"fortran",
	    "save_buffers",		"buf",
	    "sh_mode", 			"shmode",
	    "ps_mode", 			"pscript",
	    "rot13",			"rot13",
	    "search_forward",		"search",
	    "search_backward",		"search",
	    "replace_cmd",		"search",
	    "replace_across_buffer_files","replace",
	    "isearch_forward",		"isearch",
	    "isearch_backward",		"isearch",
	    "shell",			"shell",
	    "mute_set_mute_keys",	"mutekeys",
#ifndef OS2 UNIX
	    "run_shell_cmd",		"shell",
#endif
	    "html_mode",		"html",
	    "do_shell_cmd",		"shell",
	    "find_tag",			"ctags",
	    "apropos",			"help",
	    "expand_keystring",		"help",
	    "describe_bindings",	"help",
	    "describe_function",	"help",
	    "describe_variable",	"help",
	    "help_for_function",	"help",
	    "where_is",			"help",
	    "showkey",			"help",
	    "describe_mode",		"help",
	    "format_paragraph_hook",	"tmisc",
	    "dabbrev",			"dabbrev",
	    "tex_mode",			"tex",
	    "bibtex_mode",		"bibtex",
	    "latex_mode",		"latex",
	    "bkmrk_goto_mark",          "bookmark",
	    "bkmrk_set_mark",           "bookmark",
	    "add_keyword",              "syntax",
	    "lisp_mode",		"lisp",
	    "perl_mode",		"perl",
	    "vhdl_mode",		"vhdlmode",
	    "spice_mode",		"spicemod",
	    "verilog_mode",		"verilog",

%%
%% By default, tabs are every TAB columns (default 8).  Calling this function
%% will allow the user to set the tabs arbitrarily and bind the TAB key
%% appropriately.
	    "edit_tab_stops",		"tabs",
	    "tab_to_tab_stop",		"tabs",
	    "append_string_to_file",	"misc",
	    "write_string_to_file",	"misc",
	    "make_tmp_buffer_name",	"misc",
	    "abbrev_mode",		"abbrev",
	    "set_abbrev_mode",		"abbrev",
	    "save_abbrevs",		"abbrmisc",
	    "define_abbreviation",	"abbrmisc",

#ifdef VMS
	    "mail",			"mail",  % See also sendmail.sl
	    "mail_format_buffer",	"mail",
	    "dcl_mode",			"dcl",
	    "vms_help",			"vmshelp",
#endif

#ifdef UNIX OS2
	    "unix_man",			"man",
	    "ispell",			"ispell",
#endif
#ifdef UNIX
	    "rmail",			"rmail",
	    "mail",			"sendmail",
	    "mail_format_buffer",	"sendmail",
#endif
	    "idl_mode",			"idl",
	    "nroff_mode",		"nroff",
	    "modeline_hook2",		"modehook",

	    (_stkdepth () - $0) / 2);	       %  matches start of _autoload

if (is_defined ("KILL_ARRAY_SIZE"))
{
   autoload ("reg_insert_register", "register");
   autoload ("reg_copy_to_register", "register");
}

%}}}
%{{{ More Utility functions

%{{{ Simple editing and movement functions

%!% Prototype: Void go_up (Integer n);
%!% Move up 'n' lines.
%!% See also: up, go_down
define go_up() { () = up(); }

%!% Prototype: Void up_1 ();
%!% Move up 1 line.  If successful, returns 1 otherwise it returns 0.
%!% See also: @up@, @go_down@, @go_up@, @go_up_1@
define up_1() { up(1); }

%!% Prototype: Void go_up_1 ();
%!% Move up exactly 1 line if possible.
%!% See also: up, go_down
define go_up_1 () { () = up_1(); }

%!% Prototype: Void go_down (Integer n);
%!% Move down 'n' lines.
%!% See also: go_up, down
define go_down() { () = down(); }

%!% Prototype: Void down_1 ();
%!% Move down exactly one lines.  If sucessful, 1 is returned otherwise
%!% zero is returned.
%!% See also: @go_up@, @down@, @go_down_1@
define down_1 () {  down (1); }

%!% Prototype: Void go_down_1 ();
%!% Move down one lines.
%!% See also: go_up, down
define go_down_1 () { () = down_1(); }

%!% Prototype: Void go_left (Integer n);
%!% Move backward 'n' characters.
%!% See also: left, go_right
define go_left() { () = left();}

%!% Prototype: Void go_right (Integer n);
%!% Move forward 'n' characters.
%!% See also: right, go_left
define go_right() { () = right();}

%!% Prototype: Void go_right_1 ();
%!% Move forward 1 characters.
%!% See also: right, go_left
define go_right_1() { go_right (1)}

%!% Prototype: Void go_left_1 ();
%!% Move forward 1 characters.
%!% See also: left, go_left
define go_left_1() { go_left (1)}


%!% insert a character into a buffer.
%!% This function should be called instead of 'insert' when it is desired to
%!% insert a 1 character string.  Unlike 'insert', insert_char takes an integer
%!% argument.  For example, 
%!%    'x' insert_char
%!% and 
%!%    "x" insert
%!% are functionally equivalent but insert_char is more memory efficient.
define insert_char () 
{ 
   insert (char( () ));
}


%!% Prototype: Void newline (Void);
%!% insert a newline in the buffer at point.
%!% See also: insert, insert_char
define newline () 
{ 
   insert_char('\n');
}

   
%!% insert a single space into the buffer.
define insert_single_space ()
{
   insert_char(' ');
}

%!% Prototype: Integer looking_at_char (Integer ch);
%!% This function returns non-zero if the character at the current editing
%!% point is 'ch' otherwise it retuns zero.  This function performs a case 
%!% sensitive comparison.
define looking_at_char ()
{
   what_char () == ();
}


%}}}


% For compatability
define shell_cmd ()
{
   () = run_shell_cmd ();
}

%!% Prototype: Void runhooks (String fun)
%!% if S-Lang function 'fun' is defined, execute it.  It does nothing if 'fun'
%!% does not exist.
define runhooks(fun)
{
   if (is_defined (fun)) eval(fun);
}

%!% Prototype: Void local_setkey (String fun, String key);
%!% This function is like 'setkey' but unlike 'setkey' which operates on the
%!% global keymap, 'local_setkey' operates on the current keymap which may or
%!% may not be the global one.
%!% See also: setkey, definekey, local_unsetkey
define local_setkey ()
{
   definekey((), (), what_keymap());
}

%!% Prototype: Void local_unsetkey (String key);
%!% This function is like 'unsetkey' but unlike 'unsetkey' which unsets a key
%!% from the global keymap, 'local_unsetkey' operates on the current keymap
%!% which may or may not be the global one.
%!% See also: unsetkey, undefinekey, local_setkey
define local_unsetkey ()
{
   undefinekey( (), what_keymap());
}

%!% Prototype: Void pop_n (Integer n);
define pop_n ()
{
   loop () pop ();
}

%!% Prototype: String make_tmp_file (String base);
%!% This function returns a unique file name that begins with @base@.
define make_tmp_file(base)
{
   variable pid = getpid(), file, n = 1000;
   while (n)
     {
	file = strcat(base, string(pid));
	!if (file_status(file)) return (file);
	pid++;
     }
   error ("Unable to create a tmp file!");
}

%}}}
%{{{ More functions

%!% Prototype: Void pop_mark_0 ();
%!% Since @pop_mark@ is used so often with an argument of @0@, this function 
%!% is simply equivalent to @pop_mark(0)@.
%!% See also: @pop_mark@, @pop_mark_1@
define pop_mark_0 ()
{
   pop_mark (0);
}

%!% Prototype: Void pop_mark_1 ();
%!% Since @pop_mark@ is used so often with an argument of @1@, this function 
%!% is simply equivalent to @pop_mark(1)@.
%!% See also: @pop_mark@, @pop_mark_0@
define pop_mark_1 ()
{
   pop_mark (1);
}

%!% Prototype: Void goto_spot ();
%!% This function returns to the position of the last pushed spot.  The spot
%!% is not popped.
%!% See also: @push_spot@, @pop_spot@, @create_user_mark@
define goto_spot ()
{
   pop_spot ();
   push_spot ();
}

%!% Prototype: Void push_spot_bob ();
%!% The function sequence @push_spot (); bob ();@ occurs so often that
%!% it makes sense to have a single function that performs this task.
%!% See also: @push_spot@, @bob@, @pop_spot@, @push_spot_bol@
define push_spot_bob ()
{
   push_spot ();
   bob ();
}

%!% Prototype: Void push_spot_bol ();
%!% The function sequence @push_spot (); bol ();@ occurs so often that
%!% it makes sense to have a single function that performs this task.
%!% See also: @push_spot@, @bol@, @pop_spot@, @push_spot_bob@
define push_spot_bol ()
{
   push_spot ();
   bol ();
}

%!% Prototype: Void push_mark_eol ();
%!% The function sequence @push_mark (); eol ();@ occurs so often that
%!% it makes sense to have a single function that performs this task.
%!% See also: @push_mark@, @eol@, @pop_mark@, @push_mark_eob@
define push_mark_eol ()
{
   push_mark ();
   eol ();
}

%!% Prototype: Void push_mark_eob ();
%!% The function sequence @push_mark (); eob ();@ occurs so often that
%!% it makes sense to have a single function that performs this task.
%!% See also: @push_mark@, @eob@, @pop_mark@, @push_mark_eob@
define push_mark_eob ()
{
   push_mark ();
   eob ();
}

%!% Prototype: mark_buffer ();
%!% This function marks the whole buffer leaving the point at the end
%!% of the buffer.
%!% See also: @push_mark@, @pop_mark@, @bob@, @eob@
define mark_buffer ()
{
   bob ();
   push_mark_eob ();
}


%!% Prototype: String bufsubstr_delete ()
%!% This functions returns the contents of a region defined my the mark
%!% and the current point.  The region will be deleted.
%!% See also: @bufsubstr@
define bufsubstr_delete ()
{
   () = dupmark ();
   bufsubstr ();		       %  on stack
   del_region ();
}

%!% Prototype: Void del_eol ();
%!% This function deletes from the current position to the end of the line.
%!% See also: @del@, @delete_line@, @del_through_eol@
define del_eol ()
{
   push_mark_eol ();
   del_region ();
}

%!% Prototype: del_through_eol ();
%!% This function deletes all text from the current point through the end of
%!% the line.
%!% See also: @del@, @del_eol@, @del_region@
define del_through_eol ()
{
   del_eol ();
   !if (eobp ()) del ();
}

%!% Prototype: String line_as_string ()
%!% This function returns the current line as a string.  This does not include
%!% the newline character at the end of the line.  The editing point is left
%!% at the end of the line.  That is, this function does not preserve the point.
%!% See also: @bufsubstr@
define line_as_string ()
{
   bol (); push_mark_eol (); bufsubstr ();
}

define double_line ()
{
   POINT;
   line_as_string ();		       %  on stack
   newline();
   insert(());
   =POINT;
}

%!% Prototype: Void bol_trim ();
%!% Move to beginning of line and remove whitespace.
%!% See also: @bol@, @trim@
define bol_trim ()
{
  bol (); trim ();
}

%!% Prototype: Void eol_trim ();
%!% Move to end of line and remove whitespace.
%!% See also: @eol@, @trim@
define eol_trim ()
{
  eol ();
  trim ();
}

%}}}

%{{{ Backup and autosave functions

variable No_Backups = 0;

% returns backup filename.  Arguments to function are dir and file.
define make_backup_filename(dir, file)
{
#ifdef UNIX
   !if (strcmp (dir, "/tmp/")) return Null_String;
   strncat (dir, file, "~", 3);
#endif
#ifdef MSDOS OS2
   variable type;
#ifdef OS2
   !if (IsHPFSFileSystem(dir)) {
#endif

      % There are several things to worry about.  Here just break up the 
      % filename and truncate type to 2 chars and paste it back.
      % note that this takes a name like file.c and produces file.c~
      % Also, note that if the type is empty as in 'file', it produces 
      % 'file.~'

      type = file_type(file);
      file = strncat (extract_element(file, 0, '.'), ".", substr(type, 1, 2), 3);
      

#ifdef OS2
   }
#endif
   strncat (dir, file, "~", 3);
#endif  
}


% returns autosave filename.  Arguments to function are dir and file.
define make_autosave_filename(dir, file)
{
#ifdef VMS
   Sprintf ("%s_$%s;1", dir, file, 2);
#endif
   
#ifdef UNIX
   Sprintf ("%s#%s#", dir, file, 2);
#endif
   
#ifdef MSDOS OS2
#ifdef OS2
   !if (IsHPFSFileSystem(dir)) 
     {
#endif
	
	variable type = file_type(file);
	file = substr(extract_element(file, 0, '.'), 1, 7);
	if (strlen(type)) file = strcat(file, ".");
	file = strcat (file, type);

#ifdef OS2
     }
#endif
   
   strncat (dir, "#", file, 3);
#endif
}


%}}}
%{{{ Some interactive functions (goto_line, column, M-x)

%{{{ emacs_escape_x()
define emacs_escape_x()
{
   variable f = Null_String, i = 0;
  
   if (MINIBUFFER_ACTIVE)
     {
	call("evaluate_cmd");
	return;
     }
   
   forever
     {
	f = str_replace_all (f, "-", "_");
	if (is_internal(f)) 
	  {
	     call(f);
	     return;
	  }
	
	if (is_defined(f))
	  {
	     eval(f);
	     return;
	  }
	
	!if (EXECUTING_MACRO or DEFINING_MACRO)
	  {
	     if (i == 1) ungetkey(13);
	     ungetkey(' ');
	     ++i;
	  }
	
	f = read_with_completion("M-x", Null_String, f, 'F')
     } 
}

%}}}

define goto_line_cmd()
{
   read_mini("Goto line:", Null_String, Null_String);
   goto_line(integer(()));
}

define goto_column_cmd()
{
   read_mini("Goto Column:", Null_String, Null_String);
   goto_column(integer(()));
}

%;; scroll other window macros-- bind them yourself
define next_wind_up()
{
   otherwindow();  call("page_up");
   loop (nwindows() - 1) otherwindow();
}

define next_wind_dn()
{
   otherwindow();  call("page_down");
   loop (nwindows() - 1) otherwindow();
}

%!% display row and column information in minibuffer
define whatpos ()
{
   variable max_lines;   
   push_spot (); eob (); max_lines = what_line (); pop_spot ();
   vmessage ("%s, Line %d of %d lines, Column %d",  
	     count_chars (), what_line(), max_lines, what_column (),
	     4);
}

define goto_top_of_window ()
{
   go_up (window_line () - 1);
}

define goto_bottom_of_window ()
{
   go_down (window_info ('r') - window_line ());
}

%}}}
%{{{ Mode functions and settings
%!%  Mode for indenting and wrapping text
%!%  Functions that affect this mode include:
%!%
%!%    Function:                     Default Binding:
%!%      indent_line                 TAB
%!%      newline_and_indent          RETURN
%!%      format_paragraph                ESC Q
%!%      narrow_paragraph                ESC N
%!%
%!%  Variables include:
%!%      WRAP_INDENTS, WRAP
%!%      TAB, TAB_DEFAULT
define text_mode()
{
   set_mode("Text", 1);
   use_keymap("global");
   runhooks ("text_mode_hook");
}

%!%  Generic mode not designed for anything in particular.
%!%  See:  text_mode, c_mode
define no_mode()
{
   set_mode(Null_String, 0);
   use_keymap("global");
}

% Function prototypes
% These 'functions' are only here to initialize function pointers.
define _function_pop_0 (x) {0}
define _function_return_1 () {1}

%!% called from mode_hook.  Returns 0 if it is desired that control return
%!% to mode_hook or 1 if mode hook should exit after calling mode_hook_ptr
variable mode_hook_pointer = &_function_pop_0;

variable Default_Mode = &text_mode;



% Emacs allows a mode definition on the first line of a file
% -*- mode: MODENAME; VAR: VALUE; ... -*-
% which can also include values of local variables 

%!% check first line for the simplest Emacs mode statement
%!% -*- modename -*-
define modeline_hook()
{
   variable mode = Null_String, extra_hook;
   push_spot_bob ();
   go_down (4);
#iffalse
   () = bsearch ("-*- END -*-");
#endif
   push_mark (); bob ();
   narrow (); 
   
   % #!/bin/sh, #!/usr/local/bin/perl, #!/bin/csh -f ...
#ifdef 0
   if (looking_at("#!")) mode = "sh";
#endif
   
   if (re_fsearch("^#!/[^ ]+/\\([^ ]+\\)"))
     mode = regexp_nth_match (1);
   
   if (re_fsearch ("-\\*- *\\([-A-Za-z_]+\\) *-\\*-"))
     mode = strlow (regexp_nth_match (1));
   
   bob ();
   % -*- mode: VALUE -*- or -*- eval: VALUE -*-
   extra_hook = re_fsearch ("-\\*- *.+:.+ *-\\*-");

   widen ();
   
   EXIT_BLOCK
     {
	mode = ();
	if (extra_hook) (mode + modeline_hook2 ()); else mode;
	pop_spot ();		% restore place
     }
   
   if ( strlen(mode) )
     {
	variable mstr = "_mode";
	mode = str_replace_all (mode, "-", "_");
	!if (is_substr (mode, mstr)) mode = strcat (mode, "_mode" );   
	if (is_defined(mode))
	  {
	     eval (mode);
	     1;			       %  mode was defined
	     return;
	  }
     }
   0;
}

variable Mode_List_Exts = "c,h,cc,cpp,hpp,sl,txt,doc,tex,f,for,pro,idl,1,pl,v,verilog,vhd,vhdl,vt,sp,cir,spice,ps";
variable Mode_List_Modes = "c,c,c,c,c,slang,text,text,tex,fortran,fortran,idl,idl,nroff,perl,verilog,verilog,vhdl,vhdl,vhdl,spice,spice,spice,ps";

#ifdef MSDOS OS2
Mode_List_Exts = strcat (Mode_List_Exts, ",rc,bat,htm");     %  resource file
Mode_List_Modes = strcat (Mode_List_Modes, ",c,no,html");
#endif

#ifdef VMS UNIX
Mode_List_Exts = strcat (Mode_List_Exts, ",com,html");     %  resource file
Mode_List_Modes = strcat (Mode_List_Modes, ",dcl,html");
#endif

#ifdef UNIX
Mode_List_Exts = strcat (Mode_List_Exts, ",cshrc,tcshrc,login,profile,conf");
Mode_List_Modes = strcat (Mode_List_Modes, ",sh,sh,sh,sh,sh");
#endif

#ifndef MSDOS
Mode_List_Modes = strcat (Mode_List_Modes, ",java");
Mode_List_Exts = strcat (Mode_List_Exts, ",java");
#endif

%!% Prototype: Void add_mode_for_extension (String mode, String ext);
%!% This function modifies Mode_List in such a way that when a file with 
%!% filename extension `ext' is read in, function strcat (mode, "_mode")
%!% will be called to set the mode.   That is, the first parameter 'mode' 
%!% is the name of a mode without the '_mode' added to the end of it.
define add_mode_for_extension (mode, ext)
{
   variable comma = ",";
   
   Mode_List_Modes = strncat (mode, comma, Mode_List_Modes, 3);
   Mode_List_Exts = strncat (ext, comma, Mode_List_Exts, 3);
}

%!% This is a hook called by find_file routines to set the mode
%!% for the buffer. This function takes one parameter, the filename extension
%!% and returns nothing.
define mode_hook (ext)
{
   variable n, mode;
#ifdef VMS
   ext = extract_element(ext, 0, ';');
#endif
   
#ifdef MSDOS OS2 VMS
   ext = strlow (ext);
#endif
   
#ifdef UNIX
   % Strip off final ~
   n = strlen (ext); n--;
   if (andelse 
	{n > 0}
	{ext[n] == '~'})
     ext = substr (ext, 1, n);
#endif
   
   if (mode_hook_pointer(ext)) return;

   if (modeline_hook ()) return;

   n = is_list_element (Mode_List_Exts, ext, ',');
   if (n)
     {
	n--;
	mode = strcat (extract_element (Mode_List_Modes, n, ','), "_mode");
	if (is_defined(mode)) 
	  {
	     eval (mode);
	     return;
	  }
     }
   Default_Mode ();
}

%}}}
%{{{ Buffer flags and related functions
%!% sets buf modified flag. If argument is 1, mark
%!% buffer as modified.  If argument is 0, mark buffer as unchanged.
define set_buffer_modified_flag(modif)
{
   getbuf_info();
   if (modif) () | 1; else () & ~(1);
   setbuf_info(());
}

%!%  returns non-zero if the buffer modified flag is set.  It returns zero
%!%  if the buffer modified flag is not been set.  This works on the 
%!%  current buffer.  See also 'set_buffer_modified_flag'.
define buffer_modified ()
{
   variable flags;
   (, , , flags) = getbuf_info ();
   flags & 1;
}

%!% set undo mode for buffer.  If argument is 1, undo is on.  0 turns it off
define set_buffer_undo(modif)
{
   getbuf_info();
   if (modif) () | 32; else () & ~(32);
   setbuf_info(());
}


%!% Takes 1 parameter: 0 turn off readonly
%!%                    1 turn on readonly
define set_readonly(n)
{
   getbuf_info();
   if (n) () | 8; else () & ~(8);
   setbuf_info(());
}

%!% Takes 1 parameter: 0 turn off overwrite
%!%                    1 turn on overwrite
define set_overwrite(n)
{
   getbuf_info();
   if (n) () | 16; else () & ~(16);
   setbuf_info(());
}
   

define toggle_crmode ()
{
   setbuf_info(getbuf_info() xor 0x400);
   set_buffer_modified_flag (1);
}

define toggle_readonly()
{
   setbuf_info(getbuf_info() xor 8);
}

define toggle_overwrite()
{
   setbuf_info(getbuf_info() xor 16);
}

define toggle_undo()
{
   setbuf_info(getbuf_info() xor 32);
}


%!% Prototype: Void set_buffer_no_backup ();
define set_buffer_no_backup ()
{
   setbuf_info (getbuf_info() | 0x100);
}

%!% Prototype: Void set_buffer_no_autosave ();
define set_buffer_no_autosave ()
{
   setbuf_info (getbuf_info() & ~(0x2));
}

%}}}

%{{{ Help stuff

%!% string to display at bottom of screen upon JED startup and when
%!% user executes the help function.
variable help_for_help_string;

help_for_help_string =
#ifdef VMS
 "-> Help:H  Menu:?  Info:I  Apropos:A  Key:K  Where:W  Fnct:F  VMSHELP:M  Var:V";
#endif
#ifdef MSDOS
 "-> Help:H  Menu:?  Info:I  Apropos:A  Key:K  Where:W  Fnct:F  Var:V  Mem:M";
#endif
#ifdef UNIX OS2
 "-> Help:H  Menu:?  Info:I  Apropos:A  Key:K  Where:W  Fnct:F  Var:V  Man:M";
#endif

%%
%% help function
%%

%!% name of the file to load when the help function is called.
variable Help_File = "jed.hlp";   %% other modes will override this.

%{{{ help()

%!% Prototype: Void help ();
%!% Pop up a window containing a help file.  The help file that is read
%!% in is given by the variable Help_File.
define help()
{
   variable hlp = "*help*", buf, rows;
    
   !if (buffer_visible (hlp))
     {
	!if (strlen(Help_File)) Help_File = "generic.hlp";
	Help_File = expand_jedlib_file(Help_File);
	buf = whatbuf();
	onewindow();
	rows = window_info('r');
	setbuf(hlp);
	set_readonly(0);
	erase_buffer();

	() = insert_file(Help_File);
	pop2buf(hlp);
	eob(); bskip_chars("\n");
	rows = rows / 2 - (what_line() + 1);
	bob();
	set_buffer_modified_flag(0);
	set_readonly(1);
	pop2buf(buf);
	loop (rows) enlargewin();
     }
   
   update (1);
   message(help_for_help_string);
}

%}}}

variable Global_Top_Status_Line = " *** To activate menus, press `Ctrl-H ?'.  For help, press `Ctrl-H' twice. ***";
() = set_top_status_line (Global_Top_Status_Line);

%{{{ help_prefix()
define help_prefix()
{
   variable c;
   
   !if (input_pending(7)) flush (help_for_help_string);
   c = toupper (getkey());
   switch (c)
     { case  8 or case ('H', c) : help }
     { case  'A' : apropos }
     { case  'I' : info_mode}
     { case  '?' : menu_main_cmds}
     { case  'F' : describe_function}
     { case  'V' : describe_variable}
     { case  'W' : where_is}
     { case  'C' or case ('K', c) : showkey}
     { case  'M' :
#ifdef UNIX OS2
	unix_man();
#else 
#ifdef VMS
	vms_help ();
#endif
#endif
#ifdef MSDOS
	call("coreleft");
#endif
     }
     { pop(); beep(); }
}

%}}}

%}}}
%{{{ Mini-Buffer related stuff

% Load minibuffer routines now before any files are loaded.
% This will reduce fragmentation on PC.
!if (BATCH)
{
#ifdef MSDOS UNIX OS2 VMS
   () = evalfile("mini"); 
#else
   autoload ("mini_exit_minibuffer", "mini");
   autoload ("prev_mini_command", "mini");
   autoload ("next_mini_command", "mini");
#endif
}

%{{{ Reading from Mini-Buffer functions
%for compatability with older versions
%!% Prototype: String read_file_from_mini (String p);
%!% This function prompts the user for a file name using @p@ as a prompt.
%!% It reads a filename with completion from the mini-buffer and returns
%!% it.
%!% Related Functions: @read_with_completion@, @read_mini@
define read_file_from_mini ()
{
   read_with_completion( () , Null_String, Null_String, 'f');
}

%!% Prototype: String read_string_with_completion (prompt, dflt, list)
%!% This function takes 3 String parameters and returns a String.  The
%!% first parameter is used as the prompt, the second parameter is the 
%!% default value to be returned and the third parameter is a list to be used
%!% for completions.  This list is simply a comma separated list of strings.
define read_string_with_completion (prompt, dflt, list)
{
   read_with_completion (list, prompt, dflt, Null_String, 's');
}

%}}}

%}}}
%{{{ Startup hook

%!% If non-zero, startup by asking user for a filename if one was
%!% not specified on the command line.
variable Startup_With_File = 0;

%% startup hook
%!% Function that gets executed right before JED enters its main editing
%!% loop.  This is for last minute modifications of data structures that
%!% did not exist when startup files were loaded (e.g., minibuffer keymap)
define jed_startup_hook()
{
   variable n, hlp, ok = 0, mini = "Mini_Map", file, do_message = 1;

#ifdef MSDOS OS2
   definekey ("next_mini_command", "\eOr", mini);
   definekey ("next_mini_command", "\xE0P", mini);
   definekey ("prev_mini_command", "\xE0H", mini);
   definekey ("prev_mini_command", "\eOx", mini);
#else
   definekey ("next_mini_command", "^[[B", mini);
   definekey ("prev_mini_command", "^[[A", mini);
   definekey ("next_mini_command", "^[OB", mini);
   definekey ("prev_mini_command", "^[OA", mini);
#endif

   definekey ("mini_exit_minibuffer", "^M", mini);
   definekey ("exit_mini", "^[^M", mini);
   
   % turn on Abort character processing
   IGNORE_USER_ABORT = 0;

   runhooks ("startup_hook");
   
   !if (strcmp(whatbuf(), "*scratch*") or buffer_modified())
     {
	ERROR_BLOCK 
	  {
	     erase_buffer ();
	     set_buffer_modified_flag (0);
	  }
	
	() = insert_file (expand_jedlib_file("cpright.hlp")); 
	set_buffer_modified_flag (0);
	bob();
	file = Null_String;
	if (Startup_With_File > 0)
	  {
	     forever 
	       {
		  file = read_file_from_mini ("Enter Filename:");
		  if (strlen(extract_filename(file))) break;
	       }
	  }
	else !if (Startup_With_File)
	  {
	     message(help_for_help_string); do_message = 0;
	     update(1);
#ifntrue
	     $1 = Null_String;
	     while (input_pending (0))
	       {
		  variable silly = getkey ();
		  if (silly == 0) silly = "^@"; else silly = char (silly);
		  $1 = strncat ($1, "|", silly, 3);
	       }
#endif	     
	     () = input_pending(600);
	  }
	EXECUTE_ERROR_BLOCK;
	if (strlen (file)) () = find_file(file);
     }
   
   if (do_message) message(help_for_help_string);
}



%}}}

#ifdef VMS
%{{{ resume_hook()
%% This resume hook is need for VMS when returning from spawn.
%% In fact, it is NEEDED for certain JED functions on VMS so declare it.
define resume_hook()
{
   variable file = getenv("JED_FILE_NAME");
   !if (strlen(file)) return;
   
   !if (find_file(file)) error("File not found!");
}
%}}}
#endif VMS

%{{{ find_file_hook(file)

% called AFTER a file is read in to a buffer.  FILENAME is on the stack.
define find_file_hook(file)
{
   variable dir, a, f, m;
   (dir, f) = parse_filename(file); 

#ifndef VMS
   if (file_status(dir) != 2)
     {
	verror ("Directory %s is invalid!", dir, 1);
     }
#endif
   
   if (No_Backups) set_buffer_no_backup ();
   a = make_autosave_filename(dir, f);
   if (file_time_compare(a, file) > 0) 
     {
	m = "Autosave file is newer. Use ESC-X recover_file.";
	flush(m);
        () = input_pending(10); 
	message(m);
     }
   runhooks ("user_find_file_hook");
}

%}}}

%{{{ Completions

%
% completions  -- everything here must be predefined
% I just push the strings onto the stack and loop 'add_completion' over them
%
$0 = _stkdepth();
_add_completion ("toggle_undo", "calendar", "trim_buffer", "abbrev_mode",
		 "define_abbreviation", "save_abbrevs",
		 "occur", "append_region", "write_region", 
		 "replace_across_buffer_files",
		 "recover_file", "compile", "sort", "untab", "fortran_mode", 
		 "save_buffers",
		 "isearch_forward", "isearch_backward", "shell",
		 "edit_tab_stops", "c_mode", "toggle_crmode",
		 "text_mode", "no_mode", "goto_line_cmd", "goto_column_cmd", 
		 "describe_mode",
		 "evalbuffer", "open_rect", "kill_rect", "insert_rect", 
		 "copy_rect", "blank_rect",
		 "dired", "re_search_forward", "re_search_backward", 
		 "query_replace_match",
		 "describe_bindings", "search_backward", "search_forward", 
		 "replace_cmd", "find_binary_file", "latex_mode", "sh_mode",
#ifndef MSDOS OS2
		 "mail",
#endif
#ifdef UNIX OS2
		 "ispell",
#endif
		 _stkdepth - $0);      %  matches _add_completion


%}}}

%{{{ buffer_filename ()

%!% Prototype: String buffer_filename ();
%!% Returns the name of the file associated with the current buffer.  If 
%!% there is none associated with it, the empty string is returned.
define buffer_filename ()
{
   variable file, dir;
   (file, dir, , ) = getbuf_info();
   !if (strlen (file)) dir = Null_String;
   strcat (dir, file);
}

%}}}
%{{{ save_buffer()
%!% Prototype: Void save_buffer ();
%!% Save current buffer.
define save_buffer()
{
   variable flags, dir, file;
   (file, , , flags) = getbuf_info();

   !if (flags & 1) return (message("Buffer not modified."));
   if (strlen(file))
     {
	() = write_buffer(buffer_filename ()); 
     }
   else call ("save_buffers");
} add_completion("save_buffer");

%}}}
%{{{ insert_buffer()
define insert_buffer()
{
   read_with_completion("Insert Buffer:", Null_String, Null_String, 'b');
   push_spot();
   ERROR_BLOCK {pop_spot()}
   insbuf(());
   EXECUTE_ERROR_BLOCK;
}  add_completion("insert_buffer");

%}}}

%{{{ get_y_or_n (str)
%!% Prototype: Integer get_y_or_n (prompt_str);
%!% This function may be used to get a `y' or `n' response from the user
%!% using @prompt_str@ to construct the prompt.  It returns @1@ if the user
%!% responds with @y@, or @0@ if the response is @n@.
%!% See also: @get_yes_no@, @read_mini@, @message@, @flush@, @getkey@
define get_y_or_n (str)
{
   variable ch;
   EXIT_BLOCK
     {
	clear_message ();
     }
   flush (strcat (str, "? ([y]/n)"));
   ch = tolower (getkey ());
   if ((ch == 'y') or (ch == '\r')) return 1;
   if (ch == 'n') return 0;
   beep ();
   return get_yes_no (str);
}
	
%}}}

%{{{ Word movement and processing functions

%%
%%  word movement definitions.  Since these vary according to editors,
%%  they are S-Lang routines.
%%

define skip_word ()
{
  while (skip_non_word_chars(), eolp()) 
    {
      if (1 != right(1)) break;
    }
   skip_word_chars();
}


   

define bskip_word()
{
   while (bskip_non_word_chars(), bolp())
     {
	!if (left(1)) break;
     }
   bskip_word_chars();
}

define delete_word()
{
   push_mark(); skip_word(); del_region();
}

define bdelete_word ()
{  
   push_mark(); bskip_word(); del_region();
}

define xform_word ()		       %  parameter on stack
{
  while (skip_non_word_chars(), eolp()) 
    {
      if (1 != right(1)) break;
    }
   push_mark(); skip_word(); 
   xform_region(());
}

define capitalize_word()
{
   xform_word('c');
}

define upcase_word()
{
   xform_word('u');
}

define downcase_word()
{
   xform_word('d');
}

%}}}

%{{{ smart_set_mark_cmd ()

%!% Prototype: Void push_visible_mark ();
%!% This function is performs the same task as @push_mark@ except that the
%!% region between this mark and the cursor position will be highlighted.
%!% Such a mark is said to be a visible mark.
%!% Related Functions: @push_mark@, @pop_mark@, @set_mark_cmd@
define push_visible_mark ()
{
   push_mark ();
   call ("set_mark_cmd");
}

%!% Prototype: Void set_mark_cmd ();
%!% If a mark is already set, and that mark is a visible mark, then this
%!% function will remove that mark.  It will then push a visible mark onto
%!% the mark stack.
%!% Related Functions: @push_visible_mark@, @pop_mark@, @smart_set_mark_cmd@
define set_mark_cmd ()
{
   if (is_visible_mark ())
     pop_mark_0 ();
   
   push_visible_mark ();
}

%!% Prototype: Void smart_set_mark_cmd ();
%!% If the top mark is a visible mark, this function will remove that mark;
%!% otherwise it will push a visible mark onto the mark stack.  Use of
%!% this function has the effect of toggling a highlighted region.
%!% Related Functions: @set_mark_cmd@, @push_mark@, @push_visible_mark@
define smart_set_mark_cmd ()
{
   if (is_visible_mark ())
     {
	pop_mark_0 ();
	return;
     }
   set_mark_cmd ();
}

%}}}

%{{{ buffer_format_in_columns()
%!% Prototype Void buffer_format_in_columns();
%!% takes a buffer consisting of a sigle column of items and converts the
%!% buffer to a multi-column format.
define buffer_format_in_columns()
{
   push_spot_bob ();
   forever 
     {
	_for (0,4,1) 
	  {
	     goto_column(() * 14 + 1);
	     if (eolp())
	       {
		  if (eobp()) 
		    {
		       pop_spot();
		       return;
		    }
		  insert_single_space;
		  del();
	       }
	  } 
	!if (down_1 ()) break;
	% bol (); % this is a side effect of going down
     }
   pop_spot();
}

%}}}

%{{{ delete_line()
define delete_line()
{
   bol(); push_mark_eol (); go_right_1 (); del_region();
}

%}}}

%{{{ set_fill_column ()
define set_fill_column ()
{
   push_spot(); 
   eol();
   WRAP = what_column ();
   pop_spot();
   vmessage ("WRAP column at %d.", WRAP, 1);
}

%}}}

%{{{ rename_buffer(name)
define rename_buffer(name)
{
   variable flags = getbuf_info(); pop(); setbuf_info(name, flags);
}

%}}}

%{{{ info ()
define info ()
{
   info_mode ();
   definekey("exit_jed",		"Q",  "Infomap");
   definekey("exit_jed",		"q",  "Infomap");
   runhooks ("info_mode_hook");
}

%}}}
 
%{{{ deln (n)
%!% Prototype: Void deln (Integer n);
%!% delete the next 'n' characters.
define deln (n)
{
   push_mark (); go_right(n); del_region ();
}

%}}}

%{{{ insert_spaces (n)
define insert_spaces (n)
{
   loop (n) insert_single_space ();
}

%}}}

%{{{ blooking_at (str)
define blooking_at (str)
{
   variable n = strlen (str);
   
   EXIT_BLOCK 
     {
	pop_spot ();
     }
   
   push_spot ();

   if (n != left(n)) return 0;
   return looking_at (str);
}

%}}}

%{{{ exchange_point_and_mark ()
define exchange_point_and_mark ()
{
   call ("exchange");
}

%}}}

%{{{ str_split (str, n)
% This ought to be a slang intrinsic!!!
define str_split (str, n)
{
   substr (str, 1, n - 1);
   substr (str, n, strlen (str));
}

%}}}

#ifndef VMS
%{{{ expand_file_hook (file)
define expand_file_hook (file)
{
   variable changed = 0;
   variable envvar;
   variable pos, len, name, dir;
   variable file0, file1, file2;
   
   file2 = file;
   file = Null_String;
   % Check for environment variable of form $(variable)
   while (andelse 
	   {strlen (file2)}
	   {string_match (file2, "\\$[^/$]+", 1)})
     {
	changed++;
	(pos, len) = string_match_nth (0);
	pos++;
	(file0, file1) = str_split (file2, pos);
	(file1, file2) = str_split (file1, len + 1);
	
	envvar = getenv (substr (file1, 2, len - 1));
	file = strncat (file, file0, envvar, 3);
     }

   file = strcat (file, file2);

#ifdef UNIX
   % Now look for things like: /~name/...
   if (orelse
	{pos = string_match (file, "^~", 1); pos;}
	{pos = -string_match (file, "/~", 1); pos;})
     {
	if (pos < 0) 
	  {
	     pos = -pos;
	     pos++;
	  }
	pos++;
	file = substr (file, pos, strlen (file));
	pos = is_substr (file, "/");
	if (pos)
	  {
	     (name, file) = str_split (file, pos);
	  }
	else
	  {
	     name = file;
	     file = Null_String;
	  }
	if (file[0] == '/') (, file) = str_split (file, 2);
	(dir,,,,) = get_passwd_info (name);
	file = dircat (dir, file);
	changed++;
     }
#endif
   
   if (changed)
     {
	file;
     }
   changed;
}

set_expansion_hook ("expand_file_hook");

%}}}
#endif VMS

   
% This fixes some bug in OS2 dealing with 'dir' issued non-interactively.
#ifdef OS2
   if (strlen(getenv("DIRCMD"))) putenv("DIRCMD=/ogn");
#endif

() = evalfile ("os.sl"); 

%---------------------------------------------------------------------------

%{{{ This is the command line hook function
% called from main in JED executable.
#iftrue
define command_line_hook () %{{{
{
   variable n, i, file, depth, next_file;
   
   n = MAIN_ARGC; --n; i = 1;
   if (BATCH) { --n; ++i; }	% batch - 1st arg is not used

   % if first argument is -n then do NOT load init file
   if (if (n) strcmp (command_line_arg(i), "-n"); else 1)
     {
#ifdef VMS
	if (strlen (getenv("JED_HOME"))) "JED_HOME:"; else "SYS$LOGIN:";
#else
        file = getenv ("JED_HOME");
	if (strlen (file)) file; else getenv("HOME");
#endif
#ifdef UNIX
	".jedrc";
#else
	"jed.rc";
#endif
	file = dircat((), ());

	if (file_status (file) != 1) file = "jed.rc";
	depth = _stkdepth ();
	() = evalfile (file);
	depth = _stkdepth () - depth;
	if (depth)
	  {
	     flush (strcat ("Excess junk left on stack by ", file));
	     usleep (1000);
	     pop_n (depth);
	  }
     }
   else
     {
	--n; ++i;
     }

   !if (n) return;

   %
   % Is JED to emulate most?
   %
   !if (strcmp(command_line_arg(i), "-most"))
     {
	run_most (i + 1);
	return;
     }

   while (n > 0)
     {
#ifdef MSDOS			% lowercase name
	file = strlow (command_line_arg(i));
#else
	file = command_line_arg(i);
#endif
	--n; ++i;
	if (n)
	  {
#ifdef MSDOS			% use lowercase name
	     next_file = strlow (command_line_arg(i));
#else
	     next_file = command_line_arg(i);
#endif
	  }
	switch (file)
	  {case "-f" and n : eval(next_file)}
	  {case "-g" and n : goto_line(integer(next_file))}
	  {case "-s" and n :
	     () = fsearch(next_file);
	     save_search_string(next_file);
	  }
	  {case "-l" and n : () = evalfile(next_file); }
	  {case "-i" and n : () = insert_file(next_file);}
	  {case "-2" : splitwindow(); ++n; --i;}
	  {case "-tmp":
	     set_buffer_no_backup ();
	     set_buffer_no_autosave ();
	     ++n; --i;
	  }
#iftrue
   	  {case "-hook" and n:		% run user hook
	     () = evalfile (next_file);
	     if (is_defined (next_file))
	       {
		  i++;		% skip next_file
		  i;		% leave argc on the stack
		  eval (next_file);	% hook name == file name (but no .sl)
		  return;
	       }
	  }
#endif
	  {
	     pop ();
	     depth = _stkdepth;
	     % The integer call below could fail...
	     ERROR_BLOCK
	       {
		  pop_n (_stkdepth - depth);
		  _clear_error ();
		  0;
	       }

	     (andelse
	       { n and (file[0] == '+');}
	       {integer (file) >= 0})
	     :

	     () = find_file (next_file);
	     goto_line (integer (file));
	  }
	  {
	     flush (strcat ("Reading ", file));
	     () = find_file(());  ++n; --i;
	  }

	--n; ++i;
     }
}

%}}}
#else
% old version
define command_line_hook () %{{{
{
   variable n, i, file, depth, jedrc, home, next_file;
   variable script_file = Null_String;
   jedrc = "jed.rc";
  
#ifdef UNIX
   jedrc = ".jedrc";
#endif
   
   home = getenv("JED_HOME");
#ifdef VMS
%% allows JED_HOME: to be search list---
%% thanks to SYSTEM@VACMZB.chemie.Uni-Mainz.DE for suggestion
   if (strlen(home)) home = "JED_HOME:";
#endif
   !if (strlen(home))
     {
	home = getenv("HOME");
#ifdef VMS
	home = "SYS$LOGIN:";
#endif
     } 
   jedrc = dircat(home, jedrc);
   
   n = MAIN_ARGC;
   
   --n;  %% argv[0], here it is not used.
%
%  If batch then first argument is not used so start at second
%  Also, n is the number of effective command line parameters so reduce it.
%
   
   i = 1;
   if (BATCH) 
     {	
	n--;
	i++;
     }
%
% if first argument is -n then do NOT load init file
%
   % stuff left on stack for if
   if (n) strcmp (command_line_arg(i), "-n"); else 1;
   if (())
     {
	depth = _stkdepth;
	if (file_status(jedrc) == 1) jedrc; else "jed.rc";
	() = evalfile(()); 
	if ( _stkdepth != depth)
	  {
	     flush(strcat ("Excess junk left on stack by ", jedrc));
	     usleep (1000);
	  }

	pop_n (_stkdepth - depth);
     }
   else if (n) ++i;
   
   n = MAIN_ARGC - i;   
   
   !if (n) return;
   
%
% Is JED to emulate most?
%
   !if (strcmp(command_line_arg(i), "-most"))
     {
	run_most (i + 1);
	return;
     }
      
   while (n > 0)
     {
#ifdef MSDOS			% lowercase name
	file = strlow (command_line_arg(i));
#else
	file = command_line_arg(i);
#endif
	--n; ++i;
	if (n)
	  {
#ifdef MSDOS			% use lowercase name  
	     next_file = strlow (command_line_arg(i));
#else
	     next_file = command_line_arg(i);
#endif
	  }
	switch(file)
	  {case "-f" and n : eval(next_file)}
	  {case "-g" and n : goto_line(integer(next_file))}
	  {case "-s" and n : 
	     () = fsearch(next_file); 
	     save_search_string(next_file);
	  }
	  {case "-l" and n : () = evalfile(next_file); }
	  {case "-i" and n : () = insert_file(next_file);}
	  {case "-2" : splitwindow(); ++n; --i;}
	  {case "-tmp":
	     set_buffer_no_backup ();
	     set_buffer_no_autosave ();
	     ++n; --i;
	  }
	  {
	     pop ();
	     depth = _stkdepth;
	     % The integer call below could fail...
	     ERROR_BLOCK
	       {
		  pop_n (_stkdepth - depth);
		  _clear_error ();
		  0;
	       }
	     
	     (andelse
	       { n and (file[0] == '+');}
	       {integer (file) >= 0})
	     :

	     () = find_file (next_file);
	     goto_line (integer (file));
	  }
	  {
	     flush (strcat ("Reading ", file));
	     () = find_file(());  ++n; --i;
	  }
	
	--n; ++i;
     }
   
   if (strlen(script_file)) () = evalfile(script_file);
}

%}}}
#endif
%}}}

%---------------------------------------------------------------------------


%
%  This code fragment looks for the existence of "defaults.sl" and loads
%  it.  This file IS NOT distributed with JED.
%

if (strlen(expand_jedlib_file("defaults.sl"))) () = evalfile("defaults");

