% New C-mode indentation routines

autoload ("c_make_comment", "cmisc");
autoload ("c_format_paragraph", "cmisc");
autoload ("c_paragraph_sep", "cmisc");
autoload ("c_comment_region", "cmisc");
autoload ("c_top_of_function", "cmisc");
autoload ("c_end_of_function", "cmisc");
autoload ("c_mark_function", "cmisc");

define is_slang_mode ()
{
   variable is_slang;
   (, is_slang) = what_mode ();
   is_slang & 8;
}

define c_find_effective_eol ()
{
   bol ();
   while (ffind_char ('%'))
     {
	go_right_1 ();
	if (parse_to_point () == -2)
	  {
	     return;
	  }
     }
   eol ();
}

define slmode_bskip_comment ()
{
   forever 
     {
	bskip_chars (" \t%");
	
	push_mark ();
	bol ();
	pop_mark (not(looking_at_char ('#')));

	!if (bolp ()) return;
	!if (left (1)) return;
	c_find_effective_eol ();
     }
}
% This function also skips preprocessor lines
define c_bskip_over_comment ()
{
   variable num_up;
   if (is_slang_mode ()) return slmode_bskip_comment ();
   
   forever 
     {
	bskip_chars (" \t\n");
	
	push_mark ();
	num_up = 0;
	while (up(1))
	  {
	     eol ();
	     !if (blooking_at ("\\")) 
	       {
		  go_down (1);
		  break;
	       }
	     num_up++;
	  }
	if (num_up)
	  {
	     bol_skip_white ();
	     if (looking_at_char ('#'))
	       {
		  pop_mark_0 ();
		  continue;
	       }
	  }
	pop_mark_1 ();

	!if (blooking_at ("*/"))
	  {
	     push_mark ();
#iftrue
	     if (bfind ("//")) 
	       {
		  if (0 == parse_to_point ())
		    {
		       % Not in a comment or string
		       pop_mark_0 ();
		       continue;
		    }
	       }
#endif
	     bol ();
	     !if (bobp ())
#iftrue
	       if (looking_at_char ('#'))
#else
		 if (orelse 
		      {looking_at_char ('#')}
		     % This is a hack for C++ code.  I am not sure how good it
		     % is...
		      {skip_white (); looking_at ("//")})
#endif
	       {
		  pop_mark_0 ();
		  continue;
	       }
	     pop_mark_1 ();
	     break;
	  }
	!if (bsearch ("/*")) break;
     }
}

define c_looking_at (token)
{
   variable cse = CASE_SEARCH, ret = 0;
   CASE_SEARCH = 0;
   
   if (looking_at(token))
     {
	push_spot ();
	go_right(strlen(token));
	POINT;
	skip_chars ("\t :({");
	ret = (POINT - ()) or eolp();
	pop_spot ();
     }
   CASE_SEARCH = cse;
   ret;
}

define c_indent_to (n)
{
   bol_skip_white ();
   if (what_column != n)
     {
	bol_trim ();
	n--;
	whitespace (n);
     }
}

define c_indent_preprocess_line ()
{
   variable col;

   push_spot_bol ();
   
   trim ();
   !if (up_1 ())
     {
	pop_spot ();
	return;
     }
   
   !if (bol_bsearch_char ('#'))
     {
	pop_spot ();
	return;
     }
   go_right_1 ();
   skip_white ();
   col = what_column ();
   if (looking_at ("if"))
     col += C_Preprocess_Indent;
   else if (looking_at ("el"))
     col += C_Preprocess_Indent;
   
   pop_spot ();
   go_right_1 ();
   skip_white ();
   
   if (looking_at_char ('e'))
     col -= C_Preprocess_Indent;

   if (what_column () == col)
     return;
   bskip_white ();
   trim ();
   whitespace (col - 2);
}


define c_indent_line ()
{
   variable val, col, extra_indent = 0;
   variable match_char, match_indent, this_char;
   variable match_mark;

   push_spot ();
   bol_skip_white ();
   this_char = what_char ();
   
   EXIT_BLOCK
     {
	bol_trim ();
	pop_spot ();
     }
   
   if (looking_at_char ('#')) 
     {
	c_indent_preprocess_line ();
	return;
     }
   
   push_spot ();
   if (up_1 ())
     {
	eol ();
	if (blooking_at ("\\") and not (blooking_at ("\\\\")))
	  {
	     pop_spot ();
	     return;
	  }
     }
   pop_spot ();
   
   EXIT_BLOCK
     {
	push_mark ();
	bskip_white ();
	1;
	if (bolp ()) 
	  {
	     pop ();
	     skip_white ();
	     0;
	  }
	pop_mark (());		       %  take argument from stack
     }
   
   
   if (orelse
	{ c_looking_at("case"); }
	{ c_looking_at("default"); }
	{ c_looking_at("protected");}
	{ c_looking_at("private");}
	{ c_looking_at("public");}
       )
     {
	if (ffind_char (':'))
	  {
	     extra_indent -= C_INDENT;
	     extra_indent += C_Colon_Offset;
	  }
	bol ();
     }
   else 
     {
	forever
	  {
	     c_bskip_over_comment ();
	
	     !if (orelse
		   { blooking_at (";") }
		   { blooking_at (",") }
		   { blooking_at ("{") }
		   { blooking_at ("}") }
		   { blooking_at (":") })
	       {
		  extra_indent += C_CONTINUED_OFFSET;
	       }
	     !if (blooking_at (")")) break;
	     push_mark ();
	     go_left_1 ();
	     if (1 != find_matching_delimiter (')'))
	       {
		  pop_mark_1 ();
		  break;
	       }
	     c_bskip_over_comment ();
	     push_spot ();
	     if ((1 == find_matching_delimiter (')')), pop_spot ())
	       {
		  pop_mark_1 ();
		  break;
	       }
	     
	     pop_mark_0 ();
	     bol ();
	  }
     }

   val = find_matching_delimiter (')');
   col = what_column ();
   if ((val < 0) and looking_at ("/*")) val = -2;
   
   match_mark = create_user_mark ();
     
   match_char = what_char;
   bol_skip_white ();
   match_indent = what_column ();
   pop_spot ();
   
   switch (val)
     {
      case 0:			       %  mismatch

	if (match_char == '{')
	  {
	     push_spot ();
	     goto_user_mark (match_mark);
	     bskip_white ();

	     if (blooking_at (")"))
	       {
		  go_left_1 ();
		  if (1 == find_matching_delimiter (')'))
		    {
		       bol_skip_white ();
		       match_indent = what_column ();
		    }
	       }
	     pop_spot ();
	     col = match_indent;
	     col += C_INDENT;
	  }
	else 
	  {
	     push_spot_bol ();
	     trim ();
	     if (looking_at_char ('*')) insert_single_space ();
	     pop_spot ();
	     return;
	  }
     }
     {
      case 1:
	extra_indent = 1;	       %  match found
     }
     {
      case -2:			       %  inside comment
	if (is_slang_mode ()) return;
	push_spot ();
	if (this_char != '\\') col++;
	c_indent_to (col);	       %  col++ because we are looking at /*  
	if ((looking_at_char ('*')) 
	    or not (eolp ())) pop_spot ();
	else
	  {
	     insert ("* ");
	     pop_spot ();
	     if (what_column () <= col)
	       {
		  goto_column (col + 2);
	       }
	  }
	return;
     }
     {
      case 2:
	push_spot_bol ();
	trim ();
	pop_spot ();
	return;
     }
   
	
   switch (this_char)
     {
      case '}':
	col -= C_INDENT;
     }
     {
      case '{':
	col += C_BRACE;
     }
     {
	pop (); col += extra_indent;
     }
   
   push_spot ();
   c_indent_to (col);
   pop_spot ();
}

% This function returns zero if the line does not begin with "* @ "
% or returns the indent column if it does.  This combination of characters
% will most certainly appear in a comment.  See the c file jed/src/intrin.c
% for examples of how it is exploited.
define c_is_comment_example ()
{
   push_spot ();
   bol_skip_white ();
   0;
   if (looking_at ("* @ "))
     {
	pop ();
	what_column ();
     }
   pop_spot ();
}


define c_newline_and_indent ()
{
   variable slcom = "%!% ";
   if (is_slang_mode ())
     {
	push_spot_bol ();
	if (looking_at (slcom), pop_spot ())
	  {
	     newline ();
	     insert (slcom);
	     return;
	  }
     }
   
   variable col = c_is_comment_example ();
   newline ();
   if (col)
     {
	c_indent_to (col);
	insert ("* @ ");
     }
   else c_indent_line ();
}

define c_parse_to_point ()
{
   parse_to_point () or c_is_comment_example ();
}

define c_insert_bra ()
{
   if (c_parse_to_point ())
     insert_char ('{');
   else
     {
	push_spot ();
	c_bskip_over_comment ();
	if (blooking_at (","), pop_spot ())
	  {
	     insert_char ('{');
	  }
	else
	  { 
	     push_spot ();
	     skip_white ();
	     if (eolp ())
	       {
		  bskip_white ();
		  if (not (bolp ()) and C_BRA_NEWLINE, pop_spot ()) newline ();
		  push_spot ();
		  bskip_white ();
		  bolp ();	       %  on stack
		  pop_spot ();
		  insert_char ('{');
		  if ( () ) c_indent_line ();   %  off stack
		  eol ();
		  if (C_BRA_NEWLINE) c_newline_and_indent ();
	       }
	     else 
	       {
		  pop_spot ();
		  insert_char ('{');
	       }
	  }
     }
}



define c_insert_ket ()
{
   variable status = c_parse_to_point ();

   push_spot ();
   skip_white ();
   push_spot ();
   if (status 
       or not (eolp ())
       or (bol_skip_white (), looking_at_char ('{')), pop_spot ())
     {
	pop_spot ();
	insert_char ('}');
	blink_match ();
	return;
     }

   bskip_white ();
   if (bolp (), pop_spot ())
     {
	insert_char ('}');
	trim ();
     }
   else 
     {	
	eol ();
	insert ("\n}");
     }
   c_indent_line ();
   eol (); 
   blink_match ();
   if (C_BRA_NEWLINE) c_newline_and_indent ();
}

define c_insert_colon ()
{
   insert_char (':');
   !if (c_parse_to_point ())
     c_indent_line ();
}

$1 = "C";
!if (keymap_p ($1)) make_keymap ($1);
%definekey ("indent_line", "\t", $1);
%definekey ("c_newline_and_indent", "\r", $1);
definekey ("c_insert_bra", "{", $1);
definekey ("c_insert_ket", "}", $1);
definekey ("c_insert_colon", ":", $1);
definekey ("c_make_comment", "\e;", $1);
definekey ("c_comment_region", "^X;", $1);
definekey ("c_format_paragraph", "\eq", $1);
definekey ("c_top_of_function", "\e^A", $1);
definekey ("c_end_of_function", "\e^E", $1);
definekey ("c_mark_function", "\e^H", $1);


% Now create and initialize the syntax tables.
create_syntax_table ("C");
define_syntax ("/*", "*/", '%', "C");
define_syntax ("([{", ")]}", '(', "C");
define_syntax ('"', '"', "C");
define_syntax ('\'', '\'', "C");
define_syntax ('\\', '\\', "C");
define_syntax ("0-9a-zA-Z_", 'w', "C");        % words
define_syntax ("-+0-9a-fA-F.xXL", '0', "C");   % Numbers
define_syntax (",;.?:", ',', "C");
define_syntax ('#', '#', "C");
define_syntax ("%-+/&*=<>|!~^", '+', "C");
set_syntax_flags ("C", 4);

#ifdef HAS_DFA_SYNTAX
enable_highlight_cache("cmode.dfa", "C");
define_highlight_rule("^[ \t]*#", "PQpreprocess", "C");
define_highlight_rule("//.*", "comment", "C");
define_highlight_rule("/\\*.*\\*/", "Qcomment", "C");
define_highlight_rule("^([^/]|/[^\\*])*\\*/", "Qcomment", "C");
define_highlight_rule("/\\*.*", "comment", "C");
define_highlight_rule("^[ \t]*\\*+([ \t].*)?$", "comment", "C");
define_highlight_rule("[A-Za-z_\\$][A-Za-z_0-9\\$]*", "Knormal", "C");
define_highlight_rule("[0-9]+(\\.[0-9]*)?([Ee][\\+\\-]?[0-9]*)?",
		      "number", "C");
define_highlight_rule("0[xX][0-9A-Fa-f]*[LlUu]*", "number", "C");
define_highlight_rule("[0-9]+[LlUu]*", "number", "C");
define_highlight_rule("\"([^\"\\\\]|\\\\.)*\"", "string", "C");
define_highlight_rule("\"([^\"\\\\]|\\\\.)*\\\\?$", "string", "C");
define_highlight_rule("'([^'\\\\]|\\\\.)*'", "string", "C");
define_highlight_rule("'([^'\\\\]|\\\\.)*\\\\?$", "string", "C");
define_highlight_rule("[ \t]+", "normal", "C");
define_highlight_rule("[\\(\\[{}\\]\\),;\\.\\?:]", "delimiter", "C");
define_highlight_rule("[%\\-\\+/&\\*=<>\\|!~\\^]", "operator", "C");
build_highlight_table("C");
#endif

% Type 0 keywords
#ifntrue
() = define_keywords_n ("C", "doif", 2, 0);
() = define_keywords_n ("C", "asmforintnewtry", 3, 0);
() = define_keywords_n ("C", "autocasecharelseenumgotolongthisvoid", 4, 0);
() = define_keywords_n ("C", "breakcatchclassconstfloatshortthrowunionwhile", 5, 0);
() = define_keywords_n ("C", "deletedoubleexternfriendinlinepublicreturnsignedsizeofstaticstructswitch", 6, 0);
() = define_keywords_n ("C", "defaultprivatetypedefvirtual", 7, 0);
() = define_keywords_n ("C", "continueoperatorregistertemplateunsignedvolatile", 8, 0);
() = define_keywords_n ("C", "protected", 9, 0);
#else
() = define_keywords_n ("C", "doif", 2, 0);
() = define_keywords_n ("C", "asmforintnewtry", 3, 0);
() = define_keywords_n ("C", "autoboolcasecharelseenumgotolongthistruevoid", 4, 0);
() = define_keywords_n ("C", "breakcatchclassconstfalsefloatshortthrowunionusingwhile", 5, 0);
() = define_keywords_n ("C", "deletedoubleexternfriendinlinepublicreturnsignedsizeofstaticstructswitchtypeid", 6, 0);
() = define_keywords_n ("C", "defaultmutableprivatetypedefvirtualwchar_t", 7, 0);
() = define_keywords_n ("C", "continueexplicitoperatorregistertemplatetypenameunsignedvolatile", 8, 0);
() = define_keywords_n ("C", "namespaceprotected", 9, 0);
() = define_keywords_n ("C", "const_cast", 10, 0);
() = define_keywords_n ("C", "static_cast", 11, 0);
() = define_keywords_n ("C", "dynamic_cast", 12, 0);
() = define_keywords_n ("C", "reinterpret_cast", 16, 0);
#endif
% Type 1 keywords (commonly used libc functions)
() = define_keywords_n("C", 
		       "EOFabscosdivexplogpowsintan",
		       3, 1);
() = define_keywords_n("C", 
		       strncat ("FILENULLacosasinatanatofatoiatolceilcosh",
				"exitfabsfeoffmodfreegetcgetslabsldivmodf",
				"putcputsrandsinhsqrttanhtime",
				3),
		       4, 1);
() = define_keywords_n("C",
		       strncat ("abortatan2clockctimediv_terrnofgetcfgets",
				"floorfopenfputcfputsfreadfrexpfseekftell",
				"ldexplog10qsortraisescanfsrandstdin",
				3),
		       5, 1);
() = define_keywords_n("C",
		       strncat ("assertatexitcallocfcloseferrorfflushfscanf",
				"fwritegetenvgmtimemallocmemchrmemcmpmemcpy",
				"memsetmktimeperrorprintfremoverenamerewind",
				"setbufsetjmpsignalsize_tsscanfstderrstdout",
				"strcatstrchrstrcmpstrcpystrlenstrspnstrstr",
				"strtodstrtokstrtolsystemtime_ttmpnamungetc",
				"va_argva_end",
				7),
		       6, 1);
() = define_keywords_n("C",
		       strncat ("asctimebsearchclock_tfgetposfprintffreopen",
				"fsetposgetcharisalnumisalphaiscntrlisdigit",
				"isgraphislowerisprintispunctisspaceisupper",
				"jmp_buflongjmpmemmoveputcharreallocsetvbuf",
				"sprintfstrcspnstrncatstrncmpstrncpystrpbrk",
				"strrchrstrtoultmpfiletolowertoupperva_list",
				"vprintf",
				7),
		       7, 1);
() = define_keywords_n("C",
		       strncat ("clearerrdifftimeisxdigitstrerror",
				"strftimeva_startvfprintfvsprintf",
				2),
		       8, 1);
() = define_keywords_n("C", "localtime",
		       9, 1);

%!% Protoytype: Void cmode ();
%!% This is a mode that is dedicated to facilitate the editing of C language files.  
%!% Functions that affect this mode include:
%!% @ function:             default binding:
%!% @ c_insert_bra               {
%!% @ c_insert_ket               }
%!% @ newline_and_indent         RETURN
%!% @ indent_line                TAB
%!% @ goto_match                 Ctrl-\ 
%!% @ c_make_comment             ESC ;
%!% @ c_format_paragraph         ESC q
%!% @ c_top_of_function          ESC Ctrl-A
%!% @ c_end_of_function          ESC Ctrl-E
%!% @ c_mark_function            ESC Ctrl-H
%!% Variables affecting indentation include:
%!% @ C_INDENT
%!% @ C_BRACE
%!% @ C_BRA_NEWLINE
%!% @ C_CONTINUED_OFFSET
%!% @ C_Comment_Column  (used by c_make_comment)
%!% Hooks: @c_mode_hook@
define c_mode ()
{
   variable kmap = "C";
   set_mode(kmap, 2);
   use_keymap(kmap);
   use_syntax_table (kmap);
   set_buffer_hook ("par_sep", "c_paragraph_sep");
   set_buffer_hook ("indent_hook", "c_indent_line");
   set_buffer_hook ("newline_indent_hook", "c_newline_and_indent"); 
   runhooks("c_mode_hook");
}

