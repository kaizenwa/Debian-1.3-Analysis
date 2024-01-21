%
%  file : html.sl
%
%  Original Author : Raikanta Sahu, rsahu@mail.unm.edu
%  Substantial additions by Jim Knoble.

%  Modified by John E. Davis for incorporation into JED.


% 1 => html_mode wraps, like text_mode
% 0 => html_mode doesn't wrap, like no_mode

define html_paragraph_separator ()
{
   bol_skip_white ();
   eolp () or ffind_char ('>') or ffind_char ('<');
}

% Movement function (JM)
%!% skip forward past html tag
define html_skip_tag()
{
   !if (fsearch_char ('>')) return;
   go_right_1 ();
}

%!% skip backward past html tag
define html_bskip_tag()
{
   () = bsearch_char ('<');
}

%!% mark the next html tag forward
define html_mark_next_tag()
{
   variable taglng = 1;

   !if (fsearch_char ('>')) return;
   go_right(taglng);
   set_mark_cmd ();
   go_left(taglng);
   () = find_matching_delimiter (0);
}

%!% mark the previous html tag
define html_mark_prev_tag()
{
   !if (bsearch_char ('<')) return;
   set_mark_cmd ();
   () = find_matching_delimiter(0);
   go_right_1 ();
   exchange_point_and_mark();
}


%
% First define some useful functions
%

define html_insert_pair_around_region (lfttag, rgttag)
{
   % make sure mark is before point;
   % 1 => push spot first
   check_region(1);
        
   % put tags on appropriate sides of region,
   % then return to where we were
   exchange_point_and_mark();
   insert(lfttag);
   exchange_point_and_mark();
   insert(rgttag);
   pop_spot();
   pop_mark_0 ();
}

define html_insert_move (str)
{
   variable len;
   variable beg, end;
   
   len = is_substr (str, "@");
   !if (len) return;
   len--;
   if (markp ())
     {
	beg = substr (str, 1, len);
	end = substr (str, len + 2, strlen (str));
	html_insert_pair_around_region (beg, end);
	return;
     }
   
   push_spot ();
   insert (str);
   pop_spot ();
   go_right (len);
   del ();
}

define html_simple_insert (str)
{
   html_insert_move (Sprintf ("<%s>@</%s>", str, str, 2));
}

define html_insert_with_newline (str)
{
   html_insert_move (Sprintf ("<%s>\n@\n</%s>\n", str, str, 2));
}

define html_template ()
{
   insert ("<!doctype html public \"-//ietf//dtd html 2.0//en\">\n");
   html_insert_move ("<HTML>\n\n<HEAD>\n<TITLE>@</TITLE>\n</HEAD>\n\n<BODY>\n</BODY>\n\n</HTML>") ;
}

define html_form ()
{
   html_insert_move ("<form action=\"\" method=\"\">\n@\n</form>");
}

define html_input ()
{
   insert ("<input type=\"\" name=\"\" value=\"\">");
}

define html_input_text ()
{
   insert ("<input type=\"text\" name=\"\" value=\"\">");
}

define html_input_password ()
{
   insert ("<input type=\"password\" name=\"\" value=\"\">");
}

define html_input_checkbox ()
{
   insert ("<input type=\"checkbox\" name=\"\" value=\"\">");
}

define html_input_radio ()
{
   insert ("<input type=\"radio\" name=\"\" value=\"\">");
}

define html_input_submit ()
{
   insert ("<input type=\"submit\" value=\"OK\">");
}

define html_input_reset ()
{
   insert ("<input type=\"reset\"  value=\"Clear\">");
}

define html_input_hidden ()
{
   insert ("<input type=\"hidden\" name=\"\" value=\"\">");
}

define html_select ()
{
   html_insert_move ("<select name=\"@\" size=\"\">\n@\n</select>");
}

define html_text_area ()
{
   html_insert_move ("<textarea name=\"@\"></textarea>");
}

%
%  Make comment
%
define html_comment ()
{
   html_insert_move ("<!-- @ -->");
}

%
% insert Horizontal rule  TJO
%
define html_horiz_rule ()
{
   insert("\n<HR>\n") ;
}

define html_heading (c)
{
   html_insert_move (Sprintf ("<H%c>@</H%c>", c, c, 2));
}

define html_insert_eol (str)
{
   eol ();
   vinsert ("<%s>", str, 1);
}

define html_insert_bol (str)
{
   bol ();
   vinsert ("<%s>", str, 1);
}

%
% Make markers for an image
%
define html_image ()
{
   html_insert_move ("<IMG SRC=\"@\" ALT=\"\">");
}

%
% main entry point into the html mode
% commands available to keystrokes in html mode
%

define html_quoted_insert ()
{
   !if (input_pending (5)) flush ("`-");
   switch (getkey ())
     {
      case '\r':
	insert ("<br>\n");
     }
     {
      case '&':
	insert ("&amp;");
     }
     {
      case '>':
	insert ("&gt;");
     }
     {
      case '<':
	insert ("&lt;");
     }
     {
	% default:  The other special characters should be added.
	insert_char ();		       
     }
}


define html_read_key (hlp)
{
   variable key;
   !if (input_pending (3)) flush (hlp);
   tolower (getkey ());
}

define html_keymap_a ()
{
   variable name = "<A NAME=\"@\"></A>";
   variable href = "<A HREF=\"@\"></A>";
   
   switch (html_read_key ("Href  Name"))
     { case 'h': href; }
     { case 'a': href; }
     { case 'n': name; }
     {
	pop (); beep (); return;
     }
   html_insert_move (());
}

define html_keymap_d ()
{
   insert ("<dd>");
}

define html_keymap_f ()
{

   switch (html_read_key ("txtArea Chkbox Form Hidden Input Option Passw Radio Select Text Xreset Ysubmit"))
     {case 'a': html_text_area (); }
     {case 'c': html_input_checkbox (); }
     {case 'f': html_form (); }
     {case 'h': html_input_hidden (); }
     {case 'i': html_input (); }
     {case 'o': html_insert_bol("option"); }
     {case 'p': html_input_password (); }
     {case 'r': html_input_radio (); }
     {case 's': html_select (); }
     {case 't': html_input_text (); }
     {case 'x': html_input_reset (); }
     {case 'y': html_input_submit (); }
     {
	% default
	pop ();
	beep ();
     }
}

define html_keymap_h ()
{
   variable key;
   switch (html_read_key ("h1  h2  h3  h4  h5  h6  templAte Doc  Head  Body  htmL  Title"))
     { case 'd': html_insert_bol ("doc"); }
     { case 'h': html_insert_with_newline ("head"); }
     { case 'b': html_insert_with_newline ("body"); }
     { case 'l': html_insert_with_newline ("html"); }
     { case 't': html_insert_with_newline ("title"); }
     { case 'a': html_template (); }
     { % default:
	key = ();
	if ((key <= '6') and (key >= '1'))
	  html_heading (key);
	else beep ();
     }
}

define html_keymap_i ()
{
   html_image ();
}

define html_keymap_l ()
{
   switch (html_read_key ("Dir Li Menu Ordered Un-ordered"))
     { case 'd': html_insert_with_newline ("dir"); }
     { case 'i': html_insert_bol ("li"); }
     { case 'l': html_insert_bol ("li"); }
     { case 'm': html_insert_with_newline ("menu"); }
     { case 'o': html_insert_with_newline ("ol"); }
     { case 'u': html_insert_with_newline ("ul"); }
     {
	% default 
	pop ();
	beep ();
     }
}

define html_keymap_p ()
{
   switch (html_read_key ("Break Hrule Par blockQuote pRe"))
     { case 'b': html_insert_eol ("br"); }
     { case 'h': html_horiz_rule (); }
     { case 'p': insert ("<p>\n"); }
     { case 'q': html_insert_with_newline ("blockquote"); }
     { case 'r': html_insert_with_newline ("pre"); }
     {
	pop();
	beep ();
     }
}

define html_keymap_s ()
{
   switch (html_read_key ("Address Bold Cite Emph Ital Kbd cOde Samp Tt Uline Var"))
     { case 'a': "address"; }
     { case 'b': "b"; }
     { case 'c': "cite"; }
     { case 'e': "em"; }
     { case 'i': "i"; }
     { case 'k': "kbd"; }
     { case 'o': "code"; }
     { case 's': "samp"; }
     { case 't': "tt"; }
     { case 'u': "u"; }
     { case 'v': "var"; }
     {
	pop ();	beep (); return;
     }
   html_simple_insert (());
}


define html_keymap ()
{
   switch (html_read_key ("Anchors  Dfnlists  Forms  Headings  Images  Lists  Pstyles  cStyles  ?help"))
     { case 2: html_bskip_tag (); }		       %  ^B
     { case 6: html_skip_tag (); }		       %  ^F
     { case 14: html_mark_next_tag (); }   %  ^N
     { case 14: html_mark_prev_tag (); }   %  ^P
     { case 'c': html_comment (); }
     { case 'a': html_keymap_a (); }
     { case 'd': html_keymap_d (); }
     { case 'f': html_keymap_f (); }
     { case 'h': html_keymap_h (); }
     { case 'i': html_keymap_i (); }
     { case 'l': html_keymap_l (); }
     { case 'p': html_keymap_p (); }
     { case 's': html_keymap_s (); }
     {
	ungetkey (());
	html_quoted_insert ();
     }
   flush ("");
}

$1 = "html";
!if (keymap_p ($1)) make_keymap ($1);
undefinekey ("^C", $1);
definekey("html_keymap",                             "^C", $1);
undefinekey ("\e;", $1);
definekey ("html_comment",   "\e;",  $1);
definekey ("html_quoted_insert",   "`",  $1);


create_syntax_table ($1);
define_syntax ("<", ">", '(', $1);     %  make these guys blink match
define_syntax ("<>", '<', $1);
define_syntax ("<!", "->", '%', $1);
define_syntax ("A-Za-z&", 'w', $1);
define_syntax ('#', '#', $1);

#ifdef HAS_DFA_SYNTAX
% The highlighting copes with comments, "&eth;" type things, and <argh> type
% HTML tags. An unrecognised &..; construct or an incomplete <...> construct
% is flagged in delimiter colour.
enable_highlight_cache ("html.dfa", $1);
define_highlight_rule ("<!.*-[ \t]*>", "Qcomment", $1);
define_highlight_rule ("^([^\\-]|-+[^>])*-+[ \t]*>", "Qcomment", $1);
define_highlight_rule ("<!.*", "comment", $1);
define_highlight_rule ("<([^>\"]|\"[^\"]*\")*>", "keyword", $1);
define_highlight_rule ("<([^>\"]|\"[^\"]*\")*(\"[^\"]*)?$", "delimiter", $1);
define_highlight_rule ("&#[0-9]+;", "keyword1", $1);
define_highlight_rule ("&[A-Za-z]+;", "Kdelimiter", $1);
define_highlight_rule (".", "normal", $1);
build_highlight_table ($1);
#endif

() = define_keywords ($1, "&gt&lt", 3);
() = define_keywords ($1, "&ETH&amp&eth", 4);
() = define_keywords ($1, strcat (
				  "&Auml&Euml&Iuml&Ouml&Uuml",
				  "&auml&euml&iuml&nbsp&ouml&quot&uuml&yuml"
				  ), 
		      5);

() = define_keywords ($1, strcat (
  "&AElig&Acirc&Aring&Ecirc&Icirc&Ocirc&THORN&Ucirc&acirc",
  "&aelig&aring&ecirc&icirc&ocirc&szlig&thorn&ucirc"
				  ), 
		      6);

() = define_keywords ($1, strncat (
  "&Aacute&Agrave&Atilde&Ccedil&Eacute&Egrave&Iacute&Igrave",
  "&Ntilde&Oacute&Ograve&Oslash&Otilde&Uacute&Ugrave&Yacute",
  "&aacute&agrave&atilde&ccedil&eacute&egrave&iacute&igrave",
  "&ntilde&oacute&ograve&oslash&otilde&uacute&ugrave&yacute",
				   4),
		      7);

%!% Prototype: Void html_mode ();
%!% @html_mode@ is a mode designed for editing HTML files.  
%!% If a region is defined (i.e., if a mark is set), many HTML
%!% tags will insert around the region, e.g. '<B>' and '</B>'.
%!% 
%!% if the variable 'HTMLModeWraps' is set to 1, this mode will wrap
%!% (like @text_mode@ does); otherwise, this mode won't wrap (like @no_mode@).
%!% 
%!% Keybindings begin with ^C and are grouped according to function:
%!%     ^CA...  Anchors (<A>...</A>)
%!%     ^CD...  Definition lists (<DL>...</DL>)
%!%     ^CF...  Forms (<form>...</form>)
%!%     ^CH...  Headings, document type, etc.
%!%     ^CI...  Images
%!%     ^CL...  Lists (<UL>...</UL>)
%!%     ^CP...  Paragraph styles, etc. (<P>, <BR>, <HR>, <ADDRESS>, etc.)
%!%     ^CS...  Character styles (<EM>, <STRONG>, <B>, <I>, etc.)
%!% Additionally, some special movement commands and miscellaneous
%!% characters are defined:
%!%     ^C^B    skip to beginning of prior HTML tag
%!%     ^C^F    skip to end of next HTML tag
%!%     ^C^N    mark next HTML tag from '<' to '>'
%!%     ^C^P    mark prior HTML tag from '<' to '>'
%!%     ^C&     insert HTML text for '&'
%!%     ^C>     insert HTML text for '>'
%!%     ^C<     insert HMTL text for '<'
%!%     ^CC     insert HTML comment (around region, if marked)
%!% 
%!% For a complete list of keybindings, use @describe_bindings@.
%!% 
%!% This function calls @html_mode_hook@ if it exists.
define html_mode ()
{
   variable html = "html";

   set_mode(html, 1);
   set_buffer_hook ("par_sep", "html_paragraph_separator");
   use_syntax_table (html);
   use_keymap (html);
   runhooks ("html_mode_hook");
}


	 
