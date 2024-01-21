% BIBTeX mode for JED
% 
% Version: 1.4
% Author:  Carsten Tinggaard Nielsen, tinggard@iesd.auc.dk
% Update:  29 May 1995
% -----------------------------------------------------------------------
% History:
% 1.4 29 May 1995
%   fixed bug refering to bibtex_insert_quote: using tex_insert_quote
%   added usage of LaTeX font commands: ^C^F 
%     as suggested by Franz-Josef Knelangen
% 1.3 16 May 1995
%   next/prev field can now position correct in "" and "{}"
%   changed def of next entry to ESC N / Meta N
%   changed def of prev entry to ESC P / Meta P
%     ESC p and ESC P does the same job now.
% 1.2 11 May 1995
%   from fjk@ruf.uni-freiburg.de (Franz-Josef Knelangen):
%     next field ^n
%     prev field ^p
%     next entry ESC n
%     prev entry ESC p
% 1.1 10 May 1995
%   Optimized code after suggestions by John E. Davis
% 1.0 08 May 1995 
%   Public release to comp.editors,comp.text.tex,alt.lang.s-lang
% -----------------------------------------------------------------------
% 
% When bibtex mode is loaded, 'bibtex_mode_hook' is called.
% This hook will allow users to customize the mode.
% So, in your jed.rc /.jedrc file,
% add something like:
%   define bibtex_mode_hook () {
%      local_setkey ("bibtex_Article", "^C^E^A");
%   }
% which binds the function to Ctrl-C Ctrl-E Ctrl-A
% 
% For customization, the 'bibtex_item_hook' is called everytime a
% template is entered into the buffer.
% To add your own entry, 
% then add something like (in your jed.rc /.jedrc file):
%   autoload ("bib_field", "bibtex.sl");
%   define bibtex_item_hook () {
%      bib_field("myentry");
%   }
% Your additions will be placed before the note/annote field.
% The variable bibtex_item_name holds current name like Article, Manual etc.

%-------------------------------------------------------------------------
% Load the common definitions if not already loaded.  This also defines
% the TeX-Mode syntax table
!if (is_defined ("tex_font")) () = evalfile("latex");


%-------------------------------------------------------------------------
variable bib_OPT_flag = 0;  % if true then insert OPT in the field names
variable bib_indent = 16;   % amount of whitespace before '='
variable bibtex_item_name = Null_String; % name of current item

define bib_set_OPT () { bib_OPT_flag = 1; }
define bib_unset_OPT () { bib_OPT_flag = 0; }

define bib_field (fieldstr)  {
   variable sl;
   sl = bib_indent - strlen(fieldstr);
   insert(",\n  ");
   if (bib_OPT_flag) {
      insert("OPT");
      sl -= 3;
   }
   vinsert ("%s =", fieldstr, 1);
   insert_spaces(sl);
   if (strcmp(fieldstr,"title"))
     insert("\"\"");
   else
     insert("\"\{\}\"");
}

define bib_journal () { bib_field("journal"); }
define bib_year () { bib_field("year"); }
define bib_volume () { bib_field("volume"); }
define bib_number () { bib_field("number"); }
define bib_pages () { bib_field("pages"); }
define bib_month () { bib_field("month"); }
define bib_editor () { bib_field("editor"); }
define bib_publisher () { bib_field("publisher"); }
define bib_series () { bib_field("series"); }
define bib_address () { bib_field("address"); }
define bib_edition () { bib_field("edition"); }
define bib_howpublished () { bib_field("howpublished"); }
define bib_booktitle () { bib_field("booktitle"); }
define bib_organization () { bib_field("organization"); }
define bib_institution () { bib_field("institution"); }
define bib_school () { bib_field("school"); }
define bib_type () { bib_field("type"); }
define bib_chapter () { bib_field("chapter"); }
define bib_note () { bib_field("note"); }
define bib_author () { bib_field("author"); }

define bib_item (itemstr, use_author)  {
   vinsert("@%s{", itemstr, 1);
   bibtex_item_name = itemstr;
   bib_unset_OPT();
   if (use_author) { bib_author();}
   bib_field("title");
}

define bib_itemend (use_note)  {
   bib_field("location");
   bib_field("isbn");
   bib_field("keywords");
   runhooks("bibtex_item_hook");
   if (use_note) { bib_note(); }
   bib_field("annote");
   insert("\n}\n\n");
   pop(bsearch("{,"));
   go_right_1 ();
}

define bib_cref_and_key() {
   bib_set_OPT();
   bib_field("crossref");
   bib_field("key");
}

define bibtex_Article ()  {
   bib_item("Article", 1);
   bib_journal();
   bib_year();
   bib_cref_and_key();
   bib_volume();
   bib_number();
   bib_pages();
   bib_month();
   bib_itemend(1);
}

define bibtex_Book ()  {
   bib_item("Book", 1);
   bib_publisher();
   bib_year();
   bib_cref_and_key();
   bib_editor();
   bib_volume();
   bib_number();
   bib_series();
   bib_address();
   bib_edition();
   bib_month();
   bib_itemend(1);
}

define bibtex_preamble () {
   insert("@Preamble\{\}\n");
   go_left(2);
}

define bibtex_string () {
   insert("@string\{ = \"\"\}\n");
   go_left(7);
}

define bibtex_Unpublished () {
   bib_item("Unpublished", 1);
   bib_note();
   bib_cref_and_key();
   bib_year();
   bib_month();
   bib_itemend(0);
}

define bibtex_TechReport () {
   bib_item("TechReport", 1);
   bib_institution();
   bib_year();
   bib_cref_and_key();
   bib_type();
   bib_number();
   bib_address();
   bib_month();
   bib_itemend(1);
}

define bibtex_PhdThesis () {
   bib_item("PhdThesis", 1);
   bib_school();
   bib_year();
   bib_cref_and_key();
   bib_address();
   bib_month();
   bib_type();
   bib_itemend(1);
}

define bibtex_Proceedings () {
   bib_item("Proceedings", 0);
   bib_year();
   bib_cref_and_key();
   bib_editor();
   bib_volume();
   bib_series();
   bib_publisher();
   bib_organization();
   bib_address();
   bib_month();
   bib_itemend(1);
}

define bibtex_Misc () {
   bib_item("Misc", 0);
   bib_cref_and_key();
   bib_author();
   bib_howpublished();
   bib_year();
   bib_month();
   bib_itemend(1);
}

define bibtex_MastersThesis () {
   bib_item("MastersThesis", 1);
   bib_school();
   bib_year();
   bib_cref_and_key();
   bib_address();
   bib_month();
   bib_type();
   bib_itemend(1);
}

define bibtex_Manual () {
   bib_item("Manual", 0);
   bib_cref_and_key();
   bib_author();
   bib_organization();
   bib_address();
   bib_edition();
   bib_year();
   bib_month();
   bib_itemend(1);
}

define bibtex_InProceedings () {
   bib_item("InProceedings", 1);
   bib_cref_and_key();
   bib_editor();
   bib_volume();
   bib_number();
   bib_series();
   bib_pages();
   bib_booktitle();
   bib_year();
   bib_organization();
   bib_publisher();
   bib_address();
   bib_month();
   bib_itemend(1);
}

define bibtex_InCollection () {
   bib_item("InCollection", 1);
   bib_cref_and_key();
   bib_booktitle();
   bib_publisher();
   bib_year();
   bib_editor();
   bib_volume();
   bib_number();
   bib_series();
   bib_type();
   bib_chapter();
   bib_pages();
   bib_address();
   bib_edition();
   bib_month();
   bib_itemend(1);
}

define bibtex_InBook () {
   bib_item("InBook", 1);
   bib_chapter();
   bib_cref_and_key();
   bib_publisher();
   bib_year();
   bib_editor();
   bib_pages();
   bib_volume();
   bib_number();
   bib_series();
   bib_address();
   bib_edition();
   bib_type();
   bib_month();
   bib_itemend(1);
}

define SearchInThisLine(str) {
   % return 1 if str is found at current line
   variable thisline = what_line();
   variable res = 0;
   
   bol();   
   if (fsearch(str))
     if (thisline == what_line())
       res = 1;
   !if (res) {
      goto_line(thisline);
      bol();
   }
   return res;
}

variable bibtex_remove_value = 0; % 0:nothing removed 1:OPT 2:killed

define bibtex_removeOPT () {
   % remove the string OPT from the current line
   % if there is no text in the entry then the line is deleted
   
   % ensure beginning of current line
   bol();
   bibtex_remove_value = 0;
   % if current line has OPT then go on
   if (SearchInThisLine(" OPT")) {
      % replace OPT with spaces after =
      go_right_1 ();
      deln(3);
      if (SearchInThisLine(" =")) {
	 go_right(2);
	 insert_spaces(3);
      }
      bibtex_remove_value = 1;
      % if the line has no information then kill it
      if (SearchInThisLine("\"\"")) {
	 delete_line();
	 bibtex_remove_value = 2;
      }
   }
}

define bibtex_clean_entry ()  {
% remove lines where field starts with OPT and ends with ""
% remove lines if the contains no information
% if lines has OPT and "<anything>" then OPT is removed and
% line is adjusted accordingly
% the stopmark is "}" at the first position in a line or eobp().
   () = bsearch_char ('@');
   go_down_1();
   while (looking_at_char('}') == 0) {
      bibtex_removeOPT();
      if (bibtex_remove_value == 1)
	go_down(1); % removed OPT in line with content
      !if (bibtex_remove_value) {
	 % did nothing, check for empty line
	 if (SearchInThisLine("\"\""))
	   delete_line();
	 else
	   go_down_1();
      }
      bol();
   } 
   % there must not be a comma after the last entry
   go_left(2);
   if (looking_at_char(','))
     del();
   bol();
   go_down(3);
}

define bibtex_no(whatdir, whatstr) 
{
   vmessage ("There is no %s %s", whatdir, whatstr, 2);
}

define bibtex_no_next(whatstr) { bibtex_no("next", whatstr); }

define bibtex_no_prev(whatstr) { bibtex_no("previous", whatstr); }

define bibtex_next_entry() {
   % jump to the next entry: Article, Manual
   if (fsearch_char ('@')) { % first char of entry
      () = fsearch_char ('{'); % point at label
      go_right_1 ();
   }
   else
     bibtex_no_next("entry");
}

define bibtex_prev_entry() {
   % jump to the previous entry: Article, Manual
   if (bsearch_char ('}')){      % last char of entry
      () = bsearch_char ('@');   % first char of entry
      () = fsearch_char ('{');   % point at label
      go_right_1 ();
   }
   else
     bibtex_no_prev("entry");
}

define bibtex_go_into_field() {
   % A std. field is of the form ""
   % the title field is of the form "{}"
   % if the field is of the title form "{}" the
   % posiiton the cursor between the braces.
   go_right(2);
   if (looking_at_char ('{'))
     go_right_1 ();
}

define bibtex_next_field() {
   % jump to the next field: author, title
   if (fsearch(" \""))
     bibtex_go_into_field();
   else
     bibtex_no_next("field");
}

define bibtex_prev_field() {
   % jump to the previous field: author,title
   bol();
   if (bsearch(" \""))
     bibtex_go_into_field();
   else
     bibtex_no_prev("field");
}

variable bibtexName = "BibTeX";
variable bibtexModeName = strcat(bibtexName, "-Mode");

!if (keymap_p(bibtexModeName))
{
   make_keymap (bibtexModeName);
   definekey ("tex_insert_quote", "\"", bibtexModeName);
   definekey ("tex_insert_quote", "'", bibtexModeName);
   definekey ("tex_blink_dollar", "$", bibtexModeName);
}

define bibtex_mode ()
{
   use_keymap (bibtexModeName);
   set_mode (bibtexName, 0x1 | 0x20);
   set_buffer_hook ("par_sep", "tex_paragraph_separator");
   set_buffer_hook ("wrap_hook", "tex_wrap_hook");
   TAB = 0; % pure spaces in this mode
   local_setkey("bibtex_Article", "^Ca");
   local_setkey("bibtex_Book", "^Cb");
   local_setkey("bibtex_Unpublished", "^Cu");
   local_setkey("bibtex_string", "^Cs");
   local_setkey("bibtex_TechReport", "^Ct");
   local_setkey("bibtex_PhdThesis", "^CT");
   local_setkey("bibtex_preamble", "^CP");
   local_setkey("bibtex_Proceedings", "^Cp");
   local_setkey("bibtex_Misc", "^CM");
   local_setkey("bibtex_MastersThesis", "^Cm");
   local_setkey("bibtex_Manual", "^C^M");
   local_setkey("bibtex_InProceedings", "^CI");
   local_setkey("bibtex_InCollection", "^Ci");
   local_setkey("bibtex_InBook", "^CB");
   local_setkey("bibtex_removeOPT", "^C^O");
   local_setkey("bibtex_clean_entry", "^C^C");
   local_setkey("bibtex_next_field", "^N");
   local_setkey("bibtex_prev_field", "^P");
   local_setkey("bibtex_next_entry", "^[N");
   local_setkey("bibtex_prev_entry", "^[P");
   local_setkey("tex_font", "^C^F");
   runhooks ("bibtex_mode_hook");
}
%-----------------------------------------------------------%

define bibtex_info_find_node ()
{
   variable node;
   
   node = read_mini ("Node:", Null_String, Null_String);
   !if (strlen (node)) return;
   info_mode ();
   info_find_node ("(bibtex)top");
   info_find_node (strcat ("(bibtex)", node));
}
