/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: latex.sc
   Path: SmacLib/latex.sc
   Description: 
   Created: Mon Dec 12 13:02:24 MET 1994
   Author: Bruno Pages
   Modified: Mon Dec 12 13:02:26 MET 1994
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: latex mode

   Requires: mode.sc (for `standard' key bindings)

   SMAC programmer interface: 
   
   	latex_indent_step (configuration var)
	latex_indent_chapter (configuration var)
	goto_beginning_of_latex_definition()		<esc>a
	latex_indent_line()				<tab>
	latex_newline_and_indent()			<return>
	latex_indent_region()				<esc>i

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */

/*
   A very simple mode (named `Latex') to wrap lines and indent them following
   Latex keywords.

   I suppose the used font is fixed, so the width of all characters is the same
   The number of characters in a line is depending of the window width and the
   used font, but you can fixe it changing window_width('a') to the desired
   value.
   
   When you inputs a key and the line became too long the previous last space
   is changed to \n and the end of line is collapsed with the next if this last
   is not an empty line.
   
   It is not possible to insert a space or a tab at the beginning of line, if
   you really want to do it, input a control-q key before the space
   
   you can change latex_indent_step and latex_indent_chapter
   
*/

/* indent step, may be changed */
int latex_indent_step = 3;

/* if latex_indent_chapter == 0 part and chapter contents are not shifted,
   may be changed */
int latex_indent_chapter = 0;

/* key bindings, may be changed */
{
  create_mode("Latex");
  
  /* the `default' mode bindings */
  
  key_def("Latex", "^[f", "forward_word");
  key_def("Latex", "^[b", "backward_word");

  key_def("Latex", "^[d", "delete_next_word");
  key_def("Latex", "^[\b", "delete_previous_word");
  key_def("Latex", "^[\177", "delete_previous_word");      /* esc delete */

  key_def("Latex", "^[u", "upcase_word");
  key_def("Latex", "^[l", "downcase_word");
  key_def("Latex", "^[c", "capitalize_word");
  
  /* the specific bindings */
  
  key_def("Latex", "^[a", "goto_beginning_of_latex_definition");
  
  key_def("Latex", "\t", "latex_indent_line");
  key_def("Latex", "\r", "latex_newline_and_indent");

  key_def("Latex", "^[i", "latex_indent_region");
  
  set_mode_suffixes ("Latex", ".tex .latex" );
}

/* keywords must be at line beginning */
char * latex_keywords[20];

/* keywords indentation */

/* Last latex_keywords index (+1) changing indent */
int latex_indent_index;

int isa_latex_keyword();

/**/

int latex_line_too_long(int col, int * first);
int latex_change_column(int column);
int latex_column();
void latex_wrap_if_needed(int column);
int latex_reindent_line(int * pcolumn);

void latex_indent_line()
{
  int p = current_position();
  int column;
  
  p += latex_reindent_line(&column);

  if (p > (beginning_of_line() + column/8 + (column & 7)))
    goto_char(p);
  else
    p = current_position();
  latex_wrap_if_needed(column);
  goto_char(p);
}


/* function called for each normal character */

void latex_insert()
{
  int p = current_position();
  int column = latex_column();
  int line = current_line();
  
  if (p <= current_position()) {
    if (last_key() == ' ') {
      latex_wrap_if_needed(column);
      return;
    }
  }
  else
    goto_char(p);
  
  insert_char(last_key());

  latex_wrap_if_needed(column);

  if (last_key() == ' ') {
   if (p == current_position()) {
     insert_char('\n');
    }
    else if ((previous_char() == '\n') && 
	     (current_char() == '\n')) {
      insert_char('\n');
      goto_previous_char();
    }
    if(current_line() > line)
      latex_indent_line();
  }
}

/*
void latex_insert_OLD()
{
  int p = current_position();
  int column = latex_column();
  
  if (p <= current_position()) {
    if (last_key() == ' ') {
      latex_wrap_if_needed(column);
      return;
    }
  }
  else
    goto_char(p);
  
  insert_char(last_key());

  latex_wrap_if_needed(column);
}
*/

/* commands */

void goto_beginning_of_latex_definition()
{
  int index;
  
  goto_beginning_of_line();
  do {
    goto_previous_line();
    index = isa_latex_keyword();
  } while (current_position() && ((! index) || (index >= latex_indent_index)));
}

void latex_newline_and_indent()
{
  insert_char('\n');
  latex_indent_line();
}

void latex_indent_region()
{
  int distance;
  
  watch_on();
  {
    int beginning = mark_position();
    int end = current_position();
    
    if (end < beginning) {
      end = beginning;
      beginning = current_position();
    }
    goto_char(beginning);
    distance = end_of_file() - end;
  }  
  while ((end_of_file() - current_position()) > distance) {
    int column;
    int line;
    
    latex_reindent_line(&column);
    line = current_line();
    latex_wrap_if_needed(column);
    if (current_line() == line) goto_next_line();
  }
  watch_off();
}

/**/

int latex_line_too_long(int col, int * first)
{
  int max = window_width('a');
  int p = current_position();

  while (col < max) {
    switch (current_char()) {
    case '\t' :
      col = (col & ~7) + 8;
      break;
    case '\n' :
    case 0 :
      goto_char(p);
      return 0;
    case ' ':
      col += 1;
      break;
    default:
      if (first) *first = current_position();
      {
	int eol = end_of_line();
	int here = current_position();
	
	while (msearch("\t", eol, 1)) {
	  if ((col += (current_position() - here)) < max) {
	    goto_next_char();
	    here = current_position();
	    if ((col = (col & ~7) + 8) >= max)
	      return 1;
	  }
	  else {
	    goto_char(current_position() - (col - max));
	    return 1;
	  }
	}
	if ((col + (eol - here)) >= max) {
	  goto_char(here + (max - col));
	  return 1;
	}
	else {
	  goto_char(p);
	  return 0;
	}
      }
    }
    goto_next_char();
  }
}

int latex_change_column(int column)
{
  int correction = 0;

  goto_beginning_of_line();
  
  /* add necessary tab */
  
  for (; column >= 8; column -= 8) /* tabs */
    switch (current_char()) {
    case ' ' :
      replace_char('\t');
    case '\t' :
      goto_next_char();
      break;
    default:
      insert_char('\t');
      correction += 1;
  }
  
  /* add necessary spaces */
  
  while (column--)
    switch (current_char()) {
    case '\t' :
      replace_char(' ');
    case ' ' :
      goto_next_char();
      break;
    default:
      insert_char(' ');
      correction += 1;
  }
  
  /* remove undesired tabs and spaces */
  
  if (! correction)
    while (current_char() && strchr(" \t", current_char())) {
    delete_char();
    correction -= 1;
  }
  
  return correction;
}

int latex_column()
{
  int column = 0;
  
  goto_beginning_of_line();
  for (;;) {
    if (current_char() == ' ')
      column += 1;
    else if (current_char() == '\t')
      column = (column & ~7) + 8;
    else
      return column;
    goto_next_char();
  }
}  

int isa_latex_document()
{
  int pos = current_position();
  int result = 
    msearch("o", end_of_line(), 1) &&
      (previous_char() == 'd') &&
	(next_char() == 'c');
  
  goto_char(pos);
  return result;
}

int isa_latex_keyword()
    /* returns value > 0 if the next line beginning is a latex keyword */
{
  int pos = current_position();
  
  while ((current_char() == ' ') || (current_char() == '\t'))
    goto_next_char();

  if (current_char() == '\\') {
    int index = -1;
    int beg = current_position();
    
    while (latex_keywords[++index]) {
      char * s = latex_keywords[index];
      
      goto_char(beg + 1);
      while (*++s == current_char())
	goto_next_char();
      if ((! *s) && 
	  (((index + 2) != latex_indent_index) ||
	   (! isa_latex_document()))) {
	goto_char(pos);
	return index + 1;
      }
    }
  }
  
  goto_char(pos);
  return 0;
}

int latex_at_end_of_word(char * word)
{
  char * p = word;
  int pos = current_position();
  
  while (*p) p+= 1;
  
  while (current_char() == *--p) {
    if (p == word) {
      goto_char(pos);
      return 1;
    }
    goto_previous_char();
  }
  
  goto_char(pos);
  return 0;
}

int latex_next_line_is_empty()
{
  int p = current_position();
  int result = 1;
  
  for (;;)
    switch (next_char()) {
    case 0:
    case '\n':
      goto_char(p);
      return 1;
    default:
      goto_char(p);
      return 0;
    case ' ':
    case '\t':
      goto_next_char();
  }
}
  
void latex_wrap_if_needed(int column)
{
  int p = current_position();
  int first;

  while ((goto_beginning_of_line(), latex_line_too_long(0, &first)) &&
	 backward_search(" ") && (current_position() >= first)) {
    if (strchr("\n", next_char())) {
      delete_char();
      break;
    }
    replace_char('\n');
    goto_next_char();
    {
      int here = current_position();
      int delta = latex_change_column(column);
      
      if (delta) {
	if (here < p) p += delta;
	if (latex_line_too_long(column, 0)) continue;
      }
    }
    goto_end_of_line();
    if (latex_next_line_is_empty() ||
	(previous_char() == '\\') || latex_at_end_of_word("\\hline"))
      break;
    goto_next_char();
    if (isa_latex_keyword()) {
      goto_previous_char();
      break;
    }
    goto_previous_char();
    replace_char(' ');
    goto_next_char();
    while ((current_char() == ' ') || (current_char() == '\t'))
      delete_char();
    goto_previous_char();
  }

  goto_char(p);
}

int latex_frame_beginning(int * pniv)
{
  int pos = current_position();
  int niv = 0;
  
  if (pniv) *pniv = 0;
  
  while (current_position()) {
    int v = isa_latex_keyword();
    
    if (v == latex_indent_index)
      /* end */
      niv += 1;
    else if (v == (latex_indent_index - 1)) {
      /* begin */
      if (! niv) break;
      niv -= 1;
    }
    else if (v && (v < latex_indent_index)) {
      if (pniv) *pniv = v;
      break;
    }
    goto_previous_line();
  }
  
  niv = latex_column();
  goto_char(pos);
  
  return niv;
}

int latex_encompassed_frame_beginning(int type)
{
  int p = current_position();
  int result = 0;

  while (type > 0) {
    if (backward_search(latex_keywords[--type])) {
      result = latex_column();
      break;
    }
  }
  
  goto_char(p);
  return result;
}

int latex_reindent_line(int * pcolumn)
{
  int type;
  
  goto_beginning_of_line();
  
  if ((type = isa_latex_keyword()) == latex_indent_index) {
    /* an \end{??} */
    if (isa_latex_document())
      return latex_change_column(*pcolumn = 0);
    goto_previous_line();
    *pcolumn = latex_frame_beginning(0);
  }
  else if (type && (type < (latex_indent_index - 1))) {
    int prevtype;
    
    goto_previous_line();
    if ((! latex_indent_chapter) && (type <= 2))
      *pcolumn = 0;
    else {
      *pcolumn = latex_frame_beginning(&prevtype);
      if (prevtype < type)
	*pcolumn += latex_indent_step;
      else if (prevtype != type)
	*pcolumn = latex_encompassed_frame_beginning(type);
    }
  }
  else {
    goto_previous_line();
    if (((type = isa_latex_keyword()) != 0) && (type < latex_indent_index)
	&& (latex_indent_chapter || (type > 2)))
      *pcolumn = latex_column() + latex_indent_step;
    else
      *pcolumn = latex_column();
  }
  goto_next_line(); 
  
  if (latex_column() == *pcolumn)
    return 0;
  
  return latex_change_column(*pcolumn);
}

/* */

void init_latex() 
{
  char s[2];
  int c;
  
  s[1] = 0;

  /* bind all ascii visible characters to wrap line */  

  for (c = ' '; c <= '~'; c += 1) {
    s[0] = c;
    key_def("Latex", s, "latex_insert");
  }

  /**/
  
  latex_keywords[0] = "\\part";
  latex_keywords[1] = "\\chapter";
  latex_keywords[2] = "\\section";
  latex_keywords[3] = "\\subsection";
  latex_keywords[4] = "\\subsubsection";
  latex_keywords[5] = "\\paragraph";
  latex_keywords[6] = "\\subparagraph";
  latex_keywords[7] = "\\begin";
  latex_keywords[8] = "\\end";
  latex_indent_index = 8 + 1;
  
  latex_keywords[9] = "\\item";
  latex_keywords[10] = "\\\\";
  latex_keywords[11] = "\\newcommand";
  latex_keywords[12] = "\\def";
  latex_keywords[13] = "\\input";
  latex_keywords[14] = "\\newpage";
  latex_keywords[15] = "\\bibitem";
  latex_keywords[16] = "\\subitem";
  latex_keywords[17] = "\\indexspace";
  latex_keywords[18] = "\\newline";
  latex_keywords[19] = 0;
}

{
  init_latex();
}

