/*
** Copyright 1993 by Bruno Pages
**
** Permission to use, copy, and distribute for non-commercial purposes,
** is hereby granted without fee, providing that the above copyright
** notice appear in all copies and that both the copyright notice and this
** permission notice appear in supporting documentation.
** The software may be modified for your own purposes, but modified versions
** may not be distributed.
** This software is provided "as is" without any expressed or implied warranty.
**
**
*/

/* 
   ===============
   C and C++ modes
   ===============
   
   Key bindings :

    * `tab' to reindent the current line. Made by the `c_indent_line' function.

    * `return' inserts a `newline' and indent the new line according to previous
    lines.  Made  by the `c_indent_line' function. It will be perhaps necessary to
    type  `tab'  after  some characters modifying the line context (for instance
    after `case').

    * `}',  `)'  and `]' to insert the char and blink the corresponding `{', `('
    and  `[',  at  last  reindent  the  current line. Made by the function named
    `c_insert_blink_matched_char_and_indent'.

    * `{'  and `:' to insert the char and reindent the current line. Made by the
    `c_indent_line' function.
    

    beginning  of a definition is supposed to be a line whose first character is
    not  one  of #, }, `space, tab' and `newline', or whose two first characters
    are  not  //  or /*, and whose previous line last char (before `newline') is
    not  \.  Obviously  the  beginning  of the buffer is also considered to be a
    definition beginning. Made by the `goto_beginning_of_c_definition' function.

    * `escape  f'  go  to  the  end  of  the current or next expression (jumping
    comments).  Before a `{([' or `"' go after the corresponding `})]' or `"',
    else  go  to  the  end  of  the  following  word. A word contains alphabetic
    characters, digits and _. Made by the `forward_c_form' function.

    * `escape  b'  go  to  the  beginning  of the current or previous expression
    (jumping comments). Made by the `backward_c_form' function.
    
    * `escape  d'  deletes  the  end  of  the  current  expression  or  the next
    expression (jumping comments). Made by the `delete_next_c_form' function.

    * `escape  delete'  and  `escape  backspace'  deletes  the  beginning of the
    current  expression or the previous expression (jumping comments). Made by
    the `delete_previous_c_form' function.

    * `escape  i' indents the current region, if you put a mark at the beginning
    of  the  buffer  and  go to the end before type `escape i' all the buffer is
    reindented  (witch  is  not immediate when the buffer is large). Made by the
    `c_indent_region' function.

    
   Indentation is parametrized by the following global variables:
    
    * `int c_indent_in_comment' (default value 3)

    * `int c_indent_in_string' (default value 0)

    * `int c_indent_in_parenthesis' (default value 1)
    
    * `int c_indent_in_bracket' (default value 1)

    * `int c_indent_in_brace' (default value 2)

    * `int c_indent_in_statement' (default value 2)

    * `int c_arg_decl_indent' (default value 4)
    

   The  indentation  rules for an empty line, for instance when you type a
   `return' at the end of a line, are :

    * In  a  `comment'  indentation  is  set  to  previous (not  empty  or  only
    containing  spaces and tabs) line indentation, except for the second line of
    comment whose indentation is  the `/*' position  more the absolute  value of
    `indent_in_comment'.

    * In a `string', if `indent_in_string' is less than 0 the indentation is the
    string  beginning  position  minus  `indent_in_string' (so more its absolute
    value), else indentation equals `indent_in_string'.

    * In  a `parenthesis' indentation equals `(' position more absolute value of
    `indent_in_parenthesis'.
    
    * In  a  `bracket'  indentation  equals  `[' position more absolute value of
    `indent_in_bracket'.

    * In  a  `block'  (between  {})  it  all  depends  on the previous character
    (jumping space, `tab, newline' and comments):
        
        + If it is `{' (so you are at the beginning of the block) indentation is
        set to `{' position more the absolute value of `indent_in_brace'

        + If  it  is  `}' indentation is set to indentation of the corresponding
        `{' or of the expression preceding it.

        + If  it  is  `.'  or  a  `;'  indentation is set to previous expression
        indentation.

        + If  it  is  `)'  or  `]'  indentation  is  set to beginning expression
        indentation more the absolute value of `indent_in_statement'.

        + Else  the  character  line  indentation  more  the  absolute  value of
        `indent_in_statement'.

    * At top level it all depends on the last previous character (jumping space,
    `tab, newline' and comments):
        
        + If it is `}' indentation is set to 0.

        + If  it  is  `)'  indentation is set to `arg_decl_indent' because it is
        probably the parameters of a function write with the old syntax.

        + If it is `;' indentation is set to previous expression indentation.
        
        + Else,  in  an  expression  indentation is set to the absolute value of
        `indent_in_statement', else 0.
        

   The  indentation  rules  when you indent a non empty line (for instance when
   you reindent  a line with a `tab') depends on the first character(s) (jumping
   space, `tab, newline' and comments) :
    
    * If it is `#' indentation is set to 0

    * If  it  is  `*'  and  the  next  character  is `/', so you close a comment,
    indentation is set to indentation of the `/*'

    * If it is a `{' out of a block, indentation is 0

    * Else in a block:

        + If  it  is  `}' indentation is set to indentation of the corresponding
        `{' or of the expression preceding it.

        + If  it  is `case' or `default' indentation is set to the corresponding
        `switch' indentation.

        + If  it  is  `:' indentation is set to indentation of the corresponding
        `?'.
*/
							      
/* 
   ============
   default mode
   ============
   
   Key bindings :

    * `escape  f'  go  to  the  end of the current or next word. A word contains
    alphabetic  characters,  digits  and  underscore. Made by the `forward_word'
    function.
    
    * `escape  b'  go  to the beginning of the current or previous word. Made by
    the `backward_word' function.

    * `escape  d'  deletes the end of the current word or the next word. Made by
    the `delete_next_word' function.
    
    * `escape  delete'  and  `escape  backspace'  deletes  the  beginning of the
    current  word  or  the  previous  word.  Made  by the `delete_previous_word'
    function.
*/

/* You also can use the following functions for any purpose :
   
   for a c/c++ text :
   
        void goto_beginning_of_c_definition()
	int beginning_of_c_definition
        void forward_c_form()
        void backward_c_form()
        void delete_next_c_form()
        void delete_previous_c_form()
   
   for any text :
   
        void forward_word()
        void backward_word()
        void delete_next_word()
        void delete_previous_word()
*/   

{
  /* C-mode */
  
  key_def("C-mode", "\t", "c_indent_line");
  key_def("C-mode", "\r", "c_insert_and_indent");

  key_def("C-mode", "}", "c_insert_blink_matched_char_and_indent");
  key_def("C-mode", "]", "c_insert_blink_matched_char_and_indent");
  key_def("C-mode", ")", "c_insert_blink_matched_char_and_indent");

  key_def("C-mode", "{", "c_insert_and_indent");
  key_def("C-mode", ":", "c_insert_and_indent");

  key_def("C-mode", "^[a", "goto_beginning_of_c_definition");
  key_def("C-mode", "^[f", "forward_c_form");
  key_def("C-mode", "^[b", "backward_c_form");

  key_def("C-mode", "^[d", "delete_next_c_form");
  key_def("C-mode", "^[\b", "delete_previous_c_form");
  key_def("C-mode", "^[\177", "delete_previous_c_form");      /* esc delete */

  key_def("C-mode", "^[i", "c_indent_region");
  
  key_def("C-mode", "^[u", "upcase_word");
  key_def("C-mode", "^[l", "downcase_word");
  key_def("C-mode", "^[c", "capitalize_word");

  set_mode_suffixes ("C-mode", ".h .c .C .H .sc" );
  set_mode_font ("C-mode", "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1");
  
  /* C++mode */
  
  key_def("C++mode", "\t", "c_indent_line");
  key_def("C++mode", "\r", "c_insert_and_indent");

  key_def("C++mode", "}", "c_insert_blink_matched_char_and_indent");
  key_def("C++mode", "]", "c_insert_blink_matched_char_and_indent");
  key_def("C++mode", ")", "c_insert_blink_matched_char_and_indent");

  key_def("C++mode", "{", "c_insert_and_indent");
  key_def("C++mode", ":", "c_insert_and_indent");

  key_def("C++mode", "^[a", "goto_beginning_of_c_definition");
  key_def("C++mode", "^[f", "forward_c_form");
  key_def("C++mode", "^[b", "backward_c_form");

  key_def("C++mode", "^[d", "delete_next_c_form");
  key_def("C++mode", "^[\b", "delete_previous_c_form");
  key_def("C++mode", "^[\177", "delete_previous_c_form");      /* esc delete */

  key_def("C++mode", "^[i", "c_indent_region");
  
  key_def("C++mode", "^[u", "upcase_word");
  key_def("C++mode", "^[l", "downcase_word");
  key_def("C++mode", "^[c", "capitalize_word");

  set_mode_font ("C++mode", "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1");
}


/*
  Functions and variables for user and editor :
*/

/* indentation variables */

int c_indent_in_comment = 3;
int c_indent_in_string = 0;
int c_indent_in_parenthesis = 1;
int c_indent_in_bracket = 1;
int c_indent_in_brace = 2;
int c_indent_in_statement = 2;
int c_arg_decl_indent = 4;

/* Go to the beginning of the previous or current function,
   strings and comments are not taken into account */

void goto_beginning_of_c_definition()
{
  int c_at_beginning_of_definition();
  
  while ((goto_beginning_of_line(), ! c_at_beginning_of_definition()))
    goto_previous_char();
}

int * c_memo_frames_beginning;
int * c_memo_frames_end;
int * c_memo_frames_level;
int c_search_memo_first_index;
int c_search_memo_last_index;
int c_memo_area_beginning;
int c_frames_number;
int c_max_level;

int c_current_level;
int * c_column_of_level;
int * c_frame_of_level;

void c_memo_frames(int last_pos);
void c_free_frames();
int c_reindent_line();
int c_index_memo_frame(int);
void c_jump_spaces_forward();
void c_jump_spaces_empty_lines_comments_forward();

/* reindent the current line */

void c_indent_line()
{
  int pos = current_position();
  int blgn;
  int elgn;

  goto_end_of_line();
  elgn = current_position();
  
  goto_beginning_of_line();
  blgn = current_position();

  /* memorize frames */
  
  goto_previous_char();
  goto_beginning_of_c_definition();
  c_memo_area_beginning = current_position();

  c_memo_frames(elgn);

  goto_char(blgn);

  /* indent and place cursor */

  pos += c_reindent_line();

  goto_char(blgn);
  c_jump_spaces_forward();
  if (pos > current_position())
    goto_char(pos);

  c_free_frames();
}

/* indent current region */

int c_is_alone_on_the_line();

void c_indent_region()
{
  int beginning = mark_position();
  int end = current_position();
  int first_index;
  int last_index;
  int sigma_incr = 0;
  int pos;

  watch_on();

  if (end < beginning) {
    end = beginning;
    beginning = current_position();
  }

  goto_char(beginning);
  pos = beginning_of_line();

  /* memorize frames */

  goto_char(pos);
  goto_previous_char();
  goto_beginning_of_c_definition();
  c_memo_area_beginning = current_position();

  c_memo_frames(end);
  goto_char(pos);

  /* go to the first function definition */
  
  for (first_index = -1;
       (++first_index != c_frames_number) &&
       (c_memo_frames_end[first_index] < pos); )
    ;

  for (last_index = first_index;
       last_index != c_frames_number;
       last_index += 1)
    if ((! c_memo_frames_level[last_index]) &&
	(the_char(c_memo_frames_end[last_index]) == '}')) {
      goto_char(c_memo_frames_end[last_index]);
      if (c_is_alone_on_the_line()) {
	last_index += 1;
	break;
      }
    }
  c_search_memo_last_index = last_index;
  goto_char(pos);
  
  /* indentation */
  
  while ((pos = current_position()) < end) {
    int incr = c_reindent_line();
 
    /* update frames table */

    if (incr) {
      int index = last_index;

      sigma_incr += incr;

      while ((index-- != first_index) &&
	     (c_memo_frames_end[index] >= pos)) {
	c_memo_frames_end[index] += incr;
	if (c_memo_frames_beginning[index] >= pos)
	  c_memo_frames_beginning[index] += incr;
      }
      
      end += incr;
    }

    goto_end_of_line();
    goto_next_char();

    if (pos == c_memo_frames_end[last_index - 1]) {
      
      /* forget current managed definition and update of next function frames */
	
      c_memo_area_beginning = current_position();
      c_search_memo_first_index = first_index = last_index;

      for (last_index = first_index;
	   last_index != c_frames_number;
	   last_index += 1) {
	c_memo_frames_beginning[last_index] += sigma_incr;
	c_memo_frames_end[last_index] += sigma_incr;
	if ((! c_memo_frames_level[last_index]) &&
	    (the_char(c_memo_frames_end[last_index]) == '}')) {
	  goto_char(c_memo_frames_end[last_index]);
	  if (c_is_alone_on_the_line()) {
	    last_index += 1;
	    break;
	  }
	}
      }
      c_search_memo_last_index = last_index;
      goto_char(c_memo_area_beginning);
    }
  }

  c_free_frames();
  goto_char(end);
  
  watch_off();
}


/* Add last input character and indent (for instance for
   <return> '(' and '{') if nothing before on the line */

int c_nothing_before()
{
  if (current_position() != beginning_of_line()) {
    int p = current_position() - 1;
  
    for (goto_beginning_of_line(); current_position() != p; goto_next_char()) {
      if (! strchr(" \t", current_char())) {
	goto_char(p + 1);
	return 0;
      }
    }
    goto_next_char();
  }

  return 1;
}

int c_next_word_is(char *);

void c_insert_and_indent()
{
  char c = last_key();

  insert_char((c == '\r') ? '\n' : c);
  if (c_nothing_before())
    c_indent_line();
  else if (c == ':') {
    int p = current_position();
    
    goto_beginning_of_line();
    c_jump_spaces_forward();
    if (c_next_word_is("case") || c_next_word_is("default") ||
	c_next_word_is("protected") || c_next_word_is("private") ||
	c_next_word_is("public")) {
      goto_char(p);
      c_indent_line();
    }
    else
      goto_char(p);
  }
}


/* Inserts the last input character, if it is ) ] or } blink the corresponding ( [ or {.
   No effect in a comment or a string. */

int c_corresponding_frame_beginning(int);

void c_insert_blink_matched_char_and_indent()
{
  insert_char(last_key());

  if (strchr(")}]", previous_char())) {
    int pos = current_position();
    int blpos = -1;

    goto_beginning_of_c_definition();
    c_memo_area_beginning = current_position();
    c_memo_frames(pos);
    
    if ((blpos = c_corresponding_frame_beginning(pos - 1)) == (pos - 1))
      blpos = -1;

    goto_char(pos - 1);
    blink(blpos);
    goto_next_char();
    
    if (c_nothing_before()) {
      goto_beginning_of_line();
      goto_char(pos + c_reindent_line());
    }
    
    c_free_frames();
  }
}


/* go next or back an expression */

void c_treat_char(int);
char c_search_previous_char_jump_space_comment(int);
int beginning_of_c_definition();

char * chars_of_word =
  "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

void forward_c_form()
{
  for (;;) {
    c_jump_spaces_empty_lines_comments_forward();
    switch (current_char()) {
    case '{':
    case '(':
    case '[':
      c_memo_frames_beginning = 0;
      c_treat_char(end_of_file());
      goto_next_char();
      return;
      
    case '"':
      goto_next_char();
      while (msearch("\"\\\n", end_of_file(), 1))
	switch (current_char()) {
	case '"':
	case '\n':			/* illegal string */
	  goto_next_char();
	  return;
	case '\\':
	  goto_next_char();
	  goto_next_char();
      }
      return;
      
    default:
      if (! strchr(chars_of_word, current_char()))
	goto_next_char();
      else {
	while (current_char() && strchr(chars_of_word, current_char()))
	  goto_next_char();
	return;
      }
    }
  }
}

void backward_word();

void backward_c_form()
{
  int pos = current_position();
  int beginning = beginning_of_c_definition();

  /* to increase performances of the simple backward_word case */
  while ((current_position() != beginning) &&
	 (strchr(" \t", current_char())))
    goto_previous_char();
  
  if (strchr(chars_of_word, current_char())) {
    backward_word();
    return;
  }

  /* expression */
  
  goto_char(beginning);
  c_memo_area_beginning = current_position();
  c_memo_frames(pos);
  goto_char(pos);
  
  while (current_position() != beginning) {
    switch (c_search_previous_char_jump_space_comment(beginning)) {
    case '}':
    case ')':
    case ']':
    case '"':
      goto_char(c_corresponding_frame_beginning(current_position()));
      c_free_frames();
      return;
      
    default:
      if (strchr(chars_of_word, current_char())) {
	backward_word();
	c_free_frames();
	return;
      }
    }
  }
}

void delete_next_c_form()
{
  int pos = current_position();
  int ndel;
  
  forward_c_form();
  ndel = current_position() - pos;
  
  goto_char(pos);
  while (ndel--) delete_char();
}

void delete_previous_c_form()
{
  int pos = current_position();
  int ndel;

  backward_c_form();
  ndel = pos - current_position();
       
  while (ndel--) delete_char();
}


/*
  Implementation :
*/

int c_at_beginning_of_definition()
{
  /* must be at the beginning of line */
  
  return ((! current_position()) ||
	  ((! strchr("#\t\n }", current_char())) &&
	   ((current_char() != '/') || (! strchr("/*", next_char()))) &&
	   ((current_char() != '*') || (next_char() != '/')) &&
	   (the_char(current_position() - 2) != '\\')));
}

int beginning_of_c_definition()
{
  int pos = current_position();
  int result;
  
  goto_beginning_of_c_definition();
  result = current_position();
  goto_char(pos);

  return result;
}

int c_column(int);
int c_indentation(int);
int c_indentation_switch(int indent);
int c_indentation_class(int indent);
int c_compute_indentation(int * type, int * level);
int c_open_frame_line_indentation(int frame_beginning, int level, int close);
char c_jump_string_frame_backward(char *);
int c_indent_if_expr(int);

/* Indent, returns the count of added characters. Frames must be already memorized.
   Must be at the beginning of line */

int c_reindent_line()
{
  int pos = current_position();
  int type;
  int level;
  /* Indent, line beginning is not taken into account */

  int indent = c_compute_indentation(&type, &level);

  /* take into account the line beginning to correct indentation */

  c_jump_spaces_forward();

  if (type >= 0)
    /* in a body */
    switch (current_char()) {

    case '}' :
      indent = c_open_frame_line_indentation(type, level, 1);
      break;
	
    case 'c' :
      if (c_next_word_is("case")) {
	/* go to the switch { */
	goto_char(type);
	indent = c_indentation_switch(indent);
      }
      break;
	
    case 'd' :
      if (c_next_word_is("default")) {
	/* go to the switch { */
	goto_char(type);
	indent = c_indentation_switch(indent);
      }
      break;
	
    case 'p' :
      if (c_next_word_is("protected") || c_next_word_is("private") ||
	  c_next_word_is("public")) {
	/* go to the class { */
	goto_char(type);
	indent = c_indentation_class(indent);
      }
      break;
    case ':' :
      indent = c_indent_if_expr(indent);
      break;

    case '#' :
      indent = 0;
    }

  else if (type == -1) {
    /* commentaire */
    if ((current_char() == '*') && (next_char() == '/')) {
      /* indent the comment end as its beginning */
      int pos = c_corresponding_frame_beginning(current_position() + 1);

      if (pos != (current_position() + 1))
	indent = c_indentation(pos);
    }
  }
  
  else if ((type == -3) || (type == -4)) {
    /* [ or ( */
    switch (current_char()) {
    case '#' :
      indent = 0;
      break;
      
    case ':' :
      indent = c_indent_if_expr(indent);
    }
  }
  
  else if ((type == -5) && current_char() && (strchr("{#", current_char())))
    /* a first { */
    indent = 0;

  goto_char(pos);

  /* correct indentation */

  {
    int correction = 0;

    if (c_indentation(current_position()) == indent)
      return 0;

    /* add necessary tab and spaces */
    
    for (; indent >= 8; indent -= 8) /* les tab */
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
    
    while (indent--)		/* spaces */
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
}

      
int c_indent_if_expr(int indent)
{
  int if_number = 0;
  char c;
  
/* search the ?, the only one possibility to find :
   is linked with other ?: before the good ? */
  
  while (c = c_jump_string_frame_backward(":?{}([")) {
    if (c == ':') {
      if (previous_char() == ':')
	goto_previous_char();
      else
	if_number += 1;
    }
    else if (c == '?') {
      if (! if_number--) {
	indent = c_column(current_position());
	break;
      }
    }
    else /* {}([ */
      /* probably a syntax error */
      break;
  }
  
  return indent;
}

int c_indentation_switch(int indent)
{
  /* current position is the switch { */
  int c;
  
  while ((c = c_jump_string_frame_backward("s{}([")) && (c == 's')) {
    if (c_next_word_is("switch"))
      return c_column(current_position());
  }

  /* {}([ and file beginning, probably a syntax error */

  return indent;
}

int c_indentation_class(int indent)
{
  /* current position is the class { */
  int c;
  
  while ((c = c_jump_string_frame_backward("c{}([")) && (c == 'c')) {
    if (c_next_word_is("class"))
      return c_column(current_position()) + c_indent_in_statement;
  }

  /* {}([ and file beginning, probably a syntax error */

  return indent;
}

void c_jump_char_backward()
{
  /* current_position is the last ' */
  
  goto_previous_char();
  goto_previous_char();
  if (current_char() != '\'')
    goto_previous_char();
}

char c_jump_string_frame_backward(char * searched)
{
  char c;
  char str[16];

  strcpy(str, "\"])'/\n");
  strcpy(str + 6, searched);

  while (msearch(str, c_memo_area_beginning, -1)) {
    if (strchr(searched, c = current_char()))
      return c;
    if (c == '\'')
      c_jump_char_backward();
    else
      /* "]) and perhaps end of comment */
      goto_char(c_corresponding_frame_beginning(current_position()));
  }

  return (strchr(searched, c = current_char())) ? c : 0;
}


int c_next_word_is(char * word)
{
  int pos = current_position();
  int c;

  if (pos && strchr(chars_of_word, previous_char()))
    return 0;
  
  while (*word) {
    if (current_char() != *word++) {
      goto_char(pos);
      return 0;
    }
    else
      goto_next_char();
  }

  c = current_char();
  goto_char(pos);

  return (! strchr(chars_of_word, c));
}

/* Return the most included frame beginning from the
   current_position to the indicated position */

void c_jump_to(int, char *);
void c_jump_string(int);
void c_jump_comment(int);
void c_jump_comment_cpp(int);

int c_memo_size;
int c_frame_level;

void c_memo_frames(int last_pos)
{
  int pos = current_position();

  c_memo_size = 128;
  c_frames_number = c_search_memo_first_index = 0;
  c_frame_level = c_max_level = 0;
  
  if (! (c_memo_frames_beginning = (int*) calloc(c_memo_size, sizeof(int))))
    error("not enougth memory");
  if (! (c_memo_frames_end = (int*) calloc(c_memo_size, sizeof(int)))) {
    free(c_memo_frames_beginning);
    error("not enougth memory");
  }
  if (! (c_memo_frames_level = (int*) calloc(c_memo_size, sizeof(int)))) {
    free(c_memo_frames_end);
    free(c_memo_frames_beginning);
    error("not enougth memory");
  }

  while (msearch("\"([{/'#", last_pos, 1)) {
    c_treat_char(last_pos);
    goto_next_char();
  }

  if (! (c_column_of_level = (int *) calloc(c_max_level + 1, sizeof(int)))) {
    free(c_memo_frames_end);
    free(c_memo_frames_beginning);
    free(c_memo_frames_level);
    error("not enougth memory");
  }
  if (! (c_frame_of_level = (int *) calloc(c_max_level + 1, sizeof(int)))) {
    free(c_memo_frames_end);
    free(c_memo_frames_beginning);
    free(c_memo_frames_level);
    free(c_column_of_level);
    error("not enougth memory");
  }
  
  c_current_level = -1;
  c_search_memo_last_index = c_frames_number;
  goto_char(pos);
}

void c_duplicate(int ** src, int anc_taille, int new_taille)
{
  int * res;
  
  if (! (res = (int*) calloc(new_taille, sizeof(int)))) {
    c_free_frames();
    error("not enougth memory");
  }
  memcpy((char*)res, (char*)*src, anc_taille * sizeof(int));
  free(*src);
  *src = res;
}

void c_memo_new_frame(int beginning, int end, int level)
{
  if (! c_memo_frames_beginning)
    /* forward_form */
    return;
  
  if (c_frames_number == c_memo_size) {
    c_duplicate(&c_memo_frames_beginning, c_memo_size, c_memo_size * 2);
    c_duplicate(&c_memo_frames_end, c_memo_size, c_memo_size * 2);
    c_duplicate(&c_memo_frames_level, c_memo_size, c_memo_size * 2);
    c_memo_size = 2 * c_memo_size;
  }
  
  c_memo_frames_beginning[c_frames_number] = beginning;
  c_memo_frames_end[c_frames_number] = end;
  c_memo_frames_level[c_frames_number++] = level;

  if (level > c_max_level) c_max_level = level;
}  

void c_treat_char(int last_pos)
{
  switch (current_char()) {
  case '\"' :
    c_jump_string(last_pos);
    break;
  case '(' :
    c_jump_to(last_pos, ")\"([{/'#");
    break;
  case '[' :
    c_jump_to(last_pos, "]\"([{/'#");
    break;
  case '{' :
    c_jump_to(last_pos, "}\"([{/'#");
    break;
  case '/' :
    switch (next_char()) {
    case '*' :
      c_jump_comment(last_pos);
      break;
    case '/' :
      c_jump_comment_cpp(last_pos);
      break;
    }
    break;

  case '\'' :
    goto_char(current_position() + ((next_char() == '\\') ? 3 : 2));
    break;

  case '#' :
    /* does not check if the column is the first */
    c_jump_comment_cpp(last_pos);
  }
}
				   
void c_jump_to(int last_pos, char * searched)
{
  int pos = current_position();
				   
  goto_next_char();
  c_frame_level += 1;
				   
  while(msearch(searched, last_pos, 1)) {
    if (current_char() == *searched) {
      c_memo_new_frame(pos, current_position(), --c_frame_level);
      return;
    }
    c_treat_char(last_pos);
    goto_next_char();
  }
  
  c_memo_new_frame(pos, last_pos, --c_frame_level);
  return;
}

void c_jump_comment(int last_pos)
{
  /* current_position is the / */
  int pos = current_position();

  goto_char(pos + 2);

  while (msearch("*", last_pos, 1))
    if (next_char() == '/') {
      goto_next_char();		/* an other goto_next_char() will be called */
      c_memo_new_frame(pos, current_position(), c_frame_level);
      return;
    }
    else
      goto_next_char();
      
  c_memo_new_frame(pos, last_pos, c_frame_level);
}

void c_jump_comment_cpp(int last_pos)
{
  /* # and // */
  int pos = current_position();

  goto_end_of_line();
  
  if (current_position() >= last_pos) {
    c_memo_new_frame(pos, last_pos, c_frame_level);
  }
  else
    c_memo_new_frame(pos, current_position(), c_frame_level);
}


void c_jump_string(int last_pos)
{
  /* current_position is the " */
  int pos = current_position();

  goto_next_char();

  while (msearch("\\\"", last_pos, 1)) {
    if (current_char() == '\"') {
      c_memo_new_frame(pos, current_position(), c_frame_level);
      return;
    }
    goto_next_char();
    goto_next_char();
  }

  c_memo_new_frame(pos, last_pos, c_frame_level);
}

/* Frame management.
   Frames are memorized with their end growing up, contrary with their beginning.
   Several beginnings have the same end when there is no corresponding frame end. */

/* Returns the end of the frame which begin in point.
   The frame is terminated when the program is correct, search by dichotomie */

int c_corresponding_frame_beginning(int pos)
{
  int index;

  return ((index = c_index_memo_frame(pos)) >= 0)
      ? c_memo_frames_beginning[index]
      : pos;
}

int c_index_memo_frame(int pos)
{
  int inf = c_search_memo_first_index;
  int sup = c_search_memo_last_index - 1;
  int middle;
  int val;

  if (inf > sup)
    return -1;

  while ((inf + 1) < sup) {
    val = c_memo_frames_end[middle = (inf + sup)/2];
    if (val < pos)
      inf = middle;
    else if (val != pos)
      sup = middle;
    else
      return middle;
  }

  return (c_memo_frames_end[inf] == pos)
         ? inf
         : (c_memo_frames_end[sup] == pos)
	   ? sup
	   : -1;
}


/* Returns the beginning of the smallest frame which contains point.
   It is impossible to use dichotomie, but the first valid frame is the
   right frame */

int c_encompassed_frame_beginning(int pos, int * level)
{
  int index;
  
  for (index = c_search_memo_first_index;
       (index != c_search_memo_last_index) && (c_memo_frames_end[index] < pos);
       index += 1)
    ;
  
  while (index != c_search_memo_last_index)
    if (c_memo_frames_beginning[index] < pos) {
      *level = c_memo_frames_level[index];
      return c_memo_frames_beginning[index];
    }
    else
      index += 1;

  *level = -1;
  return pos;
}


/* Returns the end of the frame corresponding with beginning,
   cannot search by dichotomie */

int c_corresponding_frame_end(int beginning)
{
  int index;

  for (index = c_search_memo_first_index;
       index != c_search_memo_last_index;
       index += 1)
    if (c_memo_frames_beginning[index] == beginning)
      return c_memo_frames_end[index];

  return beginning;
}

void c_free_frames()
{
  free(c_memo_frames_beginning);
  free(c_memo_frames_end);
  free(c_memo_frames_level);
  free(c_column_of_level);
  free(c_frame_of_level);
}

/* compute the line indentation without taken into account its contents */

int c_indent_in_a_comment(int frame_beginning, int point);
int c_indent_in_a_string(int frame_beginning, int point);
int c_indent_in_parenthese(int frame_beginning, int point);
int c_indent_in_a_bracket(int frame_beginning, int point);
int c_indent_in_a_block(int frame_beginning, int point, int level);
int c_indent_out_of_block(int point);

int c_compute_indentation(int * type, int * level)
{
  int point = current_position();
  int pos;
  int frame_beginning;
  
  frame_beginning = c_encompassed_frame_beginning(point, level);

  if (frame_beginning != point) {
    switch (the_char(frame_beginning)) {
    case '/' :
      *type = -1;
      pos = c_indent_in_a_comment(frame_beginning, point);
      break;
    case '"' :
      *type = -2;
      pos = c_indent_in_a_string(frame_beginning, point);
      break;
    case '{' :
      *type = frame_beginning;
      pos = c_indent_in_a_block(frame_beginning, point, *level);
      break;
    case '[' :
      *type = -3;
      pos = c_indent_in_a_bracket(frame_beginning, point);
      break;
    default :	 /* '(' */
      *type = -4;
      pos = c_indent_in_parenthese(frame_beginning, point);
    }
  }
  else {
    pos = c_indent_out_of_block(point);
    *type = -5;
  }

  goto_char(point);
  return pos;
}

int c_indentation(int point)
     /* returns the indentation of the line which contain point */
{
  int here = current_position();
  int indent = 0;

  goto_char(point);
  goto_beginning_of_line();

  for (;;) {
    switch (current_char()) {
    case ' ' :
      indent += 1;
      break;
    case '\t' :
      indent = (indent & ~7) + 8;
      break;
    default:
      goto_char(here);
      return indent;
    }
    goto_next_char();
  }
}

int c_column(int point)
     /* returns the visual position of point */
{
  int here = current_position();
  int indent = 0;

  goto_char(point);

  for (goto_beginning_of_line(); current_position() != point; goto_next_char())
    if (current_char() == '\t')
      indent = (indent & ~7) + 8;
    else
      /* not obligatory a space */
      indent += 1;

  goto_char(here);

  return indent;
}

void c_jump_spaces_forward()
{
  while (current_char() && strchr(" \t", current_char()))
    goto_next_char();
}

void c_jump_spaces_empty_lines_comments_forward()
{
  for (;;) {
    switch (current_char()) {
    case ' ':
    case '\t':
    case '\n':
      goto_next_char();
      break;
    case '/':
      if (next_char() == '*') {
	goto_next_char();
	do
	  goto_next_char();
        while (current_char() &&
	       ((current_char() != '*') || (next_char() != '/')));
	goto_next_char();
	goto_next_char();
      }
      else if (next_char() == '/')
	goto_end_of_line();
      else
	return;
      break;
    case '#':
      goto_end_of_line();
      break;
    default:
      return;
    }
  }
}

int abs(int x) { return (x >= 0) ? x : -x; }
     
int c_indent_in_a_comment(int frame_beginning, int point)	/* cannot be // */
{
  /* find previous line indentation */
  for(;;) {
    int pos;
    
    goto_previous_char();
    goto_beginning_of_line();
    pos = current_position();
    c_jump_spaces_forward();
    if (current_char() == '\n')
      /* empty line */
      goto_char(pos);
    else {
      int result = (current_position() <= frame_beginning)
	? /* it is the second comment's line */
	  c_column(frame_beginning) + abs(c_indent_in_comment)
	: /* it is a least the third comment's line,
	     use the previous line indentation */
	  c_column(current_position());
      
      goto_char(point);
      return result;
    }
  }
}

int c_indent_in_a_string(int frame_beginning, int point)
{
  goto_char(point);

  return (c_indent_in_string < 0)
    ? c_column(frame_beginning) - c_indent_in_string
    : c_indent_in_string;
}

int c_indent_in_a_bracket(int frame_beginning, int point)
{
  goto_char(point);

  return c_column(frame_beginning) + abs(c_indent_in_bracket);
}
  
int c_indent_in_parenthese(int frame_beginning, int point)
{
  goto_char(point);

  return c_column(frame_beginning) + abs(c_indent_in_parenthesis);
}

char c_search_previous_char_jump_space_comment(int beginning)
     /* not in a string or a comment, go to the char in ch
	and returns it, jump spaces and comments */
{
  char c;
  
  while (current_position() > beginning) {
    goto_previous_char();
    switch (c = current_char()) {
    case '/' :
      if (previous_char() == '*') {
	int p = c_corresponding_frame_beginning(current_position());

	if (p != current_position())
	  goto_char(p);
	else {
	  /* the definition begining is in fact a comment because of its indentation */
	  goto_char(beginning);
	  return 0;
	}
      }
      break;
    case '\n' :			/* perhaps a comment end by // */
      goto_char(c_corresponding_frame_beginning(current_position()));
      break;
    case ' ' :
    case '\t' :
      break;
    case '\\':
      if (next_char() == '\n')
	break;
    default:
      return c;
    }
  }
  
  return 0;
}

char c_search_previous_char_jump_space_comment_OLD(int beginning)
     /* not in a string or a comment, go to the char in ch
	and returns it, jump spaces and comments */
{
  char c;
  
  while (current_position() > beginning) {
    goto_previous_char();
    switch (c = current_char()) {
    case '/' :
      if (previous_char() == '*') {
	int p = c_corresponding_frame_beginning(current_position());

	if (p != current_position())
	  goto_char(p);
	else {
	  /* the definition begining is in fact a comment because of its indentation */
	  goto_char(beginning);
	  return 0;
	}
      }
      break;
    case '\n' :			/* perhaps a comment end by // */
      goto_char(c_corresponding_frame_beginning(current_position()));
      break;
    case ' ' :
    case '\t' :
      break;
    default:
      return c;
    }
  }
  
  return 0;
}


int c_is_alone_on_the_line()
{
  int pos = current_position();
  char c;

  /* anything before ? */

  for (; current_position();) {
    goto_previous_char();
    if (! strchr(" \t", c = current_char()))
      /* perhaps anything */
      if (c == '\n')
	break;
      else if ((c == '/') && (previous_char() == '*')) {
	int p = c_corresponding_frame_beginning(current_position());

	if ((p == current_position()) || (p < beginning_of_line()))
	  /* comment beginning is unknown or the comment is
	     on more than one line suppose it is alone ! */
	  break;
	else
	  goto_char(p);
      }
      else {
	goto_char(pos);
	return 0;
      }
  }

  /* anything after ? */

  goto_char(pos);

  for (;;) {
    goto_next_char();
    if (! strchr(" \t", c = current_char()))
      /* perhaps anything */
      if (c == '\n') {
	goto_char(pos);
	return 1;
      }
      else if (c == '/')
	if (next_char() == '/') {
	  goto_char(pos);
	  return 1;
	}
	else if (next_char() == '*') {
	  int p = c_corresponding_frame_end(current_position());

	  if ((p == current_position()) || (p > end_of_line())) {
	    /* comment end is unknown or the comment is
	       on more than one line suppose it is alone ! */
	    goto_char(pos);
	    return 1;
	  }
	  else
	    goto_char(p);
	}
	else {
	  goto_char(pos);
	  return 0;
	}
      else {
	goto_char(pos);
	return 0;
      }
    else if (! c) {
      goto_char(pos);
      return 1;
    }
  }
}


/* go to the first character of the previous
   expression and return its position */

int c_corresponding_expression_beginning_position(char * separateurs,
						  char * passep)
{
  char tsep[16];
  char c;

  {
    char * p;
    char * q = tsep;

    for (p = ";,:\")]'\n/"; *p; p += 1)
      if (! strchr(passep, *p))
	*q++ = *p;
    *q = 0;
    for (p = separateurs; *p; p += 1)
      if (! strchr(tsep, *p)) {
	*q++ = *p;
	*q = 0;
      }
  }

  while (msearch(tsep, c_memo_area_beginning, -1))
    if ((strchr(separateurs, c = current_char())) &&
	((! strchr("})]", c)) || c_is_alone_on_the_line())) {
      goto_next_char();
      break;
    }
    else if (strchr(";,", c)) {
      /* the `,' is linked with struct or array initialization */
      goto_next_char();
      break;
    }
    else if (c == ':') {
      int pos = current_position();
      
      while (c = c_jump_string_frame_backward("cdp?{}([;")) {
	if (c == 'c') {
	  if (c_next_word_is("case")) {
	    goto_char(pos + 1);
	    pos = 0;
	    break;
	  }
	  goto_previous_char();
	}
	else if (c == 'd') {
	  if (c_next_word_is("default")) {
	    goto_char(pos + 1);
	    pos = 0;
	    break;
	  }
	  goto_previous_char();
	}
	else if (c == 'p') {
	  if (c_next_word_is("protected") ||
	      c_next_word_is("private") ||
	      c_next_word_is("public")) {
	    goto_char(pos + 1);
	    pos = 0;
	    break;
	  }
	  goto_previous_char();
	}
	else {
	  /* ?{}([; */
	  goto_char(pos - 1);
	  pos = 1;
	  break;
	}
      }
      if (! pos) break;
    }
    else if (c == '\'')
      c_jump_char_backward();
    else if (c == '/') {
       if (previous_char() == '*') {
	int p = c_corresponding_frame_beginning(current_position());

	if (p != current_position())
	  goto_char(p);
	else {
	  /* the definition begining is in fact a
	     comment because of its indentation */
	  goto_char(c_memo_area_beginning);
	  break;
	}
      }
    }
    else if (c == '\n') {	/* perhaps a // comment end */
      int p = c_corresponding_frame_beginning(current_position());

      if (p != current_position())
	goto_char(p);
    }
    else { /* }")] */
      int p = c_corresponding_frame_beginning(current_position());

      if (p != current_position())
	goto_char(p);
      else
	break;
    }

  c_jump_spaces_empty_lines_comments_forward();
  return current_position();
}

int c_open_frame_line_indentation(int frame_beginning, int level, int close)
{
  if (! level)
    c_current_level = level;
  else if (level <= c_current_level) {
    int encomplevel;
    int envengl =
      c_encompassed_frame_beginning(frame_beginning - 1, &encomplevel);

    while (c_current_level != level) {
      c_frame_of_level[c_current_level] =
	  c_column_of_level[c_current_level] = 0;
      c_current_level -= 1;
    }
    if ((! close) &&
	(c_frame_of_level[encomplevel] == envengl) &&
	c_column_of_level[encomplevel]) {
      c_frame_of_level[level] = frame_beginning;
      return c_column_of_level[level] = c_column_of_level[encomplevel];
    }
  }
  else
    while (++c_current_level != level)
      c_frame_of_level[c_current_level] =
	  c_column_of_level[c_current_level] = 0;
  
  goto_char(frame_beginning);
  goto_beginning_of_line();
  c_jump_spaces_forward();
  
  {
    int previous_expression_beginning;

    if (current_position() != frame_beginning) {
      /* anything before */
      goto_char(frame_beginning);
      previous_expression_beginning = 
	c_column(c_corresponding_expression_beginning_position("{}", ""));
    }
    else {
      previous_expression_beginning = c_column(frame_beginning);
      if ((! close) &&
	  c_search_previous_char_jump_space_comment(c_memo_area_beginning) == ')') {
	goto_char(c_corresponding_frame_beginning(current_position()));
	c_search_previous_char_jump_space_comment(c_memo_area_beginning);
	if (strchr("refoh", current_char())) {
	  backward_word();
	  if (c_next_word_is("for") ||
	      c_next_word_is("while") ||
	      c_next_word_is("if") ||
	      c_next_word_is("else") ||
	      c_next_word_is("do") ||
	      c_next_word_is("switch")) {
	    c_column_of_level[level] = c_column(frame_beginning);
	    return c_column(current_position());
	  }
	}
      }
    }
    c_frame_of_level[level] = frame_beginning;
    return c_column_of_level[level] = previous_expression_beginning;
  }
}

int c_indent_in_a_block(int frame_beginning, int point, int level)
{
  int pos;

  switch (c_search_previous_char_jump_space_comment(frame_beginning)) {
  case 0 :
  case '{' :
    /* it is the block beginning */
    c_open_frame_line_indentation(frame_beginning, level, 0);
    goto_char(point);
    return c_column_of_level[level] += abs(c_indent_in_brace);

  case ';' :
    if (c_column_of_level[level] &&
	(c_frame_of_level[level] == frame_beginning)) {
      goto_char(point);
      return c_column_of_level[level];
    }
    if (level < c_current_level) {
      do
	c_frame_of_level[c_current_level] =
	    c_column_of_level[c_current_level] = 0;
      while (--c_current_level != level);
      if (c_column_of_level[level])
	return c_column_of_level[level];
    }
    else
      while (c_current_level != level) {
	c_current_level += 1;
	c_frame_of_level[c_current_level] =
	    c_column_of_level[c_current_level] = 0;
      }
    pos = c_corresponding_expression_beginning_position("{}", ",");
    goto_char(point);
    c_frame_of_level[level] = frame_beginning;
    return c_column_of_level[c_current_level] = c_column(pos);
    
  case ',' :
    pos = c_corresponding_expression_beginning_position("{}[(", "");
    goto_char(point);
    return c_column(pos);

  case '}' :
    if ((pos = c_index_memo_frame(current_position())) >= 0) {
      pos = c_open_frame_line_indentation(c_memo_frames_beginning[pos],
					  c_memo_frames_level[pos], 0);
      c_frame_of_level[level + 1] = c_column_of_level[level + 1] = 0;
      goto_char(point);
      c_frame_of_level[c_current_level = level] = frame_beginning;
      return c_column_of_level[level] = pos;
    }
    c_frame_of_level[c_current_level] =
	c_column_of_level[c_current_level] = 0;
    break;			/* no corresponding { */

  case ')' :
    goto_char(c_corresponding_frame_beginning(current_position()));
    goto_beginning_of_line();
    c_jump_spaces_forward();
    pos = current_position();
    goto_char(point);
    return c_column(pos) + abs(c_indent_in_statement);

  case ':' :
      /* to avoid problems with switch case and default */
      c_frame_of_level[level] = c_column_of_level[level] = 0;
  }

  goto_beginning_of_line();
  c_jump_spaces_forward();
  pos = current_position();
  goto_char(point);
  return c_column(pos) + abs(c_indent_in_statement);
}

int c_indent_out_of_block(int point)
{
  int pos;

  switch (c_search_previous_char_jump_space_comment
	  	(c_memo_area_beginning)) {

  case '}' :
    goto_char(point);
    return 0;
    
  case ')' :
    /* probably the first function parameters list */
    goto_char(point);
    return abs(c_arg_decl_indent);

  case ';' :
    if (previous_char() == '}') {
      /* probably a class definition */
      goto_char(point);
      return 0;
    }
    {
      int ppoint = current_position();
      
      pos = c_corresponding_expression_beginning_position("}", ",");
      if (c_at_beginning_of_definition() && msearch("(", ppoint, 1)) {
	goto_char(c_corresponding_frame_end(current_position()) + 1);
	c_jump_spaces_empty_lines_comments_forward();
	if (c_indentation(current_position()) == abs(c_arg_decl_indent))
	  /* probably the second function parameters list */
	  return abs(c_arg_decl_indent);
      }
    }
    goto_char(point);
    return c_column(pos);

  default :
    if (current_position() == c_memo_area_beginning) {
      goto_char(point);
      return 0;
    }
    goto_char(point);
    return abs(c_indent_in_statement);
  }
}


/******************************************************************************/

/* Default mode */

{
  
  /* default mode */
  
  key_def("default", "^[f", "forward_word");
  key_def("default", "^[b", "backward_word");

  key_def("default", "^[d", "delete_next_word");
  key_def("default", "^[\b", "delete_previous_word");
  key_def("default", "^[\177", "delete_previous_word");      /* esc delete */

  key_def("default", "^[u", "upcase_word");
  key_def("default", "^[l", "downcase_word");
  key_def("default", "^[c", "capitalize_word");
}

void forward_word()
{
  while (! strchr(chars_of_word, current_char()))
    goto_next_char();
  while (current_char() && strchr(chars_of_word, current_char()))
    goto_next_char();
}

void backward_word()
{
  do {
    if (current_position())
      goto_previous_char();
    else
      return;
  } while (! strchr(chars_of_word, current_char()));
  
  while (current_position() && strchr(chars_of_word, current_char()))
    goto_previous_char();
  if (! strchr(chars_of_word, current_char()))
    goto_next_char();
}

void delete_next_word()
{
  int pos = current_position();
  int ndel;
  
  forward_word();
  ndel = current_position() - pos;
  
  goto_char(pos);
  while (ndel--) delete_char();
}

void delete_previous_word()
{
  int pos = current_position();
  int ndel;

  backward_word();
  ndel = pos - current_position();
       
  while (ndel--) delete_char();
}

void upcase_word()
{
  while (! strchr(chars_of_word, current_char()))
    goto_next_char();
  while (current_char() && strchr(chars_of_word, current_char())) {
    if (strchr("abcdefghijklmnopqrstuvwxyz", current_char()))
      replace_char(current_char() + 'A' - 'a');
    goto_next_char();
  }
}

void downcase_word()
{
  while (! strchr(chars_of_word, current_char()))
    goto_next_char();
  while (current_char() && strchr(chars_of_word, current_char())) {
    if (strchr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", current_char()))
      replace_char(current_char() + 'a' - 'A');
    goto_next_char();
  }
}

void capitalize_word()
{
  while (! strchr(chars_of_word, current_char()))
    goto_next_char();
  while (current_char()) {
    if (strchr("abcdefghijklmnopqrstuvwxyz", current_char()))
      replace_char(current_char() + 'A' - 'a');
    goto_next_char();
    while ((! strchr("_", current_char())) &&
	   strchr(chars_of_word, current_char())) {
      if (strchr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", current_char()))
	replace_char(current_char() + 'a' - 'A');
      goto_next_char();
    }
    if (current_char() == '_')
      do goto_next_char(); while (current_char() == '_');
    else
      break;
  }
}

