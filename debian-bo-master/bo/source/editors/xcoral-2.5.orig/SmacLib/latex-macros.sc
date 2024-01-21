/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: latex_macros.sc
   Path: /home/fournigault/c/X11/xcoral-2.404/SmacLib/latex_macros.sc
   Description: 
   Created: Fri Aug  4 09:43:04 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Aug  4 09:43:04 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   Requires: 

   Defines: 

   Suggested bindings: 

   Procedure: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

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
**	Function name : color_indent
**
**	Description :
**	Input :
**	Output :
*/
color_indent(int orig)
{
  set_mark(orig);
  goto_end_of_line();
  color_region();
  indent_region();
  reset_mark();
}


/*
**	Function name : cancelled
**
**	Description : 
**	Input :
**	Output :
*/
latex_cancelled(char *str)
{
  if (str == 0)
    return 0;
  if ( *str == 7 && strlen(str) ==  1 )
    return 1;
  else
    return 0;
}

/*
**	Function name : latex_extract_number
**
**	Description : Retourne le numero associe a la string c.
**	Input :
**	Output :
*/
latex_extract_number(char *c)
{
  
  if (c != 0 && strlen(c) > 1) 
    return read_int_from_string(c);
  else return 0;
}

/*
**	Function name : latex_print_mark
**
**	Description : Affiche p1p2 et positionne le curseur entre p1 et p2.
**	Input :
**	Output :
*/
latex_print_mark(char *p1, char *p2)
{
  int p;
  int orig = current_position();

  insert_string(p1);
  p = current_position();
  insert_string(p2);
  set_mark(orig);
  color_region();
  goto_char(p);
  reset_mark();
}

/*
**	Function name : latex_markup_region_or_position
**
**	Description :
**	Input :
**	Output :
*/
latex_markup_region_or_position(char *start, char *end)
{
  int p;
  int m;
  int tmp;
  int orig;

  p = current_position();
  m = mark_position();
  if (m<0) { /* ya pas de mark */
    latex_print_mark(start, end);
  } 
  else {
      if (m>p){
	  tmp = p;
	  p = m;
	  m = tmp;
	  set_mark(m);
      }
      goto_mark();
      orig = current_position();
      set_mark(p);
      wprintf(start);
      goto_mark();
      set_mark(m);
      wprintf(end);
      set_mark(orig);
      color_region();
      reset_mark();
  }
}


/* --------------------------------------------------------------------------------
   Latex book, report, article and letter header
   -------------------------------------------------------------------------------- */
int color_buffer();

char *_latex_gs = "\\documentclass[11pt]{xxx}

\\title{{\\Huge My beautiful document} \\\\ {\\large More Info}}
\\author{Your Name}

\\begin{document}

\\maketitle
\\tableofcontents
  
\\part{One}

\\chapter{foo}

\\section{Alpha}

\\section{Beta}

\\chapter{bar}

\\appendix
  
\\chapter{AppOne}

\\chapter{AppTwo}

\\end{document}
";

char *_latex_letter = "\\documentclass[11pt]{letter}
\\address{Xcoral compagny \\\\
          France}
\\signature{Lionel Fournigault}
\\begin{document}
\\begin{letter}{Unix users \\\\
	        and Linux users.}

\\opening{Dear friends,}

This letter to attend the broadcast of Xcoral 2.5 over the Net.
  
You are now in possession of a great editor and I hope you will enjoy it. This version includes 
plenty of new functionalities that will help you for example to write LaTeX documents or 
Html pages.
  

\\closing{Yours faithfully}
\\ps{PS : Please, if you have any remarks about Xcoral send me a mail at lf@lri.fr.}
\\cc{Bruno Pages}
\\end{letter}
\\end{document}
";

/*
**	Function name : latex_style
**
**	Description :
**	Input :
**	Output :
*/
latex_style(char *style)
{
  if(strcmp(style,"letter")==0) {
    insert_string(_latex_letter);
    set_mark(0);
    goto_end_of_file();
    indent_region();
  }
  else {
    insert_string(_latex_gs);
    redisplay();
    goto_char(0);
    forward_search("xxx");
    delete_chars(3);
    insert_string(style);
    
    if(strcmp(style,"article")==0) {
      goto_char(0);
      if (forward_search("tableofcontents")) {
	goto_beginning_of_line();
	delete_to_end_of_line();
	delete_chars(2);
      }
      while ( ! at_end_of_file()) {
	if (forward_search("chapter")) {
	  goto_beginning_of_line();
	  delete_to_end_of_line();
	  delete_chars(2);
      }
	else
	  break;
      }
    }
    set_mark(0);
    goto_end_of_file();
    indent_region();
    if (backward_search("appendix")){
	goto_beginning_of_line();
	delete_line_blanks();
    }
    goto_char(0);
    forward_search("]");
  }
}

/*
**	Function name : latex_document_style
**
**	Description :
**	Input :
**	Output :
*/
latex_document_style()
{
    char *str;
    int num=0;
    int orig = 0;
    int w = current_window();
    
    clear_list();
    
    add_list_item("1: Article");
    add_list_item("2: Report");
    add_list_item("3: Book"); 
    add_list_item("4: Letter");

    str = select_from_list("Document style");

    redisplay();
    select_window(w);
      
    if(num = latex_extract_number(str))
      set_mode("Latex");
    else
      return;
    
    if (current_buffer_is_modified()) {
      w = new_window();
      if (w<0) 
	return;
      if (w != select_window(w))
	return;
      select_window(w);
      set_mode("Latex");
    }
    else
      kill_current_buffer();

    orig = current_position();
    switch(num) {
    case 1:
      latex_style("article");
      break;
    case 2:
      latex_style("report");
      break;
    case 3:
      latex_style("book");
      break;
    case 4:
      latex_style("letter");
      break;
    default:
      break;
    }
    color_buffer();
}

/*
**	Function name : latex_title
**
**	Description :
**	Input :
**	Output :
*/
latex_title()
{
  int orig = current_position();
  
  insert_string("\\title{Xcoral user's manual}\n\\author{Lionel and his friends}\n\\abstract{summary}\n\\date{\\today}\n");
  insert_string("%Don't forget \\maketitle command which comes after \\begin{document}\n");
  
  color_indent(orig);
}

/*
**	Function name : latex_sectionning
**
**	Description :
**	Input :
**	Output :
*/
latex_sectioning()
{
    char *str = 0;
    int num=0;
    int orig = 0;
    int w = current_window();
    
    clear_list();
    
    add_list_item("1: Part");
    add_list_item("2: Chapter");
    add_list_item("3: Section"); 
    add_list_item("4: Subsection");
    add_list_item("5: Subsubsection");
    add_list_item("6: Input");
    add_list_item("7: Appendix");    

    str = select_from_list("Sectioning");

    redisplay();
    select_window(w);
      
    if(num = latex_extract_number(str))
      set_mode("Latex");
    else
      return;
    
    orig = current_position();
    switch(num){
    case 1:
      insert_string("\\part{}");
      break;
    case 2:
      insert_string("\\chapter{}");
      break;
    case 3:
      insert_string("\\section{}");
      break;
    case 4:
      insert_string("\\subsection{}");
      break;
    case 5:
      insert_string("\\subsubsection{}");
      break;
    case 6:
      insert_string("\\input{}");
      break;
    case 7:
      insert_string("\\appendix");
      break;
    default:
      break;
    }
    
    color_indent(orig);
    if (num != 7)
      backward_search("}");
}

/*
**	Function name : latex_list
**
**	Description :
**	Input :
**	Output :
*/
latex_list()
{
    char *str = 0;
    int num=0;
    int orig = 0;
    int w = current_window();
    
    clear_list();
    
    add_list_item("1: Itemize");
    add_list_item("2: Enumerate");
    add_list_item("3: Description"); 
    add_list_item("4: Item"); 
    add_list_item("5: Labeled item"); 
    
    str = select_from_list("List");

    redisplay();
    select_window(w);
      
    if(num = latex_extract_number(str))
      set_mode("Latex");
    else
      return;
    
    orig = current_position();
    switch(num){
    case 1:
      insert_string("\\begin{itemize} \n\\item \n\\item \n\\end{itemize} \n");
      break;
    case 2:
      insert_string("\\begin{enumerate} \n\\item \n\\item \n\\end{enumerate} \n \n");
      break;
    case 3:
      insert_string("\\begin{description} \n\\item[] \n\\item[] \n\\end{description} \n \n ");
    case 4:
      insert_string("\\item");
      break;
    case 5:
      insert_string("\\item[]");
      break;
    default:
      break;
    }

    color_indent(orig);
    goto_char(orig);
    forward_search("\\item");
    goto_end_of_line();
    if(num==3 || num == 5)
      goto_previous_char();
}

/*
**	Function name : latex_math
**
**	Description :
**	Input :
**	Output :
*/
latex_math()
{
  char *str = 0;
  int num=0;
  int orig = 0;
  int w = current_window();
  
  clear_list();
  
  add_list_item("1: $ ... $");
  add_list_item("2: $$ ... $$");
  add_list_item("3: Array"); 
  add_list_item("4: Equation array"); 
  add_list_item("5: Split equation array");

  str = select_from_list("Math style");
  
  redisplay();
  select_window(w);
  
  if(num = latex_extract_number(str))
    set_mode("Latex");
  else
    return;
  
  orig = current_position();
  switch(num){
  case 1:
    insert_string("$ $");
    color_indent(orig);
    backward_search("$");
    goto_previous_char();
    break;
  case 2:
    insert_string("$$\n \n$$\n");
    color_indent(orig);
    goto_char(orig);
    goto_next_line();
    goto_end_of_line();
    break;
  case 3:
    insert_string("$$\n\\begin{array}{cc}\n & \\\\ \n\\end{array}\n$$\n");
    color_indent(orig);
    goto_char(orig);
    forward_search("&");
    break;
  case 4:
    insert_string("\\begin{eqnarray}\n &  & \n\\end{eqnarray}\n");
    color_indent(orig);
    goto_char(orig);
    forward_search("&");
    break;
  case 5:
    insert_string("\\begin{eqnarray}\n \\lefteqn{} \\\\\n &  &  \n\\end{eqnarray}\n");
    color_indent(orig);
    goto_char(orig);
    forward_search("lefteqn");
    forward_search("}");
    break;
  }
}

/*
**	Function name : latex_font_style
**
**	Description :
**	Input :
**	Output :
*/
latex_font_style()
{
    char *str = 0;
    int num=0;
    int orig = 0;
    int w = current_window();
    
    clear_list();
    
    add_list_item("1: Bold");
    add_list_item("2: Sans serif");
    add_list_item("3: Slanted"); 
    add_list_item("4: Small caps"); 
    add_list_item("5: Typewriter"); 
    add_list_item("6: Roman"); 
    add_list_item("7: Italic"); 
    add_list_item("8: Emphasis"); 
    add_list_item("9: Calligraphic");

    str = select_from_list("Font style");

    redisplay();
    select_window(w);
      
    if(num = latex_extract_number(str))
      set_mode("Latex");
    else
      return;
    
    
    if ((orig=mark_position())<0) /* ya pas de mark */
      orig = current_position();

    switch(num){
    case 1:
      latex_markup_region_or_position("{\\bf ","}");
      break;
    case 2:
      latex_markup_region_or_position("{\\sf ","}");
      break;
    case 3:
      latex_markup_region_or_position("{\\sl ","}");
      break;
    case 4:
      latex_markup_region_or_position("{\\sc ","}");
      break;
    case 5:
      latex_markup_region_or_position("{\\tt ","}");
      break;
    case 6:
      latex_markup_region_or_position("{\\rm ","}");
      break;
    case 7:
      latex_markup_region_or_position("{\\it ","}");
      break;
    case 8:
      latex_markup_region_or_position("{\\em ","}");
      break;
    case 9:
      latex_markup_region_or_position("{\\cal ","}");
      break;
    default:
      break;
    }
    set_mark(orig);
    color_region();
    reset_mark();
}

/*
**	Function name : latex_font_size
**
**	Description :
**	Input :
**	Output :
*/
latex_font_size()
{
    char *str = 0;
    int num=0;
    int orig = 0;
    int w = current_window();
    
    clear_list();
    
    add_list_item("1: Tiny");
    add_list_item("2: Scriptsize");
    add_list_item("3: Footnotesize"); 
    add_list_item("4: Small"); 
    add_list_item("5: Normalsize"); 
    add_list_item("6: large"); 
    add_list_item("7: Large"); 
    add_list_item("8: LARGE"); 
    add_list_item("9: huge"); 
    add_list_item("10: Huge"); 

    str = select_from_list("Font style");

    redisplay();
    select_window(w);
      
    if(num = latex_extract_number(str))
      set_mode("Latex");
    else
      return;
    
    orig = current_position();
    switch(num){
    case 1:
      latex_markup_region_or_position("{\\tiny ","}");
      break;
    case 2:
      latex_markup_region_or_position("{\\scriptsize ","}");
      break;
    case 3:
      latex_markup_region_or_position("{\\footnotesize ","}");
      break;
    case 4:
      latex_markup_region_or_position("{\\small ","}");
      break;
    case 5:
      latex_markup_region_or_position("{\\normalsize ","}");
      break;
    case 6:
      latex_markup_region_or_position("{\\large ","}");
      break;
    case 7:
      latex_markup_region_or_position("{\\Large ","}");
      break;
    case 8:
      latex_markup_region_or_position("{\\LARGE ","}");
      break;
    case 9:
      latex_markup_region_or_position("{\\huge ","}");
      break;
    case 10:
      latex_markup_region_or_position("{\\Huge ","}");
      break;
    default:
      break;
    }
    set_mark(orig);
    color_region();
    reset_mark();
}

/*
**	Function name : latex_figure
**
**	Description :
**	Input :
**	Output :
*/
latex_figure()
{
  int orig = current_position();

  insert_string("\\begin{figure}[htbp]  \n \\centerline{\\epsfxsize=10cm \\epsfbox{filename.ps}} \n");
  insert_string("\\caption{.  \\label{filename}}\n\\end{figure} \n  \n");
  
  color_indent(orig);
  backward_search(".ps");
}

/*
**	Function name : latex_array
**
**	Description :
**	Input :
**	Output :
*/
latex_other_environment()
{
    char *str = 0;
    int num=0;
    int orig = 0;
    int w = current_window();
    
    clear_list();
    
    add_list_item("1: Tabular");
    add_list_item("2: Table");
    add_list_item("3: Verbatim"); 
    add_list_item("4: Minipage"); 
    add_list_item("5: Quotation"); 
    add_list_item("6: Quote"); 
    add_list_item("7: Center"); 
    add_list_item("8: Abstract"); 

    str = select_from_list("Array");

    redisplay();
    select_window(w);
      
    if(num = latex_extract_number(str))
      set_mode("Latex");
    else
      return;
    
    orig = current_position();
    switch(num){
    case 1:
      insert_string("\\begin{center} \n \\begin{tabular}{|c|c|} \n \\hline \n & \\\\  \n \\hline \n ");
      insert_string("\\end{tabular} \n  \\end{center}\n\n");
      color_indent(orig);
      backward_search("&");
      break;
    case 2:
      insert_string("\\begin{table}[htbp] \n \\begin{center} \n \\begin{tabular}{|c|c|} \n");
      insert_string("\\hline \n & \\\\ \n \\hline \n \\end{tabular} \n \\caption{. \\label{table-}} \n");
      insert_string("\\end{center} \n \\end{table}\n\n");
      color_indent(orig);
      backward_search("&");
      break;
    case 3:
      insert_string("\\begin{verbatim} \n & \n\\end{verbatim}\n");
      color_indent(orig);
      backward_search("&");
      delete_char();
      break;
    case 4:
      insert_string("\\begin{minipage}[t]{5cm}\n & \n\\end{minipage}\n");
      color_indent(orig);
      backward_search("&");
      delete_char();
      break;
    case 5:
      insert_string("\\begin{quotation} \n & \n\\end{quotation}\n");
      color_indent(orig);
      backward_search("&");
      delete_char();
      break;
    case 6:
      insert_string("\\begin{quote} \n & \n\\end{quote}\n");
      color_indent(orig);
      backward_search("&");
      delete_char();
      break;
    case 7:
      insert_string("\\begin{center} \n & \n\\end{center}\n");
      color_indent(orig);
      backward_search("&");
      delete_char();
      break;
    case 8:
      insert_string("\\begin{abstract} \n & \n\\end{abstract}\n");
      color_indent(orig);
      backward_search("&");
      delete_char();
      break;
    default:
      break;
    }
}

/*
**	Function name : latex_reference
**
**	Description :
**	Input :
**	Output :
*/
latex_reference()
{
  char *str = 0;
  int num=0;
  int orig = 0;
  int w = current_window();
  
  clear_list();
  
  add_list_item("1: Label"); 
  add_list_item("2: Reference");
  add_list_item("3: Page reference");
  
  str = select_from_list("Cross-reference");
  
  redisplay();
  select_window(w);
  
  if(num = latex_extract_number(str))
    set_mode("Latex");
  else
    return;
  
  orig = current_position();
  switch(num){
  case 1:
    insert_string("\\label{}");
    break;
  case 2:
    insert_string("\\ref{}");
    break;
  case 3:
    insert_string("\\pageref{}");
    break;
  default:
    break;
  }

  set_mark(orig);
  color_region();
  backward_search("}");
}

/*
**	Function name : latex_note
**
**	Description :
**	Input :
**	Output :
*/
latex_note()
{
  char *str = 0;
  int num=0;
  int orig = 0;
  int w = current_window();
  
  clear_list();
  
  add_list_item("1: Footnote");
  add_list_item("2: Marginpar");
  
  str = select_from_list("Note");
  
  redisplay();
  select_window(w);
  
  if(num = latex_extract_number(str))
    set_mode("Latex");
  else
    return;
  
  if ((orig=mark_position())<0) /* ya pas de mark */
    orig = current_position();

  switch(num){
  case 1:
    latex_markup_region_or_position("\\footnote{","}");
    break;
  case 2:
    latex_markup_region_or_position("\\marginpar{","}");
    break;
  default:
    break;
  }
    set_mark(orig);
    color_region();
    reset_mark();
}


/*
**	Function name : latex_bibliography
**
**	Description :
**	Input :
**	Output :
*/
latex_bibliography()
{
  char *str = 0;
  int num=0;
  int orig = 0;
  int w = current_window();
  
  clear_list();
  
  add_list_item("1: Bibliography article");
  add_list_item("2: Bibliography book");
  add_list_item("3: Bibliography inbook"); 
  add_list_item("4: Bibliography proceedings"); 
  add_list_item("5: Bibliography inproceedings"); 
  add_list_item("6: Bibliography mastersthesis"); 
  add_list_item("7: Bibliography techreport"); 
  add_list_item("8: Bibliography unpublished"); 
  
  str = select_from_list("Bibliography");
  
  redisplay();
  select_window(w);
  
  if(num = latex_extract_number(str))
    set_mode("Latex");
  else
    return;
  
  orig = current_position();
  switch(num){
  case 1:
    insert_string("@ARTICLE{??, \n author = \"{}\", \n title = \"{}\", \n journal = \"{}\", \n");
    insert_string("year = \"{}\",\n }  \n ");
    insert_string("% OPTIONAL FIELDS \n ");
    insert_string("% volume = \"{}\", \n % number = \"{}\", \n % pages =  \"{}\",  \n ");
    insert_string("% month =  \"{}\", \n % note =  \"{}\",\n \n ");
    break;
  case 2:
    insert_string("@BOOK{??, \n  author = \"{}\", \n  title = \"{}\", \n publisher = \"{}\", \n ");
    insert_string("year = \"{}\", \n  } \n");
    insert_string("% you must precise an author or an editor: \n % editor =\"{}\", \n ");
    insert_string("% OPTIONAL FIELDS \n");
    insert_string("% volume =  \"{}\", \n % series =  \"{}\",  \n % address = \"{}\", \n ");
    insert_string("% edition =  \"{}\", \n % month =  \"{}\", \n % note = \"{}\", \n \n");
    break;
  case 3:
    insert_string("@INBOOK{??, \n author = \"{}\", \n title = \"{}\", \n ");
    insert_string("chapter = \"{}\", \n publisher = \"{}\", \n year = \"{}\", \n } \n");
    insert_string("% you must precise an author or an editor: \n % editor =\"{}\", \n ");
    insert_string("% you must precise a chapter and or pages: \n % pages = \"{}\", \n ");
    insert_string("% OPTIONAL FIELDS \n ");
    insert_string("% volume =  \"{}\", \n % series =  \"{}\",  \n % address =  \"{}\", \n ");
    insert_string("% edition =  \"{}\", \n % month =  \"{}\", \n % note =  \"{}\", \n \n ");
    break;
  case 4:
    insert_string("@PROCEEDINGS{??, \n title = \"{}\", \n year = \"{}\", \n } \n");
    insert_string("% OPTIONAL FIELDS \n");
    insert_string("% editor =\"{}\", \n % publisher = \"{}\", \n % organization =\"{}\", \n");
    insert_string("% address =  \"{}\", \n % month =  \"{}\", \n % note =  \"{}\"\n \n");
    break;
  case 5:
    insert_string("@INPROCEEDINGS{??, \n author = \"{}\", \n title = \"{}\", \n");
    insert_string("booktitle = \"{}\", \n year = \"{}\", \n } \n");
    insert_string("	% OPTIONAL FIELDS \n ");
    insert_string("% editor =\"{}\", \n % pages = \"{}\", \n % organization =\"{}\",   \n");
    insert_string(" % publisher = \"{}\", \n % address =  \"{}\", \n % month =  \"{}\", \n");
    insert_string(" % note =  \"{}\" \n \n");
    break;
  case 6:
    insert_string("@MASTERSTHESIS{??, \n author = \"{}\", \n title = \"{}\", \n");
    insert_string("school = \"{}\", \n year = \"{}\", \n } \n"); 
    insert_string(" % OPTIONAL FIELDS \n ");
    insert_string(" % address = \"{}\", \n % month =  \"{}\", \n % note =  \"{}\"\n \n ");
    break;
  case 7:
    insert_string("@TECHREPORT{??, \n author = \"{}\", \n title = \"{}\", \n");
    insert_string("institution = \"{}\", \n year = \"{}\", \n  } \n ");
    insert_string(" % OPTIONAL FIELDS \n ");
    insert_string(" % type = \"{}\", \n % number = \"{}\", \n ");
    insert_string(" % address = \"{}\", \n % month =  \"{}\",\n % note =  \"{}\" \n \n ");
    break;
  case 8:
    insert_string("@UNPUBLISHED{??, \n author = \"{}\", \n title = \"{}\", \n note = \"{}\",  \n }\n");
    insert_string("% OPTIONAL FIELDS \n ");
    insert_string("% month =  \"{}\",\n % year =  \"{}\", \n \n");
    break;
  default:
    break;
  }
  
  color_indent(orig);
  backward_search("??");
}

/*
**	Function name : latex_macros
**
**	Description :
**	Input :
**	Output :
*/
latex_macros()
{
    char *str;
    int num=0;
    int win = current_window();
    
    clear_list();
    
    add_list_item("1: Document style..."); /* article report book letter */
    add_list_item("2: Title"); /* title author date abstract thanks */
    add_list_item("3: Sectioning..."); /* input part chapter section [sub]subsection appendix */
    add_list_item("4: List..."); /* itemize enumerate description */
    add_list_item("5: Math..."); /* $xxx$, $$xxx$$, equation etc... */
    add_list_item("6: Font style..."); /* \em */
    add_list_item("7: Font size..."); /* large small */
    add_list_item("8: Figure");
    add_list_item("9: Others environments..."); /* tabular, mini page, verbatim, quote, slide comment */
    add_list_item("10: Cross-reference..."); /* label ref pageref */
    add_list_item("11: Note..."); /*footnote, marginpar */
    add_list_item("12: Bibliography..."); /* \nocite, cite \bibliography */

    str = select_from_list("LaTeX styles and macros");

    redisplay();
    select_window(win);
      
    if(num = latex_extract_number(str))
      set_mode("Latex");
    else
      return;

    switch(num) {
    case 1:
      latex_document_style();
      break;
    case 2:
      latex_title();
      break;
    case 3:
      latex_sectioning();
      break;
    case 4:
      latex_list();
      break;
    case 5:
      latex_math();
      break;
    case 6:
      latex_font_style();
      break;
    case 7:
      latex_font_size();
      break;
    case 8:
      latex_figure();
      break;
    case 9:
      latex_other_environment();
      break;
    case 10:
      latex_reference();
      break;
    case 11:
      latex_note();
      break;
    case 12:
      latex_bibliography();
      break;
    default:
      break;
    }
}
      
