/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: html.sc
   Path: /home/fournigault/c/X11/xcoral-2.405/SmacLib/html.sc
   Description: 
   Created: Wed Sep 20 17:51:28 MET 1995
   Author: Jacques Tremblay
   Modified: Wed Sep 20 17:51:29 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

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
**	Function name : cancelled
**
**	Description : 
**	Input :
**	Output :
*/
cancelled(char *str)
{
  if (str == 0)
    return 0;
  if ( *str == 7 && strlen(str) ==  1 )
    return 1;
  else
    return 0;
}

/*
**	Function name : html_extract_number
**
**	Description : Retourne le numero associe a la string c.
**	Input :
**	Output :
*/
html_extract_number(char *c)
{
  
  if (c != 0 && strlen(c) > 1) 
    return read_int_from_string(c);
  else return 0;
}

/*
**	Function name : html_print_mark
**
**	Description : Affiche p1p2 et positionne le curseur entre p1 et p2.
**	Input :
**	Output :
*/
html_print_mark(char *p1, char *p2)
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
**	Function name : html_markup_region_or_position
**
**	Description :
**	Input :
**	Output :
*/
html_markup_region_or_position(char *start, char *end)
{
  int p;
  int m;
  int orig;

  p = current_position();
  m = mark_position();
  if (m<0) { /* ya pas de mark */
    html_print_mark(start, end);
  } else {
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

/*
**	Function name : html_header
**
**	Description : Pour les H1, H2 etc...
**	Input :
**	Output :
*/
html_header()
{
  int c;
  int p;
  char s1[20];
  char s2[20];
  
  do {
    c = getchar("Header type? [1|2|3|...|6] ");
    if (c==0) c = 1;
    if (c==7) return;
  } while (c<'1' || c>'6');
  sprintf(s1, "<H%c>", c);
  sprintf(s2, "</H%c>",c);
  html_markup_region_or_position(s1, s2);
}

/*
**	Function name : html_colors
**
**	Description :
**	Input :
**	Output :
*/
html_colors()
{
  char *background=0;
  char *bg=0, *link=0, *vlink=0, *text=0;
  char *str;
  int i;
  int win = current_window();
  int orig = current_position();
  int start = 0;
  
  clear_list();
  add_list_item("1: Background color/texture");
  add_list_item("2: Link color");
  add_list_item("3: Visited Link Color");
  add_list_item("4: Color of the Text");

  
  str = select_from_list("Color ? ");

  redisplay();
  select_window(win);
   
  i = html_extract_number(str);
  goto_char(0);
  forward_search("<BODY");
  forward_search(">");  
  start = current_position();
  
  switch(i) {
  case 0:
    break;
  case 1:
    background = gets("Background texture URL: ");
    if (gets_string_cancelled(background)) {
      bg = gets("Background Color (RRGGBB): ");
      if(cancelled(bg)) {
	if (bg) free(bg);
	if (background) free(background);
	goto_char(orig);
	return;
      }
    }
    if (!gets_string_cancelled(background)) {
      wprintf(" BACKGROUND=\"%s\"", background);
    } else if (!gets_string_cancelled(bg)) {
      wprintf(" bgcolor=\"#%s\" ", bg);
    }
    else
      goto_char(orig);
    break;
  case 2:
    link = gets("Link Color (RRGGBB): ");
    if(cancelled(link)){
      free(link);
      goto_char(orig);
      return;
    }
    if (!gets_string_cancelled(link))
      wprintf(" link=\"#%s\" ", link);
    else
      goto_char(orig);
    break;
  case 3:
    vlink = gets("Visited Link Color (RRGGBB): ");
    if(cancelled(vlink)){
      free(vlink);
      goto_char(orig);
      return;
    }
    if (!gets_string_cancelled(vlink))
      wprintf(" vlink=\"#%s\" ", vlink);
    else
      goto_char(orig);
    break;
  case 4:
    text = gets("Color of the Text (RRGGBB): ");
    if(cancelled(text)){
      free(text);
      goto_char(orig);
      return;
    }
    if (!gets_string_cancelled(text))
      wprintf(" text=\"#%s\" ", text);
    else
      goto_char(orig);
    break;
  }

  if (background)
    free(background);  
  if (link) 
    free(link);  
  if (bg) 
    free(bg);
  if (vlink) 
    free(vlink);
  if (text) 
    free(text);
  
  set_mark(start);
  indent_region();
  reset_mark();
} 

/*
**	Function name : html_colors_old
**
**	Description :
**	Input :
**	Output :
*/
html_colors_old()
{
  char *background=0;
  char *bg=0, *link=0, *vlink=0, *text=0;
  char *str;
  int i;
  int win = current_window();
  int orig = current_position();
  int start = 0;
  
  clear_list();
  add_list_item("1: Background color");
  add_list_item("2: Link color");
  add_list_item("3: Visited Link Color");
  add_list_item("4: Color of the Text");

  
  str = select_from_list("Color ? ");

  redisplay();
  select_window(win);
   
  i = html_extract_number(str);
  goto_char(0);
  forward_search("<BODY");
  forward_search(">");  
  start = current_position();
  
  switch(i) {
  case 0:
    break;
  case 1:
    background = gets("Background texture URL: ");
    if (cancelled(background)) {
      free(background);
      goto_char(orig);
      return;
    }
    if (!gets_string_cancelled(background)) {
      bg = gets("Background Color (RRGGBB): ");
      if(cancelled(bg)) {
	free(bg);
	free(background);
	goto_char(orig);
	return;
      }
    }
    if (!gets_string_cancelled(background)) {
      wprintf(" BACKGROUND=\"%s\"", background);
    } else if (!gets_string_cancelled(bg)) {
      wprintf(" bgcolor=\"#%s\" ", bg);
    }
    else
      goto_char(orig);
    break;
  case 2:
    link = gets("Link Color (RRGGBB): ");
    if(cancelled(link)){
      free(link);
      goto_char(orig);
      return;
    }
    if (!gets_string_cancelled(link))
      wprintf(" link=\"#%s\" ", link);
    else
      goto_char(orig);
    break;
  case 3:
    vlink = gets("Visited Link Color (RRGGBB): ");
    if(cancelled(vlink)){
      free(vlink);
      goto_char(orig);
      return;
    }
    if (!gets_string_cancelled(vlink))
      wprintf(" vlink=\"#%s\" ", vlink);
    else
      goto_char(orig);
    break;
  case 4:
    text = gets("Color of the Text (RRGGBB): ");
    if(cancelled(text)){
      free(text);
      goto_char(orig);
      return;
    }
    if (!gets_string_cancelled(text))
      wprintf(" text=\"#%s\" ", text);
    else
      goto_char(orig);
    break;
  }

  if (background)
    free(background);  
  if (link) 
    free(link);  
  if (bg) 
    free(bg);
  if (vlink) 
    free(vlink);
  if (text) 
    free(text);
  
  set_mark(start);
  indent_region();
  reset_mark();
} 

/*
**	Function name : html_new_html
**
**	Description :
**	Input :
**	Output :
*/
html_new_html() {
  int w;
  int p, orig;
  char *name;

  name = gets("Title name: ");
  if (cancelled(name)) {
    free(name);
    return;
  }

  if (current_buffer_is_modified()) {
    w = new_window();
    if (w<0)
      return;
    if (w != select_window(w))
      return;
    set_mode("Html");
  }
  else
    kill_current_buffer();

  orig = current_position();
  
  insert_string("<!-- This file has been generated with Xcoral html mode -->\n");
  insert_string("<HTML>\n<HEAD>\n<! Insert title here ->\n");

  if(name)
    wprintf("<TITLE>%s</TITLE>\n\n</HEAD>\n\n",name);
  insert_string("<BODY>\n<!-- Insert text here -->\n\n</BODY>\n</HTML>\n");

  backward_search("</BODY>");  
  goto_previous_line();
  wprintf("<H4>%s</H4>\n",name);

  set_mark(orig);
  color_region();
  reset_mark();
}

/*
**	Function name : html_glossarize_region
**
**	Description :
**	Input :
**	Output :
*/
html_glossarize_region()
{
  int p;
  int m;
  int nlist, orig;
  
  p = current_line();
  m = mark_position();
  if (m<0) {
    display_message("No region specified");
    return;
  }
  goto_mark();
  goto_beginning_of_line();
  nlist = (p - current_line() + 1)/2;
  orig = current_line();
  wprintf("<dl>\n");
  while(nlist--) {
    wprintf("<dt>");
    goto_next_line();
    goto_beginning_of_line();
    wprintf("<dd>");
    goto_next_line();
    goto_beginning_of_line();
  }
  wprintf("</dl>");
  set_mark(orig);
  color_region();
  reset_mark();
}

/*
**	Function name : html_list_region
**
**	Description :
**	Input :
**	Output :
*/
html_list_region(char *start, char *end)
{
  int p;
  int m;
  int nlist, orig;
  
  p = current_line();
  m = mark_position();
  if (m<0) {
    html_print_mark(start, end);
    return;
  }
  goto_mark();
  goto_beginning_of_line();
  nlist = p - current_line() + 1;
  orig = current_line();
  wprintf("%s\n", start);
  while(nlist--) {
    wprintf("<li>");
    goto_next_line();
    goto_beginning_of_line();
  }
  wprintf("%s\n", end);
  set_mark(orig);
  color_region();
  reset_mark();
}

/*
**	Function name : html_list
**
**	Description :
**	Input :
**	Output :
*/
html_list()
{
  char *str;
  int i;
  int win = current_window();
  
  clear_list();
  add_list_item("1: Glossary (i.e. item definition ...)");
  add_list_item("2: Unordered List");
  add_list_item("3: Ordered List");
  add_list_item("4: New Item and Definition");
  str = select_from_list("What type of list do you want? ");

  redisplay();
  select_window(win);
   
  i = html_extract_number(str);
  
  switch(i) {
  case 0:
    break;
  case 1:
    html_glossarize_region();
    break;
  case 2:
    html_list_region("<ul>", "</ul>");
    break;
  case 3:
    html_list_region("<ol>", "</ol>");
    break;
  case 4:
    html_print_mark("<dt>", "\n<dd>\n");
    break;
  }
}

/*
**	Function name : html_style
**
**	Description :
**	Input :
**	Output :
*/
html_style()
{
  char *str;
  int i, win = current_window();
  
  clear_list();
  add_list_item("1:  Emphasis");
  add_list_item("2:  Strong");
  add_list_item("3:  Citation");
  add_list_item("4:  Code");
  add_list_item("5:  Sample output");
  add_list_item("6:  Bold");
  add_list_item("7:  Italics");
  add_list_item("8:  Underline");
  add_list_item("9:  Typewriter");
  add_list_item("10: Blink");
  add_list_item("11: Preformated");
  str = select_from_list("What style do you want? ");
  
  redisplay();
  select_window(win);
   
  i = html_extract_number(str);
  
  switch(i) {
  case 1:
    html_markup_region_or_position("<em>", "</em>");
    break;
  case 2:
    html_markup_region_or_position("<strong>", "</strong>");
    break;
  case 3:
    html_markup_region_or_position("<cite>", "</cite>");
    break;
  case 4:
    html_markup_region_or_position("<code>", "</code>");
    break;
  case 5:
    html_markup_region_or_position("<samp>", "</samp>");
    break;
  case 6:
    html_markup_region_or_position("<b>", "</b>");
    break;
  case 7:
    html_markup_region_or_position("<i>", "</i>");
    break;
  case 8:
    html_markup_region_or_position("<u>", "</u>");
    break;
  case 9:
    html_markup_region_or_position("<tt>", "</tt>");
    break;
  case 10:
    html_markup_region_or_position("<blink>", "</blink>");
    break;
  case 11:
    html_markup_region_or_position("<pre>", "</pre>");
    break;
  }
}

/*
**	Function name : html_links_and_anchor
**
**	Description :
**	Input :
**	Output :
*/
html_links_and_anchor()
{
  char *c;
  int i, win = current_window();
  char *buffer;
  char *str=0;
  char *str1=0;
  
  clear_list();
  add_list_item("1: Anchor in a document");
  add_list_item("2: Link to an Anchor in the same document");
  add_list_item("3: Link to another URL");
  add_list_item("4: Link to an Anchor in another URL");
  c = select_from_list("What type of Link or Anchor ?");

  redisplay();
  select_window(win);
   
  if (!c) return;
  
  i = c[0];
  buffer = (char *) malloc(256);
  switch (i) {
  case '1':
    str = gets("Anchor name ? ");
    if(!gets_string_cancelled(str)) {
      sprintf(buffer, "<a name=\"%s\">", str);
      html_markup_region_or_position( buffer, "</a>");
    }
    break;
  case '2':
    str = gets("Anchor name ? ");
    if(!gets_string_cancelled(str)) {
      sprintf(buffer, "<a href=\"#%s\">", str);
      html_markup_region_or_position( buffer, "</a>");
    }
    break;
  case '3':
    str = gets("URL name ? ");
    if(!gets_string_cancelled(str)) {
      sprintf(buffer, "<a href=\"%s\">", str);
      html_markup_region_or_position( buffer, "</a>");
    }
    break;
  case '4':
    str = gets("URL name ? ");
    if(!gets_string_cancelled(str)) {
      str1 = gets("Anchor name ? ");
      if(!gets_string_cancelled(str1)) {
	sprintf(buffer, "<a href=\"%s#%s\">", str, str1);
	html_markup_region_or_position( buffer, "</a>");
      }
    }
  }

  free(buffer);
  if (str) 
    free(str);
  if (str1) 
    free(str1);
}

/*
**	Function name : html_image
**
**	Description :
**	Input :
**	Output :
*/
html_image()
{
  char *img_url=0;
  char *alt_text=0;
  char *align=0;
  char *bw=0;
  int i,orig, win = current_window();
  
  img_url = gets("Image URL name: ");
  if(gets_string_cancelled(img_url)) {
    if (img_url)
      free(img_url);
    return;
  }

  alt_text = gets("Alternate text: ");
  if(cancelled(alt_text)){
    free(alt_text);
    free(img_url);
    return;
  }
  
  bw = gets("Border Width around the Image: ");
  if(cancelled(bw)){
    free(bw);
    if(alt_text)
      free(alt_text);
    free(img_url);
    return;
  }
     
  clear_list();
  add_list_item("1: TOP");
  add_list_item("2: MIDDLE");
  add_list_item("3: BOTTOM");
  align = select_from_list("Alignment: ");
  
  redisplay();
  select_window(win);
  
  if(!align) {
    free(img_url);
    if(alt_text)
      free(alt_text);
    if(bw)
      free(bw);
    return;
  }
  
  i = html_extract_number(align);
  
  orig = current_position();
  
  wprintf("<img src=\"%s\" ", img_url);
  if (alt_text) 
    wprintf("alt=\"%s\" ", alt_text);
  
  switch(i) {
  case 1:
    wprintf("align=TOP");
    break;
  case 2:
    wprintf("align=MIDDLE");
    break;
  case 3:
    wprintf("align=BOTTOM");
    break;
  default:
    break;
  }

  if (bw)
    wprintf(" border=%s ", bw);
  
  wprintf(">\n");
  
  set_mark(orig);
  color_region();
  reset_mark();
  
  if (bw)
    free(bw);
  if (img_url)
    free(img_url);
  if (alt_text)
    free(alt_text);
}

/*
**	Function name : html_new_form
**
**	Description :
**	Input :
**	Output :
*/
html_new_form()
{
  char *action=0;
  char *method=0;
  char *buf;
  int i, win = current_window();
  
  action = gets("URL of the program: ");
  if(gets_string_cancelled(action)){
    if (action)
      free (action);
    return;
  }
    
  clear_list();
  add_list_item("1: GET");
  add_list_item("2: POST");
  
  method = select_from_list("Method: ");
  
  redisplay();
  select_window(win);
  
  if(!method){
    free(action);
    return;
  }

  buf = (char*)malloc(strlen(method)+strlen(action)+10);
  i = html_extract_number(method);

  switch(i) {
    case 1:
      sprintf(buf, "<form action=\"%s\" method=\"GET\">\n", action);
      break;
    case 2:
      sprintf(buf, "<form action=\"%s\" method=\"POST\">\n", action);
      break;
    default:
      break;
  }

  html_markup_region_or_position(buf, "\n</form>\n");
  free(action);
  free(buf);
}

/*
**	Function name : html_text_area
**
**	Description :
**	Input :
**	Output :
*/
html_text_area()
{
  char *name=0;
  char *rows=0;
  char *cols=0;
  int orig;
  
  name = gets("Name of text area: ");
  if(gets_string_cancelled(name)){
    if(name)
      free(name);
    return;
  }
  
  rows = gets("Number of rows: ");
  if(gets_string_cancelled(rows)) {
    if(rows)
      free(rows);
    free(name);
    return;
  }
  
  cols = gets("Number of columns: ");
  if(gets_string_cancelled(cols)){
    if(rows)
      free(rows);
    if(cols)
      free(cols);
    if(name)
      free(name);
    return;
  }
    
  orig = current_position();
  wprintf("<textarea name=\"%s\" rows=\"%s\" cols=\"%s\"", name, rows, cols);

  set_mark(orig);
  color_region();
  reset_mark();
    
  html_print_mark(">\n", "\n</textarea>\n");
  if(name)
    free(name);
  if(rows)
    free(rows);
  if(cols)
    free(cols);
}

/*
**	Function name : html_selected_list
**
**	Description : Menus ou scrolled liste.
**	Input :
**	Output :
*/
html_selected_list(char *type)
{
  char *name=0;
  char *value=0;
  char multiple=0;
  char *size;
  int orig;
  
  name = gets("Name of the select field: ");
  if (gets_string_cancelled(name)) {
    if(name)
      free(name);
    return;
  }

  value = gets("Default value: ");
  if(cancelled(value)) {
    free(value);
    free(name);
    return;
  }

  if(strcmp(type,"scrolled")==0) {
    /* Selection multiple ou pas ? */
    multiple = getchar("Will multiple selection be allowed ? [y/n]: ");
    if(multiple == 7) {
      if(value)
	free(value);
      free(name);
      return;
    }
  }

  orig = current_position();
  wprintf("<select name=\"%s\"", name);

  if (multiple=='y' && strcmp(type,"scrolled")==0)
    wprintf(" multiple ");

  if (strcmp(type,"menu")!=0) { /* scrolled list */
    if(value)
      wprintf("size =3>\n<option value=\"%s\">%s", value,value);
    else
      wprintf("size =3>");
  }
  else { /* menu */
    if(value)
      wprintf(">\n<option value=\"%s\">%s", value,value);
    else
      wprintf(">");
  }
  
  set_mark(orig);
  color_region();
  reset_mark();
  
  html_print_mark("\n<option>SunOS\n<option>Solaris\n<option>FreeBSD\n<option>Linux", "\n</select>");
  free(name);
  if(value)
    free(value);
}

/*
**	Function name : html_input_field
**
**	Description :
**	Input :
**	Output :
*/
html_input_field(char *type)
{
  char *name=0;
  char *value=0;
  char checked=0;
  char *size;
  char *maxlength;
  int orig;
  
  name = gets("Name of the field: ");
  if(gets_string_cancelled(name)) {
    if(name)
      free(name);
    return;
  }

  if(strcmp("passwd",type)!=0 && strcmp("hidden",type)!=0 
     && strcmp("checkbox",type)!=0 && strcmp("radio",type)!=0) {
    if (strcmp(type,"submit")==0)
      value = gets("Default Value [Submit]: ");
    else if (strcmp(type,"reset")==0)
       value = gets("Default Value [Reset]: ");
    if(cancelled(value)) {
      if(name)
	free(name);
      free(value);
      return;
    }
  }
  
  if (strcmp("checkbox", type)==0 || strcmp("radio",type)==0){
    while (checked != 'y' && checked != 'n')
      checked = getchar("Checked (y/n) ? ");
  }
  
  if(strcmp("checkbox",type)!=0 && strcmp("radio",type)!=0
     && strcmp("submit",type)!=0 && strcmp("reset",type)!=0) {
    size = gets("Size of input field: ");
    maxlength = gets("Maximum length of input: ");
  }

  orig = current_position();
  if (!gets_string_cancelled(value))
    wprintf("<input type=\"%s\" name=\"%s\" value=\"%s\"", type, name, value);
  else
      wprintf("<input type=\"%s\" name=\"%s\"", type,name); 
  
  if (!strcmp("checkbox", type) || !strcmp("radio", type)) {
    wprintf("%s", (checked=='y') ? " checked ": " ");
  }
  
  if (!gets_string_cancelled(size))
    wprintf(" size=%s ", size);
  if (!gets_string_cancelled(maxlength)) 
    wprintf(" maxlength=%s", maxlength);
  
  wprintf(">\n");

  set_mark(orig);
  color_region();
  reset_mark();
  
  free(name);
  if (value)
    free(value);
  if (size)
    free(size);
  if (maxlength)
    free(maxlength);
}

/*
**	Function name : html_input_image
**
**	Description :
**	Input :
**	Output :
*/
html_input_image()
{
  char *name=0;
  char *cgi=0;
  int orig;
    
  name = gets("Name of the source image: ");
  if(gets_string_cancelled(name)) {
    if(name)
      free(name);
    return;
  }

  cgi = gets("Action programm name: ");
  if(gets_string_cancelled(name)) {
    if(cgi)
      free(cgi);
    free(name);
    return;
  }
  
  orig = current_position();
  
  wprintf ("<FORM METHOD=\"POST\" ACTION=\"/cgi-bin/\"");
  wprintf ("%s\">\n<INPUT TYPE=\"image\" SRC=\"%s\">\n</FORM>\n", name, cgi);

  set_mark(orig);
  color_region();
  reset_mark();
}


/*
**	Function name : html_form_menu
**
**	Description :
**	Input :
**	Output :
*/
html_form_menu()
{
  int i, win = current_window();
  char *str;
  
  clear_list();
  add_list_item("1: Create a New Form");
  add_list_item("2: Add a plain text entry");
  add_list_item("3: Add a password entry");
  add_list_item("4: Add a hidden entry");
  add_list_item("5: Add a multi-line text area");
  add_list_item("6: Add a scrolled list");
  add_list_item("7: Add a menu");
  add_list_item("8: Add an image coordinate input");
  add_list_item("9: Add a checkbox");
  add_list_item("10: Add a radio button");
  add_list_item("11: Add a submit button");
  add_list_item("12: Add a reset button");

  
  str = select_from_list("Form Menu");
  
  redisplay();
  select_window(win);
  
  i = html_extract_number(str);
  
  switch(i) {
  case 1:
    html_new_form();
    break;
  case 2:
    html_input_field("text");
    break;
  case 3:
    html_input_field("passwd");
    break;
  case 4:
    html_input_field("hidden");
    break;
  case 5:
    html_text_area();
    break;
  case 6:
    html_selected_list("scrolled");
    break;
  case 7:
    html_selected_list("menu");
    break;
  case 8:
    html_input_image();
    break;
  case 9:
    html_input_field("checkbox");
    break;
  case 10:
    html_input_field("radio");
    break;
  case 11:
    html_input_field("submit");
    break;
  case 12:
    html_input_field("reset");
    break;
  default:
    break;
  }
}

/*
**	Function name : html_convert_to_french
**
**	Description :
**	Input :
**	Output :
*/
html_convert_to_french()
{
  int m;
  
  m = mark_position();
  set_mark(current_position());
  goto_char(0);
  global_replace("&eacute;", "é");
  global_replace("&egrave;", "è");
  global_replace("&euml;", "ë");
  global_replace("&ecirc;", "ê");
  global_replace("&agrave;", "à");
  global_replace("&acirc;", "â");
  global_replace("&icirc;", "î");
  global_replace("&iuml;", "ï");
  global_replace("&ocirc;", "ô");
  global_replace("&ugrave;", "ù");
  global_replace("&ucirc;", "û");
  global_replace("&uuml;", "ü");
  global_replace("&ccedil;", "ç");

  global_replace("&Eacute;", "É");
  global_replace("&Egrave;", "È");
  global_replace("&Euml;", "Ë");
  global_replace("&Ecirc;", "Ê");
  global_replace("&Agrave;", "À");
  global_replace("&Acirc;", "Â");
  global_replace("&Icirc;", "Î");
  global_replace("&Iuml;", "Ï");
  global_replace("&Ocirc;", "Ô");
  global_replace("&Ugrave;", "Ù");
  global_replace("&Ucirc;", "Û");
  global_replace("&Uuml;", "Ü");
  global_replace("&Ccedil;", "Ç");

  goto_mark();
  if (m<0) reset_mark();
  else set_mark(m);  
}

/*
**	Function name : html_convert_to_html
**
**	Description :
**	Input :
**	Output :
*/
html_convert_to_html()
{
  int m;
  
  goto_char(0);
  global_replace("é", "&eacute;");
  global_replace("è", "&egrave;");
  global_replace("ë", "&euml;");
  global_replace("ê", "&ecirc;");
  global_replace("à", "&agrave;");
  global_replace("â", "&acirc;");
  global_replace("î", "&icirc;");
  global_replace("ï", "&iuml;");
  global_replace("ô", "&ocirc;");
  global_replace("ù", "&ugrave;");
  global_replace("û", "&ucirc;");
  global_replace("ü", "&uuml;");
  global_replace("ç", "&ccedil;");

  global_replace("É", "&Eacute;");
  global_replace("È", "&Egrave;");
  global_replace("Ë", "&Euml;");
  global_replace("Ê", "&Ecirc;");
  global_replace("À", "&Agrave;");
  global_replace("Â", "&Acirc;");
  global_replace("Î", "&Icirc;");
  global_replace("Ï", "&Iuml;");
  global_replace("Ô", "&Ocirc;");
  global_replace("Ù", "&Ugrave;");
  global_replace("Û", "&Ucirc;");
  global_replace("Ü", "&Uuml;");
  global_replace("Ç", "&Ccedil;");
  goto_mark();
  if (m<0) reset_mark();
  else set_mark(m);    
}

/*
**	Function name : html_tables
**
**	Description :
**	Input :
**	Output :
*/
html_tables()
{
  int orig=current_position();
  
  insert_string("<TABLE BORDER=3>\n<CAPTION>TABLE EXEMPLE");
  insert_string("</CAPTION>\n<TR><TH ROWSPAN=2><TH COLSPAN=2>");
  insert_string("Average\n<TH ROWSPAN=2>other<BR>");
  insert_string("category<TH>Misc\n<TR><TH>height");
  insert_string("<TH>weight\n<TR><TH ALIGN=LEFT>males");
  insert_string("<TD>1.9<TD>0.003\n<TR><TH ALIGN=LEFT ");
  insert_string("ROWSPAN=2>females<TD>1.7<TD>0.002\n</TABLE>\n");
  
  set_mark(orig);
  color_region();
  reset_mark();
}

/*
**	Function name : html_macros
**
**	Description :
**	Input :
**	Output :
*/
html_macros()
{
  char *c;
  int num,orig, win = current_window();
  
  clear_list();
  add_list_item("1:  New HTML document");
  add_list_item("2:  Colors...");
  add_list_item("3:  Heading");
  add_list_item("4:  Paragraph");
  add_list_item("5:  Links and Anchors...");
  add_list_item("6:  Style...");
  add_list_item("7:  Address");
  add_list_item("8:  Lists...");
  add_list_item("9:  Image...");
  add_list_item("10: Forms...");
  add_list_item("11: Table");    
  add_list_item("12: Comment");
  add_list_item("13: Center");
  add_list_item("14: Line break");
  add_list_item("15: Horizontal rule");
  add_list_item("16: French accents to HTML");
  add_list_item("17: HTML accents to french.");
  
  c = select_from_list("What do you want ?");
  
  redisplay();
  select_window(win);
   
  if(num = html_extract_number(c))
    set_mode("Html");

  switch(num) {
  case 0: 
    break;
  case 1:
    html_new_html();
    break;
  case 2: 
    html_colors();
    break;
  case 3:
    html_header();
    break;
  case 4:
    html_markup_region_or_position("<p>","\n</p>");
    break;
  case 5:
    html_links_and_anchor();
    break;
  case 6:
    html_style();
   break;
  case 7:
    html_markup_region_or_position("<address>", "</address>");
    break;
  case 8:
    html_list();
    break;
  case 9:
    html_image();
    break;
  case 10:
    html_form_menu();
    break;
  case 11:
    html_tables();
    break;
  case 12:
    html_markup_region_or_position("<!--", "-->");
    break;
  case 13:
    html_markup_region_or_position("<center>", "</center>" );
    break;
  case 14:
    html_markup_region_or_position("<br>", "\n" );
    break;
  case 15:
    html_markup_region_or_position("<hr>", "\n" );
    break;
  case 16:
    html_convert_to_html();
    color_buffer();
    break;
  case 17:
    html_convert_to_french();
    color_buffer();
    break;
  default:
    break;
  }
}
