/* ########################################################################

                 SMAC FILE USED BY XCORAL EDITOR

   File: comments.sc
   Path: /home/c/X11/xcoral-2.19/SmacLib/comments.sc
   Description: 
   Created: Sat Jul 30 11:41:28 MET 1994
   Author: Thierry Emery
   Modified: Sun Jul 31 11:47:01 DST
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   
   ########################################################################

   Note: 

   User interface:
  
       sharp_comment, plus_comment, equal_comment,
       minus_comment, percent_comment
   
   Suggested bindings: 
  
       "^x#" "sharp_comment"
       "^x+" "plus_comment"
       "^x=" "equal_comment"
       "^x-" "minus_comment"
       "^x%" "percent_comment"
   
   Procedure: 

       - to insert function comments, execute `comment',
       `sharp_comment', `plus_comment', `equal_comment',
       `minus_comment', `percent_comment' or a function of yours
       which calls one of these

   ########################################################################

   Copyright (c) : Thierry Emery

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

{
  if (! function("insert_chars"))
    load_file("utilities.sc");
}

/* -------------------------------------------------------------------------
insert <char> delimited comment of width <width>
   ------------------------------------------------------------------------- */

void comment(char line_char, int width) {
    
    int pos;
    
    insert_string("/* ");
    insert_chars(width, line_char);
    insert_string("\n   ");
    
    pos = current_position();
    
    insert_string("\n   ");
    insert_chars(width, line_char);
    insert_string(" */\n\n");
    
    goto_char(pos);
}

/* -------------------------------------------------------------------------
   insert '#' delimited comment
   ------------------------------------------------------------------------- */

void sharp_comment() {
    
    comment('#',80);
}

/* -------------------------------------------------------------------------
   insert '+' delimited comment
   ------------------------------------------------------------------------- */

void plus_comment() {
    
    comment('+',80);
}

/* -------------------------------------------------------------------------
   insert '=' delimited comment
   ------------------------------------------------------------------------- */

void equal_comment() {
    
    comment('=',80);
}

/* -------------------------------------------------------------------------
   insert '-' delimited comment
   ------------------------------------------------------------------------- */

void minus_comment() {
    
    comment('-',80);
}

/* -------------------------------------------------------------------------
   insert '%' delimited comment
   ------------------------------------------------------------------------- */

void percent_comment() {
    
    comment('%',80);
}

