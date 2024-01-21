/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: example.sc
   Path: /home/c/X11/xcoral-2.19/SmacLib/example.sc
   Description: 
   Created: Sun Jul 31 12:20:20 DST
   Author: Lionel Fournigault
   Modified: Sun Jul 31 12:20:20 DST
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

HiddenMessage()
{
  int c;
  
  select_window(new_window());
  insert_string("Ydpsbm ju hppe gps zpv");
  while( current_position() != beginning_of_line() ) {
    goto_previous_char();
    redisplay ();
  }
  while( current_position() != end_of_line() ) {
    c = current_char();
    delete_char();
    if ( c != ' ' )
      c -= 1;
    insert_char(c);
    redisplay();
  }
  wprintf("\nYes\n");
}	     		

SomeBoxes()
{
  char *str = 0;
  
  display_message("\nDialog box test: "); /* Message box */
  str = gets ("write something "); /* Dialog box */
  if ( str != 0) {
    display_message(str); 
    free(str);
  }
  display_message("\nFile selector test: ");
  str = file_select(); /* File selector */
  if ( str != 0) {
    display_message(str); 
    free(str);
  }
  
  display_message("\nList box test: ");
  clear_list(); /* List select */
  add_list_item ( "Choice 1" );
  add_list_item ( "Choice 2" );
  add_list_item ( "Choice 3" );
  str = select_from_list("My list");
  if ( str != 0 && strlen(str) > 1 )
    display_message(str); 
}		
