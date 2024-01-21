/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: french.sc
   Path: /home/lf/c/X11/xcoral-2.19/SmacLib/french.sc
   Description: 
   Created: Sun Jul 31 12:09:43 MET 1994
   Author: Bruno Pages
   Modified: Fri Aug  5 18:16:59 MET 1994

   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   
   ########################################################################

   Note: 

   Requires: 

   Defines: 

   Suggested bindings: 

   Procedure: 

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
   Les accents en français :

   Pour obtenir un caractère accentué il suffit de taper le caractère
   (minuscule ou majuscule) puis l'accent (~ pour le tréma) :

	a`	:	à
	a^	:	â

	e'	:	é
	e`	:	è
	e^	:	ê
	e~	:	ë

	i^	:	î
	i~	:	ï

	o^	:	ô
		
	u`	:	ù
	u^	:	û
	u~	:	ü


   la cédille se tape avec l'un des trois accents (pas '):

	c~	:	ç
	c`	:	ç
	c^	:	ç

    Dans les autres cas l'accent est inséré comme n'importe
    quel autre caractère.
*/

{
  create_mode("french");
  
  key_def("french", "'", "french_accent");
  key_def("french", "`", "french_accent");
  key_def("french", "^^", "french_accent");
  key_def("french", "~", "french_accent");

  set_mode_font ("french", "-adobe-new century schoolbook-medium-r-normal--18-180-75-75-p-103-iso8859-1" );
}

int french_code_base(int c)
{
  switch(c) {
  case 'a':
  case 'A':
    return c + 127;
  case 'u':
  case 'U':
    return c + 132;
  default:
    return c + 131;
  }
}

void french_accent()
{
  if (previous_char() == 'c') {
    if (last_key() == '\'')
      insert_char(last_key());
    else {
      goto_previous_char();
      replace_char(231);
      goto_next_char();
    }
    return;
  }
  if (previous_char() == 'C') {
    if (last_key() == '\'')
      insert_char(last_key());
    else {
      goto_previous_char();
      replace_char(199);
      goto_next_char();
    }
    return;
  }
  
  goto_previous_char();
  switch (last_key()) {
    
  case '`' :
    if (strchr("aeuAEU", current_char())) {
      replace_char(french_code_base(current_char()));
      goto_next_char();
      return;
    }
    break;
    
  case '\'' :
    if (strchr("eE", current_char())) {
      replace_char(french_code_base(current_char()) + 1);
      goto_next_char();
      return;
    }
    break;
    
  case '^' :
    if (strchr("aeiouAEIOU", current_char())) {
      replace_char(french_code_base(current_char()) + 2);
      goto_next_char();
      return;
    }
    break;
    
  case '~' :
    if (strchr("eiuEIU", current_char())) {
      replace_char(french_code_base(current_char()) + 3);
      goto_next_char();
      return;
    }
  }
  
  goto_next_char();
  insert_char(last_key());
}

