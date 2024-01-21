/* #########################################################################

			       XCORAL SMAC

   File: edt.sc

   Created: Wed Jun 29 20:30:12 1994
   Author: Peter Chang
   Modified: Wed Jun 29 20:30:12 1994
   Last maintained by: Peter Chang

   RCS $Revision: 1.0 $ $State: Exp $

   #########################################################################

   Note: this is not a complete emulation of DEC EDT keys.
         The emphasis is on the common keypad and cursor cluster
	 functions. (No attempt at control keys.)
	 No help available currently.

   Problems: at the moment is that there isn't a keysym for the
             Remove key (so I added a patch to handle_key.c).
	     Keysyms for the top and right edges of
	     the keypad are dependent on the keyboard design.

   Requires: mode.c (uses global_key_def,
             so best to load at end of .xcoralrc)

   Defines: edt_forward_word, edt_previous_page, edt_next_page,
            edt_copy, edt_cut, edt_paste, kp_gold, kp_help, kp_find,
	    kp_dline, kp_page, kp_sect, kp_append, kp_dword, kp_forw,
	    kp_back, kp_cut, kp_dchar, kp_word, kp_eol, kp_char,
	    kp_bol, kp_select, kp_enter, cur_find, cur_insert,
	    cur_remove, cur_select, cur_prior, cur_next

   #########################################################################

   Copyright (c) : Peter Chang

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

   ######################################################################### */

{
  if (! function("goto_beginning_of_c_definition"))
    load_file("mode.sc");
  if (! function("global_key_def"))
    load_file("keydef-ext.sc");
}

/*

   Edt keypad extensions:

 +--------+--------+--------+  +--------+--------+--------+--------+
 |        |        |        |  |        |        | FndNxt |  Del L |
 |  Find  |Ins Here| Remove |  | *Gold  |  HELP  |        |        |
 |        |        |        |  |        |        | *Find  | *Res L |
 +--------+--------+--------+  +--------+--------+--------+--------+
 |        |Prev Scr|Next Scr|  |  Page  |  Sect  | Append |  Del W |
 | Select |        |        |  |        |        |        |        |
 |        |*Pre Win|*Nex Win|  |*Command|  *Fill |*Replace| *Res W |
 +--------+--------+--------+  +--------+--------+--------+--------+
          |        |           | Advance| Backup |   Cut  |  Del C |
          |   Up   |           |        |        |        |        |
          |        |           |*Bottom | *Top   | *Paste | *Res C |
 +--------+--------+--------+  +--------+--------+--------+--------+
 |        |        |        |  |  Word  |  EOL   |  Char  |        |
 |  Left  |  Down  | Right  |  |        |        |        | Enter  |
 |        |        |        |  |*ChngCas|*Del EOL|*Specins|        |
 +--------+--------+--------+  +--------+--------+--------+        +
                               |      Line       | Select |        |
                               |                 |        | *Subs  |
                               |   *Open Line    | *Reset |        |
                               +--------+--------+--------+--------+

Things missing are:
  Gold - numbers for counts
  Command
  Fill
  Change case
  Special insert
*/

void edt_forward_word()
{
  int cc;

  while ((cc = current_char()) && strchr(chars_of_word, cc))
    goto_next_char();
  while (! strchr(chars_of_word, current_char()))
    goto_next_char();
}

void edt_previous_page()
{
   int height = window_height();
   int cline = current_line();
   int y;

   y = cline % height;
   if (y == 0) y = height/2;
   for (; y > 0; y--) goto_previous_line();
}

void edt_next_page()
{
   int height = window_height();
   int cline = current_line();
   int y;

   y = cline % height;
   if (y == 0) y = height/2;
   for (; y > 0; y--) goto_next_line();
}

char *edt_copy(int beg, int end)
{
   char *tmp, *tptr;

   tmp = (char *)malloc(end - beg + 1);
   for (tptr = tmp; beg < end; beg++, tptr++) {
      *tptr = the_char(beg);
   }
   *tptr = '\0';
   return tmp;
}

void edt_cut(int beg, int end)
{
   goto_char(beg);
   for (; beg < end; beg++) {
      delete_char();
   }
}

void edt_paste(char *insert)
{
   insert_string(insert);
}

int gold_mode = 0;
int edt_dirn = 0;
int edt_mark = -1;
char *findstr = 0;
char charstr;
char *wordstr = 0;
char *linestr = 0;
char *selstr = 0;

void kp_gold()
{
   gold_mode = 1;
}

void kp_help()
{
   gold_mode = 0;
}

void kp_find()
{
   if (gold_mode == 0) {
      if (findstr != 0) {
	if (edt_dirn == 0) {
	  forward_search(findstr);
	} else {
	  backward_search(findstr);
	}
      }
   } else {
      char *findprompt, *tmpstr;

      findprompt = (char *) malloc(100);
      if (edt_dirn == 0) {
         if (findstr == 0)
            sprintf(findprompt, "Search forward: ");
	 else
            sprintf(findprompt, "Search forward: (default: %s) ", findstr);
         tmpstr = gets(findprompt);
	 if (tmpstr != 0) {
	    if (findstr != 0) free(findstr);
            findstr = tmpstr;
	 }
	 forward_search(findstr);
      } else {
         if (findstr == 0)
            sprintf(findprompt, "Search backward: ");
	 else
            sprintf(findprompt, "Search backward: (default: %s) ", findstr);
         tmpstr = gets(findprompt);
	 if (tmpstr != 0) {
	    free(findstr);
            findstr = tmpstr;
	 }
	 backward_search(findstr);
      }
      free(findprompt);
      gold_mode = 0;
   }
}

void kp_dline()
{
   if (gold_mode == 0) {
      int here = current_position();
      int end = end_of_line()+1;
      if (linestr != 0) free(linestr);
      linestr = edt_copy(here, end);
      edt_cut(here, end);
   } else {
      edt_paste(linestr);
      gold_mode = 0;
   }
}

void kp_page()
{
   if (gold_mode == 0) {
      if (edt_dirn == 0) {
	 forward_search("\f");
      } else {
	 backward_search("\f");
      }
   } else { /* command NA */
      gold_mode = 0;
   }
}

void kp_sect()
{
   if (gold_mode == 0) {
      if (edt_dirn == 0) {
	 edt_next_page();
      } else {
	 edt_previous_page();
      }
   } else { /* fill NA */
      gold_mode = 0;
   }
}

void kp_append()
{
   if (gold_mode == 0) {
      if (edt_mark >= 0) {
	int here = current_position();
	int end;
	char *tptr, *uptr;

	if (here < edt_mark) {
	  end = edt_mark;
	} else {
	  end = here;
	  here = edt_mark;
	}

	uptr = edt_copy(here, end);
	edt_cut(here, end);
	if (selstr != 0) {
	   tptr = (char *) malloc((strlen(selstr)+strlen(uptr))+1);
	   sprintf(tptr, "%s%s", selstr, uptr);
	   free(selstr);
	   free(uptr);
	   selstr = tptr;
	} else {
	   selstr = uptr;
	}
	edt_mark = -1;
      } else { /* search string append */
	if (findstr !=0) {
	  if (selstr != 0) {
	    char *tptr;

	    tptr = (char *) malloc((strlen(selstr)+strlen(findstr))+1);
	    sprintf(tptr, "%s%s", selstr, findstr);
	    free(selstr);
	    selstr = tptr;
	  } else {
	    selstr = strdup(findstr);
	  }
	}
      }
   } else {  /* replace select with paste buffer */
      if (edt_mark >= 0) {
	int here = current_position();
	int end;

	if (here < edt_mark) {
	  end = edt_mark;
	} else {
	  end = here;
	  here = edt_mark;
	}
	edt_cut(here, end);
	edt_paste(selstr);
	edt_mark = -1;
      }
      gold_mode = 0;
   }
}

void kp_dword()
{
   if (gold_mode == 0) {
      int here = current_position();
      int end;

      edt_forward_word();
      end = current_position();
      goto_char(here);
      if (wordstr != 0) free(wordstr);
      wordstr = edt_copy(here, end);
      edt_cut(here, end);
   } else {
      edt_paste(wordstr);
      gold_mode = 0;
   }
}

void kp_forw()
{
   if (gold_mode == 0) {
      edt_dirn = 0;
   } else {
      goto_end_of_file();
      gold_mode = 0;
   }
}

void kp_back()
{
   if (gold_mode == 0) {
      edt_dirn = 1;
   } else {
      goto_char(0);
      gold_mode = 0;
   }
}

void kp_cut()
{
   if (gold_mode == 0) {
      if (edt_mark >= 0) {
	int here = current_position();
	int end;

	if (here < edt_mark) {
	  end = edt_mark;
	} else {
	  end = here;
	  here = edt_mark;
	}
	if (selstr != 0) free(selstr);
	selstr = edt_copy(here, end);
	edt_cut(here, end);
	edt_mark = -1;
      }
   } else {
      edt_paste(selstr);
      gold_mode = 0;
   }
}

void kp_dchar()
{
   if (gold_mode == 0) {
      charstr = current_char();
      delete_char();
   } else {
      insert_char(charstr);
      gold_mode = 0;
   }
}

void kp_word()
{
   if (gold_mode == 0) {
      if (edt_dirn == 0) {
         edt_forward_word();
      } else {
         backward_word();
      }
   } else { /* changecase NA */
      gold_mode = 0;
   }
}

void kp_eol()
{
   if (gold_mode == 0) {
      if (edt_dirn == 0) {
         goto_next_char();
         goto_end_of_line();
      } else {
         goto_previous_line();
         goto_end_of_line();
      }
   } else {
      int here = current_position();
      int end = end_of_line();
      if (linestr != 0) free(linestr);
      linestr = edt_copy(here, end);
      edt_cut(here, end);
      gold_mode = 0;
   }
}

void kp_char()
{
   if (gold_mode == 0) {
      if (edt_dirn == 0) {
         goto_next_char();
      } else {
         goto_previous_char();
      }
   } else { /* spec insert NA */
      gold_mode = 0;
   }
}

void kp_bol()
{
   if (gold_mode == 0) {
      if (edt_dirn == 0) {
         goto_next_line();
	 goto_beginning_of_line();
      } else {
         goto_previous_line();
	 goto_beginning_of_line();
      }
   } else {
      insert_char('\n');
      gold_mode = 0;
   }
}

void kp_select()
{
   if (gold_mode == 0) {
      edt_mark = current_position();
   } else {
      edt_dirn = 0;
      edt_mark = -1;
      gold_mode = 0;
   }
}

void kp_enter()
{
   if (gold_mode == 0) {
   } else {
      if (selstr != 0 && findstr != 0) {
	int here = current_position();
	int end;

	if (edt_dirn == 0) {
	  goto_previous_char();
	  forward_search(findstr);
	} else {
	  goto_next_char();
	  backward_search(findstr);
	}
	if (here == current_position()) {
	  end = here + strlen(findstr);
	  edt_cut(here, end);
	  edt_paste(selstr);
	  if (edt_dirn == 0) {
	    forward_search(findstr);
	  } else {
	    backward_search(findstr);
	  }
	}
      }
      gold_mode = 0;
   }
}

void cur_find()
{
   gold_mode = 1;
   kp_find();
}

void cur_insert()
{
   if (gold_mode == 0) {
      edt_paste(selstr);
   } else {
      gold_mode = 0;
   }
}

void cur_remove()
{
   if (gold_mode == 0) {
      if (edt_mark >= 0) {
	int here = current_position();
	int end;

	if (here < edt_mark) {
	  end = edt_mark;
	} else {
	  end = here;
	  here = edt_mark;
	}
	if (selstr != 0) free(selstr);
	selstr = edt_copy(here, end);
	edt_cut(here, end);
	edt_mark = -1;
      }
   } else {
      gold_mode = 0;
   }
}

void cur_select()
{
   if (gold_mode == 0) {
      edt_mark = current_position();
   } else {
      gold_mode = 0;
   }
}

void cur_prior()
{
   if (gold_mode == 0) {
      edt_previous_page();
   } else {
      {
         int win = current_window();

         do {
	    if (win == 0) win = 32;
	    win--;
	 } while(select_window(win) == -1);
	 raise_window();
      }
      gold_mode = 0;
   }
}

void cur_next()
{
   if (gold_mode == 0) {
      edt_next_page();
   } else {
      {
         int win = current_window();

         do {
	    if (win == 31) win = -1;
	    win++;
	 } while(select_window(win) == -1);
	 raise_window();
      }
      gold_mode = 0;
   }
}

{
  global_key_def("Find", "cur_find");
  global_key_def("Insert", "cur_insert");
  global_key_def("Remove", "cur_remove");
  global_key_def("Select", "cur_select");
  global_key_def("Prior", "cur_prior");
  global_key_def("Next", "cur_next");
  global_key_def("KP_1", "kp_word");
  global_key_def("KP_2", "kp_eol");
  global_key_def("KP_3", "kp_char");
  global_key_def("KP_4", "kp_forw");
  global_key_def("KP_5", "kp_back");
  global_key_def("KP_6", "kp_cut");
  global_key_def("KP_7", "kp_page");
  global_key_def("KP_8", "kp_sect");
  global_key_def("KP_9", "kp_append");
  global_key_def("KP_0", "kp_bol");
  global_key_def("KP_Decimal", "kp_select");

/* don't like these because they are keyboard dependent but there isn't
   a better way */
  global_key_def("KP_F1", "kp_gold");
  global_key_def("KP_F2", "kp_help");
  global_key_def("KP_F3", "kp_find");
  global_key_def("KP_F4", "kp_dline");
  global_key_def("KP_Subtract", "kp_dword");
  global_key_def("KP_Separator", "kp_dchar");
  global_key_def("KP_Enter", "kp_enter");
/* */
}

