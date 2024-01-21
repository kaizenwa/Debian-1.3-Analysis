/*
Parses menu file into a linked list
Copyright (C) 1995  Brian Cully

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 675 Mass
Ave, Cambridge, MA 02139, USA.

please send patches or advice to: `shmit@kublai.com'
*/
#ifdef HAVE_CONFIG_H
#   include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "string.h"
#include "parse.h"
#include "menu.h"
#include "misc.h"

char delim=DELIM;
char comment=COMMENT;

extern char draw_box;

char *set_nobox(struct menu **, char *, char *, int *);

struct directive {
   char *str;
   char *(*func)(struct menu **, char *, char *, int *);
};

struct directive menu_directives[] = {
   { MENU_BEGIN, define_menu },
   { DIREC_NOBOX, set_nobox },
   { NULL, NULL }
};

static int get_directive(char *name, char *(**func)(struct menu **, char *, char *, int *)) {
   int i=0, found=0;

   while (menu_directives[i].str) {
      if (!strcasecmp(name, menu_directives[i].str)) {
	 found = 1;
	 break;
      }
      i++;
   }

   if (!found)
      return NO_TOKEN;

   *func = menu_directives[i].func;

#ifdef DEBUG
   printf("Directive: `%s'\n", menu_directives[i].str);
#endif
   
   return 0;
}

char *set_nobox(struct menu **menu_ptr, char *menufile, char *line, int *line_no) {
   draw_box = 0;

   return menufile;
}

/* Parses the file into a linked menu list
** On entry:
**    menuptr == empty pointer to list
**    menufile == loaded menu file
** On exit:
**    menuptr == full menu, in linked list
**    returns -1 on error, 0 otherwise
*/

int parsefile(struct menu **menuptr,char *menufile) {
   char Line[MAX_LINE], Word[MAX_WORD];
   char *line = Line, *word = Word;
   int index,i=0;

   *menuptr = (struct menu *)chk_alloc(sizeof(struct menu));
   (*menuptr)->prev = NULL;
   (*menuptr)->next = NULL;
   (*menuptr)->data = NULL;

   while ((index = getline(menufile,line)) != -1) {
      int worddex;
      char *(*func)(struct menu **, char *, char *, int *);
      
      i++;
      menufile += index+1;

      /* Skip past white space */
      strwhite(line);

      /* comment -> `\0' */
      if (*line == comment)
	 continue;

      /* Ignore blank lines */
      if (*line == '\0')
	 continue;

      if ((worddex = substr(line, word, ' ')) == 0)
	 error("Premature end-of-line\n", i);
      
      line += worddex + 1;

      if (get_directive(word, &func) == NO_TOKEN) {
	 char mess[MAX_WORD];
	 
	 sprintf(mess, "Unrecognized directive: `%s'\n", word);
	 error(mess, i);
      }

#ifdef DEBUG
      printf("calling func()\n");
#endif
      
      menufile = func(menuptr, menufile, line, &i);
   }

   /* Free last link */

   if (!(*menuptr)->prev) {
      free(*menuptr);
      *menuptr = NULL;
   } else {
      *menuptr = (*menuptr)->prev;
      free((*menuptr)->next);
      (*menuptr)->next = NULL;

      /* rewind linked list */

      while ((*menuptr)->prev)
	 *menuptr = (*menuptr)->prev;
   }

   return 0;
}

/* frees up allocated memory
** On entry:
**    menuptr == linked menu list
**    menufile == loaded menu file
** On Exit:
**    nothing, DONT USE menuptr OR menufile without allocating them again
*/

void freemem(struct menu *menuptr, char *menufile) {
   free(menufile);

   while (menuptr) {
      struct menu *p;

      p = menuptr;
      menuptr = menuptr->next;
      while (p->data) {
	 struct menu_items *q;
	 
	 q = p->data;
	 p->data = p->data->next;
	 if (q->name)
	    free(q->name);
	 if (q->args)
	    free(q->args);
	 free(q);
      }
      free(p->name);
      free(p);
   }
}
