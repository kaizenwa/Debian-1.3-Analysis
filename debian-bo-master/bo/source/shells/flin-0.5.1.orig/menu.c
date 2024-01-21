#ifdef HAVE_CONFIG_H
#   include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <malloc.h>

#include "menu.h"
#include "misc.h"
#include "string.h"

struct token {
   char *str;
   int token, flags;
};

struct token menu_tokens[] = {
   {MENU_OP_TITLE, MENU_TITLE, NO_ARGS},
   {MENU_OP_NOP, MENU_NOP, NO_ARGS},
   {MENU_OP_EXIT, MENU_EXIT, NO_ARGS},
   {MENU_OP_SUB, MENU_SUB, ARGS},
   {MENU_OP_EXEC, MENU_EXEC, ARGS},
   {MENU_OP_QUIT, MENU_QUIT, NO_ARGS},
   {MENU_OP_ARGS, MENU_ARGS, PROMPT},
   {NULL, NO_TOKEN, NO_ARGS}
};

extern char delim;
extern char comment;

static int get_token(char *name, int *flags) {
   int i=0, found=0;

   while (menu_tokens[i].str) {
      if (!strcasecmp(name, menu_tokens[i].str)) {
	 found = 1;
	 break;
      }
      i++;
   }

   if (!found) {
      *flags = 0;
      return NO_TOKEN;
   } else {
      *flags = menu_tokens[i].flags;
      return menu_tokens[i].token;
   }
}

static void alloc_items(struct menu_items **data, const char *line, int i) {
   int worddex, flags;
   char Word[MAX_WORD]; char *word = Word;
   char errmess[MENU_BUFF];

   worddex = substr(line, word, delim);

   /* Advance past word */
   line += worddex+1;

   if (!strlen(line))
      return;
   
   /* Scan for tokens, then dissemate information */
   if (((*data)->type = get_token(word, &flags)) == NO_TOKEN) {
      sprintf(errmess, "No such token `%s'\n", word);
      error(errmess, i);
   }

   if ((worddex = substr(line,word,delim)) == -1)
      error("Premature end-of-line\n",i);
      
   (*data)->name = (char *)chk_alloc(strlen(word)+1);
   strcpy((*data)->name, word);

   switch (flags) {
   case NO_ARGS:
      (*data)->args = NULL;
      (*data)->prompt = NULL;
      break;
   case ARGS:
      line += worddex+1;

      if (substr(line,word,delim) == -1)
	 error("Premature end-of-line\n",i);

      (*data)->args = (char *)chk_alloc(strlen(word)+1);
      strcpy((*data)->args, word);

      break;
   case PROMPT:
      line += worddex+1;

      if ((worddex = substr(line,word,delim)) == -1)
	 error("Premature end-of-line\n",i);

      (*data)->args = (char *)chk_alloc(strlen(word)+1);
      strcpy((*data)->args, word);

      line += worddex+1;

      /* Read in additional ARGS information, an argument prompt. */
      if ((worddex = substr(line,word,delim)) == -1) {
	 /* did not find another delimited string. */

	 (*data)->prompt = (char *)chk_alloc(strlen(DEFAULT_PROMPT)+1);
	 strcpy((*data)->prompt, DEFAULT_PROMPT);
      } else {
	 (*data)->prompt = (char *)chk_alloc(strlen(word)+1);
	 strcpy((*data)->prompt, word);
      }

      break;
   }
   
   /* Allocate another link */
   (*data)->next = (struct menu_items*)chk_alloc(sizeof(struct menu_items));
   (*data)->next->prev = *data;
   *data = (*data)->next;
   (*data)->next = NULL;
}

static char *add_menu_lines(struct menu **menuptr, char *menufile, char *line, int *start_line) {
   int i, index, menu_close;

   i = *start_line;
   menu_close = 0;
   while (!menu_close) {
      /* Error on EOF, it doesnt belong here */
      if ((index = getline(menufile, line)) == -1)
	 error("Premature end-of-file\n", i);
      /* Advance past line */
      menufile += index+1;
      i++;

      strwhite(line);
      if (*line == comment)
	 continue;

      /* Close menu on MENU_END, otherwise define node */
      if (!strcasecmp(line, MENU_END)) {
	 menu_close = 1;
      } else {
	 alloc_items(&((*menuptr)->data), line, i);
      }
   }

   /* Free last link */
   if (!(*menuptr)->data->prev) {
      free((*menuptr)->data);
      (*menuptr)->data = NULL;
   } else {
      (*menuptr)->data = (*menuptr)->data->prev;
      free((*menuptr)->data->next);
      (*menuptr)->data->next = NULL;

      /* rewind linked list */
      while ((*menuptr)->data->prev)
      (*menuptr)->data = (*menuptr)->data->prev;
   }

    *start_line = i;
    return menufile;
}

char *define_menu(struct menu **menuptr, char *menufile, char *line, int *i) {
   char Word[MAX_WORD];
   char *word = Word;

#ifdef DEBUG
   printf("define_menu()\n");
#endif
   
   if (substr(line, word, ' ') == 0)
      error("Premature end-of-line\n", *i);

   /* Store menu name, for easy retrieval */
   (*menuptr)->name = (char *)chk_alloc(strlen(word)+1);
   strcpy((*menuptr)->name, word);

   /* Initialize menu.data structure */
   (*menuptr)->data = (struct menu_items *)chk_alloc(sizeof(struct menu_items));
   (*menuptr)->data->next = NULL;
   (*menuptr)->data->prev = NULL;

   /* Loop through, defining menu, quit at MENU_END */
   menufile = add_menu_lines(menuptr, menufile, line, i);

   /* Allocate another link */
   (*menuptr)->next = (struct menu *)chk_alloc(sizeof(struct menu));
   (*menuptr)->next->prev = *menuptr;
   *menuptr = (*menuptr)->next;
   (*menuptr)->next = NULL;
   (*menuptr)->data = NULL;

   return menufile;
}

