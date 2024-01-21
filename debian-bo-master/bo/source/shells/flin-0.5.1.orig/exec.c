#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <string.h>
#include <syslog.h>
#include <signal.h>
#include <string.h>

#include "exec.h"
#include "misc.h"
#include "screen.h"
#include "menu.h"
#include "additional.h"

extern int exec_logging;

/* Finds the named menu */
static int find_menu(struct menu **menu, char *name)
{
   struct menu *q = *menu;

   while (q->prev)
      q = q->prev;

   while (q && (strcmp(q->name, name))) {
      q = q->next;
   }

   if (q) {
      *menu = q;
      return 0;
   } else
      return -1;
}

void display_file(const char *filen)
{
   pid_t pid_child;
   int child_stat;

   clear_scr();
   close_scr();

   if ((pid_child = fork()) == -1)
      error("No more pids\n", 0);
   else if (pid_child > 0) {
      waitpid(pid_child, &child_stat, WUNTRACED);
   } else {
      char *cmd_line = malloc(strlen(PAGER) + strlen(filen) + 2);

      sprintf(cmd_line, "%s %s", PAGER, filen);
      system(cmd_line);
      exit(0);
   }

   printf("[----Hit the ENTER key when you're ready----]\n");
   fflush(stdout);
   while (getchar() != '\n');
   fflush(stdin);
   init_scr();
   clear_scr();
}

/* Interface to screen module
   Execute menu item
   item == item to execute
   *menu == menu list ptr, for use with MENU_SUBs */
void exec_item(struct menu_items item, struct menu **menu)
{
   pid_t pid_child;
   int child_stat;
   char args[255];
   char exec[255];
   char *argv[128];
   int argc;

   switch (item.type) {
   case MENU_SUB:
      if (!find_menu(menu, item.args)) {
	 display_list(*menu);
      } else
	 fprintf(stderr, "No menu named %s\n", item.name);
      break;

   case MENU_ARGS:
      if (get_args(args, item.prompt) == 0) {
	 sprintf(exec, "%s %s", item.args, args);

	 clear_scr(); close_scr();
	 if (prep_for_exec(exec, &argc, argv, 127) > 0)
	    do_exec(exec, argc, argv);
      
	 printf("[----Hit ENTER when you're ready----]\n");
	 fflush(stdout);
	 while (getchar() != '\n');
	 fflush(stdin);
      }

      init_scr(); clear_scr();

      break;

   case MENU_EXEC:
      clear_scr();
      close_scr();

      if (prep_for_exec(item.args, &argc, argv, 127) > 0)
	 do_exec(item.args, argc, argv);
      
      printf("[----Hit ENTER when you're ready----]\n");
      fflush(stdout);
      while (getchar() != '\n');
      fflush(stdin);
      init_scr();
      clear_scr();
      break;
   }
}

int prep_for_exec(char *exec_line, int *argc, char **argv, int maxargc) {
   int ret;

   ret = parseline(exec_line, argc, argv, maxargc);

   if (ret >= 0)
      *(argv + *argc) = NULL;

   return ret;
}

void do_exec(char *exec_line, int argc, char **argv) {
   pid_t pid_child;
   int child_stat;

   if ((pid_child = fork()) == -1)
      error("No more pids\n", 0);
   else if (pid_child > 0) {
      if ((exec_logging == 1) && (exec_line != (char *) NULL))
	 syslog(LOG_LOCAL1 | LOG_INFO, "%d: %s (%d)", getpid(), exec_line, pid_child);

      waitpid(pid_child, &child_stat, WUNTRACED);

      for (; argc; --argc)
	 free((void *) *(argv + argc));
   } else {
      execvp(*argv, argv);
      exit(0);
   }
}
