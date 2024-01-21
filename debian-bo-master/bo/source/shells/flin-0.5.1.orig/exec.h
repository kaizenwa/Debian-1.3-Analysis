#if !defined (_EXEC_H)
#   define _EXEC_H

#include "parse.h"

void display_file(const char *filen);
void exec_item(struct menu_items item, struct menu **menu);
int parseline(char *buffer, int *argc, char **argv, int maxargc);

#endif

