#if !defined(_MENU_H)
#   define _MENU_H

#include "parse.h"

#define MENU_TITLE	0
#define MENU_NOP	1
#define MENU_EXEC	2
#define MENU_SUB	3
#define MENU_EXIT	4
#define	MENU_QUIT	5
#define MENU_ARGS	6

#define MENU_OP_TITLE	"Title"
#define MENU_OP_EXEC	"Exec"
#define MENU_OP_SUB	"SubMenu"
#define MENU_OP_NOP	"Nop"
#define MENU_OP_EXIT	"Exit"
#define MENU_OP_QUIT	"Quit"
#define MENU_OP_ARGS	"Args"

#define NO_ARGS		0
#define ARGS		1
#define PROMPT		2

#define DEFAULT_PROMPT	"Enter filename"

char *define_menu(struct menu **, char *, char *, int *);

#endif
