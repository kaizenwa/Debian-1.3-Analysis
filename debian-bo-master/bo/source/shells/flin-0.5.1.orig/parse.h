#if !defined(_PARSE_H)
#   define _PARSE_H

#define NO_TOKEN	-1

#define MENU_BEGIN	"Menu"
#define MENU_END	"EndMenu"
#define DIREC_NOBOX	"NoBox"

#define MENU_BUFF	255
#define MAX_WORD	255
#define	MAX_LINE	255

struct menu_items {
   int type;
   char *name;
   char *args;
   char *prompt;
   struct menu_items *next, *prev;
};

struct menu {
   char *name;
   struct menu_items *data;
   struct menu *next, *prev;
};

int parsefile(struct menu **,char *);
void freemem(struct menu *,char *);

#endif
