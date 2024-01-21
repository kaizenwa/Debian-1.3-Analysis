#ifndef __USER_H
#define __USER_H

void user_menu_cmd (void);
char *expand_format (char);
int check_format_view (char *);
int check_format_var (char *p, char **v);
int check_format_cd (char *);
char *check_patterns (char*);

#endif
