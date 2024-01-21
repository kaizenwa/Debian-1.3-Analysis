#if !defined(__STRING_H)
#   define __STRING_H

#include <ctype.h>

#define CR '\n'
#define TAB '\t'

int getline(const char *, char *);
int substr(const char *, char *, char);

#define strwhite(X)	while (isspace(*(X))) (X)++

#endif
