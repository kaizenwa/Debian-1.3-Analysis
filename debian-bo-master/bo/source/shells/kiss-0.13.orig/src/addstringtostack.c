#include "kiss.h"

void addstringtostack (Stringstack *what, char *newst)
{
    what->str = xrealloc (what->str, (what->nstr + 1) * sizeof (char *));
    what->str [what->nstr++] = xstrdup (newst);
}
