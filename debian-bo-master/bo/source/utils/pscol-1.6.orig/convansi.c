#include <malloc.h>

/***********************************************************************/
/*                                                                     */
/* convansi: takes the color string and converts it to an ansi string  */
/*           without the leading ESC[ and the trailing m               */
/*                                                                     */
/***********************************************************************/

char *convansi(char *ansistr)
{
 char *color[] = {"bla", "red", "gre", "yel", "blu", "mag", "cya",
                  "whi", 0};
 char *other[] = {"bri", "bli", "on", 0};
 int bg = 0, fg = 7, bri = 0, bli = 0, tmp, bground = 0, done = 0;
 char *t;
 char *out;

 out = malloc(30);
 while (*ansistr && !done)
 {
  t = ansistr;
  for (;*ansistr && *ansistr != ' ';ansistr++);
  if (*ansistr == ' ') *ansistr++ = 0;
  if (strlen(t) > 3) t[3] = 0;
  tmp = strarraycmp(t, other);
  switch (tmp)
  {
   case -1: tmp = strarraycmp(t, color);
            if (tmp != -1) if (bground) bg = tmp; else fg = tmp;
            break;
   case 0: if (!bground) bri = 1;
           break;
   case 1: if (!bground) bli = 1;
           break;
   case 2: if (!bground) bground = 1; else done = 1;
           break;
  }
 }
 sprintf(out, "%s%s3%d;4%d", bri?"01;":"", bli?"05;":"", fg, bg);
 return out;
}
