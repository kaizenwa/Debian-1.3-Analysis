#include <malloc.h>

/***********************************************************************/
/*                                                                     */
/* convcolor: converts the ansi string to readable colors              */
/*                                                                     */
/***********************************************************************/

void convcolor(char **col)
{
 char *color[] = {"bla", "red", "gre", "yel", "blu", "mag", "cya",
                  "whi", 0};
 char *stor, *ansi, *a;
 int cnt, ln, bri = 0, bli = 0, bg, fg;

 stor = malloc(40);
 for (ln = 0;col[ln];ln++)
 {
  for (cnt = 0;col[ln][cnt] && col[ln][cnt] != ',';cnt++);
  for (cnt++;col[ln][cnt] && col[ln][cnt] != ',';cnt++); /* again to look
                                                            for it */
  col[ln][cnt++] = 0;
  ansi = col[ln] + cnt;
  bri = bli = 0;
  while (*ansi)
  {
   a = ansi;
   for (;*ansi && *ansi != ';';ansi++);
   if (*ansi == ';') *ansi++ = 0;
   switch (*a)
   {
    case '0': if (a[1] == '1') bri = 1;
              if (a[1] == '5') bli = 1;
              break;
    case '3': fg = a[1] - '0';
              break;
    case '4': bg = a[1] - '0';
              break;
   }
  }
  sprintf(stor, "%s,%s%s%s on %s", col[ln], bri?"bri ":"", bli?"bli ":"",
          color[fg], color[bg]);
  col[ln] = malloc(strlen(stor) + 1);
  strcpy(col[ln], stor);
 }
}
