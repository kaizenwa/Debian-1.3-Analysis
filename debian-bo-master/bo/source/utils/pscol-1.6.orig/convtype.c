#include <malloc.h>

/**********************************************************************/
/*                                                                    */
/* convtype: converts the numberacle type to a word                   */
/*                                                                    */
/**********************************************************************/

void convtype(char **col)
{
 char *nametype[] = {"unk", "user", "state", "mem", "cpu", "size", "tty", 0};
 char *stor;
 int ln;

 stor = malloc(40);
 for (ln = 0;col[ln];ln++)
 {
  sprintf(stor, "%s%s", nametype[col[ln][0] - '1'], col[ln] + 1);
  col[ln] = malloc(strlen(stor) + 1);
  strcpy(col[ln], stor);  
 }
}
