#include <stdio.h>
#include <malloc.h>

/********************************************************************/
/*                                                                  */
/* buildvar: takes the single lined color and builds the new var    */
/*                                                                  */
/********************************************************************/

char *buildvar(char **col)
{
 char *stor;
 int size = 0, ln;

 for (ln = 0;col[ln];ln++) size += strlen(col[ln]) + 1;
 stor = malloc(size);
 for (ln = 0;ln < size;ln++) stor[ln] = 0;
 for (ln = 0;col[ln + 1];ln++)
 {
  strcat(stor, col[ln]);
  strcat(stor, "%");
 }
 strcat(stor, col[ln]);
 return stor;
}
