#include <stdio.h>
#include <string.h>


/************************************************************************
 *
 * Concatentates 3 strings
 *
 *************************************************************************/
char CatS[256];

char *CatString3(char *a, char *b, char *c)
{
  int len = 0;

  if(a != NULL)
    len += strlen(a);
  if(b != NULL)
    len += strlen(b);
  if(c != NULL)
    len += strlen(c);

  if (len > 255)
    return NULL;

  if(a == NULL)
    CatS[0] = 0;
  else
    strcpy(CatS, a);
  if(b != NULL)
  strcat(CatS, b);
  if(c != NULL)
    strcat(CatS, c);
  return CatS;
}
