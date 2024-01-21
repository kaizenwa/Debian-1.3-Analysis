#include <malloc.h>

/********************************************************************/
/*                                                                  */
/* convplus: takes the current user id and replaces it with a +     */
/*                                                                  */
/********************************************************************/

void convplus(char **col)
{
 char *stor;
 int uid, ln, cnt;

 stor = malloc(30); /* What can I say, Only allocating 1 var this time */
 for (ln = 0;col[ln];ln++) if (col[ln][0] == '2' && col[ln][1] == ',')
 { /* No point in looking for the current uid if it's not of type 2 */
  /* get uid from color line */
  for (cnt = 2;col[ln][cnt] && col[ln][cnt] != ',';cnt++);
  strncpy(stor, (col[ln] + 2), cnt - 2);
  stor[cnt - 2] = 0;
  uid = atoi(stor);

  /* check to see if this is our UID */
  if (uid == getuid())
  {
   /* rebuild the col line */
   sprintf(stor, "2,+%s", col[ln] + cnt);
   col[ln] = malloc(strlen(stor) + 1);
   strcpy(col[ln], stor);
  }
 }
}
