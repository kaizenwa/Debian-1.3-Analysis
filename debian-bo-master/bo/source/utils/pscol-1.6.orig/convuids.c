#include <malloc.h>
#include <stdio.h>
#include "config.h"

/*********************************************************************/
/*                                                                   */
/* convuids: Takes the array col and converts type 2 uids to names   */
/*           a problem may arise if you have a username made up of   */
/*           all numbers...                                          */
/*                                                                   */
/*********************************************************************/

void convuids(char **col)
{
 FILE *passwd;
 char *line, *stor, *name;
 int ln, cnt, cnt2, uid, puid, done;

 if ((passwd = fopen(PASSWD, "r")) == NULL)
 {
  fprintf(stderr, "Can't open %s for reading, cannot convert uids\n", PASSWD);
  return;
 }
 line = malloc(512);
 for (cnt = 0;cnt < 512;cnt++) line[cnt] = 0;
 name = line;
 stor = malloc(30);

 for (ln = 0;col[ln];ln++) if (col[ln][0] == '2' && col[ln][1] == ',')
 {  /* No point in converting uids if it's not of type 2! */
  /* get uid from color line */
  for (cnt = 2;col[ln][cnt] && col[ln][cnt] != ',';cnt++);
  strncpy(stor, (col[ln] + 2), cnt - 2);
  stor[cnt - 2] = 0;
  if (*stor != '+')
  {
   uid = atoi(stor);
   done = 0;

   /* get uids from /etc/passwd */
   while (!feof(passwd) && !done)
   {
    fgets(line, 512, passwd);
    while (*line && *line != ':') line++;
    *line++ = 0;
    while (*line && *line != ':') line++;
    *line++ = 0;
    for (cnt2 = 0;line[cnt2] && line[cnt2] != ':';cnt2++);
    line[cnt2] = 0;
    strcpy(stor, line);
    puid = atoi(stor);

    /* compare */
    if (puid == uid)
    {
     done = 1;
     /* rebuild */
     if (strlen(name) > 8) name[8] = 0;
     sprintf(stor, "2,%s%s", name, col[ln] + cnt);
     col[ln] = malloc(strlen(stor) + 1);
     strcpy(col[ln], stor);
    }
    line = name;
   }
   rewind(passwd);
  }
 }
 fclose(passwd);
}
