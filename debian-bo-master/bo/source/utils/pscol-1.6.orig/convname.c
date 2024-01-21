#include <malloc.h>
#include <stdio.h>
#include "config.h"
#include "pscol.h"

/*********************************************************************/
/*                                                                   */
/* convname: Takes the array col and converts everything.  the Type, */
/*           uid, and the color.  a problem may arise if you have a  */
/*           username made up of all numbers.  This function will    */
/*           think that the name is really a uid and ignore it.      */
/*                                                                   */
/*********************************************************************/

void convname(char **col)
{
 FILE *passwd;
 char *nametype[] = {"unk", "user", "state", "mem", "cpu", "size", "tty", 0};
 char *stor, *name, *line, *pline, *color, *data;
 int ln, cnt, cnt2, uid, done, type, did = 0, tmp;

 if ((passwd = fopen(PASSWD, "r")) == NULL)
 {
  fprintf(stderr, "Can't open %s for reading, cannot convert names\n", PASSWD);
  exit(0);
 }
 name = data = malloc(9);
 stor = malloc(30);
 pline = line = malloc(512);
 color = malloc(30);
 for (ln = 0;col[ln];ln++)
 {
  if (col[ln][1] == ',')
   if (col[ln][0] < '1' || col[ln][0] > '7')
   {
    fprintf(stderr, "error: %s: Unable to convert one of the lines from"
            " config\n", progname);
    exit(0);
   }
   else
    type = col[ln][0] - '0';
  else
  {
   for (cnt = 0;col[ln][cnt] && col[ln][cnt] != ',';cnt++);
   col[ln][cnt] = 0;
   type = strarraycmp(col[ln], nametype) + 1;
   col[ln][cnt] = ',';
   did = 1;
   if (!type)
   {
    fprintf(stderr, "error: %s: Unable to convert one of the lines from"
            " config\n", progname);
    exit(0);
   }
  }
  for (cnt = 0;cnt < 9;cnt++) data[cnt] = 0;
/*  printf("line: %s\n", col[ln]);*/
  for (cnt = 0;col[ln][cnt] && col[ln][cnt] != ',';cnt++);
  cnt++;
  for (cnt2 = cnt;col[ln][cnt2] && col[ln][cnt2] != ',';cnt2++);
  tmp = cnt2 - cnt;
  if (tmp > 8) tmp = 8;
  strncpy(data, col[ln] + cnt, tmp);
  strcpy(color, col[ln] + cnt2 + 1);
/*  printf("color: %s\n", color);*/
  if (type == 2)
  {
   /* check if the name is a number */
   if (!allnum(name) && name[0] != '+')
   {
    /* get name from passwd */
    done = 0;
    while (!feof(passwd) && !done)
    {
     fgets(line, 511, passwd);
     for (;*line && *line != ':';line++);
     *line++ = 0;
     if (strlen(pline) > 8) pline[8] = 0;
     /* compare */
     if (!strcmp(name, pline))
     {
      did = done = 1;
      /* get uid from passwd */
      for (;*line && *line != ':';line++);
      line++;
      for (cnt2 = 0;line[cnt2] && line[cnt2] != ':';cnt2++);
      line[cnt2] = 0;
      uid = atoi(line);
      /* make data */
      sprintf(data, "%d", uid);
     }
     line = pline;
    }
    rewind(passwd);
   }
   else if (name[0] == '+') /* if + is found, replace it with the UID */
    sprintf(data, "%d", getuid());
  }

  /* figure out colors */
  if (!isansi(color))
  {
   did = 1;
   strcpy(color, convansi(color));
  }

  if (did)
  {
   sprintf(stor, "%d,%s,%s", type, data, color);
   col[ln] = malloc(strlen(stor) + 1);
   strcpy(col[ln], stor);
  }
 }
 fclose(passwd);
}
