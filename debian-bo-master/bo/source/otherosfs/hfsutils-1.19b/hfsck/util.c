/*
 * hfsutils - tools for reading and writing Macintosh HFS volumes
 * Copyright (C) 1996, 1997 Robert Leslie
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

# include <stdio.h>
# include <string.h>
# include <stdarg.h>

# include "hfsck.h"
# include "util.h"

/*
 * NAME:	mctime()
 * DESCRIPTION:	convert Macintosh time to an ASCII string
 */
char *mctime(unsigned long secs)
{
  time_t date;
  static char str[26];

  if (secs == 0)
    return "(Never)";

  date = d_toutime(secs);
  strcpy(str, ctime(&date));
  str[24] = 0;

  return str;
}

/*
 * NAME:	extstr()
 * DESCRIPTION:	convert an extent record into a printable string
 */
char *extstr(ExtDataRec *rec)
{
  char extstr[20];
  static char str[60];
  ExtDescriptor *ext;
  int i;

  str[0] = 0;

  for (i = 0, ext = &(*rec)[0]; i < 3; ++i, ++ext)
    {
      switch (ext->xdrNumABlks)
	{
	case 0:
	  return (i == 0) ? "[]" : str;

	case 1:
	  sprintf(extstr, "1[%u]", ext->xdrStABN);
	  break;

	default:
	  sprintf(extstr, "%u[%u..%u]",
		  ext->xdrNumABlks,
		  ext->xdrStABN,
		  ext->xdrStABN + ext->xdrNumABlks - 1);
	}

      if (i > 0)
	strcat(str, "+");

      strcat(str, extstr);
    }

  return str;
}

/*
 * NAME:	ask()
 * DESCRIPTION:	answer question from user
 */
int ask(char *question, ...)
{
  int result = -1;
  va_list args;

  va_start(args, question);

  vprintf(question, args);

  if (! REPAIR)
    {
      printf(".\n");
      result = 0;
    }
  else if (YES)
    {
      printf(": fixing.\n");
      result = 1;
    }
  else
    {
      while (1)
	{
	  int i;
	  char answer[5];

	  printf(". Fix? ");
	  fflush(stdout);

	  i = scanf("%4s", answer);
	  if (i < 0)
	    {
	      result = 0;
	      break;
	    }

	  if (i > 0)
	    {
	      switch (answer[0])
		{
		case 'y':
		case 'Y':
		  result = 1;
		  break;

		case 'n':
		case 'N':
		  result = 0;
		  break;
		}
	    }

	  if (result < 0)
	    {
	      vprintf(question, args);
	      continue;
	    }
	  else
	    break;
	}
    }

  va_end(args);

  return result;
}
