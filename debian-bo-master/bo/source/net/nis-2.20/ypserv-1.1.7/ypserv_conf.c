/*
** Copyright (c) 1996, 1997 Thorsten Kukuk
**
** This file is part of the NYS YP Server.
**
** The NYS YP Server is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS YP Server is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
**
** You should have received a copy of the GNU General Public
** License along with the NYS YP Server; see the file COPYING.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Thorsten Kukuk <kukuk@uni-paderborn.de>
*/

#ifndef LINT
static const char rcsid[] = "$Id: ypserv_conf.c,v 1.5 1997/04/08 21:15:29 kukuk Exp $";
#endif

#if defined(HAVE_CONFIG_H)
#include "config.h"
#endif

#include "system.h"

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "yp_msg.h"
#include "ypserv.h"

#ifndef HAVE_STRDUP
#include <compat/strdup.c>
#endif

int
getipnr (char *n, char *network, char *netmask)
{
  char *m;
  size_t i;
  int pw, pm;
  char buf[20];

  pw = pm = 0;

  m = strtok (n, "/");

  sscanf (m, "%s", buf);

  for (i = 0; i < strlen (buf); i++)
    if ((buf[i] < '0' || buf[i] > '9') && buf[i] != '.')
      return 1;
    else if (buf[i] == '.')
      pw++;

  strcpy (network, buf);
  switch (pw)
    {
    case 0:
      strcat (network, ".0.0.0");
      pw++;
      break;
    case 1:
      if (network[strlen (network) - 1] == '.')
	strcat (network, "0.0.0");
      else
	{
	  strcat (network, ".0.0");
	  pw++;
	}
      break;
    case 2:
      if (network[strlen (network) - 1] == '.')
	strcat (network, "0.0");
      else
	{
	  strcat (network, ".0");
	  pw++;
	}
      break;
    case 3:
      if (network[strlen (network) - 1] == '.')
	strcat (network, "0");
      else
	pw++;
      break;
    default:
      return 1;
      break;
    }

  m = strtok (NULL, "/");

  if ((m != NULL) && (strlen (m) != 0))
    {
      sscanf (m, "%s", buf);

      for (i = 0; i < strlen (buf); i++)
	if ((buf[i] < '0' || buf[i] > '9') && buf[i] != '.')
	  return 1;
	else if (buf[i] == '.')
	  pm++;

      strcpy (netmask, buf);
      switch (pm)
	{
	case 0:
	  strcat (netmask, ".0.0.0");
	  break;
	case 1:
	  if (netmask[strlen (netmask) - 1] == '.')
	    strcat (netmask, "0.0.0");
	  else
	    strcat (netmask, ".0.0");
	  break;
	case 2:
	  if (netmask[strlen (netmask) - 1] == '.')
	    strcat (netmask, "0.0");
	  else
	    strcat (netmask, ".0");
	  break;
	case 3:
	  if (netmask[strlen (netmask) - 1] == '.')
	    strcat (netmask, "0");
	  break;
	default:
	  return 1;
	}
    }
  else
    switch (pw)
      {
      case 1:
	strcpy (netmask, "255.0.0.0");
	break;
      case 2:
	strcpy (netmask, "255.255.0.0");
	break;
      case 3:
	strcpy (netmask, "255.255.255.0");
	break;
      case 4:
	strcpy (netmask, "255.255.255.255");
	break;
      default:
	return 1;
      }
  return 0;
}

conffile_t *
load_ypserv_conf (const char *path)
{
  FILE *in;
  char c, *filename;
  char buf1[1025], buf2[1025], buf3[1025];
  long line = 0;
  conffile_t *ptr, *work;

  ptr = NULL;
  work = NULL;

  if ((filename = malloc (strlen (path) + strlen ("/ypserv.conf") + 2)) == NULL)
    {
      yp_msg ("ERROR: could not allocate enough memory! [%s|%d]\n", __FILE__, __LINE__);
      exit (1);
    }
  sprintf (filename, "%s/ypserv.conf", path);
  if ((in = fopen (filename, "r")) == NULL)
    {
      if (debug_flag)
	yp_msg ("WARNING: no %s found!\n", filename);
      free (filename);
      return ptr;
    }

  free (filename);

  while ((c = fgetc (in)) != (char) EOF)
    {				/*while */
      line++;
      switch (tolower (c))
	{
	case 'd':
	  {
	    size_t i, j;

	    fgets (buf1, sizeof (buf1) - 1, in);
	    i = 0;
	    while (c != ':' && i <= strlen (buf1))
	      {
		if ((c == ' ') || (c == '\t'))
		  break;
		buf2[i] = c;
		buf2[i + 1] = '\0';
		c = buf1[i];
		i++;
	      }

	    while ((buf1[i - 1] != ':') && (i <= strlen (buf1)))
	      i++;

	    if ((buf1[i - 1] == ':') && (strcmp (buf2, "dns") == 0))
	      {
		if (!dns_flag)	/* Do not overwrite parameter */
		  {
		    while (((buf1[i] == ' ') || (buf1[i] == '\t')) &&
			   (i <= strlen (buf1)))
		      i++;
		    j = 0;
		    while ((buf1[i] != '\0') && (buf1[i] != '\n'))
		      buf3[j++] = buf1[i++];
		    buf3[j] = 0;

		    sscanf (buf3, "%s", buf2);
		    if (strcmp (buf2, "yes") == 0)
		      dns_flag = 1;
		    else if (strcmp (buf2, "no") == 0)
		      dns_flag = 0;
		    else
		      yp_msg ("Unknown dns option in line %d: => Ignore line\n",
			      line);
		  }
	      }
	    else
	      yp_msg ("Parse error in line %d: => Ignore line\n", line);

	    if (debug_flag)
	      yp_msg ("ypserv.conf: dns: %d\n", dns_flag);
	    break;
	  }
	case 's':
	  {			/* sunos_kludge */
	    size_t i, j;

	    fgets (buf1, sizeof (buf1) - 1, in);
	    i = 0;
	    while (c != ':' && i <= strlen (buf1))
	      {
		if ((c == ' ') || (c == '\t'))
		  break;
		buf2[i] = c;
		buf2[i + 1] = '\0';
		c = buf1[i];
		i++;
	      }

	    while ((buf1[i - 1] != ':') && (i <= strlen (buf1)))
	      i++;

	    if ((buf1[i - 1] == ':') && (strcmp (buf2, "sunos_kludge") == 0))
	      {
		while (((buf1[i] == ' ') || (buf1[i] == '\t')) &&
		       (i <= strlen (buf1)))
		  i++;
		j = 0;
		while ((buf1[i] != '\0') && (buf1[i] != '\n'))
		  buf3[j++] = buf1[i++];
		buf3[j] = 0;

		sscanf (buf3, "%s", buf2);
		if (strcmp (buf2, "yes") == 0)
		  sunos_kludge_flag = 1;
		else if (strcmp (buf2, "no") == 0)
		  sunos_kludge_flag = 0;
		else
		  yp_msg ("Unknown sunos_kludge option in line %d: => Ignore line\n",
			  line);
	      }
	    else
	      yp_msg ("Parse error in line %d: => Ignore line\n", line);

	    if (debug_flag)
	      yp_msg ("ypserv.conf: sunos_kludge: %d\n", sunos_kludge_flag);
	    break;
	  }
	case 't':
	  {			/* tryresolve */
	    size_t i, j;

	    fgets (buf1, sizeof (buf1) - 1, in);
	    i = 0;
	    while (c != ':' && i <= strlen (buf1))
	      {
		if ((c == ' ') || (c == '\t'))
		  break;
		buf2[i] = c;
		buf2[i + 1] = '\0';
		c = buf1[i];
		i++;
	      }

	    while ((buf1[i - 1] != ':') && (i <= strlen (buf1)))
	      i++;

	    if ((buf1[i - 1] == ':') && (strcmp (buf2, "tryresolve") == 0))
	      {
		while (((buf1[i] == ' ') || (buf1[i] == '\t')) &&
		       (i <= strlen (buf1)))
		  i++;
		j = 0;
		while ((buf1[i] != '\0') && (buf1[i] != '\n'))
		  buf3[j++] = buf1[i++];
		buf3[j] = 0;

		sscanf (buf3, "%s", buf2);
		if (strcmp (buf2, "yes") == 0)
		  tryresolve_flag = 1;
		else if (strcmp (buf2, "no") == 0)
		  tryresolve_flag = 0;
		else
		  yp_msg ("Unknown tryresolve option in line %d: => Ignore line\n",
			  line);
	      }
	    else
	      yp_msg ("Parse error in line %d: => Ignore line\n", line);

	    if (debug_flag)
	      yp_msg ("ypserv.conf: tryresolve: %d\n", tryresolve_flag);
	    break;
	  }
	case 'x':
	  {			/* xfr_check_port */
	    size_t i, j;

	    fgets (buf1, sizeof (buf1) - 1, in);
	    i = 0;
	    while (c != ':' && i <= strlen (buf1))
	      {
		if ((c == ' ') || (c == '\t'))
		  break;
		buf2[i] = c;
		buf2[i + 1] = '\0';
		c = buf1[i];
		i++;
	      }

	    while ((buf1[i - 1] != ':') && (i <= strlen (buf1)))
	      i++;

	    if ((buf1[i - 1] == ':') && (strcmp (buf2, "xfr_check_port") == 0))
	      {
		while (((buf1[i] == ' ') || (buf1[i] == '\t')) &&
		       (i <= strlen (buf1)))
		  i++;
		j = 0;
		while ((buf1[i] != '\0') && (buf1[i] != '\n'))
		  buf3[j++] = buf1[i++];
		buf3[j] = 0;

		sscanf (buf3, "%s", buf2);
		if (strcmp (buf2, "yes") == 0)
		  xfr_check_port = 1;
		else if (strcmp (buf2, "no") == 0)
		  xfr_check_port = 0;
		else
		  yp_msg ("Unknown xfr_check_port option in line %d: => Ignore line\n",
			  line);
	      }
	    else
	      yp_msg ("Parse error in line %d: => Ignore line\n", line);

	    if (debug_flag)
	      yp_msg ("ypserv.conf: xfr_check_port: %d\n", xfr_check_port);
	    break;
	  }
	case '0':
	case '1':
	case '2':
	case '*':
	  {
	    char *n, *m, *s, *p, *f;
	    conffile_t *tmp;

	    fgets (buf1, sizeof (buf1) - 1, in);
	    buf2[0] = c;
	    buf2[1] = '\0';
	    strcat (buf2, buf1);

	    n = strtok (buf2, ":");
	    if (n == NULL)
	      {
		yp_msg ("Parse error in line %d => Ignore line\n", line);
		break;
	      }
	    m = strtok (NULL, ":");
	    if (m == NULL)
	      {
		yp_msg ("No map given in line %d => Ignore line\n", line);
		break;
	      }

	    s = strtok (NULL, ":");
	    if (s == NULL)
	      {
		yp_msg ("No security entry in line %d => Ignore line\n", line);
		break;
	      }
	    p = strtok (NULL, ":");
	    if (p != NULL && strlen (p) != 0)
	      f = strtok (NULL, ":");
	    else
	      f = NULL;

	    if ((tmp = malloc (sizeof (conffile_t))) == NULL)
	      {
		yp_msg ("ERROR: could not allocate enough memory! [%s|%d]\n", __FILE__, __LINE__);
		exit (1);
	      }
	    tmp->next = NULL;
	    tmp->field = 2;

	    if (c == '*')
	      {
#if defined(HAVE_INET_ATON)
		inet_aton ("0.0.0.0", &tmp->network);
		inet_aton ("0.0.0.0", &tmp->netmask);
#else
		tmp->network.s_addr = inet_addr ("0.0.0.0");
		tmp->netmask.s_addr = inet_addr ("0.0.0.0");
#endif
	      }
	    else
	      {
		if (getipnr (n, buf1, buf2) != 0)
		  {
		    yp_msg ("Malformed network/netmask entry in line %d\n", line);
		    free (tmp->map);
		    free (tmp);
		    break;
		  }
#if defined(HAVE_INET_ATON)
		inet_aton (buf1, &tmp->network);
		inet_aton (buf2, &tmp->netmask);
#else
		tmp->network.s_addr = inet_addr (buf1);
		tmp->netmask.s_addr = inet_addr (buf2);
#endif
	      }
	    sscanf (m, "%s", buf1);
	    tmp->map = strdup (buf1);

	    sscanf (s, "%s", buf1);

	    if (strcmp (buf1, "none") == 0)
	      tmp->security = SEC_NONE;
	    else if (strcmp (buf1, "deny") == 0)
	      tmp->security = SEC_DENY;
	    else if (strcmp (buf1, "port") == 0)
	      tmp->security = SEC_PORT;
	    else if (strcmp (buf1, "des") == 0)
	      tmp->security = SEC_DES;
	    else
	      {
		yp_msg ("Unknown security option \"%s\" in line %d => Ignore line\n",
			buf1, line);
		free (tmp->map);
		free (tmp);
		break;
	      }

	    switch (tmp->security)
	      {			/* switch(tmp->security */
	      case SEC_DENY:	/* Ignore mangle field since we don't allow access to map */
		tmp->mangle = MANGLE_NO;
		break;
	      case SEC_PORT:	/* Look at mangle field, since we
				   could mangle, if port > 1023 */
	      case SEC_DES:
	      case SEC_NONE:	/* Look at mangle field: */
		if (p == NULL)
		  tmp->mangle = MANGLE_NO;	/* By default, don't mangle */
		else
		  {
		    sscanf (p, "%s", buf1);
		    if (strcmp (buf1, "yes") == 0)
		      tmp->mangle = MANGLE_YES;
		    else if ((strcmp (buf1, "no") == 0) || (strlen (buf1) == 0))
		      tmp->mangle = MANGLE_NO;
		    else
		      {
			yp_msg ("Unknown mangle option \"%s\" in line %d => Ignore line\n",
				buf1, line);
			free (tmp->map);
			free (tmp);
			break;
		      }
		  }
	      }

	    if (f != NULL)
	      {
		sscanf (f, "%d", &tmp->field);
		if (tmp->field < 1)
		  {
		    yp_msg ("mangle field %d does not exist, use default field 2!\n",
			    tmp->field);
		    tmp->field = 2;
		  }
	      }
	    if (debug_flag)
	      {
		yp_msg ("ypserv.conf: %s/", inet_ntoa (tmp->network));
		yp_msg ("%s:%s:%d:%d:%d\n", inet_ntoa (tmp->netmask),
			tmp->map, tmp->security, tmp->mangle, tmp->field);
	      }

	    if (work == NULL)
	      {
		work = tmp;
		ptr = work;
	      }
	    else
	      {
		work->next = tmp;
		work = work->next;
	      }
	    break;
	  }
	case ' ':
	case '\t':
	  line--;		/* Ignore Character, no new line */
	  break;
	case '\n':
	  break;		/* Ignore newline */
	case '#':
	  fgets (buf1, sizeof (buf1) - 1, in);
	  break;
	default:
	  fgets (buf1, sizeof (buf1) - 1, in);
	  yp_msg ("Parse error in line %d: %c%s", line, c, buf1);
	  break;
	}
    }
  fclose (in);

  return ptr;
}

#if 0

int debug_flag = 1;
int dns_flag = 0;

void
main ()
{
  conffile_t *ptr;

  ptr = load_ypserv_conf (".");

  yp_msg ("Ausgabe:\n");

  while (ptr != NULL)
    {
      yp_msg ("%s/", inet_ntoa (ptr->network));
      yp_msg ("%s:%s:%d:%d\n", inet_ntoa (ptr->netmask), ptr->map,
	      ptr->security, ptr->mangle);
      ptr = ptr->next;
    }

}

#endif
