/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1996  Lectra Systemes & Pierre Ficheux
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
static char rcsid[] = "$Id: procedure.c,v 1.4 1996/10/28 13:37:35 pierre Exp $";

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "xtel.h"

static int iproc = -1;
static char *current_wait;
static struct d *current_pd;

static struct keyword keywords[] = {
    { "procedure", 'P', P_PROC },
    { "service", 'S', P_SERV },
    { "waitfor", 'A', P_WAIT },
    { "transmit", 'E', P_TRAN },
    { "record", 'R', P_REC },
    { "write", 'W', P_WRI },
    { "hangup", 'H', P_HUP },
    { "exit", 'X', P_EXI },
    { NULL, 0, 0 }
};

/* Lecture d'une commande et de son parametre */
static char *get_keyword (s, pi)
char *s;
int *pi;
{
  register int i = 0;
  char *tok, *p;

  tok = next_token (s, " ");

  while (keywords[i].mnemonic) {
    if (!strcmp (keywords[i].mnemonic, tok) || keywords[i].alias == *tok) {
      *pi = i;
      p = next_token (NULL, "\n");
      return p;
    }
    
    i++;
  }

  return NULL;
}


#define NEW(s)	((s *)calloc (1, sizeof(s)))

/* Lecture et initialisation des procedures */
init_procedures (file)
char *file;
{
  char buf[256];
  char *s, type;
  int i;
  struct d *pd = NULL, *pdprev = NULL;
  FILE *fp = fopen (file, "r");

  if (!fp)
      return 0;

  while (fgets (buf, 256, fp) != NULL) {
    if (buf[0] == '#' || buf[0] == '\n')
      continue;

    buf[strlen(buf)-1] = 0;
    s = get_keyword (buf, &i);
    if (s) {
      if ((type = keywords[i].type) == P_PROC && iproc < MAXPROC-1) {
	if (pd) {
	  free (pd);
	  pd = NULL;
	  pdprev->suivant = NULL;
	}

	procedures[++iproc].nom = build_name (s);
	procedures[iproc].d = pd = pdprev = NEW(struct d);
      }
      else if (type == P_SERV) {
	procedures[iproc].service = strdup (s);
      }
      else {
	pd->type = type;
	pd->suivant = NEW(struct d);

	switch (type) {

	case P_WAIT :

	  (pd->p).attente = strdup (s);
	break;

	case P_TRAN :

	  (pd->p).emission = strdup (s);
	break;

	case P_REC :

	  (pd->p).state = (!strcmp (s, "on") ? 1 : 0);
	break;

	case P_WRI :

	  (pd->p).filename = strdup (s);
	break;

	default:
	  break;
	}
	
	pdprev = pd;
	pd = pd->suivant;
      }
    }
    else
      printf ("<%s> non reconnu !\n", buf);
  }

  current_pd = NULL;
  current_wait = NULL;
  fclose (fp);

  return iproc+1;
}

void start_procedure (n)
int n;
{
  current_pd = procedures[n].d;
}

int get_proc_index (name)
char *name;
{
    register int i;

    if (!name)
	return -1;

    for (i = 0 ; i <= iproc ; i++)
	if (!strcmp (procedures[i].nom, name))
	    return i;

    return -1;
}

void run_procedure (c, fd)
char c;
int fd;
{
    char *p;

    while (current_pd) {

	if (current_pd->type == P_WAIT) {
	    /* Attente en cours ? */
	    if (!current_wait)
		current_wait = (current_pd->p).attente;
	
	    if (c == *current_wait) {
		current_wait++;
		if (*current_wait == 0) {
		    current_wait = NULL; /* Ok, continue la procedure */
		}
		else
		    return;
	    }
	    else {
		current_wait = NULL; /* No good ==> RAZ */
		return;
	    }
	}
	else {
	    switch (current_pd->type) {

	      case P_TRAN :

		  p = (current_pd->p).emission;
		  while (*p) {
		      if ((*p == '\\') && (*(p+1) >= 'A' && *(p+1) <= 'I')) {
			  /* Commande ENVOI, etc... */
			  write (fd, "\023", 1);
			  write (fd, p+1, 1);
			  p += 2;
		      }
		      else
			  write (fd, p++, 1);
		  }

		break;

	      case P_REC :

		if ((current_pd->p).state)
		    demarre_enregistrement (NULL, NULL, NULL);
		else
		    arrete_enregistrement (NULL, NULL, NULL);
		break;

	      case P_WRI :

		strcpy (nom_fichier_sauve, (current_pd->p).filename);
		Sauve ();
		break;

	      case P_HUP :

		write (fd, CHAINE_COMMANDE_FIN, 1);
		break;

	      case P_EXI :
		ce_n_est_qu_un_au_revoir ();
		break;

	      default :
		break;

	    }
	}

	current_pd = current_pd->suivant;
    }
}

void display_procedures ()
{
  register int i;
  register struct d *pd;

  for (i = 0 ; i <= iproc ; i++ ) {
    printf ("procedure [%s] service [%s] :\n", procedures[i].nom, procedures[i].service);

    for (pd = procedures[i].d ; pd ; pd = pd->suivant) {
      switch (pd->type) {

	case P_WAIT :

	  printf ("\tP_WAIT [%s]\n", (pd->p).attente);
	break;

	case P_TRAN :

	  printf ("\tP_TRAN [%s]\n", (pd->p).emission);
	break;

	case P_REC :

	  printf ("\tP_REC [%d]\n", (pd->p).state);
	break;

	case P_WRI :

	  printf ("\tP_WRI [%s]\n", (pd->p).filename);
	break;

	case P_HUP :

	  printf ("\tP_HUP\n");
	break;

	case P_EXI :

	  printf ("\tP_EXI\n");
	break;

	default:
	  break;
      }
    }
  }
}



