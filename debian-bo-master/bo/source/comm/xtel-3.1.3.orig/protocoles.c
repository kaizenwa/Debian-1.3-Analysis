/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1995  Lectra Systemes & Pierre Ficheux
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
static char rcsid[] = "$Id: protocoles.c,v 1.1 1996/09/14 22:10:31 pierre Exp $";

/*
 * Gestion des protocoles de telechargement
 */
#include "xtel.h"
#include <signal.h>
#include <string.h>

static int pid_tele;

static void waitchild ()
{
    int r;

    wait (&r);
    input_id = XtAppAddInput(app_context, socket_xteld, (XtPointer)XtInputReadMask, (XtInputCallbackProc)fonction_lecture_ligne, NULL);
}


/*
 * Initialise un protocole de telechargement
 */
init_protocoles (nom, proto)
char *nom;
struct protocole *proto;
{
    FILE *fp;
    register int i;
    char buf[256];

    i = 0;

    if ((fp = fopen (nom, "r")) != NULL) {
	while (fgets (buf, sizeof(buf), fp) != NULL && i < MAXPROTO) {	
	    if (buf[0] == '#' || buf[0] == '\n')
		continue;
	    buf[strlen(buf)-1] = 0;

	    proto->nom = strdup (next_token (buf, ","));
	    proto->commande = strdup (next_token (NULL, ","));
#ifdef DEBUG
	    printf ("Protocole %s, commande = %s\n", proto->nom, proto->commande);
#endif
	    i++; proto++;
	}
    }
    
    return i;
}

/*
 * Callback du bouton "Interrompre"
 */
void
Stop_telechargement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    kill (pid_tele, SIGTERM);
    XtSetSensitive (stop_tele, False);
}

/*
 * Callback du menu de telechargement
 */
void
Selection_telechargement (w, proto, call_data)
Widget w;
int proto;
XtPointer call_data;
{
    signal (SIGCHLD, waitchild);

    XtRemoveInput (input_id);
    XtSetSensitive (stop_tele, True);
    pid_tele = fork ();

    if (pid_tele < 0) {
	perror ("Selection_telechargement");
	exit (1);
    }
    else if (pid_tele == 0) { /* fils */
	dup2 (socket_xteld, 0);
	dup2 (socket_xteld, 1);

	if (execlp (protocoles[proto].commande, protocoles[proto].commande, NULL) < 0) {
	    perror ("Selection_telechargement: execlp");
	    exit (1);
	}
    }
}
