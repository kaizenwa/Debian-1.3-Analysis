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
static char rcsid[] = "$Id: bouton.c,v 1.6 1996/10/28 13:35:41 pierre Exp $";

/*
 * fonctions associees aux boutons
 */

#include "xtel.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>

/*
 * Evenements actives lorsque la souris entre/sort dans/d'un widget
 */
void evenement_entre_widget (w, client_data, pevent)
Widget w;
char *client_data;
XEvent *pevent;
{
    /* fixe le focus sur le widget courant */
    XSetInputFocus (XtDisplay (w), XtWindow (w), RevertToNone, CurrentTime);
}

/*
 * fonction associe aux boutons minitel
 */
void commandes (w, code, call_data)
Widget w;
char *code;
XtPointer call_data;
{
    Boolean flag_connexion;
#ifdef DEBUG
    printf ("Code envoye : %c\n", *code);
#endif

    XtVaGetValues (ecran_minitel, XtNconnecte, &flag_connexion, NULL);

    if (nb_services == 0 && !flag_connexion) {
	connexion_service (NULL);
    }
    else {
	write (socket_xteld, "\023", 1);
	write (socket_xteld, code, 1);
	enregistre_caractere (ecran_minitel, NULL, "\023");
	enregistre_caractere (ecran_minitel, NULL, code);
    }
}

void raccrocher (w, code, call_data)
Widget w;
char *code;
XtPointer call_data;
{
    Boolean flag_connexion;

    XtVaGetValues (ecran_minitel, XtNconnecte, &flag_connexion, NULL);

    if (flag_connexion)
	write (socket_xteld, CHAINE_COMMANDE_FIN, 1);
}

/*
 * Chargement/sauvegarde d'un enregistrement
 */

char *xtel_basename (s)
char *s;
{
    register int i;

    for (i = strlen(s)-1 ; s[i] != 0 && s[i] != '/' && i >= 0 ; i--);

    return (s+i+1);
}


void Charge ()
{
    static struct stat statb;

    if (strlen(nom_fichier_charge) == 0)
      return;

    if (stat (nom_fichier_charge, &statb) < 0) {
	affiche_erreur (xtel_basename(nom_fichier_charge), errno);
	return;
    }

    /* Prepare l'ecran */
    videotexInit (ecran_minitel);
    videotexDecode (ecran_minitel, 0x0c);

    /* Lecteur */
    raz_lecteur ();
    lecteur_valide (True);
}

void Sauve()
{
  int fd;

#ifdef DEBUG
    printf ("Sauvegarde de %d caracteres\n", cpt_buffer);
#endif

    if (strlen(nom_fichier_sauve) == 0)
      return;

    printf ("Sauvegarde de %d caracteres\n", cpt_buffer);

    unlink (nom_fichier_sauve);

    if ((fd = open (nom_fichier_sauve, O_WRONLY | O_CREAT, 0666)) < 0) {
	affiche_erreur (xtel_basename(nom_fichier_sauve), errno);
	return;
    }

    if (write (fd, zone_enregistrement, cpt_buffer) != cpt_buffer) {
	affiche_erreur ("write", errno);
    }

    close (fd);
}

/*
 * Fixe l'acces au menu de composition
 */

void composition_valide (flag)
int flag;
{
    if (nb_services > 0)  {
	XtSetSensitive (bouton_compose, flag);
	if (nb_procedures != 0)
	    XtSetSensitive (bouton_procedure, flag);
#ifndef LOW_MEMORY
	if (!low_memory && btn_direct)
	    XtSetSensitive (btn_direct, flag);
#endif /* LOW_MEMORY */
    }
}

/*
 * Fixe l'acces au popup-menu des touches MINITEL
 */
void clavier_minitel_valide (flag)
int flag;
{
    register int i;
    Widget w;
    char buf[256];

    /* Valide la sensibilite du clavier Minitel */
    for (i = 4; i < 14 ; i++) {
	if ((w = XtNameToWidget (menu_clavier, touches[i].nom)) == NULL) {
	    printf ("erreur XtNameToWidget %s\n", touches[i].nom);
	    return;
	}

	if (nb_services == 0 && i == 13)
	    XtSetSensitive (w, True);
	else
	    XtSetSensitive (w, flag);

#ifndef LOW_MEMORY
	if (!low_memory) {
	    /* Boutons icones */
	    if (strcmp (touches[i].nom, "blanc") != 0) {
		sprintf (buf, "btn_%s", touches[i].nom);
		if ((w = XtNameToWidget (boite_bas, buf)) == NULL) {
		    printf ("erreur XtNameToWidget %s\n", buf);
		    return;
		}

		if (nb_services == 0 && i == 13)
		    XtSetSensitive (w, True);
		else
		    XtSetSensitive (w, flag);
	    }
	}
#endif /* LOW_MEMORY */    
    }

    if (rsc_xtel.boutonRaccrocher) {
	if ((w = XtNameToWidget (menu_clavier, "raccrocher")) == NULL) {
	    printf ("erreur XtNameToWidget raccrocher\n");
	    return;
	}
	
	XtSetSensitive (w, flag);
    }

    /* Protocoles */
    if (nb_protocoles) 
	XtSetSensitive (bouton_proto, flag);
}

/* Fixe la partie mode d'emulation */

void mise_a_jour_mode_emulation (flag_invalide)
char flag_invalide;
{
    register int i;
    Widget w;

    for (i = 0 ; i != 3; i++) {
	if ((w = XtNameToWidget (menu_clavier, touches[i].nom)) == NULL) {
	    printf ("erreur XtNameToWidget %s\n", touches[i].nom);
	    return;
	}

	if (i == mode_emulation || flag_invalide)
	    XtSetSensitive (w, False);
	else
	    XtSetSensitive (w, True);
    }
}


/* Traitement de l'enregistrement */
void demarre_enregistrement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  if (!flag_enregistrement) {
    flag_enregistrement = 1;
    XtSetSensitive (bouton_demarre, False);
    XtSetSensitive (bouton_arrete, True);
#ifndef LOW_MEMORY
    if (!low_memory)
	XtMapWidget (la_cassete);
#endif
  }
}

void arrete_enregistrement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  if (flag_enregistrement) {
    flag_enregistrement = 0;
    XtSetSensitive (bouton_demarre, True);
    XtSetSensitive (bouton_arrete, False);
#ifndef LOW_MEMORY
    if (!low_memory)
	XtUnmapWidget (la_cassete);
#endif
  }
}

void change_enregistrement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    if (flag_enregistrement)
	arrete_enregistrement (w, NULL, NULL);
    else
	demarre_enregistrement (w, NULL, NULL);
}

void nouvel_enregistrement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    cpt_buffer = 0;
    taille_zone_enregistrement = 1000;
    zone_enregistrement = realloc (zone_enregistrement, taille_zone_enregistrement);
}
