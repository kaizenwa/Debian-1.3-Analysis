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
static char rcsid[] = "$Id: lecteur.c,v 1.3 1996/09/22 21:10:26 pierre Exp $";

/*
 * Fonctions du lecteur
 */

#include "xtel.h"
#include <errno.h>
#include <signal.h>

static XtInputId input_id_lecteur;
static char *passage_videotex = "\033[?{";
static char indice_reception;
static int fd_fichier_lu;
static Boolean flag_arret_a_decoder, flag_sep;

/* 
 * Stop automatique, en cas de fin de fichier, fin de lecteur, etc... 
 */
void stop_rapide () 
{
#ifdef DEBUG
    printf ("stop rapide !\n");
#endif
    
    affiche_compteur_lecteur_caractere ();

    /* Passe en stop et note la position */
    lecteur_play = 0;
    position_courante_fichier = lseek (fd_fichier_lu, 0L, 1);
    XtRemoveInput (input_id_lecteur);
    if (mode_emulation != MODE_VIDEOTEX)
	input_id_teleinfo = XtAppAddInput(app_context, fd_teleinfo, (XtPointer)XtInputReadMask, (XtInputCallbackProc)fonction_lecture_teleinfo, NULL);
    mise_a_jour_mode_emulation (0);

    /* active le bouton stop */
    activation_bouton_stop ();
}

/*
 *  Fonction de lecture du fichier 
 */   

/* ARGSUSED */
void fonction_lecture_fichier_courant (client_data, fid, id)
XtPointer client_data;	/* non utilise */
int *fid;
XtInputId *id;
{
    char c;

    if (read (*fid, &c, 1) == 1) {
	
	/* Test de la commande SUITE */
	if (flag_sep && c != 'H')
	    flag_sep = False;
	else if ((type_de_fin_page & ARRET_SUITE) && c == 19)
	    flag_sep = True;

	/* Test de fin de page */
	if ((c == 0x0c && (type_de_fin_page & ARRET_FF)) || (c == 'H' && flag_sep && (type_de_fin_page & ARRET_SUITE))) {
	    /* On s'arrete ? */
	    if (flag_arret_a_chaque_page) {
		stop_rapide ();
		position_courante_fichier = lseek (fd_fichier_lu, 0L, 1);
		flag_arret_a_decoder = (c == 0x0c ? True : False);
		flag_sep = False;

		return;
	    }
	    else {
		numero_page_courante++;
		affiche_compteur_lecteur_page ();
	    }
	}

	if (mode_emulation != MODE_VIDEOTEX) {
	    if (*(passage_videotex+indice_reception) == c) {
		if (indice_reception == 3) {
		    mode_emulation = MODE_VIDEOTEX;
		    kill (pid_teleinfo, SIGKILL);
		    close (fd_teleinfo);

		}
		else 
		    indice_reception = (indice_reception == 3 ? 0 : indice_reception++);
	    }   
	    
	    if (mode_emulation == MODE_TELEINFO_FR)
		c = conversion_teleinfo_fr (c);

	    write (fd_teleinfo, &c, 1);
	}
	else {
	    /* decodage */
	    videotexDecode (ecran_minitel, c);
	}

	/* caractere suivant */
	numero_caractere_courant++;
	if ((numero_caractere_courant % 100) == 0)
	    affiche_compteur_lecteur_caractere ();
    }
    /* fin du fichier */
    else {
	stop_rapide ();
	position_courante_fichier = lseek (fd_fichier_lu, 0L, 1);
    }
}

/*
 * Callback du bouton "quitte_lecteur"
 */

void fin_lecteur (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    if (lecteur_play) {
	stop_rapide ();
    }

    position_courante_fichier = lseek (fd_fichier_lu, 0L, 1);
#ifdef DEBUG
    printf ("position avant quit = %ld\n", position_courante_fichier);
#endif
    close (fd_fichier_lu);
    fd_fichier_lu = 0;

    efface_lecteur ();
    composition_valide (True);
}

/*
 * Callback du bouton "play_lecteur"
 */

void Play_lecteur (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    if (!lecteur_play) {

#ifdef DEBUG
	printf ("Play\n");
#endif

	/*
	 * On vient de s'arreter sur FF ou SUITE
	 */
	if (flag_arret_a_decoder) {
	    /* Arret sur FF */
	    if (type_de_fin_page & ARRET_FF)
		videotexDecode (ecran_minitel, 0x0c);
	    numero_page_courante++;
	    numero_caractere_courant++;
	    affiche_compteur_lecteur_caractere ();
	    affiche_compteur_lecteur_page ();
	    flag_arret_a_decoder = False;
	}

	lecteur_play = 1;
	if (mode_emulation != MODE_VIDEOTEX)
	    XtRemoveInput (input_id_teleinfo);

	mise_a_jour_mode_emulation (1);
    	input_id_lecteur = XtAppAddInput(app_context, fd_fichier_lu, (XtPointer)XtInputReadMask, (XtInputCallbackProc)fonction_lecture_fichier_courant, NULL);
    }
}

/*
 * Callback du bouton "stop_lecteur"
 */

void Stop_lecteur (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    if (lecteur_play) {

#ifdef DEBUG
	printf ("Stop\n");
#endif

	affiche_compteur_lecteur_caractere ();

	/* Passe en stop et note la position */
    	lecteur_play = 0;
	position_courante_fichier = lseek (fd_fichier_lu, 0L, 1);
	XtRemoveInput (input_id_lecteur);
	if (mode_emulation != MODE_VIDEOTEX)
	    input_id_teleinfo = XtAppAddInput(app_context, fd_teleinfo, (XtPointer)XtInputReadMask, (XtInputCallbackProc)fonction_lecture_teleinfo, NULL);
    }
}


/*
 * Callback du bouton "zero_lecteur"
 */

void Zero_lecteur (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
#ifdef DEBUG
    printf ("Zero\n");
#endif

    if (lecteur_play) {
	stop_rapide();
    }

    videotexInit (ecran_minitel);

    /* revient en debut */
    lseek (fd_fichier_lu, 0L, 0);
    position_courante_fichier = 0L;
    numero_page_courante = 1;
    numero_caractere_courant = 0;
    affiche_compteur_lecteur_caractere ();
    affiche_compteur_lecteur_page ();
}

/*
 * Callback du bouton "rewind_lecteur"
 */

void Rewind_lecteur (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    unsigned int numero_caractere = 0, numero_page = 1;
    char c;

#ifdef DEBUG
    printf ("Rewind\n");
#endif

    if (lecteur_play) {
	stop_rapide();
    }

    /* revient en debut */
    lseek (fd_fichier_lu, 0L, 0);

    /* Deja en page 1 ==> repointe le debut du fichier */
    if (numero_page_courante == 1) {
#ifdef DEBUG
	printf ("deja en page 1 !!!\n");
#endif
	numero_caractere_courant = 0;
	affiche_compteur_lecteur_caractere ();
	position_courante_fichier = 0L;
	flag_arret_a_decoder = False;

	return;
    }

    /*
     * Lecture du fichier jusqu'au FF ou SUITE precedant la
     * page courante
     */
    while (read (fd_fichier_lu, &c, 1) > 0) {

	if (flag_sep && c != 'H')
	    flag_sep = False;
	else if ((type_de_fin_page & ARRET_SUITE) && c == 19)
	    flag_sep = True;

	/* page suivante */
	if ((c == 0x0c && (type_de_fin_page & ARRET_FF)) || (c == 'H'  && flag_sep && (type_de_fin_page & ARRET_SUITE))) {
	    /* si l'on est sur la page precedant la page courante */
	    if (numero_page == numero_page_courante-1) {
		numero_page_courante--;
		numero_caractere_courant = numero_caractere;
		affiche_compteur_lecteur_page ();
		affiche_compteur_lecteur_caractere ();
 		position_courante_fichier = lseek (fd_fichier_lu, 0L, 1);
		flag_arret_a_decoder = (c == 0x0c ? True : False);

		return;
	    }   
	    numero_page++;
	}
	numero_caractere++;
    }
    
    /* 
     * il n'y a pas de FF ni SUITE dans la page ==> pointe le debut
     */
    position_courante_fichier = lseek (fd_fichier_lu, 0L, 0);	
    flag_arret_a_decoder = False;
}

/*
 * Callback du bouton "wind_lecteur"
 */

void Wind_lecteur (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    char c;
    unsigned int numero_sauve = numero_caractere_courant;

#ifdef DEBUG
    printf ("Wind\n");
#endif

    if (lecteur_play) {
	stop_rapide();
    }

    /* 
     * Lecture du fichier --> prochain FF ou SUITE 
     */
    while (read (fd_fichier_lu, &c, 1) > 0) {
	numero_caractere_courant++;
	/*
	 * Si FF ou SUITE trouve, on stoppe car on est sur la nouvelle page courante
	 */
	if (flag_sep && c != 'H')
	    flag_sep = False;
	else if ((type_de_fin_page & ARRET_SUITE) && c == 19)
	    flag_sep = True;

	/* page suivante */
	if ((c == 0x0c && (type_de_fin_page & ARRET_FF)) || (c == 'H'  && flag_sep && (type_de_fin_page & ARRET_SUITE))) {
	    numero_page_courante++;
	    affiche_compteur_lecteur_page ();
	    position_courante_fichier = lseek (fd_fichier_lu, 0L, 1);
	    flag_arret_a_decoder = (c == 0x0c ? True : False);
	    return;
	}
    }
    
#ifdef DEBUG
    printf ("derniere page, revient en %ld !\n", position_courante_fichier);
#endif

    lseek (fd_fichier_lu, position_courante_fichier, 0);
    numero_caractere_courant = numero_sauve;
    flag_arret_a_decoder = False;
}



/*
 * Remise a zero du lecteur :
 *
 *	- ouvre le nouveau fichier si le lecteur est affiche
 *	- position_courante = debut
 *	- init compteurs
 */

void raz_lecteur ()
{
#ifdef DEBUG
    printf ("raz lecteur\n");
#endif

    /* on est en train de lire ==> stop !!! */
    if (lecteur_play) {
	stop_rapide();
    }

    /* debut du fichier */
    numero_page_courante = 1;
    numero_caractere_courant = 0;
    position_courante_fichier = 0L;

    /* si le lecteur est affiche ==> fichier en cours */
    if (fd_fichier_lu > 0) {

	/* ferme le fichier */
	close (fd_fichier_lu);

	/* ouvre le nouveau */
	if ((fd_fichier_lu = open (nom_fichier_charge, O_RDONLY)) < 0) {
	    affiche_erreur (xtel_basename(nom_fichier_charge), errno);
	    return;
	}

	/* init du compteur */
	affiche_compteur_lecteur_caractere ();
	affiche_compteur_lecteur_page ();
    }
    else 
      ;/* le fichier sera ouvert par "debut_lecteur()" */
}

/*
 * Ouverture du lecteur
 */

void debut_lecteur ()
{
    if ((fd_fichier_lu = open (nom_fichier_charge, O_RDONLY)) < 0) {
	affiche_erreur (xtel_basename(nom_fichier_charge), errno);
	return;
    }

    XtVaSetValues (ecran_minitel, XtNfdConnexion, fd_fichier_lu, NULL);

#ifdef DEBUG
    printf ("positionne en %ld\n", position_courante_fichier);
#endif

    lseek (fd_fichier_lu, position_courante_fichier, 0);

    /*
     * Affiche le lecteur 
     */
    affiche_lecteur ();

    affiche_compteur_lecteur_caractere ();
    affiche_compteur_lecteur_page ();

    composition_valide (False);
}

/*
 * Callback des boutons "oui/non_arret_page_lecteur"
 */
#if 0
void arret_a_chaque_page (w, flag, call_data)
Widget w;
Boolean flag;
XtPointer call_data;
{
    flag_arret_a_chaque_page = flag;
}

void type_fin_page (w, type, state)
Widget w;
int type;
Boolean state;
{
    if (state)
	type_de_fin_page |= type;
    else
	type_de_fin_page &= ~type;
}
#endif
