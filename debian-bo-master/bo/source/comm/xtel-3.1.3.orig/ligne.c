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
static char rcsid[] = "$Id: ligne.c,v 1.8 1997/02/11 13:22:54 pierre Exp $";

/*
 * fonctions de traitement de la ligne MODEM
 */

#include "xtel.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

#include "Timer.h"

#ifdef NO_NETWORK
#include <sys/socket.h>
#include <sys/un.h>
#endif /* NO_NETWORK */

static char erreur, compte_erreur, message_erreur[80];
static char buf_temps_maxi[4];
static int temps_maxi;
static XtIntervalId bip_id;

/* Signale la deconnexion dans 30 s */
static XtTimerCallbackProc bip_bip (w, id)
Widget w;
XtIntervalId *id;
{
  XKeyboardControl kb;
  XKeyboardState kbs;
  Display *display = XtDisplay(ecran_minitel);
  int pitch[3]    = {200, 400, 500};
  int duration[3] = { 50, 100, 100}, i;

  XGetKeyboardControl (display, &kbs);

  for (i = 0; i < 3; i++) {
	kb.bell_pitch = pitch[i];
	kb.bell_duration = duration[i];
	XChangeKeyboardControl(display, KBBellPitch | KBBellDuration, &kb);
  
	XBell(display, 100);
	XFlush(display);
	usleep(100000L);
  }

  kb.bell_pitch = kbs.bell_pitch;
  kb.bell_duration = kbs.bell_duration;

  XChangeKeyboardControl (display, KBBellPitch | KBBellDuration, &kb);
}

/* Pour quitter... */
void ce_n_est_qu_un_au_revoir ()
{
  if (mode_emulation != MODE_VIDEOTEX)
    zigouille_xterm_teleinfo ();

  XFlush (XtDisplay(ecran_minitel));
  printf ("Bye...\n");
  exit (0);
}

/* 
 * Enregistre un caractere dans la zone d'enregistrement 
 */
void enregistre_caractere (w, client_data, s)
Widget w;
XtPointer client_data;
char *s;
{
    if (flag_enregistrement) {
	/*
	 * Si il ne reste plue de place dans la zone 
	 * d'enregistrement, alloue 1000 octets de +
	 */
	if (cpt_buffer && !(cpt_buffer % 1000)) {
	    taille_zone_enregistrement += 1000;
	    zone_enregistrement = realloc (zone_enregistrement, taille_zone_enregistrement);
	}
	
	*(zone_enregistrement+cpt_buffer) = *s;
	cpt_buffer++;
    }
}

/*
 * fonction de lecture de la ligne
 */

/* ARGSUSED */
void fonction_lecture_ligne(client_data, fid, id)
XtPointer client_data;	/* non utilise */
int *fid;
XtInputId *id;
{
    unsigned char c;
    Boolean flag_connexion;
    int n;

    XtVaGetValues (ecran_minitel, XtNconnecte, &flag_connexion, NULL);

    n = read (*fid, &c, 1);

    if (flag_connexion) {
	/* Deconnexion supposee */
	if (n <= 0) {
	    XtVaSetValues (ecran_minitel, XtNconnecte, False, NULL);
#ifndef LOW_MEMORY
	    if (!low_memory) {
		timerResetTimer (timer_minitel);
		XtVaSetValues (timer_minitel, XtNtimerMode, False, NULL);
	    }
#endif
	    flag_connexion = False;
	    
	    /* invalide les boutons */
	    clavier_minitel_valide (False);
	    
	    /* valide la composition et le lecteur */
	    composition_valide (True);
	    
	    if (nom_fichier_charge[0] != 0)
		lecteur_valide (True);
	    
	    XFlush (XtDisplay(ecran_minitel));
	    
	    XtVaSetValues (ecran_minitel, XtNfdConnexion, -1, NULL);
	    XtRemoveInput (input_id);

	    if (bip_id) {
		XtRemoveTimeOut (bip_id);
		bip_id = 0;
	    }

	    close (socket_xteld);
	    if (sortie_violente)
		ce_n_est_qu_un_au_revoir ();
	}
	else {
	    if (nb_procedures)
		run_procedure (c, socket_xteld);

	    enregistre_caractere (ecran_minitel, NULL, &c);
	    efface_telephone ();

	    /* decodage */
	    if (mode_emulation != MODE_VIDEOTEX)
	      write (fd_teleinfo, &c, 1);
	    else
	      videotexDecode (ecran_minitel, c);
	}
    } 
    /* Non connecte */
    else { 
	if (erreur) {
	    if (c == VALEUR_REPONSE_FIN_ERREUR) {

		erreur = 0;

		if (!flag_connexion) {
		    /* efface le petit telephone */
		    efface_telephone ();

		    /* valide la composition et le lecteur */
		    composition_valide (True);
		    if (nom_fichier_charge[0] != 0)
			lecteur_valide (True);

		    XtVaSetValues (ecran_minitel, XtNfdConnexion, -1, NULL);
		    XtRemoveInput (input_id);	
		    close (socket_xteld);	    
		}

		affiche_erreur (message_erreur, message_erreur[compte_erreur-1]);
	    }
	    else
		message_erreur[compte_erreur++] = c;
	}
	else {
	    if (c == VALEUR_REPONSE_CONNEXION) {
		XtVaSetValues (ecran_minitel, XtNconnecte, True, NULL);
#ifndef LOW_MEMORY
		if (!low_memory) {
		  if (temps_maxi > 0) {
		    XtVaSetValues (timer_minitel, XtNtimerMode, True, XtNcountDown, True, XtNmin, temps_maxi/60, XtNsec, temps_maxi%60, NULL);
		    temps_maxi = 0;
		  }
		  else
		    XtVaSetValues (timer_minitel, XtNtimerMode, True, XtNcountDown, False, XtNmin, 0, XtNsec, 0, NULL);
		  
		    timerResetTimer (timer_minitel);
		    timerStartTimer (timer_minitel);
		}
#endif
		efface_telephone ();
		flag_connexion = True;
	    
		/* valide les boutons MINITEL */
		clavier_minitel_valide (True);
		XFlush (XtDisplay(ecran_minitel));
	    }
	    else if (c == VALEUR_REPONSE_DEBUT_ERREUR) {
		erreur = 1;
		compte_erreur = 0;
	    }
	    /* VALEUR_TEMPS_MAXI, puis longueur + temps_maxi */
	    else if (c == VALEUR_TEMPS_MAXI) {
	      read (*fid, &c, 1);
	      read (*fid, buf_temps_maxi, c);
	      buf_temps_maxi[c] = 0;
	      temps_maxi = atoi(buf_temps_maxi);
	      if (temps_maxi > 30)
		bip_id = XtAppAddTimeOut (XtWidgetToApplicationContext(ecran_minitel), (unsigned long) (temps_maxi - 30) * 1000, (XtTimerCallbackProc) bip_bip, NULL);

	    }
	}
    }
}

/* 
 * Connexion a un service 
 */
void connexion_service (service)
char *service;
{
    char l;
    struct passwd *pw;

#ifdef DEBUG
    printf ("connexion au service %s\n", service);
#endif

    /* affiche le petit telephone */
    affiche_telephone ();

    /* invalide la composition et le lecteur */
    composition_valide (False);
    lecteur_valide (False);

    /* Ouvre la socket serveur */
#ifdef NO_NETWORK
    if ((socket_xteld = socket (AF_UNIX, SOCK_STREAM, 0))  < 0) {
	perror ("socket");
	exit (1);
    }

    if (connect (socket_xteld, (struct sockaddr *)&unaddr, sizeof(unaddr)) < 0) {
	perror ("connect");
	exit (1);
    }
#else
    if ((socket_xteld = c_clientbyname(rsc_xtel.serveur, rsc_xtel.nomService)) < 0) {
      fprintf (stderr, "Erreur de connexion au serveur XTEL...\n");
      return;
    }
#endif /* NO_NETWORK */

    /* Transmet le nom d'utilisateur */
    if ((pw = getpwuid(getuid())) == NULL) {
	perror ("getpwuid");
	return;
    }

    l = strlen (pw->pw_name);
    write (socket_xteld, &l, 1);
    write (socket_xteld, pw->pw_name, l);

    /* nouvelle entree = socket XTELD */
    XtVaSetValues (ecran_minitel, XtNfdConnexion, socket_xteld, NULL);
    input_id = XtAppAddInput(app_context, socket_xteld, (XtPointer)XtInputReadMask, (XtInputCallbackProc)fonction_lecture_ligne, NULL);

    /* commande de connexion a XTELD */
    if (!service) {
	write (socket_xteld, CHAINE_COMMANDE_CONNEXION_M1, 1);
    }
    else {
	write (socket_xteld, CHAINE_COMMANDE_DEMANDE_CONNEXION, 1);
	l = strlen (service);
	write (socket_xteld, &l, 1);
	write (socket_xteld, service, l);
    }
}
