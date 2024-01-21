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
static char rcsid[] = "$Id: xteld.c,v 1.25 1997/02/21 17:02:42 pierre Exp $";

/*
 * Demon XTELD (communication avec le MODEM)
 */

/* 	  
 * Contributions:
 *
 *   Michel Fingerhut	IRCAM Paris
 *		 
 *	- Traitement du fichier de log
 *	- Acces proteges aux services
 *
 *   Pierre Beyssac	SYSECA
 *
 *	- traitement du Minitel 2
 *	- utilisation de syslog
 *
 *   Vincent Gillet
 *
 *	- dialogue avec client Window$ (option -w)
 *
 *   Eric Delaunay
 *
 *	- support IAN
 *
 */

#define EXTERN

#include <stdio.h>
#include <ctype.h>
#include "demon.h"
#include "globald.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <signal.h>
#include <string.h>
#ifdef USE_SYSLOG
#include <syslog.h>
#endif /* USE_SYSLOG */

#ifdef NO_TERMIO
#include <sgtty.h>
#else
#ifdef USE_TERMIOS
#include <sys/ioctl.h>
#include <termios.h>
#else
#include <termio.h>
#endif /* USE_TERMIOS */
#endif /* NO_TERMIO */

#ifdef NO_NETWORK
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#ifdef sun
#include <sys/termios.h>
#endif /* sun */

static int sock_service;

#define XTELD_INPUT	sock_service
#define XTELD_OUTPUT	sock_service

#else

#include <netinet/in.h>
#define XTELD_INPUT	0
#define XTELD_OUTPUT	1

#endif /* NO_NETWORK */

/* Tableau pour Emulateur sous Windaube */
static char tablo_windaube[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  36,
			 37, 102, 103,  36,  37, 102, 103,  60,
			 61, 126, 127,  60,  61, 126, 127,  60,
			 61, 126, 127,  60,  61, 126, 127,  60,
			 61, 126, 127,  60,  61, 126, 127,  98,
			 99,  98,  99, 102, 103, 102, 103, 122,
			123, 122, 123, 126, 127, 126, 127, 122,
			123, 122, 123, 126, 127, 126, 127, 122,
			123, 122, 123, 126, 127, 126, 127, 102,
			103, 102, 103, 102, 103, 102, 103, 126,
			127, 126, 127, 126, 127, 126, 127, 126,
			127, 126, 127, 126, 127, 126, 127, 126,
			127, 126, 127, 126, 127, 126};

static int fin_fils;
static int pid_fils;
static int nb_services;
static fd_set a_lire;
static struct timeval timeout;
static time_t t_connexion;
static char buf[256], buf1[256], service[256], utilisateur[256];
static char flag_connexion;
static char parite;
static Boolean flag_serveur_local, flag_crlf, flag_windaube;
static int tuyau_in[2], tuyau_out[2];
static int temps_maxi;
static int detecte_ian;
static int saisie_active;
static char fin_connexion = 0;

/* Syslog or not syslog ? */
#ifdef USE_SYSLOG
void log_debug (fmt, p1, p2, p3, p4, p5, p6, p7)
char *fmt;
int  p1, p2, p3, p4, p5, p6, p7;
{
    char msg[256];

    sprintf (msg, fmt, p1, p2, p3, p4, p5, p6, p7);
    syslog(LOG_DEBUG, msg);
}

void log_err (s)
char *s;
{
    syslog(LOG_ERR, s);
}
#else
void log_debug (fmt, p1, p2, p3, p4, p5, p6, p7)
char *fmt;
int  p1, p2, p3, p4, p5, p6, p7;
{
    fprintf (fp_console, "xteld[%d] ", getpid());
    fprintf (fp_console, fmt, p1, p2, p3, p4, p5, p6, p7);
    fprintf (fp_console, "\n\r");
}
#endif /* USE_SYSLOG */

static void demande_fin_fils()
{
#ifdef DEBUG_XTELD
    log_debug ("Demande la fin du fils");
#endif
    signal (SIGALRM, SIG_IGN);
    fin_fils = 1;
}

/*
 * Transmet une erreur a XTEL 
 *	- chaine de caractere terminee par 0
 *	- errno associe
 */
void erreur_a_xtel (s, code_erreur)
char *s;
int code_erreur;
{
    char e = code_erreur;

    write (XTELD_OUTPUT, CHAINE_REPONSE_DEBUT_ERREUR, 1);
    write (XTELD_OUTPUT, s, strlen(s));
    write (XTELD_OUTPUT, "\0000", 1); 
    write (XTELD_OUTPUT, &e, 1);
    write (XTELD_OUTPUT, CHAINE_REPONSE_FIN_ERREUR, 1);
}
/* 
 * Teste si l'utilisateur courant a acces au service demande
 *
 *	1 si oui
 *	0 sinon
 */
int
service_autorise (indice_service, maxtime) 
int indice_service, *maxtime;
{    char *pt, *pt1, *auth = definition_services[indice_service].autorisations;
     char autorisations[2048];
     int default_maxtime = 0;

     *maxtime = 0;

     /* Pas de protection ou definition general du temps maxi */
     if (*auth == 0)
       return 1;

     if (*auth == '=' && !strchr (auth, ':')) {
       *maxtime = atoi (auth+1);
       return 1;
     }

     strcpy (autorisations, auth);
     pt = strtok (autorisations, ":");
     while (pt != NULL) {
       /* 
	* Format: nom=temps_maxi (si =temp_maxi, le temps est la valeur 
	* par defaut)
	*/
       *maxtime = default_maxtime;

       if (*pt == '=') {
	 /* nouvelle valeur par defaut */
	 *maxtime = default_maxtime = atoi (pt+1);
       }
       else {
	 if ((pt1 = strchr (pt, '='))) {
	   *pt1 = 0;
	   *maxtime = atoi (pt1+1);
	 }

	 if (strcmp (pt, utilisateur) == 0) {
           return(1);
	 }
       }

       pt = strtok(NULL, ":");
     }
     return(0);
}

/*
 * Fonction d'autorisation d'envoi de caractères au modem
 * (quand connexion restreinte à un service Teletel)
 */
static void active_saisie ()
{
#ifdef DEBUG_XTELD
    log_debug( "saisie autorisée" );
#endif
    saisie_active = 1;
    signal( SIGUSR1, SIG_DFL );
}

/*
 * Fonction de deconnexion (appelee sur SIGCHLD)
 */
static void waitchild ()
{
    int r;

    wait (&r);
}

static void deconnexion ()
{
    FILE *fplog;

#ifdef DEBUG_XTELD
    log_debug ("Deconnecte !");
#endif

    flag_connexion = 0;

    if (!flag_serveur_local) {

	if (definition_lignes[numero_ligne].type_dialer != DIALER_MODEM) {

	    /*
	     * Envoi du code de connexion/fin puis de la sequence
	     * de raccrochage
	     */
#ifdef DEBUG_XTELD
	    log_debug ("Raccrochage Minitel");
#endif
	    write (fd_modem, "\x13I\x1b\x39\x67", 5);
	}

	myundial (fd_modem);

    }
    else
	kill (pid_fils, SIGTERM);

    /* signal a XTEL la deconnexion */
    write (XTELD_OUTPUT, CHAINE_REPONSE_DECONNEXION, 1);

    /* supprime le fichier de log */
    sprintf (buf, "/tmp/.xtel-%s", utilisateur);
    unlink (buf);
	
    if ((fplog= fopen(FICHIER_LOG, "a")) != NULL) {
	long t= time(0), duree;
	char *at= ctime(&t);
	at[24]= '\000';

	duree = (t_connexion == 0 ? 0L : t-t_connexion);
	fprintf(fplog, "%s, %s deconnexion de : %s (%ld s sur %s)\n", at, utilisateur, service, duree, definition_lignes[numero_ligne].nom);
	fclose(fplog);
    }

    waitchild ();
    fin_connexion++;
}

/* Test de la mort subite de Xtel (errno == 2) en cours de connexion */
static void test_mort_subite ()
{
    if (errno == SIGINT && flag_connexion) {
	kill (pid_fils, SIGTERM);
	deconnexion ();
    }
}    

/* Test chaine = nombre */
static is_number (s)
char *s;
{
    register char *p = s;

    if (!s)
	return 0;

    while (*p) {
	if (!isdigit(*p))
	    return 0;

	p++;
    }

    return 1;
}

/* Separation de chaine */
static char *separe_chaine (s, c)
char *s;
char c;
{
    register char *p = s;
    char *s1;
    
    while (*p) {
	if (*p == c) {
	    s1 = strdup (p+1);
	    *p = 0;
	    return s1;
	}

	p++;
    }

    return NULL;
}

/* Trouve le device associe a un numero de telephone */
static char *find_device (service)
char *service;
{
  register int i, idev;
  char *numero_direct;

  /* On precise le device "a la main" (ex: 1234,/dev/cua0) */
  numero_direct = strchr (service, ',');
	
  /* Nom du device associe au service */
  if (numero_direct) {
    *numero_direct++ = 0;
    return (numero_direct);
  }
  else {
    for (i = 0 ; i != nb_services ; i++) {
      if (!strcmp (definition_services[i].nom_service, "Direct"))
	  idev = i;
      if (!strcmp (definition_services[i].nom_uucp, service)) {
	idev = i;
	break;
      }
    }

    (void)service_autorise (idev, &temps_maxi);
#ifdef DEBUG_XTELD
      log_debug ("find_device: %s tmax %d", definition_services[idev].device, temps_maxi);
#endif
    return (definition_services[idev].device);
  }
}

/* 
 * Connexion a un service 
 */
void appel_service (service_teletel)
char *service_teletel;
{
    char *device_associe = NULL, *option = NULL;
    char *code_service = NULL;
    FILE *fplog;
    register int i;

    detecte_ian = 0;
    saisie_active = 0;

#ifdef DEBUG_XTELD
    log_debug ("connexion au service %s utilisateur %s", service_teletel, utilisateur);
#endif

    signal (SIGTERM, demande_fin_fils);

    /* Calcul du device associe */
    device_associe = find_device (service_teletel);

    /* Service local par "pipe" */
    if (device_associe != NULL && strncmp (device_associe, "@pipe", 5) == 0) {
	flag_serveur_local = True;

	/* Options du service local (pour l'instant "crlf") */
	if (strcmp (&device_associe[5], ":crlf") == 0)
	    flag_crlf = True;

	if (pipe (tuyau_in) < 0) {
#ifdef USE_SYSLOG
	    log_err ("pipe: tuyau_in: %m");
#else
	    fprintf (fp_console, "pipe: tuyau_in: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */
	    exit (0);
	}

	if (pipe (tuyau_out) < 0) {
#ifdef USE_SYSLOG
	    log_err ("pipe: tuyau_out: %m");
#else
	    fprintf (fp_console, "pipe: tuyau_out: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */
	    exit (0);
	}

	if (!fork()) { /* fiston */
#ifdef DEBUG_XTELD
	    log_debug ("le fiston = %d", getpid());
#endif
	    dup2 (tuyau_out[0], 0);
	    close (tuyau_out[0]);
	    dup2 (tuyau_in[1], 1);
	    close (tuyau_in[1]);

	    /* On recupere l'option eventuelle */
	    option = separe_chaine (service_teletel, ' ');

#ifdef DEBUG_XTELD
	    log_debug ("service_teletel %s option -%s-", service_teletel, option);
#endif

	    if (execlp (service_teletel, service_teletel, option, NULL) < 0) {
#ifdef USE_SYSLOG
		log_err ("execlp: %m");
#else
		fprintf (fp_console, "execlp: %s: %s\n", service_teletel, sys_errlist[errno]);
#endif /* USE_SYSLOG */
		exit (0);
	    }
	}
	else {
#ifdef DEBUG_XTELD
	    log_debug ("le papa = %d", getpid());
#endif
	    fd_modem = tuyau_in[0];
	}
    }
    else 
#ifndef NO_NETWORK
      /* Connexion TCP */
      if (device_associe != NULL && strncmp (device_associe, "@tcp", 4) == 0) {
	/* Recupere les parametres */
	option = separe_chaine (service_teletel, ':');
	if (is_number (option)) {
	  fd_modem = c_clientbyport (service_teletel, atoi(option));
	}
	else {
	  fd_modem = c_clientbyname (service_teletel, option);
	}
      }
    else
#endif
      { /* Connexion MODEM */
	/*
	 * sépare téléphone et code service (nnnn:code)
	 */
	char *telno = strdup(service_teletel);
	code_service = strchr( telno, ':' );
	if (code_service) {
	    *code_service = 0;
	    code_service = strchr( service_teletel, ':' ) + 1;
	    detecte_ian = IAN_DE_GARDE;	/* attente capture code d'entrée sur le service */
	    ian_init( telno );
	    signal( SIGUSR1, active_saisie );
	}
	else {
	    saisie_active = 1;
	}
	/*
	 * compose le numéro
	 */
#ifdef DEBUG_XTELD
	log_debug( "service appelé : %s [%s]", telno, code_service?code_service:"" );
#endif
	if ((fd_modem = mydial(telno, device_associe)) < 0) {
#ifdef DEBUG_XTELD
	    log_debug ("meurt (erreur)");
#endif
	    exit (1);
	}
	free(telno);
    }

    /* valide le signal de deconnexion */
    signal (SIGCHLD, deconnexion);
    
    /* Init masque select */
    FD_ZERO (&a_lire);	
    FD_SET (fd_modem, &a_lire);
	
#ifdef DEBUG_XTELD
    log_debug ("Connecte !");
#endif
	
    flag_connexion = 1;	/* On est connecte ! */
	
    /* Envoi du temps maxi de connexion */
    if (temps_maxi) {
      char l;

      write (XTELD_OUTPUT, CHAINE_TEMPS_MAXI, 1);
      sprintf (buf, "%d", temps_maxi);
      l = strlen(buf);
      write (XTELD_OUTPUT, &l, 1);
      write (XTELD_OUTPUT, buf, l);
    }

    /* signale la connexion a XTEL */
    write (XTELD_OUTPUT, CHAINE_REPONSE_CONNEXION, 1);

    /*
     * On cree un processus qui lit le modem et ecrit sur le reseau.
     * On tue ce processus au bout de DELAI_DECONNEXION secondes sans activite 
     * (ce qui fait raccrocher le MODEM)
     */
    
    if ((pid_fils = fork()) == 0) { /* fils */
	int etat = 0;
	int ignore = 0;
	int code_fin = 1;

	/* Duree maxi autorisee */
	if (!flag_serveur_local && temps_maxi > 0) {
          signal (SIGALRM, demande_fin_fils);
	  alarm (temps_maxi);
	}

	fin_fils = 0;
	while (!fin_fils) {
	    int nread, size, i;
	    
	    timeout.tv_sec = (unsigned long)DELAI_DECONNEXION;

	    nread = select (32, &a_lire, NULL, NULL, (flag_serveur_local ? NULL : &timeout));

	    if (nread < 0 && errno == EINTR) {
		/* select() interrompu par le SIGTERM du parent */
#ifdef DEBUG_XTELD
		log_debug ("select() interrompu par le SIGTERM du parent");
#endif
		continue;
	    }
	    
	    if (nread == 0 || !FD_ISSET (fd_modem, &a_lire)) {
		/* read = 0 (timeout) ==> deconnexion */
#ifdef DEBUG_XTELD
		log_debug ("read = 0 (timeout) ==> deconnexion");
#endif
		code_fin = 2;
		break;
	    }
	    
	    size = read (fd_modem, buf, sizeof(buf));

	    if (size <= 0) {
#ifdef DEBUG_XTELD
		log_debug ("size <= 0");
#endif
		code_fin = 3;
		break;
	    }
	    
	    /*
	     * termine la connexion lors du retour à la page
	     * du service Teletel (detecte_ian == 1)
	     * ou autorise le service (detecte_ian == 2).
	     */
	    if (detecte_ian) {
		int ian;

		for( i = 0 ; i < size ; i++ )
		    if (ian = ian_valide( detecte_ian, buf[i] )) {
			switch (detecte_ian) {
			case IAN_DE_GARDE:
#ifdef DEBUG_XTELD
			    log_debug( "Emission code du service : %s + ENVOI", code_service );
#endif
			    /* ecrit le code du service */
			    write ((flag_serveur_local ? tuyau_out[1] : fd_modem),
				    code_service, strlen(code_service) );
			    /* + touche ENVOI */
			    write ((flag_serveur_local ? tuyau_out[1] : fd_modem),
				    "\023A", 2 );
			    detecte_ian = IAN_D_ENTREE;  /* autorise le service */
			    break;
			case IAN_D_ENTREE:
			    if (ian == 1) {
#ifdef DEBUG_XTELD
				log_debug( "service activé. Envoi SIGUSR1 pid %d", getppid() );
#endif
				detecte_ian = IAN_DE_FIN;
				/*
				 * autorise le père à transmettre les actions
				 * au fils, maintenant que le service a été validé
				 */
				kill( getppid(), SIGUSR1 );
			    }
			    else {
				/* devrait faire une nouvelle tentative ? */
#ifdef DEBUG_XTELD
				log_debug( "accès refusé" );
#endif
				fin_fils = 1;	/* échec */
			    }
			    break;
			case IAN_DE_FIN:
#ifdef DEBUG_XTELD
			    log_debug( "retour à la page Teletel" );
#endif
			    fin_fils = 1;     /* termine la connexion */
			}
			break;
		    }
	    }

	    for (i = 0; i < size; i++) {
		if (definition_lignes[numero_ligne].cs != CS8)
		    buf[i] &= 0x7f;
		
		if (definition_lignes[numero_ligne].type_dialer != DIALER_MODEM) {
		    /*
		     * Detection de la sequence SEP 53 signifiant
		     * la fin de la connexion
		     */
		    switch (etat) {
		      case 0:
			if (buf[i] == 0x13) {
			    /*
			     * Remplacer par des 0 toutes les sequences
			     * SEP xx venant du minitel pour eviter
			     * des echos parasites.
			     */
			    etat = 1;
			    ignore = 2;
			}
			break;
		      case 1:
			if (buf[i] == 0x53)
			    /* Sequence SEP 53 reconnue */
			    fin_fils = 1;
			etat = 0;
			break;
		    }
		    if (ignore) {
			ignore--;
		    }
		}
	    }

	    /* Conversion LF --> CF/LF si serveur local ? */
	    if (flag_serveur_local && flag_crlf) {
		register int i, size1;

		size1 = 0;
		for (i = 0 ; i != size ; i++) {
		    if (buf[i] == '\n') {
			buf1[size1++] = '\n';
			buf1[size1++] = '\r';
		    }
		    else
			buf1[size1++] = buf[i];
		}

		write (XTELD_OUTPUT, buf1, size1);
	    }
	    else
		write (XTELD_OUTPUT, buf, size);
	}

	/* Sortie du fils */
#ifdef DEBUG_XTELD
	log_debug ("meurt, code = %d", code_fin);
#endif
	exit (0);
    }
    
    signal(SIGTERM, SIG_DFL);

    /* creation du fichier de log */
    sprintf (buf, "/tmp/.xtel-%s", utilisateur);
    if ((fplog = fopen (buf, "w"))) {
#ifdef DEBUG_XTELD
	log_debug ("Creation du fichier de log %s", buf);
#endif
	chmod (buf, 0644);
	if (!flag_serveur_local) 
	    fprintf (fplog, "LIGNE = %s\n", definition_lignes[numero_ligne].nom);
	fprintf (fplog, "PROCESSUS = %d,%d\n", getpid(), pid_fils);
	fprintf (fplog, "SERVICE = %s\n", service);
	fclose (fplog);
    }
    
    if ((fplog= fopen(FICHIER_LOG, "a")) != NULL) {
	char *at;
	
	t_connexion = time(0);
	at = ctime(&t_connexion);
	at[24]= '\000';
	
	if (flag_serveur_local)
	    fprintf(fplog, "%s, %s appel de : %s (local)\n", at, utilisateur, service);
	else
	    fprintf(fplog, "%s, %s appel de : %s sur %s\n", at, utilisateur, service, definition_lignes[numero_ligne].nom);
	fclose(fplog);
    }
#ifdef NO_TERMIO
    parite = definition_lignes[numero_ligne].parity;
#endif
}

int lire_chaine(ch)
char *ch;
{
    int ret;
    unsigned char l;

    do {
	ret = read (XTELD_INPUT, &l, 1);
	if (ret < 0) goto err;
    } while (ret != 1);

    ch[l] = 0;

    while (l) {
	ret = read (XTELD_INPUT, ch, l);
	if (ret < 0) goto err;
	l -= ret;
    }

    return 0;

err:
#ifdef USE_SYSLOG
    log_err ("read: %m");
#else
    fprintf (fp_console, "read: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */
    return -1;
}

/* 
 * Convertit la chaine en supprimant les caracteres accentues les
 * plus courants.
 */
static void write_chaine_sans_accent (s)
char *s;
{
    register unsigned char *p = s;
    char c;

    while (*p) {
	if (*p >= 0xe0 && *p <= 0xe5)
	    c = 'a';
	if (*p >= 0xe8 && *p <= 0xeb )
	   c = 'e';
	else if (*p >= 0xec && *p <= 0xef)
	    c = 'i';
	else if (*p >= 0xf2 && *p <= 0xf6)
	    c = 'o';
	else if (*p >= 0xf9 && *p <= 0xf9)
	    c = 'o';
	else if (*p == 0xe7)
	    c = 'c';
	else
	    c = *p;

	p++;
	write (XTELD_OUTPUT, &c, 1);
    }
}	

static void trop_tard ()
{
#ifdef USE_SYSLOG
    log_err ("Pas de reponse du client.\n");
#else
    fprintf (fp_console, "Pas de reponse du client.\n");
#endif /* USE_SYSLOG */
    exit (1);
}

/* La meme chose que "lire_chaine" appelee au lancement de xteld */
int lire_initial(ch)
char *ch;
{
    int ret,indice_service;
    char *ptr;

    /* Demande d'identification */
    signal (SIGALRM, trop_tard);
    alarm (20);
    write (XTELD_OUTPUT, "\x1B\x39\x7b", 3);
    read (XTELD_INPUT, service, 7);
    signal (SIGALRM, SIG_IGN);

    if ((!strncmp (service, "\x01SX1",4)) && service[6] == 4) {
	/* SX1x ==> Emulateur Windows */
	sprintf (buf, "\x1B\x39\x7A%c%c\x04", tablo_windaube[service[4]], tablo_windaube[service[5]]);
	write (XTELD_OUTPUT, buf, 6);
#ifdef DEBUG_XTELD
	log_debug ("Emulateur Windows, repond : %s", service);
	log_debug ("RAM = %s", buf);
#endif
	write (XTELD_OUTPUT, "\n\r Service : \r\n", 15);
	/* Pour l'instant, on definit un seul utilisateur Window$: "windaube" */
	strcpy (ch, "windaube");

	for (indice_service=0; indice_service != nb_services; indice_service++) {
	    if (service_autorise (indice_service, &temps_maxi)) {
		sprintf (buf, "%2d - %s\r\n", indice_service, definition_services[indice_service].nom_service);
		write_chaine_sans_accent (buf);
	    }
	}
	write (XTELD_OUTPUT, "Choisissez un numero : ", 23);
		
	ptr = buf-1;
	do {
	    ptr++;
	    ret = read (XTELD_INPUT, ptr, 1);
#ifdef DEBUG_XTELD
	    log_debug ("lu : %d", *ptr);
#endif
	    if (ret < 0 || ptr-buf > 5) goto err;
	    write (XTELD_OUTPUT, ptr,1);
	} while (*ptr != '\r');
	*ptr = '\000';

#ifdef DEBUG_XTELD
	log_debug ("lu : %s",buf);
#endif
	if (atoi(buf) > nb_services || ! service_autorise (atoi(buf), &temps_maxi)) {
	    sprintf (buf, "\r\n\r\n ** Choix interdit ! **\r\n");
	    write (XTELD_OUTPUT, buf, strlen(buf));
	    goto err;
	}

	/* Il semble que cela soit nécessaire... */
	write (XTELD_OUTPUT, "\x1B[?31l\x0A\x1B\x3A\x32\x7E\x0A", 12);
	write (XTELD_OUTPUT, "\x1B\x5B\x3F\x33\x32\x68", 6);
	write (XTELD_OUTPUT, "\x1B\x5B\x3F\x33\x33\x68", 6);

	/* nom du service */
	strcpy (service, definition_services[atoi(buf)].nom_uucp);

	return 1;
    }

    return 0;

err:
#ifdef USE_SYSLOG
    log_err ("read: %m");
#else
    fprintf (fp_console, "read: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */
    return -1;
}


/*
 *	Partie principale 
 */
main (ac, av)
int ac;
char **av;
{
    unsigned char c;
    register int i;
#ifdef NO_NETWORK
    int sock_ecoute, retry;
    static struct sockaddr_un unaddr;
    struct stat statb;
    int old_umask;
#endif /* NO_NETWORK */
    int indice_service = 0;

#ifdef USE_SYSLOG
    openlog("xteld", LOG_PID | LOG_CONS, LOG_DAEMON);
#else
    if ((fp_console = fopen ("/dev/console", "w")) == NULL) {
        perror ("/dev/console");
        exit (1);
    }
#endif /* USE_SYSLOG */

    i = 1;
    while (i < ac) {
	/* Option support Windaube */
	if (strcmp (av[i], "-w") == 0) {
#ifdef DEBUG_XTELD
	    log_debug ("Option Windaube active !");
#endif
	    flag_windaube = True;
	}

	i++;
    }

    if ((nb_lignes = lecture_configuration_lignes ()) <= 0) {
	exit (1);
    }

    if ((nb_services = lecture_services ()) < 0) {
	exit (1);
    }

#ifdef NO_NETWORK

    /* Test de l'existence de /tmp/.xtel */
    if (stat (XTEL_UNIX_PATH, &statb) == 0) {
	if (unlink (XTEL_UNIX_PATH) < 0) {
#ifdef USE_SYSLOG
	    log_err ("unlink: %m");
#else
	    fprintf (fp_console, "unlink: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */
	    exit (1);
	}
    }

    /* Je deviens un petit demon... */
    if (fork())
	exit (0);

    /*
     * Detachement du tty
     */

#if defined(SYSV) || defined(SVR4)
    setpgrp ();
#else
    setpgrp (0, getpid());
#endif

    close (0); 
    close (1);
    close (2);

#ifndef SYSV386
#if defined(SYSV) || defined(SVR4) || defined(linux)
    if ((i = open ("/dev/tty", O_RDWR | O_NOCTTY)) >= 0) {
#else
    if ((i = open ("/dev/tty", O_RDWR)) >= 0) {	
	(void) ioctl (i, TIOCNOTTY, (char *) 0);    /* detachement, BSD style */
#endif /* SYSV || SVR4 */
	(void) close (i);
    }
#endif /* !SYSV386 */

    /*
     * Creation/configuration de la socket d'ecoute
     */

    if ((sock_ecoute = socket (AF_UNIX, SOCK_STREAM, 0))  < 0) {
#ifdef USE_SYSLOG
	log_err("socket: %m");
#else
	fprintf (fp_console, "socket: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */
	exit (1);
    }

    old_umask = umask (0);
    unaddr.sun_family = AF_UNIX;
    strcpy (unaddr.sun_path, XTEL_UNIX_PATH);

    if (bind (sock_ecoute, (struct sockaddr *)&unaddr, sizeof(unaddr)) < 0) {
#ifdef USE_SYSLOG
	log_err ("bind: %m");
#else
	fprintf (fp_console, "bind: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */
	exit (1);
    }

    /* Ouvre le service */
    if (listen (sock_ecoute, 5) < 0) {
#ifdef USE_SYSLOG
      log_err ("listen: %m");
#else
      fprintf (fp_console, "listen: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */

      exit (1);
    }

    (void)umask (old_umask);

    /*
     * Attente de connexion...
     */
    for (;;) {

      /* Pour eviter les zombies */
      signal (SIGCHLD, waitchild);

#ifdef DEBUG_XTELD
	log_debug ("Attente de connexion...");
#endif

	for (retry = 10 ; ; retry--) {
	    if ((sock_service = accept (sock_ecoute, (struct sockaddr *)NULL, (int *)NULL)) > 0) {
		break;
	    }
	    else if (!retry) {
#ifdef USE_SYSLOG
		log_err ("accept: %m");
#else
		fprintf (fp_console, "accept: %s\n", sys_errlist[errno]);
#endif /* USE_SYSLOG */
		exit (1);
	    }
	}
	
	/*
	 * Cree le processus de service (fils)
	 */

	if (!fork()) {

#endif /* NO_NETWORK */

#ifdef DEBUG_XTELD
	    log_debug ("Connexion XTEL...");
#endif

	    /* Lecture du nom d'utilisateur */
	    if (flag_windaube) {
		if (lire_initial(utilisateur) < 0)
		    goto fin_xteld;
		write (XTELD_OUTPUT, "\x1B\x54\x20\x0F Appel...", 13);
		appel_service (service);
	    }
	    else {
		if (lire_chaine(utilisateur) < 0)
		    goto fin_xteld;
	    }

	    /* lecture connexion XTEL */
	    while (!fin_connexion) {
		if (read (XTELD_INPUT, &c, 1) <= 0) {
		    if (errno == EINTR)
			continue;
		    else
			break;
		}
		if (flag_connexion) {
		    if (c == VALEUR_COMMANDE_FIN) {
#ifdef DEBUG_XTELD
			log_debug ("tue le fils");
#endif		      
			kill (pid_fils, SIGTERM);
		    }
		    else {
#ifdef NO_TERMIO
			if (parite != SANS) {
			    register int p;
			    /* Calcul de parite... */			
			    p = (c & 0x0f) ^ (c >> 4);
			    p = (p & 3) ^ (p >> 2);      
			    p = (p & 1) ^ (p >> 1);
			    if (parite == IMPAIR)
				p = ~p;
			    c = (c & 0x7f) | (p << 7);
			}
#endif /* NO_TERMIO */
			/*
			 * on écrit sur le modem que lorsque le code de service
			 * a été entré où qu'il n'y en a pas.
			 */
			if (flag_serveur_local)
			    write ( tuyau_out[1], &c, 1 );
			else
			    if (saisie_active)
				write ( fd_modem, &c, 1);
#ifdef DEBUG_XTELD
			    else
				log_debug( "saisie perdue : `%c' (%d)", c, c );
#endif
		    }
		}
		else {
		    switch (c) {
			
		      case VALEUR_COMMANDE_CONNEXION_M1 :

			appel_service (NULL);
			break;

		      case VALEUR_COMMANDE_DEMANDE_CONNEXION :
			  
			/* lecture du service */
			if (lire_chaine(service) < 0)
			    fin_connexion++;
			else
			    /* connexion */
			    appel_service (service);
			  
			break;
			  
		      case VALEUR_COMMANDE_SERVICE_SUIVANT :
			    
			if ((indice_service == nb_services) || flag_m1)
			    write (XTELD_OUTPUT, CHAINE_REPONSE_PLUS_DE_SERVICE, 1);
			else {
			    if (service_autorise (indice_service, &temps_maxi)) {
				c = strlen (definition_services[indice_service].nom_service);
				write (XTELD_OUTPUT, &c, 1);
				write (XTELD_OUTPUT, definition_services[indice_service].nom_service, c);
#ifdef DEBUG_XTELD
				log_debug ("service: %s", definition_services[indice_service].nom_service);
#endif		      
			    }
			    else { /* service interdit */
				c = 0;
				write (XTELD_OUTPUT, &c, 1);
				indice_service++;
			    }
			}
			  
			break;
			  
		      case VALEUR_COMMANDE_NOM_UUCP :
			    
			c = strlen (definition_services[indice_service].nom_uucp);
			write (XTELD_OUTPUT, &c, 1);
			write (XTELD_OUTPUT, definition_services[indice_service].nom_uucp, c);
#ifdef DEBUG_XTELD
			log_debug ("%s", definition_services[indice_service].nom_uucp);
#endif		      
			indice_service++;
			  
			break;
			  
		      default :
			      
			break;
		    }
		}
	    }

fin_xteld:
#ifdef NO_NETWORK

	    /* Si xtel a ete tue sauvagement, tue le fils */
	    test_mort_subite ();

#ifdef DEBUG_XTELD
	    log_debug ("Fin du service");
#endif
	    /* Fin du service */
	    exit (0);

	} /* if !fork */

#ifdef DEBUG_XTELD
	log_debug ("Ferme la socket de service");
#endif
	close (sock_service);
    }

#else

#ifdef DEBUG_XTELD
    log_debug ("%d meurt (OK), errno = %d", getpid(), errno);
#endif

    /* Si xtel a ete tue sauvagement, tue le fils */
    test_mort_subite ();

#endif /* NO_NETWORK */

#ifndef USE_SYSLOG
    fclose (fp_console);
#endif /* NO_NETWORK */
}
