/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1994  Lectra Systemes & Pierre Ficheux
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
 *		 
 */
static char rcsid[] = "$Id: dial.c,v 1.13 1996/10/28 13:36:37 pierre Exp $";

/*
 * Composition des numeros telephoniques
 */

/* Contributions:
 *
 *   Michel Fingerhut	IRCAM Paris
 *
 *	- code Ultrix pour les ioctl *		 
 *	- Traitement du fichier de log
 *	- Acces proteges aux services
 *
 *   Pierre Beyssac	SYSECA
 *
 *	- code 386BSD
 *	- traitement de l'hexa dans les chat-scripts
 *	- traitement du Minitel 2
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#ifdef SVR4
#include <sys/mkdev.h>
#endif /* SVR4 */
#ifdef NO_TERMIO
#include <sgtty.h>
#else
#ifdef USE_TERMIOS
#include <sys/ioctl.h>
#include <termios.h>
#else
#include <termio.h>
#endif /* USE_TERMIOS */
#endif
#include <errno.h>
#ifdef USE_SYSLOG
#include <syslog.h>
#endif /* USE_SYSLOG */
#if defined(lectra) && defined(SVR4) && !defined(sun)
#include <sys/termiox.h>
#endif

#include "demon.h"
#include "globald.h"

#ifdef NO_TERMIO
static struct sgttyb term_sauve, term;
#else
#ifdef USE_TERMIOS
static struct termios term_sauve, term;
#else
static struct termio term_sauve, term;
#endif /* USE_TERMIOS */
#endif /* NO_TERMIO */
#if defined(lectra) && defined(SVR4) && !defined(sun)
static struct termiox termx;
#endif /* lectra && SVR4 && !sun */

static char nom_lck[256];
static fd_set a_lire;
static struct timeval delai_maxi;

/* 
 * Envoi d'une commande MODEM 
 */

static void writemodem (fd, buf, n)
int fd;
char *buf;
int n;
{
    register int i;
    unsigned long j;

    for (i = 0 ; i != n ; i++) {
        write (fd, buf+i, 1);
        for (j = 0 ; j != 300000 ; j++);
    }
}

/*
 * fonction UNDIAL
 */

void myundial (fd)
int fd;
{
#ifdef ultrix
    int temp = 0;
#endif
    if (unlink (nom_lck) < 0) {
	erreur_a_xtel (nom_lck, errno);
    }

    /* remet la ligne en l'etat */
#ifdef NO_TERMIO
    term.sg_ispeed = term.sg_ospeed = B0;
    ioctl (fd, TIOCSETP, &term);
    ioctl (fd, TIOCSETP, &term_sauve);
#else
#ifdef USE_TERMIOS
    term.c_ispeed = B0;
    term.c_ospeed = B0;
    ioctl (fd, TIOCSETAW, &term);
    ioctl (fd, TIOCSETA, &term_sauve);
#else
    term.c_cflag &= ~CBAUD;
    term.c_cflag |= B0;
    ioctl (fd, TCSETAW, &term);
    ioctl (fd, TCSETA, &term_sauve);
#endif /* USE_TERMIOS */
#endif /* NO_TERMIO */
#ifdef ultrix
    ioctl (fd, TIOCNMODEM, &temp);
    ioctl (fd, TIOCSINUSE, NULL);
    ioctl (fd, TIOCHPCL, 0);
#endif /* ultrix */
    close (fd);
}

/* 
 * Dialogue avec le Modem (chatons, chatons...)
 */
do_chat (fd, chat_script, telno)
int fd;
char *chat_script, *telno;
{
    int i, erreur, fin, nbread, cmodem;
    char *pt_chat, c;

    erreur = 0;
    pt_chat = chat_script;
    fin = 0;

#ifndef NO_TERMIO
    /* 
     * Passe en controle local si necessaire. Cela permet d'eviter que le
     *  script se plante lorsque certains modem font bouger le DCD sur un AT&F
     *  (comme le Hayes Optima par exemple)...
     */
#ifdef USE_TERMIOS
    ioctl (fd, TCIOCGETA, &term);
#else
    ioctl (fd, TCGETA, &term);
#endif /* USE_TERMIOS */
    if ((term.c_cflag | CLOCAL) == 0) {
	term.c_cflag |= CLOCAL;
#ifdef USE_TERMIOS
	ioctl (fd, TCIOCSETA, &term);
#else
	ioctl (fd, TCSETA, &term);
#endif /* USE_TERMIOS */
	cmodem = 1;
    }
#endif /* NO_TERMIO */

    while (!fin) {

	/* 
	 * On commence par emettre la commande au Modem... (les chaines sont
	 * separees par des blancs). On attend ensuite la reponse.
	 */
#ifdef DEBUG_XTELD
	char ci; int fi;

	for (i = 0 ; *(pt_chat+i) != ' ' && *(pt_chat+i) != 0 ; i++);
	ci = pt_chat[i]; pt_chat[i] = '\0';
	log_debug ("J'envoie: \"%s\"", pt_chat);
	pt_chat[i] = ci;

	if (ci) {
	    i++; fi = i;
	    for ( ; *(pt_chat+i) != ' ' && *(pt_chat+i) != 0 ; i++);
	    ci = pt_chat[i]; pt_chat[i] = '\0';
	    log_debug ("J'attends: \"%s\"", pt_chat + fi);
	    pt_chat[i] = ci;
	}
#endif

	while (*pt_chat && *pt_chat != ' ') {
	    /*
	     * On reconnait les sequences '\' suivantes :
	     *
	     *	\T		numero de telephone
	     *	\d		tempo de 1 s
	     *	\n		LF
	     *	\r		CR
	     *	\t		VT
	     *	\a		BELL
	     *	\abc		valeur DECIMALE entre 0 et 255 sur 3 caracteres
	     *			(ex: \010 pour envoyer 10)
	     *	\xab		valeur hexadecimale
	     */

	    if (*pt_chat != '\\') {
#ifdef DEBUG_XTELD
		log_debug ("envoie: '%c'", *pt_chat);
#endif
		writemodem (fd, pt_chat, 1);
	    } else {
		pt_chat++;
		switch (*pt_chat) {
		  case 'd' :
		    sleep (1);
		    break;
		  case 'n' :
#ifdef DEBUG_XTELD
		    log_debug ("envoie: '\\n'");
#endif
		    writemodem (fd, "\n", 1);
		    break;
		  case 'r' :
#ifdef DEBUG_XTELD
		    log_debug ("envoie: '\\r'");
#endif
		    writemodem (fd, "\r", 1);
		    break;
		  case 't' :
#ifdef DEBUG_XTELD
		    log_debug ("envoie: '\\t'");
#endif
		    writemodem (fd, "\t", 1);
		    break;
		  case 'a' :
#ifdef DEBUG_XTELD
		    log_debug ("envoie: '\\a'");
#endif
		    writemodem (fd, "\a", 1);
		    break;
		  case 'T' :
#ifdef DEBUG_XTELD
		    log_debug ("envoie: \"%s\"", telno);
#endif
		    writemodem (fd, telno, strlen(telno));
		    break;

		  case '0': case '1': case '2': case '3': case '4':
		  case '5': case '6': case '7': case '8': case '9': {
		      char nb = 2;
		      char n = *pt_chat++ - '0';
		      while (nb-- && isdigit(*pt_chat)) {
			  n *= 10;
			  n += *pt_chat++ - '0';
		      }
		      pt_chat--;
#ifdef DEBUG_XTELD
		      log_debug ("envoie: '\\0x%x'", n);
#endif
		      writemodem (fd, &n, 1);
		      break;
		  }

		  case 'x': {
		      char n = 0, nb = 2;
		      pt_chat++;
		      while (nb-- && isxdigit(*pt_chat)) {
			  n *= 16;
			  if (isdigit(*pt_chat)) {
			      n += *pt_chat - '0';
			  } else {
			      if (islower(*pt_chat)) {
				  n += *pt_chat + (10 - 'a');
			      } else {
				  n += *pt_chat + (10 - 'A');
			      }
			  }
			  pt_chat++;
		      }
		      pt_chat--;
#ifdef DEBUG_XTELD
		      log_debug ("envoie: '\\0x%x'", n);
#endif
		      writemodem (fd, &n, 1);
		      break;
		  }
		  default :
#ifdef DEBUG_XTELD
		    log_debug ("envoie: '%c'", *pt_chat);
#endif
		    writemodem(fd, pt_chat, 1);
		    break;
		}
	    }
	    pt_chat++;
	}
	if (*pt_chat == 0)
	    break;
	/*
	 * On attend la reponse.
	 */

	/* pointe la chaine a recevoir */
	pt_chat++;
	i = 0;

	for (;;) {

	    nbread = select (32, &a_lire, NULL, NULL, &delai_maxi);

	    /* Si il y a qque chose a lire */
	    if (nbread > 0 && FD_ISSET (fd, &a_lire)) {
		if (read (fd, &c, 1) == 1)  {
		    char voulu;
#ifdef DEBUG_XTELD
		    log_debug ("recu: '%c'", c);
#endif
		    if (pt_chat[i] != '\\') {
			voulu = pt_chat[i];
		    } else {
			i++;
			switch(pt_chat[i]) {
			  case 'n':
			    voulu = '\n';
			    break;
			  case 'r':
			    voulu = '\r';
			    break;
			  case 't':
			    voulu = '\t';
			    break;
			  case 'a':
			    voulu = '\a';
			    break;
			  case '0': case '1': case '2':
			  case '3': case '4': case '5':
			  case '6': case '7': case '8':
			  case '9': {
			      char nb = 2;
			      voulu = pt_chat[i++] - '0';
			      while (nb-- && isdigit(pt_chat[i])) {
				  voulu *= 10;
				  voulu += pt_chat[i++] - '0';
			      }
			      i--;
			      break;
			  }
			  case 'x': {
			      char nb = 2;
			      voulu = 0;
			      i++;
			      while (nb-- && isxdigit(pt_chat[i])) {
				  voulu *= 16;
				  if (isdigit(pt_chat[i])) {
				      voulu += pt_chat[i] - '0';
				  } else {
				      if (islower(pt_chat[i])) {
					  voulu += pt_chat[i] + (10 - 'a');
				      } else {
					  voulu += pt_chat[i] + (10 - 'A');
				      }
				  }
				  i++;
			      }
			      i--;
			      break;
			  }
			  default:
			    voulu = pt_chat[i];
			    break;
			}
		    }
		    if (voulu == c) {
			i++;
#ifdef DEBUG_XTELD
			log_debug ("voulu '%c':Ok", voulu);
#endif
			if (*(pt_chat+i) == ' ' || *(pt_chat+i) == 0) {
#ifdef DEBUG_XTELD
			    log_debug (" -Ok");
#endif
			    break;
			}
		    }
		    else {
#ifdef DEBUG_XTELD
			log_debug ("voulu '%c':erreur", voulu);
#endif
			i = 0;
		    }
		}
		/* erreur read */
		else {
#ifdef DEBUG_XTELD
		    log_debug ("Erreur read !");
#endif
		    erreur = 1;
		    break;
		}
	    }
	    /* timeout */
	    else if (nbread == 0) {
#ifdef DEBUG_XTELD
		log_debug ("Erreur timeout !");
#endif
		erreur = 1;
		break;
	    }
	}

	/* Commande suivante */
	pt_chat += (i+1);
	fin = ((erreur != 0 || *(pt_chat-1) == 0) ? 1 : 0);
	sleep (1);
    }

#ifndef NO_TERMIO
    /* Repasse en controle modem */
    if (cmodem) {
	term.c_cflag &= ~CLOCAL;
#ifdef USE_TERMIOS
	ioctl (fd, TCIOCSETA, &term);
#else
	ioctl (fd, TCSETA, &term);
#endif /* USE_TERMIOS */
    }
#endif

    return erreur;
}   

/*
 * Fonction DIAL
 */
mydial (telno, device)
char *telno, *device;
{
    char buf[80], erreur;
    struct stat statb;
    int fdlck, fd;

    for (numero_ligne = 0 ; numero_ligne != nb_lignes && definition_lignes[numero_ligne].type_dialer != DIALER_M1 ; numero_ligne++) 
	;
    
    /* Minitel 1 ==> force la ligne */
    if (numero_ligne != nb_lignes) 
	nb_lignes = numero_ligne + 1;
    else
	numero_ligne = 0;

    while (numero_ligne < nb_lignes) {

	/*
	 * Recherche la premiere ligne non deja utilise par un programme UUCP
	 */
	for (;;) {
#ifdef SVR4
	    if (stat (definition_lignes[numero_ligne].nom, &statb) != 0) {
		erreur_a_xtel ("mydial()", errno);
		return (-1);
	    }
	    sprintf (nom_lck, FICHIER_LCK, (unsigned long) major(statb.st_dev), (unsigned long) major(statb.st_rdev), (unsigned long) minor(statb.st_rdev));
#else
	    sprintf (nom_lck, FICHIER_LCK, &definition_lignes[numero_ligne].nom[5]);
#endif /* SVR4 */
#ifdef DEBUG_XTELD
	    log_debug( "ligne= %s, device= %s, lock= %s", definition_lignes[numero_ligne].nom, definition_lignes[numero_ligne].device, nom_lck);
#endif
	    /* Si le lock existe */
	    if (stat (nom_lck, &statb) == 0) {
		if (numero_ligne == nb_lignes-1) {
		    /* Dommage, c'etait la derniere :-( */
		    erreur_a_xtel ("[0] Pas de MODEM disponible !", 0);
		    return (-1);
		}
	    }
	    /* Sinon, on verifie que le device corresponde */
	    else if (device == NULL || strcmp (device, definition_lignes[numero_ligne].device) == 0)
	      break;
	  once_again:
	    numero_ligne++;
	}
	
#ifdef DEBUG_XTELD
	log_debug ("creation de %s", nom_lck);
#endif
	/* on cree un fichier semaphore LCK..ttyxx */
	if ((fdlck = open (nom_lck, O_WRONLY|O_EXCL|O_CREAT, 0644)) < 0) {
	    erreur_a_xtel (nom_lck, errno);
	    return (-1);
	}
	
	/* on ecrit le PID dedans */
	sprintf (buf, "%10d\n", getpid ());
	write (fdlck, buf, strlen (buf));
	close (fdlck);
	
#ifdef DEBUG_XTELD
	log_debug ("Ouverture de la ligne %s", definition_lignes[numero_ligne].nom);
#endif
	/* ouvre la ligne */
	if ((fd = open (definition_lignes[numero_ligne].nom, O_RDWR)) < 0) {
	    /* Derniere ligne, on passe l'erreur */
	    if (numero_ligne == nb_lignes-1) {
		erreur_a_xtel (definition_lignes[numero_ligne].nom, errno);
		return (-1);
	    }
	    /* Sinon on essaye la ligne suivante */
	    else {
		unlink (nom_lck);
		goto once_again;	/* aller a, jacta est */
	    }
	}

	FD_ZERO (&a_lire);
	FD_SET (fd, &a_lire);
	delai_maxi.tv_sec = (unsigned long)definition_lignes[numero_ligne].delai;

	/* 
	 * Fixe les paremetres de la ligne en fonction du device associe
	 */
#ifdef NO_TERMIO
	ioctl (fd, TIOCGETP, &term);
	memcpy ((char *)&term_sauve, (char *)&term, sizeof(struct sgttyb));
	term.sg_flags |= RAW;
	ioctl (fd, TIOCSETP, &term);
	
	/* Flags, pour l'instant RTS/CTS */
	/* FIXME: comment passer la ligne en RTS/CTS sans termio ? */

#else

#ifdef USE_TERMIOS
	ioctl (fd, TIOCGETA, &term);
	memcpy ((char *)&term_sauve, (char *)&term, sizeof(struct termios));
#else
	ioctl (fd, TCGETA, &term);
	memcpy ((char *)&term_sauve, (char *)&term, sizeof(struct termio));
#endif /* USE_TERMIOS */
	
	/* Parametrage de la ligne */
	term.c_cc[VMIN] = 1;
	term.c_cc[VTIME] = 0;
	term.c_iflag &= ~(IXON|IXOFF|ICRNL);
	term.c_lflag &= ~(ICANON|ISIG|ECHO|IEXTEN);

#ifdef USE_TERMIOS
	/*
	 * FreeBSD 1.1 (Beta) n'a pas l'air d'apprecier qu'on mette clocal
	 * a 0... (blocage au premier write sur /dev/cua01)
	 */
	term.c_cflag &= ~(CSIZE|CSTOPB);
	term.c_cflag |= (CREAD|CRTSCTS|HUPCL);
	term.c_ispeed = term.c_ospeed = definition_lignes[numero_ligne].speed;
#else
	term.c_cflag &= ~(CSIZE|CBAUD|CLOCAL);

	/* Vitesse */
	term.c_cflag |= definition_lignes[numero_ligne].speed;
#endif /* USE_TERMIOS */
	/* Taille caractere */
	term.c_cflag |= definition_lignes[numero_ligne].cs;

	/* Parite */
	if (definition_lignes[numero_ligne].parity == SANS)
	    term.c_cflag &= ~PARENB;
	else {
	    term.c_cflag |= PARENB;
	    if (definition_lignes[numero_ligne].parity == PAIR) 
		term.c_cflag &= ~PARODD;
	}

	/* Flags, pour l'instant RTS/CTS */
#if !(defined(sgi) || (defined(lectra) && defined(SVR4) && !defined(sun)))
	if (definition_lignes[numero_ligne].flags & FLAG_RTS_CTS) {
#ifdef DEBUG_XTELD
	    log_debug ("passe la ligne %d en RTS/CTS", numero_ligne);
#endif
	    term.c_cflag |= CRTSCTS;
	}
#else
#ifdef sgi
#ifdef USE_SYSLOG
	log_err ("FIXME: RTS/CTS non supporte sur SGI !");
#else
	fprintf (fp_console, "FIXME: RTS/CTS non supporte sur SGI !\n");
#endif
#endif /* sgi */
#endif /* !(lectra && SVR4 && !sun) || sgi */

#ifdef sun
	if (definition_lignes[numero_ligne].cs != CS8)
	    term.c_iflag = ISTRIP;
#endif /* sun */

#ifdef USE_TERMIOS
	term.c_iflag &= ~(IGNCR|ICRNL|INLCR|IMAXBEL);
	term.c_iflag |= (ISTRIP|INPCK);
	term.c_lflag &= ~(ECHOCTL|IEXTEN);
	term.c_oflag &= ~OPOST;
#endif /* USE_TERMIOS */

	/* Cas du Minitel 1/2 */
	if (definition_lignes[numero_ligne].type_dialer != DIALER_MODEM)
	    term.c_cflag |= (CREAD|CLOCAL);

	/* Affectation des parametres */

#ifdef USE_TERMIOS
	ioctl (fd, TIOCSETA, &term);
#else
	ioctl (fd, TCSETA, &term);
#endif /* USE_TERMIOS */

#endif /* NO_TERMIO */

	/* RTS/CTS sur LS/UX */
#if defined(lectra) && defined(SVR4) && !defined(sun)
	if (definition_lignes[numero_ligne].flags & FLAG_RTS_CTS) {
#ifdef DEBUG_XTELD
	    log_debug ("passe la ligne %d en RTS/CTS", numero_ligne);
#endif
	    ioctl (fd, TCGETX, &termx);
	    termx.x_hflag |= (RTSXOFF|CTSXON);
	    ioctl(fd, TCSETXW, &termx);
	}
#endif /* lectra && SVR4 && !sun */
	
#ifdef DEBUG_XTELD
	log_debug ("Dialogue Modem...");
#endif
	erreur = do_chat (fd, definition_lignes[numero_ligne].chat, telno);

	/*
	 * Test de l'erreur en sortie
	 */
	if (!erreur) {
#ifdef ultrix
 	    term.c_cflag &= ~CLOCAL;
 	    ioctl (fd, TCSETA, &term);
#endif
	    return (fd);
	}
	else {
	    /*
	     * Sortie en timeout => efface le fichier semaphore 
	     */
	    
	    if (unlink (nom_lck) < 0) {
		erreur_a_xtel (nom_lck, errno);
	    }
	    close (fd);
	    numero_ligne++;
	}
    }

    /*
     * Erreur connexion !
     */

    erreur_a_xtel ("[0] Pas de MODEM disponible !", 0);

    close (fd);

    return (-1);
}
