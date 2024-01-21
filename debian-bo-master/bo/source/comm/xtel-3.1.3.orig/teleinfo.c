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
static char rcsid[] = "$Id: teleinfo.c,v 1.5 1996/09/25 10:15:47 pierre Exp $";

/*
 * Traitement du mode Tele-informatique
 * Inspire du programme "minitel" de Sylvain Meunier...
 */

#include "xtel.h"

#include <signal.h>
#include <fcntl.h>
#ifndef SVR4
#include <sys/types.h>
#include <sys/stat.h>
#else
#include <unistd.h>
#include <sys/stropts.h>
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
#endif /* NO_TERMIO */

#include <errno.h>

static char pty_m[20], pty_s[20];
static char buf[10];
static int fd_m;
static char *passage_videotex = "\033[?{";
static char indice_reception;

/* En cas de malheur (sans Gabin cette fois...) */
static void teleinfo_fatal (s1, s2, s3)
char *s1, *s2, *s3;
{
  fprintf (stderr, "teleinfo: %s %s %s\n", s1, s2, s3);
  exit (1);
}

/* 
 * Cherche un pseudo-terminal, retourn le file-descripteur maitre
 * ainsi que les noms des devices maitre/esclave
 */
static int cherche_pseudo_tty (master, slave, pl, pc)
char *master, *slave;
int *pl, *pc;
{
    int fd_master;
#ifdef SVR4
    extern char *ptsname();
    char *ptyname;
#else
    char ok = 0, c;
    register int i;
    struct stat statb;
#endif /* SVR4 */

#ifdef SVR4
    /*
     * Grace aux "streams" c'est beaucoup + propre en SVR4...
     */
    strcpy (master, "/dev/ptmx");
    if ((fd_master = open ("/dev/ptmx", O_RDWR)) < 0)
	teleinfo_fatal ("/dev/ptmx", sys_errlist[errno]);
    sighold (SIGCLD);
    if (grantpt (fd_master) == -1)
	teleinfo_fatal ("could not grant slave pty");
    sigrelse (SIGCLD);
    if (unlockpt(fd_master) == -1) 
	teleinfo_fatal ("could not unlock slave pty");
    if (!(ptyname = ptsname(fd_master)))
	teleinfo_fatal ("could not enable slave pty");	
    strcpy(slave, ptyname);
    *pl = *pc = 0;
#else
    /*
     * Methode bourrin pour les autres Unix...
     */
    for (c = 'p'; c <= 'z' && !ok; c++) {
	for (i = 0; i < 16; i++) {
	    sprintf (master, "/dev/pty%c%x", c, i);
	    if (stat (master, &statb) < 0) {
		perror (master);
		return (-1);
	    }
		  
	    fd_master = open (master, O_RDWR, 0);
	    
	    if (fd_master >= 0) {
		sprintf (slave, "/dev/tty%c%x", c, i);
		if (access (slave, 6) != 0) {
		    close (fd_master);
		    continue;
		}
		ok = 1;
		break;
	    }
	}
    }
    
    *pl = c;
    *pc = i;

#endif

    return (fd_master);
}

/*
 * Lance un "xterm" pour emuler le mode Tele-Informatique
 */
void teleinfo (nom)
char *nom;
{
    char c;
    int max_car = 20, lettre, chiffre;
#ifndef SVR4
#ifdef NO_TERMIO
    struct sgttyb term;
#else
#ifdef USE_TERMIOS
    struct termios term;
#else
    struct termio term;
#endif /* USE_TERMIOS */
#endif /* NO_TERMIO */
#endif /* !SVR4 */

    /* chercher le pseudo-tty */
    if ((fd_m = cherche_pseudo_tty (pty_m, pty_s, &lettre, &chiffre)) < 0)
	teleinfo_fatal ("erreur ouverture maitre");

    if ((fd_teleinfo = open (pty_s, O_RDWR)) < 0)
	teleinfo_fatal (pty_s, sys_errlist[errno]);

    /* Configuration de la ligne (mode RAW) */
#ifdef SVR4    
    /* Il suffit de depiler les modules pour avoir la ligne en RAW */
    while (ioctl (fd_teleinfo, I_POP, 0) > 0)
	;
#else
#ifdef NO_TERMIO
    ioctl (fd_teleinfo, TIOCGETP, &term);
    term.sg_flags |= RAW;
    ioctl (fd_teleinfo, TIOCSETP, &term);
#else
#ifdef USE_TERMIOS
    ioctl (fd_teleinfo, TIOCGETA, &term);
#else
    ioctl (fd_teleinfo, TCGETA, &term);
#endif /* USE_TERMIOS */
    term.c_cc[VMIN] = 1;
    term.c_cc[VTIME] = 0;
    term.c_lflag &= ~(ICANON|ISIG|ECHO);
#ifdef NOTDEF
    term.c_cflag &= ~(PARODD|CSIZE);
    term.c_cflag |= (CS7|PARENB);		/* a voir... */
#endif
#ifdef USE_TERMIOS
    ioctl (fd_teleinfo, TIOCSETA, &term);
#else
    ioctl (fd_teleinfo, TCSETA, &term);
#endif /* USE_TERMIOS */
#endif /* NO_TERMIO */
#endif /* SVR4 */

    /* 
     * Lancement du "xterm" 
     */
    if ((pid_teleinfo = fork()) == 0) {
	dup2(fd_m, 1);
#ifdef SVR4
	sprintf(buf, "-S%02d1", atoi(strrchr (pty_s, '/')+1));
#else
	sprintf(buf, "-S%c%d1", lettre, chiffre);
#endif /* SVR4 */
	execlp(XTERM_PATH, XTERM_PATH, "-name", nom, "-geometry", "80x25+10+10", buf, NULL);
	teleinfo_fatal ("erreur a l'execution de xterm");
    }
    
    close (fd_m);

    c = 0;
    while (read (fd_teleinfo, &c, 1) > 0 && c != '\n' && max_car--) 
      ;

    XtVaSetValues (ecran_minitel, XtNfdConnexion, fd_teleinfo, NULL);
}

static char fils_mort;

static void sigchld ()
{
    int r;

    fils_mort = 0;
    while (wait (&r) > 0)
	;

    fils_mort = 1;
}

/* 
 * Conversion pour le mode teleinfo-FR, neccessite quelques
 * amelioration pour ceux que ca interesse (y en a-t-il ???) ==> faire
 * une fonte speciale...
 */
char conversion_teleinfo_fr (car)
char car;
{
    register char c;
    
    switch (car) {
	
      case '{' :
	  
	  c = 0xe9; /* { ==> e' */
	  break;
	  
	case '}' :
	    
	    c = 0xe8; /* etc... */
	  break;
	  
	case '@' :
	    
	    c = 0xe0;
	  break;
	  
	case '\\' :
	    
	    c = 0xe7;
	  break;
	  
	case '[' :
	    
	    c = 0xb0;
	  break;
	  
	case ']' :
	    
	    c = 0xa7;
	  break;
	  
	case '#' :
	    
	    c = 0xa3;
	  break;
	  
	  default :
	      c = car;
      }
    
    return (c);
}

/*
 * Lecture du clavier en mode Teleinfo
 */
/* ARGSUSED */
void fonction_lecture_teleinfo (client_data, fid, id)
XtPointer client_data;	/* non utilise */
int *fid;
XtInputId *id;
{
    unsigned char c;
    Boolean flag_connexion;

    XtVaGetValues (ecran_minitel, XtNconnecte, &flag_connexion, NULL);
    
    if (read (*fid, &c, 1) == 1)  {
	/* Test de reception de la chaine de retour en Videotex (ESC [?{) */
	if (*(passage_videotex+indice_reception) == c) {
	    if (indice_reception == 3) {
		selection_mode_emulation (ecran_minitel, "V", NULL);
		return;
	    }
	    else 
		indice_reception = (indice_reception == 3 ? 0 : indice_reception++);
	}   

	if (mode_emulation == MODE_TELEINFO_FR)
	    c = conversion_teleinfo_fr (c);

	if (flag_connexion)
	    write (socket_xteld, &c, 1);
	else
	    write (fd_teleinfo, &c, 1);
    }
}

/*
 * Selection du mode d'emulation
 */
void zigouille_xterm_teleinfo ()
{
    kill (pid_teleinfo, SIGKILL);
    XtVaSetValues (ecran_minitel, XtNfdConnexion, -1, NULL);
    close (fd_teleinfo);
    XtRemoveInput (input_id_teleinfo);

    while (!fils_mort)
      ;
}

void selection_mode_emulation (w, mode, call_data)
Widget w;
char *mode;
XtPointer call_data;
{
    if ((mode[0] == 'V' && mode_emulation == MODE_VIDEOTEX) || (mode[0] == 'A' && mode_emulation == MODE_TELEINFO_ASCII) || (mode[0] == 'F' && mode_emulation == MODE_TELEINFO_FR))
	return;

    if (mode[0] == 'V') {
	mode_emulation = MODE_VIDEOTEX;
	zigouille_xterm_teleinfo ();
	XtVaSetValues (ecran_minitel, XtNmodeVideotex, True, NULL);
    }
    else {
	signal (SIGCHLD, sigchld);
	indice_reception = 0;
	XtVaSetValues (ecran_minitel, XtNmodeVideotex, False, NULL);
	if (mode[0] == 'A') {
	    if (mode_emulation == MODE_TELEINFO_FR) {
		zigouille_xterm_teleinfo ();
	    }
	    mode_emulation = MODE_TELEINFO_ASCII;
	    teleinfo ("T\351l\351info-ASCII");
	}
	else if (mode[0] == 'F') {
	    if (mode_emulation == MODE_TELEINFO_ASCII) {
		zigouille_xterm_teleinfo ();
	    }
	    mode_emulation = MODE_TELEINFO_FR;
	    teleinfo ("T\351l\351info-FR");
	}
	else {
	    printf ("selection_mode_emulation: Mode %d inconnu !\n");
	    return;
	}

	if (!lecteur_play)
	    input_id_teleinfo = XtAppAddInput(app_context, fd_teleinfo, (XtPointer)XtInputReadMask, (XtInputCallbackProc)fonction_lecture_teleinfo, NULL);
      }

    mise_a_jour_mode_emulation (0);
}



