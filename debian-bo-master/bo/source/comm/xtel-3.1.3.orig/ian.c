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
static char rcsid[] = "$Id: ian.c,v 1.1 1996/12/20 16:20:47 pierre Exp $";

/*
 * Support IAN (Eric Delaunay, delaunay@lix.polytechnique.fr)
 */
#include <stdio.h>
#include "demon.h"
#include "globald.h"

#ifndef USE_SYSLOG
extern FILE *fp_console;
#endif

struct ian {
	char *service;	/* numéro de téléphone du service */
	char *garde;	/* code émis avant la page de garde Teletel */
	char *entree1;	/* code émis lors de l'entrée dans le service */
	char *entree2;	/* autre possibilité */
};

static struct ian IAN[] = {
	{ "3612",	"\033  0",	"\033!\001\0010",	"\033\"0" },
	{ "3613", 	"\033 !0",	"\033!\001\0010",	"\033\"0" },
	{ "3614",	"\033 \"0",	"\033!\001\0010",	"\033\"0" },
	{ "3615", 	"\033 #0",	"\033!\001\0010",	"\033\"0" },
	{ "3616",	"\033 $0",	"\033!\001\0010",	"\033\"0" },
	{ "3617",	"\033 %0",	"\033!\001\0010",	"\033\"0" },
	{ "3618",	"\033 &0",	"\033!\001\0010",	"\033\"0" },
	{ "3619",	"\033 '0",	"\033!\001\0010",	"\033\"0" },
	{ "3621",	"service:\021",	"tablie",		"LIB"     }
};

static int ian_courant;
static char *ian_ptr1, *ian_ptr2;
int ian_n1, ian_n2;

/*
 * initialise la détéction des IAN
 */
void ian_init( telno )
    char *telno;
{
    int i;

    ian_courant = -1;
    for( i = 0 ; i < sizeof(IAN)/sizeof(IAN[0]) ; i++ ) {
	if (!strcmp( IAN[i].service, telno )) {
	    ian_courant = i;
	    ian_ptr1 = ian_ptr2 = NULL;
#ifdef DEBUG_XTELD
	    log_debug( "Prêt pour détection des IAN (service %s)", IAN[i].service );
#endif
	    break;
	}
    }
#ifdef DEBUG_XTELD
    if (ian_courant < 0)
	log_debug( "IAN non déterminé pour le numéro : %s", telno );
#endif
}

/*
 * détection des IAN
 */
int ian_valide( type_ian, c )
    int type_ian;
    char c;
{
    if (ian_courant < 0)
	return 1;
    if (!ian_ptr1) {
	switch (type_ian) {
	case IAN_DE_GARDE:
	case IAN_DE_FIN:
#ifdef DEBUG_XTELD
	    log_debug( "attente IAN de garde [%d]", IAN_DE_GARDE );
#endif
	    ian_ptr1 = IAN[ian_courant].garde;
	    ian_n1 = 0;
	    ian_ptr2 = NULL;
	    ian_n2 = 0;
	    break;
	case IAN_D_ENTREE:
#ifdef DEBUG_XTELD
	    log_debug( "attente IAN d'entrée [%d]", IAN_D_ENTREE );
#endif
	    ian_ptr1 = IAN[ian_courant].entree1;
	    ian_n1 = 0;
	    ian_ptr2 = IAN[ian_courant].entree2;
	    ian_n2 = 0;
	    break;
	default:
#ifdef USE_SYSLOG
	    log_err ("attente IAN inconnu !");
#else
	    fprintf (fp_console, "attente IAN inconnu !");
#endif /* USE_SYSLOG */
	    return 1;	/* retourne IAN valide pour déconnecter le modem */
	}
    }
    if (ian_ptr1[ian_n1] == c || ian_ptr1[ian_n1] == '\001') {
	if (ian_ptr1[++ian_n1] == 0) {
#ifdef DEBUG_XTELD
	    log_debug( "IAN ptr1 reçu" );
#endif
	    ian_ptr1 = ian_ptr2 = NULL;
	    return 1;		/* IAN détecté */
	}
    }
    else
	ian_n1 = 0;		/* caractère faux : on recommence au début de la chaîne */
    if (ian_ptr2) {
	if (ian_ptr2[ian_n2] == c || ian_ptr2[ian_n2] == '\001') {
	    if (ian_ptr2[++ian_n2] == 0) {
#ifdef DEBUG_XTELD
		log_debug( "IAN ptr1 reçu" );
#endif
		ian_ptr1 = ian_ptr2 = NULL;
		return 2;	/* IAN détecté */
	    }
	}
	else
	    ian_n2 = 0;		/* caractère faux : on recommence au début de la chaîne */
    }
    return 0;	/* IAN pas trouvé */
}

