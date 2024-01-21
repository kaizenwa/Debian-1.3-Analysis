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
#ident "$Id: demon.h,v 1.8 1997/01/08 15:00:28 pierre Exp $"

#ifndef _demon_h
#define _demon_h

/*
 * Constantes du demon XTELD
 */
#define MAX_SERVICES			20
#define MAX_LIGNES			10
#define DELAI_DECONNEXION		120

#define PAIR				0
#define IMPAIR				1
#define SANS				2

/* Types de dialer */
#define DIALER_MODEM			0
#define DIALER_M1			1
#define DIALER_M2			2

/* chat-script pour  Minitel 1/2 */
#define CHAT_M1				"\\x1b\\x39\\x68 \\x13S"
#define CHAT_M2				"\\x1b\\x3b\\x60\\x58\\x54\\x1b\\x3b\\x61\\x5c\\x53\\x0101\\x04\\x02\\TC\\x03\\d\\x1b\\x3b\\x5201 \\x13S"

/* Flags */
#define FLAG_RTS_CTS			1

/* Definition des symboles si NO_TERMIO */
#ifdef NO_TERMIO
#define CS5	0
#define CS6	1
#define CS7	2
#define CS8	3
#endif /* NO_TERMIO */

/* IAN */
#define IAN_DE_GARDE 3
#define IAN_D_ENTREE 2
#define IAN_DE_FIN 1

/* 
 * Structure de definition des services
 */

struct definition_service {
    char *device;		/* device utilise */
    char *nom_service;
    char *nom_uucp;
    char *autorisations;
};

/* 
 * Structure de definition des lignes 
 */
struct definition_ligne {
    char *device;		/* device associe a la ligne */
    char *nom;			/* nom : /dev/ttyxx */
    char flags;			/* Flags: RTS/CTS... */
    int speed;			/* vitesse: B1200, B4800, ... */
    int cs;			/* character size: CS7, CS8 */
    int parity;			/* parite: EVEN, ODD, NONE */
    char *chat;			/* chat-script */
    char type_dialer;		/* type du dialer */
    int delai;			/* timeout du dialogue Modem */
};

/*
 * Protocole de dialogue XTEL <--> XTELD
 */
#define VALEUR_COMMANDE_DEMANDE_CONNEXION	128
#define VALEUR_COMMANDE_FIN			129
#define VALEUR_COMMANDE_SERVICE_SUIVANT		130
#define VALEUR_COMMANDE_NOM_UUCP		131
#define VALEUR_COMMANDE_CONNEXION_M1		132
#define VALEUR_TEMPS_MAXI			133

#define CHAINE_COMMANDE_DEMANDE_CONNEXION	"\200"
#define CHAINE_COMMANDE_FIN			"\201"
#define CHAINE_COMMANDE_SERVICE_SUIVANT		"\202"
#define CHAINE_COMMANDE_NOM_UUCP		"\203"
#define CHAINE_COMMANDE_CONNEXION_M1		"\204"

#define VALEUR_REPONSE_CONNEXION		128
#define VALEUR_REPONSE_DECONNEXION		129
#define VALEUR_REPONSE_PLUS_DE_SERVICE		130
#define VALEUR_REPONSE_DEBUT_ERREUR		131
#define VALEUR_REPONSE_FIN_ERREUR		132

#define CHAINE_REPONSE_CONNEXION		"\200"
#define CHAINE_REPONSE_DECONNEXION		"\201"
#define CHAINE_REPONSE_PLUS_DE_SERVICE		"\202"
#define CHAINE_REPONSE_DEBUT_ERREUR		"\203"
#define CHAINE_REPONSE_FIN_ERREUR		"\204"
#define CHAINE_TEMPS_MAXI			"\205"

#ifdef NO_NETWORK
#define XTEL_UNIX_PATH		"/tmp/.xtel"	/* Nom des sockets unix utilisees */
#endif /* NO_NETWORK */

#endif
