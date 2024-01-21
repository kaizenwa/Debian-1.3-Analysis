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
 */
#ident "$Id: xtel.h,v 1.6 1997/02/25 15:53:38 pierre Exp $"

#ifndef _xtel_h
#define _xtel_h

/*
 * definition des constantes
 */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#ifdef NO_NETWORK
#include <sys/socket.h>
#include <sys/un.h>
struct sockaddr_un unaddr;
#endif /* NO_NETWORK */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>

#include "demon.h"

/* le widget Videotex */
#include "Videotex.h"

/* Mode d'impression */
#define ASCII			0
#define VIDEOTEX		1

/* Telechargement */
#define X_MODEM			0
#define Y_MODEM			1
#define Z_MODEM			2

/* Pour le mode Tele-informatique */
#define MODE_VIDEOTEX		0
#define MODE_TELEINFO_ASCII	1
#define MODE_TELEINFO_FR	2

/* Type d'arret */
#define ARRET_FF		1
#define ARRET_SUITE		2

/* definition d'une touche MINITEL */
struct definition_touche {
    char *nom;
    char *code;
};

/* ressources de Xtel */
typedef struct _ressources_xtel {
    String serveur;
    String browser_HTML;
    String URL_doc;
    String URL_whatsnew;
    Boolean petiteFonte;
    Boolean nGris;
    String commandeImpression;
    String commandeImpressionAscii;
    String nomService;
    Boolean boutonRaccrocher;
    Boolean lowMemory;
    String string[9];
    String erreur[1];
} ressources_xtel;

/* nb de procedures */
#define MAXPROC		10

#define P_PROC		0
#define P_SERV		1
#define P_WAIT		2
#define P_TRAN		3
#define P_REC		4
#define P_WRI		5
#define P_HUP		6
#define P_EXI		7

struct keyword {
    char *mnemonic;
    char alias;
    char type;
}; 

struct procedure {
  char *nom;
  char *service;
  struct d {
    char type;
    struct d *suivant;
    union {
      char *attente;
      char *emission;
      char *filename;
      char state;
    } p;
  } *d;
};

/* Protocoles de telechargement */
#define MAXPROTO	10

/* Procedure */
struct protocole {
    char *nom;
    char *commande;
};

/* Definitions globales */
#include "global.h"

#endif
