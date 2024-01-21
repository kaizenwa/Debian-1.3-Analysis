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
#ident "$Id: VideotexP.h,v 1.7 1996/10/31 12:58:14 pierre Exp $"

#ifndef _VideotexP_h
#define _VideotexP_h

/*
 * constantes et structures propres a l'emulation Videotex
 */
/* Pixmaps standards*/
#include <X11/bitmaps/black>
#include <X11/bitmaps/dimple1>
#include <X11/bitmaps/light_gray>
#include <X11/bitmaps/gray>
#include <X11/bitmaps/flipped_gray>
#include <X11/bitmaps/gray3>
#include <X11/bitmaps/cross_weave>

/* Pixmaps locaux */
#define unx1_width 16
#define unx1_height 16
static char unx1_bits[] = {
   0xff, 0xff, 0x55, 0x55, 0xff, 0xff, 0x55, 0x55, 0xff, 0xff, 0x55, 0x55,
   0xff, 0xff, 0x55, 0x55, 0xff, 0xff, 0x55, 0x55, 0xff, 0xff, 0x55, 0x55,
   0xff, 0xff, 0x55, 0x55, 0xff, 0xff, 0x55, 0x55};

#define white_width 16
#define white_height 16
static char white_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};


/*
 * Codes et commandes VIDEOTEX
 */
/* Fonctions de mise en page */

#define	CR              	13
#define LF	                10
#define US                      31
#define RS                      30
#define FF                      12
#define HT                       9
#define VT                      11
#define BS                       8
#define CAN                     24
#define REP                     18
#define NUL                      0
#define BEL                      7
#define DEL                    127
#define ESC                     27  
#define B1			48	/* pour DRCS */

/* attributs de visualisation */
/* couleurs de caracteres */
#define CARACTERE_NOIR	        64
#define CARACTERE_ROUGE         65
#define CARACTERE_VERT          66
#define CARACTERE_JAUNE         67
#define CARACTERE_BLEU          68
#define CARACTERE_MAGENTA       69
#define CARACTERE_CYAN          70
#define CARACTERE_BLANC         71

/* couleur de fond */
#define FOND_NOIR	        80
#define FOND_ROUGE              81
#define FOND_VERT               82
#define FOND_JAUNE              83
#define FOND_BLEU               84
#define FOND_MAGENTA            85
#define FOND_CYAN               86
#define FOND_BLANC              87

/* taille des caracteres */
#define NORMALE                 76
#define DOUBLE_HAUTEUR          77
#define DOUBLE_LARGEUR          78
#define DOUBLE_GRANDEUR         79  

/* clignotement */
#define LETTRES_FIXES		73
#define LETTRES_CLIGNOTANTES	72

/* masquage */
#define MASQUAGE                 88
#define DEMASQUAGE               95

/* soulignement  */
#define FIN_LIGNAGE              89
#define DEBUT_LIGNAGE            90

/* inversion de fond */
#define POSITIF                  92
#define NEGATIF                  93

/* selection de jeu */
#define SO                        14
#define SI   	                  15
#define SYN  	                  22
#define SS2  	                  25

/* Caracteres de controle non imprimables  */
#define SOH                        1
#define STX                        2
#define ETX                        3
#define EOT                        4
#define ENQ	                   5
#define DC1                       17
#define SEP                       19
#define DC4                       20
#define SUB                       26

#define DELIMITEUR_EXPLICITE	1
#define DELIMITEUR_GRAPHIQUE	2

#define BIT_COULEUR_FOND	1
#define BIT_LIGNAGE		2
#define BIT_MASQUAGE		4

/* position par rapport a l'origine du caractere */
#define POS_BG			0   	/* bas a gauche (toutes tailles) */
#define POS_HG			1	/* haut a gauche (double hauteur, double taille) */
#define POS_BD			2	/* bas a droite (double largeur, double taille) */
#define POS_HD			3	/* haut a droite (double taille) */

/* fontes */
#define G0			0
#define G1			1
#define G2			2

#define Ligne_blanche "                                        "

/*
 * Macro de calcul du x, y de trace courant en fonction de la 
 * position rangee, colonne
 */
#define Y_COURANT pv->y_courant = (pv->rangee + 1) * pv->hauteur_fonte_base - pv->descent_fonte_base;
#define X_COURANT pv->x_courant = (pv->colonne - 1) * pv->largeur_fonte_base;

/* 
 * Structure de definition des attributs 
 */
struct attribut {
    char code[2];		/* code du caractere */
    char inversion;		/* attribut d'inversion */
    char couleur_caractere;	/* attribut de couleur de caractere */
    char taille_caractere;	/* attribut de taille de caractere */
    char jeu;			/* jeu de caractere (G0, G1, G2) */
    char couleur_fond;		/* attribut de couleur de fond */
    char masquage;		/* attribut de masquage */
    char soulignement;		/* attribut de lignage */
    Boolean drcs;		/* DRCS ? */
    char clignotement;		/* attribut de clignotement */
	 struct
	 {
		 char position;	 /* position si >taille normale: BG (bas gauche), HG, BD, HD */
		 char delimiteur;/* indique si c'est un delimiteur */
	 } f;
};

typedef enum {
    EtatNormal, EtatESC, EtatUS, EtatREP, EtatSS2, EtatDetectionEnTeteDRCS, EtatChargementEnTeteDRCS, EtatChargementFonteDRCS, EtatAttenteB1, EtatCSI, EtatSEP
} etatAutomate; 

struct position {
    char rangee;
    char colonne;
    int x;
    int y;
    struct position *suivant;
};

/* Structures de definition du widget */
#include "Videotex.h"
#include <X11/CoreP.h>

/* New fields for the videotex widget instance record */
typedef struct {
    /* resources */
    Boolean	petite_fonte;		/* petite fonte (8x10) ? */ 
    Boolean	niveaux_de_gris;	/* ecran en niveaux de gris ? */
    Boolean	connecte;		/* Minitel connecte ? */
    int		fd_connexion;		/* connexion */
    String	commande_deconnexion;	/* Commande de deconnexion */
    Boolean	mode_videotex;		/* Mode Videotex ? */
    String	enqrom;			/* Reponse a ENQROM */

    /* Fontes */
    XFontStruct *fonte_g0_no_p;		/* Petite taille */
    XFontStruct *fonte_g0_dl_p;
    XFontStruct *fonte_g0_dh_p;
    XFontStruct *fonte_g0_dt_p;
    XFontStruct *fonte_g0_nos_p;
    XFontStruct *fonte_g0_dls_p;
    XFontStruct *fonte_g0_dhs_p;
    XFontStruct *fonte_g0_dts_p;
    XFontStruct *fonte_g1_p;
    XFontStruct *fonte_g1s_p;
    XFontStruct *fonte_g0_no_g;		/* Grande taille */
    XFontStruct *fonte_g0_dl_g;
    XFontStruct *fonte_g0_dh_g;
    XFontStruct *fonte_g0_dt_g;
    XFontStruct *fonte_g0_nos_g;
    XFontStruct *fonte_g0_dls_g;
    XFontStruct *fonte_g0_dhs_g;
    XFontStruct *fonte_g0_dts_g;
    XFontStruct *fonte_g1_g;
    XFontStruct *fonte_g1s_g;

    /* Callbacks */
    XtCallbackList callback_mode;	/* Chgmt de mode (Videotex/Tele) */
    XtCallbackList callback_enreg;	/* fonction d'enregistrement des commandes */
    /* Private data */
    etatAutomate etat;                  /* etat de l'automate (ESC, US, REP, SS2) */
    char a_recevoir;			/* nombre de caractere a recevoir */
    char rangee;			/* rangee (ligne) courante */
    char colonne;		        /* colonne courante */	        
    char caractere_sauve;	        /* caractere imprimable courant */
    char accent_sauve;		     	/* accent courant */
    Boolean flag_G2;		        /* vrai si caractere a sauver est de G2 */
    char couleur_caractere;    		/* couleur courante du caractere */
    char couleur_fond;		    	/* couleur courante du fond */
    char taille_caractere; 	    	/* taille courante caractere */
    char insertion;                     /* attribut d'insertion ignore */  
    char inversion;                    	/* pas d'inversion de couleurs */
    char masquage;                  	/* attribut de masquage ignore */  
    char soulignement;             	/* lignage */
    char clignotement;           	/* clignotement */
    char jeu_courant;                   /* jeu de caracteres */
    char largeur_caractere;		/* increment horizontal */
    char hauteur_caractere;		/* increment vertical */
    int  x_courant, y_courant;		/* coordonnees courantes d'affichage */
    char hauteur_fonte_base;		/* caracteristiques de la fonte de base */
    char largeur_fonte_base;		/* largeur de la fonte initiale */
    char descent_fonte_base;		/* descent */
    char ascent_fonte_base;             /* ascent, ascent+descent = hauteur */
    struct attribut attributs[25][40];	/* attributs des caracteres */
    Dimension largeur_ecran, hauteur_ecran;	/* dimensions de l'ecran en pixels */
    Boolean flag_bs;				/* flag de backing store */
    Boolean flag_sauvegarde;			/* flag de sauvegarde ecran */
    char derniere_rangee_avant_0;		
    char derniere_colonne_avant_0; 
    char indice_couleur_caractere_precedent;
    char indice_couleur_fond_precedent;
    char indice_couleur_caractere;
    char indice_couleur_fond;
    XFontStruct *fontes[24];		/* liste des fontes utilisees */
    XFontStruct *xfs_courante;		/* fonte courante */
    XRectangle r_clipping_normal[3];	/* rectangle de clipping = ecran - indicateur cx/fin */
    XColor	couleurs[8];		/* liste des definitions de couleurs */
    Pixmap	pixmap[8];		/* Si ecran monochrome */
    Pixmap	ecran_masquage;		/* pour masquage/demasquage */
    Pixmap	ecran_demasquage;
    Pixmap	ecran_sauve;		/* si pas de backing-store */
    GC 	gc;				/* les contextes graphiques */
    GC 	gc_curseur;
    GC	gc_indicateur;
    GC	gc_pixmap; 
    char a_valider_si_delimiteur;
    char inversion_avant_0;
    char couleur_caractere_avant_0;
    char taille_caractere_avant_0;
    char jeu_avant_0;
    Boolean filtrage, pro2, pro1, plein_ecran;
    char erreur_US, valeur_fonction;
    Boolean passage_teleinfo;
    char colonne_sauve, rangee_sauve;
    char valeur_couleur_fond, valeur_lignage, valeur_masquage;
    char mode_scroll;			
    Boolean masquage_plein_ecran;	/* vrai si masquage plein ecran (defaut) */
    char nb_a_ignorer;
    char chaine_repetition[40];
    char repetition;
    int indice_repetition;
    int x_courant_repetition;
    int x_fin_repetition;
    struct attribut *p_attribut_re_affichage;
    char chaine_a_re_afficher[40];
    int indice_re_affichage;
    int x_courant_re_affichage;
    Boolean flag_quelque_chose_a_reafficher;
    char flip_flop;
    int phase_curseur;
    Boolean flag_insertion;
    char buffer_csi[10];
    int nb_csi;
    /* Pointeurs de fonctions d'affichage */
    void (*fonction_affichage_curseur)();
    void (*affiche_caractere_avec_fond)();
    void (*affiche_caractere_sans_fond)();
    void (*affiche_chaine_avec_fond)();
    void (*affiche_fond_pour_clignotement)();
    int (*draw_string_sauve)();
    int (*draw_image_string_sauve)();
    int (*fill_rectangle_sauve)();
    int (*copy_area_sauve)();
    /* Drcs extension */
    Boolean chargement_set;
    Boolean flag_GP1_dans_G0;
    Boolean flag_GP10_dans_G1;
    char code_DRCS_courant;
    int numero_jeu_DRCS_a_charger;
    int numero_jeu_DRCS_prevu;
    char **jeu_DRCS_courant;
    char *jeu_GP1[94];
    char *jeu_GP10[94];
    char *bits_bitmap;
    Boolean flag_drcs;
    unsigned int largeur_bitmap, hauteur_bitmap;
    struct position *affichage_GP1[94];
    struct position *affichage_GP10[94];
    struct position *affichage_DRCS_courant;
    void (*affiche_chaine_drcs)();
#ifndef NO_TVR
    int (*fonctions_lecture_headers[5][5])();
    double x_photo, y_photo;
    double w_photo, h_photo;
    int x_image, y_image;
    double offset_x_image, offset_y_image;
    int w_image, h_image;
    int longueur_data_jpeg;
    XRectangle rectangle_photo;
#endif /* NO_TVR */
} VideotexPart;

/* Full instance record declaration */
typedef struct _VideotexRec {
   CorePart 	core;
   VideotexPart videotex;
   } VideotexRec;

/* New fields for the Videotex widget class record */
typedef struct {int dummy;} VideotexClassPart;

/* Full class record declaration. */
typedef struct _VideotexClassRec {
   CoreClassPart 	core_class;
   VideotexClassPart 	videotex_class;
   } VideotexClassRec;

/* Class pointer. */
extern VideotexClassRec videotexClassRec;

#endif /* _VideotexP_h */
