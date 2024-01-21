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
static char rcsid[] = "$Id: Videotex.c,v 1.23 1997/02/21 16:55:31 pierre Exp $";

/* 
 * Widget Videotex:	VideotexWidgetClass
 */
#include <malloc.h>
#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>
#include "VideotexP.h"


#ifdef __STDC__

static void nulle(void);
static void touche_clavier(Widget w, XKeyEvent *pevent, String *params, Cardinal nb_params);
static void emission_commande(Widget w, XEvent *event, String *params, Cardinal nb_params);
static void reconnaissance_commande(Widget w, XButtonEvent *event, String *params, Cardinal nb_params);
static void conversion_ascii_page(Widget w, XButtonEvent *event, String *params, Cardinal nb_params);
static void init_fonctions_affichage(VideotexWidget w, int nb_plans);
static void clipping_local(VideotexWidget w);
static void affiche_indicateur_connexion(VideotexWidget w);
static void fixe_les_couleurs_version_couleur(VideotexWidget w);
static void fixe_les_couleurs_version_monochrome(VideotexWidget w);
static void affiche_fond_pour_clignotement_version_monochrome(VideotexWidget w, int i, int x, int y, unsigned int l, unsigned int h);
static void affiche_fond_pour_clignotement_version_couleur(VideotexWidget w, int i, int x, int y, unsigned int l, unsigned int h);
static void affiche_caractere_avec_fond_version_couleur(VideotexWidget w, int car);
static void affiche_caractere_avec_fond_version_monochrome(VideotexWidget w, int car);
static void j_affiche_la_chaine_couleur(VideotexWidget w, Drawable d, GC gc, register int x, register int y, register char *s, register int l);
static void affiche_chaine_avec_fond_version_couleur(VideotexWidget w, register char *s, register int l, register int x, register int y);
static void affiche_chaine_avec_fond_version_monochrome(VideotexWidget w, register char *s, register int l, register int x, register int y);
static void affiche_caractere_sans_fond_version_couleur(VideotexWidget w, int car);
static void affiche_caractere_sans_fond_version_monochrome(VideotexWidget w, int car);
static void traite_debordement(VideotexWidget w);
static void caractere_suivant(VideotexWidget w);
static int re_affiche_caractere(VideotexWidget w, int indice_rangee, register struct attribut *patt);
static void affiche_caractere_normal_en_mode_repetition(VideotexWidget w, int c);
static void affiche_caractere_normal(VideotexWidget w, int c);
static void repetition_caractere_sauve(VideotexWidget w, int rep);
static void reaffiche_rectangle_ecran(VideotexWidget w, int r1, int c1, int r2, int c2);
static void init_attributs(VideotexWidget w);
static void sauve_les_attributs_du_caractere(VideotexWidget w, int c0, int c1);
static void recopie_les_attributs_delimiteur_explicite(VideotexWidget w, int strchr);
static void recopie_les_attributs_delimiteur_graphique(VideotexWidget w, int strchr);
static void recopie_les_attributs_si_ecriture_sur_delimiteur_explicite(VideotexWidget w, int strchr);
static void recopie_les_attributs_si_ecriture_sur_delimiteur_graphique(VideotexWidget w, int strchr);
static void reaffiche_zone(VideotexWidget w, int strchr);
static void traitement_delimiteur_graphique(VideotexWidget w);
static void traitement_delimiteur_explicite(VideotexWidget w, int c);
static void selection_attributs_zone_accueil(VideotexWidget w);
static void fixe_attribut_de_rangee_masquage(VideotexWidget w, int masquage, int colonne_depart);
static int accentue(register struct attribut *p);
static int fixe_les_attributs_d_un_bloc(VideotexWidget w, int r, int c, int h);
static int maj_si_ecrasement(VideotexWidget w, int r, int c, int taille);
static void clignotement(VideotexWidget w, int c);
static XtTimerCallbackProc clignote(VideotexWidget w, XtIntervalId *id);
static int init_couleurs(VideotexWidget w);
static void couleur_caractere(VideotexWidget w, int couleur);
static void couleur_fond(VideotexWidget w, int couleur);
static void affiche_curseur(VideotexWidget w, int force);
static XtTimerCallbackProc clignote_curseur(VideotexWidget w, XtIntervalId *id);
static void curseur_actif(VideotexWidget w);
static void curseur_inactif(VideotexWidget w);
static void efface_rectangle_ecran(VideotexWidget w, int, int, int, int);
static void remplissage_ligne(VideotexWidget w);
static void selection_fonte(VideotexWidget w, int f);
static void selection_jeu(VideotexWidget w, int jeu);
static void inversion(VideotexWidget w, int c);
static void masquage(VideotexWidget w, int masque);
static void demasquer_le_plein_ecran(VideotexWidget w);
static void masquer_le_plein_ecran(VideotexWidget w);
static void demasque_un_caractere(VideotexWidget w, int r, int c);
static void masque_un_caractere(VideotexWidget w, int r, int c);
static void scroll_haut(VideotexWidget w, int rangee, int n);
static void scroll_bas(VideotexWidget w, int rangee, int n);
static void scroll_droite (VideotexWidget w, int colonne, int n);
static void scroll_gauche (VideotexWidget w, int colonne, int n);
static void sonnerie(VideotexWidget w);
static void soulignement(VideotexWidget w, int lignage);
static void taille_caractere(VideotexWidget w, int taille);
static void sauve_avant_rangee_0(VideotexWidget w);
static void rangee_suivante(VideotexWidget w);
static void colonne_suivante(VideotexWidget w);
static void rangee_precedente(VideotexWidget w);
static void colonne_precedente(VideotexWidget w);
static void attributs_caractere_par_defaut(VideotexWidget w);
static void attributs_serie_par_defaut(VideotexWidget w);
static void Initialize(Widget greq, Widget gnew, ArgList args, Cardinal *num_args);
static void Realize(Widget gw, XtValueMask *valueMask, XSetWindowAttributes *attrs);
static void Destroy(Widget w);
static void Redisplay(Widget w, XEvent *pevent, Region region);
static void Resize(Widget gw);
static Boolean SetValues(Widget current, Widget request, Widget new, ArgList args, Cardinal *num_args);
static int atoni(char *str, int n);
static void csi_positionnement (VideotexWidget w, int r, int c);
static void csi_insertion_caractere (VideotexWidget, int);
static void csi_insertion_rangee (VideotexWidget, int);
static void csi_suppression_caractere (VideotexWidget, int);
static void csi_suppression_rangee (VideotexWidget, int);
static void traite_csi (VideotexWidget w);
static int g2_vers_g0 (int c);
/* DRCS */
static void init_DRCS (VideotexWidget w);
static void alloc_DRCS (VideotexWidget w);
static void init_US (VideotexWidget w);
static void cree_bitmap_X (char *pt, register unsigned int l, char **pt_rangement);
static void cree_bitmap_DRCS (VideotexWidget w, int car);
static void affiche_chaine_drcs_version_monochrome (VideotexWidget w, Drawable d, GC gc, register int x, register int y, char *s, register int l);
static void affiche_chaine_drcs_version_couleur (VideotexWidget w, Drawable d, GC gc, register int x, register int y, char *s, register int l);
static void libere (struct position *);

/* TVR */
#ifndef NO_TVR
extern Boolean decode_header_tvr (VideotexWidget, int *);
extern void decode_jpeg_tvr (VideotexWidget);
extern void init_tvr (VideotexWidget);
extern void libere_couleurs_tvr (VideotexWidget);
#endif

#else 

static void nulle( );
static void touche_clavier( );
static void emission_commande( );
static void reconnaissance_commande( );
static void conversion_ascii_page( );
static void init_fonctions_affichage( );
static void clipping_local( );
static void affiche_indicateur_connexion( );
static void fixe_les_couleurs_version_couleur( );
static void fixe_les_couleurs_version_monochrome( );
static void affiche_fond_pour_clignotement_version_monochrome( );
static void affiche_fond_pour_clignotement_version_couleur( );
static void affiche_caractere_avec_fond_version_couleur( );
static void affiche_caractere_avec_fond_version_monochrome( );
static void j_affiche_la_chaine_couleur( );
static void affiche_chaine_avec_fond_version_couleur( );
static void affiche_chaine_avec_fond_version_monochrome( );
static void affiche_caractere_sans_fond_version_couleur( );
static void affiche_caractere_sans_fond_version_monochrome( );
static void traite_debordement( );
static void caractere_suivant( );
static int re_affiche_caractere( );
static void affiche_caractere_normal_en_mode_repetition( );
static void affiche_caractere_normal( );
static void repetition_caractere_sauve( );
static void reaffiche_rectangle_ecran( );
static void init_attributs( );
static void sauve_les_attributs_du_caractere( );
static void recopie_les_attributs_delimiteur_explicite( );
static void recopie_les_attributs_delimiteur_graphique( );
static void recopie_les_attributs_si_ecriture_sur_delimiteur_explicite( );
static void recopie_les_attributs_si_ecriture_sur_delimiteur_graphique( );
static void reaffiche_zone( );
static void traitement_delimiteur_graphique( );
static void traitement_delimiteur_explicite( );
static void selection_attributs_zone_accueil( );
static void fixe_attribut_de_rangee_masquage( );
static int accentue( );
static int fixe_les_attributs_d_un_bloc( );
static int maj_si_ecrasement( );
static void clignotement( );
static XtTimerCallbackProc clignote( );
static int init_couleurs( );
static void couleur_caractere( );
static void couleur_fond( );
static void affiche_curseur( );
static XtTimerCallbackProc clignote_curseur( );
static void curseur_actif( );
static void curseur_inactif( );
static void efface_rectangle_ecran( );
static void remplissage_ligne( );
static void selection_fonte( );
static void selection_jeu( );
static void inversion( );
static void masquage( );
static void demasquer_le_plein_ecran( );
static void masquer_le_plein_ecran( );
static void demasque_un_caractere( );
static void masque_un_caractere( );
static void scroll_haut( );
static void scroll_bas( );
static void scroll_droite ( );
static void scroll_gauche ( );
static void sonnerie( );
static void soulignement( );
static void taille_caractere( );
static void sauve_avant_rangee_0( );
static void rangee_suivante( );
static void colonne_suivante( );
static void rangee_precedente( );
static void colonne_precedente( );
static void attributs_caractere_par_defaut( );
static void attributs_serie_par_defaut( );
static void Initialize( );
static void Realize( );
static void Destroy( );
static void Redisplay( );
static void Resize( );
static Boolean SetValues( );
static void scroll_droite ( );
static int atoni( );
static void csi_positionnement ( );
static void csi_insertion_caractere ( );
static void csi_insertion_rangee ( );
static void csi_suppression_caractere ( );
static void csi_suppression_rangee ( );
static void traite_csi ( );
static int g2_vers_g0 ( );
/* DRCS */
static void init_DRCS ();
static void alloc_DRCS ();
static void init_US ();
static void cree_bitmap_X ();
static void cree_bitmap_DRCS ();
static void affiche_chaine_drcs_version_monochrome ();
static void affiche_chaine_drcs_version_couleur ();
static void libere ();

/* TVR */
#ifndef NO_TVR
extern Boolean decode_header_tvr ();
extern void init_tvr ();
extern void libere_couleurs_tvr ();
#endif
#endif /* __STDC__ */

static void erreur_videotex ( );

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

#define offset(field) XtOffsetOf(VideotexRec, field)
static XtResource resources[] = {
    {XtNpetiteFonte, XtCPetiteFonte, XtRBoolean, sizeof(Boolean),
	offset(videotex.petite_fonte), XtRImmediate, (XtPointer)False},
    {XtNnGris, XtCNGris, XtRBoolean, sizeof(Boolean),
	offset(videotex.niveaux_de_gris), XtRImmediate, (XtPointer)False},
    {XtNconnecte, XtCConnecte, XtRBoolean, sizeof(Boolean),
	offset(videotex.connecte), XtRImmediate, (XtPointer)False},
    {XtNfdConnexion, XtCFdConnexion, XtRInt, sizeof (int),
	offset(videotex.fd_connexion), XtRImmediate, (XtPointer) -1},
    {XtNcommandeDeconnexion,  XtCCommandeDeconnexion, XtRString, sizeof(String),
	offset(videotex.commande_deconnexion), XtRString, NULL},
    {XtNenregCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
      offset(videotex.callback_enreg), XtRCallback, (XtPointer)NULL},
    {XtNmodeCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
      offset(videotex.callback_mode), XtRCallback, (XtPointer)NULL},
    {XtNmodeVideotex, XtCModeVideotex, XtRBoolean, sizeof(Boolean), 
      offset(videotex.mode_videotex), XtRImmediate, (XtPointer)True},
/* Fontes G0, petite */
    {XtNfonteG0no_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_no_p), XtRString, XtDefaultFont},
    {XtNfonteG0dl_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dl_p), XtRString, XtDefaultFont},
    {XtNfonteG0dh_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dh_p), XtRString, XtDefaultFont},
    {XtNfonteG0dt_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dt_p), XtRString, XtDefaultFont},
/* Fontes G0 souligne */
    {XtNfonteG0nos_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_nos_p), XtRString, XtDefaultFont},
    {XtNfonteG0dls_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dls_p), XtRString, XtDefaultFont},
    {XtNfonteG0dhs_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dhs_p), XtRString, XtDefaultFont},
    {XtNfonteG0dts_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dts_p), XtRString, XtDefaultFont},
/* Fonte G1/G1 mosaique */
    {XtNfonteG1_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g1_p), XtRString, XtDefaultFont},
    {XtNfonteG1s_p, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g1s_p), XtRString, XtDefaultFont},
/* Fontes G0, grande */
    {XtNfonteG0no_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_no_g), XtRString, XtDefaultFont},
    {XtNfonteG0dl_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dl_g), XtRString, XtDefaultFont},
    {XtNfonteG0dh_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dh_g), XtRString, XtDefaultFont},
    {XtNfonteG0dt_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dt_g), XtRString, XtDefaultFont},
/* Fontes G0 souligne */
    {XtNfonteG0nos_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_nos_g), XtRString, XtDefaultFont},
    {XtNfonteG0dls_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dls_g), XtRString, XtDefaultFont},
    {XtNfonteG0dhs_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dhs_g), XtRString, XtDefaultFont},
    {XtNfonteG0dts_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g0_dts_g), XtRString, XtDefaultFont},
/* Fonte G1/G1 mosaique */
    {XtNfonteG1_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g1_g), XtRString, XtDefaultFont},
    {XtNfonteG1s_g, XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      offset(videotex.fonte_g1s_g), XtRString, XtDefaultFont},

};
#undef offset

static void Initialize(), Realize(), Destroy(), Resize(), Redisplay();
static Boolean SetValues();

/*
 * Table de correspondance entre la position d'un caractere de G2
 * dans la fonte G0 (les caracteres de G2 sont maintenant dans G0 entre 0 et 31
 * ce qui permet d'economiser le chargement de 4 fontes...
 */
static struct {
    int indice_g2;
    int indice_g0;
} table_g2_g0[] = {
    {35, 0},
    {36, 36},
    {38, 35},
    {44, 1},
    {45, 2},
    {46, 3},
    {47, 4},
    {48, 5},
    {49, 6},
    {56, 7},
    {60, 8},
    {61, 9},
    {62, 10},
    {65, 11},
    {66, 12},
    {67, 13},
    {72, 14},
    {75, 15},
    {106, 16},
    {122, 17},
    {123, 18},
    {39, 19},
    {-1, 0}
};

#ifdef USE_MOTIF
static char defaultVideotexTranslations [] =
"\
<Btn1Down>:		reconnaissance-commande() \n\
<Btn3Down>:		conversion-ascii-page() \n\
<Key>Return:		emission-commande(A) \n\
<Key>osfPageUp:		emission-commande(B) \n\
<Key>osfInsert:		emission-commande(C) \n\
<Key>osfEndLine:	emission-commande(D) \n\
<Key>osfDelete:		emission-commande(E) \n\
<Key>osfBeginLine:	emission-commande(F) \n\
Ctrl<Key>S:		nulle() \n\
Ctrl<Key>H:		emission-commande(G) \n\
<Key>osfPageDown:	emission-commande(H) \n\
<Key>:			touche-clavier() \
";
#else
static char defaultVideotexTranslations [] =
"\
<Btn1Down>:	reconnaissance-commande() \n\
<Btn3Down>:	conversion-ascii-page() \n\
<Key>Return:	emission-commande(A) \n\
<Key>Prior:	emission-commande(B) \n\
<Key>Insert:	emission-commande(C) \n\
<Key>End:	emission-commande(D) \n\
<Key>Delete:	emission-commande(E) \n\
<Key>Home:	emission-commande(F) \n\
Ctrl<Key>S:	nulle() \n\
Ctrl<Key>H:	emission-commande(G) \n\
<Key>Next:	emission-commande(H) \n\
<Key>:		touche-clavier() \
";
#endif /* USE_MOTIF */

static XtActionsRec videotexActionsTable [] = {
  {"nulle",			(XtActionProc)nulle},
  {"touche-clavier",		(XtActionProc)touche_clavier},
  {"emission-commande",		(XtActionProc)emission_commande},
  {"reconnaissance-commande",	(XtActionProc)reconnaissance_commande},
  {"conversion-ascii-page",	(XtActionProc)conversion_ascii_page}
};


static struct  {
    char *nom;
    char *alias;
    char *code;
} commandes_minitel[] = { 
    {"envoi", "", "A"},
    {"retour", "", "B"},
    {"repetition", "", "C"},
    {"guide", "", "D"},
    {"annulation", "annul.", "E"},
    {"sommaire", "", "F"},
    {"correction", "correc.", "G"},
    {"suite", "", "H"},
    {"connexion/fin", "cx/fin", "I"}
};

VideotexClassRec videotexClassRec = {
  {
#define superclass			(&widgetClassRec)
    /* superclass	  	*/	(WidgetClass)superclass,
    /* class_name	  	*/	"Videotex",
    /* widget_size	  	*/	sizeof(VideotexRec),
    /* class_initialize   	*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	Realize,
    /* actions		  	*/	videotexActionsTable,
    /* num_actions	  	*/	XtNumber(videotexActionsTable),
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	Destroy,
    /* resize		  	*/	Resize,
    /* expose		  	*/	Redisplay,
    /* set_values	  	*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus	 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	defaultVideotexTranslations,
    /* query_geometry		*/	NULL/*XtInheritQueryGeometry*/,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
};
WidgetClass videotexWidgetClass = (WidgetClass)&videotexClassRec;

/* tableaux des attributs initiaux */
static struct attribut attributs_initiaux[40];      

/* pointeurs de fonction */
static void (*fonctions_affiche_caractere_avec_fond[])() = {
    affiche_caractere_avec_fond_version_couleur, 
    affiche_caractere_avec_fond_version_monochrome
    };

static void (*fonctions_affiche_caractere_sans_fond[])() = {
    affiche_caractere_sans_fond_version_couleur, 
    affiche_caractere_sans_fond_version_monochrome
    };
    
static void (*fonctions_affiche_chaine_avec_fond[])() = {
    affiche_chaine_avec_fond_version_couleur, 
    affiche_chaine_avec_fond_version_monochrome
    };

static void (*fonctions_affiche_chaine_drcs[])() = {
    affiche_chaine_drcs_version_couleur, 
    affiche_chaine_drcs_version_monochrome
    };

static void (*fonctions_affiche_fond_pour_clignotement[])() = {
    affiche_fond_pour_clignotement_version_couleur,
    affiche_fond_pour_clignotement_version_monochrome
    };


/* declaration des couleurs utilisees pour un ecran couleurs */
static char *nom_couleur[] = {
    "black",
    "red",
    "green",
    "yellow",
    "blue",
    "magenta",
    "cyan",
    "white"
  };

/* Niveaux de gris pour un ecran 16 niveaux */
static char *nom_niveau[] = {
    "black",
    "grey63",
    "grey76",
    "grey88",
    "grey57",
    "grey69",
    "grey82",
    "white"
  };

/* 
 * Pour DRCS 
 */

static unsigned char masktable[] = {
	0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80 };

static void calcul_flag_drcs (w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
	
    pv->flag_drcs = (((pv->flag_GP1_dans_G0 && pv->jeu_courant == G0) || (pv->flag_GP10_dans_G1 && pv->jeu_courant == G1)) && pv->rangee > 0);
}

static void alloc_DRCS (w)
VideotexWidget w;
{
    register int i;
    register VideotexPart *pv = &w->videotex;

    pv->flag_GP1_dans_G0 = pv->flag_GP10_dans_G1 = False;
    pv->chargement_set = False;

    if (!(pv->bits_bitmap = (char*)calloc (1, 160))) {
	erreur_videotex ("alloc_DRCS: erreur calloc bits_bitmap\n");	
	exit (1);
    }

    for (i = 0 ; i != 94 ; i++) {
	if (!(pv->jeu_GP1[i] = (char*)calloc (1, 14))) {
	    erreur_videotex ("alloc_DRCS: erreur calloc jeu_GP1[%d]\n", i);
	    exit (1);
	}
	if (!(pv->jeu_GP10[i] = (char*)calloc (1, 14))) {
	    erreur_videotex ("alloc_DRCS: erreur calloc jeu_GP10[%d]\n", i);
	    exit (1);
	}
    }

    for (i = 0 ; i != 94 ; i++)
      pv->affichage_GP1[i] = pv->affichage_GP10[i] = NULL;
}

static void init_DRCS (w)
VideotexWidget w;
{
    register int i;
    register VideotexPart *pv = &w->videotex;

    pv->flag_GP1_dans_G0 = pv->flag_GP10_dans_G1 = False;
    pv->chargement_set = False;

    memset (pv->bits_bitmap, 0, 160);
    for (i = 0 ; i != 94 ; i++) {
      memset (pv->jeu_GP1[i], 0, 14);
      memset (pv->jeu_GP10[i], 0, 14);
    }

    for (i = 0 ; i != 94 ; i++) {
      if (pv->affichage_GP1[i]) {
	libere (pv->affichage_GP1[i]);
	pv->affichage_GP1[i] = NULL;
      }
      if (pv->affichage_GP10[i]) {
	libere (pv->affichage_GP10[i]);
	pv->affichage_GP10[i] = NULL;
      }
    }
}

static void init_US (w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;

    pv->etat = EtatUS;
    pv->rangee_sauve = pv->rangee;
    pv->colonne_sauve = pv->colonne;
    pv->a_recevoir = 2;
    pv->code_DRCS_courant = 0;
}

/*
 * Creation d'un bitmap X a partir de donnees binaires
 *
 *	- pt 		: pointeur des donnees
 *	- l  		: largeur en bits (8, 16, 32)
 *	- pt_rangement	: adresse de rangement du resultat
 */
static void cree_bitmap_X (pt, l, pt_rangement)
char *pt;
register unsigned int l;
char **pt_rangement;
{
    register char c, i, *ptc = *pt_rangement;
    register unsigned char mask;

    /* Conversion en bitmap X */
    c = 0;
    mask = 0x80;

    for (i = 0 ; i != l ; i++) {
	int ind = (i & 7);

	if (*pt & mask)
	    c |= masktable[ind];
	
	mask >>= 1;

	if (ind == 7) {
	    *ptc++ = c & 255;
	    pt++;
	    c = 0;
	    mask = 0x80;
	}
    }
    *pt_rangement = ptc;
}

/* 
 * Fonction de creation de bitmap a partir des sextets DRCS
 * Voir la gestion des tailles de caracteres...
 */
static void cree_bitmap_DRCS (w, car)
VideotexWidget w;
int car;
{
    register VideotexPart *pv = &w->videotex;
    int decalage, nb_lignes;
    register int i, j, k, isex, dh, dl;
    int d, idata;
    char c, *pt_bitmap, *pt_sextet;
    unsigned char mask, oldmask, data[4];
    Boolean petit = pv->petite_fonte;
    
    if (!car)
	return;

    isex = d = 0;

    dh = (pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR ? 1 : 0);
    dl = (pv->taille_caractere == DOUBLE_LARGEUR || pv->taille_caractere == DOUBLE_GRANDEUR ? 1 : 0);

    pv->largeur_bitmap = (petit ? 8 : 16);
    pv->hauteur_bitmap = (petit ? 10 : 20);

    pv->largeur_bitmap <<= dl;    
    pv->hauteur_bitmap <<= dh;

    mask = oldmask = (petit ? (dl ? 0xc0 : 0x80) : (dl ? 0xf0 : 0xc0));
    decalage = (petit ? 1 : 2);
    nb_lignes = (petit ? 1 : 2);

    decalage <<= dl;
    nb_lignes <<= dh;
    
    memset (pv->bits_bitmap, 0, 160);
    pt_bitmap = pv->bits_bitmap;

    if (pv->jeu_courant == G0)
	pt_sextet = pv->jeu_GP1[car - '!'];
    else
	pt_sextet = pv->jeu_GP10[car - '!'];

    /* On calcule a partir de la fonte 8x10 */
    for (i = 0 ; i != 10 ; i++) {
	/* calcul de la valeur sur 8 bits */
	c = ((*(pt_sextet+isex) & (0x3f >> d)) << (d+2)) | (*(pt_sextet+isex+1) >> (4-d));
	memset (data, 0, 4);
	idata = 0;
	mask = oldmask;
	k = nb_lignes;
	    
	/* Extension en fonction de la taille du caractere (sur 16 ou 32 bits) */
	for (j = 0 ; j != 8 ; j++) {
	    if (c & (0x80 >> j))
		data[idata] |= mask;
	    mask >>= decalage;
	    if (mask == 0) {
		mask = oldmask;
		idata++;
	    }
	}

	while (k--) 
	    cree_bitmap_X (data, pv->largeur_bitmap, &pt_bitmap);

	isex++;
	d += 2;
	if (d > 4) {
	    d = 0;
	    isex++;
	}
    }
}

static void memorise_affichage (w, car, x, y)
VideotexWidget w;
char car;
int x, y;
{
    register VideotexPart *pv = &w->videotex;
    struct position **ppaf = (pv->jeu_courant == G0 ? &pv->affichage_GP1[car - '!'] : &pv->affichage_GP10[car - '!']);
    register struct position *paf = *ppaf, *pafp = NULL;
    int r, c;

    r = (y + pv->descent_fonte_base - 1) / pv->hauteur_fonte_base;
    c = 1 + (x / pv->largeur_fonte_base);
    /* Verifie que la position n'est pas deja presente */
    while (paf && (paf->rangee != r || paf->colonne != c)) {
	pafp = paf;
	paf = paf->suivant;
    }
    
    if (!paf) {
	struct position *newpaf = (struct position *)calloc(1, sizeof(struct position));
	if (!newpaf) {
	    erreur_videotex ("memorise_affichage: erreur calloc pour caractere %x en %d %d\n", car, x, y);
	    return;
	}
	newpaf->rangee = r;
	newpaf->colonne = c;
	newpaf->x = x;
	newpaf->y = y;
	
	if (pafp) 
	    pafp->suivant = newpaf;
	else
	    *ppaf = newpaf;
    }
}

static void affiche_chaine_drcs_version_monochrome (w, d, gc, x, y, s, l)
VideotexWidget w;
Drawable        d;
GC              gc;
register int	x, y;
register char  *s;
register int    l;
{
    register char *p, cp = 0;
    register VideotexPart *pv = &w->videotex;
    register int y_drcs;
    Pixmap pixmap_drcs_courant;

    
    /* On suppose que:
     *
     *  - indice_couleur_fond != indice_couleur_caractere (teste dans l'appelant)
     *	- le fond est trace
     */ 

    for (p = s ; (p-s) < l ; p++) {
	if (*p == ' ' || *p == 0x7f) {
	    /* A priori, rien a faire pour le ' ' */
	    if (*p == 0x7f) {
		XSetStipple(XtDisplay(w), gc, pv->pixmap[pv->indice_couleur_caractere]);
		XFillRectangle(XtDisplay(w), d, gc, x, y - pv->xfs_courante->ascent, pv->xfs_courante->max_bounds.width, pv->xfs_courante->ascent + pv->xfs_courante->descent);
	    }
	    x += pv->xfs_courante->max_bounds.width;
	}
	else {
	    /* Creation du caractere */
	    if (*p != cp)
		cree_bitmap_DRCS (w, *p);

	    cp = *p;
	    y_drcs = y + pv->descent_fonte_base - pv->hauteur_bitmap;

	    if (!(pixmap_drcs_courant = XCreateBitmapFromData (XtDisplay(w), XtWindow(w), pv->bits_bitmap, pv->largeur_bitmap, pv->hauteur_bitmap))) {
		erreur_videotex ("affiche_chaine_drcs_version_monochrome: erreur creation pixmap_drcs_courant\n");
		return;
	    }

	    /* Clipping sur le Pixmap du caractere */
	    XSetClipOrigin (XtDisplay(w), gc, x, y_drcs);
	    XSetClipMask (XtDisplay(w), gc, pixmap_drcs_courant);
	    XSetStipple (XtDisplay(w), gc, pv->pixmap[pv->indice_couleur_caractere]);
	    XFillRectangle (XtDisplay(w), d, gc, x, y_drcs, pv->largeur_bitmap, pv->hauteur_bitmap);

	    XFlush (XtDisplay(w));
	    XFreePixmap (XtDisplay(w), pixmap_drcs_courant);

	    /* memorise l'affichage */
	    if (d == XtWindow(w)) 
		memorise_affichage (w, *p, x, y);

	    x += pv->largeur_bitmap;
	}
    }

    XSetClipRectangles(XtDisplay(w), gc, 0, 0, pv->r_clipping_normal, 3, YXBanded);
}

static void affiche_chaine_drcs_version_couleur (w, d, gc, x, y, s, l)
VideotexWidget w;
Drawable        d;
GC              gc;
register int	x, y;
register char  *s;
register int    l;
{
    register char *p, cp = 0;
    register VideotexPart *pv = &w->videotex;
    register int y_drcs;
    Pixmap pixmap_drcs_courant;

    for (p = s ; (p-s) < l ; p++) {
	if (*p == ' ' || *p == 0x7f) {
	    XDrawImageString(XtDisplay(w), d, gc, x, y, p, 1);
	    x += pv->xfs_courante->max_bounds.width;
	}
	else {
	    /* Creation du caractere */
	    if (*p != cp)
		cree_bitmap_DRCS (w, *p);

	    y_drcs = y + pv->descent_fonte_base - pv->hauteur_bitmap;
	    cp = *p;

	    if (!(pixmap_drcs_courant = XCreatePixmapFromBitmapData (XtDisplay(w), d, pv->bits_bitmap, pv->largeur_bitmap, pv->hauteur_bitmap, pv->couleurs[pv->indice_couleur_caractere].pixel, pv->couleurs[pv->indice_couleur_fond].pixel, DisplayPlanes(XtDisplay(w), DefaultScreen(XtDisplay(w)))))) {
		erreur_videotex ("affiche_chaine_drcs_version_couleur: erreur creation pixmap_drcs_courant\n");
		return;
	    }

	    /* ecrit le caractere */
	    XCopyArea (XtDisplay(w), pixmap_drcs_courant, d, gc, 0, 0, pv->largeur_bitmap, pv->hauteur_bitmap, x, y_drcs);

	    XFreePixmap (XtDisplay(w), pixmap_drcs_courant);

	    /* memorise l'affichage */
	    if (d == XtWindow(w)) 
		memorise_affichage (w, *p, x, y);

	    x += pv->largeur_bitmap;
	}
    }
}

static void re_affiche_DRCS (w)
VideotexWidget w;
{

    register struct position *paf;
    register VideotexPart *pv = &w->videotex;
    
    for (paf = (pv->numero_jeu_DRCS_a_charger ? pv->affichage_GP10[pv->code_DRCS_courant] : pv->affichage_GP1[pv->code_DRCS_courant])  ; paf ; paf = paf->suivant)
	reaffiche_rectangle_ecran (w, paf->rangee, paf->colonne, paf->rangee, paf->colonne);
}

/* fonction nulle */
static void nulle ()
{}

/* Affichage d'erreur */
static void erreur_videotex (fmt, p1, p2, p3)
char *fmt;
int p1, p2, p3;
{
    fprintf (stderr, "videotexWidget: ");
    fprintf (stderr, fmt, p1, p2, p3);
    fflush (stderr);
}

/* Conversion G2 -> G0 */
static int g2_vers_g0 (c)
int c;
{
    register int i;

    c &= 0x7f;

    for (i = 0 ; table_g2_g0[i].indice_g2 > 0 ; i++) {
	if (table_g2_g0[i].indice_g2 == c)
	    return (table_g2_g0[i].indice_g0);
    }

    return (0);
}

/*
 * Action touche clavier ==> emission vers le fd de connexion ou bien
 * affichage On simule le comportement du Minitel (defaut => Majuscule, shift
 * => minuscule)
 */
static void
touche_clavier(w, pevent, params, nb_params)
Widget          w;
XKeyEvent      *pevent;
String         *params;
Cardinal        nb_params;
{
    char            buf[2];
    KeySym          ks;
    VideotexWidget  vw = (VideotexWidget) w;
    register VideotexPart *pv = &vw->videotex;

    if ((XLookupString(pevent, &buf[0], 1, &ks, 0) != 0) && ((pevent->state & Mod1Mask) == 0) && pv->mode_videotex) {
	if ((pevent->state & (ShiftMask | LockMask)) == 0)
	    buf[0] = toupper(buf[0]);
	else
	    buf[0] = tolower(buf[0]);
	buf[1] = 0;

	if (pv->connecte && pv->fd_connexion > 0)
	    write(pv->fd_connexion, buf, 1);
	else
	    videotexDecode(w, buf[0]);
    }
}

/*
 * fonction associee aux actions sur les touches minitel (raccourcis clavier)
 */

/* ARGSUSED */
static void
emission_commande(w, event, params, nb_params)
Widget          w;
XEvent         *event;
String         *params;
Cardinal        nb_params;
{
    VideotexWidget  vw = (VideotexWidget) w;
    register VideotexPart *pv = &vw->videotex;
    if (pv->connecte && pv->fd_connexion > 0) {
	write(pv->fd_connexion, "\023", 1);
	write(pv->fd_connexion, params[0], 1);
	XtCallCallbackList(w, pv->callback_enreg, (XtPointer)"\023");
	XtCallCallbackList(w, pv->callback_enreg, (XtPointer)params[0]);
    }
} 

/* Reconnaissance de commande Minitel dans la page courante */

/* ARGSUSED */
static void 
reconnaissance_commande(w, event, params, nb_params)
Widget          w;
XButtonEvent   *event;
String         *params;
Cardinal        nb_params;
{
    register int    i, j;
    int             rang, col;
    char            chaine[40];
    VideotexWidget  vw = (VideotexWidget) w;
    register VideotexPart *pv = &vw->videotex;

    if (pv->connecte && pv->fd_connexion > 0) {
	rang = event->y / pv->hauteur_fonte_base;
	col = 1 + (event->x / pv->largeur_fonte_base);

	/* recherche de la chaine */
	/*
	 * On fait un & avec 0x7f car le haut d'un caractere double hauteur
	 * est caracterise par le MSB a 1...
	 */
	for (i = col - 1; i >= 0 && pv->attributs[rang][i].jeu == G0 && (isalnum(pv->attributs[rang][i].code[0] & 0x7f) || ((pv->attributs[rang][i].code[0] & 0x7f) == '.') || ((pv->attributs[rang][i].code[0] & 0x7f) == '/')); i--);

	/* copie */
	i++;
	for (j = 0; i < 40 && pv->attributs[rang][i].jeu == G0 && (isalnum(pv->attributs[rang][i].code[0] & 0x7f) || ((pv->attributs[rang][i].code[0] & 0x7f) == '.') || ((pv->attributs[rang][i].code[0] & 0x7f) == '/')); j++) {
	    chaine[j] = tolower(pv->attributs[rang][i].code[0] & 0x7f);
	    i += (pv->attributs[rang][i].taille_caractere == NORMALE || pv->attributs[rang][i].taille_caractere == DOUBLE_HAUTEUR ? 1 : 2);
	}

	chaine[j] = 0;

	/* Emission de la commande (si elle est reconnue) */
	if (j != 0) {
	    for (i = 0; i != 9; i++) {
		if (strcmp(commandes_minitel[i].nom, chaine) == 0 || strcmp(commandes_minitel[i].alias, chaine) == 0) {
		    write(pv->fd_connexion, "\023", 1);
		    write(pv->fd_connexion, commandes_minitel[i].code, 1);
		    XtCallCallbackList(w, pv->callback_enreg, (XtPointer)"\023");
		    XtCallCallbackList(w, pv->callback_enreg, (XtPointer)commandes_minitel[i].code);

		    return;
		}
	    }
	    /*
	     * Si ce n'est pas une commande et que c'est alpha-numerique,
	     * emission de la chaine
	     */

	    if (!isalnum(chaine[0]))
		return;

	    /* Repasse la chaine en majuscules */
	    for (i = 0; i != strlen(chaine); i++)
		chaine[i] = toupper(chaine[i]);

	    write(pv->fd_connexion, chaine, strlen(chaine));
	}
    }
}

/*
 * Affiche une approximation ASCII de la page courante sur la sortie standard
 * (stdout).
 */
static void
conversion_ascii_page(w, event, params, nb_params)
Widget          w;
XButtonEvent   *event;
String         *params;
Cardinal        nb_params;
{
    videotexConversionAscii (w, stdout);
}

/* Init des fonctions d'affichage */

static void 
init_fonctions_affichage (w, nb_plans)
VideotexWidget w;
int nb_plans;
{
    register VideotexPart *pv = &w->videotex;
    register int    i = (nb_plans == 1);

    pv->affiche_caractere_avec_fond = fonctions_affiche_caractere_avec_fond[i];
    pv->affiche_chaine_avec_fond = fonctions_affiche_chaine_avec_fond[i];
    pv->affiche_chaine_drcs = fonctions_affiche_chaine_drcs[i];
    pv->affiche_caractere_sans_fond = fonctions_affiche_caractere_sans_fond[i];
}


/* Clipping local a la rangee */
static void 
clipping_local(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    XRectangle clip[2];
    int n = 1;

    clip[0].x = 0;
    clip[0].y = pv->rangee * pv->hauteur_fonte_base;
    clip[0].width = pv->largeur_ecran;
    clip[0].height = pv->hauteur_fonte_base;
    if (pv->rangee == 0) {
	clip[0].width -= 2 * pv->largeur_fonte_base;
	clip[1].x = pv->largeur_ecran - pv->largeur_fonte_base;
	clip[1].y = 0;
	clip[1].width = pv->largeur_fonte_base;
	clip[1].height = pv->hauteur_fonte_base;
	n = 2;
    }

    XSetClipRectangles(XtDisplay(w), pv->gc, 0, 0, clip, n, YXBanded);
}

/*
 * Affichage de l'indicateur de connexion F ou C
 */
static void 
affiche_indicateur_connexion(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;

    XDrawImageString(XtDisplay(w), XtWindow(w), pv->gc_indicateur, pv->largeur_ecran - 2 * pv->largeur_fonte_base, pv->ascent_fonte_base, (w->videotex.connecte ? "C" : "F"), 1);
    (*pv->draw_image_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc_indicateur, pv->largeur_ecran - 2 * pv->largeur_fonte_base, pv->ascent_fonte_base, (w->videotex.connecte ? "C" : "F"), 1);
}

/*
 * Pour fixer les couleurs/pixmaps avant ecriture...
 */
static void 
fixe_les_couleurs_version_couleur(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;

    if (pv->inversion == POSITIF) {
	pv->indice_couleur_caractere = pv->couleur_caractere - CARACTERE_NOIR;
	pv->indice_couleur_fond = pv->couleur_fond - FOND_NOIR;
    } else {
	pv->indice_couleur_caractere = pv->couleur_fond - 16 - CARACTERE_NOIR;
	pv->indice_couleur_fond = pv->couleur_caractere + 16 - FOND_NOIR;
    }

    if (pv->indice_couleur_caractere != pv->indice_couleur_caractere_precedent)
	XSetForeground(XtDisplay(w), pv->gc, pv->couleurs[pv->indice_couleur_caractere].pixel);

    if (pv->indice_couleur_fond != pv->indice_couleur_fond_precedent)
	XSetBackground(XtDisplay(w), pv->gc, pv->couleurs[pv->indice_couleur_fond].pixel);

    pv->indice_couleur_caractere_precedent = pv->indice_couleur_caractere;
    pv->indice_couleur_fond_precedent = pv->indice_couleur_fond;
}

static void 
fixe_les_couleurs_version_monochrome(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;

    if (pv->inversion == POSITIF) {
	pv->indice_couleur_caractere = pv->couleur_caractere - CARACTERE_NOIR;
	pv->indice_couleur_fond = pv->couleur_fond - FOND_NOIR;
    } else {
	pv->indice_couleur_caractere = pv->couleur_fond - 16 - CARACTERE_NOIR;
	pv->indice_couleur_fond = pv->couleur_caractere + 16 - FOND_NOIR;
    }
}


/*
 * Affiche le fond pour le clignotement
 */
static void 
affiche_fond_pour_clignotement_version_monochrome(w, i, x, y, l, h)
VideotexWidget w;
int             i, x, y;
unsigned int    l, h;
{
    register VideotexPart *pv = &w->videotex;

    XSetStipple(XtDisplay(w), pv->gc, pv->pixmap[i]);
    XFillRectangle(XtDisplay(w), XtWindow(w), pv->gc, x, y, l, h);
}


static void 
affiche_fond_pour_clignotement_version_couleur(w, i, x, y, l, h)
VideotexWidget w;
int             i, x, y;
unsigned int    l, h;
{
    register VideotexPart *pv = &w->videotex;

    XSetForeground(XtDisplay(w), pv->gc, pv->couleurs[i].pixel);
    XFillRectangle(XtDisplay(w), XtWindow(w), pv->gc, x, y, l, h);
}


/*
 * Affiche un caractere en x_courant, y_courant avec le fond
 */
static void 
affiche_caractere_avec_fond_version_couleur(w, car)
VideotexWidget w;
char car;
{
    register VideotexPart *pv = &w->videotex;
    char taille = pv->taille_caractere;
    void (*sauve_la_chaine_drcs)();

    sauve_la_chaine_drcs = (pv->flag_bs ? (void (*) ()) nulle : affiche_chaine_drcs_version_couleur);

    car &= 0x7F;

    fixe_les_couleurs_version_couleur(w);

    /*
     * On commence par tester si l'on n'est pas en derniere colonne, double
     * largeur
     */
    if ((pv->x_courant / pv->largeur_fonte_base) == 39) {
	if (taille == DOUBLE_LARGEUR)
	    taille_caractere(w, NORMALE);
	else if (taille == DOUBLE_GRANDEUR)
	    taille_caractere(w, DOUBLE_HAUTEUR);
    }

    /* ecrit le caractere dans ecran_demasquage */    
    if (pv->flag_drcs) {
	(*pv->affiche_chaine_drcs) (w, pv->ecran_demasquage, pv->gc, pv->x_courant, pv->y_courant, (char*)&car, 1);
    }
    else {
	XDrawImageString(XtDisplay(w), pv->ecran_demasquage, pv->gc, pv->x_courant, pv->y_courant, (char *)&car, 1);
    }
    
    XDrawImageString(XtDisplay(w), pv->ecran_masquage, pv->gc, pv->x_courant, pv->y_courant, " ", 1);


    if (pv->masquage == MASQUAGE) {
	/* ecrit le caractere dans win si le demasquage p.e. est valide */
	if (!pv->masquage_plein_ecran) {
	    if (pv->flag_drcs) {
		(*pv->affiche_chaine_drcs) (w, XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char*)&car, 1);
		(*sauve_la_chaine_drcs) (w, pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char*)&car, 1);
	    }
	    else {
		XDrawImageString(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		(*pv->draw_image_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *)&car, 1);
	    }
	}
	
    } else {
	if (pv->flag_drcs) {
	    (*pv->affiche_chaine_drcs) (w, XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *)&car, 1);
	    (*sauve_la_chaine_drcs) (w, pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *)&car, 1); 
	    (*pv->affiche_chaine_drcs) (w, pv->ecran_masquage, pv->gc, pv->x_courant, pv->y_courant, (char *)&car, 1);

	}
	else {
	    XDrawImageString(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *)&car, 1);
	    (*pv->draw_image_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *)&car, 1);
	    XDrawImageString(XtDisplay(w), pv->ecran_masquage, pv->gc, pv->x_courant, pv->y_courant, (char *)&car, 1);
	}
    }

    if (taille != pv->taille_caractere)
	taille_caractere(w, taille);
}

static void 
affiche_caractere_avec_fond_version_monochrome(w, car)
VideotexWidget w;
char            car;
{
    register VideotexPart *pv = &w->videotex;
    char            taille = pv->taille_caractere;
    void (*sauve_la_chaine_drcs)();

    sauve_la_chaine_drcs = (pv->flag_bs ? (void (*) ()) nulle : affiche_chaine_drcs_version_monochrome);

    car &= 0x7F;

    fixe_les_couleurs_version_monochrome(w);

    /*
     * commence par tester si on n'est pas en derniere colonne, double
     * largeur
     */
    if ((pv->x_courant / pv->largeur_fonte_base) == 39) {
	if (taille == DOUBLE_LARGEUR)
	    taille_caractere(w, NORMALE);
	else if (taille == DOUBLE_GRANDEUR)
	    taille_caractere(w, DOUBLE_HAUTEUR);
    }
    /*
     * On ecrit les fond dans les 3 ecrans !
     */
    /* fixe le stipple de fond */
    XSetStipple(XtDisplay(w), pv->gc, pv->pixmap[pv->indice_couleur_fond]);

    /* affiche le fond sur les 3 ecrans */
    XFillRectangle(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant - pv->xfs_courante->ascent, pv->xfs_courante->max_bounds.width, pv->xfs_courante->ascent + pv->xfs_courante->descent);
    (*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant - pv->xfs_courante->ascent, pv->xfs_courante->max_bounds.width, pv->xfs_courante->ascent + pv->xfs_courante->descent);
    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc, pv->x_courant, pv->y_courant - pv->xfs_courante->ascent, pv->xfs_courante->max_bounds.width, pv->xfs_courante->ascent + pv->xfs_courante->descent);
    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc, pv->x_courant, pv->y_courant - pv->xfs_courante->ascent, pv->xfs_courante->max_bounds.width, pv->xfs_courante->ascent + pv->xfs_courante->descent);

    /*
     * ecriture des caracteres
     */
    if (pv->indice_couleur_fond != pv->indice_couleur_caractere) {
	if (pv->flag_drcs) {
	    affiche_chaine_drcs_version_monochrome (w, pv->ecran_demasquage, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	}
	else {
	    /* fixe le stipple caractere */
	    XSetStipple(XtDisplay(w), pv->gc, pv->pixmap[pv->indice_couleur_caractere]);
	    /* ecrit le caractere dans ecran_demasquage */
	    XDrawString(XtDisplay(w), pv->ecran_demasquage, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	}

	/*
	 * Si il y a masquage :
	 * 
	 * - on affiche a l'ecran si l'on a valide l'attribut de demasquage
	 * plein ecran par contre, on n'ecrit pas dans pv->ecran_masquage
	 * 
	 * - on affiche dans aucun des 2 si l'attribut de demasquage p.e n'est
	 * pas valide
	 * 
	 * Si il n'y a pas masquage :
	 * 
	 * - on affiche a l'ecran et dans ecran_masquage
	 */
	if (pv->masquage == MASQUAGE) {
	    /* ecrit le caractere dans win si le demasquage p.e. est valide */
	    if (!pv->masquage_plein_ecran) {
		if (pv->flag_drcs) {
		    affiche_chaine_drcs_version_monochrome (w, XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		    (*sauve_la_chaine_drcs) (w, pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		}
		else {
		    XDrawString(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		    (*pv->draw_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		}
	    }
	} else {
	    if (pv->flag_drcs) {
		affiche_chaine_drcs_version_monochrome (w, XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		(*sauve_la_chaine_drcs) (w, pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		affiche_chaine_drcs_version_monochrome (w, pv->ecran_masquage, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	    }
	    else {
		XDrawString(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		(*pv->draw_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		XDrawString(XtDisplay(w), pv->ecran_masquage, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	    }
	}
    }
    if (taille != pv->taille_caractere)
	taille_caractere(w, taille);
}

/*
 * Affiche une chaine avec le fond (utilise par la repetition de caractere)
 */
static void
j_affiche_la_chaine_couleur(w, d, gc, x, y, s, l)
VideotexWidget w;
Drawable        d;
GC              gc;
register int	x, y;
register char  *s;
register int    l;
{
    register VideotexPart *pv = &w->videotex;
    Boolean flag_double = (pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR);

    fixe_les_couleurs_version_couleur(w);

    if (flag_double)
	clipping_local(w);

    if (pv->flag_drcs)
	affiche_chaine_drcs_version_couleur (w, d, gc, x, y, s, l);
    else
	XDrawImageString(XtDisplay(w), d, gc, x, y, s, l);

    if (flag_double && pv->etat == EtatREP) {
	if (pv->rangee) {
	    char cf = pv->couleur_fond;
	    pv->rangee--;
	    clipping_local(w);
	    pv->couleur_fond = pv->attributs[pv->rangee][pv->colonne - 2].couleur_fond;
	    fixe_les_couleurs_version_couleur(w);
	    if (pv->flag_drcs)
		affiche_chaine_drcs_version_couleur (w, d, gc, x, y, s, l);
	    else
		XDrawImageString(XtDisplay(w), d, gc, x, y, s, l);
	    pv->couleur_fond = cf;
	    pv->rangee++;
	}

	XSetClipRectangles(XtDisplay(w), gc, 0, 0, pv->r_clipping_normal, 3, YXBanded);
    }
}

static void 
affiche_chaine_avec_fond_version_couleur(w, s, l, x, y)
VideotexWidget w;
register char  *s;
register int    l, x, y;
{
    register VideotexPart *pv = &w->videotex;
    register int    sl;
    char            c = '\0';
    void            (*sauve_la_chaine) ();

    sauve_la_chaine = (pv->flag_bs ? (void (*) ()) nulle : j_affiche_la_chaine_couleur);

    fixe_les_couleurs_version_couleur(w);

    /*
     * Si la chaine est en double largeur et que le dernier caractere
     * commence en colonne 39, on affiche un caractere de moins et on affiche
     * le dernier a part en passant par la fonction d'affichage de caractere
     * unique
     */
    sl = l * pv->largeur_caractere;
    if (((x / pv->largeur_fonte_base) + sl) > 40) {	
	c = s[--l];
	s[l] = '\0';
    }
    /* ecrit le caractere dans ecran_demasquage */
    j_affiche_la_chaine_couleur(w, pv->ecran_demasquage, pv->gc, x, y, s, l);

    /* ecrit des blancs dans ecran_masquage */
    j_affiche_la_chaine_couleur(w, pv->ecran_masquage, pv->gc, x, y, Ligne_blanche, l);

    if (pv->masquage == MASQUAGE) {
	/* ecrit le caractere dans win si le demasquage p.e. est valide */
	if (!pv->masquage_plein_ecran) {
	    j_affiche_la_chaine_couleur(w, XtWindow(w), pv->gc, x, y, s, l);
	    (*sauve_la_chaine) (w, pv->ecran_sauve, pv->gc, x, y, s, l);
	}
    } else {
	j_affiche_la_chaine_couleur(w, XtWindow(w), pv->gc, x, y, s, l);
	(*sauve_la_chaine) (w, pv->ecran_sauve, pv->gc, x, y, s, l);
	j_affiche_la_chaine_couleur(w, pv->ecran_masquage, pv->gc, x, y, s, l);
    }

    /*
     * Si il reste un caractere a afficher en derniere colonne on appelle
     * affiche_caractere_version_couleur() qui possede tous les tests
     * appropries pour la double largeur
     */
    if (c) {
	int sx = pv->x_courant, sy = pv->y_courant;
	pv->x_courant = pv->largeur_ecran - pv->largeur_fonte_base;
	pv->y_courant = y;
	affiche_caractere_avec_fond_version_couleur(w, c);
	pv->x_courant = sx;
	pv->y_courant = sy;
    }
}

static void 
affiche_chaine_avec_fond_version_monochrome(w, s, l, x, y)
VideotexWidget w;
register char  *s;
register int    l, x, y;
{
    register VideotexPart *pv = &w->videotex;
    register int    sl;
    char            c = '\0';
    register int    largeur = l * pv->xfs_courante->max_bounds.width;
    char            flag_double = (pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR);
    void            (*sauve_la_chaine_drcs) ();

    sauve_la_chaine_drcs = (pv->flag_bs ? (void (*) ()) nulle : affiche_chaine_drcs_version_monochrome);

    fixe_les_couleurs_version_monochrome(w);

    sl = l * pv->largeur_caractere;
    if (((x / pv->largeur_fonte_base) + sl) > 40) {	
	c = s[--l];
	s[l] = '\0';
    }

    /*
     * On ecrit les fond dans les 3 ecrans !
     */
    /* fixe le stipple de fond */
    XSetStipple(XtDisplay(w), pv->gc, pv->pixmap[pv->indice_couleur_fond]);

    if (flag_double)
	clipping_local(w);

    XFillRectangle(XtDisplay(w), XtWindow(w), pv->gc, x, y - pv->xfs_courante->ascent, largeur, pv->xfs_courante->ascent + pv->xfs_courante->descent);
    (*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, x, y - pv->xfs_courante->ascent, largeur, pv->xfs_courante->ascent + pv->xfs_courante->descent);
    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc, x, y - pv->xfs_courante->ascent, largeur, pv->xfs_courante->ascent + pv->xfs_courante->descent);
    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc, x, y - pv->xfs_courante->ascent, largeur, pv->xfs_courante->ascent + pv->xfs_courante->descent);

    if (flag_double && pv->etat == EtatREP) {
	if (pv->rangee) {
	    char cf = pv->couleur_fond;
	    pv->rangee--;
	    clipping_local(w);
	    pv->couleur_fond = pv->attributs[pv->rangee][pv->colonne - 2].couleur_fond;
	    fixe_les_couleurs_version_monochrome(w);
	    XSetStipple(XtDisplay(w), pv->gc, pv->pixmap[pv->indice_couleur_fond]);
	    XFillRectangle(XtDisplay(w), XtWindow(w), pv->gc, x, y - pv->xfs_courante->ascent, largeur, pv->xfs_courante->ascent + pv->xfs_courante->descent);
	    (*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, x, y - pv->xfs_courante->ascent, largeur, pv->xfs_courante->ascent + pv->xfs_courante->descent);
	    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc, x, y - pv->xfs_courante->ascent, largeur, pv->xfs_courante->ascent + pv->xfs_courante->descent);
	    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc, x, y - pv->xfs_courante->ascent, largeur, pv->xfs_courante->ascent + pv->xfs_courante->descent);
	    pv->couleur_fond = cf;
	    fixe_les_couleurs_version_monochrome(w);
	    pv->rangee++;
	}

	XSetClipRectangles(XtDisplay(w), pv->gc, 0, 0, pv->r_clipping_normal, 3, YXBanded);
    }

    /*
     * ecriture des caracteres
     */
    if (pv->indice_couleur_fond != pv->indice_couleur_caractere) {
	if (pv->flag_drcs){
	    affiche_chaine_drcs_version_monochrome (w, pv->ecran_demasquage, pv->gc, x, y, s, l);
	}
	else {
	    /* fixe le stipple caractere */
	    XSetStipple(XtDisplay(w), pv->gc, pv->pixmap[pv->indice_couleur_caractere]);
	    /* ecrit le caractere dans ecran_demasquage */
	    XDrawString(XtDisplay(w), pv->ecran_demasquage, pv->gc, x, y, s, l);
	}

	/*
	 * Si il y a masquage :
	 * 
	 * - on affiche a l'ecran si l'on a valide l'attribut de demasquage
	 * plein ecran par contre, on n'ecrit pas dans ecran_masquage
	 * 
	 * - on affiche dans aucun des 2 si l'attribut de demasquage p.e n'est
	 * pas valide
	 * 
	 * Si il n'y a pas masquage :
	 * 
	 * - on affiche a l'ecran et dans ecran_masquage
	 */
	if (pv->masquage == MASQUAGE) {
	    /* ecrit le caractere dans win si le demasquage p.e. est valide */
	    if (!pv->masquage_plein_ecran) {
		if (pv->flag_drcs) {
		    affiche_chaine_drcs_version_monochrome (w, XtWindow(w), pv->gc, x, y, s, l);
		    (*sauve_la_chaine_drcs) (w, pv->ecran_sauve, pv->gc, x, y, s, l);
		}
		else {
		    XDrawString(XtDisplay(w), XtWindow(w), pv->gc, x, y, s, l);
		    (*pv->draw_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, x, y, s, l);
		}
	    }
	} else {
	    if (pv->flag_drcs) {
		affiche_chaine_drcs_version_monochrome(w, XtWindow(w), pv->gc, x, y, s, l);
		(*sauve_la_chaine_drcs) (w, pv->ecran_sauve, pv->gc, x, y, s, l);
		affiche_chaine_drcs_version_monochrome (w, pv->ecran_masquage, pv->gc, x, y, s, l);
	    }
	    else {
		XDrawString(XtDisplay(w), XtWindow(w), pv->gc, x, y, s, l);
		(*pv->draw_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, x, y, s, l);
		XDrawString(XtDisplay(w), pv->ecran_masquage, pv->gc, x, y, s, l);
	    }
	}
    }

    if (c) {
	int sx = pv->x_courant, sy = pv->y_courant;

	pv->x_courant = pv->largeur_ecran - pv->largeur_fonte_base;
	pv->y_courant = y;
	affiche_caractere_avec_fond_version_monochrome(w, c);
	pv->x_courant = sx;
	pv->y_courant = sy;
    }
}


/*
 * Idem sans le fond
 */
static void 
affiche_caractere_sans_fond_version_couleur(w, car)
VideotexWidget w;
char            car;
{
    register VideotexPart *pv = &w->videotex;
    char            taille = pv->taille_caractere;

    car &= 0x7F;

    fixe_les_couleurs_version_couleur(w);

    /*
     * commence par tester si on n'est pas en derniere colonne, double
     * largeur
     */
    if ((pv->x_courant / pv->largeur_fonte_base) == 39) {
	if (taille == DOUBLE_LARGEUR)
	    taille_caractere(w, NORMALE);
	else if (taille == DOUBLE_GRANDEUR)
	    taille_caractere(w, DOUBLE_HAUTEUR);
    }
    /* ecrit toujours dans l'ecran de demasquage */
    XDrawString(XtDisplay(w), pv->ecran_demasquage, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);

    if (pv->masquage == MASQUAGE) {
	/* ecrit le caractere dans win si le demasquage p.e. est valide */
	if (!pv->masquage_plein_ecran) {
	    XDrawString(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	    (*pv->draw_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	}
    } else {
	XDrawString(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	(*pv->draw_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	XDrawString(XtDisplay(w), pv->ecran_masquage, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
    }

    if (taille != pv->taille_caractere)
	taille_caractere(w, taille);
}


static void 
affiche_caractere_sans_fond_version_monochrome(w, car)
VideotexWidget w;
char            car;
{
    register VideotexPart *pv = &w->videotex;
    char            taille = pv->taille_caractere;

    car &= 0x7F;

    fixe_les_couleurs_version_monochrome(w);

    /*
     * commence par tester si on n'est pas en derniere colonne, double
     * largeur
     */
    if ((pv->x_courant / pv->largeur_fonte_base) == 39) {
	if (taille == DOUBLE_LARGEUR)
	    taille_caractere(w, NORMALE);
	else if (taille == DOUBLE_GRANDEUR)
	    taille_caractere(w, DOUBLE_HAUTEUR);
    }
    if (pv->indice_couleur_fond != pv->indice_couleur_caractere) {

	/* fixe le stipple caractere */
	XSetStipple(XtDisplay(w), pv->gc, pv->pixmap[pv->indice_couleur_caractere]);

	/* ecrit toujours dans l'ecran de demasquage */
	XDrawString(XtDisplay(w), pv->ecran_demasquage, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);

	if (pv->masquage == MASQUAGE) {
	    /* ecrit le caractere dans win si le demasquage p.e. est valide */
	    if (!pv->masquage_plein_ecran) {
		XDrawString(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
		(*pv->draw_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	    }
	} else {
	    XDrawString(XtDisplay(w), XtWindow(w), pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	    (*pv->draw_string_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	    XDrawString(XtDisplay(w), pv->ecran_masquage, pv->gc, pv->x_courant, pv->y_courant, (char *) &car, 1);
	}
    }
    if (taille != pv->taille_caractere)
	taille_caractere(w, taille);
}

/*
 * Fonction de traitement du debordement (colonne > 40)
 */
static void 
traite_debordement(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;

    pv->x_courant = 0;
    pv->y_courant += pv->xfs_courante->ascent + pv->xfs_courante->descent;

    /*
     * Nouveau calcul de la position de fin de repetition
     */
    if (w->videotex.etat == EtatREP) {
	pv->x_fin_repetition = pv->repetition * pv->xfs_courante->max_bounds.width;
	if ((int) pv->x_fin_repetition > (int) pv->largeur_ecran)
	    pv->x_fin_repetition = pv->largeur_ecran;
    }
    if (pv->jeu_courant == G1) {
#ifdef NOTDEF
	/*
	 * On fixe la couleur de fond a NOIR, jusqu'a ce qu'un nouveau
	 * delimiteur la re-valide
	 */
	pv->valeur_couleur_fond = pv->couleur_fond;
	pv->a_valider_si_delimiteur |= BIT_COULEUR_FOND;
	couleur_fond(FOND_NOIR);
#endif
    } else {
	/*
	 * Jeu G0 ==> RAZ de tous les attributs serie
	 */
	couleur_fond(w, FOND_NOIR);
	soulignement(w, FIN_LIGNAGE);
	masquage(w, DEMASQUAGE);
    }

    /*
     * En derniere rangee ?
     */
    if (pv->y_courant >= 25 * pv->hauteur_fonte_base) {
	/*
	 * FP, 18-FEB-93: correction des bugs de recalcul qui deconnaient en
	 * dble hauteur
	 */
	if (!pv->mode_scroll)
	    pv->y_courant = (pv->hauteur_caractere + 1) * pv->hauteur_fonte_base - pv->descent_fonte_base;
	else {
	    scroll_haut(w, 1, 1);
	    if (pv->hauteur_caractere > 1)
		scroll_haut(w, 1, 1);
	    pv->y_courant = 25 * pv->hauteur_fonte_base - pv->descent_fonte_base;
	}
    }
    /* on doit recalculer la valeur de la rangee */
    pv->rangee = (pv->y_courant + pv->descent_fonte_base - 1) / pv->hauteur_fonte_base;
}


/*
 * Passe au caractere suivant
 */
static void 
caractere_suivant(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;

    /* colonne suivante => calcul du nouvel x_courant d'affichage */
    pv->x_courant += pv->xfs_courante->max_bounds.width;

    /* test de debordement */
    if (pv->x_courant >= 40 * pv->largeur_fonte_base) {
	/*
	 * Si l'on est en cours de repetition, on affiche la chaine a
	 * afficher
	 */
	if (pv->etat == EtatREP && !pv->flag_G2) {
	    /* repetition */
	    (*pv->affiche_chaine_avec_fond) (w, pv->chaine_repetition, pv->indice_repetition, pv->x_courant_repetition, pv->y_courant);
	    pv->indice_repetition = 0;
	}
	if (pv->rangee != 0) {
	    traite_debordement(w);
	    pv->x_courant_repetition = pv->x_courant;
	} else			/* reste sur la meme position */
	    pv->x_courant -= pv->xfs_courante->max_bounds.width;
    }
    /* calcul de la colonne en fonction de x_courant */
    pv->colonne = 1 + (pv->x_courant / pv->largeur_fonte_base);
}

/*
 * Re-affiche un caractere en lisant ses attributs dans le tableau
 * "attributs"
 */
static int 
re_affiche_caractere(w, indice_rangee, patt)
VideotexWidget w;
register char   indice_rangee;
register struct attribut *patt;
{
    register VideotexPart *pv = &w->videotex;
    register int    y_re_affichage, l;
    char            c = patt->code[0] & 0x7f;

    l = (patt->taille_caractere == NORMALE || patt->taille_caractere == DOUBLE_HAUTEUR) ? 1 : 2;

    /*
     * On optimise le re-affichage dans le cas d'une repetition : ce n'est
     * pas la peine de re-afficher un caractere si on doit afficher plus tard
     * a sa position...
     */
    if (pv->etat == EtatREP) {
	if ((pv->x_courant + l * pv->largeur_fonte_base) <= pv->x_fin_repetition) {
	    /*
	     * on ne re-affiche pas, mais on met a jour
	     * x_courant_re_affichage
	     */
	    pv->x_courant_re_affichage += l * pv->largeur_fonte_base;
	    return (l);
	}
    }
    /*
     * Si un des attributs de caractere est modifie, affiche le buffer
     */
    if (pv->p_attribut_re_affichage->inversion != patt->inversion ||
      pv->p_attribut_re_affichage->taille_caractere != patt->taille_caractere ||
    pv->p_attribut_re_affichage->couleur_caractere != patt->couleur_caractere ||
	pv->p_attribut_re_affichage->jeu != patt->jeu) {
	/*
	 * Fixe les attributs d'affichage
	 */
	inversion(w, pv->p_attribut_re_affichage->inversion);
	taille_caractere(w, pv->p_attribut_re_affichage->taille_caractere);
	couleur_caractere(w, pv->p_attribut_re_affichage->couleur_caractere);

	selection_jeu(w, pv->p_attribut_re_affichage->jeu);

	/*
	 * Si c'est le haut d'un caractere double hauteur, decale le y
	 */
	if (pv->p_attribut_re_affichage->code[0] & 0x80)
	    y_re_affichage = pv->y_courant + pv->hauteur_fonte_base;
	else
	    y_re_affichage = pv->y_courant;
	(*pv->affiche_chaine_avec_fond) (w, pv->chaine_a_re_afficher, pv->indice_re_affichage, pv->x_courant_re_affichage, y_re_affichage);

	/* prepare un nouveau buffer */
	pv->indice_re_affichage = 0;
	pv->x_courant_re_affichage = pv->x_courant;
	pv->p_attribut_re_affichage = patt;
    }
    /*
     * on bufferise les caracteres (sauf pour les caracteres accentues)
     */

    /* Caractere de G2 ou caractere accentue */
    if (patt->jeu == G2) {
	if ((c < 65 || c > 67) && (c != 72) && (c != 75)) {
	    pv->chaine_a_re_afficher[pv->indice_re_affichage++] = c;
	} else {
	    /* caractere accentue */
	    register int    y_courant_sauve = pv->y_courant;

	    /* Si c'est le haut d'un caractere... */
	    if (patt->code[0] & 0x80) {
		y_courant_sauve = pv->y_courant;
		pv->y_courant += pv->hauteur_fonte_base;
	    }
	    /*
	     * Fixe les attributs d'affichage
	     */
	    inversion(w, patt->inversion);
	    taille_caractere(w, patt->taille_caractere);
	    couleur_caractere(w, patt->couleur_caractere);

	    /* Affiche l'accent, puis le caractere */
	    selection_jeu(w, patt->jeu);
	    (*pv->affiche_caractere_avec_fond) (w, g2_vers_g0(c));
	    selection_jeu(w, G0);
	    (*pv->affiche_caractere_sans_fond) (w, patt->code[1]);

	    pv->y_courant = y_courant_sauve;
	}
    } else
	pv->chaine_a_re_afficher[pv->indice_re_affichage++] = c;

    return (l);
}

/*
 * Affiche un caractere "normal" (avec fond et non accentue)
 */
static void 
affiche_caractere_normal_en_mode_repetition(w, c)
VideotexWidget w;
char            c;
{
    register VideotexPart *pv = &w->videotex;

    sauve_les_attributs_du_caractere(w, c, 0);
    pv->chaine_repetition[pv->indice_repetition++] = c & 0x7f;
    caractere_suivant(w);
}


static void 
affiche_caractere_normal(w, c)
VideotexWidget w;
char            c;
{
    register VideotexPart *pv = &w->videotex;

    sauve_les_attributs_du_caractere(w, c, 0);
    (*pv->affiche_caractere_avec_fond) (w, c);
    caractere_suivant(w);
}

/*
 * Repetition du dernier caractere imprimable (G0, G1, G2)
 * 
 * On optimise en bufferisant les caracteres a repeter jusqu'a rencontre d'un
 * delimiteur ou d'une fin de rangee ==> a ce moment la, on affiche le buffer
 * (on ne bufferise pas les caracteres accentues car c'est trop chiant a
 * traiter et qu'en plus, on repete rarement 50 fois un caractere accentue !)
 */
static void 
repetition_caractere_sauve(w, rep)
VideotexWidget w;
register char   rep;
{
    register char   flag_ecriture_sur_texte;
    register VideotexPart *pv = &w->videotex;

    /*
     * FP, 16-FEB-93: on commence par regler un petit probleme de taille: si
     * on est en double hauteur et qu'on passe en ligne 1 et qu'on affiche un
     * caractere, le minitel passe en simple hauteur
     */
    if (pv->rangee <= 1 && pv->taille_caractere != NORMALE) {
	if (pv->taille_caractere == DOUBLE_HAUTEUR)
	    taille_caractere(w, NORMALE);
	else if (pv->taille_caractere == DOUBLE_GRANDEUR)
	    taille_caractere(w, DOUBLE_LARGEUR);
    }
    /* maintenant on peut traiter la repetition */
    pv->indice_repetition = 0;
    pv->x_courant_repetition = pv->x_courant;
    pv->repetition = rep;

    /*
     * Calcul de la position de fin de repetition
     */
    pv->x_fin_repetition = pv->x_courant + pv->repetition * pv->xfs_courante->max_bounds.width;
    if ((int) pv->x_fin_repetition > (int) pv->largeur_ecran)
	pv->x_fin_repetition = pv->largeur_ecran;


    if (pv->flag_G2) {
	/* signe diacritique sauf accent */
	if ((pv->accent_sauve < 65 || pv->accent_sauve > 67) && (pv->accent_sauve != 72) && (pv->accent_sauve != 75)) {
	    selection_jeu(w, G2);
	    while (pv->repetition--) {
		if (pv->attributs[pv->rangee][pv->colonne - 1].f.delimiteur) {
		    /*
		     * ecriture sur un delimiteur : - affiche la chaine -
		     * traite l'ecriture
		     */
		    (*pv->affiche_chaine_avec_fond) (w, pv->chaine_repetition, pv->indice_repetition, pv->x_courant_repetition, pv->y_courant);
		    pv->x_courant_repetition += pv->indice_repetition * pv->xfs_courante->max_bounds.width;
		    pv->indice_repetition = 0;

		    /* on a ecrit sur le delimiteur */
		    if (pv->attributs[pv->rangee][pv->colonne - 1].f.delimiteur == DELIMITEUR_EXPLICITE)
			recopie_les_attributs_si_ecriture_sur_delimiteur_explicite(w, pv->colonne - 1);
		    else
			recopie_les_attributs_si_ecriture_sur_delimiteur_graphique(w, pv->colonne - 1);
		    reaffiche_zone(w, pv->colonne - 1);
		}
		affiche_caractere_normal_en_mode_repetition(w, g2_vers_g0 (pv->accent_sauve));
	    }

	    if (pv->indice_repetition) {
		(*pv->affiche_chaine_avec_fond) (w, pv->chaine_repetition, pv->indice_repetition, pv->x_courant_repetition, pv->y_courant);
	    }
	    selection_jeu(w, G0);
	}
	/* caractere accentue */
	else {
	    while (pv->repetition--) {
		/* test d'ecriture sur delimiteur */
		if (pv->attributs[pv->rangee][pv->colonne - 1].f.delimiteur) {
		    /* on a ecrit sur le delimiteur */
		    if (pv->attributs[pv->rangee][pv->colonne - 1].f.delimiteur == DELIMITEUR_EXPLICITE)
			recopie_les_attributs_si_ecriture_sur_delimiteur_explicite(w, pv->colonne - 1);
		    else
			recopie_les_attributs_si_ecriture_sur_delimiteur_graphique(w, pv->colonne - 1);
		    reaffiche_zone(w, pv->colonne - 1);
		}
		selection_jeu(w, G2);
		(*pv->affiche_caractere_avec_fond) (w, g2_vers_g0 (pv->accent_sauve));
		selection_jeu(w, G0);
		sauve_les_attributs_du_caractere(w, pv->accent_sauve, pv->caractere_sauve);
		(*pv->affiche_caractere_sans_fond) (w, pv->caractere_sauve);
		caractere_suivant(w);
	    }
	}
    } else {
	if (pv->caractere_sauve == ' ' && pv->a_valider_si_delimiteur) {
	    /* delimiteur explicite (blanc) ? */
	    traitement_delimiteur_explicite(w, pv->caractere_sauve);
	    pv->x_courant_repetition = pv->x_courant;
	    pv->repetition--;
	} else if (pv->jeu_courant == G1) {
	    /* caractere graphique ? */
	    traitement_delimiteur_graphique(w);
	}
	while (pv->repetition--) {
	    flag_ecriture_sur_texte = 0;

	    /*
	     * Si on est en mode graphique, on teste si on devra modifier la
	     * zone texte la + proche
	     */
	    if (pv->jeu_courant == G1) {
		if (pv->colonne != 40) {
		    flag_ecriture_sur_texte = ((pv->attributs[pv->rangee][pv->colonne - 1].jeu != G1 && (pv->attributs[pv->rangee][pv->colonne - 1].couleur_fond != pv->couleur_fond || pv->attributs[pv->rangee][pv->colonne - 1].soulignement == DEBUT_LIGNAGE)) || (pv->attributs[pv->rangee][pv->colonne].jeu != G1 && (pv->attributs[pv->rangee][pv->colonne].couleur_fond != pv->couleur_fond || pv->attributs[pv->rangee][pv->colonne].soulignement == DEBUT_LIGNAGE)));
		} else {
		    flag_ecriture_sur_texte = 0;
		}
	    }
	    if (pv->attributs[pv->rangee][pv->colonne - 1].f.delimiteur) {
		/*
		 * Ecriture sur un delimiteur : - affiche la chaine - traite
		 * l'ecriture
		 */
		(*pv->affiche_chaine_avec_fond) (w, pv->chaine_repetition, pv->indice_repetition, pv->x_courant_repetition, pv->y_courant);

		pv->x_courant_repetition += pv->indice_repetition * pv->xfs_courante->max_bounds.width;
		pv->indice_repetition = 0;

		/* on a ecrit sur le delimiteur */
		if (pv->attributs[pv->rangee][pv->colonne - 1].f.delimiteur == DELIMITEUR_EXPLICITE)
		    recopie_les_attributs_si_ecriture_sur_delimiteur_explicite(w, pv->colonne - 1);
		else
		    recopie_les_attributs_si_ecriture_sur_delimiteur_graphique(w, pv->colonne - 1);
		reaffiche_zone(w, pv->colonne - 1);
	    }
	    /*
	     * En cas de semi-graphique, on peut re-valider la couleur de
	     * fond annulee par un depassement sur la rangee suivante
	     */
	    if (pv->jeu_courant == G1)
		traitement_delimiteur_graphique(w);

	    affiche_caractere_normal_en_mode_repetition(w, pv->caractere_sauve);

	    /* Modification de la zone texte */
	    if (flag_ecriture_sur_texte) {
		recopie_les_attributs_delimiteur_graphique(w, pv->colonne - 1);
		if (pv->flag_quelque_chose_a_reafficher)
		    reaffiche_zone(w, pv->colonne - 1);
		pv->attributs[pv->rangee][pv->colonne - 2].f.delimiteur = DELIMITEUR_GRAPHIQUE;
	    }
	}

	/* affiche le dernier buffer */
	if (pv->indice_repetition) {
	    (*pv->affiche_chaine_avec_fond) (w, pv->chaine_repetition, pv->indice_repetition, pv->x_courant_repetition, pv->y_courant);
	}
    }
}

/*
 * reaffiche_rectangle_ecran
 * 
 * Efface et reaffiche le contenu d'une portion d'ecran dont on passe les
 * coordonnees FP, 23-FEB-93
 */
static void 
reaffiche_rectangle_ecran(w, r1, c1, r2, c2)
VideotexWidget w;
int             r1, c1, r2, c2;
{
    register VideotexPart *pv = &w->videotex;
    register struct attribut *p;
    int i, j, x, y;
    char chaine[41];
    char old_rangee = pv->rangee;
    char old_inversion = pv->inversion;
    char old_couleur_caractere = pv->couleur_caractere;
    char old_couleur_fond = pv->couleur_fond;
    char old_masquage = pv->masquage;
    char old_soulignement = pv->soulignement;
    char old_clignotement = pv->clignotement;
    char old_jeu_courant = pv->jeu_courant;
    char old_taille_caractere = pv->taille_caractere;
    char old_insertion = pv->insertion;
    int  old_x_courant = pv->x_courant;
    int  old_y_courant = pv->y_courant;
    Boolean old_flag_drcs = pv->flag_drcs;

    pv->indice_couleur_caractere_precedent = -1;
    pv->indice_couleur_fond_precedent = -1;
    pv->xfs_courante = NULL;

    /* reaffiche la zone */
    for (pv->rangee = r1; pv->rangee <= r2; pv->rangee++) {
	/* on clippe sur la rangee pour les double hauteur */
	clipping_local(w);

	for (i = c1 - 1; i < c2;) {
	    /*
	     * On reaffiche une suite de caracteres du meme type (memes
	     * attributs). Un traitement particulier est effectue pour les
	     * double hauteur: - Si on est sur la partie superieure de
	     * caractere double hauteur, on n'affiche qu'un espace de la
	     * couleur du fond - Si on est sur la partie inferieur de
	     * caracteres double hauteur, on affiche le caractere complet.
	     * Cette routine sera a modifier par la suite pour pouvoir
	     * traiter les caracteres double hauteur avec deux couleurs de
	     * fond.
	     * 
	     */
	    p = &pv->attributs[pv->rangee][i];
	    pv->inversion = p->inversion;
	    pv->couleur_caractere = p->couleur_caractere;
	    pv->couleur_fond = p->couleur_fond;
	    pv->masquage = p->masquage;
	    pv->soulignement = p->f.delimiteur ? FIN_LIGNAGE : p->soulignement;
	    pv->clignotement = p->clignotement;
	    pv->flag_drcs = p->drcs;
	    if ((p->taille_caractere == DOUBLE_HAUTEUR || p->taille_caractere == DOUBLE_GRANDEUR) && pv->rangee <= 1) {
		pv->rangee += 2;
		taille_caractere(w, p->taille_caractere);
		pv->rangee -= 2;
	    } else
		taille_caractere(w, p->taille_caractere);	/* positionne la fonte
							 * aussi en fonction du
							 * soulignement */
	    selection_jeu(w, p->jeu);
	    x = i * pv->largeur_fonte_base;
	    y = (pv->rangee + 1) * pv->hauteur_fonte_base - pv->descent_fonte_base;
	    if ((pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR) &&
		(p->code[0] & 0x80))
		y += pv->hauteur_fonte_base;
	    j = 0;
	    if (!accentue(p)) {
		do {
		    /*
		     * si on est en haut d'un caractere double hauteur on
		     * affiche juste le fond
		     */
		    if (pv->jeu_courant == G2)
			chaine[j++] = g2_vers_g0 (p->code[0] & 0x7f);
		    else
			chaine[j++] = p->code[0] & 0x7f;
		    if (p->taille_caractere == DOUBLE_LARGEUR || p->taille_caractere == DOUBLE_GRANDEUR)
			p++, i++;
		    p++, i++;
		}
		while (i < c2 &&
		       p->inversion == pv->inversion &&
		       p->couleur_caractere == pv->couleur_caractere &&
		       p->couleur_fond == pv->couleur_fond &&
		       p->taille_caractere == pv->taille_caractere &&
		       p->masquage == pv->masquage &&
		       p->soulignement == pv->soulignement &&
		       p->clignotement == pv->clignotement &&
		       p->jeu == pv->jeu_courant &&
		       !accentue(p));
		chaine[j] = '\0';
	    } else { /* Caractere accentue */
		pv->x_courant = x;
		pv->y_courant = y;
		(*pv->affiche_caractere_avec_fond) (w, g2_vers_g0 (p->code[0] & 0x7f));
		selection_jeu(w, G0);
		(*pv->affiche_caractere_sans_fond) (w, p->code[1]);
		if (p->taille_caractere == DOUBLE_LARGEUR || p->taille_caractere == DOUBLE_GRANDEUR)
		    p++, i++;
		p++, i++;
	    }

	    /* si on a une chaine a afficher, c'est parti mon kiki */
	    if (j)
		(*pv->affiche_chaine_avec_fond) (w, chaine, j, x, y);
	}
    }

    /* restaure tous les parametres courants */
    pv->rangee = old_rangee;
    pv->inversion = old_inversion;
    pv->couleur_caractere = old_couleur_caractere;
    pv->couleur_fond = old_couleur_fond;
    pv->masquage = old_masquage;
    pv->soulignement = old_soulignement;
    pv->clignotement = old_clignotement;
    pv->jeu_courant = old_jeu_courant;
    pv->taille_caractere = old_taille_caractere;
    pv->insertion = old_insertion;
    pv->x_courant = old_x_courant;
    pv->y_courant = old_y_courant;
    pv->flag_drcs = old_flag_drcs;
    pv->indice_couleur_caractere_precedent = -1;
    pv->indice_couleur_fond_precedent = -1;
    pv->xfs_courante = NULL;
    soulignement(w, pv->soulignement);
    taille_caractere(w, pv->taille_caractere);
    selection_jeu(w, pv->jeu_courant);	/* selection de la bonne fonte courante */
    XSetClipRectangles(XtDisplay(w), pv->gc, 0, 0, pv->r_clipping_normal, 3, YXBanded);
}

/*
 * fonctions de traitement des attributs
 */

/*
 * Initialisation des attributs
 */
static void 
init_attributs(w)
VideotexWidget w;
{
    register int    i;

    for (i = 0; i != 40; i++) {
	attributs_initiaux[i].code[0] = ' ';
	attributs_initiaux[i].code[1] = 0;
	attributs_initiaux[i].inversion = POSITIF;
	attributs_initiaux[i].couleur_caractere = CARACTERE_BLANC;
	attributs_initiaux[i].taille_caractere = NORMALE;
	attributs_initiaux[i].jeu = G1;
	attributs_initiaux[i].couleur_fond = FOND_NOIR;
	attributs_initiaux[i].masquage = DEMASQUAGE;
	attributs_initiaux[i].soulignement = FIN_LIGNAGE;
	attributs_initiaux[i].clignotement = LETTRES_FIXES;
	attributs_initiaux[i].drcs = False;
	attributs_initiaux[i].f.delimiteur = 0;
	attributs_initiaux[i].f.position = POS_BG;
    }

    for (i = 0; i != 25; i++)
	memcpy(w->videotex.attributs[i], attributs_initiaux, 40 * sizeof(struct attribut));
}


/*
 * sauve_les_attributs_du_caractere
 * 
 * Ecrit dans la matrice le caractere courant avec ses attributs en le repetant
 * plusieurs fois si c'est un double taille
 */
static void 
sauve_les_attributs_du_caractere(w, c0, c1)
VideotexWidget w;
char            c0, c1;
{
    register VideotexPart *pv = &w->videotex;
    register struct attribut *p;
    int             taille = pv->taille_caractere, dont_touch;

    if (pv->flag_insertion && pv->rangee)
	scroll_droite(w, pv->colonne, 1);

    /* si on est en derniere colonne, on n'autorise pas la double largeur */
    if (pv->colonne == 40) {
	if (taille == DOUBLE_LARGEUR)
	    taille = NORMALE;
	else if (taille == DOUBLE_GRANDEUR)
	    taille = DOUBLE_HAUTEUR;
    }
    /*
     * on fait tout de suite une mise a jour autour si on ecrase un bout de
     * caractere
     */
    dont_touch = maj_si_ecrasement(w, pv->rangee, pv->colonne, pv->taille_caractere);
    if (!dont_touch) {
	if (taille == DOUBLE_LARGEUR)
	    maj_si_ecrasement(w, pv->rangee, pv->colonne + 1, pv->taille_caractere);
	else if (taille == DOUBLE_HAUTEUR)
	    maj_si_ecrasement(w, pv->rangee - 1, pv->colonne, pv->taille_caractere);
	else if (taille == DOUBLE_GRANDEUR) {
	    maj_si_ecrasement(w, pv->rangee - 1, pv->colonne + 1, pv->taille_caractere);
	    maj_si_ecrasement(w, pv->rangee - 1, pv->colonne, pv->taille_caractere);
	    maj_si_ecrasement(w, pv->rangee, pv->colonne + 1, pv->taille_caractere);
	}
    }

    /* Sauve les attributs par rangee */
    p = &pv->attributs[pv->rangee][pv->colonne - 1];
    p->soulignement = pv->soulignement;
    p->couleur_fond = pv->couleur_fond;
    p->jeu = pv->jeu_courant;
    p->code[0] = c0;
    if (c1) {
	p->code[1] = c1;
	p->jeu = G2;
    }
    p->inversion = pv->inversion;
    p->couleur_caractere = pv->couleur_caractere;
    p->taille_caractere = taille;
    p->f.delimiteur = 0;
    p->drcs = pv->flag_drcs;
    if (p->jeu == G0 && p->code[0] == ' ')
	p->clignotement = LETTRES_FIXES;
    else
	p->clignotement = pv->clignotement;

    /* Recopie des attributs en cas de double hauteur/taille */
    if (taille == DOUBLE_HAUTEUR) {
	pv->attributs[pv->rangee - 1][pv->colonne - 1] = *p;
	p = &pv->attributs[pv->rangee - 1][pv->colonne - 1];
	p->code[0] |= 0x80;
	p->f.position = POS_HG;
	if (pv->colonne > 1) {
	    p->couleur_fond = (p - 1)->couleur_fond;
	    p->soulignement = (p - 1)->soulignement;
	    p->masquage = (p - 1)->masquage;
	} else {
	    p->couleur_fond = FOND_NOIR;
	    p->soulignement = FIN_LIGNAGE;
	    p->masquage = DEMASQUAGE;
	}
    } else if (taille == DOUBLE_LARGEUR) {
	pv->attributs[pv->rangee][pv->colonne] = pv->attributs[pv->rangee][pv->colonne - 1];
	pv->attributs[pv->rangee][pv->colonne].f.position = POS_BD;
    } else if (taille == DOUBLE_GRANDEUR) {
	pv->attributs[pv->rangee - 1][pv->colonne - 1] =
	    pv->attributs[pv->rangee - 1][pv->colonne] =
	    pv->attributs[pv->rangee][pv->colonne] = pv->attributs[pv->rangee][pv->colonne - 1];
	pv->attributs[pv->rangee][pv->colonne].f.position = POS_BD;
	p = &pv->attributs[pv->rangee - 1][pv->colonne - 1];
	p->code[0] |= 0x80;
	p->f.position = POS_HG;
	(p + 1)->code[0] |= 0x80;
	(p + 1)->f.position = POS_HD;
	if (pv->colonne > 1) {
	    (p + 1)->couleur_fond = p->couleur_fond = (p - 1)->couleur_fond;
	    (p + 1)->soulignement = p->soulignement = (p - 1)->soulignement;
	    (p + 1)->masquage = p->masquage = (p - 1)->masquage;
	} else {
	    (p + 1)->couleur_fond = p->couleur_fond = FOND_NOIR;
	    (p + 1)->soulignement = p->soulignement = FIN_LIGNAGE;
	    (p + 1)->masquage = p->masquage = DEMASQUAGE;
	}
    }
}


/*
 * Ces fonctions sont appelees si l'on ecrit sur un zone existante :
 * 
 * - avec un delimiteur explicite (blanc), ce qui valide les attributs serie 
 * - avec un semi-graphique, ce qui valide seulement la couleur de fond
 */
static void 
recopie_les_attributs_delimiteur_explicite(w, index)
VideotexWidget w;
char            index;
{
    register VideotexPart *pv = &w->videotex;
    register char   i = index;

    /*
     * Si l'on ecrit sur une zone non vide avec un autre delimiteur, les
     * attributs serie deviennent ceux a valider
     */
    pv->flag_quelque_chose_a_reafficher = False;

    /* fixe l'attribut de masquage pour toute la rangee */
    fixe_attribut_de_rangee_masquage(w, pv->masquage, pv->colonne);

    /*
     * Un delimiteur explicite valide les attributs serie Tout semi-graphique
     * est un delimiteur pour une zone texte
     */
    while (pv->attributs[pv->rangee][i].jeu != G1 && !pv->attributs[pv->rangee][i].f.delimiteur && i < 40) {
	pv->attributs[pv->rangee][i].couleur_fond = pv->couleur_fond;
	pv->attributs[pv->rangee][i].soulignement = pv->soulignement;
	if (pv->attributs[pv->rangee][i].taille_caractere == DOUBLE_LARGEUR ||
	    pv->attributs[pv->rangee][i].taille_caractere == DOUBLE_GRANDEUR) {
	    pv->attributs[pv->rangee][i + 1].couleur_fond = pv->couleur_fond;
	    pv->attributs[pv->rangee][i + 1].soulignement = pv->soulignement;
	}
	i += ((pv->attributs[pv->rangee][i].taille_caractere == NORMALE ||
	  pv->attributs[pv->rangee][i].taille_caractere == DOUBLE_HAUTEUR) ? 1 : 2);
    }

    if (i != index)
	pv->flag_quelque_chose_a_reafficher = True;

    /* Si la zone n'etait pas vide, specifie un delimiteur graphique */
    if (i < 40 && i >= pv->colonne && pv->attributs[pv->rangee][i].jeu == G1) {
	pv->attributs[pv->rangee][i].f.delimiteur = DELIMITEUR_GRAPHIQUE;
    }
}

static void 
recopie_les_attributs_delimiteur_graphique(w, index)
VideotexWidget w;
char            index;
{
    register VideotexPart *pv = &w->videotex;
    register char   i = index;

    /*
     * Si l'on ecrit sur une zone non vide avec un autre delimiteur, les
     * attributs serie deviennent ceux a valider
     */

    pv->flag_quelque_chose_a_reafficher = 0;

    /*
     * Un caractere graphique est un delimiteur pour la couleur de fond
     */
    while (pv->attributs[pv->rangee][i].jeu != G1 && !pv->attributs[pv->rangee][i].f.delimiteur && i < 40) {
	pv->attributs[pv->rangee][i].couleur_fond = pv->couleur_fond;
	pv->attributs[pv->rangee][i].soulignement = FIN_LIGNAGE;
	if (pv->attributs[pv->rangee][i].taille_caractere == DOUBLE_LARGEUR ||
	    pv->attributs[pv->rangee][i].taille_caractere == DOUBLE_GRANDEUR) {
	    pv->attributs[pv->rangee][i + 1].couleur_fond = pv->couleur_fond;
	    pv->attributs[pv->rangee][i + 1].soulignement = pv->soulignement;
	}
	i += ((pv->attributs[pv->rangee][i].taille_caractere == NORMALE ||
	  pv->attributs[pv->rangee][i].taille_caractere == DOUBLE_HAUTEUR) ? 1 : 2);
    }
    if (i != index) {
	pv->flag_quelque_chose_a_reafficher = 1;
    }
}


/*
 * Ces fonctions sont appelees si l'on ecrit sur un delimiteur :
 * 
 * - explicite (blanc) 
 * - semi-graphique
 */
static void 
recopie_les_attributs_si_ecriture_sur_delimiteur_explicite(w, index)
VideotexWidget w;
char            index;
{
    register VideotexPart *pv = &w->videotex;
    register char   i = index;
    char            cf, soul, masq;

    /*
     * Si l'on ecrit sur un delimiteur explicite, les attributs serie
     * deviennent immediatement ceux a gauche du delimiteur et ce jusqu'au
     * prochain delimiteur
     */

    /* recopie les attributs serie de gauche jusqu'au prochain delimiteur */
    cf = (index ? pv->attributs[pv->rangee][index - 1].couleur_fond : FOND_NOIR);
    soul = (index ? pv->attributs[pv->rangee][index - 1].soulignement : FIN_LIGNAGE);
    masq = (index ? pv->attributs[pv->rangee][index - 1].masquage : DEMASQUAGE);

    /* fixe l'attribut de masquage pour toute la rangee */
    fixe_attribut_de_rangee_masquage(w, masq, index + 1);

    /* copie */
    do {
	pv->attributs[pv->rangee][i].couleur_fond = cf;
	pv->attributs[pv->rangee][i].soulignement = soul;
	i += ((pv->attributs[pv->rangee][i].taille_caractere == NORMALE || pv->attributs[pv->rangee][i].taille_caractere == DOUBLE_HAUTEUR) ? 1 : 2);
    } while (!pv->attributs[pv->rangee][i].f.delimiteur && pv->attributs[pv->rangee][i].jeu != G1 && i < 40);

    /* on n'a plus de delimiteur a cet endroit */
    pv->attributs[pv->rangee][index].f.delimiteur = 0;
}


static void 
recopie_les_attributs_si_ecriture_sur_delimiteur_graphique(w, index)
VideotexWidget w;
char            index;
{
    register VideotexPart *pv = &w->videotex;
    register char   i = index;

    do {
	pv->attributs[pv->rangee][i].couleur_fond = pv->couleur_fond;
	pv->attributs[pv->rangee][i].soulignement = FIN_LIGNAGE;
	i += ((pv->attributs[pv->rangee][i].taille_caractere == NORMALE || pv->attributs[pv->rangee][i].taille_caractere == DOUBLE_HAUTEUR) ? 1 : 2);
    } while (pv->attributs[pv->rangee][i].jeu != G1 && !pv->attributs[pv->rangee][i].f.delimiteur && i < 40);

    /* on n'a plus de delimiteur a cet endroit */
    pv->attributs[pv->rangee][index].f.delimiteur = 0;
}


/*
 * Reaffiche une zone sur laquelle on a modifie les attributs serie
 */

static void 
reaffiche_zone(w, index)
VideotexWidget w;
char            index;
{
    register VideotexPart *pv = &w->videotex;
    register char   i, n;
    register int    y_re_affichage;
    int             x_courant_sauve;
    char            couleur_caractere_sauve, inversion_sauve, taille_caractere_sauve, jeu_sauve;
    char            couleur_fond_sauve, soulignement_sauve, masquage_sauve;

    clipping_local (w);

    /*
     * Si l'on ecrit sur un delimiteur, les attributs serie deviennent
     * immediatement ceux a gauche du delimiteur et ce jusqu'au prochain
     * delimiteur
     */
    x_courant_sauve = pv->x_courant;
    pv->indice_re_affichage = 0;

    inversion_sauve = pv->inversion;
    couleur_caractere_sauve = pv->couleur_caractere;
    taille_caractere_sauve = pv->taille_caractere;
    jeu_sauve = pv->jeu_courant;
    soulignement_sauve = pv->soulignement;
    masquage_sauve = pv->masquage;
    couleur_fond_sauve = pv->couleur_fond;

    pv->x_courant = index * pv->largeur_fonte_base;
    /* pv->x_courant -= pv->xfs_courante->min_bounds.width; */
    i = index;
    pv->p_attribut_re_affichage = (struct attribut *) & pv->attributs[pv->rangee][i];

    /* fixe immediatement les attributs serie */
    soulignement(w, pv->p_attribut_re_affichage->soulignement);
    couleur_fond(w, pv->p_attribut_re_affichage->couleur_fond);
    masquage(w, pv->p_attribut_re_affichage->masquage);

    pv->x_courant_re_affichage = pv->x_courant;

    /* copie */
    do {
	i += (n = re_affiche_caractere(w, pv->rangee, &pv->attributs[pv->rangee][i]));
	pv->x_courant += n * pv->largeur_fonte_base;
    } while (!pv->attributs[pv->rangee][i].f.delimiteur && i < 40 && pv->attributs[pv->rangee][i].jeu != G1);

    /* affiche le dernier buffer si il existe */
    if (pv->indice_re_affichage) {
	/*
	 * Fixe les attributs d'affichage
	 */
	inversion(w, pv->p_attribut_re_affichage->inversion);
	couleur_caractere(w, pv->p_attribut_re_affichage->couleur_caractere);
	taille_caractere(w, pv->p_attribut_re_affichage->taille_caractere);
	selection_jeu(w, pv->p_attribut_re_affichage->jeu);

	/* affiche le buffer */
	if (pv->p_attribut_re_affichage->code[0] & 0x80)
	    y_re_affichage = pv->y_courant + pv->hauteur_fonte_base;
	else
	    y_re_affichage = pv->y_courant;

	(*pv->affiche_chaine_avec_fond) (w, pv->chaine_a_re_afficher, pv->indice_re_affichage, pv->x_courant_re_affichage, y_re_affichage);

	pv->indice_re_affichage = 0;
    }
    pv->x_courant = x_courant_sauve;

    /* restaure tous les attributs */
    inversion(w, inversion_sauve);
    taille_caractere(w, taille_caractere_sauve);

    soulignement(w, soulignement_sauve);
    selection_jeu(w, jeu_sauve);
    couleur_caractere(w, couleur_caractere_sauve);
    couleur_fond(w, couleur_fond_sauve);
    masquage(w, masquage_sauve);
    XSetClipRectangles(XtDisplay(w), pv->gc, 0, 0, pv->r_clipping_normal, 3, YXBanded);
}

/*
 * Fonctions de traitement des delimiteurs
 */

static void 
traitement_delimiteur_graphique(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;

    if (pv->a_valider_si_delimiteur & BIT_COULEUR_FOND) {
	couleur_fond(w, pv->valeur_couleur_fond);
	pv->a_valider_si_delimiteur &= ~BIT_COULEUR_FOND;
    }
}

static void 
traitement_delimiteur_explicite(w, c)
VideotexWidget w;
register char   c;
{
    register VideotexPart *pv = &w->videotex;
    register int    largeur_delimiteur;
    register char   soulignement_sauve = FIN_LIGNAGE, rs, cs;
    char            depassement_rangee_suivante = 0;

    couleur_fond(w, pv->valeur_couleur_fond);
    soulignement(w, pv->valeur_lignage);
    if ((pv->a_valider_si_delimiteur & BIT_MASQUAGE) && pv->valeur_masquage == DEMASQUAGE) {
	masquage(w, pv->valeur_masquage);
	fixe_attribut_de_rangee_masquage(w, pv->valeur_masquage, pv->colonne);
    }
    if (pv->soulignement == DEBUT_LIGNAGE) {
	soulignement_sauve = DEBUT_LIGNAGE;
	soulignement(w, FIN_LIGNAGE);
    }
    rs = pv->rangee;
    cs = pv->colonne;

    /* faire les mises a jour si ecrasement de caracteres */

    affiche_caractere_normal(w, c);

    if (pv->colonne < cs)
	depassement_rangee_suivante = 1;

    if (soulignement_sauve == DEBUT_LIGNAGE)
	soulignement(w, DEBUT_LIGNAGE);

    /* largeur du delimiteur */
    largeur_delimiteur = (pv->taille_caractere == NORMALE || pv->taille_caractere == DOUBLE_HAUTEUR) ? 1 : 2;

    if (!depassement_rangee_suivante) {
	/* masquage demande : seul le blanc est un delimiteur */
	if ((pv->a_valider_si_delimiteur & BIT_MASQUAGE) && pv->valeur_masquage == MASQUAGE) {
	    masquage(w, MASQUAGE);
	    fixe_attribut_de_rangee_masquage(w, pv->valeur_masquage, pv->colonne);
	}
	/*
	 * Dans le cas de G0, on doit fixer l'attribut sur le caractere
	 * suivant (dans le cas ou la sequence de definition de l'attribut
	 * n'est suivie d'aucun affichage)
	 */
	if (pv->colonne < 40)
	    pv->attributs[pv->rangee][pv->colonne - 1].soulignement = pv->soulignement;

	/* On a maintenant un delimiteur a cet endroit */
	pv->attributs[pv->rangee][pv->colonne - 1 - largeur_delimiteur].f.delimiteur = DELIMITEUR_EXPLICITE;
	pv->attributs[pv->rangee][pv->colonne - 2].f.delimiteur = DELIMITEUR_EXPLICITE;
	pv->attributs[pv->rangee][pv->colonne - 1 - largeur_delimiteur].soulignement = soulignement_sauve;
	pv->attributs[pv->rangee][pv->colonne - 2].soulignement = soulignement_sauve;
	pv->attributs[pv->rangee][pv->colonne - 1 - largeur_delimiteur].f.position = POS_BG;

	/*
	 * Recopie des attributs sur les cellules voisines Schemas en
	 * fonction des indices :
	 * 
	 *  	 col-2 col-1 
         * 	.-----------. 
         * 	|DT DH| DT  | 
         * 	|     |     | rangee-1
	 * 	|-----+-----| 
         * 	|TN DL|DL DT| 
         * 	|DH DT|     | rangee 
         * 	`-----------'
	 * 
	 * TN=taille normale, DL=double largeur, DH=double hauteur, DT=double
	 * taille
	 * 
	 * Le bit 7 a 1 indique le haut d'un caractere double taille/hauteur.
	 */
	if (pv->taille_caractere == DOUBLE_HAUTEUR) {
	    struct attribut *p = &pv->attributs[pv->rangee - 1][pv->colonne - 2];
	    *p = pv->attributs[pv->rangee][pv->colonne - 2];
	    p->code[0] = 0x20 | 0x80;
	    p->soulignement = FIN_LIGNAGE;
	    p->f.position = POS_HG;
	} else if (pv->taille_caractere == DOUBLE_LARGEUR) {
	    pv->attributs[pv->rangee][pv->colonne - 2] = pv->attributs[pv->rangee][pv->colonne - 3];
	    pv->attributs[pv->rangee][pv->colonne - 2].f.position = POS_BD;
	} else if (pv->taille_caractere == DOUBLE_GRANDEUR) {
	    pv->attributs[pv->rangee - 1][pv->colonne - 3] = pv->attributs[pv->rangee - 1][pv->colonne - 2] = pv->attributs[pv->rangee][pv->colonne - 2] = pv->attributs[pv->rangee][pv->colonne - 3];
	    pv->attributs[pv->rangee - 1][pv->colonne - 3].code[0] = 0x20 | 0x80;
	    pv->attributs[pv->rangee - 1][pv->colonne - 2].code[0] = 0x20 | 0x80;
	    pv->attributs[pv->rangee - 1][pv->colonne - 3].soulignement = FIN_LIGNAGE;
	    pv->attributs[pv->rangee - 1][pv->colonne - 2].soulignement = FIN_LIGNAGE;
	    pv->attributs[pv->rangee - 1][pv->colonne - 3].f.position = POS_HG;
	    pv->attributs[pv->rangee - 1][pv->colonne - 2].f.position = POS_HD;
	    pv->attributs[pv->rangee][pv->colonne - 2].f.position = POS_BD;
	}
	/* modifie la zone d'accueil */
	recopie_les_attributs_delimiteur_explicite(w, pv->colonne - 1);

	/* reaffiche */
	if (pv->flag_quelque_chose_a_reafficher == 1) {
	    reaffiche_zone(w, pv->colonne - 1);
	    if (pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR) {
		    char  sauve_taille = pv->taille_caractere;

		    pv->rangee--;
		    Y_COURANT
		    recopie_les_attributs_delimiteur_explicite(w, pv->colonne - 1);
		    reaffiche_zone(w, pv->colonne - 1);
		    pv->rangee++;
		    Y_COURANT
		    if (pv->rangee == 2 && (sauve_taille == DOUBLE_HAUTEUR || sauve_taille == DOUBLE_GRANDEUR)) {
		    taille_caractere(w, sauve_taille);
		    selection_jeu(w, pv->jeu_courant);
		}
	    }
	}
	/*
	 * maintenant qu'on a fait le premier reaffichage on met la taille
	 * normale au delimiteur
	 */
	pv->attributs[pv->rangee][pv->colonne - 1 - largeur_delimiteur].taille_caractere = NORMALE;
	pv->attributs[pv->rangee][pv->colonne - 1 - largeur_delimiteur].f.position = POS_BG;
	pv->attributs[pv->rangee][pv->colonne - 2].taille_caractere = NORMALE;
	pv->attributs[pv->rangee][pv->colonne - 2].f.position = POS_BG;
	if (pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR) {
	    pv->attributs[pv->rangee - 1][pv->colonne - 1 - largeur_delimiteur].taille_caractere = NORMALE;
	    pv->attributs[pv->rangee - 1][pv->colonne - 1 - largeur_delimiteur].code[0] &= 0x7f;
	    pv->attributs[pv->rangee - 1][pv->colonne - 1 - largeur_delimiteur].f.position = POS_BG;
	    pv->attributs[pv->rangee - 1][pv->colonne - 2].taille_caractere = NORMALE;
	    pv->attributs[pv->rangee - 1][pv->colonne - 2].code[0] &= 0x7f;
	    pv->attributs[pv->rangee - 1][pv->colonne - 2].f.position = POS_BG;
	}
    } else {
	/* On a maintenant un delimiteur a cet endroit */
	pv->attributs[rs][40 - largeur_delimiteur].f.delimiteur = DELIMITEUR_EXPLICITE;
	pv->attributs[rs][40 - largeur_delimiteur].f.position = POS_BG;
	if (pv->taille_caractere == DOUBLE_HAUTEUR) {
	    pv->attributs[rs - 1][39] = pv->attributs[rs][39];
	    pv->attributs[rs - 1][39].f.position = POS_HG;
	} else if (pv->taille_caractere == DOUBLE_LARGEUR) {
	    pv->attributs[rs][39] = pv->attributs[rs][38];
	    pv->attributs[rs][39].f.position = POS_BD;
	} else if (pv->taille_caractere == DOUBLE_GRANDEUR) {
	    pv->attributs[rs - 1][38] =
		pv->attributs[rs - 1][39] =
		pv->attributs[rs][39] = pv->attributs[rs][38];
	    pv->attributs[rs][39].f.position = POS_BD;
	    pv->attributs[rs - 1][38].f.position = POS_HG;
	    pv->attributs[rs - 1][38].f.position = POS_HD;
	}
    }
    pv->a_valider_si_delimiteur = 0;
}

/*
 * Lecture des attributs de la zone d'accueil apres deplacement
 */
static void 
selection_attributs_zone_accueil(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;

    if (pv->jeu_courant != G1) {
	/* correction FP, 17-FEB-93: la colonne etait incorrecte */
	if (pv->colonne > 1) {
	    if (pv->attributs[pv->rangee][pv->colonne - 2].jeu != G1)
		soulignement(w, pv->attributs[pv->rangee][pv->colonne - 2].soulignement);
	    else
		soulignement(w, FIN_LIGNAGE);
	    couleur_fond(w, pv->attributs[pv->rangee][pv->colonne - 2].couleur_fond);
	} else {
	    soulignement(w, FIN_LIGNAGE);
	    couleur_fond(w, FOND_NOIR);
	}
    }
    /* l'attribut de masquage est defini sur la rangee a partir du delimiteur */
    if (pv->colonne > 1)
	masquage(w, pv->attributs[pv->rangee][pv->colonne - 2].masquage);
    else
	masquage(w, DEMASQUAGE);
    
    calcul_flag_drcs ((VideotexWidget)w);
}


/*
 * Le masquage est valide sur la rangee a partir du delimiteur de zone et
 * jusqu'a son annulation (ou la fin de la rangee)
 */
static void 
fixe_attribut_de_rangee_masquage(w, masquage, colonne_depart)
VideotexWidget w;
int masquage, colonne_depart;
{
    register VideotexPart *pv = &w->videotex;
    register int    i;

    for (i = colonne_depart - 1; i < 40; i++) {
	/* Modification de ecran_masque et ecran_demasque */
	if (masquage == MASQUAGE && pv->attributs[pv->rangee][i].masquage == DEMASQUAGE)
	    masque_un_caractere(w, pv->rangee, i);
	else if (masquage == DEMASQUAGE && pv->attributs[pv->rangee][i].masquage == MASQUAGE)
	    demasque_un_caractere(w, pv->rangee, i);
	pv->attributs[pv->rangee][i].masquage = masquage;
    }
}


/*
 * retourne TRUE si le caractere pointe par p est un accentue (G2+accent) FP,
 * 22-FEB-93
 */
static int 
accentue(p)
register struct attribut *p;
{
    register unsigned char c;

    c = p->code[0] & 0x7f;
    return (p->jeu != G2 || ((c < 65 || c > 67) && c != 72 && c != 75)) ? FALSE : TRUE;
}


/*
 * parcours toute une colonne en mettant a jour les attributs pour chaque ligne
 * commencant a la colonne c en se basant sur les attributs immediatement a
 * sa gauche. (on met a jour couleur de fond, soulignement et masquage)
 * retourne le numero de colonne maximum sur lequel des modifs on ete
 * effectuees
 */
static int 
fixe_les_attributs_d_un_bloc (w, r, c, h)
VideotexWidget w;
int             r, c, h;
{
    register VideotexPart *pv = &w->videotex;
    register struct attribut *p;
    char            couleur_fond, soulignement, c1, flag_g1, masquage;
    int             max_c1;

    if (c > 40)
	return 40;
    max_c1 = c;
    for (; h > 0; h--, r++) {
	/* prends les attributs a gauche */
	if (c == 1) {
	    couleur_fond = FOND_NOIR;
	    soulignement = FIN_LIGNAGE;
	    masquage = DEMASQUAGE;
	} else {
	    couleur_fond = pv->attributs[r][c - 2].couleur_fond;
	    /*
	     * Un caractere graphique est un delimiteur pour la couleur de
	     * fond, mais pas pour le lignage...
	     */
	    flag_g1 = (pv->attributs[r][c - 2].jeu == G1);
	    soulignement = pv->attributs[r][c - 2].soulignement;
	    masquage = pv->attributs[r][c - 2].masquage;
	}

	/*
	 * Parcours le reste de la ligne jusqu'au prochain graphique ou
	 * delimiteur
	 */
	p = &pv->attributs[r][c - 1];
	for (c1 = c;
	     c1 <= 40 &&
	     p->jeu != G1 &&
	     p->f.delimiteur == 0
	     ; c1++, p++) {
	    p->couleur_fond = couleur_fond;
	    if (!flag_g1)
		p->soulignement = soulignement;
	}
	if (c1 > max_c1) {
	    max_c1 = c1;
	}
	/* mise a jour du masquage pour la rangee */
	fixe_attribut_de_rangee_masquage(w, masquage, c);

    }
    return max_c1 - 1;
}


/*
 * maj_si_ecrasement
 * 
 * Fait une mise a jour autour de la position d'ecriture si on ecrase un bout de
 * caractere. Retourne 0, sauf si on ecrase la partie en bas a gauche d'une
 * caractere qui fait la meme taille que celui qu'on va afficher. Dans ce cas
 * ca ne sert a rien de bricoler la zone autour.
 * 
 */
static int 
maj_si_ecrasement(w, r, c, taille)
VideotexWidget w;
int             r, c, taille;
{
    register VideotexPart *pv = &w->videotex;
    register struct attribut *p;
    char            t;

    if (r < 1 || r > 24 || c < 1 || c > 40)
	return 0;
    p = &pv->attributs[r][c - 1];
    t = p->taille_caractere;
    switch (p->f.position) {
    case POS_HD:		/* haut a droite: double grandeur */
	p--;
	p->taille_caractere = NORMALE;
	p->code[0] &= 0x7f;
	p->f.position = POS_BG;
	p = &pv->attributs[r + 1][c - 2];
	p->taille_caractere = NORMALE;
	p->code[0] &= 0x7f;
	p->f.position = POS_BG;
	p++;
	p->taille_caractere = NORMALE;
	p->code[0] &= 0x7f;
	p->f.position = POS_BG;
	break;

    case POS_HG:		/* haut a gauche: double hauteur ou double
				 * grandeur */
	if (t == DOUBLE_GRANDEUR) {
	    p++;
	    p->taille_caractere = NORMALE;
	    p->code[0] &= 0x7f;
	    p->f.position = POS_BG;
	}
	p = &pv->attributs[r + 1][c - 1];
	p->taille_caractere = NORMALE;
	p->code[0] &= 0x7f;
	p->f.position = POS_BG;
	if (t == DOUBLE_GRANDEUR) {
	    p++;
	    p->taille_caractere = NORMALE;
	    p->code[0] &= 0x7f;
	    p->f.position = POS_BG;
	}
	break;

    case POS_BD:		/* bas a droite: double largeur ou double
				 * grandeur */
	p--;
	p->taille_caractere = NORMALE;
	p->code[0] &= 0x7f;
	p->f.position = POS_BG;
	if (t == DOUBLE_GRANDEUR) {
	    p = &pv->attributs[r - 1][c - 2];
	    p->taille_caractere = NORMALE;
	    p->code[0] &= 0x7f;
	    p->f.position = POS_BG;
	    p++;
	    p->taille_caractere = NORMALE;
	    p->code[0] &= 0x7f;
	    p->f.position = POS_BG;
	}
	break;

    case POS_BG:
	if (t == NORMALE)
	    break;
#ifdef NOTDEF
	if (t == taille)
	    return 1;		/* dont_touch */
#endif
	if (t == DOUBLE_LARGEUR || t == DOUBLE_GRANDEUR)
	    p++;
	else			/* double hauteur */
	    p = &pv->attributs[r - 1][c - 1];
	p->taille_caractere = NORMALE;
	p->code[0] &= 0x7f;
	p->f.position = POS_BG;
	if (t == DOUBLE_GRANDEUR) {
	    p = &pv->attributs[r - 1][c - 1];
	    p->taille_caractere = NORMALE;
	    p->code[0] &= 0x7f;
	    p->f.position = POS_BG;
	    p++;
	    p->taille_caractere = NORMALE;
	    p->code[0] &= 0x7f;
	    p->f.position = POS_BG;
	}
	break;
    }
    return 0;
}

/*
 * fonctions de traitement du clignotement
 */

static void 
clignotement(w, c)
VideotexWidget w;
char            c;
{
    w->videotex.clignotement = c;
}

static XtTimerCallbackProc
clignote(w, id)
VideotexWidget w;
XtIntervalId *id;
{
    register VideotexPart *pv = &w->videotex;
    register int    i, j, jdebut;
    int             x1, y1;
    unsigned int    l, h;
    char            cc_courante, cf_courante, inversion;
    int             reaff_curseur = FALSE;

    /*
     * On recherche dans la page les caracteres ayant l'attribut de
     * clignotement et non masques
     */
    for (i = 0; i < 25; i++) {
	for (j = 0; j < 40;) {
	    if (pv->attributs[i][j].clignotement == LETTRES_CLIGNOTANTES && pv->attributs[i][j].masquage != MASQUAGE) {
		cf_courante = pv->attributs[i][j].couleur_fond;
		cc_courante = pv->attributs[i][j].couleur_caractere;
		inversion = pv->attributs[i][j].inversion;
		jdebut = j;

		/*
		 * On decoupe la zone clignotante par morceaux de meme
		 * couleur de fond.
		 * 
		 */
		for (;;) {
		    /* teste si on a ecrase le curseur clignotant */
		    if (i == pv->rangee && (j + 1) == pv->colonne)
			reaff_curseur = TRUE;

		    /* teste si on est arrive en fin de zone */
		    if (++j == 40 ||
		     pv->attributs[i][j].clignotement != LETTRES_CLIGNOTANTES ||
			(pv->attributs[i][j].inversion == POSITIF && pv->attributs[i][j].couleur_fond != cf_courante) ||
			(pv->attributs[i][j].inversion == NEGATIF && pv->attributs[i][j].couleur_caractere != cc_courante))
			break;
		}

		/* Trace la zone */
		l = (j - jdebut) * pv->largeur_fonte_base;
		y1 = i * pv->hauteur_fonte_base;
		x1 = jdebut * pv->largeur_fonte_base;
		h = pv->hauteur_fonte_base;

		/* On affiche un coup le fond, un coup le caractere... */
		if ((pv->flip_flop && inversion == POSITIF) || (!pv->flip_flop && inversion == NEGATIF))
		    XCopyArea(XtDisplay(w), pv->ecran_masquage, XtWindow(w), pv->gc, x1, y1, l, h, x1, y1);
		else
		    (*pv->affiche_fond_pour_clignotement) (w, (pv->attributs[i][jdebut].inversion == POSITIF ? cf_courante - FOND_NOIR : cc_courante + 16 - FOND_NOIR), x1, y1, l, h);
	    } else
		j++;
	}
    }

    /* enfin on flushe l'affichage */
    XFlush (XtDisplay(w));

    pv->flip_flop ^= 1;
    /* necessaire, on se demande pourquoi... */
    pv->indice_couleur_caractere_precedent = -1;	

    if (reaff_curseur == TRUE)
	(*pv->fonction_affichage_curseur) (w, 1);

    XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w), (unsigned long) 1000, (XtTimerCallbackProc) clignote, (caddr_t)w);
}

/*
 * fonctions de traitement des couleurs
 */

static int
init_couleurs(w)		/* initialisation des couleurs */
VideotexWidget  w;
{
    register VideotexPart *pv = &w->videotex;
    register int    i, classe_visual = DirectColor;
    XVisualInfo     xvi;
    int screen = DefaultScreen(XtDisplay(w));
    int nplanes = DisplayPlanes(XtDisplay(w), screen);
    Colormap cmap = DefaultColormap(XtDisplay(w), screen);
    Window root = DefaultRootWindow(XtDisplay(w));

    /* Couleur ou niveaux de gris ? */
    if (nplanes > 1) {
	/* Niveaux de gris ? */
	while (!XMatchVisualInfo(XtDisplay(w), screen, nplanes, classe_visual, &xvi))
	    classe_visual--;

	/* 16 niveaux */
	if ((nplanes == 4 && classe_visual <= GrayScale) || w->videotex.niveaux_de_gris == True) {
	    for (i = 0; i != 8; i++) {
		if (!XParseColor(XtDisplay(w), cmap, nom_niveau[i], &pv->couleurs[i])) {
		    printf("erreur XParseColor sur %s\n", nom_niveau[i]);
		    return (0);
		}
		if (!XAllocColor(XtDisplay(w), cmap, &pv->couleurs[i])) {
		    printf("erreur XParseColor sur %s\n", nom_niveau[i]);
		    return (0);
		}
	    }
	}
	/* Sinon on utilise les couleurs */
	else {
	    for (i = 0; i != 8; i++) {
		if (!XParseColor(XtDisplay(w), cmap, nom_couleur[i], &pv->couleurs[i])) {
		    printf("erreur XParseColor sur %s\n", nom_couleur[i]);
		    return (0);
		}
		if (!XAllocColor(XtDisplay(w), cmap, &pv->couleurs[i])) {
		    printf("erreur XParseColor sur %s\n", nom_couleur[i]);
		    return (0);
		}
	    }
	}
    } else {
	/*
	 * monochrome => creation des pixmaps
	 */
	if (!(pv->pixmap[0] = XCreateBitmapFromData(XtDisplay(w), root, black_bits, black_width, black_height))) {
	    erreur_videotex ("Erreur chargement pixmap black\n");
	    return (0);
	}
	if (!(pv->pixmap[1] = XCreateBitmapFromData(XtDisplay(w), root, unx1_bits, unx1_width, unx1_height))) {
	    erreur_videotex ("Erreur chargement pixmap unx1\n");
	    return (0);
	}
	if (!(pv->pixmap[2] = XCreateBitmapFromData(XtDisplay(w), root, light_gray_bits, light_gray_width, light_gray_height))) {
	    erreur_videotex ("Erreur chargement pixmap light_gray\n");
	    return (0);
	}
	if (!(pv->pixmap[3] = XCreateBitmapFromData(XtDisplay(w), root, gray3_bits, gray3_width, gray3_height))) {
	    erreur_videotex ("Erreur chargement pixmap gray3\n");
	    return (0);
	}
	if (!(pv->pixmap[4] = XCreateBitmapFromData(XtDisplay(w), root, flipped_gray_bits, flipped_gray_width, flipped_gray_height))) {
	    erreur_videotex ("Erreur chargement pixmap flipped_gray\n");
	    return (0);
	}
	if (!(pv->pixmap[5] = XCreateBitmapFromData(XtDisplay(w), root, gray_bits, gray_width, gray_height))) {
	    erreur_videotex ("Erreur chargement pixmap gray\n");
	    return (0);
	}
	if (!(pv->pixmap[6] = XCreateBitmapFromData(XtDisplay(w), root, dimple1_bits, dimple1_width, dimple1_height))) {
	    erreur_videotex ("Erreur chargement pixmap dimple1\n");
	    return (0);
	}
	if (!(pv->pixmap[7] = XCreateBitmapFromData(XtDisplay(w), root, white_bits, white_width, white_height))) {
	    erreur_videotex ("Erreur chargement pixmap white\n");
	    return (0);
	}
    }

    return (1);
}

/*
 * fixe la couleur d'un caractere (attribut par caractere)
 */
static void 
couleur_caractere(w, couleur)
VideotexWidget w;
char            couleur;
{
#ifdef DEBUG
    printf("\nCouleur caractere = %s\n", nom_couleur[couleur - CARACTERE_NOIR]);
#endif
    w->videotex.couleur_caractere = couleur;
}

/*
 * fixe la couleur de fond (attribut par rangee)
 */
static void 
couleur_fond(w, couleur)
VideotexWidget w;
char            couleur;
{
#ifdef DEBUG
    printf("\nCouleur fond = %s\n", nom_couleur[couleur - FOND_NOIR]);
#endif
    w->videotex.couleur_fond = couleur;
}

/*
 * fonctions de traitement du curseur
 */

/*
 * Affiche le curseur uniquement si phase_curseur est a 1 ou si force est a 1
 */
static void 
affiche_curseur(w, force)
VideotexWidget w;
int             force;
{
    register VideotexPart *pv = &w->videotex;
    register struct attribut *pt = &pv->attributs[pv->rangee][pv->colonne - 1];
    unsigned long   col;

    if ((pv->jeu_courant != G1) && (pv->phase_curseur || force == 1)) {
	/* calcul de la couleur du curseur */
	if (DisplayPlanes(XtDisplay(w), DefaultScreen(XtDisplay(w))) > 1) {
	    col = pv->couleurs[pt->couleur_fond - FOND_NOIR].pixel ^ pv->couleurs[pt->couleur_caractere - CARACTERE_NOIR].pixel;
	    if (col == 0)
		col = pv->couleurs[0].pixel ^ pv->couleurs[7].pixel;
	    XSetForeground(XtDisplay(w), pv->gc_curseur, col);
	}
	XGrabServer(XtDisplay(w));
	XFillRectangle(XtDisplay(w), XtWindow(w), pv->gc_curseur, pv->x_courant, pv->y_courant - pv->ascent_fonte_base, pv->largeur_fonte_base, pv->hauteur_fonte_base);
	(*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc_curseur, pv->x_courant, pv->y_courant - pv->ascent_fonte_base, pv->largeur_fonte_base, pv->hauteur_fonte_base);
	XUngrabServer(XtDisplay(w));
    }
}


/*
 * Change la phase du curseur toutes les 1/2 secondes pour le faire clignoter
 */
static XtTimerCallbackProc 
clignote_curseur(w, id)
VideotexWidget w;
XtIntervalId *id;
{
    register VideotexPart *pv = &w->videotex;
    (*pv->fonction_affichage_curseur) (w, 1);
    pv->phase_curseur ^= 1;
    XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w), (unsigned long) 500, (XtTimerCallbackProc) clignote_curseur, (caddr_t)w);
}

/*
 * Active ou desactive le curseur
 */
static void 
curseur_actif(w)
VideotexWidget w;
{
    w->videotex.fonction_affichage_curseur = affiche_curseur;
}

static void 
curseur_inactif(w)
VideotexWidget w;
{
    w->videotex.fonction_affichage_curseur = nulle;
}

/*
 * Efface l'ecran
 */
static void
efface_rectangle_ecran (w, r1, c1, r2, c2)
VideotexWidget w;
int r1, c1, r2, c2;
{
    register VideotexPart *pv = &w->videotex;
    int x, y, l, h;
    register int r, c;

    if (r1 < 0 || r1 > 24 || r2 < 0 || r2 > 24 
	|| c1 < 1 || c1 > 40 || c2 < 1 || c2 > 40)
	return;

    affiche_indicateur_connexion(w);
    pv->indice_couleur_caractere_precedent = pv->indice_couleur_fond_precedent = -1;
    (*pv->fonction_affichage_curseur) (w, 0);

    x = (c1 - 1) * pv->largeur_fonte_base;
    y = r1 * pv->hauteur_fonte_base;
    l = (c2 - c1 + 1) * pv->largeur_fonte_base;
    h = (r2 - r1 + 1) * pv->hauteur_fonte_base;

    /* Efface a l'ecran... */
    (*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc_pixmap, x, y, l, h);
    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc_pixmap, x, y, l, h);
    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc_pixmap, x, y, l, h);
    XClearArea(XtDisplay(w), XtWindow(w), x, y, l, h, False);
    (*pv->fonction_affichage_curseur) (w, 0);

    /* et dans les attributs */
    for (r = r1 ; r <= r2 ; r++)  {
	for (c = c1 ; c <= c2 ; c++) {
	    memcpy ((char*)&pv->attributs[r][c-1], (char*)&attributs_initiaux[0], sizeof(struct attribut));
	}
    }
}

/*
 * Remplissage de la ligne (Sur ^X)
 */
static void 
remplissage_ligne(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    register int i;
    struct attribut attr;

    /* prise en compte des attributs courants */
    attr.code[0] = ' ';
    attr.code[1] = 0;
    attr.inversion = pv->inversion;
    attr.couleur_caractere = pv->couleur_caractere;
    attr.taille_caractere = NORMALE;
    attr.jeu = pv->jeu_courant;
    attr.clignotement = pv->clignotement;
    attr.drcs = pv->flag_drcs;
    attr.f.delimiteur = 0;
    attr.f.position = POS_BG;

    /* lecture de la couleur de fond si G1 et a valider */
    if ((pv->jeu_courant == G1) && (pv->a_valider_si_delimiteur & BIT_COULEUR_FOND)) {
	couleur_fond(w, pv->valeur_couleur_fond);
	pv->a_valider_si_delimiteur &= ~BIT_COULEUR_FOND;
    }

    /*
     * lecture de la couleur de fond, du masquage et du soulignement a gauche
     * du curseur
     */
    if (pv->jeu_courant == G0) {
	if (pv->colonne > 1) {
	    attr.couleur_fond = pv->attributs[pv->rangee][pv->colonne - 2].couleur_fond;
	    attr.soulignement = pv->attributs[pv->rangee][pv->colonne - 2].soulignement;
	    attr.masquage = pv->attributs[pv->rangee][pv->colonne - 2].masquage;
	} else {
	    attr.couleur_fond = FOND_NOIR;
	    attr.soulignement = FIN_LIGNAGE;
	    attr.masquage = DEMASQUAGE;
	}
    } else {
	attr.couleur_fond = pv->couleur_fond;
	attr.masquage = pv->masquage;
	attr.soulignement = pv->soulignement;
    }

    /* recopie les attributs sur toute la rangee et re-affiche */
    for (i = pv->colonne; i <= 40; i++)
	memcpy(&pv->attributs[pv->rangee][i - 1], &attr, sizeof(struct attribut));
    reaffiche_rectangle_ecran (w, pv->rangee, pv->colonne, pv->rangee, 40);

    /* Test de double hauteur */
    if ((pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR) && pv->rangee > 1) {
	/* on prends la couleur de fond du caractere immediatement a gauche */
	if (pv->colonne > 1) {
	    attr.couleur_fond = pv->attributs[pv->rangee - 1][pv->colonne - 2].couleur_fond;
	    attr.soulignement = pv->attributs[pv->rangee - 1][pv->colonne - 2].soulignement;
	    attr.masquage = pv->attributs[pv->rangee - 1][pv->colonne - 2].masquage;
	} else {
	    attr.couleur_fond = FOND_NOIR;
	    attr.soulignement = FIN_LIGNAGE;
	    attr.masquage = DEMASQUAGE;
	}

	/* Marque le haut du caractere double hauteur */
	attr.code[0] |= 0x80;
	
	/* on remplit la rangee et on re-affiche */
	for (i = pv->colonne; i <= 40; i++)
	    memcpy(&pv->attributs[pv->rangee - 1][i - 1], &attr, sizeof(struct attribut));
	reaffiche_rectangle_ecran (w, pv->rangee-1, pv->colonne, pv->rangee-1, 40);
    }
}

/*
 * Traitement fontes utilisees pour le decodage videotex
 */

/*
 * Selection des fontes G0, G1, ou G2 suite aux appels S1, S0, ou  SS2)
 */
static void 
selection_fonte(w, f)
VideotexWidget w;
char            f;
{
    register VideotexPart *pv = &w->videotex;
    register XFontStruct *xfs_sauve = pv->xfs_courante;

    pv->jeu_courant = f;
    pv->xfs_courante = pv->fontes[12 * (pv->soulignement - FIN_LIGNAGE) + 4 * f + pv->taille_caractere - NORMALE];

    if (pv->xfs_courante == xfs_sauve)
	return;

    XSetFont(XtDisplay(w), pv->gc, pv->xfs_courante->fid);
    XSetFont(XtDisplay(w), pv->gc_curseur, pv->xfs_courante->fid);
}

/*
 * Selection du jeu de caractere
 */
static void 
selection_jeu(w, jeu)
VideotexWidget w;
char            jeu;
{
#ifdef DEBUG
    printf("\nSelection du jeu %d\n", jeu);
#endif
    selection_fonte(w, jeu);
}

/*
 * fonction d'inversion
 */
static void 
inversion(w, c)
VideotexWidget w;
char            c;
{
    w->videotex.inversion = c;
#ifdef DEBUG
    printf("\ninversion =\n", c);
#endif
}

static void 
masquage(w, masque)
VideotexWidget w;
char masque;
{
    w->videotex.masquage = masque;
}

/*
 * Fonction de demasquage du plein ecran
 */
static void 
demasquer_le_plein_ecran(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    XCopyArea(XtDisplay(w), pv->ecran_demasquage, XtWindow(w), pv->gc, 0, pv->hauteur_fonte_base, pv->largeur_ecran, pv->hauteur_ecran - pv->hauteur_fonte_base, 0, pv->hauteur_fonte_base);
    (*pv->copy_area_sauve) (XtDisplay(w), pv->ecran_demasquage, pv->ecran_sauve, pv->gc, 0, pv->hauteur_fonte_base, pv->largeur_ecran, pv->hauteur_ecran - pv->hauteur_fonte_base, 0, pv->hauteur_fonte_base);
}

static void 
masquer_le_plein_ecran(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    XCopyArea(XtDisplay(w), pv->ecran_masquage, XtWindow(w), pv->gc, 0, pv->hauteur_fonte_base, pv->largeur_ecran, pv->hauteur_ecran - pv->hauteur_fonte_base, 0, pv->hauteur_fonte_base);
    (*pv->copy_area_sauve) (XtDisplay(w), pv->ecran_masquage, pv->ecran_sauve, pv->gc, 0, pv->hauteur_fonte_base, pv->largeur_ecran, pv->hauteur_ecran - pv->hauteur_fonte_base, 0, pv->hauteur_fonte_base);
}

/*
 * Masquage/demasquage d'une rangee
 */
static void 
demasque_un_caractere(w, r, c)
VideotexWidget w;
char r, c;
{
    register VideotexPart *pv = &w->videotex;
    int x, y, width, height, coefy, coefx;

    coefy = (pv->attributs[r][c - 1].taille_caractere == NORMALE || pv->attributs[r][c - 1].taille_caractere == DOUBLE_LARGEUR ? 1 : 2);
    coefx = (pv->attributs[r][c - 1].taille_caractere == NORMALE || pv->attributs[r][c - 1].taille_caractere == DOUBLE_HAUTEUR ? 1 : 2);

    x = (c - 1) * pv->largeur_fonte_base;
    y = (r + 1 - coefy) * pv->hauteur_fonte_base;
    width = pv->largeur_fonte_base * coefx;
    height = pv->hauteur_fonte_base * coefy;

    XCopyArea(XtDisplay(w), pv->ecran_demasquage, pv->ecran_masquage, pv->gc, x, y, width, height, x, y);
}

static void 
masque_un_caractere(w, r, c)
VideotexWidget w;
char r, c;
{
    register VideotexPart *pv = &w->videotex;
    int x, y, width, height, coefy, coefx;

    coefy = (pv->attributs[r][c - 1].taille_caractere == NORMALE || pv->attributs[r][c - 1].taille_caractere == DOUBLE_LARGEUR ? 1 : 2);
    coefx = (pv->attributs[r][c - 1].taille_caractere == NORMALE || pv->attributs[r][c - 1].taille_caractere == DOUBLE_HAUTEUR ? 1 : 2);

    x = (c - 1) * pv->largeur_fonte_base;
    y = (r + 1 - coefy) * pv->hauteur_fonte_base;
    width = pv->largeur_fonte_base * coefx;
    height = pv->hauteur_fonte_base * coefy;

    XCopyArea(XtDisplay(w), pv->ecran_masquage, pv->ecran_demasquage, pv->gc, x, y, width, height, x, y);
}

/*
 * fonctions de scrolling haut/bas/droite/gauche
 */

/* Vers le haut */
static void 
scroll_haut(w, rangee, n)
VideotexWidget w;
int rangee, n;
{
    register VideotexPart *pv = &w->videotex;
    register int i, y, h;

    y = rangee * pv->hauteur_fonte_base;
    h = n * pv->hauteur_fonte_base;

    XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w), pv->gc, 0, y + h, pv->largeur_ecran, pv->hauteur_ecran - y - h, 0, y);
    XClearArea(XtDisplay(w), XtWindow(w), 0, pv->hauteur_ecran - h, pv->largeur_ecran, h, False);
    (*pv->copy_area_sauve) (XtDisplay(w), pv->ecran_sauve, pv->ecran_sauve, pv->gc, 0, y + h, pv->largeur_ecran, pv->hauteur_ecran - y - h, 0, y);
    (*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc_pixmap, 0, pv->hauteur_ecran - h, pv->largeur_ecran, h);
    XCopyArea(XtDisplay(w), pv->ecran_masquage, pv->ecran_masquage, pv->gc, 0, y + h, pv->largeur_ecran, pv->hauteur_ecran - y - h, 0, y);
    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc_pixmap, 0, pv->hauteur_ecran - h, pv->largeur_ecran, h);
    XCopyArea(XtDisplay(w), pv->ecran_demasquage, pv->ecran_demasquage, pv->gc, 0, y + h, pv->largeur_ecran, pv->hauteur_ecran - y - h, 0, y);
    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc_pixmap, 0, pv->hauteur_ecran - h, pv->largeur_ecran, h);

    /* scroll des attributs */
    for (i = rangee+n ; i < 25; i++)
	memcpy(pv->attributs[i - n], pv->attributs[i], 40 * sizeof(struct attribut));

    for (i = 0 ; i != n ; i++)
	memcpy(pv->attributs[24-i], attributs_initiaux, 40 * sizeof(struct attribut));
}

/* Vers le bas */
static void 
scroll_bas(w, rangee, n)
VideotexWidget w;
int rangee, n;
{
    register VideotexPart *pv = &w->videotex;
    register int i, y, h;

    y = rangee * pv->hauteur_fonte_base;
    h = n * pv->hauteur_fonte_base;

    XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w), pv->gc, 0, y, pv->largeur_ecran, pv->hauteur_ecran - y, 0, y + h);
    XClearArea(XtDisplay(w), XtWindow(w), 0, y, pv->largeur_ecran, h, False);
    (*pv->copy_area_sauve) (XtDisplay(w), pv->ecran_sauve, pv->ecran_sauve, pv->gc, 0, y, pv->largeur_ecran, pv->hauteur_ecran - y, 0, y + h);
    (*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc_pixmap, 0, y, pv->largeur_ecran, h);
    XCopyArea(XtDisplay(w), pv->ecran_masquage, pv->ecran_masquage, pv->gc, 0, y, pv->largeur_ecran, pv->hauteur_ecran - y, 0, y + h);
    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc_pixmap, 0, y, pv->largeur_ecran, h);
    XCopyArea(XtDisplay(w), pv->ecran_demasquage, pv->ecran_demasquage, pv->gc, 0, y, pv->largeur_ecran, pv->hauteur_ecran - y, 0, y + h);  
    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc_pixmap, 0, y, pv->largeur_ecran, h);

    /* scroll des attributs */
    for (i = 24; i >= rangee + n ; i--)
	memcpy(pv->attributs[i], pv->attributs[i - n], 40 * sizeof(struct attribut));
    for (i = 0 ; i != n ; i++)
	memcpy(pv->attributs[rangee+i], attributs_initiaux, 40 * sizeof(struct attribut));
}

/* Vers la droite */
static void scroll_droite (w, colonne, n)
VideotexWidget w;
int colonne, n;
{
    register VideotexPart *pv = &w->videotex;
    register int i;
    int r, x, y, wi;

    r = pv->rangee;
    x = (colonne - 1) * pv->largeur_fonte_base;
    y = pv->rangee * pv->hauteur_fonte_base;
    wi = n * pv->largeur_fonte_base;

    /* Scroll de la rangee ecran */
    XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w), pv->gc, x, y, pv->largeur_ecran - x, pv->hauteur_fonte_base, x + wi, y);
    XClearArea(XtDisplay(w), XtWindow(w), x, y, wi, pv->hauteur_fonte_base, False);
    (*pv->copy_area_sauve) (XtDisplay(w), pv->ecran_sauve, pv->ecran_sauve, pv->gc, x, y, pv->largeur_ecran - x, pv->hauteur_fonte_base, x + wi, y);
    (*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc_pixmap, x, y, wi, pv->hauteur_fonte_base); 
    XCopyArea(XtDisplay(w), pv->ecran_masquage, pv->ecran_masquage, pv->gc, x, y, pv->largeur_ecran - x, pv->hauteur_fonte_base, x + wi, y);
    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc_pixmap, x, y, wi, pv->hauteur_fonte_base); 
    XCopyArea(XtDisplay(w), pv->ecran_demasquage, pv->ecran_demasquage, pv->gc, x, y, pv->largeur_ecran - x, pv->hauteur_fonte_base, x + wi, y);
    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc_pixmap, x, y, wi, pv->hauteur_fonte_base);

    /* Scroll des attributs */
    for (i = 40 ; i >= colonne + n ; i--) 
	memcpy ((char*)&pv->attributs[r][i-1], (char*)&pv->attributs[r][i-n-1], sizeof(struct attribut));
    for (i = 0 ; i != n ; i++)
	memcpy ((char*)&pv->attributs[r][i], (char*)&attributs_initiaux[0], sizeof(struct attribut));
}

/* Vers la gauche */
static void scroll_gauche (w, colonne, n)
VideotexWidget w;
int colonne, n;
{
    register VideotexPart *pv = &w->videotex;
    register int i;
    int r, x, y, wi;

    r = pv->rangee;
    x = (colonne - 1) * pv->largeur_fonte_base;
    y = pv->rangee * pv->hauteur_fonte_base;
    wi = n * pv->largeur_fonte_base;

    /* Scroll de la rangee ecran */
    XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w), pv->gc, x + wi, y, pv->largeur_ecran - x - wi, pv->hauteur_fonte_base, x, y);
    XClearArea(XtDisplay(w), XtWindow(w), pv->largeur_ecran - wi, y, wi, pv->hauteur_fonte_base, False);
    (*pv->copy_area_sauve) (XtDisplay(w), pv->ecran_sauve, pv->ecran_sauve, pv->gc, x + wi, y, pv->largeur_ecran - x - wi, pv->hauteur_fonte_base, x, y);
    (*pv->fill_rectangle_sauve) (XtDisplay(w), pv->ecran_sauve, pv->gc_pixmap, pv->largeur_ecran - wi, y, wi, pv->hauteur_fonte_base);
    XCopyArea(XtDisplay(w), pv->ecran_masquage, pv->ecran_masquage, pv->gc, x + wi, y, pv->largeur_ecran - x - wi, pv->hauteur_fonte_base, x, y); 
    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc_pixmap, pv->largeur_ecran - wi, y, wi, pv->hauteur_fonte_base);
    XCopyArea(XtDisplay(w), pv->ecran_demasquage, pv->ecran_demasquage, pv->gc, x + wi, y, pv->largeur_ecran - x - wi, pv->hauteur_fonte_base, x, y);
    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc_pixmap, pv->largeur_ecran - wi, y, wi, pv->hauteur_fonte_base);

    /* Scroll des attributs */
    for (i = colonne + n ; i <= 40 ; i++)
	memcpy ((char*)&pv->attributs[r][i-n-1], (char*)&pv->attributs[r][i-1], sizeof(struct attribut));
    for (i = 0 ; i != n ; i++)
	memcpy ((char*)&pv->attributs[r][39-i], (char*)&attributs_initiaux[0], sizeof(struct attribut));
}

/*
 * fonction de sonnerie
 */

static void 
sonnerie(w)
VideotexWidget w;
{
    XBell(XtDisplay(w), 0);
}

static void 
soulignement(w, lignage)
VideotexWidget w;
char            lignage;
{
    register VideotexPart *pv = &w->videotex;
#ifdef DEBUG
    printf("\nsoulignement = %d\n", lignage);
#endif

    if (pv->soulignement == lignage)
	return;

    pv->soulignement = lignage;
    selection_fonte(w, pv->jeu_courant);
}

/*
 * change la taille des caracteres
 */
static void 
taille_caractere(w, taille)
VideotexWidget w;
char            taille;
{
    register VideotexPart *pv = &w->videotex;
    if (pv->taille_caractere == taille)
	return;

    if ((taille == DOUBLE_HAUTEUR || taille == DOUBLE_GRANDEUR) && pv->rangee <= 1)
	return;

    pv->taille_caractere = taille;

    selection_fonte(w, pv->jeu_courant);

    switch (taille) {

      case DOUBLE_LARGEUR:
	pv->largeur_caractere = 2;
	break;

      case DOUBLE_HAUTEUR:
	pv->hauteur_caractere = 2;
	break;

      case DOUBLE_GRANDEUR:
	pv->hauteur_caractere = pv->largeur_caractere = 2;
	break;

      default:
	pv->largeur_caractere = pv->hauteur_caractere = 1;
	break;
    }
}


/* sauvegarde avant rangee 0 */
static void 
sauve_avant_rangee_0(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    pv->derniere_rangee_avant_0 = pv->rangee;
    pv->derniere_colonne_avant_0 = pv->colonne;
    pv->inversion_avant_0 = pv->inversion;
    pv->couleur_caractere_avant_0 = pv->couleur_caractere;
    pv->taille_caractere_avant_0 = pv->taille_caractere;
    pv->jeu_avant_0 = pv->jeu_courant;
    
    /* Acces en rangee 0 ==> RAZ des flags G'1 dans G0 et G'10 */
    pv->flag_GP1_dans_G0 = pv->flag_GP10_dans_G1 = pv->flag_drcs = False;
}


/*
 * Fonctions de deplacement par rangee, colonne : utilisees par LF, VT, BS,
 * HT
 * 
 * L'ecriture d'un caractere apres deplacement se fait avec les attributs serie
 * de la zone d'accueil jusqu'a ce qu'un delimiteur explicite (blanc ou
 * semi-graphique) permette la validation des attributs serie (couleur_fond,
 * lignage, masquage).
 * 
 */
static void 
rangee_suivante(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    if (pv->rangee != 0) {
	pv->rangee++;
	if (pv->rangee > 24) {
	    if (!pv->mode_scroll) {
		pv->rangee = 1;
		if (pv->hauteur_caractere > 1)
		    taille_caractere(w, NORMALE);
	    } else {
		scroll_haut(w, 1, 1);
		if (pv->hauteur_caractere > 1)
		    scroll_haut(w, 1, 1);
		pv->rangee = 24;
	    }
	}
    } else {
	/*
	 * On est en rangee 0 ==> on restitue la derniere position avant la
	 * rangee 0
	 * 
	 * restitue la position et les attributs caractere
	 */
	pv->rangee = pv->derniere_rangee_avant_0;
	pv->colonne = pv->derniere_colonne_avant_0;
	couleur_caractere(w, pv->couleur_caractere_avant_0);
	inversion(w, pv->inversion_avant_0);
	taille_caractere(w, pv->taille_caractere_avant_0);
	selection_jeu(w, pv->jeu_avant_0);
	X_COURANT;
    }
    selection_attributs_zone_accueil(w);
    Y_COURANT;
}


static void 
colonne_suivante(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    pv->colonne++;
    if (pv->colonne > 40) {
	pv->colonne = pv->largeur_caractere;
	rangee_suivante(w);
    }
    selection_attributs_zone_accueil(w);
    X_COURANT;
}

static void 
rangee_precedente(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    pv->rangee--;
    if (pv->rangee < 1) {
	if (!pv->mode_scroll)
	    pv->rangee = 25 - pv->hauteur_caractere;
	else {
	    scroll_bas(w, 1, 1);
	    pv->rangee = 1;
	}
    }
    selection_attributs_zone_accueil(w);
    Y_COURANT;
}

static void 
colonne_precedente(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    pv->colonne--;
    if (pv->colonne < 1) {
	pv->colonne = 41 - pv->largeur_caractere;
	rangee_precedente(w);
    }
    selection_attributs_zone_accueil(w);
    X_COURANT;
}

/*
 * fixe les attributs caractere par defaut : - couleur caractere = blanc -
 * pas d'inversion - taille caractere = normale - fonte G0
 */
static void 
attributs_caractere_par_defaut(w)
VideotexWidget w;
{
    inversion(w, POSITIF);
    couleur_caractere(w, CARACTERE_BLANC);
    taille_caractere(w, NORMALE);
    clignotement(w, LETTRES_FIXES);
    selection_jeu(w, G0);
}

/*
 * fixe les attributs serie (rangee) par defaut : - couleur fond = noir 
 * pas de masquage, pas de soulignement 
 */
static void 
attributs_serie_par_defaut(w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    couleur_fond(w, FOND_NOIR);
    soulignement(w, FIN_LIGNAGE);
    masquage(w, DEMASQUAGE);
    pv->valeur_couleur_fond = FOND_NOIR;
    pv->valeur_lignage = FIN_LIGNAGE;
    pv->valeur_masquage = DEMASQUAGE;
}

/* 
 * Traitement CSI 
 */

static int atoni(str, n)
char	*str;
int n;
{
    char buf[10];

    if (!n) 
	return (1);

    n = n > 9 ? 9 : n;
    (void)strncpy (buf, str, n);
    buf[n] = 0;
    return (atoi(buf));
}

/* positionnement CSI */
static void csi_positionnement (w, r, c)
VideotexWidget w;
int r, c;
{
    register VideotexPart *pv = &w->videotex;

    (*pv->fonction_affichage_curseur) (w, 0);

    if (r > 25)
	r = 25;
    
    if (r < 1)
	r = 1;

    if (c > 40)
	c = 40;
    
    if (c < 1)
	c = 1;

    pv->rangee = r;
    pv->colonne = c;
    X_COURANT;
    Y_COURANT;

    (*pv->fonction_affichage_curseur) (w, 0);
}

/* Insertion/suppression CSI */
static void csi_insertion_caractere (w, n)
VideotexWidget w;
int n;
{
    register VideotexPart *pv = &w->videotex;
    n = (n > 40 - pv->colonne ? 40 - pv->colonne : n);
    scroll_droite (w, pv->colonne, n);
}

static void csi_insertion_rangee (w, n)
VideotexWidget w;
int n;
{
    register VideotexPart *pv = &w->videotex;
    n = (n > 24 - pv->rangee ? 24 - pv->rangee : n);
    scroll_bas (w, pv->rangee, n);
}

static void csi_suppression_caractere (w, n)
VideotexWidget w;
int n;
{
    register VideotexPart *pv = &w->videotex;
    n = (n > 40 - pv->colonne ? 40 - pv->colonne : n);
    scroll_gauche (w, pv->colonne, n);
}

static void csi_suppression_rangee (w, n)
VideotexWidget w;
int n;
{
    register VideotexPart *pv = &w->videotex;
    n = (n > 24 - pv->rangee ? 24 - pv->rangee : n);
    scroll_haut (w, pv->rangee, n);
}


/* Traite les positionnement + de debut/fin d'insertion */
static void traite_csi (w)
VideotexWidget w;
{
    register VideotexPart *pv = &w->videotex;
    register int l = pv->nb_csi;
    register char *csi = pv->buffer_csi, *p;

    switch (*(csi+l-1)) {
	
      case 'h' : /* Debut insertion */
	  
	  pv->flag_insertion = True;
	  break;

	case 'l' : /* Fin insertion */

	  pv->flag_insertion = False;
	  break;

	case 'A' : /* Vers le haut */

	   csi_positionnement (w, pv->rangee - atoni (csi, l-1), pv->colonne);
	  break;
	    
	case 'B' : /* Vers le bas */

	   csi_positionnement (w, pv->rangee + atoni (csi, l-1), pv->colonne);
	  break;

	case 'C' : /* vers droite */
	    
	   csi_positionnement (w, pv->rangee, pv->colonne + atoni (csi, l-1));
	  break;

	case 'D' : /* vers gauche */
	    
	   csi_positionnement (w, pv->rangee, pv->colonne - atoni (csi, l-1));
	  break;

	case 'H' : /* Positionnement absolu */

	    p = strchr (csi, ';');
	  if (p) {
	      *p = 0;
	      csi_positionnement (w, atoi (csi), atoni (p+1, csi-p+l-2));
	  }
	  break;

	  /*
	   * Insertion/suppression
	   */

	case '@' : /* Insertion de caractere */

	    if (pv->flag_insertion && pv->rangee)
		csi_insertion_caractere (w, atoni (csi, l-1));
	  break;

	case 'P' : /* Suppression de caractere */

	    if (pv->flag_insertion && pv->rangee)
		csi_suppression_caractere (w, atoni (csi, l-1));
	  break;

	case 'L' : /* Insertion de rangee */
	    
	    if (pv->flag_insertion && pv->rangee)
		csi_insertion_rangee (w, atoni (csi, l-1));
	  break;

	case 'M' : /* Suppression de rangee */
	    
	    if (pv->flag_insertion && pv->rangee)
		csi_suppression_rangee (w, atoni (csi, l-1));
	  break;

	    /*
	     * Effacement sur ecran
	     */
	      
	case 'J' :

	    switch (*csi) {

	      case 'J' :
	      case '0' : /* Effacement depuis le curseur --> fin ecran */
		  efface_rectangle_ecran (w, pv->rangee, pv->colonne, pv->rangee, 40);
		  efface_rectangle_ecran (w, pv->rangee+1, 1, 24, 40);
		  break;

	      case '1' : /* Effacement depuis le debut ecran --> curseur */
		  
		  efface_rectangle_ecran (w, 1, 1, pv->rangee-1, 40);
		  efface_rectangle_ecran (w, pv->rangee, 1, pv->rangee, pv->colonne);
		  break;

	      case '2' : /* Effacement de tout l'ecran */
		  
		  efface_rectangle_ecran (w, 1, 1, 24, 40);
		  break;

		default :
		    break;
	      }

	  break;


	    /*
	     * Effacement sur rangee
	     */

	case 'K' :

	    switch (*csi) {

	      case 'K' :
	      case '0' : /* Effacement depuis le curseur --> fin rangee */

		  efface_rectangle_ecran (w, pv->rangee, pv->colonne, pv->rangee, 40);
		  break;

	      case '1' : /* Effacement depuis le debut --> curseur */
		  
		  efface_rectangle_ecran (w, pv->rangee, 1, pv->rangee, pv->colonne);
		  break;

	      case '2' : /* Effacement de toute la rangee */
		  
		  efface_rectangle_ecran (w, pv->rangee, 1, pv->rangee, 40);
		  break;

		default :
		    break;
	      }

	  break;

	default : /* Non traite */
	    break;
      }

    /* RAZ du buffer CSI */
    memset (csi, 0, 10);
    pv->nb_csi = 0;
}
    
/*
 * Methodes du Widget
 */

static void 
Initialize(greq, gnew, args, num_args)
Widget          greq, gnew;
ArgList         args;
Cardinal       *num_args;
{
    register int i;
    XWindowAttributes attrib;
    VideotexWidget  w = (VideotexWidget) gnew;
    register VideotexPart *pv = &w->videotex;
    int nplanes = DisplayPlanes(XtDisplay(w), DefaultScreen(XtDisplay(w)));
    XGCValues	values;

    /*
     * Chargement des fontes
     */
    if (pv->petite_fonte) {

	if (pv->fonte_g0_no_p->max_bounds.width != 8)
	    erreur_videotex ("Warning: largeur petite fonte = %d (8 conseille)\n", pv->fonte_g0_no_p->max_bounds.width);
	if (pv->fonte_g0_no_p->ascent+pv->fonte_g0_no_p->descent != 10)
	    erreur_videotex ("Warning: hauteur petite fonte = %d (10 conseille)\n", pv->fonte_g0_no_p->ascent+pv->fonte_g0_no_p->descent);

	pv->fontes[0] = pv->fonte_g0_no_p;
	pv->fontes[1] = pv->fonte_g0_dh_p;
	pv->fontes[2] = pv->fonte_g0_dl_p;
	pv->fontes[3] = pv->fonte_g0_dt_p;
	pv->fontes[4] = pv->fonte_g1_p;
	pv->fontes[12] = pv->fonte_g0_nos_p;
	pv->fontes[13] = pv->fonte_g0_dhs_p;
	pv->fontes[14] = pv->fonte_g0_dls_p;
	pv->fontes[15] = pv->fonte_g0_dts_p;
	pv->fontes[16] = pv->fonte_g1s_p;
    }
    else {
	if (pv->fonte_g0_no_g->max_bounds.width != 16)
	    erreur_videotex ("Warning: largeur grande fonte = %d (16 conseille)\n", pv->fonte_g0_no_p->max_bounds.width);
	if (pv->fonte_g0_no_g->ascent+pv->fonte_g0_no_g->descent != 20)
	    erreur_videotex ("Warning: hauteur grande fonte = %d (20 conseille)\n", pv->fonte_g0_no_g->ascent+pv->fonte_g0_no_g->descent);


	pv->fontes[0] = pv->fonte_g0_no_g;
	pv->fontes[1] = pv->fonte_g0_dh_g;
	pv->fontes[2] = pv->fonte_g0_dl_g;
	pv->fontes[3] = pv->fonte_g0_dt_g;
	pv->fontes[4] = pv->fonte_g1_g;
	pv->fontes[12] = pv->fonte_g0_nos_g;
	pv->fontes[13] = pv->fonte_g0_dhs_g;
	pv->fontes[14] = pv->fonte_g0_dls_g;
	pv->fontes[15] = pv->fonte_g0_dts_g;
	pv->fontes[16] = pv->fonte_g1s_g;
    }
 
    for (i = 0 ; i != 24 ; i++) {
	if (i >= 5 && i <= 7) 
	    pv->fontes[i] = pv->fontes[4];
	else if (i >= 17 && i <= 19)
	    pv->fontes[i] = pv->fontes[16];
	else if ((i >= 8 && i <= 11) || (i >= 20 && i <= 23)) 
	    pv->fontes[i] = pv->fontes[i-8];
	else 
	    ;
    }

    /* Init de la structure */
    pv->gc = XCreateGC (XtDisplay(gnew), RootWindowOfScreen(XtScreen(gnew)), 0, NULL);
    pv->rangee = 1;		
    pv->colonne = 1;	
    pv->couleur_caractere = CARACTERE_BLANC;
    pv->couleur_fond = FOND_NOIR;		
    pv->taille_caractere = NORMALE; 	
    pv->inversion = POSITIF;                
    pv->masquage = DEMASQUAGE;              
    pv->soulignement = FIN_LIGNAGE;         
    pv->clignotement = LETTRES_FIXES;       
    pv->jeu_courant = G0;                   
    pv->largeur_caractere = 1;		
    pv->hauteur_caractere = 1;		
    pv->derniere_colonne_avant_0 = 1; 
    pv->indice_couleur_caractere_precedent = -1;
    pv->indice_couleur_fond_precedent = -1;
    pv->indice_couleur_caractere = -1;
    pv->indice_couleur_fond = -1;
    pv->inversion_avant_0 = POSITIF;
    pv->couleur_caractere_avant_0 = CARACTERE_BLANC;
    pv->taille_caractere_avant_0 = NORMALE;
    pv->jeu_avant_0 = G0;
    pv->masquage_plein_ecran = True;
    pv->fonction_affichage_curseur = nulle;
    pv->hauteur_fonte_base = pv->fontes[0]->ascent + pv->fontes[0]->descent;
    pv->largeur_fonte_base = pv->fontes[0]->max_bounds.width;
    pv->descent_fonte_base = pv->fontes[0]->descent;
    pv->ascent_fonte_base = pv->fontes[0]->ascent;
    pv->largeur_ecran = pv->largeur_fonte_base * 40;
    pv->hauteur_ecran = pv->hauteur_fonte_base * 25;
    pv->flag_drcs = False;
    pv->filtrage = pv->pro2 = pv->pro1 = pv->plein_ecran = False;
    pv->phase_curseur = pv->flip_flop = 0;
    pv->flag_sauvegarde = False;
    pv->xfs_courante = NULL;
    pv->nb_a_ignorer = 0;
    pv->etat = EtatNormal;
    pv->mode_scroll = False;
    pv->mode_videotex = True;
    pv->flag_insertion = False;
    pv->nb_csi = 0;
    memset (pv->buffer_csi, 0, 10);

    if (!(pv->ecran_masquage = XCreatePixmap(XtDisplay(w), RootWindowOfScreen(XtScreen(w)), pv->largeur_ecran, pv->hauteur_ecran, nplanes))) {
	erreur_videotex ("Erreur creation pixmap ecran_masquage\n");
	exit(1);
    }
    if (!(pv->ecran_demasquage = XCreatePixmap(XtDisplay(w), RootWindowOfScreen(XtScreen(w)), pv->largeur_ecran, pv->hauteur_ecran, nplanes))) {
	erreur_videotex ("Erreur creation pixmap pv->ecran_demasquage\n");
	exit(1);
    }
    if (!(pv->ecran_sauve = XCreatePixmap(XtDisplay(w), RootWindowOfScreen(XtScreen(w)), pv->largeur_ecran, pv->hauteur_ecran, nplanes))) {
	erreur_videotex ("Erreur creation pixmap pv->ecran_sauve\n");
	exit(1);
    }

    /* fontes G0 dans gc */
    XSetFont(XtDisplay(w), pv->gc, pv->fontes[0]->fid);

    /* Chargement des couleurs */
    if (!init_couleurs(w))
	exit(1);

    /* GC du curseur */
    values.function = GXxor;
    pv->gc_curseur = XtGetGC(gnew, (unsigned) GCFunction, &values);

    /* GC indicateur de connexion */
    values.font = pv->fontes[0]->fid;
    pv->gc_indicateur = XtGetGC(gnew, GCFont, &values);

    /* GC pour la RAZ des pixmaps */
    values.fill_style = FillSolid;
    values.foreground = BlackPixel(XtDisplay(w), DefaultScreen(XtDisplay(w)));
    pv->gc_pixmap = XtGetGC(gnew, GCForeground | GCFillStyle, &values);

    /* init des pixmaps */
    XFillRectangle(XtDisplay(w), pv->ecran_masquage, pv->gc_pixmap, 0, 0, pv->largeur_ecran, pv->hauteur_ecran);
    XFillRectangle(XtDisplay(w), pv->ecran_demasquage, pv->gc_pixmap, 0, 0, pv->largeur_ecran, pv->hauteur_ecran);
    XFillRectangle(XtDisplay(w), pv->ecran_sauve, pv->gc_pixmap, 0, 0, pv->largeur_ecran, pv->hauteur_ecran);

    /* init des couleurs dans les GC */
    if (nplanes > 1) {
	XSetForeground(XtDisplay(w), pv->gc, pv->couleurs[7].pixel);
	XSetBackground(XtDisplay(w), pv->gc, pv->couleurs[0].pixel);
	XSetForeground(XtDisplay(w), pv->gc_indicateur, pv->couleurs[0].pixel);
	XSetBackground(XtDisplay(w), pv->gc_indicateur, pv->couleurs[7].pixel);
    } else {
	XSetForeground(XtDisplay(w), pv->gc_curseur, WhitePixel(XtDisplay(w), DefaultScreen(XtDisplay(w))));
	XSetForeground(XtDisplay(w), pv->gc_indicateur, BlackPixel(XtDisplay(w), DefaultScreen(XtDisplay(w))));
	XSetBackground(XtDisplay(w), pv->gc_indicateur, WhitePixel(XtDisplay(w), DefaultScreen(XtDisplay(w))));
    }

    /* init des attributs par rangee */
    init_attributs(w);

    /*
     * Init des pointeurs de fonctions d'affichage suivant monochrome/couleur
     */
    i = (nplanes == 1);

    /* Pour l'affichage en monochrome */
    if (i)
	XSetFillStyle(XtDisplay(w), pv->gc, FillOpaqueStippled);

    init_fonctions_affichage(w, i);
    pv->affiche_fond_pour_clignotement = fonctions_affiche_fond_pour_clignotement[i];

    XGetWindowAttributes(XtDisplay(w), DefaultRootWindow(XtDisplay(w)), &attrib);
    pv->flag_bs = (attrib.backing_store != NotUseful ? True : False);

    if (pv->flag_bs) {
	pv->draw_string_sauve = pv->draw_image_string_sauve = pv->fill_rectangle_sauve = pv->copy_area_sauve = (int (*) ()) nulle;
    } else {
	pv->draw_string_sauve = XDrawString;
	pv->draw_image_string_sauve = XDrawImageString;
	pv->fill_rectangle_sauve = XFillRectangle;
	pv->copy_area_sauve = XCopyArea;
    }

    /*
     * Fixe le clipping : il ne faut pas ecraser l'indicateur de connexion
     * (C/F)
     */
    pv->r_clipping_normal[0].width = pv->largeur_ecran - 2 * pv->largeur_fonte_base;
    pv->r_clipping_normal[0].height = pv->hauteur_fonte_base;
    pv->r_clipping_normal[0].x = 0;
    pv->r_clipping_normal[0].y = 0;
    pv->r_clipping_normal[1].width = pv->largeur_fonte_base;
    pv->r_clipping_normal[1].height = pv->hauteur_fonte_base;
    pv->r_clipping_normal[1].x = pv->largeur_ecran - pv->largeur_fonte_base;
    pv->r_clipping_normal[1].y = 0;
    pv->r_clipping_normal[2].width = pv->largeur_ecran;
    pv->r_clipping_normal[2].height = pv->hauteur_ecran - pv->hauteur_fonte_base;
    pv->r_clipping_normal[2].x = 0;
    pv->r_clipping_normal[2].y = pv->hauteur_fonte_base;
    XSetClipRectangles(XtDisplay(w), pv->gc, 0, 0, pv->r_clipping_normal, 3, Unsorted);
    XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w), (unsigned long) 1000, (XtTimerCallbackProc)clignote, (XtPointer)w);
    XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w), (unsigned long) 500, (XtTimerCallbackProc)clignote_curseur, (XtPointer)w);

    alloc_DRCS (w);
    videotexInit (gnew);

    if (w->core.width == 0)
	w->core.width = pv->largeur_ecran;

    if (w->core.height == 0)
	w->core.height = pv->hauteur_ecran;

    w->core.background_pixel = BlackPixel(XtDisplay(w), DefaultScreen(XtDisplay(w)));

    XFlush(XtDisplay(w));
}

static void 
Realize(gw, valueMask, attrs)
Widget          gw;
XtValueMask    *valueMask;
XSetWindowAttributes *attrs;
{
    XtCreateWindow(gw, (unsigned) InputOutput, (Visual *) CopyFromParent,
		   *valueMask, attrs);
}

static void libere (p)
struct position *p;
{
    register struct position *ps;

    while (p) {
	ps = p->suivant;
	free (p);
	p = ps;
    }
}

static void 
Destroy(w)
Widget w;
{
    VideotexWidget vw = (VideotexWidget)w;
    register VideotexPart *pv = &vw->videotex;
    register int i;

    XFreeGC (XtDisplay(w), pv->gc);
    XtReleaseGC(w, pv->gc_curseur);
    XtReleaseGC(w, pv->gc_indicateur);
    XtReleaseGC(w, pv->gc_pixmap);
    XFreePixmap(XtDisplay(w), pv->ecran_masquage);
    XFreePixmap(XtDisplay(w), pv->ecran_demasquage);
    XFreePixmap(XtDisplay(w), pv->ecran_sauve);

    free (pv->bits_bitmap);
    for (i = 0 ; i != 94 ; i++) {
	free (pv->jeu_GP1[i]);
	free (pv->jeu_GP10[i]);
	libere (pv->affichage_GP1[i]);
	libere (pv->affichage_GP10[i]);
    }
}

static void 
Redisplay(w, pevent, region)
Widget          w;
XEvent         *pevent;
Region          region;
{
    VideotexWidget  vw = (VideotexWidget) w;
    register VideotexPart *pv = &vw->videotex;

    if (pv->flag_bs) {
	XCopyArea(XtDisplay(w), pv->ecran_sauve, XtWindow(w), pv->gc, 0, 0, pv->largeur_ecran, pv->hauteur_ecran, 0, 0);
    } else {
	XCopyArea(XtDisplay(w), pv->ecran_sauve, XtWindow(w), pv->gc, pevent->xexpose.x, pevent->xexpose.y, pevent->xexpose.width, pevent->xexpose.height, pevent->xexpose.x, pevent->xexpose.y);
    }

    XDrawImageString(XtDisplay(w), XtWindow(w), pv->gc_indicateur, pv->largeur_ecran - 2 * pv->largeur_fonte_base, pv->ascent_fonte_base, (vw->videotex.connecte ? "C" : "F"), 1);
}

static void 
Resize(gw)
Widget gw;
{}

/* ARGSUSED */
static Boolean 
SetValues(current, request, new, args, num_args)
Widget          current, request, new;
ArgList         args;
Cardinal       *num_args;
{
    register int i;
    Boolean redisplay = False;
    VideotexWidget cvw = (VideotexWidget)current;
    VideotexWidget nvw = (VideotexWidget)new;

    for (i = 0 ; i < *num_args ; i++) {
	if (strcmp (args[i].name, XtNconnecte) == 0) {
	    /* Demande de deconnexion 
	    if (cvw->videotex.connecte && !nvw->videotex.connecte && cvw->videotex.commande_deconnexion && cvw->videotex.fd_connexion > 0)
		write(cvw->videotex.fd_connexion, cvw->videotex.commande_deconnexion, strlen(cvw->videotex.commande_deconnexion));*/
	
	    /* Modification de l'etat connecte/deconnecte */
	    if (cvw->videotex.connecte != nvw->videotex.connecte)
		affiche_indicateur_connexion (nvw);

	    redisplay = True;
	}
	else if (strcmp (args[i].name, XtNfdConnexion) || strcmp (args[i].name, XtNfdConnexion)) {
	    redisplay = True;
	}
    }

    return (redisplay);
}

/*
 * Fonctions publiques
 */

/*
 * fonction d'init
 */
void 
videotexInit (w)
Widget w;
{
    VideotexWidget vw = (VideotexWidget) w;
    register VideotexPart *pv = &vw->videotex;
    register int    i;

    pv->rangee = 1;
    pv->colonne = 1;
    X_COURANT
    Y_COURANT
    pv->largeur_caractere = pv->hauteur_caractere = 1;
    pv->derniere_rangee_avant_0 = 1;
    pv->derniere_colonne_avant_0 = 1;
    pv->indice_couleur_caractere_precedent = pv->indice_couleur_fond_precedent = -1;

    /* charge les attributs par defaut */
    attributs_caractere_par_defaut(vw);
    attributs_serie_par_defaut(vw);

    /*
     * On re-initialise le tableau des attributs par rangee
     */
    for (i = 0; i != 25; i++)
	memcpy(pv->attributs[i], attributs_initiaux, 40 * sizeof(struct attribut));

    init_DRCS (vw);
#ifndef NO_TVR
    init_tvr (vw);
#endif
}

/*
 * Fonction de decodage
 */
void
videotexDecode(w, c)
    Widget          w;
    char            c;
{
    register int    i;
    int             nr, nc;
    VideotexWidget  vw = (VideotexWidget) w;
    register VideotexPart *pv = &vw->videotex;
    Boolean         fin;

    do {
	fin = True;

	if (c == NUL)
	    return;

	if (pv->nb_a_ignorer) {
	    pv->nb_a_ignorer--;
	    return;
	}

	pv->flag_sauvegarde = True;

	/*
	 * Maintenant on traite le caractere recu
	 */
	switch (pv->etat) {
	    /*
	     * Pas de caracteres de controle en cours...
	     */
	case EtatNormal:

	    if (c < ' ') {
		/*
		 * Caractere de controle
		 */
		switch (c) {

		    /*
		     * Positionnement:  - separateur de sous articles -
		     * initialisation/interruption d'un chargement de fonte
		     * DRCS
		     */
		case US:

		    init_US(vw);
		    break;

		case ESC:	/* echappement */
		    pv->etat = EtatESC;
		    /* a priori, on attend un seul caractere... */
		    pv->a_recevoir = 1;
		    break;

		case SO:	/* hors code : selection G1 */
		    /*
		     * Le passage hors-code invalide le soulignement la
		     * couleur de fond devient la derniere couleur de fond
		     * validee par un delimiteur, le masquage est conserve.
		     * 
		     * Les attributs de taille et d'inversion ne sont pas
		     * utilisable en mode semi-graphiques, et seront annules
		     * lors du retour en code => dans notre cas, on les
		     * annule des maintenant
		     */
		    (*pv->fonction_affichage_curseur) (w, 0);

		    soulignement(vw, FIN_LIGNAGE);
		    couleur_fond(vw, pv->valeur_couleur_fond);
		    pv->valeur_lignage = FIN_LIGNAGE;
		    pv->a_valider_si_delimiteur &= ~(BIT_LIGNAGE | BIT_COULEUR_FOND);
		    inversion(vw, POSITIF);
		    taille_caractere(vw, NORMALE);
		    /* on plus de caractere de G2 a repeter */
		    pv->flag_G2 = False;
		    /* selectionne la fonte G1 */
		    selection_jeu(vw, G1);
		    calcul_flag_drcs((VideotexWidget)w);

		    /*
		     * Si on vient de remettre a zero les attributs serie
		     * (par US), le prochain caractere de G1 sera un
		     * delimiteur
		     */
		    (*pv->fonction_affichage_curseur) (w, 0);
		    break;

		case SI:	/* en code : selection G0 */
		    /*
		     * Le retour en code invalide l'inversion et fixe la
		     * taille normale Invalide egalement le lignage.
		     */
		    pv->valeur_lignage = FIN_LIGNAGE;
		    pv->a_valider_si_delimiteur &= ~BIT_LIGNAGE;
		    soulignement(vw, FIN_LIGNAGE);
		    inversion(vw, POSITIF);
		    taille_caractere(vw, NORMALE);
		    selection_jeu(vw, G0);
		    calcul_flag_drcs(vw);
		    selection_attributs_zone_accueil(vw);
		    (*pv->fonction_affichage_curseur) (w, 0);
		    break;

		case REP:	/* repetition du dernier caractere imprimable */
		    pv->etat = EtatREP;
		    (*pv->fonction_affichage_curseur) (w, 0);
		    break;

		case SYN:	/* semble etre plus souvent utilise que
				 * SS2... */
		case SS2:	/* appel G2 */
		    /*
		     * on utilise G2 seulement a partir de G0 sauf en mode
		     * DRCS
		     */
		    if (pv->jeu_courant == G0 || pv->flag_drcs) {
			if (!pv->flag_drcs) {
			    /* Le caractere a repeter fait partie de G2 */
			    pv->flag_G2 = True;
			    selection_jeu(vw, G2);
			}
			(*pv->fonction_affichage_curseur) (w, 0);
			pv->etat = EtatSS2;
			pv->a_recevoir = 2;
		    }
		    break;


		case CR:	/* retour chariot */
		    (*pv->fonction_affichage_curseur) (w, 0);
		    pv->colonne = 1;
		    selection_attributs_zone_accueil(vw);
		    X_COURANT;
		    (*pv->fonction_affichage_curseur) (w, 0);
		    break;

		    /*
		     * RS et FF sont les separateurs explicites d'articles
		     */
		case RS:	/* retour curseur en 1, 1 */
		case FF:	/* idem + effacement d'ecran */
		    (*pv->fonction_affichage_curseur) (w, 0);

		    pv->rangee = pv->derniere_rangee_avant_0 = 1;
		    pv->colonne = pv->derniere_colonne_avant_0 = 1;
		    pv->largeur_caractere = pv->hauteur_caractere = 1;

		    X_COURANT;
		    Y_COURANT;

		    (*pv->fonction_affichage_curseur) (w, 0);

		    pv->a_valider_si_delimiteur = 0;
		    pv->valeur_couleur_fond = FOND_NOIR;
		    pv->valeur_lignage = FIN_LIGNAGE;
		    pv->valeur_masquage = DEMASQUAGE;

		    attributs_caractere_par_defaut(vw);
		    couleur_fond(vw, FOND_NOIR);
		    soulignement(vw, FIN_LIGNAGE);
		    masquage(vw, DEMASQUAGE);

		    /* Si FF, efface l'ecran */
		    if (c == FF) {
			efface_rectangle_ecran (vw, 1, 1, 24, 40);
#ifndef NO_TVR
			libere_couleurs_tvr (vw);
#endif
		    }


		    pv->flag_GP1_dans_G0 = pv->flag_GP10_dans_G1 = pv->flag_drcs = False;
		    pv->flag_insertion = False; /* A verifier */

		    break;

		    /*
		     * Codes de positionnement curseur : LF, HT, VT, BS
		     */
		case LF:	/* interligne */
		    (*pv->fonction_affichage_curseur) (w, 0);
		    rangee_suivante(vw);
		    (*pv->fonction_affichage_curseur) (w, 0);
		    break;

		case HT:	/* tabulation horizontale */
		    (*pv->fonction_affichage_curseur) (w, 0);
		    colonne_suivante(vw);
		    (*pv->fonction_affichage_curseur) (w, 0);
		    break;

		case VT:	/* tabulation verticale */
		    if (pv->rangee != 0) {
			(*pv->fonction_affichage_curseur) (w, 0);
			rangee_precedente(vw);
			(*pv->fonction_affichage_curseur) (w, 0);
		    }
		    break;

		case BS:	/* retour arriere */
		    (*pv->fonction_affichage_curseur) (w, 0);
		    colonne_precedente(vw);
		    (*pv->fonction_affichage_curseur) (w, 0);
		    break;

		case SEP:	/* separateur de code */

		    pv->etat = EtatSEP;
		    break;

		case CAN:	/* annulation de la ligne */
		    remplissage_ligne(vw);
		    break;

		case DC1:	/* XON : curseur actif */
		    if (pv->fonction_affichage_curseur == (void (*) ()) nulle) {
			curseur_actif(vw);
			(*pv->fonction_affichage_curseur) (w, 0);
		    }
		    break;

		case DC4:	/* XOFF : curseur inactif */
		    if (pv->fonction_affichage_curseur != (void (*) ()) nulle) {
			(*pv->fonction_affichage_curseur) (w, 0);
			curseur_inactif(vw);
		    }
		    break;


		case BEL:
		    sonnerie(vw);
		    break;

		default:
		    break;

		}
	    } else {
		/*
		 * Caractere Imprimable
		 */
		if (pv->rangee <= 1 && pv->taille_caractere != NORMALE) {
		    if (pv->taille_caractere == DOUBLE_HAUTEUR)
			taille_caractere(vw, NORMALE);
		    else if (pv->taille_caractere == DOUBLE_GRANDEUR)
			taille_caractere(vw, DOUBLE_LARGEUR);
		}
		/* le caractere a repeter ne sera plus de la fonte G2 */
		pv->flag_G2 = False;
		pv->caractere_sauve = c;

		/*
		 * On teste si on ecrase un double taille (double largeur,
		 * double hauteur). si oui, on transforme les autres
		 * caracteres de ce double taille en espaces de la couleur
		 * d'encre courante, non inverses
		 */
#ifdef NOTDEF
		p = &pv->attributs[pv->rangee][pv->colonne - 1];
		if (p->taille_caractere != NORMALE) {
		}
#endif

		/*
		 * Le blanc (delimiteur explicite) permet de valider les
		 * attributs serie :
		 * 
		 * - couleur de fond - lignage - masquage
		 * 
		 */
		if (c == ' ' && pv->a_valider_si_delimiteur) {
		    (*pv->fonction_affichage_curseur) (w, 0);
		    traitement_delimiteur_explicite(vw, c);
		    (*pv->fonction_affichage_curseur) (w, 0);
		} else if (pv->jeu_courant == G1) {
		    (*pv->fonction_affichage_curseur) (w, 0);
		    traitement_delimiteur_graphique(vw);
		    affiche_caractere_normal(vw, c);
		    /* colonne est au caractere suivant */
		    i = fixe_les_attributs_d_un_bloc(vw, pv->rangee, pv->colonne, 1);	
		    if (i >= pv->colonne)
			reaffiche_rectangle_ecran(vw, pv->rangee, pv->colonne, pv->rangee, i);
		    (*pv->fonction_affichage_curseur) (w, 0);
		} else {
		    (*pv->fonction_affichage_curseur) (w, 0);
		    selection_attributs_zone_accueil(vw);
		    sauve_les_attributs_du_caractere(vw, c, 0);
		    i = fixe_les_attributs_d_un_bloc(vw, pv->rangee, pv->colonne + 1, 1);
		    reaffiche_rectangle_ecran(vw, pv->rangee, pv->colonne, pv->rangee, i);
		    if (pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR) {
			i = fixe_les_attributs_d_un_bloc(vw, pv->rangee - 1, pv->colonne + 1, 1);
			reaffiche_rectangle_ecran(vw, pv->rangee - 1, pv->colonne, pv->rangee - 1, i);
		    }
		    caractere_suivant(vw);
		    (*pv->fonction_affichage_curseur) (w, 0);
		}
	    }
	    break;

	    /*
	     * Positionnement en cours ou init d'un chargement de fonte DRCS
	     */

	case EtatUS:

	    /* Reception ligne */
	    if (pv->a_recevoir == 2) {
		pv->erreur_US = 0;
		if (c >= 64 && c <= (64 + 24)) {	/* positionnement absolu */
		    if (c == 64 && pv->rangee != 0) {
			/* on va en pv->rangee 0 ==> sauve */
			sauve_avant_rangee_0(vw);
		    }
		    pv->rangee = c - 64;
		} else if (c >= '0' && c <= '9') {	/* positionnement
							 * relatif */
		    if (c == '0' && pv->rangee != 0) {
			/* on va en rangee 0 ==> sauve */
			sauve_avant_rangee_0(vw);
		    }
		    pv->rangee = c - '0';

		    /*
		     * En-tete chargement DRCS ou chargement de formes DRCS
		     */
		} else if (c == 0x23) {	/* # ==> detection de l'en-tete */
		    pv->etat = EtatDetectionEnTeteDRCS;
		} else {
		    pv->rangee = c - 64;
		    pv->erreur_US = 1;
		}
		pv->a_recevoir--;
	    } else {		/* reception de la colonne */
		/* en cas d'erreur ==> filtre */
		if (c > 64 && c <= (64 + 40))	/* positionnement absolu */
		    pv->colonne = c - 64;
		else if (c >= '0' && c <= '9') {	/* positionnement
							 * relatif */
		    pv->rangee = pv->rangee * 10 + c - '0';
		    pv->colonne = 1;
		} else
		    pv->erreur_US = 2;

		/* en cas d'erreur on n'accepte pas le positionnement */
		if (pv->erreur_US) {
		    pv->rangee = pv->rangee_sauve;
		    pv->colonne = pv->colonne_sauve;
		    pv->etat = EtatNormal;
		    break;
		}
		/*
		 * on n'enleve le curseur que maintenant pour eviter les
		 * problemes
		 */
		nr = pv->rangee;
		nc = pv->colonne;
		pv->rangee = pv->rangee_sauve;
		pv->colonne = pv->colonne_sauve;
		(*pv->fonction_affichage_curseur) (w, 0);
		pv->rangee = nr;
		pv->colonne = nc;

		X_COURANT;
		Y_COURANT;

		/*
		 * US initialise les attributs par caractere
		 */
		attributs_caractere_par_defaut(vw);
		pv->a_valider_si_delimiteur = 0;
		pv->valeur_couleur_fond = FOND_NOIR;
		pv->valeur_lignage = FIN_LIGNAGE;
		pv->valeur_masquage = DEMASQUAGE;
		selection_attributs_zone_accueil(vw);

		(*pv->fonction_affichage_curseur) (w, 0);
		pv->etat = EtatNormal;
	    }

	    break;


	case EtatESC:		/* ESC en cours */

	    if (pv->filtrage) {
		pv->a_recevoir--;
	    } else if (pv->plein_ecran) {
		if (pv->a_recevoir == 1) {
		    if (c == 0x58) {	/* masquage plein ecran (etat initial
					 * du MINITEL) */
			pv->masquage_plein_ecran = True;

			/* masquer le plein ecran */
			masquer_le_plein_ecran(vw);
		    } else if (c == 0x5f) {
			/* demasquage plein ecran */
			pv->masquage_plein_ecran = False;

			/* Demasquer ... */
			demasquer_le_plein_ecran(vw);
		    }
		    pv->plein_ecran = 0;
		}
		pv->a_recevoir--;
	    } else if (pv->pro1) {
		switch (c) {

		case 0x7b:	/* type Minitel: ENQROM */

		    /*
		     * Type Minitel emule (Minitel 2 ou Minitel TVR)
		     */
		    if (vw->videotex.connecte && vw->videotex.fd_connexion > 0 && vw->videotex.enqrom) {
			write(vw->videotex.fd_connexion, "\001", 1);
			write(vw->videotex.fd_connexion, vw->videotex.enqrom, strlen(vw->videotex.enqrom));
			write(vw->videotex.fd_connexion, "\004", 1);
		    }

		    break;

		case 0x67:	/* deconnexion MODEM */

		    if (vw->videotex.connecte && vw->videotex.commande_deconnexion)
			write(vw->videotex.fd_connexion, vw->videotex.commande_deconnexion, strlen(vw->videotex.commande_deconnexion));

		default:
		    break;
		}

		pv->pro1 = False;
		pv->a_recevoir--;
	    } else if (pv->pro2) {
		if (pv->a_recevoir == 2) {
		    if (c == 0x69) {	/* validation de fonction ecran :
					 * START */
			pv->valeur_fonction = 1;
		    } else if (c == 0x6a) {	/* invalidation de fonction
						 * ecran : STOP */
			pv->valeur_fonction = 0;
		    } else if (c == '1')
			pv->passage_teleinfo = 1;
		    else;	/* les autres fonctions sont filtrees... */
		} else {
		    /* pv->a_recevoir = 1 , a modifier */
		    if (c == 0x43) {
			/* mode rouleau */
			pv->mode_scroll = pv->valeur_fonction;
		    } else if (c == 0x45) {
			/* saisie_en_minuscule=valeur; */
		    } else if (c == '}' && pv->passage_teleinfo) {
			XtCallCallbackList(w, pv->callback_mode, NULL);
		    }
		    else
			;	/* les autres fonctions sont filtrees... */

		    pv->pro2 = pv->passage_teleinfo = False;
		}
		pv->a_recevoir--;


	    } else if (pv->chargement_set) {
		if (c != ' ') {
		    if (c == '@') {
			pv->flag_GP1_dans_G0 = False;
		    } else if (c == 'c') {
			pv->flag_GP10_dans_G1 = False;
		    } else if (c == 'B') {
			pv->flag_GP1_dans_G0 = True;
		    } else if (c == 'C') {
			pv->flag_GP10_dans_G1 = True;
		    }
		    calcul_flag_drcs(vw);

#ifdef DEBUG_DRCS
		    printf("G'1 dans G0 = %d G'10 dans G1 = %d, flag_drcs = %d\n", pv->flag_GP1_dans_G0, pv->flag_GP10_dans_G1, pv->flag_drcs);
#endif
		    pv->a_recevoir = 0;
		    pv->chargement_set = False;
		}
	    } else if (pv->a_recevoir == 1) {
		/* filtrage des codes non significatifs */
		if ((c > 0x34 && c < 0x3A && c != 0x39) || c == 0x25 || c == 0x2F || c == 0x22	/* || c == 0x29 || c == 0x28 pour DRCS */ ) {
		    pv->a_recevoir = 1;
		    pv->filtrage = True;
		    break;
		} else if (c == 0x39) {	/* PRO1 */
		    pv->a_recevoir = 1;
		    pv->pro1 = True;
		    break;
		} else if (c == 0x20 /* || c == 0x5b */ ) {
		    pv->a_recevoir = 2;
		    pv->filtrage = True;
		    break;
		} else if (c == 0x5b) {	/* CSI */
		    pv->a_recevoir = 1;
		    pv->etat = EtatCSI;
		    break;
		} else if (c == 0x21 || c == 0x3b) {
		    pv->a_recevoir = 3;
		    pv->filtrage = True;
		    break;
		} else if (c == 0x23) {
		    pv->plein_ecran = True;
		    pv->a_recevoir = 2;
		    break;
		} else if (c == 0x3a) {	/* PRO2 */
		    pv->pro2 = 1;
		    pv->a_recevoir = 2;
		    break;
		} else if (c == '(' || c == ')') {
		    pv->a_recevoir = 1;
		    pv->chargement_set = True;
		    break;
		}
#ifndef NO_TVR
		else if (c == 'p')  {
		    int type_header;

		    decode_header_tvr (vw, &type_header);
		    if (type_header) 
			decode_jpeg_tvr (vw);

		    pv->filtrage = False;
		    pv->etat = EtatNormal;
		    
		    break;
		}
#endif /* NO_TVR */

		/*
		 * Attributs
		 */
		if (c >= CARACTERE_NOIR && c <= CARACTERE_BLANC)
		    couleur_caractere(vw, c);
		else if (c >= FOND_NOIR && c <= FOND_BLANC) {
		    pv->a_valider_si_delimiteur |= BIT_COULEUR_FOND;
		    pv->valeur_couleur_fond = c;
		} else if (c >= NORMALE && c <= DOUBLE_GRANDEUR && pv->jeu_courant != G1)
		    taille_caractere(vw, c);
		else if (c == DEBUT_LIGNAGE || c == FIN_LIGNAGE) {
		    pv->valeur_lignage = c;
		    if (pv->jeu_courant == G1) 
			soulignement(vw, c);
		    else 
			pv->a_valider_si_delimiteur |= BIT_LIGNAGE;
		} else if ((c == POSITIF || c == NEGATIF) && pv->jeu_courant != G1)
		    inversion(vw, c);
		else if (c == MASQUAGE || c == DEMASQUAGE) {
		    pv->valeur_masquage = c;
		    pv->a_valider_si_delimiteur |= BIT_MASQUAGE;
		} else if (c == LETTRES_FIXES || c == LETTRES_CLIGNOTANTES)
		    clignotement(vw, c);

		/* Incrustation, fond transparent non traites */

		pv->a_recevoir--;
	    }
	    /* test de fin de reception */
	    if (pv->a_recevoir == 0) {
		/* on repositionne l'automate en mode normal */
		pv->filtrage = False;
		pv->etat = EtatNormal;
	    }
	    break;

	case EtatSS2:		/* appel a G2 */
	    if (pv->a_recevoir == 2) {
		/* premier caractere */
		pv->accent_sauve = c;

		/* signes de G2 */
		if ((c < 65 || c > 67) && (c != 72) && (c != 75)) {
		    Boolean         inhibe = (pv->flag_drcs && pv->jeu_courant == G0);
		    char            jeu = pv->jeu_courant == G0;
		    /*
		     * Si on est en DRCS et G0, on force l'affichage du
		     * caractere de G2 en inhibant flag_drcs
		     */
		    if (inhibe) {
			pv->flag_drcs = False;
			/* On selectionne la fonte G2 */
			selection_jeu(vw, G2);
		    }
		    affiche_caractere_normal(vw, g2_vers_g0(c));

		    /* On remet le flag */
		    if (inhibe) {
			pv->flag_drcs = True;
			selection_jeu(vw, jeu);
		    }
		    (*pv->fonction_affichage_curseur) (w, 0);
		    pv->a_recevoir = 0;
		    pv->etat = EtatNormal;
		} else {
		    pv->a_recevoir--;
		}
		selection_jeu(vw, G0);
	    } else {
		/*
		 * Cas du DRCS
		 * 
		 * si G1 ==> affiche accent + X si G0 ==> affiche seulement X
		 */
		if (pv->flag_drcs) {
		    if (pv->jeu_courant == G1)
			affiche_caractere_normal(vw, g2_vers_g0 (pv->accent_sauve));
		    affiche_caractere_normal(vw, c);
		} else {
		    /*
		     * On affiche en meme temps l'accent et le caractere,
		     * cela permet de resoudre les problemes de curseur
		     * clignotant
		     */
		    pv->caractere_sauve = c;
		    selection_attributs_zone_accueil(vw);
		    sauve_les_attributs_du_caractere(vw, pv->accent_sauve, c);
		    selection_jeu(vw, G2);
		    (*pv->affiche_caractere_avec_fond) (vw, g2_vers_g0(pv->accent_sauve));
		    selection_jeu(vw, G0);
		    (*pv->affiche_caractere_sans_fond) (vw, c);
		    i = fixe_les_attributs_d_un_bloc(vw, pv->rangee, pv->colonne + 1, 1);
		    reaffiche_rectangle_ecran(vw, pv->rangee, pv->colonne, pv->rangee, i);
		    if (pv->taille_caractere == DOUBLE_HAUTEUR || pv->taille_caractere == DOUBLE_GRANDEUR) {
			i = fixe_les_attributs_d_un_bloc(vw, pv->rangee - 1, pv->colonne + 1, 1);
			reaffiche_rectangle_ecran(vw, pv->rangee - 1, pv->colonne, pv->rangee - 1, i);
		    }
		    caractere_suivant(vw);
		}
		(*pv->fonction_affichage_curseur) (w, 0);
		pv->etat = EtatNormal;
	    }

	    break;

	case EtatREP:		/* Repetition en cours */

	    if (c > 64) {
		/* Si on est en rangee 0 */
		if (pv->rangee == 0)
		    repetition_caractere_sauve(vw, ((c - 64 > 39 - pv->colonne) ? 39 - pv->colonne : c - 64));
		else
		    repetition_caractere_sauve(vw, c - 64);
	    }
	    (*pv->fonction_affichage_curseur) (w, 0);
	    pv->etat = EtatNormal;	/* retour en mode normal */
	    break;


	case EtatSEP:

	    pv->etat = EtatNormal;
	    if (c < ' ')
		fin = False;

	    break;


	    /*
	     * En cours de sequence CSI
	     */
	case EtatCSI:

	    pv->buffer_csi[pv->nb_csi] = c;
	    pv->nb_csi++;

	    /* Fin de la sequence sur une lettre */
	    if (isalpha(c)) {
		pv->etat = EtatNormal;
		/* CSI non disponible en rangee 0 */
		if (pv->rangee > 0)
		    traite_csi (vw);
		else { /* filtre la sequence */
		   pv->etat = EtatNormal;
		   pv->nb_csi = 0;
	       }
	    }
	    /*
	     * Caractere de controle ==> on abandonne et on continue le 
	     * decodage en etat normal
	     */
	    else if (c < ' ') {
		pv->etat = EtatNormal;
		pv->nb_csi = 0;
		fin = False;
	    }
	    
	    break;

	    /*
	     * Chargement d'une fonte DRCS
	     */

	case EtatDetectionEnTeteDRCS:

	    /* Un blanc detecte ==> traitement de l'en-tete DRCS */
	    if (c == ' ') {
		pv->etat = EtatChargementEnTeteDRCS;
		pv->filtrage = False;
		pv->a_recevoir = 4;
	    }
	    /* si != ' ', l'automate passe en mode d'attente de B1 */
	    else {
		pv->etat = EtatAttenteB1;
		pv->code_DRCS_courant = c - '!';
		pv->jeu_DRCS_courant = (pv->numero_jeu_DRCS_a_charger ? pv->jeu_GP10 : pv->jeu_GP1);
	    }

	    break;


	case EtatChargementEnTeteDRCS:

	    /*
	     * L'en-tete est :
	     * 
	     * US 23 20 20 20 42 49	pour G'1 43    	pour G'10
	     * 
	     * Une en-tete non reconnue est filtree
	     */
	    if (!pv->filtrage) {
		if (pv->a_recevoir == 1) {
		    if (c == 0x49) {
			pv->numero_jeu_DRCS_a_charger = pv->numero_jeu_DRCS_prevu;
#ifdef DEBUG_DRCS
			printf("jeu a charger = %d\n", pv->numero_jeu_DRCS_a_charger);
#endif
		    }
		    pv->etat = EtatNormal;
		    break;
		} else if (pv->a_recevoir == 2) {
		    if (c == 0x42 || c == 0x43)
			pv->numero_jeu_DRCS_prevu = (c == 0x42 ? 0 : 1);
		    else
			pv->filtrage = True;
		} else {
		    if (c != ' ')
			pv->filtrage = True;
		}
	    }
	    pv->a_recevoir--;

	    /* si filtage */
	    if (pv->a_recevoir == 0) {
		pv->etat = EtatNormal;
		pv->filtrage = False;
	    }
	    break;


	case EtatChargementFonteDRCS:

	    /* US ==> interruption du chargement */
	    if (c == US) {
		/* Re-affiche */
		re_affiche_DRCS(vw);

		init_US(vw);
	    }
	    /* Lecture des pattern */
	    else if (pv->a_recevoir > 0) {

		switch (c) {

		case B1:	/* Si B1, interruption du chargement */
#ifdef DEBUG_CHRG_DRCS
		    printf("B1 d'interruption !!!\n");
#endif
		    /* complete par des vides */
		    for (i = 14 - pv->a_recevoir; i != 14; i++)
			pv->jeu_DRCS_courant[pv->code_DRCS_courant][i] = 0;
#ifdef DEBUG_CHRG_DRCS
		    printf("%x (%c): %d slices completees par des vides !\n", pv->code_DRCS_courant, pv->code_DRCS_courant + ' ', pv->a_recevoir);
#endif
		    /* Re-affiche */
		    re_affiche_DRCS(vw);

		    /* A priori, charge le suivant */
		    pv->code_DRCS_courant++;
		    pv->a_recevoir = 14;
		    return;

		default:

		    /* Charge le caractere */
		    pv->jeu_DRCS_courant[pv->code_DRCS_courant][14 - pv->a_recevoir] = c & 0x3f;
#ifdef DEBUG_CHRG_DRCS
		    printf("charge: %x (%c) code[%d] = %x %c\n", pv->code_DRCS_courant, pv->code_DRCS_courant + '!', 14 - pv->a_recevoir, pv->jeu_DRCS_courant[pv->code_DRCS_courant][14 - pv->a_recevoir], c);
#endif
		    break;
		}

		if (--pv->a_recevoir == 0)
		    re_affiche_DRCS(vw);
	    }
	    /* + que 14, on filtre en attendant B1 */
	    else {
		if (c == B1) {
		    pv->code_DRCS_courant++;
		    pv->a_recevoir = 14;
		} else {
		    pv->etat = EtatAttenteB1;
		}
	    }

	    break;

	    /*
	     * Attente du caractere B1 (0x30) de debut de chargement
	     */

	case EtatAttenteB1:

	    if (c == B1) {
#ifdef DEBUG_CHRG_DRCS
		printf("B1 recu !\n");
#endif
		pv->etat = EtatChargementFonteDRCS;
		pv->a_recevoir = 14;
	    } else if (c == US) {
		/* Re-affiche */
		re_affiche_DRCS(vw);

		init_US(vw);	/* Interruption du telechargement */
	    }
	    break;



	default:		/* erreur */

	    break;
	}
    } while (!fin);
}


/* Retourne le Pixmap de sauvegarde */
Pixmap videotexPixmapEcranSauve (w)
Widget w;
{
    VideotexWidget vw = (VideotexWidget)w;
    return (vw->videotex.ecran_sauve);
}

/* Sauve l'ecran */
void videotexSauveEcran (w)
Widget          w;
{
    VideotexWidget vw = (VideotexWidget)w;
    register VideotexPart *pv = &vw->videotex;

    if (pv->flag_bs) {
	if (pv->flag_sauvegarde) {
	    XCopyArea(XtDisplay(w), XtWindow(w), pv->ecran_sauve, pv->gc, 0, 0, pv->largeur_ecran, pv->hauteur_ecran, 0, 0);
	    XFlush(XtDisplay(w));
	    pv->flag_sauvegarde = False;
	}
    }
}


/*
 * Affiche une approximation ASCII de la page courante sur la sortie standard
 * (stdout).
 */
void
videotexConversionAscii (w, fp)
Widget  w;
FILE *fp;
{
    register int    l, c;
    VideotexWidget vw = (VideotexWidget)w;
    register VideotexPart *pv = &vw->videotex;

    for (l = 1; l != 25; l++) {
	for (c = 0; c != 40; c++) {
	    if (pv->attributs[l][c].code[0] > ' ') {
		if (pv->attributs[l][c].jeu == G0)
		    putc(pv->attributs[l][c].code[0], fp);
		else if (pv->attributs[l][c].jeu == G2)
		    putc(pv->attributs[l][c].code[1], fp);
		else
		    putc(' ', fp);
	    } else
		putc(' ', fp);
	}
	putc('\n', fp);
    }
}
