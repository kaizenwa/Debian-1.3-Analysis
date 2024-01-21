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
#ident "$Id: global.h,v 1.8 1996/10/07 10:39:19 pierre Exp $"

#ifndef _global_h
#define _global_h

#ifndef EXTERN
#define EXTERN extern
#endif 

/* 
 * Fonctions 
 */

#ifdef __STDC__

/* xtel.c */
void init_xtel(void);
void sauve_l_ecran(Widget w, XEvent *pevent, String  *params, Cardinal nb_params);
int main(int argc, char **argv);
void Usage (void);
void create_teleph_cursor (Widget);
void affiche_telephone (void);
void efface_telephone (void);

/* ligne.c */
void ce_n_est_qu_un_au_revoir(void);
void fonction_lecture_ligne(XtPointer client_data, int *fid, XtInputId *id);
void connexion_service(char *service);
void enregistre_caractere (Widget w, XtPointer client_data, char *s);

/* bouton.c */
void evenement_entre_widget(Widget w, char *client_data, XEvent *pevent);
void commandes(Widget w, char *code, XtPointer call_data);
char *xtel_basename(char *s);
void Charge(void);
void Sauve(void);
void composition_valide(int flag);
void clavier_minitel_valide(int flag);
void mise_a_jour_mode_emulation(int flag_invalide);
void raccrocher (Widget w, char *code, XtPointer call_data);
void demarre_enregistrement (Widget w, XtPointer client_data, XtPointer call_data);
void arrete_enregistrement (Widget w, XtPointer client_data, XtPointer call_data);
void nouvel_enregistrement (Widget w, XtPointer client_data, XtPointer call_data);
void change_enregistrement (Widget w, XtPointer client_data, XtPointer call_data);

/* inet.c */
int c_clientbyname(char *host, char *name);
int c_clientbyport(char *host, int port);
/* imprime.c */
void imprime_page_courante_ascii(Widget w, XtPointer client_data, XtPointer call_data);
void imprime_page_courante_videotex(Widget w, XtPointer client_data, XtPointer call_data);
/* lecteur.c */
void stop_rapide(void);
void fonction_lecture_fichier_courant(XtPointer client_data, int *fid, XtInputId *id);
void fin_lecteur(Widget w, XtPointer client_data, XtPointer call_data);
void Play_lecteur(Widget w, XtPointer client_data, XtPointer call_data);
void Stop_lecteur(Widget w, XtPointer client_data, XtPointer call_data);
void Zero_lecteur(Widget w, XtPointer client_data, XtPointer call_data);
void Rewind_lecteur(Widget w, XtPointer client_data, XtPointer call_data);
void Wind_lecteur(Widget w, XtPointer client_data, XtPointer call_data);
void raz_lecteur(void);
void debut_lecteur(void);

/* procedure.c */
int init_procedures(char *);
void start_procedure (int);
void start_procedure (int);
void run_procedure (char, int);
void display_procedures (void);
int get_proc_index (char *);

/* teleinfo.c */
void teleinfo(char *nom);
char conversion_teleinfo_fr(int car);
void fonction_lecture_teleinfo(XtPointer client_data, int *fid, XtInputId *id);
void zigouille_xterm_teleinfo(void);
void selection_mode_emulation(Widget w, char *mode, XtPointer call_data);
/* copyright.c */
int affiche_copyright(Widget w, WidgetClass classe_forme, Pixmap pixmap);

/* xm.c/xaw.c */
Widget init_toolkit(int *pargc, char **argv);
Widget init_widgets(Widget topLevel);
void affiche_erreur(char *s, int code_erreur);
void lecteur_valide(int flag);
void efface_lecteur(void);
void affiche_lecteur(void);
void affiche_compteur_lecteur_caractere(void);
void affiche_compteur_lecteur_page(void);
void activation_bouton_stop(void);

/* protocoles.c */
int init_protocoles (char *nom, struct protocole *proto);
void Selection_telechargement(Widget w, int protocole, XtPointer call_data);
void Stop_telechargement(Widget w, XtPointer client_data, XtPointer call_data);

/* misc.c */
char *next_token(char *, char *);
char *build_name(char *);

#else

void init_xtel();
void sauve_l_ecran();
int main();
void Usage();
void create_teleph_cursor();
void affiche_telephone();
void efface_telephone();

void ce_n_est_qu_un_au_revoir();
void fonction_lecture_ligne();
void connexion_service();
void enregistre_caractere ();

void evenement_entre_widget();
void commandes();
char *xtel_basename();
void Charge();
void Sauve();
void composition_valide();
void clavier_minitel_valide();
void mise_a_jour_mode_emulation();
void raccrocher();
void demarre_enregistrement ();
void arrete_enregistrement ();
void nouvel_enregistrement ();
void change_enregistrement ();

int c_clientbyname();
int c_clientbyport();
void imprime_page_courante_ascii();
void imprime_page_courante_videotex();

void stop_rapide();
void fonction_lecture_fichier_courant();
void fin_lecteur();
void Play_lecteur();
void Stop_lecteur();
void Zero_lecteur();
void Rewind_lecteur();
void Wind_lecteur();
void raz_lecteur();
void debut_lecteur();

int init_procedures();
void start_procedure();
void start_procedure();
void run_procedure();
void display_procedures();
int get_proc_index();

void teleinfo();
char conversion_teleinfo_fr();
void fonction_lecture_teleinfo();
void zigouille_xterm_teleinfo();
void selection_mode_emulation();

int affiche_copyright();

Widget init_toolkit();
Widget init_widgets();
void affiche_erreur();
void lecteur_valide();
void efface_lecteur();
void affiche_lecteur();
void affiche_compteur_lecteur_caractere();
void affiche_compteur_lecteur_page();
void activation_bouton_stop();

int init_protocoles ();
void Selection_telechargement();
void Stop_telechargement();

char *next_token();
char *build_name();

#endif /* __STDC__ */

/* 
 * Variables 
 */
#ifndef __FreeBSD__
extern char *sys_errlist[];
#endif

/* xtel.c */
#ifdef NO_NETWORK
EXTERN struct sockaddr_un unaddr;
#endif /* NO_NETWORK */
EXTERN Widget ecran_minitel;		/* Widget Videotex */
EXTERN unsigned int  cpt_buffer;	/* nb de caracteres courant */
EXTERN Boolean flag_enregistrement;
EXTERN Boolean flag_arret_a_chaque_page;
EXTERN char type_de_fin_page;
EXTERN char sortie_violente;		/* Quit en cours de connexion */
EXTERN int nb_procedures;
EXTERN struct procedure procedures[MAXPROC];
EXTERN struct chat *chat_courant;
EXTERN char *serveur_xtel;
EXTERN char *zone_enregistrement;
EXTERN unsigned int taille_zone_enregistrement;
EXTERN char *toolkit_utilise;
EXTERN struct definition_service definition_services[MAX_SERVICES];
EXTERN int socket_xteld;
EXTERN String entree_compose[MAX_SERVICES];
EXTERN int nb_services;
EXTERN char mode_emulation;
EXTERN struct definition_touche touches[];
EXTERN XtAppContext app_context;
EXTERN char nom_fichier_sauve[256];
EXTERN char nom_fichier_charge[256];
EXTERN char numero_courant[256];
EXTERN ressources_xtel rsc_xtel;
EXTERN int nb_protocoles;
EXTERN struct protocole protocoles[MAXPROTO];

/* bouton.c */
EXTERN long position_courante_fichier;
EXTERN unsigned int numero_page_courante;
EXTERN unsigned int numero_caractere_courant;

/* lecteur.c */
EXTERN char lecteur_play;

/* ligne.c */
EXTERN XtInputId input_id;

/* teleinfo.c */
EXTERN XtInputId input_id_teleinfo;
EXTERN int fd_teleinfo;
EXTERN int pid_teleinfo;

/* xm.c/xaw.c */
EXTERN char *toolkit_utilise;
EXTERN Widget bouton_compose;
EXTERN Widget bouton_procedure;
EXTERN Widget menu_clavier;
EXTERN Widget boite_bas;
EXTERN Widget btn_direct;
EXTERN Widget timer_minitel;
EXTERN WidgetClass classe_forme_copyright;
EXTERN Widget stop_tele;
EXTERN Widget bouton_proto;
EXTERN Widget teleph;
EXTERN Widget bouton_demarre, bouton_arrete;
EXTERN Widget la_cassete;

/* copyright.c */
EXTERN Boolean flag_copyright_affiche;
EXTERN Boolean low_memory;

/* version.c */
extern char version_xtel;
extern char revision_xtel;
extern char prototype_xtel;

#endif
