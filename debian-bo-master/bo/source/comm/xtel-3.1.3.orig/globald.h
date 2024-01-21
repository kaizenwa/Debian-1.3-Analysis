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
#ident "$Id: globald.h,v 1.7 1996/12/20 16:16:38 pierre Exp $"

#ifndef _globald_h
#define _globald_h

typedef char Boolean;
#define True	1
#define False	0

#ifndef EXTERN
#define EXTERN extern
#endif

/*
 * Variables
 */
#ifndef __FreeBSD__
extern char *sys_errlist[];
#endif

EXTERN struct definition_ligne definition_lignes[MAX_LIGNES]; 
EXTERN struct definition_service definition_services[MAX_SERVICES];
EXTERN Boolean flag_m1;
EXTERN int fd_modem;
EXTERN int nb_lignes;
EXTERN char numero_ligne;
#ifndef USE_SYSLOG
EXTERN FILE *fp_console;
#endif

/*
 * Fonctions
 */
#ifdef __STDC__

/* xteld.c */
void erreur_a_xtel(char *s, int code_erreur);
int service_autorise(int indice_service, int *);
void appel_service(char *service_teletel);
int main(int ac, char **av);
/* dial.c */
void myundial(int fd);
int mydial(char *telno, char *device);
/* config.c */
int lecture_services(void);
int lecture_configuration_lignes(void);
/* misc.c */
char *next_token(char *, char *);
char *build_name(char *);
/* ian.c */
int ian_valide (int, char);
void ian_init (char*);

#else

void erreur_a_xtel();
int service_autorise();
void appel_service();
int main();
void myundial();
int mydial();
int lecture_services();
int lecture_configuration_lignes();
char *next_token();
char *build_name();
int ian_valide ();
void ian_init ();

#endif /* __STDC__ */

#endif
