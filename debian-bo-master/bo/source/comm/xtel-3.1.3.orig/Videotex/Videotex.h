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
#ident "$Id: Videotex.h,v 1.6 1996/10/31 12:59:14 pierre Exp $"

#ifndef _XtVideotex_h
#define _XtVideotex_h

#include <stdio.h>

/***********************************************************************
 *
 * Videotex Widget
 *
 ***********************************************************************/

/* Parameters:

 Name		        Class		        RepType		Default Value
 ----		        -----		        -------		-------------
 petiteFonte   	     	PetiteFonte		Boolean		False
 nGris   	     	NGris			Boolean		False
 connecte		Connecte		Boolean		False
 fdConnexion		FdConnexion		int		-1
 commandeDeconnexion	CommandeDeconnexion	String		NULL
 modeCallback		ModeCallback		Callback	NULL
 enregCallback		EnregCallback		Callback	NULL
 modeVideotex		ModeVideotex		Boolean		True
 enqROM			EnqROM			String		Cv1
*/

#define XtNpetiteFonte		"petiteFonte"
#define XtNconnecte		"connecte"
#define XtNcommandeDeconnexion	"commandeDeconnexion"
#define XtNnGris		"nGris"
#define XtNfdConnexion		"fdConnexion"
#define XtNmodeCallback		"modeCallback"
#define XtNenregCallback	"enregCallback"
#define XtNmodeVideotex		"modeVideotex"
#define XtNenqROM		"enqROM"

#define XtNfonteG0no_p		"fonteG0no_p"
#define XtNfonteG0dl_p		"fonteG0dl_p"
#define XtNfonteG0dh_p		"fonteG0dh_p"
#define XtNfonteG0dt_p		"fonteG0dt_p"

#define XtNfonteG0nos_p		"fonteG0nos_p"
#define XtNfonteG0dls_p		"fonteG0dls_p"
#define XtNfonteG0dhs_p		"fonteG0dhs_p"
#define XtNfonteG0dts_p		"fonteG0dts_p"

#define XtNfonteG1_p		"fonteG1_p"
#define XtNfonteG1s_p		"fonteG1s_p"

#define XtNfonteG0no_g		"fonteG0no_g"
#define XtNfonteG0dl_g		"fonteG0dl_g"
#define XtNfonteG0dh_g		"fonteG0dh_g"
#define XtNfonteG0dt_g		"fonteG0dt_g"

#define XtNfonteG0nos_g		"fonteG0nos_g"
#define XtNfonteG0dls_g		"fonteG0dls_g"
#define XtNfonteG0dhs_g		"fonteG0dhs_g"
#define XtNfonteG0dts_g		"fonteG0dts_g"

#define XtNfonteG1_g		"fonteG1_g"
#define XtNfonteG1s_g		"fonteG1s_g"


#define XtCPetiteFonte		"PetiteFonte"
#define XtCConnecte		"Connecte"
#define XtCCommandeDeconnexion	"CommandeDeconnexion"
#define XtCNGris		"NGris"
#define XtCFdConnexion		"FDConnexion"
#define XtCModeCallback		"ModeCallback"
#define XtCEnregCallback	"EnregCallback"
#define XtCModeVideotex		"ModeVideotex"
#define XtCModeVideotex		"ModeVideotex"
#define XtCEnqROM		"EnqROM"

typedef struct _VideotexRec *VideotexWidget;  /* completely defined in VideotexPrivate.h */
typedef struct _VideotexClassRec *VideotexWidgetClass;    /* completely defined in VideotexPrivate.h */

extern WidgetClass videotexWidgetClass;

/*
 * Fonctions publiques 
 */
#ifdef __STDC__
extern void videotexInit(Widget w);
extern void videotexDecode(Widget w, int c);
extern Pixmap videotexPixmapEcranSauve (Widget w);
extern void videotexSauveEcran (Widget w);
extern void videotexConversionAscii (Widget w, FILE* fp);
#else
extern void videotexDecode ();
extern void videotexInit ();
extern Pixmap videotexPixmapEcranSauve ();
extern void videotexSauveEcran ();
extern void videotexAfficheTelephone ();
extern void videotexEffaceTelephone ();
extern void videotexConversionAscii ();
#endif /* __STDC__ */

#endif /* _XtVideotex_h */
/* DON'T ADD STUFF AFTER THIS #endif */
