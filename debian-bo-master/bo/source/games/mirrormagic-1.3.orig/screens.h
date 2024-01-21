/***********************************************************
*  Mirror Magic II -- McDuffins Revenge                    *
*----------------------------------------------------------*
*  ©1994 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.ms.sub.org                  *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  screens.h                                               *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#ifndef SCREENS_H
#define SCREENS_H

#include "main.h"

/* Setup-Bits */
#define SETUP_TOONS			(1<<0)
#define SETUP_SOUND			(1<<1)
#define SETUP_SOUND_LOOPS		(1<<2)
#define SETUP_SOUND_MUSIC		(1<<3)
#define DEFAULT_SETUP			(SETUP_TOONS |		\
					 SETUP_SOUND |		\
					 SETUP_SOUND_LOOPS |	\
					 SETUP_SOUND_MUSIC)

/* Setup-Voreinstellungen */
#define SETUP_TOONS_ON(x)		((x) & SETUP_TOONS)
#define SETUP_SOUND_ON(x)		((x) & SETUP_SOUND)
#define SETUP_SOUND_LOOPS_ON(x)		((x) & SETUP_SOUND_LOOPS)
#define SETUP_SOUND_MUSIC_ON(x)		((x) & SETUP_SOUND_MUSIC)

void DrawMainMenu();
void HandleMainMenu(int, int, int, int, int);
void DrawHelpScreen();
void HandleHelpScreen(int);
void HandleTypeName(int, KeySym);
void DrawHallOfFame(int);
void HandleHallOfFame(int);
void DrawSetupScreen();
void HandleSetupScreen(int, int, int, int, int);

#endif
