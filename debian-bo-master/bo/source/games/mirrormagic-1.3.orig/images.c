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
*  images.c                                                *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#include "images.h"

struct PictureFile icon_pic =
{
  "mirrormagic_icon.xbm",
  "mirrormagic_iconmask.xbm"
};

struct PictureFile pic[NUM_PICTURES] =
{
  "DoubleRayScreen.xpm",
  "DoubleRayScreenMaske.xbm",

  "DoubleRayDoor.xpm",
  "DoubleRayDoorMaske.xbm",

  "DoubleRayFont.xpm",
  "DoubleRayFontMaske.xbm",

  "DoubleRayFont2.xpm",
  "DoubleRayFont2Maske.xbm",

  NULL,
  "DoubleRayMaskeF.xbm"
}; 
