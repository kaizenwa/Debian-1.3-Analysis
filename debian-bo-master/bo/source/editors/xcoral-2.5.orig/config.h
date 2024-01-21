/* ########################################################################

				config.h

   File: config.h
   Path: /home/fournigault/c/X11/xcoral-2.31/config.h
   Description: 
   Created: Fri Jan 27 10:55:13 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:55:14 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#ifndef _CONFIG_H_
#define _CONFIG_H_

/*
 *	Le nom
 */
#define CLIENTNAME	xcoral

/*
 *	Les Fontes
 */
#define TEXT_FONT	"-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1"
#define MENU_FONT	"-adobe-times-medium-r-normal--*-180-*-*-p-*-iso8859-1"

/*
 *	Les Couleurs
 */
#define COLOR_TEXT_BG		"midnightblue"
#define COLOR_TEXT_FG		"lemonchiffon"
#define COLOR_MENU_BG		"lightslategray"
#define COLOR_MENU_FG		"lemonchiffon"
#define COLOR_CONTROL_BG	"lightslategray"
#define COLOR_CONTROL_FG	"lemonchiffon"

/*
 *	Mono
 */
#define BW_TEXT_BG		"black"
#define BW_TEXT_FG		"white"
#define BW_MENU_BG		"white"
#define BW_MENU_FG		"black"
#define BW_CONTROL_BG		"white"
#define BW_CONTROL_FG		"black"

#define APP_DEFAULT		"/usr/lib/X11/app-defaults"

#endif  /* _CONFIG_H_ */
