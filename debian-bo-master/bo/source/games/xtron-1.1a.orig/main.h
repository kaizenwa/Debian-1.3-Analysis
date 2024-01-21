/* main.h - xtron v1.1 header for main
 *
 *   Copyright (C) 1995 Rhett D. Jacobs <rhett@hotel.canberra.edu.au>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Last Modified: 16/4/95
 */
 
#ifndef _MAIN_H
#define _MAIN_H

#include <string.h>
#include <sys/types.h>
#include <sys/time.h>

#include "wintype.h"
#include "xtron.h"
#include "resource.h"
#include "xpm2pixmap.h"

#define TOTAL_WIDTH 384
#define TOTAL_HEIGHT 512
#define WPIECE 8
#define HPIECE 8
#define WBOARD 128
#define HBOARD 128
#define WBUTTON1 96
#define HBUTTON1 32
#define WBUTTON2 192
#define HBUTTON2 32
#define WBUTTON3 384
#define HBUTTON3 32

extern struct Player p[2];
extern struct Board b;

void ButtonEvent(XButtonEvent *pEvent);
void KeyEvent(XKeyEvent *pEvent);
void ExposeEvent(XExposeEvent *pEvent);
Window set_window(int x, int y, int width, int height);
void mapwindows(void);
Pixmap set_icon(char *filen);
int check_valid(int p_num, int x_inc, int y_inc);
void think(int p_num);
int game_update(void);
void delay(int len);
void restart_game(void);
void open_windows(int argc, char **argv);
void assign_bitmaps(void);
void assign_keys(void);

#endif
