/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: setup.h 
 * header file for xsetup.c
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _XSETUP_H
#define _XSETUP_H

#ifdef _XSETUP_C
#define _EXTERN 
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN char **split_string (char *string, int *largc);
_EXTERN void parse_commandline (int argc, char *argv[]);
_EXTERN int interactive_config (char *argv0);
_EXTERN void interactive_setup (int num_player);
_EXTERN void send_setup (int num_player);
_EXTERN void receive_setup (int num_player);
_EXTERN void config_from_database (XBConfig *config);
#ifdef DEBUG
_EXTERN void debug_from_database (XBDebug *debug);
#endif
#ifdef XBLAST_SOUND
_EXTERN char *sound_server_from_database (void);
#endif
_EXTERN void set_setup_communication (int type);
_EXTERN void merge_default_databases (XBConfig *config);
_EXTERN void setup_from_database (XBSettings *setup);
_EXTERN void player_strings_from_database (PlayerStrings *st, XBConfig *conf);
_EXTERN void save_setup (XBConfig *config, XBSettings *setup, PlayerStrings *st);
_EXTERN void create_display_database (int disp);
_EXTERN int keys_from_database (int disp, int player, KeyPressDefine *keydef,
				PlayerAction *pa);
_EXTERN int color_mode_from_database (int disp);
_EXTERN int override_from_database (int disp);
_EXTERN void get_font_resources (int disp, char **font_name);
_EXTERN void get_color_resources (int disp, DisplayColor *dc);
_EXTERN void get_player_color_resources (PlayerColor *pc);
_EXTERN void delete_databases (int num_disp);

#else

_EXTERN char **split_string ();
_EXTERN void parse_commandline ();
_EXTERN int interactive_config ();
_EXTERN void interactive_setup ();
_EXTERN void send_setup ();
_EXTERN void receive_setup ();
_EXTERN void config_from_database ();
#ifdef DEBUG
_EXTERN void debug_from_database ();
#endif
#ifdef XBLAST_SOUND
_EXTERN char *sound_server_from_database ();
#endif
_EXTERN void set_setup_communication ();
_EXTERN void merge_default_databases ();
_EXTERN void setup_from_database ();
_EXTERN void player_strings_from_database ();
_EXTERN void save_setup ();
_EXTERN void create_display_database ();
_EXTERN int keys_from_database ();
_EXTERN int color_mode_from_database ();
_EXTERN int override_from_database ();
_EXTERN void get_font_resources ();
_EXTERN void get_color_resources ();
_EXTERN void get_player_color_resources ();
_EXTERN void delete_databases ();
#endif

#undef _EXTERN

#endif
/*
 * end of file xsetup.h
 */



