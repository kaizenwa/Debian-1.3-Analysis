/* Command-line parsing definitions for Xconq.
   Copyright (C) 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

enum parsestage {
    general_options,
    variant_options,
    player_options,
    interface_options,
    leftover_options
};

/* Global variables that command-line options tweak. */

extern int checkpointinterval;
extern int allbedesigners;
extern int option_popup_new_game_dialog;
extern int warnings_suppressed;

extern void init_options PARAMS ((void));
extern void parse_command_line PARAMS ((int argc, char **argv, int spec));
extern void add_a_module PARAMS ((char *name, char *filename));
extern void load_all_modules PARAMS ((void));
extern void set_variants_from_options PARAMS ((void));
extern void set_players_from_options PARAMS ((void));
extern void print_instructions PARAMS ((void));
