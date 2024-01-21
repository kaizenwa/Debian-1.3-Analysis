/* Definitions of system-specific things for Xconq.
   Copyright (C) 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Note that this assumes module.h is already included. */

extern FILE *open_module_library_file PARAMS ((Module *module));
extern FILE *open_module_explicit_file PARAMS ((Module *module));
extern FILE *open_library_file PARAMS ((char *filename));
extern char *default_library_filename PARAMS ((void));
extern char *news_filename PARAMS ((void));
extern char *saved_game_filename PARAMS ((void));
extern char *checkpoint_filename PARAMS ((void));
extern char *error_save_filename PARAMS ((void));
extern char *statistics_filename PARAMS ((void));
extern FILE *open_scorefile_for_reading PARAMS ((char *name));
extern FILE *open_scorefile_for_writing PARAMS ((char *name));
extern void make_pathname PARAMS ((char *path, char *name, char *extn,
				  char *pathbuf));
extern void remove_saved_game PARAMS ((void));
extern void init_signal_handlers PARAMS ((void));
extern int n_seconds_elapsed PARAMS ((int n));
extern int n_ms_elapsed PARAMS ((int n));
extern void record_ms PARAMS ((void));

