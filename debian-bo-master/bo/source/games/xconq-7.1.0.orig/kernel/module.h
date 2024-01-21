/* Definitions for game modules in Xconq.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* A variant describes an option that is available to players starting
   up a game, as well as modules including each other. */

typedef struct a_variant {
    Obj *id;			/* unique id */
    char *name;			/* displayable name */
    Obj *dflt;			/* default value */
    Obj *range;			/* description of range of values */
    Obj *cases;			/* actions to do on matches */
    int used;			/* true if the variant has been set to a value already */
    int hasintvalue;		/* true if integer value defined */
    int intvalue;		/* integer value of the variant */
} Variant;

/* A file module records relevant info about the module, what it included,
   how to write it out, etc. */

typedef struct a_module {
    char *name;			/* the actual unique name of the module */
    char *title;		/* a readable display name */
    char *blurb;		/* a descriptive one-liner */
    char *picturename;		/* name of a descriptive image */
    char *basemodulename;	/* name of the module that this one includes */
    char *defaultbasemodulename;  /* what game to load if something missing */
    char *basegame;		/* what general game this is based on */
    Obj *instructions;		/* basic instructions */
    Obj *notes;			/* player notes */
    Obj *designnotes;		/* designer notes */
    char *version;		/* the version of this module */
    char *programversion;	/* compatible program versions */
    Variant *variants;		/* array of player-choosable variants */
    char *origmodulename;	/* module that this was originally (before save) */
    Variant *origvariants;	/* variants chosen for the original module */
    char *origversion;		/* the version of the original module */
    char *contents;		/* a string with the actual contents */
    char *sp;			/* "string pointer" a la file pointer */
    char *filename;		/* the filename */
    FILE *fp;			/* the stdio file buffer */
    char *hook;			/* space for system-specific file info */
    int startlineno;		/* line number being read at start of form */
    int endlineno;		/* line number being read at end of form */
    /* Primarily for designer use, to control writing of a module */
    short def_all;		/* true if all data is to be written out */
    short read_only;		/* true if shouldn't be modified */
    short def_types;		/* flags indicating what to put in a module */
    short def_tables;
    short compress_tables;
    short def_globals;
    short def_scoring;
    short def_world;
    short def_areas;
    short def_area_terrain;
    short def_area_misc;
    short def_area_weather;
    short def_area_material;
    short compress_layers;	/* true if layer data should be compressed when written */
    short def_sides;
    short def_side_views;
    short def_side_doctrines;
    short def_players;
    short def_agreements;
    short def_units;
    short def_unit_ids;
    short def_unit_props;
    short def_unit_acts;
    short def_unit_plans;
    short def_history;
    short maybe_reshape;
    int subarea_width;
    int subarea_height;
    int subarea_x;
    int subarea_y;
    int final_subarea_width;
    int final_subarea_height;
    int final_subarea_x;
    int final_subarea_y;
    int final_width;
    int final_height;
    int final_circumference;
    short fill_type;
    short open;			/* true if currently open */
    short loaded;		/* true if already loaded */
    struct a_module *next;	/* pointer to next module */
    struct a_module *include;	/* pointer to first included module */
    struct a_module *nextinclude;  /* pointer to next included module */
    struct a_module *lastinclude;  /* pointer to last included module */
} Module;

/* Iteration over the list of modules. */

#define for_all_modules(m)  \
  for (m = modulelist; m != NULL; m = m->next)

#define for_all_includes(m,sub)  \
  for (sub = m->include; sub != NULL; sub = sub->nextinclude)

extern Module *modulelist;
extern Module *mainmodule;

/* Declarations of module functions. */

extern void clear_game_modules PARAMS ((void));
extern Module *create_game_module PARAMS ((char *name));
extern Module *find_game_module PARAMS ((char *name));
extern Module *get_game_module PARAMS ((char *name));
extern Module *add_game_module PARAMS ((char *name, Module *includer));
extern void describe_game_modules PARAMS ((int arg, char *key, char *buf));
extern void describe_game_module_aux PARAMS ((char *buf, Module *module, int level));
extern void describe_module_notes PARAMS ((char *buf, Module *module));
extern void load_default_game PARAMS ((void));
extern int load_game_description PARAMS ((Module *module));
extern void load_game_module PARAMS ((Module *module, int dowarn));
extern void load_base_module PARAMS ((Module *module));
extern int open_module PARAMS ((Module *module, int dowarn));
extern void read_forms PARAMS ((Module *module));
extern void init_module_reshape PARAMS ((Module *module));
extern int reshape_the_output PARAMS ((Module *module));
extern int valid_reshape PARAMS ((Module *module));
extern void close_module PARAMS ((Module *module));
extern char *module_desig PARAMS ((Module *module));







