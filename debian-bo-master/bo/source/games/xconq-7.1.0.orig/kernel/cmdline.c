/* Command line parsing for Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This is a command-line parser that may be used in implementations that
   get a command line from somewhere. */

/* Command lines get parsed in several stages, since for instance
   the choice of game module will decide which variants are available. */

#include "conq.h"
extern void add_library_path PARAMS ((char *path));
#include "cmdline.h"

static void version_info PARAMS ((void));
static void general_usage_info PARAMS ((void));
static void game_usage_info PARAMS ((void));
static void unixify_variant_name PARAMS ((char *buf, char *varname));
static void player_usage_info PARAMS ((void));
static void parse_world_option PARAMS ((char *str));
static void parse_realtime_option PARAMS ((char *subopt, char *arg));
static void parse_variant PARAMS ((char *str));
static void add_a_raw_player_spec PARAMS ((char *specstr));

/* The startup-settable options. */

static int option_alltimeout;
static int option_totalgametime;
static int option_perturntime;
static int option_persidetime;
static int option_add_default_player;

static char *default_ai_type = ",mplayer";

/* This says whether the command line arguments actually chose a
   specific game to play (as opposed to setting random flags). */

static int game_chosen = FALSE;

/* The list of asked-for players. */

struct raw_spec {
  char *spec;
  struct raw_spec *next;
} *raw_player_specs, *last_raw_player_spec;

static char *raw_default_player_spec;

/* The list of modules to loaded in addition to the main one. */

struct module_spec {
  Module *module;
  struct module_spec *next;
} *extra_modules, *last_extra_module;

/* The list of accumulated variant choices. */

static Obj *variant_settings;

static char *program_name = "";

static int help_wanted = FALSE;

static int variant_help_wanted = FALSE;

static int version_wanted = FALSE;

static int had_error = FALSE;

/* Set the most basic of defaults on the dynamically-settable options. */

/* (need flags to indicate which options were actually used, so variant
   handling can warn about improper use) */

void
init_options()
{
    option_add_default_player = TRUE;
    variant_settings = lispnil;
}

/* Generic command line parsing.  This is used by several different programs,
   so it just collects info, doesn't process it much.  This is called
   several times, because the validity of some args depends on which
   game modules are loaded and which players are to be in the game,
   and interfaces may need to do some of their own processing in between. */

void
parse_command_line(argc, argv, spec)
int argc, spec;
char *argv[];
{
    char *arg, *aispec, tmpspec[100];
    int i, n, numused;

/* This macro just checks that a required next argument is actually there. */

#define REQUIRE_ONE_ARG  \
  if (i + 1 >= argc) {  \
    fprintf(stderr, "Error: `%s' requires an argument, exiting now\n", argv[i]);  \
    had_error = TRUE;  \
    continue;  \
  }  \
  numused = 2;

/* Each of these causes argument parsing to skip over the option if it's
   not the right time to look at it. */

#define GENERAL_OPTION if (spec != general_options) continue;
#define VARIANT_OPTION if (spec != variant_options) continue;
#define PLAYER_OPTION  if (spec != player_options)  continue;

    /* (should peel off any path stuff) */
    program_name = argv[0];

    if (spec == general_options)
      init_options();

    for (i = 1; i < argc; ++i) {
	if (argv[i] == NULL || (argv[i])[0] == '\0') {
	    /* Already munched, nothing to do. */
	} else if ((argv[i])[0] == '-') {
	    arg = argv[i];
	    Dprintf("%s\n", arg);
	    numused = 1;
	    if (strcmp(arg, "-c") == 0) {
		REQUIRE_ONE_ARG;
		GENERAL_OPTION;
		checkpointinterval = atoi(argv[i+1]);
	    } else if (strcmp(arg, "-design") == 0) {
		GENERAL_OPTION;
#ifdef DESIGNERS
		allbedesigners = TRUE;
#else
		fprintf(stderr,
			"No designing available, ignoring option `%s'\n", arg);
#endif /* DESIGNERS */
	    } else if (strncmp(arg, "-D", 2) == 0) {
		GENERAL_OPTION;
#ifdef DEBUGGING
		Debug = TRUE;
		if (strchr(arg+2, '-'))
		  Debug = FALSE;
		if (strchr(arg+2, 'M'))
		  DebugM = TRUE;
		if (strchr(arg+2, 'G'))
		  DebugG = TRUE;
#else
		fprintf(stderr,
			"No debugging available, ignoring option `%s'\n", arg);
#endif /* DEBUGGING */
	    } else if (strncmp(arg, "-e", 2) == 0) {
		REQUIRE_ONE_ARG;
		PLAYER_OPTION;
		n = atoi(argv[i+1]);
		/* A comma indicates that the name of a particular desired
		   AI type follows. */
		if (strlen(arg) > 2) {
		    aispec = arg + 2;
		    if (*aispec != ',') {
			sprintf(tmpspec, ",mplayer%s", aispec);
			aispec = tmpspec;
		    }
		} else {
		    aispec = default_ai_type;
		}
		while (n-- > 0)
		  add_a_raw_player_spec(aispec);
	    } else if (strcmp(arg, "-f") == 0) {
		REQUIRE_ONE_ARG;
		GENERAL_OPTION;
		add_a_module(NULL, argv[i+1]); 
	    } else if (strcmp(arg, "-g") == 0) {
		REQUIRE_ONE_ARG;
		GENERAL_OPTION;
		add_a_module(copy_string(argv[i+1]), NULL);
	    } else if (strcmp(arg, "-h") == 0) {
		REQUIRE_ONE_ARG;
		PLAYER_OPTION;
		fprintf(stderr, "  %s not implemented yet, sorry\n", arg);
		had_error = TRUE;
	    } else if (strcmp(arg, "-help") == 0) {
		GENERAL_OPTION;
		help_wanted = TRUE;
		/* Will display help info later. */
	    } else if (strcmp(arg, "-join") == 0) {
		REQUIRE_ONE_ARG;
		GENERAL_OPTION;
		fprintf(stderr, "  %s not implemented yet, sorry\n", arg);
		had_error = TRUE;
	    } else if (strcmp(arg, "-L") == 0) {
		REQUIRE_ONE_ARG;
		GENERAL_OPTION;
		if (strcmp(argv[i+1], "-") == 0)
		  add_library_path(NULL);
		else
		  add_library_path(argv[i+1]);
	    } else if (strcmp(arg, "-M") == 0) {
		REQUIRE_ONE_ARG;
		VARIANT_OPTION;
		parse_world_option(argv[i+1]);
	    } else if (strcmp(arg, "-mail") == 0) {
		GENERAL_OPTION;
		fprintf(stderr, "  %s not implemented yet, sorry\n", arg);
		had_error = TRUE;
	    } else if (strcmp(arg, "-r") == 0) {
		PLAYER_OPTION;
		option_add_default_player = FALSE;
	    } else if (strcmp(arg, "-R") == 0) {
		REQUIRE_ONE_ARG;
		GENERAL_OPTION;
#ifdef DEBUGGING
		init_xrandom(atoi(argv[i+1]));
#else
		fprintf(stderr,
			"No debugging available, ignoring option `%s'\n", arg);
#endif /* DEBUGGING */
	    } else if (strcmp(arg, "-sm") == 0) {
		VARIANT_OPTION;
		push_key_int_binding(&variant_settings, K_SEQUENTIAL, 0);
	    } else if (strcmp(arg, "-sq") == 0) {
		VARIANT_OPTION;
		push_key_int_binding(&variant_settings, K_SEQUENTIAL, 1);
	    } else if (strncmp(arg, "-t", 2) == 0) {
		REQUIRE_ONE_ARG;
		VARIANT_OPTION;
		parse_realtime_option(arg + 2, argv[i+1]);
	    } else if (strncmp(arg, "-v", 2) == 0) {
		VARIANT_OPTION;
		parse_variant(arg + 2);
	    } else if (strcmp(arg, "-V") == 0) {
		VARIANT_OPTION;
		push_key_int_binding(&variant_settings, K_SEE_ALL, 1);
	    } else if (strcmp(arg, "-w") == 0) {
		GENERAL_OPTION;
		warnings_suppressed = TRUE;
	    } else if (strcmp(arg, "-wait") == 0) {
		REQUIRE_ONE_ARG;
		PLAYER_OPTION;
		fprintf(stderr, "  %s not implemented yet, sorry\n", arg);
		had_error = TRUE;
	    } else if (strcmp(arg, "-x") == 0) {
		GENERAL_OPTION;
		option_popup_new_game_dialog = TRUE;
	    } else if (strcmp(arg, "--help") == 0) {
		GENERAL_OPTION;
		help_wanted = TRUE;
		/* Will display help info later. */
	    } else if (strcmp(arg, "--version") == 0) {
		GENERAL_OPTION;
		version_wanted = TRUE;
	    } else {
		numused = 0;
		/* Anything unrecognized during the last parse is an error. */
		if (spec >= leftover_options) {
		    fprintf(stderr, "Unrecognized option `%s'\n", arg);
		    had_error = TRUE;
		}
	    }
	    if (numused >= 1)
	      argv[i] = "";
	    if (numused >= 2)
	      argv[i+1] = "";
	    if (numused >= 1)
	      i += (numused - 1);
	} else {
	    if (spec == player_options) {
		if (*(argv[i]) == '+' || *(argv[i]) == '@') {
		    raw_default_player_spec = argv[i];
		} else {
		    add_a_raw_player_spec(argv[i]);
		}
		argv[i] = NULL;
	    }
	}
    }
    if (version_wanted) {
	version_info();
    }
    if (had_error || help_wanted || variant_help_wanted) {
	/* If we want to get help about a particular game, have to load it first. */
	if (help_wanted || variant_help_wanted)
	  load_all_modules();
	if (had_error || help_wanted)
	  general_usage_info();
	if (had_error || help_wanted || variant_help_wanted)
	  game_usage_info();
	if (had_error || help_wanted)
	  player_usage_info();
	if (help_wanted && mainmodule != NULL) {
	    /* (should display other random info about the game here) */
	}
    }
    if (had_error || help_wanted || variant_help_wanted || version_wanted) {
	exit(had_error);
    }
}

/* Given a module name and/or filename, add it to the list of modules
   to load. */

void
add_a_module(name, filename)
char *name, *filename;
{
    Module *module;

    module = get_game_module(name);
    if (module == NULL)
      exit(1);  /* bad error if happens */
    if (filename)
      module->filename = copy_string(filename);
    if (mainmodule == NULL) {
	mainmodule = module;
    } else {
	struct module_spec *extra = (struct module_spec *) xmalloc(sizeof(struct module_spec));

	extra->module = module;
	if (extra_modules == NULL)
	  extra_modules = extra;
	else
	  last_extra_module->next = extra;
	last_extra_module = extra;
    }
    game_chosen = TRUE;
}

static void
add_a_raw_player_spec(specstr)
char *specstr;
{
    struct raw_spec *spec =
	(struct raw_spec *) xmalloc (sizeof(struct raw_spec));

    spec->spec = copy_string(specstr);
    if (raw_player_specs == NULL)
      raw_player_specs = spec;
    else
      last_raw_player_spec->next = spec;
    last_raw_player_spec = spec;
}

static void
version_info()
{
    printf("Xconq (%s) version %s\n", program_name, version_string());
}

/* This routine prints out help info about all the possible arguments. */

static void
general_usage_info()
{
    printf("Usage:\n\t%s [ -options ... ]\n\n", program_name);
    printf("General options:\n\n");
    printf("    -c n\t\tcheckpoint every <n> turns\n");
    printf("    -f filename\t\trun <filename> as a game\n");
    printf("    -g gamename\t\tfind <gamename> in library and run it\n");
    printf("    -help\t\tdisplay this help info\n");
    printf("    -join <game@host>\tconnect to the given game\n");
    printf("    -L pathname\t\tset <pathname> to be library location\n");
    printf("    -mail\t\tset up game as play-by-email\n");
    printf("    -t mins\t\tlimit each player to <mins> of play time total\n");
    printf("    -tside mins\t\tlimit each player to <mins> of time each turn\n");
    printf("    -tturn mins\t\tlimit each turn to <mins> minutes\n");
    printf("    -w\t\t\tsuppress warnings\n");
    printf("    -design\t\tmake every player a designer\n");
    printf("Long options:\n");
    printf("    --help\t\tdisplay this help info\n");
    printf("    --version\t\tdisplay version info\n");
}

/* Describe the available variants for a game. */

static void
game_usage_info()
{
    int i, wid, hgt, circumf, lat, lon;
    char *varname = "?", *vartypename = NULL;
    char buf[BUFSIZE];
    Variant *var;
    Obj *vardflt;

    printf("\nGame variant options");
    if (mainmodule == NULL) {
	printf(":\n\n    No game loaded, no information available.\n\n");
	return;
    }
    printf(" for \"%s\":\n\n", mainmodule->name);
    if (mainmodule->variants == NULL) {
	printf("    No variants available.\n\n");
	return;
    }
    for (i = 0; mainmodule->variants[i].id != lispnil; ++i) {
	var = &(mainmodule->variants[i]);
	varname = var->name;
	vartypename = c_string(var->id);
	vardflt = var->dflt;
	switch (keyword_code(vartypename)) {
	  case K_SEE_ALL:
	    printf("    -V\t\t\tmake everything be always seen (default %s)\n",
		   (vardflt == lispnil ? "true" :
		    (c_number(eval(vardflt)) ? "true" : "false")));
	    break;
	  case K_SEQUENTIAL:
	    printf("    -sm\t\t\tmove simultaneously (default %s)\n",
		   (vardflt == lispnil ? "true" :
		    (c_number(eval(vardflt)) ? "true" : "false")));
	    printf("    -sq\t\t\tmove sequentially (default %s)\n",
		   (vardflt == lispnil ? "false" :
		    (c_number(eval(vardflt)) ? "false" : "true")));
	    break;
	  case K_WORLD_SEEN:
	    printf("    -v\t\t\tmake the world be seen already (default %s)\n",
		   (vardflt == lispnil ? "true" :
		    (c_number(eval(vardflt)) ? "true" : "false")));
	    break;
	  case K_WORLD_SIZE:
	    printf("    -M wid[xhgt][Wcircumf][+lat][+lon]\tset world size (default ");
	    /* Note that if the game definition sets these values directly using world
	       or area forms, this is misleading; but that's the fault of the game
	       designer for including both preset values and a variant whose defaults
	       don't match those presets. */
	    circumf = DEFAULTCIRCUMFERENCE;
	    wid = DEFAULTWIDTH;  hgt = DEFAULTHEIGHT;
	    lat = lon = 0;
	    /* Pick the width and height out of the list. */
	    if (vardflt != lispnil) {
		wid = c_number(eval(car(vardflt)));
		vardflt = cdr(vardflt);
	    }
	    if (vardflt != lispnil) {
		hgt = c_number(eval(car(vardflt)));
		vardflt = cdr(vardflt);
	    } else {
		hgt = wid;
	    }
	    /* Pick up a circumference etc if given. */
	    if (vardflt != lispnil) {
		circumf = c_number(eval(car(vardflt)));
		vardflt = cdr(vardflt);
	    }
	    if (vardflt != lispnil) {
		lat = c_number(eval(car(vardflt)));
		vardflt = cdr(vardflt);
	    }
	    if (vardflt != lispnil) {
		lon = c_number(eval(car(vardflt)));
	    }
	    printf("%dx%dW%d", wid, hgt, circumf);
	    if (lat != 0 || lon != 0)
	      printf("+%d+%d", lat, lon);
	    printf(")\n");
	    break;
	  default:
	    unixify_variant_name(buf, varname);
	    printf("    -v%s[=value] (default ", buf);
	    if (vardflt == lispnil
	        || (numberp(vardflt) && c_number(vardflt) == 0)) {
		printf("false");
	    } else if (numberp(vardflt) && c_number(vardflt) == 1) {
		printf("true");
	    } else {
		sprintlisp(buf, vardflt);
		printf("%s", buf);
	    }
	    printf(")\n");
	    break;
	}
    }
}

/* Replace blanks in a variant's name with hyphens, and put the whole
   name in lowercase. */

static void
unixify_variant_name(buf, varname)
char *buf, *varname;
{
    int i, slen;

    strcpy(buf, varname);
    slen = (int) strlen(buf);
    for (i = 0; i < slen; ++i) {
	if (buf[i] == ' ')
	  buf[i] = '-';
	if (isupper(buf[i]))
	  buf[i] = tolower(buf[i]);
    }
}

static void
player_usage_info()
{
    printf("\nPlayer setup options:\n\n");
    printf("    [[name][,ai][/config]@]host[+advantage]\tadd player\n");
    printf("        ai\t\t= name of AI type or \"ai\" for default type\n");
    printf("        config\t\t= name of config file\n");
    printf("        host\t\t= name of player's host machine or display\n");
    printf("        advantage\t= numerical initial advantage (default 1)\n");
    printf("    -e number[,ai]\tadd <number> computer players\n");
    printf("    -h number[,ai]\tadd <number> human players\n");
    printf("    -r\t\t\tno default player on local display\n");
    printf("    -wait minutes\t\twait time for players to join\n");
}

/* Given a string representing world dimensions, extract various components
   and compose a variant setting. */

static void
parse_world_option(str)
char *str;
{
    int width, height, circumference;
    char *str2;
    Obj *varval;

    width = atoi(str);
    str2 = strchr(str, 'x');
    if (str2 != NULL) {
	height = atoi(str2 + 1);
    } else {
	height = width;
    }
    varval = lispnil;
    /* Add a world circumference setting if present. */
    str2 = strchr(str, 'W');
    if (str2 != NULL) {
	circumference = atoi(str2 + 1);
	if (circumference > 0)
	  varval = cons(new_number(circumference), varval);
	else
	  fprintf(stderr, "Requested circumference is bad, ignoring\n");
    }
    varval = cons(new_number(width), cons(new_number(height), varval));
    /* Glue onto the list of variant_settings. */
    push_key_cdr_binding(&variant_settings, K_WORLD_SIZE, varval);
}

static void
parse_realtime_option(subopt, arg)
char *subopt, *arg;
{
    if (strcmp(subopt, "-timeout") == 0) {
	option_alltimeout = 60 * atoi(arg);
    } else if (strcmp(subopt, "-tgame") == 0) {
	option_totalgametime = 60 * atoi(arg);
    } else if (strcmp(subopt, "-tside") == 0) {
	option_persidetime = 60 * atoi(arg);
    } else if (strcmp(subopt, "-tturn") == 0) {
	option_perturntime = 60 * atoi(arg);
    } else {
    	/* usage? */
    }
}

/* Given a variant, turn it into a list "(name val)". */

static void
parse_variant(str)
char *str;
{
    char *varname = NULL, *str2;
    Obj *varval = lispnil;

    if (strcmp(str, "") == 0) {
	push_key_int_binding(&variant_settings, K_WORLD_SEEN, 1);
    } else if (strcmp(str, "help") == 0) {
	variant_help_wanted = TRUE;
    } else {
	str2 = strchr(str, '=');
	if (str2 != NULL && str2 != str) {
	    /* (should interp val as string or number) */
	    varval = new_number(atoi(str2 + 1));
	    varname = copy_string(str);
	    varname[str2 - str] = '\0';
	} else {
	    /* Assume varname by itself means "enable" (set to value of 1). */
	    varname = str;
	    varval = new_number(1);
	}
	if (varname)
	  push_binding(&variant_settings, intern_symbol(varname), varval);
    }
}

/* Load all the game modules that were asked for on cmd line. */

void
load_all_modules()
{
    struct module_spec *extra;

    if (mainmodule != NULL) {
	load_game_module(mainmodule, TRUE);
	for (extra = extra_modules; extra != NULL; extra = extra->next) {
	    load_game_module(extra->module, TRUE);
	}
    } else {
	/* If we've got absolutely no files to load, the standard game is
	   the one to go for.  It will direct the remainder of random gen. */
	load_default_game();
    }
}

/* Set module variants from command line options. */

void
set_variants_from_options()
{
    do_module_variants(mainmodule, variant_settings);
}

/* Set player characteristics from command-line options. */

void
set_players_from_options()
{
    int mergespecs;
    Player *player;
    struct raw_spec *spec;

    player = NULL;
    /* Assume that if any players exist already, then this is a restored
       game of some sort, and merge instead of adding new players. */
    mergespecs = (numplayers > 0);
    if (mergespecs)
      player = playerlist;
    /* Add the default player. */
    if (raw_player_specs == NULL || option_add_default_player) {
	if (!mergespecs)
	  player = add_default_player();
	parse_player_spec(player, raw_default_player_spec);
	canonicalize_player(player);
	player = player->next;
    }
    /* Add the explicitly listed players. */
    for (spec = raw_player_specs; spec != NULL; spec = spec->next) {
	if (mergespecs) {
	    if (player == NULL) {
		fprintf(stderr, "Excess player spec \"%s\", ignoring\n",
			spec->spec);
		continue;
	    }
	} else {
	    player = add_player();
	}
	parse_player_spec(player, spec->spec);
	canonicalize_player(player);
	player = player->next;
    }
}

/* This is not, strictly speaking, part of command line processing,
   but command-line programs are also the ones for which stdio is useful. */

void
print_instructions()
{
    Obj *instructions = mainmodule->instructions, *rest;

    printf("\n");
    if (instructions != lispnil) {
	if (stringp(instructions)) {
	    printf("%s\n", c_string(instructions));
	} else if (consp(instructions)) {
	    for (rest = instructions; rest != lispnil; rest = cdr(rest)) {
		if (stringp(car(rest))) {
		    printf("%s\n", c_string(car(rest)));
		} else {
		    /* (should probably warn about this case too) */
		}
	    }
	} else {
	    run_warning("Instructions are of wrong type");
	}
    } else {
	printf("(no instructions supplied)\n");
    }
}
