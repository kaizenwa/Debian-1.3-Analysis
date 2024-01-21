/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.01 -- module messages.c				     */
/*									     */
/*	Internationalisation and keyboard translation/customisation.	     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#include "xsok.h"
#include <ctype.h>

void (*lastcmd)(void);
int numeric_arg = 0;

const char *xsok_messages[] = {
    "Quit game?",
    "Another game?",
    "Restart game?",
    "Next level?",
    "Previous level?",
    "Not possible",
    "Bookmark set",
    "You won! (Score %d, Best score: %d)",
    "OK",
    "XSok version %s",
    "Moves: %d, Pushes: %d, Score: %d. Current strength: %d.",
    "Already at starting position.",
    "Move undone.",
    "Redo not possible.",
    "Move redone.",
    "Welcome to XSok version %s!",

    "Saving of game FAILED.",
    "Loading the game FAILED.",
    "Could not open file.",
    "Could not open file.",
    "Could not write header.",
    "Could not read header.",
    "Could not write moves.",
    "Could not read moves.",
    "Saving the game succeeded.",
    "Loading the game succeeded.",
    "Magic match failed.",
    
    "The author of this level is unknown.",
    "New record for this level!",
    "Cannot find a savegame file.",
    "Help on keys",
    "Help on the %s level subset",

    "Starting macro recording.",
    "Recorded macro has %d moves.",
    "Wrong position for macro replay.",

    "Proceed to the next unsolved level?",
    "Least moves: %d, Least pushes: %d, Best score: %d.",
    "This level is unsolved.",
    "Cannot link savegame files.",
};

void read_message_file(const char *filename) {
    FILE *fp;
    int i;
    char line[256];
    if (*filename != '/') {
	sprintf(line, "%s/%s/%s", xsokdir, langdir, filename);
	filename = line;
    }
    if (!(fp = fopen(filename, "r")))
	return;		/* Xt philosophy: ignore error */
    for (i = 0; i < sizeof(xsok_messages) / sizeof(const char *); ++i) {
	char *p;
    again:
	if (!fgets(line, sizeof(line), fp))
	    break;	/* EOF */
	if (*line == '#')
	    goto again;
	if ((p = strrchr(line, '\n')))
	    *p = '\0';
	if (*line)	/* empty lines => keep old text */
	    xsok_messages[i] = strsav(line);
    }
    fclose(fp);
}

static void cmd_None(void) {}

static void replace_binding(struct key_action *p, const char *name) {
    int i;
    static struct translator {
	const char *name;
	void (*func)(void);
    } translator[] = {
	{ "None",		cmd_None },
	{ "rq_LeaveSok",	rq_LeaveSok },
	{ "rq_RestartGame",	rq_RestartGame },
	{ "rq_NextLevel",	rq_NextLevel },
	{ "rq_NextUnsolved",	rq_NextUnsolved },
	{ "rq_PrevLevel",	rq_PrevLevel },
	{ "Up",			cmd_Up },
	{ "Left",		cmd_Left },
	{ "Down",		cmd_Down },
	{ "Right",		cmd_Right },
	{ "NextUnsolved",	cmd_NextUnsolved },
	{ "NextLevel",		cmd_NextLevel },
	{ "PrevLevel",		cmd_PrevLevel },
	{ "UndoMove",		cmd_UndoMove },
	{ "RedoMove",		cmd_RedoMove },
	{ "LeaveSok",		cmd_LeaveSok },
	{ "ShowScore",		cmd_ShowScore },
	{ "ShowBestScore",	cmd_ShowBestScore },
	{ "ShowAuthor",		cmd_ShowAuthor },
	{ "RestartGame",	cmd_RestartGame },
	{ "ReplayGame",		cmd_ReplayGame },
	{ "SaveGame",		cmd_SaveGame },
	{ "LoadGame",		cmd_LoadGame },
	{ "ReadScores",		cmd_ReadHighscores },
	{ "ShowVersion",	cmd_ShowVersion },
	{ "ResizeWindow",	cmd_Resize },
	{ "RepeatMove",		cmd_Repeat },
	{ "MouseMove",		cmd_MouseMove },	/* requires x, y */
	{ "MousePush",		cmd_MousePush },	/* requires x, y */
	{ "MouseUndo",		cmd_MouseUndo },
	{ "LevelInfo",		cmd_LevelInfo },
	{ "DropBookmark",	cmd_DropBookmark },
	{ "GotoBookmark",	cmd_GotoBookmark },
	{ "StartMacro",		cmd_StartMacro },
	{ "EndMacro",		cmd_EndMacro },
	{ "PlayMacro",		cmd_PlayMacro },
/*	{ "Debug",		cmd_debug }, */
	{ "Cancel",		cmd_Cancel },
	{ "Confirm",		cmd_Confirm }
    };
    for (i = 0; i < sizeof(translator) / sizeof(translator[0]); ++i)
	if (!strcmp(name, translator[i].name)) {
	    p->action = translator[i].func;
	    return;
	}
    fprintf(stderr, "WARNING: no function corresponds to \"%s\"\n", name);
    p->action = cmd_None;
}

static struct key_action *global_bindings = NULL;
const char *langdir = "";

void add_keybinding(struct key_action **cp, const char *cmd, const char *function) {
    /* a NULL pointer for cmd is a wildcard */
    int done = 0;
    struct key_action *p;

    if (!cp)
	return;		/* bindings for unimplemented rulesets */
    while (*cp) {
	p = *cp;
	if (!cmd || !strcmp(p->string, cmd)) {
	    /* replace this! */
	    replace_binding(p, function);
	    done = 1;
	}
	cp = &(p->next);
    }
    if (!done && cmd) {
	/* didn't find previous command */
	/* add a new entry */
	p = *cp = malloc_(sizeof(struct key_action));
	p->next = NULL;
	p->string = strsav(cmd);
	replace_binding(p, function);
    }
}


void read_keyboard_file(const char *filename) {
    FILE *fp;
    char line[256];
    char buff[32], cmd[2];
    struct key_action **cp = &global_bindings;

    cmd[1] = '\0';	/* 1-char commands currently */
    if (*filename != '/') {
	sprintf(line, "%s/%s/%s", xsokdir, langdir, filename);
	filename = line;
    }
    /* printf("reading keyboard file \"%s\"\n", filename); */
    if (!(fp = fopen(filename, "r"))) {
	/* in xsok, the keyboard file is required, not optional */
	/* therefore, we issue an error */
	fprintf(stderr, "Cannot read keyboard definition file %s\n",
		filename);
	if (*langdir)
	    fprintf(stderr, "Hint: Perhaps unsetting LANG or making a symbolic"
		    " link\nfrom %s/%s to %s helps.\n", xsokdir,
		    langdir, xsokdir);
	exit(1);
    }
    while (fgets(line, sizeof(line), fp)) {
	char *p;
	if ((p = strrchr(line, '\n')))
	    *p = '\0';
	if (!*line)
	    continue;
	if (!strncmp(line, "#c", 2))	/* comment */
	    continue;
	if (!strncmp(line, "#include ", 9)) {	/* include */
	    read_keyboard_file(line+9);
	    continue;
	}
	if (!strncmp(line, "#x", 2) && strlen(line) < 32) { /* hex number */
	    int c;
	    sscanf(line+2, "%x %s", &c, buff);
	    cmd[0] = c;
	    add_keybinding(cp, cmd, buff);
	    continue;
	}
	/* else assume string<space>commandname */
	if (strlen(line) >= 3 && strlen(line) < 32) {
	    p = line+1;
	    while (*p && *p != '\t' && *p != ' ')
		++p;
	    if (*p) {
		*p = '\0';
		while (*++p && (*p == '\t' || *p == ' '))
		    ;
		if (*p) {
		    add_keybinding(cp, line, p);
		    continue;
		}
	    }
	}
	fprintf(stderr, "Warning: cannot parse line in keys file:%s\n", line);
    }
    fclose(fp);
}

void key_pressed(char *str) {
    struct key_action *p;

    /* search for global binding */
    for (p = global_bindings; p; p = p->next)
	if (!strcmp(p->string, str)) {
	    (*p->action)();
	    lastcmd = p->action;
	    numeric_arg = 0;
	    return;
	}

    /* not found. break up the string to smaller pieces */
    if (strlen(str) > 1) {
	char s[2];
	while (*str) {
	    s[0] = *str++;
	    s[1] = '\0';
	    key_pressed(s);
	}
	return;
    }	/* use hardcoded entries. Note: These can be overridden by assigning them "None" before. */
	
    /* process digits */
    if (isdigit(str[0])) {
	if (numeric_arg >= 3267)
	    numeric_arg = 0;
	numeric_arg = 10 * numeric_arg + (str[0] - '0');
	show_message("%d", numeric_arg);
	return;
    }
    switch (str[0]) {
    case '\b':
	if (numeric_arg >= 10) {
	    show_message("%d", numeric_arg /= 10);
	    return;
	}
	/* else fall through */
    case '\033':	/* esc */
	numeric_arg = 0;
	show_message(" ");
	return;
    case '\014':	/* ctrl-L */
    case '\022':	/* ctrl-R */
	refresh_screen();
	return;
    }
    if (str[0]) {
	const char *rulechange = "XCS";
	const char *rulename[] = { "Xsok", "Cyberbox", "Sokoban" };
	const char *s;

	if ((s = strchr(rulechange, str[0]))) {
	    /* printf("found %s\n", rulename[s-rulechange]); */
	    change_rules(rulename[s-rulechange]);
	    NewLevel(1);
	    cmd_LevelInfo();
	    return;
	}
    }
}
