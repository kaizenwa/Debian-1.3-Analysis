/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module parse.c				     */
/*									     */
/*	Parsing of the level subset definition files and level files.	     */
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

#define MAXABBREVS 32
static char abbrevs[MAXABBREVS][4];
static int numabbrevs;

int nwalls, nobjects, ninstances;
int pushcost, movecost;
const char *xsokdir;
struct objects objects[MAXOBJECTS];
struct walls walls[MAXWALLS];	/* wall types */
struct game game;
struct walls *map[MAXROW][MAXCOL];
struct objects *obj[MAXROW][MAXCOL];
struct objects instance[MAXINSTANCES];

static struct game init = { 1, 1, -1, -1, 0, 0, 0 };
static struct walls the_void = { '\0', 16, 0,0,0,0 };

static struct walls   *omap[MAXROW][MAXCOL];
static struct objects *oobj[MAXROW][MAXCOL];
static struct objects oinstance[MAXINSTANCES];
static struct game    ogame;

/* restore the game state for initial tableau. No graphics update */
void OrgLevel(void) {
    memcpy(map, omap, sizeof(map));
    memcpy(obj, oobj, sizeof(obj));
    memcpy(instance, oinstance, sizeof(instance));
    ogame.macroStart = game.macroStart;
    ogame.macroEnd   = game.macroEnd;
    ogame.macro_x    = game.macro_x;
    ogame.macro_y    = game.macro_y;
    ogame.bookmark = game.bookmark;
    ogame.stored_moves = game.stored_moves;
    game = ogame;
}

int maxlevel;
char levelcomment[100];
char levelauthor[100];
static int piped;

/* open the level database and read, starting at a given line */
static FILE *def_open(const char *firstline) {
    FILE *fp;
    char s[MAXXSOKDIRLEN+14];
    char shorttype[8];
    strcpy(shorttype, game.type);
    shorttype[7] = '\0';

    sprintf(s, "%s/%s.def", xsokdir, shorttype);
    if (!(fp = fopen(s, "r"))) {
	if (!(fp = zreadopen(s)))	/* uncompress on-the-fly */
	    fatal("Cannot open definition file for %s\n", game.type);
	piped = 1;
    }
    while (fgets(s, sizeof(s), fp))
	if (!strcmp(s, firstline))
	    return fp;
    fatal("No matching line found\n");
    /* this end is not reached */
    return fp;	/* but keep the compiler happy (volatile functions are not ANSI) */
}

void ParseDefinitionFile(void) {
    FILE *fp;
    int mode = 0;
    char s[MAXXSOKDIRLEN+22];

    piped = 0;
    maxlevel = 99;
    pushcost = 10;
    movecost = 1;
    sprintf(s, "%s/%s/definitions", xsokdir, game.type);
    if (!(fp = fopen(s, "r")))
	fp = def_open(";WALLS\n");
    numabbrevs = nwalls = nobjects = 0;
    while (fgets(s, sizeof(s), fp)) {
	int len;
	len = strlen(s);
	if (len && s[len-1] == '\n')
	    s[--len] = '\0';
	if (!strcmp(s, ";WALLS")) {
	    mode = 0;
	} else if (!strcmp(s, ";OBJECTS")) {
	    mode = 1;
	} else if (!strncmp(s, ";MAXLEVEL", 9)) {
	    maxlevel = atoi(s+9);
	    if (maxlevel < 1 || maxlevel > 99)
		maxlevel = 99;
	} else if (!strncmp(s, ";PUSHCOST", 9)) {
	    pushcost = atoi(s+9);
	} else if (!strncmp(s, ";MOVECOST", 9)) {
	    movecost = atoi(s+9);
	} else if (!strncmp(s, ";ATOP ", 6)) {
	    if (numabbrevs == MAXABBREVS)
		fatal("Too many abbrevs!\n");
	    strncpy(abbrevs[numabbrevs++], s+6, 3);
	} else if (!strncmp(s, ";LEVEL", 6))
	    break;
	if (!len || *s == ';')
	    continue;	/* comment or empty line */
	switch (mode) {
	case 0:
	    {   struct walls *wp;
		if (nwalls == MAXWALLS)
		    fatal("Too many wall types\n");
		wp = walls + nwalls++;
		wp->chr = *s;
		if (sscanf(s+1, "%x %x %x %x %d", &wp->pic, &wp->enter,
			   &wp->leave, &wp->mask, &wp->effect) != 5)
		    fatal("Bad line for character '%c':\n%s\n",
			  wp->chr, s);
	    }
	    break;
	case 1:
	    {   struct objects *wp;
		if (nobjects == MAXOBJECTS)
		    fatal("Too many object types\n");
		wp = objects + nobjects++;
		wp->chr = *s;
		if (sscanf(s+1, "%x %x %x %d %d %x %d", &wp->pic, &wp->movedir,
			   &wp->pushdir, &wp->weight, &wp->power, &wp->mask,
			   &wp->score) != 7)
		    fatal("Bad line for character '%c':\n%s\n",
			  wp->chr, s);
	    }
	    break;
	default:
	    ;
	}
    }
    if (piped)
	zreadclose(fp);
    else
	fclose(fp);
    if (!nwalls || !nobjects)
	fatal("Definition file is empty\n");
}


static void dfs(int x, int y) {
    if (x < 0 || y < 0 || x >= game.numcols || y >= game.numrows)
	return;
    if (map[y][x] != walls)
	return;
    map[y][x] = &the_void;
    dfs(x-1, y);
    dfs(x, y-1);
    dfs(x+1, y);
    dfs(x, y+1);
}

/* read current level of current game.type */

void ParseMapFile(void) {
    FILE *fp;
    int x, y;
    struct objects *ip;
    char s[MAXXSOKDIRLEN+20];

    piped = 0;
    sprintf(s, "%s/%s/screen.%02d", xsokdir, game.type, game.level);
    if (!(fp = fopen(s, "r"))) {
	sprintf(s, ";LEVEL %d\n", game.level);
	fp = def_open(s);
    }
    ninstances = 0;
    for (x = 0; x < MAXCOL; ++x)
	for (y = 0; y < MAXROW; ++y) {
	    map[y][x] = walls;
	    obj[y][x] = NULL;
	}
    init.level = game.level;
    init.type = game.type;
    *levelcomment = '\0';
    *levelauthor = '\0';
    game = init;
    ip = instance;
    for (y = 1; fgets(s, sizeof(s), fp); ++y) {
	int c, cc;
	/* parse s to map */
	if (*s == ';') {
	    if (!strncmp(s+1, "LEVEL", 5))
		break;
	    --y;	/* line doesn't count */
	    if (!strncmp(s+1, "COMMENT ", 8)) {
		strncpy(levelcomment, s+9, sizeof(levelcomment)-1);
		levelcomment[sizeof(levelcomment)-1] = '\0';
		if (strchr(levelcomment, '\n'))
		    *strchr(levelcomment, '\n') = '\0';
	    }
	    if (!strncmp(s+1, "AUTHOR ", 7)) {
		strncpy(levelauthor, s+8, sizeof(levelauthor)-1);
		levelauthor[sizeof(levelauthor)-1] = '\0';
		if (strchr(levelauthor, '\n'))
		    *strchr(levelauthor, '\n') = '\0';
	    }
	    continue;	/* skip this line (extra comment) */
	}
	if (y == MAXROW-1)
	    fatal("Level is too big\n");
	for (x = 1; (c=s[x-1]) && c != '\n'; ++x) {
	    struct objects *op;
	    struct walls *wp;
	    int n;
	    if (x == MAXCOL-1)
		fatal("Level is too wide\n");
	    if (x > game.numcols)
		game.numcols = x;
	    /* check, if c is an abbrev */
	    cc = walls[0].chr;	/* make standard floor below (should be space) */
	    for (n = 0; n < numabbrevs; ++n)
		if (abbrevs[n][0] == c) {
		    c = abbrevs[n][1];
		    cc = abbrevs[n][2];
		}
	    for (op = objects, n = nobjects; n; --n, ++op)
		if (op->chr == c) {	/* object found */
		    if (op == objects) {	/* player himself */
			if (game.x >= 0)
			    fatal("Multiple player positions\n");
			game.x = x;
			game.y = y;
		    }
		    if (ninstances++ == MAXINSTANCES)
			fatal("Too many objects\n");
		    *ip = *op;	/* copy object */
		    ip->chr = '\0';	/* mark unmoved object */
		    obj[y][x] = ip++;
		    c = cc;		/* floor type */
		    break;
		}
	    for (wp = walls, n = nwalls; n; --n, ++wp)
		if (wp->chr == c) {	/* wall found */
		    map[y][x] = wp;
		    break;
		}
	    if (!n)
		fatal("No wall type found for character '%c'\n", c);
	}
    }
    if (piped)
	zreadclose(fp);
    else
	fclose(fp);
    game.numrows = y+1;
    game.numcols += 2;
    /* change floor to void */
    dfs(0, 0);

    /* save to orglevel for easy restart */
    memcpy(omap, map, sizeof(map));
    memcpy(oobj, obj, sizeof(obj));
    memcpy(oinstance, instance, sizeof(instance));
    game.macroStart = -1;	/* no macro available */
    ogame = game;
    cmd_ReadHighscores();
    lastcmd = NULL;
}
