/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module move.c				     */
/*									     */
/*	Detection of valid moves, and effect handling functions.	     */
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

#define SAVE_STATE	/* variant which should flicker less */
#define SELECTOR_HACK	/* ugly hack to allow Cyberbox selectors */

static int dxtab[4] = { 0, -1, 0, 1 };
static int dytab[4] = { -1, 0, 1, 0 };

char *movetab = NULL;
int numalloc = 0;

/* declare the unmovable blocker which prevents race conditions */
static struct objects blocked = { 1, 0, 0, 0, 0, 0, 0, 0 };

static void storemove(int res, int dir) {   
    if (res > 1)
	++game.n_pushes;
    if (game.n_moves == numalloc)
	movetab = realloc_(movetab, numalloc += 256);
    if (game.stored_moves > game.n_moves && movetab[game.n_moves] == dir) {
	/* This is a redo, but the user retyped it. Be nice. */
	++game.n_moves;
    } else {
	/* this is a new move: reset number of stored moves (no redo beyond this one) */
	if (game.bookmark > game.n_moves)
	    game.bookmark = game.n_moves;
	movetab[game.n_moves++] = dir;
	game.stored_moves = game.n_moves;	/* reset the redo limit */
    }
}


#define DOPAINT(x1,y1,x2,y2)	if (gamegraphic) doPaint(x1,y1,x2,y2);

#ifdef SAVE_STATE
static struct walls *savemap[MAXROW][MAXCOL];
static struct objects *saveobj[MAXROW][MAXCOL];
static struct objects saveinstance[MAXINSTANCES];

static void save_state(void) {
    int x, y;
    gamegraphic = 0;
    memcpy(savemap, map, sizeof(map));	/* dereferenced objects are readonly */
    memcpy(saveinstance, instance, sizeof(instance));
    memcpy(saveobj, obj, sizeof(obj));
    for (y = 2; y < game.numrows-2; ++y)
	for (x = 2; x < game.numcols-2; ++x)
	    if (obj[y][x]) {
		int i;
		i = obj[y][x] - instance;
		saveobj[y][x] = saveinstance + i;
	    }
}

static void update_state(void) {
    int x, y;
    gamegraphic = 1;
    for (y = 2; y < game.numrows-2; ++y)
	for (x = 2; x < game.numcols-2; ++x) {
	    int pica, picb;
	    pica = obj[y][x] ? obj[y][x]->pic : -1;
	    picb = saveobj[y][x] ? saveobj[y][x]->pic : -1;
	    if (pica != picb || map[y][x] != savemap[y][x])
		doPaint(x,y, x,y);
#if 0
	    if (pica != picb) {
		printf("At (%2d,%2d): pics %d and %d differ\n", x, y, pica, picb);
	    }
#endif
	}
}
#define SAVE	save_state();
#define UPDATE	update_state();
#else
#define SAVE	gamegraphic = 0;
#define UPDATE  { gamegraphic = 1; doPaint(2,2, game.numcols-3,game.numrows-3); }
#endif

void graphics_control(int on) {
    switch (on) {
    case Disable:
	SAVE;
	break;
    case Enable:
	gamegraphic = 1;
	break;
    case EnableAndRedraw:
	UPDATE;
	break;
    }
}


/* Check if object on square (x,y) has enough power to move in direction dir */
/* If it has, do the move and return 1 or 2 (1 if object moved alone)        */

static int do_move(int x, int y, int dir) {
    struct objects *ip;
    struct walls *wp;
    int dx, dy, tx, ty;
    int dirbits, power;
#ifdef SELECTOR_HACK
    static struct objects *sel_ip = NULL;
    struct objects *new_sel_ip;
    int forbidden = 0;
    int sel_enter = 0, sel_leave = 0;

    if (obj[y][x]->pic == 7)
	sel_leave = 1;
#endif

    dx = dxtab[dir];
    dy = dytab[dir];
    dirbits = 1 << dir;
    
    tx = x;
    ty = y;

    
    wp = map[ty][tx];
    ip = obj[ty][tx];	/* what moves? */
    power = 0;
    do {
	if (ip->chr)			/* involves object already moved? */
	    return 0;
	if (!(ip->movedir & dirbits))	/* object isn't allowed to go this way */
	    return 0;
	if (ip->pushdir & dirbits)	/* does this one help pushing? */
	    power += ip->power;
	power -= ip->weight;
	if (power < 0)			/* does our power suffice? */
	    return 0;			/* it's not enough to check this at the end of the loop */
	/* may we leave from here? */
	if (!(wp->leave & dirbits))
	    return 0;	/* nope */
#ifdef SELECTOR_HACK
	if (ip->mask & forbidden) {	/* may not be pushed by THIS object */
	    switch (obj[ty-dy][tx-dx]->pic) {
	    case 0:	/* man: */
	    case 7:	/* man in selector */
		if (ip->mask == 0x30010) {	/* walkable selector */
		    sel_enter = 1;
		    new_sel_ip = ip;
		    goto move_ok;
		}
	    }
	    return 0;
	}
	forbidden = (ip->mask & 0xffff) << 16;	/* for next object */
#endif
	tx += dx;
	ty += dy;
	wp = map[ty][tx];
	if (!(wp->enter & dirbits))
	    return 0;				/* may not enter from here */
	if (!(wp->mask & ip->mask))
	    return 0;				/* may not enter square anyway */
	ip = obj[ty][tx];			/* next object, if any */
    } while (ip);

move_ok:
    ;
    /* yeah, move is OK, do it! */
    /* do the move, count blocks moved */
    {   int n;
	n = 0;
	do {
	    ++n;
	    (obj[ty][tx] = obj[ty-dy][tx-dx])->chr = 1;	/* mark object 'moved' */
	    tx -= dx;
	    ty -= dy;
	} while (tx != x || ty != y);
#ifdef SELECTOR_HACK
	if (sel_leave) {
	    obj[ty][tx] = sel_ip;
	    obj[ty+dy][tx+dx]->pic = 0;	/* normal man again */
	    sel_ip->chr = 1;
	    sel_ip = NULL;
	} else {
	    obj[ty][tx] = NULL;
	}
	if (sel_enter) {
	    sel_ip = new_sel_ip;
	    obj[ty+n*dy][tx+n*dx]->pic = 7;	/* man in a box */
	}
	DOPAINT(x, y, x+n*dx, y+n*dy);
	if (!sel_leave)
	    obj[ty][tx] = &blocked;

#else
	obj[ty][tx] = NULL;
	DOPAINT(x, y, x+n*dx, y+n*dy);
	obj[ty][tx] = &blocked;
#endif
	return n;
    }
}

static int automoves(void) {
    int x, y, flag = 0;

    for (x = game.numcols-3; x>1; --x)
	for (y = game.numrows-3; y>1; --y) {
	    struct objects *ip;
	    if ((ip = obj[y][x])) {
		if ((ip->pushdir & 1) && do_move(x, y, 0))
		    flag = 1;
		else if ((ip->pushdir & 2) && do_move(x, y, 1))
		    flag = 1;
	    }
	}
    for (x = 2; x < game.numcols-2; ++x)
	for (y = 2; y < game.numrows-2; ++y) {
	    struct objects *ip;
	    if ((ip = obj[y][x])) {
		if ((ip->pushdir & 4) && do_move(x, y, 2))
		    flag = 1;
		else if ((ip->pushdir & 8) && do_move(x, y, 3))
		    flag = 1;
	    }
	}
    return flag;	/* turn if at least one object was moved */
}

#define TURN(object, bits)	((((object) << (bits)) & 0x0f) | ((object) >> (4-(bits))))

static void check_effects(void) {
    /* check moved objects, if they entered effect squares */
    /* if so, apply effect				   */
    /* mark objects unmoved and remove the blockers	   */
    int x, y;
    for (x = 2; x < game.numcols-2; ++x)
	for (y = 2; y < game.numrows-2; ++y)
	    if (obj[y][x] && obj[y][x]->chr) {
		struct objects *ip;
		ip = obj[y][x];
		if (ip == &blocked) {
		    obj[y][x] = NULL;	/* square is free now */
		    if (map[y][x]->effect >= 2*E_ONCE)
			/* square will change now to other type */
			map[y][x] = walls + (map[y][x]->effect / E_ONCE) - 2;
		} else {
		    int effect;
		    ip->chr = '\0';
		    if ((effect = map[y][x]->effect)) {
			if (effect > E_ONCE && effect < 2 * E_ONCE)
			    map[y][x] = walls;	/* one-time effect only */
			effect %= E_ONCE;
			switch (effect) {
			case E_TURN_CCW:
			case E_TURN_180:
			case E_TURN_CW:
			    /* does a rotation affect the object? */
			    if ((ip->movedir != 0 && ip->movedir != 0x0f) ||
				(ip->pushdir != 0 && ip->pushdir != 0x0f)) {
				/* yes, it does. Are there 2 or 4 pictures for this tile? */
				if (TURN(ip->movedir, 2) == ip->movedir &&
				    TURN(ip->pushdir, 2) == ip->pushdir)
				    /* only two pictures */
				    ip->pic ^= (effect & 1);
				else
				    /* four pictures */
				    ip->pic = (ip->pic & ~3) | ((ip->pic + effect) & 3);
				ip->movedir = TURN(ip->movedir, effect);
				ip->pushdir = TURN(ip->pushdir, effect);
				    
				DOPAINT(x, y, x, y);
			    }
			    break;
			case E_ADDPOWER:
			    ++ip->power;
			    break;
			case E_SUBPOWER:
			    if (ip->power)
				--ip->power;
			    break;
			case E_TELEPORT:
			    {   int xx, yy;
				/* find a matching teleporter */
				for (xx = 2; xx < game.numcols-2; ++xx)
				    for (yy = 2; yy < game.numrows-2; ++yy)
					if ((xx != x || yy != y) &&
					    (!obj[yy][xx] || obj[yy][xx] == &blocked) &&
					    map[yy][xx]->effect % E_ONCE == E_TELEPORT) {
					    /* found a matching free teleporter */
					    obj[yy][xx] = ip;
					    if (map[yy][xx]->effect == E_TELEPORT+E_ONCE)
						map[yy][xx] = walls;
					    obj[y][x] = NULL;
					    if (gamegraphic) {
						doPaint(xx,yy,xx,yy);
						doPaint(x,y,x,y);
					    }
					    goto done;
					}
			    }
			    /* fall through if just a single teleporter, */
			    /* or no other teleporter is free: ignore! */
			done:
			    break;
			}
		    }
		} /* moved object */
	    } /* any object at all */
}

void playermove(int dir) {	/* dir is 0..3 */
    int result;
    struct objects *ip;
#if 0
    {
	int x, y;
	for (x = 2; x < game.numcols-2; ++x)
            for (y = 2; y < game.numrows-2; ++y)
		if (obj[y][x] && obj[y][x]->chr) {
		    printf("unclean object at (%d,%d) of chr %x\n", x, y, obj[y][x]->chr);
		    obj[y][x]->chr = '\0';
		}
    }
#endif
    (ip = obj[game.y][game.x])->pushdir = 1 << dir;

#ifdef SELECTOR_HACK
    if (ip->pic == 7) /* no one may push from behind */
	result = do_move(game.x, game.y, dir);
    else
#endif
    {
	int n = 0;
	/* check if there are blocks behind us that help pushing */
	/* stupid compiler, result IS initialized! */
	do
	    ++n;
	while (obj[game.y-n*dytab[dir]][game.x-n*dxtab[dir]]);
	do
	    --n;
	while (!(result = do_move(game.x-n*dxtab[dir], game.y-n*dytab[dir], dir)) && n);
	if (result && result-1 < n)
	    fatal("internal error\n");
    }
    ip->pushdir = 0;	/* this must be reset BEFORE check_effects! */

    if (result) {
	int x, y;
	storemove(result, dir);
	do {
	    if (gamegraphic)
		sync_and_wait();
	    /* add sound here too! */
	    check_effects();
	} while (automoves());
	/* player may get pushed around: search him! */
	game.x = game.y = 0;
	for (x = 2; x < game.numcols-2; ++x)
            for (y = 2; y < game.numrows-2; ++y)
		if (obj[y][x] == ip) {
		    game.y = y;
		    game.x = x;
		    break;
		}
	/* fatal("Ooops, player has vanished!\n"); */
	if (finished()) {
	    int update = 0;
	    cmd_ReadHighscores();	/* read the actual values! TODO: lock the highscore file */
	    if (highscore[game.level] < game.score) {
		highscore[game.level] = game.score;
		update = 1;
	    }
	    if (highscore[game.level+100] > game.n_moves) {
		highscore[game.level+100] = game.n_moves;
		update |= 2;
	    }
	    if (highscore[game.level+200] > game.n_pushes) {
		highscore[game.level+200] = game.n_pushes;
		update |= 4;
	    }
	    if (update) {
		const char *firstfile = NULL;
		WriteHighscores();
#if 0
		if (highscore[game.level] == game.score)
		    save_game("sol");	/* optimum score */
		else if (highscore[game.level+200] == game.n_pushes)
		    save_game("mp");	/* minimal pushes */
		else if (highscore[game.level+100] == game.n_moves)
		    save_game("mm");	/* minimal moves */
		/* TODO: should remove obsolete files and rename others. That's hard! */
#else
		/* possibly save multiple files */
		if (highscore[game.level] == game.score)
		    save_game(firstfile = "bs");	/* best score */
		if (highscore[game.level+200] == game.n_pushes)
		    firstfile ? link_game(firstfile, "mp") :
			save_game(firstfile = "mp");	/* minimal pushes */
		if (highscore[game.level+100] == game.n_moves)
		    firstfile ? link_game(firstfile, "mm") :
			save_game(firstfile = "mm");	/* minimal moves */
#endif
		show_message(TXT_NEWHIGH, game.score);
		return;
	    }
	    /* TODO: release the highscore file lock */
	    show_message(TXT_YOU_WIN, game.score, highscore[game.level]);
	    return;
	} /* if (finished()) */
	cmd_ShowScore();
    } else
	if (!game.finished)	/* hack to avoid complains on excess moves at the end */
	    show_message(TXT_MOVENOTPOSSIBLE);
}
