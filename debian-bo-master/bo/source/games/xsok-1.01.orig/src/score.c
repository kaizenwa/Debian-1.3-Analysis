/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module score.c				     */
/*									     */
/*	Score computation and checking for finished levels.		     */
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

void change_rules(const char *type) {
    game.type = type;
    game.level = 0;
    ParseDefinitionFile();
}

int compute_score(void) {
    int x, y;
    int retval = 1;
    game.score = 0;
    for (x = 1; x < game.numcols; ++x)
	for (y = 1; y < game.numrows; ++y) {
	    int c;
	    c = map[y][x]->effect;
	    if (c == E_EXIT) {
		if (game.x != x || game.y != y)
		    retval = 0;	/* EXIT field with no player on it */
		else
		    game.score += obj[y][x]->score;
	    }
	    if (c == E_DEST)
		if (!obj[y][x] || !(obj[y][x]->mask & ~1))
		    retval = 0;	/* player doesn't score! */
		else
		    game.score += obj[y][x]->score;
	}
    if (retval && !objects->score)
	game.score += 10000;	/* finished-score if no special EXIT square */
    game.score -= movecost * game.n_moves + pushcost * game.n_pushes;
    if (game.score < 0)
	game.score = 0;
    return retval;
}

int finished(void) {
    if (!compute_score())
	return game.finished = 0;
    cmd_ShowScore();
    return game.finished = 1;
}
