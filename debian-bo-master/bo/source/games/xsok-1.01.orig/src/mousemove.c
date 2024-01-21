/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module mousemove.c				     */
/*									     */
/*	Computes paths to squares in the distance.			     */
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

static int before_move;

void cmd_MouseUndo(void) {
    if (lastcmd != &cmd_MouseMove && lastcmd != &cmd_MousePush && lastcmd
	!= cmd_PlayMacro) {
	cmd_UndoMove();	/* normal undo */
	return;
    }
    jumpto_movenr(before_move);
    cmd_ShowScore();
}
    
#define RBSIZE	  1024	/* only powers of 2 will work */
#ifndef sgn
#define sgn(x)		((x) < 0 ? -1 : 1)
#endif

#define STEP(dx,dy) if ( \
  (map[y+(dy)][x+(dx)]->mask & 1) && \
   map[y+(dy)][x+(dx)]->enter == 0xf && \
   map[y+(dy)][x+(dx)]->leave == 0xf && \
   !obj[y+(dy)][x+(dx)] && \
   dist[y+(dy)][x+(dx)] > d) { \
    dist[remy[wr] = y+(dy)][remx[wr]= x+(dx)] = d; \
    if ((wr = (wr + 1) & (RBSIZE-1)) == rd) break; }

void cmd_MouseMove(void) {
    int xx = mouse_x, yy = mouse_y;
    unsigned dist[MAXROW][MAXCOL];
    int rd = 0, wr = 1, remx[RBSIZE], remy[RBSIZE];
    struct objects *ip = obj[game.y][game.x];

    before_move = game.n_moves;
    /* case 1: a distance 1 click will move or push */
    if (xx == game.x && yy == game.y)
	return;	/* shortcut! */

    if (abs(xx - game.x) + abs(yy - game.y) == 1) {
	if (xx - game.x)
	    if (xx > game.x)
		playermove(3);
	    else
		playermove(1);
	else
	    if (yy > game.y)
		playermove(2);
	    else
		playermove(0);
	return;
    }
    /* for greater distance, only use free space. Therefore, we must */
    /* temporary remove the player ;-) */
    if (!(map[yy][xx]->mask & 1))
	goto nopath;
    obj[game.y][game.x] = NULL;
    memset(dist, 0xff, sizeof(dist));
    remx[0] = xx;
    remy[0] = yy;
    dist[yy][xx] = 0;
    while (rd != wr) {
	int x, y, d;
	x = remx[rd];
	y = remy[rd];
	rd = (rd+1) & (RBSIZE-1);
	d = 1 + dist[y][x];
	STEP(1,0);
	STEP(0,1);
	STEP(-1,0);
	STEP(0,-1);
	if (dist[game.y][game.x] < 30000)
	    goto have_path;
    }
    /* no path is found */
 nopath:
    obj[game.y][game.x] = ip;
    show_message(TXT_MOVENOTPOSSIBLE);
    return;

have_path:
    obj[game.y][game.x] = ip;
    while (game.y != yy || game.x != xx) {
	unsigned length;
	int omove;
	length = dist[game.y][game.x] - 1;
	omove = game.n_moves;
	     if (dist[game.y-1][game.x] == length) playermove(0);
	else if (dist[game.y][game.x-1] == length) playermove(1);
	else if (dist[game.y+1][game.x] == length) playermove(2);
	else if (dist[game.y][game.x+1] == length) playermove(3);
	if (dist[game.y][game.x] > length)
	    break;	/* else may cause cycling (with teleporters) */
	if (game.n_moves != omove + 1)
	    break;	/* precomputed move not possible any more */
    }
}

void cmd_MousePush(void) {
    int dx, dy, i, dir;

    before_move = game.n_moves;
    dx = mouse_x - game.x;
    dy = mouse_y - game.y;
    if (dx && dy) {
	cmd_MouseMove();
	return;		/* either horizontally or vertically */
    }
    if (dx) {
	i = abs(dx);
	dir = 2 + sgn(dx);
    } else {
	i = abs(dy);
	dir = 1 + sgn(dy);
    }
    while (i--)
	playermove(dir);
}
