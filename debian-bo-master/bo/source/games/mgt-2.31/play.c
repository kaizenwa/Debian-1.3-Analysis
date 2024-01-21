/* "mgt" Copyright (c) 1991 Shodan  */


#include "mgt.h"
#include <string.h>
#include <stdio.h>

/* void printboard(char *s,pBoard b) { FILE *fil; int i,j;
 * fil=fopen("dump","a+"); if (!fil) {printf("can't open file");getch();}
 * fprintf(fil,"%s\n",s); for(i=0;i<boardsize;i++){ for(j=0;j<boardsize;j++)
 * fputc( b->b[i][j]==P_NOTHING?'.':(b->b[i][j]==P_BLACK?'*':'O'),fil);
 * fputc('\n',fil); } fclose(fil); } */




FUNCTION int legal(b, n, player, i, j)
pBoard b;
nodep n;
int i, j;
Token player;
{
   board a, copy;
   int same, x, y, savex, savey, savemove;
   boolean islegal;
   extern int moveNum;
   int savepris[2];
   Token savecurPlayer;

   savemove = moveNum;
   savecurPlayer = curPlayer;
   savex = xcur;
   savey = ycur;
   savepris[0] = prisoners[0];
   savepris[1] = prisoners[1];
   copyBoard(b, &copy);

   islegal = true;

   if (b->b[i][j] != P_NOTHING) {
      (*io->notifyError) ("There's already a piece there.");
      return false;
   }
   placeStone(&copy, i, j, (player == t_Black) ? P_BLACK : P_WHITE);
   if (!alive(&copy, i, j)) {
      (*io->notifyError) ("That move is suicide.");
      islegal = false;
   }
   if (islegal && (n = n->parent)) {
      boardClear(&a);
      buildTree0(n, &a);
      same = true;
      for (x = boardsize; same && x--;)
	 for (y = boardsize; same && y--;) {
	    same = (copy.b[x][y] == a.b[x][y]);
	 }
      if (same) {
	 (*io->notifyError) ("Can't retake the ko yet.");
	 islegal = false;
      }
   }
   xcur = savex;
   ycur = savey;
   prisoners[0] = savepris[0];
   moveNum = savemove;
   prisoners[1] = savepris[1];
   curPlayer=savecurPlayer;

   return islegal;
}




FUNCTION boolean inRange(i, j)
{
   return i >= 0 && i < boardsize && j >= 0 && j < boardsize;
}

FUNCTION boolean alive0(b, m, i, j, t)
pBoard b;
pBoard m;
int i, j;
piece t;
{
   piece pt;

   pt = b->b[i][j];
   if ((pt != P_NOTHING && pt != t) || m->b[i][j] != P_NOTHING)
      return 0;
   m->b[i][j] = (pt == t) ? (piece) 1 : (piece) 2;
   if (pt == P_NOTHING)
      return 1;
   return
      (j < boardsize - 1 && alive0(b, m, i, j + 1, t)) ||
      (i < boardsize - 1 && alive0(b, m, i + 1, j, t)) ||
      (i && alive0(b, m, i - 1, j, t)) ||
      (j && alive0(b, m, i, j - 1, t));
}

FUNCTION boolean alive(b, i, j)	/* Does group at i,j have liberties? */
pBoard b;
int i, j;
{
   board m;

   boardClear(&m);
   return alive0(b, &m, i, j, b->b[i][j]);
}

void removeStones0(b, i, j, t)
pBoard b;
int i, j;
piece t;
{
   if (b->b[i][j] != t)
      return;
   b->b[i][j] = P_NOTHING;
   prisoners[(int) t - 1]++;
   if (j < boardsize - 1)
      removeStones0(b, i, j + 1, t);
   if (i < boardsize - 1)
      removeStones0(b, i + 1, j, t);
   if (i)
      removeStones0(b, i - 1, j, t);
   if (j)
      removeStones0(b, i, j - 1, t);
}



FUNCTION void removeStones(b, i, j)
pBoard b;
int i, j;
{
   removeStones0(b, i, j, b->b[i][j]);
}

FUNCTION boolean tryKill(b, i, j, t)
pBoard b;
int i, j;
piece t;
{
   piece w;
   if (!inRange(i, j))
      return false;
   w = b->b[i][j];
   if (w != P_NOTHING && w != t && !alive(b, i, j)) {
      removeStones(b, i, j);
      return true;
   }
   return false;
}

FUNCTION void placeStone(b, i, j, t)
pBoard b;
int i, j;
piece t;
{
   if (inRange(i, j)) {
      b->b[i][j] = t;
      xcur = i;
      ycur = j;
      if (j)
	 tryKill(b, i, j - 1, t);
      if (j < boardsize - 1)
	 tryKill(b, i, j + 1, t);
      if (i)
	 tryKill(b, i - 1, j, t);
      if (i < boardsize - 1)
	 tryKill(b, i + 1, j, t);
   }
}

FUNCTION void boardSet(b, i, j, p)
pBoard b;
int i, j;
piece p;
{
   b->b[i][j] = p;
}

FUNCTION piece boardGet(b, i, j)
pBoard b;
int i, j;
{
   return b->b[i][j];
}


FUNCTION void boardClear(b)
pBoard b;
{
   memset((char *) (b->b), 0, 361 * sizeof(piece));

   /* int i, j; for (i = boardsize; i--;) for (j = boardsize; j--;) b->b[i][j]
    * = P_NOTHING; */
}

FUNCTION void copyBoard(a, b)
pBoard a, b;
{
   int i, j;
   for (i = boardsize; i--;)
      for (j = boardsize; j--;)
	 b->b[i][j] = a->b[i][j];
}


int look(stone, array, index, i)
int stone, array[], *index, i;
{
   if (stone == P_NOTHING)
      array[(*index)++] = i;
   else if (stone == P_BLACK)
      return 1;
   else if (stone == P_WHITE)
      return 2;
   return 0;
}


int fillregion(b, x, y)
pBoard b;
int x, y;
{
   int goup[19], godown[19], up = 0, down = 0, found = 0, i;

   for (i = x; i < boardsize && b->b[i][y] == P_NOTHING; i++) {
      b->b[i][y] = P_CHECKED;
      if (y > 0)
	 found |= look(b->b[i][y - 1], goup, &up, i);
      if (y < boardsize - 1)
	 found |= look(b->b[i][y + 1], godown, &down, i);
   }
   if (i != boardsize) {
      if (b->b[i][y] == P_BLACK)
	 found |= 1;
      if (b->b[i][y] == P_WHITE)
	 found |= 2;
   }
   for (i = x - 1; i >= 0 && b->b[i][y] == P_NOTHING; i--) {
      b->b[i][y] = P_CHECKED;
      if (y > 0)
	 found |= look(b->b[i][y - 1], goup, &up, i);
      if (y < boardsize - 1)
	 found |= look(b->b[i][y + 1], godown, &down, i);
   }
   if (i != -1) {
      if (b->b[i][y] == P_BLACK)
	 found |= 1;
      if (b->b[i][y] == P_WHITE)
	 found |= 2;
   }
   while (up)
      found |= fillregion(b, goup[--up], y - 1);
   while (down)
      found |= fillregion(b, godown[--down], y + 1);
   return found;
}


FUNCTION void scoreBoard(b, score)
pBoard b;
int score[];
{
   int x, y, i, j, owner;
   piece newval;

   score[0] = 0;
   score[1] = 0;
   for (i = 0; i < boardsize; i++)
      for (j = 0; j < boardsize; j++) {
	 owner = fillregion(b, i, j);
	 if (!owner)
	    owner = 3;
	 switch (owner) {
	    case 3:
	       newval = P_DAME;
	       break;
	    case 2:
	       newval = P_WHITETERR;
	       break;
	    case 1:
	       newval = P_BLACKTERR;
	       break;
	 }
	 for (x = 0; x < boardsize; x++)
	    for (y = 0; y < boardsize; y++)
	       if (b->b[x][y] == P_CHECKED) {
		  b->b[x][y] = newval;
		  if (owner != 3)
		     score[owner - 1]++;
	       }
      }
}
