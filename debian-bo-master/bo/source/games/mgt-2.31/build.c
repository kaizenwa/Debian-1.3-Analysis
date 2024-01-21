/* "mgt" Copyright (c) 1991 Shodan  */

#include "mgt.h"

int lastMoveX, lastMoveY, moveNum, lastTurn;
coordList *lastletters = 0, *lastmarks;
coordList *marks = 0;
coordList *letters = 0;


FUNCTION void clearLast()
{
   lastMoveX = lastMoveY = -1;
   lastTurn = -1;
   moveNum = 0;  
   curPlayer = t_Black;
}

FUNCTION void highlightLast()
{
   (*io->highlightLast) (lastMoveX, lastMoveY, moveNum, lastTurn);
}


static void setLastMovePos(p)
property *p;
{
   Token player;

   if (p->t==t_Pass) player=p->data.player;
   else player=p->t;

   if (player==t_White){ lastTurn=1; curPlayer=t_Black;}
   else if (player==t_Black) {lastTurn=0;curPlayer=t_White;}
   else lastTurn= -1;

   if (p->t==t_Pass)
     lastMoveX = lastMoveY = PASSVAL;
   else if (p->data.stones) {
      lastMoveX = p->data.stones->x;
      lastMoveY = p->data.stones->y;
   } else
      lastMoveX = lastMoveY = -1;
}


FUNCTION void doPlace(p, b, t)
property *p;
pBoard b;
piece t;
{
   coordList *list;

   list = p->data.stones;
   while (list) {
      placeStone(b, list->x, list->y, t);
      list = list->next;
   }
   if (p->t == t_Black || p->t == t_White){
      moveNum++;
      setLastMovePos(p);
   }

}




FUNCTION void doProps(p, b)
property *p;
pBoard b;
{
   letters = NULL;
   marks = NULL;
   lastTurn= -1;
   while (p) {
      switch (p->t) {
	 case t_White:
	    BUG("t_White ");
	    doPlace(p, b, P_WHITE);
	    break;
	 case t_Black:
	    BUG("t_Black ");
	    doPlace(p, b, P_BLACK);
	    break;
	 case t_AddWhite:
	    BUG("t_AddWhite ");
	    doPlace(p, b, P_WHITE);
	    break;
	 case t_AddBlack:
	    BUG("t_AddBlack ");
	    doPlace(p, b, P_BLACK);
	    break;
	 case t_AddEmpty:
	    BUG("t_AddEmpty ");
	    doPlace(p, b, P_NOTHING);
	    break;
	 case t_Comment:
	    break;
	 case t_Pass:
	    moveNum++;
	    setLastMovePos(p);
	    break;
	 case t_Mark:
	    marks = p->data.stones;
	    break;
	 case t_Letter:
	    letters = p->data.stones;
	    BUG("t_Letter ");
	    break;
	 default:
	    BUG("default ");
      }
      p = p->next;
   }
}



FUNCTION void doPropComment(n)
nodep n;
{
   property *p;

   if (p = getprop(n, t_Comment))
      (*io->displayComment) (p->data.comment);
}


FUNCTION void buildTree0(n, b)
nodep n;
pBoard b;
{
   BUG("parent >");
   if (n) {
      buildTree0(n->parent, b);
      BUG("\nprops:");
      doProps(n->p, b);
   }
}



FUNCTION void buildTree(n, b)
nodep n;
pBoard b;
{
   prisoners[0] = 0;
   prisoners[1] = 0;
   boardClear(b);
   (*io->clearComment) ();
   BUG("buildTree:\n");
   clearLast();
   buildTree0(n, b);
   doPropComment(n);
}


FUNCTION void setPiece(b, i, j, p)
pBoard b;
int i, j;
piece p;
{
   boardSet(b, i, j, p);
   (*io->plotPiece) (b, i, j);
}

FUNCTION void updateBoard(dst, new)
pBoard dst, new;		/* update dst to look like new */
{
   int i, j;
   coordList *cl;

   for (cl = lastmarks; cl; cl = cl->next)
      (*io->plotPiece) (new, cl->x, cl->y);

   for (cl = lastletters; cl; cl = cl->next)
      (*io->plotPiece) (new, cl->x, cl->y);
   lastmarks = marks;
   lastletters = letters;
   for (i = boardsize; i--;)
      for (j = boardsize; j--;) {
	 if (boardGet(new, i, j) != boardGet(dst, i, j)) {
	    setPiece(dst, i, j, boardGet(new, i, j));
	 }
      }
   if (marks) {
      for (cl = marks; cl; cl = cl->next)
	 (*io->plotMark) (new, cl->x, cl->y);
      marks = NULL;
   }
   if (letters) {
      for (i = 0, cl = letters; cl; cl = cl->next, i++)
	 (*io->plotLetter) (cl->x, cl->y, (i % 26) + 'a');
      letters = NULL;
   }
}
