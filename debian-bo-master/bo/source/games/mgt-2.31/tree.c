/* "mgt" Copyright (c) 1991 Shodan  */

#include "mgt.h"


static int newNodeNum;


FUNCTION boolean getCoord(x, y, list)
int x, y;
coordList *list;
{
   for (; list; list = list->next)
      if (list->x == x && list->y == y)
	 return true;
   return false;
}



FUNCTION coordList *addCoord(x, y)
int x, y;
{
   coordList *co;

   if (!(co = (coordList *) malloc(sizeof(coordList))))
      barf("Memory allocation failure (addCoord)");
   co->next = 0;
   co->x = x;
   co->y = y;
   return co;
}



FUNCTION void setCoord(x, y, startlist)
int x, y;
coordList **startlist;
{
   coordList *list;
   if (!*startlist)
      *startlist = addCoord(x, y);
   else {
      for (list = *startlist; list->next; list = list->next)
	 if (x == list->x && y == list->y)
	    return;
      list->next = addCoord(x, y);
   }
}


FUNCTION void clearCoord(x, y, startlist)
int x, y;
coordList **startlist;

{
   coordList *last, *list;

   last = 0;
   list = *startlist;
   while (list && (x != list->x || y != list->y)) {
      last = list;
      list = list->next;
   }
   if (list) {
      if (last)
	 last->next = list->next;
      else
	 *startlist = list->next;
      free(list);
   }
}


FUNCTION void initNodes()
{
   newNodeNum = 0;
}

FUNCTION nodep newNode()
{
   nodep new;
#ifdef DEBUG
   totalmemory += sizeof(node);
   fprintf(debug, "%ld %ld\n", totalmemory, coreleft());
#endif
   new = (nodep) calloc(1, sizeof(node));
   if (!new)
      barf("Memory allocation failure (newnode)");
   new->nodeNum = newNodeNum++;
   return new;
}

FUNCTION void freeNode(n)
nodep n;
{
   if (n) {
      freeNode(n->nextSibling);
      freeNode(n->child);
      delNode(n);
   }
}

FUNCTION char *dupStr(s)
char *s;
{
   char *c;
#ifdef DEBUG
   totalmemory += strlen(s) + 1;
   fprintf(debug, "%ld %ld\n", totalmemory, coreleft());
#endif
   c = (char *) malloc((unsigned) strlen(s) + 1);
   if (!c)
      barf("Memory allocation failure (dupstr)");
   strcpy(c, s);
   return c;
}



FUNCTION void freeProps(n)
nodep n;
{
   property *prop, *lastprop;
   coordList *cl, *lastcl;
   prop = n->p;
   while (prop) {
      switch (prop->t) {
	 case t_AddEmpty:
	 case t_AddBlack:
	 case t_AddWhite:
	 case t_Mark:
	 case t_Letter:
	 case t_Black:
	 case t_White:
	    cl = prop->data.stones;
	    while (cl) {
	       lastcl = cl;
	       cl = cl->next;
	       free(lastcl);
	    }
	    break;
	 case t_Name:
	 case t_Comment:
	    free(prop->data.comment);
	    break;

      }

      lastprop = prop;
      prop = prop->next;
      free(lastprop);
   }
}


FUNCTION void delNode(n)
nodep n;
{
   freeProps(n);
   free(n);
}

FUNCTION void addprop(n, p)
nodep n;
property *p;
{
   p->next = n->p;
   n->p = p;
}

FUNCTION property *getprop(n, t)
nodep n;
Token t;
{
   property *p;
   p = n->p;
   while (p && p->t != t)
      p = p->next;
   return p;
}

FUNCTION int treeCountSiblings(n)
nodep n;
{
   int i;
   nodep n1;

   n1 = n->child;
   i = 0;
   while (n1) {
      n1 = n1->nextSibling;
      i++;
   }
   return i;
}

FUNCTION nodep nthChild(n, c)	/* nodep, int */
nodep n;
int c;
{

   if (n->child) {
      n = child(n);
      while (c-- && n)
	 n = nextSibling(n);
   } else {
      n = 0;
   }
   return n;
}

/* TREE WALK functions KEEP TRACK OF DEPTH AS WELL */

FUNCTION nodep parent(n)
nodep n;
{
   return n->parent;
}

FUNCTION nodep child(n)
nodep n;
{
   return n->child;
}

FUNCTION nodep lastSibling(n)
nodep n;
{
   return n->lastSibling;
}

FUNCTION nodep nextSibling(n)
nodep n;
{
   return n->nextSibling;
}

FUNCTION nodep treeLastSibling(n)
nodep n;
{
   while (n->nextSibling) {
      n = n->nextSibling;
   }
   return n;
}

/* go to next node down the tree. Stop if at bottom. */
FUNCTION nodep treeDown(n)
nodep n;
{
   if (n->child) {
      BUG("treeDown: going down\n");
      return child(n);
   } else {
      BUG("treeDown: stop\n");
      return n;
   }
}

/* backup, stop if at top */
FUNCTION nodep treeUp(n)
nodep n;
{
   if (n->parent) {
      BUG("treeUp: going up\n");
      return parent(n);
   } else {
      BUG("treeUp: staying\n");
      return n;
   }
}


/* go to next node backing up the tree only */
FUNCTION nodep treeNextUp(n)
nodep n;
{
   if (n->nextSibling) {
      BUG("nextSibling ");
      return nextSibling(n);
   } else if (n->parent) {
      BUG("nextup-parent ");
      while (n->parent && !n->nextSibling)
	 n = parent(n);
      if (n->nextSibling)
	 return nextSibling(n);
      else
	 return n;
   } else if (n->child) {
      BUG("child ");
      return child(n);
   } else {
      BUG("current ");
      return n;
   }
}


/* go to next node, backup if neccessary */
FUNCTION nodep treeNext(n)
nodep n;
{
   nodep r;

   BUG("treeNext:");
   if (n != (r = treeDown(n))) {
      BUG("going down ");
   } else {
      BUG("going up ");
      r = treeNextUp(n);
   }
   BUG("\n");
   return r;
}



FUNCTION nodep lastChildOfLastSibling(n)
nodep n;
{

   while (n->nextSibling)
      n = nextSibling(n);
   if (n->child)
      return lastChildOfLastSibling(child(n));
   else
      return n;
}

/* go to next node, backup if neccessary */
FUNCTION nodep treeLast(n)
nodep n;
{
   nodep r;

   if (n->lastSibling) {
      n = lastSibling(n);
      if (n->child)
	 r = lastChildOfLastSibling(child(n));
      else
	 r = n;
   } else if (n->parent) {
      r = parent(n);
   } else if (n->child) {
      r = lastChildOfLastSibling(child(n));
   } else {
      r = n;
   }
   return r;
}
