/* "mgt" Copyright (c) 1991 Shodan  */

#include "mgt.h"

int first;
nodep buffer = 0;


FUNCTION void writeStrEscaped(output, s)
FILE *output;
char *s;
{
   while (*s) {
      if (*s == ')' || *s == '(' || *s == ']' || *s == '[')
	 fputc('\\', output);
      fputc(*(s++), output);
   }
}


#define WRITE(short,long) if (saveShort) fputs(short,output);else fputs(long,output)


static char *infoshort[] =
{"PB", "BR", "PW", "WR",
 "GN", "EV", "RO", "DT", "PC", "TM", "RE", "GC",
 "SO", "US", "KM"};

static char *infolong[] =
{
   "PlayerBlack", "BlackRank", "PlayerWhite", "WhiteRank", "GameName", "EVent", "ROund",
   "DaTe", "PlaCe", "TiMe", "REsult", "GameComment", "SOurce", "USer", "KoMi"};



static void writeFirst(output)
FILE *output;
{
   int p;
   if (saveShort) {
      fprintf(output, "FF[1]GM[1]VW[]SZ[%d]", boardsize);
      if (handicap)
	 fprintf(output, "HA[%d]", handicap);
   } else {
      fprintf(output, "FileFormat[1]GaMe[1]\nVieW[]\nSiZe[%d]\n", boardsize);
      if (handicap)
	 fprintf(output, "HAndicap[%d]\n", handicap);
   }

   for (p = 0; p <= t_Komi - FIRSTINFO; p++) {
      if (info[p]) {
	 WRITE(infoshort[p], infolong[p]);
	 fputc('[', output);
	 writeStrEscaped(output, info[p]);
	 WRITE("]", "]\n");
      }
   }
}



FUNCTION void writeNode(output, n)
FILE *output;
nodep n;
{
   property *prop;
   char str[1445];

   WRITE(";", ";\n");
   if (first) {
      writeFirst(output);
      first = 0;
   }
   prop = n->p;
   while (prop) {
      switch (prop->t) {
	 case t_AddBlack:
	    if (writeCoordList(prop->data.stones, str)) {
	       WRITE("AB", "AddBlack");
	       fputs(str, output);
	    }
	    break;
	 case t_AddWhite:
	    if (writeCoordList(prop->data.stones, str)) {
	       WRITE("AW", "AddWhite");
	       fputs(str, output);
	    }
	    break;
	 case t_White:
	    if (writeCoordList(prop->data.stones, str)) {
	       WRITE("W", "White");
	       fputs(str, output);
	    }
	    break;
	 case t_Black:
	    if (writeCoordList(prop->data.stones, str)) {
	       WRITE("B", "Black");
	       fputs(str, output);
	    }
	    break;
	 case t_AddEmpty:
	    if (writeCoordList(prop->data.stones, str)) {
	       WRITE("AE", "AddEmpty");
	       fputs(str, output);
	    }
	    break;
	 case t_Mark:
	    if (writeCoordList(prop->data.stones, str)) {
	       WRITE("M", "Mark");
	       fputs(str, output);
	    }
	    break;
	 case t_Letter:
	    if (writeCoordList(prop->data.stones, str)) {
	       WRITE("L", "Letter");
	       fputs(str, output);
	    }
	    break;
	 case t_Name:
	    if (strlen(prop->data.comment)) {
	       WRITE("N[", "Name[");
	       writeStrEscaped(output, prop->data.comment);
	       WRITE("]", "]\n");
	    }
	    break;
	 case t_Pass:
	    if (prop->data.player == t_Black)
	       WRITE("B", "Black");
	    else
	       WRITE("W", "White");
	    WRITE("[tt]", "[tt]\n");
	    break;

	 case t_Player:
	    WRITE("PL[", "PLayer[");
	    if (prop->data.player == t_Black)
	       fputc('B', output);
	    else
	       fputc('W', output);
	    WRITE("]", "]\n");
	    break;
	 case t_Comment:
	    if (strlen(prop->data.comment)) {
	       WRITE("C[", "Comment[");
	       writeStrEscaped(output, prop->data.comment);
	       WRITE("]", "]\n");
	    }
	    break;
      }
      prop = prop->next;
   }
}



FUNCTION void WriteSubTree(output, root, sib)
FILE *output;
nodep root;
int sib;
{
   WRITE("(", "(\n");
   do {
      if (sib && root->nextSibling) {
	 WriteSubTree(output, root, 0);
	 while (root->nextSibling) {
	    root = root->nextSibling;
	    WriteSubTree(output, root, 0);
	 }
	 root = NULL;
      } else {
	 writeNode(output, root);
	 root = root->child;
	 sib = 1;
      }
   }
   while (root);
   WRITE(")", ")\n");
}



FUNCTION int writeCoordList(list, str)
coordList *list;
char *str;
{
   *str = 0;
   while (list) {
      sprintf(str + strlen(str), "[%c%c]", list->x + 'a', list->y + 'a');
      list = list->next;
   }
   if (!(strlen(str)))
      return 0;
   if (!saveShort)
      strcat(str, "\n");
   return 1;
}



FUNCTION int writeTree(name, root)
char *name;
nodep root;
{
   FILE *output;

   if (output = fopen(name, "w")) {
      first = 1;
      WriteSubTree(output, root, 1);
      fclose(output);
      return 0;
   } else {
      (*io->notifyError) ("Error saving file.");
      return 1;
   }
}



static void clearSpace(prop, x, y)
property *prop;
int x, y;
{
   while (prop) {
      switch (prop->t) {
	 case t_AddEmpty:
	 case t_AddBlack:
	 case t_AddWhite:
	 case t_Black:
	 case t_White:
	 case t_Mark:
	 case t_Letter:
	    clearCoord(x, y, &(prop->data.stones));
	    break;
      }
      prop = prop->next;
   }
}


FUNCTION int addMark(n, t, x, y)
nodep n;
Token t;
int x, y;
{
   property *prop;

   if (prop = getprop(n, t)) {
      if ((getCoord(x, y, prop->data.stones))) {
	 clearCoord(x, y, &(prop->data.stones));
	 return 1;
      }
   } else {
      prop = (property *) malloc(sizeof(property));
      if (!prop)
	 barf("Memory allocation failure (markStone)");
      prop->next = n->p;
      n->p = prop;
      prop->data.stones = 0;
      prop->t = t;

   }
   setCoord(x, y, &(prop->data.stones));
   return 0;
}




FUNCTION void addStone(n, t, x, y)
nodep n;
Token t;
int x, y;
{
   property *prop;

   clearSpace(n->p, x, y);
   if (!(prop = getprop(n, t))) {
      prop = (property *) calloc(1, sizeof(property));
      if (!prop)
	 barf("Memory allocation failure (addStone)");
      prop->next = n->p;
      n->p = prop;
      prop->data.stones = 0;
      prop->t = t;
   }
   setCoord(x, y, &(prop->data.stones));

}

FUNCTION int makeMove(n, t, x, y)
nodep n;
Token t;
int x, y;
{
   nodep new;
   property *prop;
   int ret;
   ret = 0;

   if (!(n->p)) {
      new = n;
      ret = 1;
   } else {

      new = newNode();
      if (n->child)
	 n->child->parent = new;
      new->parent = n;
      new->child = n->child;
      n->child = new;
   }


   prop = (property *) calloc(1, sizeof(property));
   if (!prop)
      barf("Memory allocation failure (makeMove)");
   new->p = prop;
   prop->data.stones = 0;
   prop->t = t;

   setCoord(x, y, &(prop->data.stones));
   return ret;

}

FUNCTION int passMove(n, t)
nodep n;
Token t;
{
   nodep new;
   property *prop;
   int ret;
   ret = 0;

   if (!(n->p)) {
      new = n;
      ret = 1;
   } else {

      new = newNode();
      if (n->child)
	 n->child->parent = new;
      new->parent = n;
      new->child = n->child;
      n->child = new;
   }


   prop = (property *) calloc(1, sizeof(property));
   if (!prop)
      barf("Memory allocation failure (passMove)");
   new->p = prop;
   prop->data.player = t;
   prop->t = t_Pass;

   return ret;

}


FUNCTION void addPlayer(n, t)
nodep n;
Token t;
{
   property *prop;
   if (!(prop = getprop(n, t_Player))) {
      prop = (property *) calloc(1, sizeof(property));
      if (!prop)
	 barf("Memory allocation failure (addPlayer)");
      addprop(n, prop);
   }
   prop->data.player = t;
   prop->t = t_Player;
}


FUNCTION void makeVariation(n)
nodep n;
{
   nodep new, last;

   if (!(n->child)) {
      new = newNode();
      new->parent = n;
      n->child = new;
   } else {
      new = newNode();
      new->parent = n;
      last = treeLastSibling(n->child);
      last->nextSibling = new;
      new->lastSibling = last;
   }
}


FUNCTION void cutTree(n)
nodep n;
{
   freeNode(buffer);
   if (n->nextSibling)
      n->nextSibling->lastSibling = n->lastSibling;
   if (n->lastSibling)
      n->lastSibling->nextSibling = n->nextSibling;
   else if (n->parent)
      n->parent->child = n->nextSibling;
   n->nextSibling = 0;
   n->lastSibling = 0;
   buffer = n;
}


FUNCTION boolean pasteTree(n)
nodep n;
{
   nodep last, sib;
   if (buffer) {
      for (last = buffer; last->child; last = last->child);
      if (n->child) {
	 n->child->parent = last;
	 for (sib = n->child->nextSibling; sib; sib = sib->nextSibling)
	    sib->parent = last;
	 last->child = n->child;
      }
      n->child = buffer;
      buffer->parent = n;
      buffer = (nodep) 0;
      return true;
   }
   return false;
}


FUNCTION void edComment(n)
nodep n;
{
   property *prop;

   if (!(prop = getprop(n, t_Comment))) {
      prop = (property *) calloc(1, sizeof(property));
      prop->t = t_Comment;
      prop->next = n->p;
      n->p = prop;
   }
   (*io->editComment) (prop->data.comment, &(prop->data.comment));
   if (!(prop->data.comment)) {
      n->p = prop->next;
      free(prop);
   }
}



FUNCTION void deleteNode(n)
nodep *n;
{
   nodep last;
   if (!((*n)->child)) {
      freeProps(*n);
      (*n)->p = 0;
   } else {
      if ((*n)->parent && (*n)->parent->child == *n)
	 (*n)->parent->child = (*n)->child;
      if ((*n)->lastSibling) {
	 (*n)->child->lastSibling = (*n)->lastSibling;
	 (*n)->lastSibling->nextSibling = (*n)->child;
      }
      /* new parent for all of child's sibs */

      for (last = (*n)->child; last->nextSibling; last = last->nextSibling)
	 last->parent = (*n)->parent;
      last->parent = (*n)->parent;

      /* Last of child's sibs get's to point to next sib of main node */
      last->nextSibling = (*n)->nextSibling;

      if ((*n)->nextSibling)
	 (*n)->nextSibling = last;
      last = *n;
      *n = (*n)->child;
      delNode(last);
   }
}


FUNCTION void makeName(n)
nodep n;
{
   char newname[41];
   property *prop;

   (*io->queryStr) ("Name: ", newname, 40);

   if (prop = getprop(n, t_Name))
      free(prop->data.comment);
   else {
      prop = (property *) calloc(1, sizeof(property));
      addprop(n, prop);
      prop->t = t_Name;
   }
   prop->data.comment = dupStr(newname);
}


FUNCTION void replaceComment(n, str)
nodep n;
char *str;
{
   property *prop;

   if (!(prop = getprop(n, t_Comment))) {
      prop = (property *) calloc(1, sizeof(property));
      prop->t = t_Comment;
      prop->next = n->p;
      n->p = prop;
   } else
      free(prop->data.comment);
   prop->data.comment = dupStr(str);
}
