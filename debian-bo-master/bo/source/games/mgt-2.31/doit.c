/* "mgt" Copyright (c) 1991 Shodan  */

#include "mgt.h"

int xcur, ycur;
Token curPlayer;
int change;
int madechanges;
int searchNodeNum;
int ispl = 0;

FUNCTION int okChange()
{
   if (tutor) {
      (*io->notifyError) ("Can't edit in tutor mode.");
      return 0;
   }
   if ((!change) && ((*io->askYN) ("Modify tree", 0)))
      change = 1;
   if (change)
      madechanges = 1;
   return change;
}



FUNCTION int okExit(root)
nodep root;
{
   if (mailFlag) {
      if (madechanges) {
	 if (!(*io->askYN) ("Save", 1)) {
	    retVal = 1;
	    return 1;
	 }
	 if (writeTree(saveName, root))
	    return 0;
	 return 1;
      }
      retVal = 1;
      return 1;
   }
   if (madechanges) {
      if ((*io->askYN) ("Unsaved changes.  Exit without saving", 0))
	 return 1;
      return 0;
   }
   return 1;
}



FUNCTION nodep search(n)	/* return 0 on failure */
nodep n;
{
   nodep s;
   if (!n || n->nodeNum == searchNodeNum)
      return n;
   s = search(child(n));
   if (s && s->nodeNum == searchNodeNum)
      return s;
   s = search(nextSibling(n));
   if (s && s->nodeNum == searchNodeNum)
      return s;
   return (nodep) 0;
}

FUNCTION void step(n, b)
nodep n;
pBoard b;
{
   board temp;
   buildTree(n, &temp);
   updateBoard(b, &temp);
   (*io->drawTree) (n);
   (*io->refreshIO) ();
   highlightLast();
}

FUNCTION void stepDown(n, b)
nodep n;
pBoard b;
{
   board temp;
   (*io->clearComment) ();
   copyBoard(b, &temp);
   doProps(n->p, &temp);
   doPropComment(n);
   updateBoard(b, &temp);
   (*io->drawTree) (n);
   (*io->refreshIO) ();
   highlightLast();
}


static char *color[] =
{
   "Black", "White"
};


int adjust(val, halfk, ksign)
int val, halfk, ksign;
{
   if (!halfk)
      return val;
   if (val * ksign >= 0)
      return val;
   return val + ksign;
}


FUNCTION void doScore(b, curNode)
pBoard b;
nodep curNode;
{
   int savepris[2], lastpris[2];
   char out[200];
   command c;
   property *p;
   board new, old, last;
   int score[2], scored;
   char komicopy[10];
   int intkomi, halfkomi, kptr, komisign, winner, diff;

   winner = -1;
   scored = 0;
   lastpris[1] = savepris[1] = prisoners[1];
   lastpris[0] = savepris[0] = prisoners[0];
   copyBoard(b, &old);
   copyBoard(b, &last);
   copyBoard(b, &new);
   (*io->notifyClear) ();
   (*io->notifyMessage) ("return score, space kill, u undo, q quit");
   do {
      c = (command) (*io->getPoint) ();
      if (c == C_MOVE && (new.b[xcur][ycur] == P_WHITE ||
			  new.b[xcur][ycur] == P_BLACK)
	 ) {
	 if (scored) {
	    scored = 0;
	    copyBoard(&last, &new);
	    prisoners[1] = lastpris[1];
	    prisoners[0] = lastpris[0];
	 }
	 copyBoard(&new, &last);
	 lastpris[1] = prisoners[1];
	 lastpris[0] = prisoners[0];
	 removeStones(&new, xcur, ycur);
	 updateBoard(b, &new);
      }
      if (c == C_UNDO) {
	 copyBoard(&last, &new);
	 prisoners[1] = lastpris[1];
	 prisoners[0] = lastpris[0];
	 updateBoard(b, &new);
      }
      if (c == C_SCORE && !scored) {
	 copyBoard(&new, &last);
	 lastpris[1] = prisoners[1];
	 lastpris[0] = prisoners[0];
	 scoreBoard(&new, score);
	 updateBoard(b, &new);
	 if (info[t_Komi - FIRSTINFO]) {
	    halfkomi = 0;
	    for (kptr = 0; info[t_Komi - FIRSTINFO][kptr]; kptr++) {
	       if (info[t_Komi - FIRSTINFO][kptr] == '.') {
		  kptr++;
		  if (info[t_Komi - FIRSTINFO][kptr] >= '1' && info[t_Komi - FIRSTINFO][kptr] <= '9')
		     halfkomi = 1;
		  break;
	       }
	       komicopy[kptr] = info[t_Komi - FIRSTINFO][kptr];
	    }
	    komicopy[kptr] = 0;
	    intkomi = atoi(komicopy);
	    komisign = 0;
	    if (komicopy[0] == '-' || intkomi < 0)
	       komisign = -1;
	    else if (intkomi > 0 || halfkomi)
	       komisign = 1;
	    sprintf(komicopy, "+ %s ", info[t_Komi - FIRSTINFO]);
	 } else {
	    komisign = intkomi = halfkomi = 0;
	    strcpy(komicopy, "");
	 }

	 diff = score[1] + prisoners[0] - score[0] - prisoners[1] + intkomi;

	 if (!diff) {
	    if (halfkomi) {
	       if (komisign > 0)
		  winner = 1;
	       else if (komisign < 0)
		  winner = 0;
	    } else
	       winner = 2;
	 } else
	    winner = diff > 0 ? 1 : 0;


	 diff = adjust(diff, halfkomi, komisign);
	 sprintf(out,
		 "Black: %d + %d = %d\n\nWhite: %d + %d %s= %s%d%s\n\n",
		 score[0], prisoners[1], score[0] + prisoners[1],
		 score[1], prisoners[0], komicopy, (!diff) && halfkomi && (komisign == -1) ? "-" : "",
		 adjust(score[1] + prisoners[0] + intkomi, halfkomi, komisign),
		 halfkomi ? ".5" : "");
	 if (winner == 2)
	    sprintf(out + strlen(out), "Tie game.");
	 else
	    sprintf(out + strlen(out), "%s wins by %d%s",
		    color[winner], abs(diff), halfkomi ? ".5" : "");


	 (*io->clearComment) ();
	 (*io->displayComment) (out);
	 scored = 1;
      }
   }
   while (c != C_QUIT);

   if (scored && (*io->askYN) ("Keep comment", 1) && okChange())
      replaceComment(curNode, out);
   else {
      (*io->clearComment) ();
      if (p = getprop(curNode, t_Comment))
	 (*io->displayComment) (p->data.comment);
   }

   prisoners[0] = savepris[0];
   prisoners[1] = savepris[1];

   (*io->notifyClear) ();
   updateBoard(b, &old);
}



static void initBoard(b)
pBoard b;
{
   extern coordList *lastmarks, *lastletters;

   boardClear(b);
   (*io->initializeBoard) ();
   lastletters = lastmarks = 0;
}



FUNCTION nodep loadFile(filename, root, b)
char *filename;
nodep root;
board *b;
{
   openfile(filename);
   if (input) {
      madechanges = change = 0;
      freeNode(root);
      initNodes();
      readInit();
      root = parse(0);
      fclose(input);
      if (!root)
	 root = newNode();
      initBoard(b);
      (*io->setCursor) (xcur, ycur);
      (*io->refreshIO) ();
      xcur = ycur = 0;
   } else
      (*io->notifyError) ("Unable to load file.");
   return root;
}


FUNCTION nodep makeTutor(nod, tok, x, y)
nodep nod;
Token tok;
int x, y;
{
   property *prop;
   if (!nod->child)
      return 0;
   nod = nod->child;
   while (!((prop = getprop(nod, tok)) && getCoord(x, y, prop->data.stones))) {
      if (!nod->nextSibling)
	 return 0;
      nod = nod->nextSibling;
   }
   return nod;
}





static char *infotitle[] =
{
   "Game name: ",
   "Event: ",
   "rouNd: ",
   "Date: ",
   "Place: ",
   "Time limit: ",
   "Result: ",
   "Comment: ",
   "sOurce: ",
   "User: ",
   "Komi: "
};




static void showinfo()
{
   char infoarray[4000];
   char *infostr;
   int p;

   infostr = infoarray;

   sprintf(infostr, "Size: %d", boardsize);
   while (*infostr)
      infostr++;
   if (info[(int) t_Komi - FIRSTINFO]) {
      sprintf(infostr, "  Komi: %s", info[(int) t_Komi - FIRSTINFO]);
      while (*infostr)
	 infostr++;
   }
   if (handicap) {
      sprintf(infostr, "  Handicap: %d", handicap);
      while (*infostr)
	 infostr++;
   }
   *infostr = '\n';
   infostr++;

   if (info[0] || info[1]) {
      sprintf(infostr, "Black: %s%s%s\n", info[0] ? info[0] : "",
	      info[0] && info[1] ? ", " : "",
	      info[1] ? info[1] : "");
      while (*infostr)
	 infostr++;
   }
   if (info[2] || info[3]) {
      sprintf(infostr, "White: %s%s%s\n", info[2] ? info[2] : "",
	      info[2] && info[3] ? ", " : "",
	      info[3] ? info[3] : "");
      while (*infostr)
	 infostr++;
   }
   for (p = 0; p <= LASTINFO - FIRSTINFO - 4; p++) {
      if (info[p + 4]) {
	 sprintf(infostr, "%s%s\n", infotitle[p], info[p + 4]);
	 while (*infostr)
	    infostr++;
      }
   }
   *infostr = 0;
   (*io->displayInfo) (infoarray);

}


#define BUFFERSIZE 50


static int doinfo()
{
   Token change;
   int sizechanged, value;
   char buffer[BUFFERSIZE + 1];
   char *current;

   sizechanged = 0;
   do {
      showinfo();
      change = (Token) (*io->getInfoToChange) ();
      if (change == t_EOF)
	 break;
      if (!okChange())
	 continue;
      if (change == t_Size) {
	 sprintf(buffer, "%d", boardsize);
	 current = dupStr(buffer);
	 (*io->editInfo) (current, &current, t_Size);
	 if (current) {
	    value = atoi(current);
	    if (value > 1 && value < 20) {
	       if (value != boardsize)
		  sizechanged = 1;
	       boardsize = value;
	    } free(current);
	 }
      } else if (change == t_Handicap) {
	 sprintf(buffer, "%d", handicap);
	 current = dupStr(buffer);
	 (*io->editInfo) (current, &current, t_Handicap);
	 if (current) {
	    handicap = atoi(current);
	    free(current);
	 }
      } else {

	 (*io->editInfo) (info[(int) change - FIRSTINFO], &info[(int) change - FIRSTINFO],
			  change);
	 if (!strlen(info[(int) change - FIRSTINFO])) {
	    free(info[(int) change - FIRSTINFO]);
	    info[(int) change - FIRSTINFO] = 0;
	 }
      }


   } while (1);
   return sizechanged;

}




static void savescreen(bord)
board *bord;
{
   char fname[75];
   int i, j;
   FILE *out;
   extern coordList *lastletters;
   coordList *let;
   char local[19][19];

   if ((*io->queryStr) ("Save screen: ", fname, 74)) {
      if (out = fopen(fname, "a+t")) {
	 fprintf(out, "%s to play.\n\n", curPlayer == t_Black ? "Black" : "White");
	 fputs("  ", out);
	 for (i = 0; i < boardsize; i++) {
	    fputc(' ', out);
	    fputc(i + 'A' + ((i + 'A' >= 'I') ? 1 : 0), out);
	 }
	 fputc('\n', out);


	 for (j = 0; j < boardsize; j++)
	    for (i = 0; i < boardsize; i++) {
	       local[i][j] = ' ';
	       switch (bord->b[i][j]) {

		  case P_WHITE:
		     local[i][j] = 'O';
		     break;
		  case P_BLACK:
		     local[i][j] = '#';
		     break;

		  case P_NOTHING:
		  default:
		     local[i][j] = '.';
		     break;
	       }
	    }
	 let = lastletters;
	 i = 0;
	 while (let) {
	    local[let->x][let->y] = i + 'a';
	    i++;
	    i %= 26;
	    let = let->next;


	 }

	 for (j = 0; j < boardsize; j++) {
	    fprintf(out, "%2d ", boardsize - j);
	    for (i = 0; i < boardsize; i++) {
	       fputc(local[i][j], out);
	       fputc(' ', out);
	    }

	    fprintf(out, "%2d", boardsize - j);
	    if (
		  (j == boardsize / 2) && (prisoners[0] || prisoners[1]))
	       fprintf(out, "  Captured #: %d   Captured O: %d", prisoners[0], prisoners[1]);
	    fputc('\n', out);
	 }
	 fputs("  ", out);
	 for (i = 0; i < boardsize; i++) {
	    fputc(' ', out);
	    fputc(i + 'A' + ((i + 'A' >= 'I') ? 1 : 0), out);
	 }

	 fputs("\n\n", out);
	 for (i = 0; i < commentLines(); i++) {
	    fputs(commentGet(i), out);
	    putc('\n', out);
	 }

	 fclose(out);
      } else
	 (*io->notifyError) ("Unable to open file.");
   }
}





FUNCTION void doit()
{
   int quitflg, i;
   nodep root, curNode;
   board theBoard;
   extern coordList *lastletters, *lastmarks;

   curPlayer = t_Black;
   xcur = ycur = quitflg = 0;
   root = 0;
   madechanges = 0;
   change = 1;
   if (input != stdin) {
      root = parse(0);
      fclose(input);
      change = 0;
   }
   if (!root)
      root = newNode();
   curNode = root;
   initBoard(&theBoard);
   (*io->setCursor) (xcur, ycur);
   (*io->refreshIO) ();
   if (mailFlag) {
      while (curNode->child)
	 curNode = treeDown(curNode);
      change = 1;
   }
   step(curNode, &theBoard);
   while (!quitflg) {
      nodep tNode;
      command c;
      property *prop;

      if (prop = getprop(curNode, t_Player)) {
	 curPlayer = prop->data.player;
	 ispl = 1;
      } else
	 ispl = 0;

      c = (command) (*io->idle) (curNode);

      if ((int) c >= (int) C_CHOSECHILD && (int) c < (int) C_NEXTCMD) {
	 nodep new;
	 new = nthChild(curNode, (int) c - (int) C_CHOSECHILD);
	 if (new) {
	    curNode = nthChild(curNode, (int) c - (int) C_CHOSECHILD);
	    step(curNode, &theBoard);
	 }
      } else
	 switch (c) {
	    case C_LOAD:
	       if (okExit(root)) {
		  char filename[50];
		  if ((*io->queryStr) ("Load file? ", filename, 48)) {
		     root = curNode = loadFile(filename, root, &theBoard);
		     step(curNode, &theBoard);
		  }
	       }
	       break;
	    case C_BACKFILE:

	       if (currentfile > 0 && okExit(root)) {
		  currentfile--;
		  root = curNode = loadFile(files[currentfile], root, &theBoard);
		  step(curNode, &theBoard);
	       } else
		  (*io->notifyError) ("No previous file.");
	       break;


	    case C_NEXTFILE:
	       if (currentfile + 1 < filecount && okExit(root)) {
		  currentfile++;
		  root = curNode = loadFile(files[currentfile], root, &theBoard);
		  step(curNode, &theBoard);
	       } else
		  (*io->notifyError) ("No next file.");
	       break;

	    case C_INFO:
	       if (doinfo())
		  initBoard(&theBoard);
	       step(curNode, &theBoard);
	       if (xcur >= boardsize)
		  xcur = boardsize - 1;
	       if (ycur >= boardsize)
		  ycur = boardsize - 1;
	       break;
	    case C_SAVESCREEN:
	       savescreen(&theBoard);
	       break;
	    case C_REDRAW:
	       {
		  int savex, savey;
		  savex = xcur;
		  savey = ycur;
		  initBoard(&theBoard);
		  (*io->refreshIO) ();
		  step(curNode, &theBoard);
		  xcur = savex;
		  ycur = savey;
		  break;
	       }
	    case C_NOTHING:
	       break;
	    case C_TOPLAY:
	       if (okChange())
		  addPlayer(curNode, (curPlayer == t_Black) ? t_White : t_Black);
	       break;
	    case C_PASSMOVE:if (okChange()){
		  int mod_cur_node;
		  mod_cur_node = passMove(curNode, curPlayer);
		  if (mod_cur_node)
		     step(curNode, &theBoard);
		  else {
		     curNode = treeDown(curNode);
		     stepDown(curNode, &theBoard);
		  }
	       }
	       break;
	    case C_TOGGLESTONE:
		  curPlayer = (curPlayer == t_Black) ? t_White : t_Black;
	       break;
	    case C_TUTORSWAP:
	       tutor = !tutor;
	       break;
	    case C_MOVE:
	       if (tutor) {
		  nodep save;
		  save = curNode;
		  if (curNode = makeTutor(curNode, curPlayer, xcur, ycur)) {
		     step(curNode, &theBoard);
		  } else {
		     (*io->notifyError) ("Wrong move.");
		     curNode = save;;
		  }
	       } else {
		  if (okChange() && legal(&theBoard, curNode, curPlayer, xcur, ycur)) {
		     int mod_cur_node;
		     mod_cur_node = makeMove(curNode, curPlayer, xcur, ycur);
		     if (mod_cur_node)
			step(curNode, &theBoard);
		     else {
			curNode = treeDown(curNode);
			stepDown(curNode, &theBoard);
		     }
		  }
	       }

	       break;
	    case C_ADDVAR:
	       if (okChange()) {
		  makeVariation(curNode);
		  step(curNode, &theBoard);
	       }
	       break;
	    case C_DELNODE:
	       if (okChange()) {
		  deleteNode(&curNode);
		  step(curNode, &theBoard);
	       }
	       break;
	    case C_PASTE:
	       if (okChange())
		  if (pasteTree(curNode))
		     step(curNode, &theBoard);
		  else
		     (*io->notifyError) ("Nothing to paste.");
	       break;

	    case C_TREECUT:
	       if (okChange()) {
		  nodep prevNode;
		  int savex, savey;
		  savex = xcur;
		  savey = ycur;
		  prevNode = curNode->parent;
		  cutTree(curNode);
		  curNode = prevNode;
		  if (!curNode) {
		     initNodes();
		     root = curNode = newNode();
		  }
		  step(curNode, &theBoard);
		  xcur = savex;
		  ycur = savey;
		  break;
	       }
	    case C_ADDBLACK:
	       if (okChange()) {
		  coordList *cl;
		  property *prop;
		  if (boardGet(&theBoard, xcur, ycur) == P_BLACK) {
		     addStone(curNode, t_AddEmpty, xcur, ycur);
		     setPiece(&theBoard, xcur, ycur, P_NOTHING);
		  } else {
		     addStone(curNode, t_AddBlack, xcur, ycur);
		     setPiece(&theBoard, xcur, ycur, P_BLACK);
		  }
		  if (prop = getprop(curNode, t_Letter))
		     lastletters = prop->data.stones;
		  for (i = 0, cl = lastletters; cl; i++, cl = cl->next)
		     (*io->plotLetter) (cl->x, cl->y, i % 26 + 'a');
	       }
	       break;
	    case C_ADDWHITE:
	       if (okChange()) {
		  coordList *cl;
		  property *prop;
		  if (boardGet(&theBoard, xcur, ycur) == P_WHITE) {
		     addStone(curNode, t_AddEmpty, xcur, ycur);
		     setPiece(&theBoard, xcur, ycur, P_NOTHING);
		  } else {
		     addStone(curNode, t_AddWhite, xcur, ycur);
		     setPiece(&theBoard, xcur, ycur, P_WHITE);
		  }
		  if (prop = getprop(curNode, t_Letter))
		     lastletters = prop->data.stones;

		  for (i = 0, cl = lastletters; cl; i++, cl = cl->next)
		     (*io->plotLetter) (cl->x, cl->y, i % 26 + 'a');

	       }
	       break;
	    case C_ADDLETTER:
	       if (okChange()) {



		  int sx, sy;

		  for (; lastletters; lastletters = lastletters->next)
		     (*io->plotPiece) (&theBoard, lastletters->x, lastletters->y);
		  lastletters = 0;

		  sx = xcur;
		  sy = ycur;


		  if (addMark(curNode, t_Letter, xcur, ycur))
		     (*io->plotPiece) (&theBoard, xcur, ycur);
		  stepDown(curNode, &theBoard);
		  xcur = sx;
		  ycur = sy;
	       }
	       break;
	    case C_ADDMARK:
	       if (okChange()) {
		  int sx, sy;

		  for (; lastmarks; lastmarks = lastmarks->next)
		     (*io->plotPiece) (&theBoard, lastmarks->x, lastmarks->y);
		  lastmarks = 0;

		  sx = xcur;
		  sy = ycur;
		  if (addMark(curNode, t_Mark, xcur, ycur))
		     (*io->plotPiece) (&theBoard, xcur, ycur);

		  stepDown(curNode, &theBoard);
		  xcur = sx;
		  ycur = sy;
	       }
	       break;
	    case C_EDCOMMENT:
	       if (okChange()) {
		  edComment(curNode);
		  step(curNode, &theBoard);
	       }
	       break;
	    case C_ADDNAME:
	       if (okChange()) {
		  makeName(curNode);
		  step(curNode, &theBoard);
	       }
	       break;
	    case C_DOWN:
	       tNode = treeDown(curNode);
	       if (tNode != curNode) {
		  curNode = tNode;
		  stepDown(curNode, &theBoard);
	       }
	       break;
	    case C_UP:
	       curNode = treeUp(curNode);
	       step(curNode, &theBoard);
	       break;
	    case C_WALKDOWN:
	       tNode = treeNext(curNode);
	       if (tNode->parent == curNode) {
		  stepDown(tNode, &theBoard);
	       } else {
		  step(tNode, &theBoard);
	       }
	       curNode = tNode;
	       break;
	    case C_WALKUP:
	       curNode = treeLast(curNode);
	       step(curNode, &theBoard);
	       break;
	    case C_SEARCHCOMMENT:
	       {
		  nodep tBegin;
		  tBegin = curNode;
		  tNode = curNode;
		  do {
		     tNode = treeNext(tNode);
		  }
		  while (tNode != tBegin && !getprop(tNode, t_Comment));
		  if (tNode != curNode) {
		     if (tNode->parent == curNode) {
			stepDown(tNode, &theBoard);
		     } else {
			step(tNode, &theBoard);
		     }
		     curNode = tNode;
		  }
	       }
	       break;
	    case C_SEARCHBACKCOMMENT:
	       tNode = curNode;
	       curNode = curNode;
	       do {
		  curNode = treeLast(curNode);
	       }
	       while (curNode != tNode && !getprop(curNode, t_Comment));
	       step(curNode, &theBoard);
	       break;
	    case C_UPFORK:
	       while (curNode->parent) {
		  curNode = curNode->parent;
		  if (treeCountSiblings(curNode) > 1)
		     break;
	       }
	       step(curNode, &theBoard);
	       break;
	    case C_DOWNFORK:
	       {
		  while (curNode->child) {
		     curNode = treeDown(curNode);
		     if (treeCountSiblings(curNode) > 1)
			break;
		  }
		  step(curNode, &theBoard);
	       }
	       break;
	    case C_END:
	       while (curNode->child) {
		  curNode = treeDown(curNode);
	       }
	       step(curNode, &theBoard);
	       break;
	    case C_BEGINNING:
	       curNode = root;
	       step(curNode, &theBoard);
	       break;
	    case C_SCORE:
	       doScore(&theBoard, curNode);
	       break;
	    case C_GOTO:
	       {
		  char buf[7];
		  nodep new;
		  if ((*io->queryStr) ("Move to node # ?", buf, 5)) {
		     searchNodeNum = atoi(buf);
		     if (searchNodeNum == 0 && strcmp(buf, "0"))
			searchNodeNum = -1;
		     if (searchNodeNum >= 0) {
			if (new = search(root)) {
			   curNode = new;
			   step(curNode, &theBoard);
			   (*io->notifyClear) ();
			} else {
			   (*io->notifyClear) ();
			   (*io->notifyError) ("Node not found.");
			}

		     }
		  }
	       }
	       break;
	    case C_QUIT:
	       if (okExit(root))
		  quitflg++;
	       break;
	    case C_WRITE:
	       {
		  char filename[50];
		  if ((*io->queryStr) ("Save name? ", filename, 48)) {
		     if (!strcmp("*", filename)) {
			if (!writeTree(name_buf, root))
			   madechanges = 0;
		     } else {
			if (!writeTree(filename, root))
			   madechanges = 0;
		     }
		  }
	       }
	       break;
	    case C_SAVESHORT:
	       saveShort = !saveShort;
	       break;
	    case C_CURLEFT:
	       xcur = (xcur - 1 + boardsize) % boardsize;
	       (*io->setCursor) (xcur, ycur);
	       (*io->refreshIO) ();
	       break;
	    case C_CURRIGHT:
	       xcur = (xcur + 1) % boardsize;
	       (*io->setCursor) (xcur, ycur);
	       (*io->refreshIO) ();
	       break;
	    case C_CURUP:
	       ycur = (ycur - 1 + boardsize) % boardsize;
	       (*io->setCursor) (xcur, ycur);
	       (*io->refreshIO) ();
	       break;
	    case C_CURDOWN:
	       ycur = (ycur + 1) % boardsize;
	       (*io->setCursor) (xcur, ycur);
	       (*io->refreshIO) ();
	       break;
	    case C_UPLEFT:
	       ycur = (ycur - 1 + boardsize) % boardsize;
	       xcur = (xcur - 1 + boardsize) % boardsize;
	       (*io->setCursor) (xcur, ycur);
	       (*io->refreshIO) ();
	       break;
	    case C_UPRIGHT:
	       xcur = (xcur + 1) % boardsize;
	       ycur = (ycur - 1 + boardsize) % boardsize;
	       (*io->setCursor) (xcur, ycur);
	       (*io->refreshIO) ();
	       break;
	    case C_DOWNLEFT:
	       xcur = (xcur - 1 + boardsize) % boardsize;
	       ycur = (ycur + 1) % boardsize;
	       (*io->setCursor) (xcur, ycur);
	       (*io->refreshIO) ();
	       break;
	    case C_DOWNRIGHT:
	       xcur = (xcur + 1) % boardsize;
	       ycur = (ycur + 1) % boardsize;
	       (*io->setCursor) (xcur, ycur);
	       (*io->refreshIO) ();
	       break;
	 }
   }
}
