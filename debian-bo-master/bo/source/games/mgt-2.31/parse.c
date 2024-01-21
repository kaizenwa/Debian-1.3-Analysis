/* "mgt" Copyright (c) 1991 Shodan  */

#include <string.h>
#include "mgt.h"


struct {
   char *str;
   Token val;
}  tokens[] = {
   {
      "W", t_White
   },
   {
      "B", t_Black
   },
   {
      "C", t_Comment
   },
   {
      "AW", t_AddWhite
   },
   {
      "AB", t_AddBlack
   },
   {
      "L", t_Letter
   },
   {
      "AE", t_AddEmpty
   },
   {
      "N", t_Name
   },
   {
      "M", t_Mark
   },
   {
      "SZ", t_Size
   },
   {
      "GN", t_GameName
   },
   {
      "GC", t_GameComment
   },
   {
      "EV", t_Event
   },
   {
      "RO", t_Round
   },
   {
      "DT", t_Date
   },
   {
      "PC", t_Place
   },
   {
      "PB", t_PlayerBlack
   },
   {
      "PW", t_PlayerWhite
   },
   {
      "RE", t_Result
   },
   {
      "US", t_User
   },
   {
      "TM", t_TimeLimit
   },
   {
      "SO", t_Source
   },
   {
      "BR", t_BlackRank
   },
   {
      "WR", t_WhiteRank
   },
   {
      "HA", t_Handicap
   },
   {
      "KM", t_Komi
   }, {
      "PL", t_Player
   }

};


char buf[1028], *curin, started;

FUNCTION void readInit()
{
   int p;
   buf[0] = '\0';
   curin = &buf[0];
   started = 1;
   boardsize = 19;
   handicap = 0;
   for (p = 0; p <= LASTINFO - FIRSTINFO + 1; p++) {
      if (info[p])
	 free(info[p]);
      info[p] = 0;
   }
}


static char *readLine()
{
   curin = &buf[0];
   buf[0] = '\0';
   return fgets(&buf[0], 1023, input);
}


static char readChar()
{
   if (*curin)
      return *(curin++);
   curin = &buf[0];
   buf[0] = '\0';
   fgets(&buf[0], 1023, input);
   return 0;
}



static int getCoordStr(co)
coord *co;
{
   if (strlen(curin) < 4 && curin[strlen(curin) - 1] != '\n') {
      char *dest;
      for (dest = buf; *curin; curin++, dest++)
	 *dest = *curin;
      *dest = 0;
      fgets(dest, 1023, input);
      curin = buf;
   }
   if (curin[0] == '[' && curin[1] >= 'a' && curin[1] <= 's'
       && curin[2] >= 'a' && curin[2] <= 's' && curin[3] == ']') {
      co->x = curin[1] - 'a';
      co->y = curin[2] - 'a';
      curin += 4;
      while (*curin == ' ' || *curin == '\t')
	 curin++;
      if (*curin == '\n')
	 readLine();
      return 1;
   } else
      return 0;
}


static void getCoordList(clp)
coordList **clp;
{
   coord co;
   while (getCoordStr(&co)) {
      setCoord(co.x, co.y, clp);
   }
}

static void addPass(n, t)
nodep n;
Token t;
{
   property *p;
   if (!(p = (property *) calloc(1, sizeof(property))))
      barf("Memory allocation error (addPass)");
   p->t = t_Pass;
   p->data.player = t;
   addprop(n, p);

}



static void doPlayer(n)
nodep n;

{
   property *p;

   if (curin[0] == '[' && curin[2] == ']' && (curin[1] == 'B' || curin[1] == 'W')) {
      p = (property *) calloc(1, sizeof(property));
      if (!p)
	 barf("Memory allocation failure (doPlayer)");
      p->t = t_Player;
      p->data.player = curin[1] == 'B' ? t_Black : t_White;
      curin += 3;
      addprop(n, p);
   }
}


static Token tokenize()
{
   int i, len;
   char buf[4];

   len = 0;
   do {
      if (!*curin || *curin == '\n') {
	 if (!readLine())
	    return t_EOF;
      }
      if (*curin == ')') {
	 curin++;
	 return t_Close;
      }
      if (*curin == '(') {
	 curin++;
	 return t_Open;
      }
      if (*curin == ';') {
	 curin++;
	 return t_NewNode;
      }
      if (*curin >= 'A' && *curin <= 'Z')
	 buf[len++] = *curin;
      curin++;
   } while (len < 3 && *curin != '[');
   buf[len] = 0;
   if (len == 1 || len == 2)
      for (i = 0; i < sizeof(tokens) / sizeof(tokens[0]); i++)
	 if (!strcmp(buf, tokens[i].str))
	    return tokens[i].val;
   while (*curin != ']') {
      if (!*++curin) {
	 if (!readLine())
	    return t_EOF;
      }
   }
   return t_WS;
}



static void addMoveArrayProp(t, n)
Token t;
nodep n;
{
   property *p;

#ifdef DEBUG
   totalmemory += sizeof(property);
   fprintf(debug, "%ld %ld\n", totalmemory, coreleft());
#endif
   if (!(p = getprop(n, t))) {
      p = (property *) calloc(1, sizeof(property));

#ifdef DEBUG
      totalmemory += sizeof(coordList);
      fprintf(debug, "%ld %ld\n", totalmemory, coreleft());
#endif

      if (!p)
	 barf("Memory allocation failure (addMoveArrayProp)");

      p->data.stones = 0;
      p->t = t;
      addprop(n, p);
   }
   getCoordList(&(p->data.stones));
}


static void doSize()
{
   int size;
   char *s, c, b[25];
   s = &b[0];
   readChar();
   while ((c = readChar()) != ']')
      if (c && c != '\\')
	 *s++ = c;
   *s = 0;
   size = atoi(b);
   if ((size > 1) && (size <= 19))
      boardsize = size;
}



static void doHandicap()
{
   int size;
   char *s, c, b[25];
   s = &b[0];
   readChar();
   while ((c = readChar()) != ']')
      if (c && c != '\\')
	 *s++ = c;
   *s = 0;
   size = atoi(b);
   if ((size > 1) && (size <= 17))
      handicap = size;
}


/* gets text.  Assumes we start on a '['.  Returns pointer to a buffer which
 * should be freed */

static char *getText()
{
   int c;
   char *s;
   char *buffer;
   int space = COMMENTALLOC;
   int blocks = 1;

   buffer = (char *) malloc(COMMENTALLOC);
   if (!buffer)
      barf("Memory Allocation Failure (getText)");
   s = buffer;

   (void) readChar();
   while ((c = readChar()) != ']') {
      if (!c) {
	 c = readChar();
	 if (!c || c == ']')
	    break;
      }
      if (space <= 1) {
	 buffer = (char *) realloc(buffer, ++blocks * COMMENTALLOC);
	 if (!buffer)
	    barf("Memory Allocation Failure (getText)");
	 s = buffer - space + (blocks - 1) * COMMENTALLOC;
	 space += COMMENTALLOC;
      }
      if (c == '\\')
	 c = readChar();
      if (c) {
	 *s++ = c;
	 space--;
      }
   }
   if (s != buffer && *(s - 1) == '\n')
      s--;
   *s = 0;
   return buffer;
}


static void doComment(n, t)
nodep n;
Token t;
{
   property *p;
   char *buffer;

   p = (property *) calloc(1, sizeof(property));
   if (!p)
      barf("Memory Allocation Failure (doComment)");
   buffer = getText();
   p->t = t;
   p->data.comment = dupStr(buffer);
   addprop(n, p);
   free(buffer);
}


static void doInfo(t)
Token t;
{
   char *buffer;

   buffer = getText();
   info[(int) t - FIRSTINFO] = dupStr(buffer);
   free(buffer);
}


FUNCTION void addChild(n, c)	/* add child c to node n */
nodep n, c;
{
   if (n) {
      if (n->child) {
	 nodep s;
	 s = treeLastSibling(child(n));
	 s->nextSibling = c;
	 c->lastSibling = s;
	 c->parent = n;
      } else {
	 n->child = c;
	 c->parent = n;
      }
   }
}


FUNCTION nodep parse(lev)
int lev;
{
   nodep r, n, new;
   Token t;

   if (started) {
      started = 0;

      while (*curin != '(') {
	 if (!readLine())
	    break;
      }
   }
   r = n = 0;
   for (;;) {
      t = tokenize();
      switch (t) {
	 case t_Size:
	    doSize();
	    break;
	 case t_White:
	 case t_Black:
	    if (curin[1] == 't' && curin[2] == 't' && curin[0] == '[' && curin[3] == ']') {
	       addPass(n, t);
	       break;
	    }
	 case t_AddWhite:
	 case t_AddBlack:
	 case t_AddEmpty:
	 case t_Mark:
	 case t_Letter:
	    if (n)
	       addMoveArrayProp(t, n);
	    else
	       fprintf(stderr, "Error - property w/o node in data\n");
	    break;
	 case t_Player:
	    if (n)
	       doPlayer(n);
	    else
	       fprintf(stderr, "Error - property w/o node in data\n");
	    break;
	 case t_Name:
	 case t_Comment:
	    if (n)
	       doComment(n, t);
	    else
	       fprintf(stderr, "Error - property w/o node in data\n");
	    break;
	 case t_NewNode:
	    new = newNode();
	    if (!r)
	       r = new;
	    else
	       addChild(n, new);
	    n = new;
	    break;
	 case t_Open:
	    if (lev == 1 && !r)
	       n = r = newNode();
	    if (new = parse(lev + 1)) {
	       addChild(n, new);
	       if (!r)
		  r = new;
	    }
	    break;
	 case t_Close:
	 case t_EOF:
	    return r;

	 case t_GameName:
	 case t_GameComment:
	 case t_Event:
	 case t_Round:
	 case t_Date:
	 case t_Place:
	 case t_PlayerBlack:
	 case t_PlayerWhite:
	 case t_Result:
	 case t_User:
	 case t_TimeLimit:
	 case t_Source:
	 case t_BlackRank:
	 case t_WhiteRank:

	    doInfo(t);
	    break;
	 case t_Komi:{
	       char *ch, *end;
	       doInfo(t_Komi);
	       end = ch = info[(int) t_Komi - FIRSTINFO];
	       while (*end)
		  end++;
	       for (end--; *end == '0' && (end != ch); end--);
	       *++end = 0;
	    }
	    break;
	 case t_Handicap:
	    doHandicap();
	    break;
	 default:
	    break;
      }
   }
}
