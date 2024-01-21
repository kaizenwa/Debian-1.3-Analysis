# include "stdio.h"
# define U(x) ((x)&0377)
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern unsigned char yytext[];
int yymorfg;
extern unsigned char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
/*
 * parser.l -- lex parser of algebraic chess moves for XBoard
 * $Id: parser.l,v 1.29 1995/11/18 10:25:25 mann Exp $
 *
 * Copyright 1991 by Digital Equipment Corporation, Maynard, Massachusetts.
 * Enhancements Copyright 1992-95 Free Software Foundation, Inc.
 *
 * The following terms apply to Digital Equipment Corporation's copyright
 * interest in XBoard:
 * ------------------------------------------------------------------------
 * All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * ------------------------------------------------------------------------
 *
 * The following terms apply to the enhanced version of XBoard distributed
 * by the Free Software Foundation:
 * ------------------------------------------------------------------------
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 * ------------------------------------------------------------------------
 */

/* This parser handles all forms of promotion.
 * The parser resolves ambiguous moves by searching and check-testing.
 * It also parses comments of the form [anything] or (anything).
 */

#include <config.h>

#define NO_CONSTRAINT	-1
#undef YYLMAX
#define YYLMAX			4096
#define UNPUT_BUF_SIZE		YYLMAX

#ifdef FLEX_SCANNER
/* yytext is probably a char*, but could be a char[].  yy_text is set
   in YY_DECL below, because if yytext is a char*, its value is not
   constant. */
char *yy_text;
#else /*!FLEX_SCANNER*/
/* yytext is definitely a char[], so yy_text can be set here, statically. */
char *yy_text = (char *) yytext;
#endif

#ifdef FLEX_SCANNER
/* This is flex */
#undef YY_INPUT
#define YY_INPUT(buf, result, max_size) my_yy_input(buf, &result, max_size)
#undef YY_DECL
#define YY_DECL                     \
    int _yylex YY_PROTO((void));    \
    int yylex YY_PROTO((void))      \
    {                               \
	int result = _yylex();      \
	yy_text = (char *) yytext;  \
	return(result);             \
    }                               \
    int _yylex YY_PROTO((void))
#else
/* This is lex */
#undef input
#undef output
#undef unput
#endif

/* The includes must be here, below the #undef input */

#include <ctype.h>

# if HAVE_STRING_H
#  include <string.h>
# else /* not HAVE_STRING_H */
#  include <strings.h>
# endif /* not HAVE_STRING_H */

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if defined(_amigados)
# include <errno.h>
# if HAVE_FCNTL_H
#  include <fcntl.h>    /*  isatty() prototype  */
# endif /*  HAVE_FCNTL_H        */
#endif  /*  defined(_amigados)  */

#include "common.h"
#include "backend.h"
#include "frontend.h"
#include "parser.h"
#include "moves.h"

#define FakeFlags(index) \
    (((((index) % 2) == 0) ? F_WHITE_ON_MOVE : 0) | F_ALL_CASTLE_OK)

extern Board	boards[MAX_MOVES];
int		yyboardindex;
int             yyskipmoves = FALSE;
char		currentMoveString[YYLMAX];
#ifndef FLEX_SCANNER
char		unputBuffer[UNPUT_BUF_SIZE];
int		unputCount = 0;
#endif

#ifdef FLEX_SCANNER
void my_yy_input P((char *buf, int *result, int max_size));
#else /*!FLEX_SCANNER*/
static int input P((void));
static void output P((int ch));
static void unput P((int ch));
int yylook P((void));
int yyback P((int *, int));
#endif
#undef yywrap
int yywrap P((void));
extern void CopyBoard P((Board to, Board from));

# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
{
    /*
     * Fully-qualified algebraic move, possibly with promotion
     */
    int skip1 = 0, skip2 = 0;
    ChessSquare piece;
    ChessMove result;
    
    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    /* remove the / */
    if (yytext[1] == '/') skip1 = 1;
    
    /* remove the [xX:-] */
    if ((yytext[3+skip1] == 'x') || (yytext[3+skip1] == 'X') ||
	(yytext[3+skip1] == '-') || (yytext[3+skip1] == ':')) skip2 = 1;
    
    currentMoveString[0] = yytext[1+skip1];
    currentMoveString[1] = yytext[2+skip1];
    currentMoveString[2] = yytext[3+skip1+skip2];
    currentMoveString[3] = yytext[4+skip1+skip2];
    currentMoveString[4] = NULLCHAR;
    
    if (yyleng-skip1-skip2 > 5) {
	if (yytext[yyleng-1] == ')') {
	    currentMoveString[4] = ToLower(yytext[yyleng-2]);
	} else {
	    currentMoveString[4] = ToLower(yytext[yyleng-1]);
	}
	currentMoveString[5] = NULLCHAR;
    }

    piece = boards[yyboardindex]
      [currentMoveString[1] - '1'][currentMoveString[0] - 'a'];
    if (ToLower(yytext[0]) != ToLower(PieceToChar(piece)))
      return (int) BadMove;

    result = LegalityTest(boards[yyboardindex],
			  FakeFlags(yyboardindex), EP_UNKNOWN,
			  currentMoveString[1] - '1',
			  currentMoveString[0] - 'a',
			  currentMoveString[3] - '1',
			  currentMoveString[2] - 'a',
			  currentMoveString[4]);

    if (currentMoveString[4] == NULLCHAR &&
	(result == WhitePromotionQueen || result == BlackPromotionQueen)) {
	currentMoveString[4] = 'q';
	currentMoveString[5] = NULLCHAR;
    }

    return (int) result;
}
break;
case 2:
{
    /*
     * Simple algebraic move, possibly with promotion
     */
    int skip = 0;
    ChessMove result;

    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    /* remove the [xX:-] */
    if ((yytext[2] == 'x') || (yytext[2] == 'X') ||
	(yytext[2] == '-') || (yytext[2] == ':')) skip = 1;

    currentMoveString[0] = yytext[0];
    currentMoveString[1] = yytext[1];
    currentMoveString[2] = yytext[2+skip];
    currentMoveString[3] = yytext[3+skip];
    currentMoveString[4] = NULLCHAR;

    if (yyleng-skip > 4) {
	if (yytext[yyleng-1] == ')') {
	    currentMoveString[4] = ToLower(yytext[yyleng-2]);
	} else {
	    currentMoveString[4] = ToLower(yytext[yyleng-1]);
	}
	currentMoveString[5] = NULLCHAR;
    }

    result = LegalityTest(boards[yyboardindex],
			  FakeFlags(yyboardindex), EP_UNKNOWN,
			  currentMoveString[1] - '1',
			  currentMoveString[0] - 'a',
			  currentMoveString[3] - '1',
			  currentMoveString[2] - 'a',
			  currentMoveString[4]);

    if (currentMoveString[4] == NULLCHAR &&
	(result == WhitePromotionQueen || result == BlackPromotionQueen)) {
	currentMoveString[4] = 'q';
	currentMoveString[5] = NULLCHAR;
    }

    return (int) result;
}
break;
case 3:
{
    /*
     * Pawn move, possibly with promotion
     */
    DisambiguateClosure cl;
    int skip = 0;

    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    /* remove the =() */
    if (yytext[2] == '=') skip++;
    if (yytext[2+skip] == '(') skip++;

    cl.pieceIn = WhiteOnMove(yyboardindex) ? WhitePawn : BlackPawn;
    cl.rfIn = -1;
    cl.ffIn = yytext[0] - 'a';
    cl.rtIn = yytext[1] - '1';
    cl.ftIn = yytext[0] - 'a';
    cl.promoCharIn = yytext[2+skip];
    Disambiguate(boards[yyboardindex],
		 FakeFlags(yyboardindex), EP_UNKNOWN, &cl);

    currentMoveString[0] = cl.ff + 'a';
    currentMoveString[1] = cl.rf + '1';
    currentMoveString[2] = cl.ft + 'a';
    currentMoveString[3] = cl.rt + '1';
    currentMoveString[4] = cl.promoChar;
    currentMoveString[5] = NULLCHAR;

    return (int) cl.kind;
}
break;
case 4:
{
    /*
     * Pawn capture, possibly with promotion, possibly ambiguous
     */
    DisambiguateClosure cl;
    int skip1 = 0, skip2 = 0;

    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    /* remove the [xX:-] and =() */
    if ((yytext[1] == 'x') || (yytext[1] == 'X')
	|| (yytext[1] == ':') || (yytext[1] == '-')) skip1 = 1;
    if (yytext[2+skip1] == '=') skip2++;
    if (yytext[2+skip1+skip2] == '(') skip2++;

    cl.pieceIn = WhiteOnMove(yyboardindex) ? WhitePawn : BlackPawn;
    cl.rfIn = -1;
    cl.ffIn = yytext[0] - 'a';
    cl.rtIn = -1;
    cl.ftIn = yytext[1+skip1] - 'a';
    cl.promoCharIn = yytext[2+skip1+skip2];
    Disambiguate(boards[yyboardindex],
		 FakeFlags(yyboardindex), EP_UNKNOWN, &cl);

    currentMoveString[0] = cl.ff + 'a';
    currentMoveString[1] = cl.rf + '1';
    currentMoveString[2] = cl.ft + 'a';
    currentMoveString[3] = cl.rt + '1';
    currentMoveString[4] = cl.promoChar;
    currentMoveString[5] = NULLCHAR;

    return (int) cl.kind;
}
break;
case 5:
{
    /*
     * unambiguously abbreviated Pawn capture, possibly with promotion
     */
    int skip = 0;
    ChessMove result;

    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    /* remove the [xX:-] */
    if ((yytext[1] == 'x') || (yytext[1] == 'X')
	|| (yytext[1] == ':') || (yytext[1] == '-')) skip = 1;

    currentMoveString[0] = yytext[0];
    currentMoveString[2] = yytext[1+skip];
    currentMoveString[3] = yytext[2+skip];
    if (WhiteOnMove(yyboardindex)) {
	if (yytext[2+skip] == '1') return (int) BadMove;
	currentMoveString[1] = yytext[2+skip] - 1;
    } else {
	if (yytext[2+skip] == '8') return (int) BadMove;
	currentMoveString[1] = yytext[2+skip] + 1;
    }
    if (yyleng-skip > 3) {
	if (yytext[yyleng-1] == ')')
	  currentMoveString[4] = ToLower(yytext[yyleng-2]);
	else
	  currentMoveString[4] = ToLower(yytext[yyleng-1]);
	currentMoveString[5] = NULLCHAR;
    } else {
	currentMoveString[4] = NULLCHAR;
    }

    result = LegalityTest(boards[yyboardindex],
			  FakeFlags(yyboardindex), EP_UNKNOWN,
			  currentMoveString[1] - '1',
			  currentMoveString[0] - 'a',
			  currentMoveString[3] - '1',
			  currentMoveString[2] - 'a',
			  currentMoveString[4]);

    if (currentMoveString[4] == NULLCHAR &&
	(result == WhitePromotionQueen || result == BlackPromotionQueen)) {
	currentMoveString[4] = 'q';
	currentMoveString[5] = NULLCHAR;
    }

    if (result != BadMove) return (int) result;

    /* Special case: improperly written en passant capture */
    if (WhiteOnMove(yyboardindex)) {
	if (currentMoveString[3] == '5') {
	    currentMoveString[1] = '5';
	    currentMoveString[3] = '6';
	} else {
	    return (int) BadMove;
	}
    } else {
	if (currentMoveString[3] == '4') {
	    currentMoveString[1] = '4';
	    currentMoveString[3] = '3';
	} else {
	    return (int) BadMove;
	}
    }

    result = LegalityTest(boards[yyboardindex],
			  FakeFlags(yyboardindex), EP_UNKNOWN,
			  currentMoveString[1] - '1',
			  currentMoveString[0] - 'a',
			  currentMoveString[3] - '1',
			  currentMoveString[2] - 'a',
			  currentMoveString[4]);

    if (result == WhiteCapturesEnPassant || result == BlackCapturesEnPassant)
      return (int) result;
    else
      return (int) BadMove;
}
break;
case 6:
 {
    /*
     * piece move, possibly ambiguous
     */
    DisambiguateClosure cl;
    int skip = 0;

    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    /* remove the [xX:-] */
    if ((yytext[1] == 'x') || (yytext[1] == 'X')
	|| (yytext[1] == ':') || (yytext[1] == '-')) skip = 1;

    if (WhiteOnMove(yyboardindex)) {
	cl.pieceIn = CharToPiece(ToUpper(yytext[0]));
    } else {
	cl.pieceIn = CharToPiece(ToLower(yytext[0]));
    }
    cl.rfIn = -1;
    cl.ffIn = -1;
    cl.rtIn = yytext[2+skip] - '1';
    cl.ftIn = yytext[1+skip] - 'a';
    cl.promoCharIn = NULLCHAR;
    Disambiguate(boards[yyboardindex],
		 FakeFlags(yyboardindex), EP_UNKNOWN, &cl);

    currentMoveString[0] = cl.ff + 'a';
    currentMoveString[1] = cl.rf + '1';
    currentMoveString[2] = cl.ft + 'a';
    currentMoveString[3] = cl.rt + '1';
    currentMoveString[4] = cl.promoChar;
    currentMoveString[5] = NULLCHAR;

    return (int) cl.kind;
}
break;
case 7:
{
    /*
     * piece move with rank or file disambiguator
     */
    DisambiguateClosure cl;
    int skip = 0;

    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    /* remove the [xX:-] */
    if ((yytext[2] == 'x') || (yytext[2] == 'X')
	|| (yytext[2] == ':') || (yytext[2] == '-')) skip = 1;

    if (WhiteOnMove(yyboardindex)) {
	cl.pieceIn = CharToPiece(ToUpper(yytext[0]));
    } else {
	cl.pieceIn = CharToPiece(ToLower(yytext[0]));
    }
    if (isalpha(yytext[1])) {
	cl.rfIn = -1;
	cl.ffIn = yytext[1] - 'a';
    } else {
	cl.rfIn = yytext[1] - '1';
	cl.ffIn = -1;
    }
    cl.rtIn = yytext[3+skip] - '1';
    cl.ftIn = yytext[2+skip] - 'a';
    cl.promoCharIn = NULLCHAR;
    Disambiguate(boards[yyboardindex],
		 FakeFlags(yyboardindex), EP_UNKNOWN, &cl);

    currentMoveString[0] = cl.ff + 'a';
    currentMoveString[1] = cl.rf + '1';
    currentMoveString[2] = cl.ft + 'a';
    currentMoveString[3] = cl.rt + '1';
    currentMoveString[4] = cl.promoChar;
    currentMoveString[5] = NULLCHAR;

    return (int) cl.kind;
}
break;
case 8:
{
    int rf, ff, rt, ft;

    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    if (WhiteOnMove(yyboardindex)) {
	if (boards[yyboardindex][0][3] == WhiteKing) {
	    /* ICS wild castling */
	    strcpy(currentMoveString, "d1f1");
	    rf = 0;
	    ff = 3;
	    rt = 0;
	    ft = 5;
	} else {
	    strcpy(currentMoveString, "e1c1");
	    rf = 0;
	    ff = 4;
	    rt = 0;
	    ft = 2;
	}
    } else{ 
	if (boards[yyboardindex][7][3] == BlackKing) {
	    /* ICS wild castling */
	    strcpy(currentMoveString, "d8f8");
	    rf = 7;
	    ff = 3;
	    rt = 7;
	    ft = 5;
	} else {
	    strcpy(currentMoveString, "e8c8");
	    rf = 7;
	    ff = 4;
	    rt = 7;
	    ft = 2;
	}
    }
    return (int) LegalityTest(boards[yyboardindex],
			      FakeFlags(yyboardindex), EP_UNKNOWN,
			      rf, ff, rt, ft, NULLCHAR);
}
break;
case 9:
{
    int rf, ff, rt, ft;

    if (yyskipmoves) return (int) AmbiguousMove; /* not disambiguated */

    if (WhiteOnMove(yyboardindex)) {
	if (boards[yyboardindex][0][3] == WhiteKing) {
	    /* ICS wild castling */
	    strcpy(currentMoveString, "d1b1");
	    rf = 0;
	    ff = 3;
	    rt = 0;
	    ft = 1;
	} else {
	    strcpy(currentMoveString, "e1g1");
	    rf = 0;
	    ff = 4;
	    rt = 0;
	    ft = 6;
	}
    } else {
	if (boards[yyboardindex][7][3] == BlackKing) {
	    /* ICS wild castling */
	    strcpy(currentMoveString, "d8b8");
	    rf = 7;
	    ff = 3;
	    rt = 7;
	    ft = 1;
	} else {
	    strcpy(currentMoveString, "e8g8");
	    rf = 7;
	    ff = 4;
	    rt = 7;
	    ft = 6;
	}
    }
    return (int) LegalityTest(boards[yyboardindex],
			      FakeFlags(yyboardindex), EP_UNKNOWN,
			      rf, ff, rt, ft, NULLCHAR);
}
break;
case 10:
{
    /* Bughouse piece drop.  No legality checking for now. */
    currentMoveString[0] = ToUpper(yytext[0]);
    currentMoveString[1] = '@';
    currentMoveString[2] = yytext[2];
    currentMoveString[3] = yytext[3];
    currentMoveString[4] = NULLCHAR;
    return (int) (WhiteOnMove(yyboardindex) ? WhiteDrop : BlackDrop);
}
break;
case 11:
 {
    if (WhiteOnMove(yyboardindex))
      return (int) BlackWins;
    else
      return (int) WhiteWins;
}
break;
case 12:
 {
    return (int) BlackWins;
}
break;
case 13:
 {
    return (int) WhiteWins;
}
break;
case 14:
 {
    return (int) WhiteWins;
}
break;
case 15:
 {
    return (int) BlackWins;
}
break;
case 16:
{
    return (int) GameUnfinished;
}
break;
case 17:
 {
    return (int) GameIsDrawn;
}
break;
case 18:
 {
    return (int) GameIsDrawn;
}
break;
case 19:
 {
    if (WhiteOnMove(yyboardindex))
      return (int) BlackWins;
    else
      return (int) WhiteWins;
}
break;
case 20:
 {
    if (WhiteOnMove(yyboardindex))
      return (int) BlackWins;
    else
      return (int) WhiteWins;
}
break;
case 21:
 {
    return (int) GameIsDrawn;
}
break;
case 22:
{
    return (int) GameIsDrawn;
}
break;
case 23:
 {
    return (int) GameIsDrawn;
}
break;
case 24:
{ 
    return (int) WhiteWins;
}
break;
case 25:
{ 
    return (int) BlackWins;
}
break;
case 26:
{ 
    return (int) BlackWins;
}
break;
case 27:
{ 
    return (int) WhiteWins;
}
break;
case 28:
{ 
    return (int) WhiteWins;
}
break;
case 29:
{ 
    return (int) BlackWins;
}
break;
case 30:
{
    return (int) GameIsDrawn;
}
break;
case 31:
{
    return (int) GameUnfinished;
}
break;
case 32:
   {
    /* move numbers */
    if ((yyleng == 1) && (yytext[0] == '1'))
      return (int) MoveNumberOne;
}
break;
case 33:
{
    /* elapsed time indication, e.g. (0:12) */ 
    return (int) ElapsedTime;
}
break;
case 34:
{
    /* position diagram enclosed in [-- --] */
    return (int) PositionDiagram;
}
break;
case 35:
{
    /* position diagram enclosed in {-- --} */
    return (int) PositionDiagram;
}
break;
case 36:
{
    return (int) PGNTag;
}
break;
case 37:
{
    return (int) GNUChessGame;
}
break;
case 38:
{
    return (int) XBoardGame;
}
break;
case 39:
{        			/* anything in {} */
    return (int) Comment; 
}
break;
case 40:
{                                          /* ; to end of line */
    return (int) Comment;
}
break;
case 41:
{        			/* anything in [] */
    return (int) Comment; 
}
break;
case 42:
 { 	  	/* nested () */
    return (int) Comment; 
}
break;
case 43:
  { 				/* >=2 chars in () */
    return (int) Comment; 
}
break;
case 44:
 {
        /* Skip mail headers */
}
break;
case 45:
		{
        /* Skip random words */
}
break;
case 46:
			{
        /* Skip everything else */
}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */


static char *StringToLex;

#ifndef FLEX_SCANNER
static FILE *lexFP;

static int input()
{
    int ret;
    
    if (StringToLex != NULL) {
	ret = *StringToLex;
	if (ret == NULLCHAR)
	  ret = EOF;
	else
	  StringToLex++;
    } else if (unputCount > 0) {
	ret = unputBuffer[--unputCount];
    } else {
	ret = fgetc(lexFP);
    }    

    if (ret == EOF) 
      return 0;
    else
      return ret;
}

/*
 * Return offset of next pattern within current file
 */
int yyoffset()
{
    int offset = ftell(lexFP) - unputCount;

    if (offset < 0) {
	offset = 0;
    }
    return(offset);
}
 
static void output(ch)
     int ch;
{
    fprintf(stderr, "PARSER BUG: unmatched character '%c' (0%o)\n",
	    ch, ch);
}

static void unput(ch)
     int ch;
{
    if (ch == 0) return;
    if (StringToLex != NULL) {
	StringToLex--;
    } else {
	if (unputCount >= UNPUT_BUF_SIZE)
	  fprintf(stderr, "PARSER BUG: unput buffer overflow '%c' (0%o)\n",
		  ch, ch);
	unputBuffer[unputCount++] = ch;
    }
}

/* Get ready to lex from a new file.  Kludge below sticks
   an artificial newline at the front of the file, which the
   above grammar ignores, but which makes ^ at start of pattern
   match at the real start of the file.
*/
void yynewfile(f)
     FILE *f;
{
    lexFP = f;
    StringToLex = NULL;
    unputCount = 0;
    unput('\n'); /* kludge */
}

/* Get ready to lex from a string.  ^ at start of pattern WON'T
   match at the start of the string!
*/
void yynewstr(s)
     char *s;
{
    lexFP = NULL;
    StringToLex = s;
    unputCount = 0;
}
#endif /*!FLEX_SCANNER*/

#ifdef FLEX_SCANNER
void my_yy_input(buf, result, max_size)
     char *buf;
     int *result;
     int max_size;
{
    int count;

    if (StringToLex != NULL) {
	count = 0;
	while (*StringToLex != NULLCHAR) {
	    *buf++ = *StringToLex++;
	    count++;
	}
	*result = count;
	return;
    } else {
	count = fread(buf, 1, max_size, yyin);
	if (count == 0) {
	    *result = YY_NULL;
	} else {
	    *result = count;
	}
	return;
    }    
}

static YY_BUFFER_STATE my_file_buffer = NULL;

/*
    Return offset of next pattern in the current file.
*/
int yyoffset()
{
    int pos = yy_c_buf_p - yy_current_buffer->yy_ch_buf;

    return(ftell(yy_current_buffer->yy_input_file) -
         yy_n_chars + pos);
}


void yynewstr(s)
     char *s;
{
    if (my_file_buffer != NULL)
      yy_delete_buffer(my_file_buffer);
    StringToLex = s;
    my_file_buffer = yy_create_buffer(stdin, YY_BUF_SIZE);
    yy_switch_to_buffer(my_file_buffer);
}

void yynewfile(f)
     FILE *f;
{
    if (my_file_buffer != NULL)
      yy_delete_buffer(my_file_buffer);
    StringToLex = NULL;
    my_file_buffer = yy_create_buffer(f, YY_BUF_SIZE);
    yy_switch_to_buffer(my_file_buffer);
}
#endif /*FLEX_SCANNER*/

int yywrap()
{
    return TRUE;
}

/* Parse a move from the given string s */
/* ^ at start of pattern WON'T work here unless using flex */
ChessMove yylexstr(boardIndex, s)
     int boardIndex;
     char *s;
{
    ChessMove ret;
    char *oldStringToLex;
#ifdef FLEX_SCANNER
    YY_BUFFER_STATE buffer, oldBuffer;
#endif
    
    yyboardindex = boardIndex;
    oldStringToLex = StringToLex;
    StringToLex = s;
#ifdef FLEX_SCANNER
    buffer = yy_create_buffer(stdin, YY_BUF_SIZE);
    oldBuffer = YY_CURRENT_BUFFER;
    yy_switch_to_buffer(buffer);
#endif /*FLEX_SCANNER*/

    ret = (ChessMove) yylex();

#ifdef FLEX_SCANNER
    if (oldBuffer != NULL) 
      yy_switch_to_buffer(oldBuffer);
    yy_delete_buffer(buffer);
#endif /*FLEX_SCANNER*/
    StringToLex = oldStringToLex;

    return ret;
}
int yyvstop[] = {
0,

46,
0,

45,
46,
0,

46,
0,

31,
46,
0,

46,
0,

45,
46,
0,

45,
46,
-32,
0,

45,
46,
-32,
0,

46,
-40,
0,

25,
45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

24,
45,
46,
0,

46,
0,

45,
46,
0,

25,
45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

46,
0,

46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
-32,
0,

45,
46,
-32,
0,

46,
-40,
0,

25,
45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

24,
45,
46,
0,

45,
46,
0,

25,
45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

45,
46,
0,

46,
0,

45,
0,

20,
0,

45,
0,

9,
45,
0,

45,
0,

45,
-32,
0,

32,
45,
0,

-40,
0,

40,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

9,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

41,
0,

45,
0,

3,
45,
0,

45,
0,

45,
0,

4,
45,
0,

45,
0,

3,
45,
0,

45,
0,

4,
45,
0,

45,
0,

45,
0,

45,
0,

9,
45,
0,

39,
0,

45,
0,

44,
0,

45,
0,

9,
45,
0,

44,
0,

45,
0,

45,
-32,
0,

44,
0,

32,
45,
0,

-40,
0,

45,
0,

45,
0,

44,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

9,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

3,
45,
0,

44,
0,

45,
0,

45,
0,

4,
45,
0,

45,
0,

3,
45,
0,

44,
0,

45,
0,

4,
45,
0,

45,
0,

45,
0,

45,
0,

9,
45,
0,

18,
0,

9,
45,
0,

29,
45,
0,

29,
0,

8,
45,
0,

32,
0,

28,
45,
0,

28,
0,

30,
0,

45,
0,

45,
0,

45,
0,

45,
0,

6,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

9,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

4,
45,
0,

45,
0,

3,
45,
0,

45,
0,

3,
45,
0,

4,
0,

5,
45,
0,

4,
45,
0,

4,
45,
0,

45,
0,

45,
0,

3,
45,
0,

4,
0,

4,
45,
0,

5,
6,
45,
0,

4,
45,
0,

9,
45,
0,

39,
0,

9,
45,
0,

29,
45,
0,

8,
45,
0,

29,
44,
0,

28,
45,
0,

28,
44,
0,

40,
0,

-40,
0,

45,
0,

45,
0,

44,
0,

45,
0,

44,
0,

45,
0,

6,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

9,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

4,
45,
0,

45,
0,

44,
0,

3,
45,
0,

45,
0,

3,
45,
0,

4,
44,
0,

5,
45,
0,

4,
45,
0,

4,
45,
0,

45,
0,

44,
0,

45,
0,

3,
45,
0,

4,
44,
0,

4,
45,
0,

5,
6,
45,
0,

4,
45,
0,

9,
45,
0,

43,
0,

43,
0,

42,
0,

31,
0,

31,
0,

45,
0,

6,
45,
0,

7,
45,
0,

6,
0,

10,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

23,
45,
0,

45,
0,

19,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

41,
0,

3,
0,

3,
0,

2,
45,
0,

5,
0,

4,
0,

5,
45,
0,

4,
0,

2,
7,
45,
0,

5,
6,
0,

5,
6,
45,
0,

5,
45,
0,

45,
0,

31,
0,

44,
0,

45,
0,

44,
0,

44,
0,

-40,
0,

-40,
0,

6,
45,
0,

44,
0,

7,
45,
0,

6,
44,
0,

45,
0,

45,
0,

44,
0,

45,
0,

45,
0,

45,
0,

45,
0,

23,
45,
0,

45,
0,

19,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

44,
0,

2,
45,
0,

44,
0,

5,
44,
0,

44,
0,

4,
44,
0,

5,
45,
0,

44,
0,

2,
7,
45,
0,

5,
6,
44,
0,

5,
6,
45,
0,

5,
45,
0,

45,
0,

25,
0,

7,
0,

25,
45,
0,

1,
45,
0,

45,
0,

23,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

24,
0,

24,
45,
0,

36,
41,
0,

2,
0,

2,
45,
0,

5,
0,

5,
0,

2,
7,
0,

44,
0,

44,
0,

44,
0,

44,
0,

44,
0,

-40,
0,

-40,
0,

7,
44,
0,

25,
45,
0,

44,
0,

1,
45,
0,

45,
0,

23,
45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

45,
0,

24,
45,
0,

2,
44,
0,

2,
45,
0,

44,
0,

44,
0,

5,
44,
0,

4,
44,
0,

2,
7,
44,
0,

33,
43,
0,

29,
0,

29,
0,

28,
0,

28,
0,

30,
0,

30,
0,

27,
0,

25,
0,

1,
0,

1,
45,
0,

45,
0,

45,
0,

45,
0,

11,
45,
0,

45,
0,

45,
0,

26,
0,

24,
0,

34,
41,
0,

36,
0,

2,
0,

2,
0,

33,
39,
0,

29,
44,
0,

29,
44,
0,

28,
44,
0,

28,
44,
0,

-40,
0,

-40,
0,

1,
44,
0,

1,
45,
0,

45,
0,

45,
0,

45,
0,

11,
45,
0,

45,
0,

45,
0,

44,
0,

44,
0,

2,
44,
0,

5,
44,
0,

30,
0,

25,
0,

1,
0,

1,
0,

23,
0,

45,
0,

45,
0,

45,
0,

11,
45,
0,

45,
0,

45,
0,

24,
0,

-40,
0,

-40,
0,

44,
0,

44,
0,

1,
44,
0,

45,
0,

45,
0,

45,
0,

11,
45,
0,

45,
0,

45,
0,

2,
44,
0,

13,
0,

25,
0,

45,
0,

45,
0,

45,
0,

12,
0,

24,
0,

-40,
0,

-40,
0,

1,
44,
0,

45,
0,

45,
0,

45,
0,

13,
0,

45,
0,

17,
45,
0,

12,
0,

-40,
0,

-40,
0,

44,
0,

45,
0,

17,
45,
0,

22,
0,

45,
0,

45,
0,

-40,
0,

-40,
0,

44,
0,

45,
0,

45,
0,

22,
0,

45,
0,

45,
0,

-40,
0,

-40,
0,

44,
0,

44,
0,

45,
0,

45,
0,

37,
45,
0,

-38,
0,

-38,
-40,
0,

-40,
0,

44,
0,

44,
0,

37,
45,
0,

15,
0,

21,
0,

25,
0,

37,
0,

14,
0,

24,
0,

38,
0,

38,
40,
0,

-40,
0,

37,
44,
0,

27,
0,

21,
0,

26,
0,

-40,
0,

-40,
0,

16,
0,

39,
-35,
0,

35,
0,
0};
# define YYTYPE unsigned short
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,3,	1,3,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,3,	0,0,	1,3,	
1,3,	0,0,	1,3,	0,0,	
1,4,	1,5,	1,3,	1,6,	
1,7,	0,0,	1,4,	1,3,	
1,3,	1,8,	1,9,	1,10,	
1,10,	1,10,	1,10,	1,10,	
1,10,	1,10,	1,10,	1,3,	
1,11,	6,67,	1,3,	35,135,	
40,145,	0,0,	1,4,	1,12,	
1,13,	1,14,	1,4,	1,4,	
1,15,	1,4,	1,4,	19,101,	
1,16,	1,4,	1,17,	1,18,	
1,19,	1,20,	1,18,	1,21,	
1,22,	1,4,	48,160,	62,184,	
1,23,	1,4,	1,4,	7,68,	
1,24,	7,69,	1,3,	8,70,	
1,3,	69,193,	1,25,	1,26,	
1,27,	1,28,	1,29,	1,30,	
1,31,	1,32,	13,93,	33,129,	
8,71,	19,102,	8,72,	8,73,	
1,33,	71,197,	71,198,	2,35,	
1,22,	2,35,	67,191,	72,199,	
48,161,	8,74,	2,6,	61,182,	
1,34,	2,36,	1,3,	73,200,	
2,37,	2,38,	2,39,	2,39,	
2,39,	2,39,	2,39,	2,39,	
2,39,	2,39,	13,94,	2,40,	
14,95,	2,3,	15,97,	77,205,	
17,99,	2,36,	2,41,	2,42,	
2,43,	2,36,	2,36,	2,44,	
2,36,	2,36,	22,106,	2,45,	
2,36,	2,46,	2,47,	2,48,	
2,49,	2,47,	2,50,	2,51,	
2,36,	42,152,	43,154,	2,52,	
2,36,	2,36,	80,206,	2,24,	
14,96,	33,130,	15,98,	2,3,	
17,100,	2,53,	2,54,	2,55,	
2,56,	2,57,	2,58,	2,59,	
2,60,	44,156,	22,107,	46,158,	
51,165,	61,183,	78,206,	2,61,	
78,207,	90,222,	92,224,	2,51,	
4,63,	42,153,	43,155,	93,225,	
94,226,	67,192,	4,63,	2,62,	
95,227,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	96,228,	
97,229,	44,157,	98,229,	46,159,	
51,166,	99,230,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
100,231,	101,232,	102,200,	23,108,	
104,234,	106,236,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
4,63,	4,63,	4,63,	4,63,	
5,64,	37,70,	70,194,	107,237,	
70,195,	76,202,	109,244,	76,203,	
5,64,	5,64,	110,245,	23,109,	
52,108,	66,0,	37,138,	70,196,	
37,72,	37,139,	76,204,	112,111,	
66,189,	66,189,	129,271,	130,200,	
133,273,	134,274,	139,279,	37,140,	
141,281,	66,189,	66,190,	5,64,	
143,282,	5,64,	5,64,	149,290,	
5,64,	151,292,	5,64,	5,65,	
5,0,	152,293,	5,64,	23,110,	
5,64,	5,64,	5,64,	5,66,	
5,66,	131,131,	131,131,	153,294,	
52,167,	138,277,	138,278,	154,295,	
5,66,	5,64,	131,131,	155,296,	
156,297,	157,297,	158,298,	159,299,	
5,64,	5,64,	5,64,	5,64,	
5,64,	5,64,	5,64,	5,64,	
5,64,	160,300,	5,64,	5,64,	
5,64,	5,64,	5,64,	5,64,	
5,64,	5,64,	5,64,	5,64,	
52,168,	161,279,	5,64,	5,64,	
5,64,	9,75,	9,75,	163,302,	
5,64,	165,304,	166,305,	167,306,	
5,64,	5,64,	5,64,	5,64,	
5,64,	5,64,	5,64,	5,64,	
85,215,	85,215,	85,215,	85,215,	
85,215,	85,215,	85,215,	85,215,	
9,76,	168,307,	5,64,	86,216,	
86,216,	86,216,	86,216,	86,216,	
86,216,	86,216,	86,216,	182,326,	
5,64,	9,77,	9,75,	9,78,	
9,79,	9,79,	9,79,	9,79,	
9,79,	9,79,	9,79,	9,79,	
9,79,	9,79,	9,80,	183,279,	
185,328,	186,329,	194,336,	195,337,	
196,338,	197,339,	9,81,	88,220,	
88,220,	88,220,	88,220,	88,220,	
88,220,	88,220,	88,220,	9,81,	
198,340,	199,340,	9,81,	189,328,	
9,81,	9,81,	9,81,	10,75,	
10,75,	202,341,	189,189,	189,189,	
203,342,	204,343,	205,344,	20,86,	
206,344,	222,365,	224,369,	189,189,	
189,190,	9,81,	9,81,	9,81,	
9,81,	9,81,	9,81,	9,81,	
9,81,	225,370,	10,75,	9,81,	
20,89,	226,371,	9,81,	227,372,	
9,81,	9,81,	9,81,	228,372,	
230,375,	231,375,	208,347,	209,349,	
10,75,	210,351,	10,79,	10,79,	
10,79,	10,79,	10,79,	10,79,	
10,79,	10,79,	10,79,	10,79,	
89,221,	89,221,	89,221,	89,221,	
89,221,	89,221,	89,221,	89,221,	
10,81,	20,103,	20,103,	20,103,	
20,103,	20,103,	20,103,	20,103,	
20,103,	10,81,	208,348,	209,350,	
10,81,	210,351,	10,81,	10,81,	
10,81,	184,327,	11,82,	232,376,	
184,131,	184,131,	234,377,	211,352,	
229,373,	235,378,	11,82,	11,83,	
236,379,	184,131,	237,380,	10,81,	
10,81,	10,81,	10,81,	10,81,	
10,81,	10,81,	10,81,	212,354,	
213,356,	10,81,	244,391,	245,392,	
10,81,	253,250,	10,81,	10,81,	
10,81,	11,82,	254,398,	11,82,	
11,82,	260,259,	11,82,	211,353,	
11,82,	11,82,	11,82,	229,374,	
11,82,	190,328,	11,82,	11,82,	
11,82,	11,82,	11,82,	261,405,	
190,333,	190,333,	271,411,	212,355,	
213,357,	277,419,	11,82,	11,82,	
238,381,	190,333,	278,340,	239,383,	
240,385,	241,386,	11,82,	11,82,	
11,82,	11,82,	11,82,	11,82,	
11,82,	11,82,	11,82,	229,374,	
11,82,	11,82,	11,82,	11,82,	
11,82,	11,82,	11,82,	11,82,	
11,82,	11,82,	114,247,	114,247,	
11,82,	11,82,	11,82,	280,420,	
238,382,	207,345,	11,82,	239,384,	
240,385,	241,387,	11,82,	11,82,	
11,82,	11,82,	11,82,	11,82,	
11,82,	11,82,	207,346,	242,388,	
274,413,	114,247,	12,84,	114,248,	
114,114,	281,344,	274,414,	274,415,	
11,82,	276,418,	282,421,	207,346,	
114,114,	289,427,	114,114,	12,85,	
290,428,	12,86,	11,82,	12,87,	
12,87,	12,87,	12,87,	12,87,	
12,87,	12,87,	12,87,	292,432,	
12,88,	293,433,	294,434,	242,389,	
276,418,	295,435,	12,89,	103,233,	
103,233,	103,233,	103,233,	103,233,	
103,233,	103,233,	103,233,	16,85,	
296,435,	16,86,	12,90,	16,87,	
16,87,	16,87,	16,87,	16,87,	
16,87,	16,87,	16,87,	298,437,	
16,88,	297,373,	12,85,	116,249,	
116,249,	116,249,	116,249,	116,249,	
116,249,	116,249,	116,249,	12,91,	
12,91,	12,91,	12,91,	12,91,	
12,91,	12,91,	12,91,	273,412,	
273,412,	299,437,	12,92,	275,416,	
300,438,	302,439,	303,440,	214,358,	
273,412,	243,390,	16,85,	304,441,	
275,417,	214,358,	12,85,	243,390,	
297,436,	305,442,	306,443,	16,91,	
16,91,	16,91,	16,91,	16,91,	
16,91,	16,91,	16,91,	18,85,	
307,444,	18,86,	311,398,	18,87,	
18,87,	18,87,	18,87,	18,87,	
18,87,	18,87,	18,87,	214,358,	
18,88,	243,390,	16,85,	316,405,	
284,422,	214,358,	18,89,	243,390,	
297,436,	326,457,	330,0,	334,0,	
21,85,	284,423,	21,86,	335,0,	
21,87,	21,87,	21,87,	21,87,	
21,87,	21,87,	21,87,	21,87,	
339,200,	21,88,	347,468,	340,460,	
344,462,	345,464,	18,85,	21,89,	
346,466,	348,469,	345,346,	349,470,	
21,104,	350,471,	352,473,	18,91,	
18,91,	18,91,	18,91,	18,91,	
18,91,	18,91,	18,91,	345,346,	
327,458,	346,467,	24,111,	327,131,	
327,131,	351,472,	353,474,	21,85,	
354,475,	355,476,	24,112,	24,112,	
327,131,	356,477,	18,85,	357,478,	
21,91,	21,91,	21,91,	21,91,	
21,105,	21,91,	21,91,	21,91,	
118,257,	118,257,	118,257,	118,257,	
118,257,	118,257,	118,257,	118,257,	
365,482,	24,112,	369,482,	24,111,	
24,111,	351,472,	24,111,	21,85,	
24,111,	24,111,	24,111,	358,479,	
24,111,	333,328,	24,113,	24,111,	
24,111,	24,114,	24,114,	370,484,	
333,459,	333,459,	371,484,	373,487,	
376,200,	377,490,	24,114,	24,111,	
374,488,	333,459,	340,461,	344,463,	
345,465,	378,491,	24,114,	24,114,	
24,114,	24,114,	24,114,	24,114,	
24,114,	24,114,	24,114,	358,479,	
24,114,	24,114,	24,114,	24,114,	
24,114,	24,114,	24,114,	24,114,	
24,114,	24,114,	379,492,	373,487,	
24,114,	24,114,	24,114,	380,493,	
374,489,	381,494,	24,115,	382,495,	
383,496,	384,497,	24,114,	24,114,	
24,114,	24,114,	24,114,	24,114,	
24,114,	24,114,	119,121,	119,121,	
119,121,	119,121,	119,121,	119,121,	
119,121,	119,121,	25,116,	372,485,	
24,114,	386,499,	25,117,	25,117,	
25,117,	25,117,	25,117,	25,117,	
25,117,	25,117,	24,111,	25,118,	
385,498,	387,500,	26,84,	120,258,	
120,258,	120,258,	120,258,	120,258,	
120,258,	120,258,	120,258,	388,501,	
389,502,	391,504,	392,504,	26,122,	
396,398,	26,86,	401,405,	26,123,	
26,123,	26,123,	26,123,	26,123,	
26,123,	26,123,	26,123,	390,503,	
26,124,	25,119,	403,402,	404,514,	
385,498,	372,486,	26,89,	411,200,	
416,520,	417,521,	25,120,	25,121,	
25,120,	25,120,	25,120,	25,120,	
25,120,	25,120,	26,90,	122,262,	
122,262,	122,262,	122,262,	122,262,	
122,262,	122,262,	122,262,	412,516,	
412,516,	419,279,	26,125,	390,503,	
420,523,	25,119,	421,525,	422,527,	
412,516,	372,486,	423,528,	26,126,	
26,127,	26,126,	26,127,	26,127,	
26,127,	26,127,	26,127,	27,116,	
425,529,	428,530,	26,92,	27,117,	
27,117,	27,117,	27,117,	27,117,	
27,117,	27,117,	27,117,	432,530,	
27,118,	433,533,	26,125,	124,267,	
124,267,	124,267,	124,267,	124,267,	
124,267,	124,267,	124,267,	28,116,	
434,533,	438,279,	27,93,	28,117,	
28,117,	28,117,	28,117,	28,117,	
28,117,	28,117,	28,117,	439,537,	
28,118,	440,538,	436,535,	441,539,	
442,540,	443,541,	27,119,	125,268,	
125,268,	125,268,	125,268,	125,268,	
125,268,	125,268,	125,268,	27,120,	
27,121,	27,120,	27,121,	27,120,	
27,120,	27,120,	27,128,	444,541,	
28,95,	445,542,	450,547,	451,514,	
452,548,	457,279,	28,119,	420,524,	
459,550,	421,526,	436,536,	466,467,	
468,559,	469,560,	27,119,	28,120,	
28,120,	28,121,	28,120,	28,121,	
28,120,	28,120,	28,120,	128,258,	
128,258,	128,258,	128,258,	128,258,	
128,258,	128,258,	128,258,	435,485,	
28,96,	470,561,	471,562,	29,116,	
473,565,	474,566,	28,119,	29,117,	
29,117,	29,117,	29,117,	29,117,	
29,117,	29,117,	29,117,	414,70,	
29,118,	140,280,	140,137,	140,137,	
140,137,	140,137,	140,137,	140,137,	
140,137,	475,567,	476,568,	30,116,	
414,517,	467,557,	414,72,	30,117,	
30,117,	30,117,	30,117,	30,117,	
30,117,	30,117,	30,117,	477,569,	
30,118,	414,74,	478,570,	128,226,	
467,558,	435,534,	29,119,	482,574,	
486,485,	488,585,	415,518,	489,586,	
490,587,	491,587,	49,86,	29,120,	
29,120,	29,120,	29,121,	29,120,	
29,121,	29,120,	29,120,	415,519,	
479,571,	415,78,	492,588,	484,578,	
493,589,	487,583,	30,119,	49,89,	
494,590,	495,591,	496,592,	497,593,	
415,80,	435,534,	29,119,	30,120,	
30,120,	30,120,	30,120,	30,121,	
30,120,	30,121,	30,120,	31,116,	
472,563,	499,596,	500,597,	31,117,	
31,117,	31,117,	31,117,	31,117,	
31,117,	31,117,	31,117,	484,578,	
31,118,	487,584,	30,119,	472,564,	
49,162,	49,162,	49,162,	49,162,	
49,162,	49,162,	49,162,	49,162,	
501,598,	502,599,	504,602,	479,572,	
511,510,	512,606,	32,116,	513,514,	
31,97,	250,396,	32,117,	32,117,	
32,117,	32,117,	32,117,	32,117,	
32,117,	32,117,	31,119,	32,118,	
520,608,	250,396,	472,563,	472,564,	
250,396,	250,396,	521,609,	31,120,	
31,120,	31,120,	31,120,	31,120,	
31,121,	31,120,	31,121,	479,572,	
447,450,	527,614,	34,131,	528,615,	
31,98,	250,396,	530,574,	508,508,	
508,508,	503,600,	34,131,	34,132,	
447,450,	32,119,	31,119,	447,450,	
447,450,	250,396,	531,616,	534,485,	
250,396,	250,396,	32,120,	32,120,	
32,120,	32,120,	32,120,	32,120,	
32,121,	32,120,	508,508,	535,619,	
447,450,	34,131,	536,620,	34,131,	
34,131,	537,621,	34,131,	538,621,	
34,131,	34,131,	34,131,	539,622,	
34,131,	32,119,	34,131,	34,131,	
34,131,	34,133,	34,133,	458,549,	
540,623,	541,602,	458,131,	458,131,	
543,606,	546,627,	34,133,	34,131,	
503,601,	533,618,	518,202,	458,131,	
518,203,	551,0,	34,131,	34,131,	
34,131,	34,131,	34,131,	34,131,	
34,131,	34,131,	34,131,	518,204,	
34,131,	34,131,	34,131,	34,131,	
34,131,	34,131,	34,131,	34,131,	
34,131,	34,131,	552,0,	553,0,	
34,131,	34,131,	34,131,	508,604,	
503,601,	533,618,	34,131,	554,0,	
555,0,	556,0,	34,131,	34,131,	
34,131,	34,131,	34,131,	34,131,	
34,131,	34,131,	146,285,	146,285,	
146,285,	146,285,	146,285,	146,285,	
146,285,	146,285,	557,629,	558,630,	
34,131,	559,631,	36,63,	560,632,	
561,633,	562,634,	564,635,	565,636,	
36,136,	498,594,	34,134,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,137,	563,564,	566,636,	
498,595,	567,637,	568,638,	569,639,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	563,564,	498,594,	
498,595,	570,640,	572,571,	576,575,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	36,136,	36,136,	
36,136,	36,136,	38,75,	38,75,	
148,289,	148,289,	148,289,	148,289,	
148,289,	148,289,	148,289,	148,289,	
162,301,	162,301,	162,301,	162,301,	
162,301,	162,301,	162,301,	162,301,	
574,644,	577,645,	578,99,	580,647,	
581,649,	38,76,	169,308,	169,308,	
169,308,	169,308,	169,308,	169,308,	
169,308,	169,308,	583,652,	584,653,	
585,654,	586,655,	38,141,	38,75,	
38,78,	38,142,	38,142,	38,142,	
38,142,	38,142,	38,142,	38,142,	
38,142,	38,142,	38,142,	38,143,	
574,644,	588,658,	578,100,	580,648,	
581,649,	582,650,	589,659,	38,144,	
171,314,	171,314,	171,314,	171,314,	
171,314,	171,314,	171,314,	171,314,	
38,144,	571,641,	590,660,	38,144,	
591,661,	38,144,	38,144,	38,144,	
39,75,	39,75,	172,174,	172,174,	
172,174,	172,174,	172,174,	172,174,	
172,174,	172,174,	592,662,	593,663,	
595,664,	582,651,	38,144,	38,144,	
38,144,	38,144,	38,144,	38,144,	
38,144,	38,144,	87,217,	39,75,	
38,144,	516,131,	516,131,	38,144,	
594,595,	38,144,	38,144,	38,144,	
571,642,	596,665,	516,131,	87,218,	
597,665,	39,75,	598,666,	39,142,	
39,142,	39,142,	39,142,	39,142,	
39,142,	39,142,	39,142,	39,142,	
39,142,	173,315,	173,315,	173,315,	
173,315,	173,315,	173,315,	173,315,	
173,315,	39,144,	549,628,	599,667,	
594,595,	549,131,	549,131,	601,600,	
571,642,	87,217,	39,144,	600,668,	
605,606,	39,144,	549,131,	39,144,	
39,144,	39,144,	87,219,	87,219,	
87,219,	87,219,	87,219,	87,219,	
87,219,	87,219,	608,670,	609,671,	
614,672,	615,673,	617,645,	619,677,	
39,144,	39,144,	39,144,	39,144,	
39,144,	39,144,	39,144,	39,144,	
41,84,	87,217,	39,144,	620,678,	
622,681,	39,144,	516,607,	39,144,	
39,144,	39,144,	600,669,	623,682,	
626,683,	41,146,	629,685,	41,86,	
630,686,	41,147,	41,147,	41,147,	
41,147,	41,147,	41,147,	41,147,	
41,147,	631,687,	41,148,	632,688,	
633,689,	634,690,	637,692,	638,692,	
41,89,	175,317,	175,317,	175,317,	
175,317,	175,317,	175,317,	175,317,	
175,317,	45,146,	600,669,	45,86,	
41,149,	45,147,	45,147,	45,147,	
45,147,	45,147,	45,147,	45,147,	
45,147,	618,158,	45,148,	635,691,	
41,146,	177,322,	177,322,	177,322,	
177,322,	177,322,	177,322,	177,322,	
177,322,	41,150,	41,150,	41,150,	
41,150,	41,150,	41,150,	41,150,	
41,150,	639,693,	628,684,	640,693,	
41,151,	628,131,	628,131,	643,645,	
646,0,	642,695,	647,698,	644,696,	
45,146,	618,159,	628,131,	635,691,	
41,146,	648,699,	649,700,	650,701,	
651,702,	45,150,	45,150,	45,150,	
45,150,	45,150,	45,150,	45,150,	
45,150,	47,146,	652,703,	47,86,	
653,704,	47,147,	47,147,	47,147,	
47,147,	47,147,	47,147,	47,147,	
47,147,	642,695,	47,148,	644,697,	
45,146,	654,705,	655,705,	656,657,	
47,89,	658,706,	659,707,	660,708,	
661,709,	662,710,	50,146,	663,711,	
50,86,	666,713,	50,147,	50,147,	
50,147,	50,147,	50,147,	50,147,	
50,147,	50,147,	664,712,	50,148,	
667,713,	669,715,	670,716,	671,717,	
47,146,	50,89,	672,718,	673,719,	
676,720,	677,721,	50,163,	656,657,	
678,721,	47,150,	47,150,	47,150,	
47,150,	47,150,	47,150,	47,150,	
47,150,	178,323,	178,323,	178,323,	
178,323,	178,323,	178,323,	178,323,	
178,323,	50,146,	664,712,	681,722,	
682,723,	669,715,	686,464,	687,725,	
47,146,	688,725,	50,150,	50,150,	
50,150,	50,150,	50,164,	50,150,	
50,150,	50,150,	53,169,	689,693,	
690,693,	692,727,	53,170,	53,170,	
53,170,	53,170,	53,170,	53,170,	
53,170,	53,170,	679,680,	53,171,	
691,726,	50,146,	54,84,	181,315,	
181,315,	181,315,	181,315,	181,315,	
181,315,	181,315,	181,315,	694,0,	
695,730,	696,731,	697,732,	54,175,	
698,733,	54,86,	699,734,	54,176,	
54,176,	54,176,	54,176,	54,176,	
54,176,	54,176,	54,176,	701,735,	
54,177,	53,172,	679,680,	702,736,	
691,726,	703,737,	54,89,	704,737,	
706,739,	707,739,	53,173,	53,174,	
53,173,	53,173,	53,173,	53,173,	
53,173,	53,173,	54,149,	215,359,	
215,359,	215,359,	215,359,	215,359,	
215,359,	215,359,	215,359,	181,294,	
708,740,	686,465,	54,178,	709,740,	
710,713,	53,172,	711,713,	714,0,	
715,744,	716,745,	717,746,	54,179,	
54,180,	54,179,	54,180,	54,180,	
54,180,	54,180,	54,180,	55,169,	
718,747,	719,748,	54,151,	55,170,	
55,170,	55,170,	55,170,	55,170,	
55,170,	55,170,	55,170,	722,751,	
55,171,	723,751,	54,178,	216,360,	
216,360,	216,360,	216,360,	216,360,	
216,360,	216,360,	216,360,	56,169,	
725,753,	726,754,	55,152,	56,170,	
56,170,	56,170,	56,170,	56,170,	
56,170,	56,170,	56,170,	712,741,	
56,171,	727,755,	728,729,	731,757,	
730,756,	732,758,	55,172,	217,219,	
217,219,	217,219,	217,219,	217,219,	
217,219,	217,219,	217,219,	55,173,	
55,174,	55,173,	55,174,	55,173,	
55,173,	55,173,	55,181,	684,724,	
56,154,	724,752,	684,131,	684,131,	
724,131,	724,131,	56,172,	712,741,	
733,759,	727,755,	728,729,	684,131,	
730,756,	724,131,	55,172,	56,173,	
56,173,	56,174,	56,173,	56,174,	
56,173,	56,173,	56,173,	218,361,	
218,361,	218,361,	218,361,	218,361,	
218,361,	218,361,	218,361,	734,759,	
56,155,	735,760,	736,761,	57,169,	
740,765,	741,766,	56,172,	57,170,	
57,170,	57,170,	57,170,	57,170,	
57,170,	57,170,	57,170,	745,768,	
57,171,	219,362,	219,362,	219,362,	
219,362,	219,362,	219,362,	219,362,	
219,362,	746,769,	747,770,	58,169,	
748,771,	757,783,	758,784,	58,170,	
58,170,	58,170,	58,170,	58,170,	
58,170,	58,170,	58,170,	742,743,	
58,171,	744,767,	754,778,	760,788,	
755,779,	756,781,	57,172,	220,363,	
220,363,	220,363,	220,363,	220,363,	
220,363,	220,363,	220,363,	57,173,	
57,173,	57,173,	57,174,	57,173,	
57,174,	57,173,	57,173,	752,775,	
738,0,	761,789,	752,131,	752,131,	
763,0,	766,796,	58,172,	742,743,	
768,799,	744,767,	754,778,	752,131,	
755,780,	756,782,	57,172,	58,173,	
58,173,	58,173,	58,173,	58,174,	
58,173,	58,174,	58,173,	59,169,	
764,0,	769,800,	485,579,	59,170,	
59,170,	59,170,	59,170,	59,170,	
59,170,	59,170,	59,170,	770,801,	
59,171,	766,796,	58,172,	221,364,	
221,364,	221,364,	221,364,	221,364,	
221,364,	221,364,	221,364,	771,802,	
776,808,	779,813,	780,814,	485,580,	
485,581,	700,580,	60,169,	738,763,	
59,156,	259,401,	60,170,	60,170,	
60,170,	60,170,	60,170,	60,170,	
60,170,	60,170,	59,172,	60,171,	
485,582,	259,401,	700,582,	763,792,	
259,401,	259,401,	781,815,	59,173,	
59,173,	59,173,	59,173,	59,173,	
59,174,	59,173,	59,174,	485,580,	
485,581,	700,580,	64,185,	738,764,	
59,157,	259,401,	782,816,	783,817,	
784,818,	786,819,	64,185,	64,185,	
787,820,	60,172,	59,172,	788,821,	
485,582,	259,401,	700,582,	763,705,	
259,401,	259,401,	60,173,	60,173,	
60,173,	60,173,	60,173,	60,173,	
60,174,	60,173,	767,797,	777,809,	
753,776,	64,185,	789,822,	64,185,	
64,185,	792,0,	64,185,	764,793,	
64,185,	64,186,	64,0,	765,794,	
64,185,	60,172,	64,185,	64,185,	
64,185,	64,185,	64,185,	794,826,	
797,831,	798,832,	753,777,	778,811,	
793,0,	799,833,	64,185,	64,185,	
795,827,	796,829,	767,798,	777,810,	
753,776,	765,795,	64,185,	64,185,	
64,185,	64,185,	64,185,	64,185,	
64,185,	64,185,	64,185,	765,794,	
64,185,	64,185,	64,185,	64,185,	
64,185,	64,185,	64,185,	64,185,	
64,185,	64,185,	753,777,	778,812,	
64,185,	64,185,	64,185,	249,259,	
795,828,	796,830,	64,185,	544,546,	
792,825,	765,795,	64,185,	64,185,	
64,185,	64,185,	64,185,	64,185,	
64,185,	64,185,	65,186,	544,546,	
800,834,	801,835,	544,546,	544,546,	
249,260,	802,836,	65,186,	65,186,	
64,185,	249,261,	251,255,	251,255,	
251,255,	251,255,	251,255,	251,255,	
251,255,	251,255,	64,185,	544,546,	
804,749,	249,261,	809,841,	775,807,	
249,261,	249,261,	775,131,	775,131,	
808,777,	65,186,	810,841,	65,186,	
65,186,	811,842,	65,186,	775,131,	
65,186,	65,187,	65,188,	793,825,	
65,186,	249,261,	65,186,	65,186,	
65,186,	65,186,	65,186,	807,840,	
812,843,	813,844,	807,131,	807,131,	
804,838,	249,261,	65,186,	65,186,	
249,261,	249,261,	814,844,	807,131,	
808,777,	815,845,	65,186,	65,186,	
65,186,	65,186,	65,186,	65,186,	
65,186,	65,186,	65,186,	816,845,	
65,186,	65,186,	65,186,	65,186,	
65,186,	65,186,	65,186,	65,186,	
65,186,	65,186,	75,75,	75,75,	
65,186,	65,186,	65,186,	817,846,	
818,847,	587,656,	65,186,	819,848,	
258,402,	820,849,	65,186,	65,186,	
65,186,	65,186,	65,186,	65,186,	
65,186,	65,186,	314,447,	587,657,	
821,850,	75,75,	822,851,	825,0,	
827,853,	826,795,	828,853,	314,448,	
65,186,	258,403,	829,854,	84,208,	
830,855,	621,679,	258,404,	75,75,	
84,209,	587,656,	65,186,	314,449,	
831,856,	832,856,	84,210,	84,211,	
314,450,	84,212,	258,404,	621,680,	
84,213,	258,404,	258,404,	587,657,	
834,858,	84,214,	836,860,	75,201,	
314,450,	826,795,	842,863,	314,450,	
314,450,	843,863,	844,864,	84,208,	
75,201,	621,679,	258,404,	75,201,	
84,209,	75,201,	75,201,	75,201,	
79,75,	79,75,	84,210,	84,211,	
314,450,	84,212,	258,404,	621,680,	
84,213,	258,404,	258,404,	845,865,	
846,866,	84,214,	75,201,	75,201,	
75,201,	75,201,	75,201,	75,201,	
75,201,	75,201,	844,864,	79,75,	
75,201,	147,286,	847,867,	75,201,	
848,785,	75,201,	75,201,	75,201,	
849,785,	850,868,	851,869,	854,870,	
855,870,	79,75,	147,287,	79,79,	
79,79,	79,79,	79,79,	79,79,	
79,79,	79,79,	79,79,	79,79,	
79,79,	856,871,	858,872,	860,873,	
865,641,	866,875,	91,217,	867,876,	
868,785,	79,81,	91,223,	91,223,	
91,223,	91,223,	91,223,	91,223,	
91,223,	91,223,	79,81,	91,218,	
147,286,	79,81,	869,785,	79,81,	
79,81,	79,81,	871,668,	872,877,	
873,878,	147,288,	147,288,	147,288,	
147,288,	147,288,	147,288,	147,288,	
147,288,	875,880,	876,880,	878,835,	
79,81,	79,81,	79,81,	79,81,	
79,81,	79,81,	79,81,	79,81,	
882,883,	91,217,	79,81,	884,885,	
147,286,	79,81,	885,886,	79,81,	
79,81,	79,81,	91,219,	91,219,	
91,219,	91,219,	91,219,	91,219,	
91,219,	91,219,	105,217,	886,887,	
887,888,	888,889,	105,223,	105,223,	
105,223,	105,223,	105,223,	105,223,	
105,223,	105,223,	889,890,	105,218,	
882,883,	91,217,	252,397,	252,397,	
252,397,	252,397,	252,397,	252,397,	
252,397,	252,397,	890,891,	308,259,	
840,862,	891,892,	108,238,	840,131,	
840,131,	892,893,	893,894,	108,239,	
894,895,	223,366,	895,896,	896,897,	
840,131,	108,240,	108,241,	897,898,	
898,899,	105,217,	899,900,	108,242,	
308,260,	0,0,	223,367,	0,0,	
108,243,	308,316,	105,219,	105,219,	
105,219,	105,219,	105,219,	105,219,	
105,219,	105,219,	108,238,	111,111,	
111,111,	308,316,	402,513,	108,239,	
308,316,	308,316,	0,0,	0,0,	
105,235,	108,240,	108,241,	0,0,	
0,0,	105,217,	402,513,	108,242,	
223,366,	402,513,	402,513,	0,0,	
108,243,	308,316,	111,111,	0,0,	
0,0,	223,368,	223,368,	223,368,	
223,368,	223,368,	223,368,	223,368,	
223,368,	308,316,	402,513,	111,111,	
308,316,	308,316,	111,111,	111,111,	
862,874,	0,0,	0,0,	862,131,	
862,131,	0,0,	402,513,	111,111,	
223,366,	402,513,	402,513,	0,0,	
862,131,	0,0,	0,0,	111,111,	
111,111,	111,111,	111,111,	111,111,	
111,111,	111,111,	111,111,	111,111,	
0,0,	111,111,	111,111,	111,111,	
111,111,	111,111,	111,111,	111,111,	
111,111,	111,111,	111,111,	113,111,	
113,111,	111,111,	111,111,	111,111,	
0,0,	879,881,	0,0,	0,0,	
624,626,	0,0,	759,785,	111,111,	
111,111,	111,111,	111,111,	111,111,	
111,111,	111,111,	111,111,	759,786,	
624,626,	674,676,	113,111,	624,626,	
624,626,	0,0,	0,0,	0,0,	
0,0,	111,111,	0,0,	0,0,	
0,0,	674,676,	0,0,	113,246,	
674,676,	674,676,	113,111,	113,111,	
624,626,	0,0,	759,785,	879,131,	
879,131,	0,0,	0,0,	113,111,	
0,0,	0,0,	693,728,	759,787,	
879,131,	674,676,	0,0,	113,111,	
113,111,	113,111,	113,111,	113,111,	
113,111,	113,111,	113,111,	113,111,	
693,729,	113,111,	113,111,	113,111,	
113,111,	113,111,	113,111,	113,111,	
113,111,	113,111,	113,111,	0,0,	
0,0,	113,111,	113,111,	113,111,	
117,250,	0,0,	693,728,	0,0,	
0,0,	117,251,	0,0,	113,111,	
113,111,	113,111,	113,111,	113,111,	
113,111,	113,111,	113,111,	0,0,	
693,729,	0,0,	117,252,	0,0,	
0,0,	117,253,	0,0,	0,0,	
0,0,	113,111,	117,254,	255,399,	
255,399,	255,399,	255,399,	255,399,	
255,399,	255,399,	255,399,	0,0,	
0,0,	0,0,	117,254,	0,0,	
0,0,	117,254,	117,254,	0,0,	
0,0,	0,0,	0,0,	0,0,	
117,251,	263,265,	263,265,	263,265,	
263,265,	263,265,	263,265,	263,265,	
263,265,	117,255,	117,256,	117,255,	
117,255,	117,255,	117,255,	117,255,	
117,255,	874,879,	233,366,	0,0,	
874,131,	874,131,	117,254,	0,0,	
121,259,	117,254,	117,254,	0,0,	
0,0,	874,131,	0,0,	233,367,	
117,251,	121,258,	121,258,	121,258,	
121,258,	121,258,	121,258,	121,258,	
121,258,	0,0,	123,250,	256,398,	
0,0,	121,260,	0,0,	123,263,	
0,0,	0,0,	121,261,	256,399,	
256,399,	256,399,	256,399,	256,399,	
256,399,	256,399,	256,399,	0,0,	
123,264,	233,366,	121,261,	123,253,	
713,742,	121,261,	121,261,	0,0,	
123,254,	0,0,	233,368,	233,368,	
233,368,	233,368,	233,368,	233,368,	
233,368,	233,368,	713,743,	0,0,	
123,254,	0,0,	121,261,	123,254,	
123,254,	0,0,	0,0,	0,0,	
0,0,	0,0,	123,263,	0,0,	
0,0,	233,366,	121,261,	449,447,	
713,742,	121,261,	121,261,	123,265,	
123,266,	123,265,	123,265,	123,265,	
123,265,	123,265,	123,265,	0,0,	
0,0,	0,0,	713,743,	0,0,	
123,254,	0,0,	126,259,	123,254,	
123,254,	0,0,	0,0,	126,217,	
0,0,	449,450,	123,263,	126,269,	
126,269,	126,269,	126,269,	126,269,	
126,269,	126,269,	126,269,	0,0,	
126,218,	449,450,	0,0,	126,260,	
449,450,	449,450,	0,0,	0,0,	
126,261,	264,406,	264,406,	264,406,	
264,406,	264,406,	264,406,	264,406,	
264,406,	0,0,	0,0,	0,0,	
126,261,	449,450,	0,0,	126,261,	
126,261,	0,0,	0,0,	0,0,	
0,0,	0,0,	126,217,	265,407,	
265,407,	265,407,	265,407,	265,407,	
265,407,	265,407,	265,407,	126,219,	
126,270,	126,219,	126,219,	126,219,	
126,219,	126,219,	126,219,	0,0,	
0,0,	0,0,	0,0,	0,0,	
126,261,	0,0,	0,0,	126,261,	
126,261,	0,0,	0,0,	127,217,	
0,0,	0,0,	126,217,	127,269,	
127,269,	127,269,	127,269,	127,269,	
127,269,	127,269,	127,269,	266,398,	
127,218,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	266,407,	
266,407,	266,407,	266,407,	266,407,	
266,407,	266,407,	266,407,	0,0,	
0,0,	0,0,	0,0,	132,132,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	132,132,	
0,0,	0,0,	127,217,	267,408,	
267,408,	267,408,	267,408,	267,408,	
267,408,	267,408,	267,408,	127,219,	
127,219,	127,219,	127,219,	127,219,	
127,219,	127,219,	127,219,	0,0,	
0,0,	0,0,	132,132,	0,0,	
132,132,	132,132,	0,0,	132,132,	
0,0,	132,132,	132,132,	132,132,	
0,0,	132,132,	127,217,	132,132,	
132,132,	132,132,	132,132,	132,132,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	132,132,	
132,132,	0,0,	880,882,	0,0,	
0,0,	0,0,	0,0,	132,132,	
132,132,	132,132,	132,132,	132,132,	
132,132,	132,132,	132,132,	132,132,	
880,883,	132,132,	132,132,	132,132,	
132,132,	132,132,	132,132,	132,132,	
132,132,	132,132,	132,132,	0,0,	
0,0,	132,132,	132,132,	132,132,	
0,0,	0,0,	880,882,	132,132,	
0,0,	0,0,	0,0,	132,132,	
132,132,	132,132,	132,132,	132,132,	
132,132,	132,132,	132,132,	135,135,	
880,883,	0,0,	270,405,	0,0,	
0,0,	0,0,	0,0,	135,135,	
135,135,	132,132,	270,362,	270,362,	
270,362,	270,362,	270,362,	270,362,	
270,362,	270,362,	0,0,	132,272,	
285,424,	285,424,	285,424,	285,424,	
285,424,	285,424,	285,424,	285,424,	
0,0,	0,0,	135,275,	0,0,	
135,135,	135,135,	0,0,	135,135,	
0,0,	135,135,	135,135,	135,135,	
0,0,	135,135,	0,0,	135,135,	
135,135,	135,135,	135,135,	135,135,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	135,135,	
135,135,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	135,135,	
135,135,	135,135,	135,135,	135,135,	
135,135,	135,135,	135,135,	135,135,	
0,0,	135,135,	135,135,	135,135,	
135,135,	135,135,	135,135,	135,135,	
135,135,	135,135,	135,135,	0,0,	
0,0,	135,135,	135,135,	135,135,	
0,0,	0,0,	0,0,	135,135,	
0,0,	0,0,	0,0,	135,135,	
135,135,	135,135,	135,135,	135,135,	
135,135,	135,135,	135,135,	137,137,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	137,137,	
137,276,	135,135,	286,288,	286,288,	
286,288,	286,288,	286,288,	286,288,	
286,288,	286,288,	0,0,	135,135,	
287,425,	287,425,	287,425,	287,425,	
287,425,	287,425,	287,425,	287,425,	
0,0,	0,0,	137,137,	0,0,	
137,137,	137,137,	0,0,	137,137,	
0,0,	137,137,	137,137,	137,137,	
0,0,	137,137,	0,0,	137,137,	
137,137,	137,137,	137,137,	137,137,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	137,137,	
137,137,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	137,137,	
137,137,	137,137,	137,137,	137,137,	
137,137,	137,137,	137,137,	137,137,	
0,0,	137,137,	137,137,	137,137,	
137,137,	137,137,	137,137,	137,137,	
137,137,	137,137,	137,137,	0,0,	
0,0,	137,137,	137,137,	137,137,	
142,75,	142,75,	0,0,	137,137,	
0,0,	0,0,	0,0,	137,137,	
137,137,	137,137,	137,137,	137,137,	
137,137,	137,137,	137,137,	288,426,	
288,426,	288,426,	288,426,	288,426,	
288,426,	288,426,	288,426,	142,75,	
0,0,	137,137,	309,312,	309,312,	
309,312,	309,312,	309,312,	309,312,	
309,312,	309,312,	0,0,	137,137,	
0,0,	142,75,	0,0,	142,142,	
142,142,	142,142,	142,142,	142,142,	
142,142,	142,142,	142,142,	142,142,	
142,142,	310,445,	310,445,	310,445,	
310,445,	310,445,	310,445,	310,445,	
310,445,	142,144,	312,446,	312,446,	
312,446,	312,446,	312,446,	312,446,	
312,446,	312,446,	142,144,	0,0,	
0,0,	142,144,	0,0,	142,144,	
142,144,	142,144,	0,0,	145,145,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	145,145,	
145,283,	0,0,	0,0,	0,0,	
142,144,	142,144,	142,144,	142,144,	
142,144,	142,144,	142,144,	142,144,	
0,0,	0,0,	142,144,	0,0,	
0,0,	142,144,	0,0,	142,144,	
142,144,	142,144,	145,284,	0,0,	
145,145,	145,145,	0,0,	145,145,	
0,0,	145,145,	145,145,	145,145,	
0,0,	145,145,	0,0,	145,145,	
145,145,	145,145,	145,145,	145,145,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	145,145,	
145,145,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	145,145,	
145,145,	145,145,	145,145,	145,145,	
145,145,	145,145,	145,145,	145,145,	
0,0,	145,145,	145,145,	145,145,	
145,145,	145,145,	145,145,	145,145,	
145,145,	145,145,	145,145,	0,0,	
0,0,	145,145,	145,145,	145,145,	
0,0,	0,0,	0,0,	145,145,	
0,0,	0,0,	0,0,	145,145,	
145,145,	145,145,	145,145,	145,145,	
145,145,	145,145,	145,145,	318,320,	
318,320,	318,320,	318,320,	318,320,	
318,320,	318,320,	318,320,	150,286,	
0,0,	145,145,	0,0,	150,291,	
150,291,	150,291,	150,291,	150,291,	
150,291,	150,291,	150,291,	145,145,	
150,287,	319,452,	319,452,	319,452,	
319,452,	319,452,	319,452,	319,452,	
319,452,	0,0,	0,0,	164,286,	
0,0,	0,0,	0,0,	164,291,	
164,291,	164,291,	164,291,	164,291,	
164,291,	164,291,	164,291,	313,398,	
164,287,	0,0,	0,0,	0,0,	
0,0,	0,0,	150,286,	313,446,	
313,446,	313,446,	313,446,	313,446,	
313,446,	313,446,	313,446,	150,288,	
150,288,	150,288,	150,288,	150,288,	
150,288,	150,288,	150,288,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	170,250,	164,286,	0,0,	
0,0,	0,0,	170,309,	0,0,	
0,0,	0,0,	150,286,	164,288,	
164,288,	164,288,	164,288,	164,288,	
164,288,	164,288,	164,288,	170,310,	
0,0,	0,0,	170,253,	0,0,	
0,0,	0,0,	0,0,	170,311,	
0,0,	164,303,	0,0,	0,0,	
0,0,	0,0,	164,286,	0,0,	
0,0,	0,0,	0,0,	170,311,	
0,0,	0,0,	170,311,	170,311,	
0,0,	0,0,	0,0,	0,0,	
0,0,	170,309,	320,453,	320,453,	
320,453,	320,453,	320,453,	320,453,	
320,453,	320,453,	170,312,	170,313,	
170,312,	170,312,	170,312,	170,312,	
170,312,	170,312,	0,0,	291,429,	
0,0,	0,0,	0,0,	170,311,	
0,0,	174,259,	170,311,	170,311,	
0,0,	0,0,	0,0,	0,0,	
291,430,	170,309,	174,315,	174,315,	
174,315,	174,315,	174,315,	174,315,	
174,315,	174,315,	0,0,	176,250,	
321,398,	0,0,	174,260,	0,0,	
176,318,	0,0,	0,0,	174,316,	
321,453,	321,453,	321,453,	321,453,	
321,453,	321,453,	321,453,	321,453,	
0,0,	176,319,	291,429,	174,316,	
176,253,	0,0,	174,316,	174,316,	
0,0,	176,311,	0,0,	291,431,	
291,431,	291,431,	291,431,	291,431,	
291,431,	291,431,	291,431,	0,0,	
0,0,	176,311,	0,0,	174,316,	
176,311,	176,311,	0,0,	0,0,	
0,0,	0,0,	0,0,	176,318,	
0,0,	0,0,	291,429,	174,316,	
0,0,	0,0,	174,316,	174,316,	
176,320,	176,321,	176,320,	176,320,	
176,320,	176,320,	176,320,	176,320,	
0,0,	0,0,	0,0,	0,0,	
0,0,	176,311,	0,0,	179,259,	
176,311,	176,311,	0,0,	0,0,	
179,286,	0,0,	0,0,	176,318,	
179,324,	179,324,	179,324,	179,324,	
179,324,	179,324,	179,324,	179,324,	
0,0,	179,287,	325,405,	0,0,	
179,260,	0,0,	0,0,	0,0,	
0,0,	179,316,	325,426,	325,426,	
325,426,	325,426,	325,426,	325,426,	
325,426,	325,426,	0,0,	0,0,	
0,0,	179,316,	0,0,	0,0,	
179,316,	179,316,	0,0,	0,0,	
0,0,	0,0,	0,0,	179,286,	
361,481,	361,481,	361,481,	361,481,	
361,481,	361,481,	361,481,	361,481,	
179,288,	179,325,	179,288,	179,288,	
179,288,	179,288,	179,288,	179,288,	
0,0,	0,0,	0,0,	0,0,	
0,0,	179,316,	0,0,	0,0,	
179,316,	179,316,	0,0,	0,0,	
180,286,	0,0,	0,0,	179,286,	
180,324,	180,324,	180,324,	180,324,	
180,324,	180,324,	180,324,	180,324,	
0,0,	180,287,	366,368,	366,368,	
366,368,	366,368,	366,368,	366,368,	
366,368,	366,368,	367,480,	367,480,	
367,480,	367,480,	367,480,	367,480,	
367,480,	367,480,	0,0,	0,0,	
187,187,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
187,187,	187,187,	0,0,	180,286,	
368,483,	368,483,	368,483,	368,483,	
368,483,	368,483,	368,483,	368,483,	
180,288,	180,288,	180,288,	180,288,	
180,288,	180,288,	180,288,	180,288,	
0,0,	0,0,	0,0,	187,187,	
0,0,	187,187,	187,187,	0,0,	
187,187,	0,0,	187,187,	0,0,	
187,328,	0,0,	187,187,	180,286,	
187,187,	187,187,	187,187,	187,187,	
187,187,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
187,187,	187,187,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
187,187,	187,187,	187,187,	187,187,	
187,187,	187,187,	187,187,	187,187,	
187,187,	0,0,	187,187,	187,187,	
187,187,	187,187,	187,187,	187,187,	
187,187,	187,187,	187,187,	187,187,	
0,0,	0,0,	187,187,	187,187,	
187,187,	315,402,	0,0,	0,0,	
187,187,	0,0,	0,0,	0,0,	
187,187,	187,187,	187,187,	187,187,	
187,187,	187,187,	187,187,	187,187,	
188,330,	0,0,	0,0,	0,0,	
0,0,	0,0,	315,403,	0,0,	
188,330,	188,330,	187,187,	315,451,	
397,509,	397,509,	397,509,	397,509,	
397,509,	397,509,	397,509,	397,509,	
187,187,	0,0,	0,0,	315,451,	
0,0,	0,0,	315,451,	315,451,	
0,0,	0,0,	0,0,	188,330,	
0,0,	188,330,	188,330,	0,0,	
188,330,	0,0,	188,330,	188,331,	
188,332,	0,0,	188,330,	315,451,	
188,330,	188,330,	188,330,	188,330,	
188,330,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	315,451,	
188,330,	188,330,	315,451,	315,451,	
0,0,	0,0,	0,0,	0,0,	
188,330,	188,330,	188,330,	188,330,	
188,330,	188,330,	188,330,	188,330,	
188,330,	0,0,	188,330,	188,330,	
188,330,	188,330,	188,330,	188,330,	
188,330,	188,330,	188,330,	188,330,	
0,0,	0,0,	188,330,	188,330,	
188,330,	399,510,	0,0,	0,0,	
188,330,	0,0,	0,0,	0,0,	
188,330,	188,330,	188,330,	188,330,	
188,330,	188,330,	188,330,	188,330,	
191,191,	0,0,	0,0,	0,0,	
0,0,	0,0,	399,511,	0,0,	
191,191,	191,0,	188,330,	399,512,	
406,515,	406,515,	406,515,	406,515,	
406,515,	406,515,	406,515,	406,515,	
188,330,	0,0,	0,0,	399,512,	
0,0,	0,0,	399,512,	399,512,	
0,0,	0,0,	0,0,	191,191,	
0,0,	191,191,	191,191,	0,0,	
191,191,	0,0,	191,191,	191,191,	
191,334,	0,0,	191,191,	399,512,	
191,191,	191,191,	191,191,	191,191,	
191,191,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	399,512,	
191,191,	191,191,	399,512,	399,512,	
0,0,	0,0,	0,0,	0,0,	
191,191,	191,191,	191,191,	191,191,	
191,191,	191,191,	191,191,	191,191,	
191,191,	0,0,	191,191,	191,191,	
191,191,	191,191,	191,191,	191,191,	
191,191,	191,191,	191,191,	191,191,	
0,0,	0,0,	191,191,	191,191,	
191,191,	400,402,	0,0,	0,0,	
191,191,	0,0,	0,0,	0,0,	
191,191,	191,191,	191,191,	191,191,	
191,191,	191,191,	191,191,	191,191,	
192,192,	0,0,	0,0,	0,0,	
410,514,	0,0,	400,403,	0,0,	
192,192,	192,0,	191,191,	400,513,	
410,483,	410,483,	410,483,	410,483,	
410,483,	410,483,	410,483,	410,483,	
191,191,	0,0,	0,0,	400,513,	
0,0,	0,0,	400,513,	400,513,	
0,0,	0,0,	0,0,	192,192,	
0,0,	192,192,	192,192,	0,0,	
192,192,	0,0,	192,192,	192,192,	
192,192,	0,0,	192,192,	400,513,	
192,192,	192,192,	192,192,	192,192,	
192,192,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	400,513,	
192,192,	192,192,	400,513,	400,513,	
0,0,	0,0,	0,0,	0,0,	
192,192,	192,192,	192,192,	192,192,	
192,192,	192,192,	192,192,	192,192,	
192,192,	0,0,	192,192,	192,192,	
192,192,	192,192,	192,192,	192,192,	
192,192,	192,192,	192,192,	192,192,	
0,0,	0,0,	192,192,	192,192,	
192,192,	407,510,	0,0,	0,0,	
192,192,	0,0,	0,0,	0,0,	
192,192,	192,192,	192,192,	192,192,	
192,192,	192,192,	192,192,	192,192,	
246,246,	0,0,	0,0,	0,0,	
0,0,	0,0,	407,511,	0,0,	
246,246,	246,246,	192,192,	407,512,	
429,431,	429,431,	429,431,	429,431,	
429,431,	429,431,	429,431,	429,431,	
192,335,	0,0,	0,0,	407,512,	
0,0,	0,0,	407,512,	407,512,	
0,0,	0,0,	0,0,	246,246,	
0,0,	246,246,	246,246,	0,0,	
246,246,	0,0,	246,246,	246,246,	
246,246,	0,0,	246,246,	407,512,	
246,393,	246,246,	246,246,	246,246,	
246,246,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	407,512,	
246,246,	246,246,	407,512,	407,512,	
0,0,	0,0,	0,0,	0,0,	
246,246,	246,246,	246,246,	246,246,	
246,246,	246,246,	246,246,	246,246,	
246,246,	0,0,	246,246,	246,246,	
246,246,	246,246,	246,246,	246,246,	
246,246,	246,246,	246,246,	246,246,	
0,0,	0,0,	246,246,	246,246,	
246,246,	0,0,	247,247,	247,247,	
0,0,	0,0,	0,0,	0,0,	
246,246,	246,246,	246,246,	246,246,	
246,246,	246,246,	246,246,	246,246,	
430,531,	430,531,	430,531,	430,531,	
430,531,	430,531,	430,531,	430,531,	
0,0,	247,247,	246,246,	247,248,	
431,532,	431,532,	431,532,	431,532,	
431,532,	431,532,	431,532,	431,532,	
246,246,	0,0,	247,111,	0,0,	
0,0,	247,111,	247,111,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	247,111,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	247,111,	247,111,	
247,111,	247,111,	247,111,	247,111,	
247,111,	247,111,	247,111,	0,0,	
247,111,	247,111,	247,111,	247,111,	
247,111,	247,111,	247,111,	247,111,	
247,111,	247,111,	248,248,	0,0,	
247,111,	247,111,	247,111,	0,0,	
0,0,	0,0,	248,248,	248,248,	
0,0,	0,0,	247,111,	247,111,	
247,111,	247,111,	247,111,	247,111,	
247,111,	247,111,	480,573,	480,573,	
480,573,	480,573,	480,573,	480,573,	
480,573,	480,573,	0,0,	0,0,	
247,111,	248,248,	0,0,	248,394,	
248,248,	0,0,	248,248,	0,0,	
248,248,	248,248,	248,248,	0,0,	
248,248,	0,0,	248,248,	248,248,	
248,248,	248,248,	248,248,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	248,248,	248,248,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	248,248,	248,248,	
248,248,	248,248,	248,248,	248,248,	
248,248,	248,248,	248,248,	0,0,	
248,248,	248,248,	248,248,	248,248,	
248,248,	248,248,	248,248,	248,248,	
248,248,	248,248,	0,0,	0,0,	
248,248,	248,248,	248,248,	545,544,	
0,0,	0,0,	248,395,	0,0,	
0,0,	257,259,	248,248,	248,248,	
248,248,	248,248,	248,248,	248,248,	
248,248,	248,248,	257,400,	257,400,	
257,400,	257,400,	257,400,	257,400,	
257,400,	257,400,	0,0,	0,0,	
248,248,	545,546,	257,260,	0,0,	
0,0,	0,0,	0,0,	257,401,	
0,0,	0,0,	248,248,	0,0,	
0,0,	545,546,	0,0,	262,259,	
545,546,	545,546,	0,0,	257,401,	
0,0,	0,0,	257,401,	257,401,	
262,359,	262,359,	262,359,	262,359,	
262,359,	262,359,	262,359,	262,359,	
0,0,	545,546,	0,0,	0,0,	
262,260,	0,0,	0,0,	257,401,	
0,0,	262,261,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	268,259,	0,0,	257,401,	
0,0,	262,261,	257,401,	257,401,	
262,261,	262,261,	268,409,	268,409,	
268,409,	268,409,	268,409,	268,409,	
268,409,	268,409,	0,0,	0,0,	
0,0,	0,0,	268,260,	0,0,	
0,0,	262,261,	322,447,	268,261,	
269,402,	0,0,	0,0,	0,0,	
0,0,	269,366,	0,0,	322,454,	
0,0,	262,261,	0,0,	268,261,	
262,261,	262,261,	268,261,	268,261,	
0,0,	0,0,	269,367,	322,449,	
0,0,	269,403,	0,0,	0,0,	
322,450,	0,0,	269,404,	0,0,	
0,0,	0,0,	0,0,	268,261,	
301,429,	0,0,	0,0,	0,0,	
322,450,	0,0,	269,404,	322,450,	
322,450,	269,404,	269,404,	268,261,	
0,0,	301,430,	268,261,	268,261,	
269,366,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
322,450,	269,368,	269,410,	269,368,	
269,368,	269,368,	269,368,	269,368,	
269,368,	0,0,	0,0,	0,0,	
0,0,	0,0,	269,404,	0,0,	
0,0,	269,404,	269,404,	301,429,	
0,0,	0,0,	0,0,	0,0,	
269,366,	0,0,	0,0,	317,259,	
301,431,	301,431,	301,431,	301,431,	
301,431,	301,431,	301,431,	301,431,	
317,424,	317,424,	317,424,	317,424,	
317,424,	317,424,	317,424,	317,424,	
0,0,	323,259,	0,0,	0,0,	
317,260,	0,0,	0,0,	301,429,	
0,0,	317,316,	323,455,	323,455,	
323,455,	323,455,	323,455,	323,455,	
323,455,	323,455,	0,0,	0,0,	
0,0,	317,316,	323,260,	0,0,	
317,316,	317,316,	409,402,	323,316,	
324,402,	0,0,	0,0,	0,0,	
0,0,	324,429,	0,0,	0,0,	
0,0,	0,0,	0,0,	323,316,	
0,0,	317,316,	323,316,	323,316,	
0,0,	0,0,	324,430,	409,403,	
0,0,	324,403,	0,0,	0,0,	
409,404,	317,316,	324,451,	0,0,	
317,316,	317,316,	0,0,	323,316,	
0,0,	0,0,	0,0,	0,0,	
409,404,	0,0,	324,451,	409,404,	
409,404,	324,451,	324,451,	323,316,	
0,0,	331,331,	323,316,	323,316,	
324,429,	0,0,	0,0,	0,0,	
0,0,	331,331,	331,331,	0,0,	
409,404,	324,431,	324,456,	324,431,	
324,431,	324,431,	324,431,	324,431,	
324,431,	0,0,	0,0,	0,0,	
409,404,	0,0,	324,451,	409,404,	
409,404,	324,451,	324,451,	0,0,	
331,331,	0,0,	331,331,	331,331,	
324,429,	331,331,	0,0,	331,331,	
331,0,	0,0,	0,0,	331,331,	
0,0,	331,331,	331,331,	331,331,	
331,331,	331,331,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	331,331,	331,331,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	331,331,	331,331,	331,331,	
331,331,	331,331,	331,331,	331,331,	
331,331,	331,331,	0,0,	331,331,	
331,331,	331,331,	331,331,	331,331,	
331,331,	331,331,	331,331,	331,331,	
331,331,	448,544,	0,0,	331,331,	
331,331,	331,331,	0,0,	0,0,	
360,367,	331,331,	0,0,	0,0,	
454,544,	331,331,	331,331,	331,331,	
331,331,	331,331,	331,331,	331,331,	
331,331,	360,367,	448,545,	0,0,	
0,0,	0,0,	0,0,	448,546,	
0,0,	0,0,	0,0,	331,331,	
0,0,	454,545,	0,0,	0,0,	
0,0,	0,0,	454,546,	448,546,	
393,246,	331,331,	448,546,	448,546,	
0,0,	0,0,	0,0,	0,0,	
393,246,	393,246,	454,546,	360,367,	
0,0,	454,546,	454,546,	0,0,	
0,0,	0,0,	0,0,	448,546,	
360,480,	360,480,	360,480,	360,480,	
360,480,	360,480,	360,480,	360,480,	
0,0,	0,0,	454,546,	393,246,	
0,0,	393,246,	393,246,	0,0,	
393,246,	0,0,	393,246,	393,246,	
393,246,	0,0,	393,246,	360,367,	
393,505,	393,246,	393,246,	393,246,	
393,246,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
393,246,	393,246,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
393,246,	393,246,	393,246,	393,246,	
393,246,	393,246,	393,246,	393,246,	
393,246,	0,0,	393,246,	393,246,	
393,246,	393,246,	393,246,	393,246,	
393,246,	393,246,	393,246,	393,246,	
0,0,	0,0,	393,246,	393,246,	
393,246,	394,394,	394,394,	0,0,	
0,0,	0,0,	510,605,	0,0,	
393,246,	393,246,	393,246,	393,246,	
393,246,	393,246,	393,246,	393,246,	
0,0,	0,0,	510,605,	456,514,	
0,0,	510,605,	510,605,	0,0,	
394,394,	0,0,	393,246,	456,532,	
456,532,	456,532,	456,532,	456,532,	
456,532,	456,532,	456,532,	0,0,	
393,246,	394,111,	510,605,	0,0,	
394,111,	394,111,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	394,111,	510,605,	0,0,	
0,0,	510,605,	510,605,	0,0,	
0,0,	394,111,	394,111,	394,111,	
394,111,	394,111,	394,111,	394,111,	
394,111,	394,111,	0,0,	394,111,	
394,111,	394,111,	394,111,	394,111,	
394,111,	394,111,	394,111,	394,111,	
394,111,	0,0,	395,507,	394,111,	
394,111,	394,111,	0,0,	0,0,	
0,0,	394,506,	395,507,	395,507,	
0,0,	394,111,	394,111,	394,111,	
394,111,	394,111,	394,111,	394,111,	
394,111,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	394,111,	
0,0,	395,507,	0,0,	395,508,	
395,507,	0,0,	395,507,	0,0,	
395,507,	395,507,	395,507,	0,0,	
395,507,	0,0,	395,507,	395,507,	
395,507,	395,507,	395,507,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	395,507,	395,507,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	395,507,	395,507,	
395,507,	395,507,	395,507,	395,507,	
395,507,	395,507,	395,507,	0,0,	
395,507,	395,507,	395,507,	395,507,	
395,507,	395,507,	395,507,	395,507,	
395,507,	395,507,	0,0,	0,0,	
395,507,	395,507,	395,507,	446,510,	
0,0,	0,0,	395,507,	0,0,	
0,0,	0,0,	395,507,	395,507,	
395,507,	395,507,	395,507,	395,507,	
395,507,	395,507,	418,522,	0,0,	
0,0,	0,0,	0,0,	0,0,	
446,511,	0,0,	418,418,	0,0,	
395,507,	446,543,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	395,507,	0,0,	
0,0,	446,543,	0,0,	0,0,	
446,543,	446,543,	0,0,	0,0,	
0,0,	418,418,	0,0,	418,522,	
418,522,	0,0,	418,522,	0,0,	
418,522,	418,522,	418,522,	0,0,	
418,522,	446,543,	418,522,	418,522,	
418,522,	418,522,	418,522,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	446,543,	418,522,	418,522,	
446,543,	446,543,	0,0,	0,0,	
0,0,	0,0,	418,522,	418,522,	
418,522,	418,522,	418,522,	418,522,	
418,522,	418,522,	418,522,	0,0,	
418,522,	418,522,	418,522,	418,522,	
418,522,	418,522,	418,522,	418,522,	
418,522,	418,522,	453,510,	0,0,	
418,522,	418,522,	418,522,	0,0,	
455,402,	0,0,	418,522,	0,0,	
0,0,	625,624,	418,522,	418,522,	
418,522,	418,522,	418,522,	418,522,	
418,522,	418,522,	0,0,	453,511,	
0,0,	0,0,	0,0,	0,0,	
453,543,	455,403,	0,0,	0,0,	
418,522,	0,0,	455,451,	0,0,	
0,0,	0,0,	0,0,	625,626,	
453,543,	0,0,	418,522,	453,543,	
453,543,	0,0,	455,451,	0,0,	
0,0,	455,451,	455,451,	625,626,	
0,0,	460,460,	625,626,	625,626,	
0,0,	0,0,	575,643,	0,0,	
453,543,	460,460,	460,0,	0,0,	
0,0,	0,0,	455,451,	0,0,	
0,0,	0,0,	575,643,	625,626,	
453,543,	575,643,	575,643,	453,543,	
453,543,	0,0,	455,451,	0,0,	
0,0,	455,451,	455,451,	0,0,	
460,460,	0,0,	460,460,	460,460,	
0,0,	460,460,	575,643,	460,460,	
460,460,	460,551,	0,0,	460,460,	
0,0,	460,460,	460,460,	460,460,	
460,460,	460,460,	575,643,	0,0,	
0,0,	575,643,	575,643,	0,0,	
0,0,	460,460,	460,460,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	460,460,	460,460,	460,460,	
460,460,	460,460,	460,460,	460,460,	
460,460,	460,460,	0,0,	460,460,	
460,460,	460,460,	460,460,	460,460,	
460,460,	460,460,	460,460,	460,460,	
460,460,	0,0,	0,0,	460,460,	
460,460,	460,460,	483,575,	0,0,	
0,0,	460,460,	0,0,	0,0,	
0,0,	460,460,	460,460,	460,460,	
460,460,	460,460,	460,460,	460,460,	
460,460,	461,461,	0,0,	0,0,	
0,0,	0,0,	0,0,	483,576,	
0,0,	461,461,	461,0,	460,460,	
483,577,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	460,460,	0,0,	0,0,	
483,577,	0,0,	0,0,	483,577,	
483,577,	0,0,	0,0,	0,0,	
461,461,	0,0,	461,461,	461,461,	
0,0,	461,461,	0,0,	461,461,	
461,461,	461,461,	0,0,	461,461,	
483,577,	461,461,	461,461,	461,461,	
461,461,	461,461,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
483,577,	461,461,	461,461,	483,577,	
483,577,	0,0,	0,0,	0,0,	
0,0,	461,461,	461,461,	461,461,	
461,461,	461,461,	461,461,	461,461,	
461,461,	461,461,	0,0,	461,461,	
461,461,	461,461,	461,461,	461,461,	
461,461,	461,461,	461,461,	461,461,	
461,461,	0,0,	0,0,	461,461,	
461,461,	461,461,	509,510,	0,0,	
0,0,	461,461,	0,0,	0,0,	
0,0,	461,461,	461,461,	461,461,	
461,461,	461,461,	461,461,	461,461,	
461,461,	462,462,	0,0,	0,0,	
0,0,	0,0,	0,0,	509,511,	
0,0,	462,462,	462,0,	461,461,	
509,605,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	461,552,	0,0,	0,0,	
509,605,	0,0,	0,0,	509,605,	
509,605,	0,0,	0,0,	0,0,	
462,462,	0,0,	462,462,	462,462,	
0,0,	462,462,	0,0,	462,462,	
462,462,	462,553,	0,0,	462,462,	
509,605,	462,462,	462,462,	462,462,	
462,462,	462,462,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
509,605,	462,462,	462,462,	509,605,	
509,605,	0,0,	0,0,	0,0,	
0,0,	462,462,	462,462,	462,462,	
462,462,	462,462,	462,462,	462,462,	
462,462,	462,462,	0,0,	462,462,	
462,462,	462,462,	462,462,	462,462,	
462,462,	462,462,	462,462,	462,462,	
462,462,	0,0,	0,0,	462,462,	
462,462,	462,462,	532,575,	0,0,	
0,0,	462,462,	0,0,	0,0,	
0,0,	462,462,	462,462,	462,462,	
462,462,	462,462,	462,462,	462,462,	
462,462,	463,463,	0,0,	0,0,	
0,0,	0,0,	0,0,	532,576,	
0,0,	463,463,	463,0,	462,462,	
532,617,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	462,462,	0,0,	0,0,	
532,617,	0,0,	0,0,	532,617,	
532,617,	0,0,	0,0,	0,0,	
463,463,	0,0,	463,463,	463,463,	
0,0,	463,463,	0,0,	463,463,	
463,463,	463,463,	0,0,	463,463,	
532,617,	463,463,	463,463,	463,463,	
463,463,	463,463,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
532,617,	463,463,	463,463,	532,617,	
532,617,	0,0,	0,0,	0,0,	
0,0,	463,463,	463,463,	463,463,	
463,463,	463,463,	463,463,	463,463,	
463,463,	463,463,	0,0,	463,463,	
463,463,	463,463,	463,463,	463,463,	
463,463,	463,463,	463,463,	463,463,	
463,463,	0,0,	0,0,	463,463,	
463,463,	463,463,	542,624,	0,0,	
0,0,	463,463,	0,0,	0,0,	
0,0,	463,463,	463,463,	463,463,	
463,463,	463,463,	463,463,	463,463,	
463,463,	464,464,	0,0,	0,0,	
0,0,	0,0,	0,0,	542,625,	
0,0,	464,464,	464,0,	463,463,	
542,626,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	463,554,	0,0,	0,0,	
542,626,	0,0,	0,0,	542,626,	
542,626,	0,0,	0,0,	0,0,	
464,464,	0,0,	464,464,	464,464,	
0,0,	464,464,	0,0,	464,464,	
464,464,	464,555,	0,0,	464,464,	
542,626,	464,464,	464,464,	464,464,	
464,464,	464,464,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	464,464,	464,464,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	464,464,	464,464,	464,464,	
464,464,	464,464,	464,464,	464,464,	
464,464,	464,464,	0,0,	464,464,	
464,464,	464,464,	464,464,	464,464,	
464,464,	464,464,	464,464,	464,464,	
464,464,	0,0,	0,0,	464,464,	
464,464,	464,464,	548,624,	0,0,	
0,0,	464,464,	0,0,	0,0,	
0,0,	464,464,	464,464,	464,464,	
464,464,	464,464,	464,464,	464,464,	
464,464,	465,465,	0,0,	0,0,	
0,0,	0,0,	0,0,	548,625,	
0,0,	465,465,	465,0,	464,464,	
548,626,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	464,464,	0,0,	0,0,	
548,626,	0,0,	0,0,	548,626,	
548,626,	0,0,	0,0,	0,0,	
465,465,	0,0,	465,465,	465,465,	
0,0,	465,465,	0,0,	465,465,	
465,465,	465,465,	0,0,	465,465,	
548,626,	465,465,	465,465,	465,465,	
465,465,	465,465,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	465,465,	465,465,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	465,465,	465,465,	465,465,	
465,465,	465,465,	465,465,	465,465,	
465,465,	465,465,	0,0,	465,465,	
465,465,	465,465,	465,465,	465,465,	
465,465,	465,465,	465,465,	465,465,	
465,465,	0,0,	0,0,	465,465,	
465,465,	465,465,	573,575,	0,0,	
0,0,	465,465,	0,0,	0,0,	
0,0,	465,465,	465,465,	465,465,	
465,465,	465,465,	465,465,	465,465,	
465,465,	505,246,	0,0,	0,0,	
0,0,	0,0,	0,0,	573,576,	
0,0,	505,246,	505,246,	465,465,	
573,643,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	465,556,	0,0,	0,0,	
573,643,	0,0,	0,0,	573,643,	
573,643,	0,0,	0,0,	0,0,	
505,246,	0,0,	505,246,	505,246,	
0,0,	505,246,	0,0,	505,246,	
505,246,	505,246,	0,0,	505,246,	
573,643,	505,505,	505,246,	505,246,	
505,246,	505,246,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
573,643,	505,246,	505,246,	573,643,	
573,643,	0,0,	0,0,	0,0,	
0,0,	505,246,	505,246,	505,246,	
505,246,	505,246,	505,246,	505,246,	
505,246,	505,246,	0,0,	505,246,	
505,246,	505,246,	505,246,	505,246,	
505,246,	505,246,	505,246,	505,246,	
505,246,	0,0,	0,0,	505,246,	
505,246,	505,246,	616,674,	0,0,	
0,0,	505,603,	0,0,	0,0,	
0,0,	505,246,	505,246,	505,246,	
505,246,	505,246,	505,246,	505,246,	
505,246,	522,522,	0,0,	0,0,	
0,0,	0,0,	0,0,	616,675,	
0,0,	522,522,	0,0,	505,246,	
616,676,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	505,246,	0,0,	0,0,	
616,676,	0,0,	0,0,	616,676,	
616,676,	0,0,	0,0,	0,0,	
522,522,	0,0,	522,522,	522,522,	
0,0,	522,522,	0,0,	522,522,	
522,522,	522,522,	0,0,	522,522,	
616,676,	522,522,	522,522,	522,522,	
522,522,	522,522,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	522,522,	522,522,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	522,522,	522,522,	522,522,	
522,522,	522,522,	522,522,	522,522,	
522,522,	522,522,	0,0,	522,522,	
522,522,	522,522,	522,522,	522,522,	
522,522,	522,522,	522,522,	522,522,	
522,522,	0,0,	0,0,	522,522,	
522,522,	522,522,	675,674,	0,0,	
0,0,	522,522,	0,0,	0,0,	
0,0,	522,522,	522,522,	522,522,	
522,522,	522,522,	522,522,	522,522,	
522,522,	523,523,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	523,523,	0,0,	522,522,	
675,676,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	522,522,	0,0,	0,0,	
675,676,	0,0,	0,0,	675,676,	
675,676,	0,0,	0,0,	0,0,	
523,523,	0,0,	523,523,	523,523,	
0,0,	523,523,	0,0,	523,523,	
523,523,	523,610,	0,0,	523,523,	
675,676,	523,523,	523,523,	523,523,	
523,523,	523,523,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	523,523,	523,523,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	523,523,	523,523,	523,523,	
523,523,	523,523,	523,523,	523,523,	
523,523,	523,523,	0,0,	523,523,	
523,523,	523,523,	523,523,	523,523,	
523,523,	523,523,	523,523,	523,523,	
523,523,	0,0,	0,0,	523,523,	
523,523,	523,523,	0,0,	0,0,	
0,0,	523,523,	0,0,	0,0,	
0,0,	523,523,	523,523,	523,523,	
523,523,	523,523,	523,523,	523,523,	
523,523,	524,524,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	524,524,	0,0,	523,523,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	523,523,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
524,524,	0,0,	524,524,	524,524,	
0,0,	524,524,	0,0,	524,524,	
524,524,	524,524,	0,0,	524,524,	
0,0,	524,524,	524,524,	524,524,	
524,524,	524,524,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	524,524,	524,524,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	524,524,	524,524,	524,524,	
524,524,	524,524,	524,524,	524,524,	
524,524,	524,524,	0,0,	524,524,	
524,524,	524,524,	524,524,	524,524,	
524,524,	524,524,	524,524,	524,524,	
524,524,	0,0,	0,0,	524,524,	
524,524,	524,524,	0,0,	0,0,	
0,0,	524,524,	0,0,	0,0,	
0,0,	524,524,	524,524,	524,524,	
524,524,	524,524,	524,524,	524,524,	
524,524,	525,525,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	525,525,	0,0,	524,524,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	524,611,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
525,525,	0,0,	525,525,	525,525,	
0,0,	525,525,	0,0,	525,525,	
525,525,	525,612,	0,0,	525,525,	
0,0,	525,525,	525,525,	525,525,	
525,525,	525,525,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	525,525,	525,525,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	525,525,	525,525,	525,525,	
525,525,	525,525,	525,525,	525,525,	
525,525,	525,525,	0,0,	525,525,	
525,525,	525,525,	525,525,	525,525,	
525,525,	525,525,	525,525,	525,525,	
525,525,	0,0,	0,0,	525,525,	
525,525,	525,525,	0,0,	0,0,	
0,0,	525,525,	0,0,	0,0,	
0,0,	525,525,	525,525,	525,525,	
525,525,	525,525,	525,525,	525,525,	
525,525,	526,526,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	526,526,	0,0,	525,525,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	525,525,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
526,526,	0,0,	526,526,	526,526,	
0,0,	526,526,	0,0,	526,526,	
526,526,	526,526,	0,0,	526,526,	
0,0,	526,526,	526,526,	526,526,	
526,526,	526,526,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	526,526,	526,526,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	526,526,	526,526,	526,526,	
526,526,	526,526,	526,526,	526,526,	
526,526,	526,526,	0,0,	526,526,	
526,526,	526,526,	526,526,	526,526,	
526,526,	526,526,	526,526,	526,526,	
526,526,	0,0,	0,0,	526,526,	
526,526,	526,526,	0,0,	0,0,	
0,0,	526,526,	0,0,	0,0,	
0,0,	526,526,	526,526,	526,526,	
526,526,	526,526,	526,526,	526,526,	
526,526,	579,579,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	579,579,	579,0,	526,526,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	526,613,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
579,579,	0,0,	579,579,	579,579,	
0,0,	579,579,	0,0,	579,579,	
579,579,	579,646,	0,0,	579,579,	
0,0,	579,579,	579,579,	579,579,	
579,579,	579,579,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	579,579,	579,579,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	579,579,	579,579,	579,579,	
579,579,	579,579,	579,579,	579,579,	
579,579,	579,579,	0,0,	579,579,	
579,579,	579,579,	579,579,	579,579,	
579,579,	579,579,	579,579,	579,579,	
579,579,	0,0,	0,0,	579,579,	
579,579,	579,579,	0,0,	0,0,	
0,0,	579,579,	0,0,	0,0,	
0,0,	579,579,	579,579,	579,579,	
579,579,	579,579,	579,579,	579,579,	
579,579,	602,238,	0,0,	0,0,	
602,644,	0,0,	602,239,	0,0,	
0,0,	0,0,	0,0,	579,579,	
602,240,	602,241,	0,0,	602,212,	
0,0,	0,0,	602,242,	0,0,	
610,523,	579,579,	0,0,	602,243,	
0,0,	0,0,	0,0,	0,0,	
610,523,	0,0,	0,0,	0,0,	
0,0,	602,238,	0,0,	0,0,	
602,644,	0,0,	602,239,	0,0,	
0,0,	0,0,	0,0,	0,0,	
602,240,	602,241,	0,0,	602,212,	
0,0,	0,0,	602,242,	610,523,	
0,0,	610,523,	610,523,	602,243,	
610,523,	0,0,	610,523,	610,523,	
610,610,	0,0,	610,523,	0,0,	
610,523,	610,523,	610,523,	610,523,	
610,523,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
610,523,	610,523,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
610,523,	610,523,	610,523,	610,523,	
610,523,	610,523,	610,523,	610,523,	
610,523,	0,0,	610,523,	610,523,	
610,523,	610,523,	610,523,	610,523,	
610,523,	610,523,	610,523,	610,523,	
0,0,	0,0,	610,523,	610,523,	
610,523,	0,0,	0,0,	0,0,	
610,523,	0,0,	0,0,	0,0,	
610,523,	610,523,	610,523,	610,523,	
610,523,	610,523,	610,523,	610,523,	
611,524,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
611,524,	0,0,	610,523,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
610,523,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	611,524,	
0,0,	611,524,	611,524,	0,0,	
611,524,	0,0,	611,524,	611,524,	
611,524,	0,0,	611,524,	0,0,	
611,524,	611,524,	611,524,	611,524,	
611,524,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
611,524,	611,524,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
611,524,	611,524,	611,524,	611,524,	
611,524,	611,524,	611,524,	611,524,	
611,524,	0,0,	611,524,	611,524,	
611,524,	611,524,	611,524,	611,524,	
611,524,	611,524,	611,524,	611,524,	
0,0,	0,0,	611,524,	611,524,	
611,524,	0,0,	0,0,	0,0,	
611,524,	0,0,	0,0,	0,0,	
611,524,	611,524,	611,524,	611,524,	
611,524,	611,524,	611,524,	611,524,	
612,525,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
612,525,	0,0,	611,524,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
611,611,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	612,525,	
0,0,	612,525,	612,525,	0,0,	
612,525,	0,0,	612,525,	612,525,	
612,612,	0,0,	612,525,	0,0,	
612,525,	612,525,	612,525,	612,525,	
612,525,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
612,525,	612,525,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
612,525,	612,525,	612,525,	612,525,	
612,525,	612,525,	612,525,	612,525,	
612,525,	0,0,	612,525,	612,525,	
612,525,	612,525,	612,525,	612,525,	
612,525,	612,525,	612,525,	612,525,	
0,0,	0,0,	612,525,	612,525,	
612,525,	0,0,	0,0,	0,0,	
612,525,	0,0,	0,0,	0,0,	
612,525,	612,525,	612,525,	612,525,	
612,525,	612,525,	612,525,	612,525,	
613,526,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
613,526,	0,0,	612,525,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
612,525,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	613,526,	
0,0,	613,526,	613,526,	0,0,	
613,526,	0,0,	613,526,	613,526,	
613,526,	0,0,	613,526,	0,0,	
613,526,	613,526,	613,526,	613,526,	
613,526,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
613,526,	613,526,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
613,526,	613,526,	613,526,	613,526,	
613,526,	613,526,	613,526,	613,526,	
613,526,	0,0,	613,526,	613,526,	
613,526,	613,526,	613,526,	613,526,	
613,526,	613,526,	613,526,	613,526,	
0,0,	0,0,	613,526,	613,526,	
613,526,	0,0,	0,0,	0,0,	
613,526,	0,0,	0,0,	0,0,	
613,526,	613,526,	613,526,	613,526,	
613,526,	613,526,	613,526,	613,526,	
641,641,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
641,641,	641,0,	613,526,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
613,613,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	641,641,	
0,0,	641,641,	641,641,	0,0,	
641,641,	0,0,	641,641,	641,641,	
641,694,	0,0,	641,641,	0,0,	
641,641,	641,641,	641,641,	641,641,	
641,641,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
641,641,	641,641,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
641,641,	641,641,	641,641,	641,641,	
641,641,	641,641,	641,641,	641,641,	
641,641,	0,0,	641,641,	641,641,	
641,641,	641,641,	641,641,	641,641,	
641,641,	641,641,	641,641,	641,641,	
0,0,	0,0,	641,641,	641,641,	
641,641,	0,0,	0,0,	0,0,	
641,641,	0,0,	0,0,	0,0,	
641,641,	641,641,	641,641,	641,641,	
641,641,	641,641,	641,641,	641,641,	
668,668,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
668,668,	668,0,	641,641,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
641,641,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	668,668,	
0,0,	668,668,	668,668,	0,0,	
668,668,	0,0,	668,668,	668,668,	
668,714,	0,0,	668,668,	0,0,	
668,668,	668,668,	668,668,	668,668,	
668,668,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
668,668,	668,668,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
668,668,	668,668,	668,668,	668,668,	
668,668,	668,668,	668,668,	668,668,	
668,668,	0,0,	668,668,	668,668,	
668,668,	668,668,	668,668,	668,668,	
668,668,	668,668,	668,668,	668,668,	
0,0,	0,0,	668,668,	668,668,	
668,668,	0,0,	0,0,	0,0,	
668,668,	0,0,	0,0,	0,0,	
668,668,	668,668,	668,668,	668,668,	
668,668,	668,668,	668,668,	668,668,	
705,737,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
705,737,	705,0,	668,668,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
668,668,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	705,737,	
0,0,	705,737,	705,737,	0,0,	
705,737,	0,0,	705,705,	705,737,	
705,737,	0,0,	705,737,	0,0,	
705,705,	705,737,	705,737,	705,705,	
705,705,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
705,705,	705,737,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
705,705,	705,705,	705,705,	705,705,	
705,705,	705,705,	705,738,	705,705,	
705,705,	721,0,	705,705,	705,705,	
705,705,	705,705,	705,705,	705,705,	
705,705,	705,705,	705,705,	705,705,	
0,0,	0,0,	705,705,	705,705,	
705,705,	0,0,	0,0,	0,0,	
705,737,	0,0,	0,0,	0,0,	
705,705,	705,705,	705,705,	705,705,	
705,705,	705,705,	705,738,	705,705,	
0,0,	0,0,	0,0,	0,0,	
721,721,	0,0,	0,0,	721,721,	
721,721,	0,0,	705,705,	0,0,	
0,0,	0,0,	0,0,	0,0,	
721,721,	721,749,	0,0,	0,0,	
705,737,	0,0,	0,0,	0,0,	
721,721,	721,721,	721,721,	721,721,	
721,721,	721,721,	721,750,	721,721,	
721,721,	0,0,	721,721,	721,721,	
721,721,	721,721,	721,721,	721,721,	
721,721,	721,721,	721,721,	721,721,	
737,0,	0,0,	721,721,	721,721,	
721,721,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
721,721,	721,721,	721,721,	721,721,	
721,721,	721,721,	721,750,	721,721,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	737,737,	721,721,	0,0,	
0,0,	0,0,	0,0,	737,737,	
0,0,	0,0,	737,737,	737,737,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	737,737,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	737,737,	
737,737,	737,737,	737,737,	737,737,	
737,737,	737,762,	737,737,	737,737,	
0,0,	737,737,	737,737,	737,737,	
737,737,	737,737,	737,737,	737,737,	
737,737,	737,737,	737,737,	749,749,	
0,0,	737,737,	737,737,	737,737,	
0,0,	0,0,	0,0,	749,749,	
0,0,	0,0,	0,0,	737,737,	
737,737,	737,737,	737,737,	737,737,	
737,737,	737,762,	737,737,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	737,737,	749,749,	0,0,	
749,749,	749,749,	0,0,	749,749,	
0,0,	749,749,	749,749,	749,749,	
0,0,	749,749,	0,0,	749,749,	
749,749,	749,749,	749,749,	749,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	749,749,	
749,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	749,749,	
749,749,	749,749,	749,749,	749,749,	
749,749,	749,772,	749,749,	749,749,	
750,0,	749,749,	749,749,	749,749,	
749,749,	749,749,	749,749,	749,749,	
749,749,	749,749,	749,749,	0,0,	
0,0,	749,749,	749,749,	749,749,	
0,0,	0,0,	0,0,	749,749,	
0,0,	0,0,	0,0,	749,749,	
749,749,	749,749,	749,749,	749,749,	
749,749,	749,772,	749,749,	0,0,	
0,0,	0,0,	0,0,	750,721,	
0,0,	0,0,	750,721,	750,721,	
0,0,	749,749,	0,0,	0,0,	
0,0,	0,0,	0,0,	750,721,	
750,749,	0,0,	0,0,	749,749,	
0,0,	0,0,	0,0,	750,773,	
750,721,	750,721,	750,721,	750,721,	
750,721,	750,750,	750,721,	750,721,	
0,0,	750,721,	750,721,	750,721,	
750,721,	750,721,	750,721,	750,721,	
750,721,	750,721,	750,721,	762,0,	
0,0,	750,721,	750,721,	750,721,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	750,774,	
750,721,	750,721,	750,721,	750,721,	
750,721,	750,750,	750,721,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
762,737,	750,721,	0,0,	0,0,	
0,0,	0,0,	762,737,	0,0,	
0,0,	762,737,	762,737,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	762,737,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	762,790,	762,737,	
762,737,	762,737,	762,737,	762,737,	
762,762,	762,737,	762,737,	0,0,	
762,737,	762,737,	762,737,	762,737,	
762,737,	762,737,	762,737,	762,737,	
762,737,	762,737,	772,749,	0,0,	
762,737,	762,737,	762,737,	0,0,	
0,0,	0,0,	772,749,	0,0,	
0,0,	0,0,	762,791,	762,737,	
762,737,	762,737,	762,737,	762,737,	
762,762,	762,737,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
762,737,	772,749,	0,0,	772,749,	
772,749,	0,0,	772,749,	0,0,	
772,749,	772,749,	772,749,	0,0,	
772,749,	0,0,	772,749,	772,749,	
772,749,	772,749,	772,749,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	772,749,	772,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	772,803,	772,749,	
772,749,	772,749,	772,749,	772,749,	
772,772,	772,749,	772,749,	773,0,	
772,749,	772,749,	772,749,	772,749,	
772,749,	772,749,	772,749,	772,749,	
772,749,	772,749,	0,0,	0,0,	
772,749,	772,749,	772,749,	0,0,	
0,0,	0,0,	772,749,	0,0,	
0,0,	0,0,	772,804,	772,749,	
772,749,	772,749,	772,749,	772,749,	
772,772,	772,749,	0,0,	0,0,	
0,0,	0,0,	773,721,	0,0,	
0,0,	773,721,	773,721,	0,0,	
772,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	773,721,	773,749,	
0,0,	0,0,	772,749,	0,0,	
0,0,	0,0,	773,721,	773,721,	
773,721,	773,721,	773,721,	773,721,	
773,750,	773,721,	773,721,	0,0,	
773,721,	773,721,	773,805,	773,721,	
773,721,	773,721,	773,721,	773,721,	
773,721,	773,721,	774,0,	0,0,	
773,721,	773,721,	773,721,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	773,721,	773,721,	
773,721,	773,721,	773,721,	773,721,	
773,750,	773,721,	0,0,	0,0,	
0,0,	0,0,	773,721,	0,0,	
0,0,	0,0,	0,0,	0,0,	
773,721,	0,0,	0,0,	0,0,	
0,0,	774,721,	0,0,	0,0,	
774,721,	774,721,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	774,721,	774,749,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	774,721,	774,721,	774,721,	
774,721,	774,721,	774,721,	774,750,	
774,721,	774,721,	0,0,	774,721,	
774,721,	774,721,	774,721,	774,721,	
774,721,	774,721,	774,721,	774,721,	
774,721,	790,0,	0,0,	774,721,	
774,721,	774,721,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	774,721,	774,721,	774,721,	
774,721,	774,721,	774,721,	774,750,	
774,721,	0,0,	0,0,	0,0,	
0,0,	774,806,	0,0,	0,0,	
0,0,	0,0,	790,737,	774,721,	
0,0,	0,0,	0,0,	0,0,	
790,737,	0,0,	0,0,	790,737,	
790,737,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
790,737,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
790,737,	790,737,	790,737,	790,737,	
790,737,	790,737,	790,762,	790,737,	
790,737,	0,0,	790,737,	790,737,	
790,823,	790,737,	790,737,	790,737,	
790,737,	790,737,	790,737,	790,737,	
791,0,	0,0,	790,737,	790,737,	
790,737,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
790,737,	790,737,	790,737,	790,737,	
790,737,	790,737,	790,762,	790,737,	
0,0,	0,0,	0,0,	0,0,	
790,737,	0,0,	0,0,	0,0,	
0,0,	791,737,	790,737,	0,0,	
0,0,	0,0,	0,0,	791,737,	
0,0,	0,0,	791,737,	791,737,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	791,737,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	791,737,	
791,737,	791,737,	791,737,	791,737,	
791,737,	791,762,	791,737,	791,737,	
0,0,	791,737,	791,737,	791,737,	
791,737,	791,737,	791,737,	791,737,	
791,737,	791,737,	791,737,	803,749,	
0,0,	791,737,	791,737,	791,737,	
0,0,	0,0,	0,0,	803,749,	
803,276,	0,0,	0,0,	791,737,	
791,737,	791,737,	791,737,	791,737,	
791,737,	791,762,	791,737,	0,0,	
0,0,	0,0,	0,0,	791,824,	
0,0,	0,0,	0,0,	0,0,	
0,0,	791,737,	803,749,	0,0,	
803,749,	803,749,	0,0,	803,749,	
0,0,	803,749,	803,749,	803,749,	
0,0,	803,749,	0,0,	803,749,	
803,749,	803,749,	803,749,	803,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	803,749,	
803,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	803,749,	
803,749,	803,749,	803,749,	803,749,	
803,749,	803,772,	803,749,	803,749,	
0,0,	803,749,	803,749,	803,837,	
803,749,	803,749,	803,749,	803,749,	
803,749,	803,749,	803,749,	805,0,	
0,0,	803,749,	803,749,	803,749,	
0,0,	0,0,	0,0,	803,749,	
0,0,	0,0,	0,0,	803,749,	
803,749,	803,749,	803,749,	803,749,	
803,749,	803,772,	803,749,	0,0,	
0,0,	0,0,	0,0,	803,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	803,749,	0,0,	0,0,	
0,0,	0,0,	805,721,	0,0,	
0,0,	805,721,	805,721,	803,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	805,721,	805,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	805,721,	805,721,	
805,721,	805,721,	805,839,	805,721,	
805,750,	805,721,	805,721,	806,0,	
805,721,	805,721,	805,721,	805,721,	
805,721,	805,721,	805,721,	805,721,	
805,721,	805,721,	0,0,	0,0,	
805,721,	805,721,	805,721,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	805,721,	805,721,	
805,721,	805,721,	805,721,	805,721,	
805,750,	805,721,	0,0,	0,0,	
0,0,	0,0,	806,721,	0,0,	
0,0,	806,721,	806,721,	0,0,	
805,721,	0,0,	0,0,	0,0,	
0,0,	0,0,	806,721,	806,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	806,721,	806,721,	
806,721,	806,721,	806,721,	806,721,	
806,750,	806,721,	806,721,	0,0,	
806,721,	806,721,	806,721,	806,721,	
806,721,	806,721,	806,721,	806,721,	
806,721,	806,721,	823,0,	0,0,	
806,721,	806,721,	806,721,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	806,721,	806,721,	
806,721,	806,721,	806,839,	806,721,	
806,750,	806,721,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	823,737,	
806,721,	0,0,	0,0,	0,0,	
0,0,	823,737,	0,0,	0,0,	
823,737,	823,737,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	823,737,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	823,737,	823,737,	823,737,	
823,737,	823,852,	823,737,	823,762,	
823,737,	823,737,	0,0,	823,737,	
823,737,	823,737,	823,737,	823,737,	
823,737,	823,737,	823,737,	823,737,	
823,737,	824,0,	0,0,	823,737,	
823,737,	823,737,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	823,737,	823,737,	823,737,	
823,737,	823,737,	823,737,	823,762,	
823,737,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	824,737,	823,737,	
0,0,	0,0,	0,0,	0,0,	
824,737,	0,0,	0,0,	824,737,	
824,737,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
824,737,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
824,737,	824,737,	824,737,	824,737,	
824,737,	824,737,	824,762,	824,737,	
824,737,	0,0,	824,737,	824,737,	
824,737,	824,737,	824,737,	824,737,	
824,737,	824,737,	824,737,	824,737,	
833,833,	0,0,	824,737,	824,737,	
824,737,	0,0,	0,0,	0,0,	
833,833,	833,857,	0,0,	0,0,	
824,737,	824,737,	824,737,	824,737,	
824,852,	824,737,	824,762,	824,737,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	824,737,	833,833,	
0,0,	833,833,	833,833,	0,0,	
833,833,	0,0,	833,833,	833,833,	
833,833,	0,0,	833,833,	0,0,	
833,833,	833,833,	833,833,	833,833,	
833,833,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
833,833,	833,833,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
833,833,	833,833,	833,833,	833,833,	
833,833,	833,833,	833,833,	833,833,	
833,833,	0,0,	833,833,	833,833,	
833,833,	833,833,	833,833,	833,833,	
833,833,	833,833,	833,833,	833,833,	
0,0,	0,0,	833,833,	833,833,	
833,833,	0,0,	0,0,	0,0,	
833,833,	0,0,	0,0,	0,0,	
833,833,	833,833,	833,833,	833,833,	
833,833,	833,833,	833,833,	833,833,	
835,835,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
835,835,	835,859,	833,833,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
833,833,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	835,835,	
0,0,	835,835,	835,835,	0,0,	
835,835,	0,0,	835,835,	835,835,	
835,835,	0,0,	835,835,	0,0,	
835,835,	835,835,	835,835,	835,835,	
835,835,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
835,835,	835,835,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
835,835,	835,835,	835,835,	835,835,	
835,835,	835,835,	835,835,	835,835,	
835,835,	0,0,	835,835,	835,835,	
835,835,	835,835,	835,835,	835,835,	
835,835,	835,835,	835,835,	835,835,	
0,0,	0,0,	835,835,	835,835,	
835,835,	0,0,	0,0,	0,0,	
835,835,	0,0,	0,0,	0,0,	
835,835,	835,835,	835,835,	835,835,	
835,835,	835,835,	835,835,	835,835,	
837,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
837,749,	0,0,	835,835,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
835,835,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	837,749,	
0,0,	837,749,	837,749,	0,0,	
837,749,	0,0,	837,749,	837,749,	
837,749,	0,0,	837,749,	0,0,	
837,749,	837,749,	837,749,	837,749,	
837,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
837,749,	837,749,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
837,749,	837,749,	837,749,	837,749,	
837,861,	837,749,	837,772,	837,749,	
837,749,	0,0,	837,749,	837,749,	
837,749,	837,749,	837,749,	837,749,	
837,749,	837,749,	837,749,	837,749,	
0,0,	0,0,	837,749,	837,749,	
837,749,	0,0,	0,0,	0,0,	
837,749,	0,0,	0,0,	0,0,	
837,749,	837,749,	837,749,	837,749,	
837,749,	837,749,	837,772,	837,749,	
838,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
838,749,	0,0,	837,749,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
837,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	838,749,	
0,0,	838,749,	838,749,	0,0,	
838,749,	0,0,	838,749,	838,749,	
838,749,	0,0,	838,749,	0,0,	
838,749,	838,749,	838,749,	838,749,	
838,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
838,749,	838,749,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
838,749,	838,749,	838,749,	838,749,	
838,749,	838,749,	838,772,	838,749,	
838,749,	839,0,	838,749,	838,749,	
838,749,	838,749,	838,749,	838,749,	
838,749,	838,749,	838,749,	838,749,	
0,0,	0,0,	838,749,	838,749,	
838,749,	0,0,	0,0,	0,0,	
838,749,	0,0,	0,0,	0,0,	
838,749,	838,749,	838,749,	838,749,	
838,861,	838,749,	838,772,	838,749,	
0,0,	0,0,	0,0,	0,0,	
839,721,	0,0,	0,0,	839,721,	
839,721,	0,0,	838,749,	0,0,	
0,0,	0,0,	0,0,	0,0,	
839,721,	839,749,	0,0,	0,0,	
838,749,	0,0,	0,0,	0,0,	
839,721,	839,721,	839,721,	839,721,	
839,721,	839,721,	839,750,	839,721,	
839,721,	0,0,	839,721,	839,721,	
839,721,	839,721,	839,721,	839,721,	
839,721,	839,721,	839,721,	839,721,	
852,0,	0,0,	839,721,	839,721,	
839,721,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
839,721,	839,721,	839,721,	839,721,	
839,721,	839,721,	839,750,	839,721,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	852,737,	839,721,	0,0,	
0,0,	0,0,	0,0,	852,737,	
0,0,	0,0,	852,737,	852,737,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	852,737,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	852,737,	
852,737,	852,737,	852,737,	852,737,	
852,737,	852,762,	852,737,	852,737,	
0,0,	852,737,	852,737,	852,737,	
852,737,	852,737,	852,737,	852,737,	
852,737,	852,737,	852,737,	861,749,	
0,0,	852,737,	852,737,	852,737,	
0,0,	0,0,	0,0,	861,749,	
0,0,	0,0,	0,0,	852,737,	
852,737,	852,737,	852,737,	852,737,	
852,737,	852,762,	852,737,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	852,737,	861,749,	0,0,	
861,749,	861,749,	0,0,	861,749,	
0,0,	861,749,	861,749,	861,749,	
0,0,	861,749,	0,0,	861,749,	
861,749,	861,749,	861,749,	861,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	861,749,	
861,749,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	861,749,	
861,749,	861,749,	861,749,	861,749,	
861,749,	861,772,	861,749,	861,749,	
0,0,	861,749,	861,749,	861,749,	
861,749,	861,749,	861,749,	861,749,	
861,749,	861,749,	861,749,	0,0,	
0,0,	861,749,	861,749,	861,749,	
0,0,	0,0,	0,0,	861,749,	
0,0,	0,0,	0,0,	861,749,	
861,749,	861,749,	861,749,	861,749,	
861,749,	861,772,	861,749,	881,881,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	881,881,	
881,884,	861,749,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	861,749,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	881,881,	0,0,	
881,881,	881,881,	0,0,	881,881,	
0,0,	881,881,	881,881,	881,881,	
0,0,	881,881,	0,0,	881,881,	
881,881,	881,881,	881,881,	881,881,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	881,881,	
881,881,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	881,881,	
881,881,	881,881,	881,881,	881,881,	
881,881,	881,881,	881,881,	881,881,	
0,0,	881,881,	881,881,	881,881,	
881,881,	881,881,	881,881,	881,881,	
881,881,	881,881,	881,881,	0,0,	
0,0,	881,881,	881,881,	881,881,	
0,0,	0,0,	0,0,	881,881,	
0,0,	0,0,	0,0,	881,881,	
881,881,	881,881,	881,881,	881,881,	
881,881,	881,881,	881,881,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	881,881,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	881,272,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-80,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+157,	0,		yyvstop+3,
yycrank+-279,	0,		yyvstop+6,
yycrank+29,	0,		yyvstop+8,
yycrank+48,	0,		yyvstop+11,
yycrank+63,	yysvec+4,	yyvstop+13,
yycrank+360,	yysvec+4,	yyvstop+16,
yycrank+434,	yysvec+4,	yyvstop+20,
yycrank+-517,	0,		yyvstop+24,
yycrank+594,	yysvec+4,	yyvstop+27,
yycrank+34,	yysvec+4,	yyvstop+31,
yycrank+58,	yysvec+4,	yyvstop+34,
yycrank+64,	yysvec+4,	yyvstop+37,
yycrank+622,	yysvec+4,	yyvstop+40,
yycrank+79,	yysvec+4,	yyvstop+43,
yycrank+682,	yysvec+4,	yyvstop+46,
yycrank+30,	yysvec+4,	yyvstop+49,
yycrank+404,	yysvec+4,	yyvstop+52,
yycrank+707,	yysvec+4,	yyvstop+55,
yycrank+70,	yysvec+4,	yyvstop+58,
yycrank+219,	yysvec+4,	yyvstop+61,
yycrank+-789,	0,		yyvstop+65,
yycrank+857,	yysvec+4,	yyvstop+67,
yycrank+886,	yysvec+4,	yyvstop+70,
yycrank+946,	yysvec+4,	yyvstop+74,
yycrank+970,	yysvec+4,	yyvstop+77,
yycrank+1042,	yysvec+4,	yyvstop+80,
yycrank+1066,	yysvec+4,	yyvstop+83,
yycrank+1126,	yysvec+4,	yyvstop+86,
yycrank+1157,	yysvec+4,	yyvstop+89,
yycrank+62,	yysvec+4,	yyvstop+92,
yycrank+-1233,	0,		yyvstop+95,
yycrank+31,	0,		yyvstop+97,
yycrank+1311,	0,		yyvstop+99,
yycrank+249,	yysvec+36,	yyvstop+102,
yycrank+1425,	yysvec+36,	yyvstop+105,
yycrank+1499,	yysvec+36,	yyvstop+109,
yycrank+-32,	yysvec+11,	yyvstop+113,
yycrank+1572,	yysvec+36,	yyvstop+116,
yycrank+93,	yysvec+36,	yyvstop+120,
yycrank+84,	yysvec+36,	yyvstop+123,
yycrank+107,	yysvec+36,	yyvstop+126,
yycrank+1600,	yysvec+36,	yyvstop+129,
yycrank+122,	yysvec+36,	yyvstop+132,
yycrank+1660,	yysvec+36,	yyvstop+135,
yycrank+41,	yysvec+36,	yyvstop+138,
yycrank+1091,	yysvec+36,	yyvstop+141,
yycrank+1685,	yysvec+36,	yyvstop+144,
yycrank+104,	yysvec+36,	yyvstop+147,
yycrank+260,	yysvec+36,	yyvstop+150,
yycrank+1745,	yysvec+36,	yyvstop+154,
yycrank+1774,	yysvec+36,	yyvstop+157,
yycrank+1834,	yysvec+36,	yyvstop+161,
yycrank+1858,	yysvec+36,	yyvstop+164,
yycrank+1930,	yysvec+36,	yyvstop+167,
yycrank+1954,	yysvec+36,	yyvstop+170,
yycrank+2014,	yysvec+36,	yyvstop+173,
yycrank+2045,	yysvec+36,	yyvstop+176,
yycrank+78,	yysvec+36,	yyvstop+179,
yycrank+-42,	yysvec+34,	yyvstop+182,
yycrank+0,	yysvec+4,	yyvstop+184,
yycrank+-2121,	0,		0,	
yycrank+-2225,	0,		0,	
yycrank+-252,	yysvec+64,	0,	
yycrank+78,	0,		0,	
yycrank+0,	0,		yyvstop+186,
yycrank+54,	0,		0,	
yycrank+237,	0,		0,	
yycrank+65,	yysvec+4,	yyvstop+188,
yycrank+70,	0,		0,	
yycrank+79,	yysvec+4,	yyvstop+190,
yycrank+0,	yysvec+72,	0,	
yycrank+2301,	0,		0,	
yycrank+240,	yysvec+75,	0,	
yycrank+95,	yysvec+4,	yyvstop+193,
yycrank+142,	0,		0,	
yycrank+2375,	yysvec+4,	yyvstop+195,
yycrank+122,	0,		0,	
yycrank+0,	yysvec+4,	yyvstop+198,
yycrank+0,	yysvec+11,	yyvstop+201,
yycrank+0,	0,		yyvstop+203,
yycrank+2278,	0,		0,	
yycrank+287,	yysvec+4,	yyvstop+205,
yycrank+298,	0,		0,	
yycrank+1485,	yysvec+4,	yyvstop+207,
yycrank+330,	0,		0,	
yycrank+395,	0,		0,	
yycrank+128,	yysvec+4,	yyvstop+209,
yycrank+2393,	yysvec+4,	yyvstop+211,
yycrank+97,	yysvec+4,	yyvstop+213,
yycrank+130,	yysvec+4,	yyvstop+215,
yycrank+99,	yysvec+4,	yyvstop+217,
yycrank+139,	yysvec+4,	yyvstop+219,
yycrank+118,	yysvec+4,	yyvstop+221,
yycrank+131,	yysvec+4,	yyvstop+223,
yycrank+101,	yysvec+4,	yyvstop+225,
yycrank+137,	yysvec+4,	yyvstop+227,
yycrank+132,	yysvec+4,	yyvstop+229,
yycrank+170,	yysvec+4,	yyvstop+231,
yycrank+171,	yysvec+4,	yyvstop+233,
yycrank+610,	yysvec+4,	yyvstop+236,
yycrank+169,	yysvec+4,	yyvstop+238,
yycrank+2453,	yysvec+4,	yyvstop+240,
yycrank+188,	yysvec+4,	yyvstop+242,
yycrank+186,	yysvec+4,	yyvstop+244,
yycrank+2461,	yysvec+84,	0,	
yycrank+213,	yysvec+4,	yyvstop+246,
yycrank+185,	yysvec+4,	yyvstop+248,
yycrank+-2550,	yysvec+24,	0,	
yycrank+-254,	yysvec+24,	0,	
yycrank+-2626,	yysvec+24,	0,	
yycrank+-593,	yysvec+24,	0,	
yycrank+0,	0,		yyvstop+250,
yycrank+586,	yysvec+4,	yyvstop+252,
yycrank+2676,	yysvec+4,	yyvstop+254,
yycrank+715,	0,		0,	
yycrank+797,	yysvec+4,	yyvstop+257,
yycrank+870,	yysvec+4,	yyvstop+259,
yycrank+2748,	yysvec+4,	yyvstop+261,
yycrank+866,	yysvec+4,	yyvstop+264,
yycrank+2766,	yysvec+4,	yyvstop+266,
yycrank+910,	0,		0,	
yycrank+938,	yysvec+4,	yyvstop+269,
yycrank+2838,	yysvec+4,	yyvstop+271,
yycrank+2910,	yysvec+4,	yyvstop+274,
yycrank+1026,	yysvec+4,	yyvstop+276,
yycrank+191,	yysvec+4,	yyvstop+278,
yycrank+192,	yysvec+4,	yyvstop+280,
yycrank+-281,	yysvec+34,	0,	
yycrank+-2986,	yysvec+34,	0,	
yycrank+-246,	yysvec+34,	0,	
yycrank+273,	0,		yyvstop+283,
yycrank+-3090,	0,		0,	
yycrank+0,	yysvec+36,	yyvstop+285,
yycrank+-3194,	0,		yyvstop+287,
yycrank+285,	yysvec+36,	yyvstop+289,
yycrank+258,	yysvec+36,	yyvstop+291,
yycrank+-1052,	yysvec+137,	yyvstop+294,
yycrank+260,	yysvec+36,	yyvstop+296,
yycrank+3275,	yysvec+36,	yyvstop+298,
yycrank+-264,	yysvec+137,	yyvstop+301,
yycrank+0,	yysvec+36,	yyvstop+303,
yycrank+-3358,	0,		yyvstop+306,
yycrank+1241,	yysvec+36,	yyvstop+308,
yycrank+2364,	yysvec+36,	yyvstop+310,
yycrank+-1339,	yysvec+137,	yyvstop+312,
yycrank+250,	yysvec+36,	yyvstop+314,
yycrank+3426,	yysvec+36,	yyvstop+316,
yycrank+220,	yysvec+36,	yyvstop+318,
yycrank+252,	yysvec+36,	yyvstop+320,
yycrank+230,	yysvec+36,	yyvstop+322,
yycrank+270,	yysvec+36,	yyvstop+324,
yycrank+242,	yysvec+36,	yyvstop+326,
yycrank+255,	yysvec+36,	yyvstop+328,
yycrank+224,	yysvec+36,	yyvstop+330,
yycrank+258,	yysvec+36,	yyvstop+332,
yycrank+227,	yysvec+36,	yyvstop+334,
yycrank+274,	yysvec+36,	yyvstop+336,
yycrank+286,	yysvec+36,	yyvstop+338,
yycrank+1395,	yysvec+36,	yyvstop+341,
yycrank+288,	yysvec+36,	yyvstop+343,
yycrank+3450,	yysvec+36,	yyvstop+345,
yycrank+308,	yysvec+36,	yyvstop+347,
yycrank+277,	yysvec+36,	yyvstop+349,
yycrank+302,	yysvec+36,	yyvstop+351,
yycrank+288,	yysvec+36,	yyvstop+353,
yycrank+1361,	yysvec+36,	yyvstop+355,
yycrank+3497,	yysvec+36,	yyvstop+357,
yycrank+-1395,	yysvec+137,	yyvstop+360,
yycrank+1413,	yysvec+36,	yyvstop+362,
yycrank+1508,	yysvec+36,	yyvstop+364,
yycrank+3569,	yysvec+36,	yyvstop+366,
yycrank+1540,	yysvec+36,	yyvstop+369,
yycrank+3587,	yysvec+36,	yyvstop+371,
yycrank+-1564,	yysvec+137,	yyvstop+374,
yycrank+1668,	yysvec+36,	yyvstop+376,
yycrank+3659,	yysvec+36,	yyvstop+378,
yycrank+3731,	yysvec+36,	yyvstop+381,
yycrank+1758,	yysvec+36,	yyvstop+383,
yycrank+292,	yysvec+36,	yyvstop+385,
yycrank+308,	yysvec+36,	yyvstop+387,
yycrank+-472,	yysvec+34,	0,	
yycrank+-379,	yysvec+64,	0,	
yycrank+-380,	yysvec+65,	0,	
yycrank+-3807,	yysvec+65,	0,	
yycrank+-3911,	0,		0,	
yycrank+-398,	yysvec+64,	0,	
yycrank+-520,	yysvec+64,	0,	
yycrank+-4015,	0,		0,	
yycrank+-4119,	0,		0,	
yycrank+0,	0,		yyvstop+390,
yycrank+390,	0,		0,	
yycrank+391,	0,		0,	
yycrank+392,	0,		0,	
yycrank+380,	yysvec+4,	yyvstop+392,
yycrank+404,	yysvec+4,	yyvstop+395,
yycrank+405,	0,		yyvstop+398,
yycrank+0,	yysvec+4,	yyvstop+400,
yycrank+0,	0,		yyvstop+403,
yycrank+413,	0,		0,	
yycrank+416,	0,		0,	
yycrank+417,	0,		0,	
yycrank+418,	yysvec+4,	yyvstop+405,
yycrank+420,	0,		yyvstop+408,
yycrank+577,	0,		yyvstop+410,
yycrank+395,	0,		0,	
yycrank+400,	0,		0,	
yycrank+402,	0,		0,	
yycrank+458,	0,		0,	
yycrank+469,	0,		0,	
yycrank+471,	0,		0,	
yycrank+634,	0,		0,	
yycrank+1802,	yysvec+4,	yyvstop+412,
yycrank+1846,	0,		0,	
yycrank+1826,	yysvec+4,	yyvstop+414,
yycrank+1866,	0,		0,	
yycrank+1940,	yysvec+4,	yyvstop+416,
yycrank+1970,	0,		0,	
yycrank+2026,	0,		0,	
yycrank+386,	yysvec+4,	yyvstop+418,
yycrank+2488,	yysvec+4,	yyvstop+420,
yycrank+355,	yysvec+4,	yyvstop+423,
yycrank+398,	yysvec+4,	yyvstop+425,
yycrank+370,	yysvec+4,	yyvstop+427,
yycrank+384,	yysvec+4,	yyvstop+429,
yycrank+356,	yysvec+4,	yyvstop+431,
yycrank+492,	yysvec+4,	yyvstop+433,
yycrank+407,	yysvec+4,	yyvstop+435,
yycrank+376,	yysvec+4,	yyvstop+437,
yycrank+474,	yysvec+4,	yyvstop+439,
yycrank+2737,	yysvec+4,	yyvstop+442,
yycrank+449,	yysvec+4,	yyvstop+444,
yycrank+420,	yysvec+4,	yyvstop+446,
yycrank+452,	yysvec+4,	yyvstop+448,
yycrank+422,	yysvec+4,	yyvstop+450,
yycrank+493,	0,		0,	
yycrank+500,	0,		0,	
yycrank+501,	0,		0,	
yycrank+516,	0,		0,	
yycrank+554,	0,		0,	
yycrank+636,	0,		0,	
yycrank+458,	yysvec+4,	yyvstop+452,
yycrank+427,	yysvec+4,	yyvstop+454,
yycrank+-4223,	yysvec+24,	0,	
yycrank+-4305,	yysvec+24,	0,	
yycrank+-4389,	0,		0,	
yycrank+2171,	yysvec+4,	yyvstop+456,
yycrank+1139,	0,		0,	
yycrank+2141,	yysvec+4,	yyvstop+459,
yycrank+2417,	0,		0,	
yycrank+505,	yysvec+250,	0,	
yycrank+509,	yysvec+4,	yyvstop+461,
yycrank+2694,	yysvec+4,	yyvstop+464,
yycrank+2766,	yysvec+4,	yyvstop+466,
yycrank+4445,	0,		yyvstop+469,
yycrank+2280,	yysvec+4,	yyvstop+471,
yycrank+2027,	0,		0,	
yycrank+513,	yysvec+259,	0,	
yycrank+526,	yysvec+4,	yyvstop+474,
yycrank+4479,	yysvec+4,	yyvstop+477,
yycrank+2668,	yysvec+4,	yyvstop+480,
yycrank+2808,	0,		0,	
yycrank+2878,	yysvec+4,	yyvstop+482,
yycrank+2926,	yysvec+4,	yyvstop+484,
yycrank+2950,	yysvec+257,	yyvstop+487,
yycrank+4513,	yysvec+4,	yyvstop+489,
yycrank+4540,	yysvec+4,	yyvstop+492,
yycrank+3053,	yysvec+4,	yyvstop+496,
yycrank+525,	yysvec+4,	yyvstop+499,
yycrank+0,	0,		yyvstop+502,
yycrank+-651,	yysvec+34,	0,	
yycrank+582,	0,		0,	
yycrank+600,	0,		0,	
yycrank+624,	0,		0,	
yycrank+528,	yysvec+36,	yyvstop+504,
yycrank+546,	yysvec+36,	yyvstop+507,
yycrank+0,	yysvec+36,	yyvstop+510,
yycrank+-575,	yysvec+137,	yyvstop+513,
yycrank+597,	yysvec+36,	yyvstop+516,
yycrank+-602,	yysvec+137,	yyvstop+519,
yycrank+0,	yysvec+135,	yyvstop+522,
yycrank+-641,	yysvec+11,	yyvstop+524,
yycrank+3063,	yysvec+36,	yyvstop+526,
yycrank+3109,	yysvec+36,	yyvstop+528,
yycrank+-3119,	yysvec+137,	yyvstop+530,
yycrank+3250,	yysvec+36,	yyvstop+532,
yycrank+-588,	yysvec+137,	yyvstop+534,
yycrank+573,	yysvec+36,	yyvstop+536,
yycrank+3558,	yysvec+36,	yyvstop+538,
yycrank+552,	yysvec+36,	yyvstop+541,
yycrank+586,	yysvec+36,	yyvstop+543,
yycrank+555,	yysvec+36,	yyvstop+545,
yycrank+570,	yysvec+36,	yyvstop+547,
yycrank+549,	yysvec+36,	yyvstop+549,
yycrank+649,	yysvec+36,	yyvstop+551,
yycrank+610,	yysvec+36,	yyvstop+553,
yycrank+600,	yysvec+36,	yyvstop+555,
yycrank+659,	yysvec+36,	yyvstop+557,
yycrank+4567,	yysvec+36,	yyvstop+560,
yycrank+632,	yysvec+36,	yyvstop+562,
yycrank+601,	yysvec+36,	yyvstop+564,
yycrank+635,	yysvec+36,	yyvstop+566,
yycrank+609,	yysvec+36,	yyvstop+568,
yycrank+634,	yysvec+36,	yyvstop+570,
yycrank+612,	yysvec+36,	yyvstop+572,
yycrank+2483,	yysvec+36,	yyvstop+574,
yycrank+3213,	yysvec+36,	yyvstop+577,
yycrank+-3236,	yysvec+137,	yyvstop+579,
yycrank+689,	yysvec+36,	yyvstop+581,
yycrank+3293,	yysvec+36,	yyvstop+584,
yycrank+3466,	yysvec+36,	yyvstop+586,
yycrank+-2290,	yysvec+137,	yyvstop+589,
yycrank+3857,	yysvec+36,	yyvstop+592,
yycrank+702,	yysvec+36,	yyvstop+595,
yycrank+4623,	yysvec+36,	yyvstop+598,
yycrank+3366,	yysvec+36,	yyvstop+601,
yycrank+-3388,	yysvec+137,	yyvstop+603,
yycrank+3537,	yysvec+36,	yyvstop+605,
yycrank+3587,	yysvec+36,	yyvstop+607,
yycrank+-4538,	yysvec+137,	yyvstop+610,
yycrank+4641,	yysvec+36,	yyvstop+613,
yycrank+4668,	yysvec+36,	yyvstop+616,
yycrank+3677,	yysvec+36,	yyvstop+620,
yycrank+704,	yysvec+36,	yyvstop+623,
yycrank+-743,	yysvec+34,	0,	
yycrank+0,	0,		yyvstop+626,
yycrank+0,	yysvec+188,	yyvstop+628,
yycrank+-710,	yysvec+188,	0,	
yycrank+-4752,	yysvec+65,	0,	
yycrank+0,	0,		yyvstop+630,
yycrank+-792,	yysvec+64,	0,	
yycrank+-741,	yysvec+191,	yyvstop+632,
yycrank+-745,	yysvec+192,	yyvstop+634,
yycrank+0,	yysvec+72,	0,	
yycrank+0,	yysvec+72,	0,	
yycrank+0,	yysvec+72,	0,	
yycrank+716,	yysvec+4,	yyvstop+636,
yycrank+727,	0,		0,	
yycrank+0,	yysvec+80,	0,	
yycrank+0,	yysvec+78,	0,	
yycrank+0,	yysvec+80,	0,	
yycrank+728,	0,		0,	
yycrank+729,	0,		0,	
yycrank+740,	0,		0,	
yycrank+683,	0,		0,	
yycrank+658,	0,		0,	
yycrank+693,	0,		0,	
yycrank+663,	0,		0,	
yycrank+710,	0,		0,	
yycrank+694,	0,		0,	
yycrank+678,	0,		0,	
yycrank+726,	0,		0,	
yycrank+695,	0,		0,	
yycrank+718,	0,		0,	
yycrank+688,	0,		0,	
yycrank+753,	0,		0,	
yycrank+0,	yysvec+4,	yyvstop+638,
yycrank+4799,	0,		0,	
yycrank+3699,	0,		0,	
yycrank+0,	yysvec+4,	yyvstop+641,
yycrank+0,	0,		yyvstop+644,
yycrank+0,	0,		yyvstop+646,
yycrank+745,	yysvec+4,	yyvstop+648,
yycrank+3693,	yysvec+4,	yyvstop+650,
yycrank+3701,	0,		0,	
yycrank+3771,	yysvec+4,	yyvstop+652,
yycrank+715,	yysvec+4,	yyvstop+654,
yycrank+764,	yysvec+4,	yyvstop+656,
yycrank+735,	yysvec+4,	yyvstop+658,
yycrank+871,	yysvec+4,	yyvstop+660,
yycrank+776,	0,		0,	
yycrank+776,	yysvec+4,	yyvstop+663,
yycrank+0,	yysvec+4,	yyvstop+665,
yycrank+765,	yysvec+4,	yyvstop+668,
yycrank+774,	yysvec+4,	yyvstop+670,
yycrank+750,	yysvec+4,	yyvstop+672,
yycrank+805,	yysvec+4,	yyvstop+674,
yycrank+778,	yysvec+4,	yyvstop+676,
yycrank+798,	0,		0,	
yycrank+768,	0,		0,	
yycrank+802,	0,		0,	
yycrank+771,	0,		0,	
yycrank+833,	0,		0,	
yycrank+821,	0,		0,	
yycrank+801,	0,		0,	
yycrank+844,	0,		0,	
yycrank+813,	0,		0,	
yycrank+865,	0,		0,	
yycrank+860,	yysvec+4,	yyvstop+678,
yycrank+829,	yysvec+4,	yyvstop+680,
yycrank+-4875,	yysvec+24,	0,	
yycrank+-4956,	yysvec+24,	0,	
yycrank+-5041,	0,		yyvstop+682,
yycrank+891,	0,		yyvstop+684,
yycrank+3875,	0,		0,	
yycrank+0,	0,		yyvstop+686,
yycrank+3961,	yysvec+4,	yyvstop+688,
yycrank+4065,	0,		yyvstop+691,
yycrank+893,	0,		yyvstop+693,
yycrank+2496,	0,		0,	
yycrank+906,	yysvec+402,	0,	
yycrank+906,	yysvec+4,	yyvstop+695,
yycrank+0,	0,		yyvstop+698,
yycrank+3979,	0,		0,	
yycrank+4169,	yysvec+4,	yyvstop+700,
yycrank+0,	yysvec+400,	yyvstop+704,
yycrank+4666,	yysvec+4,	yyvstop+707,
yycrank+4083,	yysvec+4,	yyvstop+711,
yycrank+840,	yysvec+4,	yyvstop+714,
yycrank+-923,	yysvec+34,	0,	
yycrank+0,	yysvec+6,	yyvstop+716,
yycrank+1067,	0,		0,	
yycrank+1102,	0,		0,	
yycrank+855,	0,		0,	
yycrank+842,	0,		0,	
yycrank+-5145,	yysvec+137,	yyvstop+718,
yycrank+925,	yysvec+36,	yyvstop+720,
yycrank+-936,	yysvec+137,	yyvstop+722,
yycrank+-938,	yysvec+137,	yyvstop+724,
yycrank+-882,	yysvec+11,	yyvstop+726,
yycrank+-871,	yysvec+11,	yyvstop+728,
yycrank+0,	yysvec+36,	yyvstop+730,
yycrank+-943,	yysvec+137,	yyvstop+733,
yycrank+0,	yysvec+36,	yyvstop+735,
yycrank+0,	yysvec+137,	yyvstop+738,
yycrank+918,	yysvec+36,	yyvstop+741,
yycrank+4139,	yysvec+36,	yyvstop+743,
yycrank+-4231,	yysvec+137,	yyvstop+745,
yycrank+4291,	yysvec+36,	yyvstop+747,
yycrank+896,	yysvec+36,	yyvstop+749,
yycrank+930,	yysvec+36,	yyvstop+751,
yycrank+909,	yysvec+36,	yyvstop+753,
yycrank+1051,	yysvec+36,	yyvstop+755,
yycrank+958,	yysvec+36,	yyvstop+758,
yycrank+0,	yysvec+36,	yyvstop+760,
yycrank+938,	yysvec+36,	yyvstop+763,
yycrank+956,	yysvec+36,	yyvstop+765,
yycrank+926,	yysvec+36,	yyvstop+767,
yycrank+962,	yysvec+36,	yyvstop+769,
yycrank+931,	yysvec+36,	yyvstop+771,
yycrank+964,	yysvec+36,	yyvstop+773,
yycrank+950,	yysvec+36,	yyvstop+775,
yycrank+-1004,	yysvec+137,	yyvstop+777,
yycrank+5091,	yysvec+36,	yyvstop+779,
yycrank+-1166,	yysvec+137,	yyvstop+782,
yycrank+-4797,	yysvec+137,	yyvstop+784,
yycrank+-2819,	yysvec+137,	yyvstop+787,
yycrank+-1013,	yysvec+137,	yyvstop+789,
yycrank+1014,	yysvec+36,	yyvstop+792,
yycrank+-1007,	yysvec+137,	yyvstop+795,
yycrank+5190,	yysvec+36,	yyvstop+797,
yycrank+-4808,	yysvec+137,	yyvstop+801,
yycrank+5196,	yysvec+36,	yyvstop+805,
yycrank+4942,	yysvec+36,	yyvstop+809,
yycrank+946,	yysvec+36,	yyvstop+812,
yycrank+-1238,	yysvec+34,	0,	
yycrank+-1019,	yysvec+64,	0,	
yycrank+-5280,	0,		0,	
yycrank+-5384,	0,		0,	
yycrank+-5488,	0,		0,	
yycrank+-5592,	0,		0,	
yycrank+-5696,	0,		0,	
yycrank+-5800,	0,		0,	
yycrank+1014,	0,		0,	
yycrank+1081,	0,		0,	
yycrank+995,	0,		0,	
yycrank+964,	0,		0,	
yycrank+1015,	0,		0,	
yycrank+984,	0,		0,	
yycrank+1103,	0,		0,	
yycrank+1019,	0,		0,	
yycrank+988,	0,		0,	
yycrank+1040,	0,		0,	
yycrank+1009,	0,		0,	
yycrank+1050,	0,		0,	
yycrank+1021,	0,		0,	
yycrank+1116,	0,		yyvstop+814,
yycrank+4361,	0,		0,	
yycrank+0,	0,		yyvstop+816,
yycrank+1099,	yysvec+4,	yyvstop+818,
yycrank+5330,	yysvec+4,	yyvstop+821,
yycrank+1074,	yysvec+4,	yyvstop+824,
yycrank+2022,	0,		0,	
yycrank+1100,	yysvec+4,	yyvstop+826,
yycrank+1081,	0,		0,	
yycrank+1064,	yysvec+4,	yyvstop+829,
yycrank+1034,	yysvec+4,	yyvstop+831,
yycrank+1058,	yysvec+4,	yyvstop+833,
yycrank+1027,	yysvec+4,	yyvstop+835,
yycrank+1073,	yysvec+4,	yyvstop+837,
yycrank+1043,	yysvec+4,	yyvstop+839,
yycrank+1087,	0,		0,	
yycrank+1056,	0,		0,	
yycrank+1088,	0,		0,	
yycrank+1057,	0,		0,	
yycrank+1288,	0,		0,	
yycrank+1104,	0,		0,	
yycrank+1073,	0,		0,	
yycrank+1123,	0,		0,	
yycrank+1092,	0,		0,	
yycrank+1209,	0,		yyvstop+841,
yycrank+1166,	yysvec+4,	yyvstop+843,
yycrank+-5904,	0,		0,	
yycrank+0,	0,		yyvstop+846,
yycrank+0,	yysvec+395,	0,	
yycrank+1230,	0,		0,	
yycrank+5434,	0,		yyvstop+849,
yycrank+4904,	0,		0,	
yycrank+1160,	yysvec+510,	0,	
yycrank+1160,	yysvec+4,	yyvstop+851,
yycrank+1162,	0,		yyvstop+854,
yycrank+0,	0,		yyvstop+856,
yycrank+0,	yysvec+509,	yyvstop+858,
yycrank+-1485,	yysvec+34,	0,	
yycrank+0,	yysvec+72,	0,	
yycrank+1249,	0,		0,	
yycrank+0,	yysvec+80,	0,	
yycrank+1107,	0,		0,	
yycrank+1107,	0,		0,	
yycrank+-6008,	yysvec+137,	yyvstop+861,
yycrank+-6112,	yysvec+137,	yyvstop+863,
yycrank+-6216,	yysvec+137,	yyvstop+865,
yycrank+-6320,	yysvec+137,	yyvstop+867,
yycrank+-6424,	yysvec+137,	yyvstop+869,
yycrank+-1124,	yysvec+11,	yyvstop+871,
yycrank+-1120,	yysvec+11,	yyvstop+873,
yycrank+0,	yysvec+137,	yyvstop+875,
yycrank+1206,	yysvec+36,	yyvstop+878,
yycrank+-1201,	yysvec+137,	yyvstop+881,
yycrank+5538,	yysvec+36,	yyvstop+883,
yycrank+1216,	yysvec+36,	yyvstop+886,
yycrank+1219,	yysvec+36,	yyvstop+888,
yycrank+1194,	yysvec+36,	yyvstop+891,
yycrank+1165,	yysvec+36,	yyvstop+893,
yycrank+1191,	yysvec+36,	yyvstop+895,
yycrank+1161,	yysvec+36,	yyvstop+897,
yycrank+1198,	yysvec+36,	yyvstop+899,
yycrank+1175,	yysvec+36,	yyvstop+901,
yycrank+1253,	yysvec+36,	yyvstop+903,
yycrank+-5642,	yysvec+137,	yyvstop+906,
yycrank+1247,	yysvec+36,	yyvstop+909,
yycrank+-2149,	yysvec+137,	yyvstop+912,
yycrank+-4439,	yysvec+137,	yyvstop+914,
yycrank+-1248,	yysvec+137,	yyvstop+916,
yycrank+0,	yysvec+137,	yyvstop+919,
yycrank+-5746,	yysvec+137,	yyvstop+922,
yycrank+-1521,	yysvec+34,	0,	
yycrank+0,	0,		yyvstop+926,
yycrank+-1287,	yysvec+460,	yyvstop+929,
yycrank+-1308,	yysvec+461,	yyvstop+931,
yycrank+-1309,	yysvec+462,	yyvstop+933,
yycrank+-1317,	yysvec+463,	yyvstop+935,
yycrank+-1318,	yysvec+464,	yyvstop+937,
yycrank+-1319,	yysvec+465,	yyvstop+939,
yycrank+1299,	0,		0,	
yycrank+1297,	0,		0,	
yycrank+1267,	0,		0,	
yycrank+1237,	0,		0,	
yycrank+1283,	0,		0,	
yycrank+1252,	0,		0,	
yycrank+1287,	0,		0,	
yycrank+1322,	0,		yyvstop+941,
yycrank+1272,	0,		0,	
yycrank+1256,	0,		0,	
yycrank+1291,	0,		0,	
yycrank+1260,	0,		0,	
yycrank+1304,	0,		0,	
yycrank+1302,	0,		0,	
yycrank+1461,	0,		0,	
yycrank+1374,	0,		yyvstop+943,
yycrank+5850,	0,		yyvstop+945,
yycrank+1384,	yysvec+84,	0,	
yycrank+5220,	0,		0,	
yycrank+1367,	yysvec+575,	0,	
yycrank+1412,	yysvec+4,	yyvstop+947,
yycrank+1389,	yysvec+4,	yyvstop+950,
yycrank+-6528,	0,		0,	
yycrank+1384,	0,		0,	
yycrank+1367,	0,		0,	
yycrank+1420,	0,		0,	
yycrank+1397,	0,		0,	
yycrank+1366,	0,		0,	
yycrank+1385,	yysvec+4,	yyvstop+952,
yycrank+1354,	yysvec+4,	yyvstop+954,
yycrank+2248,	yysvec+4,	yyvstop+956,
yycrank+1420,	yysvec+4,	yyvstop+959,
yycrank+1393,	yysvec+4,	yyvstop+961,
yycrank+1420,	0,		0,	
yycrank+1390,	0,		0,	
yycrank+1449,	0,		0,	
yycrank+1418,	0,		0,	
yycrank+1453,	0,		0,	
yycrank+1488,	0,		yyvstop+963,
yycrank+1458,	0,		0,	
yycrank+1429,	0,		0,	
yycrank+1475,	0,		0,	
yycrank+1464,	0,		0,	
yycrank+1535,	0,		0,	
yycrank+1539,	0,		yyvstop+965,
yycrank+6568,	0,		0,	
yycrank+0,	0,		yyvstop+967,
yycrank+0,	0,		yyvstop+970,
yycrank+1535,	0,		yyvstop+972,
yycrank+0,	0,		yyvstop+974,
yycrank+0,	yysvec+134,	yyvstop+976,
yycrank+1489,	0,		0,	
yycrank+1486,	0,		0,	
yycrank+-6651,	yysvec+137,	yyvstop+979,
yycrank+-6755,	yysvec+137,	yyvstop+982,
yycrank+-6859,	yysvec+137,	yyvstop+985,
yycrank+-6963,	yysvec+137,	yyvstop+988,
yycrank+-1491,	yysvec+11,	yyvstop+991,
yycrank+-1488,	yysvec+11,	yyvstop+993,
yycrank+-5954,	yysvec+137,	yyvstop+995,
yycrank+1553,	yysvec+36,	yyvstop+998,
yycrank+1592,	yysvec+36,	yyvstop+1001,
yycrank+1512,	yysvec+36,	yyvstop+1003,
yycrank+1492,	yysvec+36,	yyvstop+1005,
yycrank+2276,	yysvec+36,	yyvstop+1007,
yycrank+1543,	yysvec+36,	yyvstop+1010,
yycrank+1518,	yysvec+36,	yyvstop+1012,
yycrank+-2578,	yysvec+137,	yyvstop+1014,
yycrank+-5201,	yysvec+137,	yyvstop+1016,
yycrank+-1575,	yysvec+137,	yyvstop+1018,
yycrank+0,	yysvec+137,	yyvstop+1021,
yycrank+-1633,	yysvec+34,	0,	
yycrank+1586,	0,		0,	
yycrank+1588,	0,		yyvstop+1024,
yycrank+1545,	0,		0,	
yycrank+1515,	0,		0,	
yycrank+1559,	0,		0,	
yycrank+1528,	0,		0,	
yycrank+1580,	0,		0,	
yycrank+0,	0,		yyvstop+1026,
yycrank+1551,	0,		0,	
yycrank+1520,	0,		0,	
yycrank+1599,	0,		0,	
yycrank+1569,	0,		0,	
yycrank+-7067,	0,		0,	
yycrank+1607,	0,		0,	
yycrank+1642,	0,		yyvstop+1028,
yycrank+1614,	0,		0,	
yycrank+0,	0,		yyvstop+1030,
yycrank+-1674,	yysvec+579,	yyvstop+1032,
yycrank+1604,	0,		0,	
yycrank+1579,	0,		0,	
yycrank+1662,	0,		0,	
yycrank+1615,	0,		0,	
yycrank+1584,	0,		0,	
yycrank+1623,	0,		0,	
yycrank+1593,	0,		0,	
yycrank+1638,	yysvec+4,	yyvstop+1034,
yycrank+1607,	yysvec+4,	yyvstop+1036,
yycrank+1655,	yysvec+4,	yyvstop+1038,
yycrank+0,	yysvec+4,	yyvstop+1040,
yycrank+1641,	yysvec+4,	yyvstop+1043,
yycrank+1610,	yysvec+4,	yyvstop+1045,
yycrank+1643,	0,		0,	
yycrank+1612,	0,		0,	
yycrank+1656,	0,		0,	
yycrank+1626,	0,		0,	
yycrank+1663,	0,		0,	
yycrank+0,	0,		yyvstop+1047,
yycrank+1655,	0,		0,	
yycrank+1634,	0,		0,	
yycrank+-7171,	0,		0,	
yycrank+1667,	0,		0,	
yycrank+1714,	0,		0,	
yycrank+1631,	0,		0,	
yycrank+-1718,	yysvec+11,	yyvstop+1049,
yycrank+-1635,	yysvec+11,	yyvstop+1051,
yycrank+-2591,	yysvec+137,	yyvstop+1053,
yycrank+-6058,	yysvec+137,	yyvstop+1055,
yycrank+-1711,	yysvec+137,	yyvstop+1057,
yycrank+1670,	yysvec+36,	yyvstop+1060,
yycrank+1641,	yysvec+36,	yyvstop+1062,
yycrank+1734,	yysvec+36,	yyvstop+1064,
yycrank+0,	yysvec+36,	yyvstop+1066,
yycrank+1691,	yysvec+36,	yyvstop+1069,
yycrank+1660,	yysvec+36,	yyvstop+1071,
yycrank+0,	yysvec+137,	yyvstop+1073,
yycrank+-1894,	yysvec+34,	0,	
yycrank+0,	yysvec+558,	0,	
yycrank+1738,	0,		0,	
yycrank+1696,	0,		0,	
yycrank+1666,	0,		0,	
yycrank+1707,	0,		0,	
yycrank+1676,	0,		0,	
yycrank+1726,	0,		0,	
yycrank+1761,	0,		0,	
yycrank+2617,	0,		yyvstop+1076,
yycrank+-1805,	yysvec+641,	yyvstop+1078,
yycrank+1784,	0,		0,	
yycrank+1734,	0,		0,	
yycrank+1703,	0,		0,	
yycrank+1751,	0,		0,	
yycrank+1721,	0,		0,	
yycrank+2024,	0,		0,	
yycrank+1762,	0,		0,	
yycrank+1734,	0,		0,	
yycrank+1754,	0,		0,	
yycrank+1724,	0,		0,	
yycrank+-7275,	0,		yyvstop+1080,
yycrank+1771,	yysvec+4,	yyvstop+1082,
yycrank+1740,	yysvec+4,	yyvstop+1084,
yycrank+1777,	0,		0,	
yycrank+1748,	0,		0,	
yycrank+1780,	0,		0,	
yycrank+1750,	0,		0,	
yycrank+1837,	0,		0,	
yycrank+2759,	0,		yyvstop+1086,
yycrank+-1857,	yysvec+668,	yyvstop+1088,
yycrank+1836,	0,		0,	
yycrank+1767,	0,		0,	
yycrank+1765,	0,		0,	
yycrank+-1778,	yysvec+11,	yyvstop+1090,
yycrank+-1776,	yysvec+11,	yyvstop+1092,
yycrank+0,	yysvec+137,	yyvstop+1094,
yycrank+-7339,	yysvec+705,	yyvstop+1097,
yycrank+1822,	yysvec+36,	yyvstop+1099,
yycrank+1792,	yysvec+36,	yyvstop+1101,
yycrank+-1896,	yysvec+34,	0,	
yycrank+1872,	0,		0,	
yycrank+1873,	0,		0,	
yycrank+1849,	0,		0,	
yycrank+1850,	0,		0,	
yycrank+0,	0,		yyvstop+1103,
yycrank+1836,	0,		0,	
yycrank+1852,	0,		0,	
yycrank+1822,	0,		0,	
yycrank+1879,	0,		0,	
yycrank+1870,	0,		0,	
yycrank+1889,	0,		0,	
yycrank+1858,	0,		0,	
yycrank+-7414,	yysvec+705,	0,	
yycrank+-2026,	yysvec+705,	yyvstop+1105,
yycrank+0,	yysvec+4,	yyvstop+1107,
yycrank+1944,	0,		0,	
yycrank+1945,	0,		0,	
yycrank+1943,	0,		0,	
yycrank+0,	0,		yyvstop+1110,
yycrank+1929,	0,		0,	
yycrank+1882,	0,		0,	
yycrank+1886,	0,		0,	
yycrank+-1893,	yysvec+11,	yyvstop+1112,
yycrank+-1889,	yysvec+11,	yyvstop+1114,
yycrank+-7498,	yysvec+137,	yyvstop+1116,
yycrank+-7562,	yysvec+705,	yyvstop+1118,
yycrank+0,	yysvec+36,	yyvstop+1120,
yycrank+-1990,	yysvec+34,	0,	
yycrank+2087,	0,		0,	
yycrank+1930,	0,		0,	
yycrank+1934,	0,		0,	
yycrank+1944,	0,		0,	
yycrank+1922,	0,		0,	
yycrank+1891,	0,		0,	
yycrank+2578,	0,		yyvstop+1123,
yycrank+1942,	0,		0,	
yycrank+1932,	0,		0,	
yycrank+-7637,	yysvec+705,	0,	
yycrank+-2030,	yysvec+705,	yyvstop+1125,
yycrank+-2050,	yysvec+705,	yyvstop+1127,
yycrank+2098,	0,		0,	
yycrank+1957,	0,		0,	
yycrank+2077,	0,		0,	
yycrank+1936,	0,		0,	
yycrank+1951,	0,		0,	
yycrank+-1963,	yysvec+11,	yyvstop+1129,
yycrank+-1973,	yysvec+11,	yyvstop+1131,
yycrank+-7721,	yysvec+137,	yyvstop+1133,
yycrank+-7785,	yysvec+705,	yyvstop+1135,
yycrank+-7860,	yysvec+705,	yyvstop+1137,
yycrank+-2206,	yysvec+34,	0,	
yycrank+2052,	0,		0,	
yycrank+2078,	0,		0,	
yycrank+2102,	0,		0,	
yycrank+2020,	0,		0,	
yycrank+1989,	0,		0,	
yycrank+2033,	0,		0,	
yycrank+2017,	0,		0,	
yycrank+2049,	0,		0,	
yycrank+2018,	0,		0,	
yycrank+0,	0,		yyvstop+1139,
yycrank+2060,	0,		0,	
yycrank+2031,	0,		0,	
yycrank+2051,	0,		0,	
yycrank+2038,	0,		0,	
yycrank+-7935,	yysvec+705,	0,	
yycrank+-8010,	yysvec+705,	0,	
yycrank+-2147,	yysvec+705,	yyvstop+1141,
yycrank+-2166,	yysvec+705,	yyvstop+1143,
yycrank+2139,	0,		0,	
yycrank+2107,	0,		0,	
yycrank+2108,	0,		0,	
yycrank+2095,	0,		0,	
yycrank+2064,	0,		0,	
yycrank+2076,	0,		0,	
yycrank+2196,	0,		0,	
yycrank+-2128,	yysvec+11,	yyvstop+1145,
yycrank+-2201,	yysvec+11,	yyvstop+1147,
yycrank+-8094,	0,		yyvstop+1149,
yycrank+-2171,	yysvec+803,	yyvstop+1151,
yycrank+-8169,	yysvec+705,	yyvstop+1153,
yycrank+-8233,	yysvec+705,	yyvstop+1155,
yycrank+-2230,	yysvec+34,	0,	
yycrank+2169,	0,		0,	
yycrank+2172,	0,		0,	
yycrank+2148,	0,		0,	
yycrank+2184,	0,		0,	
yycrank+2167,	0,		0,	
yycrank+2190,	0,		0,	
yycrank+2167,	0,		0,	
yycrank+2220,	0,		0,	
yycrank+2198,	0,		0,	
yycrank+2237,	0,		0,	
yycrank+2206,	0,		0,	
yycrank+2241,	0,		0,	
yycrank+2211,	0,		0,	
yycrank+2259,	0,		0,	
yycrank+2229,	0,		0,	
yycrank+-8308,	yysvec+705,	0,	
yycrank+-8383,	yysvec+705,	0,	
yycrank+-2325,	yysvec+705,	yyvstop+1157,
yycrank+2250,	0,		0,	
yycrank+2258,	0,		0,	
yycrank+2228,	0,		0,	
yycrank+2265,	0,		0,	
yycrank+2235,	0,		0,	
yycrank+2283,	0,		0,	
yycrank+2252,	0,		0,	
yycrank+-8467,	0,		yyvstop+1160,
yycrank+2262,	0,		0,	
yycrank+-8571,	0,		yyvstop+1162,
yycrank+-2264,	yysvec+11,	yyvstop+1165,
yycrank+-8675,	yysvec+137,	yyvstop+1167,
yycrank+-8779,	yysvec+137,	yyvstop+1169,
yycrank+-8843,	yysvec+705,	yyvstop+1171,
yycrank+-2479,	yysvec+34,	0,	
yycrank+0,	0,		yyvstop+1174,
yycrank+2301,	0,		0,	
yycrank+2272,	0,		0,	
yycrank+2296,	0,		yyvstop+1176,
yycrank+2363,	0,		yyvstop+1178,
yycrank+2327,	0,		0,	
yycrank+2309,	0,		0,	
yycrank+2328,	0,		0,	
yycrank+2300,	0,		0,	
yycrank+2338,	0,		0,	
yycrank+2307,	0,		0,	
yycrank+-8918,	yysvec+705,	yyvstop+1180,
yycrank+0,	0,		yyvstop+1182,
yycrank+2350,	0,		0,	
yycrank+2319,	0,		0,	
yycrank+2401,	0,		yyvstop+1184,
yycrank+0,	0,		yyvstop+1186,
yycrank+2329,	0,		0,	
yycrank+0,	0,		yyvstop+1188,
yycrank+-2330,	yysvec+11,	yyvstop+1191,
yycrank+-9002,	yysvec+137,	yyvstop+1193,
yycrank+-2555,	yysvec+34,	0,	
yycrank+0,	0,		yyvstop+1196,
yycrank+0,	0,		yyvstop+1198,
yycrank+2396,	0,		0,	
yycrank+2370,	0,		0,	
yycrank+2340,	0,		0,	
yycrank+2362,	0,		0,	
yycrank+2344,	0,		0,	
yycrank+0,	0,		yyvstop+1200,
yycrank+2418,	0,		0,	
yycrank+2351,	0,		0,	
yycrank+-2352,	yysvec+11,	yyvstop+1202,
yycrank+-2736,	yysvec+34,	0,	
yycrank+2385,	0,		0,	
yycrank+2354,	0,		0,	
yycrank+0,	yysvec+799,	0,	
yycrank+-2370,	yysvec+11,	yyvstop+1204,
yycrank+-2631,	yysvec+34,	0,	
yycrank+2977,	0,		0,	
yycrank+-9106,	0,		0,	
yycrank+2412,	0,		0,	
yycrank+0,	0,		yyvstop+1206,
yycrank+-2438,	yysvec+881,	0,	
yycrank+-2441,	yysvec+881,	0,	
yycrank+-2454,	yysvec+881,	0,	
yycrank+-2455,	yysvec+881,	0,	
yycrank+-2456,	yysvec+881,	0,	
yycrank+-2465,	yysvec+881,	0,	
yycrank+-2477,	yysvec+881,	0,	
yycrank+-2480,	yysvec+881,	0,	
yycrank+-2484,	yysvec+881,	0,	
yycrank+-2485,	yysvec+881,	0,	
yycrank+-2487,	yysvec+881,	0,	
yycrank+-2489,	yysvec+881,	0,	
yycrank+-2490,	yysvec+881,	0,	
yycrank+-2494,	yysvec+881,	0,	
yycrank+-2415,	yysvec+881,	0,	
yycrank+2532,	0,		yyvstop+1208,
yycrank+0,	0,		yyvstop+1211,
0,	0,	0};
struct yywork *yytop = yycrank+9231;
struct yysvf *yybgin = yysvec+1;
unsigned char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
' ' ,01  ,'"' ,'#' ,01  ,'%' ,01  ,047 ,
'(' ,')' ,01  ,'+' ,01  ,'-' ,'.' ,'/' ,
'0' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,
'1' ,'9' ,':' ,'%' ,01  ,'+' ,01  ,01  ,
01  ,'A' ,'B' ,'C' ,'D' ,'E' ,'F' ,'G' ,
'H' ,'I' ,'H' ,'K' ,'L' ,'M' ,'N' ,'O' ,
'P' ,'Q' ,'R' ,'S' ,'T' ,'H' ,'H' ,'W' ,
'X' ,'Y' ,'H' ,01  ,01  ,']' ,01  ,'+' ,
01  ,'a' ,'b' ,'c' ,'d' ,'e' ,'f' ,'g' ,
'h' ,'I' ,'H' ,'K' ,'L' ,'M' ,'N' ,'O' ,
'P' ,'Q' ,'R' ,'s' ,'T' ,'H' ,'H' ,'W' ,
'X' ,'Y' ,'H' ,01  ,01  ,'}' ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
unsigned char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
1,0,0,1,0,0,1,0,
1,0,0,0,0,0,0,0,
0};
/*
 * (c) Copyright 1990, OPEN SOFTWARE FOUNDATION, INC.
 * ALL RIGHTS RESERVED
 */
/*
 * OSF/1 Release 1.0
*/
/*
#
# IBM CONFIDENTIAL
# Copyright International Business Machines Corp. 1989
# Unpublished Work
# All Rights Reserved
# Licensed Material - Property of IBM
#
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# 
*/
/* @(#)ncform	1.3  com/lib/l,3.1,8951 9/7/89 18:48:47 */
int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
unsigned char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
unsigned char yysbuf[YYLMAX];
unsigned char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	unsigned char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( yyt > yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if(yyt < yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
