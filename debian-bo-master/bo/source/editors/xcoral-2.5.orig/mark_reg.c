/* ########################################################################

			       mark_reg.c

   File: mark_reg.c
   Path: /home/fournigault/c/X11/xcoral-2.31/mark_reg.c
   Description: 
   Created: Fri Jan 27 11:19:56 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:19:57 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "main_text.h"
#include "mark_reg.h"
#include "text_cursor.h"
#include "page.h"
#include "chars_cmds.h"
#include "ie_func.h"
#include "input_str.h"
#include "dial_box.h"
#include "get_file.h"

/* #define DEBUG */


extern Display *dpy;
extern void StoreInKillBuf ();

FCT (static void, AfterMarkOnCurrent, (Text *text, int len, int lines) );
FCT (static int, BeforeMarkOnCurrent, (Text *text, int len, int lines) );
FCT (static void, BigRegion, (Text *text, int op) );
FCT (static int, MarkNotOnCurrent, (Text *text, int lines) );
FCT (static int, OnMark, (Text *text, int len, int lines) );
FCT (static void, SmallRegion, (Text *text, int op) );
FCT (static void, eval_expression, (Text *text, char *str) );

static int ml, mp;

/*
**	Function name : UpdateMark
**
**	Description : Maintient si possible la position de la marque apres
**		une modification du buffer. Cependant, la marque est 
**		remise a zero dans les deux cas suivants : GlobalReplace,
**		et InsertFile.
**
**	Input : Le text courant, la longueur de la chaine ajoutee
**		ou enlevee et le nombre de lignes.
**	Ouput : 1 si la position de la marque sur sa ligne doit etre
**		recalcule, 0 sinon.
*/
int UpdateMark ( text, len, lines )
    Text *text;
    int len, lines;
{
    int result;
    
    if ( text -> markline == 0 )
      return 0;
    
    if ( text -> no_current_line == text -> markline ) {
	/*
	    * La position courante est sur la meme ligne que la marque.
	    */
	if ( text -> markpos == GetNcFromLeft ( text -> buf ))
	  result =  OnMark ( text, len, lines );
	else if ( text -> markpos < GetNcFromLeft ( text -> buf )) {
	  (void) AfterMarkOnCurrent ( text, len, lines );
	  result = 0;
	}
	else
	  result = BeforeMarkOnCurrent ( text, len, lines );
    }
    else
      result =  MarkNotOnCurrent ( text, lines );
    
    if ( (text -> markline > text -> lines_in_buf) || (text -> markline <= 0)
	 || (text -> markpos < 0) ) {
#ifdef DEBUG
	(void) fprintf ( stderr, "Update mark error\n" );
#endif /* DEBUG */
	ResetMark ( text );
	return 0;
    }
    else 
      return (result);
}

/*
**	Function name : MarkNotOnCurrent
**
**	Description : Traitement du cas ou la marque n'est pas
**		 sur la ligne courante.
**		
**	Input : Le text courant, la longueur de la chaine ajoutee
**		ou enlevee et le nombre de lignes.
**	Ouput : 1 si la position de la marque sur sa ligne doit etre
**		recalcule, 0 sinon.
*/
static int MarkNotOnCurrent ( text, lines )
    Text *text;
    int lines;
{
    int diff;
    
    if ( lines == 0 ) {
#ifdef DEBUG
	(void) fprintf ( stderr, "Mark not on current : lines = 0\n" );
#endif /* DEBUG */
	return 0;
    }
    if ( lines > 0 ) { /* On ajoute des lignes */
	if ( text -> markline > text -> no_current_line ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark not on current : add lines, mark after current\n" );
#endif /* DEBUG */
	    text -> markline += lines;
	    return 0;
	}
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark not on current : add lines, mark before current\n" );
#endif /* DEBUG */
	    return 0;
	}
    }
    if ( lines < 0 ) { /* On efface des lignes */
	if ( text -> markline < text -> no_current_line ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark not on current : delete lines, mark before current\n" );
#endif /* DEBUG */
	    return 0;
	}
	else {
	    diff = text -> markline - text -> no_current_line;
	    if ( diff > -lines ){
#ifdef DEBUG
		(void) fprintf ( stderr, "Mark not on current : delete lines, mark after current, del < diff\n" );
#endif /* DEBUG */
		text -> markline += lines;
		return 0;	
	    }
	    if ( diff == -lines ){
		if ( text -> markpos < GetNcFromLeft ( text -> buf )) {
#ifdef DEBUG
		    (void) fprintf ( stderr, "Mark not on current : delete lines, mark after current, del = diff, pos before current\n" );
#endif /* DEBUG */
				   ResetMark (text);
				   return 0;
		}
		else {
#ifdef DEBUG
		    (void) fprintf ( stderr, "Mark not on current : delete lines, mark after current, del < diff, pos after current\n" );
#endif /* DEBUG */
				   text -> markline += lines;
				   text -> markpos -= GetNcFromLeft ( text -> buf );
				   return 1;
		}
	    }
	    else {
#ifdef DEBUG
		(void) fprintf ( stderr, "Mark not on current : delete lines, mark after current, del > diff\n" );
#endif /* DEBUG */
		ResetMark ( text );
		return 0;
	    }
	}
    }
    return 0;
}

/*
**	Function name : BeforeMarkOnCurrent
**
**	Description : Traitement du cas  ou le curseur est sur la
**		meme ligne que la marque mais avant celle-ci.
**
**	Input : Le text courant, la longueur de la chaine ajoutee
**		ou enlevee et le nombre de lignes.
**	Ouput : 1 si la position de la marque sur sa ligne doit etre
**		recalcule, 0 sinon.
*/
static int BeforeMarkOnCurrent ( text, len, lines )
    Text *text;
    int len, lines;
{
    /* On est avant la marque (sur la meme ligne) */
    if ( lines > 0 ) {
	text -> markline += lines;
	text -> markpos -= GetNcFromLeft ( text -> buf );
	if ( (lines == 1) && (len == 1) ) { /* return */
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark before, on current : lines=len=1 (return)\n" );
#endif /* DEBUG */
	    return 0;
	}
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark before, on current : lines > 0\n" );
#endif /* DEBUG */
	    return 1;
	}
    }
    else if ( lines == 0 ) {
	if ( ( len < 0 ) ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark before, on current : lines=0 len<0\n" );
#endif /* DEBUG */
	    text -> markpos += len;
	    return 0;
	}
	else if ( len > 0 ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark before, on current : lines=0 len>0\n" );
#endif /* DEBUG */
	    text -> markpos += len;
	    return 0;
	}
	else if ( len == 0 ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark before, on current : lines=len=0\n" );
#endif /* DEBUG */
	    ResetMark ( text );
	    return (0);
	}
	else if ( (len == 1) || (len == -1) ) { /*1 caratere insert/del*/
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark before, on current : lines=0 len=+-1\n" );
#endif /* DEBUG */
	    text -> markpos += len;
	    return 0;
	}
    }
    else { /* lines < 0 */
	if ( (lines == -1) && (len == -1)) { /*delete return */
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark before, on current : lines=len-1\n" );
#endif /* DEBUG */
	    text -> markline += lines;
	    return 1;
	}
	if ( len == 0 ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark before, on current : lines<0 len=0\n" );
#endif /* DEBUG */
	    ResetMark ( text );
	    return (0);
	}
    }
    return 0;
}

/*
**	Function name : AfterMarkOnCurrent
**
**	Description : Traitement du cas ou le curseur est sur la
**		meme ligne que la marque mais apres celle-ci.
**
**	Input : Le text courant, la longueur de la chaine ajoutee
**		ou enlevee et le nombre de lignes.
**	Ouput : 1 si la position de la marque sur sa ligne doit etre
**		recalcule, 0 sinon.
*/
static void AfterMarkOnCurrent ( text, len, lines )
    Text *text;
    int len, lines;
{
    
    if ( len >= 1 ) {
	return;
    }
    if ( len < 0 ) {  /* On efface */
	if ( lines == 0 ) {
	    if ((GetNcFromLeft ( text -> buf ) +len) <= text -> markpos) {
#ifdef DEBUG
		(void) fprintf ( stderr, "After mark on current : len < 0, lines = 0\n" ); 
#endif /* DEBUG */
		ResetMark ( text );
	    }
	    return;
	}
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr, "After mark on current : len < 0, lines > 0\n" ); 
#endif /* DEBUG */
	    ResetMark ( text );
	}
    }
}

/*
**	Function name : OnMark
**
**	Description : Traitement du cas ou le curseur est sur la marque.
**
**	Input : Le text courant, la longueur de la chaine ajoutee
**		ou enlevee et le nombre de lignes.
**	Ouput : 1 si la position de la marque sur sa ligne doit etre
**		recalcule, 0 sinon.
*/
static int OnMark ( text, len, lines )
    Text *text;
    int len, lines;
{
    /* On est sur la marque */
    if ( lines == 0 ) {
	if ( len == 0 ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark on current : lines = 0 len = 0\n" );
#endif /* DEBUG */
	    ResetMark ( text );
	}
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark on current : lines = 0 len != 0\n" );
#endif /* DEBUG */
	    text -> markpos += len;
	}
	return 0;
    }
    else if ( lines < 0 ) { /* On enleve plusieurs lignes */
	if ( (lines == -1) && ( len == -1)) { /* delete return */
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark on current : lines -1 len -1\n" );
#endif /* DEBUG */
	    text -> markline --;
	    return 1;
	}
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark on current : lines < 0\n" );
#endif /* DEBUG */
	    ResetMark ( text );
	    return 0;
	}
    }
    else {	/* On ajoute plusieurs lignes */
	if ( (lines == 1) && ( len == 1)) {	/* return */
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark on current : lines 1 len 1\n" );
#endif /* DEBUG */
	    text -> markline ++;
	    text -> markpos = 0;
	    return 0;
	}
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr, "Mark on current : lines > 0\n" );
#endif /* DEBUG */
	    text -> markline += lines;
	    text -> markpos = 0;
	    return 1;
	}
    }
}

/*
**	Function name : UpdateMarkPos
**
**	Description : Recalcul de la position de la marque
**		sur la ligne courante.
**
**	Input : Le text courant, la longueur de la chaine ajoutee
**		ou enlevee et le nombre de lignes.
**	Ouput : 1 si la position de la marque sur sa ligne doit etre
**		recalcule, 0 sinon.
*/
void UpdateMarkPos ( text )
    Text *text;
{
    text -> markpos += GetNcFromLeft ( text -> buf );
}

/*
**	Function name : SetMark
**
**	Description : Marque la position courante.
**
**	Input : Le text courant.
**	Ouput :
*/
void SetMark ( text )
    Text *text;
{
    text -> markline = text -> no_current_line;
    text -> markpos = GetNcFromLeft ( text -> buf );
    DisplayMessage ( text -> mwin, "Set Mark" );
}


/*
**	Function name : ExchangePointMark, 
**
**	Description : Va a la marque.
**
**	Input : Le text courant.
**	Ouput :
*/
void GotoTheMark ( text)
    Text *text;
{
    if ( text -> markline == 0 )
      return;
    TextCursorOn ( text );
    
    if ( text -> markline != text -> no_current_line ) {
      StorePosition(text);
	GotoLineNumber ( text, text -> markline );
	if ( text -> markpos != 0 )
	  (void) MoveHole ( text -> buf, text -> markpos );
	SetPosition(text);
	UpdatePage(text);

	if ( text -> no_current_line != 1 )
	  (void) MoveScrollBar ( dpy, text -> swin, CURRENT,
				 text -> no_current_line - text -> n1 - 1 );
	else
	  (void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
    }
    else {
	(void) MoveHole ( text -> buf,  text -> markpos - GetNcFromLeft ( text -> buf ));
	SetCurrentLine ( text );
    }
    TextCursorOn ( text );
}

/*
    **	Function name : ExchangePointMark, 
    **
    **	Description : Va de la marque au curseur et inversement.
    **
    **	Input : Le text courant.
    **	Ouput :
    */
void ExchangePointMark ( text)
    Text *text;
{
    int pos, cline;
    
    if ( text -> markline == 0 )
      return;
    
    TextCursorOn ( text );
    /* On sauve la position courante */
    pos = GetNcFromLeft ( text -> buf );
    cline = text -> no_current_line;
    
    GotoTheMark ( text );
    
    text -> markline = cline;
    text -> markpos = pos;
}

/*
**	Function name : KillRegion
**
**	Description : Efface le texte, de la marque au cursor.
**
**	Input :  Le text courant.
**	Ouput :
*/
void KillRegion ( text )
    Text *text;
{
    if ( text -> markline == 0 ) {
	DisplayMessage ( text -> mwin, "No mark" );
	return;
    }
    if ( text -> no_current_line == text -> markline ) 
      (void) SmallRegion ( text, KILLREGION );
    else 
      (void) BigRegion ( text, KILLREGION );
    TextCursorOn ( text );
}

/*
**	Function name : CopyRegion
**
**	Description : Copie de la marque au curseur.
**
**	Input : Le text courant.
**	Ouput :
*/
void CopyRegion ( text )
    Text *text;
{
    if ( text -> markline == 0 ) {
	DisplayMessage ( text -> mwin, "No mark" );
	return;
    }
    
    if ( text -> no_current_line == text -> markline ) 
      (void) SmallRegion ( text, COPYREGION );
    else 
      (void) BigRegion ( text, COPYREGION );
    TextCursorOn ( text );
}

/*
**	Function name : SmallRegion
**
**	Description : Copie ou efface une region contenue
**		dans la ligne courante.
**
**	Input : Le text courant, l'operation a effectuer.
**	Ouput :
*/
static void SmallRegion ( text, op )
    Text *text;
    int op;
{
    int diff;
    char buf [32];
    
    /*
	* La marque et le curseur sont sur la meme ligne
	*/
    TextCursorOff ( text );
    diff = text -> markpos - GetNcFromLeft ( text -> buf );

    if ( diff == 0 ) {
	TextCursorOn ( text );
	return;
    }
    else if ( diff > 0 ) {
	/* curseur avant la marque */
	StoreInKillBuf ( RightBuf ( text -> buf ) , diff, 0 ); 
	if ( op == KILLREGION ) {
	    StoreInUndo ( text, RightBuf ( text -> buf ), (char *) 0, -diff , 0, U_STD );
    
	    /* Pour la couleur */
	    if ( text -> current_ce != 0 )
	      (void) UpdateColorList ( text, -diff );
	
	    (void) MoveHole ( text -> buf, diff );
	    (void) UpdateMark ( text, -diff, 0 );
	    DeleteNchar ( text -> buf, diff );
	}
	else {
	    (void) sprintf ( buf, "%d bytes copied", diff );
	    DisplayMessage ( text -> mwin, buf );
	}
    }
    else { /* diff < 0 */
	/* curseur apres la marque */
	StoreInKillBuf ( LeftBuf ( text -> buf ) + diff + 1, - diff, 0 ); 
	if ( op == KILLREGION ) {
	    (void) MoveHole ( text -> buf, diff ); 
	    StoreInUndo ( text, RightBuf ( text -> buf ) , (char *) 0, diff , 0, U_STD );
    
	    /* Pour la couleur */
	    if ( text -> current_ce != 0 )
	      (void) UpdateColorList ( text, diff );
	
	    (void) MoveHole ( text -> buf, -diff );
	    DeleteNchar ( text -> buf, -diff );
	    ResetMark ( text );
	}
	else {
	    (void) sprintf ( buf, "%d bytes copied", -diff );
	    DisplayMessage ( text -> mwin, buf );
	}
    }
    SetCurrentLine ( text );
    if ( op == KILLREGION )
      SetTextModif ( text);
}


/*
**	Function name : BigRegion
**
**	Description : Copie ou efface une region de plusieurs lignes.
**
**	Input : Le text courant, l'operation a effectuer.
**	Ouput :
*/
static void BigRegion ( text, op )
    Text *text;
    int op;
{
    int diff, lines;
    int len;
    char buf [32];
    char *p;
    
    if ( (lines = text -> no_current_line - text -> markline) > 0 ) 
      (void) GetBackwardLine ( text -> buf, -lines, &len );
    else
      (void) GetForwardLine ( text -> buf,  -lines, &len );
    
    if ( (diff = len + text -> markpos) < 0 )
      diff *= -1;
    
    if ( lines > 0 ) { /* Curseur apres la marque */
	  StoreInKillBuf ( LeftBuf ( text -> buf ) - diff + 1, diff, lines );
    }
    else {
	StoreInKillBuf ( RightBuf ( text -> buf ) , diff, -lines );
    }
    
    if ( op == KILLREGION ) {
	StorePosition(text);
	if ( lines > 0 ) { /* Curseur apres la marque */ 
	    (void) MoveHole ( text -> buf, -diff ); /* On va a la marque */
	    p = RightBuf ( text -> buf );			
	    (void) MoveHole ( text -> buf, diff ); /* on revient au curseur */
	    ResetMark ( text );
	}
	else { /* curseur avant la marque */
	    p = RightBuf ( text -> buf );
	    (void) MoveHole ( text -> buf, diff );
	    (void) UpdateMark ( text, -diff, lines );
	}
	DeleteNchar ( text -> buf, diff );
	if ( lines > 0 )
	  text -> no_current_line -= lines;
	text -> lines_in_buf = GetNumberOfLineInBuf ( text -> buf ); 
	SetScrollLine ( text -> swin , text -> lines_in_buf );
	SetScrollBarSize ( dpy, text -> swin );
/*	CurrentLineToMiddle ( text );  */
	SetPosition(text);
	UpdatePage(text);
    }
    else {
	(void) sprintf ( buf, "%d bytes copied", diff );
	DisplayMessage ( text -> mwin, buf );
    }
    TextCursorOff ( text );

    text -> markpos = 0;
    UpdateMarkPos ( text );
    
    if ( op == KILLREGION ) {
	StoreInUndo ( text, p, (char *) 0, -diff , (lines > 0) ? lines  : -lines, U_STD );
    
	/* Pour la couleur */
	if ( text -> current_ce != 0 ) {
	  (void) UpdateColorList ( text, -diff );
	  RefreshPage(text);
	}
	
	SetTextModif ( text);
    }
}


/*
**	Function name : PasteRegion
**
**	Description : Restore une region a partir de la position
**
**	Input : Le text courant.
**	Ouput :
*/
void PasteRegion ( text )
    Text *text;
{
    Control_Y ( text, 0 );	
}

/*
**	Function name : ResetMark
**
**	Description : Mis a zero de la marque.
**
**	Input : Le text courant.
**	Ouput :
*/
void ResetMark ( text )
    Text *text;
{
    text -> markline = text -> markpos = 0;
#ifdef DEBUG	
    (void) fprintf ( stderr, "Reset mark\n" );
#endif /* DEBUG */
}


/*
**	Function name : SaveMark
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SaveMark ( text )
    Text *text;
{
    ml = text -> markline;
    mp = text -> markpos;	
}


/*
**	Function name : RestoreMark
**
**	Description : 
**	Input : 
**	Ouput :
*/
void RestoreMark ( text )
    Text *text;
{
    text -> markline = ml;
    text -> markpos = mp;		
}


/*
**	Function name : IndentRegion
**
**	Description : 
**	Input : 
**	Ouput :
*/
void IndentRegion ( text )
    Text *text;
{
  char *msg;
  int nw = text->win_id;

  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "indent_region", 0, 0) ;
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
/*  SetPosition ( text ); */
  ie_redisplay ( text );
}      

/*
**	Function name : EvalRegion
**
**	Description : 
**	Input : 
**	Ouput :
*/
void EvalRegion ( text )
    Text *text;
{
    char *msg;
    int nw = text->win_id;
    
    StorePosition ( text );
    msg = (char *) ie_eval_region (text) ;
    text = update_cwd(nw);
    
    if ( msg != 0 )
      DisplayMessage ( text -> mwin, msg );
/*    SetPosition ( text ); */
    ie_redisplay ( text );
}      


/*
**	Function name : EvalExpressionFromKey
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void eval_expression ( text, str )
    Text *text;
    char *str;
{
    char *msg;
    int nw = text->win_id;
    
    if ( str == 0 )
      return;
    msg = (char *) ie_eval_expression ( text, str );
    text = update_cwd(nw);
    
    if ( msg != 0 )  {
	DisplayMessage ( text -> mwin, msg );
    }
    ie_redisplay ( text );
}


/*
**	Function name : EvalExpressionFromKey
**
**	Description : 
**	Input : 
**	Ouput :
*/
void EvalExpressionFromKey ( text )
    Text *text;
{
    char *str;
    char c = '\007'; /* ^G */

    StorePosition ( text );
    SetMBcontext ( CTX_EVAL_EXP );
    str = (char *) GetString ( text, "Eval exp : ", 0 );
    SetMBcontext ( CTX_FILE );
    
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
      DisplayMessage ( text -> mwin, "Abort" );
      return;
    }
    
    (void) eval_expression ( text, str );
}

/*
**	Function name : EvalExpressionFromMenu
**
**	Description : 
**	Input : 
**	Ouput :
*/
void EvalExpressionFromMenu ( text )
    Text *text;
{
    char *str;
    char c = '\007'; /* ^G */
    
    StorePosition ( text );
    SetMBcontext ( CTX_EVAL_EXP );
    str = (char *) GetStringFromDB ( "Eval expression : ", False );
    SetMBcontext ( CTX_FILE );
  
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
      DisplayMessage ( text -> mwin, "Abort" );
      return;
    }
    
    (void) eval_expression ( text, str );
}

/*
**	Function name : ColorRegion
**
**	Description :
**	Input :
**	Output :
*/
void ColorRegion(text)
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "color_region", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}

/*
**	Function name : ColorBuffer
**
**	Description :
**	Input :
**	Output :
*/
void ColorBuffer(text)
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "color_buffer", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}

