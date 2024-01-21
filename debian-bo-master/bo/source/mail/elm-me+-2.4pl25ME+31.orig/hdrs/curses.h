
/* $Id: curses.h,v 5.1 1992/10/03 22:34:39 syd Exp $ */

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.1 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 ****************************************************************************/

#define OFF		0
#define ON 		1

#undef P_
#ifdef __STDC__
#define P_(x) x
#else
#define P_(x) ()
#endif

extern int InitScreen P_(()),      /* This must be called before anything else!! */
  ClearScreen P_((void)), 	 
  CleartoEOLN P_((void)),
  MoveCursor  P_((int, int)),
  StartBold P_((void)),        
  EndBold P_((void)), 
  StartUnderline P_((void)),   
  EndUnderline   P_((void)),
  StartHalfbright P_((void)),  
  EndHalfbright   P_((void)),
  StartInverse P_((void)),     
  EndInverse   P_((void)),
  transmit_functions P_((int)),
  Raw P_((int)),              
  RawState P_((void)),
  ReadCh P_((int));

char *return_value_of();

void ScreenSize P_((int *lines, int *columns));
