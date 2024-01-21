/* ########################################################################

				rename.h

   File: rename.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/rename.h
   Description: 
   Created: Tue Feb 21 12:59:26 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:59:27 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

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



#ifndef _rename_h
#define _rename_h


/* Pour renomer les symbols communs sans devoir modifier les sources */

#define YYDEBUG_DISCARD_STATE i_YYDEBUG_DISCARD_STATE
#define YYDEBUG_DISCARD_TOKEN i_YYDEBUG_DISCARD_TOKEN
#define YYDEBUG_INDENT i_YYDEBUG_INDENT
#define YYDEBUG_LOOK_AHEAD i_YYDEBUG_LOOK_AHEAD
#define YYDEBUG_REDUCE i_YYDEBUG_REDUCE
#define YYDEBUG_SHIFT_ERROR_LEXEME i_YYDEBUG_SHIFT_ERROR_LEXEME
#define YYDEBUG_SHIFT_LEXEME i_YYDEBUG_SHIFT_LEXEME
#define numlig i_numlig
#define yychar i_yychar
#define yycheck i_yycheck
#define yydebug i_yydebug
#define yydefred i_yydefred
#define yydgoto i_yydgoto
#define yyerrflag i_yyerrflag
#define yyerror i_yyerror
#define yygindex i_yygindex
#define yylen i_yylen
#define yylhs i_yylhs
#define yylval i_yylval
#define yyname i_yyname
#define yynerrs i_yynerrs
#define yyparse i_yyparse
#define yyrindex i_yyrindex
#define yyrule i_yyrule
#define yysccsid i_yysccsid
#define yysindex i_yysindex
#define yyss i_yyss
#define yyssp i_yyssp
#define yytable i_yytable
#define yyval i_yyval
#define yyvs i_yyvs
#define yyvsp i_yyvsp
#define key_err i_key_err
#define ncform_sccsid i_ncform_sccsid
#define yyback i_yyback
#define yybgin i_yybgin
#define yycrank i_yycrank
#define yyestate i_yyestate
#define yyextra i_yyextra
#define yyfnd i_yyfnd
#define yyin i_yyin
#define yyinput i_yyinput
#define yyleng i_yyleng
#define yylex i_yylex
#define yylineno i_yylineno
#define yylook i_yylook
#define yylsp i_yylsp
#define yylstate i_yylstate
#define yymatch i_yymatch
#define yymorfg i_yymorfg
#define yyolsp i_yyolsp
#define yyout i_yyout
#define yyoutput i_yyoutput
#define yyprevious i_yyprevious
#define yysbuf i_yysbuf
#define yysptr i_yysptr
#define yysvec i_yysvec
#define yytchar i_yytchar
#define yytext i_yytext
#define yytop i_yytop
#define yyunput i_yyunput
#define yyvstop i_yyvstop
#define yyact i_yyact
#define yychk i_yychk
#define yydef i_yydef
#define yyexca i_yyexca
#define yypact i_yypact
#define yypgo i_yypgo
#define yyps i_yyps
#define yypv i_yypv
#define yyr1 i_yyr1
#define yyr2 i_yyr2
#define yyreds i_yyreds
#define yys i_yys
#define yystate i_yystate
#define yytmp i_yytmp
#define yytoks i_yytoks
#define yyv i_yyv

#define yy_init_buffer i_yy_init_buffer
#define yy_switch_to_buffer i_yy_switch_to_buffer
#define yy_delete_buffer i_yy_delete_buffer
#define yy_create_buffer i_yy_create_buffer
#define yyrestart i_yyrestart
#define yy_load_buffer_state i_yy_load_buffer_state

#endif
