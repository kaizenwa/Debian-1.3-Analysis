%{
/**
 *
 * $Id: yacc.y,v 1.6 1996/11/12 03:53:59 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/ 
#include <stdio.h>
#include <stdarg.h>
#include "uil.h"	
#include "glue.h"

#define YYSTYPE char *
static int False = 0;
static int True = 1;
extern char *yytext;
extern int LineNumber;
extern char *FileName;

extern int yylex();

%}

%token STRING
%token ID
%token VERSION
%token NAMES
%token MODULE
%token VALUE
%token INTEGER
%token FLOAT
%token STRING_TABLE
%token INTEGER_TYPE
%token FLOAT_TYPE
%token STRING_TYPE
%token ANY_TYPE
%token BOOLEAN_TYPE
%token PROCEDURE
%token PROCEDURES
%token IMPORTED
%token CONTROLS
%token ARGUMENT
%token ARGUMENTS
%token OBJECT
%token CALLBACK
%token END
%token EXPORTED
%token OBJECTS
%token CHAR_SET
%token WIDGET
%token INC_FILE
%token LIST
%token UNMANAGED
%token KEYSYM
%token ICON
%token COMPOUND_STRING
%token SEPARATE
%token BOOL
%token GADGET
%token PRIVATE
%token REASON
%token USER_DEFINED
%token COLOR
%token COLOR_TABLE
%token XBITMAPFILE
%token XPIXMAPFILE
%token FONT
%token FONT_TABLE
%token FONT_UNIT
%token BACKGROUND_COLOR
%token FOREGROUND_COLOR

%%

input: MODULE ID module body END MODULE ';'
;

body: body VALUE initializations
		{Warn(LOC,"NO OP\n");
	       $$ = $1;
	       }
	| body PROCEDURE procedure_list
		{Warn(LOC,"NO OP\n");
	       $$ = $1;
	       }
	| body LIST list
		{Warn(LOC,"NO OP\n");}
	| body OBJECT object
		{
		  	$$ = body_OBJECT_object($1,$3);

		}
	|
		{
			$$ = NULL;
		}

;

module: VERSION '=' STRING module
		{Warn(LOC,"NO OP\n");}
	| NAMES '=' ID module
		{Warn(LOC,"NO OP\n");
	       $$ = $1;
	       }
	| CHAR_SET '=' ID module
		{Warn(LOC,"NO OP\n");}
	| OBJECTS '=' '{' object_list '}'
		{Warn(LOC,"NO OP\n");}
	|
		{Warn(LOC,"NO OP\n");
	       $$ = NULL;
	       }
;

object_list: ID '=' WIDGET ';' object_list
		{Warn(LOC,"NO OP\n");}
	| ID '=' GADGET ';' object_list
		{Warn(LOC,"NO OP\n");}
	|
		{Warn(LOC,"NO OP\n");}
;

initializations: ID ':' add_expr ';' initializations 
			{
				MakeTable($1,$3, 0);
			}
		| ID ':'  STRING_TABLE '(' string_list ')' ';' initializations 
			{
				MakeTable($1,$5,0);
			}
		| ID ':'  COLOR_TABLE '(' color_list ')' ';' initializations 
			{
				MakeTable($1, $5, 0);
			}
		| ID ':' PRIVATE ARGUMENT '(' STRING ',' type ')' ';' initializations
			{Warn(LOC,"NO OP\n");}
		| ID ':'  REASON '(' STRING ')' ';' initializations
			{Warn(LOC,"NO OP\n");}
		| ID ':' EXPORTED add_expr ';' initializations
			{
				MakeTable($1, $4, 1);
			}
		| ID ':' IMPORTED type ';' initializations
			/* {Warn(LOC,"NO OP\n");} */
		|
			{
				$$ = NULL;
		       	}
;

color_list: ID '=' STRING ',' color_list
			{
				$$ = AddColor($1,$3, $5, 1);
			}
	| ID '=' STRING 
			{
				$$ = AddColor($1,$3, NULL, 1);
			}			
	| BACKGROUND_COLOR '=' STRING 
			{
				$$ = AddColor("none",$3, NULL, 0);
			}
	| BACKGROUND_COLOR '=' STRING ',' color_list
			{
			  	$$ = AddColor("none", $3, $5, 0);
			}
	| FOREGROUND_COLOR '=' STRING 
			{  /* 'black' is a hack */
			  	$$ = AddColor("black", $3, NULL, 0);
			}
	| FOREGROUND_COLOR '=' STRING ',' color_list
			{  /* 'black' is a hack */
			  	$$ = AddColor("black", $3, $5, 0);
			}
;

string_list: string_list ',' STRING
			{
				$$ = InsertString($1, $3);
			}
	| string_list ',' ID
			{
				$$ = InsertString($1, $3);
			}
	| STRING
			{
			 	$$ = InsertString(NULL,$1);
		       	}
	| ID
			{
				$$ = InsertString(NULL, $1);
			}
;

procedure_list: procedure_list ID '(' type_list ')' ';'
			{Warn(LOC,"NO OP\n");}
		| procedure_list ID '(' ')' ';'
			{Warn(LOC,"NO OP\n");
		       $$ = $1;
		       }
		| ID '(' type_list ')' ';'
			{Warn(LOC,"NO OP\n");}
		| ID '(' ')' ';'
			{Warn(LOC,"NO OP\n");
		       $$ = $1;
		       }
;

type_list: type_list ',' type
			{Warn(LOC,"NO OP\n");}
	| type
			{Warn(LOC,"NO OP\n");}
;

type: INTEGER_TYPE 
			{Warn(LOC,"NO OP\n");}
	| FLOAT_TYPE 
			{Warn(LOC,"NO OP\n");}
	| ANY_TYPE 
			{Warn(LOC,"NO OP\n");}
	| STRING_TYPE 
			{Warn(LOC,"NO OP\n");}
	| BOOLEAN_TYPE 
			{Warn(LOC,"NO OP\n");}
	| COMPOUND_STRING 
			{Warn(LOC,"NO OP\n");}
	| COLOR 
			{Warn(LOC,"NO OP\n");}
	| FONT_TABLE
			{Warn(LOC,"NO OP\n");}
;

list: list ID ':' ARGUMENTS '{' list_arg '}' ';' 
			{
				AddAttributeList($2, $6);
				$$ = NULL;
			}
	| list ID ':' CONTROLS '{' control_list '}' ';' 
			{
				AddControlList($2, $6);
				$$ = NULL;
			}
	| list ID ':' CALLBACK '{' callback_list '}' ';' 
			{
				AddCallbackList($2, $6);
				$$ = NULL;
			}
		|
			{
				$$ = NULL;
			}

list_arg: list_arg ID '=' add_expr ';' 
		{
			$$ = (char *)arglist_arglist_ID_addexpr($1, $2, $4);
		}
	|
		{
			$$ = NULL;
		}
;

object: ID ':' IMPORTED ID ';'
			{Warn(LOC,"NO OP\n");}
	| ID ':' ID '{' features '}' ';'
		{
			ID_ID_features($1,$3,$5);
			$$ = $5;
		}
	| ID ':' ID WIDGET '{' features '}' ';'
			{Warn(LOC,"NO OP\n");}
	| ID ':' EXPORTED ID '{' features '}' ';'
			{Warn(LOC,"NO OP\n");}
	| ID ':' USER_DEFINED PROCEDURE ID '{' features '}' ';'
			{Warn(LOC,"NO OP\n");}
;

features: features controls
		{
			features_controls($1,$2);
			$$ = $1;
		}
	|  features arguments
		{
			features_arguments($1,$2);
			$$ = $1;
		}
	|  features callbacks 
		{
			features_callbacks($1,$2);
			$$ = $1;
		}
	| 
		{
			$$ = (char *)Features_NULL();
		}
;

controls: CONTROLS '{' control_list '}' ';'
		{
			$$ = $3;
		}
	| CONTROLS ID ';'
		{
			Warn(LOC,"NO OP\n");
			$$ = NULL;
		}
;

control_list: control_list ID ID ';'  
			{
				$$ = (char *)
				  controllist_controllist_ID_ID($1,$2,$3,1);
			}
		| control_list ID '{' features '}' ';'
			{
				$$ = (char *)
				  control_list_ID_features($1, $2, $4, 1);
			}
		| control_list ID UNMANAGED '{' features '}' ';'
			{   /* $2 may be wrong for this type of call */
			    /* I confused this rule for the next one */
			  	$$ = (char *)
				  control_list_ID_features($1, $2, $5, 0);
			}					
		| control_list UNMANAGED ID '{' features '}' ';' 
			{
				$$ = (char *)
					control_list_ID_features($1,$3, $5, 0);
			}
		| control_list ID ':' ID '{' features '}' ';' 
			{Warn(LOC,"NO OP\n");}
		| control_list UNMANAGED ID ID ';' 
			{
				$$ = (char *)
				  controllist_controllist_ID_ID($1,$3,$4,0);
			}
		| control_list CONTROLS ID ';' 
			{
				$$ = InheritControls($1, $3);
			}
		| control_list USER_DEFINED ID ';' 
			{Warn(LOC,"NO OP\n");}
		| 
			{
				$$ = NULL;
			}
;

arguments: ARGUMENTS '{' argument_list '}' ';'
			{
				$$ = $3;
			}
;

argument_list: argument_list ID '=' add_expr ';' 
		{
			$$ = arglist_arglist_ID_addexpr($1,$2,$4);
		}
	|	argument_list ID '=' ID ID ';'  /* another widget */
		{
		       	$$ = WidgetArgument($1, $2, $5);
	     	}
	|	argument_list ARGUMENTS ID ';'
		{
			$$ = InheritArgument($1, $3);
		}
	|
		{
			$$ = NULL;
		}
;

add_expr: mult_expr
			{ $$ = $1; }
	| add_expr '-' mult_expr
			{
				$$ = Subtract($1,$3);
			}
	| add_expr '+' mult_expr
			{
				$$ = Add($1,$3);
			}
	| add_expr '&' prim_expr
			{
				$$ = AppendStrings($1, $3);
			}
;

mult_expr: prim_expr
			{ $$ = $1; }
	| mult_expr '*' prim_expr
			{
				$$ = Multiply($1,$3);
			}
	| mult_expr '/' prim_expr
			{
				$$ = Divide($1,$3);
			}
;

prim_expr: ID
		{
			$$ = (char *) expr_ID($1);
		}
	| INTEGER FONT_UNIT
		{
			Warn(LOC, "Font Unit not implemented yet\n");
			$$ = (char *) prim_exp($1);
		}
	| INTEGER
		{
			$$ = (char *)prim_exp($1);
		}
	| STRING
		{
			$$ = (char *) expr_STRING($1, /*Compound?*/ False);
		}
	| BOOL
		{
			$$ = (char *) expr_BOOL($1);
		}
	| KEYSYM '(' ID ')'
		{
		        $$ = (char *) keysym($3);
		}
	| FONT '(' STRING ')'
		{
			$$ = (char *) font($3);
		}
	| KEYSYM '(' STRING ')'
		{
			$$ = (char *) keysym($3);
		}
	| XPIXMAPFILE '(' STRING ')'
			{Warn(LOC,"NO OP\n");
			$$ = NULL;}
	| XBITMAPFILE '(' STRING ')'
		{	
			$$ = (char *) bitmap($3);
		}
	| ICON '(' COLOR_TABLE '=' ID ',' string_list ')'
		{
			$$ = (char *) pixmap($5, $7);
		}
	| ICON '(' string_list ')'
		{
			$$ = pixmap(NULL,$3);
		}
	| COLOR '(' STRING ')'
		{
			$$ = color($3);
		}
	| COLOR '(' STRING ',' ID ')'
		{
			
			$$ = color($3); /* For Now  FIX ME */
		}
	| compound_string
		{
			$$ = (char *) expr_STRING($1, /*Compound?*/ True);
		}
	| '(' add_expr ')'
		{	
			$$ = $2;
		}
;

compound_string: COMPOUND_STRING '(' STRING ',' SEPARATE '=' BOOL ')'
			{
			  string_clear();
			  string_push($3);
			  $$ = string_push("S");
			}
		|	COMPOUND_STRING '(' STRING ')'
			{
			  string_clear();
			  $$ = string_push($3);
			}
;

callbacks: CALLBACK '{' callback_list '}' ';'
		{
			$$ = $3;
		}
	| CALLBACK ID ';'
                {
			$$ = InheritCallback($2);
		}
;

callback_list: callback_list ID '=' PROCEDURE ID '(' proc_argument_list ')' ';'
		{
			$$ = (char *) callbacklist_callbacklist_PROCID_arglist(
				$1,$2,$5,$7);
		}
	| callback_list ID '=' PROCEDURES '{' procedure_list '}' ';'
			{Warn(LOC,"NO OP************\n");}
	| 
		{
			$$ = NULL;
		}
;

proc_argument_list: proc_argument_list ',' INTEGER
			{
			  $$ = Parameter($1, prim_exp($3));
			}
	| proc_argument_list ',' ID
			{
			  $$ = Parameter($1, expr_ID($3));
			}
	| proc_argument_list ',' STRING
			{
			  $$ = Parameter($1, expr_STRING($3, 0));
			}
	| INTEGER
			{
			  $$ = Parameter(NULL, prim_exp($1));
			}
	| ID
			{
			  $$ = Parameter(NULL, expr_ID($1));
			}
	| STRING
			{
			  $$ = Parameter(NULL, expr_STRING($1, 0));
			}
	| compound_string
			{
			  $$ = Parameter(NULL, expr_STRING($1, 1));
			}
	|	
		{
			$$ = Parameter(NULL, NULL);
		}
;


%%


extern optind;
extern char *optarg;

void yyerror(s)
char *s;
{
  fprintf(stderr,"%s:%d: %s\n",FileName,LineNumber,s);
  fprintf(stderr," current token \'%s\'\n",yytext);
}

void
yywrap()
{
}

void yyExit(int line, char *file, char *fmt, ...)
{
  va_list ap;

  yyerror("");
  va_start (ap, fmt);
  fprintf(stderr,"%s:%d:",file,line);
  vfprintf(stderr,fmt,ap);
  va_end(ap);
  exit(1);
}
