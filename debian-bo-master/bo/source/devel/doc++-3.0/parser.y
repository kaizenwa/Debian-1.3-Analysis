/*************************************************************************

    DOC++, a C++ (and C) documentation system for LaTeX and HTML

	    Copyright (C) 1996  Roland Wunderling,
				Malte Zoeckler


    DOC++ is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation. This program
    is distributed WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public License for more details.

    If you intend to use DOC++ commercially on a regular basis you
    must contact the authors for an appropriate donation.

 *************************************************************************/


%{
/*
 *	includes
 */
#include <stdio.h>
#include <iostream.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "doc.h"
int yyerror(const char *s);

void
dumpType( Entry* current )
{
    cerr << " --- type became `" << (const char*)current->type << "'\n" ;
}

/*
 *	imports
 */
extern int	yyLastLine ;
extern int	yyLineNr ;
extern char	yyFileName[] ;
extern McString	yyText ;

extern int	yylex();
extern void	restart() ;
extern void	done() ;
extern void	parseMessage( const char* str, const char* str2 ) ;
extern void	strip( McString& dest,const char* source ) ;

/*
 *	defines
 */
#define isalpha(ch)	(( ch=='_' || ch=='~' || ch=='[' || ch==']' || \
			   ('a'<=ch && ch<='z') || ('A'<=ch && ch<='Z') ))
#define isalphanum(ch)	(( isalpha(ch) || ('0'<=ch && ch<='9') ))


/*
 *	statics
 */
static int	protection ;	/* current protection status */

/*
static C_String*	name ;
static C_String*	memo ;
static C_String*	doc ;
static C_String*	program ;
*/

extern Entry*	current ;

%}
%token	CHAR

%token	SEPARATOR
%token	STRUCT
%token	UNION
%token	ENUM

%token	';'
%token	CODE
%token	NAME
%token	CONST
%token	CLASS
%token	MACRO
%token	ROUND
%token	CURLY

%%
Start:		
	    |	Start GetEntry
	    |	Start error	{ fprintf( stderr,
					   "ERROR before line %d of file %s: `%s'\n",
					   yyLineNr, yyFileName,
					   (const char*)yyText ) ;
				  done() ;
				}
	    ;

GetEntry:	FillType GetIt	{ done() ; }
	    ;

FillType:	
	    |	FillType CONST	{ current->name += yyText ; }
	    |	FillType CODE	{ current->name += yyText ; }
	    |	FillType NAME	{ current->type += current->name ;
				  current->name = yyText ;
	    			}
	    ;

GetIt:		Function	{ current->section = FUNCTION_SEC ;
				  parseMessage( "found function:  ", current->name ) ;
	    			}
	    |	Variable	{ current->section = VARIABLE_SEC ;
				  parseMessage( "found variable:  ", current->name ) ;
	    			}
	    |	Struct		{ current->section = CLASS_SEC ;
				  parseMessage( "found class:     ", current->name ) ;
	    			}
	    |	Class		{ current->section = CLASS_SEC ;
				  parseMessage( "found class:     ", current->name ) ;
	    			}
	    |	Enum		{ current->section = UNION_SEC ;
				  parseMessage( "found union:     ", current->name ) ;
	    			}
	    |	Union		{ current->section = UNION_SEC ;
				  parseMessage( "found union:     ", current->name ) ;
	    			}
	    |	Macro		{ current->section = MACRO_SEC ;
				  done() ;
				  parseMessage( "found macro:     ", current->name ) ;
	    			}
	    ;

Function:	ROUND		{ current->program = yyText ; }
		OptStuff
		FuncEnd
	    ;

OptStuff:
	    |	CODE
	    |	CONST
	    ;

FuncEnd:	';'
	    |	SEPARATOR
	    |	CURLY
	    ;

Struct:		STRUCT NAME	{ current->type += current->name ;
				  current->type += " class" ;
				  current->name  = yyText ; }
		OptBases
		CURLY		{ current->program = yyText ; }
	    ;

Class:		CLASS NAME	{ current->type += current->name ;
				  current->type += " class" ;
				  current->name  = yyText ; }
		OptBases
		CURLY		{ current->program = yyText ; }
	    ;

OptBases:
	    |	OptBases CODE		{ current->args += yyText ; }
	    |	OptBases SEPARATOR	{ current->args += yyText ; }
	    |	OptBases NAME		{ current->args += yyText ; }
	    ;

Macro:		MACRO NAME	{ current->type = "\\#define" ;
				  current->name = yyText ; }
		OptArgs
	    ;

OptArgs:
	    |	ROUND		{ current->program = yyText ; }
	    ;

Variable:	';'
	    |	SEPARATOR
	    ;

Enum:		ENUM NAME	{ current->type += current->name ;
				  current->type += " enum" ;
				  current->name  = yyText ; }
		OptBases
		CURLY		{ current->program = yyText ; }
	    ;

Union:		UNION NAME	{ current->type += current->name ;
				  current->type += " enum" ;
				  current->name  = yyText ; }
		OptBases
		CURLY		{ current->program = yyText ; }
	    ;

%%
#ifndef	NDEBUG
int	yydebug() { return 1 ;}
#endif	//NDEBUG

int	yyerror(const char *) { return 1 ;}
