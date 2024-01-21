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

#include "McString.h"

/*
 *	exports
 */
McString	yyText ;
FILE*		out ;
int		docified = 0 ;
int		braCnt   = 0 ;

extern "C" {
int	yywrap()	{ return 1 ; }
};

void yyMore()	{ yyText += yytext ; }
void yyDump()
{
    fprintf( out, "%s", (const char*)yyText ) ;
    yyText.clear() ;
}
void	docify()
{
    if( !docified )
    {
	if( yyText.size() && yyText[0] == '/' )
	{
	    if( yyText[1] == '/' )
		fprintf( out, "///" ) ;
	    else
		fprintf( out, "/**" ) ;
	    yyText.remove( 0, 2 ) ;
	    yyDump() ;
	}
	else
	{
	    yyDump() ;
	    for( int i = 0 ; yytext[i] == ' ' || yytext[i] == '\n' ; i++ )
		fprintf( out, "%c", yytext[i] ) ;
	    fprintf( out, "///\n" ) ;
	}
    }
    yyMore() ;
    docified = 0 ;
}

/*
 *	defines
 */
#undef	ECHO
#define	ECHO

%}

%s	Start
%x	Comment
%x	CPP
%x	Code
%x	Bracket

%%
<Start>[ \t\n]				{ ECHO ; yyMore() ; }

<Start>"//*"[^*].*\n			{ ECHO ; docified=1 ; yyMore() ; }
<Start>"//*\n"				{ ECHO ; docified=1 ; yyMore() ; }
<Start>"///"[^/].*\n			{ ECHO ; docified=1 ; yyMore() ; }
<Start>"///\n"				{ ECHO ; docified=1 ; yyMore() ; }
<Start>"//@".*\n			{ ECHO ; docified=1 ; yyMore() ; }

<Start>"/**"[^*/]			{ ECHO ; docified=1 ; yyMore() ; BEGIN(Comment) ; }
<Start>"/*/"[^*/]			{ ECHO ; docified=1 ; yyMore() ; BEGIN(Comment) ; }
<Start>"/*@"				{ ECHO ; docified=1 ; yyMore() ; BEGIN(Comment) ; }

<Comment>.				{ ECHO ; yyMore() ; }
<Comment>\n				{ ECHO ; yyMore() ; }
<Comment>"*/"				{ ECHO ; yyMore() ; BEGIN( Start ) ; }

<Start>"//".*\n				{ ECHO ; yyDump() ; yyMore() ; }
<Start>"/*"				{ ECHO ; yyDump() ; yyMore() ; BEGIN(Comment) ; }

<Start>"#"				{ ECHO ; yyMore() ; BEGIN(CPP) ; }
<CPP>.					{ ECHO ; yyMore() ; }
<CPP>"\\\n"				{ ECHO ; yyMore() ; }
<CPP>[ \t]*                             { ECHO ; yyMore() ; }
<CPP>\n                                 { ECHO ;
                                          yyMore() ; yyDump() ;
					  docified=0 ;
					  BEGIN(Start) ;
					}

<Start>[ \t]*template[ \t\n]?[^{]*"{"	{ ECHO ; docify() ; }
<Start>[ \t]*"class".*";".*\n           { ECHO ; docified=1 ; yyMore() ; docified=0 ; }
<Start>[ \t]*"class"[ \t\n][^{]*"{"	{ ECHO ; docify() ; }
<Start>[ \t]*"struct"[ \t\n][^{]*"{"	{ ECHO ; docify() ; }
<Start>[ \t]*"public:"			{ ECHO ; yyMore() ; }
<Start>[ \t]*"protected:"		{ ECHO ; yyMore() ; }
<Start>[ \t]*"private:"			{ ECHO ; yyMore() ; }
<Start>[ \t]*[a-zA-Z_~]			{ ECHO ; docify() ; BEGIN( Code ) ; }

<Code>"{"				{ ECHO ; yyMore() ; BEGIN(Bracket) ; }
<Code>";"				{ ECHO ; yyMore() ; yyDump() ; BEGIN(Start) ; }
<Code>.					{ ECHO ; yyMore() ; }
<Code>\n				{ ECHO ; yyMore() ; }

<Bracket>"{"				{ ECHO ; yyMore() ; braCnt++ ; }
<Bracket>"}"				{ ECHO ; yyMore() ;
					  if( braCnt )
					    braCnt-- ;
					  else
					  {
					    yyDump() ;
					    BEGIN( Start ) ;
					  }
					}
<Bracket>\n				{ ECHO ; yyMore() ; }
<Bracket>.				{ ECHO ; yyMore() ; }

<Start>.				{ ECHO ; yyMore() ; }
%%
// -----------------------------------------------------------------
main( int argc, char** argv )
{
    FILE*	ain ;
    FILE*	aout ;
    yyin = stdin ;
    out  = stdout ;

    if( argc > 1 )
    {
	if( argv[1][0] == '-' )
	{
	    cerr << "usage: " << argv[0] << " [infile [outfile]]" << endl ;
	    return 0 ;
	}
	ain = fopen( argv[1], "r" ) ;
	if( ain )
	    yyin = ain ;
	else
	{
	    cerr << "Could not open file `" << argv[1] << "' for reading\n" ;
	    cerr << "use stdin as default\n" ;
	}
    }

    if( argc > 2 )
    {
	aout = fopen( argv[2], "w" ) ;
	if( aout ){
	    yyout = aout ;
	    out = aout ;
	} else {
	    cerr << "Could not open file `" << argv[2] << "' for writing\n" ;
	    cerr << "use stdout as default\n" ;
	}
    }

    BEGIN(Start) ;
    yylex() ;
    yyDump() ;
    return 0 ;
}
