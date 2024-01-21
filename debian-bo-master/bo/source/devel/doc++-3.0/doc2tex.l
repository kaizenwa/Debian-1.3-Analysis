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
//#include <String.h> 
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <fstream.h>
#include <iostream.h>

#include "doc.h"
#include "classgraph.hh"

#define YY_DECL int yylex()

#undef	YY_INPUT
#define	YY_INPUT(buf,result,max_size) result=yyread(buf,max_size);


extern int	upArrows ;
extern int	withPrivate ;
extern char*	texFile ;
extern char*	texOption ;
extern char*	texPackage ;

/*
 *	statics
 */
static Entry*	current ;
static int	inPos ;
static int	lastContext ;
static const char*	inStr ;

static enum {
    C_COMMENT,
    SHORT_C_COMMENT,
    CC_COMMENT
} commentMode ;

static int	escapePercent = 0 ;
static int	verbMode      = 0 ;
static int	skip          = 0 ;
static int	tab           = 0 ;
static int	yyLineNr      = 0 ;
static Entry*	ref           = 0 ;
static Entry*	noref         = ref+1 ;
static char	yyFileName[264] ;

static int yyread(char *buf,int max_size)
{
    int c=0;
    while( c<max_size && inStr[inPos] )
    {
	*buf = inStr[inPos++] ;
	c++; buf++;
    }
    return c;
}

extern "C" {
 int	doc2texyywrap( void )		{ return 1 ; }
}

static void	setupVerbMode( void )
{
    if( skip )
    {
	fprintf( out, "\\strut\\goodbreak\n" ) ;
	fprintf( out, "\\noindent{\\tiny\\em %s}", yyFileName ) ;
	fprintf( out, "\\strut\\nopagebreak\\\\\n" ) ;
	skip = 0 ;
    }
    fprintf( out, "\\cxxCodeLine{%d} ", yyLineNr ) ;
    verbMode = 1 ;
    tab      = 0 ;
}
static void	closeVerbMode( void )
{
    fprintf( out, "\n" ) ;
    verbMode = 0 ;
}

//@ ---------------------------------------------------------------------------
%}

%x Code

%x Verb
%x Ref
%x Label
%x Listing
%x Comment
%x LVerb
%x ShortComment
%x RealComment
%x Graph
%x texverbatim
%x TeXlist
%x HTMLlist
%x HTML
%x HTMLverbatim

%%
"#\\#"			{ fprintf( out, "$\\backslash$" ) ; }
"\\#"			{ fprintf( out, "%s", yytext ) ; }
"#"			{ fprintf( out, "{\\tt\\strut " ) ;
			  BEGIN(Verb) ; 
			}
"#define"		{ fprintf( out, "\\#define" ) ;
			  BEGIN(Verb) ; 
			}

"\\Ref"[ \t]*"{"	{ BEGIN(Ref) ; }
<Ref>[^}]*		{   if( ref == 0 )
			    {
				McString	tmp = yytext ;
				ref = getRefEntry( tmp, current ) ;
				ref == noref ;
			    }
			    REJECT ;
			}
<Ref>"}"		{   if( ref &&  ref != noref )
			    {
			        fprintf( out, " ($\\rightarrow$" )  ;
				if( MAKE_DOC(ref) )
				{
				    ref->dumpNumber( out ) ;
				    fprintf( out, "," )  ;
				}
				fprintf( out, " {\\em page \\pageref{cxx." )  ;
				ref->dumpNumber( out ) ;
				fprintf( out, "}})" )  ;
			    }
			    ref = 0 ;
			    BEGIN(0) ;
			}

<Code>[#]		{ fprintf( out, "\\%c", *yytext ) ; }
<Verb>"#"		{ fprintf( out, "}" ) ;
			  BEGIN(0) ; 
			}
<LVerb>"#"		{ fprintf( out, "}" ) ;
			  BEGIN(Comment) ; 
			}
<TeXlist>[ \t\n]*	{ fprintf( out, "}\\strut{} --- ", yytext ) ;
			  BEGIN( 0 ) ;
			}
<HTMLlist>[ \t\n]*	{ fprintf( out, "}\\strut{} --- ", yytext ) ;
			  BEGIN( HTML ) ;
			}

<HTMLlist,TeXlist,Ref,LVerb,Verb,Code>[%_&><{}$ ] { fprintf( out, "\\%c", *yytext ) ; }
<HTMLlist,TeXlist,Ref,LVerb,Verb,Code>"~"	{ fprintf( out, "\\cxxtilde " ) ; }
<HTMLlist,TeXlist,Ref,LVerb,Verb,Code>\\ { fprintf( out, "{$\\backslash$\\relax}" ) ; }
<HTMLlist,TeXlist,Ref,LVerb,Verb,Code>\^	{ fprintf( out, "$\\hat{\\;}$" ) ; }
<HTMLlist,TeXlist,Ref,LVerb,Verb,Code>(.|\n)	{ fprintf( out, "%c", *yytext ) ; }

"<"			{ fprintf( out, "\\<" ) ; }
">"			{ fprintf( out, "\\>" ) ; }
"\\<"			{ fprintf( out, "\\<" ) ; }
"\\>"			{ fprintf( out, "\\>" ) ; }

"\\begin{verbatim}"		{ fprintf( out, "%s", yytext ) ; BEGIN(texverbatim) ; }
<texverbatim>.			{ fprintf( out, "%s", yytext ) ; }
<texverbatim>\n			{ fprintf( out, "%s", yytext ) ; }
<texverbatim>"\\end{verbatim}"	{ fprintf( out, "%s", yytext ) ; BEGIN(0) ; }

.				{ fprintf( out, "%s", yytext ) ; }
\n				{ fprintf( out, "%s", yytext ) ; }

<HTML>[_$#%{}&]		{ fprintf( out, "\\%s", yytext ) ; }
<HTML>[><]			{ fprintf( out, "$%s$", yytext ) ; }
<HTML>"<"[ \t]*[eE][mM][ \t]*">"	{ fprintf( out, "{\\em " ) ; }
<HTML>"<"[ \t]*[bB][ \t]*">"	{ fprintf( out, "{\\bf " ) ; }
<HTML>"<"[ \t]*[iI][ \t]*">"	{ fprintf( out, "{\\it " ) ; }
<HTML>("</em>"|"</b>"|"</i>")	{ fprintf( out, "}" ) ; }
<HTML>("</EM>"|"</B>"|"</I>")	{ fprintf( out, "}" ) ; }
<HTML>"<pre>"			{ fprintf( out, "\\begin{verbatim}\n" ) ;
				  lastContext = YY_START ;
				  BEGIN( HTMLverbatim ) ;
				}
<HTML>"<ol>"			{ fprintf( out, "\\begin{enumerate}\n" ) ; }
<HTML>"</ol>"			{ fprintf( out, "\\end{enumerate}\n" ) ; }
<HTML>"<dl>"			{ fprintf( out, "\\begin{description}\n" ) ; }
<HTML>"</dl>"			{ fprintf( out, "\\end{description}\n" ) ; }
<HTML>"<dt>"			{ fprintf( out, "\\item[" ) ; }
<HTML>"<dd>"			{ fprintf( out, "]\t" ) ; }
<HTML>"<ul>"			{ fprintf( out, "\\begin{itemize}\n" ) ; }
<HTML>"</ul>"			{ fprintf( out, "\\end{itemize}\n" ) ; }
<HTML>"<li>"			{ fprintf( out, "\\item\t" ) ; }
<HTML>"<p>"			{ fprintf( out, "\\strut\\\\" ) ; }
<HTML>\\			{ fprintf( out, "$\\backslash$" ) ; }
<HTML>.					{ fprintf( out, "%s", yytext ) ; }
<HTML>\n				{ fprintf( out, "%s", yytext ) ; } 

<HTMLverbatim>(.|\n)			{ fprintf( out, "%s", yytext ) ; }
<HTMLverbatim>"</pre>"			{ fprintf( out, "\\end{verbatim}\n" ) ;
					  BEGIN( lastContext ) ;
					}

<*>\x06[^\x06]*\x06		{ yyLineNr= 1 ; int i;
		   		  for( i = 0 ; yytext[i+1] != 6 ; i++ )
		   		    yyFileName[i] = yytext[i+1] ;
		   		  yyFileName[i] = 0 ;
				  fprintf( out, "\\strut\\\\\n" ) ;
				  skip = 1 ;
		   		}

<Listing>^[ \t]*\n	{ if( !skip )
			      fprintf( out, "\\cxxCodeLine{%d} \n\\\\\n", yyLineNr ) ;
			  yyLineNr++ ;
			}
<Listing>\t		{
			  if( !verbMode )
			    setupVerbMode() ;
			  do {
			    putc( ' ', out ) ;
			  } while( ++tab % 8 ) ;
			}
<Listing>\"[^\n"]*\"	{
			  if( !verbMode )
			    setupVerbMode() ;
			  fprintf( out, "%s", &(yytext[0]) ) ;
			  tab += yyleng ;
			}
<Listing>.		{
			  if( !verbMode )
			    setupVerbMode() ;
			  putc( yytext[0], out ) ;
			  ++tab ;
			}
<Listing>\n		{
			  if( !skip )
			  {
			    if( !verbMode )
			      setupVerbMode() ;
			    closeVerbMode() ;
			    fprintf( out, "\\\\\n" ) ;
			  }
			  yyLineNr++ ;
			}

<Listing>"//@"[^\n]*\n	{ yyLineNr++ ; }
<Listing>"/*@"[^\n]*"*/"
<Listing>[ \t\n]*"/*@"	{ BEGIN( RealComment ) ; }
<RealComment>.
<RealComment>\n		{ yyLineNr++ ; }
<RealComment>"*/"	{ BEGIN( Listing ) ; }

<Listing>"//"		{ 
			  if( !verbMode )
			    setupVerbMode() ;
			  closeVerbMode() ;
			  fprintf( out, "\\hbox{//" ) ;
			  commentMode = CC_COMMENT ;
			  BEGIN( Comment ) ;
			}
<Listing>"/*"		{ 
			  if( verbMode )
			      closeVerbMode() ;
			  fprintf( out, "\\hbox{/*" ) ;
			  commentMode = SHORT_C_COMMENT ;
			  BEGIN( Comment ) ;
			}
<Listing>[ \t]*\n[\n\t ]*"/*"	{
			  if( verbMode )
			      closeVerbMode() ;
			  commentMode = C_COMMENT ;
			  while( yyleng-- )
			    yyLineNr += (yytext[yyleng] == '\n') ;
			  BEGIN( Comment ) ;
			  fprintf( out, "\\strut\\\\" ) ;
			  fprintf( out, "\\strut\\\\" ) ;
			}
<Comment>"*/"		{
			  switch( commentMode )
			  {
			  case SHORT_C_COMMENT:
			    fprintf( out, "%s", &(yytext[0]) ) ;
			    fprintf( out, "}\\\\\n" ) ;
			    BEGIN( Listing ) ;
			    break ;
			  case C_COMMENT:
			    skip = 1 ;
			    BEGIN( Listing ) ;
			    break ;
			  default:
			    fprintf( out, "%s", &(yytext[0]) ) ;
			    break ;
			  }
			}
<Comment>\n		{
			  switch( commentMode )
			  {
			  case SHORT_C_COMMENT:
			    commentMode = C_COMMENT ;
			    fprintf( out, "}\n" ) ;
			  case C_COMMENT:
			    putc( yytext[0], out ) ;
			    break ;
			  default:
			    fprintf( out, "}\\strut\\\\\n" ) ;
			    BEGIN( Listing ) ;
			    break ;
			  }
			  yyLineNr++ ;
			}
<Comment>"#\\#"		{ fprintf( out, "$\\backslash$" ) ; }
<Comment>"\\#"		{ fprintf( out, "%s", yytext ) ; }
<Comment>"#"		{ fprintf( out, "{\\tt\\strut " ) ;
			  BEGIN(LVerb) ; 
			}
<Comment>.		{ putc( yytext[0], out ) ; }
<Graph>"SP"		{ fprintf( out, "\\cxxNone" ) ; }
<Graph>"||"		{ fprintf( out, "\\cxxLong" ) ; }
<Graph>"L."		{ fprintf( out, upArrows ? "\\cxxLinkPriLeft" \
						 : "\\cxxPriLeft" ) ; }
<Graph>"L-"		{ fprintf( out, upArrows ? "\\cxxLinkProLeft" \
						 : "\\cxxProLeft" ) ; }
<Graph>"L_"		{ fprintf( out, upArrows ? "\\cxxLinkPubLeft" \
						 : "\\cxxPubLeft" ) ; }
<Graph>"l."		{ fprintf( out, upArrows ? "\\cxxLinkPrileft" \
						 : "\\cxxPrileft" ) ; }
<Graph>"l-"		{ fprintf( out, upArrows ? "\\cxxLinkProleft" \
						 : "\\cxxProleft" ) ; }
<Graph>"l_"		{ fprintf( out, upArrows ? "\\cxxLinkPubleft" \
						 : "\\cxxPubleft" ) ; }
<Graph>"D."		{ fprintf( out, upArrows ? "\\cxxLinkPriLeft" \
						 : "\\cxxLastPriLeft" ) ; }
<Graph>"D-"		{ fprintf( out, upArrows ? "\\cxxLinkProLeft" \
						 : "\\cxxLastProLeft" ) ; }
<Graph>"D_"		{ fprintf( out, upArrows ? "\\cxxLinkPubLeft" \
						 : "\\cxxLastPubLeft" ) ; }
<Graph>"d."		{ fprintf( out, upArrows ? "\\cxxLinkPrileft" \
						 : "\\cxxLastPrileft" ) ; }
<Graph>"d-"		{ fprintf( out, upArrows ? "\\cxxLinkProleft" \
						 : "\\cxxLastProleft" ) ; }
<Graph>"d_"		{ fprintf( out, upArrows ? "\\cxxLinkPubleft" \
						 : "\\cxxLastPubleft" ) ; }
<Graph>"R."		{ fprintf( out, upArrows ? "\\cxxPriRight" \
						 : "\\cxxLinkPriRight" ) ; }
<Graph>"R-"		{ fprintf( out, upArrows ? "\\cxxProRight" \
						 : "\\cxxLinkProRight" ) ; }
<Graph>"R_"		{ fprintf( out, upArrows ? "\\cxxPubRight" \
						 : "\\cxxLinkPubRight" ) ; }
<Graph>"r."		{ fprintf( out, upArrows ? "\\cxxPriright" \
						 : "\\cxxLinkPriright" ) ; }
<Graph>"r-"		{ fprintf( out, upArrows ? "\\cxxProright" \
						 : "\\cxxLinkProright" ) ; }
<Graph>"r_"		{ fprintf( out, upArrows ? "\\cxxPubright" \
						 : "\\cxxLinkPubright" ) ; }
<Graph>"^."		{ fprintf( out, upArrows ? "\\cxxFirstPriright" \
						 : "\\cxxLinkPriright" ) ; }
<Graph>"^-"		{ fprintf( out, upArrows ? "\\cxxFirstProright" \
						 : "\\cxxLinkProright" ) ; }
<Graph>"^_"		{ fprintf( out, upArrows ? "\\cxxFirstPubright" \
						 : "\\cxxLinkPubright" ) ; }
<Graph>"|."		{ fprintf( out, upArrows ? "\\cxxFirstPriRight" \
						 : "\\cxxLinkPriRight" ) ; }
<Graph>"|-"		{ fprintf( out, upArrows ? "\\cxxFirstProRight" \
						 : "\\cxxLinkProRight" ) ; }
<Graph>"|_"		{ fprintf( out, upArrows ? "\\cxxFirstPubRight" \
						 : "\\cxxLinkPubRight" ) ; }
%%

//@ ---------------------------------------------------------------------------
static void	initLatex( void )
{
    if( texFile )
    {
	ifstream env( texFile ) ;
	if( env )
	{
	    char	ch ;
	    while( env )
	    {
		env >> ch ;
		putc( ch, out ) ;
	    }
	    if( !noLatex )
	    {
		fprintf( out, "\\usepackage{docxx}\n" ) ;
		fprintf( out, "\\begin{document}\n" ) ;
		fprintf( out, "\\pagebreak\n" ) ;
		fprintf( out, "\\sloppy\n" ) ;
	    }
	}
	else
	    cerr << "could not open " << texFile << endl ;
    }
    else if( !noLatex )
    {
	fprintf( out, "\\documentclass" ) ;
	if( texOption )
	    fprintf( out, "[%s]", texOption ) ;
	fprintf( out, "{article}\n" ) ;
	if( texPackage )
	    fprintf( out, "\\usepackage{%s}\n", texPackage ) ;
	fprintf( out, "\\usepackage{docxx}\n" ) ;
	fprintf( out, "\\begin{document}\n" ) ;
	fprintf( out, "\\pagebreak\n" ) ;
	fprintf( out, "\\sloppy\n" ) ;
    }
}

static void	finitLatex( void )
{
    if( !noLatex )
	fprintf( out, "\\end{document}\n" ) ;
}

void	listing(char* str)
{
    inPos = 0 ;
    inStr = str ;
    skip  = 1 ;

    initLatex() ;
    doc2texyyrestart(0) ;
    BEGIN(Listing);
    doc2texyylex() ;
    finitLatex() ;
}

//@ ---------------------------------------------------------------------------
static void	printCode( Entry* cur, const char* str )
{
    inPos    = 0 ;
    inStr    = str ;
    current  = cur ;
    if( cur && cur->section == MANUAL_SEC )
    {
	printYYDOC( cur, str ) ;
	return ;
    }
    doc2texyyrestart(0) ;
    BEGIN( Code );
    doc2texyylex() ;
}

void	printYYDOC( Entry* cur, const char* str, int escapePrcnt )
{
    inPos         = 0 ;
    inStr         = str ;
    current       = cur ;
    escapePercent = escapePrcnt ;

    doc2texyyrestart(0) ;
    BEGIN( HTMLsyntax ? HTML : 0);
    doc2texyylex() ;

    escapePercent = 0 ;
}

static void	dumpCxxItem( Entry* tmp )
{
    fprintf   ( out, "{" ) ;
    printCode( tmp, tmp->type ) ;
    fprintf   ( out, "}\n        {" ) ;
    printCode( tmp, tmp->name ) ;
    fprintf   ( out, "}\n        {" ) ;
    printCode( tmp, tmp->args ) ;
    fprintf   ( out, "}\n        {" ) ;
    printYYDOC( tmp, tmp->memo ) ;
    fprintf   ( out, "}\n        {" ) ;
    if( MAKE_DOC(tmp) )
    {
	tmp->dumpNumber( out ) ;
	fprintf   ( out, "}\n" ) ;
    }
    else
    {
	fprintf   ( out, "}\n" ) ;
	fprintf ( out, "\\label{cxx." ) ;
	    tmp->dumpNumber( out ) ;
	fprintf ( out, "}\n" ) ;
    }
}

static int	baseDepth( Entry* entry, int n = 1 )
{
    if( !HAS_BASES( entry ) )
	return n ;

    int i, j, nn ;
    nn = n+1 ;

    for( i = 0 ; i < entry->pubBaseclasses.size() ; ++i )
    {
	j = baseDepth( entry->pubBaseclasses[i], n+1 ) ;
	if( j > nn )
	    nn = j ;
    }
    for( i = 0 ; i < entry->proBaseclasses.size() ; ++i )
    {
	j = baseDepth( entry->proBaseclasses[i], n+1 ) ;
	if( j > nn )
	    nn = j ;
    }

    return nn ;
}

void	printYYGraph( const char* str )
{
    inPos         = 0 ;
    inStr         = str ;

    doc2texyyrestart(0) ;
    BEGIN(Graph);
    doc2texyylex() ;
}

static void	dumpInheritanceEntry( ClassGraph* cls )
{
    fprintf( out, "\\cxxInheritanceEntry{" ) ;
	for( int i = 0 ; i < cls->indent ; ++i )
	    fprintf( out, "\\cxxNone" ) ;
	printYYGraph( cls->before ) ;
    fprintf ( out, "}{" ) ;
	if( cls->entry )
	    cls->entry->dumpNumber( out ) ;
    fprintf ( out, "}{" ) ;
	if( cls->entry )
	    printCode( cls->entry, cls->entry->name ) ;
	else
	    printCode( 0, (const char*)cls->name ) ;
    fprintf ( out, "}{" ) ;
	printYYGraph( cls->after ) ;
    fprintf( out, "}\n" ) ;
}

static void	dumpHierarchy( Entry* entry )
{
    ClassGraph*	cls ;
    ClassGraph	cg( entry, 0 ) ;
    cg.addBases() ;
    cg.addDirectChilds() ;

    fprintf( out, "\\begin{cxxInheritance}\n" ) ;
    fprintf( out, "\\paragraph{Inheritance}\\strut\\smallskip\\strut\\\\\n" ) ;

    for( cls = cg.firstLine ; cls != &cg ; cls = cls->nextLine )
	dumpInheritanceEntry( cls ) ;

    fprintf( out, "\\cxxInheritanceEntry[x]{" ) ;
	for( int i = 0 ; i < cls->indent ; ++i )
	    fprintf( out, "\\cxxNone" ) ;
    fprintf ( out, "}{" ) ;
	if( cls->entry )
	    cls->entry->dumpNumber( out ) ;
    fprintf ( out, "}{" ) ;
	if( cls->entry )
	    printCode( cls->entry, cls->entry->name ) ;
	else
	    printCode( 0, (const char*)cls->name ) ;
    fprintf ( out, "}{" ) ;
    fprintf( out, "}\n" ) ;

    for( cls = cls->nextLine ; cls ; cls = cls->nextLine )
	dumpInheritanceEntry( cls ) ;

    fprintf ( out, "\\end{cxxInheritance}\n" ) ;
}

static int	dumpFields( Entry* entry)
{
    int	fields = 0 ;

    if( entry->see.size() )
    {
	fprintf   ( out, "\\cxxSee{\n" ) ;
	for( int i = 0 ; i < entry->see.size() ; ++i )
	{
	    if(i)
		fprintf( out, "\\\\\n" ) ;
	    inPos   = 0 ;
	    inStr   = *entry->see[i] ;
	    current = entry ;
	    doc2texyyrestart(0) ;
	    BEGIN( HTMLsyntax ? HTMLlist : Code );
	    doc2texyylex() ;
	    Entry* ref = getRefEntry( *entry->see[i], current ) ;
	    if( ref )
	    {
		fprintf( out, " ($\\rightarrow$" )  ;
		if( MAKE_DOC(ref) )
		{
		    ref->dumpNumber( out ) ;
		    fprintf( out, "," )  ;
		}
		fprintf( out, " {\\em page \\pageref{cxx." )  ;
		ref->dumpNumber( out ) ;
		fprintf( out, "}})" )  ;
	    }
	}
	fprintf   ( out, "\\strut}\n" ) ;
	fields++ ;
    }

    if( entry->exception.size() )
    {
	fprintf   ( out, "\\cxxExceptions{\n" ) ;
	for( int i = 0 ; i < entry->exception.size() ; ++i )
	{
	    if(i)
		fprintf( out, "\\\\\n" ) ;
	    fprintf( out, "{\\tt\\strut " ) ;
	    inPos   = 0 ;
	    inStr   = *entry->exception[i] ;
	    current = entry ;
	    doc2texyyrestart(0) ;
	    BEGIN( HTMLsyntax ? HTMLlist : TeXlist );
	    doc2texyylex() ;
	}
	fprintf   ( out, "\\strut}\n" ) ;
	fields++ ;
    }

    if( entry->param.size() )
    {
	fprintf   ( out, "\\cxxParameter{\n" ) ;
	for( int i = 0 ; i < entry->param.size() ; ++i )
	{
	    if(i)
		fprintf( out, "\\\\\n" ) ;
	    fprintf( out, "{\\tt\\strut " ) ;
	    inPos   = 0 ;
	    inStr   = *entry->param[i] ;
	    current = entry ;
	    doc2texyyrestart(0) ;
	    BEGIN( HTMLsyntax ? HTMLlist : TeXlist );
	    doc2texyylex() ;
	}
	fprintf   ( out, "\\strut}\n" ) ;
	fields++ ;
    }

    if( entry->retrn.length() )
    {
	fprintf   ( out, "\\cxxReturn{\n" ) ;
	inPos   = 0 ;
	inStr   = entry->retrn ;
	current = entry ;
	doc2texyyrestart(0) ;
	BEGIN( HTMLsyntax ? HTML : 0);
	doc2texyylex() ;
	fprintf   ( out, "\\strut}\n" ) ;
	fields++ ;
    }

    if( entry->author.length() )
    {
	fprintf   ( out, "\\cxxAuthor{\n" ) ;
	inPos   = 0 ;
	inStr   = entry->author ;
	current = entry ;
	doc2texyyrestart(0) ;
	BEGIN( HTMLsyntax ? HTML : 0);
	doc2texyylex() ;
	fprintf   ( out, "\\strut}\n" ) ;
	fields++ ;
    }

    if( entry->version.length() )
    {
	fprintf   ( out, "\\cxxVersion{\n" ) ;
	inPos   = 0 ;
	inStr   = entry->version ;
	current = entry ;
	doc2texyyrestart(0) ;
	BEGIN( HTMLsyntax ? HTML : 0);
	doc2texyylex() ;
	fprintf   ( out, "\\strut}\n" ) ;
	fields++ ;
    }

    return fields ;
}

void	dumpEntry( Entry* entry)
{
    Entry*	tmp ;
    int		fields ;

    switch( entry->section )
    {
    case VARIABLE_SEC:
	fprintf ( out, "\\begin{cxxvariable}\n" ) ;
	break ;
    case FUNCTION_SEC:
	fprintf ( out, "\\begin{cxxfunction}\n" ) ;
	break ;
    case MACRO_SEC:
	fprintf ( out, "\\begin{cxxmacro}\n" ) ;
	break ;
    case CLASS_SEC:
	fprintf ( out, "\\begin{cxxclass}\n" ) ;
	break ;
    case INTERFACE_SEC:
	fprintf ( out, "\\begin{cxxinterface}\n" ) ;
	break ;
    case UNION_SEC:
	fprintf ( out, "\\begin{cxxunion}\n" ) ;
	break ;
    default:
	fprintf ( out, "\\begin{cxxentry}\n" ) ;
	break ;
    }

    dumpCxxItem( entry ) ;
    fields = dumpFields ( entry ) ;

    if( HAS_BASES(entry)  ||  entry->pubChilds.size()  ||  entry->proChilds.size() )
	dumpHierarchy( entry ) ;

    if ( entry->sub )
    {
	if( entry->section == INTERFACE_SEC
	||  entry->section == CLASS_SEC )
	{
	    for( tmp = entry->sub ; tmp ; tmp = tmp->next )
	    {
		if( tmp->protection == PUBL )
		{
		    fprintf ( out, "\\begin{cxxpublic}\n" ) ;
		    for( tmp = entry->sub ; tmp ; tmp = tmp->next )
		    {
			if( tmp->protection == PUBL )
			{
			    fprintf    ( out, "\\cxxitem" ) ;
			    dumpCxxItem( tmp ) ;
			}
		    }
		    fprintf ( out, "\\end{cxxpublic}\n" ) ;
		    break ;
		}
	    }
	    for( tmp = entry->sub ; tmp ; tmp = tmp->next )
	    {
		if( tmp->protection == PROT )
		{
		    fprintf ( out, "\\begin{cxxprotected}\n" ) ;
		    for( tmp = entry->sub ; tmp ; tmp = tmp->next )
		    {
			if( tmp->protection == PROT )
			{
			    fprintf    ( out, "\\cxxitem" ) ;
			    dumpCxxItem( tmp ) ;
			}
		    }
		    fprintf ( out, "\\end{cxxprotected}\n" ) ;
		    break ;
		}
	    }
	    if (withPrivate){
	      for( tmp = entry->sub ; tmp ; tmp = tmp->next )
		{
		  if( tmp->protection == PRIV )
		    {
		      fprintf ( out, "\\begin{cxxprivate}\n" ) ;
		      for( tmp = entry->sub ; tmp ; tmp = tmp->next )
			{
			  if( tmp->protection == PRIV )
			    {
			      fprintf    ( out, "\\cxxitem" ) ;
			      dumpCxxItem( tmp ) ;
			    }
			}
		      fprintf ( out, "\\end{cxxprivate}\n" ) ;
		      break ;
		    }
		}
	    }
	}
	else
	{
	    fprintf ( out, "\\begin{cxxnames}\n" ) ;
	    for( tmp = entry->sub ; tmp ; tmp = tmp->next )
	    {
		fprintf    ( out, "\\cxxitem" ) ;
		dumpCxxItem( tmp ) ;
	    }
	    fprintf ( out, "\\end{cxxnames}\n" ) ;
	}
    }

    if( entry->doc.length() + fields )
    {
	fprintf   ( out, "\\begin{cxxdoc}\n" ) ;
	printYYDOC( entry, entry->doc, 0 ) ;
	fprintf   ( out, "\n\\end{cxxdoc}\n" ) ;
    }

    for( tmp = entry->sub ; tmp ; tmp = tmp->next )
    {
	if ( tmp->protection == PUBL  &&  MAKE_DOC(tmp) )
	    dumpEntry( tmp ) ;
    }
    for( tmp = entry->sub ; tmp ; tmp = tmp->next )
    {
	if ( tmp->protection == PROT  &&  MAKE_DOC(tmp) )
	    dumpEntry( tmp ) ;
    }

    if( withPrivate )
    {
	for( tmp = entry->sub ; tmp ; tmp = tmp->next )
	{
	    if ( tmp->protection == PRIV  &&  MAKE_DOC(tmp) )
		dumpEntry( tmp ) ;
	}
    }

    switch( entry->section )
    {
    case VARIABLE_SEC:
	fprintf ( out, "\\end{cxxvariable}\n" ) ;
	break ;
    case FUNCTION_SEC:
	fprintf ( out, "\\end{cxxfunction}\n" ) ;
	break ;
    case UNION_SEC:
	fprintf ( out, "\\end{cxxunion}\n" ) ;
	break ;
    case MACRO_SEC:
	fprintf ( out, "\\end{cxxmacro}\n" ) ;
	break ;
    case CLASS_SEC:
	fprintf ( out, "\\end{cxxclass}\n" ) ;
	break ;
    case INTERFACE_SEC:
	fprintf ( out, "\\end{cxxinterface}\n" ) ;
	break ;
    default:
	fprintf ( out, "\\end{cxxentry}\n" ) ;
	break ;
    }
}


static void	reNumber( Entry* tp )
{
    Entry* tmp = tp ;
    int	i = 1 ;
    for( tmp = tp->sub ; tmp ; tmp = tmp->next )
    {
	if( MAKE_DOC(tmp) )
	    tmp->subnumber = i++ ;
	reNumber( tmp ) ;
    }
    for( tmp = tp->sub ; tmp ; tmp = tmp->next )
    {
	if( !MAKE_DOC(tmp) )
	    tmp->subnumber = i++ ;
    }
}

static void	dumpContents( Entry* tmp )
{
    for( tmp=tmp->sub ; tmp ; tmp=tmp->next )
    {
	if( MAKE_DOC(tmp)
	&&  (tmp->parent->parent == 0
	    || tmp->section == CLASS_SEC
	    || tmp->section == UNION_SEC
	    || tmp->section == MANUAL_SEC
	    || tmp->section == INTERFACE_SEC
	    || tmp->section == PACKAGE_SEC) )
	{
	    fprintf ( out, "\\cxxContentsEntry{" ) ;
	    tmp->dumpNumber( out ) ;
	    fprintf ( out, "}{" ) ;
	    printCode( tmp, tmp->name ) ;
	    fprintf ( out, "}{" ) ;
	    printYYDOC( tmp, tmp->memo ) ;
	    fprintf ( out, "}\n" ) ;
	    if( tmp->sub )
	    {
		fprintf ( out, "\\begin{cxxContents}\n" ) ;
		dumpContents( tmp ) ;
		fprintf ( out, "\\end{cxxContents}\n" ) ;
	    }
	}
    }
}

static void	dumpClassGraph( Entry* entry )
{
    ClassGraph	cg( entry, 0 ) ;
    ClassGraph*	cls = &cg ;
    cg.addAllChilds() ;

    fprintf ( out, "\\strut\\\\[5pt]\\goodbreak\n" ) ;
    fprintf ( out, "\\cxxClassGraphEntry{" ) ;
    fprintf ( out, "}{" ) ;
	cls->entry->dumpNumber( out ) ;
    fprintf ( out, "}{" ) ;
	printCode( cls->entry, cls->entry->name ) ;
    fprintf ( out, "}{" ) ;
    fprintf( out, "}\n" ) ;

    for( cls = cls->nextLine ; cls ; cls = cls->nextLine )
    {
	fprintf ( out, "\\cxxClassGraphEntry{" ) ;
	    for( int i = 0 ; i < cls->indent ; ++i )
		fprintf( out, "\\cxxNone" ) ;
	    printYYGraph( cls->before ) ;
	fprintf ( out, "}{" ) ;
	    cls->entry->dumpNumber( out ) ;
	fprintf ( out, "}{" ) ;
	    printCode( cls->entry, cls->entry->name ) ;
	fprintf ( out, "}{" ) ;
	fprintf( out, "}\n" ) ;
    }
}

static void	dumpClassGraphs( Entry* tmp )
{
    for( ; tmp ; tmp=tmp->next )
    {
	if( tmp->section == CLASS_SEC
	&&  tmp->proBaseclasses.size() == 0
	&&  tmp->pubBaseclasses.size() == 0 )
	{
	    tmp->currentParent = 0 ;
	    dumpClassGraph( tmp ) ;
	}
	dumpClassGraphs( tmp->sub ) ;
    }
}

void	usermanTeX(char* str, Entry* root)
{
    Entry	*tmp ;

    initLatex() ;
    // reNumber( root ) ;

    if( root->sub && root->sub->next == 0 && root->sub->section == MANUAL_SEC )
	root = root->sub ;

    if( root->name.length() )
    {
	dumpFields ( root ) ;
	fprintf ( out, "\\cxxTitle" ) ;
	dumpCxxItem( root ) ;
    }

    fprintf ( out, "\\begin{cxxContents}\n" ) ;
    dumpContents( root ) ;
    fprintf ( out, "\\cxxContentsEntry{}{Class Graph}{}" ) ;
    fprintf ( out, "\\end{cxxContents}\n" ) ;

    if( root->name.length() )
    {
	fprintf ( out, "\\clearpage\\pagebreak\n" ) ;
	if( root->doc.length() )
	{
	    fprintf   ( out, "\\begin{cxxdoc}\n" ) ;
	    printYYDOC( root, root->doc, 0 ) ;
	    fprintf   ( out, "\n\\end{cxxdoc}\n" ) ;
	}
    }

    for( tmp=root->sub ; tmp ; tmp=tmp->next )
	dumpEntry( tmp) ;

    fprintf ( out, "\\begin{cxxClassGraph}\n" ) ;
    fprintf ( out, "\\label{cxx.}\n" ) ;
    root->currentParent = 0 ;
    dumpClassGraphs( root ) ;
    fprintf ( out, "\\end{cxxClassGraph}\n" ) ;

    finitLatex() ;
}
