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
#include <ctype.h>

#include "doc.h"


/* -----------------------------------------------------------------
 *
 *	statics
 */
static const char*	inputString ;
static int		inputPosition ;
static int		lastContext ;
static int		bracketCount = 0 ;
static Entry*		current_root = 0 ;
static Entry*		global_root  = 0 ;
static Entry*		current      = 0 ;

static int		yyLineNr = 0 ;
static char		yyFileName[264] ;

static void	msg( const char* str, const char* str2=0 )
{
    if( verb )
    {
	printf( "%s(%d):\t%s", yyFileName, yyLineNr, str ) ;
	if( str2 )
	    printf( "%s", str2 ) ;
	printf( "\n" ) ;
    }
}

static void	lineCount()
{
  for( const char* c = yytext ; *c ; ++c )
    yyLineNr += (*c == '\n') ;
}


/* -----------------------------------------------------------------
 */
#undef	YY_INPUT
#define	YY_INPUT(buf,result,max_size) result=yyread(buf,max_size);

static int yyread(char *buf,int max_size)
{
    int c=0;
    while( c < max_size && inputString[inputPosition] )
    {
	*buf = inputString[inputPosition++] ;
	c++; buf++;
    }
    return c;
}

%}

%x	FindClasses
%x	ClassName
%x	ClassBody
%x	Package
%x	Import
%x	Extends
%x	Implements

%x	FindMembers
%x	Member
%x	Args
%x	Function
%x	Throws

%x	Comment
%x	SkipComment

%x	JavaDoc
%x	See
%x	Author
%x	Version
%x	Param
%x	Return
%x	Exception

%%
<*>\x06[^\x06]*\x06				{ yyLineNr= 1 ; int i;
						  for( i = 0 ; yytext[i+1] != 6 ; i++ )
						    yyFileName[i] = yytext[i+1] ;
						  yyFileName[i] = 0 ;
						  current_root  = global_root ;
						}

<FindClasses>^[ \t]*"package"[ \t]*		{ current_root = global_root ;
						  BEGIN( Package ) ;
						}
<FindClasses>^[ \t]*"import"[ \t]*		{ BEGIN( Import ) ; }

<FindClasses>[ \t]*"private"[ \t]+		{ current->protection = PRIV ;
						  current->type += "public " ;
						}
<FindClasses>[ \t]*"public"[ \t]+		{ current->protection = PUBL ;
						  current->type += "public " ;
						}
<FindClasses>[ \t]*"protected"[ \t]+		{ current->protection = PROT ;
						  current->type += "protected " ;
						}
<FindClasses>[ \t]*"class"[ \t]+		{ current->section = CLASS_SEC ;
						  current->type += "class" ;
						  BEGIN( ClassName ) ;
						}
<FindClasses>[ \t]*"interface"[ \t]+		{ current->section = INTERFACE_SEC ;
						  current->type += "interface" ;
						  BEGIN( ClassName ) ;
						}

<Package>[a-z_A-Z0-9]*				{ Entry *find = current_root->sub ;
						  for( ; find ; find = find->next )
						  {
						    if( find->section == PACKAGE_SEC )
						    {
							if( strcmp( yytext, find->name ) == 0 )
							    break ;
						    }
						  }
						  if( find == 0 )
						  {
						    find = new Entry ;
						    find->section     = PACKAGE_SEC ;
						    find->name        = yytext ;
						    find->next        = current_root->sub ;
						    find->parent      = current_root ;
						    current_root->sub = find ;
						  }
						  current_root = find ;
						}
<Package>;					{ BEGIN( FindClasses ) ;
						  if( current->doc.length() )
						  {
						    current_root->doc = current->doc ;
						    current->doc.clear() ;
						  }
						}

<Import>[a-z_A-Z0-9.]*				{ current->import.append( new McString(yytext) ) ; }
<Import>;					{ BEGIN( FindClasses ) ; }

<ClassName>[ \t]*"extends"[ \t]*		{ BEGIN( Extends ) ; }
<ClassName>[ \t]*"implements"[ \t]*		{ BEGIN( Implements ) ; }
<ClassName>[ \t]*"{"[ \t]*			{ BEGIN( ClassBody ) ;
    						  current->file = yyFileName ;
						  current->startLine = yyLineNr ;
						}
<ClassName>[a-z_A-Z0-9]+			{ current->name = yytext ;
						  msg( "found class  ", yytext ) ;
						}

<Extends>[a-z_A-Z.0-9]+				{ current->extends.append( new McString(yytext) ) ;
						  BEGIN( ClassName ) ;
						}
<Implements>[a-z_A-Z.0-9]+			{ current->implements.append( new McString(yytext) ) ;
						  BEGIN( ClassName ) ;
						}
<Implements>[a-z_A-Z.0-9]+[\n \t]*","[\n \t]*	{ McString *tmp=new McString(yytext);
						  lineCount() ;
                                                  tmp->remove(tmp->length()-1);
                                                  while (!isalpha(tmp->last()))
						      tmp->remove(tmp->length()-1);
						  current->implements.append(tmp);
						  BEGIN(Implements) ;
						}

<ClassBody>[^\n{}"/]*				{ current->program += yytext ; }
<ClassBody>\"[^\n"]*]\"				{ current->program += yytext ; }
<ClassBody>\'["{}]				{ current->program += yytext ; }
<ClassBody>[ \t\n]*"/*""*"*[ \t]*		{ current->program += yytext ;
						  lineCount() ;
						  lastContext = YY_START ;
						  BEGIN( Comment ) ;
						}
<ClassBody>"{"					{ current->program += yytext ;
						  ++bracketCount ;
						}
<ClassBody>"}"					{ if( bracketCount )
						  {
						    current->program += yytext ;
						    --bracketCount ;
						  }
						  else
						  {
						    current_root->addSubEntry( current ) ;
						    current      = new Entry ;
						    BEGIN( FindClasses ) ;
						  }
						}
<ClassBody>\n					{ current->program += yytext ;
						  yyLineNr++ ;
						}
<ClassBody>.					{ current->program += yytext ; }



<FindMembers>";"				{ current->name.clear() ;
						  current->type.clear() ;
						  current->args.clear() ;
						  current->program.clear() ;
						  current->doc.clear() ;
						  current->section = EMPTY_SEC ;
						}
<FindMembers>[ \t]*"private"[ \t]+		{ current->protection = PRIV ;
						  BEGIN( Member ) ;
						}
<FindMembers>[ \t]*"public"[ \t]+		{ current->protection = PUBL ;
						  BEGIN( Member ) ;
						}
<FindMembers>[ \t]*"protected"[ \t]+		{ current->protection = PROT ;
						  BEGIN( Member ) ;
						}

<Member>[a-z_A-Z.0-9]+				{ if( current->type.length() )
						    current->type += ' ' ;
						  current->type += current->name ;
						  current->name  = yytext ;
						}
<Member>[;=]					{ current->section = VARIABLE_SEC ;
						  current_root->addSubEntry( current ) ;
						  msg( "found field  ", current->name ) ;
						  current = new Entry ;
						  BEGIN( FindMembers ) ;
						}

<Member>"("					{ current->section = FUNCTION_SEC ;
						  current->args = yytext ;
						  msg( "found method ", current->name ) ;
						  BEGIN( Args ) ;
						}
<Args>")"					{ current->args += *yytext ;
						  BEGIN( Function ) ;
						}
<Args>.						{ current->args += *yytext ; }
<Function>[ \t]*"throws"[ \t]+			{ BEGIN( Throws ) ;
						  current->args += " throws " ;
						}
<Function>[;{]					{
						  current_root->addSubEntry( current ) ;
						  current = new Entry ;
						  BEGIN( FindMembers ) ;
						}

<Throws>[a-z_A-Z.0-9]+[ \t]*,			{ current->args += yytext ; }
<Throws>[a-z_A-Z.0-9]+				{ current->args += yytext ;
						  BEGIN( Function ) ;
						}




<Comment>\n					{ current->program += yytext ;
						  yyLineNr++ ;
						}
<Comment>"/*"					{ current->program += yytext ; }
<Comment>.					{ current->program += yytext ; }
<Comment>.*"*/"					{ current->program += yytext ;
						  BEGIN( lastContext ) ;
						}

<*>[ \t\n]*"/**""*"*[ \t]*			{ lastContext = YY_START ;
						  lineCount() ;
						  current->doc.clear() ;
						  BEGIN( JavaDoc ) ;
						}
<JavaDoc>[\n \t]*"*"+"/"			{ if( lastContext == FindMembers
						  &&  current_root->section == INTERFACE_SEC )
						      BEGIN( Member ) ;
						  else
						      BEGIN( lastContext ) ;
						}
<JavaDoc>"/*"					{ current->doc += yytext ; }
<JavaDoc>\n[ \t]*"*"*[ \t]*			{ current->doc += '\n' ;
						  yyLineNr++ ;
						}
<JavaDoc>[^*\n]*				{ current->doc += yytext ; }
<JavaDoc>\*					{ current->doc += *yytext ; }

<*>"/*"						{ lastContext = YY_START ;
						  BEGIN( SkipComment ) ;
						}
<*>[ \t]*"*/"					{ BEGIN( lastContext ) ; }
<*>"/*""*"*"*/"
<*>.
<*>\n						{ yyLineNr++ ; }
%%


/*@ ----------------------------------------------------------------------------
 */
void	parseJavaClasses(Entry* rt)
{
    if( rt == 0 )
	return ;
    for( Entry *cr = rt->sub ; cr ; cr = cr->next )
    {
	if( cr->program.length() )
	{
	    inputString   = cr->program ;
	    inputPosition = 0 ;
	    javaYYrestart( javaYYin ) ;
	    BEGIN( FindMembers ) ;
	    current_root = cr ;
	    strcpy( yyFileName, cr->file ) ;
	    yyLineNr = cr->startLine ;
	    javaYYlex() ;
	    cr->program.clear() ;
	}
	parseJavaClasses( cr ) ;
    }
    parseDoc( rt ) ;
}

void	parseJava(Entry* rt)
{
    assert( rt ) ;

    current_root = rt ;
    global_root  = rt ;
    current      = new Entry ;

    inputString   = rt->program ;
    inputPosition = 0 ;
    javaYYrestart( javaYYin ) ;
    BEGIN( FindClasses ) ;
    javaYYlex() ;
    rt->program.clear() ;

    parseJavaClasses( rt ) ;

    delete current ;
}

extern "C" {
int	javaYYwrap()	{ return 1 ; }
};
