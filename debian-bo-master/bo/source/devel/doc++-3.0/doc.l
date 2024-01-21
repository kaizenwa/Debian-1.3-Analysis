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
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <fstream.h>
#include <iostream.h>

#include "doc.h"

#define YY_DECL int yylex()

#undef	YY_INPUT
#define	YY_INPUT(buf,result,max_size) result=yyread(buf,max_size);

/*
 *	statics
 */
static int	inPos ;
static const char*	inStr ;
static Entry*	current=0;
static McString templateArgs;
static int templateDepth;
static PROTECTION baseProtection=PRIV;

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
  int	docyywrap( void )		{ return 1 ; }
}
%}

classNameId  [A-Za-z_:][A-Za-z:0-9_]*
name    [({alpha}|\_)({alpha}|{dig}|\_)*
spaces  [ \t]*

%x findBaseClasses1
%x findBaseClasses2
%x findBaseClasses3


%%

<findBaseClasses1>{spaces}":"{spaces}  {
                             baseProtection=PROT;
			     BEGIN(findBaseClasses2);}
<findBaseClasses2>[ \t,]*
<findBaseClasses2>"private"    {baseProtection=PRIV;}
<findBaseClasses2>"protected"  {baseProtection=PROT;}
<findBaseClasses2>"public"     {baseProtection=PUBL;}
<findBaseClasses2>"virtual"    {baseProtection=PUBL;}

<findBaseClasses2>{classNameId} {current->addBaseClass(yytext,baseProtection);}
<findBaseClasses2>"<" { templateArgs="<"; templateDepth=1;
                        BEGIN(findBaseClasses3);}
<findBaseClasses3>[^<>]+ { templateArgs+=yytext;}
<findBaseClasses3>"<" { templateDepth++; templateArgs+="<";}
<findBaseClasses3>">" { templateDepth--; templateArgs+=">";
                        if (templateDepth==0) BEGIN(findBaseClasses2);}

<*>'\n'
%%

//@ ---------------------------------------------------------------------------

void	Entry::findBases()
{
  if (section!=CLASS_SEC){
    fprintf(stderr,"ERROR! findBases called for non class !\n");
    return;
  }
  if (args.length()==0)
    return;
  inPos         = 0 ;
  inStr         = (const char *) args ;
  current       = this ;
  docyyrestart(0) ;
  BEGIN(findBaseClasses1);
  //fprintf(stderr,"Baseclasses for >>>%s<<<\n",(const char *)args);
  docyylex() ;
  
}
