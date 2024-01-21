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

#include "doc.h"
#include "nametable.hh"

extern NameTable	gifs ;

#define YY_DECL int yylex()

#undef	YY_INPUT
#define	YY_INPUT(buf,result,max_size) result=yyread(buf,max_size);
McString outStr;
static int inputPosition=0,inLen=0,texKlammerCnt=0;
static const char *inStr;
static void push(char *);
static char *pop();
static void Output(const char *);
static char *tabFlag=0;
static int	doItem = 0 ;
static int yyWithLinks=0;
static int yyread(char *buf,int max_size)
{
    int c=0;
    while (c<max_size && inputPosition<inLen)
    {
	*buf = inStr[inputPosition++] ;
	c++; buf++;
    }
    return c;
}

#define forbidden(c) (c==' ' || c=='\t' || c=='\n' || c=='}' || c==']' \
		       || c=='{' || c=='[' )


static Entry*	context ;

static void outputTeX(const char *text)
{
    int num=0;
    if (noGifs){
	Output("<PRE>");
	Output(text);
	Output("</PRE>");
    }
    if( !gifs.has( text ) ){
	gifs.add( gifs.num()+1, text ) ;
	num = gifs.num();
    } else {
	num = gifs[text];
    }
    char buf[200];
    sprintf(buf,"<IMG BORDER=0 SRC=g%06d.gif>",num);
    Output(buf);
}

static char *getArg()
{
    yytext[--yyleng]=0;
    while (forbidden(yytext[yyleng-1])) yytext[--yyleng]=0;

    while (yytext[yyleng-1] != '{' && yytext[yyleng-1] != '[') --yyleng;
    return(&yytext[yyleng]);
}
static int initial;
    
%}
spaces  [ \t]*
name    [({alpha}|\_)({alpha}|{dig}|\_)*
num1    {dig}+\.?([eE][-+]?{dig}+)?
num2    {dig}*\.{dig}+([eE][-+]?{dig}+)?
possibleLink [A-Za-z_][A-Za-z0-9_:.]*
BeginTab "\\begin{tabular}{"[lrc|]+"}"
arg      [^\n}\]]*


%x Verbatim
%x texmode
%x Zaun
%x equation
%x eqnarray
%x eqn
%x texgif
%x JAVA_SEE

%x DXXisHTML
%x DXXisTeX

%%

<DXXisHTML,DXXisTeX>"\\Ref{"{arg}"}"        {
			 char *arg=getArg();
			 McString bla, tmp ;
			 tmp = arg ;
			 Entry*	ref = getRefEntry(tmp, context) ;
			 if( ref )
			 {
			     entry2link( bla, ref ) ;
			     Output(bla);
			 }
			 else
			     Output(arg);
                        }
<DXXisHTML,DXXisTeX>"\\URL["{arg}"]{"{arg}"}" {Output ("<A HREF="); char *arg2=getArg();
                         char *arg=getArg();
                         Output(arg2);
                         Output(">"); Output(arg); Output("</A>");
                       }
<DXXisHTML,DXXisTeX>"\\URL{"{arg}"}"        {Output ("<A HREF="); char *arg=getArg();
                         Output(arg);
                         Output(">"); Output(arg); Output("</A>");
                       }


<DXXisTeX>"{"{spaces}"\\em"{spaces}	{Output("<EM>"); push("</EM>");}
<DXXisTeX>"{"{spaces}"\\it"{spaces}	{Output("<IT>"); push("</IT>");}

<DXXisTeX>"{"{spaces}"\\tiny"{spaces}	{Output("<FONT SIZE=\"-4\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\scriptsize"{spaces}	{Output("<FONT SIZE=\"-3\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\footnotesize"{spaces} {Output("<FONT SIZE=\"-2\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\small"{spaces}	{Output("<FONT SIZE=\"-1\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\large"{spaces}	{Output("<FONT SIZE=\"+1\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\Large"{spaces}	{Output("<FONT SIZE=\"+2\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\LARGE"{spaces}	{Output("<FONT SIZE=\"+3\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\huge"{spaces}	{Output("<FONT SIZE=\"+4\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\Huge"{spaces}	{Output("<FONT SIZE=\"+5\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\HUGE"{spaces}	{Output("<FONT SIZE=\"+6\">"); push("</font>");}
<DXXisTeX>"{"{spaces}"\\tt"{spaces}	{Output("<TT>"); push("</tt>");}
<DXXisTeX>"{"{spaces}"\\bf"{spaces}	{Output("<B>"); push("</b>");}
<DXXisTeX>"}"				{Output(pop());}
<DXXisTeX>"{"            			{Output("{"); push("}");}
<DXXisTeX>"\\{"          			{Output("{");}

<DXXisTeX>"\\TEX{"       	{BEGIN(texmode); texKlammerCnt=1;}
<texmode>"\\}"		{yymore();}
<texmode>"\\{"		{cerr << "no problem\n"; yymore();}
<texmode>"{"            {texKlammerCnt++; yymore();}
<texmode>"}"		{ if (--texKlammerCnt == 0){
				yytext[--yyleng]=0;
				outputTeX(yytext);
				BEGIN(initial);
			 } else
			    yymore();
			}
<texmode>.|\n		{yymore();}


<DXXisTeX>"\\begin{verbatim}" {Output("<PRE>"); BEGIN(Verbatim);}

<DXXisTeX>"\\begin{equation}"		{ yymore() ; BEGIN(equation) ; }
<equation>.|\n			{ yymore() ; }
<equation>"\\end{equation"[ *]*"}" {
                                  Output("<br><CENTER>");
				  outputTeX(yytext);
				  Output("<br></CENTER>");
				  BEGIN(initial) ;
				}

<DXXisTeX>"\\["				{ yymore() ; BEGIN(eqn) ; }
<eqn>.|\n			{ yymore() ; }
<eqn>"\\]"			{ 
                                  Output("<br><CENTER>");
				  outputTeX(yytext);
				  Output("<br></CENTER>");
				  BEGIN(initial) ;
				}

\$[^$]+\$			{  outputTeX(yytext);}

<DXXisTeX>"\\begin{eqnarray}"		{ yymore() ; BEGIN(eqnarray) ; }
<eqnarray>.|\n			{ yymore() ; }
<eqnarray>"\\end{eqnarray}"	{
                                  Output("<br><CENTER>");
				  outputTeX(yytext);
				  Output("<br></CENTER>");
				  BEGIN(initial) ;
				}

<DXXisTeX>"\\begin{eqnarray}"[^}]*"}" {Output("<PRE>"); BEGIN(Verbatim);}
<DXXisTeX>"\$\$"  {Output("<PRE>"); BEGIN(Verbatim);}

<DXXisTeX>"\\begin{center}"	{ Output("<CENTER>"); }
<DXXisTeX>"\\end{center}"		{ Output("</CENTER>"); }

<DXXisTeX>"\\begin{flushleft}"	{ Output(" "); }
<DXXisTeX>"\\end{flushleft}"	{ Output(" "); }

<DXXisTeX>"\\begin{flushright}"	{ Output(" "); }
<DXXisTeX>"\\end{flushright}"	{ Output(" "); }

<DXXisTeX>"\\begin{itemize}"	{Output("<UL>"); }
<DXXisTeX>"\\end{itemize}"	{Output("</UL>"); }

<DXXisTeX>"\\begin{enumerate}"	{ Output("<OL>"); }
<DXXisTeX>"\\item"		{ Output("<LI>"); }
<DXXisTeX>"\\end{enumerate}"	{ Output("</OL>"); }

<DXXisTeX>"\\begin{description}"	{ Output("<DL>"); }
<DXXisTeX>"\\item["		{ Output("<DT><B>"); doItem = 1 ; }
<DXXisTeX>"]"			{ if( doItem )
			  {
			    Output("</B><DD>");
			    doItem = 0 ;
			  }
			  else
			    Output("]");
			}
<DXXisTeX>"\\end{description}"	{ Output("</DL>"); }

<Verbatim>"\\end{verbatim}" {Output("</pre>"); BEGIN(initial);}
<Verbatim>"\n"		{ Output(yytext); }

<DXXisTeX>"\\hline"		{
			    if( tabFlag )
				Output( "</TD></TR><TR><TD>" ) ; 
			    else
				Output( "<HR>" ) ; 
			}

<DXXisTeX>"\\\"a"		{Output("&auml;");}
<DXXisTeX>"\\\"o"		{Output("&ouml;");}
<DXXisTeX>"\\\"u"		{Output("&uuml;");}
<DXXisTeX>"\\\"A"		{Output("&Auml;");}
<DXXisTeX>"\\\"O"		{Output("&Ouml;");}
<DXXisTeX>"\\\"U"		{Output("&Uuml;");}
<DXXisTeX>"\\3"		{Output("&#223;");}

<DXXisTeX>"\\#"		{Output("#");}
<DXXisTeX>"#define"	{Output("#define");}
<DXXisTeX>"\\ "		{Output(" ");}
<DXXisTeX>"\\_"		{Output("_");}
<DXXisTeX>"<"		{Output("&lt;");}
<DXXisTeX>">"		{Output("&gt;");}

<DXXisTeX>"#"  	        {Output("<TT>"); BEGIN (Zaun);}

<Zaun>[#\n]     {Output("</TT>"); BEGIN(initial);}

<Zaun,Verbatim>"&"       {Output("&amp;");}
<Zaun,Verbatim>"<"       {Output("&lt;");}
<Zaun,Verbatim>">"       {Output("&gt;");}
<Zaun,Verbatim>"\""       {Output("&quot;");}
<Zaun,Verbatim>.     {Output(yytext);}


<DXXisTeX>{BeginTab}      {Output("<TABLE BORDER>\n<TR><TD>"); tabFlag++;}

<DXXisTeX>"&"             {if (tabFlag) Output("</TD><TD>"); else Output("&amp;");}
<DXXisTeX>"\\\\"          {if (tabFlag) Output("</TD></TR><TD>"); else Output("<BR>");}
<DXXisTeX>"\n"[ \t]*"\n"  {Output("<P>");}


<DXXisTeX>"\\end{tabular}" {Output("</TR></TABLE>");tabFlag--;}
<DXXisTeX,DXXisHTML,JAVA_SEE>{possibleLink}  { 
                  if (yyWithLinks){     
                      //if (verb) printf("link(%s)\n",yytext);
                      Entry *ref=getRefEntry(yytext,context);
		      if (ref){
			McString tmp;
			entry2link( tmp, ref ,(const char *) yytext) ;
			Output(tmp);			
		      } else 
			Output(yytext);
                  } else     
		    Output(yytext);
                 }
<JAVA_SEE>[A-Za-z_.0-9]*"#"[A-Za-z_.0-9]* { McString tmp(yytext);
                                   McString base(tmp,0,tmp.index('#'));
                                   McString label(tmp,tmp.index('#')+1,tmp.length()-tmp.index('#')-1);
                                   if (base.length()) {
	                                   if (context)
					       context->makeFullName(base);
		                           base+=".html";
				   }                                    
                                      Output("<A HREF=\"");
                                      Output(base); 
                                      Output("#"); 
                                      Output(label); 
                                      Output("\">"); 
                                      Output(label); 
                                      Output("</A>"); 
                                 }
<JAVA_SEE>.			{ Output(yytext);}
<JAVA_SEE>\n			{ Output(yytext);}
 		     
<DXXisTeX,DXXisHTML>. 		{Output(yytext);}
<DXXisTeX,DXXisHTML>"\n"	{Output(yytext);}

<*>\n
%%

McDArray <char *> stack;

int	str2htmlyywrap()	{ return 1 ; }

void push(char *s)
{
	stack.append(s);
}

char *pop()
{
  if (stack.size()){
    char *s=stack.last() ;
    stack.removeLast() ;
    return (s);
  } else {
    return ("}");
  }
}

void Output(const char *o)
{
  outStr+=o;
}

char *strToHtml(const char *in,char *dest, Entry* ct,int withLinks)
{
  context = ct ? ct : root ;
  inStr=in;
  outStr="";
  inLen=strlen(inStr);
  inputPosition=0;
  stack.resize(0);
  yyWithLinks=withLinks;
  //if (verb) printf("StrToHtml(%s), links=%d\n",inStr,yyWithLinks);
  if (HTMLsyntax)
      initial=DXXisHTML;
  else
      initial=DXXisTeX;
  BEGIN(initial);
  str2htmlyylex();
  if (dest){
    strcpy(dest,(const char *)outStr);
    return(dest);
  } else 
    return(strdup((const char *)outStr));
}

char *seeToHtml(const char *in, Entry* ct)
{
    context = ct ? ct : root ;
    inStr=in;
    outStr="";
    inLen=strlen(inStr);
    inputPosition=0;
    stack.resize(0);
    yyWithLinks=1;
    BEGIN(JAVA_SEE);
    str2htmlyylex();
    return(strdup((const char *)outStr));
}
