%{
%}

%x	Man
%x	Memo

%%
"//@Man:"[ \t]*\n[ \t\n]*"//@Memo:"	{ fprintf( yyout, "///" ) ; }
"//@Man:"[ \t]*\n[ \t\n]*"/*@Doc:"	{ fprintf( yyout, "/**" ) ; }
"//@Man:"[ \t]*\n[ \t\n]*"/**"		{ fprintf( yyout, "/**" ) ; }
"//@Man:"[ \t]*				{ fprintf( yyout, "/**" ) ;
					 BEGIN( Man ) ;
					}
<Man>[^\n]+				{ fprintf( yyout, "@name " ) ;
					  fprintf( yyout, "%s", yytext ) ;
					}
<Man>\n[ \t\n]*"/*@Doc:"		{ fprintf( yyout, "\n" ) ;
					  BEGIN( 0 ) ;
					}
<Man>\n					{ fprintf( yyout, " */\n" ) ;
					  BEGIN( 0 ) ;
					}

"///"[ \t]*\n[ \t\n]*"/*@Doc:"		{ fprintf( yyout, "/**" ) ; }
"///"[ \t]*\n[ \t\n]*"/**"		{ fprintf( yyout, "/**" ) ; }
"//@ManMemo:"[ \t]*\n[ \t\n]*"/*@Doc:"	{ fprintf( yyout, "/**" ) ; }
"//@ManMemo:"[ \t]*\n[ \t\n]*"/**"	{ fprintf( yyout, "/**" ) ; }
"/*@ManMemo:"				{ fprintf( yyout, "/**@memo " ) ; }
"/*@ManDoc:"				{ fprintf( yyout, "/**" ) ; }
"/*@Doc:"[ \t\n]*"*"+"/"
"//@ManMemo:"				{ fprintf( yyout, "///" ) ; }

("///"|"//@ManMemo:")[ \t]*		{ BEGIN( Memo ) ; }
<Memo>[^\n]*\n				{ fprintf( yyout, "/// %s", yytext ) ;
					  BEGIN( 0 ) ;
					}
<Memo>[^\n]*\n[ \t\n]*"/*@Doc:"		{ int i ;
					  fprintf( yyout, "/** " ) ;
					  for( i = 0 ; yytext[i] != '\n' ; ++i )
					      fprintf( yyout, "%c", yytext[i] ) ;
					  fprintf( yyout, ".\n" ) ;
					  BEGIN( 0 ) ;
					}

"/**"[ \t]*"*/"				{ fprintf( yyout, "///" ) ; }
"/**"[^.\n]*".?"[ \t]*"*/"		{ yytext[yyleng-2] = 0 ;
					  fprintf( yyout, "/// %s", yytext +4 ) ;
					}

%%

int	yywrap()	{ return 1 ; }

main()
{
    yylex() ;
}
