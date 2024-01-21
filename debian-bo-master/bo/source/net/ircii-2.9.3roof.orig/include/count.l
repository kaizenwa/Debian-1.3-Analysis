%{
  int n = 0;
%}
%%
"#define"(\40|\t)+[A-Z0-9_]+(\40|\t)+"$\n" {
  yytext[yyleng - 2] = '\0';
  printf ("%s%d\n", yytext, n);
  n++;
}
.*"\n" printf("%s", yytext);
%%
main()
{
  yylex();
  exit(0);
}
