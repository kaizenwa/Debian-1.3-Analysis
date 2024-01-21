%{
#include <stdio.h>
#include <string.h>
int x=0;
char str[1024];
%}
%%
^#
\".*\" {
		if (yyleng >2) {
			strcpy(str,yytext+1); str[yyleng-2]='\0';
			printf("$ #%d Original Message:(%s)\n",x++,str);
			printf("# %s\n\n",str);
		}
	}
.|\n
%%
main() {yylex();}

