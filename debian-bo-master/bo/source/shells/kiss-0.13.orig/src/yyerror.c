#include "kiss.h"

int yyerror (char *msg)
{
    if (inputparsed)
	warning ("[parser]: failed to analyze input");
    return (0);
}
    
