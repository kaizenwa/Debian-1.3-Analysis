#include "kiss.h"

int getinput (FILE *mystdin)
{
    char
	line [LINELEN];
    
    fgets (line, LINELEN - 1, mystdin);
    return (line [0]);
}
