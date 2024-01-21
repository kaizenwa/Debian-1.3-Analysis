#include <stdio.h>


char **ExtractWords(char *sentence, int *numWords);
char *FirstWord(char **sentPtr);
int CountWords(char *sentence);


char **ExtractWords(char *sentence, int *numWords)
{
    char **result;
    register int index;

    (*numWords) = CountWords(sentence);
    result = (char **) malloc((*numWords)*sizeof(char *));

    for ( index = 0; index < (*numWords); index++ )
	result[index] = FirstWord(&sentence);

    return result;
}


char *FirstWord(char **sentPtr)
{
    char *start, *end, *result, *ptr;

    /* skip spaces */

    while ( **sentPtr == ' ' )
	(*sentPtr)++;

    /* find start and end of first word */
    start = *sentPtr;
    end = start;
    while ( (**sentPtr != ' ') && (**sentPtr != '\0') )
    {
	(*sentPtr)++;
	end++;
    }

    /* allocate space and copy characters to new string */
    result = (char *) malloc(sizeof(char)*(1+(end-start)/sizeof(char)));
    ptr = result;
    while ( start != end )
    {
	*ptr = *start;
	ptr++;
	start++;
    }
    *ptr = '\0';

    return result;
}


int CountWords(char *sentence)
{
    int numWords = 0;

    while ( *sentence != '\0' )
    {
	while ( *sentence == ' ' )
	    sentence++;

	if ( *sentence == '\0' )
	    break;

	numWords++;
	while ( (*sentence != ' ') && (*sentence != '\0') )
	    sentence++;
    }

    return numWords;
}
