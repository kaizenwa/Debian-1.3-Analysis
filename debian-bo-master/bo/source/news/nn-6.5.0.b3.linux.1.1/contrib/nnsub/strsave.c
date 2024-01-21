extern char *malloc();

char *strsave(s)
char *s;
{
char *result;
result= malloc(strlen(s)+1);
strcpy(result,s);
return(result);
} /* strsave */
