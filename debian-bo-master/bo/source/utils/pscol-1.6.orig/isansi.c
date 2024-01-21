/************************************************************************/
/*                                                                      */
/* isansi: takes a string and checks to see if it's an ansi string.     */
/*         you need to strip ESC[ from the beginning and m from the end */
/*                                                                      */
/************************************************************************/

int isansi(char *color)
{
 for (;*color;color++) 
  if ((*color < '0' || *color > '9') && *color != ';') return 0;
 return 1;
}
