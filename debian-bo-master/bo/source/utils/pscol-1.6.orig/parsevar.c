#include <malloc.h>
#include <stdio.h>

/***********************************************************************/
/*                                                                     */
/* parsevar: Takes the PS_COLOR/TOP_COLOR var and splits it up.        */
/*           convuids is to convert the uid of type 2 to a name.       */
/*                                                                     */
/***********************************************************************/

char **parsevar(char *var)
{
 char **out;
 char *newvar;
 int sets = 1;
 void *pout;

 newvar = malloc(strlen(var) + 1); 
 strcpy(newvar, var);
 if (strlen(var) == 0) return 0;
 for (;*var;var++) if (*var == '%') sets++;
 out = malloc(4 * (sets + 1)); /* Last is a null terminator! */
 /* Note: I'm too lazy to bother freeing this buffer when converting uids
    to usernames.  What I have to do, since more likely the username will
    be longer than the uid, is allocate a new string for the info */
 out[sets] = 0;
 pout = (void *)out;
 *out++ = newvar;
 for (;*newvar;newvar++) if (*newvar == '%')
 {
  *newvar++ = 0;
  *out++ = newvar;
 }
/* out = (char **)pout; useless at the moment */
 return (char **)pout;
}
