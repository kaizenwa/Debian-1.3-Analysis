/*******************************************************************/
/*                                                                 */
/* allnum: takes a string to see if it consists of all numbers     */
/*         return: true if string is all numbers, false otherwise  */
/*                                                                 */
/*******************************************************************/

int allnum(char *str)
{
 for (;*str;str++) if (*str < '0' || *str > '9') return 0;
 return 1;
}
