/**********************************************************************/
/*                                                                    */
/* strarraycmp: searches for str in array.  Like searching for a char */
/*              in a string, returns the position, 0 based.  -1 = not */
/*              found                                                 */
/*                                                                    */
/**********************************************************************/

int strarraycmp(char *str, char **array)
{
 int cnt;

 for (cnt = 0;(array[cnt]) && strcmp(array[cnt], str);cnt++);
 if (!array[cnt]) return -1;
 return cnt;
}
