/* FILE:	filenames.h
 * PURPOSE:	functions for handling file names
 */

/* trim_basename creates a pointer to a basename constructed from a
 * `fullname' and an `extension'. Some examples:
 *    basename("Utopia-Regular.afm", ".afm") -> "Utopia-Regular"
 *    basename("sy______.pfb", ".")          -> "sy______"
 */   

char *trim_basename(char *fullname, char *extension);

/* fullname creates a pointer to the result of concatenating the
 * `basename' and `extension'. The nice thing about this function is
 * that the array to hold the fullname will be automatically created.
 */
char *fullname(char *basename, char *extension);


