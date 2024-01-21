/* imports from C News libcnews.a */
extern char *rfc822ize(/* char *msgid */);
extern char *strsave(/* char *s */), *emalloc(/* sizeint */);
extern char *str3save(/* char *s1, char *s2, char *s3 */);
extern char *ctlfile(/* char * */);
extern char *artfile(/* char * */), *fullartfile(/* char * */);
extern int cistreqn(/* char *s1, char *s2, int n */);
extern FILE *efopen(/* char *filename, char *type */);
extern void error(/* char *fmt, char *s */);
extern int split(/* char *string, char *fields[], int nfields, char *sep */);
extern int mkinperm(/* char *tmpname, char *grade, char *class */);
extern void timestamp(/* FILE *fp, time_t * */);
extern char *newspath();

#ifndef __STDC__
# define sizeint int
#else
# include <stddef.h>
# define sizeint size_t
#endif
