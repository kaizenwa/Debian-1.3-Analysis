/*
 * Temporary prototypes in the absence of system include files.
 */

#ifdef _Exit
novalue	_exit		Params((int));
#endif					/* _Exit */

#ifndef MacGraph
novalue	abort		Params((noargs));
double	atof		Params((char *));
long	atol		Params((char *));
pointer	calloc		Params((unsigned,unsigned));
int	execv		Params((char *, char **));
int	execvp		Params((char *, char **));
novalue	exit		Params((int));
double	fmod		Params((double, double));
char	*getenv		Params((char *));
char	*getmem		Params((unsigned));
char	*index		Params((char *s, int c));
pointer	malloc		Params((msize));
pointer	realloc		Params((pointer, unsigned));
char	*strchr		Params((char *s, int i));
int	strcmp		Params((char *s1, char *s2));
char	*strcpy		Params((char *s1, char *s2));
#if VMS
#ifndef Graphics
char	*strncat	Params((char *s1, char *s2, int n));
int	strncmp		Params((char *s1, char *s2, int n));
char	*strncpy	Params((char *s1, char *s2, int n));
#endif					/* Graphics */
#else					/* VMS */
char	*strncat	Params((char *s1, char *s2, int n));
int	strncmp		Params((char *s1, char *s2, int n));
char	*strncpy	Params((char *s1, char *s2, int n));
#endif					/* VMS */
#endif					/* MacGraph */

char	*ecvt		Params((double value, int count, int *dec, int* sign));
char	*gcvt		Params((double number,int ndigit,char *buf));
double	pow		Params((double x, double y));
#ifdef SystemFnc
int	system		Params((char *));
#endif					/* SystemFnc */
