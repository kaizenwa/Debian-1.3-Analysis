typedef struct {
 char **ps, **top;
} colorrec;

extern char *progname;
char *buildvar(char **col);
char *convansi(char *ansistr);
