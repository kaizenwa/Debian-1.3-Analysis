/* missing prototype definitions in the AIX and Ultrix system include files */

#ifndef _BSD
  #define _BSD 43
#endif

int socket(int, int, int);
int connect(int, struct sockaddr *, int);
int shutdown(int, int);
int getpeername(int, struct sockaddr *, int *);
int getsockname(int, struct sockaddr *, int *);
/* int getopt(int, char **, const char *); */
int chown(const char *, uid_t, gid_t);
int chdir(const char *);
time_t time(time_t *);
