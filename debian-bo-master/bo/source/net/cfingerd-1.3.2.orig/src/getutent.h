#ifndef __GETUTENT_H__
#define __GETUTENT_H__

#ifdef BSD
struct utmp *getutent();  /* returns next utmp file entry   */
void setutent();
char *getdomainname(char *, size_t);
#endif

#endif
