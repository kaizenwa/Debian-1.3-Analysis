/* news.h
**
** (C) 1995  Charles  (int@link.xs4all.nl)
**
**
*/

/* $Source: /project/master/sysnews/news.h,v $
 * $Revision: 1.1 $
 * $Date: 1995/01/18 05:22:19 $
 */

#define DEF_PAGER       "more"
#define NOEXPFILE       ".noexpire"
#define DATEFORMAT      "(%b %d %Y)"
#define ERRMSG          strerror(errno)

#undef OK
#define OK    0
#undef ERR
#define ERR (-1)


struct {
  int    all:1;
  int    items:1;
  int    names:1;
  int    pager:1;
  int    expire:1;
  int    oneperline:1;
  int    datestamp:1;
  int    verbose:1;
  } f;


#undef __PROTO
#ifdef __STDC__
# define __PROTO(proto) proto
#else
# define __PROTO(proto) ()
#endif

/* news.c */
void main __PROTO((int argc , char **argv ));
void read_sysnews __PROTO((int argc , char **argv ));
int cat __PROTO((char *file ));
int more __PROTO((FILE *pfp , char *file ));
FILE *open_pager __PROTO((void ));
int fcat __PROTO((int fpin , int fpout ));
void doexpire __PROTO((int expireover ));
void create_exclude_list __PROTO((void ));
void add_exclude __PROTO((char *str ));
void print_usage __PROTO((void ));

#undef __PROTO
