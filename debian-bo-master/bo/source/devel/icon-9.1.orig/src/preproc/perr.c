/*
 * The functions in this file print error messages.
 */
#include "../preproc/preproc.h"
#include "../preproc/pproto.h"
extern char *progname;

/*
 * Prototypes for static functions.
 */
hidden novalue rm_files Params((noargs));

static struct id_lst *file_lst = NULL;

/*
 * errt1 - error message in one string, location indicated by a token.
 */
novalue errt1(t, s)
struct token *t;
char *s;
   {
   errfl1(t->fname, t->line, s);
   }

/*
 * errfl1 - error message in one string, location given by file and line.
 */
novalue errfl1(f, l, s)
char *f;
int l;
char *s;
   {
   fflush(stdout);
   fprintf(stderr, "%s: File %s; Line %d: %s\n", progname, f, l, s);
   rm_files();
   exit(ErrorExit);
   }

/*
 * err1 - error message in one string, no location given
 */
novalue err1(s)
char *s;
   {
   fflush(stdout);
   fprintf(stderr, "%s: %s\n", progname, s);
   rm_files();
   exit(ErrorExit);
   }

/*
 * errt2 - error message in two strings, location indicated by a token.
 */
novalue errt2(t, s1, s2)
struct token *t;
char *s1;
char *s2;
   {
   errfl2(t->fname, t->line, s1, s2);
   }

/*
 * errfl2 - error message in two strings, location given by file and line.
 */
novalue errfl2(f, l, s1, s2)
char *f;
int l;
char *s1;
char *s2;
   {
   fflush(stdout);
   fprintf(stderr, "%s: File %s; Line %d: %s%s\n", progname, f, l, s1, s2);
   rm_files();
   exit(ErrorExit);
   }

/*
 * err2 - error message in two strings, no location given
 */
novalue err2(s1, s2)
char *s1;
char *s2;
   {
   fflush(stdout);
   fprintf(stderr, "%s: %s%s\n", progname, s1, s2);
   rm_files();
   exit(ErrorExit);
   }

/*
 * errt3 - error message in three strings, location indicated by a token.
 */
novalue errt3(t, s1, s2, s3)
struct token *t;
char *s1;
char *s2;
char *s3;
   {
   errfl3(t->fname, t->line, s1, s2, s3);
   }

/*
 * errfl3 - error message in three strings, location given by file and line.
 */
novalue errfl3(f, l, s1, s2, s3)
char *f;
int l;
char *s1;
char *s2;
char *s3;
   {
   fflush(stdout);
   fprintf(stderr, "%s: File %s; Line %d: %s%s%s\n", progname, f, l,
       s1, s2, s3);
   rm_files();
   exit(ErrorExit);
   }

/*
 * addrmlst - add a file name to the list of files to be removed if
 *   an error occurs.
 */
novalue addrmlst(fname)
char *fname;
   {
   struct id_lst *id;

   id =  new_id_lst(fname);
   id->next = file_lst;
   file_lst = id;
   }

/*
 * rm_files - remove files that must be cleaned up in the event of an
 *   error.
 */
static novalue rm_files()
   {
   while (file_lst != NULL) {
      unlink(file_lst->id);
      file_lst = file_lst->next;
      }
   }
