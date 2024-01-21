/*
*****************************************************************************
** FILE: misc.c
**
** xbmbrowser is Public Domain. However it, and all the code still belong to me.
** I do, however grant permission for you to freely copy and distribute it on 
** the condition that this and all other copyright notices remain unchanged in 
** all distributions.
**
** This software comes with NO warranty whatsoever. I therefore take no
** responsibility for any damages, losses or problems that the program may 
** cause.
**                                     Anthony Thyssen and Ashley Roll
*****************************************************************************
*/

#include <sys/types.h>     /* for missing types in stat.h */
#include <sys/stat.h>
#include "xbmbrowser.h"

/* Marco for brain dead Ultrix and SVR4 machines
** Brian Dowling <bdowling@ccs.neu.edu> -- Ultrix
** John Polstra <jdp@polstra.com> -- SVR4
*/
#ifndef S_ISLNK
# ifdef S_IFLNK
#  if defined(_S_IFMT)   /* Ultrix */
#   define S_ISLNK(mode) (((mode) & _S_IFMT) == S_IFLNK)
#  elif defined(S_IFMT)  /* SVR4 */
#   define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)
#  endif
# else /* no S_IFLNK then system probably has no symlinks */
#  define S_ISLNK(mode) (0)
# endif
#endif


/*========================================================================*/
/* Cursour Control for Application */

void
set_busywait()
/* change the cursour to a busy symbol */
{
  XtVaSetValues(mainpw, XtNcursor, (XtArgVal)waitCursor, NULL);
  XFlush(display);
}


void
clear_busywait()
/* clear the busywait cursour back to normal */
{
  XtVaSetValues(mainpw, XtNcursor, (XtArgVal)normalCursor, NULL);
  XFlush(display);
}

/*========================================================================*/
/* Control over the stipple of the icon box display */
static Pixmap stipple = (Pixmap)None;

void
set_stipple()
/* set/reset the stipple pattern on the iconbox */
{
  XtVaSetValues(iconbox,
       XtNbackgroundPixmap,
          (XtArgVal)(app_data.solid_bgnd ? XtUnspecifiedPixmap
                                         : stipple  ), NULL );
}

void
init_stipple()
/* initialize the ability to and and remove the stipple in iconbox */
{
  Pixel  foreground;

  /* find and set the stipple background pattern */
  XtVaGetValues(iconbox, XtNborderColor, &foreground, NULL);
  stipple = XmuCreateStippledPixmap(
      XtScreen(iconbox), foreground,  app_data.stipple_bg,  depth);
  XtVaSetValues(iconbox,
      XtNbackground,  app_data.solid_bg,
      XtNbackgroundPixmap,
          (XtArgVal)(app_data.solid_bgnd ? XtUnspecifiedPixmap
                                         : stipple  ), NULL );
}

/*========================================================================*/
/* sort a linked list of the files just read in */

static Item *r;  /* remainer of the list (find the second half to merge) */

static Item *
merge(a,b)
  Item *a, *b;
{
  Item  aux, *temp = &aux;

  while(b != NULL)
    if(a == NULL) { 
      a = b; 
      break;
    } else
    if( strcmp( b->fname, a->fname ) > 0) {
      temp = temp->next = a; 
      a = a->next;
    } else {
      temp = temp->next = b;
      b = b->next;
    }

  temp->next = a;
  return(aux.next);
}

static Item *
sort(n)
  int n;
{
  Item *fi,*la, *temp;

  if(r == NULL) return(NULL);
  else if(n > 1)
    return(merge(sort(n/2),sort((n+1)/2)));
  else {
    fi = r;
    la = r;
    /* build list as long as possible */
    for(r = r->next; r != NULL;)
      if(strcmp(r->fname,la->fname) >= 0) {
        la->next = r;
        la = r;
        r = r->next;
      }
      else if(strcmp(r->fname,fi->fname) <= 0) {
        temp = r;
        r = r->next;
        temp->next = fi;
        fi = temp;
      }
      else break;
    
    la->next = NULL;
    return(fi);
  }
}

/*------------------------------------------------------------------------*/

/* global variables to ease building of linked list */
static Item **last_link;
static int    count;

static Boolean
read_dir(dir)
/* scan the directory given appending any files found to the list given.
** Recursively decend into subdirectories as required.
** Assumes that directory names end in `/'.
*/
  char    *dir;     /* the directory to scan */
{
  DIR            *dirp;       /* directory file pointer */
  struct dirent  *dp;         /* current directory item */
  struct stat     info;       /* file stat info for current item */
  Item           *item;       /* the file items infomation (output) */
  Boolean         subdir;     /* are we in a sub directory? */

  subdir = strcmp(dir, "./") != 0;

  if( (dirp = opendir(dir)) == NULL )
    return FALSE;  /* this (sub)directory is unaccessable -- Bad! */
  
  while( (dp = readdir(dirp)) != NULL ) {

    if( strcmp(dp->d_name, ".") == 0  || strcmp(dp->d_name, "..") == 0 )
      continue;    /* skip directory links */

    item = (Item *)alloc_item();
    *last_link = item;
    last_link = &(item->next);
    count++;

    if( subdir )
      strncpy(item->fname, dir, MAXNAMLEN);
    strncat(item->fname, dp->d_name, MAXNAMLEN);

    stat(item->fname, &info);  /* discover information about this file */

    if( S_ISREG(info.st_mode) ) {      /* IF Plain File */
      item->type = File;                 /* normal file (to study further) */
      item->mtime = info.st_mtime;       /* save its last modification time */
    }
    else 
    if( S_ISDIR(info.st_mode) ) {      /* IF directory -- check if sym-link */
      lstat(item->fname, &info);
      item->type = ( S_ISLNK(info.st_mode) ? DirLink : Dir );
      strncat(item->fname, "/", MAXNAMLEN);     /* append `/' to dir name */
      if( app_data.recursive )                  /* do recursion if needed */
        if( ! read_dir( item->fname ) )
          item->type = DirBad;
    } else
      item->type = Unknown;            /* Unknown or Special file type */
  }

  closedir(dirp);

  return TRUE;  /* success -- Good sub-directory */
}

/*------------------------------------------------------------------------*/
/*------------------------ Public Functions ------------------------------*/

Item *
get_files()
/* return a linked link of all directory items found in the current
** directory (recursively?). The list is sorted and the first item 
** in the list is the parent directory link.
*/
{
  Item *item;   /* pointer to the first item */

  item = (Item *)alloc_item();   /* always alloc at least one item */
  last_link = &(item->next);     /* initialize the lask link pointer */
  count = 0;                     /* and no other itams are in the list */

  strcpy(item->fname, "../");     /* this item is the parent link */

  /* do the actual reading of the current directory
  ** set the first items type in the process.
  */
  item->type = (read_dir("./") ? DirUp : DirBad);

  r = item->next;   /* don't include first item in the sort */
  item->next = sort(count);
  return item;
}
 
time_t
check_file_time(file)
/* just return the current modification time for this file 
** or 0 if deleted or not a file.  This is just a quick check
** routine for the  rescan_file()  function in "images.c"
*/
  char *file;
{
  struct stat     info;       /* file stat info for current item */

  if( stat(file, &info) == 0  && S_ISREG(info.st_mode) ) {
    return  info.st_mtime;
  }
  return 0;
}


