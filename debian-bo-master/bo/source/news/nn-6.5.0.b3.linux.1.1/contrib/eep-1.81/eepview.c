/*------------------------------------------------------------------------
       Name: eepview.c

This module provides code to open a newsgroup directory, construct a
list of all the messages, and display the list, along with subjects
and names of the posters, plus size of messages.  This will replace the
full screen, and allow scrolling down with a pointer in a similar way
to the main screen.

------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <curses.h>

#ifdef UNIX
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#endif /* UNIX */

#ifdef ANSI
#include <stdlib.h>
#else
#define void int
extern char *malloc();
#endif /* ANSI */

#ifdef DIRENT	/* defined in makefile */
#include <dirent.h>	/* or <sys/dirent.h> */
#define direct dirent
#else
#include "ndir.h"
#endif /* DIRENT */

#ifdef DOS
#include <dos.h>
#endif /* DOS */

#include "eep.h"

/* function definitions */
void    showarts();

FILE    *farticle;

struct newsview    {

long    artnum;        /* article number */
long    size;        /* size in bytes */
char    *from;        /* poster */
char    *date;        /* date */
char    *msgid;        /* Msg-id: */
char    *subject;    /* Subject: */
char    *keywords;    /* Keywords: */
char    *references;    /* References: */
};

int    news_index;    /* index into newsview */
int    art_count;    /* count of articles */
int    art_current;  /* current article index */
long    artnum_tmp;    /* temporary article number */
char     *dir;        /* pointer to directory name */
int     viewpos = 0;    /* view position */

char	nirvana[] = "(null)";

extern int eeplines;
extern int eeppage;
extern int eepoint;
extern int current;
extern WINDOW *over;
extern char buffer[];
extern char *shift_lower();
extern char   *wallop();     /* see eepmisc.c */

struct newsview *art_ptr; /* pointer to news structure */

struct newsview *article[MAXARTS];    /* array of pointers to articles struct */

int    artnum_compare(item1,item2)    /* sort by article number */
struct newsview    **item1, **item2;
{
struct newsview *ptr1, *ptr2;
long int diff;

    ptr1 = *item1;
    ptr2 = *item2;
    diff = ptr1->artnum - ptr2->artnum;
    if (diff == 0L) return(0);
    if (diff > 0L) return(1);
    if (diff < 0L) return(-1);
}

#ifdef DOS

/* Note that we do not use wallop() to replace the malloc()
 * calls here because they get freed quickly.
 */
static void free_dircontents (struct _dircontents *);

#define ATTRIBUTES    (_A_RDONLY | _A_HIDDEN | _A_SYSTEM | _A_SUBDIR)

DIR *
opendir (char *name)
{
  struct find_t find_buf;
  DIR *dirp;
  struct _dircontents *dp;
  char name_buf[_MAX_PATH + 1];
  char *slash = "";

  if (!name)
    name = "";
  else if (*name)
    {
      char *s;
      int l = strlen (name);

      s = name + l - 1;
      if ( !(l == 2 && *s == ':') && *s != '\\' && *s != '/')
    slash = "/";    /* save to insert slash between path and "*.*" */
    }

  strcat (strcat (strcpy (name_buf, name), slash), "*.*");

  dirp = (DIR *) malloc (sizeof (DIR));
  if (dirp == (DIR *)0)
    return (DIR *)0;

  dirp->dd_loc = 0;
  dirp->dd_contents = dirp->dd_cp = (struct _dircontents *) 0;

  if (_dos_findfirst (name_buf, ATTRIBUTES, &find_buf))
    {
/*      free (dirp); */
      return (DIR *)0;
    }

  do
    {
      dp = (struct _dircontents *) malloc (sizeof (struct _dircontents));
      if (dp == (struct _dircontents *)0)
    {
/*      free_dircontents (dirp->dd_contents); */
      return (DIR *)0;
    }

      dp->_d_entry = (char *)malloc (strlen (find_buf.name) + 1);
      if (dp->_d_entry == (char *)0)
    {
/*      free(dp); */
/*      free_dircontents (dirp->dd_contents); */
      return (DIR *)0;
    }

      if (dirp->dd_contents)
    dirp->dd_cp = dirp->dd_cp->_d_next = dp;
      else
    dirp->dd_contents = dirp->dd_cp = dp;

      strcpy (dp->_d_entry, find_buf.name);

      dp->_d_next = (struct _dircontents *)0;

    } while (! _dos_findnext (&find_buf));

  dirp->dd_cp = dirp->dd_contents;

  return dirp;
}


void
closedir (DIR *dirp)
{
  free_dircontents (dirp->dd_contents);
  free (dirp);
}


struct direct *
readdir (DIR *dirp)
{
  static struct direct dp;

  if (dirp->dd_cp == (struct _dircontents *)0)
    return (struct direct *)0;
  dp.d_namlen = dp.d_reclen =
    strlen ((char *)strcpy (dp.d_name, dirp->dd_cp->_d_entry));
  strlwr (dp.d_name);        /* JF */
  dp.d_ino = 0;
  dirp->dd_cp = dirp->dd_cp->_d_next;
  dirp->dd_loc++;

  return &dp;
}


void
seekdir (DIR *dirp, long off)
{
  long i = off;
  struct _dircontents *dp;

  if (off < 0)
    return;
  for (dp = dirp->dd_contents; --i >= 0 && dp; dp = dp->_d_next)
    ;
  dirp->dd_loc = off - (i + 1);
  dirp->dd_cp = dp;
}


long
telldir (DIR *dirp)
{
  return dirp->dd_loc;
}


/* Garbage collection */

static void
free_dircontents (struct _dircontents *dp)
{
  struct _dircontents *odp;

  while (dp)
    {
      if (dp->_d_entry)
    free ((struct _dirdesc *)dp->_d_entry);
      dp = (odp = dp)->_d_next;
      free ((struct _dirdesc *)odp);
    }
}
#endif /* DOS */

/* Main EEP routines start here */


void    eepview(groupname)
char    *groupname;    /* place to find articles */
{
  static DIR *directory;
  struct direct *entry = (struct direct *)0;
  char *name = "";

char    *ptr;        /* general pointer */
char    search[BUFSIZE];    /* search string buffer */
int    found = FALSE,    /* flag to say we've found something */
    quit = FALSE,    /* flag used when ready to quit */
    index,        /* used when searching */
    top = 0,
    counter,    /* used when counting */
    ch;        /* input character */

    
    DIR *dirp;
    struct direct *direntp;

    if (groupname == (char *)NULL) return;
    if (strlen(groupname) == 0) return;
/* Allow two extra bytes, one for terminating null and
 * the other for the separating '/' if needed. */
    dir = (char *)wallop(strlen(NEWSBASE)+strlen(groupname)+2);
    strcpy(dir,NEWSBASE);
    if (dir[strlen(dir)-1] != '/') strcat(dir,"/");    
    strcat(dir,groupname);
    /* convert newsgroup name to directory name */
    while ((ptr = strchr(dir,'.')) != (char *) NULL)
        *ptr = '/';
    if ((dirp = opendir(dir)) == NULL)
    {
        move(eeplines - 1,0);
        deleteln();
        printw("Cannot open %s",dir);
        move(current,0);
        refresh();
        sleep(2);
        return;
    }
    move(eeplines - 1,0);
    deleteln();
    printw("Opening %s",dir);
    refresh();
    news_index = 0;
    errno = 0;
    while ((direntp = readdir(dirp)) != NULL) {
        artnum_tmp = atol(direntp->d_name);
        if (artnum_tmp <= 0) continue;

        /* atol() will be fooled by filenames that start with
         * digits, so let's confirm that it has only digits. */

        ptr = direntp->d_name;
        while (isdigit((int)(*ptr))) ptr++;
        if (*ptr != '\0') break; /* should point to terminating null */

        if ((art_ptr = (struct newsview *)wallop(sizeof(struct newsview))) == 
            (struct newsview *) NULL) {
            fprintf(stderr,"Memory allocation error!\n");
            cleanup();
        }
        article[news_index] = art_ptr;
        art_ptr->artnum = artnum_tmp;
        art_ptr->size = 0L;
        art_ptr->from = nirvana;
        art_ptr->date = nirvana;
        art_ptr->msgid = nirvana;
        art_ptr->subject = nirvana;
        art_ptr->keywords = nirvana;
        art_ptr->references = nirvana;
        news_index++;
    }
/*    closedir(dirp); */
    if (errno != 0)
        fprintf(stderr,"Error reading directory %s\n",dir);

    art_count = news_index;
    if (art_count == 0) {
        move(eeplines - 1,0);
        deleteln();
        printw("No articles found in %s",dir);
        refresh();
        sleep(2);
        return;
    }

/* We have now read in the directory, keeping only those file names
 * which are numeric.  (We're ignoring aberrant files with leading
 * zeros -- this assumes standard news files.)
 *   Now we shall sort this into numerical order. */

    qsort( article, (unsigned) art_count, 
        (int) sizeof(article[0]), artnum_compare);

 /* Now let's read each file to get some basic info on it.  */

    news_index = 0;
    while (news_index < art_count) {
        /* Start by initializing the data element */
        art_ptr = article[news_index];
        if (art_ptr == (struct newsview *)NULL) {
            news_index++;
            continue;
        }
        sprintf(buffer, "%s/%ld", dir, art_ptr->artnum); 
        if ((farticle = fopen(buffer,"r")) == (FILE *)NULL) {
            news_index++;
            continue;
        }
        while (fgets(buffer,BUFSIZE,farticle) != (char *)NULL) {

            /* Since we're reading headers, break at the first
             * line that isn't part of a header, especially
             * the blank line that separates the header from
             * the body of the message
             */
            if ((buffer[0] == '#') ||
                (buffer[0] == '\n') ||
                (buffer[0] == '\r') ||
                (buffer[0] == '\0'))
                break;
    
        /* Now allocate memory for article headers */

            ptr = shift_lower(strtok(buffer," \t\r\n"));

            if (strcmp(ptr,"from:") == 0) {
                ptr = strtok((char *)NULL,"\r\n");
                if ((art_ptr->from = (char *) wallop(strlen(ptr)+1)) 
                    == (char *)NULL) {
                    printf("Error while allocating memory!\n");
                    sleep(2);
                    return;
                }
                strcpy(art_ptr->from,ptr);
                continue;
            }

            if (strcmp(ptr,"date:") == 0) {
                ptr = strtok((char *)NULL,"\r\n");
                if ((art_ptr->date = (char *) wallop(strlen(ptr)+1)) 
                    == (char *)NULL) {
                    printf("Error while allocating memory!\n");
                    sleep(2);
                    return;
                }
                strcpy(art_ptr->date,ptr);
                continue;
            }

            if (strcmp(ptr,"message-id:") == 0) {
                ptr = strtok((char *)NULL,"\r\n");
                if ((art_ptr->msgid = (char *) wallop(strlen(ptr)+1)) 
                    == (char *)NULL) {
                    printf("Error while allocating memory!\n");
                    sleep(2);
                    return;
                }
                strcpy(art_ptr->msgid,ptr);
                continue;
            }

            if (strcmp(ptr,"subject:") == 0) {
                ptr = strtok((char *)NULL,"\r\n");
                if ((art_ptr->subject = (char *) wallop(strlen(ptr)+1)) 
                    == (char *)NULL) {
                    printf("Error while allocating memory!\n");
                    sleep(2);
                    return;
                }
                strcpy(art_ptr->subject,ptr);
                continue;
            }

            if (strcmp(ptr,"references:") == 0) {
                ptr = strtok((char *)NULL,"\r\n");
                if ((art_ptr->references = (char *) wallop(strlen(ptr)+1)) 
                    == (char *)NULL) {
                    printf("Error while allocating memory!\n");
                    sleep(2);
                    return;
                }
                strcpy(art_ptr->references,ptr);
                continue;
            }

            if (strcmp(ptr,"keywords:") == 0) {
                ptr = strtok((char *)NULL,"\r\n");
                if ((art_ptr->keywords = (char *) wallop(strlen(ptr)+1)) 
                    == (char *)NULL) {
                    printf("Error while allocating memory!\n");
                    sleep(2);
                    return;
                }
                strcpy(art_ptr->keywords,ptr);
                continue;
            }
        }
	fclose(farticle);
        news_index++;
    }
    

#ifdef UNIX
    idlok(stdscr,TRUE);
#endif /* UNIX */

    showarts(0);
    art_current = 0;

    while (quit == FALSE) {
    ch = getch();
    switch(ch) {
        case 'q':    /* quit */
        case 'Q':
        case '\033':    /* ESCAPE by itself */
        case '\003':    /* for those who like ^C */
        case '\177':    /* for those who like INTR */
            quit = TRUE;
            break;

        case '?':    /* on-line help */
        case 'h':
        case 'H':
        case KEY_F(1):
#ifdef KEY_HELP
        case KEY_HELP:
#endif
            over = newwin(13,61,8,15);
            if (!eepoint) wstandout(over);
            box(over,'\0','\0');
            if (!eepoint) wstandend(over);
            wmove(over,2,5);
            wstandout(over);
            waddstr(over," EEP! v1.8: helpful .newsrc editor ");
            wstandend(over);
            wmove(over,3,5);
            waddstr(over,"       Newsgroup browser submenu");
            wmove(over,5,5);
            waddstr(over,"t   Top of file     b   Bottom of file");
            wmove(over,6,5);
            waddstr(over,"i   Show info       j   Next line");
            wmove(over,7,5);
            waddstr(over,"k   Previous line   p   Pointer change");
            wmove(over,8,5);
            waddstr(over,"q   Quit to main    r   Redraw screen");
            wmove(over,9,5);
            waddstr(over,"^D  Page down       ^U  Page up");
            wmove(over,10,5);
            waddstr(over,"?   This help");
            wmove(over,12,5);
            wstandout(over);
            waddstr(over," Press SPACE BAR to exit from help ");
            wstandend(over);
            wrefresh(over);
            ch = wgetch(over);
            delwin(over);
            touchwin(stdscr);
            refresh();
            break;

        case 'r':    /* redraw */
        case 'R':    /* redraw */
        case '\014':    /* form feed ^L */
#ifdef KEY_REFRESH
        case KEY_REFRESH:
#endif
            touchwin(stdscr);
            clearok(stdscr,TRUE);
            refresh();
            break;

        case 'i':
            if ((art_ptr = article[art_current]) == (struct newsview *)NULL) 
                break;
            over = newwin(18,67,0,2);
            if (!eepoint) wstandout(over);
            box(over,'\0','\0');
            if (!eepoint) wstandend(over);
            wmove(over,2,2);
            wprintw(over,"No. : %ld",art_ptr->artnum);
            wmove(over,3,2);
            wprintw(over,"From: %.57s",art_ptr->from);
            wmove(over,4,2);
            wprintw(over,"Subj: %.57s",art_ptr->subject);
            wmove(over,5,2);
            wprintw(over,"MsID: %.57s",art_ptr->msgid);
            wmove(over,6,2);
            wprintw(over,"Refs: %.57s",art_ptr->references);
            wmove(over,7,2);
            wprintw(over,"Date: %.57s",art_ptr->date);
            wmove(over,8,2);
            wprintw(over,"Keys: %.57s",art_ptr->keywords);
            wmove(over,17,9);
            wstandout(over);
            waddstr(over," Press SPACE BAR to continue ");
            wstandend(over);
            wrefresh(over);
            ch = wgetch(over);
            delwin(over);
            touchwin(stdscr);
            refresh();
            break;


        case 't':
        case 'T':
        case '^':    /* top of list */
#ifdef KEY_HOME
        case KEY_HOME:
#endif
            art_current = 0;
            viewpos = 0;
            showarts(art_current);
            break;

        case 'b':
        case 'B':
        case '$':    /* bottom of list */
#ifdef KEY_SHOME
        case KEY_SHOME:
#endif
            art_current = art_count - 1;
            if (art_current < eeplines - 1) 
                viewpos = art_current;
            else
                viewpos = eeplines - 2;
            showarts(art_current);
            break;


        case ' ':
        case 'j':
        case 'J':    /* join */
        case '\016':    /* for emacs users */
#ifdef KEY_DOWN
        case KEY_DOWN:
#endif
scroll_down:
        /* Don't move if we're at the end. */
            if (art_current == art_count - 1) {
                beep();
                break;
            }
            /* remove highlights by redrawing line */
            art_ptr = article[art_current];
            move(viewpos,0);
            clrtoeol();
            if (!eepoint) standend();
            if ((art_current >= 0) && (art_current < art_count)) {
                printw("  ");
                art_ptr = article[art_current];
                printw("%8ld", art_ptr->artnum);
                move(viewpos,11);
                printw("%.38s", art_ptr->subject);
                move(viewpos,50);
                printw("%.28s", art_ptr->from);
                move(viewpos,0);
            } else clrtoeol();
            art_current++;
            if (++viewpos == eeplines - 1) { /* scroll up */
                move(0,0);
                deleteln();
                move(eeplines - 2,0);
                insertln();
                viewpos = eeplines - 2;
                top++;
            };
            /* Now paint our new position */
            move(viewpos,0);
            clrtoeol();
            art_ptr = article[art_current];
            if (!eepoint) standout();
            if ((art_current >= 0) && (art_current < art_count)) {
                printw("->");
                art_ptr = article[art_current];
                printw("%8ld", art_ptr->artnum);
                move(viewpos,11);
                printw("%.38s", art_ptr->subject);
                move(viewpos,50);
                printw("%.28s", art_ptr->from);
                move(viewpos,0);
            } else clrtoeol();
            if (!eepoint) standend();
            move(viewpos,0);
            refresh();
            break;

        case 'k':
        case 'K':
        case '\010':    /* backspace */
        case '\020':    /* for emacs users */
#ifdef KEY_UP
        case KEY_UP:
#endif
        /* Don't move if we're at the top. */
            if (art_current == 0) {
                beep();
                break;
            }
            move(viewpos,0);
            clrtoeol();
            if (!eepoint) standend();
            if ((art_current >= 0) && (art_current < art_count)) {
                printw("  ");
                art_ptr = article[art_current];
                printw("%8ld", art_ptr->artnum);
                move(viewpos,11);
                printw("%.38s", art_ptr->subject);
                move(viewpos,50);
                printw("%.28s", art_ptr->from);
                move(viewpos,0);
            } else clrtoeol();
            refresh();
            art_current--;
            if (--viewpos == -1) {
                move(eeplines - 2,0);
                deleteln();
                move(0,0);
                insertln();
                refresh();
                move(0,0);
                viewpos = 0;
                if (top > 0) top--;
            }; /* cause scroll */
            move(viewpos,0);
            clrtoeol();
            art_ptr = article[art_current];
            if (!eepoint) standout();
            if ((art_current >= 0) && (art_current < art_count)) {
                printw("->");
                art_ptr = article[art_current];
                printw("%8ld", art_ptr->artnum);
                move(viewpos,11);
                printw("%.38s", art_ptr->subject);
                move(viewpos,50);
                printw("%.28s", art_ptr->from);
                move(viewpos,0);
            } else clrtoeol();
            if (!eepoint) standend();
            move(viewpos,0);
            refresh();
            break;

        case '\004':    /* ^D */
        case '\006':    /* ^F */
#ifdef KEY_NPAGE
        case KEY_NPAGE: /* next page */
#endif

            /* If a pagedown will go past the end
            of the list, simply position at the end. */

            if (art_current == art_count - 1) {
                beep();
                break;
            }
            if ((art_current += eeppage) >= art_count) {
                art_current = art_count - 1;
                if ((art_count - art_current) < (eeplines - 2))
                    viewpos = eeplines - 2;
            } else  if (viewpos + eeppage < eeplines - 2)
                    viewpos += eeppage; 
            showarts(art_current);
            break;

        case '\025':    /* ^U */
        case '\002':    /* ^B */
#ifdef KEY_PPAGE
        case KEY_PPAGE: /* previous page */
#endif
            if (art_current == 0) {
                beep();
                break;
            }
            if ((art_current -= eeppage) < 0) {
                art_current = 0;
                viewpos = 0;
            } else  if ((viewpos - eeppage) >= 0)
                    viewpos -= eeppage;
            showarts(art_current);
            break;

        case 'p':   /* change type of pointer */
        case 'P':
            if (eepoint) eepoint = FALSE;
            else eepoint = TRUE;
            showarts(art_current);
            break;

        }
    }
}

/* showarts() -- show list of articles by number */

void    showarts(art_current)
int    art_current;
{
int    index, counter;

    erase();    /* clear screen */
    counter = 0;
    index = art_current - viewpos;
    while (counter < (eeplines - 1)) {
        move(counter,0);
        if (counter == viewpos) {
            if (!eepoint) standout();
            printw("->");
        } else    printw("  ");
        if ((index >= 0) && (index < art_count)) {
            art_ptr = article[index];
            printw("%8ld", art_ptr->artnum);
            move(counter,11);
            printw("%.38s", art_ptr->subject);
            move(counter,50);
            printw("%.28s", art_ptr->from);
            move(counter,0);
            if ((counter == viewpos) && !eepoint) 
                standend();
        } else clrtoeol();
        index++;
        counter++;
    }
    move(eeplines - 1,0);
    clrtoeol();
    printw("There are %d articles in %s",art_count,dir);
    move(eeplines - 1,60);
    printw("Press ? for Help");
    move(viewpos,0);
    refresh();
}
