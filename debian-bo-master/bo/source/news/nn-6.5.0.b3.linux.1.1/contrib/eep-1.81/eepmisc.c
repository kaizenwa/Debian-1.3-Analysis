/*
 *  eepmisc.c  -- Miscellaneous functions 
 */

#include    <stdio.h>
#include    <ctype.h>
#include    <string.h>
#include    <curses.h>

#ifdef DOS
#include    <dos.h>
#endif /* DOS */

#ifdef UNIX
#include    <ctype.h>
#include    <signal.h>
#endif /* UNIX */

#ifdef ANSI
#include <stdlib.h>
#else
#define void int
#ifndef size_t
#define size_t unsigned
#endif
extern char *malloc();
#endif /* ANSI */

#include    "eep.h"                  /* local changes */

extern int     qcompare();        /* sort alphabetically */
extern void    cleanup();         /* graceful exit */
extern char    *shift_lower();    /* see eepmisc.c */
extern void    showlist();        /* eepmenu.c */
extern char    buffer[],          /* general purpose line buffer */
               tmp[];

extern int  i_active,   /* index into arrays */
    c_active,    /* number of lines in act array */
    t_active,    /* number of lines in act array */
    eepoint,    /* pointer switch */
    noshell;    /* noshell switch */

extern int eeplines;
extern int eepcolumns;
extern int current;
extern int scrnpos;
extern int top;
extern int slider;

/* The levels array contains the Head pointers which control the
linked lists of newsgroups grouped by their "depth" in the
hierarchy, and sorted alphabetically.  E.g. alt, sci, soc are 
level 0, alt.pagan, sci.physics are level 1 and so on. */

extern struct actif *levels[];  /* keep track of levels with this array */
extern struct actif *aptr;    /* temporary pointer */
extern struct  actif *act[];    /* main data structure */
extern struct  actif *topact[]; /* top level names only */

extern int    i_levels;    /* index into array */


WINDOW    *over;    /* used for on-screen help window etc. */
void sleep();    /* forward reference */

/* These next four variables are used by wallop() */

char    *nextchunk;   /* point past most recent allocation */
size_t  measure = 0;  /* how much is left? */
extern char    *buffer_city[];
extern int     malloc_count;


/* wallop() -- wrapper for malloc() with safety and efficiency
 *
 * EEP is characterized by lots of use of malloc(), especially
 * with small (possibly single-byte) sizes.  This can impose
 * overhead on any system.  To make things more efficient, 
 * wallop() is a wrapper for malloc() that will bite off
 * much larger chunks of memory (defined in eep.h) using
 * malloc(), and slice bits off when EEP needs them.
 * The price we pay is slightly less efficient use of
 * memory, as wallop() will not assume that successive
 * calls to malloc() will return contiguous memory addresses.
 *
 * If EEP calls wallop() with a size larger than WALLOP_SIZE,
 * wallop() will simply pass the buck to malloc().
 *
 * As each large buffer is allocated with malloc(), the pointer
 * is stored in buffer_city[] so that they can easily be
 * freed when the program finishes.
 */

char      *wallop(size_wanted)
size_t    size_wanted;
{
char           *walloper;    /* point at start of chunk */

    if (size_wanted <= 0) 
        return((char *)NULL);  /* safety */
    
    /* ensure the next wallop is aligned correctly -- this is
     * necessary otherwise certain machine architectures will
     * core dump if you store a pointer or integer with the
     * wrong byte alignment. */
    while ((size_wanted % ALIGNMENT) != 0) size_wanted++;

    if (size_wanted >= WALLOP_SIZE) {
        walloper = (char *)malloc(size_wanted);
        if (walloper == (char *)NULL) {
            fprintf(stderr,"Fatal error while allocating memory!\n");
            cleanup();
        }
        buffer_city[malloc_count++] = walloper;
        return(walloper);
    }

    if (size_wanted > measure) {   /* need a new malloc() */
        nextchunk = (char *)malloc(WALLOP_SIZE);
        if (nextchunk == (char *)NULL) {
            fprintf(stderr,"Fatal error while allocating memory!\n");
            cleanup();
        }
        buffer_city[malloc_count++] = nextchunk;
        measure = WALLOP_SIZE;
    }

    walloper = nextchunk;
    nextchunk += size_wanted;
    measure  -= size_wanted;
    return(walloper);
}

    
/* getbuf() -- read input into the buffer */

void    getbuf(buf,max)
char    *buf;
int     max;     /* maximum number of characters to get */
{
int    count = 0,
    c;    /* character */

    while ((count < BUFSIZE - 1) &&
           (count < max)) { /* -1 to allow for nul */
        c = getch();
        switch(c) {
        case KEY_BREAK:
        case '\033':
        case '\003':
#ifdef DOS
        case KEY_EXIT:
#endif /* DOS */
            deleteln();
            refresh();
            buf[0] = '\0';
            return;
            break;    /* just in case */
        case KEY_BACKSPACE:
        case '\010':
        case '\177':
        case KEY_LEFT:
            if (count == 0)    continue;
            addstr("\010 \010");
            refresh();
            count--;
            continue;
        case KEY_ENTER:
        case '\n':
        case '\r':
            buf[count] = '\0';
            return;
            break;
        }
        /* yes, this is very ASCII and doesn't take into
        account non-English alphabets.  This is because
        I'm not planning to fix curses in free software. */
        if ((c < '\020') || /* some other control code */
            (c > '\177'))
            continue;    /* ignore it */
        buf[count] = c;
        addch(buf[count]);
        refresh();
        count++;
    }
    buf[count] = '\0';    /* nul terminate the string */
}

/* wrap -- this function will take a string, and word wrap it to
within the defined width by inserting CRLF at appropriate places. 
The result will be output directly.  An offset may be defined, 
which will not be used on the first line only. */

void    wrap(string,width,offset)
char    *string;
int    width,
    offset;
{
static char    temp[80];    /* temporary buffer */
char    *p,*q;    /* traverse the string with this */
int    count;    /* keep track of characters */
int    offcount;
int    first_line;

    temp[0] = '\0';
    if (string == (char *)NULL) return;
    if (width == 0) {
        printf("Can't wrap to zero width, sorry!");
        printf(CRLF);
        return;
    }
    if (strlen(string) == 0) return;
    p = string;
    q = temp;
    count = 0;
    first_line = 0;    /* no offset for first line */
    while (*p != '\000') {
        *q = *p;
        if (count == width) { /* check for wrapping */
            while (*p != ' ') {
                q--;
                p--;
                if (--count == 0) { /* can't wrap! */
                    /* so we truncate */
                    q = temp + width;
                    p = p + width; /* carry on... */
                    break;
                }
            }
            *q = '\0';  /* terminate output here */
            if (first_line != 0) {
                offcount = offset;
                while (offcount-- > 0) printf(" ");
            }
            first_line = 1;
            printf(temp);
            printf(CRLF);
            count = -1;    /* new line */
            q = temp - 1;
        }
        count++;
        p++;
        q++;
    }
    *q = '\0';
    if (first_line != 0) {
        offcount = offset;
        while (offcount-- > 0) printf(" ");
    }
    printf(temp);
    printf(CRLF);
}


/* These next two routines simply lifted from the Elm mail package, 
by Dave Taylor.  If you need a nice mail system, you won't have to 
look much further than Elm!  -- P.G. */

char    *shift_lower(string)
char    *string;
{
    /** return 'string' shifted to lower case.  Do NOT touch the
        actual string handed to us! **/

    static char buf[BUFSIZE];
    register char *bufptr = buf;

        if (string == (char *)NULL) {
            *bufptr = '\0';
            return((char *)buf);
        }

    for (; *string; string++, bufptr++)
      if (isupper(*string))
        *bufptr = tolower(*string);
      else
        *bufptr = *string;
    
    *bufptr = 0;
    
    return( (char *) buf);
}

#ifdef DOS
/*
 *  Here are some timing routines.  Call msleep() with parameter
 *  of 0L to establish precision the first time. 
 */


typedef struct
{ /* time holder */
  int hour;
  int minute;
  int sec;
  int hsec;
} TIME, *TIME_PTR;

#define GET_TIME    0x2c      /* time request       */
#define DOS_INT     0x21      /* int to call DOS    */
#define INIT        60        /* initial clock set  */

/*
 * get_time(n)
 * TIME_PTR n;
 *
 * fills timetype structure n with current time using DOS interrupt 21
 *
 */

get_time(n)
TIME_PTR n;
{
  union REGS inregs;
  union REGS outregs;

  inregs.h.ah = GET_TIME;

  int86(DOS_INT, &inregs, &outregs);

  n->hour =   outregs.h.ch;
  n->minute = outregs.h.cl;
  n->sec  =   outregs.h.dh;
  n->hsec =   outregs.h.dl;

  return(0);
}

/* This will sleep for x seconds. */

void    sleep(x)
int x;
{
  int i;
  unsigned s;
  TIME n;               /* current time record */

  i = 0;
  get_time(&n);
  s = n.sec;

  while (i < x){
    while (s == n.sec)
      get_time(&n);
    s = n.sec;
    ++i;
  }
}

void    msleep(ms)
  long ms;
{ /* sleep for ms miliseconds */
  static long estimate = 20L; /* loops per milisecond */
#define OFFSET 20L
  long loops;
  TIME n1, n2;
  unsigned int mydelay;
  
  /* If the value is 0, then we try to calculate the number of miliseconds
   * that make up a clock tick, then estimate a timing loop that will
   * delay for 1 milisecond.  */

  if (ms == 0L) {

    /* Loop until we see a change in time. */

try_again:
    get_time(&n1);
    for (loops = estimate*10; loops == 0; loops--)
        loops = (loops << 1) / 2; 
    get_time(&n2); 
    printf("Estimating... %ld\n", estimate);
    
    if ((n1.hsec == n2.hsec) && (n1.sec == n2.sec)) {
    estimate += OFFSET;
    goto try_again;
    }

    /* Calculate the difference in hundredths of seconds.  Note
     * that is is possible that n2.sec is less than n1.sec if
     * the minute has just changed, in which case we cheat.
     */
    if (n2.minute != n1.minute) n2.sec = n1.sec + 1;
    mydelay = (100*n2.sec + n2.hsec) - (100*n1.sec + n1.hsec);

    /* this is the first guess */
    estimate = estimate / ((long)mydelay * 10);

    get_time(&n1);
    for (loops = INIT*estimate*10; loops > 0; loops--)
        loops = (loops << 1) / 2;
    get_time(&n2);
    if (n2.minute != n1.minute) n2.sec = n1.sec + 1;
    mydelay = (100*n2.sec + n2.hsec) - (100*n1.sec + n1.hsec);
    if (mydelay == 0) mydelay = INIT;
    estimate = (INIT*estimate) / ((long)mydelay);
    printf("Estimate %ld loops per millisecond\n",estimate);
    for (loops = 10; loops > 0; loops--) {
    printf("Tick... ");
        msleep(1000L);
    }
    printf(" BOOM!\n");
  }
  for (loops = (long)ms*estimate; loops > 0; loops--)
    loops = (loops << 1) / 2; /* do nothing really */
}
#endif /* DOS */

/* These in_string routines borrowed from ELM by Dave Taylor. */

int 
in_string_r(buf, pat, mark)
char *buf, *pat;
int mark;
{
    /* Returns offset into buf iff pat occurs IN ITS ENTIRETY in buf. */

    register int i, j;

    i = mark;
    while (mark >= 0) {
        j = 0;
        while (buf[i++] == pat[j++])
            if (pat[j] == '\0')
                return (mark);
        i = --mark;
    }
    return (-1);
}

int 
in_string(buf, pat, mark)
char *buf, *pat;
int mark;
{
    /* Returns offset into buf iff pat occurs IN ITS ENTIRETY in buf. */

    register int i, j;

    i = mark;
    while (buf[i] != '\0') {
        j = 0;
        while (buf[i++] == pat[j++])
            if (pat[j] == '\0')
                return (i - j);
        i = i - j + 1;
    }
    return (-1);
}


/* Here is an EMACS-style search, kindly contributed to EEP by
 * Vernon C. Hoxie.  Thanks Vern!
 */

#define SEARCHBUF    60
#define REVERSE      0x01
#define FIRST        0x02
#define LEFTWARDS    0x04

void  do_search( d )
unsigned short d;
{
    char c;
    char *p;                       /* pointers into search buffer */
    char *target;
    char search[SEARCHBUF];        /* search string buffer */
    static char saved[SEARCHBUF];  /* old search string buffer */
    int found;                     /* index into string when match found */
    int index;                     /* index into act structure */
    int mark;                      /* index into target array */
    int mode;                      /* steering flags */
    int count = SEARCHBUF;

    move(eeplines - 1, 0);
    deleteln();
    refresh();
    p = search;
    *p = '\0';
    mode = FIRST;
    found = 0;
    mark = 0;
    index = current;
    if ( d == '\022' ) index--;

    while (1) {
        if ( d == '\022' ) mode |= REVERSE;
        else mode &= ~REVERSE;
        move(eeplines - 1, 0);
        printw("%s search: %s", (mode&REVERSE)?"Reverse":"Forward", search);
        clrtoeol();
        move(scrnpos, found);
        refresh();
        c = getch();
        if ( c >= ' ' && c < '\177' ) {
            if ( isupper(c) ) c = tolower(c);
            *p++ = c;
            *p = '\0';
            if ( --count <= 0 ) break;
        }
        else if ( c == '\010' && p != search ) {    /* Backspace */
            *(--p) = '\0';
            count++;
            mode ^= REVERSE;
            if ( mode & REVERSE ) mark--;
            else mark++;
        }
        else if ( c == '\022' || c == '\023' ) {    /* ^R || ^S */
            if ( search[0] == '\0' ) {
                strcpy(search,saved);
            }
            else if ( c != d ) {
                mode ^= REVERSE | FIRST;
                d = c;
            }
            if ( mode & REVERSE ) {
                if ( --mark < 0 ) {
                    index--;
                    mode |= FIRST;
                }
            } 
            else mark++;
        }
        else if ( c == '\033' ) {                /* ESCape */
            fflush( stdin );
            break;
        }

        found = 0;
        do {
            if ((aptr = act[index]) == (struct actif *) NULL) break;
            if ( mode & REVERSE ) {
                if ( mode & FIRST ) {
                    target = aptr->desc;
                    mode &= ~LEFTWARDS;
                }
                else {
                    target = aptr->name;
                    mode |= LEFTWARDS;
                }
                if ( mark < 0 ) mark = strlen(target);
                mark = in_string_r(shift_lower(target), search, mark);
            }
            else {
                if ( mode & FIRST ) {
                    target = aptr->name;
                    mode |= LEFTWARDS;
                }
                else {
                    target = aptr->desc;
                    mode &= ~LEFTWARDS;
                }
                if ( mark < 0 ) mark = 0;
                mark = in_string(shift_lower(target), search, mark);
            }
            if ( mark < 0 ) {
                if ( mode & FIRST ) mode ^= FIRST;
                else {
                    if ( mode & REVERSE ) {
                        if ( --index < 0) index = c_active - 1;
                    }
                    else if ( ++index >= c_active ) index = 0;
                    mode |= FIRST;
                    if ( index == current ) break;
                }
            }
            else {
                if ( mode & LEFTWARDS ) {
                    if (( found = mark + 3 ) > 32 ) found = 32;
                }
                else if (( found = mark + 32 ) > eepcolumns )
                            found = eepcolumns - 1;
            }

        } while ( found < 3 );
        current = index;
        if ( found > 0 ) {
            switch(slider) {
            case LEFT:
                if ( mode & LEFTWARDS ) {
                    if (( found = mark + 3 ) > 17 ) found = 17;
                }
                else if (( found = mark + 18 ) > eepcolumns )
                            found = eepcolumns - 1;
                break;
            case MIDDLE:
                if ( mode & LEFTWARDS ) {
                    if (( found = mark + 3 ) > 31 ) found = 31;
                }
                else if (( found = mark + 32 ) > eepcolumns )
                            found = eepcolumns - 1;
                break;
            case RIGHT:
                if ( mode & LEFTWARDS ) {
                    if (( found = mark + 3 ) > 41 ) found = 41;
                }
                else if (( found = mark + 42 ) > eepcolumns )
                            found = eepcolumns - 1;
                break;
            }
            showlist(current,found);
        }
        else {
            beep();
            move(eeplines - 1, 0);
            printw("Search failed");
            clrtoeol();
            refresh();
            sleep(1);
        }
    }
    strcpy(saved,search);
    move(eeplines - 1, 0);
    deleteln();
    showlist(current,0);
    refresh();
    return;
}

