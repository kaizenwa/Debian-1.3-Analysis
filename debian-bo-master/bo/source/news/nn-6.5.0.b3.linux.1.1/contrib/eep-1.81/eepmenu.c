/*  This module contains the code for the user interface for eep.  */

#include    <stdio.h>
#include    <string.h>
#include    <curses.h>

#ifdef UNIX
#include    <ctype.h>
#include    <signal.h>
#include    <fcntl.h>
/* #include    <termio.h>	not needed with curses  -sp */
#endif /* UNIX */

#ifdef ANSI
#include <stdlib.h>
#else
#define void int
extern char *malloc();
#endif /* ANSI */

#include    "eep.h"          /* local changes */

extern int    qcompare();    /* sort alphabetically */
extern int    icompare();    /* sort by index */
extern int    scompare();    /* sort by subscription status */
extern char   *shift_lower();    /* see eepmisc.c */
extern char   *wallop();     /* see eepmisc.c */
extern char   buffer[],      /* general purpose line buffer */
              tmp[];

extern struct  actif *act[];    /* main data structure */
extern struct  actif *topact[]; /* top level names only */

extern int    i_active,    /* index into arrays */
              c_active,    /* number of lines in act array */
              t_active,    /* number of lines in act array */
              eepoint,     /* pointer switch */
              bog_count,   /* bogus newsgroups */
              sub_count,   /* subscribed newsgroups */
              noshell;     /* noshell switch */

extern char   *home;       /* HOME directory */
extern FILE   *fnewsrc;    /* .newsrc.eep */

/* The levels array contains the Head pointers which control the
linked lists of newsgroups grouped by their "depth" in the
hierarchy, and sorted alphabetically.  E.g. alt, sci, soc are 
level 0, alt.pagan, sci.physics are level 1 and so on. */

extern struct actif *levels[];  /* keep track of levels with this array */
extern struct actif *aptr;    /* temporary pointer */
extern int    i_levels;    /* index into array */

/* More Global variables */

int    scrnpos = 0;            /* position of highlight on screen */
int    current = 0;            /* matching highlight in data structure */
int    alphabetized = FALSE;   /* flag showing if sorted */
int    top = 0;                /* data element on top of screen */
int    direction = 0;          /* 0 means down, 1 means up */
int    eeplines = 0;           /* number of lines on screen */
int    eepcolumns = 0;         /* columns on screen */
int    eeppage = EEPPAGE;      /* page size for scrolling */
int    desc_changed = FALSE;   /* has a description changed? */
int    slider = MIDDLE;        /* pan left or right */

/* Define two buffers: tbuf and tbufsave.  tbuf is used for changing
the keyboard modes, while tbufsave is used to restore them. */

struct termio tbuf;
struct termio tbufsave;

WINDOW    *over;        /* used for on-screen help window etc. */
void      showhelp();   /* forward declaration */
void      showlist();

/* pad() -- this function will take a pointer to a string and a 
numeric argument, and will pad the string with spaces to reach
the desired length.  If the string is too long, it will be 
truncated to fit.  The function will return a pointer to a
static buffer that will contain the padded
string (or original if it's long enough already). 
If str is a NULL pointer, we'll return spaces only. */

char    *pad(str,len)
char    *str;
int    len;
{
static char mybuf[BUFSIZE];

int    count;
    mybuf[0] = '\0';
    if (len >= BUFSIZE) return(mybuf); /* safety */
    /* In case we are passed a NULL pointer... */
    if (str == (char *) NULL) {
        mybuf[0] = '\0';
/*        while (strlen(mybuf) < len) strcat(mybuf," "); */
        count = 0;
        while (count++ < len) strcat(mybuf," ");
        return(mybuf);
    }
    if (strlen(str) >= len) {
        strncpy(mybuf,str,len);
        return(mybuf);
    }
    strcpy(mybuf,str);
/*    while (strlen(mybuf) < len) */
    count = len;
    while (count-- > 0)
        strcat(mybuf," ");
    return(mybuf);
}

/* newsmain()  -- this provides the mainline user interface. */

void    newsmain()
{

char    *ptr;
char    search[BUFSIZE];    /* search string buffer */
int     found = FALSE;      /* flag to say we've found something */
int     quit = FALSE;       /* flag used when ready to quit */
int     index;              /* used when searching */
int     counter;            /* used when counting */
int     ch;                 /* input character */

    initscr();    /* set up for curses */
    if ((ptr = (char *)getenv("LINES")) != (char *)NULL)
        eeplines = atoi(ptr);
    if ((ptr = (char *)getenv("COLUMNS")) != (char *)NULL)
        eepcolumns = atoi(ptr);

    if (eeplines <= 0)
        eeplines = LINES;
    if (eepcolumns <= 0)
        eepcolumns = COLS;
    if (eeplines <= 0)
        eeplines = EEPLINES;
    if (eepcolumns <= 0)
        eepcolumns = EEPCOLUMNS;
    if ((eeplines < 19) || (eepcolumns < 80)) {
        erase();
        refresh();
        endwin();    /* terminate */
        printf("\nSorry, EEP needs at least 19 lines by 80 columns.\n");
        exit(1);
    }
    eeppage = eeplines - 2;

    raw();
    noecho();
    nonl();
    keypad(stdscr,TRUE);
    erase();    /* clear screen */

#ifdef UNIX
    idlok(stdscr,TRUE);
    /* POSIX: tcgetattr(0, &tbuf); */
    if (ioctl(0, TCGETA, &tbuf) < 0) {
        fprintf(stderr,"Stdin must be a terminal\n");
        exit(1);
    }
    tbufsave = tbuf; /* save the old settings */
    tbuf.c_iflag &= ~(IXON | IXOFF); /* disable ^S & ^Q */
    ioctl(0, TCSETAF, &tbuf); /* POSIX: tcsetattr(0, X???WAIT, &tbuf); */
#endif /* UNIX */

    showlist(0,0);
    while (quit == FALSE) {
    ch = getch();
    switch(ch) {
        case 'q':    /* quit */
        case 'Q':
        case '\033':    /* ESCAPE by itself */
        case '\003':    /* for those who like ^C */
        case '\177':    /* for those who like INTR */
#ifdef KEY_ABORT
        case KEY_ABORT:
#endif
            move(eeplines-1,0);
            deleteln();
            addstr("Do you want to quit without saving your changes? [n]: ");
            refresh();
            if (tolower(getch()) == (int) 'y') quit = TRUE;
            deleteln();
            move(scrnpos,0);
            refresh();

	    /* remember to close & remove the temp file!  -sp */
            fclose(fnewsrc);
#ifdef UNIX
            if (home != (char *)NULL)
                sprintf(buffer, "%s/.newsrc.eep", home);
            else
                sprintf(buffer, ".newsrc.eep");
            if (unlink(buffer) < 0) {
                fprintf(stderr,"err: .newsrc.eep not removed.");
                break;
            }
#endif
            break;

        case '?':    /* on-line help */
        case 'h':
        case 'H':
        case KEY_F(1):
#ifdef KEY_HELP
        case KEY_HELP:
#endif
            over = newwin(22,60,1,15);
            if (!eepoint) wstandout(over);
            box(over,'\0','\0');
            if (!eepoint) wstandend(over);
            wmove(over,2,5);
            wstandout(over);
            waddstr(over," EEP! v1.8: helpful .newsrc editor ");
            wstandend(over);
            wmove(over,3,5);
            waddstr(over,"by Paul Gillingwater, paul@actrix.co.at");
            wmove(over,5,5);
            waddstr(over,":   Pick by number    /   Search");
            wmove(over,6,5);
            waddstr(over,"a   (un)Alphabetise   b   Bottom of file");
            wmove(over,7,5);
            waddstr(over,"c   Catch up          d   (un)Delete ");
            wmove(over,8,5);
            waddstr(over,"e   Edit description ");
            wmove(over,9,5);
            waddstr(over,"i   Show info         j   Next line");
            wmove(over,10,5);
            waddstr(over,"k   Previous line     n   Search next");
            wmove(over,11,5);
            waddstr(over,"o   Over-subscribe    p   Pointer change");
            wmove(over,12,5);
            waddstr(over,"q   Quit, no save     r   Redraw screen");
            wmove(over,13,5);
            waddstr(over,"s   Subscribe         t   Top of file");
            wmove(over,14,5);
            waddstr(over,"u   Unsubscribe       v   View subjects");
            wmove(over,15,5);
            waddstr(over,"x   Save and exit ");
            wmove(over,16,5);
            waddstr(over,"^D  Page down         ^U  Page up");
            wmove(over,17,5);
            waddstr(over,"^S  EMACS Search      ^R  Reverse search");
            wmove(over,18,5);
            waddstr(over,".   (un)Mark group    m   Move marked groups");
            sprintf(buffer,"There are %d subscribed newsgroups.",sub_count);
            wmove(over,20,5);
            waddstr(over,buffer);
            wmove(over,21,5);
            wstandout(over);
            waddstr(over," Press SPACE BAR to exit from help ");
            wstandend(over);
            wrefresh(over);
            ch = wgetch(over);
            delwin(over);
            touchwin(stdscr);
            refresh();
            break;

        case '1':
        case '<':	/* manpage indicates this should be here  -sp */
#ifdef KEY_LEFT
        case KEY_LEFT:
#endif
            switch(slider) {
            case MIDDLE:
                slider = LEFT;
                showlist(current,0);
                break;
            case RIGHT:
                slider = MIDDLE;
                showlist(current,0);
                break;
            }
            break;

        case '2':
        case '>':	/* manpage indicates this should be here  -sp */
#ifdef KEY_RIGHT
        case KEY_RIGHT:
#endif
            switch(slider) {
            case MIDDLE:
                slider = RIGHT;
                showlist(current,0);
                break;
            case LEFT:
                slider = MIDDLE;
                showlist(current,0);
                break;
            }
            break;

        case 'e':    /* edit newsgroup description */
        case 'E':
#ifdef KEY_EDIT
        case KEY_EDIT:
#endif
            aptr = act[current];
            if (aptr == (struct actif *)NULL) break;
            move(eeplines - 1,0);
            deleteln();
            printw("Enter new description: ");
            refresh();
            getbuf(buffer,60);
            if (strlen(buffer) > 0) {
                ptr = (char *)wallop(strlen(buffer)+1);
                if (ptr == (char *)NULL) {
                    fprintf(stderr, "Fatal memory allocation error.\n");
                    cleanup();
                }
                strcpy(ptr,buffer);
                aptr->desc = ptr;
                desc_changed = TRUE;
            }
            showlist(current,0);
            break;

        case ':':    /* enter number */
            move(eeplines - 1,0);
            deleteln();
            addch(':');
            refresh();
            getbuf(buffer,10);
            index = atoi(buffer);
            if ((index <= 0) || (index > c_active)) {
                move(eeplines - 1,0);
                printw("Newsgroups available: %d ",c_active);
                refresh();
                sleep(2);
                deleteln();
                move(scrnpos,0);
                refresh();
                break;
            }
            current = index - 1;
            showlist(current,0);
            break;

        case '.':    /* mark group -- used to move later */
#ifdef KEY_MOVE
        case KEY_MOVE:
#endif
            aptr = act[current];
            if (aptr->mark == 0)
                aptr->mark = 1;
            else
                aptr->mark = 0;
            if (direction == 0)
                goto    scroll_down;
            else
                goto    scroll_up;
            break;

        case 'm':    /* move marked groups */
        case 'M':
#ifdef KEY_SMOVE
        case KEY_SMOVE:
#endif
            i_active = 0;
            while (i_active < c_active) {
                if ((aptr = act[i_active]) == (struct actif *)NULL) {
                    i_active++;
                    continue;
                }
                if (aptr->mark == 0) {
                    i_active++;
                    continue;
                }
                aptr->index = current;
                aptr->mark = 0;
                i_active++;
            }
            qsort( act, (unsigned) c_active, 
                    (unsigned) sizeof(act[0]), icompare);
            alphabetized = FALSE;

            /* Renumber to match current order. */
            i_active = 0;
            while (i_active < c_active) {
                if ((aptr = act[i_active]) == (struct actif *)NULL) {
                    i_active++;
                    continue;
                }
                aptr->index = ++i_active;
            }
            showlist(current,0);
            break;

        case 'r':    /* redraw */
        case 'R':    /* redraw */
        case '\014':    /* form feed ^L */
#ifdef KEY_REFRESH	/* : after identifier caused warning  -sp */
        case KEY_REFRESH:
#endif
            touchwin(stdscr);
            clearok(stdscr,TRUE);
            refresh();
            break;

        case 'v':    /* view newsgroup */
        case '=':    /* view newsgroup */
        case '\r':
        case '\n':    /* new line to select a group */
            if ((aptr = act[current]) == (struct actif *)NULL) 
                break;
            eepview(aptr->name);
            showlist(current,0);
            break;

        case 'i':
            if ((aptr = act[current]) == (struct actif *)NULL) 
                break;
            over = newwin(18,65,0,5);
            if (!eepoint) wstandout(over);
            box(over,'\0','\0');
            if (!eepoint) wstandend(over);
            wmove(over,2,5);
            wprintw(over,"Number: %d",current + 1);
            wmove(over,3,5);
            wprintw(over,"Name: %.50s",pad(aptr->name,50));
            wmove(over,4,5);
            wprintw(over,"Desc: %.50s",pad(aptr->desc,50));
            wmove(over,6,5);
            wprintw(over,"This news group is ");
            switch(aptr->flag) {
            case 'y':
                wprintw(over,"Active.");
                break;
            case 'n':
                wprintw(over,"Not Active.");
                break;
            case 'm':
                wprintw(over,"Moderated.");
                break;
            default:
                wprintw(over,"Invalid.");
            }
            wmove(over,7,5);
            wprintw(over,"Your .newsrc says this is ");
            switch(aptr->status) {
            case ':':
                wprintw(over,"Subscribed to.");
                break;
            case '!':
                wprintw(over,"Not Subscribed to.");
                break;
            default:
                wprintw(over,"Not valid.");
                break;
            }
            if (aptr->hi > 0) {
                wmove(over,9,5);
                wprintw(over,"Active high message is %ld",
                    aptr->hi);
                wmove(over,11,5);
                wprintw(over,"Messages already read by you include:");
                wmove(over,12,5);
                wprintw(over,"%s",pad(aptr->hilo,50));
            }
            wmove(over,17,5);
            wstandout(over);
            waddstr(over," Press SPACE BAR to continue ");
            wstandend(over);
            wrefresh(over);
            ch = wgetch(over);
            delwin(over);
            touchwin(stdscr);
            refresh();
            break;

        case '/':    /* search */
        case 'n':    /* search for next occurence */
        case 'N':
#ifdef KEY_FIND
        case KEY_FIND:
            if (ch == KEY_FIND) ch = '/';
#endif
            if (ch == '/') {
                move(eeplines - 1,0);
                deleteln();
                addch('/');
                refresh();
                getbuf(tmp,60);
                if (strlen(tmp) == 0) {
                    if (!found) {
                        beep();   /* nothing to find */
                        deleteln();
                        move(scrnpos,0);
                        refresh();
                        break;
                    }
                } else strcpy(search,tmp);
            }
            if ((ch == 'n') || (ch == 'N')) {
                if (!found) {
                    beep();   /* nothing to find */
                    move(scrnpos,0);
                    refresh();
                    break;
                }
            }
            found = FALSE;
            /* make it lower case for searching */
            strcpy(search,shift_lower(search));
            if (direction == 0) { /* search downwards */
                index = current + 1;    /* track progress */
                if (index >= c_active) index = 0;
              
                while (index != current) {
                    aptr = act[index];
                    if (aptr != (struct actif *)NULL) {
                        if (in_string(shift_lower(aptr->name),search,0) >= 0 ||
                            in_string(shift_lower(aptr->desc),search,0) >= 0) {
                            /* found match */
                            found = TRUE;
                            current = index;
                            showlist(current,0);
                            break;
                        }
                    }
                    index++;
                    if (index >= c_active) index = 0;
                }
            } else {
                index = current - 1;    /* track progress */
                if (index < 0) index = c_active - 1;
              
                while (index != current) {
                    aptr = act[index];
                    if (aptr != (struct actif *)NULL) {
                        if (in_string(shift_lower(aptr->name),search,0) >= 0 ||
                            in_string(shift_lower(aptr->desc),search,0) >= 0) {
                            /* found match */
                            found = TRUE;
                            current = index;
                            showlist(current,0);
                            break;
                        }
                    }
                    index--;
                    if (index < 0) index = c_active - 1;
                }
            }
            if (!found) {
                beep();
                move(scrnpos,0);
                refresh();
            }
            break;

        /* EMACS style searching */

        case '\022':   /* ^R */
        case '\023':   /* ^S */
#ifdef KEY_SFIND
        case KEY_SFIND:
            if (ch == KEY_SFIND) ch = '\022';
#endif
            do_search(ch);       /* eepmisc.c */
            break;

        case 't':
        case 'T':
        case '^':    /* top of list */
#ifdef KEY_HOME
        case KEY_HOME:
#endif
#ifdef KEY_BEG
        case KEY_BEG:
#endif
            current = 0;
            scrnpos = 0;
            showlist(current,0);
            break;

        case 'b':
        case 'B':
        case '$':    /* bottom of list */
#ifdef KEY_SHOME
        case KEY_SHOME:
#endif
#ifdef KEY_END
        case KEY_END:
#endif
            current = c_active - 1;
            scrnpos = eeplines - 2;
            showlist(current,0);
            break;

        case 'a':    /* change sorting order */
        case 'A':
            move(eeplines-1,0);
            deleteln();
            if (alphabetized) {
                addstr("Do you wish to sort in .newsrc order? [n]: ");
                refresh();
                if (tolower(getch()) != (int) 'y') {
                    deleteln();
                    move(scrnpos,0);
                    refresh();
                    break;
                }
                qsort( act, (unsigned) c_active, 
                    (unsigned) sizeof(act[0]), icompare);
                alphabetized = FALSE;
            } else {
                addstr("Do you wish to sort alphabetically? [n]: ");
                refresh();
                if (tolower(getch()) != (int) 'y') {
                    deleteln();
                    move(scrnpos,0);
                    refresh();
                    break;
                }
                qsort( act, (unsigned) c_active, 
                    (unsigned) sizeof(act[0]), qcompare);
                alphabetized = TRUE;
            }
            deleteln();
            move(scrnpos,0);
            showlist(current,0);
            break;

        case 'o':
        case 'O':    /* order (arrange subscribed groups at top */
            move(eeplines-1,0);
            deleteln();
            addstr("Move all subscribed groups to top? [n]: ");
            refresh();
            if (tolower(getch()) == (int) 'y') {
                qsort( act, (unsigned) c_active, 
                    (unsigned) sizeof(act[0]), scompare);
                alphabetized = FALSE;
                deleteln();
                showlist(current,0);
                break;
            }
            deleteln();
            move(scrnpos,0);
            refresh();
            break;

        case '+':    /* subscribe to this newsgroup */
        case 's':
        case 'S':
#ifdef KEY_MARK
        case KEY_MARK:
#endif
            aptr = act[current];
            /* check if group is valid */
            if ((aptr->flag == 'y') ||
                (aptr->flag == 'm')) {
                aptr->status = ':';
                sub_count++;
                move(scrnpos,2);
                standout();
                addch('+');
                standend();
                move(scrnpos,0);
            } else {
                move(eeplines - 1,0);
                printw("Not a valid newsgroup -- cannot subscribe");
                refresh();
                sleep(2);
                deleteln();
                move(scrnpos,0);
            }
            refresh();
            if (direction == 0)
                goto    scroll_down;
            else
                goto    scroll_up;
            break;

        case '-':    /* unsubscribe to this newsgroup */
        case 'u':    /* also means unjoin */
        case 'U':
#ifdef KEY_SMARK
        case KEY_SMARK:
#endif
            aptr = act[current];
            /* check if group is valid */
            aptr->status = '!';
            if (sub_count > 0) sub_count--;
            if ((aptr->flag == 'y') ||
                (aptr->flag == 'm')) {
                move(scrnpos,2);
                standout();
                addch(' ');
                standend();
                move(scrnpos,0);
            } else {
                move(eeplines - 1,0);
                printw("Not a valid newsgroup -- unsubscribing anyway");
                refresh();
                sleep(2);
                deleteln();
                move(scrnpos,0);
            }
            refresh();
            if (direction == 0)
                goto    scroll_down;
            else
                goto    scroll_up;
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
            if (current == c_active - 1) {
                beep();
                break;
            }
            direction = 0;
            /* remove highlights by redrawing line */
            aptr = act[current];
            move(scrnpos,0);
            if (!eepoint) standend();
            if (aptr->mark == 0)
                printw("  ");
            else
                printw("* ");
            switch(aptr->status) {
            case ':':    printw("+"); /* subscribed */
                    break;
            case '!':    printw(" "); /* unsubscribed */
                    break;
            default:     printw("?"); /* mystery! */
            }
            if (!eepoint) {
                switch(slider) {
                case LEFT:
                    printw("%.14s ", pad(aptr->name, 14));
                    printw("%.59s ", pad(aptr->desc, 59));
                    break;
                case MIDDLE:
                    printw("%.28s ", pad(aptr->name, 28));
                    printw("%.46s ", pad(aptr->desc, 46));
                    break;
                case RIGHT:
                    printw("%.38s ", pad(aptr->name, 38));
                    printw("%.36s ", pad(aptr->desc, 36));
                    break;
                }
            }
            refresh();
            current++;
            if (++scrnpos == eeplines - 1) { /* scroll up */
                move(0,0);
                deleteln();
                move(eeplines - 2,0);
                insertln();
                refresh();
                scrnpos = eeplines - 2;
                top++;
            };
            /* Now paint our new position */
            move(scrnpos,0);
            aptr = act[current];
            if (!eepoint) standout();
            if (aptr->mark == 0)
                printw("-");
            else
                printw("*");
            switch(aptr->status) {
            case ':':    printw(">+"); /* subscribed */
                    break;
            case '!':    printw("> "); /* unsubscribed */
                    break;
            default:    printw(">?"); /* mystery! */
            }
            switch(slider) {
            case LEFT:
                printw("%.14s ", pad(aptr->name, 14));
                printw("%.59s ", pad(aptr->desc, 59));
                break;
            case MIDDLE:
                printw("%.28s ", pad(aptr->name, 28));
                printw("%.46s ", pad(aptr->desc, 46));
                break;
            case RIGHT:
                printw("%.38s ", pad(aptr->name, 38));
                printw("%.36s ", pad(aptr->desc, 36));
                break;
            }
            if (!eepoint) standend();
            move(scrnpos,0);
            refresh();
            break;

        case 'k':
        case 'K':
        case '\010':    /* backspace */
        case '\020':    /* for emacs users */
#ifdef KEY_UP
        case KEY_UP:
#endif
scroll_up:
        /* Don't move if we're at the top. */
            if (current == 0) {
                beep();
                break;
            }
            direction = 1; /* now going up */
            move(scrnpos,0);
            aptr = act[current];
            if (!eepoint) standend();
            if (aptr->mark == 0)
                printw("  ");
            else
                printw("* ");
            switch(aptr->status) {
            case ':':    printw("+"); /* subscribed */
                    break;
            case '!':    printw(" "); /* unsubscribed */
                    break;
            default:    printw("?"); /* mystery! */
            }
            if (!eepoint) {
                switch(slider) {
                case LEFT:
                    printw("%.14s ", pad(aptr->name, 14));
                    printw("%.59s ", pad(aptr->desc, 59));
                    break;
                case MIDDLE:
                    printw("%.28s ", pad(aptr->name, 28));
                    printw("%.46s ", pad(aptr->desc, 46));
                    break;
                case RIGHT:
                    printw("%.38s ", pad(aptr->name, 38));
                    printw("%.36s ", pad(aptr->desc, 36));
                    break;
                }
            }
            move(scrnpos,0);
            refresh();
            current--;
            if (--scrnpos == -1) {
                move(eeplines - 2,0);
                deleteln();
                move(0,0);
                insertln();
                refresh();
                move(0,0);
                scrnpos = 0;
                if (top > 0) top--;
            }; /* cause scroll */
            move(scrnpos,0);
            aptr = act[current];
            if (!eepoint) standout();
            if (aptr->mark == 0)
                printw("-");
            else
                printw("*");
            switch(aptr->status) {
            case ':':    printw(">+"); /* subscribed */
                    break;
            case '!':    printw("> "); /* unsubscribed */
                    break;
            default:     printw(">?"); /* mystery! */
            }
            switch(slider) {
            case LEFT:
                printw("%.14s ", pad(aptr->name, 14));
                printw("%.59s ", pad(aptr->desc, 59));
                break;
            case MIDDLE:
                printw("%.28s ", pad(aptr->name, 28));
                printw("%.46s ", pad(aptr->desc, 46));
                break;
            case RIGHT:
                printw("%.38s ", pad(aptr->name, 38));
                printw("%.36s ", pad(aptr->desc, 36));
                break;
            }
            if (!eepoint) standend();
            move(scrnpos,0);
            refresh();
            break;


        case '\004':    /* ^D */
        case '\006':    /* ^F */
#ifdef KEY_NPAGE
        case KEY_NPAGE: /* next page */
#endif
            /* If a pagedown will go past the end
            of the list, simply position at the end. */

            if (current == c_active - 1) {
                beep();
                break;
            }
            direction = 0;
            if ((current += eeppage) >= c_active) {
                current = c_active - 1;
                if ((c_active - current) < (eeplines - 2))
                    scrnpos = eeplines - 2;
            } else  if (scrnpos + eeppage < eeplines - 2)
                    scrnpos += eeppage; 
            showlist(current,0);
            break;

        case '\025':    /* ^U */
        case '\002':    /* ^B */
#ifdef KEY_PPAGE
        case KEY_PPAGE: /* previous page */
#endif
            if (current == 0) {
                beep();
                break;
            }
            direction = 1;
            if ((current -= eeppage) < 0) {
                current = 0;
                scrnpos = 0;
            } else  if ((scrnpos - eeppage) >= 0)
                    scrnpos -= eeppage;
            showlist(current,0);
            break;

        case 'p':   /* change type of pointer */
        case 'P':
            if (eepoint) eepoint = FALSE;
            else eepoint = TRUE;
            showlist(current,0);
            break;

        case 'c':   /* catch up */
        case 'C':
            aptr = act[current];
            /* only if it's a real news group */
            if (aptr->hi <= 0) {
                if (direction == 0)
                    goto    scroll_down;
                else
                    goto    scroll_up;
            }
            if ((aptr->flag == 'y') ||
                (aptr->flag == 'm') ||
                (aptr->flag == 'n')) {
                move(eeplines-1,0);
                deleteln();
                addstr("Catch up this news group? [n]: ");
                refresh();
                if (tolower(getch()) == 'y') {
                    sprintf(buffer,"1-%ld", aptr->hi);
                    if (aptr->hilo != (char *)NULL) {
                        if (aptr->hilo[0] == '0')
                            sprintf(buffer,"0-%ld", aptr->hi);
                    }
                    aptr->hilo = (char *)wallop(strlen(buffer)+1);
                    if (aptr->hilo == (char *)NULL) {
                        fprintf(stderr, "Fatal memory allocation error.\n");
                        exit(2);
                    }
                    strcpy(aptr->hilo,buffer);
                    deleteln();
                    move(eeplines-1,0);
                    printw("High message now %ld", aptr->hi);
                } else deleteln();
                move(scrnpos,0);
                refresh();
            }
            if (direction == 0)
                goto    scroll_down;
            else
                goto    scroll_up;
            break;

        case 'd':    /* delete from .newsrc */
        case 'D':
#ifdef KEY_DL
        case KEY_DL:
#endif
            move(eeplines-1,0);
            deleteln();
            aptr = act[current];
            if (aptr->status == 0) {
                printw("Group %s already marked for deletion.  Undelete? ",
                    aptr->name);
                refresh();
                if (tolower(getch()) == (int) 'y') {
                    move(eeplines-1,0);
                    deleteln();
                    aptr->status = '!';
                    printw("Group %s undeleted.",
                        aptr->name);
                } else {
                    move(eeplines-1,0);
                    deleteln();
                    printw("Group %s marked for deletion.",
                        aptr->name);
                }
                move(scrnpos,0);
                refresh();
                if (direction == 0)
                    goto    scroll_down;
                else
                    goto    scroll_up;
                break;
            }
            printw("Delete newsgroup %s? ",aptr->name);
            refresh();
            if (tolower(getch()) == (int) 'y') {
                aptr = act[current];
                aptr->status = 0;
                deleteln();
                move(eeplines-1,0);
                printw("Group %s marked for deletion.",
                    aptr->name);
            } else {
                deleteln();
                move(eeplines-1,0);
                printw("Group %s NOT marked for deletion.",
                    aptr->name);
            }
            move(scrnpos,0);
            refresh();
            if (direction == 0)
                goto    scroll_down;
            else
                goto    scroll_up;
            break;

        case 'x':   /* write the local .newsrc and exit */
        case 'X':
#ifdef KEY_EXIT
        case KEY_EXIT:
#endif /* KEY_EXIT */
            move(eeplines-1,0);
            deleteln();
            if (fnewsrc == (FILE *) NULL) {
                addstr("Couldn't open .newsrc.eep -- write failed.");
                refresh();
                break;
            }
            addstr("Do you want to save and exit? [n]: ");
            refresh();
            if (tolower(getch()) != (int) 'y') {
                deleteln();
                move(scrnpos,0);
                refresh();
                break;
            }
            if (bog_count > 0) {
                move(eeplines-1,0);
                deleteln();
                addstr("Delete bogus groups from your .newsrc? [n]: ");
                refresh();
                if (tolower(getch()) != (int) 'y') bog_count = 0;
            }
            deleteln();
            index = 0;
            counter = 0;
            while (index < c_active) {
                aptr = act[index];
                if ((aptr->flag == '\0') ||
                    (aptr->status == '\0') ||
                    (aptr->name == (char *)NULL)) {
            /* must be bogus -- shall we forget it? */
                    if (bog_count != 0) {
                        index++;
                        continue;
                    }
                }
#ifdef SUBSONLY	/* don't write unsubs back into file  -sp */
                if (aptr->status != ':') {
		    index++;
		    continue;
                }
#endif	/* SUBSONLY */

                fprintf(fnewsrc,"%s",
                    aptr->name);
                if (aptr->status == ':') {
                    fprintf(fnewsrc,": ");
                } else {
                    fprintf(fnewsrc,"! ");
                }
                if (aptr->hilo != (char *)NULL) {
                    fprintf(fnewsrc,"%s", aptr->hilo);
                }
                fprintf(fnewsrc,"\n");
                counter++;
                index++;
                if (counter % 100 == 0) {
                    move(eeplines-1,0);
                    printw("%d",counter);
                    refresh();
                }
            }
            fclose(fnewsrc);

            printf("\rTotal of %d lines written.\r\n",counter);
            if (bog_count > 0) {
                if (bog_count == 1)
                    printf("There was 1 bogus newsgroup deleted.\r\n");
                else
                    printf("There were %d bogus newsgroups deleted.\r\n",
                        bog_count);
            }

            fnewsrc = (FILE *) NULL;
            if (home != (char *)NULL) {
                sprintf(tmp, "%s/%s", home, NEWSRC);
                sprintf(buffer, "%s/%s", home, ONEWSRC);	/* -sp */
            } else {
                sprintf(tmp, NEWSRC);
                sprintf(buffer, ONEWSRC);	/* -sp */
            }
#ifdef UNIX

/* Narrative: we rename the current .newsrc to .newsrc.old -- but
 * first we unlink any existing .newsrc.old.  We have just written
 * the modified .newsrc to .newsrc.new, and now we rename it to
 * replace the .newsrc -- which should be safely backed-up.
 */

            unlink(buffer);    /* don't care about errors */

            if (link(tmp,buffer) < 0) {
                fprintf(stderr,"[1] .newsrc not replaced.");
                break;
            }
            if (unlink(tmp) < 0) {
                fprintf(stderr,"[2] .newsrc not replaced.");
                break;
            }
            if (home != (char *)NULL)
                sprintf(buffer, "%s/.newsrc.eep", home);
            else
                sprintf(buffer, ".newsrc.eep");

            if (link(buffer,tmp) < 0) {
                fprintf(stderr,"[3] .newsrc not replaced.");
                break;
            }
            if (unlink(buffer) < 0) {
		/* give this message as .newsrc IS replaced by now  -sp */
                fprintf(stderr,"[4] .newsrc.eep not removed.");
                break;
            }
#endif /* UNIX */
            noraw();
            return;
            break;
        }
    }
    move(eeplines - 1,0);
    clrtoeol();
    refresh();
    noraw();
}

/*
 * showlist() -- display list of newsgroups
 *
 * This routine will show the window that opens into the current
 * list of newsgroup lines.  Current is the one that will be
 * highlighted.  The global variable scrnpos is used to indicate
 * where this line is on the screen, with 0 being at the top line.
 * 
 * The intention of this routine is that it should be used whenever
 * there is a major change to the cursor position within the list.
 * This may be the result of a search, or jumping to the top or
 * bottom of the file, or using page up or page down keys.  The
 * desired behaviour is that the highlighted line should stay
 * roughly in the same position on the screen when this move occurs.
 * We can calculate the relative position of the current line by
 * subtracting the scrnpos from current to begin the index.
 */

void 
showlist(current,flag)
int current;
int flag;
{
    int index, counter;
    int scrn;

    scrn = eeplines - 1;
    if ( flag > 0 ) {
        if (current + scrn > c_active) {
            top = c_active - scrn;
        } else if ( current < top || current >= top + scrn ) {
            top = current - scrn / 2;
        }
        if ( top < 0 ) top = 0;
        scrnpos = current - top;
    }
    index = current - scrnpos;
    counter = 0;
    while (counter < scrn) {
        move(counter, 0);
        if ((index >= 0) && (index < c_active)) {
            if (counter == scrnpos) {
                if (!eepoint)
                    standout();
                if (aptr->mark == 0)
                    printw("->");
                else
                    printw("*>");
            } else if (aptr->mark == 0)
                printw("  ");
            else
                printw("* ");
            aptr = act[index];
                /* Show whether subscribed or not */
            switch (aptr->status) {
            case ':':
                printw("+");          /* subscribed */
                break;
            case '!':
                printw(" ");          /* unsubscribed */
                break;
            default:
                printw("?");          /* mystery! */
            }

            switch(slider) {
            case LEFT:
                printw("%.14s ", pad(aptr->name, 14));
                printw("%.59s ", pad(aptr->desc, 59));
                break;
            case MIDDLE:
                printw("%.28s ", pad(aptr->name, 28));
                printw("%.46s ", pad(aptr->desc, 46));
                break;
            case RIGHT:
                printw("%.38s ", pad(aptr->name, 38));
                printw("%.36s ", pad(aptr->desc, 36));
                break;
            }
            move(counter, 0);
            if ((counter == scrnpos) && !eepoint)
                standend();
        } else
            printw("%.79s", pad((char *) NULL, 79));
        index++;
        counter++;
    }
    if ( flag == 0 ) {
        move(scrn, 0);
        deleteln();
        printw("There are %d available newsgroups", c_active);
        move(scrn, 60);
        printw("Press ? for Help");
    }
    move(scrnpos, flag);
    refresh();
}
