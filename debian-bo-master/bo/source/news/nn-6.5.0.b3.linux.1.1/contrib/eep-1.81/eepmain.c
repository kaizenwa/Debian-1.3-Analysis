/*------------------------------------------------------------------------
       Name: eepmain.c

This program makes it easier for users to change their .newsrc file.  
It uses curses to implement a simple editor, which allows users to 
join or unjoin newsgroups, from outside of rn or trn.

     Author: Paul Gillingwater, paul@actrix.gen.nz
 Minor mods: Steven Parker, sp@sdf.lonestar.org

Usage:
    eep [-p] [-v]

Options:
    -p:  Use a terse pointer on screen instead of a bar
    -v:  Verbose mode, tells you more during startup

------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>

#ifdef UNIX
#include <fcntl.h>
#include <termio.h>
#endif /* UNIX */

#ifdef ANSI
#include <stdlib.h>
#else
#define void int
extern char *malloc();
extern char *getenv();
#endif /* ANSI */

#include "eep.h"

/* Some function definitions */

extern void    newsmain();
extern void    *wallop();  /* eepmisc.c -- malloc() replacement */

char    buffer[BUFSIZE], /* general purpose line buffer */
        tmp[BUFSIZE],
        s_hi[30],       /* strings for numbers */
        s_lo[30],       /* strings for numbers */
        s_flag[8];
char    t_status,       /* from .newsrc file */
        t_flag;         /* from active file */
char    *ptr;           /* general purpose pointer */
char	*bog_msg = "Bogus newsgroup (not in active file)";
char	*home = '\0';	/* home directory */

unsigned int
	eepoint = POINTER,	/* screen pointer type */
        noshell = 0,		/* -! flag to deny shell-out */
        uid = 0,		/* real user id */
        verbose = VERBOSE,	/* verbose flag */
        pid = 0;		/* process id */

int     result;         /* flag for searching */

FILE    *fnewsrc,
        *factive;

struct  actif *act[MAXLINES];  /* here's the main array */
struct  actif *topact[MAXDIST]; /* array for top level distributions only */
struct  actif *aptr;    /* temporary pointer */

/* To make malloc() faster, we use wallop() [see eepmisc.c] to
 * handle memory.  We'll rely on wallop() to store the pointers
 * so they can be freed during cleanup().
 */
char    *buffer_city[MAXBUFS];
int     malloc_count = 0;

int     i_active,    /* index into actif arrays */
        c_active,    /* number of elements in act array */
        t_active,    /* number of elements in topact array */
        sub_count = 0,   /* count of subscribed newsgroups */
        bog_count;   /* count of bogus newsgroups */
int     high,low,mid;    /* used for binary chop searching */

/* The levels array contains the Head pointers which control the
linked lists of newsgroups grouped by their "depth" in the
hierarchy, and sorted alphabetically.  E.g. alt, sci, soc are 
level 0, alt.pagan, sci.physics are level 1 and so on. */

struct actif *levels[MAXLEVELS];  /* keep track of levels with this array */

int    i_levels;    /* index into array */

/* Comparison functions for qsort.  These functions are used for
sorting the array of pointers that point to our actif data structures. */

int    qcompare(item1,item2)    /* sort by name */
struct actif    **item1, **item2;
{
struct actif    *ptr1, *ptr2;
    ptr1 = (struct actif *) *item1;
    ptr2 = (struct actif *) *item2;
    return (strcmp(ptr1->name, ptr2->name));
}

int    icompare(item1,item2)    /* sort by index number */
struct actif    **item1, **item2;
{
struct actif    *ptr1, *ptr2;
    ptr1 = (struct actif *) *item1;
    ptr2 = (struct actif *) *item2;
    return (ptr1->index - ptr2->index);
}

/* This routine will move all subscribed newsgroups to the top
 * of the list.  If the list was in alphabetical order, then 
 * this order will be retained, otherwise numeric order will
 * be used.
 */
int    scompare(item1,item2)    /* sort by subscription status */
struct actif    **item1, **item2;
{
struct actif    *ptr1, *ptr2;
int    temp_result;
extern int alphabetized;

    ptr1 = (struct actif *) *item1;
    ptr2 = (struct actif *) *item2;
    temp_result = ptr2->status - ptr1->status;
    if (temp_result == 0) {
       if (alphabetized) 
           return (strcmp(ptr1->name, ptr2->name));
       else
           return (ptr1->index - ptr2->index);
    }
    return (temp_result);
}

/* cleanup() -- remove lock file, then exit. */

void    cleanup()
{
#ifdef UNIX
extern struct termio tbufsave;
    ioctl(0, TCSETAF, &tbufsave);
#endif /* UNIX */
    endwin();  /* terminate curses */
#ifdef UNIX
    if (home != (char *)NULL)
        sprintf(tmp, "%s/%s", home, RNLOCK);
    else    strcpy(tmp, RNLOCK);
    if (verbose) printf("Removing lock file %s\n",tmp);
    unlink(tmp);
#endif /* UNIX */
    /*
     * Now run through the array containing pointers to the
     * buffers allocated with malloc(), and free them.
     */
    if (verbose)
        printf("Freeing %d buffers each of %d bytes.\n",
            malloc_count,WALLOP_SIZE);
    while (malloc_count > 0) {
        free(buffer_city[malloc_count - 1]);
        malloc_count--;
    }
    exit(0);
}

/* This routine will read the a file (descfile) containing names and
   descriptions of newsgroups, matching it with the active file.  */

void    read_desc(descfile)
FILE    *descfile;
{
char    *name, *desc;  /* pointers to name and description */
int     counter = 0;

    i_active = 0;      /* start with first one of course */
    aptr = (struct actif *)NULL;

    while (fgets(buffer,BUFSIZE,descfile) != (char *)NULL) {

    /* ignore comment lines or blank lines */
        switch(buffer[0]) {
        case '#':
        case '\n':
        case '\r':
        case '\0':
            continue;
        }

    /* Get name and description.  If either is absent, skip this line. */
        if ((name = strtok(buffer, " \t\r\n")) == (char *)NULL) continue;
        if ((desc = strtok((char *)NULL, "\r\n")) == (char *)NULL) continue;

    /* Advance over any whitespace preceding description */
        while ((*(desc) == ' ') || (*(desc) == '\t')) desc++;

    /* Although we cannot assume that the newsgroups are in alphabetical
     * order, we'll try looking at the next one anyway before doing a
     * search. */

        if (i_active <= c_active)    /* range check */
            aptr = act[i_active];

    /* This next line should never happen.  It would only occur if
     * we've run out of things to match! */

        if (aptr == (struct actif *)NULL) {
            if (verbose) printf("This should never happen!\n");
            continue;
        }

    /* Now compare the name read with current position in active file */

        if (strcmp(name,aptr->name) == 0) {

/* Here we look for the best description possible, i.e. one that is not 
null, and preferably longest (i.e. not just a '?').  This will be because 
we may find duplicate lines in the newsgroups file.  Also, it may be 
possible to find no description at all. */

            if (aptr->desc != (char *)NULL) {
            /* don't accept an inferior description */
                if (strlen(desc) <= strlen(aptr->desc)) continue;
            }

        /* allocate space for string + null byte, then copy it in */
            if ((aptr->desc = (char *)wallop(strlen(desc)+1)) == (char *)NULL) {
                printf("Fatal error while allocating memory!\n");
                cleanup();
            }
            strcpy(aptr->desc,desc);
            counter++;
            i_active++;      /* advance index to next pointer */
            if (i_active == c_active) i_active = 0;
            continue;
        }

    /*  Here we begin a binary chop search, comparing the newsgroup
     *  name we have just read from the newsgroups file with the list
     *  of newsgroups read from the active file.  */

        low = 0;
        high = c_active - 1;
loop1:
        if (low <= high) {
            mid = (low+high)/2;
            aptr = act[mid];
            result = strcmp(name,aptr->name);
            if (result == 0) {

                if (aptr->desc != (char *)NULL) {
                /* don't accept an inferior description */
                    if (strlen(desc) <= strlen(aptr->desc)) continue;
                }

            /* allocate space for string + null byte, then copy it in */
                if ((aptr->desc = (char *)wallop(strlen(desc)+1))
                   == (char *)NULL) {
                    printf("Fatal error while allocating memory!\n");
                    cleanup();
                }
                strcpy(aptr->desc,desc);

                i_active = mid + 1;
                if (i_active == c_active) i_active = 0;
                continue; /* with read loop */
            } else
            if (result > 0) { /* after */
                low = mid+1;
                goto loop1;
            } else
            if (result < 0) { /* before */
                high = mid-1;
                goto loop1;
            }
        }
    }
    fclose(descfile); 
    if (verbose) printf("Found %d descriptions.\n",counter);
}

/* Initialise the various chunks of memory and read in files. */

void    initial()
{

int    newsrc_order;    /* track reading order from .newsrc */
int    warning = FALSE; /* if warning messages have been issued */

char   *getenv();   /* ... or else illegal assignment  -sp */

char   *name,   /* pointers into line in active file */
       *hi,
       *lo,
       *flag;

#ifdef UNIX

/* For systems with a fixed BBS login, we have this hack
 * to deny shell access. 
 */
    uid = getuid();
    if (uid == SHELLDENY) noshell++;

/* Check for a lock file from rn or trn.  If it's not around,
 * let's make our own lock file.
 */
    
    if ((ptr = getenv("HOME"))   != (char *)NULL) home = ptr;
    if ((ptr = getenv("DOTDIR")) != (char *)NULL) home = ptr;
    if (home != (char *)NULL)
        sprintf(tmp, "%s/%s", home, RNLOCK);
    else    strcpy(tmp, RNLOCK);

    if ((factive = fopen(tmp, "r")) != (FILE *)NULL) { 
       if (fgets(buffer,BUFSIZE,factive) != (char *)NULL) { 
           pid = atoi(buffer); 
           if (pid > 0) { 
               if (kill(pid,0) >= 0) { /* doesn't exist? */ 
                    printf("Lock file %s suggests process %ld is active.\n", 
                    tmp,pid); 
                    printf("Please run EEP without rn or trn running.\n");
                    cleanup();
                } else if (verbose) {
                    printf("A lock file for rn was found (%s)\n",tmp);
                    printf("Process %ld not found -- file ignored.\n",pid);
                }
            }
        }
    }
    fclose(factive);

 /* Now create the lock file. */
    pid = getpid();
    if (verbose) printf("Creating lock file %s with pid %ld\n",tmp,pid);
    if ((factive = fopen(tmp, "w")) == (FILE *)NULL) {
        printf("Fatal: Can't create lock file %s \n", tmp);
        exit(0);
    }
    fprintf(factive,"%ld\n", pid);
    fclose(factive);
#endif /* UNIX */

#ifdef DOS
    msleep(0L);   /* initialize time routines */
#endif /* DOS */

/* CHANGES for JAN 1993: ************************************

Because NEWSGROUPS can often contain lots of duplicates, while
the active file is "cleaner", let's read the active file FIRST,
then scan the NEWSGROUPS and NEWSLOCAL file looking for
descriptions.  We should try to find the best description, i.e.
one that is not null, not a '?', and not "alt group".
A simple sequential read of these files is adequate.  Note
that the active file is authoritative when it comes to bogus
groups, which means we could simply ignore entries from the
NEWSGROUPS file that aren't in the active file.  

When we read the .newsrc, we still need to add entries for
bogus groups, to give people the option of keeping strange
stuff in their .newsrc.  This means we should prompt before
saving whether they want to lose the bogus groups or not.

*************************************************************/

    c_active = 0;    /* count the newsgroups as we go. */

    if ((factive = fopen(ACTIVEFILE, "r")) == (FILE *)NULL) {
        printf("Fatal: Unable to read %s\n",ACTIVEFILE);
        cleanup();
    }

    if (verbose) printf("Opening active file %s\n",ACTIVEFILE);
    while (fgets(buffer,BUFSIZE,factive) != (char *)NULL) {

    /* ignore comment lines or blank lines */
        switch(buffer[0]) {
        case '#':
        case '\n':
        case '\r':
        case '\0':
            continue;
        }

    /* strip off newlines or returns */
        while ((ptr = strchr(buffer,'\n')) != (char *)NULL) *ptr = '\0';
        while ((ptr = strchr(buffer,'\r')) != (char *)NULL) *ptr = '\0';

    /* process line from active file -- low message number ignored. */
        if ((name = strtok(buffer, " \t\r\n")) == (char *)NULL) continue;
        if ((hi   = strtok((char *)NULL, " \t\r\n")) == (char *)NULL) continue;
        if ((lo   = strtok((char *)NULL, " \t\r\n")) == (char *)NULL) continue;
        if ((flag = strtok((char *)NULL, " \t\r\n")) == (char *)NULL) continue;

    /* skip aliased group names, since they are essentially bogus  -sp */
        if (flag[0] == '=') continue;

    /* check validity of data.  Must have something in each position. */

    /* Now allocate a chunk of memory for actif */
        if ((aptr = (struct actif *) wallop(sizeof(struct actif)))
            == (struct actif *)NULL) {
            printf("Fatal error while allocating memory!\n");
            cleanup();
        }
        act[c_active] = aptr; /* record this pointer */

    /* allocate space for string + null byte, then copy it in */
        if ((aptr->name = (char *)wallop(strlen(name)+1)) == (char *)NULL) {
            printf("Fatal error while allocating memory!\n");
            cleanup();
        }

    /* now move data into the structure */
        strcpy(aptr->name,name);
        aptr->hi = atol(hi);
        aptr->hilo = (char *)NULL;
        aptr->desc = (char *)NULL;
        aptr->mark = 0;            /* mark flag */
        aptr->flag = flag[0];      /* from active file */
        aptr->status = '!';        /* from .newsrc */
        aptr->index = 9999;        /* sort unknowns at end */
        aptr->depth = (struct actif *) NULL;
        c_active++;
    }
    fclose(factive);
    if (c_active == 0) {
        printf("Fatal: Can't find any news groups in %s\n",ACTIVEFILE);
        cleanup();
    }

    if (verbose) {
        printf("There were %d newsgroups in the active file.\n", c_active);
        warning = TRUE;
    }

/* Sort the list by newsgroup name. */
    qsort( act, (unsigned) c_active,(unsigned) sizeof(act[0]), qcompare);

/* Now open NEWSGROUPS and NEWSLOCAL to look for descriptions.  If
 * these files don't exist, it's no great tragedy -- but the whole
 * point of EEP will be missed.  If you don't have them, encourage
 * your news administrator to create them with the checkgroups script.
 */
     
    if ((factive = fopen(NEWSGROUPS, "r")) != (FILE *)NULL) {
        if (verbose) printf("Reading descriptions from %s\n",NEWSGROUPS);
        read_desc(factive);
    }

    if ((factive = fopen(NEWSLOCAL, "r")) != (FILE *)NULL) {
        if (verbose) printf("Reading descriptions from %s\n",NEWSLOCAL);
        read_desc(factive);
    }

 /* Let's now build chains of pointers for each of the 
    hierarchies of news groups, taking our initial pointer
    from the array levels[]. */

    i_levels = 0;
    while (i_levels < MAXLEVELS) 
        levels[i_levels++] = (struct actif *) NULL; /* null array */

 /* Work backwards through the array, building the linked list.
    We also record the array index in each record.  If we were
    only accessing it as an array, this would be redundant, but
    we're using multiple linked lists of pointers as well.  
    We work backwards to ensure our chains are NULL terminated.  */

    i_active = c_active - 1;
    while (i_active >= 0) {
        aptr = act[i_active];
        ptr = aptr->name;
        i_levels = 0;
        while ((ptr = strchr(ptr,'.')) != (char *)NULL) {
            i_levels++;
            ptr++;
        }
        if (i_levels < MAXLEVELS) {
            aptr->depth = levels[i_levels];
            levels[i_levels] = aptr;
        }
        i_active--;
    }

/* This code will be re-used later when parsing newsgroups */
/* let's check the pointers in levels[] */
/*
    i_levels = 0;
    while (i_levels < MAXLEVELS) {
        aptr = levels[i_levels];
        while (aptr != (struct actif *)NULL) {
            sprintf(tmp,"%-25s %-.67s\n",
                aptr->name,
                aptr->desc);
            printf(tmp);
            aptr = (struct actif *) aptr->depth;
        }
        i_levels++;
    }
*/

    /* Now read in and match up our personal .newsrc */

    if (home != (char *)NULL)
        sprintf(tmp, "%s/%s", home, NEWSRC);
    else
        sprintf(tmp, "%s", NEWSRC);   /* default to current directory */

    if ((fnewsrc = fopen(tmp, "r")) == (FILE *)NULL) {
        printf("Fatal: Unable to read the file %s\n", tmp);
        printf("Please create it using the rn or trn news reader.\n");
        cleanup();
    }

    i_active = 0;
    newsrc_order = 1;
    bog_count = 0;
    if (verbose) printf("Now reading your personal news control file %s\n",tmp);
    
    while (fgets(buffer,BUFSIZE,fnewsrc) != (char *)NULL) {

    /* ignore comment lines */
        switch(buffer[0]) {
        case '#':
        case '\n':
        case '\r':
        case '\0':
            continue;
        }

    /* strip off CR and LF */
        while ((ptr = strchr(buffer,'\n')) != (char *)NULL) *ptr = '\0';
        while ((ptr = strchr(buffer,'\r')) != (char *)NULL) *ptr = '\0';

    /* don't try to match a null string */
        if (strlen(buffer) == 0) continue; 

/* Now examine the character that terminates the newsgroup name.
   If it's a colon ':', then this means the newsgroup is active
   for the user.  If it's an exclamation mark, then it is
   inactive (unsubscribed).  Anything else means the
   newsgroup may not be valid.  */

        if ((ptr = strchr(buffer,':')) != (char *)NULL) {
            t_status = ':';
            *ptr = '\0'; /* null terminate newsgroup */
            ptr++;    /* point to rest of line (hilo) */
        } else if ((ptr = strchr(buffer,'!')) != (char *)NULL) {
            t_status = '!';
            *ptr = '\0'; /* null terminate newsgroup */
            ptr++;    /* point to rest of line (hilo) */
        } else continue; /* not valid */

    /* advance past any whitespace */
        while ((*ptr == ' ') || (*ptr == '\t')) ptr++;
        strcpy(tmp,ptr);    /* this is hilo */

        aptr = act[i_active]; /* range check */
        if ((aptr != (struct actif *)NULL) 
            && (strcmp(buffer,aptr->name) == 0)) {
            if (strlen(tmp) > 0) {
                if ((aptr->hilo = (char *)wallop(strlen(tmp)+1)) 
                    == (char *)NULL) {
                    printf("Error allocating memory!\n");
                    cleanup();
                }
                strcpy(aptr->hilo,tmp);
            }
            if (t_status == ':') sub_count++;
            aptr->status = t_status;
            aptr->index = newsrc_order++;
            i_active++;
            if (i_active == c_active) i_active = 0;
            continue;
        }

        low = 0;
        high = c_active - 1;
loop2:
        if (low <= high) {
            mid = (low+high)/2;
            aptr = act[mid];
            result = strcmp(buffer,aptr->name);
            if (result == 0) {
                if (strlen(tmp) > 0) {
                    if ((aptr->hilo = (char *)wallop(strlen(tmp)+1)) 
                        == (char *)NULL) {
                        printf("Error allocating memory!\n");
                        return;
                    }
                    strcpy(aptr->hilo,tmp);
                }
                if (t_status == ':') sub_count++;
                aptr->status = t_status;
                aptr->index = newsrc_order++;
                i_active = mid + 1;
            /* This next hack is necessary to prevent the
            corner case where mid has pointed at the very
            last entry in the act strucutre. */
                if (i_active == c_active)  i_active = 0;
                continue; /* read loop */
            } else
            if (result > 0) { /* after */
                low = mid+1;
                goto loop2; 
            } else
            if (result < 0) { /* before */
                high = mid-1;
                goto loop2; 
            }
        } else {
            bog_count++;    /* must be bogus! */

            if ((aptr = (struct actif *) wallop(sizeof(struct actif)))
                == (struct actif *)NULL) {
                printf("Fatal error while allocating memory!\n");
                cleanup();
            }
            act[c_active] = aptr; 

            if ((aptr->name = (char *)wallop(strlen(buffer)+1)) 
               == (char *)NULL) {
                printf("Fatal error while allocating memory!\n");
                cleanup();
            }
            strcpy(aptr->name,buffer);
            if (verbose) {
                printf("Bogus newsgroup: %s\n",aptr->name);
            }

            if (strlen(tmp) > 0) {
                if ((aptr->hilo = (char *)wallop(strlen(tmp)+1)) 
                   == (char *)NULL) {
                    printf("Fatal error while allocating memory!\n");
                    cleanup();
                }
                strcpy(aptr->hilo,tmp);
            }
            aptr->hi = 0;
            aptr->mark = 0;
            aptr->desc = "Bogus newsgroup (not in active file)";
            aptr->flag = '\0';
            if (t_status == ':') sub_count++;
            aptr->status = t_status;
            aptr->index = newsrc_order++;
            aptr->depth = (struct actif *) NULL;
            c_active++;
            if (c_active < 2) continue;
            qsort( act, (unsigned) c_active,(unsigned) sizeof(act[0]), 
               qcompare);
            continue; /* with read loop */
        }
    }
    fclose(fnewsrc);

    if ((bog_count > 0) && (verbose)) {
        if (bog_count == 1)
            printf("There was 1 bogus newsgroup in your .newsrc.\n");
        else
            printf("There were %d bogus newsgroups in your .newsrc.\n",
                bog_count);
        warning = TRUE;
    }
    if ((sub_count > 0) && (verbose)) {
        printf("There were %d subscribed newsgroups in your .newsrc.\n",
                sub_count);
        warning = TRUE;
    }


    /* Now let's sort this lot into the order that we originally
    read it from the .newsrc in.  New newsgroups will be forced
    to the bottom, making it easier for them to be spotted.  */

    qsort( act, (unsigned) c_active, (unsigned) sizeof(act[0]), icompare);

#ifdef UNIX
    if (home != (char *)NULL)
        sprintf(tmp, "%s/.newsrc.eep", home);
    else    sprintf(tmp, ".newsrc.eep");

    if ((fnewsrc = fopen(tmp, "w")) == (FILE *)NULL) {
       printf("warning: cannot create .newsrc.eep -- check permissions\n");
       warning = TRUE;
    }
#endif /* UNIX */

    if (warning) {
        printf("\nPress ENTER to continue.");
        gets(buffer);
    }
}


main(argc, argv)
int    argc;
char    **argv;
{
int    ch;
int    timer;

#ifdef UNIX
    while ((ch = getopt(argc,argv,"pv!?")) != EOF) switch(ch) {
        case 'p':
            if (eepoint) 		/* make it REALLY a toggle  -sp */
                eepoint = FALSE;
            else
                eepoint = TRUE;
            break;
        case 'v':
            if (verbose) 
                verbose = FALSE;
            else
                verbose = TRUE;
            break;
        case '!':
            noshell++;
            break;
        case '?':
            fprintf(stderr,"usage: eep [-p] [-v]\n\n");
            fprintf(stderr,"-p terse pointer toggle\n");
            fprintf(stderr,"-v verbose mode toggle\n");
            fprintf(stderr,"\nUse man eep for more info.\n");
            exit(0);
    }
#endif /* UNIX */

    fprintf(stderr,"EEP! version 1.8.sp .newsrc editor\n");
    initial();    /* read in newsgroups, active and .newsrc */
    newsmain();
    cleanup();
} /* end of eepmain.c */
