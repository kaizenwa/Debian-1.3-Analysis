/***************************************************************/
/*                                                             */
/*  SORT.C                                                     */
/*                                                             */
/*  Routines for sorting reminders by trigger date             */
/*                                                             */
/*  This file is part of REMIND.                               */
/*  Copyright (C) 1992-1997 by David F. Skoll                  */
/*                                                             */
/***************************************************************/

static char const RCSID[] = "$Id: sort.c,v 1.2 1997/01/16 04:14:31 dfs Exp $";

#include "config.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <stdio.h>
#include <string.h>
#include "types.h"
#include "protos.h"
#include "expr.h"
#include "globals.h"
#include "err.h"

/* The structure of a sorted entry */
typedef struct sortrem {
    struct sortrem *next;
    char *text;
    int trigdate;
    int trigtime;
    int typ;
    int priority;
} Sortrem;

/* The sorted reminder queue */
static Sortrem *SortedQueue = (Sortrem *) NULL;

PRIVATE Sortrem *MakeSortRem ARGS ((int jul, int tim, char *body, int typ, int prio));
PRIVATE void IssueSortBanner ARGS ((int jul));

/***************************************************************/
/*                                                             */
/*  MakeSortRem                                                */
/*                                                             */
/*  Create a new Sortrem entry - return NULL on failure.       */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE Sortrem *MakeSortRem(int jul, int tim, char *body, int typ, int prio)
#else
static Sortrem *MakeSortRem(jul, tim, body, typ, prio)
int jul, tim;
char *body;
int typ, prio;
#endif
{
    Sortrem *new = NEW(Sortrem);
    if (!new) return NULL;

    new->text = StrDup(body);
    if (!new->text) {
	free(new);
	return NULL;
    }
  
    new->trigdate = jul;
    new->trigtime = tim;
    new->typ = typ;
    new->priority = prio;
    new->next = NULL;
    return new;
}

/***************************************************************/
/*                                                             */
/*  InsertIntoSortBuffer                                       */
/*                                                             */
/*  Insert a reminder into the sort buffer                     */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PUBLIC int InsertIntoSortBuffer(int jul, int tim, char *body, int typ, int prio)
#else
int InsertIntoSortBuffer(jul, tim, body, typ, prio)
int jul;
int tim;
char *body;
int typ, prio;
#endif
{
    Sortrem *new = MakeSortRem(jul, tim, body, typ, prio);
    Sortrem *cur = SortedQueue, *prev = NULL;
    int ShouldGoAfter;

    if (!new) {
	Eprint("%s", ErrMsg[E_NO_MEM]);
	IssueSortedReminders();
	SortByDate = 0;
	SortByTime = 0;
	SortByPrio = 0;
	return E_NO_MEM;
    }

    /* Find the correct place in the sorted list */
    if (!SortedQueue) {
	SortedQueue = new;
	return OK;
    }
    while (cur) {
	ShouldGoAfter = CompareRems(new->trigdate, new->trigtime, new->priority,
				    cur->trigdate, cur->trigtime, cur->priority,
				    SortByDate, SortByTime, SortByPrio);
		      
	if (ShouldGoAfter <= 0) {
	    prev = cur;
	    cur = cur->next;
	} else {
	    if (prev) {
		prev->next = new;
		new->next = cur;
	    } else {
		SortedQueue = new;
		new->next = cur;
	    }
	    return OK;
	}
      
    }
    prev->next = new;
    new->next = cur;  /* For safety - actually redundant */
    return OK;
}

   
/***************************************************************/
/*                                                             */
/*  IssueSortedReminders                                       */
/*                                                             */
/*  Issue all of the sorted reminders and free memory.         */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PUBLIC void IssueSortedReminders(void)
#else
void IssueSortedReminders()
#endif
{
    Sortrem *cur = SortedQueue;
    Sortrem *next;
    int olddate = NO_DATE;

    while (cur) {
	next = cur->next;
	switch(cur->typ) {
	case MSG_TYPE:
	    if (MsgCommand) {
		DoMsgCommand(MsgCommand, cur->text);
            } else {
		if (cur->trigdate != olddate) {
		    IssueSortBanner(cur->trigdate);
		    olddate = cur->trigdate;
		}
		printf("%s", cur->text);
            }
	    break;

	case MSF_TYPE:
#ifdef OS2_POPUP
	    FillParagraph(cur->text, 0);
#else
	    FillParagraph(cur->text);
#endif
	    break;

	case RUN_TYPE:
	    system(cur->text);
	    break;
	}

	free(cur->text);
	free(cur);
	cur = next;
    }
    SortedQueue = NULL;
}
/***************************************************************/
/*                                                             */
/*  IssueSortBanner                                            */
/*                                                             */
/*  Issue a daily banner if the function sortbanner() is       */
/*  defined to take one argument.                              */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PRIVATE void IssueSortBanner(int jul)
#else
static void IssueSortBanner(jul)
int jul;
#endif
{
    char BanExpr[25];
    int y, m, d;
    Value v;
    char *s = BanExpr;

    if (UserFuncExists("sortbanner") != 1) return;

    FromJulian(jul, &y, &m, &d);
    sprintf(BanExpr, "sortbanner('%04d/%02d/%02d')", y, m+1, d);   
    y = EvalExpr(&s, &v);
    if (y) return;
    if (DoCoerce(STR_TYPE, &v)) return;
    if (!DoSubstFromString(v.v.str, SubstBuffer, jul, NO_TIME))
	if (*SubstBuffer) printf("%s\n", SubstBuffer);
    DestroyValue(v);
}

/***************************************************************/
/*                                                             */
/*  CompareRems                                                */
/*                                                             */
/*  Compare two reminders for sorting.  Return 0 if they       */
/*  compare equal; 1 if rem2 should come after rem1, -1 if     */
/*  rem1 should come after rem2.  bydate and bytime control    */
/*  sorting direction by date and time, resp.                  */
/*                                                             */
/***************************************************************/
#ifdef HAVE_PROTOS
PUBLIC int CompareRems(int dat1, int tim1, int prio1,
                       int dat2, int tim2, int prio2,
		       int bydate, int bytime, int byprio)
#else
int CompareRems(dat1, tim1, prio1, dat2, tim2, prio2, bydate, bytime, byprio)
int dat1, tim1, prio1, dat2, tim2, prio2, bydate, bytime, byprio;
#endif
{
    int dafter, tafter, pafter;

    dafter = (bydate != SORT_DESCEND) ? 1 : -1;
    tafter = (bytime != SORT_DESCEND) ? 1 : -1;
    pafter = (byprio != SORT_DESCEND) ? 1 : -1;

    if (dat1 < dat2) return dafter;
    if (dat1 > dat2) return -dafter;

    if (tim1 == NO_TIME && tim2 != NO_TIME) return -1;
    if (tim1 != NO_TIME && tim2 == NO_TIME) return 1;
    if (tim1 < tim2) return tafter;
    if (tim1 > tim2) return -tafter;

    if (prio1 < prio2) return pafter;
    if (prio1 > prio2) return -pafter;

    return 0;
}
