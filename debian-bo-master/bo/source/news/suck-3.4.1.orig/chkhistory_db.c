#include <config.h>


/* If we use history databases, then these routines will */
/* get compiled and used.  If we don't, the routines in chkhistory.c will */
/* be used. */

#include <stdio.h>
#include "suck_config.h"
#include "suck.h"
#include "both.h"
#include "chkhistory.h"
#include "suckutils.h"
#include "phrases.h"

#ifdef TIMER
#include "timer.h"
#endif

/* These take care if user had multiple defines in makefile */
/* use DBM if we've got it */
#ifdef USE_DBM
#undef USE_NDBM
#undef USE_DBZ
#endif
/* else if have NDBM use it */
#ifdef USE_NDBM
#undef USE_DBZ
#endif

#ifdef USE_DBM
#include <dbm.h>
#define close_history() dbmclose();
#endif

#ifdef USE_NDBM
#include <ndbm.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
static DBM *db = NULL;	/* I know this isn't too pretty, but its the easiest way to do it */
#define close_history() dbm_close(db)
#endif

#ifdef USE_DBZ
#include <dbz.h>
#define close_history() dbmclose()
#endif

/* function prototypes */
int open_history(void);
int  check_history(char *);
/*---------------------------------------------------------------------------------------------------*/
void chkhistory(PMaster master) {

	PList curr, prev;
	int nrfound = 0;

	if(open_history() != TRUE) {
		error_log(ERRLOG_REPORT, chkh_phrases[0], HISTORY_FILE, NULL);
	}
	else {

		print_phrases(master->msgs, chkh_phrases[6], NULL);
		fflush(master->msgs);	/* so msg gets printed */
#ifdef TIMER
		TimerFunc(TIMER_START, 0L, NULL);
#endif

		/* okay cycle thru our list, checking each against the history DB */
		curr = master->head;
		prev = NULL;
		
		while(curr != NULL ) {
			if(check_history(curr->msgnr) == TRUE) {
#ifdef DEBUG2
				do_debug("Matched %s, nuking\n", curr->msgnr);
#endif
				/* matched, nuke it */
				nrfound++;
				master->nritems--;
				if(prev == NULL) {
					/* remove master node */
					master->head = curr->next;
					free_one_node(curr);
					curr = master->head;	/* next node to check */
				}
				else {
					prev->next = curr->next;
					free_one_node(curr);
					curr = prev->next;	/* next node to check */
				}
			}
			else {
				/* next node to check */
				prev = curr;
				curr = curr->next;
			}
		}
#ifdef TIMER
		TimerFunc(TIMER_TIMEONLY, 0l, master->msgs);
#endif
		close_history();
		print_phrases(master->msgs, chkh_phrases[4], str_int(nrfound), NULL);
	}
	return;
}
/*------------------------------------------------------------------------*/
int open_history(void) {
	
	int retval = FALSE;

#if defined (USE_DBM) || defined (USE_DBZ)

	if(dbminit(HISTORY_FILE) >= 0) {
		retval = TRUE;
	}
#else

	if((db = dbm_open(HISTORY_FILE, O_RDONLY, 0)) != NULL) {
		retval = TRUE;
	}
#endif

	return retval;
}
/*----------------------------------------------------------------*/
int check_history(char *msgid) {

	datum input, result;

	input.dptr = msgid;
	input.dsize = strlen(msgid) +1;

#if defined (USE_DBM)
	
	result = fetch(input);

#elif defined (USE_DBZ) 

	result = dbzfetch(input);

#elif defined (USE_NDBM)

	result = dbm_fetch(db, input);

#endif

	return (result.dptr != NULL) ? TRUE : FALSE;

}		
