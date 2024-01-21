/*
 * timer.c -- handles timers in ircII
 * Copyright 1993, 1996 Matthew Green
 * This file organized/adapted by Jeremy Nelson
 *
 * This used to be in edit.c, and it used to only allow you to
 * register ircII commands to be executed later.  I needed more
 * generality then that, specifically the ability to register
 * any function to be called, so i pulled it all together into
 * this file and called it timer.c
 */

#include "irc.h"
#include "ircaux.h"
#include "lastlog.h"
#include "timer.h"
#include "hook.h"
#include "output.h"
#include "edit.h"
#include "misc.h"
#include "vars.h"
#include "tcl_bx.h"

static	void	show_timer _((char *command));
static	void	delete_all_timers _((void));

/*
 * timercmd: the bit that handles the TIMER command.  If there are no
 * arguements, then just list the currently pending timers, if we are
 * give a -DELETE flag, attempt to delete the timer from the list.  Else
 * consider it to be a timer to add, and add it.
 */
#ifdef __STDC__
void	timercmd (char *command, char *args, char *subargs)
#else
void timercmd(command, args, subargs)
	char	*command;
	char	*args;
	char	*subargs;
#endif
{
	char	*waittime,
		*flag;
	char	*want = empty_string;
	char	*ptr;

	if (*args == '-' || *args == '/')
	{
		flag = next_arg(args, &args);

		if (!my_strnicmp(flag+1, "D", 1))	/* DELETE */
		{
			if (!(ptr = next_arg(args, &args)))
				say("%s: Need a timer reference number for -DELETE", command);
			else
			{
				if (!my_strnicmp(ptr, "A", 1))
					delete_all_timers();
				else
					delete_timer(ptr);
			}

			return;
		}
		else if (!my_strnicmp(flag+1, "R", 1))	/* REFNUM */
		{
			want = next_arg(args, &args);
			if (!want || !*want)
			{
				say("%s: Missing argument to -REFNUM", command);
				return;
			}
		}
		else
		{
			say("%s: %s no such flag", command, flag);
			return;
		}
	}

	/* else check to see if we have no args -> list */

	if (!(waittime = next_arg(args, &args)))
		show_timer(command);
	else
		add_timer(want, atol(waittime), NULL, args, subargs);
	return;
}

/*
 * This is put here on purpose -- we dont want any of the above functions
 * to have any knowledge of this struct.
 */
static TimerList *PendingTimers;

#ifdef WANT_TCL
extern Tcl_Interp *interp;
extern TimerList *tcl_Pending_timers;
extern TimerList *tcl_Pending_utimers;
static int timer_id = 1;

#endif

static char *current_exec_timer = empty_string;

/*
 * ExecuteTimers:  checks to see if any currently pending timers have
 * gone off, and if so, execute them, delete them, etc, setting the
 * current_exec_timer, so that we can't remove the timer while its
 * still executing.
 *
 * changed the behavior: timers will not hook while we are waiting.
 */
extern	void ExecuteTimers _((void))
{
	time_t	current;
	TimerList *next;
	int	old_in_on_who;
        static int parsingtimer = 0;

        /* We do NOT want to parse timers while waiting
         * cause it gets icky recursive
         */
        if (waiting || parsingtimer || !PendingTimers)
                return;

        parsingtimer = 1;
	time(&current);
	while (PendingTimers && PendingTimers->time <= current)
	{
		old_in_on_who = in_on_who;
#ifdef I_DONT_TRUST_MY_USERS
		in_on_who = PendingTimers->in_on_who;
#endif
		current_exec_timer = PendingTimers->ref;

		/* 
		 * If a callback function was registered, then
		 * we use it.  If no callback function was registered,
		 * then we use ''parse_line''.
		 */
		if (PendingTimers->callback)
			(*PendingTimers->callback)(PendingTimers->command);
		else
		{
			if (strchr(PendingTimers->command, '\e'))
				parse_command(PendingTimers->command, 0, empty_string);
			else
				parse_line("TIMER",(char *)PendingTimers->command, PendingTimers->subargs, 0,0);
			new_free((char **)&PendingTimers->command);
			new_free(&PendingTimers->subargs);
		}

		current_exec_timer = empty_string;
		next = PendingTimers->next;
		new_free((char **)&PendingTimers);
		PendingTimers = next;
		in_on_who = old_in_on_who;
	}
        parsingtimer = 0;
}



#ifdef WANT_TCL

/* add a timer */
char *tcl_add_timer (TimerList **stack, int elapse,char *cmd, unsigned long prev_id)
{
TimerList *old=(*stack);
	*stack= (TimerList *)new_malloc(sizeof(TimerList));
	(*stack)->next=old; 
	(*stack)->time=elapse;
	malloc_strcpy(&(*stack)->command,cmd);
	/* if it's just being added back and already had an id, */
	/* don't create a new one */
	if (prev_id > 0) 
		strcpy((*stack)->ref, ltoa(prev_id));
	else 
		strcpy((*stack)->ref, ltoa(timer_id++));
	return (*stack)->ref;
}

/* remove a timer, by id */
int tcl_remove_timer(TimerList **stack, unsigned long id)
{
TimerList *mark=*stack, *old; 
int ok = 0;
	*stack=NULL; 
	while (mark!=NULL) {
		if (strcmp(mark->ref, ltoa(id))) 
			tcl_add_timer(stack,mark->time,mark->command,mark->refno);
		else 
			ok++;
		old = mark; 
		mark = mark->next;
		new_free(&old->command); 
		new_free((char **)&old);
	}
	return ok;
}

/* check timers, execute the ones that have expired */
void do_check_timers(TimerList **stack)
{
TimerList *mark=*stack, *old; 
Tcl_DString ds; 
int argc, i; 
char **argv;
  /* new timers could be added by a Tcl script inside a current timer */
  /* so i'll just clear out the timer list completely, and add any */
  /* unexpired timers back on */

	*stack=NULL;
	while (mark!=NULL) 
	{
		mark->time--;
		if (mark->time == 0) 
		{
			int code;
			Tcl_DStringInit(&ds);
			if (Tcl_SplitList(interp,mark->command,&argc,&argv) != TCL_OK) 
				putlog(LOG_CRAP,"*","(Timer) Error for '%s': %s",mark->command, interp->result);
			else 
			{
				for (i=0; i<argc; i++) 
					Tcl_DStringAppendElement(&ds,argv[i]);
				free(argv);
				code=Tcl_Eval(interp,Tcl_DStringValue(&ds));
				/* code=Tcl_Eval(interp,mark->cmd); */
				Tcl_DStringFree(&ds);
				if (code!=TCL_OK)
					putlog(LOG_CRAP,"*","(Timer) Error for '%s': %s",mark->command, interp->result);
			}
		}
		else 
			tcl_add_timer(stack,mark->time,mark->command,mark->refno);
		old=mark; 
		mark=mark->next;
		new_free(&old->command); 
		new_free((char **)&old);
	}
}

void check_timers()
{
	do_check_timers(&tcl_Pending_timers);
}

void check_utimers()
{
	do_check_timers(&tcl_Pending_utimers);
}

void tcl_list_timer(Tcl_Interp *irp, TimerList *stack)
{
TimerList *mark=stack; 
char *x = NULL;
time_t current, time_left;
	time(&current);
	for (mark=stack; mark; mark = mark->next) 
	{
		time_left = mark->time - current;
		if (time_left < 0)
			time_left = 0;
		malloc_sprintf(&x, "%u %s timer%lu", time_left, mark->command, mark->refno);
		Tcl_AppendElement(irp,x);
		new_free(&x);
	}
}
#endif

/*
 * show_timer:  Display a list of all the TIMER commands that are
 * pending to be executed.
 */
#ifdef __STDC__
static	void	show_timer (char *command)
#else
static	void show_timer(command)
	char	*command;
#endif
{
	TimerList	*tmp;
	time_t	current,
		time_left;
	int count = 0;
	
	for (tmp = PendingTimers; tmp; tmp = tmp->next)
		if (!tmp->callback)
			count++;

	if (count == 0)
	{
		say("%s: No commands pending to be executed", command);
		return;
	}

	time(&current);
	put_it("%s", convert_output_format(get_string_var(FORMAT_TIMER_VAR), "%s %s %s","Timer","Seconds","Command"));
	for (tmp = PendingTimers; tmp; tmp = tmp->next)
	{
		time_left = tmp->time - current;
		if (time_left < 0)
			time_left = 0;
		if (tmp->callback)
			continue;
		put_it("%s", convert_output_format(get_string_var(FORMAT_TIMER_VAR), "%d %d %s", tmp->ref, time_left, tmp->callback? "(internal callback)" : (tmp->command? tmp->command : "")));
	}
}

/*
 * create_timer_ref:  returns the lowest unused reference number for a timer
 *
 * This will never return 0 for a refnum because that is what atol() returns
 * on case of error, so that it can never happen that a timer has a refnum
 * of zero which would be tripped if the user did say,
 *	/TIMER -refnum foobar 3 blah blah blah
 * which should elicit an error, not be silently punted.
 */
#ifdef __STDC__
static	int	create_timer_ref (char *refnum_want, char *refnum_gets)
#else
static	int create_timer_ref(refnum_want, refnum_gets)
	char *refnum_want;
	char *refnum_gets;
#endif
{
	TimerList       *tmp;
	int             refnum = 0;
                
	/* Max of 10 characters. */
	if (strlen(refnum_want) > REFNUM_MAX)
		refnum_want[REFNUM_MAX] = 0;

	/* If the user doesnt care */
	if (!strcmp(refnum_want, empty_string))
	{
		/* Find the lowest refnum available */
		for (tmp = PendingTimers; tmp; tmp = tmp->next)
		{
			if (refnum < my_atol(tmp->ref))
				refnum = my_atol(tmp->ref);
		}
		strncpy(refnum_gets, ltoa(refnum+1), REFNUM_MAX);
	}
	else
	{
		/* See if the refnum is available */
		for (tmp = PendingTimers; tmp; tmp = tmp->next)
		{
			if (!my_stricmp(tmp->ref, refnum_want))
				return -1;
		}
		strncpy(refnum_gets, refnum_want, REFNUM_MAX);
	}

	return 0;
}

/*
 * Deletes a refnum.  This does cleanup only if the timer is a 
 * user-defined timer, otherwise no clean up is done (the caller
 * is responsible to handle it)  This shouldnt output an error,
 * it should be more general and return -1 and let the caller
 * handle it.  Probably will be that way in a future release.
 */
extern int delete_timer (char *ref)
{
	TimerList	*tmp,
			*prev;

	if (current_exec_timer != empty_string)
	{
		say("You may not remove a TIMER from itself");
		return -1;
	}

	for (prev = tmp = PendingTimers; tmp; prev = tmp, tmp = tmp->next)
	{
		/* can only delete user created timers */
		if (!my_stricmp(tmp->ref, ref))
		{
			if (tmp == prev)
				PendingTimers = PendingTimers->next;
			else
				prev->next = tmp->next;
			if (!tmp->callback)
			{
				new_free((char **)&tmp->command);
				new_free((char **)&tmp->subargs);
			}
			new_free((char **)&tmp);
			return 0;
		}
	}
	say("TIMER: Can't delete %s, no such refnum", ref);
	return -1;
}

static void delete_all_timers _((void))
{
	while (PendingTimers)
		delete_timer(PendingTimers->ref);
	return;
}

/*
 * You call this to register a timer callback.
 *
 * The arguments:
 *  refnum_want: The refnum requested.  This should only be sepcified
 *		 by the user, functions wanting callbacks should specify
 *		 the value -1 which means "dont care".
 * The rest of the arguments are dependant upon the value of "callback"
 *	-- if "callback" is NULL then:
 *  callback:	 NULL
 *  what:	 some ircII commands to run when the timer goes off
 *  subargs:	 what to use to expand $0's, etc in the 'what' variable.
 *
 *	-- if "callback" is non-NULL then:
 *  callback:	 function to call when timer goes off
 *  what:	 argument to pass to "callback" function.  Should be some
 *		 non-auto storage, perhaps a struct or a malloced char *
 *		 array.  The caller is responsible for disposing of this
 *		 area when it is called, since the timer mechanism does not
 *		 know anything of the nature of the argument.
 * subargs:	 should be NULL, its ignored anyhow.
 */
char *add_timer(char *refnum_want, long when, int (callback) _((void *)), char *what, char *subargs)
{
	TimerList	**slot,
			*ntimer;
	char		refnum_got[REFNUM_MAX+1] = "";
	
	ntimer = (TimerList *) new_malloc(sizeof(TimerList));
	ntimer->in_on_who = in_on_who;
	ntimer->time = time(NULL) + when;

	if (create_timer_ref(refnum_want, refnum_got) == -1)
	{
		say("TIMER: Refnum %s already exists", refnum_want);
		new_free((char **)&ntimer);
		return NULL;
	}

	strcpy(ntimer->ref, refnum_got);	
	ntimer->callback = callback;
	if (callback)
	{
		ntimer->command = (void *)what;
		ntimer->subargs = (void *)NULL;
	}
	else
	{
		ntimer->command = m_strdup(what);
		ntimer->subargs = m_strdup(subargs);
	}

	/* we've created it, now put it in order */
	for (slot = &PendingTimers; *slot; slot = &(*slot)->next)
	{
		if ((*slot)->time > ntimer->time)
			break;
	}
	ntimer->next = *slot;
	*slot = ntimer;
	return ntimer->ref;
}

/*
 * TimerTimeout:  Called from irc_io to help create the timeout
 * part of the call to select.
 */
time_t TimerTimeout _((void))
{
	time_t	current;
	time_t	timeout_in;

	if (!PendingTimers)
		return 70; /* Just larger than the maximum of 60 */
	time(&current);
	timeout_in = PendingTimers->time - current;
	return (timeout_in < 0) ? 0 : timeout_in;
}
