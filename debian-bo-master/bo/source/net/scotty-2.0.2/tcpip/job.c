/*
 * job.c
 *
 * The simple job scheduler used to implement monitoring scripts.
 * This version is derived from the original job scheduler written
 * in Tcl by Stefan Schoek (schoek@ibr.cs.tu-bs.de).
 *
 * Copyright (c) 1994, 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "scotty.h"

#ifdef TCL_MSDOS_PORT
struct timezone {
    int dummy;
};
#endif

/*
 * Structure used to describe a job.
 */

typedef struct Job {
    char name[20];		/* The unique name of the job. */
    char *cmd;			/* The command to evaluate. */
    char *newCmd;		/* The new command to replaces the current. */
    int interval;		/* The time interval value in ms. */
    int remtime;		/* The remaining time in ms. */
    int iterations;		/* The number of iterations of this job. */
    int status;			/* The status of this job (see below). */
    Tcl_HashTable attributes;	/* The has table of job attributes. */
    struct Job *nextPtr;	/* Next job in our queue. */
} Job;

/*
 * The list of known jobs.
 */

static Job *jobList = NULL;

/* 
 * These are all possible stati of an existing job.
 */

#define SUSPEND 0
#define WAITING 1
#define RUNNING 2
#define EXPIRED 3

/* 
 * The last time the scheduler completed operation.
 */

static struct timeval lastTime = {0, 0};

/*
 * The pointer to the currently running job.
 */

static Job *currentJob = NULL;

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
ScheduleProc	_ANSI_ARGS_((ClientData clientData));

static void 
NextSchedule	_ANSI_ARGS_((Tcl_Interp *interp));

static void 
AdjustTime	_ANSI_ARGS_((void));

static void
Schedule	_ANSI_ARGS_((Tcl_Interp *interp));

static int
CreateJob	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static void
DestroyJob	_ANSI_ARGS_((ClientData clientData));

static int
Info		_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
Current		_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
Wait		_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
Attributes	_ANSI_ARGS_((Job *jobPtr, Tcl_Interp *interp, 
			     int argc, char **argv));
static int
JobCmd		_ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp, 
			     int argc, char **argv));

/*
 * ScheduleProc() calls the job scheduler. This function is the
 * callback for the Tk even loop.
 */

static void
ScheduleProc (clientData)
     ClientData clientData;
{
    Tcl_Interp *interp = (Tcl_Interp *) clientData;

    Schedule(interp);
}

/*
 * NextSchedule() calls the scheduler again after time
 * milliseconds. An internal token is used to make sure that we do not
 * have more than one timer event pending for the job scheduler.
 */

static void
NextSchedule (interp)
     Tcl_Interp *interp; 
{
    static Tk_TimerToken scheduleEvent = NULL;
    Job *jobPtr;
    int ms = -1;

    if (scheduleEvent != NULL) {
	Tk_DeleteTimerHandler(scheduleEvent);
	scheduleEvent = NULL;
    }

    /* Calculate the minimum of the remaining times of all not jobs
     * waiting. Tell the event manager to call us again if we found
     * a waiting job with remaining time >= 0. We also wait for 
     * expired jobs so that they will get removed from the job list.
     */

    ms = -1;
    for (jobPtr = jobList; jobPtr != NULL; jobPtr = jobPtr->nextPtr) {
        if (jobPtr->status == WAITING || jobPtr->status == EXPIRED) {
	    if (ms < 0 || jobPtr->remtime < ms) {
		ms = (jobPtr->remtime < 0) ? 0 : jobPtr->remtime;
	    }
	}
    }
    
    if (ms < 0) {
	lastTime.tv_sec = 0;
	lastTime.tv_usec = 0;
    } else {
        scheduleEvent = Tk_CreateTimerHandler(ms, ScheduleProc, 
					      (ClientData) interp);
    }
}

/*
 * AdjustTime() updates the remaining time of all jobs in the queue
 * that are not suspended and sets lastTime to the current time.
 */

static void
AdjustTime ()
{
    struct timeval currentTime;
    int delta;
    Job *jobPtr;

    if (! lastTime.tv_sec && ! lastTime.tv_usec) {
	(void) gettimeofday(&lastTime, (struct timezone *) NULL);
	return;
    }

    (void) gettimeofday(&currentTime, (struct timezone *) NULL);

    delta = (currentTime.tv_sec - lastTime.tv_sec) * 1000 
	    + (currentTime.tv_usec - lastTime.tv_usec) / 1000;

    lastTime = currentTime;

    for (jobPtr = jobList; jobPtr; jobPtr = jobPtr->nextPtr) {
        if (jobPtr->status != SUSPEND) {
	    jobPtr->remtime -= delta;
	}
    }
}

/*
 * Schedule() checks for jobs that must be processed. It finally
 * tells the event mechanism to repeat itself when the next job needs
 * attention. This function also cleans up the job queue by removing
 * all jobs that are waiting in the state expired.
 */

static void
Schedule (interp)
    Tcl_Interp *interp;
{
    Job *jobPtr;

    /*
     * Refresh the remaining time of the active jobs.
     */

    AdjustTime();

    /*
     * Execute waiting jobs with remaining time less or equal 0. 
     * Set the job status to expired if the number of iterations
     * reaches zero.
     */

    for (jobPtr = jobList; jobPtr != NULL; jobPtr = jobPtr->nextPtr) {

	if (jobPtr->newCmd) {
	    ckfree(jobPtr->cmd);
	    jobPtr->cmd = jobPtr->newCmd;
	    jobPtr->newCmd = NULL;
	}

	if ((jobPtr->status == WAITING) && (jobPtr->remtime <= 0)) {

	    int code;

	    currentJob = jobPtr;
	    jobPtr->status = RUNNING;

	    Tcl_AllowExceptions(interp);
	    code = Tcl_GlobalEval(interp, jobPtr->cmd);
	    if (code == TCL_ERROR) {
		Tcl_AddErrorInfo(interp, 
				 "\n    (script bound to job - job deleted)");
		Tk_BackgroundError(interp);
		jobPtr->status = EXPIRED;
	    }
    
	    Tcl_ResetResult(interp);
	    if (jobPtr->status == RUNNING) {
		jobPtr->status = WAITING;
	    }
	    currentJob = NULL;
	    
	    jobPtr->remtime = jobPtr->interval;
	    if (jobPtr->iterations > 0) {
		jobPtr->iterations--; 
		if (jobPtr->iterations == 0) {
		    jobPtr->status = EXPIRED;
		}
	    }
	}
    }

    /*
     * Delete all jobs which have reached the status expired.
     * We must restart the loop for every deleted job as the
     * job list is modified by calling Tcl_DeleteCommand().
     */

  repeat:
    for (jobPtr = jobList; jobPtr != NULL; jobPtr = jobPtr->nextPtr) {
        if (jobPtr->status == EXPIRED) {
#if 0
	    char cmd[40];
	    sprintf(cmd, "event raise %s", job->name);
	    Tcl_GlobalEval(interp, cmd);
	    Tcl_ResetResult(interp);
#endif
	    Tcl_DeleteCommand(interp, jobPtr->name);
	    goto repeat;
        }
    }
    
    /*
     * Compute and subtract the time needed to execute the jobs
     * and schedule the next pass through the scheduler.
     */

    AdjustTime();
    NextSchedule(interp);
}

/*
 * CreateJob() creates a new job and puts it in the jobList.
 */

static int
CreateJob (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    static unsigned lastid = 0;
    int interval;
    int iterations = 0;
    Job *jobPtr, *p;

    if (argc < 4 || argc > 5) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " create command interval ?iterations?\"", 
			 (char *) NULL);        
        return TCL_ERROR;
    }

    if (Tcl_GetInt(interp, argv[3], &interval) != TCL_OK) {
        return TCL_ERROR;
    }

    if ((argc > 4) && (Tcl_GetInt(interp, argv[4], &iterations) != TCL_OK)) {
        return TCL_ERROR;
    }

    jobPtr = (Job *) ckalloc(sizeof(Job));
    memset ((char *) jobPtr, '\0', sizeof(Job));
    sprintf(jobPtr->name, "job%d", lastid++);
    jobPtr->cmd = ckstrdup(argv[2]);
    jobPtr->interval = interval;
    jobPtr->iterations = iterations;
    jobPtr->status = WAITING;
    Tcl_InitHashTable(&(jobPtr->attributes), TCL_STRING_KEYS);

    /*
     * Put the new job in our job list. We add it at the end
     * to preserve the order in which the jobs were created.
     */

    if (jobList == NULL) {
        jobList = jobPtr;
    } else {
        for (p = jobList; p->nextPtr != NULL; p = p->nextPtr) ;
	p->nextPtr = jobPtr;
    }

    /*
     * Create a new scheduling point for this new job.
     */

    NextSchedule(interp);

    /*
     * Create a new Tcl command for this job object.
     */

    Tcl_CreateCommand(interp, jobPtr->name, JobCmd,
		      (ClientData) jobPtr, DestroyJob);
    Tcl_SetResult(interp, jobPtr->name, TCL_STATIC);
    return TCL_OK;
}

/*
 * DestroyJob() removes the job for the list of active jobs and releases
 * all memory associated with an job object.
 */

static void
DestroyJob (clientData)
     ClientData clientData;
{
    Job *jobPtr = (Job *) clientData;
    Job **jobPtrPtr = &jobList;
    
    while (*jobPtrPtr && (*jobPtrPtr) != jobPtr) {
	jobPtrPtr = &(*jobPtrPtr)->nextPtr;
    }

    if (*jobPtrPtr) {
	(*jobPtrPtr) = jobPtr->nextPtr;
    }

    Tcl_DeleteHashTable(&(jobPtr->attributes));

    ckfree(jobPtr->cmd);
    if (jobPtr->newCmd) ckfree(jobPtr->newCmd);
    ckfree((char *) jobPtr);
}

/*
 * Info() returns a list of all known jobs.
 */

static int
Info (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Job *jobPtr;

    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " info\"", (char *) NULL);        
        return TCL_ERROR;
    }

    for (jobPtr = jobList; jobPtr; jobPtr = jobPtr->nextPtr) {
	Tcl_AppendElement(interp, jobPtr->name);
    }

    return TCL_OK;
}

/*
 * Current() returns the id of the currently running job or just an
 * empty string. This is needed e.g. to let a job kill itself.
 */

static int
Current (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " current\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (currentJob) {
	Tcl_SetResult(interp, currentJob->name, TCL_STATIC);
    }

    return TCL_OK;
}

/*
 * Wait() processes events until either all jobs have been 
 * removed from the job queue. We have to restart our loop
 * through the job list as it might get modified by doing
 * an event.
 */

static int
Wait (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Job *jobPtr;

    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " wait\"", (char *) NULL);
        return TCL_ERROR;
    }

  repeat:
    for (jobPtr = jobList; jobPtr; jobPtr = jobPtr->nextPtr) {
        if (jobPtr->status == WAITING) {
	    Tk_DoOneEvent(0);
	    goto repeat;
	}
    }

    return TCL_OK;
}

/*
 * Attributes() returns or sets job attributes.
 */

static int
Attributes (jobPtr, interp, argc, argv)
     Job *jobPtr;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Tcl_HashTable *tablePtr = &(jobPtr->attributes);
    Tcl_HashEntry *entryPtr;
    Tcl_HashSearch search;

    if (argc < 2 || argc > 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " attribute ?name ?value??\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (argc == 4) {
	int isnew;
	entryPtr = Tcl_CreateHashEntry(tablePtr, argv[2], &isnew);
	if (! isnew) {
	    ckfree((char *) Tcl_GetHashValue (entryPtr));
	}
	Tcl_SetHashValue(entryPtr, ckstrdup (argv[3]));
    } else if (argc == 3) {
	entryPtr = Tcl_FindHashEntry(tablePtr, argv[2]);
	if (entryPtr) {
	    Tcl_SetResult(interp, Tcl_GetHashValue (entryPtr), TCL_STATIC);
	}
    } else {
	entryPtr = Tcl_FirstHashEntry(tablePtr, &search);
	while (entryPtr) {
	    Tcl_AppendElement(interp, Tcl_GetHashKey (tablePtr, entryPtr));
	    entryPtr = Tcl_NextHashEntry(&search);
	}
    }

    return TCL_OK;
}

/*
 * JobCmd() implements all operations on a job object.
 */

static int
JobCmd (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Job *jobPtr = (Job *) clientData;
    int len;

    if (argc < 2) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " option ?args?\"", (char *) NULL);
        return TCL_ERROR;
    }

    len = strlen(argv[1]);

    if (strncmp(argv[1], "attribute", len) == 0) {

	return Attributes(jobPtr, interp, argc, argv);

    } else if (strcmp(argv[1], "status") == 0) {

	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " status ?value?\"", (char *) NULL);
	    return TCL_ERROR;
	}

        if (argc == 3) {

	    int status = 0;

	    if (strcmp(argv[2], "suspended") == 0) {
	        status = SUSPEND;
	    } else if (strcmp(argv[2], "waiting") == 0) {
	        status = WAITING;
	    } else if (strcmp(argv[2], "running") == 0) {
	        /* No typo! A user can not switch to another job. */
	        status = WAITING;
	    } else if (strcmp(argv[2], "expired") == 0) {
	        status = EXPIRED;
	    } else {
	        Tcl_AppendResult(interp, "unknown status \"", argv[2], 
				 "\": should be suspended, waiting, ",
				 "running, or expired", (char *) NULL);
		return TCL_ERROR;
	    }

	    jobPtr->status = status;

	    /*
	     * Compute the current time offsets and create a new scheduling
	     * point. A suspended job may have resumed and we must make sure
	     * that our scheduler is running.
	     */

	    AdjustTime();
	    NextSchedule(interp);
	}

	switch (jobPtr->status) {
	case SUSPEND: 
	    Tcl_SetResult(interp, "suspended", TCL_STATIC);
	    break;
	case WAITING:
	    Tcl_SetResult(interp, "waiting", TCL_STATIC);
	    break;
	case RUNNING:
	    Tcl_SetResult(interp, "running", TCL_STATIC);
	    break;
	case EXPIRED:
	    Tcl_SetResult(interp, "expired", TCL_STATIC);
	    break;
	}
	return TCL_OK;

    } else if (strncmp(argv[1], "command", len) == 0) {

	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " command ?value?\"", (char *) NULL);
	    return TCL_ERROR;
	}

        if (argc == 3) {
	    if (jobPtr->newCmd) ckfree(jobPtr->newCmd);
	    jobPtr->newCmd = ckstrdup(argv[2]);
	}
	Tcl_SetResult(interp, jobPtr->newCmd ? jobPtr->newCmd : jobPtr->cmd, 
		      TCL_STATIC);
	return TCL_OK;

    } else if (strncmp(argv[1], "interval", len) == 0) {

	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " interval ?value?\"", (char *) NULL);
	    return TCL_ERROR;
	}

        if (argc == 3) {
	    int val;
	    if (Tcl_GetInt(interp, argv[2], &val) != TCL_OK) {
	        return TCL_ERROR;
	    }
	    if (val < 0) {
	        Tcl_SetResult(interp, "negativ interval", TCL_STATIC);
		return TCL_ERROR;
	    }
	    jobPtr->interval = val;
	}
	sprintf(interp->result, "%d", jobPtr->interval);
	return TCL_OK;        

    } else if (strncmp(argv[1], "iterations", len) == 0) {

	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " iterations ?value?\"", (char *) NULL);
	    return TCL_ERROR;
	}

        if (argc == 3) {
	    int val;
	    if (Tcl_GetInt(interp, argv[2], &val) != TCL_OK) {
	        return TCL_ERROR;
	    }
	    if (val < 0) {
	        Tcl_SetResult(interp, "negativ # of iterations", TCL_STATIC);
		return TCL_ERROR;
	    }
	    jobPtr->iterations = val;
	}
	sprintf(interp->result, "%d", jobPtr->iterations);
	return TCL_OK;

    } else if (strncmp(argv[1], "time", len) == 0) {

	if (argc != 2) {
            Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " time\"", (char *) NULL);
            return TCL_ERROR;
        }

	AdjustTime();
	sprintf(interp->result, "%d", jobPtr->remtime);
        return TCL_OK;

    } else if (strncmp(argv[1], "wait", len) == 0) {

	if (argc > 2) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " wait\"", (char *) NULL);
	    return TCL_ERROR;
	}

      repeat:
	for (jobPtr = jobList; jobPtr; jobPtr = jobPtr->nextPtr) {
	    if (jobPtr->status == WAITING 
		&& (strcmp(jobPtr->name, argv[0]) == 0)) {
		Tk_DoOneEvent(0);
		goto repeat;
	    }
	}

	return TCL_OK;
    }

    Tcl_AppendResult(interp, "bad option \"", argv[1],
		     "\": should be attribute, command, interval, ",
		     "iterations, status, or time", (char *) NULL);
    return TCL_ERROR;
}


/*
 * Scotty_JobCmd() implements the job command as described in the
 * scotty documentation. It is used to create jobs and to retrieve
 * information about all running jobs.
 */

int
Scotty_JobCmd (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    int length;
    char c;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    c = argv[1][0];
    length = strlen(argv[1]);

    if (strncmp(argv[1], "create", length) == 0) {
	return CreateJob(interp, argc, argv);

    } else if (strncmp(argv[1], "info", length) == 0) {
        return Info(interp, argc, argv);

    } else if (strncmp(argv[1], "current", length) == 0) {
        return Current(interp, argc, argv);

    } else if (strncmp(argv[1], "wait", length) == 0) {
        return Wait(interp, argc, argv);

    } else if (strncmp(argv[1], "schedule", length) == 0) {
        Schedule(interp);
	return TCL_OK;
    }

    Tcl_AppendResult(interp, "bad option \"", argv[1], 
		     "\": should be create, info, list, wait, or schedule",
		     (char *) NULL);
    return TCL_ERROR;
}
