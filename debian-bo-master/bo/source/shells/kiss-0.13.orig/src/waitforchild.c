#include "kiss.h"

static void reportstatus (char *childname, int status, int background)
{
    if (WIFEXITED (status))
	laststatus = WEXITSTATUS (status);
    else if (WIFSIGNALED (status) && !background)
	warning ("%s got signalled with sig %d", childname,
		 WTERMSIG (status));
    else if (WCOREDUMP (status))
	warning ("%s dumped core", childname);
    else if (WIFSTOPPED (status))
	warning ("%s stopped due to sig %d",
		 childname,  WSTOPSIG (status));
}

void waitforchild (char *childname, int pid, int background)
{
    int
	status;
    char
	buf [LINELEN];

    lastchildpid = pid;
    
    if (background)
	printf ("[%d]\n", pid);

    /* let's see how this child is doing */
    if (waitpid (pid, &status, background ? WNOHANG : 0) != -1)
	reportstatus (childname, status, background);
    
    /* let's see how generic kids are doing */
    while ( (pid = waitpid (WAIT_ANY, &status, WNOHANG)) > 0 )
    {
	sprintf (buf, "pid %d", pid);
	reportstatus (buf, status, 1);
    }
}
