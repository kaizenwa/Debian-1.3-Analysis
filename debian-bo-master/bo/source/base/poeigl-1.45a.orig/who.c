/* who.c - simple utility to display the active users */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <utmp.h>
#include "pathnames.h"

int main(int argc, char *argv[])
{
    struct utmp *ut;
    char *ttyn;
    
    utmpname(_PATH_UTMP);
    setutent();
    
    if(argc == 3 && strcmp(argv[1], "am") == 0 && strcmp(argv[2], "i") == 0) {
	ttyn = ttyname(0);
	while((ut = getutent())) {
	    if(strcmp(ttyn+5, ut->ut_line) == 0 && ut->ut_type == USER_PROCESS)
	      printf("%-8s\n", ut->ut_user);
	} 
	endutent();
	exit(0);
    }

    printf("User     TTY Login-time\n");
    while((ut = getutent())) {
	if(ut->ut_name[0] && ut->ut_type == USER_PROCESS)
	  printf("%-8s  %-2s %s", ut->ut_user, ut->ut_id,
		 ctime(&ut->ut_time));
    }
    endutent();
    exit(0);
}
	
