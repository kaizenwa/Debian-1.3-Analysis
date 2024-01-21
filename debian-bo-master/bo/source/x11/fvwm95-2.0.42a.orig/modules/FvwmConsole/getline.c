#include "FvwmConsole.h"

static   char cmd[256];

#ifndef HAVE_READLINE
/* no readline - starts here */
char *getline() {
  if( fgets(cmd,256,stdin) == NULL  ) {
	return(NULL);
  }
  return(cmd);
}

#else 
/* readline - starts here */
#include <readline/readline.h>

#define PROMPT ""

char *getline() {
  char *line;

  /* If the buffer has already been allocated, return the memory to the free pool. */
  if (line != (char *)NULL) {
	free (line);
	line = (char *)NULL;
  }
                             
  /* Get a line from the user. */
  line  = readline (PROMPT);
     
  if( line == NULL ) {
	return(NULL);
  } 

	/* If the line has any text in it, save it on the history. */
	if (*line != '\0')
	  add_history (line);
	
	/* add cr at the end*/
	strncpy( cmd, line, 254 );
	strcat( cmd, NEWLINE ); 

  return (cmd);

}
/* readline - end here */
#endif


