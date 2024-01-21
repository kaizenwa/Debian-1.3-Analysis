#include "config.h"
#include "jed-feat.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>
#include <process.h>
#include <errno.h>
#include <sys\stat.h>

char err_buf[128];

void msg_error(char *msg)
{
   strcpy(err_buf, msg);
}

static char *shell = NULL, *shell_switch = NULL;
static void discover_shell(void)
{
#ifdef SH_SHELL			       /* maybe some day ... */
   shell_switch = "-c";
   if ( (shell = getenv("SHELL")) != NULL )  return;
#endif
   shell_switch = "/c";
   if ( (shell = getenv("COMSPEC")) != NULL ) return;
   shell = "command.com";					/* absolute last resort! */   
}

/* ///////////////////////////////////////////////////////////////////
//  Function:   static int parse_command_line( int *argc, char ***argv,
//					       char **fname,
//					       char *command_line );
//  Descript:	parse command_line
//
//  Returns:	the handles to redirect
//
//  Caveat:	requires spaces to separate each argument
//		ie., args>filename	will NOT work
///////////////////////////////////////////////////////////////////// */
#define DIRECT_SPAWN 0x1000	       /* some arbitrary number */
static int
parse_command_line( int *argc, char ***argv, char **fname, char *command_line )
{
   int count, handles = 0;
   char *pt;

   discover_shell();		       /* find which shell to use */
   while ( (*command_line != '\0') && (*command_line == ' '))
     command_line++;		       /* start on 1st non-space */

   if ( *command_line == '!' ) {
      handles = DIRECT_SPAWN;
      command_line++;		       /* advance past '!' */
      while ((*command_line != '\0') && (*command_line == ' '))
			command_line++;		       /* start on next non-space */
   }

   pt = command_line;
   count = 0;
   while ( *pt != '\0' ) {
      count++;			       /* this is an argument */
      while ((*pt != '\0') && (*pt != ' '))  
	{
	   if ( *pt == '|' )	       /* cannot spawn directly */
	     handles = 0;		       /* need shell for pipes */
	   pt++;			       /* advance until a space */
      }
      while ( *pt == ' '  )
      	pt++;			       /* advance until a non-space */
   }

	*argv = (char **) malloc( (count+3) * sizeof(char *) );
   if ( *argv == NULL )
     return 0;			       /* malloc error */

   *argc = 0;   
   if ( !(handles & DIRECT_SPAWN) ) {
      (*argv)[ *argc ] = shell;
      (*argc)++;
      if ( count > 0 )  {
	 (*argv)[ *argc ] = shell_switch;
	 (*argc)++;
      }
      count += (*argc);
   }

   pt = command_line;
   while ((*argc < count) && (*pt != '\0')) {
      (*argv)[ *argc ] = pt;
      (*argc)++;
      while ( *pt != '\0' && *pt != ' ' )
      	pt++;			/* advance until a space */
      if ( *pt != '\0' )
      	*(pt++) = '\0';		/* parse argument here */
      while ( *pt == ' ')
      	pt++;		        /* advance until a non-space */
   }
   (*argv) [ *argc ] = (char *) NULL;	/* NULL terminated list */

/*  now examine the arguments for '>' redirect */

   for ( count = 0; count < *argc; count++ ) {
      for ( pt = (*argv)[count]; *pt && *pt != '>'; pt++ )
	 /* find '>' char */;
      if ( *pt == '>' ) {
	 if ( pt == (*argv)[count] ) {
		 handles |= 0x01;
	 } else {
	    switch ( *(--pt) ) {
	     case '1':
	       handles |= 0x01;
	       break;
	     case '2':
	       handles |= 0x02;
	       break;
	     case '&':
	       handles |= 0x03;
	       break;
	    }
	 }
	 (*argv)[count] = NULL;		/* remove from the list */
	 count++;			/* file name follows '>' */
	 if ( (*argv)[count] != NULL )
	   *fname = (*argv)[count];
      }
   }
   if ((*fname == NULL) || (**fname == '\0' ))
     handles = 0x00;		/* don't redirect if there is no name */

   return handles;
}

/* ///////////////////////////////////////////////////////////////////
 //  Function:   int sys_System(char *command_line);
 //
 //  Descript:	shell wrapper that understands some common redirection syntax
 //		command args  > filename		; stdout
 //		command args 1> filename		; stdout
 //		command args 2> filename		; stderr
 //		command args &> filename		; stdout+stderr
 //		command args  > filename 2>&1		; stdout+stderr
 //
 // additionally, if command is prefixed by a '!', then the command is
 // spawned directly without invoking the shell
 //
 //  Returns:	returns error codes as per spawn*()
 //
 //  Caveat:	requires spaces to separate each argument
 //		ie., command args>filename	will NOT work
 ///////////////////////////////////////////////////////////////////// */

static int execute_the_command (char **, int, char *);

int sys_System(char *command_line)
{
   int ret = -1, handles, argc = 0;
   char *fname = NULL, **argv = NULL;
   
	handles = parse_command_line( &argc, &argv, &fname, command_line );
   
   if (argc)
      {
	 ret = execute_the_command (argv, handles, fname);
      }
   
   return ret;
}

static int execute_the_command (char **argv, int handles, char *file)
{
   int ret = 0;
   int fd1 = -1, fd2 = -1;
   int fd_err = -1, fd_out = -1;
   
   
   if (handles & 1)
      {
	 /* save stdout file handle */
	 fd1 = dup (fileno (stdout));
	 
	 if (fd1 == -1) 
	    {
	       msg_error ("Unable to dup stdout");
	       return -1;
	    }
	 
	 fd_out = open (file,
			O_CREAT | O_TRUNC | O_TEXT | O_WRONLY | O_APPEND,
			S_IREAD | S_IWRITE);
	 
	 if ((fd_out == -1) || (-1 == dup2 (fd_out, fileno (stdout))))
	    {
	       msg_error ("Unable to redirect stdout!");
	       ret = -1;
	    }
      }
   
   if (handles & 0x2)		       /* stderr */
      {
	 /* save stderr file handle */
	 fd2 = dup (fileno (stderr));
	 
	 if (fd2 == -1) 
	    {
	       msg_error ("Unable to dup stderr");
	       return -1;
	    }
	 
	 if (fd_out == -1)
	    {
	       fd_err = open (file,
			      O_CREAT | O_TRUNC | O_TEXT | O_RDWR | O_APPEND,
			      S_IREAD | S_IWRITE);
	    }
	 else fd_err = fd_out;
	 
	 if ((fd_err == -1) || (-1 == dup2 (fd_err, fileno (stderr))))
	    {
	       msg_error ("Unable to redirect stderr!");
	       ret = -1;
	    }
      }
   
   if (fd_out != -1) close (fd_out);
   if ((fd_err != -1) && (fd_err != fd_out)) close (fd_err);
   
   
   if (ret == 0)
      {
	 ret = spawnvp(P_WAIT, argv[0], (void *) argv);
	 
	 if (-1 == ret ) 
	    {
	       switch(errno) 
		  {
		   case ENOENT:
		     if (handles & DIRECT_SPAWN ) msg_error("Command not found.");
		     else msg_error("Shell not found.");
		     break;
		   case ENOMEM:
		     msg_error("Insufficient Memory.");
		     break;
		   default:
		     msg_error("Unknown Error.");
		  }
	    }
      }
   
   if (fd1 != -1)
      {
	 if (-1 == dup2 (fd1, fileno (stdout)))
	    {
	       msg_error ("Unable to reattach stdout");
	    }
	 close (fd1);
      }
   
   if (fd2 != -1)
      {
	 if (-1 == dup2 (fd2, fileno (stderr)))
	    {
	       msg_error ("Unable to reattach stderr");
	    }
	 close (fd2);
      }
   
   return ret;
}


int main(int argc, char **argv)
{
   char *command_line, *p;
   int   i, j;
   
   FILE *f;
   int ret;
   
   err_buf[0] = 0;
   
   for(i = j = 1; i < argc; i++)
     j += strlen(argv[i]) + 1;
   
   if (NULL == (p = command_line = malloc(j + 1)))
     {
	msg_error("Malloc error");
	ret = -1;
     }
   else
     {
	for(i = 1; i < argc; i++)
	  {
	     strcpy(p, argv[i]);
	     p += strlen(argv[i]);
	     *p++ = ' ';
	  }
	*p = 0;
	
	ret = sys_System(command_line);
	free(command_line);
     }
   
   f = fopen("jedshell.tmp", "w");
   if (f == NULL)
     {
	/* I realize that this does nothing... */
	msg_error ("Unable to open tmp file.");
	return -1;
     }
   
   fprintf(f, "%d\n%s", ret, err_buf);
   fclose(f);
   return 0;
}
