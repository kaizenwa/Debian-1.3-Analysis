/*
	System: Structured text retrieval tool sgrep.
	Module: preproc.c
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: Handles preprocessing of expressions using some
		     external macro expanding program. ( like m4 or cpp )
		     used through function preprocess()
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/

/*
 * NOTE: Inbuild preprocessed has been planned but implemented. It's not
 *       clear whether it would be useful.
 */
  
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <limits.h>
#include <errno.h>
#include "defines.h"

#ifndef PIPE_BUF
 #define PIPE_BUF 4096
#endif

/*
 * preprocess preprocesses given input string using given preprocessor.
 * maxsize specifies maximum size of output string.
 * if processor==NULL inbuilt default processor is used instead
 * if processor=="-" no preprocessing is done
 * returns size of output_str
*/
int preprocess( char *input_str, char *output_str, char *processor, int maxsize )
{
#ifdef USE_EXEC
	int pfd1[2];
	int pfd2[2];
	int i,j,r,s,p;
	int t;
	int status;
	fd_set rfs;
	fd_set wfs;
#endif
	
#ifdef DEBUG
	fprintf(stderr,"Preprocess string: %s\n",input_str);
#endif

	if ( processor==NULL )
	{
		fprintf(stderr,"Inbuilt preprocessor not implemented yet.\n");
		processor="-";
	}
	
	if ( strcmp(processor,"-")==0 )
	{
		/* No processing, just return copy of input_str */
#ifdef DEBUG
		fprintf(stderr,"preprocessor: just copy to output\n");
#endif

#ifdef assert
		assert((int)strlen(input_str)<maxsize);
#endif
		strcpy(output_str,input_str);
		return 0;
	}
	
#ifndef USE_EXEC
	fprintf(stderr,"Spawning external preprocessors not compiled in.\n");
	exit(2);
#endif

#ifdef USE_EXEC

#ifdef DEBUG
	fprintf(stderr,"Spawning preprocessor '%s'\n",processor);
#endif
	if ( pipe(pfd1)!=0 || pipe(pfd2)!=0 )
	{
		perror("pipe");
		exit(2);
	}
	fflush(stderr);
	fflush(stdout);
	if ( (p=fork())==-1 )
	{
		perror("fork");
		exit(2);
	}
	if (p==0)
	{
		if (dup2(pfd1[0],0)==-1) exit(127);
		if (dup2(pfd2[1],1)==-1) exit(127);
		close(pfd1[1]);
		close(pfd1[0]);
		close(pfd2[1]);
		close(pfd2[0]);
#ifdef DEBUG
		fprintf(stderr,"child: execl(%s,%s,%s,%s,NULL)\n",EXEC_SHELL,processor);
		fflush(stderr);
#endif
		execl(EXEC_SHELL,processor,NULL);
		perror("sgrep preprocessor child");
		exit(127);
	}
	close(pfd1[0]);
	close(pfd2[1]);
	r=-2;
	i=0;
	j=0;
	while ( r!=0 )
	{	
		FD_ZERO(&rfs);
		FD_ZERO(&wfs);
		FD_SET(pfd2[0],&rfs);
		if ( input_str[i] ) 
			FD_SET(pfd1[1],&wfs);

		s=select( (pfd1[1]>pfd2[0]) ? pfd1[1]+1:pfd2[0]+1 ,&rfs,&wfs,NULL,NULL);
#ifdef DEBUG
		fprintf(stderr,"write %d read %d\r",i,j);
		fflush(stderr);
#endif
		if (s==-1 && errno!=EINTR)
		{
			kill(p,SIGTERM);
			perror("select");
			exit(2);
		}
		if ( s>0 && FD_ISSET(pfd1[1],&wfs) && input_str[i])
		{
			t=( (PIPE_BUF < (int)strlen(&input_str[i])) ? 
				PIPE_BUF:strlen(&input_str[i]) );
			if ( (t=write(pfd1[1],&input_str[i],t))==-1 )
			{
				kill(p,SIGTERM);
				perror("write to child");
				exit(2);
			}
			i+=t;
			if (!input_str[i]) 
				close(pfd1[1]);
		}
		if ( s>0 && FD_ISSET(pfd2[0],&rfs) )
		{
			r=read(pfd2[0],&output_str[j],maxsize-j);
			if ( r<0 )
			{
				kill(p,SIGTERM);
				perror("read from child");
				exit(2);
			}
			j+=r;
			if ( j>=maxsize )
			{
				kill(p,SIGTERM);
				fprintf(stderr,"%s (>%d)\n",
	"Preprocessor output exceeded maximum output size",maxsize);
				exit(2);
			}
			output_str[j]=0;	
		}	
	}	
	if (input_str[i])
	{
		close(pfd1[1]);
		/* Preprocessor didn't read all it's input.
		   should it be terminated ? */	
		/* kill(p,SIGTERM); */
		/* Should there be a warning ? */
#ifdef DEBUG		
		fprintf(stderr,"Warning: Preprocessor didn't read all it's input\n");
#endif
		/* Should we stop ? */
                /* exit(2); */
	}
	wait(&status);
	if ( !WIFEXITED(status) )
	{
		fprintf(stderr,"Preprocessor died abnormally\n");
		exit(2);
	}
	if ( WEXITSTATUS(status)==127 )
	{
		fprintf(stderr,"exec failed\n");
		exit(2);
	}
	if ( WEXITSTATUS(status)!=0 )
	{
		fprintf(stderr,"Preprocessor returned exit status %d\n",
			WEXITSTATUS(status));
		exit(2);
	}
#ifdef DEBUG
	fprintf(stderr,"Preprocessor output:%s",output_str);
#endif	
	return strlen(output_str);
#endif
}
