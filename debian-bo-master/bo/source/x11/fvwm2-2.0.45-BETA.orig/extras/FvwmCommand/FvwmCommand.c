/*
  Fvwm command input interface.
 
  Copyright 1996, Toshi Isogai. No guarantees or warantees or anything
  are provided. Use this program at your own risk. Permission to use 
  this program for any purpose is given,
  as long as the copyright is kept intact. 
*/

#include "FvwmCommand.h"

int  s;    /* socket handle */
FILE *sp;
char *MyName;  /* name of this program at executing time */


/******************************************/
/*  close socket and exit */
/******************************************/
void CloseSocket () {
  fclose(sp);
  exit(0);
}

/************************************/
/* print error message on stderr */
/************************************/
void ErrMsg( char *msg ) {
  fprintf( stderr, "[FVWM][%s] <<Error>> %s, errno %d\n", MyName, msg,errno );
  CloseSocket();
  exit(1);
}


/*******************************************************/
/* setup socket.                                       */
/* send command to and receive message from the server */
/*******************************************************/
void main ( int argc, char *argv[]) {
  char *cmd;
  struct sockaddr_in sin;
  int  clen;
  int i;

  signal (SIGINT, CloseSocket);  
  signal (SIGQUIT, CloseSocket);  

  MyName=strrchr(argv[0], '/');
  if (MyName != NULL) {
    MyName++;
  }

  /* make a socket */
  if( (s = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
	ErrMsg ("socket");
  }
  memset((char *) &sin, 0, sizeof(sin));   

  sin.sin_family = AF_INET;
  sin.sin_port = htons(PORT_CMD);
  sin.sin_addr.s_addr = htonl(INADDR_ANY); 

  if( connect( s, (struct sockaddr *)&sin, sizeof(sin) )< 0 ) {
	ErrMsg( "connect" );
  }

  sp = fdopen( s, "r" );

  /* loop to get user's command and to send it to server */
  for( i=1;i < argc; i++ ) {
	cmd = safemalloc(strlen(argv[i])+1);
	strcpy( cmd, argv[i] );
	if( cmd[0] == '\0'  ) {
	  continue;
	} else {
	   clen = strlen(cmd);

	  /* add crlf */
	  if( cmd[clen-1] != '\n' ) {
		strcat(cmd, "\n");
		clen++;
	  }
	  if( clen != 1 ) {
		/* send the command to the server if not empty */
		send( s, cmd, clen+1, 0 );
	  }
	}
	free(cmd);
  }
  CloseSocket();

}


