/*
  Fvwm command input interface.
 
  Copyright 1996, Toshi Isogai. No guarantees or warantees or anything
  are provided. Use this program at your own risk. Permission to use 
  this program for any purpose is given,
  as long as the copyright is kept intact. 
*/

#include "FvwmCommand.h"

char *MyName;

int fd[2];  /* pipe to fvwm */
int  s,ns;             /* socket handles */
char *name; /* name of this program in executable format */
int  pid;

void server(int *fd);
void GetResponse(); 
void DeadPipe();
void CloseSocket();
void ErrMsg( char *msg );
void SigHandler();

void main(int argc, char *argv[]){
  char *tmp, *tmp2;

  /* Save the program name - its used for error messages and option parsing */
  tmp = argv[0];

  tmp2=strrchr(argv[0], '/');
  if (tmp2 != NULL)
    tmp = tmp2 + 1;

  name = safemalloc(strlen(tmp)+1);
  strcpy( name, tmp );

  MyName = safemalloc(strlen(tmp)+2);
  strcpy(MyName,"*");
  strcat(MyName, tmp);

  if(argc < FARGS)    {
	fprintf(stderr,"%s Version %s should only be executed by fvwm!",MyName,
			S_VERSION);
	exit(1);
  }

  pid = 0;
  /* Dead pipes mean fvwm died */
  signal (SIGPIPE, DeadPipe);  
  signal (SIGINT, SigHandler);  
  signal (SIGQUIT, SigHandler);  

  fd[0] = atoi(argv[1]);
  fd[1] = atoi(argv[2]);

  pid = fork();
  if( pid == -1 ) {
	ErrMsg(  "fork");
	exit(1);
  }
  if( pid == 0 ) {
	server(fd);
  }
  while(1) {
	GetResponse(); 
  }
}

/**********************************************/
/* read fvwm packet and pass it to the client */
/**********************************************/
void GetResponse() {
  fd_set in_fdset;
  unsigned long *body;
  unsigned long header[HEADER_SIZE];

  FD_ZERO(&in_fdset);
  FD_SET(fd[1],&in_fdset);
  
  /* ignore anything but error message */
  if( ReadFvwmPacket(fd[1],header,&body) > 0)	 {
	if( header[1] == M_PASS )	     { 
	  send( ns, (char *)&body[3], strlen((char *)&body[3]), 0 ); 
	} 
	free(body);
  }
}

/***********************************************************************
 *	signal handler
 ***********************************************************************/
void DeadPipe() {
  fprintf(stderr,"%s: dead pipe\n", name);
  CloseSocket();
  exit(0);
}

void SigHandler() {
  CloseSocket();
  exit(1);
}

/*********************************************************/
/* close sockets                                         */
/*********************************************************/
void CloseSocket() {
  close(ns);     /* remove the socket */
  close(s);
  close(fd[0]);
  close(fd[1]);
  if( pid ) 
	kill( pid, SIGKILL );
}

/*********************************************************/
/* setup server and communicate with fvwm and the client */
/*********************************************************/
void server (int *fd) {
  struct sockaddr_in sin, fsin;
  int  fromlen;     /* length of sockaddr */
  char buf[BUFSIZE];      /*  command line buffer */
  char *bp;
  int  port;
  int  rcnt;
  
  /* make a socket  */
  if( (s = socket(AF_INET, SOCK_STREAM, 0 )) < 0  ) {
	ErrMsg( "socket");
	exit(1);
  }
  port = PORT_CMD; 

  memset((char *) &sin, 0, sizeof(sin));   
  sin.sin_family = AF_INET;
  sin.sin_port = htons(port);
  sin.sin_addr.s_addr = htonl(INADDR_ANY);  

  if( bind(s, (struct sockaddr *)&sin, sizeof(sin)) < 0 ) {
	ErrMsg( "bind" );
  }
  
  /* listen to the socket */
  if ( listen(s,5) < 0 ) {
    ErrMsg( "listen" );
  }

  fromlen = sizeof(fsin);
  while(1) {
	/* accept connections */
  
	if(( ns = accept(s, (struct sockaddr *)&fsin, &fromlen)) < 0 ) {
	  ErrMsg( "accept");
	}

	/* get command from client and return result */
	while((rcnt = recv( ns, buf, BUFSIZE, 0)) ) { 
	  
	  /* check if client is terminated */
	  if( buf == NULL ) {
		break; 
	  }
	  for( bp=buf; bp<buf+rcnt; bp+=strlen(bp)+1) {
		SendText(fd,bp,0); /* send command */
	  }
	}
	close(ns);
	/* fprintf (stderr,  "closing %d...\n",ns); */
  } 
  CloseSocket();
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


