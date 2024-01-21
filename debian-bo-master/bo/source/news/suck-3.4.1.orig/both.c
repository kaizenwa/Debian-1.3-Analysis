#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#include <sys/types.h>
#endif

#include <netdb.h>
#include <ctype.h>
#include <string.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <errno.h>
#include <stdarg.h>

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#include "suck_config.h"
#include "both.h"
#include "phrases.h"

#ifdef TIMEOUT
# if TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  if HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
# endif
#endif

#ifdef MYSIGNAL
#include <signal.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>		/* for aix */
#endif


/* internal function proto */
void vprint_phrases(FILE *, const char *, va_list);
void convert_nl(char *);

/*-----------------------------------------------------*/
/* get next number in string */
char *number(char *sp, int *intPtr) {
	int start, end;
	char c;
	char *retval;

	if(sp==NULL) {
		*intPtr=0; 
		retval = sp;
	}
	else {
		/* skip any leading spaces */
		start = 0;
		while(sp[start] == ' ') {
			start++;
		}
		end = start;
		while(isdigit(sp[end])) {
			end++;
		}
		/* now we have the numbers width*/

		c=sp[end];	/* save off the character */
		sp[end]='\0';	/* truncate nr so sscanf works right */
		sscanf(&sp[start],"%d",intPtr);
		sp[end]=c;	/* restore it back */

		/* if at EOS return the NULL, else skip space */
		retval = (sp[end] == '\0') ? sp+end : sp+(++end);
	}
	return retval;
}
/*---------------------------------------------*/
struct hostent *get_hostent(const char *host) {
	struct in_addr saddr;
	int c;
	struct hostent *hi = NULL;

	if(host==NULL) { 
		error_log(ERRLOG_REPORT,both_phrases[0], NULL); 
	}
	else {
		c=*host;
		if(isdigit(c)) {
 			saddr.s_addr = inet_addr(host); 
 			hi = gethostbyaddr((char *)&saddr,sizeof(struct in_addr),AF_INET);
		}
		else {
			hi = gethostbyname(host);
		}
	}
	return hi;
}
/*--------------------------------------------*/
int connect_to_nntphost(const char *host, struct hostent **hi, FILE *msgs, unsigned short int portnr) {
	char *ptr;
	struct in_addr *aptr;
	struct in_addr saddr;
	struct sockaddr_in address;
	char sport[10];
	int sockfd = -1;

	sprintf(sport, "%hu", portnr);	/* cause print_phrases wants all strings */
	print_phrases(msgs, both_phrases[1], sport, NULL);

	/* Find the internet address of the NNTP server */
 	*hi = get_hostent(host);
 	if(*hi == NULL) {
		error_log(ERRLOG_REPORT,"%v1%: ",host, NULL);
  		MyPerror(both_phrases[2]);
 	}
	else {
		print_phrases(msgs, both_phrases[3], (*hi)->h_name, NULL);
 		while((ptr = *((*hi)->h_aliases)) != NULL) {
			print_phrases(msgs, both_phrases[4], ptr, NULL );
			(*hi)->h_aliases++;
		}
 		if((*hi)->h_addrtype != AF_INET) {
  			error_log(ERRLOG_REPORT, both_phrases[5], NULL);
		}
		else {
			while((aptr = (struct in_addr *)*((*hi)->h_addr_list)++) != NULL) {
				saddr = *aptr;
				print_phrases(msgs, both_phrases[17], inet_ntoa(*aptr), NULL);
			}

			/* Create a socket */
 			if((sockfd = socket( AF_INET, SOCK_STREAM, SOCKET_PROTOCOL)) == -1) {
				MyPerror(both_phrases[6]);
			}
			else { 
				address.sin_family = AF_INET;
				address.sin_port = htons(portnr);  /* NNTP port */
				address.sin_addr= saddr;

				/* Establish a connection */
				if(connect(sockfd, (struct sockaddr *)&address, sizeof address ) == -1) {
					MyPerror(both_phrases[7]);
					close(sockfd);
					sockfd = -1;
				}
				else {
					print_phrases(msgs,both_phrases[8], (*hi)->h_name, NULL);
				}
			}
		}
	}
	return sockfd;
}
/*----------------------------------------------------------------*/
int sputline(int fd, const char *outbuf) {
	
#ifdef DEBUG1
	do_debug("\nSENT: %s", outbuf);
#endif
	return send(fd, outbuf, strlen(outbuf), 0);
}
/*-------------------------------------------------------------*/
#ifdef DEBUG
void do_debug(const char *fmt, ...) {

	FILE *fptr = NULL;
	va_list args;

	if((fptr = fopen(N_DEBUG, "a")) == NULL) {
		fptr = stderr;
	}
		
	va_start(args, fmt);
	vfprintf(fptr, fmt, args);
	va_end(args);

	if(fptr != stderr) {
		fclose(fptr);
	}
}
/*------------------------------------------------------------*/
#endif
/*------------------------------------------------------------*/
void MyPerror(const char *message) {

#ifdef DEBUG	
	do_debug("%d: %s\n", errno, message);
#endif
	/* can't just use perror, since it goes to stderr */
	/* and I need to route it to my errlog */
	/* so I have to recreate perror's format */
#ifdef HAVE_STRERROR
	error_log(ERRLOG_REPORT, "%v1%: %v2%\n", message, strerror(errno), NULL);
#else
	error_log(ERRLOG_REPORT, both_phrases[9], message, errno, NULL);
#endif
}	
/*-----------------------------------------------------------*/
int sgetline(int fd, char **inbuf) {

	static char buf[MAXLINLEN+MAXLINLEN+6];
	static char *start = buf;
	static char *eob = buf;		/* end of buffer */
	int ret, i, len;
	char *ptr;

#ifdef TIMEOUT
	fd_set myset;
	struct timeval mytimes;
#endif
	ret = 0;
	ptr = NULL;

	if(eob == start || (ptr = strstr(start,"\r\n")) == NULL) {
		/* TEST for not a full line in buffer */	
		/* the eob == start test is needed in case the buffer is */
		/* empty, since we don't know what is in it. */

		i = eob-start; 	/* length of partial line in buf */
		if((eob - buf) > MAXLINLEN) {
#ifdef DEBUG1
			do_debug("SHIFTING BUFFER\n");
#endif
			/* not enuf room in buffer for a full recv */
			memmove(buf, start, i);		/* move to start of buf */
			eob = buf + i;
			*eob = '\0';
			start = buf;			/* reset pointers */
		}
		len = i;
		/* try to get a line in, up to maxlen */
		do {
#ifdef TIMEOUT
			/* handle timeout value */
			FD_ZERO(&myset);
			FD_SET(fd, &myset);
			mytimes.tv_sec = TIMEOUT;
			mytimes.tv_usec = 0;
	
#ifdef MYSIGNAL
			signal_block(MYSIGNAL_BLOCK);
			/* block so we can't get interrupted by our signal defined in config.h */
#endif

			if((i = select(FD_SETSIZE, &myset, (fd_set *) NULL, (fd_set *) NULL, &mytimes)) == 1) {
				i = recv(fd, eob, MAXLINLEN-len, 0);	/* get line */
			}
			else if(i == 0) {
				error_log(ERRLOG_REPORT, both_phrases[10], NULL);
			}      /* other errors will be handled down below */
#else 
			i = recv(fd, eob, MAXLINLEN-len, 0);	/* get line */
#endif
#ifdef DEBUG1
			do_debug("\nSelect/Recv returned %d", i);
#endif
#ifdef MYSIGNAL
			signal_block(MYSIGNAL_UNBLOCK);
			/* we are done, now unblock it */
#endif

			if(i < 1) {
				if(i == 0) {
					/* No data read in either from recv or select timed out*/
					error_log(ERRLOG_REPORT, both_phrases[11], NULL);
				}
				else {
					MyPerror(both_phrases[12]);
				}
				ret = -1;
			}
			else {
				eob += i;	/* increment buffer end */
				*eob = '\0';	/* NULL terminate it  */
#ifdef DEBUG1
				do_debug("\nGOT: %.*s",i,eob-i);
#endif 
				len += i;				
				ptr = strstr(start, "\r\n");		
			}
		} while(ptr == NULL && len < MAXLINLEN && ret == 0);
	}
	if(ptr != NULL) {
		/* we have a full line left in buffer */
		*ptr++ = '\n';	/* change \r\n to just \n */
		*ptr++ = '\0';	/* null terminate */
		*inbuf = start;
		ret = (ptr-1) - start;	/* length of string */
		start = ptr; 	/* skip \r\n */
		if(eob == start) {
			/* nothing left in buffer, reset pointers to start of buffer */
			/* to give us a full buffer to receive next data into */
			/* hopefully doing this will mean less buffer shifting */
			/* meaning faster receives */
			eob = start = buf;
#ifdef DEBUG1
			do_debug("Empty buffer, resetting to start\n");
#endif
		}
	}
	else if(ret == 0) {
		/* partial line in buffer */
		/* null terminate */
		*eob = '\0';
		*inbuf = start;
		ret = (eob-1) - start;	/* length of string */
		start = ++eob;	/* point both past end */
	}
#ifdef DEBUG1
	if(ret > 0) {
		int flag = FALSE;
		char saveit = '\0';
		/* change nl to something we can see */
		if((*inbuf)[ret-1] == '\n') {
			flag = TRUE;
			(*inbuf)[ret-1] = '\\';
			(*inbuf)[ret] = 'n';
			saveit = (*inbuf)[ret+1];
			(*inbuf)[ret+1] = '\0';
		}
		do_debug("\nRETURNING len %d: %s\n", ret, *inbuf);
		/* put things back the way they were */
		if(flag == TRUE) {
			(*inbuf)[ret-1] = '\n';
			(*inbuf)[ret] = '\0';
			(*inbuf)[ret+1] = saveit;
		}
	}
	else {
		do_debug("\nRETURNING error, ret = %d", ret);
	}
#endif  
	return ret;
}		
/*----------------------------------------------------*/
#ifdef MYSIGNAL
void signal_block(int action) {

#ifdef POSIX_SIGNALS
	static sigset_t blockers;
	static do_block = FALSE;

	switch(action) {
		case MYSIGNAL_SETUP:	/* This must be called first */
			sigemptyset(&blockers);
			if(sigaddset(&blockers, MYSIGNAL) == -1 || sigaddset(&blockers,PAUSESIGNAL) == -1) {
				do_block = FALSE;
				error_log(ERRLOG_REPORT, both_phrases[13], str_int(MYSIGNAL), NULL);
			}
			else {
				do_block = TRUE;
			}
			break;
		case MYSIGNAL_ADDPIPE:	/* add SIGPIPE for killprg.c */
			if(sigaddset(&blockers, SIGPIPE) == -1) {
				error_log(ERRLOG_REPORT, both_phrases[14], NULL);
			}
			break;
		case MYSIGNAL_BLOCK:
			if(do_block == TRUE) {
				sigprocmask(SIG_BLOCK, &blockers, NULL);
			}
			break;
		case MYSIGNAL_UNBLOCK:
			if(do_block == TRUE) {
				sigprocmask(SIG_UNBLOCK, &blockers, NULL);
			}
			break;
	}
#endif /* POSIX_SIGNALS */
}
#endif /* MYSIGNAL */
/*-------------------------------------------------------------------*/
void error_log(int mode, const char *fmt, ...) {

	/* if we have been passed a file, report all errors to that file */
	/* else report all errors to stderr */
	/* handle printf type formats, hence the varargs stuff */

	FILE *fptr = NULL;
	va_list args;
	static char errfile[PATH_MAX] = { '\0' };

	va_start(args, fmt);	/* set up args */
	
	switch(mode) {
	  case ERRLOG_SET_FILE:
		strcpy(errfile,fmt);
		break;
	  case ERRLOG_SET_STDERR:
		errfile[0] = '\0';
		break;
	  case ERRLOG_REPORT:
		if(errfile[0] == '\0' || (fptr = fopen(errfile, "a")) == NULL) {
			fptr = stderr;
		}
		vprint_phrases(fptr, fmt, args);
		if(fptr != stderr) {
			fclose(fptr);
		}
		break;
	}
	va_end(args);		/* so we can return normally */
	return;
}
/*--------------------------------------------------------------------------*/
char **build_args(const char *fname, int *nrargs) {
	/* read a file and parse the args into an argv array */
	/* make two passes thru the file, 1st count them so can allocate array */
	/* then allocate each one individually */

	char **args = NULL;
	char linein[MAXLINLEN+1];
	int x, len, done, counter, argc = 0;
	FILE *fpi;

	if((fpi = fopen(fname, "r")) == NULL) {
		MyPerror(fname);
	}
	else {
		/* first pass, just count em */
		counter = 0;
		while(fgets(linein, MAXLINLEN, fpi) != NULL) {
			x = 0;
			done = FALSE;
			while(linein[x] != '\0' && done == FALSE) {
				while(isspace(linein[x])) {
					x++;		/* skip white space */
				}
				if(linein[x] == FILE_ARG_COMMENT || linein[x] == '\0') {
					done = TRUE;	/* skip rest of line */
				}
				else {
					counter++;	/* another arg */
					while(!isspace(linein[x]) && linein[x] != '\0') {
						x++;	/* skip rest of arg */
					}
				}
			}
		}
#ifdef DEBUG1
		do_debug("Counted %d args in %s\n", counter, fname);
#endif  
		if((args = calloc( counter, sizeof(char *))) == NULL) {
			error_log(ERRLOG_REPORT, both_phrases[15], fname, NULL);
		}
		else {
			/* pass two  read em and alloc em*/
			fseek(fpi, 0L, SEEK_SET);	/* rewind the file for pass two */
			counter = 0;	/* start at 0 again */
			while(fgets(linein, MAXLINLEN, fpi) != NULL) {
				x = 0;
				done = FALSE;
				while(linein[x] != '\0' && done == FALSE) {
					while(isspace(linein[x])) {
						x++;		/* skip white space */
					}
					if(linein[x] == FILE_ARG_COMMENT || linein[x] == '\0') {
						done = TRUE;	/* skip rest of line */
					}
					else {
						/* have another arg */
						len = 1;
						while(!isspace(linein[x+len]) && linein[x+len] != '\0') {
							len++;	/* find length of arg */
						}
						if((args[counter] = calloc( len+1, sizeof(char))) == NULL) {
							error_log(ERRLOG_REPORT, both_phrases[16], NULL);
						}
						else {
							strncpy(args[counter], &linein[x], len);
							args[counter][len] = '\0';	/* ensure null termination */
#ifdef DEBUG1
							do_debug("Read arg #%d: '%s'\n", counter, args[counter]);
#endif
							counter++;
						}
						x += len;	/* go onto next one */
					}
				}
			}
			argc = counter;		/* total nr of args read in and alloced */
		}
		fclose(fpi);
	}

	*nrargs = argc;
	return args;
}
/*-----------------------------------------------------------------------------*/
/* free the memory allocated in build_args() */
void free_args(int argc, char *argv[]) {

	int i;
	for(i=0;i<argc;i++) {
		free(argv[i]);
	}
	free(argv);
}
/*--------------------------------------------------------------------------------*/
/* These are used by the load_phrases() stuff					  */
/* read_array() and do_a_phrase() don't use the stored phrases                    */
/* since they are used when the phrases are being loaded. 			  */
/*--------------------------------------------------------------------------------*/
char **read_array(FILE *fpi, int nr, int save_yn) {

	int i, retval = 0;
	char **array = NULL;
	char linein[MAXLINLEN+1];

	if(save_yn == TRUE && (array = calloc( nr, sizeof(char *))) == NULL) {
		error_log(ERRLOG_REPORT, "Out of memory reading phrases\n", NULL);
	}
	else {
		for(i=0;retval == 0 && i < nr; i++) {
			if(fgets(linein, MAXLINLEN, fpi) != NULL) {
				if(save_yn == TRUE) {
					if((array[i] = do_a_phrase(linein)) == NULL) {
						retval = -1;
					}
				}
			}
		}
	}

	if(retval == -1) {
		free_array(nr, array);
		array = NULL;
	}
	return array;
}
/*-------------------------------------------------------------------------------*/
void free_array(int n, char **arr) {
	int i;
	if(arr != NULL) {
		for(i=0;i<n;i++) {
			if(arr[i] != NULL) {
				free(arr[i]);
			}
		}
		free(arr);
	}	
}
/*--------------------------------------------------------------------------------*/
char *do_a_phrase(char linein[]) {

	char *lineout;
	int i, stt, end;
	static char default_phrase[] = "";

	
	i = strlen(linein);
	lineout = NULL;
	
	/* find start and finish of string */
	for(stt = 0; stt != i && linein[stt] != '"'; stt++) {
	}
	for(end = i-1;  i!= 0 && linein[end] != '"'; end--) {
	}
	if(end <= stt) {
		error_log(ERRLOG_REPORT, "Invalid line, %v1%, Ignoring\n", linein, NULL);
		lineout = default_phrase;
	}
	else {
		i = end - stt;
		if((lineout = calloc(i, sizeof(char))) == NULL) {
			error_log(ERRLOG_REPORT, "Out of Memory Reading Phrases\n", NULL);
		}
		else {
			strncpy(lineout, &linein[stt+1], i-1);	/* put the string into place */
			lineout[i-1] = '\0';			/* null terminate it */
#ifdef DEBUG1
			do_debug("Got phrase:%s:\n", lineout);
#endif
			convert_nl(lineout);
		}
	}
	return lineout;
}
/*-----------------------------------------------------------------------------------*/
void convert_nl(char *linein) {
	/* go thru a string, and convert \r \t \n (2 chars) to actual control codes */
	char c;
	int i, x, len;

	if(linein != NULL) {
		len = strlen(linein);
		for(i = 0; i < len ; i++ ) {
			if(linein[i] == '\\') {
				c = linein[i+1];
				if(c == 'r' || c == 'n' || c == 't') {
					switch(c) {
					  case 'r':
						c = '\r';
						break;
					  case 'n':
						c = '\n';
						break;
					  case 't':
						c = '\t';
						break;
					}
					linein[i] = c;
					for ( x = i + 1; x < len ; x++ ) {
						linein[x] = linein[x+1];
					}
					len--;		
				}
			}
		}
	}				
}
/*-----------------------------------------------------------------------------------*/
void print_phrases(FILE *fpout, const char *argstr, ... ){

	va_list vargs;
	
	va_start(vargs, argstr);

	vprint_phrases(fpout, argstr, vargs);

	va_end(vargs);
}

/*-----------------------------------------------------------------------------------*/
void vprint_phrases(FILE *fpout, const char *argstr, va_list vargs) {
	/* this routine takes in argstr, parses it for args and prints everything */
	/* out, the args may not be in order ("hi %v2% there %v1% guys")	  */  
	/* args are %v#%.  The varargs are all char ptrs */

	const char *ptr, *pnumber;
	char *tptr, block[PHRASES_BLOCK_SIZE+1], *in[PHRASES_MAX_NR_VARS];
	int len, varyn, vnr, nrvars;

	/* create an array with our string pointers */
	nrvars = 0;
	tptr = va_arg(vargs, char *);
	while(tptr != NULL && nrvars < PHRASES_MAX_NR_VARS) {
		in[nrvars++] = tptr;
		tptr = va_arg(vargs, char *);
	}		

	varyn = FALSE;
	ptr = argstr;
	if(ptr != NULL) {
		while( *ptr != '\0' ) {
			len = 0;
			while(len < PHRASES_BLOCK_SIZE && *ptr != '\0') {
				if(*ptr == PHRASES_SEPARATOR && *(ptr+1) == PHRASES_VAR_CHAR) {
					pnumber = ptr+2;
					while(isdigit(*pnumber)) {
						pnumber++;
					}
					if(pnumber != ptr+2 && *pnumber == PHRASES_SEPARATOR) {
						/* bingo, we match syntax %v1% handle the variables */
						sscanf(ptr+2, "%d", &vnr);	/* get the number */
						/* now print it */
						if(vnr <= nrvars) {
							varyn = TRUE;
							/* first, print what's currently in the buffer */
							block[len] = '\0';
							fputs(block,fpout);
							len = 0;
							/* now print the variable */
							/* -1 cause array starts at 0 vars at 1 */
							fputs(in[vnr-1], fpout);
							ptr = pnumber+1;	/* advance past var */
						}
							
					}
				}
				if(varyn == FALSE) {
					block[len++] = *ptr++;
				}
				else {
					varyn = FALSE;		/* reset it */
				}
			}
			if(len > 0 ) {
				block[len] = '\0';	/* null terminate string */
				fputs(block, fpout);
			}
		}
	}
}
/*----------------------------------------------------------*/
char *str_int(int nrin) {
	
	static char strings[PHRASES_MAX_NR_VARS][12];	/* lets pray ints don't get bigger than this */
	static int which = 0;
	
	/* use a set of rotating buffers so can make multiple str_int calls */
	if(++which == PHRASES_MAX_NR_VARS) {
		which = 0;
	}
	
	sprintf(strings[which], "%d", nrin);

	return strings[which];
}
