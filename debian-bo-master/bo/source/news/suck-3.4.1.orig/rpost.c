#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include "suck_config.h"
#include "both.h"
#include "phrases.h"

struct nntp_auth {
	char *userid;
	char *passwd;
};

typedef struct {
	char *batch;
	char *prefix;
	char *host;
	char **filter_args;
	int filter_argc;
	int deleteyn;
	int do_modereader;
	struct nntp_auth auth;
	FILE *status_fptr;
	unsigned short int portnr;
	char *phrases;
} Args, *Pargs;

/* function declarations */
int do_article(int, FILE *, FILE *, struct nntp_auth *);
int do_batch(int, Pargs);
int do_filter(int, char *[]);
int send_command(int, const char *, char **, int, struct nntp_auth *);
int scan_args(Pargs, int, char *[]);
void log_fail(char *, char *);
void load_phrases(Pargs);
void free_phrases(void);

/* stuff needed for language phrases */
	/* set up defaults */	
char **rpost_phrases = default_rpost_phrases;
char **both_phrases = default_both_phrases;

enum { RETVAL_ERROR = -1, RETVAL_OK, RETVAL_ARTICLE_PROB, RETVAL_NOAUTH, RETVAL_UNEXPECTEDANS};
/*------------------------------------------------*/
void main(int argc, char *argv[]) {
	char *inbuf;
	int sockfd, response, retval, loop, fargc;
	struct hostent *hi;
	char **args, **fargs;
	Args myargs;

	/* initialize everything */
	retval = RETVAL_OK;
	fargc = 0;
	fargs = NULL;

	myargs.batch = NULL;
	myargs.prefix =NULL;
	myargs.filter_argc = -1;
	myargs.filter_args = NULL;
	myargs.status_fptr = stdout;
	myargs.deleteyn = FALSE;
	myargs.auth.userid = NULL;
	myargs.auth.passwd = NULL;
	myargs.do_modereader = FALSE;
	myargs.portnr = DEFAULT_NNRP_PORT;
	myargs.host = getenv("NNTPSERVER");		/* the default */
	myargs.phrases = NULL;

	/* allow no args, only the hostname, or hostname as first arg */
	/* also have to do the file argument checking */
	switch(argc) {
	  case 1:
		break;
	  case 2:
		/* the fargs == NULL test so only process first file name */
		if(argv[1][0] == FILE_CHAR) {
			if((fargs = build_args(&argv[1][1], &fargc)) != NULL) {
				retval = scan_args(&myargs, fargc, fargs);
			}
		}   
		else {
			myargs.host = argv[1];
		}
		break;
	  default:
		for(loop=1;loop<argc && fargs == NULL;loop++) {
			if(argv[loop][0] == FILE_CHAR) {
				if((fargs = build_args(&argv[loop][1], &fargc)) != NULL) {
					retval = scan_args(&myargs, fargc, fargs);
				}
			}   
		}
		/* this is here so anything at command line overrides file */
		if(argv[1][0] != '-' && argv[1][0] != FILE_CHAR) {
			myargs.host = 	argv[1];
			argc-= 2;
			args = &(argv[2]);
		}
		else {
			args = &(argv[1]);
			argc--;
		}
		retval = scan_args(&myargs, argc, args);	
		break;
	}
	load_phrases(&myargs);	/* this has to be here so rest prints okay */

	if(retval == RETVAL_ERROR) {
		error_log(ERRLOG_REPORT, rpost_phrases[0], argv[0], NULL);
		error_log(ERRLOG_REPORT, rpost_phrases[1], NULL);
		error_log(ERRLOG_REPORT, rpost_phrases[2], NULL);
	}
	else {
#ifdef DEBUG3
	do_debug("Rpost version %d.%d.%d\n",SUCKVER, SUCKVER_MINOR, SUCKVER_PATCH);
	do_debug("myargs.batch = %s\n", myargs.batch);
	do_debug("myargs.prefix = %s\n", myargs.prefix);
	do_debug("myargs.filter_argc = %d\n", myargs.filter_argc);
	for(loop=0;loop<myargs.filter_argc;loop++) {
		do_debug("myargs.filter_args[%d] = %s\n", loop, myargs.filter_args[loop]);
	}
	do_debug("myargs.status_fptr = %s\n", (myargs.status_fptr == stdout) ? "stdout" : "not stdout");
	do_debug("myargs.deleteyn = %d\n", myargs.deleteyn);
	do_debug("myargs.auth.userid = %s\n", myargs.auth.userid);
	do_debug("myargs.auth.passwd = %s\n", myargs.auth.passwd);
	do_debug("myargs.do_modereader = %d\n", myargs.do_modereader);
	do_debug("myargs.portnr = %d\n", myargs.portnr);
	do_debug("myargs.host = %s\n", myargs.host);
	do_debug("myargs.phrases = %s\n", myargs.phrases);

#endif
		/* we processed args okay */
		sockfd = connect_to_nntphost( myargs.host, &hi, myargs.status_fptr, myargs.portnr);
		if(sockfd < 0) {
			retval = RETVAL_ERROR;
		}
		else {
			/* Get the announcement line */
			sgetline(sockfd, &inbuf);
			fputs(inbuf, myargs.status_fptr );
			if(myargs.do_modereader == TRUE) {
				retval = send_command(sockfd,"mode reader\r\n", &inbuf, 0, &(myargs.auth));
				if( retval  == RETVAL_OK) {				
					/* Again the announcement */
					fputs(inbuf, myargs.status_fptr );
					retval = RETVAL_OK;
				}
			}
		}
		if(retval == 0) {
			number(inbuf, &response);
			if(response != 200) {
				error_log(ERRLOG_REPORT, rpost_phrases[3], NULL);
				retval = RETVAL_ERROR;
			}
			else if(myargs.batch == NULL) {
				/* do one article from stdin */
				retval = do_article(sockfd, stdin, myargs.status_fptr,&(myargs.auth));
			}
			else {
				/* batch mode */
				retval = do_batch(sockfd, &myargs);
			}		
			print_phrases(myargs.status_fptr, rpost_phrases[4], hi->h_name, NULL);
#ifdef DEBUG3
			do_debug("Sending quit");
#endif
			send_command( sockfd, "quit\r\n", NULL, 0, &(myargs.auth));
 			close(sockfd);
		}
	}
	if(myargs.status_fptr != NULL && myargs.status_fptr != stdout) {
		fclose(myargs.status_fptr);
	}
	free_phrases();	/* do this last so everything is displayed correctly */
	exit(retval);
}
/*----------------------------------------------------------------------------------*/
int do_article(int sockfd, FILE *fptr, FILE *sptr, struct nntp_auth *authp) {

	int len, response, retval, longline;
	char buf[MAXLINLEN+4], *inbuf, *ptr;
	/* the +4 in NL, CR, if have to double . in first pos */
	retval = RETVAL_OK;
	longline = FALSE;

	/* Initiate post mode */
	/* don't use IHAVE since I'd have to scan the message for the msgid, */
	/* send it, wait for the response, rewind and reread the file */
	/* its just as quick to do a post and let the other end worry about it*/
	response = send_command(sockfd, "POST\r\n", &inbuf, 340, authp);
	fputs(inbuf, sptr);
	if(response!=RETVAL_OK) {
		error_log(ERRLOG_REPORT, rpost_phrases[5], NULL);
		retval = RETVAL_ERROR;
	}
	else {
		while(fgets(buf, MAXLINLEN, fptr) != NULL) {
			len=strlen(buf);
			if(longline == FALSE) {
				/* only check this if we are at the beginning of a line */
				if(buf[0]=='.')  {
				/* Yup, this has to be doubled */
					memmove(buf+1,buf,++len);
					buf[0]='.';
   				}
			}
			if(buf[len-1] == '\n') {
				/* only do this if we have an EOL in buffer */
	   			strncpy(&buf[len-1],"\r\n",3);
				longline = FALSE;
			}
			else {
				longline = TRUE;
			}
   			sputline(sockfd, buf);
#ifdef DEBUG3
			do_debug("ARTICLE: %s", buf);
#endif
		}
		/* if the last line didn't have a nl on it, we need to */
		/* put one so that the eom is recognized */
		if(longline == TRUE) {
			sputline(sockfd, "\r\n");
		}
		sputline(sockfd, ".\r\n");
#ifdef DEBUG3
		do_debug("ARTICLE END\n");
#endif


		sgetline(sockfd, &inbuf);
		fputs(inbuf, sptr);
#ifdef DEBUG3
		do_debug("RESPONSE: %s", inbuf);
#endif

		ptr = number(inbuf, &response);
	 	if(response!=240) {
			if(response == 441) {
				number(ptr, &response);
			}
			if(response == 435) { 
				print_phrases(sptr, rpost_phrases[6], NULL);
			}
			else {
				error_log(ERRLOG_REPORT, rpost_phrases[7], NULL);
				retval = RETVAL_ARTICLE_PROB;
			}
		}
	}
	return retval;
}
/*--------------------------------------------------------------------------------*/
int do_batch(int sockfd, Pargs myargs) {

	int i, x, loop, argon, infilenr, nrdone, nrok, retval = 0;
	FILE *fpi_batch, *fpi_msg;
	char buf[MAXLINLEN+1], file[MAXLINLEN+1];
	char *infile, *outfile, *ourargs[RPOST_MAXARGS];

	outfile = NULL;
	retval = RETVAL_OK;
	argon = infilenr = -1;
	nrdone = nrok = 0;

	if((fpi_batch = fopen(myargs->batch, "r")) == NULL) {
		MyPerror(myargs->batch);
		retval = RETVAL_ERROR;
	}
	else {
		if(myargs->filter_argc > 0) {
			/* build filter args */
 
			i = strlen(RPOST_FILTER_OUT);

			/* build array of args to past to fork, execl */
			/* if see RPOST_FILTER_IN as arg, substitute the infile name */
			/* if see RPOST_FILTER_OUT as first part of arg, get outfile name */
			for(argon = 0, loop = 0; loop < myargs->filter_argc; loop++) {
				if(strcmp(myargs->filter_args[loop], RPOST_FILTER_IN) == 0) {
					/* substitute infile name */
					infilenr = argon++;
				}
				else if(strncmp(myargs->filter_args[loop], RPOST_FILTER_OUT, (unsigned int) i) == 0) {
					/* arg should be RPOST_FILTER_OUT=filename */
					if(myargs->filter_args[loop][i] != '=') {
						error_log(ERRLOG_REPORT, rpost_phrases[17], myargs->filter_args[loop], NULL);
					}
					else {
						outfile = (char *) &(myargs->filter_args[loop][i+1]);
					}
				}
				else {
					ourargs[argon++] = myargs->filter_args[loop];
				}			
			}
			ourargs[argon] = NULL;	/* just to be on the safe side */
			if(outfile == NULL) {
				/* no outfile defined, use built-in default */
				outfile = tmpnam(NULL);
				error_log(ERRLOG_REPORT, rpost_phrases[9], outfile, NULL);
			}
			if(infilenr < 0) {
				error_log(ERRLOG_REPORT, rpost_phrases[10], NULL);
				retval = RETVAL_ERROR;
			}
		}
		while((retval != RETVAL_ERROR) && (fgets(buf, MAXLINLEN, fpi_batch) != NULL)) {
			/* build file name */
			if(myargs->prefix == NULL) {
				strcpy(file, buf);
			}
			else {
				strcpy(file, myargs->prefix);
				if(file[strlen(file)-1] != '/') {
					strcat(file, "/");
				}
				strcat(file, buf);
			}
			/* strip off nl */
			i = strlen(file);
			if(file[i-1] == '\n') {
				file[i-1] = '\0';
			}
			/* some INNs put article number on line in addition to file name */
			/* so lets parse through string, if find a ' ' NULL terminate at */
			/* that point */
			for(x=0;x<i;x++) {
				if(file[x] == ' ') {
					file[x] = '\0';
					x = i;	/* to break loop */
				}
			}
			/* okay here have file name */
#ifdef DEBUG3
			do_debug("Article Name: %s\n", file);
#endif
			/* if we are using a filter, use outfilename */
			/* else just use regular file name */
			if(myargs->filter_argc > 0) {
				ourargs[infilenr] =  file;
				retval = do_filter(argon, ourargs);
				infile = outfile;
			}
			else {
				infile = file;
			}

			if(infile == NULL) {
				error_log(ERRLOG_REPORT, rpost_phrases[11], NULL);
				retval = RETVAL_ERROR;
			}
			else if((fpi_msg = fopen(infile, "r")) == NULL) {
				/* skip to next article if infile don't exist */
				error_log(ERRLOG_REPORT, rpost_phrases[12],NULL);
			}
			else {
				retval = do_article(sockfd, fpi_msg, myargs->status_fptr, &(myargs->auth));
				fclose(fpi_msg);
				nrdone++;
				if(retval == RETVAL_OK) {
					nrok++;
				}
				/* log failed uploads (dupe article is not a failed upload ) */
				else if (retval == RETVAL_ARTICLE_PROB) {
					log_fail(myargs->batch, buf);
				}
			}	
		}
	 	fclose(fpi_batch);

		if(retval != RETVAL_ERROR) {
			retval = (nrok == nrdone) ? RETVAL_OK : RETVAL_ARTICLE_PROB;
		}		
		if(myargs->deleteyn == TRUE && retval == RETVAL_OK) {
			unlink(myargs->batch);
			print_phrases(myargs->status_fptr, rpost_phrases[13], myargs->batch,NULL);
		}
	}
	return retval;
}
/*--------------------------------------------------------------------*/
int do_filter(int argc, char *argv[]) {
	int retval = RETVAL_OK;

#ifdef DEBUG3
	{
		int xxxi;
		do_debug("ARGS:");
		for(xxxi=0;xxxi<argc;xxxi++) {
			do_debug(" %d=%s", xxxi, argv[xxxi]);
		}
		do_debug("\n");
	}
#endif
	switch((int) fork()) {
	  case 0:	/* in child, do execl */
		if(execvp(argv[0], argv) == -1) {
			MyPerror(rpost_phrases[14]);
			retval = RETVAL_ERROR;
		}
		break;
	  case -1:	/* should never get here */
		MyPerror(rpost_phrases[15]);
		retval = RETVAL_ERROR;
		break;
	  default:	/* in parent, wait for child to finish */
		wait(NULL);		/* no status check on finish */
	}					

	return retval;
}
/*------------------------------------------------------------------------------*/
int send_command(int sockfd, const char *cmd, char **ret_response, int good_response, struct nntp_auth *authp) {
	/* this is needed so can do user authorization */

	int len, retval = RETVAL_OK, nr;
	char buf[256], *resp;

#ifdef DEBUG3
	do_debug("sending command: %s", cmd);
#endif
	sputline(sockfd, cmd);
	len = sgetline(sockfd, &resp);	
	if( len < 0) {	
		retval = RETVAL_ERROR;		  	
	}					
	else {
#ifdef DEBUG3
		do_debug("got answer: %s", resp);
#endif
		number(resp, &nr);
		if(nr == 480 ) { /* we must do authorization */
			sprintf(buf, "AUTHINFO USER %s\r\n", authp->userid);
#ifdef DEBUG3
			do_debug("sending command: %s", buf);
#endif
			sputline(sockfd, buf);
			len = sgetline(sockfd, &resp);
			if( len < 0) {	
				retval = RETVAL_ERROR;		  	
			}					
			else {
#ifdef DEBUG3
				do_debug("got answer: %s", resp);
#endif
				number(resp, &nr);
				if(nr != 381) {
					error_log(ERRLOG_REPORT, rpost_phrases[16], resp,NULL);
					retval = RETVAL_NOAUTH;
				}
				else {
					sprintf(buf, "AUTHINFO PASS %s\r\n", authp->passwd);
					sputline(sockfd, buf);
#ifdef DEBUG3
					do_debug("sending command: %s", buf);
#endif
					len = sgetline(sockfd, &resp);
					if(len < 0) {	
						retval = RETVAL_ERROR;		  	
					}					
					else {
#ifdef DEBUG3
						do_debug("got answer: %s", resp);
#endif
						number(resp, &nr);
						switch(nr) {
						  case 281: /* bingo resend original command*/
							sputline(sockfd, cmd);
#ifdef DEBUG3
							do_debug("sending command: %s", cmd);
#endif
							len = sgetline(sockfd, &resp);
							if( len < 0) {	
								retval = RETVAL_ERROR;		  	
							}
							else {
								number(resp,&nr);
#ifdef DEBUG3
								do_debug("got answer: %s", resp);
#endif
							}
							break;
						  case 502: /* permission denied */
							retval = RETVAL_NOAUTH;
							error_log(ERRLOG_REPORT, rpost_phrases[17], NULL);
							break;
						  default: /* wacko error */
							error_log(ERRLOG_REPORT, rpost_phrases[16], resp, NULL);
							retval = RETVAL_NOAUTH;
							break;
						}
					}
				}
			}
		}
		if (good_response != 0 && nr != good_response) {
			error_log(ERRLOG_REPORT,rpost_phrases[18], cmd,resp, NULL);
			retval = RETVAL_UNEXPECTEDANS;
		}
	}
	if(ret_response != NULL) {
		*ret_response = resp;
	}
	return retval;
}
/*--------------------------------------------------------------------------------------*/
int scan_args(Pargs myargs, int argc, char *argv[]) {

	int loop, retval = RETVAL_OK;
	
	for(loop=0;loop<argc && retval == RETVAL_OK;loop++) {
#ifdef DEBUG3
		do_debug("Checking arg #%d-%d: '%s'\n", loop, argc, argv[loop]);
#endif
		/* check for valid args format */
		if(argv[loop][0] != '-' && argv[loop][0] != FILE_CHAR) {
			retval = RETVAL_ERROR;
		}
		if(argv[loop][0] == '-' && argv[loop][2] != '\0') {
			retval = RETVAL_ERROR;
		}

		if(retval != RETVAL_OK) {
			error_log(ERRLOG_REPORT, rpost_phrases[19], argv[loop], NULL);
		}
		else if(argv[loop][0] != FILE_CHAR) {
			switch(argv[loop][1]) {
			  case 'h': 	/* hostname */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, rpost_phrases[20], NULL);
					retval = RETVAL_ERROR;
				}
				else { 
					myargs->host = argv[++loop];
				}
				break;
				
			  case 'b':	/* batch file Mode, next arg = batch file */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, rpost_phrases[21], NULL);
					retval = RETVAL_ERROR;
				}
				else { 
					myargs->batch = argv[++loop];
				}
				break;
			  case 'p':	/* specify directory prefix for files in batch file */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, rpost_phrases[22], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					myargs->prefix = argv[++loop];
				}
				break;
			  case 'f':	/* filter prg, rest of cmd line is args */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, rpost_phrases[23], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					myargs->filter_argc = argc - (loop+1);
					myargs->filter_args = &(argv[loop+1]);
					/* point filter to first arg */
					loop = argc;		/* terminate main loop */
				}
				break;
			  case 'e':	/* use default error log path */
				error_log(ERRLOG_SET_FILE, ERROR_LOG,NULL);
				break;
			  case 'E':	/* error log path */
				if(loop+1 == argc) { 
					error_log(ERRLOG_REPORT, "%s\n",rpost_phrases[24],NULL);
					retval = RETVAL_ERROR;
				}
				else {
					error_log(ERRLOG_SET_FILE, argv[++loop]);
				}
				break;
		  	  case 's':	/* use default status log path */
				if((myargs->status_fptr = fopen(STATUS_LOG, "a")) == NULL) {
					MyPerror(rpost_phrases[25]);
					retval = RETVAL_ERROR;
				}
				break;
			  case 'S':	/* status log path */
				if(loop+1 == argc) { 
					error_log(ERRLOG_REPORT, rpost_phrases[26],NULL);
					retval = RETVAL_ERROR;
				}	
				else if((myargs->status_fptr = fopen(argv[++loop], "a")) == NULL) {
					MyPerror(rpost_phrases[25]);
					retval = RETVAL_ERROR;
				}
				break;
			  case 'd':	/* delete batch file on success */
				myargs->deleteyn = TRUE;
				break;
			  case 'U': 	/* next arg is userid for authorization */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, rpost_phrases[27],NULL);
					retval = RETVAL_ERROR;
				}
				else { 
					myargs->auth.userid = argv[++loop];
				}
				break;
			  case 'P': 	/* next arg is password for authorization */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, rpost_phrases[28],NULL);
					retval = RETVAL_ERROR;
				}
				else { 
					myargs->auth.passwd = argv[++loop];
				}
				break;
			  case 'M':	/* do mode reader command */
				myargs->do_modereader = TRUE;
				break;
			  case 'N':	/* override port number */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, rpost_phrases[29],NULL);
					retval = RETVAL_ERROR;
				}
				else {
					myargs->portnr = atoi(argv[++loop]);
				}
				break;
			  case 'l': 	/* language  phrase file */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, rpost_phrases[31],NULL);
					retval = RETVAL_ERROR;
				}
				else {
					myargs->phrases = argv[++loop];
				}
				break;
			  default:
				error_log(ERRLOG_REPORT, rpost_phrases[30], argv[loop],NULL);
				break;
			}
		}
	}
	return retval;
}
/*---------------------------------------------------------------*/
void log_fail(char *batch_file, char *article) {

	char file[MAXLINLEN+1];

	FILE *fptr;

	sprintf(file, "%s%s", batch_file, RPOST_FAIL_EXT);

	if((fptr = fopen(file, "a")) == NULL) {
		MyPerror(file);
	}
	else {
		fputs(article, fptr);
		fclose(fptr);
	}
}

/*--------------------------------------------------------------------------------*/
/* THE strings in this routine is the only one not in the arrays, since           */
/* we are in the middle of reading the arrays, and they may or may not be valid.  */
/*--------------------------------------------------------------------------------*/
void load_phrases(Pargs myargs) {

	int error=TRUE;
	FILE *fpi;

	if(myargs->phrases != NULL) {
		
		if((fpi = fopen(myargs->phrases, "r")) == NULL) {
			MyPerror(myargs->phrases);
		}
		else if((both_phrases = read_array(fpi, NR_BOTH_PHRASES, TRUE)) != NULL &&
		        (rpost_phrases = read_array(fpi, NR_RPOST_PHRASES, TRUE)) != NULL) {
			error = FALSE;
		}
		fclose(fpi);
		if(error == TRUE) {
			/* reset back to default */
			error_log(ERRLOG_REPORT, "Using default Language phrases\n",NULL);
			rpost_phrases = default_rpost_phrases;
			both_phrases = default_both_phrases;
		}
	}		
}
/*--------------------------------------------------------------------------------*/
void free_phrases(void) {
		/* free up the memory alloced in load_phrases() */
		if(rpost_phrases != default_rpost_phrases) {
			free_array(NR_RPOST_PHRASES, rpost_phrases);
		}
		if(both_phrases != default_both_phrases) {
			free_array(NR_BOTH_PHRASES, both_phrases);
		}
		
}
	
