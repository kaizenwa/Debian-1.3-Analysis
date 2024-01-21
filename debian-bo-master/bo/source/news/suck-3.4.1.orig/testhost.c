#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <netdb.h>
#ifdef HAVE_DIRENT_H
# include <dirent.h>
#else
# define dirent direct
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
#endif
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>
#include "both.h"
#include "suck_config.h"
#include "phrases.h"

/* set up defaults */
char **test_phrases=default_test_phrases;
char **both_phrases=default_both_phrases;

#define MAXCMDLEN 128	/* length of longest command to send + its args */
struct {
	const char *command;
	int response;
} cmds[]  = { { "help", 100 }, 
	      { "list", 215 },
	      { "newgroups", 231},
	      { "mode reader\r\nnewgroups", 231},
};

enum { CMD_HELP, CMD_LIST, CMD_NEWGRP };
enum { RETVAL_OK = 0, RETVAL_ERROR = -1, RETVAL_BAD_DATES = -2, RETVAL_NOAUTH = -3};

/*-functions----*/
int do_a_command(int, int, FILE *, char *, char *, char *);
int check_date_format(char *, char *, char *);
void load_phrases(char *);
void free_phrases(void);

/*------------------------------------------------------------------*/
void main(int argc, char *argv[]) {

	int sockfd, response, loop, cmd, retval = RETVAL_OK;
	struct hostent *hi;
	unsigned short int portnr;
	FILE *fptr = stdout;		/* used to print output to */

	char *host, *buf, *ptr, *cmdargs, *userid, *passwd, *phrases;
	char ndate[14];	/* 6 for yymmdd 6 for hhmmss and 1 for space and 1 for null */

	/* set up defaults */
	host = getenv("NNTPSERVER");
	cmd = CMD_HELP;
	loop = 1;
	cmdargs = NULL;
	portnr = DEFAULT_NNRP_PORT;
	passwd = NULL;
	userid = NULL;
	phrases = NULL;

	if(argc > 1 && argv[loop][0] != '-') {
		host = argv[loop++];
	}
	for(;loop<argc;loop++) {
		if(argv[loop][0] == '-') {
			switch(argv[loop][1]) {
			  case 'a': 
				cmd = CMD_LIST;
				break;
			  case 'n':
				cmd = CMD_NEWGRP;
				/* next two args must be date & time YYMMDD HHMMSS format */
				if(loop + 2 > argc) {
					error_log(ERRLOG_REPORT, test_phrases[0], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					if((retval = check_date_format(ndate, argv[loop+1], argv[loop+2])) == RETVAL_OK) {
						cmdargs = ndate;
					}
					else {
						error_log(ERRLOG_REPORT, test_phrases[1], argv[loop+1], argv[loop+2], NULL);
					}
					loop += 2;	/* past the args if there, if not this will finish up the arg loop */
				}
				break;
			  case 'e':	/* use default error log path */
				error_log(ERRLOG_SET_FILE, ERROR_LOG, NULL);
				break;
			  case 'E':	/* error log path */
				if(loop+1 == argc) { 
					error_log(ERRLOG_REPORT, test_phrases[2], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					error_log(ERRLOG_SET_FILE, argv[++loop], NULL);
				}
				break;
			  case 's':	/* use default status log path */
				if((fptr = fopen(STATUS_LOG, "a")) == NULL) {
					MyPerror(test_phrases[3]);
					retval = RETVAL_ERROR;
				}
				break;
			  case 'S':	/* status log path */
				if(loop+1 == argc) { 
					error_log(ERRLOG_REPORT, test_phrases[12], NULL);
					retval = RETVAL_ERROR;
				}
				else if((fptr = fopen(argv[++loop], "a")) == NULL) {
					MyPerror(test_phrases[3]);
					retval = RETVAL_ERROR;
				}
				break;
			  case 'N':	/* override port nr */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, test_phrases[4], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					portnr = atoi(argv[++loop]);
				}
				break;
			  case 'U': 	/* next arg is userid for authorization */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, test_phrases[5], NULL);
					retval = RETVAL_ERROR;
				}
				else { 
					userid = argv[++loop];
				}
				break;
			  case 'P': 	/* next arg is password for authorization */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, test_phrases[6], NULL);
					retval = RETVAL_ERROR;
				}
				else { 
					passwd = argv[++loop];
				}
				break;
			  case 'l': 	/* next arg is phrases file */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, test_phrases[11], NULL);
					retval = RETVAL_ERROR;
				}
				else { 
					phrases = argv[++loop];
				}
				break;
			  default:
				retval = RETVAL_ERROR;
				error_log(ERRLOG_REPORT, test_phrases[7], argv[loop], NULL);
			}
		}
		else {
			retval = RETVAL_ERROR;
			error_log(ERRLOG_REPORT, test_phrases[7], argv[loop], NULL);
		}
	}

	if(retval == RETVAL_OK) {
		load_phrases(phrases);	/* this is here so everything displays okay */

		sockfd = connect_to_nntphost( host, &hi, fptr, portnr);
		if(sockfd < 0 ) {
			retval = RETVAL_ERROR;
		}
		/* get the announcement line */
		else if(sgetline(sockfd, &buf) < 0) {
			retval = RETVAL_ERROR;
		}
		else {
			ptr = number(buf, &response);
			fprintf(fptr,"%s",ptr);	/* print it */

			if(response != 200 && response != 201) {
				/* not a valid response */
				retval = RETVAL_ERROR;
			}
			else {
				do_a_command(sockfd, cmd, fptr, cmdargs, userid, passwd);
			}
		}
		if(retval == RETVAL_ERROR) {
			error_log(ERRLOG_REPORT, test_phrases[8], host, NULL);
		}
	}
	if(fptr != stdout && fptr != NULL) {
		fclose(fptr);
	}
	free_phrases();	/* do this last so everything is displayed correctly */
	exit(retval);
}
/*-------------------------------------------------------------------------------*/
int do_a_command(int sockfd, int cmd, FILE *fptr, char *cmdargs, char *userid, char *passwd) {
	char *ptr, *buf, cmdstr[MAXCMDLEN], buf2[MAXCMDLEN];
	int response;
	int len, done, retval = RETVAL_OK;

	/* build command to send */
	strcpy(cmdstr, cmds[cmd].command);
	if(cmdargs != NULL) {
		strcat(cmdstr, " ");
		strcat(cmdstr, cmdargs);
	}
	strcat(cmdstr, "\r\n");
	
	sputline(sockfd, cmdstr);		/* which command do I run? */
	sgetline(sockfd, &buf);			/* get response */

	ptr = number(buf, &response);
	if(response == 480 ) { /* we must do authorization */
		sprintf(buf2, "AUTHINFO USER %s\r\n", userid);
		sputline(sockfd, buf2);
		len = sgetline(sockfd, &buf);
		if( len < 0) {	
			retval = RETVAL_ERROR;		  	
		}					
		else {
			ptr  = number(buf, &response);
			if(response != 381) {
				error_log(ERRLOG_REPORT, test_phrases[9], buf, NULL);
				retval = RETVAL_NOAUTH;
			}
			else {
				sprintf(buf2, "AUTHINFO PASS %s\r\n", passwd);
				sputline(sockfd, buf2);
				len = sgetline(sockfd, &buf);
				if(len < 0) {	
					retval = RETVAL_ERROR;		  	
				}					
				else {
					number(buf, &response);
					switch(response) {
					  case 281: /* bingo resend original command*/
						sputline(sockfd, cmdstr);
						len = sgetline(sockfd, &buf);
						if( len < 0) {	
							retval = RETVAL_ERROR;		  	
						}
						else {
							ptr = number(buf,&response);
						}
						break;
					  case 502: /* permission denied */
						retval = RETVAL_NOAUTH;
						error_log(ERRLOG_REPORT, test_phrases[10], NULL);
						break;
					  default: /* wacko error */
						error_log(ERRLOG_REPORT, test_phrases[9], ptr, NULL);
						retval = RETVAL_NOAUTH;
						break;
					}
				}
			}
		}
	}
	if(cmds[cmd].response != response) {
		error_log(ERRLOG_REPORT, "%v1%\n", buf, NULL);
		retval = RETVAL_ERROR;
	}
	else {
		/* okay here we go */
		fprintf(fptr, "%s", ptr); 
		/* commands are ended by a . on a line by itself */

		len = done = 0;
		while( len >=0 && done == 0) {
		
			len = sgetline(sockfd, &buf);

			if(len == 2 && strcmp(buf, ".\n") == 0) {
				done = 1;
			}
			else if(len > 0) {
				fprintf(fptr,"%s",buf);
			}
		}
		if(len < 0) {
			retval = RETVAL_ERROR;
		}
	}
	return retval;
}
/*---------------------------------------------------------------*/
int check_date_format(char *dates, char *indate, char *intime) {

	/* if indate & intime are not valid format, return error */
	/* when done, dates will be yymmdd hhmmss */
	
	int i, retval = RETVAL_OK;	
	
	/* now test my incoming args */
	if(indate == NULL || intime == NULL) {
		retval = RETVAL_ERROR;
	}
	else if(strlen(indate) != 6 || strlen(intime) != 6) {
		retval = RETVAL_ERROR;
	}
	else {
		for(i=0;i<6;i++) {
			if(!isdigit(indate[i]) || !isdigit(intime[i])) {
					retval = RETVAL_ERROR;
			}
		}
	}

	if(retval == RETVAL_OK) {
		sprintf(dates, "%s %s", indate, intime);
	}
	return retval;
}
/*--------------------------------------------------------------------------------*/
/* THE strings in this routine is the only one not in the arrays, since           */
/* we are in the middle of reading the arrays, and they may or may not be valid.  */
/*--------------------------------------------------------------------------------*/
void load_phrases(char *phrases) {

	int error=TRUE;
	FILE *fpi;

	if(phrases != NULL) {
		
		if((fpi = fopen(phrases, "r")) == NULL) {
			MyPerror(phrases);
		}
		else if((both_phrases = read_array(fpi, NR_BOTH_PHRASES, TRUE)) != NULL) {
			read_array(fpi, NR_RPOST_PHRASES, FALSE);	/* skip these */
			if((test_phrases = read_array(fpi, NR_TEST_PHRASES, TRUE)) != NULL) {
				error = FALSE;
			}
		}
		fclose(fpi);
		if(error == TRUE) {
			/* reset back to default */
			error_log(ERRLOG_REPORT, "Using default Language phrases\n", NULL);
			test_phrases = default_test_phrases;
			both_phrases = default_both_phrases;
		}
	}		
}
/*--------------------------------------------------------------------------------*/
void free_phrases(void) {
		/* free up the memory alloced in load_phrases() */
		if(test_phrases != default_test_phrases) {
			free_array(NR_TEST_PHRASES, test_phrases);
		}
		if(both_phrases != default_both_phrases) {
			free_array(NR_BOTH_PHRASES, both_phrases);
		}
		
}
		
		
