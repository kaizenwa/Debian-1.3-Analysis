
#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

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

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#include <netdb.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>
#include <errno.h>

#include "suck_config.h"
#include "both.h"
#include "suck.h"
#include "suckutils.h"
#include "dedupe.h"
#include "phrases.h"

#ifdef KILLFILE
#include "killfile.h"
#endif

#ifdef TIMER
#include "timer.h"
#endif

#ifdef MYSIGNAL 
#include <signal.h>
#endif

#ifdef CHECK_HISTORY
#include "chkhistory.h"
#endif

/* function prototypes */
int get_message_index(PMaster);
int get_articles(PMaster);
int get_one_article(PMaster, int);
int get_announcements(PMaster);
int do_supplemental(PMaster);
int do_innbatch(PMaster);
int do_rnewsbatch(PMaster);
int allocnode(PMaster, char *, int);
int restart_yn(PMaster);
int scan_args(PMaster, int, char *[]);
void do_cleanup(void);
void load_phrases(PMaster);
void free_phrases(void);
#ifdef MYSIGNAL
RETSIGTYPE sighandler(int);
void pause_signal(int, PMaster);
enum {PAUSE_SETUP, PAUSE_DO};

/*------------------------------------------*/
int GotSignal = FALSE;		
/* the only static variable allowed, it's   */
/* the only graceful way to handle a signal */
/*------------------------------------------*/
#endif

/* set up for phrases */
char **both_phrases=default_both_phrases;
char **suck_phrases=default_suck_phrases;
char **timer_phrases=default_timer_phrases;
char **chkh_phrases=default_chkh_phrases;
char **dedupe_phrases=default_dedupe_phrases;
char **killf_reasons=default_killf_reasons;
char **killf_phrases=default_killf_phrases;
char **killp_phrases=default_killp_phrases;
char **sucku_phrases=default_sucku_phrases;


enum { BATCH_FALSE, BATCH_INNXMIT, BATCH_RNEWS };		/* poss values for batch variable */
enum { MANDATORY_YES = 'M' , MANDATORY_OPTIONAL = 'O' };	/* do we have to download this article */
enum { STATUS_STDOUT, STATUS_STDERR };
enum { RESTART_YES, RESTART_NO, RESTART_ERROR };
/*------------------------------------------------------------------*/
void main(int argc, char *argv[]) {
	
	struct hostent *hi;
	Master master;
	PList temp;
	char **args, **fargs = NULL;
	int loop, fargc, retval = RETVAL_OK;

#ifdef LOCKFILE
	const char *lockfile = NULL;
#endif

	/* initialize master structure */
	master.head = master.curr = NULL;
	master.nritems =  master.itemon = master.nrgot = 0;
	master.MultiFile = FALSE;
	master.msgs = stdout;
	master.sockfd = -1;
	master.status_file = FALSE;	/* are status messages going to a file */
	master.status_file_name = NULL;
	master.do_killfile = TRUE;
	master.do_chkhistory = TRUE;
	master.do_modereader = FALSE;
	master.always_batch = FALSE;
	master.rnews_size = 0L;
	master.batch = BATCH_FALSE;
	master.batchfile = NULL;
	master.cleanup = FALSE;
	master.portnr = DEFAULT_NNRP_PORT;
	master.host = getenv("NNTPSERVER");		/* the default */
	master.pause_time = -1;
	master.pause_nrmsgs = -1;
	master.sig_pause_time = -1;
	master.sig_pause_nrmsgs = -1;
	master.ms_kludge = FALSE;	/* microsoft kludge for xhdr command */
	master.killfile_log = TRUE;	/* do we log killed messages */
	master.phrases = NULL;

	/* allow no args, only the hostname, or hostname as first arg */
	/* also have to do the file argument checking */
	switch(argc) {
	  case 1:
		break;
	  case 2:
		/* the fargs == NULL test so only process first file name */
		if(argv[1][0] == FILE_CHAR) {
			if((fargs = build_args(&argv[1][1], &fargc)) != NULL) {
				retval = scan_args(&master, fargc, fargs);
			}
		}   
		else if(argv[1][0] == '-') {
			/* in case of suck -V */
			retval = scan_args(&master, 1, &(argv[1]));
		} 
		else{	
			master.host = argv[1];
		}
		break;
	  default:
		for(loop=1;loop<argc && fargs == NULL;loop++) {
			if(argv[loop][0] == FILE_CHAR) {
				if((fargs = build_args(&argv[loop][1], &fargc)) != NULL) {
					retval = scan_args(&master, fargc, fargs);
				}
			}   
		}
		/* this is here so anything at command line overrides file */
		if(argv[1][0] != '-' && argv[1][0] != FILE_CHAR) {
			master.host = 	argv[1];
			argc-= 2;
			args = &(argv[2]);
		}
		else {
			args = &(argv[1]);
			argc--;
		}
		retval = scan_args(&master, argc, args);	
		break;
	}
	if(retval == RETVAL_OK) {
	
#ifdef DEBUG2
		do_debug("Suck version %d.%d.%d\n",SUCKVER, SUCKVER_MINOR, SUCKVER_PATCH);
		do_debug("master.MultiFile = %d\n", master.MultiFile);
		do_debug("master.msgs = %d\n", master.msgs);
		do_debug("master.status_file = %d\n",master.status_file);
		do_debug("master.status_file_name = %s\n", (master.status_file_name == NULL) ? "NULL" : master.status_file_name);
		do_debug("master.do_killfile = %d\n", master.do_killfile);
		do_debug("master.do_chkhistory = %d\n", master.do_chkhistory);
		do_debug("master.do_modereader = %d\n", master.do_modereader);
		do_debug("master.always_batch = %d\n", master.always_batch);
		do_debug("master.rnews_size = %ld\n", master.rnews_size);
		do_debug("master.batch = %d\n", master.batch);
		do_debug("master.batchfile = %s\n", (master.batchfile == NULL) ? "NULL" : master.batchfile );
		do_debug("master.cleanup = %d\n", master.cleanup);
		do_debug("master.host = %s\n", (master.host == NULL) ? "NULL" : master.host);
		do_debug("master.portnr = %u\n", master.portnr);
		do_debug("master.pause_time = %d\n", master.pause_time);
		do_debug("master.pause_nrmsgs = %d\n", master.pause_nrmsgs);
		do_debug("master.sig_pause_time = %d\n", master.sig_pause_time);
		do_debug("master.sig_pause_nrmsgs = %d\n", master.sig_pause_nrmsgs);
		do_debug("master.ms_kludge = %d\n", master.ms_kludge);
		do_debug("master.killfile_log = %d\n", master.killfile_log);
		do_debug("master.phrases = %s\n", master.phrases);
#endif

#ifdef LOCKFILE
		/* this has to be here since we use full_path() to get path for lock file */
		/* and it isn't set up until we process the args above. */
		if(do_lockfile(&master) != RETVAL_OK) {
			exit(-1);
		}
#endif

#ifdef MYSIGNAL
		signal(MYSIGNAL, sighandler); 	/* set up signal handlers */
		signal(PAUSESIGNAL, sighandler);

		signal_block(MYSIGNAL_SETUP);	/* set up sgetline() to block signal */ 
		pause_signal(PAUSE_SETUP, &master);	/* set up routine for pause swap if signal */	
#endif
		load_phrases(&master);	/* this has to be here so rest prints okay */

		/* set up status log, if none specified or unable to open status log */
		/* then  use stdout or stderr */

		if(master.status_file_name != NULL) {
			/* okay attempt to open it up */
			if((master.msgs = fopen(master.status_file_name, "a")) == NULL) {
				MyPerror(suck_phrases[0]);
			}
			else {
				master.status_file = TRUE;
			}
		}
		if(master.status_file == FALSE) {
			/* if not in multifile mode, all status msgs MUST go to stderr to not mess up articles */
			master.msgs = ( master.MultiFile == FALSE) ? stderr : stdout ;
		}
		
		print_phrases(master.msgs, suck_phrases[1], master.host, NULL);		
		master.sockfd = connect_to_nntphost( master.host, &hi, master.msgs, master.portnr);
		if(master.sockfd < 0 ) {
			retval = RETVAL_ERROR;
		}
		else if(get_announcements(&master) != RETVAL_OK) {
			retval = RETVAL_ERROR;
		}
		else {
			if((loop = restart_yn(&master)) == RESTART_YES) { 
  				print_phrases(master.msgs, suck_phrases[2], str_int(master.itemon), str_int(master.nritems), NULL);
				retval = get_articles(&master);
			}
			else if(loop == RESTART_ERROR) {
				retval = RETVAL_ERROR;
			}
			else if(get_message_index(&master) < RETVAL_OK) {
				retval = RETVAL_ERROR;
			}
			else if(master.nritems == 0) {
 				print_phrases(master.msgs,suck_phrases[3], NULL);
				retval = RETVAL_NOARTICLES;
			}
			else {
				retval = get_articles(&master);
			}	
			send_command( &master, "quit\r\n", NULL, 0 );
		}
		if(master.sockfd >= 0) {
			close(master.sockfd);
			print_phrases(master.msgs,suck_phrases[4], hi->h_name, NULL);
		}
#ifdef DEBUG2
		do_debug("retval=%d (RETVAL_OK=%d), m.nrgot=%d, m.batch=%d\n", retval, RETVAL_OK, master.nrgot, master.batch);
#endif
		if((retval == RETVAL_OK || master.always_batch == TRUE) && master.nrgot > 0) {
			switch(master.batch) {
			  case BATCH_INNXMIT:
#ifdef DEBUG2
				do_debug("calling do_innbatch()\n");
#endif
				print_phrases(master.msgs, suck_phrases[5], NULL);
				do_innbatch(&master);
				break;
			  case BATCH_RNEWS:
#ifdef DEBUG2
				do_debug("calling do_rnewsbatch()\n");
#endif
				print_phrases(master.msgs,suck_phrases[6], NULL);
				do_rnewsbatch(&master);
				break;
			  default:
#ifdef DEBUG2
				do_debug("calling unknown batcher, something screwy is up\n");
#endif

				break;
			}
		}
		if((retval == RETVAL_NOARTICLES || retval == RETVAL_OK) && master.cleanup == TRUE) {
			print_phrases(master.msgs, suck_phrases[7], NULL);
			do_cleanup();
		}
		/* close out status log */
		if(master.msgs != NULL && master.msgs != stdout && master.msgs != stderr) {
			fclose(master.msgs);
		}
		if(master.head != NULL) {
			/* clean up memory */
			master.curr = master.head;
			while(master.curr != NULL) {
				temp = (master.curr)->next;
				free_one_node(master.curr);
				master.curr = temp;
			}
		}
		if(fargs != NULL) {
			free_args(fargc, fargs);
		}		
#ifdef LOCKFILE
		lockfile = full_path(FP_GET, FP_TMPDIR, N_LOCKFILE);	
		if(lockfile != NULL) {
			unlink(lockfile);
		}
#endif	
	}
	free_phrases();	/* do this last so everything is displayed correctly */
	exit(retval);
}
/*--------------------------------------------------------------------*/
int get_message_index(PMaster master) {

	int count,low,high,response,lastread,retval,i,maxread,xhdr_result,nrread;
	char *sp,*inbuf,buf[MAXLINLEN],group[512],cmd[MAXLINLEN];
	FILE *ifp,*tmpfp;

	retval = RETVAL_OK;
	ifp = tmpfp = NULL;

#ifdef TIMER
	TimerFunc(TIMER_START, 0, NULL);
#endif
	/* msnews.microsoft.com doesn't use the standard return value */
	/* for the xhdr command, it uses 224, so I have to be able to */
	/* handle either one */
	xhdr_result = ( master->ms_kludge == FALSE) ? 221 : 224 ;

	if((ifp = fopen(full_path(FP_GET, FP_DATADIR, N_OLDRC), "r" )) == NULL) {
		MyPerror(full_path(FP_GET, FP_DATADIR, N_OLDRC));
		retval = RETVAL_ERROR;
	}
	else if((tmpfp = fopen(full_path(FP_GET, FP_TMPDIR, N_NEWRC), "w" )) == NULL) {
		MyPerror(full_path(FP_GET, FP_TMPDIR, N_NEWRC));
		retval = RETVAL_ERROR;
	}
	while(retval == RETVAL_OK && fgets(buf, MAXLINLEN-1, ifp) != NULL) {
		if(buf[0] == SUCKNEWSRC_COMMENT_CHAR) {
			/* skip this group */
			fputs(buf, tmpfp);
			print_phrases(master->msgs, suck_phrases[8], buf, NULL);
			continue;
		}
		nrread = sscanf(buf, "%s %d %d\n", group, &lastread, &maxread);
		if ( nrread < 2 || nrread > 3) {
			error_log(ERRLOG_REPORT, suck_phrases[9], buf, NULL);
			fputs(buf, tmpfp);	/* rewrite the line */
			continue;
		}
		/* now make some sanity tests on the maxread value */
		if(nrread == 2) { 
			maxread = 0;
		}
		else if (maxread <= 0) {
			error_log(ERRLOG_REPORT, suck_phrases[10], maxread, NULL);
			maxread = 0;
		}
  		sprintf(cmd,"group %s\r\n",group);
		if(send_command(master,cmd,&inbuf,0) != RETVAL_OK) {
			retval = RETVAL_ERROR;
		}
		else {
			sp = number(inbuf, &response);
			if(response != 211) {
				fputs(buf, tmpfp);	/* rewrite line AS IS in newrc */
				/* handle group not found */
				switch(response) {
				  case 411:
					error_log(ERRLOG_REPORT, suck_phrases[11], group, NULL);
					break;
				  case 500:
					error_log(ERRLOG_REPORT, suck_phrases[48], NULL);
					break;
				  default:
					error_log(ERRLOG_REPORT, suck_phrases[12],group,str_int(response),NULL);
					break;
				}
   				continue; /* next newsgroup */
			}
			sp = number(sp, &count);
  			sp = number(sp, &low);
			sp = number(sp, &high);
  			fprintf(tmpfp, "%s %d", group, high);
			
			/* now if nrread == 3, need to rewrite the maxread, else just nl */
			if(nrread==3) {
				fprintf(tmpfp, " %d", maxread);
			}
			fputs("\n", tmpfp);
  			
  			/* add a sanity check in case remote host changes its numbering scheme */
			/* the > 0 is needed, since if a nnrp site has no article it will reset */
			/* the count to 0.  So not an error */
  			if(lastread > high && high > 0) {
  				print_phrases(master->msgs,suck_phrases[13],group,str_int(high),NULL);
  			}

			if(lastread < 0 ) {
				/* if a neg number, get the last X nr of messages, handy for starting */
				/* a new group off ,say -100 will get the last 100 messages */
				lastread += high;	/* this works since lastread is neg */
				if(lastread < 0) {
					lastread = 0;	/* just to be on the safeside */
				}
			}

			/* this has to be >= 0 since if there are no article on server high = 0 */
			/* so if we write out 0, we must be able to handle zero as valid lastread */
			/* the count > 0 so if no messages available we don't even try */
			if (count > 0 && lastread < high && lastread >= 0) {
   				if(lastread < low) {
					lastread = low - 1;
				}
				/* now set the max nr of messages to read */
				if(maxread > 0 && high-maxread > lastread) {
					lastread = high-maxread;
					print_phrases(master->msgs, suck_phrases[14], group, str_int(maxread), NULL);
				}
   
   				sprintf(cmd, "xhdr Message-ID %d-\r\n", lastread+1);
   				print_phrases(master->msgs, suck_phrases[15], group, str_int(lastread+1), str_int(high), NULL);
				i = send_command(master,cmd,&inbuf,xhdr_result);
				if(i != RETVAL_OK) {
					retval = RETVAL_ERROR;
				}
				else {
					do {
     						if(sgetline(master->sockfd, &inbuf) < 0) {
							retval = RETVAL_ERROR;
						}
						else if (*inbuf != '.' ) {
							retval = allocnode(master, inbuf, MANDATORY_OPTIONAL);
						}
					} while (retval == RETVAL_OK && *inbuf != '.' && *(inbuf+1) != '\n');
				} /* end if response */
  			} /* end if lastread */
		} /* end else */
	} /* end while */
#ifdef TIMER
	TimerFunc(TIMER_TIMEONLY, 0,master->msgs);
#endif
	if(retval == RETVAL_OK) {
		print_phrases(master->msgs, suck_phrases[16], str_int(master->nritems), NULL);
		retval = do_supplemental(master);
	}
	else if(ifp != NULL) {
		/* this is in case we had to abort the above while loop (due to loss of pipe to server) and */
		/* we hadn't finished writing out the suck.newrc, this finishes it up. */
		do {
			fputs(buf, tmpfp);
		}
		while(fgets(buf, MAXLINLEN-1, ifp) != NULL);
	}
	if(tmpfp != NULL) {
		fclose(tmpfp);
	}
	if(ifp != NULL) {
		fclose(ifp);
	}
	return retval;
}
/*-----------------------------------------------------------*/
int get_articles(PMaster master) {

	int retval, logcount, i;
	FILE *logfp;

#ifdef KILLFILE
	PKillStruct killp = NULL;
#endif

	retval = RETVAL_OK;
	logfp = NULL;

	/* figure out how many digits wide the articleCount is for display purposes */
	/* this used to be log10()+1, but that meant I needed the math library */ 
	for(logcount=1, i=master->nritems; i > 9 ; logcount++) {
		i /= 10;
	}
 
#ifdef KILLFILE
	if(master->do_killfile == TRUE) {
		killp = parse_killfile(master->killfile_log);
	}
#endif		
	if(master->MultiFile == TRUE && checkdir(full_path(FP_GET, FP_MSGDIR, NULL)) == FALSE) {
		retval = RETVAL_ERROR;
	}
	else {
	 	if((logfp = fopen(full_path(FP_GET, FP_TMPDIR, N_RESTART), "w" )) == NULL) {
			MyPerror(full_path(FP_GET, FP_TMPDIR, N_RESTART));
			retval = RETVAL_ERROR;
		}
 		else {
			/* do this here so we write out the restart for all articles */
			/* already downloaded 	*/
			master->curr = master->head;
			for(i=0;i<master->itemon;i++) {
				fprintf(logfp,"1");
				master->curr = (master->curr)->next;
			}
			fflush(logfp);

#ifdef TIMER
			TimerFunc(TIMER_START, 0, NULL);
#endif
#ifdef MYSIGNAL
			while(retval == RETVAL_OK && master->curr != NULL && GotSignal == FALSE) {
#else
			while(retval == RETVAL_OK && master->curr != NULL) {
#endif
#ifdef DEBUG2
				do_debug("Article nr = %s mandatory = %c\n", (master->curr)->msgnr, (master->curr)->mandatory);
#endif
				/* write out restart */
				fputc('1',logfp);
  				fflush(logfp);
				master->itemon++;
				/* to be polite to the server, lets allow for a pause every once in a while */
				if(master->pause_time > 0 && master->pause_nrmsgs > 0) {
					if(master->itemon % master->pause_nrmsgs == 0) {
						sleep(master->pause_time);
					}
				}
				if(master->status_file == FALSE) {
					/* if we are going to a file, we don't want all of these articles printed */
#ifndef TIMER
  					fprintf(master->msgs, "%5d\r",master->nritems  - master->itemon);
#else 
					/* these don't go thru print_phrases so I can keep spacing right */
					/* and it only prints numbers */
					fprintf(master->msgs, "%5d ", master->nritems - master->itemon);
					TimerFunc(TIMER_DISPLAY, 0, master->msgs);
					fprintf(master->msgs, "\r");
#endif
					fflush(master->msgs);	/* so message gets printed now */
				}
#ifdef KILLFILE
				/* do we check for kill or not */
				retval = ((master->curr)->mandatory == MANDATORY_YES || killp == NULL) 
					? get_one_article(master, logcount) : get_one_article_kill(master, logcount, killp);
#else
				retval = get_one_article(master, logcount);
#endif
				master->curr = (master->curr)->next; 	/* get next article */
 
		 	} /* end while */
			fclose(logfp);
			if(retval == RETVAL_OK && master->nritems == master->itemon) {
				unlink(full_path(FP_GET, FP_TMPDIR, N_RESTART));
			}
#ifdef TIMER
			if(retval == RETVAL_OK) {
				TimerFunc(TIMER_TOTALS, 0, master->msgs);
			}
#endif				
		}
	}
#ifdef KILLFILE
	free_killfile(killp);
#endif
	return retval;
}
/*------------------------------------------------------------*/
int get_announcements(PMaster master) {
	char *inbuf;
	int retval = RETVAL_OK;
		
	/* Get the announcement line */
	if(sgetline(master->sockfd, &inbuf) < 0) {
		retval = RETVAL_ERROR;
	}
	else {
#ifdef DEBUG2
		do_debug("Got: %s", inbuf);
#endif
		fprintf(master->msgs,"%s", inbuf );
		if(master->do_modereader == TRUE) {
			retval = send_command(master, "mode reader\r\n", &inbuf, 0);
			if(retval == RETVAL_OK) {			
				/* Again the announcement */
				fprintf(master->msgs,"%s",inbuf);
			}
		}
	}
	return retval;
}
/*----------------------------------------------------------------------*/
/* add items from supplemental list to link list then write it out	*/
/*----------------------------------------------------------------------*/ 
int do_supplemental(PMaster master) {

	int retval, oldkept;
	FILE *fp;
	PList curr; 
	char linein[MAXLINLEN+1];

	retval = RETVAL_OK;
	oldkept = master->nritems;		

	if((fp = fopen(full_path(FP_GET, FP_DATADIR, N_SUPPLEMENTAL), "r")) != NULL) {
		print_phrases(master->msgs, suck_phrases[17], NULL);
		while(retval == RETVAL_OK && fgets(linein, MAXLINLEN, fp) != NULL) {
			if(linein[0] != '<') {
				error_log(ERRLOG_REPORT, suck_phrases[18], linein, NULL);
			}
			else {
				retval = allocnode(master, linein, MANDATORY_YES);
			}
		}
		print_phrases(master->msgs, suck_phrases[19], str_int(master->nritems-oldkept), \
			str_int(master->nritems), NULL);
		fclose(fp);
	}
	dedupe_list(master);
#ifdef TIMER
	TimerFunc(TIMER_TIMEONLY, 0, NULL);
#endif

#ifdef CHECK_HISTORY
	if(master->do_chkhistory == TRUE) {
		chkhistory(master);
	}
#endif
	print_phrases(master->msgs,suck_phrases[20], str_int(master->nritems), NULL); 

	if(master->head != NULL && master->nritems > 0) {
		/* now write out whatever list we have*/
		if((fp = fopen(full_path(FP_GET, FP_TMPDIR, N_SORTED), "w")) == NULL) {
			retval = RETVAL_ERROR;
			MyPerror(full_path(FP_GET, FP_TMPDIR, N_SORTED));
		}
		else {
			curr = master->head;
			while(curr != NULL && retval == RETVAL_OK) {
				if(fprintf(fp, "%s %c\n", curr->msgnr, curr->mandatory) == 0) {
					retval = RETVAL_ERROR;
					MyPerror(full_path(FP_GET, FP_TMPDIR, N_SORTED));
				}
				curr = curr->next;
			}
			fclose(fp);
		}
	}
	return retval;
}
/*-----------------------------------------------------------------------*/
int allocnode(PMaster master, char *linein, int mandatory) {
	/* if allocate memory here, must free in free_one_node */
	
	PList ptr = NULL;
	char *end_ptr, *st_ptr;
	static PList curr = NULL;	/* keep track of current end of list */
	int retval = RETVAL_OK;

	/* first, find the message number */
	st_ptr = linein;
	while(*st_ptr != '<' && *st_ptr != '\0') {
		st_ptr++;
	}
	end_ptr = st_ptr;
	while(*end_ptr != '>' && *end_ptr != '\0') {
		end_ptr++;
	}
	
	if(*st_ptr != '<' && *end_ptr != '>') {
		error_log(ERRLOG_REPORT, suck_phrases[21], linein, NULL);
	}
	else {
		*(end_ptr+1) = '\0';	/* ensure null termination */
		
		if((ptr = malloc(sizeof(List))) == NULL) {
			error_log(ERRLOG_REPORT, suck_phrases[22], NULL);
			retval = RETVAL_ERROR;
		}
		else if((ptr->msgnr = calloc(strlen(st_ptr)+1, sizeof(char))) == NULL) {
			error_log(ERRLOG_REPORT, suck_phrases[22], NULL);
			free(ptr);
			retval = RETVAL_ERROR;
		}
		else {
			strcpy(ptr->msgnr, st_ptr);
			ptr->next = NULL;
			ptr->mandatory = (char) mandatory;

			/* now put on list */
			if( curr == NULL) {
				/* first node */
				master->head = curr = ptr;
			}
			else {
				curr->next = ptr;
				curr = ptr;
			}
			master->nritems++;
		}
	}
	return retval;
}
/*------------------------------------------------------------------------*/
void free_one_node(PList node) {

	free(node->msgnr);
	free(node);
}
/*----------------------------------------------------------*/
int do_innbatch(PMaster master) {
	/* build batch file that contains article listing */
	/* needed by innxmit */
	/* this is done by searching thru MSGDIR to find files */
	/* which match our naming convention */

	int retval = RETVAL_OK;
	FILE *fptr;
	char tmp[20];
	DIR *dptr;
	struct dirent *entry;


	if((fptr = fopen(master->batchfile, "w")) == NULL) {
		MyPerror(master->batchfile);
		retval = RETVAL_ERROR;
	}
	else if((dptr = opendir(full_path(FP_GET, FP_MSGDIR, ""))) == NULL) {
		MyPerror(full_path(FP_GET, FP_MSGDIR, ""));
		retval = RETVAL_ERROR;
		fclose(fptr);
	}
	else {
		sprintf(tmp, "-%d",master->nritems);	/* this will be string we search for */
		/* look for entries which have our file name ...-xxxxx where xxxx is articleCount */
		while((entry = readdir(dptr)) != NULL) {
			if(strstr(entry->d_name, tmp) != NULL) {
				fprintf(fptr, "%s\n", full_path(FP_GET_NOPOSTFIX, FP_MSGDIR, entry->d_name));
			}
		}
		fclose(fptr);
		closedir(dptr);
	}
	return retval;
}
/*----------------------------------------------------------*/
int do_rnewsbatch(PMaster master) {

	/* build rnews formated file of articles */
	/* this is done by searching thru MSGDIR to find files */
	/* which match our naming convention */

	/* if max_file_size > 0, then create multiple files up to  max file size */

	int i, batchnr = 0, retval = RETVAL_OK;
	FILE *fptr = NULL, *fpin;
	const char *tptr;
	char tmp[20], buf[MAXLINLEN];
	DIR *dptr;
	struct dirent *entry;
	struct stat sbuf;
	long cursize = 0L;

	if((dptr = opendir(full_path(FP_GET, FP_MSGDIR, ""))) == NULL) {
		MyPerror(full_path(FP_GET, FP_MSGDIR, ""));
		retval = RETVAL_ERROR;
		fclose(fptr);
	}
	else {
		sprintf(tmp, "-%d",master->nritems);	/* this will be string we search for */
		/* look for entries which have our file name ...-xxxxx where xxxx is articleCount */
		while(retval == RETVAL_OK && (entry = readdir(dptr))) {
			if(strstr(entry->d_name, tmp) != NULL) {
				tptr = full_path(FP_GET_NOPOSTFIX, FP_MSGDIR, entry->d_name);
				if(stat(tptr, &sbuf) != 0 || (fpin = fopen(tptr, "r")) == NULL) {
					MyPerror(tptr);
					retval = RETVAL_ERROR;
				}
				else {
					if( cursize == 0 ) {
						if(fptr != NULL) {
							/* close old file */
							fclose(fptr);
							batchnr++;
						}
						/* have to open file */
						if(batchnr == 0) {
							strcpy(buf, master->batchfile);
						}
						else {
							sprintf(buf, "%s%d", master->batchfile, batchnr);
						}
#ifdef DEBUG2
						do_debug("BATCH FILE: %s\n", buf);
#endif
						if((fptr = fopen(buf, "w")) == NULL) {
							MyPerror(buf);
							retval = RETVAL_ERROR;
						}
					}
					if(retval == RETVAL_OK) {
						/* first put #! rnews size */
						fprintf(fptr, "#! rnews %ld\n", (long) sbuf.st_size);
		
						/* use fread/fwrite in case lines are longer than MAXLINLEN */
						while((i = fread(buf, 1, MAXLINLEN, fpin)) > 0) {
							fwrite(buf, 1, i, fptr);
						}
						fclose(fpin);
						unlink(tptr);	/* we are done with it, nuke it */
						cursize += sbuf.st_size;
						/* keep track of current file size, we can ignore the #! rnews */
						/* size, since it adds so little to the overall size */
					}
					if(master->rnews_size > 0L && cursize > master->rnews_size) {
						/* reached file size length */
						cursize = 0L;	/* this will force a close and open on next article */
					}
				}
			}
		}
		fclose(fptr);
		closedir(dptr);
	}
	return retval;
}
/*-----------------------------------------------------------------------------------------*/
int get_one_article(PMaster master, int logcount) {

	char buf[MAXLINLEN+1];
	const char *fname;
	int retval;
	FILE *fptr;

	retval = RETVAL_OK;

	/* build command to get article header*/
	sprintf(buf, "article %s\r\n", (master->curr)->msgnr);
	switch(send_command(master, buf, NULL, 220)) {
	  case RETVAL_OK:
		if(master->MultiFile == TRUE) {
			/* open file */
			/* file name will be ####-#### ex 001-166 (nron,total) */
			sprintf(buf,"%0*d-%d", logcount, master->itemon ,master->nritems);
			fname = full_path(FP_GET, FP_MSGDIR, buf);
#ifdef DEBUG2
			do_debug("File name = \"%s\"", fname);
#endif
			if((fptr = fopen(fname, "w")) == NULL) {
				MyPerror(fname);
				retval = RETVAL_ERROR;
			}
			else {
				retval = get_a_chunk(master->sockfd, fptr);
				fclose(fptr);
			}
			/* if we don't get the whole article, nuke the sucker */
			if(retval != RETVAL_OK) {
				unlink(fname);
			}
		}
		else {
			retval = get_a_chunk(master->sockfd, stdout);
			fputs(".\n", stdout);	/* needed */
		}
		if(retval == RETVAL_OK) { 
			master->nrgot++;
		}
		break;
	  case RETVAL_ERROR:
		retval = RETVAL_ERROR;
		break;
	  case RETVAL_UNEXPECTEDANS:	/* just skip to next article */
		break;
	}
	return retval;
}
/*---------------------------------------------------------------------------*/
int get_a_chunk(int sockfd, FILE *fptr) {

	int done, partial, len, retval;
	char *inbuf;

	retval = RETVAL_OK;
	done = FALSE;
	partial = FALSE;

	/* partial = was the previous line a complete line or not */
	/* this is needed to avoid a scenario where the line is MAXLINLEN+1 */
	/* long and the last character is a ., which would make us think */
	/* that we are at the end of the article when we actually aren't */
	
	while(done == FALSE && (len = sgetline(sockfd, &inbuf)) >= 0) {
#ifdef TIMER
		TimerFunc(TIMER_ADDBYTES, len, NULL);
#endif			
		if(inbuf[0] == '.' && partial == FALSE) {
			if(len == 2 && inbuf[1] == '\n') {
				done = TRUE;
			}
			else if(fptr != stdout) {
				/* handle double dots IAW RFC977 2.4.1*/
				/* if we are going to stdout, we have to leave double dots alone */
				/* since it might mess up the .\n for EOM */
				inbuf++;	/* move past first dot */
				len--;
			}
		}
		if(done == FALSE) {
			fputs(inbuf, fptr);
			fflush(fptr);
			partial= (len==MAXLINLEN&&inbuf[len-1]!='\n') ? TRUE : FALSE;
#ifdef DEBUG2
			do_debug("changing partial to %s\n", (partial == TRUE) ? "TRUE" : "FALSE" );
#endif
		} 
	}
	if(len < 0) {
		retval = RETVAL_ERROR;
	}
	return retval;
}
/*-----------------------------------------------------------------------------*/
int restart_yn(PMaster master) {
	/* the restart file consists of 1 byte for each article prior to its */
	/* being written.  So if its 2 bytes long, we've actually only got */
	/* the first article.  If its zero bytes long, we aborted before we */
	/* even got a chance to write the first byte */

	struct stat buf;
	int i, retval = RESTART_NO;
	FILE *fp;
	char msgnr[MAXLINLEN], optional;
	const char *fso;

	if(stat(full_path(FP_GET, FP_TMPDIR, N_RESTART), &buf) == 0) {
		/* If suck.restart exists something went wrong */
		retval = RESTART_YES;	
		master->itemon = (buf.st_size == 0) ? 0 : buf.st_size - 1;
		/* the above is done since we write the byte before hand */
		/* now build the link list in master */

		i = RETVAL_OK;

		fso = full_path(FP_GET, FP_TMPDIR, N_SORTED);
		if((fp = fopen(fso, "r")) != NULL) {
			while(i == RETVAL_OK && fscanf(fp, "%s %c", msgnr, &optional) == 2) {
				i = allocnode(master, msgnr, optional);
			}
			fclose(fp);
		}
		else {
			i = RETVAL_ERROR;
			error_log(ERRLOG_REPORT, suck_phrases[23],full_path(FP_GET, FP_TMPDIR,N_RESTART),fso, NULL);
		}
		if(i == RETVAL_ERROR) {
			retval = RESTART_ERROR;
		}
	}
	return retval;
}
/*-----------------------------------------------------------------*/
#ifdef MYSIGNAL
RETSIGTYPE sighandler(int what) {

	switch(what) {
	  case PAUSESIGNAL:
		pause_signal(PAUSE_DO, NULL);
		/* if we don't do this, the next time called, we'll abort */
		signal(PAUSESIGNAL, sighandler);
		signal_block(MYSIGNAL_SETUP);	/* jus to be on the safe side */ 
		break;
	  case MYSIGNAL:
	  default:
		error_log(ERRLOG_REPORT, suck_phrases[24], NULL);
		GotSignal = TRUE;
	}
}
/*----------------------------------------------------------------*/
void pause_signal(int action, PMaster master) {

	int x, y;
	static PMaster psave = NULL;

	switch(action) {
	  case PAUSE_SETUP:
		psave = master;
		break;
	  case PAUSE_DO:
		if(psave == NULL) {
			error_log(ERRLOG_REPORT, suck_phrases[25], NULL);
		}
		else {
			/* swap pause_time and pause_nrmsgs with the sig versions */
			x = psave->pause_time;
			y = psave->pause_nrmsgs;
			psave->pause_time = psave->sig_pause_time;
			psave->pause_nrmsgs = psave->sig_pause_nrmsgs;
			psave->sig_pause_time = x;
			psave->sig_pause_nrmsgs = y;
			print_phrases(psave->msgs, suck_phrases[26], NULL);
		}
	}
}		
#endif
/*----------------------------------------------------------------*/
int send_command(PMaster master, const char *cmd, char **ret_response, int good_response) {
	/* this is needed so can do user authorization */

	int len, retval = RETVAL_OK, nr;
	char buf[256], *resp;

#ifdef DEBUG2
	do_debug("sending command: %s", cmd);
#endif
	sputline(master->sockfd, cmd);
	len = sgetline(master->sockfd, &resp);	
	if( len < 0) {	
		retval = RETVAL_ERROR;		  	
	}					
	else {
#ifdef DEBUG2
		do_debug("got answer: %s", resp);
#endif
#ifdef TIMER
		TimerFunc(TIMER_ADDBYTES, len, NULL);
#endif
		number(resp, &nr);
		if(nr == 480 ) { /* we must do authorization */
			sprintf(buf, "AUTHINFO USER %s\r\n", master->userid);
#ifdef DEBUG2
			do_debug("sending command: %s", buf);
#endif
			sputline(master->sockfd, buf);
			len = sgetline(master->sockfd, &resp);
			if( len < 0) {	
				retval = RETVAL_ERROR;		  	
			}					
			else {
#ifdef DEBUG2
				do_debug("got answer: %s", resp);
#endif
#ifdef TIMER
				TimerFunc(TIMER_ADDBYTES, len, NULL);
#endif
				number(resp, &nr);
				if(nr != 381) {
					error_log(ERRLOG_REPORT, suck_phrases[27], resp, NULL);
					retval = RETVAL_NOAUTH;
				}
				else {
					sprintf(buf, "AUTHINFO PASS %s\r\n", master->passwd);
					sputline(master->sockfd, buf);
#ifdef DEBUG2
					do_debug("sending command: %s", buf);
#endif
					len = sgetline(master->sockfd, &resp);
					if(len < 0) {	
						retval = RETVAL_ERROR;		  	
					}					
					else {
#ifdef DEBUG2
						do_debug("got answer: %s", resp);
#endif
#ifdef TIMER
						TimerFunc(TIMER_ADDBYTES, len, NULL);
#endif
						number(resp, &nr);
						switch(nr) {
						  case 281: /* bingo resend original command*/
							sputline(master->sockfd, cmd);
#ifdef DEBUG2
							do_debug("sending command: %s", cmd);
#endif
							len = sgetline(master->sockfd, &resp);
							if( len < 0) {	
								retval = RETVAL_ERROR;		  	
							}
							else {
								number(resp,&nr);
#ifdef DEBUG2
								do_debug("got answer: %s", resp);
#endif
#ifdef TIMER
								TimerFunc(TIMER_ADDBYTES, len, NULL);
#endif
							}
							break;
						  case 502: /* permission denied */
							retval = RETVAL_NOAUTH;
							error_log(ERRLOG_REPORT, suck_phrases[28], NULL);
							break;
						  default: /* wacko error */
							error_log(ERRLOG_REPORT, suck_phrases[27], resp, NULL);
							retval = RETVAL_NOAUTH;
							break;
						}
					}
				}
			}
		}
		if (good_response != 0 && nr != good_response) {
			error_log(ERRLOG_REPORT, suck_phrases[29],cmd,resp,NULL);
			retval = RETVAL_UNEXPECTEDANS;
		}
	}
	if(ret_response != NULL) {
		*ret_response = resp;
	}
	return retval;
}
/*------------------------------------------------------------------------------*/
/* 1. move N_OLDRC to N_OLD_OLDRC					        */
/* 2. move N_NEWRC to N_OLDRC							*/
/* 3. rm N_SORTED								*/
/* 4. rm N_SUPPLEMENTAL								*/
/*------------------------------------------------------------------------------*/
void do_cleanup() {
	const char *oldptr;
	char ptr[PATH_MAX+1];
	strcpy(ptr,full_path(FP_GET, FP_DATADIR, N_OLDRC));	
	/* must strcpy since full path overwrites itself everytime */
	oldptr = full_path(FP_GET, FP_DATADIR, N_OLD_OLDRC);

	if(rename(ptr, oldptr) != 0) {
		MyPerror(suck_phrases[30]);
	}
	else if(rename(full_path(FP_GET, FP_TMPDIR, N_NEWRC), ptr) != 0) {
		MyPerror(suck_phrases[31]);
	}
	else if(unlink(full_path(FP_GET, FP_TMPDIR, N_SORTED)) != 0 && errno != ENOENT) {
		/* ENOENT is not an error since this file may not always exist */
		/* and it won't if there weren't any articles */
		MyPerror(suck_phrases[32]);
	}
	else if(unlink(full_path(FP_GET, FP_DATADIR, N_SUPPLEMENTAL)) != 0 && errno != ENOENT) {
		/* ENOENT is not an error since this file may not always exist */
		MyPerror(suck_phrases[33]);
	}
}
/*---------------------------------------------------------------------------------------*/
int scan_args(PMaster master, int argc, char *argv[]) {

	int loop, retval = RETVAL_OK;
	const char *ptr;
	
	for(loop=0;loop<argc && retval == RETVAL_OK;loop++) {
#ifdef DEBUG2
		do_debug("Checking arg #%d-%d: '%s'\n", loop, argc, argv[loop]);
#endif
		/* check for valid args format */
		if(argv[loop][0] == '-') {
			if(argv[loop][1] == 'b' || argv[loop][1] == 'd') {
				if(argv[loop][3] != '\0') {
					retval = RETVAL_ERROR;
				}
			}
			else if( argv[loop][2] != '\0') {
				retval = RETVAL_ERROR;
			}
		}
		else if( argv[loop][0] != FILE_CHAR ){
			/* we just ignore the file_char arg, it is processed elswhere */
			retval = RETVAL_ERROR;
		}
		if(retval != RETVAL_OK) {
			error_log(ERRLOG_REPORT, suck_phrases[34], argv[loop], NULL);
		}
		else if(argv[loop][0] != FILE_CHAR) {
			switch(argv[loop][1]) {
			  case 'a':	/* if we have downloaded at least one article, then batch up */
					/* even on errors */
				master->always_batch = TRUE;
				break;
			  case 'h':
				master->host = (loop+1 == argc) ? getenv("NNTPSERVER") : argv[++loop];
				break;	
			  case 'm':
				master->MultiFile = TRUE;
				break;
			  case 'r':
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[35], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->rnews_size = atol(argv[++loop]);
				}
				break;
			  case 'p':
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[36], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					full_path(FP_SET_POSTFIX, FP_NONE, argv[++loop]);
				}
				break;
			  case 'd':	/* change one of the directory names */
				if(loop+1 == argc) { 
					error_log(ERRLOG_REPORT, suck_phrases[37], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					ptr = NULL;
					switch (argv[loop][2]) {
					  case 't':
						ptr = full_path(FP_SET, FP_TMPDIR, argv[++loop]);
						break;
					  case 'd':
						ptr = full_path(FP_SET, FP_DATADIR, argv[++loop]);
						break;
					  case 'm':
						ptr = full_path(FP_SET, FP_MSGDIR, argv[++loop]);
						break;
					}
					if(ptr == NULL) { 
						error_log(ERRLOG_REPORT, suck_phrases[38], NULL);
						retval = RETVAL_ERROR;
						break;
					}
				}
				break;		 
			  case 'b':	/* batch file implies MultiFile Mode */
				switch (argv[loop][2]) {
				  case 'i':
					master->batch = BATCH_INNXMIT;
					master->MultiFile = TRUE;		
					break;
				  case 'r':
					master->batch = BATCH_RNEWS;
					master->MultiFile = TRUE;
					break;
				  default:
					error_log(ERRLOG_REPORT, suck_phrases[39], argv[loop], NULL);
					retval = RETVAL_ERROR;
					break;
			  	}
				if(retval == RETVAL_OK && loop+1 == argc) {
					error_log(ERRLOG_REPORT, "%s\n", suck_phrases[40], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->batchfile = argv[++loop];
				}
				break;
			  case 'e':	/* use default error log path */
				error_log(ERRLOG_SET_FILE, ERROR_LOG, NULL);
				break;
			  case 'E':	/* error log path */
				if(loop+1 == argc) { 
					error_log(ERRLOG_REPORT, suck_phrases[41], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					error_log(ERRLOG_SET_FILE, argv[++loop], NULL);
				}
				break;
			  case 's':	/* use default status log name */
				master->status_file_name = STATUS_LOG;
				break;
			  case 'S':	/* status log path */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[42], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->status_file_name = argv[++loop];
				}
				break;
			  case 'V': 	/* show version number */
			  	error_log(ERRLOG_REPORT,"Suck version %v1%.%v2%.%v3%\n",str_int(SUCKVER), \
					str_int(SUCKVER_MINOR), str_int(SUCKVER_PATCH));
				retval = RETVAL_VERNR;	/* so we don't do anything else */
			  	break;
			  case 'K':
				master->do_killfile = FALSE;
				break;
			  case 'L':
				master->killfile_log = FALSE;
				break;
			  case 'H':
				master->do_chkhistory = FALSE;
				break;
			  case 'U':	/* userid */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[43], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->userid = argv[++loop];
				}
				break;
			  case 'P':	/* passwd */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[44], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->passwd = argv[++loop];
				}
				break;
			  case 'c': 	/* cleanup option */
				master->cleanup = TRUE;
				break;
			  case 'M': /* mode reader */
				master->do_modereader = TRUE;
				break;
			  case 'N':	/* override default portnr */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[45], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->portnr = atoi(argv[++loop]);
				}
				break;
			  case 'W':	/* wait (sleep) x seconds between every y articles */
				if(loop+2 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[46], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->pause_time = atoi(argv[++loop]);
					master->pause_nrmsgs = atoi(argv[++loop]); 
				}
				break;
			  case 'w':	/* wait (sleep) x seconds between every y articles IF we get a signal */
				if(loop+2 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[46], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->sig_pause_time = atoi(argv[++loop]);
					master->sig_pause_nrmsgs = atoi(argv[++loop]); 
				}
				break;
			  case 'k':	/* microsoft news xhdr 224 kludge */
				master->ms_kludge = TRUE;
				break;
			  case 'l': 	/* load language support */
				if(loop+1 == argc) {
					error_log(ERRLOG_REPORT, suck_phrases[47], NULL);
					retval = RETVAL_ERROR;
				}
				else {
					master->phrases = argv[++loop];
				}
				break;
				
			  default:
				error_log(ERRLOG_REPORT, suck_phrases[34], argv[loop], NULL);
				retval = RETVAL_ERROR;
				break;
			}			
		}
	}

	return retval;

}
/*--------------------------------------------------------------------------------*/
/* THE strings in this routine is the only one not in the arrays, since           */
/* we are in the middle of reading the arrays, and they may or may not be valid.  */
/*--------------------------------------------------------------------------------*/
void load_phrases(PMaster master) {

	int error=TRUE;
	FILE *fpi;

	if(master->phrases != NULL) {
		
		if((fpi = fopen(master->phrases, "r")) == NULL) {
			MyPerror(master->phrases);
		}
		else if((both_phrases = read_array(fpi, NR_BOTH_PHRASES, TRUE)) != NULL) {
			read_array(fpi, NR_RPOST_PHRASES, FALSE);	/* skip these */
			read_array(fpi, NR_TEST_PHRASES, FALSE);
			if(( suck_phrases = read_array(fpi, NR_SUCK_PHRASES, TRUE)) != NULL &&
			   ( timer_phrases= read_array(fpi, NR_TIMER_PHRASES,TRUE)) &&
			   (  chkh_phrases= read_array(fpi, NR_CHKH_PHRASES,TRUE)) &&
			   (dedupe_phrases= read_array(fpi, NR_DEDUPE_PHRASES,TRUE)) &&
			   ( killf_reasons= read_array(fpi, NR_KILLF_REASONS,TRUE)) &&
			   ( killf_phrases= read_array(fpi, NR_KILLF_PHRASES,TRUE)) &&
			   ( sucku_phrases= read_array(fpi, NR_SUCKU_PHRASES,TRUE))) {
				error = FALSE;
			}
		}
		fclose(fpi);
		if(error == TRUE) {
			/* reset back to default */
			error_log(ERRLOG_REPORT, "Using default Language phrases\n", NULL);
			both_phrases = default_both_phrases;
			suck_phrases=default_suck_phrases;
			timer_phrases=default_timer_phrases;
			chkh_phrases=default_chkh_phrases;
			dedupe_phrases=default_dedupe_phrases;
			killf_reasons=default_killf_reasons;
			killf_phrases=default_killf_phrases;
			killp_phrases=default_killp_phrases;
			sucku_phrases=default_sucku_phrases;
		}
	}		
}
/*--------------------------------------------------------------------------------*/
void free_phrases(void) {
		/* free up the memory alloced in load_phrases() */
		if(both_phrases != default_both_phrases) {
			free_array(NR_BOTH_PHRASES, both_phrases);
		}
		if(suck_phrases != default_suck_phrases) {
			free_array(NR_SUCK_PHRASES, suck_phrases);
		}
		if(timer_phrases != default_timer_phrases) {
			free_array(NR_TIMER_PHRASES, timer_phrases);
		}
		if(chkh_phrases != default_chkh_phrases) {
			free_array(NR_CHKH_PHRASES, chkh_phrases);
		}
		if(dedupe_phrases != default_dedupe_phrases) {
			free_array(NR_DEDUPE_PHRASES, dedupe_phrases);
		}
		if(killf_reasons != default_killf_reasons) {
			free_array(NR_KILLF_REASONS, killf_reasons);
		}
		if(killf_phrases != default_killf_phrases) {
			free_array(NR_KILLF_PHRASES, killf_phrases);
		}
		if(killp_phrases != default_killp_phrases) {
			free_array(NR_KILLP_PHRASES, killp_phrases);
		}
		if(sucku_phrases != default_sucku_phrases) {
			free_array(NR_SUCKU_PHRASES, sucku_phrases);
		}		
}
