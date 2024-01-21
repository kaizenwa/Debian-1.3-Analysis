#ifndef _SUCK_SUCK_H
#define _SUCK_SUCK_H 1

#include <stdio.h>	/* for FILE */

/* link list structure one for each article */
typedef struct LinkList {
	struct LinkList *next;
	char *msgnr;
	char mandatory;
} List, *PList;

/* Master Structure */
typedef struct {
	PList head;
	PList curr;
	int nritems;
	int itemon;
	int nrgot;
	int sockfd;
	int MultiFile;
	int status_file;
	int do_killfile;
	int do_chkhistory;
	int do_modereader;
	int always_batch;
	int cleanup;
	int batch;
	int pause_time;
	int pause_nrmsgs;
	int sig_pause_time;
	int sig_pause_nrmsgs;
	int ms_kludge;
	int killfile_log;
	unsigned short int portnr;
	long rnews_size;
	FILE *msgs;
	const char *userid;
	const char *passwd;
	const char *host;
	const char *batchfile;
	const char *status_file_name;
	const char *phrases;
} Master, *PMaster;

int get_a_chunk(int, FILE *);
void free_one_node(PList);
int send_command(PMaster, const char *, char **, int);

enum { RETVAL_ERROR = -1, RETVAL_OK = 0, RETVAL_NOARTICLES, RETVAL_UNEXPECTEDANS, RETVAL_VERNR, RETVAL_NOAUTH };

#endif /* _SUCK_SUCK_H */
