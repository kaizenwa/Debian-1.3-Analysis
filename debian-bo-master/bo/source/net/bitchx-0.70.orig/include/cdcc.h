#ifndef _CDCC_H_
#define _CDCC_H_

/* local commands */
extern double cdcc_minspeed;

typedef struct {
	char *name;
	int (*function)(char *args, char *rest);
	char *help;
} local_cmd;

/* remote commands */
typedef struct {
	char *name;
	int (*function)(char *from, char *args);
	char *help;
} remote_cmd;

/* offer pack type */
typedef struct packtype {
	int num;
	char *file;
	char *desc;
	char *notes;
	int numfiles;
	int gets;
	unsigned long size;
	double minspeed;
	struct packtype *next;
} pack;

/* cdcc queue struct */
typedef struct queuetype {
	char *nick;
	char *file;
	int numfiles;
	time_t time;
	char *desc;
	int num;
	struct queuetype *next;
} queue;
	
/* local command parser */
void cdcc(char *, char *, char *);

/* remote message command parser */
char *msgcdcc(char *, char *, char *);

/* local commands */
/* -------------- */

/* cdcc help */
int l_help(char *, char *);

/* add a pack to the offer list */
int l_offer(char *, char *);

/* send a pack to someone */
int l_send(char *, char *);

/* re-send a pack to someone */
int l_resend(char *, char *);

/* publicly list offered packs */
int l_plist(char *, char *);

/* remove a pack or all packs from the offer list */
int l_doffer(char *, char *);

/* localy list offered packs */
int l_list(char *, char *);

/* notify the channel that packs are offered */
int l_notice(char *, char *);

/* view your queue */
int l_queue(char *, char *);

/* save all offered packs to cdcc.save */
int l_save(char *, char *);

/* load packs from cdcc.save */
int l_load(char *, char *);

int l_minspeed(char *, char *);

/* remote CDCC commands */
/* -------------------- */

/* show help to a remote user */
int r_help(char *, char *);

/* list packs offered to remote user */
int r_list(char *, char *);

/* send pack to remote user */
int r_send(char *, char *);

/* re-send pack to remote user */
int r_rsend(char *, char *);


/* misc commands */
/* ------------- */

/* add files */
void add_files(char *, char *);

/* add description */
void add_desc(char *, char *);

/* remove pack/all packs */
void del_pack(char *, char *);

/* add/remove public channel */
int l_channel(char *, char *);

/* add note to a pack */
int l_note(char *, char *);

int l_echo(char *, char *);
int l_type(char *, char *);

/* add a person to the dcc queue */
int add_to_queue(char *, pack *);

/* send a file from the queue */
void dcc_sendfrom_queue _((void));

void cdcc_timer_offer _((void));

#endif
