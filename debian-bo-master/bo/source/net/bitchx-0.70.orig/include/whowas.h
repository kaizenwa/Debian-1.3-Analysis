
#ifndef _WhoWas_h
#define _WhoWas_h

#define whowas_userlist_max 75
#define whowas_reg_max 200
#define whowas_chan_max 20

typedef struct _whowaschan_str {
	struct _whowaschan_str *next;
	char *channel;
	ChannelList *channellist;
	time_t time;
} WhowasChanList;

typedef struct _whowas_str {
	struct _whowas_str *next;
	int	has_ops; 	/* true is user split away with opz */
	char 	*channel;	/* name of channel */
	char 	*server1;	/* name of their server */
	char 	*server2;	/* name of our server */
	time_t 	time;		/* time of split/leave */
	NickList *nicklist;	/* pointer to nicklist */
	ShitList *shitlist;	/* pointer to shitlist */
	ChannelList *channellist;		
} WhowasList;

WhowasList *check_whowas_buffer _((char *, char *, char *, int));
WhowasList *check_whowas_nick_buffer _((char *, char *, int));
WhowasList *find_in_whowas _((char *));
WhowasList *check_whosplitin_buffer _((char *, char *, char *, int));

void add_to_whowas_buffer _((NickList *, char *, char *, char *));
void add_to_whosplitin_buffer _((NickList *, char *, char *, char *));

int remove_oldest_whowas _((WhowasList **, time_t, int));
void clean_whowas_list _((void));
void sync_whowas_adduser _((UserList *));
void sync_whowas_unuser _((UserList *));
void sync_whowas_addshit _((ShitList *));
void sync_whowas_unshit _((ShitList *));
WhowasChanList *check_whowas_chan_buffer _((char *, int));
void add_to_whowas_chan_buffer _((ChannelList *));
int remove_oldest_chan_whowas _((WhowasChanList **, time_t, int));
void clean_whowas_chan_list _((void));
void show_whowas _((void));
void show_wholeft _((char *));

extern WhowasList *whowas_splitin_list;

#endif
