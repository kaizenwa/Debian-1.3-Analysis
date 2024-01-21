/*
 * Userlist functions and definitions.
 * Copyright Colten Edwards 1996
 * 
 */
 
#ifndef _user_list_h
#define _user_list_h

void add_shit _((char *, char *, char *));
void prepare_userhost _((WhoisStuff *, char *, char *));
void add_user _((char *, char *, char *));
void add_to_a_list _((char *, int, char *, int, int, int, char *, char *, int));
void add_to_linked_list _((NickList **, char *, char *, int, int, int, char *, char *, int)); 
void showlist _((NickList *, char *));
void showuserlist _((char *, char *, char *));
UserList *lookup_userlevelc _((char *, char *, char *, UserList *));

UserList *nickinuser _((char *, char *));
UserList *nickinbot _((char *, char *));
ShitList *nickinshit _((char *, char *));

int	find_user_level _((char *, char *, char *));
int	find_shit_level _((char *, char *, char *));

void savelists _((char *, char *, char *));
NickList *check_auto _((char *, char *, char *, ChannelList *));
int check_prot _((char *, char *, ChannelList *, BanList *, NickList *));
void check_shit _((ChannelList *));
void check_hack _((char *, ChannelList *, NickList *, char *));
void change_user _((char *, char *, char *));
int check_channel_match _((char *, char *));
int delay_check_auto _((char *));
extern UserList *user_list;
extern ShitList *shitlist_list;
extern LameList *lame_list;
extern WordKickList *ban_words;

#define SHITLIST_ADD 11
#define SHITLIST_REMOVE 12
#define USERLIST_ADD 13
#define USERLIST_REMOVE 14
#define USERLIST_MOD 15
#define BOTLIST_REMOVE 16


#endif

