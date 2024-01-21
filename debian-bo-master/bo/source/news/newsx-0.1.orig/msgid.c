/*  VER 014  TAB P   $Id: msgid.c,v 1.4 1996/11/22 12:31:52 src Exp $
 *
 *  handle the message IDs
 *  to ensure we don't read a message twice
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "news.h"

/* 
 *  globals for message IDs
 */
typedef struct message_id {
    struct message_id *next;
    char name[1]; /* extend as required... */
} MESSAGE_ID;

#define MESSAGE_HASH 999
MESSAGE_ID *messageids[MESSAGE_HASH] = { 0 };

/*
 *  check if msgid is not seen before, adding
 *  it to the list if not
 */
int new_msgid(char *msgid)
{
    int n; 
    MESSAGE_ID *mp;
    int h = hashindex(msgid,MESSAGE_HASH);

    /* find it first.. */
    for (mp = messageids[h]; mp; mp = mp->next) {
	if (strcmp(mp->name,msgid)==0) {
	    log_msg(L_DEBUG3,"msgid %s fetched already",msgid);
	    return 0;
	}
    }

    /* add to list */
    n = sizeof(MESSAGE_ID) + strlen(msgid);
    if (!(mp = malloc(n))) {
	/* out of memory */
	return 1;
    }
    strcpy(mp->name,msgid);
    mp->next = messageids[h];
    messageids[h] = mp;

    /* new */
    return 1;
}

