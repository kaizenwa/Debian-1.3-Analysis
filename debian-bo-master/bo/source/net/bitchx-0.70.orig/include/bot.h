/*
 * Bot list handling code
 * Colten Edwards edwac@sasknet.sk.ca
 * 01/25/96
 *
 */
 
 
#ifndef __bot_h_
#define __bot_h_

void bot _((char *, char *, char *));
void prepare_bothost _((WhoisStuff *, char *, char *));
void save_bot _((FILE *fp));
extern UserList *Bot_list;
#endif
