/*
 * $Id: host.c,v 1.8 1995/11/09 15:30:00 begemot Exp $
 * $Log: host.c,v $
 * Revision 1.8  1995/11/09  15:30:00  begemot
 * Bug caused "Internal hash management error" msg fixed, I hope.
 *
 * Revision 1.7  1995/02/26  19:21:02  begemot
 * Some improvements (?) in hash management.
 *
 * Revision 1.6  1995/02/26  18:59:25  begemot
 * Added RCS Id & Log entries into the all source files.
 *
 */

/*
 * Copyright (C) 1994-1996 D.Gorodchanin. See COPYING for more info.
 */

#include "trafshow.h"

#define HASH_BITS 8
#define HASH_SIZE (1 << HASH_BITS)
#define HASH(x)  ((x ^ (x >> 8) ^ (x >> 16) ^ (x >> 24)) & (HASH_SIZE - 1))
#define TABLE_SIZE (HOSTS_COUNT + 1)

struct hash_entry  {
	unsigned long key;
	unsigned long seq;
	unsigned int  next;
	char name[MAX_HOST_NAME + 1];
};

static struct hash_entry table[HASH_SIZE];
static unsigned int hash_table[HASH_SIZE];

static unsigned long lookup_no = 0;

static char const * lookup(unsigned long const key)
{
	int h;
	
	h = hash_table[HASH(key)];
	
	while ( h )  {
		if  (key == table[h].key)  {
			table[h].seq = ++lookup_no;
			return table[h].name;
		}
		h = table[h].next;
	}
	return NULL;
}

static char * insert(unsigned long const key, char const * const name)
{
	unsigned int i;
	unsigned long minseq = lookup_no;
	unsigned int minno = 0;
	
	for (i = 1; i < TABLE_SIZE; i++) {
		if (table[i].seq < minseq)  {
			minno = i;
			if (!(minseq = table[i].seq))  {
				break;
			}
		}
	}
	if (minseq)  {
		i = hash_table[HASH(table[minno].key)];
		if (i == minno)  {
			hash_table[HASH(table[minno].key)] = table[i].next;
		} else  {
			while (table[i].next != minno)  {
				if (!table[i].next)  {
					printf ("host: Internal logic error in hash management!!!!\n");
					exit(1);
				}
				i = table[i].next;
			}
			table[i].next = table[minno].next;
		}
	}
		
	table[minno].key = key;
	table[minno].seq = ++lookup_no;		
	strncpy(table[minno].name, name, MAX_HOST_NAME);
	table[minno].name[MAX_HOST_NAME] = '\0';
	table[minno].next = hash_table[HASH(key)];
	hash_table[HASH(key)] = minno;
	
	return table[minno].name;
}

char const * get_host_name(unsigned long const addr)
{
	char const *name;
	struct hostent *h;
	
	if ((name = lookup(addr))) {
		return name;
	}
	
	if ((h = gethostbyaddr((char *) &addr, sizeof(addr), AF_INET)))  {
		name = h->h_name;
	} else  {
		name = inet_ntoa(*((struct in_addr *) &addr));
	}
	
	return insert(addr, name);
}
