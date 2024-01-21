/*
 * $Id: table.c,v 1.17 1995/02/26 19:21:02 begemot Exp $
 * $Log: table.c,v $
 * Revision 1.17  1995/02/26  19:21:02  begemot
 * Some improvements (?) in hash management.
 *
 * Revision 1.16  1995/02/26  18:59:25  begemot
 * Added RCS Id & Log entries into the all source files.
 *
 */

/*
 * Copyright (C) 1994-1996 D.Gorodchanin. See COPYING for more info.
 */

#include "trafshow.h"

#define HASH_BITS 8
#define HASH_SIZE (1 << HASH_BITS)
#define TABLE_SIZE (CHANNELS_COUNT + 1)
#define HASH(saddr,daddr,sport,dport,proto) \
	((((saddr) ^ (daddr) ^ (sport) ^ (dport) ^ ((proto) << 8)) >> 8) \
	& (HASH_SIZE - 1))

static struct channel_entry table[HASH_SIZE];
static unsigned int hash_table[HASH_SIZE];

static int get_free_slot()
{
	unsigned int i, hash;
	time_t t = now + 1;
	unsigned int minno = 0;
	
	for (i = 1; i < TABLE_SIZE; i++) {
		if (table[i].tm < t)  {
			minno = i;
			if (!(t = table[i].tm))  {
				return minno;
			}
		}
	}
	
	hash = HASH(table[minno].saddr, table[minno].daddr,
		    table[minno].sport, table[minno].dport, table[minno].proto);
	
	i = hash_table[hash];
	
	if (i == minno)  {
		hash_table[hash] = table[minno].next;
	} else  {
		while (table[i].next != minno)  {
			i = table[i].next;
		}
		table[i].next = table[minno].next;
	}
	return minno;
}

void update_channels (unsigned long const saddr,
		   unsigned long const daddr,
		   unsigned short const sport,
		   unsigned short const dport,
		   unsigned char const proto,
		   int const size,
		   unsigned char const * const ifname)
{
	unsigned int h, hash;
	
	hash = HASH(saddr,daddr,sport,dport,proto);
	h = hash_table[hash];
	
	while ( h )  {
		if  (saddr == table[h].saddr &&
		     daddr == table[h].daddr &&
		     sport == table[h].sport &&
		     dport == table[h].dport &&
		     proto == table[h].proto)  {
			     if (now - table[h].tm < forget_interval) {
				     if (!strcmp(table[h].ifname, ifname))  {
					     table[h].in += size;
				     }
			     } else { 
				     table[h].in = size;
				     table[h].out = 0;
				     strcpy(table[h].ifname, ifname);
			     }
			     table[h].tm = now;
			     return;
		} else if (saddr == table[h].daddr &&
		           daddr == table[h].saddr &&
			   sport == table[h].dport &&
			   dport == table[h].sport &&
			   proto == table[h].proto)  {
			     if (now - table[h].tm < forget_interval) {
				     if (!strcmp(table[h].ifname, ifname))  {
					     table[h].out += size;
				     }
			     } else { 
				     table[h].in = 0;
				     table[h].out = size;
				     strcpy(table[h].ifname, ifname);
			     }
			     table[h].tm = now;
			     return;
		}
		h = table[h].next;
	}
	
	h = get_free_slot();

	memset(&table[h], 0, sizeof(table[h]));
	if (ntohs(sport) <= ntohs(dport))  {
		table[h].saddr = saddr;
		table[h].daddr = daddr;
		table[h].sport = sport;
		table[h].dport = dport;
		table[h].in = size;
	} else  {
		table[h].saddr = daddr;
		table[h].daddr = saddr;
		table[h].sport = dport;
		table[h].dport = sport;
		table[h].out = size;
	}
	table[h].proto = proto;
	table[h].tm = now;
	table[h].next = hash_table[hash];
	strcpy(table[h].ifname, ifname);
	hash_table[hash] = h;
}

int get_channels_list(struct channel_entry * * const list,
		      int const size)
{
	unsigned int i = 0, last_i, j;
	time_t up_mark; 
	time_t low_mark;
	
	up_mark = now + 1;
	
	while (i < size)  {
		low_mark = 0;
		last_i   = i;
		for (j = 1; j < TABLE_SIZE; j++)  {
			if (table[j].tm >= low_mark && table[j].tm < up_mark) {
				if (table[j].tm != low_mark)  {
					i = last_i;
					low_mark = table[j].tm;
				}
				if (i < size)  {
					list[i++] = &table[j];
				}
			}
		}
		if (last_i == i || !(up_mark = low_mark) || 
		    now - list[last_i]->tm >= remove_interval)  {
			return last_i;
		}
	}
	return i;
}


void init_channels_table( void )
{
}
