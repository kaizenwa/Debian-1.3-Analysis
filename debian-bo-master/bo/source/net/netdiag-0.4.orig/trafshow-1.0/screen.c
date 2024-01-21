/*
 * $Id: screen.c,v 1.22 1995/02/26 19:55:45 begemot Exp $
 * $Log: screen.c,v $
 * Revision 1.22  1995/02/26  19:55:45  begemot
 * ICMP reqest-reply packets now displayed as single line.
 *
 * Revision 1.21  1995/02/26  19:21:02  begemot
 * Change <user:host> to <user@host>
 *
 * Revision 1.20  1995/02/26  18:59:25  begemot
 * Added RCS Id & Log entries into the all source files.
 *
 */

/*
 * Copyright (C) 1994-1996 D.Gorodchanin. See COPYING for more info.
 */

#include "trafshow.h"

#define TOP		1	       /* Reseved top lines */
#define BOTTOM		1	       /* Reserved bottom lines */

static int top_attr;
static int tcp_attr;
static int udp_attr;
static int icmp_attr;
static int unkn_attr;
static int norm_attr;

static void clearline(int line)
{
	int i;

	move(line, 0);
	attrset(norm_attr);
	for (i = 0; i < COLS; i++)
		addstr(" ");	
	move(line, 0);
}

static void get_user_name(struct channel_entry *p)
{
	FILE *f;
	int i = 0;
	unsigned long saddr, daddr;
	unsigned int sport, dport;
	unsigned int uid;
	unsigned char *target = NULL;
	struct passwd *pwd;
	
	switch (p->proto)  {
	 case IPPROTO_TCP:
		f = fopen(_PATH_TCP_INFO,"r");
		break;
	 case IPPROTO_UDP:
		f = fopen(_PATH_UDP_INFO,"r");
		break;
	 default :
		return;
	}
	if (!f)  {
		return;
	}
	do  {
		i = getc(f);
	} while (i != '\n' && i != EOF);
			
	while (fscanf(f, "%*d: %lX:%X %lX:%X %*X %*X:%*X %*X:%*X %*X %d %*d %*d\n",
		      &saddr, &sport, &daddr, &dport, &uid) == 5)  {
		if (saddr == p->saddr && daddr == p->daddr &&
		    sport == htons(p->sport) && dport == htons(p->dport))  {
			    target = p->suser;
		    }
		if (daddr == p->saddr && saddr == p->daddr &&
		    dport == htons(p->sport) && sport == htons(p->dport))  {
			    target = p->duser;
		    }
		if (target)  {
			if (!uid || !(pwd = getpwuid(uid)))  {
				break;
			}
			strncpy(target, pwd->pw_name, MAX_USER_NAME);
			target[strlen(target)] = '\0';
			break;
		}
	}
	fclose (f);
	return;
}

static void get_serv_name( unsigned short const port, char const * const proto,
			  char * const where)
{
	struct servent *s;
#if 1
	static int broken = -1;

	if (broken < 0) {
		broken = (getservbyport(htons(7),"tcp") == NULL);
	} 
	
	if ((s = getservbyport(broken ? ntohs(port) : port, proto)))  {
#else 
	if ((s = getservbyport(port, proto)))  {
#endif
		strncpy(where, s->s_name, MAX_SERV_NAME);
		where[MAX_SERV_NAME] = '\0';
	} else  {
		sprintf(where, "%-*d", MAX_SERV_NAME, ntohs(port)); 
	}
}

static void get_icmp_type( int const type, char * const where )
{
	struct icmptypes  {
		unsigned char t;
		char *n;
	};
	
	static struct icmptypes table[] =  {
		{  0, "echo"  },
		{  3, "destun" },
		{  4, "sqnch" },
		{  5, "redir" },
		{  8, "echorq" },
		{ 11, "timexd" },
		{ 12, "parmpb" },
		{ 13, "timerq" },
		{ 14, "time" },
		{ 15, "inforq" },
		{ 16, "info" },
		{ 17, "addrrq" },
		{ 18, "addr" },
		{  0, NULL },
	};
	
	int i;
	
	for (i = 0; table[i].n; i++)  {
		if (table[i].t == type)  {
			strncpy(where, table[i].n, MAX_SERV_NAME);
			where[MAX_SERV_NAME] = '\0';
			return;
		}
	}
	sprintf(where, "%-*d", MAX_SERV_NAME, type);
}

static void draw_host (unsigned char *host, unsigned char *user)
{
	unsigned char hostinfo[MAX_HOST_NAME + 1];
	unsigned int len = MAX_HOST_NAME;

	if (*user)  {
		strncpy(hostinfo , user, MAX_HOST_NAME - 3);
		len = strlen(hostinfo);
		hostinfo[len] = '@';
		hostinfo[++len] = '\0';
		addstr(hostinfo);
		len = MAX_HOST_NAME - len;
	}
	strncpy(hostinfo, host, len);
	hostinfo[MAX_HOST_NAME] = '\0';
	addstr(hostinfo);
}
	

static void draw_line (struct channel_entry * const p)
{
	unsigned char bytes[MAX_DATA_SIZE + 1 + MAX_DATA_SIZE + 1];
		
	clearline(p->line + TOP);
	
	if (!(*p->suser && *p->duser) && (p->get_user_try)++ < 5) {
		get_user_name(p); 
	}
	
	move(p->line + TOP, MAX_PROT_NAME + 1);
	if (dont_resolve)  {
		draw_host(inet_ntoa(*((struct in_addr *)&p->saddr)), p->suser);
	} else  {
		draw_host((char *) get_host_name(p->saddr), p->suser);
	}
	move(p->line + TOP, MAX_PROT_NAME + 1 + MAX_HOST_NAME + 1 + MAX_SERV_NAME + 1);
	if (dont_resolve)  {
		draw_host(inet_ntoa(*((struct in_addr *)&p->daddr)), p->duser);
	} else  {
		draw_host((char *) get_host_name(p->daddr), p->duser);
	}
	
	memset(bytes, ' ', sizeof(bytes) - 1);
	if (p->in)  {
		sprintf(bytes,"%*ld ", MAX_DATA_SIZE, p->in);
		bytes[MAX_DATA_SIZE] = ' ';
	}
	if (p->out)  {
		sprintf(bytes + MAX_DATA_SIZE + 1, "%*ld ", MAX_DATA_SIZE, p->out);
	}
	bytes[sizeof(bytes) - 1] = '\0';
		
	move(p->line + TOP, MAX_PROT_NAME + 1 + 
	     2 * ( MAX_HOST_NAME + 1 + MAX_SERV_NAME + 1));
	     
	addstr(bytes);
	
	move(p->line + TOP, 0);
	
	if (!(*p->suser && *p->duser)) {
		get_user_name(p); 
	}
	
	switch (p->proto)  {
	 case IPPROTO_TCP:
		attrset(tcp_attr);
		addstr("tcp");
		
		if (!*p->sserv)  {
			get_serv_name(p->sport, "tcp", p->sserv);
		}
		move(p->line + TOP, MAX_PROT_NAME + 1 + MAX_HOST_NAME + 1);
		addstr(p->sserv);
		if (!*p->dserv)  {
			get_serv_name(p->dport, "tcp", p->dserv);
		}
		move(p->line + TOP, MAX_PROT_NAME + 1 + MAX_HOST_NAME + 1
		     + MAX_SERV_NAME + 1 + MAX_HOST_NAME + 1 );
		addstr(p->dserv);
		
		break;
	 case IPPROTO_UDP:	
		attrset(udp_attr);
		addstr("udp");
		
		if (!*p->sserv)  {
			get_serv_name(p->sport, "udp", p->sserv);
		}
		move(p->line + TOP, MAX_PROT_NAME + 1 + MAX_HOST_NAME + 1);
		addstr(p->sserv);
		
		if (!*p->dserv)  {
			get_serv_name(p->dport, "udp", p->dserv);
		}
		move(p->line + TOP, MAX_PROT_NAME + 1 + MAX_HOST_NAME + 1
		     + MAX_SERV_NAME + 1 + MAX_HOST_NAME + 1 );
		addstr(p->dserv);
		
		break;
	 case IPPROTO_ICMP:
		attrset(icmp_attr);
		addstr("icmp");
		
		if (!*p->sserv)  {
			get_icmp_type(p->sport, p->sserv);
		}
		move(p->line + TOP, MAX_PROT_NAME + 1 + MAX_HOST_NAME + 1);
		addstr(p->sserv);
		if (p->dport != 0xffff)  {
			if (!*p->dserv)  {
				get_icmp_type(p->dport, p->dserv);
			}
			move(p->line + TOP, MAX_PROT_NAME + 1 + MAX_HOST_NAME + 1
			     + MAX_SERV_NAME + 1 + MAX_HOST_NAME + 1 );
			addstr(p->dserv);
		}
		break;
	 default:	
		attrset(unkn_attr);
		addstr("unkn");
		break;
	}
	attrset(norm_attr);
}

void screen_update(unsigned long const total, unsigned long const ill, 
		   unsigned long const frag, unsigned long const tcp,
		   unsigned long const udp, unsigned long const icmp,
		   unsigned long const unkn )
{
	struct channel_entry * * list;
	char * records;
	char string[80];
	
	int i, j;
	int count;
	int up;
	
	static int last_count = 0;
	static int last_seq = 1;
	
	
	list = (struct channel_entry **) 
		alloca ((LINES - TOP - BOTTOM) * sizeof(*list)); 
	records = (char *) alloca ((LINES - TOP - BOTTOM) * sizeof(*records)); 
	
	count = get_channels_list(list, LINES - TOP - BOTTOM);
		
#ifdef DEBUG
	printf("Screen update\n");
#endif	
	memset(records, 0, (LINES - TOP - BOTTOM) * sizeof(*records));
	
	while (last_count > count)  {
		last_count--;
		clearline(last_count + TOP);
	}
	
	for (i = 0; i < count; i++)  {
		if (list[i]->scr_seq == last_seq && list[i]->line < count) {
			records[list[i]->line] = 1;
			list[i]->scr_seq++;
			draw_line(list[i]);
			list[i] = NULL;
		}
	}
	
	last_seq++;
	
	for (i = j = 0; i < count; i++)  {
		if (list[i])  {
			while (records[j])  {
				j++;
			}
			list[i]->line = j;
			list[i]->scr_seq = last_seq;
			draw_line(list[i]);
			j++;
		}
	}

	up = now - start;
	sprintf(string, "%d days %02d hrs %02d min %02d sec",
		up / 24 / 60 / 60,
		up / 60 / 60 % 24,
		up / 60 % 60,
		up % 60);
	move(0, MIN_RAWS - strlen(string));
	attrset(top_attr);
	addstr(string);
	clearline(LINES - BOTTOM);
	sprintf(string, "%ldK total, %ldK bad, %ldK frag",
		total / 1024, ill / 1024, frag / 1024);

	attrset(top_attr);
	addstr(string);
	
	sprintf(string,"%ldK tcp, %ldK udp, %ldK icmp, %ldK unkn",
		tcp / 1024, udp / 1024, icmp / 1024, unkn / 1024);
	move(LINES - BOTTOM, MIN_RAWS - strlen(string) - 1);
	addstr(string);
	attrset(norm_attr);
	
	refresh();
	last_count = count;
}

void screen_open( void )
{
	char string[80];
	int i;
	
	initscr();
	
	if (LINES < MIN_LINES || COLS < MIN_RAWS)  {
		endwin();
		fprintf(stderr, "Screen too small.\n");
		exit(1);
	}
	start_color();
	cbreak();
	noecho();
	
	if (has_colors() && !force_mono)  {
		init_pair(1, COLOR_WHITE, COLOR_BLACK);
		top_attr  = A_BOLD | COLOR_PAIR(1);
		
		init_pair(2, COLOR_GREEN, COLOR_BLACK);
		tcp_attr  = COLOR_PAIR(2);
		
		init_pair(3, COLOR_CYAN, COLOR_BLACK);
		udp_attr  = COLOR_PAIR(3);
		
		init_pair(4, COLOR_RED, COLOR_BLACK);
		icmp_attr = COLOR_PAIR(4);
		
		init_pair(5, COLOR_YELLOW, COLOR_BLACK);
		unkn_attr = COLOR_PAIR(5);
		
		init_pair(6, COLOR_WHITE, COLOR_BLACK);
		norm_attr = COLOR_PAIR(6) | A_DIM;
	} else  {
		top_attr  = A_BOLD;
		tcp_attr  = A_NORMAL;
		udp_attr  = A_NORMAL;
		icmp_attr = A_BOLD;
		unkn_attr = A_STANDOUT;
		norm_attr = A_NORMAL;
	}
	
	attrset(norm_attr);
	for (i = 0; i < LINES; i++) {
		clearline(i);
	}

	move(0,0);
	attrset(top_attr);
	gethostname(string, sizeof(string)); 
	addstr(string);
	
	attrset(norm_attr);
	refresh();
}

void screen_close( void )
{
	
	clear();
	refresh();
	endwin();
}
