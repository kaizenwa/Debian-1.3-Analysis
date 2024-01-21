
%{
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifndef _OS2_
#include <asm/io.h> 
#else
#include <sys/hw.h>
#endif /* _OS2_ */
#include "pnp.h"

static char rcsid[] = "$Id: isapnp.y,v 1.9 1997/01/14 21:28:25 fox Exp $";

#ifdef REALTIME
/*
 * Code to set scheduler to Realtime, Round-Robin, so usleeps right etc
 */
#include <linux/unistd.h>
#include <linux/sched.h>
#include <sys/time.h>

int realtimeok = 0;

#ifdef NEEDSETSCHEDULER
_syscall3(int, sched_setscheduler, pid_t, pid, int, policy, struct sched_param *, sched_p)
#endif
#ifdef NEEDNANOSLEEP
_syscall2(int, nanosleep, struct timespec *, rqtp, struct timespec *, rmtp)
#endif

void
setroundrobin(void)
{
	pid_t mypid = getpid();
	struct sched_param sched_p = { 50 };
	/* Use the syscall, as my library not up todate */
	if(sched_setscheduler( mypid, SCHED_RR, &sched_p) < 0)
	{
		perror("Couldn't set real-time scheduling, may be a bit slow");
	}
	else
		realtimeok = 1;
}

/*
 * Use nanosleep for realtime delays
 */
void
delaynus(long del)
{
	struct timespec t;
	t.tv_sec = 0;
	t.tv_nsec = del * 1000;
	/*
	 * Need to handle case where binary for later kernel run on an
	 * earlier one, which doesn't support nanosleep (emergency backup spare !)
	 */
	if(realtimeok)
	{
		if(nanosleep(&t, (struct timespec *) 0) < 0)
		{
			perror("nanosleep failed");
			realtimeok = 0;
		}
	}
	else
		usleep(del);
}
#else
#define delaynus(x) usleep(x)
#endif /* REALTIME */

int yywrap () { return 1;}

int ignorecrc = 0;
int read_port = 0;
int debug = 0;
int linenumber = 1;
int initiated = 0;

/*
 * The scanner produces a list of actions, in the form of a function
 * pointer and a string argument
 */
typedef struct action
{
	int (*fn)(char *);
	char *name;
	char *arg;
	int linenum;
} ACTION, *ACTIONPTR;

#define ADDACTIONS 100

ACTION *actions = (ACTION *)0;
int maxactions = 0;
int nextaction = 0;

int parseerror = 0;

#ifdef _OS2_
unsigned inb(unsigned port){return(_inp8(port));}
unsigned inw(unsigned port){return(_inp16(port));}
unsigned inl(unsigned port){return(_inp32(port));}
void outb(unsigned value, unsigned port){_outp8(port, value);}
/* void outw(unsigned port, unsigned value){_outp16(port, value);} */
/* void outl(unsigned port, unsigned value){_outp32(port, value);} */
unsigned ioperm(unsigned port, unsigned a, unsigned b){return(0);}
unsigned iopl(unsigned port){return(0);}
unsigned usleep(unsigned wait){return(_sleep2(wait/1000));}
int strncasecmp(char *str1, char *str2, unsigned length)
{
  char *buffer1, *buffer2;
  int  counter, return_value;

  /* Allocate space for temp buffer */
  if((buffer1 = malloc(strlen(str1)+1)) == (char *)0) {
    perror("Allocating spare memory");
    exit(1);
  }
  else {
    (void)strcpy(buffer1, str1);
  }
  if((buffer2 = malloc(strlen(str2)+1)) == (char *)0) {
    perror("Allocating spare memory");
    exit(1);
  }
  else {
    (void)strcpy(buffer2, str2);
  }

  /* Decase letters to lower case */
  for(counter = 0 ; counter < strlen(str1) ; counter++)
    if(str1[counter] >= 'a')
      buffer1[counter] = (char)('A' + (str1[counter]-'a'));
  for(counter = 0 ; counter < strlen(str2) ; counter++)
    if(str2[counter] >= 'a')
      buffer2[counter] = (char)('A' + (str2[counter]-'a'));

  /* Now calculate the return value */
  return_value = strncmp(str1, str2, length);

  /* Free up the space */
  (void)free(str1);
  (void)free(str2);

  /* Now give the return value */
  return(return_value);
}
void *yy_flex_alloc(int size) 
{
  void *ptr;

  /* Allocate the space */
  if((ptr = malloc(size)) == (void *)0) {
    perror("Allocating Flex Buffer");
    exit(1);
  }

  /* Return the pointer */
  return(ptr);
}
void *yy_flex_realloc(void *ptr, int size)
{
  void *nptr;

  /* Reallocate space */
  if((nptr = realloc(ptr, size)) == (void *)0) {
    perror("ReAllocating Flex Buffer");
    exit(1);
  }

  /* Return the new pointer */
  return(nptr);
}
void yy_flex_free(void *ptr){(void)free(ptr);}
#endif /* _OS2_ */

int
addaction(int (*fn)(char*), int skip)
{
	char *sptr = yytext+skip;
	/* Make sure there is some room */
	if(nextaction == maxactions)
	{
		maxactions += ADDACTIONS;
		actions = (ACTION *)realloc((void *)actions, sizeof(ACTION) * maxactions);
		if(!actions)
		{
			/* This is, like, fatal */
			fprintf(stderr, "Unable to allocate memory for action list processing line %d\n", linenumber);
			parseerror = 1;
			return 0;
		}
	}
	actions[nextaction].fn = fn;
	actions[nextaction].name = strdup(yytext);
	actions[nextaction].linenum = linenumber;
	while((*sptr) && isspace(*sptr))
	{
		if(*sptr == '\n')
			linenumber++;
		sptr++;
	}
	if(*sptr)
		actions[nextaction].arg = strdup(sptr);
	else
		actions[nextaction].arg = "";
	nextaction++;
	return 1;
}

int
IgnoreCRC(char *s)
{
	ignorecrc = 1;
	return 0;
}

int
Read_Port(char *s)
{
	/* Read decimal or hex number */
	read_port = (int)strtol(s, (char**)NULL, 0);
	if((read_port < MIN_READ_ADDR)||(read_port > MAX_READ_ADDR))
	{
		fprintf(stderr, "Port address %s (0x%04x) out of range 0x203..0x3ff\n", s, read_port);
		return 1;
	}
	read_port |= 3;
	if(ioperm(read_port, 1, 1) < 0)
	{
		fprintf(stderr, "Unable to obtain ioperm for data port %04x - ", read_port);
		perror("");
		return 1;
	}
	return 0;
}

int
checkreadport(void)
{
	if(!read_port)
	{
		fprintf(stderr, "READPORT not set\n");
		return(1);
	}
	return(0);
}

#define IDENT_LEN 9
#define TMP_LEN 16
#define LARGE_LEN 65536
#define MAXSLOTS 32

unsigned char serial_identifier[MAXSLOTS+1][IDENT_LEN];
char *boardid[MAXSLOTS+1];
unsigned long serno[MAXSLOTS+1];
unsigned char tmp[TMP_LEN];
unsigned char large[LARGE_LEN];

int boards_found = 0;

int do_isolate(void);
char * devidstr(unsigned char, unsigned char, unsigned char, unsigned char);

static char initdata[INIT_LENGTH] = INITDATA;

void
initiate(void)
{
	int i;
	ADDRESS(0);
	ADDRESS(0);
	for(i = 0; i < INIT_LENGTH; i++)
		ADDRESS(initdata[i]);
	delaynus(2000L);
	initiated = 1;
}

int
Isolate(char *s)
{
	int i;

	ADDRESS(0);
	ADDRESS(0);
	for(i = 0; i < INIT_LENGTH; i++)
		ADDRESS(initdata[i]);
	/* Reset the cards */
	CONFIGCONTROL;
	WRITE_DATA(CONFIG_RESET_DRV);
	delaynus(2000L);
	/* Send the key again */
	ADDRESS(0);
	ADDRESS(0);
	for(i = 0; i < INIT_LENGTH; i++)
		ADDRESS(initdata[i]);
	delaynus(2000L);
	initiated = 1;
	/* All cards now isolated, read the first one */
	if(read_port) /* Port specified */
	{
		do_isolate();
	}
	else
	{
		for(read_port = MIN_READ_ADDR; read_port <= MAX_READ_ADDR; read_port += 4)
		{
			if(debug)
				printf("Trying port address %04x\n", read_port);
			if(ioperm(read_port, 1, 1) < 0)
				continue;
			if(do_isolate())
				break;
		}
	}
	if(!boards_found)
		return 1; /* Error */
	if(debug)
	{
		printf("Board %d has serial identifier", boards_found);
		for(i = IDENT_LEN; i-- ; )
			printf(" %02x", serial_identifier[boards_found][i]);
		printf(" (%s/%ld)\n", boardid[boards_found], serno[boards_found]);
	}
	while(do_isolate())
	{
		if(debug)
		{
			printf("Board %d has serial identifier", boards_found);
			for(i = IDENT_LEN; i--; )
				printf(" %02x", serial_identifier[boards_found][i]);
			printf(" (%s/%ld)\n", boardid[boards_found], serno[boards_found]);
		}
	}
	return 0;
}

int
do_isolate(void)
{
	int csum;
	int i;
	int index;
	int newbit;
	int goodaddress = 0;
	int nextboard = boards_found + 1;
	unsigned char c1, c2;
	csum = 0x6a;
	Wake(0);
	SetRdPort(read_port);
	delaynus(1000L);
	SERIALISOLATION;
	delaynus(1000L);
	for(index = 0; index < IDENT_LEN - 1; index++)
	{
		for(i = 0; i < 8; i++)
		{
			newbit = 0x00;
			/* Two reads per identifier bit */
			delaynus(250L);
			c1 = READ_DATA;
			delaynus(250L);
			c2 = READ_DATA;
			if(c1 == 0x55)
			{
				if(c2 == 0xAA)
				{
					goodaddress = 1;
					newbit = 0x80;
				}
				else
				{
					goodaddress = 0;
				}
			}
			serial_identifier[nextboard][index] >>= 1;
			serial_identifier[nextboard][index] |= newbit;
			/* Update checksum */
			if(((csum >> 1) ^ csum) & 1)
				newbit ^= 0x80;
			csum >>= 1;
			csum |= newbit;
		}
		/*
		printf("%02x ", serial_identifier[nextboard][index]);
		*/
	}
	/*
	printf("csum is %02x\n", csum);
	*/
	for(i = 0; i < 8; i++)
	{
		newbit = 0x00;
		/* Two reads per identifier bit */
		delaynus(250L);
		c1 = READ_DATA;
		delaynus(250L);
		c2 = READ_DATA;
		if(c1 == 0x55)
		{
			if(c2 == 0xAA)
			{
				goodaddress = 1;
				newbit = 0x80;
			}
		}
		serial_identifier[nextboard][index] >>= 1;
		serial_identifier[nextboard][index] |= newbit;
	}
	if(goodaddress && (ignorecrc || (serial_identifier[nextboard][8] == csum)))
	{
		boardid[nextboard] = strdup(devidstr(serial_identifier[nextboard][0],
				serial_identifier[nextboard][1], serial_identifier[nextboard][2],
				serial_identifier[nextboard][3])); 
		serno[nextboard] = serial_identifier[nextboard][7] << 24;
		serno[nextboard] |= serial_identifier[nextboard][6] << 16;
		serno[nextboard] |= serial_identifier[nextboard][5] << 8;
		serno[nextboard] |= serial_identifier[nextboard][4];
		boards_found = nextboard;
		CARDSELECTNUMBER;
		WRITE_DATA(nextboard);
		return(1);
	}
	/*
	else
	{
		printf("csum = %02x\n", csum);
	}
	*/
	return(0);
}

char *
devidstr(unsigned char d1, unsigned char d2, unsigned char d3, unsigned char d4)
{
	static char resstr[] = "PNP0000";
	if(d1 & 0x80)
		return("-------");
	sprintf(resstr, "%c%c%c%x%x%x%x", 'A' + (d1 >> 2) - 1, 'A' + (((d1 & 3) << 3) | (d2 >> 5)) - 1,
	       'A' + (d2 & 0x1f) - 1, d3 >> 4, d3 & 0x0f, d4 >> 4, d4 & 0x0f);
	return resstr;
}

int
statuswait(void)
{
#ifdef REALTIME
#define TIMEOUTLOOPS 100
	int to; /* For timeout */
	/*
	 * Try for up to 1ms
	 */
	for(to = 0; to < TIMEOUTLOOPS; to++)
	{
		if(STATUS & 1)
			break;
		delaynus(10L);
	}
	if(to >= TIMEOUTLOOPS)
	{
		fprintf(stderr, "Timeout attempting to read ident - is READPORT correct ?\n");
		return(1);
	}
#else
	/*
	 * Infinite loop potentially, but if we usleep, we may lose 10ms
	 */
	while(!(STATUS & 1))
		;
#endif /* REALTIME */
	return(0);
}

int
ident(int csn)
{
	int i;
	unsigned char id[IDENT_LEN];
	if((csn < 1)||(csn > boards_found))
		return(1);
	fflush(stdout);
	if(checkreadport())
		return(1);
	Wake(csn);
	delaynus(250L);
	for(i = 0; i < IDENT_LEN; i++)
	{
		if(statuswait())
			return(1);
		id[i] = RESOURCEDATA;
	}
	printf("Board %d has Identity", csn);
	fflush(stdout);
	for(i = IDENT_LEN; i--; )
		printf(" %02x", id[i]);
	fflush(stdout);
	strcpy(boardid[csn],devidstr(id[0],id[1],id[2],id[3]));
	serno[csn] = (unsigned long) id[4] + (id[5] << 8) +  (id[6] << 16) + (id[7] << 24);
	printf(":  %s Serial No %lu [checksum %02x]\n", boardid[csn], serno[csn], id[8]);
	fflush(stdout);
	return(0);
}

int
Identify(char *s)
{
	int err = 0;
	int csn;
	if(*s == '*')
	{
		for(csn = 1; (!err) && (csn <= boards_found); csn++)
			err |= ident(csn);
	}
	else
	{
		csn = (int)strtol(s, (char**)NULL, 0);
		err = ident(csn);
	}
	return err;
}

int
WaitForKey(char *s)
{
	CONFIGCONTROL;
	WRITE_DATA(CONFIG_WAIT_FOR_KEY);
	return 0;
}

int csn;

int
SelectCSN(char *s)
{
	if(!initiated)
		initiate();
	csn = (unsigned int)strtol(s, (char**)NULL, 0);
	/* Check it's sensible, and we won't have array problems */
	if((csn < 1)||(csn > MAXSLOTS))
		return 1;
	if(csn > boards_found)
	{
		for(boards_found++; csn >= boards_found; boards_found++)
			boardid[boards_found] = strdup("-------");
		boards_found = csn;
	}
	if(checkreadport())
		return(1);
	Wake(csn);
	return 0;
}

int
SelectIdent(char *s)
{
	unsigned int sn = (unsigned int)strtol(s+8, (char**)NULL, 0);
	for(csn = 1; csn <= boards_found; csn++)
		if((serno[csn] == sn)&&(strncasecmp(s, boardid[csn], 7) == 0))
			break;
	if((csn < 1)||(csn > boards_found))
	{
		printf("Board %s not found\n", s);
		return(1);
	}
	if(debug)
		printf("Found board %s as Card Select Number %d\n", s, csn);
	if(checkreadport())
		return(1);
	Wake(csn);
	return 0;
}

/*
 * Misc global register setting
 */

int LogDev;

int
SetLogicalDevice(char *s)
{
	LogDev = (int)strtol(s, (char**)NULL, 0);
	LOGICALDEVICENUMBER;
	WRITE_DATA(LogDev);
	return READ_DATA != LogDev;
}

int
Activate(char *s)
{
	ACTIVATE;
	if(toupper(*s) == 'Y')
		WRITE_DATA(1);
	else
		WRITE_DATA(0);
	return 0;
}

/*
 * Register address, etc, for peek and poke
 */
#define MAXNAMELEN 16

int Reg = 0;
int RegSize = 0;
char RegName[MAXNAMELEN] = {0};

/*
 * Interrupt resource setting
 */

int INTReg = 0;

int
SelectINTReg(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	if((n < 0)||(n > 1))
		return 1;
	INTReg = n;
	Reg = 0x70 + (n << 1);
	RegSize = 2;
	strcpy(RegName, "INT 0");
	RegName[4] = n + '0';
	return 0;
}

int
SetIRQLine(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	if((n < 0)||(n > 15))
		return 1;
	ADDRESS(0x70 + (INTReg << 1));
	WRITE_DATA(n);
	return 0;
}

int
SetIRQMode(char *s)
{
	int n = 0;
	if(*s++ == '+')
		n |= 2;
	if(toupper(*s) == 'L')
		n |= 1;
	ADDRESS(0x71 + (INTReg << 1));
	WRITE_DATA(n);
	return 0;
}

/*
 * I/O resource setting
 */

int IOReg = 0;

int
SelectIOReg(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	if((n < 0)||(n > 7))
		return 1;
	IOReg = n;
	Reg = 0x60 + (n << 1);
	RegSize = 2;
	strcpy(RegName, "IO  0");
	RegName[4] = n + '0';
	return 0;
}

int
SetIOBaseReg(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	if((n < 0)||(n > 0xffff))
		return 1;
	ADDRESS(0x60 + (IOReg << 1));
	WRITE_DATA(n >> 8);
	ADDRESS(0x61 + (IOReg << 1));
	WRITE_DATA(n & 0xff);
	return 0;
}

/*
 * DMA resource setting
 */

int DMAReg = 0;

int
SelectDMAReg(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	if((n < 0)||(n > 1))
		return 1;
	DMAReg = n;
	Reg = 0x74 + n;
	RegSize = 1;
	strcpy(RegName, "DMA 0");
	RegName[4] = n + '0';
	return 0;
}

int
SetDMAChannelReg(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	if((n < 0)||(n > 7))
		return 1;
	ADDRESS(0x74 + DMAReg);
	WRITE_DATA(n);
	return 0;
}

/*
 * Memory resource setting stuff
 */

int MemReg = 0;

int
SelectMemReg(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	if((n < 0)||(n > 3))
		return 1;
	MemReg = n;
	Reg = 0x40 + (n << 3);
	RegSize = 8;
	strcpy(RegName, "MEM 0");
	RegName[4] = n + '0';
	return 0;
}

int
SetMemBaseReg(char *s)
{
	unsigned long n;
	n = strtoul(s, (char**)NULL, 0);
	/* Specify as absolute address, check for bits not programable */
	if(n & 0xff0000ff)
		return 1;
	ADDRESS(0x40 + (MemReg << 3));
	WRITE_DATA(n >> 16);
	ADDRESS(0x41 + (MemReg << 3));
	WRITE_DATA((n >> 8) & 0xff);
	return 0;
}

/*
 * Modes [wb][ru]
 * w = 16 bit
 * b = 8 bit
 * r = upper is range length
 * u = upper is upper limit
 * Note that R/U is a read only bit, so verify
 */
int
SetMemModeReg(char *s)
{
	int n = 0;
	if(toupper(*s) == 'W')
		n |= 2;
	s++;
	if(toupper(*s) == 'U')
		n |= 1;
	ADDRESS(0x42 + (MemReg << 3));
	WRITE_DATA(n);
	if((READ_DATA ^ n) & 3)	/* Verify */
		return 1;
	return 0;
}

int
SetMemUpperReg(char *s)
{
	unsigned long n;
	n = strtoul(s, (char**)NULL, 0);
	/* Specify as absolute address, check for bits not programable */
	if(n & 0xff0000ff)
		return 1;
	ADDRESS(0x43 + (MemReg << 3));
	WRITE_DATA(n >> 16);
	ADDRESS(0x44 + (MemReg << 3));
	WRITE_DATA((n >> 8) & 0xff);
	return 0;
}

/*
 * General purpose logical device peek and poke
 */

int
SelectReg(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	if((n < 0)||(n > 0xff))
		return 1;
	Reg = n;
	RegSize = 1;
	strcpy(RegName, "-REG-");
	return 0;
}

int
PokeReg(char *s)
{
	int n;
	n = (int)strtol(s, (char**)NULL, 0);
	/* Can only poke bytes */
	if(RegSize > 1)
	{
		fprintf(stderr, "Can only poke bytes\n");
		return(1);
	}
	if((n < 0)||(n > 0xff))
		return 1;
	ADDRESS(Reg);
	WRITE_DATA(n);
	return 0;
}

/*
 * Peek will work for named stuff too
 */
int
PeekReg(char *s)
{
	int i;
	ADDRESS(Reg);
	printf("Peek(%s/%ld)[%d][0x%02x](%s) is 0x%02X", boardid[csn], serno[csn], LogDev, Reg, RegName, READ_DATA);
	for(i = 1; i < RegSize; i++)
	{
		ADDRESS(Reg + i);
		printf("%02X", READ_DATA);
	}
	ADDRESS(Reg);
	printf(", (%d", READ_DATA);
	for(i = 1; i < RegSize; i++)
	{
		ADDRESS(Reg + i);
		printf(",%d", READ_DATA);
	}
	printf(")\n");
	return 0;
}

void
printgot(void)
{
	if(debug) printf("Got %s\n", yytext);
}

#define ONDEBUG() if (debug) printgot()
#define ADDACTION(x, y) if(!addaction(x, y)) yyterminate()
%}

/*
# LD n      = Logical device, ie write to reg 7
# IO n      = IO descriptor n
# MEM n     = MEM descriptor n (24bit descriptors only for ISA)
# DMA n     = DMA descriptor n
# REG n     = Select register n
# POKE n    = Poke a value
# PEEK      = Read a value and print on stdout
# BASE n    = IO/Mem address n
# UPPER n   = Mem upper address/range n
# INT n     = Interrupt req n
# IRQ n     = Interrupt level n
# MODE      = IRQ line levels etc/Memory width etc
# CHANNEL n = DMA channel
# ACT       = Activate

(IGNORECRC)
(READPORT 0x207)
(ISOLATE3
(CSN 1 (LD 0 (REG 0x42 (POKE 0x00) (PEEK))))
(CONFIGURE DFX0000/1493 (LD 0 (REG 0x42 (POKE 0x00) (PEEK))))
(CONFIGURE DFX0000/1493 (LD 0 (MEM 0 (BASE 0x400000) (MODE wu) (UPPER 0x800000)) (DMA 0 (CHANNEL 1))))
(CONFIGURE DFX0000/1493 (LD 0 (IO 0 (BASE 0x3e8)) (INT 0 (IRQ 12 (MODE +E))) (ACT Y)))
(WAITFORKEY)
*/

DIGIT	[0-9]
HEXDIGIT	[0-9a-f]
ID		[@A-Z[\\^\]_]{3}{HEXDIGIT}{4}

%s LEVEL1
%s LEVEL2
%s LEVEL3
%s LEVEL4IO
%s LEVEL4MEM
%s LEVEL4DMA
%s LEVEL4INT
%s LEVEL4REG
%s LEVEL5INT
%s NOMORE
%s NOMLEVEL3
%s CONFIG
%s LOGDEV
%s IOSET
%s MEMSET
%s DMASET
%s ACTSET
%s INTSET
%s REGSET
%s IRQSET
%s PEARSHAPED

%%

<INITIAL>"("	BEGIN(LEVEL1);

<LEVEL1>")"		BEGIN(INITIAL);
<LEVEL1>"DEBUG"	BEGIN(NOMORE); debug = 1; ONDEBUG();
<LEVEL1>"IGNORECRC"	BEGIN(NOMORE); ADDACTION(IgnoreCRC, 9); ONDEBUG();
<LEVEL1>"READPORT"[ \t\n]*0x{HEXDIGIT}+ |
<LEVEL1>"READPORT"[ \t\n]*{DIGIT}+	{ BEGIN(NOMORE); ADDACTION(Read_Port, 8); ONDEBUG(); }
<LEVEL1>"IDENTIFY"[ \t\n]*\* |
<LEVEL1>"IDENTIFY"[ \t\n]*0x{HEXDIGIT}+ |
<LEVEL1>"IDENTIFY"[ \t\n]*{DIGIT}+	{ BEGIN(NOMORE); ADDACTION(Identify, 8); ONDEBUG(); }
<LEVEL1>"ISOLATE"	BEGIN(NOMORE); ADDACTION(Isolate, 7); ONDEBUG();
<LEVEL1>"CONFIGURE"[ \t\n]*{ID}"/"-?{DIGIT}+ 	BEGIN(CONFIG); ADDACTION(SelectIdent, 9); ONDEBUG();
<LEVEL1>"CSN"[ \t\n]*{DIGIT}+ 	BEGIN(CONFIG); ADDACTION(SelectCSN, 3); ONDEBUG();
<LEVEL1>"WAITFORKEY"	BEGIN(NOMORE); ADDACTION(WaitForKey, 10); ONDEBUG();

<NOMORE>")"		BEGIN(INITIAL);

<CONFIG>")"		BEGIN(INITIAL);
<CONFIG>"("		BEGIN(LEVEL2);

<LEVEL2>")"		BEGIN(CONFIG);
<LEVEL2>"LD"[ \t\n]*{DIGIT}+	BEGIN(LOGDEV); ADDACTION(SetLogicalDevice, 2); ONDEBUG();

<LOGDEV>")"		BEGIN(CONFIG);
<LOGDEV>"("		BEGIN(LEVEL3);

<LEVEL3>")"		BEGIN(LOGDEV);
<LEVEL3>"IO"	BEGIN(IOSET); ONDEBUG();
<LEVEL3>"MEM"	BEGIN(MEMSET); ONDEBUG();
<LEVEL3>"DMA"	BEGIN(DMASET); ONDEBUG();
<LEVEL3>"INT"	BEGIN(INTSET); ONDEBUG();
<LEVEL3>"REG"	BEGIN(REGSET); ONDEBUG();
<LEVEL3>"ACT"	BEGIN(ACTSET); ONDEBUG();

<ACTSET>[YN]	BEGIN(NOMLEVEL3); ADDACTION(Activate, 0); ONDEBUG();
<IOSET,INTSET,MEMSET,DMASET,REGSET,ACTSET,NOMLEVEL3>")"		BEGIN(LOGDEV);

<IOSET>{DIGIT}+	ADDACTION(SelectIOReg, 0); ONDEBUG();
<IOSET>"("		BEGIN(LEVEL4IO);

<MEMSET>{DIGIT}+	ADDACTION(SelectMemReg, 0); ONDEBUG();
<MEMSET>"("		BEGIN(LEVEL4MEM);

<DMASET>{DIGIT}+	ADDACTION(SelectDMAReg, 0); ONDEBUG();
<DMASET>"("		BEGIN(LEVEL4DMA);

<INTSET>{DIGIT}+	ADDACTION(SelectINTReg, 0); ONDEBUG();
<INTSET>"("		BEGIN(LEVEL4INT);

<REGSET>0x{HEXDIGIT}+	|
<REGSET>{DIGIT}+	ADDACTION(SelectReg, 0); ONDEBUG();
<REGSET>"("		BEGIN(LEVEL4REG);

<LEVEL4IO>")"		BEGIN(IOSET);
<LEVEL4IO>"BASE"[ \t\n]*0x{HEXDIGIT}+	|
<LEVEL4IO>"BASE"[ \t\n]*{DIGIT}+	ADDACTION(SetIOBaseReg, 4); ONDEBUG();
<LEVEL4IO>"PEEK"	ADDACTION(PeekReg, 4); ONDEBUG();

<LEVEL4MEM>")"		BEGIN(MEMSET);
<LEVEL4MEM>"BASE"[ \t\n]*0x{HEXDIGIT}+	|
<LEVEL4MEM>"BASE"[ \t\n]*{DIGIT}+	ADDACTION(SetMemBaseReg, 4); ONDEBUG();
<LEVEL4MEM>"UPPER"[ \t\n]*0x{HEXDIGIT}+	|
<LEVEL4MEM>"UPPER"[ \t\n]*{DIGIT}+	ADDACTION(SetMemUpperReg, 5); ONDEBUG();
<LEVEL4MEM>"MODE"[ \t\n]*[BW][RU]	ADDACTION(SetMemModeReg, 4); ONDEBUG();
<LEVEL4MEM>"PEEK"	ADDACTION(PeekReg, 4); ONDEBUG();

<LEVEL4DMA>")"		BEGIN(DMASET);
<LEVEL4DMA>"CHANNEL"[ \t\n]*{DIGIT}+	ADDACTION(SetDMAChannelReg, 7); ONDEBUG();
<LEVEL4DMA>"PEEK"	ADDACTION(PeekReg, 4); ONDEBUG();

<LEVEL4INT>")"		BEGIN(INTSET);
<LEVEL4INT>"IRQ"[ \t\n]*{DIGIT}+	BEGIN(IRQSET); ADDACTION(SetIRQLine, 4); ONDEBUG();
<LEVEL4INT>"PEEK"	ADDACTION(PeekReg, 4); ONDEBUG();

<LEVEL4REG>")"		BEGIN(REGSET);
<LEVEL4REG>"POKE"[ \t\n]*0x{HEXDIGIT}+	|
<LEVEL4REG>"POKE"[ \t\n]*{DIGIT}+	ADDACTION(PokeReg, 4); ONDEBUG();
<LEVEL4REG>"PEEK"	ADDACTION(PeekReg, 4); ONDEBUG();

<IRQSET>")"		BEGIN(INTSET);
<IRQSET>"("		BEGIN(LEVEL5INT);

<LEVEL5INT>")"		BEGIN(IRQSET);
<LEVEL5INT>"MODE"[ \t\n]*[+-][EL]	ADDACTION(SetIRQMode, 3); ONDEBUG();

"#"	{ /* Swallow comment to eol */ register int c; while(((c = input()) != '\n') && (c != EOF)) ; linenumber++; }
[ \t]*	/* Swallow whitespace */
[\n]	{ linenumber++; /* Count these */ }
.	BEGIN(PEARSHAPED); fprintf(stderr, "Don't know what to do with %s", yytext); parseerror = 1;

<PEARSHAPED>.*$	fprintf(stderr, "%s on or around line %d\n", yytext, linenumber); yyterminate();

%%

int
main(int argc, char **argv)
{
	int i;
	int retval;
	if(argc != 2)
	{
		fprintf(stderr, "%s\nThis is free software, see the sources for details.\nThis software has NO WARRANTY, use at your OWN RISK\n\nUsage: %s configfile\n", rcsid, argv[0]);
		exit(1);
	}
	if(ioperm(ADDRESS_ADDR, 1, 1) < 0)
	{	
		perror("Unable to get io permission for ADDRESS");
		exit(1);
	}
	/* Have to get unrestricted access to io ports, as WRITE_DATA port > 0x3ff */
	if(iopl(3) < 0)
	{	
		perror("Unable to get io permission for WRITE_DATA");
		ioperm(ADDRESS_ADDR, 1, 0);
		exit(1);
	}
	retval = 0;
	yyin = fopen(argv[1], "r");
	yylex();
	if(parseerror)
	{
		fprintf(stderr, "Error occurred parsing config file on line %d --- no action taken\n", linenumber);
		retval = 1;
	}
	else
	{
#ifdef REALTIME
		setroundrobin();
#endif /* REALTIME */
		for(i = 0; i < nextaction; i++)
		{
			if(debug)
				printf("Executing %s\n", actions[i].name);
			if(actions[i].fn(actions[i].arg))
			{
				fflush(stdout); /* Make sure peeks etc output first */
				fprintf(stderr, "Error occurred executing request '%s' on or around line %d --- further action aborted\n", actions[i].name, actions[i].linenum);
				retval = 1;
				break;
			}
		}
	}
	/* Release resources */
	ioperm(ADDRESS_ADDR, 1, 0);
	ioperm(read_port, 1, 0);
	return retval;
}

