#include <stdio.h>
#ifndef _OS2_
#include <asm/io.h>
#else
#include <sys/hw.h>
#endif /* _OS2_ */
#include <unistd.h>
#include <stdlib.h>

#define TAG_DEBUG 0
#define DUMPADDR 0

#include "pnp.h"

#define NUM_CARDS 128
#define IDENT_LEN 9
#define TMP_LEN 16
#define LARGE_LEN 65536

static char rcsid[] = "$Id: pnpdump.c,v 1.8 1997/01/14 21:05:35 fox Exp $";

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
#endif /* _OS2_ */

unsigned char serial_identifier[NUM_CARDS+1][IDENT_LEN];
unsigned char tmp[TMP_LEN];
unsigned char large[LARGE_LEN];

int read_port;
int boards_found = 0;

int do_isolate(void);
void initiate(void);
void dumpdata(int);
void read_idents(void);

#ifdef DUMPREGS
void dumpregs(int);
#endif /* DUMPREGS */

char *devidstr(unsigned char, unsigned char, unsigned char, unsigned char);

int
main(int argc, char **argv)
{
	int i;
	if((argc != 1)&&(argc != 3))
	{
		fprintf(stderr, "%s\nThis is free software, see the sources for details.\nThis software has NO WARRANTY, use at your OWN RISK\n\nUsage: %s\n", rcsid, argv[0]);
		exit(1);
	}
	/* If a number of boards and read_port have been given, don't ISOLATE */
	if(argc == 3)
	{
		boards_found = atoi(argv[1]);
		if((boards_found < 0)||(boards_found >= NUM_CARDS))
		{
			fprintf(stderr, "Cannot handle %d boards, recompile with NUM_CARDS bigger\n", boards_found);
			exit(1);
		}
		/* Read decimal or hex number */
		read_port = (int)strtol(argv[2], (char**)NULL, 0);
		if((read_port < MIN_READ_ADDR)||(read_port > MAX_READ_ADDR))
		{
			fprintf(stderr, "Port address %s (0x%04x) out of range 0x203..0x3ff\n", argv[2], read_port);
			exit(1);
		}
		read_port |= 3;
		if(ioperm(read_port, 1, 1) < 0)
		{
			fprintf(stderr, "Unable to obtain ioperm for data port %04x - ", read_port);
			perror("");
			exit(1);
		}
	}
	if(ioperm(ADDRESS_ADDR, 1, 1) < 0)
	{
		perror("Unable to get io permission for ADDRESS");
		exit(1);
	}
	/*
	 * Have to get unrestricted access to io ports, as WRITE_DATA port >
	 * 0x3ff
	 */
	if(iopl(3) < 0)
	{
		perror("Unable to get io permission for WRITE_DATA");
		exit(1);
	}
#ifdef REALTIME
	setroundrobin();
#endif /* REALTIME */
	printf("# %s\n# This is free software, see the sources for details.\n# This software has NO WARRANTY, use at your OWN RISK\n# \n# For details of this file format, see isapnp.conf(5)\n#\n# Compiler flags:", rcsid);
#ifdef REALTIME
	printf(" -DREALTIME");
#endif /* REALTIME */
#ifdef NEEDSETSCHEDULER
	printf(" -DNEEDSETSCHEDULER");
#endif /* NEEDSETSCHEDULER */
#ifdef NEEDNANOSLEEP
	printf(" -DNEEDNANOSLEEP");
#endif /* NEEDNANOSLEEP */
#ifdef DUMPREGS
	printf(" -DDUMPREGS");
#endif /* DUMPREGS */
	printf("\n#\n");
	initiate();
	for(i = 1; i <= boards_found; i++)
	{
		dumpdata(i);
#ifdef DUMPREGS
		dumpregs(i);
#endif /* DUMPREGS */
	}
	/* Release resources */
	ioperm(ADDRESS_ADDR, 1, 0);
	ioperm(read_port, 1, 0);
	return (0);
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
		fprintf(stderr, "Timeout attempting to read resource data - is READPORT correct ?\n");
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

int port;

void
initiate(void)
{
	static char initdata[INIT_LENGTH] = INITDATA;
	int i;

	ADDRESS(0);
	ADDRESS(0);
	for(i = 0; i < INIT_LENGTH; i++)
		ADDRESS(initdata[i]);
	if(!boards_found)
	{
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
		/* All cards now isolated, read the first one */
		for(port = MIN_READ_ADDR; port <= MAX_READ_ADDR; port += 4)
		{
			printf("# Trying port address %04x\n", port);
			if(ioperm(port, 1, 1) < 0)
				continue;
			if(do_isolate())
				break;
		}
		if(port > MAX_READ_ADDR)
		{
			printf("# No boards found\n");
			exit(0);
		}
		printf("# Board %d has serial identifier", boards_found);
		for(i = IDENT_LEN; i-- ; )
			printf(" %02x", serial_identifier[boards_found][i]);
		printf("\n");
		while(do_isolate())
		{
			printf("# Board %d has serial identifier", boards_found);
			for(i = IDENT_LEN; i--;)
				printf(" %02x", serial_identifier[boards_found][i]);
			printf("\n");
		}
		printf("\n# (DEBUG)\n(READPORT 0x%04x)\n(ISOLATE)\n(IDENTIFY *)\n\n", port);
	}
	else
	{
		read_idents();
		printf("\n# (DEBUG)\n(READPORT 0x%04x)\n(CSN %d)\n(IDENTIFY *)\n\n", read_port, boards_found);
	}
}

void
read_idents(void)
{
	int csn = 0;
	int i;
	for(csn = 1; csn <= boards_found; csn++)
	{
		Wake(csn);
		for(i = 0; i < IDENT_LEN; i++)
		{
			if(statuswait())
				return;
			serial_identifier[csn][i] = RESOURCEDATA;
		}
		printf("# Board %d has serial identifier", csn);
		for(i = IDENT_LEN; i--;)
			printf(" %02x", serial_identifier[csn][i]);
		printf("\n");
		if(serial_identifier[csn][0] == 0xff)
			boards_found = csn - 1;
	}
}

int
do_isolate(void)
{
	unsigned char c1, c2;
	int csum;
	int i;
	int index;
	int newbit;
	int goodaddress = 0;
	if(boards_found >= NUM_CARDS)
	{
		fprintf(stderr, "Too many boards found, recompile with NUM_CARDS bigger\n");
		return 0;
	}
	csum = 0x6a;
	/* Assume we will find one */
	boards_found++;
	Wake(0);
	SetRdPort(port);
	delaynus(1000L);
	SERIALISOLATION;
	delaynus(1000L);
	for(index = 0; index < IDENT_LEN - 1; index++)
	{
		for(i = 0; i < 8; i++)
		{
			newbit = 0x00;
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
			/* printf("%02x %02x - ",c1,c2); */
			serial_identifier[boards_found][index] >>= 1;
			serial_identifier[boards_found][index] |= newbit;
			/* Update checksum */
			if(((csum >> 1) ^ csum) & 1)
				newbit ^= 0x80;
			csum >>= 1;
			csum |= newbit;
		}
		/* printf("%02x ", serial_identifier[boards_found][index]); */
		/* printf("\n"); */
	}
	/* printf("computed csum is %02x", csum); */
	for(i = 0; i < 8; i++)
	{
		newbit = 0x00;
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
		/* printf("%02x %02x | ",c1,c2); */
		serial_identifier[boards_found][index] >>= 1;
		serial_identifier[boards_found][index] |= newbit;
	}
	/* printf("\n"); */
	if(goodaddress && (csum == serial_identifier[boards_found][index]))
	{
		CARDSELECTNUMBER;
		WRITE_DATA(boards_found);
		return (1);
	}
#ifdef NOTACHANCE
	else
	{
		printf("csum = %02x\n", csum);
	}
#endif
	/* We didn't find one */
	boards_found--;
	return (0);
}

char *
devidstr(unsigned char d1, unsigned char d2, unsigned char d3, unsigned char d4)
{
	static char resstr[] = "PNP0000";
	sprintf(resstr, "%c%c%c%x%x%x%x", 'A' + (d1 >> 2) - 1, 'A' + (((d1 & 3) << 3) | (d2 >> 5)) - 1,
			'A' + (d2 & 0x1f) - 1, d3 >> 4, d3 & 0x0f, d4 >> 4, d4 & 0x0f);
	return resstr;
}

void
dumpdata(int csn)
{
	int i;
	int len = 0;
	int type = 0;
	int csum = 0;
	int logdevno = 0;
	int nestdepth = 0;
	int curdma = 0, depdma = 0;
	int curio = 0, depio = 0;
	int curint = 0, depint = 0;
	int curmem = 0, depmem = 0;
	int starteddeps = 0;
	char *indep = "";
	Wake(csn);
	printf("# Card %d: (serial identifier", csn);
	for(i = IDENT_LEN; i--;)
		printf(" %02x", serial_identifier[csn][i]);
	printf(")\n");
	printf("# %s Serial No %ld [checksum %02x]\n",
		   devidstr(serial_identifier[csn][0],
					serial_identifier[csn][1],
					serial_identifier[csn][2],
					serial_identifier[csn][3]),
		   (long) serial_identifier[csn][4] +
		   (serial_identifier[csn][5] << 8) +
		   (serial_identifier[csn][6] << 16) +
		   (serial_identifier[csn][7] << 24),
		   serial_identifier[csn][8]);
	/* Check for broken cards that don't reset their resource pointer properly */
	for(i = 0; i < TMP_LEN; i++)
		tmp[i] = 0;
	/* Get the first byte */
	if(statuswait())
		return;
	tmp[0] = RESOURCEDATA;
	/* Just check the first byte, if these match we assume it's ok */
	if(tmp[0] != serial_identifier[csn][0])
	{
		printf("# Ident byte %d, (%02x) differs from resource data (%02x)\n", i, serial_identifier[csn][i], tmp[0]);
		printf("# Assuming the card is broken and this is the start of the resource data\n");
		goto broken;	/* Wouldn't need this if people read the spec */
	}
	/* Read resource data past serial identifier - As it should be spec section 4.5 para 2 */
	for(i = 1; i < IDENT_LEN; i++)
	{
		if(statuswait())
			return;
		/* Use tmp[0] as a temporary vbl */
		tmp[0] = RESOURCEDATA;
		/*
		 * Checksum byte is not guaranteed, but the previous bytes should match
		 */
		if((tmp[0] != serial_identifier[csn][i])&&(i < (IDENT_LEN - 1)))
			printf("# Ident byte %d, (%02x) differs from resource data (%02x)\n", i, serial_identifier[csn][i], tmp[0]);
	}
	/* Now for the actual resource data */
	do
	{
		for(i = 0; i < TMP_LEN; i++)
			tmp[i] = 0;
		if(statuswait())
			return;
		tmp[0] = RESOURCEDATA;
broken:
		csum += tmp[0];
		/* Get the rest of the resource data into the buffers */
		if(tmp[0] & 0x80)
		{
			/* Large item */
			type = tmp[0];
			for(i = 1; i <= 2; i++)
			{
				if(statuswait())
					return;
				tmp[i] = RESOURCEDATA;
				csum += tmp[i];
			}
			len = (tmp[2] << 8) | tmp[1];
			for(i = 0; i < len; i++)
			{
				if(statuswait())
					return;
				large[i] = RESOURCEDATA;
				csum += large[i];
				csum &= 0xff;
			}
		}
		else
		{
			/* Small item */
			type = (tmp[0] >> 3) & 0x0f;
			len = tmp[0] & 7;
			for(i = 1; i <= len; i++)
			{
				if(statuswait())
					return;
				tmp[i] = RESOURCEDATA;
				csum += tmp[i];
				csum &= 0xff;
			}
		}
#if TAG_DEBUG
		printf("# %s %02X Tag %02X[%02X] :",
			   (tmp[0] & 0x80) ? "L" : "S", tmp[0], type, len);
		if(tmp[0] & 0x80)
		{
			for(i = 0; i < len; i++)
			{
				printf(" %02x", large[i]);
			}
		}
		else
		{
			for(i = 1; i <= len; i++)
			{
				printf(" %02x", tmp[i]);
			}
		}
		printf("\n");
#endif
		switch (type)
		{
		case PnPVerNo_TAG:
			{
				printf("# %sVersion %d.%d, Vendor version %x.%x\n", indep,
					   tmp[1] >> 4,
					   tmp[1] & 0x0f,
					   tmp[2] >> 4,
					   tmp[2] & 0x0f);
				continue;
			}
		case LogDevId_TAG:
			{
				int reg;
				static char *regfns[8] =
				{"Device capable of taking part in boot process",
				 "Device support I/O range check register",
				 "Device supports reserved register @ 0x32",
				 "Device supports reserved register @ 0x33",
				 "Device supports reserved register @ 0x34",
				 "Device supports reserved register @ 0x35",
				 "Device supports reserved register @ 0x36",
				 "Device supports reserved register @ 0x37"};
				indep = "";
				if(nestdepth)
				{
					printf("# (ACT Y)\n");
					while(nestdepth--)
						printf(")");
					printf("\n");
				}
				printf("#\n# %sLogical device id %s\n", indep, devidstr(tmp[1], tmp[2], tmp[3], tmp[4]));
				indep = "    ";
				if(tmp[5])
				{
					for(i = 1, reg = 2; reg < 256; i++, reg <<= 1)
					{
						if(tmp[5] & reg)
							printf("# %s%s\n", indep, regfns[i]);
					}
				}
				if(tmp[6])
				{
					for(i = 0, reg = 1; reg < 256; i++, reg <<= 1)
					{
						if(tmp[6] & reg)
							printf("# %sDevice supports vendor reserved register @ 0x%02x\n", indep, 0x38 + i);
					}
				}
				printf("#\n# Edit the entries below to uncomment out the configuration required.\n");
				printf("# Note that only the first value of any range is given, this may be changed if required\n");
				printf("# Don't forget to uncomment the activate (ACT Y) when happy\n\n");
				printf("(CONFIGURE %s/%ld (LD %d\n",
					   devidstr(serial_identifier[csn][0],
								serial_identifier[csn][1],
								serial_identifier[csn][2],
								serial_identifier[csn][3]),
					   (long) serial_identifier[csn][4] +
					   (serial_identifier[csn][5] << 8) +
					   (serial_identifier[csn][6] << 16) +
					   (serial_identifier[csn][7] << 24),
					   logdevno++);
				nestdepth = 2;
				curdma = 0; depdma = 0;
				curio = 0; depio = 0;
				curint = 0; depint = 0;
				curmem = 0; depmem = 0;
				starteddeps = 0;
				continue;
			}
		case CompatDevId_TAG:
			{
				printf("# %sCompatible device id %s\n", indep, devidstr(tmp[1], tmp[2], tmp[3], tmp[4]));
				continue;
			}
		case IRQ_TAG:
			{
				int irq, firstirq = 0;
				int first = 1;
				char *edge = "+E";
				if((len >= 2) && (tmp[1] || tmp[2]))
				{
					printf("# %sIRQ", indep);
					for(irq = 0; tmp[1]; irq++)
					{
						if(tmp[1] & 1)
						{
							printf("%s%d",
							   (tmp[1] == 1 && !first && !tmp[2]) ? " or " :
								   (!first) ? ", " : " ", irq);
							if(first)
								firstirq = irq;
							first = 0;
						}
						tmp[1] >>= 1;
					}
					for(irq = 8; tmp[2]; irq++)
					{
						if(tmp[2] & 1)
						{
							printf("%s%d",
								   (tmp[2] == 1 && !first) ? " or " :
								   (!first) ? ", " : " ", irq);
							if(first)
								firstirq = irq;
							first = 0;
						}
						tmp[2] >>= 1;
					}
					printf(".\n");
					if(len == 3)
					{
						if(tmp[3] & 1)
						{
							printf("# %s%sHigh true, edge sensitive interrupt\n", indep, indep);
							edge = "+E";
						}
						if(tmp[3] & 2)
						{
							printf("# %s%sLow true, edge sensitive interrupt\n", indep, indep);
							edge = "-E";
						}
						if(tmp[3] & 4)
						{
							printf("# %s%sHigh true, level sensitive interrupt\n", indep, indep);
							edge = "+L";
						}
						if(tmp[3] & 8)
						{
							printf("# %s%sLow true, level sensitive interrupt\n", indep, indep);
							edge = "+L";
						}
					}
					else
					{
						printf("# %s%sHigh true, edge sensitive interrupt (by default)\n", indep, indep);
					}
					printf("# (INT %d (IRQ %d (MODE %s)))\n", curint, firstirq, edge);
					curint++;
					if(!starteddeps)
						depint = curint;
				}
				else
				{
					printf("# %s*** ERROR *** No IRQ specified!\n", indep);
				}
				continue;
			}
		case DMA_TAG:
			{
				int dma, firstdma = 4;	/* Ie, no DMA */
				int first = 1;
				if(tmp[1])
				{
					printf("# %s%sDMA channel",
						   indep, (curdma == 0) ? "First " : "Next ");
					for(dma = 0; tmp[1]; dma++)
					{
						if(tmp[1] & 1)
						{
							printf("%s%d",
								   (tmp[1] == 1 && !first) ? " or " :
								   (!first) ? ", " : " ", dma);
							if(first)
								firstdma = dma;
							first = 0;
						}
						tmp[1] >>= 1;
					}
					printf(".\n");
				}
				else
				{
					printf("# %s*** ERROR *** No DMA channel specified!\n", indep);
				}
				if((tmp[2] & 3) == 0)
					printf("# %s%s8 bit DMA only\n", indep, indep);
				if((tmp[2] & 3) == 1)
					printf("# %s%s8 & 16 bit DMA\n", indep, indep);
				if((tmp[2] & 3) == 2)
					printf("# %s%s16 bit DMA only\n", indep, indep);
				printf("# %s%sLogical device is%s a bus master\n", indep, indep, tmp[2] & 4 ? "" : " not");
				printf("# %s%sDMA may%s execute in count by byte mode\n", indep, indep, tmp[2] & 8 ? "" : " not");
				printf("# %s%sDMA may%s execute in count by word mode\n", indep, indep, tmp[2] & 0x10 ? "" : " not");
				if((tmp[2] & 0x60) == 0x00)
					printf("# %s%sDMA channel speed in compatible mode\n", indep, indep);
				if((tmp[2] & 0x60) == 0x20)
					printf("# %s%sDMA channel speed type A\n", indep, indep);
				if((tmp[2] & 0x60) == 0x40)
					printf("# %s%sDMA channel speed type B\n", indep, indep);
				if((tmp[2] & 0x60) == 0x60)
					printf("# %s%sDMA channel speed type F\n", indep, indep);
				printf("# (DMA %d (CHANNEL %d))\n", curdma, firstdma);
				curdma++;
				if(!starteddeps)
					depdma = curdma;
				continue;
			}
		case StartDep_TAG:
			{
				putchar('\n');
				if(!starteddeps)
					printf("# Multiple choice time, choose one only !\n\n");
				if(!(tmp[0] & 0x01))
				{
					printf("# %sStart dependent functions: priority acceptable\n", indep);
				}
				else
					switch (tmp[1])
					{
					case 0:
						printf("# %sStart dependent functions: priority preferred\n", indep);
						break;
					case 1:
						printf("# %sStart dependent functions: priority acceptable\n", indep);
						break;
					case 2:
						printf("# %sStart dependent functions: priority functional\n", indep);
						break;
					default:
						printf("# %sStart dependent functions: priority INVALID\n", indep);
						break;
					}
				indep = "      ";
				starteddeps = 1;
				curio = depio;
				curdma = depdma;
				curint = depint;
				curmem = depmem;
				continue;
			}
		case EndDep_TAG:
			{
				indep = "    ";
				printf("\n# %sEnd dependent functions\n", indep);
				continue;
			}
		case IOport_TAG:
			{
				printf("# %sLogical device decodes %s IO address lines\n", indep, tmp[1] ? "16 bit" : "10 bit");
				printf("# %s%sMinimum IO base address 0x%02x%02x\n", indep, indep, tmp[3], tmp[2]);
				printf("# %s%sMaximum IO base address 0x%02x%02x\n", indep, indep, tmp[5], tmp[4]);
				printf("# %s%sIO base alignment %d bytes\n", indep, indep, tmp[6]);
				printf("# %s%sNumber of IO addresses required: %d\n", indep, indep, tmp[7]);
#if DUMPADDR
				for(i = ((tmp[3] << 8) + tmp[2]); i <= ((tmp[5] << 8) + tmp[4]); i += tmp[6])
				{
					printf("# %s%s0x%04x..0x%04x\n", indep, indep, indep, i, i + tmp[7] - 1);
				}
#endif /* DUMPADDR */
				printf("# (IO %d (BASE 0x%02x%02x))\n", curio, tmp[3], tmp[2]);
				curio++;
				if(!starteddeps)
					depio = curio;
				continue;
			}
		case FixedIO_TAG:
			{
				printf("# %sFixed IO base address 0x%02x%02x\n", indep, tmp[2], tmp[1]);
				printf("# %s%sNumber of IO addresses required: %d\n", indep, indep, tmp[3]);
				printf("# (IO %d (BASE 0x%02x%02x))\n", curio, tmp[2], tmp[1]);
				curio++;
				if(!starteddeps)
					depio = curio;
				continue;
			}
		case MemRange_TAG:
			{
				long minbase, length;
				char width = 'w';
				if(len != 9)
				{
					printf("# %sInvalid length for memory range tag\n", indep);
					continue;
				}
				printf("# %sMemory is %s\n", indep, large[0] & 1 ? "writeable" : "non-writeable (ROM)");
				printf("# %sMemory is %s\n", indep, large[0] & 2 ? "read cacheable, write-through" : "non-cacheable");
				printf("# %sMemory decode supports %s\n", indep, large[0] & 4 ? "range length" : "high address");
				if((large[0] & 0x18) == 0x00)
				{
					width = 'b';
					printf("# %smemory is 8-bit only\n", indep);
				}
				if((large[0] & 0x18) == 0x08)
					printf("# %smemory is 16-bit only\n", indep);
				if((large[0] & 0x18) == 0x10)
					printf("# %smemory is 8-bit and 16-bit\n", indep);
				if(large[0] & 0x20)
					printf("# %smemory is shadowable\n", indep);
				if(large[0] & 0x40)
					printf("# %smemory is an expansion ROM\n", indep);
				printf("# %sMinimum memory base address 0x%02x%02x00\n", indep, large[2], large[1]);
				minbase = (((long) large[2] << 8) | large[1]) * 256;
				printf("# %sMaximum memory base address 0x%02x%02x00\n", indep, large[4], large[3]);
				printf("# %sRange base alignment mask 0xff%02x%02x bytes\n", indep, large[6], large[5]);
				printf("# %sRange length %ld bytes\n", indep, length = (((long) large[8] << 8) | large[7]) * 256);
#ifdef DUMPADDR
#if 0
				***untested ***
					for(i = ((large[2] << 16) + (large[1] << 8));
						 i <= ((large[4] << 16) + (large[3] << 8));
						 i += ((large[6] << 8) + large[5]))
				{
					printf("# %s%s0x%06x..0x%06x\n", indep, indep,
						   i, i + (large[8] << 16) + (large[7] << 8) - 1);
				}
#endif
#endif /* DUMPADDR */
				printf("# Choose UPPER = Range, or UPPER = Upper limit to suit hardware\n");
				printf("# (MEM %d (BASE 0x%06lx) (MODE %cu) (UPPER 0x%06lx))\n", curmem, minbase, width, minbase + length);
				printf("# (MEM %d (BASE 0x%06lx) (MODE %cr) (UPPER 0x%06lx))\n", curmem, minbase, width, length);
				curmem++;
				if(!starteddeps)
					depmem = curmem;
				continue;
			}
		case ANSIstr_TAG:
			{
				printf("# %sANSI string -->", indep);
				for(i = 0; i < len; i++)
					putchar(large[i]);
				printf("<--\n");
				continue;
			}
		case UNICODEstr_TAG:
			{
				printf("# %sUNICODE string -->", indep);
				for(i = 0; i < len; i++)
					putchar(large[i]);
				printf("<--\n");
				continue;
			}
		case VendorShort_TAG:
		case VendorLong_TAG:
			{
				printf("# %sVendor defined tag: ", indep);
				for(i = 0; i <= len; i++)
					printf(" %02x", tmp[i]);
				putchar('\n');
				continue;
			}
		case End_TAG:
			{
				char *indep = "";
				if(nestdepth)
				{
					printf("# (ACT Y)\n");
					while(nestdepth--)
						printf(")");
					printf("\n");
				}
				printf("# %sEnd tag... Checksum 0x%02x (%s)\n\n", indep, csum, (csum % 256) ? "BAD" : "OK");
				continue;
			}
		case Mem32Range_TAG:
		case FixedMem32Range_TAG:
			{
				printf("# %s32-bit MemRange tag %02x ...\n", indep, type);
				continue;
			}
		default:
			{
				printf("# %sUnknown tag %02x ...\n", indep, type);
				continue;
			}
		}
	}while(type != End_TAG);
}

#ifdef DUMPREGS
void
dumpregs(int csn)
{
	int logical_device;
	int i;
	int addr;
	int desc;
	Wake(csn);
	printf("\nConfiguration regsisters\n\nVendor defined card level registers 0x20..2f:");
	for(addr = 0x20; addr < 0x30; addr++)
	{
		ADDRESS(addr);
		printf(" %02x", READ_DATA);
	}
	putchar('\n');
	for(logical_device = 0; logical_device < 256; logical_device++)
	{
		LOGICALDEVICENUMBER;
		WRITE_DATA(logical_device);
		if(READ_DATA != logical_device)
			break;
		printf("Logical device %d\n", logical_device);
		for(addr = 0x40, desc = 0; addr < 0x60; addr += 8, desc++)
		{
			printf("24 bit Memory descriptor %d at %02x..%02x: ", desc, addr, addr + 4);
			for(i = 0; i < 5; i++)
			{
				ADDRESS(addr + i);
				tmp[i] = READ_DATA;
			}
			printf("Base address 0x%02x%02x00 %s 0x%02x%02x00, %d bit\n", tmp[0], tmp[1],
				   tmp[2] & 1 ? "...." : "size",
				   tmp[3], tmp[4], tmp[2] & 2 ? 16 : 8);
		}
		for(addr = 0x76, desc = 0; addr < 0xb0; addr += 16, desc++)
		{
			printf("32 bit Memory descriptor %d at %02x..%02x: ", desc, addr, addr + 8);
			for(i = 0; i < 9; i++)
			{
				ADDRESS(addr + i);
				tmp[i] = READ_DATA;
			}
			printf("Base address 0x%02x%02x%02x%02x %s 0x%02x%02x%02x%02x, %d bit\n", tmp[0], tmp[1], tmp[2], tmp[3],
				   tmp[4] & 1 ? "...." : "size",
				   tmp[5], tmp[6], tmp[7], tmp[8], tmp[8] & 4 ? 32 : (tmp[8] & 2 ? 16 : 8));
			addr &= 0xf0;
		}
		for(addr = 0x60, desc = 0; addr < 0x70; addr += 2, desc++)
		{
			printf("IO descriptor %d at %02x..%02x: ", desc, addr, addr + 1);
			for(i = 0; i < 2; i++)
			{
				ADDRESS(addr + i);
				tmp[i] = READ_DATA;
			}
			printf("Base address 0x%02x%02x\n", tmp[0], tmp[1]);
		}
		for(addr = 0x70, desc = 0; addr < 0x74; addr += 2, desc++)
		{
			printf("Interrupt descriptor %d at %02x..%02x: ", desc, addr, addr + 1);
			for(i = 0; i < 2; i++)
			{
				ADDRESS(addr + i);
				tmp[i] = READ_DATA;
			}
			printf("Interrupt level %d, active %s, %s triggered\n", tmp[0], tmp[1] & 2 ? "high" : "low", tmp[1] & 1 ? "level" : "edge");
		}
		for(addr = 0x74; addr < 0x76; addr++)
		{
			printf("DMA descriptor %d at %02x: ", addr - 0x74, addr);
			ADDRESS(addr);
			tmp[0] = READ_DATA;
			printf("DMA channel %d\n", tmp[0]);
		}
	}
}

#endif /* DUMPREGS */
