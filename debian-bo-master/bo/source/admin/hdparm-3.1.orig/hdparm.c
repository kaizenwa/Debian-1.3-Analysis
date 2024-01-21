/* hdparm.c - Command line interface to get/set hard disk parameters */
/*          - by Mark S. Lord (c) 1994-1996 -- freely distributable */

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <linux/hdreg.h>
#include <linux/fs.h>
#include <linux/major.h>

/*
 * For kernels prior to 1.3.61, HDIO_SET_32BIT is HDIO_SET_CHIPSET
 */
#ifndef HDIO_SET_32BIT
#ifdef HDIO_SET_CHIPSET
#define	HDIO_SET_32BIT HDIO_SET_CHIPSET
#define	HDIO_GET_32BIT HDIO_GET_CHIPSET
#endif
#endif

#define TIMING_MB		16
#define TIMING_BUF_MB		1
#define TIMING_BUF_BYTES	(TIMING_BUF_MB * 1024 * 1024)
#define TIMING_BUF_COUNT	(TIMING_MB / TIMING_BUF_MB)
#define BUFCACHE_FACTOR		2

char *progname;
static struct hd_driveid id;
static int verbose = 0, get_identity = 0, get_geom = 0, noisy = 1, quiet = 0;
static int flagcount = 0, do_flush = 0, is_scsi_hd = 0;;
static int do_ctimings, do_timings = 0;

static unsigned long set_readahead= 0, get_readahead= 0, readahead= 0;     
static unsigned long set_readonly = 0, get_readonly = 0, readonly = 0;     
static unsigned long set_unmask   = 0, get_unmask   = 0, unmask   = 0;     
static unsigned long set_mult     = 0, get_mult     = 0, mult     = 0;     
#ifdef HDIO_SET_DMA
static unsigned long set_dma      = 0, get_dma      = 0, dma      = 0;     
#endif
#ifdef HDIO_SET_NOWERR
static unsigned long set_nowerr   = 0, get_nowerr   = 0, nowerr   = 0;
#endif
#ifdef HDIO_GET_KEEPSETTINGS
static unsigned long set_keep     = 0, get_keep     = 0, keep     = 0;
#endif
#ifdef HDIO_SET_32BIT
static unsigned long set_io32bit  = 0, get_io32bit  = 0, io32bit  = 0;
#endif
#ifdef HDIO_SET_PIO_MODE
static unsigned long set_piomode  = 0, noisy_piomode= 0, piomode  = 0;
#endif
#ifdef HDIO_DRIVE_CMD
static unsigned long set_dkeep    = 0, get_dkeep    = 0, dkeep    = 0;
static unsigned long set_standby  = 0, get_standby  = 0, standby  = 0;
static unsigned long set_xfermode = 0, get_xfermode = 0, xfermode = 0;
static unsigned long set_lookahead= 0, get_lookahead= 0, lookahead= 0;
static unsigned long set_prefetch = 0, get_prefetch = 0, prefetch = 0;
static unsigned long set_wcache   = 0, get_wcache   = 0, wcache   = 0;
static unsigned long set_seagate  = 0, get_seagate  = 0;
#endif
#ifdef HDIO_SET_32BIT
static int get_IDentity = 0;
#endif

char *cfg_str[] =
{	"", " HardSect", " SoftSect", " NotMFM", " HdSw>15uSec", " SpinMotCtl",
	" Fixed", " Removeable", " DTR<=5Mbs", " DTR>5Mbs", " DTR>10Mbs",
	" RotSpdTol>.5%", " dStbOff", " TrkOff", " FmtGapReq", " nonMagnetic"
};

char *SlowMedFast[]	= {"slow", "medium", "fast", "eide"};
char *BuffType[]	= {"?", "1Sect", "DualPort", "DualPortCache"};

#define YN(b)	(((b)==0)?"no":"yes")

static void dmpstr (char *prefix, unsigned int i, char *s[], unsigned int maxi)
{
	printf("%s%d(%s)", prefix, i, (i > maxi) ? "?" : s[i]);
}

static void dump_identity (struct hd_driveid *id)
{
	int i;

	printf("\n Model=%.40s, FwRev=%.8s, SerialNo=%.8s", 
		id->model, id->fw_rev, id->serial_no);
	printf("\n Config={");
	for (i=0; i<=15; i++) {
		if (id->config & (1<<i))
			printf("%s", cfg_str[i]);
	}
	printf(" }\n");
	printf(" RawCHS=%d/%d/%d, TrkSize=%d, SectSize=%d, ECCbytes=%d\n",
		id->cyls, id->heads, id->sectors,
		id->track_bytes, id->sector_bytes, id->ecc_bytes);
	dmpstr (" BuffType=",id->buf_type,BuffType,3);
	printf(", BuffSize=%dkB, MaxMultSect=%d", id->buf_size/2, id->max_multsect);
	if (id->max_multsect) {
		printf(", MultSect=");
		if (!(id->multsect_valid&1))
			printf("?%d?", id->multsect);
		else if (id->multsect)
			printf("%d", id->multsect);
		else
			printf("off");
	}
	printf("\n DblWordIO=%s", YN(id->dword_io&1));
	dmpstr (", maxPIO=",id->tPIO,SlowMedFast,3);
	printf(", DMA=%s", YN(id->capability&1));
	if (id->capability&1)
		dmpstr (", maxDMA=",id->tDMA,SlowMedFast,2);
	putchar('\n');
	if (!(id->field_valid&1))
		printf(" (maybe):");
	printf(" CurCHS=%d/%d/%d, CurSects=%d",
		id->cur_cyls, id->cur_heads, id->cur_sectors, *(int *)&id->cur_capacity0);
	printf(", LBA=%s", YN(id->capability&2));
	if (id->capability&2)
		printf(", LBAsects=%d", id->lba_capacity);
	if (id->capability&1) {
		if ((id->field_valid&2) || (id->dma_1word | id->dma_mword))
			printf("\n ");
		if (id->field_valid&2) {
			printf("tDMA={min:%d,rec:%d}",
			 id->eide_dma_min, id->eide_dma_time);
		}
		if (id->dma_1word | id->dma_mword) {
			if (id->field_valid&2)
				printf(", ");
			printf("DMA modes: ");
			if (id->dma_1word & 0x100)	printf("*");
			if (id->dma_1word & 1)		printf("sword0 ");
			if (id->dma_1word & 0x200)	printf("*");
			if (id->dma_1word & 2)		printf("sword1 ");
			if (id->dma_1word & 0x400)	printf("*");
			if (id->dma_1word & 4)		printf("sword2 ");
			if (id->dma_1word & 0xf800)	printf("*");
			if (id->dma_1word & 0xf8)	printf("sword? ");
			if (id->dma_mword & 0x100)	printf("*");
			if (id->dma_mword & 1)		printf("mword0 ");
			if (id->dma_mword & 0x200)	printf("*");
			if (id->dma_mword & 2)		printf("mword1 ");
			if (id->dma_mword & 0x400)	printf("*");
			if (id->dma_mword & 4)		printf("mword2 ");
			if (id->dma_mword & 0xf800)	printf("*");
			if (id->dma_mword & 0xf8)	printf("mword? ");
		}
	}
	if ((id->capability&8) || (id->field_valid&2)) {
		printf("\n IORDY=");
		if (id->capability&8)
			printf((id->capability&4) ? "on/off" : "yes");
		else
			printf("no");
		if (id->field_valid&2) {
			printf(", tPIO={min:%d,w/IORDY:%d}, PIO modes: ",
			 id->eide_pio, id->eide_pio_iordy);
			if (id->eide_pio_modes & 1) printf("mode3 ");
			if (id->eide_pio_modes & 2) printf("mode4 ");
			if (id->eide_pio_modes &~3) printf("mode? ");
		}
	}
	printf("\n\n");
}

void flush_buffer_cache (int fd)
{
	fsync (fd);				/* flush buffers */
	if (ioctl(fd, BLKFLSBUF, NULL))		/* do it again, big time */
		perror("BLKFLSBUF failed");
#ifdef HDIO_DRIVE_CMD
	if (is_scsi_hd) {
		sleep(1);
	} else {
		if (ioctl(fd, HDIO_DRIVE_CMD, NULL))	/* await completion */
			perror("HDIO_DRIVE_CMD failed");
	}
#endif
}

int seek_to_zero (int fd)
{
	if (lseek(fd, (off_t) 0, SEEK_SET)) {
		perror("lseek() failed");
		return 1;
	}
	return 0;
}

int read_big_block (int fd, char *buf)
{
	int i, rc;
	if ((rc = read(fd, buf, TIMING_BUF_BYTES)) != TIMING_BUF_BYTES) {
		if (rc)
			perror("read() failed");
		else
			fputs ("read() hit EOF - device too small\n", stderr);
		return 1;
	}
	/* access all sectors of buf to ensure the read fully completed */
	for (i = 0; i < TIMING_BUF_BYTES; i += 512)
		buf[i] &= 1;
	return 0;
}

static double correction = 0.0;

void time_cache (int fd)
{
	int i;
	char *buf;
	struct itimerval e1, e2;
	int shmid;

	if ((shmid = shmget(IPC_PRIVATE, TIMING_BUF_BYTES, 0600)) == -1) {
		perror ("could not allocate sharedmem buf");
		return;
	}
	if (shmctl(shmid, SHM_LOCK, NULL) == -1) {
		perror ("could not lock sharedmem buf");
		(void) shmctl(shmid, IPC_RMID, NULL);
		return;
	}
	if ((buf = shmat(shmid, (char *) 0, 0)) == (char *) -1) {
		perror ("could not attach sharedmem buf");
		(void) shmctl(shmid, IPC_RMID, NULL);
		return;
	}
	if (shmctl(shmid, IPC_RMID, NULL) == -1)
		perror ("shmctl(,IPC_RMID,) failed");

	/* Clear out the device request queues & give them time to complete */
	sync();
	sleep(3);

	/* Calculate a correction factor for the basic
	 * overhead of doing a read() from the buffer cache.
	 * To do this, we read the data once to "cache it" and
	 * to force full preallocation of our timing buffer,
	 * and then we re-read it 10 times while timing it.
	 *
	 * getitimer() is used rather than gettimeofday() because
	 * it is much more consistent (on my machine, at least).
	 */
	setitimer(ITIMER_REAL, &(struct itimerval){{1000,0},{1000,0}}, NULL);
	if (seek_to_zero (fd)) return;
	if (read_big_block (fd, buf)) return;
	printf(" Timing buffer-cache reads:   ");
	fflush(stdout);

	/* Clear out the device request queues & give them time to complete */
	sync();
	sleep(1);

	/* Time re-reading from the buffer-cache */
	getitimer(ITIMER_REAL, &e1);
	for (i = (BUFCACHE_FACTOR * TIMING_BUF_COUNT) ; i > 0; --i) {
		if (seek_to_zero (fd)) goto quit;
		if (read_big_block (fd, buf)) goto quit;
	}
	getitimer(ITIMER_REAL, &e2);
	correction = (e1.it_value.tv_sec - e2.it_value.tv_sec)
	 + ((e1.it_value.tv_usec - e2.it_value.tv_usec) / 1000000.0);

	/* Now remove the lseek() from the correction factor */
	getitimer(ITIMER_REAL, &e1);
	for (i = (BUFCACHE_FACTOR * TIMING_BUF_COUNT) ; i > 0; --i) {
		if (seek_to_zero (fd)) goto quit;
	}
	getitimer(ITIMER_REAL, &e2);
	correction -= (e1.it_value.tv_sec - e2.it_value.tv_sec)
	 + ((e1.it_value.tv_usec - e2.it_value.tv_usec) / 1000000.0);

	printf("%2d MB in %5.2f seconds =%5.2f MB/sec\n",
		(BUFCACHE_FACTOR * TIMING_MB), correction,
		(BUFCACHE_FACTOR * TIMING_MB) / correction);
	correction /= BUFCACHE_FACTOR;

	flush_buffer_cache(fd);
	sleep(1);
quit:
	if (-1 == shmdt(buf))
		perror ("could not detach sharedmem buf");
}

void time_device (int fd)
{
	int i;
	char *buf;
	double elapsed;
	struct itimerval e1, e2;
	int shmid;

	if ((shmid = shmget(IPC_PRIVATE, TIMING_BUF_BYTES, 0600)) == -1) {
		perror ("could not allocate sharedmem buf");
		return;
	}
	if (shmctl(shmid, SHM_LOCK, NULL) == -1) {
		perror ("could not lock sharedmem buf");
		(void) shmctl(shmid, IPC_RMID, NULL);
		return;
	}
	if ((buf = shmat(shmid, (char *) 0, 0)) == (char *) -1) {
		perror ("could not attach sharedmem buf");
		(void) shmctl(shmid, IPC_RMID, NULL);
		return;
	}
	if (shmctl(shmid, IPC_RMID, NULL) == -1)
		perror ("shmctl(,IPC_RMID,) failed");

	/* Clear out the device request queues & give them time to complete */
	sync();
	sleep(3);

	printf(" Timing buffered disk reads:  ");
	fflush(stdout);

	/*
	 * getitimer() is used rather than gettimeofday() because
	 * it is much more consistent (on my machine, at least).
	 */
	setitimer(ITIMER_REAL, &(struct itimerval){{1000,0},{1000,0}}, NULL);

	/* Now do the timings for real */
	getitimer(ITIMER_REAL, &e1);
	for (i = TIMING_BUF_COUNT; i > 0; --i) {
		if (read_big_block (fd, buf)) goto quit;
	}
	getitimer(ITIMER_REAL, &e2);

	elapsed = (e1.it_value.tv_sec - e2.it_value.tv_sec)
	 + ((e1.it_value.tv_usec - e2.it_value.tv_usec) / 1000000.0);
	printf("%2d MB in %5.2f seconds =%5.2f MB/sec\n",
		TIMING_MB, elapsed, TIMING_MB / elapsed);

	if (elapsed <= (correction * 2))
		printf("Hmm.. suspicious results: probably not enough free memory for a proper test.\n");
#if 0  /* the "estimate" is just plain wrong for many systems.. */
	else if (correction != 0.0) {
		printf(" Estimating raw driver speed: ");
		elapsed -= correction;
		printf("%2d MB in %5.2f seconds =%5.2f MB/sec\n",
			TIMING_MB, elapsed, TIMING_MB / elapsed);
	}
#endif
quit:
	if (-1 == shmdt(buf))
		perror ("could not detach sharedmem buf");
}

void no_scsi (void)
{
	if (is_scsi_hd) {
		fputs (" operation not supported on SCSI disks\n", stderr);
		exit(EINVAL);
	}
}

static void on_off (unsigned int value)
{
	printf(value ? " (on)\n" : " (off)\n");
}

#ifdef HDIO_DRIVE_CMD
static void interpret_standby (unsigned int standby)
{
	printf(" (");
	switch(standby) {
		case 0:		printf("off");
				break;
		case 252:	printf("21 minutes");
				break;
		case 253:	printf("vendor-specific");
				break;
		case 254:	printf("?reserved");
				break;
		case 255:	printf("21 minutes + 15 seconds");
				break;
		default:
			if (standby <= 240) {
				unsigned int secs = standby * 5;
				unsigned int mins = secs / 60;
				secs %= 60;
				if (mins)	  printf("%d minutes", mins);
				if (mins && secs) printf(" + ");
				if (secs)	  printf("%d seconds", secs);
			} else if (standby <= 251) {
				unsigned int mins = (standby - 240) * 30;
				unsigned int hrs  = mins / 60;
				mins %= 60;
				if (hrs)	  printf("%d hours", hrs);
				if (hrs && mins)  printf(" + ");
				if (mins)	  printf("%d minutes", mins);
			} else
				printf("illegal value)\n");
			break;
	}
	printf(")\n");
}

static void interpret_xfermode (unsigned int xfermode)
{
	printf(" (");
	switch(xfermode) {
		case 0:		printf("default PIO mode");
				break;
		case 1:		printf("default PIO mode, disable IORDY");
				break;
		case 8:
		case 9:
		case 10:
		case 11:
		case 12:
		case 13:
		case 14:
		case 15:	printf("PIO flow control mode%d", xfermode-8);
				break;
		case 16:
		case 17:
		case 18:
		case 19:
		case 20:
		case 21:
		case 22:
		case 23:	printf("singleword DMA mode%d", xfermode-16);
				break;
		case 32:
		case 33:
		case 34:
		case 35:
		case 36:
		case 37:
		case 38:
		case 39:	printf("multiword DMA mode%d", xfermode-32);
				break;
		default:
			if ((xfermode & 0x78) == 64)
				printf("reserved mode%d", xfermode-64);
			else
				printf("unknown, probably not valid");
			break;
	}
	printf(")\n");
}
#endif /* HDIO_DRIVE_CMD */

void process_dev (char *devname)
{
	int fd;
	static long parm, multcount;
	struct stat stat_buf;
#ifndef HDIO_DRIVE_CMD
	int force_operation = 0;
#endif
	if (stat(devname,&stat_buf)) {
		perror(devname);
		exit(errno);
	}

	if ((major(stat_buf.st_rdev) == SCSI_DISK_MAJOR) 
#ifdef MD_MAJOR
		|| (major(stat_buf.st_rdev) == MD_MAJOR)
#endif
							) 
		is_scsi_hd = 1;
	else
		if (major(stat_buf.st_rdev) != IDE0_MAJOR
		 && major(stat_buf.st_rdev) != IDE1_MAJOR
#ifdef IDE2_MAJOR
		 && major(stat_buf.st_rdev) != IDE2_MAJOR
#endif
#ifdef IDE3_MAJOR
		 && major(stat_buf.st_rdev) != IDE3_MAJOR
#endif
	                                                      ) {
		fprintf(stderr,"%s is not a hard disk.\n",devname);
		exit(EINVAL);
	}

	fd = open (devname, O_RDONLY);
	if (fd < 0) {
		perror(devname);
		exit(errno);
	}

	if (!quiet)
		printf("\n%s:\n", devname);
	if (set_readahead) {
		if (get_readahead)
			printf(" setting fs readahead to %ld\n", readahead);
		if (ioctl(fd, BLKRASET, readahead)) 
			perror(" BLKRASET failed");
	}
#ifdef HDIO_SET_PIO_MODE
	if (set_piomode) {
		no_scsi();
		if (noisy_piomode) {
			if (piomode == 255)
				printf(" attempting to auto-tune PIO mode\n");
			else
				printf(" attempting to set PIO mode to %ld\n", piomode);
		}
		if (ioctl(fd, HDIO_SET_PIO_MODE, piomode)) 
			perror(" HDIO_SET_PIO_MODE failed");
	}
#endif
#ifdef HDIO_SET_32BIT
	if (set_io32bit) {
		no_scsi();
		if (get_io32bit)
			printf(" setting 32-bit I/O support flag to %ld\n", io32bit);
		if (ioctl(fd, HDIO_SET_32BIT, io32bit)) 
			perror(" HDIO_SET_32BIT failed");
	}
#endif
	if (set_mult) {
		no_scsi();
		if (get_mult)
			printf(" setting multcount to %ld\n", mult);
		if (ioctl(fd, HDIO_SET_MULTCOUNT, mult)) 
			perror(" HDIO_SET_MULTCOUNT failed");
#ifndef HDIO_DRIVE_CMD
		else force_operation = 1;
#endif
	}
	if (set_readonly) {
		if (get_readonly) {
			printf(" setting readonly to %ld", readonly);
			on_off(readonly);
		}
		if (ioctl(fd, BLKROSET, &readonly))
			perror(" BLKROSET failed");
	}
	if (set_unmask) {
		no_scsi();
		if (get_unmask) {
			printf(" setting unmaskirq to %ld", unmask);
			on_off(unmask);
		}
		if (ioctl(fd, HDIO_SET_UNMASKINTR, unmask))
			perror(" HDIO_SET_UNMASKINTR failed");
	}
#ifdef HDIO_SET_DMA
	if (set_dma) {
		no_scsi();
		if (get_dma) {
			printf(" setting using_dma to %ld", dma);
			on_off(dma);
		}
		if (ioctl(fd, HDIO_SET_DMA, dma))
			perror(" HDIO_SET_DMA failed");
	}
#endif
#ifdef HDIO_SET_NOWERR
	if (set_nowerr) {
		no_scsi();
		if (get_nowerr) {
			printf(" setting nowerr to %ld", nowerr);
			on_off(nowerr);
		}
		if (ioctl(fd, HDIO_SET_NOWERR, nowerr))
			perror(" HDIO_SET_NOWERR failed");
	}
#endif
#ifdef HDIO_GET_KEEPSETTINGS
	if (set_keep) {
		no_scsi();
		if (get_keep) {
			printf(" setting keep_settings to %ld", keep);
			on_off(keep);
		}
		if (ioctl(fd, HDIO_SET_KEEPSETTINGS, keep))
			perror(" HDIO_SET_KEEPSETTINGS failed");
	}
#endif /* HDIO_GET_KEEPSETTINGS */
#ifdef HDIO_DRIVE_CMD
	if (set_dkeep) {
		/* lock/unlock the drive's "feature" settings */
		unsigned char args[4] = {WIN_SETFEATURES,0,0,0};
		no_scsi();
		if (get_dkeep) {
			printf(" setting drive keep features to %ld", dkeep);
			on_off(dkeep);
		}
		args[2] = dkeep ? 0x66 : 0xcc;
		if (ioctl(fd, HDIO_DRIVE_CMD, &args))
			perror(" HDIO_DRIVE_CMD failed");
	}
	if (set_prefetch) {
		unsigned char args[4] = {WIN_SETFEATURES,0,0xab,0};
		no_scsi();
		args[1] = prefetch;
		if (get_prefetch)
			printf(" setting drive prefetch to %ld\n", prefetch);
		if (ioctl(fd, HDIO_DRIVE_CMD, &args))
			perror(" HDIO_DRIVE_CMD failed");
	}
	if (set_xfermode) {
		unsigned char args[4] = {WIN_SETFEATURES,0,3,0};
		no_scsi();
		args[1] = xfermode;
		if (get_xfermode) {
			printf(" setting xfermode to %ld", xfermode);
			interpret_xfermode(xfermode);
		}
		if (ioctl(fd, HDIO_DRIVE_CMD, &args))
			perror(" HDIO_DRIVE_CMD failed");
	}
	if (set_lookahead) {
		unsigned char args[4] = {WIN_SETFEATURES,0,0,0};
		no_scsi();
		args[2] = lookahead ? 0xaa : 0x55;
		if (get_lookahead) {
			printf(" setting drive read-lookahead to %ld", lookahead);
			on_off(lookahead);
		}
		if (ioctl(fd, HDIO_DRIVE_CMD, &args))
			perror(" HDIO_DRIVE_CMD failed");
	}
	if (set_wcache) {
		unsigned char args[4] = {WIN_SETFEATURES,0,0,0};
		no_scsi();
		args[2] = wcache ? 0x02 : 0x82;
		if (get_wcache) {
			printf(" setting drive write-caching to %ld", wcache);
			on_off(wcache);
		}
		if (ioctl(fd, HDIO_DRIVE_CMD, &args))
			perror(" HDIO_DRIVE_CMD failed");
	}
	if (set_seagate) {
		unsigned char args[4] = {0xfb,0,0,0};
		no_scsi();
		if (get_seagate)
			printf(" disabling Seagate auto powersaving mode\n");
		if (ioctl(fd, HDIO_DRIVE_CMD, &args))
			perror(" HDIO_DRIVE_CMD failed");
	}
	if (set_standby) {
		unsigned char args[4] = {WIN_SETIDLE1,standby,0,0};
		no_scsi();
		if (get_standby) {
			printf(" setting standby to %ld", standby);
			interpret_standby(standby);
		}
		if (ioctl(fd, HDIO_DRIVE_CMD, &args))
			perror(" HDIO_DRIVE_CMD failed");
	}
#else	/* HDIO_DRIVE_CMD */
	if (force_operation) {
		char buf[512];
		flush_buffer_cache(fd);
		if (-1 == read(fd, buf, sizeof(buf)))
			perror(" access failed");
	}
#endif	/* HDIO_DRIVE_CMD */

	if (!flagcount)
		verbose = 1;

	if ((verbose && !is_scsi_hd) || get_mult || get_identity) {
		no_scsi();
		multcount = -1;
		if (ioctl(fd, HDIO_GET_MULTCOUNT, &multcount))
			perror(" HDIO_GET_MULTCOUNT failed");
		else if (verbose | get_mult) {
			printf(" multcount    = %2ld", multcount);
			on_off(multcount);
		}
	}
#ifdef HDIO_GET_32BIT
	if ((verbose && !is_scsi_hd) || get_io32bit) {
		no_scsi();
		if (ioctl(fd, HDIO_GET_32BIT, &parm))
			perror(" HDIO_GET_32BIT failed");
		else {
			printf(" I/O support  =%3ld (", parm);
			switch (parm) {
				case 0:	printf("default ");
				case 2: printf("16-bit)\n");
					break;
				case 1:	printf("32-bit)\n");
					break; 
				case 3:	printf("32-bit w/sync)\n");
					break; 
				default:printf("???)\n");
			}
		}
	}
#endif
	if ((verbose && !is_scsi_hd) || get_unmask) {
		no_scsi();
		if (ioctl(fd, HDIO_GET_UNMASKINTR, &parm))
			perror(" HDIO_GET_UNMASKINTR failed");
		else {
			printf(" unmaskirq    = %2ld", parm);
			on_off(parm);
		}
	}
#ifdef HDIO_GET_DMA
	if ((verbose && !is_scsi_hd) || get_dma) {
		no_scsi();
		if (ioctl(fd, HDIO_GET_DMA, &parm))
			perror(" HDIO_GET_DMA failed");
		else {
			printf(" using_dma    = %2ld", parm);
			on_off(parm);
		}
	}
#endif
#ifdef HDIO_GET_KEEPSETTINGS
	if ((verbose && !is_scsi_hd) || get_keep) {
		no_scsi();
		if (ioctl(fd, HDIO_GET_KEEPSETTINGS, &parm))
			perror(" HDIO_GET_KEEPSETTINGS failed");
		else {
			printf(" keepsettings = %2ld", parm);
			on_off(parm);
		}
	}
#endif /* HDIO_GET_KEEPSETTINGS */

#ifdef HDIO_SET_NOWERR
	if (verbose || get_nowerr) {
		no_scsi();
		if (ioctl(fd, HDIO_GET_NOWERR, &parm))
			perror(" HDIO_GET_NOWERR failed");
		else {
			printf(" nowerr       = %2ld", parm);
			on_off(parm);
		}
	}
#endif
	if (verbose || get_readonly) {
		if (ioctl(fd, BLKROGET, &parm))
			perror(" BLKROGET failed");
		else {
			printf(" readonly     = %2ld", parm);
			on_off(parm);
		}
	}
	if ((verbose && !is_scsi_hd) || get_readahead) {
		if (ioctl(fd, BLKRAGET, &parm))
			perror(" BLKRAGET failed");
		else {
			printf(" readahead    = %2ld", parm);
			on_off(parm);
		}
	}
	if (verbose || get_geom) {
		static struct hd_geometry g;
		if (ioctl(fd, BLKGETSIZE, &parm))
			perror(" BLKGETSIZE failed");
		else if (ioctl(fd, HDIO_GETGEO, &g))
			perror(" HDIO_GETGEO failed");
		else	printf(" geometry     = %d/%d/%d, sectors = %ld, start = %ld\n",
				g.cylinders, g.heads, g.sectors, parm, g.start);
	}
	if (get_identity) {
		no_scsi();
		if (!(ioctl(fd, HDIO_GET_IDENTITY, &id))) {
			if (multcount != -1) {
				id.multsect = multcount;
				id.multsect_valid |= 1;
			} else
				id.multsect_valid &= ~1;
			dump_identity(&id);
		} else if (errno == -ENOMSG)
			printf(" no identification info available\n");
		else
			perror(" HDIO_GET_IDENTITY failed");
	}
#ifdef HDIO_SET_32BIT
	if (get_IDentity) {
		unsigned char args[4+512] = {WIN_IDENTIFY,0,0,1,};
		no_scsi();
		if (!(ioctl(fd, HDIO_DRIVE_CMD, &args)))
			dump_identity((struct hd_driveid *)&args[4]);
		else
			perror(" HDIO_DRIVE_CMD failed");
	}
#endif
	if (do_ctimings)
		time_cache (fd);
	if (do_timings)
		time_device (fd);
	if (do_flush)
		flush_buffer_cache (fd);
	close (fd);
}

void usage_error (void)
{
	fprintf(stderr,"\n%s - get/set hard disk parameters - version 3.1\n\n", progname);
	fprintf(stderr,"Usage:  %s  [options] [device] ..\n\n", progname);
	fprintf(stderr,"Options:\n"
	" -a    get/set fs readahead\n"
#ifdef HDIO_DRIVE_CMD
	" -A  * set drive read-lookahead flag (0/1)\n"
#endif
#ifdef HDIO_SET_32BIT
	" -c  * get/set IDE 32-bit IO setting\n"
#endif
#ifdef HDIO_SET_DMA
	" -d  * get/set using_dma flag\n"
#endif
	" -f    flush buffer cache for device on exit\n"
	" -g    display drive geometry\n"
	" -h    display terse usage information\n"
	" -i  * display drive identification\n"
#ifdef HDIO_SET_32BIT
	" -I  * read drive identification directly from drive\n"
#endif
#ifdef HDIO_GET_KEEPSETTINGS
	" -k  * get/set keep_settings_over_reset flag (0/1)\n"
#endif
#ifdef HDIO_DRIVE_CMD
	" -K  * set drive keep_features_over_reset flag (0/1)\n"
#endif
	" -m  * get/set multiple sector count\n"
#ifdef HDIO_SET_NOWERR 
	" -n  * get/set ignore-write-errors flag (0/1)\n"
#endif
#ifdef HDIO_SET_PIO_MODE
	" -p  * set PIO mode on IDE interface chipset (0,1,2,3,4,5)\n"
#endif
#ifdef HDIO_DRIVE_CMD
	" -P  * set drive prefetch count\n"
#endif
	" -q    change next setting quietly\n"
	" -r    get/set readonly flag (DANGEROUS to set)\n"
#ifdef HDIO_DRIVE_CMD
	" -S  * set standby (spindown) timeout\n"
#endif
	" -t    perform device read timings\n"
	" -T    perform cache read timings\n"
	" -u  * get/set unmaskirq flag (0/1) (DANGEROUS)\n"
	" -v    default; same as -acdgkmnru (-gr for SCSI)\n"
#ifdef HDIO_DRIVE_CMD
	" -W  * set drive write-caching flag (0/1) (DANGEROUS)\n"
	" -X  * set IDE xfer mode (DANGEROUS)\n"
	" -Z  * disable Seagate auto-powersaving mode\n"
#endif
	"        * = (E)IDE drives only\n\n");
	exit(1);
}

#define GET_NUMBER(flag,num)	num = 0; \
				if (!*p && argc && isdigit(**argv)) \
					p = *argv++, --argc; \
				while (isdigit(*p)) { \
					flag = 1; \
					num = (num * 10) + (*p++ - '0'); \
				}

int main(int argc, char **argv)
{
	char c, *p;

	if  ((progname = (char *) strrchr(*argv, '/')) == NULL)
                progname = *argv;
        else
                progname++;
	++argv;

	if (!--argc)
		usage_error();
	while (argc--) {
		p = *argv++;
		if (*p == '-') {
			if (!*++p)
				usage_error();
			while ((c = *p++)) {
				++flagcount;
				switch (c) {
					case 'v':
						verbose = 1;
						break;
#ifdef HDIO_SET_32BIT
					case 'I':
						get_IDentity = 1;
						break;
#endif
					case 'i':
						get_identity = 1;
						break;
					case 'g':
						get_geom = 1;
						break;
					case 'f':
						do_flush = 1;
						break;
					case 'q':
						quiet = 1;
						noisy = 0;
						break;
					case 'u':
						get_unmask = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p == '0' || *p == '1') {
							set_unmask = 1;
							unmask = *p++ - '0';
						}
						break;
#ifdef HDIO_SET_DMA
					case 'd':
						get_dma = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p == '0' || *p == '1') {
							set_dma = 1;
							dma = *p++ - '0';
						}
						break;
#endif
#ifdef HDIO_SET_NOWERR
					case 'n':
						get_nowerr = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p == '0' || *p == '1') {
							set_nowerr = 1;
							nowerr = *p++ - '0';
						}
						break;
#endif
#ifdef HDIO_SET_PIO_MODE
					case 'p':
						noisy_piomode = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p >= '0' && *p <= '9')
							piomode = *p++ - '0';
						else
							piomode = 255;  /* auto-tune */
						set_piomode = 1;
						break;
#endif
					case 'r':
						get_readonly = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p == '0' || *p == '1') {
							set_readonly = 1;
							readonly = *p++ - '0';
						}
						break;
					case 'm':
						get_mult = noisy;
						noisy = 1;
						GET_NUMBER(set_mult,mult);
						break;
#ifdef HDIO_SET_32BIT
					case 'c':
						get_io32bit = noisy;
						noisy = 1;
						GET_NUMBER(set_io32bit,io32bit);
						break;
#endif /* HDIO_SET_32BIT */
#ifdef HDIO_DRIVE_CMD
					case 'S':
						get_standby = noisy;
						noisy = 1;
						GET_NUMBER(set_standby,standby);
						if (!set_standby)
							fprintf(stderr, "-S: missing value\n");
						break;

					case 'P':
						get_prefetch = noisy;
						noisy = 1;
						GET_NUMBER(set_prefetch,prefetch);
						if (!set_prefetch)
							fprintf(stderr, "-P: missing value\n");
						break;

					case 'X':
						get_xfermode = noisy;
						noisy = 1;
						GET_NUMBER(set_xfermode,xfermode);
						if (!set_xfermode)
							fprintf(stderr, "-X: missing value\n");
						break;

					case 'K':
						get_dkeep = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p == '0' || *p == '1') {
							set_dkeep = 1;
							dkeep = *p++ - '0';
						} else
							fprintf(stderr, "-K: missing value (0/1)\n");
						break;

					case 'A':
						get_lookahead = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p == '0' || *p == '1') {
							set_lookahead = 1;
							lookahead = *p++ - '0';
						} else
							fprintf(stderr, "-A: missing value (0/1)\n");
						break;

					case 'W':
						get_wcache = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p == '0' || *p == '1') {
							set_wcache = 1;
							wcache = *p++ - '0';
						} else
							fprintf(stderr, "-W: missing value (0/1)\n");
						break;

					case 'Z':
						get_seagate = noisy;
						noisy = 1;
						set_seagate = 1;
						break;
#endif /* HDIO_DRIVE_CMD */
#ifdef HDIO_GET_KEEPSETTINGS
					case 'k':
						get_keep = noisy;
						noisy = 1;
						if (!*p && argc && isdigit(**argv))
							p = *argv++, --argc;
						if (*p == '0' || *p == '1') {
							set_keep = 1;
							keep = *p++ - '0';
						}
						break;
#endif /* HDIO_GET_KEEPSETTINGS */
					case 'a':
						get_readahead = noisy;
						noisy = 1;
						GET_NUMBER(set_readahead,readahead);
						break;
					case 't':
						do_timings = 1;
						break;
					case 'T':
						do_ctimings = 1;
						break;
					case 'h':
					default:
						usage_error();
				}
			}
			if (!argc)
				usage_error();
		} else {
			process_dev (p);
		} 
	}
	exit (0);
}
