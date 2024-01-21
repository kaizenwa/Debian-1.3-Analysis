/***
 * CopyPolicy: GNU Public License 2 applies
 * Copyright (C) by Heiko Eissfeldt heiko@colossus.escape.de
 *
 * Interface module for cdrom drive access
 *
 * Two interfaces are possible.
 *
 * 1. using 'cooked' ioctls()
 *    disadvantages: available for atapi, sbpcd and cdu31a drives only.
 *
 * 2. using the generic scsi device (for details see SCSI Prog. HOWTO).
 *    NOTE: a bug/misfeature in the kernel requires blocking signal
 *          SIGINT during SCSI command handling. Once this flaw has
 *          been removed, the sigprocmask SIG_BLOCK and SIG_UNBLOCK calls
 *          should removed, thus saving context switches.
 *
 * Version 0.8: used experiences of Jochen Karrer.
 *              SparcLinux port fixes
 *              AlphaLinux port fixes
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>

#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>

#include <linux/cdrom.h>
#include <linux/major.h>
#include <linux/sbpcd.h>
#include <linux/version.h>
/* some include file locations have changed with newer kernels */
#if LINUX_VERSION_CODE > 0x10300 + 97
#include <scsi/sg.h>
#include <scsi/scsi.h>
#else /* old stuff */
#include <linux/../../drivers/scsi/sg.h>
#include <linux/../../drivers/scsi/scsi.h>
#endif
/* HACK! the kernel header files were misdesigned for
 *  user applications.
 * #include <linux/blkdev.h> did not work
 */
struct request {		/* this is and may not used */
  int dummy;
};

#include "cdda2wav.h"
#include "uti.h"
#include "interface.h"
#include "share.h"
#include "setuid.h"
#include "byteorder.h"

sigset_t sigset;	/* to circumvent a problem with the generic driver from the kernel */
struct sigaction sigac = { exit, 0, 0, NULL }; 
unsigned interface;
int sem_id;
int shm_id;
#define	MAXOFF max(sizeof(struct sg_header), 2*sizeof(int))
unsigned OFF;
unsigned nsectors = NSECTORS;
unsigned overlap = 3;
int lowendian = -1;

int trackindex_disp = FALSE;

/* buffer for cdrom audio data sector */
static unsigned char *bufferCddaRaw;
unsigned char *bufferCdda;
static unsigned char *bufferTOC;
static unsigned char *SubQbuffer;
static unsigned char *cmd;
unsigned char Extra_buffer[2048];
static int adjust_ssize;


/* Hack hack hack hack hack... makes the code clean, though. Zygo was here */
#ifndef SG_BIG_BUFF
#define SG_BIG_BUFF 4096	/* FIXME: should be the size of page */
#endif

/*******************************************************************
 *
 *	SCSI section
 *
 */

/* process a complete scsi cmd. Use either generic or ioctl interface. */
static int handle_scsi_cmd(unsigned cmd_len, 
			   unsigned in_size, 
			   unsigned char *i_buff, 
			   unsigned out_size, 
			   unsigned char *o_buff,
			   int expect_result)
{
    int status = 0;
    unsigned char *buff_p;
    struct sg_header *sg_hd;

    /* safety checks */
    if (!cmd_len) return -1;
    if (!i_buff) return -1;
    if (OFF + cmd_len + in_size > SG_BIG_BUFF) return -1;
    if (OFF + out_size > SG_BIG_BUFF) return -1;

    if (!o_buff) out_size = 0;

    if (out_size > in_size + cmd_len) {
	buff_p = o_buff;
	memcpy(o_buff + OFF, i_buff + OFF, in_size + cmd_len);
    } else {
	buff_p = i_buff;
    }

    if (interface == GENERIC_SCSI) {
	/* generic scsi device services */
	sg_hd = (struct sg_header *) buff_p;
	sg_hd->reply_len = OFF + out_size;
	sg_hd->twelve_byte = cmd_len == 12;
	sg_hd->result = 0;	/* is not cleared by the write call */
#if	0
	sg_hd->pack_len; /* not used */
	sg_hd->pack_id;	/* not used */
	sg_hd->other_flags; 	/* not used */
#endif
        sigprocmask ( SIG_BLOCK, &sigset, NULL );
	status = write( cd_fd, buff_p, OFF + cmd_len + in_size );
	if ( status < 0 || status != OFF + cmd_len + in_size || sg_hd->result ) {
	    if (status == -1) {
		perror("generic scsi write: ");
		exit(status);
	    } else {
	       fprintf( stderr, "\ngeneric scsi write status = 0x%x, result = 0x%x cmd = %x\n", 
		       status, sg_hd->result, i_buff[OFF] );
	    }
	    return status;
	}

	status = read( cd_fd, buff_p, OFF + out_size);
	if ( status < 0 || status != OFF + out_size || (sg_hd->result && !expect_result) ) {
	  if (sg_hd->sense_buffer[0]==0xf0 && /* hack to make Plextor 6plex happy */
	      sg_hd->sense_buffer[1]==0x0 &&
	      sg_hd->sense_buffer[2]==0x5 &&
	      /* 3-6 are some sort of block address */
	      sg_hd->sense_buffer[7]==0xa &&
	      sg_hd->sense_buffer[8]==0x0 &&
	      sg_hd->sense_buffer[9]==0x0 &&
	      sg_hd->sense_buffer[10]==0x0 &&
	      sg_hd->sense_buffer[11]==0x0 &&
	      sg_hd->sense_buffer[12]==0xbf &&
	      sg_hd->sense_buffer[13]==0x0 &&
	      sg_hd->sense_buffer[14]==0x0 &&
	      sg_hd->sense_buffer[15]==0x0) {
	    ;
	  } else {
	    fprintf( stderr, "\nread(generic) status = 0x%x, result = 0x%x cmd = %x\n", 
		    status, sg_hd->result, i_buff[OFF] );
	    fprintf( stderr, "read(generic) sense "
		  "%x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x\n", 
		  sg_hd->sense_buffer[0],	     sg_hd->sense_buffer[1],
		  sg_hd->sense_buffer[2],	     sg_hd->sense_buffer[3],
		  sg_hd->sense_buffer[4],	     sg_hd->sense_buffer[5],
		  sg_hd->sense_buffer[6],	     sg_hd->sense_buffer[7],
		  sg_hd->sense_buffer[8],	     sg_hd->sense_buffer[9],
		  sg_hd->sense_buffer[10],	     sg_hd->sense_buffer[11],
		  sg_hd->sense_buffer[12],	     sg_hd->sense_buffer[13],
		  sg_hd->sense_buffer[14],	     sg_hd->sense_buffer[15]
		  );
	  }
	  sigprocmask ( SIG_UNBLOCK, &sigset, NULL );
	  return 1;
	} else
	    /* get output back */
	    if (out_size && out_size <= in_size + cmd_len)
		memcpy(o_buff, i_buff, out_size + OFF);
	/* Look if we got what we expected to get */
	sigprocmask ( SIG_UNBLOCK, &sigset, NULL );
	if (status == OFF + out_size) status = 0; /* got them all */
    }
    return status;
}

static unsigned char density = 0;
static unsigned char orgmode4 = 0xff;
static unsigned char orgmode10, orgmode11;

/* get current sector size from SCSI cdrom drive */
static unsigned int 
get_orig_sectorsize(unsigned char *m4, unsigned char *m10, 
		    unsigned char *m11)
{
      /* first get current values for density, etc. */
      /* MODE_SENSE */
      static unsigned char cmdblk0 [6] = { 
	  0x1A, /* MODE_SENSE */
	  0x00, /* return block descriptor */
	  0x01, /* page control current values, page 1 */
	  0, /* reserved */
	  12, /* sizeof(modesense - OFF) */
	  0}; /* reserved */

      static unsigned char modesense[MAXOFF+12]
          __attribute__ ((aligned (__alignof__ (struct sg_header))));

      memcpy( cmd + OFF, cmdblk0, sizeof(cmdblk0) );
      /* do the scsi cmd */
      if (handle_scsi_cmd (6, 0, cmd, 12, modesense, 0))
	  perror ("get_orig_sectorsize mode sense failed\n");

      if (m4 != NULL)                       /* density */
        *m4 = modesense[OFF + 4];
      if (m10 != NULL)                      /* MSB sector size */
        *m10 = modesense[OFF + 10];
      if (m11 != NULL)                      /* LSB sector size */
        *m11 = modesense[OFF + 11];

      return (modesense[OFF + 10] << 8) + modesense[OFF + 11];
}


/* switch CDROM scsi drives to given sector size  */
static int set_sectorsize (unsigned int secsize)
{
  /* reserved, Medium type=0, Dev spec Parm = 0, block descriptor len 0 oder 8,
     Density (cd format) 
     (0=YellowBook, XA Mode 2=81h, XA Mode1=83h and raw audio via SCSI=82h),
     # blks msb, #blks, #blks lsb, reserved,
     blocksize, blocklen msb, blocklen lsb,
   */

  /* MODE_SELECT, page = SCSI-2  save page disabled, reserved, reserved, 
     parm list len, flags */
  static unsigned char cmdblk [6 + 12] = { 
                         0x15, /* MODE_SELECT */
			 0x10, /* no save page */
			    0, /* reserved */
			    0, /* reserved */
			    12, /* sizeof(mode) */
			    0, /* reserved */
       /* mode section */
			    0, 
                            0, 0, 
                            8,       /* Block Descriptor Length */
                            0,       /* Density Code */
                            0, 0, 0, /* # of Blocks */
                            0,       /* reserved */
                            0, 0, 0};/* Blocklen */
  unsigned char *mode = cmd + OFF + 6;
  int retval;

  if (orgmode4 == 0xff) {
    get_orig_sectorsize(&orgmode4, &orgmode10, &orgmode11);
  }

  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  /* prepare to read cds in the previous mode */
  mode [4] = orgmode4; 	       /* normal density */
  mode [10] =  secsize >> 8;   /* block length "msb" */
  mode [11] =  secsize & 0xFF; /* block length lsb */

  /* do the scsi cmd */
  if ((retval = handle_scsi_cmd (6, 12, cmd, 0, NULL, 0)))
        perror ("setting sector size failed\n");

  return retval;
}


/* switch Toshiba/DEC and HP drives from/to cdda density */
static void EnableCddaModeSelect (int fAudioMode)
{
  /* reserved, Medium type=0, Dev spec Parm = 0, block descriptor len 0 oder 8,
     Density (cd format) 
     (0=YellowBook, XA Mode 2=81h, XA Mode1=83h and raw audio via SCSI=82h),
     # blks msb, #blks, #blks lsb, reserved,
     blocksize, blocklen msb, blocklen lsb,
   */

  /* MODE_SELECT, page = SCSI-2  save page disabled, reserved, reserved, 
     parm list len, flags */
  static unsigned char cmdblk [6 + 12] = { 
                         0x15, /* MODE_SELECT */
			 0x10, /* no save page */
			    0, /* reserved */
			    0, /* reserved */
			    12, /* sizeof(mode) */
			    0, /* reserved */
       /* mode section */
			    0, 
                            0, 0, 
                            8,       /* Block Descriptor Length */
                            0,       /* Density Code */
                            0, 0, 0, /* # of Blocks */
                            0,       /* reserved */
                            0, 0, 0};/* Blocklen */
  unsigned char *mode = cmd + OFF + 6;

  if (orgmode4 == 0xff && fAudioMode) {
    get_orig_sectorsize(&orgmode4, &orgmode10, &orgmode11);
  }

  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  if (fAudioMode) {
    /* prepare to read audio cdda */
    mode [4] = density;  			/* cdda density */
    mode [10] = (CD_FRAMESIZE_RAW >> 8);   /* block length "msb" */
    mode [11] = (CD_FRAMESIZE_RAW & 0xFF);
  } else {
    /* prepare to read cds in the previous mode */
    mode [4] = orgmode4; /* 0x00; 			\* normal density */
    mode [10] = orgmode10; /* (CD_FRAMESIZE >> 8);  \* block length "msb" */
    mode [11] = orgmode11; /* (CD_FRAMESIZE & 0xFF); \* block length lsb */
  }

  /* do the scsi cmd */
  if (handle_scsi_cmd (6, 12, cmd, 0, NULL, 0))
        perror ("Audio mode switch failed\n");
}

static int tmp_fd;

static void Read_CD_Extra_Info(unsigned long sector)
{
  lseek(tmp_fd, (signed)sector * CD_FRAMESIZE, SEEK_SET);
  read(tmp_fd, Extra_buffer, CD_FRAMESIZE);
}

void Read_Subinfo(unsigned int pos, unsigned int length)
{
  int got, num_infos;
  unsigned char *Subp;

  Subp = alloca(length);
  if (Subp == NULL) {
    fprintf(stderr, "alloc error(%d)\n",length);
    exit(2);
  }

  lseek(tmp_fd, (signed)pos*CD_FRAMESIZE, SEEK_SET);
  got = read(tmp_fd, Subp, length);
  if (got == -1) {
    perror("read_subinfo");
    fprintf(stderr, "read error\n");
    exit(2);
  }

  num_infos = Subp[45]+(Subp[44] << 8);
  fprintf(stderr, "subinfo version %c%c.%c%c, %d info packets\n",
	  Subp[8],
	  Subp[9],
	  Subp[10],
	  Subp[11],
	  num_infos);

  length -= 46;
  Subp += 46;
  while (length > 0) {
    const char *infopacketID[] = { "0", 
			      "track identifier", 
			      "album title",
			      "universal product code", 
			      "international standard book number",
			      "copyright",
			      "track title",
			      "notes",
			      "main interpret",
			      "secondary interpret",
			      "composer",
			      "original composer",
			      "creation date",
			      "release date",
			      "publisher",
			      "0f",
			      "isrc audio track",
			      "isrc lyrics",
			      "isrc pictures",
			      "isrc MIDI data",
			      "14", "15", "16", "17", "18", "19",
			      "copyright state SUB_INFO",
			      "copyright state intro lyrics",
			      "copyright state lyrics",
			      "copyright state MIDI data",
			      "1e", "1f",
			      "intro lyrics",
			      "pointer to lyrics text file and length", 
			      "22", "23", "24", "25", "26", "27", "28",
			      "29", "2a", "2b", "2c", "2d", "2e", "2f",
			      "still picture descriptor",
			      "31",
			      "32", "33", "34", "35", "36", "37", "38",
			      "39", "3a", "3b", "3c", "3d", "3e", "3f",
			      "MIDI file descriptor",
			      "genre code",
			      "tempo",
			      "key"
			     };
    int id = *Subp;
    int len = *(Subp +1);

    if (id >= 0x44) {
      fprintf(stderr, "unknown ID (%d)\n", id);
      Subp += 2 + 1;
      length -= 2 + 1;
      continue;
    }
    fprintf(stderr, "Id=%d, ",id);

    switch (id) {
    case 1:
      if (!strncmp("00", (char *) (Subp + 2), 2)) {
	fprintf(stderr, "info for whole disc\n");
      } else {
	fprintf(stderr, "info for tracks upto %c%c\n", *(Subp + 2), *(Subp + 3));
      }
      break;

    case 0x1a:
    case 0x1b:
    case 0x1c:
    case 0x1d:
	fprintf(stderr, "%s %scopyrighted\n", infopacketID[id], *(Subp + 2) == 0 ? "not " : "");
      break;

    case 0x21:
      fprintf(stderr, "lyrics file beginning at sector %u",
	      (unsigned) be32_to_cpu(*(unsigned int *)(Subp + 2)));
      if (len == 8)
	fprintf(stderr, ", having length: %u\n", 
                (unsigned) be32_to_cpu(*(unsigned int *)(Subp + 6)));
      else
	fputs("\n", stderr);
      break;

    case 0x40:
      fprintf(stderr, "MIDI file beginning at sector %u",
	      (unsigned) be32_to_cpu(*(unsigned int *)(Subp + 2)));
      if (len == 8)
	fprintf(stderr, ", having length: %u\n", 
		(unsigned) be32_to_cpu(*(unsigned int *)(Subp + 6)));
      else
	fputs("\n", stderr);
      break;

    case 0x42:
      fprintf(stderr, "%s: %d beats per minute\n",infopacketID[id], *(Subp + 2));
      break;

    case 0x41:
    case 0x43:
    case 0x30:
      fprintf(stderr, "%s: format unknown\n",infopacketID[id]);
      break;

    default:
      fprintf(stderr, "%s: %*.*s\n",infopacketID[id], len, len, (Subp +2));
    }

    if (len & 1) len++;
    Subp += 2 + len;
    length -= 2 + len;
  }
}

void dump_extra_info()
{
  unsigned char *p = Extra_buffer + 48;
  unsigned int pos, length;

  while (*p) {
    pos    = be32_to_cpu(*((unsigned int *)(p+2)));
    length = be32_to_cpu(*((unsigned int *)(p+6)));
    pos += session_start;
    fprintf(stderr, "Language: %c%c (ISO 639) at sector %u, len=%u\n",
	             *p, *(p+1), pos, length);

    /* dump this entry */
    Read_Subinfo(pos, length);
    p += 10;
  }
}


static int FixupTOC(unsigned long tracks)
{
  /*
   * FIXME: we would have to do a ioctl (CDROMMULTISESSION)
   *        for the cdrom device associated with the generic device
   *	    not just AUX_DEVICE
   */
  struct cdrom_multisession ms_str;

  if (interface == GENERIC_SCSI)
    tmp_fd = open (aux_name, O_RDONLY);
  else
    tmp_fd = cd_fd;

  if (tmp_fd != -1) {
    int result;

    ms_str.addr_format = CDROM_LBA;
    result = ioctl(tmp_fd, CDROMMULTISESSION, &ms_str);
    if (result == -1)
      perror("ioctl failed: ");

    if (ms_str.addr.lba > 100) {
      int j;

      /* believe the multisession offset :-) */
      /* adjust end of last audio track to be in the first session */
      for (j = tracks-1; j >= 0; j--) {
	if (j > 0 && !IS_AUDIO(j) && IS_AUDIO(j-1)) {
	  if (g_toc[j].dwStartSector > ms_str.addr.lba - 11400) {
	    session_start = ms_str.addr.lba;
	    Read_CD_Extra_Info(g_toc[j].dwStartSector + 75);
	    g_toc[j].dwStartSector = ms_str.addr.lba - 11400;
	    return 1;
	  }
	  break;
	}
      }
    }
  } else
    fprintf(stderr,"cannot open %s: %s\n",aux_name,strerror(errno));
  return 0;
}

/* read the table of contents from the cd and fill the TOC array */
static unsigned ReadTocSCSI ( TOC *toc )
{
  int i;
  unsigned tracks;

  /* READTOC, MSF format flag, res, res, res, res, Start track, len msb,
     len lsb, flags */
  static unsigned char cmdblk [] = { 
    0x43, 0, 0, 0, 0,   0, 1, CD_FRAMESIZE >> 8, CD_FRAMESIZE & 0xFF, 0
  };
  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  /* do the scsi cmd (read table of contents) */
  if (handle_scsi_cmd ( sizeof(cmdblk), 0, cmd, CD_FRAMESIZE, bufferTOC, 0))
      FatalError ("Read TOC failed.\n");

  /* copy to our structure and convert start sector */
  tracks = ((bufferTOC [OFF + 0] << 8) + bufferTOC [OFF + 1] - 2) / 8;
  if (tracks > MAXTRK) return 0;

  for (i = 0; i < tracks; i++) {
      memcpy (&toc[i], bufferTOC + OFF + 4 + 8*i, 8);
      toc[i].ISRC[0] = 0;
      toc[i].dwStartSector = adjust_ssize * be32_to_cpu(toc[i].dwStartSector);
      if ( toc [i].bTrack != i+1 )
	  toc [i].bTrack = i+1;
  }
  have_CD_extra = FixupTOC(tracks);
  return --tracks;           /* without lead-out */
}


/* Read max. SectorBurst of cdda sectors to bufferCdda+OFF
   via standard SCSI-2 Read(10) command */
static void ReadStandard (unsigned char *p, long lSector, unsigned long SectorBurstVal )
{
  /* READ10, flags, block1 msb, block2, block3, block4 lsb, reserved, 
     transfer len msb, transfer len lsb, block addressing mode */
  static unsigned char cmdblk [10] = {0x28, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  unsigned char *cmd0 = cmd + OFF;
  int count = 3;

  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  cmd0 [3] = (unsigned char)((lSector >> 16) & 0xFF);
  cmd0 [4] = (unsigned char)((lSector >> 8) & 0xFF);
  cmd0 [5] = (unsigned char)(lSector & 0xFF);

  cmd0 [8] = (unsigned char)SectorBurstVal;

  while (handle_scsi_cmd(sizeof(cmdblk), 0, cmd, 
			 SectorBurstVal * CD_FRAMESIZE_RAW, p - OFF, 0 )
	 && count--)
    ;
  if (!count)
    FatalError ("Read CD-ROM10 failed\n");
}


/* Read max. SectorBurst of cdda sectors to bufferCdda+OFF
   via vendor-specific ReadCdda(10) command */
static void ReadCdda10 (unsigned char *p, long lSector, unsigned long SectorBurstVal )
{
  /* READ10, flags, block1 msb, block2, block3, block4 lsb, reserved, 
     transfer len msb, transfer len lsb, block addressing mode */
  static unsigned char cmdblk [10] = {0xd4, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  unsigned char *cmd0 = cmd + OFF;
  int count = 3;

  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  cmd0 [3] = (unsigned char)((lSector >> 16) & 0xFF);
  cmd0 [4] = (unsigned char)((lSector >> 8) & 0xFF);
  cmd0 [5] = (unsigned char)(lSector & 0xFF);

  cmd0 [8] = (unsigned char)SectorBurstVal;

  while (handle_scsi_cmd(sizeof(cmdblk), 0, cmd, 
			 SectorBurstVal * CD_FRAMESIZE_RAW, p - OFF, 0 )
	 && count--)
    ;
  if (!count)
    FatalError ("Read CD-ROM10 (NEC) failed\n");
}

/* Read max. SectorBurst of cdda sectors to bufferCdda+OFF
   via vendor-specific ReadCdda(12) command */
static void ReadCdda12 (unsigned char *p, long lSector, unsigned long SectorBurstVal )
{
  static unsigned char cmdblk [12] = {0xd8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  unsigned char *cmd0 = cmd + OFF;
  int count = 3;

  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  cmd0 [3] = (unsigned char)((lSector >> 16) & 0xFF);
  cmd0 [4] = (unsigned char)((lSector >> 8) & 0xFF);
  cmd0 [5] = (unsigned char)(lSector & 0xFF);
  cmd0 [9] = (unsigned char)SectorBurstVal;

  while (handle_scsi_cmd(sizeof(cmdblk), 0, cmd, 
			 SectorBurstVal * CD_FRAMESIZE_RAW, p - OFF, 0 )
	 && count--)
    ;
  if (!count)
    FatalError ("Read CD-ROM12 failed\n");
}

/* Read the Sub-Q-Channel to SubQbuffer+OFF */
static subq_chnl *ReadSubQSCSI ( unsigned char format, unsigned char track )
{
  static unsigned char cmdblk [10] = {0x42, 0x02, 0x40, 0, 0, 0, 0, 0, 48, 0};

  unsigned char *cmd0 = cmd + OFF;
  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  cmd0 [3] = format;
  cmd0 [6] = track;

  if (handle_scsi_cmd(sizeof(cmdblk), 0, cmd, 48, SubQbuffer, 0 ))
    FatalError ("Read SubQ failed\n");

  return (subq_chnl *)(SubQbuffer + OFF);
}

/* request vendor brand and model */
unsigned char *Inquiry ( void )
{
  static unsigned char cmdblk [6] = {0x12, 0, 0, 0, 56, 0};
  static unsigned char Inqbuffer[sizeof(struct sg_header) + 56]
          __attribute__ ((aligned (__alignof__ (struct sg_header))));

  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  if (handle_scsi_cmd(sizeof(cmdblk), 0, cmd, sizeof(Inqbuffer) - OFF, 
			    Inqbuffer, 0 )) {
      FatalError ("Inquiry failed\n");
  }

  return (Inqbuffer + OFF);
}

#define TESTUNITREADY_CMD 0
#define TESTUNITREADY_CMDLEN 6

#define ADD_SENSECODE 12
#define ADD_SC_QUALIFIER 13
#define NO_MEDIA_SC 0x3a
#define NO_MEDIA_SCQ 0x00

int TestForMedium ( void )
{
  /* request READY status */
  static unsigned char cmdblk [TESTUNITREADY_CMDLEN] = {
      TESTUNITREADY_CMD, /* command */
                      0, /* reserved */
                      0, /* reserved */
                      0, /* reserved */
                      0, /* reserved */
                      0};/* reserved */

  if (interface != GENERIC_SCSI) {
    return 1;
  }

  memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  if (handle_scsi_cmd(sizeof(cmdblk), 0, cmd, 
			    0, NULL, 1)) {
      FatalError ("Test unit ready failed\n");
      return 0;
  }

  return 
   *(((struct sg_header*)cmd)->sense_buffer +ADD_SENSECODE) != NO_MEDIA_SC ||
   *(((struct sg_header*)cmd)->sense_buffer +ADD_SC_QUALIFIER) != NO_MEDIA_SCQ;
}


/* hook */
static void Dummy (int Switch)
{
}

/* set the correct command set for *different*
 * vendor specific implementations. Was this really necessary, folks?
 */
static void SetupSCSI( void )
{
    unsigned char *p;

    if (interface != GENERIC_SCSI) {
	/* unfortunately we have the wrong interface and are
	 * not able to change on the fly */
	fprintf(stderr, "The generic SCSI interface and devices are required\n");
	exit(1);
    }

    /* check for the correct type of unit. */
    p = Inquiry();
    if (*p != TYPE_ROM && *p != TYPE_WORM) {
	fprintf(stderr, "this is neither scsi cdrom nor worm\n");
	exit(1);
    }

    /* generic Sony type defaults */
    density = 0x0;
    EnableCdda = Dummy;
    ReadCdRom = ReadCdda12;

    /* check for brands and adjust special peculiaritites */

    /* If your drive is not treated correctly, you can adjust some things
       here:

       lowendian: should be to 1, if the CDROM drive or CD-Writer delivers 
                  the samples in the native byteorder of the audio cd
		  (LSB first).
		  HP CD-Writers need it set to 0.
       NOTE: If you get correct wav files when using sox with the '-x' option,
             the endianess is wrong. In this case email a report to
	     heiko@colossus.escape.de with details of your drive (the output
	     of 'cat /proc/scsi/scsi') and toggle the value of lowendian.


     */
    if (!memcmp(p+8,"TOSHIBA", 7) ||
        !memcmp(p+8,"DEC", 3)) {
	density = 0x82;
	EnableCdda = EnableCddaModeSelect;
 	ReadCdRom = ReadStandard;
	lowendian = 1;
    } else if (!memcmp(p+8,"IMS",3) ||
               !memcmp(p+8,"KODAK",5) ||
               !memcmp(p+8,"HP",2) ||
               !memcmp(p+8,"PHILIPS",7) ||
               !memcmp(p+8,"PLASMON",7) ||
               !memcmp(p+8,"GRUNDIG CDR100IPW",17) ||
               !memcmp(p+8,"MITSUMI CD-R ",13)) {
	EnableCdda = EnableCddaModeSelect;
	ReadCdRom = ReadStandard;

	/* treat all of these as bigendian */
	lowendian = 0;

	/* no overlap reading for cd-writers */
	overlap = 0;
    } else if (!memcmp(p+8,"YAMAHA",6)) {
	EnableCdda = EnableCddaModeSelect;

	/* no overlap reading for cd-writers */
	overlap = 0;
	lowendian = 1;
    } else if (!memcmp(p+8,"PLEXTOR",7) ||
               !memcmp(p+8,"SONY",4)) {
/*	overlap = 0; */
	lowendian = 1;
    } else if (!memcmp(p+8,"NEC",3)) {
	ReadCdRom = ReadCdda10;
	lowendian = 1;
    }

    ReadToc = ReadTocSCSI;
    ReadSubQ = ReadSubQSCSI;

    /* look if caddy is loaded */
    while (!TestForMedium()) {
	fprintf(stderr,"load cdrom please and press enter");
	getchar();
    }
}

/*******************************************************************
 *
 *	cooked ioctl section
 *
 */
static struct cdrom_tochdr hdr;
static struct cdrom_tocentry entry[100];
static struct cdrom_read_audio arg;
static int err;

/* read the table of contents (toc) via the ioctl interface */
static unsigned ReadToc_cooked ( TOC *toc )
{
    int i;
    unsigned tracks;

    /* get TocHeader to find out how many entries there are */
    err = ioctl( cd_fd, CDROMREADTOCHDR, &hdr );
    if ( err != 0 ) {
	/* error handling */
	if (err == -1) {
	    if (errno == EPERM)
		fprintf( stderr, "Please run this program setuid root.\n");
	    perror("cooked: Read TOC ");
	    exit( err );
	} else {
	    fprintf( stderr, "can't get TocHeader (error %d).\n", err );
	    exit( -1 );
	}
    }
    /* get all TocEntries */
    for ( i = 0; i < hdr.cdth_trk1; i++ ) {
	entry[i].cdte_track = 1+i;
	entry[i].cdte_format = CDROM_LBA;
	err = ioctl( cd_fd, CDROMREADTOCENTRY, &entry[i] );
	if ( err != 0 ) {
	    /* error handling */
	    fprintf( stderr, "can't get TocEntry #%d (error %d).\n", i+1, err );
	    exit( -1 );
	}
    }
    entry[i].cdte_track = CDROM_LEADOUT;
    entry[i].cdte_format = CDROM_LBA;
    err = ioctl( cd_fd, CDROMREADTOCENTRY, &entry[i] );
    if ( err != 0 ) {
	/* error handling */
	fprintf( stderr, "can't get TocEntry LEADOUT (error %d).\n", err );
	exit( -1 );
    }
    tracks = hdr.cdth_trk1+1;
    for (i = 0; i < tracks; i++) {
        toc[i].bFlags = (entry[i].cdte_adr << 4) | (entry[i].cdte_ctrl & 0x0f);
        toc[i].bTrack = entry[i].cdte_track;
        toc[i].dwStartSector = entry[i].cdte_addr.lba;
    }
    have_CD_extra = FixupTOC(tracks);
    return --tracks;           /* without lead-out */
}

/* read 'SectorBurst' adjacent sectors of audio sectors 
 * to Buffer '*p' beginning at sector 'lSector'
 */
static void ReadCdRom_cooked (unsigned char *p, long lSector, unsigned long SectorBurstVal )
{
  int retry_count=0;

/* read 2352 bytes audio data */
  arg.addr.lba = lSector;
  arg.addr_format = CDROM_LBA;
  arg.nframes = SectorBurstVal;
  arg.buf = &p[0];

  do {
    err = ioctl(cd_fd, CDROMREADAUDIO, &arg);
    retry_count++;
  } while ((err) && (retry_count < 30));
  if (err != 0) {
      /* error handling */
      if (err == -1) {
	  if (errno == EINVAL || errno == EIO)
	      fprintf( stderr, "Sorry, this driver and/or drive does not support cdda reading.\n");
	  perror("cooked: Read cdda ");
	  exit( err );
      } else {
	  fprintf(stderr, "can't read frame #%d (error %d).\n", 
		  arg.addr.lba, err);
	  exit( -1 );
      }
  }
  if (!quiet && retry_count != 1)
    fprintf(stderr, "\n%d retries\n",retry_count-1);
}

/* request sub-q-channel information. This function may cause confusion
 * for a drive, when called in the sampling process.
 */
static subq_chnl *ReadSubQ_cooked ( unsigned char format, unsigned char track )
{
    struct cdrom_subchnl sub_ch;

    if ( format != GET_POSITIONDATA ) return NULL;  /* not supported by kernel */

    if (!(err = ioctl(cd_fd, CDROMSUBCHNL, &sub_ch))) {
	/* copy to SubQbuffer */
	subq_chnl *SQp = (subq_chnl *) (SubQbuffer + OFF);
	subq_position *SQPp = (subq_position *) &SQp->data;
	SQp->audio_status 	= sub_ch.cdsc_audiostatus;
	SQp->format 		= sub_ch.cdsc_format;
	SQp->control_adr	= (sub_ch.cdsc_adr << 4) | (sub_ch.cdsc_ctrl & 0x0f);
	SQp->track 		= sub_ch.cdsc_trk;
	SQp->index 		= sub_ch.cdsc_ind;
	SQPp->abs_min 		= sub_ch.cdsc_absaddr.msf.minute;
	SQPp->abs_sec 		= sub_ch.cdsc_absaddr.msf.second;
	SQPp->abs_frame 	= sub_ch.cdsc_absaddr.msf.frame;
	SQPp->trel_min 		= sub_ch.cdsc_reladdr.msf.minute;
	SQPp->trel_sec 		= sub_ch.cdsc_reladdr.msf.second;
	SQPp->trel_frame 	= sub_ch.cdsc_reladdr.msf.frame;
    } else {
	if (err == -1) {
	    if (errno == EPERM)
		fprintf( stderr, "Please run this program setuid root.\n");
	    perror("cooked: Read subq ");
	    exit( err );
	} else {
	    fprintf(stderr, "can't read sub q channel (error %d).\n", err);
	    exit( -1 );
	}
    }
  return (subq_chnl *)(SubQbuffer + OFF);
}

/* set function pointers to use the ioctl routines */
static void SetupCookedIoctl( char *pdev_name )
{
    struct stat statstruct;

    if (fstat(cd_fd, &statstruct)) {
      fprintf(stderr, "cannot stat cd %ld\n",cd_fd);
      exit(1);
    }
    switch (statstruct.st_rdev >> 8) {
    case CDU31A_CDROM_MAJOR:	/* sony cdu-31a/33a */
        if (nsectors >= 14) {
	  overlap = 10;
	}
        break;
    case MATSUSHITA_CDROM_MAJOR:	/* sbpcd 1 */
    case MATSUSHITA_CDROM2_MAJOR:	/* sbpcd 2 */
    case MATSUSHITA_CDROM3_MAJOR:	/* sbpcd 3 */
    case MATSUSHITA_CDROM4_MAJOR:	/* sbpcd 4 */
        /* some are more compatible than others */
	err = ioctl(cd_fd, CDROMAUDIOBUFSIZ, nsectors);
        if (err == -1) {
	  perror("ioctl(CDROMAUDIOBUFSIZ)");
        }
	break;
    }

    EnableCdda = Dummy;
    ReadCdRom = ReadCdRom_cooked;
    ReadToc = ReadToc_cooked;
    ReadSubQ = ReadSubQ_cooked;
}

/********************** General setup *******************************/

/* As the name implies, interfaces and devices are checked.  We also
   adjust nsectors, overlap, and interface for the first time here.
   Any unnecessary privileges (setuid, setgid) are also dropped here.
*/
static void Check_interface_for_device( struct stat *statstruct, char *pdev_name)
{
    static const char *int_names[]={
       "generic_scsi", "cooked_ioctl"
    };

    if (!S_ISCHR(statstruct->st_mode) &&
	!S_ISBLK(statstruct->st_mode)) {
      fprintf(stderr, "%s is not a device\n",pdev_name);
      exit(1);
    }

    switch (statstruct->st_rdev >> 8) {
    case SCSI_CDROM_MAJOR:     /* scsi cd */
       if (!S_ISBLK(statstruct->st_mode)) {
           fprintf(stderr, "%s is not a block device\n",pdev_name);
           exit(1);
       }
       fprintf(stderr,"cdda2wav no longer supports the scsi_ioctl interface.\n");
       fprintf(stderr,"Please use generic_scsi instead.\n");
       break;
    case SCSI_GENERIC_MAJOR:	/* generic */
       if (!S_ISCHR(statstruct->st_mode)) {
	 fprintf(stderr, "%s is not a char device\n",pdev_name);
	 exit(1);
       }
       if (interface != GENERIC_SCSI) {
	 if (!S_ISCHR(statstruct->st_mode)) {
	   fprintf(stderr, "%s is not a char device\n",pdev_name);
	   exit(1);
	 }
	    
	 fprintf(stderr, "wrong interface (%s) for this device (%s)\n"
                 "set to generic_scsi\n",int_names[interface], pdev_name);
	 interface = GENERIC_SCSI;
       }
       nsectors = (SG_BIG_BUFF - OFF)/CD_FRAMESIZE_RAW;
       neverneedroot(); /* still may need group */
       break;
    default:
	if (!S_ISBLK(statstruct->st_mode)) {
	    fprintf(stderr, "%s is not a block device\n",pdev_name);
	    exit(1);
	}
	if (interface != COOKED_IOCTL) {
	    fprintf(stderr, "cdrom device (%s). "
		    "Setting interface to cooked_ioctl.\n", pdev_name);
	    interface = COOKED_IOCTL;
	}
        neverneedroot();    /* no special privilege required */
        neverneedgroup();
	break;
    }
    if (overlap >= nsectors)
      overlap = nsectors-1;
}

/* open the cdrom device */
static int OpenCdRom ( char *pdev_name )
{
  int cd_filedesc;
  struct stat statstruct, fstatstruct;

  if (stat(dev_name, &statstruct)) {
      fprintf(stderr, "cannot stat device %s\n", pdev_name);
      exit(1);
  }
  Check_interface_for_device( &statstruct, pdev_name );

  if (interface == GENERIC_SCSI) {
      /* may need special group privileges if there is a group for SCSI
         generic devices and we are setgid. */
      needgroup();
      cd_filedesc = open(pdev_name,O_RDWR);
      dontneedgroup();
  } else
      cd_filedesc = open(pdev_name,O_RDONLY);

  if (cd_filedesc < 0)
    FatalError ("open(%s): %s\n",pdev_name,strerror(errno));

  /* Do final security checks here */
  if (fstat(cd_filedesc, &fstatstruct)) {
    fprintf(stderr, "Could not fstat %s (fd %d): %s\n", pdev_name, cd_filedesc,
               strerror(errno));
    exit(1);
  }
  Check_interface_for_device( &fstatstruct, pdev_name );

  /* Watch for race conditions */
  if (fstatstruct.st_dev != statstruct.st_dev ||
      fstatstruct.st_ino != statstruct.st_ino) {
    fprintf(stderr,"Race condition attempted in OpenCdRom.  Exiting now.\n");
    exit(1);
  }
  return cd_filedesc;
}

/* perform initialization depending on the interface used. */
void SetupInterface( unsigned char *int_name )
{
    /* build signal set to block for during generic scsi */
    sigemptyset (&sigset);
    sigaddset (&sigset, SIGINT);
    sigaddset (&sigset, SIGPIPE);
    sigac.sa_handler = exit;
    sigemptyset(&sigac.sa_mask);
    sigac.sa_flags = 0;
    sigaction( SIGINT, &sigac, NULL);
    sigaction( SIGQUIT, &sigac, NULL);
    sigaction( SIGTERM, &sigac, NULL);
    sigaction( SIGHUP, &sigac, NULL);
    sigaction( SIGPIPE, &sigac, NULL);
    sigaction( SIGTRAP, &sigac, NULL);
    sigaction( SIGIOT, &sigac, NULL);


    /* ensure interface is setup correctly */
    cd_fd = OpenCdRom ( dev_name );

    /* set offset OFF according to interface */
    switch (interface) {
    case GENERIC_SCSI:	OFF = sizeof(struct sg_header); 
	break;
    case COOKED_IOCTL:	OFF = 0;
	break;
    }

    /* install semaphores for double buffer usage */
    sem_id = seminstall(IPC_PRIVATE,4);
    if ( sem_id == -1 ) {
      perror("seminstall");
      exit(1);
    }

    /* request a doublebuffer for cdda sector audio data */
    shm_id = shm_request(IPC_PRIVATE,
                    2*(OFF + nsectors * CD_FRAMESIZE_RAW),&bufferCddaRaw);
    if ( shm_id == -1 ) {
	perror("sem_request");
	exit(1);
    }
    bufferCdda = bufferCddaRaw + OFF;

    /* request one sector for table of contents */
    bufferTOC = malloc( OFF + CD_FRAMESIZE );      /* assumes sufficient aligned addresses */
    /* SubQchannel buffer */
    SubQbuffer = malloc( OFF + 48 );               /* assumes sufficient aligned addresses */
    cmd = malloc( OFF + 18 );                      /* assumes sufficient aligned addresses */
    if ( !bufferTOC || !SubQbuffer || !cmd ) {
       fprintf( stderr, "Too low on memory. Giving up.\n");
       exit(2);
    }

    adjust_ssize = 1;
    /* if drive is of type scsi, get vendor name */
    if (interface == GENERIC_SCSI) {
        unsigned sector_size;

	SetupSCSI();
        sector_size = get_orig_sectorsize(&orgmode4, &orgmode10, &orgmode11);
        if ( sector_size != 2048 && set_sectorsize(2048) ) {
	  fprintf( stderr, "Could not change sector size from %d to 2048\n", sector_size );
	  adjust_ssize = 2048 / sector_size;
        }
    } else {
	SetupCookedIoctl( dev_name );
    }
}

/* release semaphores and shared memory */
void free_semshm(void)
{
  int   mycmd;
  union semun myarg;

  mycmd = IPC_RMID;
  semctl(sem_id,0,mycmd,myarg);
  shmctl(shm_id,mycmd,0L);
}
