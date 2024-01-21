/* readcdda.c: Read Audio-tracks from a CDROM 
   28.3.96 T. Niederreiter 
   Supports 2 modes of operation: 
   - normal mode, used to record the cdda-data to a file, or listen to it
     without interruption.
   - interactive mode, only to listen to a track and controlling the
     playback immeditely. 
   Based on cdda2wav from H. Eissfeldt 

   1.11.96: Bugfix in generic-scsi-code. (That workaround switch is
            no longer needed)
   4.01.97: D. Gates
	    added Sony support
*/

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <linux/cdrom.h>
#include <scsi/scsi.h>
#include <scsi/sg.h>
#include <getopt.h>
#include <linux/soundcard.h>
#include <linux/cdrom.h>

#define MAX_TRACKS 100		/* maximum of audio tracks */
#define MAX_TOC_LENGTH 0x322	/* max. length of a TOC */
#define NSECTORS 13

#define OFF sizeof(struct sg_header)

int cd_fd;
int blk_fd;
int sndfd;
FILE *audio = NULL;
int tracks = 0;
int track = 1;
int endtrack = -1;
int echo = 0;
int verbose = 0;
int quiet = 0;
int swapendian = 0;
int swapaudio = 0;
int no_file = 0;
int tclmode = 0;
int stopnow = 0;
int tick = 0;
int oldtick = 0;
int doplay = 0;
int speed = -1;
int percent = 0;
int perc = 0;
int oldperc = 0;
int sonyRawNormal = 0;

char fname[1024] = "audio.cdr";
char dname[1024] = "";
char blkname[1024] = "/dev/cdrom";
char sname[1024] = "";
unsigned char *cmd;
unsigned char *bufferCddaRaw;
unsigned char *bufferCdda;
unsigned nsectors = NSECTORS;
char keybuffer[1024];
static struct cdrom_tochdr hdr;
static struct cdrom_tocentry entry[101];
static struct cdrom_read_audio arg;


/* Funktion-Pointers */
int (*EnableCdda)();
void (*ReadCdRom)(long lSector, unsigned long SectorBurst);



/* Close all open files */

void CloseAll(void) {

	if (echo) {
		close(sndfd);
	}

	if (audio) {
		fclose(audio);
		audio = NULL;
	}

	if (cd_fd >= 0) {
		EnableCdda(0);
		close(cd_fd);
		cd_fd = -1;
	}

	if (blk_fd >= 0) {
		close(blk_fd);
		blk_fd = -1;
	}
}


/* Called when User presses CTRL-C */

void sigint_handle(int status) {

	exit(1);
}

/* report a fatal error, clean up and exit */

void FatalError (char *szMessage, ...)
{
  	vprintf (szMessage, (char *)(&szMessage + 1));
  	CloseAll();
	exit(1);
}


/* Open Audiofile for writing */

void OpenAudio(char *fname) {

	audio = fopen(fname, "wb");
	if (!audio) 
		FatalError ("Could not open %s\n", fname);
}


/* Converts frames into a formated text-string showing min:sec.centi_sec */

void convertframes2time( unsigned long dw, char *tmpstr ) {
unsigned mins,sec,centi_sec;

  	mins = dw / ( 60*75 );
  	sec = ( dw % ( 60*75 ) ) / 75;
        centi_sec = (4* ( dw % 75 ) +1 ) /3; 

        sprintf(tmpstr,"%2u:%02u.%02u",mins,sec,centi_sec);
}
	

/* Converts frames into MegaBytes */

int convertframes2mb( unsigned long dw ) {

	return (CD_FRAMESIZE*dw/1024/1024);
}

/* Display percent recorded until now */

void displaypercent(long Total, long ToDo) {

	perc=(int)((Total-ToDo)*100/Total)+1;
	if ((perc != oldperc) && (perc <= 100)) {
		printf("%d\n",perc);
		fflush(stdout);
		oldperc=perc;
	}
}

/* Save Cdda-Buffer to disk and sounddevice */

int SaveBuffer(long SecsToDo, long TotalSecs) {
unsigned long BytesToDo,outlen;
unsigned char *pDst;
unsigned char *pStart;
int retval = 0;
char tmpstr[10];
unsigned char *ii;
char tmpswap;

 	BytesToDo = (SecsToDo > nsectors ? nsectors : SecsToDo)*CD_FRAMESIZE_RAW;
	pStart = (unsigned char *)bufferCdda;
	pDst = pStart + BytesToDo;

	outlen = pDst - pStart;

	if (!quiet && !percent) {
		convertframes2time((TotalSecs-SecsToDo),tmpstr);
		printf("%s\r",tmpstr);
		fflush(stdout);
	}

	if (percent) {
		displaypercent(TotalSecs,SecsToDo);
	}

	/* No byte-swapping and sound-output */
      	if ((swapaudio == 0) && echo) {
        	write(sndfd, pStart, outlen);
	}

	/* Byte-swapping and disk-output */
     	if ((swapendian == 1) && !no_file) { 
    		retval = (fwrite( pStart, outlen, 1, audio) != 1);
       	}

        /* Swap the byte-order in the buffer */
   	for (ii=pStart; ii<pDst; ii+=2) {
    		tmpswap=ii[0];
    		ii[0]=ii[1];
    		ii[1]=tmpswap;
   	}

	/* Byte-swapping and sound-output */
      	if ((swapaudio == 1) && echo) {
        	write(sndfd, pStart, outlen);
	}

	/* No byte-swapping and disk-output */
     	if ((swapendian == 0) && !no_file) { 
    		retval = (fwrite( pStart, outlen, 1, audio) != 1);
       	}

	return retval;
}


/* Get from the TOC the StartSector of p_track */

long GetStartSector (int p_track) {
int i;
long dw;

	for (i = 1; i <= tracks; i++) {
   		if (entry[i].cdte_track == p_track) {
              		dw = entry[i].cdte_addr.lba;
              		if (entry[i].cdte_ctrl & CDROM_DATA_TRACK)
                		return -1;
              		return dw;
            	}
     	}
   	return -1;
}


/* Get the EndSector of a Track. 
   Done by getting the Start Sector of next track minus 1 */

long GetEndSector (int p_track) {
int i;
long dw;
	
      	for (i = 2; i <= tracks+1; i++ ) {
            	if (entry[i-1].cdte_track == p_track ) {
              		dw = entry[i].cdte_addr.lba;
              		if (entry[i].cdte_ctrl & CDROM_DATA_TRACK )
                		return -1;
              		return dw-1;
     		}	
  	}
   	return -1;
}


/* Find the first audio-track on CD 
   Returns zero if no audio-track was found */

long FirstTrack (void) {
int i;

   	for (i = 1; i <= tracks; i++) {
            	if ( entry[i].cdte_track != CDROM_LEADOUT &&
                	!( entry[i].cdte_ctrl & CDROM_DATA_TRACK ) )
              		return entry[i].cdte_track;
    	}
  	return 0;
}

/* Process a Generic-SCSI-Command */

int handle_scsi_cmd(unsigned cmd_len, 
                    unsigned in_size, 
                    unsigned char *i_buff, 
                    unsigned out_size, 
                    unsigned char *o_buff) {
int i;
int status = 0;
unsigned char *buff_p;
struct sg_header *sg_hd;

	/* safety checks */
    	if (!cmd_len) return -1;
    	if (!i_buff) return -1;
#ifdef SG_BIG_BUFF
    	if (OFF + cmd_len + in_size > SG_BIG_BUFF) return -1;
    	if (OFF + out_size > SG_BIG_BUFF) return -1;
#else
    	if (OFF + cmd_len + in_size > 4096) return -1;
    	if (OFF + out_size > 4096) return -1;
#endif

    	if (!o_buff) out_size = 0;

    	if (out_size > in_size + cmd_len) {
        	buff_p = o_buff;
        	memcpy(o_buff + OFF, i_buff + OFF, in_size + cmd_len);
    	} else {
        	buff_p = i_buff;
    	}

        /* generic scsi device services */
   	sg_hd = (struct sg_header *) buff_p;
  	sg_hd->pack_len = OFF + cmd_len + in_size;
  	sg_hd->reply_len = OFF + out_size;
    	sg_hd->twelve_byte = cmd_len == 12;
	sg_hd->result = 0;
#if 0
     	sg_hd->pack_id; /* not used */
   	sg_hd->other_flags;     /* not used */
#endif
  	status = write( cd_fd, buff_p, OFF + cmd_len + in_size );

    	if ( status < 0 || status != OFF + cmd_len + in_size || sg_hd->result ) {
       		if (status == -EPERM) {
                	fprintf( stderr, "Please run this program setuid root.\n");
                	exit(status);
            	} else {
            		fprintf( stderr, "\nwrite(generic) status = 0x%x, result = 0x%x cmd = %x\n", 
                       	status, sg_hd->result, i_buff[OFF] );
            	}
           	return status;
     	}
        
    	status = read( cd_fd, buff_p, OFF + out_size);

    	if ( status < 0 || status != OFF + out_size || sg_hd->result ) {
            	fprintf( stderr, "\nread(generic) status = 0x%x, result = 0x%x cmd = %x\n", 
			status, sg_hd->result, i_buff[OFF] );
            	fprintf( stderr, "\nread(generic) sense "
                     	"%x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x\n", 
             		sg_hd->sense_buffer[0], sg_hd->sense_buffer[1],
             		sg_hd->sense_buffer[2], sg_hd->sense_buffer[3],
             		sg_hd->sense_buffer[4], sg_hd->sense_buffer[5],
             		sg_hd->sense_buffer[6], sg_hd->sense_buffer[7],
             		sg_hd->sense_buffer[8], sg_hd->sense_buffer[9],
             		sg_hd->sense_buffer[10],sg_hd->sense_buffer[11],
             		sg_hd->sense_buffer[12],sg_hd->sense_buffer[13],
             		sg_hd->sense_buffer[14],sg_hd->sense_buffer[15] );
     	} else {
            	/* get output back */
            	if (out_size && out_size <= in_size + cmd_len)
                	memcpy(o_buff, i_buff, out_size + OFF);
	}

      	/* Look if we got what we expected to get */
       	if (status == OFF + out_size) status = 0; /* got them all */
    	return status;
}



/* Swap byteorder */

void *swaporder (void *p, int size) {
char *pc = p;
char tmp;

  	if (size == 4) {
    		tmp = pc [0];       /* reverse 4 bytes */
    		pc [0] = pc [3];
    		pc [3] = tmp;
    		tmp = pc [1];
    		pc [1] = pc [2];
    		pc [2] = tmp;
  	} else {
    		tmp = pc [0];       /* reverse 2 bytes */
    		pc [0] = pc [1];
    		pc [1] = tmp;
  	}

  	return p;
}


/* Check if CD is in Drive */
/* Returns 0 when ready, 1 when not ready */

int TestForMedium ( void ) {
char sensestatus;
unsigned char cmdblk [6] = { TEST_UNIT_READY, 0, 0, 0, 0, 0 };

  	memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  	if (handle_scsi_cmd(sizeof(cmdblk), 0, cmd, 0, NULL)) {
		FatalError("Test-Unit-Ready failed\n");
  	}

  	sensestatus=*(((struct sg_header*)cmd)->sense_buffer + 2);

  	if ((sensestatus & 0x0F) == NOT_READY) {
		/* Medium not loaded */
   		return 0;
  	}
  	return 1;
}

/* Read Toc of CD-ROM */

unsigned ReadToc ( ) {
int i,err;

        err=ioctl(blk_fd, CDROMREADTOCHDR, &hdr);
        if (err!=0) {
                fprintf(stderr, "can't get TocHeader (error %d).\n", err);
                exit (-1);
        }

        for (i=1; i <= hdr.cdth_trk1+1; i++) {
                if (i != hdr.cdth_trk1+1) 
                        entry[i].cdte_track = i;
                else    
                        entry[i].cdte_track = CDROM_LEADOUT;

                entry[i].cdte_format = CDROM_LBA;
                err=ioctl(blk_fd, CDROMREADTOCENTRY, &entry[i]);
                if (err!=0) {
                        fprintf(stderr, "can't get TocEntry #%d (error %d).\n", 
i, err);
                        exit (-1);
                }
        }

        return hdr.cdth_trk1;
}


/* Philips Speed select */

int set_speed_philips(int speed) {
unsigned char cmdblk [6 + 4 + 8] = { MODE_SELECT, 0x10, 0, 0, 12, 0,
				0, 0, 0, 0,
				0x23,6,0,0, 0,0,0,0 };
unsigned char *mode = cmd + OFF + 6;


	/* if no speed specified use default */
	if (speed == -1) return;

	memcpy(cmd+OFF, cmdblk, sizeof(cmdblk));
	mode[6] = speed;

  	if (handle_scsi_cmd (6, 12, cmd, 0, NULL))
        	perror ("Philips-Speed-Select failed\n");

}


/* Enable CDDA for Philips or HP Writers */

int EnableCddaPhilips_HP (int fAudioMode) {
/* MODE_SELECT, page = SCSI-2  save page disabled, reserved, reserved,
   parm list len, flags */
unsigned char cmdblk [6 + 12] = { 0x15, 0, 0, 0, 12, 0,
				0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0 };  

/* Pointer to paramlist of mode-select */ 
unsigned char *mode = cmd + OFF + 6;
  	
	memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  	if (fAudioMode) {
   		mode [10] = (CD_FRAMESIZE_RAW >> 8);   
    		mode [11] = (CD_FRAMESIZE_RAW & 0xFF);
  	} else {
    		mode [10] = (CD_FRAMESIZE >> 8);  
    		mode [11] = (CD_FRAMESIZE & 0xFF);
  	}

  	if (handle_scsi_cmd (6, 12, cmd, 0, NULL))
        	perror ("Audio mode switch failed\n");

	set_speed_philips(speed);

  	return 1; /* Swapbyteorder */
}


/* Yamaha Speed select */

int set_speed_yamaha(int speed) {
unsigned char cmdblk [6 + 4 + 4] = { MODE_SELECT, 0x10, 0, 0, 8, 0,
				0, 0, 0, 0,
				0x31,0x02,0,0 };
unsigned char *mode = cmd + OFF + 6;


	/* if no speed specified use default */
	if (speed == -1) return;

	memcpy(cmd+OFF, cmdblk, sizeof(cmdblk));
	mode[7] = (speed == 4 ? 0x20 : speed == 1 ? 0x00 : 0x10);

  	if (handle_scsi_cmd (6, 8, cmd, 0, NULL))
        	perror ("Yamaha-Speed-Select failed\n");

}


/* Enable CDDA for Yamaha-Writers */

int EnableCddaYamaha (int fAudioMode) {
/* MODE_SELECT, page = SCSI-2  save page disabled, reserved, reserved,
   parm list len, flags */
unsigned char cmdblk [6 + 12] = { 0x15, 0, 0, 0, 12, 0,
				0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0 };  

/* Pointer to paramlist of mode-select */ 
unsigned char *mode = cmd + OFF + 6;
  	
	memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  	if (fAudioMode) {
		mode [4] = 0; 		
   		mode [10] = (CD_FRAMESIZE_RAW >> 8);   
    		mode [11] = (CD_FRAMESIZE_RAW & 0xFF);
  	} else {
		mode [4] = 0;
    		mode [10] = (CD_FRAMESIZE >> 8);  
    		mode [11] = (CD_FRAMESIZE & 0xFF);
  	}

  	if (handle_scsi_cmd (6, 12, cmd, 0, NULL))
        	perror ("Audio mode switch failed\n");

	set_speed_yamaha(speed);

  	return 0; /* Not swapbyteorder */
}

/* Enable CDDA for Toshiba-CDROMs */

int EnableCddaToshiba (int fAudioMode) {
/* MODE_SELECT, page = SCSI-2  save page disabled, reserved, reserved,
   parm list len, flags */
unsigned char cmdblk [6 + 12] = { 0x15, 0, 0, 0, 12, 0,
				0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0 };  

/* Pointer to paramlist of mode-select */ 
unsigned char *mode = cmd + OFF + 6;
  	
	memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  	if (fAudioMode) {
		mode [4] = 0x82;	/* cdda density */
   		mode [10] = (CD_FRAMESIZE_RAW >> 8);   
    		mode [11] = (CD_FRAMESIZE_RAW & 0xFF);
  	} else {
		mode [4] = 0x00; 	/* normal density */
    		mode [10] = (CD_FRAMESIZE >> 8);  
    		mode [11] = (CD_FRAMESIZE & 0xFF);
  	}

  	if (handle_scsi_cmd (6, 12, cmd, 0, NULL))
        	perror ("Audio mode switch failed\n");

  	return 0; /* Not swapbyteorder */
}


/* Enable CDDA for Sony-Writers */

int EnableCddaSony (int fAudioMode) {
/* MODE_SELECT, page = SCSI-2  save page disabled, reserved, reserved,
   parm list len, flags */
unsigned char cmdblk [6 + 12 +4] = { 0x15, 0, 0, 0, 12+4, 0,
                                0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0,

                           /* Mode Page 0x31 (Drive Configuration) */
                           0x31, /* 6+4+8+0 Page Code */
                           0x02, /* 6+4+8+1 Parameter Length */
                           0x01, /* 6+4+8+2 2x Speed default */
                           0     /* 6+4+8+3 reserved */
                           };

/* Pointer to paramlist of mode-select */ 
unsigned char *mode = cmd + OFF + 6;
        
        memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

	if (speed != -1) {
		mode [14] = (speed == 2 ? 0x01 : 0x00);
	}

        if (fAudioMode) {               
                mode [10] = (CD_FRAMESIZE_RAW >> 8);   
                mode [11] = (CD_FRAMESIZE_RAW & 0xFF);
		sonyRawNormal = 0;
        } else {
                mode [10] = (CD_FRAMESIZE >> 8);  
                mode [11] = (CD_FRAMESIZE & 0xFF);
		sonyRawNormal = 1;
        }

        if (handle_scsi_cmd (6, 12+4, cmd, 0, NULL))
                perror ("Audio mode switch failed\n");

        return 0; /* NOT swapbyteorder */
}


/* Read max. SectorBurst of cdda sectors to bufferCdda+OFF */

void Read6 (long lSector, unsigned long SectorBurst) {

/* READ6, block1 msb, block2, block3, block4 lsb,
   transfer len, block addressing mode */
unsigned char cmdblk [6] = {0x08, 0, 0, 0, 0, 0};
unsigned char *cmd0 = cmd + OFF;

  	memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  	cmd0 [1] = (unsigned char)((lSector >> 16) & 0xFF);
  	cmd0 [2] = (unsigned char)((lSector >> 8) & 0xFF);
  	cmd0 [3] = (unsigned char)(lSector & 0xFF);
  	cmd0 [4] = (unsigned char)SectorBurst;

  	if (handle_scsi_cmd(sizeof(cmdblk), 0, cmd,
        	SectorBurst * CD_FRAMESIZE_RAW, bufferCddaRaw ))
    		FatalError ("Read CD-ROM6 failed\n");
}

/* Read max. SectorBurst of cdda sectors to bufferCdda+OFF */

void Read12 (long lSector, unsigned long SectorBurst) {

/* READ12, block1 msb, block2, block3, block4 lsb,
   transfer len, block addressing mode */
unsigned char cmdblk [12] = {0xd8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
unsigned char *cmd0 = cmd + OFF;

  	memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  	cmd0 [3] = (unsigned char)((lSector >> 16) & 0xFF);
  	cmd0 [4] = (unsigned char)((lSector >> 8) & 0xFF);
  	cmd0 [5] = (unsigned char)(lSector & 0xFF);
  	cmd0 [9] = (unsigned char)SectorBurst;

	if (sonyRawNormal) 
		cmd0 [10] = 0x02;	/* normal density */

  	if (handle_scsi_cmd(sizeof(cmdblk), 0, cmd,
        	SectorBurst * CD_FRAMESIZE_RAW, bufferCddaRaw ))
    		FatalError ("Read CD-ROM12 failed\n");
}

/* request vendor brand and model */

unsigned char *Inquiry ( void ) {
static unsigned char Inqbuffer[sizeof(struct sg_header) + 56];
unsigned char cmdblk [6] = {0x12, 0, 0, 0, 56, 0};

  	memcpy( cmd + OFF, cmdblk, sizeof(cmdblk) );

  	if (handle_scsi_cmd(sizeof(cmdblk), 0, cmd, sizeof(Inqbuffer) - OFF,
                            Inqbuffer )) {
      		FatalError ("Inquiry failed\n");
  	}
  	return (Inqbuffer + OFF);
}


/* Open generic-cdrom device */

int OpenCdRom ( char *dev_name ) {
int cdfd;

	/* Open Read/Write because its a generic SCSI-Device */
      	cdfd = open(dev_name, O_RDWR);

  	if (cdfd < 0) {
  		perror("open");
                FatalError("Failed to open %s\n",dev_name);
	}

  	return cdfd;
}


/* Open block-cdrom device */

int OpenCdRom2 ( char *dev_name ) {
int cdfd;

        cdfd = open(dev_name, O_RDONLY);

        if (cdfd < 0) {
                perror("open");
                fprintf(stderr,"Failed to open %s\n",dev_name);
                exit(1);
        }

        return cdfd;
}


/* Check how many sectors fit inside the generic-scsi-buffer */

void Setnrsectors() {

        nsectors = NSECTORS;
#ifdef SG_BIG_BUFF
        if (nsectors * CD_FRAMESIZE_RAW + OFF > SG_BIG_BUFF) {
            nsectors = (SG_BIG_BUFF - OFF)/CD_FRAMESIZE_RAW;
            fprintf(stderr,
                    "setting nsectors to %d, so that transfer size < %d\n",
                     nsectors, SG_BIG_BUFF);
	}
#else
        if (nsectors * CD_FRAMESIZE_RAW + OFF > 4096) {
            nsectors = 1;
            fprintf(stderr,
                    "setting nsectors to %d, so that transfer size < %d\n",
                    nsectors, 4096);
	}
#endif
}

void SetupSCSI() {

unsigned char *p = Inquiry();

    	if (*p != 0x05 && *p != 0x04) {    
        	fprintf(stderr, "Error: This is no scsi cdrom\n");
        	exit(1);
    	}
    	if (!memcmp(p+8,"TOSHIBA", 7) ||
	    !memcmp(p+8,"PLEXTOR",7)) {
         /* setup pointer */
        	EnableCdda = EnableCddaToshiba;
        	ReadCdRom = Read6;
    	} else
    	if (!memcmp(p+8,"YAMAHA", 6)) {
         /* setup pointer */
       	 	EnableCdda = EnableCddaYamaha;
        	ReadCdRom = Read12;
    	} else
    	if (!memcmp(p+8,"IMS", 3) ||
            !memcmp(p+8,"PHILIPS", 7) ||
            !memcmp(p+8,"PLASMON", 7) ||
	    !memcmp(p+8,"HP", 2) ||
            !memcmp(p+8,"GRUNDIG CDR100IPW",17) ||
            !memcmp(p+8,"MITSUMI CD-R ",13) ||
            !memcmp(p+8,"KODAK", 5)) {
        	EnableCdda = EnableCddaPhilips_HP;
        	ReadCdRom = Read6;
	} else 
	if (!memcmp(p+8,"RICOH",5)) {
		EnableCdda = EnableCddaPhilips_HP;
		ReadCdRom = Read12;
	} else 
	if (!memcmp(p+8,"SONY",4)) {
		EnableCdda = EnableCddaSony;
		ReadCdRom = Read12;
	} else {
		fprintf(stderr, "Error: Unsupported device\n");
		exit(1);
	}

	if (! TestForMedium()) {
		FatalError("Error: Medium not loaded.\n");
	}
}


/* Display the data in the g_toc stucture in readable format */

int DisplayToc () {
unsigned i,tmp;
unsigned long dw;
char tmpstr[10];
int data = 0;
int audio = 0;

  	/* g_toc [i].bFlags contains two fields:
         bits 7-4 (ADR) : 0 no sub-q-channel information
                        : 1 sub-q-channel contains current position
                        : 2 sub-q-channel contains media catalog number
                        : 3 sub-q-channel contains International Standard
                                                   Recording Code ISRC
                        : other values reserved
         bits 3-0 (Control) :
         bit 3 : when set indicates there are 4 audio channels else 2 channels
         bit 2 : when set indicates this is a data track else an audio track
         bit 1 : when set indicates digital copy is permitted else prohibited
         bit 0 : when set indicates pre-emphasis is present else not present
  	*/

	/* Convert total-frames to mins:sec:centi_sec */
        dw = (unsigned long) entry[tracks+1].cdte_addr.lba;
        convertframes2time(dw,tmpstr);

  	printf ( "Table of Contents:\nTracks:%u\nTime  :%s\n", tracks, tmpstr);

    	puts( "track pre-emphasis copy-permitted tracktype channels sub-Q-channel size" );
  	for ( i = 1; i <= tracks; i++ ) {
   		printf("%2d %12s %14s %18s ",entry[i].cdte_track,
                        entry[i].cdte_ctrl & 1 ? "yes" : "no",
                        entry[i].cdte_ctrl & 2 ? "yes" : "no",
                        entry[i].cdte_ctrl & 4 ? "Data  1       " : 
                        entry[i].cdte_ctrl & 8 ? "Audio 4       " :
                                                 "Audio 2       " );

                switch ( entry[i].cdte_adr ) {
                        case 0:  printf("no_data      " ); break;
                        case 1:  printf("position     " ); break;
                        case 2:  printf("media_catalog" ); break;
                        case 3:  printf("ISRC         " ); break;
                        default: printf( "invalid_0x%2x\n", entry[i].cdte_ctrl);
                }

                dw = (unsigned long) (entry[i+1].cdte_addr.lba - 
                                      entry[i].cdte_addr.lba);

                if ( entry[i].cdte_ctrl & CDROM_DATA_TRACK) {
			printf(" %3d MB\n",convertframes2mb(dw));
			data++;
		} else {
			convertframes2time(dw,tmpstr);
      			printf (" %s\n", tmpstr);
			audio++;
		}
    	}

	if (data != 0 && audio != 0) {
		/* Mixed-Mode */
		tmp=3;
	} else 
	if (data != 0 && audio == 0) {
		/* Data-CD */
		tmp=2;
	} else
	if (data == 0 && audio != 0) {
		/* Audio-CD */
		tmp=1;
	} else 
		/* Invalid */
		tmp=0;

	return tmp;
}


/* Open the sound-device: 16-bit, 44.1 kHz, Stereo */

void OpenSound() {
int tmp;

  	if (echo) {
    		if ((sndfd=open(sname, O_WRONLY, 0)) == EOF) {
        		FatalError("Cannot open %s\n",sname);
    		}
		
		tmp = 16;
 		ioctl(sndfd, SNDCTL_DSP_SAMPLESIZE, &tmp);
  		if (tmp != 16) {
        		FatalError("Unable to set 16-bit sample size\n");
  		}

		tmp = 1;
  		if (ioctl(sndfd, SNDCTL_DSP_STEREO, &tmp)==-1) {
      			FatalError("Unable to set stereo\n");
  		}

		tmp = 44100;
  		if (ioctl(sndfd, SNDCTL_DSP_SPEED, &tmp) == -1) {
			FatalError("Unable to set to 44.1 kHz\n");
    		}
    	}
}

/* Print Usage-Screen */

void usage() {

	fprintf(stderr,"\
Usage: readcdda [options] [cdrfile.cdr]\n\
Options:\n\
	-D device   : generic cdrom device\n\
	-B device   : block cdrom device\n\
	-t track    : start track\n\
	-z track    : end track (included)\n\
	-o offset   : offset on beginning of track (in frames)\n\
	-s offset   : offset on end of track (in frames)\n\
	-d duration : recording time in seconds\n\
	-e device   : echo audio data to device (will read single speed)\n\
	-S speed    : read-speed (1,2,4 ..)\n\
	-n	    : no file operation\n\
	-q	    : quiet operation\n\
	-v	    : print information about cd\n\
	-x	    : swap byte order on file (little-endian)\n\
	-y	    : swap byte order on sound-device\n\
	-p	    : output read-progress in percent\n\
	-T	    : tcl-mode\n\
");

}


/* Display seconds played until now */

void tickdisplay(long sector,int force) {

	tick=sector/75;
	if ((tick != oldtick) || force) {
		printf("%c%d\n",doplay?'p':'s',tick);
		fflush(stdout);
		oldtick=tick;
	}
	
}

/* Do the reading of the audio data */

void main(int argc, char **argv) {
long lSector;
long lSector_p1;
long sector_offset1 = 0;
long sector_offset2 = 0;
long nSectorsToDo = 0;
long SectorBurst;
long SectorCount;
double time = 0.0;
char tmpstr[10];
int i;
int c;

	while ((c=getopt(argc,argv,"D:B:t:z:o:s:d:e:S:nqvxypT")) != EOF) {
		switch(c) {

		case 'D':
			strcpy(dname,optarg);
			break;

		case 'B':
			strcpy(blkname,optarg);
			break;

		case 't':
			track = atoi(optarg);
			break;

		case 'z':
			endtrack = atoi(optarg);
			break;

		case 'o':
			sector_offset1 = atol(optarg);
			break;

		case 's':
			sector_offset2 = atol(optarg);
			break;
	
		case 'd':
			time = strtod(optarg, NULL);
			break;

		case 'e':
			strcpy(sname,optarg);
			echo = 1;
			break;

		case 'S':
			speed = atoi(optarg);
			break;
	
		case 'n':
			no_file = 1;
			break;

		case 'q':
			quiet = 1;
			break;

		case 'v':
			verbose = 1;
			break;
 
		case 'x':
			swapendian = 1;
			break;

		case 'y':
			swapaudio = 1;
			break;

		case 'p':
			percent = 1;
			break;

		case 'T':
			verbose = 0;
			quiet = 1;
			tclmode = 1;
			break;

		default:
			usage();
			exit(-1);
		}
	}

	/* Arguments without options, in this case the filename */
	if (optind < argc) {
		strcpy(fname,argv[optind]);
	}

	/* No arguments at all? */
	if (argc == 1) {
		usage();
		exit(-1);
	}

	/* if echo is enabled, set speed to 1x */
	if (echo) 
		speed = 1;


	signal(SIGINT, sigint_handle); 
	atexit(CloseAll);

	Setnrsectors();
  	bufferCddaRaw = malloc (OFF + nsectors * CD_FRAMESIZE_RAW);
    	bufferCdda = bufferCddaRaw + OFF;

	cmd = malloc (OFF+36);
	cd_fd = OpenCdRom (dname);
	blk_fd = OpenCdRom2 (blkname);

	SetupSCSI();

	tracks = ReadToc();
	if (verbose) DisplayToc();

  	if (!FirstTrack ())
    		FatalError ("This is no audio disk\n");

	if (EnableCdda(1)) {
		/* If the device needs byte-swapping */
		swapendian=1-swapendian;
		swapaudio=1-swapaudio;
	}

	/* No endtrack spezified? */
	if (endtrack == -1) {
		endtrack = track;
	}

	lSector = GetStartSector(track);
	lSector_p1 = GetEndSector(endtrack)+1;

	if (lSector < 0) 
		FatalError ("Track %d not found\n",track);

	lSector += sector_offset1;
	lSector_p1 += sector_offset2;

  	if ( lSector >= lSector_p1 ) {
    		printf("Sector offset exceeds track size (ignored)\n");
    		lSector -= sector_offset1;
		lSector_p1 -= sector_offset2;
  	}

	/*
	printf("Track: %d, StartSector: %ld, Length: %ld\n",track,lSector,lSector_p1-lSector);
	*/

  	if ( time == 0.0 ) {
    		/* set time to track time */
    		nSectorsToDo = lSector_p1 - lSector;
    		time = nSectorsToDo / 75.0;
  	} else {
    		if ( time > 4440.0 ) {
      		/* Prepare the maximum recording duration.
       		 * It is defined as the biggest amount of
       		 * adjacent audio sectors beginning with the
       		 * specified track/index/offset. */

      			for (i = track; i < tracks; i++) {
        			/* break if a nonaudio track follows */
        			if ( entry[i-1].cdte_ctrl & CDROM_DATA_TRACK ) break;
        			lSector_p1 = GetEndSector (i)+1;
      			}
      			time = (lSector_p1 - lSector) / 75.0;
    		}

    		/* calculate # of sectors to read */
    		nSectorsToDo = (long)(time * 75.0 + 0.5);
  	}

	OpenSound();

	if (!quiet) {
		convertframes2time(nSectorsToDo,tmpstr);
		printf("Recording Track %d",track);
		if (endtrack != track) {
			printf("-%d",endtrack);
		}
		printf(": %s minutes.\n",tmpstr);
		if (!no_file) {
			printf("Output-file: %s\n",fname);
		}
	}

	if (!no_file) OpenAudio(fname);

	if (tclmode) {

	/* Tcl-operation */
        fcntl(STDIN_FILENO,F_SETFL,O_NONBLOCK); /* stdin must be nonblocking! */
	SectorCount = 0;
	doplay = 1;
	printf("%d\n",nSectorsToDo/75);
	tickdisplay(SectorCount,1);

	while (1) {
		
		/* Watch for new commands in pipe */

           	if (gets(keybuffer) != NULL) { 
            		if (strcmp(keybuffer,"stop") == 0) {
                        	SectorCount=0; doplay=0;
				tickdisplay(SectorCount,1);
			}
			if (strcmp(keybuffer,"play") == 0) { 
				doplay=1; 
			}
            		if (strcmp(keybuffer,"pause") == 0) {
                        	doplay=1-doplay;
				tickdisplay(SectorCount,1);
                	}	
  			if (strncmp(keybuffer,"set",3) == 0) {
                		SectorCount=atoi(keybuffer+3)*75;
				tickdisplay(SectorCount,1);
			}
   			if (strcmp(keybuffer,"quit") == 0) { 
				break; 
			}
		}
	
		i = nSectorsToDo-SectorCount;	/* How much sectors left */
		if (doplay && i) {
			/* How many Sectors can be read in one command, 
			   without exceding track-limits? */
			SectorBurst = i > nsectors ? nsectors : i;
			ReadCdRom (lSector+SectorCount, SectorBurst);

			if (SaveBuffer(i, nSectorsToDo)) {
				FatalError("\nDisk space exhausted\n");
			}
			tickdisplay(SectorCount,0);

			SectorCount+=SectorBurst;
		} else {
			usleep(100); 	/* Save CPU-Resources in loop */
		}

		/* Reached end of file */
		if ( (SectorCount >= nSectorsToDo) && doplay ) {
			doplay=0;
			SectorCount = nSectorsToDo;
			tickdisplay(SectorCount,1);
		}
	}

	} else {
	/* Normal operation-mode */

  	for (i = nSectorsToDo; i > 0; i -= SectorBurst) {

		SectorBurst = i > nsectors ? nsectors : i;
		ReadCdRom (lSector, SectorBurst);

		if (SaveBuffer(i, nSectorsToDo)) {
			FatalError("\nDisk space exhausted\n");
		}

		lSector+=SectorBurst;
	}
	if (!quiet) printf("\n");
	fprintf(stdout,"end\n");
	}

	CloseAll();
	exit(0);
}

