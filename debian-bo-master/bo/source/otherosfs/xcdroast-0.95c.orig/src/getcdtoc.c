/* getidetoc.c: Get the TOC and Type of a NON-SCSI-CD */
/* 21.5.96 T. Niederreiter */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <fcntl.h>
#include <unistd.h>
#include <linux/cdrom.h>

int cd_fd;
static struct cdrom_tochdr hdr;
static struct cdrom_tocentry entry[101];
static struct cdrom_read_audio arg;


/* Read Toc of CD-ROM */

unsigned ReadToc ( ) {
int i,err;

	err=ioctl(cd_fd, CDROMREADTOCHDR, &hdr);
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
      		err=ioctl(cd_fd, CDROMREADTOCENTRY, &entry[i]);
      		if (err!=0) {
          		fprintf(stderr, "can't get TocEntry #%d (error %d).\n", i, err);
          		exit (-1);
        	}
    	}

	return hdr.cdth_trk1;
}


/* Open cdrom device */

int OpenCdRom ( char *dev_name ) {
int cdfd;

      	cdfd = open(dev_name, O_RDONLY);

  	if (cdfd < 0) {
  		perror("open");
                fprintf(stderr,"Failed to open %s\n",dev_name);
                exit(1);
	}

  	return cdfd;
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


/* Display the data in the g_toc stucture in readable format */

int DisplayToc ( int tracks ) {
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

  	printf ( "#Table of Contents:\nTracks:%u\nTime  :%s\n", tracks, tmpstr);

    	puts( "#track pre-emphasis copy-permitted tracktype channels sub-Q-channel size" );
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
			printf(" %3d %ld\n",convertframes2mb(dw),dw);
			data++;
		} else {
			convertframes2time(dw,tmpstr);
      			printf (" %s %ld\n", tmpstr,dw);
			audio++;
		}
    	}
	puts("#");

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


/* Help-funktion for finding empty chars */

int empty(char c) {

    	return (c == 0 || c == ' ');
}


/* Extract text-string from first block of a CD-Rom */

void printnsp(int begin, int end, char *buf, char *label) {
int i,j,k;

	/* First erase label-string */
	strcpy(label,"");
    	for(i=begin; i<end; i++) {
        	if (empty(buf[i]))
          		continue;
        	for(j=i+1; j<end; j++)
          		if (!buf[j] || (j < end-1 && empty(buf[j]) 
				&& empty(buf[j+1]))) break;
        	for(k=i; k<j; k++)
			sprintf(label,"%s%c",label,buf[k]);
        	i = j;
    	}
}


/* 
 Looks in first block of an block-device and checks if there is an ISO9660-
 filesystem on it. This is done by checking for a string "CD001".
*/

int getcdlabel(char *blkdev, char *label) {

unsigned char buf[1024];
int c;
int tmp;

	tmp = 0;

    	if (lseek(cd_fd, 32768, SEEK_SET) < 0) {
        	perror("lseek");
        	exit(1);
    	}
    	if (read(cd_fd, buf, sizeof(buf)) != sizeof(buf)) {
        	perror("read");
        	exit(1);
    	}
    	if (strncmp(buf, "\001CD001\001", 8) != 0) {
		/* Not a ISO9660-FS */
		return 0;
        }
    	else {
		/* ISO9660-FS, get volume-id */
		printnsp(40,72,buf,label);
		return 1;
    	}
}

void QueryType(int type,char *blkdev) {
char label[33];

	switch (type) {
		case 3:
		/* Mixed-Mode-CD */
		if (getcdlabel(blkdev,label) == 1) {		
			printf("Type : ISO-Mixed-Mode\n");
			printf("Label: %s\n",label);
		} else {
			printf("Type : Unknown-Mixed-Mode\n");
			printf("Label: N/A\n");
		}
		break;

		case 2:
		/* Data-CD */
		if (getcdlabel(blkdev,label) == 1) {		
			printf("Type : ISO9660-Data\n");
			printf("Label: %s\n",label);
		} else {
			printf("Type : Unknown-Data\n");
			printf("Label: N/A\n");
		}
		break;

		case 1:
		/* Audio-CD */
			printf("Type : Audio\n");
			printf("Label: N/A\n");
		break;
			 
		default:
			printf("Type : Unknown\n");
			printf("Label: N/A\n");
	}			
}


void main(int argc, char **argv) {
char blkdev[1024];
int tracks,type;
int drivestat;

	if (argc != 2) {
		printf("Usage: %s <block-CD-device>\n", argv[0]);
		exit(0);
	} else {
		strcpy(blkdev,argv[1]);
	}

	cd_fd = OpenCdRom (blkdev);

	tracks = ReadToc();
	type = DisplayToc(tracks);
 	QueryType(type,blkdev);	

	close(cd_fd);
	exit(0);
}

