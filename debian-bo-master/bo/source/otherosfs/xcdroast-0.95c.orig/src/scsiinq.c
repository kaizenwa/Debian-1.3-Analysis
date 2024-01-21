/* scsiinq.c: Inquire SCSI-Devices */
/* 13.1.96 T.Niederreiter */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <scsi/sg.h>

#define SCSI_OFF sizeof(struct sg_header)

unsigned char cmd[SCSI_OFF + 18];      /* SCSI command buffer */
int fd;                                /* SCSI device/file descriptor */

/* process a complete scsi cmd. Use the generic scsi interface. */
int handle_scsi_cmd(unsigned cmd_len,         /* command length */
                           unsigned in_size,         /* input data size */
                           unsigned char *i_buff,    /* input buffer */
                           unsigned out_size,        /* output data size */
                           unsigned char *o_buff     /* output buffer */
                           ) {
int status = 0;
struct sg_header *sg_hd;

    	/* safety checks */
    	if (!cmd_len) return -1;            /* need a cmd_len != 0 */
    	if (!i_buff) return -1;             /* need an input buffer != NULL */
#ifdef SG_BIG_BUFF
    	if (SCSI_OFF + cmd_len + in_size > SG_BIG_BUFF) return -1;
    	if (SCSI_OFF + out_size > SG_BIG_BUFF) return -1;
#else
    	if (SCSI_OFF + cmd_len + in_size > 4096) return -1;
    	if (SCSI_OFF + out_size > 4096) return -1;
#endif

    	if (!o_buff) out_size = 0;

    	/* generic scsi device header construction */
    	sg_hd = (struct sg_header *) i_buff;
    	sg_hd->pack_len    = SCSI_OFF + cmd_len + in_size;
    	sg_hd->reply_len   = SCSI_OFF + out_size;
    	sg_hd->twelve_byte = cmd_len == 12;
	sg_hd->result = 0;
#if     0
    	sg_hd->pack_id;     /* not used */
    	sg_hd->other_flags; /* not used */
#endif

    	/* send command */
    	status = write( fd, i_buff, SCSI_OFF + cmd_len + in_size );
    	if ( status < 0 || status != SCSI_OFF + cmd_len + in_size ||
                       sg_hd->result ) {
        	/* some error happened */
        	fprintf( stderr, "write(generic) result = 0x%x cmd = 0x%x\n",
                    	sg_hd->result, i_buff[SCSI_OFF] );
        	perror("");
        	return status;
    	}	

    	if (!o_buff) o_buff = i_buff;       /* buffer pointer check */

    	/* retrieve result */
    	status = read( fd, o_buff, SCSI_OFF + out_size);
    	if ( status < 0 || status != SCSI_OFF + out_size || sg_hd->result ) {
        	/* some error happened */
        	fprintf( stderr, "read(generic) result = 0x%x cmd = 0x%x\n",
                	sg_hd->result, o_buff[SCSI_OFF] );
        	fprintf( stderr, "read(generic) sense "
                	"%x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x\n",
                	sg_hd->sense_buffer[0],         sg_hd->sense_buffer[1],
                	sg_hd->sense_buffer[2],         sg_hd->sense_buffer[3],
                	sg_hd->sense_buffer[4],         sg_hd->sense_buffer[5],
                	sg_hd->sense_buffer[6],         sg_hd->sense_buffer[7],
                	sg_hd->sense_buffer[8],         sg_hd->sense_buffer[9],
                	sg_hd->sense_buffer[10],        sg_hd->sense_buffer[11],
                	sg_hd->sense_buffer[12],        sg_hd->sense_buffer[13],
                	sg_hd->sense_buffer[14],        sg_hd->sense_buffer[15]);
        	perror("");
    	}
    	/* Look if we got what we expected to get */
    	if (status == SCSI_OFF + out_size) status = 0; /* got them all */

    	return status;  /* 0 means no error */
}

#define INQUIRY_CMD     0x12
#define INQUIRY_CMDLEN  6
#define INQUIRY_REPLY_LEN 96

/* request vendor brand and model */
unsigned char *Inquiry ( void ) {
static  unsigned char Inqbuffer[ SCSI_OFF + INQUIRY_REPLY_LEN ];
unsigned char cmdblk [ INQUIRY_CMDLEN ] =
      { INQUIRY_CMD,  /* command */
                  0,  /* lun/reserved */
                  0,  /* page code */
                  0,  /* reserved */
  INQUIRY_REPLY_LEN,  /* allocation length */
                  0 };/* reserved/flag/link */

  	memcpy( cmd + SCSI_OFF, cmdblk, sizeof(cmdblk) );

  	if (handle_scsi_cmd(sizeof(cmdblk), 0, cmd,
                      sizeof(Inqbuffer) - SCSI_OFF, Inqbuffer )) {
      		fprintf( stderr, "Inquiry failed\n" );
      		exit(2);
  	}
  	return (Inqbuffer + SCSI_OFF);
}

void main( void )
{
int i;
char device[]="/dev/sgx";
char *inqresult;

  	for (i=0;i<8;i++) {
   		device[7]='a'+i;      /* Create generic SCSI device names */

  		fd = open(device, O_RDWR);
  		if (fd < 0) {
   			 printf( "x\n" );
   			 continue;
  		}

  		inqresult=Inquiry();
  		inqresult[0x2c]='\0';

  		close(fd);
 	
  		printf("%1x%1x%s\n",inqresult[0],(inqresult[1]&0x80)?1:0,inqresult+8);

  	}
 	exit (0);
}

