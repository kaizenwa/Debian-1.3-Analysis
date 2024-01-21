/*
 * scsi.c
 * Iomega Zip/Jaz drive tool
 * change protection mode and eject disk
 */

/* scis.c by Markus Gyger <mgyger@itr.ch> */
/* This code is based on ftp://gear.torque.net/pub/ziptool.c */
/* by Grant R. Guenther with the following copyright notice: */

/*  (c) 1996   Grant R. Guenther,  based on work of Itai Nahshon  */
/*  http://www.torque.net/ziptool.html  */


/* A.K. Moved this from mzip.c to a separate file in order to share with
 * plain_io.c */

#include "sysincludes.h"
#include "mtools.h"
#include "scsi.h"

#if defined hpux
#include <sys/scsi.h>
#endif

#ifdef solaris
#include <sys/scsi/scsi.h>
#endif /* solaris */

#ifdef sunos3
#include <scsi/generic/commands.h>
#include <scsi/impl/uscsi.h>
#endif /* sunos3 */

#ifdef linux
#define SCSI_IOCTL_SEND_COMMAND 1
struct scsi_ioctl_command {
    int  inlen;
    int  outlen;
    char cmd[5008];
};
#endif


int scsi_max_length(void)
{
#ifdef linux
	return 8;
#else
	return 255;
#endif
}

int scsi_cmd(int fd, unsigned char *cdb, int cmdlen, scsi_io_mode_t mode,
	     void *data, size_t len)
{
#if defined hpux
	struct sctl_io sctl_io;
	
	memset(&sctl_io, 0, sizeof sctl_io);   /* clear reserved fields */
	memcpy(sctl_io.cdb, cdb, cmdlen);      /* copy command */
	sctl_io.cdb_length = cmdlen;           /* command length */
	sctl_io.max_msecs = 2000;              /* allow 2 seconds for cmd */

	switch (mode) {
		case SCSI_IO_READ:
			sctl_io.flags = SCTL_READ;
			sctl_io.data_length = len;
			sctl_io.data = data;
			break;
		case SCSI_IO_WRITE: 
			sctl_io.flags = 0;
			sctl_io.data_length = data ? len : 0;
			sctl_io.data = len ? data : 0;
			break;
	}

	if (ioctl(fd, SIOC_IO, &sctl_io) == -1) {
		perror("scsi_io");
		return -1;
	}

	return sctl_io.cdb_status;
	
#elif defined sunos3 || defined solaris
	struct uscsi_cmd uscsi_cmd;
	memset(&uscsi_cmd, 0, sizeof uscsi_cmd);
	uscsi_cmd.uscsi_cdb = (char *)cdb;
	uscsi_cmd.uscsi_cdblen = cmdlen;
#ifdef solaris
	uscsi_cmd.uscsi_timeout = 20;  /* msec? */
#endif /* solaris */
	
	uscsi_cmd.uscsi_buflen = (u_int)len;
	uscsi_cmd.uscsi_bufaddr = data;

	switch (mode) {
		case SCSI_IO_READ:
			uscsi_cmd.uscsi_flags = USCSI_READ;
			break;
		case SCSI_IO_WRITE:
			uscsi_cmd.uscsi_flags = USCSI_WRITE;
			break;
	}

	if (ioctl(fd, USCSICMD, &uscsi_cmd) == -1) {
		perror("scsi_io");
		return -1;
	}

	if(uscsi_cmd.uscsi_status) {
		errno = 0;
		fprintf(stderr,"scsi status=%x\n",  
			(unsigned short)uscsi_cmd.uscsi_status);
		return -1;
	}
	
	return 0;
	
#elif defined linux
	struct scsi_ioctl_command scsi_cmd;


	memcpy(scsi_cmd.cmd, cdb, cmdlen);        /* copy command */

	switch (mode) {
		case SCSI_IO_READ:
			scsi_cmd.inlen = 0;
			scsi_cmd.outlen = len;
			break;
		case SCSI_IO_WRITE:
			scsi_cmd.inlen = len;
			scsi_cmd.outlen = 0;
			memcpy(scsi_cmd.cmd + cmdlen,data,len);
			break;
	}
	
	if (ioctl(fd, SCSI_IOCTL_SEND_COMMAND, &scsi_cmd) < 0) {
		perror("scsi_io");
		return -1;
	}
	
	switch (mode) {
		case SCSI_IO_READ:
			memcpy(data, &scsi_cmd.cmd[0], len);
			break;
		case SCSI_IO_WRITE:
			break;
    }

	return 0;  /* where to get scsi status? */
	
#else
	fprintf(stderr, "scsi_io not implemented\n");
	return -1;
#endif
}
