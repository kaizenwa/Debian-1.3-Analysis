#ifndef __cdrom_h_
#define __cdrom_h_

#include <mntent.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <fcntl.h>

#include <linux/cdrom.h>
#include <linux/errno.h>

struct cdrom_etocentry 
{
	u_char	cdte_track;
	u_char	cdte_adr	:4;
	u_char	cdte_ctrl	:4;
	u_char	cdte_format;
	union cdrom_addr cdte_addr;
	u_char	cdte_datamode;
	int avoid;
	int length;
};

#endif
