#ifndef __cdrom_h_
#define __cdrom_h_

#include <mntent.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <fcntl.h>

#include <linux/cdrom.h>
#include <linux/errno.h>

extern void	set_cd_device _((Window *, char *, int));
extern void cd_stop _((char *, char *, char *));
extern void cd_eject _((char *, char *, char *));
extern void cd_play _((char *, char *, char *));
extern void cd_list _((char *, char *, char *));
extern void cd_volume _((char *, char *, char *));
extern void cd_pause _((char *, char *, char *));
extern void cd_help _((char *, char *, char *));

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
