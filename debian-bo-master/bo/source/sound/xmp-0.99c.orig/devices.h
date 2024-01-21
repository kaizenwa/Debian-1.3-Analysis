/* $Id: devices.h,v 1.2 1997/03/13 13:25:14 claudio Exp $
 *
 * $Log: devices.h,v $
 * Revision 1.2  1997/03/13 13:25:14  claudio
 * Function interfaces changed for xmp 0.99.
 *
 * Revision 1.1  1996/12/07 17:48:54  claudio
 * Initial revision
 *
 */

#ifndef __XMP_DEVICES_H
#define __XMP_DEVICES_H

#include "xxm.h"

#define XMP_PAT_16BIT	0x0001
#define XMP_PAT_SIGNED	0x0002
#define XMP_PAT_LOOP	0x0004
#define XMP_PAT_BILOOP	0x0008

enum dev_num { NONE, GUS, AWE, SOFTMIX };

struct dev_info {
    int num;
    int dev;
    int pch;
    char *id;
    char *name;
    struct dev_info *next;
};

int register_device(int,char *,char *,int);
void list_devices(char *);
struct dev_info *get_device(char *);
void device_numvoices(int,struct dev_info *);
void device_voicepos(int,int,struct dev_info *);
void device_writepatch(FILE *,struct dev_info *,int,int,int,
    struct xxm_sample *);

void seq_starttimer(void);
void seq_stoptimer(void);
void seq_setnote(struct dev_info *,int,int);
void seq_setvol(struct dev_info *,int,int);
void seq_setpatch(struct dev_info *,int,int);
void seq_setpan(struct dev_info *,int,int);
void seq_setbend(struct dev_info *,int,int);
void seq_echoback(int);
int seq_getmsg(void);

void seq_clearmem(struct dev_info *);
int seq_getfreemem(struct dev_info *);
void seq_mainvolume(int,int,int);
void seq_sync(double);
void seq_resetvoices(struct dev_info *);
void seqbuf_dump(void);

#endif /* __XMP_DEVICES_H */

