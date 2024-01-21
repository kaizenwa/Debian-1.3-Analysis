/* load.h - common loader extern data and code
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: load.h,v 1.3 1997/03/20 02:31:01 claudio Exp $
 *
 * $Log: load.h,v $
 * Revision 1.3  1997/03/20 02:31:01  claudio
 * Track allocation macro changed to recognize tracks with 256 rows.
 *
 * Revision 1.2  1997/03/13 13:19:54  claudio
 * Lots of changes (more macros, includes and externs).
 *
 * Revision 1.1  1997/01/26 22:41:57  claudio
 * Initial revision
 *
 */

#ifndef __LOAD_H
#define __LOAD_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xmp.h"
#include "xxm.h"
#include "devices.h"

extern char module_name[MODULE_NAME_MAXSIZE];
extern struct options opt;
extern double replay_rate;
extern int c4_rate;
extern int vol_base;
extern int *vol_xlat;

extern struct xxm_header *xxh;
extern struct xxm_pattern **xxp;
extern struct xxm_track **xxt;
extern struct xxm_channel xxc[32];
extern uint8 xxo[256];
extern struct xxm_instrument_header *xxih;
extern struct xxm_instrument_map *xxim;
extern struct xxm_instrument **xxi;
extern struct xxm_sample *xxs;
extern uint16 **xxae;
extern uint16 **xxpe;
extern uint16 **xxfe;

static char module_type[80];

#define LOAD_INIT() { \
    memset(xxh,0,sizeof(*xxh)); \
    memset(module_name,0,MODULE_NAME_MAXSIZE); \
    fseek(f,0,SEEK_SET); \
}

#define MODULE_INFO() { \
    if(opt.verbose) { \
	if(*module_name) report("Module title   : %s\n",module_name); \
        report("Module type    : %s\n",module_type); \
        if(xxh->len) report("Module length  : %d patterns\n",xxh->len); \
    } \
}

#define INSTRUMENT_INIT() { \
    xxih=calloc(sizeof(struct xxm_instrument_header),xxh->ins); \
    xxim=calloc(sizeof(struct xxm_instrument_map),xxh->ins); \
    xxi=calloc(sizeof(struct xxm_instrument*),xxh->ins); \
    xxs=calloc(sizeof(struct xxm_sample),xxh->smp); \
    xxae=calloc(sizeof(uint16*),xxh->ins); \
    xxpe=calloc(sizeof(uint16*),xxh->ins); \
    xxfe=calloc(sizeof(uint16*),xxh->ins); \
}

#define PATTERN_INIT() { \
    xxt=calloc(sizeof(struct xxm_track*),xxh->trk); \
    xxp=calloc(sizeof(struct xxm_pattern*),xxh->pat+1); \
}

#define PATTERN_ALLOC(x) { \
    xxp[x]=calloc(1,sizeof(struct xxm_pattern)+\
    sizeof(struct xxm_trackinfo)*(xxh->chn-1)); \
}

#define TRACK_ALLOC(i) { \
    int j; \
    for(j=0;j<xxh->chn;j++) { \
	xxp[i]->info[j].xpose=0; \
	xxp[i]->info[j].index=i*xxh->chn+j; \
	xxt[i*xxh->chn+j]=calloc(sizeof(struct xxm_track)+ \
	    sizeof(struct xxm_event)*(xxp[i]->rows?xxp[i]->rows:0x100),1); \
	xxt[i*xxh->chn+j]->rows=xxp[i]->rows; \
    } \
}

#endif /* __LOAD_H */
