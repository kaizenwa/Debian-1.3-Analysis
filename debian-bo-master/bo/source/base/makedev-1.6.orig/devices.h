/*
 * devices.h: header file for devices.c
 *
 * Written 25-Mar-95 by David A. Holland, dholland@husc.harvard.edu
 *
 * Copyright 1994, 1995. All rights reserved. 
 * See the file LEGAL.NOTICE for conditions of redistribution.
 *
 * Bugs:
 *    None known right now.
 *
 * History:
 *
 * Version 1    25-Mar-95       Protos for the split of makedev.syn.
 */

#define YES 1
#define NO 0

/*
 * Proto for parser.
 */
void doparse(FILE *f, int filetype, const char *filename);

/*
 * Data types used by both parser and devices stuff
 */
#define MAXTARGETS 32
typedef struct {
    const char *name;  /* name of batch */
    const char *targets[MAXTARGETS];
    int ntargets;
    int busy;
} batch;

/*
 * Functions for parser
 */
void crash(const char *msg);
void warn(const char *format, ...);
int get_major(const char *procname, int ischar, int defaalt);
void addalias(const char *procname, const char *groupname);
void init(const char *name, const char *grp, const char *class,
	  int major, int minor, int type);
void initlots(const char *base, int lo, int hi, const char *grp,
	      const char *class,
	      int maj, int baseminor, int type);
void initdisk(const char *base, int low, int high, int nparts,
	      int maj, int minmult);
void initlink(const char *name, const char *grp, const char *target);
void set_major(const char *procname, int ischar, int num);
void updatefromcache(const char *name, int major, int type);
batch *add2batch(batch *b, const char *target);
batch *addbatch(const char *name);
void ignore_procname(const char *procname);
void addclass(const char *name, const char *o, const char *g, int m);

/*
 * Proto for main operative function.
 */
typedef enum { M_CREATE, M_OMIT } makeopts;
void make(const char *batch_or_grp_or_devname, makeopts);

