#ifndef __PARSER_H__
#define __PARSER_H__
/*
This software is provided as-is, with no warranties for suitability of
use or support implied.  This package makes no guarantees that it will
perform in any manner.  The authors and Texas A&M University accept no
liability for any damages incurred while using this software.

This software may be copied, redistributed, and updated in any
fashion as long as this comment header is not removed or altered.

Douglas Lee Schales
Doug.Schales@sc.tamu.edu
Texas A&M University
Supercomputer Center

01/26/1993
*/

struct selector {
     struct selector *next;
     int seltype;
     int oper;
     char *value;
};


struct record {
     struct record *next;
     struct selector *sel;
     unsigned long mountopts;
#define VTYPE  TYPE
#define VRHOST RHOST
#define VRFS   RFS
#define VOPTS  OPTS
#define VSUBLINK SUBLINK
#define VFS    FS
#define VPREF  PREF
#define VDEV DEV
#define VDELAY DELAY
#define VREMOPTS REMOPTS
#define VMOUNT MOUNT
#define VUNMOUNT UNMOUNT
#define VCACHE CACHE

#define VEND   VCACHE
#define VMAP   (VEND+1)
#define VKEY   (VEND+2)
#define VPATH  (VEND+3)
#define NUMVARS (VEND+4)
/* Don't forget to update the varnames array in scanner.c */
     char *variables[NUMVARS];
     char *mtpnt;
};

struct {
     int type;
     char *val;
} lastvar;

struct entry {
     struct entry *next;
     char *key;
     struct record *rec;
     int flags;
#define KEYDEFAULT 0x01
};

struct amdmap {
     struct amdmap *next;
     char *name;
     char *mtpnt;
     char *realmtpnt;
     struct entry *entries;
     struct entry *defaults;
};

#endif
