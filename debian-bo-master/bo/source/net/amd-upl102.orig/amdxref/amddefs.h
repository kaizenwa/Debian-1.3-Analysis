#ifndef __AMDDEFS_H__
#define __AMDDEFS_H__
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

01/30/1993
*/

enum {
     HOST, ARCH, OS, BYTE, CLUSTER,
     DOMAIN, HOSTD, KARCH, AUTODIR,
     KEY, MAP, PATH, WIRE
     } seltoken_t;

enum {
     TYPE, RHOST, RFS, OPTS, SUBLINK,
     FS, PREF, DEV, DELAY, REMOPTS,
     MOUNT, UNMOUNT, CACHE
     } opttoken_t;

struct keywords {
     char *name;
     int token;
};

#ifdef PARSE_TABLES
struct keywords selector[] = {
     { "host", HOST },
     { "arch", ARCH },
     { "os", OS },
     { "byte", BYTE },
     { "cluster", CLUSTER },
     { "domain", DOMAIN },
     { "hostd", HOSTD },
     { "karch", KARCH },
     { "autodir", AUTODIR },
     { "key", KEY },
     { "map", MAP },
     { "path", PATH },
     { "wire", WIRE },
     { (char *)0, 0 }
     };

struct keywords options[] = {
     { "type", TYPE },
     { "rhost", RHOST },
     { "rfs", RFS },
     { "opts", OPTS },
     { "sublink", SUBLINK },
     { "fs", FS },
     { "pref", PREF },
     { "dev", DEV },
     { "delay", DELAY },
     { "remopts", REMOPTS },
     { "mount", MOUNT },
     { "unmount", UNMOUNT },
     { "cache", CACHE },
     { (char *)0, 0 }
};
#else
extern struct keywords selector[];
extern struct keywords options[];
#endif

#endif
