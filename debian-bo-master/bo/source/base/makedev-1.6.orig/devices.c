/*
 * devices.c: Code for MAKEDEV, a program to create entries in /dev.
 *
 * Based on the MAKEDEV shell script, version 2.0, distributed with
 * util-linux 1.10 and written by Nick Holloway. 
 *
 * A number of bugs were fixed, and some additional features added.
 * Written 10-Dec-94 by David A. Holland, dholland@husc.harvard.edu
 * Rik Faith (faith@cs.unc.edu) contributed ideas and patches.
 *
 * Copyright 1994, 1995. All rights reserved. 
 * See the file LEGAL.NOTICE for conditions of redistribution.
 *
 * Bugs:
 *    None known right now.
 *
 * History:
 *
 * Version 2: 25-Mar-95    Fixed makefile. 
 *    Look for config files in ".." under testing conditions.
 *    Big source split: makedev.syn -> parser.syn and devices.c.
 *    Consequently, this file's version numbers aren't the same as the
 *    whole program's any more.
 * Version 1.4b: 25-Mar-95 Merged Rik's changes. Additional bug fixes:
 *    Don't leave off the last entry in a range.
 *    Parse hex digits correctly [sigh...].
 *    Now we actually *use* the ishex flag.
 * Version 1.4a: 26-Feb-95 Forced devinfo and makedev.cfg to be in /etc.
 *                         [from faith@cs.unc.edu]
 * Version 1.4: 15-Jan-95  Wrote man pages. Now reads DEVINFO.local.
 * Version 1.3: 31-Dec-94  Bug fixes. Added batches. Added omits.
 * Version 1.2: 11-Dec-94  Add configuration file parsing.
 * Version 1.1: 11-Dec-94  Distinguish block and character devices in the
 *    table of major device numbers. Changed the name and format of the
 *    update cache file to include the type. It appears that the old script
 *    was broken in this regard.
 * Version 1.0: 10-Dec-94  Initial version.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>
#include <sys/stat.h>

#include "devices.h"
#include "version.h"

static int isverbose=NO;  /* flag: print out what we do? */
static int deletion=NO;   /* flag: delete instead of create */
static int donothing=NO;  /* flag: don't actually do anything */

/*
 * Roll over and die.
 */
void crash(const char *msg) {
  fprintf(stderr, "MAKEDEV: %s\n", msg);
  exit(1);
}

/*
 * Print a warning.
 */
void warn(const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  fprintf(stderr, "MAKEDEV: ");
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n");
  va_end(ap);
}

/*
 * Translate string name to uid.
 */
static uid_t name2uid(const char *name) {
  struct passwd *p = getpwnam(name);
  if (!p) warn("undefined user: %s, using uid 0", name);
  return p ? p->pw_uid : 0;  /* make things owned by root by default */
}

/*
 * Translate string name to gid.
 */
static gid_t name2gid(const char *name) {
  struct group *g = getgrnam(name);
  if (!g) warn("undefined group: %s, using gid 0", name);
  return g ? g->gr_gid : 0;  /* group 0 is a good default too */
}

/************************* device classes *************************/

/*
 * A device class is a string attached to the device which tells us
 * what set of permissions and ownership should be used. This is the
 * table of classes.
 */

typedef struct {
    const char *classname;
    const char *owner;
    const char *group;
    int mode;
} devclass;

#define MAXCLASSES 32
static devclass classes[MAXCLASSES];
static int nclasses=0;

void addclass(const char *name, const char *o, const char *g, int m) {
  if (nclasses>=MAXCLASSES) crash("out of space for device classes");
  classes[nclasses].classname = name;
  classes[nclasses].owner = o;
  classes[nclasses].group = g;
  classes[nclasses].mode = m;
  nclasses++;
  name2uid(o);  /* check for undefined users/groups */
  name2gid(g);
}

static void loadclasses(void) {
  FILE *f;
#ifdef TESTING
  f = fopen("../makedev.cfg", "r");
#else
  f = fopen("/etc/makedev.cfg", "r");
#endif
  if (!f) crash("Can't load /etc/makedev.cfg");
  doparse(f, 4, "makedev.cfg");
  fclose(f);
}

/*
 * Return the index into the above table for a particular class name.
 */
static int which_class(const char *name) {
  int i;
  for (i=0; i<nclasses; i++)
    if (!strcmp(classes[i].classname, name)) return i;
  return 0;
}

/*
 * Produce an "ls -l"-ish mode string.
 */
static const char *modestring(int mode) {
  static char rv[12];
  int i,z;
  strcpy(rv, "rwxrwxrwx");
  for (i=8,z=1; i>=0; i--, z<<=1) if (!(mode&z)) rv[i]='-';
  return rv;
}

/*
 * Create (or delete, or update) a block or character device.
 */
static void class_makedev(const char *name, const char *class,
			  int major, int minor, char type) {
  int x = which_class(class), mode = classes[x].mode;
  const char *owner = classes[x].owner, *group = classes[x].group;
  if (isverbose) {
    if (deletion) printf("rm -f %s\n", name);
    else printf("%c%s   1 %-8s %-8s %3d, %3d for %s\n", type,
		modestring(mode), owner, group, major, minor, name);
  }
  if (donothing) return;
  if (unlink(name) && deletion) warn("Couldn't remove %s\n", name);
  if (!deletion) {
    dev_t q = (major<<8) | minor;
    if (mknod(name, type=='c' ? S_IFCHR : S_IFBLK,  q) ||
	chown(name, name2uid(owner), name2gid(group)) ||
	chmod(name, mode)) {
      warn("couldn't create %s: %s", name, strerror(errno));
    }
  }
}

/************************* major number list *************************/

/*
 * In Linux device major numbers can be allocated dynamically, so we go
 * look in /proc/devices to see what they are. This keeps track of things.
 */

typedef struct {
    const char *procname;
    int flag;
} majorentry;

#define MAXMAJORS 256
static majorentry cmajors[MAXMAJORS];  /* initialized to 0 */
static majorentry bmajors[MAXMAJORS];  /* initialized to 0 */
static int no_proc=0;   /* true if we didn't find /proc/devices */

/*
 * Store the name associated with a particular major device number.
 */
void set_major(const char *procname, int ischar, int num) {
  if (num<0 || num>255) {
    warn("warning: got bogus major number %d for %s", num, procname);
    return;
  }
  if (ischar) cmajors[num].procname=procname;
  else bmajors[num].procname=procname;
}

/*
 * Look up a major device number by name; return the default value
 * if provided. A default value of -1 implies the device is only
 * dynamic, and so if there's no entry we shouldn't even note its
 * existence.
 */
int get_major(const char *procname, int ischar, int defaalt) {
  int i;
  if (!procname) return defaalt;
  if (ischar) {
    for (i=0; i<MAXMAJORS; i++)
      if (cmajors[i].procname && !strcmp(cmajors[i].procname, procname))
	return i;
  }
  else {
    for (i=0; i<MAXMAJORS; i++)
      if (bmajors[i].procname && !strcmp(bmajors[i].procname, procname))
	return i;
  }
  return defaalt;
}

/*
 * Read /proc/devices.
 */
static void setup_majors(void) {
  FILE *f = fopen("/proc/devices", "r");
  if (!f) {
    fprintf(stderr, "MAKEDEV: warning: can't read /proc/devices\n");
    no_proc = 1;
    return;
  }
  doparse(f, 1, "/proc/devices");
  fclose(f);
}

/************************** procname list *************************/

/*
 * The names found in /proc/devices aren't usually quite the same
 * as the names we use. This is a mapping between the two namespaces.
 */
typedef struct {
    const char *procname;
    const char *groupname;
} namealias;

#define MAXALIASES 100
static namealias aliases[MAXALIASES];
static int naliases=0;

void addalias(const char *procname, const char *groupname) {
  if (naliases>=MAXALIASES) crash("out of space for aliases");
  aliases[naliases].procname = procname;
  aliases[naliases].groupname = groupname;
  naliases++;
}

void ignore_procname(const char *procname) {
  addalias(procname, NULL);
}

static const char *procnameof(const char *groupname) {
  int i;
  for (i=0; i<naliases; i++) if (!strcmp(groupname, aliases[i].groupname))
    return aliases[i].procname;
  return NULL;
}

static const char *groupnameof(const char *procname) {
  int i;
  for (i=0; i<naliases; i++) if (!strcmp(procname, aliases[i].procname))
    return aliases[i].groupname;
  return NULL;
}

/************************* batch list *************************/
/*
 * Create a device "batch" - a bunch of devices or groups.
 * This is used for "generic" and automatically for disk entries.
 * (Disk entries for "hd" come up with groups hda, hdb, etc., but
 * "hd" itself needs to run these too.)
 */
#define MAXBATCHES 64

/* batch structure is in devices.h */

static batch batches[MAXBATCHES];
static int nbatches=0;

/*
 * Start a new batch.
 */
batch *addbatch(const char *name) {
  batch *b;
  if (nbatches>=MAXBATCHES) crash("Out of space for batches");
  b = &batches[nbatches++];
  b->name = name;
  b->busy = NO;
  return b;
}

/*
 * Add something to a batch.
 */
batch *add2batch(batch *b, const char *target) {
  if (b->ntargets>=MAXTARGETS) {
    warn("Too many targets for batch %s (max %d)", b->name, MAXTARGETS);
    return b;
  }
  b->targets[b->ntargets++] = target;
  return b;
}

/*
 * Run a batch.
 */
static void run_batch(const batch *b, makeopts m) {
  int i;
  for (i=0; i<b->ntargets; i++) make(b->targets[i], m);
}

/*
 * Try to run a batch; returns YES if it found one.
 */
static int try_run_batch(const char *name, makeopts m) {
  int i;
  for (i=0; i<nbatches; i++) {
    if (!strcmp(name, batches[i].name)) {
      if (batches[i].busy) {
	warn("Found recursive batch definition for %s", batches[i].name);
	continue;
      }
      batches[i].busy=YES;
      run_batch(&batches[i], m);
      batches[i].busy=NO;
      return YES;
    }
  }
  return NO;
}

/************************* device list *************************/

/*
 * Structure to remember the properties of an individual device.
 * NOTE: if the device is actually a symbolic link, the "class"
 * member is used to store the thing it should be linked to.
 */
typedef struct {
    const char *name;   /* file name to create */
    const char *grp;    /* device "group" name (e.g. "busmice") */
    const char *class;  /* device class ( -> owner and permissions) */
    int major, minor;   /* device number */
    char type;          /* 'c', 'b', or 'l' for symbolic link */
    int omit;           /* don't make me if this is nonzero */
} device;

/*
 * Create a device (link or actual "special file") - special files are
 * passed on to class_makedev().
 */
void makedev(device *d, makeopts m) {
  if (m==M_OMIT) {
    d->omit=1;
  }
  if (d->omit==1) return;
  if (d->type=='l') {
    if (isverbose) {
      if (deletion) printf("rm -f %s\n", d->name);
      else printf("lrwxrwxrwx   %s -> %s\n", d->name, d->class);
    }
    if (donothing) return;
    if (unlink(d->name) && deletion) warn("Couldn't remove %s\n", d->name);
    if (!deletion) {
      if (symlink(d->class, d->name)) /* class holds thing pointed to */
	warn("couldn't link %s -> %s: %s", d->name, d->class, strerror(errno));
    }
  }
  else class_makedev(d->name, d->class, d->major, d->minor, d->type);
}

/*
 * Array of devices. We allocate it once from main(); it doesn't grow.
 * Should maybe make it growable sometime. This keeps track of all possible
 * devices. We build this thing first, and then create devices from it as
 * requested.
 */
static device *devices = NULL;
static int maxdevices, ndevices;

/*
 * Allocate space for the device array.
 */
static void allocate_devs(int nd) {
  devices = malloc(nd * sizeof(device));
  if (!devices) crash("Out of memory");
  ndevices = 0;
  maxdevices = nd;
}

/*
 * Check all the devices for having valid device classes.
 */
static void check_classes(void) {
  int i;
  const char *q=NULL;
  for (i=0; i<ndevices; i++) 
    if (devices[i].type!='l' && !devices[i].omit &&
	which_class(devices[i].class)<0) {
      if (!q || strcmp(q, devices[i].class)) {
	warn("Invalid device class %s for %s", 
	     devices[i].class, devices[i].name);
	q = devices[i].class;
      }
      devices[i].class = "default";
    }
}

/*
 * Create an entry in the device table for a single device.
 */
void init(const char *name, const char *grp, const char *class,
		 int major, int minor, int type) {
  if (major < 0) return;
  if (!strchr("bcl", type)) {
    warn("invalid device type %c for %s (skipping)", type, name);
    return;
  }
  if (ndevices>=maxdevices) crash("out of space for devices");
  devices[ndevices].name = name;
  devices[ndevices].grp = grp;
  devices[ndevices].class = class;
  devices[ndevices].major = major;
  devices[ndevices].minor = minor;
  devices[ndevices].type = type;
  devices[ndevices].omit = 0;
  ndevices++;
}

/*
 * Create an entry for a symbolic link "device", such as /dev/fd
 * (which is a symbolic link to /proc/self/fd)
 */
void initlink(const char *name, const char *grp, const char *target) {
  init(name, grp, target, 0, 0, 'l');
}

/*
 * Init lots of devices. This creates a number of devices, numbered between
 * lo and hi. The idea is that "base" contains a %d or %x (or something like
 * that) in it which pulls in the number. The device group can also do this,
 * though this will in most cases not be useful. "baseminor" is the minor
 * number of the first device created.
 */
void initlots(const char *base, int lo, int hi, const char *grp,
		     const char *class,
		     int maj, int baseminor, int type) {
  char buf[32], gbuf[32];
  int i;
  if (maj<0) return;
  for (i=lo; i<=hi; i++) {
    sprintf(buf, base, i);
    if (grp) sprintf(gbuf, grp, i);  /* grp is permitted to contain a %d */
    init(strdup(buf), grp ? strdup(gbuf) : NULL, class, 
	 maj, baseminor+i-lo, type);
  }
}

/*
 * Init a whole (hard) disk's worth of devices - given `hd', it makes
 * hda1...hda8 through hdd1...hdd8 in one fell swoop. "low" and "high"
 * are the letters to use ('a' and 'd' for the previous example).
 * "nparts" is the number of partitions to create, ordinarily 8.
 * "maj" is the major device number; minmult is the multiplier for the
 * minor number. That is, if hda starts at 0, and hdb starts at 64, minmult
 * is 64.
 *
 * Note that it creates "hda", "hdb", etc. too, and puts things in the
 * groups "hda", "hdb", etc. as appropriate. The class is set to "disk".
 */
void initdisk(const char *base, int low, int high, int nparts,
	      int maj, int minmult) {
  char buf[16], buf2[16];
  int i;
  batch *b;
  if (maj<0) return;
  if (low>=high) return;
  b = addbatch(base);
  for (i=low; i<=high; i++) {
    char *q;
    sprintf(buf, "%s%c", base, i);
    q = strdup(buf);
    init(q, q, "disk", maj, (i-low)*minmult,   'b');
    strcpy(buf2, buf);
    strcat(buf2, "%d");
    initlots(buf2, 1, nparts, buf, "disk", maj, (i-low)*minmult+1, 'b');
    add2batch(b, q);
  }
}

static void initdevs(void) {
  FILE *f;
#ifdef TESTING
  f = fopen("../devinfo", "r");
#else
  f = fopen("/etc/devinfo", "r");
#endif
  if (!f) crash("Can't read /etc/devinfo");
  doparse(f,3, "devinfo");
  fclose(f);
  f = fopen("/usr/local/etc/devinfo.local", "r");
  if (!f) f = fopen("/etc/devinfo.local", "r");
  if (f) {
    doparse(f,3, "devinfo.local");
    fclose(f);
  }
}

/************************** update *************************/

/*
 * Call make() with our names for something that appeared in /proc/devices.
 */

static void transmake(const char *procname, makeopts m) {
  const char *gname = groupnameof(procname);
  if (gname) make(gname, m);
}

/*
 * Update a device that appeared in MAKEDEV.cache. Whenever we update,
 * we save what we did into MAKEDEV.cache; this lets us avoid doing
 * them over the next time. We only do something if the device has
 * disappeared or the major number has changed.
 *
 * Note that this caching made the shell version go much faster (it took
 * around 15 seconds with the cache, vs. over a minute if the cache was
 * blown away.) For us, it still does, but it hardly matters: it shaves
 * one second off a two-second execution.
 *
 * Also note the old script used DEVICES instead of MAKEDEV.cache. We
 * changed because the old file didn't record whether something was
 * a block or character device; since the sets of numbers are independent,
 * this was bound to break.
 */
static void update2(const char *name, int ischar, int major) {
  int now = get_major(name, ischar, -1);
  if (now<0) {
    deletion = 1;   /* must have been zero if we're doing an update */
    transmake(name, M_CREATE);
    deletion = 0;
  }
  else if (now!=major) { /* oops, it moved; remake it */
    transmake(name, M_CREATE);
    if (ischar) cmajors[now].flag=1;
    else bmajors[now].flag=1;
  }
  else {
    if (ischar) cmajors[now].flag=1; /* unchanged; inhibit remaking it */
    else bmajors[now].flag=1; /* unchanged; inhibit remaking it */
  }
}

void updatefromcache(const char *name, int major, int type) {
  update2(name, type=='c', major);
}


/*
 * Update. Read the information stored in MAKEDEV.cache from the last
 * update; fix anything that changed; then create any new devices that
 * weren't listed the last time. (We use the "flag" field in the
 * majors array to check this.) At that point, write out a new
 * cache file.
 */
#define CACHEFILE "MAKEDEV.cache"

static void update(void) {
  FILE *f;
  int i;
  if (no_proc) { warn("Couldn't read anything from /proc/devices"); return; }
  if (deletion) { warn("update and -d are incompatible"); return; }
  f = fopen(CACHEFILE, "r");
  if (f) {
    doparse(f, 2, CACHEFILE);
    fclose(f);
  }
  for (i=0; i<MAXMAJORS; i++) {
    if (cmajors[i].procname && !cmajors[i].flag) {
      transmake(cmajors[i].procname, M_CREATE);
      cmajors[i].flag=1;
    }
    if (bmajors[i].procname && !bmajors[i].flag) {
      transmake(bmajors[i].procname, M_CREATE);
      bmajors[i].flag=1;
    }
  }
  if (donothing) return;
  f = fopen(CACHEFILE, "w");
  if (f) {
    for (i=0; i<MAXMAJORS; i++)  {
      if (cmajors[i].procname) fprintf(f, "%s %d char\n", cmajors[i].procname, i);
      if (bmajors[i].procname) fprintf(f, "%s %d block\n", bmajors[i].procname, i);
    }
    fclose(f);
  }
  else warn("warning: can't write MAKEDEV.cache");
}

/************************* work *************************/

/*
 * Create (or delete, etc. according to flags) a device or device group.
 * The "generic" group is handled specially by recursing once.
 * "update" is handled specially; see update() below.
 * "local" issues a warning; people should use DEVINFO.local instead.
 */
void make(const char *what, makeopts m) {
  int i;
  if (!strcmp(what, "update")) {
    if (m!=M_CREATE) warn("update not compatible with those options");
    else update();
  }
  else if (!strcmp(what, "local")) {
    warn("The local target is obsolete.");
  }
  else if (!try_run_batch(what, m)) {
    int found=0;
    for (i=0; i<ndevices; i++) {
      if ((devices[i].grp && !strcmp(what, devices[i].grp)) ||
          !strcmp(what, devices[i].name)) {
        makedev(&devices[i], m);
        found = 1;
      }
    }
    if (!found) warn("unknown device or device group %s", what);
  }
}

/*
 * A major improvement over the shell version...
 */
static void usage(void) {
  printf("%s usage:\n", MYNAME);
  printf("    %s [-vdcn] device [device...]\n", MYNAME);
  printf("      -v          Verbose output\n");
  printf("      -d          Remove specified devices\n");
  printf("      -c          Create devices (default)\n");
  printf("      -n          Don't actually do anything (implies -v)\n");
  printf("      -I          Immediate; work in current directory, not /dev\n");
  printf("      -V          Print version information\n");
  printf("\n");
}

static void setcwd(void) {
  char buf[64];
  if (!getcwd(buf, sizeof(buf)) || strcmp(buf, "/dev")) {
      printf("Notice: changing my directory to /dev.\n");
      chdir("/dev");
  }
}

/*
 * We should use getopt one of these days.
 */
int main(int argc, char **argv) {
  int i,j, done=0;
  int immediate;
#ifdef TESTING
  immediate=1;   /* you don't think I run buggy versions in /dev, do you? */
#else
  immediate=0;
#endif

  for (i=1; i<argc && argv[i][0]=='-' && !done; i++) {
    for (j=1; argv[i][j] && !done; j++) switch(argv[i][j]) {
        case '-': done=1; break;
	case 'v': isverbose = 1; break;
	case 'd': deletion = 1; break;
	case 'c': deletion = 0; break;
	case 'n': donothing = 1; isverbose = 1; break;
	case 'h': usage(); exit(0);
        case 'I': immediate = 1; break;
	case 'V': printf("MAKEDEV: %s\n", version); exit(0);
	default: fprintf(stderr, "%s: unknown flag %c\n", MYNAME, argv[i][j]);
	  exit(1);
    }
  }
  if (!immediate) setcwd();  /* go into /dev */
  setup_majors();      /* read major device numbers from /proc */
  allocate_devs(4500); /* make space to hold devices */
  initdevs();          /* set up device structures */
  loadclasses();       /* load device classes from config file */
  check_classes();     /* make sure no devices have bogus classes */
  if (i==argc) warn("didn't do anything; try -h for help.");
  else for (; i<argc; i++) make(argv[i], M_CREATE);
  return 0;
}


