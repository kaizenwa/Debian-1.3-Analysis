/* lilo.c  -  LILO command-line parameter processing */

/* Copyright 1992-1996 Werner Almesberger. See file COPYING for details. */


#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>

#include <linux/config.h>
#include <asm/page.h>

#include "config.h"
#include "common.h"
#include "lilo.h"
#include "temp.h"
#include "device.h"
#include "geometry.h"
#include "map.h"
#include "bsect.h"
#include "cfg.h"
#include "identify.h"


#define S2(x) #x
#define S(x) S2(x)


static void show_images(char *map_file)
{
    DESCR_SECTORS descrs;
    BOOT_SECTOR boot;
    GEOMETRY geo;
    SECTOR_ADDR addr[2];
    char buffer[SECTOR_SIZE];
    char *name;
    int fd,image,i;
    int tsecs;
    unsigned short checksum,flags;

    fd = geo_open(&geo,map_file,O_RDONLY);
    if (read(fd,buffer,SECTOR_SIZE) != SECTOR_SIZE)
	die("read %s: %s",map_file,strerror(errno));
    if (read(fd,(char *) &descrs,sizeof(descrs)) != sizeof(descrs))
	die("read %s: %s",map_file,strerror(errno));
    if (verbose > 0) {
	bsect_read(cfg_get_strg(cf_options,"boot"),&boot);
	printf("Global settings:\n");
	tsecs = (boot.par_1.delay*55+50)/100;
	printf("  Delay before booting: %d.%d seconds\n",tsecs/10,tsecs % 10);
	if (boot.par_1.timeout == 0xffff) printf("  No command-line timeout\n");
	else {
	    tsecs = (boot.par_1.timeout*55+50)/100;
	    printf("  Command-line timeout: %d.%d seconds\n",tsecs/10,
	      tsecs % 10);
	}
	if (boot.par_1.prompt) printf("  Always enter boot prompt\n");
	else printf("  Enter boot prompt only on demand\n");
	if (!boot.par_1.port) printf("  Serial line access is disabled\n");
	else printf("  Boot prompt can be accessed from COM%d\n",
	      boot.par_1.port);
	if (!boot.par_1.msg_len) printf("  No message for boot prompt\n");
	else printf("  Boot prompt message is %d bytes\n",boot.par_1.msg_len);
	if (*(unsigned short *) buffer != DC_MAGIC || !buffer[2])
	    printf("  No default boot command line\n");
	else printf("  Default boot command line: \"%s\"\n",buffer+2);
	printf("Images:\n");
    }
    for (image = 0; image < MAX_IMAGES; image++)
	if (*(name = descrs.d.descr[image].name)) {
	    printf("%s%-" S(MAX_IMAGE_NAME) "s%s",verbose > 0 ? "  " : "",name,
	      image ? "  " : " *");
	    if (verbose > 1) {
		printf(" <dev=0x%02x,hd=%d,cyl=%d,sct=%d>",
		  descrs.d.descr[image].start.device,
		  descrs.d.descr[image].start.head,
		  descrs.d.descr[image].start.track,
		  descrs.d.descr[image].start.sector);
	    }
	    printf("\n");
	    if (verbose > 0) {
		flags = descrs.d.descr[image].flags;
		if (!*descrs.d.descr[image].password)
		    printf("    No password\n");
		else printf("    Password is required for %s\n",flags &
		      FLAG_RESTR ? "specifying options" : "booting this image");
		printf("    Boot command-line %s be locked\n",flags &
		  FLAG_LOCK ? "WILL" : "won't");
		if (!(flags & FLAG_VGA))
		   printf("    VGA mode is taken from boot image\n");
		else {
		    printf("    VGA mode: ");
		    switch (descrs.d.descr[image].vga_mode) {
			case NORMAL_VGA:
			    printf("NORMAL\n");
			    break;
			case EXTENDED_VGA:
			    printf("EXTENDED\n");
			    break;
			case ASK_VGA:
			    printf("ASK\n");
			    break;
			default:
			    printf("%d\n",descrs.d.descr[image].vga_mode);
		    }
		}
		if (!descrs.d.descr[image].start_page)
		    printf("    Kernel is loaded \"low\"\n");
		else printf("    Kernel is loaded \"high\", at 0x%08lx\n",
		      (unsigned long) descrs.d.descr[image].start_page*
		      PAGE_SIZE);
		if (!*(unsigned long *) descrs.d.descr[image].rd_size)
		    printf("    No initial RAM disk\n");
		else printf("    Initial RAM disk is %ld bytes\n",
		      *(unsigned long *) descrs.d.descr[image].rd_size);
#if 0
		if (descrs.d.descr[image].write_part)
		    printf("    PARTITION TABLE WILL BE REWRITTEN (to activate"
		     " partition %d)\n",descrs.d.descr[image].write_part-0x80);
#endif
		if (!geo_find(&geo,descrs.d.descr[image].start)) {
		    printf("    Map sector not found\n");
		    continue;
		}
		if (read(fd,addr,2*sizeof(SECTOR_ADDR)) !=
		  2*sizeof(SECTOR_ADDR))
			die("Read on map file failed (access conflict ?)");
		if (!geo_find(&geo,addr[0]))
		    printf("    Fallback sector not found\n");
		else {
		    if (read(fd,buffer,SECTOR_SIZE) != SECTOR_SIZE)
			die("Read on map file failed (access conflict ?)");
		    if (*(unsigned short *) buffer != DC_MAGIC)
			printf("    No fallback\n");
		    else printf("    Fallback: \"%s\"\n",buffer+2);
		}
		if (!geo_find(&geo,addr[1]))
		    printf("    Options sector not found\n");
		else {
		    if (read(fd,buffer,SECTOR_SIZE) != SECTOR_SIZE)
			die("Read on map file failed (access conflict ?)");
		    printf("    Options: \"%s\"\n",buffer);
		}
	    }
	}
    (void) close(fd);
    checksum = INIT_CKS;
    for (i = 0; i < sizeof(descrs)/sizeof(unsigned short); i++)
	checksum ^= ((unsigned short *) &descrs)[i];
    if (!checksum) exit(0);
    fflush(stdout);
    fprintf(stderr,"Checksum error\n");
    exit(1);
}


static void usage(char *name)
{
    char *here;

    if (here = strrchr(name,'/')) name = here+1;
    fprintf(stderr,"usage: %s [ -C config_file ] -q [ -m map_file ] "
      "[ -v ... ]\n",name);
    fprintf(stderr,"%7s%s [ -C config_file ] [ -b boot_device ] [ -c ] "
      "[ -l ]\n","",name);
    fprintf(stderr,"%12s[ -i boot_sector ] [ -m map_file ] [ -d delay ]\n","");
    fprintf(stderr,"%12s[ -v ... ] [ -t ] [ -s save_file | -S save_file ]\n",
      "");
    fprintf(stderr,"%12s[ -P fix | -P ignore ] [ -r root_dir ] [ -w ]\n","");
    fprintf(stderr,"%7s%s [ -C config_file ] [ -m map_file ] "
      "-R [ word ... ]\n","",name);
    fprintf(stderr,"%7s%s [ -C config_file ] -I name [ options ]\n","",name);
    fprintf(stderr,"%7s%s [ -C config_file ] [ -s save_file ] "
      "-u | -U [ boot_device ]\n","",name);
    fprintf(stderr,"%7s%s -V\n\n","",name);
    exit(1);
}


int main(int argc,char **argv)
{
    char *name,*config_file,*reboot_arg,*identify,*ident_opt,*new_root;
    char *uninst_dev;
    int query,more,version,uninstall,validate;
    BOOT_SECTOR dummy;
    IMAGE_DESCR dummy2;
    struct stat st;
    int fd;

    config_file = DFL_CONFIG;
    reboot_arg = identify = ident_opt = new_root = uninst_dev = NULL;
    query = version = uninstall = validate = 0;
    name = *argv++;
    argc--;
    cfg_init(cf_options);
    while (argc && **argv == '-') {
	argc--;
	if (argv[0][2]) usage(name);
	switch ((*argv++)[1]) {
	    case 'b':
		if (!argc) usage(name);
		cfg_set(cf_options,"boot",*argv++,NULL);
		argc--;
		break;
	    case 'c':
		cfg_set(cf_options,"compact",NULL,NULL);
		compact = 1;
		break;
	    case 'd':
		if (!argc) usage(name);
		cfg_set(cf_options,"delay",*argv++,NULL);
		argc--;
		break;
	    case 'D':
		if (!argc) usage(name);
		cfg_set(cf_options,"default",*argv++,NULL);
		argc--;
		break;
	    case 'f':
		if (!argc) usage(name);
		cfg_set(cf_options,"disktab",*argv++,NULL);
		argc--;
		break;
	    case 'l':
		cfg_set(cf_options,"linear",NULL,NULL);
		linear = 1;
		break;
	    case 'm':
		if (!argc) usage(name);
		cfg_set(cf_options,"map",*argv++,NULL);
		argc--;
		break;
	    case 'i':
		if (!argc) usage(name);
		cfg_set(cf_options,"install",*argv++,NULL);
		argc--;
		break;
	    case 'I':
		if (!argc) usage(name);
		identify = *argv++;
		if (--argc) {
		    ident_opt = *argv++;
		    argc--;
		}
		break;
	    case 'X':
		printf("-DIMAGES=%d -DCODE_START_1=%d -DCODE_START_2=%d "
		  "-DDESCR_SIZE=%d -DDSC_OFF=%d -DDSC_OFF2=%d -DDFCMD_OFF=%d "
		  "-DMSG_OFF=%d -DFLAGS_OFF=%d\n",MAX_IMAGES,
		  sizeof(BOOT_PARAMS_1),sizeof(BOOT_PARAMS_2),
		  sizeof(IMAGE_DESCR),
		  (void *) &dummy.par_1.descr[0]-(void *) &dummy,
		  (void *) &dummy.par_1.descr[1]-(void *) &dummy,
		  (void *) &dummy.par_1.descr[2]-(void *) &dummy,
		  (void *) &dummy.par_1.msg_len-(void *) &dummy,
		  (void *) &dummy2.flags-(void *) &dummy2);
		exit(0);
	    case 'C':
		if (!argc) usage(name);
		config_file = *argv++;
		argc--;
		break;
	    case 'S':
		if (!argc) usage(name);
		cfg_set(cf_options,"force-backup",*argv++,NULL);
		argc--;
		break;
	    case 's':
		if (!argc) usage(name);
		cfg_set(cf_options,"backup",*argv++,NULL);
		argc--;
		break;
	    case 'P':
		if (!argc) usage(name);
		if (!strcmp(*argv,"fix"))
		    cfg_set(cf_options,"fix-table",NULL,NULL);
		else if (!strcmp(*argv,"ignore"))
			cfg_set(cf_options,"ignore-table",NULL,NULL);
		    else usage(name);
		argv++;
		argc--;
		break;
	    case 'q':
		query = 1;
		break;
	    case 'r':
		if (!argc) usage(name);
		new_root = *argv++;
		argc--;
		break;
	    case 'R':
		if (!argc) reboot_arg = "";
		else while (argc) {
			if (!reboot_arg)
			    *(reboot_arg = alloc(strlen(*argv)+1)) = 0;
			else strcat(reboot_arg = ralloc(reboot_arg,
			      strlen(reboot_arg)+strlen(*argv)+2)," ");
			strcat(reboot_arg,*argv++);
			argc--;
		    }
		break;
	    case 't':
		test = 1;
		break;
	    case 'u':
		validate = 1;
		/* fall through */
	    case 'U':
		uninstall = 1;
		if (argc) {
		    if (argc-- > 1) usage(name);
		    uninst_dev = *argv;
		}
	    case 'v':
		verbose++;
		break;
	    case 'V':
		version = 1;
		break;
	    case 'w':
		cfg_set(cf_options,"nowarn",NULL,NULL);
		nowarn = 1;
		break;
	    default:
		usage(name);
	}
    }
    if (!new_root) new_root = getenv("ROOT");
    if (new_root && *new_root) {
	if (chroot(new_root) < 0) die("chroot %s: %s",new_root,strerror(errno));
	if (chdir("/") < 0) die("chdir /: %s",strerror(errno));
    }
    if (atexit(temp_remove)) die("atexit() failed");
    if (verbose > 0 || version) {
	printf("LILO version 19%s",version ? "\n" : "");
	if (version) return 0;
	printf(", Copyright 1992-1996 Werner Almesberger\n\n");
    }
#if 0
    if (((install || test || boot_device || disktab_file || compact) && !argc)
      || (compact && linear && 0)) usage(name);
#endif
    if (!nowarn && compact && linear)
	fprintf(stderr,"Warning: COMPACT may conflict with LINEAR on some "
	  "systems\n");
    fd = cfg_open(config_file);
    more = cfg_parse(cf_options);
    if (!nowarn) {
	if (fstat(fd,&st) < 0) {
	    fprintf(stderr,"fstat %s: %s\n",config_file,strerror(errno));
	    exit(1);
	}
	if (S_ISREG(st.st_mode)) {
	    if (st.st_uid)
		fprintf(stderr,"Warning: %s should be owned by root\n",
		  config_file);
	    else if (st.st_mode & (S_IWGRP | S_IWOTH))
		    fprintf(stderr,"Warning: %s should be writable only for "
		      "root\n",config_file);
		else if ((cfg_get_strg(cf_all,"password") || cfg_get_strg(
		      cf_options,"password")) && (st.st_mode & (S_IRGRP |
		      S_IROTH)))
			fprintf(stderr,"Warning: %s should be readable only "
			  "for root if using PASSWORD\n",config_file);
	}
    }
    preload_dev_cache();
    if (identify) identify_image(identify,ident_opt);
    if (uninstall)
	bsect_uninstall(uninst_dev ? uninst_dev : cfg_get_strg(cf_options,
	  "boot"),cfg_get_strg(cf_options,"backup"),validate);
    compact = cfg_get_flag(cf_options,"compact");
    linear = cfg_get_flag(cf_options,"linear");
    nowarn = cfg_get_flag(cf_options,"nowarn");
    if (cfg_get_strg(cf_options,"verbose"))
	verbose += to_number(cfg_get_strg(cf_options,"verbose"));
    if (reboot_arg) {
	map_patch_first(cfg_get_strg(cf_options,"map") ? cfg_get_strg(
	  cf_options,"map") : MAP_FILE,reboot_arg);
	exit(0);
    }
    if (argc) usage(name);
    geo_init(cfg_get_strg(cf_options,"disktab"));
    if (query)
	show_images(!cfg_get_strg(cf_options,"map") ? MAP_FILE :
	  cfg_get_strg(cf_options,"map"));
    bsect_open(cfg_get_strg(cf_options,"boot"),cfg_get_strg(cf_options,"map") ?
      cfg_get_strg(cf_options,"map") : MAP_FILE,cfg_get_strg(cf_options,
      "install"),cfg_get_strg(cf_options,"delay") ? to_number(cfg_get_strg(
      cf_options,"delay")) : 0,cfg_get_strg(cf_options,"timeout") ?
      to_number(cfg_get_strg(cf_options,"timeout")) : -1);
    if (more) {
        cfg_init(cf_top);
        if (cfg_parse(cf_top)) cfg_error("Syntax error");
    }
    if (!bsect_number()) die("No images have been defined.");
    check_fallback();
    if (!test)
	if (cfg_get_strg(cf_options,"force-backup"))
	    bsect_update(cfg_get_strg(cf_options,"force-backup"),1);
	else bsect_update(cfg_get_strg(cf_options,"backup"),0);
    else {
	bsect_cancel();
	fprintf(stderr,"The boot sector and the map file have *NOT* been "
	  "altered.\n");
    }
    return 0;
}
