#include <stdio.h>
#include <linux/fs.h>
#include <sys/sysmacros.h>
#include <fcntl.h>
#include <linux/major.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <strings.h>

#include <linux/scsi.h>
#include <getopt.h>

int use_symlink = 0;
int filemode = 0600;
int verbose = 0;
char * no_serial = "No serial number";


/*
 * This program builds entries in /dev/scsi that have names that are tied
 * to the actual device number and lun.   This is pretty solid as long as
 * you have only one scsi controller, but with multiple scsi controllers
 * this all falls down.  The problem is that you can have multiple controllers
 * from one manufacturer, so it is not sufficient to assign an id based upon
 * an index from the driver.  Currently the controller numbers are just the
 * internal index that is assigned by the kernel.
 *
 * A better solution would be for the kernel to assign id numbers for each
 * controller type, and the we could assign numbers based upon the number
 * of the controller located.  This would guarantee that we could generate
 * useful number.
 */

struct regnames
{
  struct regnames * next;
  char * name;
  char * manufacturer;
  char * model;
  char * serial;
  struct regnames * alias;
  int major;
  int minor;
  int hostnum;
  int hostid;
  int chan;
  int id;
  int lun;
  int partition;
};

struct regnames * reglist = NULL;

/*
 * Used to maintain a list of the nodes that we have seen
 * which we know to be OK and active.
 */
register_dev(char * name, int major, int minor, int hnum, 
	     int hid, int chan, int id, 
	     int lun, int part, struct regnames * alias)
{
  struct regnames * rpnt;
  char * pnt;

  rpnt = (struct regnames *) malloc(sizeof(struct regnames));
  pnt = strrchr(name, '/');
  rpnt->name = strdup(pnt + 1);
  rpnt->major = major;
  rpnt->minor = minor;
  rpnt->hostnum = hnum;
  rpnt->hostid = hid;
  rpnt->chan = chan;
  rpnt->id = id;
  rpnt->lun = lun;
  rpnt->partition = part;
  rpnt->next = reglist;
  rpnt->alias = alias;
  /*
   * Initialize this - they may be needed later.
   */
  rpnt->model = rpnt->manufacturer = rpnt->serial = NULL;
  reglist = rpnt;
}

/*
 * We need to "fix" any device nodes that are currently not used because
 * it is a sercurity risk to leave these lying about.  These are fixed
 * by setting the minor number to 255.  If these become active again,
 * we will be able to use them again because the minor number will be
 * set back again, and we are preserving the ownership and permissions.
 */
void sanitize_sdev()
{
  struct dirent * de;
  char filename[60];
  DIR * sdir;
  struct regnames * rpnt;
  struct stat statbuf;
  int status;

  /*
   * Next, delete all of the existing entries in the /dev/scsi directory.
   * The idea is that if we have added/removed devices, the numbers might have
   * changed.
   */
  sdir = opendir("/dev/scsi");
  while(1==1)
    {
      de = readdir(sdir);
      if(de == NULL) break;
      /*
       * OK, we have the name.  See whether this is something
       * we know about already.
       */
      for( rpnt = reglist; rpnt; rpnt = rpnt->next )
	{
	  if( strcmp(de->d_name, rpnt->name) == 0 )
	    {
	      break;
	    }
	}
      if( rpnt == NULL )
	{
	  strcpy(filename, "/dev/scsi/");
	  strcat(filename, de->d_name);
	  status = stat(filename, &statbuf);
	  if(    status == 0
	      && (S_ISCHR(statbuf.st_mode) || S_ISBLK(statbuf.st_mode)))
	    {
	      /*
	       * OK, this one is something new that we have to do something
	       * with.  No big deal, stat it so we get the particulars, then
	       * create a new one with a safe minor number.
	       */
	      unlink(filename);

	      status = mknod(filename, statbuf.st_mode ,
			     makedev(major(statbuf.st_rdev),255));
	      status = chmod(filename, statbuf.st_mode);
	      chown(filename, statbuf.st_uid, statbuf.st_gid);

	    }
	}
    }
  closedir(sdir);

}


void flush_sdev()
{
  struct dirent * de;
  char filename[60];
  DIR * sdir;
  /*
   * Next, delete all of the existing entries in the /dev/scsi directory.
   * The idea is that if we have added/removed devices, the numbers might have
   * changed.
   */
  sdir = opendir("/dev/scsi");
  while(1==1)
    {
      de = readdir(sdir);
      if(de == NULL) break;
      if(de->d_name[0] != 's' && de->d_name[0] != 'r') continue;
      strcpy(filename, "/dev/scsi/");
      strcat(filename, de->d_name);
      unlink(filename);
    }
  closedir(sdir);
  printf("Flushed old /dev/scsi entries...\n", de->d_name);

}

/*
 * Check to see if a given entry exists.  If not, create it,
 * if it does make sure the major and minor numbers are correct
 * and save permissions and ownerships if this is the case.
 */
void update_device(char * path, int devtype, int major, int minor)
{
  struct stat statbuf;
  int recreate;
  int newmode;
  int uid, gid;
  int status;

  newmode = devtype | 
    (major != SCSI_CDROM_MAJOR ? filemode : (filemode & ~0222));

  recreate = 1;
  uid = gid = -1;
  status = stat(path, &statbuf);
  if( status == 0 ) {
      recreate = 0;
      /*
       * Make sure we have the correct device type too.
       */
      if( (statbuf.st_mode & S_IFMT) != devtype ) {
	  unlink(path);
	  recreate = 1;
	}
      /*
       * Compare the device number.  If something changed, then 
       * unlink it so that we can recreate it.  Save the mode of
       * the thing so that we can keep the same permissions.
       */
      else if( statbuf.st_rdev != makedev(major, minor) ) {
	  newmode = statbuf.st_mode;
	  uid = statbuf.st_uid;
	  gid = statbuf.st_gid;
	  unlink(path);
	  recreate = 1;
	}
    }

  /*
   * If we need to recreate the device, then do it.
   */
  if( recreate )
    {
      status = mknod(path, newmode,
		     makedev(major,minor));
      if( status == -1 ) {
	  fprintf(stderr,"mknod (%s) failed\n", path);
	  exit(1);
	}
      
      /*
       * The mknod system call will not always use
       * the right permissions because of umask.
       * Fix it so that it is really correct.
       */
      status = chmod(path, newmode);
      if( status == -1 ) {
	  fprintf(stderr,"chmod (%s) failed\n", path);
	  exit(1);
	}
      
      if( uid != -1 )
	  chown(path, uid, gid);
    }  
}

void build_sdev(int major, int mode, int devtype, char devchar)
{
  int chan;
  int fd;
  char hostname[60];
  int newmode;
  int recreate;
  int h_id;
  int id[2];
  int lun;
  int minor = 0;
  int uid, gid;
  struct stat statbuf;
  int scsi_id;
  char scsidev[64];
  char linkto[64];
  int status;

  status = stat("/tmp/testdev", &statbuf);
  if(status == 0)
    {
      unlink("/tmp/testdev");
    }

  status = stat("/dev/scsi", &statbuf);
  if(status == -1)
      return;

  while(1==1)
    {
      status = mknod("/tmp/testdev",0600 | devtype ,
		     makedev(major,minor));
      fd = open("/tmp/testdev", mode);
      if(fd == -1) {
	if( verbose == 2 )
	  {
	    fprintf(stderr,"open(%x/%x) returned %d (%d)\n",
		    major, minor, fd, errno);
	  }
	/*
	 * For disks, only stop searching when we cannot find a whole-disk
	 * somewhere (there could be sparse partition tables).  For anything
	 * else, stop as soon as we find something non-existant.
	 */
	if(   (major == SCSI_DISK_MAJOR)
	      && minor < 128)
	  {
	    minor++;
	    unlink ("/tmp/testdev");
	    continue;
	  } else if ( major != SCSI_DISK_MAJOR )
	    break;
      }
      status = ioctl(fd, SCSI_IOCTL_GET_IDLUN, &id);

      if(status == -1)	{
	if( verbose == 2 )
	  {
	    fprintf(stderr,"probe(%x/%x) returned %d (%d)\n",
		    major, minor, status, errno);
	  }
	close(fd);
	break;
      }

      status = ioctl(fd, SCSI_IOCTL_PROBE_HOST, hostname);
      close(fd);

      if(status == -1)	break;

#if 0
      printf("Found scsi device %x with idlun %x\n", makedev(major, minor), id);
#endif
      h_id = id[0] >> 24;
      chan = (id[0] >> 16) & 0xff;
      lun = (id[0] >> 8 ) & 0xff;
      scsi_id = id[0] & 0xff;
      if(major == SCSI_DISK_MAJOR && (minor & 0xf))
	{
	  sprintf(scsidev,"/dev/scsi/s%ch%d-%xc%di%dl%dp%d",
		  devchar,
		  h_id,
		  id[1],
		  chan,
		  scsi_id,
		  lun, (minor & 0xf));
	  register_dev(scsidev, major, minor, 
		       h_id, id[1], chan, scsi_id, 
		       lun, (minor & 0xf), NULL);
      } else {
	sprintf(scsidev,"/dev/scsi/s%ch%d-%xc%di%dl%d",
		devchar,
		h_id,
		id[1],
		chan,
		scsi_id,
		lun);
	register_dev(scsidev, major, minor, 
		     h_id, id[1], chan, scsi_id, lun, -1, NULL);
      }
      printf("Found scsi device %s %s\n", scsidev, hostname);

      if( use_symlink )
	{
	  /*
	   * Figure out what we should be symlinked to here.
	   */
	  switch(major)
	    {
	    case SCSI_DISK_MAJOR:
	      if( minor & 0xf)
		sprintf(linkto, "/dev/sd%c%d", 
			(minor >> 4) + 'a', (minor & 0xf)); 
	      else
		sprintf(linkto, "/dev/sd%c", 
			(minor >> 4) + 'a'); 

	      break;
	    case SCSI_CDROM_MAJOR:
	      sprintf(linkto, "/dev/sr%d", minor);
	      break;
	    case SCSI_TAPE_MAJOR:
	      sprintf(linkto, "/dev/st%d", minor);
	      break;
	    case SCSI_GENERIC_MAJOR:
	      sprintf(linkto, "/dev/sg%d", minor);
	      break;
	    }

	  symlink(linkto, scsidev);

	  /*
	   * Now make sure that the device the symlink points to actually
	   * exists.  If not, then create that device.
	   */
	  status = stat(scsidev, &statbuf);
	  if( status == 0 )
	    {
	      if( statbuf.st_rdev != makedev(major, minor) )
		{
		  fprintf(stderr,
			  "Warning - device %s does not point to expected device\n", linkto);
		}
	    }
	  else
	    {
	      /*
	       * Unable to stat the file.  Assume this is because it did not
	       * exist, so we create it.
	       */
	      newmode = devtype | 
		(major != SCSI_CDROM_MAJOR ? filemode : (filemode & ~0222));
	      status = mknod(linkto, newmode,
			     makedev(major,minor));
	      fprintf(stderr,"Creating %s\n", linkto);
	    }
	  
	  if( major == SCSI_TAPE_MAJOR )
	    {
	      sprintf(scsidev,"/dev/scsi/rs%ch%d-%xc%di%dl%d",
		      devchar,
		      h_id,
		      id[1],
		      chan,
		      scsi_id,
		      lun);
	      sprintf(linkto, "/dev/rst%d", minor);
	      symlink(linkto, scsidev);
	      /*
	       * Now make sure that the device the symlink points to actually
	       * exists.  If not, then create that device.
	       */
	      status = stat(scsidev, &statbuf);
	      if( status == 0 )
		{
		  if( statbuf.st_rdev != makedev(major, minor) )
		    {
		      fprintf(stderr,
			      "Warning - device %s does not point to expected device\n", linkto);
		    }
		}
	      else
		{
		  /*
		   * Unable to stat the file.  Assume this is because it did not
		   * exist, so we create it.
		   */
		  newmode = devtype | 
		    (major != SCSI_CDROM_MAJOR ? filemode : (filemode & ~0222));
		  status = mknod(linkto, newmode,
				 makedev(major,minor));
		  fprintf(stderr,"Creating %s\n", linkto);
		}
	    }
	}
      else
	{
	  update_device(scsidev, devtype, major, minor);

	  if( major == SCSI_TAPE_MAJOR )
	    {
	      sprintf(scsidev,"/dev/scsi/rs%ch%d-%xc%di%dl%d",
		      devchar,
		      h_id,
		      id[1],
		      chan,
		      scsi_id,
		      lun);
	      register_dev(scsidev, major, minor | 0x80, 
			   h_id, id[1], chan, scsi_id, lun, -1, NULL);

	      update_device(scsidev, devtype, major, minor | 0x80);
	    }
	}
      
      unlink("/tmp/testdev");
      minor += 1;
    }
  unlink("/tmp/testdev");
}


/*
 * Build a /dev/scsi tree that contains the correct device entries.
 */

usage()
{
  fprintf(stderr,"scsidev v1.3\n");
  fprintf(stderr,"Usage: scsidev [-f] [-l] [-v] [-m mode]\n");
}

main(int argc, char * argv[])
{
  char c;
  int force = 0;
  int show_serial = 0;
  struct stat statbuf;
  struct regnames * rpnt;
  int status;

  status = stat("/dev/scsi", &statbuf);
  if(    status == -1
      || !S_ISDIR(statbuf.st_mode))
    {
      fprintf(stderr,"/dev/scsi either does not exist, or is not a directory\n");
      exit(0);
    }
  while ((c = getopt(argc, argv, "flvsm:")) != EOF)
    {
      switch (c)
	{
	case 'f':
	  force = 1;
	  break;
	case 'm':
	  filemode = strtoul(optarg, 0, 0);
	  break;
	case 'l':
	  use_symlink = 1;
	  break;
	case 's':
	  show_serial = 1;
	  break;
	case 'v':
	  verbose++;
	  break;
	default:
	  usage();
	  exit(1);
	  break;
	}
    }

  if( force ) flush_sdev();

#ifdef DEBUG
  register_dev("/dev/scsi/sdh4-334c0i0l0",   8,  0, 6, 0x334, 0, 0, 0, -1, NULL);
  register_dev("/dev/scsi/sdh4-334c0i0l0p1", 8,  1, 6, 0x334, 0, 0, 0,  1, NULL);
  register_dev("/dev/scsi/sdh4-334c0i0l0p2",8,  2,6, 0x334, 0, 0, 0,  2, NULL);
  register_dev("/dev/scsi/sdh4-334c0i0l0p3",8,  3,6, 0x334, 0, 0, 0,  3, NULL);
  register_dev("/dev/scsi/sgh4-334c0i0l0", 21,  0,6, 0x334, 0, 0, 0, -1, NULL);
  register_dev("/dev/scsi/sgh4-334c0i2l0", 21,  1,6, 0x334, 0, 2, 0, -1, NULL);
  register_dev("/dev/scsi/sgh4-334c0i5l0", 21,  2,6, 0x334, 0, 5, 0, -1, NULL);
  register_dev("/dev/scsi/srh4-334c0i2l0", 11,  0,6, 0x334, 0, 2, 0, -1, NULL);
  register_dev("/dev/scsi/sth4-334c0i5l0",  9,  0,6, 0x334, 0, 5, 0, -1, NULL);
  register_dev("/dev/scsi/rsth4-334c0i5l0", 9,128,6, 0x334, 0, 5, 0, -1, NULL);
#else
  build_sdev(SCSI_DISK_MAJOR,    O_RDONLY, S_IFBLK, 'd');
  build_sdev(SCSI_CDROM_MAJOR,   O_RDONLY, S_IFBLK, 'r');
  build_sdev(SCSI_TAPE_MAJOR,    O_RDONLY, S_IFCHR, 't');
  build_sdev(SCSI_GENERIC_MAJOR, O_RDWR,   S_IFCHR, 'g');
#endif

  if( show_serial )
    {
      for (rpnt = reglist; rpnt; rpnt = rpnt->next)
	{
	  if( rpnt->partition != -1 ) continue;
	  if( rpnt->major == SCSI_TAPE_MAJOR
	      && (rpnt->minor & 0x80) != 0 ) continue;

	  if( rpnt->serial == NULL )  get_serial_number(rpnt);
	  if( rpnt->serial == no_serial )
	    printf("Device  %s has no serial number\n", rpnt->name);
	  else
	    printf("Serial number of %s: \"%s\"\n", rpnt->name, rpnt->serial);
	}
    }
  /*
   * Now, read the configuration file and see whether there
   * are any special device names we want to try and match.
   */
  build_special();

  if( !force )
    {
      sanitize_sdev();
    }

}

char * get_string (char * pnt, char **result)
{
  char quote = 0;

  while (*pnt == ' ' || *pnt == '\t') pnt++;

  if( *pnt == '"' ) 
    {
      quote = *pnt++;
    }

  if( *pnt == '\'' ) 
    {
      quote = *pnt++;
    }

  *result = pnt;

  if( quote != 0 )
    {
      while( *pnt != quote ) pnt++;
      *pnt++ = 0;
    }
  else
    {
      while( *pnt != ',' && *pnt != ' ' && *pnt != '\t' ) pnt++;
      *pnt++ = 0;
    }

  while (*pnt == ' ' || *pnt == '\t') pnt++;
  if(*pnt == ',') pnt++;
  while (*pnt == ' ' || *pnt == '\t') pnt++;
  return pnt;
}

char * get_number(char * pnt, int * result)
{
  int base = 10;
  int num;

  while (*pnt == ' ' || *pnt == '\t') pnt++;
  if(pnt[0] == '0' && pnt[1] == 'x') { base = 16; pnt += 2; }

  num = 0;
  while(1==1)
    {
      if(base == 10 && *pnt >= '0' && *pnt <= '9' )
	{
	  num = num * 10 + *pnt - '0';
	  pnt++;
	  continue;
	}
      if( base == 16 )
	{
	  if (*pnt >= '0' && *pnt <= '9')
	    {
	      num = num * 10 + *pnt - '0';
	      pnt++;
	      continue;
	    }
	  if (*pnt >= 'a' && *pnt <= 'f')
	    {
	      num = num * 10 + *pnt - 'a' + 10;
	      pnt++;
	      continue;
	    }
	  if (*pnt >= 'A' && *pnt <= 'F')
	    {
	      num = num * 10 + *pnt - 'A' + 10;
	      pnt++;
	      continue;
	    }
	  break;
	}
      /*
       * Isn't a digit.  Must be the end of the number.
       */
      break;
    }
  while (*pnt == ' ' || *pnt == '\t') pnt++;
  if(*pnt == ',') pnt++;
  while (*pnt == ' ' || *pnt == '\t') pnt++;
  *result = num;
  return pnt;

}

/*
 * The configuration file is designed to be something that can match
 * any number of fields.  Thus we need to be able to specify a large
 * number of different things in the hope that we can uniquely match
 * to one specific device.
 *
 * Each match contains a series of tokens:
 *
 * ID=number
 * LUN=number
 * CHANNEL=number
 * HOSTID=number
 * MANUFACTURER="string"
 * MODEL="string"
 * SERIAL_NUMBER="string"	(for those devices that support this).
 * NAME="string"
 * DEVTYPE="disk", "tape", "generic", or "cdrom".
 */
build_special()
{
  FILE *	configfile;
  char buffer[256];
  char * pnt;
  char * pnt1;
  struct regnames * rpnt, *match;
  char scsidev[64];
  int type;

  int lun, chan, id, part, hostid, hostnum;
  int line;
  int devtype_i;
  char * manufacturer, *model, *serial_number, *name, *devtype;

#ifdef DEBUG
  configfile = fopen("scsi.alias", "r");
#else
  configfile = fopen("/etc/scsi.alias", "r");
#endif
  line = 0;
  while(1==1)
    {
      fgets(buffer, sizeof(buffer), configfile);
      line++;
      if( feof(configfile)) break;

      /*
       * Remove trailing \n, if present.
       */
      pnt = buffer + strlen(buffer) - 1;
      if( *pnt == '\n' ) *pnt = '\0';

      /*
       * First, tokenize the input line, and pick out the parameters.
       */
      lun = -1;
      chan = -1;
      id = -1;
      part = -1;
      hostid = -1;
      hostnum = -1;
      manufacturer = NULL;
      model = NULL;
      serial_number = NULL;
      name = NULL;
      devtype = NULL;
      pnt = buffer;
      while (*pnt == ' ' || *pnt == '\t') pnt++;

      /* allow blank lines and comments... */
      if( *pnt == '\0' ) continue;
      if( *pnt == '#' ) continue;

      while(1==1)
	{
	  pnt1 = pnt;
	  while (*pnt1 != '=' && *pnt1 != '\0') pnt1++;
	  if( *pnt1 == '\0' ) break;

	  *pnt1 = '\0';
	  if( strncmp(pnt, "manu", 4) == 0 )
	    {
	      pnt = get_string(pnt1 + 1, &manufacturer);
	    }
	  else if ( strncmp(pnt, "mode", 4) == 0 )
	    {
	      pnt = get_string(pnt1 + 1, &model);
	    }
	  else if ( strncmp(pnt, "seri", 4) == 0 )
	    {
	      pnt = get_string(pnt1 + 1, &serial_number);
	    }
	  else if ( strcmp(pnt, "id") == 0 )
	    {
	      pnt = get_number(pnt1 + 1, &id);
	    }
	  else if ( strcmp(pnt, "lun") == 0 )
	    {
	      pnt = get_number(pnt1 + 1, &lun);
	    }
	  else if ( strncmp(pnt, "chan", 4) == 0 )
	    {
	      pnt = get_number(pnt1 + 1, &chan);
	    }
	  else if ( strncmp(pnt, "part", 4) == 0 )
	    {
	      pnt = get_number(pnt1 + 1, &part);
	    }
	  else if ( strcmp(pnt, "hostid") == 0 )
	    {
	      pnt = get_number(pnt1 + 1, &hostid);
	    }
	  else if ( strcmp(pnt, "hostnum") == 0 )
	    {
	      pnt = get_number(pnt1 + 1, &hostnum);
	    }
	  else if ( strncmp(pnt, "alia", 4) == 0 )
	    {
	      pnt = get_string(pnt1 + 1, &name);
	    }
	  else if ( strncmp(pnt, "devt", 4) == 0 )
	    {
	      pnt = get_string(pnt1 + 1, &devtype);
	    }
	  else
	    {
	      fprintf(stderr,"Unrecognized specifier \"%s\" on line\n", pnt,
		      line);
	      break;
	    }
	}

      /*
       * OK, got one complete entry.  Make sure it has the required
       * fields, and then attempt to match it to a real device.
       */
      if( name == NULL )
	{
	  fprintf(stderr,"Line %d is missing \"alias\" specifier\n", line);
	  continue;
	}
      if( devtype == NULL )
	{
	  fprintf(stderr,"Line %d is missing \"devtype\" specifier\n", line);
	  continue;
	}
      if( strcmp(devtype, "disk") == 0 )
	devtype_i = SCSI_DISK_MAJOR;
      else if( strcmp(devtype, "cdrom") == 0)
	devtype_i = SCSI_CDROM_MAJOR;
      else if( strcmp(devtype, "tape") == 0)
	devtype_i = SCSI_TAPE_MAJOR;
      else if(strcmp(devtype, "generic") == 0 )
	devtype_i = SCSI_GENERIC_MAJOR;
      else
	{
	  fprintf(stderr,"Line %d has invalid  \"devtype\" specifier(%s)\n", 
		  line, devtype);
	  continue;
	}

      /*
       * OK, minimal requirements are met.  Try and match this to something
       * we know about already.
       */
      match = NULL;
      for (rpnt = reglist; rpnt; rpnt = rpnt->next)
	{
	  /*
	   * Check the integers first.  Some of the strings we have to
	   * request, and we want to avoid this if possible.
	   */
	  if( rpnt->alias != NULL ) continue;
	  if( id != -1 && id != rpnt->id ) continue;
	  if( chan != -1 && id != rpnt->chan ) continue;
	  if( lun != -1 && lun != rpnt->lun ) continue;
	  if( hostid != -1 && hostid != rpnt->hostid ) continue;
	  if( hostnum != -1 && hostnum != rpnt->hostnum ) continue;
	  if( devtype_i != rpnt->major ) continue;
	  if( part != rpnt->partition ) continue;
	  if(    rpnt->major == SCSI_TAPE_MAJOR 
	      && (rpnt->minor & 0x80) != 0) continue;

	  /*
	   * OK, that matches, now obtain some of the strings
	   * that might be needed.
	   */
	  if(    (manufacturer != NULL || model != NULL)
	      && rpnt->manufacturer == NULL)
	    {
	      inquire(rpnt);
	    }
	  
	  if( serial_number && rpnt->serial == NULL )
	    {
	      get_serial_number(rpnt);
	    }

	  if(    manufacturer != NULL && rpnt->manufacturer != NULL
	      && strcmp(rpnt->manufacturer, manufacturer) != 0 )
	    continue;

	  if( model != NULL && rpnt->model != NULL
	      && strcmp(rpnt->model, model) != 0 )
	    continue;

	  if( serial_number != NULL && rpnt->serial != NULL
	      && strcmp(rpnt->serial, serial_number) != 0 )
	    continue;

	  /*
	   * We have a match.  Record it and keep looking just in
	   * case we find a duplicate.
	   */
	  if( match != NULL )
	    {
	      fprintf(stderr,"Line %d not matched uniquely\n", line);
	      break;
	    }
	  else
	    {
	      match = rpnt;
	    }
	}

      /*
       * See if there was a non-unique mapping.  If so, then
       * don't do anything for this one.
       */
      if( rpnt != NULL )
	{
	  continue;
	}

      if( match != NULL )
	{
	  /*
	   * OK, we have a unique match.  Create the device entries,
	   * as requested.
	   */
	  fprintf(stderr,"Building alias device entries for %s...\n", name);

	  if( devtype_i == SCSI_DISK_MAJOR
	      || devtype_i == SCSI_CDROM_MAJOR )
	    type = S_IFBLK;
	  else
	    type = S_IFCHR;

	  /*
	   * If this is just an ordinary single device type,
	   * Just create it.
	   */
	  sprintf(scsidev, "/dev/scsi/%s", name);
	  update_device(scsidev, type, 
			match->major, match->minor);
	  register_dev(scsidev, match->major, match->minor | 0x80, 
		       0, 0, 0, 0, 0, 0, match);

	  if( devtype_i == SCSI_TAPE_MAJOR )
	    {
	      sprintf(scsidev, "/dev/scsi/r%s", name);
	      update_device(scsidev, S_IFCHR, 
			    match->major, (match->minor | 0x80) );
	      register_dev(scsidev, match->major, match->minor | 0x80, 
			   0, 0, 0, 0, 0, 0, match);
	    }

	  if(    devtype_i == SCSI_DISK_MAJOR
		 && match->partition == -1 )
	    {
	      /*
	       * This is the master entry for a disk.
	       * we need to go through and generate entries
	       * for each partition.  The trick is to find
	       * all of the related entries so we know which
	       * ones we actually need to create.
	       */
	      for( rpnt = reglist; rpnt; rpnt = rpnt->next )
		{
		  if( rpnt->alias != NULL ) continue;
		  if( rpnt->partition == -1 ) continue;
		  if( rpnt->major != devtype_i ) continue;
		  if( rpnt->id != match->id ) continue;
		  if( rpnt->lun != match->lun ) continue;
		  if( rpnt->chan != match->chan ) continue;
		  if( rpnt->hostnum != match->hostnum ) continue;
		  if (rpnt->hostid != match->hostid ) continue;

		  sprintf(scsidev, "/dev/scsi/%s-p%d", name, 
			  rpnt->partition);
		  update_device(scsidev, S_IFBLK, 
				match->major, rpnt->minor);
		  register_dev(scsidev, match->major, match->minor | 0x80, 
			       0, 0, 0, 0, 0, 0, rpnt);
		}
	    }
	}
      else
	{
	  fprintf(stderr,"Unable to match device for line %d\n", line);
	}
    }

  fclose(configfile);
}

/*
 * Note that this does the same thing as an inquiry, but
 * it also grabs the serial number too.  Some devices complain
 * if you ask for the serial number, so we have a version with and
 * without.
 */
int get_serial_number(struct regnames * rpnt)
{
#ifdef DEBUG
  /*
   * Fill in some entries, just so I can test this on a non-scsi system.
   */
  if(rpnt->id == 0 )  rpnt->serial = strdup("DX908FK");
  else  rpnt->serial = no_serial;
#else
  int status, i;
  unsigned char *cmd;
  unsigned char * pagestart;
  unsigned char buffer[1024];
  char path[64];
  int infile;
  struct regnames * rpnt1;

  memset(buffer, 0, sizeof(buffer));
  
  *( (int *)  buffer )		= 0;	/* length of input data */
  *( ((int *) buffer) + 1 )	= 1024;	/* length of output buffer */

  cmd = (char *) ( ((int *) buffer) + 2 );
  
  cmd[0] = 0x12;			/* INQUIRY */
  cmd[1] = 0x01;			/* lun=0, evpd=1 */
  cmd[2] = 0x80;			/* page code = 0x80, serial number */
  cmd[3] = 0x00;			/* (reserved) */
  cmd[4] = 0xff;			/* allocation length */
  cmd[5] = 0x00;			/* control */

  strcpy(path, "/dev/scsi/");
  strcat(path, rpnt->name);
  infile = open(path, O_RDONLY);
  if( infile == -1 ) return;
  status = ioctl( infile, 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer );
  close(infile);

  if(status) rpnt->serial = no_serial;

  pagestart = buffer + 8;

  rpnt->serial = (char *) malloc(pagestart[3] + 1);
  memcpy(rpnt->serial, pagestart+4, pagestart[3]);

  /*
   * NULL terminate and trim trailing whitespace.
   */
  rpnt->serial[pagestart[3]] = '\0';
  for(i = pagestart[3] - 1; i >= 0 && rpnt->serial[i] == ' '; i-- )
      rpnt->serial[i] = '\0';

  /*
   * Copy information to identical nodes to avoid excessive probing.
   */
  for( rpnt1 = reglist; rpnt1; rpnt1 = rpnt1->next )
    {
      if( rpnt1 == rpnt ) continue;
      if( rpnt1->alias != NULL ) continue;
      if( rpnt1->id != rpnt->id ) continue;
      if( rpnt1->chan != rpnt->chan ) continue;
      if( rpnt1->hostnum != rpnt->hostnum ) continue;
      if( rpnt1->hostid != rpnt->hostid ) continue;
      if( rpnt1->serial == NULL ) rpnt1->serial = rpnt->serial;
    }
#endif
}

int inquire(struct regnames * rpnt)
{
#ifdef DEBUG
  /*
   * Fill in some entries just so that I can test this.
   */
  if(rpnt->id == 0 ) {
    rpnt->manufacturer = strdup("CONNER");
    rpnt->model=strdup("CFP4207S");
  } else if(rpnt->id == 2 ) {
    rpnt->manufacturer = strdup("HP");
    rpnt->model=strdup("C4324/C4325");
  } else if(rpnt->id == 5 ) {
    rpnt->manufacturer = strdup("WANGTEK");
    rpnt->model=strdup("5150ES");
  }
#else
  int status, i;
  char c;
  unsigned char *cmd;
  unsigned char * pagestart;
  unsigned char tmp;
  unsigned char buffer[1024];
  char path[64];
  struct regnames * rpnt1;
  int infile;

  memset(buffer, 0, sizeof(buffer));
  
  *( (int *)  buffer )		= 0;	/* length of input data */
  *( ((int *) buffer) + 1 )	= 1024;	/* length of output buffer */

  cmd = (char *) ( ((int *) buffer) + 2 );
  
  cmd[0] = 0x12;			/* INQUIRY */
  cmd[1] = 0x00;			/* lun=0, evpd=0 */
  cmd[2] = 0x00;			/* page code = 0 */
  cmd[3] = 0x00;			/* (reserved) */
  cmd[4] = 0xff;			/* allocation length */
  cmd[5] = 0x00;			/* control */

  strcpy(path, "/dev/scsi/");
  strcat(path, rpnt->name);
  infile = open(path, O_RDONLY);
  if( infile == -1 ) return;

  status = ioctl( infile, 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer );

  close(infile);

  if(status) return;

  pagestart = buffer + 8;

  for( i = 15; i >= 8 && pagestart[i] == ' '; i-- ) 
    pagestart[i] = 0;

  c = pagestart[16];
  pagestart[16] = 0;
  rpnt->manufacturer = strdup(pagestart+8);
  pagestart[16] = c;

  for( i = 31; i >= 16 && pagestart[i] == ' '; i-- ) 
    pagestart[i] = 0;
  c = pagestart[32];
  pagestart[32] = 0;
  rpnt->model = strdup(pagestart+16);
  pagestart[32] = c;

  /*
   * Copy information to identical nodes to avoid excessive probing.
   */
  for( rpnt1 = reglist; rpnt1; rpnt1 = rpnt1->next )
    {
      if( rpnt1 == rpnt ) continue;
      if( rpnt1->alias != NULL ) continue;
      if( rpnt1->id != rpnt->id ) continue;
      if( rpnt1->chan != rpnt->chan ) continue;
      if( rpnt1->hostnum != rpnt->hostnum ) continue;
      if( rpnt1->hostid != rpnt->hostid ) continue;
      if( rpnt1->manufacturer == NULL ) 
	rpnt1->manufacturer = rpnt->manufacturer;
      if( rpnt1->model == NULL ) rpnt1->model = rpnt->model;
    }
  return;
#endif
}
