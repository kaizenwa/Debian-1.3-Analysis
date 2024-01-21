/****************************************************************************/
/*                                                                          */
/*                              Device Mounter                              */
/*                                                                          */
/*                              (c) Thomas Uhl                              */
/*                                                                          */
/*                                                                          */
/*                             v1.0 - 7.2.1993                              */
/*                             v1.1 - 28.3.1993                             */
/*                             v2.0 - 17.4.1993                             */
/*                             v2.5 - 5.5.1993                              */
/*                             v3.0 - 22.5.1993                             */
/*                             v3.1 - 7.6.1993                              */
/*                             v3.5 - 14.8.1993                             */
/*                             v3.6 - 20.8.1994                             */
/*                                                                          */
/****************************************************************************/


#include <xview/xview.h>
#include <xview/panel.h>

#include <string.h>
#include <fcntl.h>
#include <assert.h>

#include <mntent.h>
#include <linux/msdos_fs.h>
#include <linux/minix_fs.h>
#include <linux/ext_fs.h>
#include <linux/ext2_fs.h>
#include <linux/xia_fs.h>
#include <linux/iso_fs.h>

#include "iconimage.h"
#include "iconmask.h"

#define VERSION "3.6"

#define SEC_SIZE 1024
#define BUF_SIZE SEC_SIZE * 2    
#define MOUNTED_LOCK "/etc/mtab~"
#define CONFIG_FILE "xvmounttab"
#define XVMOUNTPATH "XVMOUNTPATH"

#define BOOL int
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define UNKNOWN 0
#define MINIX 1
#define EXT 2
#define MSDOS 3
#define EXT2 4
#define XIAFS 5
#define ISOFS 6

/* MS-DOS Filesystem */
#define CLUSTERSIZE 8
#define FATS 2
#define HEADS 32
#define SECTORSIZE 1024
#define TRACKSIZE 128

#define STRLEN 80
#define LINELEN 255
#define MAXITEMS 16

#define ITEM_KEY 1000
#define ITEM_NR_KEY 1001

#define AUTO "auto"

#define streq(s, t)     (strcmp ((s), (t)) == 0)

/* types */
typedef struct {
  char name[STRLEN];
  char dev[STRLEN];
  char dir[STRLEN];
  char type[STRLEN];
  char opt[STRLEN];
  BOOL mounted;
  Panel_item Label, Button;
} MITEM;

typedef struct {
  const char *opt;              /* option name */
  int  inv;                     /* true if flag value should be inverted */
  int  mask;                    /* flag mask value */
} OPTMAP;


/* function prototypes */
int check_msdos ();
int check_minix ();
int check_ext ();
int check_fs_trype (char *);
void check_mounted_fs ();
BOOL do_mount (char *, char*, char *, char *, Panel_item);
BOOL do_umount (char *, Panel_item);
Icon create_icon (char *);
void create_buttons ();
void parse_opt (char *, int *, char *);
void parse_opts (char *, int *, char *);

/* callback prototypes */
void but_cb (Panel_item, Event*);

/* global variables */
Frame frame;
Panel panel;
Panel_item Floppy0Txt, Floppy1Txt;
Panel_item Floppy0But, Floppy1But;

int devices;              /* number of devices */
MITEM mitems[MAXITEMS];   /* device list*/

char *fs_types[] = {
  "unknown",
  "minix",
  "ext",
  "msdos",
  "ext2",
  "xiafs",
  "iso9660"
};

const OPTMAP opt_map[] =
{
  { "defaults", 0, 0            },      /* default options */
  { "ro",       0, MS_RDONLY    },      /* read-only */
  { "rw",       1, MS_RDONLY    },      /* read-write */
  { "exec",     1, MS_NOEXEC    },      /* permit execution of binaries */
  { "noexec",   0, MS_NOEXEC    },      /* don't execute binaries */
  { "suid",     1, MS_NOSUID    },      /* honor suid executables */
  { "nosuid",   0, MS_NOSUID    },      /* don't honor suid executables */
  { "dev",      1, MS_NODEV     },      /* interpret device files  */
  { "nodev",    0, MS_NODEV     },      /* don't interpret devices */
  { "sync",     0, MS_SYNC      },      /* synchronous I/O */
  { "async",    1, MS_SYNC      },      /* asynchronous I/O */
#ifdef MS_NOSUB
  { "sub",      1, MS_NOSUB     },      /* allow submounts */
  { "nosub",    0, MS_NOSUB     },      /* don't allow submounts */
#endif
  /* add new options here */
  { NULL,       0, 0            }
};


main (int argc, char *argv[])
{
  Icon icon;

  xv_init (XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
  
  frame = (Frame) xv_create (XV_NULL, FRAME,
                                      FRAME_LABEL, "Tom's Device Mounter v" 
			                           VERSION,
                                      FRAME_SHOW_FOOTER, TRUE, 
                                      FRAME_SHOW_RESIZE_CORNER, FALSE,
                                      NULL);

  icon = create_icon (argv[0]);

  panel = (Panel) xv_create (frame, PANEL, 
                                    XV_WIDTH, 300,
                                    NULL);

  create_buttons ();

  xv_set (frame, FRAME_ICON, icon, NULL);

  window_fit (panel);
  window_fit (frame);

  xv_main_loop (frame);
}


void but_cb (Panel_item item, Event *event)
{
  MITEM *mitem;

#ifdef DEBUG
  printf ("Selected Item-Nr: %d\n", xv_get (item, XV_KEY_DATA, ITEM_NR_KEY));
#endif

  mitem = (MITEM *) xv_get (item, XV_KEY_DATA, ITEM_KEY);

  if (!mitem->mounted)
  {
    if (mitem->mounted = do_mount (mitem->dev, 
				   mitem->dir, 
                                   mitem->opt,
				   mitem->type,
				   mitem->Label))
      xv_set (item, PANEL_LABEL_STRING, "umount", NULL);
  }
  else
  {
    if (!(mitem->mounted = !do_umount (mitem->dev, mitem->Label)))
      xv_set (item, PANEL_LABEL_STRING, "mount", NULL);
  }
}


BOOL do_mount (char *dev, char* mdir, char *opt, char *type, Panel_item item)
{
  int flags = 0, ret;
  char extra_opts[STRLEN];
  struct mntent mnt;
  FILE *F_mtab;
  char msg[STRLEN], tmptype[STRLEN];

  strcpy (tmptype, type);

  /* autodetect? */
  if (streq (type, AUTO))
  {
    ret = check_fs_type (dev);

    if (ret < 0)
      sprintf (msg, "Check FS: %s", strerror (errno));
    else
      strcpy (tmptype, fs_types[ret]);
  } 
    
  sprintf (msg, "Mounted Filesystemtype: %s", tmptype);
  xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);

  if (ret == UNKNOWN || ret < 0)
    return FALSE;

  parse_opts (opt, &flags, extra_opts);

#ifdef DEBUG
  printf ("mount (<%s>, <%s>, <%s>, %d, <%s>)\n", 
          dev, mdir, tmptype, flags, extra_opts);
#endif

#ifndef DONT_MOUNT
  if (mount (dev, mdir, tmptype, flags | MS_MGC_VAL, extra_opts) < 0)
  {
    sprintf (msg, "Mount: %s", strerror (errno));
    xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
    return FALSE;
  }

  if ((F_mtab = setmntent (MOUNTED, "a+")) == NULL)
  {
    sprintf (msg, "Can't open %s: %s", MOUNTED, strerror (errno));
    xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
    /* simulate successful operation */
    return TRUE;
  }

  mnt.mnt_fsname = dev;
  mnt.mnt_dir = mdir;
  mnt.mnt_type = tmptype;
  mnt.mnt_opts = opt;
  mnt.mnt_freq = 0;
  mnt.mnt_passno = 0;
  addmntent (F_mtab, &mnt);
  endmntent (F_mtab);
#endif

  sprintf (msg, "%s FS mounted", tmptype);
  xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
  xv_set (item, PANEL_VALUE, tmptype, NULL);

  return TRUE;
}


BOOL do_umount (char *dev, Panel_item item)
{
  FILE *F_mtab, *F_lock;
  struct mntent *mnt;
  char msg[80];

  sync ();

#ifdef DEBUG
  printf ("umount (<%s>)\n", dev);
#endif

#ifndef DONT_MOUNT
  if (umount (dev) < 0)
  {
    sprintf (msg, "Unmount: %s", strerror (errno));
    xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
    return FALSE;
  }

  if ((F_mtab = setmntent (MOUNTED, "r")) == NULL)
  {
    sprintf (msg, "Can't open %s: %s", MOUNTED, strerror (errno));
    xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
    /* simulate successful operation */
    return TRUE;
  }
  if ((F_lock = setmntent (MOUNTED_LOCK, "w+")) == NULL)
  {
    sprintf (msg, "Can't open %s: %s", MOUNTED_LOCK, strerror (errno));
    xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
    /* simulate successful operation */
    return TRUE;
  }

  while (mnt = getmntent (F_mtab))
    if (strcmp (dev, mnt->mnt_fsname))
      addmntent (F_lock, mnt);
  endmntent (F_mtab);
  endmntent (F_lock);

  if (rename (MOUNTED_LOCK, MOUNTED) < 0)
  {
    sprintf (msg, "Can't rename %s to %s: %s", MOUNTED_LOCK, MOUNTED, 
                                               strerror (errno));
    xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
    /* simulate successful operation */
    return TRUE;
  }
#endif

  sprintf (msg, "%s unmounted", dev);
  xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
  xv_set (item, PANEL_VALUE, "unmounted" , NULL);

  return TRUE;
}


int check_fs_type (char *dev)
{
  int fd, ret;
  char *b;
  unsigned char fats;
  unsigned short cluster_size, heads, track_size;
  unsigned long clusters, sectors, sector_size, total_sect;
  struct msdos_boot_sector *msdos_b;
  struct minix_super_block *minix_b;
  struct ext_super_block *ext_b;
  struct ext2_super_block *ext2_b;
  struct xiafs_super_block *xia_b;
  struct iso_volume_descriptor *iso_b;
  struct hs_volume_descriptor *hs_b;

  fd = open (dev, O_RDONLY);
  if (fd > 0)
  {
    b = malloc (BUF_SIZE);
    assert (b);
    memset (b, 0, BUF_SIZE);
    read (fd, b, BUF_SIZE);
    close (fd);
  }
  else
    return -1;

  /* Minix Filesystem */
  minix_b = (struct minix_super_block *) (b + SEC_SIZE);
  if (minix_b->s_magic == MINIX_SUPER_MAGIC ||
      minix_b->s_magic == NEW_MINIX_SUPER_MAGIC)
  {
    free (b);
    return MINIX;
  }
 
  /* Ext Filesystem */
  ext_b = (struct ext_super_block *) (b + SEC_SIZE);
  if (ext_b->s_magic == EXT_SUPER_MAGIC)
  {
    free (b);
    return EXT;
  }

  /* Ext2 Filesystem */
  ext2_b = (struct ext2_super_block *) (b + SEC_SIZE);
  if (ext2_b->s_magic == EXT2_SUPER_MAGIC ||
      ext2_b->s_magic == EXT2_PRE_02B_MAGIC)
  {
    free (b);
    return EXT2;
  }

  /* Xia Filesystem */
  xia_b = (struct xiafs_super_block *) (b);
  if (xia_b->s_magic == _XIAFS_SUPER_MAGIC)
  {
    free (b);
    return XIAFS;
  }

  /* MS-DOS Filesystem */
  msdos_b = (struct msdos_boot_sector *) b; 
  cluster_size = msdos_b->cluster_size;           /* cluster size in sectors */
  fats = msdos_b->fats;                                    /* number of fats */
  heads = msdos_b->heads;                                 /* number of heads */
  sectors = *((unsigned short *) & msdos_b->sectors);   /* number of sectors */
  track_size = msdos_b->secs_track;                            /* track size */
  total_sect = msdos_b->total_sect;         /* total sectors if sectors == 0 */
  sector_size = *((unsigned short *) &msdos_b->sector_size);   /* sector size*/

  if ((cluster_size && cluster_size <= CLUSTERSIZE) && 
      (fats && fats <= FATS) && 
      (heads && heads <= HEADS) && 
      (sectors || total_sect) &&
      (sector_size || sector_size <= SECTORSIZE) &&
      (track_size && track_size <= TRACKSIZE)) 
  {
    free (b);
    return MSDOS;
  }

  /* Seek to $8000 */
  fd = open (dev, O_RDONLY);
  if (fd > 0)
  {
    ret = lseek (fd, 0x8000, SEEK_SET);
    memset (b, 0, BUF_SIZE);
    read (fd, b, BUF_SIZE);
    close (fd);
  }
  else
    return -1;

  /* ISO Filesystem */
  iso_b = (struct iso_volume_descriptor *) (b);
  if (strncmp (iso_b->id, ISO_STANDARD_ID, sizeof (iso_b->id)) == 0)
  {
    free (b);
#ifdef DEBUG
    printf ("ISO9660\n");
#endif
    return ISOFS; 
  }

  /* High Sierra Filesystem */
  hs_b = (struct hs_volume_descriptor *) (b);
  if (strncmp (hs_b->id, HS_STANDARD_ID, sizeof (hs_b->id)) == 0)
  {
    free (b);
#ifdef DEBUG
    printf ("HIGH SIERRA\n");
#endif
    return ISOFS; 
  }

  free (b);
  return UNKNOWN;
}


void parse_opt (char *opt, int *mask, char *extra_opts)
{
  const OPTMAP *om;

  for (om = opt_map; om->opt != NULL; ++om)
    if (streq (opt, om->opt))
    {
      if (om->inv)
        *mask &= ~om->mask;
      else
        *mask |= om->mask;
      return;
    }

  if (*extra_opts)
    strcat (extra_opts, ",");
  strcat (extra_opts, opt);
}


void parse_opts (char *opts, int *flags, char *extra_opts)
{
  char *opt, *lopts;

  lopts = strdup (opts);
  assert (lopts);

  *extra_opts = '\0';

  if (!streq (lopts, "defaults"))
  {
    for (opt = strtok (lopts, ","); 
	 opt != NULL; 
	 opt = strtok (NULL, ","))
      parse_opt (opt, flags, extra_opts);
  }

  free (lopts);
}


Icon create_icon (char *label)
{
  Icon icon;
  Server_image icon_image, mask_image;

  if (strrchr (label, '/'))
  {
    label = strrchr (label, '/');
    label++;
  }

  icon_image = (Server_image) xv_create (XV_NULL, SERVER_IMAGE,
                                         XV_WIDTH, iconimage_width,
                                         XV_HEIGHT, iconimage_height,
                                         SERVER_IMAGE_X_BITS, iconimage_bits,
                                         NULL);

  mask_image = (Server_image) xv_create (XV_NULL, SERVER_IMAGE,
                                         XV_WIDTH, iconmask_width,
                                         XV_HEIGHT, iconmask_height,
                                         SERVER_IMAGE_X_BITS, iconmask_bits,
                                         NULL);

  icon = (Icon) xv_create (XV_NULL, ICON,
                                    ICON_IMAGE, icon_image,
                                    ICON_LABEL, label,
                                    ICON_MASK_IMAGE, mask_image,
                                    NULL);
  return icon;
}


void create_buttons ()
{
  int i, lin = 1, res;
  FILE *fd;
  char *cp;
  char path[LINELEN];
  char line[LINELEN];

  /* Create file path for xvmounttab */
  if ((cp = getenv (XVMOUNTPATH)) != NULL)
  {
    res = strlen (cp) - 1;
    if (cp[res] == '/')
      cp[res] = '\0';
    sprintf (path, "%s/%s", cp, CONFIG_FILE); 
  } else {
    sprintf (path, "/etc/%s", CONFIG_FILE);
  }

  devices = 0;

  /* Read devices in xvmounttab */
  if (fd = fopen (path, "r"))
  {
    while ((cp = fgets (line, LINELEN, fd)) && devices < MAXITEMS)
    {
      while (*cp)
      {
        if (*cp == '#' || *cp == '\n')
          *cp = '\0';
        cp++;
      }
      
      if (line[0])
      {
        res = sscanf (line, "%s%s%s%s%s", &mitems[devices].name, 
                                          &mitems[devices].dev, 
                                          &mitems[devices].dir,
		                          &mitems[devices].type,
                                          &mitems[devices].opt);
        if (res > 0 && res < 5)
        {
          fprintf (stderr, "xvmount: error in line %d\n", lin);
          exit (1);
        }

#ifdef DEBUG
        printf ("%s - %s - %s - %s - %s\n", mitems[devices].name, 
                                            mitems[devices].dev, 
		                            mitems[devices].dir,
		                            mitems[devices].type,
                                            mitems[devices].opt);
#endif

        devices++;
      }
      lin++;
    }
    fclose (fd);
  } else {
    fprintf (stderr, "xvmount: %s %s\n", path, strerror (errno));
    exit (1);
  }

  /* Create buttons in panel */
  for (i = 0; i < devices; i++)
  {
    mitems[i].mounted = FALSE;
    mitems[i].Label = xv_create (panel, PANEL_TEXT,
                                 PANEL_LABEL_STRING, mitems[i].name,
                                 PANEL_VALUE_DISPLAY_WIDTH, 90,
                                 PANEL_VALUE, "unmounted",
                                 PANEL_VALUE_UNDERLINED, FALSE,
                                 PANEL_READ_ONLY, TRUE,
                                 NULL);

     mitems[i].Button = xv_create (panel, PANEL_BUTTON,
                                   XV_X, 170,
                                   PANEL_LABEL_STRING, "mount",
                                   PANEL_NOTIFY_PROC, but_cb,
                                   XV_KEY_DATA, ITEM_KEY, &mitems[i],
                                   XV_KEY_DATA, ITEM_NR_KEY, i,
                                   NULL);
  }

  check_mounted_fs ();
}


void check_mounted_fs ()
{
  int i;
  FILE *F_mtab;
  struct mntent *mnt;
  char msg[80];

  if ((F_mtab = setmntent (MOUNTED, "r")) == NULL)
  {
    sprintf (msg, "Can't open %s: %s", MOUNTED, strerror (errno));
    xv_set (frame, FRAME_LEFT_FOOTER, msg, NULL);
    return;
  }

  while (mnt = getmntent (F_mtab))
  {
    for (i = 0; i < devices; i++)
    {
      if (!strcmp (mitems[i].dev, mnt->mnt_fsname))
      {
        /* fs is already mounted */
        mitems[i].mounted = TRUE;
        xv_set (mitems[i].Label, PANEL_VALUE,  mnt->mnt_type, NULL);
        xv_set (mitems[i].Button, PANEL_LABEL_STRING, "umount", NULL);
      }
    }
  }

  endmntent (F_mtab);
}
