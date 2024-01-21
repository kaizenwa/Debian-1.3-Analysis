/*
 * syslinux.c
 *
 * Creates a Linux bootable MS-DOS floppy
 *
 * Copyright (C) 1994-96 H. Peter Anvin
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <dos.h>
#include <string.h>

extern char boot_sector[];
extern char ldlinux_sys[];
extern unsigned int ldlinux_len;

#define BOOTSECT_LEN    512         /* Length of the boot sector */
#define RETRIES         4           /* Make 4 read/write attempts */

typedef unsigned int word;
typedef unsigned char byte;
typedef unsigned long dword;

struct bootsector
{
    byte jump[3];
    byte oemname[8];
    word bytespersec;
    byte secperclust;
    word ressectors;
    byte fats;
    word rootdirents;
    word sectors;
    byte media;
    word fatsecs;
    word secpertrack;
    word heads;
    dword hiddensecs;
    dword hugesectors;
    byte drivenumber;
    byte reserved1;
    byte bootsignature;
    dword volumeid;
    byte volumelabel[11];
    byte filesystype[8];
    byte bootstrap_code[448];
    word boot_magic;
};

struct diskio
{
    unsigned long sector;
    unsigned count;
    void far *buffer;
};

/*
 * read_boot_sect:  read the boot sector of the specified drive into the
 *                  given buffer, using the DOS function INT 25h
 */
int read_boot_sect(int drive, void *buffer, int huge_disk)
{
    int retry_ctr, err;
    struct diskio huge_buf;
    unsigned sectors;

    if ( huge_disk )
    {
        huge_buf.buffer = buffer;
        huge_buf.sector = 0;
        huge_buf.count = 1;
        buffer = &huge_buf;
        sectors = 0xFFFF;
    }
    else
        sectors = 1;            /* Read one sector */

    err = 0;
    for ( retry_ctr = RETRIES ; retry_ctr ; retry_ctr-- )
    {
        _BX = (unsigned) buffer;
        _CX = sectors;
        _DX = 0;                    /* Sector zero = boot sector */
        _AL = drive;
        geninterrupt(0x25);
        asm pop ax;                 /* Flags left on the stack */
        asm sti;                    /* Make sure interrupts are on */
        asm jnc read_ok;
    }
    err = 1;                        /* Operation failed */
read_ok:
    return err;
}

/*
 * write_boot_sect:  read the boot sector of the specified drive into the
 *                   given buffer, using the DOS function INT 26h
 */
int write_boot_sect(int drive, void *buffer, int huge_disk)
{
    int retry_ctr, err, flg;
    struct diskio huge_buf;
    unsigned sectors;

    if ( huge_disk )
    {
        huge_buf.buffer = buffer;
        huge_buf.sector = 0;
        huge_buf.count = 1;
        buffer = &huge_buf;
        sectors = 0xFFFF;
    }
    else
        sectors = 1;            /* Write one sector */


    err = 0;
    for ( retry_ctr = RETRIES ; retry_ctr ; retry_ctr-- )
    {
        _BX = (unsigned) buffer;
        _CX = sectors;
        _DX = 0;                    /* Sector zero = boot sector */
        _AL = drive;
        geninterrupt(0x26);
        asm pop ax;                 /* Flags left on the stack */
        asm sti;                    /* Make sure interrupts are on */
        asm jnc write_ok;
    }
    err = 1;                        /* Operation failed */
write_ok:
    return err;
}

/*
 * disk_is_huge:    Check if a disk is larger than 64K sectors (32 Mb)
 */
int disk_is_huge(int drive, int *sectsize)
{
    unsigned secperclust, bytespersec, clusters;
    unsigned long sectors;

    _DL = drive+1;
    _AH = 0x36;
    geninterrupt(0x21);
    *sectsize = _CX;
    asm mul dx;

    return _DX;
}

/*
 * usage:   Complain to the user, and exit
 */
void usage(void)
{
    fprintf(stderr, "Usage:\n"
                    "         SYSLINUX drive:\n");
    exit(1);
}

/*
 * Main program
 *
 */
int main(int argc, char *argv[])
{
    int drive, huge_disk, sectsize;
    char *cmd_ptr;
    int cmd_maxlen, cmd_len;
    struct bootsector *orig_sect;
    static char *ld_name = "?:\LDLINUX.SYS";
    FILE *ld;
    int i;
    unsigned dosattr;


    /* If no drive specified */
    if ( argc < 2 || strlen(argv[1]) != 2 || argv[1][1] != ':' )
        usage();

    drive = argv[1][0] | 0x20;
    if ( drive < 'a' || drive > 'z' )
        usage();

    if ( argc > 2 )
    {
        fprintf(stderr,
        "Warning: specifying the default command on the SYSLINUX command\n"
        "         line is no longer supported.  Create a SYSLINUX.CFG file\n"
        "         with a \"default\" line instead.  The default kernel name\n"
        "         in the absence of a SYSLINUX.CFG file is now always\n"
        "         \"linux\".\n");
    }

    drive -= 'a';
    huge_disk = disk_is_huge(drive, &sectsize);

    orig_sect = malloc(sectsize);
    if ( !orig_sect )
    {
        fprintf(stderr, "Error: could not malloc %d bytes\n", sectsize);
        exit(1);
    }

    /* Read in the original boot sector */
    if ( read_boot_sect(drive, orig_sect, huge_disk) )
    {
        fprintf(stderr, "Error: could not read original boot sector\n");
        exit(1);
    }

    /* Make sure we're dealing with something sane here */
    if ( orig_sect->bytespersec != 512 )
    {
        fprintf(stderr, "Error: Bytes per sector is not 512\n");
        exit(1);
    }
    if ( orig_sect->boot_magic != 0xAA55 )
    {
        fprintf(stderr, "Error: Boot sector signature not found on disk\n");
        exit(1);
    }
    if ( orig_sect->bootsignature == 0x29 &&
         memcmp(orig_sect->filesystype, "FAT12   ", 8) )
    {
        fprintf(stderr, "Error: Filesystem is not type FAT12 (or "
                        "ancient format program)\n");
        exit(1);
    }

    /* Very well, copy the thing */
    memcpy( &boot_sector[0x0B], &(orig_sect->bytespersec), 0x33 );

    /* Write out the new boot sector */
    if ( write_boot_sect(drive, boot_sector, huge_disk) )
    {
        fprintf(stderr, "Error: could not write new boot sector\n");
        exit(1);
    }

    /* Get full name of LDLINUX.SYS */
    *ld_name = drive+'A';

    /* Clear readonly, hidden, system flags (but remember them) */
    /* If these call fail, assume it is because the file doesn't exist */
    if (_dos_getfileattr(ld_name, &dosattr))
        dosattr = FA_ARCH;      /* default attributes */
    else
    {
        dosattr &= ~(FA_RDONLY|FA_HIDDEN|FA_SYSTEM);
        _dos_setfileattr(ld_name, dosattr);
    }

    unlink(ld_name);            /* If it exists, delete it */
    ld = fopen(ld_name, "wb");
    if ( !ld )
    {
        perror("Writing LDLINUX.SYS");
        exit(1);
    }
    if ( fwrite(ldlinux_sys,1,ldlinux_len,ld) < ldlinux_len )
    {
        perror("Writing LDLINUX.SYS");
        fclose(ld);
        exit(1);
    }
    fclose(ld);

    /* Set readonly flag - if you want to have the hidden flag set by
       default, you can add FA_HIDDEN here.  I suggest not setting FA_SYSTEM
       since it implies a file that must not be moved, which is not the
       case for LDLINUX.SYS. */
    _dos_setfileattr(ld_name, dosattr|FA_ARCH|FA_RDONLY);

    fprintf(stderr, "The Linux boot system was successfully installed.\n");
    return 0;
}

