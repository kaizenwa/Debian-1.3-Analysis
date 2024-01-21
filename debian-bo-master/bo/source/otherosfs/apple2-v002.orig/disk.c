#include <stdio.h>
#include <sys/stat.h>

typedef enum { False, True } Tr;	/* --- Domain of Truth values
					       (using the notation by
					       John Allen -
						"The Anatomy of LISP") --- */

int	media_has_changed_6;	/* New diskette inserted?		     */
int	motor_6;		/* Motor on/off (slot 6) 	       	     */
int	drive_6;		/* Current drive (slot 6)		     */
int	ddrw_6;			/* Data direction (read or write)	     */
int	disk_byte_6;		/* Disk byte (slot 6)			     */
int	volume_6;		/* Disk volume				     */
int	checksum_6;		/* Disk checksum			     */
int	old_value_6;
int	exor_value_6;
int	wr_trk_6, wr_sec_6;

int	protected_6[2];		/* Drive A/B is protected		     */
int	track_6[2];		/* Drive A/B current track 		     */
int	sector_6[2];		/* Drive A/B current sector		     */
int	run_byte_6[2];		/* Drive A/B runbyte			     */

FILE	*fp_6[2];		/* Drive A/B file structure pointer	     */
char	file_name_6[2][1024];	/* Drive A/B file name			     */
long	file_pos_6[2];		/* Drive A/B file position		     */
long	file_size_6[2];		/* Drive A/B file size			     */
int	compressed[2];		/* If diskette into Drive A/B was compressed */

unsigned char disk_data_6[258];	/* Disk sector buffer			     */

int	skew_table_6[16] =	/* Sector skew table 		     	     */
	{ 0,7,14,6,13,5,12,4,11,3,10,2,9,1,8,15 };

int	translate_table_6[256] =/* Translation table			     */
	{ 
	    0x96, 0x97, 0x9a, 0x9b, 0x9d, 0x9e, 0x9f, 0xa6,
	    0xa7, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xb2, 0xb3,
	    0xb4, 0xb5, 0xb6, 0xb7, 0xb9, 0xba, 0xbb, 0xbc,
	    0xbd, 0xbe, 0xbf, 0xcb, 0xcd, 0xce, 0xcf, 0xd3,
	    0xd6, 0xd7, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde,
	    0xdf, 0xe5, 0xe6, 0xe7, 0xe9, 0xea, 0xeb, 0xec,
	    0xed, 0xee, 0xef, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6,
	    0xf7, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00, 0x01,
	    0x80, 0x80, 0x02, 0x03, 0x80, 0x04, 0x05, 0x06,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x07, 0x08,
	    0x80, 0x80, 0x80, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
	    0x80, 0x80, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13,
	    0x80, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	    0x80, 0x80, 0x80, 0x1b, 0x80, 0x1c, 0x1d, 0x1e,
	    0x80, 0x80, 0x80, 0x1f, 0x80, 0x80, 0x20, 0x21,
	    0x80, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
	    0x80, 0x80, 0x80, 0x80, 0x80, 0x29, 0x2a, 0x2b,
	    0x80, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32,
	    0x80, 0x80, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
	    0x80, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f
	};

extern unsigned char disk_path[1024];

/* -------------------------------------------------------------------------
    c_init_6()
   ------------------------------------------------------------------------- */

void c_init_6()
{
    media_has_changed_6 = 1;
    fp_6[ 0 ] = fp_6[ 1 ] = NULL;
    track_6[ 0 ] = track_6[ 1 ] = 42;

    if (file_name_6[0][0] == '\0')
        sprintf(file_name_6[0], "%s/disk_s6d1", disk_path);
    if (file_name_6[1][0] == '\0')
        sprintf(file_name_6[1], "%s/disk_s6d2", disk_path);

    motor_6 = 1;
    drive_6 = 0;
    ddrw_6 = 0;
    volume_6 = 254;
}

/* -------------------------------------------------------------------------
    c_new_diskette_6( char *filename )
   ------------------------------------------------------------------------- */

void c_new_diskette_6( int drive, char *file_name, Tr cmpr )
{
    static char cmd[4096];

    if (compressed[drive])
    {
        sprintf(cmd, "gzip '%s'", file_name_6[drive]);
	system(cmd);
    }

    strcpy(file_name_6[drive], file_name);
    compressed[ drive ] = cmpr;
    media_has_changed_6 = 1;
}
 
/* -------------------------------------------------------------------------
    c_open_files_6()
   ------------------------------------------------------------------------- */

void c_open_files_6()
{
    int		i;
    struct stat	buf;

    if (media_has_changed_6)
    {
	media_has_changed_6 = 0;
	
	for (i = 0; i < 2; i++) /* Iterate over drive A and B */
	{
	    if (fp_6[i])		/* Close files */
		fclose(fp_6[i]);

	    if (stat(file_name_6[i], &buf) < 0)
		fp_6[i] = NULL;
	    else
	    {
		file_size_6[i] = buf.st_size;	/* Get file size */

		/* Open for read AND write */
		fp_6[i] = fopen(file_name_6[i], "r+");
		if (fp_6[i] == NULL)
		{
	     	    /* Failed. Open only for read */

		    fp_6[i] = fopen(file_name_6[i], "r");
		    protected_6[i] = 1; /* Disk is write protected! */
		}
		else
		    protected_6[i] = 0;
	    }

	    sector_6[i] = 0;    /* Initialize drive values */
	    run_byte_6[i] = 0;
	} /* End for */
    } /* End if */
}

/* -------------------------------------------------------------------------
    c_read_byte_6()
   ------------------------------------------------------------------------- */

unsigned char c_read_byte_6()
{
    int			position;

    unsigned char	value = 0;

    c_open_files_6();

    if (fp_6[drive_6] == NULL)
	return 0xFF;		/* Return FF if there is no disk in drive */

    if (motor_6)		/* Motor turned on? */
    {
	if (motor_6 > 99)
	    return 0;
	else
	    motor_6++;
    }

    /* The run byte tells what's to do */

/*
    printf("Run byte: %d (drive %d)\n", run_byte_6[drive_6], drive_6 );
*/

    switch (run_byte_6[drive_6])
    {
	case 0: case 1: case 2: case 3: case 4: case 5:
	case 20: case 21: case 22: case 23: case 24:
	    /* Sync */
	    value = 0xFF;
	    break;

	case 6: case 25:
	    /* Prologue (first byte) */
	    value = 0xD5;
	    break;

	case 7: case 26:
	    /* Prologue (second byte) */
	    value = 0xAA;
	    break;

	case 8:
	    /* Prologue (third byte) */
	    value = 0x96;
	    break;

	case 9:
	    /* Volume (encoded) */
	    value = (volume_6 >> 1) | 0xAA;
	    checksum_6 = volume_6;
	    break;

	case 10:
	    /* Volume (encoded) */
	    value = volume_6 | 0xAA;
	    break;

	case 11:
/*
	    printf("Track number: %d\n", track_6[drive_6]);
*/
	    /* Track number (encoded) */
	    checksum_6 ^= (track_6[ drive_6 ] >> 1);
	    value = (track_6[ drive_6 ] >> 2) | 0xAA;
	    break;

	case 12:
	    /* Track number (encoded) */
	    value = (track_6[drive_6] >> 1) | 0xAA;
	    break;

	case 13:
	    /* Sector number (encoded) */
	    checksum_6 ^= sector_6[ drive_6 ];
	    value = (sector_6[ drive_6 ] >> 1) | 0xAA;
	    break;

	case 14:
	    /* Sector number (encoded) */
	    value = sector_6[ drive_6 ] | 0xAA;
	    break;

	case 15:
	    /* Checksum */
	    value = (checksum_6 >> 1) | 0xAA;
	    break;

	case 16:
	    /* Checksum */
	    value = checksum_6 | 0xAA;
	    break;

	case 17: case 371:
	    /* Epilogue (first byte) */
	    value = 0xDE;
	    break;

	case 18: case 372:
	    /* Epilogue (second byte) */
	    value = 0xAA;
	    break;

	case 19: case 373:
	    /* Epilogue (third byte) */
	    value = 0xEB;
	    break;

	case 27:
	    /* Data header */
	    exor_value_6 = 0;

	    /* Set file position variable */
	    file_pos_6[drive_6] = 256 * 16 * (track_6[drive_6] >> 1) +
				  256 * skew_table_6[ sector_6[drive_6] ];

	    /* File large enough? */
	    if (file_pos_6[drive_6] + 255 > file_size_6[drive_6])
	        return 0xFF;

	    /* Set position */
	    fseek( fp_6[drive_6], file_pos_6[drive_6], SEEK_SET );
	    
	    /* Read sector */
	    fread( disk_data_6, 1, 256, fp_6[drive_6] );
	    disk_data_6[ 256 ] = disk_data_6[ 257 ] = 0;
	    value = 0xAD;
	    break;

	case 370:
	    /* Checksum */
	    value = translate_table_6[exor_value_6 & 0x3F];

	    /* Increment sector number (and wrap if necessary) */
	    sector_6[drive_6]++;
	    if (sector_6[drive_6] == 16)
		sector_6[drive_6] = 0;

	    break;

	default:
	    position = run_byte_6[drive_6] - 28;
	    if (position >= 0x56)
	    {
		position -= 0x56;
		old_value_6 = disk_data_6[ position ];
		old_value_6 = old_value_6 >> 2;
		exor_value_6 ^= old_value_6;
		value = translate_table_6[exor_value_6 & 0x3F];
		exor_value_6 = old_value_6;
	    }
	    else
	    {
		old_value_6 = 0;
		old_value_6 |= (disk_data_6[position] & 0x1) << 1;
		old_value_6 |= (disk_data_6[position] & 0x2) >> 1;
		old_value_6 |= (disk_data_6[position+0x56] & 0x1) << 3;
		old_value_6 |= (disk_data_6[position+0x56] & 0x2) << 1;
		old_value_6 |= (disk_data_6[position+0xAC] & 0x1) << 5;
		old_value_6 |= (disk_data_6[position+0xAC] & 0x2) << 3;
		exor_value_6 ^= old_value_6;
	        value = translate_table_6[exor_value_6 & 0x3F];
		exor_value_6 = old_value_6;
	    }
	    break;
    } /* End switch */

    /* Continue by increasing run byte value */
    run_byte_6[drive_6]++;
    if (run_byte_6[drive_6] > 373)
	run_byte_6[drive_6] = 0;

    return value;
}

/* -------------------------------------------------------------------------
    c_write_byte_6()   disk_byte_6 contains the value
   ------------------------------------------------------------------------- */

void c_write_byte_6()
{
    int		position;

    c_open_files_6();

    if (fp_6[drive_6] == NULL)
	return;		/* Return if there is no disk in drive */

    if (protected_6[drive_6])
	return;		/* Do not write if diskette is write protected */

    if (disk_byte_6 < 0x96)
	return;		/* Only byte values at least 0x96 are allowed */

    if (disk_byte_6 == 0xD5)
	run_byte_6[drive_6] = 6;    /* Initialize run byte value */

    /* The run byte tells what's to do */

    switch (run_byte_6[drive_6])
    {
	case 0: case 1: case 2: case 3: case 4: case 5:
	case 20: case 21: case 22: case 23: case 24:
	    /* Sync */
	    break;

	case 6: case 25:
	    /* Prologue (first byte) */
	    if (disk_byte_6 == 0xFF)
		run_byte_6[drive_6]--;
	    break;

	case 7: case 26:
	    /* Prologue (second byte) */
	    break;

	case 8:
	    /* Prologue (third byte) */
	    if (disk_byte_6 == 0xAD)
		exor_value_6 = 0, run_byte_6[ drive_6 ] = 27;
	    break;

	case 9: case 10:
	    /* Volume */
	    break;

	case 11:
	    /* Track number (encode it) */
	    wr_trk_6 = disk_byte_6 << 1;
	    wr_trk_6 &= 0xFF;
	    wr_trk_6 |= 0x55;
	    break;

	case 12:
	    /* Track number (encode it) */
	    wr_trk_6 &= disk_byte_6;
	    wr_trk_6 = wr_trk_6 << 1;
	    break;

	case 13:
	    /* Sector number (encode it) */
	    wr_sec_6 = disk_byte_6 << 1;
	    wr_sec_6 &= 0xFF;
	    wr_sec_6 |= 0x55;
	    break;

	case 14:
	    /* Sector number (encode it) */
	    wr_sec_6 &= disk_byte_6;
	    sector_6[drive_6] = wr_sec_6;
	    break;

	case 15:
	    /* Checksum */
	    break;

	case 16:
	    /* Checksum */
	    break;

	case 17: case 371:
	    /* Epilogue (first byte) */
	    break;

	case 18: case 372:
	    /* Epilogue (second byte) */
	    break;

	case 19: case 373:
	    /* Epilogue (third byte) */
	    break;

	case 27:
	     exor_value_6 = 0;
	     break;

	case 370:
	    /* Set file position variable */
	    file_pos_6[drive_6] = 256 * 16 * (track_6[drive_6] >> 1) +
				  256 * skew_table_6[ sector_6[drive_6] ];

	    /* Is the file large enough? */
	    if (file_pos_6[drive_6] + 255 > file_size_6[drive_6])
	        return;

	    /* Set position */
	    fseek( fp_6[drive_6], file_pos_6[drive_6], SEEK_SET );

	    /* Write sector */
	    fwrite(disk_data_6, 1, 256, fp_6[drive_6]);
	    fflush( fp_6[drive_6] );

	    /* Increment sector number (and wrap if necessary) */
	    sector_6[drive_6]++;
	    if (sector_6[drive_6] == 16)
		sector_6[drive_6] = 0;
	    break;

	default:
	    position = run_byte_6[drive_6] - 28;
	    disk_byte_6 = translate_table_6[ disk_byte_6 ];
	    if (position >= 0x56)
	    {
		position -= 0x56;
		disk_byte_6 ^= exor_value_6;
		old_value_6 = disk_byte_6;
		disk_data_6[position] |= (disk_byte_6 << 2) & 0xFC;
		exor_value_6 = old_value_6;
	    }
	    else
	    {
		disk_byte_6 ^= exor_value_6;
		old_value_6 = disk_byte_6;
		disk_data_6[position] = (disk_byte_6 & 0x01) << 1;
		disk_data_6[position] |= (disk_byte_6 & 0x02) >> 1;
		disk_data_6[position + 0x56] = (disk_byte_6 & 0x04) >> 1;
		disk_data_6[position + 0x56] |= (disk_byte_6 & 0x08) >> 3;
		disk_data_6[position + 0xAC] = (disk_byte_6 & 0x10) >> 3;
		disk_data_6[position + 0xAC] |= (disk_byte_6 & 0x20) >> 5;
		exor_value_6 = old_value_6;
	    }
	    break;
    } /* End switch */

    run_byte_6[ drive_6 ]++;
    if (run_byte_6[ drive_6 ] > 373)
	run_byte_6[ drive_6 ] = 0;
}
