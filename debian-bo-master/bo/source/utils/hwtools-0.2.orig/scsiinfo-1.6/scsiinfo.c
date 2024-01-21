/* * This program reads various mode pages and bits of other
 * information from a scsi device and interprets the raw data for you
 * with a report written to stdout.  Usage:
 *
 * ./scsiinfo [options] /dev/sda2
 *
 * Options are:
 * -c    Display information from Cache control page.
 * -C    Display information from Control Page.
 * -d    Display defect lists.
 * -e    Display information from error recovery page.
 * -f    Display information from Format Device Page.
 * -g    Display information from Hard disk geometry page.
 * -i    Display all information from Inquiry command.
 * -s    Display all information from unit serial number page.
 * -D    Display information from disconnect-reconnect page.
 * -n    Display information from notch parameters page.
 * -p    Display information from Peripheral Device Page.
 * -V    Display information from Verify Error Recovery Page.
 * -v    Show version number
 * -a    All of the above.
 * -l    List known scsi devices on the system
 * -L    List pages supported by program and target
 *
 * Only one of the following three options can be specified.
 * None of these three implies the current values are returned.
 * -m    Display modifiable fields instead of current values
 * -M    Display manufacturer defaults instead of current values
 * -S    Display saved defaults instead of current values
 *
 * -X    Display output suitable for the X-based interface.
 * -R    Replace parameters - best used with -X
 *
 * Compile with command: gcc -O2 -o scsiinfo scsiinfo.c
 * You need to be able to open the scsi device as a file.  
 * Depending on the permissions, you may need root privileges
 * to run this.
 *
 * Eric Youngdale - 11/1/93.  Version 1.0.
 *
 * Version 1.1: Ability to change parameters on cache page, support for
 *  X front end.
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2, or (at your option)
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  
 *
 * Michael Weller (eowmob@exp-math.uni-essen.de) 11/23/94 massive extensions from 1.4a
 *
 */

#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <linux/hdreg.h>

FILE *infile;
char *device_name;
unsigned char buffer[64*1024+100];
unsigned char buffer1[10*1024+100];
char cache = 0;
char defect = 0;
char geometry = 0;
char format = 0;
char error = 0;
char disconnect = 0;
char control = 0;
char inquiry = 0;
char serial_number = 0;
char default_param = 0;
char modifiable = 0;
char saved = 0;
char x_interface = 0;
char replace = 0;
char notch = 0;
char list = 0;
char list_pages = 0;
char verify = 0;
char peripheral = 0;
char save_mode = 0; /* All mode pages output only a single line, and preceed
		       the output with the appropriate restore command */

char *page_names[] = {
	NULL,
	"Read-Write Error Recovery",
	"Disconnect-Reconnect",
	"Format Device",
	"Rigid Disk Geometry",
	/* "Flexible Disk" */ NULL,
	NULL,
	"Verify Error Recovery",
	"Caching",
	"Peripheral Device",
	"Control Mode",
	/* "Medium Types Supported" */ NULL,
	"Notch and Partition",
	/* "CD-ROM" */ NULL,
	/* "CD-ROM Audio Control" */ NULL,
	NULL,
	/* "Medium Partition (1)" */ NULL,
	/* "Medium Partition (2)" */ NULL,
	/* "Medium Partition (3)" */ NULL,
	/* "Medium Partition (4)" */ NULL};

#define MAX_PAGENO (sizeof(page_names)/sizeof(char *))

#if 0
char *log_names[] = {
	"Supported Log Pages ",
	"Buffer Over-Run/Under-Run ",
	"Error Counter Write ",
	"Error Counter Read ",
	"Error Counter Read Reverse ",
	"Error Counter Verify ",
	"Non-Medium Error ",
	"Last n Error Events "};

#define MAX_LOGNO (sizeof(log_names)/sizeof(char *))
#endif

int hexdata_ptr = -1; /* We allow for one hexdata (@) field.. this is where we
			 parsed it */
int next_parameter;
int n_replacement_values;
unsigned long long replacement_values[16];

/*  These are the mode pages for direct access devices (i.e. disks). 
 *  Asterisks mark those which this program can currently read and interpret.
 *  Section 7 is generic scsi, section 8 is hard disk, 13 is cdrom and 9 is
 *  tape.

*  01h        Read-Write Error Recovery Page                        8.3.3.6
*  02h        Disconnect-Reconnect Page                             7.3.3.2
*  03h        Format Device Page                                    8.3.3.3
*  04h        Rigid Disk Geometry Page                              8.3.3.7
   05h        Flexible Disk Page                                    8.3.3.2
*  07h        Verify Error Recovery Page                            8.3.3.8
*  08h        Caching Page                                          8.3.3.1
*  09h        Peripheral Device Page                                7.3.3.3
*  0Ah        Control Mode Page                                     7.3.3.1
   0Bh        Medium Types Supported Page                           8.3.3.4
*  0Ch        Notch and Partition Page                              8.3.3.5
   0Dh        CD-ROM Page                                          13.3.3.2
   0Eh        CD-ROM Audio Control Page                            13.3.3.1
   10h        Device Configuration Page                             9.3.3.1
   11h        Medium Partition Page(1)                              9.3.3.2
   12h        Medium Partition Page(2)                              9.3.3.3
   13h        Medium Partition Page(3)                              9.3.3.3
   14h        Medium Partition Page(4)                              9.3.3.3
*/

#define MODE_SENSE 0x1a
#define MODE_SENSE_10 0x5a
#define MODE_SELECT 0x15
#define LOG_SENSE 0x4d

#define SETUP_MODE_PAGE(NPAGE, NPARAM) 		\
  status = get_mode_page(NPAGE, page_code); 	\
  if(status) return status; 			\
  bdlen = buffer[11]; 				\
  pagestart = buffer + 12 + bdlen; 		\
  if(x_interface && replace) { 			\
    if(n_replacement_values != NPARAM) { 	\
      fprintf(stderr,"Wrong number of replacement values\n"); \
      return 0; 				\
    }; 						\
    next_parameter = 1; 			\
  };

/* forward declaration */
void usage(char *);

char *get_page_name(int pageno){
  if((pageno <= 0) || (pageno >= MAX_PAGENO) || (!page_names[pageno]))
    return "Mode";
  return page_names[pageno];
}

#if 0
char *get_log_name(int pageno){
  if((pageno >= MAX_LOGNO) || (!log_names[pageno]))
    return "Unknown ";
  return log_names[pageno];
}
#endif

void dump( void *buffer, unsigned int length){
  unsigned int i;

  for (i=0; i<length; i++)
  {
#if 0
    if(((unsigned char *) buffer)[i] > 0x20)
      printf( " %c ", (unsigned int) ((unsigned char *) buffer)[i] );
    else
#endif
      printf( "%02x ", (unsigned int) ((unsigned char *) buffer)[i] );
    if (i%16 == 15) { printf( "\n" ); }
  }
  printf( "\n" );

}

int getnbyte(unsigned char * pnt, int nbyte){
  unsigned int result;
  int i;
  result = 0;
  for(i=0; i<nbyte; i++)
    result = (result << 8) | (pnt[i] & 0xff);
  return result;
}

int putnbyte(unsigned char * pnt, unsigned int value, unsigned int nbyte){
  int i;

  for(i=nbyte-1; i>= 0; i--){
    pnt[i] = value & 0xff;
    value = value >> 8;
  }
  return 0;
}

void check_parm_type(int i) {
if(hexdata_ptr == next_parameter)
  i = !i;
if(i)
  usage("@ hexdatafield instead of a simple number (or vice-versa)");  
}

void bitfield(unsigned char * pageaddr, 
	     char * text, int mask, int shift)
{
  if(x_interface && replace) {
    check_parm_type(0);
    *pageaddr = (*pageaddr & ~(mask << shift)) | 
      ((replacement_values[next_parameter++] & mask) << shift);
  }
  else if(x_interface)
    printf("%d ", (*pageaddr >> shift) & mask);
  else
    printf("%-35s%d\n", text, (*pageaddr >> shift) & mask);
}

void notbitfield(unsigned char * pageaddr, 
	     char * text, int mask, int shift)
{
  if(modifiable) return bitfield(pageaddr, text, mask, shift);
  if(x_interface && replace) {
    check_parm_type(0);
    *pageaddr = (*pageaddr & ~(mask << shift)) | 
      (((!replacement_values[next_parameter++]) & mask) << shift);
  }
  else if(x_interface)
    printf("%d ", !((*pageaddr >> shift) & mask));
  else
    printf("%-35s%d\n", text, !((*pageaddr >> shift) & mask));
}

void intfield(char * pageaddr, int nbytes, char * text)
{
  if(x_interface && replace) {
    check_parm_type(0);
    putnbyte(pageaddr, replacement_values[next_parameter++], nbytes);
  }
  else if(x_interface)
    printf("%d ", getnbyte(pageaddr, nbytes));
  else
    printf("%-35s%d\n", text, getnbyte(pageaddr, nbytes));
}

void hexfield(char * pageaddr, int nbytes, char * text)
{
  if(x_interface && replace) {
    check_parm_type(0);
    putnbyte(pageaddr, replacement_values[next_parameter++], nbytes);
  }
  else if(x_interface)
    printf("%d ", getnbyte(pageaddr, nbytes));
  else
    printf("%-35s0x%x\n", text, getnbyte(pageaddr, nbytes));
}

void hexdatafield(unsigned char * pageaddr, int nbytes, char * text)
{
  if(x_interface && replace) {
    unsigned char *ptr;
    unsigned tmp;

    /* Though in main we ensured that a @string has the right format, we have
       to check that we are working on a @ hexdata field */

    check_parm_type(1);

    ptr = (unsigned char *)(unsigned long)(replacement_values[next_parameter++]);
    ptr++; /* Skip @ */

    while(*ptr) {
      if(!nbytes)
	goto illegal;
      tmp = (*ptr >= 'a') ? (*ptr - 'a' + 'A') : *ptr;
      tmp -= (tmp >= 'A') ? 'A' - 10 : '0';
      
      *pageaddr = tmp << 4;
      ptr++;

      tmp = (*ptr >= 'a') ? (*ptr - 'a' + 'A') : *ptr;
      tmp -= (tmp >= 'A') ? 'A' - 10 : '0';

      *pageaddr++ += tmp;
      ptr++;
      nbytes--;
      }

    if(nbytes) {
     illegal:
      fputs("scsiinfo: incorrect number of bytes in @hexdatafield.\n", stderr);
      exit(2);
    }
  }
  else if(x_interface) {
    putchar('@');
    while(nbytes-- > 0)
      printf("%02x", *pageaddr++);
    putchar(' ');
  }
  else {
    printf("%-35s0x", text);
    while(nbytes-- > 0)
      printf("%02x", *pageaddr++);
    putchar('\n');
  }
}

int get_mode_page(int page, int page_code)
{
  int status, quiet;
  unsigned char *cmd;

  memset(buffer, 0, sizeof(buffer));
  
  quiet = page_code & ~3;
  page_code &= 3;

  *( (int *)  buffer )		= 0;	/* length of input data */
  *( ((int *) buffer) + 1 )	= 0xff;	/* length of output buffer */
  
  cmd = (char *) ( ((int *) buffer) + 2 );
  
  cmd[0] = MODE_SENSE;			/* MODE SENSE (6) */
  cmd[1] = 0x00;			/* lun = 0, inhibitting BD makes this fail for me */
  cmd[2] = (page_code << 6) | page;
  cmd[3] = 0x00;			/* (reserved) */
  cmd[4] = 0xff;			/* allocation length */
  cmd[5] = 0x00;			/* control */
  
  status = ioctl( fileno(infile), 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer );
  if(status && (!quiet))
    fprintf(stderr,"Unable to read %s Page %02xh\n", get_page_name(page), page);
  return status;
}

/* Same as above, but this time with MODE_SENSE_10 */
int get_mode_page10(int page, int page_code)
{
  int status, quiet;
  unsigned char *cmd;

  memset(buffer, 0, sizeof(buffer));
  
  quiet = page_code & ~3;
  page_code &= 3;

  *( (int *)  buffer )		= 0;		/* length of input data */
  *( ((int *) buffer) + 1 )	= 0xffff;	/* length of output buffer */
  
  cmd = (char *) ( ((int *) buffer) + 2 );
  
  cmd[0] = MODE_SENSE_10;			/* MODE SENSE (10) */
  cmd[1] = 0x00;			/* lun = 0, inhibitting BD makes this fail for me */
  cmd[2] = (page_code << 6) | page;
  cmd[3] = 0x00;			/* (reserved) */
  cmd[4] = 0x00;			/* (reserved) */
  cmd[5] = 0x00;			/* (reserved) */
  cmd[6] = 0x00;			/* (reserved) */
  cmd[7] = 0xff;			/* allocation length hi */
  cmd[8] = 0xff;			/* allocation length lo */
  cmd[9] = 0x00;			/* control */
  
  status = ioctl( fileno(infile), 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer );
  if(status && (!quiet))
    fprintf(stderr,"Unable to read %s Page %02xh with MODESENSE(10)\n",
			get_page_name(page), page);
  return status;
}

#if 0
int get_log_page(int page, int page_code, char save_values, unsigned short parameter_ptr)
{
  int status, quiet;
  unsigned char *cmd;

  memset(buffer, 0, sizeof(buffer));
  
  quiet = page_code & ~3;
  page_code &= 3;

  *( (int *)  buffer )		= 0;	/* length of input data */
  *( ((int *) buffer) + 1 )	= 0xff; /* length of output buffer */
  
  cmd = (char *) ( ((int *) buffer) + 2 );
  
  cmd[0] = LOG_SENSE;			/* LOG SENSE  */
  cmd[1] = 0x00 | (save_values & 1);	/* lun = 0, Parameter pointer control 0 for full page */
  cmd[2] = (page_code << 6) | page;
  cmd[3] = 0x00;			/* (reserved) */
  cmd[4] = 0x00;			/* (reserved) */
  cmd[5] = parameter_ptr >> 8;		/* hi */
  cmd[6] = parameter_ptr & 0xff;	/* lo */
  cmd[7] = 0;			/* allocation length hi */
  cmd[8] = 0xff;			/* allocation length lo */
  cmd[9] = 0x00;			/* control */
  
  dump(buffer, 200);
  status = ioctl( fileno(infile), 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer );
  if(status && (!quiet))
    fprintf(stderr,"Unable to read %sLog Page %02xh\n", get_log_name(page), page);
  dump(buffer, 400);
  return status;
}
#endif

/* Contents should point to the mode parameter header that we obtained
   in a prior read operation.  This way we do not have to work out the
   format of the beast */

int put_mode_page(int page, char * contents, int page_code)
{
  int status;
  int pagelen, pagelen1;
  unsigned char *cmd;

  memset(buffer1, 0, sizeof(buffer1));
  
  pagelen = contents[3]+4; /* How many actual bytes we are sending in the page */
  pagelen1 = contents[0] + 1;

  *( (int *)  buffer1 )		= pagelen1;	/* length of input data */
  *( ((int *) buffer1) + 1 )	= pagelen1;	/* length of output buffer */
  
  cmd = (char *) ( ((int *) buffer1) + 2 );
  
  cmd[0] = MODE_SELECT;
  cmd[1] = 0x10 | (page_code ? 1 : 0);
  cmd[2] = 0x00;
  cmd[3] = 0x00;			/* (reserved) */
  cmd[4] = pagelen1;			/* (reserved) */
  cmd[5] = 0x00;			/* (reserved) */

  memcpy(cmd+6, contents, pagelen1);

  cmd[6] = 0; /* Mask off the mode parameter list length - resreved field */
  memset(cmd+6+4, 0, 5);  /* Mask off reserved fields in block descriptor */
  cmd[6+pagelen] &= 0x3f;  /* Mask off this reserved field in page code */
  
  /* dump (buffer1, 48); */
  status = ioctl( fileno(infile), 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer1 );
  if(status){
    fprintf(stderr,"Unable to store %s Page %02xh\n",
                        get_page_name(page), page);
    dump (buffer1, 48);
  }

  return status;

}


int read_geometry(int page_code)
{
  int status;
  int bdlen;
  unsigned char * pagestart;

  if(save_mode)
    printf("/usr/bin/scsiinfo -gXR %s ", device_name);

  SETUP_MODE_PAGE(4, 9);

  if(!x_interface && !replace){
    printf("Data from Rigid Disk Drive Geometry Page\n");
    printf("----------------------------------------\n");
  };
  intfield(pagestart+2, 3, "Number of cylinders");
  intfield(pagestart+5, 1, "Number of heads");
  intfield(pagestart+6, 3, "Starting write precomp");
  intfield(pagestart+9, 3, "Starting reduced current");
  intfield(pagestart+12,2, "Drive step rate");
  intfield(pagestart+14,3, "Landing Zone Cylinder");
  bitfield(pagestart+17,   "RPL", 3, 0);
  intfield(pagestart+18,1, "Rotational Offset");
  intfield(pagestart+20,2, "Rotational Rate");
  if(x_interface && replace)
    return put_mode_page(4, buffer+8, 0);
  else
    printf("\n");
  return 0;

}

int read_disconnect_reconnect_data(int page_code)
{
  int status;
  int bdlen;
  unsigned char * pagestart;

  if(save_mode)
    printf("/usr/bin/scsiinfo -DXR %s ", device_name);

  SETUP_MODE_PAGE(2, 7);

  if (!x_interface && !replace) {
    printf("Data from Disconnect-Reconnect Page\n");
    printf("-----------------------------------\n");
  };
  intfield(pagestart+2, 1, "Buffer full ratio");
  intfield(pagestart+3, 1, "Buffer empty ratio");
  intfield(pagestart+4, 2, "Bus Inactivity Limit");
  intfield(pagestart+6, 2, "Disconnect Time Limit");
  intfield(pagestart+8, 2, "Connect Time Limit");
  intfield(pagestart+10, 2, "Maximum Burst Size");
  hexfield(pagestart+12, 1, "DTDC");
  if(x_interface && replace)
    return put_mode_page(2, buffer+8, 0);
  else
    printf("\n");
  return 0;

}

int read_control_page(int page_code)
{
  int status;
  int bdlen;
  unsigned char * pagestart;

  if(save_mode)
    printf("/usr/bin/scsiinfo -CXR %s ", device_name);

  SETUP_MODE_PAGE(10, 9);

  if (!x_interface && !replace) {
    printf("Data from Control Page\n");
    printf("----------------------\n");
  }
  bitfield(pagestart+2, "RLEC", 1, 0);
  bitfield(pagestart+3, "QErr", 1, 1);
  bitfield(pagestart+3, "DQue", 1, 0);
  bitfield(pagestart+4, "EECA", 1, 7);
  bitfield(pagestart+4, "RAENP", 1, 2);
  bitfield(pagestart+4, "UUAENP", 1, 1);
  bitfield(pagestart+4, "EAENP", 1, 0);
  bitfield(pagestart+3, "Queue Algorithm Modifier", 0xf, 4);
  intfield(pagestart+6, 2, "Ready AEN Holdoff Period");
  if(x_interface && replace)
    return put_mode_page(10, buffer+8, 0);
  else
    printf("\n");
  return 0;

}

int error_recovery_page(int page_code)
{
  int status;
  int bdlen;
  unsigned char * pagestart;

  if(save_mode)
    printf("/usr/bin/scsiinfo -eXR %s ", device_name);

  SETUP_MODE_PAGE(1, 14);
  if (!x_interface && !replace) {
    printf("Data from Error Recovery Page\n");
    printf("-----------------------------\n");
  }
  bitfield(pagestart+2, "AWRE", 1, 7);
  bitfield(pagestart+2, "ARRE", 1, 6);
  bitfield(pagestart+2, "TB", 1, 5);
  bitfield(pagestart+2, "RC", 1, 4);
  bitfield(pagestart+2, "EER", 1, 3);
  bitfield(pagestart+2, "PER", 1, 2);
  bitfield(pagestart+2, "DTE", 1, 1);
  bitfield(pagestart+2, "DCR", 1, 0);
  intfield(pagestart+3, 1, "Read Retry Count");
  intfield(pagestart+4, 1, "Correction Span");
  intfield(pagestart+5, 1, "Head Offset Count");
  intfield(pagestart+6, 1, "Data Strobe Offset Count");
  intfield(pagestart+8, 1, "Write Retry Count");
  intfield(pagestart+10, 2, "Recovery Time Limit");
  if(x_interface && replace)
    return put_mode_page(1, buffer+8, 0);
  else
    printf("\n");
  return 0;
}

int notch_parameters_page(int page_code)
{
  int status;
  int bdlen;
  unsigned char * pagestart;

  if(save_mode)
    printf("/usr/bin/scsiinfo -nXR %s ", device_name);

  SETUP_MODE_PAGE(0xc, 7);

  if (!x_interface && !replace) {
    printf("Data from Notch Parameters Page\n");
    printf("-------------------------------\n");
  };
  bitfield(pagestart+2, "Notched Drive", 1, 7);
  bitfield(pagestart+2, "Logical or Physical Notch", 1, 6);
  intfield(pagestart+4, 2, "Max # of notches");
  intfield(pagestart+6, 2, "Active Notch");
  if(pagestart[2] & 0x40){
    intfield(pagestart+8, 4, "Starting Boundary");
    intfield(pagestart+12, 4, "Ending Boundary");
  }
  else{ /*Hex is more meaningful for physical notches*/
    hexfield(pagestart+8, 4, "Starting Boundary");
    hexfield(pagestart+12, 4, "Ending Boundary");
  }

  if(x_interface && !replace) {
    if(modifiable)
      printf("0");
    else 
      printf("0x%8.8x%8.8x", getnbyte(pagestart + 16, 4), getnbyte(pagestart + 20, 4));
  };
  if(!x_interface)
    printf("Pages Notched                      %8.8x %8.8x\n", 
	   getnbyte(pagestart + 16,4), getnbyte(pagestart + 20,4));
  if(x_interface && replace)
    return put_mode_page(0xc, buffer+8, 0);
  else
    printf("\n");
  return 0;
}

int read_defect_list(int page_code)
{
  int status = 0, i;
  int table;
  unsigned char *cmd;
  unsigned char * df;
  int len;

  printf("Data from Defect Lists\n");
  printf("----------------------\n");
  for(table=0; table<2; table++){
    memset(buffer, 0, sizeof(buffer));
    
    *( (int *)  buffer )		= 0;	/* length of input data */
    *( ((int *) buffer) + 1 )	= 8192;	/* length of output buffer */
    
    cmd = (char *) ( ((int *) buffer) + 2 );
    len = 8192;
    
    cmd[0] = 0x37;			/* READ DEFECT DATA */
    cmd[1] = 0x00;			/* lun=0 */
    cmd[2] = 0x04 | (table ? 0x08 : 0x10 );   /*  List, Format */
    cmd[3] = 0x00;			/* (reserved) */
    cmd[4] = 0x00;			/* (reserved) */
    cmd[5] = 0x00;			/* (reserved) */
    cmd[6] = 0x00;			/* (reserved) */
    cmd[7] = (len >> 8);			/* Alloc len */
    cmd[8] = (len & 0xff);			/* Alloc len */
    cmd[9] = 0x00;			/* control */
    
    
    i = ioctl( fileno(infile), 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer );
    if(i){
      fprintf(stderr,"Unable to read %s defect data.\n", (table ? "grown" : "manufacturer"));
    }
    status |= i;
    
    len = (buffer[10] << 8) | buffer[11];
    df = (unsigned char *) (buffer+12);
    printf("%d entries in %s table.\n", len/8, (table ? "grown" : "manufacturer"));
    if(len){
      printf("  Cyl Head Byte_offset\n");
      while(len){
	printf("%5d %3d %6d\n", getnbyte(df, 3),  df[3], getnbyte(df+4, 4));
	len -= 8;
	df += 8;
      };
    };
  };
  printf("\n");
  return status;

}

int read_cache(int page_code)
{
  int status;
  int bdlen;
  unsigned char * pagestart;

  if(save_mode)
    printf("/usr/bin/scsiinfo -cXR %s ", device_name);

  SETUP_MODE_PAGE(8, 9);

  if (!x_interface && !replace) {
    printf("Data from Caching Page\n");
    printf("----------------------\n");
  };
  bitfield(pagestart+2, "Write Cache", 1, 2);
  notbitfield(pagestart+2, "Read Cache", 1, 0);
  bitfield(pagestart+2, "Prefetch units", 1, 1);
  bitfield(pagestart+3, "Demand Read Retention Priority", 0xf, 4);
  bitfield(pagestart+3, "Demand Write Retention Priority", 0xf, 0);
  intfield(pagestart+4, 2, "Disable Pre-fetch Transfer Length");
  intfield(pagestart+6, 2, "Minimum Pre-fetch");
  intfield(pagestart+8, 2, "Maximum Pre-fetch");
  intfield(pagestart+10, 2, "Maximum Pre-fetch Ceiling");
  if(x_interface && replace)
    return put_mode_page(8, buffer+8, 0);
  else
    printf("\n");
  return 0;
}

int read_format_info(int page_code)
{
  int status;
  int bdlen;
  unsigned char * pagestart;

  if(save_mode)
    printf("/usr/bin/scsiinfo -fXR %s ", device_name);

  SETUP_MODE_PAGE(3, 13);

  if (!x_interface && !replace) {
    printf("Data from Format Device Page\n");
    printf("----------------------------\n");
  };
  bitfield(pagestart+20, "Removable Medium", 1, 5);
  bitfield(pagestart+20, "Supports Hard Sectoring", 1, 6);
  bitfield(pagestart+20, "Supports Soft Sectoring", 1, 7);
  bitfield(pagestart+20, "Addresses assigned by surface", 1, 4);
  intfield(pagestart+2 , 2, "Tracks per Zone");
  intfield(pagestart+4 , 2, "Alternate sectors per zone");
  intfield(pagestart+6 , 2, "Alternate tracks per zone");
  intfield(pagestart+8 , 2, "Alternate tracks per lun");
  intfield(pagestart+10, 2, "Sectors per track");
  intfield(pagestart+12, 2, "Bytes per sector");
  intfield(pagestart+14, 2, "Interleave");
  intfield(pagestart+16, 2, "Track skew factor");
  intfield(pagestart+18, 2, "Cylinder skew factor");
  if(x_interface && replace)
    return put_mode_page(3, buffer+8, 0);
  else
    printf("\n");
  return 0;

}

int verify_error_recovery(int page_code)
{
  int status;
  int bdlen;
  unsigned char * pagestart;

  if(save_mode)
    printf("/usr/bin/scsiinfo -VXR %s ", device_name);

  SETUP_MODE_PAGE(7, 7);

  if (!x_interface && !replace) {
    printf("Data from Verify Error Recovery Page\n");
    printf("------------------------------------\n");
  }

  bitfield(pagestart +  2, "EER", 1, 3);
  bitfield(pagestart +  2, "PER", 1, 2);
  bitfield(pagestart +  2, "DTE", 1, 1);
  bitfield(pagestart +  2, "DCR", 1, 0);
  intfield(pagestart +  3, 1, "Verify Retry Count");
  intfield(pagestart +  4, 1, "Verify Correction Span (bits)");
  intfield(pagestart + 10, 2, "Verify Recovery Time Limit (ms)");
    
  if(x_interface && replace)
    return put_mode_page(7, buffer+8, 0);
  else
    printf("\n");
  return 0;
}

int peripheral_device_page(int page_code)
{
  static char *idents[] = {
    "X3.131: Small Computer System Interface",
    "X3.91M-1987: Storage Module Interface",
    "X3.170: Enhanced Small Device Interface",
    "X3.130-1986; X3T9.3/87-002: IPI-2",
    "X3.132-1987; X3.147-1988: IPI-3"
  };
  int status;
  int bdlen;
  unsigned ident;
  unsigned char * pagestart;
  char *name;

  if(save_mode)
    printf("/usr/bin/scsiinfo -pXR %s ", device_name);

  SETUP_MODE_PAGE(9, 2);

  if (!x_interface && !replace) {
    printf("Data from Peripheral Device Page\n");
    printf("--------------------------------\n");
  };

#if 0
  dump( pagestart , 20);
  pagestart[1]+=2; /*TEST*/
  buffer[8]+=2; /*TEST*/
#endif

  ident=getnbyte(pagestart + 2, 2);
  if(ident < (sizeof(idents)/sizeof(char *)))
    name = idents[ident];
  else if(ident < 0x8000)
    name = "Reserved";
  else
    name = "Vendor Specific";

  bdlen = pagestart[1] - 6;
  if(bdlen < 0)
    bdlen = 0;
  hexfield(pagestart + 2, 2, "Interface Identifier");
  if(!x_interface) {
    for(ident = 0; ident < 35; ident++)
      putchar(' ');
    puts(name);
  }
  hexdatafield(pagestart + 8, bdlen, "Vendor Specific Data");

  if(x_interface && replace)
    return put_mode_page(9, buffer+8, 0);
  else
    printf("\n");
  if(x_interface && !save_mode)
    puts(name);
  return 0;
}
/*  end  */

int do_inquiry(int page_code)
{
  int status, i;
  unsigned char *cmd;
  unsigned char * pagestart;
  unsigned char tmp;

  for (i=0; i<10*1024; i++)
  {
    buffer[i] = 0;
  }
  
  *( (int *)  buffer )		= 0;	/* length of input data */
  *( ((int *) buffer) + 1 )	= 1024;	/* length of output buffer */

  cmd = (char *) ( ((int *) buffer) + 2 );
  
  cmd[0] = 0x12;			/* INQUIRY */
  cmd[1] = 0x00;			/* lun=0, evpd=0 */
  cmd[2] = 0x00;			/* page code = 0 */
  cmd[3] = 0x00;			/* (reserved) */
  cmd[4] = 0xff;			/* allocation length */
  cmd[5] = 0x00;			/* control */

  status = ioctl( fileno(infile), 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer );
  if(status) printf( "ioctl(SCSI_IOCTL_SEND_COMMAND) status\t= %d\n", status );

  if(status) return status;

  pagestart = buffer + 8;

  if (!x_interface && !replace) {
    printf("Inquiry command\n");
    printf("---------------\n");
  };
  bitfield(pagestart+7, "Relative Address", 1, 7);
  bitfield(pagestart+7, "Wide bus 32", 1, 6);
  bitfield(pagestart+7, "Wide bus 16", 1, 5);
  bitfield(pagestart+7, "Synchronous neg.", 1, 4);
  bitfield(pagestart+7, "Linked Commands", 1, 3);
  bitfield(pagestart+7, "Command Queueing", 1, 1);
  bitfield(pagestart+7, "SftRe", 1, 0);
  bitfield(pagestart+0, "Device Type", 0x1f, 0);
  bitfield(pagestart+0, "Peripheral Qualifier", 0x7, 5);
  bitfield(pagestart+1, "Removable?", 1, 7);
  bitfield(pagestart+1, "Device Type Modifier", 0x7f, 0);
  bitfield(pagestart+2, "ISO Version", 3, 6);
  bitfield(pagestart+2, "ECMA Version", 7, 3);
  bitfield(pagestart+2, "ANSI Version", 7, 0);
  bitfield(pagestart+3, "AENC", 1, 7);
  bitfield(pagestart+3, "TrmIOP", 1, 6);
  bitfield(pagestart+3, "Response Data Format", 0xf, 0);
  if(x_interface) printf("\n");
  tmp = pagestart[16];
  pagestart[16] = 0;
  printf("%s%s\n", (!x_interface ? "Vendor:                    " : ""),
	 pagestart+8);
  pagestart[16] = tmp;

  tmp = pagestart[32];
  pagestart[32] = 0;
  printf("%s%s\n", (!x_interface ? "Product:                   " : ""),
	 pagestart+16);
  pagestart[32] = tmp;

  printf("%s%s\n", (!x_interface ? "Revision level:            " : ""),
	 pagestart+32);

  printf( "\n" );
  return status;

}

int do_serial_number(int page_code)
{
  int status, i;
  unsigned char *cmd;
  unsigned char * pagestart;

  for (i=0; i<10*1024; i++)
  {
    buffer[i] = 0;
  }
  
  *( (int *)  buffer )		= 0;	/* length of input data */
  *( ((int *) buffer) + 1 )	= 1024;	/* length of output buffer */

  cmd = (char *) ( ((int *) buffer) + 2 );
  
  cmd[0] = 0x12;			/* INQUIRY */
  cmd[1] = 0x01;			/* lun=0, evpd=1 */
  cmd[2] = 0x80;			/* page code = 0x80, serial number */
  cmd[3] = 0x00;			/* (reserved) */
  cmd[4] = 0xff;			/* allocation length */
  cmd[5] = 0x00;			/* control */

  status = ioctl( fileno(infile), 1 /* SCSI_IOCTL_SEND_COMMAND */, buffer );
  if(status) printf( "ioctl(SCSI_IOCTL_SEND_COMMAND) status\t= %d\n", status );

  if(status) return status;

  pagestart = buffer + 8;

  printf("Serial Number '");
  for (i = 0; i < pagestart[3]; i++)
    printf("%c", pagestart[4+i]);
  printf("'\n");

  return status;

}

char * devices[] = {"/dev/sda","/dev/sdb","/dev/sdc","/dev/sdd","/dev/sde","/dev/sdf","/dev/sdg",
		    "/dev/sdh","/dev/scd0","/dev/scd1","/dev/nst0","/dev/nst1"};
/* Print out a list of the known devices on the system */
void show_devices(){
  int i;
  FILE * test;
  for(i=0; i<sizeof(devices)/sizeof(char *); i++){
    test = fopen(devices[i],"r");
    if(!test) continue;
    printf("%s ", devices[i]);
    fclose(test);
  };
  printf("\n");
}

/* select the given notch w/o changing anything else, if save_mode is
   set output code to do the same in a script as well.. */
int select_notch(int notch){
  int status;
  int bdlen;
  int page_code = 0; /* for SETUP_MODE_PAGE */
  unsigned char * pagestart;

  if(save_mode){
    printf("set -- `/usr/bin/scsiinfo -nX %s`\n", device_name);
    printf("/usr/bin/scsiinfo -nXR %s $1 $2 $3 %d $5 $6 $7\n", device_name, notch);
  }

  SETUP_MODE_PAGE(0xc, 0);
  putnbyte(pagestart + 6, notch, 2);
  return put_mode_page(0xc, buffer+8, 0);
}

int show_pages(int page_code){
  int offset;
  int length;
  int i;
  int status = 0;
  unsigned long long pages_sup = 0;
  unsigned long long pages_mask = 0;

  if(!get_mode_page10(0x3f, page_code | 0x10)){
    length = 9 + getnbyte(buffer + 8, 2);
    offset = 16 + getnbyte(buffer + 14, 2);
  }
  else if(!get_mode_page(0x3f, page_code | 0x10)){
    length = 9 + buffer[8];
    offset = 12 + buffer[11];
  }
  else{ /* Assume SCSI-1 and fake settings to report NO pages */
    offset = 10;
    length = 0;
  }

  /* Get mask of pages supported by prog: */
  for(i=0; i < MAX_PAGENO; i++)
    if(page_names[i])
      pages_mask |= (1LL << i);

  /* Get pages listed in mode_pages */
  while(offset < length){
    pages_sup |= (1LL << (buffer[offset] & 0x3f));
    offset += 2 + buffer[offset + 1];
  }

  /* Mask out pages unsupported by this binary */
  pages_sup &= pages_mask;
		
  /* Notch page supported? */
  if(pages_sup & (1LL << 12)){
    if(get_mode_page(12, 0))
      return 2;
    offset = 12 + buffer[11];
  }
  else{ /* Fake empty notch page */
    memset(buffer, 0, sizeof(buffer));
    offset = 0;
  }

  if(replace){
    /* Ok we are creating a safe file.. first of all reset the
       fake replace setting. */
    replace = 0;
    save_mode = 1;
    x_interface = 1;

    if(modifiable)
      usage("do not use -LR with -m");

    /* Just a reminder: */
    puts("#!/bin/sh");

    if(pages_sup & (1 << 12)){
      /* save away the notched pages list.. */
      pages_mask = (unsigned long) getnbyte(buffer + offset + 16, 4),
      pages_mask <<= 32;
      pages_mask = getnbyte(buffer + offset + 20, 4),

      i = getnbyte(buffer + offset + 4, 2);

      /* loop through all notches > 0 */
      while(i > 0){
        status |= select_notch(i);
        if(pages_mask & (1 << 1))
          status |= error_recovery_page(page_code);
        if(pages_mask & (1 << 2))
          status |= read_disconnect_reconnect_data(page_code);
        if(pages_mask & (1 << 3))
          status |= read_format_info(page_code);
        if(pages_mask & (1 << 4))
          status |= read_geometry(page_code);
        if(pages_mask & (1 << 7))
          status |= verify_error_recovery(page_code);
        if(pages_mask & (1 << 8))
          status |= read_cache(page_code);
        if(pages_mask & (1 << 9))
          status |= peripheral_device_page(page_code);
        if(pages_mask & (1 << 10))
          status |= read_control_page(page_code);
        if(pages_mask & (1 << 12))
          status |= notch_parameters_page(page_code);
        i--;
      }

      /* Back to notch 0 and safe the notch 0 page itself */
      status |= select_notch(0);
      status |= notch_parameters_page(page_code);
    }
    if(pages_sup & (1 << 1))
      status |= error_recovery_page(page_code);
    if(pages_sup & (1 << 2))
      status |= read_disconnect_reconnect_data(page_code);
    if(pages_sup & (1 << 3))
      status |= read_format_info(page_code);
    if(pages_sup & (1 << 4))
      status |= read_geometry(page_code);
    if(pages_sup & (1 << 7))
      status |= verify_error_recovery(page_code);
    if(pages_sup & (1 << 8))
      status |= read_cache(page_code);
    if(pages_sup & (1 << 9))
      status |= peripheral_device_page(page_code);
    if(pages_sup & (1 << 10))
      status |= read_control_page(page_code);
    return status;
  }
  

  if(x_interface){
    printf("0x%08lx%08lx 0x%08x%08x %d\n",
	(unsigned long)(pages_sup >> 32),
	(unsigned long)pages_sup,
	getnbyte(buffer + offset + 16, 4),
	getnbyte(buffer + offset + 20, 4),
	getnbyte(buffer + offset + 6, 2));
  }
  else{
    pages_mask = getnbyte(buffer + offset + 16, 4);
    pages_mask <<= 32;
    pages_mask += getnbyte(buffer + offset + 20, 4);

    puts("Mode Pages supported by this binary and target:");
    puts("-----------------------------------------------");
    for(i=0; i < MAX_PAGENO; i++)
      if(pages_sup & (1LL << i))
	printf("%02xh: %s Page%s\n", i, get_page_name(i),
		  (pages_mask & (1LL << i)) ? " (notched)" : "");
    if(pages_sup & (1LL << 12)){
      printf("\nCurrent notch is %d.\n", getnbyte(buffer + offset + 6, 2));
    }
    if(!pages_sup)
      puts("No mode pages supported (SCSI-1?).");
  }
  
  return 0;
}

void usage(char * errtext){
  fprintf(stderr,"Error: scsiinfo - %s\n", errtext);
  fputs("Usage: scsiinfo [-options] [device]\n"
"\tAllowed options are:\n"
"\t-c    Display information from Caching Page.\n"
"\t-C    Display information from Control Mode Page.\n"
"\t-d    Display defect lists.\n"
"\t-e    Display information from Error Recovery page.\n"
"\t-f    Display information from Format Device Page.\n"
"\t-g    Display information from Rigid Disk Drive Geometry Page.\n"
"\t-i    Display all information from Inquiry command.\n"
"\t-s    Display all information from unit serial number page.\n"
"\t-D    Display information from Disconnect-Reconnect Page.\n"
"\t-n    Display information from Notch and Partition Page.\n"
"\t-p    Display information from Peripheral Device Page.\n"
"\t-V    Display information from Verify Error Recovery Page.\n"
"\t-v    Show version number\n"
"\t-a    All of the above.\n\n"
"\t-l    List known scsi devices on the system\n"
"\t-L    List pages supported notched by program and target\n"
"\t        (notched and active notch are also returned)\n\n"
"\tOnly one of the following three options can be specified.\n"
"\tNone of these three implies the current values are returned.\n"
"\t-m    Display modifiable fields instead of current values\n"
"\t-M    Display manufacturer defaults instead of current values\n"
"\t-S    Display saved defaults instead of current values\n\n"
"\t-X    Display output suitable for the X-based interface.\n"
"\t-R    Replace parameters - best used with -X (expert use only)\n\n"
"All options except -l and -v require that exactly one device is given.\n"
"-X and -R can be used only with one of the display page options.\n"
"-m and -M cannot be used with -R.\n"
"You may use -M, -S with -L though it should make no difference\n"
"as a special goodie when using -LXR then a /bin/sh script is written\n"
"to stdout that will restore the current settings of the target when\n"
"executed. You can use one of -M, -S with -LXR to save the corresponding\n"
"values.\n", stderr);
  exit(2);
}




/* This is a bit of a cheat, but we simply request the info from
   the disconnect/reconnect page, and then send it back again, except
   with the save bit set. In theory, sending a single page with the
   Save Pages bit set should be sufficient to save all the pages, but
   in at least one case (Conner CFP4207S) this did not work correctly,
   so we perform a MODE SELECT for all the important pages just to be
   sure. */
int replace_parameters(){
  /* Update Read-Write Error Recovery Page */
  get_mode_page(0x01, 0);
  put_mode_page(0x01, buffer+8, 1);
  /* Update Disconnect-Reconnect Page */
  get_mode_page(0x02, 0);
  put_mode_page(0x02, buffer+8, 1);
  /* Update Format Device Page */
  get_mode_page(0x03, 0);
  put_mode_page(0x03, buffer+8, 1);
  /* Update Caching Page */
  get_mode_page(0x08, 0);
  put_mode_page(0x08, buffer+8, 1);
  /* Update Control Mode Page */
  get_mode_page(0x0A, 0);
  put_mode_page(0x0A, buffer+8, 1);
  return 0;
}

int main( int argc, char *argv[])
{
  char c;
  int page_code;
  int status = 0;
  char all = 0;
  int i;
  long tmp;

  if(argc < 2)
    usage("too few arguments");
  while ((c = getopt(argc, argv, "agdcfisDeCXmMSRvlnLpV")) != EOF)
    {
      switch (c)	
	{
	case 'l':
	  list = 1;
	  break;
	case 'e':
	  error = 1;
	  break;
	case 'd':
	  defect = 1;
	  break;
	case 'n':
	  notch = 1;
	  break;
	case 'i':
	  inquiry = 1;
	  break;
	case 's':
	  serial_number = 1;
	  break;
	case 'D':
	  disconnect = 1;
	  break;
	case 'M':
	  default_param = 1;
	  break;
	case 'm':
	  modifiable = 1;
	  break;
	case 'S':
	  saved = 1;
	  break;
	case 'f':
	  format = 1;
	  break;
	case 'g':
	  geometry = 1;
	  break;
	case 'C':
	  control = 1;
	  break;
	case 'c':
	  cache = 1;
	  break;
	case 'X':
	  x_interface = 1;
	  break;
	case 'R':
	  replace = 1;
	  break;
	case 'L':
	  list_pages = 1;
	  break;
	case 'V':
	  verify = 1;
	  break;
	case 'p':
	  peripheral = 1;
	  break;
	case 'a':
	  all	     = 1;

	  verify     = 1;
	  peripheral = 1;
	  error      = 1;
	  defect     = 1;
	  inquiry    = 1;
	  serial_number = 1;
	  disconnect = 1;
	  format     = 1;
	  geometry   = 1;
	  control    = 1;
	  cache      = 1;
	  notch      = 1;
	  /* fall through */
	case 'v':
	  fprintf(stderr," Scsiinfo version 1.5(eowmob)\n");
	  break;
	default:
	  fprintf(stderr,"Unknown option '-%c' (ascii %02xh)\n", c, c);
	  usage("bad option");
	};
    };

  if(saved + modifiable + default_param > 1) usage("only one of -m, -M, or -S allowed");
  if(x_interface && (inquiry + geometry + cache + format + 
		     error + control + disconnect + defect + list_pages) > 1)
    usage("-X can be used only with exactly one display page option.");
  if(replace && !x_interface) usage("-R requires -X");
  if(replace && (modifiable || default_param) && !list_pages)
    usage("-R not allowed for -m or -M");

  if(replace && !saved){
    for(i=1; i<argc-optind; i++) {
      if(strncmp(argv[optind+i],"0x", 2) == 0) {
	char * pnt = argv[optind+i] + 2;
	replacement_values[i] = 0;
	/* This is a kluge, but we can handle 64 bit quantities this way. */
	while(*pnt) {
	  if(*pnt >= 'a' && *pnt <= 'f') *pnt -= 32;
	  replacement_values[i] = (replacement_values[i] << 4) |
	    (*pnt > '9' ? (*pnt - 'A' + 10) : (*pnt - '0'));
	  pnt++;
	}
	continue;
      }
      if(argv[optind+i][0] == '@') {
	/*Ensure that this string contains an even number of hex-digits*/
	int len = strlen(argv[optind + i] + 1);

	if((len & 1) || (len != strspn(argv[optind + i] + 1, "0123456789ABCDEFabcdef")))
	  usage("Odd number of chars or non-hex digit in @hexdatafield");

	replacement_values[i] = (unsigned long)argv[optind + i];
	hexdata_ptr = i;
	continue;
      }
      /* Using a tmp here is silly but the most clean approach*/
      sscanf(argv[optind+i],"%ld",&tmp);
      replacement_values[i] = tmp;
    };
    n_replacement_values = argc - optind - 1;
  };
  if(list) {
    show_devices();
    exit(0);
  }

  if(optind >= argc)
    usage("no device name given");
  infile = fopen( device_name = argv[optind], "r" );
  if(!infile) {
    perror("scsiinfo(fopen)");
    exit(1);
  }

  /* Save the current parameters in NOVRAM on the device */
  if(saved && replace && !list_pages) {
    replace_parameters();
    fclose(infile);
    exit(0);
  };

  page_code = 0;
  if(modifiable) page_code=1;
  if(default_param) page_code=2;
  if(saved) page_code=3;

  if(!x_interface) printf("\n");

  if(inquiry)
    status |= do_inquiry(page_code);
  if(serial_number)
    status |= do_serial_number(page_code);
  if(geometry)
    status |= read_geometry(page_code);
  if(cache)
    status |= read_cache(page_code);
  if(format)
    status |= read_format_info(page_code);
  if(error)
    status |= error_recovery_page(page_code);
  if(control)
    status |= read_control_page(page_code);
  if(disconnect)
    status |= read_disconnect_reconnect_data(page_code);
  if(defect)
    status |= read_defect_list(page_code);
  if(notch)
    status |= notch_parameters_page(page_code);
  if(verify)
    status |= verify_error_recovery(page_code);
  if(peripheral)
    status |= peripheral_device_page(page_code);

  if(list_pages)
    status |= show_pages(page_code);

  if(all)
	return 0;
  return status?1:0;
}
