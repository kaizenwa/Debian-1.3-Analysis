/*
 * Copyright 1994, 1995 Yggdrasil Computing, Inc.
 *
 * Originally written by Adam J. Richter <adam@yggdrasil.com>.
 * Maintained by H. Peter Anvin <hpa@storm.net>.
 * For other credits, please see the man page.
 * 
 * This file may be copied under the terms and conditions of version 2
 * of the GNU General Public License, as published by the Free
 * Software Foundation (Cambridge, Massachusetts).
 */

/* Change log:
	version 1.1: fixed bug that resulted in stream being written
		to be truncated to a 16kB boundary.
	version 1.2: added -v (verbose) option.
	version 1.3: improved buffering - should avoid device starvation.
	version 1.4: added setpriority() call with large negative nice value.
	version 1.5: added multitrack and audio capabilities; further
	             buffering improvements; SCSI generic handling improved.
        version 2.0: Yamaha and HP support added, command line changes.
*/

const char *version = "2.0";
#ifndef DEFAULT_DEV
#define DEFAULT_DEV "/dev/cdwriter"
#endif
const char *default_dev = DEFAULT_DEV;

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/file.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <limits.h>

#include <linux/version.h>
#ifndef LINUX_VERSION_CODE	/* Very old kernel? */
#define LINUX_VERSION_CODE 0
#endif
#if LINUX_VERSION_CODE >= 0x01031a /* <linux/scsi.h> introduced in 1.3.26 */
#include <linux/scsi.h>
#else
#define __KERNEL__
#include <linux/fs.h>
#undef __KERNEL__
#include "block/blk.h"
#include "scsi/scsi.h"
#endif
#include "scsi/sg.h"

#include <string.h>

#define max(A,B) ((A)<(B)?(B):(A))
#define min(A,B) ((A)<(B)?(A):(B))

#define STDIN  0		/* Standard file descriptors */
#define STDOUT 1
#define STDERR 2

enum writer_t {
  type_unknown,			/* Unknown type */
  type_philips,			/* Philips/IMS/Kodak */
  type_yamaha,			/* Yamaha */
  type_hp			/* Hewlett-Packard */
}
cdwriter_type = type_unknown;

char *type_name[] = {
  "Unknown",
  "Philips/IMS/Kodak",
  "Yamaha",
  "HP"
};

struct track_info_t
{
  int fd;			/* File descriptor of input file */
  int bytes;			/* Bytes to write (or -1 for until EOF) */
  int audio;			/* Audio track? */
  int preemp;			/* Audio track recorded w/preemphasis */
  int pad;			/* Pad data track */
};

int pack_id = 5;
int verbose = 0;

char *program;

#define FUDGE 10		/* Number of bytes to reserve for request
				   structure itself */
#define HDRSPC (sizeof(struct sg_header)+FUDGE)

#define DATA_CD_BLOCK_SIZE	2048
#define DATA_BLOCKS_PER_WRITE	((SG_BIG_BUFF-HDRSPC)/DATA_CD_BLOCK_SIZE)
#define DATA_WRITE_BLOCK_SIZE	(DATA_CD_BLOCK_SIZE * DATA_BLOCKS_PER_WRITE)

#define AUDIO_CD_BLOCK_SIZE     2352
#define AUDIO_BLOCKS_PER_WRITE  ((SG_BIG_BUFF-HDRSPC)/AUDIO_CD_BLOCK_SIZE)
#define AUDIO_WRITE_BLOCK_SIZE  (AUDIO_CD_BLOCK_SIZE * AUDIO_BLOCKS_PER_WRITE)

#define WRITE_BLOCK_SIZE max(AUDIO_WRITE_BLOCK_SIZE,DATA_WRITE_BLOCK_SIZE)

#define MAX_TRACKS 100		/* Red Book track limit */

#define PAD_SIZE 15		/* Number of sectors for the -pad option
				   Must be <= DATA_BLOCKS_PER_WRITE */

struct sg_request {
  struct sg_header header;
  unsigned char bytes[WRITE_BLOCK_SIZE+FUDGE];
} sg_request;

struct sg_reply {
  struct sg_header header;
  unsigned char bytes[100];
};

#define RESERVE_TRACK	0xE4
#define WRITE_TRACK	0xE6
#define LOAD_UNLOAD	0xE7	/* Philips/IMS/Kodak, HP */
#define MEDIUM_UNLOAD	0xE7	/* Yamaha */
#define FIXATION	0xE9	/* Write table of contents */
#define RECOVER		0xEC

/* Forward declarations */

int
quiet_request_sense (int fd, int iteration, char *note,
		     int *reply_len, struct sg_reply *rep);

int die(char *message)
{
  fprintf(stderr, "%s: %s\n", program, message);
  exit(1);
}

void
print_reply (const char *description, int reply_len, struct sg_reply *rep) {
   int i;
   int bytes_to_show;

   if (reply_len > 18)
     bytes_to_show = 18;
   else
     bytes_to_show = reply_len;
#if 0
   printf ("Reply length %d, sizeof (struct sg_header) %d.\n",
	   reply_len, sizeof(struct sg_header));
   printf ("Reply: pack_len %d reply_len %d pack_id %d result %d.\n",
	   rep->header.pack_len,
	   rep->header.reply_len,
	   rep->header.pack_id,
	   rep->header.result);
#endif
   if ( rep->header.result != 0 || (rep->header.sense_buffer[2] & 0xF) != 0) {
      fprintf (stderr, "%s result %d, pack_id %d sense",
	      description, rep->header.result, rep->header.pack_id);
      for ( i = 0; i < 16; i++ ) {
	 fprintf (stderr, " %02X", rep->header.sense_buffer[i]);
      }
      fprintf (stderr, "\n");
   }
   fprintf (stderr, "%d of %d %s reply bytes:", bytes_to_show,
	   reply_len, description);
   for (i = 0; i < bytes_to_show; i++ )
     fprintf (stderr, " %02X", rep->bytes[i]);
   fprintf (stderr, "\n");
}

static int auto_request_sense = 1;

int
send_request(int fd, char *note, int *reply_len,
	     struct sg_reply *rep, int nbytes, ...) {
   va_list args;
   struct sg_request sg_request;
   int i;
   int result;
   int expected_size;

   memset(&sg_request, 0, sizeof(sg_request));
   sg_request.header.pack_len = sizeof(struct sg_header) + 10;
   sg_request.header.reply_len = *reply_len + sizeof(struct sg_header);
   sg_request.header.pack_id = pack_id++;
   sg_request.header.result = 0;

   va_start(args, nbytes);
   for (i = 0; i < nbytes; i++ ) {
      sg_request.bytes[i] = va_arg(args,unsigned int);
   }
   va_end (args);

   expected_size = sizeof(struct sg_header) + nbytes;
   if ((result = write(fd, &sg_request, expected_size)) < 0) {
      perror ("write_track: write");
      return -1;
   }
   else if (result != expected_size) {
      fprintf (stderr, "send_request %s wrote %d bytes, expected to write %d.\n",
	       note ? "<no note>" : note, result, expected_size);
   }
   *reply_len = read (fd, rep, sizeof(struct sg_reply));
   /* if (note) print_reply(note,*reply_len,rep); */
   if (auto_request_sense) {
      auto_request_sense = 0;
      quiet_request_sense(fd, 0, note, reply_len, rep);
      auto_request_sense = 1;
   }

   return 0;
}

int
write_data_track (int fd, int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof(struct sg_reply);
   return send_request (fd, "write_data_track", reply_len, rep, 10,
			WRITE_TRACK,	/* 0 */
			0,		/* 1 */
			0,0,0,		/* 2..4 */
			0,	        /* 5: track number (0=new) */
			1,		/* 6: data track, mode 1 */
			0,0,0		/* 7..9 */);
}

int
write_audio_track (int fd, int *reply_len, struct sg_reply *rep, int preemp) {
   *reply_len = sizeof(struct sg_reply);
   return send_request (fd, "write_audio_track", reply_len, rep, 10,
			WRITE_TRACK,	/* 0 */
			0,		/* 1 */
			0,0,0,		/* 2..4 */
			0,	        /* 5: track number (0=new) */
			preemp ? 5 : 4,	/* 6: audio track */
			0,0,0		/* 7..9 */);
}

int
pipe_to_cd (struct track_info_t *track_info, int out_fd, int *reply_len,
	    struct sg_reply *rep)
{
   unsigned char *start, *end;
   int bytes_written = 0;
   int last_bytes_written = 0;
   int total_bytes_read = 0;
   int this_len = 0;		/* Initialize to keep gcc happy */
   int iteration = 1;
   int expected_len;
   unsigned char *write_ptr;
   int write_block_size;
   int blocks_per_write;
   int block_size;
   int in_fd = track_info->fd;
   int audio = track_info->audio;
   int max_bytes_to_copy = track_info->bytes;
   int bytes_to_read;
   int this_read_len;
   int this_write_len;
   int short_read=0;
   int slop;
   int pad;
   static int track_no = 0;

   track_no++;

   blocks_per_write = audio ? AUDIO_BLOCKS_PER_WRITE : DATA_BLOCKS_PER_WRITE;
   if ( blocks_per_write > 255 ) blocks_per_write = 255;
   block_size = audio ? AUDIO_CD_BLOCK_SIZE : DATA_CD_BLOCK_SIZE;
   write_block_size = blocks_per_write * block_size;

   pad = track_info->pad && !audio; /* Data track padding */

   memset(&sg_request, 0, sizeof(sg_request));
   sg_request.header.pack_len = sizeof(struct sg_header)+6+write_block_size;
   sg_request.header.reply_len = 20;
   sg_request.header.pack_id = pack_id++;
   sg_request.header.result = 0;

   sg_request.bytes[0] = WRITE_6;
   sg_request.bytes[4] = blocks_per_write;

   if (verbose) {
       if ( max_bytes_to_copy != INT_MAX )
         printf("Track %02d:   0 of %d Mb written.\r",
		track_no, max_bytes_to_copy >> 20);
       else
	 printf("Track %02d:   0 Mb written.\r", track_no);
       fflush(stdout);
   }

   /* Read one block. */

   start = &sg_request.bytes[6];
   end = start + write_block_size;
   this_read_len = 0;
   if (max_bytes_to_copy != INT_MAX)
     bytes_to_read = min (write_block_size,
			  max_bytes_to_copy-total_bytes_read);
   else
     bytes_to_read = write_block_size;
   
   while (bytes_to_read)
     {
       this_len = read(in_fd, start, bytes_to_read);
       if (this_len > 0)
	 {
	   start += this_len;
	   total_bytes_read += this_len;
	   this_read_len += this_len;
	   bytes_to_read -= this_len;
	 }
       else if (this_len == 0)
	 break;
       else 
	 {
	   if ( errno != EAGAIN && errno != EINTR )
	     break;
	 }
     }
   
   if ( this_read_len == 0 ) {
     if (this_len < 0) {
       perror ("cdwrite-read");
       return (-1);
     }
     else if (this_len == 0) {
       /* Nothing to do! */
       return (0);
     }
   }

   if (start != end) {
     short_read = 1;
     if ( (slop = this_read_len % block_size) != 0 ) {
       slop = block_size-slop;
       this_read_len += slop;
       memset (start, 0, slop);
     }
   }

   /* write the first block */
   if (short_read) {
     sg_request.header.pack_len = sizeof(struct sg_header)+6+this_read_len;
     sg_request.bytes[4] = this_read_len/block_size;
   }
   expected_len = sizeof(struct sg_header) + 6 + this_read_len;
   write_ptr = (unsigned char*) &sg_request;
   do {
     this_write_len = write( out_fd, write_ptr, expected_len );
     if (this_write_len < 0) {
       perror ("pipe_to_cd: write");
       return -1;
     }
     else if (this_write_len >= 0 && this_write_len != expected_len) {
       fprintf (stderr, "After writing %d bytes, only wrote %d bytes to CD "
		        "(should have written %d).\n",
		bytes_written, this_write_len, expected_len);
     }
     expected_len -= this_write_len;
     write_ptr += this_write_len;
     bytes_written += this_write_len;
   } while (expected_len != 0);

   while (this_read_len > 0 && total_bytes_read < max_bytes_to_copy)
     {
       /* Now we read the next block */
       
       start = &sg_request.bytes[6];
       end = start + write_block_size;
       this_read_len = 0;
       if (max_bytes_to_copy != INT_MAX)
	 bytes_to_read = min (write_block_size,
			      max_bytes_to_copy-total_bytes_read);
       else
	 bytes_to_read = write_block_size;

       while (bytes_to_read)
	 {
	   this_len = read(in_fd, start, bytes_to_read);
	   if (this_len > 0)
	     {
	       start += this_len;
	       total_bytes_read += this_len;
	       this_read_len += this_len;
	       bytes_to_read -= this_len;
	     }
	   else if (this_len == 0)
	     break;
	   else 
	     {
	       if ( errno != EAGAIN && errno != EINTR )
		 break;
	     }
	 }

       if ( this_read_len == 0 )
	 {
	   if (this_len < 0)
	     {
	       perror ("cdwrite-read");
	       break;
	     }
	   else if (this_len == 0)
	     break;		/* We're done */
	 }
       
       if (start != end) {
	 short_read = 1;
	 if ( (slop = this_read_len % block_size) != 0 ) {
	   slop = block_size - slop;
	   this_read_len += slop;
	   memset (start, 0, slop);
	 }
       }
     
       if (verbose && (bytes_written^last_bytes_written) & ~0xfffff) {
	 /* FIXME: bytes_written is not the best for this purpose, since
	    it includes SCSI request headers */
	 printf ("Track %02d: %3d\r", track_no, bytes_written >> 20);
	 last_bytes_written = bytes_written;
	 fflush(stdout);
       }
       
       /* this is a good spot to turn off interrupts
	  if (use_cli) {
	  cli();
	  }
	  */
       
       /* Get the response from the last block written */
       *reply_len = read (out_fd, rep, sizeof(struct sg_reply));
       quiet_request_sense (out_fd, iteration++, "pipe_to_cd", reply_len, rep);
       
       /* write the block */
       if (short_read) {
	 sg_request.header.pack_len = sizeof(struct sg_header)+6+this_read_len;
	 sg_request.bytes[4] = this_read_len/block_size;
       }

       expected_len = sizeof(struct sg_header) + 6 + this_read_len;
       write_ptr = (unsigned char*) &sg_request;
       do {
	 this_write_len = write( out_fd, write_ptr, expected_len );
	 if (this_write_len < 0) {
	   /* we would need to turn on interrupts here. 
	      if (use_cli) {
	      sti();
	      } */
	   perror ("pipe_to_cd: write");
	   return -1;
	 }
	 else if (this_write_len >= 0 && this_write_len != expected_len) {
	   fprintf (stderr, "After writing %d bytes, only wrote %d bytes to CD"
		    " (should have written %d).\n",
		    bytes_written, this_write_len, expected_len);
	 }
	 /* we would also need to turn on interrupts here. 
	    if (use_cli) {
	    sti();
	    } */
	 
	 expected_len -= this_write_len;
	 write_ptr += this_write_len;
	 bytes_written += this_write_len;
       } while (expected_len != 0);
     }
  
   if ( pad )
     {
       /* Note: we only pad data tracks, so we use the defined constants */

       expected_len = sg_request.header.pack_len =
	 sizeof(struct sg_header) + 6 + PAD_SIZE * DATA_CD_BLOCK_SIZE;
       sg_request.bytes[4] = PAD_SIZE;
       memset(&sg_request.bytes[6], 0, PAD_SIZE*DATA_CD_BLOCK_SIZE);
       
       /* Wait for writer to ack the last data */
       *reply_len = read (out_fd, rep, sizeof(struct sg_reply));
       quiet_request_sense (out_fd, iteration++, "pipe_to_cd", reply_len, rep);
       
       write_ptr = (unsigned char*) &sg_request;
       do {
	 this_write_len = write( out_fd, write_ptr, expected_len );
	 if (this_write_len < 0) {
	   perror ("pipe_to_cd: write pad");
	   return -1;
	 }
	 else if (this_write_len >= 0 && this_write_len != expected_len) {
	   fprintf (stderr, "After writing %d bytes, only wrote %d bytes to CD"
		    " (should have written %d).\n",
		    bytes_written, this_write_len, expected_len);
	 }
	 
	 expected_len -= this_write_len;
	 write_ptr += this_write_len;
	 bytes_written += this_write_len;
       } while (expected_len != 0);
     }       
   
   /* wait for the Writer to ack the last block */
   *reply_len = read (out_fd, rep, sizeof(struct sg_reply));
   quiet_request_sense (out_fd, iteration++, "pipe_to_cd", reply_len, rep);

   printf ("Track %02d: Total data bytes written: %d.\n",
	   track_no, total_bytes_read);
   return 0;
}

int
clear_unit_attention (int fd, int *reply_len, struct sg_reply *rep) {
   int saved_auto_request_sense = auto_request_sense, result;
   auto_request_sense = 0;
   *reply_len = sizeof(struct sg_reply);
   result = send_request (fd, "test_unit_ready", reply_len, rep, 6,
		 TEST_UNIT_READY,/* 0 */
		 0,		/* 1 */
		 0,0,0,0	/* 2..5 */);
   auto_request_sense = saved_auto_request_sense;
   return result;
}

int
test_unit_ready (int fd, int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof(struct sg_reply);
   return send_request (fd, "test_unit_ready", reply_len, rep, 6,
			TEST_UNIT_READY,/* 0 */
			0,		/* 1 */
			0,0,0,0		/* 2..5 */);
}

int
rezero_unit (int fd, int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof(struct sg_reply);
   return send_request (fd, "rezero_unit", reply_len, rep, 6,
			REZERO_UNIT, 	/* 0 */
			0,		/* 1 */
			0,0,0,0		/* 2..5 */);
}

int
start_stop (int fd, int start, int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof( struct sg_reply );
   return send_request (fd, "start_stop", reply_len, rep, 6,
			START_STOP,	/* 0 */
			0,		/* 1 */
			0,0,		/* 2..3 */
			start,		/* 4 */
			0		/* 5 */);
}

int
synchronize_cache (int fd, int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof( struct sg_reply );
   return send_request (fd, "synchronize_cache", reply_len, rep, 10,
			SYNCHRONIZE_CACHE,	/* 0 */
			0,0,0,0,0,	/* 1..5 */
			0,		/* 6 */
			0,0,0		/* 7..9 */);
}

int
set_removable (int fd, int removable, int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof( struct sg_reply );
   return send_request (fd, "set_removable", reply_len, rep, 6,
			ALLOW_MEDIUM_REMOVAL,	/* 0 */
			0,0,0,	/* 1..3 */
			!removable, /* 4 */
			0	/* 5 */);
}

int
fixation (int fd, int *reply_len, struct sg_reply *rep, int cdrom) {
   *reply_len = sizeof( struct sg_reply );
   return send_request (fd, "fixation", reply_len, rep, 10,
			FIXATION,       /* 0 */
			0,0,0,0,0,0,0,  /* 1..7 */
			cdrom ? 1 : 0,	/* 8: TOC type */
			0		/* 9 */);
}

int
recover (int fd, int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof( struct sg_reply );
   return send_request (fd, "recover", reply_len, rep, 10,
			RECOVER, /* 0 */
			0,0,0,0,0,0,0, /* 1..7 */
			0,	/* 8 */
			0	/* 9 */);
}

int
medium_load_unload (int fd, int load, int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof( struct sg_reply );

   /** ALPHA NOTES: Turned off IMMEDIATE bit; do this only once **/

   switch ( cdwriter_type )
     {
#if 0
     case type_philips:
       /* Philips drivers seem to have this told twice */
       if ( send_request (fd, "load_unload", reply_len, rep, 10,
			  LOAD_UNLOAD, /* 0 */
			  1,0,0,0,0,0,0, /* 1..7 */
			  !load,	/* 8 */
			  0	/* 9 */) )
	 return 1;
       
       /* Fall though */

     case type_hp:

       return send_request (fd, "load_unload", reply_len, rep, 10,
			    LOAD_UNLOAD, /* 0 */
			    1,0,0,0,0,0,0, /* 1..7 */
			    !load,	/* 8 */
			    0	/* 9 */);
#endif
     case type_philips:
     case type_hp:
       return send_request (fd, "load_unload", reply_len, rep, 10,
			    LOAD_UNLOAD, /* 0 */
			    0,0,0,0,0,0,0, /* 1..7 */
			    !load,	/* 8 */
			    0	/* 9 */);

     case type_yamaha:
       if ( load )
	 return 1;		/* There is no LOAD command for Yamaha */

       return send_request (fd, "medium_unload", reply_len, rep, 10,
			    MEDIUM_UNLOAD, /* 0 */
			    0,0,0,0,0,0,0, /* 1..7 */
			    0,	/* 8 */
			    0	/* 9 */);

     default:
       die("default in medium_load_unload");
     }

   return -1;			/* For GCC */
}

int
request_sense (int fd, int *reply_len, struct sg_reply *rep) {
   *reply_len = 18;
   return send_request (fd, "request_sense", reply_len, rep, 6,
			REQUEST_SENSE, /* 0 */
			0,0,0,18,0 /* 1..5 */);
}

int
quiet_active_request_sense (int fd, int iteration, char *note,
		     int *reply_len, struct sg_reply *rep) {
   *reply_len = 18;

   if (!auto_request_sense) return 0;
   auto_request_sense = 0;
   return send_request (fd, NULL, reply_len, rep, 6,
			REQUEST_SENSE, /* 0 */
			1,0,0,18,0 /* 1..5 */);
}

int
quiet_request_sense (int fd, int iteration, char *note,
		     int *reply_len, struct sg_reply *rep) {
   *reply_len = 18;
   if (rep->header.sense_buffer[2] & 0xF || rep->header.result != 0) {
      if (iteration != 0) {
	 fprintf (stderr, "SENSE_ERROR iter %d: ", iteration);
      }
      print_reply( note, *reply_len, rep);
#if 0
      quiet_active_request_sense (fd, iteration, "request_sense", reply_len, rep);
#endif
   }
   return 0;
}

int
read_capacity (int fd, int *reply_len, struct sg_reply *rep) {
   int result;
   *reply_len = sizeof( struct sg_reply );
   
   result= send_request (fd, "read_capacity", reply_len, rep, 10,
			READ_CAPACITY, /* 0 */
			0,0,0,0,0,0,0,0,0 /* 1..9 */);
   printf ("Capacity = %d blocks, block length = %d (0x%x).\n",
	   (rep->bytes[0] << 24) + (rep->bytes[1] << 16) +
	   (rep->bytes[2] << 8) + rep->bytes[3],
	   (rep->bytes[4] << 24) + (rep->bytes[5] << 16) +
	   (rep->bytes[6] << 8) + rep->bytes[7],
	   (rep->bytes[4] << 24) + (rep->bytes[5] << 16) +
	   (rep->bytes[6] << 8) + rep->bytes[7]);

   return result;
}

int
reserve_track (int fd, unsigned long len,
	       int *reply_len, struct sg_reply *rep) {
   *reply_len = sizeof( struct sg_reply );
   return send_request (fd, "reserve_track", reply_len, rep, 10,
			RESERVE_TRACK, /* 0 */
			0,0,0,0, /* 1..4 */
			(len >> 24) & 0xFF, /* 5 */
			(len >> 16) & 0xFF, /* 6 */
			(len >> 8) & 0xFF, /* 7 */
			len & 0xFF, /* 8 */
			0	/* 9 */);
}

int
inquiry (int fd, int *reply_len, struct sg_reply *rep, char *manufacturer,
	 char *model, char *revision) {
  int result;
  int i;
  char *reply;
  *reply_len = sizeof( struct sg_reply );
  result = send_request (fd, "inquiry", reply_len, rep, 6,
			INQUIRY, /* 0 */
			0,0,0,255,0);
  for(i=15; i>8; i--)
    if(rep->bytes[i] != ' ') break;
  reply = (char *) &rep->bytes[8];
  while(i-->=8) *manufacturer++ = *reply++;
  *manufacturer = '\0';

  for(i=31; i>16; i--)
    if(rep->bytes[i] != ' ') break;
  reply = (char *) &rep->bytes[16];
  while(i-->=16) *model++ = *reply++;
  *model = '\0';

  for(i=35; i>32; i--)
    if(rep->bytes[i] != ' ') break;
  reply = (char *) &rep->bytes[32];
  while(i-->=32) *revision++ = *reply++;
  *revision = '\0';

  return result;
}

int
mode_select1(int fd, int *reply_len, int audio, int preemp,
	     struct sg_reply *rep) {
  /* Per-track mode select */

  *reply_len = sizeof( struct sg_reply );
  switch (cdwriter_type)
    {
    case type_yamaha:
      return send_request (fd, "mode_select6#1", reply_len, rep, 18,
			   MODE_SELECT, /* 0 */
			   0x00, /* 1 : Page Format Bit */
			   0, 0, /* 2..3 */
			   12,	 /* 4 : Parameter List Length */
			   0,	 /* 5 */

			   /* Mode Select Header */
			   0,	 /* 6+0 reserved */
			   0,	 /* 6+1 Medium Type */
			   0,	 /* 6+2 res<<7 + host_application_code */
			   0x08, /* 6+3 Block Descriptor Length */

			   /* Block Descriptor */
			   0,	 /* 6+4+0 Density Code */
			   0,0,0, /* 6+4+1..3 No of Blocks 0,0,0 =
				     All Blocks on Disk */
			   0,	 /* 6+4+4 reserved */
			   0,	 /* 6+4+5 MSB Block Size */
			   (audio ? 0x09 : 0x08), /* 6+4+6 Block Size */
			   (audio ? 0x30 : 0x00)  /* 6+4+7 LSB Block Size */
			   );
    case type_philips:
    case type_hp:
      return send_request (fd, "mode_select6#1", reply_len, rep, 26,
			   MODE_SELECT, /* 0 */
			   0x10,	/* 1 : page format bit */
			   0, 0,	/* 2..3 */
			   20,	/* 4 : parameter list length? */
			   0,	/* 5 */

			   /* Mode select header: */
			   0,	/* 6+0 reserved */
			   0,	/* 6+1 medium type */
			   0,	/* 6+2 res<<7 + host_application_code */
			   0,	/* 6+3 block descriptor length */

			   /* Mode Page 0x21 */
			   0x21,	/* 6+4+0 page code */
			   14,	/* 6+4+1 paramater list length */
			   0,	/* 6+4+2 reserved */
				    /* 6+4+3 type of sector */
			   audio ? (preemp ? 5 : 4) : 1,
			   0,	/* 6+4+4 create new track ??? */
			   0,0,0,0,0, /* reserved... */
			   0,0,0,0,0, /* reserved */
			   0	/* 6+4+15, total byte count = 26 */
			   );
    default:
      die("default in mode_select1");
    }
  return -1;
}

int
mode_select2(int fd, int *reply_len, int dummy, int speed,
	     struct sg_reply *rep) {
  /* Per-disc mode select */

   *reply_len = sizeof( struct sg_reply );
  switch (cdwriter_type)
    {
    case type_yamaha:
      return send_request (fd, "mode_select6#2", reply_len, rep, 14,
			   MODE_SELECT, /* 0 */
			   0x10, /* 1 : Page Format Bit */
			   0, 0, /* 2..3 */
			   8,	 /* 4 : Parameter List Length */
			   0,	 /* 5 */

			   /* Mode Select Header */
			   0,	 /* 6+0 reserved */
			   0,	 /* 6+1 Medium Type */
			   0,	 /* 6+2 res<<7 + host_application_code */
			   0,	 /* 6+3 Block Descriptor Length */

			   /* Mode Page 0x31 (Drive Configuration) */
			   0x31, /* 6+4+0 Page Code */
			   0x02, /* 6+4+1 Parameter Length */
			   0,	 /* 6+4+2 reserved */
			   /* 6+4+3 Speed and Write Mode */
			   ((speed == 4 ? 0x20 : speed == 1 ? 0x00 : 0x10) |
			    (dummy ? 0x01 : 0x00))
			   );
    case type_philips:
    case type_hp:
      return send_request (fd, "mode_select6#2", reply_len, rep, 18,
			   MODE_SELECT, /* 0 */
			   0x10,	/* 1 : page format bit */
			   0, 0,	/* 2..3 */
			   12,	/* 4 : parameter list length? */
			   0,	/* 5 */

			   /* Mode select header: */
			   0,	/* 6+0 reserved */
			   0,	/* 6+1 medium type */
			   0,	/* 6+2 res<<7 + host_application_code */
			   0,	/* 6+3 block descriptor length */

			   /* Mode Page 0x23 */
			   0x23,/* 6+4+0 page code */
			   6,	/* 6+4+1 paramter length? */
			   speed, /* 6+4+2 speed, 1 = audio, 2 = double speed */
			   dummy, /* 6+4+3 1 = dummy write, 0 = real */
			   0,0,0,0 /* ...6+4+7 reserved, total byte count = 18 */
			   );

    default:
      die("default in mode_select2");
    }
   return -1;
}

void usage(int exit_code)
{
  fprintf(stderr, "cdwrite %s\n"
                  "Usage:\n"
	  "\t%s [general_options] [track_options] track...\n"
	  "General options:\n"
	  "\t-v, --verbose     Verbose display\n"
	  "\t-s, --speed NN    Writing speed\n"
          "\t-y, --dummy       Simulation mode (laser off)\n"
	  "\t-e, --eject       Eject tray on completion\n"
	  "\t-D, --device dev  CD-writer device (default: %s)\n"
	  "\t    --philips     CD-writer is a Philips/IMS/Kodak model\n"
	  "\t    --yamaha      CD-writer is a Yamaha model\n"
	  "\t    --hp          CD-writer is an HP model\n"
	  "Track options:\n"
	  "\t-b, --bytes NN    Length of track data in bytes\n"
          "\t-a, --audio       Subsequent tracks are Redbook audio (CDDA)\n"
          "\t-d, --data        Subsequent tracks are CD-ROM data (default)\n"
	  "\t-p, --preemp      Audio tracks are mastered with preemphasis\n"
	  "\t-n, --nopreemp    No preemphasis (default)\n"
	  "\t-P, --pad         Pad data tracks with zeroes (see man page)\n"
	  "\t-N, --nopad       Do not pad data tracks (default)\n"
	  , version, program, default_dev);
  exit(exit_code);
}

struct long_opt_t
{
  char *optname;
  int optchar;
};

struct long_opt_t longopt[] =
{
  {"--verbose", 'v'},
  {"--version", 'V'},
  {"--speed", 's'},
  {"--dummy", 'y'},
  {"--eject", 'e'},
  {"--device", 'D'},
  {"--philips", 1001},
  {"--ims", 1001},
  {"--kodak", 1001},
  {"--yamaha", 1002},
  {"--hp", 1003},
  {"--bytes", 'b'},
  {"--audio", 'a'},
  {"--data", 'd'},
  {"--preemp", 'p'},
  {"--nopreemp", 'n'},
  {"--pad", 'P'},
  {"--nopad", 'N'},
  {"--help", 'h'},
  {NULL, 0}
};


int main( int argc, char **argv ) {
   int fd;
   struct sg_reply reply;
   int reply_len;
   int speed_factor = 0;
   int dummy_write = 0;
   unsigned long timeout = 10 * 60 * 100;
				/* default timeout = 1 minute, but it */
				/* takes slightly longer than that to */
				/* write the leadin and leadout. (The */
				/* fixation command.)  So, we set it */
				/* to 10 minutes, just to be safe. */
   int tracks = 0;
   int do_eject = 0;
   int cdrom = 0;
   struct track_info_t track_info[MAX_TRACKS];
   struct track_info_t trackopt;
   const char *cd_writer = default_dev;
   char Manufacturer[8+1];
   char Model[16+1];
   char Revision[4+1];
   struct stat st;
   struct rlimit rlim;
   int optchar;
   char *optarg;
   int i, j;

   program = argv[0];

   /* Set track defaults */

   trackopt.audio = 0;		/* data */
   trackopt.preemp = 0;		/* no preemphasis */
   trackopt.pad = 0;		/* no padding */
   trackopt.bytes = INT_MAX;	/* no -bytes option given */

   /* Set max # of file descriptors to no less than MAX_TRACKS + 16 */

   getrlimit(RLIMIT_NOFILE, &rlim);
   rlim.rlim_cur = MAX_TRACKS + 16;
   if ( rlim.rlim_cur > rlim.rlim_max )
     {
       fprintf(stderr, "%s: warning: low file descriptor limit (%ld)\n",
	       program, rlim.rlim_max);
     }
   setrlimit(RLIMIT_NOFILE, &rlim);
   
   /* Parse command line */

   for ( i = 1 ; i < argc ; i++ )
     {
       optarg = argv[i];	/* i can be incremented; keep a pointer */

       if ( optarg[0] == '-' && optarg[1] )
	 {
	   optchar = 0;
	   j = 1;

	   if ( optarg[1] == '-' )
	     {
	       /* Long options */

	       for ( j = 0 ; longopt[j].optchar ; j++ )
		 {
		   if ( strcmp(optarg,longopt[j].optname) == 0 )
		     break;
		 }
	       optchar = longopt[j].optchar;

	       if ( !optchar )
		 {
		   fprintf(stderr,"%s: Unknown option: %s\n",program,argv[i]);
		   usage(1);
		 }

	       j = 0;		/* Indicates long option */
	     }
	   
	   do
	     {
	       if ( j )
		 optchar = optarg[j];
	   
	       switch(optchar)
		 {
		 case 'b':	/* -b, --bytes */
		   trackopt.bytes = atoi(argv[++i]);
		   break;

		 case 's':	/* -s, --speed */
		   speed_factor = atoi(argv[++i]);
		   if ( speed_factor < 0 || speed_factor > 32 )
		     /* Sanity check */
		     die("Bad --speed option");
		   break;
		   
		 case 'y':	/* -y, --dummy */
		   dummy_write = 1;
		   break;

		 case 'e':	/* -e, --eject */
		   do_eject = 1;
		   break;

		 case 'v':	/* -v, --verbose */
		   verbose++;
		   break;

		 case 'V':	/* -V, --version */
		   printf("cdwrite %s\n", version);
		   exit(0);
		   break;

		 case 'a':	/* -a, --audio */
		   trackopt.audio = 1;
		   break;

		 case 'd':	/* -d, --data */
		   trackopt.audio = 0;
		   break;
		   
		 case 'p':	/* -p, --preemp */
		   trackopt.preemp = 1;
		   break;

		 case 'n':	/* -n, --nopreemp */
		   trackopt.preemp = 0;
		   break;

		 case 'P':	/* -P, --pad */
		   trackopt.pad = 1;
		   break;

		 case 'N':	/* -N, --nopad */
		   trackopt.pad = 0;
		   break;

		 case 'D':	/* -D, --device */
		   cd_writer = argv[++i];
		   break;

		 case 'h':	/* -h, --help */
		   usage(0);

		 case 1001:	/* --philips, --kodak, --ims */
		   cdwriter_type = type_philips;
		   break;
		   
		 case 1002:	/* --yamaha */
		   cdwriter_type = type_yamaha;
		   break;

		 case 1003:	/* --hp */
		   cdwriter_type = type_hp;
		   break;

		 case 0:	/* End of option string */
		   break;

		 default:	/* Unknown option */
		   fprintf(stderr, "%s: Unknown option: -%c\n", program,
			   optarg[j]);
		   usage(1);
		 }
	     }
	   while ( optchar && j++ );
	 }
       else
	 {
	   if ( tracks >= MAX_TRACKS )
	     die("Track limit exceeded");

	   track_info[tracks] = trackopt; /* Set track options */

	   if ( strcmp(argv[i], "-") == 0 )
	     track_info[tracks].fd = STDIN;
	   else
	     if ( (track_info[tracks].fd = open(argv[i], O_RDONLY)) < 0 )
	       {
		 perror(argv[i]);
		 exit(1);
	       }

	   tracks++;
	   trackopt.bytes = INT_MAX; /* Clear --bytes option for next track */
	 }
     }

   if ( verbose )
     printf("cdwrite %s\n", version);

   if ( !tracks && !do_eject )
     {
       /* If no tracks specified, only auxilliary actions */

       fprintf(stderr, "%s: Nothing to do!\n", program);
       exit(7);
     }

   /* I *think* we're supposed to write a CD-ROM TOC if we have any data
      tracks present, but it *could* be that it is only the first track
      that matters.  There is also a distinct possibility it makes no
      difference whatsoever, as audio CD's and CD-ROMs use the same
      TOC code.

      Also, if any of the inputs are regular files and -bytes has not
      been specified, use fstat() to figure out the actual length of the
      file. */

   for ( i = 0 ; i < tracks ; i++ )
     {
       if ( !track_info[i].audio )
	 cdrom = 1;

       if ( track_info[i].bytes == INT_MAX )
	 {
	   if ( !fstat(track_info[i].fd, &st) && S_ISREG(st.st_mode) )
	     track_info[i].bytes = st.st_size;
	 }

       if ( verbose )
	 {
	   if ( track_info[i].audio ) {
	     if ( track_info[i].bytes != INT_MAX )
	       printf("Track %02d: audio %3d Mb (%2d:%02d.%02d) %spreemp\n",
		      i+1, track_info[i].bytes >> 20,
		      track_info[i].bytes / (44100*4*60),
		      (track_info[i].bytes / (44100*4)) % 60,
		      (track_info[i].bytes / (441*4)) % 100,
		      track_info[i].preemp ? "" : "no ");
	     else
	       printf("Track %02d: audio unknown length         %spreemp\n",
		      i+1, track_info[i].preemp ? "" : "no ");
	   } else {
	     if ( track_info[i].bytes != INT_MAX )
	       printf("Track %02d: data  %3d Mb         %s\n",
		      i+1, track_info[i].bytes >> 20,
		      track_info[i].pad ? "pad" : "");
	     else
	       printf("Track %02d: data  unknown length %s\n",
		      i+1, track_info[i].pad ? "pad" : "");
	   }
	 }
     }

   /* Note: getting distracted while writing a CD costs real money, so at
      least attempt to give ourselves preferential treatment (this will work
      for root, which is usually the user running cdwrite */

   setpriority(PRIO_PROCESS, 0, -20);

   /* Open SCSI device */
 
   if ( (fd = open (cd_writer, O_RDWR)) < 0 ) {
      perror ("opening scsi device");
      exit (4);
   }

   /* Eat any unwanted garbage from prior use of this device */

   i = fcntl(fd, F_GETFL);	/* Be very proper about this */
   fcntl(fd, F_SETFL, i|O_NONBLOCK);
   while ( read(fd, &reply, sizeof(struct sg_reply)) != -1 ||
	   errno != EAGAIN );	/* Empty buffer */
   fcntl(fd, F_SETFL, i&~O_NONBLOCK);

   memset(&reply, 0, sizeof(struct sg_reply));

   /* Set SCSI timeout */

   if (ioctl(fd, SG_SET_TIMEOUT, &timeout) < 0) {
     perror ("ioctl SG_SET_TIMEOUT");
     exit(5);
   }

   inquiry (fd, &reply_len, &reply, Manufacturer, Model, Revision);

   if ( cdwriter_type == type_unknown )
     {
       if (strcmp(Manufacturer, "PHILIPS") == 0 ||
	   strcmp(Manufacturer, "IMS") == 0 ||
	   strcmp(Manufacturer, "KODAK") == 0)
	 cdwriter_type = type_philips;
       else if (strcmp(Manufacturer, "YAMAHA") == 0)
	 cdwriter_type = type_yamaha;
       else if (strcmp(Manufacturer, "HP") == 0)
	 cdwriter_type = type_hp;
       else
	 {
	   fprintf(stderr,
		   "Unknown CD-Writer; if this model is compatible with any\n"
		   "supported type, please use the appropriate command line\n"
		   "flag.\n"
		   "\n"
		   "Manufacturer:  %s\n"
		   "Model:         %s\n"
		   "Revision:      %s\n", Manufacturer, Model, Revision);
	   exit(6);
	 }
     }

   if ( verbose )
     {
       printf("Manufacturer:  %s\n"
	      "Model:         %s\n"
	      "Revision:      %s\n"
	      "Using mode:    %s\n",
	      Manufacturer, Model, Revision, type_name[cdwriter_type]);
     }
   
   /* Do mode-dependent adjustments here */

   if ( speed_factor == 0 && cdwriter_type != type_hp )
     {
       /* On HP, speed_factor == 0 can be passed to the drive. */
       speed_factor = 2;
     }

   if ( verbose )
     {
       if ( speed_factor )
	 printf("Using speed:   %d\n", speed_factor);
       else
	 printf("Using speed:   maximum\n");
     }

   /* Do it! */

   clear_unit_attention (fd, &reply_len, &reply);

   if ( tracks )
     {
       medium_load_unload (fd, 1, &reply_len, &reply);
       set_removable (fd, 0, &reply_len, &reply);
       start_stop (fd, 1, &reply_len, &reply);
       rezero_unit (fd, &reply_len, &reply);
       test_unit_ready (fd, &reply_len, &reply);
       start_stop (fd, 1, &reply_len, &reply);
       mode_select2 (fd, &reply_len, dummy_write, speed_factor, &reply);
       
       for ( i = 0 ; i < tracks ; i++ )
	 {
	   mode_select1 (fd, &reply_len, track_info[i].audio,
			 track_info[i].preemp, &reply);
	   if ( track_info[i].audio )
	     write_audio_track (fd, &reply_len, &reply, track_info[i].preemp);
	   else
	     write_data_track (fd, &reply_len, &reply);
	   
	   pipe_to_cd (&track_info[i], fd, &reply_len, &reply);
	   synchronize_cache (fd, &reply_len, &reply);
	 }

       if ( verbose )
	 printf("Fixating...\n");
       fixation(fd, &reply_len, &reply, cdrom);
       
       if (cdwriter_type != type_yamaha)
	 {
	   /* Possibly unneeded, but doesn't hurt */
	   synchronize_cache (fd, &reply_len, &reply);
	 }
     }
   
   start_stop (fd, 0, &reply_len, &reply);
   set_removable (fd, 1, &reply_len, &reply);

   if ( do_eject )
     medium_load_unload (fd, 0, &reply_len, &reply);
   
   close(fd);
   return 0;
}
