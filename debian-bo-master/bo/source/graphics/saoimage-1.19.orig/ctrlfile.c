#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	ctrlfile.c (Control File)
 * Purpose:	Prompt for file name and open file for specified purpose
 * Subroutine:	open_output_file()
 * Subroutine:	open_input_file()
 * Subroutine:	swap_bytes()
 * UNIX calls:	fopen(), lstat(), unlink(), extern int errno
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  9 July 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* FILE, stderr, NULL, etc. */
#include <ctype.h>		/* macros for isupper, tolower, etc */
#include <sys/types.h>		/* needed for lstat */
#include <sys/stat.h>		/* needed for lstat, struct stat */
#include <errno.h>		/* ENOENT */
#include <X11/Xlib.h>		/* X window stuff */
#include "hfiles/edit.h"	/* EditStruct */

#ifdef VMS
#define lstat stat
#define unlink delete
#endif

/* May not get defined on some systems, redeclaration doesn't seem to hurt */
extern int errno;

/*
 * Subroutine:	open_output_file
 * Purpose:	Open a file for writing, as specified by user
 * Post state:	Sets pointer to file, open and ready for writing
 * Returns:	2 = append, 1 = new file, 0 = user decided not to, -1 = error
 */
int open_output_file ( fd, edit, one_popup_row, prompt )
     FILE **fd;
     EditStruct *edit;
     int one_popup_row;		/* i: put-promt-before-edit-line, else above */
     char *prompt;
{
  int exist;
  static EditStruct *instruct = NULL;
  static char *response;
  int reply;
  char filename[128];
  char open_type[4];
  int done;				/* l: return status */
  int get_edit_input();
  EditStruct *init_edit_popup();
  void clear_edit_buf(), unmap_popwin();
  static int file_exists();

  open_type[0] = 0;
  if( (get_edit_input(edit, one_popup_row, 1, 0, prompt) <= 0) ||
      (edit->char_cnt == 0) ) {
    unmap_popwin();
    return( 0 );
  }
  strncpy(filename, edit->string, edit->char_cnt);
  filename[edit->char_cnt] = '\0';
  /* check if file already exists */
  exist = file_exists(filename);
  done = -3;
  if( exist >0 ) {
    /* if it exists, get instructions on how to respond */
    if( instruct == NULL ) {
      instruct = init_edit_popup(NULL, 8);
      response = instruct->string;
    }
    do {
      clear_edit_buf (instruct);
      reply =
	get_edit_input(instruct, 1, 0, 0,
		       "Enter 'o' (overwrite), 'a' (append), 'q' (quit): ");
      if( reply <= 0 ) {
        done = 0;
      } else if( instruct->char_cnt > 0 ) {
	if( isupper(response[0]) )
	  response[0] = tolower(response[0]);
	/* quit, do nothing */
	if( response[0] == 'q') {
	  /* quit */
	  done = 0;
	} else if( response[0] == 'o') {
	  /* overwrite existing file */
	  strcpy(open_type, "w");
	  if( unlink(filename) <0 ) {
	    (void)fprintf(stderr, "Error: can't delete file\n");
	    perror (filename);
	    done = -1;
	  } else
	    done = 1;
	} else if( response[0] == 'a') {
	  /* append to end of existing file */
	  strcpy(open_type, "a");
	  done = 2;
	} else
	  /* else unknown response */
	  XBell(edit->display, 20);
      }
    } while( done == -3 );
  } else if( exist ==0 ) {
    /* file does not yet exist */
    strcpy(open_type, "a");
    done = 1;
  } else {
    /* problem accessing file */
    (void)fprintf(stderr, "Error attempting to access file\n");
    perror(filename);
    done = -1;
  }
  unmap_popwin();
  if( done > 0 ) {
    /* open the file appropriately */
    if( (*fd=fopen(filename, open_type)) == NULL ) {
      (void)fprintf(stderr, "Error attempting to open file\n");
      perror(filename);
      done = -1;
    }
  }
  if( done < 0 )
    XBell(edit->display, 20);
  return( done );
}

/*
 * Subroutine:	open_input_file
 * Purpose:	Open a file for reading, as specified by user
 * Note:	Sets pointer of file open and ready for reading
 * Returns:	1 on success, 0 if user decided not to, -1 on error
 */
int open_input_file ( fd, edit, one_popup_row, prompt )
     FILE **fd;
     EditStruct *edit;
     int one_popup_row;		/* i: put-promt-before-edit-line, else above */
     char *prompt;
{
  char filename[132];
  int exist;
  int get_edit_input();
  static int file_exists();

  if( get_edit_input(edit, one_popup_row, 1, 1, prompt) <= 0 )
    return( 0 );
  strncpy(filename, edit->string, edit->char_cnt);
  filename[edit->char_cnt] = '\0';
  /* see if it exists */
  exist = file_exists(filename);
  if( exist ==0 ) {
    (void)fprintf(stderr, "Error: file %s does not exist\n", filename);
    return( -1 );
  } else if( exist <0 ) {
    (void)fprintf(stderr, "Error attempting to access file\n");
    perror(filename);
    return( -1 );
  }
  /* open the cursor file */
  if( (*fd = fopen(filename, "r")) == NULL ) {
    (void)fprintf(stderr, "Error attemping to open file\n");
    perror(filename);
    return( -1 );
  }
  return( 1 );
}


/*
 * Subroutine:	file_exists
 * Purpose:	does a file exist for opening (1=yes, 0=no, -1=problem)
 */
static int file_exists ( name )
     char *name;
{
  int i;
  struct stat buf;

  i = lstat(name, &buf);
  if( i ==0 )
    return(1);            /* found the file */
  else if( errno == ENOENT )
    return(0);            /* didn't find the file */
  else
#ifndef VMS
    return(-1);           /* a real error of some sort */
#else
    return(0);		  /* VMS doesn't set errno properly in this case */
#endif
}


/*
 * Subroutine:	swap_bytes
 * Purpose:	Swap bytes in place, (swab is not guaranteed to work in place)
 */
void swap_bytes ( array, nbytes )
     char *array;
     int nbytes;
{
  register char *low_byte, *high_byte, *last_byte;
  register unsigned temp;
  register int next=2;

  /* swap successive pairs of bytes */
  low_byte = array;
  high_byte = array+1;
  last_byte = array + nbytes;
  while( high_byte < last_byte ) {
    temp = *low_byte;
    *low_byte = *high_byte;
    *high_byte = temp;
    /* there is no hardware incr on the 68000, so why bother with ++? */
    low_byte += next;
    high_byte += next;
  }
}
                         
                                                               
                                                               
                                                               
                                                               
                                       
