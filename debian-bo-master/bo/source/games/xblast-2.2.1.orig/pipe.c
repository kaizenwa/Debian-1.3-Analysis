/*
 * Programm XBLAST V2.1.11 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * November 15th 1996
 * started August 1993
 *
 * File: pipe.c
 * ipc routines
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <unistd.h>
#include <errno.h>

#define _PIPE_C

#include "include.h"
#include "const.h"
#include "mytypes.h"
#include "pipe.h"
#include "main.h"

/* 
 * a simple 32Bit random generator 
 * it will also be used to sync client and server
 */
#define RANDOM_MAX 4294967296.0
static unsigned rnum;

/*
 * public function seed_random
 *   seeds the random number generator
 */
#ifdef __STDC__
void
seed_random (unsigned seed)
#else
void
seed_random (seed)
  unsigned seed;
#endif
{
  rnum = seed;
}

/* 
 * local function random
 *   creates a 32 Bit pseudo random number
 */
#ifdef __STDC__
static unsigned 
random (void)
#else
static unsigned 
random ()
#endif
{
  rnum = 1664525L * rnum + 1013904223L;
  return rnum;
}

/*
 * public function random_number
 *  creates an integer random number between 0 and max_val-1;
 */
#ifdef __STDC__
int 
random_number (int max_val)
#else
int 
random_number (max_val)
  int max_val;
#endif
{
  return (int)(random()/RANDOM_MAX * max_val);
}



/*
 * now the actual IPC code
 */

/* local variables */
int num_children = 0;
static int max_fin = 0;
static XBConnection pipe_parent;
static XBConnection pipe_child[MAX_DISPLAY];

#ifdef __STDC__
int
create_child (void)
#else
int 
create_child ()
#endif
{
  int toChild[2], fromChild[2];
  int cpid, ppid;

  /* id of parent process */
  ppid = getpid();

  /* first create pipes */
  if ((pipe(toChild)) || 
      (pipe(fromChild)) ) {
    fprintf (stderr, "failed to create pipe\n");
    exit_prg(1);
  }
  
  /* now fork process */
  if (0 == (cpid = fork() )) {
#ifdef DEBUG
    fprintf(stderr, "Child [%d] created.\n", getpid());
#endif
    /* child process running */
    pipe_parent.in = toChild[0];
    pipe_parent.out = fromChild[1];
    pipe_parent.pid = ppid;
    /* close other files */
    close(toChild[1]);
    close(fromChild[0]);
    
    return 0;
  } else {
#ifdef DEBUG
    fprintf(stderr, "Parent [%d] forked.\n", getpid());
#endif
    /* parent process running */
    pipe_child[num_children].in = fromChild[0];
    pipe_child[num_children].out = toChild[1];
    pipe_child[num_children].pid = cpid;
    /* update max_fin */
    if (pipe_child[num_children].in > max_fin) {
      max_fin = pipe_child[num_children].in;
    }
    /* close other files */
    close(fromChild[1]);
    close(toChild[0]);
    /* increment number of childred*/
    num_children++;

    return cpid;
  }
}


/*
 * dummy functions
 */ 
#ifdef __STDC__
void
no_keys_to_server (void) 
#else
void
no_keys_to_server ()
#endif
{
}

#ifdef __STDC__
void
no_keys_to_clients (void)
#else
void
no_keys_to_clients ()
#endif
{
}

/*
 * IO function for in game keypress exchange between children and parent
 */

/*
 * public function: child_link_keys
 *
 * links player action struct to iovec struct 
 */
#ifdef __STDC__
void
child_link_keys (PlayerAction *pa, 
		 int p1, 
		 int p2)
#else
void
child_link_keys (pa, p1, p2)
     PlayerAction *pa; 
     int p1, p2;
#endif
{
  int i;

  /* output io vector */
  pipe_parent.nkey_out = (p1==p2) ? 1 : 2;
  pipe_parent.key_out[0].iov_len = sizeof(PlayerAction);
  pipe_parent.key_out[0].iov_base = (caddr_t) (pa + p1);
  if (p1!=p2) {
    pipe_parent.key_out[1].iov_len = sizeof(PlayerAction);
    pipe_parent.key_out[1].iov_base = (caddr_t) (pa + p2);
  } 
  /* get number of bytes */
  pipe_parent.skey_out = 0;
  for (i=0; i<pipe_parent.nkey_out; i++) {
    pipe_parent.skey_out += pipe_parent.key_out[i].iov_len;
  }
  
  /* input io vector */
  pipe_parent.nkey_in = 1;
  pipe_parent.key_in[0].iov_len = MAX_PLAYER*sizeof(PlayerAction);
  pipe_parent.key_in[0].iov_base = (caddr_t) pa;
  /* get number of bytes */
  pipe_parent.skey_in = 0;
  for (i=0; i<pipe_parent.nkey_in; i++) {
    pipe_parent.skey_in += pipe_parent.key_in[i].iov_len;
  }
}


/*
 * public function: child_link_keys
 *
 * links player action struct to iovec struct 
 */
#ifdef __STDC__
void
parent_link_keys (int child,
		  PlayerAction *pa, 
		  int p1, 
		  int p2)
#else
void
parent_link_keys (child, pa, p1, p2)
     int child;
     PlayerAction *pa; 
     int p1, p2;
#endif
{
  int i;

  if (child <0) {
    child = num_children -1;
  }

  /* input vector */
  pipe_child[child].nkey_in = (p1==p2) ? 1 : 2;
  pipe_child[child].key_in[0].iov_len = sizeof(PlayerAction);
  pipe_child[child].key_in[0].iov_base = (caddr_t) (pa + p1);
  if (p1!=p2) {
    pipe_child[child].key_in[1].iov_len = sizeof(PlayerAction);
    pipe_child[child].key_in[1].iov_base = (caddr_t) (pa + p2);
  }
  /* get number of bytes */
  pipe_child[child].skey_out = 0;
  for (i=0; i<pipe_child[child].nkey_out; i++) {
    pipe_child[child].skey_out += pipe_child[child].key_out[i].iov_len;
  }
  

  /* output vector */
  pipe_child[child].nkey_out = 1;
  pipe_child[child].key_out[0].iov_len = MAX_PLAYER*sizeof(PlayerAction);
  pipe_child[child].key_out[0].iov_base = (caddr_t) pa;
  /* get number of bytes */
  pipe_child[child].skey_in = 0;
  for (i=0; i<pipe_child[child].nkey_in; i++) {
    pipe_child[child].skey_in += pipe_child[child].key_in[i].iov_len;
  }
}


/*
 * public function: send_keys_to_parent
 *
 * send keystrokes of local player(s) to parent process
 */
#ifdef __STDC__
void
send_keys_to_parent (void) 
#else
void
send_keys_to_parent ()
#endif
{
  writev(pipe_parent.out, pipe_parent.key_out, pipe_parent.nkey_out);
}


/*
 * public function: get_keys_from_parent
 *
 * get keystrokes for all players from parent
 */
#ifdef __STDC__
void
get_keys_from_parent (void)
#else
void
get_keys_from_parent ()
#endif
{
  int result;

  result = readv(pipe_parent.in, pipe_parent.key_in, pipe_parent.nkey_in);
  if (result>0) {
    return;
  } 
  if (result<0) {
    fprintf (stderr, "Failed to read keys from parent\n");
    exit_prg(1);
  }
  fprintf(stderr, "Parent has closed connection.\n");
  return;

}



/*
 * public function: get_keys_from_children
 *
 * receive keystroke form all child processes
 */
#ifdef __STDC__
void
get_keys_from_children (void) 
#else
void
get_keys_from_children ()
#endif
{
  fd_set rset;
  int checked = 0;
  int i;

  /* init file descriptor array */
  FD_ZERO(&rset);
  for (i=0; i<num_children; i++) {
    FD_SET( (pipe_child[i].in), &rset);
  }

  while (checked < num_children) {
    if (select (max_fin+1, &rset, NULL, NULL, NULL) > 0) {
      for (i=0; i<num_children; i++) {
	if (FD_ISSET( (pipe_child[i].in), &rset)) {
	  /* read keys from this child */
	  readv(pipe_child[i].in, pipe_child[i].key_in, 
		pipe_child[i].nkey_in);
	  /* correct bits and counter */
	  checked ++;
	  FD_CLR((pipe_child[i].in), &rset);
	} else {
	  /* set bit again */
	  FD_SET((pipe_child[i].in), &rset);
	}
      }
    }
  }
}



/*
 * public function: send_keys_to_children
 *
 * send all players' keypresses to all child processes
 */
#ifdef __STDC__
void
send_keys_to_children () 
#else
void
send_keys_to_children ()
#endif
{
  int i;

  for (i=0; i<num_children; i++) {
    writev(pipe_child[i].out, pipe_child[i].key_out, 
	   pipe_child[i].nkey_out);
  }
}


/*
 * IO function for string data exchange between parent and children 
 */

/*
 * public function string to children
 *
 * sends a string to all children 
 */
#ifdef __STDC__
void
string_to_children (char *buf) 
#else
void
string_to_children (buf) 
     char *buf;
#endif
{
  struct iovec iov[2];
  int i;
  short length;

  length = strlen(buf)+1;
  iov[0].iov_len  = sizeof(short);
  iov[0].iov_base = (caddr_t) &length;
  iov[1].iov_len  = length;
  iov[1].iov_base = (caddr_t) buf;

#ifdef DEBUG_IPC
  fprintf(stderr, "STC: %s\n", buf);
#endif

  for (i=0; i<num_children; i++) {
    writev(pipe_child[i].out, iov, 2);
  }
}


/*
 * public function string_from_children
 *
 * receives a string from children
 */
#ifdef __STDC__
int
string_from_children (char *buf)
#else
int
string_from_children (buf)
     char *buf;
#endif
{
  int i;
  short length;
  fd_set rset;

  /* init file descriptor array */
  FD_ZERO(&rset);

  for (i=0; i<num_children; i++) {
    FD_SET( (pipe_child[i].in), &rset);
  }
  
  if (select (max_fin+1, &rset, NULL, NULL, NULL) > 0) {
    for (i=0; i<num_children; i++) {
      if (FD_ISSET( (pipe_child[i].in), &rset)) {
	/* read from this child */
	read(pipe_child[i].in, (caddr_t) &length, sizeof(short));
	read(pipe_child[i].in, (caddr_t) buf, length);
	return i;
      }
    }
  }
  fprintf(stderr, "Oops! select returned no fd\n");
  exit_prg(1);
  return -1;
}


/*
 * public function: string_to_parent
 *
 * send a string to parent
 */
#ifdef __STDC__
void
string_to_parent (char *buf)
#else
void
string_to_parent (buf)
     char *buf;
#endif
{
  struct iovec iov[2];
  short length;

  length = strlen(buf)+1;
  iov[0].iov_len  = sizeof(short);
  iov[0].iov_base = (caddr_t) &length;
  iov[1].iov_len  = length;
  iov[1].iov_base = (caddr_t) buf;

  writev(pipe_parent.out, iov, 2);
}



/*
 * public function: string_from_parent
 *
 * receive a parent from parent
 */
#ifdef __STDC__
void
string_from_parent (char *buf)
#else
void
string_from_parent (buf)
     char *buf;
#endif
{
  short length;

  read(pipe_parent.in, (caddr_t) &length, sizeof(short));
  read(pipe_parent.in, (caddr_t) buf, length);

#ifdef DEBUG_IPC
  fprintf(stderr, "SFP: %s\n", buf);
#endif
}



/*
 * public function: buffer_to_children
 *
 * send a generic buffer to child processes
 */
#ifdef __STDC__
void
buffer_to_children (int nbytes,
		    caddr_t buf)
#else
void
buffer_to_children (nbytes, buf)
     int nbytes;
     caddr_t buf;
#endif
{
  int i;

#ifdef DEBUG_IPC
  fprintf(stderr, "BTC: %d bytes\n", nbytes);
#endif

  for (i=0; i<num_children; i++) {
    if (0 > write(pipe_child[i].out, buf, nbytes)) {
      perror("BTC");
    }
  }
 
}



/*
 * public function: buffer_from_parent
 *
 * receive a buffer from parent
 */
#ifdef __STDC__
void
buffer_from_parent (int nbytes,
		    caddr_t buf)
#else
void
buffer_from_parent (nbytes, buf)
     int nbytes;
     caddr_t buf;
#endif
{
  if (nbytes != read(pipe_parent.in, buf, nbytes) ) {
    fprintf (stderr, "failed to read buffer from pipe\n");
    perror("BFP");
  }

#ifdef DEBUG_IPC
  fprintf(stderr, "BFP: %d nbytes\n", nbytes);
#endif
}



/*
 * end of file pipe.c
 */



