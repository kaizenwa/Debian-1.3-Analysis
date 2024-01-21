/*
 * sysinfo.c		- get kernel info
 *
 */

/* 
 * Written by Gabor Herr <herr@iti.informatik.th-darmstadt.de>.
 *
 * Copyright (c) 1992, 1993 by Gabor Herr, all rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear in
 * supporting documentation, and that may name is not used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. I make no representations
 * about the suitability of this software for any purpose. It is
 * provided "as is" without express or implied warranty.
 */

/* $Id: sysinfo.c,v 1.3 1993/02/12 11:40:16 gabor Exp $ */
 
#include "sysinfo.h"
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>

static int meminfo_fd;
static int load_fd;
static int loadavg_fd;

static char buffer[1024];

static void reread( int fd, char *message )
{
  if ( lseek( fd, 0L, 0 ) == 0 &&
       read( fd, buffer, sizeof(buffer) - 1 ) > 0 )
    return;
       
  perror(message);
  exit(1);
}

static int getentry(const char *tag, const char *bufptr) {
  char *tail;
  int retval, len = strlen(tag);
  
  while (bufptr) {
    if (*bufptr == '\n') bufptr++;
    if (!strncmp(tag, bufptr, len)) {
      retval = strtol(bufptr + len, &tail, 10);
      if (tail == bufptr + len) {
        return -1;
      } else {
        return retval;
      }
    }
    bufptr = strchr( bufptr, '\n' );
  }
  return -1;
}

void get_meminfo( int *swapdevs, struct meminfo *result )
{
  int i;
  char *bufptr;
  int res;

  reread( meminfo_fd, "get_meminfo:");

  /* Try new /proc/meminfo format first */
  *swapdevs = 1;
  if ((result[0].total = getentry("MemTotal:", buffer)) < 0 ||
      (result[0].free  = getentry("MemFree:", buffer)) < 0 ||
      (result[0].shared = getentry("MemShared:", buffer)) < 0 ||
      (result[0].buffers = getentry("Buffers:", buffer)) < 0 ||
      (result[0].cache = getentry("Cached:", buffer)) < 0 ||
      (result[1].total = getentry("SwapTotal:", buffer)) < 0 ||
      (result[1].free = getentry("SwapFree:", buffer)) < 0) {

    /* Fall back to old format */
    bufptr = strchr( buffer, '\n' ) + 1;
    sscanf( bufptr, "Mem: %d %*d %d %d %d",
    	&result[0].total,
    	&result[0].free,
    	&result[0].shared,
    	&result[0].cache );
    result[0].buffers = -1;
    
    for ( i = 1; i < MAX_SWAPFILES; i++ ) {

      bufptr = strchr( bufptr, '\n' ) + 1;

      if ( *bufptr == '\0' ||
  	 (res = sscanf( bufptr, "Swap: %d %*d %d",
    		&result[i].total,
    		&result[i].free )) != 2 )
  	break; 

    }

    *swapdevs = i - 1;  
  }
}

double get_loadavg( void )
{
  double load;

  reread( loadavg_fd, "get_load:");

  sscanf( buffer, "%lf", &load );

  return load;
}

void get_load(struct load * result)
{
  static struct load last_load = { 0, 0, 0, 0, 0 };
  struct load curr_load;

  reread( load_fd, "get_load:" );
  sscanf( buffer, "%*s %lu %lu %lu %lu\n", &curr_load.user, &curr_load.nice, &curr_load.system, &curr_load.idle );
  curr_load.total = curr_load.user + curr_load.nice + curr_load.system + curr_load.idle;

  result->total  = curr_load.total  - last_load.total;
  result->user   = curr_load.user   - last_load.user;
  result->nice   = curr_load.nice   - last_load.nice;
  result->system = curr_load.system - last_load.system;
  result->idle   = curr_load.idle   - last_load.idle;
  last_load.total  = curr_load.total;
  last_load.user   = curr_load.user;
  last_load.nice   = curr_load.nice;
  last_load.system = curr_load.system;
  last_load.idle   = curr_load.idle;
}

int sysinfo_init( void )
{
  if ((meminfo_fd = open("/proc/meminfo",O_RDONLY)) < 0) {
    perror("/proc/meminfo");
    return 1;
  }
  if ((loadavg_fd = open("/proc/loadavg",O_RDONLY)) < 0) {
    perror("/proc/loadavg");
    return 1;
  }
  if ((load_fd = open("/proc/stat",O_RDONLY)) < 0) {
    perror("/proc/stat");
    return 1;
  }
  return 0;
}

