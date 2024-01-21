/*
 * A client for term. Hacked about by Jeff Grills, bashed a little here and
 * there by me. Mostly formatting changes + a few error condition
 * changes. 
 * [-u] (unlink) by Darren Drake (drd@gnu.ai.mit.edu)
 * File checksumming by Olaf Titz (olaf@bigred.ka.sub.org)
 */

/* If the remote file exists, tupload tries to checks if it can resume
 * with the following conditions:
 *
 * 	- Either the remote file is read only or it is the same size as the 
 *	  local
 *	- The remote file is not larger than the local file.
 *	- The local file has not been edited, touched, or otherwise modified.
 *	- The remote and local checksums match.
 * (bcr@physics.purdue.edu)
 */

#define I_IOCTL
#define I_SYS
#define I_STRING
#define I_STAT
#define I_TIME
#define I_LIMITS
#define DEBUG

#include "includes.h"
#include "client.h"

#ifdef ONE_CLIENT
# define main tupload
#else
int term_debug = 0;
#endif

int force = 0;
int unlinkmode = 0;
int literal_filename = 0;

struct stat st;

/*---------------------------------------------------------------------------*/

static int local_options ( char opt, char *optarg )
{
  switch(opt)
  {
  case 'f' :
    force = 1;
    break;
  case 'q' :
    verbose = 0;
    break;
  case 'u':
    unlinkmode = 1;
    break;
  case 'l':
    literal_filename = 1;
    break;
  default:
    return -1;
  }
  return 0;
}
/*---------------------------------------------------------------------------*/

#define CPS_WINDOW 4
#define PRIME_WRITE_SIZE 509

double upload_delta_timeval(struct timeval *a, struct timeval *b)
{
  return (a->tv_sec + a->tv_usec/1000000.0) - 
         (b->tv_sec + b->tv_usec/1000000.0);
}

int main(int argc, char *argv[])
{
				/* From client.c */
  extern int lcompression, rcompression;
				/* locals for upload. */
  struct Buffer buffer;
  char *cutpath, *file, *remote, path[255], filename[255];
  int term, fd, first, stdin_used, type, perms, i, lcksum, rcksum;
  int filesent, j=0, fullperms=0;
  long ret, remote_size, bytesent, total_bytesent, bytesent_base;
  long dumpsize, cpsbytes, write_len;
  double cps = 0;
  struct timeval total_starttime, total_stoptime, starttime, lasttime, nowtime;
  double etime, total_etime = 0;
  double recent_cpsbytes[CPS_WINDOW], recent_etime[CPS_WINDOW];
  int cps_count, done;
  fd_set writes;
  int permissions;
  long atime = 0, mtime = 0, rmtime = 0;

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif

  term = -1;
  fd = -1;
  first = -1;
  stdin_used = 0;
  priority = -5; /* The scale is -20 to 20 with higher being better */
  verbose = 1; 
  bytesent = 0;
  total_bytesent = 0;
  gettime(&total_starttime);
  filesent = 0;
  path[0] = '\0';
  lcompression = rcompression = 0;
  buffer.start = buffer.end = buffer.size = buffer.alloced = 0;
  buffer.data = NULL;
  
  /* Handle the options. We use the standard client argument handling */
  /* to do this. Options peculiar to upload are 'f' for force, q for */
  /* quiet, and v for verbose, u for unlink */
  if ( ((first = client_options(argc,argv,"fquvl",local_options)) == -1)
      || (first >= argc) ) {
    fprintf(stderr, 
	    "Usage: upload [-f] [-q] [-u] [-v] [-r] [-c] [-p <num>] <file>"
	    " [... <file>] [remote dir]\n"); 
    exit(1);
  }

  use_term_command(PRIVILEGED);

				/* Make sure the buffer is non-zero in */
				/* size. */
  add_to_buffer(&buffer, 0);
  get_from_buffer(&buffer);

#ifdef linux
  term = x__socket(AF_INET, SOCK_STREAM, 0);
#else
  term = -1;
#endif
  if ((term = socket_connect_server(term, term_server)) < 0) {
    fprintf(stderr,"Term: %s\n", command_result);
    exit(1);
  }

  /* check the last arg to see if it's a dir, and if so,  */
  /* that's the path we send to. */
  if ((first < argc) && (send_command(term, C_STAT, 0, "%s",
				      argv[argc-1]) >= 0)) {
    sscanf(command_result, "%ld %d %d", &remote_size, &type, &perms);

    /* check if it's a dir, and writable. */
    if (type == 1) {
      strcpy(path, argv[--argc]);
      if ( ! (i = strlen(path))) {
	fprintf(stderr, "\tFatal. Zero length path passed.\n");
	exit(1);
      }
      if ( path[i - 1] != '/' ) {
	path[i] = '/';
	path[i+1] = '\0';
      }
    }
  }

  /* should check here for a file to send.  enforce command line. */
  /*   or, maybe no args should mean take from stdin. */
  /*   but then, what's the output file name? -q */

  while ( first < argc ) {

    /* close the input file if it was left open */
    if (fd > 0) {
      x__close(fd);
      fd = -1;
    }

    /* get the filename to send.*/
    file = argv[first++];

    if ( (first+1 < argc) &&
        (! strcmp(argv[first],"-as")|| !strcmp(argv[first],"--as")) ) {
      remote = argv[first+1];
      first += 2;
    }
    else {
      /* leave the filename alone */
      remote = file;
      
      /* remove the pathname for the outgoing file */
      if (!literal_filename) {
          for ( cutpath = remote; *cutpath; cutpath++ )
              if ( ( *cutpath == '/' ) && ( *(cutpath+1) ) )
                  remote = cutpath + 1;
      }
    }
    
    /* prepend the specified path, if there is one. */
    if ( *path ) {
      strcpy(filename, path);
      strcat(filename, remote);
      remote = filename;
    }

    if ( verbose > 0 )
      if ( file == remote )
	fprintf(stderr, "sending %s\n",file);
      else
	fprintf(stderr, "sending %s as %s\n", file, remote );
    
    /* open input file, or use stdin. The  stdin is a bit of a kludge */
    /* but it can be useful. */
    fd = -1;
    if ( strcmp(file, "-") ) 
      fd = open(file, O_RDONLY);
    else 
      /* try to use stdin's file descriptor */
      if ( stdin_used++ ) {
	fprintf(stderr, "\tSkipped : can only take input from stdin once\n");
        j=1;
	continue;
      }
      else
	fd = 0;
    
    if (fd < 0) {
      fprintf(stderr, "\tSkipped : can't open local file\n");
      j=1;
      continue;
    }

    /* get some info on the local file. We need to know the file */
    /* size at the very least. */

    if (fd) {
      stat(file, &st);

      if ( st.st_mode & S_IFDIR ) {
        fprintf(stderr,"\tSkipping directory.\n");
        continue;
      }
      permissions = (int) st.st_mode & 0777;
      atime = (long) st.st_atime;
      mtime = (long) st.st_mtime;
    }else {
      int current_umask = umask(0);
      umask(current_umask);
      permissions = 0666 & ~current_umask;
      atime = mtime = 0;
    }

    /* see if the file exists to be resumed. Done by asking term to */
    /* stat the file on the remote end. */
    remote_size = 0;
    if (remote_term_version>=20164) {
      i = send_command(term, C_STAT, 0, "\n%ld\n%s", 0, remote);
    }else
      i = send_command(term, C_STAT, 0, "%s", remote);
    if (i >= 0) {
      gettime(&nowtime);
      if (force && remote_term_version < 20164)
	fprintf(stderr,"\tWarning, overwriting file on remote host\n");
      else
	if (!fd) {
          if (force) {
	    fprintf(stderr,"\tWarning, overwriting file on remote host\n");
          }else {
	    fprintf(stderr,"\tCannot resume from stdin\n");
            j=1;
	    continue;
          }
	}else {
	  
	  /* Ok. The remote file exists. lets check a few things here.. */
          fullperms = 0; rcksum = 0;
	  if (sscanf(command_result,"%ld %*d %*d %*d %ld %o %u",
		     &remote_size, &rmtime, &fullperms, &rcksum) < 1) {
            remote_size = 0;
          }
		/* The permissions on the remote file have been changed. */
	  if (fullperms & 0333) {
            if (force) {
              remote_size = (remote_size != (long)st.st_size) ? 0 : remote_size;
            }else {
	      fprintf(stderr, "\tSkipping, remote file has write/execute permission\n");
	      j=1;
              continue;
            }
	  }
 
	  	/* Local file has been changed since last upload. */
	  if ((remote_size || ! force) && (rmtime) && (rmtime+nowtime.tv_sec < mtime)) {
            if (force) {
              remote_size = 0;
            }else {
	      fprintf(stderr,"\tSkipping, the local file has been modified\n");
              j = 0;
              continue; 
            }
          }

	  	/* remote file is same size.  Identical? */
	  if ( (remote_size || ! force) && remote_size == (long)st.st_size ) {
	    if (! force) fprintf(stderr,"\tRemote file is same size as local");
	    if (rcksum) {
	      lcksum = file_crc(file, (long)st.st_size);
	      if (lcksum) {
		if (lcksum==rcksum) {
	          if (force && verbose > 1) fprintf(stderr,
                    "\tSame size local and remote file");
		  else 
                    fprintf(stderr, ", checksums do match");
		  /* Same file - set mode right */
		/* For old versions this will be ignored anyways... */
		  if (send_command(term, C_CHMOD, 0, "%o %ld %ld\n%s", 
			  permissions,
                          (atime) ? (atime - (long)nowtime.tv_sec) : 0,
			  (mtime) ? (mtime - (long)nowtime.tv_sec) : 0, 
			  remote) && (! force || verbose > 1))
		    fprintf(stderr, ", setting file mode");
                  if (!force || verbose > 1) fprintf(stderr, "\n");
                  continue;
		}else if (force) {
                  remote_size = 0;
                }else {
		  fprintf(stderr, ", checksums do not match, skipping\n");
                  j=1;
                  continue;
                }
	      }
	    }else if (force) {
              remote_size = 0;
	    } else {
	      fprintf(stderr, ", skipping\n");
	      j=1;
              continue;
            }
	  } 
	  	/* remote file is larger than current file. Skip this.*/ 
	  else if ( remote_size && remote_size >  (long)st.st_size ) {
            if (force) {
              remote_size = 0;
	    }else {
	      fprintf(stderr,"\tSkipping, remote file is larger than local\n");
              j=1;
	      continue;
            }
	  }
	  	/* Remote is a different shorter file. Skip */
	  else if ((remote_size || ! force) && rcksum) {
	    lcksum = file_crc(file, remote_size);
	    if ((lcksum) && (rcksum!=lcksum)) {
              if (force) {
                remote_size = 0;
              }else {
	        fprintf(stderr, "\tSkipping, remote file differs from beginning of local\n");
	        j=1;
	        continue;
              }
	    }
	  }

          if (force && ! remote_size) 
	    fprintf(stderr,"\tWarning, overwriting file on remote host\n");
	  else
            fprintf(stderr, "\tAttempting to restart upload from %ld\n",
              remote_size);
	}
    }
    
    if (remote_size) {
      /* open file on the remote end. We use C_OPEN instead of */
      /* C_UPLOAD as we don't want to truncate the file. */
      
      if (remote_term_version >= 20057) {
        if (send_command(term, C_OPEN, 0, "%ld %o\n%s", 
            remote_size, 0400, remote) < 0) {
	  fprintf(stderr,"\tSkipped : Couldn't open remote file, %s\n",
		command_result);
          j=1;
	  continue;
        }    
      }else {  /* With older versions we had to send multiple commands :-( */
        if (send_command(term, C_OPEN, 0, "%s", remote) < 0) {
	  fprintf(stderr,"\tSkipped : Couldn't open remote file, %s\n",
		command_result);
          j=1;
	  continue;
        }    
        /* do the remote file seek. */
        if (send_command(term, C_SEEK, 0, "%ld", remote_size) < 0) {
    	  fprintf(stderr, "\tSkipped, remote seek failed, %s\n",
		command_result); 
          j=1;
	  continue;
        }
      } 
      /* do the local file seek */
      if (lseek(fd, (off_t)remote_size, 0) < (off_t)0) {
	fprintf(stderr, "\tSkipped, local seek failed, ");
	x__perror("Reason given");
        j=1;
	continue;
      }
    }else {
      /* it's a new file. Use C_UPLOAD to open, and possibly creat, */
      /* or truncate it. */
      if (remote_term_version >= 20057) {
        if (send_command(term, C_UPLOAD, 0, "0 %o\n%s", 0400, remote) < 0) {
          fprintf(stderr,"\tSkipped : Couldn't open remote file, %s\n",
   		command_result);
	  j=1;
          continue;
        }
      }else {
        if (send_command(term, C_UPLOAD, 0, "%s", remote) < 0) {
          fprintf(stderr,"\tSkipped : Couldn't open remote file, %s\n",
   		command_result);
	  j=1;
          continue;
        }
      }
    }
    
    /* dump the file over the socket. We handle this by using C_DUMP */
    /* commands to escape the data. */
    if (verbose > 1) {
      gettime(&starttime);
      lasttime = starttime;
    }
    bytesent = 0;
    bytesent_base = 0;
    cpsbytes = 0;
    dumpsize = 0;
    cps_count = 0;
    done = 0;
    for (i = 0; i < CPS_WINDOW; ++i) {
      recent_cpsbytes[i] = 0;
      recent_etime[i] = 0;
    }
    filesent++;
    do {
      if (buffer.size < PIPE_BUFFER) {
	ret = (long)(read_into_buff(fd, &buffer,0)); 
	if (ret < 0) break;
	if (ret > 0 && buffer.size < PIPE_BUFFER) continue;
      }
      if (buffer.size == 0)
        done = 1;
      else {
        if (dumpsize < 0) {
          fprintf(stderr, "\tAborted, dumpsize %ld < 0\n", dumpsize);
          j=1;
	  break;
        }
        if (dumpsize == 0) {
	  if (send_command(term, C_DUMP, 1, "%d", buffer.size) < 0) {
	    fprintf(stderr, "\tAborted, couldn't turn off command"
		  " processing, %s\n", command_result );
            j=1;
	    break;
	  }
	  dumpsize = buffer.size;
        }
        set_nonblock(term);
        termerrno = 0;
        write_len = dumpsize < PRIME_WRITE_SIZE ? dumpsize : PRIME_WRITE_SIZE;
	ret = (long) write_from_buff_async(term, &buffer, write_len);
	if (ret <= 0 && termerrno) {
	  fprintf(stderr, "\tError writing to term server. Exiting..\n");
	  exit(1);
        }

	bytesent += ret;
	total_bytesent += ret;
	remote_size += ret;
	cpsbytes += ret;
	dumpsize -= ret;
	if (write_len == ret && cpsbytes < 65536)  /* guarantee some output */
	  continue;
        if (cpsbytes == 0)
          continue;	/* non-blocking lossage or some such */
      }

      gettime(&nowtime);
      if ( (verbose > 2)) {

	  /* Compute CPS as follows.  We only get here when we were unable
	     to write all the data in the buffer, which means that we have
	     stuffed the pipe full.  We use PRIME_WRITE_SIZE so that it
	     will be very uncommon for a write to complete fully at the 
	     exact end of the pipe.  For the first second of uploading,
	     we ignore CPS, since we don't know the depth of the pipe
	     to term.  From then on, each CPS value is placed in a
	     CPS_WINDOW-sized buffer and the average of the buffer is
	     used as the CPS value.  */

	  etime = upload_delta_timeval(&nowtime, &lasttime);

          lasttime = nowtime;
	  if (upload_delta_timeval(&nowtime, &starttime) < 1) {
	    cps = 0;
	    bytesent_base = bytesent;	/* use to adjust ETA */
          } else {
	    recent_cpsbytes[cps_count] = cpsbytes;
	    recent_etime[cps_count] = etime;
	    if (++cps_count >= CPS_WINDOW)
	      cps_count = 0;
            for (i = 0, cps = 0, etime = 0; i < CPS_WINDOW; ++i) {
              cps += recent_cpsbytes[i];
              etime += recent_etime[i];
            }
            if (etime > 0)
              cps /= etime;
            else
              cps = 0;
          }

	  if ( fd && (st.st_size) ) {
            long perc;

	    perc = (remote_size*100) / (long)st.st_size;
	    if (cps)
	      fprintf(stderr, "\r\t%ld of %ld (%ld%%), current CPS %.0f. ETA: %.1f TT: %.1f   ",
	            remote_size, (long)st.st_size, perc, 
	            cps, (st.st_size-remote_size+bytesent_base)/cps, 
	            st.st_size/cps);
	    else
	      fprintf(stderr, "\r\t%ld of %ld (%ld%%)   ", 
	            remote_size, (long)st.st_size, perc);
	  }
	  else if (!fd) {
	    if (cps)
	      fprintf(stderr, "\r\t%ld, current CPS %.0f. ", remote_size, cps );
	    else
	      fprintf(stderr, "\r\t%ld  ", remote_size);
          }

	  fflush(stderr);
	  cpsbytes = 0;
      }

      if (done)
	break;

      /* Wait until we can write again before we try reading any more */
      FD_ZERO(&writes);
      FD_SET(term, &writes);
      set_block(term);
      select(term+1, NULL, &writes, NULL, (struct timeval *)NULL);
    } while (1); 

    /* give them cps ratings */
    if (verbose > 1) {
      gettime(&nowtime);
      etime = upload_delta_timeval(&nowtime, &starttime);
      total_etime += etime;
      if (etime == 0)
        etime = 0.01;
      fprintf(stderr, "\r\t%ld bytes sent in %.1f seconds;"
             " CPS = %.0f                   ",
             bytesent, etime, bytesent/((etime < 0.5) ? 0.5 : etime));
    }

    /* Close the file */
    send_command(term, C_CLCLOSE, 0, 0);
    send_command(term, C_WAIT, 0, 0);  /* We must delay, to allow the file to */
				       /* to close before we send C_CHMOD */
    send_command(term, C_CHMOD, 0, "%o %ld %ld\n%s", permissions,
      (atime) ? (atime - (long)nowtime.tv_sec) : 0,
      (mtime) ? (mtime - (long)nowtime.tv_sec) : 0, remote);

    /* check the remote file after send */
    if (remote_term_version>=20164) {
      i = send_command(term, C_STAT, 0, "\n%ld\n%s", 0, remote);
      fprintf(stderr, "CRC ");
    }
    else
      i = send_command(term, C_STAT, 0, "%s", remote);
    if (i >= 0) {
      long l=0;
      rcksum = 0;
      if(sscanf(command_result,"%ld %*d %*d %*d %*d %*o %u",
               &l, &rcksum)<1)
        l = 0;
      if (( fd && (l != (long)st.st_size) ) ||
         ( (!fd) && (l != bytesent) )) {
       fprintf(stderr, "F\n\tRemote file is a different size from local after"
               " upload!\n\t(r=%ld,l=%ld)\n", l, (long)st.st_size); 
       j=1;
       continue;
      }
      if (fd && rcksum) {
       lcksum = file_crc(file, (long)st.st_size);
       if (lcksum && (lcksum!=rcksum)) {
         fprintf(stderr, "F\n\tRemote and local file checksums don't match"
                 " after upload!\n");
         j=1;
         continue;
       } else {
         fprintf(stderr, "OK");
       }
      }
	/* unlink files if we wanna remove them after *successful* send */
	if(fd && unlinkmode) {
	  if((unlink(file))&&(verbose>1)) {
	    fprintf(stderr,"\n\tUnable to remove sent file\n");
            j=1;
	  }else {
	    fprintf(stderr,"\n\tSent file removed.\n");
          }
	} else fprintf(stderr, "\n");
    }
    else {
      fprintf(stderr,"\n\t%s",command_result);
      fprintf(stderr,"\n\tcouldn't stat remote file after upload. Please"
	      " check it.\n"); 
      j=1;
    }
  }

  /* give them global cps rating */
  if ( (verbose > 1) && (filesent > 1) ) {
    gettime(&total_stoptime);
    etime = upload_delta_timeval(&total_stoptime, &total_starttime);
    if (etime == 0)
      etime = 0.01;
    fprintf(stderr, "%ld total bytes sent in %.1f seconds; overall CPS %.0f\n",
      total_bytesent, etime, total_bytesent/((etime < 0.5) ? 0.5 : etime)); 
  }

  exit(j);
}


