/*
 * A client for term. Taken from upload.c sources by Chris Metcalf
 * (metcalf@lcs.mit.edu).
 * Hacked up for checksums by ot
 */

/* If the remote file exists, tupload tries to checks if it can resume
 * with the following conditions:
 *
 *      - Either the remote file is read only or it is the same size as the
 *        local
 *      - The remote file is not larger than the local file.
 *      - The local file has not been edited, touched, or otherwise modified.
 *      - The remote and local checksums match.
 * (bcr@physics.purdue.edu)
 */

#define I_IOCTL
#define I_STRING
#define I_STAT
#define I_TIME
#define I_UTIME
#define I_TYPES
#define DEBUG

#include "includes.h"
#include "client.h"

#ifdef ONE_CLIENT
# define main tdownload
#else
int term_debug = 0;
#endif
int download_force = 0;

int download_unlinkmode = 0;
int download_literal_filename = 0;

struct stat st;

/*---------------------------------------------------------------------------*/

static int local_options ( char opt, char *optarg )
{
  switch(opt)
  {
  case 'f' :
    download_force = 1;
    break;
  case 'q' :
    verbose = 0;
    break;
  case 'u':
    download_unlinkmode = 1;
    break;
  case 'l':
    download_literal_filename = 1;
    break;
  default:
    return -1;
  }
  return 0;
}
/*---------------------------------------------------------------------------*/

#define CPS_WINDOW 10		/* about ten seconds */

double download_delta_timeval(struct timeval *a, struct timeval *b)
{
  return (a->tv_sec + a->tv_usec/1000000.0) - 
         (b->tv_sec + b->tv_usec/1000000.0);
}

int main(int argc, char *argv[])
{
				/* From client.c */
				/* locals for upload. */
  struct Buffer buffer;
  char *cutpath, *file, *local, path[255], filename[255];
  int term, fd, first, type, perms, i;
  unsigned int lcksum, rcksum;
  int filesent, j=0, fullperms=0;
  long ret, remote_size, local_size;
  long bytesrecvd, total_bytesrecvd;
  long atime, mtime, rmtime;
  double cps = 0;
  struct timeval total_starttime, total_stoptime, starttime, lasttime, nowtime;
  double etime, total_etime = 0;
  double recent_cpsbytes[CPS_WINDOW], recent_etime[CPS_WINDOW];
  int cps_count, read_size;
  int bufsiz = BUFSIZ;
  struct utimbuf utb;
  char *remote_unlink = NULL;

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif
  term = -1;
  fd = -1;
  first = -1;
  priority = -5; /* The scale is -20 to 20 with higher being better */
  verbose = 1;
  bytesrecvd = 0;
  total_bytesrecvd = 0;
  gettime(&total_starttime);
  filesent = 0;
  path[0] = '\0';
  buffer.start = buffer.end = buffer.size = buffer.alloced = 0;
  buffer.data = NULL;
  
  /* Handle the options. We use the standard client argument handling */
  /* to do this. Options peculiar to upload are 'f' for force, q for */
  /* quiet, and v for verbose, u for unlink */
  if ( ((first = client_options(argc,argv,"fquvl",local_options)) == -1)
      || (first >= argc) ) {
    fprintf(stderr, 
	    "Usage: download [-f] [-q] [-u] [-v] [-r] [-c] [-p <num>] <file>"
	    " [... <file>] [local dir]\n"); 
    exit(1);
  }

  use_term_command(PRIVILEGED);

				/* Make sure the buffer is non-zero in */
				/* size. */
  add_to_buffer(&buffer, 0);
  get_from_buffer(&buffer);
  if ((term = socket_connect_server(-1, term_server)) < 0) {
    fprintf(stderr,"Term: %s\n", command_result);
    exit(1);
  }

  /* check the last arg to see if it's a dir, and if so,  */
  /* that's where we put the received files. */
  if (first < argc && stat(argv[argc-1], &st) == 0 && S_ISDIR(st.st_mode)) {
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

  /* should check here for a file to send.  enforce command line. */
  /*   or, maybe no args should mean take from stdin. */
  /*   but then, what's the output file name? -q */

  while ( first < argc ) {

    /* close the output file if it was left open (it shouldn't be) */
    if (fd > 0) {
      x__close(fd);
      fd = -1;
    }

    /* get the filename to receive.*/
    file = argv[first++];

    if ( (first+1 < argc) &&
        (! strcmp(argv[first],"-as") || ! strcmp(argv[first],"--as")) ) {
      local = argv[first+1];
      first += 2;
    }
    else {
      /* leave the filename alone */
      local = file;
      
      /* remove the pathname for the outgoing file */
      if (!download_literal_filename) {
          for ( cutpath = local; *cutpath; cutpath++ )
              if ( ( *cutpath == '/' ) && ( *(cutpath+1) ) )
                  local = cutpath + 1;
      }
    }
    
    /* prepend the specified path, if there is one. */
    if ( *path ) {
      strcpy(filename, path);
      strcat(filename, local);
      local = filename;
    }

    if ( verbose > 0 ) {
      if ( file == local )
	fprintf(stderr, "receiving %s\n",file);
      else
	fprintf(stderr, "receiving %s as %s\n", file, local );
    }
    /* use stdout if requested */
    fd = -1;
    if ( !strcmp(local, "-") ) 
      fd = 1;

    /* get some info on the remote file. We need to know the file */
    /* size at the very least. */
    remote_size = 0;
    if (fd != 1 && remote_term_version >= 20164 && stat(local, &st) == 0) 
      i=send_command(term, C_STAT, 0, "\n%ld\n%s", st.st_size, file);
    else
      i=send_command(term, C_STAT, 0, "%s", file);

    rmtime = 0; fullperms = 0; rcksum = 0;
    if (i >= 0) {
      gettime(&nowtime);
      if (sscanf(command_result,"%ld %d %*d %*d %ld %o %u",
		   &remote_size, &type, &rmtime, &fullperms, &rcksum) < 2) {
	   fprintf(stderr, "\tSkipped : can't stat %s: %s\n", file,
		command_result);
	   j=1;
	   continue;
      }
      if (type != 0) {
	   fprintf(stderr, "\tSkipped : %s is not a regular file\n", file);
	   j=1;
	   continue;
      }
    }else {
      fprintf(stderr, "\tSkipped : can't open remote file\n");
      j=1;
      continue;
    }

    /* see if the file exists to be resumed (but don't resume to stdout,
       even if it's pointing to a file). */
    local_size = 0;
    if (fd != 1) {
      if (stat(local, &st) == 0) {
        local_size = st.st_size;
		/* Local file has been changed since last download. Warn */
        if (download_force && remote_term_version < 20164) {
           local_size = 0;
        }else {
	  if (rmtime && (rmtime+nowtime.tv_sec > st.st_mtime)) {
	    if (download_force) {
	      local_size = 0;
	    }else {
	      fprintf(stderr,"\tSkipping, the remote file has been modified\n");
	      j = 1;
	      continue;
	    }
          }
          if ((local_size || ! download_force) && (st.st_mode & 0333)) {
            if (download_force) {
              local_size = (local_size != remote_size) ? 0 : local_size;
            }else {
              fprintf(stderr, "\tSkipping, local file has write/exec permission\n");
              j=1;
              continue;
            }
          }
          else if ((local_size || ! download_force) && chmod(local, 0600)) {
            if (download_force) {
              local_size = 0;
            }else {
              fprintf(stderr, "\tSkipping, can't change file mode to 0600\n");
              j=1;
              continue;
            }
          }
	  /* Ok. The local file exists. lets check a few things here.. */

	  /* local file is larger than remote file. Skip this.*/ 
	  if (local_size && local_size > remote_size) {
            if (download_force) {
              local_size = 0;
            }else {
	      fprintf(stderr,"\tSkipping, local file is larger than remote\n");
              j=1;
	      continue;
            }
	  }

	  /* remote file is same size. Skip it. */
          if ((local_size || ! download_force) && local_size == remote_size) {
	    if (! download_force) 
              fprintf(stderr,"\tLocal file is same size as remote");
	    if (remote_term_version >= 20164 && i >= 0) {
	      fullperms = 0; rcksum = 0;
	      sscanf(command_result,"%*d %*d %*d %ld %ld %o %u",
		     &atime, &mtime, &perms, &rcksum);
	      if (rcksum) {
		lcksum = file_crc(local, (long)st.st_size);
		if (lcksum) {
		  if (lcksum==rcksum) {
		    long now;
		    now = time(NULL);
                    if(download_force && verbose > 1) 
			fprintf(stderr,"\tSame size local and remote file");
                    if(verbose > 1 || ! download_force)
		    	fprintf(stderr, ", checksums do match, setting mode");
		    utb.actime = atime + now;
		    utb.modtime = mtime + now;
                    if(verbose > 1 || ! download_force) {
		      if (utime(local, &utb))
		        fprintf(stderr, " (time failed)");
		      if (perms != -1 && chmod(local, perms))
		        fprintf(stderr, " (perms failed)");
		      fprintf(stderr,"\n");
                    }
	            if (! download_force) j=1;
                    continue;
		  }else if(download_force) {
		    local_size = 0;
                  }else {
		    fprintf(stderr, ", checksums do not match, skipping");
		  }
		}
	      }else if (download_force) {
                local_size = 0;
	      }else if (! download_force) {
		fprintf(stderr, ", skipping");
              }
	    }else if (download_force) {
              local_size = 0;
	    }else if (! download_force) {
	      fprintf(stderr, ", skipping");
            }
            if (! download_force) {
	      fprintf(stderr, "\n");
	      j=1;
	      continue;
            }
	  }
	  
	  /* Local file is smaller than remote. Perform additional checks */
	  if (local_size && (remote_term_version >= 20164) && i >= 0) {
	    if (rcksum) {
	      /* Local is a different shorter file. Skip */
	      lcksum = file_crc(local, local_size);
	      if ((lcksum) && (rcksum!=lcksum)) {
                if (download_force) {
                  local_size = 0;
                }else {
  	 	  fprintf(stderr, "\tSkipping, local file differs from beginning of remote\n");
                  j=1;
	          continue;
                }
	      }
	    }
          }
	  if (local_size || ! download_force ) {
            fprintf(stderr, "\tAttempting to restart download from %ld\n",
		  local_size);
          }
        }
      }

      if (local_size == 0) {
        if (download_force) {
          /* Try to make sure we can download to the file.  If we can */
          /* chmod it, we are certain to succeed.  If not, perhaps we have */
          /* write access as group or other; try to truncate.  If that */
          /* fails too, unlink it, and hope we can create a new file below. */
          if (chmod(local, 0666) < 0) {
            if ((fd = open(local, O_WRONLY|O_CREAT|O_TRUNC, 0666)) < 0) {
              (void) unlink(local);
              fd = open(local, O_WRONLY|O_CREAT|O_TRUNC, 0666);
            }
          }else {
            fd = open(local, O_WRONLY|O_CREAT|O_TRUNC, 0666);
          }
        }else {
          fd = open(local, O_WRONLY|O_CREAT|O_TRUNC, 0666);
        }
      }else {
        fd = open(local, O_WRONLY|O_APPEND, 0666);
      }
      chmod(local, 0444);
    }

    if (send_command(term, C_DOWNLOAD, 0, "%ld\n%s", local_size, file) < 0) {
      fprintf(stderr,"\tSkipped : Couldn't open remote file, %s\n",
		command_result);
      j=1;
      continue;
    }else {
      long now;
      now = time(NULL);
      atime = mtime = 0;
      perms = -1;
      if (sscanf(command_result, "%*d %d %*d %ld %ld %o",
		&type, &atime, &mtime, &perms) < 2) {
	   fprintf(stderr, "\tSkipped : can't stat %s: %s\n", file,
		command_result);
	   j=1;
           send_command(term, C_CLOSE, 0, 0);
	   continue;
      }
      atime += (long)now;
      mtime += (long)now;
    }

    /* receive the file over the socket. */
    if (verbose > 1) {
      gettime(&starttime);
      lasttime = starttime;
    }
    bytesrecvd = 0;
    cps_count = 0;
    for (i = 0; i < CPS_WINDOW; ++i) {
      recent_cpsbytes[i] = 0;
      recent_etime[i] = 0;
    }
    filesent++;
    if (verbose > 2) {
      if (remote_size)
        fprintf(stderr, "\r\t%ld of %ld (0%%)   ", local_size, remote_size);
      else
        fprintf(stderr, "\r\t%ld  ", local_size);
    }
    do {
      read_size = bufsiz;
      if (verbose > 2 && (local_size%bufsiz))	/* after restarting download */
        read_size = bufsiz - (local_size%bufsiz);
      if (read_size > remote_size-local_size)	/* at end of file */
        read_size = remote_size-local_size;
      if (buffer.size < read_size) {
	ret = (long)(read_into_buff(term, &buffer, read_size-buffer.size)); 
	if (ret < 0) break;
	if (ret > 0 && buffer.size < read_size) continue;
      }
      if (buffer.size == 0)
        break;
      else {
	ret = (long) write_from_buff(fd, &buffer, 0);
	if (ret <= 0 && termerrno) {
	  fprintf(stderr, "\tError writing to file. Aborting..\n");
	  break;
        }

	bytesrecvd += ret;
	total_bytesrecvd += ret;
	local_size += ret;
      }

      if ( (verbose > 2)) {

	/* Each CPS value is placed in a
	   CPS_WINDOW-sized buffer and the average of the buffer is
	   used as the CPS value.  */

	gettime(&nowtime);
	etime = download_delta_timeval(&nowtime, &lasttime);

        lasttime = nowtime;
	recent_cpsbytes[cps_count] = ret;
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

        /* Adjust bufsiz so we see about one report per second */
        while (bufsiz < cps/2 && local_size%(2*bufsiz) == 0)
          bufsiz *= 2;
        while (bufsiz > cps*2)
          bufsiz /= 2;

	if (remote_size) {
          long perc;

	  perc = (local_size*100) / remote_size;
	  if (cps)
	    fprintf(stderr, "\r\t%ld of %ld (%ld%%), current CPS %.0f. ETA: %.1f TT: %.1f   ",
	          local_size, remote_size, perc, 
	          cps, (remote_size-local_size)/cps, 
	          remote_size/cps);
	  else
	    fprintf(stderr, "\r\t%ld of %ld (%ld%%)   ", 
	          local_size, remote_size, perc);
	}
	else {
	  if (cps)
	    fprintf(stderr, "\r\t%ld, current CPS %.0f. ", local_size, cps );
	  else
	    fprintf(stderr, "\r\t%ld  ", local_size);
        }
      }
    } while (local_size < remote_size); 

    /* Reopen the term connection */
    if (term >= 0)
      x__close(term);
    if ((term = socket_connect_server(-1, term_server)) < 0) {
      fprintf(stderr,"Term: %s\n", command_result);
      exit(1);
    }

    /* give them cps ratings */
    if (verbose > 1) {
      gettime(&nowtime);
      etime = download_delta_timeval(&nowtime, &starttime);
      if (etime == 0)
        etime = 0.1;
     fprintf(stderr, "\r\t%ld bytes received in %.1f seconds;"
	     " CPS = %.0f               ",
	      bytesrecvd, etime, bytesrecvd/etime );
      total_etime += etime;
    }

    /* check the local file after send */
    if (fd == 1) {
      if (bytesrecvd == remote_size) {
	if (download_unlinkmode)
	  remote_unlink = file;
      } else {
        fprintf(stderr,
          "\n\tCount of bytes received is different from initial remote size!\n");
        j=1;
      }
    }
    else {
      int err = 0;
      x__close(fd);
      fd = -1;
      if (stat(local, &st)) {
        fprintf(stderr, "\n\tCan't stat local file after download!\n");
        err=j=1;
      }
      if (st.st_size != remote_size) {
        fprintf(stderr, "\n\tLocal file is a different size from initial "
	    "remote size!\n");
        err=j=1;
      }
      utb.actime = atime;
      utb.modtime = mtime;
      if (utime(local, &utb)) {
        fprintf(stderr, "\n\tCan't set correct times on file after download!\n");
        err=j=1;
      }
      if (perms != -1 && chmod(local, perms)) {
        fprintf(stderr, "\n\tCan't set permission on file after download!\n");
        err=j=1;
      }
    
      if (remote_term_version>=20164) {
	i = send_command(term, C_STAT, 0, "\n%ld\n%s", remote_size, file);
	fprintf(stderr, "CRC ");
	if (i >= 0) {
	  rcksum = 0;
	  sscanf(command_result,"%*d %*d %*d %*d %*d %*o %u", &rcksum);
	  if (rcksum) {
	    lcksum = file_crc(local, (long)st.st_size);
	    if ((lcksum) && (lcksum!=rcksum)) {
	      fprintf(stderr, "F\n\tRemote and local file checksums don't"
		      " match after upload!\n");
	      err=j=1;
	    } else {
	      fprintf(stderr, "OK\n");
	    }
	  } else {
	    fprintf(stderr, "?\n");
	  }
	}
      }
      if (!err && download_unlinkmode)
        remote_unlink = file;
    }

  /* delete the last file if it was transferred successfully... painful */
  if (remote_unlink) {
    if (send_command(term, C_UNLINK, 0, "%s", remote_unlink) < 0) {
      fprintf(stderr,"\tUnable to remove sent file\n");
      j=1;
    } else
      fprintf(stderr,"\tSent file removed.\n");
    remote_unlink = NULL;
  }
  }

  /* give them global cps rating */
  if ( (verbose > 1) && (filesent > 1) ) {
    gettime(&total_stoptime);
    etime = download_delta_timeval(&total_stoptime, &total_starttime);
    if (etime == 0)
      etime = 0.1;
    fprintf(stderr, "%ld total bytes received in %.1f seconds; overall CPS %.0f\n",
      total_bytesrecvd, etime, total_bytesrecvd/total_etime); 
  }

  exit(j);
}
