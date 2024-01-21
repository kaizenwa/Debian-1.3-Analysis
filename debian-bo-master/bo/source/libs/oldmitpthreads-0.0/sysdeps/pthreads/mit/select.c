/* ==== select.c ============================================================
 * Copyright (c) 1994 by Chris Provenzano, proven@mit.edu
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *  This product includes software developed by Chris Provenzano.
 * 4. The name of Chris Provenzano may not be used to endorse or promote 
 *	  products derived from this software without specific prior written
 *	  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRIS PROVENZANO ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL CHRIS PROVENZANO BE LIABLE FOR ANY 
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
 * SUCH DAMAGE.
 *
 * This code based on code contributed by 
 * Peter Hofmann <peterh@prz.tu-berlin.d400.de>
 *
 * Description : Select.
 *
 *  1.23 94/04/26 proven
 *      -Started coding this file.
 */

#ifndef lint
static const char rcsid[] = "$Id: select.c,v 1.5 1996/10/04 22:53:37 hjl Exp $";
#endif

#include <string.h>
#include <errno.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/time.h>

extern struct   pthread_queue fd_wait_select;

extern int    machdep_sys_select     (int, fd_set *, fd_set *, fd_set *,
                                      struct timeval * tv);
extern void   pthread_resched_resume (enum pthread_state);
extern void   pthread_sched_resume   (void);

/*
 * These don't seem to be implemented. vch
 */
extern int    sleep_cancel           (struct pthread * );

/*
 * Prototype
 */
int           select                 (int, fd_set *, fd_set *, fd_set *,
                                      struct timeval *);

/* ==========================================================================
 * select() 
 */
int select(int numfds, fd_set *readfds, fd_set *writefds,
           fd_set *exceptfds, struct timeval *timeout)
{
	fd_set real_exceptfds, real_readfds, real_writefds; /* mapped fd_sets */
	fd_set * real_readfds_p, * real_writefds_p, * real_exceptfds_p;
	fd_set read_locks, write_locks, rdwr_locks;
	struct timespec timeout_time, current_time;
	struct timeval zero_timeout = { 0, 0 };
	int i, j, ret = 0, got_all_locks = 1;
	struct pthread_select_data data;

	if (numfds > dtablesize) {
		numfds = dtablesize;
	}

	data.nfds = 0;
	FD_ZERO(&data.readfds);
	FD_ZERO(&data.writefds);
	FD_ZERO(&data.exceptfds);

	/* Do this first */
	if (timeout) {
		machdep_gettimeofday(&current_time);
		timeout_time.tv_sec = current_time.tv_sec + timeout->tv_sec;
		if ((timeout_time.tv_nsec = current_time.tv_nsec + 
		  (timeout->tv_usec * 1000)) > 1000000000) {
			timeout_time.tv_nsec -= 1000000000;
			timeout_time.tv_sec++;
		}
	}

	FD_ZERO(&read_locks);
	FD_ZERO(&write_locks);
	FD_ZERO(&rdwr_locks);
	FD_ZERO(&real_readfds);
	FD_ZERO(&real_writefds);
	FD_ZERO(&real_exceptfds);

	/* lock readfds */
	if (readfds || writefds || exceptfds) {
  		for (i = 0; i < numfds; i++) {
   			if ((readfds && (FD_ISSET(i, readfds))) || 
			  (exceptfds && FD_ISSET(i, exceptfds))) {
				if (writefds && FD_ISSET(i ,writefds)) {
					if ((ret = fd_lock(i, FD_RDWR, NULL)) != OK) {
          				got_all_locks = 0;
          				break;
        			}
        			FD_SET(i, &rdwr_locks);
      				FD_SET(fd_table[i]->fd.i,&real_writefds);
      			} else {
        			if ((ret = fd_lock(i, FD_READ, NULL)) != OK) {
          				got_all_locks = 0;
          				break;
        			}
        			FD_SET(i, &read_locks);
      			}
      			if (readfds && FD_ISSET(i,readfds)) {
        			FD_SET(fd_table[i]->fd.i, &real_readfds);
				}
      			if (exceptfds && FD_ISSET(i,exceptfds)) {
        			FD_SET(fd_table[i]->fd.i, &real_exceptfds);
				}
				if (fd_table[i]->fd.i >= data.nfds) {
					data.nfds = fd_table[i]->fd.i + 1;
				}
			} else {
   				if (writefds && FD_ISSET(i, writefds)) {
        			if ((ret = fd_lock(i, FD_WRITE, NULL)) != OK) {
						got_all_locks = 0;
          				break;
        			}
        			FD_SET(i, &write_locks);
      				FD_SET(fd_table[i]->fd.i,&real_writefds);
				}
				if (fd_table[i]->fd.i >= data.nfds) {
					data.nfds = fd_table[i]->fd.i + 1;
				}
			}
		}
	}

	if (got_all_locks) {

		pthread_sched_prevent();

  		memcpy(&data.readfds,&real_readfds,sizeof(fd_set));
  		memcpy(&data.writefds,&real_writefds,sizeof(fd_set));
  		memcpy(&data.exceptfds,&real_exceptfds,sizeof(fd_set));

  		real_readfds_p = (readfds == NULL) ? NULL : &real_readfds;
  		real_writefds_p = (writefds == NULL) ? NULL : &real_writefds;
  		real_exceptfds_p = (exceptfds == NULL) ? NULL : &real_exceptfds;

		if ((ret = machdep_sys_select(data.nfds, real_readfds_p, 
		  real_writefds_p, real_exceptfds_p, &zero_timeout)) == OK) {

  			real_exceptfds_p = (exceptfds == NULL) ? NULL : &data.exceptfds;
  			real_writefds_p = (writefds == NULL) ? NULL : &data.writefds;
  			real_readfds_p = (readfds == NULL) ? NULL : &data.readfds;

			pthread_queue_enq(&fd_wait_select, pthread_run);
			pthread_run->data.select_data = &data;
		    SET_PF_WAIT_EVENT(pthread_run);

			if (timeout) {
				machdep_gettimeofday(&current_time);
				sleep_schedule(&current_time, &timeout_time);
	
				pthread_resched_resume(PS_SELECT_WAIT);

				/* We're awake */
				pthread_sched_prevent();
				CLEAR_PF_DONE_EVENT(pthread_run);

				if (sleep_cancel(pthread_run) == NOTOK) {
					SET_ERRNO(ETIMEDOUT);
					ret = 0;
				} else {
					ret = data.nfds;
				}
				pthread_sched_resume();
			} else {
				pthread_resched_resume(PS_SELECT_WAIT);
				CLEAR_PF_DONE_EVENT(pthread_run);
				ret = data.nfds; /* XXX ??? snl */
			}
		} else if (ret < 0) {
			pthread_sched_resume();
			SET_ERRNO(-ret);
			ret = NOTOK;
		} else {
			pthread_sched_resume();
		}
	}

	/* clean up the locks */
	for (i = 0; i < numfds; i++)
		if (FD_ISSET(i,&read_locks)) fd_unlock(i,FD_READ);
	for (i = 0; i < numfds; i++)
		if (FD_ISSET(i,&rdwr_locks)) fd_unlock(i,FD_RDWR);
	for (i = 0; i < numfds; i++)
		if (FD_ISSET(i,&write_locks)) fd_unlock(i,FD_WRITE);

	if (ret > 0) {
  		if (readfds != NULL) {
    		for (i = 0; i < numfds; i++) {
				if (! (FD_ISSET(i,readfds) &&
				  FD_ISSET(fd_table[i]->fd.i,real_readfds_p)))
				FD_CLR(i,readfds);
			}
		}
		if (writefds != NULL) {
			for (i = 0; i < numfds; i++)
				if (! (FD_ISSET(i,writefds) &&
				FD_ISSET(fd_table[i]->fd.i,real_writefds_p)))
				FD_CLR(i,writefds);
		}
		if (exceptfds != NULL) {
			for (i = 0; i < numfds; i++)
				if (! (FD_ISSET(i,exceptfds) &&
				FD_ISSET(fd_table[i]->fd.i,real_exceptfds_p)))
				FD_CLR(i,exceptfds);
		}
	} else {
		if (exceptfds != NULL) FD_ZERO(exceptfds);
		if (writefds != NULL) FD_ZERO(writefds);
		if (readfds != NULL) FD_ZERO(readfds);
	}

	return(ret);
}

#if defined(__ELF__) || defined(__GNU_LIBRARY__)
#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (select, __select);
#endif
#endif
