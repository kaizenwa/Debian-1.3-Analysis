#define TEST
#undef TEST
#define IPCTST
#undef IPCTST
/* -------------------------------------------------------------------- */
/*        semshm.c                                                      */
/* -------------------------------------------------------------------- */
/*               int seminstall(key,amount)                             */
/*               int semrequest(semid,semnum)                           */
/*               int semrelease(semid,semnum)                           */
/* -------------------------------------------------------------------- */

#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include "share.h"

/*-----------------------------------------------------------------*/
int seminstall(key_t key, int amount)
{
  int           ret_val;
  int           semflag;

  semflag = IPC_CREAT | 0600;
#ifdef IPCTST
  fprintf(stderr,"seminstall: key: %d, #sems %d, flags %4x\n",
          key,amount,semflag);
#endif
  ret_val = semget(key,amount,semflag);
  if ( ret_val == -1 )
  {
    fprintf(stderr,"semget: (Key %x, #%d) failed: ",
            key,amount);
    perror("");
  }
  return ret_val;
}

/*-----------------------------------------------------------------*/
int semrequest(int semid, int semnum)
{
  struct sembuf sops[1];
  int    ret_val;

#ifdef IPCTST
  fprintf(stderr,"pid %d, ReQuest id:num %d:%d\n",getpid(),semid,semnum);
#endif
  sops[0].sem_op  = -1;
  sops[0].sem_num = semnum;
  sops[0].sem_flg = 0;

  do {
    errno = 0;
    ret_val = semop(semid,sops,1);
    if (ret_val == -1 && errno != EAGAIN)
      {
	fprintf(stderr,"Request Sema%d(%d) failed: ",semid,semnum);
	perror("");
      }
  } while (errno == EAGAIN);
  return(ret_val);
}

/*-----------------------------------------------------------------*/
int semrelease(int semid, int semnum)
{
  struct sembuf sops[1];
  int    ret_val;

#ifdef IPCTST
  fprintf(stderr,"%d RL %d:%d\n",getpid(),semid,semnum);
#endif
  sops[0].sem_op  = 1;
  sops[0].sem_num = semnum;
  sops[0].sem_flg = 0;
  ret_val = semop(semid,sops,1);
  if ( ret_val == -1 && errno != EAGAIN)
  {
    fprintf(stderr,"Release Sema%d(%d) failed: ",semid,semnum);
    perror("");
  }
  return(ret_val);
}


