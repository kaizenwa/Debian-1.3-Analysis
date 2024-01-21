/* This is a routine that tests how fast the kernel will respond to the
 * select() delay.  
 */

#define I_IOCTL
#define I_SYS
#define I_STRING
#define I_STAT
#define I_TIME
#define I_LIMITS
#define I_SIGNAL
#define I_WAIT
#include "includes.h"

void sig_quit(int dummy) {
  exit(0);
}

void sig_ignore(int dummy) {
  signal(SIGALRM, sig_ignore);
  signal(SIGPIPE, sig_ignore);
}


double upload_delta_timeval(struct timeval *a, struct timeval *b)
{
  return (a->tv_sec + a->tv_usec/1000000.0) - 
         (b->tv_sec + b->tv_usec/1000000.0);
}

int freqtest(void) {
  struct timeval timeout, total_starttime, total_endtime;
  int pid = 0,i,j;
#ifdef USE_WAITPID
  int stat_loc;
#endif
  double time_diff = 10.0;
  float frequency[2];

  signal(SIGCHLD, sig_ignore);
  for (j = 0; j < 2; j++) {
    gettime(&total_starttime);
    for(i=0;i<100 || time_diff < 5.0;){
      ++i;
      timeout.tv_sec = 0;
      timeout.tv_usec = (unsigned long int) 1;
      select(0, NULL, NULL, NULL, &timeout); 
      gettime(&total_endtime);
      time_diff = upload_delta_timeval(&total_endtime,&total_starttime);
      if (time_diff > 30.0) break;
    }
    if (time_diff < 1.0/CLK_TCK) time_diff = 1.0/CLK_TCK;
    frequency[j] = (float) ((double) i)/time_diff;
    if (j) break;
    if ((pid = x__fork()) < 0) {
      x__perror("fork");
      exit(1);
    }else if (! pid) {
      signal(SIGCHLD, sig_quit);
      signal(SIGTERM, sig_quit);
      signal(SIGHUP, sig_quit);
      signal(SIGQUIT, sig_quit);
      signal(SIGTERM, sig_quit);
      for(i=0; 1; i++);
      exit(i);
    }
  }
  for(j=0;j < 2; j++)
    fprintf(stderr,"%s Frequency: %dHz (Measured) %dHz (Expected)\n",
      j ? "Highly Load" : "Normally Loaded", (int)(frequency[j]+0.5), CLK_TCK); 
#ifdef USE_WAITPID
  kill(pid,SIGTERM);
  waitpid((pid_t)-1, &stat_loc, WNOHANG);
#else
  wait3(0, WNOHANG, 0);
#endif /* SVR4 */
  return (int)
    (((frequency[0] < frequency[1]) ? frequency[0] : frequency[1]) +0.5);
}
 
