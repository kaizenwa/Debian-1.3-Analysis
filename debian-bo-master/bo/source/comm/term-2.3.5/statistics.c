#define I_STRING
#define I_SYS
#include "includes.h"


void do_stats(char *ret, int opt, struct Client *cl) {
  char buff[20];
  extern int stat_cooked_out, stat_uncomp_in, stat_uncomp_out,
  stat_cooked_in, stat_cooked_out;
  struct timeval t;

  int i;
  switch(opt) {
  case -10:
    if (term_remoteaddr != INADDR_ANY)
      sprintf(ret, "VERSION %lu %lx %s", remote_term_version, term_remoteaddr,
        term_remotehost);
    else
      sprintf(ret, "VERSION %lu %lx %s", remote_term_version, term_remoteaddr,
        "remotehost");
    break;
  case -9:
	gettime(&t);
	sprintf(ret, "%ld %ld", (long)t.tv_sec, (long)t.tv_usec);
	break;
  case -8:
    sprintf(ret, "%d %d", stat_cooked_in, stat_cooked_out);
    break;
  case -6:
    sprintf(ret, "%d", cl->number);
    break;
  case -5:
    sprintf(ret, "%u %d %lu", max_cps, window_size, packet_timeout);
    break;
  case -4:
    sprintf(ret, "%d %d", p_in_num, p_out_num);
    break;
  case -3:
    sprintf(ret,"%d %d %d %d", stat_modem_recv, stat_modem_ack, stat_modem_in, stat_modem_out);
    break;
  case -2:			/* Return the compression statistics. */
    sprintf(ret,"%d %d %d %d", stat_comp_in, stat_comp_out,
	    stat_uncomp_in, stat_uncomp_out);
    break;
  case -1:			/* Return a list of all the active clients.*/
    ret[0] = 0;
    for (i = 0; i < MAX_CLIENTS;++i) {
      if (clients[i].fd >= 0) {
	sprintf(buff,"%d %d ", i, clients[i].in_buff.size
			+ clients[i].out_buff.size);
	strcat(ret, buff);
      }
    }
    break;
  default:
    sprintf(ret, "%d %d %d %d %d %d %s", 
	    clients[opt].fd,
	    clients[opt].priority,
	    clients[opt].type,
	    clients[opt].state,
	    clients[opt].pid,
	    clients[opt].number, 
	    clients[opt].name);
    
  }
}
