#ifndef _RSPF_OUT_H_
#define _RSPF_OUT_H_

void send_rrh(char *iface);
int send_rspf(u_long saddr, u_long daddr, u_char *buf, int buflen,  char *iface);
void send_news(u_long saddr, u_long daddr, u_char sigbits, u_char cost, char *port);
void send_rspf_env(u_char *buf, int size, int node, char *iface);
void add_outfrag(int frag, int sync, u_char *startbuf, u_char *endbuf);
void send_full_bulletin(char *iface);

#endif /* _RSPF_OUT_H_ */