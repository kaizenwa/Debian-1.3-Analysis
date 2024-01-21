#ifndef _FSP_S_EXTERN_H_
#define _FSP_S_EXTERN_H_

/* conf.c */
extern void load_configuration PROTO0((void));

/* file.c */
extern char *check_path PROTO0((char *, int, PPATH *));
extern char *server_get_dir PROTO0((PPATH *, unsigned long, FILE **));
extern char *server_del_file PROTO0((PPATH *, unsigned long));
extern char *server_del_dir PROTO0((PPATH *, unsigned long));
extern char *server_make_dir PROTO0((PPATH *, unsigned long));
extern char *server_get_file PROTO0((PPATH *, FILE **, unsigned long,
				     unsigned short));
extern char *server_get_pro PROTO0((PPATH *, char *, unsigned long));
extern char *server_set_pro PROTO0((PPATH *, char *, unsigned long));
extern char *server_up_load PROTO0((char *, int, unsigned long, unsigned long,
				    unsigned short));
extern char *server_install PROTO0((PPATH *, unsigned long, unsigned short));
extern char *server_secure_file PROTO0((PPATH *, unsigned long,
					unsigned short));
extern char *server_grab_file PROTO0((PPATH *, FILE **, unsigned long,
				      unsigned short));
extern char *server_grab_done PROTO0((PPATH *, unsigned long, unsigned short));
extern void init_home_dir PROTO0((void));

/* filecache.c */
extern void clear_cache PROTO0((FPCACHE *, FPCACHE **));
extern FPCACHE *find_cache PROTO0((FPCACHE *, unsigned short, unsigned long,
				   char *));
extern void delete_cache PROTO0((FPCACHE *));
extern FPCACHE *add_cache PROTO0((FPCACHE *, FPCACHE *, unsigned short,
				  unsigned long, char *, FILE *));

/* host.c */
extern char *check_ip PROTO0((unsigned long));
extern IPrange *parse_ipline PROTO0((char *));
extern int dump_iptab PROTO0((void));
extern HTAB *find_host PROTO0((unsigned long));
extern int init_htab PROTO0((void));
extern int dump_htab PROTO0((void));

/* lib.c */
extern int server_loop PROTO0((unsigned long));
extern unsigned long get_next_key PROTO0((void));
extern int server_reply PROTO0((struct sockaddr_in *, UBUF *, int, int));
extern int send_error PROTO0((struct sockaddr_in *, UBUF *, char *));
extern void send_file PROTO0((struct sockaddr_in *, UBUF *, FILE *, int,
			      char *));
extern void init_network PROTO0((int));
extern void init_inetd PROTO0((void));

/* main.c */
extern void server_get_packet PROTO0((int, UBUF *, int, HTAB *,
				      struct sockaddr_in *));

#endif /* _FSP_S_EXTERN_H_ */
