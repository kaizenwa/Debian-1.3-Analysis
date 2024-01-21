/* Operations of the rpc manager */

/* Please note that the RPC manager does not use integers, it only uses */
/* 4-byte integers for the comunication */
enum {
    RPC_END,			/* End of RPC commands */
    RPC_INT,			/* Next argument is integer */
    RPC_STRING,			/* Next argument is a string */
    RPC_BLOCK			/* Next argument is a len/block */
};

int rpc_get (int sock, ...);
int rpc_send (int sock, ...);
void rpc_add_get_callback (int sock, void (*cback)(int));
int socket_read_block (int sock, char *dest, int len);
int socket_write_block (int sock, char *buffer, int len);
int send_string (int sock, char *string);
void tcp_init (void);
int open_tcp_link  (char *host, int *port, int *version, char *caller);
char *get_host_and_username (char *path, char **host, char **user, int *port,
			     int default_port, int default_to_anon, char **pass);

extern int tcp_inited;
extern int use_netrc;
extern int got_sigpipe;
