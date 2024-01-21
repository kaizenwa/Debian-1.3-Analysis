#ifndef _SLRN_NNTPLIB_H
#define _SLRN_NNTPLIB_H

typedef struct 
{
#define NNTP_RECONNECT_OK	0x1
   unsigned int flags;
   int init_state;
   int can_post;
#define NNTP_MAX_GROUP_NAME_LEN 80
#define NNTP_MAX_CMD_LEN 512
   char group_name [NNTP_MAX_GROUP_NAME_LEN + 1];
#define NNTP_MAX_HOST_LEN	80
   char host [NNTP_MAX_HOST_LEN + 1];
   int port;
   int code;
#define NNTP_RSPBUF_SIZE	512
   char rspbuf[NNTP_RSPBUF_SIZE];

   /* Capabilities-- if -1, probe server needs to be done */
   int can_xover;
   int can_xhdr;
   int can_xpat;
   int can_xgtitle;
   
   int (*auth_hook)(char *, char **, char **);

   SLTCP_Type *tcp;
}
NNTP_Type;

extern void nntp_disconnect_server (NNTP_Type *);
extern int nntp_check_connection (NNTP_Type *);
extern int nntp_reconnect_server (NNTP_Type *);

extern int nntp_server_cmd (NNTP_Type *, char *);
extern int nntp_server_vcmd (NNTP_Type *, char *, ...);
extern int nntp_write_server (NNTP_Type *, char *, unsigned int);
extern int nntp_fgets_server (NNTP_Type *, char *, unsigned int);
extern int nntp_fputs_server (NNTP_Type *, char *);
extern int nntp_gets_server (NNTP_Type *, char *, unsigned int);
extern int nntp_puts_server (NNTP_Type *, char *);
extern int nntp_get_server_response (NNTP_Type *);
extern int nntp_start_server_cmd (NNTP_Type *, char *);
extern int nntp_start_server_vcmd (NNTP_Type *, char *, ...);
extern int nntp_server_cmd (NNTP_Type *, char *);
extern int nntp_server_vcmd (NNTP_Type *, char *, ...);

extern char *nntp_get_server_name (void);

extern int nntp_close_server (NNTP_Type *);
extern NNTP_Type *nntp_open_server (char *, int);

extern int nntp_read_line (NNTP_Type *s, char *, unsigned int);
extern int nntp_discard_output (NNTP_Type *s);


extern int nntp_has_cmd (NNTP_Type *, char *);
extern int nntp_list_newsgroups (NNTP_Type *);
extern int nntp_list_active (NNTP_Type *);
extern int nntp_end_post (NNTP_Type *);
extern int nntp_post_cmd (NNTP_Type *);
extern int nntp_list_active_cmd (NNTP_Type *);

extern int nntp_select_group (NNTP_Type *, char *, int *, int *);
extern int nntp_xpat_cmd (NNTP_Type *, char *, int, int, char *);
extern int nntp_xgtitle_cmd (NNTP_Type *, char *);
extern int nntp_xhdr_cmd (NNTP_Type *, char *, int, char *, unsigned int);

extern int nntp_listgroup (NNTP_Type *, char *);
extern int nntp_head_cmd (NNTP_Type *, int, char *, int *);

extern int nntp_xover_cmd (NNTP_Type *, int, int);
extern int nntp_next_cmd (NNTP_Type *s, int *);
extern int nntp_body_cmd (NNTP_Type *s, int, char *);
extern int nntp_article_cmd (NNTP_Type *s, int, char *);

extern char *nntp_read_and_malloc (NNTP_Type *);

extern void (*NNTP_Connection_Lost_Hook) (NNTP_Type *);
extern int (*NNTP_Authorization_Hook) (char *, char **, char **);
extern FILE *NNTP_Debug_Fp;

#endif				       /* _SLRN_NNTPLIB_H */
