#ifndef _SLTCP_H_LOADED_
#define _SLTCP_H_LOADED_

typedef struct 
{
   int tcp_fd;
}
SLTCP_Type;

extern int sltcp_fputs (SLTCP_Type *, char *);
extern int sltcp_vfprintf (SLTCP_Type *, char *, va_list);
extern int sltcp_fgets (SLTCP_Type *, char *, unsigned int);
extern unsigned int sltcp_write (SLTCP_Type *, char *, unsigned int);
extern int sltcp_flush_output (SLTCP_Type *);
extern int sltcp_close (SLTCP_Type *);
extern unsigned int sltcp_read (SLTCP_Type *, char *, unsigned int);
extern unsigned int sltcp_write (SLTCP_Type *, char *, unsigned int);
extern SLTCP_Type *sltcp_open_connection (char *, int);
extern int sltcp_map_service_to_port (char *);
extern int sltcp_close_socket (SLTCP_Type *);
extern int (*SLTCP_Interrupt_Hook) (void);

#endif
