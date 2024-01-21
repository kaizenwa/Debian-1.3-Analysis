#ifndef _SLTCP_H_LOADED_
#define _SLTCP_H_LOADED_

#ifndef VMS

#include <stdarg.h>

typedef struct 
{
   int tcp_fd;
#define SLTCP_EOF_FLAG	0x1
   unsigned int tcp_flags;
   unsigned char *tcp_write_ptr;
   unsigned char *tcp_write_ptr_min;
   unsigned char *tcp_read_ptr_max;
   unsigned char *tcp_read_ptr;
#define SLTCP_BUF_SIZE 8192
   unsigned char tcp_read_buf [SLTCP_BUF_SIZE];
   unsigned char tcp_write_buf [SLTCP_BUF_SIZE];
   unsigned int bytes_out;
   unsigned int bytes_in;
}
SLTCP_Type;

extern int sltcp_close_socket (SLTCP_Type *);
extern SLTCP_Type *sltcp_open_connection (char *, int);
extern int sltcp_close (SLTCP_Type *);
extern unsigned int sltcp_read (SLTCP_Type *, char *, unsigned int);
extern unsigned int sltcp_write (SLTCP_Type *, char *, unsigned int);
extern int sltcp_flush_output (SLTCP_Type *);
extern int sltcp_map_service_to_port (char *);
extern int (*SLTCP_Interrupt_Hook) (void);

extern int sltcp_fputs (SLTCP_Type *, char *);
extern int sltcp_vfprintf (SLTCP_Type *, char *, va_list);
extern int sltcp_fgets (SLTCP_Type *, char *, unsigned int);

#endif				       /* NOT VMS */

#endif
