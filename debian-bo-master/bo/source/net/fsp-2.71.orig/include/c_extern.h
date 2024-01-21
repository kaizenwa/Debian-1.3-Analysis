#ifndef _FSP_C_EXTERN_H_
#define _FSP_C_EXTERN_H_

/* lib.c */
extern UBUF *client_interact PROTO0((unsigned int, unsigned long, unsigned int,
				     unsigned char *, unsigned int, 
				     unsigned char *));
extern void init_client PROTO0((char *, int, int));
extern int client_done PROTO0((void));

/* lock.c */
extern unsigned short client_get_key PROTO0((void));
extern void client_put_key PROTO0((unsigned short));
extern void client_init_key PROTO0((unsigned long, unsigned long,
				    unsigned short));

/* util.c */
extern char *util_abs_path PROTO0((char *));
extern char *util_getwd PROTO0((char *));
extern int util_download PROTO0((char *, FILE *, unsigned long));
extern int util_grab_file PROTO0((char *, FILE *, unsigned long));
extern int util_upload PROTO0((char *, FILE *));
extern void env_client PROTO0((void));
extern RDIR *util_opendir PROTO0((char *));
extern void util_closedir PROTO0((RDIR *));
extern rdirent *util_readdir PROTO0((RDIR *));
extern int util_stat PROTO0((char *, struct stat *));
extern int util_cd PROTO0((char *));
extern int util_cd2 PROTO0((char *));
extern void util_process_file PROTO0((char *, int, void (*)(), int (*)(),
				     void (*)(), int));

#endif /* _FSP_C_EXTERN_H_ */
