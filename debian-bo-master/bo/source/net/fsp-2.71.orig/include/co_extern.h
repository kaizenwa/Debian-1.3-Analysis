#ifndef _FSP_CO_EXTERN_H_
#define _FSP_CO_EXTERN_H_

#ifdef NEED_STRDUP
/* strdup.c */
extern char *strdup PROTO0((char *));
#endif

#ifdef NEED_RANDOM
/* random.c */
extern void srandom PROTO0((unsigned int));
extern char * initstate PROTO0((unsigned int, char *, int));
extern char *setstate PROTO0((char *));
extern long random PROTO0((void));
#endif

/* udp_io.c */
extern int _x_udp PROTO0((int *));
extern int _x_adr PROTO0((char *, int, struct sockaddr_in *));
extern int _x_select PROTO0((unsigned int *, long));

#endif /* _FSP_CO_EXTERN_H_ */
