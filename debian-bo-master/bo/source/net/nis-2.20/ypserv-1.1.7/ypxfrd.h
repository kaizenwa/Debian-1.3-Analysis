/*
** $Id: ypxfrd.h,v 1.1 1996/08/19 15:42:39 kukuk Exp $
*/
#ifndef _YPXFRD_H_RPCGEN
#define _YPXFRD_H_RPCGEN

#include <rpc/rpc.h>

#define _YPMAXRECORD 1024
#define _YPMAXDOMAIN 64
#define _YPMAXMAP 64
#define _YPMAXPEER 64
#define YPXFRBLOCK 32767

enum xfrstat {
	XFR_REQUEST_OK = 1,
	XFR_DENIED = 2,
	XFR_NOFILE = 3,
	XFR_ACCESS = 4,
	XFR_BADDB = 5,
	XFR_READ_OK = 6,
	XFR_READ_ERR = 7,
	XFR_DONE = 8,
	XFR_DB_ENDIAN_MISMATCH = 9,
	XFR_DB_TYPE_MISMATCH = 10
};

typedef enum xfrstat xfrstat;
#ifdef __cplusplus 
extern "C" bool_t xdr_xfrstat(XDR *, xfrstat*);
#elif __STDC__ 
extern  bool_t xdr_xfrstat(XDR *, xfrstat*);
#else /* Old Style C */ 
bool_t xdr_xfrstat();
#endif /* Old Style C */ 

enum xfr_db_type {
	XFR_DB_ASCII = 1,
	XFR_DB_BSD_HASH = 2,
	XFR_DB_BSD_BTREE = 3,
	XFR_DB_BSD_RECNO = 4,
	XFR_DB_BSD_MPOOL = 5,
	XFR_DB_BSD_NDBM = 6,
	XFR_DB_GNU_GDBM = 7,
	XFR_DB_DBM = 8,
	XFR_DB_NDBM = 9,
	XFR_DB_OPAQUE = 10,
	XFR_DB_ANY = 11,
	XFR_DB_UNKNOWN = 12
};

typedef enum xfr_db_type xfr_db_type;
#ifdef __cplusplus 
extern "C" bool_t xdr_xfr_db_type(XDR *, xfr_db_type*);
#elif __STDC__ 
extern  bool_t xdr_xfr_db_type(XDR *, xfr_db_type*);
#else /* Old Style C */ 
bool_t xdr_xfr_db_type();
#endif /* Old Style C */ 

enum xfr_byte_order {
	XFR_ENDIAN_BIG = 1,
	XFR_ENDIAN_LITTLE = 2,
	XFR_ENDIAN_ANY = 3
};

typedef enum xfr_byte_order xfr_byte_order;
#ifdef __cplusplus 
extern "C" bool_t xdr_xfr_byte_order(XDR *, xfr_byte_order*);
#elif __STDC__ 
extern  bool_t xdr_xfr_byte_order(XDR *, xfr_byte_order*);
#else /* Old Style C */ 
bool_t xdr_xfr_byte_order();
#endif /* Old Style C */ 

typedef char *xfrdomain;
#ifdef __cplusplus 
extern "C" bool_t xdr_xfrdomain(XDR *, xfrdomain*);
#elif __STDC__ 
extern  bool_t xdr_xfrdomain(XDR *, xfrdomain*);
#else /* Old Style C */ 
bool_t xdr_xfrdomain();
#endif /* Old Style C */ 

typedef char *xfrmap;
#ifdef __cplusplus 
extern "C" bool_t xdr_xfrmap(XDR *, xfrmap*);
#elif __STDC__ 
extern  bool_t xdr_xfrmap(XDR *, xfrmap*);
#else /* Old Style C */ 
bool_t xdr_xfrmap();
#endif /* Old Style C */ 

typedef char *xfrmap_filename;
#ifdef __cplusplus 
extern "C" bool_t xdr_xfrmap_filename(XDR *, xfrmap_filename*);
#elif __STDC__ 
extern  bool_t xdr_xfrmap_filename(XDR *, xfrmap_filename*);
#else /* Old Style C */ 
bool_t xdr_xfrmap_filename();
#endif /* Old Style C */ 

#ifdef __cplusplus 
extern "C" bool_t xdr_xfrstat(XDR *, xfrstat*);
#elif __STDC__ 
extern  bool_t xdr_xfrstat(XDR *, xfrstat*);
#else /* Old Style C */ 
bool_t xdr_xfrstat();
#endif /* Old Style C */ 

#ifdef __cplusplus 
extern "C" bool_t xdr_xfr_db_type(XDR *, xfr_db_type*);
#elif __STDC__ 
extern  bool_t xdr_xfr_db_type(XDR *, xfr_db_type*);
#else /* Old Style C */ 
bool_t xdr_xfr_db_type();
#endif /* Old Style C */ 

#ifdef __cplusplus 
extern "C" bool_t xdr_xfr_byte_order(XDR *, xfr_byte_order*);
#elif __STDC__ 
extern  bool_t xdr_xfr_byte_order(XDR *, xfr_byte_order*);
#else /* Old Style C */ 
bool_t xdr_xfr_byte_order();
#endif /* Old Style C */ 

struct ypxfr_mapname {
	xfrmap xfrmap;
	xfrdomain xfrdomain;
	xfrmap_filename xfrmap_filename;
	xfr_db_type xfr_db_type;
	xfr_byte_order xfr_byte_order;
};

typedef struct ypxfr_mapname ypxfr_mapname;
#ifdef __cplusplus 
extern "C" bool_t xdr_ypxfr_mapname(XDR *, ypxfr_mapname*);
#elif __STDC__ 
extern  bool_t xdr_ypxfr_mapname(XDR *, ypxfr_mapname*);
#else /* Old Style C */ 
bool_t xdr_ypxfr_mapname();
#endif /* Old Style C */ 

struct xfr {
	bool_t ok;
	union {
		struct {
			u_int xfrblock_buf_len;
			char *xfrblock_buf_val;
		} xfrblock_buf;
		xfrstat xfrstat;
	} xfr_u;
};

typedef struct xfr xfr;
#ifdef __cplusplus 
extern "C" bool_t xdr_xfr(XDR *, xfr*);
#elif __STDC__ 
extern  bool_t xdr_xfr(XDR *, xfr*);
#else /* Old Style C */ 
bool_t xdr_xfr();
#endif /* Old Style C */ 

#define YPXFRD_FREEBSD_PROG ((u_long)600100069)
#define YPXFRD_FREEBSD_VERS ((u_long)1)

#ifdef __cplusplus
#define YPXFRD_GETMAP ((u_long)1)
extern "C" struct xfr * ypxfrd_getmap_1(ypxfr_mapname *, CLIENT *);
extern "C" struct xfr * ypxfrd_getmap_1_svc(ypxfr_mapname *, struct svc_req *);

#elif __STDC__
#define YPXFRD_GETMAP ((u_long)1)
extern  struct xfr * ypxfrd_getmap_1(ypxfr_mapname *, CLIENT *);
extern  struct xfr * ypxfrd_getmap_1_svc(ypxfr_mapname *, struct svc_req *);

#else /* Old Style C */ 
#define YPXFRD_GETMAP ((u_long)1)
extern  struct xfr * ypxfrd_getmap_1();
extern  struct xfr * ypxfrd_getmap_1_svc();
#endif /* Old Style C */ 

#endif /* !_YPXFRD_H_RPCGEN */
