
#ifndef PWDB_RADIUS_PUBLIC_H
#define PWDB_RADIUS_PUBLIC_H

#include <pwdb/radius.h>

/* RADIUS stuff */

/* useful defines */

#ifdef  BUFFER_SIZE
#undef  BUFFER_SIZE
#endif /* BUFFER_SIZE */
#define  BUFFER_SIZE   4096

#define _PWDB_STR_DATE_LENGTH	128
#define LONG_VAL_PTR(ptr) ((*(ptr)<<24)+(*((ptr)+1)<<16)+(*((ptr)+2)<<8)+(*((ptr)+3)))

typedef struct {
    char 		*hostname; 	/* the server name */
    char 		*secret;	/* password for server */
} RADIUS_SERVER;

typedef struct {
    unsigned int  length;
    void          *result;
} RADIUS_RESULT;

/* This is the core functions we export */
int rad_authenticate (RADIUS_SERVER,  /* RADIUS server entry */
		      const char*,      /* user name */
		      const char*,      /* password */
		      RADIUS_RESULT*    /* response */
		      );	  

int rad_change_passwd (RADIUS_SERVER, /* RADIUS server entry */
		       const char*,     /* user name */ 
		       const char*,     /* old password */
		       const char *,    /* new password */
		       RADIUS_RESULT*   /* response */
		       );	  

int radius_acct_send (RADIUS_SERVER server, /* RADIUS server entry */
                      const char* username, /* username */
		      int sense,            /* sense = PW_STATUS_START | PW_STATUS_STOP */
		      int session_time      /* session time - when stop */
		      );

#define radius_acct_start(server, user) \
        radius_acct_send((server), (user), PW_STATUS_START, 0)
#define radius_acct_stop(server, user, session_time) \
        radius_acct_send((server), (user), PW_STATUS_STOP, (session_time))

/* Return codes for the above function */
#define PWDB_RADIUS_SUCCESS	0	/* success */
#define PWDB_RADIUS_CONF_ERR	1	/* configuration file error */
#define PWDB_RADIUS_AUTH_FAIL	2	/* authetication failure */
#define PWDB_RADIUS_NET_FAIL	3	/* comm. with the RADIUS server failed */
#define PWDB_RADIUS_BAD_REQ    	4	/* bad request */
#define PWDB_RADIUS_RESOLV_ERR	5	/* resolver error on server hostname */
#define PWDB_RADIUS_LOCAL_ERR	6	/* local error - services file, etc. */
#define PWDB_RADIUS_SOCKET_ERR	7	/* socket creation/communication error */
#define PWDB_RADIUS_TIMEOUT     8       /* timeout communicating with the server */

/* Helper RADIUS functions */
const char *radstr_ust(u_int type);
const char *radstr_fp(u_int type);
const char *radstr_fr(u_int type);
const char *radstr_ls(u_int type); 
const char *radstr_ast(u_int type);
const char *radstr_aa(u_int type);
int get_server_entries (char *hostname, char *secret);

/* RADIUS code */
char *  ip_hostname( UINT4 );
UINT4   get_ipaddr( char * );
int     good_ipaddr( char * );
void    ipaddr2str( char *, UINT4 );
void    pairfree( VALUE_PAIR * );
UINT4   ipstr2long( char * );

/* Dictionary processing functions */
int dict_init(void);
DICT_ATTR *dict_attrget(int attribute);
DICT_ATTR  *dict_attrfind(char *attrname);
DICT_VALUE *dict_valfind(char *valname);
DICT_VALUE *dict_valget(UINT4 value, char *attrname);


/******************************************************************/

#endif /* PWDB_RADIUS_PUBLIC_H */
