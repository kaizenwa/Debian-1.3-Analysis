
/* This is the RADIUS main support file */

#include "../_pwdb_internal.h"
#include "../_pwdb_macros.h"
#include "md5.h"

#define MAXPWNAM 20
   /* maximum user name length. Server dependent    */
   /* this is the default value                     */
#define MAXPASS 16
   /* max password length. Again, depends on server */
   /* compiled in. This is the default.             */

#define LONG_VAL_PTR(ptr) ((*(ptr)<<24)+(*((ptr)+1)<<16)+(*((ptr)+2)<<8)+(*((ptr)+3)))

static u_char recv_buffer[BUFFER_SIZE];
   /* buffer for receiving the response             */
   /* from the RADIUS server                        */
static char send_buffer[BUFFER_SIZE];
   /* buffer to build the query for                 */
   /* the RADIUS server                             */
static char vector[AUTH_VECTOR_LEN];

/* alarm and timeout stuff */
#define TIMEOUT     30 /* timeout the request if not hear 
			* back from the radius server in this
			* many seconds */
#define FAIL_RETRY_TIME 4 /* how long to wait to send the second 
			   * request */

#include <signal.h>
#include <setjmp.h>

jmp_buf env;
static int alarmbackoff;

/* just to shut off the compiler warning... */
void auth_timeout(int sig);

void auth_timeout(int sig)
{
    signal(SIGALRM, auth_timeout);
    alarmbackoff = alarmbackoff * 2;
    D(("SIGALRM handler"));
    /* double the alarmbackoff and try again... */
    alarm(alarmbackoff);
    longjmp(env,1);
}

/*************************************************************
 * Build a random vector for MD5 hashing of the sensitive data
 *************************************************************/
static void random_vector(char *vect) 
{
    int    randno;
    int    i;

    srand(time(0));
    for(i = 0;i < AUTH_VECTOR_LEN;) {
        randno = rand();
        memcpy(vect, &randno, sizeof(int));
        vect += sizeof(int);
        i += sizeof(int);
    }
}

/*****************************************************************
 * This is the function called to encrypt sensitive data, to avoid
 * being transmitted in clear over untrusted networks
 *****************************************************************/
static void md5_calc(unsigned char *output, char *input, unsigned int inlen) {
    MD5_CTX context;

    MD5Init(&context);
    MD5Update(&context, (unsigned char*)input, inlen);
    MD5Final(output, &context);
}

/********************************************************************
 * radius_get_random_id
 * builds a random ID for use with the auth packet id
 ********************************************************************/
static u_char radius_get_random_id(void)
{
    /*
     * this should be True Random(tm).
     * If anyone have a better idea to avoid sending duplicate IDs
     * for about 30 secs to the RADIUS server, be my guest. For now
     * this will do it... 
     * CG
     */
    int i;
    u_char i1 = 0;
    u_char i2 = 0;
    u_char i3 = 0;
        
    srand(time(0));
    /* 
     * I've seen that calling rand() multiple times improves this
     * thing a little bit... CG
     */
    for(i=0;i< 1000; i++) {
        i1+=rand();
        i2+=2*rand();
        i3+=3*rand();
    }
    return (u_char) (getpid()+i1+i2+i3);
}
/*********************************************************************************/
/* Obtain a free local port for binding and communicating with the RADIUS server */
/* Not quite optimal, but... it works.                                           */
/*********************************************************************************/
static u_short radius_get_local_port(int socket_fd)
{
    struct sockaddr salocal;
    struct sockaddr_in *s_in;
    u_short local_port;
    
    s_in = (struct sockaddr_in *) & salocal;
    memset ((char *) s_in, '\0', sizeof (salocal));
    s_in->sin_family = AF_INET;
    s_in->sin_addr.s_addr = INADDR_ANY;
    /* avoid long loops... CG */
    local_port = (int) getppid();
    local_port += 1024;
    do {
        local_port++;
        s_in->sin_port = htons((u_short)local_port);
    } while ((bind(socket_fd, &salocal, sizeof (struct sockaddr_in)) < 0) && 
             (local_port < 64000));
    if (local_port >= 64000) {
        close(socket_fd);
        return 0;
    }
    return local_port;
}
 
/********************************************************************/
/* rad_authenticate                                                 */
/* Authenticate user on radius server                               */
/********************************************************************/
int rad_authenticate (RADIUS_SERVER server, 
		      const char* username,
		      const char* password,
		      RADIUS_RESULT *rad_result)
{
    int                salen;
    int                sockfd;
    struct sockaddr    saremote;
    struct sockaddr_in *s_in;
    struct servent     *svp;
    short              svc_port;
    AUTH_HDR           *auth;
    char               passbuf[AUTH_PASS_LEN];
    char               md5buf[256];
    UINT4              auth_ipaddr;
    u_short            local_port;
    static int         total_length;
    u_char             *ptr;
    int                length,
	               secretlen;
    int                i;
    static int         result;
    void               (*old_sigalrm)(int);

    D(("called"));
    /* sanity checks */
    if (!server.hostname || !server.secret || !rad_result)
        return PWDB_RADIUS_BAD_REQ;

    rad_result->length = -1;
    rad_result->result = NULL;
    if (!strlen(password) || !strlen(username))
        return PWDB_RADIUS_BAD_REQ;

    svp = getservbyname ("radius", "udp");
    if (svp == (struct servent *) 0) {
        return PWDB_RADIUS_LOCAL_ERR;
    }

    svc_port = ntohs((u_short) svp->s_port);

    /* Get the IP address of the authentication server */
    if ((auth_ipaddr = get_ipaddr(server.hostname)) == (UINT4)0) {
        return PWDB_RADIUS_RESOLV_ERR;
    }

    sockfd = socket (AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        return PWDB_RADIUS_SOCKET_ERR;
    }

    local_port=radius_get_local_port(sockfd);
    if (local_port == 0) {
        close(sockfd);
        return PWDB_RADIUS_SOCKET_ERR;
    }
        
        
    /******************************************************************* 
     * About auth packet:
     * - in auth->length we store the size of the auth->data
     * - auth->data is a list of auth tokens with the following
     *     structure:
     *     - a byte with the resource ID (PW_USER_NAME, PW_PASSWORD, etc);
     *     - a byte with the length of this resource + 2;
     *     - the resource data
     *******************************************************************/
    /* Build authenticaton request */
    random_vector(vector);

    auth = (AUTH_HDR *)send_buffer;
    auth->code = PW_AUTHENTICATION_REQUEST;
    auth->id = radius_get_random_id();
    memcpy(auth->vector, vector, AUTH_VECTOR_LEN);
    total_length = AUTH_HDR_LEN;
    ptr = auth->data;

    /***** USER NAME *****/
    *ptr++ = PW_USER_NAME;
    length = strlen(username);
    if (length > MAXPWNAM)
        length = MAXPWNAM;
    *ptr++ = length + 2;
    memcpy (ptr, username, length);
    ptr += length;
    total_length += length + 2;

    /***** PASSWORD *****/
    *ptr++ = PW_PASSWORD;
    *ptr++ = AUTH_PASS_LEN + 2;
    /* Encrypt the Password */
    length = strlen (password);
    if (length > MAXPASS)
        length = MAXPASS;
    memset(passbuf, 0, AUTH_PASS_LEN);
    memcpy(passbuf, password, length);
    /* Calculate the MD5 Digest */
    secretlen = strlen (server.secret);
    strcpy (md5buf, server.secret);
    memcpy (md5buf + secretlen, auth->vector, AUTH_VECTOR_LEN);
    md5_calc(ptr, md5buf, secretlen + AUTH_VECTOR_LEN);
    /* Xor the password into the MD5 digest */
    for (i=0; i < AUTH_PASS_LEN; i++)
        *ptr++ ^= passbuf[i];
    total_length += AUTH_PASS_LEN + 2;

    {
        long hostid;
        char * c_hostid;

        hostid = gethostid();
        c_hostid = (char *)&hostid;

        *ptr++ = PW_CLIENT_ID;
        *ptr++ = 6;
        *ptr++ = c_hostid[2];
        *ptr++ = c_hostid[3];
        *ptr++ = c_hostid[0];
        *ptr++ = c_hostid[1];
        total_length += 6;
    }

    *ptr++ = PW_CLIENT_PORT_ID;
    *ptr++ = 6;
    *ptr++ = 0;
    *ptr++ = 0;
    *(short *)ptr = htons(local_port);
    ptr += 2;
    total_length += 6;

    auth->length = htons (total_length);
    s_in = (struct sockaddr_in *) & saremote;
    memset ((char *) s_in, '\0', sizeof (saremote));
    s_in->sin_family = AF_INET;
    s_in->sin_addr.s_addr = htonl(auth_ipaddr);
    s_in->sin_port = htons(svc_port);

    result = 0;
    /* send the second request after FAIL_RETRY_TIME secs, then repeat
     * at 2* secs, then 4* secs, ... while < TIMEOUT */
    alarmbackoff = FAIL_RETRY_TIME;
    old_sigalrm = signal(SIGALRM, auth_timeout);
    alarm(alarmbackoff);
    setjmp(env);
    while ((result < 1) && (alarmbackoff < TIMEOUT)) {
	D(("sending auth request to radius server, backoff %d seconds",
	   alarmbackoff));
	sendto (sockfd, (char *) auth, (int) total_length,
		(int) 0, &saremote, sizeof(struct sockaddr_in));
	salen = sizeof (saremote);
	result = recvfrom (sockfd, (char *)recv_buffer, sizeof(recv_buffer),
			   (int)0, &saremote, &salen);
    }
    alarm(0);
    signal(SIGALRM, old_sigalrm);
    close (sockfd);
    /* did our request timed out ? */
    if ((alarmbackoff >= TIMEOUT) && (result < 1)) {
	/* timed out, nothing received */
	rad_result->length = -1;
	rad_result->result = NULL;
	return PWDB_RADIUS_TIMEOUT;
    }
    /* else - not timeout, something received */
    if (result >= AUTH_HDR_LEN) { /* valid response ? */
	AUTH_HDR *recv_auth;
	int recv_len;

	recv_auth = (AUTH_HDR *)recv_buffer;
	recv_len = ntohs(recv_auth->length) - AUTH_HDR_LEN;
	if (recv_auth->code == PW_AUTHENTICATION_ACK) {
	    rad_result->length = recv_len;
	    rad_result->result = (void *)recv_auth->data;
	    return PWDB_RADIUS_SUCCESS;
	} 
    }
    /* invalid response */
    rad_result->length = -1;
    rad_result->result = NULL;
    return PWDB_RADIUS_AUTH_FAIL;
}

/********************************************************************
 * rad_change_passwd
 * Change a user password from a RADIUS server
 *
 *******************************************************************/
int rad_change_passwd (RADIUS_SERVER server, 
                       const char* username,
                       const char* old_password,
                       const char* new_password,
		       RADIUS_RESULT *rad_result)
{
    int salen;
    int sockfd;
    struct sockaddr saremote;
    struct sockaddr_in *s_in;
    struct servent *svp;
    short svc_port;
    AUTH_HDR *auth;
    char passbuf[AUTH_PASS_LEN];
    char md5buf[256];
    UINT4 auth_ipaddr;
    u_short local_port;
    static int total_length;
    u_char *ptr, *oldvector;
    int length, secretlen, i;
    static int result;
    void (*old_sigalrm)(int);
    
    /* sanity checks */
    if (!server.hostname || !server.secret || !rad_result)
        return PWDB_RADIUS_BAD_REQ;        

    rad_result->length = -1;
    rad_result->result = NULL;
    if ((*old_password == '\0') || (*new_password == '\0')) {
        return PWDB_RADIUS_BAD_REQ;
    }

    svp = getservbyname ("radius", "udp");
    if (svp == (struct servent *) 0) {
        return PWDB_RADIUS_LOCAL_ERR;
    }

    svc_port = ntohs((u_short) svp->s_port);

    /* Get the IP address of the authentication server */
    if ((auth_ipaddr = get_ipaddr(server.hostname)) == (UINT4)0) {
        return PWDB_RADIUS_RESOLV_ERR;
    }

    sockfd = socket (AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        return PWDB_RADIUS_SOCKET_ERR;
    }

    local_port=radius_get_local_port(sockfd);
    if (local_port == 0) {
        close(sockfd);
        return PWDB_RADIUS_SOCKET_ERR;
    }
        
        
    /******************************************************************* 
     * About auth packet:
     * - in auth->length we store the size of the auth->data
     * - auth->data is a list of auth tokens with the following
     *     structure:
     *     - a byte with the resource ID (PW_USER_NAME, PW_PASSWORD, etc);
     *     - a byte with the length of this resource + 2;
     *     - the resource data
     *******************************************************************/
    /* Build authenticaton request */
    random_vector(vector);

    auth = (AUTH_HDR *)send_buffer;
    auth->code = PW_PASSWORD_REQUEST;
    auth->id = radius_get_random_id();
    memcpy(auth->vector, vector, AUTH_VECTOR_LEN);
    total_length = AUTH_HDR_LEN;
    ptr = auth->data;

    /***** USER NAME *****/
    *ptr++ = PW_USER_NAME;
    length = strlen(username);
    if (length > MAXPWNAM)
        length = MAXPWNAM;
    *ptr++ = length + 2;
    memcpy (ptr, username, length);
    ptr += length;
    total_length += length + 2;

    /***** PASSWORDS *****/

    /* Pass on the new password */
    *ptr++ = PW_PASSWORD;
    *ptr++ = AUTH_PASS_LEN + 2;
    length = strlen (new_password);
    if (length > MAXPASS)
        length = MAXPASS;
    memset(passbuf, 0, AUTH_PASS_LEN);
    memcpy(passbuf, new_password, length);
    /* Calculate the MD5 Digest */
    secretlen = strlen (old_password);
    strcpy (md5buf, old_password);
    memcpy (md5buf + secretlen, auth->vector, AUTH_VECTOR_LEN);
    md5_calc(ptr, md5buf, secretlen + AUTH_VECTOR_LEN);
    /* save the md5 calc pointer for later use with old_password */
    oldvector = ptr;
    /* Xor the password into the MD5 digest */
    for (i=0; i < AUTH_PASS_LEN; i++)
        *ptr++ ^= passbuf[i];
    total_length += AUTH_PASS_LEN + 2;

    /* Pass on the old password */
    *ptr++ = PW_OLD_PASSWORD;
    *ptr++ = AUTH_PASS_LEN + 2;
    length = strlen (old_password);
    if (length > MAXPASS)
        length = MAXPASS;
    memset(passbuf, 0, AUTH_PASS_LEN);
    memcpy(passbuf, old_password, length);
    /* Calculate the MD5 Digest */
    secretlen = strlen (old_password);
    strcpy (md5buf, old_password);
    memcpy (md5buf + secretlen, oldvector, AUTH_VECTOR_LEN);
    md5_calc(ptr, md5buf, secretlen + AUTH_VECTOR_LEN);
    /* Xor the password into the MD5 digest */
    for (i=0; i < AUTH_PASS_LEN; i++)
        *ptr++ ^= passbuf[i];
    total_length += AUTH_PASS_LEN + 2;


    {
        long hostid;
        char * c_hostid;

        hostid = gethostid();
        c_hostid = (char *)&hostid;

        *ptr++ = PW_CLIENT_ID;
        *ptr++ = 6;
        *ptr++ = c_hostid[2];
        *ptr++ = c_hostid[3];
        *ptr++ = c_hostid[0];
        *ptr++ = c_hostid[1];
        total_length += 6;
    }

    *ptr++ = PW_CLIENT_PORT_ID;
    *ptr++ = 6;
    *ptr++ = 0;
    *ptr++ = 0;
    *(short *)ptr = htons(local_port);
    ptr += 2;
    total_length += 6;    

    auth->length = htons (total_length);
    s_in = (struct sockaddr_in *) & saremote;
    memset ((char *) s_in, '\0', sizeof (saremote));
    s_in->sin_family = AF_INET;
    s_in->sin_addr.s_addr = htonl(auth_ipaddr);
    s_in->sin_port = htons(svc_port);

    result = 0;
    /* send the second request after FAIL_RETRY_TIME secs, then repeat
     * at 2* secs, then 4* secs, ... while < TIMEOUT */
    alarmbackoff = FAIL_RETRY_TIME;
    old_sigalrm = signal(SIGALRM, auth_timeout);
    alarm(alarmbackoff);
    setjmp(env);
    while ((result < 1) && (alarmbackoff < TIMEOUT)) {
        D(("sending passwd_chg request to radius server, backoff %d seconds",
           alarmbackoff));
	sendto (sockfd, (char *) auth, (int) total_length,
		(int) 0, &saremote, sizeof(struct sockaddr_in));
	salen = sizeof (saremote);
	result = recvfrom (sockfd, (char *)recv_buffer, sizeof(recv_buffer),
			   (int) 0, &saremote, &salen);
    }
    alarm(0);
    signal(SIGALRM, old_sigalrm);
    close (sockfd);
    /* did our request timed out ? */
    if ((alarmbackoff >= TIMEOUT) && (result < 1)) {
        /* timed out, nothing received */
        rad_result->length = -1;
        rad_result->result = NULL;
        return PWDB_RADIUS_TIMEOUT;
    }
    /* else - not timeout, something received */
    if (result >= AUTH_HDR_LEN) { /* valid response ? */
        AUTH_HDR *recv_auth;
        int recv_len;

        recv_auth = (AUTH_HDR *)recv_buffer;
        recv_len = ntohs(recv_auth->length) - AUTH_HDR_LEN;
        if (recv_auth->code == PW_PASSWORD_ACK) {
            rad_result->length = recv_len;
            rad_result->result = (void *)recv_auth->data;
            return PWDB_RADIUS_SUCCESS;
        }
    }
    /* invalid response */
    rad_result->length = -1;
    rad_result->result = NULL;
    return PWDB_RADIUS_AUTH_FAIL;
}

int radius_acct_send (RADIUS_SERVER server, 
                      const char* username,
                      int sense,
                      int session_time)
{
    int salen;
    int sockfd;
    struct sockaddr saremote;
    struct sockaddr_in *s_in;
    struct servent *svp;
    short svc_port;
    AUTH_HDR *auth;
    unsigned char md5buf[256];
    UINT4 auth_ipaddr;
    u_short local_port;
    int total_length;
    u_char *ptr;
    int length, secretlen;
    int result;

    /* sanity checks */
    if (!server.hostname || !server.secret)
        return PWDB_RADIUS_BAD_REQ;

    svp = getservbyname ("radius", "udp");
    if (svp == (struct servent *) 0) {
        return PWDB_RADIUS_LOCAL_ERR;
    }

    svc_port = ntohs((u_short) svp->s_port);

    /* Get the IP address of the authentication server */
    if ((auth_ipaddr = get_ipaddr(server.hostname)) == (UINT4)0) {
        return PWDB_RADIUS_RESOLV_ERR;
    }

    sockfd = socket (AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        return PWDB_RADIUS_SOCKET_ERR;
    }

    local_port=radius_get_local_port(sockfd);
    if (local_port == 0) {
        close(sockfd);
        return PWDB_RADIUS_SOCKET_ERR;
    }
        
    auth = (AUTH_HDR *)send_buffer;
    auth->code = PW_ACCOUNTING_REQUEST;
    auth->id = radius_get_random_id();
    total_length = AUTH_HDR_LEN;
    ptr = auth->data;

    /***** USER NAME *****/
    *ptr++ = PW_USER_NAME;
    length = strlen(username);
    if (length > MAXPWNAM)
        length = MAXPWNAM;
    *ptr++ = length + 2;
    memcpy (ptr, username, length);
    ptr += length;
    total_length += length + 2;

    {
        long hostid;
        char * c_hostid;
        
        hostid = gethostid();
        c_hostid = (char *)&hostid;
        
        *ptr++ = PW_CLIENT_ID;
        *ptr++ = 6;
        *ptr++ = c_hostid[2];
        *ptr++ = c_hostid[3];
        *ptr++ = c_hostid[0];
        *ptr++ = c_hostid[1];
        total_length += 6;
    }

    {
        char pid_str[AUTH_HDR_LEN];
        int pstr_len;
        
        memset(pid_str, 0, AUTH_HDR_LEN);
        sprintf(pid_str,"%09d",getpid());
        pstr_len = strlen(pid_str);
        
        *ptr++ = PW_ACCT_SESSION_ID;
        *ptr++ = pstr_len + 2;
        memcpy(ptr, pid_str, pstr_len);
        ptr += pstr_len;
        total_length += pstr_len + 2;
    }

    *ptr++ = PW_CLIENT_PORT_ID;
    *ptr++ = 6;
    *ptr++ = 0;
    *ptr++ = 0;
    *(short *)ptr = htons(local_port);
    ptr += 2;
    total_length += 6;    

    *ptr++ = PW_ACCT_STATUS_TYPE;
    *ptr++ = 6;
    *ptr++ = 0;
    *ptr++ = 0;
    *ptr++ = 0;
    *ptr++ = sense;
    total_length += 6;    

    *ptr++ = PW_ACCT_DELAY_TIME;
    *ptr++ = 6;
    *ptr++ = 0;
    *ptr++ = 0;
    *ptr++ = 0;
    *ptr++ = 0;
    total_length += 6;    

    *ptr++ = PW_ACCT_SESSION_TIME;
    *ptr++ = 6;
    *(long *)ptr = htonl(session_time);
    ptr += 4;
    total_length += 6;

    /*
     * The NAS and RADIUS accounting server share a secret.  The Request
     * Authenticator field in Accounting-Request packets contains a one-
     * way MD5 hash calculated over a stream of octets consisting of the
     * Code + Identifier + Length + 16 zero octets + request attributes +
     * shared secret (where + indicates concatenation).  The 16 octet MD5
     * hash value is stored in the Authenticator field of the
     * Accounting-Request packet.
     */                                     
    memset(auth->vector, 0, AUTH_VECTOR_LEN);
    auth->length = htons (total_length);
    secretlen = strlen (server.secret);
    strcpy(send_buffer+total_length, server.secret);
    md5_calc(md5buf, (char *)auth, total_length+secretlen);
    memcpy(auth->vector, md5buf, AUTH_VECTOR_LEN);
    memset(send_buffer+total_length, 0, secretlen);
                
    s_in = (struct sockaddr_in *) & saremote;
    memset ((char *) s_in, '\0', sizeof (saremote));
    s_in->sin_family = AF_INET;
    s_in->sin_addr.s_addr = htonl(auth_ipaddr);
    s_in->sin_port = htons(svc_port);
    sendto (sockfd, (char *)auth, (int)total_length,
            (int)0, &saremote, sizeof(struct sockaddr_in));
    salen = sizeof (saremote);
    result = recvfrom (sockfd, (char *)recv_buffer, sizeof(recv_buffer),
                       (int)0, &saremote, &salen);
    close (sockfd);
    if (result >= AUTH_HDR_LEN) { /* valid response ? */
        AUTH_HDR *recv_auth;
        int recv_len;

        recv_auth = (AUTH_HDR *)recv_buffer;
        recv_len = ntohs(recv_auth->length) - AUTH_HDR_LEN;
        if (recv_auth->code == PW_ACCOUNTING_RESPONSE) {
            return PWDB_RADIUS_SUCCESS;
        }
    }
    /* invalid response */
    return PWDB_RADIUS_NET_FAIL;
}
