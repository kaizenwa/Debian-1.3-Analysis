/*
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 */
/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
** The original source is from the RPCSRC 4.0 package from Sun Microsystems.
** The Interface to keyserver protocoll 2 was added by 
** Thorsten Kukuk <kukuk@vt.uni-paderborn.de>
*/

#include <stdio.h>
#include <signal.h>
#include <rpc/rpc.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <rpc/key_prot.h>
#include <rpc/auth.h>
#include <signal.h>

#define KEY_TIMEOUT	5	/* per-try timeout in seconds */
#define KEY_NRETRY	12	/* number of retries */

#define debug(msg)		/* turn off debugging */

static struct timeval trytimeout = { KEY_TIMEOUT, 0 };
static struct timeval tottimeout = { KEY_TIMEOUT * KEY_NRETRY, 0 };

#ifdef __STDC__
extern int        _openchild       ( char *, FILE **, FILE ** );
extern bool_t     xdr_keybuf       ( XDR *xdrs, keybuf objp );
extern bool_t     xdr_keystatus    ( XDR *xdrs, keystatus *objp );
extern bool_t     xdr_cryptkeyarg  ( XDR *, cryptkeyarg * );
extern bool_t     xdr_cryptkeyres  ( XDR *, cryptkeyres * );

int         key_setsecret      __P (( char * ));
int         key_encryptsession __P (( char *, des_block * ));
int         key_decryptsession __P (( char *, des_block * ));
int         key_gendes         __P (( des_block * ));
static int  key_call           __P (( u_long proc,
                                   bool_t (*xdr_arg)( XDR *, void * ),
                                   void *arg,
                                   bool_t (*xdr_rslt)( XDR *, void * ),
                                   void *rslt ));
#else
extern bool_t xdr_des_block();
extern int _openchild();

int key_setsecret();
int key_encryptsession();
int key_decryptsession();
int key_gendes();
static int key_call();
#endif	/* __STDC__ */

#ifdef __STDC__
int
key_setsecret( char *secretkey )
#else
int
key_setsecret( secretkey )
   char *secretkey;
#endif
{
  keystatus status;
  
  if (!key_call( (u_long)KEY_SET, xdr_keybuf, (void *) secretkey,
		 xdr_keystatus, (void *) &status )) {
    return (-1);
  }
  if (status != KEY_SUCCESS) {
    debug("set status is nonzero");
    return (-1);
  }
  return (0);
}

/* key_secretkey_is_set() returns 1 if the keyserver has a secret key
  * stored for the caller's effective uid; it returns 0 otherwise
  *
  * N.B.:  The KEY_NET_GET key call is undocumented.  Applications shouldn't
  * be using it, because it allows them to get the user's secret key.
  */
int 
key_secretkey_is_set (void)
{
  struct key_netstres kres;
  
  memset((void*)&kres, 0, sizeof (kres));
  if (key_call((u_long) KEY_NET_GET, xdr_void, (char *)NULL,
	       xdr_key_netstres, (char *) &kres) &&
      (kres.status == KEY_SUCCESS) &&
      (kres.key_netstres_u.knet.st_priv_key[0] != 0))
    {
      /* avoid leaving secret key in memory */
      memset(kres.key_netstres_u.knet.st_priv_key, 0, HEXKEYBYTES);
      return (1);
    }
  return (0);
}



#ifdef __STDC__
int
key_encryptsession( char *remotename, des_block *deskey )
#else
int
key_encryptsession( remotename, deskey )
   char *remotename;
   des_block *deskey;
#endif
{
	cryptkeyarg arg;
	cryptkeyres res;

	arg.remotename = remotename;
	arg.deskey = *deskey;
	if (!key_call((u_long)KEY_ENCRYPT, xdr_cryptkeyarg,
                  (void *) &arg, xdr_cryptkeyres, (void *) &res)) {
		return (-1);
	}
	if (res.status != KEY_SUCCESS) {
		debug("encrypt status is nonzero");
		return (-1);
	}
	*deskey = res.cryptkeyres_u.deskey;
	return (0);
}

#ifdef __STDC__
int
key_decryptsession( char *remotename, des_block *deskey )
#else
int
key_decryptsession( remotename, deskey )
   char *remotename;
   des_block *deskey;
#endif
{
	cryptkeyarg arg;
	cryptkeyres res;

	arg.remotename = remotename;
	arg.deskey = *deskey;
	if (!key_call((u_long)KEY_DECRYPT, xdr_cryptkeyarg,
		(void *)&arg, xdr_cryptkeyres, (void *)&res))
	{
		return (-1);
	}
	if (res.status != KEY_SUCCESS) {
		debug("decrypt status is nonzero");
		return (-1);
	}
	*deskey = res.cryptkeyres_u.deskey;
	return (0);
}

int
key_encryptsession_pk (char *remotename, netobj *remotekey, des_block *deskey)
{
  cryptkeyarg2 arg;
  cryptkeyres res;
  
  arg.remotename = remotename;
  arg.remotekey = *remotekey;
  arg.deskey = *deskey;
  if (!key_call((u_long)KEY_ENCRYPT_PK, xdr_cryptkeyarg2, (char *)&arg,
		xdr_cryptkeyres, (char *)&res)) 
    {
      return (-1);
    }
  if (res.status != KEY_SUCCESS) 
    {
      debug("encrypt status is nonzero");
      return (-1);
    }
  *deskey = res.cryptkeyres_u.deskey;
  return (0);
}

int
key_decryptsession_pk(char *remotename, netobj *remotekey, des_block *deskey)
{
  cryptkeyarg2 arg;
  cryptkeyres res;
  
  arg.remotename = remotename;
  arg.remotekey = *remotekey;
  arg.deskey = *deskey;
  if (!key_call((u_long)KEY_DECRYPT_PK, xdr_cryptkeyarg2, (char *)&arg,
		xdr_cryptkeyres, (char *)&res))
    {
      return (-1);
    }
  if (res.status != KEY_SUCCESS) 
    {
      debug("decrypt status is nonzero");
      return (-1);
    }
  *deskey = res.cryptkeyres_u.deskey;
  return (0);
}

#ifdef __STDC__
int
key_gendes( des_block *key )
#else
int
key_gendes( key )
   des_block *key;
#endif
{
  struct sockaddr_in sin;
  CLIENT *client;
  int socket;
  enum clnt_stat stat;
  
  
  sin.sin_family = AF_INET;
  sin.sin_port = 0;
  sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  bzero(sin.sin_zero, sizeof(sin.sin_zero));
  socket = RPC_ANYSOCK;
  client = clntudp_bufcreate(&sin, (u_long)KEY_PROG, (u_long)KEY_VERS,
			     trytimeout, &socket, RPCSMALLMSGSIZE, 
			     RPCSMALLMSGSIZE);
  if (client == NULL) {
    return (-1);
  }
  stat = clnt_call(client, KEY_GEN, (xdrproc_t)xdr_void, NULL,
		   (xdrproc_t)xdr_des_block, (caddr_t)key, tottimeout);
  clnt_destroy(client);
  /*	(void) close(socket); */
  if (stat != RPC_SUCCESS) {
    return (-1);
  }
  return (0);
}

int
key_setnet(struct key_netstarg *arg)
{
  keystatus status;
  
  if (!key_call((u_long) KEY_NET_PUT, xdr_key_netstarg, (char *) arg,
		xdr_keystatus, (char *) &status))
    return (-1);
  
  if (status != KEY_SUCCESS)
    {
      debug("key_setnet status is nonzero");
      return (-1);
    }
  return (1);
}

int
key_get_conv(char *pkey, des_block *deskey)
{
  cryptkeyres res;
  
  if (!key_call((u_long) KEY_GET_CONV, xdr_keybuf, pkey,
		xdr_cryptkeyres, (char *)&res)) 
    {
      return (-1);
    }
  if (res.status != KEY_SUCCESS) 
    {
      debug("get_conv status is nonzero");
      return (-1);
    }
  *deskey = res.cryptkeyres_u.deskey;
  return (0);
}

/*
 * Hack to allow the keyserver to use AUTH_DES (for authenticated
 * NIS+ calls, for example).  The only functions that get called
 * are key_encryptsession_pk, key_decryptsession_pk, and key_gendes.
 *
 * The approach is to have the keyserver fill in pointers to local
 * implementations of these functions, and to call those in key_call().
 */

cryptkeyres *(*__key_encryptsession_pk_LOCAL)() = 0;
cryptkeyres *(*__key_decryptsession_pk_LOCAL)() = 0;
des_block *(*__key_gendes_LOCAL)() = 0;

#include <stdio.h>
#include <signal.h>
#include <sys/wait.h>

#ifdef __STDC__
static int
key_call( u_long proc, bool_t (*xdr_arg)( XDR *, void * ), void *arg,
			bool_t (*xdr_rslt)( XDR *, void * ), void *rslt )
#else
static int
key_call( proc, xdr_arg, arg, xdr_rslt, rslt )
   u_long proc;
   bool_t (*xdr_arg)();
   char *arg;
   bool_t (*xdr_rslt)();
   char *rslt;
#endif
{
  XDR xdrargs;
  XDR xdrrslt;
  FILE *fargs;
  FILE *frslt;
  int (*osigchild) ();
  union wait status;
  int pid;
  int success;
  int ruid;
  int euid;
  static char MESSENGER[] = "/usr/etc/keyenvoy";
  
  success = 1;
  osigchild = signal(SIGCHLD, SIG_IGN);

  if (proc == KEY_ENCRYPT_PK && __key_encryptsession_pk_LOCAL)
    {
      cryptkeyres *res;
      res = (*__key_encryptsession_pk_LOCAL)(geteuid (), arg);
      *(cryptkeyres*)rslt = *res;
      return 1;
    } 
  else 
    if (proc == KEY_DECRYPT_PK && __key_decryptsession_pk_LOCAL)
      {
	cryptkeyres *res;
	res = (*__key_decryptsession_pk_LOCAL)(geteuid (), arg);
	*(cryptkeyres*)rslt = *res;
	return 1;
      } 
    else 
      if (proc == KEY_GEN && __key_gendes_LOCAL)
	{
	  des_block *res;
	  res = (*__key_gendes_LOCAL)(geteuid(), 0);
	  *(des_block*)rslt = *res;
	  return 1;
	}
  
  /*
   * We are going to exec a set-uid program which makes our effective uid
   * zero, and authenticates us with our real uid. We need to make the 
   * effective uid be the real uid for the setuid program, and 
   * the real uid be the effective uid so that we can change things back.
   */
  euid = geteuid();
  ruid = getuid();
  (void) setreuid(euid, ruid);
  pid = _openchild(MESSENGER, &fargs, &frslt);
  (void) setreuid(ruid, euid);
  if (pid < 0) {
    debug("open_streams");
    return (0);
  }
  xdrstdio_create(&xdrargs, fargs, XDR_ENCODE);
  xdrstdio_create(&xdrrslt, frslt, XDR_DECODE);
  
  if (!xdr_u_long(&xdrargs, &proc) || !(*xdr_arg)(&xdrargs, arg)) {
		debug("xdr args");
		success = 0; 
	}
  (void) fclose(fargs);
  
  if (success && !(*xdr_rslt)(&xdrrslt, rslt)) {
    debug("xdr rslt");
    success = 0;
  }
  
#ifdef NOTDEF
  /*
   * WARNING! XXX
   * The original code appears first.  wait4 returns only after the process
   * with the requested pid terminates.  The effect of using wait() instead
   * has not been determined.
   */
  (void) fclose(frslt);
  if (wait4(pid, &status, 0, NULL) < 0 || status.w_retcode != 0) {
    debug("wait4");
    success = 0;
  }
#endif /* def NOTDEF */
  if (wait(&status) < 0 || status.w_retcode != 0) {
    debug("wait");
    success = 0;
  }
  (void)signal(SIGCHLD, osigchild);
  
  return (success);
}

