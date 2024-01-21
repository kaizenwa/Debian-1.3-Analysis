#if 0
#ifndef lint
static char sccsid[] = "@(#)xcrypt.c	2.2 88/08/10 4.0 RPCSRC";
#endif
#endif /* #if 0 */
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
 * Hex encryption/decryption and utility routines
 *
 * Copyright (C) 1986, Sun Microsystems, Inc. 
 */

#include <stdio.h>
#include <des_crypt.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>

extern char hex[];	/* forward */

#ifdef __STDC__
int xencrypt( char *, char * );
int xdecrypt( char *, char * );
void passwd2des( char *, char * );
static void hex2bin( int, char *, char * );
static void bin2hex( int, unsigned char *, char * );
static char hexval( char );
#else
int xencrypt();
int xdecrypt();
void passwd2des();
static void hex2bin();
static void bin2hex();
static char hexval();
#endif 	/* __STDC__ */

/*
 * Encrypt a secret key given passwd
 * The secret key is passed and returned in hex notation.
 * Its length must be a multiple of 16 hex digits (64 bits).
 */
#ifdef __STDC__
int
xencrypt( char *secret, char *passwd )
#else
int
xencrypt(secret, passwd)
	char *secret;
	char *passwd;
#endif
{
	char key[8];
	char ivec[8];
	char *buf;
	int err;
	int len;

	len = strlen(secret) / 2;
	buf = malloc((unsigned)len);

	hex2bin(len, secret, buf);
	passwd2des(passwd, key);
	bzero(ivec, 8);

	err = cbc_crypt(key, buf, len, DES_ENCRYPT | DES_HW, ivec);
	if (DES_FAILED(err)) {	
		free(buf);
		return (0);
	}
	bin2hex(len, (unsigned char *) buf, secret);
	free(buf);
	return (1);
}

/*
 * Decrypt secret key using passwd
 * The secret key is passed and returned in hex notation.
 * Once again, the length is a multiple of 16 hex digits
 */
#ifdef __STDC__
int
xdecrypt( char *secret, char *passwd )
#else
int
xdecrypt(secret, passwd)
	char *secret;
	char *passwd;
#endif
{
	char key[8];
	char ivec[8];
	char *buf;
	int err;
	int len;

	len = strlen(secret) / 2;
	buf = malloc((unsigned)len);

	hex2bin(len, secret, buf);
	passwd2des(passwd, key);	
	bzero(ivec, 8);

	err = cbc_crypt(key, buf, len, DES_DECRYPT | DES_HW, ivec);
	if (DES_FAILED(err)) {
		free(buf);
		return (0);
	}
	bin2hex(len, (unsigned char *) buf, secret);
	free(buf);
	return (1);
}

/*
 * Turn password into DES key
 */
#ifdef __STDC__
void
passwd2des( char *pw, char *key )
#else
void
passwd2des(pw, key)
	char *pw;
	char *key;
#endif
{
	int i;

	bzero(key, 8);
	for (i = 0; *pw; i = (i+1)%8) {
		key[i] ^= *pw++ << 1;
	}
	des_setparity(key);
}

/*
 * Hex to binary conversion
 */
#ifdef __STDC__
static void
hex2bin( int len, char *hexnum, char *binnum )
#else
static void
hex2bin(len, hexnum, binnum)
	int len;
	char *hexnum;
	char *binnum;
#endif
{
	int i;

	for (i = 0; i < len; i++) {
		*binnum++ = 16 * hexval(hexnum[2*i]) + hexval(hexnum[2*i+1]);	
	}
}

/*
 * Binary to hex conversion
 */
#ifdef __STDC__
static void
bin2hex( int len, unsigned char *binnum, char *hexnum )
#else
static void
bin2hex(len, binnum, hexnum)
	int len;
	unsigned char *binnum;
	char *hexnum;
#endif
{
	int i;
	unsigned val;

	for (i = 0; i < len; i++) {
		val = binnum[i];
		hexnum[i*2] = hex[val >> 4];
		hexnum[i*2+1] = hex[val & 0xf];
	}
	hexnum[len*2] = 0;
}

static char hex[16] = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
};

#ifdef __STDC__
static char
hexval( char c )
#else
static char
hexval(c)
	char c;
#endif
{
	if (c >= '0' && c <= '9') {
		return (c - '0');
	} else if (c >= 'a' && c <= 'z') {
		return (c - 'a' + 10);
	} else if (c >= 'A' && c <= 'Z') {
		return (c - 'A' + 10);
	} else {
		return (-1);
	}
}
