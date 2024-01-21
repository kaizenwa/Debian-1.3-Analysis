/*
 * crypt.h: header for crypt.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: crypt.h,v 1.9 1995/08/31 03:51:16 scottr Exp $
 */

#ifndef __crypt_h_
#define __crypt_h_

	char	*crypt_msg _((char *, char *, int));
	void	encrypt_cmd _((char *, char *, char *));
	char	*is_crypted _((char *));

#define CRYPT_HEADER ""
#define CRYPT_HEADER_LEN 5

#endif /* __crypt_h_ */
