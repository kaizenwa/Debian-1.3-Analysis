/*
 *  linux/ibcs/socket.h
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: socket.h,v 1.1 1994/03/03 11:36:14 mike Exp $
 * $Source: /usr/CVS/ibcs/include/ibcs/socket.h,v $
 */

/* Linux spells this differently. */
#define SO_ACCEPTCONN	SO_ACCEPTCON

/* These aren't (currently) defined by Linux. Watch out for warnings
 * about redefinitions...
 */
#define SO_USELOOPBACK	0xff02
#define SO_ORDREL	0xff03
#define SO_IMASOCKET	0xff04
#define SO_SNDLOWAT	0xff05
#define SO_RCVLOWAT	0xff06
#define SO_SNDTIMEO	0xff07
#define SO_RCVTIMEO	0xff08
#define SO_PROTOTYPE	0xff09
