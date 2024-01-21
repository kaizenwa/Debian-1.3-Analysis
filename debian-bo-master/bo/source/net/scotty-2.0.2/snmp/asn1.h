/*
 * asn1.h
 *
 * Copyright (c) 1994, 1995
 *
 * Sven Schmidt, J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#ifndef _ASN1_H
#define _ASN1_H

/*
 * Definition of ASN1 data types used for SNMP.
 */

#define UNIVERSAL		( 0x00 )
#define APPLICATION		( 0x40 )
#define CONTEXT_SPECIFIC	( 0x80 )
#define PRIVATE			( 0xc0 )

#define PRIMITIVE		( 0x00 )
#define CONSTRUCTED		( 0x20 )

#define TAG_0			( 0x00 )
#define TAG_1			( 0x01 )
#define TAG_2			( 0x02 )
#define TAG_3			( 0x03 )
#define TAG_4			( 0x04 )
#define TAG_5			( 0x05 )
#define TAG_6			( 0x06 )
#define TAG_7			( 0x07 )
#define TAG_8			( 0x08 )

#define	ASN1_OTHER		( 0x00 )
#define ASN1_BOOLEAN		( 0x01 )
#define	ASN1_INTEGER		( 0x02 )
#define ASN1_BIT_STRING		( 0x03 )
#define	ASN1_OCTET_STRING	( 0x04 )
#define	ASN1_NULL		( 0x05 )
#define ASN1_OBJECT_IDENTIFIER	( 0x06 )
#define ASN1_SEQUENCE		( 0x10 )
#define	ASN1_SEQUENCE_OF	( 0x11 )

#define ASN1_IpAddress		( APPLICATION | PRIMITIVE | TAG_0 )
#define ASN1_Counter32		( APPLICATION | PRIMITIVE | TAG_1 )
#define ASN1_Gauge32		( APPLICATION | PRIMITIVE | TAG_2 )
#define ASN1_TimeTicks		( APPLICATION | PRIMITIVE | TAG_3 )
#define ASN1_Opaque		( APPLICATION | PRIMITIVE | TAG_4 )
#define ASN1_NsapAddress	( APPLICATION | PRIMITIVE | TAG_5 )
#define ASN1_Counter64		( APPLICATION | PRIMITIVE | TAG_6 )
#define ASN1_UInteger32		( APPLICATION | PRIMITIVE | TAG_7 )

/*
 * Exception codes used in SNMPv2 varbind lists (RFC 1448).
 */

#define ASN1_NO_SUCH_OBJECT	( CONTEXT_SPECIFIC | PRIMITIVE | TAG_0 )
#define ASN1_NO_SUCH_INSTANCE	( CONTEXT_SPECIFIC | PRIMITIVE | TAG_1 )
#define ASN1_END_OF_MIB_VIEW	( CONTEXT_SPECIFIC | PRIMITIVE | TAG_2 )

/*
 * Non-aggregate types for MIB tree nodes that are not defined in SMI or
 * to distinguish from their SNMPv1 values. 
 */

#define Integer32		( 0x14 )
#define	NetworkAddress		( 0x15 )
#define	Counter			( 0x16 )
#define Gauge			( 0x17 )

/*
 * Object identifier are represented as a vector of unsigned int.
 * The maximum length of an object identifier is 128 (see RFC 1448).
 */

#define OID_MAXLEN 128
typedef u_int ASN1_OID;

/*
 * Functions that manipulate Object Identifier.
 */

extern char*
ASN1_Oid2Str		_ANSI_ARGS_((ASN1_OID *oid, int len));

extern ASN1_OID*
ASN1_Str2Oid		_ANSI_ARGS_((char *str, int *len));

extern ASN1_OID*
ASN1_OidDup		_ANSI_ARGS_((int *buflen, ASN1_OID *oid, int oidLen));

extern int
ASN1_IsOid		_ANSI_ARGS_((char *str));

extern char*
ASN1_Hex2Oid		_ANSI_ARGS_((char *str));

/*
 * Convert syntax names into the internal representation and back.
 */

extern int
ASN1_Str2Sntx		_ANSI_ARGS_((char *str));

extern char*
ASN1_Sntx2Str		_ANSI_ARGS_((int sntx));

/*
 * This function return a static error string describing the last error.
 */

extern char*
ASN1_ErrorString	_ANSI_ARGS_((void));

/*
 * Functions visible for other modules (encoding/decoding)
 */

extern u_char*
ASN1_EncodeLength	_ANSI_ARGS_((u_char *packet, int *packetlen,
				     u_char *len_fld, int length));
extern u_char*
ASN1_DecodeLength	_ANSI_ARGS_((u_char *packet, int *packetlen,
				     u_int *length));
extern u_char*
ASN1_EncodeInt		_ANSI_ARGS_((u_char *packet, int *packetlen,
				     u_char tag, int value));
extern u_char*
ASN1_DecodeInt		_ANSI_ARGS_((u_char *packet, int *packetlen,
				     u_char tag, int *value));
extern u_char*
ASN1_EncodeCounter64	_ANSI_ARGS_((u_char *packet, int *packetlen,
				     double value));
extern u_char*
ASN1_DecodeCounter64	_ANSI_ARGS_((u_char *packet, int *packetlen,
				     double *value));
extern u_char*
ASN1_EncodeOID		_ANSI_ARGS_((u_char *packet, int *packetlen,
				     ASN1_OID *oid, int oidlen));
extern u_char*
ASN1_DecodeOID		_ANSI_ARGS_((u_char *packet, int *packetlen,
				     ASN1_OID *oid, int *oidlen));
extern u_char*
ASN1_EncodeOctetString	_ANSI_ARGS_((u_char *packet, int *packetlen,
				     u_char tag, u_char *octets, int len));
extern u_char*
ASN1_DecodeOctetString	_ANSI_ARGS_((u_char *packet, int *packetlen,
				     u_char tag, u_char **octets, int *len));
extern u_char*
ASN1_EncodeNull		_ANSI_ARGS_((u_char *packet, int *packetlen,
				     u_char tag));
extern u_char*
ASN1_DecodeNull		_ANSI_ARGS_((u_char *packet, int *packetlen,
				     u_char tag));

#endif /* _ASN1_H */
