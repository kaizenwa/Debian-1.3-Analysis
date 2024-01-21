/*
 * asn1.c
 *
 * This is the implementation of the ASN1/BER encoding and 
 * decoding functions. This file also includes the functions
 * to handle ASN1 object identifier.
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

#include "snmp.h"

#include <math.h>

#define BIT_8 0x80

/*
 * The static error string is modified whenever we have found an error.
 * ASN1_ErrorString() can be used to retrieve the error message from other
 * modules.
 */

static char error[256];

char*
ASN1_ErrorString ()
{
    return error;
}


/*
 * The following table is used to convert syntax names to syntax token.
 * Access to the table is done by ASN1_Str2Sntx() and ASN1_Sntx2Str().
 */

struct ASN1Type {
   char *name;
   int syntax;
};

static struct ASN1Type syntaxTable[] =
{
   { "ASN1_OTHER",		ASN1_OTHER 		},
   { "BOOLEAN",			ASN1_BOOLEAN 		},
   { "INTEGER",			ASN1_INTEGER 		},
   { "BIT STRING",		ASN1_BIT_STRING		},
   { "OCTET STRING",		ASN1_OCTET_STRING 	},
   { "ASN1 NULL",		ASN1_NULL		},
   { "OBJECT IDENTIFIER",	ASN1_OBJECT_IDENTIFIER	},
   { "7",			ASN1_OTHER 		},
   { "8",			ASN1_OTHER 		},
   { "9",			ASN1_OTHER 		},
   { "10",			ASN1_OTHER 		},
   { "11",			ASN1_OTHER 		},
   { "12",			ASN1_OTHER 		},
   { "13",			ASN1_OTHER 		},
   { "14",			ASN1_OTHER 		},
   { "15",			ASN1_OTHER 		},
   { "SEQUENCE",		ASN1_SEQUENCE		},
   { "SEQUENCE OF",		ASN1_SEQUENCE_OF	},
   { "18",			ASN1_OTHER 		},
   { "19",			ASN1_OTHER 		},
   { "Integer32",		ASN1_INTEGER		},
   { "NetworkAddress",		ASN1_IpAddress		},
   { "Counter32",		ASN1_Counter32		},
   { "Gauge32",			ASN1_Gauge32		},
   { "DisplayString",		ASN1_OCTET_STRING	},
   { "PhysAddress",		ASN1_OCTET_STRING	},
   { "OwnerString",		ASN1_OCTET_STRING	},
   { "EntryStatus",		ASN1_INTEGER		},
   { "Party",			ASN1_OBJECT_IDENTIFIER	},
   { "TAddress",		ASN1_OCTET_STRING	},
   { "30",			ASN1_OTHER 		},
   { "31",			ASN1_OTHER 		},
   { "32",			ASN1_OTHER 		},
   { "33",			ASN1_OTHER 		},
   { "34",			ASN1_OTHER 		},
   { "35",			ASN1_OTHER 		},
   { "36",			ASN1_OTHER 		},
   { "37",			ASN1_OTHER 		},
   { "38",			ASN1_OTHER 		},
   { "39",			ASN1_OTHER 		},
   { "40",			ASN1_OTHER 		},
   { "41",			ASN1_OTHER 		},
   { "42",			ASN1_OTHER 		},
   { "43",			ASN1_OTHER 		},
   { "44",			ASN1_OTHER 		},
   { "45",			ASN1_OTHER 		},
   { "46",			ASN1_OTHER 		},
   { "47",			ASN1_OTHER 		},
   { "48",			ASN1_OTHER 		},
   { "49",			ASN1_OTHER 		},
   { "50",			ASN1_OTHER 		},
   { "51",			ASN1_OTHER 		},
   { "52",			ASN1_OTHER 		},
   { "53",			ASN1_OTHER 		},
   { "54",			ASN1_OTHER 		},
   { "55",			ASN1_OTHER 		},
   { "56",			ASN1_OTHER 		},
   { "57",			ASN1_OTHER 		},
   { "58",			ASN1_OTHER 		},
   { "59",			ASN1_OTHER 		},
   { "60",			ASN1_OTHER 		},
   { "61",			ASN1_OTHER 		},
   { "62",			ASN1_OTHER 		},
   { "63",			ASN1_OTHER 		},
   { "IpAddress",		ASN1_IpAddress		},
   { "Counter",			ASN1_Counter32		},
   { "Gauge",			ASN1_Gauge32		},
   { "TimeTicks",		ASN1_TimeTicks		},
   { "Opaque",			ASN1_Opaque		},
   { "NsapAddress",		ASN1_NsapAddress	},
   { "Counter64",		ASN1_Counter64		},
   { "UInteger32",		ASN1_UInteger32		},
   { NULL }
};

/*
 * ASN1_Sntx2Str() converts a syntax token into a string representation
 * which is some static storage. ASN1_Sntx2Str() returns NULL if the 
 * argument is not one of the known syntaxes.
 */

char*
ASN1_Sntx2Str (syntax)
     int syntax;
{
    if (syntax >= 0 
	&& (syntax < sizeof(syntaxTable)/sizeof(struct ASN1Type))) {
	return syntaxTable[syntax].name;
    } else {
	return NULL;
    }
}


/* 
 * ASN1_Str2Sntx() converts the string to an ASN1 syntax token. 
 * ASN1_Str2Sntx() returns 0 if the string is not a know syntax.
 */

int
ASN1_Str2Sntx (string)
     char *string;
{
    struct ASN1Type *typePtr;

    for (typePtr = syntaxTable; typePtr->name; typePtr++) {
	if (!strcmp (string, typePtr->name)) {
	    return typePtr->syntax;
	}
    }
    
    return 0;
}


/*
 * ASN1_OidDup() duplicates an object identifier. The length is 
 * returned in buflen.
 */

ASN1_OID*
ASN1_OidDup (buflen, oid, oidLen)
     int *buflen;
     ASN1_OID *oid;
     int oidLen;
{
    ASN1_OID *buf = (ASN1_OID *) ckalloc (sizeof (ASN1_OID) * oidLen);
    *buflen = oidLen;
    memcpy (buf, oid, oidLen * sizeof (ASN1_OID));
    return buf;
}


/*
 * ASN1_Oid2Str() converts an object identifier into string in 
 * dotted notation. The string is build in static memory and must 
 * not be freed by the caller. The caller is responsible to
 * make a copy of the string if it is to be used after another
 * call to ASN1_Oid2Str().
 */

char*
ASN1_Oid2Str (oid, oidLen)
     ASN1_OID *oid;
     int oidLen;
{
    int	 i;
    static char buf[OID_MAXLEN * 8];
    char *cp;

    if (oid == NULL) return NULL;

    buf[0] = '\0';
    
    for (cp = buf, i = 0; i < oidLen; i++) {
	sprintf (cp, "%u.", oid[i]);
	while (*cp) cp++;
    }
    if (cp > buf) {
	*--cp = '\0';
    }
    
    return buf;
}


/*
 * ASN1_Str2Oid() converts a string with an oid in dotted representation
 * into an object identifier vector. The vector is static memory and must
 * not be freed by the caller. 
 */

ASN1_OID*
ASN1_Str2Oid (str, len)
     char *str;
     int  *len;
{
    static ASN1_OID oid[OID_MAXLEN];

    if (str == NULL) return NULL;
    if (*str == '.') str++;

    memset ((char *) oid, '\0', sizeof (oid));

    if (! *str) {
	*len = 0;
	return oid;
    }

    for (*len = 0; *str; str++) {
	if (isdigit (*str)) {
	    oid[*len] = 10 * oid[*len] + *str - '0';
	} else {
	    *len += 1;
	}
    }

    *len += 1;
    return oid;
}


/*
 * ASN1_IsOid() tests the given string, whether it consists of dots and
 * digits only. If the string is an OID, 1 is returned, 0 otherwise.
 * Do not accept any hexadecimal digits. Use ASN1_Hex2Oid() to convert
 * object identifier containing hexadecimal digits.
 */

int
ASN1_IsOid (str)
     char *str;
{
    char *cp;

    for (cp = str; *cp != '\0'; cp++) {
        if (!isdigit (*cp) && *cp != '.') return 0;
    }
    
    return 1;
}


/*
 * ASN1_Hex2Oid() tests whether the string contains any hexadecimal
 * subidentifier and returns a new string with all hex subidentifier
 * expanded. If there is nothing to expand, a NULL pointer is returned.
 */

char*
ASN1_Hex2Oid (str)
     char *str;
{
    static char expstr[OID_MAXLEN * 8];
    char *p, *s;
    int convert = 0;

    if (! str) return NULL;

    /* 
     * Test if the str contains any hexadecimal subidentifier indicated 
     * by ':' separators that should be converted to integer subidentifiers
     * first.
     */
    
    for (p = str; *p; p++) {
	if (*p == ':') {
	    convert = 1;
	    break;
	}
    }

    if (!convert) return NULL;

    /*
     * Scan through the string all convert hexadecimal subidentifier
     * to integer subidentifier. Allocate some memory to hold the new
     * expanded str.
     */
    
    for (p = str, s = expstr; *p; ) { 
	if (p[0] == ':' && isxdigit(p[1]) && isxdigit(p[2]) 
	    && (p[3] == '.' || p[3] == ':' || p[3] == '\0')) {
	    char c = p[1] & 0xff;
	    int v = c >= 'a' ?  c - 87 : (c >= 'A' ? c - 55 : c - 48);
	    c = p[2] & 0xff;
	    v = (v << 4) + (c >= 'a' ?  c - 87 
			    : (c >= 'A' ? c - 55 : c - 48));
	    sprintf (s, ".%d", v);
	    while (*s != '\0') s++;
	    p += 3;
	} else {
	    *s++ = *p++;
	}
    }
    *s = '\0';
    
    return expstr;
}


/* 
 * ASN1_EncodeLength() sets the length field of any ASN1 encoded
 * type. If length is > 0x7F the array is shifted to get enough 
 * space to hold the value.
 */

u_char*
ASN1_EncodeLength (packet, packetlen, len_fld, length)
     u_char	*packet;
     int	*packetlen;
     u_char	*len_fld;
     int	length;
{
    int i;

    if (! packet) {
	return packet;
    }

    if (length <= 0x7F) {

	*len_fld = length;
	
    } else if (length <= 0xFF) {

        for (i = packet - len_fld - 1; i > 0; i--) {
	    len_fld[i + 1] = len_fld[i];
	}
	packet     += 1;
	*packetlen += 1;
	*len_fld++ = ( BIT_8 | 0x01 );
	*len_fld++ = length;

    } else if (length <= 0xFFFF) {

	for (i = packet - len_fld - 1; i > 0; i--) {
	    len_fld[i + 2] = len_fld[i];
        }
	packet     += 2;
	*packetlen += 2;
	*len_fld++ = ( BIT_8 | 0x02 );
	*len_fld++ = ( ( length >> 8 ) & 0xFF );
	*len_fld++ = ( length & 0xFF );

    } else {

	strcpy (error, "failed to encode very long ASN1 length");
	return NULL;
    }

    return packet;
}



/*
 * ASN1_DecodeLength() decodes the length field of any ASN1 encoded type.
 * If length field is in indefinite length form or longer than
 * the size of an unsigned int, an error is reported.
 */

u_char*
ASN1_DecodeLength (packet, packetlen, length)
     u_char	*packet;
     int	*packetlen;
     u_int	*length;
{
    u_char	*cp;

    if (! packet) {
        return packet;
    }
    
    /*
     * Check if length field is longer than one byte.
     */
    
    if (*packet & BIT_8) {

	cp = packet;
	*packet &= ~BIT_8;
	if (*packet == 0) {
	    strcpy (error, "indefinite length format not supported");
	    return NULL;
	}
	if (*packet > sizeof (int)) {
	    strcpy (error, "data lengths of this size not supported");
	    return NULL;
	}
	memcpy ((char *) length, (char *) packet + 1, (int) *packet);
	*length = ntohl( *length );
	*length >>= ( 8 * ( (sizeof *length) - (int) *packet));
	
	*packetlen += ( 1 + *packet );
	packet     += ( 1 + *packet );
	*cp |= BIT_8;

    } else {

	*length    = (int) *packet++;
	*packetlen += 1;
    }

    return packet;
}


/*
 * ASN1_EncodeInt() encodes an ASN1 Integer value (means a int) to an
 * octet string by using the primitive, definite length encoding
 * method. Returns a pointer to the first byte past the end of this 
 * object or NULL on any error.
 */

u_char*
ASN1_EncodeInt (packet, packetlen, tag, value)
     u_char	*packet;
     int	*packetlen;
     u_char	tag;
     int	value;
{
    int asnlen  = 0;
    int intsize = sizeof (int);
    int mask;
    u_char *length;

    if (! packet) {
	return packet;
    }

    /* 
     * Encode tag and reserve space for length.
     */

    *packet++  = tag;
    *packetlen += 1;
    
    length = packet++;   
    *packetlen += 1;
    
    /* 
     * Set the leftmost 9 bits of mask to 1 and check if the 
     * leftmost bits of value are 0 or 1.
     */

    mask = 0x1FF << ( ( 8 * ( sizeof (int) - 1 ) ) - 1 );
    
    while ((((value & mask) == 0) 
	    || ((value & mask) == mask )) && intsize > 1) {
	intsize--;
	value <<= 8;
    }

    /*
     * Set the leftmost 8 bits of mask to 1 and build the 
     * two's complement of value.
     */

    mask = 0xFF << ( 8 * ( sizeof (int) - 1 ) );
    while (intsize--) {
	*packet++ = (( value & mask ) >> ( 8 * ( sizeof (int) - 1 )));
	*packetlen += 1;
	value <<= 8;
	asnlen += 1;
    }

    /*
     * Encode length field and return.
     */

    packet = ASN1_EncodeLength (packet, packetlen, length, asnlen);
    return packet;
}


/*
 * ASN1_DecodeInt() decodes an ASN.1 integer value. We return an
 * error if an int is not large enough to hold the ASN.1 value.
 */

u_char*
ASN1_DecodeInt (packet, packetlen, tag, value)
     u_char	*packet;
     int	*packetlen;
     u_char	tag;
     int	*value;
{
    u_int asnlen = 0;

    if (! packet) {
        return packet;
    }

    if (*packet++ != tag) {
	sprintf (error, "invalid tag: 0x%.2x, expecting 0x%.2x",
		 *--packet, tag);
	return NULL;
    }
    *packetlen += 1;

   /*
    * Handle invalid integer size.
    */

    packet = ASN1_DecodeLength (packet, packetlen, &asnlen);
    if (packet == NULL) return NULL;
    
    if (asnlen == 0) {
	*value = 0;
	return packet;
    }

    /*
     * Check for an overflow for normal 32 bit integer values.
     */
    
    if ((*packet != 0 && asnlen > sizeof (int))
	|| (*packet == 0 && asnlen-1 > sizeof (int))) {
	sprintf (error,
		 "integer overflow: %d bytes received, %d bytes available",
		 asnlen, (int) sizeof (int));
	return NULL;
    }
    
    /*
     * Check if it's a negative value and decode data.
     */
    
    if ((tag == ASN1_INTEGER) && (*packet & BIT_8)) {
	*value = -1;
    } else {
	*value = 0;
    }

    while (asnlen-- > 0) {
	*value = ( *value << 8 ) | (*packet++ & 0xff);
	*packetlen += 1;
    }

    return packet;
}


/*
 * ASN1_EncodeCounter64() encodes an ASN1 Counter64 value into an
 * octet string by using the primitive, definite length encoding
 * method. Returns a pointer to the first byte past the end of this 
 * object or NULL on any error. Note, on 64 bit machines, the
 * ASN1_EncodeInt() should be used as it yields accurate results.
 */

u_char*
ASN1_EncodeCounter64 (packet, packetlen, value)
     u_char	*packet;
     int	*packetlen;
     double	value;
{
    int i, asnlen = 0;
    u_char *length;
    double d;

    if (! packet) {
	return packet;
    }

    /* 
     * Encode type and reserve space for length.
     */

    *packet++  = ASN1_Counter64;
    *packetlen += 1;
    
    length = packet++;   
    *packetlen += 1;

    /*
     * Calculate the number of bytes needed to encode the ASN.1
     * integer.
     */

    for (d = value; d >= 1; asnlen++) {
	d /= 256.0;
    }

    /*
     * Now encode the bytes: We start at the end and move up
     * to the high byte.
     */

    for (i = asnlen - 1; i >= 0; i--) {
	d = value / 256.0;
	packet[i] = (int) (value - floor (d) * 256);
	value = d;
    }
    packet += asnlen;
    *packetlen += asnlen;
    
    /*
     * Encode length field and return.
     */

    packet = ASN1_EncodeLength (packet, packetlen, length, asnlen);
    return packet;
}


/*
 * ASN1_DecodeCounter64() decodes an ASN.1 Counter64 value. We return 
 * the result as a double. Use ASN1_DecodeInt() on 64 bit machines
 * to get an accurate result.
 */

u_char*
ASN1_DecodeCounter64 (packet, packetlen, value)
     u_char	*packet;
     int	*packetlen;
     double	*value;
{
    u_int asnlen = 0;

    if (! packet) {
        return packet;
    }

    if (*packet++ != ASN1_Counter64) {
	sprintf (error, "invalid tag: 0x%.2x, expecting 0x%.2x",
		 *--packet, ASN1_Counter64);
	return NULL;
    }
    *packetlen += 1;

    /*
     * Handle invalid integer size.
     */

    packet = ASN1_DecodeLength (packet, packetlen, &asnlen);
    if (packet == NULL) return NULL;
    
    if (asnlen == 0) {
	*value = 0;
	return packet;
    }

    /*
     * Accept any integer length. :-)
     */

    *value = 0;
    while (asnlen-- > 0) {
	*value = *value * 256 + (*packet++ & 0xff);
	*packetlen += 1;
    }

    return packet;
}


/*
 * ASN1_EncodeOID() encodes an OBJECT IDENTIFIER to an octet string by
 * using the primitive, definite length encoding method. On success, a
 * pointer to the first byte past the end of this object is returned,
 * or NULL on any error.
 */

u_char*
ASN1_EncodeOID (packet, packetlen, oid, oidLen)
     u_char	*packet;
     int	*packetlen;
     ASN1_OID	*oid;
     int	oidLen;
{
    int asnlen = 0;
    long mask, bits;
    ASN1_OID *op = oid;
    u_char *length;
    
    if (! packet) {
	return packet;
    }

    /* 
     * Check for a valid length.
     */

    if (oidLen == 0) {
	strcpy (error, "OBJECT IDENTIFIER of length 0");
        return NULL;
    }

    /* 
     * Encode type and reserve space for length.
     */

    *packet++  = ASN1_OBJECT_IDENTIFIER;
    *packetlen += 1;

    length = packet++;   
    *packetlen += 1;

    if (oidLen == 1) {
      
        *packet++  = oid[0];
        *packetlen += 1;
        asnlen += 1;

    } else {

        /*
	 * Encode the first two components using the formula (X * 40) + Y
	 */
      
        *packet++  = oid[0] * 40 + oid[1];
	*packetlen += 1;
	asnlen += 1;
	oidLen -= 2;
	op     += 2;
	
	/*
	 * Encode the remaining subidentifier.
	 */
	
	while (oidLen-- > 0) {
	
	    /* are seven bits enough for this component */
	
	    if (*op <= 0x7F) {
	    
	        *packet++  = *op++;
	        *packetlen += 1;
	        asnlen += 1;
	      
	    } else {
	    
	        /* we need two or more octets for encoding */
	    
	        /* check nr of bits for this component */
	    
	        int n = sizeof (*op) * 8;		/* max bit of op */
	    
		mask = 1 << (n - 1);
		for (bits = n; bits > 0; bits--, mask >>= 1) {
		    if (*op & mask) break;
		}
	    
		/* round # of bits to multiple of 7: */
	    
		bits = ((bits + 6) / 7) * 7;

		/* Handle the first sequence of 7 bits if we have a
		   large number. */

		if (bits > n ) {
		    bits -= 7;
		    *packet++  = ((( *op >> bits ) & 0x7F) | BIT_8 );
                    *packetlen += 1;
		    asnlen += 1;
		}

		mask = (1 << bits) - 1;
	    
		/* encode the mostleft 7 bits and shift right */
		
		for (; bits > 7; mask >>= 7 ) {
		    bits -= 7;
		    *packet++  = ((( *op & mask ) >> bits ) | BIT_8 );
		    *packetlen += 1;
		    asnlen += 1;
	        }
	    
		*packet++ = ( *op++ & mask );
		*packetlen += 1;
		asnlen += 1;
	    }
	}
    }

    /* 
     * Finally, encode length field.
     */

    packet = ASN1_EncodeLength (packet, packetlen, length, asnlen);
    return packet;
}



/* 
 * ASN1_DecodeOID() decodes and object identifier. The caller of this
 * function is responsible to provide enough memory to hold the
 * object identifier.
 */

u_char*
ASN1_DecodeOID (packet, packetlen, oid, oidLen)
     u_char	*packet;
     int	*packetlen;
     ASN1_OID	*oid;
     int	*oidLen;
{
    u_int asnlen;
    ASN1_OID *op = oid;

    if (! packet) {
        return packet;
    }

    if (*packet++ != ASN1_OBJECT_IDENTIFIER) {
	sprintf (error, "invalid tag: 0x%.2x, expecting 0x%.2x",
		 *--packet, ASN1_OBJECT_IDENTIFIER );
	return NULL;
    }
    *packetlen += 1;
    
    /*
     * Handle invalid length field.
     */
    
    packet = ASN1_DecodeLength (packet, packetlen, &asnlen);
    if (packet == NULL) return NULL;
    
    if (asnlen == 0) {
	strcpy (error, "OBJECT IDENTIFIER of length 0");
	return NULL;
    }
    
    if (asnlen == 1 && (*packet % 40 == *packet)) {
	*oid       = *packet++;
	*oidLen    = 1;
	*packetlen += 1;
	return packet;
    }
    
    /*
     * Decode the first component to the first two subidentifiers.
     */
    
    oid[1] = (u_char)( *packet % 40 );
    oid[0] = (u_char)(( *packet++ - oid[1] ) / 40 );
    op         += 2;
    *oidLen    = 2;
    asnlen     -= 1;
    *packetlen += 1;
    
    /*
     * Decode the remaining subidentifer.
     */

    while (asnlen > 0) {
	memset ((char *)op, '\0', sizeof (oid));
	while (*packet > 0x7F) {
	    /* hansb@aie.nl (Hans Bayle) had problems with SCO. */
	    *op = ( *op << 7 ) + ( *packet++ & 0x7F );
	    asnlen     -= 1;
	    *packetlen += 1;
	}

	*op = ( *op << 7 ) + ( *packet++ );
	op         += 1;
	*oidLen    += 1;
	asnlen     -= 1;
	*packetlen += 1;
    }

    return packet;
}


/*
 * ASN1_EncodeOctetString() encodes an OCTET STRING to an octet string by
 * using the primitive, definite length encoding method. On success, a
 * pointer to the first byte past the end of this object is returned,
 * or NULL on any error. The first byte is the tag of this octet string
 * followed by the length of the octet string and the octets itself.
 */

u_char*
ASN1_EncodeOctetString (packet, packetlen, tag, octets, len)
     u_char	*packet;
     int	*packetlen;
     u_char	tag;
     u_char	*octets;
     int	len;
{
    int	i, asnlen = 0;
    u_char *length, *op = octets;

    if (! packet) {
	return packet;
    }

    *packet++  = tag;
    *packetlen += 1;
    length     = packet++;   
    *packetlen += 1;

    for (i = 0; i < len; i++) {
	*packet++ = *op++;
    }
    *packetlen += len;
    asnlen     += len;

    packet = ASN1_EncodeLength (packet, packetlen, length, asnlen);
    return packet;
}



/*
 * ASN1_DecodeOctetString() decodes an octet string. On success, a
 * pointer to the first byte of the octet string is returned and
 * the length is written to len. It is up to the caller to copy the
 * octets in private memory if the packet itself is cleared. The 
 * first byte in the packet must match the tag given by type followed 
 * by the length of the octet string and the octets themself.
 */

u_char*
ASN1_DecodeOctetString (packet, packetlen, tag, octets, len)
     u_char		*packet;
     int		*packetlen;
     u_char		tag;
     u_char		**octets;
     int		*len;
{
    u_int asnlen;

    if (! packet) {
        return packet;
    }

    if (*packet++ != tag) {
	sprintf (error, "invalid tag: 0x%.2x, expecting 0x%.2x",
		 *--packet, tag );
	return NULL;
    }
    *packetlen += 1;

    /*
     * Handle zero length octet string of the form 0x04 0x00.
     */

    packet = ASN1_DecodeLength (packet, packetlen, &asnlen);
    if (packet == NULL) return NULL;

    if (octets) {
        *octets = packet;
	*len = asnlen;
    }

    *packetlen += asnlen;
    return (packet + asnlen);
}


/*
 * ASN1_EncodeNull() encodes a NULL value.  On success, a pointer to
 * the first byte past the end of this object is returned, or NULL on
 * any error.
 */

u_char*
ASN1_EncodeNull (packet, packetlen, tag)
     u_char 	*packet;
     int	*packetlen;
     u_char	tag;
{
    if (! packet) {
	return packet;
    }

    *packet++  = tag;
    *packet++  = '\0';
    *packetlen += 2;
    return packet;
}


/*
 * ASN1_DecodeNull() decodes a NULL value. On success, a pointer to
 * the first byte past the end of this object is returned, or NULL on
 * any error. Check if the tag matches the required tag given in tag.
 */

u_char *
ASN1_DecodeNull (packet, packetlen, tag)
     u_char     *packet;
     int        *packetlen;
     u_char	tag;
{
    u_int asnlen;

    if (! packet) {
        return packet;
    }

    if (*packet++ != tag) {
	sprintf (error, "invalid tag: 0x%.2x, expecting 0x%.2x",
		 *--packet, tag);
	return NULL;
    }
    *packetlen += 1;

    packet = ASN1_DecodeLength (packet, packetlen, &asnlen);
    if (packet == NULL) return NULL;

    return (packet + asnlen);
}

