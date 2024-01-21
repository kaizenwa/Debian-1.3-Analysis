/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: iiop.h,v 1.8 1996/05/31 18:23:58 janssen Exp $ */
/* Last tweaked by Mike Spreitzer November 7, 1993 9:08 pm PST */

#include <stdio.h>

#include <sys/types.h>

#include <fcntl.h>

struct IIOP_IOR_TaggedProfile {
  ilu_cardinal	tag;
  ilu_cardinal	profileDataLen;
  ilu_bytes	profileData;
};

struct IIOP_IOR_IOR {
  ilu_string	type_id;
  ilu_cardinal	nProfiles;
  struct IIOP_IOR_TaggedProfile	Profile[1];
};  

struct IIOP_DataBlock {
  ilu_cardinal major;
  ilu_cardinal minor;
};

#define IIOP_TAG_INTERNET_IOP		0
#define IIOP_TAG_ILU_IOP		0x494c5500
#define IIOP_TAG_ILU_SUNRPC_IOP		0x494c5501
#define IIOP_TAG_ILU_COURIER_IOP	0x494c5502
#define IIOP_TAG_ILU_UNUSED1_IOP	0x494c5503
#define IIOP_TAG_ILU_UNUSED2_IOP	0x494c5504
#define IIOP_TAG_ILU_UNUSED3_IOP	0x494c5505
#define IIOP_TAG_ILU_UNUSED4_IOP	0x494c5506
#define IIOP_TAG_ILU_UNUSED5_IOP	0x494c5507

#define GIOP_PacketType_Request		0
#define GIOP_PacketType_Reply		1
#define GIOP_PacketType_CancelRequest	2
#define GIOP_PacketType_LocateRequest	3
#define GIOP_PacketType_LocateReply	4
#define GIOP_PacketType_CloseConnection	5
#define GIOP_PacketType_MessageError	6

#define GIOP_ReplyStatusType_NO_EXCEPTION	0
#define GIOP_ReplyStatusType_USER_EXCEPTION	1
#define GIOP_ReplyStatusType_SYSTEM_EXCEPTION	2
#define GIOP_ReplyStatusType_LOCATION_FORWARD	3

enum IIOP_LocateStatus { IIOP_UNKNOWN_OBJECT, IIOP_OBJECT_HERE, IIOP_OBJECT_FORWARD };

enum byte_order { BigEndian, LittleEndian };

typedef struct ilu_packet_s {
  struct ilu_packet_methods_s *methods;
  ilu_Transport bs;
  ilu_byte *vop;
  enum byte_order byteorder;
  int ptype;
  ilu_bytes objKey;
  ilu_cardinal objKeyLen;
  ilu_cardinal size;
  ilu_bytes principal;
  ilu_cardinal principalLen;
} * PACKET;

struct ilu_packet_methods_s {
  /*for access and calling: L2 >= {call's conn's iomu};
			    L1, Main unconstrained*/

  void	(*put_s8) (PACKET p, signed char l, ILU_ERRS((IoErrs)) *);
  void	(*put_s16) (PACKET p, ilu_shortinteger l, ILU_ERRS((IoErrs)) *);
  void	(*put_s32) (PACKET p, ilu_integer l, ILU_ERRS((IoErrs)) *);
  void	(*put_s64) (PACKET p, ilu_longinteger *l, ILU_ERRS((IoErrs)) *);
  void	(*put_u8) (PACKET p, ilu_byte l, ILU_ERRS((IoErrs)) *);
  void	(*put_u16) (PACKET p, ilu_shortcardinal l, ILU_ERRS((IoErrs)) *);
  void	(*put_u32) (PACKET p, ilu_cardinal l, ILU_ERRS((IoErrs)) *);
  void	(*put_u64) (PACKET p, ilu_longcardinal *l, ILU_ERRS((IoErrs)) *);
  void	(*put_r32) (PACKET p, ilu_shortreal l, ILU_ERRS((IoErrs)) *);
  void	(*put_r64) (PACKET p, ilu_real l, ILU_ERRS((IoErrs)) *);
  void	(*put_r128) (PACKET p, ilu_longreal l, ILU_ERRS((IoErrs)) *);
  void	(*put_bytes) (PACKET p, ilu_bytes bytes, ilu_cardinal l, ILU_ERRS((IoErrs)) *);
  void	(*put_opaque) (PACKET p, ilu_bytes bytes, ilu_cardinal l, ILU_ERRS((IoErrs)) *);

  void	(*get_s8) (PACKET p, signed char *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_s16) (PACKET p, ilu_shortinteger *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_s32) (PACKET p, ilu_integer *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_s64) (PACKET p, ilu_longinteger *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_u8) (PACKET p, ilu_bytes l, ILU_ERRS((IoErrs)) * err);
  void	(*get_u16) (PACKET p, ilu_shortcardinal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_u32) (PACKET p, ilu_cardinal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_u64) (PACKET p, ilu_longcardinal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_r32) (PACKET p, ilu_shortreal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_r64) (PACKET p, ilu_real *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_r128) (PACKET p, ilu_longreal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_bytes) (PACKET p, ilu_bytes *bytes, ilu_cardinal *l, ilu_cardinal max_size, ILU_ERRS((IoErrs)) * err);
  void	(*get_opaque) (PACKET p, ilu_bytes *bytes, ilu_cardinal l, ILU_ERRS((IoErrs)) * err);

  void	(*destroy) (PACKET p, ilu_Error *err);
};

/*L2 >= {call's conn's iomu}; L1, Main unconstrained*/

#define packet_put_s8(p,b,e)		((*((p)->methods)->put_s8)((p),(b),(e)))
#define packet_put_s16(p,b,e)		((*((p)->methods)->put_s16)((p),(b),(e)))
#define packet_put_s32(p,b,e)		((*((p)->methods)->put_s32)((p),(b),(e)))
#define packet_put_s64(p,b,e)		((*((p)->methods)->put_s64)((p),(b),(e)))
#define packet_put_u8(p,b,e)	((*((p)->methods)->put_u8)((p),(b),(e)))
#define packet_put_u16(p,b,e)	((*((p)->methods)->put_u16)((p),(b),(e)))
#define packet_put_u32(p,b,e)	((*((p)->methods)->put_u32)((p),(b),(e)))
#define packet_put_u64(p,b,e)	((*((p)->methods)->put_u64)((p),(b),(e)))
#define packet_put_r32(p,b,e)		((*((p)->methods)->put_r32)((p),(b),(e)))
#define packet_put_r64(p,b,e)	((*((p)->methods)->put_r64)((p),(b),(e)))
#define packet_put_r128(p,b,e)	((*((p)->methods)->put_r128)((p),(b),(e)))
#define packet_put_bytes(p,b,l,e)	((*((p)->methods)->put_bytes)((p),(b),(l),(e)))
#define packet_put_opaque(p,b,l,e)	((*((p)->methods)->put_opaque)((p),(b),(l),(e)))

#define packet_get_s8(p,b,e)	((*((p)->methods)->get_s8)((p),(b),(e)))
#define packet_get_s16(p,b,e)	((*((p)->methods)->get_s16)((p),(b),(e)))
#define packet_get_s32(p,b,e)	((*((p)->methods)->get_s32)((p),(b),(e)))
#define packet_get_s64(p,b,e)	((*((p)->methods)->get_s64)((p),(b),(e)))
#define packet_get_u8(p,b,e)	((*((p)->methods)->get_u8)((p),(b),(e)))
#define packet_get_u16(p,b,e)	((*((p)->methods)->get_u16)((p),(b),(e)))
#define packet_get_u32(p,b,e)	((*((p)->methods)->get_u32)((p),(b),(e)))
#define packet_get_u64(p,b,e)	((*((p)->methods)->get_u64)((p),(b),(e)))
#define packet_get_r32(p,b,e)	((*((p)->methods)->get_r32)((p),(b),(e)))
#define packet_get_r64(p,b,e)	((*((p)->methods)->get_r64)((p),(b),(e)))
#define packet_get_r128(p,b,e)	((*((p)->methods)->get_r128)((p),(b),(e)))
#define packet_get_bytes(p,b,l,lim,e)	((*((p)->methods)->get_bytes)((p),(b),(l),(lim),(e)))
#define packet_get_opaque(p,b,l,e)	((*((p)->methods)->get_opaque)((p),(b),(l),(e)))

#define packet_destroy(p,e)	((*((p)->methods)->destroy)((p),(e)))

#define iiop_packet(x)		((PACKET)((x)->ca_prdata2))
#define iiop_set_packet(x,p)	((x)->ca_prdata2 = ((ilu_refany)(p)))
#define iiop_packetType(x)	(iiop_packet(x)->ptype)
#define iiop_objKey(x)		(iiop_packet(x)->objKey)
#define iiop_objKeyLen(x)	(iiop_packet(x)->objKeyLen)
#define iiop_size(x)		(iiop_packet(x)->size)
#define iiop_vop(x)		(iiop_packet(x)->vop)
#define iiop_incr_vop(x,n)	((iiop_packet(x)->vop)+=(n))
#define iiop_byte_order(x)	(iiop_packet(x)->byteorder)
#define iiop_principal(x)	(iiop_packet(x)->principal)
#define iiop_principalLen(x)	(iiop_packet(x)->principalLen)
#define iiop_major_version(x)	(((struct IIOP_DataBlock *)(call_connection(x)->co_protocol_data))->major)
#define iiop_minor_version(x)	(((struct IIOP_DataBlock *)(call_connection(x)->co_protocol_data))->minor)

#define ODD(x)		(((x)&0x1)!=0)
#define EVEN(x)		(((x)&0x1)==0)  

/* These macros assume that a pointer and an int are the same size; this is not true under WIN16. */

#if defined(WIN16)
#define PAD2(x)		(2-(((unsigned long)(x))&0x1))
#define PAD4(x)		(4-(((unsigned long)(x))&0x3))
#define PAD8(x)		(8-(((unsigned long)(x))&0x7))

#define PADDED_PTR(p,x)		(((unsigned long)((p)+((x)-1)))&(~(((unsigned long)(x))-1)))
#define PADDING_NEC(p,x)	(PADDED_PTR((p),(x))-((unsigned long)(p)))
#define PADDED(x,p)		(PADDED_PTR((p),(x))-((unsigned long)(p))+(x))

#else
#define PAD2(x)		(2-(((unsigned)(x))&0x1))
#define PAD4(x)		(4-(((unsigned)(x))&0x3))
#define PAD8(x)		(8-(((unsigned)(x))&0x7))

#define PADDED_PTR(p,x)		(((unsigned)((p)+((x)-1)))&(~(((unsigned)(x))-1)))
#define PADDING_NEC(p,x)	(PADDED_PTR((p),(x))-((unsigned)(p)))
#define PADDED(x,p)		(PADDED_PTR((p),(x))-((unsigned)(p))+(x))
#endif

#define PACKET_ADJUST(p,a)	(p) = (unsigned char *)PADDED_PTR((p),(a))
#define PACKET_INCR(p,x,a)	((p)->vop += (PADDING_NEC((p)->vop, (a)) + (x)))

#define PTR_ADJUST(x,p,a)	((x)+=PADDING_NEC((p)->vop,(a)))

#define PACKET_OBTAIN(p,x,a,e)	(transport_get_input_buffer((p)->bs, PADDING_NEC((p)->vop,(a)) + (x), (e)))
#define PACKET_ALLOC(p,x,a,e)	(transport_get_output_buffer((p)->bs, PADDING_NEC((p)->vop,(a)) + (x), (e)))
#define PACKET_READ(p,b,l,e)	(transport_read_bytes((p)->bs, (b), (l), (e)))
#define PACKET_WRITE(p,b,l,e)	(transport_write_bytes((p)->bs, (b), (l), (e)))

#define MEMCPY(to,from,len)	memcpy((void *)(to),(void *)(from),(len))

#ifdef WORDS_BIGENDIAN
#define NATIVE_BYTE_ORDER BigEndian
#else
#define NATIVE_BYTE_ORDER LittleEndian
#endif

#define ENDIAN_MATCH(p)		((p)->byteorder == NATIVE_BYTE_ORDER)

#define iiop_transport(x)	((x)->ca_prTrans)
