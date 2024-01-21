/*
 *  ftl.h 1.1 1995/05/23 19:52:23 (David Hinds)
 */

#ifndef _LINUX_FTL_H
#define _LINUX_FTL_H

typedef struct erase_unit_header_t {
    u_char	LinkTargetTuple[5];
    u_char	DataOrgTuple[10];
    u_char	NumTransferUnits;
    u_long	EraseCount;
    u_short	LogicalEUN;
    u_char	BlockSize;
    u_char	EraseUnitSize;
    u_short	FirstPhysicalEUN;
    u_short	NumEraseUnits;
    u_long	FormattedSize;
    u_long	FirstVMAddress;
    u_short	NumVMPages;
    u_char	Flags;
    u_char	Code;
    u_long	SerialNumber;
    u_long	AltEUHOffset;
    u_long	BAMOffset;
    u_char	Reserved[12];
    u_char	EndTuple[2];
} erase_unit_header_t;

/* Flags in erase_unit_header_t */
#define HIDDEN_AREA		0x01
#define REVERSE_POLARITY	0x02
#define DOUBLE_BAI		0x04

/* Definitions for block allocation information */

#define BLOCK_FREE(b)		((b) == 0xffffffff)
#define BLOCK_DELETED(b)	(((b) == 0) || ((b) == 0xfffffffe))

#define BLOCK_TYPE(b)		((b) & 0x7f)
#define BLOCK_ADDRESS(b)	((b) & ~0x7f)
#define BLOCK_NUMBER(b)		((b) >> 9)
#define BLOCK_CONTROL		0x30
#define BLOCK_DATA		0x40
#define BLOCK_REPLACEMENT	0x60
#define BLOCK_BAD		0x70

#endif /* _LINUX_FTL_H */
