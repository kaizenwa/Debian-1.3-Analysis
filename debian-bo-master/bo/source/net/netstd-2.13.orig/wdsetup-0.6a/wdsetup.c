/* wdsetup.c: A WD and SMC ethernet card configurer for linux. */
/*
    Written 1993 by Gregg Weber. This is alpha test code.
    This is an extension to the Linux operating system, and is covered by
    same Gnu Public License that covers that work.

    version 0.3 3-mar-1993 gw added command line configuration stuff 
    version 0.4 15-mar-1993 gw added twisted pair support 
    version 0.5 19-mar-1993 gw fixed bug in wrongly id 8003 board with
	interface chip as 8013.
    version 0.6 28-april-1992 gw updated based on new lmgetcnfg code from SMC
*/
    
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#define wdsetup_version "0.6a"
#define PERM_OFF 0
#define PERM_ON 1
#define WD_JUMPERS 6

#undef FACTORY_INIT

unsigned char eedata[8];
int verbose;
unsigned char ConfigZero[8];
int cnfg_val;
static unsigned char irqlist[8] = {9,3,5,7,10,11,15,4};


/*--------------- General Equates -----------------*/
#define CHECKSUM	0x0ff
#define RECALL_DATA	0
#define RECALL_LANADDR	1

/******************************
 Adapter Structure Definition
******************************/

typedef struct
{  
  int	adapter_num;		/* logical adapter no. */
  int	pc_bus;			/* Bus type */
  int	io_base;		/* I/O base address */
  char	adapter_name[12];
  int	irq_value;   		/* IRQ line used by hardware */
  int	rom_size;		/* num of 1024 byte blocks */
  int	rom_base;		/* physical address of ROM */
  int	rom_access;		/* seg:off value to access ROM */
  int	ram_size;		/* num of 1024 byte blocks */
  int	ram_base;		/* physical address of RAM */
  int	ram_access;		/* seg:off value to access RAM */
  int	ram_usable;		/* num of 1024 byte blocks (window size) */
  int	io_base_new;		/* new base I/O Addr (for PutCnfg) */
  char	node_address[6]; 	/* network address */
  int	max_packet_size;	/* max pkt size hardware supports */
  int	num_of_tx_buffs;	/* TX buffs available in hardware */
  int	media_type;		/* BNC, AUI, UTP_4, etc. */
  int	adapter_bus;
  int	pos_id;			/* Adapter POS ID (MCA only) */
  int	adapter_flags;
  int	slot_num;		/* Micro Channel slot number */

/* -----------------...Local vars for each adapter...----------------------*/

  int	bic_type;
  int	nic_type;
  int	board_id;		/* WDM defined Board ID */
  int	extra_info;		/* WDM defined Extra Board Info */
  int	full_bid;		/* 32 bit board info */
  int	mode_bits;		/* mode bits for adapter */
  int	status_bits;
  int	config_mode;		/*  1=store config in EEROM */
  int	page_offset_mask;
  int	clone;			/* 1=card is a clone */
} CNFG_Adapter;


/**************
 Return Codes
**************/

#define SUCCESS			0x0000	/* this code is defined by NDIS spec */
#define ADAPTER_AND_CONFIG	0x0001
#define ADAPTER_NO_CONFIG	0x0002
#define UNKNOWN_ADAPTER		0x0086
#define CONFIG_ERROR		0x0087
#define CONFIG_WARNING		0x0088
#define NO_FIXED_CNFG		0x0089
#define EEROM_CKSUM_ERROR	0x008A
#define ADAPTER_NOT_FOUND	0x0ffff
#define ILLEGAL_FUNCTION	INVALID_FUNCTION

/*
 Bit-Mapped codes returned in cnfg_val if return code from LM_GET_CONFIG is
 CONFIG_ERROR or CONFIG_WARNING:
*/

/* Errors */
#define IO_BASE_INVALID		0x0001
#define IO_BASE_RANGE		0x0002
#define IRQ_INVALID		0x0004
#define IRQ_RANGE		0x0008
#define RAM_BASE_INVALID	0x0010
#define RAM_BASE_RANGE		0x0020
#define RAM_SIZE_RANGE		0x0040

/* Warnings */
#define IRQ_MISMATCH		0x0080
#define RAM_BASE_MISMATCH	0x0100
#define RAM_SIZE_MISMATCH	0x0200


/*
 Definitions for the field BUS_TYPE
*/
#define AT_BUS			0x00
#define MCA_BUS			0x01
#define EISA_BUS		0x02
#define PCMCIA_BUS		0x03

/**********************
 MODE BIT DEFINITIONS
**********************/

#define INTERRUPT_STATUS_BIT	0x8000	/* PC Interrupt Line: 0=Not Enabled */
#define BOOT_STATUS_MASK	0x6000	/* Mask to isolate BOOT_STATUS */
#define BOOT_INHIBIT		0x0000	/* BOOT_STATUS is 'inhibited' */
#define BOOT_TYPE_1		0x2000	/* Unused BOOT_STATUS value */
#define BOOT_TYPE_2		0x4000	/* Unused BOOT_STATUS value */
#define BOOT_TYPE_3		0x6000	/* Unused BOOT_STATUS value */
#define ZERO_WAIT_STATE_MASK	0x1800	/* Mask to isolate Wait State flags */
#define ZERO_WAIT_STATE_8_BIT	0x1000	/* 0=Disabled (Inserts Wait States) */
#define ZERO_WAIT_STATE_16_BIT	0x0800	/* 0=Disabled (Inserts Wait States) */
#define BNC_INTERFACE		0x0400	/* net defs for adv. feature adapters*/
#define AUI_INTERFACE		0x0300	
#define AUI_10BT_INTERFACE	0x0200	
#define STARLAN_10_INTERFACE	0x0100	
#define INTERFACE_TYPE_MASK	0x0700	/* Mask value for interface type */

/****************************************************************************
 Definitions for the field:
 media_type
 TP = Twisted Pair
 STP = Shielded twisted pair
 UTP = Unshielded twisted pair
*****************************************************************************/
#define CNFG_MEDIA_TYPE_MASK	0x03	/* bits 0-1 */

#define MEDIA_S10		0x00000	/* Ethernet adapter, TP. */
#define MEDIA_AUI_UTP		0x00001	/* Ethernet adapter, AUI/UTP media. */
#define MEDIA_BNC		0x00002	/* Ethernet adapter, BNC media. */
#define MEDIA_AUI		0x00003	/* Ethernet adapter, AUI media. */
#define MEDIA_STP_16		0x00004	/* TokenRing adap, 16Mbit STP. */
#define MEDIA_STP_4		0x00005	/* TokenRing adap, 4Mbit STP. */
#define MEDIA_UTP_16		0x00006	/* TokenRing adap, 16Mbit UTP. */
#define MEDIA_UTP_4		0x00007	/* TokenRing adap, 4Mbit UTP. */
#define MEDIA_UNKNOWN		0x0FFFF	/* Unknown adapter/media type */

/****************************************************************************
 Definitions for the field:
 bic_type (Bus interface chip type)
*/
#define BIC_NO_CHIP		0x00000	/* Bus interface chip not implemented */
#define BIC_583_CHIP		0x00001	/* 83C583 bus interface chip */
#define BIC_584_CHIP		0x00002	/* 83C584 bus interface chip */
#define BIC_585_CHIP		0x00003	/* 83C585 bus interface chip */
#define BIC_593_CHIP		0x00004	/* 83C593 bus interface chip */
#define BIC_594_CHIP		0x00005	/* 83C594 bus interface chip */
#define BIC_564_CHIP		0x00006	/* PCMCIA bus interface chip */
#define BIC_790_CHIP		0x00007	/* 83C790 bus i-face/Ethernet NIC chip */

/****************************************************************************
 Definitions for the field:
 nic_type (Bus interface chip type)
*/
#define NIC_UNK_CHIP		0x00000	/* Unknown NIC chip */
#define NIC_8390_CHIP		0x00001	/* DP8390 Ethernet NIC */
#define NIC_690_CHIP		0x00002	/* 83C690 Ethernet NIC */
#define NIC_825_CHIP		0x00003	/* 83C825 Token Ring NIC */
/* #define NIC_???_CHIP		0x00004	 Not used */
/* #define NIC_???_CHIP		0x00005	 Not used */
/* #define NIC_???_CHIP		0x00006	 Not used */
#define NIC_790_CHIP		0x00007	/* 83C790 bus i-face/Ethernet NIC chip */

/****************************************************************************
 Definitions for the field:
 adapter_type	The adapter_type field describes the adapter/bus
		configuration.
*/
#define BUS_UNK_TYPE		0x00000
#define BUS_ISA16_TYPE		0x00001	/* 16 bit adap in 16 bit (E)ISA slot */
#define BUS_ISA8_TYPE		0x00002	/* 8/16b adap in 8 bit XT/(E)ISA slot */
#define BUS_MCA_TYPE		0x00003	/* Micro Channel adapter */
#define BUS_EISA32M_TYPE	0x00004	/* EISA 32 bit bus master adapter */
#define BUS_EISA32S_TYPE	0x00005	/* EISA 32 bit bus slave adapter */
#define BUS_PCMCIA_TYPE		0x00006	/* PCMCIA Bus */

	   
/*******************************************
 config_mode defs
*******************************************/

#define STORE_EEROM		0x00001	/* Store config in EEROM. */
#define STORE_REGS		0x00002	/* Store config in register set. */

/* Adapter POS ID's */

#define CNFG_ID_8003E	       	0x6fc0
#define CNFG_ID_8003S	       	0x6fc1
#define CNFG_ID_8003W	       	0x6fc2
#define CNFG_ID_8115TRA	       	0x6ec6		/* Token Ring MCA */ 
#define CNFG_ID_8013E	       	0x61C8
#define CNFG_ID_8013W	       	0x61C9
#define CNFG_ID_BISTRO03E      	0x0EFE5
#define CNFG_ID_BISTRO13E      	0x0EFD5
#define CNFG_ID_BISTRO13W      	0x0EFD4

/* 583 & 584 registers, needed for getinfo */

#define CNFG_MSR_583	       	0
#define CNFG_ICR_583	       	1
#define CNFG_IAR_583	       	2
#define CNFG_BIO_583	       	3
#define CNFG_EAR_583            3
#define CNFG_IRR_583	       	4
#define CNFG_LAAR_584	       	5
#define CNFG_GP2	       	7
#define CNFG_LAAR_MASK	       	0x01F
#define CNFG_LAAR_ZWS	       	0x020
#define CNFG_LAAR_L16E	       	0x040
#define CNFG_ICR_IR2_584       	0x04
#define CNFG_ICR_MASK           0x08
#define CNFG_ICR_MSZ            0x08
#define CNFG_ICR_RLA            0x010
#define CNFG_ICR_STO            0x080
#define CNFG_IRR_IRQS	       	0x060
#define CNFG_IRR_IEN	       	0x080
#define CNFG_IRR_ZWS	       	0x01
#define CNFG_GP2_BOOT_NIBBLE   	0x0F
#define CNFG_IRR_OUT2           0x04
#define CNFG_IRR_OUT1           0x02

#define CNFG_SIZE_8kb	       	8
#define CNFG_SIZE_16kb	       	16
#define CNFG_SIZE_32kb	       	32
#define CNFG_SIZE_64kb	       	64

#define ROM_DISABLE	       	0

#define CNFG_SLOT_ENABLE_BIT   	0x08

#define CNFG_POS_CONTROL_REG   	0x096
#define CNFG_POS_REG0	       	0x100
#define CNFG_POS_REG1	       	0x101
#define CNFG_POS_REG2	       	0x102
#define CNFG_POS_REG3	       	0x103
#define CNFG_POS_REG4	       	0x104
#define CNFG_POS_REG5	       	0x105

#define CNFG_ADAPTER_TYPE_MASK 	0x00e
/*****************************************************************************
*	General register definitions for identifying board types
******************************************************************************/
#define REG_IJR      	0x06
#define LAR0      	0x08	/*LAN address ROM registers */
#define LAR1      	0x09
#define LAR2      	0x0A
#define LAR3      	0x0B
#define LAR4      	0x0C
#define LAR5      	0x0D

#define BID_BOARD_ID_BYTE      	0x0E
#define BID_CHCKSM_BYTE	       	0x0F
#define REG_CKSM	0x0F

#define BID_LAR_OFFSET 	0x08	/*offset for aliasing check */

/******************************************************************************
*	General definitions
******************************************************************************/
#define BID_MSZ_583_BIT	       	0x08
#define BID_SIXTEEN_BIT_BIT    	0x01

/******************************************************************************
*	Mask for extracting the board revision number
******************************************************************************/
#define BID_BOARD_REV_MASK     	0x1E

/*****************************************************************************
*	Definitions for board rev numbers greater that 1
******************************************************************************/
#define BID_MEDIA_TYPE_BIT     	0x01
#define BID_SOFT_CONFIG_BIT    	0x20
#define BID_RAM_SIZE_BIT       	0x40
#define BID_BUS_TYPE_BIT       	0x80

/*****************************************************************************
*	Defs for identifying the 690
******************************************************************************/
#define BID_CR	       	0x10		/* Command Register */
#define BID_TXP	       	0x04		/* Transmit Packet Command */
#define BID_TCR_DIFF   	0x0D		/* Transmit Configuration Register */
#define BID_TCR_VAL    	0x18		/* Value to Test 8390 or 690 */
#define BID_PS0	       	0x00		/* Register Page Select 0 */
#define BID_PS1	       	0x40		/* Register Page Select 1 */
#define BID_PS2	       	0x80		/* Register Page Select 2 */
#define BID_PS_MASK    	0x3F		/* For Masking Off Page Select Bits */

/*****************************************************************************
*	Defs for manipulating the 584
******************************************************************************/
#define BID_EEPROM_0		       	0x08
#define BID_EEPROM_1		       	0x09
#define BID_EEPROM_2		       	0x0A
#define BID_EEPROM_3		       	0x0B
#define BID_EEPROM_4		       	0x0C
#define BID_EEPROM_5		       	0x0D
#define BID_EEPROM_6		       	0x0E
#define BID_EEPROM_7		       	0x0F

#define BID_OTHER_BIT		       	0x02
#define BID_ICR_MASK		       	0x0C
#define BID_EAR_MASK		       	0x0F
#define BID_ENGR_PAGE		       	0x0A0
#define BID_RLA			       	0x10
#define BID_EA6			       	0x80
#define BID_RECALL_DONE_MASK	       	0x010
#define BID_EEPROM_OVERRIDE	       	0xFFD0FFB0
#define BID_BID_EEPROM_OVERRIDE	       	0x0FFB0
#define BID_EXTRA_EEPROM_OVERRIDE      	0x0FFD0
#define BID_EEPROM_MEDIA_MASK	       	0x07
#define BID_STARLAN_TYPE	       	0x00
#define BID_ETHERNET_TYPE	       	0x01
#define BID_TP_TYPE		       	0x02
#define BID_EW_TYPE		       	0x03
#define BID_EEPROM_IRQ_MASK	       	0x18
#define BID_PRIMARY_IRQ		       	0x00
#define BID_ALTERNATE_IRQ_1	       	0x08
#define BID_ALTERNATE_IRQ_2	       	0x10
#define BID_ALTERNATE_IRQ_3	       	0x18
#define BID_EEPROM_RAM_SIZE_MASK       	0x0E0
#define BID_EEPROM_RAM_SIZE_RES1       	0x00
#define BID_EEPROM_RAM_SIZE_RES2       	0x20
#define BID_EEPROM_RAM_SIZE_8K	       	0x40
#define BID_EEPROM_RAM_SIZE_16K	       	0x60
#define BID_EEPROM_RAM_SIZE_32K	       	0x80
#define BID_EEPROM_RAM_SIZE_64K	       	0x0A0
#define BID_EEPROM_RAM_SIZE_RES3       	0x0C0
#define BID_EEPROM_RAM_SIZE_RES4       	0x0E0
#define BID_EEPROM_BUS_TYPE_MASK       	0x07
#define BID_EEPROM_BUS_TYPE_AT	       	0x00
#define BID_EEPROM_BUS_TYPE_MCA	       	0x01
#define BID_EEPROM_BUS_TYPE_EISA       	0x02
#define BID_EEPROM_BUS_SIZE_MASK       	0x18
#define BID_EEPROM_BUS_SIZE_8BIT       	0x00
#define BID_EEPROM_BUS_SIZE_16BIT      	0x08
#define BID_EEPROM_BUS_SIZE_32BIT      	0x10
#define BID_EEPROM_BUS_SIZE_64BIT      	0x18
#define BID_EEPROM_RAM_PAGING	       	0x40
#define BID_EEPROM_ROM_PAGING	       	0x80
#define BID_EEPROM_PAGING_MASK	       	0x0C0

#define OFFSET_585_ENGR_DATA	       	0x00A
#define OFFSET_585_LAN_ADDR	       	0x006
/*****************************************************************************
*	Defs for local variables
******************************************************************************/
#define BID_LOCAL_BID	       	2
#define BID_LOCAL_EXTRA	       	4
#define BID_LOCAL_CR	       	2
#define BID_LOCAL_TCR	       	4

/* board_id stuff */
#define STARLAN_MEDIA	       	0x0001		/* StarLAN */
#define ETHERNET_MEDIA	       	0x0002		/* Ethernet */
#define TWISTED_PAIR_MEDIA     	0x0003		/* Twisted Pair */
#define EW_MEDIA	       	0x0004		/* Ethernet and Twisted Pair */
#define TOKEN_MEDIA	       	0x0005		/* Token Ring Media */
#define MICROCHANNEL	       	0x0008		/* MicroChannel Adapter */
#define INTERFACE_CHIP	       	0x0010		/* Soft Config Adapter */
#define ADVANCED_FEATURES      	0x0020	/* Adv. netw. interface features */
#define BOARD_16BIT	       	0x0040		/* 16 bit capability */
#define PAGED_RAM	       	0x0080		/* Adapter has paged RAM */
#define PAGED_ROM	       	0x0100		/* Adapter has paged ROM */
#define MEDIA_MASK	       	0x0007		/* Isolates Media Type */
#define PCM_ADAPTER	       	0x0200		/* PCMCIA Adapter */

#define RAM_SIZE_UNKNOWN       	0x00000000	/* Unknown RAM size */
#define RAM_SIZE_RESERVED_1    	0x00010000	/* Reserved RAM size */
#define RAM_SIZE_8K	       	0x00020000	/* 8k RAM */
#define RAM_SIZE_16K	       	0x00030000	/* 16k RAM */
#define RAM_SIZE_32K	       	0x00040000	/* 32k RAM */
#define RAM_SIZE_64K	       	0x00050000	/* 64k RAM */
#define RAM_SIZE_RESERVED_6    	0x00060000	/* Reserved RAM size */
#define RAM_SIZE_RESERVED_7    	0x00070000	/* Reserved RAM size */
#define SLOT_16BIT	       	0x00080000	/* 16 bit board-16 bit slot */
#define NIC_690_BIT	       	0x00100000	/* NIC is 690 */
#define ALTERNATE_IRQ_BIT      	0x00200000	/* Alternate IRQ is used */
#define INTERFACE_5X3_CHIP     	0x00000000	/* 0000 = 583 or 593 chips */
#define INTERFACE_584_CHIP     	0x00400000	/* 0001 = 584 chip */
#define INTERFACE_594_CHIP     	0x00800000	/* 0010 = 594 chip */
#define INTERFACE_585_CHIP     	0x01000000	/* 0100 = 585/790 chip */
#define RAM_SIZE_MASK	       	0x00070000	/* Isolates RAM Size */
#define INTERFACE_CHIP_MASK    	0x03C00000	/* Isolates Intfc Chip Type */
#define NIC_825_BIT	       	0x04000000	/* TRC 83C825 NIC */
#define NIC_790_BIT	       	0x08000000	/* NIC is 83C790 Ethernet */
#define STATIC_ID_MASK		0x0000ffff
/*****************************************************************************
*	Full board type definitions
******************************************************************************/

#define WD8003E	       	ETHERNET_MEDIA
#define WD8003EBT      	WD8003E
#define WD8003S	       	STARLAN_MEDIA
#define WD8003SH       	WD8003S
#define WD8003WT       	TWISTED_PAIR_MEDIA
#define WD8003W	       	(TWISTED_PAIR_MEDIA | INTERFACE_CHIP)
#define WD8003EB       	(ETHERNET_MEDIA | INTERFACE_CHIP)
#define WD8003EP       	WD8003EB	/* with INTERFACE_584_CHIP */
#define WD8003EW       	(EW_MEDIA | INTERFACE_CHIP)
#define WD8003ETA      	(ETHERNET_MEDIA | MICROCHANNEL)
#define WD8003STA      	(STARLAN_MEDIA | MICROCHANNEL)
#define WD8003EA       	(ETHERNET_MEDIA | MICROCHANNEL | INTERFACE_CHIP)
#define WD8013EPA      	WD8003EA	/* with INTERFACE_594_CHIP */
#define WD8003SHA      	(STARLAN_MEDIA | MICROCHANNEL | INTERFACE_CHIP)
#define WD8003WA       	(TWISTED_PAIR_MEDIA | MICROCHANNEL | INTERFACE_CHIP)
#define WD8013WPA      	WD8003WA	/* with INTERFACE_594_CHIP */
#define WD8013EBT      	(ETHERNET_MEDIA | BOARD_16BIT)
#define WD8013EB       	(ETHERNET_MEDIA | BOARD_16BIT | INTERFACE_CHIP)
#define WD8013W	       	(TWISTED_PAIR_MEDIA | BOARD_16BIT | INTERFACE_CHIP)
#define WD8013EW       	(EW_MEDIA | BOARD_16BIT | INTERFACE_CHIP)
#define WD8013EWC      	(WD8013EW | ADVANCED_FEATURES)
#define WD8013WC       	(WD8013W | ADVANCED_FEATURES)
#define WD8013EPC      	(WD8013EB | ADVANCED_FEATURES)
#define WD8003WC       	(WD8003W | ADVANCED_FEATURES)
#define WD8003EPC      	(WD8003EP | ADVANCED_FEATURES)
#define WD8115TA       	(TOKEN_MEDIA | MICROCHANNEL | INTERFACE_CHIP | PAGED_RAM)
#define WD8115T	       	(TOKEN_MEDIA | INTERFACE_CHIP | BOARD_16BIT | PAGED_RAM)
#define WD8203W	       	(WD8003WC | PAGED_ROM)
#define WD8203EP       	(WD8003EPC | PAGED_ROM)
#define WD8216T	       	(WD8013WC | PAGED_ROM | PAGED_RAM)
#define WD8216 	       	(WD8013EPC | PAGED_ROM | PAGED_RAM)
#define WD8216C	       	(WD8013EWC | PAGED_ROM | PAGED_RAM)
#define PCM10BT	       	(TWISTED_PAIR_MEDIA | PCM_ADAPTER | PAGED_RAM | ADVANCED_FEATURES)

/* Defs for PCM adapters */
#define REG_PCM_RESETDRV       	0x002
#define REG_PCM_GEN_CTRL       	0x003
#define GEN_CTRL_INTR	       	0x010
#define GEN_CTRL_IO_CARD       	0x020
#define GEN_CTRL_RST	       	0x040


#define REG_PCM_IO_START_LO    	0x008
#define REG_PCM_IO_START_HI    	0x009
#define REG_PCM_IO_STOP_LO     	0x00A
#define REG_PCM_IO_STOP_HI     	0x00B

#define REG_PCM_MEM_START_LO   	0x010
#define REG_PCM_MEM_START_HI   	0x011
#define REG_PCM_MEM_STOP_LO    	0x012
#define REG_PCM_MEM_STOP_HI    	0x013

#define REG_PCM_MEM_OFST_LO    	0x014
#define REG_PCM_MEM_OFST_HI    	0x015
#define MEM_ADD17	       	0x020	/* Memory Addr bit 17 aligned at 12. */

#define REG_PCM_WIN_ENABLE     	0x006
#define PCM_MEMWIN_EN0	       	0x001
#define PCM_MEMWIN_EN1	       	0x002
#define PCM_MEMWIN_EN2	       	0x004
#define PCM_MEMWIN_EN3	       	0x008
#define PCM_MEMWIN_EN4	       	0x010

#define PCM_IOWIN_EN0	       	0x040
#define PCM_IOWIN_EN1	       	0x080

#define PCM_MEM_16BIT	       	0x080
#define PCM_MEM_WAIT_STATE     	0x040



#define __SLOW_DOWN_IO __asm__ __volatile__("outb %al,$0x80")

#ifdef REALLY_SLOW_IO
#define SLOW_DOWN_IO { __SLOW_DOWN_IO; __SLOW_DOWN_IO; __SLOW_DOWN_IO; __SLOW_DOWN_IO; }
#else
#define SLOW_DOWN_IO __SLOW_DOWN_IO
#endif

inline void outb(char value, unsigned short port)
{
__asm__ __volatile__ ("outb %%al,%%dx"
		::"a" ((char) value),"d" ((unsigned short) port));
}

inline unsigned int inb(unsigned short port)
{
	unsigned int _v;
__asm__ __volatile__ ("inb %%dx,%%al"
		:"=a" (_v):"d" ((unsigned short) port),"0" (0));
	return _v;
}

inline void outb_p(char value, unsigned short port)
{
__asm__ __volatile__ ("outb %%al,%%dx"
		::"a" ((char) value),"d" ((unsigned short) port));
	SLOW_DOWN_IO;
}

inline unsigned int inb_p(unsigned short port)
{
	unsigned int _v;
__asm__ __volatile__ ("inb %%dx,%%al"
		:"=a" (_v):"d" ((unsigned short) port),"0" (0));
	SLOW_DOWN_IO;
	return _v;
}

void SelectOtherRegister(int ioaddr)
{
  outb_p((inb_p(ioaddr + CNFG_ICR_583) & 4) | 2,ioaddr + CNFG_ICR_583);
}

void RecallEERomData(int ioaddr, int flag)
{
  int temp;
  temp = (inb_p(ioaddr + CNFG_ICR_583) & 4) + 0x10;
  if (flag != RECALL_LANADDR) temp |= 2; /* set other bit if recall data */
  outb_p(temp, ioaddr + CNFG_ICR_583);
  while (inb_p(ioaddr + CNFG_ICR_583) & 0x10); /* wait for recall to complete */
}

int SumEERomData(int ioaddr, int page, int nbytes)
{
  int i,temp;
  unsigned char cksum;
  cksum = 0;
  SelectOtherRegister(ioaddr);
  outb_p((inb_p(ioaddr + CNFG_BIO_583) & 0xf) | (page << 4),ioaddr + CNFG_BIO_583);
  RecallEERomData(ioaddr, RECALL_DATA);
  for (i = 0; i < nbytes; i++) {
    temp = inb_p(ioaddr + LAR0 + i);
    cksum += temp;
    if (verbose >= 5)
      printf("sumeeromdata page %d byte= %x\n",page, temp);
  }
  RecallEERomData(ioaddr, RECALL_LANADDR);
  return((int) cksum);
}

int Read584Checksum(int ioaddr)
{
  int eepage;
  unsigned char cksum,cksumpage;
  cksum = 0;
  for (eepage = 0; eepage <= 15; eepage++)
    {
      cksumpage = SumEERomData(ioaddr, eepage, 8);
      if (verbose >= 4) printf("read584checksum page %d cksum= %x\n",
				    eepage, (int) cksumpage);
      cksum += cksumpage;
    }
  if (verbose >= 3) printf("read584checksum final cksum= %x\n",(int) cksum);
  return((cksum == CHECKSUM) ? SUCCESS : EEROM_CKSUM_ERROR);
}

void SaveConfigZero(int ioaddr)
{
  int i;
  SelectOtherRegister(ioaddr);
  outb_p(inb_p(ioaddr + CNFG_EAR_583) & 0xf, ioaddr + CNFG_EAR_583); /* page 0 */
  RecallEERomData(ioaddr, RECALL_DATA);
  for (i = 0; i < 8; i++) ConfigZero[i] =
    (unsigned char) inb_p(ioaddr + LAR0 + i);
}

void StoreEERomData(int ioaddr)
{
  outb_p((inb_p(ioaddr + CNFG_ICR_583) & 4) | 0x82, ioaddr + CNFG_ICR_583);
  while (inb_p(ioaddr + CNFG_ICR_583) & 0x80); /* wait for store to complete */
}

void WriteCheckSumByte(int ioaddr, int value)
{
  int i;
  SaveConfigZero(ioaddr);
  SelectOtherRegister(ioaddr);
  outb_p(inb_p(ioaddr + CNFG_EAR_583) | 0xf0, ioaddr + CNFG_EAR_583); /* page 16 */
  RecallEERomData(ioaddr, RECALL_DATA);
  outb_p(value, ioaddr + REG_CKSM);
  StoreEERomData(ioaddr);
  /* restore config 0, since it was corrupted by the above store */
  for (i = 0; i < 8; i++) outb_p(ConfigZero[i], ioaddr + LAR0 + i);
  outb_p(inb_p(ioaddr + CNFG_EAR_583) & 0xf, ioaddr + CNFG_EAR_583); /* page 0 */
  StoreEERomData(ioaddr);
  /* recall the lan address */
  RecallEERomData(ioaddr, RECALL_LANADDR);
}

int Write584Checksum(int ioaddr)
{
  int page;
  unsigned char cksum,cksumpage;
  cksum = 0;
  for (page = 0; page <= 14; page++)
    {
      cksumpage = SumEERomData(ioaddr, page, 8);
      cksum += cksumpage;
      if (verbose >= 4)
	printf("wr584checksum page %d, cksumpage= %x\n",page, (int) cksumpage);
    }
  cksumpage = SumEERomData(ioaddr, 15, 7);
  if (verbose >= 4)
    printf("write584ckecksum page %d cksumpage= %x\n",page,(int) cksumpage);
  cksum += cksumpage;
  cksum = CHECKSUM - cksum;
  if (verbose >= 3)
    printf("write584checksum final cksum= %x\n",(int) cksum);
  WriteCheckSumByte(ioaddr, cksum);
  return(SUCCESS);
}

/* getcnfg.c: this program gets config info from WD and SMC ether cards */
/*
    Written 1993 by Gregg Weber. This is alpha test code.
    This is a extension to the Linux operating system, and is covered by
    same Gnu Public License that covers that work.

	version 0.1 29-jan-93
	re-written for Linux in C, and Microchannel stuff removed
	by Gregg Weber

	version 0.2 31-jan-93
	added check for nic chip type

	version 0.3 19-mar-93
	fixed bug in get_board_id , wrongly identified 8003 as 8013 sometimes
	on boards with interface chips

	version 0.6 28-april-1993 gw
	updated based on new information from SMC
*/

int check_for_585(int ioaddr)
{
  /* TODO */
  return(0);
}

int bid_check_bic_type(int ioaddr)
{
 check_for_585(ioaddr);
 return(0);
}

/************************************************************************
  this determines if the nic chip is a 690 or 8390.
*************************************************************************/
int bid_check_for_690(int ioaddr)
{
  int i,oldcr,enhval,enhval2;
  int is690 = 0;
  oldcr = inb_p(ioaddr + 0x10) & 0xfb; /* get command register, mask txp */
  outb_p((oldcr & 0x3f) | 0x80,ioaddr + 0x10); /* select page 2 */
  enhval = inb_p(ioaddr + 0x17); /* get enh */
  outb_p(enhval + 0x18,ioaddr + 0x17); /* write 0x18 to enh */
  i = inb_p(ioaddr + 0x10); /* put something else on bus */
  enhval2 = inb_p(ioaddr + 0x17) & 0x18; /* get new enh */
  if (enhval2 == 0x18) is690 = 1;
  /* restore stuff */
  outb_p(enhval,ioaddr + 0x17); /* restore enh */
  outb_p(oldcr,ioaddr + 0x10); /* restore cr */
  return(is690);
}

int bid_get_board_rev_number(int ioaddr)
{
 return((inb_p(ioaddr + BID_BOARD_ID_BYTE) & BID_BOARD_REV_MASK) >> 1);
}

int bid_check_aliasing(int ioaddr)
{
  int i;
  /* see if there is register aliasing */
  /* only check regs 1,2,3,4,7 - some ASICs don't have regs 5 and 6
   and register 0 is never aliased */
  for (i = 1; i <= 4; i++) {
    if (inb_p(ioaddr+i) != inb_p(ioaddr+i+8)) {
      return(0);
    }
  }
  if (inb_p(ioaddr+7) != inb_p(ioaddr+7+8)) return(0);
  return (1);  
}

int bid_interface_chip(int ioaddr)
{
  int temp7,temp2;
  /* see if there is an interface chip */
  temp7 = inb_p(ioaddr+CNFG_GP2); /* save reg just in case */
  /* see if we can write and read some values in gp2 register */
  outb_p(0x35,ioaddr+CNFG_GP2); /* write something */
  temp2 = inb_p(ioaddr); /* put something else on bus */
  if (inb_p(ioaddr+CNFG_GP2) == 0x35) {
    outb_p(0x3a,ioaddr+CNFG_GP2); /* try another value */
    temp2 = inb_p(ioaddr); /* put something else on bus */
    if (inb_p(ioaddr+CNFG_GP2) == 0x3a) {
      outb_p(temp7,ioaddr+CNFG_GP2); /* restore reg just in case */
      return(1);
    }
  }
  return(check_for_585(ioaddr)); /* returns true if 585/790 is present */
}

/* is board 16 bit? */
int bid_board_16bit(int ioaddr)
{
  int temp7,temp1,temp2;
  temp7 = inb_p(ioaddr+CNFG_ICR_583); /* save value */
  /* flip the bit */
  temp1 = temp7 ^ BID_SIXTEEN_BIT_BIT;
  outb_p(temp1,ioaddr+CNFG_ICR_583); /* try to flip the 16-bit bit */
  temp2 = inb_p(ioaddr); /* put something else on bus */
  temp2 = inb_p(ioaddr+CNFG_ICR_583); /* read it back */
  temp1 = temp7 & BID_SIXTEEN_BIT_BIT; /* original state of bit */
  temp2 &= BID_SIXTEEN_BIT_BIT; /* current state of bit */
  if (temp1 == temp2) {
    /* new value didn't stick, so its 16 bit board */
    outb_p(temp7 & 0xfe,ioaddr+CNFG_ICR_583); /* restore it and clear bit 0 */
    return(1);
  }
  outb_p(temp7, ioaddr+CNFG_ICR_583); /* restore it just in case */
  return(0);
}

/* is 16 bit board in 16 bit slot? */
int bid_slot_16bit(int ioaddr)
{
  int temp1;
  if (check_for_585(ioaddr)) {
    return(0);
/* TODO   return((inb_p(ioaddr+REG_HWR) & HWR_HOST16) != 0); */
  }
  else {
    temp1 = inb_p(ioaddr+CNFG_ICR_583) & 0xc;
    outb_p(temp1,ioaddr+CNFG_ICR_583);
    return((inb_p(ioaddr+CNFG_ICR_583) & BID_SIXTEEN_BIT_BIT) != 0);
  }
}

int bid_get_base_info(int ioaddr)
{
  int temp = 0;
  if (!bid_check_aliasing(ioaddr)) {
    /* no aliasing, check for interface chip */
    if (bid_interface_chip(ioaddr)) return(INTERFACE_CHIP);
    /* no interface chip, check 16 bit */
    if (bid_board_16bit(ioaddr)) {
      temp |= BOARD_16BIT;
      if (bid_slot_16bit) temp |= SLOT_16BIT;
    }
  }
  return(temp);
}

/* find the media type */
int bid_get_media_type(int ioaddr,int board_rev)
{
  if ((inb_p(ioaddr+BID_BOARD_ID_BYTE) & BID_MEDIA_TYPE_BIT) != 0)
    return(ETHERNET_MEDIA);
  else {
    if (board_rev == 1) return(STARLAN_MEDIA);
    else return(TWISTED_PAIR_MEDIA);
  }
}

/* get board id byte info */
int bid_get_id_byte_info(int ioaddr, int board_id)
{
  int temp7;
  temp7 = inb_p(ioaddr+BID_BOARD_ID_BYTE);
  /* check for soft config bit */
  if ((temp7 & BID_SOFT_CONFIG_BIT) != 0) {
    temp7 = board_id & STATIC_ID_MASK;
    if ((temp7 == WD8003EB) || (temp7 == WD8003W))
      return(ALTERNATE_IRQ_BIT);
  }
  return(0);
}

void bid_wait_for_recall(int ioaddr)
{
  while (inb_p(ioaddr+1) & 0x10); /* wait for recall */
}

void bid_recall_engr_eeprom(int ioaddr)
{
  if (check_for_585(ioaddr)) {
/* TODO */
  }
  else {
    outb_p((inb_p(ioaddr+1) & 0xc) | 2,ioaddr+1); /* set other bit */
    outb_p((inb_p(ioaddr+3) & 0xf) | 0xa0,ioaddr+3); /* set engr page */
    outb_p((inb_p(ioaddr+1) & 0xc) | 0x12,ioaddr+1); /* set rla, other bit */
  }
  bid_wait_for_recall(ioaddr);
}

/* get eeprom info */
int bid_get_eeprom_info(int ioaddr)
{
  int temp_id,temp7;
  temp_id = 0;
  /* first recall the reserved engineering bytes from eeprom */
  bid_recall_engr_eeprom(ioaddr);
  temp7 = inb_p(ioaddr+BID_EEPROM_1);
  if ((temp7 & BID_EEPROM_BUS_TYPE_MASK) == BID_EEPROM_BUS_TYPE_MCA)
    temp_id |= MICROCHANNEL;
  if ((temp7 & BID_EEPROM_PAGING_MASK) != 0) {
    if ((temp7 & BID_EEPROM_RAM_PAGING) != 0)
      temp_id |= PAGED_RAM;
    if ((temp7 & BID_EEPROM_ROM_PAGING) != 0)
      temp_id |= PAGED_ROM;
  }
  if ((temp7 & BID_EEPROM_BUS_SIZE_MASK) == BID_EEPROM_BUS_SIZE_16BIT) {
    temp_id |= BOARD_16BIT;
    /* is 16 bit board in 16 bit slot? */
    if (bid_slot_16bit(ioaddr)) temp_id |= SLOT_16BIT;
  }
  temp7 = inb_p(ioaddr+BID_EEPROM_0);
  switch(temp7 & BID_EEPROM_MEDIA_MASK) {
  case BID_STARLAN_TYPE: temp_id |= STARLAN_MEDIA;
    break;
  case BID_TP_TYPE: temp_id |= TWISTED_PAIR_MEDIA;
    break;
  case BID_EW_TYPE: temp_id |= EW_MEDIA;
    break;
  default: temp_id |= ETHERNET_MEDIA;
    break;
  }
  if ((temp7 & 0x18) == 8) temp_id |= ALTERNATE_IRQ_BIT;
  switch(temp7 & 0xe0) {
  case 0x40: temp_id |= RAM_SIZE_8K;
    break;
  case 0x60: if ((temp_id & BOARD_16BIT) && !(temp_id & SLOT_16BIT))
    temp_id |= RAM_SIZE_8K;
  else temp_id |= RAM_SIZE_16K;
    break;
  case 0x80: temp_id |= RAM_SIZE_32K;
    break;
  case 0xa0: if ((temp_id & BOARD_16BIT) && !(temp_id & SLOT_16BIT))
    temp_id |= RAM_SIZE_32K;
  else temp_id |= RAM_SIZE_64K;
    break;
  default: temp_id |= RAM_SIZE_UNKNOWN;
    break;
  }
  /* now recall the lan address from eeprom */
  outb_p((inb_p(ioaddr+1) & 0xc) | 2,ioaddr+1); /* set other bit */
  outb_p((inb_p(ioaddr+3) & 0xf) | 0x80,ioaddr+3); /* set page */
  outb_p((inb_p(ioaddr+1) & 0xc) | 0x10,ioaddr+1); /* set rla */
  while (inb_p(ioaddr+1) & 0x10); /* wait for recall */
  return(temp_id);
}

/* get ram size */
int bid_get_ram_size(int ioaddr, int board_rev, int board_id)
{
  int temp1,temp2,temp_id;
  temp_id = 0;
  if (board_rev >= 2) {
    temp1 = inb_p(ioaddr+0xe); /* get hardware id byte */
    temp2 = board_id & STATIC_ID_MASK; /* get board type */
    if ((temp2 == WD8003E) || (temp2 == WD8003S) ||
	(temp2 == WD8003WT) || (temp2 == WD8003W) ||
	(temp2 == WD8003EB)) {
      /* hardware ram size bit determines 8K or 32K */
      temp_id |= (temp1 & 0x40) ? RAM_SIZE_32K : RAM_SIZE_8K;
    }
    else {
      if (temp2 == WD8013EBT) {
	if (board_id & SLOT_16BIT) {
	  /* hardware ram size bit determines 16K or 64K */
	  temp_id |= (temp1 & 0x40) ? RAM_SIZE_64K : RAM_SIZE_16K;
	}
	else {
	  /* hardware ram size bit determines 8K or 32K */
	  temp_id |= (temp1 & 0x40) ? RAM_SIZE_32K : RAM_SIZE_8K;
	}
      }
      else temp_id |= RAM_SIZE_UNKNOWN;
    }
  }
  else {
    /* old rev boards */
    if (board_id & BOARD_16BIT) {
      if (board_id & SLOT_16BIT) temp_id |= RAM_SIZE_16K;
      else temp_id |= RAM_SIZE_8K;
    }
    else {
      if (board_id & INTERFACE_CHIP) {
	/* look at memory size bit in register 1 */
	temp_id |= (inb_p(ioaddr+1) & 8) ? RAM_SIZE_32K : RAM_SIZE_8K;
      }
      /* can't determine ram size */
      else temp_id |= RAM_SIZE_UNKNOWN;
    }
  }
  return(temp_id);
}

/*
	get board id information.

	returns 0 if bad board
	else returns 32 bit word with bits encoding information about
	the board. See getcnfg.h for definitions of bits.
*/
unsigned long lm_gc_get_bid(int ioaddr)
{
  unsigned long board_id = 0;
  int board_rev;
  unsigned char temp1;
  /* rev number = 0 means board is broken */
  board_rev = bid_get_board_rev_number(ioaddr);
  if (board_rev == 0) return(0);
  board_id |= bid_get_base_info(ioaddr);
  board_id |= bid_get_media_type(ioaddr,board_rev);
  if (board_rev >= 2) {
    board_id |= bid_get_id_byte_info(ioaddr,board_id);
    if (board_rev >= 3) {
      board_id &= BID_EEPROM_OVERRIDE;
      board_id |=
	check_for_585(ioaddr) ? INTERFACE_585_CHIP : INTERFACE_584_CHIP;
      board_id |= bid_get_eeprom_info(ioaddr);
    }
    else {
      board_id |= bid_get_ram_size(ioaddr, board_rev, board_id);
    }
  }
  else {
    board_id |= bid_get_ram_size(ioaddr, board_rev, board_id);
  }
  if (board_rev >= 4) {
    board_id |= ADVANCED_FEATURES;
    temp1 = bid_check_bic_type(ioaddr);
    if (temp1) {
      board_id &= ~INTERFACE_CHIP_MASK;
      board_id |= temp1;
    }
    else {
      if (bid_check_for_690(ioaddr)) board_id |= NIC_690_BIT; /*check for 690*/
    }
  }
  else {
    if (bid_check_for_690(ioaddr)) board_id |= NIC_690_BIT; /*check for 690*/
  }
  return(board_id);
}

/* check if board is there via checksum */
/* return 0 if no board there,
   return 1 if board there */
int lm_gc_is_board_there(int ioaddr)
{
  int i;
  unsigned char csum = 0;
  if (ioaddr & 0x1f) return(0); /* check for illegal io address */
  for (i = 0; i< 8; i++) csum += inb_p(ioaddr + 8 + i);
  return(csum == 0xff); /* if bad checksum report no board */
}

/* test for clone */
int lm_verify_nadd(int ioaddr)
{
 if ((inb_p(ioaddr + LAR0) == 0) &&(inb_p(ioaddr + LAR1) == 0) &&
     (inb_p(ioaddr + LAR2) == 0xc0)) return(0);
 return(1);
}

/*
   get lots of configuration information about any western digital
   or SMC ethernet card.

   gets the following info:
   board id
   media type - starlan or ethernet or twisted pair
   interface chip type - none or 583 or 584 or 593 or 594
   NIC chip type - 8390 or 690
   8/16 bit board
   8/16 bit slot
   base address of ram
   size of ram
   interrupt line used
   base address of rom
   size of rom

   also returns the following values:

*/

int lm_get_at_config(CNFG_Adapter *cfg_info)
{
  unsigned long board_id,temp1;
  unsigned short base_addr;
  int irnum,i,rbase1;
  int ram_size,intf_chip,irq_value;

  cnfg_val = 0;
  base_addr = cfg_info->io_base;
  if (!lm_gc_is_board_there(base_addr)) return(ADAPTER_NOT_FOUND);
  board_id = lm_gc_get_bid(base_addr);
  cfg_info->board_id = board_id & STATIC_ID_MASK;
  cfg_info->full_bid = board_id;
  /* copy ram size from board_id to the config structure */
  temp1 = board_id & RAM_SIZE_MASK; /* get ram size bits */
  if ((temp1 >= RAM_SIZE_8K) && (temp1 <= RAM_SIZE_64K)) {
    ram_size = CNFG_SIZE_8kb << ((temp1 >> 16) - 2);
    if (cfg_info->ram_size != ram_size) cnfg_val |= RAM_SIZE_MISMATCH;
    cfg_info->ram_size = ram_size;
    if ((board_id & PAGED_RAM) != 0) ram_size = CNFG_SIZE_16kb;
    if (cfg_info->ram_usable != ram_size) cnfg_val |= RAM_SIZE_MISMATCH;
    cfg_info->ram_usable = ram_size;
  }
  if (!(board_id & INTERFACE_CHIP)) {
    cfg_info->mode_bits |= INTERRUPT_STATUS_BIT;
    cfg_info->media_type = MEDIA_UNKNOWN;
    cfg_info->bic_type = BIC_NO_CHIP;
    cfg_info->clone = lm_verify_nadd(base_addr);
    return(ADAPTER_NO_CONFIG); /* no interface chip */
  }
  intf_chip = board_id & INTERFACE_CHIP_MASK;
  switch(intf_chip) {
  case INTERFACE_5X3_CHIP:
    cfg_info->bic_type = BIC_583_CHIP;
    break;
  case INTERFACE_585_CHIP:
    cfg_info->bic_type = BIC_585_CHIP;
    break;
  default:
    cfg_info->bic_type = BIC_584_CHIP;
    break;
  }
  /*************************************************
                 get interrupt line
  ***************************************************/
  irnum = 0;
  irq_value = 0;	/* FvK */
  switch(intf_chip) {
  case INTERFACE_585_CHIP:
    /* TODO */
    break;
  case INTERFACE_584_CHIP:
    irnum = inb_p(base_addr + 1) & 4; 
  default:
    irnum += (inb_p(base_addr + 4) & 0x60) >> 5;
    if ((irnum == 2) && !(board_id & ALTERNATE_IRQ_BIT))
      irq_value = 4;
    else irq_value = irqlist[irnum];
    break;
  }
  if (cfg_info->irq_value != irq_value) cnfg_val |= IRQ_MISMATCH;
  cfg_info->irq_value = irq_value;
  /* get irq status */
  cfg_info->mode_bits &= ~INTERRUPT_STATUS_BIT;
  switch(intf_chip) {
  case INTERFACE_585_CHIP:
    /* TODO */
    break;
  default:
    if ((inb_p(base_addr + CNFG_IRR_583) & 0x80) != 0)
      cfg_info->mode_bits |= INTERRUPT_STATUS_BIT;
    break;
  }
  /******************************************************
                   get ram base
  *******************************************************/
  rbase1 = inb_p(base_addr) & 0x3f;
  if ((board_id & INTERFACE_CHIP_MASK) != INTERFACE_5X3_CHIP) {
    temp1 =
      (inb_p(base_addr + 5) & 0x1f) << 19 | (rbase1 << 13);
  }
  else temp1 = (rbase1 | 0x40) << 13;
  if (cfg_info->ram_base != temp1) cnfg_val |= RAM_BASE_MISMATCH;
  cfg_info->ram_base = temp1;
  /*****************************************************
                     get rom base
  ******************************************************/
  cfg_info->rom_base = ((inb_p(base_addr + CNFG_BIO_583) & 0x3e) | 0x40) << 13;
  /*****************************************************
                   get rom size (in Kb)
  ******************************************************/
  cfg_info->rom_size = (inb_p(base_addr + CNFG_BIO_583) & 0xc0) >> 2;
  /*****************************************************
                    get boot status
  ******************************************************/
  cfg_info->mode_bits &= ~BOOT_STATUS_MASK;
  if ((inb_p(base_addr + CNFG_GP2) & CNFG_GP2_BOOT_NIBBLE) == 0)
    cfg_info->mode_bits |= BOOT_TYPE_1;
  /*****************************************************
                  get zero wait state
  ******************************************************/
  cfg_info->mode_bits &= ~ZERO_WAIT_STATE_MASK;
  if ((inb_p(base_addr + CNFG_IRR_583) & 1) != 0)
    cfg_info->mode_bits |= ZERO_WAIT_STATE_8_BIT;
  if (board_id & BOARD_16BIT) {
    if (inb_p(base_addr + CNFG_LAAR_584) & 0x20)
      cfg_info->mode_bits |= ZERO_WAIT_STATE_16_BIT;
  }
  /*****************************************************
                 get advanced features
  ******************************************************/
  cfg_info->mode_bits &= ~INTERFACE_TYPE_MASK;
  temp1 = inb_p(base_addr + CNFG_IRR_583);
  if (board_id & ADVANCED_FEATURES) {
    if (temp1 & 0x02) {
      if (temp1 & 0x04) {
	cfg_info->mode_bits |= BNC_INTERFACE;
	cfg_info->media_type = MEDIA_BNC;
      }
      else {
	if ((board_id & MEDIA_MASK) == ETHERNET_MEDIA) {
	  cfg_info->mode_bits |= AUI_INTERFACE;
	  cfg_info->media_type = MEDIA_AUI;
	}
	else {
	  cfg_info->mode_bits |= AUI_10BT_INTERFACE;
	  cfg_info->media_type = MEDIA_AUI_UTP;
	}
      }
    }
    else {
      cfg_info->mode_bits |= STARLAN_10_INTERFACE;
      cfg_info->media_type = MEDIA_S10;
    }
  }
  else {
    if (temp1 & 0x02) {
      cfg_info->mode_bits |= STARLAN_10_INTERFACE;
      cfg_info->media_type = MEDIA_S10;
    }
    else {
      cfg_info->media_type = MEDIA_UNKNOWN;
    }
  }
  /***************************************************
                get adapter type
  ****************************************************/
 cfg_info->adapter_bus =
   (board_id & SLOT_16BIT) ? BUS_ISA16_TYPE : BUS_ISA8_TYPE;
  /***************************************************
                     checksum
  ****************************************************/
  if (cfg_info->bic_type == BIC_584_CHIP) {
    if ((i = Read584Checksum(base_addr)) != SUCCESS) return(i);
  }
  if (cnfg_val) return(CONFIG_WARNING);
  else return(ADAPTER_AND_CONFIG);
}

/* get name of board type */
void GetAdapterName(CNFG_Adapter *cfg_info)
{
 switch(cfg_info->board_id) {
 case WD8003E: strcpy(cfg_info->adapter_name,"8003 Family");
   break;
 case WD8003WT: strcpy(cfg_info->adapter_name,"WD8003WT");
   break;
 case WD8003W: strcpy(cfg_info->adapter_name,"WD8003W");
   break;
 case WD8003EB:
   if ((cfg_info->full_bid & INTERFACE_CHIP_MASK) == INTERFACE_584_CHIP)
     strcpy(cfg_info->adapter_name,"WD8003EP");
   else
     strcpy(cfg_info->adapter_name,"WD8003EB");
   break;
 case WD8003EW: strcpy(cfg_info->adapter_name,"WD8003EW");
   break;
 case WD8003ETA: strcpy(cfg_info->adapter_name,"WD8003ETA");
   break;
 case WD8003EA:
   if ((cfg_info->full_bid & INTERFACE_CHIP_MASK) == INTERFACE_594_CHIP)
     strcpy(cfg_info->adapter_name,"WD8003EPA");
   else
     strcpy(cfg_info->adapter_name,"WD8003EA");
   break;
 case WD8003WA:
   if ((cfg_info->full_bid & INTERFACE_CHIP_MASK) == INTERFACE_594_CHIP)
     strcpy(cfg_info->adapter_name,"WD8003WPA");
   else
     strcpy(cfg_info->adapter_name,"WD8003WA");
   break;
 case WD8013EBT: strcpy(cfg_info->adapter_name,"WD8013EBT");
   break;
 case WD8013EB:  strcpy(cfg_info->adapter_name,"WD8013EB");
   break;
 case WD8013W: strcpy(cfg_info->adapter_name,"WD8013W");
   break;
 case WD8013EW: strcpy(cfg_info->adapter_name,"WD8013EW");
   break;
 case WD8013EWC: strcpy(cfg_info->adapter_name,"WD8013EWC");
   break;
 case WD8013WC: strcpy(cfg_info->adapter_name,"WD8013WC");
   break;
 case WD8013EPC: strcpy(cfg_info->adapter_name,"WD8013EPC");
   break;
 case WD8003EPC: strcpy(cfg_info->adapter_name,"WD8003EPC");
   break;
 case WD8115T: strcpy(cfg_info->adapter_name,"WD8115T");
   break;
 case WD8115TA: strcpy(cfg_info->adapter_name,"WD8115TA");
   break;
 case WD8003WC: strcpy(cfg_info->adapter_name,"WD8003WC");
   break;
 case PCM10BT: strcpy(cfg_info->adapter_name,"PCM10BT");
   break;
 case WD8203EP: strcpy(cfg_info->adapter_name,"WD8203EP");
   break;
 case WD8203W: strcpy(cfg_info->adapter_name,"WD8203W");
   break;
 case WD8216: strcpy(cfg_info->adapter_name,"WD8216");
   break;
 case WD8216T: strcpy(cfg_info->adapter_name,"WD8216T");
   break;
 case WD8216C: strcpy(cfg_info->adapter_name,"WD8216C");
   break;
 default: sprintf(cfg_info->adapter_name,"??%x",cfg_info->board_id);
   break;
 }
}

int VerifyParams(CNFG_Adapter *cfg_info)
{
  int intf_chip;

  cnfg_val = 0;
  intf_chip = cfg_info->full_bid & INTERFACE_CHIP_MASK;
  switch(cfg_info->irq_value) {
  case 3:
    break;
  case 4:
    if ((cfg_info->full_bid & ALTERNATE_IRQ_BIT) &&
	      (intf_chip == INTERFACE_5X3_CHIP))
    cnfg_val |= IRQ_INVALID;
    break;
  case 5:
    if (!(cfg_info->full_bid & ALTERNATE_IRQ_BIT) &&
	      (cfg_info->full_bid & INTERFACE_CHIP))
    cnfg_val |= IRQ_INVALID;
    break;
  case 7:
    break;
  case 9:
  case 10:
  case 11:
    if (!(cfg_info->full_bid & BOARD_16BIT)) cnfg_val |= IRQ_RANGE;
    else if (!(cfg_info->full_bid & SLOT_16BIT)) cnfg_val |= IRQ_INVALID;
    break;
  case 15:
    break;
  default:
    cnfg_val |= IRQ_RANGE;
    break;
  }
  switch(cfg_info->ram_usable) {
  case 8:
  case 16:
  case 32:
    break;
  default:
  cnfg_val |= RAM_SIZE_RANGE;
  }
  if (cfg_info->ram_base < 0x80000) cnfg_val |= RAM_BASE_RANGE;
  if (cfg_info->full_bid & SLOT_16BIT) {
    if (cfg_info->ram_base >= 0x1000000) cnfg_val |= RAM_BASE_RANGE;
  }
  else if (cfg_info->ram_base >= 0x100000) cnfg_val |= RAM_BASE_RANGE;
  if ((cfg_info->io_base < 0x200) || (cfg_info->io_base > 0x3e0)) {
    cnfg_val |= IO_BASE_RANGE;
  }
  else if (cfg_info->io_base & 0x1f) cnfg_val |= IO_BASE_INVALID;
  return((cnfg_val == 0) ? SUCCESS : CONFIG_ERROR);
}

int LM_GetCnfg(CNFG_Adapter *cfg_info)
{
 int retval,retval2,temp;
 retval = lm_get_at_config(cfg_info);
 if (verbose > 2)
   printf("lm_get_at_config returned %x, cnfg_val=%x\n",retval,cnfg_val);
 if (retval == ADAPTER_NOT_FOUND) return(retval);
 if (retval != EEROM_CKSUM_ERROR) {
   temp = cnfg_val;
   retval2 = VerifyParams(cfg_info);
   if (verbose > 2)
     printf("verifyparams returned %x, cnfg_val=%x\n",retval2,cnfg_val);
   if (retval2 != SUCCESS) {
     cnfg_val |= temp; /* combine flags */
     retval = retval2;
   }
   else cnfg_val = temp; /* restore cnfg_val */
 }
 GetAdapterName(cfg_info);
 cfg_info->nic_type = NIC_8390_CHIP;
 if (cfg_info->full_bid & NIC_690_BIT) cfg_info->nic_type = NIC_690_CHIP;
 if (cfg_info->full_bid & NIC_790_BIT) cfg_info->nic_type = NIC_790_CHIP;
 if (cfg_info->full_bid & NIC_825_BIT) cfg_info->nic_type = NIC_825_CHIP;
 return(retval);
}

/* returns 1 if config is soft/soft, 0 otherwise */
int GetJumperStatus(int ioaddr)
{
  return((~(inb_p(ioaddr + REG_IJR)) & 7) == 0);
}

void RecallConfigZero(int ioaddr)
{
  SelectOtherRegister(ioaddr);
  outb_p(inb_p(ioaddr + CNFG_EAR_583) & 0xf,ioaddr + CNFG_EAR_583); /* select page 0 */
  RecallEERomData(ioaddr, RECALL_DATA);
}

void RecallLanAddress(int ioaddr)
{
  RecallEERomData(ioaddr, RECALL_LANADDR);
}

int LM_Get_SoftCnfg(CNFG_Adapter *cfg_info)
{
  int ioaddr,irnum,irq_value,temp,rbase1;
  if ((cfg_info->full_bid & INTERFACE_CHIP_MASK) == INTERFACE_5X3_CHIP)
    return(NO_FIXED_CNFG);
  ioaddr = cfg_info->io_base;
  if (GetJumperStatus(ioaddr)) return(NO_FIXED_CNFG);
  RecallConfigZero(ioaddr);
/*******************************************************************
                interrupt line
 *******************************************************************/
  if (!(inb_p(ioaddr + LAR0 + CNFG_IRR_583) & 0x80)) {
    cfg_info->irq_value = 0;
  }
  else {
    irnum = inb_p(ioaddr + LAR0 + 1) & 4; 
    irnum += (inb_p(ioaddr + LAR0 + 4) & 0x60) >> 5;
    if ((irnum == 2) && !(cfg_info->full_bid & ALTERNATE_IRQ_BIT))
      irq_value = 4;
    else irq_value = irqlist[irnum];
    cfg_info->irq_value = irq_value;
  }
/*******************************************************************
                io address
 *******************************************************************/
  temp = inb_p(ioaddr + LAR0 + CNFG_IAR_583);
  cfg_info->io_base = ((temp & 0xe0) << 8) + ((temp & 0x1f) << 5);
/*******************************************************************
                ram base
 *******************************************************************/
  rbase1 = inb_p(ioaddr + LAR0) & 0x3f;
  cfg_info->ram_base =
    (inb_p(ioaddr + LAR0 + 5) & 0x1f) << 19 | (rbase1 << 13);
/*******************************************************************
                rom base and size
 *******************************************************************/
  cfg_info->rom_base = ((inb_p(ioaddr + LAR0 + CNFG_BIO_583) & 0x3e) | 0x40) << 13;
  cfg_info->rom_size = (inb_p(ioaddr + LAR0 + CNFG_BIO_583) & 0xc0) >> 2;
/*******************************************************************
                recall lan address
 *******************************************************************/
 RecallLanAddress(ioaddr);
 return(SUCCESS);
}

void lm_pc_58x_io(CNFG_Adapter *cfg_info,int ioaddr)
{
  int temp;
  temp = cfg_info->io_base_new;
  temp = ((temp & 0x3e0) >> 5) + ((temp & 0xe000) >> 8);
  outb_p(temp, ioaddr + CNFG_IAR_583);
  if (cfg_info->config_mode & STORE_REGS) {
    cfg_info->io_base = cfg_info->io_base_new;
  }
}

void lm_pc_58x_irq(CNFG_Adapter *cfg_info,int ioaddr)
{
  int if583,icrval,irrval;

  icrval = irrval = 0;	/* FvK */
  if583 = (cfg_info->full_bid & INTERFACE_CHIP_MASK) == INTERFACE_5X3_CHIP;
  switch(cfg_info->irq_value) {
  case 3:
    icrval = 0;
    irrval = 0x20;
    break;
  case 4:
    if (cfg_info->full_bid & ALTERNATE_IRQ_BIT) {
      icrval = 4;
      irrval = 0x60;
    }
    else {
      icrval = 0;
      irrval = 0x40;
    }
    break;
  case 5:
    icrval = 0;
    irrval = 0x40;
    break;
  case 7:
    icrval = 0;
    irrval = 0x60;
    break;
  case 8:
    icrval = 0;
    irrval = 0x20;
    break;
  case 9:
    icrval = 0;
    irrval = 0;
    break;
  case 10:
    icrval = 4;
    irrval = 0;
    break;
  case 11:
    icrval = 4;
    irrval = 0x20;
    break;
  case 15:
    icrval = 4;
    irrval = 0x40;
    break;
  default:
    break;
  }
  if (!if583) {
    icrval |= (inb_p(ioaddr + CNFG_ICR_583) & CNFG_ICR_MASK);
    outb_p(icrval, ioaddr + CNFG_ICR_583);
  }
  irrval |= (inb_p(ioaddr + CNFG_IRR_583) & 0x9f);
  outb_p(irrval, ioaddr + CNFG_IRR_583);
}

void lm_pc_58x_irq_status(CNFG_Adapter *cfg_info, int ioaddr)
{
  int temp;
  temp = inb_p(ioaddr + CNFG_IRR_583) & ~CNFG_IRR_IEN;
  if (cfg_info->mode_bits & INTERRUPT_STATUS_BIT) temp |= CNFG_IRR_IEN;
  outb_p(temp, ioaddr + CNFG_IRR_583);
}

void lm_pc_58x_boot_status(CNFG_Adapter *cfg_info, int ioaddr)
{
  int temp;
  temp = inb_p(ioaddr + CNFG_GP2) & ~CNFG_GP2_BOOT_NIBBLE;
  if (!(cfg_info->mode_bits & BOOT_STATUS_MASK)) temp |= 1;
  outb_p(temp, ioaddr + CNFG_GP2);
}

void lm_pc_58x_zero_wait_state(CNFG_Adapter *cfg_info, int ioaddr)
{
  int temp;
  temp = inb_p(ioaddr + CNFG_IRR_583) & ~CNFG_IRR_ZWS;
  if (cfg_info->mode_bits & ZERO_WAIT_STATE_8_BIT) temp |= CNFG_IRR_ZWS;
  outb_p(temp, ioaddr + CNFG_IRR_583);
  if (cfg_info->full_bid & BOARD_16BIT) {
    temp = inb_p(ioaddr + CNFG_LAAR_584) & ~CNFG_LAAR_ZWS;
    if (cfg_info->mode_bits & ZERO_WAIT_STATE_16_BIT) temp |= CNFG_LAAR_ZWS;
    outb_p(temp, ioaddr + CNFG_LAAR_584);
  }
}

void lm_pc_58x_net_interface(CNFG_Adapter *cfg_info, int ioaddr)
{
  int temp,net_int;
  temp = inb_p(ioaddr + CNFG_IRR_583) & 0xf9;
  net_int = cfg_info->mode_bits & INTERFACE_TYPE_MASK;
  switch(net_int) {
  case BNC_INTERFACE:
    temp |= 6;
    break;
  case AUI_INTERFACE:
    temp |= 2;
    break;
  case AUI_10BT_INTERFACE:
    temp |= 4;
    break;
  }
  /* if not advanced feature adapter, invert link integrity test bit */
  if (!(cfg_info->full_bid & ADVANCED_FEATURES)) temp ^= 2;
  outb_p(temp, ioaddr + CNFG_IRR_583);
}

void lm_pc_58x_ram_base(CNFG_Adapter *cfg_info, int ioaddr)
{
  int temp;
  if (!((cfg_info->full_bid & INTERFACE_CHIP_MASK) == INTERFACE_5X3_CHIP)) {
    temp = (cfg_info->ram_base & 0xf80000) >> 19;
    temp |= inb_p(ioaddr + CNFG_LAAR_584) & CNFG_LAAR_MASK;
    if (!(cfg_info->full_bid & BOARD_16BIT)) temp = 1;
    outb_p(temp,ioaddr + CNFG_LAAR_584);
  }
  temp = (cfg_info->ram_base & 0x07e000) >> 13;
  outb_p(temp,ioaddr);
}

void lm_pc_58x_ram_size(CNFG_Adapter *cfg_info, int ioaddr)
{
  int temp;
  temp = inb_p(ioaddr + CNFG_ICR_583) & ~CNFG_ICR_MSZ;
  if ((cfg_info->ram_size != CNFG_SIZE_8kb) &&
      (cfg_info->ram_size != CNFG_SIZE_16kb)) temp |= CNFG_ICR_MSZ;
  outb_p(temp, ioaddr + CNFG_ICR_583);
}

void lm_pc_58x_rom_base(CNFG_Adapter *cfg_info, int ioaddr)
{
  int temp;
  temp = (cfg_info->rom_base & 0x7c000) >> 13;
  temp |= inb_p(ioaddr + CNFG_BIO_583) & 0x0c0;
  temp &= 0xfe;
  outb_p(temp, ioaddr + CNFG_BIO_583);
}

void lm_pc_58x_rom_size(CNFG_Adapter *cfg_info, int ioaddr)
{
  int temp;
  switch(cfg_info->rom_size) {
  case CNFG_SIZE_64kb:
    temp = 0xc0;
    break;
  case CNFG_SIZE_32kb:
    temp = 0x80;
    break;
  case CNFG_SIZE_16kb:
    temp = 0x40;
    break;
  default:
    temp = 0;
    break;
  }
  temp |= inb_p(ioaddr + CNFG_BIO_583) & 0x3e;
  outb_p(temp, ioaddr + CNFG_BIO_583);
}


/******************************************************
             store 584 eeprom stuff
 ******************************************************

		ROM	IRQ-IO-RAM	EEROM PG.
		==========	========
		soft	soft	  	0
		D800	soft		1
		soft	3-280-D000	2
		D800	3-280-D000	3
		soft	5-300-CC00	4
		D800	5-300-CC00	5
		(Hardware Initialized)	6
		D800	7-280-D000	7
only soft configs are changed.
 *********************************************************/

void lm_pc_584_store(CNFG_Adapter *cfg_info)
{
  int page,iobase,iobasefake,temp;
#ifdef FACTORY_INIT
  int saverombase,saverambase,saveirq,saveiobase,saveiobasenew;
#endif
  if (verbose > 2) printf("lm_pc_584_store\n");
  for (page = 7; page >= 0; page--) {
    /* recall eeprom into lan address regs */
    iobase = cfg_info->io_base;
    temp = inb_p(iobase + CNFG_ICR_583) & CNFG_ICR_IR2_584;
    temp |= BID_OTHER_BIT;
    outb_p(temp,iobase + CNFG_ICR_583);
    temp = inb_p(iobase + CNFG_EAR_583);
    temp &= 0xf;
    temp |= page << 4;
    outb_p(temp, iobase + CNFG_EAR_583);
    temp = inb_p(iobase + CNFG_ICR_583) & 0x0e;
    temp |= CNFG_ICR_RLA; /* set RLA bit */
    outb_p(temp, iobase + CNFG_ICR_583); /* do the recall */
    while (inb_p(iobase + CNFG_ICR_583) & CNFG_ICR_RLA); /* wait for recall */
    iobasefake = iobase + LAR0; /* point to lan address regs */
/* this is only needed if someone wiped out the factory settings */
#ifdef FACTORY_INIT
    /* these bits should be 0 */
    outb_p(inb_p(iobasefake + 1) & 0xf, iobasefake + 1);
    saverombase = cfg_info->rom_base;
    saverambase = cfg_info->ram_base;
    saveirq = cfg_info->irq_value;
    saveiobase = cfg_info->io_base;
    saveiobasenew = cfg_info->io_base_new;
    switch(page) {
    case 0:
      break;
    case 1:
      cfg_info->rom_base = 0xd8000;
      lm_pc_58x_rom_base(cfg_info,iobasefake);
      break;
    case 2:
      cfg_info->irq_value = 3;
      lm_pc_58x_irq(cfg_info,iobasefake);
      cfg_info->ram_base = 0x0d0000;
      lm_pc_58x_ram_base(cfg_info,iobasefake);
      cfg_info->io_base_new = 0x280;
      lm_pc_58x_io(cfg_info,iobasefake);
      break;
    case 3:
      cfg_info->rom_base = 0xd8000;
      lm_pc_58x_rom_base(cfg_info,iobasefake);
      cfg_info->irq_value = 3;
      lm_pc_58x_irq(cfg_info,iobasefake);
      cfg_info->ram_base = 0x0d0000;
      lm_pc_58x_ram_base(cfg_info,iobasefake);
      cfg_info->io_base_new = 0x280;
      lm_pc_58x_io(cfg_info,iobasefake);
      break;
    case 4:
      cfg_info->irq_value = 5;
      lm_pc_58x_irq(cfg_info,iobasefake);
      cfg_info->ram_base = 0x0cc000;
      lm_pc_58x_ram_base(cfg_info,iobasefake);
      cfg_info->io_base_new = 0x300;
      lm_pc_58x_io(cfg_info,iobasefake);
      break;
    case 5:
      cfg_info->rom_base = 0xd8000;
      lm_pc_58x_rom_base(cfg_info,iobasefake);
      cfg_info->irq_value = 5;
      lm_pc_58x_irq(cfg_info,iobasefake);
      cfg_info->ram_base = 0x0cc000;
      lm_pc_58x_ram_base(cfg_info,iobasefake);
      cfg_info->io_base_new = 0x300;
      lm_pc_58x_io(cfg_info,iobasefake);
      break;
    case 6:
      cfg_info->rom_base = 0xd8000;
      lm_pc_58x_rom_base(cfg_info,iobasefake);
      cfg_info->irq_value = 9;
      lm_pc_58x_irq(cfg_info,iobasefake);
      cfg_info->ram_base = 0x0d0000;
      lm_pc_58x_ram_base(cfg_info,iobasefake);
      cfg_info->io_base_new = 0x280;
      lm_pc_58x_io(cfg_info,iobasefake);
      break;
    case 7:
      cfg_info->rom_base = 0xd8000;
      lm_pc_58x_rom_base(cfg_info,iobasefake);
      cfg_info->irq_value = 7;
      lm_pc_58x_irq(cfg_info,iobasefake);
      cfg_info->ram_base = 0x0d0000;
      lm_pc_58x_ram_base(cfg_info,iobasefake);
      cfg_info->io_base_new = 0x280;
      lm_pc_58x_io(cfg_info,iobasefake);
      break;
    }
    cfg_info->rom_base = saverombase;
    cfg_info->ram_base = saverambase;
    cfg_info->io_base_new = saveiobasenew;
    cfg_info->io_base = saveiobase;
    outb_p(0, iobasefake + 7);
    if (page > 0) outb_p(0, iobasefake + 6);
#endif  
    if (page <= 1) {
      lm_pc_58x_irq(cfg_info,iobasefake);
      lm_pc_58x_irq_status(cfg_info,iobasefake);
      lm_pc_58x_ram_base(cfg_info,iobasefake);
      lm_pc_58x_ram_size(cfg_info,iobasefake);
    }
    if ((page == 0) || (page == 2) ||(page == 4)) {
      lm_pc_58x_rom_base(cfg_info,iobasefake);
      lm_pc_58x_rom_size(cfg_info,iobasefake);
    }
    if (page <= 1) {
      lm_pc_58x_io(cfg_info,iobasefake);
    }
    lm_pc_58x_boot_status(cfg_info,iobasefake);
    lm_pc_58x_zero_wait_state(cfg_info,iobasefake);
    lm_pc_58x_net_interface(cfg_info,iobasefake);
    temp = inb_p(iobase + CNFG_ICR_583) & CNFG_ICR_IR2_584;
    temp |= BID_OTHER_BIT | CNFG_ICR_STO;
    outb_p(temp,iobase + CNFG_ICR_583); /* store it */
    while (inb_p(iobase + CNFG_ICR_583) & CNFG_ICR_STO); /* wait for store */
  }
  /* now recall the lan address */
  temp = inb_p(iobase + CNFG_ICR_583) & CNFG_ICR_IR2_584;
  outb_p(temp,iobase + CNFG_ICR_583); /* restore bio register */
  temp |= CNFG_ICR_RLA;
  outb_p(temp,iobase + CNFG_ICR_583); /* recall lan address */
  while (inb_p(iobase + CNFG_ICR_583) & CNFG_ICR_RLA); /* wait for recall */
  if ((cfg_info->full_bid & INTERFACE_CHIP_MASK) == INTERFACE_584_CHIP) {
    Write584Checksum(iobase);
  }
}

void Write584Config(CNFG_Adapter *cfg_info)
{
  int iobase;
  iobase = cfg_info->io_base;
  lm_pc_58x_irq(cfg_info, iobase);
  lm_pc_58x_irq_status(cfg_info, iobase);
  lm_pc_58x_ram_base(cfg_info, iobase);
  lm_pc_58x_ram_size(cfg_info, iobase);
  lm_pc_58x_rom_base(cfg_info, iobase);
  lm_pc_58x_rom_size(cfg_info, iobase);
  lm_pc_58x_boot_status(cfg_info, iobase);
  lm_pc_58x_zero_wait_state(cfg_info, iobase);
  lm_pc_58x_net_interface(cfg_info, iobase);
  lm_pc_58x_io(cfg_info, iobase);
}

/*
  returns:
  1 - no interface chip
  0 - no board there
*/
int LM_PutCnfg(CNFG_Adapter *cfg_info)
{
  int iobase,board_id,intf_chip;
  iobase = cfg_info->io_base;
  if (!lm_gc_is_board_there(iobase)) return(0xffff);
  board_id = cfg_info->full_bid;
  if (!(board_id & INTERFACE_CHIP)) return(1);
  if (cfg_info->config_mode & STORE_REGS) {
    Write584Config(cfg_info);
    iobase = cfg_info->io_base; /* in case it has changed! */
  }
  if (cfg_info->config_mode & STORE_EEROM) {
    intf_chip = board_id & INTERFACE_CHIP_MASK;
    if (intf_chip != INTERFACE_5X3_CHIP) {
      if (intf_chip != INTERFACE_584_CHIP) {
	/* must be 585 chip, punt for now */
	return(0xffff);
      }
      /* 584 chip handled here */
      lm_pc_584_store(cfg_info);
    }
    else {
      /* 583 chip handled here */
      /* do a store into eerom */
      outb_p((inb_p(iobase + CNFG_ICR_583) & 0xf) | 0x80,
	     iobase + CNFG_ICR_583);
      /* wait for it to complete */
      while (inb_p(iobase + CNFG_ICR_583) & 0x80);
    }
  }
  return(0);
}


void show_eeprom(int ioaddr, int flag583)
{
 int page,i;
 if (flag583) {
   printf("eeprom  ");
   SelectOtherRegister(ioaddr);
   outb_p((inb_p(ioaddr + CNFG_BIO_583) & 0xf), ioaddr + CNFG_BIO_583);
   RecallEERomData(ioaddr, RECALL_DATA);
   for (i = 0; i < 8; i++) {
     printf("%02x ",inb_p(ioaddr + 8 + i));
   }
   printf("\n");
 }
 else {
   for (page = 0; page < 16; page++) {
     printf("eeprom page %02d  ",page);
     SelectOtherRegister(ioaddr);
     outb_p((inb_p(ioaddr + CNFG_BIO_583) & 0xf) | (page << 4),ioaddr + CNFG_BIO_583);
     RecallEERomData(ioaddr, RECALL_DATA);
     for (i = 0; i < 8; i++) {
       printf("%02x ",inb_p(ioaddr + 8 + i));
     }
     printf("\n");
   }
 }
 /* restore lan address */
 RecallEERomData(ioaddr, RECALL_LANADDR);
}

void show_regs(int ioaddr)
{
  int i,page,csav;
  unsigned int reg;

  printf("ASIC regs: ");
  for (i = 0; i < 0x10; i++) {
    reg = inb_p(ioaddr + i);
    printf("%02x ",reg);
  }
  printf("\n");
  csav = inb_p(ioaddr + 0x10) & 0xc0;
  for (page = 0; page < 4; page++) {
    outb(page<<6,ioaddr + 0x10); /* set page */
    printf("NIC page %d ",page);
    for (i = 0; i < 0x10; i++) {
      reg = inb_p(ioaddr + 0x10 + i);
      printf("%02x ",reg);
    }
    printf("\n");
  }
  outb_p(csav, ioaddr + 0x10);
}

void test_status(int status)
{
  switch(status) {
  case ADAPTER_NOT_FOUND:
    printf("no card found\n");
    exit(-1);
    break;
  case ADAPTER_AND_CONFIG:
    /* this is ok */
    break;
  case ADAPTER_NO_CONFIG:
    printf("Your card is not software configurable.\n");
    printf("The only way to configure it is to remove the card and set\n");
    printf("the jumpers. See your manual for more information.\n");
    exit(-1);
    break;
  case CONFIG_ERROR:
    printf("configuration error: %x\n",cnfg_val);
    exit(-1);
    break;
  case CONFIG_WARNING:
    /* this is the expected result */
    break;
  case EEROM_CKSUM_ERROR:
    printf("Warning - eerom checksum error.\n");
    break;
  default:
    printf("unknown return value from lm_getcnfg.\n");
    exit(-1);
  }
}

int main(int argc, char *argv[])
{
 unsigned char response[80];
 unsigned int iobase;
 int i,newval,itsok,jumpers;
 CNFG_Adapter smc_card_info,soft_cnfg_info;
 int argok,cmd_eeprom,cmd_regs,onepage;
 int numfound,status,fixed_cnfg;
 int found_addr[20];
 int media,net_interface,zerows;
 int do_set_port,do_set_rambase,do_set_irq,do_set_net_interface;
 int set_port,set_rambase,set_irq,set_net_interface;

 cmd_eeprom = cmd_regs = 0;	/* FvK */
 do_set_irq = do_set_net_interface = 0;	/* FvK */
 set_port = set_rambase = set_irq = set_net_interface = 0; /* FvK */
 do_set_port = do_set_rambase = do_set_irq = do_set_net_interface = 0;
 iobase = 0;
 verbose = 0;
 smc_card_info.adapter_num = 0;
 smc_card_info.pc_bus = 0;
 smc_card_info.io_base = 0;
 smc_card_info.adapter_name[0] = '\0';
 smc_card_info.irq_value = 0;
 smc_card_info.rom_size = 0;
 smc_card_info.rom_base = 0;
 smc_card_info.rom_access = 0;
 smc_card_info.ram_size = 0;
 smc_card_info.ram_base = 0;
 smc_card_info.ram_access = 0;
 smc_card_info.ram_usable = 0;
 smc_card_info.io_base_new = 0;
 smc_card_info.node_address[0] = 0;
 smc_card_info.max_packet_size = 0;
 smc_card_info.num_of_tx_buffs = 0;
 smc_card_info.media_type = 0;
 smc_card_info.adapter_bus = 0;
 smc_card_info.pos_id = 0;
 smc_card_info.adapter_flags = 0;
 smc_card_info.slot_num = 0;
 smc_card_info.bic_type = 0;
 smc_card_info.nic_type = 0;
 smc_card_info.board_id = 0;
 smc_card_info.extra_info = 0;
 smc_card_info.full_bid = 0;
 smc_card_info.mode_bits = 0;
 smc_card_info.status_bits = 0;
 smc_card_info.config_mode = 0;
 smc_card_info.page_offset_mask = 0;
 smc_card_info.clone = 0;
 soft_cnfg_info = smc_card_info;
 if (argc > 1)
 for (i = 1; i < argc; i++) {
   argok = 0;
   if (((strcmp(argv[i],"-v") == 0) || (strcmp(argv[i],"--verbose") == 0))
       && (argc > (i + 1))) {
     if (sscanf(argv[++i],"%d",&newval) == 1) {
       verbose = newval;
       argok = 1;
     }
     goto next_arg;
   }
   if (((strcmp(argv[i],"-p") == 0) || (strcmp(argv[i],"--newaddr") == 0))
       && (argc > (i + 1))) {
     if (sscanf(argv[++i],"%x",&newval) == 1) {
       if ((newval & 0xe3e0) == newval) {
	 do_set_port = 1;
	 set_port = newval;
	 argok = 1;
       }
     }
     goto next_arg;
   }
   if (((strcmp(argv[i],"-b") == 0) || (strcmp(argv[i],"--ramstart") == 0))
       && (argc > (i + 1))) {
     if (sscanf(argv[++i],"%x",&newval) == 1) {
       do_set_rambase = 1;
       set_rambase = newval;
       argok = 1;
     }
     goto next_arg;
   }
   if (((strcmp(argv[i],"-i") == 0) || (strcmp(argv[i],"--irq") == 0))
       && (argc > (i + 1))) {
     if (sscanf(argv[++i],"%u",&newval) == 1) {
       do_set_irq = 1;
       set_irq = newval;
       argok = 1;
     }
     goto next_arg;
   }
   if (((strcmp(argv[i],"-m") == 0) || (strcmp(argv[i],"--media") == 0))
       && (argc > (i + 1))) {
     i++;
     if (strcmp(argv[i],"aui") == 0) {
       do_set_net_interface = 1;
       set_net_interface = AUI_INTERFACE;
       argok = 1;
     }
     if (strcmp(argv[i],"bnc") == 0) {
       do_set_net_interface = 1;
       set_net_interface = BNC_INTERFACE;
       argok = 1;
     }
     if (strcmp(argv[i],"twp") == 0) {
       do_set_net_interface = 1;
       set_net_interface = AUI_10BT_INTERFACE;
       argok = 1;
     }
     goto next_arg;
   }
   if (strcmp(argv[i],"-e") == 0) {
     cmd_eeprom = argok = 1;
     goto next_arg;
   }
   if (strcmp(argv[i],"-r") == 0) {
     cmd_regs = argok = 1;
     goto next_arg;
   }
   if (((strncmp(argv[i],"-a",2) == 0) || (strcmp(argv[i],"--baseaddr") == 0))
       && (argc > (i + 1))) {
     sscanf(argv[++i],"%x",&newval);
     if ((newval & 0xe3e0) == newval) {
       iobase = newval;
       argok = 1;
     }
     else {
       printf("addr must be [02468ace][0-3][02468ace]0\n");
       exit(-1);
     }
     goto next_arg;
   }
 next_arg:
   if (!argok) {
     printf("Usage: wdsetup [options]\n\
options are:\n\
-a addr\t\tspecify board's base io address\n\
--baseaddr addr\t\n\
-v verbose_level\tspecify verbosity level\n\
--verbose verbose_level\n
-r\t\tdump registers\n\
--regs\t\tdump registers\n\
-e\t\tdump eeprom\n\
-eeprom\t\tdump eeprom\n\
note: if any of the following options are used, the interactive\n\
    reconfiguring of the eeprom will be skipped.\n\
-e\t\tdump eeprom contents\n\
-r\t\tdump registers\n\
-p addr\tset board's new io address\n\
--newaddr addr\t\n\
-b addr\t\tset new ram start address\n\
--ramstart addr\t\n\
-i irq\t\tset new interrupt number\n\
--irq irq\t\n\
-m media\tset media type [aui] || [bnc] || [twp]\n\
--media media\t\n");
     exit(-1);
   }
 }

 if (iopl(3)) {
   perror("io-perm2");
   exit (-1);
 }

/* do configuring via command line args here */
 if (do_set_port || do_set_rambase || do_set_irq || do_set_net_interface) {
   if (iobase == 0) {
     return(-1); /* didn't specify address */
   }
   smc_card_info.io_base = iobase;
   smc_card_info.io_base_new = iobase;
   status = LM_GetCnfg(&smc_card_info);
   test_status(status);
   if (do_set_port) {
     smc_card_info.io_base_new = set_port;
   }
   if (do_set_rambase) {
     if ((set_rambase & 0xffe000) == set_rambase) {
       smc_card_info.ram_base = set_rambase;
     }
     else return(-1); /* bad ram address */
   }
   if (do_set_irq) {
     smc_card_info.irq_value = set_irq;
   }
   if (do_set_net_interface) {
     media = smc_card_info.full_bid & MEDIA_MASK;
     switch(set_net_interface) {
     case AUI_INTERFACE:
       if ((media != ETHERNET_MEDIA) && (media != EW_MEDIA)) exit(-1);
       break;
     case BNC_INTERFACE:
       if ((media != ETHERNET_MEDIA) && (media != EW_MEDIA)) exit(-1);
       break;
     case AUI_10BT_INTERFACE: 
       if ((media != TWISTED_PAIR_MEDIA) && (media != EW_MEDIA)) exit(-1);
       break;
     default:
       return(-1); /* bad set_net_interface type */
       break;
     }
     smc_card_info.mode_bits &= ~INTERFACE_TYPE_MASK;
     smc_card_info.mode_bits |= set_net_interface;
   }
   status = LM_PutCnfg(&smc_card_info);
   return(0); /* normal return for command line configuring */
 }
/* non command line configuring */

 printf("Setup for Western Digital and SMC ethercards, version %s\n",
	wdsetup_version);
 if (iobase == 0) {
   /* look for cards at all possible addresses */
   numfound = 0;
   for (iobase = 0x200; iobase <= 0x3e0; iobase += 0x20) {
     if (lm_gc_is_board_there(iobase)) {
       found_addr[numfound++] = iobase;
     }
   }
   if (numfound == 0) {
     printf("no cards recognized.\n");
     exit(-1);
   }
   if (numfound == 1) iobase = found_addr[0];
   if (numfound > 1) {
     printf("%d cards recognized.\naddresses = ",numfound);
     for (i = 0; i < numfound; i++) {
       printf("%04x ",found_addr[i]);
     }
     printf("\nWhich address do you want to set-up ? ");
     fgets(response,80,stdin);
     sscanf(response,"%x",&iobase);
   }
 }
 smc_card_info.io_base = iobase;
 smc_card_info.io_base_new = iobase;
 status = LM_GetCnfg(&smc_card_info);
 test_status(status);
 if (verbose > 1) {
   printf("media_type = %x\n",smc_card_info.media_type);
   printf("mode_bits = %x\n",smc_card_info.mode_bits);
   printf("full_bid = %x\n",smc_card_info.full_bid);
 }
 if (cmd_regs) {
   show_regs(iobase);
   exit(0);
 }
 if (cmd_eeprom) {
   onepage = (smc_card_info.full_bid & INTERFACE_CHIP_MASK) == INTERFACE_5X3_CHIP;
   show_eeprom(iobase, onepage);
   exit(0);
 }
 soft_cnfg_info = smc_card_info;
 status = LM_Get_SoftCnfg(&soft_cnfg_info);
 if (verbose >= 2) printf("lmgetsoftcnfg returned %x\n",status);
 fixed_cnfg = (status != NO_FIXED_CNFG);
 if (smc_card_info.clone) printf("Clone: ");
 printf("Board type:\t%s\n",smc_card_info.adapter_name);
 printf("node address: ");
 for (i = 0; i < 6; i++) printf("%02x",inb_p(iobase + 8 + i));
 if (fixed_cnfg) {
   printf("\n\n\t\t\tFixed (current)\tSoft\n");
   printf("\t\t\tsetup\t\tsetup\n\n");
 }
 else printf("\n\n\t\t\tcurrent setup\n\n");
 jumpers = (inb_p(iobase + WD_JUMPERS) & 7) ^ 7;
 printf("i/o base addr\t\t%04x",smc_card_info.io_base);
 if (fixed_cnfg) printf("\t\t%04x",soft_cnfg_info.io_base);
 printf("\nirq\t\t\t%s%d",
	(smc_card_info.mode_bits & INTERRUPT_STATUS_BIT) ?
	"" : "(Disabled) ",smc_card_info.irq_value);
 if (fixed_cnfg) {
   printf("\t\t%s%d",
	(soft_cnfg_info.mode_bits & INTERRUPT_STATUS_BIT) ?
	"" : "(Disabled) ",soft_cnfg_info.irq_value);
 }
 printf("\nram size\t\t%d K",smc_card_info.ram_size);
 if (fixed_cnfg) {
   printf("\t\t%d K",soft_cnfg_info.ram_size);
 }
 printf("\nram base address\t%06x",smc_card_info.ram_base);
 if (fixed_cnfg) {
   printf("\t\t%06x",soft_cnfg_info.ram_base);
 }
 if ((smc_card_info.full_bid & BOARD_16BIT) &&
     (smc_card_info.full_bid & SLOT_16BIT)) {
   zerows = smc_card_info.mode_bits & ZERO_WAIT_STATE_16_BIT;
 }
 else zerows = smc_card_info.mode_bits & ZERO_WAIT_STATE_8_BIT;
 printf("\nadd wait states\t\t%s", zerows ? "no" : "yes");
 if (fixed_cnfg) {
   printf("\t\t%s", zerows ? "no" : "yes");
 }
 printf("\nnetwork connection\t");
 net_interface = smc_card_info.mode_bits & INTERFACE_TYPE_MASK;
 switch(net_interface) {
   case AUI_INTERFACE: printf("aui");
     break;
   case AUI_10BT_INTERFACE: printf("twp");
     break;
   case BNC_INTERFACE: printf("bnc");
     break;
   default: printf("unknown");
   break;
   }
 if (fixed_cnfg) {
   switch(net_interface) {
   case AUI_INTERFACE: printf("\t\taui");
     break;
   case AUI_10BT_INTERFACE: printf("\t\ttwp");
     break;
   case BNC_INTERFACE: printf("\t\tbnc");
     break;
   default: printf("\t\tunknown");
     break;
   }
 }
 printf("\n\n");
 if (smc_card_info.rom_size == 0) printf("rom size\t\t(Disabled)");
 else printf("rom size\t\t%d K",smc_card_info.rom_size);
 if (fixed_cnfg) {
   if (soft_cnfg_info.rom_size == 0) printf("\t\t(Disabled)");
   else printf("\t\t%d K",soft_cnfg_info.rom_size);
 }
 printf("\nrom base address\t%05x",smc_card_info.rom_base);
 if (fixed_cnfg) {
   printf("\t\t%05x",soft_cnfg_info.rom_base);
 }
 printf("\n\nchange the soft configuration in eeprom? (y) ");
 fgets(response,80,stdin);
 if ((response[0] == 'y') || (response[0] == 'Y') || (response[0] == '\n')) {
   itsok = 0;
   while (!itsok) {
     printf("What io address do you want (%x) : ",smc_card_info.io_base);
     fgets(response,80,stdin);
     itsok = 1;
     if (response[0] != '\n') {
       sscanf(response,"%x",&newval);
       if ((newval & 0x3e0) == newval) {
	 smc_card_info.io_base_new = newval;
       }
       else {
	 itsok = 0;
	 printf("addr must be [2-3][02468ace]0\n");
       }
     }
   }
   itsok = 0;
   while (!itsok) {
     printf("What irq do you want (%d) : ",smc_card_info.irq_value);
     fgets(response,80,stdin);
     itsok = 1;
     if (response[0] != '\n') {
       sscanf(response,"%d",&newval);
       switch(newval) {
       case 3:
       case 4:
       case 5:
       case 7:
       case 9:
       case 10:
       case 11:
       case 15:
	 smc_card_info.irq_value = newval;
	 break;
       default:
	 itsok = 0;
	 break;
       }
     }
   }
   itsok = 0;
   while (!itsok) {
     printf("enter ram start address (%06x) : ",smc_card_info.ram_base);
     fgets(response,80,stdin);
     itsok = 1;
     if (response[0] != '\n') {
       sscanf(response,"%x",&newval);
       if ((newval & 0xffe000) == newval) {
	 printf("ram address ok\n");
	 smc_card_info.ram_base = newval;
       }
       else itsok = 0;
     }
   }
   itsok = 0;
   while (!itsok) {
     printf("add wait states ? (%s) : ", zerows ? "no" : "yes");
     fgets(response,80,stdin);
     itsok = 1;
     if (response[0] != '\n') {
       if ((response[0] == 'y') || (response[0] == 'Y')) {
	 smc_card_info.mode_bits &= ~ZERO_WAIT_STATE_MASK;
       }
       else {
	 if ((response[0] == 'n') || (response[0] == 'N')) {
	   smc_card_info.mode_bits &= ~ZERO_WAIT_STATE_MASK;
	   smc_card_info.mode_bits |=
	     ZERO_WAIT_STATE_8_BIT | ZERO_WAIT_STATE_16_BIT;
	 }
	 else itsok = 0;
       }
     }
   }
   itsok = 0;
   media = smc_card_info.full_bid & MEDIA_MASK;
   while (!itsok) {
     if ((media == ETHERNET_MEDIA) || (media == EW_MEDIA)) {
       printf("\n1 = aui");
       printf("\n2 = bnc");
     }
     if ((media == TWISTED_PAIR_MEDIA) || (media == EW_MEDIA)) {
       printf("\n3 = twisted pair");
     }
     printf("\nnetwork connection ? ");
     switch(net_interface) {
     case AUI_INTERFACE: printf("(aui)");
       break;
     case AUI_10BT_INTERFACE: printf("(twp)");
       break;
     case BNC_INTERFACE: printf("(bnc)");
       break;
     default: printf("(unknown)");
       break;
     }
     printf(" : ");
     fgets(response,80,stdin);
     if (response[0] != '\n') {
       switch(response[0]) {
       case '1':
	 if ((media == ETHERNET_MEDIA) || (media == EW_MEDIA)) {
	   smc_card_info.mode_bits &= ~INTERFACE_TYPE_MASK;
	   smc_card_info.mode_bits |= AUI_INTERFACE;
	   itsok = 1;
	 }
	 break;
       case '2':
	 if ((media == ETHERNET_MEDIA) || (media == EW_MEDIA)) {
	   smc_card_info.mode_bits &= ~INTERFACE_TYPE_MASK;
	   smc_card_info.mode_bits |= BNC_INTERFACE;
	   itsok = 1;
	 }
	 break;
       case '3': 
	 if ((media == TWISTED_PAIR_MEDIA) || (media == EW_MEDIA)) {
	   smc_card_info.mode_bits &= ~INTERFACE_TYPE_MASK;
	   smc_card_info.mode_bits |= AUI_10BT_INTERFACE;
	   itsok = 1;
	 }
       }
     }
     else itsok = 1;
   }
   smc_card_info.config_mode = STORE_REGS | STORE_EEROM;
   LM_PutCnfg(&smc_card_info);
   /* now see if jumpers are set to soft config position */
   if (jumpers == 0) {
   }
   else {
     printf("Note - Change W1 or W2 to select new soft configuration values.\n");
   }
 }
 exit(0);
}
