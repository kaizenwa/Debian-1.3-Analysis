typedef struct hsc {
	unsigned char byte0;
	unsigned char head;		/* starting head */
	unsigned char sector;		/* starting sector */
	unsigned char cyl;		/* starting cylinder */
} hsc;

#define head(x) ((x).head)
#define sector(x) ((x).sector & 0x3f)
#define cyl(x) ((x).cyl | (((x).sector & 0xc0)<<2))

#define BEGIN(p) _DWORD((p).start_sect)
#define END(p) (_DWORD((p).start_sect)+(_DWORD((p).nr_sects)))


struct partition {
	hsc start  PACKED;
	hsc end  PACKED;
	unsigned char start_sect[4];	/* starting sector counting from 0 */
	unsigned char nr_sects[4];     	/* nr of sectors in partition */
};

#define boot_ind start.byte0
#define sys_ind end.byte0
