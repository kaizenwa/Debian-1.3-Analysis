/* pu_defs.h					*/
/* definitions for the pic read/write routines	*/
/* I.King 1994					*/

#define MAXPICSIZE 8192

typedef struct picdefn
	{
		unsigned short	picmemmap[MAXPICSIZE];
		int		pictype;
		int		picid[4];
		float		clock;
		int		osctype;
		int		cp_fuse;
		int		wd_fuse;
		int		pu_fuse;
	} PICDEFN;

#define PU_OK	1
#define PU_FAIL	0

extern void PU_Clear();
extern int  PU_WriteHeader();
extern int  PU_WriteBlock();
extern int  PU_WriteTrailer();
extern int	PU_Read(const char *filename, PICDEFN *pic, int *top);

/* ... The End ... */
