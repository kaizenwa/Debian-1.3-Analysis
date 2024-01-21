




/* command fields */

#define NOPER		0
#define PIXEL		1	
#define FRAME		2
#define FILER		3


/* PIXEL and FRAME command modes */

#define SET		1
#define ADD		2
#define AND		3
#define XOR		4


/* FILER subcommands	*/

#define INITI	1
#define GNAME	2
#define SNAME	3
#define WFILE	4
#define RFILE	5


typedef struct _position {
	short x, y, z;
} position;


typedef struct _cmdrec {
	short	command;
	short	count;
	short	x, y, z;
	short	dx, dy, mode;
} cmdrec;
