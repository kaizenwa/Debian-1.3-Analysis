/* Map Linux RESTART* values (512,513,514) to EINTR */
static unsigned char LNX_err_table[] = {
	EINTR, EINTR, EINTR
};


/* Default Linux to iBCS mapping.
 * We good remove some of the long identity mapped runs but at the
 * expense of extra comparisons for each mapping at run time...
 */
static unsigned char SVR4_err_table[] = {
	  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
	 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
	 32, 33, 34, 45, 78, 46, 89, 93, 90, 90, 35, 36, 37, 38, 39, 40,
	 41, 42, 43, 44, 50, 51, 52, 53, 54, 55, 56, 57, 60, 61, 62, 63,
	 64, 65, 66, 67, 68, 69, 70, 71, 74, 76, 77, 79, 80, 81, 82, 83,
	 84, 85, 86, 87, 88, 91, 92, 94, 95, 96, 97, 98, 99,120,121,122,
	123,124,125,126,127,128,129,130,131,132,133,134,143,144,145,146,
	147,148,149,150, 22,135,137,138,139,140, 28
};


/* SCO exceptions to the default iBCS mapping.
 * Table 1 covers errno = 39 to 40.
 * Table 2 covers errno = 75 to 122.
 */
static unsigned char SCO_err_table1[] = {
	145,150
};
static unsigned char SCO_err_table2[] = {
	 79, 80, 81, 82, 83, 84, 85, 86, 87, 88,152,153, 22, 93, 94,
	 95, 96,118, 97, 98, 99,100,101,102,103,104,105,106,107,108,
	 63,110,111,112,113,114,115,116,117, 92, 91,151,135,137,138,
	139,140, 28
};


/* Wyse exceptions to the default iBCS mapping.
 * Table 1 covers errno = 38 to 41.
 * Table 2 covers errno = 75 to 122.
 */
static unsigned char WYSE_err_table1[] = {
	228, 46, 22,231,227,200
};
static unsigned char WYSE_err_table2[] = {
	 22, 80, 81, 82, 83, 84, 85, 86, 87, 22,  4, 22,233,203,204,
	205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,
	220,221,222,223,224,225,226,229,230,202,201,237,135,137,138,
	139,140,234
};


/* SVR4 (aka the full official iBCS) is the base mapping - no exceptions,
 * other than the RESTART* values.
 */
static struct map_segment SVR4_err_map[] = {
	{ 0,	0+sizeof(SVR4_err_table)-1,	SVR4_err_table },
	{ 512,	512+sizeof(LNX_err_table)-1,	LNX_err_table },
	{ -1 }
};


static struct map_segment SCO_err_map[] = {
	{ 39,	39+sizeof(SCO_err_table1)-1,	SCO_err_table1 },
	{ 75,	75+sizeof(SCO_err_table2)-1,	SCO_err_table2 },
	{ 0,	0+sizeof(SVR4_err_table)-1,	SVR4_err_table },
	{ 512,	512+sizeof(LNX_err_table)-1,	LNX_err_table },
	{ -1 }
};


static struct map_segment WYSE_err_map[] = {
	{ 38,	38+sizeof(WYSE_err_table1)-1,	WYSE_err_table1 },
	{ 75,	75+sizeof(WYSE_err_table2)-1,	WYSE_err_table2 },
	{ 0,	0+sizeof(SVR4_err_table)-1,	SVR4_err_table },
	{ 512,	512+sizeof(LNX_err_table)-1,	LNX_err_table },
	{ -1 }
};


#ifdef EMU_BSD
static unsigned char BSD_err_table1[] = {
	11, 63, 77, 78, 66, 62, 35
};
static unsigned char BSD_err_table2[] = {
	68, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
	53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 65, 37, 36, 70, EINVAL,
	EINVAL, EINVAL, EINVAL, EINVAL, 69
};
static struct map_segment BSD_err_map[] = {
	{ 11,	11,				(unsigned char *)35 },
	{ 35,	35+sizeof(BSD_err_table1)-1,	BSD_err_table1 },
	{ 66,	66,				(unsigned char *)71 },
	{ 87,	87+sizeof(BSD_err_table2)-1,	BSD_err_table2 },
	{ 0,	1000,				NULL },
	{ -1 }
};
#endif /* EMU_BSD */


/* Since the original error numbers come from the Linux set the Linux
 * personality uses a null mapping for everything. Why Linux programs
 * are making iBCS calls I don't know...
 */
static struct map_segment LINUX_err_map[] = {
	{ 0, 1000, NULL },
	{ -1 }
};


struct map_segment *err_map[] = {
	LINUX_err_map,
	SVR4_err_map,
	SVR4_err_map,	/* SVR3 is a subset of SVR4 */
	SCO_err_map,
	WYSE_err_map,
	SVR4_err_map,	/* ISC4 - not checked yet */
#ifdef EMU_BSD
	BSD_err_map,	/* BSD-ish */
#else
	NULL,
#endif
	SVR4_err_map	/* Xenix is a subset of SVR3. */
};
