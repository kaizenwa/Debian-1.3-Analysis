#define HostStr "AT&T 6386 WGS 3.1 1 i386"
#define MaxHdr  4708

#define index strchr
#define rindex strrchr

/*#define i386  /* already defined - if not, define it!*/

#define MaxAbrSize		512000	/* size of block region in bytes */
#define MaxStrSpace		256000	/* size of string space in bytes */
#define MaxStatSize		 6000	/* size of static region in bytes */
#define QualLstSize		17000	/* size of qualifier pointer region */
#define MStackSize		17000	/* size of the main stack in words */
#define StackSize		 4000	/* size of co-expression stack */

#define UNIX 1
