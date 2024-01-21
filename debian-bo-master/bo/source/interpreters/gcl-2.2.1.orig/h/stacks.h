#ifndef MAXPAGE
#define MAXPAGE 32768
#endif
#ifndef VSSIZE
#define VSSIZE 8192
#endif

#define VSGETA 128
EXTER object value_stack[VSSIZE + (STACK_OVER +1) *VSGETA];     

#define BDSSIZE		1024
#define	BDSGETA		40
EXTER struct bds_bd bind_stack[BDSSIZE + (STACK_OVER +1)* BDSGETA];

     
#define	IHSSIZE		1024
#define	IHSGETA		32
EXTER struct invocation_history ihs_stack[IHSSIZE + (STACK_OVER +1) * IHSGETA];     


#define FRSSIZE		1024
#define	FRSGETA		16
EXTER struct frame frame_stack[FRSSIZE + (STACK_OVER +1) * FRSGETA];

