// -funaligned-pointers
extern "C" void printf (char *, ...);

#define FAST register

#ifndef apollo
#define ALIGN_LWORD __attribute__ ((aligned (4), packed))
#define ALIGN_WORD __attribute__ ((aligned (2), packed))
#define ALIGN_BYTE __attribute__ ((aligned (1), packed))
#else
#define ALIGN_LWORD
#define ALIGN_WORD
#define ALIGN_BYTE
#endif

#define EXPORT
#define VOID void 

#define OFFSET(x, y) ((BYTE *)(x)-(BYTE *)(y))

typedef unsigned short WORD;
typedef short COUNT;
typedef unsigned long LWORD;
typedef long LCOUNT;
typedef char TEXT;
typedef unsigned char BYTE, TBOOL;
typedef int ARG, BOOL, INT;
typedef float FLOAT;
typedef double DOUBLE;

typedef struct mvtab_hdr MVTAB_HDR;
typedef struct mvtab_hdr
    {
    COUNT mvar_cnt  ALIGN_WORD;  /* Number of entries in table */
    LWORD offset[1] ALIGN_WORD;  /* Table of offsets to entries from beginning of the table */
    } ;

    
BYTE barray[100];


VOID main(VOID)
{         
    MVTAB_HDR *mvtptr;
    COUNT mvno=1;
    LWORD off;
     
    barray[0] = 0;
    barray[1] = 18;
    barray[2] = 0;
    barray[3] = 0;
    barray[4] = 0;
    barray[5] = 0xFF;
    barray[6] = 0;
    barray[7] = 0;
    barray[8] = 0;
    barray[9] = 0xEE;
    barray[10] = 0;
    barray[11] = 0;
    barray[12] = 0;
    barray[13] = 0xDD;

    mvtptr = (MVTAB_HDR *)barray;

    if (mvtptr->offset[mvno] != mvtptr->offset[1])
      printf ("FAIL\n");
    else
      printf ("PASS\n");
}   
