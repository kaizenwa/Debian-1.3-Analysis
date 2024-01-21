/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/*
   FILE
   bitio.c
   Test HDF bit-level I/O routines

   REMARKS

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
   Quincey Koziol

   MODIFICATION HISTORY
   10/19/93 - Started coding.
 */

/* bitio.c,v 1.18 1996/05/16 18:42:39 koziol Exp */

#ifdef RCSID
static char RcsId[] = "@(#)1.18";
#endif

#include "tproto.h"
#include <time.h>

#define TESTFILE_NAME "tbitio.hdf"
#define DATAFILE_NAME "bitio.dat"

/* Last ditch attempt to define this value... */
#ifndef UINT_MAX
#define UINT_MAX (unsigned)(-1)
#endif

#ifndef RAND_MAX
#define RAND_MAX (UINT_MAX)
#endif

/* define aliases for random number generation */
#define RAND rand
#define SEED(a) srand(a)

#define BUFSIZE     8192
#define DATASIZE    4096
#define NUM_BITS    32

#define DATA_TAG_1      1000
#define DATA_REF_1      1000

#define BITIO_TAG_1     1500
#define BITIO_REF_1     1500
#define BITIO_TAG_2     2500
#define BITIO_REF_2     2500

static uint8  outbuf[BUFSIZE],  inbuf[DATASIZE];

static uint32  outbuf2[BUFSIZE],  inbuf2[BUFSIZE],  totbits[BUFSIZE];

static uint32  maskbuf[] =
{
    0x00000000,
    0x00000001, 0x00000003, 0x00000007, 0x0000000f,
    0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff,
    0x000001ff, 0x000003ff, 0x000007ff, 0x00000fff,
    0x00001fff, 0x00003fff, 0x00007fff, 0x0000ffff,
    0x0001ffff, 0x0003ffff, 0x0007ffff, 0x000fffff,
    0x001fffff, 0x003fffff, 0x007fffff, 0x00ffffff,
    0x01ffffff, 0x03ffffff, 0x07ffffff, 0x0fffffff,
    0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff};

static void test_bitio_write(void);
static void test_bitio_read(void);
static void test_bitio_seek(void);

static void
test_bitio_write(void)
{
    int32       fid;
    int32       bitid1;
    int32       ret;
    intn        i;
    uint32      tot_bits = 0;

    MESSAGE(6, printf("Testing bitio write routines\n");
        );
    SEED((int) time(NULL));
    for (i = 0; i < BUFSIZE; i++)
      {
          totbits[i] = tot_bits;
          tot_bits += (uint32) (outbuf[i] = (uint8) (((RAND() >> 4) % 32) + 1));    /* number of bits to output */
          outbuf2[i] = RAND() & maskbuf[outbuf[i]];     /* actual bits to output */
      }     /* end for */

    fid = Hopen(TESTFILE_NAME, DFACC_CREATE, 0);
    CHECK(fid, FAIL, "Hopen");
    bitid1 = Hstartbitwrite(fid, BITIO_TAG_1, BITIO_REF_1, 16);
    CHECK(bitid1, FAIL, "Hstartbitwrite");
    ret = Hbitappendable(bitid1);
    RESULT("Hbitappendable");

    for (i = 0; i < BUFSIZE; i++)
      {
          ret = Hbitwrite(bitid1, outbuf[i], (uint32) outbuf2[i]);
#ifdef TESTING
          printf("outbuf[%d]=%u, outbuf2[%d]=%u, totbits[%d]=%u\n", i, outbuf[i], i, outbuf2[i], i, totbits[i]);
#endif
          VERIFY((uint32) ret, outbuf[i], "Hbitwrite");
      }     /* end for */

    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    bitid1 = Hstartbitread(fid, BITIO_TAG_1, BITIO_REF_1);
    CHECK(bitid1, FAIL, "Hstartbitread");

    for (i = 0; i < BUFSIZE; i++)
      {
          ret = Hbitread(bitid1, outbuf[i], &inbuf2[i]);
#ifdef TESTING
          printf("outbuf[%d]=%u, inbuf2[%d]=%u, totbits[%d]=%u\n", i, outbuf[i], i, inbuf2[i], i, totbits[i]);
#endif
          VERIFY((uint32) ret, outbuf[i], "Hbitread");
      }     /* end for */
    if (HDmemcmp(outbuf2, inbuf2, sizeof(int32) * BUFSIZE))
      {
          printf("Error in writing/reading bit I/O data\n");
          HEprint(stdout, 0);
#ifdef TESTING
          for (i = 0; i < BUFSIZE; i++)
              if (outbuf2[i] != inbuf2[i])
                  printf("outbuf[%d]=%u, outbuf2[%d]=%u inbuf2[%d]=%u, totbits[%d]=%u\n", i, outbuf[i], i, outbuf2[i], i, inbuf2[i], i, totbits[i]);
#endif
          num_errs++;
      }     /* end for */

    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");
    ret = Hclose(fid);
    RESULT("Hclose");
}   /* test_bitio_write() */

static void
test_bitio_read(void)
{
    int32       fid;
    int32       bitid1;
    int32       ret;
    intn        inbits;
    uint32      tempbuf;
    intn        i;
    uint8      *test_ptr;

    SEED((int) time(NULL));

    MESSAGE(6, printf("Testing bitio read routines\n");
        );

    fid = Hopen(DATAFILE_NAME, DFACC_READ, 0);
    CHECK(fid, FAIL, "Hopen");
    ret = Hgetelement(fid, DATA_TAG_1, DATA_REF_1, inbuf);
    RESULT("Hgetelement");
    ret = Hclose(fid);
    RESULT("Hclose");

    MESSAGE(8, printf("Reading 8 bits at a time\n");
        );
    fid = Hopen(DATAFILE_NAME, DFACC_READ, 0);
    CHECK(fid, FAIL, "Hopen");
    bitid1 = Hstartbitread(fid, DATA_TAG_1, DATA_REF_1);
    CHECK(bitid1, FAIL, "Hstartbitread");

    for (i = 0; i < DATASIZE; i++)
      {
          ret = Hbitread(bitid1, 8, &inbuf2[i]);
          VERIFY(ret, 8, "Hbitread");
      }     /* end for */
    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    /* check the data */
    for (i = 0; i < DATASIZE; i++)
      {
          if (inbuf[i] != (uint8) inbuf2[i])
            {
                printf("Error in reading bit I/O data at position %d\n", i);
                num_errs++;
            }   /* end for */
      }     /* end for */

    MESSAGE(8, printf("Read random # of bits at a time\n");
        );
    bitid1 = Hstartbitread(fid, DATA_TAG_1, DATA_REF_1);
    CHECK(bitid1, FAIL, "Hstartbitread");

    /* read in random #'s of bits */
    for (i = 0; i < DATASIZE / (NUM_BITS / 8); i++)
      {
          inbits = ((RAND() >> 4) % NUM_BITS) + 1;  /* number of bits to input */
          ret = Hbitread(bitid1, inbits, &inbuf2[i]);
          VERIFY(ret, inbits, "Hbitread");
          if (inbits < NUM_BITS)
            {   /* if we've already grabbed 32-bit don't try for more */
                inbits = NUM_BITS - inbits;
                ret = Hbitread(bitid1, inbits, &tempbuf);
                VERIFY(ret, inbits, "Hbitread");
                inbuf2[i] <<= inbits;
                inbuf2[i] |= tempbuf;
            }   /* end if */
      }     /* end for */
    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    test_ptr = (uint8 *) HDmalloc((DATASIZE / 4) * DFKNTsize(DFNT_UINT32));
    CHECK(test_ptr, NULL, "HDmalloc");
    ret = DFKconvert(inbuf2, test_ptr, DFNT_UINT32, (DATASIZE / 4), DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    /* check the data */
    if (HDmemcmp(inbuf, test_ptr, DATASIZE) != 0)
      {
          printf("Error in reading bit I/O data\n");
          HEprint(stdout, 0);
          num_errs++;
      }     /* end if */
    HDfree(test_ptr);

    ret = Hclose(fid);
    RESULT("Hclose");
}   /* test_bitio_read() */

static void
test_bitio_seek(void)
{
    int32       fid;
    int32       bitid1;
    int32       ret;
    intn        i;

    SEED((int) time(NULL));

    MESSAGE(6, printf("Testing bitio seek routines\n");
        );

    fid = Hopen(TESTFILE_NAME, DFACC_READ | DFACC_WRITE, 0);
    CHECK(fid, FAIL, "Hopen");

    MESSAGE(8, printf("Seek & read from start of dataset\n");
        );
    bitid1 = Hstartbitread(fid, BITIO_TAG_1, BITIO_REF_1);
    CHECK(bitid1, FAIL, "Hstartbitread");

    for (i = 0; i < BUFSIZE; i++)
      {
          ret = Hbitseek(bitid1, totbits[i] / 8, (intn) (totbits[i] % 8));
          CHECK(ret, FAIL, "Hbitseek");
          ret = Hbitread(bitid1, outbuf[i], &inbuf2[i]);
          VERIFY((uint32) ret, outbuf[i], "Hbitread");
          if (outbuf2[i] != inbuf2[i])
              printf("outbuf[%d]=%u, outbuf2[%d]=%lu inbuf2[%d]=%lu, totbits[%d]=%lu\n", i, outbuf[i], i, (long) outbuf2[i], i, (long) inbuf2[i], i, (long) totbits[i]);
      }     /* end for */

    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    MESSAGE(8, printf("Seek & read from end of dataset\n");
        );
    bitid1 = Hstartbitread(fid, BITIO_TAG_1, BITIO_REF_1);
    CHECK(bitid1, FAIL, "Hstartbitread");

    for (i = BUFSIZE - 1; i >= 0; i--)
      {
          ret = Hbitseek(bitid1, totbits[i] / 8, (intn) (totbits[i] % 8));
          CHECK(ret, FAIL, "Hbitseek");
          ret = Hbitread(bitid1, outbuf[i], &inbuf2[i]);
          VERIFY((uint32) ret, outbuf[i], "Hbitread");
          if (outbuf2[i] != inbuf2[i])
              printf("outbuf[%d]=%u, outbuf2[%d]=%lu inbuf2[%d]=%lu, totbits[%d]=%lu\n", i, outbuf[i], i, (long) outbuf2[i], i, (long) inbuf2[i], i, (long) totbits[i]);
      }     /* end for */

    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    MESSAGE(8, printf("Seek & write from start of dataset\n");
        );
    bitid1 = Hstartbitwrite(fid, BITIO_TAG_1, BITIO_REF_1, 16);
    CHECK(bitid1, FAIL, "Hstartbitwrite");

    MESSAGE(9, printf("Writing new data to every other bit-sequence\n");
        );
    /* re-write every other sequence of bits in the dataset */
    for (i = 0; i < BUFSIZE; i += 2)
      {
          outbuf2[i] = RAND() & maskbuf[outbuf[i]];     /* actual bits to output */

          ret = Hbitseek(bitid1, totbits[i] / 8, (intn) (totbits[i] % 8));
          CHECK(ret, FAIL, "Hbitseek");
          ret = Hbitwrite(bitid1, outbuf[i], (uint32) outbuf2[i]);
          VERIFY((uint32) ret, outbuf[i], "Hbitwrite");
      }     /* end for */

    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    MESSAGE(9, printf("Verifying new data\n");
        );
    bitid1 = Hstartbitread(fid, BITIO_TAG_1, BITIO_REF_1);
    CHECK(bitid1, FAIL, "Hstartbitread");

    for (i = 0; i < BUFSIZE; i++)
      {
          ret = Hbitseek(bitid1, totbits[i] / 8, (intn) (totbits[i] % 8));
          CHECK(ret, FAIL, "Hbitseek");
          ret = Hbitread(bitid1, outbuf[i], &inbuf2[i]);
          VERIFY((uint32) ret, outbuf[i], "Hbitread");
          if (outbuf2[i] != inbuf2[i])
              printf("outbuf[%d]=%u, outbuf2[%d]=%lu inbuf2[%d]=%lu, totbits[%d]=%lu\n", i, outbuf[i], i, (long) outbuf2[i], i, (long) inbuf2[i], i, (long) totbits[i]);
      }     /* end for */

    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    MESSAGE(8, printf("Seek & write from end of dataset\n");
        );
    bitid1 = Hstartbitwrite(fid, BITIO_TAG_1, BITIO_REF_1, 16);
    CHECK(bitid1, FAIL, "Hstartbitwrite");

    MESSAGE(9, printf("Writing new data to every other bit-sequence from the end\n");
        );
    /* re-write every other sequence of bits in the dataset */
    for (i = BUFSIZE - 1; i >= 0; i -= 2)
      {
          outbuf2[i] = RAND() & maskbuf[outbuf[i]];     /* actual bits to output */

          ret = Hbitseek(bitid1, totbits[i] / 8, (intn) (totbits[i] % 8));
          CHECK(ret, FAIL, "Hbitseek");
          ret = Hbitwrite(bitid1, outbuf[i], (uint32) outbuf2[i]);
          VERIFY((uint32) ret, outbuf[i], "Hbitwrite");
      }     /* end for */

    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    MESSAGE(9, printf("Verifying new data again\n");
        );
    bitid1 = Hstartbitread(fid, BITIO_TAG_1, BITIO_REF_1);
    CHECK(bitid1, FAIL, "Hstartbitread");

    for (i = 0; i < BUFSIZE; i++)
      {
          ret = Hbitseek(bitid1, totbits[i] / 8, (intn) (totbits[i] % 8));
          CHECK(ret, FAIL, "Hbitseek");
          ret = Hbitread(bitid1, outbuf[i], &inbuf2[i]);
          VERIFY((uint32) ret, outbuf[i], "Hbitread");
          if (outbuf2[i] != inbuf2[i])
              printf("outbuf[%d]=%u, outbuf2[%d]=%lu inbuf2[%d]=%lu, totbits[%d]=%lu\n", i, outbuf[i], i, (long) outbuf2[i], i, (long) inbuf2[i], i, (long) totbits[i]);
      }     /* end for */

    ret = Hendbitaccess(bitid1, 0);
    RESULT("Hbitendaccess");

    ret = Hclose(fid);
    RESULT("Hclose");
}   /* test_bitio_seek() */

void
test_bitio()
{
    test_bitio_read();
    test_bitio_write();
    test_bitio_seek();
}
