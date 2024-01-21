/* resampling module 
 *
 * The audio data has been read. Here are the
 * functions to ensure a correct continuation
 * of the output stream and to convert to a
 * lower sample rate.
 *
 */

#define SYNC_OFF	150L
#define SYNC_SIZE	320L	/* has to be smaller than CD_FRAMESAMPLES */
#undef    DEBUG_SHIFTS		/* simulate bad cdrom drives */
#undef SHOW_JITTER

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <limits.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <linux/cdrom.h>

#include "cdda2wav.h"
#include "interface.h"
#include "byteorder.h"


long waitforsignal = 0;	/* flag: wait for any audio response */

short undersampling;	/* conversion factor */
short samples_to_do;	/* loop variable for conversion */
int Halved;		/* interpolate due to non integral divider */

static long lsum = 0, rsum = 0;	       /* accumulator for left/right channel */
static long ls2 = 0, rs2 = 0, ls3 = 0, rs3 = 0, auxl = 0, auxr = 0;

#ifndef USE_GNUMEMMEM
#if 0
/* smarter but slower than brute force :-( */
static void 
init_search(const unsigned char * NEEDLE, size_t NEEDLE_LEN, size_t shift[])
{
  size_t *sp;
  size_t c;

  NEEDLE_LEN++;
  sp = shift;

  /* initialize jump table for characters non existent in NEEDLE */
  for ( c = 1 + (size_t)UCHAR_MAX; c-- > 0; ) *sp++ = NEEDLE_LEN;

  /* initialize jump table for characters existent in NEEDLE */
  for (; --NEEDLE_LEN != 0; shift[(int)*(unsigned char *)(NEEDLE++)] = NEEDLE_LEN );
}

static const unsigned char *
my_memmem (const unsigned char * const HAYSTACK, const size_t HAYSTACK_LEN,
	   const unsigned char * const NEEDLE, const size_t NEEDLE_LEN)
{
  const unsigned char * const UPPER_LIMIT = HAYSTACK + HAYSTACK_LEN;

  static size_t shift[1 + (int)UCHAR_MAX];
  const unsigned char *cp;
  
  if (HAYSTACK_LEN < NEEDLE_LEN) return NULL;
  if (HAYSTACK == NULL || HAYSTACK_LEN == 0) return NULL;
  if (NEEDLE == NULL || NEEDLE_LEN == 0) return HAYSTACK;

  init_search(NEEDLE, NEEDLE_LEN, shift);

  cp = HAYSTACK + NEEDLE_LEN;
  while (cp <= (const unsigned char *) UPPER_LIMIT) {
    const unsigned char *p = NEEDLE;
    const unsigned char *t = cp - NEEDLE_LEN;

    do {
      if ((NEEDLE + NEEDLE_LEN) == p) {
	  return (void *) (cp - NEEDLE_LEN);
      }
    } while ( *p++ == *t++ );
    cp += shift[(int)*cp];
  }
  return NULL;
}

#else
/* A simple brute force approach */
/* A bit faster than gnu memmem and much faster than the smart memmem above */
static const unsigned char *
my_memmem (const unsigned char * HAYSTACK, const size_t HAYSTACK_LEN,
	   const unsigned char * const NEEDLE, const size_t NEEDLE_LEN)
{
  const unsigned char * const UPPER_LIMIT = HAYSTACK + HAYSTACK_LEN - NEEDLE_LEN;

  if (HAYSTACK_LEN < NEEDLE_LEN) return NULL;
  if (HAYSTACK == NULL || HAYSTACK_LEN == 0) return NULL;
  if (NEEDLE == NULL || NEEDLE_LEN == 0) return HAYSTACK;

  while (HAYSTACK <= UPPER_LIMIT) {
    if (memcmp(NEEDLE, HAYSTACK, NEEDLE_LEN) == 0) {
      return HAYSTACK;
    } else {
      HAYSTACK++;
    }
  }
  return NULL;
}
#endif
#else
#define my_memmem memmem
#endif

/* find continuation in new buffer */
static const unsigned char *
sync_buffers(const unsigned char * const newbuf)
{
    const unsigned char *retval = newbuf;

    if (overlap != 0) {
      /* find position of SYNC_SIZE bytes 
	 of the old buffer in the new buffer */
      size_t haystack_len;
      size_t needle_len   = SYNC_SIZE;
      const unsigned char * const oldbuf = get_previous_read_buffer();
      const unsigned char * needle = oldbuf + nsectors*CD_FRAMESIZE_RAW - SYNC_SIZE - SYNC_OFF;
      const unsigned char * haystack;

      /* compare the end of the previous buffer with the beginning
       * of the new one
       *
       * At first we try from the exact position.
       * When no match occurs, we search in the head part of the new buffer.
       * When no match occurs, we search in the tail part of the new buffer.
       * When no match occurs, the sequence cannot be found.
       */

      haystack = newbuf + overlap*CD_FRAMESIZE_RAW - SYNC_SIZE - SYNC_OFF;

#ifdef DEBUG_SHIFTS
#define SIM_SHIFTBYTES (-2)
      retval =  haystack + SYNC_SIZE + SYNC_OFF + SIM_SHIFTBYTES;
#else
      if (memcmp(haystack, needle, needle_len) == 0) {
	retval = haystack + SYNC_SIZE + SYNC_OFF;
      } else {
	/* not exact */
	haystack = newbuf;
	haystack_len = nsectors*CD_FRAMESIZE_RAW;
	retval = my_memmem(haystack, haystack_len, needle, needle_len);

	if (retval) {
          retval += SYNC_SIZE + SYNC_OFF;
	}
      }

#endif

#ifdef SHOW_JITTER
      if (retval) {
	fprintf(stderr,"matched\tj1=%d\t",
		retval-(newbuf+overlap*CD_FRAMESIZE_RAW));
      } else {
	fprintf(stderr,"no match\n");
      }
#endif
    }
    return retval;
}

/* quadratic interpolation
 * p1, p3 span the interval 0 - 2. give interpolated value for 1/2 */
static long int 
interpolate( long int p1, long int p2, long int p3)
{
  return (3L*p1 + 6L*p2 - p3)/8L;
}

static int any_signal = 0;
static unsigned char *pStart;	/* running ptr defining end of output buffer */
static unsigned char *pDst;	/* start of output buffer */
/*
 * Write the filtered sample into the output buffer.
 */
static void 
emit_sample( long lsumval, long rsumval, long channels )
{
    /* convert to output format */
    if ( channels == 1 ) {
	short sum;       /* mono section */
	sum = ( lsumval + rsumval ) >> (sh_bits + 1);
	if ( sh_bits == 8 ) {
	    if ( ( (char) sum) != '\0' ) {
		if ( any_signal == 0 ) {
		    pStart = pDst;
		    any_signal = 1;
		}
	    }
	    *pDst++ = ( unsigned char ) sum + ( 1 << 7 );
	} else {
	    short * myptr = (short *) pDst;
	    if ( sum != 0 ) {
		if ( any_signal == 0 ) {
		    pStart = pDst;
		    any_signal = 1;
		}
	    }
	    *myptr = sum;
	    pDst += sizeof( short );
	}
    } else {
	/* stereo section */
	lsumval >>= sh_bits;
	rsumval >>= sh_bits;
	if ( sh_bits == 8 ) {
	    if ( (( char ) lsumval != '\0') || (( char ) rsumval != '\0')) {
		if ( any_signal == 0 ) {
		    pStart = pDst;
		    any_signal = 1;
		}
	    }
	    *pDst++ = ( unsigned char )( short ) lsumval + ( 1 << 7 );
	    *pDst++ = ( unsigned char )( short ) rsumval + ( 1 << 7 );
	} else {
	    short * myptr = (short *) pDst;
	    if ( (( short ) lsumval != 0) || (( short ) rsumval != 0)) {
		if ( any_signal == 0 ) {
		    pStart = pDst;
		    any_signal = 1;
		}
	    }
	    *myptr++ = ( short ) lsumval;
	    *myptr   = ( short ) rsumval;
	    pDst += 2*sizeof( short );
	}
    }
}

static void
change_endianness(char *pSam, unsigned Samples)
{
#if !defined i386 || defined PORTABLE
    unsigned long *plong = (unsigned long *)pSam;
    const unsigned long const * pend = 
                           (unsigned long *)(pSam + Samples*4);

    for (;(unsigned char *)plong < pend;)
      *plong++ = ((*plong >> 8) & 0x00ff00ff00ff00ff) |
                 ((*plong << 8) & 0xff00ff00ff00ff00);
#else
    __asm__ ("cld\n\t"
	     "1: movl 0(%%edi),%%eax\n\t"
	     "   bswap %%eax\n\t"
	     "   rorl $16, %%eax\n\t"
	     "   stosl\n\t"
	     "   cmpl %%edx,%%edi\n\t"
	     "   jb 1b\n\t":
	     :"D" ((long) pSam),
	      "d" ((long) pSam + Samples*4));
#endif
}

int jitterShift = 0; 

/* convert cdda data to required output format
 * sync code for panasonic cdroms included
 * 
 */
long 
SaveBuffer (unsigned char *p, long channels, 
	    long SamplesToDo, long *TotSamplesDone, long TotSamplesWanted)
{
  char *pSrc;		        /* start of cdrom buffer */
  char *pSrcStop;	        /* end of cdrom buffer */
  static int jitter = 0; 

  if (lowendian == -1) {
    short *p2 = (short *)p;

    while ((((unsigned char *)p2 - p) < 4*SamplesToDo-4) && *p2 == *(p2+2)) p2++;
    if ((((unsigned char *)p2 - p) < 4*SamplesToDo-4)) {
      /* analyse samples */
      unsigned char *p3 = (unsigned char *)p2;
      int diff_lowl = *(p2+0) - *(p2+2);
      int diff_lowr = *(p2+1) - *(p2+3);
      int diff_bigl = ((*(p3  ) << 8) + *(p3+1)) - ((*(p3+4) << 8) + *(p3+5));
      int diff_bigr = ((*(p3+2) << 8) + *(p3+3)) - ((*(p3+6) << 8) + *(p3+7));

      if ((abs(diff_lowl) + abs(diff_lowr)) <
	  (abs(diff_bigl) + abs(diff_bigr))) {
	fprintf(stderr, "low endian detected\n");
	lowendian = 1;
      } else
	if ((abs(diff_lowl) + abs(diff_lowr)) >
	    (abs(diff_bigl) + abs(diff_bigr))) {
	  fprintf(stderr, "big endian detected\n");
	  lowendian = 0;
	}
    }
  }

  /* ENDIAN ISSUES:
   * the individual endianess of cdrom/cd-writer, cpu, 
   * sound card and audio output format need a careful treatment.
   *
   * For possible sample processing (rate conversion) we need
   * the samples in cpu byte order. This is the first conversion.
   *
   * After processing it depends on the endianness of the output
   * format, whether a second conversion is needed.
   *
   */

  if (lowendian != MY_LOW_ENDIAN) {
    /* change endianess of delivered samples to native cpu order */
    change_endianness(p, SamplesToDo);
  }

  /* synchronisation code */

  if (*TotSamplesDone != 0 && overlap != 0 && SamplesToDo > CD_FRAMESAMPLES) {

    pSrc = (char *) sync_buffers(p);
    if (!pSrc ) {
#if 0

#define OLD_READ "__buff_old"
#define NEW_READ "__buff_new"
      { FILE *b1 = fopen(OLD_READ, "wb");
	FILE *b2 = fopen(NEW_READ, "wb");
	unsigned char *oldbuf = get_previous_read_buffer();
	if (b1 != NULL && b2 != NULL) {
	  fprintf(b1,"old: version "VERSION", interface %5.5s, nsectors %2u, "
		      "overlap %1u, SYNC_SIZE %4ld, BytesToDo %6u\n",
		      interface == GENERIC_SCSI ? "SCSI" : "ATAPI",
		      nsectors, overlap, SYNC_SIZE, nsectors*CD_FRAMESIZE_RAW);
	  if (fwrite(oldbuf, 1, nsectors*CD_FRAMESIZE_RAW, b1) !=
	                        nsectors*CD_FRAMESIZE_RAW)
	    perror("write error dump1:");
	  (void) fclose(b1);

	  fprintf(b2,"new: version "VERSION", interface %5.5s, nsectors %2u, "
		     "overlap %1u, SYNC_SIZE %3ld, BytesToDo %6ld\n",
		  interface == GENERIC_SCSI ? "SCSI" : "ATAPI",
		  nsectors, overlap, SYNC_SIZE, SamplesToDo*4);
	  if (fwrite(p, 1, (size_t) SamplesToDo*4, b2) !=
	                   (size_t) SamplesToDo*4)
	    perror("write error dump2:");
	  (void) fclose(b2);

	  fprintf(stderr, "Could not synchronize successive reads.\n"
		          "Please email the files "OLD_READ" and "NEW_READ"\n"
                          "in uuencoded form to heiko@colossus.escape.de\n");
	  return 1;
	}
      }
#else
      fprintf(stderr, "no match: increase overlap (%u) or decrease SYNC_SIZE (%ld).\n", overlap, SYNC_SIZE);
      fprintf(stderr, "no match: at position %ld from %ld\n",
	      *TotSamplesDone, TotSamplesWanted);
#endif
      pSrc = (char *) p;
    }
    jitter = ((unsigned char *)pSrc - (p + overlap*CD_FRAMESIZE_RAW))/4;
    jitterShift += jitter;
    SamplesToDo -= jitter + overlap*CD_FRAMESAMPLES;
#if 0
    fprintf(stderr,
	    "Length: pre %d, diff1 %ld, diff2 %ld, min %ld\n", SamplesToDo,
	   (TotSamplesWanted - *TotSamplesDone),
	   SamplesNeeded((TotSamplesWanted - *TotSamplesDone), undersampling),
	   min(SamplesToDo, SamplesNeeded((TotSamplesWanted - *TotSamplesDone), undersampling)));
#endif
    SamplesToDo = min(SamplesToDo, SamplesNeeded((TotSamplesWanted - *TotSamplesDone), undersampling));

    p = (unsigned char *) pSrc;
  } else {
    pSrc = ( char * ) p;
  }

  pDst = ( unsigned char * ) pSrc;
  pStart = ( unsigned char * ) pSrc;
  pSrcStop = pSrc + SamplesToDo*4;
#ifdef SHOW_JITTER
  if (overlap)
    fprintf(stderr, "Total: %d, Jitter: %d\n", jitterShift, jitter);
#endif

  /* code for subsampling and output stage */

  /* optimize the case of no conversion */
  if ( undersampling == 1 && samples_to_do == 1 &&
       channels == 2 && OutSampleSize == 2 && Halved == 0) {
      /* output format is the original cdda format ->
       * just forward the buffer 
       */
      
      if ( waitforsignal != 0 && any_signal == 0) {
	  long *myptr = (long *)pStart;
	  while ((char *)pStart < pSrcStop && *myptr++ == 0);
	  pStart = (unsigned char *) myptr;
	  /* scan for first signal */
	  if ( (char *)pStart != pSrcStop ) {
	      /* first non null amplitude is found in buffer */
	      any_signal = 1;
	  }
      }
      pDst = (unsigned char *) pSrcStop;		/* set pDst to end */
  } else {

#define none_missing	0
#define one_missing	1
#define two_missing	2
#define collecting	3

    static int sample_state = collecting;
    static int Toggle_on = 0;

    /* conversion required */
    while ( pSrc < pSrcStop ) {
	  
	long l,r;

	long iSamples_left = (pSrcStop - pSrc) / sizeof(signed short) / 2;
	signed short *myptr = (signed short *) pSrc;

	/* LSB l, MSB l */
	l = *myptr++;	/* left channel */
	r = *myptr++;	/* right channel */
	pSrc = (char *) myptr;

	switch (sample_state) {
	case two_missing:
two__missing:
	    ls2 += l; rs2 += r;
	    if (undersampling > 1) {
		ls3 += l; rs3 += r;
	    }
	    sample_state = one_missing;
	    break;
	case one_missing:
	    auxl = l; auxr = r;

	    ls3 += l; rs3 += r;
	    sample_state = none_missing;

	    /* FALLTHROUGH */
none__missing:
	case none_missing:
	    /* Filtered samples are complete. Now interpolate and scale. */

	    if (Halved != 0 && Toggle_on == 0) {
                lsum = interpolate(lsum, ls2, ls3)/(signed) undersampling;
	        rsum = interpolate(rsum, rs2, rs3)/(signed) undersampling;
            } else {
		lsum /= (signed) undersampling;
		rsum /= (signed) undersampling;
            }
	    emit_sample(lsum, rsum, channels);
	    /* reload counter */
	    samples_to_do = undersampling - 1;
	    lsum = auxl;
	    rsum = auxr;
	    /* reset sample register */
	    auxl = ls2 = ls3 = 0;
	    auxr = rs2 = rs3 = 0;
	    Toggle_on ^= 1;
	    sample_state = collecting;
	    break;
	case collecting:
	    if ( samples_to_do > 0) {
		samples_to_do--;
		if (Halved != 0 && Toggle_on == 0) {
		    /* Divider x.5 : we need data for quadratic interpolation */
		    iSamples_left--;

		    lsum += l; rsum += r;
		    if ( samples_to_do < undersampling - 1) {
			ls2 += l; rs2 += r;
		    }
		    if ( samples_to_do < undersampling - 2) {
			ls3 += l; rs3 += r;
		    }
		} else {
		    /* integral divider */
		    lsum += l;
		    rsum += r;
		    iSamples_left--;
		}
	    } else {
	        if (Halved != 0 && Toggle_on == 0) {
		    sample_state = two_missing;
		    goto two__missing;
		} else {
		    auxl = l;
		    auxr = r;
		    sample_state = none_missing;
		    goto none__missing;
		}
	    }
	    break;
	} /* switch state */

    } /* while */

    /* flush_buffer */
    if ((samples_to_do == 0 && Halved == 0) ||
         (*TotSamplesDone + (pDst - pStart)/OutSampleSize/channels == TotSamplesWanted-1))
    {
	if (Halved != 0 && Toggle_on == 0) {
	    lsum = interpolate(lsum, ls2, ls3)/(signed) undersampling;
	    rsum = interpolate(rsum, rs2, rs3)/(signed) undersampling;
	} else {
	    lsum /= (signed) undersampling;
	    rsum /= (signed) undersampling;
	}
	emit_sample(lsum, rsum, channels);
	
	/* reload counter */
	samples_to_do = undersampling;
	
	/* reset sample register */
	lsum = auxl = ls2 = ls3 = 0;
	rsum = auxr = rs2 = rs3 = 0;
	Toggle_on ^= 1;
	sample_state = collecting;
    }

  } /* if optimize else */

  if ( waitforsignal == 0 ) pStart = (unsigned char *)p;
  else if (any_signal != 0) SkippedSamples += (pStart - p)/4;
  else SkippedSamples += (pSrcStop - (char *)p)/4;

  if ( waitforsignal == 0 || any_signal != 0) {
    int retval = 0;
    size_t outlen;

    assert(pDst >= pStart);
    outlen = (size_t) (pDst - pStart);

    if (outlen <= 0) return 0;

#ifdef	ECHO_TO_SOUNDCARD
    if (echo != 0) {
        if (write(soundcard_fd, pStart, outlen) != (ssize_t) outlen)
	  perror("");
    }
#endif

    if ( no_file != 0 ) {
        outlen /= OutSampleSize*channels;
        *TotSamplesDone += outlen;
        return 0;
    }
    if ( need_big_endian != MY_BIG_ENDIAN && OutSampleSize > 8) {
      /* change endianness from native cpu order 
         to required output endianness */
      change_endianness(pStart, *TotSamplesDone);
    }
    if ((retval = write ( audio, pStart, outlen )) == (ssize_t) outlen) {
	outlen /= OutSampleSize*channels;
	*TotSamplesDone += outlen;
	return 0;
    } else {
        fprintf(stderr, "write(audio, 0x%p, %u) = %d\n",pStart,outlen,retval);
        perror("Probably disk space exhausted");
        return 1;
    }
  } else return 0;
}


