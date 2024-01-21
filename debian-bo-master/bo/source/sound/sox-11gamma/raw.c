/*
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

/*
 * Sound Tools raw format file.
 *
 * Includes .ub, .uw, .sb, .sw, and .ul formats at end
 */

/*
 * Notes: most of the headerless formats set their handlers to raw
 * in their startread/write routines.  
 *
 */

#include "st.h"
#include "libst.h"

rawstartread(ft) 
ft_t ft;
{
}

rawstartwrite(ft) 
ft_t ft;
{
}

/* Read raw file data, and convert it to */
/* the sox internal signed long format. */

rawread(ft, buf, nsamp) 
ft_t ft;
long *buf, nsamp;
{
	register long datum;
	int done = 0;

	switch(ft->info.size) {
		case BYTE: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					datum = getc(ft->fp);
					if (feof(ft->fp))
						return done;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 24);
					done++;
				}
				return done;
			case UNSIGNED:
				while(done < nsamp) {
					datum = getc(ft->fp);
					if (feof(ft->fp))
						return done;
					/* Convert to signed */
					datum ^= 128;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 24);
					done++;
				}
				return done;
			case ULAW:
				while(done < nsamp) {
					datum = getc(ft->fp);
					if (feof(ft->fp))
						return done;
					datum = st_ulaw_to_linear(datum);
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				return done;
			case ALAW:
				while(done < nsamp) {
				        datum = getc(ft->fp);
				        if (feof(ft->fp))
				                return done;
				        datum = st_Alaw_to_linear(datum);
				        /* scale signed up to long's range */
				        *buf++ = LEFT(datum, 16);
				        done++;
				}

				return done;
			}
		case WORD: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					datum = rshort(ft);
					if (feof(ft->fp))
						return done;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				return done;
			case UNSIGNED:
				while(done < nsamp) {
					datum = rshort(ft);
					if (feof(ft->fp))
						return done;
					/* Convert to signed */
					datum ^= 0x8000;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				return done;
			case ULAW:
				fail("No U-Law support for shorts");
				return done;
			case ALAW:
				fail("No A-Law support for shorts");
				return done;
			}
		case FLOAT:
			while(done < nsamp) {
				datum = dovolume? volume * rfloat(ft)
						: rfloat(ft);
				if (feof(ft->fp))
					return done;
				*buf++ = LEFT(datum, 16);
				done++;
			}
			return done;
		default:
			fail("Drop through in rawread!");

		}
	fail("Sorry, don't have code to read %s, %s",
		styles[ft->info.style], sizes[ft->info.size]);
}

/* Convert the sox internal signed long format */
/* to the raw file data, and write it. */

void
rawwrite(ft, buf, nsamp) 
ft_t ft;
long *buf, nsamp;
{
	register int datum;
	int done = 0;

	switch(ft->info.size) {
		case BYTE: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 24);
					putc(datum, ft->fp);
					done++;
				}
				return;
			case UNSIGNED:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 24);
					/* Convert to unsigned */
					datum ^= 128;
					putc(datum, ft->fp);
					done++;
				}
				return;
			case ULAW:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					/* round up to 12 bits of data */
					datum += 0x8;	/* + 0b1000 */
					datum = st_linear_to_ulaw(datum);
					putc(datum, ft->fp);
					done++;
				}
				return;
			case ALAW:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					/* round up to 12 bits of data */
					datum += 0x8;	/* + 0b1000 */
					datum = st_linear_to_Alaw(datum);
					putc(datum, ft->fp);
					done++;
				}
				return;
			}
		case WORD: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					wshort(ft, datum);
					done++;
				}
				return;
			case UNSIGNED:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					/* Convert to unsigned */
					datum ^= 0x8000;
					wshort(ft, datum);
					done++;
				}
				return;
			case ULAW:
fail("No U-Law support for shorts (try -b option ?)");
				return;
			case ALAW:
fail("No A-Law support for shorts (try -b option ?)");
				return;
			}
		case FLOAT:
			while(done < nsamp) {
				/* scale signed up to long's range */
				datum = RIGHT(*buf++, 16);
			 	wfloat(ft, (double) datum);
				done++;
			}
			return;
		default: {
		        fail("Drop through in rawwrite!");
		        }

		}
	fail("Sorry, don't have code to write %s, %s",
		styles[ft->info.style], sizes[ft->info.size]);
}

/* special raw2 output routine to output just like raw, but
if the input file is one channel, duplicate the output
forming a 'stereo' output file.  This is handy for our
AIFF-> raw redbook stereo, since the rate and channel
convertions could not be done in only one pass.  This
accomplishes the channel replication at the tail end.
*/

/*
 * This abomination refers to 'informat' directly.
 * Remove when SOX does multiple effects.
 */
#ifdef UNNEEDED
void
raw2write(ft, buf, nsamp) 
ft_t ft;
long *buf, nsamp;
{
	register int datum;
	int done = 0;

	switch(ft->info.size) {
		case BYTE: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 24);
					putc(datum, ft->fp);
					if( informat.info.channels == 1 )
						putc(datum, ft->fp);
					done++;
				}
				return;
			case UNSIGNED:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 24);
					/* Convert to unsigned */
					datum ^= 128;
					putc(datum, ft->fp);
					if( informat.info.channels == 1 )
						putc(datum, ft->fp);
					done++;
				}
				return;
			case ULAW:
				/* grab table from Posk stuff */
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					/* round up to 12 bits of data */
					datum += 0x8;	/* + 0b1000 */
					datum = st_linear_to_ulaw(datum);
					putc(datum, ft->fp);
					if( informat.info.channels == 1 )
						putc(datum, ft->fp);
					done++;
				}
				return;
			case ALAW:
			       while(done < nsamp) {
                                        /* scale signed up to long's range */
                                        datum = RIGHT(*buf++, 16);
					/* round up to 12 bits of data */
					datum += 0x8;	/* + 0b1000 */
                                        datum = st_linear_to_Alaw(datum);
                                        putc(datum, ft->fp);
					if( informat.info.channels == 1 )
						putc(datum, ft->fp);
                                        done++;
                                }
				return;
			}
		case WORD: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					wshort(ft, datum);
					if( informat.info.channels == 1 )
						wshort(ft, datum);
					done++;
				}
				return;
			case UNSIGNED:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					/* Convert to unsigned */
					datum ^= 0x8000;
					wshort(ft, datum);
					if( informat.info.channels == 1 )
						wshort(ft, datum);
					done++;
				}
				return;
			case ULAW:
fail("No U-Law support for shorts (try -b option ?)");
				return;
			case ALAW:
fail("No A-Law support for shorts (try -b option ?)");
				return;
			}
		case FLOAT:
			while(done < nsamp) {
				/* scale signed up to long's range */
				datum = RIGHT(*buf++, 16);
			 	wfloat(ft, (double) datum);
				if( informat.info.channels == 1 )
					wfloat(ft, (double) datum);
				done++;
			}
			return;
		default: {
		        fail("Drop through in raw2write!");
		        }

		}
	fail("Sorry, don't have code to write %s, %s",
		styles[ft->info.style], sizes[ft->info.size]);
}
#endif


/*
* Set parameters to the fixed parameters known for this format,
* and change format to raw format.
*/

static  rawdefaults();

/* Signed byte */
sbstartread(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = SIGN2;
	rawdefaults(ft);
}

sbstartwrite(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = SIGN2;
	rawdefaults(ft);
}

ubstartread(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = UNSIGNED;
	rawdefaults(ft);
}

ubstartwrite(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = UNSIGNED;
	rawdefaults(ft);
}

uwstartread(ft) 
ft_t ft;
{
	ft->info.size = WORD;
	ft->info.style = UNSIGNED;
	rawdefaults(ft);
}

uwstartwrite(ft) 
ft_t ft;
{
	ft->info.size = WORD;
	ft->info.style = UNSIGNED;
	rawdefaults(ft);
}

swstartread(ft) 
ft_t ft;
{
	ft->info.size = WORD;
	ft->info.style = SIGN2;
	rawdefaults(ft);
}

swstartwrite(ft) 
ft_t ft;
{
	ft->info.size = WORD;
	ft->info.style = SIGN2;
	rawdefaults(ft);
}

ulstartread(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = ULAW;
	rawdefaults(ft);
}

ulstartwrite(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = ULAW;
	rawdefaults(ft);
}

alstartread(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = ALAW;
	rawdefaults(ft);
}

alstartwrite(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = ALAW;
	rawdefaults(ft);
}

static
rawdefaults(ft)
ft_t ft;
{
	if (ft->info.rate == 0)
		ft->info.rate = 8000;
	if (ft->info.channels == -1)
		ft->info.channels = 1;
}


