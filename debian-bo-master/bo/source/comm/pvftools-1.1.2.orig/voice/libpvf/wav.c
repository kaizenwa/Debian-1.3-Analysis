/*
 * wav.c
 *
 * Converts pvf <--> wav.
 * it was written by Karlo Gross kg@orion.ddorf.rhein-ruhr.de
 * by using parts from Rick Richardson and Lance Norskog's
 * wav.c, found in sox-11-gamma. Thank you for some funtions.
 * This is the 1. alpha release from 1997/2/14
 *
 */

char *libpvf_wav_c = "$Id: wav.c,v 1.5 1997/01/22 07:35:32 marc Exp $";

#include "../include/voice.h"

extern char error_text[80];

char *sizes[] = {
     "NONSENSE!",
     "bytes",
     "shorts",
     "NONSENSE",
     "longs",
     "32-bit floats",
     "64-bit floats",
     "IEEE floats"
};


/* Private data for .wav file */

typedef struct wavstuff {
     long samples;
     int  second_header; /* non-zero on second header write */
} *wav_t;

/* wave file characteristics */
unsigned short wFormatTag;              /* data format */
unsigned short wChannels;               /* number of channels */
unsigned long  wSamplesPerSecond;       /* samples per second per channel */
unsigned long  wAvgBytesPerSec;    /* estimate of bytes per second needed */
unsigned short wBlockAlign;     /* byte alignment of a basic sample block */
unsigned short wBitsPerSample;          /* bits per sample */
unsigned long  data_length;             /* length of sound data in bytes */
unsigned long  bytespersample;          /* bytes per sample (per channel) */

static char *wav_format_str();


int pvftowav (FILE *fd_in, FILE *fd_out, pvf_header *header_in)
/*------------------------------------------------------------*/
     {
     int bytespersample;
     long data_size = 0;
     int buffer_size = 0;
     int *buffer = NULL;
     int data,*ptr;
     int voice_samples = 0;
     struct soundstream s;

     bytespersample = (header_in->nbits + 7)/8;
     switch (bytespersample)
          {
          case 1:
                    s.info.size = BYTE;
               break;
          case 2:
                    s.info.size = WORD;
               break;
          case 4:
                    s.info.size = LONG;
               break;
          default:
               printf("sorry, don't understand .wav size");
               return(ERROR);
          }

     s.info.rate     = header_in->speed;
     s.info.style    = -1; /*ULAW; oder ALAW aber eher UNSIGNED */
     s.info.channels = header_in->channels;
     s.comment      =  NULL;
     s.swap         = 0;
     s.filetype     = (char *) 0;
     s.fp                = fd_out;
     s.seekable          = 0;



     while((data = header_in->read_pvf_data(fd_in)) != EOF)
          {
          if (voice_samples >= buffer_size)
               {
               buffer_size += BLOCK_SIZE;
               buffer = (int *) realloc(buffer, buffer_size * sizeof(int));

               if (buffer == NULL)
                    {
                    fprintf(stderr, "%s: out of memory in pvfcut", program_name);
                    free(buffer);
                    exit(99);
                    };

               }

          buffer[voice_samples++] = data;
          data_size++;
          }

          if   (wavstartwrite(&s,data_size) != OK)
               {
               free(buffer);
               return ERROR;
               }

          ptr = buffer;
          while (data_size--)
               {
               *ptr >>=16;
               if   (*ptr > 0x7f)
                    *ptr = 0x7f;
               if   (*ptr < -0x80)
                    *ptr = -0x80;

               putc(*ptr+0x80,fd_out);
               ptr++;
          };
          free(buffer);
          return(OK);
     }

int wavtopvf (FILE *fd_in, FILE *fd_out, pvf_header *header_out)
/*------------------------------------------------------------*/
    {
     struct soundstream s;

     s.fp = fd_in;

     if   (wavstartread(&s) != OK)
          return ERROR;

     header_out->channels = (int)wChannels;
     header_out->speed = (int)wSamplesPerSecond;
     header_out->nbits = (int)wBitsPerSample;

     /* nbits evtl. rauslassen, weil default angeblich auf 32 */

     write_pvf_header(fd_out, header_out);

          /*printf("data_len = %d\n",data_length);*/

     while (data_length--)
          {
     int d = getc(fd_in);

     if (feof(fd_in))
     return(OK);

     /* if (type <= 2) ist das fuer mich interessant ???  */
          /* was ist type ?? */
     header_out->write_pvf_data(fd_out, (d - 0x80) << 16);
     };


     return(OK);

     }



int wavstartread(ft_t ft)
/*------------------------------------------------------------*/
     {
     wav_t     wav = (wav_t) ft->priv;
     char magic[4];
    unsigned int len;
     int  littlendian = 1;
     char *endptr;


     ft->info.rate      = 0;
     ft->info.size      = -1;
     ft->info.style     = -1;
     ft->info.channels  = -1;
     ft->comment   =  NULL;
     ft->swap      = 0;
     /*ft->filetype  = outformat.filetype  = (char *) 0; */
     /*ft->fp        = stdin; */
     /*ft->filename  = "input"; */


     endptr = (char *) &littlendian;
     if   (!*endptr) ft->swap = 1;

     /* If you need to seek around the input file. */
     if   (0 && ! ft->seekable)
          printf("Sorry, .wav input file must be a file, not a pipe");

     if   (   fread(magic, 1, 4, ft->fp) != 4
         || strncmp("RIFF", magic, 4))
          {
          printf("Sorry, not a RIFF file");
          return ERROR;
          }

     len = rllong(ft);

     if   (   fread(magic, 1, 4, ft->fp) != 4
         || strncmp("WAVE", magic, 4))
          {
          printf("Sorry, not a WAVE file");
          return ERROR;
          }

     /* Now look for the format chunk */
     for (;;)
          {
          if   ( fread(magic, 1, 4, ft->fp) != 4 )
               {
               printf("Sorry, missing fmt spec");
               return ERROR;
               }
          len = rllong(ft);
          if   (strncmp("fmt ", magic, 4) == 0)
               break;    /* Found the format chunk */
          while (len > 0 && !feof(ft->fp))   /* skip to next chunk */
               {
               getc(ft->fp);
               len--;
               }
          }

     if   ( len < 16 )
          printf("Sorry, fmt chunk is too short");

     wFormatTag = rlshort(ft);
     switch (wFormatTag)
          {
          case WAVE_FORMAT_UNKNOWN:
          printf("Sorry, this WAV file is in Microsoft Official Unknown format.");
          return ERROR;

          case WAVE_FORMAT_PCM:    /* this one, at least, I can handle */
               break;

          case WAVE_FORMAT_ADPCM:
          printf("Sorry, this WAV file is in Microsoft ADPCM format.");
          return ERROR;

          case WAVE_FORMAT_ALAW:   /* Think I can handle this */
                    ft->info.style = ALAW;
               break;

          case WAVE_FORMAT_MULAW:  /* Think I can handle this */
                    ft->info.style = ULAW;
               break;

          case WAVE_FORMAT_OKI_ADPCM:
          printf("Sorry, this WAV file is in OKI ADPCM format.");
          return ERROR;

          case WAVE_FORMAT_DIGISTD:
          printf("Sorry, this WAV file is in Digistd format.");
          return ERROR;

          case WAVE_FORMAT_DIGIFIX:
          printf("Sorry, this WAV file is in Digifix format.");
          return ERROR;

          case IBM_FORMAT_MULAW:
          printf("Sorry, this WAV file is in IBM U-law format.");
          return ERROR;

          case IBM_FORMAT_ALAW:
          printf("Sorry, this WAV file is in IBM A-law format.");
          return ERROR;

          case IBM_FORMAT_ADPCM:
          printf("Sorry, this WAV file is in IBM ADPCM format.");
          return ERROR;

          default:  printf("Sorry, don't understand format");
          return ERROR;

          }
     wChannels = rlshort(ft);
     ft->info.channels = wChannels;
     wSamplesPerSecond = rllong(ft);
     ft->info.rate = wSamplesPerSecond;
     wAvgBytesPerSec = rllong(ft); /* Average bytes/second */
     wBlockAlign = rlshort(ft);    /* Block align */
     wBitsPerSample =  rlshort(ft);     /* bits per sample per channel */
     bytespersample = (wBitsPerSample + 7)/8;
     switch (bytespersample)
     {
          case 1:
                    ft->info.size = BYTE;
               break;
          case 2:
                    ft->info.size = WORD;
               break;
          case 4:
                    ft->info.size = LONG;
               break;
          default:
               printf("Sorry, don't understand .wav size");
               return ERROR;
     }
     len -= 16;
     while (len > 0 && !feof(ft->fp))
     {
          getc(ft->fp);
          len--;
     }

     /* Now look for the wave data chunk */
     for (;;)
     {
          if ( fread(magic, 1, 4, ft->fp) != 4 )
               {
               printf("Sorry, missing data chunk");
               return ERROR;
               }
          len = rllong(ft);
          if (strncmp("data", magic, 4) == 0)
               break;    /* Found the data chunk */
          while (len > 0 && !feof(ft->fp)) /* skip to next chunk */
          {
               getc(ft->fp);
               len--;
          }
     }
     data_length = len;
     wav->samples = data_length/ft->info.size;    /* total samples */

     printf("Reading Wave file: %s format, %d channel%s, %ld samp/sec\n",
             wav_format_str(wFormatTag), wChannels,
             wChannels == 1 ? "" : "s", wSamplesPerSecond);

     printf("%ld byte/sec, %d block align, %d bits/samp, %lu data bytes\n",
                wAvgBytesPerSec, wBlockAlign, wBitsPerSample, data_length);
return OK;
}





int wavstartwrite(ft_t ft,long data_size)
/*------------------------------------------------------------*/
{
     wav_t     wav = (wav_t) ft->priv;
     int  littlendian = 1;
     char *endptr;

     endptr = (char *) &littlendian;
     if (!*endptr) ft->swap = 1;

     wav->samples = 0;
     wav->second_header = 0;

     if   (wavwritehdr(ft,data_size) != OK)
          return ERROR;
return OK;
}

int wavwritehdr(ft_t ft,long data_size)
/*------------------------------------------------------------*/
{
     wav_t     wav = (wav_t) ft->priv;


     switch (ft->info.size)
     {
          case BYTE:
               wBitsPerSample = 8;
               if (ft->info.style == -1 || ft->info.style == UNSIGNED)
                    ft->info.style = UNSIGNED;
               else if (!wav->second_header && ft->info.style != ALAW && ft->info.style != ULAW)
                    printf("User options overiding style written to .wav header");
               break;
          case WORD:
               wBitsPerSample = 16;
               if (ft->info.style == -1 || ft->info.style == SIGN2)
                    ft->info.style = SIGN2;
               else if (!wav->second_header)
                    printf("User options overiding style written to .wav header");
               break;
          case LONG:
               wBitsPerSample = 32;
               if (ft->info.style == -1 || ft->info.style == SIGN2)
                    ft->info.style = SIGN2;
               else if (!wav->second_header)
                    printf("User options overiding style written to .wav header");
               break;
          default:
               wBitsPerSample = 32;
               if (ft->info.style == -1)
                    ft->info.style = SIGN2;
               if (!wav->second_header)
     printf("Warning - writing bad .wav file using %s",sizes[ft->info.size]);
               break;
     }

     switch (ft->info.style)
     {
          case UNSIGNED:
               wFormatTag = WAVE_FORMAT_PCM;
               if (wBitsPerSample != 8 && !wav->second_header)
                    printf("Warning - writing bad .wav file using unsigned data and %d bits/sample",wBitsPerSample);
               break;
          case SIGN2:
               wFormatTag = WAVE_FORMAT_PCM;
               if (wBitsPerSample == 8 && !wav->second_header)
                    printf("Warning - writing bad .wav file using signed data and %d bits/sample",wBitsPerSample);
               break;
          case ALAW:
               wFormatTag = WAVE_FORMAT_ALAW;
               if (wBitsPerSample != 8 && !wav->second_header)
                    printf("Warning - writing bad .wav file using A-law data and %d bits/sample",wBitsPerSample);
               break;
          case ULAW:
               wFormatTag = WAVE_FORMAT_MULAW;
               if (wBitsPerSample != 8 && !wav->second_header)
                    printf("Warning - writing bad .wav file using U-law data and %d bits/sample",wBitsPerSample);
               break;
     }


     wSamplesPerSecond = ft->info.rate;
     bytespersample = (wBitsPerSample + 7)/8;
     wAvgBytesPerSec = ft->info.rate * ft->info.channels * bytespersample;
     wChannels = ft->info.channels;
     wBlockAlign = ft->info.channels * bytespersample;
/*   if (!wav->second_header)*/    /* use max length value first time */
/*        data_length = 0x7fffffff - (8+16+12);  */
/*   else*/    /* fixup with real length */
/*        data_length = bytespersample * wav->samples; */
          data_length = data_size;

     /* figured out header info, so write it */
     fputs("RIFF", ft->fp);
     wllong(ft, data_length + 8+16+12+1);    /* Waveform chunk size: FIXUP(4) */
                                    /* die 1 ist von mir karlo */
     fputs("WAVE", ft->fp);
     fputs("fmt ", ft->fp);
     wllong(ft, (long)16);         /* fmt chunk size */
     wlshort(ft, wFormatTag);
     wlshort(ft, wChannels);
     wllong(ft, wSamplesPerSecond);
     wllong(ft, wAvgBytesPerSec);
     wlshort(ft, wBlockAlign);
     wlshort(ft, wBitsPerSample);

     fputs("data", ft->fp);
     wllong(ft, data_length);  /* data chunk size: FIXUP(40) */

     if (!wav->second_header) {
          printf("Writing Wave file: %s format, %d channel%s, %ld samp/sec",
               wav_format_str(wFormatTag), wChannels,
               wChannels == 1 ? "" : "s", wSamplesPerSecond);
          printf(" %ld byte/sec, %d block align, %d bits/samp\n",
                     wAvgBytesPerSec, wBlockAlign, wBitsPerSample);
     } else
          printf("Finished writing Wave file, %lu data bytes\n",data_length);
return OK;
}



/*
 * Return a string corresponding to the wave format type.
 */
static char * wav_format_str(unsigned wFormatTag)
/*------------------------------------------------------------*/
{
     switch (wFormatTag)
     {
          case WAVE_FORMAT_UNKNOWN:
               return "Microsoft Official Unknown";
          case WAVE_FORMAT_PCM:
               return "Microsoft PCM";
          case WAVE_FORMAT_ADPCM:
               return "Microsoft ADPCM";
          case WAVE_FORMAT_ALAW:
               return "Microsoft A-law";
          case WAVE_FORMAT_MULAW:
               return "Microsoft U-law";
          case WAVE_FORMAT_OKI_ADPCM:
               return "OKI ADPCM format.";
          case WAVE_FORMAT_DIGISTD:
               return "Digistd format.";
          case WAVE_FORMAT_DIGIFIX:
               return "Digifix format.";
          case IBM_FORMAT_MULAW:
               return "IBM U-law format.";
          case IBM_FORMAT_ALAW:
               return "IBM A-law";
                case IBM_FORMAT_ADPCM:
                    return "IBM ADPCM";
          default:
               return "Unknown";
     }
}
/* Read short, little-endian: little end first. VAX/386 style. */
unsigned short rlshort(ft_t ft)
/*------------------------------------------------------------*/
{
     unsigned char uc, uc2;
     uc  = getc(ft->fp);
     uc2 = getc(ft->fp);
     return (uc2 << 8) | uc;
}


/* Read long, little-endian: little end first. VAX/386 style. */
unsigned long rllong(ft_t ft)
/*------------------------------------------------------------*/
{
     unsigned char uc, uc2, uc3, uc4;
/*   if (feof(ft->fp))
          printf(readerr);          No worky! */
     uc  = getc(ft->fp);
     uc2 = getc(ft->fp);
     uc3 = getc(ft->fp);
     uc4 = getc(ft->fp);
     return ((long)uc4 << 24) | ((long)uc3 << 16) | ((long)uc2 << 8) | (long)uc;
}





/* Write long, little-endian: little end first. VAX/386 style. */

int wllong(ft_t ft, unsigned long ul)
/*------------------------------------------------------------*/
{
int datum;

     datum = (ul) & 0xff;
     putc(datum, ft->fp);
     datum = (ul >> 8) & 0xff;
     putc(datum, ft->fp);
     datum = (ul >> 16) & 0xff;
     putc(datum, ft->fp);
     datum = (ul >> 24) & 0xff;
     putc(datum, ft->fp);
     if (ferror(ft->fp))
          return 1;
     return 0;
}


/* Write short, little-endian: little end first. VAX/386 style. */
int wlshort(ft_t ft, unsigned short us)
/*------------------------------------------------------------*/
{
     putc(us, ft->fp);
     putc(us >> 8, ft->fp);
     if (ferror(ft->fp))
          return 1;
     return 0;
}

