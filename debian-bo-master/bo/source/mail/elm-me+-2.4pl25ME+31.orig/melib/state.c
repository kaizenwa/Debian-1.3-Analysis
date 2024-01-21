#include "headers.h"
#include "melib.h"

int NULL_filter(c,st)
     int c;          /* unsigned char assumed */
     struct out_state * st;
{
  st++;
  return c;
}

static void state_panic P_((char *,int,char *, char *)); /* Prototype */
static void state_panic(f,ln,pr,ms) 
     char * f;
     int ln;
     char *pr;
     char *ms;
{
  int do_cursor = RawState();

  dprint(1,(debugfile,"\nSTATE PANIC in %s:%d:%s\n",f,ln,pr));
  dprint(1,(debugfile,">>%s\n",ms));

  /* softkeys_off(); */

  if (do_cursor) {
    error1("STATE PANIC: %s",ms);
    sleep(1);
  }

  if (do_cursor) {
    MoveCursor(elm_LINES, 0);
    Raw(OFF);
  }

  fprintf(stderr,"\nSTATE PANIC in %s:%d:%s\n",f,ln,pr);
  fprintf(stderr,">>%s\n",ms);

#if DEBUG
  if (debug > 10) {
    fprintf(stderr,"ABORTING...\n");
    fflush(stderr);
    abort();
  }
#endif
  emergency_exit();
}

static void init_si_file P_((in_state_t *)); /* Prototype */
static void init_si_file(s)
     in_state_t *s;
{
  s->magic  = STATE_in_file;
  s->u.file.fpin = NULL;
}

static void init_si_string P_((in_state_t *)); /* Prototype */
static void init_si_string(s)
     in_state_t *s;
{
  s->magic  = STATE_in_string;
  s->u.string.inbuf = NULL;
  s->u.string.inreadp = NULL;
}

void in_state_clear (s,m)
     in_state_t *s;
     int m;
{
  s->magic = 0;  /* Not initilized yet */

  switch(m) {
  case STATE_in_file:   init_si_file(s);   break;
  case STATE_in_string: init_si_string(s); break;
  default:
    state_panic(__FILE__,__LINE__,"in_state_clear","Bad magic number");
  }
}

static void dest_si_file P_((in_state_t *)); /* Prototype */
static void dest_si_file(s)
     in_state_t *s;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"dest_si_file","Bad magic number");
  s->u.file.fpin = NULL;
}

static void dest_si_string P_((in_state_t *)); /* Prototype */
static void dest_si_string(s)
     in_state_t *s;
{
  if (s->magic  != STATE_in_string)
    state_panic(__FILE__,__LINE__,"dest_si_string","Bad magic number");

  if (s->u.string.inbuf)
    free(s->u.string.inbuf);
  
  s->u.string.inbuf = NULL;
  s->u.string.inreadp = NULL;
}

void in_state_destroy (s)
     in_state_t *s;
{
  switch(s->magic) {
  case STATE_in_file:   dest_si_file(s);   break;
  case STATE_in_string: dest_si_string(s); break;
  default:
    state_panic(__FILE__,__LINE__,"in_state_destroy","Bad magic number");
  }
  s->magic = 0; /* State destroyed */
}

void set_in_state_buffer (c, s)
     char *c;
     in_state_t *s;
{
  if (s->magic  != STATE_in_string)
    state_panic(__FILE__,__LINE__,"set_in_state_buffer","Bad magic number");
  
  if (s->u.string.inbuf)
    state_panic(__FILE__,__LINE__,"set_in_state_buffer","Already called");
  
  s->u.string.inreadp = s->u.string.inbuf = strmcpy (s->u.string.inbuf, c);
}

void set_in_state_file (F, s)
     FILE *F;
     in_state_t *s;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"set_in_state_file","Bad magic number");

  if (s->u.file.fpin)
    state_panic(__FILE__,__LINE__,"set_in_state_file","Already called");

  s->u.file.fpin = F;
}

int in_state_seekable (s)
     in_state_t *s;
{
  switch(s->magic) {
  case STATE_in_file:   return 1;   
  case STATE_in_string: return 0; 
  default:
    state_panic(__FILE__,__LINE__,"in_state_seekable","Bad magic number");
  }

  return 0;
}

int in_state_fseek (s,pos)
     in_state_t *s;
     long pos;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"in_state_fseek","Bad magic number");

  if (s->u.file.fpin == NULL)
    state_panic(__FILE__,__LINE__,"in_state_fseek","NULL file pointer");

  return fseek(s->u.file.fpin,pos,SEEK_SET);
}

long in_state_ftell (s)
     in_state_t *s;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"in_state_ftell","Bad magic number");
  
  if (s->u.file.fpin == NULL)
    state_panic(__FILE__,__LINE__,"in_state_ftell","NULL file pointer");

  return ftell(s->u.file.fpin);
}

FILE * in_state_FILE (s) 
     in_state_t *s;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"in_state_FILE","Bad magic number");

  if (s->u.file.fpin == NULL)
    state_panic(__FILE__,__LINE__,"in_state_FILE","NULL file pointer");
  
  return s->u.file.fpin;
}

static int getc_si_file P_((in_state_t *)); /* Prototype */
static int getc_si_file(s)
     in_state_t *s;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"getc_si_file","Bad magic number");
  if (s->u.file.fpin == NULL)
    state_panic(__FILE__,__LINE__,"getc_si_file","NULL filepointer");

  return fgetc (s->u.file.fpin);
}

static int getc_si_string P_((in_state_t *)); /* Prototype */
static int getc_si_string(s)
     in_state_t *s;
{
  if (s->magic  != STATE_in_string)
    state_panic(__FILE__,__LINE__,"getc_si_string","Bad magic number");

  if (s->u.string.inbuf) {
    unsigned char ret;

    if (! *s->u.string.inreadp) {
      /* No more data left in the buffer, so clean up... */
      s->u.string.inreadp = NULL;
      free (s->u.string.inbuf);
      s->u.string.inbuf = NULL;
      return EOF;
    }
    
    if (*s->u.string.inreadp)
      ret = (unsigned char) *s->u.string.inreadp++;
    return ret;
  }
  
  dprint(5, (debugfile, "getc_si_string: inbuf==NULL\n"));
  return EOF;
}

int state_getc (s)
     in_state_t *s;
{
  switch(s->magic) {
  case STATE_in_file:   return getc_si_file(s);   
  case STATE_in_string: return getc_si_string(s); 
  default:
    state_panic(__FILE__,__LINE__,"state_getc","Bad magic number");
  }

  return EOF;
}

static int ungetc_si_file P_((int,in_state_t *)); /* Prototype */
static int ungetc_si_file (c,s)
     int c;
     in_state_t *s;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"ungetc_si_file","Bad magic number");
  if (s->u.file.fpin == NULL)
    state_panic(__FILE__,__LINE__,"umgetc_si_file","NULL filepointer");

  return ungetc(c,s->u.file.fpin);
}

static int ungetc_si_string P_((int,in_state_t *)); /* Prototype */
static int ungetc_si_string (c,s)
     int c;
     in_state_t *s;
{
  if (s->magic  != STATE_in_string)
    state_panic(__FILE__,__LINE__,"ungetc_si_string","Bad magic number");

  if (s->u.string.inbuf) {
    if (s->u.string.inreadp > s->u.string.inbuf) {
      return *(--(s->u.string.inreadp));
    } else {
      dprint(5, (debugfile, "ungetc_si_string: In beginning of buffer.\n"));
      return EOF;
    }
  } else {
    dprint(5, (debugfile, "ungetc_si_string: inbuf==NULL\n"));
    return EOF;
  }
}

int state_ungetc (c,s)
     int c;
     in_state_t *s;
{
  switch(s->magic) {
  case STATE_in_file:   return ungetc_si_file(c,s);   
  case STATE_in_string: return ungetc_si_string(c,s); 
  default:
    state_panic(__FILE__,__LINE__,"state_ungetc","Bad magic number");
  }
  return EOF;
}


static char *gets_si_file P_((char *, int, in_state_t *));
static char *gets_si_file (dest, length, s)
     char *dest;
     int length;
     in_state_t *s;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"gets_si_file","Bad magic number");
  if (s->u.file.fpin == NULL)
    state_panic(__FILE__,__LINE__,"gets_si_file","NULL filepointer");
  
  return (fgets (dest, length, s->u.file.fpin));
}

static int getl_si_file P_((char *, int, in_state_t *));
static int getl_si_file (dest, length, s)
     char *dest;
     int length;
     in_state_t *s;
{
  if (s->magic  != STATE_in_file)
    state_panic(__FILE__,__LINE__,"getl_si_file","Bad magic number");
  if (s->u.file.fpin == NULL)
    state_panic(__FILE__,__LINE__,"getl_si_file","NULL filepointer");
  
  return (mail_gets (dest, length, s->u.file.fpin));
}


static int getl_si_string P_((char *, int, in_state_t *));
static int getl_si_string (dest, length, s)
     char *dest;
     int length;
     in_state_t *s;
{
  int i = 0, ch;
  
  if (s->magic  != STATE_in_string)
    state_panic(__FILE__,__LINE__,"getl_si_string","Bad magic number");

  while (length > 1) {
    ch = getc_si_string (s);
    if (ch == EOF)
      break;
    else if (ch == '\n') {
      dest[i++] = '\n';
      break;
    }
    else
      dest[i++] = ch;
    length--;
  }
  dest[i] = '\0';
  return i;
}
  
static char *gets_si_string P_((char *, int, in_state_t *));
static char *gets_si_string (dest, length, s)
     char *dest;
     int length;
     in_state_t *s;
{
  int len;

  if (s->magic  != STATE_in_string)
    state_panic(__FILE__,__LINE__,"getl_si_string","Bad magic number");

  len = getl_si_string (dest, length, s);
  if (len > 0)
    return dest;
  else
    return NULL;
}

char *state_gets (dest, length, s)
     char *dest;
     int length;
     in_state_t *s;
{
  switch(s->magic) {
  case STATE_in_file:   return gets_si_file(dest,length,s);   
  case STATE_in_string: return gets_si_string(dest,length,s); 
  default:
    state_panic(__FILE__,__LINE__,"state_gets","Bad magic number");
  }
  return NULL;
}

int state_getl (dest, length, s)
     char *dest;
     int length;
     in_state_t *s;
{
  switch(s->magic) {
  case STATE_in_file:   return getl_si_file(dest,length,s);   
  case STATE_in_string: return getl_si_string(dest,length,s); 
  default:
    state_panic(__FILE__,__LINE__,"state_getl","Bad magic number");
  }
  return 0;
}


static void init_so_file P_((out_state_t *)); /* Prototype */
static void init_so_file(s)
     out_state_t *s;
{
  s->magic   = STATE_out_file;
  s->u.file.fpout = NULL;
}

static void init_so_string P_((out_state_t *)); /* Prototype */
static void init_so_string(s)
     out_state_t *s;
{
  s->magic        = STATE_out_string;
  s->u.string.outbuf     = NULL;
  s->u.string.outwritep  = NULL;
  s->u.string.outbufsize = 0;
}

void out_state_clear (s,m)
     out_state_t *s;
     int m;
{
  s->magic = 0;  /* Not initilized yet */

  s->displaying = 0;
  s->prefix     = NULL;
  s->filter     = NULL_filter;

  switch(m) {
  case STATE_out_file:   init_so_file(s);   break;
  case STATE_out_string: init_so_string(s); break;
  default:
    state_panic(__FILE__,__LINE__,"out_state_clear","Bad magic number");
  }
}

static void dest_so_file P_((out_state_t *)); /* Prototype */
static void dest_so_file(s)
     out_state_t *s;
{
  if (s->magic   != STATE_out_file)
    state_panic(__FILE__,__LINE__,"dest_so_file","Bad magic number");
  s->u.file.fpout = NULL;
}

static void dest_so_string P_((out_state_t *)); /* Prototype */
static void dest_so_string(s)
     out_state_t *s;
{
  if (s->magic        != STATE_out_string)
    state_panic(__FILE__,__LINE__,"dest_so_string","Bad magic number");

  s->u.string.outbuf     = NULL;
  s->u.string.outwritep  = NULL;
  s->u.string.outbufsize = 0;
}

void out_state_destroy (s)
     out_state_t *s;
{

  switch(s->magic) {
  case STATE_out_file:   dest_so_file(s);   break;
  case STATE_out_string: dest_so_string(s); break;
  default:
    state_panic(__FILE__,__LINE__,"out_state_destroy","Bad magic number");
  }

  s->displaying = 0;
  s->prefix     = NULL;
  s->filter     = NULL_filter;
  s->magic      = 0; /* No longer initialized */
}

void set_out_state_buffer (buffer,size,s)
     char * buffer;
     int size;
     out_state_t *s;
{
  if (s->magic        != STATE_out_string)
    state_panic(__FILE__,__LINE__,"set_out_state_buffer","Bad magic number");

  if (s->u.string.outbuf     != NULL)
    state_panic(__FILE__,__LINE__,"set_out_state_buffer","Already called");

  s->u.string.outbuf     = buffer;
  s->u.string.outwritep  = buffer;
  s->u.string.outbufsize = size;
}

void out_state_ref_buffer (s,p,len)
     out_state_t *s;
     char **p;
     int *len;
{
  if (s->magic        != STATE_out_string)
    state_panic(__FILE__,__LINE__,"out_state_ref_buffer","Bad magic number");

  if (s->u.string.outbuf     == NULL)
    state_panic(__FILE__,__LINE__,
		"out_state_ref_buffer","NULL buffer pointer");

  *p   = s->u.string.outbuf;
  *len = s->u.string.outwritep - s->u.string.outbuf;
}

void set_out_state_file (file,s)
     FILE * file;
     out_state_t *s;
{
  if (s->magic   != STATE_out_file)
    state_panic(__FILE__,__LINE__,"set_out_state_file","Bad magic number");
  if (s->u.file.fpout != NULL)
    state_panic(__FILE__,__LINE__,"set_out_state_file","Already called");
  s->u.file.fpout = file;
}

static int putc_so_file P_((out_state_t *, int)); /* Prototype */
static int putc_so_file(s,ch)
     out_state_t *s;
     int ch;
{
  int res;
  if (s->magic   != STATE_out_file)
    state_panic(__FILE__,__LINE__,"putc_so_file","Bad magic number");
  if (s->u.file.fpout == NULL)
    state_panic(__FILE__,__LINE__,"putc_so_file","NULL file pointer");

  res = s-> filter(ch,s);

  if (res < 0) /* Destroy char */
    return EOF;

  return (fputc (res, s->u.file.fpout));
}

static int putc_so_string P_((out_state_t *, int)); /* Prototype */
static int putc_so_string(s,ch)
     out_state_t *s;
     int ch;
{
  int res;
  if (s->magic   != STATE_out_string)
    state_panic(__FILE__,__LINE__,"putc_so_string","Bad magic number");
  if (s->u.string.outbuf == NULL)
    state_panic(__FILE__,__LINE__,"putc_so_string","NULL buffer pointer");

  res = s-> filter(ch,s);

  if (res < 0) /* Destroy char */
    return EOF;

  if (s->u.string.outbuf - s->u.string.outbuf < s->u.string.outbufsize-1) {
    *s->u.string.outwritep++ = res;
    *s->u.string.outwritep = '\0';
    return res;
  }
  else
    return EOF;
}

int state_putc (ch, s)
     int ch;                        /* unsigned char assumed */
     out_state_t *s;
{
  switch(s->magic) {
  case STATE_out_file:   return putc_so_file(s,ch);   break;
  case STATE_out_string: return putc_so_string(s,ch); break;
  default: state_panic(__FILE__,__LINE__,"state_putc","Bad magic number");
  }
  return EOF;
}

static int put_so_file P_((out_state_t *, char *,int)); /* Prototype */
static int put_so_file(s,string,len)
     out_state_t *s;
     char *string;
     int len;
{
  if (s->magic   != STATE_out_file)
    state_panic(__FILE__,__LINE__,"put_so_file","Bad magic number");
  if (s->u.file.fpout == NULL)
    state_panic(__FILE__,__LINE__,"put_so_file","NULL file pointer");

  if (s->filter == NULL_filter)
    return fwrite (string, 1, len, s->u.file.fpout);
  else {
    int count;
    
    for (count = 0; count < len; count++) {
      if (putc_so_file (s,(unsigned char) string[count]) == EOF)
	return (count > 0 ? count : EOF);
    }
    return count;
  }
}

static int put_so_string P_((out_state_t *, char *,int)); /* Prototype */
static int put_so_string(s,string,len)
     out_state_t *s;
     char *string;
     int len;
{
  int count;

  if (s->magic   != STATE_out_string)
    state_panic(__FILE__,__LINE__,"put_so_string","Bad magic number");
  if (s->u.string.outbuf == NULL)
    state_panic(__FILE__,__LINE__,"put_so_string","NULL buffer pointer");
    
  for (count = 0; count < len; count++) {
    if (putc_so_string (s,(unsigned char) string[count]) == EOF)
      return (count > 0 ? count : EOF);
  }
  return count;
}

int state_put (string, len, s)
     char *string;
     int len;
     out_state_t *s;
{
  switch(s->magic) {
  case STATE_out_file:   return put_so_file(s,string,len);   break;
  case STATE_out_string: return put_so_string(s,string,len); break;
  default:  state_panic(__FILE__,__LINE__,"state_put","Bad magic number");
  }
  return EOF;
}

int state_puts (string, state)
     char *string;
     out_state_t *state;
{
  return state_put(string,strlen(string),state);
}


