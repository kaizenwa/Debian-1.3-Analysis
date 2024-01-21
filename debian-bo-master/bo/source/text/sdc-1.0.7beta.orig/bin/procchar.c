/* Copyright (c) 1995 by jfw.  All Rights Reserved */

/***
   NAME
     procchar.c
   PURPOSE
     charater handling for typeset.
   NOTES
     get's imported from chrproc.scm
     There is some more less used handling defined. This is only for speed.
   HISTORY
	jw - 31. Mar 1995 Created.
	jw - 29. Okt 1995 Changed to return buffers instead of writing ports
***/

extern char * GC_malloc(int);
extern char * GC_realloc(char*, int);

#define malloc(a)    GC_malloc(a)
#define realloc(a,b) GC_realloc((a),(b))

#define INT(a) (((int)(a))&0xff)

/* The first version happend to import write_out_str and write the
 * result of translations instead of giving the translated string back
 *
extern int write_out_str (char *);
 *
 */

static int syschar = 0;

/* collect escaped characters */

int proc_esc_char ( unsigned char c)
{
  static int escape = 0;
  static int ccode  = 0;
  if( escape ) {
    switch(c) {
    case 'n':  escape = 0; return '\n';
    case '\\': escape = 0; return '\\';
    case '|': escape = 0; syschar = !syschar; return - (syschar + 2);
    default:
      if( (c >= '0') && (c <= '9')) {
	ccode = (ccode << 3) | (c - '0');
      }
      if( escape < 3 ) {
	++escape;
	return -1;
      } else {
	escape=0;
	return ccode;
      }
    }
  } else {
    if( c == '\\' ) {
      escape=1;
      ccode=0;
      return -1;
    } else {
      return (int) c;
    }
  }
}

/* use c_buf to return characters to be written out */

static unsigned char c_buf [2] = "\0";

static unsigned char * plain_proc_char ( unsigned char c)
{
  int ci = proc_esc_char(c);
  if( ci < 0 ) return 0;
  *c_buf=(unsigned char)ci;
  return c_buf;
}

static unsigned char * info_proc_char ( unsigned char c)
{
  int ci = proc_esc_char(c);
  if( ci < 0 ) return 0;
  switch (ci) {
  case '\n': case '\t':
    *c_buf = (unsigned char) ' ';
    return c_buf;
  default:
    *c_buf=(unsigned char)ci;
    return c_buf;
  }
}

static unsigned char * html_proc_char ( unsigned char c)
{
 int ci = proc_esc_char(c);
 if( ci < 0 ) return 0;
 switch( ci ) {
 case '>': return "&gt;";
 case '&': return "&amp;";
 case '<': return "&lt;";
 case INT('ä'): return "&auml;";
 case INT('ö'): return "&ouml;";
 case INT('ü'): return "&uuml;";
 case INT('Ä'): return "&Auml;";
 case INT('Ö'): return "&Ouml;";
 case INT('Ü'): return "&Uuml;";
 case INT('ß'): return "&szlig;";
 case '"': return "&quot;";
 default: 
   *c_buf = (unsigned char) ci;
   return c_buf;
 }
}

static unsigned char * lout_proc_char ( unsigned char c)
{
 int ci = proc_esc_char(c);
 if( ci < 0 ) return 0;
 switch(ci) {
 case '&': return "\"&\"";
 case '"': return "\"\\\"\"";
 case '{': return "\"{\"";
 case '}': return "\"}\"";
 case '|': return "\"|\"";
 case '\\': return "\"\\\\\"";
 case '/': return "\"/\"";
 case '#': return "\"#\"";
 case '@': return "\"@\"";
 default: *c_buf=(unsigned char)ci; return c_buf;
 }
}

static unsigned char * latex_proc_char( unsigned char c)
{
  int ci = proc_esc_char(c);
  if( ci < 0 ) return 0;
  switch( ci ) {
  case '\\': return "$\\backslash$";
  case '%': return "\\%";
  case '&': return "\\&";
  case '$': return "\\$";
  case '#': return "\\#";
  case '_': return "\\_";
  case '{': return "\\{";
  case '}': return "\\}";
  case '<': return "{\\tt<}";
  case '>': return "{\\tt>}";
  case '~': return "\\~{ }";
  case '^': return "\\^{ }";
  case INT('Ä'): return "\\\"A";
  case INT('Ö'): return "\\\"O";
  case INT('Ü'): return "\\\"U";
  case INT('ä'): return "\\\"a";
  case INT('ö'): return "\\\"o";
  case INT('ü'): return "\\\"u";
  case INT('ß'): return "\\ss{}";
  default: *c_buf=(unsigned char)ci; return c_buf;
  }
}

static unsigned char * latex_math_proc_char( unsigned char c)
{
  int ci = proc_esc_char(c);
  if( ci < 0 ) return 0;
  switch( ci ) {
  case '\\': return "\\backslash ";
  case '%': return "\\%";
  case '&': return "\\&";
  case '$': return "\\$";
  case '#': return "\\#";
  case '_': return "\\_";
  case '{': return "\\{";
  case '}': return "\\}";
  case '~': return "\\~{ }";
  case '^': return "\\^{ }";
  case INT('Ä'): return "\\\"A";
  case INT('Ö'): return "\\\"O";
  case INT('Ü'): return "\\\"U";
  case INT('ä'): return "\\\"a";
  case INT('ö'): return "\\\"o";
  case INT('ü'): return "\\\"u";
  case INT('ß'): return "\\ss{}";
  default: *c_buf=(unsigned char)ci; return c_buf;
  }
}

static unsigned char * man_proc_char( unsigned char c )
{
  static int cc = 0;
  int ci = proc_esc_char(c);
  if( ci < 0 ) return 0;
  switch (ci) {
  case '\n':
    cc=0; *c_buf='\n'; return c_buf;
  case '-':
    ++cc; return "\\-";
  case '\\': 
    ++cc; return "\\e";
  case '.': 
    ++cc; *c_buf=ci;
    if(cc==1) return "\\&."; else return c_buf;
  case '\'': 
    ++cc; *c_buf=ci;
    if(cc==1) return "\\\'"; else return c_buf;
  default:
    ++cc;
    *c_buf = ci;
    return c_buf;
  }
}

/* ---------------------------------------------------------------------- */

typedef unsigned char* chrp;
typedef unsigned char* (*chrprocp) (unsigned char);

static unsigned char * tr_string( chrprocp f, unsigned char * line)
{
  int ii=0, bi=0, ll, sl, bs, ci;
  unsigned char *buf;
  unsigned char * r,c;

  ll  = strlen(line);
  sl  = ll > 16 ? ll/8 : 2;	/* 10% grow expected, at least 2 chars    */
  bs  = ll + sl;		/* buffer size */
  buf = (chrp) malloc(bs*sizeof(unsigned char));

  while((c=line[ii++])) {
    if( syschar ) {
      ci = proc_esc_char(c);
      if( ci >= 0 ) buf[bi++]=(unsigned char)ci;
    } else {
      r=(*f)(c);
      if( r == c_buf ) {
	buf[bi++]=*r;
      } else if (r) {
	while(*r) {
	  buf[bi++] = *r++;
	  if(bi==bs) {
	    bs += sl;
	    buf = (chrp) realloc(buf, bs);
	  }
	}
      }
    }
    if( bi == bs) {		/* No space left */
      bs += sl;
      buf = (chrp) realloc(buf, bs);
    }
  }
  buf[bi]=0;
  return buf;
}

unsigned char * plain_tr_string( unsigned char * line )
{
  return tr_string( plain_proc_char, line);
}

unsigned char * html_tr_string( unsigned char * line)
{
  return tr_string( html_proc_char, line);
}

unsigned char * man_tr_string( unsigned char * line)
{
  return tr_string( man_proc_char, line);
}

unsigned char * lout_tr_string( unsigned char * line)
{
  return tr_string( lout_proc_char, line);
}

unsigned char * latex_tr_string( unsigned char * line)
{
  return tr_string( latex_proc_char, line);
}

static unsigned char * latex_condens_math( unsigned char* line )
{
  int i, j;
  for( i=0, j=0; line[i];) {
    if(line[i] == '\\') {	/* buggy if the last char was a \ */
				/* but this *should* be impossible */
      line[j++]=line[i++];
    } else {
      if( line[i] == '$') {
	++i;
	continue;
      }
    }
    line[j++]=line[i++];
  }
  line[j]='\0';
  return line;
}

unsigned char * latex_math_tr_string( unsigned char * line)
{
  return latex_condens_math( tr_string( latex_math_proc_char, line));
}

unsigned char * info_tr_string( unsigned char * line)
{
  return tr_string( info_proc_char, line);
}

