/*
 * Protocol:
 *
 * new: PREFIX byte + old:
 * old:
 *
 * Host sends command length (1 byte) + command
 * A command consists of:
 *  bcpl filename + 0 byte + 
 *  command specific data (0-6 bytes) + command type (1 byte)
 * Command specific data:
 * - CREATE/SETATTR: attributes (NYI)
 * - RENAME: strlen of the second bcpl string
 * - READ/WRITE: 4 byte offset, 2 byte length
 * - GETVERS: filename[0] == 'C' and return > 1 then crc is used: 
 *     - READ/WRITE: after the data 2 byte crc follows.
 *
 * Answer:
 * new: PREFIX byte + old:
 * old:
 * - status of operation (0 for success)
 * - Thereafter, if return is 0 and op is
 *   - GETATTR, a 12 byte attribute structure is sent
 *   - READ,    the data is sent (+crc).
 *   - READDIR, a list of cstrings are sent, the last is of length 0
 *   - GETDEVS, a list of (cstring + 14 bytes), the last ...
 *   - WRITE, when crc is used, if crc is ok byte 0 is returned.
 *
 *
 * In short:
 *
 * Command:
 * - PREFIX 0x80, omitted if old protocol is used
 * - command length
 * - namelen
 * - name
 * - 0x00
 * - request data (0-6 bytes)
 * - request type
 * 
 *
 * Answer:
 * - PREFIX 0x80, omitted if old protocol is used
 * - status (0 == OK)
 * - data
 *
 */

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#ifdef _IBMR2
# include <sys/select.h>
#endif
#include <termios.h>	/* for tcdrain() */
#ifdef __SVR4
#include <poll.h>
#include <unistd.h>
#include <string.h>
#endif
#include "nfs_prot.h"
#include "mp.h"
#include "config.h"

/* Since the psion is little endian, we have to do some conversion */
/* Note: this should work for little endian hosts too */

#include <sys/errno.h>
extern int errno;
#ifndef       __NetBSD__
extern char *sys_errlist[];
#endif

void
short2pstr(l, s)
  unsigned int l;
  unsigned char *s;
{
  s[0] = l & 0xff;
  s[1] = (l & 0xff00) >> 8;
}

void
long2pstr(l, s)
  unsigned int l;
  unsigned char *s;
{
  s[0] = l & 0xff;
  s[1] = (l & 0xff00) >> 8;
  s[2] = (l & 0xff0000) >> 16;
  s[3] = (l & 0xff000000) >> 24;
}

unsigned int
pstr2long(s)
  unsigned char *s;
{
  int l = s[0];

  l |= s[1] <<  8; l |= s[2] << 16;
  return l | (s[3] << 24);
}


int
senddata(p, len)
  char *p;
  int len;
{
  int ret;
  int lerr;

  if(debug) printf("\t-> psion\n");
  psion_alive = fd_is_still_alive(psionfd,1);
  if(!psion_alive)
    {
      if(debug) printf("Senddata: Not alive...\n");
      return EOF;
    }

  lerr = 0;
  if(debug > 1) printf("\tSending: %#x, %d\n", (unsigned int)p, len);
  while((ret = write(psionfd, p, len)) < len)
    {
      if(debug > 1)
	fprintf(stderr, "\t\twrite(%d, %#x, %d) = %d\n",
	        psionfd, (unsigned int)p, len, ret);
      if(ret > 0)
	{
	  p += ret;
	  len -= ret;
	  lerr = 0;
	  continue;
	}
      if(ret < 0 && errno != EAGAIN)
        {
	  perror("p3nfsd: Write");
	  return 1;
	}

      lerr++;
      if(lerr > 1)
	{
	  if(debug)
	    fprintf(stderr, "\twrite=%d after flush/select\n", ret);
	  return 1;
	}

#ifdef NO_WRITE_SELECT

      if(debug > 1)
	fprintf(stderr, "\t\tdraining serial line\n");
      if(tcdrain(psionfd))
	perror("p3nfsd: tcdrain");

#else

      {
	fd_set writefd;
	struct timeval to;

	FD_ZERO(&writefd); FD_SET(psionfd, &writefd);
	to.tv_sec = 5; to.tv_usec = 0; 
	if((ret = select(psionfd+1, 0, &writefd, 0, &to)) <= 0)
	  {
	    if(debug)
	      fprintf(stderr, "p3nfsd: writeselect returned %d\n", ret);
	    return 1; 
	  }
      }

#endif
    }
  if(debug > 1)
    fprintf(stderr, "\t\twrite(%d, %#x, %d) = %d\n", psionfd, (unsigned int)p, len, ret);

#if !defined(linux)
  /*
   * tcdrain is broken on my linux version, strace reports 
   * ioctl(3, TCSBRK, 0x1) = 0
   */
#endif
  if(tcdrain(psionfd))
    perror("p3nfsd: tcdrain");
  return 0;
}

#define TOUT 5

static int
getbyte()
{
  static unsigned char readbuf[256];
  static int idx = 0, len = 0;
  int err;

  if(idx == len)
    {
      for(;;)
        {
#ifdef __SVR4
          struct pollfd readfd;

	  errno = 0;
	  readfd.fd = psionfd;
	  readfd.events = POLLIN;
	  err = poll(&readfd, 1, TOUT * 1000);
#else
          fd_set readfd;
          struct timeval to;

	  errno = 0;
	  to.tv_sec = TOUT; to.tv_usec = 0; 
	  FD_ZERO(&readfd); FD_SET(psionfd, &readfd);
	  err = select(psionfd+1, &readfd, 0, 0, &to);
#endif
	  if (err <= 0)
	    {
	      if(errno == EINTR)
	        continue;
	      fprintf(stderr, "p3nfsd: select: %s\n",
	              err < 0 ? sys_errlist[errno] : "timeout" );
	      return EOF;
	    }
	  break;
	}

      idx = 0;
      if(debug > 1) { printf("\t\tFilling read buffer..."); fflush(stdout); }
      len = read(psionfd, readbuf, sizeof(readbuf));
      if(debug > 1) printf("got %d bytes\n", len);
      if(debug > 2) 
        {
	  int i;

	  for(i = 0; i < len; i++)
	    {
	      printf("%02x ", readbuf[i]);
	      if((i & 15) == 15)
	        printf("\n");
	    }
	  printf("\n");
	}
      if(len <= 0)
        {
	  perror("p3nfsd: read"); len = 0;
	  return EOF;
	}
    }
  return readbuf[idx++];
}

int
sendcmd(cmd, fname, rest, restlen)
  char *fname, *rest;
  int cmd, restlen;
{
  int l;
  unsigned char mbuf[255], *str;

  if(old_nfsc)
    str = mbuf;
  else
    {
      str = mbuf+1;
      mbuf[0] = PREFIX;
    }

  str[1] = l = strlen(fname);
  if(l + restlen + 4 > 254)
    {
      printf("ERROR: filename too long (%d)\n", l);
      return 1;
    }

  bcopy(fname, str+2, l+1);	/* include trailing \0 */
  l += 3;
  if(restlen)
    {
      bcopy(rest, str+l, restlen);
      l += restlen;
    }
  str[l++] = cmd;
  *str = l-1;

  if(debug > 1)
    printf("\tSendcmd %d ('%s'+%d) %d bytes\n", cmd, fname, restlen, l);
  return senddata((char *)mbuf, l + (old_nfsc ? 0 : 1));
}

int
getcount(str, num)
  unsigned char *str; int num;
{
  int c;

  if(debug > 1) {printf("\tGoing to read %d bytes...",num);fflush(stdout);}
  while(num-- > 0)
    if((c = getbyte()) == EOF)
      {
	if(debug) printf("Bad connection to psion... (%d)\n", c);
	return -53;
      }
    else
      *str++ = c;
  if(debug > 1) printf("\tRead ok\n");
  return 0;
}

int
getstr(str)
  char *str;
{
  int c;
  unsigned char *s = (unsigned char *)str;

  if(debug > 1) {printf("\tGoing to read string...");fflush(stdout);}
  for(;;)
    {
      if((c = getbyte()) == EOF)
        {
	  if(debug) printf("Bad connection to psion... received EOF\n");
	  return 1;
	}
      *s++ = c;
      if(c == 0)
        {
	  if(debug > 1) printf(" got string (%s)\n", str);
	  return 0;
	}
    }
}

/* Old protocol: 1 byte, new Protocol 2 bytes */
int
getanswer()
{
  int i;

  i = getbyte();
  if(debug > 1) printf("Answer:(1) %#x\n", i);
  if(old_nfsc)
    return i;

  while(i != EOF)
    {
      if(i == PREFIX)
        {
	  i = getbyte();
	  if(debug > 1) printf("Answer:(1) %#x\n", i);
	  break;
	}
      shell_feed(i); /* this byte *has to be* input for our shell */
      i = getbyte();
      if(debug > 1) printf("Answer:(3) %#x\n", i);
    }
  return i;
}

int
sendop(cmd, fname, rest, restlen)
  char *fname, *rest;
  int cmd, restlen;
{
  int i = EOF;

  if((i = sendcmd(cmd, fname, rest, restlen)) == 0)
    i = getanswer();
  if(debug > 1) printf("\tReceived: (op %d) %d\n", cmd, i);
  return i;
}
