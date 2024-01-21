/*| -*- Mode: C -*-
/*|------------------------------------------------------------
/*|
/*| File: 	mactcp.c
/*| Created: 	Mon Aug 26 14:51:44 1991
/*| Author: 	Antony Courtney (courtney@nora)
/*| $Locker:  $
/*|
/*|------------------------------------------------------------
/*|
/*| Description: 
/*|
/*|   Code to handle MacTCP connection setup.
/*|
/*|------------------------------------------------------------
/*|
/*| Copyright 1991 Xerox Corporation
/*| All Rights Reserved Worldwide.
/*/
#ifndef lint
    static char xerox_copyright[] = "\
    \
    Copyright 1991 Xerox Corporation \
    All Rights Reserved";
#endif /* lint */
/*|------------------------------------------------------------
/*|
/*|  "$Header: /holmes/root/var/tmp/2.0alpha8/RCS/mactcp.c,v 1.7 1994/02/15 20:33:29 janssen Exp $"
/*/
#ifndef lint
      static char rcs_id[] = "$Header: /holmes/root/var/tmp/2.0alpha8/RCS/mactcp.c,v 1.7 1994/02/15 20:33:29 janssen Exp $";
#endif /* lint */
/*|
/*|------------------------------------------------------------
/*/

#pragma segment ilu
#pragma load "MacHeaders"

#include <string.h>
#include "ilu.h"
#include "iluntrnl.h"

#include "transprt.h"
#include "mooring.h"

#include "MacTCPCommonTypes.h"
#include "TCPPB.h"
 
#ifdef NOPE
#include "glob.h"
#include "tcp.h"
#include "tcplow.h"
#endif
#include "mactcpapi.h"
#include "macutil.h"

extern int errno;

#include "bffrlist.h"

/* Macintosh support routines */
void Init()
{
	EventRecord ev;
	
	InitGraf(&qd.thePort);
	InitWindows();
    InitCursor();
	InitCursorCtl(nil);
	
	FlushEvents(everyEvent,0);
    EventAvail(everyEvent,&ev);
}

void ilu_mac_init()
{
	OSErr err;
	
	Init();
	/* initialise network */
	if ((err=InitNetwork())!=noErr) {
		printf("error initiliazing network: %d\n",err);
		exit(1);
	}
	/* give ourselves a bogus server id for now */
	/* ilu_SetServerID("Fellini.fea7ff6.734a.deadbeef"); */
}

void ilu_mac_tcp_init()
{
	OSErr err;
	
	/* initialise network */
	if ((err=InitNetwork())!=noErr) {
		printf("error initiliazing network: %d\n",err);
		exit(1);
	}
	/* give ourselves a bogus server id for now */
	/* ilu_SetServerID("Fellini.fea7ff6.734a.deadbeef"); */
}

ilu_TransportClass _ilu_tcp_TransportClass(void);

typedef struct tcpparms {
  ilu_string hostname;
  ilu_cardinal port;
  bufferList buffer;
} *TCPParms;

#define TCP_HOSTNAME(a) (((TCPParms)(a))->hostname)
#define TCP_PORT(a) (((TCPParms)(a))->port)
#define TCP_BUFFER(a) (((TCPParms)(a))->buffer)

#define MAXDUMP		10000

char *_ilu_tcp_CurrentHostInetName(void);

/***********************************************************************
/***********************************************************************
/***********************************************************************
/**** First, the methods for the TCP Transport ************************
/***********************************************************************
/***********************************************************************
/**********************************************************************/

static ilu_boolean SigPIPEHandler = FALSE;

static void _tcp_HandleSigPIPE (void)
{
#ifdef NOPE
  /* Ignore SIGPIPEs, since we can occasionally get them.  There
     should be a way to do this that cooperates with other modules
     that want to handle signals.

     [Thanks to Dave Nichols <nichols@parc.xerox.com> for this code.]  */

  struct sigvec sv, osv;

  sv.sv_handler = SIG_IGN;
  sv.sv_mask = 0;
  sv.sv_flags = 0;

  if (OS_SIGVEC(SIGPIPE, &sv, &osv) == 0 && osv.sv_handler != SIG_DFL)
    /* Oops!  Set it back. */
    OS_SIGVEC(SIGPIPE, &osv, &sv);
  SigPIPEHandler = TRUE;
#endif
}

static ilu_private _tcp_InterpretInfo (char *info)
{
  char *start, *tmp;
  char hostname[1000];
  int port;
  ilu_boolean buffered = FALSE;

  if (*info == 'b')
    buffered = TRUE;
  /* AC: changed from sscanf() to by-hand parse to fix bug in Macintosh sscanf()... */
  start=info + (buffered ? 1 : 0);
  if (strncmp(start,"tcp_",4)!=0) {
  	return (NULL);
  }
  start+=4;	/* advance to hostname */
  if ((tmp=strchr(start,'_'))==NULL)
  	return NULL;
  strncpy(hostname,start,tmp-start);
  hostname[tmp-start]='\0';
  tmp++;
  port=(int) _ilu_strtoul(tmp,NULL,10);
#ifdef BROKEN
  if ((sscanf (info + (buffered ? 1 : 0), "tcp_%[^_]_%u", hostname, &port)) == 2)
#endif
  if (TRUE)
  {
      struct tcpparms *new = (struct tcpparms *) malloc(sizeof(struct tcpparms));
      new->hostname = _ilu_Strdup(hostname);
      new->port = port;
      if (buffered)
	{
	  new->buffer = (bufferList) malloc(sizeof(struct bufferlist_s));
	  new->buffer->size = 0;
	  new->buffer->head = NULL;
	  new->buffer->tail = NULL;
	}
      else
	new->buffer = NULL;
      return ((ilu_private) new);
    }
  else
    return (NULL);
}

static void DumpPacket (unsigned char *packet, short unsigned int length)
{
  unsigned int dumplength, n;
  unsigned char c;
  register int i, j;

  if (length > MAXDUMP)
    {
      fprintf (stderr, "Request to dump packet of %u bytes.  Only %u bytes being dumped.\n", length, MAXDUMP);
      dumplength = MAXDUMP;
    }
  else
    dumplength = length;
  if (packet == NULL)
    {
      fprintf (stderr, "Attempt to dump NULL packet.\n");
      return;
    }
  fprintf (stderr, "DumpPacket of packet 0x%x, length %u bytes, dumping %u bytes:\n",
	   packet, length, dumplength);
  for (i = 0;  i < dumplength;  i += 16)
    {
      fprintf (stderr, "%6u:  ", i);
      for (j = 0;  j < 16 AND (i + j) < dumplength;  j += 1)
	fprintf (stderr, "%02x%s ", packet[i + j], ((j % 4) == 3) ? " " : "");
      n = (((16-j)/4)*(13)+((16-j)%4)*3)+1;		/* padding before ascii version */
      fprintf (stderr, "% *.*s", n, n, "");
      for (j = 0;  j < 16 AND (i + j) < dumplength;  j += 1)
	{
	  c = packet[i + j];
	  fprintf (stderr, "%c", ((c >= ' ') && (c <= '~')) ? (char) c : '.');
	}
      fprintf (stderr, "\n");
    }
}

/*LL >= {conn}*/
static ilu_integer _tcp_NDataPresent (ilu_Transport conn)
{
  TCPStatusPB connstat;
  OSErr err;
  
  err = LowTCPStatus(conn->tr_fd,&connstat);
  if (err!=noErr) {
      fprintf(stderr,
	      "_tcp_NDataPresent(): error in call to LowTCPStatus: %d\n",
	      err);
      exit(1);
    }
  return ( (connstat.amtUnreadData > 0) ? 1 : 0);
}

/*LL = {}*/
static ilu_boolean _tcp_WaitForInput (ilu_Transport conn)
{
/*
 * This should just call the _tcp_NDataPresent() operation above, and give
 * time back to the system, using GiveTime(), until something happens.
 */
  ilu_boolean status=FALSE;
  
  while (!status) {
      status = _tcp_NDataPresent(conn) > 0;	/* locking bug */
      if (!status)
	GiveTime();
    }
  return (status);
}

static ilu_boolean _tcp_FlushOutput (ilu_Transport self)
{
/* in this implementation, _tcp_SendMessage() writes everything
 * synchronously...
 */
#ifdef OLDEN
  bufferList buf;
  bufferElement new, old;
  ilu_boolean status;
  int i;

  if (self == NULL || !ilu_AcquireWriteLock(transport_lock(self)))
    return (FALSE);
  
  if ((buf = TCP_BUFFER(transport_data(self))) != NULL && buf->size > 0)
    {
      for (new = buf->head, status = TRUE;  status && buf->size > 0 && new != NULL;  buf->size -= 1)
	{
	  unsigned char lenbuf[4];
	  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
		(stderr, "_tcp_FlushOutput:  writing %d bytes from 0x%x to fd %d.\n", new->size, new->packet, self->tr_fd));

	  lenbuf[0] = ((new->size >> 24) & 0xFF) | 0x80;
	  lenbuf[1] = (new->size >> 16) & 0xFF;
	  lenbuf[2] = (new->size >> 8) & 0xFF;
	  lenbuf[3] = new->size & 0xFF;
	  if ((i = OS_WRITE (self->tr_fd, lenbuf, 4)) < 4)
	    {
	      fprintf (stderr, "_tcp_SendMessage:  output to fd %u of %d bytes signalled error \"%s\".\n",
		       self->tr_fd, 4, (i < 0) ? ANSI_STRERROR(errno) : ((i == 0) ? "Connection lost" : "not enough bytes written"));
	      status = FALSE;
	    }
	  else if ((i = OS_WRITE (self->tr_fd, new->packet, new->size)) < new->size)
	    {
	      fprintf (stderr, "_tcp_SendMessage:  output to fd %u of %d bytes signalled error \"%s\".\n",
		       self->tr_fd, 4, (i < 0) ? ANSI_STRERROR(errno) : ((i == 0) ? "Connection lost" : "not enough bytes written"));
	      status = FALSE;
	    }
	  else
	    {
	      old = new;
	      new = new->next;
	      free(old->packet);
	      free(old);
	    }
	}
      if (status)
	{
	  buf->size = 0;
	  buf->head = NULL;
	  buf->tail = NULL;
	}
    }

  ilu_ReleaseWriteLock(transport_lock(self));
  return (status);
#endif
  return TRUE;
}

static ilu_boolean _tcp_AddPacketToBuffer(ilu_Transport self, unsigned char *packet, unsigned long size, ilu_boolean eom)
{
  bufferList buf;
  bufferElement new;

  if (self == NULL || !ilu_AcquireWriteLock(transport_lock(self)))
    return (FALSE);

  buf = TCP_BUFFER(transport_data(self));
  new = (bufferElement) malloc(sizeof(struct bufferlist_element_s));

  new->packet = packet;
  new->size = size;
  new->next = NULL;
  new->EOM = eom;
  if (buf->tail == NULL)
    buf->head = buf->tail = new;
  else
    buf->tail->next = new;
  buf->size += 1;

  ilu_ReleaseWriteLock(transport_lock(self));
}

static ilu_boolean _tcp_SendMessage (ilu_Transport conn, unsigned char *buffer, ilu_cardinal count)
{
  ilu_boolean status = TRUE;
  unsigned char lenbuf[4];
  OSErr err;

  if (conn == NULL || !ilu_AcquireWriteLock(transport_lock(conn)))
    return (FALSE);



   lenbuf[0] = ((count >> 24) & 0xFF) | 0x80;
   lenbuf[1] = (count >> 16) & 0xFF;
   lenbuf[2] = (count >> 8) & 0xFF;
   lenbuf[3] = count & 0xFF;
     
	  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr, "_tcp_SendMessage:  writing %d bytes from 0x%x to fd %d.\n", count, buffer, conn->tr_fd));

   /* send length first */
   if ((err=SendData(conn->tr_fd,lenbuf,4))!=noErr) {
	fprintf(stderr,"_tcp_SendMessage(): error from SendData: %d\n",err);
	exit(1);
   }
   /* and then just send the data itself */
   if ((err=SendData(conn->tr_fd,buffer,count))!=noErr) {
	fprintf(stderr,"_tcp_SendMessage(): error from SendData: %d\n",err);
	exit(1);
    } 
	if ((_ilu_DebugLevel & PACKET_DEBUG) != 0)
		DumpPacket(buffer, count);
#ifdef OLDEN
  if (TCP_BUFFER(transport_data(conn)) == NULL || (!_tcp_AddPacketToBuffer(conn, buffer, count, FALSE)))
    {
      unsigned char lenbuf[4];
      ilu_boolean status;
      int i;

      _tcp_FlushOutput (conn);

      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr, "_tcp_SendMessage:  writing %d bytes from 0x%x to fd %d.\n", count, buffer, conn->tr_fd));

      if ((_ilu_DebugLevel & PACKET_DEBUG) != 0)
	DumpPacket(buffer, count);

      lenbuf[0] = ((count >> 24) & 0xFF) | 0x80;
      lenbuf[1] = (count >> 16) & 0xFF;
      lenbuf[2] = (count >> 8) & 0xFF;
      lenbuf[3] = count & 0xFF;
      status = TRUE;
      if ((i = OS_WRITE (conn->tr_fd, lenbuf, 4)) < 4)
	{
	  fprintf (stderr, "_tcp_SendMessage:  output to fd %u of %d bytes signalled error \"%s\".\n",
		   conn->tr_fd, 4,
		   (i < 0) ? ANSI_STRERROR(errno) : ((i == 0) ? "Connection lost" : "not enough bytes written"));
	  status = FALSE;
	}
      else
	{
	  if ((i = OS_WRITE (conn->tr_fd, buffer, count)) < count)
	    {
	      fprintf (stderr, "_tcp_SendMessage:  output to fd %u of %d bytes signalled error \"%s\".\n",
		       conn->tr_fd, count,
		       (i < 0) ? ANSI_STRERROR(errno) : ((i == 0) ? "Connection lost" : "not enough bytes written"));
	      status = FALSE;
	    }
	  else
	    status = TRUE;
	}
      free(buffer);
    };
#endif
  ilu_ReleaseWriteLock(transport_lock(conn));
  return (status);
}

static ilu_boolean _tcp_Connect (ilu_Transport self)
{
  unsigned long bufsiz=32768;
  unsigned long fd;
  unsigned long addr;
  char *hostname;
  ilu_boolean status = TRUE;
  OSErr err;

  _ilu_AutoSetDebugLevel();
  if (!SigPIPEHandler)
    _tcp_HandleSigPIPE();

  if (self == NULL)
    return (FALSE);

  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	(stderr, "_tcp_Connect:  connecting to host %s, port %d...\n",
	 TCP_HOSTNAME(transport_data(self)), TCP_PORT(transport_data(self))));

  if (!ilu_AcquireWriteLock(transport_lock(self)))
    {
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr, "_tcp_Connect:  Can't acquire write lock on 0x%x.\n", self));
      return (FALSE);
    }
   /* get host IP address */
   hostname=TCP_HOSTNAME(transport_data(self));
   if ((err=IPNameToAddr(hostname,&addr))!=noErr) {
	fprintf(stderr,"Error resolving hostname %s: %d\n",hostname,err);
	exit(1);
    }
    /* open stream TCP connection to appropriate port on host now that
     * we have resolved the address
     */
    if ((err=CreateStream(&fd,bufsiz))!=noErr) {
	fprintf(stderr,"Error creating TCP stream: %d\n",err);
	exit(1);
    }
    if ((err=OpenConnection(fd,addr,TCP_PORT(transport_data(self)),20))!=noErr) {
	fprintf(stderr,"Error opening connection: %d\n",err);
		if ((err=ReleaseStream(fd))!=noErr) {
			fprintf(stderr,"Error releasing stream after OpenConnection() failure: %d\n",err);
		}
	exit(1);
    }
    self->tr_fd = fd;
#ifdef OLDEN
  memset((char *) &sin, 0, sizeof(sin));
  if ((fd = OS_SOCKET(AF_INET, SOCK_STREAM, 0)) < 0)
    {
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr, "_tcp_Connect:  socket call failed:  %s.\n", ANSI_STRERROR(errno)));
      status = FALSE;
    }
  else
    {
      (void) OS_SETSOCKOPT(fd, SOL_SOCKET, SO_REUSEADDR,(char *) NULL, 0);
      (void) OS_SETSOCKOPT(fd, SOL_SOCKET, SO_USELOOPBACK,(char *) NULL, 0);

/* the following was suggested by Ken Birman of the Cornell ISIS project */
#ifdef IPPROTO_TCP
#ifdef TCP_NODELAY
      {
	int enable = 1;
	OS_SETSOCKOPT (fd, IPPROTO_TCP, TCP_NODELAY, &enable, sizeof(enable));
      }
#endif /* TCP_NODELAY */
#endif /* IPPROTO_TCP */     

      sin.sin_family = AF_INET;
      sin.sin_port = TCP_PORT(transport_data(self));

      sin.sin_addr.s_addr = OS_INETADDR(TCP_HOSTNAME(transport_data(self)));
      if (sin.sin_addr.s_addr == -1 || sin.sin_addr.s_addr == 0)
	if ((hp = OS_GETHOSTBYNAME(TCP_HOSTNAME(transport_data(self)))) != NULL)
	  OS_BCOPY(hp->h_addr,(char *) &sin.sin_addr, hp->h_length);

      if (sin.sin_addr.s_addr == -1 || sin.sin_addr.s_addr == 0)
	{
	  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
		(stderr, "_tcp_Connect:  Invalid host name (%s)\n", TCP_HOSTNAME(transport_data(self))));
	  status = FALSE;
	}
      else
	{
	  if (OS_CONNECT(fd,(struct sockaddr *) &sin, sizeof(sin)) < 0)
	    {
	      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
		    (stderr, "%s %s[%d] failed, error %d (%s)\n",
		     "_tcp_Connect: connect to",
		     OS_INETNTOA(sin.sin_addr), TCP_PORT(transport_data(self)), errno, ANSI_STRERROR(errno)));
	      OS_CLOSE(fd);
	      status = FALSE;
	    }
	  else
	    {
	      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
		    (stderr, "_tcp_Connect:  connected to %s[%d]\n", inet_ntoa(sin.sin_addr), TCP_PORT(transport_data(self))));
	      self->tr_fd = fd;
	      status = TRUE;
	    }
	}
    }
#endif
  ilu_ReleaseWriteLock(transport_lock(self));
  return (status);
}

static void _tcp_Close (ilu_Transport self)
{
  OSErr err;

  if (self == NULL || !ilu_AcquireWriteLock(transport_lock(self)))
    return;
/*
  if ((err=CloseConnection(self->tr_fd))!=noErr) {
	fprintf(stderr,"error closing connection: %d\n",err);
	exit(1);
  }
 */
  if ((err=ReleaseStream(self->tr_fd))!=noErr) {
	fprintf(stderr,"error releasing stream: %d\n", err);
	exit(1);
  } 
  ilu_ReleaseWriteLock(transport_lock(self));
}

static ilu_boolean _tcp_ReadMessage (ilu_Transport self, ilu_bytes *buffer, ilu_cardinal *size)
{
  OSErr err;
  unsigned char lenbuf[4];
  ilu_cardinal i, bytesToRead, bytesRead, totalSize = 0;
  unsigned short bsize;
  ilu_bytes packet = NULL;
  ilu_boolean status = FALSE;
  ilu_boolean lastPacket;

  if (self == NULL || !ilu_AcquireWriteLock(transport_lock(self)))
    return(FALSE);

  do {
    /* try and read the length first */
	/* The following comment no longer holds:  This add 1 business was really bogus. */
    /* we'll try a length of 5 for now, because RecvData() src. decrements
     * this value by 1 before making sys. call
     */
    bsize=4;
    if ((err=RecvData(self->tr_fd, lenbuf, &bsize, true))!=noErr) {
	fprintf(stderr,"error in len call to RecvData: %d\n",err);
	return FALSE;
    }
	DEBUG((INCOMING_DEBUG | TCP_DEBUG), (stderr, "_tcp_ReadMessage:  lenbuf is %d.%d.%d.%d\n",
				     lenbuf[0], lenbuf[1], lenbuf[2], lenbuf[3]));

    lastPacket = (lenbuf[0] & 0x80) != 0;
    bytesToRead = ((lenbuf[0] & 0x7F) << 24) | (lenbuf[1] << 16) | (lenbuf[2] << 8) | lenbuf[3];
    bytesRead = 0;
    if (packet == NULL) {
      packet = (ilu_bytes) malloc(bytesToRead);
	  if (packet==NULL) {
	  	DEBUG((INCOMING_DEBUG | TCP_DEBUG), (stderr, "_tcp_ReadMessage:  malloc() failed to alloc %d bytes.\n",bytesToRead));
		return FALSE;
	  }
	} else {
      packet = (ilu_bytes) realloc (packet, bytesToRead + totalSize);
	  if (packet==NULL) {
	  	DEBUG((INCOMING_DEBUG | TCP_DEBUG), (stderr, "_tcp_ReadMessage:  realloc() failed to alloc %d bytes.\n",bytesToRead + totalSize));
		return FALSE;
	  }
	}
    status = TRUE;
    while (bytesToRead > 0 && status) {
	/* once again, we add 1 and hope it works... */
	bsize=bytesToRead;
	if ((err=RecvData(self->tr_fd,packet+totalSize+bytesRead,
			&bsize, true))!=noErr) {
		fprintf(stderr,"Error calling RecvData: %d\n",err);
		exit(1);
	}
	bytesRead+=bsize;
	bytesToRead-=bsize;			
    }
    totalSize += bytesRead;
  } while (status AND (NOT lastPacket));
      
#ifdef NOPE
    if ((i = OS_READ (self->tr_fd, lenbuf, 4)) < 4)
      {
	DEBUG((INCOMING_DEBUG | TCP_DEBUG),
	      (stderr, "_tcp_ReadMessage:  read on fd %d returns %d (errno is %s) while reading packet length\n",
	       self->tr_fd, i, ANSI_STRERROR(errno)));
	status = FALSE;
      }
    else
      {
	DEBUG((INCOMING_DEBUG | TCP_DEBUG), (stderr, "_tcp_ReadMessage:  lenbuf is %d.%d.%d.%d\n",
					     lenbuf[0], lenbuf[1], lenbuf[2], lenbuf[3]));
	lastPacket = (lenbuf[0] & 0x80) != 0;
	bytesToRead = ((lenbuf[0] & 0x7F) << 24) | (lenbuf[1] << 16) | (lenbuf[2] << 8) | lenbuf[3];
	bytesRead = 0;
	if (packet == NULL)
	  packet = (ilu_bytes) malloc(bytesToRead+128);
	else
	  packet = (ilu_bytes) realloc (packet, bytesToRead + totalSize+128);
	status = TRUE;
	while (bytesToRead > 0 && status)
	  {
	    i = OS_READ (self->tr_fd, packet + totalSize + bytesRead, bytesToRead);
	    if (i <= 0)
	      {
		DEBUG((INCOMING_DEBUG | TCP_DEBUG),
		      (stderr, "_tcp_ReadMessage:  read on fd %d returns %d (errno is %s) while reading packet of length %u, having already read %u bytes.\n",
		       self->tr_fd, i, ANSI_STRERROR(errno), bytesToRead, bytesRead));
		status = FALSE;
	      }
	    else
	      {
		DEBUG((INCOMING_DEBUG | TCP_DEBUG),
		      (stderr, "_tcp_ReadMessage:  read %d bytes from fd %d while asking for %u bytes.\n",
		       i, self->tr_fd, bytesToRead));
		bytesRead += i;
		bytesToRead -= i;
	      }
	  }
	totalSize += bytesRead;
      }
  } while (status AND (NOT lastPacket));
#endif

  if (status)
    {
      *buffer = packet;
      *size = totalSize;
    }
  else
    {
      if (packet != NULL)
	free(packet);
    }

  ilu_ReleaseWriteLock(transport_lock(self));

  if (status AND (_ilu_DebugLevel & PACKET_DEBUG) != 0)
    DumpPacket(*buffer, *size);

  return (status);
}

/***********************************************************************
/***********************************************************************
/***********************************************************************
/**** Now the methods for the TCP Mooring ******************************
/***********************************************************************
/***********************************************************************
/**********************************************************************/

static ilu_string _tcp_FormHandle (TCPParms parms)
{
  char buf[1000];

  if (parms == NULL)
    return (NULL);
  sprintf (buf, "%stcp_%s_%u", (TCP_BUFFER(parms) == NULL) ? "" : "b",
	   TCP_HOSTNAME(parms), TCP_PORT(parms));
  return (_ilu_Strdup(buf));
}

static ilu_Transport _tcp_AcceptClient (ilu_Mooring self)
{
#ifdef NOPE
  register int ns;
  struct sockaddr_in addr;
  ilu_Transport new = NULL;
  long addrlen;
  ilu_TransportClass _ilu_tcp_TransportClass(void);

  _ilu_AutoSetDebugLevel();

  if (self == NULL || !ilu_AcquireWriteLock(mooring_lock(self)))
    return (NULL);

  DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	(stderr, "_tcp_AcceptClient: mooring file descriptor is %d\n", self->tr_fd));

  addrlen = sizeof(addr);

  if ((ns = OS_ACCEPT(self->tr_fd, &addr, &addrlen)) < 0)
    {
      perror ("_tcp_AcceptClient:  accept");
    }
  else
    {
      DEBUG((CONNECTION_DEBUG | TCP_DEBUG),
	    (stderr, "_tcp_AcceptClient: new connection on fd %d\n", ns));

      new = (ilu_Transport) malloc (sizeof(struct _ilu_Transport_s));
      new->tr_class = _ilu_tcp_TransportClass();
      new->tr_fd = ns;
      new->tr_data = self->tr_data;
    }
  ilu_ReleaseWriteLock(mooring_lock(self));
  return (new);
#endif
}

static ilu_Mooring _ilu_tcp_CreateMooring (TCPParms parms)
{
#ifdef NOPE
  struct sockaddr_in addr;
  struct linger linger;
  int skt, namelen;
  int on = 1;
  ilu_Mooring self;
  
  _ilu_AutoSetDebugLevel();
  if (!SigPIPEHandler)
    _tcp_HandleSigPIPE();

  /* setup the new server */
  
  if ((skt = OS_SOCKET(AF_INET, SOCK_STREAM, 0)) < 0) {
    DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	  (stderr, "_tcp_CreateMooring: socket failed:  %s.\n", ANSI_STRERROR(errno)));
    return (NULL);
  }
    
  OS_SETSOCKOPT(skt, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
  linger.l_onoff = 0;
  OS_SETSOCKOPT(skt, SOL_SOCKET, SO_LINGER, &linger, sizeof(struct linger));

  addr.sin_family = AF_INET;
  addr.sin_port = TCP_PORT(parms);
  addr.sin_addr.s_addr = INADDR_ANY;	/* me, me! */

  if (OS_BIND(skt, &addr, sizeof(addr)) < 0) {
    DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	  (stderr, "_tcp_CreateMooring: bind to port %d failed:  %s.\n",
	   TCP_PORT(parms), ANSI_STRERROR(errno)));
    OS_CLOSE (skt);
    return (NULL);
  }

  /* If servicePort was 0 then discover kernel allocated port */
  
  if (addr.sin_port == 0) {
    namelen = sizeof(addr);
    if (OS_GETSOCKNAME(skt, (struct sockaddr *) &addr, &namelen) < 0)
      {
	DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	      (stderr, "_tcp_CreateMooring: getsockname failed:  %s.\n", ANSI_STRERROR(errno)));
	OS_CLOSE (skt);
	return (NULL);
      }
  }

  if (OS_LISTEN(skt, 4) < 0) {
    DEBUG((EXPORT_DEBUG | TCP_DEBUG),
	  (stderr, "_tcp_CreateMooring:  listen on port %d failed:  %s.\n",
	   addr.sin_port, ANSI_STRERROR(errno)));
    OS_CLOSE (skt);
    return (NULL);
  }

  self = (ilu_Mooring) malloc (sizeof(struct _ilu_Mooring_s));

  self->mo_lock = ilu_CreateLock(self);
  self->mo_fd = skt;
  self->mo_transportClass = _ilu_tcp_TransportClass();
  self->mo_connection_pending_p = NULL;
  self->mo_accept_connection = _tcp_AcceptClient;
  TCP_PORT(parms) = addr.sin_port;
  if (TCP_HOSTNAME(parms) != NULL)
    free(TCP_HOSTNAME(parms));
  TCP_HOSTNAME(parms) = _ilu_Strdup(_ilu_tcp_CurrentHostInetName());
  self->mo__data = (ilu_private) parms;

  return (self);
#endif
}

ilu_TransportClass _ilu_tcp_TransportClass (void)
{
  static ilu_TransportClass m = NULL;
  if (m == NULL)
    {
      m = (ilu_TransportClass) malloc(sizeof(struct _ilu_TransportClass_s));
      m->type = ilu_TransportType_TCP;
      m->tc_interpret_info = _tcp_InterpretInfo;
      m->tc_form_info = (ilu_string (*)(ilu_refany)) _tcp_FormHandle;
      m->tc_connect = _tcp_Connect;
      m->tc_close = _tcp_Close;
      m->tc_n_data_present = _tcp_NDataPresent;
      m->tc_wait_for_input = _tcp_WaitForInput;
      m->tc_send_message = _tcp_SendMessage;
      m->tc_read_message = _tcp_ReadMessage;
      m->tc_flush_output = _tcp_FlushOutput;
      m->tc_create_mooring = (ilu_Mooring (*)(ilu_private)) _ilu_tcp_CreateMooring;
    }
  return (m);
}

/***********************************************************************
/***********************************************************************
/***********************************************************************
/**** TCP utilities ****************************************************
/***********************************************************************
/***********************************************************************
/**********************************************************************/

char *_ilu_tcp_CurrentHostInetName(void)
{
#ifdef NOPE
  char name[100];
  static char *inetname = NULL;
  struct hostent *he;
  char *p;

  if (inetname == NULL)
    {
      if (OS_GETHOSTNAME(name,sizeof(name)) != 0) {
	perror("no hostname for this machine!");
	return (NULL);
      }
  
      he = OS_GETHOSTBYNAME(name);
      p = (char *) inet_ntoa (he->h_addr_list[0]);
      inetname = _ilu_Strdup (p);
    }
  return (inetname);
#endif
}


/* Last tweaked by Mike Spreitzer October 1, 1993 8:58 am PDT */
     
 ù  5  ïNewlineDelimiter
ô Jòƒ¿Jò@ó Öó    `Ü  `√  
