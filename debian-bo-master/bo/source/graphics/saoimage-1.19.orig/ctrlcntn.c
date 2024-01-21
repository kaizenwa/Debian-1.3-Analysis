#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	ctrlcntn.c (Control Connection)
 * Purpose:	Handle all IO with remote independent processes
 * Subroutine:	init_connections()		returns: void
 * Subroutine:	open_connection()		returns: int
 * Subroutine:	close_connection()		returns: void
 * Subroutine:	flush_connection()		returns: void
 * Subroutine:	respond_to_connection()		returns: void
 * Subroutine:	read_connection()		returns: int
 * Subroutine:	write_connection()		returns: int
 * Copyright:	1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} M. VanHilst	initial version			10 March 1990
 *		{1} D. Mink SunOS file descriptor bit limit	 8 Sept. 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */ 
#include "hfiles/control.h"	/* declare control structure types */
extern struct controlRec control;
extern Display *display;

#define IOP_socket_listener 777
#define IOP_socket_acceptee 555

/*
 * Subroutine:	init_connections
 * Purpose:	set up connections after display connection has been opened
 * Note:	necessary since parser precedes display connection
 */
void init_connections ()
{
  if( control.IRAF_in.open == 1 ) {
    control.IRAF_in.open = 0;
    if( control.IRAF_in.func != NULL )
      (*control.IRAF_in.func)();
  }
  if( control.AIPS_in.open == 1 ) {
    control.AIPS_in.open = 0;
    if( control.AIPS_in.func != NULL )
      (*control.AIPS_in.func)();
  }
  if( control.aux_in.open == 1 ) {
    control.aux_in.open = 0;
    if( control.aux_in.func != NULL )
      (*control.aux_in.func)();
  }
}

/*
 * Subroutine:	open_connection
 * Purpose:	Open a connection to a remote process and update event handlers
 * Returns:	-1 on failure, else IPC number
 */
int open_connection ( connection )
     struct connectRec *connection;
{
  int ipc, flush_flag;
#ifdef VMS
  extern int open_mailbox();
#else
  extern int ButtonSelectMask(), open_pipe();
  extern int open_socket_listener();
  extern struct connectRec *accept_socket_connection();
  static void init_select();
#endif

#ifdef NOPIPEFLUSH
  flush_flag = 0;
#else
  flush_flag = 1;
#endif

  if( ((connection->type != IOP_socket) &&
       (connection->name == NULL)) ||
      ((connection->type == IOP_socket) &&
       (connection->address <= 0)) ) {
    if( connection->direction == IOP_Write )
      (void)fprintf(stderr,"Error: output device not defined\n");
    else
      (void)fprintf(stderr,"Error: input device not defined\n");
    return( -1 );
  }
  if( (connection->type != IOP_socket_listener) &&
      (connection->open && (connection->fd >= 0)) ) {
    (void)fprintf(stderr, "Warning: connection already open: %s\n",
		  connection->name);
    return(connection->fd);
  }
#ifdef VMS
  if( connection->type == IOP_mailbox ) {
    ipc = open_mailbox(connection->name, connection->direction, 1);
  }
#else
  if( connection->type == IOP_pipe ) {
    ipc = open_pipe(connection->name, connection->direction, flush_flag);
  } else if( connection->type == IOP_socket ) {
    ipc = open_socket_listener(&(connection->name), connection->address);
  } else if( connection->type == IOP_socket_listener ) {
    struct connectRec *temp;

    if( (temp = accept_socket_connection(connection)) != NULL ) {
      connection = temp;
      ipc = connection->fd;
      connection->type = IOP_socket_acceptee;
    } else
      ipc = -1;
  }
#endif
  if( ipc < 0 ) {
    if( connection->direction == IOP_Write )
      (void)fprintf(stderr,"Error: No remote output possible.\n");
    else
      (void)fprintf(stderr,"Error: No remote input possible.\n");
    connection->fd = -1;
    connection->open = 0;
    return( -1 );
  } else {
    connection->fd = ipc;
    connection->open = 1;
#ifdef VMS
    {
      extern int  XZ_efn;
      extern void XZ_ast();

      ZSelectAsyncInput(ipc, XZ_ast, XZ_efn);
    }
#else
    if( (control.select_size <= 0) || (control.Xserver.open != 1) )
      init_select();
    /* compute IPC channel's select mask and set up event handling */
    if( ipc < 32 ) {
      connection->mask[0] = (1 << ipc);
      connection->mask[1] = 0;
      connection->mask[2] = 0;
      connection->mask[3] = 0;
    } else if( ipc < 64 ) {
      connection->mask[0] = 0;
      connection->mask[1] = (1 << (ipc - 32));
      connection->mask[2] = 0;
      connection->mask[3] = 0;
    } else if( ipc < 96 ) {
      connection->mask[0] = 0;
      connection->mask[1] = 0;
      connection->mask[2] = (1 << (ipc - 64));
      connection->mask[3] = 0;
    } else if( ipc < 128 ) {
      connection->mask[0] = 0;
      connection->mask[1] = 0;
      connection->mask[2] = 0;
      connection->mask[3] = (1 << (ipc - 96));
    } else {
      (void)fprintf(stderr, "device channel out of range\n");
      return 0;
    }
#endif
    if( (connection->direction != IOP_Write) ||
        (connection->type == IOP_socket) ) {
      /* if connnection used to read, add it to event accepting list */
      connection->next = control.Xserver.next;
      control.Xserver.next = connection;
      ++control.remote_connected;
#ifndef VMS
      /* add bits to client's mask and adjust client's mask size */
      control.select_mask[0] |= connection->mask[0];
      control.select_mask[1] |= connection->mask[1];
      control.select_mask[2] |= connection->mask[2];
      control.select_mask[4] |= connection->mask[3];
      /* inform the event handler in buttonlib about our ipc messages */
      if( ButtonSelectMask(display, connection->mask,
			   control.select_size, 1) != 1 )
	(void)fprintf(stderr,"Warning: error setting button event mask\n");
#endif
    }
  }
  return ipc;
}

/*
 * Subroutine:	close_connection
 * Purpose:	Close the remote process connection and update event handlers
 */
void close_connection ( connection )
     struct connectRec *connection;
{
#ifdef VMS
  extern int close_mailbox();
#else
  extern int ButtonSelectMask();
  extern void close_pipe(), close_socket();
#endif

  if( connection->open ) {
#ifdef VMS
    if( connection->type == IOP_mailbox )
      (void)close_mailbox(connection->fd, connection->name);
#else
    if( connection->type == IOP_pipe ) {
      close_pipe(connection->fd, connection->name);
    } else if( (connection->type == IOP_socket) ||
	       (connection->type == IOP_socket_listener) ||
	       (connection->type == IOP_socket_acceptee) ) {
      close_socket(connection->fd, connection->name);
    }
#endif
    connection->fd = -1;
    connection->open = 0;
    if( (connection->type == IOP_socket_listener) ||
	(connection->type == IOP_socket) ||
	(connection->direction != IOP_Write) ) {
      struct connectRec *port = &control.Xserver;

      /* remove connection from event servicing queue */
      while( (port != NULL) && (port->next != connection) )
	port = port->next;
      if( (port != NULL) && (port->next == connection) ) {
	port->next = connection->next;
	connection->next = NULL;
	--control.remote_connected;
      }
#ifdef VMS
    }
#else
      /* remove connection from UNIX select event servicing masks */
      if( (connection->mask[0] & control.select_mask[0]) ||
	  (connection->mask[1] & control.select_mask[1]) || 
	  (connection->mask[1] & control.select_mask[1]) ||
	  (connection->mask[1] & control.select_mask[1]) ) {
	if( ButtonSelectMask((Display *)NULL, connection->mask,
			     control.select_size, 0) )
	  (void)fprintf(stderr, "Warning: error clearing button event mask\n");
	if( (connection->mask[0] != 0) &&
	    (connection->mask[0] != control.Xserver.mask[0]) ) {
	  control.select_mask[0] &= (~(connection->mask[0]));
          connection->mask[0] = 0;
        }
	if( (connection->mask[1] != 0) &&
	    (connection->mask[1] != control.Xserver.mask[1]) ) {
	  control.select_mask[1] &= (~(connection->mask[1]));
	  connection->mask[1] = 0;
	}
	if( (connection->mask[2] != 0) &&
	    (connection->mask[2] != control.Xserver.mask[1]) ) {
	  control.select_mask[2] &= (~(connection->mask[1]));
	  connection->mask[2] = 0;
	}
	if( (connection->mask[3] != 0) &&
	    (connection->mask[3] != control.Xserver.mask[1]) ) {
	  control.select_mask[3] &= (~(connection->mask[1]));
	  connection->mask[3] = 0;
	}
      }
      /* if only X connections are sought, turn off remote connection flag */
      if( (control.select_mask[0] == control.Xserver.mask[0]) &&
	  (control.select_mask[1] == control.Xserver.mask[1]) &&
	  (control.select_mask[2] == control.Xserver.mask[2]) &&
	  (control.select_mask[3] == control.Xserver.mask[3]) )
    	control.remote_connected = 0;
    }
    connection->mask[0] = 0;
    connection->mask[1] = 0;
    connection->mask[2] = 0;
    connection->mask[3] = 0;
    /* coordinate sockets reader and listener */
    if( connection->type == IOP_socket_acceptee ) {
      /* closing a connection reopens its listener */
      connection->affiliate->type = IOP_socket;
      connection->affiliate->affiliate = NULL;
      (void)open_connection(connection->affiliate);
      /* acceptees are malloc'd space */
      free(connection);
    }
  } else if( (connection->type == IOP_socket_listener) &&
	     (connection->affiliate != NULL) &&
	     connection->affiliate->open ) {
    /* if closing an inactive listener, close its affiliate connection */
    if( connection->affiliate->direction == IOP_Write ) {
      close_socket(connection->affiliate);
    } else {
      /* clear up event descritor if not write-only */
      connection->affiliate->type = IOP_socket;
      close_connection(connection->affiliate);
    }
    free(connection->affiliate);
    connection->affiliate = NULL;
#endif
  }
}

/*
 * Subroutine:	flush_connection
 * Purpose:	Suck all bytes out of a pipe open for reading
 */
void flush_connection ( connection )
     struct connectRec *connection;
{
#ifdef VMS
  extern void flush_mailbox();
#else
  extern void flush_pipe(), flush_socket();
#endif

#ifdef VMS
  if( connection->type == IOP_mailbox )
    flush_mailbox(connection->fd, connection->name);
#else
  if( connection->type == IOP_pipe )
    flush_pipe(connection->fd, connection->name);
  else if( connection->type == IOP_socket_acceptee )
    flush_socket(connection->fd, connection->name);
#endif
}

/*
 * Subroutine:	respond_to_connection
 * Purpose:	call the function for this connection event
 */
void respond_to_connection ( id )
     int *id;	/* call identifier (UNIX bit mask or VMS mailbox number) */
{
  int i;
  struct connectRec *port;

  port = control.Xserver.next;
  /* loop has redundant terminations which should coincide */
  for( i=0; (port != NULL) && (i<control.remote_connected); i++ ) {
#ifdef VMS
    if( *id == port->fd )
#else
    if( (id[0] & port->mask[0]) || (id[1] & port->mask[1]) ||
        (id[2] & port->mask[2]) || (id[3] & port->mask[3]) )
#endif
      continue;
    port = port->next;
  }
  if( port != NULL ) {
#ifndef VMS
    if( port->type == IOP_socket ) {
      /* event on open listener means open socket acceptee, close listener */
      port->type = IOP_socket_listener;
      /* open on redefined listener creates the acceptee */
      if( open_connection(port) < 0 )
        port->type = IOP_socket;
      else {
	close_connection(port);
	/* set the file descriptor, in case somebody tries to write to it */
        port->fd = port->affiliate->fd;
      }
    } else 
#endif
      if( port->func != NULL )
	/* execute the connection's response function */
        (*port->func)(port);
#ifdef DEBUG
  } else
    (void)fprintf(stderr, "Unrecognized select or mailbox event\n");
#else
  }
#endif
}

/*
 * Subroutine:	read_connection
 * Purpose:	read specified number of bytes from connection into buf
 */
int read_connection ( connection, buf, bytes )
     struct connectRec *connection;
     char *buf;
     int bytes;
{
  int status;
#ifdef VMS
  extern int read_mailbox();

  if( connection->type == IOP_mailbox )
    status = read_mailbox(connection->fd, buf, bytes, 1, connection->name,
			  "mailbox input");
#else
  extern int read_disk();

  if( (connection->type == IOP_pipe) ||
      (connection->type == IOP_socket_acceptee) )
    status = read_disk(connection->fd, buf, bytes, 1, connection->name,
		       "remote input");
#endif
  else
    status = -1;
  return( status );
}

/*
 * Subroutine:	write_connection
 * Purpose:	write specified number of bytes to connection from buf
 */
int write_connection ( connection, buf, bytes )
     struct connectRec *connection;
     char *buf;
     int bytes;
{
  int status;
#ifdef VMS
  extern int write_mailbox();

  if( connection->type == IOP_mailbox )
    status = write_mailbox(connection->fd, buf, bytes, connection->name);
#else
  extern int write_disk();

  if( (connection->type == IOP_pipe) ||
      (connection->type == IOP_socket_acceptee) )
    status = write_disk(connection->fd, buf, bytes, connection->name);
  else if( connection->type == IOP_socket_listener )
    status = write_disk(connection->affiliate->fd, buf, bytes,
			connection->name);
#endif
  else
    status = -1;
  return( status );
}

#ifndef VMS
/*
 * Subroutine:	init_select
 * Purpose:	Initialize event handler parameters
 * Note:	SYSV machines don't have an easy way to get the number of
 *		device bits in the select mask.  Most have 64.  Under BSD,
 *		Sun uses 32 (1 int), Apollo uses 128 (4 ints).
 */
static void init_select ()
{
  int server_chan;

#if defined SYSV || defined SUN
  control.select_size = 64;
#else
  control.select_size = getdtablesize();
#endif
  /* get the x window server channel and compute its mask */
  server_chan = ConnectionNumber(display);
  if( server_chan < 32 ) {
    control.Xserver.mask[0] = (1 << server_chan);
    control.Xserver.mask[1] = 0;
    control.Xserver.mask[2] = 0;
    control.Xserver.mask[3] = 0;
  } else if( server_chan < 64 ) {
    control.Xserver.mask[0] = 0;
    control.Xserver.mask[1] = (1 << (server_chan - 32));
    control.Xserver.mask[2] = 0;
    control.Xserver.mask[3] = 0;
  } else if( server_chan < 96 ) {
    control.Xserver.mask[0] = 0;
    control.Xserver.mask[1] = 0;
    control.Xserver.mask[2] = (1 << (server_chan - 64));
    control.Xserver.mask[3] = 0;
  } else if( server_chan < 64 ) {
    control.Xserver.mask[0] = 0;
    control.Xserver.mask[1] = 0;
    control.Xserver.mask[2] = 0;
    control.Xserver.mask[3] = (1 << (server_chan - 96));
  }
  /* initialize combined mask and open new pipe */
  control.select_mask[0] = control.Xserver.mask[0];
  control.select_mask[1] = control.Xserver.mask[1];
  control.select_mask[2] = control.Xserver.mask[2];
  control.select_mask[3] = control.Xserver.mask[3];
  control.Xserver.open = 1;
}
#endif
