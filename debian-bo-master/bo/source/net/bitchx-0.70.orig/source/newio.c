/*
 * newio.c: This is some handy stuff to deal with file descriptors in a way
 * much like stdio's FILE pointers 
 *
 * IMPORTANT NOTE:  If you use the routines here-in, you shouldn't switch to
 * using normal reads() on the descriptors cause that will cause bad things
 * to happen.  If using any of these routines, use them all 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"
#include "ircaux.h"
#include "newio.h"

#ifdef ISC22
# include <sys/bsdtypes.h>
#endif /* ISC22 */

#include "irc_std.h"

#ifdef WINNT
#define WIN32_LEAN_AND_MEAN

#include <winsock.h>

#include <ntport.h>
#endif

#define IO_BUFFER_SIZE 512

#ifdef HAVE_SYSCONF
# define IO_ARRAYLEN sysconf(_SC_OPEN_MAX)
#else
# ifdef FD_SETSIZE
#  define IO_ARRAYLEN FDSETSIZE
# else
#  define IO_ARRAYLEN NFDBITS
# endif
#endif

typedef	struct	myio_struct
{
	char	buffer[IO_BUFFER_SIZE + 1];
	unsigned int	read_pos,
			write_pos;
	unsigned	misc_flags;
#ifdef WINNT
	int sockhandle;
	int buffer_pending;
	struct myio_struct *next;
	unsigned issock;
#endif WINNT
}           MyIO;

#define IO_SOCKET 1

static	struct	timeval	right_away = { 0L, 0L };
static	MyIO	**io_rec;

static	struct	timeval	dgets_timer;
static	struct	timeval	*timer;
	int	dgets_errno = 0;

static	void	init_io _((void));

/*
 * dgets_timeout: does what you'd expect.  Sets a timeout in seconds for
 * dgets to read a line.  if second is -1, then make it a poll.
 */
extern	time_t dgets_timeout(int sec)
{
	time_t	old_timeout = dgets_timer.tv_sec;

	if (sec)
	{
		dgets_timer.tv_sec = (sec == -1) ? 0 : sec;
		dgets_timer.tv_usec = 0;
		timer = &dgets_timer;
	}
	else
		timer = NULL;
	return old_timeout;
}

static	void init_io(void)
{
	static	int	first = 1;

	if (first)
	{
		int	c, max_fd = IO_ARRAYLEN;

		io_rec = (MyIO **)new_malloc(sizeof(MyIO *) * max_fd);
		for (c = 0; c < max_fd; c++)
			io_rec[c] = NULL;
		(void) dgets_timeout(-1);
		first = 0;
	}
}

/*
 * dgets: works much like fgets except on descriptor rather than file
 * pointers.  Returns the number of character read in.  Returns 0 on EOF and
 * -1 on a timeout (see dgets_timeout()) 
 */
int dgets(char *str, int len, int des, char *specials)
{
	int	cnt = 0,
		c;
	fd_set	rd;
	int	BufferEmpty;

	init_io();
#ifndef WINNT
	if (io_rec[des] == NULL)
	{
		io_rec[des] = (MyIO *) new_malloc(sizeof(MyIO));
		io_rec[des]->read_pos = 0;
		io_rec[des]->write_pos = 0;
		io_rec[des]->misc_flags = 0;
	}
#else
	io_rec_des = FindOrNew(des);
	if (io_rec_des == NULL)
		return -1;
#endif

#ifndef WINNT
	while (1)
	{
		if ((BufferEmpty = (io_rec[des]->read_pos == io_rec[des]->write_pos)))
		{
			if(BufferEmpty)
			{
				io_rec[des]->read_pos = 0;
				io_rec[des]->write_pos = 0;
			}
			FD_ZERO(&rd);
			FD_SET(des, &rd);
			switch (select(des + 1, &rd, NULL, NULL, timer))
			{
				case 0:
				{
					str[cnt] = (char) 0;
					dgets_errno = 0;
					return (-1);
				}
				default:
				{
					c = read(des, io_rec[des]->buffer +
					 io_rec[des]->write_pos,
					 IO_BUFFER_SIZE-io_rec[des]->write_pos);
	
					if (c <= 0)
					{
						if (c == 0)
							dgets_errno = -1;
						else
							dgets_errno = errno;
						return 0;
					}
					io_rec[des]->write_pos += c;
					break;
				}
			}
		}
		while (io_rec[des]->read_pos < io_rec[des]->write_pos)
		{
			if (((str[cnt++] = io_rec[des]->buffer[(io_rec[des]->read_pos)++])
				== '\n') || (cnt == len))
			{
				dgets_errno = 0;
				str[cnt] = (char) 0;
				return (cnt);
			}
		}
	}
#else
#error ("This made no sense to me")
#endif
}

/*
 * new_select: works just like select(), execpt I trimmed out the excess
 * parameters I didn't need.  
 */
int new_select(fd_set *rd, fd_set *wd, struct timeval *timeout)
{
	int	i,
		set = 0;
		fd_set new;
	struct	timeval	*newtimeout,
			thetimeout;
		int	max_fd = -1;
	static	int	num_fd = 0;
	
	if (!num_fd)
	{
		num_fd = IO_ARRAYLEN; /* why do it a zillion times? */
		if (num_fd > FD_SETSIZE)
			num_fd = FD_SETSIZE;
	}
	
	if (timeout)
	{
		newtimeout = &thetimeout;
		bcopy(timeout, newtimeout, sizeof(struct timeval));
	}
	else
		newtimeout = NULL;

	init_io();
	FD_ZERO(&new);
	for (i = 0; i < num_fd; i++)
	{
		if (i > max_fd && ((rd && FD_ISSET(i, rd)) || (wd && FD_ISSET(i, wd))))
			max_fd = i;
		if (io_rec[i])
		{
			if (io_rec[i]->read_pos < io_rec[i]->write_pos)
			{
				FD_SET(i, &new);
				set = 1;
			}
		}
	}
	if (set)
	{
		set = 0;
		if (select(max_fd + 1, rd, wd, NULL, &right_away) <= 0)
			FD_ZERO(rd);
		for (i = 0; i < num_fd; i++)
		{
			if ((FD_ISSET(i, rd)) || (FD_ISSET(i, &new)))
			{
				set++;
				FD_SET(i, rd);
			}
			else
				FD_CLR(i, rd);
		}
		return (set);
	}
	return (select(max_fd + 1, rd, wd, NULL, newtimeout));
}

/* new_close: works just like close */
void new_close(int des)
{
	if (des < 0 || !io_rec)
		return;
#ifndef WINNT
	new_free((char **)&(io_rec[des]));
	close(des);
#else
	FindAndFree(des);
#endif
}

/* set's socket options */
extern	void
set_socket_options(s)
	int	s;
{
	int	opt = 1;
	int	optlen = sizeof(opt);
#ifndef NO_STRUCT_LINGER
	struct linger	lin;
	lin.l_onoff = lin.l_linger = 0;
	(void) setsockopt(s, SOL_SOCKET, SO_LINGER, (void *) &lin, optlen);
#endif /* NO_STRUCT_LINGER */
#ifdef WINNT
	mark_socket(s);
#endif
	(void) setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (void *) &opt, optlen);
	opt = 1;
	(void) setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, (void *) &opt, optlen);
}

#ifdef WINNT

#define xmalloc(s) HeapAlloc(GetProcessHeap(),HEAP_ZERO_MEMORY,(s))
#define xfree(p) HeapFree(GetProcessHeap(),0,(p))
#include <errno.h>

void mark_socket(int des) {
	MyIO *ptr = FindOrNew(des);
	ptr->issock = 1;
	ptr->buffer = xmalloc(8*IO_BUFFER_SIZE);
}
int check_bufs(int des) {
	MyIO *ptr = FindMyIo(des);
	if (!ptr)
		return 1;
	
	if (ptr->read_pos < ptr->write_pos){
		return 1;
	}
	//
	// apparently win95 never returns 109 in peeknamedpipe
	// -amol 10/5/96
	//
	if (gdwPlatform == VER_PLATFORM_WIN32_WINDOWS) {
		if (ptr->read_pos == ptr->write_pos){
			return 1;
		}
	}
	return 0;
}

MyIO *FindOrNew(int sockhandle){

	MyIO *ptr = io_rec;

	if (io_rec == NULL) {
		io_rec = xmalloc(sizeof(MyIO));
		if (!io_rec) {
			errno = ENOMEM;
			return NULL;
		}
		io_rec->read_pos = 0;
		io_rec->write_pos = 0;
		io_rec->misc_flags = 0;
		io_rec->issock = 0;
		io_rec->buffer_pending = 0;
		io_rec->sockhandle = sockhandle;
		io_rec->next = NULL;
		io_rec->buffer = &(io_rec->buffer_io[0]);
		tail = io_rec;
		return io_rec;
	}
	if (ptr->sockhandle == sockhandle)
		return ptr;
	while(ptr->next != NULL) {
		ptr = ptr->next;
		if (ptr->sockhandle == sockhandle)
			return ptr;
	}
	if (tail != ptr) {
		fprintf(stderr,"Fatal\007\007! link corruption %s %d\n",__FILE__,
		__LINE__);
		return NULL;
	}

	ptr = xmalloc(sizeof(MyIO));
	ptr->read_pos = 0;
	ptr->write_pos = 0;
	ptr->misc_flags = 0;
	ptr->sockhandle = sockhandle;
	ptr->buffer = &(ptr->buffer_io[0]);
	ptr->next = NULL;

	tail->next = ptr;
	tail = tail->next;

	return ptr;


}
MyIO* FindMyIo(int sockhandle){

	MyIO *ptr = io_rec;

	while(ptr && ptr->sockhandle != sockhandle) {
		ptr = ptr->next;	
	}
	return ptr;
}
void FindAndFree(int sockhandle){

	MyIO *ptr = io_rec;
	MyIO *ptr2 = io_rec;

	if (!ptr)
		return;
	
	// Found socket in first element of list..
	if (ptr->sockhandle == sockhandle) {
		io_rec = ptr->next;
		if (tail == ptr)
			tail = io_rec;
		if (ptr->issock ){
			closesocket(sockhandle);
			xfree(ptr->buffer);
		}
		else
			CloseHandle((HANDLE)sockhandle);
		xfree(ptr);
		return ;
	}
	while(ptr && ptr->sockhandle != sockhandle) {
		ptr2 = ptr;
		ptr = ptr->next;
	}
	if (!ptr)
		return;


	ptr2->next = ptr->next;
	if (tail == ptr)
		tail = ptr2;
		
		/* Added 9/23/96 -amol*/
	if (ptr->issock ){
		closesocket(sockhandle);
		xfree(ptr->buffer);
	}
	else
		CloseHandle((HANDLE)sockhandle);
	xfree(ptr);

}
int FindAndFreeWithReturnCode(int sockhandle){

	MyIO *ptr = io_rec;
	MyIO *ptr2 = io_rec;

	if (!ptr)
		return 1;
	
	// Found socket in first element of list..
	if (ptr->sockhandle == sockhandle) {
		io_rec = ptr->next;
		if (tail == ptr)
			tail = io_rec;
		if (ptr->issock ){
			closesocket(sockhandle);
			xfree(ptr->buffer);
		}
		else
			CloseHandle((HANDLE)sockhandle);
		xfree(ptr);
		return 0;
	}
	while(ptr && ptr->sockhandle != sockhandle) {
		ptr2 = ptr;
		ptr = ptr->next;
	}
	if (!ptr)
		return 1;


	ptr2->next = ptr->next;
	if (tail == ptr)
		tail = ptr2;
		
		/* Added 9/23/96 -amol*/
	if (ptr->issock ){
		closesocket(sockhandle);
		xfree(ptr->buffer);
	}
	else
		CloseHandle((HANDLE)sockhandle);
	xfree(ptr);

	return 0;

}
#endif WINNT
