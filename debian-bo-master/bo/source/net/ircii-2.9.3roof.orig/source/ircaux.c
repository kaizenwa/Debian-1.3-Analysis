/*
 * ircaux.c: some extra routines... not specific to irc... that I needed 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990, 1991 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: ircaux.c,v 1.42 1996/01/31 22:50:21 glen Exp $";
#endif

#include "irc.h"

#if defined(ESIX) || defined(MIPS_SYSV)
# define _TERMIOS_INCLUDED
# define _INCLUDE_TERMIO
# include <sys/termio.h>
#endif

#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif /* HAVE_SYS_UN_H */

#include <pwd.h>

#include "ircaux.h"
#include "output.h"
#include "ircterm.h"
#include "newio.h"

extern	struct	in_addr	MyHostAddr;

#ifdef	ALLOC_DEBUG
# ifdef  _IBMR2
struct	HeapDesc
{
	u_long	 Link;
	u_long	 Size;
};
# endif
#define	ALLOC_LIST 2048
static	unsigned char	*MemList[ALLOC_LIST];
static	long	MemSize[ALLOC_LIST];
static	int	Init = 0;

static	void	dump_mem _((void));

static	void
dump_mem()
{
	int	i;
	FILE	*fp;
#ifdef _IBMR2
	struct	HeapDesc *HeapElement;
	char	*lowest;
	char	*highest;
	long	size;
#endif
	int	fits;

	fp = fopen("debug.log", "w");
	fprintf(fp, "ircII failed because of a segmentation violation\nKnown allocations follow:\n\n");
	for (i = 0; i < ALLOC_LIST; i++)
		if (MemList[i])
		{
#ifdef _IBMR2
	/*
	 * The following determines if the size shown for this element of
	 * memory matches the size we have recorded in the allocation list.
	 * this is very much machine dependant, and could even vary between
	 * SysV and BSD on the same machine.
	 */
			size = (0x08 << (*((long *) MemList[i] - 1)));
			if (size - 0x08 >= MemSize[i] && (size >> 1) - 0x08<MemSize[i])
				fits = 1;
			else
				fits = 0;
#else
			fits = 1;
#endif
			fprintf(fp, "     %08lx  %08lx  %08lx  %08lx %s\n",
			    (long) MemList[i], MemSize[i],
			    (long) *((long *) MemList[i]-2),
			    *((long *) MemList[i]-1), fits ? "" : "SUSPICIOUS");
		}
#ifdef _IBMR2
	/*
	 * Now we'll walk the heap. We do this by finding the lowest and
         * highest elements in our list, and use the HeapDesc structure
	 * to find our way around.
	 */
	highest = NULL;
	lowest = NULL;
	for (i = 0; i < ALLOC_LIST; i++)
	{
		if (!MemList[i])
			continue;
		if (!lowest)
			lowest = MemList[i];
		if ((u_long) MemList[i] < (u_long) lowest)
			lowest = MemList[i];
		if ((u_long) MemList[i] > (u_long) highest)
			highest = MemList[i];
	}
	fprintf(fp, "\nKnown allocations start at %08x and end at %08x\n",
	    lowest, highest);
	fprintf(fp, "\nHeap walks as follows:\n\n");
	for (HeapElement = lowest-0x08; HeapElement<highest;
	    HeapElement = (HeapElement->Link == 0xefL || !HeapElement->Link)?
	    (HeapElement+(0x01<<HeapElement->Size)):
	    ((struct	HeapDesc *) HeapElement->Link))
	{
		fprintf(fp, "    %08x %08x %08x\n",
		    HeapElement + 1,
		    HeapElement->Link,
		    HeapElement->Size);
	}
#endif
	fclose(fp);
	fprintf(stderr, "Segmentation violation. Debug information saved to debug.log\n");

	/*
	 * If we resume on a segmentation violation, hopefully it repeats the
	 * violation and we get both our log and the core dump from the point
	 * at which things fail.
	 */
	(void) MY_SIGNAL(SIGSEGV, (sigfunc *)SIG_DFL, 0);
	return;
}
#endif

/*
 * new_free:  Why do this?  Why not?  Saves me a bit of trouble here and
 * there 
 */
void
new_free(iptr)
	void	*iptr;
{
	void	**ptr = (void **) iptr;
#ifdef ALLOC_DEBUG
	FILE	*fp;
	int	i;
#endif
#ifdef DO_USER2
	int	oldmask;
#endif /* DO_USER2 */

	if (*ptr)
	{
#ifdef DO_USER2
		oldmask = sigblock(sigmask(SIGUSR2));
#endif /* DO_USER2 */
#ifdef FREE_DEBUG
		if (free(*ptr) < 0)
			put_it("*debug* free failed '%s'", (char *) ptr);
#else
		free(*ptr);
#endif
#ifdef DO_USER2
		sigblock(oldmask);
#endif /* DO_USER2 */
#ifdef ALLOC_DEBUG
		for (i = 0; i < ALLOC_LIST; i++)
		{
			if ((void *) MemList[i] == *ptr)
				break;
		}
		if (i == ALLOC_LIST)
		{
			fprintf(stderr,
				"Memory freed that was never allocated\n");
			fp=fopen("debug.log", "w");
			fprintf(fp, "failed by freeing %08lx\n", (long) *ptr);
			fprintf(fp, "List is as follows:\n");
			for (i = 0; i < ALLOC_LIST; i++)
				if (MemList[i])
					fprintf(fp, "    %08lx  %08lx\n",
						(long) MemList[i], MemSize[i]);
			fclose(fp);
			abort();
		}
		MemList[i] = (void *) 0;
		MemSize[i] = 0L;
#endif
		*ptr = (void *) 0;
	}
}

#define WAIT_BUFFER 2048
static char *pointers[WAIT_BUFFER], **current = pointers;

/*
 * wait_new_free: same as new_free() except that free() is postponed.
 */
void
wait_new_free(ptr)
	char	**ptr;
{
	if (*current)
		new_free(current);
	*current++ = *ptr;
	if (current >= pointers + WAIT_BUFFER)
		current = pointers;
	*ptr = (char *) 0;
}

/*
 * reall_free: really free the data if level == 0
 */
void
really_free(level)
	int	level;
{
	if (level != 0)
		return;
	for (current = pointers; current < pointers + WAIT_BUFFER; current++)
		if (*current)
			new_free(current);
	current = pointers;
}

char	*
new_realloc(ptr, size)
	char	*ptr;
	int	size;
{
	char	*new_ptr;
#ifdef ALLOC_DEBUG
	int	i;
#endif

	if ((new_ptr = (char *) realloc(ptr, size)) == (char *) 0)
	{
		fprintf(stderr, "realloc failed (%d): %s\nIrc Aborted!\n",
			size, strerror(errno));
		exit(1);
	}
#ifdef ALLOC_DEBUG
	for (i = 0;i < ALLOC_LIST; i++)
		if ((char *) MemList[i] == ptr)
			break;
	if (i == ALLOC_LIST)
	{
		fprintf(stderr, "Memory freed that was never allocated");
		abort();
	}
	MemList[i] = new_ptr;
	MemSize[i] = size;
#endif
	return (new_ptr);
}

char	*
new_malloc(size)
	int	size;
{
	char	*ptr;

#ifdef	ALLOC_DEBUG
	int	i;

	if (!Init)
	{
		Init = 1;
		for (i = 0; i < ALLOC_LIST; i++)
		{
			MemList[i] = (void	*) 0;
			MemSize[i] = 0L;
		}
		if (getenv("DEBUG"))
			(void) MY_SIGNAL(SIGSEGV, dump_mem, 0);
	}
#endif
	if ((ptr = (char *) malloc(size)) == (char *) 0)
	{
		static	char	error[] = "Malloc failed: \nIrc Aborted!\n";

		write(2, error, strlen(error));
		write(2, strerror(errno), strlen(strerror(errno)));
		term_reset();
		exit(1);
	}
#ifdef ALLOC_DEBUG
	for (i = 0; i < ALLOC_LIST && MemList[i]; i++)
		;
	if (i == ALLOC_LIST)
	{
		FILE	*fp;
		int	j;

		fprintf(stderr,
		    "Out of space in memory record. Probable memory leak\n");
		fp = fopen("debug.log", "w");
		for (i = 0; i < ALLOC_LIST; i++)
		{
			fprintf(fp, "    %08lx %08lx \"",
				(long) MemList[i], MemSize[i]);
			for (j = 0; j < MemSize[i] && j < 45; j++)
			{
				if (MemList[i][j] < 32 || MemList[i][j] > 127)
					putc('.', fp);
				else
					putc(MemList[i][j], fp);
			}
			fprintf(fp, "\"\n");
		}
		fclose(fp);
		abort();
	}
	MemList[i]=ptr;
	MemSize[i]=size;
#endif
	return (ptr);
}

#ifdef ALLOC_DEBUG
void
alloc_cmd(command, args, subargs)
	char    *command,
		*args,
		*subargs;
{
	char	*arg;
	int	f_count = 0,
		f_dump = 0;
	int	i, j;
	int	count;
	long	size;
	FILE	*fp;

	while ((arg = next_arg(args, &args)))
	{
		while (*arg)
		{
			switch(*arg++)
			{
			case 'c':
			case 'C':
				f_count = 1;
				break;
			case 'd':
			case 'D':
				f_dump = 1;
				break;
			}
		}
	}
	if (f_dump)
		fp = fopen("debug.log", "w");
	else
		fp = NULL;
	for (size = count = i = 0; i < ALLOC_LIST; i++)
	{
		if (fp && MemList[i])
		{
			fprintf(fp, "    %08lx %08lx \"",
				(long) MemList[i], MemSize[i]);
			for (j = 0; j < MemSize[i] && j < 45; j++)
			{
				if (MemList[i][j] < 32 || MemList[i][j] > 127)
					putc('.', fp);
				else
					putc(MemList[i][j], fp);
			}
			fprintf(fp, "\"\n");
		}
		if (MemList[i])
		{
			count++;
			size += MemSize[i];
		}
	}
	if (fp)
		fclose(fp);
	if (f_count)
	{
		say("%d blocks allocated out of %d", count, ALLOC_LIST);
		say("%ld bytes allocated, an average of %ld per block",
				size, size/count);
	}
}
#endif

/*
 * malloc_strcpy:  Mallocs enough space for src to be copied in to where
 * ptr points to.
 *
 * Never call this with ptr pointing to an uninitialised string, as the
 * call to new_free() might crash the client... - phone, jan, 1993.
 */
void
malloc_strcpy(ptr, src)
	char	**ptr;
	char	*src;
{
	new_free(ptr);
	if (src)
	{
		*ptr = new_malloc(strlen(src) + 1);
		strcpy(*ptr, src);
	}
	else
		*ptr = (char *) 0;
}

/* malloc_strcat: Yeah, right */
void
malloc_strcat(ptr, src)
	char	**ptr;
	char	*src;
{
	char	*new;

	if (*ptr)
	{
		new = (char *) new_malloc(strlen(*ptr) + strlen(src) + 1);
		strcpy(new, *ptr);
		strcat(new, src);
		new_free(ptr);
		*ptr = new;
	}
	else
		malloc_strcpy(ptr, src);
}

char	*
upper(s)
	char	*s;
{
	char	*t = (char *) 0;

	if (s)
		for (t = s; *s; s++)
			if (islower(*s))
				*s = toupper(*s);
	return (t);
}

char *
lower(s)
	char *	s;
{
	char *	t = (char *) 0;

	if (s)
		for (t = s; *s; s++)
			if (isupper(*s))
				*s = tolower(*s);
	return t;
}

#if 0
/* case insensitive string searching */
char    *
stristr(source, search)
	char    *source,
	        *search;
{
        char    *u,
                *s = (char *) 0,
                *t = (char *) 0,
		*where;
        int     x = 0,
                len = strlen(search);

        if (!source || !*source || !search || !*search)
                return (char *) 0;

        malloc_strcpy(&s, source);
        malloc_strcpy(&t, search);
        upper(s);
        upper(t);

        u = s;
        while (u[x])
        {
                if (u[x] == t[x])
                {
			where = u;
                        if (++x >= len)
                        {
                                new_free(&s);
                                new_free(&t);
                                return (where);
                        }
                }
                else
                {
                        x = 0;
                        u++;
                }
        }
        new_free(&t);
        new_free(&s);
        return (char *) 0;
}
/* case insensitive string searching from the end */
char    *
rstristr(source,search)
	char    *source,
	        *search;
{
        char    *u,
                *s = (char *) 0,
                *t = (char *) 0,
		*where;
        int     x = 0,
                len = strlen(search),
                slen = strlen(source);

        if (!source || !*source || !search || !*search)
                return empty_string;

        malloc_strcpy(&s, source);
        malloc_strcpy(&t, search);
        upper(s);
        upper(t);

        u = s + slen - len;
        while (u >= s)
        {
                if (u[x] == t[x])
                {
                        where = u;
                        x++;
                        if (x >= len)
                        {
                                new_free(&t);
                                new_free(&s);
                                return (where);
                        }
                }
                else
                {
                        x = 0;
			u--;
                }
        }
        new_free(&t);
        new_free(&s);
        return (char *) 0;
}
#endif

/*
 * Connect_By_Number Performs a connecting to socket 'service' on host
 * 'host'.  Host can be a hostname or ip-address.  If 'host' is null, the
 * local host is assumed.   The parameter full_hostname will, on return,
 * contain the expanded hostname (if possible).  Note that full_hostname is a
 * pointer to a char *, and is allocated by connect_by_numbers() 
 *
 * The following special values for service exist:
 *
 * 0  Create a socket for accepting connections
 *
 * -1 Create a UDP socket
 *
 * -2 Connect to the address passed in place of the hostname parameter
 *
 * Errors: 
 *
 * -1 get service failed 
 *
 * -2 get host failed 
 *
 * -3 socket call failed 
 *
 * -4 connect call failed 
 */
int
connect_by_number(service, host, nonblocking)
	int	service;
	char	*host;
	int	nonblocking;
{
	int	s = -1;
	char	buf[100];
	struct	sockaddr_in server;
	struct	hostent *hp;

	if (service == -2)
	{
		server = (*(struct sockaddr_in *) host);
	}
	else if (service > 0)
	{
		if (host == (char *) 0)
		{
			gethostname(buf, sizeof(buf));
			host = buf;
		}
		if ((server.sin_addr.s_addr = inet_addr(host)) == -1)
		{
			if ((hp = gethostbyname(host)) != NULL)
			{
				bzero((char *) &server, sizeof(server));
				bcopy(hp->h_addr, (char *) &server.sin_addr,
					hp->h_length);
				server.sin_family = hp->h_addrtype;
			}
			else
				return (-2);
		}
		else
			server.sin_family = AF_INET;
		server.sin_port = (unsigned short) htons(service);
	}
	if (((service == -1) && ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0)) ||
	    ((service != -1) && ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)))
		return (-3);
	if (service != -1)
		set_socket_options(s);
	if (service <= 0 && service != -2)
	{
		struct	sockaddr_in localaddr;

		bzero(&localaddr, sizeof(struct	sockaddr_in));
		localaddr.sin_family = AF_INET;
		if (!service)
			localaddr.sin_addr.s_addr = INADDR_ANY;
		else
			localaddr.sin_addr = MyHostAddr;
		localaddr.sin_port = 0;
		if (bind(s, (struct sockaddr *) &localaddr,
			sizeof(localaddr)) == -1 ||
			(!service && listen(s, 1) == -1))
		{
			new_close(s);
			return -4;
		}
		service = sizeof(localaddr);
		getsockname(s, (struct	sockaddr *) &localaddr, &service);
		return (s);
	}
#if defined(PRIV_PORT) || defined(PRIV_PORT_ULC)
#ifdef PRIV_PORT_ULC
	seteuid(0);
#endif
	/* attempt to bind to a privileged port */
	if (geteuid() == 0)
	{
		struct	sockaddr_in localaddr;
		int	portnum;

		localaddr = server;
		localaddr.sin_addr.s_addr=INADDR_ANY;
		for (portnum = 1023; portnum > 600; portnum--)
		{
			localaddr.sin_port = htons(portnum);
			if (bind(s, (struct sockaddr *) &localaddr,
					sizeof(localaddr)) != -1)
				break;
		}
	}
#ifdef PRIV_PORT_ULC
	seteuid(getuid());
#endif
#endif /*PRIV_PORT*/
#ifdef NON_BLOCKING_CONNECTS
	if (nonblocking && set_non_blocking(s) < 0)
	{
#ifdef ESIX
		t_close(s);
		unmark_socket(s);
#endif /* ESIX */
		new_close(s);
		return -4;
	}
#endif /* NON_BLOCKING_CONNECTS */
	if (connect(s, (struct sockaddr *) &server, sizeof(server)) < 0)
	{
		if (!(errno == EINPROGRESS && nonblocking))
		{
#ifdef ESIX
			t_close(s);
			unmark_socket(s);
#endif /* ESIX */
			new_close(s);
			return -4;
		}
	}
	return s;
}

char	*
next_arg(str, new_ptr)
	char	*str,
		**new_ptr;
{
	char	*ptr;

	if ((ptr = sindex(str, "^ ")) != NULL)
	{
		if ((str = sindex(ptr, " ")) != NULL)
			*str++ = (char) 0;
		else
			str = empty_string;
	}
	else
		str = empty_string;
	if (new_ptr)
		*new_ptr = str;
	return ptr;
}

char	*
new_next_arg(str, new_ptr)
	char	*str,
		**new_ptr;
{
	char	*ptr,
		*start;

	if ((ptr = sindex(str, "^ \t")) != NULL)
	{
		if (*ptr == '"')
		{
			start = ++ptr;
			while ((str = sindex(ptr, "\"\\")) != NULL)
			{
				switch (*str)
				{
				case '"':
					*str++ = '\0';
					if (*str == ' ')
						str++;
					if (new_ptr)
						*new_ptr = str;
					return (start);
				case '\\':
					if (*(str + 1) == '"')
						strcpy(str, str + 1);
					ptr = str + 1;
				}
			}
			str = empty_string;
		}
		else
		{
			if ((str = sindex(ptr, " \t")) != NULL)
				*str++ = '\0';
			else
				str = empty_string;
		}
	}
	else
		str = empty_string;
	if (new_ptr)
		*new_ptr = str;
	return ptr;
}

/* my_stricmp: case insensitive version of strcmp */
int
my_stricmp(str1, str2)
	char	*str1,
		*str2;
{
	int	xor;

	if (!str1) return -1;
	if (!str2) return 1; 
	for (; *str1 || *str2 ; str1++, str2++)
	{
		if (!*str1 || !*str2)
			return (*str1 - *str2);
		if (isalpha(*str1) && isalpha(*str2))
		{
			xor = *str1 ^ *str2;
			if (xor != 32 && xor != 0)
				return (*str1 - *str2);
		}
		else
		{
			if (*str1 != *str2)
				return (*str1 - *str2);
		}
	}
	return 0;
}

/* my_strnicmp: case insensitive version of strncmp */
int	
my_strnicmp(str1, str2, n)
	char	*str1,
		*str2;
	int	n;
{
	int	i,
		xor;

	for (i = 0; i < n; i++, str1++, str2++)
	{
		if (isalpha(*str1) && isalpha(*str2))
		{
			xor = *str1 ^ *str2;
			if (xor != 32 && xor != 0)
				return (*str1 - *str2);
		}
		else
		{
			if (*str1 != *str2)
				return (*str1 - *str2);
		}
	}
	return 0;
}

/*
 * strmcpy: Well, it's like this, strncpy doesn't append a trailing null if
 * strlen(str) == maxlen.  strmcpy always makes sure there is a trailing null 
 */
void
strmcpy(dest, src, maxlen)
	char	*dest,
		*src;
	int	maxlen;
{
	strncpy(dest, src, maxlen);
	dest[maxlen] = '\0';
}

/*
 * strmcat: like strcat, but truncs the dest string to maxlen (thus the dest
 * should be able to handle maxlen+1 (for the null)) 
 */
void
strmcat(dest, src, maxlen)
	char	*dest,
		*src;
	int	maxlen;
{
	int	srclen,
	len;

	srclen = strlen(src);
	if ((len = strlen(dest) + srclen) > maxlen)
		strncat(dest, src, srclen - (len - maxlen));
	else
		strcat(dest, src);
}

/*
 * strmcat_ue: like strcat, but truncs the dest string to maxlen (thus the dest
 * should be able to handle maxlen + 1 (for the null)). Also unescapes
 * backslashes.
 */
void
strmcat_ue(dest, src, maxlen)
	char	*dest,
		*src;
	int	maxlen;
{
	int	dstlen;

	dstlen = strlen(dest);
	dest += dstlen;
	maxlen -= dstlen;
	while (*src && maxlen > 0)
	{
		if (*src == '\\')
		{
			if (index("npr0", src[1]))
				*dest++ = '\020';
			else if (*(src + 1))
				*dest++ = *++src;
			else
				*dest++ = '\\';
		}
		else
			*dest++ = *src;
		src++;
	}
	*dest = '\0';
}

/*
 * scanstr: looks for an occurrence of str in source.  If not found, returns
 * 0.  If it is found, returns the position in source (1 being the first
 * position).  Not the best way to handle this, but what the hell 
 */
extern	int
scanstr(source, str)
	char	*str,
		*source;
{
	int	i,
		max,
		len;

	len = strlen(str);
	max = strlen(source) - len;
	for (i = 0; i <= max; i++, source++)
	{
		if (!my_strnicmp(source, str, len))
			return (i + 1);
	}
	return (0);
}

/* expand_twiddle: expands ~ in pathnames. */
char	*
expand_twiddle(str)
	char	*str;
{
	char	buffer[BIG_BUFFER_SIZE + 1];

	if (*str == '~')
	{
		str++;
		if (*str == '/' || *str == '\0')
		{
			strmcpy(buffer, my_path, BIG_BUFFER_SIZE);
			strmcat(buffer, str, BIG_BUFFER_SIZE);
		}
		else
		{
			char	*rest;
			struct	passwd *entry;

			if ((rest = index(str, '/')) != NULL)
				*rest++ = '\0';
			if ((entry = getpwnam(str)) != NULL)
			{
				strmcpy(buffer, entry->pw_dir, BIG_BUFFER_SIZE);
				if (rest)
				{
					strmcat(buffer, "/", BIG_BUFFER_SIZE);
					strmcat(buffer, rest, BIG_BUFFER_SIZE);
				}
			}
			else
				return (char *) NULL;
		}
	}
	else
		strmcpy(buffer, str, BIG_BUFFER_SIZE);
	str = '\0';
	malloc_strcpy(&str, buffer);
	return (str);
}

/* islegal: true if c is a legal nickname char anywhere but first char */
#define islegal(c) ((((c) >= 'A') && ((c) <= '}')) || \
		    (((c) >= '0') && ((c) <= '9')) || \
		     ((c) == '-') || ((c) == '_'))

/*
 * check_nickname: checks is a nickname is legal.  If the first character is
 * bad new, null is returned.  If the first character is bad, the string is
 * truncd down to only legal characters and returned 
 *
 * rewritten, with help from do_nick_name() from the server code (2.8.5),
 * phone, april 1993.
 */
char	*
check_nickname(nick)
	char	*nick;
{
	char	*s;

	if (!nick || *nick == '-' || isdigit(*nick))
		return NULL;

	for (s = nick; *s && (s - nick) < NICKNAME_LEN; s++)
		if (!islegal(*s) || isspace(*s))
			break;
	*s = '\0';

	return *nick ? nick : NULL;
}

/*
 * sindex: much like index(), but it looks for a match of any character in
 * the group, and returns that position.  If the first character is a ^, then
 * this will match the first occurence not in that group.
 */
char	*
sindex(string, group)
	char	*string,
		*group;
{
	char	*ptr;

	if (!string || !group)
		return (char *) NULL;
	if (*group == '^')
	{
		group++;
		for (; *string; string++)
		{
			for (ptr = group; *ptr; ptr++)
			{
				if (*ptr == *string)
					break;
			}
			if (*ptr == '\0')
				return string;
		}
	}
	else
	{
		for (; *string; string++)
		{
			for (ptr = group; *ptr; ptr++)
			{
				if (*ptr == *string)
					return string;
			}
		}
	}
	return (char *) NULL;
}

/* is_number: returns true if the given string is a number, false otherwise */
int
is_number(str)
	char	*str;
{
	while (*str == ' ')
		str++;
	if (*str == '-')
		str++;
	if (*str)
	{
		for (; *str; str++)
		{
			if (!isdigit((*str)))
				return (0);
		}
		return 1;
	}
	else
		return 0;
}

/* rfgets: exactly like fgets, cept it works backwards through a file!  */
char	*
rfgets(buffer, size, file)
	char	*buffer;
	int	size;
	FILE	*file;
{
	char	*ptr;
	off_t	pos;

	if (fseek(file, -2L, 1))
		return NULL;
	do
	{
		switch (fgetc(file))
		{
		case EOF:
			return NULL;
		case '\n':
			pos = ftell(file);
			ptr = fgets(buffer, size, file);
			fseek(file, pos, 0);
			return ptr;
		}
	}
	while (fseek(file, -2L, 1) == 0);
	rewind(file);
	pos = 0L;
	ptr = fgets(buffer, size, file);
	fseek(file, pos, 0);
	return ptr;
}

/*
 * path_search: given a file called name, this will search each element of
 * the given path to locate the file.  If found in an element of path, the
 * full path name of the file is returned in a static string.  If not, null
 * is returned.  Path is a colon separated list of directories 
 */
char	*
path_search(name, path)
	char	*name;
	char	*path;
{
	static	char	buffer[BIG_BUFFER_SIZE + 1];
	char	*ptr,
		*free_path = (char *) 0;

	malloc_strcpy(&free_path, path);
	path = free_path;
	while (path)
	{
		if ((ptr = index(path, ':')) != NULL)
			*(ptr++) = '\0';
		strcpy(buffer, empty_string);
		if (path[0] == '~')
		{
			strmcat(buffer, my_path, BIG_BUFFER_SIZE);
			path++;
		}
		strmcat(buffer, path, BIG_BUFFER_SIZE);
		strmcat(buffer, "/", BIG_BUFFER_SIZE);
		strmcat(buffer, name, BIG_BUFFER_SIZE);
		if (access(buffer, F_OK) == 0)
			break;
		path = ptr;
	}
	new_free(&free_path);
	return (path) ? buffer : (char *) 0;
}

/*
 * double_quote: Given a str of text, this will quote any character in the
 * set stuff with the QUOTE_CHAR. It returns a malloced quoted, null
 * terminated string 
 */
char	*
double_quote(str, stuff)
	char	*str;
	char	*stuff;
{
	char	buffer[BIG_BUFFER_SIZE + 1];
	char	*ptr = NULL;
	char	c;
	int	pos;

	if (str && stuff)
	{
		for (pos = 0; (c = *str); str++)
		{
			if (index(stuff, c))
			{
				if (c == '$')
					buffer[pos++] = '$';
				else
					buffer[pos++] = '\\';
			}
			buffer[pos++] = c;
		}
		buffer[pos] = '\0';
		malloc_strcpy(&ptr, buffer);
	}
	else
		malloc_strcpy(&ptr, str);
	return ptr;
}

/*
 * new_stty: given a string of stty commands sets the tty 
 *           via ioctls TCGETA/TCSETA.
 *
 * WARNING: if someone of the architectures specified in 
 *          #if statement don't work ... please comment out
 *          the relative statement and send a report to
 *   
 *          mez002@cdc835.cdc.polimi.it  or
 *          rfac@ghost.unimi.it
 *          
 *          or talk with me on IRC ... (i think is better)
 *
 *                                    - Allanon -
 *
 */
void
new_stty(option)
	char	*option;
{
#if defined(ESIX) || defined(MIPS_SYSV)
	struct	termio ttyset;

	ioctl(0, TCGETA, &ttyset);

	if (strstr(option, "opost"))
		ttyset.c_oflag |= OPOST;
	if (strstr(option, "sane"))
	{
		ttyset.c_iflag &= ~(IGNBRK | PARMRK | INPCK | INLCR | IGNCR |
			IUCLC | IXOFF);
		ttyset.c_lflag &= ~(XCASE | ECHOE | ECHONL | NOFLSH);
		ttyset.c_oflag &= ~(OLCUC | OCRNL | ONOCR | ONLRET | OFILL |
			OFDEL | NLDLY | CRDLY | TABDLY | BSDLY | VTDLY | FFDLY);
		ttyset.c_iflag |= (BRKINT | IGNPAR | ISTRIP | ICRNL | IXON);
		ttyset.c_lflag |= (ISIG | ICANON | ECHO | ECHOK);
		ttyset.c_oflag |= (OPOST | ONLCR);
		ttyset.c_cc[VERASE] = CERASE;
		ttyset.c_cc[VKILL] = CKILL;
		ttyset.c_cc[VQUIT] = CQUIT;
		ttyset.c_cc[VINTR] = CINTR;
		ttyset.c_cc[VEOF] = CEOF;
		ttyset.c_cc[VEOL] = CNUL;
		ttyset.c_cc[VSWTCH] = CNUL;
	}
	if (strstr(option, "cooked"))   /*  cooked == -raw  */
	{
		ttyset.c_cflag &= ~CSIZE;
		ttyset.c_cflag |= PARENB;
		ttyset.c_iflag |= (BRKINT | IGNPAR | ISTRIP | IXON);
		ttyset.c_oflag |= OPOST;
		ttyset.c_lflag |= (ICANON | ISIG);
		ttyset.c_cc[VEOF] = CEOF;
		ttyset.c_cc[VEOL] = CNUL;
	}
	if (strstr(option, "raw"))
	{
		ttyset.c_cflag &= ~(CSIZE | PARENB);
		ttyset.c_iflag &= ~(-1);
		ttyset.c_lflag &= ~(ISIG | ICANON | XCASE);
		ttyset.c_oflag &= ~OPOST;
		ttyset.c_cflag |= CS8;
		ttyset.c_cc[VMIN] = 1;
		ttyset.c_cc[VTIME] = 1;
	}
	if (strstr(option, "-echo"))
		ttyset.c_lflag &= ~ECHO;

	ioctl(0, TCSETAW, &ttyset);
#endif
}

#ifdef ZCAT
/* Here another interesting stuff:
 * it handle zcat of compressed files
 * You can manage compressed files in this way:
 *
 * IN: char *name, the compressed file FILENAME
 * OUT: a FILE *, from which read the expanded file
 *
 */
FILE	*
zcat(name)
	char	*name;
{
	FILE	*fp;
	int	in[2];

	in[0] = -1;
	in[1] = -1;
	if (pipe(in))
	{
		say("Unable to start decompression process: %s", strerror(errno));
		if(in[0] != -1)
		{
			new_close(in[0]);
			new_close(in[1]);
		}
		return(NULL);
	}
	switch(fork())
	{
	case -1:
		say("Unable to start decompression process: %s", strerror(errno));
		return(NULL);
	case 0:
		(void) MY_SIGNAL(SIGINT, (sigfunc *) SIG_IGN, 0);
		dup2(in[1], 1);
		new_close(in[0]);
		setuid(getuid());
		setgid(getgid());
#ifdef ZARGS
		execl(ZCAT, ZCAT, ZARGS, name, NULL);
#else
		execl(ZCAT, ZCAT, name, NULL);
#endif
		exit(0);
	default:
		new_close(in[1]);
		if ((fp = fdopen(in[0], "r")) == (FILE *) 0)
		{
			say("Cannot open pipe file descriptor: %s", strerror(errno));
			return(NULL);
		}
		break;
	}
	return(fp);
}
#endif /*ZCAT*/

#ifdef NEED_INDEX

extern	char	*
index(s, c)
	char	*s;
	char	c;
{
# ifdef HAVE_STRSTR
	return strstr(s, c);
# else
	
	int	len = strlen(s);

	for (; len > 0 && c != *s; s++, len--)
		;
	return (len) ? s : (char *) NULL;
# endif /* HAVE_STRSTD */
}

#endif /* NEED_INDEX */

#ifdef NEED_RINDEX

extern	char	*
rindex(s, c)
	char	*s;
	char	c;
{
# ifdef HAVE_STRRSTR
	return strrstr(s, c);
# else

	int	len = strlen(s);
	char	*t = s;

	s += len;
	for (; s >= t && c != *s; s--)
		;
	return (s < t) ? (char *) NULL : s;
# endif /* HAVE_STRRSTR */
}

#endif /* NEED_RINDEX */

#ifdef NON_BLOCKING_CONNECTS
int
set_non_blocking(fd)
int	fd;
{
	int	res, nonb = 0;

#if defined(NBLOCK_POSIX)
	nonb |= O_NONBLOCK;
#else
# if defined(NBLOCK_BSD)
	nonb |= O_NDELAY;
# else
#  if defined(NBLOCK_SYSV)
	res = 1;

	if (ioctl (fd, FIONBIO, &res) < 0)
		return -1;
#  else
#   error no idea how to set an fd to non-blocking 
#  endif
# endif
#endif
#if (defined(NBLOCK_POSIX) || defined(NBLOCK_BSD)) && !defined(NBLOCK_SYSV)
	if ((res = fcntl(fd, F_GETFL, 0)) == -1)
		return -1;
	else if (fcntl(fd, F_SETFL, res | nonb) == -1)
		return -1;
#endif
	return 0;
}

int
set_blocking(fd)
int	fd;
{
	int	res, nonb = 0;

#if defined(NBLOCK_POSIX)
	nonb |= O_NONBLOCK;
#else
# if defined(NBLOCK_BSD)
	nonb |= O_NDELAY;
# else
#  if defined(NBLOCK_SYSV)
	res = 0;

	if (ioctl (fd, FIONBIO, &res) < 0)
		return -1;
#  else
#   error no idea how to return an fd blocking 
#  endif
# endif
#endif
#if (defined(NBLOCK_POSIX) || defined(NBLOCK_BSD)) && !defined(NBLOCK_SYSV)
	if ((res = fcntl(fd, F_GETFL, 0)) == -1)
		return -1;
	else if (fcntl(fd, F_SETFL, res &~ nonb) == -1)
		return -1;
#endif
	return 0;
}
#endif
