/*
 * ircaux.c: some extra routines... not specific to irc... that I needed 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990, 1991 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#include "irc.h"
#include "alias.h"
#include "log.h"
#include "misc.h"
#include "vars.h"
#include "screen.h"

#ifndef WINNT
#include <pwd.h>
#else
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define malloc(s) HeapAlloc(GetProcessHeap(),HEAP_ZERO_MEMORY,(s))
#define free(s) HeapFree(GetProcessHeap(),0,(s))
#define realloc(p,s) HeapReAlloc(GetProcessHeap(),0,(p),(s))

#ifdef NTDBG
unsigned long ticksb4,ticks_after,tickstotal;
unsigned long mall_hits;
#endif NTDBG
#endif WINNT


#include <sys/stat.h>

#include "ircaux.h"
#include "output.h"
#include "ircterm.h"


/*
 * These are used by the malloc routines.  We actually ask for an int-size
 * more of memory, and in that extra int we store the malloc size.  Very
 * handy for debugging and other hackeries.
 */

#define alloc_start(ptr) ((ptr) - sizeof(void *) - sizeof(void *))
#define alloc_size(ptr) (*(int *)( alloc_start((ptr)) + sizeof(void *)))
#define alloc_magic(ptr) (*(int *)( alloc_start((ptr)) ))
#define FREED_VAL -3
#define ALLOC_MAGIC 0xafbdce70

/*
 * Malloc allocator with size caching.
 */
char	*n_malloc (size_t size, char *file, int line)
{
	char	*ptr;

	if (!(ptr = (char *)calloc(1, size+sizeof(void *)+sizeof(void *))))
	{
		yell("Malloc() failed, giving up!");
		putlog(LOG_ALL, "*", "*** failed calloc %s (%d)", file, line);
		term_reset();
		exit(1);
	}

	/* Store the size of the allocation in the buffer. */
	ptr += sizeof(void *) + sizeof(void *);
	alloc_magic(ptr) = ALLOC_MAGIC;
	alloc_size(ptr) = size;
	return ptr;
}

/*
 * new_free:  Why do this?  Why not?  Saves me a bit of trouble here and there 
 */
char *	n_free(char **ptr, char *file, int line)
{
	if (*ptr)
	{
#ifdef FREE_DEBUG
		if (alloc_magic(*ptr) != ALLOC_MAGIC)
			abort();

		/* Check to make sure its not been freed before */
		if (alloc_size(*ptr) == FREED_VAL)
			abort();
#endif
		alloc_size(*ptr) = FREED_VAL;
		free((void *)alloc_start(*ptr));
		*ptr = NULL;
	}
	return (*ptr);
}


char	*n_realloc (char *ptr, size_t size, char *file, int line)
{
	char *ptr2 = NULL;

	if (ptr)
	{
		if (size)
		{
			size_t msize = alloc_size(ptr);

			if (msize >= size)
				return ptr;

			ptr2 = new_malloc(size);
			bcopy(ptr, ptr2, msize);
		}
		new_free(&ptr);
	} 
	else if (size)
		ptr2 = new_malloc(size);
	
	return ptr2;
}

#define WAIT_BUFFER 128
static char *pointers[WAIT_BUFFER], **current = pointers;

/*
 * wait_new_free: same as new_free() except that free() is postponed.
 */
void	wait_new_free (char **ptr)
{
	if (*current)
		new_free(current);
	*current++ = *ptr;
	if (current >= pointers + WAIT_BUFFER)
		current = pointers;
	*ptr = NULL;
}

/*
 * really_free: really free the data if level == 0
 */
void	really_free (int level)
{
	if (level != 0)
		return;
	for (current = pointers; current < pointers + WAIT_BUFFER; current++)
		if (*current)
			new_free(current);
	current = pointers;
}

/*
 * malloc_strcpy:  Mallocs enough space for src to be copied in to where
 * ptr points to.
 *
 * Never call this with ptr pointinng to an uninitialised string, as the
 * call to new_free() might crash the client... - phone, jan, 1993.
 */
char *	malloc_strcpy (char **ptr, char *src)
{
	if (!src)
		return new_free(ptr);
	if (ptr && *ptr)
	{
		if (alloc_size(*ptr) > strlen(src))
			return strcpy(*ptr, src);
		new_free(ptr);
	}
	*ptr = new_malloc(strlen(src) + 1);
	return strcpy(*ptr, src);
	return *ptr;
}

/* malloc_strcat: Yeah, right */
char *	malloc_strcat (char **ptr, char *src)
{
	size_t  msize;

	if (*ptr)
	{
		if (!src)
			return *ptr;
		msize = strlen(*ptr) + strlen(src) + 1;
		*ptr = new_realloc(*ptr, msize); 
		return strcat(*ptr, src);
	}
	return (*ptr = m_strdup(src));
}

char *m_3dup (const char *str1, const char *str2, const char *str3)
{
	size_t msize = strlen(str1) + strlen(str2) + strlen(str3) + 1;
	return strcat(strcat(strcpy((char *)new_malloc(msize), str1), str2), str3);
}

char *m_opendup (const char *str1, ...)
{
	va_list args;
	int size;
	char *this_arg = NULL;
	char *retval = NULL;

	size = strlen(str1);
	va_start(args, str1);
	while ((this_arg = va_arg(args, char *)))
		size += strlen(this_arg);

	retval = (char *)new_malloc(size + 1);

	strcpy(retval, str1);
	va_start(args, str1);
	while ((this_arg = va_arg(args, char *)))
		strcat(retval, this_arg);

	va_end(args);
	return retval;
}

char	*m_strdup (const char *str)
{
	char *ptr;
	
	if (!str)
		str = empty_string;
	ptr = (char *)new_malloc(strlen(str) + 1);
	return strcpy(ptr, str);
}

char	*m_s3cat (char **one, char *maybe, char *definitely)
{
	if (*one && **one)
		return m_3cat(one, maybe, definitely);
	return *one = m_strdup(definitely);
}

char *m_s3cat_s (char **one, char *maybe, char *ifthere)
{
	if (ifthere && *ifthere)
		return m_3cat(one, maybe, ifthere);
	return *one;
}

char	*m_3cat(char **one, char *two, char *three)
{
	int len = 0;
	char *str;

	if (*one)
		len = strlen(*one);
	if (two)
		len += strlen(two);
	if (three)
		len += strlen(three);
	len += 1;

	str = (char *)new_malloc(len);
	if (*one)
		strcpy(str, *one);
	if (two)
		strcat(str, two);
	if (three)
		strcat(str, three);

	new_free(one);
	return ((*one = str));
}

char	*upper (char *str)
{
	char	*ptr = NULL;

	if (str)
	{
		ptr = str;
		for (; *str; str++)
		{
			if (islower(*str))
				*str = toupper(*str);
		}
	}
	return (ptr);
}

char	*lower (char *str)
{
	char	*ptr = NULL;

	if (str)
	{
		ptr = str;
		for (; *str; str++)
		{
			if (isupper(*str))
				*str = tolower(*str);
		}
	}
	return (ptr);
}

char *malloc_sprintf (char **to, char *pattern, ...)
{
	char booya[BIG_BUFFER_SIZE];
	*booya = 0;
	
	if (pattern)
	{
		va_list args;
		va_start (args, pattern);
		vsprintf(booya, pattern, args);
		va_end(args);
	}
	malloc_strcpy(to, booya);
	return *to;
}

/* same thing, different variation */
char *m_sprintf (char *pattern, ...)
{
	char booya[BIG_BUFFER_SIZE * 4 + 1];
	*booya = 0;
	
	if (pattern)
	{
		va_list args;
		va_start (args, pattern);
		vsprintf(booya, pattern, args);
		va_end(args);
	}
	return m_strdup(booya);
}

/* case insensitive string searching */
char	*stristr (char *source, char *search)
{
        int     x = 0;

        if (!source || !*source || !search || !*search || strlen(source) < strlen(search))
		return NULL;

        while (*source)
        {
                if (source[x] && toupper(source[x]) == toupper(search[x]))
			x++;
                else if (search[x])
			source++, x = 0;
		else
			return source;
        }
	return NULL;
}

/* case insensitive string searching from the end */
char	*rstristr (char *source, char *search)
{
	char *ptr;
	int x = 0;

        if (!source || !*source || !search || !*search || strlen(source) < strlen(search))
		return NULL;

	ptr = source + strlen(source) - strlen(search);

	while (ptr >= source)
        {
		if (!search[x])
			return ptr;

		if (toupper(ptr[x]) == toupper(search[x]))
			x++;
		else
			ptr--, x = 0;
	}
	return NULL;
}

/* 
 * word_count:  Efficient way to find out how many words are in
 * a given string.  Relies on isspace() not being broken.
 */
extern  int     word_count (char *str)
{
        int cocs = 0;
        int isv = 1;
        char *foo = str;

        if (!foo)
                return 0;

        while (*foo)
        {
                if (!my_isspace(*foo) != !isv)
                {
                        isv = my_isspace(*foo);
                        cocs++;
                }
                foo++;
        }
        return (cocs + 1) / 2;
}

char	*next_arg (char *str, char **new_ptr)
{
	char	*ptr;

	/* added by Sheik (kilau@prairie.nodak.edu) -- sanity */
	if (!str || !*str)
		return NULL;

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

extern char *remove_trailing_spaces (char *foo)
{
	char *end;
	if (!*foo)
		return foo;

	end = foo + strlen(foo) - 1;
	while (my_isspace(*end))
		end--;
	end[1] = 0;
	return foo;
}

/*
 * yanks off the last word from 'src'
 * kinda the opposite of next_arg
 */
extern char *last_arg (char **src)
{
	char *ptr;

	if (!src || !*src)
		return NULL;

	remove_trailing_spaces(*src);
	ptr = *src + strlen(*src);
	while ((ptr > *src) && !my_isspace(*ptr))
		ptr--;

	if (ptr <= *src)
	{
		ptr = *src;
		*src = empty_string;
	}
	else
	{
		*ptr = 0;
		ptr++;
		remove_trailing_spaces(*src);
	}
	return ptr;
}

char	*new_next_arg (char *str, char **new_ptr)
{
	char	*ptr,
		*start;

	if (!str || !*str)
		return NULL;

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

/*
 * This function is "safe" because it doesnt ever return NULL.
 * XXXX - this is an ugly kludge that needs to go away
 */
char	*safe_new_next_arg (char *str, char **new_ptr)
{
	char	*ptr,
		*start;

	if (!str || !*str)
		return empty_string;

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

	if (!ptr)
		return empty_string;

	return ptr;
}

char	*new_new_next_arg (char *str, char **new_ptr, char *type)
{
	char	*ptr,
		*start;

	if (!str || !*str)
		return NULL;

	if ((ptr = sindex(str, "^ \t")) != NULL)
	{
		if ((*ptr == '"') || (*ptr == '\''))
		{
			char blah[3];
			blah[0] = *ptr;
			blah[1] = '\\';
			blah[2] = '\0';

			*type = *ptr;
			start = ++ptr;
			while ((str = sindex(ptr, blah)) != NULL)
			{
				switch (*str)
				{
				case '\'':
				case '"':
					*str++ = '\0';
					if (*str == ' ')
						str++;
					if (new_ptr)
						*new_ptr = str;
					return (start);
				case '\\':
					if (str[1] == *type)
						strcpy(str, str + 1);
					ptr = str + 1;
				}
			}
			str = empty_string;
		}
		else
		{
			*type = '\"';
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

char stricmp_table [] = 
{
	0,	1,	2,	3,	4,	5,	6,	7,
	8,	9,	10,	11,	12,	13,	14,	15,
	16,	17,	18,	19,	20,	21,	22,	23,
	24,	25,	26,	27,	28,	29,	30,	31,
	32,	33,	34,	35,	36,	37,	38,	39,
	40,	41,	42,	43,	44,	45,	46,	47,
	48,	49,	50,	51,	52,	53,	54,	55,
	56,	57,	58,	59,	60,	61,	62,	63,
	64,	65,	66,	67,	68,	69,	70,	71,
	72,	73,	74,	75,	76,	77,	78,	79,
	80,	81,	82,	83,	84,	85,	86,	87,
	88,	89,	90,	91,	92,	93,	94,	95,
	96,	65,	66,	67,	68,	69,	70,	71,
	72,	73,	74,	75,	76,	77,	78,	79,
	80,	81,	82,	83,	84,	85,	86,	87,
	88,	89,	90,	91,	92,	93,	126,	127,

	128,	129,	130,	131,	132,	133,	134,	135,
	136,	137,	138,	139,	140,	141,	142,	143,
	144,	145,	146,	147,	148,	149,	150,	151,
	152,	153,	154,	155,	156,	157,	158,	159,
	160,	161,	162,	163,	164,	165,	166,	167,
	168,	169,	170,	171,	172,	173,	174,	175,
	176,	177,	178,	179,	180,	181,	182,	183,
	184,	185,	186,	187,	188,	189,	190,	191,
	192,	193,	194,	195,	196,	197,	198,	199,
	200,	201,	202,	203,	204,	205,	206,	207,
	208,	209,	210,	211,	212,	213,	214,	215,
	216,	217,	218,	219,	220,	221,	222,	223,
	224,	225,	226,	227,	228,	229,	230,	231,
	232,	233,	234,	235,	236,	237,	238,	239,
	240,	241,	242,	243,	244,	245,	246,	247,
	248,	249,	250,	251,	252,	253,	254,	255
};

/* my_stricmp: case insensitive version of strcmp */
int	my_stricmp (register char *str1, register char *str2)
{
	while (*str1 && *str2 && (stricmp_table[(unsigned short)*str1] == stricmp_table[(unsigned short)*str2]))
		str1++, str2++;

	return (*str1 - *str2);
}

/* my_strnicmp: case insensitive version of strncmp */
int	my_strnicmp (register char *str1, register char *str2, register int n)
{
	while (n && *str1 && *str2 && (stricmp_table[(unsigned short)*str1] == stricmp_table[(unsigned short)*str2]))
		str1++, str2++, n--;

	return (n) ? (*str1 - *str2) : 0;
}


/* chop -- chops off the last character. capiche? */
char *chop (char *stuff, int nchar)
{
	*(stuff + strlen(stuff) - nchar) = 0;
	return stuff;
}

/*
 * strext: Makes a copy of the string delmited by two char pointers and
 * returns it in malloced memory.  Useful when you dont want to munge up
 * the original string with a null.  end must be one place beyond where
 * you want to copy, ie, its the first character you dont want to copy.
 */
char *strext(char *start, char *end)
{
	char *ptr, *retval;

	ptr = retval = (char *)new_malloc(end-start+1);
	while (start < end)
		*ptr++ = *start++;
	*ptr = 0;
	return retval;
}


/*
 * strmcpy: Well, it's like this, strncpy doesn't append a trailing null if
 * strlen(str) == maxlen.  strmcpy always makes sure there is a trailing null 
 */
char *	strmcpy (char *dest, char *src, int maxlen)
{
	strncpy(dest, src, maxlen);
	dest[maxlen] = '\0';
	return dest;
}

/*
 * strmcat: like strcat, but truncs the dest string to maxlen (thus the dest
 * should be able to handle maxlen+1 (for the null)) 
 */
char *	strmcat(char *dest, char *src, int maxlen)
{
	int	srclen,
	len;

	srclen = strlen(src);
	if ((len = strlen(dest) + srclen) > maxlen)
		strncat(dest, src, srclen - (len - maxlen));
	else
		strcat(dest, src);

	return dest;
}

/*
 * strmcat_ue: like strcat, but truncs the dest string to maxlen (thus the dest
 * should be able to handle maxlen + 1 (for the null)). Also unescapes
 * backslashes.
 */
char *	strmcat_ue(char *dest, char *src, int maxlen)
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
	return dest;
}

/*
 * m_strcat_ues: Given two strings, concatenate the 2nd string to
 * the end of the first one, but if the "unescape" argument is 1, do
 * unescaping (like in strmcat_ue).
 * (Malloc_STRCAT_UnEscape Special, in case you were wondering. ;-))
 *
 * This uses a cheating, "not-as-efficient-as-possible" algorithm,
 * but it works with negligible cpu lossage.
 */
char *	m_strcat_ues(char **dest, char *src, int unescape)
{
	int total_length = (*dest) ? strlen(*dest) : 0;
	char *buffer = NULL;
	char *ptr, *ptr2;

	if (unescape)
	{
		for (ptr = src; *ptr; ptr++, total_length++)
		{
			if (*ptr == '\\' && ptr[1])
				ptr++;
		}
	}
	else
		total_length += strlen(src);

	buffer = (char *) new_malloc (total_length + 1);
	if (*dest)
		strcpy(buffer, *dest);
	else
		*buffer = 0;

	if (unescape)
	{
		ptr2 = buffer + strlen(buffer);
		for (ptr = src; *ptr; ptr++)
		{
			if (*ptr == '\\')
			{
				switch (*++ptr)
				{
					case 'n': case 'p': case 'r': case '0':
						*ptr2++ = '\020';
						break;
					case (char) 0:
						*ptr2++ = '\\';
						break;
					default:
						*ptr2++ = *ptr;
				}
			}
			else
				*ptr2++ = *ptr;
		}
		*ptr2 = '\0';
	}
	else
		strcat(buffer, src);

	new_free(dest);
	*dest = buffer;
	return *dest;
}


/*
 * scanstr: looks for an occurrence of str in source.  If not found, returns
 * 0.  If it is found, returns the position in source (1 being the first
 * position).  Not the best way to handle this, but what the hell 
 */
extern	int	scanstr (char *str, char *source)
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
char	*expand_twiddle (char *str)
{
	char	buffer[BIG_BUFFER_SIZE + 1];
	char *str2;

	if (*str == '~')
	{
		str++;
#ifdef WINNT
		if (*str == '\\' || *str == '\0')
#else
		if (*str == '/' || *str == '\0')
#endif
		{
			strmcpy(buffer, my_path, BIG_BUFFER_SIZE);
			strmcat(buffer, str, BIG_BUFFER_SIZE);
		}
		else
		{
#ifdef WINNT
			return NULL;
#else
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
#endif
		}
	}
	else
		strmcpy(buffer, str, BIG_BUFFER_SIZE);

	/* This isnt legal! */
	str2 = NULL;
	malloc_strcpy(&str2, buffer);
	return (str2);
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
char	*check_nickname (char *nick)
{
	char	*s;

	if (!nick || *nick == '-' || isdigit(*nick))
		return NULL;

	for (s = nick; *s && (s - nick) < NICKNAME_LEN ; s++)
		if (!islegal(*s) || my_isspace(*s))
			break;
	*s = '\0';

	return *nick ? nick : NULL;
}

/*
 * sindex: much like index(), but it looks for a match of any character in
 * the group, and returns that position.  If the first character is a ^, then
 * this will match the first occurence not in that group.
 */
char	*sindex (char *string, char *group)
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

/*
 * rsindex: much like rindex(), but it looks for a match of any character in
 * the group, and returns that position.  If the first character is a ^, then
 * this will match the first occurence not in that group.
 */
char	*rsindex (char *string, char *start, char *group)
{
	char	*ptr;

	if (!string || !group || !start)
		return (char *) NULL;
	if (*group == '^')
	{
		group++;
		for (; string >= start; string--)
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
		for (; string >= start; string--)
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
int	is_number (char *str)
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
char	*rfgets (char *buffer, int size, FILE *file)
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
char	*path_search (char *name, char *path)
{
	static	char	buffer[BIG_BUFFER_SIZE + 1];
	char	*ptr,
		*free_path = NULL;

	/* A "relative" path is valid if the file exists */
	/* A "relative" path is searched in the path if the
	   filename doesnt really exist from where we are */
#ifdef WINNT
	if (strchr(name, '\\') != NULL)
#else
	if (strchr(name, '/') != NULL)
#endif
		if (!access(name, F_OK))
			return name;

	/* an absolute path is always checked, never searched */
#ifdef WINNT
	if (name[0] == '\\')
#else
	if (name[0] == '/')
#endif
		return (access(name, F_OK) ? (char *) 0 : name);

	/* This is cheating. >;-) */
	free_path = path = m_strdup(path);
	while (path)
	{
#ifdef WINNT
		if ((ptr = index(path, ';')) != NULL)
#else
		if ((ptr = index(path, ':')) != NULL)
#endif
			*(ptr++) = '\0';
		strcpy(buffer, empty_string);
		if (path[0] == '~')
		{
			strmcat(buffer, my_path, BIG_BUFFER_SIZE);
			path++;
		}
		strmcat(buffer, path, BIG_BUFFER_SIZE);
#ifdef WINNT
		strmcat(buffer, "\\", BIG_BUFFER_SIZE);
#else
		strmcat(buffer, "/", BIG_BUFFER_SIZE);
#endif
		strmcat(buffer, name, BIG_BUFFER_SIZE);

		if (access(buffer, F_OK) == 0)
			break;
		path = ptr;
	}
	new_free(&free_path);
	return (path) ? buffer : NULL;
}

/*
 * double_quote: Given a str of text, this will quote any character in the
 * set stuff with the QUOTE_CHAR. It returns a malloced quoted, null
 * terminated string 
 */
char	*double_quote (char *str, char *stuff)
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

void	ircpanic (char *format, ...)
{
	char buffer[BIG_BUFFER_SIZE + 1];
	extern char cx_function[];
	
	if (format)
	{
		va_list arglist;
		va_start(arglist, format);
		vsprintf(buffer, format, arglist);
		va_end(arglist);
	}

	yell("An unrecoverable logic error has occured.");
	yell("Please email %s giving me the following message", "edwac@sk.sympatico.ca"  );

	yell("Panic: [%s:%s %s]", irc_version, buffer, cx_function?cx_function:"");
#ifdef BITCHX_DEBUG
	yell("Panic: %d @ %s", cx_line,cx_file);
	irc_exit(0, "BitchX panic... Could it possibly be a bug?  Nahhhh...");
	putlog(LOG_ALL, "*", "Some kinda error at %s (%d)", cx_file, cx_line);
	(void)MY_SIGNAL(SIGABRT, SIG_DFL, 0);
	kill(getpid(), SIGABRT);
#else
	irc_exit(1, "BitchX panic... Could it possibly be a bug?  Nahhhh...");
#endif
}


#ifdef NEED_INDEX

extern	char	*index (char *s, char c)
{
# ifdef HAVE_STRSTR
	return strstr(s, c);
# else
	
	int	len = strlen(s);

	for (; len > 0 && c != *s; s++, len--)
		;
	return (len) ? s : (char *) NULL;
# endif /* HAVE_STRSTR */
}
#endif /* NEED_INDEX */


#ifdef NEED_RINDEX
extern	char	*rindex (char *s, char c)
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


/* Not very complicated, but very handy function to have */
int end_strcmp (const char *one, const char *two, int bytes)
{
	return (strcmp(one + strlen (one) - bytes, two));
}

/* beep_em: Not hard to figure this one out */
void beep_em (int beeps)
{
	int	cnt,
		i;

	for (cnt = beeps, i = 0; i < cnt; i++)
		term_beep();
}



FILE *open_compression (char *executable, char *filename)
{
	FILE *file_pointer;
	int pipes[2];

	context;
	pipes[0] = -1;
	pipes[1] = -1;

	if (pipe (pipes) == -1)
	{
		bitchsay("Cannot start decompression: %s\n", sys_errlist[errno]);
		if (pipes[0] != -1)
		{
			close (pipes[0]);
			close (pipes[1]);
		}
		return NULL;
	}

	switch (fork ())
	{
		case -1:
		{
			bitchsay("Cannot start decompression: %s\n", sys_errlist[errno]);
			return NULL;
		}
		case 0:
		{
			dup2 (pipes[1], 1);
			close (pipes[0]);
			close (2);	/* we dont want to see errors */
			setuid (getuid ());
			setgid (getgid ());
#ifdef ZARGS
			execl (executable, executable, "-c", ZARGS, filename, NULL);
#else
			execl (executable, executable, "-c", filename, NULL);
#endif
			exit (0);
		}
		default :
		{
			close (pipes[1]);
			if ((file_pointer = fdopen(pipes[0], "r")) == NULL)
			{
				bitchsay("Cannot start decompression: %s\n", sys_errlist[errno]);
				return NULL;
			}
			break;
		}
	}
	return file_pointer;
}

/* Front end to fopen() that will open ANY file, compressed or not, and
 * is relatively smart about looking for the possibilities, and even
 * searches a path for you! ;-)
 */
FILE *uzfopen (char **filename, char *path)
{
	static int	setup				= 0;
	int 		ok_to_decompress 		= 0;
	char *		filename_path;
	char 		filename_trying			[256];
	char		*filename_blah;
	static char 	path_to_gunzip[513];
	static char	path_to_uncompress[513];
	FILE *		doh;

	context;
	if (setup == 0)
	{
		char *gzip = path_search("gunzip", getenv("PATH"));
		char *compress = path_search("uncompress", getenv("PATH"));
		if (gzip)
			strcpy (path_to_gunzip, path_search ("gunzip", getenv("PATH")));
		if (compress)
			strcpy (path_to_uncompress, path_search ("uncompress", getenv("PATH")));
		setup = 1;
	}

	/* It is allowed to pass to this function either a true filename
	   with the compression extention, or to pass it the base name of
	   the filename, and this will look to see if there is a compressed
	   file that matches the base name */

	/* Start with what we were given as an initial guess */
	/* kev asked me to call expand_twiddle here */
	filename_blah = expand_twiddle(*filename);
	strcpy(filename_trying, filename_blah);
	new_free(&filename_blah);

	/* Look to see if the passed filename is a full compressed filename */
	if ((! end_strcmp (filename_trying, ".gz", 3)) ||
	    (! end_strcmp (filename_trying, ".z", 2))) 
	{
		if (*path_to_gunzip)
		{	
			ok_to_decompress = 1;
			filename_path = path_search (filename_trying, path);
		}
		else
		{
			bitchsay("Cannot open file %s because gunzip was not found", filename_trying);
			new_free(filename);
			return NULL;
		}
	}
	else if (! end_strcmp (filename_trying, ".Z", 2))
	{
		if (*path_to_gunzip || *path_to_uncompress)
		{
			ok_to_decompress = 1;
			filename_path = path_search (filename_trying, path);
		}
		else
		{
			bitchsay("Cannot open file %s becuase uncompress was not found", filename_trying);
			new_free(filename);
			return NULL;
		}
	}

	/* Right now it doesnt look like the file is a full compressed fn */
	else
	{
		struct stat file_info;

		/* Trivially, see if the file we were passed exists */
		filename_path = path_search (filename_trying, path);

		/* Nope. it doesnt exist. */
		if (!filename_path)
		{
			/* Is there a "filename.gz"? */
			strcpy (filename_trying, *filename);
			strcat (filename_trying, ".gz");
			filename_path = path_search (filename_trying, path);

			/* Nope. no "filename.gz" */
			if (!filename_path)
			{
				/* Is there a "filename.Z"? */
				strcpy (filename_trying, *filename);
				strcat (filename_trying, ".Z");
				filename_path = path_search (filename_trying, path);
				
				/* Nope. no "filename.Z" */
				if (!filename_path)
				{
					/* Is there a "filename.z"? */
					strcpy (filename_trying, *filename);
					strcat (filename_trying, ".z");
					filename_path = path_search (filename_trying, path);

					/* Nope.  No more guesses? then punt */
					if (!filename_path)
					{
						bitchsay("File not found: %s", *filename);
						new_free(filename);
						return NULL;
					}
					/* Yep. there's a "filename.z" */
					else
						ok_to_decompress = 2;
				}
				/* Yep. there's a "filename.Z" */
				else
					ok_to_decompress = 1;
			}
			/* Yep. There's a "filename.gz" */
			else
				ok_to_decompress = 2;
		}
		/* Imagine that! the file exists as-is (no decompression) */
		else
			ok_to_decompress = 0;

		stat (filename_path, &file_info);
		if (file_info.st_mode & S_IFDIR)
		{
			bitchsay("%s is a directory", filename_trying);
			new_free(filename);
			return NULL;
		}
		if (file_info.st_mode & 0111)
		{
			bitchsay("Cannot open %s -- executable file", filename_trying);
			new_free(filename);
			return NULL;
		}
	}

	malloc_strcpy (filename, filename_path);

	/* at this point, we should have a filename in the variable
	   filename_trying, and it should exist.  If ok_to_decompress
	   is one, then we can gunzip the file if guzip is available,
	   else we uncompress the file */
	if (ok_to_decompress)
	{
		if (*path_to_gunzip)
			return open_compression (path_to_gunzip, filename_path);
		else if ((ok_to_decompress == 1) && *path_to_uncompress)
			return open_compression (path_to_uncompress, filename_path);

		bitchsay("Cannot open compressed file %s becuase no uncompressor was found", filename_trying);
		new_free(filename);
		return NULL;
	}

	/* Its not a compressed file... Try to open it regular-like. */
	if ((doh = fopen(filename_path, "r")) != NULL);
		return doh;

	/* nope.. we just cant seem to open this file... */
	bitchsay("Cannot open file %s: %s", filename_path, sys_errlist[errno]);
	new_free(filename);
	return NULL;
}


/*
From: carlson@Xylogics.COM (James Carlson)
Newsgroups: comp.terminals
Subject: Re: Need to skip VT100 codes, any help?
Date: 1 Dec 1994 15:38:48 GMT
*/

/* This function returns 1 if the character passed is NOT printable, it
 * returns 0 if the character IS printable.  It doesnt actually do anything
 * with the character, though.
 */
int vt100_decode (register char chr)
{
	static enum {
		Normal, Escape, SCS, CSI, DCS, DCSData, DCSEscape
	} vtstate = Normal;

	if (chr == 0x1B) 	/* ASCII ESC */
		if (vtstate == DCSData || vtstate == DCSEscape)
			vtstate = DCSEscape;
		else {
			vtstate = Escape;
			return 1;
		}
	else if (chr == 0x18 || chr == 0x1A) 		/* ASCII CAN & SUB */
		vtstate = Normal;

	else if (chr == 0xE || chr == 0xF)		/* ASCII SO & SI */
		;

	/* C0 codes are dispatched without changing machine state!  Oh, my! */
	else if (chr < 0x20) 
		return 0;

	switch (vtstate) 
	{
		case Normal:
			return 0;
			break;
		case Escape:
			switch (chr) 
			{
				case '[':
					vtstate = CSI;
					break;
				case 'P':
					vtstate = DCS;
					break;
				case '(':
				case ')':
					vtstate = SCS;
					break;
				default:
					vtstate = Normal;
			}
			return 1;
			break;
		case SCS:
			vtstate = Normal;
			break;
		case CSI:
			if (isalpha(chr))
				vtstate = Normal;
			break;
		case DCS:
			if (chr >= 0x40 && chr <= 0x7E)
				vtstate = DCSData;
			break;
		case DCSData:
			break;
		case DCSEscape:
			vtstate = Normal;
			break;
	}
	return 1;
}


/* some more string manips by hop (june, 1995) */
extern int fw_strcmp(comp_len_func *compar, char *one, char *two)
{
	int len = 0;
	char *pos = one;

	while (!my_isspace(*pos))
		pos++, len++;

	return compar(one, two, len);
}



/* 
 * Compares the last word in 'one' to the string 'two'.  You must provide
 * the compar function.  my_stricmp is a good default.
 */
extern int lw_strcmp(comp_func *compar, char *one, char *two)
{
	char *pos = one + strlen(one) - 1;

	if (pos > one)			/* cant do pos[-1] if pos == one */
		while (!my_isspace(pos[-1]) && (pos > one))
			pos--;
	else
		pos = one;

	if (compar)
		return compar(pos, two);
	else
		return my_stricmp(pos, two);
}

/* 
 * you give it a filename, some flags, and a position, and it gives you an
 * fd with the file pointed at the 'position'th byte.
 */
extern int opento(char *filename, int flags, int position)
{
	int file;

	file = open(filename, flags, 777);
	lseek(file, position, SEEK_SET);
	return file;
}


/* swift and easy -- returns the size of the file */
long file_size (char *filename)
{
	struct stat statbuf;

	if (!stat(filename, &statbuf))
		return (long)(statbuf.st_size);
	else
		return -1;
}

/* Gets the time in second/usecond if you can,  second/0 if you cant. */
struct timeval get_time(struct timeval *timer)
{
	static struct timeval timer2;
#ifdef HAVE_GETTIMEOFDAY
	if (timer)
	{
		gettimeofday(timer, NULL);
		return *timer;
	}
	gettimeofday(&timer2, NULL);
	return timer2;
#else
	time_t time2 = time(NULL);

	if (timer)
	{
		timer.tv_sec = time2;
		timer.tv_usec = 0;
		return *timer;
	}
	timer2.tv_sec = time2;
	timer2.tv_usec = 0;
	return timer2;
#endif
}

/* 
 * calculates the time elapsed between 'one' and 'two' where they were
 * gotten probably with a call to get_time.  'one' should be the older
 * timer and 'two' should be the most recent timer.
 */
double time_diff (struct timeval one, struct timeval two)
{
	struct timeval td;

	td.tv_sec = two.tv_sec - one.tv_sec;
	td.tv_usec = two.tv_usec - one.tv_usec;

	return (double)td.tv_sec + ((double)td.tv_usec / 1000000.0);
}

int time_to_next_minute _((void))
{
	time_t now;
	struct tm *now_tm;

	time(&now);
	now_tm = gmtime(&now);

	return 60-now_tm->tm_sec;
}

#ifdef __STDC__
char *plural (int number)
#else
char *plural (number)
int number;
#endif
{
	return (number != 1) ? "s" : empty_string;
}

char *my_ctime (time_t when)
{
	return chop(ctime(&when), 1);
}

char *ltoa (long foo)
{
	static char buffer[BIG_BUFFER_SIZE+1];
	char *pos = buffer + BIG_BUFFER_SIZE-1;
	long absv;
	int negative;

	absv = (foo < 0) ? -foo : foo;
	negative = (foo < 0) ? 1 : 0;

	buffer[BIG_BUFFER_SIZE] = 0;
	for (; absv > 9; absv /= 10)
		*pos-- = (absv % 10) + '0';
	*pos = (absv) + '0';

	if (negative)
		*--pos = '-';

	return pos;
}

/*
 * Formats "src" into "dest" using the given length.  If "length" is
 * negative, then the string is right-justified.  If "length" is
 * zero, nothing happens.  Sure, i cheat, but its cheaper then doing
 * two sprintf's.
 */
char *strformat (char *dest, char *src, int length, char pad_char)
{
	char *ptr1 = dest, 
	     *ptr2 = src;
	int tmplen = length;
	int abslen;
	char padc;
		
	abslen = (length >= 0 ? length : -length);
	if (pad_char)
		padc = pad_char;
	else
		padc = (char)get_int_var(PAD_CHAR_VAR);
	if (!padc)
		padc = ' ';

	/* Cheat by spacing out 'dest' */
	for (tmplen = abslen - 1; tmplen >= 0; tmplen--)
		dest[tmplen] = padc;
	dest[abslen] = 0;

	/* Then cheat further by deciding where the string should go. */
	if (length > 0)		/* left justified */
	{
		while ((length-- > 0) && *ptr2)
			*ptr1++ = *ptr2++;
	}
	else if (length < 0)	/* right justified */
	{
		length = -length;
		ptr1 = dest;
		ptr2 = src;
		if (strlen(src) < length)
			ptr1 += length - strlen(src);
		while ((length-- > 0) && *ptr2)
			*ptr1++ = *ptr2++;
	}
	return dest;
}


/* MatchingBracket returns the next unescaped bracket of the given type */
extern char	*MatchingBracket(char *string, char left, char right)
{
	int	bracket_count = 1;

	while (*string && bracket_count)
	{
		if (*string == left)
			bracket_count++;
		else if (*string == right)
		{
			if (!--bracket_count)
				return string;
		}
		else if (*string == '\\' && string[1])
			string++;
		string++;
	}
	return NULL;
}

/*
 * parse_number: returns the next number found in a string and moves the
 * string pointer beyond that point	in the string.  Here's some examples: 
 *
 * "123harhar"  returns 123 and str as "harhar" 
 *
 * while: 
 *
 * "hoohar"     returns -1  and str as "hoohar" 
 */
extern	int	parse_number(char **str)
{
	long ret;
	char *ptr = *str;	/* sigh */

	ret = strtol(ptr, str, 10);
	if (*str == ptr)
		ret = -1;

	return (int)ret;
}

extern char *chop_word(char *str)
{
	char *end = str + strlen(str) - 1;

	while (my_isspace(*end) && (end > str))
		end--;
	while (!my_isspace(*end) && (end > str))
		end--;

	if (end >= str)
		*end = 0;

	return str;
}

#ifdef NON_BLOCKING_CONNECTS
int set_non_blocking(int fd)
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

int set_blocking(int fd)
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

extern int splitw (char *str, char ***to)
{
	int numwords = word_count(str);
	int counter;

	*to = (char **)new_malloc(sizeof(char *) * numwords);
	for (counter = 0; counter < numwords; counter++)
		(*to)[counter] = new_next_arg(str, &str);

	return numwords;
}

extern char * unsplitw (char **str, int howmany)
{
	char *retval = NULL;

	while (howmany)
	{
		m_s3cat(&retval, " ", *str);
		str++, howmany--;
	}

	return retval;
}

char *m_2dup (const char *str1, const char *str2)
{
	size_t msize = strlen(str1) + strlen(str2) + 1;
	return strcat(strcpy((char *)new_malloc(msize), str1), str2);
}

char *m_e3cat (char **one, char *yes1, char *yes2)
{
	if (*one && **one)
		return m_3cat(one, yes1, yes2);
	else
		*one = m_2dup(yes1, yes2);
	return *one;
}

#if !defined(HAVE_MEMMOVE)
/*  $Revision: 1.3 $
**
**  This file has been modified to get it to compile more easily
**  on pre-4.4BSD systems.  Rich $alz, June 1991.
*/

/*
 * sizeof(word) MUST BE A POWER OF TWO
 * SO THAT wmask BELOW IS ALL ONES
 */
typedef	int word;		/* "word" used for optimal copy speed */

#define	wsize	sizeof(word)
#define	wmask	(wsize - 1)

/*
 * Copy a block of memory, handling overlap.
 * This is the routine that actually implements
 * (the portable versions of) bcopy, memcpy, and memmove.
 */
void * mem_move(char *dst0, const char *src0, register size_t length)
{
	register char *dst = dst0;
	register const char *src = src0;
	register size_t t;

	if (length == 0 || dst == src)		/* nothing to do */
		goto retval;

	/*
	 * Macros: loop-t-times; and loop-t-times, t>0
	 */
#define	TLOOP(s) if (t) TLOOP1(s)
#define	TLOOP1(s) do { s; } while (--t)

	if ((unsigned long)dst < (unsigned long)src) {
		/*
		 * Copy forward.
		 */
		t = (int)src;	/* only need low bits */
		if ((t | (int)dst) & wmask) {
			/*
			 * Try to align operands.  This cannot be done
			 * unless the low bits match.
			 */
			if ((t ^ (int)dst) & wmask || length < wsize)
				t = length;
			else
				t = wsize - (t & wmask);
			length -= t;
			TLOOP1(*dst++ = *src++);
		}
		/*
		 * Copy whole words, then mop up any trailing bytes.
		 */
		t = length / wsize;
		TLOOP(*(word *)dst = *(word *)src; src += wsize; dst += wsize);
		t = length & wmask;
		TLOOP(*dst++ = *src++);
	} else {
		/*
		 * Copy backwards.  Otherwise essentially the same.
		 * Alignment works as before, except that it takes
		 * (t&wmask) bytes to align, not wsize-(t&wmask).
		 */
		src += length;
		dst += length;
		t = (int)src;
		if ((t | (int)dst) & wmask) {
			if ((t ^ (int)dst) & wmask || length <= wsize)
				t = length;
			else
				t &= wmask;
			length -= t;
			TLOOP1(*--dst = *--src);
		}
		t = length / wsize;
		TLOOP(src -= wsize; dst -= wsize; *(word *)dst = *(word *)src);
		t = length & wmask;
		TLOOP(*--dst = *--src);
	}
retval:
	return(dst0);
}
#endif

extern int check_val (char *sub)
{
	long sval;
	char *endptr;
	double strtod();

	if (!*sub)
		return 0;

	/* get the numeric value (if any). */
	sval = strtod(sub, &endptr);

	/* Its OK if:
	 *  1) the f-val is not zero.
	 *  2) the first illegal character was not a null.
	 *  3) there were no valid f-chars.
	 */
	if (sval || *endptr || (sub == endptr))
		return 1;

	return 0;
}

char *on_off(int var)
{
	if (var)
		return ("On");
	return ("Off");
}

#ifdef NEED_STRTOUL
#ifndef ULONG_MAX
#define ULONG_MAX (unsigned long) -1
#endif

/*
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Convert a string to an unsigned long integer.
 *
 * Ignores `locale' stuff.  Assumes that the upper and lower case
 * alphabets and digits are each contiguous.
 */
unsigned long strtoul (const char *nptr, char **endptr, int base)
{
	const char *s;
	unsigned long acc, cutoff;
	int c;
	int neg, any, cutlim;

	s = nptr;
	do
		c = *s++;
	while (my_isspace(c));

	if (c == '-') 
	{
		neg = 1;
		c = *s++;
	} 
	else 
	{
		neg = 0;
		if (c == '+')
			c = *s++;
	}

	if ((base == 0 || base == 16) && c == '0' && (*s == 'x' || *s == 'X')) 
	{
		c = s[1];
		s += 2;
		base = 16;
	}

	if (base == 0)
		base = c == '0' ? 8 : 10;

	cutoff = ULONG_MAX / (unsigned long)base;
	cutlim = ULONG_MAX % (unsigned long)base;

	for (acc = 0, any = 0;; c = *s++) 
	{
		if (isdigit(c))
			c -= '0';
		else if (isalpha(c))
			c -= isupper(c) ? 'A' - 10 : 'a' - 10;
		else
			break;

		if (c >= base)
			break;

		if (any < 0)
			continue;

		if (acc > cutoff || acc == cutoff && c > cutlim) 
		{
			any = -1;
			acc = ULONG_MAX;
			errno = ERANGE;
		}
		else 
		{
			any = 1;
			acc *= (unsigned long)base;
			acc += c;
		}
	}
	if (neg && any > 0)
		acc = -acc;
	if (endptr != 0)
		*endptr = (char *) (any ? s - 1 : nptr);
	return (acc);
}
#endif

/*
 * Appends 'num' copies of 'app' to the end of 'str'.
 */
extern char *strextend(char *str, char app, int num)
{
	char *ptr = str + strlen(str);

	for (;num;num--)
		*ptr++ = app;

	*ptr = (char) 0;
	return str;
}

extern char *strfill(char c, int num)
{
static char buffer[BIG_BUFFER_SIZE+1];
int i = 0;
	if (num > BIG_BUFFER_SIZE)
		num = BIG_BUFFER_SIZE;
	for (i = 0; i < num; i++)
		buffer[i] = c;
	buffer[num] = '\0';
	return buffer;
}

/*
 * Pull a substring out of a larger string
 * If the ending delimiter doesnt occur, then we dont pass
 * anything (by definition).  This is because we dont want
 * to introduce a back door into CTCP handlers.
 */
extern char *pullstr (char *source_string, char *dest_string)
{
	char delim = *source_string;
	char *end;

	end = index(source_string + 1, delim);

	/* If there is no closing delim, then we punt. */
	if (!end)
		return NULL;

	*end = 0;
	end++;

	strcpy(dest_string, source_string + 1);
	strcpy(source_string, end);
	return dest_string;
}


extern int empty (const char *str)
{
	while (str && *str && *str == ' ')
		str++;

	if (str && *str)
		return 0;

	return 1;
}

/* makes foo[one][two] look like tmp.one.two -- got it? */
char *remove_brackets (char *name, char *args, int *arg_flag)
{
	char *ptr, *right, *result1, *rptr, *retval;

	/* XXXX - ugh. */
	rptr = m_strdup(name);

	while ((ptr = strchr(rptr, '[')))
	{
		*ptr++ = 0;
		right = ptr;
		if ((ptr = MatchingBracket(right, '[', ']')))
			*ptr++ = 0;

		result1 = upper(expand_alias(NULL, right, args, arg_flag, NULL));
		retval = (char *)new_malloc(strlen(rptr) + strlen(result1) + (ptr ? strlen(ptr) : 0) + 2);
		strcpy(retval, rptr);
		strcat(retval, ".");
		strcat(retval, result1);
		if (ptr)
			strcat(retval, ptr);

		new_free(&result1);
		if (rptr)
			new_free(&rptr);
		rptr = retval;
	}
	return rptr;
}

long my_atol (char *str)
{
	return (long) strtol(str, NULL, 0);
}

