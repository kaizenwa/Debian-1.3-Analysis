/*                             -*- Mode: C++-C -*- 
 * 
 *  
 * 	     Copyright 1994 Christopher B. Liebman
 * 
 *  Permission to use, copy, modify, distribute, and sell this software
 *  and its documentation for any purpose is hereby granted without fee,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name Christopher B. Liebman not
 *  be used in advertising or publicity pertaining to distribution of this
 *  software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 * B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 * INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 * PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * 
 * 
 * Author          : Chris Liebman
 * Created On      : Sun Jan 30 00:08:36 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Mon Mar  7 16:30:27 1994
 * Update Count    : 47
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Disk based search for faces.
*/

#ifndef lint
static char *RCSid = "$Id: face_search_facedb.c,v 1.5 1994/03/08 02:17:40 liebman Exp $";
#endif

#include "faces.h"
#include "face_search.h"

typedef struct facedb_map FacedbMap;
typedef struct host_entry HostEntry;
typedef struct entry Entry;

struct facedb_map
{
    char*	name;
    Entry*	hosts;
    HostEntry*	users;
    FacedbMap*	next;
};

/*
 * This is the top if the users list.  Each host name has a list of
 * aliases.
*/

struct host_entry
{
    char*	name;
    Entry*	users;
    HostEntry*	next;
};

struct entry
{
    char*	from;
    char*	to;
    Entry*	next;
};

static FacedbMap*	maps = NULL;

static char*	buffer = NULL;
static int	buffer_size = 0;

static int
FacedbReadLine(file)
FILE* file;
{
    int	len;
    int c;
    
    if (buffer == NULL)
    {
	buffer_size = 1024;
	buffer = XtMalloc(buffer_size);
    }
    
    len = 0;
    buffer[len] = '\0';
    
    while((c = fgetc(file)) != EOF && c != '\n')
    {
	if (len >= (buffer_size - 1))
	{
	    buffer_size += 1024;
	    buffer = XtRealloc(buffer, buffer_size);
	}
	
	buffer[len++] = c;
	buffer[len] = '\0';
    }
    
    /*
     *   We only return 0 at eof.
    */
    
    if (len == 0 && c != EOF)
    {
	buffer[len++] = '\n';
	buffer[len] = '\0';
    }
    
    return len;
}

static Entry*
HostMapLoad(name)
char*	name;
{
    char*	filename;
    FILE*	file;
    char**	data;
    char*	from;
    char*	to;
    Entry*	head = NULL;
    Entry*	tail = NULL;
    Entry*	e;
    
    filename = XtMalloc(strlen(name) + strlen(TheFacesResources.machine) + 2);
    sprintf(filename, "%s/%s", name, TheFacesResources.machine);
    
    file = fopen(filename, "r");
    
    XtFree(filename);
    
    if (file == NULL)
    {
	return NULL;
    }
    
    while(FacedbReadLine(file))
    {
	/*
	 *   Skip comment chars.
	*/
	
	if (*buffer == '#')
	{
	    continue;
	}
	
	data = StringParse(buffer, "=");
	
	/*
	 *  Must have two parts and two parts only.
	*/
	
	if (data[0] == NULL || data[1] == NULL || data[2] != NULL)
	{
	    continue;
	}
	
	/*
	 *  Strip any whitespace from both values as they are saved.
	*/
	
	from = ParseToken(&(data[0]), " \t\n");
	to   = ParseToken(&(data[1]), " \t\n");
	
	XtFree((char *) data);
	
	if (from == NULL || *from == '\0' ||
	    to   == NULL || *to   == '\0')
	{
	    continue;
	}
	
	e = (Entry*) XtMalloc(sizeof(Entry));
	e->from = XtNewString(from);
	e->to   = XtNewString(to);
	e->next = NULL;
	
	if (!head)
	{
	    head = e;
	    tail = e;
	}
	else
	{
	    tail->next = e;
	    tail = e;
	}
    }

    return head;
}

static HostEntry*
UserMapLoad(name)
char* name;
{
    char*	filename;
    FILE*	file;
    char**	data;
    char**	data2;
    char*	host;
    char*	from;
    char*	to;
    HostEntry*	head = NULL;
    Entry*	e;
    HostEntry*	h;
    
    filename = XtMalloc(strlen(name) + strlen(TheFacesResources.people) + 2);
    sprintf(filename, "%s/%s", name, TheFacesResources.people);
    
    file = fopen(filename, "r");
    
    XtFree(filename);
    
    if (file == NULL)
    {
	return NULL;
    }
    
    while(FacedbReadLine(file))
    {
	/*
	 *   Skip comment chars.
	*/
	
	if (*buffer == '#')
	{
	    continue;
	}
	
	data = StringParse(buffer, "=");
	
	/*
	 *  Must have two parts and two parts only.
	*/
	
	if (data[0] == NULL || data[1] == NULL || data[2] != NULL)
	{
	    XtFree((char*)data);
	    continue;
	}

	/*
	 *   Break up the first part into two parts.
	*/

	data2 = StringParse(data[0], "/");
	
	/*
	 *  Must have two parts and two parts only.
	*/
	
	if (data2[0] == NULL || data2[1] == NULL || data2[2] != NULL)
	{
	    XtFree((char*)data2);
	    XtFree((char*)data);
	    continue;
	}
	
	/*
	 *  Strip any whitespace from values as they are saved.
	*/
	
	host = ParseToken(&(data2[0]), " \t\n");
	from = ParseToken(&(data2[1]), " \t\n");
	to   = ParseToken(&(data[1]), " \t\n");
	
	XtFree((char*)data2);
	XtFree((char *) data);
	
	if (host == NULL || *host == '\0' ||
	    from == NULL || *from == '\0' ||
	    to   == NULL || *to   == '\0')
	{
	    continue;
	}

	/*
	 * Locate host entry in user list, create if needed.
	*/

	for(h = head; h; h = h->next)
	{
	    if (strcmp(h->name, host) == 0)
	    {
		break;
	    }
	}

	if (h == NULL)
	{
	    h = (HostEntry*) XtMalloc(sizeof(HostEntry));
	    h->name = XtNewString(host);
	    h->users = NULL;
	    h->next  = head;
	    head = h;
	}
	
	e = (Entry*) XtMalloc(sizeof(Entry));
	e->from = XtNewString(from);
	e->to   = XtNewString(to);
	e->next = h->users;
	h->users = e;
    }
    
    return head;
}

static FacedbMap*
MapByName(name)
char* name;
{
    FacedbMap*	map;
    
    for (map = maps; map != NULL; map = map->next)
    {
	if (strcmp(name, map->name) == 0)
	{
	    return map;
	}
    }
    
    /*
     *   No map yet.   Create one.
    */
    
    map = (FacedbMap*) XtMalloc(sizeof(FacedbMap));
    map->name = XtNewString(name);
    map->hosts = HostMapLoad(name);
    map->users = UserMapLoad(name);
    map->next = maps;
    maps = map;
    
    return map;
}

static char *
LookupHost(name, realname, map)
char*		name;
char*		realname;
FacedbMap*	map;

{
    Entry* host;
    
    for(host = map->hosts; host; host = host->next)
    {
	if (strcmp(name, host->from) == 0)
	{
	    return host->to;
	}
    }
    
    if (realname != NULL)
    {
	for(host = map->hosts; host; host = host->next)
	{
	    if (strcmp(realname, host->from) == 0)
	    {
		return host->to;
	    }
	}
	
	return realname;
    }
    
    return name;
}

static char *
LookupUser(username, hostname, map)
char*		username;
char*		hostname;
FacedbMap*	map;
{
    Entry*	user;
    HostEntry*	host;
    
    for(host = map->users; host; host = host->next)
    {
	if (strcmp(hostname, host->name) == 0)
	{
	    break;
	}
    }
    
    if (host != NULL)
    {
	for (user = host->users; user; user = user->next)
	{
	    if (strcmp(username, user->from) == 0)
	    {
		return user->to;
	    }
	}
    }
    
    return username;
}

typedef struct facedb_info
{
    MailItem*		item;
    FaceSearchData*	data;
    char*		user;
    char**		host;
    int			parts;
} FacedbInfo;

static void
FaceSearchFacedbMakeName(file, info, parts, try)
char*	file;
FacedbInfo*	info;
int	parts;
char*	try;
{
    char* src;
    char* dst;
    int	i;
    
    src = file;
    
    /*
     * Copy the path.
    */
    
    dst = try;
    while(*src)
    {
	*dst++ = *src++;
    }
    
    /*
     * Now copy each  host part.
    */
    
    for(i = info->parts-1; i > (info->parts - 1) - parts; --i)
    {
	src = info->host[i];
	while(*src)
	{
	    *dst++ = *src++;
	}
	
	*dst++ = '/';
    }
    
    src = info->user;
    while(*src)
    {
	*dst++ = *src++;
    }
    
    *dst = '\0';
}

static int
FaceSearchFacedbWork(file, path, info)
char*		file;
char*		path;
FacedbInfo*	info;
{
    static char*	buffer1 = NULL;
    static int		buffer1_size = 0;
    int			i;
    int			len;
    char*		tail;
    char*		hostname;
    char*		hostdata;
    char**		host;
    int			parts;
    char*		username;
    FacedbMap*		map;
    
    if (buffer1 == NULL)
    {
	buffer1_size = 1024;
	buffer1 = XtMalloc(buffer1_size);
    }

    if (*file == '\0')
    {
	file = "./";
    }
    
    map = MapByName(path);
    hostname = LookupHost(info->item->host, info->item->realhost, map);
    username = LookupUser(info->item->user, hostname, map);
    
    /*
     *  Break the host name up into parts.
    */
    
    hostdata = XtNewString(hostname);
    host = StringParse(hostdata, ".");
    
    /*
     * Count the parts.
    */
    
    for(parts = 0; host[parts] != NULL; ++parts);
    
    info->user  = username;
    info->host  = host;
    info->parts = parts;
    
    /*
     *  Make sure that the buffer is big enough.
    */
    
    len = strlen(file) + strlen(hostname) + strlen(username) + 50;
    
    if (len >= buffer1_size)
    {
	buffer1_size += len;
	buffer1 = XtRealloc(buffer1, buffer1_size);
    }
    
    for( i = parts; i > 0; --i)
    {
	/*
	 *   Compute the path.
	*/
	
	FaceSearchFacedbMakeName(file, info, i, buffer1);
	
	/*
	 * Try to load this guy.
	*/
	
#ifdef FACEDB_DEBUG
	fprintf(stderr, "facedb try: <%s>\n", buffer1);
#endif
	
	if (FaceSearchLoad(buffer1, info->item, info->data))
	{
	    XtFree((char*) host);
	    XtFree(hostdata);
	    return 1;
	}
	
	/*
	 *  try adding "/face" and see what we find.
	*/
	
	for(tail = buffer1; *tail; ++tail);
	
	strcpy(tail, "/face");
	
#ifdef FACEDB_DEBUG
	fprintf(stderr, "facedb try: <%s>\n", buffer1);
#endif
	
	if (FaceSearchLoad(buffer1, info->item, info->data))
	{
	    XtFree((char*) host);
	    XtFree(hostdata);
	    return 1;
	}
	
#if 0
	/*
	 *  Finally if we are looking for an image try "/48x48x1".
	*/
	
	if (info->data->format == FormatImage)
	{
	    strcpy(tail, "/48x48x1");
	    
	    if (FaceSearchLoad(buffer1, info->item, info->data))
	    {
		XtFree((char*) host);
		XtFree(hostdata);
		return 1;
	    }
	}
#endif
    }
    
    /*
     *   try for user in the MISC directory.
    */
    
    sprintf(buffer1, "%sMISC/%s", file, username);
    
#ifdef FACEDB_DEBUG
    fprintf(stderr, "facedb try: <%s>\n", buffer1);
#endif
	
    if (FaceSearchLoad(buffer1, info->item, info->data))
    {
	XtFree((char*) host);
	XtFree(hostdata);
	return 1;
    }
    
    sprintf(buffer1, "%sMISC/%s/face", file, username);
    
#ifdef FACEDB_DEBUG
    fprintf(stderr, "facedb try: <%s>\n", buffer1);
#endif
    
    if (FaceSearchLoad(buffer1, info->item, info->data))
    {
	XtFree((char*) host);
	XtFree(hostdata);
	return 1;
    }
    
    /*
     *  Now attempt the same thing using unknown for the user.
    */
    
    info->user = "unknown";
    
    for( i = parts; i > 0; --i)
    {
	/*
	 *   Compute the path.
	*/
	
	FaceSearchFacedbMakeName(file, info, i, buffer1);
	
	/*
	 * Try to load this guy.
	*/
	
#ifdef FACEDB_DEBUG
	fprintf(stderr, "facedb try: <%s>\n", buffer1);
#endif
	
	if (FaceSearchLoad(buffer1, info->item, info->data))
	{
	    XtFree((char*) host);
	    XtFree(hostdata);
	    
	    if (info->data->format == FormatImage)
	    {
		info->item->unknown = 1;
	    }
	    
	    return 1;
	}
	
	/*
	 *  try adding "/face" and see what we find.
	*/
	
	for(tail = buffer1; *tail; ++tail);
	
	strcpy(tail, "/face");
	
#ifdef FACEDB_DEBUG
	fprintf(stderr, "facedb try: <%s>\n", buffer1);
#endif
	
	if (FaceSearchLoad(buffer1, info->item, info->data))
	{
	    XtFree((char*) host);
	    XtFree(hostdata);
	    
	    if (info->data->format == FormatImage)
	    {
		info->item->unknown = 1;
	    }
	    
	    return 1;
	}
	
#if 0
	/*
	 *  Finally if we are looking for an image try "/48x48x1".
	*/
	
	if (info->data->format == FormatImage)
	{
	    strcpy(tail, "/48x48x1");
	    
	    if (FaceSearchLoad(buffer1, info->item, info->data))
	    {
		XtFree((char*) host);
		XtFree(hostdata);
		return 1;
	    }
	}
#endif
    }
    
    XtFree((char*) host);
    XtFree(hostdata);
    
    /*
     *   One last try.  The MISC directory. Look for "unknown"
    */
    
    sprintf(buffer1, "%sMISC/unknown", file);
    
#ifdef FACEDB_DEBUG
    fprintf(stderr, "facedb try: <%s>\n", buffer1);
#endif
	
    if (FaceSearchLoad(buffer1, info->item, info->data))
    {
	    
	if (info->data->format == FormatImage)
	{
	    info->item->unknown = 1;
	}
	
	return 1;
    }
    
    sprintf(buffer1, "%sMISC/unknown/face", file);
    
#ifdef FACEDB_DEBUG
    fprintf(stderr, "facedb try: <%s>\n", buffer1);
#endif
    
    if (FaceSearchLoad(buffer1, info->item, info->data))
    {
	
	if (info->data->format == FormatImage)
	{
	    info->item->unknown = 1;
	}
	
	return 1;
    }
    
    return 0;
}

static int
FaceSearchFacedb(item, data)
MailItem*	item;
FaceSearchData*	data;
{
    char**	paths = TheFacesResources.facedb_paths;
    int		found = 0;
    FacedbInfo	info;
    
    if ((data != NULL) && (data->paths != NULL))
    {
	paths = data->paths;
    }
    
    /*
     * load the info struct.
    */
    
    info.item  = item;
    info.data  = data;
    info.user  = NULL;
    info.host  = NULL;
    info.parts = 0;
    
    found = PathEnumerate("", paths, FaceSearchFacedbWork, &info);
    
    return found;
}

static FaceSearchType facedb =
{
    "facedb",
    FaceSearchFacedb,
    NULL,
    NULL,
};

void
FaceSearchFacedbInit()
{
    FaceSearchTypeRegister(&facedb);
}
