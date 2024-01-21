/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include "global.h"
#include "child.h"
#include "codes.h"
#include "expand.h"
#include "file.h"
#include "newsrc.h"
#include "resource.h"
#include "server.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"

#define NO_GROUPS	4096
#define NEW_GROUPS	 256

#define MAX_GROUP_LEN	1024

static int comp_group_nodes(const void *a, const void *b)
{
    return strcmp( (*((GROUP **)a))->name, (*((GROUP **)b))->name);
}

int get_newsgroups(void)
{
    char	*buffer;
    long	status;

    buffer = server_comm(main_server, "LIST\r\n", False);
    if (!buffer)
	return -1;
    sscanf(buffer, "%ld", &status);
    if (status != NNTP_OK_GROUPS)
	return status;

    global.no_groups = 0;
    while ((buffer = server_read(main_server)) && !IS_DOT(buffer)) {
	char *c;
	long first, last;
	char mod;

	if (global.no_groups > global.max_groups - 2) {
	    long	i = global.max_groups;

	    global.max_groups += NO_GROUPS;
	    global.groups =
		(GROUP **)XtRealloc((char *)global.groups,
				    global.max_groups *
				    sizeof(GROUP *));

	    while (i < global.max_groups)
		global.groups[i++] = NULL;

	}

	c = strchr(buffer, ' ');
	if (c) {
	    GROUP	*group;
	    int		k;

	    *(c++) = '\0';
	    k = sscanf(c, "%ld%ld%*c%c", &last, &first, &mod);
	    if (k < 2)
		continue;
	    if (k == 2)
		mod = 'y';
	    if (mod == '=' || mod == 'x')
		continue;
	    group = (GROUP *)XtMalloc(sizeof(GROUP));
	    global.groups[global.no_groups++] = group;
	    group->description = NULL;
	    group->read_arts = NULL;
	    group->subscribed = False;
	    group->found_in_newsrc = False;
	    group->ahead_flag = False;
	    group->moderated = (mod == 'm');
	    group->name = XtNewString(buffer);
	    group->first_art = first;
	    group->last_art = last;
	}
    }

    if (!buffer) {
	long	i;

	for (i = 0 ; i < global.no_groups ; i++)
	    XtFree(global.groups[i]->name);
	XtFree((char *)global.groups);

	global.groups = NULL;
	global.no_groups = 0;
	global.max_groups = 0;

	return -1;
    }

    qsort(global.groups, global.no_groups,
	  sizeof(GROUP *), comp_group_nodes);

    return NNTP_OK_GROUPS;
}

/* the grouplist must be sorted here */
GROUP *find_group(char *name)
{
    long	first = 0;
    long	last = global.no_groups - 1;

    while (first <= last &&
	   global.groups[first]->subscribed) {
	if (strcmp(name, global.groups[first]->name) == 0)
	    return global.groups[first];
	first++;
    }

    while (first <= last) {
	long	mid = (first + last) / 2;
	int	comp = strcmp(name, global.groups[mid]->name);

	if (comp == 0)
	    return global.groups[mid];

	if (comp < 0)
	    last = mid - 1;
	else
	    first = mid + 1;
    }

    return NULL;
}

int check_for_new_groups(void)
{
    GROUP	**list = NULL;
    long	l_max = 0;
    struct tm	*tm;
    char	command[64];
    char	*buffer;
    long	status, n = 0;

    if (!res_check_for_new_groups() || global.last_time == 0)
	return True;

    global.new_groups = NULL;
    global.no_new_groups = 0;

    tm = gmtime(&global.last_time);
    sprintf(command, "NEWGROUPS %02d%02d%02d %02d%02d%02d GMT\r\n",
	    tm->tm_year, tm->tm_mon+1, tm->tm_mday,
	    tm->tm_hour, tm->tm_min, tm->tm_sec);
    buffer = server_comm(main_server, command, False);
    if (!buffer)
	return -1;
    status = atoi(buffer);
    if (status != NNTP_OK_NEWGROUPS) {
	fprintf(stderr,
		"check_for_new_groups failed: Message from server is: %s\n",
		buffer);
	return status;
    }

    l_max = NEW_GROUPS;
    list = (GROUP **)XtMalloc(l_max * sizeof(GROUP *));

    while ((buffer = server_read(main_server)) && !IS_DOT(buffer)) {
	char	mod;
	char	*c;

	if (n >= l_max) {
	    l_max += NEW_GROUPS;
	    list = (GROUP **)XtRealloc((char *)list, l_max * sizeof(GROUP *));
	}

	c = strchr(buffer, ' ');
	if (c) {
	    *c++ = '\0';
	    if (sscanf(c, "%*d%*d%*c%c", &mod) == 1 &&
		(mod == '=' || mod == 'x'))
		continue;
	}
	list[n] = find_group(buffer);
	if (!list[n])
	    list[n] = create_group(buffer);
	n++;
    }

    sort_groups();

    if (!buffer) {
	XtFree((char *)list);
	return False;
    }

    if (n <= 0)
	XtFree((char *)list);
    else {
	global.new_groups = list;
	global.no_new_groups = n;
    }

    return NNTP_OK_NEWGROUPS;
}

/* this is a weak point */
static void put_subscribed_groups_first(long n)
{
    GROUP	**subs;
    long	i, j;

    if (n == 0)
	return;

    subs = (GROUP **)XtMalloc(n * sizeof(GROUP *));
    for (i = j = global.no_groups - 1 ; i >= 0 ; i--) {
	if (global.groups[i]->subscribed)
	    subs[global.groups[i]->disp] = global.groups[i];
	else
	    global.groups[j--] = global.groups[i];
    }
    for (i = 0 ; i < n ; i++)
	global.groups[i] = subs[i];

    XtFree((char *)subs);
}

void sort_groups(void)
{
    long i, n;

    for (n = 0 ; n < global.no_groups ; n++)
	if (!global.groups[n]->subscribed)
	    break;

    for (i = n ; i < global.no_groups ; i++)
	if (global.groups[i]->subscribed) {
	    GROUP *temp = global.groups[n];

	    global.groups[n] = global.groups[i];
	    global.groups[i] = temp;
	    n++;
	}

    if (n > 0 && global.sort_groups)
	qsort(global.groups, n, sizeof global.groups[0], comp_group_nodes);

    if (n < global.no_groups)
	qsort(global.groups + n, global.no_groups - n,
	      sizeof global.groups[0], comp_group_nodes);
}

static int	did_rename = False;

int update_newsrc(void)
{
    char	*newsrc_file = res_newsrc_file();
    int		fill         = res_fill_newsrc_file();
    char	*path;
    FILE	*fp;
    long	i;
    int		ok;

    if (!newsrc_file) {
	fputs("knews: newsrcFile is NULL!!\n", stderr);
	return False;
    }

    path = expand_path(newsrc_file);
    if (!path)
	return False;

    block_sighup();

    if (!did_rename) {
	char	*old_newsrc_file = res_old_newsrc_file();
	char	*opath;

	opath = old_newsrc_file ? expand_path(old_newsrc_file) : NULL;
	if (opath) {
	    if (rename(path, opath) < 0)
		perror("rename");
	    XtFree(opath);
	}

	did_rename = True;
    }

    fp = fopen_mkdir(path, "w", True);
    XtFree(path);
    if (!fp) {
	unblock_sighup();
	return False;
    }

    for (i = 0 ; i < global.no_groups ; i++)
	if (fill || global.groups[i]->read_arts ||
	    global.groups[i]->subscribed) {
	    ART_LIST_NODE	*loop = global.groups[i]->read_arts;

	    fprintf(fp, "%s%c", global.groups[i]->name,
		    global.groups[i]->subscribed ? ':' : '!');
	    if (loop)
		fputc(' ', fp);
	    while (loop) {
		fprintf(fp, "%ld", loop->first);
		if (loop->first != loop->last)
		    fprintf(fp, "-%ld", loop->last);
		loop = loop->next;
		if (loop)
		    fputc(',', fp);
	    }
	    fputc('\n', fp);
	}

    ok = fclose(fp) == 0;

    unblock_sighup();

    return ok;
}

void calc_no_unread(GROUP *group)
{
    ART_LIST_NODE *loop;

    if (group->first_art == 0 && group->last_art == 0) {
	group->no_unread = 0;
	return;
    }

    group->no_unread = group->last_art - group->first_art + 1;
    for (loop = group->read_arts ; loop ; loop = loop->next) {
	if (loop->last > group->last_art) {
	    if (loop->first <= group->last_art)
		group->no_unread -= (group->last_art - loop->first + 1);
	    return;
	} else if (loop->first < group->first_art) {
	    if (loop->last >= group->first_art)
		group->no_unread -= (loop->last - group->first_art +1);
	} else {
	    group->no_unread -= (loop->last - loop->first + 1);
	}
    }
}

int get_descriptions(void)
{
    char	*file_name = res_descriptions_file();
    FILE	*fp = NULL;
    SERVER	*server;
    char	*buffer;
    long	n;

    if (res_retrieve_descriptions()) {
	set_message("Retrieving group descriptions...", False);
	server = main_server;
	buffer = server_comm(main_server, "LIST NEWSGROUPS\r\n", False);
	if (!buffer)
	    return -1;
	sscanf(buffer, "%ld", &n);
	if (n != NNTP_OK_GROUPS)
	    return n;

	if (file_name)
	    fp = fopen_expand(file_name, "w", True);
	server_set_bs(server, fp);
    } else {
	int	fd;

	if (!file_name)
	    return 0;
	fd = open_expand(file_name, O_RDONLY, True);
	if (fd < 0)
	    return 0;

	set_message("Reading group descriptions from file...", False);
	server = server_create(fd);
    }

    while ((buffer = server_read(server)) && !IS_DOT(buffer)) {
	GROUP	*group;
	char	*c;

	c = buffer;
	while (*c != '\0' && *c != '\t' && *c != ' ') c++;
	if (*c == '\0')
	    continue;

	*c++ = '\0';
	while (*c == ' ' || *c == '\t') c++;
	group = find_group(buffer);
	if (group) {
	    if (group->description) {
		long	len = strlen(group->description);

		/* if duplicate descriptions, take the longest */
		if (len >= strlen(c))
		    continue;
		XtFree(group->description);
	    }

	    group->description = XtNewString(c);
	}
    }

    if (fp) {
	server_set_bs(server, NULL);
	if (fclose(fp) < 0 || !buffer)
	    unlink_expand(file_name);
    }

    if (server != main_server)
	server_free(server);
    else if (!buffer)
	return -1;

    return NNTP_OK_GROUPS;
}

GROUP *create_group(char *name)
{
    GROUP	*group;

    global.groups =
	(GROUP **)XtRealloc((char *)global.groups,
			    (global.no_groups + 1) * sizeof global.groups[0]);
    group = global.groups[global.no_groups++] =
	(GROUP *)XtMalloc(sizeof(GROUP));
    group->name = XtNewString(name);
    group->description = NULL;
    group->no_unread = 0;
    group->first_art = 0;
    group->last_art = 0;
    group->read_arts = NULL;
    group->disp = -1;
    group->subscribed = False;
    group->moderated = False;
    group->found_in_newsrc = False;
    group->ahead_flag = False;

    return group;
}

static void check_read_arts_sanity(GROUP *temp)
{
    ART_LIST_NODE	*loop;

    for (loop = temp->read_arts ; loop ; loop = loop->next)
	if (loop->first > loop->last ||
	    (loop->next && loop->last > loop->next->first)) {
	    fprintf(stderr,
		    "knews: Bad list of read articles in .newsrc file, "
		    "group %s.  Marking all articles unread.\n",
		    temp->name);
	    free_read_arts_list(temp);
	    temp->read_arts = NULL;
	    return;
	}
}

static ART_LIST_NODE *parse_read_arts_list(char *list)
{
    ART_LIST_NODE	*result = NULL;
    long		first = 0, last = 0;

    while (IS_SPACE(*list))
	list++;
    if (*list == '\0')
	return NULL;

    while (IS_DIGIT(*list)) {
	first *= 10;
	first += *list - '0';
	list++;
    }

    if (*list == ',' || *list == '\0')
	last = first;
    else if (*list == '-') {
	list++;
	while (IS_DIGIT(*list)) {
	    last *= 10;
	    last += *list - '0';
	    list++;
	}
    }

    result = (ART_LIST_NODE *)XtMalloc(sizeof *result);
    result->first = first;
    result->last  = last;
    result->next  = NULL;

    if (*list == ',')
	result->next = parse_read_arts_list(list + 1);
    else if (*list != '\0')
	fputs("Parse error in newsrc.\n", stderr);

    return result;
}

static void create_new_newsrc(char *path)
{
    FILE	*fp;
    int		c;

    fp = fopen_mkdir(path, "w", True);
    popup_title_notice(fp ? "Created newsrc file" :
		       "Failed to create newsrc file", path, fp == NULL);
    if (!fp)
	return;

    if (global.auto_subscribe)
	if (global.auto_subscribe[0] != '/')
	    fputs(global.auto_subscribe, fp);
	else {
	    FILE	*ng = fopen(global.auto_subscribe, "r");

	    if (!ng)
		perror(global.auto_subscribe);
	    else {
		while ((c = getc(ng)) != EOF)
		    putc(c, fp);
		fclose(ng);
	    }
	}

    fclose(fp);
}

void parse_newsrc(int create)
{
    char	*new = res_newsrc_file();
    char	*buffer, *path;
    SERVER	*s;
    long	pos;
    int		fd;

    did_rename = False;

    if (!new) {
	fputs("knews: newsrcFile is NULL!\n", stderr);
	return;
    }

    path = expand_path(new);
    if (!path)
	return;

    fd = open(path, O_RDONLY);
    if (fd < 0) {
	perror(path);
	create_new_newsrc(path);
	fd = open(path, O_RDONLY);
    }
    XtFree(path);
    if (fd < 0)
	return;

    pos = 0;
    s = server_create(fd);
    while ((buffer = server_read(s))) {
	int	n = 0, subscribed;
	GROUP	*temp;

	while (buffer[n] != '\0' && buffer[n] != ':' && buffer[n] != '!')
	    n++;
	if (buffer[n] == '\0') {
	    fprintf(stderr, "Garbage line in newsrc: %s\n", buffer);
	    continue;
	}

	subscribed = buffer[n] == ':';
	buffer[n] = '\0';

	temp = find_group(buffer);
	if (!temp && create)
	    temp = create_group(buffer);
	if (!temp) {
	    fprintf(stderr, "Bogus group in newsrc file: %s\n", buffer);
	    continue;
	}
	if (temp->found_in_newsrc) {
	    fprintf(stderr, "Group %s appeared twice in newsrc.\n", buffer);
	    continue;
	}
	temp->subscribed = subscribed;
	temp->found_in_newsrc = True;
	if (subscribed)
	    temp->disp = pos++;
	temp->read_arts = parse_read_arts_list(buffer + n + 1);
	check_read_arts_sanity(temp);
    }

    server_free(s);
    put_subscribed_groups_first(pos);
    sort_groups();
}

int get_newsgroups_from_newsrc(void)
{
    char	*tmp;

    parse_newsrc(True);
    tmp = rescan();
    if (!tmp)
	return -1;

    return atoi(tmp);
}

char *rescan(void)
{
    char	message[128];
    char	*buffer, *p;
    long	n;

    for (n = 0 ; n < global.no_groups ; n++)
	if (!global.groups[n]->subscribed)
	    break;
    if (n == 0)
	return CODE_TO_STR(NNTP_OK_GROUPS);

    strcpy(message, "Rescan in progress...  ");
    p = message + strlen(message);

    if (res_read_active_file()) {
	long	i, j, k, m;
	buffer = server_comm(main_server, "LIST\r\n", True);
	if (!buffer || atoi(buffer) != NNTP_OK_GROUPS)
	    return buffer;

	j = m = global.no_groups / 16;
	i = 0;
	while ((buffer = server_read(main_server)) && !IS_DOT(buffer)) {
	    char	*c = strchr(buffer, ' ');

	    if (c) {
		*c++ = '\0';
		for (k = 0 ; k < n ; k++) {
		    if (strcmp(buffer, global.groups[k]->name) == 0) {
			sscanf(c, "%ld%ld",
			       &global.groups[k]->last_art,
			       &global.groups[k]->first_art);
			break;
		    }
		}
	    }

	    if (j-- <= 0) {
		sprintf(p, "%3ld%%", i / global.no_groups);
		set_message(message, False);
		j = m;
	    }
	    i += 100;
	}

	if (!buffer)
	    return NULL;
    } else {
	int	list = res_try_list_active();
	char	command[1024];
	long	i, j, first, last;
	char	m;

	for (i = 0, j = 0 ; j < global.no_groups ; j++) {
	    if (!global.groups[j]->subscribed)
		break;
	    if (strlen(global.groups[j]->name) > 480) {
		fprintf(stderr, "Group name too long: %s\n",
			global.groups[j]->name);
		continue;
	    }

	    if (list) {
		sprintf(command, "LIST ACTIVE %s\r\n", global.groups[j]->name);
		server_write(main_server, command);
		buffer = server_read(main_server);
		if (!buffer)
		    return NULL;
		if (atol(buffer) == NNTP_OK_GROUPS) {
		    while ((buffer = server_read(main_server)) &&
			   !IS_DOT(buffer)) {
			char	*c = strchr(buffer, ' ');

			if (c) {
			    *c++ = '\0';
			    m = 'y';
			    if (case_strcmp(global.groups[j]->name,
					    buffer) == 0 &&
				sscanf(c, "%ld%ld%*c%c",
				       &last, &first, &m) >= 2 &&
				m != '-' && m != 'x') {
				global.groups[j]->first_art = first;
				global.groups[j]->last_art = last;
				global.groups[j]->moderated = (m == 'm');
			    }
			}
		    }

		    if (!buffer)
			return NULL;
		} else {
		    fprintf(stderr,
			    "knews: 'LIST ACTIVE wildmat' failed, message "
			    "from server is: \n"
			    "       %s\n"
			    "       Falling back to GROUP\n", buffer);
		    list = False;
		}
	    }

	    if (!list) {
		sprintf(command, "GROUP %s\r\n", global.groups[j]->name);
		server_write(main_server, command);
		buffer = server_read(main_server);
		if (!buffer)
		    return NULL;
		if (atoi(buffer) != NNTP_OK_GROUP)
		    fprintf(stderr, "knews: Bogus group: %s\n",
			    global.groups[j]->name);
		else
		    sscanf(buffer, "%*d%*d%ld%ld",
			   &global.groups[j]->first_art,
			   &global.groups[j]->last_art);
	    }

	    i += 100;
	    sprintf(p, "%ld%%", i / n);
	    set_message(message, False);
	}
    }

    return CODE_TO_STR(NNTP_OK_GROUPS);
}
