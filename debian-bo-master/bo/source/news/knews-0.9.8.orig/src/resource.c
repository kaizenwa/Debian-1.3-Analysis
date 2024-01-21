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
#include <sys/stat.h>
#include <utime.h>
#include <X11/Xresource.h>
#include "file.h"
#include "resource.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/Compat.h"

typedef enum {
    KeepTypeNo   = False,
    KeepTypeYes  = True,
    KeepTypeSubscribed
} KeepType;

#define DEFAULT_QUOTE_REGEXP	"^[ \t]*[:>|]"
#define DEFAULT_ATTRIBUTION	"In article %m,\n\t%f writes:";

#define DEFAULT_DB_SIZE   4096

static const char	*bad_regex_msg =
"knews: regcomp() couldn't compile the default quote regexp:\n"
"          \"" DEFAULT_QUOTE_REGEXP "\"\n"
"       Chances are your system's regex implementaton sucks\n"
"       You might want to recompile knews with HAVE_POSIX_REGEXPS\n"
"       set to 0.  See the file configure.h for details.\n";

static XrmDatabase	db = 0;

static char	*pw_name = NULL;
static char	*default_header_format[] = {
    "Subject",		"Newsgroups",		"Followup-To",
    "Reply-To:",	"Content-Description",	"Date",
    "Organization",	"From",			NULL,
};
static regex_t	default_quote_re;
static regex_t	quote_re;

struct per_server{
    struct {
	char	*newsrc_file;
	char	*old_newsrc_file;
	char	*kill_file;
	char	*remote_newsrc_file;
	char	*remote_kill_file;
	char	*auth_info_user;
	char	*auth_info_pass;
	char	*rescan_timeout;
	char    *retrieve_descriptions;
	char	*check_for_new_groups;
	char	*read_active_file;
	char	*thread_ahead_groups;
	char	*ask_how_many;
	char	*posting_agent;
	char	*fill_newsrc_file;
	char	*try_list_active;
	char	*cache_dir;
	char	*descriptions_file;
	char	*save_thread_info;
	char	*group_name_columns;
    } str;
    struct {
	char	**thread_ahead_groups;
	long	rescan_timeout;
	int	group_name_columns;
	char	retrieve_descriptions;
	char	check_for_new_groups;
	char	read_active_file;
	char	ask_how_many;
	char	fill_newsrc_file;
	char	try_list_active;
	char	save_thread_info;
    } conv;
};

static struct per_server per_server, per_server_default = {{0, }, };

struct per_group {
    struct {
	char	*header_format;
	char	*quote_regexp;
	char	*quote_string;
	char	*quote_quote_string;
	char	*attribution;
	char	*full_name;
	char	*signature_file;
	char	*organization;
	char	*reply_to;
	char	*extra_headers;
	char	*followup_headers;
	char	*posted_and_mailed;
	char	*uu_dir;
	char	*uu_program;
	char	*distribution;
	char	*full_header;
	char	*process_xrefs;
	char	*show_number_lines;
	char	*keep_thread_info;
	char	*sort_threads;
	char	*expire_kills;
	char	*default_charset;
	char	*assemble_partials;
	char	*cache_ahead_size;
	char	*cache_trail_size;
	char	*subject_columns;
    } str;
    struct {
	char	**header_format;
	regex_t	*quote_regexp;
	int	cache_ahead_size;
	int	cache_trail_size;
	int	subject_columns;
	char	full_header;
	char	process_xrefs;
	char	show_number_lines;
	char	keep_thread_info;
	char	expire_kills;
	char	assemble_partials;
    } conv;
};

static struct per_group per_group, per_group_default = {{0, }, };

typedef union {
    char	*str;
    XrmQuark	q;
} RES_NAME;

typedef struct {
    RES_NAME	name;
    RES_NAME	class;
    const int	offset;
} RES_DESC;

static RES_DESC per_server_desc[] = {
#define offset(field) XtOffsetOf(struct per_server, str.field)
    {{"newsrcFile"},		{"NewsrcFile"},
     offset(newsrc_file)},
    {{"oldNewsrcFile"},		{"OldNewsrcFile"},
     offset(old_newsrc_file)},
    {{"killFile"},		{"KillFile"},
     offset(kill_file)},
    {{"remoteNewsrcFile"},	{"RemoteNewsrcFile"},
     offset(remote_newsrc_file)},
    {{"remoteKillFile"},	{"RemoteKillFile"},
     offset(remote_kill_file)},
    {{"authInfoUser"},		{"AuthInfoUser"},
     offset(auth_info_user)},
    {{"authInfoPass"},		{"AuthInfoPass"},
     offset(auth_info_pass)},
    {{"postingAgent"},		{"PostingAgent"},
     offset(posting_agent)},
    {{"rescanTimeout"},		{"RescanTimeout"},
     offset(rescan_timeout)},
    {{"retrieveDescriptions"},	{"RetrieveDescriptions"},
     offset(retrieve_descriptions)},
    {{"checkForNewGroups"},	{"CheckForNewGroups"},
     offset(check_for_new_groups)},
    {{"readActiveFile"},	{"ReadActiveFile"},
     offset(read_active_file)},
    {{"fillNewsrcFile"},	{"FillNewsrcFile"},
     offset(fill_newsrc_file)},
    {{"threadAheadGroups"},	{"ThreadAheadGroups"},
     offset(thread_ahead_groups)},
    {{"askHowMany"},		{"AskHowMany"},
     offset(ask_how_many)},
    {{"tryListActive"},		{"TryListActive"},
     offset(try_list_active)},
    {{"cacheDir"},		{"CacheDir"},
     offset(cache_dir)},
    {{"descriptionsFile"},	{"DescriptionsFile"},
     offset(descriptions_file)},
    {{"saveThreadInfo"},	{"SaveThreadInfo"},
     offset(save_thread_info)},
    {{"groupNameColumns"},	{"GroupNameColumns"},
     offset(group_name_columns)},
#undef offset
};

static RES_DESC per_group_desc[] = {
#define offset(field)	XtOffsetOf(struct per_group, str.field)
    {{"fullName"},		{"FullName"},
     offset(full_name)},
    {{"organization"},		{"Organization"},
     offset(organization)},
    {{"replyTo"},		{"ReplyTo"},
     offset(reply_to)},
    {{"distribution"},		{"Distribution"},
     offset(distribution)},
    {{"headerFormat"},		{"HeaderFormat"},
     offset(header_format)},
    {{"quoteRegexp"},		{"QuoteRegexp"},
     offset(quote_regexp)},
    {{"quoteString"},		{"QuoteString"},
     offset(quote_string)},
    {{"quoteQuoteString"},	{"QuoteString"},
     offset(quote_quote_string)},
    {{"attribution"},		{"Attribution"},
     offset(attribution)},
    {{"signatureFile"},		{"SignatureFile"},
     offset(signature_file)},
    {{"extraHeaders"},		{"ExtraHeaders"},
     offset(extra_headers)},
    {{"followupHeaders"},	{"FollowupHeaders"},
     offset(followup_headers)},
    {{"postedAndMailed"},	{"PostedAndMailed"},
     offset(posted_and_mailed)},
    {{"uuDir"},			{"UuDir"},
     offset(uu_dir)},
    {{"uuProgram"},		{"UuProgram"},
     offset(uu_program)},
    {{"fullHeader"},		{"FullHeader"},
     offset(full_header)},
    {{"processXrefs"},		{"ProcessXrefs"},
     offset(process_xrefs)},
    {{"showNumberLines"},	{"ShowNumberLines"},
     offset(show_number_lines)},
    {{"keepThreadInfo"},	{"KeepThreadInfo"},
     offset(keep_thread_info)},
    {{"sortThreads"},		{"SortThreads"},
     offset(sort_threads)},
    {{"expireKills"},		{"ExpireKills"},
     offset(expire_kills)},
    {{"defaultCharset"},	{"DefaultCharset"},
     offset(default_charset)},
    {{"assemblePartials"},	{"AssemblePartials"},
     offset(assemble_partials)},
    {{"cacheAheadSize"},	{"CacheSize"},
     offset(cache_ahead_size)},
    {{"cacheTrailSize"},	{"CacheSize"},
     offset(cache_trail_size)},
    {{"subjectColumns"},	{"SubjectColumns"},
     offset(subject_columns)},
#undef offset
};

static void clear_per_server(void)
{
    if (per_server.conv.thread_ahead_groups) {
	XtFree(per_server.conv.thread_ahead_groups[0]);
	XtFree((char *)per_server.conv.thread_ahead_groups);
    }
    per_server = per_server_default;
}

static void clear_per_group(void)
{
    if (per_group.conv.header_format &&
	per_group.conv.header_format != default_header_format) {
	XtFree(per_group.conv.header_format[0]);
	XtFree((char *)per_group.conv.header_format);
    }
    if (per_group.conv.quote_regexp) {
	if (per_group.conv.quote_regexp != &default_quote_re) {
	    regfree(per_group.conv.quote_regexp);
	    memset(per_group.conv.quote_regexp, 0, sizeof(regex_t));
	}
	per_group.conv.quote_regexp = NULL;
    }
    per_group = per_group_default;
}

static char **str_to_list(char *list, char *sep)
{
    char	**res;
    int		n_alloc, n;

    if (!list || !sep || *sep == '\0')
	return NULL;

    while (*list != '\0' && strchr(sep, *list))
	list++;
    if (*list == '\0') {
	res = (char **)XtMalloc(sizeof res[0]);
	res[0] = NULL;
	return res;
    }

    list = XtNewString(list);

    n_alloc = 8;
    res = (char **)XtMalloc(n_alloc * sizeof res[0]);
    n = 0;

    do {
	if (n + 4 > n_alloc) {
	    n_alloc *= 2;
	    res = (char **)XtRealloc((char *)res, n_alloc * sizeof res[0]);
	}

	res[n++] = list;

	while (*list != '\0' && !strchr(sep, *list))
	    list++;

	if (*list == '\0')
	    break;
	*list++ = '\0';

	while (*list != '\0' && strchr(sep, *list))
	    list++;
    } while (*list != '\0');

    res[n] = NULL;

    return res;
}

static int str_to_bool(char *str, int def)
{
    static struct {
	char	*str;
	char	len;
	char	val;
    } values[] = {
	{"true",	4,	True},
	{"false",	5,	False},
	{"yes",		3,	True},
	{"no",		2,	False},
	{"on",		2,	True},
	{"off",		3,	False},
    };
    int		len;
    int		i;

    if (!str)
	return def;

    len = strlen(str);
    for (i = 0 ; i < XtNumber(values) ; i++)
	if (len == values[i].len && case_lstrcmp(str, values[i].str) == 0)
	    return values[i].val;

    fprintf(stderr, "knews: failed to convert \"%s\" to a boolean.\n", str);

    return def;
}

static int str_to_keep(char *str, int def)
{
    if (!str)
	return def;
    if ((*str == 's' || *str == 'S') &&
	case_lstrcmp(str, "subscribed") == 0)
	return KeepTypeSubscribed;
    return str_to_bool(str, def);
}

static long str_to_long(char *str, long def)
{
    long  res;

    if (!str)
	return def;

    if (sscanf(str, "%ld", &res) == 1)
	return res;

    fprintf(stderr, "knews: failed to convert \"%s\" to an integer.\n", str);

    return def;
}

/*************************************************************************/

static long print_tabs(char *dest, long from, long to)
{
    long	n = 0;

    while (from < to) {
	dest[n++] = '\t';
	from += 8;
    }

    return n;
}

static long print_res(char *dest, char *name, char *set_val, char *def_val)
{
    long	len;

    if (set_val)
	len = 0;
    else {
	dest[0] = '!';
	dest[1] = ' ';
	len = 2;
	if (def_val)
	    set_val = def_val;
	else
	    set_val = "";
    }

    strcpy(dest + len, name);
    len = strlen(dest);
    dest[len++] = ':';

    len += print_tabs(dest + len, len, 32);

    strcpy(dest + len, set_val);
    len += strlen(dest + len);

    dest[len++] = '\n';
    dest[len] = '\0';

    return len;
}

static long print_bool(char *dest, char *name, char *set_val, int def_val)
{
    return print_res(dest, name, set_val, def_val ? "True" : "False");
}

static void get_default_database(char *str, long len)
{
    int	is_config_server;

    is_config_server =
	global.config_nntp_server && global.nntp_server &&
	strcmp(global.config_nntp_server, global.nntp_server) == 0;

    strcpy(str,
	   "! Automatically generated knews config file.  Rows beginning\n"
	   "! with ! are comments.  Below are some commented-out defaults.\n"
	   "! Change and uncomment some of them if you wish.\n"
	   "!\n"
	   "! The environment variables below are only for illustrative\n"
	   "! purposes; they won't work here since there is no shell to\n"
	   "! expand them.  On the other hand, knews will expand ~ file\n"
	   "! names in most cases, but not in #include's, since it's Xlib\n"
	   "! that handles those.\n"
	   "\n"
	   "\n");
    str += strlen(str);

    if (is_config_server) {
	str += print_res(str, "newsrcFile", "~/.newsrc",
			 per_server_default.str.newsrc_file);
	str += print_res(str, "oldNewsrcFile", "~/.oldnewsrc",
			 per_server_default.str.old_newsrc_file);
    } else {
	str += print_res(str, "newsrcFile", global.newsrc_templ,
			 per_server_default.str.newsrc_file);
	str += print_res(str, "oldNewsrcFile", global.old_newsrc_templ,
			 per_server_default.str.old_newsrc_file);
    }

    str += print_res(str, "killFile", NULL, "~/.kill-%s");
    str += print_bool(str, "readActiveFile", global.read_active_file,
		      per_server_default.conv.read_active_file);
    str += print_bool(str, "retrieveDescriptions", global.retrieve_descr,
		      per_server_default.conv.retrieve_descriptions);
    str += print_bool(str, "fillNewsrcFile", global.fill_newsrc_file,
		      per_server_default.conv.fill_newsrc_file);
    str += print_res(str, "tryListActive", NULL, "True");
    str += print_bool(str, "checkForNewGroups", global.check_for_new_groups,
		      per_server_default.conv.check_for_new_groups);
    str += print_res(str, "rescanTimeout", NULL, "60");
    str += print_res(str, "askHowMany", NULL, "False");
    str += print_res(str, "postingAgent",
		     is_config_server ? global.config_posting_agent : NULL,
		     NULL);
    str += print_res(str, "threadAheadGroups", NULL,
		     "white space separated list of groups");
    str += print_res(str, "cacheDir", NULL, per_server_default.str.cache_dir);
    str += print_res(str, "descriptionsFile", NULL,
		     per_server_default.str.descriptions_file);
    str += print_res(str, "saveThreadInfo", NULL, "False");
    str += print_res(str, "groupNameColumns", NULL, "42");

    strcpy(str,
	   "\n"
	   "\n"
	   "! The resources below may be set on a per-group basis.\n"
	   "\n");
    str += strlen(str);

    str += print_res(str, "*keepThreadInfo",
		     global.keep_thread_info, "False");
    str += print_res(str, "*sortThreads", NULL, "none");
    str += print_res(str, "*headerFormat", NULL,
		     "Subject:Newsgroups:Followup-To:Reply-To:\\\n"
		     "!		Content-Description:Date:Organization:From:");
    str += print_res(str, "*quoteRegexp", NULL, "^[ 	]*[:>|]");
    str += print_res(str, "*quoteString", NULL, "> ");
    str += print_res(str, "*quotequoteString", NULL, ">");
    str += print_bool(str, "*showNumberLines", global.show_number_lines,
		      per_group_default.conv.show_number_lines);
    str += print_res(str, "*attribution", NULL, 
		     "In article %m,\\n	%f writes:");
    str += print_res(str, "*signatureFile", NULL, "~/.signature");
    str += print_res(str, "*organization", NULL,
		     "${NEWSORG:-${ORGANIZATION}}");
    str += print_res(str, "*fullName", NULL, "$NAME");
    str += print_res(str, "*replyTo", NULL, "$REPLYTO");
    str += print_res(str, "*distribution", NULL, "$DEFNEWSDIS");
    str += print_res(str, "*extraHeaders", NULL, NULL);
    str += print_res(str, "*followupHeaders", NULL, NULL);
    str += print_res(str, "*postedAndMailed", NULL,
		     per_group_default.str.posted_and_mailed);
    str += print_res(str, "*uuDir", NULL, "~/News");
    str += print_res(str, "*uuProgram", NULL, NULL);
    str += print_bool(str, "*expireKills", NULL,
		      per_group_default.conv.expire_kills);
    str += print_bool(str, "*assemblePartials", NULL,
		      per_group_default.conv.assemble_partials);
    str += print_res(str, "*defaultCharset", NULL, "us-ascii");
    str += print_res(str, "*cacheAheadSize", NULL, "0");
    str += print_res(str, "*cacheTrailSize", NULL, "0");
    str += print_res(str, "*groupNameColumns", NULL, "56");

    strcpy(str,
	   "\n"
	   "! Here are a few examples to illustrate bindings.  '*' is\n"
	   "! a loose binding, i.e. it matches any number of components\n"
	   "! of a group name.\n"
	   "\n"
	   "! swnet*signatureFile:             ~/.signature-svensk\n"
	   "! de*signatureFile:                ~/.signature-deutsch\n"
	   "! *linux*signatureFile:            ~/.signature-linux\n");
    str += strlen(str);
}

static XrmDatabase load_database(char *path)
{
    FILE	*fp = NULL;
    XrmDatabase	db = 0;
    struct stat	stat_buf;
    int		fd;

    if (path)
	if (stat(path, &stat_buf) < 0 ||
	    (fd = open(path, O_RDONLY)) < 0)
	    if (errno != ENOENT)
		perror(path);
	    else {
		fprintf(stderr, "Knews: creating config file %s\n", path);
		fd = open_mkdir(path, O_WRONLY|O_TRUNC|O_EXCL|O_CREAT, True);
		if (fd >= 0 && !(fp = fdopen(fd, "w"))) {
		    perror(path);
		    close(fd);
		}
		popup_title_notice(fp ? "Created config file" :
				   "Failed to create config file",
				   path, fp == NULL);
	    }
	else {
	    char	*buffer;

	    global.last_time = stat_buf.st_atime;
	    buffer = snarf_file(fd, NULL);
	    if (buffer) {
		db = XrmGetStringDatabase(buffer);
		free(buffer);
	    }

	    if (global.bogus_file_system)
		if (utime(path, NULL) < 0)
		    perror("utime");

	    close(fd);
	}

    if (!db) {
	char	default_db[4096];

	get_default_database(default_db, sizeof default_db);
	if (fp) {
	    fputs(default_db, fp);
	    if (fclose(fp) != 0)
		perror(path);
	}
	db = XrmGetStringDatabase(default_db);
	if (!db)
	    db = XrmGetStringDatabase("");
	if (!db)
	    fputs("Knews: Failed to create empty config database! "
		  "This ain't happning!\n", stderr);
    }

    return db;
}

static XrmQuark *quarkify(char *str, int cap)
{
    int		n, n_alloc = 16;
    XrmQuark	*res;
    char	*c;

    n_alloc = 16;
    res = (XrmQuark *)XtMalloc(n_alloc * sizeof res[0]);
    n = 0;
    do {
	int	ch = *str;

	if (n + 4 > n_alloc) {
	    n_alloc *= 2;
	    res = (XrmQuark *)XtRealloc((char *)res, n_alloc * sizeof res[0]);
	}

	c = strchr(str, '.');
	if (c)
	    *c = '\0';
	if (cap && IS_UPPER(ch))
	    *str = TO_LOWER(ch);

	res[n++] = XrmStringToQuark(str);

	if (cap)
	    *str = ch;
	if (c)
	    *c++ = '.';
	str = c;
    } while (str);

    return res;
}

void res_initialize(void)
{
    char	*tmp;
    int		i;

    for (i = 0 ; i < XtNumber(per_server_desc) ; i++) {
	per_server_desc[i].name.q =
	    XrmPermStringToQuark(per_server_desc[i].name.str);
	per_server_desc[i].class.q =
	    XrmPermStringToQuark(per_server_desc[i].class.str);
    }

    for (i = 0 ; i < XtNumber(per_group_desc) ; i++) {
	per_group_desc[i].name.q =
	    XrmPermStringToQuark(per_group_desc[i].name.str);
	per_group_desc[i].class.q =
	    XrmPermStringToQuark(per_group_desc[i].class.str);
    }

    if (regcomp(&default_quote_re, DEFAULT_QUOTE_REGEXP,
		REGEXP_COMPILE_FLAGS & ~REG_NOSUB) != 0)
	fputs(bad_regex_msg, stderr);

    per_server_default.str.newsrc_file =
	global.newsrc_templ ? global.newsrc_templ : "~/.newsrc-%s";
    per_server_default.str.old_newsrc_file =
	global.old_newsrc_templ ? global.old_newsrc_templ : "~/.oldnewsrc-%s";
    per_server_default.str.kill_file =
	global.kill_file_templ ? global.kill_file_templ : "~/.kill-%s";
    per_server_default.str.retrieve_descriptions =
	global.retrieve_descr;
    per_server_default.str.read_active_file      =
	global.read_active_file;
    per_server_default.str.fill_newsrc_file      =
	global.fill_newsrc_file;
    per_server_default.str.check_for_new_groups  =
	global.check_for_new_groups;
    per_server_default.str.cache_dir = "~/.knews/cache-%s";

    per_server_default.conv.rescan_timeout         = 60;
    per_server_default.conv.retrieve_descriptions  = True;
    per_server_default.conv.check_for_new_groups   = True;
    per_server_default.conv.ask_how_many           = False;
    per_server_default.conv.read_active_file       = True;
    per_server_default.conv.fill_newsrc_file       = False;
    per_server_default.conv.try_list_active        = True;
    per_server_default.conv.save_thread_info	   = False;
    per_server_default.conv.group_name_columns     = 42;

    per_group_default.str.quote_string        = "> ";
    per_group_default.str.quote_quote_string  = ">";
    per_group_default.str.attribution         = DEFAULT_ATTRIBUTION;
    per_group_default.str.signature_file      = "~/.signature";
    per_group_default.str.uu_dir              = "~/News";
    per_group_default.str.posted_and_mailed   = "[Posted and mailed]";
    per_group_default.str.show_number_lines =
	global.show_number_lines;
    per_group_default.str.keep_thread_info =
	global.keep_thread_info;

    per_group_default.conv.full_header       = False;
    per_group_default.conv.process_xrefs     = True;
    per_group_default.conv.show_number_lines = False;
    per_group_default.conv.header_format     = default_header_format;
    per_group_default.conv.keep_thread_info  = KeepTypeNo;
    per_group_default.conv.expire_kills      = True;
    per_group_default.conv.assemble_partials = True;
    per_group_default.conv.quote_regexp      = &default_quote_re;
    per_group_default.conv.subject_columns   = 56;

    tmp = getenv("NAME");
    if (tmp)
	tmp = XtNewString(tmp);
    else
	tmp = pw_name;
    per_group_default.str.full_name = tmp;

    tmp = getenv("NEWSORG");
    if (!tmp)
	tmp = getenv("ORGANIZATION");
    if (tmp)
	tmp = XtNewString(tmp);
    per_group_default.str.organization = tmp;

    tmp = getenv("REPLYTO");
    if (tmp)
	tmp = XtNewString(tmp);
    per_group_default.str.reply_to = tmp;

    tmp = getenv("DEFNEWSDIS");
    if (tmp)
	tmp = XtNewString(tmp);
    per_group_default.str.distribution = tmp;

    per_server = per_server_default;
    per_group  = per_group_default;
}

void res_set_pw_name(char *name)
{
    if (name)
	pw_name = XtNewString(name);
}

int res_load(char *path)
{
    XrmQuark	name[2];
    XrmQuark	class[2];
    XrmQuark	rep;
    int		i;

    clear_per_group();
    clear_per_server();
    if (db)
	XrmDestroyDatabase(db);

    global.last_time = 0;
    db = load_database(path);
    if (!db)
	return -1;

    name[1] = class[1] = 0;

    for (i = 0 ; i < XtNumber(per_server_desc) ; i++) {
	XrmValue	val;

	name[0]  = per_server_desc[i].name.q;
	class[0] = per_server_desc[i].class.q;
	if (XrmQGetResource(db, name, class, &rep, &val) && val.addr)
	    *((char **)((char *)&per_server +
			per_server_desc[i].offset)) =
		(char *)val.addr;
    }

    per_server.conv.thread_ahead_groups =
	str_to_list(per_server.str.thread_ahead_groups, " \t");

    per_server.conv.rescan_timeout =
	str_to_long(per_server.str.rescan_timeout,
		    per_server.conv.rescan_timeout);
    per_server.conv.group_name_columns =
	str_to_long(per_server.str.group_name_columns,
		    per_server.conv.group_name_columns);

    per_server.conv.retrieve_descriptions =
	str_to_bool(per_server.str.retrieve_descriptions,
		    per_server.conv.retrieve_descriptions);
    per_server.conv.check_for_new_groups =
	str_to_bool(per_server.str.check_for_new_groups,
		    per_server.conv.check_for_new_groups);
    per_server.conv.read_active_file =
	str_to_bool(per_server.str.read_active_file,
		    per_server.conv.read_active_file);
    per_server.conv.ask_how_many =
	str_to_bool(per_server.str.ask_how_many,
		    per_server.conv.ask_how_many);
    per_server.conv.fill_newsrc_file =
	str_to_bool(per_server.str.fill_newsrc_file,
		    per_server.conv.fill_newsrc_file);
    per_server.conv.try_list_active =
	str_to_bool(per_server.str.try_list_active,
		    per_server.conv.try_list_active);
    per_server.conv.save_thread_info =
	str_to_bool(per_server.str.save_thread_info,
		    per_server.conv.save_thread_info);

    res_enter_group("none");

    return 0;
}

void res_enter_group(char *group)
{
    clear_per_group();

    if (db) {
	XrmQuark	*name;
	XrmQuark	*class;
	XrmHashTable	*list = NULL;
	int		n = 4;

	group = XtNewString(group);
	name  = quarkify(group, False);
	class = quarkify(group, True);

	do {
	    n *= 2;
	    XtFree((char *)list);
	    list = (XrmHashTable *)XtMalloc(n * sizeof list[0]);
	} while (!XrmQGetSearchList(db, name, class, list, n));

	for (n = 0 ; n < XtNumber(per_group_desc) ; n++) {
	    XrmValue	val;
	    XrmQuark	rep;

	    if (XrmQGetSearchResource(list,
				      per_group_desc[n].name.q,
				      per_group_desc[n].class.q,
				      &rep, &val) &&
		val.addr)
		*((char **)((char *)&per_group +
			    per_group_desc[n].offset)) = (char *)val.addr;
	}

	XtFree((char *)list);
	XtFree((char *)name);
	XtFree((char *)class);
	XtFree(group);
	group = NULL;
    }

    if (per_group.str.header_format)
	per_group.conv.header_format =
	    str_to_list(per_group.str.header_format, " \t:");

    if (per_group.str.quote_regexp)
	if (regcomp(&quote_re, per_group.str.quote_regexp,
		    REGEXP_COMPILE_FLAGS & ~REG_NOSUB) == 0)
	    per_group.conv.quote_regexp = &quote_re;
	else
	    fprintf(stderr, "Warning: failed to compile regexp \"%s\".\n",
		    per_group.str.quote_regexp);

    per_group.conv.full_header =
	str_to_bool(per_group.str.full_header, per_group.conv.full_header);
    per_group.conv.process_xrefs =
	str_to_bool(per_group.str.process_xrefs, per_group.conv.process_xrefs);
    per_group.conv.show_number_lines =
	str_to_bool(per_group.str.show_number_lines,
		    per_group.conv.show_number_lines);
    per_group.conv.keep_thread_info =
	str_to_keep(per_group.str.keep_thread_info,
		    per_group.conv.keep_thread_info);
    per_group.conv.assemble_partials =
	str_to_bool(per_group.str.assemble_partials,
		    per_group.conv.assemble_partials);
    per_group.conv.cache_ahead_size =
	str_to_long(per_group.str.cache_ahead_size,
		    per_group.conv.cache_ahead_size);
    per_group.conv.cache_trail_size =
	str_to_long(per_group.str.cache_trail_size,
		    per_group.conv.cache_trail_size);
    per_group.conv.subject_columns =
	str_to_long(per_group.str.subject_columns,
		    per_group.conv.subject_columns);
}

#define PER_SERVER_STR_FUNC(field) \
char *res_##field(void)            \
{                                  \
    return per_server.str.field;   \
}

PER_SERVER_STR_FUNC(newsrc_file)
PER_SERVER_STR_FUNC(old_newsrc_file)
PER_SERVER_STR_FUNC(kill_file)
PER_SERVER_STR_FUNC(remote_newsrc_file)
PER_SERVER_STR_FUNC(remote_kill_file)
PER_SERVER_STR_FUNC(auth_info_user)
PER_SERVER_STR_FUNC(auth_info_pass)
PER_SERVER_STR_FUNC(posting_agent)
PER_SERVER_STR_FUNC(cache_dir)
PER_SERVER_STR_FUNC(descriptions_file)

#define PER_SERVER_FUNC(type, field) \
type res_##field(void)               \
{                                    \
    return per_server.conv.field;    \
}

PER_SERVER_FUNC(char**, thread_ahead_groups)
PER_SERVER_FUNC(long,   rescan_timeout)
PER_SERVER_FUNC(int,    retrieve_descriptions)
PER_SERVER_FUNC(int,    check_for_new_groups)
PER_SERVER_FUNC(int,    read_active_file)
PER_SERVER_FUNC(int,    ask_how_many)
PER_SERVER_FUNC(int,    fill_newsrc_file)
PER_SERVER_FUNC(int,    try_list_active)
PER_SERVER_FUNC(int,	save_thread_info)
PER_SERVER_FUNC(int,	group_name_columns)

#define PER_GROUP_STR_FUNC(field) \
char *res_##field(void)           \
{                                 \
    return per_group.str.field;   \
}

PER_GROUP_STR_FUNC(quote_string)
PER_GROUP_STR_FUNC(quote_quote_string)
PER_GROUP_STR_FUNC(attribution)
PER_GROUP_STR_FUNC(full_name)
PER_GROUP_STR_FUNC(signature_file)
PER_GROUP_STR_FUNC(organization)
PER_GROUP_STR_FUNC(reply_to)
PER_GROUP_STR_FUNC(extra_headers)
PER_GROUP_STR_FUNC(followup_headers)
PER_GROUP_STR_FUNC(posted_and_mailed)
PER_GROUP_STR_FUNC(uu_dir)
PER_GROUP_STR_FUNC(uu_program)
PER_GROUP_STR_FUNC(distribution)
PER_GROUP_STR_FUNC(sort_threads)
PER_GROUP_STR_FUNC(default_charset)

#define PER_GROUP_FUNC(type, field) \
type res_##field(void)              \
{                                   \
    return per_group.conv.field;    \
}

PER_GROUP_FUNC(char**,   header_format)
PER_GROUP_FUNC(int,      full_header)
PER_GROUP_FUNC(int,      process_xrefs)
PER_GROUP_FUNC(int,      show_number_lines)
PER_GROUP_FUNC(int,      expire_kills)
PER_GROUP_FUNC(regex_t*, quote_regexp)
PER_GROUP_FUNC(int,      assemble_partials)
PER_GROUP_FUNC(int,      cache_ahead_size)
PER_GROUP_FUNC(int,      cache_trail_size)
PER_GROUP_FUNC(int,      subject_columns) 

int res_keep_thread_info(int subscribed)
{
    KeepType	keep = per_group.conv.keep_thread_info;

    if (keep == KeepTypeNo)
	return False;
    if (keep == KeepTypeYes)
	return True;
    return subscribed;
}

void res_set_keep_thread_info(int keep)
{
    per_group.conv.keep_thread_info = keep ? KeepTypeYes : KeepTypeNo;
}

void res_set_full_header(int full)
{
    per_group.conv.full_header = full;
}

void res_set_ask_how_many(int ask)
{
    per_server.conv.ask_how_many = ask;
}

void res_set_default_charset(char *charset)
{
    per_group.str.default_charset = charset;
}
