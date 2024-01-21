/* ====================================================================
 * Copyright (c) 1996 The Apache Group.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * 4. The names "Apache Server" and "Apache Group" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission.
 *
 * 5. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * THIS SOFTWARE IS PROVIDED BY THE APACHE GROUP ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
 * IT'S CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Group and was originally based
 * on public domain software written at the National Center for
 * Supercomputing Applications, University of Illinois, Urbana-Champaign.
 * For more information on the Apache Group and the Apache HTTP server
 * project, please see <http://www.apache.org/>.
 *
 */


/*
 * mod_conv.c: Handles the on-the-fly html index generation with ftp functions
 * 
 * Jakub Jelinek (jj@sunsite.mff.cuni.cz)
 * Version 0.2
 * Based on mod_dir by Rob McCool and others
 * 
 */

#include "httpd.h"
#include "http_config.h"
#include "http_core.h"
#include "http_request.h"
#include "http_protocol.h"
#include "http_log.h"
#include "http_main.h"
#include "util_script.h"
#if defined (HAVE_RX_H) && defined (HAVE_REGCOMP)
#include <rx.h>
#else
#include "regex.h"
#endif

#include <stdarg.h>

module conv_module;

#define FTP_MAGIC_TYPE "httpd/ftp-directory"

#define CONVICONS 1
#define FORCEINDEX 2
#define TABLES    128

/****************************************************************
 *
 * Handling configuration directives...
 */

struct item {
    char *type;
    char *apply_to;
    char *apply_path;
    char *data;
};

typedef struct {
    char *real;
    char *fake;
} alias_entry;

#define TABLEALLOW 0
#define TABLEDENY  1
#define JAVAALLOW  2
#define JAVADENY   3
typedef struct ftp_config_struct {

    char *default_icon;
    char *index_names;
    char *prolog;
    char *epilog;
    char *dirpath_icon, *dircurrent_icon, *dirspace_icon, *conv_icon;
    char *title, *convtitle, *search_filename, *hidesymlink;
    char *tar_path, *gzip_path, *compress_path, *zip_path;
    char *xferlog;
    
    array_header *allowdeny[4];
  
    array_header *icon_list, *ign_list;
    array_header *rdme_list, *opts_list;
  
} ftp_config_rec;

char c_by_encoding, c_by_type, c_by_path;

#define BY_ENCODING &c_by_encoding
#define BY_TYPE &c_by_type
#define BY_PATH &c_by_path

static void push_item(array_header *arr, char *type, char *to, char *path, char *data)
{
    struct item *p = (struct item *)push_array(arr);

    if (!to) to = "";
    if (!path) path = "";
    
    p->type = type;
    p->data = data ? pstrdup(arr->pool, data): NULL;
    p->apply_path = pstrcat(arr->pool, path, "*", NULL);
    
    if((type == BY_PATH) && (!is_matchexp(to)))
        p->apply_to = pstrcat (arr->pool, "*", to, NULL);
    else if (to)
        p->apply_to = pstrdup (arr->pool, to);
    else
        p->apply_to = NULL;
}

static char *add_icon(cmd_parms *cmd, void *d, char *icon, char *to)
{
    if(cmd->info == BY_PATH) 
        if(!strcmp(to,"**DIRECTORY**"))
            to = "^^DIRECTORY^^";

    push_item(((ftp_config_rec *)d)->icon_list, cmd->info, to, cmd->path,
	      icon);
    return NULL;
}

static char *add_ignore(cmd_parms *cmd, void *d, char *ext) {
    push_item(((ftp_config_rec *)d)->ign_list, 0, ext, cmd->path, NULL);
    return NULL;
}

static char *add_readme(cmd_parms *cmd, void *d, char *name) {
    push_item(((ftp_config_rec *)d)->rdme_list, 0, NULL, cmd->path, name);
    return NULL;
}

static char *add_opts_int(cmd_parms *cmd, void *d, int opts) {
    push_item(((ftp_config_rec *)d)->opts_list, (char*)opts, NULL,
	      cmd->path, NULL);
    return NULL;
}

static char *add_allowdeny(cmd_parms *cmd, void *d, char *name)
{
    char **p;
    
    p = (char **)push_array(((ftp_config_rec *)d)->allowdeny [(int)cmd->info]);
    *p = pstrdup (((ftp_config_rec *)d)->allowdeny [(int)cmd->info]->pool, name);
    return NULL;
}

char *add_ftp_alias(cmd_parms *cmd, void *dummy, char *f, char *r)
{
    server_rec *s = cmd->server;
    array_header *aliases =
        (array_header *)get_module_config(s->module_config,&conv_module);
    alias_entry *new = push_array (aliases);

    new->fake = f; new->real = r;
    return NULL;
}

static char *add_opts(cmd_parms *cmd, void *d, char *optstr) {
    char *w;
    int opts = 0;
    while(optstr[0]) {
        w = getword_conf(cmd->pool, &optstr);
        if(!strcasecmp(w,"ConversionIcons"))
            opts |= CONVICONS;
        else if(!strcasecmp(w,"ForceIndex"))
            opts |= FORCEINDEX;
        else if(!strcasecmp(w,"None"))
            opts = 0;
	else
	    return "Invalid directory indexing option";
    }
    return add_opts_int(cmd, d, opts);
}

#define DIR_CMD_PERMS OR_INDEXES

command_rec ftp_cmds[] = {
{ "ConvAlias", add_ftp_alias, NULL, RSRC_CONF, TAKE2,
    "a fakename and a realname"},
{ "ConvIcon", add_icon, BY_PATH, DIR_CMD_PERMS, ITERATE2,
    "an icon URL followed by one or more filenames" },
{ "ConvIconByType", add_icon, BY_TYPE, DIR_CMD_PERMS, ITERATE2,
    "an icon URL followed by one or more MIME types" },
{ "ConvIconByEncoding", add_icon, BY_ENCODING, DIR_CMD_PERMS, ITERATE2,
    "an icon URL followed by one or more content encodings" },
{ "ConvDefaultIcon", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, default_icon),
    DIR_CMD_PERMS, TAKE1, "an icon URL"},
{ "ConvConversionIcon", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, conv_icon),
    DIR_CMD_PERMS, TAKE1, "an icon URL"},
{ "ConvDirPathIcon", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, dirpath_icon),
    DIR_CMD_PERMS, TAKE1, "an icon URL"},
{ "ConvDirCurrentIcon", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, dircurrent_icon),
    DIR_CMD_PERMS, TAKE1, "an icon URL"},
{ "ConvDirSpaceIcon", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, dirspace_icon),
    DIR_CMD_PERMS, TAKE1, "an icon URL"},
{ "ConvIndexOptions", add_opts, NULL, DIR_CMD_PERMS, RAW_ARGS,
    "one or more index options" },
{ "ConvIgnore", add_ignore, NULL, DIR_CMD_PERMS, ITERATE,
    "one or more file extensions" },
{ "ConvProlog", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, prolog),
    DIR_CMD_PERMS, TAKE1, "a html prolog URL (local only)"},
{ "ConvEpilog", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, epilog),
    DIR_CMD_PERMS, TAKE1, "a html epilog URL (local only)"},
{ "ConvMessage", add_readme, NULL, DIR_CMD_PERMS, TAKE1, "a message URL (local only)" },
{ "ConvDirIndex", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, index_names),
    DIR_CMD_PERMS, RAW_ARGS, "a list of filenames or None" },
{ "ConvSearch", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, search_filename),
    DIR_CMD_PERMS, RAW_ARGS, "a html search box URL (local only)" },
{ "ConvTitle", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, title),
    DIR_CMD_PERMS, RAW_ARGS, "a short descriptive name (%s will become dirname)" },
{ "ConvConversionTitle", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, convtitle),
    DIR_CMD_PERMS, RAW_ARGS, "a short descriptive name (%s will become filename)" },
{ "TableAllow", add_allowdeny, (char *)TABLEALLOW, DIR_CMD_PERMS, RAW_ARGS,
    "user agent regular expression" },
{ "TableDeny", add_allowdeny, (char *)TABLEDENY, DIR_CMD_PERMS, RAW_ARGS,
    "user agent regular expression" },
{ "JavaAllow", add_allowdeny, (char *)JAVAALLOW, DIR_CMD_PERMS, RAW_ARGS,
    "user agent regular expression" },
{ "JavaDeny", add_allowdeny, (char *)JAVADENY, DIR_CMD_PERMS, RAW_ARGS,
    "user agent regular expression" },
{ "ConvHideSymlink", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, hidesymlink),
    DIR_CMD_PERMS, RAW_ARGS, "regular expression of symlinks shown as regular files" },
{ "Xferlog", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, xferlog),
    DIR_CMD_PERMS, RAW_ARGS, "a full path to executable or None" },
{ "Tar", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, tar_path),
    DIR_CMD_PERMS, RAW_ARGS, "a full path to executable or None" },
{ "Gzip", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, gzip_path),
    DIR_CMD_PERMS, RAW_ARGS, "a full path to executable or None" },
{ "Compress", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, compress_path),
    DIR_CMD_PERMS, RAW_ARGS, "a full path to executable or None" },
{ "Zip", set_string_slot,
    (void*)XtOffsetOf(ftp_config_rec, zip_path),
    DIR_CMD_PERMS, RAW_ARGS, "a full path to executable or None" },
{ NULL }
};

void *create_ftp_config (pool *p, char *dummy)
{
    ftp_config_rec *new =
        (ftp_config_rec *) pcalloc (p, sizeof(ftp_config_rec));

    new->icon_list = make_array (p, 4, sizeof (struct item));
    new->ign_list = make_array (p, 4, sizeof (struct item));
    new->rdme_list = make_array (p, 4, sizeof (struct item));
    new->opts_list = make_array (p, 4, sizeof (struct item));
    new->allowdeny [TABLEALLOW] = make_array (p, 4, sizeof (char *));
    new->allowdeny [TABLEDENY] = make_array (p, 4, sizeof (char *));
    new->allowdeny [JAVAALLOW] = make_array (p, 4, sizeof (char *));
    new->allowdeny [JAVADENY] = make_array (p, 4, sizeof (char *));
    
    return (void *)new;
}

#define mfc(a) new-> a = add-> a ? add-> a : base-> a ;

void *merge_ftp_configs (pool *p, void *basev, void *addv)
{
    ftp_config_rec *new=(ftp_config_rec*)pcalloc (p, sizeof(ftp_config_rec));
    ftp_config_rec *base = (ftp_config_rec *)basev;
    ftp_config_rec *add = (ftp_config_rec *)addv;

    mfc(default_icon)
    mfc(conv_icon)
    mfc(dirpath_icon)
    mfc(dircurrent_icon)
    mfc(dirspace_icon)
    mfc(index_names)
    mfc(prolog)
    mfc(epilog)
    mfc(title)
    mfc(convtitle)
    mfc(search_filename)
    mfc(tar_path)
    mfc(gzip_path)
    mfc(compress_path)
    mfc(zip_path)
    mfc(xferlog)
    mfc(hidesymlink)

    new->ign_list = append_arrays (p, add->ign_list, base->ign_list);
    new->icon_list = append_arrays (p, add->icon_list, base->icon_list);
    new->rdme_list = append_arrays (p, add->rdme_list, base->rdme_list);
    new->opts_list = append_arrays (p, add->opts_list, base->opts_list);
    new->allowdeny [TABLEALLOW] = append_arrays (p, add->allowdeny [TABLEALLOW], base->allowdeny [TABLEALLOW]);
    new->allowdeny [TABLEDENY] = append_arrays (p, add->allowdeny [TABLEDENY], base->allowdeny [TABLEDENY]);
    new->allowdeny [JAVAALLOW] = append_arrays (p, add->allowdeny [JAVAALLOW], base->allowdeny [JAVAALLOW]);
    new->allowdeny [JAVADENY] = append_arrays (p, add->allowdeny [JAVADENY], base->allowdeny [JAVADENY]);
    
    return new;
}

void *create_ftp_server_config (pool *p, server_rec *s)
{
    return make_array (p, 20, sizeof(alias_entry));
}

void *merge_ftp_server_configs (pool *p, void *basev, void *overridesv)
{
    return append_arrays (p, (array_header *)overridesv, (array_header *)basev);
}



/****************************************************************
 *
 * Looking things up in config entries...
 */

/* Structure used to hold entries when we're actually building an index */

struct ent {
    char *name;
    char *linkname;
    char *icon;
    size_t size;
    mode_t mode;
    int checked;
    time_t lm;
    struct ent *next;
};

static char *find_item(request_rec *r, array_header *list, int path_only) {
    char *content_type = r->content_type;
    char *content_encoding = r->content_encoding;
    char *path = r->filename;

    struct item *items = (struct item *)list->elts;
    int i;

    for (i = 0; i < list->nelts; ++i) {
        struct item *p = &items[i];
      
        /* Special cased for ^^DIRECTORY^^ and ^^BLANKICON^^ */
        if((path[0] == '^') || (!strcmp_match(path,p->apply_path))) {
            if(!*(p->apply_to))
                return p->data;
            else if(p->type == BY_PATH || path[0] == '^') {
                if(!strcmp_match(path,p->apply_to))
                    return p->data;
            } else if(!path_only) {
                if(!content_encoding) {
                    if(p->type == BY_TYPE) {
                        if(!strcmp_match(content_type,p->apply_to))
                            return p->data;
                    }
                } else {
                    if(p->type == BY_ENCODING) {
                        if(!strcmp_match(content_encoding,p->apply_to))
                            return p->data;
                    }
                }
            }
        }
    }
    return NULL;
}

#define find_icon(d,p,t) find_item(p,d->icon_list,t)
#define find_readme(d,p) find_item(p,d->rdme_list,0)

static char *find_default_icon (ftp_config_rec *d, char *bogus_name)
{
    request_rec r;

    /* Bleah.  I tried to clean up find_item, and it lead to this bit
     * of ugliness.   Note that the fields initialized are precisely
     * those that find_item looks at...
     */
    
    r.filename = bogus_name;
    r.content_type = r.content_encoding = NULL;

    return find_item (&r, d->icon_list, 1);
}

static int ignore_entry(ftp_config_rec *d, char *path) {
    array_header *list = d->ign_list;
    struct item *items = (struct item *)list->elts;
    char *tt;
    int i;

    if((tt=strrchr(path,'/')) == NULL)
      tt=path;
    else {
      tt++;
    }

    for (i = 0; i < list->nelts; ++i) {
        struct item *p = &items[i];
	char *ap;

	if((ap=strrchr(p->apply_to,'/')) == NULL)
	  ap=p->apply_to;
	else
	  ap++;

        if(!strcmp_match(path,p->apply_path) && !strcmp_match(tt,ap))
	   return 1;
    }
    return 0;
}

static int find_opts(ftp_config_rec *d, request_rec *r) {
    char *path = r->filename;
    array_header *list = d->opts_list;
    struct item *items = (struct item *)list->elts;
    int i;

    for (i = 0; i < list->nelts; ++i) {
        struct item *p = &items[i];
	
        if(!strcmp_match(path,p->apply_path))
            return (int)p->type;
    }
    return 0;
}

/*****************************************************************
 *
 * Actually generating output
 */

static void send_fd_replacing(FILE *f, request_rec *r, char *string)
{
    char buf[IOBUFSIZE];
    register int i, isreplace = 0;
    conn_rec *c = r->connection;
                
    while (!c->aborted) {
        i = getc (f);
        if (ferror(f) && errno == EINTR)
            continue;
        if (feof(f) || i == -1)
            break;
        if (i == '%') {
            if (isreplace)
                rputc('%',r);
            else
                isreplace = 1;
            continue;
        } else if (isreplace) {
            isreplace = 0;
            if (i == 's') {
                rprintf(r,string);
                continue;
            } else {
                rputc('%',r);
            }
        }
        rputc(i,r);
    }
    fflush(c->client);
}

long send_fd_len(FILE *f, request_rec *r, long len)
{
    char buf[IOBUFSIZE];
    long total_bytes_sent;
    register int n,o,w;
    conn_rec *c = r->connection;
    
    total_bytes_sent = 0;
    while (!r->connection->aborted) {
        n = len;
        if (n > IOBUFSIZE) n = IOBUFSIZE;
        while ((n= fread(buf, sizeof(char), IOBUFSIZE, f)) < 1
	       && ferror(f) && errno == EINTR)
	    continue;
	
	if (n < 1) {
            break;
        }
        o=0;
        if (r->bytes_sent != -1) r->bytes_sent += n;
	total_bytes_sent += n;
	len -= n;
	
        while(n && !r->connection->aborted) {
            w=fwrite(&buf[o],sizeof(char),n,c->client);
            n-=w;
            o+=w;
        }
    }
    fflush(c->client);
    
    return total_bytes_sent;
}

            
static int insert_readme(char *name, char *readme_fname, int rule, char *replace, request_rec *r) {
    char *fn;
    FILE *f;
    struct stat finfo;
    int plaintext=0;
    int i;
    request_rec *rr = sub_req_lookup_uri (readme_fname, r);

    if (!rr->finfo.st_mode) {
        destroy_sub_req (rr);
        return 0;
    }
    fn = pstrdup(r->pool,rr->filename);
    destroy_sub_req (rr);
    
    i = strlen (fn);
    if (i <= 5 || strcmp (fn + i - 5, ".html")) {
        fn = pstrcat(r->pool, fn, ".html", NULL);
        if(stat(fn,&finfo) == -1) {
            /* A brief fake multiviews search for README.html */
            fn[strlen(fn)-5] = '\0';
            if(stat(fn,&finfo) == -1)
                return 0;
            plaintext=1;
            if(rule) rprintf(r,"<HR>%c",LF);
            rprintf(r,"<PRE>%c",LF);
        }
        else if(rule) rprintf(r,"<HR>%c",LF);
    } else if(rule) rprintf(r,"<HR>%c",LF);
    if(!(f = pfopen(r->pool,fn,"r")))
        return 0;
    if (replace)
        send_fd_replacing(f, r, replace);
    else
        send_fd(f, r);
    pfclose(r->pool, f);
    if(plaintext)
        rprintf(r,"</PRE>%c",LF);
    return 1;
}


#ifdef NOTDEF

/* The only call to this function anyplace is commented out in the base
 * code below.  It could be fixed along the same lines as the shell_escape
 * stuff in util.c, but for now, why bother?
 */

void escape_html(char *fn) {
    register int x,y;
    char copy[MAX_STRING_LEN];

    strcpy(copy,fn);
    for(x=0,y=0;copy[y];x++,y++) {
        if(copy[y] == '<') {
            strcpy(&fn[x],"&lt;");
            x+=3;
        }
        else if(copy[y] == '>') {
            strcpy(&fn[x],"&gt;");
            x+=3;
        }
        else if(copy[y] == '&') {
            strcpy(&fn[x],"&amp;");
            x+=4;
        }
        else
            fn[x] = copy[y];
    }
    fn[x] = '\0';
}
#endif

static struct ent *make_ftp_entry(char *name, int ftp_opts,
			          ftp_config_rec *d, request_rec *r,
			          regex_t *rreg, char *dirname)
{
    struct ent *p;
    request_rec *rr;
    struct stat mystat;
    char linkname[HUGE_STRING_LEN];

    if((name[0] == '.') && ((!name[1]) || (name[1] == '.' && !name[2])))
        return(NULL);

    if (ignore_entry(d, make_full_path (r->pool, dirname, name)))
        return(NULL);

    p=(struct ent *)pcalloc(r->pool, sizeof(struct ent));
    p->name = pstrdup (r->pool, name);
    p->size = 0;
    p->checked = 0;
    p->icon = NULL;
    p->lm = -1;
    p->linkname = NULL;

    rr = sub_req_lookup_file (name, r);
	
    if (rr->finfo.st_mode != 0) {
        p->lm = rr->finfo.st_mtime;
        if(S_ISDIR(rr->finfo.st_mode)) {
            if(!(p->icon = find_icon(d,rr,1)))
                p->icon = find_default_icon(d,"^^DIRECTORY^^");
            p->size = -1;
        } else {
            p->icon = find_icon(d, rr, 0);
            p->size = rr->finfo.st_size;
        }
        p->mode = rr->finfo.st_mode; 
        if (lstat (rr->filename, &mystat) >= 0) {
            int i;
            
            if (S_ISLNK (mystat.st_mode)) {
                i = readlink (rr->filename, linkname, HUGE_STRING_LEN);
                if (i > 0) {
                    linkname [i] = 0;
                    if (!rreg || regexec (rreg, linkname, 0, NULL, 0))
                        p->linkname = pstrdup (r->pool, linkname);
                }
            }
        }
        
    }
    destroy_sub_req (rr);
    return(p);
}

static void output_directories(struct ent **ar, int n,
			       ftp_config_rec *d, request_rec *r, 
			       int ftp_opts, char *forcestr)
{
    int x;
    char timebuf[40];
    time_t current_time = time(NULL);
    char *name = r->uri;
    char *tp;
    int tables = ftp_opts & TABLES;
    
    if(name[0] == '\0') name = "/";

    for(x=0;x<n;x++) {
	char *anchor = NULL, *t = NULL, *t2 = NULL;
	
        rprintf(r,"%s<IMG SRC=\"%s\" ALIGN=CENTER>", tables ? "<TR><TD NOWRAP>" : "", ar[x]->icon ? ar[x]->icon : d->default_icon);
        rprintf(r,"%s<INPUT TYPE=CHECKBOX NAME=file VALUE=\"%s\"%s>%s",
            tables ? "<TD NOWRAP>" : " ", ar[x]->name, ar[x]->checked ? " checked" : "",
            tables ? "<TD NOWRAP>" : " ");
        if (tables)
            rprintf(r,"<A HREF=\"%s%s%s\">%s</A><TD NOWRAP ALIGN=RIGHT>", ar[x]->name, ar[x]->size == -1 ? "/" : "", ar[x]->size == -1 ? forcestr : "", ar[x]->name);
        else if (strlen (ar[x]->name) < 20)
            rprintf(r,"<A HREF=\"%s%s%s\">%s</A> %*s", ar[x]->name, ar[x]->size == -1 ? "/" : "", ar[x]->size == -1 ? forcestr : "", ar[x]->name, 20 - strlen (ar[x]->name), "");
        else
            rprintf(r,"<A HREF=\"%s%s%s\">%.19s&gt;</A> ", ar[x]->name, ar[x]->size == -1 ? "/" : "", ar[x]->size == -1 ? forcestr : "", ar[x]->name);
        if (ar[x]->size != -1) {
            if (tables)
                rprintf(r,"%lu", ar[x]->size);
            else
                rprintf(r,"%10lu", ar[x]->size);
        } else if (!tables)
            rprintf(r,"%10s","");
        strcpy (timebuf, ctime (&ar[x]->lm));
        if (current_time > ar[x]->lm + 6L * 30L * 24L * 60L * 60L ||
            current_time < ar[x]->lm - 60L * 60L) {
            strcpy (timebuf + 11, timebuf + 19);
        }
        timebuf [16] = 0;
        rprintf(r,"%s%s%s", 
        	tables ? "<TD NOWRAP>" : " ",
                timebuf, 
                tables ? "<TD NOWRAP ALIGN=CENTER>" : " ");
        if (ftp_opts & CONVICONS)
            rprintf(r,"<A HREF=\"%s%s?C\"><IMG SRC=\"%s\" BORDER=0></A>%c", 
            	ar[x]->name, ar[x]->size == -1 ? "/" : "", d->conv_icon, LF);
        else
            rputc(LF, r);
    }
}

void rwx (unsigned short mode, char *str)                                         
{                                                                                 
    str [0] = (mode & S_IRUSR) ? 'r' : '-';                                       
    str [1] = (mode & S_IWUSR) ? 'w' : '-';                                       
    str [2] = (mode & S_IXUSR) ? 'x' : '-';                                       
}                                                                                 
                                                                                              
char *printmode (unsigned short mode, char *pbuf)                                             
{                                                                                 
#ifdef S_ISBLK                                                                    
    if (S_ISBLK (mode))                                                           
        pbuf [0] = 'b';                                                           
    else     
#endif                                                                            
    if (S_ISCHR (mode))                                                      
        pbuf [0] = 'c';                                                           
    else if (S_ISDIR (mode))                                                      
        pbuf [0] = 'd';                                                           
    else if (S_ISREG (mode))                                                      
        pbuf [0] = '-';                                                           
#ifdef S_ISFIFO                                                                   
    else if (S_ISFIFO (mode))                                                     
        pbuf [0] = 'p';                                                           
#endif                                                                            
#ifdef S_ISLNK                                                                    
    else if (S_ISLNK (mode))                                                      
        pbuf [0] = 'l';                                                           
#endif                                                                            
#ifdef S_ISSOCK                                                                   
    else if (S_ISSOCK (mode))                                                     
        pbuf [0] = 's';                                                           
#endif                                                                            
#ifdef S_ISMPC                                                                    
    else if (S_ISLNK (mode))                                                      
        pbuf [0] = 'm';                                                           
#endif                                                                            
#ifdef S_ISNWK                                                                    
    else if (S_ISNWK (mode))                                                      
        pbuf [0] = 'n';                                                           
#endif                                                                            
    else                                                                          
        pbuf [0] = '?';                                                           
    rwx ((mode & 0700) << 0, pbuf + 1);                                           
    rwx ((mode & 0070) << 3, pbuf + 4);                                           
    rwx ((mode & 0007) << 6, pbuf + 7);                                           
    pbuf [10] = 0;                                                                
#ifdef S_ISUID                                                                    
    if (mode & S_ISUID) {                                                         
        if (pbuf [3] != 'x')                                                      
            pbuf [3] = 'S';                                                       
        else
            pbuf [3] = 's';                                                       
    }                                                                             
#endif                                                                            
#ifdef S_ISGID                                                                    
    if (mode & S_ISGID) {                                                         
        if (pbuf [6] != 'x')                                                      
            pbuf [6] = 'S';                                                       
        else                                                                      
            pbuf [6] = 's';                                                       
    }                                                                             
#endif                                                                            
#ifdef S_ISVTX                                                                    
    if (mode & S_ISVTX) {                                                         
        if (pbuf [9] != 'x')                                                      
            pbuf [9] = 'T';                                                       
        else                                                                      
            pbuf [9] = 't';                                                       
    }                                                                             
#endif
    return pbuf;    
}

static void output_java_directories(struct ent **ar, int n,
			           ftp_config_rec *d, request_rec *r, int ftp_opts)
{
    int x;
    char timebuf[40];
    time_t current_time = time(NULL);
    char *name = r->uri;
    char *tp;
    char modebuf[12];
    
    if(name[0] == '\0') name = "/";

    for(x=0;x<n;x++) {
        strcpy (timebuf, ctime (&ar[x]->lm));
        if (current_time > ar[x]->lm + 6L * 30L * 24L * 60L * 60L ||
            current_time < ar[x]->lm - 60L * 60L) {
            strcpy (timebuf + 11, timebuf + 19);
        }
        timebuf [16] = 0;
        rprintf(r,"%s %s %lu %s %s",
                printmode(ar[x]->mode,modebuf),
                ar[x]->icon ? ar[x]->icon : d->default_icon,
                ar[x]->size == -1 ? 0 : ar[x]->size,
                timebuf,
                ar[x]->name);
        if (ar[x]->linkname)
            rprintf(r," -> %s%c", ar[x]->linkname, LF);
        else
            rputc(LF,r);
    }
}

static int dsortf(struct ent **s1,struct ent **s2)
{
    return(strcmp((*s1)->name,(*s2)->name));
}

/* params - resulting array, path_to_chdir, first_program path, args..., NULL, [ second_program path, args... ], NULL
   ftp_child will then do:
       if second program is not specified, it will just popen("first_program args...","r")
       otherwise popen("first_program args... | second_program args...","r")
   but without touching a shell (why should we bother)
 */
void ftp_pipe (char **argv, ...)
{
    va_list args;
    char *p;
    int i = 0;
    
    va_start (args, argv);
    while ((p = va_arg (args, char *)) != NULL)
        argv[i++] = p;
    argv[i++] = NULL;
    while ((p = va_arg (args, char *)) != NULL)
        argv[i++] = p;
    argv[i++] = NULL;
    va_end (args);    
}

void ftp_child (void *child_stuff)
{
    char **argv1 = (char **)child_stuff;
    int pipes[2];
    char *pname1, *pname2;
    char **argv2;
    
    if (chdir (*argv1++) < 0)
        exit (1);
    pname1 = *argv1++;
    for (argv2 = argv1; *argv2 != NULL; argv2++);
    argv2++;
    pname2 = *argv2++;
    
    if (pname2 == NULL) {
        close (2);
        open ("/dev/null", O_WRONLY);
        execv (pname1, argv1);
        exit (1);
    }
    
    if (pipe (pipes) < 0)
        exit (1);
    switch (fork ()) {
     case -1: exit (1);
     case 0:
	close (2);
	open ("/dev/null", O_WRONLY);
	close (1);
	dup (pipes [1]);
	close (pipes [0]);
	close (pipes [1]);
	execv (pname1, argv1);
	exit (1);
     default:
        close (2);
        open ("/dev/null", O_WRONLY);
        close (0);
        dup (pipes [0]);
        close (pipes [1]);
        close (pipes [0]);
        execv (pname2, argv2);
        exit (1);
    }
}

#define MODE_NONE 0
#define MODE_UNCOMPRESS 1
#define MODE_COMPRESS 2
#define MODE_GUNZIP 3
#define MODE_GZIP 4
#define MODE_TAR 5
#define MODE_TGZ 6
#define MODE_TAZ 7
#define MODE_UNCGZIP 8
#define MODE_GUNCOMPRESS 9
#define MODE_ZIP 10
#define MODE_SPLIT 11

void xferlog (request_rec *r, ftp_config_rec *ftp_conf, int mode, time_t time1, time_t time2, int collection)
{
    char *m;
    FILE *f;
    char *p, *host, *q;
    host = "foo"; // get_remote_host (r->connection, r->per_dir_config, REMOTE_NAME);

    if (ftp_conf->xferlog) {

        switch (mode) {
    	    case MODE_NONE: m = "_"; break;
	    case MODE_UNCOMPRESS: m = "U"; break;
	    case MODE_COMPRESS: m = "C"; break;
	    case MODE_GUNZIP: m = "U"; break;
	    case MODE_GZIP: m = "C"; break;
	    case MODE_TAR: m = "T"; break;
	    case MODE_TGZ: m = "CT"; break;
	    case MODE_TAZ: m = "CT"; break;
	    case MODE_UNCGZIP: m = "CU"; break;
	    case MODE_GUNCOMPRESS: m = "CU"; break;
	    case MODE_ZIP: m = "CT"; break;
	    case MODE_SPLIT: m = "_"; break;
        }
        
        if (!host) host = "UNKNOWN_HOST";

	for (q = p = pstrdup (r->pool, table_get (r->notes, "ftp-name")); *p; p++)
	    if (isspace(*p) || iscntrl(*p))
	        *p = '_';
        f = pfopen (r->pool, ftp_conf->xferlog, "a");
        if (f) {
            fprintf (f, "%.24s %d %s %d %s%s %c %s %c %c %s web %d %s\n",
                ctime(&time2), time2 - time1, host,
                r->bytes_sent, q, collection ? "/" : "", 'b', m,
                'o', 'a', "WWWUser@",
                0, "*");
            pfclose (r->pool, f);
        }
    }
}

int set_content_length2 (request_rec *r, long clength)                             
{                                                                                 
    char ts[30];                                                      
    
    sprintf (ts, "%ld", clength);                                  
    table_set (r->headers_out, "Content-length", pstrdup (r->pool, ts));          
    return 0;                                                                     
}                                                                                 

int ftp_cat (request_rec *r, ftp_config_rec *ftp_conf)
{
    char buffer [4096], *p, *q, c, *ext, *extp = NULL, *extp2 = NULL, *name;
    int i, fix = 0;
    int mode = MODE_NONE;
    FILE *f;
    struct stat mystat;
    time_t time1, time2;
    int pipes [2];
    int split_chunk, split_no, split_len;
    char *filename = palloc (r->pool, strlen (r->filename) + 10); 
    int reget_pos = 0;

    if (r->args) {
        int split_no = 0, large_split_chunk = 1;
        int mode = 0;
        char *p, *q, *q2, c = 1;
        
        q = r->args;
        while (c) {
            p = strchr (q, '&');
            if (!p)
                c = 0;
            else
                *p = 0;
            plustospace(q);
            unescape_url(q);
            q2 = strchr (q, '=');
            if (q2) {
                *q2 = 0;
                if (!q [1]) {
                    switch (*q) {
                        case 'N': split_no = atoi (q2 + 1); break;
                        case 'P': reget_pos = atoi (q2 + 1); break;
                        case 'T': large_split_chunk = (!strcmp (q2 + 1, "1024")) ? 1 : 0; break;
                        case 'R': mode = 2; break;
                        case 'S': mode = 1; break;
                    }
                }
            }
            q = p + 1;
        }
        if (mode != 2)
            reget_pos = 0;
        if (mode == 1) {
            char* ifile;
            char buffer[4];
            
            if (!split_no || (!large_split_chunk && split_no > 100)
                || (large_split_chunk && split_no > ('z' - 'a' + 1) * ('z' - 'a' + 1))) {
                log_reason ("wrong GET data from form", r->filename, r);
                return SERVER_ERROR;
            }

	    split_no--;            
            buffer[0] = '.';
            if (large_split_chunk) {
                buffer[1] = split_no / ('z' - 'a' + 1) + 'a';
                buffer[2] = split_no % ('z' - 'a' + 1) + 'a';
            } else {
                buffer[1] = split_no / 10 + '0';
                buffer[2] = split_no % 10 + '0';
            }
            buffer[3] = 0;
            
            ifile = pstrcat (r->pool, r->uri, buffer, NULL);
	    table_set (r->headers_out, "Location",
		       construct_url(r->pool, ifile, r->server));
	    return REDIRECT;
	}
    }
    strcpy (filename, r->filename);
    name = strrchr (filename, '/');
    if (!name)
        name = filename;
    ext = strrchr (name, '.');
    if (ext && !strcmp (ext, ".gz")) {
        extp2 = ext;
        *ext = 0;
        ext = strrchr (name, '.');
        r->content_encoding = "x-gzip";
        fix = 1;
    } else if (ext && !strcmp (ext, ".Z")) {
        extp2 = ext;
        *ext = 0;
        ext = strrchr (name, '.');
        r->content_encoding = "x-compress";
        fix = 1;
    } else if (ext && !strcmp (ext, ".tgz")) {                                    
        extp = ext;                                                               
        ext = NULL;                                                               
        r->content_encoding = "x-gzip";                                                      
        r->content_type = "application/x-tar";                                               
    } else if (ext && !strcmp (ext, ".taz")) {                                    
        extp = ext;                                                               
        ext = NULL;                                                               
        r->content_encoding = "x-compress";                                                  
        r->content_type = "application/x-tar";                                               
    } else if (r->content_encoding) {
        if (!strcmp (r->content_encoding, "x-gzip"))
            r->content_encoding = NULL;
        else if (!strcmp (r->content_encoding, "x-compress"))
            r->content_encoding = NULL;
    }
    if (ext) {
        extp = ext;
    }
    if (fix)
        *strchr (name, 0) = '.';
    f = pfopen (r->pool, filename, "r");
    if (!f) {
        p = strchr (name, 0);
        if (!r->content_encoding) {
            if (ftp_conf->compress_path) {
                strcpy (p, ".Z");
                if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_UNCOMPRESS;
                    goto cat_go_on;
                }
            }
            if (ftp_conf->gzip_path) {
                strcpy (p, ".gz");
                if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_GUNZIP;
                    goto cat_go_on;
                }
            }
            if (ftp_conf->tar_path && extp && r->content_type && !strcmp (r->content_type, "application/x-tar")) {
            	*extp = 0;
                if (stat (filename, &mystat) >= 0) {
                    mode = MODE_TAR;
                    goto cat_go_on;
                }
            }
            if (ftp_conf->zip_path && extp && r->content_type && !strcmp (r->content_type, "application/zip")) {
            	*extp = 0;
                if (stat (filename, &mystat) >= 0) {
                    mode = MODE_ZIP;
                    goto cat_go_on;
                }
            }
            if (extp) {
                *extp = 0;
                if (stat (filename, &mystat) >= 0) {
                    *p = 0;
                    if (extp [1] >= '0' && extp [1] <= '9' && extp [2] >= '0' && extp [2] <= '9' && !extp [3]) {
                        mode = MODE_SPLIT;
                        r->content_type = "application/octet-stream";
                        r->content_encoding = NULL;
                        split_chunk = 512 * 1024; /* .5M */
                        split_no = (extp[1] - '0') * 10 + extp[2] - '0';
                    
                        goto cat_go_on;
                    } else if (extp [1] >= 'a' && extp [1] <= 'z' && extp [2] >= 'a' && extp [2] <= 'z' && !extp [3]) {
                        mode = MODE_SPLIT;
                        r->content_type = "application/octet-stream";
                        r->content_encoding = NULL;
                        split_chunk = 1024 * 1024; /* 1M */
                        split_no = (extp[1] - 'a') * ('z' - 'a' + 1) + extp[2] - 'a';
                        goto cat_go_on;
                    }
                }
                *extp = '.';
            }
        } else if (ftp_conf->gzip_path && !strcmp (r->content_encoding, "x-gzip")) {
            if (ftp_conf->tar_path && extp && r->content_type && !strcmp (r->content_type, "application/x-tar")) {
            	*extp = 0;
                if (stat (filename, &mystat) >= 0) {
                    mode = MODE_TGZ;
                    goto cat_go_on;
                }
                strcpy (extp, ".tar");
            	if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_GZIP;
                    goto cat_go_on;
                }
                strcpy (extp, ".tar.Z");
            	if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_UNCGZIP;
                    goto cat_go_on;
                }
                strcpy (extp, ".taz");
            	if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_UNCGZIP;
                    goto cat_go_on;
                }
            }
            if (extp2) {
                *extp2 = 0;
                if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_GZIP;
                    goto cat_go_on;
                }
                strcpy (extp2, ".Z");
                if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_UNCGZIP;
                    goto cat_go_on;
                }
            }
        } else if (ftp_conf->compress_path && !strcmp (r->content_encoding, "x-compress")) {
            if (ftp_conf->tar_path && extp && r->content_type && !strcmp (r->content_type, "application/x-tar")) {
            	*extp = 0;
                if (stat (filename, &mystat) >= 0) {
                    mode = MODE_TAZ;
                    goto cat_go_on;
                }
                strcpy (extp, ".tar");
            	if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_COMPRESS;
                    goto cat_go_on;
                }
                if (ftp_conf->gzip_path) {
                    strcpy (extp, ".tar.gz");
            	    if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                        mode = MODE_GUNCOMPRESS;
                        goto cat_go_on;
                    }
                    strcpy (extp, ".tgz");
            	    if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                        mode = MODE_GUNCOMPRESS;
                        goto cat_go_on;
                    }
                }
            }
            if (extp2) {
                *extp2 = 0;
                if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                    mode = MODE_COMPRESS;
                    goto cat_go_on;
                }
                if (ftp_conf->gzip_path) {
                    strcpy (extp2, ".gz");
                    if (stat (filename, &mystat) >= 0 && !S_ISDIR (mystat.st_mode)) {
                        mode = MODE_GUNCOMPRESS;
                        goto cat_go_on;
                    }
                }
            }
        }
        log_reason("Couldn't find file nor any possible conversion", r->filename, r);
        return NOT_FOUND;
    }
cat_go_on:    
    if (!mode) {
        if (stat (filename, &mystat) >= 0) {
            if (mystat.st_size < reget_pos) {
                pfclose (r->pool, f);
                log_reason("Reget past the end of the file", r->filename, r);
                return NOT_FOUND;
            }
            set_content_length2 (r, mystat.st_size - reget_pos);
            if (reget_pos)
                fseek (f, reget_pos, SEEK_SET);
        } else {
            pfclose (r->pool, f);
            log_reason("Couldn't find file nor any possible conversion", r->filename, r);
            return NOT_FOUND;
        }
    } else if (mode == MODE_SPLIT) {
        if (stat (filename, &mystat) >= 0)
            if (!split_no || split_no * split_chunk < mystat.st_size) {
                split_len = mystat.st_size - split_no * split_chunk;
                if (split_len > split_chunk) split_len = split_chunk;
                set_content_length2 (r, split_len);
            } else {
                pfclose (r->pool, f);
                log_reason("Trying to get a part past the end of file", r->filename, r);
                return NOT_FOUND;
            }
        else {
            pfclose (r->pool, f);
            log_reason("Couldn't find file nor any possible conversion", r->filename, r);
            return NOT_FOUND;
        }
    }
        
    soft_timeout ("send ftp", r);

    send_http_header(r);

    if (r->header_only) {
        if (f) fclose (f);
	return 0;
    }
    
    time1 = time(NULL);
    if (!mode) {
        send_fd (f, r);
        pfclose (r->pool, f);
    } else if (mode == MODE_SPLIT) {
        f = pfopen (r->pool, filename, "r");
        if (!f) {
            log_reason("Couldn't find file nor any possible conversion", r->filename, r);
            return NOT_FOUND;
        }
        fseek (f, split_no * split_chunk, SEEK_SET);
        send_fd_len (f, r, split_len);
        pfclose (r->pool, f);
    } else {
        char *arg [12];
        char *fn = strrchr (filename, '/');
      
        *fn++ = 0;
        switch (mode) {
	  case MODE_UNCOMPRESS:
	      ftp_pipe (arg, filename, ftp_conf->compress_path ? ftp_conf->compress_path : ftp_conf->gzip_path, ftp_conf->compress_path ? "compress" : "gzip", "-c", "-d", fn, NULL, NULL); break;
	  case MODE_COMPRESS:
	      ftp_pipe (arg, filename, ftp_conf->compress_path, "compress", "-c", fn, NULL, NULL); break;
	  case MODE_GUNZIP:
	      ftp_pipe (arg, filename, ftp_conf->gzip_path, "gzip", "-c", "-d", fn, NULL, NULL); break;
	  case MODE_GZIP:
	      ftp_pipe (arg, filename, ftp_conf->gzip_path, "gzip", "-9c", fn, NULL, NULL); break;
	  case MODE_TAR:
	      ftp_pipe (arg, filename, ftp_conf->tar_path, "tar", "cf", "-", fn, NULL, NULL); break;
	  case MODE_TGZ:
	      ftp_pipe (arg, filename, ftp_conf->tar_path, "tar", "cf", "-", fn, NULL, ftp_conf->gzip_path, "gzip", "-9c", NULL); break;
	  case MODE_TAZ: 
	      ftp_pipe (arg, filename, ftp_conf->tar_path, "tar", "cf", "-", fn, NULL, ftp_conf->compress_path, "compress", "-c", NULL); break;
	  case MODE_UNCGZIP:
	      ftp_pipe (arg, filename, ftp_conf->compress_path ? ftp_conf->compress_path : ftp_conf->gzip_path, ftp_conf->compress_path ? "compress" : "gzip", "-c", "-d", fn, NULL, ftp_conf->gzip_path, "gzip", "-9c", NULL); break;
	  case MODE_GUNCOMPRESS:
	      ftp_pipe (arg, filename, ftp_conf->gzip_path, "gzip", "-c", "-d", fn, NULL, ftp_conf->compress_path, "compress", "-c", NULL); break;
	  case MODE_ZIP:
	      ftp_pipe (arg, filename, ftp_conf->zip_path, "zip", "-ry9", "-", fn, NULL, NULL); break;
        }
        if (!spawn_child (r->connection->pool, ftp_child, (void *)arg,
            kill_after_timeout, NULL, &f)) {
            *(--fn) = '/';
            log_reason ("couldn't spawn child process", r->filename, r);
            return SERVER_ERROR;
        }
        *(--fn) = '/';
        send_fd(f,r);
        pfclose (r->connection->pool, f);
    }
    
    time2 = time(NULL);
    xferlog (r, ftp_conf, mode, time1, time2, 0);
    
    return 0;
}

int ftp_conversion (request_rec *r, ftp_config_rec *ftp_conf)
{
    request_rec rr;
    char *ext, *ext2;
    char *ftpname = table_get (r->notes, "ftp-name");
    char *root_uri = table_get (r->notes, "ftp-root-uri");

    r->content_type = "text/html";
    r->content_encoding = NULL;
    
    soft_timeout ("send conversion", r);
    send_http_header(r);

    if (r->header_only) {
	return 0;
    }

    /* Spew HTML preamble */
    
    rprintf (r,"<HEAD><TITLE>");
    rprintf (r,ftp_conf->convtitle,
	     *ftpname ? ftpname : "/");
    rprintf (r,"</TITLE></HEAD><BODY>%c", LF);

    if (S_ISDIR (r->finfo.st_mode))
        ext = NULL;
    else { 
        ext2 = strrchr (ftpname, '/');
        if (!ext2)
            ext2 = ftpname;
        ext = strrchr (ext2, '.');
        if (ext && (!strcmp (ext, ".gz") || !strcmp (ext, ".Z"))) {
            *ext = 0;
            ext2 = strrchr (ext2, '.');
            if (ext2 && !strcmp (ext2, ".tar")) {
                *ext = '.';
                ext = ext2;
            } else
                *ext = '.';
        }
    }
    rprintf (r,"<H1>Download %s as:</H1>%c", *ftpname ? ftpname : "/", LF);	          
    if (!S_ISDIR (r->finfo.st_mode))
        rprintf(r,"<A HREF=\"%s%s\">As is, no conversion</A><BR>%c", root_uri, ftpname, LF);
    if (!ext || (strcmp (ext, ".tar") && strcmp (ext, ".tgz") && strcmp (ext, ".taz")
        && strcmp (ext, ".tar.gz") && strcmp (ext, ".tar.Z"))) {
        if (ftp_conf->tar_path)
            rprintf(r,"<A HREF=\"%s%s.tar\">Tar archive</A><BR>%c", root_uri, ftpname, LF);
        if (ftp_conf->tar_path && ftp_conf->gzip_path)
            rprintf(r,"<A HREF=\"%s%s.tgz\">Gzipped Tar archive</A><BR>%c", root_uri, ftpname, LF);
        if (ftp_conf->tar_path && ftp_conf->compress_path)
            rprintf(r,"<A HREF=\"%s%s.taz\">Compressed Tar archive</A><BR>%c", root_uri, ftpname, LF);
        if (!S_ISDIR (r->finfo.st_mode)) {
            if (ftp_conf->gzip_path) {
                if (!ext || strcmp (ext, ".gz")) {
                    if (ext && !strcmp (ext, ".Z")) {
                        *ext = 0;
                        rprintf(r,"<A HREF=\"%s%s.gz\">Gzipped file</A><BR>%c", root_uri, ftpname, LF);
                        *ext = '.';
                    } else
                        rprintf(r,"<A HREF=\"%s%s.gz\">Gzipped file</A><BR>%c", root_uri, ftpname, LF);
		} else {
		    *ext = 0;
		    rprintf(r,"<A HREF=\"%s%s\">Plain file</A><BR>%c", root_uri, ftpname, LF);
		    *ext = '.';
		}
            }
            if (!ext || strcmp (ext, ".Z")) {
                if (ftp_conf->compress_path) {
                    if (ext && !strcmp (ext, ".gz") && ftp_conf->gzip_path) {
                        *ext = 0;
                        rprintf(r,"<A HREF=\"%s%s.Z\">Compressed file</A><BR>%c", root_uri, ftpname, LF);
                        *ext = '.';
                    } else
                        rprintf(r,"<A HREF=\"%s%s.Z\">Compressed file</A><BR>%c", root_uri, ftpname, LF);
                }
	    } else {
	        if (ftp_conf->gzip_path || ftp_conf->compress_path) {
		    *ext = 0;
		    rprintf(r,"<A HREF=\"%s%s\">Plain file</A><BR>%c", root_uri, ftpname, LF);
		    *ext = '.';
		}
            }
        }
    } else {
        if (!strcmp (ext, ".tar")) {
            if (ftp_conf->gzip_path) {
                strcpy (ext, ".tgz");
	        rprintf(r,"<A HREF=\"%s%s\">Gzipped Tar archive</A><BR>%c", root_uri, ftpname, LF);                
	    }
            if (ftp_conf->compress_path) {
                strcpy (ext, ".taz");
	        rprintf(r,"<A HREF=\"%s%s\">Compressed Tar archive</A><BR>%c", root_uri, ftpname, LF);                
	    }
	} else if (!strcmp (ext, ".tgz") || !strcmp (ext, ".tar.gz")) {
            if (ftp_conf->gzip_path) {
                strcpy (ext, ".tar");
	        rprintf(r,"<A HREF=\"%s%s\">Tar archive</A><BR>%c", root_uri, ftpname, LF);                
	    }
            if (ftp_conf->gzip_path && ftp_conf->compress_path) {
                strcpy (ext, ".taz");
	        rprintf(r,"<A HREF=\"%s%s\">Compressed Tar archive</A><BR>%c", root_uri, ftpname, LF);                
	    }
	} else if (!strcmp (ext, ".taz") || !strcmp (ext, ".tar.Z")) {
            if (ftp_conf->gzip_path || ftp_conf->compress_path) {
                strcpy (ext, ".tar");
	        rprintf(r,"<A HREF=\"%s%s\">Tar archive</A><BR>%c", root_uri, ftpname, LF);                
	    }
            if (ftp_conf->gzip_path) {
                strcpy (ext, ".tgz");
	        rprintf(r,"<A HREF=\"%s%s\">Gzipped Tar archive</A><BR>%c", root_uri, ftpname, LF);                
	    }
	}
    }
    if (!ext || strcmp (ext, ".zip")) {
        if (ftp_conf->zip_path)
            rprintf(r,"<A HREF=\"%s%s.zip\">ZIP archive</A><BR>%c", root_uri, ftpname, LF);
    }
    if (!S_ISDIR (r->finfo.st_mode)) {
        rprintf (r,"<FORM METHOD=GET ACTION=\"%s%s\"><INPUT TYPE=TEXT NAME=N SIZE=4 VALUE=1>th(st,nd,rd) part of the file split by%c"
                   "<SELECT NAME=T>%c"
                   "<OPTION SELECTED VALUE=1024>1024K%c"
                   "<OPTION VALUE=512>512K%c"
                   "</SELECT>%c"
                   "<INPUT TYPE=SUBMIT NAME=S VALUE=\"Get it\"><BR>%c"
                   "Reget the file at position <INPUT TYPE=TEXT NAME=P SIZE=8 VALUE=0>B%c"
                   "<INPUT TYPE=SUBMIT NAME=R VALUE=\"Get it\"><BR>%c"
                   "</FORM>%c", root_uri, ftpname, LF, LF, LF, LF, LF, LF, LF, LF, LF);
    }
    
    rprintf(r,"%c</BODY>%c",LF,LF);
    return 0;
}
    
int ftp_index (request_rec *r, ftp_config_rec *ftp_conf, char *selmask, char *deselmask, array_header *sel_files, int forced)
{
    char *ftpname = table_get (r->notes, "ftp-name");
    char *name = r->filename;
    char *pp, *qq;
    char **ppp;
    char *root_uri = table_get (r->notes, "ftp-root-uri");
    
    DIR *d;
    struct DIR_TYPE *dstruct;
    int num_ent=0,x;
    struct ent *head,*p;
    struct ent **ar;
    char *tmp;
    int ftp_opts = find_opts(ftp_conf, r);
    int i, j;
    char c;
    int java = 0;
    char *javardme = NULL;
    char *forcestr = "";
    char *forcestr2 = "";

    switch (forced) {
     case 1: forcestr2 = "?F"; break;
     case 2: java = 1; break;
     case 3: ftp_opts |= TABLES; forcestr = (ftp_opts & FORCEINDEX) ? "?a" : "?A"; forcestr2 = "?A"; break;
     case 4: ftp_opts &= ~TABLES; forcestr = (ftp_opts & FORCEINDEX) ? "?l" : "?L"; forcestr2 = "?L"; break;
    }
    if (!*forcestr && !(ftp_opts & FORCEINDEX)) 
        forcestr = "?F"; 
    if (forced < 3 && table_get (r->notes, "use_tables") != NULL)
        ftp_opts |= TABLES;

    if(!(d=opendir(name))) return FORBIDDEN;

    if (java) {
        r->content_type = "text/plain";
        r->content_encoding = NULL;
    } else {
        r->content_type = "text/html";
        r->content_encoding = NULL;
    }
    
    soft_timeout ("send directory", r);
    send_http_header(r);

    if (r->header_only) {
	closedir (d);
	return 0;
    }

    if (!java) {
        int deftables = (table_get (r->notes, "use_tables") != NULL);
        /* Spew HTML preamble */

        rprintf (r,"<HEAD><TITLE>");
        rprintf (r,ftp_conf->title, 
	         *ftpname ? ftpname : "/");
	rprintf (r,"</TITLE></HEAD><BODY>%c", LF);

        if(!ftp_conf->prolog || (!(insert_readme(name,ftp_conf->prolog,0,NULL,r))))
            rprintf(r,"<H1>Listing of %s</H1>%c",ftpname,LF);
        
        rprintf (r,"<P>%c", LF);
        if(ftp_opts & TABLES)
            rprintf (r,"<TT><FONT SIZE=-2><A HREF=\"%s%s/%s\">Switch to plain view (no tables)</A></FONT></TT><P>%c", 
                       root_uri, ftpname, deftables ? "?L" : "?F", LF);
        else
            rprintf (r,"<TT><FONT SIZE=-2><A HREF=\"%s%s/%s\">Switch to table view</A></FONT></TT><P>%c", 
                       root_uri, ftpname, !deftables ? "?A" : "?F", LF);
    }

    if ((tmp = find_readme(ftp_conf,r))) {
        if (!java)
            insert_readme(name,tmp,1,NULL,r);
        else {
            request_rec *rr = sub_req_lookup_uri (tmp, r);

            if (rr->finfo.st_mode)
    		javardme = pstrdup(r->pool,rr->uri);
    	    destroy_sub_req (rr);
        }
    }

    if (!java) {    
        if (*ftpname)
            pp = ftpname + 1;
        else
            pp = "";
        if (!(ftp_opts & TABLES) && ftp_conf->search_filename) {
	    if (insert_readme(name,ftp_conf->search_filename,0,ftpname,r))
                rprintf (r,"<HR>");
        }
        if (ftp_opts & TABLES)
            rprintf (r,"<TABLE WIDTH=452 BORDER=0 CELLSPACING=1 CELLPADDING=0>%c"
                       "<TR><TD ALIGN=LEFT>%c", LF, LF);
        rprintf (r,"<A HREF=\"%s/%s\"><IMG SRC=\"%s\" ALIGN=CENTER BORDER=0> /</A>%c", 
                   root_uri, forcestr, 
                   *pp ? ftp_conf->dirpath_icon : ftp_conf->dircurrent_icon, LF);
                   
        i = 1;               
        while (*pp) {
            qq = pp;
            pp = strchr (qq, '/');
            if (!pp)
                pp = strchr (qq, 0);
            c = *pp;
            *pp = 0;
            rprintf (r,"<BR>");
            for (j=0;j<i;j++)
                rprintf(r,"<IMG SRC=\"%s\">",ftp_conf->dirspace_icon);
                rprintf (r,"<A HREF=\"%s%s/%s\"><IMG SRC=\"%s\" ALIGN=CENTER BORDER=0> %s</A>%c",
                           root_uri, ftpname, c ? forcestr : forcestr2,  
                           c ? ftp_conf->dirpath_icon : ftp_conf->dircurrent_icon,
                           qq, LF);
            if (!c)
                break;
            *pp++ = '/';
            i++;
        }
    
        if ((ftp_opts & TABLES) && ftp_conf->search_filename) {
            rprintf (r,"<TD ALIGN=RIGHT>");
	    insert_readme(name,ftp_conf->search_filename,0,ftpname,r);
        }
        if (ftp_opts & TABLES)
	    rprintf (r,"</TABLE>");
    
        rprintf (r,"<HR>%c"
    	           "<FORM METHOD=POST ACTION=\"%s%s/%s\">%c", 
    	           LF, root_uri, ftpname, forcestr2, LF);
    	if (ftp_opts & TABLES)
    	    rprintf (r, "<TABLE BORDER=0>%c<TR><TH><TH><TH NOWRAP><B>name</B><TH NOWRAP><B>size [B]</B><TH NOWRAP><B>date</B>%s%c", 
    	                LF, (ftp_opts & CONVICONS) ? "<TH NOWRAP><B>conv</B>" : "", LF);
    }
    
    /* 
     * Since we don't know how many dir. entries there are, put them into a 
     * linked list and then arrayificate them so qsort can use them. 
     */
    {
        regex_t reg, *rreg = NULL;
        
        if (ftp_conf->hidesymlink)
            if (!regcomp (&reg, ftp_conf->hidesymlink, REG_EXTENDED|REG_NOSUB))
                rreg = &reg;
        head=NULL;
        while((dstruct=readdir(d))) {
            if((p = make_ftp_entry(dstruct->d_name, ftp_opts, ftp_conf, r, rreg, *ftpname ? ftpname : "/"))) {
                p->next=head;
                head=p;
                num_ent++;
            }
        }
	if (rreg)
	    regfree (rreg);        
    }
    ar=(struct ent **) palloc(r->pool, num_ent*sizeof(struct ent *));
    p=head;
    x=0;
    while(p) {
        ar[x++]=p;
        p = p->next;
    }
    
    qsort((void *)ar,num_ent,sizeof(struct ent *),
#ifdef ULTRIX_BRAIN_DEATH
          (int (*))dsortf);
#else
          (int (*)(const void *,const void *))dsortf);
#endif
    if (!java && (selmask || deselmask || sel_files))
        for (x = 0; x < num_ent; x++) {
            if (selmask && !strcmp_match (ar[x]->name, selmask))
                ar[x]->checked = 1;
            else if (deselmask && !strcmp_match (ar[x]->name, deselmask))
                ar[x]->checked = 0;
            else if (sel_files) {
                ppp = (char **)sel_files->elts;
                for (j = 0; j < sel_files->nelts; j++)
                    if (!strcmp (ppp[j], ar[x]->name)) {
                        ar[x]->checked = 1;
                        break;
                    }
            }
        }
    if (java) {
        rprintf(r,"%d %s %s %s%c", num_ent, root_uri, *ftpname ? ftpname : "/", javardme ? javardme : "", LF);
        output_java_directories(ar, num_ent, ftp_conf, r, ftp_opts);
    } else {
        if (!(ftp_opts & TABLES))
            rprintf (r,"<PRE>");
        output_directories(ar, num_ent, ftp_conf, r, ftp_opts, forcestr);
        if (!(ftp_opts & TABLES))
            rprintf (r,"</PRE>");
    }
    closedir(d);

    if (!java) {
        if (ftp_opts & TABLES)
            rprintf(r,"</TABLE>%c", LF);

        {
             request_rec rr;
             char *icon;


	     rprintf (r,"<P><HR>%s<DL><DT>Download as<BR><I>(if more than one item is selected):</I>%c", 
	                ftp_opts & TABLES ? "<TABLE BORDER=0><TR><TD>" : "", LF);	          
             if (ftp_conf->tar_path) {
                 rr.filename = "a.tar";
                 rr.content_type = "application/x-tar";
                 rr.content_encoding = NULL;
                 icon = find_icon (ftp_conf, &rr, 0);
                 if (!icon) icon = ftp_conf->default_icon;
                 rprintf(r,"<DD><IMG SRC=\"%s\"><INPUT TYPE=RADIO NAME=as VALUE=tar> Tar%c", icon, LF);
             }
             if (ftp_conf->tar_path && ftp_conf->gzip_path) {
                 rr.filename = "a.tar.gz";
                 rr.content_type = "application/x-tar";
                 rr.content_encoding = "x-gzip";
                 icon = find_icon (ftp_conf, &rr, 0);
                 if (!icon) icon = ftp_conf->default_icon;
                 rprintf(r,"<DD><IMG SRC=\"%s\"><INPUT TYPE=RADIO NAME=as VALUE=tar.gz CHECKED> Gzipped Tar%c", icon, LF);
             }
	     if (ftp_conf->tar_path && ftp_conf->compress_path) {
                 rr.filename = "a.tar.Z";
                 rr.content_type = "application/x-tar";
                 rr.content_encoding = "x-compress";
                 icon = find_icon (ftp_conf, &rr, 0);
                 if (!icon) icon = ftp_conf->default_icon;
                 rprintf(r,"<DD><IMG SRC=\"%s\"><INPUT TYPE=RADIO NAME=as VALUE=tar.Z> Compressed Tar%c", icon, LF);
             }
             if (ftp_conf->zip_path) {
                 rr.filename = "a.zip";
                 rr.content_type = "application/zip";
                 rr.content_encoding = NULL;
                 icon = find_icon (ftp_conf, &rr, 0);
                 if (!icon) icon = ftp_conf->default_icon;
                 rprintf(r,"<DD><IMG SRC=\"%s\"><INPUT TYPE=RADIO NAME=as VALUE=zip> ZIP%c", icon, LF);
             }
             rprintf(r,"</DL>%c%s<INPUT TYPE=SUBMIT NAME=submit VALUE=Receive>%c"
                       "<INPUT TYPE=RESET VALUE=\"Restore selection\">%c<BR>%c"
                       "<INPUT TYPE=SUBMIT NAME=submit VALUE=Select><INPUT TYPE=TEXT NAME=selectwhat VALUE=* SIZE=6>%c"
                       "<INPUT TYPE=SUBMIT NAME=submit VALUE=Deselect><INPUT TYPE=TEXT NAME=deselectwhat VALUE=* SIZE=6>%s</FORM>%c",
                       LF,ftp_opts & TABLES ? "<TD>" : "<P>", LF,LF,LF,LF,ftp_opts & TABLES ? "</TABLE>" : "", LF);
        }
        if(ftp_conf->epilog)
            insert_readme(name,ftp_conf->epilog,0,NULL,r);
      
        rprintf(r,"</BODY>");
    }
    return 0;
}

/* The formal handler... */

int handle_ftp (request_rec *r)
{
    ftp_config_rec *d =
      (ftp_config_rec *)get_module_config (r->per_dir_config, &conv_module);
    char *names_ptr = d->index_names ? d->index_names : "None";
    int allow_opts = allow_options (r);
    int urilen;
    array_header *sel_files = NULL;
    char *selmask = NULL, *deselmask = NULL;

    if (r->method_number != M_GET) {
        if (r->method_number == M_POST) {
            char *p, **pp, *q, *q2;
            #define AS_TAR 1
            #define AS_TGZ 2
            #define AS_TAZ 3
            #define AS_ZIP 4
            int as = 0, mode = 0;
            int content_length, n;
            void (*handler)();
            char buffer[HUGE_STRING_LEN];

            p = table_get (r->headers_in, "Content-type");
            if (!p || strcasecmp (p, "application/x-www-form-urlencoded"))
                return NOT_IMPLEMENTED;
            p = table_get (r->headers_in, "Content-length");
            if (!p || (content_length = atoi (p)) <= 0)
                return NOT_IMPLEMENTED;
                
            sel_files = make_array (r->pool, 40, sizeof (char *));
            hard_timeout ("fetch args", r);
            handler = signal (SIGPIPE, SIG_IGN);

            q = buffer;
            q2 = buffer;
            while (content_length > 0) {
                n = content_length;
                if (n >= buffer - q2 + HUGE_STRING_LEN) n = buffer - q2 + HUGE_STRING_LEN - 1;
                if (n <= 0) {
                    log_reason ("to long POST parameter", r->filename, r);
                    return SERVER_ERROR;
                }
                n = read_client_block (r, q2, n);
                q2 += n;
                *q2 = 0;
                content_length -= n;
                for (;;) {
                    if (!*q && !content_length)
                        break;
                    p = strchr (q, '&');
                    if (!p && content_length) {
                        memmove (buffer, q, q2 - q + 1);
                        q2 = buffer + (q2 - q);
                        q = buffer;
                        break;
                    } else if (p)
                        *p = 0;
                    plustospace(q);
                    n = unescape_url (q);
                    if (n) {
                        log_reason ("wrong POST parameter", r->filename, r);
                        return n;
                    }
                    p = strchr (q, '=');
                    if (p) {
                        *p++ = 0;
                	if (!strcasecmp (q, "file")) {
                	    pp = (char **)push_array(sel_files);
                	    *pp = pstrdup (r->pool, p);
                	} else if (!strcasecmp (q, "as")) {
                	    if (!strcasecmp (p, "tar"))
                	        as = AS_TAR;
                	    else if (!strcasecmp (p, "tar.gz") || !strcasecmp (p, "tgz"))
                	        as = AS_TGZ;
                	    else if (!strcasecmp (p, "tar.Z") || !strcasecmp (p, "taz"))
                	        as = AS_TAZ;
                	    else if (!strcasecmp (p, "zip"))
                	        as = AS_ZIP;
                	} else if (!strcasecmp (q, "submit")) {
                	    if (!strcasecmp (p, "Select"))
                	        mode = 2;
                	    else if (!strcasecmp (p, "Deselect"))
                	        mode = 3;
                	    else if (!strcasecmp (p, "Receive"))
                	        mode = 1;
                	} else if (!strcasecmp (q, "selectwhat"))
                	    selmask = pstrdup (r->pool, p);
                	else if (!strcasecmp (q, "deselectwhat"))
                	    deselmask = pstrdup (r->pool, p);                
                        q = strchr (p, 0) + 1;
                    } else
                        q = strchr (q, 0) + 1;
                }
            }
            signal (SIGPIPE, handler);
            kill_timeout (r);

            if (mode == 1) {
                char **argv;
                int i, mode;
                char **ppp;
                FILE *f;
                time_t time1, time2;
                
                if (!as || !sel_files->nelts)
                    return NOT_IMPLEMENTED;
                if (as <= AS_TAZ && !d->tar_path || as == AS_ZIP && !d->zip_path ||
                    as == AS_TGZ && !d->gzip_path || as == AS_TAZ && !d->compress_path)
                    return NOT_IMPLEMENTED;

    		soft_timeout ("send ftp", r);
    		
    		switch (as) {
    		    case AS_TAR: mode = MODE_TAR; r->content_type = "application/x-tar"; break;
    		    case AS_TGZ: mode = MODE_TGZ; r->content_type = "application/x-tar"; r->content_encoding = "x-gzip"; break;
    		    case AS_TAZ: mode = MODE_TAZ; r->content_type = "application/x-tar"; r->content_encoding = "x-compress"; break;
    		    case AS_ZIP: mode = MODE_ZIP; r->content_type = "application/zip"; break;
    		}

    		send_http_header(r);

    		if (r->header_only) {
		    return 0;
    		}
    
    		time1 = time(NULL);

                argv = pcalloc (r->pool, (sel_files->nelts + 16) * sizeof (char *));
                
                argv[0] = r->filename;
                if (as <= AS_TAZ) {
                    argv[1] = d->tar_path;
                    argv[2] = "tar";
                    argv[3] = "cf";
                    argv[4] = "-";
                } else {
                    argv[1] = d->zip_path;
                    argv[2] = "zip";
                    argv[3] = "-ry9";
                    argv[4] = "-";
                }
                
                ppp = (char **)sel_files->elts;
                for (i = 0; i < sel_files->nelts; i++)
                    argv[5+i] = ppp[i];    
                i += 5;
                argv[i++] = NULL;
                if (as == AS_TGZ) {
                    argv[i++] = d->gzip_path;
                    argv[i++] = "gzip";
                    argv[i++] = "-9c";
                } else if (as == AS_TAZ) {
                    argv[i++] = d->compress_path;
                    argv[i++] = "compress";
                    argv[i++] = "-c";
                }
                argv[i] = NULL;
                
                if (!spawn_child (r->connection->pool, ftp_child, (void *)argv,
                    kill_after_timeout, NULL, &f)) {
                    log_reason ("couldn't spawn child process", r->filename, r);
                    return SERVER_ERROR;
                }
                
                send_fd(f,r);
                pfclose (r->connection->pool, f);
                
                time2 = time(NULL);
                xferlog (r, d, mode, time1, time2, 1);
                return 0;
                
            } else if (mode == 2) {
                if (!selmask || !*selmask)
                    selmask = "*";
                deselmask = NULL;
            } else if (mode == 3) {
                if (!deselmask || !*deselmask)
                    deselmask = "*";
                selmask = NULL;
            } else
                return NOT_IMPLEMENTED;
        } else
            return NOT_IMPLEMENTED;
    }

    if (S_ISDIR (r->finfo.st_mode) && (r->uri[0] == '\0' || r->uri[(urilen = strlen(r->uri))-1] != '/')) {
        char* ifile = pstrcat (r->pool, r->uri, "/", NULL);
	table_set (r->headers_out, "Location",
		   construct_url(r->pool, ifile, r->server));
	return REDIRECT;
    }

    if (S_ISDIR (r->finfo.st_mode)) {
        int i;
        if (r->args && (!strcmp (r->args, "C") || !strcasecmp (r->args, "conv")))
            names_ptr = "";
        r->filename = pstrcat (r->pool, r->filename, "/", NULL);
        while (*names_ptr) {
	    char *name_ptr = getword_conf (r->pool, &names_ptr);
	    request_rec *rr;
	
	    if (!strcasecmp (name_ptr, "None"))
	        break;
	    rr = sub_req_lookup_uri (name_ptr, r);
           
	    if (rr->status == 200 && rr->finfo.st_mode != 0) {
                char *new_uri = escape_uri(r->pool, rr->uri);
	        destroy_sub_req (rr);
        
        	/* In case uri contains /?F at the end, force displaying of index */
        	/* Force Java /?J help output as well */
        	i = 0;
        	if (r->args) {
        	    if (r->args[0] && !r->args[1])
        	        switch (r->args[0]) {
        	         case 'F': i = 1; break;
        	         case 'J': i = 2; break;
        	         case 'A': i = 3; break;
        	         case 'L': i = 4; break;
        	        }
        	    else if (!strcasecmp (r->args, "force"))
        	        i = 1;
        	    else if (!strcasecmp (r->args, "java"))
        	        i = 2;
        	    else if (!strcasecmp (r->args, "table"))
        	        i = 3;
        	    else if (!strcasecmp (r->args, "plain"))
        	        i = 4;
        	}
        	if (i) {
                    return ftp_index (r, d, selmask, deselmask, sel_files, i);
                }
	        internal_redirect (new_uri, r);
	        return OK;
	    }

            destroy_sub_req (rr);
        }
        if (r->args && (!strcmp (r->args, "C") || !strcasecmp (r->args, "conv")))
            return ftp_conversion (r, d);
        i = 0;
        if (r->args) {
       	    if (r->args[0] && !r->args[1])
        	switch (r->args[0]) {
        	 case 'J': i = 2; break;
        	 case 'A': i = 3; break;
        	 case 'L': i = 4; break;
        	 case 'a': i = 3; break;
        	 case 'l': i = 4; break;
        	}
            else if (!strcasecmp (r->args, "java"))
                i = 2;
            else if (!strcasecmp (r->args, "table"))
                i = 3;
            else if (!strcasecmp (r->args, "plain"))
                i = 4;
        }
        return ftp_index (r, d, selmask, deselmask, sel_files, i);
    } else {
        char *p = table_get (r->notes, "not-forced-type");
        if (p)
            r->content_type = p;
        else
            r->content_type = NULL;
        if (r->args && (!strcmp (r->args, "C") || !strcasecmp (r->args, "conv")))
            return ftp_conversion (r, d);
        return ftp_cat (r, d);
    }
}

static int alias_matches (char *uri, char *alias_fakename)
{
    char *end_fakename = alias_fakename + strlen (alias_fakename);
    char *aliasp = alias_fakename, *urip = uri;

    while (aliasp < end_fakename) {
	if (*aliasp == '/') {
	    /* any number of '/' in the alias matches any number in
	     * the supplied URI, but there must be at least one...
	     */
	    if (*urip != '/') return 0;
	    
	    while (*aliasp == '/') ++ aliasp;
	    while (*urip == '/') ++ urip;
	}
	else {
	    /* Other characters are compared literally */
	    if (*urip++ != *aliasp++) return 0;
	}
    }

    /* Check last alias path component matched all the way */

    if (aliasp[-1] != '/' && *urip != '\0' && *urip != '/')
	return 0;

    /* Return number of characters from URI which matched (may be
     * greater than length of alias, since we may have matched
     * doubled slashes)
     */

    return urip - uri;
}

char *try_ftp_aliases (request_rec *r, array_header *aliases)
{
    alias_entry *entries = (alias_entry *)aliases->elts;
    int i;
    
    for (i = 0; i < aliases->nelts; ++i) {
        alias_entry *p = &entries[i];
        int l = alias_matches (r->uri, p->fake);

        if (l > 0) {
            char *q, *q2;
            
	    table_set (r->notes, "ftp-forced-type", FTP_MAGIC_TYPE);
	    q = pstrdup (r->pool, p->fake);
	    for (q2 = q + strlen (q) - 1; q2 >= q && *q2 == '/'; q2--);
	    q2[1]=0;
	    table_set (r->notes, "ftp-root-uri", q);
	    if (r->uri[l] != '/' && r->uri[l-1] == '/')
	        q = pstrdup (r->pool, r->uri + l - 1);
	    else
	        q = pstrdup (r->pool, r->uri + l);
	    for (q2 = q + strlen (q) - 1; q2 >= q && *q2 == '/'; q2--);
	    q2[1]=0;
	    table_set (r->notes, "ftp-name", q);	    
	    return pstrcat(r->pool, p->real, r->uri + l, NULL);
        }
    }

    return NULL;
}

static int translate_alias_redir(request_rec *r)
{
    void *sconf = r->server->module_config;
    array_header *aliases =
        (array_header *)get_module_config(sconf, &conv_module);
    char *ret;

    if (r->uri[0] != '/' && r->uri[0] != '\0') 
        return BAD_REQUEST;

    if ((ret = try_ftp_aliases (r, aliases)) != NULL) {
        r->filename = ret;
        return OK;
    }
    
    return DECLINED;
}

int fixup_ftp_alias (request_rec *r)
{
    char *t = table_get (r->notes, "ftp-forced-type");
    char **p;
    regex_t reg;
    int i, j, k, l;
    int table = 0, java = 0;
    char *agent = table_get (r->headers_in, "User-Agent");
    ftp_config_rec *d =
      (ftp_config_rec *)get_module_config (r->per_dir_config, &conv_module);
    
    if (!t) return DECLINED;
    table_set (r->notes, "not-forced-type", r->content_type);
    r->content_type = t;
    
    if (agent) {
	for (i = TABLEALLOW; i <= JAVADENY; i++) {
	    p = (char **)d->allowdeny [i]->elts;
	    k = d->allowdeny [i]->nelts;
	    l = 0;
	    if (p)
	        for (j = 0; j < k; j++) {
	            if (!regcomp (&reg, p [j], REG_EXTENDED|REG_NOSUB)) {
	                if (!regexec (&reg, agent, 0, NULL, 0)) {
	                    l = 1;
	                    switch (i) {
	                     case TABLEALLOW: table = 1; break;
	                     case TABLEDENY: table = 0; break;
	                     case JAVAALLOW: java = 1; break;
	                     case JAVADENY: java = 0; break;
	                    }
	                    regfree (&reg);
	                    break;
	                }
	                regfree (&reg);
	            }
	        }
	    if (!l && !(i & 1))
	        i++;
	}
    }
    if (table)
        table_set (r->notes, "use_tables", "yes");
    if (java)
        table_set (r->notes, "use_java", "yes");
        
    return OK;
}


handler_rec ftp_handlers[] = {
{ FTP_MAGIC_TYPE, handle_ftp },
{ NULL }
};

module conv_module = {
   STANDARD_MODULE_STUFF,
   NULL,			/* initializer */
   create_ftp_config,		/* dir config creater */
   merge_ftp_configs,		/* dir merger --- default is to override */
   create_ftp_server_config,	/* server config */
   merge_ftp_server_configs,	/* merge server config */
   ftp_cmds,			/* command table */
   ftp_handlers,		/* handlers */
   translate_alias_redir,	/* filename translation */
   NULL,			/* check_user_id */
   NULL,			/* check auth */
   NULL,			/* check access */
   NULL,			/* type_checker */
   fixup_ftp_alias,		/* fixups */
   NULL				/* logger */
};
