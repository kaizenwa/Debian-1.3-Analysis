
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
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
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

#include "httpd.h"
#include "http_config.h"
#include "http_log.h"

/* mod_speling.c - by Alexei Kosut <akosut@organic.com> June, 1996
 *
 * This module is transparent, and simple. It attemps to correct
 * mispellings of URLs that users might have entered, namely by checking
 * capitalizations. If it finds a match, it sends a redirect.
 *
 * Activate it with "CheckSpelling On"
 */

module speling_module;

/* We use the "unconventional" mod_userdir approach here. And heck,
 * here it's just one int!
 */

void *create_speling_config (pool *dummy, server_rec *s) { 
    return (void *)0;
}

char *set_speling (cmd_parms *cmd, void *dummy, int arg)
{
    void *server_conf = cmd->server->module_config;
    
    set_module_config (server_conf, &speling_module, (void *)arg);
    return NULL;
}

command_rec speling_cmds[] = {
{ "CheckSpelling", set_speling, NULL, RSRC_CONF, FLAG,
    "whether or not to fix miscapitalized requests" },
{ NULL }
};


int check_speling (request_rec *r)
{
    void *server_conf = r->server->module_config;
    char *good, *bad, *postgood, *url;
    int filoc, dotloc, urlen, pglen;
DIR *dirp;
    struct DIR_TYPE *dir_entry;

    if (!(int)get_module_config(server_conf, &speling_module))
      return DECLINED;

    /* We only want to worry about GETs */
    if (r->method_number != M_GET) return DECLINED;

    /* We've already got a file of some kind or another */
    if (r->proxyreq || (r->finfo.st_mode != 0)) return DECLINED;

    /* This is a sub request - don't mess with it */
    if (r->main) return DECLINED;

    /* The request should end up looking like this:
     * r->uri: /correct-url/mispelling/more
     * r->filename: /correct-file/mispelling r->path_info: /more
     *
     * So we do this in steps. First break r->filename into two peices
     */

    filoc = rind(r->filename, '/');
    if (filoc == -1) return DECLINED;

    /* good = /correct-file */
    good = pstrndup(r->pool, r->filename, filoc);
    /* bad = mispelling */
    bad = pstrdup(r->pool, r->filename+filoc+1);
    /* postgood = mispelling/more */
    postgood = pstrcat(r->pool, bad, r->path_info, NULL);

    urlen = strlen(r->uri);
    pglen = strlen(postgood);

    /* Check to see if the URL peices add up */
    if (strcmp(postgood, r->uri + (urlen-pglen)))
      return DECLINED;

    /* url = /correct-url */
    url = pstrndup(r->pool, r->uri, (urlen-pglen));

    /* Now open the directory and do ourselves a check... */
    dirp = opendir (good);
    if (dirp == NULL)	/* Oops, not a directory... */
      return DECLINED;

    while ((dir_entry = readdir (dirp))) {
      if (!strcasecmp(bad, dir_entry->d_name)) {
	/* Wow... we found us a mispelling. Construct a fixed url */
	char *nuri = pstrcat(r->pool, url, dir_entry->d_name, r->path_info,
			     NULL);
	char *ref = table_get(r->headers_in, "Referer");
	
	table_set(r->headers_out, "Location", construct_url(r->pool,
		 nuri, r->server));
	log_error(pstrcat(r->pool, "Fixed spelling: ", r->uri, " to ", nuri,
			  ref ? " from " : NULL, ref, NULL), r->server);
	closedir(dirp);
	return REDIRECT;
      }
    }

    /* Okay... we didn't find anything. Now we take out the hard-core
     * power tools. There are several cases here. Someone might have
     * entered a wrong extension (.htm instead of .html or vice versa)
     * or the document could be negotated. At any rate, now we just compare
     * stuff before the first dot. If it matches, we figure we got us a
     * match. This can result in wrong things if there are files of
     * different content types but the same prefix (e.g. foo.gif and foo.html)
     * This code will pick the first one it finds. Better than a Not Found,
     * though.
     */

    rewinddir(dirp);

    dotloc = ind(bad, '.');
    if (dotloc == -1)
      dotloc = strlen(bad);

    while ((dir_entry = readdir (dirp))) {
      int entloc = ind(dir_entry->d_name, '.');
      if (entloc == -1)
	entloc = strlen(dir_entry->d_name);

      if ((dotloc == entloc) && !strncasecmp(bad, dir_entry->d_name, dotloc)) {
	/* Wow... we found us a mispelling. Construct a fixed url */
	char *nuri = pstrcat(r->pool, url, dir_entry->d_name, r->path_info,
			     NULL);
	char *ref = table_get(r->headers_in, "Referer");
	
	table_set(r->headers_out, "Location", construct_url(r->pool,
		 nuri, r->server));
	log_error(pstrcat(r->pool, "Fixed spelling: ", r->uri, " to ", nuri,
			  ref ? " from " : NULL, ref, NULL), r->server);
	closedir(dirp);
	return REDIRECT;
      }
    }

    closedir(dirp);

    return OK;
}

module speling_module = {
   STANDARD_MODULE_STUFF,
   NULL,			/* initializer */
   NULL,			/* create per-dir config */
   NULL,			/* merge per-dir config */
   create_speling_config,    	/* server config */
   NULL,		       	/* merge server config */
   speling_cmds,       		/* command table */
   NULL,			/* handlers */
   NULL,			/* filename translation */
   NULL,			/* check_user_id */
   NULL,			/* check auth */
   NULL,			/* check access */
   NULL,			/* type_checker */
   check_speling,     		/* fixups */
   NULL				/* logger */
};
