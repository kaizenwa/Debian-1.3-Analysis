
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

/* mod_block.c - by Alexei Kosut <akosut@organic.com> June, 1996
 *
 * This module allows you to block access to your site that was
 * referred to from a certain URL. This isn't a 100%-foolproof
 * way of preventing access to your site, but if someone has a link
 * to a page or image of yours, that you don't like, it's a pretty
 * good way of blocking access from that location.
 *
 * Example:
 * BlockReferer www.organic.com
 * BlockReferer netscape.com/comprod/sales/index.html
 */


#include "httpd.h"
#include "http_config.h"

module block_module;

typedef struct {
    table *block_referer_list;
} block_state;

void *make_block_state (pool *p, server_rec *s)
{
    block_state *cls =
      (block_state *)palloc (p, sizeof (block_state));

    cls->block_referer_list = make_table(p, 1);
    return (void *)cls;
}

void *merge_block_state (pool *p, void *basev, void *overridesv)
{
   block_state *a = (block_state *)pcalloc (p, sizeof (block_state));
   block_state *base = (block_state *)basev,
     *overrides = (block_state *)overridesv;

   a->block_referer_list = overlay_tables (p, overrides->block_referer_list,
					  base->block_referer_list);

   return a;
}

char *add_referer_block (cmd_parms *parms, void *dummy, char *arg)
{
    block_state *cls = get_module_config (parms->server->module_config,
					      &block_module);

    table_set(cls->block_referer_list, arg, arg);
    
    return NULL;
}

command_rec block_cmds[] = {
{ "BlockReferer", add_referer_block, NULL, RSRC_CONF, ITERATE,
    "referer hostnames to block" },
{ NULL }
};

int proc_block_trans(request_rec *r)
{
    block_state *cls = get_module_config (r->server->module_config,
					  &block_module);
  
    char *referer = table_get(r->headers_in, "Referer");
    array_header *list_arr = table_elts(cls->block_referer_list);
    table_entry *list = (table_entry *)list_arr->elts;
    int i;

    if (!referer || !list)
        return DECLINED;    

    for (i = 0; i < list_arr->nelts; ++i) {
        char *str = list[i].key;

	if (str && strstr(referer, str))
	  return FORBIDDEN;
    }

    return DECLINED;
}


module block_module = {
   STANDARD_MODULE_STUFF,
   NULL,			/* initializer */
   NULL,			/* create per-dir config */
   NULL,			/* merge per-dir config */
   make_block_state,		/* server config */
   merge_block_state,	       	/* merge server config */
   block_cmds,			/* command table */
   NULL,			/* handlers */
   NULL,			/* filename translation */
   NULL,			/* check_user_id */
   NULL,			/* check auth */
   proc_block_trans,		/* check access */
   NULL,			/* type_checker */
   NULL,			/* fixups */
   NULL				/* logger */
};
