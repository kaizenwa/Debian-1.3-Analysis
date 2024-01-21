
/* ====================================================================
 * Copyright (c) 1995 The Apache Group.  All rights reserved.
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
 * http_uri_remap.c: Stuff for dealing with directory uri_remap_entries
 * 
 * Version 0.6 Feb 1996
 * 
 * Dirk.vanGulik@jrc.it, for the http://ewse.ceo.org European Wide Service
 * Exchange, which is a project funded by the http://www.ceo.org Center for
 * Earth Observation.
 *
 * Added to commands :
 *  
 * Rename URI URI
 * Mother URI
 *
 * Which allow one to one mapping on entry URI level. This allows
 * you to move around pages within say an authentification structure
 * or to easily force a 'null' request to be handled by a URI much
 * deeper in the structure.
 *
 * Example of use: (in the srm.conf file)
 *
 * Mother /home_page.html
 * Rename /home.htm  /home_page.html
 * Rename /Home.htm  /home_page.html
 *
 * This maps the /home.htm and Home.html onto /home_page.html. Furthermore
 * it connects any 'empty' URI, such as '', '/' or '/.' into the local
 * /home_page.html. 
 *
 * The advantage of using the above, rather than a large number of symbolic
 * links and clever (Script)Alias-es is that it can also be used to map
 * pages into a different security domain. See http://ewse.ceo.org for an
 * example. You can not do this with Alias as it compares partial strings.
 *
 * 0.3 First oparational release on ewse.ceo.org.
 * 0.4 DW added Mother command
 * 0.5 DW rename 'map' -> 'uri_reuri_remap'
 * 0.6 DW error trapping.
 */

#include "httpd.h"
#include "http_config.h"

typedef struct {
    char *real;
    char *fake;
    char *forced_type;
} uri_remap_entry;

typedef struct {
    char *realname;
} mother_remap_entry;

typedef struct {
    array_header *uri_remap_entries;
    array_header *mother_remap_entries;
} uri_remap_server_conf;

module uri_remap_module;

void *create_uri_remap_config (pool *p, server_rec *s)
{
    uri_remap_server_conf *a =
      (uri_remap_server_conf *)pcalloc (p, sizeof(uri_remap_server_conf));

    /* barf upon an error */
    if (!a) 
	return NULL;

    a->uri_remap_entries = make_array (p, 20, sizeof(uri_remap_entry));
    a->mother_remap_entries = make_array (p, 20, sizeof(mother_remap_entry));
    return a;
}

void *merge_uri_remap_config (pool *p, void *basev, void *overridesv)
{
    uri_remap_server_conf *a =
	(uri_remap_server_conf *)pcalloc (p, sizeof(uri_remap_server_conf));

    uri_remap_server_conf *base = (uri_remap_server_conf *)basev,
	*overrides = (uri_remap_server_conf *)overridesv;

    /* barf upon an error */
    if (!a) 
	return NULL;

    a->uri_remap_entries = append_arrays (p, overrides->uri_remap_entries, base->uri_remap_entries);
    a->mother_remap_entries = append_arrays (p, overrides->mother_remap_entries, base->mother_remap_entries);
    return a;
}

char *add_uri_rename(cmd_parms *cmd, void *dummy, char *f, char *r)
{
    server_rec *s = cmd->server;
    uri_remap_server_conf *conf =
        (uri_remap_server_conf *)get_module_config(s->module_config,&uri_remap_module);

    uri_remap_entry *new = push_array (conf->uri_remap_entries);
    if (!new)
	return "uri_remap: allocation problem";
    
    new->fake = f; new->real = r; new->forced_type = cmd->info;
    return NULL;
}

char *add_uri_mother(cmd_parms *cmd, void *dummy, char *realname)
{
    server_rec *s = cmd->server;
    uri_remap_server_conf *conf =
        (uri_remap_server_conf *)get_module_config(s->module_config,&uri_remap_module);

    mother_remap_entry *new = push_array (conf->mother_remap_entries);

    if (!new)
	return "uri_remap: allocation problem";

    new->realname = realname;

    return NULL;
}

command_rec uri_remap_cmds[] = {
{ "Rename", add_uri_rename, NULL, RSRC_CONF, TAKE2, 
    "Please supply old URI and new URI"},
{ "Mother", add_uri_mother, NULL, RSRC_CONF, TAKE1, 
    "Please supply a URI to map the empty entry to"},
{ NULL }
};

int uri_map_matches (char *uri, char *uri_remap_fakename)
{
    char *end_fakename 
		= uri_remap_fakename + strlen (uri_remap_fakename),
   	 *renamep 
		= uri_remap_fakename, 
	 *urip = uri;

    while (renamep < end_fakename) {

	if (*renamep == '/') {
	    /* any number of '/' in the uri_remap matches any number in
	     * the supplied URI, but there must be at least one...
	     */
	    if (*urip != '/') return 0;
	    
	    while (*renamep == '/') ++ renamep;
	    while (*urip == '/') ++ urip;
	}
	else {
	    /* Other characters are compared literally */
	    if (*urip++ != *renamep++) return 0;
	}
    }

    /* Check last uri_remap path component matched all the way 
     * to the very end (and in (only) this it is different to the
     * alias module :-)
     */

    if (renamep == '\0' && *urip == '\0')  return urip - uri;

    return 0;
    }

char *walk_uri_mapping_list (request_rec *r, array_header *uri_remap_entries, int doesc)
{
    uri_remap_entry *entries = (uri_remap_entry *)uri_remap_entries->elts;
    int i;
    
    for (i = 0; i < uri_remap_entries->nelts; ++i) {
        uri_remap_entry *p = &entries[i];
        int l = uri_map_matches (r->uri, p->fake);

        if (l > 0) {
	    if (p->forced_type)
		table_set (r->notes, "uri_remap-forced-type", p->forced_type);
			   
	    if (doesc) {
		char *escurl;
		/* would like to use os_escape_path here, but can't */
		escurl = escape_uri(r->pool, r->uri + l);
		return pstrcat(r->pool, p->real, escurl, NULL);
	    } else
		return pstrcat(r->pool, p->real, r->uri + l, NULL);
        }
    }

    return NULL;
}

int translate_uri_remapping(request_rec *r)
{
    void *sconf = r->server->module_config;
    uri_remap_server_conf *conf =
        (uri_remap_server_conf *)get_module_config(sconf, &uri_remap_module);
    char *ret;

    /* Not sure wether it is up to *this* module
     * to deceide on this; this smells like implementing
     * part of the http spec in the wrong place.
     *    
     * if (r->uri[0] != '/' && r->uri[0] != '\0') 
     *  return BAD_REQUEST;
     *  return DECLINED;
     */

    /* try mum first.. triggers on
     * empty, '/' and '/.'
     */

    if (conf->mother_remap_entries) 
	if ( (r->uri[0] == '\0') || 
	     (r->uri[0] == '/' && r->uri[1] == '\0') ||
	     (r->uri[0] == '/' && r->uri[1] == '.' && r->uri[2] == '\0') 
	    ) if (ret=pstrdup(r->pool,(((mother_remap_entry *) conf->mother_remap_entries->elts ) ->realname))) { 
			/* Do we want to log this ?! */
			r->uri = ret;
			return DECLINED;
			};

    /* try the usual real renaming 
     */

    if (conf->uri_remap_entries) 
       if ((ret = walk_uri_mapping_list (r, conf->uri_remap_entries, 0)) != NULL) {
	/* Do we want to log this ?! */
        r->uri = ret;
        return DECLINED;
    	}
    return DECLINED;
}



module uri_remap_module = {
   STANDARD_MODULE_STUFF,
   NULL,			/* initializer */
   NULL,			/* dir config creater */
   NULL,			/* dir merger --- default is to override */
   create_uri_remap_config,	/* server config */
   merge_uri_remap_config,	/* merge server configs */
   uri_remap_cmds,		/* command table */
   NULL,			/* handlers */
   translate_uri_remapping,	/* filename translation */
   NULL,			/* check_user_id */
   NULL,			/* check auth */
   NULL,			/* check access */
   NULL,			/* type_checker */
   NULL,			/* fixups */
   NULL				/* logger */
};
