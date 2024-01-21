/*
 * mod_trailer.c
 *
 *  An Apache module that adds document trailer functionality.
 *
 *  A document trailer is a text or HTML file that is appended to all
 *  objects with a certain prefix in their URI.  This module allows
 *  you to assign a trailer file to a URI with the config file directive
 *
 *  "Trailer <URI prefix> <trailer filename>"  
 *
 *  For example, to append a trailer with a filename of
 *  "/usr/local/htdocs/trailer.html" to all objects with URIs beginning
 *  with "/add/trailers/here/" you would place the following in your srm.conf:
 *
 *  Trailer /add/trailers/here/ /usr/local/htdocs/trailer.html
 *
 *
 *  This is only my second Apache module, and is much more complex than the
 *  first one I wrote (which was a simple authentication module to 
 *  authenticate users from the unix password file).  If you have any 
 *  comments or suggestions as to how this might be improved, please drop me
 *  some email.
 *
 *  If someone wanted some fancy trailers, it would be pretty simple to
 *  have this append server-parsed-html trailers instead of just plain
 *  text or html.  Some other features that might be useful would be a
 *  per directory default trailer (perhaps a .trailer file that gets appended
 *  to all files in its directory) or a feature that appends a specified
 *  trailer to all documents with a certain extension.
 *
 *  Feel free to use this module however you like, and distribute it freely.
 *  I only ask that you do not remove my name from the credits.  If you
 *  modify this code and distribute it, please include a list of revisions
 *  in the history, along with your name.  And if you find this module
 *  useful, send me some email and let me know!
 *
 *  Nathan W. Schrenk (nschrenk@neog.com) 11/10/95
 *
 *  HISTORY:
 *
 *  v 0.1 - 11/10/95 - mod_trailer.c created.  This is probably full of bugs,
 *          and perhaps can be done in a much better way.  But hey, it
 *          seems to work for me.
 *
 *  v 0.2 - 11/11/95 - A bug that popped up with the POST request method was
 *          fixed and now only the most specific URI prefix is matched.
 *          You can now specify overlapping trailers, and the trailer
 *          associated with the longest prefix will be used).
 *
 *  v 0.3unoff1 - 01/19/97 - Fixed bug which prevented module from
 *          working with mod_dir.  -- Johnie Ingram <johnie@debian.org>
 */

#include "httpd.h"
#include "http_config.h"
#include "http_log.h"
#include "http_main.h"
#include "http_protocol.h"
#include "http_request.h"

#define TRAILER_MAGIC_TYPE "text/x-trailer-appended-html"
#define MAX_TRAILERS 40

/* trailer_entry = a struct to hold trailer alias info */

typedef struct {
  char *prefix;   /* prefix in which to use trailer */
  char *t_file;   /* filename of trailer to use */
} trailer_entry;


module trailer_module;

/*
 * create_trailer_config() and merg_trailer_config() handle trailer
 * configuration stuff.  Pretty self explanatory...
 */

void *create_trailer_config(pool *p, server_rec *s)
{
  array_header *t_cf = (array_header *) pcalloc(p,sizeof(array_header));

  t_cf = make_array(p,MAX_TRAILERS,sizeof(trailer_entry));

  return t_cf;
}

void *merge_trailer_config(pool *p, void *basev, void *overridesv)
{
  array_header *t_cf = (array_header *) pcalloc(p,sizeof(array_header));
  array_header *base = (array_header *) basev,
               *overrides = (array_header *) overridesv;

  t_cf = append_arrays(p,overrides,base);

  return t_cf;
}


/*
 * add_trailer() adds trailer config file info into the server config
 */

char *add_trailer(cmd_parms *cmd, void *dummy, char *p, char *f)
{
  server_rec *s = cmd->server;
  array_header *trailer_conf = 
    (array_header *) get_module_config(s->module_config,&trailer_module);
  trailer_entry *new_trailer = push_array(trailer_conf);
  
  new_trailer->prefix = p;
  new_trailer->t_file = f;

  return NULL;
}
  
command_rec trailer_cmds[] = {
  {"Trailer", add_trailer, NULL, RSRC_CONF, TAKE2,
     "A URI prefix and a trailer filename"},
  { NULL }
};

/*
 * find_trailer() returns the trailer filename for request r, or NULL
 * if the trailer cannot be determined.  This has to give the trailer
 * of the most specific prefix, so we can't stop when we find the first
 * one that matches.
 */

char *find_trailer(request_rec *r)
{
  void *sconf = r->server->module_config;
  array_header *t_array = 
    (array_header *) get_module_config(sconf,&trailer_module);
  trailer_entry *entries = (trailer_entry *) t_array->elts, *t;
  int i,l,longest = 0;
  char *trailer = NULL;

  for (i = 0; i < t_array->nelts; i++) {
    t = &entries[i];
    l = strlen(t->prefix);
    if (!strncmp(r->uri,t->prefix,l) && (l > longest)) {
      longest = l;
      trailer = t->t_file;
    }
  }
  
  return trailer;
}

/*
 * check_for_trailer() is run in the fixup stage to check
 * and see if the requested object has a trailer.  If so, the request's
 * real content type is noted in r->notes, then is set to
 * TRAILER_MAGIC_TYPE.
 */

int check_for_trailer(request_rec *r)
{
  if (!r->main && r->method_number == M_GET)  
    if (find_trailer(r)) {
      /* check to see if this is already typed as a trailer to
	 prevent recursively calling the trailer handler -- does this
	 ever do anything?  */
      
      if (!strcmp(r->content_type,TRAILER_MAGIC_TYPE))
	return DECLINED;

      /* make a note of the real content type for the trailer handler */
      
      table_set(r->notes,"pre-trailer-type",r->content_type);
      r->content_type = TRAILER_MAGIC_TYPE;
      return OK;
    }
  
  return DECLINED;
}

/*
 * trailer_handler() sends out the document requested and appends
 * the trailer file to it if the requested document's content type begins
 * with "text/"
 */

int trailer_handler(request_rec *r)
{
  int subr_status;
  char *t_file,*old_type,buf[MAX_STRING_LEN];
  FILE *tf;
  struct stat tf_stat;
  request_rec *subr = sub_req_lookup_uri(r->uri,r);
  
  /* set subr's content_type to the old content type */
  old_type = table_get(r->notes,"pre-trailer-type");
  if (old_type)
    subr->content_type = old_type;

  subr_status = run_sub_req(subr);  /* run subrequest */

  if ((subr_status == OK) && !strncmp(subr->content_type,"text/",5)) {
    t_file = find_trailer(r);
    if (t_file && (stat(t_file,&tf_stat) || (!tf_stat.st_mode))) {
      sprintf(buf,"Trailer file %s does not exist",t_file);
      log_reason(buf,r->uri,r);
      
      return subr_status;
    }
    
    tf = pfopen(r->pool,t_file,"r");
    if (tf == NULL) {
      log_reason("file permissions deny server access to trailer file",
		 t_file, r);
      return subr_status;
    }
    
    soft_timeout ("send trailer", r);
    if (!r->header_only)
      send_fd (tf, r);
    pfclose (r->pool,tf);

    r->headers_out = subr->headers_out;
    r->err_headers_out = subr->headers_out;

    return OK;
  }

  /* copy headers out to handle redirect, etc */

  if (subr_status == REDIRECT) {
    r->headers_out = subr->headers_out;
    r->err_headers_out = subr->headers_out;
  }

  return subr_status;
}
  
handler_rec trailer_handlers[] = {
  { TRAILER_MAGIC_TYPE, trailer_handler },
  { NULL }
};

module trailer_module = {
   STANDARD_MODULE_STUFF,
   NULL,			/* initializer */
   NULL,			/* dir config creater */
   NULL,			/* dir merger --- default is to override */
   create_trailer_config,       /* server config */
   merge_trailer_config,       	/* merge server configs */
   trailer_cmds,	       	/* command table */
   trailer_handlers,	       	/* handlers */
   NULL,	                /* filename translation */
   NULL,			/* check_user_id */
   NULL,			/* check auth */
   NULL,			/* check access */
   NULL,            	        /* type_checker */
   check_for_trailer,		/* fixups */
   NULL				/* logger */
};
