
/*-
 * Copyright (c) 1995 The Apache Group. All rights reserved.
 * 
 *
 * Apache httpd license
 * ====================
 * 
 *
 * This is the license for the Apache Server. It covers all the
 * files which come in this distribution, and should never be removed.
 * 
 * The "Apache Group" has based this server, called "Apache", on
 * public domain code distributed under the name "NCSA httpd 1.3".
 * 
 * NCSA httpd 1.3 was placed in the public domain by the National Center 
 * for Supercomputing Applications at the University of Illinois 
 * at Urbana-Champaign.
 * 
 * As requested by NCSA we acknowledge,
 * 
 *  "Portions developed at the National Center for Supercomputing
 *   Applications at the University of Illinois at Urbana-Champaign."
 *
 * Copyright on the sections of code added by the "Apache Group" belong
 * to the "Apache Group" and/or the original authors. The "Apache Group" and
 * authors hereby grant permission for their code, along with the
 * public domain NCSA code, to be distributed under the "Apache" name.
 * 
 * Reuse of "Apache Group" code outside of the Apache distribution should
 * be acknowledged with the following quoted text, to be included with any new
 * work;
 * 
 * "Portions developed by the "Apache Group", taken with permission 
 *  from the Apache Server   http://www.apache.org/apache/   "
 *
 *
 * Permission is hereby granted to anyone to redistribute Apache under
 * the "Apache" name. We do not grant permission for the resale of Apache, but
 * we do grant permission for vendors to bundle Apache free with other software,
 * or to charge a reasonable price for redistribution, provided it is made
 * clear that Apache is free. Permission is also granted for vendors to 
 * sell support for for Apache. We explicitly forbid the redistribution of 
 * Apache under any other name.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 */
 
 
/* HACK hack HACK

   Author: Rob Hartill   hartill@lanl.gov
   
   Module for Apache URL counting

   THIS IS JUST AN EXAMPLE OF HOW TO WRITE A COUNTER MODULE IN
   APACHE. I HAVE NO INTENTION OF SUPPORTING THIS. IT'S NOT MEANT
   TO BE EFFICIENT OR WELL DESIGNED. FEEL FREE TO USE IT AS A BASE
   FOR SOMETHING BETTER, THEN HAVE THE BETTER MODULE PUT HERE
   INSTEAD.
   
   CounterLog   is a new config parameter, set it to point to
     a file which contains URL counts.
   The format is
   
   URL-to-matchNNNNNNNNNN\n
   
   e.g.
   
   /index.html0000000000
   /logo.gif0000012345
   /some/subdir/index.gif0000000003
   
   The number at the end is a 10 digit count. Pad with 0s to make it 10 digits
   This module will scan the file for a matching URL, then increment a 
   count. It also creates a "CGI" variable called "URL_COUNTER" for use
   in CGI and includes.
   
   
*/
   
    
#define DEFAULT_CNTRLOG "/etc/apache/counter.log"


#include "httpd.h"
#include "http_config.h"

module counter_module;

static int xfer_flags = ( O_RDWR | O_CREAT );
static mode_t xfer_mode = ( S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH );

typedef struct {
    char *fname;
    int counter_fd;
} counter_state;


static int line_read (int fd, char *buffer, int bufsz)
{
    int rv, orig_sz = bufsz;
    char *cptr = buffer;
   
    do {
        rv = read (fd, cptr, 1);
        if (rv > 0) {
        /*fprintf(stderr, "R(%c)",*cptr);*/
            bufsz --;
            if (*cptr++ == '\n') break;
        }
    } while (rv > 0 && bufsz > 0);
    *cptr = '\0';
       
    return rv < 0? rv : orig_sz - bufsz;
}

void *make_counter_state (pool *p, server_rec *s)
{
    counter_state *cls =
      (counter_state *)palloc (p, sizeof (counter_state));

    cls->fname = DEFAULT_CNTRLOG;
    cls->counter_fd = -1;

    return (void *)cls;
}

char *set_counter (cmd_parms *parms, void *dummy, char *arg)
{
    counter_state *cls = get_module_config (parms->server->module_config,
					       &counter_module);
  
    cls->fname = arg;
    return NULL;
}

command_rec counter_cmds[] = {
{ "CounterLog", set_counter, NULL, RSRC_CONF, TAKE1,
    "the filename of the counter log" },
{ NULL }
};


void open_counter (server_rec *s, pool *p)
{
    counter_state *cls = get_module_config (s->module_config,
					       &counter_module);
  
    char *fname = server_root_relative (p, cls->fname);
    
    if (cls->counter_fd > 0) return; /* virtual log shared w/main server */
    
    if((cls->counter_fd = popenf(p, fname, xfer_flags, xfer_mode)) < 0) {
        fprintf(stderr,"httpd: could not open counter log file %s.\n", fname);
        perror("open");
        exit(1);
    }
}

void init_counter (server_rec *s, pool *p)
{
    for (; s; s = s->next) open_counter (s, p);
}

int look_for_counter_then_increment(request_rec *orig)
{
    counter_state *cls = get_module_config (orig->server->module_config,
					       &counter_module);
  
    char str[HUGE_STRING_LEN];
    request_rec *r;
    int line_len, uri_len;

    /* Some comments need to go here */
        
    for (r = orig; r->next; r = r->next)
        continue;
        
    if (!r->the_request) return OK;    
           /* Stops includes incrementing requested URLs counter */
       
    uri_len = strlen(r->uri); 

    lseek(cls->counter_fd,0, 0);   /* jump to start of counter log */

    /* Search for the URL */
    while (line_read(cls->counter_fd, str, HUGE_STRING_LEN) >0) {
    
       line_len = strlen(str);
       
       if (line_len <12 || (line_len-11) < uri_len) continue;


       if (!strncmp(str, r->uri, line_len-11)) {
           int count = atoi(str+line_len-10);      
           lseek(cls->counter_fd, -11, 1);   /* reverse 11 bytes */
           sprintf(str, "%010d", ++count);
         
           write(cls->counter_fd, str, 10);
         
           sprintf(str, "%d", count);
           table_set (r->subprocess_env, "URL_COUNTER", str);
       
           return OK;
       }
    }
    return OK;
}

module counter_module = {
   STANDARD_MODULE_STUFF,
   init_counter,		   /* initializer */
   NULL,			   /* create per-dir config */
   NULL,			   /* merge per-dir config */
   make_counter_state,	           /* server config */
   NULL,			   /* merge server config */
   counter_cmds,		   /* command table */
   NULL,			   /* handlers */
   NULL,			   /* filename translation */
   NULL,			   /* check_user_id */
   NULL,			   /* check auth */
   NULL,			   /* check access */
   NULL,			   /* type_checker */
   look_for_counter_then_increment,/* fixups */
   NULL                            /* logger */
};
