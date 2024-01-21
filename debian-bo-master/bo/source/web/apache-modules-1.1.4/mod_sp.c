
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
 * mod_sp.c: server-push
 *
 * Sameer Parekh
 */

#include "httpd.h"
#include "http_config.h"
#include "http_request.h"
#include "http_core.h"
#include "http_protocol.h"
#include "http_main.h"
#include "http_log.h"
#include "util_script.h"

#define BOUNDARY "2387lcskdncluvw3doiqefdup9w47dwechsdkcsdfv"

/* Process the animation file */
void do_animation(FILE *f, request_rec *r)
{
  int errstatus, time;
  char buf[MAX_STRING_LEN];
  char fname[MAX_STRING_LEN], ctype[MAX_STRING_LEN];
  FILE *fp;
  struct stat st;

  while(fgets(buf, MAX_STRING_LEN, f))
    {
      /* parse 'buf' */
      if(sscanf(buf, "File: %s Type: %s", fname, ctype) == 2)
	{
	  /* Get a file */

	  /* Check permissions, etc. */
	  if(stat(fname, &st))
	    {
	      log_reason("can't stat", fname, r);
	      continue;
	    }
	  if(st.st_uid != r->finfo.st_uid)
	    {
	      log_reason("owners don't match", fname, r);
	      continue;
	    }
	  
	  fp = fopen(fname, "r");
	  if(fp == NULL)
	    {
	      log_reason("can't open", fname, r);
	      continue;
	    }
	  /* Send the file */
	  rprintf(r, "--%s\n", BOUNDARY);
	  rprintf(r, "Content-type: %s\n\n", ctype);

	  send_fd (fp, r);
	  rputc('\n', r);
	  fclose(fp);
	}
      /* Sleep directive */
      else if(sscanf(buf, "Sleep: %d", &time) == 1)
	{
	  sleep(time);
	}
    }
  rprintf(r, "--%s--\n", BOUNDARY);
}

int sp_handler (request_rec *r)
{
    int errstatus, ret;
    FILE *f;

    if (r->method_number != M_GET) return DECLINED;
    if (r->finfo.st_mode == 0) {
	log_reason("File does not exist", r->filename, r);
	return NOT_FOUND;
    }

    f = fopen (r->filename, "r");

    if (f == NULL) {
        log_reason("file permissions deny server access", r->filename, r);
        return FORBIDDEN;
    }

    soft_timeout ("send", r);

    /* Set content-type */
    r->content_type = (char *) palloc(r->pool, MAX_STRING_LEN);
    sprintf(r->content_type, "multipart/x-mixed-replace;boundary=%s\n",
	  BOUNDARY);

    send_http_header (r);
    if (!r->header_only) do_animation(f, r);
    fclose(f);
    return OK;
}

handler_rec sp_handlers[] = {
{ "application/x-httpd-serverpush", sp_handler },
{ NULL }
};

module sp_module = {
   STANDARD_MODULE_STUFF,
   NULL,			/* initializer */
   NULL,			/* dir config creater */
   NULL,			/* dir merger --- default is to override */
   NULL,			/* server config */
   NULL,			/* merge server config */
   NULL,			/* command table */
   sp_handlers,    		/* handlers */
   NULL,			/* filename translation */
   NULL,			/* check_user_id */
   NULL,			/* check auth */
   NULL,			/* check access */
   NULL,			/* type_checker */
   NULL,			/* fixups */
   NULL				/* logger */
};
