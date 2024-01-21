/*
 * lftp and utils
 *
 * Copyright (c) 1996-1997 by Alexander V. Lukyanov (lav@yars.free.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>
#include <stdio.h>
#include <string.h>
#include "xalloca.h"
#include "url.h"

/* parses url and returns 1 if successfully, otherwise 0 */
int   parse_url(char *url,struct url_components *res)
{
   int n;

   strcpy(res->path,"");   // default path
   strcpy(res->user,"");   // default user
   res->port=-1;	   // default port

   /* first try: protocol://host/path */
   n=sscanf(url,"%[a-z]://%[^/]%[^\n]",res->proto,res->host,res->path);
   if(n==3 || n==2)
      goto check_user_in_host;
   if(n==EOF)
   {
      n=sscanf(url,"%[a-z]://%[^/]",res->proto,res->host);
      if(n==2)
	 goto check_user_in_host;
   }

   /* second try: ftp.some.domain/path */
   if(!strncmp(url,"ftp.",4))
   {
      sscanf(url,"%[^.]",res->proto);
      n=sscanf(url,"%[^/]%[^\n]",res->host,res->path);
      if(n==2 || n==1)
	 goto check_user_in_host;
      n=sscanf(url,"%[^/]",res->host);
      goto check_user_in_host;
   }

   return 0;

check_user_in_host:
   // check if user name is specified
   char *s=(char*)alloca(strlen(res->host));

   // user@host:port
   n=sscanf(res->host,"%[^@]@%[^:]:%d",res->user,s,&res->port);
   if(n==3 || n==2)
   {
      strcpy(res->host,s);
      return 1;
   }
   // user@host
   n=sscanf(res->host,"%[^@]@%[^:]",res->user,s);
   if(n==2)
   {
      strcpy(res->host,s);
      return 1;
   }

   // no user name found -- restore the field
   strcpy(res->user,"");

   // host:port
   n=sscanf(res->host,"%[^:]:%d",s,&res->port);
   if(n==2)
   {
      strcpy(res->host,s);
      return 1;
   }
   return 1;
}
