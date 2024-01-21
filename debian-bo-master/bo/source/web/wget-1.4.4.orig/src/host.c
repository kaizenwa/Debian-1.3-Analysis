/* Dealing with host names.
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* $Id: host.c,v 1.1.1.1.2.2 1997/02/15 19:22:57 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <assert.h>
#include <sys/types.h>

#ifdef WINDOWS
# include <winsock.h>
#else
# include <sys/socket.h>
# include <netinet/in.h>
# include <arpa/inet.h>
# include <netdb.h>
#endif /* WINDOWS */

#ifdef HAVE_SYS_SYSTEMINFO_H
#  include <sys/systeminfo.h>
#endif

#include "wget.h"
#include "options.h"
#include "utils.h"
#include "url.h"
#include "host.h"

/* These are often not declared in header files, so I do it. */
#ifndef __cplusplus
int gethostname();
int getdomainname();
#endif /* __cplusplus */

extern struct options opt;
host_t *hlist;

/* The same as gethostbyname, but supports internet addresses of the
   form N.N.N.N */
struct hostent *
ngethostbyname(const char *name)
{
   struct hostent *hp;
   unsigned long addr;

   addr = (unsigned long)inet_addr(name);
   if ((int)addr != -1)
      hp = gethostbyaddr((char *)&addr, sizeof (addr), AF_INET);
   else
      hp = gethostbyname(name);
   return hp;
}

/* Store the address of host, internet-style. First check for it in
   the host hash, and (if not found), use ngethostbyname to get it.

   The function returns 1 on successful finding of the hostname, 0
   otherwise. */
int
store_hostaddress(unsigned char *where, const char *hostname)
{
   host_t *t;
   unsigned long addr;
   struct hostent *hptr;
   struct in_addr in;
   char *inet_s;

   /* If the address is of the form d.d.d.d, there will be no trouble
      with it. */
   addr = (unsigned long)inet_addr(hostname);
   if ((int)addr == -1)
   {
      /* If it is not of that form, try to find it in the cache. */
      t = search_host(hlist, hostname);
      if (t)
	 addr = (unsigned long)inet_addr(t->realname);
   }
   /* If we have the numeric address, just store it. */
   if ((int)addr != -1)
   {
      /* This works on both little and big endian, since inet_addr
	 returns the address in the proper order.  It appears to work
	 on 64-bit machines too. */
      memcpy(where, &addr, 4);
      return 1;
   }
   /* Since all else has failed, let's try gethostbyname. Note that
      it's gethostbyname, not ngethostbyname, since we *know* the
      address is not numerical. */
   if (!(hptr = gethostbyname(hostname)))
      return 0;
   /* Copy the address of the host to socket description. */
   memcpy(where, hptr->h_addr_list[0], hptr->h_length);
   /* Now that we're here, we could as well cache the hostname for
      future use, as in realhost(). First, we have to look for it by
      address to know if it's already in the cache by another name. */
   memcpy(&in.s_addr, *hptr->h_addr_list, sizeof(in.s_addr));
   inet_s = nstrdup(inet_ntoa(in));
   t = search_address(hlist, inet_s);
   if (t) /* Found in the list, as realname. */
   {
      /* Set the default, 0 quality. */
      hlist = add_hlist(hlist, hostname, inet_s, 0);
      free(inet_s);
      return 1;
   }
   /* Since this is really the first time this host is encountered,
      set quality to 1. */
   hlist = add_hlist(hlist, hostname, inet_s, 1);
   free(inet_s);
   return 1;
}

/* Add a host to the host list. The list is sorted by addresses. For
   equal addresses, the entries with quality should bubble towards the
   beginning of the list. */
host_t *
add_hlist(host_t *l, const char *nhost, const char *nreal, int quality)
{
   host_t *t, *old, *beg;
   int cmp;

   /* The entry goes to the beginning of the list if the list is empty
      or the order requires it. */
   if (!l || (cmp = strcmp(nreal, l->realname) < 0))
   {
      t = (host_t *)nmalloc(sizeof(host_t));
      t->hostname = nstrdup(nhost);
      t->realname = nstrdup(nreal);
      t->quality = quality;
      t->next = l;
      return t;
   }

   beg = l;
   /* Second two one-before-the-last element. */
   while (l->next)
   {
      old = l;
      l = l->next;
      cmp = strcmp(nreal, l->realname);
      if (cmp >= 0)
	 continue;
      /* If the next list element is greater than s, put s between the
	 current and the next list element. */
      t = (host_t *)nmalloc(sizeof(host_t));
      old->next = t;
      t->next = l;
      t->hostname = nstrdup(nhost);
      t->realname = nstrdup(nreal);
      t->quality = quality;
      return beg;
   }
   t = (host_t *)nmalloc(sizeof(host_t));
   t->hostname = nstrdup(nhost);
   t->realname = nstrdup(nreal);
   t->quality = quality;
   /* Insert the new element after the last element. */
   l->next = t;
   t->next = NULL;
   return beg;
}

/* Search the linked list by hostname. Return the entry, if found, or
   NULL otherwise. The search is case-insensitive. */
host_t *
search_host(host_t *l, const char *host)
{
   while (l)
   {
      if (strcasecmp(l->hostname, host) == 0)
	 return l;
      l = l->next;
   }
   return NULL;
}

/* Like search_host, but searches by address. */
host_t *
search_address(host_t *l, const char *address)
{
   int cmp;

   while (l)
   {
      if ((cmp = strcmp(l->realname, address)) == 0)
	 return l;
      else if (cmp > 0)
	 return NULL;
      l = l->next;
   }
   return NULL;
}

/* This routine frees memory allocated by host linked list. */
void
free_hlist(host_t *l)
{
   host_t *p;

   while (l)
   {
      p = l->next;
      free(l->hostname);
      free(l->realname);
      free(l);
      l = p;
   }
}


/* The routine used to determine the "real" name of the host, as
   viewed by the program. If host is referenced by more than one name,
   "real" name is considered to be the first one.
   
   If the host cannot be found in the list of already dealed-with
   hosts, try with its INET address. If this fails too, add it to the
   list.  The routine does not call gethostbyname twice for the same
   host if it can possibly avoid it. */
char *
realhost(const char *host)
{
   host_t *l;
   struct in_addr in;
   struct hostent *hptr;
   char *inet_s;

#ifdef DEBUG
   if (opt.debug)
      fprintf(opt.lfile, "Checking for %s.\n", host);
#endif
   /* Look for the host, looking by the host name. */
   l = search_host(hlist, host);
   if (l && l->quality)              /* Found it with quality */
   {
      DEBUGP("It was already used, by that name.\n");
      /* Here we return l->hostname, not host, because of the possible
         case differences (e.g. jaGOR.srce.hr and jagor.srce.hr are
         the same, but we want the one that was first.  */
      return nstrdup(l->hostname);
   }
   else if (!l)                      /* Not found, with or without quality */
   {
      /* The fact that gethostbyname will get called makes it
	 necessary to store it to the list, to ensure that
	 gethostbyname will not be called twice for the same
	 string. However, the quality argument must be set
	 appropriately.
	 
	 Note that add_hlist must be called *after* the realname
	 search, or the quality would be always set to 0 */
      DEBUGP("This is the first time I hear about that host by that name.\n");
      hptr = ngethostbyname(host);
      if (!hptr)
	 return nstrdup(host);
      memcpy(&in.s_addr, *hptr->h_addr_list, sizeof(in.s_addr));
      inet_s = nstrdup(inet_ntoa(in));
   }
   else /* Found, without quality */
   {
      /* This case happens when host is on the list,
	 but not as first entry (the one with quality).
	 Then we just get its INET address and pick
	 up the first entry with quality. */
      DEBUGP("We've dealt with this host, but under a different name.\n");
      inet_s = nstrdup(l->realname);
   }
   
   /* Now we certainly have the INET address. The following
      loop is guaranteed to pick either an entry with
      quality (because it is the first), or nothing. */
   l = search_address(hlist, inet_s);
   if (l) /* Found in the list, as realname. */
   {
      /* Set the default, 0 quality. */
      hlist = add_hlist(hlist, host, inet_s, 0);
      free(inet_s);
      return nstrdup(l->hostname);
   }
   /* Since this is really the first time this host is encountered,
      set quality to 1. */
   hlist = add_hlist(hlist, host, inet_s, 1);
   free(inet_s);
   return nstrdup(host);
}

/* This routine compares two hostnames (out of URL-s if the arguments
   are URL-s), taking care of aliases.  It uses realhost to determine
   a unique hostname for each of two hosts. If simple_check is set,
   only strcmp is called. */
int
same_host(const char *u1, const char *u2)
{
   const char *s;
   char *p1, *p2;
   char *real1, *real2;

   /* Skip protocol, if present. */
   u1 += skip_url(u1);
   u2 += skip_url(u2);
   u1 += skip_proto(u1);
   u2 += skip_proto(u2);
   
   /* Skip username ans password, if present. */
   u1 += skip_uname(u1);
   u2 += skip_uname(u2);
   
   for (s = u1; *u1 && *u1 != '/' && *u1 != ':'; u1++);
   p1 = strdupdelim(s, u1);
   for (s = u2; *u2 && *u2 != '/' && *u2 != ':'; u2++);
   p2 = strdupdelim(s, u2);
#ifdef DEBUG
   if (opt.debug)
      fprintf(opt.lfile, "Comparing hosts %s and %s...\n", p1, p2);
#endif
   if (!strcasecmp(p1, p2))
   {
      free(p1);
      free(p2);
      DEBUGP("They are quite alike.\n");
      return 1;
   }
   else if (opt.simple_check)
   {
      free(p1);
      free(p2);
      DEBUGP("Since checking is simple, I'd say they are not the same.\n");
      return 0;
   }
   real1 = realhost(p1);
   real2 = realhost(p2);
   free(p1);
   free(p2);
   if (strcasecmp(real1, real2) == 0)
   {
      DEBUGP("They are alike.\n");
      free(real1);
      free(real2);
      return 1;
   }
   else
   {
      DEBUGP("They are not the same.\n");
      free(real1);
      free(real2);
      return 0;
   }
}

/* Determine whether a URL is acceptable to be followed,
   according to a list of domains to accept */
int
accept_domain(urlinfo *u)
{
   if (opt.domains)
   {
      assert(u->host != NULL);
      if (!sufmatch((const char **)opt.domains, u->host))
	 return 0;
   }
   if (opt.exclude_domains)
   {
      assert(u->host != NULL);
      if (sufmatch((const char **)opt.exclude_domains, u->host))
	 return 0;
   }
   return 1;
}

/* Check whether a host is matched in the list of domains */
int
sufmatch(const char **list, const char *what)
{
   int i, j, k, lw;

   lw = strlen(what);
   for (i = 0; list[i]; i++)
   {
      for (j = strlen(list[i]), k = lw; j >= 0 && k >= 0; j--, k--)
	 if (tolower(list[i][j]) != tolower(what[k]))
	    break;
      /* The domain must be first to reach to beginning. */
      if (j == -1)
	 return 1;
   }
   return 0;
}

/* Get email address of the form username@host.domain suitable for
   anonymous FTP passwords. If you have problems, hard-code your
   hostname and domainname by defining MY_HOST and MY_DOMAIN in
   config.h.

   If none of the available methods of getting host name and domain
   name works, the function returns username@. If it cannot get
   username, the program quits. */
char *
ftp_getaddress(void)
{
   static char address[256];
   int i, pos;
   static int first = 1;

   /* Do it only the first time, since it won't change. */
   if (first)
   {
      first = 0;
      if (!mycuserid(address))
      {
	 if (!opt.quiet)
	    fprintf(stderr, "Cannot determine user-id.\n");
	 exit(1);
      }
      i = strlen(address);
      address[i++] = '@';
      address[i] = '\0';
      pos = i;
      /* pos holds the position after '@'. */
#ifdef MY_HOST
      strcpy(address + i, MY_HOST);
#elif defined(HAVE_GETHOSTNAME)
      if (gethostname(address + i, 256 - i - 1) < 0)
	 return address;
#elif defined(HAVE_SYSINFO)
      if (sysinfo(SI_HOSTNAME, address + i, 256 - i - 1) < 0)
	 return address;
#else
 #error Cannot determine hostname
#endif
      i = strlen(address);
      /* If we have the dot somewhere in the address, it probably
	 means we have the whole domain (hopefully!).  */
      if (strchr(address + pos, '.'))
	 return address;
#ifdef MY_DOMAIN
      if (*MY_DOMAIN != '.')  /* Check for leading dot. */
	 address[i] = '.';
      ++i;
      strcat(address, MY_DOMAIN);
#elif defined(HAVE_GETDOMAINNAME)
      address[i++] = '.';
      if (getdomainname(address + i, 256 - i - 1) < 0)
      {
	 address[pos] = '\0';
	 return address;
      }
#elif defined(HAVE_SYSINFO)
      address[i++] = '.';
      if (sysinfo(SI_SRPC_DOMAIN, address + i, 256 - i - 1) < 0)
      {
	 address[pos] = '\0';
	 return address;
      }
#else
 #error Cannot determine domainname
#endif
      /* Check for various invalid/null domains. */
      if (!*(address + i)
	  || !strcasecmp(address + i, "null")
	  || !strcasecmp(address + i, "(null)")
	  || !strcasecmp(address + i, "(none)"))
      {
	 address[pos] = '\0';
	 return address;
      }
   }
   return address;
}

/* Print error messages for host errors. */
char *
herrmsg(int error)
{
   char *msg;

   /* Can't use switch since some constants are equal. */
   if (error == HOST_NOT_FOUND || error == NO_RECOVERY
       || error == NO_DATA || error == NO_ADDRESS
       || error == TRY_AGAIN)
      msg = "Host not found";
   else
      msg = "Unknown error";
   return msg;
}

/* Clean the host list. */
void
clean_hosts(void)
{
   free_hlist(hlist);
   hlist = NULL;
}
