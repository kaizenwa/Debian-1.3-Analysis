#define I_STRING
#define I_MEMORY
#define I_INET
#define I_CTYPE
#include "includes.h"

/* This opens the hosts.term file. */

static FILE *open_termhosts(void) {
  char *ptr = NULL;
  char *path;

  do {
    if ((path = get_term_path(&ptr))) {
      strcat(path,"/hosts.term");
      strcat(path,term_server);
      if (access(path, R_OK) >= 0) break;
    }else break;
  } while (ptr);
  if (! ptr) do {
    if ((path = get_term_path(&ptr))) {
      strcat(path,"/hosts.term");
      if (access(path, R_OK) >= 0) break;
    }else break;
  } while (! ptr);
  return fopen(path,"r");
}

/* This reads in the next term hostent entry. */

static struct hostent *gettermhostent(int stayopen) {
	static char hostbuf[BUFSIZ+1];
	static struct hostent host;
        static FILE *hostf = NULL;
        static char *hosts_addrs[2] = {NULL, NULL};
        static char **hosts_aliases = NULL;
        static int laddr, hosts_aliases_size;
        int count;
        char *p, *cp;

	if (hostf) {
		if (! stayopen) fclose(hostf);
	}else hostf = open_termhosts();
        while (hostf) {
	        if ((p = fgets(hostbuf, BUFSIZ, hostf)) == NULL) break;

        	for (;*p && ! isdigit(*p) && *p != '#' ; p++);
        	if (! isdigit(*p)) continue;
                for (cp=p ;*cp && (isdigit(*cp) || *cp == '.'); cp++);
                if (!isspace(*cp)) continue;
                *cp = '\0';

			/* We now know the IP address. */
                host.h_addr_list = hosts_addrs;
		laddr = inet_addr(p);
                hosts_addrs[0] = (char *)&laddr;
		host.h_length = sizeof(laddr);

		for (p=cp+1;*p && isspace(*p);p++);
                if (! *p || *p == '#') continue;

			/* First we count the aliases to ensure we have */
			/* space for the array. */
		for(count = 0, cp = p; *cp && *cp != '#';count++) {
			for (;*cp && ! isspace(*cp);cp++);
			if (! *cp || *cp == '#') continue;
			for(;*cp && isspace(*cp);cp++);
		}

			/* If needed, allocate more memory space. */
                if (! hosts_aliases) 
			hosts_aliases = (char **)malloc(
                        	(count + 1) * sizeof(char *));
		else if (hosts_aliases_size <= count) 
			hosts_aliases = (char **)realloc(hosts_aliases,
				(count + 1) * sizeof(char *));
		memset(hosts_aliases,0,sizeof(char *) * (count + 1));
		host.h_aliases = hosts_aliases;

			/* Finally we fill the respective aliases */

		for (count = 0, cp = p; *cp && *cp != '#';count++) {
			hosts_aliases[count] = cp;
			for(;*cp && ! isspace(*cp); cp++);
			if (*cp == '#') *cp = 0;		
			if (! *cp) continue;
			*cp = 0;
			for (cp++;*cp && isspace(*cp);cp++);
		}
                host.h_name = host.h_aliases[0];
		host.h_addrtype = AF_INET;
		return &host;
	}
	if (hostf) {
		fclose(hostf);
		hostf = NULL;
	}
	return NULL;
}

/* This routine finds if there is a hosts.term entry for */
/* the specified address */

static struct hostent *gettermhostbyaddr(char *addr, int length, int type) {
  int j;
  struct hostent *hp;

  if (type != AF_INET) return NULL;
  while ( (hp=gettermhostent(1)) ) {
    if (hp->h_addr_list && hp->h_length == length) {
      for(j = 0; hp->h_addr_list[j]; j++) 
        if (! memcmp(addr,hp->h_addr_list[j],hp->h_length)) break;
      if (hp->h_addr_list[j]) break;
    } 
  }
  gettermhostent(0);
  return hp;
}

/* This routine finds if there is a hosts.term entry for */
/* the specified hostname */
		
static struct hostent *gettermhostbyname(char *name) {
  int j;
  struct hostent *hp;

  if (isdigit(*name)) {
	unsigned long l;
	l = inet_addr(name);
	return gettermhostbyaddr((char *)&l, sizeof(unsigned long), AF_INET);
  }
  while ( (hp=gettermhostent(1)) ) {
    if (hp->h_name && hp->h_aliases) {
      for(j = 0; hp->h_aliases[j]; j++) 
        if (! strcmp(name,hp->h_aliases[j])) break;
      if (hp->h_aliases[j]) break;
    } 
  }
  gettermhostent(0);
  return hp;
}
		
/* This intended to speedup hostname lookups by keeping a list of
 * already known hostnames.  The translations is:
 *   gethostbyaddr(addr,len,type) --> host_lookup(addr,len,type,0,NULL)
 *   gethostbyname(name)          --> host_lookup(name,0,AF_INET,0,NULL)
 *
 *    how -->  0 is for local network lookups, 1 is for special lookups.
 *    add_hp --> Only makes sense with "how=~1".  This specifies that if
 *               the host isn't already listed, add this host lookup
 *		 to the table of hostnames.
 *
 * The main advantage of this routine is the lookup table avoids saves time
 * by avoiding networking calls to get addresses we already know.  The main
 * disadvantage is that if there is a problem with the named server, the
 * client program won't check again later to see if it has been resolved.
 */

struct hostent *host_lookup(char *addr,int length, int type, int how,
    struct hostent *add_hp) {
  int i, j, k;
  unsigned long laddr;
  static int num_hosts = 0;
  struct hostent *hp = NULL;
  typedef struct {
	struct hostent hs;
	unsigned long ip_addr;
        int active, how, h_errno;
    } Hosts_type;
  static Hosts_type *Hosts = NULL;
  extern int h_errno;

  if (! addr) return add_hp;
  if (type != AF_INET) {
    if (! how) {
      if (! length) 
        return hp = x__gethostbyname(addr);
      else
        return hp = x__gethostbyaddr(addr, length, type);
    }else {
      if (! length) 
        return hp = gettermhostbyname(addr);
      else
        return hp = gettermhostbyaddr(addr, length, type);
    }
  }

  if (! length && isdigit(*addr)) {
    laddr = inet_addr(addr);
    addr = (char *)&laddr;
    length = sizeof(laddr);
  }

  if (! length) {
    for(i=0;i < num_hosts; ++i) 
      if (Hosts[i].hs.h_name && how == Hosts[i].how) {
        if (! strcmp(addr,Hosts[i].hs.h_name)) break;
        if (! Hosts[i].hs.h_aliases) continue;
        for(j = 0; Hosts[i].hs.h_aliases[j]; j++) 
          if (! strcmp(addr,Hosts[i].hs.h_aliases[j]) ) break;
        if (Hosts[i].hs.h_aliases[j]) break;
      } 
  }else  {
    for(i=0;i < num_hosts; ++i) 
      if (Hosts[i].ip_addr != ~0 && how == Hosts[i].how) {
        if (! memcmp(addr,&Hosts[i].ip_addr,length)) break;
	if (! Hosts[i].hs.h_addr_list) continue;
	for(j = 0; Hosts[i].hs.h_addr_list[j]; j++) 
          if (! memcmp(addr,Hosts[i].hs.h_addr_list[j],length)) break;
        if (Hosts[i].hs.h_addr_list[j]) break;
      }
  }

  if (i < num_hosts) {
    if (Hosts[i].active) return &Hosts[i].hs;
    h_errno = Hosts[i].h_errno;
    return NULL;
  }


  if (add_hp) {
    if (! (~(unsigned long)add_hp)) 
      hp = NULL;
    else
      hp = add_hp;
  }else if (! how) {
    if (! length) 
      hp = x__gethostbyname(addr);
    else
      hp = x__gethostbyaddr(addr, length, AF_INET);
  }else {
    if (! length) 
      hp = gettermhostbyname(addr);
    else
      hp = gettermhostbyaddr(addr, length, type);
    if (! hp) return NULL;
  }


  if (!Hosts) 
    Hosts = (Hosts_type *) malloc(sizeof(Hosts_type));
  else 
    Hosts = (Hosts_type *) realloc((char *) Hosts, (num_hosts + 1) *
                                   sizeof(Hosts_type)); 
  memset(&Hosts[(i = num_hosts++)],0,sizeof(Hosts_type));
  Hosts[i].hs.h_addrtype = AF_INET;
  Hosts[i].hs.h_name = NULL;
  Hosts[i].how = how;
  Hosts[i].ip_addr = ~0;
  Hosts[i].hs.h_aliases = NULL;


  Hosts[i].active = 0;
  if (! hp) {
    char *s = NULL;
    Hosts[i].h_errno = h_errno;
    Hosts[i].hs.h_length = sizeof(unsigned long);
    if (! length) {
      if ((s = (char *)malloc(strlen(addr)+2))) {
        strcpy(s,addr);
        s[strlen(s)+1] = 0;
        Hosts[i].hs.h_name = s;
      }
    }else {
      memcpy(&Hosts[i].ip_addr,addr,length);
    }
    return NULL;
  }
  Hosts[i].h_errno = 0;
  Hosts[i].hs.h_length = hp->h_length;
  if (hp->h_addrtype != AF_INET) return hp;

	/* Copy the list of aliases */


  for(k=j=0; hp->h_aliases[j]; ++j)
    k += 1 + strlen(hp->h_aliases[j]);

  if ((Hosts[i].hs.h_aliases = (char **)malloc((j+1) * sizeof(char *)))) {
    char *s = NULL;
    if ((s = (char *)malloc(k))) {
      for(j=0; hp->h_aliases[j]; ++j) {
        strcpy(s,hp->h_aliases[j]);
        Hosts[i].hs.h_aliases[j] = s;
        s += strlen(s) + 1;
      }
      Hosts[i].hs.h_aliases[j] = NULL;
    }else {
      Hosts[i].hs.h_aliases[0] = NULL;
    }
  }
	/* Copy the list of addresses */

  for(k=j=0; hp->h_addr_list[j]; ++j)
    k += hp->h_length;

  if ((Hosts[i].hs.h_addr_list = (char **)malloc((j+1) * sizeof(char *)))) {
    char *s = NULL;
    if ((s = (char *)malloc(k))) {
      for(j=0; hp->h_addr_list[j]; ++j) {
        unsigned long l;
        memcpy(s,hp->h_addr_list[j],hp->h_length);
        Hosts[i].hs.h_addr_list[j] = s;
	memcpy(&l,s,hp->h_length);
        s += hp->h_length;
      }
      Hosts[i].hs.h_addr_list[j] = NULL;
    }else {
      Hosts[i].hs.h_addr_list[0] = NULL;
    }
  }

	/* Make sure the input is part of the stored entry */

  if (! length) {
    memcpy(&Hosts[i].ip_addr,Hosts[i].hs.h_addr,hp->h_length);
    Hosts[i].hs.h_name = (char *) malloc(strlen(addr)+1);
    strcpy((char *)Hosts[i].hs.h_name, addr);
  }else {
    memcpy(&Hosts[i].ip_addr,addr,sizeof(Hosts[i].ip_addr)); 
    Hosts[i].hs.h_name = (char *) malloc(strlen(hp->h_name)+1);
    strcpy((char *)Hosts[i].hs.h_name, hp->h_name);
  }
  Hosts[i].active = 1;
  return hp;
}




