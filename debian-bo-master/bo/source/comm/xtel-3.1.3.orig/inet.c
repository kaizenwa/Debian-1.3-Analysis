/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1996  Lectra Systemes & Pierre Ficheux
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
/*
 *
 *	Routines standard de creation de sockets cote client.
 *
 *	Permet de creer des socket tcp sur un service donne).
 *
 *      D'apres Ronan KERYELL (Ecole Normale Superieure, Paris)
 *               keryell@ens.fr
 */
static char rcsid[] = "$Id: inet.c,v 1.3 1996/10/04 17:04:11 pierre Exp $";

#ifndef NO_NETWORK

#include <sys/types.h>
#define _TYPE
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>

#if defined(SVR4) || defined(hpux)
#define bcopy(src,dest,len) (memcpy(dest,src,len))
#endif
 
#ifndef u_short
#define u_short ushort
#endif

c_socket (host, port)
char *host;
int port;
{
  int sock;
  struct sockaddr_in addr;
  struct hostent *hp;

  /* Recupere l'adresse de l'hote cible */
  if (!(hp = gethostbyname (host))){
    perror("gethostbyname");
    return (-1);
  }
  /* Et on cree la socket */
  if ((sock = socket(AF_INET, SOCK_STREAM,0)) < 0){
    perror("socket");
    return (-1);
  }
  /* Preparons l'adresse pour le connect */
  /* Le type d'adresse */
  addr.sin_family = hp->h_addrtype;
  /* Le port (recupere dans le getservbyname) */
  addr.sin_port = port;
  /* L'adresse de l'hote cible */
  bcopy(hp->h_addr, (caddr_t)&addr.sin_addr, hp->h_length);

  /* Et zoup : on se connecte */
  if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    perror("connect");
    return (-1);
  }
  /* Ok ! : on a une socket en tcp de bon gout */
  return (sock);
}

/*
 *	Cree une socket vers un hote donne, sur un service donne,
 *
 *	Ex : sock = c_clientbyname("aneth", "courier")
 *
 */
c_clientbyname (host, name)
char *host, *name;
{
  struct servent *sv;  

  /* Description du service recherche (essentiellement, le numero de port) */
  if (!(sv = getservbyname (name, "tcp"))) {
    perror ("getservbyname");
    return (-1);
  }

  return (c_socket (host, sv->s_port));
}

/*
 *	Cree une socket vers un hote donne, sur un port donne,
 *
 *	Ex : sock = c_clientbyport ("aneth", 1313)
 *
 */
c_clientbyport (host, port)
char *host;
int port;
{
  struct servent *sv;  

  /* Description du service recherche (essentiellement, le numero de port) */
  if (!(sv = getservbyport (htons(port), "tcp"))) {
    perror ("getservbyport");
    return (-1);
  }

  return (c_socket (host, sv->s_port));
}

#endif /* NO_NETWORK */
