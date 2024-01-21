/*
 *   dhttpd/1.02 - Personal web page server version 1.02
 *   Copyright (C) 1997  David A. Bartold
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

class Socket
{
	int sock;

protected:
	FILE *io;

public:
	int geterror() { return !io; }
	virtual void handle();
	FILE *getio() { return io; }

	Socket( int s );
	~Socket();
};

class ListenSocket
{
	struct sockaddr cSid;
	int sock;

public:
	int geterror() { return sock==-1; }
	int newsock();
	int getfd() { return sock; }

	ListenSocket( int port );
	~ListenSocket();
};
