//
// Copyright(c) 1996 Amol Deshpande
// amold@microsoft.com
// Redistribution in source or binary form is permitted as long as you 
// retain this notice in the distribution
// 
// See LICENSE.TXT for complete redistribution/usage conditions
//
// There is no warranty, implied or otherwise, with this software. Use at your
// own risk.
//
// The original ircii code is under the copyright indicated in the source.
// 
// And finally,
// Microsoft Corporation has nothing to do with this code. 
//
// access.c
// replacement for msvc's access() and winsock error message routine
// 
//
//
#define WIN32_LEAN_AND_MEAN
#include <winsock.h>
#include <errno.h>

#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0

int nt_access(char *filename, int mode) {
	
	DWORD attribs, bintype;

	if (!filename) {
		errno = ENOENT;
		return -1;
	}
	attribs = GetFileAttributes(filename);

	if (attribs == (DWORD) -1) {
		errno = EACCES;
		return -1;
	}
	if ( (mode & W_OK) &&  (attribs & FILE_ATTRIBUTE_READONLY) ) {
		errno = EACCES;
		return -1;
	}
	if (mode & X_OK) {
		if ((!(attribs & FILE_ATTRIBUTE_DIRECTORY)) && 
				!GetBinaryType(filename,&bintype) ) {
			errno = EACCES;
			return -1;
		}
	}
	return 0;
}
int getuid(void){
	return 0;
}

int getppid(void){
	return 0;
}
char * mystrerror(int errornum) {
	
	if (errornum < WSABASEERR)
		return strerror(errornum);


	switch (errornum) {
		case WSAEWOULDBLOCK:
			return "Operation would  block";
			break;
		case WSAEINPROGRESS:
			return "Blocking operation in progress";
			break;
		case WSAEALREADY:
			return "Whatever the hell EALREADY means";
			break;
		case WSAENOTSOCK:
			return "Socket operation on non-socket";
			break;
		case WSAEADDRINUSE:
			return "Address is in use";
			break;
		case WSAENETDOWN:
			return "The network subsystem has failed";
			break;
		case WSAENETUNREACH:
			return "Destination net unreachable";
			break;
		case WSAECONNABORTED:
			return "Connection aborted";
			break;
		case WSAECONNRESET:
			return "Connection reset by peer";
			break;
		case WSAENOBUFS:
			return "No more buffers";
			break;
		case WSAEISCONN:
			return "Socket is already connected";
			break;
		case WSAENOTCONN:
			return "Socket is not connected";
			break;
		case WSAESHUTDOWN:
			return "Cannot send after shutdown";
			break;
		case WSAETIMEDOUT:
			return "Connection timed out";
			break;
		case WSAECONNREFUSED:
			return "Connection refused";
			break;
		case WSAEHOSTDOWN:
			return "Remote host is down";
			break;
		case WSAEHOSTUNREACH:
			return "Host unreachable";
			break;
		case WSAHOST_NOT_FOUND:
			return "Host not found";
			break;
		case WSATRY_AGAIN:
			return "Host not found,try again";
			break;
		case WSANO_DATA:
			return "No data record of requested type(host not found?)";
			break;

		default:
			return "Some bloody winsock error";
			break;
	}
	
}
