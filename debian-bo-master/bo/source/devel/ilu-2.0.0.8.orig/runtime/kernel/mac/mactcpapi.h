#include "macglob.h"
OSErr InitNetwork(void);
OSErr CreateStream(unsigned long *stream, unsigned long recvLen);
OSErr ReleaseStream(unsigned long stream);
OSErr OpenConnection(unsigned long stream, long remoteHost,
 short remotePort, Byte timeout); 
OSErr WaitForConnection(unsigned long stream, Byte timeout, short localPort,
	long *remoteHost, short *remotePort);
OSErr AsyncWaitForConnection(unsigned long stream, Byte timeout, short localPort,
	long remoteHost, short remotePort, TCPiopb **returnBlock);
OSErr AsyncGetConnectionData(TCPiopb *returnBlock, long *remoteHost, 
	short *remotePort);
OSErr CloseConnection(unsigned long stream);	
OSErr AbortConnection(unsigned long stream);
OSErr SendData(unsigned long stream, Ptr data, unsigned short length);
OSErr SendMultiData(unsigned long stream, CStr255 data[], short numData);
OSErr RecvData(unsigned long stream, Ptr data, unsigned short *length,
	Boolean retry);	
OSErr GetConnectionState(unsigned long stream, byte *state);
OSErr IPNameToAddr(char *name, unsigned long *addr);
OSErr IPAddrToName(unsigned long addr, char *name);
OSErr GetMyIPAddr(unsigned long *addr);
OSErr GetMyIPAddrStr(char *addrStr);
OSErr GetMyIPName(char *name);
