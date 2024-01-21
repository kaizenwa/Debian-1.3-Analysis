/*
EXPORTS

DONE	_GetFileVersionInfoA@16
DONE	_GetFileVersionInfoSizeA@8
	_GetFileVersionInfoSizeW@8
	_GetFileVersionInfoW@16
	_VerFindFileA@32
	_VerFindFileW@32
	_VerInstallFileA@32
	_VerInstallFileW@32
DONE	_VerQueryValueA@16
	_VerQueryValueIndexA@24
	_VerQueryValueIndexW@24
	_VerQueryValueW@16
*/


#ifdef UNICODE
#define GetFileVersionInfoSize GetFileVersionInfoSizeW
#else
#define GetFileVersionInfoSize GetFileVersionInfoSizeA
#define GetFileVersionInfo GetFileVersionInfoA
#define VerQueryValue VerQueryValueA
#endif

DWORD WINAPI GetFileVersionInfoSizeA(char *, DWORD*);
BOOL WINAPI  GetFileVersionInfoA(char *, DWORD, DWORD, void *);
BOOL WINAPI  VerQueryValueA(const void *p, char *q, void **buf, int *len);

