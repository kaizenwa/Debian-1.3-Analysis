#ifndef _WINDEF_H

#define MAKELONG(a, b)  ((((a) & 0xffff))|(((b) & 0xffff)<<16))
#define MAKELANGID(a,b) ((a) | ((b)<<10))

typedef DWORD   COLORREF;
DECLARE_HANDLE(HPALETTE);
DECLARE_HANDLE(HPEN);
DECLARE_HANDLE(HRGN);

#define MAKEWPARAM(l, h)      (WPARAM)MAKELONG(l, h)
#define MAKELPARAM(l, h)      (LPARAM)MAKELONG(l, h)
#define MAKELRESULT(l, h)     (LRESULT)MAKELONG(l, h)

#endif
