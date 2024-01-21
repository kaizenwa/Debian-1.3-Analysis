#ifndef _COMMDLG_H
#define _COMMDLG_H

#ifdef UNICODE
#define PRINTDLG 	PRINTDLGW
#define CHOOSECOLOR 	CHOOSECOLORW
#define CHOOSEFONT 	CHOOSEFONTW
#define OPENFILENAME  	OPENFILENAMEW
#else
#define PRINTDLG	PRINTDLGA
#define CHOOSECOLOR 	CHOOSECOLORA
#define CHOOSEFONT 	CHOOSEFONTA
#define OPENFILENAME  	OPENFILENAMEA
#define ChooseFont ChooseFontA
#define GetOpenFileName GetOpenFileNameA
#define GetSaveFileName GetSaveFileNameA
#define PrintDlg PrintDlgA
#endif


/**********************************************************************/
/* PRINT DIALOG */

#define PD_ALLPAGES                  0x00000000
#define PD_COLLATE                   0x00000010
#define PD_DISABLEPRINTTOFILE        0x00080000
#define PD_NOPAGENUMS                0x00000008
#define PD_NOPAGENUMS                0x00000008
#define PD_NOSELECTION               0x00000004
#define PD_NOSELECTION               0x00000004
#define PD_PRINTSETUP                0x00000040
#define PD_PRINTTOFILE               0x00000020
#define PD_RETURNDC                  0x00000100
#define PD_RETURNDEFAULT             0x00000400
#define PD_SHOWHELP                  0x00000800
#define PD_HIDEPRINTTOFILE           0x00100000

typedef struct
{
  DWORD 	lStructSize;
  HWND 		hwndOwner;
  HGLOBAL 	hDevMode;
  HGLOBAL 	hDevNames;
  HDC 		hDC;
  DWORD 	Flags;
  WORD 		nFromPage;
  WORD		nToPage;
  WORD 		nMinPage;
  WORD 		nMaxPage;
  WORD 		nCopies;
  HINSTANCE 	hInstance;
  LPARAM 	lCustData;
  void 		(*lpfnPrintHook) ();
  void		(*lpfnSetupHook) ();
  const char 	*lpPrintTemplateName;
  const char 	*lpSetupTemplateName;
  HGLOBAL 	hPrintTamplte;
  HGLOBAL 	hSetupTemplate;
}
PRINTDLGA;

typedef struct
  {
    DWORD 	lStructSize;
    HWND 	hwndOwner;
    HGLOBAL	hDevMode;
    HGLOBAL 	hDevNames;
    HDC 	hDC;
    DWORD	Flags;
    WORD 	nFromPage;
    WORD 	nToPage;
    WORD 	nMinPage;
    WORD 	nMaxPage;
    WORD 	nCopies;
    HINSTANCE 	hInstance;
    LPARAM	lCustData;
    void 	(*lpfnPrintHook) ();
    void 	(*lpfnSetupHook) ();
    const wchar_t *lpPrintTemplateName;
    const wchar_t *lpSetupTemplateName;
    HGLOBAL 	hPrintTemplate;
    HGLOBAL 	hSetupTemplate;
  }
PRINTDLGW;

typedef struct
  {
    WORD wDriverOffset;
    WORD wDeviceOffset;
    WORD wOutputOffset;
    WORD wDefault;
  }
DEVNAMES;

typedef PRINTDLG *LPPRINTDLG;
typedef DEVNAMES *LPDEVNAMES;

/**********************************************************************/
/* CHOOSE COLOR DIALOG BOX */

#define CC_RGBINIT               0x00000001
#define CC_PREVENTFULLOPEN       0x00000004
typedef struct
  {
    DWORD lStructSize;
    HWND hwndOwner;
    HWND hInstance;
    COLORREF rgbResult;
    COLORREF *lpCustColors;
    DWORD Flags;
    LPARAM lCustData;
    void (*lpfnHook) ();
    const char *lpTemplateName;
  }
CHOOSECOLORA;

BOOL WINAPI ChooseColorA (CHOOSECOLORA *);

/**********************************************************************/
/* CHOOSEFONT */
#define CF_ANSIONLY                0x00000400
#define CF_EFFECTS                 0x00000100
#define CF_INITTOLOGFONTSTRUCT     0x00000040
#define CF_LIMITSIZE               0x00002000
#define CF_NOSIMULATIONS           0x00001000
#define CF_PRINTERFONTS            0x00000002
#define CF_SCREENFONTS             0x00000001
#define CF_SHOWHELP                0x00000004
#define CF_TTONLY                  0x00040000


typedef struct
  {
    DWORD lStructSize;
    HWND hwndOwner;
    HDC hDC;
    LOGFONTA *lpLogFont;
    INT iPointSize;
    DWORD Flags;
    COLORREF rgbColors;
    LPARAM lCustData;
    void (*lpfnHook) ();
    char *lpTemplateName;
    HINSTANCE hInstance;
    char *lpszStyle;
    WORD nFontType;
    WORD pad;
    INT nSizeMin;
    INT nSizeMax;
  }
CHOOSEFONTA;

#define SCREEN_FONTTYPE       0x2000

/**********************************************************************/
/* OPENFILE */

#define OFN_HIDEREADONLY             0x00000004
#define OFN_OVERWRITEPROMPT          0x00000002
#define OFN_PATHMUSTEXIST            0x00000800
#define OFN_FILEMUSTEXIST            0x00001000
/*typedef UINT (WINAPI *LPOFNHOOKPROC) (HWND, UINT, WPARAM, LPARAM); */

typedef struct
  {
    DWORD 	lStructSize;
    HWND 	hwndOwner;
    HINSTANCE 	hInstance;
    const char 	*lpstrFilter;
    char 	*lpstrCustomFilter;
    DWORD 	nMaxCustFilter;
    DWORD 	nFilterIndex;
    char 	*lpstrFile;
    DWORD 	nMaxFile;
    char 	*lpstrFileTitle;
    DWORD 	nMaxFileTitle;
    const char *lpstrInitialDir;
    const char *lpstrTitle;
    DWORD 	Flags;
    WORD 	nFileOffset;
    WORD	nFileExtension;
    const char *lpstrDefExt;
    LPARAM 	lCustData;
    void (*lpfnHook) ();
    const char *lpTemplateName;
  }
OPENFILENAMEA;

BOOL WINAPI ChooseFontA(CHOOSEFONTA *);
BOOL WINAPI PrintDlgA(PRINTDLGA *);
BOOL WINAPI GetOpenFileNameA(OPENFILENAMEA *);
BOOL WINAPI GetSaveFileNameA(OPENFILENAMEA *);


#endif
