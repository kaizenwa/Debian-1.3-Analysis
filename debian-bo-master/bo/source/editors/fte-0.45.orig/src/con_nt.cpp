/*	  con_nt.cpp
 *
 *	  Copyright (c) 1994-1996, Marko Macek
 *
 *	  You may distribute under the terms of either the GNU General Public
 *	  License or the Artistic License, as specified in the README file.
 *
 */

/* this was hacked once from os/2 version */
/* it used to work, but is no longer up to date */
/* some functionality is now shared with g_text.cpp and g_draw.cpp */
/* new os/2 code needs to be merged with this */
/* the console apis on win'95 seem to be really screwed up */

/*
 *	10/13/96 Jal:
 *		Rebuilt for Windows NT, generic; no port/Watcom code should
 *		be needed to compile (jal).
 *		Removed most mouse handling code (unnecessary), added pipe handler by
 *		porting the OS/2 version to NT.
 *		Solved some bugs with regard to the TCell problem.
 *
 *  10/28/96 Jal:
 *      Started to replace threaded pipe code with nonthreaded code, using
 *      overlapped I/O.
 *
 */
#define WIN32_LEAN_AND_MEAN 1
#include "windows.h"
#include <process.h>

#include <stdio.h>
#include "sysdep.h"
#include "console.h"
#include "gui.h"


static Initialized = 0;
static MousePresent = 0;
static int CursorVisible = 1; /* 1 means visible */
static int MouseVisible = 0; /* 0 means hidden */
static TEvent MouseEv = { evNone };
static TEvent EventBuf = { evNone };
static TEventMask EventMask;

static HANDLE hOldConOut;
static HANDLE hOldConIn;
static HANDLE hConsole;         // out output buffer
static DWORD oldConsoleMode;

static int LastMouseX = 0;
static int LastMouseY = 0;

#if 0
void dbg(const char* s, ...)
{
}
#else

void dbg(const char* s, ...) /*FOLD00*/
{
    char	buf[256];
    va_list args;
    
    va_start(args, s);
    vsprintf(buf, s, args);
    va_end(args);
    OutputDebugString(buf);
}
#endif


static void DrawCursor(int Show) { /*FOLD00*/
    CONSOLE_CURSOR_INFO cci;
    
    GetConsoleCursorInfo(hConsole, &cci);
    cci.bVisible = Show ? TRUE : FALSE;
    SetConsoleCursorInfo(hConsole, &cci);
}

#if 0
static struct {
    USHORT CharScan;
    TKeyCode KeyCode;
} TransCharScan[] = {
{ 0x0100, kbEsc },					   { 0x011B, kbEsc },
{ 0x1C0D, kbEnter },				   { 0x1C0A, kbEnter },
{ 0x1C00, kbEnter },				   { 0xE00D, kbEnter | kfGray },
{ 0xA600, kbEnter | kfGray },		   { 0xE00A, kbEnter | kfGray },
{ 0x0E08, kbBackSp },				   { 0x0E7F, kbBackSp },
{ 0x0E00, kbBackSp },				   { 0x0F09, kbTab },
{ 0x9400, kbTab },					   { 0xA500, kbTab },
{ 0x0F00, kbTab },					   { 0x4E00, '+' | kfGray },
{ 0x9000, '+' | kfGray },              { 0x4E2B, '+' | kfGray },
{ 0x4A00, '-' | kfGray },              { 0x8E00, '-' | kfGray },
{ 0x4A2D, '-' | kfGray },              { 0x3700, '*' | kfGray },
{ 0x9600, '*' | kfGray },              { 0x372A, '*' | kfGray },
{ 0xE02F, '/' | kfGray },              { 0xA400, '/' | kfGray },
{ 0x9500, '/' | kfGray },              { 0x0300, 0 }
};

static struct {
    int ScanCode;
    TKeyCode KeyCode;
} TransScan[] = {
{ 0x78, '1' }, { 0x79, '2' }, { 0x7A, '3' }, { 0x7B, '4' }, { 0x7C, '5' },
{ 0x7D, '6' }, { 0x7E, '7' }, { 0x7F, '8' }, { 0x80, '9' }, { 0x81, '0' },

{ 0x10, 'Q' }, { 0x11, 'W' }, { 0x12, 'E' }, { 0x13, 'R' }, { 0x14, 'T' },
{ 0x15, 'Y' }, { 0x16, 'U' }, { 0x17, 'I' }, { 0x18, 'O' }, { 0x19, 'P' },

{ 0x1E, 'A' }, { 0x1F, 'S' }, { 0x20, 'D' }, { 0x21, 'F' }, { 0x22, 'G' },
{ 0x23, 'H' }, { 0x24, 'J' }, { 0x25, 'K' }, { 0x26, 'L' },

{ 0x2C, 'Z' }, { 0x2D, 'X' }, { 0x2E, 'C' }, { 0x2F, 'V' }, { 0x30, 'B' },
{ 0x31, 'N' }, { 0x32, 'M' },

{ 0x29, '`' }, { 0x82, '-' }, { 0x83, '=' }, { 0x2B, '\\' }, { 0x1A, '[' },
{ 0x1B, ']' }, { 0x27, ';' }, { 0x28, '\'' }, { 0x33, ',' }, { 0x34, '.' },
{ 0x35, '/' }, { 0x37, '*' }, { 0x4E, '+' }, { 0x4A, '-' },

{ 0x3B, kbF1	},	{ 0x3C, kbF2	},	{ 0x3D, kbF3	},
{ 0x3E, kbF4	},	{ 0x3F, kbF5	},	{ 0x40, kbF6	},
{ 0x41, kbF7	},	{ 0x42, kbF8	},	{ 0x43, kbF9	},
{ 0x44, kbF10	},	{ 0x85, kbF11	},	{ 0x86, kbF12	},

{ 0x54, kbF1	},	{ 0x55, kbF2	},	{ 0x56, kbF3	},
{ 0x57, kbF4	},	{ 0x58, kbF5	},	{ 0x59, kbF6	},
{ 0x5A, kbF7	},	{ 0x5B, kbF8	},	{ 0x5C, kbF9	},
{ 0x5D, kbF10	},	{ 0x87, kbF11	},	{ 0x88, kbF12	},

{ 0x5E, kbF1	},	{ 0x5F, kbF2	},	{ 0x60, kbF3	},
{ 0x61, kbF4	},	{ 0x62, kbF5	},	{ 0x63, kbF6	},
{ 0x64, kbF7	},	{ 0x65, kbF8	},	{ 0x66, kbF9	},
{ 0x67, kbF10	},	{ 0x89, kbF11	},	{ 0x8A, kbF12	},

{ 0x68, kbF1	},	{ 0x69, kbF2	},	{ 0x6A, kbF3	},
{ 0x6B, kbF4	},	{ 0x6C, kbF5	},	{ 0x6D, kbF6	},
{ 0x6E, kbF7	},	{ 0x6F, kbF8	},	{ 0x70, kbF9	},
{ 0x71, kbF10	},	{ 0x8B, kbF11	},	{ 0x8C, kbF12	},

{ 0x47, kbHome	},	{ 0x48, kbUp	},	{ 0x49, kbPgUp	},
{ 0x4B, kbLeft	},	{ 0x4C, kbCenter},	{ 0x4D, kbRight },
{ 0x4F, kbEnd	},	{ 0x50, kbDown	},	{ 0x51, kbPgDn	},
{ 0x52, kbIns	},	{ 0x53, kbDel	},

{ 0x77, kbHome	},	{ 0x8D, kbUp	},	{ 0x84, kbPgUp	},
{ 0x73, kbLeft	},						{ 0x74, kbRight },
{ 0x75, kbEnd	},	{ 0x91, kbDown	},	{ 0x76, kbPgDn	},
{ 0x92, kbIns	},	{ 0x93, kbDel	},

{ 0x97, kbHome	| kfGray },  { 0x98, kbUp	 | kfGray },  { 0x99, kbPgUp  | kfGray },
{ 0x9B, kbLeft	| kfGray }, 							  { 0x9D, kbRight | kfGray },
{ 0x9F, kbEnd	| kfGray },  { 0xA0, kbDown  | kfGray },  { 0xA1, kbPgDn  | kfGray },
{ 0xA2, kbIns	| kfGray },  { 0xA3, kbDel	 | kfGray }
};
#endif


struct {
    SHORT VirtCode;
    unsigned long KeyCode;
} VirtTab[]  =
{
    { 112, kbF1 },
    { 113, kbF2 },
    { 114, kbF3 },
    { 115, kbF4 },
    { 116, kbF5 },
    { 117, kbF6 },
    { 118, kbF7 },
    { 119, kbF8 },
    { 120, kbF9 },
    { 121, kbF10 },
    { 122, kbF11 },
    { 123, kbF12 },
    
    { 35, kbEnd },
    { 36, kbHome },
    { 33, kbPgUp },
    { 34, kbPgDn },
    { 38, kbUp },
    { 37, kbLeft },
    { 39, kbRight },
    { 40, kbDown },
    { 45, kbIns },
    { 46, kbDel },
    
    { 27, kbEsc },
    { 13, kbEnter },
    { 8, kbBackSp },
    { 32, kbSpace },
    { 9, kbTab },
    
    { 0, 0 }
};

void ReadConsoleEvent(TEvent *E) /*FOLD00*/
{
    INPUT_RECORD inp;
    DWORD nread;
    TKeyCode Ch = 0;
    TKeyCode flg = 0;
    ULONG flags;
    int I;

    ReadConsoleInput(hOldConIn, &inp, 1, &nread);
    if (nread == 1)
    {
        switch (inp.EventType)
        {
        case WINDOW_BUFFER_SIZE_EVENT:
            //** Resized the window. Make FTE use the new size..
            frames->Resize(inp.Event.WindowBufferSizeEvent.dwSize.X, inp.Event.WindowBufferSizeEvent.dwSize.Y);
            frames->Repaint();
            1;
            return;
            
        case KEY_EVENT:
            
            flags = inp.Event.KeyEvent.dwControlKeyState;
            
            if (flags & (RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED)) flg |= kfAlt;
            if (flags & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED)) flg |= kfCtrl;
            if (flags & SHIFT_PRESSED) flg |= kfShift;
            if (flags & ENHANCED_KEY) flg |= kfGray;
            
            //** Skip shift, control and alt key stuff..
            switch(inp.Event.KeyEvent.wVirtualKeyCode) {	
                case VK_SHIFT: case VK_CONTROL: case VK_MENU: case VK_PAUSE:
                case VK_CAPITAL: case VK_LWIN:	case VK_RWIN: case VK_APPS:
                    return;
            }
            
            //			  printf("Keycode= %u\n", inp.Event.KeyEvent.wVirtualKeyCode);
            //			  Sleep(3000);
            
            Ch = 0;
            for (I = 0; I < sizeof(VirtTab)/sizeof(VirtTab[0]); I++)
                if (VirtTab[I].VirtCode == inp.Event.KeyEvent.wVirtualKeyCode)
                {
                    Ch = VirtTab[I].KeyCode;
                    break;
                }
            if (Ch == 0)
            {
                if ((Ch = (TKeyCode) inp.Event.KeyEvent.uChar.AsciiChar) != 0)
                {
                    if (flg & kfAlt) Ch = toupper(Ch);
                }
            }
            
            if (Ch == 0)
            {
                /*	printf("Key event down=%d, rep=%d, virt=%d, scan=%d, ascii=%d(%c), control=%X\n",
                 inp.Event.KeyEvent.bKeyDown,
                 inp.Event.KeyEvent.wRepeatCount,
                 inp.Event.KeyEvent.wVirtualKeyCode,
                 inp.Event.KeyEvent.wVirtualScanCode,
                 inp.Event.KeyEvent.uChar.AsciiChar,
                 inp.Event.KeyEvent.uChar.AsciiChar,
                 inp.Event.KeyEvent.dwControlKeyState);*/
            }
            
            if (inp.Event.KeyEvent.bKeyDown)
            {
                E->What = evKeyDown;
                E->Key.Code = Ch | flg;
            }
            else
            {
                E->What = evKeyUp;
                E->Key.Code = Ch | flg;
            }
            break;
            
            
        case MOUSE_EVENT:
            /*		printf("Mouse event x=%d, y=%d, but=%X, key=%X, flags=%d\n",
             inp.Event.MouseEvent.dwMousePosition.X,
             inp.Event.MouseEvent.dwMousePosition.Y,
             inp.Event.MouseEvent.dwButtonState,
             inp.Event.MouseEvent.dwControlKeyState,
             inp.Event.MouseEvent.dwEventFlags);
             */
            LastMouseX = E->Mouse.X = inp.Event.MouseEvent.dwMousePosition.X;
            LastMouseY = E->Mouse.Y = inp.Event.MouseEvent.dwMousePosition.Y;
            E->Mouse.Buttons = inp.Event.MouseEvent.dwButtonState;
            E->Mouse.Count = 1;
            if (inp.Event.MouseEvent.dwEventFlags & DOUBLE_CLICK)
                E->Mouse.Count = 2;
            if (inp.Event.MouseEvent.dwEventFlags == MOUSE_MOVED)
            {
                E->What = evMouseMove;
            }
            else
            {
                static WORD mb = 0;
                
                if (inp.Event.MouseEvent.dwButtonState & ~mb)
                {
                    E->What = evMouseDown;
                    E->Mouse.Buttons = inp.Event.MouseEvent.dwButtonState & ~mb;
                    //puts("Down");
                }
                else
                {
                    E->What = evMouseUp;
                    E->Mouse.Buttons = mb & ~inp.Event.MouseEvent.dwButtonState;
                    //puts("Up");
                }
                mb = inp.Event.MouseEvent.dwButtonState;
            }
            break;
        }
    }
}


int ConInit(int /*XSize*/, int /*YSize*/) /*FOLD00*/
{
    if (Initialized) return 0;
    
    EventBuf.What = evNone;
    MousePresent	= 0; //MOUSInit();

    hOldConIn = GetStdHandle(STD_INPUT_HANDLE);
    hOldConOut = GetStdHandle(STD_OUTPUT_HANDLE);
    hConsole = CreateConsoleScreenBuffer(GENERIC_READ | GENERIC_WRITE,
                                         0, NULL, 
                                         CONSOLE_TEXTMODE_BUFFER, NULL);
    SetConsoleMode(hConsole, ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT);
    
    ConContinue();
    Initialized = 1;
    return 0;
}

int ConDone(void) { /*FOLD00*/
    ConSuspend();
    CloseHandle(hConsole);
    return 0;
}

int ConSuspend(void) { /*FOLD00*/
    SetConsoleActiveScreenBuffer(hOldConOut);
    SetConsoleMode(hOldConIn, oldConsoleMode);
    return 0;
}

int ConContinue(void) { /*FOLD00*/
    SetConsoleActiveScreenBuffer(hConsole);
    GetConsoleMode(hOldConIn, &oldConsoleMode);
    SetConsoleMode(hOldConIn, ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT);
    return 0;
}

int ConClear(void) /*FOLD00*/
{
    int W, H;
    TDrawBuffer B;
    
    MoveChar(B, 0, ConMaxCols, ' ', 0x07, 1);
    if ((ConQuerySize(&W, &H) == 0) &&
        ConSetBox(0, 0, W, H, B[0])) return 0;
    return -1;
}


#if 0	// Mouse control not necessary when using console functions.
/*--------------------------------------------------------------------------*/
/*	CLASS:	tMouHelp is used to control mouse cursor visibility during		*/
/*			screen updates. 												*/
/*--------------------------------------------------------------------------*/
class tMouHelp
{
protected:
    int 	mh_x, mh_y; 			// Current mouse position / 0
    int 	mh_valid;
    int 	mh_disabled;			// T if mouse should be re-enabled.
    
public:
    tMouHelp() : mh_x(0), mh_y(0), mh_valid(FALSE), mh_disabled(FALSE)	{}
    ~tMouHelp()
    {	if(MouseVisible && mh_disabled) DrawMouse(1);
    }
    
    void	disIfLine(int x, int w, int y)
    {
        if(mh_disabled) return;
        if(! mh_valid)
        {
            ConQueryMousePos(&mh_x, &mh_y);
            mh_valid = TRUE;
        }
        if(y == mh_y && mh_x >= x && mh_x < x+y)
        {
            mh_disabled= TRUE;
            DrawMouse(0);
        }
    }
};
#endif

int ConPutBox(int X, int Y, int W, int H, PCell Cell) /*FOLD00*/
{
    int 		I;
    PCell		p = Cell;
    COORD		corg, csize;
    SMALL_RECT	rcl;
    BOOL		rc;
    
    for (I = 0; I < H; I++)
    {
        corg.X	= corg.Y = 0;
        csize.X = W;
        csize.Y = 1;
        rcl.Left= X;
        rcl.Top = I + Y;
        rcl.Bottom = I + Y + 1;
        rcl.Right = X + W;
        
        rc = WriteConsoleOutput(hConsole, (PCHAR_INFO)p, csize, corg, &rcl);
        if (rc != TRUE) {
            printf("WriteConsoleOutput %d\n", rc);
        }
        p += W;
    }
    return 0;
}

int ConGetBox(int X, int Y, int W, int H, PCell Cell) /*FOLD00*/
{
    int 		I;
    USHORT		WW = W << 1;
    PCell		p = Cell;
    COORD		corg, csize;
    SMALL_RECT	rcl;
    
    for (I = 0; I < H; I++)
    {
        corg.X = corg.Y = 0;
        csize.X = W;
        csize.Y = 1;
        rcl.Left = X;
        rcl.Top = I + Y;
        rcl.Bottom = I + Y + 1;
        rcl.Right = X + W;
        
        ReadConsoleOutput(hConsole, (PCHAR_INFO)p, csize, corg, &rcl);
        p += W;
    }
    return 0;
}

int ConPutLine(int X, int Y, int W, int H, PCell Cell) /*FOLD00*/
{
    int 		I;
    COORD		corg, csize;
    SMALL_RECT	rcl;
    BOOL rc;
    
    for (I = 0; I < H; I++)
    {
        corg.X = corg.Y = 0;
        csize.X = W;
        csize.Y = 1;
        rcl.Left = X;
        rcl.Top = I + Y;
        rcl.Bottom = I + Y + 1;
        rcl.Right = X + W;
        
        rc = WriteConsoleOutput(hConsole, (PCHAR_INFO)Cell, csize, corg, &rcl);
        if (rc != TRUE) {
            printf("WriteConsoleOutput %d\n", rc);
        }
    }
    return 0;
}

int ConSetBox(int X, int Y, int W, int H, TCell Cell) /*FOLD00*/
{
    int 		I;
    COORD		corg, csize;
    SMALL_RECT	rcl;
    TDrawBuffer B;
    
    I = W;
    while (I-- > 0) B[I] = Cell;
    
    for (I = 0; I < H; I++)
    {
        corg.X = corg.Y = 0;
        csize.X = W;
        csize.Y = 1;
        rcl.Left = X;
        rcl.Top = I + Y;
        rcl.Bottom = I + Y;
        rcl.Right = X + W;
        
        WriteConsoleOutput(hConsole, (PCHAR_INFO)B, csize, corg, &rcl);
    }
    return 0;
}

int ConScroll(int Way, int X, int Y, int W, int H, TAttr Fill, int Count) /*FOLD00*/
{
    TCell		FillCell;
    SMALL_RECT	rect, clip;
    COORD		dest;
    
    MoveCh(&FillCell, ' ', Fill, 1);
    
    clip.Left = X;
    clip.Top = Y;
    clip.Right = X + W - 1;
    clip.Bottom = Y + H - 1;
    
    rect = clip;
    dest.X = X;
    dest.Y = Y;
    
    switch (Way) {
    case csUp:
        rect.Top += Count;
        break;
    case csDown:
        rect.Bottom -= Count;
        dest.Y += Count;
        break;
    case csLeft:
        rect.Left += Count;
        break;
    case csRight:
        rect.Right += Count;
        dest.X += Count;
        break;
    }
    
    ScrollConsoleScreenBuffer(hConsole, &rect, &clip, dest, (PCHAR_INFO)&FillCell);
    return 0;
}

int ConSetSize(int X, int Y) { /*FOLD00*/
    return -1;
}

int ConQuerySize(int *X, int *Y) { /*FOLD00*/
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    
    GetConsoleScreenBufferInfo(hConsole, &csbi);
    *X = csbi.dwSize.X;
    *Y = csbi.dwSize.Y;
    return 0;
}

int ConSetCursorPos(int X, int Y) { /*FOLD00*/
    COORD xy;
    
    xy.X = X;
    xy.Y = Y;
    SetConsoleCursorPosition(hConsole, xy);
    return 0;
}

int ConQueryCursorPos(int *X, int *Y) { /*FOLD00*/
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    
    GetConsoleScreenBufferInfo(hConsole, &csbi);
    *X = csbi.dwCursorPosition.X;
    *Y = csbi.dwCursorPosition.Y;
    return 0;
}

int ConShowCursor(void) { /*FOLD00*/
    CursorVisible = 1;
    DrawCursor(1);
    return 0;
}

int ConHideCursor(void) { /*FOLD00*/
    CursorVisible = 0;
    DrawCursor(0);
    return 0;
}

int ConCursorVisible() { /*FOLD00*/
    return (CursorVisible == 1);
}

int ConSetCursorSize(int Start, int End) { /*FOLD00*/
    return -1;
}

int ConSetMousePos(int X, int Y) { /*FOLD00*/
    return -1;
}

int ConQueryMousePos(int *X, int *Y) { /*FOLD00*/
    *X = LastMouseX;
    *Y = LastMouseY;
    
    // NT does not have this ? (not needed anyway, but check mouse hiding above).
    return 0;
}

int ConShowMouse(void) { /*FOLD00*/
    MouseVisible = 1;
    if (!MousePresent) return -1;
    return 0;
}

int ConHideMouse(void) { /*FOLD00*/
    MouseVisible = 0;
    if (!MousePresent) return -1;
    return 0;
}

int ConMouseVisible() { /*FOLD00*/
    return (MouseVisible == 1);
}

int ConQueryMouseButtons(int *ButtonCount) { /*FOLD00*/
    return 0;
}

int ConPutEvent(TEvent Event) { /*FOLD00*/
    EventBuf = Event;
    return 0;
}

int ConFlush(void) { /*FOLD00*/
    return 0;
}

int ConGrabEvents(TEventMask EventMask) { /*FOLD00*/
    return 0;
}


static PCell SavedScreen = 0;
static int SavedX, SavedY, SaveCursorPosX, SaveCursorPosY;

int SaveScreen() { /*FOLD00*/
    if (SavedScreen)
        free(SavedScreen);
    
    ConQuerySize(&SavedX, &SavedY);
    
    SavedScreen = (PCell) malloc(SavedX * SavedY * sizeof(TCell));
    
    if (SavedScreen)
        ConGetBox(0, 0, SavedX, SavedY, SavedScreen);
    ConQueryCursorPos(&SaveCursorPosX, &SaveCursorPosY);
    return 0;
}

int RestoreScreen() { /*FOLD00*/
    if (SavedScreen) {
        ConPutBox(0, 0, SavedX, SavedY, SavedScreen);
        ConSetCursorPos(SaveCursorPosX, SaveCursorPosY);
    }
    return 1;
}


GUI::GUI(int &argc, char **argv, int XSize, int YSize) { /*FOLD00*/
    fArgc = argc;
    fArgv = argv;
    ::ConInit(-1, -1);
    SaveScreen();
    ::ConSetSize(XSize, YSize);
    gui = this;
}

GUI::~GUI() { /*FOLD00*/
    RestoreScreen();
    ::ConDone();
    gui = 0;
}

int GUI::ConSuspend(void) { /*FOLD00*/
    RestoreScreen();
    return ::ConSuspend();
}

int GUI::ConContinue(void) { /*FOLD00*/
    SaveScreen();
    return ::ConContinue();
}

int GUI::ShowEntryScreen() { /*FOLD00*/
    TEvent E;
    
    ConHideMouse();
    RestoreScreen();
    do { gui->ConGetEvent(evKeyDown, &E, -1, 1, 0); } while (E.What != evKeyDown);
    ConShowMouse();
    if (frames)
        frames->Repaint();
    return 1;
}

char ConGetDrawChar(int index) { /*FOLD00*/
    static char tab[] = "Ú¿ÀÙÄ³ÂÃ´ÁÅ\x1AúÄ±°\x1B\x1A";
    
    assert(index >= 0 && index < strlen(tab));
    
    return tab[index];
}


int GUI::RunProgram(char *Command) { /*FOLD00*/
    int rc, W, H, W1, H1;
    
    ConQuerySize(&W, &H);
    ConHideMouse();
    ConSuspend();
    
    if (*Command == 0)	// empty string = shell
        Command = getenv(
                         "COMSPEC"
                        );
    
    rc = system(Command);
    
    ConContinue();
    ConShowMouse();
    ConQuerySize(&W1, &H1);
    
    if (W != W1 || H != H1) {
        frames->Resize(W1, H1);
    }
    frames->Repaint();
    return rc;
}

int ConSetTitle(char *Title, char *STitle) { /*FOLD00*/
    return 0;
}

int ConGetTitle(char *Title, int MaxLen, char *STitle, int SMaxLen) { /*FOLD00*/
    strcpy(Title, "FTE");
    strcpy(STitle, "FTE");
    return 0;
}



/****************************************************************************/
/*																			*/
/*	CODING: Pipe handler.													*/
/*																			*/
/****************************************************************************/
/*--------------------------------------------------------------------------*/
/*	STATIC GLOBALS. 														*/
/*--------------------------------------------------------------------------*/
#define MAX_PIPES 	4
#define PIPE_BUFLEN 4096
#define PIPEBUF_SZ	4096

class NTHandle
{
protected:
    HANDLE  nth_h;
    
public:
    operator HANDLE()
    {   return nth_h;
    }
    
    void    close()
    {   if(nth_h != INVALID_HANDLE_VALUE)
        {
            CloseHandle(nth_h);
            nth_h = INVALID_HANDLE_VALUE;
        }
    }
    
    
    NTHandle()  {   nth_h = INVALID_HANDLE_VALUE;   }
    
    ~NTHandle()
    {   close();
    }
    
    NTHandle(const HANDLE& h) : nth_h(h)    {}
    NTHandle(const NTHandle& nth);      		// UNDEFINED (no assgn)
    NTHandle& operator =(const NTHandle& nth);  // UNDEFINED (no assgn)
    NTHandle& operator =(const HANDLE nth)
    {   close();
        nth_h = nth;
        return *this;
    }
};


class GPipe
{
public:
    int 	p_used;
    int 	p_id;
    char*	p_buffer;
    int 	p_buflen;
    int 	p_bufused;
    int 	p_bufpos;
    EModel* p_notify;
    char*	p_command;
    int 	p_retcode;
    int 	p_doterm;
    
    //** NT specific.
    HANDLE  p_proc_h;               // Handle of spawned process,
    HANDLE  p_data_evh;           	// Handle for EVENT when overlapped io done,
    HANDLE  p_pipe_ph;              // Input pipe (read by FTE)
    HANDLE  p_child_ph;             // Client side's handle (written to by spawned)
    DWORD   p_read_len;             // #bytes read in overlapped I/O
    int     p_io_posted;			// T when overlapped I/O is posted,
    int     p_completed;            // T when client process closed down.
    int     p_has_data;             // T when OVERLAPPED completed.
    OVERLAPPED  p_ovl;
    
    static GPipe    pipe_ar[MAX_PIPES];
    
    
public:
    int		open(char *Command, EModel *notify);
    int     close();
    int 	read(void *buffer, int len);
    int 	getEvent(TEvent* event);
    
    
protected:
    int 	createPipe();
    void 	releasePipe();
    int 	runCommand();
    void 	closeProc();
    int 	handlePost();
    int 	postRead();
    
    
    
public:
    static GPipe*	getFreePipe();
    static GPipe*   getPipe(int id);
    
};

GPipe GPipe::pipe_ar[MAX_PIPES];


#define dbm(x)		printf(x), Sleep(3000)


/*
 *	getFreePipe() locates an unused GPipe structure. It also assigns it's ID.
 */
GPipe* GPipe::getFreePipe() /*FOLD00*/
{
    int 	i;
    
    for(i = 0; i < MAX_PIPES; i++)
    {
        if(! pipe_ar[i].p_used)
        {
            pipe_ar[i].p_id = i;		// Set pipenr,
            return pipe_ar + i;
        }
    }
    return NULL;						// No free pipe
}


GPipe* GPipe::getPipe(int id) /*FOLD00*/
{
    if (id < 0 || id > MAX_PIPES) return NULL;
    if(! pipe_ar[id].p_used) return NULL;
    return pipe_ar + id;
}


int GPipe::createPipe() /*FOLD00*/
{
    /*
     *  Called from open() to create and open the server and the client pipes.
     */
    static int	PCount = 0;
    HANDLE		hchild;
    char		pipename[50];
    int 		ok;
    SECURITY_ATTRIBUTES sa;
    
    //** Create the named pipe, and handle the SERVER (edit)'s end...
    sprintf(pipename, "\\\\.\\pipe\\fte%d\\child%d", getpid(), PCount);
    p_pipe_ph = CreateNamedPipe(pipename,
                                PIPE_ACCESS_INBOUND | FILE_FLAG_OVERLAPPED,
                                PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
                                1,			// nMaxInstances,
                                0, PIPEBUF_SZ,
                                1000,
                                0);
    if(p_pipe_ph == INVALID_HANDLE_VALUE)
        return FALSE;
    PCount++;
    
    /*
     *	Client side: get a connection to the server's pipe. Do this before the
     *	call to ConnectNamedPipe() to prevent it from blocking.
     */
    sa.nLength = sizeof(sa);            // Security descriptor for INHERIT.
    sa.lpSecurityDescriptor = 0;
    sa.bInheritHandle	= 1;
    
#if 1
    p_child_ph	= CreateFile(pipename, GENERIC_WRITE, 0, &sa,
                             OPEN_EXISTING, 0, 0);
#else
    p_child_ph	= CreateFile("_test", GENERIC_WRITE|GENERIC_READ, 0, &sa,
                             CREATE_ALWAYS, 0, 0);
#endif
    if(p_child_ph == INVALID_HANDLE_VALUE)
        dbm("CreateFile(client_side_pipe) has failed");
    else
    {
        //** Server side: aquire connection..
        ok	= TRUE;
        if(! ConnectNamedPipe(p_pipe_ph, 0))	// Get connect;
        {
            if(GetLastError() != ERROR_PIPE_CONNECTED)
                ok = FALSE;
        }
        
        //** Connect worked?
        if(!ok)
            dbm("ConnectNmPipe() has failed");
        else
            return TRUE;                // All opened & ready for action!
        
        //** Something went wrong.
        CloseHandle(p_child_ph);		// Close child: was inh.
        DisconnectNamedPipe(p_pipe_ph);	// Force disconnection of client (-)
        CloseHandle(p_child_ph);
    }
    CloseHandle(p_pipe_ph);
    return FALSE;									// Something has failed.
}


void GPipe::releasePipe() /*FOLD00*/
{
    /*
     *  releasePipe() releases all that createPipe() allocates. It's usually
     *  called when an error causes the process to abort.
     */
    if(p_child_ph != INVALID_HANDLE_VALUE)
    {
        CloseHandle(p_child_ph);
        p_child_ph  = INVALID_HANDLE_VALUE;
    }
    
    if(p_pipe_ph != 0)
    {
        DisconnectNamedPipe(p_pipe_ph);
        CloseHandle(p_pipe_ph);
        p_pipe_ph = INVALID_HANDLE_VALUE;
    }
}


int GPipe::runCommand() /*FOLD00*/
{
    /*
     *  runCommand() takes the child pipe, dups it onto stdout and stderr while
     *  saving their old assignments, then it spawns 
     */
    HANDLE	hChildConsole;
    int 	ok;
    char*	comspec, *args;
    
    //** Save current stdout and stderr handles,
    ok  	= FALSE;
    //horgout = GetStdHandle(STD_OUTPUT_HANDLE);
    //	horgerr = GetStdHandle(STD_ERROR_HANDLE);

    hChildConsole = CreateConsoleScreenBuffer(GENERIC_READ | GENERIC_WRITE,
                                         0, NULL, 
                                         CONSOLE_TEXTMODE_BUFFER, NULL);
    SetStdHandle(STD_INPUT_HANDLE, hChildConsole);
    
    if(! SetStdHandle(STD_OUTPUT_HANDLE, p_child_ph))
        dbm("SetStdHandle(out) failed");
    else
    {
        //		if(! SetStdHandle(STD_ERROR_HANDLE, p_child_ph))
        //			dbm("SetStdHandle(err) failed");
        //		else
        {
            //** Handles redirected; now run the child asynchonously
            comspec = getenv("COMSPEC");
            args	= (char *)malloc(strlen(comspec) + strlen(p_command) + 20);
            if(args == 0)
                dbm("malloc() failed for command line..");
            else
            {
                //** Form a command line for the process;
                strcpy(args, comspec);
                strcat(args, " /c ");
                strcat(args, p_command);
                
                PROCESS_INFORMATION 	pi;
                STARTUPINFO 			si;
                
                /* Set up members of STARTUPINFO structure. */
                si.cb = sizeof(STARTUPINFO);
                si.lpReserved = NULL;
                si.lpReserved2 = NULL;
                si.cbReserved2 = 0;
                si.lpDesktop = NULL;
                si.dwFlags = 0;
                if (CreateProcess(NULL, args, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi))
                {
                    ok	= TRUE;
                    CloseHandle(pi.hThread);	// Thread handle not needed
                    p_proc_h	= pi.hProcess;	// Return process handle (to get RC)
                }
                
                free(args); 					// Release command line.
            }
            
            //** And restore the original handles.
            //			if(! SetStdHandle(STD_ERROR_HANDLE, horgerr))
            //				dbm("SetStdHandle(err) to original failed");
        }
        SetStdHandle(STD_OUTPUT_HANDLE, hOldConOut);
        SetStdHandle(STD_INPUT_HANDLE, hOldConIn);
        // seems to get lost somewhere ???
        //SetConsoleMode(horgout,
        //               ENABLE_WINDOW_INPUT |
        //               ENABLE_MOUSE_INPUT);
    }
    
    //** And last but not least: close the child handle.
    CloseHandle(p_child_ph);
    p_child_ph = INVALID_HANDLE_VALUE;
    return ok;
}


void GPipe::closeProc() /*FOLD00*/
{
    /*
     *  closeProc() gets called when a read fails. It assumes the process has
     *  ended, retrieves the process return code, then it closes all handles.
     *  The state is set to p_completed.
     */
    DWORD	ec;
    
    dbg("[closeProc] ");
    
    if(! GetExitCodeProcess(p_proc_h, &ec)) ec = 0xabcd;
    p_retcode   = ec;                   	// Save return code of process,
    if(p_proc_h != INVALID_HANDLE_VALUE)    // Close process,
    {
        CloseHandle(p_proc_h);
        p_proc_h = INVALID_HANDLE_VALUE;
    }
    
    //** Close the main pipe,
    if(p_pipe_ph != INVALID_HANDLE_VALUE)
    {
        CloseHandle(p_pipe_ph);
        p_pipe_ph = INVALID_HANDLE_VALUE;
    }
    p_completed = TRUE;
    p_has_data  = TRUE;
}


int GPipe::handlePost() /*FOLD00*/
{
    /*
     *  handlePost() gets called when a posted read has completed. If the read
     *	has returned data all flags are set allowing pipeRead() to be called
     *	till the returned data is complete. If the read failed it is assumed
     *	to be EOF; so the exit status of the process is saved and the process
     *	handle is freed.
     */
    DWORD   ec;
    
    dbg("[handlePost ");
    if(p_io_posted)                     // Called because post has completed?
    {
        p_bufpos    = 0;				// Data at start of buffer,
        p_io_posted	= FALSE;
        if(! GetOverlappedResult(p_pipe_ph, &p_ovl, &p_read_len, FALSE))
        {
            //** Treat as end-of-pipe.
            closeProc();
        }
        p_has_data  = TRUE;
        dbg("ok] ");
    }
    else
        dbg(" no post] ");
    return TRUE;
}


int GPipe::postRead() /*FOLD00*/
{
    /*
     *  postRead() checks if an overlapped READ needs to be posted by checking
     *  the io_posted flag. If that's clear and no termination or closed flag
     *  is set then a new overlapped I/O request is issued.
     */
    p_has_data  = FALSE;
    dbg("[postRead ");
    if(p_io_posted || p_completed)
        dbg("no action: %s] ", p_io_posted ? "posted" : "complete");
    else
    {
        p_ovl.hEvent    = p_data_evh;       // Signal this when done,
        if(!ReadFile(p_pipe_ph, p_buffer, p_buflen, &p_read_len, &p_ovl))
        {
            DWORD	ec = GetLastError();
            if(ec != ERROR_IO_PENDING)
            {
                //** Something's wrong. Treat as closed pipe for now.
                closeProc();                // Close pipe, complete stuff...
                dbg("postfail] ");
                return FALSE;               // And return failure.
            }
        }
        p_io_posted = TRUE;                	// Handle pending ioresult.
        dbg("posted] ");
    }
    return TRUE;
}


int GPipe::open(char* command, EModel* notify) /*FOLD00*/
{
    memset(&p_ovl, 0, sizeof(p_ovl));		// Clear overlapped,
    p_bufused   = 0;
    p_bufpos    = 0;
    p_io_posted = FALSE;
    p_has_data  = FALSE;
    p_completed = FALSE;                    // T if client closed.
    p_doterm    = FALSE;
    p_buflen    = PIPE_BUFLEN;
    p_notify    = notify;
    p_doterm    = FALSE;
    
    p_pipe_ph   = INVALID_HANDLE_VALUE;
    p_child_ph  = INVALID_HANDLE_VALUE;
    if( (p_command = strdup(command)) == 0)
        return -1;
    
    //** Allocate the read buffer;
    if( (p_buffer = (char*) malloc(p_buflen)) != 0)
    {
        //** Allocate the "overlapped IO complete" event semaphore,
        if( (p_data_evh = CreateEvent(0, 1, 0, 0)) == 0)
            dbm("CreateEvent(data_evh) failed.");
        else
        {
            if(createPipe())                // Create server & client pipe.
            {
                if(! postRead())
                    dbm("postRead() initial failed.");
                else
                {
                    if(runCommand())
                    {
                        p_used  = TRUE;
                        return p_id;
                    }
                }
                releasePipe();              // Release pipes,
            }
            CloseHandle(p_data_evh);
        }
        free(p_buffer);
    }
    free(p_command);
    return -1;
}


int GPipe::close() /*FOLD00*/
{
    /*
     *  close() disconnects from the spawned task, closes the pipe and releases
     *  all stuff.
     */
    if(! p_used) return -1;
    if(! p_completed)                       // Overlapped I/O not complete yet?
    {
        //** We *must* wait till the overlapped I/O completes,
        if(p_io_posted)
        {
            GetOverlappedResult(p_pipe_ph, &p_ovl, &p_read_len, TRUE);
            p_io_posted = FALSE;
        }
    }
    p_completed= TRUE;
    
    //** Now close all that might be pending,
    free(p_buffer);
    free(p_command);
    
    releasePipe();                      // Close all pipe stuff,
    if(p_proc_h != INVALID_HANDLE_VALUE)
    {
        CloseHandle(p_proc_h);
        p_proc_h = INVALID_HANDLE_VALUE;
    }
    
    CloseHandle(p_data_evh);
    
    p_used = FALSE;
    return p_retcode;
}


int GPipe::read(void *buffer, int len) /*FOLD00*/
{
    /*
     *  read() is called to get the current data from the pipe. It takes the
     *  #bytes read and returns them. It returns data till the buffer is
     *  exhausted. If the process is completed it returns -1; else it returns
     *  the #bytes read. It returns 0 if the buffer's empty.
     */
    dbg("[read ");
    if(p_has_data)
    {
        if(p_bufpos < p_read_len)               // Data in buffer?
        {
            unsigned    l;
            
            l   = p_read_len - p_bufpos;        // Try to output all,
            if(l > len) l = len;
            memcpy(buffer, p_buffer+p_bufpos, l);   // Copy data from the buffer,
            p_bufpos    += l;
            dbg("%u data] ", l);
            return l;							// Data returned,
        }
        
        //** There's nothing left in the buffer. Is the task complete?
        if(p_completed)
        {
            dbg("no data, complete] ");
            return -1;
        }
        
        if(! postRead())
        {
            dbg("post failed-> complete] ");
            return -1;
        }
        
        dbg("nodata, post] ");
        return 0;
    }
    else if(p_completed)
    {
        dbg("completed] ");
        return -1;
    }
    
    dbg("nothing] ");
    return 0;
}


int GPipe::getEvent(TEvent* event) /*FOLD00*/
{
    dbg("[getpipeevent: ");
    event->What = evNone;
    
    if(! p_used || p_notify == 0) return 0;     // No data.
    if(! handlePost()) return 0;                // Again: no data,
    //** This pipe has data!
    event->What			= evNotify;
    event->Msg.View 	= 0;
    event->Msg.Model	= p_notify;
    event->Msg.Command 	= cmPipeRead;
    event->Msg.Param1 	= p_id;
    dbg("ok] ");
    return 1;
}


/*
 *  NT Pipe handler - overview
 *  ==========================
 *  The NT pipe handler uses overlapped I/O to read console events.
 *
 *  OpenPipe():
 *  When the pipe is opened, one of the pipe structures is allocated and set
 *  to used. Then an event semaphore (reset_manual) is created. This semaphore
 *  will be signalled when data is available on the input pipe which gathers
 *  the spawned tasks's output.
 *
 *  Then a pipe is created, opened for the client side and stdout and stderr
 *  are redirected therein. After that the client task is spawned.
 *
 *  If the spawn succeeds an overlapped READ is posted for the pipe; then the
 *  OpenPipe function returns.
 *
 *  ConGetEvent():
 *  The ConGetEvent() handler does a WaitForMultipleObjects() on the console
 *  handle and all pipe handles currently active. If a pipe has data the
 *  overlapped result is gotten, and the output is sent to the message window.
 *  Then, if the thread didn't finish, a new overlapped read is posted.
 *
 *
 */
int GUI::OpenPipe(char *Command, EModel *notify) /*FOLD00*/
{
    GPipe*	gp;
    
    if( (gp = GPipe::getFreePipe()) == 0)
        return -1;                          // Out of pipes.
    return gp->open(Command, notify);       // And ask the pipe to init.
}


int GUI::SetPipeView(int id, EModel *notify) /*FOLD00*/
{
    GPipe*  p;
    
    if( (p = GPipe::getPipe(id)) == 0) return -1;
    p->p_notify = notify;
    return 0;
}


int GUI::ReadPipe(int id, void *buffer, int len) /*FOLD00*/
{
    int 	l;
    GPipe*	p;
    
    if( (p = GPipe::getPipe(id)) == 0) return -1;
    return p->read(buffer, len);
}


int GUI::ClosePipe(int id) /*FOLD00*/
{
    GPipe*	p;
    
    if( (p = GPipe::getPipe(id)) == 0) return -1;
    return p->close();
}


static int GetPipeEvent(int id, TEvent *Event) /*FOLD00*/
{
    int 	i;
    GPipe*	p;
    
    if( (p = GPipe::getPipe(id)) == 0) return -1;
    return p->getEvent(Event);
}


int ConGetEvent(TEventMask EventMask, TEvent *Event, int WaitTime, int Delete) /*FOLD00*/
{
    //** Any saved events left?
    if (EventBuf.What != evNone)
    {
        *Event = EventBuf;
        if (Delete) EventBuf.What = evNone;
        return 0;
    }
    if (MouseEv.What != evNone)
    {
        *Event = MouseEv;
        if (Delete) MouseEv.What = evNone;
        return 0;
    }

    EventBuf.What = evNone;
    Event->What = evNone;

    HANDLE		o_ar[1 + MAX_PIPES];
    DWORD		rc;
    int 		i, nh;
    
    //** Fill the handle array with all active handles for pipes && console,
    o_ar[0] = hConsole;
    for(i = 0, nh = 1; i < MAX_PIPES; i++)		// For all possible pipes
    {
        if(GPipe::pipe_ar[i].p_used)
            o_ar[nh++] = GPipe::pipe_ar[i].p_data_evh;
    }
    
    rc = WaitForMultipleObjects(nh, o_ar, FALSE, WaitTime);
    if(rc != WAIT_FAILED && (rc >= WAIT_OBJECT_0 && rc < WAIT_OBJECT_0+nh))
    {
        i = rc - WAIT_OBJECT_0;				// Get item that signalled new data
        if(i == 0)								// Was console?
            ReadConsoleEvent(Event);
        else
            GetPipeEvent(i - 1, Event); 		// Read data from pipe.
        return 0;
    }
    return -1;
}


