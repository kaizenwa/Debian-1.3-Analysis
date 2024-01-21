Program WinHello;

{$X+}

type
  Word          = __unsigned__ integer;
  DWord         = __unsigned__ integer;         { unsigned 32-bit }

  THandle       = Word;
  HWnd          = THandle;
  PChar         = ^Char;

const
  mb_Ok               = $0000;
  mb_OkCancel         = $0001;

  mb_IconHand         = $0010;
  mb_IconQuestion     = $0020;
  mb_IconExclamation  = $0030;
  mb_IconAsterisk     = $0040;

  mb_IconInformation  = mb_IconAsterisk;
  mb_IconStop         = mb_IconHand;


{$ifdef UNICODE}
function MessageBox(WndParent: HWnd;
                    Txt, Caption: PChar;
                    TextType: Word): Integer; attribute stdcall; asmname 'MessageBoxW';
{$else}
function MessageBox(WndParent: HWnd;
                    Txt, Caption: PChar;
                    TextType: Word): Integer; attribute stdcall; asmname 'MessageBoxA';
{$endif}

function MessageBox(WndParent: HWnd;
                    Txt, Caption: PChar;
                    TextType: Word): Integer; external;

var
  caption : string[15];
  text    : string[25];

begin
  caption     := 'Hello World';
  caption[12] := chr(0);
  text        := 'This is GNU Pascal !';
  text[21]    := chr(0);

  MessageBox(0,
              @text[1],
              @caption[1],
              MB_ICONINFORMATION OR MB_OK);

end.
