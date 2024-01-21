{ Print the machine name }
program CompName;

{$X+}
{$define WINAPI(X) asmname X; attribute(stdcall)}

type
  Word  = __unsigned__ integer;
  DWord = __unsigned__ integer;
  PWord = ^Word;
  PChar = ^Char;

function GetComputerName(pBuff: PChar; pBuffLen: PWord): Boolean; WINAPI('GetComputerNameA');
function GetComputerName(pBuff: PChar; pBuffLen: PWord): Boolean; external;

function GetLastError: DWord; WINAPI('GetLastError');
function GetLastError: DWord; external;

{ This is stupid }
procedure WritePChar(pc: PChar);
begin
  while pc^<>chr(0) do
  begin
    write(pc^);
    pc:=pc+1;
  end;
end;

const
  BuffLen  : Word = 80;

var
  pBuffLen : PWord;
  pBuff    : PChar;

begin
  getmem(pBuff, BuffLen);
  new(pBuffLen);
  pBuffLen^ := BuffLen;

  if not GetComputerName(pBuff, pBuffLen)
  then
    writeln('Error : ', GetLastError, ': see <winerror.h>')
  else begin
         write('This computer is : ');
         WritePChar(pBuff);
         writeln;
       end;

  freemem(pBuff, BuffLen);
  dispose(pBuffLen);
end.
