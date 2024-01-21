{ Print the Windows version }
program PrintVersion;

{$define WINAPI(X) asmname X; attribute(stdcall)}

type
  Word = __unsigned__ integer;
  DWord = __unsigned__ integer;

function GetVersion: DWord; WINAPI('GetVersion');

function GetVersion: DWord; external;

var
  ver : Word;

begin
  ver := GetVersion;
  writeln('Windows version ', (ver AND $00FF):2, '.', (ver AND $FF00):2);
end.
