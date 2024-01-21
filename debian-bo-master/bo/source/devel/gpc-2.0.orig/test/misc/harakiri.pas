(* Commit suicide with a signal of your choice *)
program Harakiri(output);

(* I assume these are in your C library:
 *	pid_t getpid(void);
 *	int kill(pid_t pid, int sig);
 * and pid_t is really int
 *)

{$X+}

type
  pid_t = integer;
  TString = string[255];

(* We really should get some standard libraries <sigh> *)

function getpid: pid_t; C;
function kill(pid: pid_t; sig: integer): integer; C;
function getpid: pid_t; external;
function kill(pid: pid_t; sig: integer): integer; external;

{$W-}	{ don't complain about underscores }
function _p_paramcount: integer; C;
function _p_paramstr(num: integer; var str: TString): integer; C;
function _p_paramcount: integer; external;
function _p_paramstr(num: integer; var str: TString): integer; external;
{$W+}

function ParamCount: integer;
begin
  ParamCount := _p_paramcount - 1;
end;

function ParamStr(num: integer): TString;
var
  s: TString;
begin
  if _p_paramstr(num, s)
  then
    ParamStr := trim(s)
  else
    ParamStr := '';
end;

function atoi(s: TString): integer;
var
  i, j, l : integer;
begin
  i := 0;
  l := length(s);

  for j:=1 to l do
    i := i+ integer((ord(s[j]) - 48) * (10**(l-j)));

  atoi := i;
end;


begin
  if (ParamCount < 1)
  then 
    writeln('Usage: ', ParamStr(0), ' [signal number]')
  else
    kill(getpid, atoi(ParamStr(1)) );
end.
