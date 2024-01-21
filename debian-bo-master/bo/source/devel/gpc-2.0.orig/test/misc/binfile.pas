{ Writing to a binary file }

program Write2;

type
  TString = string[255];
  byte = __byte__ integer;
  bytefile = file of byte;

procedure assign(var f: bytefile; name: TString);
var
  b: BindingType;
begin
  unbind(f);
  b := binding(f);
  b.name:= name + chr(0);
  bind(f,b);
end;

var
  f: bytefile;
  b: byte;

begin
  assign(f, 'hello1.bin');
  rewrite(f);

  for b:=0 to 30 do write(f, b);

  close(f);
end.
