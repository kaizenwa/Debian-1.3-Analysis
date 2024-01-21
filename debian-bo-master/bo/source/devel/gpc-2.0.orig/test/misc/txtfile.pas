{ writing to a textfile }

program Write1;

type
  TString = string[255];

procedure assign(var f: text; name: TString);
var
  b: BindingType;
begin
  unbind(f);
  b := binding(f);
  b.name:= name + chr(0);
  bind(f,b);
end;

var
  f: text;

begin
  assign(f, 'hello1.txt');
  rewrite(f);

  writeln(f,'Hello World!');
  writeln(f,'Goodbye, cruel world!');

  close(f);
end.
