Program AsmNames;

Var
  S: __static__ String ( 10 );
  OK: __AsmName__ 's.2' String ( 10 );

Procedure puts ( C: __cstring__ );
AsmName 'puts';

begin
  S:= 'OK';
  puts ( OK );
end.
