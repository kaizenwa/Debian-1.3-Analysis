{ Sample program to demonstrate the Fibonacci sequence }
program Fibonacci(output);

var
  i : integer;

{ calculate the fibonacci sequence for N }
function Fib(N : integer) : real;
var
  F1, F2 : real;
begin
  if N = 0 then
    Fib := 0.0
  else
    if N = 1 then
      Fib := 1.0
    else
      Fib := Fib(N - 1) + Fib(N - 2);
end;

begin
  for i := 0 to 15 do
    Writeln(i, '. ', Fib(i));
end.
