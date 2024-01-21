{ This is Extended Pascal }
{ See ISO 10206:1990 paragraphs 6.4.3.4, 6.7.5.8 and  6.7.5.9 }

program TimeDemo;

var
  t: TimeStamp;

begin
  GetTimeStamp(t);

  if t.DateValid
  then
    writeln('Today is: ', date(t))
  else
    writeln('GetTimeStamp returned an invalid date.');

  if t.TimeValid
  then
    writeln('Current time is: ',time(t))
  else
    writeln('GetTimeStamp returned an invalid time.');
end.
