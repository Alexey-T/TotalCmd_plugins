{$apptype console}

uses SProc;

function Decode(s: string): string;
var
  n1, n2: integer;
begin
  repeat
    n1:= Pos('<', s);
    if n1=0 then Break;
    n2:= Pos('>', Copy(s, n1+1, MaxInt));
    if n2=0 then Break;
    Delete(s, n1, n2+1);
  until false;
  Result:= ConvertFromUTF8(s);
end;

var
  s: string;
begin
  while not Eof do
    begin
    Readln(s);
    Writeln(Decode(s));
    end;
end.


