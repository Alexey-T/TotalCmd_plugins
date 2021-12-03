unit SProc;

interface

function SFmt(const s: string; len: integer): string;
function SParamName(const s: string): string;
function SParamVal(const s: string): string;
function SExpandVars(const s: string): string;
function SExpanded(const s: string): boolean;

procedure SReplace(var s: string; const sfrom, sto: string);
procedure SReplaceI(var s: string; const sfrom, sto: string); //not case-sensitive
procedure SReplaceAll(var s: string; const sfrom, sto: string);
procedure SReplaceIAll(var s: string; const sfrom, sto: string); //not case-sensitive

function SReplace1(const s: string; const sfrom, sto: string): string;
function SDeleteFrom(const s, sfrom: string): string;
function SDeleteTo(const s, sto: string): string; // 'some,,,s' -> 's'
function SCopyFrom(const s, sfrom: string): string;
function SCount(ch: char; const s: string): integer;

type TCharSet = set of char;
function STrimLeft(var s: string; chars: TCharSet): string;
function STrimRight(var s: string; chars: TCharSet): string;

function SGetWord1(var s: string): string;
function SGetVersion(s: string): string;
function SCharCount(const S: string; ch: Char): integer;
function SIEqual(const S1, S2: string): boolean;

function ToOEM(const s: string): string;
function ToANSI(const s: string): string;

function IMin(N1, N2: integer): integer;
function IMax(N1, N2: integer): integer;


implementation

uses
  Windows, SysUtils;

function SFmt(const s: string; len: integer): string;
begin
  if Length(s)>=len 
    then
      Result:= Copy(s, 1, len)
    else
      begin
      SetLength(Result, len);
      FillChar(Result[1], len, ' ');
      Move(s[1], Result[1], Length(s));
      end;
end;

function SParamName(const s: string): string;
begin
  Result:= Copy(s, 1, Pos('=', s)-1);
  STrimRight(Result, [' ']);
end;

function SParamVal(const s: string): string;
var
  i: integer;
begin
  i:= Pos('=', s);
  if i>0
    then Result:= Copy(s, i+1, MaxInt)
    else Result:= '';
  STrimLeft(Result, [' ']);
end;


procedure SReplace(var s: string; const sfrom, sto: string);
var
  i: integer;
begin
  i:= Pos(sfrom, s);
  if i>0 then
    begin
    Delete(s, i, Length(sfrom));
    Insert(sto, s, i);
    end;
end;

procedure SReplaceI(var s: string; const sfrom, sto: string);
var
  i: integer;
begin
  i:= Pos(LowerCase(sfrom), LowerCase(s));
  if i>0 then
    begin
    Delete(s, i, Length(sfrom));
    Insert(sto, s, i);
    end;
end;


procedure SReplaceAll(var s: string; const sfrom, sto: string);
var
  i: integer;
begin
  repeat
    i:= Pos(sfrom, s); 
    if i=0 then Break;
    Delete(s, i, Length(sfrom));
    Insert(sto, s, i);
  until false;
end;

procedure SReplaceIAll(var s: string; const sfrom, sto: string);
var
  i: integer;
begin
  repeat
    i:= Pos(LowerCase(sfrom), LowerCase(s)); 
    if i=0 then Break;
    Delete(s, i, Length(sfrom));
    Insert(sto, s, i);
  until false;
end;


function SReplace1(const s: string; const sfrom, sto: string): string;
begin
  Result:= s;
  SReplace(Result, sFrom, sTo);
end;


function SExpandVars(const s: string): string;
var
  buf: array[0..2*1024-1] of char;
begin
  SetString(Result, buf, ExpandEnvironmentStrings(PChar(s), buf, SizeOf(buf))-1);
end;

function SExpanded(const s: string): boolean;
begin
  Result:= Pos('%', s)=0;
end;

function IsDigit(ch: char): boolean;
begin
  Result:= ch in ['0'..'9', '.'];
end;

// 'name1;name2;name3'
function SGetWord1(var s: string): string;
var
  i: integer;
begin
  i:= Pos(';', s);
  if i=0 then i:= MaxInt;
  Result:= Copy(s, 1, i-1);
  Delete(s, 1, i);
end;

// ' nn.nn '
function SGetWord2(s: string): string;
var
  i1, i2: integer;
begin
  i1:= 1;  while (i1<=Length(s)) and (s[i1]=' ') do Inc(i1);
  i2:= i1; while (i2<=Length(s)) and IsDigit(s[i2]) do Inc(i2);
  Result:= Copy(s, i1, i2-i1);
end;


function STrimRight(var s: string; chars: TCharSet): string;
begin
  while (s<>'') and (s[Length(s)] in chars) do
    SetLength(s, Length(s)-1);
  Result:= s;
end;

function STrimLeft(var s: string; chars: TCharSet): string;
begin
  while (s<>'') and (s[1] in chars) do
    Delete(s, 1, 1);
  Result:= s;
end;

function SGetVersion(s: string): string;
var
  n, i1, i2: integer;
begin
  Result:= '';
  repeat
    n:= Pos('.', s);
    if n=0 then Exit;
    if (n>1) and (n<Length(s)) and IsDigit(s[n-1]) and IsDigit(s[n+1]) 
      then
        begin
        i1:= n-1;  while (i1>=1) and IsDigit(s[i1]) do Dec(i1);
        i2:= n+1;  while (i2<=Length(s)) and IsDigit(s[i2]) do Inc(i2);
        Result:= Copy(s, i1+1, i2-i1-1);
        if LowerCase(Copy(s, i2+1, 4))='beta' then
          Result:= Result+'.'+SGetWord2(Copy(s, i2+5, MaxInt));
        STrimRight(Result, ['.']);
        Exit
        end
      else
        Delete(s, 1, n+1);
  until Length(s)<3;
end;


(*
function SGetEmail(const s: string): string;
  function IsLetter(ch: char): boolean;
  begin
    Result:= UpCase(ch) in ['A'..'Z', '0'..'9', '.', '_', '-'];
  end;
var
  n, i1, i2: integer;
begin
  Result:= '';
  n:= Pos('@', s);
  if (n>1) and (n<Length(s)) and IsLetter(s[n-1]) and IsLetter(s[n+1]) then
    begin
    i1:= n-1;  while (i1>=1) and IsLetter(s[i1]) do Dec(i1);
    i2:= n+1;  while (i2<=Length(s)) and IsLetter(s[i2]) do Inc(i2);
    Result:= Copy(s, i1+1, i2-i1-1);
    STrimRight(Result, ['.', '_', '-']);
    end
end;
*)


function SDeleteFrom(const s, sfrom: string): string;
var
  i: integer;
begin
  i:= Pos(sfrom, s);
  if i=0 
    then Result:= s
    else Result:= Copy(s, 1, i-1);
end;

function SDeleteTo(const s, sto: string): string; // 'some,,,s' -> 's'
var
  i: integer;
begin
  Result:= s;
  i:= Pos(sto, s);
  if i>0 then Delete(Result, 1, i+Length(sto)-1);
end;


function SCopyFrom(const s, sfrom: string): string;
begin
  Result:= Copy(s, Pos(sfrom, s)+1, MaxInt);
end;


function SCount(ch: char; const s: string): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to Length(s) do
    if s[i]=ch then Inc(Result);
end;


function ToOEM(const s: string): string;
begin
  SetLength(Result, Length(s));
  CharToOemBuff(PChar(s), PChar(Result), Length(s));
end;

function ToANSI(const s: string): string;
begin
  SetLength(Result, Length(s));
  OemToCharBuff(PChar(s), PChar(Result), Length(s));
end;


function _Max(n1, n2: integer): integer;
begin
  if n1>n2 then Result:= n1 else Result:= n2;
end;

function IMin(N1, N2: integer): integer;
begin
  if N1<N2 then
    Result:= N1
  else
    Result:= N2;
end;

function IMax(N1, N2: integer): integer;
begin
  if N1>N2 then
    Result:= N1
  else
    Result:= N2;
end;

function SCharCount(const S: string; ch: Char): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    if S[i]=ch then Inc(Result);
end;

function SIEqual(const S1, S2: string): boolean;
begin
  Result:= StrIComp(PChar(S1), PChar(S2)) = 0;
end;

end.
