unit SProc;

interface

function GetIniKey(const section, key, default, fnIni: string): string;
procedure StrCopyBuf(p, p2: PAnsiChar; MaxLen: integer);
procedure StrCopyBufW(p, p2: PWideChar; MaxLen: integer);
procedure SReplace(var s: string; const sfrom, sto: string);
procedure SReplaceI(var s: string; const sfrom, sto: string);
procedure SReplaceAll(var s: string; const sfrom, sto: string);
procedure SReplaceIAll(var s: string; const sfrom, sto: string);

function SDeleteFrom(const s, sfrom: string): string;
function SDeleteTo(const s, sto: string): string;
function SBetween(const s, s1, s2: string): string;
function STail(const s: string; len: integer): string;
function SDeleteTags(const s: string): string;
function PosFrom(const SubStr, s: string; FromPos: integer): integer;
function Pos2(const SubStr1, SubStr2, s: string): integer;
function Pos2From(const SubStr1, SubStr2, s: string; FromPos: integer): integer;
function SCopyFromTo(const s: string; Pos: integer; const sTo: string): string;
function Min(n1, n2: integer): integer;

type
  TDecodeRec = record SFrom, STo: string; end;
  TDecodeProgress = procedure(N: integer);

function SDecode(const s: string; const Decode: array of TDecodeRec; Progress: TDecodeProgress): string;


implementation

uses
  SysUtils, Windows;

procedure StrCopyBuf(p, p2: PAnsiChar; MaxLen: integer);
begin
  FillChar(p^, MaxLen, 0);
  lstrcpynA(p, p2, MaxLen);
end;

procedure StrLCpyW(p, p2: PWideChar; MaxLen: integer);
begin
  FillChar(p^, MaxLen, 0);
  lstrcpynW(p, p2, MaxLen);
end;

procedure StrCopyBufW(p, p2: PWideChar; MaxLen: integer);
begin
  FillChar(p^, MaxLen, 0);
  lstrcpynW(p, p2, MaxLen div 2);
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


function SDeleteFrom(const s, sfrom: string): string;
var
  i: integer;
begin
  i:= Pos(sfrom, s);
  if i=0
    then Result:= s
    else Result:= Copy(s, 1, i-1);
end;

function SDeleteTo(const s, sto: string): string;
var
  i: integer;
begin
  Result:= s;
  i:= Pos(sto, s);
  if i>0 then Delete(Result, 1, i+Length(sto)-1);
end;

function SBetween(const s, s1, s2: string): string;
var
  n1, n2: integer;
begin
  Result:= '';
  n1:= Pos(s1, s);                     if n1=0 then Exit;
  n2:= Pos(s2, Copy(s, n1+1, MaxInt)); if n2=0 then Exit;
  Result:= Copy(s, n1+Length(s1), n2-Length(s1));
end;

function STail(const s: string; len: integer): string;
begin
  if Length(s)<=len
    then Result:= s
    else Result:= Copy(s, Length(s)-len+1, len);
end;

function SDeleteTags(const s: string): string;
var
  n1, n2: integer;
begin
  Result:= s;
  repeat
    n1:= Pos('<', Result);
    if n1=0 then Break;
    n2:= PosFrom('>', Result, n1+1);
    if n2=0 then Break;
    Delete(Result, n1, n2-n1+1);
  until false;
end;

//--------------------------------------------
function ToOEM(const s: Ansistring): Ansistring;
begin
  SetLength(Result, Length(s));
  CharToOemBuffA(PAnsiChar(s), PAnsiChar(Result), Length(s));
end;

function ToANSI(const s: Ansistring): Ansistring;
begin
  SetLength(Result, Length(s));
  OemToCharBuffA(PAnsiChar(s), PAnsiChar(Result), Length(s));
end;

function PosFrom(const SubStr, s: string; FromPos: integer): integer;
var
  n: integer;
begin
  for n:= FromPos to Length(s)-Length(SubStr)+1 do
    if SubStr=Copy(s, n, Length(SubStr)) then
      begin Result:= n; Exit end;
  Result:= 0;
end;

function Pos2(const SubStr1, SubStr2, s: string): integer;
begin
  Result:= Pos2From(SubStr1, SubStr2, s, 1);
end;

function Pos2From(const SubStr1, SubStr2, s: string; FromPos: integer): integer;
var
  n: integer;
begin
  for n:= FromPos to Length(s) do
    if (SubStr1=Copy(s, n, Length(SubStr1))) or
       (SubStr2=Copy(s, n, Length(SubStr2))) then
      begin Result:= n; Exit end;
  Result:= 0;
end;

function Min(n1, n2: integer): integer;
begin
  if n1<n2 then Result:= n1 else Result:= n2;
end;

function SDecode(const s: string; const Decode: array of TDecodeRec; Progress: TDecodeProgress): string;
var
  i, j: integer;
  DoDecode: boolean;
begin
  Result:= '';
  i:= 1;
  repeat
    if i>Length(s) then Break;
    DoDecode:= false;
    for j:= Low(decode) to High(decode) do
      with Decode[j] do
        if SFrom=Copy(s, i, Length(SFrom)) then
          begin
          DoDecode:= true;
          Result:= Result+STo;
          Inc(i, Length(SFrom));
          Break
          end;
    if DoDecode then Continue;
    Result:= Result+s[i];
    Inc(i);

    if Assigned(Progress) then
      Progress(i*100 div Length(s));
  until false;
end;

function SCopyFromTo(const s: string; Pos: integer; const sTo: string): string;
var
  n: integer;
begin
  n:= PosFrom(sTo, s, Pos);
  Result:= Copy(s, Pos, n-Pos);
end;

function GetIniKey(const section, key, default, fnIni: string): string;
var
  buf: array[0..300] of char;
begin
  GetPrivateProfileString(PChar(section), PChar(key), PChar(default),
    buf, SizeOf(buf), PChar(fnIni));
  Result:= buf;
end;


end.
