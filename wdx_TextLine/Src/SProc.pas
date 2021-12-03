unit SProc;

interface

function FShortName(const fn: string): string;

type TCharSet = set of AnsiChar;
procedure SReplaceAll(var s: string; const sfrom, sto: string);
procedure STrimLeft(var s: string; chars: TCharSet);
procedure STrimRight(var s: string; chars: TCharSet);
//function ToOEM(const s: string): string;
function ToANSI(const s: Ansistring): Ansistring;
function GetIniKey(const section, key, default, fnIni: string): string;

implementation

uses Windows;

//--------------------------------------------
procedure STrimRight(var s: string; chars: TCharSet);
var
  i: integer;
begin
  i:= Length(s);
  while (i>0) and (s[i] in chars) do Dec(i);
  SetLength(s, i);
end;

procedure STrimLeft(var s: string; chars: TCharSet);
var
  i: integer;
begin
  i:= 1;
  while (i<=Length(s)) and (s[i] in chars) do Inc(i);
  Delete(s, 1, i-1);
end;

//--------------------------------------------
{
function ToOEM(const s: Ansistring): Ansistring;
begin
  SetLength(Result, Length(s));
  CharToOemBuff(PChar(s), PChar(Result), Length(s));
end;
}

function ToANSI(const s: Ansistring): Ansistring;
begin
  SetLength(Result, Length(s));
  OemToCharBuffA(PAnsiChar(s), PAnsiChar(Result), Length(s));
end;

//--------------------------------------------
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


//--------------------------------------------
function FShortName(const fn: string): string;
var
  buf: array[0..MAX_PATH-1] of char;
begin
  Result:= '';
  FillChar(buf, SizeOf(buf), 0);
  if GetShortPathName(PChar(fn), buf, MAX_PATH)>0 then
    Result:= buf;
end;

//--------------------------------------------
function GetIniKey(const section, key, default, fnIni: string): string;
var
  buf: array[0..300] of char;
begin
  GetPrivateProfileString(PChar(section), PChar(key), PChar(default),
    buf, 300, PChar(fnIni));
  Result:= buf;
end;

end.
