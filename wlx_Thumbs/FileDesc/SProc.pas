{$I-}
unit SProc;

interface

function Trim(const S: string): string;
function TrimLeft(const S: string): string;
function TrimRight(const S: string): string;
function UpperCase(const S: string): string;
function LowerCase(const S: string): string;
function StrIComp(Str1, Str2: PChar): Integer; assembler;

function ExtractFileName(const fn: string): string;
function ExtractFilePath(const fn: string): string;
function ExtractFileExt(const fn: string): string;
function ChangeFileName(const fn, NewName: string): string;
function FShortName(const fn: string): string;
function IsFileExist(const fn: string): boolean;
function GetPluginFilename: string;

function IntToStr(n: integer): string;
function StrToIntDef(const s: string; default: integer): integer;
function FloatToStr(n: double): string;

type
  TCharSet = set of char;

procedure SReplaceAll(var s: string; const sFrom, sTo: string);
procedure STrimLeft(var s: string; chars: TCharSet);
procedure STrimRight(var s: string; chars: TCharSet);
function PosFrom(const SubStr, s: string; FromPos: integer): integer;
function SBetween(const s, s1, s2: string): string;
function SBetweenI(const s, s1, s2: string): string;
procedure StrLCpy(p, p2: PChar; MaxLen: integer);
function ToOEM(const s: string): string;
function ToANSI(const s: string): string;
function GetIniKey(const section, key, default, fnIni: string): string;
procedure SetIniKey(const section, key, value, fnIni: string);
procedure DelIniKey(const section, key, fnIni: string);
function SRead(const fn: string; nLines: integer; const sDelimiter: string): string;
function SFormat(const fmt: string; params: array of const): string;

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
procedure StrLCpy(p, p2: PChar; MaxLen: integer);
begin
  if lstrlen(p2)>=MaxLen
    then
      begin
      lstrcpyn(p, p2, MaxLen);
      p[MaxLen-1]:= #0;
      end
    else
      lstrcpy(p, p2);
end;

//--------------------------------------------
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

//--------------------------------------------
function StrIComp(Str1, Str2: PChar): Integer; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     EAX,EAX
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,EDX
        XOR     EDX,EDX
@@1:    REPE    CMPSB
        JE      @@4
        MOV     AL,[ESI-1]
        CMP     AL,'a'
        JB      @@2
        CMP     AL,'z'
        JA      @@2
        SUB     AL,20H
@@2:    MOV     DL,[EDI-1]
        CMP     DL,'a'
        JB      @@3
        CMP     DL,'z'
        JA      @@3
        SUB     DL,20H
@@3:    SUB     EAX,EDX
        JE      @@1
@@4:    POP     ESI
        POP     EDI
end;

//--------------------------------------------
function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function LowerCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

//--------------------------------------------
function Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeft(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function TrimRight(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

//--------------------------------------------
function ExtractFileName(const fn: string): string;
var
  i: integer;
begin
  i:= Length(fn);
  while (i>0) and (fn[i]<>'\') do Dec(i);
  Result:= Copy(fn, i+1, MaxInt);
end;

function ExtractFilePath(const fn: string): string;
var
  i: integer;
begin
  i:= Length(fn);
  while (i>0) and (fn[i]<>'\') do Dec(i);
  Result:= Copy(fn, 1, i);
end;

function ExtractFileExt(const fn: string): string;
var
  i: integer;
begin
  i:= Length(fn);
  while (i>0) and (not (fn[i] in ['.', '\'])) do Dec(i);
  if (i>0) and (fn[i]='.')
    then Result:= Copy(fn, i, MaxInt)
    else Result:= '';
end;

function ChangeFileName(const fn, NewName: string): string;
var
  i: integer;
begin
  i:= Length(fn);
  while (i>0) and (fn[i]<>'\') do Dec(i);
  Result:= Copy(fn, 1, i)+NewName;
end;

//--------------------------------------------
function IntToStr(n: integer): string;
begin
  Str(n:0, Result);
end;

function StrToIntDef(const s: string; default: integer): integer;
var
  code: integer;
begin
  Val(s, Result, code);
  if code>0 then Result:= default;
end;

function FloatToStr(n: double): string;
begin
  Str(n:0:0, Result);
end;

//--------------------------------------------
procedure SReplaceAll(var s: string; const sFrom, sTo: string);
var
  i: integer;
begin
  repeat
    i:= Pos(sFrom, s); 
    if i=0 then Break;
    Delete(s, i, Length(sFrom));
    Insert(sTo, s, i);
  until false;
end;

//--------------------------------------------
function IsFileExist(const fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
begin
  if fn='' then begin Result:= false; Exit end;
  h:= FindFirstFile(PChar(fn), fd);
  Result:= h<>INVALID_HANDLE_VALUE;
  if Result then Windows.FindClose(h);
end;

//--------------------------------------------
function GetPluginFilename: string;
var
  buf: array[0..MAX_PATH-1] of char;
begin
  SetString(Result, buf, GetModuleFileName(hInstance, buf, SizeOf(buf)));
end;

//--------------------------------------------
function FShortName(const fn: string): string;
var
  buf: array[0..MAX_PATH-1] of char;
begin
  SetString(Result, buf, GetShortPathName(PChar(fn), buf, SizeOf(buf)));
end;

//--------------------------------------------
function GetIniKey(const section, key, default, fnIni: string): string;
var
  buf: array[0..300] of char;
begin
  GetPrivateProfileString(PChar(section), PChar(key), PChar(default),
    buf, SizeOf(buf), PChar(fnIni));
  Result:= buf;
end;

procedure SetIniKey(const section, key, value, fnIni: string);
begin
  WritePrivateProfileString(PChar(section), PChar(key), PChar(value), PChar(fnIni));
end;

procedure DelIniKey(const section, key, fnIni: string);
begin
  WritePrivateProfileString(PChar(section), PChar(key), nil, PChar(fnIni));
end;

//--------------------------------------------
function SBetween(const s, s1, s2: string): string;
var
  n1, n2: integer;
begin
  Result:= '';
  n1:= Pos(s1, s);                     if n1=0 then Exit;
  n2:= Pos(s2, Copy(s, n1+1, MaxInt)); if n2=0 then Exit;
  Result:= Copy(s, n1+Length(s1), n2-Length(s1));
end;

function SBetweenI(const s, s1, s2: string): string;
var
  n1, n2: integer;
  ss: string;
begin
  Result:= '';
  ss:= LowerCase(s);
  n1:= Pos(LowerCase(s1), ss);                     if n1=0 then Exit;
  n2:= Pos(LowerCase(s2), Copy(ss, n1+1, MaxInt)); if n2=0 then Exit;
  Result:= Copy(s, n1+Length(s1), n2-Length(s1));
end;

//--------------------------------------------
function SRead(const fn: string; nLines: integer; const sDelimiter: string): string;
var
  f: Text;
  s: string;
  i: integer;
begin
  Result:= '';
  AssignFile(f, fn);
  Reset(f);
  if IOResult<>0 then Exit;
  for i:= 1 to nLines do
    begin
    if Eof(f) then Break;
    Readln(f, s);
    Result:= Result+s+sDelimiter;
    end;
  CloseFile(f);
end;

//--------------------------------------------
function SFormat(const fmt: string; params: array of const): string;
var Buffer: array[ 0..2047 ] of Char;
    ElsArray, El: PDWORD;
    I : Integer;
    P : PDWORD;
begin
  ElsArray := nil;
  if High( params ) >= 0 then
    GetMem( ElsArray, (High( params ) + 1) * sizeof( Pointer ) );
  El := ElsArray;
  for I := 0 to High( params ) do
  begin
    P := @params[ I ];
    P := Pointer( P^ );
    El^ := DWORD( P );
    Inc( El );
  end;
  wvsprintf( @Buffer[0], PChar( fmt ), PChar( ElsArray ) );
  Result := Buffer;
  if ElsArray <> nil then
     FreeMem( ElsArray );
end;

//--------------------------------------------
function PosFrom(const SubStr, s: string; FromPos: integer): integer;
var
  n: integer;
begin
  for n:= FromPos to Length(s)-Length(SubStr)+1 do
    if SubStr=Copy(s, n, Length(SubStr)) then
      begin Result:= n; Exit end;
  Result:= 0;
end;


end.
