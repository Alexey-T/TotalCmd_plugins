unit SProc;

interface

uses Windows;

function AStrToWStr(const s: RawByteString; CodePage: Cardinal): string;
function WStrToAStr(const s: UnicodeString; const CodePage: Cardinal): RawByteString;

function StrCopyW(Dest: PWideChar; const Source: PWideChar): PWideChar;
function PTrim(const S: string): string; inline;
procedure StrLCpy(p, p2: PWideChar; MaxLen: integer);
procedure StrLCpyA(p, p2: PAnsiChar; MaxLen: integer);
function Format(const fmt: string; params: array of const): string;
function UpperCase(const S: string): string;
function LowerCase(const S: string): string;
function IntToStr(n: integer): string;
function StrToIntDef(const s: string; default: integer): integer;

function ExtractFileExt(const fn: string): string;
function FileSetAttr(const FileName: string; Attr: Integer): Integer;
function IsFileExist(const fn: string): boolean;

procedure SReplaceAll(var s: string; const sfrom, sto: string);
procedure SReplaceIAll(var s: string; const sfrom, sto: string);

function WideToANSI(sStr: string): AnsiString;
function IsUTF8Memory(AMem: PBYTE; ASize: Int64): boolean;

function SwapOrder32(Value: DWord): DWord;
function SameText(const S1, S2: string): Boolean;

implementation

function WStrToAStr(const s: UnicodeString; const CodePage: Cardinal): RawByteString;
var
  AStr: RawByteString;
  n: Integer;
begin
  n := WideCharToMultiByte(CodePage, 0, PWideChar(s), Length(s), nil, 0, nil, nil);
  if n = 0 then
    Exit;
  SetLength(AStr, n);
  n := WideCharToMultiByte(CodePage, 0, PWideChar(s), Length(s), PAnsiChar(AStr), n, nil, nil);
  if n = 0 then
    Exit;
  Result := AStr;
end;

function AStrToWStr(const s: RawByteString; CodePage: Cardinal): string;
var
  WStr: string;
  n: integer;
begin
  n := MultiByteToWideChar(CodePage, 0, PAnsiChar(s), Length(s), nil, 0);
  if n = 0 then
    Exit;
  SetLength(WStr, n);
  n := MultiByteToWideChar(CodePage, 0, PAnsiChar(s), Length(s), PWideChar(WStr), n);
  if n = 0 then
    Exit;
  Result := WStr;
end;

function StrCopyW(Dest: PWideChar; const Source: PWideChar): PWideChar;
var
  Src: PWideChar;
begin
  Result := Dest;
  if Dest <> nil then
  begin
    Src := Source;
    if Src <> nil then
      while Src^ <> #0 do
      begin
        Dest^ := Src^;
        Inc(Src);
        Inc(Dest);
      end;
    Dest^ := #0;
  end;
end;

function PTrim(const S: string): string; inline;
var
  I, L: Integer;
  P: PChar;
begin
  Result := '';
  L := Length(S) - 1;
  if L < 0 then
    Exit;
  I := 0;
  P := Pointer(S);
  if (L > -1) and (P[I] > ' ') and (P[L] > ' ') then Exit(S);
  while (I <= L) and (P[I] <= ' ') do Inc(I);
  if I > L then Exit('');
  while P[L] <= ' ' do Dec(L);
  Result := Copy(S, I + 1, L - I + 1);
end;

function SameText(const S1, S2: string): Boolean;
begin
  Result:=(lstrcmpiW(PWideChar(S1), PWideChar(S2)) = 0);
end;

function SwapOrder32(Value: DWord): DWord;
type Tab_3 = array[0..3]of Byte;
begin
    Tab_3(Result)[0]:= Tab_3(Value)[3];
    Tab_3(Result)[1]:= Tab_3(Value)[2];
    Tab_3(Result)[2]:= Tab_3(Value)[1];
    Tab_3(Result)[3]:= Tab_3(Value)[0];
end;
function FileSetAttr(const FileName: string; Attr: Integer): Integer;
begin
  Result := 0;
  if not SetFileAttributes(PWideChar(FileName), Attr) then
    Result := GetLastError;
end;

function WideToANSI(sStr: string): AnsiString;
var
  pw: PAnsiChar;
  iSize, iNewSize: integer;
begin
  iSize := Length(sStr) + 1;

  pw := AllocMem(iSize);
  try
    WideCharToMultiByte(CP_ACP, 0, PWideChar(sStr), iSize, pw, iSize, nil, nil);
    Result := pw;
  finally
    FreeMem(pw)
  end;
end;

function IsUTF8Memory(AMem: PBYTE; ASize: Int64): boolean;
var
  i: Int64;
  c: Integer;

  function UTF8CharLength(const c: BYTE): Integer;
  begin
    // First Byte: 0xxxxxxx
    if ((c and $80) = $00) then
      Result:=1
    // First Byte: 110yyyyy
    else if ((c and $E0) = $C0) then
      Result:=2
    // First Byte: 1110zzzz
    else if ((c and $F0) = $E0) then
      Result:=3
    // First Byte: 11110uuu
    else if ((c and $F8) = $F0) then
      Result:=4
    // not valid, return the error value
    else
      Result:=-1;
  end;

  //After than you check all the trail bytes for that characters (if any)
  //for conformity with this:
  function UTF8IsTrailChar(const c: BYTE): BOOLEAN;
  begin
    // trail bytes have this form: 10xxxxxx
    Result:=((c and $C0) = $80);
  end;

begin
  Result := True;
  i := 0;
  while (i < ASize) do
  begin
    // get the length if the current UTF-8 character
    c:=UTF8CharLength(AMem^);
    // check if it is valid and fits into ASize
    if ((c>= 1) and (c <= 4) and ((i+c-1) < ASize)) then
    begin
      inc(i, c);
      inc(AMem);
      // if it is a multi-byte character, check the trail bytes
      while (c>1) do
      begin
        if (not UTF8IsTrailChar(AMem^)) then
        begin
          Result := False;
          break;
        end
        else
        begin
          dec(c);
          inc(AMem);
        end;
      end;
    end
    else
    begin
      Result:=False;
    end;
    if (not Result) then break;
  end;
end;

procedure StrLCpy(p, p2: PWideChar; MaxLen: integer);
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

procedure StrLCpyA(p, p2: PAnsiChar; MaxLen: integer);
begin
  if lstrlenA(p2)>=MaxLen
    then
      begin
      lstrcpynA(p, p2, MaxLen);
      p[MaxLen-1]:= #0;
      end
    else
      lstrcpyA(p, p2);
end;

function Format(const fmt: string; params: array of const): string;
var Buffer: array[ 0..1023 ] of WideChar;
    ElsArray, El, P: PNativeUint;
    I : Integer;
    //P : PDWORD;
begin
  ElsArray := nil;
  if High( params ) >= 0 then
    GetMem( ElsArray, (High( params ) + 1) * sizeof( Pointer ) );
  El := ElsArray;
  for I := 0 to High( params ) do
  begin
    P := @params[ I ];
    P := Pointer( P^ );
    El^ := NativeUint( P );
    Inc( El );
  end;
  wvsprintfW( @Buffer[0], PWideChar( fmt ), PWideChar( ElsArray ) );
  Result := Buffer;
  if ElsArray <> nil then
     FreeMem( ElsArray );
end;

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

end.
