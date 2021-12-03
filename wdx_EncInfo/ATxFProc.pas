unit ATxFProc;

interface

uses
  Windows;

var
  opTextBufferSizeKb: integer = 1; //Kb
  opUtf8BufferSizeKb: integer = 64; //Kb
  opBinaryIgnore: string = '';
  //
  opOemEnabled: integer = 1;
  opOemBufferSizeKb: integer = 1; //Kb
  opOemPercent: integer = 18;
  opOemIgnore: string = 'AB BB';
  //
  opRusEnabled: integer = 1;
  opRusBufferSizeKb: integer = 2; //Kb
  opRusMinSize: integer = 8; //bytes
  opRusPercent: integer = 30;
  opRusWordLen: integer = 0;

function IsFileExist(const FileName: WideString; var IsDir: Boolean): Boolean; overload;
function IsFileExist(const FileName: WideString): Boolean; overload;
function IsFileOrDirExist(const FileName: WideString): Boolean;
function IsDirExist(const DirName: WideString): Boolean;
function IsFileAccessed(const FileName: WideString): Boolean;
function IsFileWritable(const FileName: WideString): boolean;

function FFileOpen(const FileName: WideString): THandle;
function FFileCopy(const OldName, NewName: WideString): Boolean;
function FFileMove(const OldName, NewName: WideString): Boolean;
function FGetFileSize(const FileName: WideString): Int64; overload;
function FGetFileSize(Handle: THandle): Int64; overload;
function FGetFileInfo(const FileName: WideString; var Size: Int64; var Time: TFileTime): Boolean;
function FGetShortName(const FileName: WideString): WideString;
function FGetFullPathName(const FileName: WideString): WideString;

type
  PInt64Rec = ^TInt64Rec;
  TInt64Rec = packed record
    Lo, Hi: DWORD;
  end;

function FCreateDir(const FileName: WideString): Boolean;

function IsFileXmlUTF8(const fn: WideString): boolean;
function IsFileUTF8NoBOM(const fn: WideString): boolean;
function IsFileUnicode_(h: THandle; var BE: boolean): Boolean;
function IsFileUTF8(h: THandle): Boolean;
function IsFileRTF(h: THandle): Boolean;
function IsFileWeb(h: THandle): Boolean;
procedure IsFileRTFAndUTF8(const AFileName: WideString; var IsRTF, IsUTF8: Boolean);
function IsFileText(h: THandle; BufSizeKb: DWORD; DetectOEM: Boolean; var IsOEM: Boolean): Boolean;
function IsFileTextSlow(const fn: WideString): boolean;
function IsFileTextFast(const fn: WideString): boolean;
function IsFileUnicodeNoBOM(const fn: Widestring; var IsBE: boolean): boolean; overload;
function IsFileUnicodeWithBOM(const fn: Widestring; var BE: boolean): boolean;
function IsFileOEM(const fn: Widestring): boolean;
function IsFileRus(const fn: Widestring; var IsOem: boolean): boolean; overload;

//Convertion of Unicode filename to ANSI one:
//1. Trying to simply convert Unicode string_ to ANSI
//2. If not successfull, trying to get short name, it's always ANSI
//3. If not successfull, function returns empty string_ (fails)
function FFileNameWideToAnsi(const FileName: WideString): AnsiString;

function FFindFirstFile(const DirName, Mask: WideString): WideString;


implementation

uses
  SysUtils,
  ATxUTF8Detect;

function IsFileExist(const FileName: WideString; var IsDir: Boolean): Boolean; overload;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
begin
  IsDir := False;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    h := FindFirstFileW(PWideChar(FileName), fdW);
    Result := h <> INVALID_HANDLE_VALUE;
    if Result then
    begin
      IsDir := (fdW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
      Windows.FindClose(h);
    end;
  end
  else
  begin
    h := FindFirstFileA(PAnsiChar(AnsiString(FileName)), fdA);
    Result := h <> INVALID_HANDLE_VALUE;
    if Result then
    begin
      IsDir := (fdA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
      Windows.FindClose(h);
    end;
  end;
end;

function IsFileExist(const FileName: WideString): Boolean; overload;
var
  IsDir: Boolean;
begin
  Result := IsFileExist(FileName, IsDir) and (not IsDir);
end;

function IsFileOrDirExist(const FileName: WideString): Boolean;
var
  IsDir: Boolean;
begin
  Result := IsFileExist(FileName, IsDir);
end;

function SDelLastSlashW(const S: WideString): WideString;
begin
  Result := S;
  if (Result <> '') and (Result[Length(Result)] = '\') then
    SetLength(Result, Length(Result) - 1);
end;

function IsDirRoot(const s: Widestring): boolean;
begin
  Result :=
    ((Length(s) = 3) and (Copy(s, 2, 2) = ':\')) or
    ((Length(s) = 2) and (s[2] = ':'));
end;

function IsDirExist(const DirName: WideString): Boolean; overload;
var
  IsDir: Boolean;
begin
  Result :=
    (IsFileExist(SDelLastSlashW(DirName), IsDir) and IsDir) or
    (IsDirRoot(DirName));
end;

function FFileOpen(const FileName: WideString): THandle;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := CreateFileW(PWideChar(FileName),
              GENERIC_READ,
              FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
              nil, OPEN_EXISTING, 0, 0)
  else
    Result := CreateFileA(PAnsiChar(AnsiString(FileName)),
              GENERIC_READ,
              FILE_SHARE_READ or FILE_SHARE_WRITE, //FILE_SHARE_DELETE not supported under Win9x
              nil, OPEN_EXISTING, 0, 0);
end;

function FFileCopy(const OldName, NewName: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := CopyFileW(PWideChar(OldName), PWideChar(NewName), False)
  else
    Result := CopyFileA(PAnsiChar(AnsiString(OldName)), PAnsiChar(AnsiString(NewName)), False);
end;

function FFileMove(const OldName, NewName: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result:= MoveFileW(PWideChar(OldName), PWideChar(NewName))
  else
    Result:= MoveFileA(PAnsiChar(AnsiString(OldName)), PAnsiChar(AnsiString(NewName)));
end;


function IsFileAccessed(const FileName: WideString): Boolean;
var
  h: THandle;
begin
  h := FFileOpen(FileName);
  Result := h <> INVALID_HANDLE_VALUE;
  if Result then CloseHandle(h);
end;

function FGetFileSize(const FileName: WideString): Int64; overload;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  SizeRec: TInt64Rec absolute Result;
begin
  Result := -1;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    h := FindFirstFileW(PWideChar(FileName), fdW);
    if h <> INVALID_HANDLE_VALUE then
    begin
      SizeRec.Hi := fdW.nFileSizeHigh;
      SizeRec.Lo := fdW.nFileSizeLow;
      Windows.FindClose(h);
    end;
  end
  else
  begin
    h := FindFirstFileA(PAnsiChar(AnsiString(FileName)), fdA);
    if h <> INVALID_HANDLE_VALUE then
    begin
      SizeRec.Hi := fdA.nFileSizeHigh;
      SizeRec.Lo := fdA.nFileSizeLow;
      Windows.FindClose(h);
    end;
  end;
end;

function FGetFileSize(Handle: THandle): Int64; overload;
var
  Size: Int64;
  SizeRec: TInt64Rec absolute Size;
begin
  SizeRec.Lo := GetFileSize(Handle, @SizeRec.Hi);
  if (SizeRec.Lo = $FFFFFFFF) and (GetLastError <> NO_ERROR) then
    Result := -1
  else
    Result := Size;
end;

function FGetFileInfo(const FileName: WideString; var Size: Int64; var Time: TFileTime): Boolean;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  SizeRec: TInt64Rec absolute Size;
begin
  Result := False;

  Size := 0;
  FillChar(Time, SizeOf(Time), 0);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
    h := FindFirstFileW(PWideChar(FileName), fdW);
    if h <> INVALID_HANDLE_VALUE then
      begin
      Result := True;
      //Attr := fdW.dwFileAttributes;
      SizeRec.Hi := fdW.nFileSizeHigh;
      SizeRec.Lo := fdW.nFileSizeLow;
      Time := fdW.ftLastWriteTime;
      Windows.FindClose(h);
      end;
    end
  else
    begin
    h := FindFirstFileA(PAnsiChar(AnsiString(FileName)), fdA);
    if h <> INVALID_HANDLE_VALUE then
      begin
      Result := True;
      //Attr := fdA.dwFileAttributes;
      SizeRec.Hi := fdA.nFileSizeHigh;
      SizeRec.Lo := fdA.nFileSizeLow;
      Time := fdA.ftLastWriteTime;
      Windows.FindClose(h);
      end;
    end;
end;


function FGetShortName(const FileName: WideString): WideString;
var
  bufA: array[0..MAX_PATH - 1] of AnsiChar;
  bufW: array[0..MAX_PATH - 1] of WideChar;
  resA: AnsiString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    FillChar(bufW, SizeOf(bufW), 0);
    SetString(Result, bufW, GetShortPathNameW(PWideChar(FileName), bufW, SizeOf(bufW) div 2));
  end
  else
  begin
    FillChar(bufA, SizeOf(bufA), 0);
    SetString(resA, bufA, GetShortPathNameA(PAnsiChar(AnsiString(FileName)), bufA, SizeOf(bufA)));
    Result := resA;
  end;
end;


function FCreateDir(const FileName: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := CreateDirectoryW(PWideChar(FileName), nil)
  else
    Result := CreateDirectoryA(PAnsiChar(AnsiString(FileName)), nil);
end;


function IsFileUnicode_(h: THandle; var BE: boolean): Boolean;
var
  Buffer: Word;
  BytesRead: DWORD;
begin
  Buffer := 0;
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  Result :=
    ReadFile(h, Buffer, SizeOf(Buffer), BytesRead, nil) and
    (BytesRead >= SizeOf(Buffer)) and
    ((Buffer = $FEFF) or (Buffer = $FFFE));
  BE:= (Buffer = $FFFE);
end;

function IsFileUnicodeNoBOM(h: THandle; var IsBE: boolean): Boolean; overload;
var
  Buffer: packed array[0..1, 0..1] of byte;
  BytesRead: DWORD;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  Result :=
    ReadFile(h, Buffer, SizeOf(Buffer), BytesRead, nil) and
    (BytesRead >= SizeOf(Buffer)) and
    ( ((Buffer[0, 0] = 0) and (Buffer[1, 0] = 0)) or
      ((Buffer[0, 1] = 0) and (Buffer[1, 1] = 0)) );
  if Result then
    IsBE := ((Buffer[0, 0] = 0) and (Buffer[1, 0] = 0));
end;

function IsFileUTF8(h: THandle): Boolean;
var
  Buffer: packed array[0..2] of byte;
  BytesRead: DWORD;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  Result :=
    ReadFile(h, Buffer, SizeOf(Buffer), BytesRead, nil) and
    (BytesRead >= 3) and
    ((Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF));
end;

function IsFileUTF8NoBOM_(h: THandle): Boolean;
const
  cMax = 10*1024; //max buffer 10Mb
var
  Buffer: PAnsiChar;
  BufSize, BytesRead: DWORD;
begin
  Result := False;
  if opUtf8BufferSizeKb < 1 then opUtf8BufferSizeKb := 1;
  if opUtf8BufferSizeKb > cMax then opUtf8BufferSizeKb := cMax;
  BufSize := opUtf8BufferSizeKb * 1024;
  GetMem(Buffer, BufSize);
  try
    FillChar(Buffer^, BufSize, 0);
    SetFilePointer(h, 0, nil, FILE_BEGIN);
    if not ReadFile(h, Buffer^, Pred(BufSize), BytesRead, nil) then Exit;
    Result := IsBufferUtf8(Buffer, True{PartialAllowed});
  finally
    FreeMem(Buffer);
  end;
end;


function IsFileRTF(h: THandle): Boolean;
const
  Sign = '{\rtf';
  SignLen = Length(Sign);
var
  Buffer: packed array[0 .. SignLen] of AnsiChar; //Sign + #0
  BytesRead: DWORD;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  {$WARNINGS OFF}
  Result :=
    ReadFile(h, Buffer, SignLen, BytesRead, nil) and
    (BytesRead >= SignLen) and
    (Buffer = Sign);
  {$WARNINGS ON}
end;

function IsFileWeb(h: THandle): Boolean;
const
  Sign = '<?xml';
  SignLen = Length(Sign);
var
  Buffer: packed array[0 .. SignLen] of AnsiChar; //Sign + #0
  BytesRead: DWORD;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  {$WARNINGS OFF}
  Result :=
    ReadFile(h, Buffer, SignLen, BytesRead, nil) and
    (BytesRead >= SignLen) and
    (Buffer = Sign);
  {$WARNINGS ON}
end;


procedure IsFileRTFAndUTF8(const AFileName: WideString; var IsRTF, IsUTF8: Boolean);
var
  h: THandle;
begin
  IsRTF := False;
  IsUTF8 := False;

  h := FFileOpen(AFileName);
  if h <> INVALID_HANDLE_VALUE then
    try
      IsRTF := IsFileRTF(h);
      IsUTF8 := IsFileUTF8(h);
    finally
      CloseHandle(h);
    end;
end;


function IsIgnoredCode(n: Integer; const Str: string): boolean;
begin
  Result:= Pos(' '+IntToHex(n, 2)+' ', ' '+Str+' ') > 0;
end;

function IsBinChar(n: Integer): boolean;
begin
  Result:= (n < 32) and (n <> 09) and (n <> 13) and (n <> 10);
  //handle exclude chars
  if Result then
    if IsIgnoredCode(n, opBinaryIgnore) then
      Result:= false;
end;

type
  TFreqTable = array[$80 .. $FF] of Integer;

function IsFileText(h: THandle; BufSizeKb: DWORD; DetectOEM: Boolean; var IsOEM: Boolean): Boolean;
var
  Buffer: PAnsiChar;
  BufSize, BytesRead, i: DWORD;
  n: Integer;
  Table: TFreqTable;
  TableSize: Integer;
begin
  Result := False;
  IsOEM := False;

  if BufSizeKb = 0 then Exit;
  Buffer := nil;
  BufSize := BufSizeKb * 1024;

  //Init freq table
  TableSize := 0;
  FillChar(Table, SizeOf(Table), 0);

  try
    GetMem(Buffer, BufSize);
    FillChar(Buffer^, BufSize, 0);
    SetFilePointer(h, 0, nil, FILE_BEGIN);

    if ReadFile(h, Buffer^, BufSize, BytesRead, nil) then
      if BytesRead > 0 then
      begin
        Result := True;
        for i := 0 to BytesRead - 1 do
        begin
          n := Ord(Buffer[i]);

          //If control chars present, then non-text
          if IsBinChar(n) then
            begin Result := False; Break end;

          //Calculate freq table
          if DetectOEM then
            if (n >= Low(Table)) and (n <= High(Table)) then
            begin
              Inc(TableSize);
              Inc(Table[n]);
            end;
        end;
      end;

    //Analize table
    if DetectOEM then
      if Result and (TableSize > 0) then
        for i := Low(Table) to High(Table) do
          if not IsIgnoredCode(i, opOemIgnore) then
          begin
            Table[i] := Table[i] * 100 div TableSize;
            if ((i >= $B0) and (i <= $DF))
              or (i = $FF) {or (i = $A9)} then
              if Table[i] >= opOemPercent then
                begin IsOEM := True; Break end;
          end;

  finally
    if Assigned(Buffer) then
      FreeMem(Buffer);
  end;
end;


procedure SAddSlash(var S: AnsiString);
begin
  if (S <> '') and (S[Length(S)] <> '\') then
    S := S + '\';
end;

function FFileNameWideToAnsi(const FileName: WideString): AnsiString;
begin
  if IsDirExist(FileName) then
  begin
    Result := FileName;
    //Convert to short form only "pure Unicode" names:
    if FileName <> WideString(AnsiString(FileName)) then
    begin
      Result := FGetShortName(FileName);
      if not IsDirExist(Result) then
        Result := '';
    end;
    //Add trailing slash, Lister plugins expect it:
    SAddSlash(Result);
  end
  else
  begin
    Result := FileName;
    //Convert to short form only "pure Unicode" names:
    if FileName <> WideString(AnsiString(FileName)) then
    begin
      Result := FGetShortName(FileName);
      if not IsFileAccessed(Result) then
        Result := '';
    end;
  end;
end;


function FFindFirstFile(const DirName, Mask: WideString): WideString;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  IsDir: Boolean;
begin
  Result := '';
  h := INVALID_HANDLE_VALUE;
  try
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      h := FindFirstFileW(PWideChar(DirName + '\' + Mask), fdW);
      if h <> INVALID_HANDLE_VALUE then
        repeat
          IsDir := (fdW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
          if not IsDir then
          begin
            Result := DirName + '\' + fdW.cFileName;
            Exit
          end;
        until not FindNextFileW(h, fdW);
    end
    else
    begin
      h := FindFirstFileA(PAnsiChar(AnsiString(DirName+'\'+Mask)), fdA);
      if h <> INVALID_HANDLE_VALUE then
        repeat
          IsDir := (fdA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
          if not IsDir then
          begin
            Result := DirName + '\' + fdA.cFileName;
            Exit
          end;
        until not FindNextFileA(h, fdA);
    end;
  finally
    Windows.FindClose(h);
  end;
end;


function FGetFullPathName(const FileName: WideString): WideString;
var
  bufA: array[0 .. MAX_PATH - 1] of AnsiChar;
  bufW: array[0 .. MAX_PATH - 1] of WideChar;
  partA: PAnsiChar;
  partW: PWideChar;
begin
  Result := '';
  if FileName <> '' then //Result for empty string_ should be empty string_!
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      if GetFullPathNameW(PWideChar(FileName), SizeOf(bufW) div 2, bufW, partW) <> 0 then
        Result := bufW;
    end
    else
    begin
      if GetFullPathNameA(PAnsiChar(AnsiString(FileName)), SizeOf(bufA), bufA, partA) <> 0 then
        Result := AnsiString(bufA);
    end;
end;


function IsFileWritable(const FileName: WideString): boolean;
var h: THandle;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    h := CreateFileW(PWideChar(FileName),
              GENERIC_WRITE,
              FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
              nil, OPEN_EXISTING, 0, 0)
  else
    h := CreateFileA(PAnsiChar(AnsiString(FileName)),
              GENERIC_WRITE,
              FILE_SHARE_READ or FILE_SHARE_WRITE, //FILE_SHARE_DELETE not supported under Win9x
              nil, OPEN_EXISTING, 0, 0);
  Result := h <> invalid_handle_value;
  if Result then CloseHandle(h);
end;


//----
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

//-----------------------------------------
// <?xml version="1.0" encoding="UTF-8" ?>
function SEncodedUTF8(s: string): boolean;
var
  tagStart: string;
begin
  Result:= false;
  tagStart:= '<?xml ';
  if Pos(tagStart, s)>0 then
    begin
    s:= SDeleteTo(s, tagStart);
    s:= SDeleteFrom(s, '?>');
    if Pos('encoding=', s)=0 then Exit;
    s:= SDeleteTo(s, 'encoding=');
    s:= SBetween(s, '"', '"');
    Result:= UpperCase(s) = 'UTF-8';
    end;
end;

function IsFileXmlUTF8(const fn: WideString): boolean;
var
  h: THandle;
  Buffer: packed array[0..100] of AnsiChar;
  BytesRead: DWORD;
begin
  Result := False;
  h := FFileOpen(fn);
  if h <> INVALID_HANDLE_VALUE then
  try
    if IsFileUTF8(h) then Exit; //BOM at start
    FillChar(Buffer, SizeOf(Buffer), 0);
    SetFilePointer(h, 0, nil, FILE_BEGIN);
    Result :=
      ReadFile(h, Buffer, SizeOf(Buffer)-1{!}, BytesRead, nil) and
      SEncodedUTF8(string(Buffer));
  finally
    CloseHandle(h);
  end;
end;

function IsFileUTF8NoBOM(const fn: WideString): boolean;
var
  h: THandle;
begin
  Result := False;
  h := FFileOpen(fn);
  if h <> INVALID_HANDLE_VALUE then
  try
    Result :=
      not IsFileUTF8(h) and
      IsFileUTF8NoBOM_(h);
  finally
    CloseHandle(h);
  end;
end;

function IsFileTextSlow(const fn: WideString): boolean; overload;
var
  h: THandle;
  b, ext_txt: Boolean;
begin
  Result := False;
  h := FFileOpen(fn);
  ext_txt := LowerCase(ExtractFileExt(fn)) = '.txt';
  if h <> INVALID_HANDLE_VALUE then
    try
      Result :=
        (FGetFileSize(h) = 0) or
        IsFileUTF8(h) or
        IsFileUnicode_(h, b) or
        (IsFileUnicodeNoBOM(h, b) and ext_txt) or
        (IsFileText(h, opOemBufferSizeKb, False, b) and not IsFileRTF(h));
    finally
      CloseHandle(h);
    end;
end;

function IsFileTextFast(const fn: WideString): boolean; overload;
var
  h: THandle;
  b, ext_txt: Boolean;
begin
  Result := False;
  h := FFileOpen(fn);
  //ext_txt := LowerCase(ExtractFileExt(fn)) = '.txt';
  if h <> INVALID_HANDLE_VALUE then
    try
      Result :=
        (FGetFileSize(h) = 0) or
        //(IsFileUnicodeNoBOM(h, b) and ext_txt) or
        (IsFileText(h, opTextBufferSizeKb, False{DetectOEM}, b));
    finally
      CloseHandle(h);
    end;
end;


function IsFileUnicodeNoBOM(const fn: Widestring; var IsBE: boolean): boolean; overload;
var
  h: THandle;
begin
  Result := False;
  h := FFileOpen(fn);
  if h <> INVALID_HANDLE_VALUE then
    try
      Result := IsFileUnicodeNoBOM(h, IsBE);
    finally
      CloseHandle(h);
    end;
end;

function IsFileUnicodeWithBOM(const fn: Widestring; var BE: boolean): boolean;
var
  h: THandle;
begin
  Result := False;
  h := FFileOpen(fn);
  if h <> INVALID_HANDLE_VALUE then
    try
      Result := IsFileUnicode_(h, BE);
    finally
      CloseHandle(h);
    end;
end;


function FReadString(const fn: string): string;
var
  f: TextFile;
begin
  Result:= '';
  AssignFile(f, fn);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult<>0 then Exit;
  Readln(f, Result);
  CloseFile(f);
end;


//-------------------------------------------------
function IsFileOEM(const fn: Widestring): boolean;
var
  h: THandle;
  oem: boolean;
begin
  Result := False;
  h := FFileOpen(fn);
  if h <> INVALID_HANDLE_VALUE then
    try
      Result := IsFileText(h, opOemBufferSizeKb, true{detectOEM}, oem) and oem;
    finally
      CloseHandle(h);
    end;
end;


function IsRusChar(ch: AnsiChar; OemMode: boolean): boolean;
begin
  if not OemMode then
    Result:= (ch >= #$C0) and (ch <= #$FF)
  else
    Result:= ((ch >= #$80) and (ch <= #$AF)) or ((ch >= #$E0) and (ch <= #$F1));
end;

function IsBufferRus(Buf: PAnsiChar; BytesRead: Integer; OemMode: boolean): boolean;
var
  i: Integer;
  Cnt, WordLen: Integer;
begin
  Cnt:= 0;
  WordLen:= 0;

  for i:= 0 to Pred(BytesRead) do
    if IsRusChar(Buf[i], OemMode) then
    begin
      Inc(Cnt);
      Inc(WordLen);
      if (opRusWordLen > 0) and (WordLen >= opRusWordLen) then
        begin Result:= true; Exit end;
    end
    else
    begin
      WordLen:= 0;
    end;

  Result:= Cnt * 100 div BytesRead >= opRusPercent;
end;

function IsFileRus(h: THandle; BufSize: Integer; var IsOem: boolean): Boolean; overload;
const
  cMax = 2*1024; //max buffer size
var
  Buffer: PAnsiChar;
  BytesRead: DWORD;
begin
  Result := False;
  IsOem := false;

  if BufSize < 1 then BufSize := 1;
  if BufSize > cMax then BufSize := cMax;
  BufSize := BufSize * 1024;
  GetMem(Buffer, BufSize);
  try
    FillChar(Buffer^, BufSize, 0);
    SetFilePointer(h, 0, nil, FILE_BEGIN);
    if not ReadFile(h, Buffer^, Pred(BufSize), BytesRead, nil) then Exit;

    if (BytesRead >= opRusMinSize) then
    begin
      if IsBufferRus(Buffer, BytesRead, false{OemMode}) then
        Result := true
      else
      if IsBufferRus(Buffer, BytesRead, true{OemMode}) then
        begin Result := true; IsOem := true; end;
    end;
  finally
    FreeMem(Buffer);
  end;
end;


function IsFileRus(const fn: Widestring; var IsOem: boolean): boolean; overload;
var
  h: THandle;
begin
  Result := False;
  h := FFileOpen(fn);
  if h <> INVALID_HANDLE_VALUE then
    try
      Result := IsFileRus(h, opRusBufferSizeKb, IsOem);
    finally
      CloseHandle(h);
    end;
end;


end.
