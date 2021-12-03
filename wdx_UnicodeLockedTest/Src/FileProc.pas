unit FileProc;

interface

uses
  Windows, SysUtils;

type
  TUnicodeTest = 
    (sInvalid, sAnsi, sUnicode);
  TAccessTest =
    (accDir, accNotFound, accEnabled, accDenied);

const
  SUnicodeTest: array[TUnicodeTest] of string =
    ('Invalid', 'ANSI', 'Unicode');
  SAccessTest: array[TAccessTest] of string =
    ('', 'Not found', 'Access', 'Locked');

function FUnicodeTest(const fn: string): TUnicodeTest;
function FReadTest(const fn: string): TAccessTest;
function FWriteTest(const fn: string): TAccessTest;

implementation

//-----------------------------------------------------
procedure FExistTest(const fn: string; var fExist, fDir, fRO: boolean);
var
  h: THandle;
  fd: TWin32FindData;
begin
  fExist:= false;
  fDir:= false;
  fRO:= false;
  if fn='' then Exit;
  h:= FindFirstFile(PChar(fn), fd);
  if h=INVALID_HANDLE_VALUE then Exit;
  fExist:= true;
  fDir:= (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0;
  fRO:= (fd.dwFileAttributes and FILE_ATTRIBUTE_READONLY)<>0;
  Windows.FindClose(h);
end;

//-----------------------------------------------------
function FReadTest(const fn: string): TAccessTest;
var
  h: THandle;
  fExist, fDir, fRO: boolean;
begin
  FExistTest(fn, fExist, fDir, fRO);
  if not fExist then begin Result:= accNotFound; Exit end;
  if fDir then begin Result:= accDir; Exit end;

  h:= CreateFile(PChar(fn),
    GENERIC_READ,
    0{FILE_SHARE_READ},
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if h<>INVALID_HANDLE_VALUE then
    Result:= accEnabled
  else
    Result:= accDenied;
  if h<>INVALID_HANDLE_VALUE then
    CloseHandle(h);
end;

//-----------------------------------------------------
function FWriteTest(const fn: string): TAccessTest;
var
  h: THandle;
  fExist, fDir, fRO: boolean;
begin
  FExistTest(fn, fExist, fDir, fRO);
  if not fExist then begin Result:= accNotFound; Exit end;
  if fDir then begin Result:= accDir; Exit end;

  if fRO then
    FileSetReadOnly(fn, false);

  h:= CreateFile(PChar(fn),
    GENERIC_READ or GENERIC_WRITE,
    0{FILE_SHARE_READ},
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if h<>INVALID_HANDLE_VALUE then
    Result:= accEnabled
  else
    Result:= accDenied;
  if h<>INVALID_HANDLE_VALUE then
    CloseHandle(h);

  if fRO then
    FileSetReadOnly(fn, true);
end;

//-----------------------------------------------------
function IsNameLong(const fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
  fnA, dir: string;
begin
  Result:= false;
  dir:= ExtractFileDir(fn);
  fnA:= ExtractFileName(fn);
  h:= FindFirstFile(PChar(dir+'\*.*'), fd);
  if h<>INVALID_HANDLE_VALUE then
  repeat
    if string(fd.cFileName)='.' then Continue;
    if StrIComp(fd.cFileName, PChar(fnA))=0 then
      begin Result:= true; Break end;
  until not FindNextFile(h, fd);
  Windows.FindClose(h);
end;

//-----------------------------------------------------
function IsNameShort(const fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
  fnA, dir: string;
begin
  Result:= false;
  dir:= ExtractFileDir(fn);
  fnA:= ExtractFileName(fn);
  h:= FindFirstFile(PChar(dir+'\*.*'), fd);
  if h<>INVALID_HANDLE_VALUE then
  repeat
    if string(fd.cFileName)='.' then Continue;
    if (StrIComp(fd.cFileName, fd.cAlternateFileName)<>0) and
       (StrIComp(fd.cAlternateFileName, PChar(fnA))=0) then
      begin Result:= true; Break end;
  until not FindNextFile(h, fd);
  Windows.FindClose(h);
end;

//-----------------------------------------------------
function IsNameUnicode(const fn: string): boolean;
var
  h: THandle;
  fdW: TWin32FindData;
  fnS: string;
  dirW: string;
begin
  Result:= false;
  fnS:= ExtractFileName(fn);
  dirW:= ExtractFileDir(fn);
  h:= FindFirstFile(PWChar(dirW+'\*.*'), fdW);
  if h<>INVALID_HANDLE_VALUE then
  repeat
    if WideString(fdW.cFileName)=WideChar('.') then Continue;
    if UpperCase(fnS) = UpperCase(fdW.cAlternateFileName) then
      begin Result:= true; Break end;
  until not FindNextFile(h, fdW);
  Windows.FindClose(h);
end;

//-----------------------------------------------------
function FUnicodeTest(const fn: string): TUnicodeTest;
var
  fEx, fDir, fRO: boolean;
begin
  FExistTest(fn, fEx, fDir, fRO);
  if fEx then
  begin
    if fn = string(AnsiString(fn)) then Result:= sAnsi else
     Result:= sUnicode;
  end
  else
    Result:= sInvalid;
end;

end.
