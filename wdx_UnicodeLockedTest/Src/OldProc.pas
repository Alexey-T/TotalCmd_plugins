//-----------------------------------------------------
function IsStringUnicode(const s: WideString): boolean;
var
  i: integer;
  ch: WideChar;
  bytes: array[0..1] of char absolute ch;
begin
  Result:= false;
  for i:= 1 to Length(s) do
    begin
    ch:= s[i];
    if bytes[1]<>#0 then begin Result:= true; Break end;
    end;
end;

//-----------------------------------------------------
{
function IsNameUnicode(const fn: AnsiString): TUnicodeTest;
var
  h, hfA, hfW: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  fnA, fnS, dirA: AnsiString;
  fnW, dirW: WideString;
  AnsiRead, UnicodeRead: boolean;
  IsDir, IsMatch: boolean;
begin
  //Result:= sInvalid;
  AnsiRead:= false;
  UnicodeRead:= false;
  IsDir:= false;

  //Test for ANSI reading
  dirA:= ExtractFileDir(fn);
  fnA:= ExtractFileName(fn);
  fnS:= '';
  h:= FindFirstFileA(PChar(dirA+'\*.*'), fdA);
  if h<>INVALID_HANDLE_VALUE then
  repeat
    if fdA.cFileName='.' then Continue;
    if StrIComp(fdA.cFileName, PChar(fnA))<>0 then Continue;
    fnS:= fdA.cAlternateFileName;
    IsDir:= (fdA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0;

    if IsDir then
      begin
      AnsiRead:= SetCurrentDirectoryA(PChar(fn));
      end
    else
      begin
      hfA:= CreateFileA(PChar(fn), GENERIC_READ, FILE_SHARE_READ,
        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      AnsiRead:= hfA<>INVALID_HANDLE_VALUE;
      if hfA<>INVALID_HANDLE_VALUE then CloseHandle(hfA);
      end;
    Break
  until not FindNextFileA(h, fdA);
  Windows.FindClose(h);

  //Write('fnS: ', fnS:12, ' ');

  //Test for Unicode reading
  dirW:= WideString(dirA);
  h:= FindFirstFileW(PWChar(dirW+'\*.*'), fdW);
  if h<>INVALID_HANDLE_VALUE then
  repeat
    if AnsiString(fdW.cFileName)='.' then Continue;
    IsMatch:= ( (fnS<>'') and (fnS=AnsiString(fdW.cAlternateFileName)) ) or
              ( (fnS='') and (fnA=AnsiString(fdW.cFileName)) );
    if not IsMatch then Continue;

    fnW:= dirW+'\'+fdW.cFileName;
    if IsDir then
      begin
      UnicodeRead:= SetCurrentDirectoryW(PWChar(fnW));
      end
    else
      begin
      hfW:= CreateFileW(PWChar(fnW), GENERIC_READ, FILE_SHARE_READ,
        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      UnicodeRead:= hfW<>INVALID_HANDLE_VALUE;
      if hfW<>INVALID_HANDLE_VALUE then CloseHandle(hfW);
      end;
    Break
  until not FindNextFileW(h, fdW);
  Windows.FindClose(h);

  if AnsiRead then Result:= sANSI else
   if UnicodeRead then Result:= sUnicode else
    Result:= sLocked;
end;
}

