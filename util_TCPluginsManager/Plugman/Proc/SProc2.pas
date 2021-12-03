unit SProc2;

interface

function SOsName: string;
function SOsVersion: string;

function GetFileDiz(const fn, fn_diz: string): string;
procedure SetFileDiz(const fn, fn_diz, desc: string);


implementation

uses Windows, SysUtils, RegProc, SProc;

//------------------------------------------------------
function SOsName: string;
var
  vi: TOsVersionInfo;
begin
  vi.dwOSVersionInfoSize:= SizeOf(vi);
  GetVersionEx(vi);
  if vi.dwPlatformId=VER_PLATFORM_WIN32s then Result:= 'Win32s' else
   if vi.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS then 
     Result:= GetRegKeyStr(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion',
      'ProductName', 'Windows 9x') else
    //if vi.dwPlatformId=VER_PLATFORM_WIN32_NT then
     Result:= GetRegKeyStr(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows NT\CurrentVersion',
      'ProductName', 'Windows NT');
  SReplace(Result, 'Microsoft ', '');
end;


function SOsVersion: string;
var
  vi: TOsVersionInfo;
begin
  vi.dwOSVersionInfoSize:= SizeOf(vi);
  GetVersionEx(vi);
  Result:= Format('%d.%02.2d.%d %s', 
    [vi.dwMajorVersion, vi.dwMinorVersion,
     LoWord(vi.dwBuildNumber), vi.szCSDVersion]);
  SReplace(Result, '  ', ' ');
end;


//------------------------------------------------------
//fn_diz='Descript.ion'or 'Files.bbs'
function GetFileDiz(const fn, fn_diz: string): string;
var
  f: Text;
  fn0, s, ss: string;
  i: integer;
begin
  Result:= '';
  {$I-}
  AssignFile(f, ExtractFilePath(fn)+fn_diz);
  Reset(f);
  if IOResult<>0 then Exit;

  fn0:= ExtractFileName(fn);
  while not Eof(f) do
    begin
    Readln(f, s);
    i:= Pos(' ', s); if i=0 then Continue;
    ss:= Copy(s, 1, i-1);
    STrimLeft(ss, ['"']);
    STrimRight(ss, ['"']);
    if StrIComp(PChar(fn0), PChar(ss))=0 then
      begin Result:= Copy(s, i+1, MaxInt); STrimLeft(Result, [' ']); Break end;
    end;
  CloseFile(f);
end;

//------------------------------------------------------
procedure SetFileDiz(const fn, fn_diz, desc: string);
var
  f1, f2: Text;
  fn1, fn2, fn0, s, s_new, ss: string;
  found: boolean;
  i: integer;
begin
  {$I-}
  fn1:= ExtractFilePath(fn)+fn_diz;
  fn2:= fn1+'.tmp';
  fn0:= ExtractFileName(fn);
  found:= false;
  if (desc='') or (desc='-')
    then s_new:= ''
    else s_new:= Format('"%s" %s', [fn0, desc]);

  AssignFile(f2, fn2);
  Rewrite(f2);
  if IOResult<>0 then Exit;

  AssignFile(f1, fn1);
  Reset(f1);
  if IOResult=0 then
    begin
    while not Eof(f1) do
      begin
      Readln(f1, s);
      i:= Pos(' ', s); if i=0 then Continue;
      ss:= Copy(s, 1, i-1);
      STrimLeft(ss, ['"']);
      STrimRight(ss, ['"']);
      if StrIComp(PChar(fn0), PChar(ss))=0 then
        begin found:= true; s:= s_new end;
      Writeln(f2, s);
      end;
    CloseFile(f1);
    end;

  if not found then Writeln(f2, s_new);
  CloseFile(f2);
  DeleteFile(PChar(fn1));
  MoveFile(PChar(fn2), PChar(fn1));

  //чтобы не путался под ногами
  SetFileAttributes(PChar(fn1), FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_HIDDEN);
end;



end.
