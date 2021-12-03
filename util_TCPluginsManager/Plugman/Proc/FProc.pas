unit FProc;

interface

uses Windows, ShellAPI;

function IsFileUsed(const fn: string): boolean;
function IsFileExist(const fn: string): boolean;
function IsDirExist(const fn: string): boolean;

function FSearchDir(const sname, sdir: string; var fn: string): boolean;
procedure FDeleteDir(const sdir: string);
function FDirsInDir(const sdir: string): integer;
function FShortName(const fn: string): string;
function FCreateDirs(const sdir, snames: string): boolean; overload;
function FCreateDirs(const sdir: string): boolean; overload;
function FFileSize(const fn: string): integer;

procedure FOpenURL(const s: string; hWnd: THandle);
procedure FOpenFolder(hWnd: THandle; const fn: string);
function FExecProcess(const cmd: string; ShowCmd: integer; DoWait: boolean): boolean;
function FExecShell(const cmd, params, dir: string; ShowCmd: integer; DoWait: boolean): boolean;
procedure FShowProperties(const fn: string; hWnd: THandle);

function FTempDirectory: string;
procedure SDelLastSlash(var S: string);
function _MoveFileEx(Src, Dest: PChar; Flags: DWORD): BOOL;


implementation

uses
  SysUtils;

function IsFileUsed(const fn: string): boolean;
var
  h: THandle;
begin
  h:= CreateFile(PChar(fn), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result:= h=INVALID_HANDLE_VALUE;
  if not Result then CloseHandle(h);
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

function IsDirExist(const fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
begin
  if fn='' then begin Result:= false; Exit end;
  h:= FindFirstFile(PChar(fn), fd);
  if h=INVALID_HANDLE_VALUE then begin Result:= false; Exit end;
  Result:= (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0;
  Windows.FindClose(h);
end;

function FFileSize(const fn: string): integer;
var
  h: THandle;
  fd: TWin32FindData;
begin
  Result:= 0;
  h:= FindFirstFile(PChar(fn), fd);
  if h<>INVALID_HANDLE_VALUE then
    begin
    Result:= fd.nFileSizeLow;
    Windows.FindClose(h);
    end;
{
  Result:= 0;
  h:= CreateFile(PChar(fn), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if h<>INVALID_HANDLE_VALUE then
    begin
    Result:= GetFileSize(h, nil);
    CloseHandle(h);
    end;
}
end;


function FSearchDir(const sname, sdir: string; var fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
begin
  if not IsDirExist(sdir) then begin Result:= false; Exit end;

  h:= FindFirstFile(PChar(sdir+'\'+sname), fd);
  Result:= h<>INVALID_HANDLE_VALUE;
  if Result then 
    begin fn:= sdir+'\'+fd.cFileName; Windows.FindClose(h); Exit end;

  h:= FindFirstFile(PChar(sdir+'\*.*'), fd);
  if h=INVALID_HANDLE_VALUE then Exit;
  
  repeat
    if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0) and
      (fd.cFileName[0]<>'.') then
        begin
        Result:= FSearchDir(sname, sdir+'\'+fd.cFileName, fn);
        if Result then Break;
        end;

  until not FindNextFile(h, fd);

  Windows.FindClose(h);
end;


procedure FDeleteDir(const sdir: string);
var
  h: THandle;
  fd: TWin32FindData;
begin
  if not IsDirExist(sdir) then Exit;
  h:= FindFirstFile(PChar(sdir+'\*.*'), fd);
  if h=INVALID_HANDLE_VALUE then Exit;
  
  repeat
    if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0) then
      begin
      //Writeln('Deleting: ', sdir+'\'+fd.cFileName);
      DeleteFile(PChar(sdir+'\'+fd.cFileName));
      end;

    if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0) and
      (fd.cFileName[0]<>'.') then
        FDeleteDir(sdir+'\'+fd.cFileName);

  until not FindNextFile(h, fd);

  Windows.FindClose(h);
  //Writeln('Removing: ', sdir);
  RemoveDirectory(PChar(sdir));
end;


function FDirsInDir(const sdir: string): integer;
var
  h: THandle;
  fd: TWin32FindData;
begin
  Result:= 0;
  h:= FindFirstFile(PChar(sdir+'\*.*'), fd);
  if h=INVALID_HANDLE_VALUE then Exit;
  repeat
    if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0) and
      (fd.cFileName[0]<>'.') then Inc(Result);
  until not FindNextFile(h, fd);
  Windows.FindClose(h);
end;


function FShortName(const fn: string): string;
var
  buf: array[0..MAX_PATH-1] of char;
begin
  SetString(Result, buf, GetShortPathName(PChar(fn), buf, SizeOf(buf)));
end;


procedure FOpenURL(const s: string; hWnd: THandle);
begin
  ShellExecute(hWnd, 'open', PChar(s), nil, nil, SW_SHOW);
end;

procedure FOpenFolder(hWnd: THandle; const fn: string);
begin
  ShellExecute(hWnd, nil, PChar(fn), nil, nil, SW_SHOWNORMAL);
end;


function FExecProcess(const cmd: string; ShowCmd: integer; DoWait: boolean): boolean;
var
  pi: TProcessInformation;
  si: TStartupInfo;
begin
  FillChar(pi, SizeOf(pi), 0);
  FillChar(si, SizeOf(si), 0);
  si.cb:= SizeOf(si);
  si.dwFlags:= STARTF_USESHOWWINDOW;
  si.wShowWindow:= ShowCmd;

  Result:= CreateProcess(nil, PChar(cmd), nil, nil, false, 0,
    nil, nil, si, pi);
  if Result then
    begin
    if DoWait then WaitForSingleObject(pi.hProcess, INFINITE);
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
    end;
end;


function FExecShell(const cmd, params, dir: string; ShowCmd: integer; DoWait: boolean): boolean;
var
  si: TShellExecuteInfo;
begin
  FillChar(si, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  si.lpFile:= PChar(cmd);
  si.lpParameters:= PChar(params);
  si.lpDirectory:= PChar(dir);
  si.nShow:= ShowCmd;
  Result:= ShellExecuteEx(@si);
  if Result and DoWait then
    WaitForSingleObject(si.hProcess, INFINITE);
  CloseHandle(si.hProcess);
end;



function FCreateDirs(const sdir, snames: string): boolean;
var
  s, sdir1, sname1: string;
  i: integer;
begin
  sdir1:= sdir;
  s:= snames;
  repeat
    i:= Pos('\', s);
    if i=0
      then begin sname1:= s; s:= ''; end
      else begin sname1:= Copy(s, 1, i-1); Delete(s, 1, i); end;
    sdir1:= sdir1+'\'+sname1;
    Result:= CreateDirectory(PChar(sdir1), nil);
  until s='';
end;

function FCreateDirs(const sdir: string): boolean;
var
  s, sdir1, sname1: string;
  i: integer;
begin
  i:= Pos(':\', sdir); //'C:\path1\path2\path3'
  if i=0 then begin Result:= false; Exit end;

  sdir1:= Copy(sdir, 1, i); //'C:'
  s:= Copy(sdir, i+2, MaxInt); //'path1\path2\path3'
  repeat
    i:= Pos('\', s);
    if i=0
      then begin sname1:= s; s:= ''; end
      else begin sname1:= Copy(s, 1, i-1); Delete(s, 1, i); end;
    sdir1:= sdir1+'\'+sname1;
    Result:= CreateDirectory(PChar(sdir1), nil);
  until s='';
end;


function FTempDirectory: string;
var
  buf: array[0..MAX_PATH-1] of char;
begin
  FillChar(buf, SizeOf(buf), 0);
  GetTempPath(SizeOf(buf), buf);
  Result:= buf;
  SDelLastSlash(Result);
end;

procedure SDelLastSlash(var S: string);
begin
  if (S<>'') and (S[Length(S)] in ['\', '/']) then
    SetLength(S, Length(S)-1);
end;

//MoveFileEx isn't supported under Win9x
function _MoveFileEx(Src, Dest: PChar; Flags: DWORD): BOOL;
begin
  if Win32Platform=VER_PLATFORM_WIN32_NT then 
    Result:= MoveFileEx(Src, Dest, Flags)
  else
    begin
    DeleteFile(Dest);
    Result:= MoveFile(Src, Dest);
    end;
end;

procedure FShowProperties(const fn: string; hWnd: THandle);
var
  sei: TShellExecuteInfoA;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.wnd := hWnd;
  sei.lpFile := PChar(fn);
  sei.lpVerb := 'properties';
  sei.fMask := SEE_MASK_INVOKEIDLIST;
  ShellExecuteExA(@sei);
end;



end.
