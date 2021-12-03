unit FProc;

interface

function SExpandVars(const s: string): string;
function FExecShell(const cmd, params: string; ShowCmd: integer; fWait: boolean): boolean;
function FReadToString(const fn: string; var s: string): boolean;
function FWriteString(const fn: string; const s: Ansistring): boolean;
function IsFileExist(const fn: string): boolean;
function IsDirExist(const fn: string): boolean;
function FSearchDir(const sname, sdir: string; var fn: string): boolean;
function FShortName(const fn: string): string;

function FGetPluginFilename: string;
function FChangeFileName(const fn, NewName: string): string;
function FGetTempPath: string; overload;
function FGetTempPath(const SubDir: string): string; overload;
function FGetTempPathOOo: string;
procedure FDeleteFileMask(const sDir, sMask: string);
procedure FDeleteDir(const sDir: string);


implementation

uses
  Windows, ShellAPI,
  SysUtils;

function SExpandVars(const s: string): string;
var
  buf: array[0..800] of char;
begin
  SetString(Result, buf, ExpandEnvironmentStrings(PChar(s), buf, SizeOf(buf))-1);
end;

function FExecShell(const cmd, params: string; ShowCmd: integer; fWait: boolean): boolean;
var
  si: TShellExecuteInfo;
begin
  FillChar(si, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  si.lpFile:= PChar(cmd);
  si.lpParameters:= PChar(params);
  si.nShow:= ShowCmd;
  Result:= ShellExecuteEx(@si);
  if Result and fWait then
    WaitForSingleObject(si.hProcess, INFINITE);
  CloseHandle(si.hProcess);
end;


function FReadToString(const fn: string; var s: string): boolean;
var
  Buffer: PAnsiChar;
  BufferSize, ReadSize: DWORD;
  Handle: THandle;
begin
  Result:= false;
  S:= '';

  Handle:= CreateFile(PChar(fn), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if Handle=INVALID_HANDLE_VALUE then Exit;

  BufferSize:= GetFileSize(Handle, nil);
  if BufferSize=$FFFFFFFF then
    begin CloseHandle(Handle); Exit end;

  GetMem(Buffer, BufferSize+1);
  FillChar(Buffer^, BufferSize+1, 0);
  if not ReadFile(Handle, Buffer^, BufferSize, ReadSize, nil) then
    begin CloseHandle(Handle); Exit end;
  S:= AnsiString(Buffer);
  FreeMem(Buffer);

  CloseHandle(Handle);
  Result:= true;
end;


procedure Msg(const s: string);
begin
  //MessageBox(0, PChar(s), 'Viewer', mb_ok or mb_iconerror or mb_taskmodal);
end;

function FWriteString(const fn: string; const s: AnsiString): boolean;
var
  Handle: THandle;
  OutSize: DWORD;
begin
  Result:= false;
  if s='' then Exit;

  if IsFileExist(fn) then
    begin
    SetFileAttributes(PChar(fn), FILE_ATTRIBUTE_NORMAL);
    if not DeleteFile(PChar(fn)) then
      begin Msg('Cannot delete temp file'); Exit end;
    end;

  try
    Handle:= CreateFile(PChar(fn), GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    Result:= Handle<>INVALID_HANDLE_VALUE;
    if not Result then begin Msg('Cannot create temp file'); Exit end;

    Result:= WriteFile(Handle, s[1], Length(s), OutSize, nil);
    if not Result then begin Msg('Cannot write temp file'); Exit end;
  finally
    CloseHandle(Handle);
  end;
end;


function IsFileExist(const fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
begin
  if fn='' then
    begin Result:= false; Exit end;

  h:= FindFirstFile(PChar(fn), fd);
  Result:= h<>INVALID_HANDLE_VALUE;
  if Result then
    Windows.FindClose(h);
end;

function IsDirExist(const fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
begin
  if fn='' then
    begin Result:= false; Exit end;

  h:= FindFirstFile(PChar(fn), fd);
  Result:= h<>INVALID_HANDLE_VALUE;
  if Result then
    Result:= (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0
  else
    Windows.FindClose(h);
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


function FGetPluginFilename: string;
begin
  Result:= GetModuleName(hInstance);
end;

function FChangeFileName(const fn, NewName: string): string;
var
  i: integer;
begin
  i:= Length(fn);
  while (i>0) and (fn[i]<>'\') do Dec(i);
  Result:= Copy(fn, 1, i)+NewName;
end;

function FGetTempPath: string; overload;
begin
  Result:= SExpandVars('%temp%');
end;


function FGetTempPath(const SubDir: string): string; overload;
begin
  Result:= FGetTempPath + '\' + SubDir;
  FDeleteDir(Result);
  CreateDirectory(PChar(Result), nil);
end;

function FGetTempPathOOo: string;
begin
  //Folder name "OOoUtils" must be without extension,
  //ODF Converter dosn't support extension part:
  Result:= FGetTempPath('OOoUtils');
end;


function FShortName(const fn: string): string;
var
  buf: array[0..MAX_PATH] of char;
begin
  SetString(Result, buf, GetShortPathName(PChar(fn), buf, SizeOf(buf)));
end;

procedure FDeleteFileMask(const sDir, sMask: string);
var
  h: THandle;
  fd: TWin32FindData;
begin
  if not IsDirExist(sDir) then Exit;

  h:= FindFirstFile(PChar(sDir+'\'+sMask), fd);
  if h=INVALID_HANDLE_VALUE then Exit;

  repeat
    if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
      DeleteFile(PChar(sDir+'\'+fd.cFileName));
  until not FindNextFile(h, fd);

  Windows.FindClose(h);
end;


procedure FDeleteDir(const sDir: string);
begin
  FDeleteFileMask(sDir, '*.*');
  RemoveDirectory(PChar(sDir));
end;


end.
