// (c) 12/03 Alexey Torgashin
// Written for FAR Plugins Manager project
unit FUnpack;

interface

uses Windows;

function UnZip(const fn, sdir: string): boolean;
function UnRar(const fn, sdir: string): boolean;
function Unpack(const fn, sdir: string): boolean;

var
  fRarPath: string = '';


implementation

uses SysUtils, ShellAPI;


function FShortName(const fn: string): string;
var
  buf: array[0..300] of char;
begin
  SetString(Result, buf, GetShortPathName(PChar(fn), buf, SizeOf(buf)));
end;



function ExecSh_(const cmd, params: string): boolean;
begin
  Result:= ShellExecute(0, nil, PChar(cmd), PChar(params), nil, SW_SHOWNORMAL)>32;
  if Result then Sleep(1000);
end;

function ExecShell(const cmd, params: string): boolean;
var
  si: TShellExecuteInfo;
begin
  FillChar(si, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  si.lpFile:= PChar(cmd);
  si.lpParameters:= PChar(params);
  si.nShow:= SW_SHOWNORMAL;
  Result:= ShellExecuteEx(@si);
  if Result then
    WaitForSingleObject(si.hProcess, INFINITE);
  CloseHandle(si.hProcess);
end;


function Exec(const cmd: string): boolean;
var
  pi: TProcessInformation;
  si: TStartupInfo;
begin
  FillChar(pi, SizeOf(pi), 0);
  FillChar(si, SizeOf(si), 0);
  si.cb:= SizeOf(si);
  si.dwFlags:= STARTF_USESHOWWINDOW;
  si.wShowWindow:= SW_SHOWNORMAL; //MINIMIZED;

  Result:= CreateProcess(nil, PChar(cmd), nil, nil, false, 0,
    nil, nil, si, pi);
  if Result then
    begin
    WaitForSingleObject(pi.hProcess, INFINITE);
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
    end;
end;


function UnZip(const fn, sdir: string): boolean;
var
  s: string;
begin
  Result:= true;
  s:= Format('x -y %s %s\', [fn, sdir]);
  if ExecShell('winrar.exe', s) then Exit;
  if fRarPath<>'' then
   if ExecShell(fRarPath, s) then Exit;
  if Exec(Format('pkzipc.exe -ext -dir -over=all %s %s\', [fn, sdir])) then Exit;
  if Exec(Format('unzip.exe -o %s -d %s', [fn, sdir])) then Exit;
  if Exec(Format('%s\unzip.exe -o %s -d %s', [ExtractFileDir(ParamStr(0)), fn, sdir])) then Exit;
  //if Exec(Format('pkunzip.exe -d -o %s %s\', [fn, sdir])) then Exit;
  //if Exec(Format('wzunzip.exe -d -o %s %s\', [fn, sdir])) then Exit;
  Result:= false;
end;


function UnRar(const fn, sdir: string): boolean;
var
  s: string;
begin
  Result:= true;
  s:= Format('x -y %s %s\', [fn, sdir]);
  if ExecShell('winrar.exe', s) then Exit;
  if fRarPath<>'' then
   if ExecShell(fRarPath, s) then Exit;
  //if ExecShell('rar.exe', s) then Exit;
  //if ExecShell('unrar.exe', s) then Exit;
  if Exec(Format('rar.exe x -y %s %s\', [fn, sdir])) then Exit;
  if Exec(Format('unrar.exe x -y %s %s\', [fn, sdir])) then Exit;
  if Exec(Format('%s\unrar.exe x -y %s %s\', [ExtractFileDir(ParamStr(0)), fn, sdir])) then Exit;
  Result:= false;
end;


function Unpack(const fn, sdir: string): boolean;
var
  s: string;
begin
  //MessageBox(0, PChar(Format('fn: "%s"'#13'sdir: "%s\"', [FShortName(fn), sdir])), 'Unpack', MB_OK);

  s:= LowerCase(ExtractFileExt(fn));
  if s='.zip' then Result:= UnZip(FShortName(fn), '"'+sdir+'"') else
   if s='.rar' then Result:= UnRar(FShortName(fn), '"'+sdir+'"') else
    Result:= false;
  if Result then Sleep(200);
end;


end.
