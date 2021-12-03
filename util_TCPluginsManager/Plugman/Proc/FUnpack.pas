{$I-}

unit FUnpack;

interface

function UnZip(const fn, sdir: string): boolean;
function UnRar(const fn, sdir: string): boolean;
function Unpack(const fn, sdir: string): boolean;

var
  OptUnpack: record
    RarPath: string;
    RunMinimized: boolean;
  end;


implementation

uses
  Windows, ShellAPI, SysUtils, FProc, SProc, LogProc;

const
  cShowCmd: array[boolean] of DWORD = (SW_SHOWNORMAL, SW_SHOWMINNOACTIVE);


function ExecShell(const cmd, params: string): boolean;
begin
  Result:= FExecShell(cmd, params, '', cShowCmd[OptUnpack.RunMinimized], true);
  LogMessage(Result, '"'+cmd+'" '+params);
end;


function Exec(const cmd: string): boolean;
begin
  Result:= FExecProcess(cmd, cShowCmd[OptUnpack.RunMinimized], true);
  LogMessage(Result, cmd);
end;


function UnZip(const fn, sdir: string): boolean;
var
  param: string;
begin
  Result:= true;

  param:= Format('x -y "%s" "%s\"', [fn, sdir]);

  if ExecShell('WinRAR.exe', param) then Exit;

  if (OptUnpack.RarPath<>'') and
    ExecShell(SExpandVars(OptUnpack.RarPath), param) then Exit;

  if Exec(Format('unzip.exe -o "%s" -d "%s"', [fn, sdir])) then Exit;
  if Exec(Format('"%s\unzip.exe" -o "%s" -d "%s"', [ExtractFileDir(ParamStr(0)), fn, sdir])) then Exit;
  if Exec(Format('pkunzip.exe -d -o "%s" "%s\"', [fn, sdir])) then Exit;
  if Exec(Format('wzunzip.exe -d -o "%s" "%s\"', [fn, sdir])) then Exit;
  if Exec(Format('pkzipc.exe -ext -dir -over=all "%s" "%s\"', [fn, sdir])) then Exit;

  Result:= false;
end;


function UnRar(const fn, sdir: string): boolean;
var
  param: string;
begin
  Result:= true;

  param:= Format('x -y "%s" "%s\"', [fn, sdir]);

  if ExecShell('WinRAR.exe', param) then Exit;

  if (OptUnpack.RarPath<>'') and
    ExecShell(SExpandVars(OptUnpack.RarPath), param) then Exit;

  if Exec('Rar.exe '+param) then Exit;
  if Exec('UnRar.exe '+param) then Exit;
  if Exec('"'+ExtractFileDir(ParamStr(0))+'\UnRar.exe" '+param) then Exit;

  Result:= false;
end;


function Unpack(const fn, sdir: string): boolean;
var
  s: string;
begin
  s:= LowerCase(ExtractFileExt(fn));
  if s='.zip' then Result:= UnZip(fn, sdir) else
   if s='.rar' then Result:= UnRar(fn, sdir) else
    Result:= false;

  if Result then Sleep(200);
end;


end.
