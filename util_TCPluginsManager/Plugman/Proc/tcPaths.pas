unit tcPaths;

interface

function tcDefExe: string;
function tcDefIni: string;
function tcDefIniFtp: string;


implementation

uses
  Windows, SysUtils, RegProc, SProc;

function tcDefDir: string;
begin
  Result:= SExpandVars('%COMMANDER_PATH%');
  if SExpanded(Result) then Exit;

  Result:=
    GetRegKeyStr(HKEY_CURRENT_USER, 'Software\Ghisler\Total Commander', 'InstallDir',
    GetRegKeyStr(HKEY_LOCAL_MACHINE, 'Software\Ghisler\Total Commander', 'InstallDir', ''));
end;

function tcDefExe: string;
begin
  Result:= tcDefDir;

  if Result<>'' then
    Result:= Result+'\Totalcmd.exe';

  if not FileExists(Result) then
    Result:= '';
end;

function tcDefIni: string;
begin
  Result:= SExpandVars('%COMMANDER_INI%');
  if SExpanded(Result) then Exit;

  Result:= 
    GetRegKeyStr(HKEY_CURRENT_USER, 'SOFTWARE\Ghisler\Total Commander', 'IniFileName',
    GetRegKeyStr(HKEY_LOCAL_MACHINE, 'SOFTWARE\Ghisler\Total Commander', 'IniFileName',
    ''));
  if Result='' then Exit;

  if Pos('\', Result)=0 then
    Insert('%windir%\', Result, 1);

  if Pos('.\', Result)=1 then
    SReplace(Result, '.', tcDefDir);

  Result:= SExpandVars(Result);

  if not FileExists(Result) then
    Result:= '';
end;

function tcDefIniFtp: string;
begin
  Result:= 
    GetRegKeyStr(HKEY_CURRENT_USER, 'SOFTWARE\Ghisler\Total Commander', 'FtpIniName',
    GetRegKeyStr(HKEY_LOCAL_MACHINE, 'SOFTWARE\Ghisler\Total Commander', 'FtpIniName',
    ''));
  if Result='' then Exit;

  if Pos('\', Result)=0 then
    Insert('%windir%\', Result, 1);

  if Pos('.\', Result)=1 then
    SReplace(Result, '.', tcDefDir);

  Result:= SExpandVars(Result);

  if not FileExists(Result) then
    Result:= '';
end;

end.
