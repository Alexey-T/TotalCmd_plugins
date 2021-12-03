unit IEProc;

interface

var fOnline: boolean = true;
function GetIEExe: string;
procedure SetOfflineMode(Value: integer = 1);

implementation

uses Windows, SysUtils, RegProc;

//----------------------------------------------
function GetIEExe: string;
begin
  Result:= GetRegKeyStr(HKEY_LOCAL_MACHINE,
    'Software\Microsoft\Windows\CurrentVersion\App Paths\IEXPLORE.EXE', '', '');
end;

//----------------------------------------------
procedure SetOfflineMode(Value: integer = 1);
begin
  SetRegKeyInt(HKEY_CURRENT_USER,
    'Software\Microsoft\Windows\CurrentVersion\Internet Settings',
    'GlobalUserOffline', Value);
end;

initialization

finalization
  if fOnline then
    SetOfflineMode(0);

end.
