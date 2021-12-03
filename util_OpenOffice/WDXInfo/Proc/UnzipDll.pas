unit UnzipDll;

interface

function UnzipSingle(const fn, dir: string; masks: array of string): boolean;
function UnzipAll(const fn, dir: string): boolean;

implementation

uses Windows, SysUtils,
  FProc;

function UnzipSingle(const fn, dir: string; masks: array of string): boolean;
var fe, fp: string;
  s, si: string;
begin
  Result:= false;
  fe:= ExtractFilePath(GetModuleName(HInstance)) + 'unzip.exe';
  if not FileExists(fe) then
  begin
    MessageBox(0, 'Unzip.exe not found', 'OOInfo', mb_ok or mb_iconerror or mb_taskmodal);
    Exit
  end;

  s:= '';
  for si in masks do
    s:= s+si+' ';
  fp:= Format('"%s" %s -d "%s"', [fn, s, dir]);

  Result:= FExecShell(fe, fp, sw_hide, true);
end;

function UnzipAll(const fn, dir: string): boolean;
begin
  Result:= UnzipSingle(fn, dir, ['*.*']);
end;

end.
