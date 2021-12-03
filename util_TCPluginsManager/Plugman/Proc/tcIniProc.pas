{
From TC History:

17.05.06  Added: ini section [Configuration] can now also be redirected via
RedirectSection= option. The options AlternateUserIni= and UseIniInProgramDir=
will always be read from the non-redirected file, though

28.02.06  Added: When using AlternateUserIni= option, don't redirect the
following sections by default: Layout, Packer, Confirmation, Extensions,
Shortcuts, PackerPlugins, FileSystemPlugins, ListerPlugins, ContentPlugins

28.02.06  Added: wincmd.ini: In each section except for [Configuration],
an entry RedirectSection= will instruct tc to redirect(or not) this section
to other ini: Values: 0=no redirect, 1=redirect to AlternateUserIni=, <ininame>
redirect to that specific ini

28.02.06  Added: wincmd.ini [Configuration] AlternateUserIni=<somename> will
redirect storing of user-changeable sections to an alternate name, except for
section [Configuration]. Useful for companies where wincmd.ini
is write-protected
}

unit tcIniProc;

interface

function GetTcIniKey(const fn, section, key: string): string;
procedure SetTcIniKey(const fn, section, key: string; const value: string);
procedure DelTcIniKey(const fn, section, key: string);
function GetTcIniKeys(const fn, section: string): string;


implementation

uses
  SysUtils, Windows, IniProc, SProc;

function ActualFileName(const fn, section: string): string;
var
  S: string;
begin
  Result:= fn;

  S:= SExpandVars(GetIniKey(fn, section, 'RedirectSection', ''));

  if (S='') or (S='0') then Exit
  else
  if (S='1') then
    Result:= SExpandVars(GetIniKey(fn, 'Configuration', 'AlternateUserIni', fn))
  else
    Result:= S;

  if Pos('\', Result)=0 then
    Result:= ExtractFilePath(fn)+Result;
end;

function GetTcIniKey(const fn, section, key: string): string;
begin
  Result:= GetIniKey(
    ActualFileName(fn, section),
    section, key, '');
end;

procedure SetTcIniKey(const fn, section, key: string; const value: string);
begin
  SetIniKey(
    ActualFileName(fn, section),
    section, key, value);
end;

procedure DelTcIniKey(const fn, section, key: string);
begin
  DelIniKey(
    ActualFileName(fn, section),
    section, key);
end;

function GetTcIniKeys(const fn, section: string): string;
begin
  Result:= GetIniKeys(
    ActualFileName(fn, section),
    section);
end;


end.
