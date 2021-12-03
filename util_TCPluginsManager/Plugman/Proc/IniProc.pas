unit IniProc;

interface

procedure DelIniKey(const fn, section, key: string);

procedure SetIniKey(const fn, section, key, value: string); overload;
procedure SetIniKey(const fn, section, key: string; value: integer); overload;
procedure SetIniKey(const fn, section, key: string; value: boolean); overload;

function GetIniKey(const fn, section, key, default: string): string; overload;
function GetIniKey(const fn, section, key: string; default: integer): integer; overload;
function GetIniKey(const fn, section, key: string; default: boolean): boolean; overload;

function GetIniKeys(const fn, section: string): string;


implementation

uses SysUtils, Windows;

procedure DelIniKey(const fn, section, key: string);
begin
  WritePrivateProfileString(PChar(section), PChar(key), nil, PChar(fn));
end;

procedure SetIniKey(const fn, section, key, value: string);
begin
  WritePrivateProfileString(PChar(section), PChar(key), PChar(value), PChar(fn));
end;

procedure SetIniKey(const fn, section, key: string; value: integer);
begin
  SetIniKey(fn, section, key, IntToStr(value));
end;

procedure SetIniKey(const fn, section, key: string; value: boolean);
begin
  SetIniKey(fn, section, key, IntToStr(integer(value)));
end;


function GetIniKey(const fn, section, key, default: string): string;
var
  buf: array[0..5*1024-1] of char;
  Size: DWORD;
begin
  if (section='') or (key='') then
    begin Result:= default; Exit end;

  FillChar(buf, SizeOf(buf), 0);
  Size:= GetPrivateProfileString(PChar(section), PChar(key), PChar(default),
    buf, SizeOf(buf), PChar(fn));
  SetString(Result, buf, Size);
end;

function GetIniKey(const fn, section, key: string; default: integer): integer;
begin
  Result:= StrToIntDef(GetIniKey(fn, section, key, ''), default);
end;

function GetIniKey(const fn, section, key: string; default: boolean): boolean;
begin
  Result:= boolean(GetIniKey(fn, section, key, integer(default)));
end;


function GetIniKeys(const fn, section: string): string;
var
  buf: array[0..40*1024-1] of char;
  Size: DWORD;
begin
  FillChar(buf, SizeOf(buf), 0);
  Size:= GetPrivateProfileString(PChar(section), nil, '',
    buf, SizeOf(buf)-1, PChar(fn))+1;
  SetString(Result, buf, Size);
end;

end.
