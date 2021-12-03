// RegProc.pas - simple Registry reading/writing
// Written by Alexey Torgashin, thanks to Eugene Roshal

unit RegProc;

interface

uses Windows;

procedure SetRegKeyStr(RootKey: HKEY; SubKey: PChar; Name: PChar; const Value: string);
procedure SetRegKeyInt(RootKey: HKEY; SubKey: PChar; Name: PChar; const Value: DWORD);
procedure SetRegKeyBin(RootKey: HKEY; SubKey: PChar; Name: PChar; const DataPtr: pointer; DataSize: DWORD);
function GetRegKeyStr(RootKey: HKEY; SubKey: PChar; Name: PChar; const Default: string): string;
function GetRegKeyInt(RootKey: HKEY; SubKey: PChar; Name: PChar; const Default: DWORD): DWORD;
function GetRegKeyBin(RootKey: HKEY; SubKey: PChar; Name: PChar; var DataPtr: pointer; var DataSize: DWORD): boolean;
     
implementation

function CreateRegKey(RootKey: HKEY; SubKey: PChar): HKEY;
var
  Disposition: DWORD;
begin
  if RegCreateKeyEx(RootKey, SubKey, 0, nil,
                    REG_OPTION_NON_VOLATILE, KEY_WRITE, nil,
                    Result, @Disposition)<>ERROR_SUCCESS
    then Result:= 0;
end;

function OpenRegKey(RootKey: HKEY; SubKey: PChar): HKEY;
begin
  if RegOpenKeyEx(RootKey, SubKey, 0, KEY_QUERY_VALUE, Result)<>ERROR_SUCCESS
    then Result:= 0;
end;

procedure SetRegKeyStr(RootKey: HKEY; SubKey: PChar; Name: PChar; const Value: string);
var
  h: HKEY;
begin
  h:= CreateRegKey(RootKey, SubKey);
  RegSetValueEx(h, Name, 0, REG_SZ, PChar(Value), Length(Value)+1);
  RegCloseKey(h);
end;

procedure SetRegKeyInt(RootKey: HKEY; SubKey: PChar; Name: PChar; const Value: DWORD);
var
  h: HKEY;
begin
  h:= CreateRegKey(RootKey, SubKey);
  RegSetValueEx(h, Name, 0, REG_DWORD, @Value, SizeOf(DWORD));
  RegCloseKey(h);
end;

procedure SetRegKeyBin(RootKey: HKEY; SubKey: PChar; Name: PChar; const DataPtr: pointer; DataSize: DWORD);
var
  h: HKEY;
begin
  h:= CreateRegKey(RootKey, SubKey);
  RegSetValueEx(h, Name, 0, REG_BINARY, DataPtr, DataSize);
  RegCloseKey(h);
end;

function GetRegKeyStr(RootKey: HKEY; SubKey: PChar; Name: PChar; const Default: string): string;
var
  h: HKEY;
  Buffer: PChar;
  DataType, DataSize: DWORD;
begin
  Result:= Default;
  h:= OpenRegKey(RootKey, SubKey);
  if (RegQueryValueEx(h, Name, nil, @DataType, nil, @DataSize)<>ERROR_SUCCESS)
    or (DataType<>REG_SZ) then
    begin RegCloseKey(h); Exit end;

  GetMem(Buffer, DataSize);
  if (RegQueryValueEx(h, Name, nil, @DataType, PByte(Buffer), @DataSize)<>ERROR_SUCCESS)
    or (DataType<>REG_SZ) then
    begin RegCloseKey(h); Exit end;

  Result:= Buffer;
  FreeMem(Buffer, DataSize);
  RegCloseKey(h);
end;

function GetRegKeyInt(RootKey: HKEY; SubKey: PChar; Name: PChar; const Default: DWORD): DWORD;
var
  h: HKEY;
  DataType, DataSize: DWORD;
begin
  DataSize:= SizeOf(DWORD);
  h:= OpenRegKey(RootKey, SubKey);
  if (RegQueryValueEx(h, Name, nil, @DataType, PByte(@Result), @DataSize)<>ERROR_SUCCESS)
    or (DataType<>REG_DWORD)
    then Result:= Default;
  RegCloseKey(h);
end;

function GetRegKeyBin(RootKey: HKEY; SubKey: PChar; Name: PChar; var DataPtr: pointer; var DataSize: DWORD): boolean;
var
  h: HKEY;
  DataType: DWORD;
begin
  h:= OpenRegKey(RootKey, SubKey);
  Result:= (RegQueryValueEx(h, Name, nil, @DataType, PByte(DataPtr), @DataSize)=ERROR_SUCCESS)
    and (DataType=REG_BINARY);
  if not Result then
    begin DataPtr:= nil; DataSize:= 0 end;
  RegCloseKey(h);
end;

end.
