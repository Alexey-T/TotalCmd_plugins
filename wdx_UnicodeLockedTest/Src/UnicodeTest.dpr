{$E wdx}

library UnicodeTest;

{$R *.RES}

uses
  Windows, ContPlug, FileProc;

const
  _FieldsNum = 3;
  _Fields: array[0.._FieldsNum-1] of PChar =
    ('Unicode test', 'Locked test', 'Write test');

  _FieldTypes: array[0.._FieldsNum-1] of integer = 
    (ft_multiplechoice, ft_multiplechoice, ft_multiplechoice);

  _FieldUnits: array[0.._FieldsNum-1] of PChar =
    ('ANSI|Partial Unicode|Pure Unicode|Invalid',
     'Access|Locked|Not found',
     'Access|Locked|Not found');

//--------------------------------------------
procedure StrLCpyA(p, p2: PAnsiChar; MaxLen: integer);
begin
  FillChar(p^, MaxLen, 0);
  lstrcpynA(p, p2, MaxLen);
end;

procedure StrLCpyW(p, p2: PWideChar; MaxLen: integer);
begin
  FillChar(p^, MaxLen, 0);
  lstrcpynW(p, p2, MaxLen);
end;

//--------------------------------------------
function ContentGetSupportedField(
  FieldIndex: integer;
  FieldName, Units: pAnsiChar;
  maxlen: integer): integer; stdcall;
begin
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOMOREFIELDS; Exit end;

  StrLCpyA(FieldName, PAnsiChar(Ansistring(_Fields[FieldIndex])), MaxLen);
  StrLCpyA(Units, PAnsiChar(Ansistring(_FieldUnits[FieldIndex])), MaxLen);
  Result:= _FieldTypes[FieldIndex];
end;

//--------------------------------------------
function ContentGetValue(
  fn: pAnsiChar;
  FieldIndex, UnitIndex: integer;
  FieldValue: PAnsiChar;
  maxlen, flags: integer): integer; stdcall;
begin
  Result:= ft_fieldempty;
end;

function ContentGetValueW(
  fn: pWideChar;
  FieldIndex, UnitIndex: integer;
  FieldValue: PByte;
  maxlen, flags: integer): integer; stdcall;
var
  s: string;
begin
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOSUCHFIELD; Exit end;

  case FieldIndex of
    0: s:= SUnicodeTest[FUnicodeTest(fn)];
    1: s:= SAccessTest[FReadTest(fn)];
    2: s:= SAccessTest[FWriteTest(fn)];
  end;

  if s='' then
    Result:= ft_fieldempty
  else
  begin
    StrLCpyA(PAnsiChar(FieldValue), PAnsiChar(Ansistring(s)), MaxLen {div 2});
    Result:= _FieldTypes[FieldIndex];
  end;
end;

exports
  ContentGetSupportedField,
  ContentGetValue,
  ContentGetValueW;

end.
