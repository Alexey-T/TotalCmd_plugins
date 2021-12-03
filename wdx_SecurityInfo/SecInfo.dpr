{$E wdx}

library SecInfo;

{$R *.RES}

uses
  Windows, ContPlug, SecProc;

const
  _FieldsNum = 2;
  _Fields: array[0.._FieldsNum-1] of PChar = 
    ('Group', 'Owner');

  _FieldTypes: array[0.._FieldsNum-1] of integer = 
    (ft_stringw, ft_stringw);

  _FieldUnits: array[0.._FieldsNum-1] of PChar =
    ('', '');

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
function ContentGetSupportedField(FieldIndex: integer;
  FieldName: pAnsiChar;
  Units: pAnsiChar; maxlen: integer): integer; stdcall;
begin
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOMOREFIELDS; Exit end;

  StrLCpyA(FieldName, PAnsiChar(Ansistring(_Fields[FieldIndex])), MaxLen);
  StrLCpyA(Units, PAnsiChar(Ansistring(_FieldUnits[FieldIndex])), MaxLen);
  Result:= _FieldTypes[FieldIndex];
end;

//--------------------------------------------
function ContentGetValue(fn: pAnsiChar;
  FieldIndex, UnitIndex: integer;
  FieldValue: PAnsiChar;
  maxlen, flags: integer): integer; stdcall;
begin
  Result:= ft_fieldempty;
end;

function ContentGetValueW(fn: pWideChar;
  FieldIndex, UnitIndex: integer;
  FieldValue: PWideChar;
  maxlen, flags: integer): integer; stdcall;
var
  s: string;
begin
  //if (flags and CONTENT_DELAYIFSLOW)>0 then
  //  begin Result:= FT_DELAYED; Exit end;

  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOSUCHFIELD; Exit end;

  case FieldIndex of
    0: s:= FFileGroup(fn);
    1: s:= FFileOwner(fn);
  end;

  if s='' then
    Result:= FT_FIELDEMPTY
  else
  begin
    StrLCpyW(FieldValue, PWideChar(Widestring(s)), MaxLen);
    Result:= _FieldTypes[FieldIndex];
  end;
end;

exports
  ContentGetSupportedField,
  ContentGetValue,
  ContentGetValueW;

end.
