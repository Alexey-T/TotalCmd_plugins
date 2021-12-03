library TextSearch;

uses
  SysUtils, ContPlug,
  SProc, TextProc;

const
  _FieldsNum = 1;
  _Fields: array[0.._FieldsNum-1] of PChar = ('Text');
  _FieldTypes: array[0.._FieldsNum-1] of integer = (FT_FULLTEXT);
  _FieldUnits: array[0.._FieldsNum-1] of PChar = ('');


function ContentGetSupportedField(
  FieldIndex: integer;
  FieldName, Units: PChar;
  maxlen: integer): integer; stdcall;
begin
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    Exit(FT_NOMOREFIELDS);

  StrCopyBuf(FieldName, _Fields[FieldIndex], MaxLen);
  StrCopyBuf(Units, _FieldUnits[FieldIndex], MaxLen);
  Result:= _FieldTypes[FieldIndex];
end;


function ContentGetValue(
  NamePtr: PChar;
  FieldIndex, UnitIndex: integer;
  FieldValue: PAnsiChar;
  MaxLen, Flags: integer): integer; stdcall;
begin
  Result:= FT_FIELDEMPTY;
end;


function ContentGetValueW(
  NamePtr: PWideChar;
  FieldIndex, UnitIndex: integer;
  FieldValue: PByte;
  MaxLen, Flags: integer): integer; stdcall;
var
  Filename, StrA: string;
  StrW: Widestring;
begin
  if (Flags and CONTENT_DELAYIFSLOW)>0 then
    Exit(FT_DELAYED);

  if (FieldIndex=0) then
  begin
    //Clear cache
    if UnitIndex=-1 then
    begin
      TextObj.Clear;
      Exit(FT_FIELDEMPTY);
    end;

    //MessageBox(0, PChar(IntToStr(UnitIndex)), 'UnitIndex', MB_OK);
    if UnitIndex=0 then
    begin
      Filename:= UTF8Encode(WideString(NamePtr));
      if not TextObj.ReadFile(Filename) then
        Exit(FT_FILEERROR);
    end;

    //UnitIndex is bytes, MaxLen too
    StrW:= Copy(TextObj.TextW, UnitIndex div 2+1, MaxLen div 2);
    if StrW='' then
      Exit(FT_FIELDEMPTY);

    StrCopyBufW(PWideChar(FieldValue), PWideChar(StrW), MaxLen);
    Exit(FT_FULLTEXTW); //fulltextW
  end;

  Result:= FT_FIELDEMPTY;
end;


//--------------------------------------------
exports
  ContentGetSupportedField,
  ContentGetValue,
  ContentGetValueW;

end.
