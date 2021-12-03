{$E wdx}

library EncInfo;

uses
  Windows,
  SysUtils,
  ContPlug,
  IniFile,
  ATxFProc,
  ATxUtf8Detect;

const
  _FieldsNum = 2;
  _Fields: array[0.._FieldsNum-1] of PChar = (
    'Type',
    'Encoding'
    );

  _FieldTypes: array[0.._FieldsNum-1] of integer = (
    ft_multiplechoice,
    ft_multiplechoice
    );

  cResDir       = 'Folder';
  cResBinary    = 'Binary';
  cResText      = 'Text';

  cResUtf16LE   = 'UTF-16 LE';
  cResUtf16BE   = 'UTF-16 BE';
  cResUtf8Bom   = 'UTF-8 BOM';
  cResUtf8NoBom = 'UTF-8 no BOM';
  cResOem       = 'DOS';
  cResOemRus    = 'DOS Ru';
  cResAnsi      = 'ANSI';
  cResAnsiRus   = 'ANSI Ru';
  cResRtf       = 'RTF';

  _FieldUnits: array[0.._FieldsNum-1] of PChar = (
    cResDir+'|'+cResBinary+'|'+cResText,
    //
    cResUtf16LE+'|'+cResUtf16BE+'|'+
    cResUtf8Bom+'|'+cResUtf8NoBom+'|'+
    cResAnsi+'|'+cResAnsiRus+'|'+
    cResOem+'|'+cResOemRus+'|'+
    cResRtf+'|'+cResBinary
	);

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

//--------------------------------------------
function SEnc(const fn: Widestring): string;
var
  b, IsRtf, IsUtf8: boolean;
begin
  Result:= '';

  if IsDirExist(fn) then
  begin
    Result:= '';
    Exit
  end;

  if IsFileUnicodeWithBOM(fn, b) then
  begin
    if b then Result:= cResUtf16BE else Result:= cResUtf16LE;
    Exit
  end;

  IsFileRTFAndUTF8(fn, IsRtf, IsUtf8);
  if IsRtf then
  begin
    Result:= cResRtf;
    Exit
  end;
  if IsUtf8 then
  begin
    Result:= cResUtf8Bom;
    Exit
  end;

  if IsFileUTF8NoBOM(fn) then
  begin
    Result:= cResUtf8NoBom;
    Exit
  end;

  if not IsFileTextFast(fn) then
  begin
    Result:= cResBinary;
    Exit
  end;

  if Bool(opOemEnabled) then
    if IsFileOEM(fn) then
    begin
      Result:= cResOem;
      Exit
    end;

  if Bool(opRusEnabled) then
    if IsFileRus(fn, b) then
    begin
      if b then Result:= cResOemRus else Result:= cResAnsiRus;
      Exit
    end;

  Result:= cResAnsi;
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
    0: //Type
      begin
        if IsDirExist(fn) then
          s:= cResDir
        else
        if IsFileTextFast(fn) then
          s:= cResText
        else
          s:= cResBinary;
      end;
    1: //Encoding
      s:= SEnc(WideString(fn));
  end;

  if s='' then
    Result:= ft_fieldempty
  else
  begin
    StrLCpyA(PAnsiChar(FieldValue), PAnsiChar(Ansistring(s)), MaxLen {div 2});
    Result:= _FieldTypes[FieldIndex];
  end;
end;

procedure Init;
var
  fnIni, fnEx: string;
  //
  procedure GetN(var N: Integer; const Id: string);
  begin
    N:= StrToIntDef(GetIniKey('ini', Id, IntToStr(N), fnIni), N);
  end;
  procedure GetS(var S: string; const Id: string);
  begin
    S:= GetIniKey('ini', Id, S, fnIni);
  end;
  //
begin
  fnIni:= ChangeFileExt(GetModuleName(HInstance), '.ini');
  fnEx:= ChangeFileExt(GetModuleName(HInstance), '.example.ini');
  if FileExists(fnEx) and not FileExists(fnIni) then
    CopyFile(PChar(fnEx), PChar(fnIni), true);

  GetN(opTextBufferSizeKb, 'text_buffer_size_kb');
  GetN(opUtf8BufferSizeKb, 'utf8_buffer_size_kb');
  GetS(opBinaryIgnore, 'binary_ignore');

  GetN(opOemEnabled, 'oem_enabled');
  GetN(opOemBufferSizeKb, 'oem_buffer_size_kb');
  GetN(opOemPercent, 'oem_percent');
  GetS(opOemIgnore, 'oem_ignore');

  GetN(opRusEnabled, 'rus_enabled');
  GetN(opRusBufferSizeKb, 'rus_buffer_size_kb');
  GetN(opRusMinSize, 'rus_min_size');
  GetN(opRusPercent, 'rus_percent');
  GetN(opRusWordLen, 'rus_word_len');
end;


exports
  ContentGetSupportedField,
  ContentGetValue,
  ContentGetValueW;

begin
  Init;  

end.
