{$E wdx}
library TextLine;

uses
  Windows,
  SysUtils,
  ContPlug, SProc;

var
  fnIni: string;
  fExt: string;
  fSkip: boolean;
  fReplaces: array[1..20] of
    record sFrom, sTo: string; end;

//--------------------------------------------
function ContentGetSupportedField(FieldIndex: integer;
  FieldName, Units: pAnsiChar;
  maxlen: integer): integer; stdcall;
begin
  if (FieldIndex<0) or (FieldIndex>9) then
    begin Result:= FT_NOMOREFIELDS; Exit end;

  lstrcpynA(FieldName, PAnsiChar(Ansistring(IntToStr(FieldIndex+1))), MaxLen);
  lstrcpynA(Units, 'win|dos', MaxLen);
  Result:= FT_STRINGW;
end;

//------------
procedure Init;
var n:Integer;
  s, fnEx: string;
begin
  fnIni:= ChangeFileExt(GetModuleName(HInstance), '.ini');
  fnEx:= ChangeFileExt(GetModuleName(HInstance), '.example.ini');
  if FileExists(fnEx) and not FileExists(fnIni) then
    CopyFile(PChar(fnEx), PChar(fnIni), true);

  fExt:= GetIniKey('Options', 'Extensions', '', fnIni);
  fSkip:= (GetIniKey('Options', 'SkipEmpty', '0', fnIni) <> '0');
  for n:= Low(fReplaces) to High(fReplaces) do
    begin
    s:= GetIniKey('Replaces', 'S'+IntToStr(n), '=', fnIni);
    fReplaces[n].sfrom:= Copy(s, 1, Pos('=', s)-1);
    fReplaces[n].sto:= Copy(s, Pos('=', s)+1, MaxInt);
    end;
end;

//--------------------------------------------
function ContentGetValue(fn: pAnsiChar;
  FieldIndex, UnitIndex: integer;
  FieldValue: PAnsiChar;
  MaxLen, Flags: integer): integer; stdcall;
begin
  Result:= ft_fieldempty;
end;

function ContentGetValueW(fn: pWideChar;
  FieldIndex, UnitIndex: integer;
  FieldValue: PWideChar;
  MaxLen, Flags: integer): integer; stdcall;
var
  f: TextFile;
  s, ext: string;
  n, EmptyNum: integer;
begin
  if (FieldIndex<0) or (FieldIndex>9) then
    begin Result:= FT_NOSUCHFIELD; Exit end;

  ext:= LowerCase(ExtractFileExt(fn));
  if (ext='') then ext:= '.';
  if (ext<>'') and (Pos('.', ext)=1) then Delete(ext, 1, 1);
  if (fExt<>'') and (Pos(' '+ext+' ', ' '+fExt+' ')=0) then
    begin Result:=FT_FILEERROR; Exit end;

//  if (Flags and CONTENT_DELAYIFSLOW)>0 then
//    begin Result:= FT_DELAYED; Exit end;
  Result:= FT_FIELDEMPTY;

  {$I-}
  AssignFile(f, fn);
  Reset(f);
  {$I+}
  if IOResult<>0 then Exit;

  n:=1;
  EmptyNum:=0;
  repeat
    Readln(f, s);
    s:=Trim(s);
    inc(n);
    //if (s='') and fSkip then inc(EmptyNum);    TO DO !
  until n = FieldIndex+2+EmptyNum;
  Close(f);

  for n:= Low(fReplaces) to High(fReplaces) do
   with fReplaces[n] do
    if sFrom<>'' then
     SReplaceAll(s, sFrom, sTo);
  
  if UnitIndex=1 then
    s:= ToANSI(s);

  if s<>'' then
  begin
    lstrcpynW(FieldValue, PWideChar(Widestring(s)), MaxLen);
    Result:= FT_STRINGW;
  end;
end;

exports
  ContentGetSupportedField,
  ContentGetValue,
  ContentGetValueW;

begin
  Init;
    
end.
