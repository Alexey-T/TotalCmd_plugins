library Ntfs_diz;

{$E wdx}
{$R *.RES}

uses
  Windows, ContPlug,
  SysUtils;

var
  sStreams: array[0..9] of string;
  KeepTime: boolean;

//----------------
function GetIniKey(const section, key, default, fnIni: string): string;
var
  buf: array[0..200] of char;
begin
  GetPrivateProfileString(PChar(section), PChar(key),
    PChar(default), buf, 200, PChar(fnIni));
  Result:= buf;
end;

procedure Init;
var
  fnIni, fnEx: string;
  i:Integer;
begin
  fnIni:= ChangeFileExt(GetModuleName(HInstance), '.ini');
  fnEx:= ChangeFileExt(GetModuleName(HInstance), '.sample.ini');
  if FileExists(fnEx) and not FileExists(fnIni) then
    CopyFile(PChar(fnEx), PChar(fnIni), true);
    
  for i:= 0 to High(sStreams) do
    sStreams[i]:= GetIniKey('Streams', IntToStr(i), '', fnIni);
  KeepTime:= GetIniKey('Options', 'KeepTime', '0', fnIni) = '1';

  if sStreams[0] = '' then
    MessageBox(0, 'Plugins doesn''t have filled NTFS_diz.ini file', 'NTFS_diz', mb_ok or mb_iconerror or mb_taskmodal);
end;

//--------------------------------------------
function ContentGetSupportedField(FieldIndex: integer;
  FieldName: pAnsiChar;
  Units: pAnsiChar; maxlen: integer): integer; stdcall;
begin
  if FieldIndex<0 then
    begin Result:= FT_NOMOREFIELDS; Exit end;

  if sStreams[FieldIndex]='' then
    begin Result:= FT_NOMOREFIELDS; Exit end;

  lstrcpynA(FieldName, PAnsiChar(Ansistring(sStreams[FieldIndex])), MaxLen-1);
  lstrcpyA(Units, '');
  Result:= FT_STRINGW;
end;

function ContentGetSupportedFieldFlags(FieldIndex:integer):integer; stdcall;
begin
  Result:= CONTFLAGS_EDIT;
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
  fname, s: string;
  f: Text;
begin
  if FieldIndex<0 then
    begin Result:= FT_NOSUCHFIELD; Exit end;

  s:= '';
  fname:= fn + ':' + sStreams[FieldIndex];

  {$I-}
  AssignFile(f, fname);
  Reset(f);
  if IOResult=0 then
  begin
    Read(f, s);
    Close(f);
  end;
  {$I+}

  (*
  FReadToString(fname, s);
  *)

  if s='' then
  begin
    Result:= ft_fieldempty;
  end
  else
  begin
    //Messagebox(0, PChar('"'+fname+'"'#13'"'+s+'"'), 'TT', 0);
    lstrcpynW(FieldValue, PWideChar(Widestring(s)), MaxLen-1);
    Result:= ft_stringW;
  end;
end;

//----------------------
function ContentSetValue(fn: pAnsiChar;
  FieldIndex,UnitIndex,FieldType:integer;
  FieldValue:PAnsiChar;
  Flags:integer):integer; stdcall;
begin
  Result:= ft_fileerror;
end;

function ContentSetValueW(fn: pWideChar;
  FieldIndex,UnitIndex,FieldType:integer;
  FieldValue:PWideChar;
  Flags:integer):integer; stdcall;
var
  f: TextFile;
  fHan, fDate: integer;
begin
  if FieldIndex<0 then
    begin Result:= FT_NOSUCHFIELD; Exit end;
  fDate:= 0;  

  {$I-}
  if KeepTime then
    begin
      fHan:= FileOpen(fn, fmOpenRead or fmShareDenyNone);
      fDate:= FileGetDate(fHan);
      FileClose(fHan);
    end;

  AssignFile(f, fn + ':' + sStreams[FieldIndex]);
  Rewrite(f);
  if IOResult<>0 then
    begin
      Result:= FT_FILEERROR;
      Exit
    end;
  Write(f, FieldValue);
  Close(f);
  if length(FieldValue) = 0 then
    begin
     Erase(f);
     Result:= FT_SETSUCCESS;
     Exit
    end;

  if KeepTime then
    begin
      fHan:= FileOpen(fn, fmOpenWrite or fmShareDenyNone);
      FileSetDate(fHan, fDate);
      FileClose(fHan);
    end;

  {$I+}
  Result:= FT_SETSUCCESS;
end;

exports
  ContentGetSupportedField,
  ContentGetSupportedFieldFlags,
  ContentGetValue,
  ContentGetValueW,
  ContentSetValue,
  ContentSetValueW;

begin
  Init;

end.
