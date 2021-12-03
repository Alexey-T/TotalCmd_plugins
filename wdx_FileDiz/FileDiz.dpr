{$E wdx}

library FileDiz;

uses
  Windows, ContPlug, SProc, VersionInfo, TextProc;

const
  _FieldsNum = 2;
  _Fields: array[0.._FieldsNum-1] of PChar = (
    //'Short description',
    'Long description',
    'Version info'
    );

  _VersionUnitsNum = 12;
  _VersionUnits1: array[0.._VersionUnitsNum-1] of PChar = (
    'CompanyName',
    'FileDescription',
    'FileVersion',
    'InternalName',
    'OriginalFilename',
    'ProductName',
    'ProductVersion',
    'LegalCopyright',
    'LegalTrademarks',
    'Comments',
    'PrivateBuild',
    'SpecialBuild'
    );

  _VersionUnits2: PChar =
    'Company name|'+
    'File description|'+
    'File version|'+
    'Internal name|'+
    'Original filename|'+
    'Product name|'+
    'Product version|'+
    'Legal copyright|'+
    'Legal trademarks|'+
    'Comments|'+
    'Private build|'+
    'Special build';

var
  fnIni: string;
  fDesc: array[1..10] of string;
  fExtHtml: string;
  fExtText: string;
  fExtExe: string;
  fExtUrl: string;

//--------------------------------------------
procedure ContentGetDetectString(DetectString: pchar; maxlen: integer); stdcall;
begin
  StrLCpy(DetectString, ''{DetectString}, MaxLen);
  fnIni:= ChangeFileName(GetPluginFilename, 'FileDiz.ini');
end;

//--------------------------------------------
function ContentGetSupportedField(FieldIndex: integer; FieldName: pchar;
  Units: pchar; maxlen: integer): integer; stdcall;
var
  s: string;
  n: integer;
begin
  //read ini file
  if FieldIndex=0 then
    begin
    for n:= Low(fDesc) to High(fDesc) do
      fDesc[n]:= GetIniKey('Options', 'Desc'+IntToStr(n), '', fnIni);

    fDescChar:= GetIniKey('Options', 'LongDescChar', ' ', fnIni);
    if fDescChar='\n' then fDescChar:= #13;

    fExtHtml:= GetIniKey('Extensions', 'HTML', 'htm html', fnIni);
    fExtText:= GetIniKey('Extensions', 'Text', 'txt', fnIni);
    fExtExe:= GetIniKey('Extensions', 'Exe', 'exe dll', fnIni);
    fExtUrl:= GetIniKey('Extensions', 'Url', 'url', fnIni);

    for n:= Low(fReplaces) to High(fReplaces) do
      begin
      s:= GetIniKey('Replaces', 'S'+IntToStr(n), '=', fnIni);
      fReplaces[n].sfrom:= Copy(s, 1, Pos('=', s)-1);
      fReplaces[n].sto:= Copy(s, Pos('=', s)+1, MaxInt);
      end;

    {
    s:= '';
    for n:= Low(fReplaces) to High(fReplaces) do
     with fReplaces[n] do
      s:= s+'"'+sfrom+'" = "'+sto+'"'#13;
    MessageBox(0, PChar(s), 'Replaces', MB_OK);
    }
    end;

  //get fields
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOMOREFIELDS; Exit end;

  StrLCpy(FieldName, _Fields[FieldIndex], MaxLen);
  Result:= FT_STRING;

  if (FieldIndex=Pred(_FieldsNum))
    then StrLCpy(Units, _VersionUnits2, MaxLen)
    else StrLCpy(Units, '', MaxLen);
end;

//--------------------------------------------
function ExtMatch(const ext, list: string): boolean;
begin
  if ext=''
    then Result:= Pos(' . ',       ' '+list+' ')>0
    else Result:= Pos(' '+ext+' ', ' '+list+' ')>0;
end;

//--------------------------------------------
function ReadDescription(const fn, desc: string{; descType: integer}): string;
var
  ext: string;
begin
  Result:= '';
  ext:= LowerCase(ExtractFileExt(fn));

  if Pos('File:', desc)=1 then
    begin
    Result:= GetDesc(fn, Copy(desc, 6, MaxInt){, descType});
    Exit
    end;

  if Pos('HTML:Meta:', desc)=1 then
    begin
    if ExtMatch(ext, fExtHtml) then
      Result:= GetHtmlMetaTag(fn, Copy(desc, 11, MaxInt));
    Exit
    end;

  if Pos('HTML:Title', desc)=1 then
    begin
    if ExtMatch(ext, fExtHtml) then
      Result:= GetHtmlTag(fn, 'title');
    Exit
    end;

  if desc='Text:ANSI' then
    begin
    if ExtMatch(ext, fExtText) then
      Result:= GetText(fn{, descType});
    Exit
    end;

  if desc='Text:OEM' then
    begin
    if ExtMatch(ext, fExtText) then
      Result:= ToANSI(GetText(fn{, descType}));
    Exit
    end;

  if Pos('VerInfo:', desc)=1 then
    begin
    if ExtMatch(ext, fExtExe) then
      Result:= FileVersionInfo(fn, Copy(desc, 9, MaxInt));
    Exit
    end;

  if desc='UrlInfo' then
    begin
    if ExtMatch(ext, fExtUrl) then
      Result:= GetIniKey('InternetShortcut', 'URL', '', fn);
    Exit
    end;
end;

//--------------------------------------------
function ContentGetValue(fn: pchar; FieldIndex, UnitIndex: integer;
  FieldValue: PChar; MaxLen, Flags: integer): integer; stdcall;
var
  s, ext: string;
  IsExe: boolean;
  n: integer;
begin
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOSUCHFIELD; Exit end;

  ext:= LowerCase(ExtractFileExt(fn));
  IsExe:= ExtMatch(ext, fExtExe);

  if IsExe and ((Flags and CONTENT_DELAYIFSLOW)>0) then
    begin Result:= FT_DELAYED; Exit end;

  //VersionInfo fields
  if (FieldIndex=Pred(_FieldsNum)) then
    begin
    if IsExe then
      s:= FileVersionInfo(fn, _VersionUnits1[UnitIndex]);
    end
  else
  //Description fields
  for n:= Low(fDesc) to High(fDesc) do
    if fDesc[n]<>'' then
      begin
      s:= ReadDescription(fn, fDesc[n]{, FieldIndex});
      //MessageBox(0, PChar(s), PChar(fDesc[n]), MB_OK);
      if s<>'' then Break;
      end;

  if s=''
    then
      Result:= FT_FIELDEMPTY
    else
      begin
      StrLCpy(FieldValue, PChar(s), MaxLen);
      Result:= FT_STRING;
      end;
end;

exports
  ContentGetDetectString,
  ContentGetSupportedField,
  ContentGetValue;

end.
