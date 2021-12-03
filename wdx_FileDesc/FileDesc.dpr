{$E wdx}

library FileDesc;

{$R *.RES}

// SysUtils is used mainly because it handles exceptions, you may try
// to comment its usage (plugin size will decrease by ~20Kb)
uses
  Windows, SysUtils, ContPlug, DescPlugin, VersionInfo, SProc;

const
  _FieldsNum = 9;
  _Fields: array[0.._FieldsNum-1] of PChar = (
    'Description',
    'Description (2)',
    'Description (3)',
    'Description (4)',
    'Description (5)',
    'Description (6)',
    'Description (7)',
    'Description (8)',
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

//--------------------------------------------
procedure InitDescPlugins(const fnIni: string);
var
  fn, sDetect, sFormat: string;
  i: integer;
begin
  for i:= 0 to 100 do
    begin
    fn:= GetIniKey('Plugins', IntToStr(i), '', fnIni);
    if fn<>'' then
      begin
      if Pos(':\', fn)=0 then //don't modify absolute path
        fn:= ExtractFilePath(GetPluginFilename)+'Plugins\'+fn;
      sDetect:= GetIniKey('Plugins', IntToStr(i)+'_detect', '', fnIni);
      sFormat:= GetIniKey('Plugins', IntToStr(i)+'_format', '', fnIni);
      if IsFileExist(fn) then
        InitDescPlugin(fn, sDetect, sFormat);
      end;
    end;
end;

//--------------------------------------------
function ContentGetSupportedField(FieldIndex: integer; FieldName: pchar;
  Units: pchar; maxlen: integer): integer; stdcall;
begin
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOMOREFIELDS; Exit end;

  StrLCpy(FieldName, _Fields[FieldIndex], MaxLen);
  Result:= FT_STRING;

  if (FieldIndex=Pred(_FieldsNum))
    then StrLCpy(Units, _VersionUnits2, MaxLen)
    else StrLCpy(Units, '', MaxLen);
end;

//--------------------------------------------
var
  fLastFilename: string = '';
  fLastDesc: string = '';

function GetCachedDesc(const fn: string; FieldIndex: integer): string;

  function SGetStr(var s: string): string;
  var
    k: integer;
  begin
    k:= Pos('\n', s); if k=0 then k:= MaxInt-1;
    Result:= Copy(s, 1, k-1);
    Delete(s, 1, k+1);
  end;
var
  desc: string;
  i: integer;
begin
  if StrIComp(PChar(fn), PChar(fLastFilename))=0
    then
      desc:= fLastDesc
    else
      begin
      desc:= GetPluginDesc(fn);
      fLastFilename:= fn;
      fLastDesc:= desc;
      end;
  for i:= 0 to FieldIndex do
    Result:= SGetStr(desc);
end;  

//--------------------------------------------
function ContentGetValue(fn: pchar; FieldIndex, UnitIndex: integer;
  FieldValue: PChar; MaxLen, Flags: integer): integer; stdcall;
var
  s: string;
begin
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOSUCHFIELD; Exit end;

  if (Flags and CONTENT_DELAYIFSLOW)>0 then
    begin Result:= FT_DELAYED; Exit end;

  try
    if FieldIndex=Pred(_FieldsNum)
      then
        s:= FileVersionInfo(fn, _VersionUnits1[UnitIndex])
      else
        s:= GetCachedDesc(fn, FieldIndex);
  except
    s:= 'Exception!';
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
  ContentGetSupportedField,
  ContentGetValue;

var
  fnIni, fnEx: string;
begin
  fnIni:= ChangeFileName(GetPluginFilename, 'FileDesc.ini');
  fnEx:= ChangeFileName(GetPluginFilename, 'FileDesc.example.ini');
  if IsFileExist(fnEx) and not IsFileExist(fnIni) then
    CopyFile(PChar(fnEx), PChar(fnIni), true);
  if not IsFileExist(fnIni) then
    MessageBox(0, 'Configuration file FileDesc.ini not found!', PChar(fMainCaption), MB_OK or MB_ICONERROR or MB_SETFOREGROUND);

  InitDescPlugins(fnIni);

end.
