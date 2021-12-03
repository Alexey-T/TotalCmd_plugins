unit MsgFile;

interface

procedure SetMsgLang(const s: string);

function MsgCaption(n: integer): string;
function MsgCaption2(n: integer): string;
function MsgString(const section, key: string): string; overload;
function MsgString(n: integer): string; overload;

type
  TProc = procedure;

var
  MsgUpdateProc: TProc = nil;


implementation

uses
  Windows, SysUtils, Forms, SProc, FProc, IniProc;

var
  FMsgFile,
  FMsgFileEn: string;

function MsgCaption(n: integer): string;
begin
  Result:= GetIniKey(FMsgFile, 'Captions', Format('%4.4d', [n]), '');
  if Result='' then
    Result:= GetIniKey(FMsgFileEn, 'Captions', Format('%4.4d', [n]), '');
end;

function MsgCaption2(n: integer): string;
begin
  Result:= MsgCaption(n);
  SReplace(Result, '&', '');
end;


function MsgString(n: integer): string; overload;
begin
  Result:= GetIniKey(FMsgFile, 'Messages', PChar(Format('%4.4d', [n])), '');
  if Result='' then
    Result:= GetIniKey(FMsgFileEn, 'Messages', PChar(Format('%4.4d', [n])), '');

  SReplaceAll(Result, '\n', #13);
  SReplaceAll(Result, '\r', #10);
  SReplaceAll(Result, '\t', #9);
end;


function MsgString(const section, key: string): string; overload;
begin
  Result:= GetIniKey(FMsgFile, section, key, '');
end;


procedure SetMsgLang(const s: string);
begin
  FMsgFile:= Format('%sLanguage\%s.lng', [ExtractFilePath(ParamStr(0)), s]);
  FMsgFileEn:= Format('%sLanguage\English.lng', [ExtractFilePath(ParamStr(0))]);

  //If localized file does not exist, try to use English file
  if not IsFileExist(FMsgFile) then
    FMsgFile:= FMsgFileEn;

  //If English file does not exist, halt
  if not IsFileExist(FMsgFile) then
    begin
    Application.MessageBox(
      PChar(Format('Language file "%s" not found.'#13+
      'Try to reinstall application or delete current Plugman.ini file.', [ExtractFileName(FMsgFile)])),
      'TC Plugins Manager', MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
    Halt
    end;

  if Assigned(MsgUpdateProc) then
    MsgUpdateProc;
end;


end.
