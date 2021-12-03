library DText;

uses Windows, WIHeader, SProc, TextProc;

var
  fnIni: string;
  fANSI: boolean;

procedure ModuleInfo(ModuleName, Author, Version: PChar); stdcall;
begin
  lstrcpy(ModuleName, 'DText Module');
  lstrcpy(Author, 'Alexey Torgashin');
  lstrcpy(Version, '1.0.3');
end;

procedure ShowOptions;
var
  s: string;
  n: integer;
begin
  s:= '';
  for n:= Low(fReplaces) to High(fReplaces) do
    with fReplaces[n] do
      s:=s+'Replace['+IntToStr(n)+']: "'+sFrom+'"="'+sTo+'"'#13;
  s:= SFormat('%s'#13'DescChar=%s', [s, fLongDescChar]);
  MessageBox(0, PChar(s), 'Options', MB_OK);
end;

procedure ReadOptions;
var
  s: string;
  n: integer;
begin
  fnIni:= ChangeFileName(GetPluginFilename, 'Plugins.ini');
  fANSI:= GetIniKey('Options', 'TextCodepage', 'ANSI', fnIni)='ANSI';

  fLongDescChar:= GetIniKey('Options', 'LongDescChar', ' ', fnIni);
  if fLongDescChar='\n' then fLongDescChar:= #13;
  
  for n:= Low(fReplaces) to High(fReplaces) do
    begin
    s:= GetIniKey('TextReplaces', 'Replace'+IntToStr(n), '', fnIni);
    fReplaces[n].sFrom:= Copy(s, 1, Pos('=', s)-1);
    fReplaces[n].sTo:= Copy(s, Pos('=', s)+1, MaxInt);
    end;
end;

procedure RegisterDescPlugins(RegisterPlugin: TWIRegisterPlugin); stdcall;
begin
  RegisterPlugin(0{ID}, ptSameFile, 'URL Files', '*.URL', nil);
  RegisterPlugin(1{ID}, ptSameFile, 'Text Files', '*.*', nil);
  ReadOptions;
end;

function ImportDesc_SameFile(PluginID: Word; FileName, Desc: PChar;
  RequireFile: TWIRequireFileProc): Boolean; stdcall;
var
  fn, s: string;
begin
  Result:= false;
  fn:= RequireFile(FileName);
  case PluginID of
    0:
      begin
      s:= GetIniKey('InternetShortcut', 'URL', '', fn);
      StrLCpy(Desc, PChar(s), MaxDescLength);
      Result:= lstrlen(Desc)>0;
      Exit
      end;
    1:
      begin
      s:= GetText(fn);
      if not fANSI then
        s:= ToANSI(s);
      StrLCpy(Desc, PChar(s), MaxDescLength);
      Result:= lstrlen(Desc)>0;
      Exit
      end;
  end;
end;

exports
  ModuleInfo,
  RegisterDescPlugins,
  ImportDesc_SameFile;

end.
