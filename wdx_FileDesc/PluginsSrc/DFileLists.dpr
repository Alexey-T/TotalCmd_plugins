library DFileLists;

uses Windows, WIHeader, SProc, TextProc;

var
  fnIni: string;
  fList1,
  fList2: string;

procedure ModuleInfo(ModuleName, Author, Version: PChar); stdcall;
begin
  lstrcpy(ModuleName, 'DFileLists Module');
  lstrcpy(Author, 'Alexey Torgashin');
  lstrcpy(Version, '1.0.2');
end;

procedure RegisterDescPlugins(RegisterPlugin: TWIRegisterPlugin); stdcall;
begin
  RegisterPlugin(0{ID}, ptSameFile, 'Files with Descript.ion description', '*.*', nil);
  RegisterPlugin(1{ID}, ptSameFile, 'Files with Files.bbs description', '*.*', nil);

  fnIni:= ChangeFileName(GetPluginFilename, 'Plugins.ini');
  fList1:= GetIniKey('Options', 'FileList1', 'Descript.ion', fnIni);
  fList2:= GetIniKey('Options', 'FileList2', 'Files.bbs', fnIni);
  fLongDescChar:= GetIniKey('Options', 'LongDescChar', ' ', fnIni);
  if fLongDescChar='\n' then fLongDescChar:= #13;
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
      s:= GetDesc(fn, fList1);
      StrLCpy(Desc, PChar(s), MaxDescLength);
      Result:= lstrlen(Desc)>0;
      Exit
      end;
    1:
      begin
      s:= GetDesc(fn, fList2);
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
