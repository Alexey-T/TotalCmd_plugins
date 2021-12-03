library DHTML;

uses Windows, WIHeader, SProc, TextProc;

procedure ModuleInfo(ModuleName, Author, Version: PChar); stdcall;
begin
  lstrcpy(ModuleName, 'DHTML Module');
  lstrcpy(Author, 'Alexey Torgashin');
  lstrcpy(Version, '1.0.2');
end;

procedure RegisterDescPlugins(RegisterPlugin: TWIRegisterPlugin); stdcall;
begin
  RegisterPlugin(0{ID}, ptSameFile, 'HTML Files - Title tag', '*.*', nil);
  RegisterPlugin(1{ID}, ptSameFile, 'HTML Files - Meta tag', '*.*', nil);
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
      s:= GetHtmlTag(fn, 'title');
      StrLCpy(Desc, PChar(s), MaxDescLength);
      Result:= lstrlen(Desc)>0;
      Exit
      end;
    1:
      begin
      s:= GetHtmlMetaTag(fn, 'Description');
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
