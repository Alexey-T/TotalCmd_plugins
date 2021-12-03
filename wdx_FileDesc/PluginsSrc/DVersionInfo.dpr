library DVersionInfo;

uses Windows, WIHeader, SProc, VersionInfo;

procedure ModuleInfo(ModuleName, Author, Version: PChar); stdcall;
begin
  lstrcpy(ModuleName, 'DVersionInfo Module');
  lstrcpy(Author, 'Alexey Torgashin');
  lstrcpy(Version, '1.0.2');
end;

procedure RegisterDescPlugins(RegisterPlugin: TWIRegisterPlugin); stdcall;
begin
  RegisterPlugin(0{ID}, ptSameFile, 'Executables', '*.*', nil);
end;

function ImportDesc_SameFile(PluginID: Word; FileName, Desc: PChar;
  RequireFile: TWIRequireFileProc): Boolean; stdcall;
var
  fn, ver1, ver2: string;
begin
  Result:= false;
  fn:= RequireFile(FileName);
  case PluginID of
    0:
      begin
      ver1:= FileVersionInfo(fn, 'FileDescription');
      ver2:= FileVersionInfo(fn, '');
      if ver2<>'' then
        ver1:= SFormat('%s (%s)', [ver1, ver2]);
      StrLCpy(Desc, PChar(ver1), MaxDescLength);
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
