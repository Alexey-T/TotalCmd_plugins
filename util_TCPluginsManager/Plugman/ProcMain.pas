//-------------------------------------------------
function FindPluginsInDir(const Dir: string; List: TStringList): boolean;
var
  h: THandle;
  fd: TWin32FindData;
  Name, Ext: string;
begin
  Result:= true;

  //Show progress
  if ProgressActionCancelled(Dir, 0, 0) then
    begin
    Result:= false;
    Exit
    end;

  //Find
  h:= FindFirstFile(PChar(Dir+'\*.*'), fd);
  if h<>INVALID_HANDLE_VALUE then
    try
      repeat
        Name:= Dir+'\'+string(fd.cFileName);
        Ext:= UpperCase(ExtractFileExt(Name));

        if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
          begin
          if (Ext='.WCX') or (EXT='.WDX') or (EXT='.WLX') or (Ext='.WFX') then
            List.Add(Name);
          end
        else
          begin
          if (string(fd.cFileName)<>'.') and (string(fd.cFileName)<>'..') then
            if not FindPluginsInDir(Name, List) then
              begin
              Result:= false;
              Exit
              end;
          end;

      until not FindNextFile(h, fd);
    finally
      Windows.FindClose(h);
    end;
end;

//-------------------------------------------------
function FMoveFolder(const Src, Dest: string; const Log: TLibLog): boolean;
var
  h: THandle;
  fd: TWin32FindData;
  SrcName, DestName: string;
begin
  Result:= false;

  if not IsDirExist(Src) then
    begin
    Log.Errors.Add(MsgCaption(0430)+': '+Src);
    Exit
    end;

  FCreateDirs(Dest);
  if not IsDirExist(Dest) then
    begin
    Log.Errors.Add(MsgCaption(0431)+': '+Dest);
    Exit
    end;

  h:= FindFirstFile(PChar(Src+'\*.*'), fd);
  if h<>INVALID_HANDLE_VALUE then
    try
      repeat
        SrcName:= Src+'\'+string(fd.cFileName);
        DestName:= Dest+'\'+string(fd.cFileName);

        if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then
          begin
          if not _MoveFileEx(PChar(SrcName), PChar(DestName),
            MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING) then
            begin
            Log.Errors.Add(MsgCaption(0432)+': '+SrcName+' --> '+DestName);
            Exit;
            end;
          end
        else
          begin
          if (string(fd.cFileName)<>'.') and (string(fd.cFileName)<>'..') then
            if not FMoveFolder(SrcName, DestName, Log) then
              Exit;
          end;

      until not FindNextFile(h, fd);
    finally
      Windows.FindClose(h);
    end;

  if not RemoveDirectory(PChar(Src)) then
    begin
    Log.Errors.Add(MsgCaption(0433)+': '+Src);
    Exit
    end;

  Result:= true;
end;

//-------------------------------------------------
procedure InstallPluginMatchFromArc(const fn: string);
var
  Dir, fn_inf, fn_dll, fn_dir: string;
begin
  Dir:= FTempDirectory+'\TCPM.tmp';
  FDeleteDir(Dir);
  try
    if not Unpack(fn, Dir) then
      begin MsgError(MsgString(0423)); Exit end;

    fn_inf:= Dir+'\Pluginst.inf';
    if not IsFileExist(fn_inf) then
      begin MsgError(MsgString(0065)); Exit end;

    fn_dll:= GetIniKey(fn_inf, 'PluginInstall', 'File', '');
    fn_dir:= GetIniKey(fn_inf, 'PluginInstall', 'DefaultDir', '');
    if not ((LowerCase(fn_dll) = 'tcmatch.dll') and (fn_dir = '')) then
      begin MsgError(MsgString(0436)); Exit end;
  finally
    FDeleteDir(Dir);
  end;

  Dir:= ExtractFileDir(Opt.TCExe);
  if not Unpack(fn, Dir) then
    begin MsgError(MsgString(0423)); Exit end;

  fn_inf:= Dir+'\Pluginst.inf';
  DeleteFile(PChar(fn_inf));
  MsgInfo(Format(MsgString(0063), [ExtractFileName(fn)]));
end;

//-------------------------------------------------
procedure InstallPluginFromTemp(const TempDir: string; const Log: TLibLog);
var
  fn_inf, fn_dll, fn_dir, fn_ext: string;
  typ: TLibType;
begin
  try
    try
      fn_inf:= TempDir+'\Pluginst.inf';
      fn_dll:= '';
      fn_dir:= '';
      fn_ext:= '';

      if IsFileExist(fn_inf)
        then
          begin
          fn_dll:= GetIniKey(fn_inf, 'PluginInstall', 'File', '');
          fn_dir:= GetIniKey(fn_inf, 'PluginInstall', 'DefaultDir', '');
          fn_ext:= GetIniKey(fn_inf, 'PluginInstall', 'DefaultExtension', '');
          fn_dir:= ExtractFileName(SExpandVars(fn_dir)); //Expand + Strip paths
          if fn_dll='' then
            begin
            Log.Errors.Add(MsgCaption(0436)+': '+TempDir);
            Exit
            end;
          if fn_dir='' then
            fn_dir:= ChangeFileExt(ExtractFileName(fn_dll), '');
          end
        else
          begin
          if (not FSearchDir('*.W?X', TempDir, fn_dll))
             //and not FSearchDir('*.DLL', TempDir, fn_dll))
             then
            begin
            Log.Errors.Add(MsgCaption(0427)+': '+TempDir);
            Exit
            end;
          Delete(fn_dll, 1, Length(TempDir)+1); //Strip base path from dll name
          fn_dir:= ChangeFileExt(ExtractFileName(fn_dll), '');
          end;

      typ:= tcTypeOf(TempDir+'\'+fn_dll);
      if typ=fUnknownType then
        begin
        Log.Errors.Add(MsgCaption(0428)+': '+fn_dll);
        Exit
        end;

      fn_dir:= _DefPluginDir(typ)+'\'+fn_dir;
      fn_dll:= fn_dir+'\'+fn_dll;

      if not FMoveFolder(TempDir, fn_dir, Log) then
        begin
        Log.Errors.Add(MsgCaption(0429)+': '+TempDir+' --> '+fn_dir);
        Exit
        end;

      DeleteFile(PChar(fn_dir+'\Pluginst.inf'));

      InstallPlugin(fn_dll, false, fn_ext, Log);

    finally
      FDeleteDir(TempDir);
    end;
  except
    LogMessage(false, 'Exception in InstallPluginFromTemp: '+TempDir);
  end;
end;


//-------------------------------------------------
procedure InstallPluginFromArchive(const fn: string; const Log: TLibLog);
var
  Dir: string;
  List: TStringList;
  i: integer;
begin
  _CreatePluginDirs;

  Dir:= FTempDirectory+'\TCPM.tmp';
  FDeleteDir(Dir);

  List:= TStringList.Create;

  try
    try
      if not Unpack(fn, Dir) then
        begin
        Log.Errors.Add(MsgCaption(0423)+': '+fn);
        Exit
        end;

      ProgressShow(nil, progressScanDirs);

      if not FindPluginsInDir(Dir, List) then
        begin
        Log.Errors.Add(MsgCaption(0434));
        Exit
        end;

      if List.Count=0 then
        begin
        Log.Errors.Add(MsgCaption(0426)+': '+fn);
        Exit;
        end;

      ProgressShow(nil, progressPlugins);

      for i:= 0 to List.Count-1 do
        begin
        //Show progress
        if ProgressActionCancelled(List[i], i+1, List.Count) then
          begin
          Log.Errors.Add(MsgCaption(0434));
          Break;
          end;

        //Install from temp folder
        if IsFileExist(List[i]) then
          InstallPluginFromTemp(ExtractFileDir(List[i]), Log)
        else
          Log.Errors.Add(MsgCaption(0435)+': '+fn+' -- '+
            '\'+Copy(List[i], Length(Dir)+2, MaxInt));
        end;

    finally
      List.Free;
      FDeleteDir(Dir);
    end;
  except
    LogMessage(false, 'Exception in InstallPluginFromArchive: '+fn);
  end;
end;

//-------------------------------------------------
procedure InstallPlugin(
  const fn: string; CheckDups: boolean;
  const DefaultPackerExt: string; const Log: TLibLog);
var
  ext: string;
  IsPacker: boolean;
  typ: TLibType;
begin
  try
    //Install
    typ:= tcTypeOf(fn);
    if typ=fUnknownType then
      begin
      Log.Errors.Add(MsgCaption(0428)+': '+fn);
      Exit
      end;

    IsPacker:= typ=fPacker;
    ext:= DefaultPackerExt;

    //Assume default packer extension as the name
    //of the DLL (without extension):
    if (ext='') and (not Opt.InstTweakBefore) then
      ext:= ChangeFileExt(ExtractFileName(fn), '');

    if IsPacker and (ext='') then //No default extension: ask for it
      with TFormAdd.Create(nil) do
        try
          sFile.Text:= fn;
          sExt.Text:= ChangeFileExt(ExtractFileName(fn), '');
          sExt.MaxLength:= cMaxPackerExtLen;
          fLibType:= typ;
          if ShowModal<>mrOk then
            begin
            Log.Errors.Add(MsgCaption(0424)+': '+fn);
            Exit;
            end;
          ext:= sExt.Text;
        finally
          Release;
        end;

    if tcInstallPlugin(fn, ext, CheckDups, Log) then
      begin
      if IsPacker and Opt.InstTweakAfter then
        with TFormTweak.Create(nil) do
          try
            SetFN(fn);
            ShowModal;
          finally
            Release;
          end;
      Log.Info.Add(Format('%s (%s)', [LibDesc(fn), fn]));
      LogMessage(true, 'Installed: '+Format('%s (%s)', [LibDesc(fn), fn]));
      end
    else
      begin
      LogMessage(false, 'Cannot install: '+fn);
      end;

  except
    Log.Errors.Add(MsgCaption(0420)+': '+fn);
    LogMessage(false, 'Exception in InstallPlugin: '+fn);
  end;
end;


//-------------------------------------------------
procedure RunTC;
var
  fmt, cmd: string;
begin
  fmt:= '%s /i="%s"';

  if Opt.TCIniFTP<>'' then
    fmt:= fmt+' /f="%s"';

  cmd:= Format(fmt, [Opt.TCExe, Opt.TCIni, SExpandVars(Opt.TCIniFTP)]);
  FExecProcess(cmd, SW_SHOW, false);
  Sleep(Opt.RestartTime);
end;

//-------------------------------------------------
procedure RestartTC;
var
  hWnd: THandle;
begin
  case Opt.RestartCmd of
    0:
      //Simple internal restarter, thanks Ipse
      begin
      hWnd:= FindWindow(cTotalCmdClass, nil);
      SendMessage(hWnd, WM_CLOSE, 0, 0);
      Sleep(200);
      RunTC;
      end;
    1:
      begin
      repeat
        hWnd:= FindWindow(cTotalCmdClass, nil);
        if hWnd=0 then Break;
        SendMessage(hWnd, WM_CLOSE, 0, 0);
        Sleep(200);
      until false;
      RunTC;
      end;
  end;
end;


//-------------------------------------------------
function SGetWord(var s: string): string;
var
  i1, i2, len: integer;
begin
  len:= Length(s);
  if len=0 then begin Result:= ''; Exit end;

  i1:= 1; while ((i1<=len) and (s[i1]=' ')) do Inc(i1);
  i2:= i1; while ((i2<=len) and (s[i2]<>' ')) do Inc(i2);

  Result:= Copy(s, i1, i2-i1);
  Delete(s, 1, i2);
end;


//-------------------------------------------------
procedure ShowLog(const Log: TLibLog; AllowRestart: boolean = true);
var
  i: integer;
  DoRestart: boolean;
begin
  DoRestart:= AllowRestart and Opt.InstRestart and (Log.Info.Count>0);

  with TFormInstallLog.Create(nil) do
    try
      labInfo.Caption:= Format(MsgCaption(0401), [Log.Info.Count]);
      labErr.Caption:= Format(MsgCaption(0402), [Log.Errors.Count]);

      if Log.Info.Count>0 then
        begin
        if DoRestart
          then labRestart.Caption:= MsgCaption(0404)
          else labRestart.Caption:= MsgCaption(0403);
        end
      else
        labRestart.Caption:= MsgCaption(0405);

      with boxInfo do
        begin
        Items.Clear;
        for i:= 0 to Log.Info.Count-1 do
          Items.Add.Caption:= Log.Info[i];
        end;

      with boxErr do
        begin
        Items.Clear;
        for i:= 0 to Log.Errors.Count-1 do
          Items.Add.Caption:= Log.Errors[i];
        end;

      ShowModal;
    finally
      Release;
    end;

  if DoRestart then
    RestartTC;
end;

//-------------------------------------------------
procedure ShowShortcutsHelp;
begin
  MsgInfo(
    MsgCaption(0450) + #13#13 +
    'Ins'      + #9 + MsgCaption(0451) + #13 +
    'Alt+Ins'  + #9 + MsgCaption(0453) + #13 +
    'Ctrl+Ins' + #9 + MsgCaption(0452) + #13 +
    'Enter'    + #9 + MsgCaption(0454) + #13 +
    '1 - 6'    + #9 + MsgCaption(0455) + #13 +
    'F1'       + #9 + MsgCaption(0465) + #13 +
    'F4'       + #9 + MsgCaption(0456) + #13 +
    'F6'       + #9 + MsgCaption(0457) + #13 +
    'F8, Del'  + #9 + MsgCaption(0458) + #13 +
    'F9'       + #9 + MsgCaption(0459) + #13 +
    'Alt+Enter'+ #9 + MsgCaption(0466) + #13 +
    'Ctrl+A'   + #9 + MsgCaption(0460) + #13 +
    'Ctrl+F'   + #9 + MsgCaption(0461) + #13 +
    'Ctrl+O'   + #9 + MsgCaption(0462) + #13 +
    'Ctrl+R'   + #9 + MsgCaption(0463) + #13 +
    'Ctrl+Z'   + #9 + MsgCaption(0464)
    );
end;


//-------------------------------------------------
function InstallationConfirmed(const fn: string): boolean;
var
  S: string;
begin
  if tcInstalled(fn, S)
    then S:= Format(S0068, [S])
    else S:= '';
  Result:= MsgConfirmed(Format(S0059, [ExtractFileName(fn)]) + S);
end;

//-------------------------------------------------
procedure InstallPluginWithConfirmation(const fn: string);
var
  Log: TLibLog;
begin
  if InstallationConfirmed(fn) then
    begin
    InitLog(Log);
    InstallPlugin(fn, true, '', Log);
    ShowLog(Log); //AllowRestart was False in 1.9.2, why??
    FreeLog(Log);
    end;
end;

//-------------------------------------------------
function ShowOrderDialog(typ: TLibType): boolean;
begin
  Assert(typ in [fLister, fPacker, fContent],
    'Invalid plugin type for Order dialog');

  with TFormOrder.Create(nil) do
    try
      FOrderFileName:= Opt.MainIni;
      FOrderType:= typ;
      Result:= ShowModal=mrOk;
    finally
      Release;
    end;
end;

//-------------------------------------------------
procedure RunEditor(const fn: string);
var
  S: string;
begin
  if Opt.EditorUse and (Opt.EditorPath <> '') then
    S:= SExpandVars(Opt.EditorPath)
  else
    S:= tcEditor;

  ShellExecute(Application.Handle, 'open', PChar(S),
    PChar('"' + fn + '"'), PChar(ExtractFileDir(fn)), SW_SHOW);
end;

//-------------------------------------------------
procedure UninstallPluginFilename(const fn: string);
begin
  if tcUninstallPath2(fn, false) then
    MsgInfo(Format(S0076, [ExtractFileName(fn)]))
  else
    MsgError(Format(S0077, [ExtractFileName(fn)]));
end;
