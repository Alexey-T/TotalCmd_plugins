type
  TFsGetRoot = procedure(buf: PChar; buflen: integer); stdcall;
  TFsExecFile = function(hWnd: THandle; RemoteName, Verb: PChar): integer; stdcall;
  TFsInit = function(PluginNr: integer;
              pProgressProc: TProgressProc;
              pLogProc: TLogProc;
              pRequestProc: TRequestProc): integer; stdcall;
  TFsStatusInfo = procedure(RemoteDir: PChar;
              InfoStartEnd, InfoOperation: integer); stdcall;
  TFsSetDefaultParams = procedure(dps: pFsDefaultParamStruct); stdcall;

  TPackerCfg = procedure(hWnd, hDll: THandle); stdcall;
  TPackerCaps = function: integer; stdcall;
  TPackerSetParams = procedure(dps: pPackDefaultParamStruct); stdcall;

  TLSGetDetectString = procedure(Str: PChar; MaxLen: integer); stdcall;
  TContentGetDetectString = procedure(Str: PChar; MaxLen: integer); stdcall;

const
  cFSNameMaxLen = 200;
  cDetectMaxLen = 2*1024;


//-----------------------------------------------
function tcFSRootName(const fn: string): string;
var
  hLib: THandle;
  RootProc: TFsGetRoot;
  buf: array[0..cFSNameMaxLen-1] of char;
begin
  Result:= ChangeFileExt(ExtractFileName(fn), '');

  try
    hLib:= LoadLibrary(PChar(fn));
    if hLib=0 then Exit;

    try
      @RootProc:= GetProcAddress(hLib, 'FsGetDefRootName');
      if Assigned(RootProc) then
        begin
        FillChar(buf, SizeOf(buf), 0);
        RootProc(buf, SizeOf(buf));
        Result:= buf;
        end;
    finally
      FreeLibrary(hLib);
    end;
  except
    MsgError(Format(S0073, [fn, 'DllMain']));
  end;
end;


//-----------------------------------------------
function tcPackerConfigure(const fn: string; hWnd: THandle): boolean;
var
  hLib: THandle;
  Proc: TPackerCfg;
  Proc1: TPackerSetParams;
  dps: TPackDefaultParamStruct;
begin
  Result:= false;

  try
    hLib:= LoadLibrary(PChar(fn));
    if hLib=0 then Exit;

    try
      @Proc1:= GetProcAddress(hLib, 'PackSetDefaultParams');
      if Assigned(Proc1) then
        try
          FillChar(dps, SizeOf(dps), 0);
          dps.Size:= SizeOf(dps);
          dps.PluginInterfaceVersionLow:= 10;
          dps.PluginInterfaceVersionHi:= 2;
          lstrcpy(dps.DefaultIniName, PChar(tcDir+'\pkplugin.ini'));
          Proc1(@dps);
        except
          MsgError(Format(S0073, [fn, 'PackSetDefaultParams']));
          Exit;
        end;

      @Proc:= GetProcAddress(hLib, 'ConfigurePacker');
      if not Assigned(Proc) then Exit;

      try
        Proc(hWnd, hLib);
      except
        MsgError(Format(S0073, [fn, 'ConfigurePacker']));
        Exit;
      end;

      Result:= true;

    finally
      FreeLibrary(hLib);
    end;

  except
    MsgError(Format(S0073, [fn, 'DllMain']));
  end;
end;


//-----------------------------------------------
function tcFSConfigure2(const fn, tcDir: string; hWnd: THandle): boolean;
var
  cmd: string;
begin
  Result:= false;

  cmd:= ExtractFileDir(ParamStr(0))+'\Config.exe';
  if not IsFileExist(cmd) then
    begin
    MsgError(Format(S0074, ['Config.exe']));
    Exit
    end;

  try
    Result:= FExecShell(cmd,
      Format('"%s" "%s" %d "%s"', [fn, tcDir, hWnd, MsgCaption(0467)]),
      '', SW_SHOW, false);
  except
  end;
end;


//-----------------------------------------------
function tcLSDetectString(const fn: string): string;
var
  hLib: THandle;
  Proc: TLsGetDetectString;
  buf: array[0..cDetectMaxLen-1] of char;
begin
  Result:= '';

  try
    hLib:= LoadLibrary(PChar(fn));
    if hLib=0 then Exit;

    try
      @Proc:= GetProcAddress(hLib, 'ListGetDetectString');
      if Assigned(Proc) then
        try
          FillChar(buf, SizeOf(buf), 0);
          Proc(buf, SizeOf(buf));
          Result:= buf;
        except
          MsgError(Format(S0073, [fn, 'ListGetDetectString']));
        end;
    finally
      FreeLibrary(hLib);
    end;
  except
    MsgError(Format(S0073, [fn, 'DllMain']));
  end;
end;

//-----------------------------------------------
function tcContentDetectString(const fn: string): string;
var
  hLib: THandle;
  Proc: TContentGetDetectString;
  buf: array[0..cDetectMaxLen-1] of char;
begin
  Result:= '';

  try
    hLib:= LoadLibrary(PChar(fn));
    if hLib=0 then Exit;

    try
      @Proc:= GetProcAddress(hLib, 'ContentGetDetectString');
      if Assigned(Proc) then
        try
          FillChar(buf, SizeOf(buf), 0);
          Proc(buf, SizeOf(buf));
          Result:= buf;
        except
          MsgError(Format(S0073, [fn, 'ContentGetDetectString']));
        end;
    finally
      FreeLibrary(hLib);
    end;
  except
    MsgError(Format(S0073, [fn, 'DllMain']));
  end;
end;

//-----------------------------------------------
function tcPackerCaps(const fn: string): integer;
var
  hLib: THandle;
  Proc: TPackerCaps;
begin
  Result:= 0;

  try
    hLib:= LoadLibrary(PChar(fn));
    if hLib=0 then Exit;

    try
      @Proc:= GetProcAddress(hLib, 'GetPackerCaps');
      if Assigned(Proc) then
        Result:= Proc;
    finally
      FreeLibrary(hLib);
    end;
  except
    MsgError(Format(S0073, [fn, 'DllMain']));
  end;
end;


//-----------------------------------------------
function tcTypeOf(const fn: string): TLibType;
var
  s: string;
  hLib: THandle;
begin
  Result:= fUnknownType;

  if not IsFileExist(fn) then Exit;

  s:= LowerCase(ExtractFileExt(fn));
  if s='.wfx' then begin Result:= fFS; Exit end;
  if s='.wlx' then begin Result:= fLister; Exit end;
  if s='.wcx' then begin Result:= fPacker; Exit end;
  if s='.wdx' then begin Result:= fContent; Exit end;
  if s<>'.dll' then Exit;

  try
    hLib:= LoadLibrary(PChar(fn));
    if hLib=0 then Exit;

    try
      if GetProcAddress(hLib, 'FsInit')<>nil then Result:= fFS else
       if GetProcAddress(hLib, 'ListLoad')<>nil then Result:= fLister else
        if GetProcAddress(hLib, 'OpenArchive')<>nil then Result:= fPacker else
         if GetProcAddress(hLib, 'ContentGetValue')<>nil then Result:= fContent;
    finally
      FreeLibrary(hLib);
    end;
  except
    MsgError(Format(S0073, [fn, 'DllMain']));
  end;
end;
