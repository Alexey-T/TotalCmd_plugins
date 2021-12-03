const
  cMainCaption = 'TC Plugins Manager: FS-Plugins config tool';

type
  TFsExecFile = function(hWnd: THandle; RemoteName, Verb: PChar): integer; stdcall;
  TFsInit = function(PluginNr: integer;
              pProgressProc: TProgressProc;
              pLogProc: TLogProc;
              pRequestProc: TRequestProc): integer; stdcall;
  TFsStatusInfo = procedure(RemoteDir: PChar;
              InfoStartEnd, InfoOperation: integer); stdcall;
  TFsSetDefaultParams = procedure(dps: pFsDefaultParamStruct); stdcall;

function _ProgressProc(PluginNr: integer; SourceName, TargetName: PChar; PercentDone: integer): integer; stdcall;
begin
  Result:= 0;
end;

procedure _LogProc(PluginNr, MsgType: integer; LogString: PChar); stdcall;
begin
end;

function _RequestProc(PluginNr, RequestType: integer;
  CustomTitle, CustomText, ReturnedText: PChar; maxlen: integer): bool; stdcall;
begin
  Result:= false;
end;

//-----------------------------------------------
function tcFSConfigure2(const fn, tcDir: string; hWnd: THandle; sMsg: string): boolean;
var
  hLib: THandle;
  FsExec: TFsExecFile;
  FsInit: TFsInit;
  FsStatusInfo: TFsStatusInfo;
  FsSetParams: TFsSetDefaultParams;
  dfs: TFsDefaultParamStruct;
  Remote: array[0..MAX_PATH-1] of char;
begin
  Result:= false;
  hLib:= LoadLibrary(PChar(fn));
  if hLib=0 then Exit;
  @FsInit:= GetProcAddress(hLib, 'FsInit');
  if @FsInit=nil then begin FreeLibrary(hLib); Exit end;
  @FsExec:= GetProcAddress(hLib, 'FsExecuteFile');
  if @FsExec=nil then begin FreeLibrary(hLib); Exit end;
  @FsStatusInfo:= GetProcAddress(hLib, 'FsStatusInfo'); //can be nil
  @FsSetParams:= GetProcAddress(hLib, 'FsSetDefaultParams'); //can be nil

  FillChar(dfs, SizeOf(dfs), 0);
  dfs.Size:= SizeOf(dfs);
  dfs.PluginInterfaceVersionLow:= 30;
  dfs.PluginInterfaceVersionHi:= 1;
  lstrcpy(dfs.DefaultIniName, PChar(tcDir+'\fsplugin.ini'));

  try
   try
     FsInit(1, _ProgressProc, _LogProc, _RequestProc);
     if @FsSetParams<>nil then
       FsSetParams(@dfs);
     if @FsStatusInfo<>nil then
       FsStatusInfo('\', FS_STATUS_START, FS_STATUS_OP_EXEC);

     Remote:= '\';
     Result:= FsExec(hWnd, Remote, 'properties')=FS_EXEC_OK;

     if sMsg='' then sMsg:= 'Press OK to close config tool';
     MessageBox(hWnd, PChar(sMsg), 'TC Plugins Manager', mb_ok or mb_iconinformation or mb_taskmodal);

     if @FsStatusInfo<>nil then
       FsStatusInfo('\', FS_STATUS_END, FS_STATUS_OP_EXEC);
   except
     MessageBox(hWnd, PChar('Exception in '+fn), cMainCaption, MB_OK or MB_ICONERROR or MB_APPLMODAL);
   end;
  finally
    Sleep(100);
    FreeLibrary(hLib);
  end;

  Result:= true;
end;
