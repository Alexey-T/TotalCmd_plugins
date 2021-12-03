library IECache;

uses Windows, SysUtils, WinInet, //ShellAPI,
  Forms,
  FSPlugin, CacheObj, FilesObj, FProc, RegProc, IniFile, SProc, IEProc,
  unOpt,
  unPro,
  Controls,
  Clipbrd;

{$E wfx}
{$IFDEF WIN64}
  {$E w64}
  {$IFNDEF VER230} // XE3 and up
    {$E wfx64}
  {$ENDIF}
{$ENDIF}
{$R IEIco.res}
{$R *.res}

const
  fPluginVersion = '1.4.4.2';

var
  fLanguage: string;
  fRereadName: string = '(Reread cache)';
  fShowCookies: boolean;
  fConfirmOpen: boolean;
  fOfflineOpen: boolean;
  fOfflineStart: boolean;
  fStripParam: boolean;
  fStripBr: boolean;
  fOp: integer;    //last op
  fOpDel: boolean; //last op was move
  fClip: boolean; //copying to "c"
  fMode: boolean; //url copied to FindData.cFileName

{$I IECacheProc.pas}

type
  TFindRecord = record
    fList: PFileList; //filelist
    fPath: string;    //path for which filelist is build
    fLevel: integer;  //level of path (0/1/2)
    fItem: integer;   //currently retrieved item from filelist
    end;
  PFindRecord=^TFindRecord;


procedure UpdateOptions;
begin
  fRereadName:= GetMsg(002);
  fReadItemMsg:= GetMsg(005);
  fReadCaption:= GetMsg(004);
  if fOfflineStart then
    SetOfflineMode;
  fExclMask:= URLHISTORY_CACHE_ENTRY;
  if not fShowCookies then
    fExclMask:= fExclMask or COOKIE_CACHE_ENTRY;
end;

procedure ReadIni;
begin
  fLanguage:= GetIniKey('Options', 'Lang', 'English');
  fShowCookies:= boolean(GetIniKey('Options', 'ShowCookies', 1));
  fOfflineOpen:= boolean(GetIniKey('Options', 'OfflineOpen', 1));
  fOfflineStart:= boolean(GetIniKey('Options', 'OfflineStart', 0));
  fOnline:= boolean(GetIniKey('Options', 'OnlineExit', 1));
  fConfirmOpen:= boolean(GetIniKey('Options', 'ConfirmOpen', 1));
  fSkipDomains:= boolean(GetIniKey('Options', 'SkipDomains', 1));
  fSkipDomainsList:= GetIniKey('Options', 'SkipDomainsList', fSkipDomainsList);
  fStripParam:= boolean(GetIniKey('Options', 'StripParam', 1));
  fMode:= boolean(GetIniKey('Options', 'Mode', 0));
  fStripBr:= boolean(GetIniKey('Options', 'HideBr', 0));
  UpdateOptions;
end;

procedure SaveIni;
begin
  SetIniKey('Options', 'Lang', fLanguage);
  SetIniKey('Options', 'ShowCookies', integer(fShowCookies));
  SetIniKey('Options', 'OfflineOpen', integer(fOfflineOpen));
  SetIniKey('Options', 'OfflineStart', integer(fOfflineStart));
  SetIniKey('Options', 'OnlineExit', integer(fOnline));
  SetIniKey('Options', 'ConfirmOpen', integer(fConfirmOpen));
  SetIniKey('Options', 'SkipDomains', integer(fSkipDomains));
  SetIniKey('Options', 'SkipDomainsList', fSkipDomainsList);
  SetIniKey('Options', 'StripParam', integer(fStripParam));
  SetIniKey('Options', 'Mode', integer(fMode));
  SetIniKey('Options', 'HideBr', integer(fStripBr));
end;

procedure ReadCache;
begin
  ProgressN(0);
  ProgressMax(GetCacheItemsNum);
  Cache.Progress:= @ProgressN;
  Cache.ReadCache;
  ProgressHide;
end;


var
  _NPlugin: integer;
  _PProgress: TProgressProc;
  _IsCacheRead: boolean = false;

function FsInit(PluginNr: integer; pProgressProc: tProgressProc;
                pLogProc: tLogProc;
                pRequestProc: tRequestProc): integer; stdcall;
begin
  Result:= 0;
  _NPlugin:= PluginNr;
  _PProgress:= pProgressProc;
  ReadIni;
end;


function FsFindFirst(RemotePath: pAnsiChar; var fd: TWin32FindDataA): thandle; stdcall;
var
  List: PFileList;
  path, up: string;
  level: integer;
  ptr: PFindRecord;
begin
  if not _IsCacheRead then
    begin
    ReadCache;
    _IsCacheRead:= true;
    end;

  path:= RemotePath;
  if (path<>'') and (path[Length(path)]='\') then
    SetLength(path, Length(path)-1);

  if path='' then begin level:= 0; up:= ''; end else
   if SCharCount(path, '\')=1 then begin level:= 1; up:= Copy(path, 2, MaxInt); end else
    begin level:= 2; up:= FilenameToUrl(path); end;

  FillChar(fd, SizeOf(fd), 0);
  List:= New(PFileList, Init);
  List.ReadCache(level, up);

  if not ((level=0) or (List.ItemsNum>0)) then
    begin
    Dispose(List, Done);
    Result:= INVALID_HANDLE_VALUE;
    SetLastError(1);
    //MessageBox(0, PChar('Empty: '+path), 'FsFindFirst', MB_OK);
    Exit
    end;

  GetMem(ptr, SizeOf(TFindRecord));
  FillChar(ptr^, SizeOf(TFindRecord), 0);
  with ptr^ do
    begin
    fPath:= path;
    fList:= List;
    fItem:= 1;
    fLevel:= level;
    if fLevel=0
      then
        begin
        lstrcpynA(fd.cFileName, PAnsiChar(AnsiString(fRereadName)), MAX_PATH);
        fd.dwFileAttributes:= 0;
        end
      else
        if List.ItemsNum>0 then
         with List.Items[fItem] do
          begin
          UrlCopy(fd.cFileName, fName, fNum, fLevel);
          if fLevel<2
            then fd.dwFileAttributes:= FILE_ATTRIBUTE_DIRECTORY
            else fd.dwFileAttributes:= 0;
          fd.nFileSizeHigh:= 0;
          fd.nFileSizeLow:= fSize;
          Move(fTime, fd.ftLastWriteTime, SizeOf(TFileTime));
          Inc(fItem);
          end;
    end;

  ProgressMax(List.ItemsNum);
  ProgressN(0);
  ProgressN(1);

  Result:= THandle(ptr);
end;


function FsFindNext(Hdl: thandle; var fd: TWin32FindDataA): bool; stdcall;
var
  ptr: PFindRecord;
begin
  ptr:= PFindRecord(Hdl);
  with ptr^ do
    begin
    Result:= fItem <= fList.ItemsNum;
    if Result then
     with fList.Items[fItem] do
      begin
      FillChar(fd, SizeOf(fd), 0);
      UrlCopy(fd.cFileName, fName, fNum, fLevel);
      if fLevel < 2
        then fd.dwFileAttributes:= FILE_ATTRIBUTE_DIRECTORY
        else fd.dwFileAttributes:= 0;
      fd.nFileSizeHigh:= 0;
      fd.nFileSizeLow:= fSize;
      Move(fTime, fd.ftLastWriteTime, SizeOf(TFileTime));
      ProgressN(fItem);
      Inc(fItem);
      end;
    end;
end;

function FsFindClose(Hdl: thandle): integer; stdcall;
var
  ptr: PFindRecord;
begin
  ptr:= PFindRecord(Hdl);
  with ptr^ do
    begin
    Dispose(fList, Done);
    fList:= nil;
    fPath:= '';
    fLevel:= 0;
    fItem:= 0;
    end;
  FreeMem(ptr, SizeOf(TFindRecord));
  Result:= 0;
  ProgressHide;
end;


procedure FsGetDefRootName(DefRootName: pAnsiChar; maxlen: integer); stdcall;
begin
  lstrcpynA(DefRootName, 'MSIE Cache', MaxLen);
end;

function FsGetFile(RemoteName, LocalName: pAnsiChar; CopyFlags: integer;
         RemoteInfo: pRemoteInfo): integer; stdcall;
var
  fn, fn2, fn3: string;
begin
  if RemoteName = '\' + fRereadName then
    begin
    Result:= FS_FILE_NOTFOUND;
    Exit
    end;

  fClip:= false;
  if fOp = FS_STATUS_OP_GET_MULTI then
    begin
    fn:= ExtractFileName(LocalName);
    fn2:= ExtractFileName(ExtractFileDir(LocalName));
    fn3:= ExtractFileName(ExtractFileDir(ExtractFileDir(LocalName)));
    if (fn = 'c') or (fn = 'C') or
      (fn2 = 'c') or (fn2 = 'C') or
      (fn3 = 'c') or (fn3 = 'C') then
      begin
      fClip:= true;
      Clipboard.AsText:= Clipboard.AsText + FilenameToUrl(RemoteName) + #13#10;
      Result:= FS_FILE_OK;
      Exit
      end;
    end;

  fn:= UrlToFilename(FilenameToUrl(RemoteName));
  //MessageBox(0, PChar(Format('%s'#13'%s', [fn, LocalName])), 'RName LName', MB_OK or mb_taskmodal);

  CopyFileA(PAnsiChar(AnsiString(fn)), LocalName, false);
  if IsFileExist(LocalName) then
    Result:= FS_FILE_OK
  else
    Result:= FS_FILE_READERROR;

  if (CopyFlags and FS_COPYFLAGS_MOVE) <> 0 then
    begin
    fOpDel:= true;
    DeleteUrlCacheEntryA(PAnsiChar(Ansistring(FilenameToUrl(RemoteName))));
    end;
end;

procedure FsStatusInfo(RemoteDir: pAnsiChar; InfoStartEnd, InfoOperation: integer); stdcall;
begin
  if InfoStartEnd = FS_STATUS_END then
    begin
    if (InfoOperation = FS_STATUS_OP_DELETE) or
      ((InfoOperation = FS_STATUS_OP_GET_MULTI) and fOpDel)
    then
      ReadCache
    else
    if fClip and ((fOp = FS_STATUS_OP_GET_SINGLE) or
                  (fOp = FS_STATUS_OP_GET_MULTI)) then
      MessageBox(0, PChar(GetMsg(008)), PChar(GetMsg(001)), MB_OK or MB_iconinformation or MB_TASKMODAL);
    end;

  fOp:= 0;
  fOpDel:= false;
  fClip:= false;
  if InfoStartEnd = FS_STATUS_START then
    begin
    fOp:= InfoOperation;
    if fOp = FS_STATUS_OP_GET_MULTI then
      Clipboard.AsText:= '';
    end;
end;


procedure InitOptionsStrings;
var
  ver, path: string;
begin
  ver:= GetRegKeyStr(HKEY_LOCAL_MACHINE,
    'Software\Microsoft\Internet Explorer', 'Version', '3.0');
  path:= GetRegKeyStr(HKEY_CURRENT_USER,
    'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', 'Cache', '');

  if Assigned(fmOpt) then
  with fmOpt do
    begin
    Caption:= GetMsg(001);
    btnOk.Caption:= GetMsg(006);
    btnCancel.Caption:= GetMsg(007);
    ffAbout.Caption:= GetMsg(030);
    ffPluginVer.Caption:= Format(GetMsg(031), [fPluginVersion]);
    ffMsieVer.Caption:= Format(GetMsg(032), [ver]);
    ffCacheFolder.Caption:= Format(GetMsg(033), ['']);
    ffCachePath.Text:= path;
    ffLang.Caption:= GetMsg(034);
    ffCookies.Caption:= GetMsg(035);
    ffOfflineOpen.Caption:= GetMsg(036);
    ffOfflineStart.Caption:= GetMsg(037);
    ffConfirmOpen.Caption:= GetMsg(038);
    ffSkipDomains.Caption:= GetMsg(039);
    ffStripPar.Caption:= GetMsg(040);
    ffOnline.Caption:= GetMsg(041);
    ffMode.Caption:= GetMsg(042);
    ffStripBr.Caption:= GetMsg(043);
    end;
end;

procedure UpdateOptionsStrings;
begin
  if Assigned(fmOpt) then
    fLanguage:= fmOpt.ffLanguage.Text;
  InitOptionsStrings;
end;

function FsExecuteFile(MainWin: thandle; RemoteName, Verb: pAnsiChar): integer; stdcall;
var
  url, exe: string;
  h: THandle;
  fd: TWin32FindDataA;
  n: integer;
begin
  Result:= FS_EXEC_ERROR;
  if Verb = 'properties' then
    begin
    if RemoteName = '\' then
      begin
      fmOpt:= TfmOpt.Create(nil);
      try
        with fmOpt do
          begin
          InitOptionsStrings;
          //LangsChange:= @UpdateOptionsStrings;
  
          ffCookies.Checked:= fShowCookies;
          ffOfflineOpen.Checked:= fOfflineOpen;
          ffOfflineStart.Checked:= fOfflineStart;
          ffOnline.Checked:= fOnline;
          ffConfirmOpen.Checked:= fConfirmOpen;
          ffSkipDomains.Checked:= fSkipDomains;
          ffSkipList.Text:= fSkipDomainsList;
          ffStripPar.Checked:= fStripParam;
          ffStripBr.Checked:= fStripBr;
          ffMode.Checked:= fMode;
          //ffModeClick(nil);
  
          h:= FindFirstFileA(PAnsiChar(AnsiString(LangFN('*'))), fd);
          if h <> INVALID_HANDLE_VALUE then
            with ffLanguage do
              begin
              Items.BeginUpdate;
              Items.Clear;
              repeat
                Items.Add(ChangeFileExt(fd.cFileName, ''));
              until not FindNextFileA(h, fd);
              n:= Items.IndexOf(fLanguage);
              if n >= 0 then ItemIndex:= n else ItemIndex:= 0;
              Items.EndUpdate;
              Windows.FindClose(h);
              end;
  
          fmOpt.ShowModal;
          //MessageBox(MainWin, PChar(IntToStr(Form.ModalResult)), 'Options', MB_OK);
          if fmOpt.ModalResult = mrOk then
            begin
            fLanguage:= ffLanguage.Text;
            fShowCookies:= ffCookies.Checked;
            fOfflineOpen:= ffOfflineOpen.Checked;
            fOfflineStart:= ffOfflineStart.Checked;
            fOnline:= ffOnline.Checked;
            fConfirmOpen:= ffConfirmOpen.Checked;
            fSkipDomains:= ffSkipDomains.Checked;
            fSkipDomainsList:= ffSkipList.Text;
            fStripParam:= ffStripPar.Checked;
            fStripBr:= ffStripBr.Checked;
            fMode:= ffMode.Checked;
  
            SaveIni;
            UpdateOptions;
            if _IsCacheRead then
              ReadCache;
            end;
          end;
        finally
          FreeAndNil(fmOpt);
        end;
      SetForegroundWindow(MainWin);
      end;
    Result:= FS_EXEC_OK
    end;

  if Verb = 'open' then
    begin
    if RemoteName = '\' + fRereadName then
      begin
      ReadIni;
      ReadCache;
      lstrcpyA(RemoteName, '\');
      Result:= FS_EXEC_SYMLINK
      end
    else
      begin
      url:= FilenameToUrl(RemoteName);
      if (not fConfirmOpen) or (MessageBox(MainWin,
        PChar(Format(GetMsg(003), [url])),
        PChar(GetMsg(001)), MB_OKCANCEL or MB_ICONQUESTION) = IDOK) then
        begin
        exe:= GetIEExe;
        if exe = '' then Exit;
        if fOfflineOpen then SetOfflineMode;
        ExecShell(exe, url, SW_SHOW, false);
        //ShellExecute(MainWin, 'open', PChar(url), nil, nil, 0{SW_SHOW});
        Result:= FS_EXEC_OK
        end
      end;
    end;
end;

function FsDeleteFile(RemoteName: pAnsiChar): bool; stdcall;
begin
  DeleteUrlCacheEntryA(PAnsiChar(Ansistring(FilenameToUrl(RemoteName))));
  Result:= true;
end;


exports
  FsInit,
  FsFindFirst,
  FsFindNext,
  FsFindClose,
  FsGetDefRootName,
  FsGetFile,
  FsStatusInfo,
  FsExecuteFile,
  FsDeleteFile;

begin
  IniFilename:= ExtractFilePath(GetPluginFilename) + 'IECache.ini';

end.
