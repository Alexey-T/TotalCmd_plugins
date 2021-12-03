unit DescPlugin;

interface

uses Windows, WIHeader, DEPHeader, ContPlug;

var
  fMainCaption: string = 'File Descriptions';
  fDebug: boolean = false;
  fCheckIllegal: boolean = true; //This controls checking if illegal plugins - should be True!


type
  TDescPluginType = (TypeUnknown, TypeWI, TypeDEP, TypeWDX);
  TDescPluginRec = record
    fDetect: string;
    fFileName: string;
    fHandle: THandle;
    fType: TDescPluginType;
    fName, fAuthor, fVersion: string;
    fData: integer; //for Config.exe
    fFlags: integer; //$0001: Error message was shown

    fRecordWI: record
      fWIPluginsNum: integer;
      fWIPlugins: array[1..100] of record
        fID: integer;
        fType: TWIPluginType;
        fName: string;
        fMask: string;
        fConfigProc: TWIConfigPluginProc;
      end;
      fInitDescImport: TWIInitDescImport;
      fDoneDescImport: TWIDoneDescImport;
      fImportDesc_SameFile: TWIImportDesc_SameFile;
      fImportDesc_SameFileEx: TWIImportDesc_SameFileEx;
      fImportThumbnail: TWIImportThumbnailProcEx;
      //fPluginDefaultStateProc: TWIPluginDefaultStateProc;
      //fPluginCapabilitiesProc: TWIPluginCapabilitiesProc;
    end;

    fRecordDEP: record
      fDEInit: TDEProc;
      fDEConfig: TDEWndProc;
      fDEAbout: TDEWndProc;
      fDEQuit: TDEProc;
      fDEProcessFile: TDEProcessFileProc;
      fDEFreeProcessMem: TDEFreeProcessMemProc;
    end;

    fRecordWDX: record
      fFormat: string;
      fFields: array[0..50] of record
        fType: integer;
        fUnits: string;
        fName: string;
      end;
      fContentGetSupportedField: TContentGetSupportedField;
      fContentGetValue: TContentGetValue;
      fContentSetDefaultParams: TContentSetDefaultParams;
      fContentGetDetectString: TContentGetDetectString;
      fContentPluginUnloading: TContentPluginUnloading;
    end;
  end;

var
  DescPluginsNum: integer;
  DescPlugins: array[1..100] of TDescPluginRec;

procedure InitDescPlugin(const fn, sDetect, sFormat: string);
procedure ShowDescPlugins(ffType: TWIPluginType);
procedure FreeDescPlugins;

function MaskMatch(const fn, fMask: string): boolean;

function GetPluginDesc(const fn: string): string;
function GetPluginThumb(const fn: string; fSizeX, fSizeY: integer): HBitmap;

implementation

uses SProc, VersionInfo;

{$I WDXProc.pas}

//--------------------------------------------
function MaskMatch(const fn, fMask: string): boolean;
var
  ext, mask: string;
begin
  ext:= UpperCase(ExtractFileExt(fn));
  if ext='' then ext:= '.';
  mask:= UpperCase(fMask);
  Result:=
    (Pos('*.*', mask)>0) or
    (Pos(',*'+ext+',', ','+mask+',')>0);

  if Pos('/', mask)>0 then
    Result:= Result or
    (Pos('/*'+ext+'/', '/'+mask+'/')>0) or
    (Pos('/*'+Copy(ext, 1, 3)+'?/', '/'+mask+'/')>0);
end;

//--------------------------------------------
function RegisterPlugin(PluginID: Word; PluginType: TWIPluginType;
  PluginName, FileMask: PChar; ConfigProc: TWIConfigPluginProc): Integer; stdcall;
begin
  with DescPlugins[DescPluginsNum] do
   with fRecordWI do
    if fWIPluginsNum<High(fWIPlugins) then
      begin
      Inc(fWIPluginsNum);
      with fWIPlugins[fWIPluginsNum] do
        begin
        fID:= PluginID;
        fType:= PluginType;
        fName:= PluginName;
        fMask:= FileMask;
        SReplaceAll(fMask, ' ', '');
        @fConfigProc:= @ConfigProc;
        end;
      end;
  Result:= 0;
end;

//--------------------------------------------
procedure GetPluginName(hLib: THandle; var sName, sAuthor, sVersion: string);
var
  ModuleInfo: TWIModuleInfo;
  ModuleInfoEx: TWIModuleInfoEx;
  ModuleInfoRec: TWIModuleInfoRec;
  bufName, bufAuthor, bufVersion: array[0..1024] of char;
begin
  sName:= '';
  sAuthor:= '';
  sVersion:= '';

  @ModuleInfo:= GetProcAddress(hLib, 'ModuleInfo');
  @ModuleInfoEx:= GetProcAddress(hLib, 'ModuleInfoEx');
  if (@ModuleInfo=nil) and (@ModuleInfoEx=nil) then Exit;

  if Assigned(ModuleInfo) then
    begin
    FillChar(bufName, SizeOf(bufName), 0);
    FillChar(bufAuthor, SizeOf(bufAuthor), 0);
    FillChar(bufVersion, SizeOf(bufVersion), 0);
    ModuleInfo(@bufName, @bufAuthor, @bufVersion);
    sName:= bufName;
    sAuthor:= bufAuthor;
    sVersion:= bufVersion;
    Exit
    end;

  if Assigned(ModuleInfoEx) then
    begin
    FillChar(bufName, SizeOf(bufName), 0);
    FillChar(bufAuthor, SizeOf(bufAuthor), 0);
    FillChar(bufVersion, SizeOf(bufVersion), 0);
    ModuleInfoRec.lpszModuleName:= @bufName;
    ModuleInfoRec.lpszAuthor:= @bufAuthor;
    ModuleInfoRec.lpszVersion:= @bufVersion;
    ModuleInfoEx(@ModuleInfoRec);
    sName:= bufName;
    sAuthor:= bufAuthor;
    sVersion:= bufVersion;
    Exit
    end;
end;

//--------------------------------------------
procedure InitDescPlugin(const fn, sDetect, sFormat: string);
const
  cNames: array[1..3] of string = (
    'WhereIsIt Main Description Module',
    'WhereIsIt Graphic Plugins Module',
    'WhereIsIt CDDA Module');
var
  hLib: THandle;
  sName, sAuthor, sVersion: string;
  fIllegal: boolean;
  i: integer;
  LibType: TDescPluginType;
  RegisterDescPlugins: TWIRegisterDescPlugins;
  DEFilterPluginProc: TDEFilterPluginProc; 
  DEPlugin: PDEFilterPlugin;
  ContentGetSupportedField: TContentGetSupportedField;
begin
  hLib:= LoadLibrary(PChar(fn));
  if hLib=0 then
    begin
    MessageBox(0, PChar('Failed to load: '+fn),
      PChar(fMainCaption), MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
    Exit
    end;

  GetPluginName(hLib, sName, sAuthor, sVersion);
  if sName='' then sName:= FileVersionInfo(fn, vsFileDescription);
  if sName='' then sName:= ExtractFileName(fn);
  if sVersion='' then sVersion:= FileVersionInfo(fn, ''{vsFileVersion});
  if sAuthor='' then sAuthor:= FileVersionInfo(fn, vsLegalCopyright);

  if fCheckIllegal then
    begin
    fIllegal:= false;
    for i:= Low(cNames) to High(cNames) do
      if sName=cNames[i] then
        begin fIllegal:= true; Break end;

    if fIllegal then
      begin
      MessageBox(0, PChar(
        'Due to copyright reasons, it''s prohibited to use the following modules:'#13+
        'DescPlugin.dll (WhereIsIt Main Description Module)'#13+
        'DescGraphics.dll (WhereIsIt Graphic Plugins Module)'#13+
        'CDDAPlugin.dll (WhereIsIt CDDA Module)'#13#13+
        'Usage of these modules not as a part of WhereIsIt program is illegal'#13+
        'and violates copyright of WhereIsIt author.'#13#13+
        'Failed to load: '+fn),
        PChar(fMainCaption), MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
      FreeLibrary(hLib);
      Exit
      end;
    end;

  @RegisterDescPlugins:= GetProcAddress(hLib, 'RegisterDescPlugins');
  @DEFilterPluginProc:= GetProcAddress(hLib, 'DEFilterPlug_Get');
  @ContentGetSupportedField:= GetProcAddress(hLib, 'ContentGetSupportedField');

  LibType:= TypeUnknown;
  if Assigned(RegisterDescPlugins) then LibType:= TypeWI else
   if Assigned(DEFilterPluginProc) then LibType:= TypeDEP else
    if Assigned(ContentGetSupportedField) then LibType:= TypeWDX;

  if LibType=TypeUnknown then
    begin
    MessageBox(0, PChar(
      'Library is not supported'#13+
      '(does not contain required exported functions).'#13#13+
      'Failed to load: '+fn),
      PChar(fMainCaption), MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
    FreeLibrary(hLib);
    Exit
    end;

  if DescPluginsNum<High(DescPlugins) then
    begin
    Inc(DescPluginsNum);
    with DescPlugins[DescPluginsNum] do
      begin
      fDetect:= sDetect;
      fFileName:= fn;
      fHandle:= hLib;

      fName:= sName;
      fAuthor:= sAuthor;
      fVersion:= sVersion;
      fType:= LibType;

      fData:= 0;
      fFlags:= 0;

      case LibType of
        TypeWI:
          with fRecordWI do
            begin
            @fInitDescImport:= GetProcAddress(hLib, 'InitDescImport');
            @fDoneDescImport:= GetProcAddress(hLib, 'DoneDescImport');
            @fImportDesc_SameFile:= GetProcAddress(hLib, 'ImportDesc_SameFile');
            @fImportDesc_SameFileEx:= GetProcAddress(hLib, 'ImportDesc_SameFileEx');
            @fImportThumbnail:= GetProcAddress(hLib, 'ImportThumbnailProcEx');
            //@fPluginDefaultStateProc:= GetProcAddress(hLib, 'PluginDefaultStateProc');
            //@fPluginCapabilitiesProc:= GetProcAddress(hLib, 'PluginCapabilitiesProc');
            if Assigned(RegisterDescPlugins) then RegisterDescPlugins(@RegisterPlugin);
            end;

        TypeDEP:
          with fRecordDEP do
            begin
            DEPlugin:= DEFilterPluginProc;
            with DEPlugin^ do
              begin
              fDetect:= SupportedFileTypes;
              @fDEInit:= @Init;
              @fDEConfig:= @Config;
              @fDEAbout:= @About;
              @fDEQuit:= @Quit;
              @fDEProcessFile:= @ProcessFile;
              @fDEFreeProcessMem:= @FreeProcessMem;
              lstrcpy(AppRegPath, 'Software\AtlSoft\DEP Filters');
              fDEInit;
              end;
            end;

        TypeWDX:
          with fRecordWDX do
            begin
            fFormat:= sFormat;
            WDXInitPlugin(DescPluginsNum, ExtractFilePath(ParamStr(0))+'ContPlug.ini');
            end;
      end; //case
      end;
    end;
end;

//--------------------------------------------
procedure ShowDescPlugins(ffType: TWIPluginType);
var
  i, j: integer;
begin
  for i:= 1 to DescPluginsNum do
    with DescPlugins[i] do
      if fType=TypeWI then
       with fRecordWI do
        begin
        Writeln;
        Writeln(fName, ' - ', ExtractFileName(fFileName));
        for j:= 1 to fWIPluginsNum do
          with fWIPlugins[j] do
            if fType=ffType then
              Writeln(fName, ' (', fMask, ')');
        end;
end;

//--------------------------------------------
procedure FreeDescPlugins;
var
  i, j: integer;
begin
  for i:= DescPluginsNum downto 1 do
    with DescPlugins[i] do
      begin
      case fType of
        TypeWI:
          with fRecordWI do
            begin
            for j:= fWIPluginsNum downto 1 do
              with fWIPlugins[j] do
                begin
                fID:= -1;
                fName:= '';
                fMask:= '';
                @fConfigProc:= nil;
                end;
            fWIPluginsNum:= 0;
            end;

        TypeDEP:
          with fRecordDEP do
            if Assigned(fDEQuit) then fDEQuit;

        TypeWDX:
          with fRecordWDX do
            begin
            fFormat:= '';
            for j:= High(fFields) downto Low(fFields) do
              with fFields[j] do
                begin
                fType:= -1;
                fUnits:= '';
                fName:= '';
                end;
            if Assigned(fContentPluginUnloading) then fContentPluginUnloading;
            end;
      end;
      fName:= '';
      fAuthor:= '';
      fVersion:= '';
      fDetect:= '';
      FreeLibrary(fHandle);
      fHandle:= 0;
      fFileName:= '';
      end;
  DescPluginsNum:= 0;
end;

//--------------------------------------------
function RequireFile(FileName: PChar): PChar; stdcall;
begin
  Result:= FileName;
end;

//--------------------------------------------
function GetPluginDesc(const fn: string): string;
var
  ImportSameFileRec: TWIImportSameFileRec;
  bufDesc: array[0..MaxDescLength-1] of char;
  bufAlias: array[0..MaxAliasLength-1] of char;
  i, j: integer;
  res1: boolean;
  res2: integer;
  DEFileInfo: TDEFILTERPLUG_FILEINFO;
  DEOutInfo: TDEFILTERPLUG_OUT;
begin
  Result:= '';
  FillChar(ImportSameFileRec, SizeOf(TWIImportSameFileRec), 0);
  FillChar(DEFileInfo, SizeOf(DEFileInfo), 0);
  DEFileInfo.Size:= SizeOf(DEFileInfo);
  FillChar(DEOutInfo, SizeOf(DEOutInfo), 0);
  DEOutInfo.Size:= SizeOf(DEOutInfo);

  for i:= 1 to DescPluginsNum do
    with DescPlugins[i] do
      begin
      if (fDetect='') or MaskMatch(fn, fDetect) then
        case fType of
          TypeWI:
            with fRecordWI do
              begin
              //DescAPI 1.x
              if Assigned(fImportDesc_SameFile) then
                begin
                if Assigned(fInitDescImport) then fInitDescImport;

                for j:= 1 to fWIPluginsNum do
                  with fWIPlugins[j] do
                    if (fType=ptSameFile) and MaskMatch(fn, fMask) then
                      begin
                      if fDebug then Writeln('Calling: ', fName);
                      FillChar(bufDesc, SizeOf(bufDesc), 0);
                      res1:= fImportDesc_SameFile(fID, PChar(fn), @bufDesc, @RequireFile);
                      if fDebug then Writeln('Call result: ', res1);
                      if (res1) and (lstrlen(bufDesc)>0) then
                        begin Result:= bufDesc; Break end;
                      end;

                if Assigned(fDoneDescImport) then fDoneDescImport;
                if Result<>'' then Break;
                end;

              //DescAPI 2.x
              if Assigned(fImportDesc_SameFileEx) then
                begin
                if Assigned(fInitDescImport) then fInitDescImport;

                for j:= 1 to fWIPluginsNum do
                  with fWIPlugins[j] do
                    if (fType=ptSameFile) and MaskMatch(fn, fMask) then
                      begin
                      if fDebug then Writeln('Calling: ', fName);
                      FillChar(bufDesc, SizeOf(bufDesc), 0);
                      FillChar(bufAlias, SizeOf(bufAlias), 0);
                      with ImportSameFileRec do
                        begin
                        FileName:= PChar(fn);
                        Desc:= @bufDesc;
                        AliasName:= @bufAlias;
                        RequireFileProc:= @RequireFile;
                        MaxDescSize:= 4096;
                        SilentMode:= true;
                        OwnerWnd:= 0;
                        end;
                      res2:= fImportDesc_SameFileEx(fID, @ImportSameFileRec);
                      if fDebug then Writeln('Call result: ', res2);
                      if (res2=0) and (lstrlen(bufDesc)>0) then
                        begin Result:= bufDesc; Break end;
                      end;

                if Assigned(fDoneDescImport) then fDoneDescImport;
                if Result<>'' then Break;
                end;
              end;

          TypeDEP:
            with fRecordDEP do
              if Assigned(fDEProcessFile) then
                begin
                if fDebug then Writeln('Calling: ', fName);
                StrLCpy(@DEFileInfo.FileName, PChar(fn), MAX_PATH);
                res1:= fDEProcessFile(@DEFileInfo, @DEOutInfo);
                if fDebug then Writeln('Call result: ', res1);
                if fDebug and res1 then
                  case DEOutInfo.DataType of
                    DEFILTERPLUG_DATATYPE_NONE: Writeln('Data type: None');
                    DEFILTERPLUG_DATATYPE_TEXT: Writeln('Data type: Text');
                    DEFILTERPLUG_DATATYPE_IMAGE: Writeln('Data type: Image');
                    DEFILTERPLUG_DATATYPE_VIDEO:  Writeln('Data type: Video');
                  end;
                if res1 and (DEOutInfo.DataType=DEFILTERPLUG_DATATYPE_TEXT) then
                  Result:= PChar(DEOutInfo.Data);
                if Assigned(fDEFreeProcessMem) then fDEFreeProcessMem(@DEOutInfo);
                if Result<>'' then Break;
                end;

          TypeWDX:
            with fRecordWDX do
              begin
              Result:= WDXGetDesc(i, fn);
              if Result<>'' then Break;
              end;
        end; //case
      end;
  SReplaceAll(Result, #9, ' ');
end;

//--------------------------------------------
function GetPluginThumb(const fn: string; fSizeX, fSizeY: integer): HBitmap;
var
  ThumbnailRec: TWIThumbnailRec;
  i, j: integer;
  res: integer;
begin
  Result:= 0;
  for i:= 1 to DescPluginsNum do
    with DescPlugins[i] do
     if (fDetect='') or MaskMatch(fn, fDetect) then
      case fType of
        TypeWI:
          with fRecordWI do
            if Assigned(fImportThumbnail) then
              begin
              for j:= 1 to fWIPluginsNum do
                with fWIPlugins[j] do
                  if (fType=ptThumbnails) and MaskMatch(fn, fMask) then
                    begin
                    if fDebug then Writeln('Calling: ', fName);
                    with ThumbnailRec do
                      begin
                      FileName:= PChar(fn);
                      XSize:= fSizeX;
                      YSize:= fSizeY;
                      BitmapHandle:= 0;
                      @RequireFileProc:= @RequireFile;
                      SilentMode:= true;
                      OwnerWnd:= 0;
                      end;
                    res:= fImportThumbnail(fID, @ThumbnailRec);
                    if fDebug then Writeln('Call result: ', res);
                    if res=0 then
                      begin Result:= ThumbnailRec.BitmapHandle; Break end;
                    end;
              if Result<>0 then Break;
              end;
      end;
end;


initialization
  DescPluginsNum:= 0;
  FillChar(DescPlugins, SizeOf(DescPlugins), 0);

finalization
  FreeDescPlugins;

end.
