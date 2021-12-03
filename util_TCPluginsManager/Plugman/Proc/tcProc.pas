unit tcProc;

interface

uses
  SysUtils, Windows, Classes, Dialogs;

function tcFileExists(const fn: string): boolean;
procedure tcSetPaths(const sExe, sIni: string);
procedure tcSearchPlugins(readDesc: boolean);
procedure tcReport(const f: Text);
function tcDefButtonBar: string;
function tcEditor: string;

function tcOption(const section, key: string): string;
function tcOptionPath(const section, key: string): string;
procedure tcOptionDel(const section, key: string; DoDisable: boolean);
procedure tcOptionSet(const section, key: string; value: string);
procedure tcOptionSetWithDetect(const section: string; index: integer; const sFN, sDetect: string);
procedure tcOptionInsertWithDetect(const section: string; RecommendedIndex: integer; const sFN, sDetect: string);
function tcPackerExtensionPresent(const Ext: string; var PluginName: string): boolean;

type
  TLibType = (
    fUnknownType,
    fLister,
    fPacker,
    fFS,
    fContent
    );

  TLibComment = (
    libCmtFilename,
    libCmtVerInfo
    );

  TLibLog = record
    Info,
    Errors: TStringList;
  end;

var
  cLibTypeNames: array[TLibType] of string = (
    '?',
    'Lister',
    'Packer',
    'FS',
    'Content'
    );

const
  cLibSections: array[TLibType] of string = (
    '?',
    'ListerPlugins',
    'PackerPlugins',
    'FileSystemPlugins',
    'ContentPlugins'
    );

const
  cSectionDisabled =
    'DisabledPlugins';

function tcInstallPlugin(const fn: string; PackerExtension: string; CheckDups: boolean; const Log: TLibLog): boolean;
procedure tcUninstallPlugin(n: integer; DoDisable: boolean);
procedure tcRenamePlugin(n: integer; const key: string);
procedure tcEnablePlugin(const fn_enable: string; typ: TLibType);

function tcInstalled(const fn: string; typ: TLibType): boolean; overload;
function tcInstalled(const fn: string; var fParams: string): boolean; overload;
function tcUninstallPath2(const fn: string; DoDisable: boolean): boolean;

function tcLSDetectString(const fn: string): string;
function tcContentDetectString(const fn: string): string;
function tcFSRootName(const fn: string): string;
function tcFSConfigure2(const fn, tcDir: string; hWnd: THandle): boolean;
function tcPackerConfigure(const fn: string; hWnd: THandle): boolean;
function tcPackerCaps(const fn: string): integer;
function tcTypeOf(const fn: string): TLibType;

const
  cMaxLibs = 550;
  cMaxPackerExts = 200;
  cMaxPackerExtLen = 15;

var
  libNum: integer;
  libList: array[1..cMaxLibs] of record
    fn, title, params: string;
    fIndex: integer;
    fType: TLibType;
    fDisabled: boolean;
    end;

function LibCount(typ: TLibType): integer;
function LibDesc(const fn: string): string;
function LibState(n: integer): string;

procedure InitLog(var Log: TLibLog);
procedure ClearLog(const Log: TLibLog);
procedure FreeLog(var Log: TLibLog);

var
  OptTC: record
    UseTCPath: boolean;
    ShowNames: TLibComment;
    ShowDeletedAlways: boolean;
  end;

type
  TPackerFlags = record
    num, sel: integer;
    flagDefault: integer;
    ext: array[0..cMaxPackerExts-1] of record
      str: string; flag: integer end;
    end;  

procedure tcReadPackerFlags(const fn: string; var Flags: TPackerFlags);
procedure tcWritePackerFlags(const fn: string; const Flags: TPackerFlags);

function LSKey(index: integer): string;
function LSKeyD(index: integer): string;
procedure tcReadKeys(const Section: string; Keys: TStringList);


//-----------------------------------------------
implementation

uses
  tcIniProc,
  Msg, MsgFile, VersionInfo, SProc, FProc, RegProc,
  FSPlugin, WCXPlugin, Messages;

//-----------------------------------------------
var
  tcDir: string;
  tcIni: string;

procedure tcSetPaths(const sExe, sIni: string);
begin
  tcDir:= ExtractFileDir(sExe);
  tcIni:= sIni;
end;

//-----------------------------------------------
procedure LibClear;
var
  i: integer;
begin
  for i:= libNum downto 1 do
    with libList[i] do
      begin
      params:= '';
      title:= '';
      fn:= '';
      fIndex:= 0;
      fType:= fUnknownType;
      fDisabled:= false;
      end;
  libNum:= 0;
end;

function LibCount(typ: TLibType): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to libNum do
    if libList[i].fType=typ then
      Inc(Result);
end;

function LibDesc(const fn: string): string;
begin
  Result:= '';

  if OptTC.ShowNames=libCmtVerInfo then
    Result:= FileVersionInfo(fn, vsFileDescription);

  if Length(Result)<2 then
    Result:= ChangeFileExt(ExtractFileName(fn), '');
end;

function LibState(n: integer): string;
var
  fDeleted: boolean;
begin
  with libList[n] do
    begin
    fDeleted:= not tcFileExists(fn);
    if fDeleted and OptTC.ShowDeletedAlways then Result:= ssDeleted else
     if fDisabled then Result:= ssDisabled else
      if fDeleted then Result:= ssDeleted else
       if IsFileUsed(fn) then Result:= ssLoaded else
        Result:= ssIdle;
    end;
end;

function LibIndex(const fn: string; typ: TLibType = fUnknownType): integer;
var
  n: integer;
begin
  Result:= 0;
  for n:= 1 to libNum do
    if (UpperCase(fn) = UpperCase(libList[n].fn)) and
      ((typ = fUnknownType) or (typ = libList[n].fType)) then
      begin Result:= n; Exit end;
end;

procedure LibAdd;
begin
  if libNum>=High(libList) then Exit;
  Inc(libNum);
  with libList[libNum] do
    begin
    fn:= '';
    title:= '';
    params:= '';
    fIndex:= 0;
    fType:= fUnknownType;
    fDisabled:= false;
    end;
end;


//-----------------------------------------------
const
  cDetect = '_detect';

function LSKey(index: integer): string;
begin
  Result:= IntToStr(index);
end;

function LSKeyD(index: integer): string;
begin
  Result:= IntToStr(index)+cDetect;
end;

//-----------------------------------------------
function tcExpandPath(const KeyValue: string): string;
begin
  Result:= KeyValue;
  Result:= SDeleteTo(Result, ','); //Handle Packer flags
  Result:= SExpandVars(Result); //Handle %COMMANDER_PATH%
end;

function tcCollapsePath(const KeyValue: string): string;
begin
  Result:= KeyValue;
  if OptTC.UseTCPath and (Length(tcDir)>3) then
    SReplaceI(Result, tcDir, '%COMMANDER_PATH%');
end;

//-----------------------------------------------
function tcOption(const section, key: string): string;
begin
  Result:= GetTcIniKey(tcIni, section, key);
end;

function tcOptionPath(const section, key: string): string;
begin
  Result:= tcExpandPath(tcOption(section, key));
end;

procedure tcOptionSet(const section, key: string; value: string);
begin
  SetTcIniKey(tcIni, section, key, tcCollapsePath(value));
end;

procedure tcOptionSetWithDetect(const section: string; index: integer; const sFN, sDetect: string);
begin
  tcOptionSet(section, LSKey(index), sFN);
  if sDetect=''
    then tcOptionDel(section, LSKeyD(index), false)
    else tcOptionSet(section, LSKeyD(index), sDetect);
end;

//-----------------------------------------------
function tcNumberedSectionNum(const section: string): integer;
var
  List: TStringList;
  i: integer;
begin
  Result:= 0;
  List:= TStringList.Create;
  try
    tcReadKeys(section, List);
    for i:= 0 to List.Count-1 do
      if Pos('_', List[i])=0 then
        Inc(Result);
  finally
    List.Free;
  end;
end;

//-----------------------------------------------
procedure tcOptionInsertWithDetect(const section: string; RecommendedIndex: integer; const sFN, sDetect: string);
var
  ListN, ListD: TStringList;
  MaxIndex, NeededIndex, i: integer;
begin
  ListN:= TStringList.Create;
  ListD:= TStringList.Create;

  try
    ListN.Add(sFN);
    ListD.Add(sDetect);

    MaxIndex:= tcNumberedSectionNum(section)-1;
    NeededIndex:= IMin(RecommendedIndex, MaxIndex+1);

    for i:= NeededIndex to MaxIndex do
      begin
      ListN.Add(tcOption(section, LSKey(i)));
      ListD.Add(tcOption(section, LSKeyD(i)));

      tcOptionDel(section, LSKey(i), false);
      tcOptionDel(section, LSKeyD(i), false);
      end;

    for i:= 0 to ListN.Count-1 do
      tcOptionSetWithDetect(section, NeededIndex+i, ListN[i], ListD[i]);

  finally
    ListD.Free;
    ListN.Free;
  end;
end;

//-----------------------------------------------
procedure tcOptionDel(const section, key: string; DoDisable: boolean);
var
  fn, params: string;
begin
  if DoDisable then
    begin
    fn:= tcCollapsePath(tcOption(section, key));
    params:= tcOption(section, key+cDetect);
    tcOptionSet(cSectionDisabled, Format('%s;%s;%s;', [section, fn, key]), params);
    end;

  DelTcIniKey(tcIni, section, key);
end;


//-----------------------------------------------
procedure tcReadKeys(const section: string; Keys: TStringList);
var
  s, skey: string;
  n: integer;
begin
  Assert(Assigned(Keys), 'StringList not assigned');
  Keys.Clear;

  s:= GetTcIniKeys(tcIni, section);

  while Pos(#0#0, s)>0 do
    begin
    n:= Pos(#0, s);
    skey:= Copy(s, 1, n-1);
    if StrIComp(PChar(skey), 'RedirectSection')<>0 then
      Keys.Add(skey); //Add to list all except "RedirectSection" key, which is reserved
    Delete(s, 1, n);
    end;
end;

//-----------------------------------------------
{$I tcProcSearch.pas}


//-----------------------------------------------
procedure tcUninstallPath(const section, sfn: string; DoDisable: boolean);
var
  i: integer;
  s, skey: string;
  Keys, Keys1, Keys2: TStringList;
begin
  Keys:= TStringList.Create;
  Keys1:= TStringList.Create;
  Keys2:= TStringList.Create;

  try
    tcReadKeys(section, Keys);

    for i:= 0 to Keys.Count-1 do
      begin
      skey:= Keys[i];
      s:= tcOptionPath(section, skey);
      if StrIComp(PChar(s), PChar(sfn))=0 then
        begin
        tcOptionDel(section, skey, DoDisable);
        tcOptionDel(section, skey+cDetect, false);
        end;
      end;

    //Renumber left keys
    if tcTypeFromSection(section) in [fLister, fContent] then
      begin
      //Reread
      tcReadKeys(section, Keys);

      //Remember keys
      for i:= 0 to Keys.Count-1 do
        if Pos('_', Keys[i])=0 then
          begin
          Keys1.Add(tcOption(section, Keys[i]));
          Keys2.Add(tcOption(section, Keys[i]+cDetect));
          end;

      //Clear section
      for i:= 0 to Keys.Count-1 do
        tcOptionDel(section, Keys[i], false);

      //Write keys
      for i:= 0 to Keys1.Count-1 do
        tcOptionSetWithDetect(section, i, Keys1[i], Keys2[i]);
      end;

  finally
    Keys2.Free;
    Keys1.Free;
    Keys.Free;
  end;
end;


//-----------------------------------------------
procedure tcUninstallPlugin(n: integer; DoDisable: boolean);
begin
  with libList[n] do
    tcUninstallPath(cLibSections[fType], fn, DoDisable);
end;


//-----------------------------------------------
procedure tcDisablePackerAssociation(const skey: string);
var
  j: integer;
begin
  for j:= 1 to libNum do
    with libList[j] do
      if (fType=fPacker) and (fDisabled=false) and
         (Pos(' '+UpperCase(skey)+' ', ' '+UpperCase(params)+' ')>0) then
        begin
        //MessageBox(0, PChar(title+': '+params), 'To disable', MB_OK);
        tcUninstallPlugin(j, true);
        end;
  tcSearchPlugins(false);
end;

//-----------------------------------------------
procedure tcEnablePlugin(const fn_enable: string; typ: TLibType);
var
  Keys: TStringList;
  _tckey, s, ssection, sfn, sfn2, skey, svalue: string;
  i: integer;
begin
  //MessageBox(0, PChar('File to enable: '+fn_enable), 'Enable plugin', MB_OK);

  Keys:= TStringList.Create;
  try
    //1) Led's suggestion: disable Packer plugins with the same association
    tcReadKeys(cSectionDisabled, Keys);
    for i:= 0 to Keys.Count-1 do
      begin
      //---
      // section;nnn,fn;key; = value
      _tckey:= Keys[i];
      s:= _tckey;
      svalue:= tcOption(cSectionDisabled, _tckey);

      ssection:= SDeleteFrom(s, ';');
      s:= SDeleteTo(s, ';');
      if ssection='' then
        begin MsgError('Empty section for disabled plugin:'#13+Keys[i]); Continue end;

      sfn:= SDeleteFrom(s, ';');
      s:= SDeleteTo(s, ';');
      if sfn='' then
        begin MsgError('Empty filename for disabled plugin:'#13+Keys[i]); Continue end;

      skey:= SDeleteFrom(s, ';');
      if skey='' then
        begin MsgError('Empty key for disabled plugin:'#13+Keys[i]); Continue end;

      sfn2:= tcExpandpath(sfn);
      //---

      //plugin matched in same section?
      if UpperCase(sfn2) <> UpperCase(fn_enable) then Continue;
      if (typ <> fUnknownType) and (typ <> tcTypeFromSection(ssection)) then Continue;

      if tcTypeFromSection(ssection)=fPacker then
        tcDisablePackerAssociation(skey);
      end;

    //2) Enable selected plugin by filename
    tcReadKeys(cSectionDisabled, Keys);
    for i:= 0 to Keys.Count-1 do
      begin
      //---
      // section;nnn,fn;key; = value
      _tckey:= Keys[i];
      s:= _tckey;
      svalue:= tcOption(cSectionDisabled, _tckey);

      ssection:= SDeleteFrom(s, ';');
      s:= SDeleteTo(s, ';');
      if ssection='' then
        begin MsgError('Empty section for disabled plugin:'#13+Keys[i]); Continue end;

      sfn:= SDeleteFrom(s, ';');
      s:= SDeleteTo(s, ';');
      if sfn='' then
        begin MsgError('Empty filename for disabled plugin:'#13+Keys[i]); Continue end;

      skey:= SDeleteFrom(s, ';');
      if skey='' then
        begin MsgError('Empty key for disabled plugin:'#13+Keys[i]); Continue end;

      sfn2:= tcExpandPath(sfn);
      //---

    //MessageBox(0, PChar('Find: '+fn_enable+#13+
    //                    'Current: '+sfn2), 'Enable plugin', MB_OK);

      //plugin matched in same section?
      if UpperCase(sfn2) <> UpperCase(fn_enable) then Continue;
      if (typ <> fUnknownType) and (typ <> tcTypeFromSection(ssection)) then Continue;

      case tcTypeFromSection(ssection) of
        fPacker,
        fFS:
          tcOptionSet(ssection, skey, sfn);
        fLister,
        fContent:
          tcOptionInsertWithDetect(ssection, StrToIntDef(skey, 0), sfn, svalue);
      end;

      tcOptionDel(cSectionDisabled, _tckey, false);
      end;
  finally
    Keys.Free;
  end;
end;

//-----------------------------------------------
function tcInstallPlugin(const fn: string; PackerExtension: string; CheckDups: boolean; const Log: TLibLog): boolean;

  function SGetExt(var sExt: string): string;
  var
    k: integer;
  begin
    k:= Pos(',', sExt); if k=0 then k:= MaxInt;
    Result:= Copy(sExt, 1, k-1);
    Delete(sExt, 1, k);
  end;
var
  section, sDetect, sParams: string;
  index: integer;
  typ: TLibType;
begin
  Result:= false;

  try
    //Is valid plugin?
    typ:= tcTypeOf(fn);
    if typ=fUnknownType then
      begin
      Log.Errors.Add(MsgCaption(0428)+': '+fn);
      Exit
      end;

    //Is already installed?
    if (typ in [fLister, fFS, fContent]) and (tcInstalled(fn, typ)) then 
      begin
      if CheckDups then
        Log.Errors.Add(MsgCaption(0421)+': '+fn);
      Result:= not CheckDups;
      Exit
      end;

    //Do installing
    section:= cLibSections[typ];
    case typ of
      fPacker:
        begin
        sParams:= Format('%d,%s', [tcPackerCaps(fn), fn]);
        while PackerExtension<>'' do
          tcOptionSet(section, SGetExt(PackerExtension), sParams);
        end;
      fFS:
        begin
        tcOptionSet(section, tcFSRootName(fn), fn);
        end;
      fLister,
      fContent:
        begin
        index:= tcNumberedSectionNum(section);
        if typ=fLister
          then sDetect:= tcLSDetectString(fn)
          else sDetect:= tcContentDetectString(fn);
        tcOptionSetWithDetect(section, index, fn, sDetect);
        end;
      end;

    Result:= true;

  except
    Log.Errors.Add(MsgCaption(0420)+': '+fn);
  end;
end;


//-----------------------------------------------
procedure tcRenamePlugin(n: integer; const key: string);
var
  section: string;
begin
  with libList[n] do
    begin
    section:= cLibSections[fType];
    case fType of
      fFS:
        begin
        tcOptionDel(section, params, false);
        tcOptionSet(section, key, fn);
        end;
      fLister,
      fContent:
        begin
        tcOptionSet(section, LSKeyD(fIndex), key);
        end;
    end;
    end;
end;


//-----------------------------------------------
{$I tcProcMisc.pas}
{$I tcProcDLL.pas}


//-----------------------------------------------
function IsExtensionPresent(const Ext, ExtList: string): boolean;
begin
  Result:= Pos(' '+UpperCase(Ext)+' ', ' '+UpperCase(ExtList)+' ') > 0;
end;

function tcPackerExtensionPresent(const Ext: string; var PluginName: string): boolean;
var
  i: integer;
begin
  Result:= false;
  PluginName:= '';
  for i:=1 to LibNum do
    with libList[i] do
      if (fType=fPacker) and (fDisabled=false) and IsExtensionPresent(Ext, Params) then
        begin
        Result:= true;
        PluginName:= Title;
        Break
        end;
end;


//check existing of WLX, WLX64, UWLX
function tcFileExists(const fn: string): boolean;
var
  ext: string;
  fn2, fn3: string;
begin
  ext:= ExtractFileExt(fn);
  Delete(ext, 1, 1);
  if Length(ext)>3 then
    Result:= FileExists(fn)
  else
  begin
    fn2:= ChangeFileExt(fn, '.'+ext+'64');
    fn3:= ChangeFileExt(fn, '.u'+ext);
    Result:= FileExists(fn) or
      FileExists(fn2) or
      FileExists(fn3);
  end;
end;


//-----------------------------------------------
initialization
  LibClear;

finalization
  LibClear;

end.
