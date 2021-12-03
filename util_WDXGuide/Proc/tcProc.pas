unit tcProc;

interface

uses SysUtils, Windows;

procedure tcSetPaths(const sExe, sIni: string);
procedure tcSearchPlugins(readDesc: boolean);

function tcOption(const section, key: string): string;
function tcDefIni: string;
function tcDefExe: string;

function IsFileUsed(const fn: string): boolean;
function IsFileExist(const fn: string): boolean;

type
  TLibType = (fLister, fPacker, fFS, fContent);
var
  sTypeNames: array[TLibType] of string;
const
  sLibSections: array[TLibType] of string = (
    'ListerPlugins',
    'PackerPlugins',
    'FileSystemPlugins',
    'ContentPlugins');
  section_disabled = 'DisabledPlugins';

var
  libNum: integer;
  libList: array[1..300] of record
    fn, title, version, params: string;
    fIndex: integer;
    fType: TLibType;
    fLoaded,
    fDeleted,
    fDisabled: boolean;
    end;

type
  TLibComment = (commentFilenameExt, commentFilename, commentVersionInfo);
  TLibVersions = (versionsNone, versionsNum, versionsStr);
var
  fUseTCPath: boolean;
  fShowNames: TLibComment = commentVersionInfo;
  fShowVersions: TLibVersions = versionsNum;

function SExpandVars(const s: string): string; //%Commander_path%

var
  ssIdle: string = 'idle';
  ssLoaded: string = 'loaded';
  ssDisabled: string = '-disabled-';
  ssDeleted: string = '-deleted-';


//-----------------------------------------------
implementation

uses
  SProc, RegProc;

var
  tcDir: string;
  tcIni: string;

procedure tcSetPaths(const sExe, sIni: string);
begin
  tcDir:= ExtractFileDir(sExe);
  tcIni:= sIni;
end;


procedure LibClear;
var
  i: integer;
begin
  for i:= libNum downto 1 do
    with libList[i] do
      begin
      params:= ''; version:= ''; title:= ''; fn:= '';
      end;
  libNum:= 0;
end;

function LibCount(fType: TLibType): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to libNum do
    if libList[i].fType=fType then Inc(Result);
end;

function LibDesc(const fn: string): string;
begin
  Result:= '';
  if fShowNames=commentVersionInfo then
    begin
    //Result:= FileVersionInfo(fn, vsFileDescription);
    end;
  if Length(Result)<2 then
    if fShowNames=commentFilenameExt //with extension
      then Result:= ExtractFileName(fn)
      else Result:= ChangeFileExt(ExtractFileName(fn), '');
end;

function LibState(n: integer): string;
begin
  with libList[n] do
   if fDisabled then Result:= ssDisabled else
    if fDeleted then Result:= ssDeleted else
     if fLoaded then Result:= ssLoaded else
      Result:= ssIdle;
end;

procedure LibStateUpdate(n: integer);
begin
  with libList[n] do
    begin
    fDeleted:= not FileExists(fn);
    fLoaded:= false;
    end;
end;



function LibIndex(const fn: string): integer;
var
  n: integer;
begin
  for n:= 1 to libNum do
    if StrIComp(PChar(fn), PChar(libList[n].fn))=0 then
      begin Result:= n; Exit end;
  Result:= 0;
end;

procedure LibAdd;
begin
  if libNum>=High(libList) then Exit;
  Inc(libNum);
  with libList[libNum] do
    begin
    fn:= '';
    title:= '';
    version:= '';
    params:= '';
    fType:= fPacker;
    fLoaded:= false;
    fDeleted:= false;
    fDisabled:= false;
    end;
end;


//-----------------------------------------------
function tcOption(const section, key: string): string;
var
  buf: array[0..5*1024] of char;
begin
  GetPrivateProfileString(PChar(section), PChar(key), '',
    buf, SizeOf(buf), PChar(tcIni));
  Result:= buf;
end;

function tcOptionPath(const section, key: string): string;
begin
  Result:= SDeleteTo(tcOption(section, key), ',');
  Result:= SExpandVars(Result); //%COMMANDER_PATH%
end;


//-----------------------------------------------
var
  _tcKeys: array[1..300] of string; //zip= rar= 7z= ....
  _tcKeysNum: integer = 0;

procedure tcReadKeys(const section: string);
var
  s: string;
  n: integer;
begin
  for n:= _tcKeysNum downto 1 do
    _tcKeys[n]:= '';
  _tcKeysNum:= 0;
  SetLength(s, 10*1024);
  FillChar(s[1], Length(s), 0);
  SetLength(s, GetPrivateProfileString(PChar(section), nil, '',
    @s[1], Length(s), PChar(tcIni))+1);
  //Writeln('"', s, '"');
  while (Pos(#0#0, s)>0) and (_tcKeysNum<High(_tcKeys)) do
    begin
    n:= Pos(#0, s);
    Inc(_tcKeysNum);
    _tcKeys[_tcKeysNum]:= Copy(s, 1, n-1);
    Delete(s, 1, n);
    end;
end;


{$I tcProcSearch}


//-----------------------------------------------
function SExpandVars(const s: string): string;
begin
  Result:= s;
  SReplaceIAll(Result, '%Commander_path%', tcDir);
  Result:= SProc.SExpandVars(Result);
end;


function tcDefDir: string;
begin
  //поправка SAM-а, чтобы путь брался из запущенного TC
  Result:= SProc.SExpandVars('%Commander_path%');
  //брать из реестра
  if Pos(':\', Result)=0 then
  Result:=
    GetRegKeyStr(HKEY_CURRENT_USER, 'Software\Ghisler\Total Commander', 'InstallDir',
    GetRegKeyStr(HKEY_LOCAL_MACHINE, 'Software\Ghisler\Total Commander', 'InstallDir',
      'C:\TotalCmd'));
end;

function tcDefExe: string;
begin
  Result:= tcDefDir+'\Totalcmd.exe';
end;

function tcDefIni: string;
begin
  Result:= 
    GetRegKeyStr(HKEY_CURRENT_USER, 'SOFTWARE\Ghisler\Total Commander', 'IniFileName',
    GetRegKeyStr(HKEY_LOCAL_MACHINE, 'SOFTWARE\Ghisler\Total Commander', 'IniFileName',
    'wincmd.ini'));
  if Pos('\', Result)=0 then Insert('%windir%\', Result, 1);
  //если путь записан как ".\Wincmd.ini":
  if Pos('.\', Result)=1 then
    SReplace(Result, '.', tcDefDir);
  Result:= SExpandVars(Result);
end;


function IsFileUsed(const fn: string): boolean;
var
  h: THandle;
begin
  h:= CreateFile(PChar(fn), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result:= h=INVALID_HANDLE_VALUE;
  if not Result then CloseHandle(h);
end;


function IsFileExist(const fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
begin
  if fn='' then begin Result:= false; Exit end;
  h:= FindFirstFile(PChar(fn), fd);
  Result:= h<>INVALID_HANDLE_VALUE;
  if Result then Windows.FindClose(h);
end;


initialization
  LibClear;

finalization
  LibClear;

end.
