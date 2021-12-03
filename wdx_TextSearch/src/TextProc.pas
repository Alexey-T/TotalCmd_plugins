unit TextProc;

interface

type
  TTextManager = class
    private
      FFileName: string;
      FTempPath: string;
      FTextW: UnicodeString;
      FLogFileName: string;
      FLogEnabled: boolean;
      FBoxEnabled: boolean;
      FDeleteOutFile: boolean;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure Message(const S: string);
      procedure InitLogging;
      function ReadFile(const fn: string): boolean;
      property TextW: UnicodeString read FTextW;
    end;

var
  TextObj: TTextManager;
  ConfigIni: string;


implementation

uses
  SysUtils, SProc, SConvert,
  FileUtil, FProc;

//TTextManager
constructor TTextManager.Create;
begin
  Clear;
  FTempPath:= GetTempDir;
end;

destructor TTextManager.Destroy;
begin
  Clear;
end;

procedure TTextManager.Clear;
begin
  FFileName:= '';
  FTextW:= '';
end;


//Search for '{Macro:xxxxx}' substring and returning of 'xxxxx' part
function CmdMacroParam(var Cmd: string; const Macro: string): string;
var
  N, N2: integer;
begin
  Result:= '';
  N:= Pos('{' + Macro + ':', Cmd);
  if N > 0 then
    begin
    N2:= PosFrom('}', Cmd, N);
    Result:= Copy(Cmd, N + Length(Macro) + 2, N2 - N - Length(Macro) - 2);
    Delete(Cmd, N, N2 - N + 1);
    end;
end;

function SCheckAndDelete(var S: string; const SubStr: string): boolean;
var
  n: integer;
begin
  n:= Pos(SubStr, S);
  Result:= n > 0;
  if Result then
    Delete(S, n, Length(SubStr));
end;


procedure TTextManager.Message(const S: string);
var
  f: System.Text;
begin
  if FBoxEnabled then
    DoErrorMessage(S);

  if FLogEnabled then
    begin
    {$I-} //required to not crash
    AssignFile(f, FLogFileName);
    Append(f);
    if IOResult <> 0 then
      Rewrite(f);
    if IOResult <> 0 then Exit;
    Writeln(f, Format('%s %s: File "%s" : %s', [DateToStr(Date), TimeToStr(Time), FFileName, S]));
    CloseFile(f);
    end;
end;

procedure TTextManager.InitLogging;
begin
  FLogFileName:= FTempPath+'TextSearch.log';
  FLogEnabled:= GetIniKey('Options', 'Log', '0', ConfigIni)='1';
  FBoxEnabled:= GetIniKey('Options', 'ShowErrors', '1', ConfigIni)='1';
  FDeleteOutFile:= GetIniKey('Options', 'DeleteOutFile', '1', ConfigIni)='1';
end;


function TTextManager.ReadFile(const fn: string): boolean;
const
  sTempName = 'TextSrch.txt'; //8.3
var
  FText, Cmd, Ext, Dir, Out, OutShort, S: string;
  CPs: set of TMyCodepage;
  CP: TMyCodepage;
  ParamHome: string;
  CmdRequired: boolean;
  i: integer;
begin
  Result:= false;

  FFileName:= fn;
  FText:= '';
  FTextW:= '';

  if not FileExists(FFileName) then
    Exit;

  //----------------------------------------------------------
  //Init logging
  InitLogging;

  //----------------------------------------------------------
  //Search for converter

  Ext:= ExtractFileExt(FFileName);
  if Ext <> '' then
    Delete(Ext, 1, 1);

  Cmd:= GetIniKey('Converters', Ext, '', ConfigIni);
  CmdRequired:= false;

  //Extension points to another extension:
  if (Cmd <> '') and (Pos(' ', Cmd) = 0) and (Pos('{', Cmd) = 0) then
    begin
    Cmd:= GetIniKey('Converters', Cmd, '', ConfigIni);
    CmdRequired:= true;
    end
  else
  //Try to find the '*' converter:
  if (Cmd = '') then
    begin
    Cmd:= GetIniKey('Converters', '*', '', ConfigIni);
    end;

  if Cmd='' then
    begin
    if CmdRequired then
      Message(Format('Cannot find specified converter for "%s".', [Ext]));
    Exit
    end;

  Dir:= ExtractFileDir(GetPluginFilename);
  Out:= FTempPath+sTempName;
  OutShort:= ExtractShortPathName(FTempPath)+sTempName;

  //----------------------------------------------------------
  //Process macros

  //{CP:xxxx}

  CPs:= [];
  repeat
    S:= CmdMacroParam(Cmd, 'CP');
    if S = '' then Break;
    CP:= CodepageStringToCodepageId(S);
    if CP = cpUnknown then
      begin
      Message(Format('Unknown codepage specified for "%s" converter: "%s".', [Ext, S]));
      Exit
      end;
    Include(CPs, CP);
  until false;

  //{Home:xxxx}

  ParamHome:= CmdMacroParam(Cmd, 'Home');
  ParamHome:= DoExpandVars(ParamHome);

  //{In}, {Out} etc

  SReplaceI(Cmd, '{In}', FFileName);
  SReplaceI(Cmd, '{InShort}', ExtractShortPathName(FFileName));
  SReplaceI(Cmd, '{Out}', Out);
  SReplaceI(Cmd, '{OutShort}', OutShort);

  Cmd:= DoExpandVars(Cmd);
  Cmd:= Trim(Cmd); //Need to trim when multiple CPs specified

  //----------------------------------------------------------
  //Run converter or read file directly

  if Pos(' ', Cmd) > 0 then
    //Run converter
    try
      DeleteFile(Out);
      SetCurrentDir(Dir);

      if ParamHome <> '' then
        Dir:= ParamHome;

      case DoRunProcess(Cmd, Dir) of
        run_CannotRun:
          begin
          Message(Format('Cannot run converter for "%s".'#13'Command: "%s".', [Ext, Cmd]));
          Exit
          end;
        run_Exception:
          begin
          Message(Format('Converter exception for "%s".'#13'Command: "%s".', [Ext, Cmd]));
          Exit
          end;
      end;

      FText:= ReadFileToString(Out);
      if FText='' then
        begin
        Message(Format('Cannot convert file "%s" to "%s".'#13'Command: "%s".', [FFileName, Out, Cmd]));
        Exit
        end;
    finally
      if FDeleteOutFile then
        DeleteFile(Out);
    end
  else
    //Read directly
    begin
    FText:= ReadFileToString(FFileName);
    if FText='' then
      begin
      Message(Format('Cannot read file "%s".', [FFileName]));
      Exit
      end;
    end;

  //----------------------------------------------------------
  //Perform the decoding

  if CPs <> [] then
    begin
    S:= FText;
    FText:= '';
    for CP in TMyCodepage do
      if CP in CPs then
        FText:= FText + Conv_AnyCodepage(S, CP) + #13#10;
    end;

  FTextW:= UTF8Decode(FText);

  //----------------------------------------------------------
  //Delete zeroes (file may be binary)
  for i:= 1 to Length(FTextW) do
    if FTextW[i]=#0 then
      FTextW[i]:= ' ';

  Result:= true;
end;


var
  SampleIni: string;

initialization
  ConfigIni:= ExtractFilePath(GetPluginFilename)+'TextSearch.ini';
  SampleIni:= ExtractFilePath(GetPluginFilename)+'TextSearch.Sample.ini';
  if not FileExists(ConfigIni) and FileExists(SampleIni) then
    CopyFile(SampleIni, ConfigIni);
  TextObj:= TTextManager.Create;

finalization
  FreeAndNil(TextObj);

end.
