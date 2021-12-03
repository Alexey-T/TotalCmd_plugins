unit FProc;

interface

uses
  Windows, SysUtils,
  Process,
  UTF8Process;

type
  TRunResult = (run_Ok, run_CannotRun, run_Exception);

function DoRunProcess(const CmdLine, CurrentDir: string): TRunResult;
procedure DoErrorMessage(const S: string);
function GetPluginFilename: string;
function DoExpandVars(const s: string): string;


implementation

function DoExpandVars(const s: string): string;
var
  buf: array[0..4*1024-1] of char;
begin
  SetString(Result, buf, ExpandEnvironmentStrings(PChar(s), buf, SizeOf(buf))-1);
end;


function DoRunProcess(const CmdLine, CurrentDir: string): TRunResult;
var
  P: TProcessUTF8;
begin
  P:= TProcessUTF8.Create(nil);
  try
    P.ShowWindow:= swoHIDE;
    P.CurrentDirectory:= CurrentDir;
    P.CommandLine:= CmdLine;
    try
      P.Execute;
      if P.WaitOnExit then
        Result:= run_Ok
      else
        Result:= run_CannotRun;
    except
      Result:= run_Exception;
    end;
  finally
    FreeAndNil(P);
  end;
end;


function GetPluginFilename: string;
begin
  Result:= GetModuleName(System.HINSTANCE);
end;

procedure DoErrorMessage(const S: string);
begin
  MessageBoxW(0, PWChar(WideString(UTF8Decode(S))), 'TextSearch plugin',
    MB_OK or MB_ICONERROR or MB_TASKMODAL);
end;


end.
