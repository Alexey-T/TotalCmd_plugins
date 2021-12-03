{$IOCHECKS OFF}

unit LogProc;

interface

type
  TLogOperationType = (
    logRun,
    logInstall
  );

var
  OptLog: record
    Filename: string;
    Enable: boolean;
  end;

procedure LogMessage(OK: boolean; const Cmd: string);


implementation

uses
  Windows, SysUtils, Msg;

const
  cMsgOK: array[boolean] of string = ('Failed:', 'OK:');
  cMsgLogCannotOpen = 'Cannot open log file';


procedure LogMessage(OK: boolean; const Cmd: string);
var
  f: Text;
begin
  try
    if OptLog.Enable then
      begin
      if OptLog.Filename='' then
        begin
        MsgError(cMsgLogCannotOpen);
        Exit
        end;

      Assign(f, OptLog.Filename);
      Append(f);

      if IOResult<>0 then
        Rewrite(f);

      if IOResult<>0 then
        MsgError(cMsgLogCannotOpen+': '+OptLog.Filename)
      else
        begin
        Writeln(f, Format('[%s %s] %s %s', [DateToStr(Date), TimeToStr(Time), cMsgOK[OK], Cmd]));
        Close(f);
        end;
      end;
  except
  end;
end;

end.
