unit Msg;

interface

procedure MsgInfo(const Msg: string);
procedure MsgError(const Msg: string);
function MsgConfirmed(const Msg: string): boolean;

var
  ssIdle: string = 'idle';
  ssLoaded: string = 'loaded';
  ssRunning: string = 'running';
  ssDisabled: string = 'disabled';
  ssDeleted: string = 'deleted';
  ssLangAuthor: string = '';

var
  S0040,
  S0041,
  S0042,
  S0043,
  S0044,
  S0045,

  S0050,
  S0051,
  S0052,
  S0053,
  S0054,
  S0055,
  S0056,
  S0057,
  S0058,
  S0059,

  S0060,
  S0062,
  S0068,
  S0069,
  S0070,
  S0071,
  S0072,
  S0073,
  S0074,
  S0075,
  S0076,
  S0077,
  S0078,

  S0080,
  S0081,
  S0082,
  S0083,
  S0084,
  S0085,
  S0086,
  S0087,
  S0088,
  S0089,
  S0090,
  S0100,
  S0000: string;

procedure MsgUpdate;


implementation

uses
  Windows, Messages, MsgFile, Forms;

procedure MsgUpdate;
begin
  ssIdle:= MsgString(0020);
  ssLoaded:= MsgString(0021);
  ssRunning:= MsgString(0022);
  ssDisabled:= MsgString(0023);
  ssDeleted:= MsgString(0024);

  S0040:= MsgString(0040);
  S0041:= MsgString(0041);
  S0042:= MsgString(0042);
  S0043:= MsgString(0043);
  S0044:= MsgString(0044);
  S0045:= MsgString(0045);

  S0050:= MsgString(0050);
  S0051:= MsgString(0051);
  S0052:= MsgString(0052);
  S0053:= MsgString(0053);
  S0054:= MsgString(0054);
  S0055:= MsgString(0055);
  S0056:= MsgString(0056);
  S0057:= MsgString(0057);
  S0058:= MsgString(0058);
  S0059:= MsgString(0059);

  S0060:= MsgString(0060);
  S0062:= MsgString(0062);
  S0068:= MsgString(0068);
  S0069:= MsgString(0069);
  S0070:= MsgString(0070);
  S0071:= MsgString(0071);
  S0072:= MsgString(0072);
  S0073:= MsgString(0073);
  S0074:= MsgString(0074);
  S0075:= MsgString(0075);
  S0076:= MsgString(0076);
  S0077:= MsgString(0077);
  S0078:= MsgString(0078);

  S0080:= MsgString(0080);   
  S0081:= MsgString(0081);   
  S0082:= MsgString(0082);   
  S0083:= MsgString(0083);   
  S0084:= MsgString(0084);   
  S0085:= MsgString(0085);   
  S0086:= MsgString(0086);   
  S0087:= MsgString(0087);   
  S0088:= MsgString(0088);   
  S0089:= MsgString(0089);   
  S0090:= MsgString(0090);   

  S0100:= MsgString(0100);
end;


function MsgBox(const Msg: string; Flags: integer): integer;
begin
  Result:= Application.MessageBox(PChar(Msg), PChar(S0051), Flags or MB_SETFOREGROUND);
end;

procedure MsgInfo(const Msg: string);
begin
  MsgBox(Msg, MB_OK or MB_ICONINFORMATION);
end;

procedure MsgError(const Msg: string);
begin
  MsgBox(Msg, MB_OK or MB_ICONERROR);
end;

function MsgConfirmed(const Msg: string): boolean;
begin
  Result:= MsgBox(Msg, MB_OKCANCEL or MB_ICONQUESTION) = IDOK;
end;


initialization

  MsgUpdateProc:= MsgUpdate;

end.
