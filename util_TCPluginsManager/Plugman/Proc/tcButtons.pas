unit tcButtons;

interface

//function tcButtonsNum(const fn: string): integer;
procedure tcButtonDelete(const fn, cmd: string);
procedure tcButtonAppend(const fn, cmd, cmd_params, cmd_desc: string; iconnum: integer);
procedure tcButtonAppendSeparator(const fn: string);

implementation

uses Windows, SysUtils, tcIniProc;


function GetOption(const fn, key: string): string;
begin
  Result:= GetTcIniKey(fn, 'Buttonbar', key);
end;

procedure SetOption(const fn, key, value: string);
begin
  SetTcIniKey(fn, 'Buttonbar', key, value);
end;


function tcButtonsNum(const fn: string): integer;
begin
  Result:= StrToIntDef(GetOption(fn, 'Buttoncount'), 0);
end;

function ButtonIndex(const fn, cmd: string): integer; overload;
var
  i: integer;
  s: string;
begin
  for i:= 1 to tcButtonsNum(fn) do
    begin
    s:= GetOption(fn, Format('cmd%d', [i]));
    if Pos(LowerCase(cmd), LowerCase(s))>0 then
      begin Result:= i; Exit end;
    end;
  Result:= 0;
end;

function ButtonIndex(const fn, cmd, cmd_params: string): integer; overload;
var
  i: integer;
  s, ss: string;
begin
  for i:= 1 to tcButtonsNum(fn) do
    begin
    s:= GetOption(fn, Format('cmd%d', [i]));
    ss:= GetOption(fn, Format('param%d', [i]));
    if (StrIComp(PChar(cmd), PChar(s))=0) and
       (StrIComp(PChar(cmd_params), PChar(ss))=0) then
      begin Result:= i; Exit end;
    end;
  Result:= 0;
end;


procedure ButtonChange(const fn, cmd, cmd_params, cmd_desc: string; iconnum: integer);
var
  i_need: integer;
begin
  i_need:= ButtonIndex(fn, cmd);
  if i_need=0 then
    begin
    i_need:= tcButtonsNum(fn)+1;
    SetOption(fn, 'Buttoncount', IntToStr(i_need));
    end;

  SetOption(fn, Format('button%d', [i_need]), Format('%s,%d', [cmd, iconnum]));
  SetOption(fn, Format('cmd%d', [i_need]), cmd);
  SetOption(fn, Format('menu%d', [i_need]), cmd_desc);
  SetOption(fn, Format('param%d', [i_need]), cmd_params);
end;


procedure tcButtonAppend(const fn, cmd, cmd_params, cmd_desc: string; iconnum: integer);
var
  i_need: integer;
  s: string;
begin
  i_need:= tcButtonsNum(fn)+1;
  SetOption(fn, 'Buttoncount', IntToStr(i_need));
  if iconnum=0 then s:= cmd else s:= Format('%s,%d', [cmd, iconnum]);
  SetOption(fn, Format('button%d', [i_need]), s);
  SetOption(fn, Format('cmd%d', [i_need]), cmd);
  SetOption(fn, Format('menu%d', [i_need]), cmd_desc);
  SetOption(fn, Format('param%d', [i_need]), cmd_params);
end;

procedure tcButtonAppendSeparator(const fn: string);
var
  n: integer;
begin
  n:= tcButtonsNum(fn);
  if GetOption(fn, Format('cmd%d', [n]))<>'' then
    tcButtonAppend(fn, '', '', '', 0);
end;


procedure tcButtonDelete(const fn, cmd: string);
var
  i, i_need, n: integer;
begin
  repeat
    n:= tcButtonsNum(fn);
    i_need:= ButtonIndex(fn, cmd);
    if i_need=0 then Exit;

    for i:= i_need to n-1 do
      begin
      SetOption(fn, Format('button%d', [i]), GetOption(fn, Format('button%d', [i+1])));
      SetOption(fn, Format('cmd%d', [i]),    GetOption(fn, Format('cmd%d', [i+1])));
      SetOption(fn, Format('menu%d', [i]),   GetOption(fn, Format('menu%d', [i+1])));
      SetOption(fn, Format('param%d', [i]),  GetOption(fn, Format('param%d', [i+1])));
      end;

    SetOption(fn, Format('button%d', [n]), '');
    SetOption(fn, Format('cmd%d', [n]), '');
    SetOption(fn, Format('menu%d', [n]), '');
    SetOption(fn, Format('param%d', [n]), '');
    SetOption(fn, 'Buttoncount', IntToStr(n-1));
  until false;
end;


end.
