{$apptype console}

uses Windows, FProc, tcProc;


procedure _Report(const fn: string);
var
  f: Text;
begin
  Assign(f, fn);
  Rewrite(f);
  if IOResult<>0 then begin Writeln('Cannot create ', fn); Exit end;

  tcSearchPlugins(true);
  tcReport(f);
  Close(f);
end;


const
  fn_report = '_Report.txt';
var
  fn: string;
begin
  Writeln('TC plugins list (test from TC Plugman 1.7)');
  if ParamCount<1 then
    begin
    Writeln('Usage:');
    Writeln('tcList <Totalcmd.exe>');
    Writeln('tcList <Totalcmd.exe> <Wincmd.ini>');
    Exit
    end;

  fn:= ParamStr(1);
  if not IsFileExist(fn) then begin Writeln('File not found: ', fn); Exit end;
  fn:= ParamStr(2);
  if fn='' then fn:= tcDefIni;
  if not IsFileExist(fn) then begin Writeln('File not found: ', fn); Exit end;


  tcSetPaths(ParamStr(1), ParamStr(2));
  _Report(fn_report);
  Writeln('See '+fn_report);
  ExecShell(fn_report, '', SW_SHOW, false);

  
  
  Exit;
  {
  fn:= 'C:\TC\TCplugins\MSI.wcx';
  Writeln;
  Writeln(fn);
  if tcInstalled(fn, fn) then
    begin
    Writeln('Installed');
    Writeln('Params: ', fn);
    end
  else Writeln('Not installed');
  }
end.
