{$Apptype console}

uses Windows, SysUtils, tcProc, ContPlug, SProc, ComCtrls;

{$I wdxProc.pas}

var
  i: integer;
  fFile: string;
begin
  if ParamCount<2 then
    begin MessageBox(0,
      'Program to show fields supported by WDX plugins. 24/11/04 by Alextp'#13+
      'Usage: WdxList2.exe "Totalcmd.ini" "Filename"',
      'About', MB_OK or MB_ICONINFORMATION); Exit end;

  Writeln('TC ini: ', ParamStr(1));
  Writeln('Filename: ', ParamStr(2));

  tcSetPaths('C:\TC\TC.exe', ParamStr(1));
  tcSearchPlugins(true);

  for i:= 1 to libNum do
   with libList[i] do
    if (fType=fContent) and (not fDisabled) then
     begin
     Writeln;
     Writeln(Format('Plugin%d: "%s"', [fIndex, fn]));

     fFile:= ParamStr(2);
     WdxList(fn, fFile, nil, 'C:\TC\ContPlug.ini');
     //Exec(Format('WdxList.exe "%s" "%s"', [fn, fFile]), SW_SHOW, true);
     end;

  Exit;
  Writeln('Enter...');
  Readln;
end.
