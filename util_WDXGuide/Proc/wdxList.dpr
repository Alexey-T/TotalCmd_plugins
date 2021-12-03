{$Apptype console}
uses Windows, SysUtils, ContPlug, SProc, ComCtrls;

{$I wdxProc.pas}


begin
  if ParamCount<2 then
    begin MessageBox(0,
      'Usage: WdxList "Plugin.wdx" "Filename"',
      'About', MB_OK or MB_ICONINFORMATION); Exit end;

  WdxList(ParamStr(1), ParamStr(2), nil, 'C:\ContPlug.ini');
end.
