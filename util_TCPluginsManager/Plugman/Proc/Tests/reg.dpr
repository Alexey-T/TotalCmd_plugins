{apptype console}

uses Windows, SysUtils, RegProc, tcProc;

begin
  tcRegisterExtension('.wcx', true);
  tcRegisterExtension('.wlx', true);
  tcRegisterExtension('.wfx', true);
  if ParamCount>0 then
    MessageBox(0, PChar(ParamStr(1)), 'Reg', MB_OK);
end.
