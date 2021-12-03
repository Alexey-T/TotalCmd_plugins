{$apptype console}

uses tcProc;

begin
  tcSetPaths('c:\tc\totalcmd.exe', 'c:\tc\tc_my.ini');
  tcSearchPlugins(false);

  tcLSSwap(2, 3);
end.
