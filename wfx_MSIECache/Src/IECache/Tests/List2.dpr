{$apptype console}
uses Windows, WinInet, CacheObj, FProc, RegProc, SysUtils, SysUtils1;

var
  List: PFileList;
  f: Text;
begin
  {
  exe:= GetRegKeyStr(HKEY_LOCAL_MACHINE,
    'Software\Microsoft\Windows\CurrentVersion\App Paths\IEXPLORE.EXE', '', '');
  ExecShell(exe, 'http://delphi.about.com/favicon.ico', SW_SHOW, false);
  }

  AssignFile(f, ChangeFileExt(ParamStr(0), '.txt'));
  Rewrite(f);

  Cache.ReadCache;

  Writeln(f, 'FileList:');
  List:= New(PFileList, Init);
  List.ReadCache(0, '');
  List.WriteAll(f);
  Dispose(List, Done);

  Writeln(f);
  Writeln(f, 'yandex.ru:');
  List:= New(PFileList, Init);
  List.ReadCache(1, 'yandex.ru');
  List.WriteAll(f);
  Dispose(List, Done);

  Writeln(f);
  Writeln(f, 'kiev.ua:');
  List:= New(PFileList, Init);
  List.ReadCache(1, 'kiev.ua');
  List.WriteAll(f);
  Dispose(List, Done);

  Writeln(f, 'Done');
  CloseFile(f);

end.
