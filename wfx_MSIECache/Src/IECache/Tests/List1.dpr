{$apptype console}
uses Windows, WinInet, CacheObj, SysUtils;

var
  List: PFileList;
  f: Text;
begin
  AssignFile(f, ChangeFileExt(ParamStr(0), '.txt'));
  Rewrite(f);

  Writeln('Cache items num: ', GetCacheItemsNum);

  Cache.ReadCache;
  //Writeln(f, 'Size: ', Cache.GetSize);
  Writeln(f, 'Cache:');
  Cache.WriteAll(f);
  Writeln(f, 'Done');

  CloseFile(f);

  Readln;
end.
