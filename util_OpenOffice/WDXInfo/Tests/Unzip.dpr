{$apptype console}

uses Windows, Unzip, UnzipDll;

var
  fn, dir: string;
begin
  fn:= ParamStr(1);
  if fn='' then begin Writeln('Parameter required'); Exit end;
  dir:= 'C:\Temp\Unzip.tmp';
  Writeln('Archive: ', fn);
  Writeln('Dir: ', dir);

  Writeln(Wiz_ErrorToStr(UnzipSingle(fn, dir, ['Thumbnails/thumbnail.png'])));
  //Writeln(Wiz_ErrorToStr(UnzipSingle(fn, dir, ['meta.xml', 'content.xml'])));
  //Writeln(Wiz_ErrorToStr(UnzipAll(fn, dir)));
end.
