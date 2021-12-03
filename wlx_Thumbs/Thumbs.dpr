{$E wlx}

library Thumbs;

{$R *.RES}

// SysUtils is used mainly because it handles exceptions, you may try
// to comment its usage (plugin size will decrease by ~20Kb)
uses
  Windows, SysUtils, DescPlugin, SProc, BitmapProc;

//--------------------------------------------
function ListLoad(ListerWin: HWND; FileToLoad: PChar; ShowFlags: integer): HWND; stdcall;
begin
  Result:= 0;
end;

//--------------------------------------------
function ListGetPreviewBitmap(FileToLoad: PChar; Width, Height: integer;
  Buf: PChar; BufLen: integer): HBitmap; stdcall;
//var
//  bmp: HBitmap;
begin
  Result:= 0;
  try
    Result:= GetPluginThumb(FileToLoad, Width, Height);
    ResizeBitmap(Result, Width, Height);
    
    //bmp:= CopyBitmap(Result);
    //DeleteObject(Result);
    //Result:= bmp;

    //ShowBitmap(FileToLoad, Result);
  except
    DeleteObject(Result);
    Result:= 0;
    MessageBox(0, PChar('Exception while reading thumbnail:'#13+FileToLoad), 'File Thumbnails', MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
  end;
end;

//--------------------------------------------
procedure InitDescPlugins(const fnIni: string);
var
  fn, sDetect: string;
  i: integer;
begin
  for i:= 0 to 30 do
    begin
    fn:= GetIniKey('Plugins', IntToStr(i), '', fnIni);
    if fn<>'' then
      begin
      if Pos(':\', fn)=0 then //don't modify absolute path
        fn:= ExtractFilePath(GetPluginFilename)+'Plugins\'+fn;
      sDetect:= GetIniKey('Plugins', IntToStr(i)+'_detect', '', fnIni);
      InitDescPlugin(fn, sDetect, ''{sFormat is not needed here});
      //MessageBox(0, PChar(fn), PChar(fMainCaption), MB_OK or MB_SETFOREGROUND);
      end;
    end;
end;

//--------------------------------------------
exports
  ListLoad,
  ListGetPreviewBitmap;

var
  fnIni: string;
begin
  fMainCaption:= 'File Thumbnails';
  fnIni:= ChangeFileName(GetPluginFilename, 'Thumbs.ini');
  InitDescPlugins(fnIni);

end.
