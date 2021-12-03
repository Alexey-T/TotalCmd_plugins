{$E wlx}

library OOView;

{$R *.RES}

uses
  SysUtils, Windows, StdCtrls,
  SProc, WLXPlugin,
  OOThumbnail,
  UFormMain in 'UFormMain.pas' {FormMain},
  UFormOptions in 'UFormOptions.pas' {FormOptions},
  UFormProgress in 'UFormProgress.pas' {FormProgress};

const
  _DetectString: PAnsiChar =
    'EXT="ODT" | EXT="ODS" | EXT="ODP" | EXT="ODG" | EXT="ODF" | EXT="ODB" | EXT="ODM" | '+
    'EXT="OTT" | EXT="OTH" | EXT="OTS" | EXT="OTG" | EXT="OTP" | '+
    'EXT="SXW" | EXT="SXC" | EXT="SXG" | EXT="SXI" | EXT="SXD" | EXT="SXM" | '+
    'EXT="STW" | EXT="STC" | EXT="STD" | EXT="STI"';

procedure ListGetDetectString(DetectString: PAnsiChar; MaxLen: integer); stdcall;
begin
  StrLCpyA(DetectString, _DetectString, MaxLen);
end;

function ListLoad(ListerWin: HWND;
  FileToLoad: PAnsiChar; ShowFlags: integer): HWND; stdcall;
begin
  Result:= ShowDoc(ListerWin, string(AnsiString(FileToLoad)), ShowFlags);
end;

function ListLoadW(ListerWin: HWND;
  FileToLoad: PWideChar; ShowFlags: integer): HWND; stdcall;
begin
  Result:= ShowDoc(ListerWin, WideString(FileToLoad), ShowFlags);
end;

procedure ListCloseWindow(PluginWin: HWND); stdcall;
begin
  HideDoc(PluginWin);
end;

function ListGetPreviewBitmap(
  FileToLoad: PAnsiChar;
  Width, Height: integer;
  Buf: PChar; BufLen: integer): HBitmap; stdcall;
begin
  Result:= 0;
end;

function ListGetPreviewBitmapW(
  FileToLoad: PWideChar;
  Width, Height: integer;
  Buf: PChar; BufLen: integer): HBitmap; stdcall;
begin
  Result:= 0;
  try
    Result:= GetOOoThumbnail(WideString(FileToLoad), Width, Height);
  except
    DeleteObject(Result);
    Result:= 0;
    MessageBox(0, PChar(Format(ssExceptionThumbnailMsg, [WideString(FileToLoad)])),
      ssCaption, MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
  end;
end;

function ListSendCommand(ListWin: THandle; Command, Parameter: integer): integer; stdcall;
var
  p: ^TPlugInfo;
  fm: TFormMain;
begin
  try
    p:= Pointer(GetWindowLong(ListWin, GWL_USERDATA));
    fm:= p^.PlugForm;
    case Command of
      lc_copy:
        fm.Memo1.CopyToClipboard;
      lc_selectall:
        fm.Memo1.SelectAll;
      {
      //Commented due to a bug in RichEdit:
      //it can show RTF trash after changing WordWrap value
      lc_newparams:
        with fm.Memo1 do
          begin
          PlainText:= true;
          WordWrap:= (Parameter and lcp_wraptext)<>0;
          if WordWrap
            then ScrollBars:= ssVertical
            else ScrollBars:= ssBoth;
        end;
      }
    end;
    Result:= LISTPLUGIN_OK;
  except
    Result:= LISTPLUGIN_ERROR;
  end;
end;

exports
  ListGetDetectString,
  ListLoad,
  ListCloseWindow,
  ListGetPreviewBitmap,
  ListGetPreviewBitmapW,
  ListSendCommand;

end.
