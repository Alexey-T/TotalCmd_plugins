{$E wlx}

library OOoViewer;

{$R *.RES}

uses
  SysUtils, Windows,
  SProc, FProc,
  WLXPlugin, WBProc, //OOThumbnail,
  UFormMain in 'UFormMain.pas' {FormMain};

const
  cDetectString: PAnsiChar =
    'EXT="ODT" | EXT="OTT" | ' +
    'EXT="SXW" | EXT="STW" | EXT="SXC" | EXT="STC" | EXT="SXD" | EXT="STD" | ' +
    'EXT="DOCX" | EXT="DOTX" | ' +
    'EXT="FB2" | EXT="FB2Z" | EXT="ZFB2" | EXT="FBZ"';


function ClassNameOf(hWnd: THandle): string;
var
  Buf: array[0..200] of char;
begin
  FillChar(Buf, SizeOf(Buf), 0);
  GetClassName(hWnd, Buf, SizeOf(Buf));
  Result:= Buf;
end;

procedure ListGetDetectString(DetectString: PAnsiChar; MaxLen: integer); stdcall;
begin
  StrLCpyA(DetectString, cDetectString, MaxLen);
end;

function ListLoad(ListerWin: HWND;
  FileToLoad: PAnsiChar;
  ShowFlags: integer): HWND; stdcall;
var
  OptAllow,
  IsLister: boolean;
begin
  Result:= 0;

  OptAllow:= IniFile.ReadBool('Options', 'AllowWorkInLister', true);
  IsLister:= (GetParent(ListerWin) = 0) and (ClassNameOf(ListerWin) = 'TLister');
  if (not OptAllow) and IsLister then Exit;

  Result:= ShowDoc(ListerWin, AnsiString(FileToLoad), ShowFlags);
end;

procedure ListCloseWindow(PluginWin: HWND); stdcall;
begin
  HideDoc(PluginWin);
end;

(*
function ListGetPreviewBitmap(FileToLoad: PChar; Width, Height: integer;
  Buf: PChar; BufLen: integer): HBitmap; stdcall;
begin
  try
    Result:= GetOOoThumbnail(FileToLoad, Width, Height);
  except
    Result:= 0;
    DeleteObject(Result);
    MsgBoxErr(0, Format(ssExceptionThumbnailMsg, [string(FileToLoad)]));
  end;
end;
*)

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
        begin
        WB_Copy(fm.Browser1);
        WB_SetFocus(fm.Browser1);
        end;
      lc_selectall:
        begin
        WB_SelectAll(fm.Browser1);
        WB_SetFocus(fm.Browser1);
        end;
    end;
    Result:= LISTPLUGIN_OK;
  except
    Result:= LISTPLUGIN_ERROR;
  end;
end;

function ListPrint(ListWin: THandle;
  FileToPrint, DefPrinter: pAnsiChar;
  PrintFlags: integer; var Margins: TRect): integer; stdcall;
var
  p: ^TPlugInfo;
  fm: TFormMain;
begin
  try
    p:= Pointer(GetWindowLong(ListWin, GWL_USERDATA));
    fm:= p^.PlugForm;
    WB_ShowPrintDialog(fm.Browser1);
    Result:= LISTPLUGIN_OK;
  except
    Result:= LISTPLUGIN_ERROR;
  end;
end;

function ListSearchDialog(ListWin: THandle; FindNext: integer): integer; stdcall;
var
  p: ^TPlugInfo;
  fm: TFormMain;
begin
  Result:= LISTPLUGIN_OK;
  try
    p:= Pointer(GetWindowLong(ListWin, GWL_USERDATA));
    fm:= p^.PlugForm;
    WB_ShowFindDialog(fm.Browser1);
  except
  end;
end;

exports
  ListGetDetectString,
  ListLoad,
  ListCloseWindow,
  //ListGetPreviewBitmap,
  ListSendCommand,
  ListPrint,
  ListSearchDialog;

end.
