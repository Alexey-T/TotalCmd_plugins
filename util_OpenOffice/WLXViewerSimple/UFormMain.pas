{$WRITEABLECONST ON}

unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, ComCtrls;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    LabelTitle: TLabel;
    LabelSubject: TLabel;
    LabelDesc: TLabel;
    PopupMenu1: TPopupMenu;
    mnuOptions: TMenuItem;
    Memo1: TRichEdit;
    N1: TMenuItem;
    mnuSelectAll: TMenuItem;
    mnuCopy: TMenuItem;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuOptionsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
  private
    TotCmdWin: HWND;
    ParentWin: HWND;
    QuickView: boolean;
    procedure AppException(Sender: TObject; E: Exception);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor CreateParented(ParentWindow: HWND; const FileToView: string); reintroduce;
  end;

const
  ssCaption = 'OpenOffice.org Simple Viewer';
  ssTitle = 'Title: %s';
  ssSubject = 'Subject: %s';
  ssDescription = 'Description: %s';
  ssErrorMsg = 'Error opening document:'#13'%s';
  ssExceptionMsg = 'Exception:'#13'%s';
  ssExceptionDestroyMsg = 'Exception in DestroyWindow:'#13'%s';
  ssExceptionThumbnailMsg = 'Exception while reading thumbnail:'#13'%s';

function ShowDoc(ListerWin: HWND; const FileToLoad: string; ShowFlags: integer): HWND;
procedure HideDoc(PluginWin: HWND);

type
  TPlugInfo = record
    PlugWinProc: Pointer;
    PlugForm: TFormMain;
  end;

implementation

uses
  SProc, FProc, IniFile, OOData, XmlProc, UFormOptions, UFormProgress,
  PNGImage, BitmapProc, Unzip, UnzipDll, RichEdit;

{$R *.DFM}

var
  fShowMeta: boolean;
  fFontName: string;
  fFontSize: integer;
  fFontStyle: TFontStyles;
  fFontStyle2: integer absolute fFontStyle;
  fFontColor: integer;
  fFontCharset: TFontCharset;
  fBackColor: integer;

procedure MsgBox(h: HWND; const msg: string);
begin
  MessageBox(h, PChar(msg), ssCaption, MB_OK or MB_ICONINFORMATION);
end;

procedure MsgBoxErr(h: HWND; const msg: string);
begin
  MessageBox(h, PChar(msg), ssCaption, MB_OK or MB_ICONERROR);
end;

procedure TFormMain.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {
  if (Shift = [ssAlt]) and (Chr(Lo(Key)) = 'X') then begin
    Application.Handle := 0; //C++ GPF (Delphi AV)
    Application.RemoveComponent(Self);
    PostMessage(TotCmdWin, WM_SYSCOMMAND, SC_CLOSE, 0);
    exit;
  end;
  }
  if Shift=[] then
    begin
    if Key=VK_ESCAPE then
      begin
      if not QuickView
        then PostMessage(ParentWin, WM_KEYDOWN, VK_ESCAPE, 0)
        else PostMessage(ParentWin, WM_KEYDOWN, VK_TAB, 0);
      Key:= 0;
      end
    else
    if Chr(Lo(Key)) in ['N', 'P', 'W', '1'..'8'] then
      begin
      PostMessage(ParentWin, WM_KEYDOWN, Key, 0);
      Key:= 0;
      end;
    end;
  {
  if (Shift=[]) and (Key=VK_F1) then
    Help1.Click;
  if (Shift=[]) and (Key=VK_F2) then
    About1.Click;
  }
  Memo1.SetFocus;
end;

procedure TFormMain.AppException(Sender: TObject; E: Exception);
begin
  MsgBoxErr(Handle, Format(ssExceptionMsg, [E.Message]));
end;

procedure TFormMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style:= (WS_CHILD or WS_MAXIMIZE) and not WS_CAPTION and not WS_BORDER;
  Params.WindowClass.cbWndExtra:= SizeOf(Pointer); //4 bytes for address of form
end;

constructor TFormMain.CreateParented(ParentWindow: HWND; const FileToView: string);
const
  WinCmdClassName = 'TTOTAL_CMD';
begin
  inherited CreateParented(ParentWindow);
  TotCmdWin:= FindWindow(WinCmdClassName, nil);
  ParentWin:= ParentWindow;
  QuickView:= GetParent(ParentWin)<>0;
end;

function HookDestroy(PluginWin: HWND; Msg, wParam, lParam: LongInt): LongInt; stdcall;
var
  p: ^TPlugInfo;
begin
  p:= Pointer(GetWindowLong(PluginWin, GWL_USERDATA));
  if Msg<>WM_DESTROY
    then
      Result:= CallWindowProc(p^.PlugWinProc, PluginWin, Msg, wParam, lParam)
    else
      begin
      HideDoc(PluginWin);
      Result:= 0;
      end;
end;

procedure HideDoc(PluginWin: HWND);
var
  p: ^TPlugInfo;
begin
  p:= Pointer(GetWindowLong(PluginWin, GWL_USERDATA));
  with p^.PlugForm do
    try
      Application.RemoveComponent(p^.PlugForm);
      Application.Handle:= 0;
      //restore callback function
      SetWindowLong(Handle, GWL_WNDPROC, Integer(p^.PlugWinProc));
      Free;
    except
      on E: Exception do
        MsgBoxErr(Handle, Format(ssExceptionDestroyMsg, [E.Message]));
    end;
  Dispose(p);
end;

procedure Progress(const Msg: string; N: integer);
const
  OldMsg: string = '';
  OldN: integer = -1;
begin
  if (Msg=OldMsg) and (N=OldN) then Exit;
  OldMsg:= Msg;
  OldN:= N;
  with FormProgress do
    begin
    if N>0
      then Label1.Caption:= Format('%s... (%d%%)', [Msg, N])
      else Label1.Caption:= Format('%s...', [Msg]);
    ProgressBar1.Position:= N;
    end;
  Application.ProcessMessages;
end;

function ShowDoc(ListerWin: HWND; const FileToLoad: string; ShowFlags: integer): HWND;
var
  ok: boolean;
  fmMain: TFormMain;
  p: ^TPlugInfo;
begin
  try
    FormProgress:= TFormProgress.Create(nil);
    FormProgress.Show;
    XmlProgress:= Progress;
    ok:= GetOODataAndText(FileToLoad);
  finally
    FormProgress.Release;
    FormProgress:= nil;
  end;

  if not ok then
    begin
    MsgBoxErr(ListerWin, Format(ssErrorMsg, [FileToLoad]));
    Result:= 0;
    Exit
    end;

  try
    fmMain:= TFormMain.CreateParented(ListerWin, FileToLoad);
    {
    //Commented due to a bug in RichEdit with WordWrap changing
    with fmMain.Memo1 do
      begin
      WordWrap:= (ShowFlags and lcp_wraptext)<>0;
      if WordWrap
        then ScrollBars:= ssVertical
        else ScrollBars:= ssBoth;
      end;
    }
    fmMain.Show;

    //synchronize our form and Lister
    Application.Handle:= ListerWin;
    Application.OnException:= fmMain.AppException;
    Application.InsertComponent(fmMain);

    //substitution callback function
    New(p);
    SetWindowLong(fmMain.Handle, GWL_USERDATA, Integer(p));
    p^.PlugForm:= fmMain;
    p^.PlugWinProc:= Pointer(SetWindowLong(fmMain.Handle, GWL_WNDPROC, Integer(@HookDestroy)));

    //set focus to our window
    if not fmMain.QuickView then
      PostMessage(fmMain.Handle, WM_SETFOCUS, 0, 0);
    Result:= fmMain.Handle;

  except
    on E: Exception do
      begin
      MsgBoxErr(ListerWin, Format(ssExceptionMsg, [E.Message]));
      Result:= 0;
      end;
  end;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TFormMain.FormCreate(Sender: TObject);
  function SGet(var s: string): string;
  var
    k: integer;
  begin
    k:= Pos(',', s); if k=0 then k:= MaxInt;
    Result:= Copy(s, 1, k-1);
    Delete(s, 1, k);
  end;
var
  sFont: string;
begin
  fShowMeta:= boolean(GetIniKey('Options', 'ShowMeta', 1));
  sFont:= GetIniKey('Options', 'TextFont', 'Tahoma,10,0,0');
  fFontName:= SGet(sFont);
  fFontSize:= StrToIntDef(SGet(sFont), 10);
  fFontStyle2:= StrToIntDef(SGet(sFont), 0);
  fFontColor:= StrToIntDef(SGet(sFont), clBlack);
  fFontCharset:= StrToIntDef(SGet(sFont), integer(DEFAULT_CHARSET));
  fBackColor:= GetIniKey('Options', 'BackColor', clWhite);
  Panel1.Visible:= fShowMeta;
  Memo1.Font.Name:= fFontName;
  Memo1.Font.Size:= fFontSize;
  Memo1.Font.Style:= fFontStyle;
  Memo1.Font.Color:= fFontColor;
  Memo1.Font.Charset:= fFontCharset;
  Memo1.Color:= fBackColor;
end;

procedure TFormMain.mnuOptionsClick(Sender: TObject);
begin
  with TFormOptions.Create(nil) do
    try
      ffShowMeta.Checked:= fShowMeta;
      FontDialog1.Font.Name:= fFontName;
      FontDialog1.Font.Size:= fFontSize;
      FontDialog1.Font.Style:= fFontStyle;
      FontDialog1.Font.Color:= fFontColor;
      FontDialog1.Font.Charset:= fFontCharset;
      ColorDialog1.Color:= fBackColor;
      ShowModal;
      if ModalResult=mrOk then
        begin
        fShowMeta:= ffShowMeta.Checked;
        fFontName:= FontDialog1.Font.Name;
        fFontSize:= FontDialog1.Font.Size;
        fFontStyle:= FontDialog1.Font.Style;
        fFontColor:= FontDialog1.Font.Color;
        fFontCharset:= FontDialog1.Font.Charset;
        fBackColor:= ColorDialog1.Color;
        Panel1.Visible:= fShowMeta;
        Memo1.Font.Name:= fFontName;
        Memo1.Font.Size:= fFontSize;
        Memo1.Font.Style:= fFontStyle;
        Memo1.Font.Color:= fFontColor;
        Memo1.Font.Charset:= fFontCharset;
        Memo1.Color:= fBackColor;
        SetIniKey('Options', 'ShowMeta', integer(fShowMeta));
        SetIniKey('Options', 'TextFont', Format('%s,%d,%d,%d,%d', [fFontName, fFontSize, fFontStyle2, fFontColor, integer(fFontCharset)]));
        SetIniKey('Options', 'BackColor', fBackColor);
        end;
    finally
      Free;
    end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  SReplaceAll(foooDesc, #13, ' ');
  SReplaceAll(foooDesc, #10, ' ');
  LabelTitle.Caption:= Format(ssTitle, [foooTitle]);
  LabelSubject.Caption:= Format(ssSubject, [foooSubject]);
  LabelDesc.Caption:= Format(ssDescription, [foooDesc]);

  with Memo1 do
    begin
    //Apply font, to be sure that right font is set
    Text:= '';

    Font.Name:= 'Webdings';
    Font.Size:= 8;
    Font.Color:= clWhite;

    Font.Name:= fFontName;
    Font.Size:= fFontSize;
    Font.Style:= fFontStyle;
    Font.Color:= fFontColor;
    Font.Charset:= fFontCharset;

    //Put text
    Text:= foooTextList.Text;
    SelStart:= 0;
    end;
end;

procedure TFormMain.mnuSelectAllClick(Sender: TObject);
begin
  Memo1.SelectAll;
end;

procedure TFormMain.mnuCopyClick(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

initialization
  IniFilename:= ChangeFileName(GetPluginFilename, 'OOView.ini');

end.
