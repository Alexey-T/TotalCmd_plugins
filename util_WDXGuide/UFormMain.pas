unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, XPMan, Menus, TntComCtrls, TntStdCtrls,
  TntDialogs;

type
  TForm1 = class(TForm)
    OpenDialog1: TTntOpenDialog;
    Panel1: TGroupBox;
    Label5: TLabel;
    sTcIni: TEdit;
    btnBrowseIni: TButton;
    Panel2: TGroupBox;
    Label2: TLabel;
    sFilename: TTntEdit;
    btnBrowse: TButton;
    btnClose: TButton;
    OpenDialog2: TOpenDialog;
    Image1: TImage;
    sTCExe: TEdit;
    Label4: TLabel;
    btnBrowseExe: TButton;
    OpenDialog3: TOpenDialog;
    Panel3: TPanel;
    Panel4: TPanel;
    ListView1: TTntListView;
    XPManifest1: TXPManifest;
    PopupMenu1: TPopupMenu;
    mnuCopy1: TMenuItem;
    mnuCopy2: TMenuItem;
    mnuCopy3: TMenuItem;
    Label3: TLabel;
    chkText: TCheckBox;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    Label1: TLabel;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnBrowseIniClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseExeClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mnuCopy1Click(Sender: TObject);
    procedure mnuCopy2Click(Sender: TObject);
    procedure mnuCopy3Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure chkTextClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadWdx;
    function FName(I: Integer): string;
    function DefIni: string;
    function PluginName: string;
    function TestName: WideString;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ActiveX,
  tcProc, SProc, IniFile,
  wdxProc, TntClipbrd, TntSystem, UFormMemo;

{$R *.DFM}


procedure TForm1.btnBrowseClick(Sender: TObject);
begin
  with OpenDialog1 do
    begin
    InitialDir:= ExtractFileDir(sFilename.Text);
    if Execute then
      begin
      sFilename.Text:= Filename;
      LoadWdx;
      end;
    end;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnBrowseIniClick(Sender: TObject);
begin
  with OpenDialog2 do
    begin
    InitialDir:= ExtractFileDir(sTcIni.Text);
    if Execute then
      begin
      sTcIni.Text:= Filename;
      FormShow(Self);
      end;
    end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: integer;
begin
  if not FileExists(sTcExe.Text) then Exit;
  if not FileExists(sTcIni.Text) then Exit;
  tcSetPaths(sTCExe.Text, sTcIni.Text);
  tcSearchPlugins(true);

  Listbox1.Items.Clear;
  for i:= 1 to libNum do
   with libList[i] do
    if (fType=fContent) and (not fDisabled) then
      begin
      Listbox1.Items.Add(ChangeFileExt(ExtractFileName(fn), ''));
      end;

  with Listbox1 do
    ItemIndex:= Items.IndexOf(GetIniKey('Window', 'TabS', ''));
  LoadWdx;    
end;

function TForm1.DefIni: string;
begin
  Result:= ExtractFilePath(sTcIni.Text) + 'ContPlug.ini';
end;

function TForm1.TestName: WideString;
begin
  Result:= sFilename.Text;
end;

function TForm1.PluginName: string;
var
  i: integer;
begin
  Result:= '';
  for i:= 1 to libNum do
   with libList[i] do
    if (fType = fContent) and (not fDisabled)
      and (fIndex = Listbox1.ItemIndex) then
        begin Result:= fn; Break end;
end;

procedure TForm1.LoadWdx;
begin
  if (TestName<>'') and (PluginName<>'') then
    WdxList(
      PluginName,
      TestName,
      DefIni,
      ListView1,
      chkText.Checked,
      -1);
end;

procedure TForm1.ListChange(Sender: TObject);
begin
  LoadWdx;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  IniFilename:= ChangeFileExt(ParamStr(0), '.ini');
  sTcExe.Text:= GetIniKey('Paths', 'TCExe', tcDefExe);
  sTcIni.Text:= GetIniKey('Paths', 'TCIni', tcDefIni);
  sFilename.Text:= UTF8Decode(GetIniKey('Paths', 'TestFile', ''));

  Left:= GetIniKey('Window', 'Left', Left);
  Top:= GetIniKey('Window', 'Top', Top);
  Width:= GetIniKey('Window', 'Width', Width);
  Height:= GetIniKey('Window', 'Height', Height);

  with ListView1 do
    for i:= 0 to Columns.Count-1 do
      Columns[i].Width:= GetIniKey('Window', Format('Column%dW', [i]), Columns[i].Width);

  Listbox1.Width:= GetIniKey('Window', 'TabWidth', Listbox1.Width);
  Font.Size:= GetIniKey('Window', 'FontSize', Font.Size);
  chkText.Checked:= boolean(GetIniKey('Opt', 'ShowText', 0));

  if ParamCount>0 then
    sFilename.Text:= WideParamStr(1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  SetIniKey('Paths', 'TCExe', sTcExe.Text);
  SetIniKey('Paths', 'TCIni', sTcIni.Text);
  SetIniKey('Paths', 'TestFile', UTF8Encode(TestName));
  SetIniKey('Window', 'Left', Left);
  SetIniKey('Window', 'Top', Top);
  SetIniKey('Window', 'Width', Width);
  SetIniKey('Window', 'Height', Height);
  
  with ListView1 do
    for i:= 0 to Columns.Count-1 do
      SetIniKey('Window', Format('Column%dW', [i]), Columns[i].Width);

  SetIniKey('Opt', 'ShowText', integer(chkText.Checked));
  SetIniKey('Window', 'TabWidth', Listbox1.Width);
  SetIniKey('Window', 'FontSize', Font.Size);
  with Listbox1 do
   if ItemIndex >= 0 then
    SetIniKey('Window', 'TabS', Items[ItemIndex]);
end;

procedure TForm1.btnBrowseExeClick(Sender: TObject);
begin
  with OpenDialog3 do
    begin
    InitialDir:= ExtractFileDir(sTcExe.Text);
    if Execute then
      begin
      sTcExe.Text:= Filename;
      FormShow(Self);
      end;
    end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
  if (Key = Ord('A')) and (Shift = [ssCtrl]) then
    ListView1.SelectAll;
  if Key = VK_RETURN then
    ListView1DblClick(Self);
end;

function TForm1.FName;
begin
  Result:= '';
  with ListView1 do
    begin
      Result:= Items[i].Caption;
      if Items[i].SubItems[0] <> '' then
        Result:= Result + ', ' + Items[i].SubItems[0];
    end;  
end;

procedure TForm1.mnuCopy1Click(Sender: TObject);
var
  s: WideString;
  i: Integer;
begin
  S:= '';
  with ListView1 do
    if Selected<>nil then
      for i:= 0 to Items.Count-1 do
        if Items[i].Selected then
          S:= S + FName(i) + #13#10;
  TntClipboard.AsWideText:= S;
end;

procedure TForm1.mnuCopy2Click(Sender: TObject);
var
  s: WideString;
  i: Integer;
begin
  S:= '';
  with ListView1 do
    if Selected<>nil then
      for i:= 0 to Items.Count-1 do
        if Items[i].Selected then
          S:= S + Items[i].SubItems[2] + #13#10;
  TntClipboard.AsWideText:= S;
end;

procedure TForm1.mnuCopy3Click(Sender: TObject);
var
  s: WideString;
  i: Integer;
begin
  S:= '';
  with ListView1 do
    if Selected<>nil then
      for i:= 0 to Items.Count-1 do
        if Items[i].Selected then
          S:= S + FName(i) + ' = ' + Items[i].SubItems[2] + #13#10;
  TntClipboard.AsWideText:= S;
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
begin
  with ListView1 do
    if Selected<>nil then
      with FormMemo do
        begin
        if Selected.SubItems[3] <> '' then
          Memo1.Text:= WdxList(
            PluginName,
            TestName,
            DefIni,
            nil,
            true,
            StrToInt(Selected.SubItems[3]))
        else
          Memo1.Text:= Selected.SubItems[2];
        ShowModal;
        end;
end;

procedure TForm1.chkTextClick(Sender: TObject);
begin
  LoadWdx;
end;

initialization
  CoInitialize(nil);

finalization
  CoUninitialize;

end.
