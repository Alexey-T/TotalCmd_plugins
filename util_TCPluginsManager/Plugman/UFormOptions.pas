unit UFormOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Buttons, Dialogs, ComCtrls;

type
  TFormOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ffRegister: TCheckBox;
    LabelLang: TLabel;
    sLang: TComboBox;
    ffTimer: TCheckBox;
    ffTimerSec: TEdit;
    LabelSec: TLabel;
    ffShowTypes: TCheckBox;
    TabSheet3: TTabSheet;
    ffRestart1: TRadioButton;
    ffRestart2: TRadioButton;
    LabelInstalling: TLabel;
    ffUseTCVar: TCheckBox;
    ffInstRestart: TCheckBox;
    ffInstTweakAfter: TCheckBox;
    LabelRestartTime: TLabel;
    ffRestartTime: TEdit;
    TabSheet4: TTabSheet;
    LabelLister: TLabel;
    ffPluginsLister: TEdit;
    ffPluginsPacker: TEdit;
    ffPluginsFS: TEdit;
    ffPluginsContent: TEdit;
    LabelPacker: TLabel;
    LabelFS: TLabel;
    LabelContent: TLabel;
    LabelFolders: TLabel;
    OpenDialog2: TOpenDialog;
    OpenDialog1: TOpenDialog;
    ffRestartForeground: TCheckBox;
    ffInstTweakBefore: TCheckBox;
    boxColumns: TGroupBox;
    ffColDesc: TCheckBox;
    ffColAssoc: TCheckBox;
    ffColVersion: TCheckBox;
    ffColFilename: TCheckBox;
    ffColState: TCheckBox;
    ffShowNumbers: TCheckBox;
    OpenDialog3: TOpenDialog;
    TabSheet5: TTabSheet;
    LabelFtp: TLabel;
    ffFtpIni: TEdit;
    btnFtpBrowse: TButton;
    ffRarPath: TEdit;
    btnRarBrowse: TButton;
    LabelRar: TLabel;
    ffUseEditor: TCheckBox;
    ffEditor: TEdit;
    btnEditorBrowse: TButton;
    btnBarAdd: TButton;
    procedure FormShow(Sender: TObject);
    procedure sLangChange(Sender: TObject);
    procedure ffTimerClick(Sender: TObject);
    procedure btnBarAddClick(Sender: TObject);
    procedure btnFtpBrowseClick(Sender: TObject);
    procedure btnRarBrowseClick(Sender: TObject);
    procedure btnEditorBrowseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ffLang: string;
    ffBarPath: string;
    ffOptionsIni: string;
  end;

var
  FormOptions: TFormOptions;

implementation

uses
  Msg, MsgFile, FProc, SProc,
  tcProc, tcButtons;

{$R *.DFM}

procedure TFormOptions.FormShow(Sender: TObject);
var
  h: THandle;
  fd: TWin32FindData;
  n: integer;
begin
  {$I Init_UFormOptions.pas}
  btnBarAdd.Caption:= Format(MsgCaption(0214), [ExtractFileName(ffBarPath)]);
  ffTimerClick(Self);

  with PageControl1 do
  if ActivePage=TabSheet1 then ActiveControl:= ffShowTypes else
   if ActivePage=TabSheet2 then ActiveControl:= ffTimer else
    if ActivePage=TabSheet3 then ActiveControl:= ffRestartTime else
     if ActivePage=TabSheet4 then ActiveControl:= ffPluginsLister;

  //Languages:
  h:= FindFirstFile(PChar(ExtractFileDir(ParamStr(0))+'\Language\*.lng'), fd);
  if h<>INVALID_HANDLE_VALUE then
   with sLang do
    begin
    Items.BeginUpdate;
    Items.Clear;
    repeat
      Items.Append(ChangeFileExt(fd.cFileName, ''));
    until not FindNextFile(h, fd);
    n:= Items.IndexOf(ffLang);
    if n>=0
      then ItemIndex:= n
      else ItemIndex:= Items.IndexOf('English');
    Items.EndUpdate;
    end;
end;


procedure TFormOptions.sLangChange(Sender: TObject);
begin
  with sLang do
    if ItemIndex>=0
      then ffLang:= Items[ItemIndex]
      else ffLang:= 'English';
  SetMsgLang(ffLang);
  FormShow(Self);
end;


procedure TFormOptions.ffTimerClick(Sender: TObject);
begin
  ffTimerSec.Enabled:= ffTimer.Checked;
end;

procedure TFormOptions.btnBarAddClick(Sender: TObject);
var
  cmd: string;
begin
  if MsgConfirmed(S0100) then
    begin
    cmd:= ParamStr(0);
    tcButtonDelete(ffBarPath, ExtractFileName(cmd));
    //tcButtonAppendSeparator(ffBarPath);
    tcButtonAppend(ffBarPath, cmd, '', MsgCaption(0050), 0);
    tcButtonAppend(ffBarPath, cmd, '/Restart', MsgCaption(0061), 6);
    end;
end;

procedure TFormOptions.btnFtpBrowseClick(Sender: TObject);
begin
  with OpenDialog2 do
    begin
    InitialDir:= ExtractFileDir(ffOptionsIni);
    FileName:= '';
    if Execute then
      ffFtpIni.Text:= FileName;
    end;
end;

procedure TFormOptions.btnRarBrowseClick(Sender: TObject);
begin
  with OpenDialog1 do
    begin
    InitialDir:= SExpandVars('%ProgramFiles%');
    if not SExpanded(InitialDir) then
      InitialDir:= 'C:\Program Files';
    FileName:= 'WinRAR.exe';

    if Execute then
      ffRarPath.Text:= FileName;
    end;
end;

procedure TFormOptions.btnEditorBrowseClick(Sender: TObject);
begin
  with OpenDialog3 do
    if Execute then
      ffEditor.Text:= FileName;
end;

end.
